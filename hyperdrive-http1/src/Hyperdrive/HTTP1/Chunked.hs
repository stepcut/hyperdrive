{-# LANGUAGE OverloadedStrings #-}
-- | Decoding the chunked transfer coding of a request body (RFC 9112,
-- Section 7.1), incrementally: the decoder is given whatever input has
-- arrived and says how much of it it used.
--
-- Like the head parser it is strict, since a chunked body that two
-- recipients split differently is a way to smuggle a request: lines end
-- with CRLF, the data of a chunk must be followed by exactly CRLF, and
-- a chunk size too large for 64 bits is an error.
module Hyperdrive.HTTP1.Chunked
  ( ChunkedLimits(..)
  , defaultChunkedLimits
  , Decoder
  , startDecoder
  , Decoded(..)
  , ChunkError(..)
  , decodeChunked
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word64)
import Hyperdrive.HTTP1.Head (HeadError, checkLine, parseFieldLine)
import Hyperdrive.Semantics.Field (Fields, emptyFields, fieldsFromList)
import Hyperdrive.Semantics.FieldSyntax (parseQuotedString)
import Hyperdrive.Semantics.Syntax (isHEXDIG, isOWS, isTChar)

data ChunkedLimits = ChunkedLimits
  { maxChunkExtensions :: !Int  -- ^ octets of chunk extensions in the whole body
  , maxTrailerSection  :: !Int  -- ^ octets of the trailer section, with its CRLFs
  } deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.1
-- # A server
-- # ought to limit the total length of chunk extensions received in a
-- # request to an amount reasonable for the services provided, in the
-- # same way that it applies length limitations and timeouts for other
-- # parts of a message, and generate an appropriate 4xx (Client Error)
-- # response if that amount is exceeded.

-- | Chunk extensions of 1 KiB and a trailer section of 8 KiB
-- (docs/http1-core-design.md, D4).
defaultChunkedLimits :: ChunkedLimits
defaultChunkedLimits = ChunkedLimits { maxChunkExtensions = 1024, maxTrailerSection = 8192 }

-- | Where the decoder is in a chunked body.
data Decoder = Decoder
  { decoderPhase      :: !Phase
  , decoderExtensions :: !Int   -- ^ octets of chunk extensions so far
  } deriving (Eq, Show)

data Phase
  = SizeLine          -- ^ expecting a chunk-size line
  | InData !Word64    -- ^ in the data of a chunk, with this many octets left
  | DataEnd           -- ^ expecting the CRLF after a chunk's data
  | Trailers !Int     -- ^ in the trailer section, searched this far for its end
  deriving (Eq, Show)

startDecoder :: Decoder
startDecoder = Decoder SizeLine 0

data Decoded
  = Data !ByteString !Int !Decoder
    -- ^ content (a slice of the input), the octets of input used, and
    -- the decoder for what follows
  | NeedMore !Int !Decoder
    -- ^ the octets of input used (chunk-size lines and CRLFs), and the
    -- decoder: call it again with the rest of the input and more
  | Finished !Fields !Int
    -- ^ the body is complete: its trailer section, and the octets of
    -- input used; what follows is the next request
  | DecodeError !ChunkError
  deriving (Show)

-- | The body cannot be decoded; the server responds with 400 (Bad
-- Request) and closes the connection.
data ChunkError
  = InvalidChunkSize          -- ^ not 1*HEXDIG
  | ChunkSizeTooLarge         -- ^ larger than 64 bits
  | InvalidChunkExtension
  | ChunkExtensionsTooLong    -- ^ more than 'maxChunkExtensions' in the body
  | ChunkSizeLineTooLong      -- ^ a chunk-size line longer than 64 octets plus 'maxChunkExtensions'
  | MissingChunkEnd           -- ^ chunk data not followed by CRLF
  | ChunkLineEnding           -- ^ a bare CR or LF in a chunk-size line
  | TrailerSectionTooLarge    -- ^ more than 'maxTrailerSection'
  | InvalidTrailer !HeadError -- ^ a trailer line that is not a field line
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # chunked-body   = *chunk
-- # last-chunk
-- # trailer-section
-- # CRLF

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # chunk          = chunk-size [ chunk-ext ] CRLF
-- # chunk-data CRLF
-- # chunk-size     = 1*HEXDIG
-- # last-chunk     = 1*("0") [ chunk-ext ] CRLF
-- # chunk-data     = 1*OCTET ; a sequence of chunk-size octets

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # The chunked transfer coding is complete
-- # when a chunk with a chunk-size of zero is received, possibly followed
-- # by a trailer section, and finally terminated by an empty line.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # A recipient MUST be able to parse and decode the chunked transfer
-- # coding.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.2
-- # A recipient that removes the chunked coding from a message MAY
-- # selectively retain or discard the received trailer fields.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.2
-- # A
-- # recipient that retains a received trailer field MUST either store/
-- # forward the trailer field separately from the received header fields
-- # or merge the received trailer field into the header section.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.2
-- # A
-- # recipient MUST NOT merge a received trailer field into the header
-- # section unless its corresponding header field definition explicitly
-- # permits and instructs how the trailer field value can be safely
-- # merged.

-- | Decode as much of the input as can be decoded, up to and including
-- the next piece of content. The caller drops the octets used from its
-- input, adds what arrives, and calls again.
--
-- The trailer section is returned on its own, never merged into the
-- header section, and is checked only once all of it has arrived.
decodeChunked :: ChunkedLimits -> Decoder -> ByteString -> Decoded
decodeChunked limits = go 0
  where
    go used d input = case decoderPhase d of
      InData n
        | B.null input -> NeedMore used d
        | otherwise ->
            let piece = B.take (fromIntegral (min n (fromIntegral (B.length input)))) input
                left  = n - fromIntegral (B.length piece)
                next  = d { decoderPhase = if left == 0 then DataEnd else InData left }
            in Data piece (used + B.length piece) next
      DataEnd
        | B.length input < 2       -> NeedMore used d
        | B.take 2 input == "\r\n" -> go (used + 2) d { decoderPhase = SizeLine } (B.drop 2 input)
        | otherwise                -> DecodeError MissingChunkEnd
      SizeLine -> case B.breakSubstring "\r\n" (B.take (maxSizeLine + 2) input) of
        (line, rest)
          | B.length line > maxSizeLine -> DecodeError ChunkSizeLineTooLong
          | B.null rest -> NeedMore used d
          | otherwise -> case sizeLine line of
              Left e -> DecodeError e
              Right (size, extLength)
                | decoderExtensions d + extLength > maxChunkExtensions limits -> DecodeError ChunkExtensionsTooLong
                | otherwise ->
                    let d' = d { decoderExtensions = decoderExtensions d + extLength
                               , decoderPhase = if size == 0 then Trailers 0 else InData size }
                    in go (used + B.length line + 2) d' (B.drop (B.length line + 2) input)
      Trailers searched -> trailers used searched input

    -- a chunk-size line: 64 octets for the size (16 digits are enough for
    -- any size, the rest allows leading zeros) and the extensions allowed
    maxSizeLine = 64 + maxChunkExtensions limits

    trailers used searched input
      | B.take 2 input == "\r\n" = Finished emptyFields (used + 2)
      -- only the first maxTrailerSection octets can hold a section that
      -- is not too large, with its final CRLF CRLF
      | otherwise = case B.breakSubstring "\r\n\r\n" (B.drop start (B.take limit input)) of
          (before, rest)
            | B.null rest ->
                if B.length input >= limit then DecodeError TrailerSectionTooLarge
                else NeedMore used (Decoder (Trailers (B.length input)) 0)
            | otherwise ->
                let section = B.take (start + B.length before) input
                in case traverse fieldLine (splitCRLF section) of
                     Left e   -> DecodeError (InvalidTrailer e)
                     Right fs -> Finished (fieldsFromList fs) (used + B.length section + 4)
      where start = max 0 (searched - 3)
            limit = maxTrailerSection limits
    fieldLine l = checkLine l >> parseFieldLine l

splitCRLF :: ByteString -> [ByteString]
splitCRLF bs = case B.breakSubstring "\r\n" bs of
  (line, rest) | B.null rest -> [line]
               | otherwise   -> line : splitCRLF (B.drop 2 rest)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # Therefore, recipients MUST
-- # anticipate potentially large hexadecimal numerals and prevent parsing
-- # errors due to integer conversion overflows or precision loss due to
-- # integer representation.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.1
-- # chunk-ext      = *( BWS ";" BWS chunk-ext-name
-- # [ BWS "=" BWS chunk-ext-val ] )
-- # chunk-ext-name = token
-- # chunk-ext-val  = token / quoted-string

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.1
-- # A recipient MUST ignore unrecognized chunk extensions.

-- | The size and the length of the extensions of a chunk-size line
-- (without its CRLF). The extensions are checked and then ignored.
sizeLine :: ByteString -> Either ChunkError (Word64, Int)
sizeLine line
  | B.elem 0x0A line || B.elem 0x0D line = Left ChunkLineEnding
  | B.null digits = Left InvalidChunkSize
  | otherwise = do
      extensions ext
      size <- maybe (Left ChunkSizeTooLarge) Right (B.foldl' step (Just 0) digits)
      Right (size, B.length ext)
  where
    (digits, ext) = B.span isHEXDIG line
    step acc w = do
      n <- acc
      if n > maxBound `div` 16 then Nothing else Just (n * 16 + hexValue w)
    hexValue w
      | w <= 0x39 = fromIntegral (w - 0x30)
      | w >= 0x61 = fromIntegral (w - 0x61 + 10)
      | otherwise = fromIntegral (w - 0x41 + 10)
    -- *( BWS ";" BWS chunk-ext-name [ BWS "=" BWS chunk-ext-val ] )
    extensions s
      | B.null s = Right ()
      | otherwise = case B.uncons (B.dropWhile isOWS s) of
          Just (0x3B, s1) -> do
            let (name, s2) = B.span isTChar (B.dropWhile isOWS s1)
            if B.null name then Left InvalidChunkExtension else Right ()
            let s3 = B.dropWhile isOWS s2
            case B.uncons s3 of
              Just (0x3D, s4) -> value (B.dropWhile isOWS s4) >>= extensions
              _               -> extensions s2
          _ -> Left InvalidChunkExtension
    value s = case B.uncons s of
      Just (0x22, _) -> either (const (Left InvalidChunkExtension)) (Right . snd) (parseQuotedString s)
      _ -> let (t, rest) = B.span isTChar s
           in if B.null t then Left InvalidChunkExtension else Right rest
