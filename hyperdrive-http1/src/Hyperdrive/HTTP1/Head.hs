{-# LANGUAGE BangPatterns, OverloadedStrings, PatternSynonyms #-}
-- | Parsing the head of an HTTP/1.1 request: the request line and the
-- header section (RFC 9112, Sections 2 to 5).
--
-- The parser is strict wherever RFC 9112 allows a choice, since a
-- server that accepts what others reject, or reads it differently, is
-- how request smuggling works: lines end with CRLF, the request line's
-- parts are separated by single spaces, and lines beginning with
-- whitespace are rejected.
module Hyperdrive.HTTP1.Head
  ( RawRequestHead(..)
  , Limits(..)
  , defaultLimits
  , HeadResult(..)
  , HeadError(..)
  , parseRequestHead
  , headErrorStatus
    -- * Field lines, also used for trailer sections
  , parseFieldLine
  , checkLine
  , scanFieldLines
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import Data.Maybe (fromJust)
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import Hyperdrive.Semantics.Field (FieldName, FieldValue, FieldValueError, Fields, emptyFields, fieldsFromList, mkFieldName, mkFieldValue)
import Hyperdrive.Semantics.Internal (FieldName(..), FieldValue(..), Fields(..))
import Hyperdrive.Semantics.Message (HttpVersion(..))
import Hyperdrive.Semantics.Method (Method, mkMethod)
import Hyperdrive.Semantics.Status (Status, mkStatus, pattern BadRequest, pattern HTTPVersionNotSupported, pattern NotImplemented, pattern URITooLong)
import Hyperdrive.Semantics.Syntax (isOWS, isTChar)

-- | A request head as received. The request target is checked only for
-- its characters here; it is turned into a 'Hyperdrive.Semantics.Target.Target',
-- together with @Host@, in a later step.
data RawRequestHead = RawRequestHead
  { headMethod  :: !Method
  , headTarget  :: !ByteString
  , headVersion :: !HttpVersion
  , headFields  :: !Fields
  } deriving (Show)

-- | Limits on the head of a request.
data Limits = Limits
  { maxRequestLine  :: !Int  -- ^ octets in the request line, without its CRLF
  , maxFieldSection :: !Int  -- ^ octets in the header section, with its CRLFs
  , maxFields       :: !Int  -- ^ field lines in the header section
  } deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3
-- # It is RECOMMENDED that all HTTP senders and recipients
-- # support, at a minimum, request-line lengths of 8000 octets.

-- | A request line of 8 KiB, a header section of 32 KiB and 100 field
-- lines (docs/http1-core-design.md, D4).
defaultLimits :: Limits
defaultLimits = Limits { maxRequestLine = 8192, maxFieldSection = 32768, maxFields = 100 }

data HeadResult
  = HeadIncomplete !Int
    -- ^ no complete head yet. The number is how far the input has been
    -- searched: pass it back with the longer input so that the search
    -- resumes there, rather than starting over on every read.
  | HeadComplete !RawRequestHead !Int
    -- ^ the head, and the number of octets it took (with any empty
    -- lines before it): the body or the next request starts there
  | HeadError !HeadError
  deriving (Show)

data HeadError
  = MethodTooLong                -- ^ a method longer than the request line allows
  | RequestTargetTooLong         -- ^ a request target longer than the request line allows
  | RequestLineTooLong           -- ^ a request line too long for another reason
  | InvalidRequestLine           -- ^ not method SP request-target SP HTTP-version
  | UnsupportedVersion !Int      -- ^ an HTTP major version other than 1
  | BareLineFeed                 -- ^ a line ending in LF without CR
  | BareCarriageReturn           -- ^ a CR not followed by LF
  | LineFolding                  -- ^ a line starting with whitespace: obsolete line folding, or
                                 -- whitespace between the request line and the first field
  | WhitespaceBeforeColon        -- ^ whitespace between a field name and its colon
  | InvalidFieldLine             -- ^ not field-name ":" OWS field-value OWS
  | InvalidFieldValue !FieldValueError
  | FieldSectionTooLarge         -- ^ more octets than 'maxFieldSection'
  | TooManyFields                -- ^ more field lines than 'maxFields'
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3
-- # A server that receives a method longer than any that it implements
-- # SHOULD respond with a 501
-- # (Not Implemented) status code.  A server that receives a request-
-- # target longer than any URI it wishes to parse MUST respond with a 414
-- # (URI Too Long) status code

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.2
-- # A server can send a 505
-- # (HTTP Version Not Supported) response if it wishes, for any reason,
-- # to refuse service of the client's major protocol version.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.6.6
-- # The 505 (HTTP Version Not Supported) status code indicates that the
-- # server does not support, or refuses to support, the major version of
-- # HTTP that was used in the request message.

-- | The status to respond with, before closing the connection.
-- A header section too large gets 431 (Request Header Fields Too
-- Large, RFC 6585).
headErrorStatus :: HeadError -> Status
headErrorStatus e = case e of
  MethodTooLong          -> NotImplemented
  RequestTargetTooLong   -> URITooLong
  UnsupportedVersion _   -> HTTPVersionNotSupported
  FieldSectionTooLarge   -> requestHeaderFieldsTooLarge
  TooManyFields          -> requestHeaderFieldsTooLarge
  _                      -> BadRequest
  where requestHeaderFieldsTooLarge = fromJust (mkStatus 431)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-2.2
-- # In the interest of robustness, a server that is expecting to receive
-- # and parse a request-line SHOULD ignore at least one empty line (CRLF)
-- # received prior to the request-line.

-- | Parse the head of a request from the start of the input, resuming
-- the search for its end where an earlier, shorter input left off.
--
-- Empty lines before the request line are skipped. Nothing is parsed
-- until the whole head (up to the empty line that ends it) has
-- arrived, except that a request line, or a head, longer than the
-- limits is rejected as soon as it is known to be.
parseRequestHead :: Limits -> Int -> ByteString -> HeadResult
parseRequestHead limits searched input =
  case findEnd (max start (searched - 3)) of
    Just end -> parseHead (B.take (end - start) (B.drop start input)) (end + 4)
    Nothing
      | Just e <- tooLong -> HeadError e
      | otherwise         -> HeadIncomplete (B.length input)
  where
    -- the empty lines before the request line
    start = skipEmptyLines 0
    skipEmptyLines i
      | B.take 2 (B.drop i input) == "\r\n" = skipEmptyLines (i + 2)
      | otherwise                          = i
    -- the index of the CRLF CRLF that ends the head, at or after i
    findEnd i = case B.breakSubstring "\r\n\r\n" (B.drop i input) of
      (before, after) | B.null after -> Nothing
                      | otherwise    -> Just (i + B.length before)
    -- what can be rejected before the head is complete: a request line
    -- too long, or more after it than a header section may hold
    tooLong = case B.breakSubstring "\r\n" (B.drop start input) of
      (line, rest)
        | B.length line > maxRequestLine limits -> Just (lineTooLong line)
        | not (B.null rest), B.length rest - 2 > maxFieldSection limits -> Just FieldSectionTooLarge
        | otherwise -> Nothing

    parseHead head' consumed =
      let (requestLine, rest) = B.breakSubstring "\r\n" head'
          fieldLines = if B.null rest then [] else splitLines (B.drop 2 rest)
          -- A head that parses has no CR or LF in it (names are tokens,
          -- values are checked by mkFieldValue, the request line is
          -- visible characters), so the line checks are skipped when it
          -- does, and the field lines are read by scanFieldLines
          -- without splitting them first; any failure takes the full path, which finds
          -- the error that comes first.
          withinLimits = B.length requestLine <= maxRequestLine limits
            && B.length head' + 4 - B.length requestLine - 2 <= maxFieldSection limits
          fast | B.null rest = Just emptyFields
               | otherwise   = scanFieldLines (maxFields limits) (B.drop 2 rest)
      in if not withinLimits then full requestLine fieldLines else
         case parseRequestLine requestLine of
           Right (method, target, 1, minor)
             | Just fields <- fast -> HeadComplete (RawRequestHead method target (HttpVersion 1 minor) fields) consumed
           _ -> full requestLine fieldLines
      where
       full requestLine fieldLines = either HeadError (\h -> HeadComplete h consumed) $ do
           checkLine requestLine
           mapM_ checkLine fieldLines
           if B.length requestLine > maxRequestLine limits then Left (lineTooLong requestLine) else Right ()
           if B.length head' + 4 - B.length requestLine - 2 > maxFieldSection limits
             then Left FieldSectionTooLarge else Right ()
           if length fieldLines > maxFields limits then Left TooManyFields else Right ()
           (method, target, major, minor) <- parseRequestLine requestLine
           fields <- mapM parseFieldLine fieldLines
           -- a version we do not serve is reported only for a head that
           -- is otherwise well formed; a malformed one gets 400
           if major /= 1 then Left (UnsupportedVersion major) else Right ()
           Right (RawRequestHead method target (HttpVersion 1 minor) (fieldsFromList fields))

    lineTooLong line
      | not (B.elem 0x20 line) = MethodTooLong
      | otherwise              = RequestTargetTooLong

-- | Split on CRLF.
splitLines :: ByteString -> [ByteString]
splitLines bs = case B.breakSubstring "\r\n" bs of
  (line, rest) | B.null rest -> [line]
               | otherwise   -> line : splitLines (B.drop 2 rest)

-- | The field lines of a header section (without the CRLF CRLF that
-- ends it), if every line is a valid field line and there are at most
-- this many; 'Nothing' otherwise, for the full path to report. Each
-- line is found with one search for CR (memchr) and checked with one
-- scan of its value. It accepts exactly what 'splitLines', 'checkLine' and
-- 'parseFieldLine' do and gives the same fields: a name of tchars
-- (lower-cased), a colon, and a value of field-vchars, SP and HTAB
-- with the whitespace at either end left out.
scanFieldLines :: Int -> ByteString -> Maybe Fields
scanFieldLines limit section0 = line 0 [] section0
  where
    line :: Int -> [(FieldName, FieldValue)] -> ByteString -> Maybe Fields
    line !count acc s
      | count >= limit = Nothing
      -- the line ends at the end of the section, or at a CRLF
      | otherwise = case B.elemIndex 0x0D s of
          Nothing -> case fieldLine s of
            Just field -> Just (Fields (Seq.fromList (reverse (field : acc))))
            Nothing    -> Nothing
          Just k
            | k + 1 < B.length s, BU.unsafeIndex s (k + 1) == 0x0A -> case fieldLine (BU.unsafeTake k s) of
                Just field -> line (count + 1) (field : acc) (BU.unsafeDrop (k + 2) s)
                Nothing    -> Nothing
            | otherwise -> Nothing
    fieldLine :: ByteString -> Maybe (FieldName, FieldValue)
    fieldLine l
      | B.null name || B.null afterName || BU.unsafeHead afterName /= 0x3A
        || not (B.all isValueOctet raw) = Nothing
      | otherwise =
          let !v = B.dropWhileEnd isOWS (B.dropWhile isOWS raw)
              !n = if B.any isUpper name then B.map toLower name else name
          in Just (FieldName n, FieldValue v)
      where
        name = B.takeWhile isTChar l
        afterName = BU.unsafeDrop (B.length name) l
        raw = BU.unsafeTail afterName
    {-# INLINE fieldLine #-}
    -- field-vchar, SP or HTAB
    isValueOctet w = (w >= 0x20 && w /= 0x7F) || w == 0x09
    isUpper w = w >= 0x41 && w <= 0x5A
    toLower w = if isUpper w then w + 0x20 else w

-- = https://www.rfc-editor.org/rfc/rfc9112#section-2.2
-- # A recipient of such a bare CR MUST consider that element to be
-- # invalid or replace each bare CR with SP before processing the element
-- # or forwarding the message.

-- | A line (without its CRLF) may not contain CR or LF.
checkLine :: ByteString -> Either HeadError ()
checkLine line
  | B.elem 0x0A line = Left BareLineFeed
  | B.elem 0x0D line = Left BareCarriageReturn
  | otherwise        = Right ()

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3
-- # request-line   = method SP request-target SP HTTP-version

-- = https://www.rfc-editor.org/rfc/rfc9112#section-2.3
-- # HTTP-version  = HTTP-name "/" DIGIT "." DIGIT
-- # HTTP-name     = %s"HTTP"

-- | The parts of a request line, separated by exactly one space each,
-- with the major and minor version. The request target is any run of
-- visible ASCII characters here; its grammar is checked when it is
-- turned into a target.
parseRequestLine :: ByteString -> Either HeadError (Method, ByteString, Int, Word8)
parseRequestLine line = case B.split 0x20 line of
  [m, target, v]
    | Just method <- mkMethod m
    , not (B.null target), B.all (\w -> w >= 0x21 && w <= 0x7E) target
    , B.length v == 8, B.take 5 v == "HTTP/", BU.unsafeIndex v 6 == 0x2E  -- "HTTP/" DIGIT "." DIGIT
    , digit (BU.unsafeIndex v 5), digit (BU.unsafeIndex v 7)
    -> Right (method, target, fromIntegral (BU.unsafeIndex v 5 - 0x30), BU.unsafeIndex v 7 - 0x30)
  _ -> Left InvalidRequestLine
  where digit w = w >= 0x30 && w <= 0x39

-- = https://www.rfc-editor.org/rfc/rfc9112#section-5
-- # field-line   = field-name ":" OWS field-value OWS

-- = https://www.rfc-editor.org/rfc/rfc9112#section-5.1
-- # A
-- # server MUST reject, with a response status code of 400 (Bad Request),
-- # any received request message that contains whitespace between a
-- # header field name and colon.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-5.2
-- # A server that receives an obs-fold in a request message that is not
-- # within a "message/http" container MUST either reject the message by
-- # sending a 400 (Bad Request), preferably with a representation
-- # explaining that obsolete line folding is unacceptable, or replace
-- # each received obs-fold with one or more SP octets prior to
-- # interpreting the field value or forwarding the message downstream.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-2.2
-- # A recipient that receives whitespace between the start-line and the
-- # first header field MUST either reject the message as invalid or
-- # consume each whitespace-preceded line without further processing of
-- # it

-- | A field line. A line that starts with whitespace is rejected: it is
-- either obsolete line folding or whitespace after the request line.
parseFieldLine :: ByteString -> Either HeadError (FieldName, FieldValue)
parseFieldLine line
  | Just (w, _) <- B.uncons line, w == 0x20 || w == 0x09 = Left LineFolding
  | otherwise =
      let (name, rest) = B.span isTChar line
      in case B.uncons rest of
           Just (0x3A, value)
             | Just n <- mkFieldName name -> either (Left . InvalidFieldValue) (\v -> Right (n, v)) (mkFieldValue value)
           Just (w, _)
             | w == 0x20 || w == 0x09, not (B.null name)
             , B.take 1 (B.dropWhile (\x -> x == 0x20 || x == 0x09) rest) == ":" -> Left WhitespaceBeforeColon
           _ -> Left InvalidFieldLine
