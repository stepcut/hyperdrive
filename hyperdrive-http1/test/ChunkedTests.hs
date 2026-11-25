{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module ChunkedTests (chunkedTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toLower, toUpper)
import Data.List (sort)
import Data.Maybe (fromJust)
import Hyperdrive.ABNF.Generate (nearMisses, validStrings)
import Hyperdrive.ABNF.Grammar (mkGrammar)
import Hyperdrive.ABNF.HTTP (rfc3986, rfc9110, rfc9110Errata, rfc9112, rfc9112Errata, withErrata)
import Hyperdrive.ABNF.QQ (abnf)
import Hyperdrive.ABNF.Recognize (Library, ListForm(..), library, matches)
import Hyperdrive.ABNF.Types (mkRuleName)
import Hyperdrive.HTTP1.Chunked
import Hyperdrive.HTTP1.Head (HeadError(..))
import Hyperdrive.Semantics.Field
import Numeric (readHex, showHex)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

chunkedTests :: TestTree
chunkedTests = testGroup "Chunked transfer coding (RFC 9112 Section 7.1)"
  [ testProperty "agrees with a decoder following RFC 9112 Section 7.1.3 and its grammar" prop_reference
  , testProperty "decodes what was encoded" prop_roundTrip
  , testProperty "reading in pieces gives the same result as reading at once" prop_incremental
  , testProperty "limits, at their boundaries" prop_limits
  , testProperty "each rule, by example" prop_examples
  ]

------------------------------------------------------------------------------
-- running the decoder
------------------------------------------------------------------------------

data Outcome
  = Incomplete
  | Failed ChunkError
  | Done ByteString [(ByteString, ByteString)] Int  -- ^ content, trailers, octets used
  deriving (Eq, Show)

-- | Feed the pieces one after another, as a connection would: the
-- decoder is called until it needs more, then the next piece is added
-- to what it has not used.
decodeAll :: ChunkedLimits -> [ByteString] -> Outcome
decodeAll limits = go startDecoder [] 0 ""
  where
    go d acc used buf pieces = case decodeChunked limits d buf of
      Data b n d'   -> go d' (b : acc) (used + n) (B.drop n buf) pieces
      NeedMore n d' -> case pieces of
        []       -> Incomplete
        (p : ps) -> go d' acc (used + n) (B.drop n buf <> p) ps
      Finished fs n -> Done (B.concat (reverse acc)) (fieldList fs) (used + n)
      DecodeError e -> Failed e

fieldList :: Fields -> [(ByteString, ByteString)]
fieldList fs = [ (fieldNameBytes k, fieldValueBytes v) | (k, v) <- fieldsToList fs ]

-- | Limits no generated input reaches, for comparing with the reference.
noLimits :: ChunkedLimits
noLimits = ChunkedLimits { maxChunkExtensions = 1000000, maxTrailerSection = 1000000 }

------------------------------------------------------------------------------
-- the reference decoder
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # chunked-body   = *chunk
-- # last-chunk
-- # trailer-section
-- # CRLF

grammarLibrary :: Library
grammarLibrary = library
  [ ("T", grammar [abnf|
      ; a chunk-size line and a last-chunk line, without its CRLF
      size-line  = chunk-size [ chunk-ext ]
      chunk-size = <chunk-size, see [HTTP/1.1], Section 7.1>
      chunk-ext  = <chunk-ext, see [HTTP/1.1], Section 7.1.1>
      field-line = <field-line, see [HTTP/1.1], Section 5>
      |])
  , ("HTTP/1.1", grammar (withErrata rfc9112Errata rfc9112))
  , ("HTTP", grammar (withErrata rfc9110Errata rfc9110))
  , ("URI", grammar rfc3986) ]
  where grammar = either (error . show) id . mkGrammar

-- | Matchers for the two kinds of line, each compiled once.
isSizeLine, isFieldLine :: ByteString -> Bool
isSizeLine = ruleMatches "size-line"
isFieldLine = ruleMatches "field-line"

ruleMatches :: String -> ByteString -> Bool
ruleMatches r = let m = matches Recipient grammarLibrary "T" (fromJust (mkRuleName (C.pack r)))
                in either (error . show) id . m

ruleStrings, ruleNearMisses :: String -> Gen ByteString
ruleStrings r = either (error . show) id (validStrings Recipient grammarLibrary "T" (fromJust (mkRuleName (C.pack r))))
ruleNearMisses r = either (error . show) id (nearMisses Recipient grammarLibrary "T" (fromJust (mkRuleName (C.pack r))))

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.3
-- # A process for decoding the chunked transfer coding can be represented
-- # in pseudo-code as:

-- | RFC 9112's decoding process, written as directly as possible, on the
-- whole input at once: each line is checked with the RFC's own grammar,
-- and sizes are read as unbounded integers. It differs in one way
-- that the decoder chose: the trailer section is checked once its end
-- has arrived, not line by line. And a trailer section with a field of
-- framing is an error. "error" is any error; "overflow" is a size too
-- large for 64 bits.
reference :: ByteString -> Either String (ByteString, [(ByteString, ByteString)], Int)
reference input = sizes 0 []
  where
    -- read chunk-size, chunk-ext (if any), and CRLF
    sizes pos content = do
      (line, next) <- readLine pos
      if not (isSizeLine line) then Left "error" else Right ()
      let size = case readHex (C.unpack (C.takeWhile isHex line)) of
            [(n, "")] -> n :: Integer
            _         -> error "size-line without a chunk-size"
      if size >= 2 ^ (64 :: Int) then Left "overflow" else Right ()
      if size > 0
        -- read chunk-data and CRLF; append chunk-data to content
        then do
          if toInteger (B.length input) < toInteger next + size + 2 then Left "incomplete" else Right ()
          let n = fromInteger size
          if B.take 2 (B.drop (next + n) input) /= "\r\n" then Left "error" else Right ()
          sizes (next + n + 2) (B.take n (B.drop next input) : content)
        else trailers next (B.concat (reverse content))
    -- read trailer field, while (trailer field is not empty)
    trailers pos content = do
      _ <- endOfSection pos
      let go p acc = do
            (line, next) <- readLine p
            if B.null line
              then if any ((`elem` ["content-length", "transfer-encoding"]) . fst) acc
                     then Left "error" else Right (content, reverse acc, next)
            else if isFieldLine line then go next (field line : acc)
            else Left "error"
      go pos []
    endOfSection pos
      | B.take 2 (B.drop pos input) == "\r\n" = Right ()
      | otherwise = case B.breakSubstring "\r\n\r\n" (B.drop pos input) of
          (_, rest) | B.null rest -> Left "incomplete"
                    | otherwise   -> Right ()
    readLine pos = case B.breakSubstring "\r\n" (B.drop pos input) of
      (_, rest) | B.null rest -> Left "incomplete"
      (line, _) -> Right (line, pos + B.length line + 2)
    field line =
      let (name, rest) = C.break (== ':') line
      in (C.map toLower name, B.dropWhileEnd ows (B.dropWhile ows (B.drop 1 rest)))
    ows w = w == 0x20 || w == 0x09
    isHex c = c `elem` ("0123456789abcdefABCDEF" :: String)

outcome :: Outcome -> Either String (ByteString, [(ByteString, ByteString)], Int)
outcome o = case o of
  Incomplete               -> Left "incomplete"
  Failed ChunkSizeTooLarge -> Left "overflow"
  Failed _                 -> Left "error"
  Done c t n               -> Right (c, t, n)

------------------------------------------------------------------------------
-- generators
------------------------------------------------------------------------------

-- | A chunked body, made of the given content split into chunks, with
-- sizes in either case and with leading zeros, extensions and trailers
-- from the RFC's grammar.
genBody :: Gen ([ByteString], [ByteString], ByteString)  -- ^ chunks, trailer lines, the body
genBody = do
  chunks <- listOf (B.pack <$> listOf1 arbitrary)
  trailerLines <- frequency [(2, pure []), (1, resize 8 (listOf (resize 6 (ruleStrings "field-line"))))]
  sizeLines <- mapM (\c -> sizeLineFor (B.length c)) chunks
  lastLine <- sizeLineFor (0 :: Int)
  let body = B.concat [ s <> "\r\n" <> c <> "\r\n" | (s, c) <- zip sizeLines chunks ]
             <> lastLine <> "\r\n" <> B.concat [ t <> "\r\n" | t <- trailerLines ] <> "\r\n"
  pure (chunks, trailerLines, body)
  where
    sizeLineFor n = do
      zeros <- elements [0, 0, 0, 1, 3]
      upper <- arbitrary
      ext <- frequency [(3, pure ""), (1, resize 6 (ruleStrings "chunk-ext"))]
      let digits = showHex n ""
      pure (C.replicate zeros '0' <> C.pack (if upper then map toUpper digits else digits) <> ext)

-- | Bodies, damaged or not, cut short or not, and followed by the next
-- request or not.
genInput :: Gen ByteString
genInput = do
  (_, _, body) <- genBody
  damaged <- frequency
    [ (4, pure body)
    , (2, damage body)
    , (1, (<> "\r\n") <$> resize 10 (ruleNearMisses "size-line"))
    , (1, (\l -> l <> "\r\n" <> body) <$> elements ["10000000000000000", "FFFFFFFFFFFFFFFF", "0000000000000000001"])
    , (1, (\t -> "0\r\n" <> t <> "\r\n\r\n") <$> resize 8 (oneof [ruleStrings "field-line", ruleNearMisses "field-line"]))
    , (1, (\ts -> "0\r\n" <> B.intercalate "\r\n" ts <> "\r\n\r\n") <$> (sublistOf
             ["A: b", "Content-Length: 0", "content-length:3", "Transfer-Encoding: chunked", "TRANSFER-ENCODING:", "Content-Lengths: 1", "C: d"] >>= shuffle))
    , (1, pure (B.take (B.length body `div` 2) body)) ]
  trailing <- elements ["", "", "GET / HTTP/1.1\r\n\r\n", "\r\n", "0\r\n\r\n"]
  pure (damaged <> trailing)
  where
    damage s = do
      i <- choose (0, max 0 (B.length s - 1))
      b <- elements (B.unpack "\r\n;= \t\"0aG:\0\\")
      op <- elements [0, 1, 2 :: Int]
      pure $ case op of
        0 -> B.take i s <> B.singleton b <> B.drop (i + 1) s
        1 -> B.take i s <> B.singleton b <> B.drop i s
        _ -> B.take i s <> B.drop (i + 1) s

------------------------------------------------------------------------------
-- properties
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # chunk          = chunk-size [ chunk-ext ] CRLF
-- # chunk-data CRLF
-- # chunk-size     = 1*HEXDIG
-- # last-chunk     = 1*("0") [ chunk-ext ] CRLF
-- # chunk-data     = 1*OCTET ; a sequence of chunk-size octets

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.1
-- # chunk-ext      = *( BWS ";" BWS chunk-ext-name
-- # [ BWS "=" BWS chunk-ext-val ] )
-- # chunk-ext-name = token
-- # chunk-ext-val  = token / quoted-string

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.2
-- # trailer-section   = *( field-line CRLF )
prop_reference :: Property
prop_reference = checkCoverage $ withMaxSuccess 2000 $ forAll genInput $ \s ->
  let want = reference s
  in cover 30 (either (const False) (const True) want) "complete" $
     cover 15 (want == Left "error") "rejected" $
     cover 5 (want == Left "incomplete") "incomplete" $
     cover 1 (want == Left "overflow") "overflow" $
     counterexample (show s) $
       outcome (decodeAll noLimits [s]) === want

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # A recipient MUST be able to parse and decode the chunked transfer
-- # coding.
prop_roundTrip :: Property
prop_roundTrip = withMaxSuccess 500 $ forAll genBody $ \(chunks, trailerLines, body) ->
  forAll (elements ["", "GET / HTTP/1.1\r\n\r\n"]) $ \next ->
    case decodeAll noLimits [body <> next] of
      Done content trailers n ->
        counterexample (show body) $
          (content, length trailers, n) === (B.concat chunks, length trailerLines, B.length body)
      r -> counterexample (show (body, r)) False

-- | The input arrives in pieces, and the result is what decoding all of
-- it at once gives, with the default limits.
prop_incremental :: Property
prop_incremental = withMaxSuccess 1000 $ forAll genInput $ \s ->
  forAll (sort <$> listOf (choose (0, B.length s))) $ \cuts ->
    let pieces = zipWith (\a b -> B.take (b - a) (B.drop a s)) (0 : cuts) (cuts ++ [B.length s])
    in counterexample (show (s, cuts)) $
         decodeAll defaultChunkedLimits pieces === decodeAll defaultChunkedLimits [s]

------------------------------------------------------------------------------
-- limits
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.1
-- # A server
-- # ought to limit the total length of chunk extensions received in a
-- # request to an amount reasonable for the services provided, in the
-- # same way that it applies length limitations and timeouts for other
-- # parts of a message, and generate an appropriate 4xx (Client Error)
-- # response if that amount is exceeded.
prop_limits :: Property
prop_limits = once $ conjoin
  [ -- chunk extensions: 1 KiB over the whole body
    done (chunk 1 (ext 1024) <> "0\r\n\r\n")
  , done (chunk 1 (ext 1000) <> "0" <> ext 24 <> "\r\n\r\n")
  , result (chunk 1 (ext 1000) <> "0" <> ext 25 <> "\r\n\r\n") === Failed ChunkExtensionsTooLong
  , result (chunk 1 (ext 1025) <> "0\r\n\r\n") === Failed ChunkExtensionsTooLong
    -- a chunk-size line: 64 octets and the extensions allowed, before
    -- its CRLF has arrived as well as after
  , done (C.replicate 63 '0' <> chunk 1 (ext 1024) <> "0\r\n\r\n")
  , result (C.replicate 64 '0' <> chunk 1 (ext 1024) <> "0\r\n\r\n") === Failed ChunkSizeLineTooLong
  , result (C.replicate 1088 '0') === Incomplete
  , result (C.replicate 1089 '0') === Failed ChunkSizeLineTooLong
    -- the trailer section: 8 KiB with its CRLFs and the final empty line
  , done ("0\r\n" <> trailerOf 8192)
  , result ("0\r\n" <> trailerOf 8193) === Failed TrailerSectionTooLarge
  , result ("0\r\n" <> B.take 8191 (trailerOf 9000)) === Incomplete
  , result ("0\r\n" <> B.take 8192 (trailerOf 9000)) === Failed TrailerSectionTooLarge
    -- the largest size there is, and one more
  , result "FFFFFFFFFFFFFFFF\r\n" === Incomplete
  , result "0FFFFFFFFFFFFFFFF\r\n" === Incomplete
  , result "10000000000000000\r\n" === Failed ChunkSizeTooLarge
  ]
  where
    result s = decodeAll defaultChunkedLimits [s]
    done s = case result s of
      Done _ _ n -> n === B.length s
      r          -> counterexample (show r) False
    chunk n e = C.pack (showHex (n :: Int) "") <> e <> "\r\n" <> C.replicate n 'x' <> "\r\n"
    -- chunk extensions of n octets
    ext n = ";" <> C.replicate (n - 1) 'a'
    -- a trailer section of n octets, with its CRLFs
    trailerOf n = "a: " <> C.replicate (n - 7) 'b' <> "\r\n\r\n"

------------------------------------------------------------------------------
-- examples
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # The chunked transfer coding is complete
-- # when a chunk with a chunk-size of zero is received, possibly followed
-- # by a trailer section, and finally terminated by an empty line.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1
-- # Therefore, recipients MUST
-- # anticipate potentially large hexadecimal numerals and prevent parsing
-- # errors due to integer conversion overflows or precision loss due to
-- # integer representation.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-7.1.1
-- # A recipient MUST ignore unrecognized chunk extensions.

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

-- | Trailers come back on their own, in 'Done', never with the header
-- section, which the decoder does not see.
prop_examples :: Property
prop_examples = once $ conjoin
  [ complete "4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n" "Wikipedia" []
  , complete "a\r\n0123456789\r\nA\r\n0123456789\r\n000\r\n\r\n" "01234567890123456789" []
  , complete "4;a=b; c ;d = \"e;\\\"f\"\r\nWiki\r\n0;g\r\n\r\n" "Wiki" []
  , complete "4\r\n\r\n\r\n\r\n0\r\n\r\n" "\r\n\r\n" []
  , complete "0\r\nExpires: never\r\nX-A:b \r\n\r\n" "" [("expires", "never"), ("x-a", "b")]
  , complete "0\r\n\r\n" "" []
    -- not complete
  , result "4\r\nWiki\r\n" === Incomplete
  , result "4\r\nWiki\r" === Incomplete
  , result "0\r\nA: b\r\n" === Incomplete
  , result "0\r\n" === Incomplete
    -- sizes
  , result "\r\n" === Failed InvalidChunkSize
  , result "0x4\r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkExtension
  , result "+4\r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkSize
  , result " 4\r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkSize
  , result "4 \r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkExtension
  , result "4\nWiki\r\n0\r\n\r\n" === Failed ChunkLineEnding
  , result "4\rx\r\nWiki\r\n0\r\n\r\n" === Failed ChunkLineEnding
    -- extensions
  , result "4;\r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkExtension
  , result "4;a=\r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkExtension
  , result "4;a=\"b\r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkExtension
  , result "4;a=b c\r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkExtension
  , result "4;a@b\r\nWiki\r\n0\r\n\r\n" === Failed InvalidChunkExtension
    -- the end of a chunk's data
  , result "4\r\nWikix\r\n0\r\n\r\n" === Failed MissingChunkEnd
  , result "4\r\nWiki\n0\r\n\r\n" === Failed MissingChunkEnd
  , result "4\r\nWik\r\n0\r\n\r\n" === Failed MissingChunkEnd
    -- trailers
  , result "0\r\n A: b\r\n\r\n" === Failed (InvalidTrailer LineFolding)
  , result "0\r\nA: b\r\n c\r\n\r\n" === Failed (InvalidTrailer LineFolding)
  , result "0\r\nA : b\r\n\r\n" === Failed (InvalidTrailer WhitespaceBeforeColon)
  , result "0\r\nA\r\n\r\n" === Failed (InvalidTrailer InvalidFieldLine)
  , result "0\r\nA: b\nC: d\r\n\r\n" === Failed (InvalidTrailer BareLineFeed)
    -- the fields of framing are not trailers
  , result "0\r\nContent-Length: 0\r\n\r\n" === Failed FramingFieldInTrailers
  , result "0\r\nA: b\r\ntransfer-encoding: chunked\r\n\r\n" === Failed FramingFieldInTrailers
  , result "0\r\nA: b\r\nCONTENT-LENGTH: 5\r\nC: d\r\n\r\n" === Failed FramingFieldInTrailers
  , complete "0\r\nContent-Lengths: 1\r\nContent-Type: a/b\r\n\r\n" "" [("content-lengths", "1"), ("content-type", "a/b")]
  ]
  where
    result s = decodeAll defaultChunkedLimits [s]
    -- a body, followed by the next request
    complete s content trailers = result (s <> "GET / HTTP/1.1\r\n\r\n") === Done content trailers (B.length s)
