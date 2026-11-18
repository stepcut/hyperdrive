{-# LANGUAGE OverloadedStrings, PatternSynonyms, QuasiQuotes #-}
module HeadTests (headTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List (sort)
import Data.Maybe (fromJust)
import Hyperdrive.ABNF.Generate (nearMisses, validStrings)
import Hyperdrive.ABNF.Grammar (mkGrammar)
import Hyperdrive.ABNF.HTTP (rfc3986, rfc9110, rfc9110Errata, rfc9112, rfc9112Errata, withErrata)
import Hyperdrive.ABNF.QQ (abnf)
import Hyperdrive.ABNF.Recognize (Library, ListForm(..), library, matches)
import Hyperdrive.ABNF.Types (mkRuleName)
import Hyperdrive.HTTP1.Head
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message (HttpVersion(..))
import Hyperdrive.Semantics.Method (methodBytes)
import Hyperdrive.Semantics.Status (statusCode)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

headTests :: TestTree
headTests = testGroup "Request head (RFC 9112 Sections 2 to 5)"
  [ testProperty "agrees with RFC 9112's grammar, on generated heads and near misses" prop_grammar
  , testProperty "reading in pieces gives the same result as reading at once" prop_incremental
  , testProperty "what is parsed is what was sent" prop_contents
  , testProperty "limits, at their boundaries" prop_limits
  , testProperty "each rule, by example" prop_examples
  , testProperty "the status for each error" prop_statuses
  ]

------------------------------------------------------------------------------
-- the grammar
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-2.1
-- # HTTP-message   = start-line CRLF
-- # *( field-line CRLF )
-- # CRLF
-- # [ message-body ]

-- | The head of a request as RFC 9112 defines it, from its own rules,
-- except that the request target is any visible ASCII here: it is
-- checked when it becomes a target.
grammarLibrary :: Library
grammarLibrary = library
  [ ("T", grammar [abnf|
      head         = request-line CRLF *( field-line CRLF ) CRLF
      request-line = method SP target SP HTTP-version
      ; heads of HTTP/1.x requests, which most generated inputs should be
      head-1       = method SP target SP %s"HTTP/1." DIGIT CRLF *( field-line CRLF ) CRLF
      target       = 1*%x21-7E
      method       = <method, see [HTTP/1.1], Section 3.1>
      HTTP-version = <HTTP-version, see [HTTP/1.1], Section 2.3>
      field-line   = <field-line, see [HTTP/1.1], Section 5>
      |])
  , ("HTTP/1.1", grammar (withErrata rfc9112Errata rfc9112))
  , ("HTTP", grammar (withErrata rfc9110Errata rfc9110))
  , ("URI", grammar rfc3986) ]
  where grammar = either (error . show) id . mkGrammar

isHead :: ByteString -> Bool
isHead = either (error . show) id . matches Recipient grammarLibrary "T" (fromJust (mkRuleName "head"))

genHead, genHead1, nearHead :: Gen ByteString
genHead  = either (error . show) id (validStrings Recipient grammarLibrary "T" (fromJust (mkRuleName "head")))
genHead1 = either (error . show) id (validStrings Recipient grammarLibrary "T" (fromJust (mkRuleName "head-1")))
nearHead = either (error . show) id (nearMisses Recipient grammarLibrary "T" (fromJust (mkRuleName "head-1")))

-- | heads, near misses, with empty lines before and a body after
genInput :: Gen ByteString
genInput = do
  h <- resize 20 (frequency [(2, genHead1), (1, genHead), (2, nearHead)])
  before <- elements ["", "", "", "\r\n", "\r\n\r\n"]
  trailing <- elements ["", "", "body", "GET / HTTP/1.1\r\n\r\n"]
  pure (before <> h <> trailing)

-- | What the result should be: the head runs from after any empty lines
-- to the first empty line that ends it, and is complete exactly when it
-- matches the grammar with HTTP major version 1.
expected :: ByteString -> Either String Int
expected s =
  let start = skip 0
      skip i = if B.take 2 (B.drop i s) == "\r\n" then skip (i + 2) else i
  in case B.breakSubstring "\r\n\r\n" (B.drop start s) of
       (_, rest) | B.null rest -> Left "incomplete"
       (before, _) ->
         let candidate = B.take (B.length before + 4) (B.drop start s)
             major = B.take 1 (B.drop 6 (snd (B.breakSubstring " HTTP/" (C.takeWhile (/= '\r') candidate))))
         in if not (isHead candidate) then Left "error"
            else if major == "1" then Right (start + B.length candidate)
            else Left "505"

outcome :: HeadResult -> Either String Int
outcome r = case r of
  HeadIncomplete _                    -> Left "incomplete"
  HeadComplete _ n                    -> Right n
  HeadError (UnsupportedVersion _)    -> Left "505"
  HeadError _                         -> Left "error"

-- = https://www.rfc-editor.org/rfc/rfc9112#section-5
-- # field-line   = field-name ":" OWS field-value OWS

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3
-- # request-line   = method SP request-target SP HTTP-version
prop_grammar :: Property
prop_grammar = checkCoverage $ withMaxSuccess 2000 $ forAll genInput $ \s ->
  let want = expected s
  in cover 20 (either (const False) (const True) want) "complete" $
     cover 15 (want == Left "error") "rejected" $
     cover 2 (want == Left "505") "unsupported version" $
     counterexample (show s) $
       outcome (parseRequestHead defaultLimits 0 s) === want

-- | The input arrives in pieces; after each, the parser is called again
-- with where its last search stopped. Until the head is complete the
-- result is incomplete (or an early rejection), and in the end it is
-- what parsing all of it at once gives.
prop_incremental :: Property
prop_incremental = withMaxSuccess 1000 $ forAll genInput $ \s ->
  forAll (sort <$> listOf (choose (0, B.length s))) $ \cuts ->
    let once' = parseRequestHead defaultLimits 0 s
        go _ [] = Nothing
        go searched (n : ns) = case parseRequestHead defaultLimits searched (B.take n s) of
          HeadIncomplete k -> go k ns
          r                -> Just (n, r)
        final = case go 0 (cuts ++ [B.length s]) of
          Just (_, r) -> r
          Nothing     -> parseRequestHead defaultLimits 0 s
    in counterexample (show (s, cuts)) $ show final === show once'

-- | A head built from known parts parses back to those parts.
prop_contents :: Property
prop_contents = withMaxSuccess 500 $
  forAll (elements ["GET", "POST", "PATCH", "X-Custom!"]) $ \m ->
  forAll (elements ["/", "/a/b?c=d", "*", "example.com:443", "http://example.com/x"]) $ \t ->
  forAll (elements [0, 1 :: Int]) $ \minor ->
  forAll (listOf ((,) <$> elements ["Host", "accept", "X-A", "Cookie"] <*> elements ["v", "a b", "", "\xFFx", "1, 2"])) $ \fs ->
    let wire = m <> " " <> t <> " HTTP/1." <> C.pack (show minor) <> "\r\n"
               <> B.concat [ n <> ":" <> sp <> v <> "\r\n" | ((n, v), sp) <- zip fs (cycle [" ", "", "\t "]) ] <> "\r\n"
    in case parseRequestHead defaultLimits 0 (wire <> "rest") of
         HeadComplete h n ->
           counterexample (show wire) $
             (methodBytes (headMethod h), headTarget h, headVersion h, n)
               === (m, t, HttpVersion 1 (fromIntegral minor), B.length wire)
             .&&. [ (fieldNameBytes k, fieldValueBytes v) | (k, v) <- fieldsToList (headFields h) ]
                    === [ (C.map toLower' k, v) | (k, v) <- fs ]
         r -> counterexample (show (wire, r)) False
  where toLower' c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c

------------------------------------------------------------------------------
-- limits
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3
-- # It is RECOMMENDED that all HTTP senders and recipients
-- # support, at a minimum, request-line lengths of 8000 octets.
prop_limits :: Property
prop_limits = once $ conjoin
  [ -- a request line of exactly the limit, and one octet more
    property (ok (requestLine 8192 <> "\r\n\r\n"))
  , err (requestLine 8193 <> "\r\n\r\n") === Just RequestTargetTooLong
    -- rejected before the head is complete, but not at exactly the limit
  , err (requestLine 9000) === Just RequestTargetTooLong
  , err (requestLine 8193) === Just RequestTargetTooLong
  , err (requestLine 8192) === Nothing
  , err (requestLine 8192 <> "\r\nHost: a\r\n") === Nothing
  , err (C.replicate 9000 'G') === Just MethodTooLong
  , ok (requestLine 8000)  === False
    -- the field section: 32 KiB with its CRLFs and the final empty line
    -- one field line, its CRLF and the final CRLF: 32 KiB, then one more
  , property (ok (head' [fieldOf (32768 - 4)]))
  , err (head' [fieldOf (32768 - 3)]) === Just FieldSectionTooLarge
  , err ("GET / HTTP/1.1\r\n" <> C.replicate 40000 'a') === Just FieldSectionTooLarge
    -- the number of field lines
  , property (ok (head' (replicate 100 "a: b")))
  , err (head' (replicate 101 "a: b")) === Just TooManyFields
  ]
  where
    -- a request line of n octets
    requestLine n = "GET /" <> C.replicate (n - 14) 'a' <> " HTTP/1.1"
    head' fs = "GET / HTTP/1.1\r\n" <> B.concat [ f <> "\r\n" | f <- fs ] <> "\r\n"
    -- a field line of n octets
    fieldOf n = "a: " <> C.replicate (n - 3) 'b'
    ok s = case parseRequestHead defaultLimits 0 s of
      HeadComplete _ _ -> True
      _                -> False
    err s = case parseRequestHead defaultLimits 0 s of
      HeadError e -> Just e
      _           -> Nothing

------------------------------------------------------------------------------
-- examples
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-2.2
-- # Although the line terminator for the start-line and fields is the
-- # sequence CRLF, a recipient MAY recognize a single LF as a line
-- # terminator and ignore any preceding CR.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-2.2
-- # A recipient of such a bare CR MUST consider that element to be
-- # invalid or replace each bare CR with SP before processing the element
-- # or forwarding the message.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-2.2
-- # A recipient that receives whitespace between the start-line and the
-- # first header field MUST either reject the message as invalid or
-- # consume each whitespace-preceded line without further processing of
-- # it

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

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3
-- # Although the request-line grammar rule requires that each of the
-- # component elements be separated by a single SP octet, recipients MAY
-- # instead parse on whitespace-delimited word boundaries
prop_examples :: Property
prop_examples = once $ conjoin
  [ result "GET / HTTP/1.1\r\nHost: a\r\n\r\n" === "complete 27"
  , result "\r\n\r\nGET / HTTP/1.1\r\n\r\n" === "complete 22"
  , result "GET / HTTP/1.1\r\nHost: a\r\n" === "incomplete"
  , result "GET / HTTP/1.1\nHost: a\n\n" === "incomplete"
  , result "GET / HTTP/1.1\r\nHost: a\n\r\n\r\n" === "BareLineFeed"
  , result "GET / HTTP/1.1\r\nHost: a\rb\r\n\r\n" === "BareCarriageReturn"
  , result "GET / HTTP/1.1\r\n Host: a\r\n\r\n" === "LineFolding"
  , result "GET / HTTP/1.1\r\nHost: a\r\n b\r\n\r\n" === "LineFolding"
  , result "GET / HTTP/1.1\r\n\tHost: a\r\n\r\n" === "LineFolding"
  , result "GET / HTTP/1.x\r\n\r\n" === "InvalidRequestLine"
  , result "GET / HTTP/1.1\r\nHost : a\r\n\r\n" === "WhitespaceBeforeColon"
  , result "GET / HTTP/1.1\r\nHost\t: a\r\n\r\n" === "WhitespaceBeforeColon"
  , result "GET / HTTP/1.1\r\nHost a\r\n\r\n" === "InvalidFieldLine"
  , result "GET / HTTP/1.1\r\n: a\r\n\r\n" === "InvalidFieldLine"
  , result "GET / HTTP/1.1\r\nHost: a\0b\r\n\r\n" === "InvalidFieldValue ContainsCRLFNUL"
  , result "GET  / HTTP/1.1\r\n\r\n" === "InvalidRequestLine"
  , result "GET\t/ HTTP/1.1\r\n\r\n" === "InvalidRequestLine"
  , result "GET / HTTP/1.1 \r\n\r\n" === "InvalidRequestLine"
  , result "GET / http/1.1\r\n\r\n" === "InvalidRequestLine"
  , result "GET / HTTP/1.10\r\n\r\n" === "InvalidRequestLine"
  , result "GET / HTTP/1\r\n\r\n" === "InvalidRequestLine"
  , result "GET /a\x80 HTTP/1.1\r\n\r\n" === "InvalidRequestLine"
  , result "G@T / HTTP/1.1\r\n\r\n" === "InvalidRequestLine"
  , result "GET / HTTP/2.0\r\n\r\n" === "UnsupportedVersion 2"
  , result "GET / HTTP/0.9\r\n\r\n" === "UnsupportedVersion 0"
  , result "GET / HTTP/1.0\r\n\r\n" === "complete 18"
  , result "GET / HTTP/1.9\r\n\r\n" === "complete 18"
  ]
  where
    result s = case parseRequestHead defaultLimits 0 s of
      HeadComplete _ n -> "complete " ++ show n
      HeadIncomplete _ -> "incomplete"
      HeadError e      -> show e

prop_statuses :: Property
prop_statuses = once $
  map (statusCode . headErrorStatus)
    [ MethodTooLong, RequestTargetTooLong, UnsupportedVersion 2, FieldSectionTooLarge, TooManyFields
    , RequestLineTooLong, InvalidRequestLine, BareLineFeed, BareCarriageReturn, LineFolding
    , WhitespaceBeforeColon, InvalidFieldLine ]
    === [501, 414, 505, 431, 431, 400, 400, 400, 400, 400, 400, 400]
