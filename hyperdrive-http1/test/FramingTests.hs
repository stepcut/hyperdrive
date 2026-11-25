{-# LANGUAGE OverloadedStrings, PatternSynonyms, QuasiQuotes #-}
module FramingTests (framingTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import qualified Data.ByteString as B
import Hyperdrive.ABNF.Generate (nearMisses, validStrings)
import Hyperdrive.ABNF.Grammar (mkGrammar)
import Hyperdrive.ABNF.HTTP (rfc3986, rfc9110, rfc9110Errata, withErrata)
import Hyperdrive.ABNF.QQ (abnf)
import Hyperdrive.ABNF.Recognize (Library, ListForm(..), library, matches)
import Hyperdrive.ABNF.Types (mkRuleName)
import Hyperdrive.HTTP1.Framing
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message (HttpVersion, pattern HTTP_1_0, pattern HTTP_1_1)
import Hyperdrive.Semantics.Method (Method, pattern CONNECT, pattern GET, pattern HEAD, pattern OPTIONS, pattern POST)
import Hyperdrive.Semantics.Status (Status, StatusClass(..), mkStatus, statusClass, statusCode, pattern BadRequest, pattern NotImplemented)
import Data.Word (Word64)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

framingTests :: TestTree
framingTests = testGroup "Framing (RFC 9112 Section 6)"
  [ testProperty "the labelled samples agree with RFC 9110's grammars" prop_labels
  , testProperty "every combination of up to two lines of each field (exhaustive)" prop_exhaustive
  , testProperty "the RFC's examples, and the status for each error" prop_examples
  , testProperty "responses: a client reads exactly the body sent (exhaustive)" prop_responseReadBack
  , testProperty "responses: the RFC's MUST NOTs and SHOULDs (exhaustive)" prop_responseRules
  , testProperty "responses: the framing fields for each kind of framing" prop_framingFields
  , testProperty "TE: trailers, against RFC 9110's grammar" prop_acceptsTrailers
  ]

------------------------------------------------------------------------------
-- labelled samples
------------------------------------------------------------------------------

-- | Content-Length values, each with the numbers it lists, or Nothing if
-- it is not a list of decimal numbers. A value is read as a list
-- (RFC 9112 Section 6.3, rule 5), but one without empty elements.
contentLengths :: [(ByteString, Maybe [Integer])]
contentLengths =
  [ ("0", Just [0]), ("5", Just [5]), ("05", Just [5]), ("5, 5", Just [5, 5]), ("5,5", Just [5, 5])
  , ("5, 05", Just [5, 5]), ("5, 6", Just [5, 6]), ("5 ,\t5", Just [5, 5]), ("5,5,5", Just [5, 5, 5])
  , (", 5", Nothing), ("", Nothing), ("5,", Nothing), ("5,,5", Nothing), ("5, ,5", Nothing), (",", Nothing)
  , ("18446744073709551615", Just [18446744073709551615])
  , ("18446744073709551616", Just [18446744073709551616])
  , ("-1", Nothing), ("+5", Nothing), ("5.0", Nothing), ("0x10", Nothing), ("5 5", Nothing)
  , ("5;q=1", Nothing), ("\"5\"", Nothing) ]

-- | Transfer-Encoding values, each with its codings (the name in lower
-- case, and whether it has parameters), or Nothing if it is not a list
-- of transfer codings.
transferEncodings :: [(ByteString, Maybe [(ByteString, Bool)])]
transferEncodings =
  [ ("chunked", Just [("chunked", False)]), ("Chunked", Just [("chunked", False)])
  , ("CHUNKED", Just [("chunked", False)])
  , ("gzip, chunked", Just [("gzip", False), ("chunked", False)])
  , ("chunked, gzip", Just [("chunked", False), ("gzip", False)])
  , ("chunked, chunked", Just [("chunked", False), ("chunked", False)])
  , ("chunked;x=y", Just [("chunked", True)]), ("chunked ; x = y", Just [("chunked", True)])
  , ("gzip;q=\"a,b\", chunked", Just [("gzip", True), ("chunked", False)])
  , ("identity", Just [("identity", False)]), ("", Just [])
  , (",chunked,", Just [("chunked", False)]), ("x-unknown, chunked", Just [("x-unknown", False), ("chunked", False)])
  , ("chunked x", Nothing), ("chunked;", Nothing), ("chunked;x", Nothing), ("chunked=1", Nothing)
  , ("\"chunked\"", Nothing), ("chunked;x=", Nothing) ]

-- | RFC 9110's grammars: @#transfer-coding@ in the form recipients
-- accept lists, with empty elements, and @1#Content-Length@ in the form
-- senders generate them, without.
grammarMatches :: String -> ByteString -> Bool
grammarMatches r = either (error . show) id . matches Recipient lib "L" (fromJust (mkRuleName (C.pack r)))
  where
    lib = library
      [ ("L", grammar [abnf|
          lengths  = length *( ows "," ows length )
          length   = <Content-Length, see [HTTP], Section 8.6>
          ows      = <OWS, see [HTTP], Section 5.6.3>
          codings  = #coding
          coding   = <transfer-coding, see [HTTP], Section 10.1.4>
          |])
      , ("HTTP", grammar (withErrata rfc9110Errata rfc9110)), ("URI", grammar rfc3986) ]
    grammar = either (error . show) id . mkGrammar

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.6
-- # Content-Length = 1*DIGIT

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.4
-- # transfer-coding    = token *( OWS ";" OWS transfer-parameter )
-- # transfer-parameter = token BWS "=" BWS ( token / quoted-string )
prop_labels :: Property
prop_labels = once $ conjoin $
  [ counterexample (show v) $ grammarMatches "lengths" v === (lab /= Nothing) | (v, lab) <- contentLengths ]
  ++ [ counterexample (show v) $ grammarMatches "codings" v === (lab /= Nothing) | (v, lab) <- transferEncodings ]

------------------------------------------------------------------------------
-- the oracle
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.3
-- # The length of a message body is determined by one of the following
-- # (in order of precedence):

-- | RFC 9112's rules, in order, applied to the labels of the field lines.
expected :: HttpVersion -> [Maybe [Integer]] -> [Maybe [(ByteString, Bool)]] -> Either FramingError RequestFraming
expected version cls tes
  | not (null tes), version == HTTP_1_0 = Left TransferEncodingInHTTP10
  | not (null tes), not (null cls)      = Left BothLengthAndEncoding
  | not (null tes) = case sequence tes of
      Nothing -> Left InvalidTransferEncoding
      Just lists -> case reverse (concat lists) of
        ("chunked", True) : _ -> Left ChunkedWithParameters
        ("chunked", False) : earlier
          | "chunked" `elem` map fst earlier -> Left ChunkedTwice
          | (name, _) : _ <- reverse earlier -> Left (UnsupportedTransferCoding name)
          | otherwise                        -> Right Chunked
        _ -> Left ChunkedNotFinal
  | not (null cls) = case concat <$> sequence cls of
      Just (n : ns) | all (== n) ns, n < 2 ^ (64 :: Int) -> Right (ContentLength (fromInteger n))
      _ -> Left InvalidContentLength
  | otherwise = Right NoBody

-- | Every combination, for both versions, of none, one or two field
-- lines of each of Content-Length and Transfer-Encoding, drawn from the
-- samples: over 200,000 requests.
prop_exhaustive :: Property
prop_exhaustive = once $
  let upToTwo xs = [[]] ++ [ [x] | x <- xs ] ++ [ [x, y] | x <- xs, y <- xs ]
      cases = [ (v, cl, te) | v <- [HTTP_1_0, HTTP_1_1], cl <- upToTwo contentLengths, te <- upToTwo transferEncodings ]
      request cl te = fieldsFromList ([ (hContentLength, value v) | (v, _) <- cl ] ++ [ (hTransferEncoding, value v) | (v, _) <- te ])
      failures = [ (v, map fst cl, map fst te, got, want)
                 | (v, cl, te) <- cases
                 , let got = requestFraming v (request cl te)
                       want = expected v (map snd cl) (map snd te)
                 , got /= want ]
  in counterexample ("disagreements (version, Content-Length, Transfer-Encoding, got, expected): " ++ show (take 5 failures)) $
       length cases > 200000 .&&. null failures

value :: ByteString -> FieldValue
value = either (error . show) id . mkFieldValue

------------------------------------------------------------------------------
-- examples
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # For example,
-- #
-- # Transfer-Encoding: gzip, chunked
-- #
-- # indicates that the content has been compressed using the gzip coding
-- # and then chunked using the chunked coding while forming the message
-- # body.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.1
-- # A server that receives a request message with a transfer coding it
-- # does not understand SHOULD respond with 501 (Not Implemented).
prop_examples :: Property
prop_examples = once $ conjoin
  [ framing HTTP_1_1 [] [] === Right NoBody
  , framing HTTP_1_0 [] [] === Right NoBody
  , framing HTTP_1_1 ["42"] [] === Right (ContentLength 42)
  , framing HTTP_1_1 ["42", "42"] [] === Right (ContentLength 42)
  , framing HTTP_1_1 ["42", "43"] [] === Left InvalidContentLength
  , framing HTTP_1_1 ["42, 42"] [] === Right (ContentLength 42)
  , framing HTTP_1_1 ["42,"] [] === Left InvalidContentLength
  , framing HTTP_1_1 ["", "42"] [] === Left InvalidContentLength
  , framing HTTP_1_1 [] ["chunked"] === Right Chunked
  , framing HTTP_1_1 [] ["gzip", "chunked"] === Left (UnsupportedTransferCoding "gzip")
  , framing HTTP_1_1 [] ["gzip, chunked"] === Left (UnsupportedTransferCoding "gzip")
  , framing HTTP_1_1 ["5"] ["chunked"] === Left BothLengthAndEncoding
  , framing HTTP_1_0 ["5"] ["chunked"] === Left TransferEncodingInHTTP10
  , framing HTTP_1_1 [] ["chunked, gzip"] === Left ChunkedNotFinal
  , map framingErrorStatus [ TransferEncodingInHTTP10, BothLengthAndEncoding, InvalidTransferEncoding
                           , ChunkedNotFinal, ChunkedTwice, ChunkedWithParameters, InvalidContentLength ]
      === replicate 7 BadRequest
  , framingErrorStatus (UnsupportedTransferCoding "gzip") === NotImplemented
  ]
  where
    framing v cls tes = requestFraming v (fieldsFromList ([ (hContentLength, value c) | c <- cls ] ++ [ (hTransferEncoding, value t) | t <- tes ]))

------------------------------------------------------------------------------
-- responses
------------------------------------------------------------------------------

-- | Every combination of request version and method, final status, and
-- kind of payload.
responseCases :: [(HttpVersion, Method, Status, Maybe Payload)]
responseCases =
  [ (v, m, st, p)
  | v <- [HTTP_1_0, HTTP_1_1]
  , m <- [GET, HEAD, POST, OPTIONS, CONNECT]
  , Just st <- map mkStatus [200 .. 599]
  , p <- [ Nothing, Just (Payload (Just 0) False), Just (Payload (Just 7) False)
         , Just (Payload Nothing False), Just (Payload (Just 7) True), Just (Payload Nothing True) ] ]

-- | How a client determines the length of a response's body.
data ClientReads = ReadsNothing | Tunnel | ReadsLength Word64 | ReadsChunked | ReadsUntilClose
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.3
-- # 1.  Any response to a HEAD request and any response with a 1xx
-- # (Informational), 204 (No Content), or 304 (Not Modified) status
-- # code is always terminated by the first empty line after the
-- # header fields, regardless of the header fields present in the
-- # message, and thus cannot contain a message body or trailer
-- # section.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-6.3
-- # 8.  Otherwise, this is a response message without a declared message
-- # body length, so the message body length is determined by the
-- # number of octets received prior to the server closing the
-- # connection.

-- | RFC 9112 Section 6.3's rules as a client applies them to a
-- response, written independently of 'responseFraming', from the
-- request method, the status and the framing fields sent.
clientReads :: Method -> Status -> [(FieldName, FieldValue)] -> ClientReads
clientReads method status fs
  | method == HEAD || code < 200 || code == 204 || code == 304 = ReadsNothing
  | method == CONNECT && code >= 200 && code < 300 = Tunnel
  | (_ : _) <- te = if last (concatMap codings te) == "chunked" then ReadsChunked else ReadsUntilClose
  | [v] <- cl = ReadsLength (read (C.unpack (fieldValueBytes v)))
  | otherwise = ReadsUntilClose
  where
    code = statusCode status
    te = [ v | (n, v) <- fs, n == hTransferEncoding ]
    cl = [ v | (n, v) <- fs, n == hContentLength ]
    codings = C.split ',' . C.filter (/= ' ') . fieldValueBytes

-- | Whatever the response, a client following RFC 9112 reads exactly
-- the body that is sent: no more, which would eat the next response,
-- and no less.
prop_responseReadBack :: Property
prop_responseReadBack = once $
  let failures = [ (v, m, statusCode st, p, f, clientRead)
                 | (v, m, st, p) <- responseCases
                 , let f = responseFraming v m st p
                       clientRead = clientReads m st (framingFields f)
                 , not (consistent f clientRead) ]
      consistent f clientRead' = case (f, clientRead') of
        (HeaderOnly _, ReadsNothing)          -> True
        (HeaderOnly _, Tunnel)                -> True
        (SendLength n, ReadsLength n')        -> n == n'
        (SendChunked, ReadsChunked)           -> True
        (SendUntilClose, ReadsUntilClose)     -> True
        _                                     -> False
  in counterexample (show (take 5 failures)) (length responseCases > 20000 .&&. null failures)

-- | The requirements on what a server sends, each checked over every
-- case it applies to.
-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.3.2
-- # The server SHOULD send the same header fields in response to a HEAD
-- # request as it would have sent if the request method had been GET.
prop_responseRules :: Property
prop_responseRules = once $ conjoin
  [ rule "no Content-Length or Transfer-Encoding with 204 or a 2xx to CONNECT (RFC 9110 8.6, RFC 9112 6.1)" $
      \_ m st _ f -> not (code st == 204 || (m == CONNECT && statusClass st == Successful)) || null (framingFields f)
  , rule "no Content-Length with 304, whose 200's length is unknown" $
      \_ _ st _ f -> code st /= 304 || null (framingFields f)
  , rule "a response to HEAD has Content-Length only if it is GET's length (RFC 9110 8.6)" $
      \_ m st p f -> m /= HEAD || code st `elem` [204, 304] || f == HeaderOnly (p >>= payloadLength)
  , rule "a response to HEAD carries the Content-Length GET would send, when known (RFC 9110 9.3.2)" $
      \_ m st p f -> m /= HEAD || code st `elem` [204, 304] || case p >>= payloadLength of
        Just n  -> framingFields f == [(hContentLength, value (C.pack (show n)))]
        Nothing -> null (framingFields f)
  , rule "no Transfer-Encoding unless the request is HTTP/1.1 (RFC 9112 6.1)" $
      \v _ _ _ f -> v /= HTTP_1_0 || f /= SendChunked
  , rule "Content-Length whenever the length is known and nothing needs chunking (RFC 9110 8.6)" $
      \_ m st p f -> not (sendsBody m st) || case p of
        Just (Payload (Just n) False) -> f == SendLength n
        _                             -> True
  , rule "close-delimited only for HTTP/1.0 with an unknown length (RFC 9112 6.3)" $
      \v _ _ p f -> f /= SendUntilClose || (v == HTTP_1_0 && (p >>= payloadLength) == Nothing)
  , rule "trailers are sent, with chunked, to HTTP/1.1 clients" $
      \v m st p f -> not (sendsBody m st && v == HTTP_1_1 && maybe False payloadTrailers p) || f == SendChunked
  ]
  where
    code = statusCode
    sendsBody m st = m /= HEAD && code st `notElem` [204, 304] && not (m == CONNECT && statusClass st == Successful)
    rule name ok = counterexample name $
      let failures = [ (v, m, code st, p) | (v, m, st, p) <- responseCases, not (ok v m st p (responseFraming v m st p)) ]
      in counterexample (show (take 5 failures)) (null failures)

prop_framingFields :: Property
prop_framingFields = once $ conjoin
  [ framingFields (HeaderOnly Nothing) === []
  , framingFields (HeaderOnly (Just 12)) === [(hContentLength, value "12")]
  , framingFields (SendLength 0) === [(hContentLength, value "0")]
  , framingFields (SendLength 18446744073709551615) === [(hContentLength, value "18446744073709551615")]
  , framingFields SendChunked === [(hTransferEncoding, value "chunked")]
  , framingFields SendUntilClose === []
  ]

------------------------------------------------------------------------------
-- TE
------------------------------------------------------------------------------

-- | TE from RFC 9110, and a TE whose members include "trailers" itself,
-- written with the RFC's t-codings and the list syntax recipients
-- accept (RFC 9110 Section 5.6.1.2).
teLibrary :: Library
teLibrary = library
  [ ("L", grammar [abnf|
      ; RFC 9110 Appendix A has the sender form of the list; this is #t-codings
      ; as recipients accept it (Section 5.6.1.2)
      TE          = #t-codings
      t-codings   = <t-codings, see [HTTP], Section 10.1.4>
      OWS         = <OWS, see [HTTP], Section 5.6.3>
      te-trailers = *( [ t-codings ] OWS "," OWS ) "trailers" *( OWS "," OWS [ t-codings ] )
      |])
  , ("HTTP", grammar (withErrata rfc9110Errata rfc9110)), ("URI", grammar rfc3986) ]
  where grammar = either (error . show) id . mkGrammar

isTE, hasTrailersMember :: ByteString -> Bool
isTE = teMatcher "TE"
hasTrailersMember = teMatcher "te-trailers"

teMatcher :: String -> ByteString -> Bool
teMatcher r = let m = matches Recipient teLibrary "L" (fromJust (mkRuleName (C.pack r)))
              in either (error . show) id . m

genTE :: Gen ByteString
genTE = resize 8 $ oneof (map (either (error . show) id)
  [ validStrings Recipient teLibrary "L" (rule "TE"), validStrings Recipient teLibrary "L" (rule "te-trailers")
  , nearMisses Recipient teLibrary "L" (rule "TE"), nearMisses Recipient teLibrary "L" (rule "te-trailers") ])
  where rule = fromJust . mkRuleName

-- = https://www.rfc-editor.org/rfc/rfc9110#section-10.1.4
-- # As described in Section 6.5, a TE field with a "trailers" member sent
-- # in a request indicates that the client will not discard trailer
-- # fields.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.5.1
-- # The presence of the keyword "trailers" in the TE header field
-- # (Section 10.1.4) of a request indicates that the client is willing to
-- # accept trailer fields, on behalf of itself and any downstream
-- # clients.

-- | One or two TE field lines, read as their combination: a valid TE
-- with a trailers member.
prop_acceptsTrailers :: Property
prop_acceptsTrailers = checkCoverage $ withMaxSuccess 2000 $
  forAll (resize 2 (listOf1 (genTE `suchThat` validValue))) $ \lines' ->
    let values = [ fieldValueBytes v | Right v <- map mkFieldValue lines' ]
        combined = B.intercalate ", " values
        want = isTE combined && hasTrailersMember combined
        fields = fieldsFromList [ (fromJust (mkFieldName "te"), v) | Right v <- map mkFieldValue lines' ]
    in cover 25 want "trailers" $ cover 15 (not want && isTE combined) "valid, no trailers" $
       cover 10 (not (isTE combined)) "invalid" $
       counterexample (show values) $
         acceptsTrailers fields === want
     .&&. once (map (\v -> acceptsTrailers (te v)) examples === map snd teExamples)
  where
    validValue v = either (const False) (const True) (mkFieldValue v)
    te v = fieldsFromList [(fromJust (mkFieldName "TE"), either (error . show) id (mkFieldValue v))]
    examples = map fst teExamples

teExamples :: [(ByteString, Bool)]
teExamples =
  [ ("trailers", True), ("Trailers", True), ("TRAILERS, deflate;q=0.5", True), ("gzip, trailers", True)
  , (",trailers,", True), ("trailers;q=1", False), ("trailers ;q=1", False), ("deflate", False)
  , ("", False), ("\"trailers\"", False), ("trailers, x;", False), ("trailer", False), ("trailers x", False) ]
