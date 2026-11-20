{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module MessageTests (messageTests) where

import qualified Data.ByteString.Char8 as C
import Data.Either (isLeft, isRight)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message
import Hyperdrive.Semantics.Method
import Hyperdrive.Semantics.Status
import Hyperdrive.Semantics.Target
import Hyperdrive.Semantics.URI
import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

messageTests :: TestTree
messageTests = testGroup "Message"
  [ testGroup "Requests"
    [ testProperty "the target form must suit the method"      prop_requestTargetForms
    , testProperty "an empty path is normalized"               prop_requestEmptyPath
    , testProperty "protocol fields are rejected"              prop_requestProtocolFields
    , testProperty "isProtocolField is exactly the seven names" prop_isProtocolField
    , testProperty "Host is rejected: the authority is in the target" prop_requestHost
    , testProperty "bytesBody reads its bytes, then the end"   prop_bytesBody
    , testProperty "trailers accepted: off until set, and nothing else changes" prop_acceptsTrailers
    ]
  , testGroup "Responses"
    [ testProperty "content rules for every status (exhaustive)" prop_responseContent
    , testProperty "protocol fields are rejected"              prop_responseProtocolFields
    , testProperty "Set-Cookie field lines stay separate"      prop_setCookie
    , testProperty "repeated singleton fields are rejected"    prop_repeatedSingletons
    , testProperty "a 2xx response to CONNECT is rejected"     prop_checkResponseFor
    , testProperty "fields that cannot be trailers"            prop_checkTrailers
    , testProperty "file ranges cannot overflow"               prop_fileRange
    ]
  ]

field :: C.ByteString -> FieldName
field = maybe (error "invalid field name") id . mkFieldName

value :: C.ByteString -> FieldValue
value = either (error "invalid field value") id . mkFieldValue

fields :: [(C.ByteString, C.ByteString)] -> Fields
fields ls = fieldsFromList [ (field n, value v) | (n, v) <- ls ]

uriTarget :: C.ByteString -> Target
uriTarget p = TargetURI HTTP (either (error "authority") id (parseAuthority "example.com"))
                             (maybe (error "path") id (mkPath p)) Nothing

connect :: Target
connect = maybe (error "connect") id (connectTarget (either (error "authority") id (parseAuthority "example.com:443")))

request :: Method -> Target -> Fields -> IO (Either RequestError Request)
request m t fs = mkRequest HTTP_1_1 m t fs <$> bytesBody ""

standardMethods :: [Method]
standardMethods = [GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE, PATCH]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.1
-- # These forms MUST
-- # NOT be used with other methods.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2.3
-- # The "authority-form" of request-target is only used for CONNECT
-- # requests (Section 9.3.6 of [HTTP]).
prop_requestTargetForms :: Property
prop_requestTargetForms = once $ monadicIO $ do
  results <- run $ sequence
    [ (\r -> (v, m, t, isRight r)) <$> (mkRequest v m target emptyFields <$> bytesBody "")
    | v <- [HTTP_1_0, HTTP_1_1, HTTP_2, HTTP_3]
    , m <- standardMethods
    , (t, target) <- [("uri", uriTarget "/"), ("connect", connect), ("asterisk", TargetAsterisk)] ]
  assert (and [ ok == expected v m t | (v, m, t, ok) <- results ])
  where
    expected :: HttpVersion -> Method -> String -> Bool
    expected _ m "connect"  = m == CONNECT
    expected _ m "asterisk" = m == OPTIONS
    -- an ordinary target with CONNECT is an extended CONNECT, which
    -- only HTTP/2 and HTTP/3 have
    expected v m _          = m /= CONNECT || v >= HTTP_2

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.3
-- # When not being used as the target of an OPTIONS request, an empty
-- # path component is equivalent to an absolute path of "/"
prop_requestEmptyPath :: Property
prop_requestEmptyPath = once $ monadicIO $ do
  get     <- run (request GET     (uriTarget "") emptyFields)
  options <- run (request OPTIONS (uriTarget "") emptyFields)
  assert (fmap requestTarget get == Right (uriTarget "/"))
  assert (fmap requestTarget options == Right (uriTarget ""))

protocolFieldNames :: [C.ByteString]
protocolFieldNames =
  [ "Connection", "Keep-Alive", "Proxy-Connection", "TE", "Transfer-Encoding"
  , "Upgrade", "Content-Length", "CONTENT-LENGTH", "transfer-encoding" ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.6.1
-- # Note that some versions of HTTP prohibit the use of fields for such
-- # information, and therefore do not allow the Connection field.
prop_requestProtocolFields :: Property
prop_requestProtocolFields = once $ monadicIO $ do
  rejected <- run $ mapM (\n -> request GET (uriTarget "/") (fields [("Accept", "*/*"), (n, "1")])) protocolFieldNames
  accepted <- run $ request GET (uriTarget "/") (fields [("Accept", "*/*"), ("Content-Type", "text/plain"), ("Trailer", "x")])
  assert (all isLeft rejected)
  assert (isRight accepted)
  assert (map isProtocolField [field "Host", field "Content-Type", field "Trailer"] == [False, False, False])

-- | The seven names and no others, including names of the same lengths
-- that differ in one octet.
prop_isProtocolField :: Property
prop_isProtocolField = conjoin
  [ once $ conjoin [ isProtocolField (field n) === True | n <- seven ]
  , once $ conjoin [ counterexample (show n') (isProtocolField (field n') === False)
                   | n <- seven, i <- [0 .. C.length n - 1]
                   , let n' = C.take i n <> "x" <> C.drop (i + 1) n, n' `notElem` seven ]
  , forAll (elements (seven ++ ["host", "accept", "user-agent", "trailer", "content-type", "x"])) $ \n ->
      forAll (C.pack <$> sublistOf (C.unpack n)) $ \n' ->
        not (C.null n') ==> isProtocolField (field n') === (n' `elem` seven)
  ]
  where
    seven = [ "connection", "keep-alive", "proxy-connection", "te"
            , "transfer-encoding", "upgrade", "content-length" ]

prop_bytesBody :: Property
prop_bytesBody = once $ monadicIO $ do
  body <- run (bytesBody "hello")
  chunks <- run (sequence [readChunk body, readChunk body, readChunk body])
  trailers <- run (readTrailers body)
  body' <- run (bytesBody "unread")
  trailers' <- run (readTrailers body')
  afterTrailers <- run (readChunk body')
  assert (bodyLength body == Just 5)
  assert (chunks == ["hello", "", ""])
  assert (fieldsToList trailers == [] && fieldsToList trailers' == [] && afterTrailers == "")

data ContentKind = None | Empty | Bytes | File | Stream
  deriving (Eq, Show, Enum, Bounded)

content :: ContentKind -> Content
content None   = ContentNone
content Empty  = ContentBytes ""
content Bytes  = ContentBytes "hello"
content File   = ContentFile "/dev/null" Nothing
content Stream = ContentStream Nothing [] (\write _ -> write "hi" >> pure emptyFields)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.4.1
-- # All 1xx (Informational), 204 (No Content), and 304 (Not Modified)
-- # responses do not include content.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.4.1
-- # All other responses do include content, although that content might
-- # be of zero length.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.3.6
-- # Since the 205 status code implies that no additional content will be
-- # provided, a server MUST NOT generate content in a 205 response.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.5.22
-- # The server MUST send an Upgrade header field in a
-- # 426 response to indicate the required protocol(s) (Section 7.8).

-- | Every status from 100 to 599 with every kind of content. A 205 has
-- content of zero length (RFC 9112 Section 6.3 does not end it at the
-- header section), and a 426 cannot be sent without Upgrade.
prop_responseContent :: Property
prop_responseContent = once $ conjoin
  [ counterexample (show (code, kind)) (result === expected code kind)
  | code <- [100 .. 599]
  , Just status <- [mkStatus code]
  , kind <- [minBound .. maxBound]
  , let result = either Left (const (Right ())) (mkResponse status emptyFields (content kind)) ]
  where
    expected :: Int -> ContentKind -> Either ResponseError ()
    expected code kind
      | code < 200                      = Left InterimStatus
      | code == 426                     = Left UpgradeNotSupported
      | code == 205                     = case kind of
          Empty -> Right ()
          None  -> Left ContentRequired
          _     -> Left ContentNotAllowed
      | code `elem` [204, 304]          = if kind == None then Right () else Left ContentNotAllowed
      | kind == None                    = Left ContentRequired
      | otherwise                       = Right ()

prop_responseProtocolFields :: Property
prop_responseProtocolFields = conjoin
  [ counterexample (show n) $
      either Just (const Nothing) (mkResponse OK (fields [("Content-Type", "text/plain"), (n, "1")]) (content Bytes))
        === Just (ResponseProtocolField (field n))
  | n <- protocolFieldNames ]
  .&&. isRight (mkResponse OK (fields [("Content-Type", "text/plain"), ("Cache-Control", "no-store")]) (content Bytes))

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # Since it cannot be combined into a single field value,
-- # |  recipients ought to handle "Set-Cookie" as a special case while
-- # |  processing fields.
prop_setCookie :: Property
prop_setCookie =
  case mkResponse OK (fields [("Set-Cookie", "a=1; Path=/"), ("X", "y"), ("Set-Cookie", "b=2, c")]) (content Empty) of
    Left e  -> counterexample (show e) False
    Right r -> map fieldValueBytes (lookupAll hSetCookie (responseHeaders r)) === ["a=1; Path=/", "b=2, c"]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.2
-- # The target URI's authority information is critical for handling a
-- # request.
prop_requestHost :: Property
prop_requestHost = once $ monadicIO $ do
  results <- run $ mapM (\n -> request GET (uriTarget "/") (fields [(n, "evil.example")])) ["Host", "HOST", "host"]
  -- with a protocol field as well, before or after it, that is reported
  both <- run $ mapM (request GET (uriTarget "/") . fields)
            [[("Host", "a"), ("TE", "trailers")], [("TE", "trailers"), ("Host", "a")]]
  assert (results' results)
  assert (all (either (== RequestProtocolField (field "te")) (const False)) both)
  where
    results' rs = and [ either (== RequestProtocolField hHost) (const False) r | r <- rs ]

prop_acceptsTrailers :: Property
prop_acceptsTrailers = once $ monadicIO $ do
  r <- run (request GET (uriTarget "/a") (fields [("Accept", "*/*")]))
  case r of
    Left e -> fail (show e)
    Right req -> do
      let summary q = (requestVersion q, requestMethod q, requestTarget q, requestHeaders q)
      assert (not (requestAcceptsTrailers req))
      assert (requestAcceptsTrailers (setAcceptsTrailers True req))
      assert (not (requestAcceptsTrailers (setAcceptsTrailers False (setAcceptsTrailers True req))))
      assert (summary (setAcceptsTrailers True req) == summary req)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.3
-- # Recipients often
-- # attempt to handle this error by using the last syntactically valid
-- # member of the list, leading to potential interoperability and
-- # security issues if different implementations have different error
-- # handling behaviors.
prop_repeatedSingletons :: Property
prop_repeatedSingletons = conjoin $
  [ counterexample (show n) $
      either Just (const Nothing) (mkResponse OK (fields [(n, "a"), ("X", "b"), (n, "c")]) (content Bytes))
        === Just (RepeatedSingleton (field n))
  | n <- singletons ]
  ++
  [ counterexample (show n) $ isRight (mkResponse OK (fields [(n, "a")]) (content Bytes))
  | n <- singletons ]
  ++
  -- list-based fields may be repeated
  [ counterexample (show n) $ isRight (mkResponse OK (fields [(n, "a"), (n, "b")]) (content Bytes))
  | n <- ["Cache-Control", "Vary", "Content-Encoding", "Content-Language", "Allow", "WWW-Authenticate", "Set-Cookie", "Link"] ]
  where
    singletons = [ "Content-Type", "Content-Range", "Content-Location", "Location", "ETag"
                 , "Last-Modified", "Retry-After", "Date", "Server", "Age", "Expires", "content-TYPE" ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.3.6
-- # Any 2xx (Successful) response indicates that the sender (and all
-- # inbound proxies) will switch to tunnel mode immediately after the
-- # response header section; data received after that header section is
-- # from the server identified by the request target.
prop_checkResponseFor :: Property
prop_checkResponseFor = once $ monadicIO $ do
  reqs <- run $ mapM (\m -> either (error . show) id <$> request m (target m) emptyFields) standardMethods
  let results = [ (requestMethod r, code, checkResponseFor r resp)
                | r <- reqs, code <- [200 .. 599], code `notElem` [204, 205, 304]
                , Just st <- [mkStatus code]
                , Right resp <- [mkResponse st emptyFields (content Bytes)] ]
      expected m code
        | m == CONNECT && code < 300 = Left TunnelNotSupported
        | otherwise                  = Right ()
  -- every status but 204, 205, 304 and 426
  assert (length results == length standardMethods * (400 - 4))
  assert (and [ fmap (const ()) r == expected m code | (m, code, r) <- results ])
  where
    target m | m == CONNECT = connect
             | otherwise    = uriTarget "/"

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.5.1
-- # A sender MUST NOT
-- # generate a trailer field unless the sender knows the corresponding
-- # header field name's definition permits the field to be sent in
-- # trailers.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # This means that, aside from the well-known exception noted below, a
-- # sender MUST NOT generate multiple field lines with the same name in a
-- # message (whether in the headers or trailers)
prop_checkTrailers :: Property
prop_checkTrailers = conjoin $
  -- fields that cannot be trailers cannot be declared
  [ counterexample (show n) $ fmap (const ()) (stream [] [n]) === Left (NotATrailer (field n))
  | n <- [ "Content-Length", "Transfer-Encoding", "Trailer", "Host", "Authorization"
         , "WWW-Authenticate", "Cache-Control", "Location", "Set-Cookie", "Content-Type"
         , "Content-Encoding", "Content-Range", "Range", "Expect", "content-type" ] ]
  ++
  -- RFC 9110 permits ETag and Accept-Ranges, Authentication-Info when the
  -- scheme does, and others are the application's to know
  [ counterexample (show n) $ fmap (const ()) (stream [] [n]) === Right ()
  | n <- [ "ETag", "Authentication-Info", "Accept-Ranges", "Server-Timing", "Content-Digest", "X-Checksum" ] ]
  ++
  -- a singleton may not be both a header and a trailer
  [ fmap (const ()) (stream [("ETag", "\"a\"")] ["ETag"]) === Left (RepeatedSingleton (field "ETag"))
  , fmap (const ()) (stream [("Server-Timing", "a")] ["Server-Timing"]) === Right ()
  ]
  ++
  -- the trailers sent: only declared fields, singletons once
  [ check ["ETag", "Server-Timing"] [("ETag", "\"a\""), ("Server-Timing", "b")] === Right ()
  , check [] [] === Right ()
  , check ["ETag"] [("Server-Timing", "b")] === Left (UndeclaredTrailer (field "Server-Timing"))
  , check ["ETag"] [("ETag", "\"a\""), ("ETag", "\"b\"")] === Left (RepeatedTrailer (field "ETag"))
  , check ["Server-Timing"] [("Server-Timing", "a"), ("Server-Timing", "b")] === Right ()
  , check ["etag"] [("ETag", "\"a\"")] === Right ()
  ]
  where
    stream hs names = mkResponse OK (fields hs) (ContentStream Nothing (map field names) (\_ _ -> pure emptyFields))
    check names ts = case stream [] names of
      Left e  -> error (show e)
      Right r -> fmap (const ()) (checkTrailers r (fields ts))

prop_fileRange :: Property
prop_fileRange = conjoin
  [ fmap rangeLength (mkFileRange 0 maxBound)          === Just maxBound
  , fmap rangeOffset (mkFileRange maxBound 0)          === Just maxBound
  , mkFileRange 1 maxBound                             === Nothing
  , mkFileRange maxBound 1                             === Nothing
  , fmap rangeLength (mkFileRange 10 (maxBound - 10))  === Just (maxBound - 10)
  , mkFileRange 11 (maxBound - 10)                     === Nothing
  ]
