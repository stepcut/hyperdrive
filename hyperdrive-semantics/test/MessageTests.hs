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
    , testProperty "Host is rejected: the authority is in the target" prop_requestHost
    , testProperty "bytesBody reads its bytes, then the end"   prop_bytesBody
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
prop_requestTargetForms :: Property
prop_requestTargetForms = once $ monadicIO $ do
  results <- run $ sequence
    [ (\r -> (m, t, isRight r)) <$> request m target emptyFields
    | m <- standardMethods
    , (t, target) <- [("uri", uriTarget "/"), ("connect", connect), ("asterisk", TargetAsterisk)] ]
  assert (and [ ok == expected m t | (m, t, ok) <- results ])
  where
    expected :: Method -> String -> Bool
    expected m "connect"  = m == CONNECT
    expected m "asterisk" = m == OPTIONS
    expected _ _          = True

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
content Stream = ContentStream Nothing (\write _ -> write "hi" >> pure emptyFields)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.4.1
-- # All 1xx (Informational), 204 (No Content), and 304 (Not Modified)
-- # responses do not include content.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.4.1
-- # All other responses do include content, although that content might
-- # be of zero length.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15.3.6
-- # Since the 205 status code implies that no additional content will be
-- # provided, a server MUST NOT generate content in a 205 response.

-- | Every status from 100 to 599 with every kind of content.
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
      | code `elem` [204, 205, 304]     = if kind == None then Right () else Left ContentNotAllowed
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
  assert (results' results)
  where
    results' rs = and [ either (== RequestProtocolField hHost) (const False) r | r <- rs ]

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
  assert (length results == length standardMethods * (400 - 3))
  assert (and [ fmap (const ()) r == expected m code | (m, code, r) <- results ])
  where
    target m | m == CONNECT = connect
             | otherwise    = uriTarget "/"

-- = https://www.rfc-editor.org/rfc/rfc9110#section-6.5.1
-- # A sender MUST NOT
-- # generate a trailer field unless the sender knows the corresponding
-- # header field name's definition permits the field to be sent in
-- # trailers.
prop_checkTrailers :: Property
prop_checkTrailers = conjoin $
  [ counterexample (show n) $ checkTrailers (fields [("Server-Timing", "a"), (n, "1")]) === Left (field n)
  | n <- [ "Content-Length", "Transfer-Encoding", "Trailer", "Host", "Authorization"
         , "WWW-Authenticate", "Cache-Control", "Location", "Set-Cookie", "Content-Type"
         , "Content-Encoding", "Content-Range", "Range", "Expect", "content-type" ] ]
  ++
  -- RFC 9110 explicitly allows these in trailers
  [ counterexample (show n) $ fmap (const ()) (checkTrailers (fields [(n, "1")])) === Right ()
  | n <- [ "ETag", "Authentication-Info", "Proxy-Authentication-Info", "Accept-Ranges"
         , "Server-Timing", "Content-Digest", "X-Checksum" ] ]

prop_fileRange :: Property
prop_fileRange = conjoin
  [ fmap rangeLength (mkFileRange 0 maxBound)          === Just maxBound
  , fmap rangeOffset (mkFileRange maxBound 0)          === Just maxBound
  , mkFileRange 1 maxBound                             === Nothing
  , mkFileRange maxBound 1                             === Nothing
  , fmap rangeLength (mkFileRange 10 (maxBound - 10))  === Just (maxBound - 10)
  , mkFileRange 11 (maxBound - 10)                     === Nothing
  ]
