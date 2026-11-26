{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
module ControlDataTests (controlDataTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (toUpper)
import Data.Maybe (fromJust, isJust)
import Data.Word (Word16, Word64)
import Grammar (valid)
import Hyperdrive.Semantics.ControlData
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Method (mkMethod)
import Hyperdrive.Semantics.Status (pattern BadRequest, pattern MisdirectedRequest, pattern OK, pattern NotFound)
import Hyperdrive.Semantics.Target
import Hyperdrive.Semantics.URI
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

controlDataTests :: TestTree
controlDataTests = testGroup "ControlData (RFC 9113 Section 8)"
  [ testProperty "a well-formed request maps to the request it describes" prop_wellFormed
  , testProperty "each rule broken once gives its error, malformed or refused" prop_nearMisses
  , testProperty "split Cookie fields are joined with \"; \" (RFC 9113 8.2.3's example)" prop_cookieExample
  , testProperty "trailers" prop_trailers
  , testProperty "responses: :status first, then the fields" prop_response
  ]

------------------------------------------------------------------------------
-- well-formed requests
------------------------------------------------------------------------------

-- | A request's parts, and the request they should map to.
data Req = Req
  { rContext  :: TargetContext
  , rPseudo   :: [(ByteString, ByteString)]  -- ^ in the order they are sent
  , rRegular  :: [(ByteString, ByteString)]  -- ^ likewise, after them
  , rExpected :: RequestControl
  }

instance Show Req where
  show r = show (rPseudo r ++ rRegular r)

fieldList :: Req -> [(ByteString, ByteString)]
fieldList r = rPseudo r ++ rRegular r

-- a host that parses (and is not an IPvFuture, which is refused)
genAuthority :: Gen (ByteString, Authority)
genAuthority = do
  host <- oneof
    [ elements ["example.com", "EXAMPLE.com", "127.0.0.1", "[::1]", "xn--bcher-kva.example", "a-b.c"]
    , valid "URI" "host" ]
  port <- frequency [(3, pure ""), (1, (\p -> ":" <> C.pack (show (p :: Word16))) <$> choose (1, 65535))]
  let text = host <> port
  case parseAuthority text of
    Right a | not (isFuture (authorityHost a)) -> pure (text, a)
    _ -> genAuthority
  where isFuture h = case h of { HostIPvFuture _ -> True; _ -> False }

-- a token: a method (case-sensitive) or a field name (lower case)
genToken :: Gen ByteString
genToken = C.pack <$> listOf1 (elements "abcdefghijklmnopqrstuvwxyz0123456789!#$%&'*+-.^_`|~")

genValue :: Gen ByteString
genValue = frequency
  [ (3, elements ["", "x", "text/html", "a b", "a\tb", "\"q, s\"", "caf\233"])
  , (1, valid "HTTP" "field-value") ]

genWellFormed :: Gen Req
genWellFormed = do
  secure <- arbitrary
  let ctxScheme = if secure then HTTPS else HTTP
  defaultAuth <- oneof [pure Nothing, Just . snd <$> genAuthority]
  trusted <- arbitrary
  let ctx = TargetContext ctxScheme defaultAuth trusted
  scheme <- if secure || trusted then elements ["http", "https", "HTTPS"] else elements ["http", "HTTP"]
  (authText, auth) <- genAuthority
  -- the authority from :authority, from Host, or from the default
  source <- elements (["pseudo", "host"] ++ ["default" | isJust defaultAuth] :: [String])
  -- Host beside :authority, the same authority however it is written
  hostToo <- arbitrary
  hostSpelling <- elements [authText, C.map toUpper authText]
  asterisk <- frequency [(1, pure True), (6, pure False)]
  method <- if asterisk then pure "OPTIONS" else oneof [elements ["GET", "POST", "OPTIONS", "get"], genToken]
  path <- if asterisk then pure "*" else valid "HTTP/1.1" "origin-form"
  -- regular fields, cookies, TE and Content-Length among them
  others <- listOf ((,) <$> suchThat genToken (`notElem` special) <*> genValue)
  -- whitespace around some values, which is removed (D11)
  wrapped <- traverse (\(n, v) -> do
                         pre <- elements ["", "", " ", "\t", " \t"]
                         post <- elements ["", "", " ", "\t "]
                         pure (n, pre <> v <> post, v)) others
  cookies <- listOf (elements ["a=b", "c=d; e=f", "g=h"])
  te <- arbitrary
  len <- oneof [pure Nothing, Just <$> (arbitrary :: Gen Word64)]
  regular <- shuffle ([(n, sent) | (n, sent, _) <- wrapped] ++ [("cookie", c) | c <- cookies] ++ [("te", "trailers") | te]
                      ++ [("content-length", C.pack (show n)) | Just n <- [len]]
                      ++ [("host", hostSpelling) | source == "host" || source == "pseudo" && hostToo])
  pseudos <- shuffle ([(":method", method), (":scheme", scheme), (":path", path)]
                      ++ [(":authority", authText) | source == "pseudo"])
  let authority = case source of
        "default" -> fromJust defaultAuth
        "host" -> fromJust (either (const Nothing) Just (parseAuthority hostSpelling))
        _ -> auth
      sch = fromJust (mkScheme scheme)
      target = if asterisk then TargetAsterisk else
        let (p, q) = B.break (== 0x3F) path
        in TargetURI sch authority (fromJust (mkPath p)) (if B.null q then Nothing else mkQuery (B.drop 1 q))
      -- the expected fields: the others in order, the cookies joined
      -- where the first was
      strip = B.dropWhileEnd (`elem` [0x20, 0x09]) . B.dropWhile (`elem` [0x20, 0x09])
      appFields = joinCookies [(n, strip v) | (n, v) <- regular, n `notElem` ["te", "content-length", "host"]]
  pure Req
    { rContext = ctx, rPseudo = pseudos, rRegular = regular
    , rExpected = RequestControl
        { controlMethod = fromJust (mkMethod method)
        , controlTarget = target
        , controlFields = fields appFields
        , controlContentLength = len
        , controlAcceptsTrailers = te } }
  where
    special = ["te", "host", "content-length", "cookie", "connection", "proxy-connection"
              , "keep-alive", "transfer-encoding", "upgrade"]
    joinCookies fs =
      let cs = [v | ("cookie", v) <- fs]
          go seen ((n, v) : rest)
            | n == "cookie" = if seen then go True rest else ("cookie", B.intercalate "; " cs) : go True rest
            | otherwise = (n, v) : go seen rest
          go _ [] = []
      in go False fs

fields :: [(ByteString, ByteString)] -> Fields
fields fs = fieldsFromList [(fromJust (mkFieldName n), either (error . show) id (mkFieldValue v)) | (n, v) <- fs]

same :: RequestControl -> RequestControl -> Property
same a b = conjoin
  [ controlMethod a === controlMethod b, controlTarget a === controlTarget b
  , fieldsToList (controlFields a) === fieldsToList (controlFields b)
  , controlContentLength a === controlContentLength b
  , controlAcceptsTrailers a === controlAcceptsTrailers b ]

prop_wellFormed :: Property
prop_wellFormed = forAll genWellFormed $ \r ->
  case requestFromFields (rContext r) (fieldList r) of
    Right got -> same got (rExpected r)
    Left e -> counterexample (show e) False

------------------------------------------------------------------------------
-- near misses
------------------------------------------------------------------------------

-- a simple valid request on a secured connection, to break one rule of
base :: [(ByteString, ByteString)]
base = [(":method", "GET"), (":scheme", "https"), (":authority", "example.com"), (":path", "/a?b"), ("accept", "*/*")]

secured :: TargetContext
secured = TargetContext HTTPS Nothing False

replace :: ByteString -> ByteString -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
replace n v = map (\(n', v') -> if n' == n then (n', v) else (n', v'))

without :: ByteString -> [(ByteString, ByteString)] -> [(ByteString, ByteString)]
without n = filter ((/= n) . fst)

malformed :: Malformation -> Maybe ControlDataError
malformed = Just . Malformed

refused :: Refusal -> Maybe ControlDataError
refused = Just . Refused BadRequest

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.1
-- # A request or response that contains a field that violates any of
-- # these conditions MUST be treated as malformed (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3
-- # Endpoints MUST treat a request or response that
-- # contains undefined or invalid pseudo-header fields as malformed
-- # (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3
-- # All pseudo-header fields MUST appear in a field block before all
-- # regular field lines.  Any request or response that contains a pseudo-
-- # header field that appears in a field block after a regular field line
-- # MUST be treated as malformed (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3
-- # The same pseudo-header field name MUST NOT appear more than once in a
-- # field block.  A field block for an HTTP request or response that
-- # contains a repeated pseudo-header field name MUST be treated as
-- # malformed (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.3.1
-- # All HTTP/2 requests MUST include exactly one valid value for the
-- # ":method", ":scheme", and ":path" pseudo-header fields, unless they
-- # are CONNECT requests (Section 8.5).  An HTTP request that omits
-- # mandatory pseudo-header fields is malformed (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.2
-- # Any message containing connection-
-- # specific header fields MUST be treated as malformed (Section 8.1.1).
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.2.2
-- # The only exception to this is the TE header field, which MAY be
-- # present in an HTTP/2 request; when it is, it MUST NOT contain any
-- # value other than "trailers".
--
-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.5
-- # A CONNECT request that does not conform to these restrictions is
-- # malformed (Section 8.1.1).
nearMisses :: [(String, TargetContext, [(ByteString, ByteString)], Maybe ControlDataError)]
nearMisses =
  [ ("valid", secured, base, Nothing)
  , ("upper-case name", secured, base ++ [("Accept", "x")], malformed (BadFieldName "Accept"))
  , ("space in a name", secured, base ++ [("a b", "x")], malformed (BadFieldName "a b"))
  , ("empty name", secured, base ++ [("", "x")], malformed (BadFieldName ""))
  , ("colon in a name", secured, base ++ [("a:b", "x")], malformed (BadFieldName "a:b"))
  , ("value with CR", secured, base ++ [("x", "a\rb")], malformed (BadFieldValue "x"))
  , ("value with LF", secured, base ++ [("x", "a\nb")], malformed (BadFieldValue "x"))
  , ("value with NUL", secured, base ++ [("x", "a\0b")], malformed (BadFieldValue "x"))
  , ("value starting with SP: stripped, not refused (D11)", secured, base ++ [("x", " a")], Nothing)
  , ("value ending with HTAB: stripped, not refused (D11)", secured, base ++ [("x", "a\t")], Nothing)
  , ("a control character inside stripped whitespace", secured, base ++ [("x", " \1 ")], malformed (BadFieldValue "x"))
  , ("value with a control character", secured, base ++ [("x", "a\1b")], malformed (BadFieldValue "x"))
  , ("pseudo-header value with LF", secured, replace ":path" "/a\n" base, malformed (BadFieldValue ":path"))
  , ("unknown pseudo-header", secured, (":foo", "x") : base, malformed (UnknownPseudoHeader ":foo"))
  , ("a response's pseudo-header", secured, (":status", "200") : base, malformed (UnknownPseudoHeader ":status"))
  , ("extended CONNECT's :protocol (not supported)", secured, (":protocol", "websocket") : base, malformed (UnknownPseudoHeader ":protocol"))
  , ("repeated :path", secured, (":path", "/") : base, malformed (RepeatedPseudoHeader ":path"))
  , ("pseudo-header after a field", secured, base ++ [(":x", "y")], malformed (PseudoHeaderAfterField ":x"))
  , ("missing :method", secured, without ":method" base, malformed (MissingPseudoHeader ":method"))
  , ("missing :scheme", secured, without ":scheme" base, malformed (MissingPseudoHeader ":scheme"))
  , ("missing :path", secured, without ":path" base, malformed (MissingPseudoHeader ":path"))
  , ("invalid :method", secured, replace ":method" "GE T" base, malformed (InvalidPseudoHeader ":method"))
  , ("empty :method", secured, replace ":method" "" base, malformed (InvalidPseudoHeader ":method"))
  , ("invalid :scheme", secured, replace ":scheme" "1http" base, malformed (InvalidPseudoHeader ":scheme"))
  , ("empty :path", secured, replace ":path" "" base, malformed (InvalidPseudoHeader ":path"))
  , ("relative :path", secured, replace ":path" "a" base, malformed (InvalidPseudoHeader ":path"))
  , (":path with a fragment", secured, replace ":path" "/a#b" base, malformed (InvalidPseudoHeader ":path"))
  , (":path with a space", secured, replace ":path" "/a b" base, malformed (InvalidPseudoHeader ":path"))
  , (":path as an absolute URI", secured, replace ":path" "https://example.com/" base, malformed (InvalidPseudoHeader ":path"))
  , ("CONNECT with :scheme", secured, connect ++ [(":scheme", "https")], malformed ConnectWithSchemeOrPath)
  , ("CONNECT with :path", secured, connect ++ [(":path", "/")], malformed ConnectWithSchemeOrPath)
  , ("CONNECT without :authority", secured, without ":authority" connect, malformed (MissingPseudoHeader ":authority"))
  , ("Connection", secured, base ++ [("connection", "close")], malformed (ConnectionSpecificField "connection"))
  , ("Proxy-Connection", secured, base ++ [("proxy-connection", "x")], malformed (ConnectionSpecificField "proxy-connection"))
  , ("Keep-Alive", secured, base ++ [("keep-alive", "x")], malformed (ConnectionSpecificField "keep-alive"))
  , ("Transfer-Encoding", secured, base ++ [("transfer-encoding", "chunked")], malformed (ConnectionSpecificField "transfer-encoding"))
  , ("Upgrade", secured, base ++ [("upgrade", "h2c")], malformed (ConnectionSpecificField "upgrade"))
  , ("TE: Trailers, in any case", secured, base ++ [("te", "Trailers")], Nothing)
  , ("TE: gzip", secured, base ++ [("te", "gzip")], malformed TENotTrailers)
  , ("TE: trailers, gzip", secured, base ++ [("te", "trailers, gzip")], malformed TENotTrailers)
  , ("Host differs from :authority", secured, base ++ [("host", "other.example")], malformed HostDiffersFromAuthority)
  , ("Host differs in port", secured, base ++ [("host", "example.com:444")], malformed HostDiffersFromAuthority)
  , ("Host with the default port agrees", secured, base ++ [("host", "EXAMPLE.com:443")], Nothing)
  , ("CONNECT: Host differs in port", secured, connect ++ [("host", "example.com")], malformed HostDiffersFromAuthority)
    -- refused, answered as HTTP/1.1 answers
  , (":authority with userinfo", secured, replace ":authority" "u@example.com" base, malformed (InvalidAuthority AuthorityHasUserinfo))
  , (":authority with a bad port", secured, replace ":authority" "example.com:99999" base, malformed (InvalidAuthority AuthorityInvalidPort))
  , ("empty :authority", secured, replace ":authority" "" base, malformed (InvalidAuthority AuthorityEmptyHost))
  , ("CONNECT :authority with userinfo", secured, replace ":authority" "u@example.com:443" connect, malformed (InvalidAuthority AuthorityHasUserinfo))
  , ("invalid Host beside :authority", secured, base ++ [("host", "u@x")], refused (InvalidHost AuthorityHasUserinfo))
  , ("invalid Host alone", secured, without ":authority" base ++ [("host", "u@x")], refused (InvalidHost AuthorityHasUserinfo))
  , ("two Host lines", secured, without ":authority" base ++ [("host", "a"), ("host", "a")], refused RepeatedHost)
  , ("two Host lines beside :authority", secured, base ++ [("host", "example.com"), ("host", "example.com")], refused RepeatedHost)
  , ("no authority and no default", secured, without ":authority" base, refused NoAuthority)
  , ("empty Host and no default", secured, without ":authority" base ++ [("host", "")], refused NoAuthority)
  , ("no authority, a default", TargetContext HTTPS (Just (either (error . show) id (parseAuthority "d.example"))) False, without ":authority" base, Nothing)
  , ("IPvFuture", secured, replace ":authority" "[v1.x]" base, refused UnsupportedIPvFuture)
  , ("CONNECT to an IPvFuture host", secured, replace ":authority" "[v1.x]:443" connect, refused UnsupportedIPvFuture)
  , ("CONNECT without a port", secured, replace ":authority" "example.com" connect, refused MissingConnectPort)
  , ("* with GET", secured, replace ":path" "*" base, refused AsteriskNotOptions)
  , ("* with OPTIONS", secured, replace ":method" "OPTIONS" (replace ":path" "*" base), Nothing)
  , ("invalid Content-Length", secured, base ++ [("content-length", "x")], refused InvalidContentLength)
  , ("differing Content-Length lines", secured, base ++ [("content-length", "1"), ("content-length", "2")], refused InvalidContentLength)
  , ("scheme ftp", secured, replace ":scheme" "ftp" base, Just (Refused MisdirectedRequest (MisdirectedScheme (fromJust (mkScheme "ftp")))))
  , ("https on cleartext", TargetContext HTTP Nothing False, base, Just (Refused MisdirectedRequest (MisdirectedScheme HTTPS)))
  , ("https on cleartext from a trusted gateway", TargetContext HTTP Nothing True, base, Nothing)
  , ("scheme ftp, even from a trusted gateway", TargetContext HTTP Nothing True, replace ":scheme" "ftp" base,
      Just (Refused MisdirectedRequest (MisdirectedScheme (fromJust (mkScheme "ftp")))))
  , ("http on a secured connection", secured, replace ":scheme" "http" base, Nothing)
  ]
  where
    connect = [(":method", "CONNECT"), (":authority", "example.com:443")]

prop_nearMisses :: Property
prop_nearMisses = once $ conjoin
  [ counterexample name $ either Just (const Nothing) (requestFromFields ctx list) === expected
  | (name, ctx, list, expected) <- nearMisses ]

prop_cookieExample :: Property
prop_cookieExample = once $
  fmap (fieldsToList . controlFields)
    (requestFromFields secured (base ++ [("cookie", "a=b"), ("x", "1"), ("cookie", "c=d"), ("cookie", "e=f")]))
  === Right (fieldsToList (fields [("accept", "*/*"), ("cookie", "a=b; c=d; e=f"), ("x", "1")]))

-- = https://www.rfc-editor.org/rfc/rfc9113#section-8.1
-- # Trailers MUST NOT
-- # include pseudo-header fields (Section 8.3).  An endpoint that
-- # receives pseudo-header fields in trailers MUST treat the request or
-- # response as malformed (Section 8.1.1).
prop_trailers :: Property
prop_trailers = once $ conjoin
  [ fmap fieldsToList (trailersFromFields [("x-sum", "abc"), ("x-n", "1")])
      === Right (fieldsToList (fields [("x-sum", "abc"), ("x-n", "1")]))
  , trailersFromFields [(":path", "/")] `isLeftWith` PseudoHeaderInTrailers ":path"
  , trailersFromFields [("content-length", "1")] `isLeftWith` FramingFieldInTrailers "content-length"
  , trailersFromFields [("transfer-encoding", "chunked")] `isLeftWith` ConnectionSpecificField "transfer-encoding"
  , trailersFromFields [("te", "trailers")] `isLeftWith` ConnectionSpecificField "te"
  , trailersFromFields [("X", "1")] `isLeftWith` BadFieldName "X"
  , fmap fieldsToList (trailersFromFields [("x", " 1\t")]) === Right (fieldsToList (fields [("x", "1")]))
  , trailersFromFields [("x", "1\r")] `isLeftWith` BadFieldValue "x"
  ]
  where
    isLeftWith r e = case r of
      Left e' -> e' === e
      Right _ -> counterexample "accepted" False

prop_response :: Property
prop_response = once $ conjoin
  [ responseToFields OK (fields [("content-type", "text/html"), ("x", "1")])
      === [(":status", "200"), ("content-type", "text/html"), ("x", "1")]
  , responseToFields NotFound (fields []) === [(":status", "404")]
  , trailersToFields (fields [("x-sum", "1")]) === [("x-sum", "1")]
  ]
