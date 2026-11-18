{-# LANGUAGE OverloadedStrings, PatternSynonyms, QuasiQuotes #-}
module TargetTests (targetTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Either (isRight)
import Data.Maybe (fromJust)
import Hyperdrive.ABNF.Generate (nearMisses, validStrings)
import Hyperdrive.ABNF.Grammar (mkGrammar)
import Hyperdrive.ABNF.HTTP (rfc3986, rfc9110, rfc9110Errata, rfc9112, rfc9112Errata, withErrata)
import Hyperdrive.ABNF.QQ (abnf)
import Hyperdrive.ABNF.Recognize (Library, ListForm(..), library, matches)
import Hyperdrive.ABNF.Types (mkRuleName)
import Hyperdrive.HTTP1.Target
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.Message (HttpVersion, pattern HTTP_1_0, pattern HTTP_1_1)
import Hyperdrive.Semantics.Method (Method, pattern CONNECT, pattern GET, pattern OPTIONS, pattern POST)
import Hyperdrive.Semantics.Status (statusCode)
import Hyperdrive.Semantics.Target (Target(..))
import Hyperdrive.Semantics.URI
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

targetTests :: TestTree
targetTests = testGroup "Request target (RFC 9112 Sections 3.2 and 3.3)"
  [ testProperty "agrees with RFC 9112's forms and RFC 9110's URI and Host grammars" prop_grammar
  , testProperty "Host is removed from the fields, and nothing else is" prop_fields
  , testProperty "the RFC's examples" prop_rfcExamples
  , testProperty "each rule, by example" prop_examples
  ]

------------------------------------------------------------------------------
-- the oracle
------------------------------------------------------------------------------

grammarLibrary :: Library
grammarLibrary = library
  [ ("T", grammar [abnf|
      origin-form    = <origin-form, see [HTTP/1.1], Section 3.2.1>
      authority-form = <authority-form, see [HTTP/1.1], Section 3.2.3>
      absolute-URI   = <absolute-URI, see [URI], Section 4.3>
      http-URI       = <http-URI, see [HTTP], Section 4.2.1>
      https-URI      = <https-URI, see [HTTP], Section 4.2.2>
      Host           = <Host, see [HTTP], Section 7.2>
      |])
  , ("HTTP/1.1", grammar (withErrata rfc9112Errata rfc9112))
  , ("HTTP", grammar (withErrata rfc9110Errata rfc9110))
  , ("URI", grammar rfc3986) ]
  where grammar = either (error . show) id . mkGrammar

isOriginForm, isAuthorityForm, isHttpURI, isHttpsURI, isHost :: ByteString -> Bool
isOriginForm    = ruleMatches "origin-form"
isAuthorityForm = ruleMatches "authority-form"
isHttpURI       = ruleMatches "http-URI"
isHttpsURI      = ruleMatches "https-URI"
isHost          = ruleMatches "Host"

ruleMatches :: String -> ByteString -> Bool
ruleMatches r = let m = matches Recipient grammarLibrary "T" (fromJust (mkRuleName (C.pack r)))
                in either (error . show) id . m

ruleStrings, ruleNearMisses :: String -> Gen ByteString
ruleStrings r = either (error . show) id (validStrings Recipient grammarLibrary "T" (fromJust (mkRuleName (C.pack r))))
ruleNearMisses r = either (error . show) id (nearMisses Recipient grammarLibrary "T" (fromJust (mkRuleName (C.pack r))))

-- | The target, or the status of the rejection, following RFC 9112
-- Sections 3.2 and 3.3 rule by rule, with the forms decided by the
-- RFCs' grammars. Once a string matches its rule, 'parseAuthority',
-- 'mkPath' and 'mkQuery' only build the values.
--
-- Beyond the grammars: an authority must have a non-empty host and no
-- userinfo (RFC 9110 Section 4.2.1 and 4.2.4), a port must fit in 16
-- bits, and IPvFuture is not served (design notes, P3). A target that
-- starts with a scheme other than http and https is 421 without
-- further checks: the request is not for this server, whatever the
-- rest of the URI says.
oracle :: TargetContext -> HttpVersion -> Method -> ByteString -> [ByteString] -> Either Int Target
oracle ctx version method raw hosts = do
  -- RFC 9112 Section 3.2: a missing (HTTP/1.1), repeated or invalid Host is 400
  hostAuthority <- case hosts of
    [] | version >= HTTP_1_1 -> Left 400
       | otherwise           -> Right Nothing
    [""] -> Right Nothing
    [h] | isHost h, servable h -> Right (Just (authority h))
        | otherwise            -> Left 400
    _ -> Left 400
  if method == CONNECT
    -- Section 3.2.3: the authority-form, with a port to connect to
    then if isAuthorityForm raw && servable raw && authorityPort (authority raw) `notElem` [Nothing, Just 0]
           then case authority raw of
                  Authority h (Just p) -> Right (TargetConnect h p)
                  _                    -> Left 400
           else Left 400
    else if raw == "*"
      -- Section 3.2.4
      then if method == OPTIONS then Right TargetAsterisk else Left 400
    else if isOriginForm raw
      -- Section 3.3: the authority is Host, or the configured default
      then do
        a <- maybe (maybe (Left 400) Right (contextDefaultAuthority ctx)) Right hostAuthority
        let (p, q) = pathQuery raw
        Right (TargetURI (contextScheme ctx) a p q)
    else if isHttpURI raw || isHttpsURI raw
      -- Section 3.2.2: Host is ignored
      then do
        let (s, rest) = C.break (== ':') raw
            scheme = fromJust (mkScheme s)
            (auth, pq) = C.break (\c -> c == '/' || c == '?') (B.drop 3 rest)
            (p, q) = pathQuery pq
        if scheme == HTTPS && contextScheme ctx /= HTTPS then Left 421 else Right ()
        if servable auth then Right (TargetURI scheme (authority auth) p q) else Left 400
    else if C.elem ':' raw && maybe False (`notElem` [HTTP, HTTPS]) (mkScheme (C.takeWhile (/= ':') raw))
      then Left 421
    else Left 400
  where
    -- a host and port this server can serve
    servable a =
      let (h, port) = hostPort a
      in not (B.null h) && not (C.elem '@' a) && C.take 2 h /= "[v" && C.take 2 h /= "[V"
         && maybe True (\ds -> B.length (C.dropWhile (== '0') ds) <= 5 && read ('0' : C.unpack ds) <= (65535 :: Int)) port
    hostPort a
      | C.take 1 a == "[" = let (h, r) = C.break (== ']') a in (h <> C.take 1 r, portOf (C.drop 1 r))
      | otherwise         = let (h, r) = C.break (== ':') a in (h, portOf r)
    portOf r = if C.take 1 r == ":" && B.length r > 1 then Just (C.drop 1 r) else Nothing
    pathQuery s =
      let (p, q) = C.break (== '?') s
      in (fromJust (mkPath p), if B.null q then Nothing else mkQuery (B.drop 1 q))

result :: TargetContext -> HttpVersion -> Method -> ByteString -> [ByteString] -> Either Int Target
result ctx version method raw hosts =
  either (Left . statusCode . targetErrorStatus) (Right . fst)
    (reconstructTarget ctx version method raw (hostFields hosts))

hostFields :: [ByteString] -> Fields
hostFields hosts = fieldsFromList [ (hHost, v) | Right v <- map mkFieldValue hosts ]

------------------------------------------------------------------------------
-- generators
------------------------------------------------------------------------------

genContext :: Gen TargetContext
genContext = TargetContext <$> elements [HTTP, HTTPS] <*> elements [Nothing, Just (authority "default.example:8080")]

-- | A method and a request-target as they can arrive in a request
-- line (visible ASCII), mostly in a form the method may use.
genRequestLine :: Gen (Method, ByteString)
genRequestLine = (`suchThat` (visible . snd)) $ resize 10 $ frequency
  [ (4, (,) <$> elements [GET, POST, OPTIONS] <*> ruleStrings "origin-form")
  , (3, (,) <$> elements [GET, POST] <*> ruleStrings "http-URI")
  , (2, (,) <$> elements [GET, POST] <*> ruleStrings "https-URI")
  , (2, (,) CONNECT <$> ruleStrings "authority-form")
  , (1, (,) <$> elements [GET, OPTIONS] <*> pure "*")
  , (1, (,) <$> elements [GET, POST, OPTIONS, CONNECT] <*> ruleStrings "absolute-URI")
  , (1, (,) <$> elements [GET, CONNECT] <*> ruleStrings "authority-form")
  , (1, (,) <$> elements [GET, OPTIONS] <*> ruleNearMisses "origin-form")
  , (1, (,) <$> elements [GET, POST] <*> ruleNearMisses "http-URI")
  , (1, (,) CONNECT <$> ruleNearMisses "authority-form") ]
  where visible t = not (B.null t) && B.all (\w -> w >= 0x21 && w <= 0x7E) t

-- | Host field lines: none, one, an empty one, two, or a near miss
genHosts :: Gen [ByteString]
genHosts = resize 8 $ frequency
  [ (1, pure [])
  , (4, (: []) <$> ruleStrings "Host")
  , (4, (: []) <$> elements ["a.example", "a.example:8080", "127.0.0.1", "[::1]:443", "A.EXAMPLE."])
  , (1, pure [""])
  , (1, vectorOf 2 (ruleStrings "Host"))
  , (2, (: []) <$> ruleNearMisses "Host") ]
  `suchThat` all (\h -> isRight (mkFieldValue h) && fieldValueBytes' h == h)
  where fieldValueBytes' = either (const "") fieldValueBytes . mkFieldValue

------------------------------------------------------------------------------
-- properties
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2
-- # A server MUST respond with a 400 (Bad Request) status code to any
-- # HTTP/1.1 request message that lacks a Host header field and to any
-- # request message that contains more than one Host header field line or
-- # a Host header field with an invalid field value.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2.2
-- # When an origin server receives a request with an absolute-form of
-- # request-target, the origin server MUST ignore the received Host
-- # header field (if any) and instead use the host information of the
-- # request-target.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2.2
-- # A server MUST accept the absolute-form in requests even though most
-- # HTTP/1.1 clients will only send the absolute-form to a proxy.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.4
-- # In particular, a request for an "https"
-- # resource MUST be rejected unless it has been received over a
-- # connection that has been secured via a certificate valid for that
-- # target URI's origin, as defined by Section 4.2.2.
prop_grammar :: Property
prop_grammar = checkCoverage $ withMaxSuccess 3000 $
  forAll genContext $ \ctx ->
  forAll (elements [HTTP_1_0, HTTP_1_1, HTTP_1_1, HTTP_1_1]) $ \version ->
  forAll genRequestLine $ \(method, raw) ->
  forAll genHosts $ \hosts ->
    let want = oracle ctx version method raw hosts
    in cover 30 (isRight want) "accepted" $
       cover 3 (either (== 421) (const False) want) "misdirected" $
       cover 5 (case want of Right TargetURI{} -> True; _ -> False) "target URI" $
       cover 1 (case want of Right TargetConnect{} -> True; _ -> False) "CONNECT" $
       counterexample (show (ctx, version, method, raw, hosts)) $
         result ctx version method raw hosts === want

-- | Every field but Host is kept, in order.
prop_fields :: Property
prop_fields = withMaxSuccess 200 $
  forAll (listOf (elements [("host", "a.example"), ("accept", "*/*"), ("x-a", "1"), ("x-a", "2")])) $ \fs ->
    let fields = fieldsFromList [ (fromJust (mkFieldName n), either (error . show) id (mkFieldValue v)) | (n, v) <- fs ]
        ctx = TargetContext HTTP (Just (Authority (HostIPv4 0x7F000001) Nothing))
    in case reconstructTarget ctx HTTP_1_0 GET "/" fields of
         Right (_, rest) -> map name (fieldsToList rest) === [ n | (n, _) <- fs, n /= "host" ]
         Left e -> property (length (filter ((== "host") . fst) fs) > 1 && e == RepeatedHost)
  where name (n, _) = fieldNameBytes n

------------------------------------------------------------------------------
-- examples
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.3
-- # Example 1: The following message received over a secure connection

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.3
-- # Example 2: The following message received over an insecure connection
prop_rfcExamples :: Property
prop_rfcExamples = once $ conjoin
  [ -- Section 3.3, Example 1: https://www.example.org/pub/WWW/TheProject.html
    target secure GET "/pub/WWW/TheProject.html" ["www.example.org"]
      === Right (uri HTTPS "www.example.org" "/pub/WWW/TheProject.html" Nothing)
    -- Section 3.3, Example 2: http://www.example.org:8080, the server as a whole
  , target plain OPTIONS "*" ["www.example.org:8080"] === Right TargetAsterisk
    -- Section 3.2.1: http://www.example.org/where?q=now
  , target plain GET "/where?q=now" ["www.example.org"]
      === Right (uri HTTP "www.example.org" "/where" (Just "q=now"))
    -- Section 3.2.2
  , target plain GET "http://www.example.org/pub/WWW/TheProject.html" ["www.example.org"]
      === Right (uri HTTP "www.example.org" "/pub/WWW/TheProject.html" Nothing)
    -- Section 3.2.3
  , target plain CONNECT "www.example.com:80" ["www.example.com"]
      === Right (TargetConnect (host "www.example.com") 80)
    -- Section 3.2.4, as a proxy would have sent it before forwarding
  , target plain OPTIONS "http://www.example.org:8001" ["www.example.org:8001"]
      === Right (uri HTTP "www.example.org:8001" "" Nothing)
  ]

prop_examples :: Property
prop_examples = once $ conjoin
  [ -- Host: required in HTTP/1.1, not in HTTP/1.0; one line; valid
    target plain GET "/" [] === Left MissingHost
  , target10 GET "/" [] === Left NoAuthority
  , reconstruct (TargetContext HTTP (Just (authority "d.example"))) HTTP_1_0 GET "/" [] === Right (uri HTTP "d.example" "/" Nothing)
  , target plain GET "/" [""] === Left NoAuthority
  , target plain GET "/" ["a.example", "a.example"] === Left RepeatedHost
  , target plain GET "/" ["a.example, b.example"] === Left (InvalidHost AuthorityInvalidHost)
  , target plain GET "/" ["user@a.example"] === Left (InvalidHost AuthorityHasUserinfo)
  , target plain GET "/" ["a.example:99999"] === Left (InvalidHost AuthorityInvalidPort)
  , target plain GET "/" ["[v1.x]"] === Left UnsupportedIPvFuture
  , target plain GET "/" ["A.Example:80"] === Right (uri HTTP "a.example:80" "/" Nothing)
    -- the absolute-form: Host ignored, but still checked
  , target plain GET "http://a.example/x" ["b.example"] === Right (uri HTTP "a.example" "/x" Nothing)
  , target plain GET "http://a.example/x" [] === Left MissingHost
  , target plain GET "http://a.example/x" ["b.example", "c.example"] === Left RepeatedHost
  , target plain GET "HTTP://A.example?q" [""] === Right (uri HTTP "a.example" "" (Just "q"))
  , target plain GET "http://user@a.example/" [""] === Left (InvalidTargetAuthority AuthorityHasUserinfo)
  , target plain GET "http:///x" [""] === Left (InvalidTargetAuthority AuthorityEmptyHost)
  , target plain GET "http:/x" [""] === Left InvalidTarget
  , target plain GET "http://a.example/x#f" [""] === Left InvalidTarget
  , target plain GET "https://a.example/" [""] === Left (MisdirectedScheme HTTPS)
  , target secure GET "https://a.example/" [""] === Right (uri HTTPS "a.example" "/" Nothing)
  , target secure GET "http://a.example/" [""] === Right (uri HTTP "a.example" "/" Nothing)
  , target plain GET "ftp://a.example/" [""] === Left (MisdirectedScheme (fromJust (mkScheme "ftp")))
  , target plain GET "1ttp://a.example/" [""] === Left InvalidTarget
    -- the forms each method may use
  , target plain GET "a.example:443" ["a.example"] === Left (MisdirectedScheme (fromJust (mkScheme "a.example")))
  , target plain GET "*" ["a.example"] === Left TargetFormNotForMethod
  , target plain CONNECT "/" ["a.example"] === Left TargetFormNotForMethod
  , target plain CONNECT "*" ["a.example"] === Left TargetFormNotForMethod
  , target plain CONNECT "a.example" ["a.example"] === Left MissingConnectPort
  , target plain CONNECT "a.example:" ["a.example"] === Left MissingConnectPort
  , target plain CONNECT "a.example:0" ["a.example"] === Left MissingConnectPort
  , target plain CONNECT "[::1]:443" ["a.example"] === Right (TargetConnect (host "[::1]") 443)
  , target plain CONNECT "http://a.example:443" ["a.example"] === Left (InvalidTargetAuthority AuthorityInvalidPort)
    -- the origin-form
  , target plain GET "//a" ["a.example"] === Right (uri HTTP "a.example" "//a" Nothing)
  , target plain GET "/a?" ["a.example"] === Right (uri HTTP "a.example" "/a" (Just ""))
  , target plain GET "/a?b?c/d" ["a.example"] === Right (uri HTTP "a.example" "/a" (Just "b?c/d"))
  , target plain GET "/a%2" ["a.example"] === Left InvalidTarget
  , target plain GET "/a[" ["a.example"] === Left InvalidTarget
    -- statuses
  , map targetErrorStatus' [MisdirectedScheme HTTPS, MissingHost, InvalidTarget] === [421, 400, 400]
  ]
  where
    target10 m raw hosts = reconstruct plain HTTP_1_0 m raw hosts
    targetErrorStatus' = statusCode . targetErrorStatus

------------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------------

plain, secure :: TargetContext
plain  = TargetContext HTTP Nothing
secure = TargetContext HTTPS Nothing

reconstruct :: TargetContext -> HttpVersion -> Method -> ByteString -> [ByteString] -> Either TargetError Target
reconstruct ctx version m raw hosts = fst <$> reconstructTarget ctx version m raw (hostFields hosts)

target :: TargetContext -> Method -> ByteString -> [ByteString] -> Either TargetError Target
target ctx = reconstruct ctx HTTP_1_1

authority :: ByteString -> Authority
authority = either (error . show) id . parseAuthority

host :: ByteString -> Host
host = authorityHost . authority

uri :: Scheme -> ByteString -> ByteString -> Maybe ByteString -> Target
uri s a p q = TargetURI s (authority a) (fromJust (mkPath p)) (q >>= mkQuery)
