{-# LANGUAGE OverloadedStrings #-}
-- | Tests driven by third-party data in test/data; see test/data/README.md.
module ThirdPartyTests (thirdPartyTests) where

import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import Data.List (nub, sort, (\\))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Grammar (ruleBytes)
import Hyperdrive.Semantics.FieldSyntax (parseQuotedString)
import Hyperdrive.Semantics.SafePath
import Hyperdrive.Semantics.URI
import System.Environment (lookupEnv)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

thirdPartyTests :: TestTree
thirdPartyTests = testGroup "Third-party data"
  [ testGroup "web-platform-tests urltestdata.json"
    [ testProperty "a host browsers read as an IP address is never a plain name to us" $
        once (ioProperty prop_wptAddresses)
    , testProperty "IPv6 addresses agree exactly" $
        once (ioProperty prop_wptIPv6)
    , testProperty "every other difference is a reviewed, expected one" $
        once (ioProperty prop_wptDifferences)
    , testProperty "RFC 9110's Host rule agrees with us, in every difference and on every authority" $
        once (ioProperty prop_wptDifferencesFollowRFC)
    , testProperty "a host we accept as a DNS host name, browsers accept as the same name" $
        once (ioProperty prop_wptDnsHostNames)
    ]
  , testProperty "PayloadsAllTheThings SSRF hosts are addresses, flagged, rejected or known names" $
      once (ioProperty prop_ssrfHosts)
  , testProperty "SecLists traversal payloads never escape through SafePath" $
      once (ioProperty prop_lfiPayloads)
  , testProperty "structured-field-tests strings agree with parseQuotedString" $
      once (ioProperty prop_sfStrings)
  ]

------------------------------------------------------------------------------
-- web-platform-tests
------------------------------------------------------------------------------

-- | What the URL Standard makes of a URL's host
data WptHost = WptFail | WptIPv4 String | WptIPv6 String | WptName String
  deriving (Eq, Show)

-- | the authority of each test whose input is a special-scheme URL
-- written as scheme://authority, and what the URL Standard makes of it
wptCases :: IO [(ByteString, WptHost)]
wptCases = do
  json <- A.eitherDecodeFileStrict "test/data/wpt/urltestdata.json"
  entries <- either fail pure json :: IO [A.Value]
  pure [ (TE.encodeUtf8 a, outcome o) | A.Object o <- entries, Just a <- [authorityOf o] ]
  where
    str k o = case KM.lookup k o of
      Just (A.String s) -> Just s
      _                 -> Nothing
    authorityOf o = do
      input <- str "input" o
      let (scheme, rest) = T.breakOn "://" input
      if T.toLower scheme `elem` ["http", "https", "ws", "wss", "ftp"] && "://" `T.isPrefixOf` rest
        then Just (T.takeWhile (`notElem` ("/?#" :: String)) (T.drop 3 rest))
        else Nothing
    outcome o
      | KM.lookup "failure" o == Just (A.Bool True) = WptFail
      | otherwise =
          case T.unpack <$> str "hostname" o of
            Just h@('[' : _) -> WptIPv6 h
            Just h | isDotted h -> WptIPv4 h
            Just h -> WptName h
            Nothing -> WptFail
    isDotted h = let ps = T.splitOn "." (T.pack h)
                 in length ps == 4 && all (\p -> not (T.null p) && T.all isDigit p) ps

-- | our own parse of a host written in the form the URL Standard
-- serializes, which is valid RFC 3986 syntax
hostOf :: String -> Host
hostOf h = either (error ("cannot parse " ++ h)) authorityHost (parseAuthority (C.pack h))

-- = https://www.rfc-editor.org/rfc/rfc3986#section-7.4
-- # However, they
-- # can become a security concern if an application attempts to filter
-- # access to resources based on the IP address in string literal format.

-- | Browsers read hosts like 0x7f.1, 192.168.257 or
-- %30%78%63%30%2e%30%32%35%30.01 as IPv4 addresses. Whenever they do, we
-- must reject the host, parse the same address, or flag it with
-- isNumericLookalike, so that an allow-list cannot see it as an ordinary
-- name.
prop_wptAddresses :: IO Property
prop_wptAddresses = do
  cases <- wptCases
  let addressCases = [ (a, ip) | (a, w) <- cases, Just ip <- [address w] ]
      address (WptIPv4 h) = Just (hostOf h)
      address (WptIPv6 h) = Just (hostOf h)
      address _           = Nothing
      safe (a, ip) = case authorityHost <$> parseAuthority a of
        Left _                -> True
        Right (HostRegName r) -> isNumericLookalike r || canonicalHost (HostRegName r) == ip
        Right h               -> canonicalHost h == ip
      failures = filter (not . safe) addressCases
  pure $ counterexample ("an address to browsers but a plain name or a different address to us: " ++ show failures)
           (null failures)
         .&&. counterexample "vacuous" (length addressCases >= 20)

prop_wptIPv6 :: IO Property
prop_wptIPv6 = do
  cases <- wptCases
  let checks = [ (a, h, authorityHost <$> parseAuthority a) | (a, WptIPv6 h) <- cases ]
      failures = [ c | c@(_, h, ours) <- checks, ours /= Right (hostOf h) ]
  pure $ counterexample ("different IPv6 address: " ++ show failures) (null failures)
         .&&. counterexample "vacuous" (length checks >= 5)

-- | Every other case where we and the URL Standard disagree is put in a
-- category with a reason, and listed in
-- test/data/wpt/expected-differences.txt. A difference that fits no
-- category fails the test, as does any change to the list, until the
-- file is reviewed and updated. Set HYPERDRIVE_UPDATE_EXPECTED=1 to
-- rewrite the file from the current results, then review the diff.
prop_wptDifferences :: IO Property
prop_wptDifferences = do
  cases <- wptCases
  let file = "test/data/wpt/expected-differences.txt"
      rows = sort [ (category a w ours, show (C.unpack a), describe w, ours)
                  | (a, w) <- cases, let ours = describeOurs a, differs w a ]
      current = [ c ++ "\t" ++ a ++ "\t" ++ w ++ "\t" ++ o | (c, a, w, o) <- rows ]
      unexplained = [ r | r@("unexplained", _, _, _) <- rows ]
  update <- lookupEnv "HYPERDRIVE_UPDATE_EXPECTED"
  if update == Just "1"
    then do writeFile file (unlines (header ++ current)); pure (property True)
    else do
      expected <- filter (\l -> not (null l) && take 1 l /= "#") . lines <$> readFile file
      let missing = filter (`notElem` expected) current
          gone    = filter (`notElem` current) expected
      pure $ counterexample ("differences in no category: " ++ show unexplained) (null unexplained)
             .&&. counterexample ("new differences:\n" ++ unlines missing ++ "no longer differing:\n" ++ unlines gone)
                    (null missing && null gone)
  where
    header =
      [ "# Hosts on which hyperdrive-semantics and the WHATWG URL Standard disagree,"
      , "# generated by ThirdPartyTests from urltestdata.json. Columns: category, the"
      , "# authority, what the URL Standard makes of it, what we make of it."
      , "# In every row, what we make of it is what RFC 9110's Host rule says: the"
      , "# test \"RFC 9110's Host rule agrees with us\" checks that."
      , "#"
      , "# Categories:"
      , "#   userinfo     We reject userinfo (user@host), as RFC 9110 Section 4.2.4"
      , "#                says to treat it as an error. The URL Standard ignores it."
      , "#   numeric      The URL Standard rejects a host that looks numeric but is"
      , "#                not a valid address. We accept it as a registered name"
      , "#                flagged by isNumericLookalike, which the protocol layer"
      , "#                rejects (docs/design-notes.md, P8)."
      , "#   not-rfc3986  The authority contains characters RFC 3986 does not allow"
      , "#                in a host (raw non-ASCII, backslash, whitespace, quotes),"
      , "#                which the URL Standard strips, maps or tolerates. We reject."
      , "#   encoded      A percent-encoded host. The URL Standard decodes it and"
      , "#                applies IDNA, giving a different name or a failure. We keep"
      , "#                the encoding, as RFC 3986 allows. Decoded, such a host can"
      , "#                contain NUL, CR, LF, \"/\", \"@\" or \":\", so anything that"
      , "#                decodes it is open to truncation, injection and URL"
      , "#                confusion. Browsers never send encoded hosts, so the"
      , "#                protocol layer rejects them (docs/design-notes.md, P10)."
      ]
    describe WptFail     = "fail"
    describe (WptIPv4 h) = "ipv4 " ++ h
    describe (WptIPv6 h) = "ipv6 " ++ h
    describe (WptName h) = "name " ++ h
    describeOurs a = case parseAuthority a of
      Left e -> "reject " ++ show e
      Right (Authority h _) -> case canonicalHost h of
        HostRegName r | isNumericLookalike r -> "numeric-lookalike " ++ C.unpack (regNameBytes r)
        h' -> "host " ++ C.unpack (renderHost h')
    category a w ours
      | ours == "reject AuthorityHasUserinfo"            = "userinfo"
      | w == WptFail, take 18 ours == "numeric-lookalike " = "numeric"
      | take 7 ours == "reject ", C.any notRfc3986 a     = "not-rfc3986"
      | C.elem '%' a, take 5 ours == "host "             = "encoded"
      | otherwise                                        = "unexplained"
    notRfc3986 ch = ch >= '\x80' || ch `elem` ("\\ \t\n\r\"`{}|^<>" :: String)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.2
-- # Host = uri-host [ ":" port ] ; Section 4

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.1
-- # A recipient that processes such a URI reference MUST
-- # reject it as invalid.

-- | Wherever we and the URL Standard disagree, the RFCs agree with us:
-- the categories in expected-differences.txt say why the URL Standard
-- differs, and this checks that in each case our answer is the RFC's.
--
-- More generally, on every authority in urltestdata.json we accept
-- exactly what RFC 9110's Host rule (with RFC 3986's host and port)
-- matches, but for two things the rule allows: an empty host, which
-- RFC 9110 Section 4.2.1 says to reject as invalid, and ports above
-- 65535, which we reject because http and https name a TCP port
-- (docs/design-notes.md).
prop_wptDifferencesFollowRFC :: IO Property
prop_wptDifferencesFollowRFC = do
  cases <- wptCases
  let verdicts = [ (a, w, parseAuthority a, ruleBytes "HTTP" "Host" a) | (a, w) <- cases ]
      accepted = either (const False) (const True)
      allowed _ (Left AuthorityEmptyHost) True = True
      allowed a (Left AuthorityInvalidPort) True = portAbove65535 a
      allowed _ ours rule = accepted ours == rule
      -- the digits after the last ":" that is not inside brackets
      portAbove65535 a =
        let port = C.takeWhileEnd (\ch -> ch /= ':' && ch /= ']') a
        in C.length port < B.length a && not (B.null port) && C.all isDigit port
             && (read (C.unpack port) :: Integer) > 65535
      differences = [ v | v@(a, w, _, _) <- verdicts, differs w a ]
      disagree vs = [ (C.unpack a, accepted ours, rule) | (a, _, ours, rule) <- vs, not (allowed a ours rule) ]
      excepted = [ (C.unpack a, ours) | (a, _, ours, rule) <- verdicts, accepted ours /= rule ]
  pure $ counterexample ("where we and the URL Standard differ, the Host rule does not side with us (authority, we accept, rule matches): "
                         ++ show (disagree differences)) (null (disagree differences))
         .&&. counterexample ("the Host rule and we differ on (authority, we accept, rule matches): "
                              ++ show (disagree verdicts)) (null (disagree verdicts))
         .&&. counterexample ("the exceptions (an empty host, a port above 65535) are no longer exercised: " ++ show excepted)
                (any ((== Left AuthorityEmptyHost) . snd) excepted && any ((== Left AuthorityInvalidPort) . snd) excepted)
         .&&. counterexample "vacuous" (length differences >= 100)

-- | Do we and the URL Standard disagree about an authority? Agreement is:
-- both reject, or both accept the same host after canonicalHost. IP
-- addresses are covered by their own properties.
differs :: WptHost -> ByteString -> Bool
differs WptFail a = either (const False) (const True) (parseAuthority a)
differs (WptName h) a = case parseAuthority a of
  Right (Authority host _) -> Just (canonicalHost host) /= (canonicalHost . authorityHost <$> either (const Nothing) Just (parseAuthority (C.pack h)))
  Left _ -> True
differs _ _ = False

-- | The guarantee behind requiring isDnsHostName of every Host
-- (docs/design-notes.md, P10): every host it accepts is one that
-- browsers also accept, as the same name. So none of the differences
-- in expected-differences.txt can reach a server that does so.
prop_wptDnsHostNames :: IO Property
prop_wptDnsHostNames = do
  cases <- wptCases
  let accepted = [ (a, r, w) | (a, w) <- cases
                             , Right (Authority (HostRegName r) _) <- [parseAuthority a]
                             , isDnsHostName r ]
      sameName r (WptName h) =
        either (const False) ((== canonicalHost (HostRegName r)) . canonicalHost . authorityHost)
               (parseAuthority (C.pack h))
      sameName _ _ = False
      failures = [ (a, w) | (a, r, w) <- accepted, not (sameName r w) ]
  pure $ counterexample ("accepted by isDnsHostName but not the same name to browsers: " ++ show failures)
           (null failures)
         .&&. counterexample "vacuous" (length accepted >= 50)

------------------------------------------------------------------------------
-- PayloadsAllTheThings
------------------------------------------------------------------------------

-- | Names that reach internal addresses only through DNS or the hosts
-- file. Nothing syntactic can catch them: an allow-list has to resolve
-- the name and check the address (docs/design-notes.md, A2).
resolvedNames :: [ByteString]
resolvedNames =
  [ "localhost", "localtest.me", "localh.st", "spoofed.redacted.oastify.com"
  , "company.127.0.0.1.nip.io", "ip6-localhost", "ip6-loopback"
  , "307.r3dir.me", "62epax5fhvj3zzmzigyoe5ipkbn7fysllvges3a.302.r3dir.me" ]

prop_ssrfHosts :: IO Property
prop_ssrfHosts = do
  hosts <- filter (not . B.null) . C.lines <$> B.readFile "test/data/payloadsallthethings/ssrf-hosts.txt"
  let outcomeOf a = case parseAuthority a of
        Left _ -> "rejected"
        Right (Authority h _) -> case canonicalHost h of
          HostIPv4 _ -> "address"
          HostIPv6 _ -> "address"
          HostRegName r
            | isNumericLookalike r -> "numeric look-alike"
            | otherwise            -> "name " ++ C.unpack (regNameBytes r)
          HostIPvFuture _ -> "rejected"
      names = nub (sort [ n | a <- hosts, 'n' : 'a' : 'm' : 'e' : ' ' : n <- [outcomeOf a] ])
      known = sort (map C.unpack resolvedNames)
  pure $ counterexample ("names that are not known to need resolving: " ++ show (names \\ known)
                          ++ "\nknown names that no longer appear: " ++ show (known \\ names))
           (names == known)
         .&&. counterexample "vacuous" (length hosts >= 40)
         & tabulate "outcome" [ takeWhile (/= ' ') (outcomeOf a) | a <- hosts ]
  where (&) = flip ($)

------------------------------------------------------------------------------
-- SecLists
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-17.3
-- # For example, UNIX, Microsoft Windows, and other operating systems use
-- # ".." as a path component to indicate a directory level above the
-- # current one

-- | Each payload is used as the path of a request: cut at the first "?"
-- or "#", as the request target's syntax would, and given a leading "/"
-- if it has none. Whatever SafePath accepts must not escape the
-- directory once joined into a file path.
prop_lfiPayloads :: IO Property
prop_lfiPayloads = do
  payloads <- filter (not . B.null) . map (C.filter (/= '\r')) . C.lines
                <$> B.readFile "test/data/seclists/LFI-Jhaddix.txt"
  let asPath p = let q = C.takeWhile (`notElem` ("?#" :: String)) p
                 in if C.take 1 q == "/" then q else "/" <> q
      outcome p = case mkPath (asPath p) of
        Nothing -> Left "not a valid path"
        Just path -> case safePathSegments path of
          Left e     -> Left ("SafePath: " ++ show e)
          Right segs -> Right (map safeSegmentBytes segs)
      escapes segs =
        let joined = B.intercalate "/" segs
            parts  = B.split 0x2F joined
        in (not (null segs) && parts /= segs)
           || any (`elem` [".", ".."]) parts
           || B.elem 0x00 joined
      accepted = [ (p, segs) | p <- payloads, Right segs <- [outcome p] ]
      failures = [ p | (p, segs) <- accepted, escapes segs ]
      -- not a failure on POSIX, but the file server must handle these on
      -- Windows (docs/design-notes.md, F5)
      windows  = [ p | (p, segs) <- accepted, any (C.elem '\\') segs ]
  pure $ counterexample ("escape the directory: " ++ show failures) (null failures)
         .&&. counterexample "vacuous" (length payloads > 500 && not (null accepted))
         & tabulate "outcome" [ either id (const "accepted") (outcome p) | p <- payloads ]
         & tabulate "accepted, but a backslash is a separator on Windows" [ show (length windows) ]
  where (&) = flip ($)

------------------------------------------------------------------------------
-- structured-field-tests
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.4
-- # Recipients
-- # that process the value of a quoted-string MUST handle a quoted-pair
-- # as if it were replaced by the octet following the backslash.

-- | RFC 9651 strings are a strict subset of RFC 9110 quoted-strings, so:
--
-- * a string RFC 9651 accepts decodes to the same value;
--
-- * of the strings RFC 9651 rejects, we accept exactly those that the
--   RFC 9110 grammar allows (tab, non-ASCII, escapes of characters other
--   than DQUOTE and backslash), as decided by RFC 9110's own grammar;
--
-- * and, as a check on the subset relation itself, the RFC 9110 grammar
--   accepts every string RFC 9651 accepts.
prop_sfStrings :: IO Property
prop_sfStrings = do
  tests <- concat <$> mapM load [ "test/data/structured-field-tests/string.json"
                                , "test/data/structured-field-tests/string-generated.json" ]
  let ours v = case parseQuotedString v of
        Right (s, rest) | B.null rest -> Just s
        _                             -> Nothing
      rfc9110 = ruleBytes "HTTP" "quoted-string"
      check :: (String, ByteString, Maybe ByteString) -> [(String, String)]
      check (name, v, Just want) =
        [ (name, "accepted by RFC 9651: same value") | ours v /= Just want ]
        ++ [ (name, "accepted by RFC 9651, but not by the RFC 9110 grammar") | not (rfc9110 v) ]
      check (name, v, Nothing)
        | rfc9110 v = [ (name, "RFC 9110 allows it, so we must accept") | ours v == Nothing ]
        | otherwise = [ (name, "RFC 9110 forbids it, so we must reject") | ours v /= Nothing ]
      failures = concatMap check tests
      outcome (_, v, Just _) | rfc9110 v = "accepted by both"
      outcome (_, v, Nothing)
        | rfc9110 v = "rejected by RFC 9651 only (tab, non-ASCII, other escapes)"
        | otherwise = "rejected by both"
      outcome _ = "?"
  pure $ counterexample ("failures: " ++ show failures) (null failures)
         .&&. counterexample "vacuous" (length tests > 250)
         & tabulate "outcome" (map outcome tests)
  where
    (&) = flip ($)
    -- (name, the field value as it arrives, the expected string or
    -- Nothing if RFC 9651 parsing must fail)
    load file = do
      json <- A.eitherDecodeFileStrict file
      entries <- either fail pure json :: IO [A.Value]
      pure [ (T.unpack name, value raw, expected o)
           | A.Object o <- entries
           , Just (A.String name) <- [KM.lookup "name" o]
           , Just (A.Array raw) <- [KM.lookup "raw" o] ]
    -- field lines are combined with ", ", and RFC 9651 parsing discards
    -- leading and trailing spaces
    value raw = C.dropWhileEnd (== ' ') . C.dropWhile (== ' ') . B.intercalate ", "
                  $ [ TE.encodeUtf8 t | A.String t <- foldr (:) [] raw ]
    expected o
      | KM.lookup "must_fail" o == Just (A.Bool True) = Nothing
      | otherwise = case KM.lookup "expected" o of
          Just (A.Array e) | (A.String v : _) <- foldr (:) [] e -> Just (TE.encodeUtf8 v)
          _ -> Nothing
