{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module RecognizeTests (recognizeTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List (nub, sort)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hyperdrive.ABNF.Grammar
import Hyperdrive.ABNF.QQ
import Hyperdrive.ABNF.Recognize
import Hyperdrive.ABNF.Types
import Numeric.Natural (Natural)
import RFCTests (Spec(..), loadSpecs)
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

recognizeTests :: IO TestTree
recognizeTests = do
  (http, http1, uri) <- loadSpecs
  let lib = library [ ("HTTP", specGrammar http), ("HTTP/1.1", specGrammar http1)
                    , ("URI", specGrammar uri) ]
  pure $ testGroup "Recognizer"
    [ testProperty "terminal values"                 prop_terminals
    , testProperty "alternatives are all tried"      prop_ambiguity
    , testProperty "repetition bounds (exhaustive)"  prop_repetitionBounds
    , testProperty "# lists, RFC 9110 examples"      prop_listExamples
    , testProperty "# lists, sender and recipient"   prop_listForms
    , testProperty "agrees with a naive matcher"     prop_naive
    , testProperty "errors"                          prop_errors
    , testProperty "RFC 3986 URIs"                   (prop_uri lib)
    , testProperty "RFC 9110 fields"                 (prop_http lib)
    , testProperty "RFC 9112 messages"               (prop_http1 lib)
    , testProperty "long inputs"                     (prop_long lib)
    ]

name :: ByteString -> RuleName
name = fromJust . mkRuleName

grammar :: RuleList -> Grammar
grammar = either (error . show) id . mkGrammar

-- | match the whole input against a rule of a single grammar
match' :: ListForm -> RuleList -> ByteString -> ByteString -> Either RecognizeError Bool
match' form rules r = matches form (library [("T", grammar rules)]) "T" (name r)

accepts, rejects :: RuleList -> ByteString -> [ByteString] -> Property
accepts rules r inputs = conjoin
  [ counterexample (C.unpack i) (match' Recipient rules r i === Right True) | i <- inputs ]
rejects rules r inputs = conjoin
  [ counterexample (C.unpack i) (match' Recipient rules r i === Right False) | i <- inputs ]

------------------------------------------------------------------------------
-- RFC 5234
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.1
-- # will match only "aBc" and will not match "abc", "Abc", "abC", "ABc",
-- # "aBC", "AbC", or "ABC".

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.1
-- # will both match "abc", "Abc", "aBc", "abC", "ABc", "aBC", "AbC", and
-- # "ABC".

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.3
-- # will match only the string that comprises only the lowercase
-- # characters, abc.
prop_terminals :: Property
prop_terminals = once $ conjoin
  [ accepts g "i" ["abc", "Abc", "aBc", "abC", "ABc", "aBC", "AbC", "ABC"]
  , accepts g "s" ["aBc"]
  , rejects g "s" ["abc", "Abc", "abC", "ABc", "aBC", "AbC", "ABC"]
  , accepts g "d" ["abc"]
  , rejects g "d" ["Abc", "ABC", "ab", "abcd"]
  , accepts g "x" ["abc"]
  , accepts g "range" ["0", "5", "9"]
  , rejects g "range" ["/", ":", "", "00"]
    -- only letters are folded
  , rejects g "punct" ["{"]
  , accepts g "punct" ["["]
    -- characters are integers; bytes never match values above 255
  , rejects g "big" ["\x00", "\xFF", "\x01\x00"]
  , accepts g "bytes" ["\x00\xFF"]
  , accepts g "empty" [""]
  , rejects g "empty" ["a"]
  ]
  where
    g = [abnf|
      i     = %i"aBc"
      s     = %s"aBc"
      d     = %d97 %d98 %d99
      x     = %x61.62.63
      range = %x30-39
      punct = "["
      big   = %x100 / %x100-1FF
      bytes = %x00 %xFF
      empty = ""
      |]

prop_ambiguity :: Property
prop_ambiguity = once $ conjoin
  [ accepts g "y" ["ab", "abb"]
  , rejects g "y" ["a", "abbb"]
    -- a greedy matcher would take all the a's and fail
  , accepts g "greedy" ["aa", "aaa", "aaaaaa"]
  , rejects g "greedy" ["a", ""]
  , accepts g "opt" ["b", "ab"]
  , accepts g "nested" ["", "a", "aa", "aaaa"]
  , fmap Set.toList (matchEnds Recipient (library [("T", grammar g)]) "T" (name "x") "abc") === Right [1, 2]
  ]
  where
    g = [abnf|
      x      = "a" / "ab"
      y      = x "b"
      greedy = *"a" "a" "a"
      opt    = ["a"] "b"
      nested = *( *"a" )
      |]

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.6
-- # Default values are 0 and infinity so that *<element> allows any
-- # number, including zero; 1*<element> requires at least one;
-- # 3*3<element> allows exactly 3; and 1*2<element> allows one or two.

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.7
-- # That is, exactly <n> occurrences of <element>.

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.8
-- # [foo bar]
-- #
-- # is equivalent to
-- #
-- # *1(foo bar).
prop_repetitionBounds :: Property
prop_repetitionBounds = once $ conjoin
  [ counterexample (show (lo, hi, k)) $
      match' Recipient (rules lo hi "\"a\"") "r" (C.replicate k 'a')
        === Right (fromIntegral k >= lo && maybe True (fromIntegral k <=) hi)
  | lo <- [0 .. 4], hi <- Nothing : map Just [0 .. 4], k <- [0 .. 6 :: Int] ]
  .&&. conjoin
  -- elements that match the empty string reach any count of zero or
  -- more a's (up to hi a's at most for a single "a" each)
  [ counterexample (show (lo, hi, el, k)) $
      match' Recipient (rules lo hi el) "r" (C.replicate k 'a')
        === Right (case el of
                     "*\"a\"" -> hi /= Just 0 || k == 0
                     _        -> maybe True (fromIntegral k <=) hi)
  | lo <- [0 .. 4], hi <- Nothing : map Just [0 .. 4]
  , el <- ["( \"a\" / \"\" )", "[ \"a\" ]", "*\"a\""], k <- [0 .. 6 :: Int]
  , maybe True (lo <=) hi ]
  .&&. conjoin
  [ accepts [abnf|r = 3"a"|] "r" ["aaa"], rejects [abnf|r = 3"a"|] "r" ["aa", "aaaa"]
  , accepts [abnf|r = ["a" "b"]|] "r" ["", "ab"], rejects [abnf|r = ["a" "b"]|] "r" ["a", "abab"]
  , match' Recipient [abnf|r = 3*2"a"|] "r" "aa" === Right False
  ]
  where
    rules :: Natural -> Maybe Natural -> ByteString -> RuleList
    rules lo hi el =
      let src = "r = " <> C.pack (show lo) <> "*" <> maybe "" (C.pack . show) hi <> "(" <> el <> ")"
      in either (error . show) id (parseQuoted (C.unpack src))

------------------------------------------------------------------------------
-- RFC 9110 lists
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # Then the following are valid values for example-list (not including
-- # the double quotes, which are present for delimitation only):
-- #
-- # "foo,bar"
-- # "foo ,bar,"
-- # "foo , ,bar,charlie"

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # In contrast, the following values would be invalid, since at least
-- # one non-empty element is required by the example-list production:
-- #
-- # ""
-- # ","
-- # ",   ,"
prop_listExamples :: Property
prop_listExamples = once $ conjoin
  [ accepts g "example-list" ["foo,bar", "foo ,bar,", "foo , ,bar,charlie"]
  , rejects g "example-list" ["", ",", ",   ,"]
  ]
  where
    g = [abnf|
      example-list      = 1#example-list-elmt
      example-list-elmt = token
      token             = 1*%x61-7A
      |]

prop_listForms :: Property
prop_listForms = once $ conjoin
  [ checkForms "a"            [(Sender, one'), (Recipient, one')]
  , checkForms "a,b"          [(Sender, two), (Recipient, two)]
  , checkForms "a , b\t,\tc"  [(Sender, three), (Recipient, three)]
  , checkForms "a,b,c"        [(Sender, three), (Recipient, three)]
  , checkForms ""             [(Sender, zero), (Recipient, zero)]
  , checkForms ","            [(Sender, none), (Recipient, zero)]
  , checkForms ",   ,"        [(Sender, none), (Recipient, zero)]
  , checkForms "a,,b"         [(Sender, none), (Recipient, two)]
  , checkForms "a, ,b"        [(Sender, none), (Recipient, two)]
  , checkForms ",a"           [(Sender, none), (Recipient, one')]
  , checkForms "a,"           [(Sender, none), (Recipient, one')]
  , checkForms "a b"          [(Sender, none), (Recipient, none)]
  , checkForms " a"           [(Sender, none), (Recipient, none)]
  , checkForms "a,"           [(Sender, none), (Recipient, one')]
  ]
  where
    -- list0 = #e, list1 = 1#e, list12 = 1#2e, list2 = 2#e, list02 = #2e
    rules = [abnf|
      list0  = #e
      list1  = 1#e
      list12 = 1#2e
      list2  = 2#e
      list02 = #2e
      e      = 1*%x61-7A
      |]
    names = ["list0", "list1", "list12", "list2", "list02"]
    -- the results for list0, list1, list12, list2 and list02 when a
    -- list has zero, one, two or three elements
    zero  = [True, False, False, False, True]
    one'  = [True, True, True, False, True]
    two   = [True, True, True, True, True]
    three = [True, True, False, True, False]
    none  = [False, False, False, False, False]
    checkForms input expected = conjoin
      [ counterexample (show (form, input)) $
          map (\n -> match' form rules n input) names === map Right want
      | (form, want) <- expected ]

------------------------------------------------------------------------------
-- a naive matcher to compare with
------------------------------------------------------------------------------

-- | Every end position of a match, by trying every way of matching
-- (removing duplicates as it goes, to keep the cost down). Simple
-- enough to check by reading. Grammars must
-- not be recursive. Sender lists are expanded into plain ABNF with
-- the rules of RFC 9110, Section 5.6.1.1, rather than interpreted.
naive :: ListForm -> Map.Map RuleName Alternation -> ByteString -> RuleName -> Int -> [Int]
naive form rules input = \n p -> nub (ruleEnds n p)
  where
    ruleEnds n = alt (rules Map.! n)
    alt (Alternation cs) p = nub (concatMap (\c -> conc c p) (NE.toList cs))
    conc (Concatenation rs) p = foldl (\ps r -> nub (concatMap (rep r) ps)) [p] (NE.toList rs)
    rep (Repetition Nothing e) p = el e p
    rep (Repetition (Just (Repeat Star lo hi)) e) p = star lo hi (el e) p
    rep (Repetition (Just (Repeat Hash lo hi)) e) p = case form of
      Sender    -> rep (Repetition Nothing (Group (expand lo hi e))) p
      Recipient -> recipient lo hi (el e) p
    el e p = case e of
      RuleRef n -> ruleEnds n p
      Group a   -> alt a p
      Option a  -> p : alt a p
      CharVal CaseSensitive s -> [ p + B.length s | s `B.isPrefixOf` B.drop p input ]
      CharVal CaseInsensitive s -> [ p + B.length s | C.map lower s `B.isPrefixOf` C.map lower (B.drop p input) ]
      NumVal _ (Values vs) ->
        [ p + length vs | map fromIntegral (B.unpack (B.take (length vs) (B.drop p input))) == NE.toList vs ]
      NumVal _ (Range lo hi) ->
        [ p + 1 | Just w <- [B.indexMaybe input p], fromIntegral w >= lo, fromIntegral w <= hi ]
      ProseVal _ -> error "naive: prose"
    lower c = if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c
    -- k counts matches so far; a match that consumes nothing is only
    -- worth taking to reach the minimum
    star lo hi f = go 0
      where
        go k p = nub $ [ p | k >= lo ] ++
          if Just k == hi then []
          else [ r | q <- nub (f p), q /= p || k < lo, r <- go (k + 1) q ]
    recipient lo hi f p =
      [ q | (q, n) <- concatMap more ((p, 0) : [ (q, 1) | q <- f p ])
          , n >= lo, maybe True (n <=) hi ]
      where
        more (q, n) = (q, n) : concat
          [ more (r, n) ++ concatMap (\s -> more (s, n + 1)) (f r) | r <- separator q ]
    separator q =
      [ c + 1 + j
      | i <- [0 .. B.length (B.takeWhile ows (B.drop q input))]
      , let c = q + i, B.indexMaybe input c == Just 0x2C
      , j <- [0 .. B.length (B.takeWhile ows (B.drop (c + 1) input))] ]
    ows w = w == 0x20 || w == 0x09

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.1
-- # and for n >= 1 and m > 1:
-- #
-- # <n>#<m>element => element <n-1>*<m-1>( OWS "," OWS element )

-- | RFC 9110's sender expansion of @<lo>#<hi>element@, as ABNF.
expand :: Natural -> Maybe Natural -> Element -> Alternation
expand 0 hi e = one (Option (expand 1 hi e))
expand lo hi e = Alternation (Concatenation
  (Repetition Nothing e :|
    [ Repetition (Just (Repeat Star (lo - 1) (subtract 1 <$> hi))) $ Group $ Alternation $ Concatenation
        (ows :| [ Repetition Nothing (CharVal CaseInsensitive ","), ows, Repetition Nothing e ]) :| [] ])
  :| [])
  where
    ows = Repetition (Just (Repeat Star 0 Nothing)) $ Group $ Alternation $
      Concatenation (Repetition Nothing (NumVal Hexadecimal (Values (0x20 :| []))) :| [])
        :| [Concatenation (Repetition Nothing (NumVal Hexadecimal (Values (0x09 :| []))) :| [])]

one :: Element -> Alternation
one e = Alternation (Concatenation (Repetition Nothing e :| []) :| [])

-- | Random grammars whose rules only refer to rules defined before
-- them, over a small alphabet.
genGrammar :: Gen [Rule]
genGrammar = do
  n <- choose (1, 4)
  mapM (\i -> Rule (ruleN i) Basic <$> genAlt i (3 :: Int)) [0 .. n - 1]
  where
    ruleN i = name (C.pack ("r" ++ show (i :: Int)))
    genAlt i d = Alternation <$> nonEmpty (Concatenation <$> nonEmpty (genRep i d))
    nonEmpty g = do { k <- choose (0, 2); (:|) <$> g <*> vectorOf k g }
    genRep i d = do
      e <- genEl i d
      frequency
        [ (4, pure (Repetition Nothing e))
        , (2, do lo <- elements [0, 1, 2]; hi <- elements [Nothing, Just 0, Just 1, Just 2, Just 3]
                 pure (Repetition (Just (Repeat Star lo hi)) e))
        , (2, do lo <- elements [0, 1, 2]; hi <- elements [Nothing, Just 2, Just 3]
                 pure (Repetition (Just (Repeat Hash lo (max lo <$> hi))) e)) ]
    genEl i d = frequency $
      [ (3, CharVal <$> elements [CaseSensitive, CaseInsensitive] <*> elements ["a", "b", "ab", "A", ""])
      , (2, NumVal Hexadecimal <$> elements [Values (0x61 :| []), Values (0x61 :| [0x62]), Range 0x61 0x62, Range 0x20 0x2C])
      ] ++
      [ (2, RuleRef . ruleN <$> choose (0, i - 1)) | i > 0 ] ++
      [ (1, Group <$> genAlt i (d - 1)) | d > 0 ] ++
      [ (1, Option <$> genAlt i (d - 1)) | d > 0 ]

-- | every string of up to four characters over the alphabet the
-- random grammars use
shortInputs :: [ByteString]
shortInputs = concatMap strings [0 .. 4]
  where strings k = map C.pack (sequence (replicate k "aAb, "))

genInput :: Gen ByteString
genInput = C.pack <$> (choose (5, 9) >>= \k -> vectorOf k (elements "aAb, \t"))

-- | The recognizer and the naive matcher find the same ends for every
-- short input and some longer ones.
prop_naive :: Property
prop_naive = withMaxSuccess 300 $
  forAll genGrammar $ \rules ->
  forAll (elements [Sender, Recipient]) $ \form ->
  forAll (vectorOf 20 genInput) $ \longer ->
    let top = ruleName (last rules)
        lib = library [("T", grammar rules)]
        expected input = sort (naive form (grammarRules (grammar rules)) input top 0)
        inputs = shortInputs ++ longer
        full = [ i | i <- inputs, B.length i `elem` expected i ]
        usesList = any (\r -> "Hash" `B.isInfixOf` C.pack (show r)) rules
    in checkCoverage $
       cover 50 (length full > 1) "more than one input matches" $
       cover 20 (usesList && any (B.elem 0x2C) full) "a list with a comma matches" $
       counterexample (C.unpack (B.concat [ C.pack (show r) <> "\n" | r <- rules ])) $
       counterexample (show form) $
         conjoin [ counterexample (show input) $
                     fmap Set.toList (matchEnds form lib "T" top input) === Right (expected input)
                 | input <- inputs ]

------------------------------------------------------------------------------
-- errors
------------------------------------------------------------------------------

prop_errors :: Property
prop_errors = once $ conjoin
  [ matches Recipient lib "missing" (name "a") "" === Left (UnknownGrammar "missing")
  , m "undefined" === Left (UndefinedRule (Tagged "T") (name "nowhere"))
  , m "left" === Left (LeftRecursion (Tagged "T") (name "left"))
  , m "prose" === Left (ProseValue (Tagged "T") (name "prose") "some prose")
  , m "imported" === Left (UnresolvedImport (Tagged "T") (name "imported") (Import (name "x") "ELSEWHERE" "1"))
    -- zero repetitions never look at the element
  , m "zero" === Right True
    -- core rules are available, and local definitions come first
  , matches Recipient lib "T" (name "core") "\r\n" === Right True
  , matches Recipient lib "T" (name "DIGIT") "x" === Right True
    -- right recursion is fine
  , matches Recipient lib "T" (name "right") "xxx" === Right True
  ]
  where
    lib = library [("T", grammar [abnf|
      undefined = nowhere
      left      = left "x" / "x"
      prose     = <some prose>
      imported  = <x, see [ELSEWHERE], Section 1>
      zero      = 0<anything at all>
      core      = CRLF
      DIGIT     = "x"
      right     = "x" right / "x"
      |])]
    m n = matches Recipient lib "T" (name n) ""

------------------------------------------------------------------------------
-- the RFCs' own grammars
------------------------------------------------------------------------------

check :: Library -> ByteString -> ByteString -> [(ByteString, Bool)] -> Property
check lib tag r cases = conjoin
  [ counterexample (C.unpack r ++ " " ++ show input) $
      matches Recipient lib tag (name r) input === Right ok
  | (input, ok) <- cases ]

prop_uri :: Library -> Property
prop_uri lib = once $ conjoin
  [ check lib "URI" "URI-reference"
      [ ("http://example.com/a/b?q=1#f", True), ("//host", True), ("a:b", True)
      , ("", True), ("?q", True), ("#", True), ("http://[::1]:80/", True)
      , ("http://a b", False), ("http://[::1/", False), ("%zz", False) ]
  , check lib "URI" "IPv6address"
      [ ("::", True), ("::1", True), ("1::", True), ("1:2:3:4:5:6:7:8", True)
      , ("1:2:3:4:5:6:1.2.3.4", True), ("::ffff:1.2.3.4", True), ("1::2:3", True)
      , ("1:2:3:4:5:6:7", False), ("1:2:3:4:5:6:7:8:9", False), ("1::2::3", False)
      , ("12345::", False), (":1", False), ("::1.2.3.256", False), ("", False) ]
  , check lib "URI" "path-empty" [("", True), ("a", False)]
  , conjoin [ counterexample (show n) $
                matches Recipient lib "URI" (name "dec-octet") (C.pack (show n)) === Right (n <= 255)
            | n <- [0 .. 999 :: Int] ]
  , check lib "URI" "dec-octet" [("01", False), ("00", False), ("", False)]
  ]

prop_http :: Library -> Property
prop_http lib = once $ conjoin
  [ check lib "HTTP" "token" [("GET", True), ("a!#$%&'*+-.^_`|~9", True), ("", False), ("a b", False), ("a/b", False)]
  , check lib "HTTP" "Host" [("example.com", True), ("example.com:8080", True), ("[::1]:80", True), ("", True), ("a@b", False)]
    -- the appendix expands lists in the sender form, so empty elements
    -- are not allowed
  , check lib "HTTP" "Accept" [("text/html, application/json;q=0.9", True), ("", True), (",,", False), ("text", False)]
  , check lib "HTTP" "Content-Length" [("0", True), ("123", True), ("", False), ("1 2", False)]
  , check lib "HTTP" "quoted-string" [("\"a\\\"b\"", True), ("\"a\"b\"", False), ("\"\x80\"", True)]
  , check lib "HTTP" "IMF-fixdate" [("Sun, 06 Nov 1994 08:49:37 GMT", True), ("Sun, 06 Nov 1994 08:49:37 gmt", False)]
  , matches Recipient lib "HTTP" (name "From") "a@b" === Left
      (UnresolvedImport (Tagged "HTTP") (name "mailbox") (Import (name "mailbox") "RFC5322" "3.4"))
  ]

prop_http1 :: Library -> Property
prop_http1 lib = once $ check lib "HTTP/1.1" "HTTP-message"
  [ ("GET / HTTP/1.1\r\nHost: example.com\r\n\r\n", True)
  , ("POST /a?b HTTP/1.1\r\nContent-Length: 3\r\n\r\nabc", True)
  , ("HTTP/1.1 200 OK\r\nServer: x\r\n\r\n", True)
  , ("HTTP/1.1 200 \r\n\r\n", True)
  , ("GET / HTTP/1.1\nHost: example.com\n\n", False)
  , ("GET  / HTTP/1.1\r\n\r\n", False)
  , ("GET / HTTP/1.1\r\nHost : example.com\r\n\r\n", False)
  , ("GET / http/1.1\r\n\r\n", False)
  ]

prop_long :: Library -> Property
prop_long lib = once $ conjoin
  [ check lib "HTTP" "token" [(C.replicate 20000 'a', True)]
  , check lib "HTTP" "Accept" [(B.intercalate ", " (replicate 500 "text/html;q=0.5"), True)]
  , check lib "URI" "URI-reference" [("http://example.com/" <> C.replicate 5000 'a', True)]
  ]
