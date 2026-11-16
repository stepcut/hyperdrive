{-# LANGUAGE OverloadedStrings #-}
module ParserTests (parserTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import Generators
import Hyperdrive.ABNF.Parser
import Hyperdrive.ABNF.Printer
import Hyperdrive.ABNF.RFC
import Hyperdrive.ABNF.Types
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

parserTests :: IO TestTree
parserTests = do
  rfc5234 <- unpaginate <$> B.readFile "test/data/rfc5234.txt"
  rfc7405 <- unpaginate <$> B.readFile "test/data/rfc7405.txt"
  pure $ testGroup "Parser"
    [ testProperty "parse . print is the identity"     prop_roundTrip
    , testProperty "printRule matches printRuleList"   prop_printRule
    , testProperty "RFC 5234 section 4 (ABNF of ABNF)" (prop_abnfOfAbnf rfc5234)
    , testProperty "RFC 7405 section 2.2 (char-val)"   (prop_charVal rfc7405)
    , testProperty "terminal values"                   prop_terminalValues
    , testProperty "strings and case"                  prop_strings
    , testProperty "operators and precedence"          prop_operators
    , testProperty "repetition"                        prop_repetition
    , testProperty "#rule lists"                       prop_hashRule
    , testProperty "comments and continuation lines"   prop_comments
    , testProperty "invalid ABNF is rejected"          prop_invalid
    , testProperty "errors give line and column"       prop_errorPosition
    , testProperty "bytes allowed in strings, prose and comments" prop_bytes
    , testProperty "printed form"                      prop_printed
    , testProperty "page breaks are removed"           prop_unpaginate
    ]

------------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------------

name :: ByteString -> RuleName
name = fromJust . mkRuleName

ref :: ByteString -> Element
ref = RuleRef . name

-- | an alternation of concatenations of unrepeated elements
alts :: [[Element]] -> Alternation
alts = alt . map (map (Repetition Nothing))

alt :: [[Repetition]] -> Alternation
alt cs = Alternation (fromList (map (Concatenation . fromList) cs))
  where fromList (x : xs) = x :| xs
        fromList []       = error "alt: empty"

-- | a single element
one :: Element -> Alternation
one e = alts [[e]]

rep :: RepeatKind -> Integer -> Maybe Integer -> Element -> Repetition
rep k lo hi = Repetition (Just (Repeat k (fromInteger lo) (fromInteger <$> hi)))

hex :: [Integer] -> Element
hex (v : vs) = NumVal Hexadecimal (Values (fromInteger v :| map fromInteger vs))
hex []       = error "hex: empty"

-- | parse one rule, written with LF line endings for readability
parses :: ByteString -> [Rule] -> Property
parses src rules =
  counterexample (C.unpack src) (parseRuleList (crlfs src) === Right rules)

rejects :: ByteString -> Property
rejects src = counterexample (C.unpack src) (property (isLeft (parseRuleList (crlfs src))))

crlfs :: ByteString -> ByteString
crlfs = toCRLF . C.lines

basic :: ByteString -> Alternation -> Rule
basic n = Rule (name n) Basic

------------------------------------------------------------------------------
-- round trip
------------------------------------------------------------------------------

prop_roundTrip :: Property
prop_roundTrip = forAll genRuleList $ \rs ->
  counterexample (C.unpack (printRuleList rs)) $
    parseRuleList (printRuleList rs) === Right rs

prop_printRule :: Property
prop_printRule = forAll genRuleList $ \rs ->
  B.concat (map printRule rs) === printRuleList rs
    .&&. conjoin [ parseAlternation (printAlternation a) === Right a
                 | Rule _ _ a <- rs ]

------------------------------------------------------------------------------
-- the grammars of ABNF itself
------------------------------------------------------------------------------

-- | the ABNF block of a section: from the line that starts with the
-- first rule name to the end of the section
abnfBlock :: ByteString -> ByteString -> [ByteString] -> ByteString
abnfBlock heading firstRule =
  toCRLF . dedent . dropWhile (not . (firstRule `B.isPrefixOf`) . C.dropWhile (== ' '))
    . section heading

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # rulelist       =  1*( rule / (*c-wsp c-nl) )
prop_abnfOfAbnf :: [ByteString] -> Property
prop_abnfOfAbnf rfc = once $
  case parseRuleList (abnfBlock "4.  " "rulelist" rfc) of
    Left err -> counterexample (show err) False
    Right rs -> conjoin
      [ map ruleName rs === map name
          [ "rulelist", "rule", "rulename", "defined-as", "elements"
          , "c-wsp", "c-nl", "comment", "alternation", "concatenation"
          , "repetition", "repeat", "element", "group", "option"
          , "char-val", "num-val", "bin-val", "dec-val", "hex-val"
          , "prose-val" ]
      , all ((== Basic) . ruleDefinedAs) rs === True
      , lookup (name "repeat") [ (ruleName r, ruleElements r) | r <- rs ] === Just
          (alt [ [ rep Star 1 Nothing (ref "DIGIT") ]
               , [ Repetition Nothing $ Group $ alt
                     [ [ rep Star 0 Nothing (ref "DIGIT")
                       , Repetition Nothing (CharVal CaseInsensitive "*")
                       , rep Star 0 Nothing (ref "DIGIT") ] ] ] ])
      , lookup (name "hex-val") [ (ruleName r, ruleElements r) | r <- rs ] === Just
          (alt [ [ Repetition Nothing (CharVal CaseInsensitive "x")
                 , rep Star 1 Nothing (ref "HEXDIG")
                 , Repetition Nothing $ Option $ alt
                     [ [ rep Star 1 Nothing $ Group $ alt
                           [ [ Repetition Nothing (CharVal CaseInsensitive ".")
                             , rep Star 1 Nothing (ref "HEXDIG") ] ] ]
                     , [ Repetition Nothing $ Group $ alt
                           [ [ Repetition Nothing (CharVal CaseInsensitive "-")
                             , rep Star 1 Nothing (ref "HEXDIG") ] ] ] ] ] ])
      , lookup (name "prose-val") [ (ruleName r, ruleElements r) | r <- rs ] === Just
          (alt [ [ Repetition Nothing (CharVal CaseInsensitive "<")
                 , rep Star 0 Nothing $ Group $ alts
                     [ [NumVal Hexadecimal (Range 0x20 0x3D)]
                     , [NumVal Hexadecimal (Range 0x3F 0x7E)] ]
                 , Repetition Nothing (CharVal CaseInsensitive ">") ] ])
      , parseRuleList (printRuleList rs) === Right rs
      ]

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # char-val       =  case-insensitive-string /
-- # case-sensitive-string
prop_charVal :: [ByteString] -> Property
prop_charVal rfc = once $
  case parseRuleList (abnfBlock "2.2.  " "char-val" rfc) of
    Left err -> counterexample (show err) False
    Right rs -> conjoin
      [ map ruleName rs === map name
          ["char-val", "case-insensitive-string", "case-sensitive-string", "quoted-string"]
      , map ruleElements (take 3 rs) ===
          [ alts [[ref "case-insensitive-string"], [ref "case-sensitive-string"]]
          , alts [[Option (one (CharVal CaseInsensitive "%i")), ref "quoted-string"]]
          , alts [[CharVal CaseInsensitive "%s", ref "quoted-string"]] ]
      ]

------------------------------------------------------------------------------
-- RFC 5234, Sections 2 and 3
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.3
-- # CR          =  %d13

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.3
-- # CR          =  %x0D

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.3
-- # CRLF        =  %d13.10

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.4
-- # DIGIT       =  %x30-39

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.4
-- # char-line = %x0D.0A %x20-7E %x0D.0A
prop_terminalValues :: Property
prop_terminalValues = once $ conjoin
  [ parses "CR          =  %d13" [basic "CR" (one (NumVal Decimal (Values (13 :| []))))]
  , parses "CR          =  %x0D" [basic "CR" (one (hex [13]))]
  , parses "CRLF        =  %d13.10" [basic "CRLF" (one (NumVal Decimal (Values (13 :| [10]))))]
  , parses "DIGIT       =  %x30-39" [basic "DIGIT" (one (NumVal Hexadecimal (Range 0x30 0x39)))]
  , parses "char-line = %x0D.0A %x20-7E %x0D.0A"
      [basic "char-line" (alts [[hex [13, 10], NumVal Hexadecimal (Range 0x20 0x7E), hex [13, 10]]])]
  , parses "b = %b1010" [basic "b" (one (NumVal Binary (Values (10 :| []))))]
    -- base letters and hexadecimal digits are case-insensitive
  , parses "x = %X0d.Ff" [basic "x" (one (hex [13, 255]))]
  , parses "x = %B1 %D1" [basic "x" (alts [[NumVal Binary (Values (1 :| [])), NumVal Decimal (Values (1 :| []))]])]
    -- characters are integers, not bytes
  , parses "x = %x10FFFF" [basic "x" (one (hex [0x10FFFF]))]
  , parses "x = %d99999999999999999999" [basic "x" (one (NumVal Decimal (Values (99999999999999999999 :| []))))]
  ]

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.3
-- # command     =  "command string"

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.1
-- # rulename = %i"aBc"

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.1
-- # rulename = %s"aBc"
prop_strings :: Property
prop_strings = once $ conjoin
  [ parses "command     =  \"command string\""
      [basic "command" (one (CharVal CaseInsensitive "command string"))]
  , parses "rulename = %i\"aBc\"" [basic "rulename" (one (CharVal CaseInsensitive "aBc"))]
  , parses "rulename = \"aBc\""   [basic "rulename" (one (CharVal CaseInsensitive "aBc"))]
  , parses "rulename = %s\"aBc\"" [basic "rulename" (one (CharVal CaseSensitive "aBc"))]
    -- the prefixes are themselves case-insensitive strings
  , parses "rulename = %S\"aBc\" %I\"x\""
      [basic "rulename" (alts [[CharVal CaseSensitive "aBc", CharVal CaseInsensitive "x"]])]
  , parses "empty = \"\"" [basic "empty" (one (CharVal CaseInsensitive ""))]
  , parses "p = <a prose description, with \"quotes\">"
      [basic "p" (one (ProseVal "a prose description, with \"quotes\""))]
  ]

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.1
-- # mumble      =  foo bar foo

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.3
-- # ruleset     =  alt1 / alt2

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.3
-- # ruleset     =/ alt3

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.3
-- # ruleset     =/ alt4 / alt5

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.5
-- # matches (elem foo blat) or (elem bar blat), and

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.5
-- # matches (elem foo) or (bar blat).

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.5
-- # (elem foo) / (bar blat)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.8
-- # [foo bar]
prop_operators :: Property
prop_operators = once $ conjoin
  [ parses "mumble      =  foo bar foo"
      [basic "mumble" (alts [[ref "foo", ref "bar", ref "foo"]])]
  , parses "ruleset     =  alt1 / alt2\n\nruleset     =/ alt3\n\nruleset     =/ alt4 / alt5"
      [ basic "ruleset" (alts [[ref "alt1"], [ref "alt2"]])
      , Rule (name "ruleset") Incremental (one (ref "alt3"))
      , Rule (name "ruleset") Incremental (alts [[ref "alt4"], [ref "alt5"]]) ]
  , parses "x = elem (foo / bar) blat"
      [basic "x" (alts [[ref "elem", Group (alts [[ref "foo"], [ref "bar"]]), ref "blat"]])]
  , parses "x = elem foo / bar blat"
      [basic "x" (alts [[ref "elem", ref "foo"], [ref "bar", ref "blat"]])]
  , parses "x = (elem foo) / (bar blat)"
      [basic "x" (alts [[Group (alts [[ref "elem", ref "foo"]])], [Group (alts [[ref "bar", ref "blat"]])]])]
  , parses "x = [foo bar]" [basic "x" (one (Option (alts [[ref "foo", ref "bar"]])))]
    -- whitespace is optional around "/" and inside brackets
  , parses "x = a/b ( c ) [ d ]"
      [basic "x" (alts [[ref "a"], [ref "b", Group (one (ref "c")), Option (one (ref "d"))]])]
  , parses "x=a" [basic "x" (one (ref "a"))]
  ]

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.6
-- # Default values are 0 and infinity so that *<element> allows any
-- # number, including zero; 1*<element> requires at least one;
-- # 3*3<element> allows exactly 3; and 1*2<element> allows one or two.

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.7
-- # That is, exactly <n> occurrences of <element>.  Thus, 2DIGIT is a
-- # 2-digit number, and 3ALPHA is a string of three alphabetic
-- # characters.
prop_repetition :: Property
prop_repetition = once $ conjoin
  [ parses "x = *e"    [basic "x" (alt [[rep Star 0 Nothing (ref "e")]])]
  , parses "x = 1*e"   [basic "x" (alt [[rep Star 1 Nothing (ref "e")]])]
  , parses "x = 3*3e"  [basic "x" (alt [[rep Star 3 (Just 3) (ref "e")]])]
  , parses "x = 1*2e"  [basic "x" (alt [[rep Star 1 (Just 2) (ref "e")]])]
  , parses "x = *2e"   [basic "x" (alt [[rep Star 0 (Just 2) (ref "e")]])]
  , parses "x = 2DIGIT 3ALPHA"
      [basic "x" (alt [[rep Star 2 (Just 2) (ref "DIGIT"), rep Star 3 (Just 3) (ref "ALPHA")]])]
  , parses "x = 3e" (map (basic "x") [alt [[rep Star 3 (Just 3) (ref "e")]]])
  , parseRuleList "x = 3e\r\n" === parseRuleList "x = 3*3e\r\n"
  , parses "x = 2*(a / b) 1*%x41 *\"c\" 0<p>"
      [basic "x" (alt [[ rep Star 2 Nothing (Group (alts [[ref "a"], [ref "b"]]))
                       , rep Star 1 Nothing (hex [0x41])
                       , rep Star 0 Nothing (CharVal CaseInsensitive "c")
                       , rep Star 0 (Just 0) (ProseVal "p") ]])]
  ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1
-- # The full form is "<n>#<m>element"
-- # indicating at least <n> and at most <m> elements

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # example-list      = 1#example-list-elmt
-- # example-list-elmt = token ; see Section 5.6.2
prop_hashRule :: Property
prop_hashRule = once $ conjoin
  [ parses "example-list      = 1#example-list-elmt\nexample-list-elmt = token ; see Section 5.6.2"
      [ basic "example-list" (alt [[rep Hash 1 Nothing (ref "example-list-elmt")]])
      , basic "example-list-elmt" (one (ref "token")) ]
  , parses "x = #e"    [basic "x" (alt [[rep Hash 0 Nothing (ref "e")]])]
  , parses "x = 2#3e"  [basic "x" (alt [[rep Hash 2 (Just 3) (ref "e")]])]
  , parses "x = #3e"   [basic "x" (alt [[rep Hash 0 (Just 3) (ref "e")]])]
  , parses "x = 2#2e"  [basic "x" (alt [[rep Hash 2 (Just 2) (ref "e")]])]
  ]

-- = https://www.rfc-editor.org/rfc/rfc5234#section-2.2
-- # For visual ease, rule definitions are left aligned.  When a rule
-- # requires multiple lines, the continuation lines are indented.

-- = https://www.rfc-editor.org/rfc/rfc5234#section-3.9
-- # A semicolon starts a comment that continues to the end of line.
prop_comments :: Property
prop_comments = once $ conjoin
  [ parses "x = a ; comment\n" [basic "x" (one (ref "a"))]
  , parses "; a comment line\nx = a\n\n   \n; another\n" [basic "x" (one (ref "a"))]
  , parses "x = a\n    / b ; why b\n    / c\n" [basic "x" (alts [[ref "a"], [ref "b"], [ref "c"]])]
  , parses "x = a ; comment\n  b\n" [basic "x" (alts [[ref "a", ref "b"]])]
    -- errata 2968 and 3076: indented comment lines after a rule belong
    -- to no rule
  , parses "x = a\n        ; continues if next line starts\n        ;  with white space\ny = b"
      [basic "x" (one (ref "a")), basic "y" (one (ref "b"))]
  , parses "x = a  \ny = b  ; trailing\n" [basic "x" (one (ref "a")), basic "y" (one (ref "b"))]
  , parses "x = (\n  a\n  ; inside a group\n  / b\n  )"
      [basic "x" (one (Group (alts [[ref "a"], [ref "b"]])))]
  ]

prop_invalid :: Property
prop_invalid = once $ conjoin
  [ counterexample "LF line endings" $ property (isLeft (parseRuleList "x = a\n"))
  , counterexample "no final line ending" $ property (isLeft (parseRuleList "x = a"))
  , counterexample "empty input" $ property (isLeft (parseRuleList ""))
  , rejects "  x = a"                 -- a rule must start in column 0
  , rejects "x = a\nb"                -- continuation lines are indented
  , rejects "x a"
  , rejects "x ="
  , rejects "x == a"
  , rejects "1x = a"
  , rejects "x = a_b"
  , rejects "x = \"a\"\"b\""          -- concatenation needs whitespace
  , rejects "x = (a)(b)"
  , rejects "x = a[b]"
  , rejects "x = \"abc"
  , rejects "x = \"\t\""              -- strings are SP and VCHAR only
  , rejects "x = <a>b>"
  , rejects "x = <a"
  , rejects "x = %x41.42-43"          -- values and ranges cannot be mixed
  , rejects "x = %x41-42-43"
  , rejects "x = %x"
  , rejects "x = %b2"
  , rejects "x = %dA"
  , rejects "x = %q1"
  , rejects "x = %r\"a\""
  , rejects "x = (a"
  , rejects "x = [a"
  , rejects "x = ()"
  , rejects "x = a /"
  , rejects "x = / a"
  , rejects "x = 1*"
  , rejects "x = * a"
  , rejects "x = 1**a"
  , rejects "x = 1#*a"
  , rejects "x = a ; caf\xC3\xA9"     -- comments are WSP and VCHAR only
  , rejects "x = a\r"
  ]

prop_errorPosition :: Property
prop_errorPosition = once $ conjoin
  [ parseRuleList "x = a\r\ny = %q\r\n"
      === Left (ParseError 2 6 ["\"i\"", "\"s\"", "\"b\"", "\"d\"", "\"x\""])
    -- an unclosed group could continue on an indented next line
  , either (\e -> (errorLine e, errorColumn e)) (const (0, 0))
      (parseRuleList "x = a\r\ny = (b\r\n") === (3, 1)
  , either errorLine (const 0) (parseRuleList "x = a\r\ny = b\r\nz = %q\r\n") === 3
  , either errorColumn (const 0) (parseRuleList "x = a\r\ny = b\r\nz = %q\r\n") === 6
  ]

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # quoted-string  =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # comment        =  ";" *(WSP / VCHAR) CRLF
prop_bytes :: Property
prop_bytes = once $ conjoin
  [ counterexample (show w) $ conjoin
      [ ok (wrap "x = \"" "\"") === (in_ 0x20 0x21 || in_ 0x23 0x7E)
      , ok (wrap "x = <" ">")     === (in_ 0x20 0x3D || in_ 0x3F 0x7E)
      , ok (wrap "x = a ;" "")    === (w == 0x09 || in_ 0x20 0x7E)
      ]
  | w <- [minBound .. maxBound]
  , let wrap a b = a <> B.singleton w <> b <> "\r\n"
        in_ lo hi = w >= lo && w <= hi
  ]
  where ok = either (const False) (const True) . parseRuleList

prop_printed :: Property
prop_printed = once $ conjoin
  [ reprint "x = %x0D.0A / %x30-39 / %d13 / %b101 / %x10FFFF / %x7"
            "x = %x0D.0A / %x30-39 / %d13 / %b101 / %x10FFFF / %x07"
  , reprint "x =/ \"a\" %s\"B\" %i\"c\" <prose>"
            "x =/ \"a\" %s\"B\" \"c\" <prose>"
  , reprint "x = 3e 3*3e *e 1*e *2e 1*2e #e 1#e 2#2e #3e"
            "x = 3e 3e *e 1*e *2e 1*2e #e 1#e 2#2e #3e"
  , reprint "Rule-Name = ( a  b ) [c] ; comment\n  / d"
            "Rule-Name = (a b) [c] / d"
  ]
  where
    reprint src expected = counterexample (C.unpack src) $
      fmap printRuleList (parseRuleList (crlfs src)) === Right (expected <> "\r\n")

prop_unpaginate :: Property
prop_unpaginate = once $ conjoin
  [ unpaginate page === ["Header", "", "   x = a /", "    b", "   y = c", ""]
  , fmap (map ruleName) (parseRuleList (toCRLF (dedent (section "Header" ("Header" : drop 1 (unpaginate page))))))
      === Right [name "x", name "y"]
  , dedent ["", "   a", "     b", "", "   c", "  "] === ["a", "  b", "", "c"]
  , section "2.  " ["1.  One", "   a", "2.  Two", "   b", "", "     c", "3.  Three", "   d"]
      === ["   b", "", "     c"]
  , toCRLF ["a", "b"] === "a\r\nb\r\n"
  ]
  where
    page = C.unlines
      [ "Header", "", "   x = a /", "", "", "Author   Standards Track   [Page 1]\r"
      , "\f", "RFC 0000   Title   Date", "", "", "    b", "   y = c", "" ]
