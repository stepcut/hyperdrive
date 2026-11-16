{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module QQTests (qqTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Maybe (fromJust)
import Hyperdrive.ABNF.Core
import Hyperdrive.ABNF.Parser
import Hyperdrive.ABNF.QQ
import Hyperdrive.ABNF.RFC
import Hyperdrive.ABNF.Types
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

qqTests :: IO TestTree
qqTests = do
  rfc5234 <- unpaginate <$> B.readFile "test/data/rfc5234.txt"
  pure $ testGroup "Core rules and quasiquoter"
    [ testProperty "coreRules is RFC 5234 Appendix B.1" (prop_coreRules rfc5234)
    , testProperty "the quasiquoter parses ABNF"       prop_quasiquoter
    , testProperty "indentation is removed"            prop_indentation
    , testProperty "errors are positioned in the quote" prop_quoteErrors
    ]

name :: ByteString -> RuleName
name = fromJust . mkRuleName

-- = https://www.rfc-editor.org/rfc/rfc5234#appendix-B.1
-- # Certain basic rules are in uppercase, such as SP, HTAB, CRLF, DIGIT,
-- # ALPHA, etc.
prop_coreRules :: [ByteString] -> Property
prop_coreRules rfc = once $
  let block = toCRLF (dedent (dropWhile (not . ("ALPHA " `B.isPrefixOf`) . C.dropWhile (== ' '))
                                        (section "B.1.  " rfc)))
  in conjoin
       [ parseRuleList block === Right coreRules
       , map ruleName coreRules === map name
           [ "ALPHA", "BIT", "CHAR", "CR", "CRLF", "CTL", "DIGIT", "DQUOTE"
           , "HEXDIG", "HTAB", "LF", "LWSP", "OCTET", "SP", "VCHAR", "WSP" ]
       ]

prop_quasiquoter :: Property
prop_quasiquoter = once $ conjoin
  [ [abnf|x = a|] === parsed "x = a\r\n"
  , [abnf|
      example-list      = 1#example-list-elmt
      example-list-elmt = token ; see Section 5.6.2
      |] === parsed "example-list = 1#example-list-elmt\r\nexample-list-elmt = token\r\n"
  , [abnf|
      ruleset     =  alt1 / alt2

      ruleset     =/ alt3
      |] === parsed "ruleset = alt1 / alt2\r\nruleset =/ alt3\r\n"
  , [abnf||] === []
  ]
  where parsed = either (error . show) id . parseRuleList

prop_indentation :: Property
prop_indentation = once $ conjoin
  [ [abnf|
        x = a
          / b
        y = c
    |] === [abnf|
x = a
  / b
y = c
|]
  , parseQuoted "\n   x = a\n     / b\n\n   y = c\n" === parseQuoted "x = a\n  / b\ny = c"
    -- a rule indented more than the others is a continuation, so its
    -- "=" is an error
  , either (\e -> (errorLine e, errorColumn e)) (const (0, 0))
      (parseQuoted "\n   x = a\n     y = b\n") === (3, 8)
  , parseQuoted "x = a\r\ny = b\r\n" === parseQuoted "x = a\ny = b\n"
    -- a comment may be indented further than the rules
  , parseQuoted "\n         ; comment\n   x = a\n" === parseQuoted "x = a"
  ]

prop_quoteErrors :: Property
prop_quoteErrors = once $ conjoin
  [ parseQuoted "\n    x = a\n    y = %q\n" === Left (ParseError 3 10 ["\"i\"", "\"s\"", "\"b\"", "\"d\"", "\"x\""])
  , parseQuoted "x = \"caf\233\"" === Left (ParseError 1 9 ["ASCII"])
  , parseQuoted "x = a\ny = <\8212>" === Left (ParseError 2 6 ["ASCII"])
  ]
