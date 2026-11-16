{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
-- | The collected ABNF of the RFCs hyperdrive implements, parsed from
-- the published text.
module RFCTests (rfcTests) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Hyperdrive.ABNF.Core
import Hyperdrive.ABNF.Grammar
import Hyperdrive.ABNF.Parser
import Hyperdrive.ABNF.Printer
import Hyperdrive.ABNF.QQ
import Hyperdrive.ABNF.RFC
import Hyperdrive.ABNF.Types
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

data Spec = Spec
  { specName    :: String
  , specLines   :: [ByteString]
  , specRules   :: RuleList
  , specGrammar :: Grammar
  }

loadSpec :: String -> FilePath -> ByteString -> IO Spec
loadSpec n file heading = do
  ls <- unpaginate <$> B.readFile file
  rules <- either (fail . ((n ++ ": ") ++) . show) pure (parseRuleList (sectionABNF heading ls))
  grammar <- either (fail . ((n ++ ": ") ++) . show) pure (mkGrammar rules)
  pure (Spec n ls rules grammar)

rfcTests :: IO TestTree
rfcTests = do
  http  <- loadSpec "RFC 9110" "test/data/rfc9110.txt" "Appendix A.  Collected ABNF"
  http1 <- loadSpec "RFC 9112" "test/data/rfc9112.txt" "Appendix A.  Collected ABNF"
  uri   <- loadSpec "RFC 3986" "test/data/rfc3986.txt" "Appendix A.  Collected ABNF for URI"
  let specs = [http, http1, uri]
      -- the citation tags the RFCs use for each other
      tags = [("HTTP", http), ("URI", uri)]
  pure $ testGroup "Collected ABNF"
    [ testProperty "each appendix reprints" $ once $ conjoin
        [ counterexample (specName s) (parseRuleList (printRuleList (specRules s)) === Right (specRules s))
        | s <- specs ]
    , testProperty "rules defined as in the RFCs" (prop_definitions http http1 uri)
    , testProperty "only declared core rules are left undefined" (prop_coreRulesUsed http http1 uri)
    , testProperty "imports name rules defined where they say" (prop_imports tags [http, http1])
    , testProperty "Grammar merges =/ and rejects redefinition" prop_mkGrammar
    , testProperty "importOf reads only the import form" prop_importOf
    , testProperty "sectionABNF drops the prose before the rules" prop_sectionABNF
    ]

name :: ByteString -> RuleName
name = fromJust . mkRuleName

-- | Every rule of the expected list is defined in the grammar with the
-- same alternatives.
definesAll :: Spec -> RuleList -> Property
definesAll s expected = counterexample (specName s) $ conjoin
  [ counterexample (C.unpack (ruleNameBytes n)) $
      lookupRule n (specGrammar s) === Just alt
  | Rule n _ alt <- expected ]

-- A sample of rules from each appendix, written out by hand (with the
-- list rules expanded, as the appendices have them). The rule lists
-- themselves are checked in full by the round trip.
prop_definitions :: Spec -> Spec -> Spec -> Property
prop_definitions http http1 uri = once $ conjoin
  [ definesAll http [abnf|
      GMT = %x47.4D.54
      Host = uri-host [ ":" port ]
      If-Match = "*" / [ entity-tag *( OWS "," OWS entity-tag ) ]
      OWS = *( SP / HTAB )
      qdtext = HTAB / SP / "!" / %x23-5B / %x5D-7E / obs-text
      tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
       "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
      token = 1*tchar
      token68 = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+" / "/" ) *"="
      uri-host = <host, see [URI], Section 3.2.2>
      year = 4DIGIT
      |]
  , definesAll http1 [abnf|
      HTTP-message = start-line CRLF *( field-line CRLF ) CRLF [ message-body ]
      HTTP-name = %x48.54.54.50
      chunk-ext = *( BWS ";" BWS chunk-ext-name [ BWS "=" BWS chunk-ext-val ] )
      last-chunk = 1*"0" [ chunk-ext ] CRLF
      request-target = origin-form / absolute-form / authority-form / asterisk-form
      status-code = 3DIGIT
      |]
  , definesAll uri [abnf|
      IPv6address   =                            6( h16 ":" ) ls32
                    /                       "::" 5( h16 ":" ) ls32
                    / [               h16 ] "::" 4( h16 ":" ) ls32
                    / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
                    / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
                    / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
                    / [ *4( h16 ":" ) h16 ] "::"              ls32
                    / [ *5( h16 ":" ) h16 ] "::"              h16
                    / [ *6( h16 ":" ) h16 ] "::"
      h16           = 1*4HEXDIG
      dec-octet     = DIGIT / %x31-39 DIGIT / "1" 2DIGIT / "2" %x30-34 DIGIT / "25" %x30-35
      path-empty    = 0<pchar>
      sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                    / "*" / "+" / "," / ";" / "="
      |]
  , Map.size (grammarRules (specGrammar http)) === 142
  , Map.size (grammarRules (specGrammar http1)) === 42
  , Map.size (grammarRules (specGrammar uri)) === 36
  ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-2.1
-- # The following core rules are included by reference, as defined in
-- # Appendix B.1 of [RFC5234]: ALPHA (letters), CR (carriage return),
-- # CRLF (CR LF), CTL (controls), DIGIT (decimal 0-9), DQUOTE (double
-- # quote), HEXDIG (hexadecimal 0-9/A-F/a-f), HTAB (horizontal tab), LF
-- # (line feed), OCTET (any 8-bit sequence of data), SP (space), and
-- # VCHAR (any visible US-ASCII character).

-- = https://www.rfc-editor.org/rfc/rfc9112#section-1.2
-- # The following core rules are included by reference, as defined in
-- # [RFC5234], Appendix B.1: ALPHA (letters), CR (carriage return), CRLF
-- # (CR LF), CTL (controls), DIGIT (decimal 0-9), DQUOTE (double quote),
-- # HEXDIG (hexadecimal 0-9/A-F/a-f), HTAB (horizontal tab), LF (line
-- # feed), OCTET (any 8-bit sequence of data), SP (space), and VCHAR (any
-- # visible [USASCII] character).

-- = https://www.rfc-editor.org/rfc/rfc3986#section-1.3
-- # This specification uses the Augmented Backus-Naur Form (ABNF)
-- # notation of [RFC2234], including the following core ABNF syntax rules
-- # defined by that specification: ALPHA (letters), CR (carriage return),
-- # DIGIT (decimal digits), DQUOTE (double quote), HEXDIG (hexadecimal
-- # digits), LF (line feed), and SP (space).
prop_coreRulesUsed :: Spec -> Spec -> Spec -> Property
prop_coreRulesUsed http http1 uri = once $ conjoin
  [ check http  httpCore
  , check http1 httpCore
  , check uri   ["ALPHA", "CR", "DIGIT", "DQUOTE", "HEXDIG", "LF", "SP"]
  ]
  where
    httpCore = [ "ALPHA", "CR", "CRLF", "CTL", "DIGIT", "DQUOTE", "HEXDIG"
               , "HTAB", "LF", "OCTET", "SP", "VCHAR" ]
    core = Set.fromList (map ruleName coreRules)
    check s declared =
      let undefined' = undefinedReferences (specGrammar s)
          declared'  = Set.fromList (map name declared)
      in counterexample (specName s) $
           counterexample ("undefined: " ++ show (Set.toList undefined')) $
             property (undefined' `Set.isSubsetOf` declared' && declared' `Set.isSubsetOf` core)

-- = https://www.rfc-editor.org/rfc/rfc9112#section-1.2
-- # The rules below are defined in [HTTP]:

-- = https://www.rfc-editor.org/rfc/rfc9112#section-1.2
-- # The rules below are defined in [URI]:

-- | Every import names a rule that the cited RFC defines (not imports
-- in turn) in the cited section, except imports from RFCs hyperdrive
-- does not implement, and except where a verified erratum corrects
-- the section.
prop_imports :: [(ByteString, Spec)] -> [Spec] -> Property
prop_imports tags importers = once $ conjoin $
  [ counterexample (specName s ++ ": " ++ show local ++ " = " ++ show i) $
      case lookup (importFrom i) tags of
        Nothing -> property (importFrom i `elem` external)
        Just target -> conjoin
          [ counterexample "not defined" $
              fmap importOf (lookupRule (importName i) (specGrammar target)) === Just Nothing
          , counterexample "not in that section" $
              definedIn target (importName i) (corrected (specName s) local (importSection i))
          ]
  | s <- importers
  , (local, i) <- Map.toList (imports (specGrammar s)) ] ++
  -- each erratum is still needed: the RFC's own section is wrong
  [ counterexample (spec ++ ": erratum " ++ show local) $
      case [ (i, t) | s <- importers, specName s == spec
                    , Just i <- [Map.lookup local (imports (specGrammar s))]
                    , Just t <- [lookup (importFrom i) tags] ] of
        [(i, t)] -> importSection i === original
                      .&&. counterexample "the cited section defines it after all"
                             (not (definesIn t (importName i) original))
        found    -> counterexample (show (map fst found)) False
  | (spec, local, original, _) <- errata ] ++
  [ counterexample (specName s) (Map.size (imports (specGrammar s)) === n)
  | (s, n) <- zip importers [12, 15] ]
  where
    external = ["RFC4647", "RFC5646", "RFC5322"]
    corrected spec local number =
      case [ fixed | (spec', local', _, fixed) <- errata, spec' == spec, local' == local ] of
        fixed : _ -> fixed
        []        -> number
    definedIn target n number =
      counterexample (C.unpack number) (property (definesIn target n number))
    definesIn target n number =
      any (definesName n) (section (number <> ".  ") (specLines target))
    definesName n l =
      let (w, rest) = C.break (== ' ') (C.dropWhile (== ' ') l)
      in mkRuleName w == Just n && "=" `B.isPrefixOf` C.dropWhile (== ' ') rest

-- = https://www.rfc-editor.org/rfc/rfc9112#section-1.2
-- # obs-text      = <obs-text, see [HTTP], Section 5.6.4>

-- | Verified errata that correct the section an import cites: the RFC,
-- the local rule name, the section the RFC cites and the corrected one.
--
-- Erratum 7744 (RFC 9112): obs-text is used in Section 5.6.4 of RFC
-- 9110 but defined in Section 5.5, in both Section 1.2 and Appendix A.
-- The tests found this before it was looked up.
errata :: [(String, RuleName, ByteString, ByteString)]
errata = [("RFC 9112", name "obs-text", "5.6.4", "5.5")]

prop_mkGrammar :: Property
prop_mkGrammar = once $ conjoin
  [ mkGrammar [abnf|
      ruleset     =  alt1 / alt2
      other       =  x
      ruleset     =/ alt3
      ruleset     =/ alt4 / alt5
      |] === mkGrammar [abnf|
      ruleset     =  alt1 / alt2 / alt3 / alt4 / alt5
      other       =  x
      |]
  , mkGrammar [abnf|
      a = x
      A = y
      |] === Left (Redefined (name "A"))
  , mkGrammar [abnf|
      a =/ x
      a = y
      |] === Left (IncrementalBeforeDefinition (name "a"))
  , fmap undefinedReferences (mkGrammar [abnf|
      a = b [ c / (d e) ] 2*f <g> "h" %x69
      b = a
      |]) === Right (Set.fromList (map name ["c", "d", "e", "f"]))
  , fmap (Map.keys . imports) (mkGrammar [abnf|
      a = <a, see [X], Section 1>
      b = <b, see [X], Section 1> c
      c = <just prose>
      |]) === Right [name "a"]
  ]

prop_importOf :: Property
prop_importOf = once $ conjoin
  [ imp "<host, see [URI], Section 3.2.2>" === Just (Import (name "host") "URI" "3.2.2")
  , imp "<Language-Tag, see [RFC5646], Section 2.1>" === Just (Import (name "Language-Tag") "RFC5646" "2.1")
  , imp "<host, see [URI]>" === Nothing
  , imp "<host see [URI], Section 3.2.2>" === Nothing
  , imp "<, see [URI], Section 3.2.2>" === Nothing
  , imp "<host, see [], Section 3.2.2>" === Nothing
  , imp "<host, see [URI], Section >" === Nothing
  , imp "<1host, see [URI], Section 3.2.2>" === Nothing
  , imp "*<host, see [URI], Section 3.2.2>" === Nothing
  ]
  where imp src = either (const Nothing) importOf (parseAlternation src)

prop_sectionABNF :: Property
prop_sectionABNF = once $
  sectionABNF "A.  " (unpaginate (C.unlines
    [ "\xEF\xBB\xBF\&A.  Collected"
    , "   In the collected ABNF below, list rules are expanded per"
    , "   Section 5.6.1."
    , ""
    , "   a = b"
    , "   c = d /"
    , "    e"
    , ""
    , "B.  Next"
    , "   f = g" ]))
    === "a = b\r\nc = d /\r\n e\r\n"
