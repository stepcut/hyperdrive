{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module FieldSyntaxTests (fieldSyntaxTests) where

import Control.Monad (replicateM, void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, ord, toLower)
import Data.Either (isRight)
import Data.List (nub)
import Data.Maybe (catMaybes)
import Grammar (rule)
import Hyperdrive.ABNF.Grammar (mkGrammar)
import Hyperdrive.ABNF.HTTP (rfc3986, rfc9110, rfc9110Errata, withErrata)
import Hyperdrive.ABNF.QQ (abnf)
import Hyperdrive.ABNF.Recognize (ListForm(..), library, matches)
import Hyperdrive.ABNF.Types (mkRuleName)
import Hyperdrive.Semantics.Field
import Hyperdrive.Semantics.FieldSyntax
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)
import Text.ParserCombinators.ReadP

fieldSyntaxTests :: TestTree
fieldSyntaxTests = testGroup "FieldSyntax"
  [ testProperty "the ReadP oracles accept what RFC 9110's grammar does" prop_oraclesMatchGrammar
  , testGroup "Lists"
    [ testProperty "RFC 9110 5.6.1.2 examples"                 prop_rfcListExamples
    , testProperty "RFC 9110 5.5 examples (commas in quotes)"  prop_rfcQuotedListExamples
    , testProperty "splitList agrees with the grammar (exhaustive, no comments)" $
        once (prop_splitListExhaustive NoComments)
    , testProperty "splitList agrees with the grammar (exhaustive, comments)" $
        once (prop_splitListExhaustive AllowComments)
    , testProperty "split then join is the identity"           prop_splitJoin
    , testProperty "each field line is split separately"        prop_fieldLines
    ]
  , testGroup "Entity tags"
    [ testProperty "splitEntityTags agrees with #entity-tag (exhaustive)" prop_entityTagsExhaustive
    , testProperty "examples"                                         prop_entityTagExamples
    ]
  , testGroup "Quoted strings"
    [ testProperty "parseQuotedString agrees with the grammar (exhaustive)" prop_quotedExhaustive
    , testProperty "quoteString then parseQuotedString is the identity"     prop_quoteRoundTrip
    , testProperty "quoteString only escapes DQUOTE and backslash"          prop_quoteMinimal
    ]
  , testGroup "Parameters"
    [ testProperty "RFC 9110 8.3.1 equivalent media types"      prop_rfcMediaTypes
    , testProperty "parseParameters agrees with the grammar (exhaustive)" prop_parametersExhaustive
    , testProperty "examples"                                   prop_parameterExamples
    ]
  ]

value :: ByteString -> FieldValue
value bs = either (error . ("invalid field value: " ++) . show) id (mkFieldValue bs)

split :: Comments -> ByteString -> Either FieldSyntaxError [ByteString]
split c = splitList c . value

------------------------------------------------------------------------------
-- oracles: the grammars of RFC 9110 Section 5.6, transcribed into ReadP
------------------------------------------------------------------------------

isFieldVCharC, isQDTextC, isQuotedPairC, isCTextC, isTCharC :: Char -> Bool
isFieldVCharC c = (ord c >= 0x21 && ord c <= 0x7E) || ord c >= 0x80
-- qdtext = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
isQDTextC c = c `elem` ['\t', ' ', '\x21'] || (c >= '\x23' && c <= '\x5B') || (c >= '\x5D' && c <= '\x7E') || ord c >= 0x80
-- quoted-pair = "\" ( HTAB / SP / VCHAR / obs-text )
isQuotedPairC c = c == '\t' || c == ' ' || (c >= '\x21' && c <= '\x7E') || ord c >= 0x80
-- ctext = HTAB / SP / %x21-27 / %x2A-5B / %x5D-7E / obs-text
isCTextC c = c == '\t' || c == ' ' || (c >= '\x21' && c <= '\x27') || (c >= '\x2A' && c <= '\x5B') || (c >= '\x5D' && c <= '\x7E') || ord c >= 0x80
isTCharC c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("!#$%&'*+-.^_`|~" :: String)

ows :: ReadP ()
ows = void (many (satisfy (`elem` (" \t" :: String))))

-- | quoted-string, returning its value
quotedStringO :: ReadP String
quotedStringO = between (char '"') (char '"') (concat <$> many (qdtext +++ quotedPair))
  where
    qdtext     = (: []) <$> satisfy isQDTextC
    quotedPair = char '\\' >> (: []) <$> satisfy isQuotedPairC

-- | comment = "(" *( ctext / quoted-pair / comment ) ")"
commentO :: ReadP ()
commentO = char '(' >> void (many (void (satisfy isCTextC) +++ quotedPair +++ commentO)) >> void (char ')')
  where quotedPair = char '\\' >> void (satisfy isQuotedPairC)

-- | #element => [ element ] *( OWS "," OWS [ element ] ), for an
-- element made of quoted-strings, comments (when allowed) and any other
-- visible characters except ",", with whitespace allowed inside it but
-- not at either end. Returns the raw text of each element.
listO :: Comments -> ReadP [String]
listO c = do
  first <- option [] ((: []) <$> element)
  rest  <- many (ows >> char ',' >> ows >> option [] ((: []) <$> element))
  pure (first ++ concat rest)
  where
    element = fst <$> gather (item >> many (many (satisfy (`elem` (" \t" :: String))) >> item))
    item = void quotedStringO +++ (if c == AllowComments then commentO else pfail) +++ void (satisfy other)
    other ch = isFieldVCharC ch && ch /= ',' && ch /= '"'
               && (c == NoComments || ch `notElem` ("()" :: String))

-- | parameters = *( OWS ";" OWS [ parameter ] ), with names lower case
parametersO :: ReadP [(String, String)]
parametersO = catMaybes <$> many (ows >> char ';' >> ows >> option Nothing (Just <$> parameter))
  where
    parameter = do
      name <- munch1 isTCharC
      _ <- char '='
      v <- munch1 isTCharC +++ quotedStringO
      pure (map toLower name, v)

-- | all complete parses, without duplicates
parses :: ReadP a -> String -> [a]
parses p s = [ x | (x, "") <- readP_to_S p s ]

-- | The oracles above return values, which a grammar cannot, so they
-- stay; but which strings they accept is checked here against the
-- rules of RFC 9110 as published, rather than trusted as transcriptions.
prop_oraclesMatchGrammar :: Property
prop_oraclesMatchGrammar = once $ conjoin
  [ agree "quoted-string" (void quotedStringO) "\"\\a \t\DEL\128("
  , agree "comment"       commentO             "()\\a \t\"\128"
  , agree "parameters"    (void parametersO)   "a;= \"\\A"
  ]
  where
    agree r p alphabet =
      let failures = [ s | s <- concatMap (`replicateM` alphabet) [0 .. 6]
                         , not (null (parses p s)) /= rule "HTTP" r s ]
      in counterexample (r ++ " disagrees for: " ++ show (take 10 failures)) (null failures)

------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # Then the following are valid values for example-list (not including
-- # the double quotes, which are present for delimitation only):

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # A
-- # recipient MUST parse and ignore a reasonable number of empty list
-- # elements: enough to handle common mistakes by senders that merge
-- # values, but not so much that they could be used as a denial-of-
-- # service mechanism.
prop_rfcListExamples :: Property
prop_rfcListExamples = conjoin
  [ split NoComments "foo,bar"            === Right ["foo", "bar"]
  , split NoComments "foo ,bar,"          === Right ["foo", "bar"]
  , split NoComments "foo , ,bar,charlie" === Right ["foo", "bar", "charlie"]
  -- invalid for 1#element because there is no element, which is for
  -- the caller to check: there are no elements
  , split NoComments ""                   === Right []
  , split NoComments ","                  === Right []
  , split NoComments ",   ,"              === Right []
  ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # For example, a textual date and a URI (either of which might contain
-- # a comma) could be safely carried in list-based field values like
-- # these:
prop_rfcQuotedListExamples :: Property
prop_rfcQuotedListExamples = conjoin
  [ split NoComments "\"http://example.com/a.html,foo\", \"http://without-a-comma.example.com/\""
      === Right ["\"http://example.com/a.html,foo\"", "\"http://without-a-comma.example.com/\""]
  , split NoComments "\"Sat, 04 May 1996\", \"Wed, 14 Sep 2005\""
      === Right ["\"Sat, 04 May 1996\"", "\"Wed, 14 Sep 2005\""]
  ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # In other words, a recipient MUST accept lists
-- # that satisfy the following syntax:
-- # #element => [ element ] *( OWS "," OWS [ element ] )

-- | Every field value up to length 6 over an alphabet of the
-- characters that matter: the result of splitList agrees with the
-- oracle, and the oracle parses unambiguously.
prop_splitListExhaustive :: Comments -> Property
prop_splitListExhaustive c =
  let inputs   = [ s | s <- concatMap (`replicateM` "a, \t\"\\()") [0 .. 6]
                     , isRight (mkFieldValue (C.pack s)) ]
      check s  = case (split c (C.pack s), nub (parses (listO c) (stripOWS s))) of
                   (Right es, [o]) -> map C.unpack es == o
                   (Left _,   [])  -> True
                   _               -> False
      failures = filter (not . check) inputs
      valid    = length [ () | s <- inputs, not (null (parses (listO c) (stripOWS s))) ]
  in counterexample ("failed for: " ++ show (take 10 failures)) (null failures)
     .&&. counterexample "vacuous" (valid > 1000 && valid < length inputs)
  where stripOWS = reverse . dropWhile (`elem` (" \t" :: String)) . reverse . dropWhile (`elem` (" \t" :: String))

-- | elements that may contain commas inside quotes or comments
genElement :: Gen String
genElement = concat <$> listOf1 (frequency
  [ (5, (: []) <$> elements "abc=;/")
  , (2, do inner <- listOf (elements "a ,()\\\""); pure (show' inner))
  , (1, do inner <- listOf (elements "a ,\"("); pure ("(" ++ concatMap esc inner ++ ")"))
  ])
  where
    -- a quoted-string containing the characters, escaping as needed
    show' inner = "\"" ++ concatMap (\ch -> if ch `elem` ("\"\\" :: String) then ['\\', ch] else [ch]) inner ++ "\""
    esc ch = if ch `elem` ("()\\" :: String) then ['\\', ch] else [ch]

prop_splitJoin :: Property
prop_splitJoin =
  forAll (listOf genElement) $ \es ->
  forAll (traverse (const separator) es) $ \seps ->
    let joined = concat (zipWith (++) es seps)
    in case mkFieldValue (C.pack joined) of
         Left _   -> discard
         Right fv -> fmap (map C.unpack) (splitList AllowComments fv) === Right es
  where
    -- "," with optional whitespace, and sometimes empty elements
    separator = do
      empties <- frequency [(4, pure 0), (1, choose (1, 3))]
      ws <- elements ["", " ", "\t", "  "]
      pure (concat (replicate (empties + 1) (ws ++ "," ++ ws)))

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # A recipient MAY combine multiple field lines within a field section
-- # that have the same field name into one field line
prop_fieldLines :: Property
prop_fieldLines = conjoin
  [ fieldListElements NoComments hConnection (lines' ["a, b", "c"]) === Right ["a", "b", "c"]
  , fieldListElements NoComments hConnection (lines' []) === Right []
  , fieldListElements NoComments hConnection (lines' [", ,", "a"]) === Right ["a"]
    -- combined, this would be one element "\"x, y\"": split separately,
    -- the open quote on the first line is an error rather than
    -- swallowing the second line
  , fieldListElements NoComments hConnection (lines' ["\"x", "y\""]) === Left UnterminatedQuotedString
  ]
  where lines' vs = fieldsFromList [ (hConnection, value v) | v <- vs ]

------------------------------------------------------------------------------
-- Quoted strings
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.4
-- # Recipients
-- # that process the value of a quoted-string MUST handle a quoted-pair
-- # as if it were replaced by the octet following the backslash.
prop_quotedExhaustive :: Property
prop_quotedExhaustive = once $
  let inputs = concatMap (`replicateM` "\"\\a \t\DEL\128") [0 .. 6]
      check s = case (parseQuotedString (C.pack s), [ (v, rest) | (v, rest) <- readP_to_S quotedStringO s ]) of
                  (Right (v, rest), os) -> (C.unpack v, C.unpack rest) `elem` os
                  (Left _, [])          -> True
                  _                     -> False
      failures = filter (not . check) inputs
  in counterexample ("failed for: " ++ show (take 10 failures)) (null failures)

prop_quoteRoundTrip :: Property
prop_quoteRoundTrip = forAll (B.pack <$> listOf arbitrary) $ \v ->
  case quoteString v of
    Nothing -> property (B.any (\w -> (w < 0x20 && w /= 0x09) || w == 0x7F) v)
    Just q  -> parseQuotedString q === Right (v, "")

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.4
-- # A sender SHOULD NOT generate a quoted-pair in a quoted-string except
-- # where necessary to quote DQUOTE and backslash octets occurring within
-- # that string.
prop_quoteMinimal :: Property
prop_quoteMinimal = conjoin
  [ quoteString ""         === Just "\"\""
  , quoteString "a b\tc"   === Just "\"a b\tc\""
  , quoteString "say \"hi\"" === Just "\"say \\\"hi\\\"\""
  , quoteString "a\\b"     === Just "\"a\\\\b\""
  , quoteString "caf\xe9"  === Just "\"caf\xe9\""
  , quoteString "a\nb"     === Nothing
  , quoteString "a\DELb"   === Nothing
  ]

------------------------------------------------------------------------------
-- Parameters
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.3.1
-- # For example, the following media types are equivalent in describing
-- # HTML text data encoded in the UTF-8 character encoding scheme
prop_rfcMediaTypes :: Property
prop_rfcMediaTypes = conjoin
  -- the parameters of text/html;charset=utf-8 and its equivalents. The
  -- charset value is case-insensitive by the definition of charset,
  -- which is for the caller to apply.
  [ parseParameters ";charset=utf-8"      === Right [("charset", "utf-8")]
  , parseParameters ";Charset=\"utf-8\""  === Right [("charset", "utf-8")]
  , parseParameters "; charset=\"utf-8\"" === Right [("charset", "utf-8")]
  , parseParameters ";charset=UTF-8"      === Right [("charset", "UTF-8")]
  ]

prop_parametersExhaustive :: Property
prop_parametersExhaustive = once $
  let inputs   = concatMap (`replicateM` "a;= \"\\A") [0 .. 6]
      check s  = case (parseParameters (C.pack s), nub (parses parametersO s)) of
                   (Right ps, [o]) -> [ (C.unpack k, C.unpack v) | (k, v) <- ps ] == o
                   (Left _, [])    -> True
                   _               -> False
      failures = filter (not . check) inputs
      valid    = length [ () | s <- inputs, not (null (parses parametersO s)) ]
  in counterexample ("failed for: " ++ show (take 10 failures)) (null failures)
     .&&. counterexample "vacuous" (valid > 100 && valid < length inputs)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.6
-- # *Note:* Parameters do not allow whitespace (not even "bad"
-- # whitespace) around the "=" character.
prop_parameterExamples :: Property
prop_parameterExamples = conjoin
  [ parseParameters ""                 === Right []
  , parseParameters ";;"               === Right []
  , parseParameters "; ; a=b ;"        === Right [("a", "b")]
  , parseParameters ";q=0.5;Q=1"       === Right [("q", "0.5"), ("q", "1")]
  , parseParameters ";a=\"b;c\";d=e"   === Right [("a", "b;c"), ("d", "e")]
  , parseParameters ";a=\"\""          === Right [("a", "")]
  , parseParameters ";a =b"            === Left InvalidParameter
  , parseParameters ";a= b"            === Left InvalidParameter
  , parseParameters ";a"               === Left InvalidParameter
  , parseParameters ";a="              === Left InvalidParameter
  , parseParameters ";=b"              === Left InvalidParameter
  , parseParameters "a=b"              === Left InvalidParameter
  , parseParameters ";a=b "            === Left InvalidParameter
  , parseParameters ";a=\"b"           === Left UnterminatedQuotedString
  ]

------------------------------------------------------------------------------
-- Entity tags
------------------------------------------------------------------------------

-- | RFC 9110's list of entity tags, in the form recipients accept
-- (empty elements allowed), with entity-tag taken from RFC 9110 itself.
entityTagList :: ByteString -> Bool
entityTagList = either (error . show) id . matches Recipient lib "L" (maybe (error "name") id (mkRuleName "l"))
  where
    lib = library
      [ ("L", grammar [abnf|
          l = #e
          e = <entity-tag, see [HTTP], Section 8.8.3>
          |])
      , ("HTTP", grammar (withErrata rfc9110Errata rfc9110)), ("URI", grammar rfc3986) ]
    grammar = either (error . show) id . mkGrammar

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3
-- # entity-tag = [ weak ] opaque-tag
-- # weak       = %s"W/"
-- # opaque-tag = DQUOTE *etagc DQUOTE
-- # etagc      = %x21 / %x23-7E / obs-text
-- # ; VCHAR except double quotes, plus obs-text

-- | Every value of up to six characters over an alphabet that exercises
-- the weak prefix, quotes, backslashes, commas and whitespace: the
-- splitter accepts exactly what the grammar matches, every tag it
-- returns is an entity-tag, and the tags joined again split the same.
prop_entityTagsExhaustive :: Property
prop_entityTagsExhaustive = once $
  let inputs = [ v | s <- concatMap (`replicateM` "W/\"\\a, ") [0 .. 6], Right v <- [mkFieldValue (C.pack s)] ]
      check v = case splitEntityTags v of
        Right tags -> entityTagList (fieldValueBytes v)
                      && all (rule "HTTP" "entity-tag" . C.unpack) tags
                      && (splitEntityTags (value (B.intercalate ", " tags)) == Right tags)
        Left _     -> not (entityTagList (fieldValueBytes v))
      failures = [ fieldValueBytes v | v <- inputs, not (check v) ]
      valid = length [ () | v <- inputs, entityTagList (fieldValueBytes v), not (B.null (fieldValueBytes v)) ]
  in counterexample ("failed for: " ++ show (take 10 failures)) (null failures)
     .&&. counterexample "vacuous" (valid > 100)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3
-- # |  *Note:* Previously, opaque-tag was defined to be a quoted-
-- # |  string ([RFC2616], Section 3.11); thus, some recipients might
-- # |  perform backslash unescaping.  Servers therefore ought to avoid
-- # |  backslash characters in entity tags.
prop_entityTagExamples :: Property
prop_entityTagExamples = conjoin
  [ splitEntityTags (value "\"a\\\", \"b\"") === Right ["\"a\\\"", "\"b\""]
  , splitEntityTags (value "W/\"x\", \"y\" , ,W/\"\"") === Right ["W/\"x\"", "\"y\"", "W/\"\""]
  , splitEntityTags (value "") === Right []
  , splitEntityTags (value "w/\"x\"") === Left InvalidEntityTag
  , splitEntityTags (value "*") === Left InvalidEntityTag
  , splitEntityTags (value "\"a\" \"b\"") === Left InvalidEntityTag
  , splitEntityTags (value "\"a") === Left InvalidEntityTag
  , splitEntityTags (value "\"a b\"") === Left InvalidEntityTag
  , splitEntityTags (value "\"caf\xE9\"") === Right ["\"caf\xE9\""]
  ]
