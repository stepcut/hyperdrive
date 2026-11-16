{-# LANGUAGE OverloadedStrings #-}
-- | A parser for ABNF, following the grammar of RFC 5234, Section 4,
-- with
--
--   * the verified errata 2968 and 3076, which keep a rule from
--     running on into the comment lines after it,
--   * RFC 7405's @%s@ and @%i@ strings, and
--   * RFC 9110's @#@ lists. @#@ is not valid in plain ABNF, so
--     accepting it changes the meaning of no RFC 5234 grammar.
--
-- The input must use CRLF line endings, as RFC 5234 requires; see
-- 'Hyperdrive.ABNF.RFC.toCRLF' for text that does not.
module Hyperdrive.ABNF.Parser
  ( parseRuleList
  , parseAlternation
  , ParseError(..)
  ) where

import Control.Applicative (Alternative(..), optional)
import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Hyperdrive.ABNF.Types
import Numeric.Natural (Natural)

-- | Where parsing failed, and what was expected there. Lines and
-- columns count from 1, and columns count bytes.
data ParseError = ParseError
  { errorLine     :: Int
  , errorColumn   :: Int
  , errorExpected :: [String]
  } deriving (Eq, Show)

-- | Parse a complete rule list.
parseRuleList :: ByteString -> Either ParseError RuleList
parseRuleList = runParser (rulelist <* eof)

-- | Parse the right-hand side of a rule, such as
-- @token *( OWS \",\" OWS token )@. Leading and trailing whitespace,
-- comments and line breaks are allowed.
parseAlternation :: ByteString -> Either ParseError Alternation
parseAlternation = runParser (many cwsp *> alternation <* many cwsp <* eof)

------------------------------------------------------------------------------
-- a small backtracking parser that remembers the furthest failure
------------------------------------------------------------------------------

-- | The furthest offset any alternative failed at, and what it
-- expected there.
data Failure = Failure !Int [String]

instance Semigroup Failure where
  a@(Failure i xs) <> b@(Failure j ys) = case compare i j of
    GT -> a
    LT -> b
    EQ -> Failure i (xs ++ ys)

data Result a = Ok a !Int Failure | Err Failure

newtype Parser a = Parser (ByteString -> Int -> Result a)

instance Functor Parser where
  fmap f (Parser p) = Parser $ \s i -> case p s i of
    Ok a j e -> Ok (f a) j e
    Err e    -> Err e

instance Applicative Parser where
  pure a = Parser $ \_ i -> Ok a i (Failure (-1) [])
  pf <*> pa = do { f <- pf; f <$> pa }

instance Monad Parser where
  Parser p >>= k = Parser $ \s i -> case p s i of
    Err e    -> Err e
    Ok a j e -> let Parser q = k a in case q s j of
      Ok b l e' -> Ok b l (e <> e')
      Err e'    -> Err (e <> e')

instance Alternative Parser where
  empty = Parser $ \_ i -> Err (Failure i [])
  Parser p <|> Parser q = Parser $ \s i -> case p s i of
    Ok a j e -> Ok a j e
    Err e    -> case q s i of
      Ok a j e' -> Ok a j (e <> e')
      Err e'    -> Err (e <> e')

runParser :: Parser a -> ByteString -> Either ParseError a
runParser (Parser p) s = case p s 0 of
  Ok a _ _ -> Right a
  Err (Failure i xs) ->
    let before = B.take i s
        line   = C.count '\n' before + 1
        column = i - maybe 0 (+ 1) (C.elemIndexEnd '\n' before) + 1
    in Left (ParseError line column (dedup xs))
  where
    dedup = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

(<?>) :: Parser a -> String -> Parser a
Parser p <?> what = Parser $ \s i -> case p s i of
  Err (Failure j _) | j == i -> Err (Failure i [what])
  r                          -> r
infix 0 <?>

satisfy :: (Word8 -> Bool) -> Parser Word8
satisfy f = Parser $ \s i -> case B.indexMaybe s i of
  Just w | f w -> Ok w (i + 1) (Failure (-1) [])
  _            -> Err (Failure i [])

byte :: Word8 -> Parser ()
byte w = void (satisfy (== w)) <?> show (C.singleton (toEnum (fromIntegral w)))

-- | a case-insensitive ASCII letter
letter :: Char -> Parser ()
letter c = void (satisfy (\w -> w == lower || w == lower - 0x20)) <?> show [c]
  where lower = fromIntegral (fromEnum c)

-- | a case-insensitive ASCII string
string :: String -> Parser ()
string str = mapM_ one str <?> show str
  where
    one c | c >= 'a' && c <= 'z' = letter c
          | otherwise            = byte (fromIntegral (fromEnum c))

takeWhile1 :: (Word8 -> Bool) -> Parser ByteString
takeWhile1 f = Parser $ \s i ->
  let bs = B.takeWhile f (B.drop i s)
  in if B.null bs then Err (Failure i []) else Ok bs (i + B.length bs) (Failure (-1) [])

takeWhile0 :: (Word8 -> Bool) -> Parser ByteString
takeWhile0 f = takeWhile1 f <|> pure B.empty

eof :: Parser ()
eof = Parser $ \s i ->
  if i == B.length s then Ok () i (Failure (-1) []) else Err (Failure i ["end of input"])

------------------------------------------------------------------------------
-- core rules (RFC 5234, Appendix B.1)
------------------------------------------------------------------------------

isAlpha, isDigit, isHexDig, isBit, isWSP, isVChar :: Word8 -> Bool
isAlpha w  = (w >= 0x41 && w <= 0x5A) || (w >= 0x61 && w <= 0x7A)
isDigit w  = w >= 0x30 && w <= 0x39
-- HEXDIG is written with strings, which are case-insensitive
isHexDig w = isDigit w || (w >= 0x41 && w <= 0x46) || (w >= 0x61 && w <= 0x66)
isBit w    = w == 0x30 || w == 0x31
isWSP w    = w == 0x20 || w == 0x09
isVChar w  = w >= 0x21 && w <= 0x7E

wsp :: Parser ()
wsp = void (satisfy isWSP) <?> "whitespace"

crlf :: Parser ()
crlf = (byte 0x0D *> byte 0x0A) <?> "CRLF"

------------------------------------------------------------------------------
-- RFC 5234, Section 4
------------------------------------------------------------------------------

-- erratum 3076: rulelist = 1*( rule / (*WSP c-nl) )
rulelist :: Parser RuleList
rulelist = concat <$> some ((pure <$> rule) <|> ([] <$ (many wsp *> cnl)))

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # rule           =  rulename defined-as elements c-nl
rule :: Parser Rule
rule = Rule <$> rulename <*> definedAs <*> elements <* cnl

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # rulename       =  ALPHA *(ALPHA / DIGIT / "-")
rulename :: Parser RuleName
rulename = do
  bs <- takeWhile1 isAlpha <?> "rule name"
  rest <- takeWhile0 (\w -> isAlpha w || isDigit w || w == 0x2D)
  maybe empty pure (mkRuleName (bs <> rest))

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # defined-as     =  *c-wsp ("=" / "=/") *c-wsp
definedAs :: Parser DefinedAs
definedAs = many cwsp *> d <* many cwsp
  where d = byte 0x3D *> ((Incremental <$ byte 0x2F) <|> pure Basic)

-- erratum 2968: elements = alternation *WSP
elements :: Parser Alternation
elements = alternation <* many wsp

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # c-wsp          =  WSP / (c-nl WSP)
cwsp :: Parser ()
cwsp = wsp <|> (cnl *> wsp)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # c-nl           =  comment / CRLF
cnl :: Parser ()
cnl = comment <|> crlf

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # comment        =  ";" *(WSP / VCHAR) CRLF
comment :: Parser ()
comment = byte 0x3B *> void (takeWhile0 (\w -> isWSP w || isVChar w)) *> crlf

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # alternation    =  concatenation
-- # *(*c-wsp "/" *c-wsp concatenation)
alternation :: Parser Alternation
alternation = do
  c  <- concatenation
  cs <- many (many cwsp *> byte 0x2F *> many cwsp *> concatenation)
  pure (Alternation (c :| cs))

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # concatenation  =  repetition *(1*c-wsp repetition)
concatenation :: Parser Concatenation
concatenation = do
  r  <- repetition
  rs <- many (some cwsp *> repetition)
  pure (Concatenation (r :| rs))

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # repetition     =  [repeat] element
repetition :: Parser Repetition
repetition = Repetition <$> optional repeat_ <*> element

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # repeat         =  1*DIGIT / (*DIGIT "*" *DIGIT)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1
-- # The full form is "<n>#<m>element"
-- # indicating at least <n> and at most <m> elements
repeat_ :: Parser Repeat
repeat_ = do
  lo <- optional decimal
  let operator = (Star <$ byte 0x2A) <|> (Hash <$ byte 0x23)
  kind <- (Just <$> operator) <|> pure Nothing
  case (lo, kind) of
    (Just n, Nothing) -> pure (Repeat Star n (Just n))
    (Nothing, Nothing) -> empty
    (_, Just k)       -> Repeat k (fromMaybe 0 lo) <$> optional decimal
  where decimal = digitsValue 10 <$> takeWhile1 isDigit <?> "digit"

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # element        =  rulename / group / option /
-- # char-val / num-val / prose-val
element :: Parser Element
element =
      (RuleRef <$> rulename)
  <|> (Group <$> bracketed 0x28 0x29)
  <|> (Option <$> bracketed 0x5B 0x5D)
  <|> charVal
  <|> numVal
  <|> proseVal

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # group          =  "(" *c-wsp alternation *c-wsp ")"

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # option         =  "[" *c-wsp alternation *c-wsp "]"
bracketed :: Word8 -> Word8 -> Parser Alternation
bracketed open close =
  byte open *> many cwsp *> alternation <* many cwsp <* byte close

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # char-val       =  case-insensitive-string /
-- # case-sensitive-string

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # case-insensitive-string =
-- # [ "%i" ] quoted-string

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # case-sensitive-string =
-- # "%s" quoted-string
charVal :: Parser Element
charVal =
      (CharVal CaseInsensitive <$> (optional (string "%i") *> quotedString))
  <|> (CharVal CaseSensitive <$> (string "%s" *> quotedString))

-- = https://www.rfc-editor.org/rfc/rfc7405#section-2.2
-- # quoted-string  =  DQUOTE *(%x20-21 / %x23-7E) DQUOTE
quotedString :: Parser ByteString
quotedString = byte 0x22 *> takeWhile0 ok <* byte 0x22
  where ok w = (w >= 0x20 && w <= 0x21) || (w >= 0x23 && w <= 0x7E)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # num-val        =  "%" (bin-val / dec-val / hex-val)
numVal :: Parser Element
numVal = byte 0x25 *> (value 'b' Binary 2 isBit
                   <|> value 'd' Decimal 10 isDigit
                   <|> value 'x' Hexadecimal 16 isHexDig)
  where
    value c base radix ok = do
      letter c
      let digits = digitsValue radix <$> takeWhile1 ok <?> "digit"
      v <- digits
      body <- (Range v <$> (byte 0x2D *> digits))
          <|> (Values . (v :|) <$> some (byte 0x2E *> digits))
          <|> pure (Values (v :| []))
      pure (NumVal base body)

-- = https://www.rfc-editor.org/rfc/rfc5234#section-4
-- # prose-val      =  "<" *(%x20-3D / %x3F-7E) ">"
proseVal :: Parser Element
proseVal = ProseVal <$> (byte 0x3C *> takeWhile0 ok <* byte 0x3E)
  where ok w = (w >= 0x20 && w <= 0x3D) || (w >= 0x3F && w <= 0x7E)

-- | the value of digits that are known to be valid in the radix
digitsValue :: Natural -> ByteString -> Natural
digitsValue radix = B.foldl' (\acc w -> acc * radix + digitValue w) 0
  where
    digitValue w
      | isDigit w = fromIntegral (w - 0x30)
      | w >= 0x61 = fromIntegral (w - 0x61 + 10)
      | otherwise = fromIntegral (w - 0x41 + 10)
