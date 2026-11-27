{-# LANGUAGE OverloadedStrings #-}
-- | Structured Field Values for HTTP (RFC 9651): the parsing algorithms
-- of its Section 4.2 and the serialization of its Section 4.1, step for
-- step (docs/priorities-design.md, P1). Tested against the httpwg's
-- structured-field-tests.
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-1.2
-- # When parsing from HTTP fields, implementations MUST have behavior
-- # that is indistinguishable from following the algorithms.
module Hyperdrive.Semantics.StructuredField
  ( -- * Values
    BareItem(..)
  , Item(..)
  , Member(..)
  , Parameters
  , List
  , Dictionary
  , parameter
  , dictionaryMember
    -- * Parsing (Section 4.2)
  , combineFieldLines
  , parseList
  , parseDictionary
  , parseItem
    -- * Serializing (Section 4.1)
  , serializeList
  , serializeDictionary
  , serializeItem
  ) where

import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.Ratio ((%))
import Data.Word (Word8)

-- | A bare Item (Section 3.3).
data BareItem
  = SFInteger !Integer          -- ^ at most 15 digits once serialized
  | SFDecimal !Rational         -- ^ at most 12 integer and 3 fractional digits once serialized
  | SFString !ByteString        -- ^ printable ASCII
  | SFToken !ByteString
  | SFByteSequence !ByteString
  | SFBoolean !Bool
  | SFDate !Integer             -- ^ seconds since 1970-01-01T00:00:00Z
  | SFDisplayString !ByteString -- ^ Unicode, as UTF-8
  deriving (Eq, Show)

-- | Parameters (Section 3.1.2): keys in order, each once.
type Parameters = [(ByteString, BareItem)]

-- | An Item with its Parameters (Section 3.3).
data Item = Item !BareItem !Parameters
  deriving (Eq, Show)

-- | A member of a List or Dictionary: an Item, or an Inner List with
-- its Parameters (Section 3.1.1).
data Member = MemberItem !Item | InnerList ![Item] !Parameters
  deriving (Eq, Show)

type List = [Member]

-- | A Dictionary (Section 3.2): keys in order, each once.
type Dictionary = [(ByteString, Member)]

-- = https://www.rfc-editor.org/rfc/rfc9651#section-3.1.2
-- # Implementations MUST provide access to Parameters both by index and
-- # by key.
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-3.2
-- # Implementations MUST provide access to Dictionaries both by index and
-- # by key.
--
-- By index: they are lists. By key:

parameter :: ByteString -> Parameters -> Maybe BareItem
parameter = lookup

dictionaryMember :: ByteString -> Dictionary -> Maybe Member
dictionaryMember = lookup

------------------------------------------------------------------------------
-- Parsing (Section 4.2)
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9651#section-4.2
-- # When generating input_bytes, parsers MUST combine all field lines in
-- # the same section (header or trailer) that case-insensitively match
-- # the field name into one comma-separated field-value, as per
-- # Section 5.2 of [HTTP]; this assures that the entire field value is
-- # processed correctly.

-- | A field's lines, combined as HTTP combines them.
combineFieldLines :: [ByteString] -> ByteString
combineFieldLines = B.intercalate ", "

-- A parser: the rest of input_string, and the value, or a failure.
newtype P a = P { runP :: ByteString -> Maybe (a, ByteString) }

instance Functor P where
  fmap f (P p) = P $ \s -> fmap (\(a, s') -> (f a, s')) (p s)

instance Applicative P where
  pure a = P $ \s -> Just (a, s)
  P pf <*> P pa = P $ \s -> case pf s of
    Nothing -> Nothing
    Just (f, s') -> fmap (\(a, s'') -> (f a, s'')) (pa s')

instance Monad P where
  P p >>= k = P $ \s -> case p s of
    Nothing -> Nothing
    Just (a, s') -> runP (k a) s'

failP :: P a
failP = P (const Nothing)

peek :: P (Maybe Word8)
peek = P $ \s -> Just (if B.null s then Nothing else Just (B.head s), s)

atEnd :: P Bool
atEnd = P $ \s -> Just (B.null s, s)

consume :: P Word8
consume = P B.uncons

-- consume the character if it is this one
consumeIf :: Word8 -> P Bool
consumeIf c = P $ \s -> case B.uncons s of
  Just (x, rest) | x == c -> Just (True, rest)
  _ -> Just (False, s)

discardWhile :: (Word8 -> Bool) -> P ()
discardWhile f = P $ \s -> Just ((), B.dropWhile f s)

takeWhileP :: (Word8 -> Bool) -> P ByteString
takeWhileP f = P $ \s -> Just (B.span f s)

isSP, isOWS :: Word8 -> Bool
isSP = (== 0x20)
isOWS c = c == 0x20 || c == 0x09

-- | Parse a whole field value as a type (Section 4.2's steps 1 to 8).
parseAs :: P a -> ByteString -> Maybe a
parseAs p input
  -- step 1: the value must be ASCII (each type's own checks reject
  -- every octet above 0x7E as well; kept as the algorithm's step)
  | B.any (> 0x7F) input = Nothing
  | otherwise = case runP (discardWhile isSP >> p) input of
      Just (a, rest) | B.null (B.dropWhile isSP rest) -> Just a
      _ -> Nothing

-- = https://www.rfc-editor.org/rfc/rfc9651#section-4.2
-- # If parsing fails, either the entire field value MUST be ignored (i.e.,
-- # treated as if the field were not present in the section), or
-- # alternatively the complete HTTP message MUST be treated as malformed.
--
-- A failure is 'Nothing': the caller ignores the field.

parseList :: ByteString -> Maybe List
parseList = parseAs listP

parseDictionary :: ByteString -> Maybe Dictionary
parseDictionary = parseAs dictionaryP

parseItem :: ByteString -> Maybe Item
parseItem = parseAs itemP

-- Section 4.2.1
listP :: P List
listP = go []
  where
    go members = atEnd >>= \e -> if e then pure (reverse members) else do
      m <- itemOrInnerListP
      discardWhile isOWS
      e' <- atEnd
      if e' then pure (reverse (m : members)) else do
        c <- consume
        if c /= 0x2C then failP else do
          discardWhile isOWS
          trailing <- atEnd
          if trailing then failP else go (m : members)

-- Section 4.2.1.1
itemOrInnerListP :: P Member
itemOrInnerListP = peek >>= \c -> if c == Just 0x28 then innerListP else MemberItem <$> itemP

-- Section 4.2.1.2
innerListP :: P Member
innerListP = do
  c <- consume
  if c /= 0x28 then failP else go []
  where
    go items = atEnd >>= \e -> if e then failP else do
      discardWhile isSP
      close <- consumeIf 0x29
      if close then InnerList (reverse items) <$> parametersP else do
        item <- itemP
        next <- peek
        if next == Just 0x20 || next == Just 0x29 then go (item : items) else failP

-- Section 4.2.2
dictionaryP :: P Dictionary
dictionaryP = go []
  where
    go dict = atEnd >>= \e -> if e then pure dict else do
      key <- keyP
      equals <- consumeIf 0x3D
      member <- if equals then itemOrInnerListP else MemberItem . Item (SFBoolean True) <$> parametersP
      let dict' = insertOrdered key member dict
      discardWhile isOWS
      e' <- atEnd
      if e' then pure dict' else do
        c <- consume
        if c /= 0x2C then failP else do
          discardWhile isOWS
          trailing <- atEnd
          if trailing then failP else go dict'

-- an ordered map's insertion: a key already there keeps its place and
-- takes the new value (Sections 4.2.2 and 4.2.3.2)
insertOrdered :: ByteString -> v -> [(ByteString, v)] -> [(ByteString, v)]
insertOrdered k v m
  | any ((== k) . fst) m = [ (k', if k' == k then v else v') | (k', v') <- m ]
  | otherwise = m ++ [(k, v)]

-- Section 4.2.3
itemP :: P Item
itemP = Item <$> bareItemP <*> parametersP

-- Section 4.2.3.1
bareItemP :: P BareItem
bareItemP = peek >>= \c -> case c of
  Just x | x == 0x2D || isDigitW x -> numberP
         | x == 0x22 -> SFString <$> stringP
         | isAlphaW x || x == 0x2A -> SFToken <$> tokenP
         | x == 0x3A -> SFByteSequence <$> byteSequenceP
         | x == 0x3F -> SFBoolean <$> booleanP
         | x == 0x40 -> dateP
         | x == 0x25 -> SFDisplayString <$> displayStringP
  _ -> failP

-- Section 4.2.3.2
parametersP :: P Parameters
parametersP = go []
  where
    go params = do
      semicolon <- consumeIf 0x3B
      if not semicolon then pure params else do
        discardWhile isSP
        key <- keyP
        equals <- consumeIf 0x3D
        value <- if equals then bareItemP else pure (SFBoolean True)
        go (insertOrdered key value params)

-- Section 4.2.3.3
keyP :: P ByteString
keyP = peek >>= \c -> case c of
  Just x | isLcAlpha x || x == 0x2A -> takeWhileP isKeyChar
  _ -> failP

isKeyChar :: Word8 -> Bool
isKeyChar x = isLcAlpha x || isDigitW x || x `B.elem` "_-.*"

-- Section 4.2.4
numberP :: P BareItem
numberP = do
  negative <- consumeIf 0x2D
  e <- atEnd
  first <- peek
  if e || maybe True (not . isDigitW) first then failP else do
    (digits, isDecimal) <- go B.empty False
    let sign = if negative then -1 else 1
    if not isDecimal then pure (SFInteger (sign * readInteger digits)) else do
      let (whole, dotFrac) = B.break (== 0x2E) digits
          frac = B.drop 1 dotFrac
          scale = 10 ^ B.length frac
      if B.null frac || B.length frac > 3 then failP
      else pure (SFDecimal ((sign * (readInteger whole * scale + readInteger frac)) % scale))
  where
    go acc isDecimal = peek >>= \c -> case c of
      Just x | isDigitW x -> consume >> check (B.snoc acc x) isDecimal
             | x == 0x2E && not isDecimal ->
                 if B.length acc > 12 then failP else consume >> check (B.snoc acc x) True
      _ -> pure (acc, isDecimal)
    check acc isDecimal
      | not isDecimal && B.length acc > 15 = failP
      | isDecimal && B.length acc > 16 = failP
      | otherwise = go acc isDecimal

readInteger :: ByteString -> Integer
readInteger = B.foldl' (\n d -> n * 10 + fromIntegral (d - 0x30)) 0

-- Section 4.2.5
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-3.3.3
-- # Furthermore, only DQUOTE and "\" can be escaped; other characters
-- # after "\" MUST cause parsing to fail.
stringP :: P ByteString
stringP = do
  c <- consume
  if c /= 0x22 then failP else go []
  where
    go acc = atEnd >>= \e -> if e then failP else do
      c <- consume
      if c == 0x5C then atEnd >>= \e' -> if e' then failP else do
        next <- consume
        if next == 0x22 || next == 0x5C then go (next : acc) else failP
      else if c == 0x22 then pure (B.pack (reverse acc))
      else if c < 0x20 || c > 0x7E then failP
      else go (c : acc)

-- Section 4.2.6
tokenP :: P ByteString
tokenP = peek >>= \c -> case c of
  Just x | isAlphaW x || x == 0x2A -> takeWhileP (\y -> isTChar y || y == 0x3A || y == 0x2F)
  _ -> failP

-- Section 4.2.7
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-4.2.7
-- # This specification does not relax the requirements in Sections 3.1
-- # and 3.3 of [RFC4648]; therefore, parsers MUST fail on characters
-- # outside the base64 alphabet and on line feeds in encoded data.
byteSequenceP :: P ByteString
byteSequenceP = do
  c <- consume
  if c /= 0x3A then failP else do
    content <- takeWhileP (/= 0x3A)
    closing <- consumeIf 0x3A
    if not closing || B.any (not . isBase64Char) content then failP
    else maybe failP pure (base64Decode content)

isBase64Char :: Word8 -> Bool
isBase64Char x = isAlphaW x || isDigitW x || x == 0x2B || x == 0x2F || x == 0x3D

-- = https://www.rfc-editor.org/rfc/rfc9651#section-4.2.7
-- # Because some implementations of base64 do not allow rejection of
-- # encoded data that is not properly "=" padded (see [RFC4648],
-- # Section 3.2), parsers SHOULD NOT fail when "=" padding is not
-- # present, unless they cannot be configured to do so.
--
-- = https://www.rfc-editor.org/rfc/rfc9651#section-4.2.7
-- # Because some implementations of base64 do not allow rejection of
-- # encoded data that has non-zero pad bits (see [RFC4648], Section 3.5),
-- # parsers SHOULD NOT fail when non-zero pad bits are present, unless
-- # they cannot be configured to do so.

-- | Base64 (RFC 4648, Section 4), padding synthesized if missing, pad
-- bits not checked; "=" only at the end.
base64Decode :: ByteString -> Maybe ByteString
base64Decode input
  | B.any (== 0x3D) body = Nothing
  | B.length padding > 2 = Nothing
  | B.length body `mod` 4 == 1 = Nothing
  | not (B.null padding) && (B.length body + B.length padding) `mod` 4 /= 0 = Nothing
  | otherwise = Just (B.pack (decode (map value (B.unpack body))))
  where
    (body, padding) = B.spanEnd (== 0x3D) input
    value x
      | x >= 0x41 && x <= 0x5A = x - 0x41
      | x >= 0x61 && x <= 0x7A = x - 0x61 + 26
      | x >= 0x30 && x <= 0x39 = x - 0x30 + 52
      | x == 0x2B = 62
      | otherwise = 63
    decode (a : b : c : d : rest) =
      fromIntegral (word a b c d `shiftR` 16) : fromIntegral (word a b c d `shiftR` 8 .&. 0xFF)
        : fromIntegral (word a b c d .&. 0xFF) : decode rest
    decode [a, b, c] = [fromIntegral (word a b c 0 `shiftR` 16), fromIntegral (word a b c 0 `shiftR` 8 .&. 0xFF)]
    decode [a, b] = [fromIntegral (word a b 0 0 `shiftR` 16)]
    decode _ = []
    word :: Word8 -> Word8 -> Word8 -> Word8 -> Int
    word a b c d = fromIntegral a `shiftL` 18 .|. fromIntegral b `shiftL` 12 .|. fromIntegral c `shiftL` 6 .|. fromIntegral d

-- Section 4.2.8
booleanP :: P Bool
booleanP = do
  c <- consume
  if c /= 0x3F then failP else do
    v <- consume
    case v of
      0x31 -> pure True
      0x30 -> pure False
      _ -> failP

-- Section 4.2.9
dateP :: P BareItem
dateP = do
  c <- consume
  if c /= 0x40 then failP else numberP >>= \n -> case n of
    SFInteger i -> pure (SFDate i)
    _ -> failP

-- Section 4.2.10
displayStringP :: P ByteString
displayStringP = do
  percent <- consumeIf 0x25
  quote <- consumeIf 0x22
  if not (percent && quote) then failP else go []
  where
    go acc = atEnd >>= \e -> if e then failP else do
      c <- consume
      if c < 0x20 || c > 0x7E then failP
      else if c == 0x25 then do
        h1 <- consume
        h2 <- consume
        case (hexValue h1, hexValue h2) of
          (Just a, Just b) -> go (a * 16 + b : acc)
          _ -> failP
      else if c == 0x22 then
        let bytes = B.pack (reverse acc) in if validUtf8 bytes then pure bytes else failP
      else go (c : acc)
    hexValue x
      | isDigitW x = Just (x - 0x30)
      | x >= 0x61 && x <= 0x66 = Just (x - 0x61 + 10)
      | otherwise = Nothing

-- | Whether the octets are well-formed UTF-8 (RFC 3629): no overlong
-- forms, no surrogates, nothing above U+10FFFF.
validUtf8 :: ByteString -> Bool
validUtf8 = go . B.unpack
  where
    go [] = True
    go (a : rest)
      | a < 0x80 = go rest
      | a >= 0xC2 && a <= 0xDF = cont 1 rest
      | a == 0xE0 = second (>= 0xA0) 1 rest
      | a == 0xED = second (<= 0x9F) 1 rest
      | a >= 0xE1 && a <= 0xEF = cont 2 rest
      | a == 0xF0 = second (>= 0x90) 2 rest
      | a == 0xF4 = second (<= 0x8F) 2 rest
      | a >= 0xF1 && a <= 0xF3 = cont 3 rest
      | otherwise = False
    isCont b = b >= 0x80 && b <= 0xBF
    cont :: Int -> [Word8] -> Bool
    cont n bs = let (cs, rest) = splitAt n bs in length cs == n && all isCont cs && go rest
    second ok n (b : rest) = isCont b && ok b && cont n rest
    second _ _ [] = False

isDigitW, isAlphaW, isLcAlpha, isTChar :: Word8 -> Bool
isDigitW x = x >= 0x30 && x <= 0x39
isAlphaW x = (x >= 0x41 && x <= 0x5A) || (x >= 0x61 && x <= 0x7A)
isLcAlpha x = x >= 0x61 && x <= 0x7A
isTChar x = isAlphaW x || isDigitW x || x `B.elem` "!#$%&'*+-.^_`|~"

------------------------------------------------------------------------------
-- Serializing (Section 4.1)
------------------------------------------------------------------------------

-- | A List's field value; the empty string for no members (the field is
-- then not sent). 'Nothing' if a value cannot be serialized.
serializeList :: List -> Maybe ByteString
serializeList members = B.intercalate ", " <$> mapM memberS members

serializeDictionary :: Dictionary -> Maybe ByteString
serializeDictionary dict = B.intercalate ", " <$> mapM entry dict
  where
    -- = https://www.rfc-editor.org/rfc/rfc9651#section-3.2
    -- # Members whose value is Boolean (see Section 3.3.6) true MUST omit
    -- # that value when serialized.
    entry (k, MemberItem (Item (SFBoolean True) params)) = (<>) <$> keyS k <*> parametersS params
    entry (k, m) = (\k' m' -> k' <> "=" <> m') <$> keyS k <*> memberS m

serializeItem :: Item -> Maybe ByteString
serializeItem (Item bare params) = (<>) <$> bareItemS bare <*> parametersS params

memberS :: Member -> Maybe ByteString
memberS (MemberItem item) = serializeItem item
memberS (InnerList items params) =
  (\is ps -> "(" <> B.intercalate " " is <> ")" <> ps) <$> mapM serializeItem items <*> parametersS params

parametersS :: Parameters -> Maybe ByteString
parametersS params = B.concat <$> mapM one params
  where
    -- = https://www.rfc-editor.org/rfc/rfc9651#section-3.1.2
    -- # Parameters whose value is Boolean (see Section 3.3.6) true MUST omit
    -- # that value when serialized.
    one (k, SFBoolean True) = (";" <>) <$> keyS k
    one (k, v) = (\k' v' -> ";" <> k' <> "=" <> v') <$> keyS k <*> bareItemS v

keyS :: ByteString -> Maybe ByteString
keyS k = case B.uncons k of
  Just (c, rest) | (isLcAlpha c || c == 0x2A) && B.all isKeyChar rest -> Just k
  _ -> Nothing

bareItemS :: BareItem -> Maybe ByteString
bareItemS item = case item of
  SFInteger n -> integerS n
  SFDecimal r -> decimalS r
  SFString s
    | B.all (\c -> c >= 0x20 && c <= 0x7E) s -> Just ("\"" <> B.concatMap escape s <> "\"")
    | otherwise -> Nothing
  SFToken t -> case B.uncons t of
    Just (c, rest) | (isAlphaW c || c == 0x2A) && B.all (\y -> isTChar y || y == 0x3A || y == 0x2F) rest -> Just t
    _ -> Nothing
  SFByteSequence b -> Just (":" <> base64Encode b <> ":")
  SFBoolean b -> Just (if b then "?1" else "?0")
  SFDate n -> ("@" <>) <$> integerS n
  SFDisplayString u
    | validUtf8 u -> Just ("%\"" <> B.concatMap percent u <> "\"")
    | otherwise -> Nothing
  where
    escape c
      | c == 0x22 || c == 0x5C = B.pack [0x5C, c]
      | otherwise = B.singleton c
    percent c
      | c == 0x25 || c == 0x22 || c < 0x20 || c > 0x7E =
          B.pack [0x25, hexDigit (c `shiftR` 4), hexDigit (c .&. 0x0F)]
      | otherwise = B.singleton c
    hexDigit d = if d < 10 then 0x30 + d else 0x61 + d - 10

-- Section 4.1.4
integerS :: Integer -> Maybe ByteString
integerS n
  | abs n > 999999999999999 = Nothing
  | otherwise = Just (C.pack (show n))

-- Section 4.1.5: rounded to three fractional digits, ties to even
-- (Haskell's 'round'), at most twelve integer digits, trailing zeros of
-- the fraction dropped but one digit kept
decimalS :: Rational -> Maybe ByteString
decimalS r
  | whole > 999999999999 = Nothing
  | otherwise = Just (C.pack (sign ++ show whole ++ "." ++ fraction))
  where
    thousandths = round (r * 1000) :: Integer
    sign = if thousandths < 0 then "-" else ""
    (whole, frac) = abs thousandths `divMod` 1000
    digits = let s = show frac in replicate (3 - length s) '0' ++ s
    fraction = case reverse (dropWhile (== '0') (reverse digits)) of
      "" -> "0"
      f -> f

-- = https://www.rfc-editor.org/rfc/rfc9651#section-4.1.8
-- # Likewise, encoded data SHOULD have pad bits set to zero, as per
-- # [RFC4648], Section 3.5, unless it is not possible to do so due to
-- # implementation constraints.

-- | Base64 with padding (RFC 4648, Section 4); pad bits zero.
base64Encode :: ByteString -> ByteString
base64Encode = B.pack . go . B.unpack
  where
    go (a : b : c : rest) = let w = triple a b c in map (char . (\s -> w `shiftR` s .&. 63)) [18, 12, 6, 0] ++ go rest
    go [a, b] = let w = triple a b 0 in map (char . (\s -> w `shiftR` s .&. 63)) [18, 12, 6] ++ [0x3D]
    go [a] = let w = triple a 0 0 in map (char . (\s -> w `shiftR` s .&. 63)) [18, 12] ++ [0x3D, 0x3D]
    go [] = []
    triple :: Word8 -> Word8 -> Word8 -> Int
    triple a b c = fromIntegral a `shiftL` 16 .|. fromIntegral b `shiftL` 8 .|. fromIntegral c
    char :: Int -> Word8
    char v
      | v < 26 = fromIntegral (0x41 + v)
      | v < 52 = fromIntegral (0x61 + v - 26)
      | v < 62 = fromIntegral (0x30 + v - 52)
      | v == 62 = 0x2B
      | otherwise = 0x2F

