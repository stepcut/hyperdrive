{-# LANGUAGE OverloadedStrings #-}
-- | Common syntax used inside field values (RFC 9110, Section 5.6):
-- lists, quoted strings, comments and parameters.
module Hyperdrive.Semantics.FieldSyntax
  ( FieldSyntaxError(..)
    -- * Lists
  , Comments(..)
  , splitList
  , fieldListElements
    -- * Entity tags
  , splitEntityTags
    -- * Quoted strings
  , parseQuotedString
  , quoteString
    -- * Parameters
  , parseParameters
  , lookupParameter
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)
import Hyperdrive.Semantics.Field (FieldName, FieldValue, Fields, fieldValueBytes, lookupAll)
import Hyperdrive.Semantics.Syntax (isFieldVChar, isOWS, isTChar, isVChar, isObsText)

data FieldSyntaxError
  = UnterminatedQuotedString
  | InvalidQuotedString      -- ^ a character that is not allowed in a quoted-string
  | UnterminatedComment
  | UnexpectedCloseParen     -- ^ a ")" with no matching "("
  | InvalidParameter
  | DuplicateParameter ByteString -- ^ the (lower case) name of a parameter given more than once
  | InvalidEntityTag         -- ^ an element of a list of entity tags that is not one
  deriving (Eq, Show)

------------------------------------------------------------------------------
-- Lists
------------------------------------------------------------------------------

-- | Whether the elements of a list may contain comments. Comments are
-- only allowed in fields whose definition says so, such as @Via@; in
-- other fields "(" and ")" are ordinary characters.
data Comments = NoComments | AllowComments
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # A
-- # recipient MUST parse and ignore a reasonable number of empty list
-- # elements: enough to handle common mistakes by senders that merge
-- # values, but not so much that they could be used as a denial-of-
-- # service mechanism.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.1.2
-- # In other words, a recipient MUST accept lists
-- # that satisfy the following syntax:
-- # #element => [ element ] *( OWS "," OWS [ element ] )

-- | Split a list-based field value into its elements.
--
-- Commas inside a quoted-string, or inside a comment when comments are
-- allowed, do not separate elements. Whitespace around each element is
-- removed and empty elements are dropped. Any number of empty elements
-- is accepted: they are skipped in linear time without allocation, so
-- the only limit needed is the one on the size of the field.
--
-- The result may be empty. Whether that is allowed depends on the
-- field: @1#element@ requires at least one element.
--
-- The elements themselves are not validated beyond their quoting;
-- that is up to the grammar of the particular field.
--
-- Not for lists of entity tags (@If-Match@, @If-None-Match@): a
-- backslash in an entity tag is an ordinary character, not an escape,
-- so @\"a\\\", \"b\"@ is two tags. Use 'splitEntityTags'.
splitList :: Comments -> FieldValue -> Either FieldSyntaxError [ByteString]
splitList comments fv = go 0 0
  where
    bs = fieldValueBytes fv
    n  = B.length bs

    -- start: where the current element began; i: the next octet
    go :: Int -> Int -> Either FieldSyntaxError [ByteString]
    go start i
      | i >= n    = Right (element start i)
      | otherwise =
          case B.index bs i of
            0x2C -> (element start i ++) <$> go (i + 1) (i + 1)  -- ","
            0x22 -> skipQuoted (i + 1) >>= go start              -- DQUOTE
            0x28 | comments == AllowComments -> skipComment 1 (i + 1) >>= go start
            0x29 | comments == AllowComments -> Left UnexpectedCloseParen
            _    -> go start (i + 1)

    element start end =
      let e = B.dropWhileEnd isOWS (B.dropWhile isOWS (B.take (end - start) (B.drop start bs)))
      in [ e | not (B.null e) ]

    -- the index just after the closing DQUOTE
    skipQuoted i
      | i >= n    = Left UnterminatedQuotedString
      | otherwise =
          case B.index bs i of
            0x22 -> Right (i + 1)
            0x5C -> skipQuoted (i + 2)   -- quoted-pair
            _    -> skipQuoted (i + 1)

    -- the index just after the ")" that closes the outermost comment
    skipComment :: Int -> Int -> Either FieldSyntaxError Int
    skipComment depth i
      | i >= n    = Left UnterminatedComment
      | otherwise =
          case B.index bs i of
            0x29 | depth == 1 -> Right (i + 1)
                 | otherwise  -> skipComment (depth - 1) (i + 1)
            0x28 -> skipComment (depth + 1) (i + 1)
            0x5C -> skipComment depth (i + 2)   -- quoted-pair
            _    -> skipComment depth (i + 1)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # A recipient MAY combine multiple field lines within a field section
-- # that have the same field name into one field line, without changing
-- # the semantics of the message, by appending each subsequent field line
-- # value to the initial field line value in order, separated by a comma
-- # (",") and optional whitespace (OWS, defined in Section 5.6.3).

-- | The elements of a list-based field, from every field line with
-- this name, in order.
--
-- Each field line is split on its own rather than after combining
-- them, so that a quote left open on one line cannot swallow the next.
-- For a well-formed message the result is the same.
fieldListElements :: Comments -> FieldName -> Fields -> Either FieldSyntaxError [ByteString]
fieldListElements comments name fields =
  concat <$> traverse (splitList comments) (lookupAll name fields)

------------------------------------------------------------------------------
-- Entity tags
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3
-- # entity-tag = [ weak ] opaque-tag
-- # weak       = %s"W/"
-- # opaque-tag = DQUOTE *etagc DQUOTE
-- # etagc      = %x21 / %x23-7E / obs-text
-- # ; VCHAR except double quotes, plus obs-text

-- = https://www.rfc-editor.org/rfc/rfc9110#section-8.8.3
-- # |  *Note:* Previously, opaque-tag was defined to be a quoted-
-- # |  string ([RFC2616], Section 3.11); thus, some recipients might
-- # |  perform backslash unescaping.  Servers therefore ought to avoid
-- # |  backslash characters in entity tags.

-- | Split a list of entity tags (@#entity-tag@, as in @If-Match@ and
-- @If-None-Match@) into its tags, each as written, with its @W/@ prefix
-- and quotes. A backslash is an ordinary character in an entity tag,
-- not an escape. Empty elements are skipped, as for any list; anything
-- that is not an entity tag is an error. The value @*@, which those
-- fields allow instead of a list, is for the caller to check first.
splitEntityTags :: FieldValue -> Either FieldSyntaxError [ByteString]
splitEntityTags fv = go 0
  where
    bs = fieldValueBytes fv
    n  = B.length bs
    at i = if i < n then Just (B.index bs i) else Nothing
    skipOWS i = if at i == Just 0x20 || at i == Just 0x09 then skipOWS (i + 1) else i

    -- before an element, or an empty one
    go i0 = let i = skipOWS i0 in case at i of
      Nothing   -> Right []
      Just 0x2C -> go (i + 1)
      _         -> do
        end <- entityTag i
        let j = skipOWS end
        rest <- case at j of
          Nothing   -> Right []
          Just 0x2C -> go (j + 1)
          _         -> Left InvalidEntityTag
        Right (B.take (end - i) (B.drop i bs) : rest)

    -- the index just after an entity tag that starts at i
    entityTag i
      | at i == Just 0x57, at (i + 1) == Just 0x2F = opaque (i + 2)   -- "W/"
      | otherwise                                  = opaque i
    opaque i
      | at i == Just 0x22 = close (i + 1)
      | otherwise         = Left InvalidEntityTag
    close i = case at i of
      Just 0x22 -> Right (i + 1)
      Just w | isETagC w -> close (i + 1)
      _ -> Left InvalidEntityTag
    isETagC w = w == 0x21 || (w >= 0x23 && w <= 0x7E) || isObsText w

------------------------------------------------------------------------------
-- Quoted strings
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.4
-- # quoted-string  = DQUOTE *( qdtext / quoted-pair ) DQUOTE
-- # qdtext         = HTAB / SP / %x21 / %x23-5B / %x5D-7E / obs-text
isQDText :: Word8 -> Bool
isQDText w = w == 0x09 || w == 0x20 || w == 0x21
          || (w >= 0x23 && w <= 0x5B) || (w >= 0x5D && w <= 0x7E) || isObsText w

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.4
-- # quoted-pair    = "\" ( HTAB / SP / VCHAR / obs-text )
isQuotedPairChar :: Word8 -> Bool
isQuotedPairChar w = w == 0x09 || w == 0x20 || isVChar w || isObsText w

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.4
-- # Recipients
-- # that process the value of a quoted-string MUST handle a quoted-pair
-- # as if it were replaced by the octet following the backslash.

-- | Parse a quoted-string at the start of the input. Returns its value,
-- with each quoted-pair replaced by the octet it quotes, and the rest
-- of the input.
parseQuotedString :: ByteString -> Either FieldSyntaxError (ByteString, ByteString)
parseQuotedString bs =
  case B.uncons bs of
    Just (0x22, rest) -> go [] rest
    _                 -> Left InvalidQuotedString
  where
    go acc s =
      case B.uncons s of
        Nothing -> Left UnterminatedQuotedString
        Just (0x22, rest) -> Right (B.pack (reverse acc), rest)
        Just (0x5C, rest) ->
          case B.uncons rest of
            Nothing -> Left UnterminatedQuotedString
            Just (c, rest')
              | isQuotedPairChar c -> go (c : acc) rest'
              | otherwise          -> Left InvalidQuotedString
        Just (c, rest)
          | isQDText c -> go (c : acc) rest
          | otherwise  -> Left InvalidQuotedString

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.4
-- # A sender SHOULD NOT generate a quoted-pair in a quoted-string except
-- # where necessary to quote DQUOTE and backslash octets occurring within
-- # that string.

-- | Render a value as a quoted-string, escaping only DQUOTE and
-- backslash. Returns 'Nothing' if the value contains a control
-- character other than HTAB, which no quoted-string can carry.
quoteString :: ByteString -> Maybe ByteString
quoteString v
  | B.all (\w -> isFieldVChar w || w == 0x20 || w == 0x09) v =
      Just ("\"" <> B.concatMap escape v <> "\"")
  | otherwise = Nothing
  where
    escape w | w == 0x22 || w == 0x5C = B.pack [0x5C, w]
             | otherwise              = B.singleton w

------------------------------------------------------------------------------
-- Parameters
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.6
-- # parameters      = *( OWS ";" OWS [ parameter ] )
-- # parameter       = parameter-name "=" parameter-value
-- # parameter-name  = token
-- # parameter-value = ( token / quoted-string )

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.6
-- # Parameter names are case-insensitive.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.6
-- # A parameter value that matches the token production can be
-- # transmitted either as a token or within a quoted-string.  The quoted
-- # and unquoted values are equivalent.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.6
-- # *Note:* Parameters do not allow whitespace (not even "bad"
-- # whitespace) around the "=" character.

-- | Parse @parameters@: everything after the item they belong to, for
-- example the @; q=0.5@ of @gzip; q=0.5@. Names are returned in lower
-- case, since they are case-insensitive, and quoted values are
-- unquoted, since quoted and unquoted values are equivalent. Empty
-- parameters (@;;@) are skipped. Parameters are returned in order,
-- including any repeated names; use 'lookupParameter' to read them.
--
-- A value can be any printable text, such as @"../../etc/passwd"@.
-- Values that name files, like a @filename@ parameter, must be checked
-- with "Hyperdrive.Semantics.SafePath" before use.
parseParameters :: ByteString -> Either FieldSyntaxError [(ByteString, ByteString)]
parseParameters = go
  where
    go s =
      let s' = B.dropWhile isOWS s
      in case B.uncons s' of
           Nothing | B.null s    -> Right []
                   | otherwise   -> Left InvalidParameter   -- OWS must be followed by ";"
           Just (0x3B, rest)     -> parameter (B.dropWhile isOWS rest)   -- ";"
           Just _                -> Left InvalidParameter

    parameter s
      | B.null s || B.head s == 0x3B = go s                           -- empty parameter
      | otherwise =
          let (name, rest) = B.span isTChar s
          in case B.uncons rest of
               Just (0x3D, valueAndRest) | not (B.null name) -> do     -- "="
                 (value, rest') <- parameterValue valueAndRest
                 ((B.map toLower name, value) :) <$> go rest'
               _ -> Left InvalidParameter

    parameterValue s
      | B.take 1 s == "\"" = parseQuotedString s
      | otherwise =
          let (value, rest) = B.span isTChar s
          in if B.null value then Left InvalidParameter else Right (value, rest)

    toLower w | w >= 0x41 && w <= 0x5A = w + 0x20
              | otherwise              = w

-- | The value of the parameter with this name, which is compared
-- case-insensitively. A name given more than once is an error rather
-- than a choice between its values: as with repeated singleton fields,
-- different implementations choose differently, and an attacker can
-- send @charset=utf-8;charset=utf-7@ hoping that a filter checks one
-- and the application uses the other. Only the name asked for is
-- checked: another parameter given twice does not make this fail. /O(n)/
lookupParameter :: ByteString -> [(ByteString, ByteString)] -> Either FieldSyntaxError (Maybe ByteString)
lookupParameter name ps =
  case [ v | (k, v) <- ps, k == lname ] of
    []  -> Right Nothing
    [v] -> Right (Just v)
    _   -> Left (DuplicateParameter lname)
  where
    lname = B.map lower name
    lower w | w >= 0x41 && w <= 0x5A = w + 0x20
            | otherwise              = w
