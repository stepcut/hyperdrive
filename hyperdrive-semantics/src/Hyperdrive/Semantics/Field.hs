{-# LANGUAGE OverloadedStrings #-}
-- | Fields (RFC 9110, Section 5)
module Hyperdrive.Semantics.Field
  ( -- * Field names
    FieldName
  , mkFieldName
  , fieldNameBytes
    -- * Field values
  , FieldValue
  , FieldValueError(..)
  , mkFieldValue
  , fieldValueBytes
  , combineValues
    -- * Field sections
  , Fields
  , emptyFields
  , fieldsFromList
  , fieldsToList
  , appendField
  , lookupField
  , lookupAll
    -- * Common field names
  , hConnection
  , hContentLength
  , hContentType
  , hHost
  , hSetCookie
  , hTransferEncoding
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Foldable (toList)
import Data.Maybe (listToMaybe)
import Data.Sequence ((|>))
import qualified Data.Sequence as Seq
import Data.Word (Word8)
import Hyperdrive.Semantics.Internal (FieldName(..), FieldValue(..), Fields(..))
import Hyperdrive.Semantics.Syntax (isFieldVChar, isOWS, isToken)

------------------------------------------------------------------------------
-- Field names
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.1
-- # field-name     = token
-- # Field names are case-insensitive

-- | Validate a field name. Because field names are case-insensitive,
-- the name is normalized to lower case, which is also the form that
-- HTTP/2 and HTTP/3 require on the wire.
mkFieldName :: ByteString -> Maybe FieldName
mkFieldName bs
  | isToken bs = Just (FieldName (B.map toLower bs))
  | otherwise  = Nothing
  where
    toLower :: Word8 -> Word8
    toLower w
      | w >= 0x41 && w <= 0x5A = w + 0x20
      | otherwise              = w

-- | the field name, in lower case
fieldNameBytes :: FieldName -> ByteString
fieldNameBytes (FieldName bs) = bs

hConnection, hContentLength, hContentType, hHost, hSetCookie, hTransferEncoding :: FieldName
hConnection       = FieldName "connection"
hContentLength    = FieldName "content-length"
hContentType      = FieldName "content-type"
hHost             = FieldName "host"
hSetCookie        = FieldName "set-cookie"
hTransferEncoding = FieldName "transfer-encoding"

------------------------------------------------------------------------------
-- Field values
------------------------------------------------------------------------------

data FieldValueError
  = ContainsCRLFNUL -- ^ the value contains CR, LF or NUL
  | ContainsCTL     -- ^ the value contains some other control character
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # field-value    = *field-content
-- # field-content  = field-vchar
-- # [ 1*( SP / HTAB / field-vchar ) field-vchar ]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # When
-- # a specific version of HTTP allows such whitespace to appear in a
-- # message, a field parsing implementation MUST exclude such whitespace
-- # prior to evaluating the field value.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # Field values containing CR, LF, or NUL characters are invalid and
-- # dangerous, due to the varying ways that implementations might parse
-- # and interpret those characters; a recipient of CR, LF, or NUL within
-- # a field value MUST either reject the message or replace each of those
-- # characters with SP before further processing or forwarding of that
-- # message.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # Field values containing other CTL characters are also
-- # invalid; however, recipients MAY retain such characters for the sake
-- # of robustness when they appear within a safe context (e.g., an
-- # application-specific quoted string that will not be processed by any
-- # downstream HTTP parser).

-- | Validate a field value as received from the wire.
--
-- Leading and trailing whitespace is removed. Values containing CR,
-- LF or NUL are rejected rather than repaired. Values containing any
-- other control character are also rejected: we do not take up the
-- option to retain them.
--
-- RFC 9110 requires rejecting the whole /message/, not just the field
-- line, so a 'Left' must lead to the request being refused (typically
-- with 400 Bad Request). Dropping the field line and carrying on is
-- neither of the two behaviours the RFC allows.
mkFieldValue :: ByteString -> Either FieldValueError FieldValue
mkFieldValue bs
  | B.any isCRLFNUL v                            = Left ContainsCRLFNUL
  | not (B.all (\w -> isFieldVChar w || isOWS w) v) = Left ContainsCTL
  | otherwise                                    = Right (FieldValue v)
  where
    v = stripOWS bs
    isCRLFNUL w = w == 0x0D || w == 0x0A || w == 0x00

stripOWS :: ByteString -> ByteString
stripOWS = B.dropWhileEnd isOWS . B.dropWhile isOWS

fieldValueBytes :: FieldValue -> ByteString
fieldValueBytes (FieldValue bs) = bs

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # A recipient MAY combine multiple field lines within a field section
-- # that have the same field name into one field line, without changing
-- # the semantics of the message, by appending each subsequent field line
-- # value to the initial field line value in order, separated by a comma
-- # (",") and optional whitespace (OWS, defined in Section 5.6.3).  For
-- # consistency, use comma SP.

-- | Combine the values of several field lines with the same name into
-- a single value. Returns 'Nothing' for an empty list.
--
-- This is only meaningful for fields whose definition allows a
-- comma-separated list. Notably, @Set-Cookie@ does not.
combineValues :: [FieldValue] -> Maybe FieldValue
combineValues [] = Nothing
combineValues vs =
  -- an empty value at either end would leave a stray SP behind
  Just (FieldValue (stripOWS (B.intercalate ", " (map fieldValueBytes vs))))

------------------------------------------------------------------------------
-- Field sections
------------------------------------------------------------------------------

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.3
-- # The order in which field lines with the same name are received is
-- # therefore significant to the interpretation of the field value

emptyFields :: Fields
emptyFields = Fields Seq.empty

fieldsFromList :: [(FieldName, FieldValue)] -> Fields
fieldsFromList = Fields . Seq.fromList

-- | the field lines, in order
fieldsToList :: Fields -> [(FieldName, FieldValue)]
fieldsToList (Fields fs) = toList fs

-- | add a field line after all existing field lines. /O(1)/
appendField :: FieldName -> FieldValue -> Fields -> Fields
appendField n v (Fields fs) = Fields (fs |> (n, v))

-- | the value of the first field line with this name. /O(n)/
lookupField :: FieldName -> Fields -> Maybe FieldValue
lookupField n = listToMaybe . lookupAll n

-- | the values of every field line with this name, in order. /O(n)/
lookupAll :: FieldName -> Fields -> [FieldValue]
lookupAll n (Fields fs) = [ v | (n', v) <- toList fs, n' == n ]
