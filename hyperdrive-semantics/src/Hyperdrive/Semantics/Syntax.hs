{-# LANGUAGE OverloadedStrings #-}
-- | Character classes and small syntax predicates shared across HTTP.
module Hyperdrive.Semantics.Syntax
  ( isTChar
  , isToken
  , isVChar
  , isObsText
  , isFieldVChar
  , isOWS
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word (Word8)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.2
-- # Tokens are short textual identifiers that do not include whitespace
-- # or delimiters.
-- # token          = 1*tchar
-- # tchar          = "!" / "#" / "$" / "%" / "&" / "'" / "*"
-- # / "+" / "-" / "." / "^" / "_" / "`" / "|" / "~"
-- # / DIGIT / ALPHA

-- | @tchar@
isTChar :: Word8 -> Bool
isTChar w =
     (w >= 0x30 && w <= 0x39)   -- DIGIT
  || (w >= 0x41 && w <= 0x5A)   -- ALPHA (upper)
  || (w >= 0x61 && w <= 0x7A)   -- ALPHA (lower)
  || B.elem w "!#$%&'*+-.^_`|~"

-- | @token = 1*tchar@
isToken :: ByteString -> Bool
isToken bs = not (B.null bs) && B.all isTChar bs

-- | @VCHAR@ from RFC 5234: visible (printing) US-ASCII characters
isVChar :: Word8 -> Bool
isVChar w = w >= 0x21 && w <= 0x7E

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # obs-text       = %x80-FF

-- | @obs-text@
isObsText :: Word8 -> Bool
isObsText w = w >= 0x80

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
-- # field-vchar    = VCHAR / obs-text

-- | @field-vchar@
isFieldVChar :: Word8 -> Bool
isFieldVChar w = isVChar w || isObsText w

-- = https://www.rfc-editor.org/rfc/rfc9110#section-5.6.3
-- # OWS            = *( SP / HTAB )

-- | a single octet of @OWS@ (@SP@ or @HTAB@)
isOWS :: Word8 -> Bool
isOWS w = w == 0x20 || w == 0x09
