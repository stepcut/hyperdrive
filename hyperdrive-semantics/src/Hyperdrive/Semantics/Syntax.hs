{-# LANGUAGE OverloadedStrings #-}
-- | Character classes and small syntax predicates shared across HTTP.
module Hyperdrive.Semantics.Syntax
  ( isTChar
  , isToken
  , isVChar
  , isObsText
  , isFieldVChar
  , isOWS
    -- * URI character classes (RFC 3986)
  , isALPHA
  , isDIGIT
  , isHEXDIG
  , isUnreserved
  , isSubDelim
  , isPChar
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

------------------------------------------------------------------------------
-- URI character classes
------------------------------------------------------------------------------

-- | @ALPHA@ from RFC 5234
isALPHA :: Word8 -> Bool
isALPHA w = (w >= 0x41 && w <= 0x5A) || (w >= 0x61 && w <= 0x7A)

-- | @DIGIT@ from RFC 5234
isDIGIT :: Word8 -> Bool
isDIGIT w = w >= 0x30 && w <= 0x39

-- | @HEXDIG@ from RFC 5234. ABNF strings are case-insensitive, so this
-- includes the lower case letters.
isHEXDIG :: Word8 -> Bool
isHEXDIG w = isDIGIT w || (w >= 0x41 && w <= 0x46) || (w >= 0x61 && w <= 0x66)

-- = https://www.rfc-editor.org/rfc/rfc3986#section-2.3
-- # unreserved  = ALPHA / DIGIT / "-" / "." / "_" / "~"

-- | @unreserved@
isUnreserved :: Word8 -> Bool
isUnreserved w = isALPHA w || isDIGIT w || B.elem w "-._~"

-- = https://www.rfc-editor.org/rfc/rfc3986#section-2.2
-- # sub-delims  = "!" / "$" / "&" / "'" / "(" / ")"
-- # / "*" / "+" / "," / ";" / "="

-- | @sub-delims@
isSubDelim :: Word8 -> Bool
isSubDelim w = B.elem w "!$&'()*+,;="

-- = https://www.rfc-editor.org/rfc/rfc3986#section-3.3
-- # pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

-- | @pchar@, except for @pct-encoded@, which spans three octets and so
-- has to be checked separately.
isPChar :: Word8 -> Bool
isPChar w = isUnreserved w || isSubDelim w || w == 0x3A || w == 0x40
