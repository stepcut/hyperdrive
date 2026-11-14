{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | Request methods (RFC 9110, Section 9)
module Hyperdrive.Semantics.Method
  ( Method
  , mkMethod
  , methodBytes
  , isSafe
  , isIdempotent
    -- * Standard methods
  , pattern GET
  , pattern HEAD
  , pattern POST
  , pattern PUT
  , pattern DELETE
  , pattern CONNECT
  , pattern OPTIONS
  , pattern TRACE
  , pattern PATCH
  ) where

import Data.ByteString (ByteString)
import Hyperdrive.Semantics.Internal (Method(..))
import Hyperdrive.Semantics.Syntax (isToken)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.1
-- # method = token
-- # The method token is case-sensitive

-- | validate a method token. Methods are case-sensitive, so @get@ is
-- a valid method, but it is not 'GET'.
mkMethod :: ByteString -> Maybe Method
mkMethod bs
  | isToken bs = Just (Method bs)
  | otherwise  = Nothing

methodBytes :: Method -> ByteString
methodBytes (Method bs) = bs

pattern GET, HEAD, POST, PUT, DELETE, CONNECT, OPTIONS, TRACE :: Method
pattern GET     = Method "GET"
pattern HEAD    = Method "HEAD"
pattern POST    = Method "POST"
pattern PUT     = Method "PUT"
pattern DELETE  = Method "DELETE"
pattern CONNECT = Method "CONNECT"
pattern OPTIONS = Method "OPTIONS"
pattern TRACE   = Method "TRACE"

-- | defined by RFC 5789
pattern PATCH :: Method
pattern PATCH = Method "PATCH"

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.2.1
-- # Of the request methods defined by this specification, the GET, HEAD,
-- # OPTIONS, and TRACE methods are defined to be safe.

-- | Is this a safe method? Methods we do not know about are assumed
-- to be unsafe.
isSafe :: Method -> Bool
isSafe m = m `elem` [GET, HEAD, OPTIONS, TRACE]

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.2.2
-- # Of the request methods
-- # defined by this specification, PUT, DELETE, and safe request methods
-- # are idempotent.

-- | Is this an idempotent method? Methods we do not know about are
-- assumed not to be.
isIdempotent :: Method -> Bool
isIdempotent m = isSafe m || m == PUT || m == DELETE
