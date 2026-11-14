{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | The target of a request (RFC 9110, Section 7.1)
--
-- Each protocol version reconstructs the target from different parts
-- of the message: HTTP/1.1 from the request-target and @Host@, HTTP/2
-- and HTTP/3 from the @:scheme@, @:authority@ and @:path@ pseudo-header
-- fields. This module is what they reconstruct it into.
module Hyperdrive.Semantics.Target
  ( Target(..)
  , connectTarget
  , validForMethod
  , normalizeTarget
  ) where

import Data.Word (Word16)
import Hyperdrive.Semantics.Internal (Path(..))
import Hyperdrive.Semantics.Method (Method, pattern CONNECT, pattern OPTIONS)
import Hyperdrive.Semantics.URI

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.1
-- # Upon receipt of a client's request, a server reconstructs the target
-- # URI from the received components in accordance with their local
-- # configuration and incoming connection context.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.1
-- # There are two unusual cases for which the request target components
-- # are in a method-specific form:
data Target
  = -- | an ordinary target URI. The fragment, if any, is never part of
    -- the target.
    TargetURI !Scheme !Authority !Path !(Maybe Query)
    -- | the tunnel destination of a CONNECT request
  | TargetConnect !Host !Word16
    -- | @*@: the server as a whole, rather than a resource. Only used
    -- with OPTIONS.
  | TargetAsterisk
  deriving (Eq, Ord, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.3.6
-- # CONNECT uses a special form of request target, unique to this method,
-- # consisting of only the host and port number of the tunnel
-- # destination, separated by a colon.

-- = https://www.rfc-editor.org/rfc/rfc9110#section-9.3.6
-- # A server MUST reject a CONNECT request that targets an empty or
-- # invalid port number, typically by responding with a 400 (Bad Request)
-- # status code.

-- | The target of a CONNECT request. There is no default port, so the
-- port must be present, and port 0 cannot be connected to.
connectTarget :: Authority -> Maybe Target
connectTarget (Authority h (Just p)) | p /= 0 = Just (TargetConnect h p)
connectTarget _                               = Nothing

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.1
-- # These forms MUST
-- # NOT be used with other methods.

-- | The CONNECT and asterisk forms may only be used with CONNECT and
-- OPTIONS respectively. The reverse does not hold: an extended CONNECT
-- (RFC 8441, RFC 9220) has an ordinary target URI.
validForMethod :: Method -> Target -> Bool
validForMethod m TargetConnect{} = m == CONNECT
validForMethod m TargetAsterisk  = m == OPTIONS
validForMethod _ TargetURI{}     = True

-- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.3
-- # When not being used as the target of an OPTIONS request, an empty
-- # path component is equivalent to an absolute path of "/", so the
-- # normal form is to provide a path of "/" instead.

-- | Give an empty path its normal form of "/", unless the method is
-- OPTIONS.
normalizeTarget :: Method -> Target -> Target
normalizeTarget m (TargetURI s a (Path "") q)
  | m /= OPTIONS = TargetURI s a (Path "/") q
normalizeTarget _ t = t
