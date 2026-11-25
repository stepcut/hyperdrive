{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | The target of a request (RFC 9110, Section 7.1)
--
-- Each protocol version reconstructs the target from different parts
-- of the message: HTTP/1.1 from the request-target and @Host@, HTTP/2
-- and HTTP/3 from the @:scheme@, @:authority@ and @:path@ pseudo-header
-- fields. This module is what they reconstruct it into.
module Hyperdrive.Semantics.Target
  ( Target(..)
  , TargetContext(..)
  , connectTarget
  , validForMethod
  , normalizeEmptyPath
  ) where

import Data.Word (Word16)
import Hyperdrive.Semantics.Internal (Path(..))
import Hyperdrive.Semantics.Method (Method, pattern CONNECT, pattern OPTIONS)
import Hyperdrive.Semantics.URI

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.3
-- # A
-- # server that can uniquely identify an authority from the request
-- # context MAY use that identity as a default without this risk.

-- | What the server knows about the connection that a target is
-- reconstructed with.
--
-- RFC 9112 Section 3.3 warns that a default authority is unsafe on a
-- secured connection unless the authority is certain, for example
-- the one name its certificate is valid for.
data TargetContext = TargetContext
  { contextScheme           :: !Scheme
    -- ^ the scheme of the target URI of requests in origin-form or
    -- asterisk-form: @https@ if the connection is secured (or a trusted
    -- gateway says it was), else @http@. A request in absolute-form for
    -- an @https@ target (or, in HTTP/2 and HTTP/3, with @:scheme@
    -- @https@) is accepted only if this is @https@.
  , contextDefaultAuthority :: !(Maybe Authority)
    -- ^ the authority to use when a request has no @Host@ (HTTP/1.0)
    -- or an empty one, or (HTTP/2, HTTP/3) neither @:authority@ nor
    -- @Host@. With 'Nothing', such requests are rejected.
  } deriving (Eq, Show)

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
--
-- This is the only part of the scheme-based normalization of RFC 9110
-- Section 4.2.3 that is done here. In particular a default port is
-- kept, so @http://h:80/@ and @http://h/@ are different 'Target's, and
-- percent-encodings in the path and query are kept as received.
normalizeEmptyPath :: Method -> Target -> Target
normalizeEmptyPath m (TargetURI s a (Path "") q)
  | m /= OPTIONS = TargetURI s a (Path "/") q
normalizeEmptyPath _ t = t
