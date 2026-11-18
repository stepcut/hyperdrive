{-# LANGUAGE OverloadedStrings, PatternSynonyms #-}
-- | Reconstructing the target of an HTTP/1.1 request from its
-- request-target and @Host@ header field (RFC 9112, Sections 3.2 and
-- 3.3; RFC 9110, Section 7.1).
--
-- The form of the request-target is decided by the method: only
-- CONNECT uses the authority-form, and CONNECT uses nothing else. This
-- matters because a string like @example.com:443@ is both an
-- authority-form and an absolute-URI with the scheme @example.com@.
module Hyperdrive.HTTP1.Target
  ( TargetContext(..)
  , TargetError(..)
  , reconstructTarget
  , targetErrorStatus
  ) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Hyperdrive.Semantics.Field (Fields, fieldValueBytes, fieldsFromList, fieldsToList, hHost, lookupAll)
import Hyperdrive.Semantics.Message (HttpVersion, pattern HTTP_1_1)
import Hyperdrive.Semantics.Method (Method, pattern CONNECT, pattern OPTIONS)
import Hyperdrive.Semantics.Status (Status, pattern BadRequest, pattern MisdirectedRequest)
import Hyperdrive.Semantics.Target (Target(..), connectTarget)
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
    -- an @https@ target is accepted only if this is @https@.
  , contextDefaultAuthority :: !(Maybe Authority)
    -- ^ the authority to use when a request has no @Host@ (HTTP/1.0)
    -- or an empty one. With 'Nothing', such requests are rejected.
  } deriving (Eq, Show)

-- | Why a request's target cannot be reconstructed. Each is answered
-- with 'targetErrorStatus', and the connection is closed.
data TargetError
  = InvalidTarget                        -- ^ not a request-target this server accepts
  | TargetFormNotForMethod               -- ^ the authority-form without CONNECT, CONNECT without
                                         -- it, or the asterisk-form without OPTIONS
  | InvalidTargetAuthority !AuthorityError -- ^ the authority of the target is invalid
  | MissingConnectPort                   -- ^ a CONNECT target without a port, or with port 0
  | UnsupportedIPvFuture                 -- ^ an IPvFuture host, which this server cannot serve
  | MisdirectedScheme !Scheme            -- ^ an absolute-form target with a scheme this
                                         -- connection does not serve
  | MissingHost                          -- ^ an HTTP/1.1 request without @Host@
  | RepeatedHost                         -- ^ more than one @Host@ field line
  | InvalidHost !AuthorityError          -- ^ a @Host@ field value that is not @uri-host [ ":" port ]@
  | NoAuthority                          -- ^ no authority from @Host@, and no default
  deriving (Eq, Show)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.4
-- # The 421 (Misdirected Request) status code in a response indicates
-- # that the origin server has rejected the request because it appears to
-- # have been misdirected (Section 15.5.20).

-- | 421 (Misdirected Request) for a scheme this connection does not
-- serve, 400 (Bad Request) for everything else.
targetErrorStatus :: TargetError -> Status
targetErrorStatus MisdirectedScheme{} = MisdirectedRequest
targetErrorStatus _                   = BadRequest

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2
-- # request-target = origin-form
-- # / absolute-form
-- # / authority-form
-- # / asterisk-form

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2
-- # A server MUST respond with a 400 (Bad Request) status code to any
-- # HTTP/1.1 request message that lacks a Host header field and to any
-- # request message that contains more than one Host header field line or
-- # a Host header field with an invalid field value.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2.2
-- # When an origin server receives a request with an absolute-form of
-- # request-target, the origin server MUST ignore the received Host
-- # header field (if any) and instead use the host information of the
-- # request-target.

-- | The target of a request, given its method and request-target, and
-- the header fields without @Host@ (which 'Hyperdrive.Semantics.Message.mkRequest'
-- requires: the authority is in the target and nowhere else).
--
-- @Host@ is checked in every request, as RFC 9112 Section 3.2
-- requires, but its value is only used for the origin-form and the
-- asterisk-form: with the absolute-form the target's own authority is
-- used, whatever @Host@ says, and with the authority-form the target is
-- the authority. The asterisk-form has no authority in its 'Target'.
reconstructTarget :: TargetContext -> HttpVersion -> Method -> ByteString -> Fields
                  -> Either TargetError (Target, Fields)
reconstructTarget ctx version method raw fields = do
  host <- hostField
  target <- if method == CONNECT then authorityForm else otherForms host
  Right (target, fieldsFromList [ f | f@(n, _) <- fieldsToList fields, n /= hHost ])
  where
    -- Nothing when there is no Host (allowed before HTTP/1.1) or it is empty
    hostField = case lookupAll hHost fields of
      [] | version >= HTTP_1_1 -> Left MissingHost
         | otherwise           -> Right Nothing
      [v] | B.null (fieldValueBytes v) -> Right Nothing
          | otherwise -> Just <$> (either (Left . InvalidHost) Right (parseAuthority (fieldValueBytes v)) >>= servable)
      _ -> Left RepeatedHost

    authorityForm
      | raw == "*" || B.take 1 raw == "/" = Left TargetFormNotForMethod
      | otherwise = do
          a <- either (Left . InvalidTargetAuthority) Right (parseAuthority raw) >>= servable
          maybe (Left MissingConnectPort) Right (connectTarget a)

    otherForms host
      | raw == "*" = if method == OPTIONS then Right TargetAsterisk else Left TargetFormNotForMethod
      | B.take 1 raw == "/" = do
          authority <- maybe (maybe (Left NoAuthority) Right (contextDefaultAuthority ctx)) Right host
          (path, query) <- pathAndQuery raw
          Right (TargetURI (contextScheme ctx) authority path query)
      | otherwise = absoluteForm

    -- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.1
    -- # http-URI = "http" "://" authority path-abempty [ "?" query ]

    -- = https://www.rfc-editor.org/rfc/rfc9110#section-4.2.2
    -- # https-URI = "https" "://" authority path-abempty [ "?" query ]
    absoluteForm = do
      let (s, rest) = B.break (== 0x3A) raw
      scheme <- maybe (Left InvalidTarget) Right (mkScheme s)
      if B.null rest then Left InvalidTarget else Right ()
      if scheme /= HTTP && scheme /= HTTPS then Left (MisdirectedScheme scheme) else Right ()
      if scheme == HTTPS && contextScheme ctx /= HTTPS then Left (MisdirectedScheme scheme) else Right ()
      hier <- maybe (Left InvalidTarget) Right (B.stripPrefix "://" rest)
      let (auth, pathQuery) = B.break (\w -> w == 0x2F || w == 0x3F) hier
      authority <- either (Left . InvalidTargetAuthority) Right (parseAuthority auth) >>= servable
      (path, query) <- pathAndQuery pathQuery
      Right (TargetURI scheme authority path query)

    pathAndQuery s = do
      let (p, q) = B.break (== 0x3F) s
      path <- maybe (Left InvalidTarget) Right (mkPath p)
      query <- if B.null q then Right Nothing else maybe (Left InvalidTarget) (Right . Just) (mkQuery (B.drop 1 q))
      Right (path, query)

    -- IPvFuture names no address mechanism this server supports
    -- (docs/design-notes.md, P3)
    servable a = case authorityHost a of
      HostIPvFuture _ -> Left UnsupportedIPvFuture
      _               -> Right a

-- = https://www.rfc-editor.org/rfc/rfc9110#section-7.4
-- # In particular, a request for an "https"
-- # resource MUST be rejected unless it has been received over a
-- # connection that has been secured via a certificate valid for that
-- # target URI's origin, as defined by Section 4.2.2.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2.2
-- # A server MUST accept the absolute-form in requests even though most
-- # HTTP/1.1 clients will only send the absolute-form to a proxy.

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2.3
-- # authority-form = uri-host ":" port

-- = https://www.rfc-editor.org/rfc/rfc9112#section-3.2.4
-- # asterisk-form  = "*"
