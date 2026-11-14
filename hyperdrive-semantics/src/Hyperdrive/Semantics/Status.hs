{-# LANGUAGE PatternSynonyms #-}
-- | Status codes (RFC 9110, Section 15)
module Hyperdrive.Semantics.Status
  ( Status
  , mkStatus
  , statusCode
  , StatusClass(..)
  , statusClass
    -- * Status codes defined by RFC 9110
  , pattern Continue
  , pattern SwitchingProtocols
  , pattern OK
  , pattern Created
  , pattern Accepted
  , pattern NonAuthoritativeInformation
  , pattern NoContent
  , pattern ResetContent
  , pattern PartialContent
  , pattern MultipleChoices
  , pattern MovedPermanently
  , pattern Found
  , pattern SeeOther
  , pattern NotModified
  , pattern UseProxy
  , pattern TemporaryRedirect
  , pattern PermanentRedirect
  , pattern BadRequest
  , pattern Unauthorized
  , pattern PaymentRequired
  , pattern Forbidden
  , pattern NotFound
  , pattern MethodNotAllowed
  , pattern NotAcceptable
  , pattern ProxyAuthenticationRequired
  , pattern RequestTimeout
  , pattern Conflict
  , pattern Gone
  , pattern LengthRequired
  , pattern PreconditionFailed
  , pattern ContentTooLarge
  , pattern URITooLong
  , pattern UnsupportedMediaType
  , pattern RangeNotSatisfiable
  , pattern ExpectationFailed
  , pattern MisdirectedRequest
  , pattern UnprocessableContent
  , pattern UpgradeRequired
  , pattern InternalServerError
  , pattern NotImplemented
  , pattern BadGateway
  , pattern ServiceUnavailable
  , pattern GatewayTimeout
  , pattern HTTPVersionNotSupported
  ) where

import Hyperdrive.Semantics.Internal (Status(..))

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15
-- # The status code of a response is a three-digit integer code

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15
-- # All valid status codes are within the
-- # range of 100 to 599, inclusive.

-- | Validate a status code. Codes outside @100..599@ are invalid.
mkStatus :: Int -> Maybe Status
mkStatus c
  | c >= 100 && c <= 599 = Just (Status c)
  | otherwise            = Nothing

statusCode :: Status -> Int
statusCode (Status c) = c

data StatusClass
  = Informational -- ^ 1xx
  | Successful    -- ^ 2xx
  | Redirection   -- ^ 3xx
  | ClientError   -- ^ 4xx
  | ServerError   -- ^ 5xx
  deriving (Eq, Ord, Show, Enum, Bounded)

-- = https://www.rfc-editor.org/rfc/rfc9110#section-15
-- # The first digit of the status code defines the class of response.

statusClass :: Status -> StatusClass
statusClass (Status c) =
  case c `quot` 100 of
    1 -> Informational
    2 -> Successful
    3 -> Redirection
    4 -> ClientError
    _ -> ServerError -- the Status invariant guarantees this is 5

pattern Continue, SwitchingProtocols :: Status
pattern Continue           = Status 100
pattern SwitchingProtocols = Status 101

pattern OK, Created, Accepted, NonAuthoritativeInformation, NoContent, ResetContent, PartialContent :: Status
pattern OK                          = Status 200
pattern Created                     = Status 201
pattern Accepted                    = Status 202
pattern NonAuthoritativeInformation = Status 203
pattern NoContent                   = Status 204
pattern ResetContent                = Status 205
pattern PartialContent              = Status 206

pattern MultipleChoices, MovedPermanently, Found, SeeOther, NotModified, UseProxy, TemporaryRedirect, PermanentRedirect :: Status
pattern MultipleChoices   = Status 300
pattern MovedPermanently  = Status 301
pattern Found             = Status 302
pattern SeeOther          = Status 303
pattern NotModified       = Status 304
pattern UseProxy          = Status 305
pattern TemporaryRedirect = Status 307
pattern PermanentRedirect = Status 308

pattern BadRequest, Unauthorized, PaymentRequired, Forbidden, NotFound, MethodNotAllowed, NotAcceptable, ProxyAuthenticationRequired, RequestTimeout, Conflict, Gone, LengthRequired, PreconditionFailed, ContentTooLarge, URITooLong, UnsupportedMediaType, RangeNotSatisfiable, ExpectationFailed, MisdirectedRequest, UnprocessableContent, UpgradeRequired :: Status
pattern BadRequest                  = Status 400
pattern Unauthorized                = Status 401
pattern PaymentRequired             = Status 402
pattern Forbidden                   = Status 403
pattern NotFound                    = Status 404
pattern MethodNotAllowed            = Status 405
pattern NotAcceptable               = Status 406
pattern ProxyAuthenticationRequired = Status 407
pattern RequestTimeout              = Status 408
pattern Conflict                    = Status 409
pattern Gone                        = Status 410
pattern LengthRequired              = Status 411
pattern PreconditionFailed          = Status 412
pattern ContentTooLarge             = Status 413
pattern URITooLong                  = Status 414
pattern UnsupportedMediaType        = Status 415
pattern RangeNotSatisfiable         = Status 416
pattern ExpectationFailed           = Status 417
pattern MisdirectedRequest          = Status 421
pattern UnprocessableContent        = Status 422
pattern UpgradeRequired             = Status 426

pattern InternalServerError, NotImplemented, BadGateway, ServiceUnavailable, GatewayTimeout, HTTPVersionNotSupported :: Status
pattern InternalServerError     = Status 500
pattern NotImplemented          = Status 501
pattern BadGateway              = Status 502
pattern ServiceUnavailable      = Status 503
pattern GatewayTimeout          = Status 504
pattern HTTPVersionNotSupported = Status 505
