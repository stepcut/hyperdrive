{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
-- | The collected ABNF of the RFCs that define HTTP/1.1 and the URIs it
-- uses, quoted from each RFC's Appendix A as published (only the page
-- breaks are removed). The tests check that each one equals the
-- appendix parsed from the text of the RFC.
--
-- The appendices of RFC 9110 and RFC 9112 write lists out in the form
-- senders must generate (RFC 9110, Section 5.6.1), so they have no @#@
-- rules.
module Hyperdrive.ABNF.HTTP
  ( httpLibrary
  , rfc9110
  , rfc9112
  , rfc3986
  ) where

import Hyperdrive.ABNF.Grammar (mkGrammar)
import Hyperdrive.ABNF.Library (Library, library)
import Hyperdrive.ABNF.QQ (abnf)
import Hyperdrive.ABNF.Types (RuleList)

-- | The three grammars, tagged as the RFCs cite each other: @HTTP@ for
-- RFC 9110, @HTTP/1.1@ for RFC 9112 and @URI@ for RFC 3986. Rules that
-- RFC 9110 imports from RFC 4647, 5646 and 5322 (@language-range@,
-- @language-tag@ and @mailbox@) cannot be resolved.
httpLibrary :: Library
httpLibrary = library
  [ ("HTTP", grammar rfc9110), ("HTTP/1.1", grammar rfc9112), ("URI", grammar rfc3986) ]
  where grammar = either (error . ("Hyperdrive.ABNF.HTTP: " ++) . show) id . mkGrammar

-- | The collected ABNF of RFC 9110 (HTTP Semantics), Appendix A.
rfc9110 :: RuleList
rfc9110 = [abnf|
  Accept = [ ( media-range [ weight ] ) *( OWS "," OWS ( media-range [
   weight ] ) ) ]
  Accept-Charset = [ ( ( token / "*" ) [ weight ] ) *( OWS "," OWS ( (
   token / "*" ) [ weight ] ) ) ]
  Accept-Encoding = [ ( codings [ weight ] ) *( OWS "," OWS ( codings [
   weight ] ) ) ]
  Accept-Language = [ ( language-range [ weight ] ) *( OWS "," OWS (
   language-range [ weight ] ) ) ]
  Accept-Ranges = acceptable-ranges
  Allow = [ method *( OWS "," OWS method ) ]
  Authentication-Info = [ auth-param *( OWS "," OWS auth-param ) ]
  Authorization = credentials

  BWS = OWS

  Connection = [ connection-option *( OWS "," OWS connection-option )
   ]
  Content-Encoding = [ content-coding *( OWS "," OWS content-coding )
   ]
  Content-Language = [ language-tag *( OWS "," OWS language-tag ) ]
  Content-Length = 1*DIGIT
  Content-Location = absolute-URI / partial-URI
  Content-Range = range-unit SP ( range-resp / unsatisfied-range )
  Content-Type = media-type

  Date = HTTP-date

  ETag = entity-tag
  Expect = [ expectation *( OWS "," OWS expectation ) ]

  From = mailbox

  GMT = %x47.4D.54 ; GMT

  HTTP-date = IMF-fixdate / obs-date
  Host = uri-host [ ":" port ]

  IMF-fixdate = day-name "," SP date1 SP time-of-day SP GMT
  If-Match = "*" / [ entity-tag *( OWS "," OWS entity-tag ) ]
  If-Modified-Since = HTTP-date
  If-None-Match = "*" / [ entity-tag *( OWS "," OWS entity-tag ) ]
  If-Range = entity-tag / HTTP-date
  If-Unmodified-Since = HTTP-date

  Last-Modified = HTTP-date
  Location = URI-reference

  Max-Forwards = 1*DIGIT

  OWS = *( SP / HTAB )

  Proxy-Authenticate = [ challenge *( OWS "," OWS challenge ) ]
  Proxy-Authentication-Info = [ auth-param *( OWS "," OWS auth-param )
   ]
  Proxy-Authorization = credentials

  RWS = 1*( SP / HTAB )
  Range = ranges-specifier
  Referer = absolute-URI / partial-URI
  Retry-After = HTTP-date / delay-seconds

  Server = product *( RWS ( product / comment ) )

  TE = [ t-codings *( OWS "," OWS t-codings ) ]
  Trailer = [ field-name *( OWS "," OWS field-name ) ]

  URI-reference = <URI-reference, see [URI], Section 4.1>
  Upgrade = [ protocol *( OWS "," OWS protocol ) ]
  User-Agent = product *( RWS ( product / comment ) )

  Vary = [ ( "*" / field-name ) *( OWS "," OWS ( "*" / field-name ) )
   ]
  Via = [ ( received-protocol RWS received-by [ RWS comment ] ) *( OWS
   "," OWS ( received-protocol RWS received-by [ RWS comment ] ) ) ]

  WWW-Authenticate = [ challenge *( OWS "," OWS challenge ) ]

  absolute-URI = <absolute-URI, see [URI], Section 4.3>
  absolute-path = 1*( "/" segment )
  acceptable-ranges = range-unit *( OWS "," OWS range-unit )
  asctime-date = day-name SP date3 SP time-of-day SP year
  auth-param = token BWS "=" BWS ( token / quoted-string )
  auth-scheme = token
  authority = <authority, see [URI], Section 3.2>

  challenge = auth-scheme [ 1*SP ( token68 / [ auth-param *( OWS ","
   OWS auth-param ) ] ) ]
  codings = content-coding / "identity" / "*"
  comment = "(" *( ctext / quoted-pair / comment ) ")"
  complete-length = 1*DIGIT
  connection-option = token
  content-coding = token
  credentials = auth-scheme [ 1*SP ( token68 / [ auth-param *( OWS ","
   OWS auth-param ) ] ) ]
  ctext = HTAB / SP / %x21-27 ; '!'-'''
   / %x2A-5B ; '*'-'['
   / %x5D-7E ; ']'-'~'
   / obs-text

  date1 = day SP month SP year
  date2 = day "-" month "-" 2DIGIT
  date3 = month SP ( 2DIGIT / ( SP DIGIT ) )
  day = 2DIGIT
  day-name = %x4D.6F.6E ; Mon
   / %x54.75.65 ; Tue
   / %x57.65.64 ; Wed
   / %x54.68.75 ; Thu
   / %x46.72.69 ; Fri
   / %x53.61.74 ; Sat
   / %x53.75.6E ; Sun
  day-name-l = %x4D.6F.6E.64.61.79 ; Monday
   / %x54.75.65.73.64.61.79 ; Tuesday
   / %x57.65.64.6E.65.73.64.61.79 ; Wednesday
   / %x54.68.75.72.73.64.61.79 ; Thursday
   / %x46.72.69.64.61.79 ; Friday
   / %x53.61.74.75.72.64.61.79 ; Saturday
   / %x53.75.6E.64.61.79 ; Sunday
  delay-seconds = 1*DIGIT

  entity-tag = [ weak ] opaque-tag
  etagc = "!" / %x23-7E ; '#'-'~'
   / obs-text
  expectation = token [ "=" ( token / quoted-string ) parameters ]

  field-content = field-vchar [ 1*( SP / HTAB / field-vchar )
   field-vchar ]
  field-name = token
  field-value = *field-content
  field-vchar = VCHAR / obs-text
  first-pos = 1*DIGIT

  hour = 2DIGIT
  http-URI = "http://" authority path-abempty [ "?" query ]
  https-URI = "https://" authority path-abempty [ "?" query ]

  incl-range = first-pos "-" last-pos
  int-range = first-pos "-" [ last-pos ]

  language-range = <language-range, see [RFC4647], Section 2.1>
  language-tag = <Language-Tag, see [RFC5646], Section 2.1>
  last-pos = 1*DIGIT

  mailbox = <mailbox, see [RFC5322], Section 3.4>
  media-range = ( "*/*" / ( type "/*" ) / ( type "/" subtype ) )
   parameters
  media-type = type "/" subtype parameters
  method = token
  minute = 2DIGIT
  month = %x4A.61.6E ; Jan
   / %x46.65.62 ; Feb
   / %x4D.61.72 ; Mar
   / %x41.70.72 ; Apr
   / %x4D.61.79 ; May
   / %x4A.75.6E ; Jun
   / %x4A.75.6C ; Jul
   / %x41.75.67 ; Aug
   / %x53.65.70 ; Sep
   / %x4F.63.74 ; Oct
   / %x4E.6F.76 ; Nov
   / %x44.65.63 ; Dec

  obs-date = rfc850-date / asctime-date
  obs-text = %x80-FF
  opaque-tag = DQUOTE *etagc DQUOTE
  other-range = 1*( %x21-2B ; '!'-'+'
   / %x2D-7E ; '-'-'~'
   )

  parameter = parameter-name "=" parameter-value
  parameter-name = token
  parameter-value = ( token / quoted-string )
  parameters = *( OWS ";" OWS [ parameter ] )
  partial-URI = relative-part [ "?" query ]
  path-abempty = <path-abempty, see [URI], Section 3.3>
  port = <port, see [URI], Section 3.2.3>
  product = token [ "/" product-version ]
  product-version = token
  protocol = protocol-name [ "/" protocol-version ]
  protocol-name = token
  protocol-version = token
  pseudonym = token

  qdtext = HTAB / SP / "!" / %x23-5B ; '#'-'['
   / %x5D-7E ; ']'-'~'
   / obs-text
  query = <query, see [URI], Section 3.4>
  quoted-pair = "\" ( HTAB / SP / VCHAR / obs-text )
  quoted-string = DQUOTE *( qdtext / quoted-pair ) DQUOTE
  qvalue = ( "0" [ "." *3DIGIT ] ) / ( "1" [ "." *3"0" ] )

  range-resp = incl-range "/" ( complete-length / "*" )
  range-set = range-spec *( OWS "," OWS range-spec )
  range-spec = int-range / suffix-range / other-range
  range-unit = token
  ranges-specifier = range-unit "=" range-set
  received-by = pseudonym [ ":" port ]
  received-protocol = [ protocol-name "/" ] protocol-version
  relative-part = <relative-part, see [URI], Section 4.2>
  rfc850-date = day-name-l "," SP date2 SP time-of-day SP GMT

  second = 2DIGIT
  segment = <segment, see [URI], Section 3.3>
  subtype = token
  suffix-length = 1*DIGIT
  suffix-range = "-" suffix-length

  t-codings = "trailers" / ( transfer-coding [ weight ] )
  tchar = "!" / "#" / "$" / "%" / "&" / "'" / "*" / "+" / "-" / "." /
   "^" / "_" / "`" / "|" / "~" / DIGIT / ALPHA
  time-of-day = hour ":" minute ":" second
  token = 1*tchar
  token68 = 1*( ALPHA / DIGIT / "-" / "." / "_" / "~" / "+" / "/" )
   *"="
  transfer-coding = token *( OWS ";" OWS transfer-parameter )
  transfer-parameter = token BWS "=" BWS ( token / quoted-string )
  type = token

  unsatisfied-range = "*/" complete-length
  uri-host = <host, see [URI], Section 3.2.2>

  weak = %x57.2F ; W/
  weight = OWS ";" OWS "q=" qvalue

  year = 4DIGIT
  |]

-- | The collected ABNF of RFC 9112 (HTTP/1.1), Appendix A.
rfc9112 :: RuleList
rfc9112 = [abnf|
  BWS = <BWS, see [HTTP], Section 5.6.3>

  HTTP-message = start-line CRLF *( field-line CRLF ) CRLF [
   message-body ]
  HTTP-name = %x48.54.54.50 ; HTTP
  HTTP-version = HTTP-name "/" DIGIT "." DIGIT

  OWS = <OWS, see [HTTP], Section 5.6.3>

  RWS = <RWS, see [HTTP], Section 5.6.3>

  Transfer-Encoding = [ transfer-coding *( OWS "," OWS transfer-coding
   ) ]

  absolute-URI = <absolute-URI, see [URI], Section 4.3>
  absolute-form = absolute-URI
  absolute-path = <absolute-path, see [HTTP], Section 4.1>
  asterisk-form = "*"
  authority = <authority, see [URI], Section 3.2>
  authority-form = uri-host ":" port

  chunk = chunk-size [ chunk-ext ] CRLF chunk-data CRLF
  chunk-data = 1*OCTET
  chunk-ext = *( BWS ";" BWS chunk-ext-name [ BWS "=" BWS chunk-ext-val
   ] )
  chunk-ext-name = token
  chunk-ext-val = token / quoted-string
  chunk-size = 1*HEXDIG
  chunked-body = *chunk last-chunk trailer-section CRLF

  field-line = field-name ":" OWS field-value OWS
  field-name = <field-name, see [HTTP], Section 5.1>
  field-value = <field-value, see [HTTP], Section 5.5>

  last-chunk = 1*"0" [ chunk-ext ] CRLF

  message-body = *OCTET
  method = token

  obs-fold = OWS CRLF RWS
  obs-text = <obs-text, see [HTTP], Section 5.6.4>
  origin-form = absolute-path [ "?" query ]

  port = <port, see [URI], Section 3.2.3>

  query = <query, see [URI], Section 3.4>
  quoted-string = <quoted-string, see [HTTP], Section 5.6.4>

  reason-phrase = 1*( HTAB / SP / VCHAR / obs-text )
  request-line = method SP request-target SP HTTP-version
  request-target = origin-form / absolute-form / authority-form /
   asterisk-form

  start-line = request-line / status-line
  status-code = 3DIGIT
  status-line = HTTP-version SP status-code SP [ reason-phrase ]

  token = <token, see [HTTP], Section 5.6.2>
  trailer-section = *( field-line CRLF )
  transfer-coding = <transfer-coding, see [HTTP], Section 10.1.4>

  uri-host = <host, see [URI], Section 3.2.2>
  |]

-- | The collected ABNF of RFC 3986 (URI Generic Syntax), Appendix A.
rfc3986 :: RuleList
rfc3986 = [abnf|
  URI           = scheme ":" hier-part [ "?" query ] [ "#" fragment ]

  hier-part     = "//" authority path-abempty
                / path-absolute
                / path-rootless
                / path-empty

  URI-reference = URI / relative-ref

  absolute-URI  = scheme ":" hier-part [ "?" query ]

  relative-ref  = relative-part [ "?" query ] [ "#" fragment ]

  relative-part = "//" authority path-abempty
                / path-absolute
                / path-noscheme
                / path-empty

  scheme        = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )

  authority     = [ userinfo "@" ] host [ ":" port ]
  userinfo      = *( unreserved / pct-encoded / sub-delims / ":" )
  host          = IP-literal / IPv4address / reg-name
  port          = *DIGIT

  IP-literal    = "[" ( IPv6address / IPvFuture  ) "]"

  IPvFuture     = "v" 1*HEXDIG "." 1*( unreserved / sub-delims / ":" )

  IPv6address   =                            6( h16 ":" ) ls32
                /                       "::" 5( h16 ":" ) ls32
                / [               h16 ] "::" 4( h16 ":" ) ls32
                / [ *1( h16 ":" ) h16 ] "::" 3( h16 ":" ) ls32
                / [ *2( h16 ":" ) h16 ] "::" 2( h16 ":" ) ls32
                / [ *3( h16 ":" ) h16 ] "::"    h16 ":"   ls32
                / [ *4( h16 ":" ) h16 ] "::"              ls32
                / [ *5( h16 ":" ) h16 ] "::"              h16
                / [ *6( h16 ":" ) h16 ] "::"

  h16           = 1*4HEXDIG
  ls32          = ( h16 ":" h16 ) / IPv4address
  IPv4address   = dec-octet "." dec-octet "." dec-octet "." dec-octet
  dec-octet     = DIGIT                 ; 0-9
                / %x31-39 DIGIT         ; 10-99
                / "1" 2DIGIT            ; 100-199
                / "2" %x30-34 DIGIT     ; 200-249
                / "25" %x30-35          ; 250-255

  reg-name      = *( unreserved / pct-encoded / sub-delims )

  path          = path-abempty    ; begins with "/" or is empty
                / path-absolute   ; begins with "/" but not "//"
                / path-noscheme   ; begins with a non-colon segment
                / path-rootless   ; begins with a segment
                / path-empty      ; zero characters

  path-abempty  = *( "/" segment )
  path-absolute = "/" [ segment-nz *( "/" segment ) ]
  path-noscheme = segment-nz-nc *( "/" segment )
  path-rootless = segment-nz *( "/" segment )
  path-empty    = 0<pchar>

  segment       = *pchar
  segment-nz    = 1*pchar
  segment-nz-nc = 1*( unreserved / pct-encoded / sub-delims / "@" )
                ; non-zero-length segment without any colon ":"

  pchar         = unreserved / pct-encoded / sub-delims / ":" / "@"

  query         = *( pchar / "/" / "?" )

  fragment      = *( pchar / "/" / "?" )

  pct-encoded   = "%" HEXDIG HEXDIG

  unreserved    = ALPHA / DIGIT / "-" / "." / "_" / "~"
  reserved      = gen-delims / sub-delims
  gen-delims    = ":" / "/" / "?" / "#" / "[" / "]" / "@"
  sub-delims    = "!" / "$" / "&" / "'" / "(" / ")"
                / "*" / "+" / "," / ";" / "="
  |]

