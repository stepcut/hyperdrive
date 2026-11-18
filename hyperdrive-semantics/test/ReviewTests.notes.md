# Notes from the second independent review

Written on 2026-11-17 by a reviewer (a separate Claude Opus 5.5 agent) that
saw only the API (`scripts/extract-api`: export lists, signatures and
documentation, no implementation, tests or design notes) and the texts of
RFC 9110, 9112, 3986, 5952, 1123 and 952. It could type-check its tests
against the compiled library but not run them. It concentrated on what
had been added since the first review (`IndependentTests.notes.md`):
`Message`, `FieldSyntax`, duplicate fields, and host comparison and
rendering. Tests: `ReviewTests.hs`; 4 of 947 failed when first run.

What was done about each point:

| Point | Outcome |
|---|---|
| C1, A1: 205 has content of zero length | Fixed: a 205 takes `ContentBytes ""`, and `ContentNone` is rejected. |
| C2, gap 1: 426 cannot carry Upgrade | Decided: `mkResponse` rejects 426 (`UpgradeNotSupported`) until upgrades are supported. Test changed. |
| C3: entity-tag lists | Fixed: `splitEntityTags` added, and `splitList` says it is not for them. Test changed to use it. |
| C4, A4: canonicalHost and authorityKey | Documentation: a policy for routing and allow-lists, not URI or origin equivalence. |
| C5, A3: RFC 5952 Section 5 | Documentation: `renderHost` says which prefixes get mixed notation and why. |
| C6, A5: trailers | Changed: a stream declares its trailer fields, an allow-list checked by `mkResponse` (and the source of the `Trailer` field); `checkTrailers` takes the response. |
| C7: citations | Fixed (`readTrailers`, `isNumericLookalike`). |
| A6: "alphabetic" top-level labels | Documented why it is not applied literally: internationalized TLDs (`xn--p1ai`). |
| A7, A8, A10 | No change: the readings the API takes are the ones recommended here. A10: RFC 6874 is now obsolete (RFC 9844), see design-notes.md. |
| A9: lookupParameter | Documented that only the name asked for is checked. |
| A11: CONNECT with an ordinary target | Fixed: rejected for HTTP/1.x, where only the authority form exists. A reviewer test built an extended CONNECT as HTTP/1.1 and now uses HTTP/2. |
| Gap 2: repeated singletons in trailers | Fixed: within the trailers (`checkTrailers`) and across headers and trailers (`mkResponse`). |
| Gaps 3 to 11 | Recorded as requirements on the protocol layer, docs/design-notes.md M2, M4 to M7 and M10 to M12. |
| Gap 12: IDNA | No change: hosts must be DNS host names (P10), which rules out encoded names. |

The reviewer's notes follow, unchanged.

---

# Review notes: hyperdrive-semantics (Message, Field, FieldSyntax, host comparison)

Sources: API.md, RFC 9110, 9112, 3986, 5952, 1123, 952, plus RFC 1035 and
RFC 4291 fetched from rfc-editor.org. I did not look at the implementation
or run anything. `ReviewTests.hs` only type-checks (`./check.sh`). Tests
marked POLICY rest on a documented choice the RFCs allow but do not require.

## Conflicts

### C1. 205 Reset Content is treated as a status with no content

API: `ContentNotAllowed -- content with a 204, 205 or 304 status`. The
`Response` invariant says "there is no content exactly when the status says
there cannot be", and `ContentNone` is "no content at all, as for 204".

RFC 9110 Section 6.4.1: "All 1xx (Informational), 204 (No Content), and 304
(Not Modified) responses do not include content. All other responses do
include content, although that content might be of zero length." Section
15.3.6: "a server MUST NOT generate content in a 205 response." RFC 9112
Section 6.3 leaves 205 out of rule 1 (HEAD, 1xx, 204, 304 end at the
header section). So a 205 with no framing falls under rule 8: "the message
body length is determined by the number of octets received prior to the
server closing the connection."

So a 205 has content, and that content must be empty. In HTTP/1.1 it has
to be framed, for example with `Content-Length: 0`. If the API makes
applications use `ContentNone` for 205 and the HTTP/1.1 layer then sends
the same framing as for 204 (none), clients will read until the connection
closes. Test: `mkResponse 205 [] (ContentBytes "")` should be accepted.
Rejecting non-empty content is correct.

### C2. A 426 response cannot carry the Upgrade field the RFC requires

API: `Upgrade` is a protocol field. It "never appear[s] in a ... Response",
and the protocol layer generates such fields "from the structure of the
message". `mkResponse` rejects it with `ResponseProtocolField`.

RFC 9110 Section 15.5.22: "The server MUST send an Upgrade header field in
a 426 response to indicate the required protocol(s)". Section 7.8 says the
same, and also: "A server MAY send an Upgrade header field in any other
response to advertise that it implements support for upgrading". Nothing
in a `Response` says which protocols to list, so a conforming 426 cannot
be built, and Upgrade cannot be advertised (for example `h2c`). One test
covers this.

### C3. `splitList` / `fieldListElements` get entity-tag lists wrong

API: "Split a list-based field value into its elements. Commas inside a
quoted-string ... do not separate elements. ... The elements themselves are
not validated beyond their quoting; that is up to the grammar of the
particular field."

RFC 9110 Section 8.8.3: "opaque-tag = DQUOTE *etagc DQUOTE; etagc = %x21 /
%x23-7E / obs-text". A backslash is an ordinary etagc there, not an
escape. The RFC adds: "Previously, opaque-tag was defined to be a
quoted-string ...; thus, some recipients might perform backslash
unescaping." For `If-None-Match: "a\", "b"` (13.1.2, `#entity-tag`) the
RFC gives two tags, `"a\"` and `"b"`. A quoted-string-aware splitter reads
`\"` as an escape and reports `UnterminatedQuotedString`. One test covers
this. The documentation should at least exclude `#entity-tag` fields
(If-Match, If-None-Match), or the function needs a mode that does not
treat backslash as an escape.

### C4. `canonicalHost` / `authorityKey` claim more equivalence than the RFCs give

API (`authorityKey`): "A key for comparing the authorities of two targets
with the same scheme: `http://example.com:80` and `http://Example.COM.`
have the same key." (`canonicalHost`): "Two spellings that the network
treats as the same are mapped to one ... for example when matching virtual
hosts or checking an allow-list". It maps `[::ffff:a.b.c.d]` to
`a.b.c.d` and strips one trailing dot.

The RFCs:
- RFC 9110 Section 4.2.3 says http(s) URIs "are normalized and compared
  according to the methods defined in Section 6 of [URI]". Neither RFC
  3986 Section 6 nor 9110 4.2.3 strips a trailing dot or maps IPv4-mapped
  IPv6 literals.
- RFC 3986 Section 3.2.2 says the trailing dot is significant: the
  rightmost label "may be followed by a single "." and should be if it is
  necessary to distinguish between the complete domain name and some
  local domain".
- RFC 9110 Section 4.3.1: "Two origins are distinct if they differ in
  scheme, host, or port."
- For https, RFC 9110 Section 4.3.5 says an IPv4 IP-ID is 4 octets and an
  IPv6 IP-ID is 16. A certificate for `127.0.0.1` therefore does not
  authenticate `[::ffff:127.0.0.1]`, so the two are not the same authority
  for Section 7.4 purposes.

The documentation does say "This is not a normal form for URIs", but
`authorityKey` is presented as the way to compare target authorities. "Which
is what a dual-stack socket connects to" describes OS behaviour, not an
RFC (it fails with `IPV6_V6ONLY`). These are reasonable policies for
routing or deny-lists, but they are not RFC equivalence. The tests mark
them POLICY. The documentation should not call this comparing authorities
or origins.

### C5. "Rendered as recommended by RFC 5952" while departing from RFC 5952 Section 5

API (`renderHost`): "IPv6 addresses are rendered as recommended by RFC
5952". (`renderIPv6`): only `::ffff:0:0/96` gets dotted decimal, "not RFC
4291's deprecated IPv4-compatible prefix (::/96) ... nor the obsolete
IPv4-translated prefix of RFC 2765 (::ffff:0:0:0/96)".

RFC 5952 Section 5: "mixed notation is RECOMMENDED if ... the address can
be distinguished as having IPv4 addresses embedded in the lower 32 bits
solely from the address field through the use of a well-known prefix. Such
prefixes are defined in [RFC4291] and [RFC2765] at the time of this
writing."

So the RFC 2765 prefix is one RFC 5952 names, and the library knowingly
renders it differently. There are good reasons (RFC 2765 is obsolete, and
::/96 contains `::1`; see A3), but the documentation should say it departs
from the recommendation rather than claim to follow it. There are no tests
on these prefixes.

### C6. `checkTrailers` overstates what RFC 9110 permits, and has the wrong default

API: "Fields that RFC 9110 explicitly allows in trailers, such as ETag and
Authentication-Info, are accepted." RFC 9110 Section 11.6.3: "Authentication-Info
can be sent as a trailer field (Section 6.5) when the authentication
scheme explicitly allows this." That permission is conditional, not
explicit. Proxy-Authentication-Info (11.7.3) is the same.

Also, Section 16.3.2 (Considerations for New Fields): "If the field is
allowable in trailers; by default, it will not be (see Section 6.5.1)."
`checkTrailers` has the opposite default: it accepts every field not on its
deny-list. That is documented as the application's job, so the X-Checksum
test is POLICY, but the "clearly cannot be" list is necessarily
incomplete.

### C7. Citation errors (minor)

- `readTrailers`: "Trailers are never merged into the header section (RFC
  9110 Section 6.5.2)". The prohibition is in Section 6.5.1 ("A recipient
  MUST NOT merge a trailer field into a header section unless ...") and in
  RFC 9112 Section 7.1.2.
- `isNumericLookalike`: "No real host name has a numeric top-level label
  (RFC 1123, Section 2.1)". RFC 1123 says the label "will be alphabetic",
  which is stronger (see A6).
- `isDnsHostName` points to `docs/design-notes.md, P10`, which reviewers
  of the API cannot see.

## Ambiguities

### A1. How 205 is expressed (`ContentNone` vs `ContentBytes ""`)
- Reading 1: a 205 "includes content of zero length" (6.4.1), so it is
  `ContentBytes ""`, and `ContentNone` is wrong because HTTP/1.1 would
  leave the response unframed.
- Reading 2: `ContentNone` is acceptable if the protocol layer emits
  `Content-Length: 0` for 205.

The API seems to require `ContentNone` (C1). I only test that `ContentBytes ""`
is accepted for 205. I do not test whether `ContentNone` is accepted.

### A2. Zero-length content for 204 and 304
I read 6.4.1's split between "do not include content" and "include content
... of zero length" to mean that `ContentBytes ""` is content, so it is
rejected for 204 and 304. This matches the API's own distinction between
`ContentNone` and `ContentBytes ""`. Someone who treats empty bytes as "no
content" would accept it. The difference matters on the wire only if the
layer emits `Content-Length: 0` for a 204, which RFC 9110 Section 8.6
forbids ("MUST NOT send a Content-Length header field in any response with
a status code of ... 204").

### A3. RFC 5952 Section 5's "well-known prefixes"
Read literally, "[RFC4291] and [RFC2765]" covers the IPv4-compatible
`::/96` prefix (which contains `::1`, `::2:1` and so on) and
`::ffff:0:0:0/96`. That reading would render `::1` as `::0.0.0.1`. The other
reading covers only prefixes still in use, i.e. IPv4-mapped. The API takes
the second. My tests avoid every address in `::/96` except `::` and `::1`,
whose compressed forms RFC 4291 Section 2.2 gives explicitly. They also
avoid `::ffff:0:x:x` and `64:ff9b::/96`.

### A4. Trailing-dot equivalence (see C4)
DNS resolves `example.com.` and `example.com` alike (in most resolver
configurations), but RFC 3986 calls the dot significant. Treating them as
the same is a reasonable comparison policy, but it is not RFC equivalence.
The tests mark it POLICY.

### A5. Which fields are "routing, authentication, request modifiers, response controls, or content format" (RFC 9110 6.5.1)
The RFC does not list them. I test only clear cases: framing and
connection fields, Host, the credential and challenge fields,
Cache-Control, conditionals, Range, Expect, Max-Forwards, Content-Type,
Content-Encoding and Content-Range. I do not test Location, Vary,
Retry-After, Age, Expires, Date, Set-Cookie, Content-Language,
Content-Location, Trailer or the Accept-* fields. Any of these could
reasonably be called "response controls" or "request modifiers", and none
of their definitions permits trailers.

### A6. "Numeric" vs "alphabetic" top-level labels
RFC 1123 Section 2.1: "at least the highest-level component label will be
alphabetic". The API rejects only last labels that are numeric (decimal,
octal-looking, `0x` hex). Under the strict reading, `example.a1` or
`example.123abc` is not a host name, but `isDnsHostName` accepts it. I
test only all-digit and `0x` last labels, plus `08`, which I treat as
numeric because it is all digits even though it is not valid octal.

### A7. Maximum name length: 253 vs 255
RFC 1123: "SHOULD handle host names of up to 255 characters". RFC 3986
Section 3.2.2: names should be limited "to no more than 255 characters".
RFC 1035 Section 2.3.4 limits names to 255 *octets* on the wire, which is
253 characters without the final dot. The API's 253 (plus an optional dot)
matches DNS. A name of 254 or 255 characters that RFC 1123 says hosts
SHOULD handle cannot be resolved anyway. My tests follow RFC 1035.

### A8. Quoted strings that span field lines (`fieldListElements`)
RFC 9110 Section 5.5 (Note) applies a field's grammar "after said value
has been extracted ... and multiple instances combined into a list".
Combined, the lines `x: "a, b` and `x: c"` form a well-formed quoted-string.
Line by line they are malformed. The API splits line by line and says "For
a well-formed message the result is the same". Whether such a message is
well-formed depends on the reading. The API rejects it, which is the safer
choice. I do not test it.

### A9. Duplicate parameters
RFC 9110 5.6.6 says nothing about a parameter name given twice. The API
treats it as an error on lookup (POLICY tests). It is not documented what
`lookupParameter "a"` returns when a *different* name (`b`) is repeated.
I do not test that.

### A10. IPv6 zone identifiers
RFC 3986 has no zone IDs. RFC 6874 (which updates 3986) adds `%25zone`.
RFC 9110 cites only RFC 3986, so I test only that `[::1%eth0]` is
rejected (it is invalid under both) and leave `%25` alone.

### A11. Extended CONNECT with an ordinary target
`validForMethod` lets CONNECT use an ordinary target URI on every version.
RFC 9110 Section 9.3.6 says CONNECT "uses a special form of request target,
unique to this method". RFC 9112 Section 3.2.3 says a client "MUST send only
the host and port of the tunnel destination". Only RFC 8441/9220 (HTTP/2
and HTTP/3) define the extended form. Since `Request` records the version,
`mkRequest` could reject an HTTP/1.x CONNECT with an origin-form target.
Section 7.1's MUST NOT does not by itself forbid this, so I have no test.
I do test that a 2xx to an extended CONNECT also counts as a tunnel.

## Gaps

1. **426 / Upgrade advertisement** (C2): there is no way to meet RFC 9110
   7.8 / 15.5.22.
2. **Repeated singletons in trailers, and across headers and trailers**: RFC
   9110 Section 5.3 forbids repeating a non-list field "in a message
   (whether in the headers or trailers)". Section 6.5.2: "Trailer fields
   that might be generated more than once during a message MUST be defined
   as a list-based field". `checkTrailers` sees only the trailers and is
   not documented to reject repeated singletons. Nothing stops ETag from
   appearing in both the header section and the trailer section. One test
   covers ETag twice in trailers.
3. **Content-Length / Transfer-Encoding for 304 and HEAD**: RFC 9110 8.6 and
   RFC 9112 6.1 let a 304 carry the 200's Content-Length (and
   Transfer-Encoding). For a 304 (`ContentNone`) the application has no
   way to supply it. For HEAD with `ContentStream Nothing` no length can be
   given. Both are MAYs, so they are not violations.
4. **Fields named in Connection**: RFC 9110 7.6.1 makes every field named
   as a connection option hop-by-hop ("remove any header or trailer
   field(s) from the message with the same name as the connection-option").
   It also says "a connection-specific field received without a
   corresponding connection option ... ought to be ignored". Connection is
   removed before `mkRequest`, so `mkRequest` cannot check this. The
   documentation does not say whether the protocol layer strips those
   fields. `isProtocolField` covers only the fixed list (not, for example,
   HTTP2-Settings from h2c upgrades).
5. **TE: trailers is invisible**: TE is a protocol field. The application
   cannot see whether the client will keep trailers (RFC 9110 6.5.1, 10.1.4)
   when deciding what to put in them ("a server SHOULD NOT generate trailer
   fields that it believes are necessary for the user agent to receive").
   It is not documented what happens to `ContentStream` trailers for an
   HTTP/1.0 client, or for a client without `TE: trailers`.
6. **Trailer header field**: RFC 9110 6.6.2 says a sender planning trailers
   "SHOULD generate a Trailer header field". Nothing ties a
   `ContentStream`'s trailers to the Trailer field, and `checkTrailers`
   does not compare them with it.
7. **Date**: RFC 9110 6.6.1 says "An origin server with a clock MUST generate
   a Date header field in all 2xx ..., 3xx ..., and 4xx ... responses".
   The API does not say whether the protocol layer adds Date or the
   application must.
8. **Interim responses / Expect**: `Response` excludes 1xx and there is no
   other API for 100 Continue or 103 Early Hints. Handling of
   `Expect: 100-continue` (RFC 9110 10.1.1, including "MUST ignore that
   expectation" for HTTP/1.0) is not documented.
9. **CONNECT has no content** (RFC 9110 9.3.6: "A CONNECT request message
   does not have content"): neither `mkRequest` nor `RequestBody` enforces
   or documents this.
10. **BWS parameters**: `parseParameters` follows 5.6.6 (no whitespace
    around "="), so it cannot parse `transfer-parameter` in TE (10.1.4:
    `token BWS "=" BWS ...`). RFC 9110 5.6.3 requires recipients to accept
    BWS. This matters only to the protocol layer, but the module looks
    reusable for it.
11. **Singleton list beyond RFC 9110/9111**: `singletonResponseFields` does
    not include fields from other RFCs that must not repeat (e.g.
    Content-Disposition, Strict-Transport-Security), so applications that
    send those get no protection.
12. **IDNA**: `canonicalHost` does not identify a percent-encoded UTF-8 name
    with its A-label (RFC 3986 3.2.2: such names "must be transformed to the
    IDNA encoding [RFC3490] prior to name lookup"). That is defensible,
    since the documentation only claims the mappings it lists, but an
    allow-list keyed by A-labels can be bypassed by U-label spellings
    unless `isDnsHostName` is required.

## Assumptions

- `splitList` returns each element exactly as written (quotes, backslashes
  and inner whitespace kept), with only the OWS around commas removed.
  Several tests also check only the element count.
- `renderHost` may or may not bracket an IPv6 literal. Tests strip
  brackets before comparing, and check the brackets through
  `renderAuthority`, where RFC 3986 requires them.
- `mkRequest` rejects `Host`, but the documentation does not say with
  which error. `isProtocolField Host` is documented as False, so it is
  probably not `RequestProtocolField`. I test only `isLeft`.
- `mkRequest` normalizes an empty path rather than rejecting it, since
  `RequestError` has no constructor for that (POLICY tests).
- `ContentStream (Just 0)` and `ContentFile` count as content for
  204/304, because a stream can produce trailers and a file's size is
  unknown up front.
- For inputs with several errors (e.g. a CTL and a CR), I avoided relying
  on which error wins. Each `mkFieldValue` error test has a single
  offending octet.
- `parseParameters` takes everything after the item with no trailing
  whitespace. I did not test trailing OWS, which the grammar does not
  allow.
- `isNumericLookalike` decodes percent-encoded digits before judging (the
  `RegName` invariant says names are stored normalized). "Numeric" covers
  all-decimal labels (including `08`) and `0x`/`0X` hex labels.
- `isDnsHostName`'s 253-character limit does not count the trailing dot.
- `mkFileRange`'s "end" may be exclusive (offset+length) or inclusive. I
  chose overflow cases that overflow under both.
- `HostIPv4` holds the address in network order as a `Word32` (most
  significant octet first), as for `IPv6`'s "most significant half first".
- `checkTrailers`' "first field" means first in the order of the trailer
  section.
- A `%XX` sequence that decodes to an unreserved character is equivalent
  to that character, even when the result turns a reg-name into an
  IPv4address (`%31%32%37.0.0.1`). RFC 3986 2.3 makes the URIs equivalent,
  so a comparison key must agree. I test this through `canonicalHost` and
  `authorityKey`, not through the raw `Host`.
