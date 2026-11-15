# Notes from the independent re-derivation

Written on 2026-11-15 by a reviewer who saw only the RFC 9110 and RFC 3986
texts and the API (signatures and documentation, no implementation or
tests), alongside IndependentTests.hs. Kept as a record of that review;
see the commit history for what was done about each point.

Tests: `IndependentTests.hs`, 355 cases. I could not compile it (no GHC
here). Descriptions marked POLICY rely on a documented choice that the RFC
permits but does not require.

Assumptions I had to make because API.md does not say:

- `mkPath` accepts exactly `path-abempty` (empty, or starting with "/").
  This is the path rule for http(s) URIs (RFC 9110 4.2.1) and the only one
  allowed with an authority (RFC 3986 3.3: "the path component must either
  be empty or begin with a slash"). So `mkPath ""` is accepted and
  `mkPath "a/b"` and `mkPath "*"` are rejected.
- `mkQuery` takes the query without its leading "?".
- `renderHost` includes the brackets of an IP-literal, because the RFC 3986
  `host` rule includes them.
- `pathSegments ""` is `Right []`, and `"//"` is `["", ""]`. This extends the
  documented `"/" -> []` and `"/a/" -> ["a", ""]` examples.

## Conflicts

No documented behaviour clearly breaks a MUST. These are the places where the
documentation goes beyond the RFC text or does not match it.

1. **Reg-name normalization and percent-encoding hex case.**
   The doc for `normalizeRegName` says "lower case, except for the hex digits
   of percent-encodings". RFC 3986 6.2.2.1 says the hex digits "should be
   normalized to use uppercase letters", and 3.2.2 says "only using uppercase
   letters for percent-encodings". If "except" means "left as received", then
   `%c3` and `%C3` give two different `RegName`s for the same host. That
   contradicts the claim that the result is a normal form. I test that
   `%c3%a9` becomes `%C3%A9`.
   - RFC 3986 2.3 and 6.2.2.2 also say percent-encoded unreserved octets
     "should be decoded". The doc does not mention this, so `%61.com` and
     `a.com` are different `Host`s. I did not test this, but anyone using
     `Host` equality for virtual-host lookup needs to know.

2. **`renderHost`: "as recommended by RFC 5952".** The doc's summary covers
   lower case, no leading zeros and the longest run (first on a tie). From
   memory, because RFC 5952 is not in the provided files: RFC 5952 section 5
   also recommends mixed notation for IPv4-mapped addresses,
   `::ffff:192.0.2.1` rather than `::ffff:c000:201`. The doc does not say
   which form is produced, so I left it untested. If the claim is "RFC 5952",
   either implement section 5 or narrow the claim.

3. **Port 0 for CONNECT** (`connectTarget`). RFC 9110 9.3.6 only says "A
   server MUST reject a CONNECT request that targets an empty or invalid port
   number". Port 0 matches `port = *DIGIT`. Treating it as "invalid" is
   reasonable, since nothing can connect to it, but it is a policy and not RFC
   text. Note that `parseAuthority` accepts `:0`, which is correct for Host.

4. **Ports above 65535** (`parseAuthority`). RFC 3986 3.2.3 has
   `port = *DIGIT` with no upper bound. Rejecting them is a policy, justified
   because http and https name a TCP port (RFC 9110 4.2.1/4.2.2). The error
   must come from range checking, not from overflow: I test 65616 and
   2^64+80, which would wrap to 80.

5. **`pathSegments "/" == Right []`.** By the grammar
   (`path-abempty = *( "/" segment )`), "/" has one empty segment, while
   "/a/" yields `["a", ""]`. So the documented behaviour treats a lone "/"
   differently from other trailing slashes. RFC 9110 4.2.3 justifies it
   ("an empty path component is equivalent to an absolute path of '/'"), but
   that equivalence explicitly does not apply to OPTIONS. `pathSegments` has
   no method to hand, so for OPTIONS it merges "" and "/". This is harmless
   but a deviation from the grammar.

## Judgment calls

- **Reject rather than repair CR/LF/NUL in field values.** RFC 9110 5.5: "MUST
  either reject the message or replace each of those characters with SP".
  Rejecting is permitted and the better choice for a server, because
  repairing hides smuggling attempts. But the RFC says reject *the message*.
  Callers must turn `Left` into a 400. They must not drop just the field
  line, which would be neither of the two allowed behaviours.
- **Reject other CTLs.** RFC: "also invalid; however, recipients MAY retain
  such characters". Rejecting is permitted and good. Caveats: HTAB is a CTL
  in RFC 5234 but explicitly allowed inside field-content, and DEL (0x7F) is
  a CTL.
- **Field names lower-cased.** Permitted ("Field names are case-insensitive")
  and right for HTTP/2 and HTTP/3. The original spelling is lost, which only
  matters to a proxy.
- **Userinfo is an error.** RFC 9110 4.2.4: "SHOULD parse for userinfo and
  treat its presence as an error". Good. The `Host` grammar
  (`uri-host [ ":" port ]`) has no userinfo at all, so for Host it is plain
  invalid syntax anyway.
- **Unknown methods are unsafe and not idempotent.** Conservative and
  correct. Method comparison must be case-sensitive (9.1), so "get" is
  unknown.
- **Path and query kept exactly as received.** Permitted ("any HTTP
  component MAY perform normalization", 4.2.3), and right for a server
  because decoding `%2F` changes meaning (RFC 3986 2.2).
- **`normalizeTarget` only turns an empty path into "/".** RFC 9110 4.2.3
  also lists as normal form: omit a default port (80 for http, 443 for
  https), lower-case scheme and host (done at construction), and do not
  percent-encode unreserved characters. RFC 3986 6.2.2.3 adds dot-segment
  removal. None of this is required, but the name `normalizeTarget`
  suggests more than it does. `http://h:80/` and `http://h/` normalize to
  different `Target`s, which matters for cache keys and for comparing
  against a configured origin. I test only that non-default ports such as
  http:443 and https:80 are *kept*. I did not test that default ports are
  dropped, because that is beyond the documented behaviour.
- **IPvFuture accepted as a host.** Grammatically correct. RFC 3986 3.2.2
  says an application that does not know the version "should return an
  appropriate error for 'address mechanism not supported'". A server may
  prefer to reject it, but the parser accepting it is right.
- **Extended CONNECT allowed with an ordinary URI.** Consistent with RFC 7.1
  ("These forms MUST NOT be used with other methods"), which only restricts
  the *special* forms.
- **`combineValues` produces comma SP.** That is what 5.3 says ("For
  consistency, use comma SP"). See the trap about empty values below.

## Traps

- **Empty field values in `combineValues`.** Plain concatenation of `"a"` and
  `""` gives `"a, "`, which has trailing whitespace and so is not a valid
  `field-value` (5.5: "does not include leading or trailing whitespace").
  Either drop the trailing SP or skip empty values. RFC 5.6.1.2 says
  recipients ignore empty list elements, so skipping is semantically safe.
  Tested.
- **HTAB is a CTL.** A check like "reject any CTL except CR/LF/NUL as
  ContainsCTL" will wrongly reject `a<TAB>b`. Tested.
- **DEL (0x7F) is a CTL**, and it is neither VCHAR nor obs-text. Tested for
  field values, tokens and the predicates.
- **Whitespace stripping applies to SP/HTAB only.** A trailing CR, or a
  leading VT or FF, must be *rejected*, not stripped. An implementation that
  strips with `isSpace` hides a CR. Tested.
- **first-match-wins.** `256.1.1.1`, `01.2.3.4`, `1.2.3`, `1.2.3.4.`,
  `0x7f.0.0.1` and `1234` are all *valid reg-names*, not errors. dec-octet
  forbids leading zeros. RFC 3986 7.4 warns that system resolvers may
  interpret these as IPv4 (octal, hex, short forms). So anything that
  filters on hosts (SSRF allow-lists, vhost matching) must not pass a
  reg-name straight to `inet_aton`/`getaddrinfo`. The API does not mention
  this.
- **No reg-name fallback inside brackets.** `[::01.2.3.4]` and
  `[::256.1.1.1]` are simply invalid.
- **IPv6 piece counting.** 7 pieces plus "::" is valid in both positions
  (`1:2:3:4:5:6:7::`, `::2:3:4:5:6:7:8`). 8 pieces plus "::" is invalid. An
  embedded IPv4 address counts as 2 pieces and only at the end. A single
  leading or trailing colon is invalid, and ":::" is invalid.
- **h16 is 1 to 4 hex digits**, so `01234` is invalid even though its value
  fits in 16 bits.
- **Zone IDs (`%25eth0`) are not RFC 3986 syntax** (RFC 6874 adds them). A
  parser that reuses its pct-encoded logic inside brackets may accept them.
- **The IPvFuture "v" is case-insensitive** (3.2.2: "starts with 'v'
  (case-insensitive)"). IPvFuture forbids pct-encoded and "/".
- **Port is `*DIGIT`.** Leading zeros are allowed (`:080` is 80, and so is
  `:000…080`). An empty port is the same as no port. A sign, hex or a space
  is invalid. Overflow must not wrap.
- **An empty port must not be rendered**: 3986 3.2 "should omit the ':'
  delimiter ... if the port component is empty".
- **Case sensitivity.** Scheme and host are case-insensitive. Path, query and
  method are case-sensitive (RFC 9110 4.2.3 and 9.1). Field names are
  case-insensitive. Percent-encoding hex digits are case-insensitive
  everywhere, including in `percentDecode` (`%4a`).
- **Lower-case `options`** is not OPTIONS. So it may not use `*`, and its
  empty path *is* normalized to "/".
- **Decode exactly once.** `%2541` decodes to `%41`, and `%252e%252e` to the
  harmless name `%2e%2e` (RFC 3986 2.4).
- **"+" is not a space** in paths or in `percentDecode`. That is HTML form
  encoding, not RFC 3986.
- **Encoded dot-segments.** `%2e` is an encoded unreserved char (2.3), so
  `%2e%2e`, `.%2e` and `%2E` are dot-segments. Checks must run after
  decoding (3986 7.3: "Security checks ... should be applied after decoding
  the octets").
- **`%00` is valid URI syntax.** `mkPath "/a%00"` must succeed, and only
  `pathSegments` and `SafePath` reject it (3986 7.3: "should be rejected if
  the application is not expecting ... raw data").
- **An empty query is not an absent query.** RFC 3986 6.2.3: "Normalization
  should not remove delimiters when their associated component is empty".
  Tested via `normalizeTarget`.
- **Status codes.** 306 is reserved but still within 100..599, and 600..999
  are used internally but invalid on the wire.
- **Set-Cookie** must never be combined (5.3 Note). `Fields` must keep its
  lines separate. Tested via `lookupAll`.

## Not covered by the API

These are requirements in the sections the API touches that it does not
address:

- **List syntax (5.6.1).** Parsing `#rule` values: empty elements,
  `OWS "," OWS`, quoted-string (5.6.4), comments (5.6.5) and parameters
  (5.6.6). Splitting a combined value on "," is unsafe because of quoted
  commas, and nothing helps with that.
- **Field limits (5.4).** "A server that receives a request header field
  line, field value, or set of fields larger than it wishes to process MUST
  respond with an appropriate 4xx". There are no size limits or hooks in
  `mkFieldValue` or `Fields`.
- **URI length (4.1).** "RECOMMENDED ... support ... 8000 octets". There is
  no limit and no 414 path.
- **Singleton field duplicates (5.5, 5.3).** Nothing detects multiple
  `Host`, `Content-Length` and so on. `lookupField` silently returns the
  first. For Host and Content-Length a server must reject duplicates or
  conflicting values. The rules are in RFC 9112, but `lookupField` makes it
  easy to forget.
- **Host vs. target authority consistency** (7.2, and RFC 9112 3.2). The
  API has no function relating the request-target authority to the Host or
  `:authority` field.
- **Scheme restriction.** `TargetURI` accepts any `Scheme`. An origin server
  normally only serves http/https, and RFC 9110 4.2.1/4.2.2 empty-host
  rejection is defined only for those. This is fine as long as callers
  check it.
- **OPTIONS with an empty path is the same as `*`** (RFC 9112 3.2.4, and
  RFC 9110 7.7's mention of the "*" replacement). `normalizeTarget` leaves
  the empty path but does not map it to `TargetAsterisk`, so callers must
  handle both.
- **Scheme-based normalization (4.2.3).** No default-port removal, no
  decoding of unreserved percent-encodings, no hex upper-casing in paths,
  and no dot-segment removal (3986 6.2.2.3). See Judgment calls.
- **Hosts.** No length limit on reg-names (3986 3.2.2 "should limit these
  names to no more than 255 characters"). No check that reg-name
  percent-encodings are UTF-8 (3.2.2 "must not use percent-encoding in host
  unless it is used to represent a UTF-8 character sequence"). No IDNA
  handling.
- **Decoded host data.** A reg-name may legally contain `%2F`, `%00` or
  sub-delims like "," and ";". Nothing warns callers who decode it for
  file-system or header use.
- **Fragments.** `Target` has no fragment, but nothing documents that a "#"
  in an HTTP/1.1 request-target or `:path` must be rejected by the caller.
  `mkPath` and `mkQuery` reject "#".
- **Windows file-name hazards in `SafePath`** ("\\", ":", device names, and
  trailing dots and spaces, per RFC 9110 17.3 and RFC 3986 7.3) are
  documented as out of scope.
