# Design notes

## Where checks belong

Many attacks on HTTP servers use input that is perfectly valid by the
RFCs but means something different to another system: a file system, a
DNS resolver, a proxy in front of the server, a log viewer. RFC 3986
Section 7.3 and RFC 9110 Section 17.3 both put the defence at the point
where data crosses into that other system, because only the code at
that boundary knows the other system's rules.

So:

- **`hyperdrive-semantics`** owns facts that are true of HTTP regardless
  of deployment. It should make the safe use the easy one, and unsafe
  use visible at the call site (`lookupSingleton` vs `lookupFirst`,
  `SafeSegment` vs `pathSegments`).
- **The protocol layers** (HTTP/1.1, HTTP/2, HTTP/3) own anything that
  means rejecting a message, and the limits that protect the server.
- **The application-facing layers** (combinators, file serving, logging,
  access control) own policy that depends on where the data goes.

## Decisions so far

- The engine works on strict `ByteString`. Text only appears in the
  combinator layer, after strict UTF-8 decoding.
- Protocol cores are pure state machines (sans-IO), and the core is not
  tied to any streaming library.
- Field values containing CR, LF or NUL are rejected, never repaired;
  other control characters are rejected too. Userinfo in an authority
  is an error. Ports above 65535 are rejected.
- Paths and queries are kept exactly as received. `pathSegments` decodes
  faithfully; `SafePath` is the only way to turn segments into file
  names, and it guarantees only that they cannot escape a directory.
- Host names are kept in RFC 3986 normal form. `canonicalHost` and
  `authorityKey` give the form to compare hosts with.

## Requirements for other layers

Each item came from a security review of `hyperdrive-semantics`
(2026-11-15) or from the independent re-derivation of the RFCs
(`hyperdrive-semantics/test/IndependentTests.notes.md`). All are open.

### Protocol layers (HTTP/1.1, HTTP/2, HTTP/3)

- **P1. Repeated `Host` and `Content-Length`.** Read them with
  `lookupSingleton` and respond 400 to a repeated `Host` (RFC 9112
  Section 3.2). Repeated or list-valued `Content-Length` is invalid
  framing unless every value is the same (RFC 9112 Section 6.3; RFC
  9110 Section 8.6).
  Silently picking one value is how request smuggling and host header
  cache poisoning work.
- **P2. Host and target authority.** With an absolute-form target, the
  target's authority is used and `Host` is ignored (RFC 9112 Section
  3.2.2). Never let the two disagree silently.
- **P3. Target schemes.** An origin server serves `http` and `https`
  only, and must reject an `https` target received over an unsecured
  connection (RFC 9110 Section 7.4). Reject IPvFuture hosts as an
  address mechanism we do not support (RFC 3986 Section 3.2.2).
- **P4. OPTIONS.** An absolute-form OPTIONS target with an empty path
  and no query is forwarded by the last proxy as `*` (RFC 9112 Section
  3.2.4), so both mean the server as a whole; make sure both reach the
  same code.
- **P5. Limits.** Limits on the size of a field line, the size of the
  field section, the number of fields and the length of the target,
  answered with 431 (RFC 6585), 400 or 414 (RFC 9110 Sections 4.1 and
  5.4). The
  semantics package scales linearly but has no limits of its own.
- **P6. Rejecting field values.** A `Left` from `mkFieldValue` means
  rejecting the whole message with 400 (RFC 9110 Section 5.5), not
  dropping the field line.
- **P7. Field names with underscores.** Offer a setting, on by default,
  to reject or drop field names containing "_", since gateway
  interfaces map `Content_Length` and `Content-Length` to the same
  variable (RFC 9110 Section 17.10). nginx drops them by default.
- **P8. Numeric look-alike hosts.** Reject a `Host` or target authority
  for which `isNumericLookalike` holds, by default (RFC 3986 Section
  7.4, RFC 1123 Section 2.1).
- **P10. Encoded and non-ASCII hosts.** Reject a `Host` or target
  authority whose registered name is not a DNS host name
  (`isDnsHostName`), which rules out percent-encoding among other
  things. Every host that `isDnsHostName` accepts is one browsers
  accept as the same name (tested against the web-platform-tests data). RFC 3986
  allows any encoded octet in a registered name, but browsers send only
  ASCII names (with punycode for internationalized ones), and decoded
  hosts are dangerous: `hello%00` is `hello` to a C resolver, `ho%40st`
  and `ho%2Fst` change meaning when put back into a URL, and `%0D%0A`
  injects into logs. Names that IDNA maps to something else, such as
  `a%C2%ADb` (`ab`, the soft hyphen is dropped) or fullwidth digits
  (ASCII digits, possibly an IPv4 address), are rejected by the same
  rule. Found by the web-platform-tests URL data; see
  `hyperdrive-semantics/test/data/wpt/expected-differences.txt`.
- **P9. Query characters.** Decide whether to accept the characters
  that some clients send unencoded in queries (`|`, `{`, `}`, `^`, `` ` ``),
  for example by percent-encoding them on the way in, or to reject
  them. `Query` itself stays strict.

### The `Response` type (next in `hyperdrive-semantics`)

- **R1.** Response fields can only be built from `FieldName` and
  `FieldValue`, so decoded request data containing CR or LF (which
  `pathSegments` allows) cannot be reflected into a header.
- **R2.** `Set-Cookie` field lines are never combined.

### File serving

- **F1.** Every segment that becomes part of a file name goes through
  `SafePath`.
- **F2. Hidden files.** Refuse segments starting with "." by default
  (`.git`, `.env`, `.htpasswd`), with a configurable allow-list that
  includes `.well-known`. `SafeSegment` deliberately allows them.
- **F3. Symbolic links.** A link inside the served directory can point
  outside it. Open files relative to the root without following links
  out of it (`openat2` with `RESOLVE_BENEATH`, or resolve and check the
  real path), rather than checking names alone.
- **F4. Case and Unicode equivalence.** On case-insensitive file systems
  (Windows, macOS by default) and normalizing ones (macOS),
  `SECRET.txt` and `secret.txt`, or a precomposed and a decomposed
  `é`, can name the same file (RFC 9110 Section 17.3). Any check made
  on names must use the file system's own comparison.
- **F5. Windows.** Also reject `\`, `:`, device names (`CON`, `PRN`,
  `AUX`, `NUL`, `COM1`-`COM9`, `LPT1`-`LPT9`, with or without an
  extension), trailing dots and spaces, and alternate data streams
  (RFC 3986 Section 7.3).
- **F6.** File names from other sources, such as a `filename`
  parameter in `Content-Disposition` or multipart bodies, go through
  `mkSafeSegment` too.

### Combinators and the `Text` layer

- **T1. Strict UTF-8.** Decode with strict UTF-8 and reject invalid
  input. Lenient decoding maps different byte strings to the same
  replacement character, so two different paths would compare equal.
- **T2. One representation.** Route and check access on the same
  representation of the path, the segments from `pathSegments` (or
  `SafePath`). Otherwise `/%61dmin`, `/./admin` or `//admin` can pass a
  check written for `/admin`.
- **T3. Singleton fields and parameters.** Expose `lookupSingleton` and
  `lookupParameter`, not `lookupFirst`, and `fieldListElements` for
  list-based fields.
- **T4. Query strings.** Decide what repeated keys mean (parameter
  pollution), and only treat "+" as a space for form-encoded data.
- **T5. Gateway-style name mapping.** If field names are ever mapped to
  variable names, detect collisions such as `Content_Length` vs
  `Content-Length` (RFC 9110 Section 17.10).

### Logging

- **L1.** Escape control characters (CR, LF, ESC) and bytes that are not
  valid UTF-8 in anything logged from a request: paths, field values,
  methods. Otherwise requests can forge log lines or send escape
  sequences to whoever views the log.

### Access control and virtual hosts

- **A1.** Compare hosts with `authorityKey` or `canonicalHost`, never
  as text: `example.com.`, `Example.COM:80` and `example.com` are the
  same server, as are `[::ffff:127.0.0.1]` and `127.0.0.1`.
- **A2.** Allow or deny by address only after resolving a name, and
  compare the numeric address. If hyperdrive ever makes outbound
  requests (as a proxy), connect to the address that was checked, so
  that a second DNS lookup cannot return a different one (DNS
  rebinding).
