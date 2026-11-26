# HTTP/2: design proposal

Status (2026-11-25): D1-D12 decided (D3 and D4 on 2026-11-24, the rest
on 2026-11-25); D13 lists what is left for later. Nothing is
implemented; RFC 9113 and RFC 7541 are in duvet.

## Constraints already decided

- **HTTP/3 is wanted, and every decision here is made with it in mind**
  (the user, 2026-11-24). HTTP/2 comes first because it is the smaller
  protocol and stays the fallback where UDP is blocked and the usual
  protocol between proxies and origins. What HTTP/2 settles (the stream
  model, the handler interface, mapping pseudo-header fields to a
  `Request`, priorities, the TLS library) should carry over to HTTP/3
  unchanged; each decision below says how it does.
- As for HTTP/1.1: the protocol core is pure (sans-IO), with one `step`;
  applications see only `Request` and `Response` from
  `hyperdrive-semantics`; no streaming library; protocol fields never
  reach the application; the RFC's text is cited beside the code with
  duvet; every check has a mutant that the tests kill.
- No dependency on Kazu Yamamoto's `http2`, `quic`, `http3` or `tls`.
- The driver and ALPN go in `hyperdrive-server` (docs/server-design.md,
  "Shape"); TLS comes with HTTP/2 (S5 there).

## Proposed shape

A package `hyperdrive-http2` (D1) with:

- `Hyperdrive.HPACK.*` (RFC 7541): prefixed integers
  (`Hyperdrive.HPACK.Integer`), the Huffman code
  (`Hyperdrive.HPACK.Huffman`), the static and dynamic tables, a
  decoder and an encoder. The integer and Huffman modules depend on
  nothing else, since QPACK (RFC 9204, Section 4.1) uses both
  unchanged; the names are HPACK's because QPACK's RFC calls them that.
- `Hyperdrive.HTTP2.Frame`: parsing and serializing the ten frame types,
  with the size, padding, flag and stream-identifier checks that do not
  need connection state.
- `Hyperdrive.HTTP2.Connection`: the connection and its streams, `step`.

`step` has the shape of HTTP/1.1's, with a stream identifier on
everything that belongs to one exchange:

```haskell
step :: Input -> Conn -> (Conn, [Output])

data Input
  = Received ByteString              -- bytes from the transport
  | PeerClosed
  | Respond StreamId ResponseHead    -- the handler's response head
  | WriteBody StreamId ByteString    -- response content, within what Writable allowed
  | EndBody StreamId Fields          -- end of content, with trailers
  | Consumed StreamId Int            -- the handler read this much request body (D6)
  | Abort StreamId                   -- the handler failed after its response started
  | TimedOut
  | Shutdown                         -- GOAWAY, finish open streams, close (D10)
  -- and the coarse time, for D9's nuisance budget (how is to be decided)

data Output
  = NewRequest StreamId RequestHead  -- a complete, checked request head
  | RequestData StreamId ByteString
  | RequestEnd StreamId Fields
  | RequestAborted StreamId          -- RST_STREAM from the client, or a stream error
  | Writable StreamId Int            -- the stream may send this many more octets (D6)
  | Send [ByteString]
  | NeedInput
  | Close
```

The core owns HPACK's two tables, the flow-control windows, the stream
states and the choice of which stream's data is sent next (D8). It
never holds more than a bounded amount of anyone's data (D6).

## Decisions to make

**D1. Packages (decided, 2026-11-25: as recommended below).** One package, `hyperdrive-http2`, with
HPACK inside it, and the modules QPACK will reuse (prefixed integers,
Huffman) kept free of anything HTTP/2-specific, so that moving them to a
shared package when HTTP/3 starts is mechanical. A separate
`hyperdrive-hpack` package now would be the alternative; it buys nothing
until a second user exists. The mapping between pseudo-header fields and
`Request`/`Response` is version-independent (RFC 9114 Section 4.3
repeats RFC 9113 Section 8.3 almost word for word), so it goes in
`hyperdrive-semantics` (D11).

**D2. Cleartext HTTP/2 (h2c) (decided, 2026-11-25: (b), on by default
on cleartext listeners, never on TLS).** Options: (a) none; (b) with
prior knowledge only (RFC 9113 Section 3.3): a connection whose first
octets are the preface `PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n` is HTTP/2; (c)
also the `Upgrade: h2c` dance from HTTP/1.1, which RFC 9113 Section 3.1
deprecates. Decided: (b). It costs one comparison a connection, lets
h2spec and h2load run before TLS exists, and is what proxies use toward
origins. The driver compares the first octets with the preface before
the HTTP/1.1 head parser sees them (which would answer `PRI * HTTP/2.0`
with a 505): fewer than 24 octets that are a prefix of the preface wait
for more; anything else goes to HTTP/1.1, whose core is unchanged. On a
TLS connection ALPN alone decides ("HTTP/2 connections over TLS MUST use
protocol negotiation in TLS", Section 3.3, cited in the driver): a
preface after ALPN chose `http/1.1` or nothing is a bad HTTP/1.1
request.

**D3. The TLS library (decided, 2026-11-24: as recommended below).** Requirements: TLS 1.3 and 1.2 with ALPN for
HTTP/2; an interface for QUIC (RFC 9001: handshake octets in and out at
each encryption level, and the traffic secrets), so that the same
library serves HTTP/3; usable through the FFI with our own non-blocking
socket I/O (memory buffers, not a library that owns the socket); not
Kazu's `tls`; maintained, and in nixpkgs.

The candidates (researched 2026-11-24; sources in the session's notes,
the few claims not verified marked so):

| library | QUIC interface | releases | licence | nixpkgs | notes |
|---|---|---|---|---|---|
| OpenSSL 3.5+ | its own third-party QUIC API (`SSL_set_quic_tls_cbs`, 3.5, April 2025) | 3.5 is LTS to 2030-04 | Apache-2.0 | default `openssl` (3.5.9) | used by ngtcp2 (marked experimental) and nginx; 3.x scales poorly across threads (lock and atomic contention, HAProxy's measurements, 2025) |
| AWS-LC | BoringSSL-style (`SSL_QUIC_METHOD`), slightly different | versioned, no LTS; FIPS branches kept 5 years | Apache-2.0 | `aws-lc` 5.14 | fastest in HAProxy's tests, their first choice; FIPS 140-3 certified |
| BoringSSL | the original `SSL_QUIC_METHOD` | date tags, "no guarantees of API or ABI stability" | mostly Apache-2.0 | yes | quiche, lsquic (not verified), Chromium |
| rustls | `rustls::quic` in Rust; **none in rustls-ffi's C API** (issue open since 2021) | 0.24 redesign under way | Apache/MIT/ISC | rustls-ffi 0.15 | memory-safe; needs a Rust shim of ours, and does not hand out raw secrets |
| quictls | (fork of OpenSSL) | original archived 2025-04 | | removed | superseded by OpenSSL 3.5 |
| wolfSSL | BoringSSL-style | | GPLv3 since 5.8.2, or commercial | removed (licensing) | ruled out by licence |
| picotls | yes | no tags | MIT | no | TLS 1.3 only: no TLS 1.2 for HTTP/2 |
| s2n-tls | "experimental… testing purposes only" | | Apache-2.0 | | not ready |
| GnuTLS | hooks since 3.7 | | LGPL | yes | little used by QUIC servers |

Haskell bindings: none fits. HsOpenSSL (Vladimir Shabanov, Andreas
Abel, Masatake Daimon) does not mention TLS 1.3, ALPN or QUIC and is
built around blocking sockets; the `rustls` package (amesgen) binds
rustls-ffi, which has no QUIC; no bindings to AWS-LC or BoringSSL's TLS
were found. So we write our own, small and close to the C API.

Recommended: our own thin binding through a small C shim, written
against an interface of our own that is just what TCP and QUIC need
(octets in and out, for QUIC by encryption level; traffic secrets;
ALPN; QUIC transport parameters; certificates and SNI). The C library's
callbacks fill buffers in the shim, which Haskell drains after an
`unsafe` call, so no callback enters Haskell; signing with the private
key, the one costly call, is `safe`. Behind that interface, **AWS-LC
first**: the fastest in independent measurements, a mature QUIC
interface of the kind the major QUIC implementations use, versioned
releases and FIPS; speed is the reason it beats OpenSSL here, since the
aim is to be faster than Kazu's stack, and OpenSSL 3.x's contention
across threads is the known obstacle. **OpenSSL 3.5 second**, as an
alternative behind the same interface (its long LTS and being every
distribution's default are worth having); each is a few hundred lines
of C, and benchmarks decide which is the default. rustls is the one to
watch for assurance, once its C API has QUIC.

Added 2026-11-25 after the second independent review. Signing happens
inside the handshake, so it cannot be `safe` while the handshake call
is `unsafe` unless the shim uses AWS-LC's asynchronous private key
method (the key operation returns "retry", the signature is made in a
separate `safe` call, the handshake resumes); that is the mechanism,
and each `unsafe` call is held to a budget of about 100 µs (ECDHE fits,
RSA does not). RFC 9113 Section 9.2 binds the shim's configuration:
renegotiation off ("An endpoint MUST treat a TLS renegotiation as a
connection error"), SNI supported, and TLS 1.2 limited to ephemeral
key exchange with AEAD ciphers (Section 9.2.2 and Appendix A). A small
spike of the shim comes well before step 7 of D12, since D3 is the
decision costliest to reverse and the last one D12 tests.

**D4. Streams and threads (decided, 2026-11-24: option A).** One owner
thread per connection holds the `Conn` and does all transport I/O; each
request's handler runs in a thread of its own and talks to the owner
through the connection's wake-up `MVar` and a queue. Considered and not
chosen: separate reader and writer threads (the state would be shared,
needing STM or locks, which were bottlenecks before; the owner gets the
same effect by waiting for writability in the same wait as
readability); running handlers on the owner thread (one slow handler
would stall every stream and the connection's own PING and
flow-control traffic; perhaps an opt-in setting later, by measurement);
a shared worker pool (contention for no gain, GHC threads being cheap;
a per-connection cache of parked threads fits inside A if `forkIO`
shows up in measurements); one event loop per core (the deferred
experiment in docs/benchmarks.md). A QUIC connection has the same shape
(one owner of packet numbers, loss recovery and QPACK, many independent
streams), so the HTTP/3 driver can share this structure.

**D5. The handler interface (decided, 2026-11-25).** Unchanged:
`Application = Context -> Request -> IO Response`, request bodies read
with `readChunk`, response content as bytes, a file or a
`ContentStream`. The version is already in the request
(`requestVersion`, `HttpVersion 2 0` for HTTP/2 as RFC 9110 Section 2.5
numbers it), so `Context` gains only the TLS details, later. An
application then runs on HTTP/1.1, /2 and /3 without knowing which. In
the stream's thread, `readChunk` and a `ContentStream`'s writes become
messages to the owner, and wait for its answer on the stream's own
`MVar`.

- Cancellation is cooperative: when the client resets the stream (or
  the connection goes), the stream's `readChunk`, `write` and `flush`
  throw `ConnectionClosed` (reused, perhaps renamed: the handler's
  answer is the same, stop, and the interface stays the same across
  versions). A handler computing without I/O runs until its next call.
  No asynchronous exceptions, as before.
- A stream reset while its handler runs keeps a slot until the handler
  returns, so that a client cannot reset and reopen streams faster
  than handlers end and pile up running handlers (rapid reset, D9);
  how this is counted is C6.
- `ContentFile` is read into frame-sized buffers: frames need a header
  every 16 KiB at most, and TLS rules out `sendfile` (short of kTLS).
  A known difference in speed from HTTP/1.1.

**D6. Flow control and back-pressure (decided, 2026-11-25).**
Receiving: the server opens a stream's window as the handler *reads*
(`Consumed`), not as data arrives, so a handler that does not read
stops its client after one window, and the memory held per stream is
bounded by the window. The connection window is also given back only
as handlers read, so the memory held per connection is bounded by that
one setting (100 streams of 64 KiB would otherwise be 6.4 MB a
connection). A few stalled streams can then use up the connection
window and stall the connection's other streams, but every stream on a
connection is the same client's, so that harm is self-inflicted.

- Initial windows, both settings: 1 MiB per stream and 1 MiB per
  connection, as Go (changed 2026-11-25 from 64 KiB per stream, after
  the survey of implementations below: Go and hyper use 1 MiB, warp
  256 KiB, Envoy 16 MiB; a 64 KiB window caps one upload at about 1.3
  MB/s at 50 ms). Memory per connection stays bounded by the 1 MiB
  connection window, credit being returned only as handlers read; the
  cost, as in Go, is that one handler that stops reading can stall the
  other uploads on its own connection. Growing windows (mod_http2 grows
  them to 2 MiB) are in D13, to be decided by measurement.
- WINDOW_UPDATE is sent once the credit not yet given back reaches half
  the window, not for every read; the core decides, so it is tested.
- A handler that responds without reading all of the body: once the
  response is complete, RST_STREAM with NO_ERROR (RFC 9113 Section 8.1
  allows it), instead of HTTP/1.1's reading and discarding.
- A client lowering SETTINGS_INITIAL_WINDOW_SIZE can make a send window
  negative (Section 6.9.2); the core handles it, and generated sessions
  include it.
- A client holding a stream's window at zero is a slow reader: each
  stream has a write timeout (`timeoutWrite`, applied to each stream).

Sending: a handler's write is taken in pieces the core allows
(`Writable`); a writer that gets ahead of its stream's window, or of a
per-stream buffer limit in the owner (recommended 64 KiB), waits on its
`MVar`. So memory per connection is bounded by settings, whatever the
client's windows. HTTP/3 has the same per-stream and per-connection
credit (QUIC's MAX_STREAM_DATA and MAX_DATA), so `Consumed` and
`Writable` carry over.

**D7. Server push: not implemented (decided, 2026-11-25).** Browsers
have removed it (Chrome in 2022), and HTTP/3's version is little used.
The server never sends PUSH_PROMISE, and RFC 9113's requirements on
sending pushes (Sections 6.6, 8.4.1, 8.4.2) become duvet exceptions
with that reason. The MUSTs that still bind a server that does not push
are implemented and tested:

- a PUSH_PROMISE from the client is a connection error PROTOCOL_ERROR
  (Section 8.4);
- the client's SETTINGS_ENABLE_PUSH other than 0 or 1 is a connection
  error PROTOCOL_ERROR (Section 6.5.2); otherwise it is ignored;
- the server's own SETTINGS omit SETTINGS_ENABLE_PUSH ("if a server
  does include a value, it MUST be 0").

What replaced push in practice is 103 Early Hints, which `Response`
cannot express on any version; that is in D13.

**D8. Priorities (decided, 2026-11-25).** RFC 9113 Section 5.3.2
deprecates RFC 7540's priority tree. The server sends
`SETTINGS_NO_RFC7540_PRIORITIES` (RFC 9218) and does not act on RFC
7540 signals, but still checks them: a PRIORITY frame on stream 0 is a
connection error, one of a length other than 5 a stream error
FRAME_SIZE_ERROR, none may come between the frames of a field block,
and they may arrive on idle and closed streams without changing their
state (Section 6.3); they count against D9's control-frame budget. The
priority fields of HEADERS are parsed, checked and dropped. Control
frames (SETTINGS and PING acknowledgements, WINDOW_UPDATE, RST_STREAM,
GOAWAY) are sent before any data.

The scheduler is a pure function in the core (which stream sends its
next frame), tested like the rest. The first version sends one frame
at a time from each stream that can send, in turn (round robin)
(changed 2026-11-25 after the independent review: the first version
had sent streams in order of identifier, which lets one endless
response, an SSE stream say, hold back every later stream's content
for ever; RFC 9218's "one by one" is for responses a client marks as
not incremental, and Go keeps round robin for clients that send no
signals). Its data structure is already RFC 9218's (a queue for each
urgency, in stream order), so that the next step adds parsing and the
one-by-one rule for streams the client marks, not a new structure. The next step, after the
benchmarks against warp (h2load sends no signals, so the benchmark
would not see it) but before HTTP/2 is finished and before HTTP/3
starts: the rest of RFC 9218, the same for HTTP/2 and HTTP/3 (the eight
urgencies, round robin among incremental streams of one urgency, the
`Priority` field and PRIORITY_UPDATE frames), with a structured-field
parser (RFC 9651) in `hyperdrive-semantics` and RFC 9218 in duvet.

**D9. Limits and attacks (decided, 2026-11-25).** The settings the
server sends, each a setting of ours with this default:

| setting | value | note |
|---|---|---|
| MAX_CONCURRENT_STREAMS | 100 | a stream counts until its handler returns (D5) |
| HEADER_TABLE_SIZE | 4096 | the protocol's default, not sent |
| MAX_FRAME_SIZE | 16384 | the protocol's default, not sent; the server's own frames stay at 16 KiB at most too, for fairness between streams |
| MAX_HEADER_LIST_SIZE | 32 KiB | measured as RFC 9113 does (name, value and 32 for each field), so not exactly HTTP/1.1's measure; and HTTP/1.1's 100 fields (D4 there) |
| INITIAL_WINDOW_SIZE | 1 MiB | D6 |
| NO_RFC7540_PRIORITIES | 1 | D8 |
| ENABLE_PUSH | not sent | D7 |

The client's HEADER_TABLE_SIZE governs the server's encoder's dynamic
table: a client may announce 4 GiB, and the encoder uses at most 4096
whatever it announces.

Each known attack, its defence, and the error. Each gets generated
sessions with the attack and with legitimate traffic just under the
limit, and a mutant for each check.

| attack | defence | error |
|---|---|---|
| rapid reset (CVE-2023-44487) | a stream's slot is held until its handler returns (D5); a stream above the limit gets RST_STREAM REFUSED_STREAM; resets by the client before the response cost nuisance budget | ENHANCE_YOUR_CALM |
| CONTINUATION flood (2024) | the encoded field block (HEADERS and its CONTINUATIONs) is bounded as it arrives, never buffered past the limit; empty CONTINUATIONs cost nuisance budget | connection error (HPACK cannot be skipped without decoding) |
| HPACK bomb (a small encoding of a large field list) | the decoded size counted while decoding; over MAX_HEADER_LIST_SIZE with the encoding within its limit, the table is still updated (it must stay in step), the fields dropped, and the answer is 431, then reset | stream error |
| malformed HPACK (Huffman padding or EOS, an index out of range, a table size update above our setting) | the decoder's checks | COMPRESSION_ERROR |
| floods of frames that do no work (PING, SETTINGS, PRIORITY, RST_STREAM, empty DATA without END_STREAM, PRIORITY_UPDATE; the 2019 CVEs) | one nuisance budget for the connection, below | ENHANCE_YOUR_CALM |
| floods while not reading the acknowledgements | a cap on control frames queued and not yet written (as Go and nginx); the driver keeps reading, as Section 5.2.2 requires (C2, C5; changed 2026-11-25: the first version stopped reading, which that MUST forbids) | ENHANCE_YOUR_CALM |
| windows held at zero, data dribbled | the bounded send buffer of each stream (D6); a write timeout per stream and per connection | RST_STREAM, close |
| SETTINGS never acknowledged | SETTINGS_TIMEOUT after a timeout (Section 6.5.3) | connection error |
| slow clients | timeouts for the preface, for an unfinished field block (CONTINUATIONs are contiguous, so this is the connection's), and for a connection idle with no open streams (GOAWAY) | close |
| RFC 7540 priority trees (the "resource loop") | none built (D8); PRIORITY frames cost nuisance budget | |

The nuisance budget is a leaky bucket: each frame that does no work
costs one, and credit comes back with time, up to a burst. A count
that only useful work paid back would kill idle connections whose
clients send PINGs to keep them alive (gRPC does). So the core is given
the coarse time as an input (the ticker already keeps it and the driver
reads it at no cost), and `step` stays pure: tests control time
completely. The burst and rate are settings, chosen from what browsers
and gRPC clients actually send (a burst of a few hundred and tens a
second to start).

Optional and off by default: a number of requests after which the
server sends GOAWAY (nginx's `keepalive_requests`), to rotate very
long-lived connections.

**D10. Errors and shutdown (decided, 2026-11-25).** Five cases:

1. Connection errors, as RFC 9113 requires: violations of framing and
   of stream states, and every HPACK error (the decoder's state is
   shared by all streams, so a bad field block cannot be dropped by
   itself; a head that cannot be read is therefore never a stream
   error).
2. Malformed in RFC 9113's sense (Section 8.1.1: pseudo-header fields
   wrong, upper-case names, connection-specific fields, prohibited
   characters, `TE` other than `trailers`, an invalid `:authority`, a
   `content-length` that differs from the DATA received): RST_STREAM
   PROTOCOL_ERROR alone, the stream error Section 8.1.1 requires, and
   the reason logged for the developer. This is what nghttp2, warp,
   Rust's h2 and (for most malformations) Go send (decided 2026-11-25
   after the survey of implementations below; the first version sent
   a 400 before the reset, which no implementation does, and which a
   400 with END_STREAM would make a frame on a closed stream). A
   `content-length` found wrong once the handler has started: the
   same, and the handler's `readChunk` throws `IncompleteBody` as on
   HTTP/1.1.
3. Well-formed, but refused (the 400 and 421 of D11, and D9's 431): the
   response with END_STREAM, then RST_STREAM NO_ERROR only if the
   request is still sending (Section 8.1 allows it after a complete
   response). This is what Go, nginx, nghttpx and mod_http2 do with
   the responses they send. Not PROTOCOL_ERROR.
4. The handler fails: before its response starts, 500, as on HTTP/1.1;
   after (`Abort`, or a `ContentStream` of the wrong length),
   RST_STREAM INTERNAL_ERROR.
5. Refused streams (over the concurrency limit, or above the last
   stream identifier of our GOAWAY): RST_STREAM REFUSED_STREAM, which
   tells the client nothing was processed and it may retry; so it is
   used only when that is true.

Connection errors: GOAWAY with the last processed stream, then close
as in HTTP/1.1 (half-close, linger), so that a reset does not destroy
the GOAWAY before the client reads it. Graceful shutdown: GOAWAY with
the largest stream identifier (2^31-1), one round trip (a PING), then
GOAWAY with the last stream actually accepted, then finish those
streams within the grace period, as docs/server-design.md's two
stages; at the end of the grace period, RST_STREAM CANCEL for the
streams left, GOAWAY, the staged close, and the handlers' threads
killed (one of the two places asynchronous exceptions are used).

**D11. Pseudo-header fields to `Request` (decided, 2026-11-25: as
below, with the additions after it; `:scheme` corrected the same day).** `:method`, `:scheme`,
`:authority` and `:path` become the method and a `Target` through the
existing target reconstruction, with the checks of RFC 9113 Section
8.3.1 (required, single, before regular fields, no unknown ones, CONNECT's
special form), `Host` as the fallback for a missing `:authority`, split
`Cookie` fields joined (Section 8.2.3), connection-specific fields
refused (Section 8.2.2), `TE` allowed only as `trailers` and turned into
`setAcceptsTrailers`. Recommended in `hyperdrive-semantics`, shared by
HTTP/2 and HTTP/3, with the layer table in docs/design-notes.md
extended. Field names must arrive in lower case (Section 8.2.1); the
fast field scanning of HTTP/1.1 does not apply, since HPACK delivers
names and values already separated.

- `:authority` and a `Host` that differs: malformed, following "A
  server SHOULD treat a request as malformed if it contains a Host
  header field that identifies an entity that differs from the entity
  in the ':authority' pseudo-header field" (Section 8.3.1). The two
  are compared after the existing authority parsing (host without
  case, default port removed). This is the opposite of HTTP/1.1, where
  an absolute-form target overrides a disagreeing `Host` by a MUST; the
  two RFCs say different things, and two sources for the authority are
  what routing and smuggling attacks use. `Host` is then left out of
  the `Request`, as on HTTP/1.1.
- `:scheme` (corrected twice on 2026-11-25): RFC 9113 allows any
  scheme ("':scheme' is not restricted to 'http' and 'https' schemed
  URIs", Section 8.3.1), so such a request is well formed. As HTTP/1.1
  does for an absolute-form target (design-notes P3), a scheme other
  than `http` or `https`, or `https` on a cleartext connection, gets
  421 (Misdirected Request) as a normal response (D10's case 3),
  unless the listener is set to trust its peer as a gateway (RFC 9110
  Section 7.4: "Unless the connection is from a trusted gateway, an
  origin server MUST reject..."), in which case `:scheme` (or the
  absolute-form's scheme, on HTTP/1.1) `https` is accepted (other
  schemes still get 421: this server serves only `http` and `https`). That setting
  is off by default, which keeps the MUST; a proxy speaking h2c to the
  server (D2's use) needs it on, and the 421 is logged saying so. The
  connection's own security is `contextSecure` either way. All nine
  implementations surveyed accept `https` over cleartext; only nghttpx
  can refuse it, with an option that is off by default.
- Whitespace around a field value is removed, as HTTP/1.1 removes it,
  rather than making the request malformed (decided 2026-11-25 after
  the survey): RFC 9113 Section 8.2.1 says "A field value MUST NOT
  start or end with an ASCII whitespace character" and calls such a
  message malformed, but of nine implementations only h2o refuses it
  (Go, warp, nginx, Rust's h2 and Envoy pass it through, mod_http2
  strips it, nghttp2 drops the field), and when nghttp2 began to
  enforce it, Envoy turned the check off after "widespread" failures
  at Lyft (Envoy issue 23266). A known deviation, recorded in duvet as
  an exception with this reason. NUL, CR, LF and other control
  characters remain malformed.
- Malformed (D10's case 2: RST_STREAM PROTOCOL_ERROR): what Section 8
  calls malformed. A field name that is not a token, has upper case or
  a colon; a value with NUL, CR, LF or another character outside
  field-value; an unknown or repeated pseudo-header field, or one
  after a regular field; a missing `:method`, `:scheme` or `:path`, or
  one without a valid value ("exactly one valid value"), and an
  `:authority` that is not `uri-host [ ":" port ]` ("invalid
  pseudo-header fields", Section 8.3; as nghttp2 and Go); CONNECT with
  `:scheme` or `:path`, or without `:authority`; connection-specific
  fields; `TE` other than `trailers`; a `Host` that differs from
  `:authority`; in trailers, pseudo-header fields, and `Content-Length`
  or `Transfer-Encoding` (refused as HTTP/1.1 refuses them).
- Refused (D10's case 3, a normal response, as HTTP/1.1 answers): an
  invalid `Host` (userinfo, a bad port), more than one `Host`, no
  authority and no default, `*` with a method other than OPTIONS, an
  invalid `Content-Length` (400); the scheme cases above (421).
- `TargetContext` and the parsing of `Content-Length` move from
  `hyperdrive-http1` to `hyperdrive-semantics`, re-exported where they
  were, so that HTTP/2 does not depend on HTTP/1.1.
- The result separates the two kinds of refusal (D10's cases 2 and 3),
  so that the core knows whether the error is PROTOCOL_ERROR: an error
  type with the class in it, as `Either PseudoError Request`.
- The reverse mapping for responses (`:status` first, names in lower
  case, no protocol fields, which `mkResponse` already ensures) goes in
  the same module, also shared with HTTP/3.

**D12. Order of work (decided, 2026-11-25).** Each step tested before
the next:

1. duvet: RFC 9113 and 7541.
2. HPACK, against RFC 7541 Appendix C's examples and the hpack-test-case
   corpus, with round-trip properties and mutants, and the differential
   test of the decoder against nghttp2 (in the llhttp harness's shape:
   a cabal flag, a shell, a build directory). It starts with a short
   note on the encoder's policy, which the RFC leaves open: what enters
   the dynamic table (a server's responses repeat `content-type`,
   `server`, `cache-control`), Huffman only when shorter, and which
   fields are never indexed (`set-cookie`: an attacker who can inject
   into a response and see its compressed size can guess secrets, RFC
   7541 Section 7.1).
3. Frames (the differential test against nghttp2 is in step 6).
4. D11's mapping in `hyperdrive-semantics` (independent of 2 and 3,
   but HPACK settles the shape of what it receives).
5. `step`: stream states, flow control, settings, errors, the scheduler;
   a reference model and generated sessions, as for HTTP/1.1.
6. The driver over h2c (D2, D4); h2spec; h2load against warp; and the
   differential test of frames against nghttp2, moved here from step 3
   (the user, 2026-11-25): nghttp2 has no public function that parses
   one frame, only a whole session, so the comparison needs the
   connection core. Then the rest of RFC 9218 (D8).
7. TLS (D3): HTTP/1.1 over TLS first, benchmarked against warp-tls, so
   that TLS's own cost is measured on the protocol already known; then
   HTTP/2 through ALPN, and benchmarks over TLS.

**D13. Later.** Extended CONNECT (RFC 8441, WebSockets over HTTP/2; RFC
9220 for HTTP/3), 103 Early Hints (an interim response in the
application interface, for every version; D7), the
inline-handler setting (D4), parked stream threads (D4), receive windows
that grow while a handler keeps up (D6).

## HPACK's encoder (decided, 2026-11-25: H1-H4 as recommended)

The decoder is fully specified by RFC 7541; the encoder chooses. Each
field it sends is one of: an index into the tables; a literal added to
the dynamic table ("with incremental indexing"); a literal not added
("without indexing"); or a literal not added that intermediaries must
not add either ("never indexed", Section 6.2.3). And each string is
sent plain or Huffman-coded. The server encodes responses only.

**H1. What enters the dynamic table.** Options:

- (a) Nothing: the static table and literals only. The cheapest in CPU
  (no table kept), and no compression across responses, so no attack
  that guesses a secret from compressed sizes (Section 7.1). Costs the
  repeated fields' bytes on every response.
- (b) A fixed list of field names whose values repeat across
  responses: `:status` values not in the static table, `content-type`,
  `server`, `cache-control`, `vary`, `content-encoding`, `date`
  (changes once a second, one entry a second), `alt-svc`,
  `strict-transport-security`, `x-content-type-options`,
  `access-control-allow-origin`. Everything else (`content-length`,
  `etag`, `last-modified`, `location`, application fields) without
  indexing. Deterministic, the table is small enough to search
  linearly, and no attacker-influenced value enters the table unless
  its name is on the list.
- (c) Everything, with heuristics, as nghttp2 does: the most bytes
  saved, but every value an attacker can reflect into a response
  shares a context with the rest.

Recommended: (b). The list is fixed in the code at first; making it a
setting can wait for an application that needs one. Amended 2026-11-25
after the second independent review: an entry is indexed only if it
is at most a quarter of the table (one larger empties both sides'
tables, as nghttp2's encoder also avoids); the long security fields
that repeat on every response are added (`content-security-policy`,
`permissions-policy`, `referrer-policy`, `x-frame-options`,
`cross-origin-opener-policy`, `cross-origin-embedder-policy`,
`cross-origin-resource-policy`, `accept-ranges`); and
`access-control-allow-origin` is removed, since it usually reflects
the request's `Origin`, which would put a value the client chose into
the table. Measured against
(a) on the benchmark app's responses (bytes a response, µs a response,
and h2load's "space savings"): if (b) costs measurable CPU for the
bytes it saves, (a).

**H2. Never indexed.** `set-cookie`, `www-authenticate`,
`proxy-authenticate` and `authentication-info` (its `nextnonce`): sent
as never-indexed literals, the name still taken from the static table.
Section 7.1.3: "An encoder might also choose not to index values for
header fields that are considered to be highly valuable or sensitive
to recovery". A way for an application to mark its own fields
sensitive would need a change to `Fields`; not now.

**H3. Huffman.** A string is Huffman-coded when that is strictly
shorter; computing the coded length is a sum over a 256-entry table,
done before encoding. Ties go to the plain string (no decoding work
for the client).

**H4. Table size.** The encoder's table is the smaller of the client's
SETTINGS_HEADER_TABLE_SIZE and our own cap, 4096 (D9). Section 4.2:
the update "MUST occur at the beginning of the first header block
following the change", and if the size changed more than once in
between, "the smallest maximum table size that occurs in that interval
MUST be signaled", then the final one (at most two updates). Recommended:
whenever a client's SETTINGS changes HEADER_TABLE_SIZE, the next block
starts with the smallest size since the last block and then the
current one (one update if they are equal), even if our cap means the
size did not change: an unneeded update is valid, and a missing one is
a COMPRESSION_ERROR at the client. A client's 0 makes (b) behave as
(a).

**The decoder (decided, 2026-11-25).** Any number of dynamic table
size updates may start a block; one after a field is refused. Section
4.2's "resulting in at most two dynamic table size updates" describes
what an encoder's requirements lead to, not a limit on decoders, and
nghttp2 accepts more (the differential tests found this, the only
difference between the two decoders in 100,000 generated sessions).
Each update costs a resize, and a block's size is bounded (D9).

**For HTTP/3.** QPACK can take H1's list and H2 unchanged; whether its
encoder uses a dynamic table at all (which brings an encoder stream,
acknowledgements and blocked streams) is a separate decision for
HTTP/3, and (a) is a sound start there.

## Frames (decided, 2026-11-25: F1-F5 as below)

`Hyperdrive.HTTP2.Frame` parses one frame from the start of the
received octets and serializes the frames a server sends. It makes
every check that needs no connection state; the core (`step`) makes the
rest. The RFC fixes most of it; five choices remain.

**F1. Unknown frames are returned, not dropped.** "Implementations MUST
ignore and discard frames of unknown types" (Section 4.1), but an
unknown frame in the middle of a field block is a connection error
(Section 5.5), and D9's nuisance budget must count them. So the parser
returns `Unknown` with the type, stream and length (no payload), and
the core ignores it except for those two. Unused flags have no meaning
and are masked off when parsing; unknown settings are dropped
("MUST ignore that setting", Section 6.5.2).

**F2. Errors carry the scope the RFC gives them, and the core may
widen it.** A PRIORITY frame of the wrong length and a WINDOW_UPDATE of
0 on a stream are stream errors; everything else the parser finds is a
connection error. The core turns a stream error on an idle stream into
a connection error (it may not send RST_STREAM for an idle stream,
Section 6.4; "an endpoint MAY choose to treat a stream error as a
connection error", Section 5.4).

**F3. A frame longer than our SETTINGS_MAX_FRAME_SIZE is a connection
error, whatever its type,** found from the 9-octet header before any of
the payload is buffered. The RFC requires a connection error only for
some types (Section 4.2) and allows it for all; a stream error would
still oblige the server to read and discard up to 16 MB, and to count a
DATA frame's length against the connection window.

**F4. The contents of padding are not checked** (changed 2026-11-25
after the survey of implementations below: the first version refused
non-zero padding). Section 6.1: "A receiver is not obligated to verify
padding but MAY treat non-zero padding as a connection error". None of
the nine implementations surveyed checks it, so a server that did
would be the only one to reset a client for it, for no gain in
safety; only the Pad Length is checked.

**F5. Payloads are slices of the received octets.** DATA's content and
HEADERS' and CONTINUATION's fragments are not copied; the core joins a
field block's fragments (usually there is one) and hands DATA to the
handler. Padding and the deprecated priority fields are checked and
dropped. The server never sends padding, PRIORITY or PUSH_PROMISE, so
the serializer has no way to.

## The connection core (decided, 2026-11-25, after two reviews)

`Hyperdrive.HTTP2.Connection`: the pure `step` of D12's step 5, which
the driver (step 6) runs as D4's owner thread. The sketch under
"Proposed shape" is replaced by C1-C13. A first version was reviewed by
the author, then independently (Fable, 2026-11-25: RFC 9113's text,
the behaviour of browsers, curl and gRPC, and the code of h2spec, Go's
x/net/http2, nghttp2, Envoy and nginx); this is the result.

**C1. Time is an argument, and the core keeps its own deadlines.**
`step :: Time -> Input -> Conn -> (Conn, [Output])`, with `Time` the
coarse clock the ticker already keeps (milliseconds; the driver reads
it from an `IORef` for each step). HTTP/1.1's core reports one phase
and the driver runs one timer for it (D8 there); an HTTP/2 connection
has deadlines per stream as well as its own, so the core keeps them
and exposes `nextDeadline :: Conn -> Maybe Time` (the earliest, kept
current as deadlines change, not searched for). The driver arms one
timer for it and feeds `TimedOut`; the core acts on whatever has
expired. D9's nuisance budget refills from the same time. Tests control
time completely.

**C2. Inputs and outputs.**

```haskell
data Input
  = Received ByteString            -- octets from the transport (the preface first)
  | Sent Int                       -- the transport has written this many more octets (C5)
  | PeerClosed
  | Respond StreamId ResponseHead  -- the handler's response head (C12)
  | WriteBody StreamId ByteString  -- response content (C5)
  | EndBody StreamId Fields        -- the end of the content, with trailers
  | Abort StreamId                 -- the handler failed after responding (D10 case 4)
  | WantContent StreamId           -- the handler read with nothing queued (C9)
  | Consumed StreamId Int          -- the handler read this much request content (D6)
  | HandlerDone StreamId           -- the handler returned (C6); before Respond, it failed
  | TimedOut                       -- nextDeadline has passed
  | Shutdown                       -- graceful shutdown (D10)

data Output
  = NewRequest StreamId RequestHead  -- D11's RequestControl, and whether content follows
  | RequestData StreamId ByteString  -- request content (C3)
  | RequestEnd StreamId Fields       -- its end, with trailers
  | RequestAborted StreamId Reason   -- the stream ended early: reset by the client, timed
                                     -- out, content not matching its length, the
                                     -- connection gone; the driver turns it into the
                                     -- handler's exception (ConnectionClosed, or
                                     -- IncompleteBody for content) and logs it
  | Writable StreamId                -- the stream's buffer is below its bound again (C5)
  | Send [ByteString]
  | Close                            -- after a GOAWAY, staged as HTTP/1.1's
  | ProtocolMisuse Misuse            -- the driver's bug, as HTTP/1.1's Q4
```

The core writes every response it makes itself, as HTTP/1.1's core
does (D3 there): the 400, 421 and 431 of D10 and D9, 417 for an
`Expect` other than `100-continue` (C9), and a 500 when `HandlerDone`
comes before `Respond` (the handler failed). Inputs from a handler for
a stream the core no longer serves (`Consumed`, `WantContent`,
`WriteBody`, `EndBody`, `Respond`, `Abort`, `HandlerDone`) are ignored,
not a misuse: the handler's thread races the owner, and a reset can
arrive between its action and the owner seeing it. When the connection
ends for any reason (a connection error, `PeerClosed`, the client's
GOAWAY once its streams are done, the end of the shutdown grace
period), every stream still served gets `RequestAborted`, so that no
handler waits for ever on its `MVar` (D5 has no asynchronous
exceptions but the grace period's); `HandlerDone` is still accepted
after `Close`. The driver always reads (Section 5.2.2: "Endpoints MUST
read and process HTTP/2 frames from the TCP receive buffer as soon as
data is available").

**C3. Request content is pushed, and its credit comes back as it is
read.** The core passes DATA on as `RequestData` as it arrives, and the
driver queues it for the stream's handler. `Consumed` returns the
credit, at both levels, and the core sends WINDOW_UPDATE once a
window's unreturned credit reaches half of it (D6). HTTP/1.1 pulls
content instead (`WantBody`, Q1 there), because one TCP stream has no
window; here the window is the pull.

- DATA beyond the window we advertised is refused (Section 6.9.1:
  "The sender MUST NOT send a flow-controlled frame with a length that
  exceeds the space available in either of the flow-control windows"):
  over the stream's, RST_STREAM FLOW_CONTROL_ERROR and `RequestAborted`
  (the frame still counted against the connection's, Section 6.9);
  over the connection's, GOAWAY FLOW_CONTROL_ERROR. This is what makes
  D6's bound hold. The server never lowers its stream window during a
  connection, so Section 6.9.3's allowance for data sent before a
  lowered setting arrived never applies.
- Credit the handler can never return comes back at once: a DATA
  frame's padding and Pad Length (counted by flow control, Section
  6.1); all of a DATA frame for a stream nobody will read (reset,
  refused, closed, or ended by D6's NO_ERROR), which Section 6.9
  requires be counted; and, when a stream ends, what was passed on but
  not consumed (else sixteen uploads that each send 64 KiB and cancel
  would use up a 1 MiB connection window for good). A `Consumed` for a
  stream already ended is ignored (C2), its credit having come back.
- The DATA received is checked against `Content-Length`: more than it,
  or less at END_STREAM, makes the request malformed (Section 8.1.1;
  D10 case 2).
- A `RequestData` that is small beside the receive buffer it is a
  slice of is copied when it is queued (under 4 KiB and under a quarter
  of the buffer, to start): otherwise a client could pin a 16 KiB
  buffer with each octet of window on a stream whose handler does not
  read, by pairing it with data for a stream that does.

**C4. Stream states.** A server that never pushes uses idle, open,
half-closed (both kinds) and closed (Section 5.1). Client streams are
odd and increasing (Section 5.1.1), so the core keeps the streams it
still serves in a map (with each one's protocol state: a stream reset
while its handler runs stays in the map, closed, until `HandlerDone`,
C6) and the highest identifier opened.

- Idle (above the highest): HEADERS opens the stream (an even
  identifier is a connection error PROTOCOL_ERROR, Section 5.1.1);
  PRIORITY is allowed; any other frame is a connection error
  PROTOCOL_ERROR (Section 5.1, idle: "Receiving any frame other than
  HEADERS or PRIORITY on a stream in this state MUST be treated as a
  connection error").
- Half-closed (remote): DATA or HEADERS is a stream error STREAM_CLOSED
  (a MUST of Section 5.1).
- Closed (in the map as closed, or at or below the highest and not in
  the map): frames are minimally processed and discarded, which Section
  5.1 allows for every closed stream ("An endpoint can perform this
  minimal processing for all streams that are in the 'closed' state"):
  field blocks decoded for HPACK, DATA counted against the connection
  window and its credit returned. A HEADERS there may be trailers the
  client sent before our reset reached it, so it cannot be told from a
  reused identifier; nghttp2 also discards. Each such frame costs
  nuisance budget (C10), so it is not free. h2spec, written to RFC
  7540, expects errors in four "closed" cases; those are listed as
  known differences, with this reason, when h2spec runs (D12 step 6).

**C5. Response content is buffered per stream, the scheduler sends it,
and the transport paces it.** `WriteBody` is accepted, whatever its
size, while the stream's buffer holds less than 16 KiB and the
connection's response buffers together less than 256 KiB (changed
2026-11-25 from 64 KiB per stream and no connection-wide bound, after
the second review and the survey: Go holds 4 KiB a stream, mod_http2
32 KiB, h2o bounds its output at 80 KiB; 100 streams of 64 KiB were
6.4 MB a connection for a client that stops reading); then the driver
waits for `Writable` (writing more is a misuse), so a handler blocks
on its `MVar` while its buffer is full, and `Writable` goes to the
waiting streams in turn. The core does not copy the application's
chunks.

- The core emits DATA only while what it has emitted and the driver
  has not yet reported written (`Sent`) is under a bound (64 KiB, a
  setting). Without it, a client could open its windows wide and stop
  reading its socket, and data would move from the bounded stream
  buffers into the driver's unbounded write queue (Section 10.5: "An
  endpoint that constructs and remembers frames for sending without
  considering TCP limits might be subject to resource exhaustion"). So
  `Writable` follows what the transport takes, not only the windows;
  Go paces its writes the same way. This is also HTTP/3's shape, where
  QUIC's congestion window is how much the transport will take.
- Control frames (SETTINGS and PING acknowledgements, WINDOW_UPDATE,
  RST_STREAM, GOAWAY) go before any DATA and are not paced, but those
  queued and not yet written are capped (a setting, 10,000 frames, as
  Go caps them); over the cap, the connection is closed with
  ENHANCE_YOUR_CALM. This replaces D9's "stop reading".
- Response heads go out as soon as they come (HEADERS is not
  flow-controlled, and HPACK's encoder must see blocks in the order
  sent; a HEADERS and its CONTINUATIONs are one contiguous `Send`).
- DATA goes within the peer's windows and SETTINGS_MAX_FRAME_SIZE, one
  frame at a time from each stream that can send, in turn (D8). A
  DATA frame is at least 1 KiB unless it ends the content, so a window
  opened by a few octets at a time does not draw a frame for each
  (Section 10.5: "Providing tiny increments to flow control in
  WINDOW_UPDATE frames can cause a sender to generate a large number of
  DATA frames").
- The peer's windows may go negative (Section 6.9.2). A WINDOW_UPDATE
  that takes a window above 2^31-1 is RST_STREAM FLOW_CONTROL_ERROR
  for a stream and GOAWAY FLOW_CONTROL_ERROR for the connection
  (Section 6.9.1); a SETTINGS_INITIAL_WINDOW_SIZE that takes any stream
  window above it is a connection error FLOW_CONTROL_ERROR (Section
  6.9.2).

**C6. Concurrency (D5, D9).** Section 5.1.2 counts open and
half-closed streams, and a client counts the same way, so the core
counts those against MAX_CONCURRENT_STREAMS (100). Separately, a
stream reset (by either side) while its handler still runs keeps a
slot until `HandlerDone`, so that streams reset faster than handlers
end cannot pile up handlers (rapid reset); those slots have their own
bound (another 100, a setting), so that a browser that cancels a
page's requests and opens the next page's at once is not refused while
the cancelled handlers finish. A stream that ends normally frees its
slot when it closes. A HEADERS over either bound gets RST_STREAM
REFUSED_STREAM (Section 5.1.2 allows it, and Section 8.7 allows it
because nothing of the stream has reached a handler) and costs
nuisance budget; its field block is still decoded. A client may open
more than 100 before it has our SETTINGS ("Initially, there is no
limit"); those are refused the same way, and the budget's burst covers
them.

**C7. Field blocks.** After a HEADERS without END_HEADERS only a
CONTINUATION on the same stream may come (Sections 6.2, 6.10, 5.5);
anything else is a connection error. The encoded block is bounded as
it arrives, at 4 times MAX_HEADER_LIST_SIZE (128 KiB), beyond which it
is a connection error ENHANCE_YOUR_CALM (D9: it cannot be skipped).
Below that the block is decoded, and a field list over
MAX_HEADER_LIST_SIZE (32 KiB) is answered with 431 (Section 10.5.1: "A
server that receives a larger field block than it is willing to handle
can send an HTTP 431"), as D10 case 2 says (the 431, then the reset, or
the reset alone if the request has ended); real header sections above
64 KiB exist (Kerberos tokens, SSO cookies). Then D11's mapping. A
second HEADERS on an open stream is its trailers and must carry
END_STREAM, or the request is malformed (Section 8.1); after that the
stream is half-closed (remote) (C4).

**C8. Timers, each a setting (0 disables).**

- The connection: the preface and first SETTINGS (the head timeout,
  30 s); a field block left unfinished (30 s); our SETTINGS unacknowledged (10 s, SETTINGS_TIMEOUT, Section 6.5.3;
  counted from the arrival of the client's preface and SETTINGS, since
  a client cannot be faulted before it has started); the shutdown
  PING unanswered (1 s, as Go, after which shutdown goes on as if it
  had been answered); a connection with no streams (the idle timeout,
  60 s: GOAWAY NO_ERROR, then close), which a PING from the client
  restarts, so that a client keeping a quiet connection alive (gRPC's
  keepalive) is not closed, the budget bounding how often it may ping.
- Each stream: an idle timer that any frame in either direction on the
  stream restarts (5 minutes, as Envoy's `stream_idle_timeout`), not
  running while the stream waits on our side (a handler not reading
  with content queued, or a buffer full); and an upload-stall timer (30
  s, as HTTP/1.1's body timer) that runs only while a `WantContent` is
  unanswered, both windows are open and no 100 is owed. Both end with
  RST_STREAM CANCEL and `RequestAborted`. Long-lived streams (gRPC
  subscriptions, slow consumers exerting back-pressure, SSE) live
  as long as anything moves; a client that has stopped reading its
  socket altogether is ended by the driver's write timeout, as for
  HTTP/1.1. (The first version had per-stream 30 s timers on content
  in each direction, which would have ended every quiet gRPC stream.)

**C9. `Expect`.** `Expect` is taken out of the fields, as HTTP/1.1
does. `100-continue` (when content is to follow): the core sends the
100 on the stream's first `WantContent`, which the driver feeds when
the handler first reads with nothing queued, as HTTP/1.1's core sends
it when the handler first asks for content (D7 there). So a handler
that refuses before reading costs the client no upload, on either
version (D5). Content that arrives anyway (a client need not wait, RFC
9110 Section 10.1.1) is passed on as usual, and the 100 is then not
sent. Any other expectation: 417, as HTTP/1.1 answers it. The 100
never carries END_STREAM (Section 8.1).

**C10. The nuisance budget (D9).** A leaky bucket, a burst of 1,000
and 100 a second, both settings and provisional until measured against
real browser sessions in step 6 (navigating away, scrolling, seeking
send bursts of resets). Each costs one: PING, SETTINGS (not
acknowledgements), each setting in a SETTINGS after its first six,
PRIORITY, empty DATA without END_STREAM, an empty CONTINUATION, a
frame of unknown type, a frame on a closed stream (C4), a refused
stream, a WINDOW_UPDATE that leaves the window under the 1 KiB DATA
floor (C5), each RST_STREAM we send for a stream error, and a
RST_STREAM from the client before our response ended (rapid reset).
Not counted: other WINDOW_UPDATEs (a client with generous windows
sends many) and PRIORITY_UPDATE (type 0x10, RFC 9218, ignored until
RFC 9218 is supported, D8). Over budget: GOAWAY ENHANCE_YOUR_CALM.
Refused outright: a SETTINGS acknowledgement for a SETTINGS we did not
send (as Go and nghttp2), and a SETTINGS that repeats a setting or has
more than 100 (as Go; nghttp2 keeps the last value) (connection errors PROTOCOL_ERROR; Section 10.5
names "changing the same setting multiple times in the same frame").
The budget, the slots and the threads are per connection; the server's
limit on connections is what bounds them together, so its default
becomes finite.

**C11. The preface, and the client's GOAWAY.** The core expects the
client's preface (24 octets, Section 3.4) and then a SETTINGS frame;
anything else is a connection error PROTOCOL_ERROR. `newConn` returns
the server's preface as its first output: SETTINGS with D9's values
(INITIAL_WINDOW_SIZE 65,536), and a WINDOW_UPDATE on stream 0 that
raises the connection window from the protocol's 65,535 to 1 MiB (D6;
a SETTINGS cannot change it, Section 6.9.2). The driver (D2) only
recognises the preface; the core checks it. A GOAWAY from the client
(Section 6.8): no new streams are accepted, the open ones are
finished, then our own GOAWAY and the close; frames after it are not
errors.

**C12. Responses with no content, and trailers.** The HTTP/2
`ResponseHead` carries what HTTP/1.1's does (status, fields, whether
there is content, its length if known, the trailers declared). A
response with no content (HEAD, 204, 304, `ContentNone`) is a HEADERS
with END_STREAM, with the declared `content-length` for HEAD (Section
8.1.1: such a response "MAY have a non-zero content-length header
field"); `WriteBody` for it is discarded and its length not checked, as
HTTP/1.1's `HeaderOnly`. Otherwise the length is checked as HTTP/1.1
checks it: other than declared is `Abort` (RST_STREAM INTERNAL_ERROR).
With trailers, the last DATA does not carry END_STREAM and a HEADERS
with the trailers and END_STREAM follows; without, END_STREAM goes on
the last DATA, or on an empty DATA if the buffer had emptied before
`EndBody` (the driver folds `EndBody` into the last `WriteBody` of
`ContentBytes` to avoid it).

**C13. The tests.** A testing sub-library with a client-side reader of
the server's frames (the frame parser and HPACK decoder already exist)
and a generator of sessions: several streams interleaved, frames in
random sizes and split at random points, the client's settings
(including a HEADER_TABLE_SIZE and an INITIAL_WINDOW_SIZE changed
mid-connection) and window updates, handlers that respond, read and
fail in any order, a transport that takes octets slowly or not at
all, and time.

- A single-stream session has its exact outputs predicted by a
  reference model. With several streams, timing decides the
  interleaving, so the checks are relations: each stream's content
  reaches its handler once and in order, and the client gets each
  response whole; the credit returned equals the content consumed, per
  stream and for the connection; nothing is sent beyond a window or
  beyond the transport bound; frames are within the peer's size;
  GOAWAY's last stream never rises; memory held (queued content,
  buffers, receive buffers kept alive) stays within the windows and
  bounds; `nextDeadline` is never in the past after a step.
- An explicit model of Section 5.1: for each stream state and frame
  type, the expected result (accept, discard, stream error or
  connection error, and the code), checked by generated legal and
  illegal sequences.
- Each D9 attack is a generated session with its budget, and
  legitimate traffic just under it.
- The races between handler threads and the owner (a `Consumed` or
  `WriteBody` queued when a reset arrives; `HandlerDone` after `Close`)
  are tested in `hyperdrive-server`'s in-memory tests with scripted
  interleavings, since the core alone cannot show them.
- h2spec's expected failures are listed by name from its first run,
  as the llhttp differences are: the four "closed" cases of C4, and
  the two self-dependency cases of Section 5.3.1 (RFC 9113 dropped
  that rule, and the parser keeps no dependency).
- Mutants for every check.

## Other implementations (surveyed 2026-11-25)

Five questions the second review raised were settled by what other
servers do, read from their current source: Go's x/net/http2, Kazu
Yamamoto's http2 (warp), nginx, h2o, Rust's h2 and hyper, and nghttp2
with nghttpx, Apache's mod_http2 and Envoy.

- Malformed requests: the libraries (nghttp2, warp, h2, most of Go)
  send RST_STREAM PROTOCOL_ERROR alone; nginx and parts of Go,
  nghttpx and mod_http2 send 400; h2o mostly GOAWAY; Envoy closes the
  connection. No one sends a 400 and then PROTOCOL_ERROR. A response,
  when sent, ends with END_STREAM, and RST_STREAM NO_ERROR follows if
  the request is still sending (Go, nginx, nghttpx, mod_http2). An
  oversized header list: 431 (Go, h2, nghttpx, mod_http2) or a reset.
  (D10.)
- `:scheme` `https` over cleartext: all accept; nghttpx can refuse it
  with an option, off by default; warp and nginx take security from
  the transport. (D11.)
- Whitespace around values: passed through by Go, warp, nginx, h2 and
  Envoy (which turned nghttp2's check off), stripped by mod_http2,
  the field dropped by nghttp2, refused only by h2o. (D11.)
- Non-zero padding: no one checks it. (F4.)
- Buffers and windows: per-stream response buffering from 4 KiB (Go)
  to 16 MiB (Envoy); receive windows from 64 KiB (nghttpx, nginx and
  h2o before a body is read) through 1 MiB (Go, hyper) and 256 KiB
  (warp) to 16 MiB (Envoy); only mod_http2 grows windows. (D6, C5.)

## Testing

- HPACK: RFC 7541 Appendix C's examples verbatim; the hpack-test-case
  corpus (encodings of real header sets by several implementations);
  decode-after-encode for generated header lists and table sizes;
  dynamic-table size accounting checked against a simple model.
- Frames: parse-after-serialize for every type, and generated near
  misses (each length, flag and identifier rule broken once).
- `step`: generated sessions of interleaved streams, split at random
  points, against a whole-input reference model; the attacks of D9 as
  generated sessions with their budgets.
- Outside checks: h2spec (conformance), h2load (load), and a
  differential test of HPACK and frame decoding against nghttp2, in the
  way the llhttp tests are run (behind a flag, in their own shell).
- Mutants for every check, as before.

## Decisions in short

- **D1.** One package, HPACK inside, QPACK's shared parts kept separate.
- **D2.** h2c with prior knowledge only, on for cleartext listeners, never on TLS (ALPN decides).
- **D3.** Our own TLS binding through a C shim; AWS-LC first, OpenSSL 3.5 behind the same interface.
- **D4.** One owner thread per connection; a thread per request's handler.
- **D5.** The handler interface unchanged; cancellation cooperative (`ConnectionClosed`); a stream's slot held until its handler returns.
- **D6.** Credit given as the handler reads, at both levels; 64 KiB streams, 1 MiB connection; unread bodies reset with NO_ERROR.
- **D7.** No server push; the MUSTs on receiving push signals kept.
- **D8.** No RFC 7540 priorities; round robin first (changed after review), then RFC 9218 after the warp benchmarks.
- **D9.** The settings, a defence for each known attack, a nuisance budget with the coarse time as an input.
- **D10.** HPACK errors close the connection; malformed: RST_STREAM PROTOCOL_ERROR alone; refused: the response, then RST_STREAM NO_ERROR if the request is still sending (both after the survey); staged GOAWAY.
- **D11.** The pseudo-header mapping in `hyperdrive-semantics`; a differing `Host` and an invalid `:authority` malformed; other schemes and `https` on cleartext 421 unless the peer is a trusted gateway; whitespace around values stripped (after the survey).
- **D12.** HPACK, frames, D11, `step`, h2c driver, RFC 9218, TLS (HTTP/1.1 first).
- **C1-C13.** The connection core, after the author's and an independent review.
