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

- `Hyperdrive.HTTP2.HPACK` (RFC 7541): prefixed integers, string
  literals, the Huffman code, the static and dynamic tables, a decoder
  and an encoder. The integer and Huffman modules depend on nothing
  else, since QPACK (RFC 9204, Section 4.1) uses both unchanged.
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
- A stream counts against the concurrency limit until its handler has
  returned, not only until the protocol closes it: otherwise a client
  can reset and reopen streams faster than handlers end, and pile up
  running handlers (rapid reset, D9).
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

- Initial windows, both settings: 64 KiB per stream, 1 MiB per
  connection, starting points until measured. A fixed stream window
  caps one upload at a window a round trip (about 1.3 MB/s at 50 ms);
  growing a stream's window while its handler keeps up (as gRPC and
  Chrome do) is in D13, to be decided by measurement.
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
next frame), tested like the rest. The first version sends streams in
order of stream identifier: this is RFC 9218 with every stream at its
default (urgency 3, not incremental, which RFC 9218 serves one after
another), not round robin, so the later step changes no behaviour for
clients that send no signals. Its data structure is already RFC 9218's
(a queue for each urgency, in stream order), so that the next step adds
parsing and policy, not a new structure. The next step, after the
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
| INITIAL_WINDOW_SIZE | 64 KiB | D6 |
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
| floods while not reading the acknowledgements | the owner stops reading input while its unsent output is over a bound, so TCP pushes back on the client, and the write timeout closes the connection (falls out of D4) | close |
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
   characters, `TE` other than `trailers`, a `content-length` that
   differs from the DATA received): a stream error PROTOCOL_ERROR,
   and "a server MAY send an HTTP response prior to closing or
   resetting the stream": 400, then RST_STREAM PROTOCOL_ERROR, as
   HTTP/1.1 answers 400 (D3 there), so that a developer with a broken
   client library sees a reason. A `content-length` found wrong once
   the handler has started: RST_STREAM alone, and the handler's
   `readChunk` throws `IncompleteBody` as on HTTP/1.1. To check: that
   h2spec accepts the 400 before the reset; if not, these get the
   reset alone.
3. Well-formed, but refused by `mkRequest` (the target's syntax,
   userinfo, a port out of range: HTTP/1.1's checks): 400 with
   END_STREAM, a normal response, and RST_STREAM NO_ERROR if the
   request's body has not ended (D6). Not PROTOCOL_ERROR.
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
below, with the additions after it).** `:method`, `:scheme`,
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
- `:scheme`: only `http` and `https` (the RFC allows others, for
  proxies; this is an origin server); anything else is malformed. The
  target keeps the scheme the client sent, and `contextSecure` says
  what the connection is: an application that cares about `https` over
  h2c answers 421, as it would for an HTTP/1.1 absolute-form target.
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
3. Frames, with the differential test against nghttp2.
4. D11's mapping in `hyperdrive-semantics` (independent of 2 and 3,
   but HPACK settles the shape of what it receives).
5. `step`: stream states, flow control, settings, errors, the scheduler;
   a reference model and generated sessions, as for HTTP/1.1.
6. The driver over h2c (D2, D4); h2spec; h2load against warp. Then the
   rest of RFC 9218 (D8).
7. TLS (D3): HTTP/1.1 over TLS first, benchmarked against warp-tls, so
   that TLS's own cost is measured on the protocol already known; then
   HTTP/2 through ALPN, and benchmarks over TLS.

**D13. Later.** Extended CONNECT (RFC 8441, WebSockets over HTTP/2; RFC
9220 for HTTP/3), 103 Early Hints (an interim response in the
application interface, for every version; D7), the
inline-handler setting (D4), parked stream threads (D4), receive windows
that grow while a handler keeps up (D6).

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
- **D8.** No RFC 7540 priorities; stream order first, then RFC 9218 after the warp benchmarks.
- **D9.** The settings, a defence for each known attack, a nuisance budget with the coarse time as an input.
- **D10.** HPACK errors close the connection; malformed 400 then PROTOCOL_ERROR; invalid 400; staged GOAWAY.
- **D11.** The pseudo-header mapping in `hyperdrive-semantics`; a differing `Host` malformed; `http` and `https` only.
- **D12.** HPACK, frames, D11, `step`, h2c driver, RFC 9218, TLS (HTTP/1.1 first).
