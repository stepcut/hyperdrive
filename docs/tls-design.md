# TLS in the server (step 7 of D12)

Status: T1-T10 decided as recommended after Fable's review (the user,
2026-11-27).

D3 (docs/http2-core-design.md) chose our own binding over AWS-LC; the
spike (docs/tls-spike-design.md, ca9a4ef) showed it works, with the
handshake's steps as `safe` calls. Step 7 puts it in the server:
"HTTP/1.1 over TLS first, benchmarked against warp-tls, so that TLS's
own cost is measured on the protocol already known; then HTTP/2 through
ALPN, and benchmarks over TLS." This note is how.

## What is there to build on

- Both drivers reach the network only through a `Transport` record
  (hyperdrive-server, `Hyperdrive.Server.Transport`): `recv`,
  `tryRecv`, `tryRecvLarge`, `onReadable`, `sendMany`, `trySend`,
  `onWritable`, `shutdownWrite`, `abort`, `close`.
- Each connection's transport is used from one thread: HTTP/1.1's
  driver runs the connection on its own thread, HTTP/2's owner thread
  does all its I/O (D4, V1). A TLS session, which is not safe to share
  between threads, fits as it is. (`abort` is called from the timer's
  thread, to unblock a write; over TLS it only shuts the socket down.)
- `serveConnection` chooses the protocol from the first octets (h2c's
  preface, D2); `Context` already has `contextSecure`, which sets the
  request's scheme to `https`.
- The shim (`hyperdrive-tls`): handshakes through buffers, ALPN, SNI,
  a server and a client side.
- warp-tls 3.4.14 is in nixpkgs, on Kazu Yamamoto's `tls`; wrk and
  h2load speak TLS; h2spec has `--tls`.

## Decisions

The first draft was reviewed by Fable (2026-11-27); T1, T2, T3, T4, T5,
T6, T7, T8 and T10 below take in its findings, checked against the
code and AWS-LC's headers.

**T1. TLS as a `Transport` over the socket's.** A function in
hyperdrive-server, `tlsTransport`, that runs the handshake over a
socket's transport and gives a `Transport` carrying plaintext, and what
was agreed (ALPN, version, group). The drivers' loops do not change;
`serveConnection` gains a path where the protocol is given by ALPN
(T2) instead of read from the first octets.

- Receiving: `SSL_read` first; only when it wants more, one receive
  from the socket, fed to the session, and `SSL_read` again. So
  `tryRecv` gives `Nothing` only when the session holds no plaintext
  and at most part of a record, and waiting for the socket's
  readiness is then right; `onReadable` is the socket's. (Not
  `SSL_has_pending`, which is also true of a partial record, and would
  make HTTP/2's loop spin.) One `SSL_read` gives at most a record (16
  KiB), so `tryRecvLarge` reads records into a fresh buffer sized from
  what is waiting; the whole allocation is still what is kept (C3), but
  the plaintext is copied once from the library, which the doc string
  will say.
- Anything a read or a handshake step leaves to send (an alert, the
  reply to a client's KeyUpdate) joins the ciphertext waiting to be
  sent, on the same path as writes.
- Sending: plaintext is encrypted and its ciphertext written. For
  `trySend`, which must never block (V1), the number reported is only
  the plaintext whose ciphertext the socket has taken. Ciphertext not
  taken is kept, with how much plaintext it covers; the driver offers
  the same octets again (it drops only what was reported), and the
  transport skips the part already encrypted. So HTTP/2's core counts
  as sent only what left the socket (C5's pacing holds), and the
  driver's writability wait and write timeout stay armed while
  anything waits. The plaintext encrypted in one call is bounded (64
  KiB).
- A clean `close_notify` from the client is the end of input; a TLS
  error is an error (its alert sent, then the close).
- `abort`, from the timer's thread, only shuts the socket down; the
  session is only touched from the connection's I/O thread.
- Every shim call returns its error as a value: the library's error
  queue is per OS thread, and a Haskell thread making `safe` calls may
  move between OS threads.

Recommended. (Not chosen: TLS inside each driver.)

**T2. The protocol from ALPN.** On a TLS listener, `h2` gives HTTP/2
(its preface still checked by the core), `http/1.1` or no ALPN gives
HTTP/1.1, and nothing else: no h2c on TLS (D2; RFC 9113 Section 3.3:
"HTTP/2 connections over TLS MUST use protocol negotiation in TLS
[TLS-ALPN]", and Section 3.2: "The "h2c" protocol identifier MUST NOT
be sent by a client or selected by a server"). `h2` is offered only if
`settingsHTTP2` is on (the shim's ALPN list from the configuration); a
client offering only `h2` to a server without it gets
`no_application_protocol`. RFC 9113 Section 9.2 is met by
construction: compression is not available in AWS-LC, a server rejects
every renegotiation, SNI is read, no client certificates (so no
post-handshake authentication), and TLS 1.2's ciphers are only ECDHE
with AEAD for the whole listener, so `h2` over a prohibited suite
cannot happen. The consequence, chosen deliberately (the user,
2026-11-27): a TLS 1.2 client with none of those ciphers cannot
connect, even for HTTP/1.1, though Section 9.2.2 would allow HTTP/1.1
over a prohibited suite; current clients all have them. Our order of
preference applies (`SSL_OP_CIPHER_SERVER_PREFERENCE`). Recommended.

**T3. Settings.** `settingsTLS :: Maybe TLSSettings`: the certificate
chain and the key, as files or as PEM octets; the key-exchange groups
(default X25519MLKEM768, X25519, P-256: the spike found AWS-LC puts the
hybrid first); the lowest version (TLS 1.2); the cap on handshakes in
progress (T4). Checked at startup: the key must match the leaf
certificate's public key (compared by us), an encrypted key is refused
(else the library would ask for a passphrase), and the signing
algorithms offered come from the key's type (P-256, P-384, RSA, Ed25519),
not a fixed list. One listener per `Settings`, as now (a server on 80
and 443 runs two; the connection limit is each listener's). One
certificate per listener in the first version. Shaped so that QUIC's
listener reuses it (TLS 1.3 only there). `Settings` gains a field;
`Context` gains nothing yet. Recommended; a public API change, so the
user's call.

**T4. A handshake timeout.** `Timeouts` gains `timeoutHandshake`: a
total deadline from the connection's accept to the handshake's end, not
an idle timer, so that a client trickling octets cannot hold a
handshake open; 10 s by default. On expiry the connection is closed,
without an alert. A connection in its handshake counts toward the
connection limit from its accept (the handshake costs the server 85 to
470 µs of CPU, the spike). Recommended; a public field, so the user's
call. What other servers do (surveyed 2026-11-27 from their source and
documentation; below) supports each part.

| server | default | measures | name |
|---|---|---|---|
| h2o | 10 s | total, from accept (PROXY protocol included) | `handshake-timeout` |
| Kestrel | 10 s | total | `HandshakeTimeout` |
| Netty | 10 s | total, from the handshake's start | `setHandshakeTimeoutMillis` |
| axum-server (rustls) | 10 s | total | `handshake_timeout` |
| warp-tls 3.4.19 | 30 s | total, applied to the first read and again to the handshake (up to about 60 s) | `settingsTimeout` |
| nginx (http) | 60 s, shared | total, from accept (one timer) | `client_header_timeout` |
| nginx (stream) | 60 s | total | `ssl_handshake_timeout` |
| Pingora | 60 s | total | `handshake_timeout` (main; hard-coded in 0.9.1) |
| Caddy | 1 min | total (Go's rule, below) | `read_header_timeout` |
| HAProxy | `timeout client`, itself unset by default (infinite, with a warning) | total, from accept | `timeout client-hs` |
| Go net/http | none | total: the least of ReadHeaderTimeout, ReadTimeout, WriteTimeout set | shared |
| Envoy | none | total | `transport_socket_connect_timeout` (scaled down under overload) |
| tokio-rustls, hyper-rustls | none | the caller's ("wrap this in a timeout") | |
| Apache httpd | stage off; core `TimeOut 60` | idle (or a rate, with `MinRate`) | `RequestReadTimeout handshake=` |
| Node.js | 120 s | idle, despite its name (`socket.setTimeout`) | `handshakeTimeout` |
| Jetty 12 | idle timeout 30 s | idle | `idleTimeout` |
| lighttpd | 60 s | idle | `server.max-read-idle` |

Most use a total deadline; the idle timers (Node.js, Jetty, lighttpd,
Apache's core) let a slow client keep a handshake alive. Dedicated
handshake timeouts are 10 s (four of them) or 60 s. None of those read
sends an alert on expiry; where visible, a handshaking connection counts
toward the connection limit. Envoy's timer shortened under load is not proposed.

With the handshake's steps `safe` (T5), a burst of ClientHellos becomes
as many OS threads doing CPU-bound work in C (an RSA-2048 signature
about 0.36 ms), competing with the capabilities. So, as Pingora's
`max_pending_handshakes`: a cap on handshakes in progress, a semaphore
taken before the first step and given back at the handshake's end, its
failure or its timeout; a few per capability by default (a setting). A
connection waiting for it is still within its handshake timeout.
(Fable's review.) Whether concurrent signing with one shared key is
safe in AWS-LC is to be checked in step 1.

**T5. Calls and threads.** All of a session's calls from its
connection's I/O thread (T1); the handshake's steps `safe`, record work
`unsafe` (2 to 3 µs a record, the spike). With the steps `safe`, the
asynchronous private-key method buys nothing (it existed to keep the
signature out of `unsafe` calls), so it is dropped: the library signs
with the key itself, inside the step (the user, 2026-11-27, on Fable's
review). That also removes the BoringSSL-only `SSL_PRIVATE_KEY_METHOD`,
which T9's OpenSSL would not have. An RSA signature (about 0.36 ms)
then blocks only its connection's thread. Recommended.

**T6. Closing.** A graceful close sends TLS's `close_notify` (the first
stage of `SSL_shutdown`, which AWS-LC's header says most callers should
stop at), drained and written under the write timeout as the drivers'
staged close is now, then shuts the socket's writing down; lingering
reads until the client's `close_notify` or the socket's end. An abort
only closes. A client's missing `close_notify` is not an error: HTTP's
own framing says where messages end. Recommended.

**T7. Not in the first version, and AWS-LC's defaults pinned.**
Resumption is off: no TLS 1.3 tickets (`SSL_OP_NO_TICKET`; AWS-LC
issues them by default, as the spike saw) and no TLS 1.2 session cache
(`SSL_SESS_CACHE_OFF`). It can come back cheaply later: AWS-LC rotates
its default ticket key every two days
(`SSL_DEFAULT_TICKET_KEY_ROTATION_INTERVAL`), enough for one process.
0-RTT stays off (AWS-LC's default without opting in). Our cipher and
group order applies (T2). The record size stays AWS-LC's (16 KiB); ECH
(experimental in AWS-LC) is not used. Also not yet: client
certificates, OCSP stapling, reloading certificates, several
certificates by SNI, kTLS. Recommended.

**T8. Benchmarks.** The bench application served over TLS by both
servers, hyperdrive and warp-tls, with the same certificates (ECDSA
P-256 and RSA-2048) and neither resuming sessions: wrk over HTTPS
(HTTP/1.1, keep-alive), h2load over TLS (h2 by ALPN; m1, m10, m64,
large, upload as `scripts/compare-h2`), and a handshake rate (a new
connection for each request). Each run records the version, cipher and
group each server negotiated (Kazu's `tls` may not offer X25519MLKEM768,
in which case the handshake rate compares different key exchanges, and
the table says so), and the CPU a request costs. Also: resident memory
at 10,000 idle TLS connections, for both. These are the "faster than
Kazu's" comparison over TLS. Timing runs wait for a quiet machine.
Recommended.

**T9. OpenSSL 3.5.** Behind the same C interface (a second C file
chosen at build time), once HTTP/1.1 and HTTP/2 over TLS work on
AWS-LC; the benchmarks pick the default, as D3 says. Simpler after T5.
Recommended at the end of step 7.

**T10. Tests and assurance.** In hyperdrive-server's tests: a TLS
client from the shim's client side (as `H2Client` is HTTP/2's), for
handshakes, ALPN's choice, requests over both protocols, the handshake
timeout and the cap, `close_notify`, `trySend` under a socket that takes
little (nothing stranded, the write timeout armed), garbage after the
handshake (an alert, no crash), receives of one octet, a client that
stalls after each flight, a client's KeyUpdate, a burst of handshakes,
and sessions freed on every path, exceptions included. h2spec over TLS
(`scripts/h2spec --tls`). The shim brought up to the project's
standard (S5): tests, mutants, citations. In duvet: RFC 9113 Section
9.2's requirements (already there, now implementable) and RFC 7301
(ALPN) added for the server's part; RFC 8446 not added, its
requirements being the library's. Browsers by hand; tlsfuzzer or BoGo
later. Recommended.

## Also noted (from the review)

- Memory per connection: the session and its record buffers (AWS-LC
  releases them while idle), two memory BIOs, the socket's 16 KiB
  buffer; measured at 10,000 connections (T8).
- The key is in the process's memory either way; file permissions and
  no core dumps are the realistic protection, for the deployment notes.
- HTTP/3 will not go through `Transport` (QUIC encrypts its own
  packets); what it shares is the TLS configuration (T3) and the shim.
- kTLS, if it comes, would be another transport (the socket with the
  traffic keys installed in the kernel), not a change to this one.

## Order of work

1. The shim for production: errors as values, the asynchronous key
   method dropped, limits on its buffers, ALPN, groups and versions
   from settings, resumption off and our preference order, the key
   checked against the certificate, tests and mutants.
2. `tlsTransport` and HTTP/1.1 over TLS (T1-T6), with the tests.
3. Benchmarks against warp-tls over HTTP/1.1 (T8), quiet machine.
4. HTTP/2 through ALPN, h2spec over TLS, the h2load comparisons.
5. OpenSSL 3.5 behind the interface, and the benchmarks that pick the
   default (T9).

## As built: step 1, the shim (2026-11-27)

`hyperdrive-tls` rewritten: errors as values (each failing call keeps
its own message); the asynchronous key method gone, the library signing
with the key, which must be the certificate's; certificate chain and
key from PEM octets (an encrypted key refused); ALPN, groups, versions
(and, for test clients, TLS 1.2 ciphers) from the configuration;
resumption off (`SSL_OP_NO_TICKET`, `SSL_SESS_CACHE_OFF`) and
`SSL_OP_CIPHER_SERVER_PREFERENCE`; a bound on ciphertext buffered;
close_notify told from an error; `Hyperdrive.TLS`, a Haskell API whose
configurations and sessions are freed by finalizers. Tests (14,
`hyperdrive-tls-tests`) with committed test certificates (ECDSA P-256
and P-384, RSA-2048, Ed25519, an encrypted key, a mismatched key, a
chain). Found on the way, all as AWS-LC documents: in TLS 1.2 an ECDSA
certificate's curve must be one the client offers (a P-384 certificate
fails with `BAD_ECC_CERT` against a client offering only P-256), and
the ECDSA code points do not tie curve to hash; AWS-LC's client accepts
no Ed25519 signature unless asked (browsers none at all); a KeyUpdate,
and the reply to one, go with their sender's next write (`SSL_key_update`
"does not _send_ the message itself"), which RFC 8446 allows for the
reply; TLS 1.3 tickets would go in the server's flight with its
Finished. Two checks of ours turned out to be AWS-LC's as well (the key
matching the certificate, QUIC's handshake-flight bound); kept.

## Decisions in short

- **T1.** TLS as a `Transport` over the socket's: decrypt what is held
  before receiving; send what reads leave; report only plaintext whose
  ciphertext left the socket; errors as values; a "protocol from ALPN"
  path in `serveConnection`.
- **T2.** The protocol from ALPN; no h2c on TLS; TLS 1.2 only ECDHE with
  AEAD, even for HTTP/1.1; our preference order.
- **T3.** `settingsTLS` (certificate and key as files or PEM, groups,
  lowest version, handshake cap), checked at startup; one certificate
  per listener.
- **T4.** A handshake timeout (total from accept, 10 s, closed without
  an alert, counted toward the connection limit), and a cap on
  handshakes in progress.
- **T5.** Calls from the connection's I/O thread; handshake steps
  `safe`, records `unsafe`; the asynchronous key method dropped.
- **T6.** `close_notify` on a graceful close, under the write timeout;
  its absence from a client not an error.
- **T7.** Resumption off (tickets and the session cache), 0-RTT off,
  our preference order; not yet: client certificates, OCSP, reloading,
  SNI's choice, kTLS.
- **T8.** Benchmarks against warp-tls (HTTPS, h2, handshake rate), the
  negotiated group recorded, memory at 10,000 idle connections.
- **T9.** OpenSSL 3.5 behind the same interface at the end; benchmarks
  pick the default.
- **T10.** A TLS test client and the robustness cases, h2spec over TLS,
  the shim brought up to standard, RFC 9113 Section 9.2 and RFC 7301 in
  duvet.
