# The HTTP/2 driver over h2c (step 6 of D12): V1-V6 decided as recommended, 2026-11-26

The connection core (`Hyperdrive.HTTP2.Connection`, C1-C13 and parts
1-6 in docs/http2-core-design.md) is pure and done. This note proposes
the IO around it in `hyperdrive-server`, next to the HTTP/1.1 driver
(docs/server-design.md). The user decided V1-V6 as recommended after
the survey of ten servers below (2026-11-26), keeping D10's forced
stage and a `flush` that does nothing. What is already decided and only applied
here: h2c with prior knowledge on cleartext listeners, never on TLS
(D2); one owner thread per connection and a thread per request's
handler (D4); the handler interface unchanged, cancellation through
`ConnectionClosed`, a reset stream's slot held until its handler
returns (D5); a dead connection's handlers killed after 5 s (C2). The
questions are marked **V**, each with a recommendation.

## The owner never blocks on a send (V1)

The core paces DATA by what the transport reports written (`Sent`,
C5), and expects to keep answering PINGs, SETTINGS and WINDOW_UPDATEs
while a client is slow to read. The HTTP/1.1 driver's `sendMany`
blocks until everything is written; on the owner thread that would
stall every stream behind one slow socket, and `Sent` would only ever
report everything at once.

**V1.** `Transport` gains a non-blocking send and a writable
notification, as it already has for reading:

```haskell
  , transportTrySend    :: [ByteString] -> IO Int     -- octets the socket took now, never waits
  , transportOnWritable :: IO () -> IO (IO ())        -- once, when a send would take something
```

The owner keeps the octets the core emitted in a queue, sends what the
socket takes, feeds `Sent n`, and when the queue is not empty also
waits for writability in its one wait (D4: "waiting for writability in
the same wait as readability"). The HTTP/1.1 driver is unchanged.
Recommended. (Not chosen: a writer thread per connection, which D4
ruled out; `sendMany` with a timeout, which blocks the owner.)

A connection-level write timeout remains: queued output with no
progress for `timeoutWrite` (30 s) closes the connection. The core's
per-stream minimum rate already covers slow reading per stream; this
catches a client that stops reading everything, control frames
included.

## The owner's wait (V2)

One `MVar` per connection, as now, which anything that needs the owner
fills with `tryPutMVar`: the socket readable, the socket writable, the
timer, shutdown, and a handler with something to say. What handlers say
goes in a list in an `IORef` (`atomicModifyIORef'` to add, swapped out
whole by the owner), so a burst of messages costs one wake-up.

**V2.** `Wake` gains `WakeWritable` and `WakeHandler`, and the handler
messages are an `IORef [Message]` beside the `MVar`. Recommended over a
`TQueue` (STM over shared variables was a bottleneck before, docs/
benchmarks.md) and a `Chan` (two `MVar`s per message).

The loop: take everything handlers said and feed it (`Respond`,
`WriteBody`, `EndBody`, `Abort`, `WantContent`, `Consumed`,
`HandlerDone`); try a receive and feed it; send what is queued and feed
`Sent`; route the outputs; arm the timer for `nextDeadline`; wait.
Inputs are fed in the order they were taken, so a handler's
`WriteBody` and its `EndBody` keep their order.

## The handler side (V3)

Each stream gets a small record the owner and its handler share: an
`MVar ()` to wake the handler, and an `IORef` with what the owner has
for it (content, the end and trailers, a `Writable`, an abort).

- `readChunk`: take queued content (and say `Consumed` for it, which
  returns the credit, D6); with nothing queued, say `WantContent` and
  wait. After `RequestEnd`, `""`; after `RequestAborted`,
  `IncompleteBody` for content not as declared, `ConnectionClosed`
  otherwise (D5).
- A response: `checkResponseFor`, `Date` added, then `Respond`.
  `ContentBytes` is one `WriteBody` (and `EndBody`); `ContentFile` is
  read in 16 KiB pieces (D5: no `sendfile` under HTTP/2); a
  `ContentStream`'s write is `WriteBody` and waits for its `Writable`
  (every write gets exactly one, C5).
- **V3. A `ContentStream`'s `flush` does nothing on HTTP/2:** the core
  sends what a write gives it as soon as the windows and the transport
  allow, without waiting for more, so there is nothing held back to
  flush. Recommended. (The alternative, waiting until the stream's
  buffer has gone to the socket, would make a server-sent-events
  handler wait on its slowest client for no gain.)
- Exceptions from the handler: before `Respond`, reported and the core
  answers 500 on `HandlerDone`; after, `Abort` (RST_STREAM
  INTERNAL_ERROR). `HandlerDone` always, in a `finally`.

## Handler threads (V4)

Forked with `forkIOWithUnmask` by the owner on `NewRequest`, kept in a
map by stream. When the connection ends (the core said `Close`), the
owner has already told every waiting handler (`RequestAborted
ConnectionEnded`, C2); it keeps the connection (and its place in the
server's count, C2) until the handlers have returned, and kills those
still running after a grace period with `killThread`. If the owner
itself is killed (the server's forced shutdown), its handlers are
killed with it, so no handler outlives its connection.

**V4.** `Settings` gains `settingsHandlerGrace` (5 s, C2's period).
Recommended.

The optional handler time limit (`settingsHandlerTimeLimit`) applies
per handler thread, as on HTTP/1.1: before `Respond`, a 503 is sent;
after, `Abort`. On HTTP/2 this ends only that stream, not the
connection.

## Time, settings and shutdown (V5)

- The core's `Time` (milliseconds) is the timers' clock, which the
  ticker keeps (nanoseconds, read from an `IORef`); `nextDeadline` is
  armed as the read deadline. The core's timeouts are in its own
  `Config` (C8), so HTTP/1.1's `Timeouts` by phase do not apply,
  except `timeoutWrite` (above) and `timeoutLinger` (the staged close).
- **V5.** `Settings` gains `settingsHTTP2 :: Maybe Config` (the core's
  configuration; `Just defaultConfig` by default; `Nothing` turns h2c
  off, D2's "on by default on cleartext listeners"). Recommended.
- Shutdown: the first wake with `WakeShutdown` feeds `Shutdown`
  (GOAWAY, PING, the final GOAWAY); at the end of the server's grace
  period, the owner is fed `Shutdown` again (the core then resets
  what is left and closes, part 5) before the forced stage kills
  threads. This needs the server to signal the grace period's end to
  each connection before killing it: a second `WakeShutdown`, through
  the same registry. HTTP/1.1 ignores it.

## Which protocol (V6)

**V6.** `serveConnection` reads the connection's first octets before
choosing a driver: fewer than 24 octets that are a prefix of the
preface wait for more (under the head timeout); the whole preface goes
to the HTTP/2 driver; anything else goes to the HTTP/1.1 driver as its
first input. So the HTTP/1.1 driver gains a way to start with octets
already read, which the dispatcher of "Later" (docs/HANDOFF.md: serve
an accepted socket starting with these octets) needs too.
Recommended. The cost on an HTTP/1.1 connection is one comparison of
its first octets.

## Requests

`NewRequest` gives D11's `RequestControl`: `mkRequest (HttpVersion 2
0)` with its method, target and fields, a `RequestBody` whose length is
the declared `content-length` (or 0 when no content follows), and
`setAcceptsTrailers`. The `Context` is the connection's.

## Testing

- In memory: a client built from the package's own frame renderer,
  parser and HPACK, driving `serveConnection` over the in-memory
  transport: requests and responses of every kind, bodies both ways,
  many streams at once, a handler that fails before and after
  responding, a client that resets streams, one that stops reading
  (the transport taking nothing), shutdown in both stages, the 5 s kill.
- Over loopback: the same sessions on real sockets.
- h2spec (`nix develop .#h2spec`, h2spec 2.6.0): every case, with the
  expected failures listed by name, as C13 says (the four "closed"
  cases of C4, the two self-dependency cases of Section 5.3.1) and any
  others investigated before being listed.
- h2load (nghttp2 1.69) against warp's h2c, in the bench shell, with
  the user's go-ahead for timings.
- Mutants for the driver's checks.

## Not in this step

TLS and ALPN (step 7); RFC 9218's priorities (after the warp
benchmarks, D8); the differential test of frames against nghttp2 (it
needs a running server; after h2spec passes).

## What other servers do (surveyed 2026-11-26)

From the current source of Go (net/http 1.27.4, whose HTTP/2 server
moved into `net/http/internal/http2`), Rust's hyper with h2, warp with
Kazu Yamamoto's http2 (master), Kestrel, nginx, h2o, nghttpx with
nghttp2, Envoy, Netty and Jetty 12.1, each read for V1-V6.

- **V1, writing.** No event-loop server ever blocks its connection on a
  write: nginx, h2o, nghttpx, Envoy, Netty and Jetty write what the
  socket takes and wait for writability; hyper does the same in one
  task (and stops reading frames while its write buffer is full). Go's
  owner never blocks either, starting a goroutine for each large frame
  write. warp alone has a sender thread that blocks in `sendAll`.
  Learning what was written: Netty's flow controller hands out only
  what the socket has room for (`bytesBeforeUnwritable`, 64 KiB
  watermark), the nearest to our `Sent` pacing; Envoy releases its
  frame-flood counters only when the octets leave its buffer, as part 6
  does; the others count a frame as sent when it is in their buffer. A
  write timeout on no progress: nginx 60 s (connection, and per stream
  as RST_STREAM), h2o 60 s, nghttpx 30 s (armed as soon as output is
  queued, not only after a partial write), warp 30 s for each write,
  Jetty its 30 s idle timeout, Go's `WriteByteTimeout` off by default,
  hyper and Envoy none. Kestrel instead aborts the whole connection
  below 240 B/s. V1 as proposed is the majority's design.
- **V2, handlers to the owner.** Go: buffered channels, one message a
  wake-up, batching only on the output side. hyper: a shared mutex and
  the connection task's waker, every pending stream drained per
  wake-up. warp: STM `TQueue`s. Kestrel: a bounded channel onto which
  each stream schedules itself at most once. Jetty: a locked queue, and
  the calling thread runs the flusher itself if it is idle. A list in
  an `IORef` with one `MVar` (V2) batches as hyper's and Jetty's do,
  without STM.
- **V3, flush.** Two camps. The producer waits until its octets are in
  the kernel: nginx (zero-copy buffers returned once written), h2o
  (`proceed` after the write completes), Netty (the write's future),
  Jetty (the callback; one outstanding write per stream). It waits only
  until the connection has the octets: Go (`Flush` blocks until the
  owner has framed the DATA, window waits included), warp (a hint to
  send now), Kestrel (the stream's 4 KiB pipe), nghttpx (a 128 KiB
  stream buffer), Envoy, hyper. No one's flush is an outright no-op,
  but in the second camp it adds nothing to a write that is already
  bounded, as ours is (a write waits for `Writable` once 16 KiB is
  buffered). V3 stands. The alternative closest to Go's would be a
  flush that waits until the stream's buffer has been framed (emitted
  as DATA).
- **V4, handlers.** On a reset, cancellation is cooperative everywhere:
  Go cancels the context, Kestrel a token, warp and Netty fail the next
  read or write, hyper drops the future at its next await, the event
  loops abort the generator at once. When the connection ends: warp
  kills the application's threads with asynchronous exceptions;
  hyper drops every stream's future (the same in effect); Go and
  Kestrel only cancel, and Kestrel waits for handlers with no limit.
  C2's 5 s and then `killThread` sits between warp and Kestrel. A time
  limit per handler, where there is one, ends the stream only (Go's
  `WriteTimeout`, nginx).
- **V5, configuration and shutdown.** h2c is on by default in warp,
  Envoy, h2o and nghttpx, opt-in in Go, nginx, Kestrel (whose default
  never serves cleartext HTTP/2), Netty and Jetty; D2 chose on. The
  GOAWAY 2^31-1 and PING of D10: Rust's h2 and warp; h2o and nghttpx
  send the second GOAWAY on a fixed timer (1 s, 2 s) without a PING;
  Kestrel, Envoy and Jetty send GOAWAY 2^31-1 and the real one when the
  streams are done or a drain timer (Envoy 5 s) fires; Go and nginx
  send a single GOAWAY with the real last stream. At the end of the
  grace period, no one sends RST_STREAM CANCEL and a GOAWAY: nginx,
  h2o, Go, hyper, Netty and Kestrel close the socket outright, and
  Jetty sends a GOAWAY with an error and fails the streams. D10's
  forced stage (CANCEL for each stream left, GOAWAY, the staged close)
  is more than anyone does; it costs the second `WakeShutdown` of V5.
- **V6, which protocol.** Compare what has arrived with the preface and
  wait while it is a prefix: nginx, nghttpx, Netty, hyper-util (octet
  by octet, so an HTTP/1.1 client is told apart at its first octet), Go
  (peeks 14 octets, then 24, and never reads past the preface). Envoy
  looks at 12 octets of the first read and ignores a partial prefix;
  warp at 4 (`"PRI "`); Jetty lets its HTTP/1.1 parser read `PRI *
  HTTP/2.0` as a request line; Kestrel chooses by configuration only.
  V6 is nginx's, nghttpx's, Netty's and hyper's. `Upgrade: h2c`
  (left out by D2) is kept by h2o, nghttpx, Netty and Jetty, and
  absent from nginx, Go's net/http, hyper, warp, Kestrel and Envoy.

Changed by the survey: nothing in V1-V6. Taken from it: the write
timeout is armed as soon as output is queued and restarted on each
progress (nghttpx, Go's `WriteByteTimeout`), not only after a partial
write.

## h2spec's first run (2026-11-26), and two questions it raised

h2spec 2.6.0 against the bench application over h2c: 132 of 146
passed. Each failure was read in h2spec's source. Six were expected
(C13: the four "closed" cases of C4, and 5.3.1's two self-dependency
cases). Two follow from decisions: 3.5/2 sends `INVALID CONNECTION
PREFACE`, which D2 and V6 give to HTTP/1.1 (it answers 400); 6.5.3/1
repeats INITIAL_WINDOW_SIZE in one SETTINGS, which C10 refuses (as Go
does). Two questions came from the other six.

**Q1. The 1 KiB DATA floor (C5).** Five cases close the client's
window, open it by one octet, and wait for a one-octet DATA frame,
which C5's floor holds back. Of ten servers read (Go 1.27.4, Rust's
h2, Kazu Yamamoto's http2, Kestrel, Netty 4.2, nginx, h2o, nghttp2,
Envoy, Jetty 12.1), none has a floor: each sends min(the windows,
the frame size), however small (Go `FrameWriteRequest.Consume`, h2
`Prioritize::pop_frame`, http2 `outputAndSync`, nghttp2
`nghttp2_session_enforce_flow_control_limits`, nginx
`ngx_http_v2_send_chain`, ...; Netty's 1 KiB `minAllocationChunk` is a
fairness quantum, clipped by the window). None has a defence aimed at
Section 10.5's tiny WINDOW_UPDATEs: Envoy's ratio of updates to DATA
frames never trips when each update draws a frame, nginx's ratio of
non-payload to payload octets trips only after megabytes, Kestrel's
minimum rate limits slowness and not frames. C10's charge for a
WINDOW_UPDATE that leaves a window under 1 KiB is already more than any
of them does, and bounds the tiny frames a client can draw to the
budget's rate.

**Q2. HEADERS on a lower stream identifier with no state.** h2spec
5.1.1/2 opens stream 5, then sends HEADERS on 3, which Section 5.1.1
makes a connection error PROTOCOL_ERROR; C4 discards it, since a
HEADERS on a lower identifier may be trailers that crossed our
RST_STREAM. The same ten:

| | a skipped identifier | trailers after our RST_STREAM | state kept |
|---|---|---|---|
| Go | connection PROTOCOL_ERROR | ignored until the reset is written, then connection PROTOCOL_ERROR | none |
| Rust h2 | connection PROTOCOL_ERROR | ignored while kept | streams it reset: 5 s, at most 2 x MAX_CONCURRENT_STREAMS (at least 50) |
| Kazu's http2 | connection PROTOCOL_ERROR | ignored (block decoded) | the last 1,000 streams it reset (`resetByMe`), no time limit |
| Kestrel | connection STREAM_CLOSED | taken as trailers during a 5 s drain, then connection STREAM_CLOSED | completed streams, 5 s, at most 200 |
| Netty | stream STREAM_CLOSED | ignored until the reset is written, then stream STREAM_CLOSED | none |
| nginx | connection PROTOCOL_ERROR | connection PROTOCOL_ERROR (refuses request trailers outright) | none |
| h2o | connection STREAM_CLOSED | connection STREAM_CLOSED | none |
| nghttp2 (and Envoy by default) | ignored | ignored | none (closed-stream retention removed) |
| oghttp2 (Envoy's option) | connection PROTOCOL_ERROR | connection PROTOCOL_ERROR once the stream is gone | none |
| Jetty | connection STREAM_CLOSED | connection PROTOCOL_ERROR | none |

Nine of ten make a skipped identifier a connection error; only nghttp2
ignores it. Two (h2, Kazu's http2) remember the streams they reset so
that crossing trailers are not a connection error.

**Decided 2026-11-27 (the user), as recommended:** Q1, the floor is
dropped and C10's charge kept (C5); Q2, the core remembers the latest
1,000 streams it reset and discards field blocks on those, and any
other HEADERS on a lower identifier is a connection error (C4). While
building Q2: a stream the client reset whose handler still runs gets
the same answer as one whose handler has returned (STREAM_CLOSED, RFC
9113 Section 5.1's MAY), so the answer does not depend on timing.
h2spec then: 140 of 146, and 141 once repeated settings were taken in order (C10, changed after the differential test against nghttp2). The five left are decided differences: 3.5/2
(D2), "closed" 8 and 11 (DATA on closed streams discarded, C4), 5.3.1's
two (a rule RFC 9113 dropped).

## Questions in short

- **V1.** A non-blocking send and a writable notification in
  `Transport`; the owner never blocks; a connection write timeout on
  no progress.
- **V2.** `Wake` gains `WakeWritable` and `WakeHandler`; handler
  messages in an `IORef` list beside the connection's `MVar`.
- **V3.** A `ContentStream`'s `flush` does nothing on HTTP/2.
- **V4.** `settingsHandlerGrace` (5 s); handlers killed with their
  connection.
- **V5.** `settingsHTTP2 :: Maybe Config`, on by default; a second
  `WakeShutdown` at the end of the grace period.
- **V6.** Protocol chosen from the first octets in `serveConnection`;
  the HTTP/1.1 driver can start with octets already read.
