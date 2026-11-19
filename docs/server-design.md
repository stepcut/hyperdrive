# The server: IO driver for HTTP/1.1 (S1 to S7 decided as recommended, 2026-11-18)

The protocol core (`Hyperdrive.HTTP1.Connection.step`) is pure and
done. This document proposes the IO around it: sockets, timers, running
the application, and turning `Request`/`Response` into `step`'s inputs
and outputs. Questions for the user are marked **S**.

## Shape

A new package, **`hyperdrive-server`** (S1), depending on
`hyperdrive-http1`, `hyperdrive-semantics`, `network`, `stm` and
`bytestring`. HTTP/2 and HTTP/3 drivers, and ALPN, will live here
later, so the protocol packages stay free of IO.

```haskell
type Application = Context -> Request -> IO Response          -- S2

data Context = Context                                         -- M8
  { contextPeer   :: SockAddr
  , contextLocal  :: SockAddr
  , contextSecure :: Bool }

runServer       :: Settings -> Application -> IO ()     -- listen, accept, serve until shut down
serveConnection :: Settings -> Transport -> Context -> Application -> IO ()

-- what a connection is carried over: a socket now, TLS later (S5)
data Transport = Transport
  { transportRecv          :: IO ByteString        -- "" at end of input
  , transportSendMany      :: [ByteString] -> IO ()
  , transportWaitReadable  :: IO (STM ())          -- see Waiting
  , transportShutdownWrite :: IO ()
  , transportClose         :: IO () }
```

`serveConnection` is the whole driver for one connection; `runServer`
is the accept loop around it. Tests use `serveConnection` with an
in-memory transport as well as real sockets.

## One thread per connection

Each connection runs in one Haskell thread (cheap in GHC, as warp
does). The application's handler runs in that same thread, and so do
`readChunk` and a `ContentStream`'s write and flush. So the
connection's `Conn` lives in an `IORef` that only one thread touches:
no locks, and the order of `step` inputs is the order things happen.

The loop: feed `step`, act on each output in order:

| output | driver does |
|---|---|
| `NewRequest h` | build the `Request` (`mkRequest`, `setAcceptsTrailers`), run the handler, send its `Response` |
| `RequestData b`, `RequestEnd t`, `RequestAborted` | answer the pending `readChunk` (or `readTrailers`) |
| `Send bs` | add to the write buffer (see Writing) |
| `NeedInput` | flush, wait for input (see Waiting), feed `Received` or `PeerClosed` |
| `Close` | flush, then lingering close (RFC 9112 Section 9.6) |
| `ProtocolMisuse m` | a bug in the driver: log it; `Close` follows |

`readChunk` feeds `WantBody` and loops until the answer arrives;
after `RequestEnd` it returns `""`, and after `RequestAborted` it
throws `IncompleteBody`, as `RequestBody` documents. A body with
`reqBodyLength = Just 0` never touches `step`. `readChunk` after the
exchange has ended throws `IncompleteBody`.

Responding: `checkResponseFor`, then `Respond` with a `ResponseHead`
(the driver adds `Date`, see below), then the content:
`ContentBytes` is one `WriteBody`; `ContentFile` is read in pieces
and written (S6); `ContentStream` gets write and flush callbacks that
feed `WriteBody` and flush, and its trailers go through
`checkTrailers` before `EndBody`. If the length written differs from
the one declared, or the stream throws, the driver feeds `Abort`.

**S3. Exceptions from the handler.** Before the response has
started: a minimal 500 (`Content-Length: 0`), and the connection is
kept if `step` allows it (the unread body is discarded as usual).
After it started: `Abort`, which closes. Either way the exception goes
to a `settingsOnException` hook (default: print to stderr).
Asynchronous exceptions (the thread being killed) are rethrown after
closing the socket.

## Waiting: timers and shutdown without asynchronous exceptions

Every wait for input is one STM transaction over three things:

```haskell
atomically $ (Readable <$ readable) `orElse` (TimerFired <$ expired) `orElse` (ShuttingDown <$ shutdown)
```

- `readable` from `threadWaitReadSTM` on the socket's descriptor (the
  transport's `transportWaitReadable`);
- `expired` from `registerDelay`, armed by phase (D8);
- `shutdown`, a `TVar` the server sets when it is asked to stop.

Then the driver calls `recv` (which does not block, since data or end
of input is there), or feeds `TimedOut`, or feeds `Shutdown`. **S4:**
protocol timeouts and the graceful stage of shutdown use no
asynchronous exceptions, so they never land in the middle of the
application's code or of a `step`; they are only noticed where the
driver waits. Asynchronous exceptions are kept for the two cases
where interrupting the application is the point: the end of the
shutdown grace period, and the optional handler time limit (below). The cost is a
transaction and a timer registration per wait, to be measured
against `System.Timeout` and warp's timer manager once there are
benchmarks.

Timers by phase (`phase` from `step`), with proposed defaults (S7):

| phase | timer | default |
|---|---|---|
| `Idle` | from the end of the last response to the first byte | 60 s |
| `ReadingHead` | from the first byte to the end of the head, total (slowloris) | 30 s |
| `ReadingBody` | since the last bytes arrived (a large upload may take long, but must keep moving) | 30 s |
| `DiscardingBody` | total | 5 s |
| writing | per flush, while the client does not read | 30 s |
| lingering close | after half-closing | 2 s |

A write cannot wait in the same transaction (sending blocks in
`network`), so the write timeout is `System.Timeout.timeout` around
each flush; on expiry the connection is closed.

Shutdown (`runServer`'s stop action) has two stages:

1. Graceful: stop accepting, set the `shutdown` TVar. An idle
   connection closes at once; a busy one finishes its exchange and
   closes (D8).
2. Forced: when the grace period (default 30 s) ends, every connection
   thread still running is killed (`throwTo`). Since the handler runs
   in its connection's thread, this also stops a handler that is stuck
   in a loop or waiting forever. The driver closes the socket, and
   `runServer` returns, so the application can be restarted in-process
   or by a supervisor.

Two limits of GHC apply here, as for every Haskell server: a loop that
never allocates never reaches a point where an asynchronous exception
is delivered (compile the application with `-fno-omit-yields` to
prevent that, at a small cost), and a thread blocked in a foreign call
cannot be interrupted until the call returns. Ending the process is
the remaining remedy for both, and works regardless.

**Handler time limit** (optional, off by default): a setting that
limits how long a handler may run. When it expires, the connection's
thread is interrupted with an asynchronous exception: if the response
has not started, a 503 (Service Unavailable) is sent and the
connection closed; otherwise the response is aborted. It is opt-in
because it interrupts application code at an arbitrary point, which an
application must be written to tolerate (with `bracket` and the like).

## Writing

`Send` outputs are collected in a list and written with one
`sendMany` (`writev`), so a small response is one system call. The
buffer is flushed before waiting for input, when the response ends,
when a `ContentStream` flushes, and when it holds more than 16 KiB.
`TCP_NODELAY` is set, since the driver does its own coalescing.

`Date` (required with a clock, RFC 9110 Section 6.6.1, M4) comes from
a value the server refreshes once a second, so formatting a date is
not on every response's path.

## The accept loop

`runServer` binds (`settingsHost`, `settingsPort`, or a given socket),
accepts, sets `TCP_NODELAY`, and forks a thread per connection with
`forkIOWithUnmask`, registered so that shutdown can wait for it.
Accept errors from running out of descriptors (`EMFILE`, `ENFILE`)
pause accepting briefly rather than spin or stop. A limit on
connections is a setting (default: none).

## Not in this step

- **TLS** (S5): the `Transport` record leaves room for it. HTTP/2 needs
  it in practice (ALPN), so it comes with HTTP/2.
- **`sendfile`** (S6): files are read and written in 64 KiB pieces
  first; a zero-copy path once benchmarks say it matters, using
  Happstack's `sendfile` package (https://github.com/Happstack/sendfile),
  which we maintain and can change as needed.
- **Benchmarks** against warp come right after: `wrk`-style small
  responses, pipelining, large bodies, many idle connections. No
  tuning before measuring.

## Testing

- **In-memory transport**: the session generators of
  `ConnectionTests` (valid, damaged, split at random points), run
  through `serveConnection` with a scripted `Application`, must give
  the same requests and the same bytes as the simulated driver there
  (which already agrees with the reference model). This tests the
  driver's handling of outputs, `Request`/`Response` conversion and
  body reading.
- **Real sockets**: the same sessions over loopback, written with
  random splits and small delays, and read back with the client-side
  reader.
- **Timers and shutdown**: short timeouts, checked by phase: a stalled
  head gets 408, an idle connection closes quietly, shutdown closes
  idle connections at once and busy ones after their response.
- **Handler failures**: exceptions before and after the response
  starts, streams that write too much or too little, bad trailers.
- Mutation testing of the driver modules, as for the core.

## Questions

- **S1.** A new package `hyperdrive-server` for all IO. Recommended.
- **S2.** `Application = Context -> Request -> IO Response`, with the
  peer and local addresses and whether the connection is secured
  (design note M8). Recommended over putting them in `Request`, which
  is version-independent and built without IO.
- **S3.** Handler exceptions: 500 before the response starts (keeping
  the connection), `Abort` after. Recommended.
- **S4.** Timeouts and shutdown through STM waits, never asynchronous
  exceptions. Recommended.
- **S5.** Cleartext only in this step; TLS with HTTP/2. Recommended.
- **S6.** Files read and written in pieces first; `sendfile` after
  benchmarks. Recommended.
- **S7.** The timeout defaults in the table above (close to nginx's and
  warp's). Recommended.
