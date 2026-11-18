# HTTP/1.1 core: design proposal

Status (2026-11-17): D2, D4 and D9 decided as recommended; the other
recommendations stand unless revisited. Nothing here is implemented
yet, apart from framing (`Hyperdrive.HTTP1.Framing`).

## Constraints already decided

- The protocol core is pure (sans-IO): it never touches a socket, a
  clock or a thread. An IO driver feeds it bytes and events and carries
  out what it asks. The core is tested without IO; the driver is
  optimized separately.
- Applications see only `Request` and `Response` from
  `hyperdrive-semantics`. Request bodies are read with `readChunk`;
  response content is bytes, a file or a `ContentStream`. No streaming
  library.
- Framing is decided by `requestFraming` and `responseFraming`, and
  ambiguous framing is rejected and the connection closed.
- Protocol fields (`Connection`, `Transfer-Encoding`, `Content-Length`,
  `TE`, `Upgrade`, ...) are interpreted by the core and never reach the
  application.

## Proposed shape

One function, folding inputs into a connection state and producing
outputs:

```haskell
data Conn                        -- the state of one connection; pure
newConn :: Config -> Conn

step :: Input -> Conn -> (Conn, [Output])

data Input
  = Received ByteString          -- bytes from the socket
  | PeerClosed                   -- the client closed its side
  | Respond ResponseHead         -- the application's response: status, fields, framing payload
  | WriteBody ByteString         -- a chunk of response content
  | EndBody Fields               -- end of the content, with trailers
  | WantBody                     -- the application started reading the request body
  | TimedOut                     -- the driver's timer for the current phase expired
  | Shutdown                     -- finish the current exchange, then close

data Output
  = RequestHead RequestHead      -- a request arrived: version, method, target, fields, framing
  | RequestData ByteString       -- part of its body: a slice of the bytes received, not a copy
  | RequestEnd Fields            -- the end of its body, with trailers
  | Send [ByteString]            -- write these, in order (one writev)
  | NeedInput                    -- nothing more until the next input
  | Close                        -- close the connection after sending what was asked, in
                                 -- stages: half-close, then read until the client closes
                                 -- or a short timer expires (RFC 9112 Section 9.6)
```

The driver is a loop: read from the socket when the core wants input
and the application has asked for body data, feed `Received`, act on
the outputs, and turn the application's `Response` into `Respond`,
`WriteBody` and `EndBody`. `RequestBody`'s `readChunk` is backed by
`RequestData` and `RequestEnd`.

Why one `step` rather than separate receive and send functions (as h11
does): every behaviour is then a sequence of inputs and a sequence of
outputs, which is what model-based tests generate and check, and the
driver is a single loop. The cost is a sum type for inputs; a thin
convenience layer can still offer `receive`/`send` names.

Performance: input is never copied. Body data is handed out as slices
of what was received, and response content is passed through
untouched, with chunked framing added as small separate buffers around
it, so a response is one vectored write. The core keeps only the
unparsed tail of the input.

## Decisions to make

Each has a recommendation. The ones marked **(decided)** were agreed
with the user on 2026-11-17.

**D1. One `step`, or receive/send halves.** Recommended: one `step`,
for testing and a single driver loop (above).

**D2. Pipelining (decided: one request at a time).** A client may send the next request before the
response to the previous one. Options: (a) parse requests one at a
time: after a request's head, the next is not parsed until its response
has been sent (as h11 does); pipelined requests still work, served in
order. (b) Parse ahead and queue heads, to let the application work on
several at once. Recommended: (a). (b) adds buffering, fairness and
error ordering questions for a feature browsers do not use.

**D3. Who writes error responses.** When a request is malformed
(framing, a bad request line, limits), something must send 400, 413,
414, 431 or 501 and close. Recommended: the core, as a minimal
response (`Content-Length: 0`, `Connection: close`), so that every
protocol error path is pure and tested. An optional hook could let the
application supply a body later.

**D4. Limits (decided: the defaults below).** The core enforces limits on the request line,
the length of a field line, the size of the header section, the number
of fields, chunk sizes and chunk extensions, and the trailer section,
answering with 414, 431 or 400 (RFC 9112 Section 3, RFC 9110 Sections
4.1 and 5.4, RFC 6585). Body size is left to the application (it sees
`bodyLength` and reads with `readChunk`). Recommended defaults, close
to common servers: request line 8 KiB, field section 32 KiB, 100
fields, trailer section 8 KiB, chunk extensions 1 KiB.

**D5. Persistence.** HTTP/1.1 connections stay open unless either side
sends `Connection: close`, the framing was rejected, the response is
close-delimited, or the driver asks (`Shutdown`). Recommended: HTTP/1.0
requests always get `Connection: close` and the connection is closed.
HTTP/1.0 persistence (`Connection: keep-alive`) is an old extension
whose implementations RFC 9112 Appendix C.2.2 describes as faulty in
practice, and supporting it adds framing cases for little benefit.

**D6. Responding before the body is read.** An application may respond
(for example 413, or a redirect) without reading the request body.
Options: (a) after the response, read and discard the rest of the body
up to a limit, then reuse the connection, or close if the limit is
exceeded; (b) always close. Recommended: (a) with a limit of 64 KiB,
which keeps connections alive for small bodies without letting a
client make the server read gigabytes. Either way, closing must be
staged: closing a socket with unread data in it can make TCP send a
reset that destroys the response before the client reads it (RFC 9112
Section 9.6). RFC 9110 Section 15.5.14 allows closing after a 413.

**D7. `Expect: 100-continue`.** Recommended: the core sends `100
Continue` when the application first asks for the body (`WantBody`),
and only if no final response has started and the request is HTTP/1.1.
If the application responds without reading the body, no 100 is sent,
and D6 applies (RFC 9110 Section 10.1.1).

**D8. Timeouts.** The core has no clock. Recommended: it reports its
phase (waiting for a request, reading a head, reading a body, writing a
response, idle between requests), the driver runs a timer per phase
with configured durations, and feeds `TimedOut` when one expires. The
core then decides: close quietly when idle or before any byte of a
request, send 408 when part of a head has arrived (a slow-header
attack), and close when a body stalls.

**D9. Trailers when the client did not send `TE: trailers` (decided:
send them, and tell the application).**
RFC 9110 Section 10.1.4: the client is "willing to accept trailer
fields" if it sends it; Section 6.5.1: a server "SHOULD NOT generate
trailer fields that it believes are necessary for the user agent to
receive" otherwise. Options: (a) always send the declared trailers
(every HTTP/1.1 client must parse a trailer section, so it is harmless
on the wire); (b) drop them unless the client sent `TE: trailers`;
(c) send them, and tell the application whether the client asked, as
a version-independent property of `Request`. Recommended: (c). The
application knows whether its trailers are necessary; the core does
not. HTTP/2 and HTTP/3 carry the same signal (`TE: trailers` is the
one `TE` value they allow).

**D10. Upgrade and CONNECT.** Recommended for now: `Upgrade` in a
request is ignored (the response is a normal one), and CONNECT gets
whatever the application responds except 2xx, which `checkResponseFor`
already rejects. Upgrades (WebSocket) come later as extended CONNECT
(design notes, M7).

**D11. The request head.** Recommended: the core hands over the
version, the method, the reconstructed `Target` (from the
request-target and `Host`, design notes P2), the fields with protocol
fields removed, and the framing, ready for `mkRequest`; and rejects a
request whose `Host` is missing, repeated or disagrees with an
absolute-form target (RFC 9112 Section 3.2).

## Testing

- The parser of request lines and fields against RFC 9112's grammar,
  with the existing tools: exhaustive short inputs, generated valid
  strings and near misses, alternative coverage.
- `step` with model-based tests: generate sequences of inputs
  (requests split at every byte boundary, pipelined, with bodies,
  trailers, errors and timeouts), run them through `step` and through a
  simple reference model of the RFC's connection rules, and compare the
  outputs.
- The bytes the core sends, read back by an independent client-side
  parser, as the response framing tests do.
- Differential tests against llhttp, h11, hyper and nginx on the same
  inputs (the HTTP Garden), and the llhttp and HTTP Garden corpora.
