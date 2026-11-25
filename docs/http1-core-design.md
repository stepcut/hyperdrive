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

Implemented: `requestAcceptsTrailers` and `setAcceptsTrailers` on
`Request` (`mkRequest` leaves it `False`), and `acceptsTrailers` in
`Hyperdrive.HTTP1.Framing`, which reads it from `TE`. A `TE` that is
not a valid list counts as no `trailers` member rather than an error,
and `Connection: TE` is not required.

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

Implemented (`Hyperdrive.HTTP1.Target`) with one change: a `Host` that
disagrees with an absolute-form target is not rejected, since RFC 9112
Section 3.2.2 says an origin server "MUST ignore the received Host
header field" then. It is still required (HTTP/1.1), single and valid,
as Section 3.2 requires of every request, and is removed from the
fields, so the application never sees a host that routing did not use.
An absolute-form `https` target over an unsecured connection, or one
with any scheme other than `http` and `https`, gets 421 (RFC 9110
Section 7.4). A request with no `Host` (HTTP/1.0) or an empty one uses
a configured default authority, or is rejected if there is none (RFC
9112 Section 3.3).

## `step`: state and transitions (Q1 to Q5 decided as recommended, 2026-11-18)

Implemented in `Hyperdrive.HTTP1.Connection`. Details settled while
implementing it:

- The output announcing a request is `NewRequest RequestHead` (a
  constructor `RequestHead` would clash with the record's).
- A request without a body (`reqBodyLength = Just 0`) gets no
  `RequestData` or `RequestEnd`: the driver answers `readChunk` itself.
  After `RequestEnd` it does the same. `WantBody` then is misuse.
- For a `Content-Length` body, `RequestEnd` comes with the last
  `RequestData`; for chunked, on the `WantBody` after it.
- A final response sent while a `100-continue` request's body is unread
  and no 100 was sent closes the connection: the client may never send
  the body, so the rest of the input cannot be told apart.
- `Connection` options must be tokens; anything else is 400.

The reference-model test below is `prop_model` in
`test/ConnectionTests.hs`; it compares scripts that read all of a body
or none of it, since how much a partial read gets depends on how the
input arrived.

Everything `step` needs underneath now exists: `parseRequestHead`,
`requestFraming`, `reconstructTarget`, `acceptsTrailers`,
`decodeChunked` and `responseFraming`. This section fixes what `step`
does with them. Questions for the user are marked **Q**.

### Types

```haskell
data Config = Config
  { headLimits    :: Limits          -- D4
  , chunkLimits   :: ChunkedLimits   -- D4
  , targetContext :: TargetContext   -- scheme of the connection, default authority
  , discardLimit  :: Word64          -- D6: 64 KiB
  }

-- what the driver passes on to mkRequest and the application
data RequestHead = RequestHead
  { headVersion, headMethod, headTarget
  , headFields          :: Fields    -- protocol fields and Host removed
  , headBodyLength      :: Maybe Word64  -- Just 0: no body; Nothing: chunked
  , headAcceptsTrailers :: Bool }    -- D9

-- what the driver makes of the application's Response
data ResponseHead = ResponseHead
  { respStatus   :: Status            -- final, never 1xx (M5)
  , respFields   :: Fields            -- no protocol fields; the driver adds Date (M4)
  , respLength   :: Maybe Word64      -- Nothing: unknown
  , respTrailers :: [FieldName]       -- declared, for the Trailer field (M6)
  , respHasContent :: Bool }          -- False for ContentNone
```

Inputs and outputs are as in "Proposed shape" above, with three
changes:

- **Q1. `WantBody` means "the application wants the next chunk"**,
  one per `readChunk`, not "it started reading". The core answers each
  with one `RequestData` (from what it holds, or after `NeedInput`),
  or `RequestEnd`. That is the flow control: the core never asks for
  bytes nobody wants, so its buffer stays bounded (by one read plus the
  head limits) however large the body is.
- **New output `RequestAborted`**: the body will not arrive in full
  (the client closed, the chunked coding was invalid, or the body timed
  out). `readChunk` throws `IncompleteBody`.
- **New input `Abort`**: the application failed while sending a
  response (an exception, or `ContentStream` wrote more or less than
  it declared). The core answers with `Close`, never ending the body
  normally, so a truncated response cannot pass for a complete one.

`phase :: Conn -> Phase` tells the driver which timer to run (D8).

### States

The connection is a request side and a response side, plus flags:

```
request side:   AwaitingHead buffer searched     -- includes "no bytes yet" (idle)
                ReadingBody  BodyState           -- Remaining n | Chunked Decoder
                Discarding   BodyState budget    -- D6, after the response
                RequestDone                      -- body read; next head waits (D2)
response side:  AwaitingResponse | SendingBody SendState | ResponseDone
flags:          keepAlive, continueSent / expects100, peerClosed, closing
```

D2 holds by construction: the request side leaves `RequestDone` for
`AwaitingHead` only when the response side reaches `ResponseDone`, and
only then are the buffered bytes (a pipelined request) parsed.

### Transitions

**Reading a head** (`Received`, or bytes left over from the previous
request): append, `parseRequestHead`.
- Incomplete: `NeedInput`.
- Any error, from the head, `requestFraming`, `reconstructTarget`, or
  the checks below: a minimal error response (D3: status line,
  `Content-Length: 0`, `Connection: close`) and `Close`. Nothing reaches
  the application.
- Complete: remove protocol fields, decide `keepAlive` (D5: false for
  HTTP/1.0 or `Connection: close`), emit `RequestHead`, go to
  `ReadingBody` (or `RequestDone` if there is no body).
- Checks that are new in `step`:
  - **Q2.** A CONNECT request with `Content-Length` or
    `Transfer-Encoding`: 400. RFC 9110 Section 9.3.6 gives CONNECT
    content no meaning, and M10 delivers an empty body anyway.
  - **Q3.** `Expect` with anything other than `100-continue`: 417
    (Expectation Failed), which RFC 9110 Section 10.1.1 allows. In an
    HTTP/1.0 request `100-continue` is ignored, as it requires.

**Reading a body**: on `WantBody`, deliver from the buffer (a slice,
never a copy), or `NeedInput` and deliver on the next `Received`. The
first `WantBody` sends `100 Continue` if the request expects it and no
response has started (D7). At the end, `RequestEnd` with the trailers
(empty for `Content-Length`). An invalid chunked body, or `PeerClosed`
or `TimedOut` before the end: `RequestAborted`, `keepAlive` off, and
an error response (400, or 408 for a timeout) if none has started,
else `Close`.

**Responding** (`Respond`): `responseFraming` from the request's
version and method, then one `Send` of the status line (always
`HTTP/1.1`, RFC 9110 Section 6.2), the fields, the framing fields,
`Trailer` if chunked with declared trailers, and `Connection: close`
if the connection will close. `WriteBody` adds the bytes, with chunk
framing for chunked (an empty write sends nothing, since an empty
chunk would end the body), and nothing for `HeaderOnly` (HEAD, 204,
304). `EndBody`: the last chunk and trailers for chunked; for
`Content-Length`, `Close` instead if fewer bytes were written; for a
close-delimited body, `Close`. Writing more than `Content-Length`
declared is `Abort`.

**After the response**:
- request body fully read and `keepAlive`: back to reading a head,
  starting with the bytes already buffered;
- body not fully read (the application responded first): `Discarding`
  up to `discardLimit` more octets, then as above, or `Close` if the
  body is longer (D6);
- otherwise `Close`.

`Close` is staged by the driver: send what is queued, half-close,
read and discard until the client closes or a short timer expires,
then close (RFC 9112 Section 9.6). After `Close`, every input is
ignored.

**Other inputs**:
- `PeerClosed`: idle or mid-head, `Close` (no response: there is no
  request). During a body, see above. Once the request is complete,
  finish the response, then `Close` (a client may half-close after
  sending its request).
- `TimedOut`: idle, `Close`; mid-head, 408 and `Close`; during a body,
  as above. Waiting for the application or writing is the driver's
  business, not a protocol timeout.
- `Shutdown`: idle, `Close`; otherwise `keepAlive` off, so the current
  response carries `Connection: close` (if not yet sent) and the
  connection closes after it.

**Q4. Misuse by the driver** (`WriteBody` before `Respond`, two
`Respond`s, `WantBody` with no body): `Close`, and a `ProtocolMisuse`
output naming it, so tests and logs see the bug rather than a silent
close. These are bugs in hyperdrive's own driver, never the
application's or the client's.

**Q5. Error response bodies**: empty (`Content-Length: 0`), as D3
recommends. A short `text/plain` reason would help someone debugging
with curl, and costs nothing in safety. Recommended: keep them empty
for now; an application hook can come later (D3).

### Tests

- **Model**: a reference function on the *whole* input at once, with
  no incremental state: split the bytes into requests with the same
  parsers used once over each message, and compute the outputs a
  scripted application (which responds with given heads and bodies,
  reads all, part or none of each body) should see. `step` driven
  with the same bytes split at arbitrary points must give the same
  requests, the same body bytes (chunk boundaries aside) and the same
  sent bytes.
- **Read-back**: everything `Send` produces, parsed by an independent
  client-side response parser, is exactly one response per request, in
  order, with the bodies and trailers written (extending the existing
  response framing test).
- **Properties**: nothing after `Close`; no `RequestHead` while a
  response is unfinished; buffered input never exceeds one read plus
  the head limits; every byte of input is used exactly once, by a
  head, a body or discarding.
- Mutation testing as for the parsers.

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

## Differential tests against llhttp (2026-11-24)

`hyperdrive-http1/test-llhttp`, behind the cabal flag `llhttp` (off by
default, since it needs llhttp's C library), run by
`scripts/differential-llhttp` in the shell `.#llhttp`. Generated
sessions of one to three requests, a quarter of them damaged and all of
them split at random points, are given to llhttp 9.4.3 (through a small
C shim that records its callbacks) and to `step` under a simulated
driver. The requests that both take in full must be the same: method,
target as sent, version, every field line, body and trailers. Where one
takes a request that the other does not, the reason must be one the
test knows, by name; anything else fails. What follows the last
complete request is not compared, since hyperdrive reads a head only
when all of it has arrived and llhttp refuses as soon as it can.
`LLHTTP_SURVEY=n` prints every kind of difference in `n` sessions with
its count and shortest example.

In 100,000 sessions about 9,900 differ, and none in framing: no request
that both take is read differently, and neither awaits more of a
request that the other has finished. (llhttp leaves the whitespace
after a field's value in it; the test trims it.)

hyperdrive takes, llhttp refuses:

| what | llhttp's error | |
|---|---|---|
| a method llhttp has no name for (`get`, `X-Custom`) | `INVALID_METHOD` | methods are open (RFC 9110 Section 9.1) |
| HTTP/1.2 and above | `INVALID_VERSION` | to be read as 1.1 (Section 2.5) |
| `Content-Length` repeated, or a list, with one value (`5, 5`) | `UNEXPECTED_`, `INVALID_CONTENT_LENGTH` | allowed (Section 8.6); also `5,` and an empty line beside `5`, see below |
| `Transfer-Encoding` with whitespace after it, or an empty element (`,chunked`) | `INVALID_TRANSFER_ENCODING` | allowed (Sections 5.5, 5.6.1.2) |
| whitespace around a chunk extension (`8 ;a`, `2; a=b`) | `INVALID_CHUNK_SIZE`, `STRICT` | BWS (RFC 9112 Section 7.1.1) |
| `Content-Length` or `Transfer-Encoding` in the trailer section | the same as in a head | given to the application as trailers, see below |

llhttp takes, hyperdrive refuses (`stricter` in the test lists them by
error): a line feed or carriage return alone before the request line; a
request line without a version, and versions 0.9 and 2.0; everything
about `Host` and the target's form, which llhttp does not look at;
`Transfer-Encoding` in HTTP/1.0; transfer codings other than `chunked`
(501); a chunk extension with an empty value; a body or a bad
`Transfer-Encoding` with CONNECT, whose framing llhttp does not read;
an expectation other than `100-continue` (417); a `Connection` field
that is not a list of tokens.

Two of hyperdrive's leniencies are open questions:

- `Content-Length` is read as a list, as Section 8.6 allows a recipient
  to do for repeated values, and a list may have empty elements: so
  `5,` and an empty `Content-Length` line beside a `5` are 5. llhttp
  refuses them; a parser that took them with another length would make
  them a way to smuggle a request, and none is known, but refusing
  empty elements here would cost nothing.
- A trailer section's `Content-Length` and `Transfer-Encoding` are
  given to the application with the other trailers. They do not change
  the framing, and trailers are kept apart from the header fields, but
  Section 6.5.1 says a sender must not put framing fields there;
  refusing or dropping them would be stricter.
