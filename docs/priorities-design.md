# Extensible priorities (RFC 9218), for HTTP/2 and later HTTP/3: P1-P10 decided as recommended, 2026-11-27

D8 (docs/http2-core-design.md) left this for after the benchmarks
against warp: the scheduler sends one frame at a time from each stream
that can, in turn, and its structure was meant to become RFC 9218's
(a queue for each urgency, in stream order). The questions for the
user are marked **P**, each with a recommendation. RFC 9218 and RFC
9651 (structured fields, which the `Priority` field is written in) are
in duvet: 31 and 30 requirements in scope, the rest marked as clients'
or intermediaries' (scripts/duvet-exceptions, with the overrides read
from the text).

## What RFC 9218 asks of a server

- Two parameters, urgency `u` (0 to 7, default 3, lower first) and
  incremental `i` (a boolean, default false), carried in the
  `Priority` request field (a structured-field Dictionary) and in
  PRIORITY_UPDATE frames (type 0x10, on stream 0, naming the stream).
  Unknown parameters, values out of range and values of other types
  "MUST be ignored" (Section 4); a request without them is treated as
  carrying the defaults (SHOULD).
- Scheduling (Section 10, all RECOMMENDED or SHOULD): higher urgency
  first; within an urgency, non-incremental responses one after the
  other in stream order, incremental ones sharing the bandwidth; and
  "avoid such starvation where possible" when incremental and
  non-incremental responses of one urgency compete.
- PRIORITY_UPDATE (Section 7.1): stream 0 only, else PROTOCOL_ERROR; a
  prioritized stream of 0, or an idle push stream (any even stream for
  a server that never pushes), PROTOCOL_ERROR; prioritized idle streams
  plus active ones beyond MAX_CONCURRENT_STREAMS, PROTOCOL_ERROR. The
  most recent update for a stream not yet opened SHOULD be kept and
  applied when it opens. A field value that does not parse MAY be a
  connection error.
- The response's `Priority` field is the origin's view (Section 8),
  meant for intermediaries; how to merge it is left open.

## Decisions

**P1. A structured-field parser and serializer, all of RFC 9651, in
`hyperdrive-semantics`.** The `Priority` field needs only a Dictionary
of integers and booleans, but RFC 9218 says "successfully parsed": a
value that is not valid structured-field syntax is not parsed at all,
which a parser of a subset cannot tell. And other fields an
application handles are structured (`Cache-Status`, `Proxy-Status`,
`Client-Cert`, the fetch metadata `Sec-Fetch-*`). Tested as the HPACK
code was, against a published corpus: the httpwg's
structured-field-tests (JSON, every type, valid and invalid,
serialisation too), as a flake input. Recommended. (Not chosen: a
parser of the Dictionary subset only.)

**P2. Where priorities live.** A `Priority` type (urgency,
incremental) and its parsing from a field value in
`hyperdrive-semantics` (`Hyperdrive.Semantics.Priority`), so that
HTTP/3 shares it; the field stays in the request's fields for the
application to read. The scheduler a pure module of its own
(`Hyperdrive.HTTP2.Schedule`), written without anything of HTTP/2 in
it, so that HTTP/3's core can use it as it is (QUIC streams are
scheduled the same way). Recommended.

**P3. The scheduling policy, and starvation.** Eight urgency levels,
the lowest number first. Within one: the non-incremental streams one
after the other in stream order, the incremental ones in turn, frame by
frame (today's round robin). For incremental and non-incremental
streams at one urgency, which RFC 9218 asks to keep from starving each
other: alternate between the first non-incremental stream and the
incremental ones in turn. Between urgencies: strictly in order (a
response of urgency 7 waits while any of urgency 0 to 6 can send).
Recommended (the survey below: all seven implementations are strict
across urgencies; only Go guards against the starvation, and the same
way).

*As built (P1-P3, 2026-11-27).* `Hyperdrive.Semantics.StructuredField`
(all of RFC 9651; the httpwg corpus, a round-trip property, 15
mutants). `Hyperdrive.Semantics.Priority`: `Priority` made only through
`mkPriority` (urgency 0 to 7), `PriorityParams` for the parameters a
value names, `parsePriorityParams` (a value that is not a Dictionary is
`Nothing`, the field as if absent, so not a signal for P9) and
`applyParams` (over the defaults for a request or PRIORITY_UPDATE, over
the client's for a response; an urgency out of range made by hand is
ignored, as one in a field is). `Hyperdrive.HTTP2.Schedule`, over any
ordered keys: `insert` (a stream with something to send; at the same
priority it keeps its place, moved within its urgency the alternation
stays as it was), `sent` (after every frame, the last one too),
`delete`, and `candidates`, the order to try streams in, so that one
blocked by its window does not lose its place. Tested against a model
written another way (stamps instead of a queue), with examples, and with
properties of the order frames go out when all can send: strict by
urgency, and an incremental stream waits at most 2I - 1 frames of its
urgency (I the incremental streams there), the first non-incremental
one at most one. 12 mutants. Not yet used by the core (next: P4, P5,
P9, P10).

**P4. The application's view.** The response's `Priority` field, if
the handler sets one, overrides the request's for the parameters it
names (Section 8's example: the server's urgency, the client's
incremental), and is sent to the client as it is. So a handler that
knows its response is a long stream (SSE) can mark it incremental, or
an image less urgent than the client asked. Recommended. (Not chosen:
the client's signal alone.)

**P5. PRIORITY_UPDATE.** Parsed (type 0x10), with RFC 9218's MUSTs
above. An update for a stream not yet opened is kept (the latest for
each), within the bound Section 7.1 sets (prioritized idle plus active
streams at most MAX_CONCURRENT_STREAMS, beyond it PROTOCOL_ERROR), and
applied when the stream opens; one for a closed stream is ignored. A
field value that does not parse: ignored, the defaults kept, not a
connection error (the MAY not taken: a client's bad syntax is not
worth its connection). The nuisance budget (C10), which until now let
PRIORITY_UPDATE go free: one for an update that names a closed stream
or changes nothing; free otherwise (browsers send them as a page
scrolls). Recommended, the budget part to be checked in the same survey.

**P6. NO_RFC7540_PRIORITIES changed after the first SETTINGS** (RFC
9218 Section 2.1's MAY, which nghttp2 takes): not taken; the value is
not used for anything. Recommended.

**P7. The tests.** The scheduler against a reference model (given the
streams' priorities and what each can send, the order frames go out),
generated as C13's model of Section 5.1 is; the parser against the
corpus; PRIORITY_UPDATE's rules in the core's state table; the
differential test against nghttp2 extended with `priority` fields and
PRIORITY_UPDATE frames (nghttp2 implements RFC 9218); and h2load, which
sends no signals, to show nothing got slower.

**P8. Order of work.** P1 (the parser, with the corpus), then P2 and P3
(the scheduler and its model), then P4 and P5 in the core and the
driver, then the differential and the timings.

## What other implementations do (surveyed 2026-11-27)

From current source. RFC 9218 is implemented by Go 1.27 (HTTP/2, on by
default), nghttp2 with nghttpx (HTTP/2; nghttp3 for HTTP/3), nginx
(HTTP/2), h2o (HTTP/3; for HTTP/2 only a mapping of a response's
urgency onto Chromium's dependency tree), Cloudflare's quiche, LiteSpeed's
lsquic and Google's quiche (HTTP/3). Not by Kazu Yamamoto's http2 and
http3 (warp), Kestrel, Envoy's own code (its HTTP/3 inherits Google's
quiche), Jetty 12, Netty, Rust's h2 and hyper, Apache's mod_http2.

| | across urgencies | within one | incremental against non-incremental | response `Priority` | updates for unopened streams | bad value | budget | parser |
|---|---|---|---|---|---|---|---|---|
| Go | strict | non-incremental to completion in opening order; incremental round robin, a frame each | alternate, frame by frame | no | one slot | stream error (RST_STREAM) | none | full RFC 9651 |
| nghttp2 | strict | the same (byte-weighted round robin) | none | nghttpx: the backend's overrides, then wins | idle stream objects, at most MAX_CONCURRENT_STREAMS, beyond a connection error | ignored | every update one token of its glitch limiter (10,000, 330 a second) | full (but one bad `u` drops the value) |
| nginx | strict | non-incremental, then incremental, each in stream order (no round robin) | none | the upstream's, parameter by parameter | 32 nodes shared with closed streams, the oldest evicted | ignored | none of its own | full RFC 8941 |
| h2o (HTTP/3) | strict | as Go | none | yes, parameter by parameter (a later update resets) | stream state up to the stream limit | ignored | none | ad hoc |
| Cloudflare quiche | strict | as Go | none | no (an API) | one per stream within the limit | not parsed on receipt | none | crate (clamps `u`) |
| lsquic | strict | as Go | none | no (an API) | one per stream within the limit | connection error | none | full |
| Google quiche | strict | first come by default; as Go behind a flag | none | no | one per stream within the limit | connection error | none | full RFC 8941 |

Two choices of Go's bear on ours. Until a client has sent a `priority`
field or a PRIORITY_UPDATE, Go takes its streams as incremental (u=3,
i), not RFC 9218's default (i absent, so false): a client that sends no
signals (h2load, older clients, most non-browser clients) keeps the
round robin it had, and one endless response (an SSE stream) cannot
hold back the rest, which D8 worried about. And once a request carries
`via`, `forwarded` or `x-forwarded-for`, Go ignores request `priority`
fields on that connection (u=3, i), following Section 13.2 ("Backend
servers SHOULD only schedule based on client priority information where
that information can be scoped to individual end clients"): a proxy
multiplexes many clients' requests on one connection, and one client's
urgency should not starve another's. Nobody detects a change of
SETTINGS_NO_RFC7540_PRIORITIES except nghttp2.

What it changes in the recommendations: P3 stands and is Go's (strict
across urgencies; alternation is the only starvation guard any of them
has). P4 stands (nghttpx, nginx and h2o all use the response's field
where they have one). P5 stands: unparsable values ignored as
nghttp2, nginx and h2o do; a bound on buffered updates as Section 7.1
sets it, as nghttp2 does; only nghttp2 charges for updates at all. P1
stands: five of seven parse fully. Two additions, P9 and P10, follow.

**P9. A client that sends no signals.** Streams are incremental until
the connection has carried a `priority` field or a PRIORITY_UPDATE;
from then on RFC 9218's defaults (u=3, not incremental). A deviation
from Section 4's SHOULD, recorded as such, as Go's. Recommended: it
keeps today's round robin for clients that do not use RFC 9218, and so
the protection D8 chose it for.

**P10. Requests through a proxy.** Once a request on a connection
carries `via`, `forwarded` or `x-forwarded-for`, request `priority`
fields on it are ignored, as Go does (Section 13.2's SHOULD);
PRIORITY_UPDATE frames still apply (they are the connection's own).
Recommended, with the response's field (P4) still the application's
way to set priorities behind a proxy.

## As built in the core (P4, P5, P9, P10; 2026-11-27)

`Hyperdrive.HTTP2.Frame` parses PRIORITY_UPDATE (`PriorityUpdateFrame`,
the prioritized stream and the value unparsed): not on stream 0, or a
prioritized stream of 0, PROTOCOL_ERROR; a payload shorter than the
stream ID, FRAME_SIZE_ERROR (RFC 9113 Section 4.2, "too small to contain
mandatory frame data"). In the core, `cOrder` gave way to a `Schedule`;
each stream keeps the client's priority and the response's parameters,
and is scheduled at the second over the first. `pump` tries the
schedule's candidates in order, takes the stream's turn before its frame
(so that a last frame counts), and leaves a stream that has sent all it
has; `forget` takes a stream out as it closes.

- P9: until the connection carries a `priority` field that parses or a
  PRIORITY_UPDATE that parses, a new stream is urgency 3, incremental;
  from then on the defaults. Streams already open keep theirs. With no
  signals the order is the round robin before (the existing scheduler
  tests pass unchanged).
- P10: a request carrying `via`, `forwarded` or `x-forwarded-for` marks
  the connection; its own `priority` field and those of every later
  request are ignored, and are not signals. PRIORITY_UPDATE still
  applies.
- P5: for an even stream, PROTOCOL_ERROR (a push stream, idle, for a
  server that never pushes); for a stream not yet opened, kept (the
  latest), PROTOCOL_ERROR if the streams kept plus the active ones would
  exceed our MAX_CONCURRENT_STREAMS; applied when the stream opens, over
  its request's field (Section 7: the latest update "overrides any other
  signal"), and those for streams passed over dropped then; for an open
  stream, applied at once, moving it in the schedule; for a closed one,
  ignored. Its parameters not named are the defaults. One nuisance unit
  (C10) for a value that does not parse, one for a closed stream, one
  that changes nothing (the same priority, or the same value kept again);
  others are free.
- P4: the response's `priority` field, if it parses, sets the parameters
  it names over the client's, for as long as the stream sends;
  PRIORITY_UPDATEs after it change the client's part only. The field is
  sent as the handler wrote it.

Tests: the order DATA goes out for each case above, PRIORITY_UPDATE's
errors, bound and charges at their boundaries (StreamTests), the frame's
parsing (FrameTests), h2spec unchanged (141/146). Left: the nghttp2
differential extended with `priority` fields and PRIORITY_UPDATE (P7),
and timings (asked for first).

## Questions in short

- **P1.** All of RFC 9651 in `hyperdrive-semantics`, tested against the
  httpwg corpus.
- **P2.** `Priority` in semantics; the scheduler a module free of HTTP/2,
  for HTTP/3 to share.
- **P3.** Strict by urgency; one by one or in turn within an urgency;
  alternation against starvation (as Go).
- **P4.** The handler's response `Priority` overrides the parameters it
  names.
- **P5.** PRIORITY_UPDATE kept for streams not yet open (bounded),
  unparsable values ignored, budget for useless ones only.
- **P6.** A changed NO_RFC7540_PRIORITIES not an error.
- **P7.** A model for the scheduler, the corpus for the parser, the
  nghttp2 differential extended.
- **P8.** Parser, scheduler, core and driver, then measurement.
- **P9.** Streams incremental until the client sends a signal, as Go.
- **P10.** Request `priority` fields ignored once a request says it came
  through a proxy, as Go.
