# Benchmarks against warp

Run with `nix develop .#bench -c scripts/bench`. The same application
(bench/Hyperdrive.hs, bench/Warp.hs: a 13-byte response, a 1 MiB
response, and a 1 MiB upload whose length is returned) on hyperdrive
and on warp 3.4.9, both built with `-O2 -threaded` and run with
`+RTS -N8 -A32m`, the server on cores 0-7 and `wrk` (8 threads) on
cores 8-15. Each figure is one 10-second run: on this machine runs vary
by up to about 20%, and other work was running on it, so differences
smaller than that mean little.

## 2026-11-18, AMD Ryzen 9 9955HX (16 cores), after 21781f2 and the coarse timers

| | hyperdrive | warp |
|---|---|---|
| small responses, 100 connections | 476k req/s (p50 0.17 ms, p99 2.2 ms) | 711k (0.11 ms, 3.1 ms) |
| small responses, 1000 connections | 407k (2.1 ms, 14 ms) | 745k (1.0 ms, 10 ms) |
| small responses, pipelined 16 deep | 722k (1.4 ms, 11 ms) | 931k (0.9 ms) |
| 1 MiB responses, 32 connections | 12.1k (2.4 ms, 8.3 ms) | 12.6k (2.4 ms, 6.6 ms) |
| 1 MiB uploads, 32 connections | 7.5k (3.8 ms, 12 ms) | 6.5k (3.8 ms, 78 ms) |
| memory per idle connection (10,000) | 17 KB | 22 KB |

The first run, before the coarse timers, gave hyperdrive 34k req/s on
small responses against warp's 595k: every wait and every write
registered with GHC's timer manager, and threads queued on its lock
(a quarter of the time was in pthread mutexes, with only 3 of 8 cores
busy). Without timers at all it reached 481k; with the coarse timers
(Hyperdrive.Server.Timer) it keeps every timeout and reaches 460k to
476k.

Next: small responses, where hyperdrive spends about 11 µs of CPU per
request to warp's 7.6 µs. Profile before changing anything.

## 2026-11-19, the small-response path profiled

Measured per request with 100 connections (perf, `+RTS -s`, strace,
/proc): hyperdrive used about 11.3 µs of CPU (4.7 user, 6.7 kernel),
warp 6.6 (1.8 user, 4.8 kernel). The pure core (`step` for a whole
exchange, single-threaded) costs 1.7 µs and allocates 6.9 KB; the rest
is the driver and the kernel. Changes, each measured:

- Shutdown is signalled through a `TVar` per connection, not one that
  every waiting connection read (its STM bookkeeping showed in perf).
- `Date` reads whole seconds from `getSystemTime`; `getPOSIXTime` went
  through Integer arithmetic on every response.
- The driver tries a non-blocking receive before waiting. System calls
  per request went from recv 1, writev 1, epoll_ctl 1, epoll_wait 0.9
  to recv 1, writev 1, epoll 0.03 each, as warp's.
- Each connection receives into one 16 KiB buffer and copies out what
  arrived. Allocation fell from 46 KB to 13 KB a request (warp: 5.8
  KB), and young-generation collections from 2,277 to 101 in 5 s; GC is
  now 7% of the time. The buffer costs memory: 26 KB per idle
  connection, from 17 (warp: 22).

CPU per request hardly moved (11.3 to 11.2 µs): what remains is spread
over the driver (about 3 µs of user time beyond the core) and the
kernel (about 2 µs more than warp's for the same system calls, not
explained yet; kernel profiling needs privileges this machine does not
give). Joining small writes into one buffer made no difference.

| | hyperdrive | warp |
|---|---|---|
| small responses, 100 connections | 449k req/s (p50 0.17 ms, p99 5.4 ms) | 691k (0.11 ms, 4.1 ms) |
| small responses, 1000 connections | 451k (1.2 ms, 62 ms) | 704k (1.1 ms, 42 ms) |
| small responses, pipelined 16 deep | 724k (1.2 ms, 7.2 ms) | 882k (1.0 ms, 7.3 ms) |
| 1 MiB responses, 32 connections | 11.1k (2.7 ms, 7.5 ms) | 11.4k (2.7 ms, 7.6 ms) |
| 1 MiB uploads, 32 connections | 8.2k (3.4 ms, 19 ms) | 6.0k (3.9 ms, 120 ms) |
| memory per idle connection (10,000) | 26 KB | 22 KB |

Then (same day): each connection waits on one `MVar` of its own,
filled by the event manager's one-shot callback when its socket is
readable, by the ticker when its timer expires, and by shutdown,
instead of an STM transaction over three `TVar`s (about a tenth of the
time in the profile). CPU per request 11.2 to 10.4-10.7 µs (user 4.6,
kernel 6.1); 490k-530k small responses a second in the local runs.
