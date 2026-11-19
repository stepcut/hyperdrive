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
