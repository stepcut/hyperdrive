# Third-party test data

Test inputs written by others, so that the tests do not only check the
code against our own reading of the specifications. Each directory keeps
the upstream license. Files are unmodified copies unless noted.

Most of this data was written for other specifications, so it is used in
one of three ways:

- as an exact oracle, only where that specification and ours agree;
- as a security oracle, where we must not give an answer that could be
  exploited, even if it differs from theirs;
- as a source of hard inputs, recording our results and reviewing every
  disagreement.

## wpt/

`urltestdata.json` from [web-platform-tests](https://github.com/web-platform-tests/wpt),
`url/resources/urltestdata.json` at commit
`065d99ab62bf0b6ca04907f423625801c350edb2` (2026-11-12).
3-Clause BSD License (`LICENSE.md`).

These are the tests for the WHATWG URL Standard, which is what browsers
implement. It differs from RFC 3986 in deliberate ways: it accepts
shorthand IPv4 forms such as `127.1` and `0x7f000001`, decodes and maps
host names through IDNA, and fails on hosts that RFC 3986 allows.

## seclists/

`LFI-Jhaddix.txt` from [SecLists](https://github.com/danielmiessler/SecLists),
`Fuzzing/LFI/LFI-Jhaddix.txt` at commit
`122cc78a22a114c4e1049c5606723ca4667b8f24` (2025-06-29).
MIT License (`LICENSE`).

Path traversal and local file inclusion payloads.

## payloadsallthethings/

`ssrf-hosts.txt` is extracted from
[PayloadsAllTheThings](https://github.com/swisskyrepo/PayloadsAllTheThings),
`Server Side Request Forgery/README.md` at commit
`12e5d82ca65f2803f15e5ce74abc33674703dff2` (2026-07-23), by `extract.py`,
which documents what it extracts:

    extract.py README.md > ssrf-hosts.txt

MIT License (`LICENSE`).

The hosts used in server-side request forgery examples to reach internal
addresses while getting past filters: decimal, octal and hexadecimal
addresses, shorthand forms, IPv6 notations and names that resolve to
loopback.

## structured-field-tests/

`string.json` and `string-generated.json` from
[httpwg/structured-field-tests](https://github.com/httpwg/structured-field-tests)
at commit `10781e4a59c387853c900cc06cdd8a3fd7ae4134` (2026-10-30).
BSD-style license of the IETF Trust (`LICENSE.md`).

Tests of strings in RFC 9651 Structured Field Values. Its `sf-string`
is a strict subset of the RFC 9110 `quoted-string`: printable ASCII
only, and only `\"` and `\\` as escapes. So strings it accepts must
decode to the same value with `parseQuotedString`, and of those it
rejects, we must reject exactly the ones RFC 9110 rejects too.
