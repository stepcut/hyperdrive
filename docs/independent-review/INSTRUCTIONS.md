# Independent review of hyperdrive-semantics

You are reviewing a Haskell library, `hyperdrive-semantics`, the
version-independent HTTP semantics (RFC 9110) of an HTTP server. Your job
is to check it against the RFCs, independently: by reading the RFCs and
the library's documented API, never its implementation, tests or design
notes. The point is to catch places where its authors read the RFCs
wrongly, or where its documentation promises something the RFCs do not
support. Do not assume the API or its documentation is right.

## What you may read

Only the files in this directory:

- `API.md`: every module's export list, types, signatures and
  documentation, with the function bodies removed. Use only names that
  appear in export lists (some unexported helper signatures appear too).
- `rfc9110.txt` (HTTP Semantics), `rfc9112.txt` (HTTP/1.1),
  `rfc3986.txt` (URI syntax), `rfc5952.txt` (IPv6 text form),
  `rfc1123.txt` and `rfc952.txt` (host names).

You may fetch other RFCs as plain text from https://www.rfc-editor.org/
if one of these cites something you need. Do not read anything else on
this machine: not the repository at @REPO@, not its git
history, not its build directory or interface files (do not use
`ghc --show-iface`), and do not search the web for this project. If you
cannot decide something from these sources, say so in your notes rather
than looking further.

## Scope

An earlier review covered methods, status codes, URI parsing, targets and
SafePath. Concentrate on what has been added since:

1. `Message.hs`: Request and Response (RFC 9110 Section 6), including what
   may and may not be part of a message, content, trailers, and the
   handling of framing and connection fields (RFC 9110 Sections 6, 7.6,
   8.6; RFC 9112 Sections 6 and 7).
2. `FieldSyntax.hs`: lists, tokens, quoted strings, comments and
   parameters (RFC 9110 Section 5.6), and `Field.hs`: field names and
   values, combining field lines, and the handling of duplicate fields
   and parameters (RFC 9110 Sections 5.1 to 5.5).
3. Host comparison and rendering in `URI.hs`: `canonicalHost`,
   `authorityKey`, `effectivePort`, `isDnsHostName`,
   `isNumericLookalike`, `renderHost` and `renderAuthority` (RFC 9110
   Sections 4.2 and 4.3, RFC 3986 Sections 3.2 and 6, RFC 5952, RFC 1123).

Report problems you notice anywhere else too.

## What to produce

1. `ReviewTests.hs`: a module `ReviewTests` exporting
   `reviewTests :: [(String, Bool)]`. Each entry is a test case: a
   description and whether the library behaves as the RFCs require. Each
   description cites the RFC and section and quotes the text that
   justifies the expected result. Write "POLICY" in the description when
   the expectation rests on a choice the API documents and the RFC permits
   but does not require. Cover the requirements (MUST, SHOULD, and the
   grammar) that apply to these APIs, and their edge cases, as thoroughly
   as you can: several hundred cases is reasonable. Where the RFC can be
   read more than one way, do not write a test that assumes one reading;
   put it in the notes instead.

   Run `./check.sh` to type-check the module against the compiled library,
   and fix it until it compiles. It does not run the tests, and you should
   not try to run them: your expectations must come from the RFCs, not
   from what the library does.

2. `REVIEW-NOTES.md`, with these sections:
   - **Conflicts**: places where the documentation contradicts the RFCs
     or claims more than they say. Quote both.
   - **Ambiguities**: places where the RFCs can be read in more than one
     way that matters here. Give each reading, which one the API seems to
     take, and what would differ.
   - **Gaps**: requirements on these APIs that the API does not seem to
     meet or to give a way to meet.
   - **Assumptions**: what you had to assume where the documentation is
     silent, so that the tests can be judged.

When you finish, reply with a short summary: how many test cases, and the
most important points from your notes.
