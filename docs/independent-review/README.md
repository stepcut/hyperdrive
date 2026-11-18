# Independent review

How hyperdrive-semantics is checked against the RFCs by a reviewer who
never sees its implementation, tests or design notes: only the documented
API and the RFC texts. The reviewer writes test cases from the RFC text,
each quoting the text it rests on, and notes on conflicts, ambiguities,
gaps and assumptions. Because they work from the RFCs and not from our
code, they catch readings we got wrong and documentation that promises
more than the RFCs say.

Reviews so far:

- 2026-11-15: `hyperdrive-semantics/test/IndependentTests.hs` and
  `IndependentTests.notes.md`.
- 2026-11-17, by a separate Claude Opus 5.5 agent:
  `hyperdrive-semantics/test/ReviewTests.hs` and `ReviewTests.notes.md`.
  `API-2026-11-17.md` is the API it saw.

## Running a review

1. Build the library, so that the reviewer's tests can be type-checked:
   `nix develop -c cabal build hyperdrive-semantics`.
2. Build a package, outside the repository so the reviewer has no reason
   to look in it: `docs/independent-review/prepare /tmp/review`. It holds
   `API.md` (from `scripts/extract-api`: export lists, signatures and
   documentation, no function bodies or duvet citations), the texts of
   RFC 9110, 9112, 3986, 5952, 1123 and 952, `INSTRUCTIONS.md`, and
   `check.sh`, which type-checks the reviewer's module against the
   compiled interface files and runs nothing.
3. Give the reviewer the package and `INSTRUCTIONS.md`, and nothing else.
   A model other than the one that wrote the code is more independent;
   a human reviewer more so. The instructions ask the reviewer to read
   only the package, never to run its tests (its expectations must come
   from the RFCs), and to produce `ReviewTests.hs` and
   `REVIEW-NOTES.md`.
4. Add the tests to the test suite, run them, and investigate every
   failure and every note against the RFC text. Fix, document or decide
   each one, and record what was done at the top of the notes, as in
   `ReviewTests.notes.md`. Change a reviewer's test only to follow a
   recorded decision, with a comment saying so.

`INSTRUCTIONS.md` here says what to concentrate on; update its scope to
what has changed since the last review. To compare two reviewers (for
example two models) on identical input, give both the same package; to
repeat the 2026-11-17 review exactly, use `API-2026-11-17.md` as
`API.md`.
