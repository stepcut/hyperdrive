# Errata

The errata of the RFCs hyperdrive cites, as of 2026-11-17, and what was
done about each one that matters. Duvet checks citations against the
published text, so it cannot see errata: when a cited section has a
verified erratum, the code follows the corrected text and says so in a
comment.

Status is the RFC Editor's: verified, held for document update (the
correction is agreed but waits for a revision), reported (not yet
reviewed) or rejected. Rejected errata are not listed unless they bear on
a decision here, and editorial errata are listed only when they touch
something hyperdrive uses.

To review again, compare https://www.rfc-editor.org/errata/rfcNNNN with
this file.

## RFC 9110 (HTTP Semantics)

| Erratum | Status | Section | Change | Decision |
|---|---|---|---|---|
| 7306 | verified, technical | 14.1.1 | `ranges-specifier = range-unit "=" OWS range-set`: whitespace after the "=" | Applied in `httpLibrary` (`rfc9110Errata`). The RFC's own example `bytes= 0-999, 4500-5499, -1000` needs it; a test checks that. Apply it in the Range parser when there is one. |
| 7138 | verified, technical | 12.5.1 | a quality value in the media type example table: 0.7 is 0.3 | Nothing to do: hyperdrive does not use the table. Test against the corrected value if content negotiation is tested with it. |
| 7419 | verified, technical | 8.3.2 | "Accept-Encoding" should read "Accept-Charset" | Nothing to do yet. |
| 7109 | verified, editorial | 15.4.9 | a user agent MUST NOT change the method when following a 308 | Client requirement; not hyperdrive's. |
| 7105, 8268 | verified, editorial | B.1, B.2 | change notes | Nothing to do. |
| 9164 | rejected | Appendix A | proposed saying that Appendix A is a normalised rendering of the ABNF | The test comparing each RFC's sections with its Appendix A confirms it: they agree up to spelling (for example `%s"GMT"` is written `%x47.4D.54`). |

## RFC 9112 (HTTP/1.1)

| Erratum | Status | Section | Change | Decision |
|---|---|---|---|---|
| 7744 | verified, editorial | 1.2, Appendix A | `obs-text` is defined in RFC 9110 Section 5.5, not 5.6.4 | Found by the tests before it was looked up. Applied in `httpLibrary` (`rfc9112Errata`), and the import check in hyperdrive-abnf's `RFCTests` uses the corrected section. |
| 8284 | verified, editorial | C.3 | a change note about the absolute form taking priority over Host | Nothing to do now; the behaviour itself belongs to the HTTP/1.1 package (RFC 9112 Section 3.2.2). |

## RFC 3986 (URI Generic Syntax)

| Erratum | Status | Section | Change | Decision |
|---|---|---|---|---|
| 2525 | verified, technical | 3.1 | a reference: RFC 2718 is RFC 4395 | Nothing to do. |
| 4942 | held, technical | 3.2.2, Appendix A | restrict `reg-name` to letters, digits, "-" and "." | Not applied: it is not verified, and RFC 9110 uses the published `reg-name`. hyperdrive keeps the RFC 3986 grammar for parsing and applies DNS rules separately (`isDnsHostName`), which is the same split. |
| 5428 | held, technical | 4.2 | add `path-abempty` to `relative-part` | Not implemented yet. RFC 9110 uses `relative-part` in `partial-URI` (Content-Location); revisit when that is parsed. |
| 2033 | held, editorial | 3.3, Appendix A | `path-empty = 0<pchar>` should be `""` | No change needed: the two match the same, and the recognizer treats `0<prose>` as matching the empty string. |
| 2933, 4547, 4789 | held, technical/editorial | 5.2.2, 5.2.3, 5.4.2 | reference resolution: merge arguments, a base path ending in "/..", abnormal examples | hyperdrive does not resolve references. Apply these if it ever does. |
| 2624 | held, technical | Appendix B | escape "/" in the regular expression | hyperdrive does not use the regular expression. |
| 9147 | reported | 2.3 | add backslash to `unreserved` | Not reviewed by the RFC Editor; ignored. |
| 4393, 4394 | rejected | 3.2.2 | reorder `dec-octet`, and rewrite `IPv6address`, for parsers that take the first alternative that matches | Not applied. ABNF alternatives have no order, and the recognizer tries every one, so the published rules are already unambiguous for it. |

## RFC 5952 (IPv6 Text Representation)

| Erratum | Status | Section | Change | Decision |
|---|---|---|---|---|
| 6272 | held, technical | 4.2.1 | "shorten as much as possible" applies only to runs of two or more zero fields, so that it does not conflict with 4.2.2 | Followed: `renderIPv6` never shortens a single zero field. The citation of 4.2.1 in `URI.hs` notes this. |
| 2498 | verified, editorial | 6 | a typo | Nothing to do. |
| 2656, 4483 | rejected | 4.3 | proposed upper case hex digits | Lower case, as published. |

## RFC 5234 (ABNF) and RFC 7405 (case-sensitive strings)

| Erratum | Status | Section | Change | Decision |
|---|---|---|---|---|
| 2968 | verified, technical | 4 | `elements = alternation *WSP` | Applied in `Hyperdrive.ABNF.Parser`. |
| 3076 | verified, technical | 4 | `rulelist = 1*( rule / (*WSP c-nl) )` | Applied in `Hyperdrive.ABNF.Parser`. |
| 6173 | held, editorial | 3.6 | "If <b> is omitted, there is no upper limit" | Already the reading (`Repeat`'s maximum is `Nothing`). |
| 2820 | held, editorial | 4 | prose-val's comment: only ">" is excluded | Already the reading: the parser follows the rule, not the comment. |
| 1423, 3096, 4040, 4564, 5110 | rejected | various | | Not applied. (4040 proposed adding lower case letters to HEXDIG; they are already allowed, since ABNF strings are case-insensitive. RFC 9112's rejected erratum 7214 makes the same point about its HEXDIG description.) |

RFC 7405 has one erratum, 5334, rejected.
