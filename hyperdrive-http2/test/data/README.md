# Test data

`rfc7541.txt` is a copy of the RFC in `.duvet/specifications/`, so
that the tests can read the Huffman code (Appendix B), the static table
(Appendix A) and the examples (Appendix C) from the published text. The
package is built from this directory alone, so it cannot read duvet's
copy. `nix flake check` fails if the two differ.
