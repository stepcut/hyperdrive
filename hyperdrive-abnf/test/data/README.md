# Test data

`rfc*.txt` are copies of the RFCs in `.duvet/specifications/`, so
that the tests can read grammars from the published text. The package
is built from this directory alone, so it cannot read duvet's copies.
`nix flake check` fails if the two differ.
