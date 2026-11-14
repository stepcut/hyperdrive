hyperdrive is a fast, low-level HTTP server written in
Haskell. hyperdrive will use a variety of techniques to ensure that it
provides a correct implementation and reliable performance.

This code is not yet fit for human consumption.


Requirements traceability
-------------------------

Conformance to the HTTP RFCs is tracked with
[duvet](https://github.com/awslabs/duvet). Code that implements a
requirement cites the exact text of the RFC:

    -- = https://www.rfc-editor.org/rfc/rfc9110#section-5.5
    -- # a recipient of CR, LF, or NUL within
    -- # a field value MUST either reject the message

Annotations in test modules are counted as tests. To generate the
compliance report in `.duvet/reports/`:

    nix develop -c duvet report
