# The TLS spike (D3, before step 7 of D12)

Status: S1-S6 decided as recommended (the user, 2026-11-27); built, D3 holds with one refinement ("Findings").

D3 (docs/http2-core-design.md) decided our own thin TLS binding: a
small C shim over AWS-LC, with OpenSSL 3.5 as an alternative behind the
same interface, serving both TCP (HTTP/1.1 and HTTP/2) and QUIC
(HTTP/3). Its note asks for "a small spike of the shim ... well before
step 7 of D12, since D3 is the decision costliest to reverse and the
last one D12 tests". This is that spike: the least code that shows
whether D3's shape works, before anything depends on it.

## What the headers already confirm

Read from AWS-LC 5.12.0's `openssl/ssl.h` (the version in nixpkgs; its
libraries are `libssl-awslc` and `libcrypto-awslc`, its headers under
`include/aws-lc`, so it sits beside OpenSSL 3.5.9 without a clash):

- Asynchronous signing: `SSL_PRIVATE_KEY_METHOD` (`sign`, `decrypt`,
  `complete`); `sign` may return `ssl_private_key_retry`, the handshake
  then fails with `SSL_ERROR_WANT_PRIVATE_KEY_OPERATION`, and is
  resumed once the operation is done, `complete` returning the result.
- QUIC: `SSL_QUIC_METHOD` (`set_read_secret`, `set_write_secret`,
  `add_handshake_data`, `flush_flight`, `send_alert`), with
  `SSL_provide_quic_data`, `SSL_process_quic_post_handshake`,
  `SSL_set_quic_transport_params`, `SSL_quic_max_handshake_flight_len`
  and the four `ssl_encryption_level_t` levels. "It is an error to call
  `SSL_read` and `SSL_write` in QUIC": QUIC encrypts its own packets
  from the secrets.
- ALPN: `SSL_CTX_set_alpn_select_cb`, a C callback; "the cipher suite
  is selected before negotiating ALPN", so HTTP/2's cipher constraints
  can be checked in it (`SSL_get_pending_cipher`).
- SNI: `SSL_get_servername`, and certificate selection by callback
  (`SSL_CTX_set_select_certificate_cb`, `SSL_CTX_set_cert_cb`).
- Renegotiation: "If `ssl` is a server, peer-initiated renegotiations
  are *always* rejected", which is what RFC 9113 Section 9.2 requires
  ("An endpoint MUST treat a TLS renegotiation as a connection error").
- OpenSSL 3.5.9's QUIC interface is `SSL_set_quic_tls_cbs` with an
  `OSSL_DISPATCH` table (crypto send and receive, yield secret,
  transport parameters, alert): different in form, the same in
  substance, so one interface of ours can sit over both.

What the headers cannot show, and the spike must: that these fit our
I/O model (non-blocking sockets owned by Haskell, no callback ever
entering Haskell, each `unsafe` call short), and what they cost.

## The questions it answers

1. A TLS 1.3 and 1.2 server handshake driven entirely through buffers:
   Haskell hands in the octets received and takes out the octets to
   send; the library never touches a socket.
2. Signing outside the `unsafe` calls: the private-key method returns
   "retry"; Haskell then makes one `safe` call that signs (in C, with
   the key the shim holds); the handshake resumes. Each `unsafe` call
   within D3's budget of about 100 µs, whatever the key (RSA-2048
   signing takes half a millisecond or more, so it must be the `safe`
   one; the spike measures it).
3. ALPN choosing `h2` or `http/1.1`, SNI read, renegotiation refused,
   TLS 1.2 limited to ECDHE with AEAD ciphers (RFC 9113 Section 9.2.2
   and Appendix A).
4. The QUIC side through the same shim: a server and a client, both
   AWS-LC in one process, completing a handshake by passing each
   level's handshake data across, with both sides' secrets agreeing at
   each level. No QUIC packets: only that the interface serves RFC
   9001.
5. Costs: handshakes a second on one core (ECDSA P-256 and RSA-2048
   keys, TLS 1.3 and 1.2), and the cost of a record (encrypting and
   decrypting 16 KiB, and the copies through the shim's buffers).
6. That it builds and links from nixpkgs in our flake, beside OpenSSL.

## The interface (a sketch)

One opaque session per connection, in C; every function below is an
`unsafe` call except the one that signs.

```c
/* configuration: certificate chain, key, ALPN list; once per listener */
hd_tls_config *hd_tls_config_new(const uint8_t *cert_pem, size_t, const uint8_t *key_pem, size_t);
hd_tls       *hd_tls_new_server(hd_tls_config *);

/* TCP: ciphertext in and out, plaintext in and out */
int  hd_tls_feed(hd_tls *, const uint8_t *in, size_t len);      /* octets received */
int  hd_tls_handshake(hd_tls *);                                 /* HD_DONE, HD_WANT_READ, HD_WANT_SIGN, HD_ERROR */
int  hd_tls_sign(hd_tls *);                                      /* the safe call: makes the pending signature */
long hd_tls_read(hd_tls *, uint8_t *out, size_t cap);           /* plaintext available */
long hd_tls_write(hd_tls *, const uint8_t *in, size_t len);     /* plaintext to encrypt */
long hd_tls_drain(hd_tls *, uint8_t *out, size_t cap);          /* ciphertext to send */
const uint8_t *hd_tls_alpn(hd_tls *, size_t *len);
const char    *hd_tls_servername(hd_tls *);

/* QUIC: handshake octets by level, secrets as events */
int  hd_tls_quic_provide(hd_tls *, int level, const uint8_t *in, size_t len);
long hd_tls_quic_take(hd_tls *, int *level, uint8_t *out, size_t cap);   /* handshake data to send */
int  hd_tls_quic_secret(hd_tls *, int *level, int *direction, uint8_t *out, size_t cap);
```

The callbacks AWS-LC makes (ALPN, the private key, the QUIC hooks) only
fill or read the session's buffers in C. Haskell calls, then drains;
nothing calls Haskell. The Haskell side of the spike is a module of
`foreign import ccall unsafe` declarations and a loop that runs a
handshake over a loopback socket, enough to talk to real clients.

## Not in the spike

Session resumption and tickets (and their key rotation), 0-RTT, client
certificates, OCSP stapling, certificate reloading, kTLS, the driver's
integration (step 7), HTTP/1.1 or HTTP/2 over TLS, QUIC packets. Each is
step 7's or HTTP/3's, once the shape holds.

## Success

The spike passes if: curl, nghttp and `openssl s_client` complete
handshakes with it over TLS 1.3 and 1.2, ALPN choosing `h2` when offered
and `http/1.1` otherwise; a RSA-2048 handshake's signing happens only in
the `safe` call and no `unsafe` call takes more than 100 µs (measured);
the in-process QUIC handshake completes with matching secrets at every
level; and the costs are recorded. If any of these fails, D3 is
reopened with what was learned.

## Decisions

**S1. Where the code goes.** A new package, `hyperdrive-tls`: the C
shim (`cbits/`), its FFI module, and the spike's test program; the
server depends on it in step 7. Recommended, since HTTP/3 will use it
too. (Not chosen: inside `hyperdrive-server`.)

**S2. How octets cross.** AWS-LC's memory BIOs (`BIO_s_mem`) for the
spike: the simplest, at the cost of a copy each way, which the spike
measures. A custom BIO over buffers Haskell owns, saving the copies, is
for step 7 if the measurement says it matters. Recommended.

**S3. Keys.** ECDSA P-256 (the common choice now, signing fast enough to
be `unsafe` but taken through the same `safe` path for uniformity) and
RSA-2048 (signing half a millisecond or more: the case the asynchronous method
exists for). Both in the spike. Recommended.

**S4. OpenSSL 3.5 in the spike?** Not yet: AWS-LC first, as D3 says;
OpenSSL behind the same interface in step 7, once the interface has
settled. The spike only checks, from the headers above, that its QUIC
interface can sit under ours. Recommended.

**S5. The spike's code afterwards.** Kept as the start of
`hyperdrive-tls` if it passes, with tests added in step 7 (handshakes
against curl and nghttp in the test suite; the RFC 8446 and 9001
requirements that bind a server in duvet). A spike's code is not held
to the project's usual standard (mutants, citations) until then.
Recommended.

**S6. Test peers.** curl, nghttp2's `nghttp` and `h2load`, and
`openssl s_client`, all in nixpkgs, in a dev shell of the flake
(`.#tls`). Recommended.

## Findings (2026-11-27): D3 holds, with one refinement

Built as decided: `hyperdrive-tls` (the shim `cbits/hd_tls.c`, about
400 lines; `Hyperdrive.TLS.Shim`; the program `tls-spike`), the `.#tls`
shell, and `scripts/tls-spike`, which reproduces everything below (the
program and the script were removed when the shim was rewritten for
step 7, docs/tls-design.md; both are at ca9a4ef). The
machine was not quiet: the numbers are for orders of magnitude.

1. Handshakes through buffers work: with real clients, `openssl
   s_client` (TLS 1.3 and 1.2), curl (HTTP/1.1 answered; `h2` accepted)
   and nghttp (`h2` negotiated). ALPN chooses `h2` before `http/1.1`;
   with no ALPN the handshake completes; with nothing in common the
   client gets `no_application_protocol`. SNI is read. Only AEAD
   ciphers with ECDHE in TLS 1.2.
2. Signing through the asynchronous key method works as D3 meant: the
   signature is made in the `safe` call, about 21 µs for ECDSA P-256 and
   358 µs for RSA-2048 (RSA-PSS, `0x0804`, in TLS 1.3 and 1.2).
3. **The refinement.** AWS-LC 5.12 uses the hybrid post-quantum group
   X25519MLKEM768 for TLS 1.3 by default (X25519 for TLS 1.2). The
   server's handshake step that answers the ClientHello then takes
   about 80 µs at the 90th percentile and 96 to 113 at the 99th (the
   server's steps: median 13 µs; TLS 1.2's: 99th percentile about 45 µs).
   That is at D3's budget of about 100 µs for an `unsafe` call, and over
   it once the machine is busy. So the handshake's steps are `safe`
   calls too: a `safe` call costs a fraction of a microsecond more, which
   is nothing against tens of microseconds, and the measured handshake
   cost did not move beyond noise (127 to 130 µs). Record encryption
   and decryption, the per-request work, stay `unsafe`.
4. QUIC through the same shim works: an AWS-LC server and client in one
   process complete a TLS 1.3 handshake (ALPN `h3`) by passing each
   level's handshake octets across, the receiver stepped after each
   level as a QUIC stack must (it can take a level's data only once it
   has that level's keys: `WRONG_ENCRYPTION_LEVEL_RECEIVED` otherwise).
   Secrets come out for the handshake and application levels, each the
   other side's opposite, all agreeing; none for the initial level,
   which QUIC derives from the connection ID itself (RFC 9001). The
   server's session ticket arrives at the application level.
5. Costs on one core, server side: a TLS 1.3 handshake about 129 µs
   with ECDSA P-256 (466 µs with RSA-2048), TLS 1.2 about 85 µs (421);
   both sides together about 2,700 and 3,700 handshakes a second with
   ECDSA. A 16 KiB record: about 2.1 µs to encrypt and drain, 2.7 µs to
   feed and decrypt (about 7.5 and 6 GB/s), the copies through the
   memory BIOs included, so S2's copies are not worth avoiding yet.
6. It builds from nixpkgs: AWS-LC is `libssl-awslc` and
   `libcrypto-awslc` there, beside OpenSSL; the cabal file names them
   in `pkgconfig-depends` and the flake maps both to `pkgs.aws-lc`.

For step 7, beyond what the spike left out: a session's buffers grow
without bound (the read BIO and QUIC's per-level buffers), so they need
the limits the HTTP/2 core already has (and
`SSL_quic_max_handshake_flight_len` for QUIC); the hybrid group's larger
ClientHello and ServerHello (1,411 and 1,178 octets in the QUIC run) are
worth knowing for QUIC's first datagrams; the group list should be a
setting, X25519MLKEM768 by default.

## Decisions in short

- **S1.** A new package, `hyperdrive-tls`.
- **S2.** Memory BIOs for now; the copy measured.
- **S3.** ECDSA P-256 and RSA-2048.
- **S4.** AWS-LC only in the spike; OpenSSL 3.5 in step 7.
- **S5.** The code kept if it passes, brought up to standard in step 7.
- **S6.** curl, nghttp, h2load, `openssl s_client` in `.#tls`.
