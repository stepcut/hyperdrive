/* TLS over AWS-LC for hyperdrive (docs/tls-design.md; D3 of
 * docs/http2-core-design.md).
 *
 * Called from Haskell: the handshake's steps with `safe` calls, all else
 * with `unsafe` ones (T5). The library's callbacks (ALPN, the QUIC
 * hooks) only fill or read the session's buffers in C; nothing calls
 * back into Haskell. The library never touches a socket: for TCP,
 * ciphertext crosses through memory BIOs; for QUIC, handshake octets
 * cross by encryption level.
 *
 * Errors are values (T1): a call that fails returns a negative result
 * and leaves the library's message in the session (or in the caller's
 * buffer, for configuration). The library's error queue belongs to the
 * OS thread, and a Haskell thread making `safe` calls may move between
 * OS threads, so the message is taken within the failing call itself.
 */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
#include "hd_tls.h"

#define HD_LEVELS 4
#define HD_MAX_SECRETS 8
#define HD_ERROR_LEN 256

struct hd_tls_config {
  SSL_CTX *ctx;
  int server, quic;
  uint8_t alpn[256];         /* wire format: what a server may select, in
                                our order, or what a client offers */
  size_t alpn_len;
};

struct hd_buf { uint8_t *data; size_t len, cap; };

struct hd_secret { int level, direction; size_t len; uint8_t secret[64]; };

struct hd_tls {
  SSL *ssl;
  size_t max_buffered;       /* ciphertext received and not yet processed */
  char error[HD_ERROR_LEN];
  /* QUIC: handshake octets to send, by level; secrets as they come */
  struct hd_buf out[HD_LEVELS];
  struct hd_secret secrets[HD_MAX_SECRETS]; int nsecrets, next_secret;
  int alert;
};

/* the oldest error on this thread's queue, or the given text; the queue
   then cleared */
static void take_error(char *out, size_t cap, const char *otherwise) {
  unsigned long e = ERR_get_error();
  if (cap == 0) return;
  if (e) ERR_error_string_n(e, out, cap);
  else snprintf(out, cap, "%s", otherwise);
  ERR_clear_error();
}

static hd_tls *of(SSL *ssl) { return SSL_get_app_data(ssl); }

static int buf_append(struct hd_buf *b, const uint8_t *data, size_t len) {
  if (b->len + len > b->cap) {
    size_t cap = b->cap ? b->cap : 4096;
    while (cap < b->len + len) cap *= 2;
    uint8_t *d = realloc(b->data, cap);
    if (!d) return 0;
    b->data = d; b->cap = cap;
  }
  memcpy(b->data + b->len, data, len);
  b->len += len;
  return 1;
}

/* ---- ALPN ----------------------------------------------------------- */

static int select_alpn(SSL *ssl, const uint8_t **out, uint8_t *out_len,
                       const uint8_t *in, unsigned in_len, void *arg) {
  (void)ssl;
  hd_tls_config *c = arg;
  if (c->alpn_len == 0) return SSL_TLSEXT_ERR_NOACK;
  /* our order of preference: SSL_select_next_proto takes the first
     protocol of its first list that is in its second, so ours goes
     first (RFC 7301: the server selects; with none in common,
     no_application_protocol) */
  uint8_t *selected;
  if (SSL_select_next_proto(&selected, out_len, c->alpn, (unsigned)c->alpn_len, in, in_len)
      != OPENSSL_NPN_NEGOTIATED)
    return SSL_TLSEXT_ERR_ALERT_FATAL;
  *out = selected;
  return SSL_TLSEXT_ERR_OK;
}

/* ---- QUIC hooks ----------------------------------------------------- */

static int add_secret(SSL *ssl, enum ssl_encryption_level_t level, int direction,
                      const uint8_t *secret, size_t len) {
  hd_tls *t = of(ssl);
  if (t->nsecrets == HD_MAX_SECRETS || len > 64) return 0;
  struct hd_secret *s = &t->secrets[t->nsecrets++];
  s->level = level; s->direction = direction; s->len = len;
  memcpy(s->secret, secret, len);
  return 1;
}

static int quic_read_secret(SSL *ssl, enum ssl_encryption_level_t level, const SSL_CIPHER *cipher,
                            const uint8_t *secret, size_t len) {
  (void)cipher;
  return add_secret(ssl, level, 0, secret, len);
}

static int quic_write_secret(SSL *ssl, enum ssl_encryption_level_t level, const SSL_CIPHER *cipher,
                             const uint8_t *secret, size_t len) {
  (void)cipher;
  return add_secret(ssl, level, 1, secret, len);
}

static int quic_add_handshake_data(SSL *ssl, enum ssl_encryption_level_t level, const uint8_t *data, size_t len) {
  return buf_append(&of(ssl)->out[level], data, len);
}

static int quic_flush_flight(SSL *ssl) { (void)ssl; return 1; }

static int quic_send_alert(SSL *ssl, enum ssl_encryption_level_t level, uint8_t alert) {
  (void)level;
  of(ssl)->alert = alert;
  return 1;
}

static const SSL_QUIC_METHOD quic_method = {
  quic_read_secret, quic_write_secret, quic_add_handshake_data, quic_flush_flight, quic_send_alert };

/* ---- configuration -------------------------------------------------- */

hd_tls_config *hd_tls_config_new(int server, int quic, char *err, size_t errcap) {
  ERR_clear_error();
  hd_tls_config *c = calloc(1, sizeof *c);
  if (!c) { snprintf(err, errcap, "out of memory"); return NULL; }
  c->server = server;
  c->quic = quic;
  c->ctx = SSL_CTX_new(TLS_method());
  if (!c->ctx) { take_error(err, errcap, "SSL_CTX_new"); free(c); return NULL; }
  /* RFC 9113 Section 9.2: TLS 1.2 or later; QUIC needs TLS 1.3 */
  SSL_CTX_set_min_proto_version(c->ctx, quic ? TLS1_3_VERSION : TLS1_2_VERSION);
  SSL_CTX_set_max_proto_version(c->ctx, TLS1_3_VERSION);
  /* RFC 9113 Section 9.2.2 and Appendix A: TLS 1.2 with ephemeral key
     exchange and AEAD ciphers only, for the whole listener, so that h2
     cannot be negotiated over a prohibited suite (T2) */
  if (!SSL_CTX_set_strict_cipher_list(c->ctx,
        "ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:"
        "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:"
        "ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305")) {
    take_error(err, errcap, "cipher list");
    hd_tls_config_free(c);
    return NULL;
  }
  if (server) {
    /* our order of ciphers and groups, not the client's (T2); no
       resumption (T7): no TLS 1.3 tickets, no TLS 1.2 session cache.
       Renegotiation needs nothing: AWS-LC as a server rejects every one
       (RFC 9113 Section 9.2.1). */
    SSL_CTX_set_options(c->ctx, SSL_OP_CIPHER_SERVER_PREFERENCE | SSL_OP_NO_TICKET);
    SSL_CTX_set_session_cache_mode(c->ctx, SSL_SESS_CACHE_OFF);
    SSL_CTX_set_alpn_select_cb(c->ctx, select_alpn, c);
  }
  return c;
}

/* a key without a passphrase only: an encrypted key fails at once
   instead of the library asking for one (T3) */
static int no_passphrase(char *buf, int size, int rwflag, void *userdata) {
  (void)buf; (void)size; (void)rwflag; (void)userdata;
  return -1;
}

int hd_tls_config_set_cert(hd_tls_config *c, const uint8_t *chain, size_t chain_len,
                           const uint8_t *key, size_t key_len, char *err, size_t errcap) {
  ERR_clear_error();
  BIO *cb = BIO_new_mem_buf(chain, (long)chain_len);
  BIO *kb = BIO_new_mem_buf(key, (long)key_len);
  int ok = 0;
  if (!cb || !kb) { take_error(err, errcap, "BIO_new_mem_buf"); goto done; }
  X509 *leaf = PEM_read_bio_X509(cb, NULL, no_passphrase, NULL);
  if (!leaf) { take_error(err, errcap, "no certificate in the chain"); goto done; }
  ok = SSL_CTX_use_certificate(c->ctx, leaf);
  X509_free(leaf);
  if (!ok) { take_error(err, errcap, "certificate"); goto done; }
  /* the rest of the chain, in order */
  for (;;) {
    X509 *x = PEM_read_bio_X509(cb, NULL, no_passphrase, NULL);
    if (!x) { ERR_clear_error(); break; }   /* the end of the PEM */
    if (!SSL_CTX_add0_chain_cert(c->ctx, x)) {
      X509_free(x);
      take_error(err, errcap, "chain certificate");
      ok = 0;
      goto done;
    }
  }
  EVP_PKEY *pkey = PEM_read_bio_PrivateKey(kb, NULL, no_passphrase, NULL);
  if (!pkey) { take_error(err, errcap, "no key, or an encrypted one"); ok = 0; goto done; }
  ok = SSL_CTX_use_PrivateKey(c->ctx, pkey);
  EVP_PKEY_free(pkey);
  if (!ok) { take_error(err, errcap, "key"); goto done; }
  /* the key must be the certificate's (T3); the signature algorithms
     offered then follow from the key's type */
  ok = SSL_CTX_check_private_key(c->ctx);
  if (!ok) take_error(err, errcap, "the key does not match the certificate");
done:
  BIO_free(cb);
  BIO_free(kb);
  return ok ? 0 : -1;
}

int hd_tls_config_set_alpn(hd_tls_config *c, const uint8_t *wire, size_t len) {
  if (len > sizeof c->alpn) return -1;
  memcpy(c->alpn, wire, len);
  c->alpn_len = len;
  /* a client offers its list; SSL_CTX_set_alpn_protos returns zero on
     success, against the usual convention */
  if (!c->server && SSL_CTX_set_alpn_protos(c->ctx, wire, len) != 0) return -1;
  return 0;
}

int hd_tls_config_set_groups(hd_tls_config *c, const char *groups, char *err, size_t errcap) {
  ERR_clear_error();
  if (!SSL_CTX_set1_groups_list(c->ctx, groups)) { take_error(err, errcap, "groups"); return -1; }
  return 0;
}

int hd_tls_config_set_versions(hd_tls_config *c, int min_version, int max_version) {
  return SSL_CTX_set_min_proto_version(c->ctx, (uint16_t)min_version)
      && SSL_CTX_set_max_proto_version(c->ctx, (uint16_t)max_version) ? 0 : -1;
}

/* for a client in the tests: offer other TLS 1.2 ciphers */
int hd_tls_config_set_ciphers(hd_tls_config *c, const char *list, char *err, size_t errcap) {
  ERR_clear_error();
  if (!SSL_CTX_set_strict_cipher_list(c->ctx, list)) { take_error(err, errcap, "ciphers"); return -1; }
  return 0;
}

void hd_tls_config_free(hd_tls_config *c) {
  if (!c) return;
  SSL_CTX_free(c->ctx);
  free(c);
}

/* ---- sessions ------------------------------------------------------- */

hd_tls *hd_tls_new(hd_tls_config *c, const char *servername, size_t max_buffered) {
  hd_tls *t = calloc(1, sizeof *t);
  if (!t) return NULL;
  ERR_clear_error();
  t->ssl = SSL_new(c->ctx);
  if (!t->ssl) { ERR_clear_error(); free(t); return NULL; }
  t->max_buffered = max_buffered;
  SSL_set_app_data(t->ssl, t);
  if (c->server) SSL_set_accept_state(t->ssl);
  else {
    SSL_set_connect_state(t->ssl);
    if (servername) SSL_set_tlsext_host_name(t->ssl, servername);
  }
  if (c->quic) {
    SSL_set_quic_method(t->ssl, &quic_method);
  } else {
    BIO *rbio = BIO_new(BIO_s_mem()), *wbio = BIO_new(BIO_s_mem());
    if (!rbio || !wbio) { BIO_free(rbio); BIO_free(wbio); SSL_free(t->ssl); free(t); return NULL; }
    /* an empty read BIO means "wait for more", not the end */
    BIO_set_mem_eof_return(rbio, -1);
    SSL_set_bio(t->ssl, rbio, wbio);
  }
  return t;
}

void hd_tls_free(hd_tls *t) {
  if (!t) return;
  SSL_free(t->ssl);
  for (int i = 0; i < HD_LEVELS; i++) free(t->out[i].data);
  free(t);
}

const char *hd_tls_error(hd_tls *t) { return t->error; }

int hd_tls_handshake(hd_tls *t) {
  ERR_clear_error();
  int r = SSL_do_handshake(t->ssl);
  if (r == 1) return HD_DONE;
  switch (SSL_get_error(t->ssl, r)) {
    case SSL_ERROR_WANT_READ: return HD_WANT_READ;
    default: take_error(t->error, sizeof t->error, "handshake"); return HD_ERROR;
  }
}

/* TCP: ciphertext in, refused beyond the bound on what is buffered */
int hd_tls_feed(hd_tls *t, const uint8_t *in, size_t len) {
  BIO *rbio = SSL_get_rbio(t->ssl);
  if (BIO_ctrl_pending(rbio) + len > t->max_buffered) {
    snprintf(t->error, sizeof t->error, "more than %zu octets of input buffered", t->max_buffered);
    return HD_ERROR;
  }
  if (BIO_write(rbio, in, (int)len) != (int)len) {
    take_error(t->error, sizeof t->error, "BIO_write");
    return HD_ERROR;
  }
  return 0;
}

/* TCP: ciphertext out */
long hd_tls_pending_out(hd_tls *t) { return (long)BIO_ctrl_pending(SSL_get_wbio(t->ssl)); }

long hd_tls_drain(hd_tls *t, uint8_t *out, size_t cap) {
  int n = BIO_read(SSL_get_wbio(t->ssl), out, (int)cap);
  return n > 0 ? n : 0;
}

/* plaintext: >0 octets; 0, none until more input; HD_CLOSED, the peer's
   close_notify; HD_ERROR, an error (an alert may be waiting to be sent) */
long hd_tls_read(hd_tls *t, uint8_t *out, size_t cap) {
  ERR_clear_error();
  int n = SSL_read(t->ssl, out, (int)cap);
  if (n > 0) return n;
  switch (SSL_get_error(t->ssl, n)) {
    case SSL_ERROR_WANT_READ: return 0;
    case SSL_ERROR_ZERO_RETURN: return HD_CLOSED;
    default: take_error(t->error, sizeof t->error, "read"); return HD_ERROR;
  }
}

long hd_tls_write(hd_tls *t, const uint8_t *in, size_t len) {
  ERR_clear_error();
  int n = SSL_write(t->ssl, in, (int)len);
  if (n > 0) return n;
  take_error(t->error, sizeof t->error, "write");
  return HD_ERROR;
}

/* close_notify queued: the first stage of SSL_shutdown, the one most
   callers should stop at (T6) */
int hd_tls_shutdown(hd_tls *t) {
  ERR_clear_error();
  int r = SSL_shutdown(t->ssl);
  if (r >= 0) return 0;
  take_error(t->error, sizeof t->error, "shutdown");
  return HD_ERROR;
}

/* a client asks for a key update and the peer's (for the tests) */
int hd_tls_key_update(hd_tls *t) {
  ERR_clear_error();
  if (SSL_key_update(t->ssl, SSL_KEY_UPDATE_REQUESTED)) return 0;
  take_error(t->error, sizeof t->error, "key update");
  return HD_ERROR;
}

/* what was agreed */
const uint8_t *hd_tls_alpn(hd_tls *t, unsigned *len) {
  const uint8_t *p; SSL_get0_alpn_selected(t->ssl, &p, len); return p;
}
const char *hd_tls_servername(hd_tls *t) { return SSL_get_servername(t->ssl, TLSEXT_NAMETYPE_host_name); }
int hd_tls_version(hd_tls *t) { return SSL_version(t->ssl); }
const char *hd_tls_cipher(hd_tls *t) {
  const SSL_CIPHER *c = SSL_get_current_cipher(t->ssl); return c ? SSL_CIPHER_get_name(c) : "";
}
const char *hd_tls_group(hd_tls *t) {
  const char *g = SSL_get_group_name(SSL_get_group_id(t->ssl)); return g ? g : "";
}
int hd_tls_peer_signature_algorithm(hd_tls *t) { return SSL_get_peer_signature_algorithm(t->ssl); }
/* for a client: whether the server gave it a way to resume (a ticket, or
   a TLS 1.2 session ID it would keep) */
int hd_tls_resumable(hd_tls *t) {
  SSL_SESSION *s = SSL_get_session(t->ssl);
  return s && SSL_SESSION_is_resumable(s);
}
/* the certificates the peer sent (a client counts the leaf too) */
int hd_tls_peer_certificates(hd_tls *t) {
  STACK_OF(X509) *chain = SSL_get_peer_cert_chain(t->ssl);
  return chain ? (int)sk_X509_num(chain) : 0;
}

/* ---- QUIC ----------------------------------------------------------- */

int hd_tls_quic_set_transport_params(hd_tls *t, const uint8_t *params, size_t len) {
  ERR_clear_error();
  if (SSL_set_quic_transport_params(t->ssl, params, len)) return 0;
  take_error(t->error, sizeof t->error, "transport parameters");
  return HD_ERROR;
}

int hd_tls_quic_provide(hd_tls *t, int level, const uint8_t *in, size_t len) {
  ERR_clear_error();
  if (len > SSL_quic_max_handshake_flight_len(t->ssl, (enum ssl_encryption_level_t)level)) {
    snprintf(t->error, sizeof t->error, "a handshake flight over the limit at level %d", level);
    return HD_ERROR;
  }
  if (SSL_provide_quic_data(t->ssl, (enum ssl_encryption_level_t)level, in, len)) return 0;
  take_error(t->error, sizeof t->error, "provide");
  return HD_ERROR;
}

/* the handshake octets waiting at the lowest level that has any:
   >0 octets (and *level), 0 none, HD_ERROR if cap is too small */
long hd_tls_quic_take(hd_tls *t, int *level, uint8_t *out, size_t cap) {
  for (int l = 0; l < HD_LEVELS; l++) {
    struct hd_buf *b = &t->out[l];
    if (b->len == 0) continue;
    if (b->len > cap) return HD_ERROR;
    memcpy(out, b->data, b->len);
    long n = (long)b->len;
    b->len = 0;
    *level = l;
    return n;
  }
  return 0;
}

/* the next secret not yet taken: its length, 0 if none */
long hd_tls_quic_secret(hd_tls *t, int *level, int *direction, uint8_t *out, size_t cap) {
  if (t->next_secret == t->nsecrets) return 0;
  struct hd_secret *s = &t->secrets[t->next_secret++];
  if (s->len > cap) return HD_ERROR;
  *level = s->level; *direction = s->direction;
  memcpy(out, s->secret, s->len);
  return (long)s->len;
}

int hd_tls_quic_alert(hd_tls *t) { return t->alert; }
