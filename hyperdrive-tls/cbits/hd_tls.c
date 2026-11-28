/* The TLS spike's shim over AWS-LC (docs/tls-spike-design.md).
 *
 * Every function here is called from Haskell with an `unsafe` call,
 * except hd_tls_sign, the one costly operation, which is `safe`. The
 * library's callbacks (ALPN, the private key, the QUIC hooks) only fill
 * or read the session's buffers; nothing calls back into Haskell. The
 * library never touches a socket: for TCP, ciphertext crosses through
 * memory BIOs (S2); for QUIC, handshake octets cross by encryption
 * level.
 */
#include <string.h>
#include <stdlib.h>
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/pem.h>
#include <openssl/bio.h>
#include "hd_tls.h"

#define HD_LEVELS 4
#define HD_MAX_SECRETS 8

struct hd_tls_config {
  SSL_CTX *ctx;
  EVP_PKEY *key;            /* signed with in hd_tls_sign only */
  int quic;
};

struct hd_buf { uint8_t *data; size_t len, cap; };

struct hd_secret { int level, direction; size_t len; uint8_t secret[64]; };

struct hd_tls {
  SSL *ssl;
  hd_tls_config *config;
  /* a pending signature (the asynchronous private key method) */
  int pending;
  uint16_t algorithm;
  uint8_t *to_sign; size_t to_sign_len;
  uint8_t signature[1024]; size_t signature_len; int signed_ok;
  /* QUIC: handshake octets to send, by level; secrets as they come */
  struct hd_buf out[HD_LEVELS];
  struct hd_secret secrets[HD_MAX_SECRETS]; int nsecrets, next_secret;
  int alert;
};

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

static hd_tls *of(SSL *ssl) { return SSL_get_app_data(ssl); }

/* ---- the asynchronous private key method ---------------------------- */

static enum ssl_private_key_result_t key_sign(SSL *ssl, uint8_t *out, size_t *out_len, size_t max_out,
                                              uint16_t algorithm, const uint8_t *in, size_t in_len) {
  (void)out; (void)out_len; (void)max_out;
  hd_tls *t = of(ssl);
  free(t->to_sign);
  t->to_sign = malloc(in_len);
  if (!t->to_sign) return ssl_private_key_failure;
  memcpy(t->to_sign, in, in_len);
  t->to_sign_len = in_len;
  t->algorithm = algorithm;
  t->pending = 1;
  t->signed_ok = 0;
  /* the signature is made by hd_tls_sign, a `safe` call, then the
     handshake resumes and `complete` hands it over */
  return ssl_private_key_retry;
}

static enum ssl_private_key_result_t key_decrypt(SSL *ssl, uint8_t *out, size_t *out_len, size_t max_out,
                                                 const uint8_t *in, size_t in_len) {
  /* RSA key exchange: not offered (RFC 9113 Section 9.2.2: ephemeral
     key exchange only) */
  (void)ssl; (void)out; (void)out_len; (void)max_out; (void)in; (void)in_len;
  return ssl_private_key_failure;
}

static enum ssl_private_key_result_t key_complete(SSL *ssl, uint8_t *out, size_t *out_len, size_t max_out) {
  hd_tls *t = of(ssl);
  if (t->pending) return ssl_private_key_retry;
  if (!t->signed_ok || t->signature_len > max_out) return ssl_private_key_failure;
  memcpy(out, t->signature, t->signature_len);
  *out_len = t->signature_len;
  t->signed_ok = 0;
  return ssl_private_key_success;
}

static const SSL_PRIVATE_KEY_METHOD key_method = { key_sign, key_decrypt, key_complete };

int hd_tls_sign(hd_tls *t) {
  if (!t->pending) return -1;
  const EVP_MD *md = SSL_get_signature_algorithm_digest(t->algorithm);
  EVP_MD_CTX *ctx = EVP_MD_CTX_new();
  EVP_PKEY_CTX *pctx = NULL;
  int ok = ctx && EVP_DigestSignInit(ctx, &pctx, md, NULL, t->config->key);
  if (ok && SSL_is_signature_algorithm_rsa_pss(t->algorithm))
    ok = EVP_PKEY_CTX_set_rsa_padding(pctx, RSA_PKCS1_PSS_PADDING)
         && EVP_PKEY_CTX_set_rsa_pss_saltlen(pctx, -1 /* the digest's length */);
  size_t len = sizeof t->signature;
  ok = ok && EVP_DigestSign(ctx, t->signature, &len, t->to_sign, t->to_sign_len);
  EVP_MD_CTX_free(ctx);
  t->pending = 0;
  t->signed_ok = ok;
  t->signature_len = ok ? len : 0;
  return ok ? 0 : -1;
}

/* ---- ALPN ----------------------------------------------------------- */

static const uint8_t alpn_tcp[] = "\x02h2\x08http/1.1";
static const uint8_t alpn_quic[] = "\x02h3";

static int select_alpn(SSL *ssl, const uint8_t **out, uint8_t *out_len,
                       const uint8_t *in, unsigned in_len, void *arg) {
  (void)ssl;
  hd_tls_config *c = arg;
  const uint8_t *ours = c->quic ? alpn_quic : alpn_tcp;
  unsigned ours_len = c->quic ? sizeof alpn_quic - 1 : sizeof alpn_tcp - 1;
  /* our order of preference, h2 before http/1.1: SSL_select_next_proto
     takes the first protocol of its first list that is in its second,
     so ours goes first (RFC 7301: the server selects; with no protocol
     in common, no_application_protocol) */
  uint8_t *selected;
  if (SSL_select_next_proto(&selected, out_len, ours, ours_len, in, in_len) != OPENSSL_NPN_NEGOTIATED)
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

static void config_common(SSL_CTX *ctx, int quic) {
  /* = RFC 9113 Section 9.2: TLS 1.2 or later; RFC 9001 Section 4.2:
     QUIC needs TLS 1.3 */
  SSL_CTX_set_min_proto_version(ctx, quic ? TLS1_3_VERSION : TLS1_2_VERSION);
  SSL_CTX_set_max_proto_version(ctx, TLS1_3_VERSION);
  /* = RFC 9113 Section 9.2.2 and Appendix A: TLS 1.2 with ephemeral
     key exchange and AEAD ciphers only (TLS 1.3's are all AEAD) */
  SSL_CTX_set_strict_cipher_list(ctx,
    "ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES128-GCM-SHA256:"
    "ECDHE-ECDSA-AES256-GCM-SHA384:ECDHE-RSA-AES256-GCM-SHA384:"
    "ECDHE-ECDSA-CHACHA20-POLY1305:ECDHE-RSA-CHACHA20-POLY1305");
}

hd_tls_config *hd_tls_config_server(const char *cert_file, const char *key_file, int quic) {
  hd_tls_config *c = calloc(1, sizeof *c);
  if (!c) return NULL;
  c->quic = quic;
  c->ctx = SSL_CTX_new(TLS_method());
  FILE *kf = fopen(key_file, "r");
  if (kf) { c->key = PEM_read_PrivateKey(kf, NULL, NULL, NULL); fclose(kf); }
  if (!c->ctx || !c->key || SSL_CTX_use_certificate_chain_file(c->ctx, cert_file) != 1) {
    hd_tls_config_free(c);
    return NULL;
  }
  config_common(c->ctx, quic);
  /* the key stays out of the library: it signs through key_method */
  SSL_CTX_set_private_key_method(c->ctx, &key_method);
  static const uint16_t ecdsa[] = { SSL_SIGN_ECDSA_SECP256R1_SHA256 };
  static const uint16_t rsa[] = { SSL_SIGN_RSA_PSS_RSAE_SHA256, SSL_SIGN_RSA_PSS_RSAE_SHA384,
                                  SSL_SIGN_RSA_PKCS1_SHA256, SSL_SIGN_RSA_PKCS1_SHA384 };
  if (EVP_PKEY_id(c->key) == EVP_PKEY_EC)
    SSL_CTX_set_signing_algorithm_prefs(c->ctx, ecdsa, sizeof ecdsa / sizeof ecdsa[0]);
  else
    SSL_CTX_set_signing_algorithm_prefs(c->ctx, rsa, sizeof rsa / sizeof rsa[0]);
  SSL_CTX_set_alpn_select_cb(c->ctx, select_alpn, c);
  /* = RFC 9113 Section 9.2.1: renegotiation is refused by AWS-LC
     whenever it is the server ("peer-initiated renegotiations are
     *always* rejected") */
  return c;
}

/* a client, for the in-process tests: no verification */
hd_tls_config *hd_tls_config_client(int quic) {
  hd_tls_config *c = calloc(1, sizeof *c);
  if (!c) return NULL;
  c->quic = quic;
  c->ctx = SSL_CTX_new(TLS_method());
  if (!c->ctx) { free(c); return NULL; }
  config_common(c->ctx, quic);
  if (quic) SSL_CTX_set_alpn_protos(c->ctx, alpn_quic, sizeof alpn_quic - 1);
  else SSL_CTX_set_alpn_protos(c->ctx, alpn_tcp, sizeof alpn_tcp - 1);
  return c;
}

int hd_tls_config_limit_version(hd_tls_config *c, int tls12_only) {
  return SSL_CTX_set_max_proto_version(c->ctx, tls12_only ? TLS1_2_VERSION : TLS1_3_VERSION);
}

void hd_tls_config_free(hd_tls_config *c) {
  if (!c) return;
  SSL_CTX_free(c->ctx);
  EVP_PKEY_free(c->key);
  free(c);
}

/* ---- sessions ------------------------------------------------------- */

hd_tls *hd_tls_new(hd_tls_config *c, int server, const char *servername) {
  hd_tls *t = calloc(1, sizeof *t);
  if (!t) return NULL;
  t->config = c;
  t->ssl = SSL_new(c->ctx);
  if (!t->ssl) { free(t); return NULL; }
  SSL_set_app_data(t->ssl, t);
  if (server) SSL_set_accept_state(t->ssl); else SSL_set_connect_state(t->ssl);
  if (!server && servername) SSL_set_tlsext_host_name(t->ssl, servername);
  if (c->quic) {
    static const uint8_t params[] = { 0x01, 0x02, 0x40, 0x64 }; /* max_idle_timeout 100: a placeholder */
    SSL_set_quic_method(t->ssl, &quic_method);
    SSL_set_quic_transport_params(t->ssl, params, sizeof params);
  } else {
    BIO *rbio = BIO_new(BIO_s_mem()), *wbio = BIO_new(BIO_s_mem());
    /* an empty read BIO means "wait for more", not the end */
    BIO_set_mem_eof_return(rbio, -1);
    SSL_set_bio(t->ssl, rbio, wbio);
  }
  return t;
}

void hd_tls_free(hd_tls *t) {
  if (!t) return;
  SSL_free(t->ssl);
  free(t->to_sign);
  for (int i = 0; i < HD_LEVELS; i++) free(t->out[i].data);
  free(t);
}

int hd_tls_handshake(hd_tls *t) {
  int r = SSL_do_handshake(t->ssl);
  if (r == 1) return HD_DONE;
  switch (SSL_get_error(t->ssl, r)) {
    case SSL_ERROR_WANT_READ: return HD_WANT_READ;
    case SSL_ERROR_WANT_PRIVATE_KEY_OPERATION: return HD_WANT_SIGN;
    default: return HD_ERROR;
  }
}

/* TCP: ciphertext in */
int hd_tls_feed(hd_tls *t, const uint8_t *in, size_t len) {
  return BIO_write(SSL_get_rbio(t->ssl), in, (int)len) == (int)len ? 0 : -1;
}

/* TCP: ciphertext out; how much is waiting */
long hd_tls_pending_out(hd_tls *t) { return (long)BIO_ctrl_pending(SSL_get_wbio(t->ssl)); }

long hd_tls_drain(hd_tls *t, uint8_t *out, size_t cap) {
  int n = BIO_read(SSL_get_wbio(t->ssl), out, (int)cap);
  return n > 0 ? n : 0;
}

/* plaintext: >0 octets, 0 none yet, -1 closed or an error */
long hd_tls_read(hd_tls *t, uint8_t *out, size_t cap) {
  int n = SSL_read(t->ssl, out, (int)cap);
  if (n > 0) return n;
  return SSL_get_error(t->ssl, n) == SSL_ERROR_WANT_READ ? 0 : -1;
}

long hd_tls_write(hd_tls *t, const uint8_t *in, size_t len) {
  int n = SSL_write(t->ssl, in, (int)len);
  return n > 0 ? n : -1;
}

int hd_tls_shutdown(hd_tls *t) { return SSL_shutdown(t->ssl); }

/* what was agreed */
const uint8_t *hd_tls_alpn(hd_tls *t, unsigned *len) {
  const uint8_t *p; SSL_get0_alpn_selected(t->ssl, &p, len); return p;
}
const char *hd_tls_servername(hd_tls *t) { return SSL_get_servername(t->ssl, TLSEXT_NAMETYPE_host_name); }
const char *hd_tls_version(hd_tls *t) { return SSL_get_version(t->ssl); }
const char *hd_tls_cipher(hd_tls *t) {
  const SSL_CIPHER *c = SSL_get_current_cipher(t->ssl); return c ? SSL_CIPHER_get_name(c) : "";
}
uint16_t hd_tls_signature_algorithm(hd_tls *t) { return t->algorithm; }
const char *hd_tls_group(hd_tls *t) {
  const char *g = SSL_get_group_name(SSL_get_group_id(t->ssl)); return g ? g : "";
}

/* the oldest error on this thread's queue, as text */
void hd_tls_error(char *out, size_t cap) {
  unsigned long e = ERR_get_error();
  if (e) ERR_error_string_n(e, out, cap); else if (cap) out[0] = 0;
}

/* ---- QUIC ----------------------------------------------------------- */

int hd_tls_quic_provide(hd_tls *t, int level, const uint8_t *in, size_t len) {
  return SSL_provide_quic_data(t->ssl, (enum ssl_encryption_level_t)level, in, len) ? 0 : -1;
}

/* the handshake octets waiting at the lowest level that has any:
   >0 octets (and *level), 0 none, -1 cap too small */
long hd_tls_quic_take(hd_tls *t, int *level, uint8_t *out, size_t cap) {
  for (int l = 0; l < HD_LEVELS; l++) {
    struct hd_buf *b = &t->out[l];
    if (b->len == 0) continue;
    if (b->len > cap) return -1;
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
  if (s->len > cap) return -1;
  *level = s->level; *direction = s->direction;
  memcpy(out, s->secret, s->len);
  return (long)s->len;
}

int hd_tls_quic_alert(hd_tls *t) { return t->alert; }
