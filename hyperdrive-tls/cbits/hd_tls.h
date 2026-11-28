/* TLS over AWS-LC for hyperdrive (docs/tls-design.md). */
#ifndef HD_TLS_H
#define HD_TLS_H
#include <stddef.h>
#include <stdint.h>

typedef struct hd_tls_config hd_tls_config;
typedef struct hd_tls hd_tls;

/* results */
#define HD_DONE 0
#define HD_WANT_READ 1
#define HD_ERROR (-1)
#define HD_CLOSED (-2)

hd_tls_config *hd_tls_config_new(int server, int quic, char *err, size_t errcap);
int hd_tls_config_set_cert(hd_tls_config *c, const uint8_t *chain, size_t chain_len,
                           const uint8_t *key, size_t key_len, char *err, size_t errcap);
int hd_tls_config_set_alpn(hd_tls_config *c, const uint8_t *wire, size_t len);
int hd_tls_config_set_groups(hd_tls_config *c, const char *groups, char *err, size_t errcap);
int hd_tls_config_set_versions(hd_tls_config *c, int min_version, int max_version);
int hd_tls_config_set_ciphers(hd_tls_config *c, const char *list, char *err, size_t errcap);
void hd_tls_config_free(hd_tls_config *c);

hd_tls *hd_tls_new(hd_tls_config *c, const char *servername, size_t max_buffered);
void hd_tls_free(hd_tls *t);
const char *hd_tls_error(hd_tls *t);
int hd_tls_handshake(hd_tls *t);

int hd_tls_feed(hd_tls *t, const uint8_t *in, size_t len);
long hd_tls_pending_out(hd_tls *t);
long hd_tls_drain(hd_tls *t, uint8_t *out, size_t cap);
long hd_tls_read(hd_tls *t, uint8_t *out, size_t cap);
long hd_tls_write(hd_tls *t, const uint8_t *in, size_t len);
int hd_tls_shutdown(hd_tls *t);
int hd_tls_key_update(hd_tls *t);

const uint8_t *hd_tls_alpn(hd_tls *t, unsigned *len);
const char *hd_tls_servername(hd_tls *t);
int hd_tls_version(hd_tls *t);
const char *hd_tls_cipher(hd_tls *t);
const char *hd_tls_group(hd_tls *t);
int hd_tls_peer_signature_algorithm(hd_tls *t);
int hd_tls_peer_certificates(hd_tls *t);
int hd_tls_resumable(hd_tls *t);

int hd_tls_quic_set_transport_params(hd_tls *t, const uint8_t *params, size_t len);
int hd_tls_quic_provide(hd_tls *t, int level, const uint8_t *in, size_t len);
long hd_tls_quic_take(hd_tls *t, int *level, uint8_t *out, size_t cap);
long hd_tls_quic_secret(hd_tls *t, int *level, int *direction, uint8_t *out, size_t cap);
int hd_tls_quic_alert(hd_tls *t);

#endif
