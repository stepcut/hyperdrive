/* The TLS spike's shim over AWS-LC (docs/tls-spike-design.md). */
#ifndef HD_TLS_H
#define HD_TLS_H
#include <stddef.h>
#include <stdint.h>

typedef struct hd_tls_config hd_tls_config;
typedef struct hd_tls hd_tls;

/* hd_tls_handshake's results */
#define HD_DONE 0
#define HD_WANT_READ 1
#define HD_WANT_SIGN 2
#define HD_ERROR (-1)

hd_tls_config *hd_tls_config_server(const char *cert_file, const char *key_file, int quic);
hd_tls_config *hd_tls_config_client(int quic);
int hd_tls_config_limit_version(hd_tls_config *c, int tls12_only);
void hd_tls_config_free(hd_tls_config *c);

hd_tls *hd_tls_new(hd_tls_config *c, int server, const char *servername);
void hd_tls_free(hd_tls *t);
int hd_tls_handshake(hd_tls *t);
int hd_tls_sign(hd_tls *t);

int hd_tls_feed(hd_tls *t, const uint8_t *in, size_t len);
long hd_tls_pending_out(hd_tls *t);
long hd_tls_drain(hd_tls *t, uint8_t *out, size_t cap);
long hd_tls_read(hd_tls *t, uint8_t *out, size_t cap);
long hd_tls_write(hd_tls *t, const uint8_t *in, size_t len);
int hd_tls_shutdown(hd_tls *t);

const uint8_t *hd_tls_alpn(hd_tls *t, unsigned *len);
const char *hd_tls_servername(hd_tls *t);
const char *hd_tls_version(hd_tls *t);
const char *hd_tls_cipher(hd_tls *t);
uint16_t hd_tls_signature_algorithm(hd_tls *t);
const char *hd_tls_group(hd_tls *t);
void hd_tls_error(char *out, size_t cap);

int hd_tls_quic_provide(hd_tls *t, int level, const uint8_t *in, size_t len);
long hd_tls_quic_take(hd_tls *t, int *level, uint8_t *out, size_t cap);
long hd_tls_quic_secret(hd_tls *t, int *level, int *direction, uint8_t *out, size_t cap);
int hd_tls_quic_alert(hd_tls *t);

#endif
