/* Runs nghttp2's HPACK decoder and encoder over a sequence of blocks
 * on one connection, and writes down what they produce for the Haskell
 * side to read: records of a tag, a length (4 octets, little-endian)
 * and that many octets.
 *
 *   N V      a decoded field's name, then its value
 *   E        the end of a decoded block
 *   B        an encoded block
 *   X        the end: an error; the octets are nghttp2's name for it
 *
 * Before block k, limits[k] >= 0 is a new SETTINGS_HEADER_TABLE_SIZE,
 * acknowledged: given to the decoder or the encoder as nghttp2's
 * change_table_size.
 */
#include <nghttp2/nghttp2.h>
#include <stdint.h>
#include <string.h>

struct out { uint8_t *at; uint8_t *end; int full; };

static int put(struct out *o, char tag, const uint8_t *data, size_t n) {
  if ((size_t)(o->end - o->at) < n + 5) { o->full = 1; return -1; }
  *o->at++ = (uint8_t)tag;
  *o->at++ = n & 0xff; *o->at++ = (n >> 8) & 0xff;
  *o->at++ = (n >> 16) & 0xff; *o->at++ = (n >> 24) & 0xff;
  if (n) memcpy(o->at, data, n);
  o->at += n;
  return 0;
}

static void error(struct out *o, long code) {
  const char *s = nghttp2_strerror((int)code);
  put(o, 'X', (const uint8_t *)s, strlen(s));
}

static long finish(struct out *o, uint8_t *start) {
  return o->full ? -1 : (long)(o->at - start);
}

/* Decode the n blocks, one after another; lengths[k] is block k's. */
long hd_nghttp2_inflate(const uint8_t *in, const size_t *lengths, const long *limits,
                        size_t n, uint8_t *out, size_t capacity) {
  nghttp2_hd_inflater *inf;
  struct out o = { out, out + capacity, 0 };
  if (nghttp2_hd_inflate_new(&inf) != 0) return -2;
  for (size_t k = 0; k < n; k++) {
    const uint8_t *p = in;
    size_t left = lengths[k];
    in += lengths[k];
    if (limits[k] >= 0) {
      int rv = nghttp2_hd_inflate_change_table_size(inf, (size_t)limits[k]);
      if (rv != 0) { error(&o, rv); goto done; }
    }
    for (;;) {
      nghttp2_nv nv;
      int flags = 0;
      nghttp2_ssize rv = nghttp2_hd_inflate_hd3(inf, &nv, &flags, p, left, 1);
      if (rv < 0) { error(&o, rv); goto done; }
      p += rv;
      left -= (size_t)rv;
      if (flags & NGHTTP2_HD_INFLATE_EMIT) {
        put(&o, 'N', nv.name, nv.namelen);
        put(&o, 'V', nv.value, nv.valuelen);
      }
      if (flags & NGHTTP2_HD_INFLATE_FINAL) {
        nghttp2_hd_inflate_end_headers(inf);
        put(&o, 'E', 0, 0);
        break;
      }
      if (!(flags & NGHTTP2_HD_INFLATE_EMIT) && left == 0) {
        put(&o, 'X', (const uint8_t *)"no end", 6);
        goto done;
      }
    }
  }
done:
  nghttp2_hd_inflate_del(inf);
  return finish(&o, out);
}

static uint32_t get32(const uint8_t **p) {
  const uint8_t *q = *p;
  *p += 4;
  return (uint32_t)q[0] | (uint32_t)q[1] << 8 | (uint32_t)q[2] << 16 | (uint32_t)q[3] << 24;
}

/* Encode n field lists. Each is a count (4 octets, little-endian), then
 * for each field the name's length, the name, the value's length and
 * the value. The encoder's own cap on its table is max_table. */
long hd_nghttp2_deflate(const uint8_t *in, const long *limits, size_t n, size_t max_table,
                        uint8_t *out, size_t capacity) {
  nghttp2_hd_deflater *def;
  struct out o = { out, out + capacity, 0 };
  if (nghttp2_hd_deflate_new(&def, max_table) != 0) return -2;
  for (size_t k = 0; k < n; k++) {
    uint32_t count = get32(&in);
    nghttp2_nv nva[count ? count : 1];
    for (uint32_t i = 0; i < count; i++) {
      nva[i].namelen = get32(&in); nva[i].name = (uint8_t *)in; in += nva[i].namelen;
      nva[i].valuelen = get32(&in); nva[i].value = (uint8_t *)in; in += nva[i].valuelen;
      nva[i].flags = NGHTTP2_NV_FLAG_NONE;
    }
    if (limits[k] >= 0) {
      int rv = nghttp2_hd_deflate_change_table_size(def, (size_t)limits[k]);
      if (rv != 0) { error(&o, rv); goto done; }
    }
    size_t bound = nghttp2_hd_deflate_bound(def, nva, count);
    if ((size_t)(o.end - o.at) < bound + 5) { o.full = 1; goto done; }
    nghttp2_ssize rv = nghttp2_hd_deflate_hd2(def, o.at + 5, bound, nva, count);
    if (rv < 0) { error(&o, rv); goto done; }
    o.at[0] = 'B';
    o.at[1] = rv & 0xff; o.at[2] = (rv >> 8) & 0xff;
    o.at[3] = (rv >> 16) & 0xff; o.at[4] = (rv >> 24) & 0xff;
    o.at += 5 + rv;
  }
done:
  nghttp2_hd_deflate_del(def);
  return finish(&o, out);
}

/* A whole server session: the client's octets (preface first) given to
 * nghttp2 at once, after a SETTINGS like the core's own (D9) and its
 * connection window raised to 1 MiB, as the core's preface does.
 * Records:
 *
 *   R  a request was received: its stream (4 octets, little-endian)
 *   F  a frame was received: type, flags, stream (4 octets) -- for the
 *      survey's examples
 *   O  everything nghttp2 then wrote, its frames for the Haskell side
 *      to read
 *   X  nghttp2_session_mem_recv2 failed: its name for the error
 */
static int on_frame(nghttp2_session *s, const nghttp2_frame *f, void *user) {
  (void)s;
  struct out *o = user;
  uint8_t b[6] = { f->hd.type, f->hd.flags,
                   f->hd.stream_id & 0xff, (f->hd.stream_id >> 8) & 0xff,
                   (f->hd.stream_id >> 16) & 0xff, (f->hd.stream_id >> 24) & 0xff };
  put(o, 'F', b, 6);
  if (f->hd.type == NGHTTP2_HEADERS && f->headers.cat == NGHTTP2_HCAT_REQUEST)
    put(o, 'R', b + 2, 4);
  return 0;
}

long h2_nghttp2_session(const uint8_t *in, size_t len, uint8_t *out, size_t capacity) {
  struct out o = { out, out + capacity, 0 };
  nghttp2_session_callbacks *cb;
  nghttp2_session *s;
  if (nghttp2_session_callbacks_new(&cb) != 0) return -2;
  nghttp2_session_callbacks_set_on_frame_recv_callback(cb, on_frame);
  if (nghttp2_session_server_new(&s, cb, &o) != 0) return -2;
  nghttp2_session_callbacks_del(cb);
  nghttp2_settings_entry iv[] = {
    { NGHTTP2_SETTINGS_MAX_CONCURRENT_STREAMS, 100 },
    { NGHTTP2_SETTINGS_INITIAL_WINDOW_SIZE, 1048576 },
    { NGHTTP2_SETTINGS_MAX_HEADER_LIST_SIZE, 32768 },
    { NGHTTP2_SETTINGS_NO_RFC7540_PRIORITIES, 1 } };
  nghttp2_submit_settings(s, NGHTTP2_FLAG_NONE, iv, 4);
  nghttp2_session_set_local_window_size(s, NGHTTP2_FLAG_NONE, 0, 1048576);
  nghttp2_ssize rv = nghttp2_session_mem_recv2(s, in, len);
  if (rv < 0) {
    const char *e = nghttp2_strerror((int)rv);
    put(&o, 'X', (const uint8_t *)e, strlen(e));
  }
  for (;;) {
    const uint8_t *data;
    nghttp2_ssize n = nghttp2_session_mem_send2(s, &data);
    if (n <= 0) break;
    put(&o, 'O', data, (size_t)n);
  }
  nghttp2_session_del(s);
  return finish(&o, out);
}
