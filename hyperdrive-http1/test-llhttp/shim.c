/* Runs llhttp over a request stream given in pieces and writes down
 * what it reports, for the Haskell side to read: records of a tag, a
 * length (4 octets, little-endian) and that many octets.
 *
 *   B                 a message begins
 *   M U V F H D       part of the method, the target, the version, a
 *                     field's name, a field's value, the body
 *   m u v f h         the end of each of the first five
 *   E                 the header section is complete
 *   C                 the message is complete
 *   X                 the end: llhttp's error number (1 octet), then
 *                     its name for it
 */
#include <llhttp.h>
#include <stdint.h>
#include <string.h>

struct out { uint8_t *at; uint8_t *end; int full; };

static int put(llhttp_t *p, char tag, const char *data, size_t n) {
  struct out *o = p->data;
  if ((size_t)(o->end - o->at) < n + 5) { o->full = 1; return -1; }
  *o->at++ = (uint8_t)tag;
  *o->at++ = n & 0xff; *o->at++ = (n >> 8) & 0xff;
  *o->at++ = (n >> 16) & 0xff; *o->at++ = (n >> 24) & 0xff;
  if (n) memcpy(o->at, data, n);
  o->at += n;
  return 0;
}

#define DATA(name, tag) static int name(llhttp_t *p, const char *at, size_t n) { return put(p, tag, at, n); }
#define MARK(name, tag) static int name(llhttp_t *p) { return put(p, tag, 0, 0); }
MARK(on_begin, 'B')
DATA(on_method, 'M') MARK(on_method_end, 'm')
DATA(on_url, 'U') MARK(on_url_end, 'u')
DATA(on_version, 'V') MARK(on_version_end, 'v')
DATA(on_field, 'F') MARK(on_field_end, 'f')
DATA(on_value, 'H') MARK(on_value_end, 'h')
MARK(on_headers, 'E')
DATA(on_body, 'D')
MARK(on_complete, 'C')

/* Returns the number of octets written, or -1 if they did not fit. */
long hd_llhttp_run(const char *input, const size_t *lengths, size_t pieces,
                   uint8_t *out, size_t capacity) {
  llhttp_settings_t settings;
  llhttp_t parser;
  struct out o = { out, out + capacity, 0 };
  llhttp_errno_t err = HPE_OK;
  const char *name;

  llhttp_settings_init(&settings);
  settings.on_message_begin = on_begin;
  settings.on_method = on_method; settings.on_method_complete = on_method_end;
  settings.on_url = on_url; settings.on_url_complete = on_url_end;
  settings.on_version = on_version; settings.on_version_complete = on_version_end;
  settings.on_header_field = on_field; settings.on_header_field_complete = on_field_end;
  settings.on_header_value = on_value; settings.on_header_value_complete = on_value_end;
  settings.on_headers_complete = on_headers;
  settings.on_body = on_body;
  settings.on_message_complete = on_complete;
  llhttp_init(&parser, HTTP_REQUEST, &settings);
  parser.data = &o;

  for (size_t i = 0; i < pieces && err == HPE_OK; i++) {
    err = llhttp_execute(&parser, input, lengths[i]);
    input += lengths[i];
  }
  if (o.full) return -1;
  name = llhttp_errno_name(err);
  if ((size_t)(o.end - o.at) < strlen(name) + 6) return -1;
  {
    size_t n = strlen(name) + 1;
    *o.at++ = 'X';
    *o.at++ = n & 0xff; *o.at++ = (n >> 8) & 0xff; *o.at++ = 0; *o.at++ = 0;
    *o.at++ = (uint8_t)err;
    memcpy(o.at, name, n - 1);
    o.at += n - 1;
  }
  return o.at - out;
}
