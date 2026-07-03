/*
** re_utf8.c - UTF-8 utility functions for regexp engine
**
** See Copyright Notice in mruby.h
*/

#include "re_internal.h"

/* Return byte length of UTF-8 character at s.
   Returns 1 for invalid sequences (treat as single byte). */
int
mrb_re_utf8_charlen(const char *s, const char *end)
{
  uint8_t c = (uint8_t)*s;
  int len;

  if (c < 0x80) return 1;
  else if (c < 0xc0) return 1;  /* invalid continuation */
  else if (c < 0xe0) len = 2;
  else if (c < 0xf0) len = 3;
  else if (c < 0xf8) len = 4;
  else return 1;  /* invalid */

  if (s + len > end) return 1;  /* truncated */
  return len;
}

/* Decode a UTF-8 character and return its codepoint.
   *len is set to the byte length consumed. Invalid or truncated
   sequences consume a single byte, mirroring mrb_re_utf8_charlen. */
uint32_t
mrb_re_utf8_decode(const char *s, const char *end, int *len)
{
  uint8_t c = (uint8_t)s[0];
  uint32_t cp;
  int n = mrb_re_utf8_charlen(s, end);

  *len = n;
  switch (n) {
  case 2:
    cp = (c & 0x1f) << 6;
    cp |= ((uint8_t)s[1] & 0x3f);
    return cp;
  case 3:
    cp = (c & 0x0f) << 12;
    cp |= ((uint8_t)s[1] & 0x3f) << 6;
    cp |= ((uint8_t)s[2] & 0x3f);
    return cp;
  case 4:
    cp = (c & 0x07) << 18;
    cp |= ((uint8_t)s[1] & 0x3f) << 12;
    cp |= ((uint8_t)s[2] & 0x3f) << 6;
    cp |= ((uint8_t)s[3] & 0x3f);
    return cp;
  default:
    return c;  /* ASCII, or invalid/truncated byte returned as-is */
  }
}

/* Check if character is a "word" character (\w): [a-zA-Z0-9_] */
mrb_bool
mrb_re_is_word_char(uint32_t c)
{
  if (c >= 'a' && c <= 'z') return TRUE;
  if (c >= 'A' && c <= 'Z') return TRUE;
  if (c >= '0' && c <= '9') return TRUE;
  if (c == '_') return TRUE;
  return FALSE;
}
