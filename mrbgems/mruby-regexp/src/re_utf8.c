/*
** re_utf8.c - UTF-8 utility functions for regexp engine
**
** See Copyright Notice in mruby.h
*/

#include "re_internal.h"

/* Return byte length of UTF-8 character at s.
   Returns 1 for invalid sequences (treat as single byte): a byte that starts
   no sequence, a sequence cut short by end, one whose continuation bytes are
   not 10xxxxxx, and one that spells a codepoint a shorter sequence already
   holds, a surrogate, or a value above U+10FFFF. The last three are decided
   by the lead byte together with the first continuation byte. */
int
mrb_re_utf8_charlen(const char *s, const char *end)
{
  uint8_t c = (uint8_t)*s;
  uint8_t lo = 0x80, hi = 0xbf;  /* range the first continuation byte must fall in */
  int len;

  if (c < 0x80) return 1;
  else if (c < 0xc2) return 1;  /* continuation byte, or an overlong two-byte lead */
  else if (c < 0xe0) len = 2;
  else if (c < 0xf0) {
    len = 3;
    if (c == 0xe0) lo = 0xa0;       /* below U+0800 */
    else if (c == 0xed) hi = 0x9f;  /* U+D800 to U+DFFF */
  }
  else if (c < 0xf5) {
    len = 4;
    if (c == 0xf0) lo = 0x90;       /* below U+10000 */
    else if (c == 0xf4) hi = 0x8f;  /* above U+10FFFF */
  }
  else return 1;  /* invalid */

  if (s + len > end) return 1;  /* truncated */
  if ((uint8_t)s[1] < lo || (uint8_t)s[1] > hi) return 1;
  for (int i = 2; i < len; i++) {
    if (((uint8_t)s[i] & 0xc0) != 0x80) return 1;  /* invalid continuation */
  }
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

#ifdef MRB_REGEXP_UNICODE_CASE

#include "re_casefold.h"

/* Locate the run holding cp, or NULL. Runs are emitted in ascending source
   order and never overlap, so a binary search on start is enough; a run with
   stride 2 covers only every other codepoint in its span, which the modulo
   check rejects. */
static const re_fold_run*
fold_run_for(uint32_t cp)
{
  if (cp < RE_FOLD_MIN || cp > RE_FOLD_MAX) return NULL;

  size_t lo = 0, hi = RE_FOLD_RUN_COUNT;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    const re_fold_run *r = &re_fold_runs[mid];
    uint32_t last = r->start + (uint32_t)(r->count - 1) * r->stride;
    if (cp < r->start) hi = mid;
    else if (cp > last) lo = mid + 1;
    else return ((cp - r->start) % r->stride) == 0 ? r : NULL;
  }
  return NULL;
}

uint32_t
mrb_re_case_fold(uint32_t cp)
{
  if (cp < 128) return (cp >= 'A' && cp <= 'Z') ? cp + 32 : cp;
  const re_fold_run *r = fold_run_for(cp);
  return r ? (uint32_t)((int32_t)cp + r->delta) : cp;
}

int
mrb_re_case_unfold(uint32_t cp, uint32_t *out, int max)
{
  int n = 0;
  uint32_t folded = mrb_re_case_fold(cp);

  /* The folded form is itself a member of the class. */
  if (folded != cp && n < max) out[n++] = folded;

  /* ASCII sources are not in the table, so the upper case letter that folds
     into a lower case one is added here. A non-ASCII source folding into
     ASCII (U+017F into 's') is in the table and is found by the scan below
     like any other. */
  if (folded >= 'a' && folded <= 'z' && folded - 32 != cp && n < max) {
    out[n++] = folded - 32;
  }

  for (size_t i = 0; i < RE_FOLD_RUN_COUNT && n < max; i++) {
    const re_fold_run *r = &re_fold_runs[i];
    int32_t src = (int32_t)folded - r->delta;
    if (src < (int32_t)r->start) continue;
    uint32_t off = (uint32_t)src - r->start;
    if (off % r->stride) continue;
    if (off / r->stride >= r->count) continue;
    if ((uint32_t)src == cp) continue;
    out[n++] = (uint32_t)src;
  }
  return n;
}

/* Report the case counterparts of every codepoint in [lo, hi] by calling add()
   with each span of them. Walking the table run by run keeps a wide range
   cheap: a run of stride 1 contributes one span, and only the interleaved
   stride 2 runs are reported one codepoint at a time. Spans may repeat or
   overlap what the caller already holds; the caller merges. */
void
mrb_re_case_unfold_range(uint32_t lo, uint32_t hi,
                         void (*add)(void *, uint32_t, uint32_t), void *user)
{
  for (size_t i = 0; i < RE_FOLD_RUN_COUNT; i++) {
    const re_fold_run *r = &re_fold_runs[i];
    uint32_t span = (uint32_t)(r->count - 1) * r->stride;

    /* Sources of this run that the range covers, reported as their folds. */
    uint32_t s_lo = r->start > lo ? r->start : lo;
    uint32_t s_hi = r->start + span < hi ? r->start + span : hi;
    if (s_lo <= s_hi) {
      /* Round s_lo up and s_hi down to codepoints the run actually holds. */
      uint32_t off = s_lo - r->start;
      if (off % r->stride) s_lo += r->stride - (off % r->stride);
      s_hi -= (s_hi - r->start) % r->stride;
      if (s_lo <= s_hi) {
        if (r->stride == 1) {
          add(user, (uint32_t)((int32_t)s_lo + r->delta), (uint32_t)((int32_t)s_hi + r->delta));
        }
        else {
          for (uint32_t cp = s_lo; cp <= s_hi; cp += r->stride) {
            uint32_t f = (uint32_t)((int32_t)cp + r->delta);
            add(user, f, f);
          }
        }
      }
    }

    /* Folds of this run that the range covers, reported as their sources. */
    int32_t f_start = (int32_t)r->start + r->delta;
    int32_t f_end = f_start + (int32_t)span;
    uint32_t f_lo = (uint32_t)f_start > lo ? (uint32_t)f_start : lo;
    uint32_t f_hi = (uint32_t)f_end < hi ? (uint32_t)f_end : hi;
    if (f_start >= 0 && f_lo <= f_hi) {
      uint32_t off = f_lo - (uint32_t)f_start;
      if (off % r->stride) f_lo += r->stride - (off % r->stride);
      f_hi -= (f_hi - (uint32_t)f_start) % r->stride;
      if (f_lo <= f_hi) {
        if (r->stride == 1) {
          add(user, (uint32_t)((int32_t)f_lo - r->delta), (uint32_t)((int32_t)f_hi - r->delta));
        }
        else {
          for (uint32_t cp = f_lo; cp <= f_hi; cp += r->stride) {
            uint32_t s = (uint32_t)((int32_t)cp - r->delta);
            add(user, s, s);
          }
        }
      }
    }
  }
}
#endif  /* MRB_REGEXP_UNICODE_CASE */
