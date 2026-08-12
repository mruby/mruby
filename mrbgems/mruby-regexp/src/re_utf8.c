/*
** re_utf8.c - case folding and word characters for regexp engine
**
** See Copyright Notice in mruby.h
*/

#include "re_internal.h"

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

#else  /* !MRB_REGEXP_UNICODE_CASE */

#include "re_cased.h"

mrb_bool
mrb_re_needs_case_data(uint32_t lo, uint32_t hi)
{
  if (hi < RE_CASED_MIN || lo > RE_CASED_MAX) return FALSE;
  for (size_t i = 0; i < RE_CASED_RANGE_COUNT; i++) {
    if (lo <= re_cased_ranges[i][1] && re_cased_ranges[i][0] <= hi) return TRUE;
  }
  return FALSE;
}

#endif  /* MRB_REGEXP_UNICODE_CASE */

uint32_t
mrb_re_case_fold(uint32_t cp)
{
  if (cp < 128) return (cp >= 'A' && cp <= 'Z') ? cp + 32 : cp;
#ifdef MRB_REGEXP_UNICODE_CASE
  const re_fold_run *r = fold_run_for(cp);
  return r ? (uint32_t)((int32_t)cp + r->delta) : cp;
#else
  if (cp == RE_FOLD_LONG_S) return 's';
  if (cp == RE_FOLD_KELVIN) return 'k';
  return cp;
#endif
}

#ifdef MRB_REGEXP_UNICODE_CASE
int
mrb_re_case_unfold(uint32_t cp, uint32_t *out, int max)
{
  int n = 0;
  uint32_t folded = mrb_re_case_fold(cp);

  /* The folded form is itself a member of the class. */
  if (folded != cp && n < max) out[n++] = folded;

  /* ASCII sources are in no table, so the upper case letter that folds into a
     lower case one is added here. */
  if (folded >= 'a' && folded <= 'z' && folded - 32 != cp && n < max) {
    out[n++] = folded - 32;
  }

  /* A non-ASCII source folding into ASCII (U+017F into 's') is in the table
     and is found by this scan like any other. */
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

/* Both range walks read the table run by run, which keeps a wide range cheap:
   a run of stride 1 contributes one span whatever its length, and only the
   interleaved stride 2 runs are reported one codepoint at a time. */
void
mrb_re_case_fold_range(uint32_t lo, uint32_t hi,
                       void (*add)(void *, uint32_t, uint32_t), void *user)
{
  for (size_t i = 0; i < RE_FOLD_RUN_COUNT; i++) {
    const re_fold_run *r = &re_fold_runs[i];
    uint32_t span = (uint32_t)(r->count - 1) * r->stride;

    uint32_t s_lo = r->start > lo ? r->start : lo;
    uint32_t s_hi = r->start + span < hi ? r->start + span : hi;
    if (s_lo > s_hi) continue;
    /* Round s_lo up and s_hi down to codepoints the run actually holds. */
    uint32_t off = s_lo - r->start;
    if (off % r->stride) s_lo += r->stride - (off % r->stride);
    s_hi -= (s_hi - r->start) % r->stride;
    if (s_lo > s_hi) continue;

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

void
mrb_re_case_unfold_range(uint32_t lo, uint32_t hi,
                         void (*add)(void *, uint32_t, uint32_t), void *user)
{
  for (size_t i = 0; i < RE_FOLD_RUN_COUNT; i++) {
    const re_fold_run *r = &re_fold_runs[i];
    uint32_t span = (uint32_t)(r->count - 1) * r->stride;

    int32_t f_start = (int32_t)r->start + r->delta;
    if (f_start < 0) continue;
    int32_t f_end = f_start + (int32_t)span;
    uint32_t f_lo = (uint32_t)f_start > lo ? (uint32_t)f_start : lo;
    uint32_t f_hi = (uint32_t)f_end < hi ? (uint32_t)f_end : hi;
    if (f_lo > f_hi) continue;
    uint32_t off = f_lo - (uint32_t)f_start;
    if (off % r->stride) f_lo += r->stride - (off % r->stride);
    f_hi -= (f_hi - (uint32_t)f_start) % r->stride;
    if (f_lo > f_hi) continue;

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
#endif  /* MRB_REGEXP_UNICODE_CASE */
