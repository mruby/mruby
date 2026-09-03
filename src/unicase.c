/*
** unicase.c - what case a Unicode character has
**
** The tables in unicase.h and the lookups over them. What a string does with
** the answers is string.c's business, and what a pattern does with them is
** mruby-regexp's; this file knows only about codepoints.
**
** See Copyright Notice in mruby.h
*/

#include <string.h>
#include <mruby.h>

#if defined(MRB_UTF8_STRING) && !defined(MRB_USE_ASCII_CTYPE)

#include <mruby/internal.h>
#include "unicase.h"

mrb_static_assert(UNI_CASE_MAX_BYTES <= MRB_UNI_CASE_MAX_BYTES,
                  "a mapping outgrew the buffer its callers hand over");

static const struct case_table {
  const uint8_t *runs;
  size_t run_count;
  const uint8_t *multi;
  size_t multi_count;
  uint32_t min, max;
} case_tables[] = {
  {UNI_LOWER_RUNS, UNI_LOWER_RUN_COUNT, UNI_LOWER_MULTI, UNI_LOWER_MULTI_COUNT,
   UNI_LOWER_MIN, UNI_LOWER_MAX},
  {UNI_UPPER_RUNS, UNI_UPPER_RUN_COUNT, UNI_UPPER_MULTI, UNI_UPPER_MULTI_COUNT,
   UNI_UPPER_MIN, UNI_UPPER_MAX},
  {UNI_TITLE_RUNS, UNI_TITLE_RUN_COUNT, UNI_TITLE_MULTI, UNI_TITLE_MULTI_COUNT,
   UNI_TITLE_MIN, UNI_TITLE_MAX},
  {UNI_SWAP_RUNS, UNI_SWAP_RUN_COUNT, UNI_SWAP_MULTI, UNI_SWAP_MULTI_COUNT,
   UNI_SWAP_MIN, UNI_SWAP_MAX},
  {UNI_FOLD_RUNS, UNI_FOLD_RUN_COUNT, UNI_FOLD_MULTI, UNI_FOLD_MULTI_COUNT,
   UNI_FOLD_MIN, UNI_FOLD_MAX},
};

/* The `n` bytes at `p`, least significant first, which is how unicase.h packs
   both of its entry kinds. */
static uint64_t
case_bits(const uint8_t *p, int n)
{
  uint64_t v = 0;
  for (int i = 0; i < n; i++) v |= (uint64_t)p[i] << (8 * i);
  return v;
}

typedef struct case_run {
  uint32_t start, count, stride;
  int32_t delta;
} case_run;

static void
case_run_at(const uint8_t *runs, size_t i, case_run *r)
{
  uint64_t v = case_bits(runs + i * UNI_CASE_RUN_BYTES, UNI_CASE_RUN_BYTES);
  r->start = (uint32_t)(v & 0x1FFFFF);
  r->count = (uint32_t)((v >> 21) & 0x7F);
  r->stride = (uint32_t)((v >> 28) & 1) + 1;
  r->delta = (int32_t)((v >> 29) & 0x1FFFF) - UNI_CASE_DELTA_BIAS;
}

/* Locate the run holding cp. Runs are emitted in ascending source order and
   never overlap, so a binary search on start is enough; a run with stride 2
   covers only every other codepoint in its span, which the modulo check
   rejects. */
static mrb_bool
case_run_for(const struct case_table *t, uint32_t cp, case_run *out)
{
  size_t lo = 0, hi = t->run_count;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    case_run r;
    case_run_at(t->runs, mid, &r);
    uint32_t last = r.start + (r.count - 1) * r.stride;
    if (cp < r.start) hi = mid;
    else if (cp > last) lo = mid + 1;
    else {
      if ((cp - r.start) % r.stride != 0) return FALSE;
      *out = r;
      return TRUE;
    }
  }
  return FALSE;
}

static mrb_bool
case_multi_for(const struct case_table *t, uint32_t cp, uint32_t *off, uint32_t *len)
{
  size_t lo = 0, hi = t->multi_count;
  while (lo < hi) {
    size_t mid = lo + (hi - lo) / 2;
    uint64_t v = case_bits(t->multi + mid * UNI_CASE_MULTI_BYTES, UNI_CASE_MULTI_BYTES);
    uint32_t mcp = (uint32_t)(v & 0x1FFFFF);
    if (cp < mcp) hi = mid;
    else if (cp > mcp) lo = mid + 1;
    else {
      *off = (uint32_t)((v >> 21) & 0xFFF);
      *len = (uint32_t)((v >> 33) & 7);
      return TRUE;
    }
  }
  return FALSE;
}

/* Ask one table about cp, writing what it says into buf. Answers the byte
   count written, 0 for a character the table maps to itself, and -1 for one
   the table says nothing about. The two apart is what lets title case hand a
   character it says nothing about on to upper case while keeping the ones it
   deliberately maps to themselves. */
static mrb_int
case_map_one(enum mrb_case_kind kind, uint32_t cp, char *buf)
{
  const struct case_table *t = &case_tables[kind];
  if (cp < t->min || t->max < cp) return -1;

  uint32_t off, len;
  if (case_multi_for(t, cp, &off, &len)) {
    memcpy(buf, uni_case_pool + off, len);
    return (mrb_int)len;
  }
  case_run r;
  if (!case_run_for(t, cp, &r)) return -1;
  if (r.delta == 0) return 0;
  return mrb_utf8_to_buf(buf, (mrb_int)cp + r.delta);
}

mrb_int
mrb_uni_case_map(enum mrb_case_kind kind, uint32_t cp, char *buf)
{
  mrb_int n = case_map_one(kind, cp, buf);
  if (n >= 0) return n;

  switch (kind) {
  case MRB_CASE_KIND_TITLE:
    /* Title case is the difference from upper case, so a character the
       difference says nothing about takes the upper case answer. */
    n = case_map_one(MRB_CASE_KIND_UPPER, cp, buf);
    break;
  case MRB_CASE_KIND_SWAP:
    /* Swapping is the difference from this rule: a character with a lower
       case is an upper case one and swaps down, and one without swaps up. */
    n = case_map_one(MRB_CASE_KIND_LOWER, cp, buf);
    if (n < 0) n = case_map_one(MRB_CASE_KIND_UPPER, cp, buf);
    break;
  case MRB_CASE_KIND_FOLD:
    /* Folding is the difference from the lowercase mapping, so a character
       the difference says nothing about folds the way it lower cases. */
    n = case_map_one(MRB_CASE_KIND_LOWER, cp, buf);
    break;
  default:
    break;
  }
  return n < 0 ? 0 : n;
}

#ifdef HAVE_MRUBY_REGEXP_GEM

/* ------------------------------------------------------------- folding

   Simple case folding is what a pattern asks for under /i, and it is the two
   run tables read in order: the folding difference first, and the lowercase
   mapping under it for the sources the difference passes over. A source whose
   folding spells several characters (U+FB00 to "ff") is in a multi table and
   in neither set of runs, which is exactly the set simple folding leaves
   alone.

   /i is the whole of what asks, so the gem that has one says it is here and
   the rest of this file goes with it. Nothing else would drop it: the mapping
   above is in the same object, every build calls that, and the linker takes
   an object whole. */

/* Whether the folding difference holds a run over any of [lo, hi]. A caller
   walking the lowercase runs asks this to know whether the difference has
   something to say about the span it is about to report whole. Runs ascend
   and never overlap, so their ends ascend too: the first run whose end
   reaches lo is the only one that can start before hi, and the search below
   keeps its start, since the last run it steps h down to is that one. */
static mrb_bool
fold_diff_touches(uint32_t lo, uint32_t hi)
{
  const struct case_table *t = &case_tables[MRB_CASE_KIND_FOLD];
  size_t l = 0, h = t->run_count;
  uint32_t start = UINT32_MAX;  /* past any hi, so no run found means FALSE */
  while (l < h) {
    size_t mid = l + (h - l) / 2;
    case_run r;
    case_run_at(t->runs, mid, &r);
    if (r.start + (r.count - 1) * r.stride < lo) l = mid + 1;
    else {
      h = mid;
      start = r.start;
    }
  }
  return start <= hi;
}

uint32_t
mrb_uni_case_fold(uint32_t cp)
{
  /* ASCII is out of the tables, being what the tables are the rest of. */
  if (cp < 128) return (cp >= 'A' && cp <= 'Z') ? cp + 32 : cp;

  case_run r;
  const struct case_table *f = &case_tables[MRB_CASE_KIND_FOLD];
  if (f->min <= cp && cp <= f->max && case_run_for(f, cp, &r)) {
    /* A run of delta 0 here is the difference saying the source folds to
       itself where it lower cases to something, which is an answer. */
    return (uint32_t)((int32_t)cp + r.delta);
  }
  const struct case_table *l = &case_tables[MRB_CASE_KIND_LOWER];
  if (l->min <= cp && cp <= l->max && case_run_for(l, cp, &r)) {
    return (uint32_t)((int32_t)cp + r.delta);
  }
  return cp;
}

/* Every source in `t` whose folded form is `folded`, checked back through
   mrb_uni_case_fold() so that a source the difference overrides is not
   reported from the mapping under it. */
static int
unfold_from(const struct case_table *t, uint32_t cp, uint32_t folded,
            uint32_t *out, int max, int n)
{
  for (size_t i = 0; i < t->run_count && n < max; i++) {
    case_run r;
    case_run_at(t->runs, i, &r);
    int32_t s = (int32_t)folded - r.delta;
    if (s < (int32_t)r.start) continue;
    uint32_t off = (uint32_t)s - r.start;
    if (off % r.stride) continue;
    if (off / r.stride >= r.count) continue;
    if ((uint32_t)s == cp) continue;
    if (mrb_uni_case_fold((uint32_t)s) != folded) continue;
    for (int k = 0; k < n; k++) if (out[k] == (uint32_t)s) goto next;
    out[n++] = (uint32_t)s;
  next:
    ;
  }
  return n;
}

int
mrb_uni_case_unfold(uint32_t cp, uint32_t *out, int max)
{
  int n = 0;
  uint32_t folded = mrb_uni_case_fold(cp);

  /* The folded form is itself a member of the class. */
  if (folded != cp && n < max) out[n++] = folded;

  /* ASCII sources are in no table, so the upper case letter that folds into a
     lower case one is added here. */
  if (folded >= 'a' && folded <= 'z' && folded - 32 != cp && n < max) {
    out[n++] = folded - 32;
  }

  /* A non-ASCII source folding into ASCII (U+017F into 's') is the only thing
     an ASCII source can find in a table, and the generator lists those beside
     the tables, so the list is the whole answer for one. What that saves is
     the two full scans below, which an ASCII source would otherwise make to
     collect the entries the list already holds. Most patterns are ASCII and
     nothing else, and this is what /i costs them. */
  if (cp < 128) {
#define X(src, to) if (folded == (to) && n < max) out[n++] = (src);
    UNI_FOLD_TO_ASCII
#undef X
    return n;
  }

  n = unfold_from(&case_tables[MRB_CASE_KIND_FOLD], cp, folded, out, max, n);
  n = unfold_from(&case_tables[MRB_CASE_KIND_LOWER], cp, folded, out, max, n);
  return n;
}

/* Both range walks read the tables run by run, which keeps a wide range cheap:
   a run of stride 1 contributes one span whatever its length. A run the
   folding difference has something to say about is reported one codepoint at a
   time instead, since the difference may override any part of it. */
static void
fold_span(uint32_t lo, uint32_t hi, void (*add)(void *, uint32_t, uint32_t), void *user)
{
  for (uint32_t cp = lo; cp <= hi; cp++) {
    uint32_t f = mrb_uni_case_fold(cp);
    if (f != cp) add(user, f, f);
  }
}

static void
fold_range_of(const struct case_table *t, mrb_bool guard, uint32_t lo, uint32_t hi,
              void (*add)(void *, uint32_t, uint32_t), void *user)
{
  for (size_t i = 0; i < t->run_count; i++) {
    case_run r;
    case_run_at(t->runs, i, &r);
    uint32_t span = (r.count - 1) * r.stride;

    uint32_t s_lo = r.start > lo ? r.start : lo;
    uint32_t s_hi = r.start + span < hi ? r.start + span : hi;
    if (s_lo > s_hi) continue;
    /* Round s_lo up and s_hi down to codepoints the run actually holds. */
    uint32_t off = s_lo - r.start;
    if (off % r.stride) s_lo += r.stride - (off % r.stride);
    s_hi -= (s_hi - r.start) % r.stride;
    if (s_lo > s_hi) continue;

    if (guard && fold_diff_touches(s_lo, s_hi)) {
      fold_span(s_lo, s_hi, add, user);
    }
    else if (r.stride == 1) {
      if (r.delta != 0) {
        add(user, (uint32_t)((int32_t)s_lo + r.delta), (uint32_t)((int32_t)s_hi + r.delta));
      }
    }
    else {
      for (uint32_t cp = s_lo; cp <= s_hi; cp += r.stride) {
        if (r.delta == 0) continue;
        uint32_t f = (uint32_t)((int32_t)cp + r.delta);
        add(user, f, f);
      }
    }
  }
}

void
mrb_uni_case_fold_range(uint32_t lo, uint32_t hi,
                        void (*add)(void *, uint32_t, uint32_t), void *user)
{
  fold_range_of(&case_tables[MRB_CASE_KIND_FOLD], FALSE, lo, hi, add, user);
  fold_range_of(&case_tables[MRB_CASE_KIND_LOWER], TRUE, lo, hi, add, user);
}

/* The sources of the folds landing in [lo, hi]. A source found through the
   lowercase mapping is checked back through mrb_uni_case_fold(), since the
   difference may fold it elsewhere or leave it alone. */
static void
unfold_range_of(const struct case_table *t, mrb_bool guard, uint32_t lo, uint32_t hi,
                void (*add)(void *, uint32_t, uint32_t), void *user)
{
  for (size_t i = 0; i < t->run_count; i++) {
    case_run r;
    case_run_at(t->runs, i, &r);
    uint32_t span = (r.count - 1) * r.stride;

    int32_t f_start = (int32_t)r.start + r.delta;
    if (f_start < 0) continue;
    int32_t f_end = f_start + (int32_t)span;
    uint32_t f_lo = (uint32_t)f_start > lo ? (uint32_t)f_start : lo;
    uint32_t f_hi = (uint32_t)f_end < hi ? (uint32_t)f_end : hi;
    if (f_lo > f_hi) continue;
    uint32_t off = f_lo - (uint32_t)f_start;
    if (off % r.stride) f_lo += r.stride - (off % r.stride);
    f_hi -= (f_hi - (uint32_t)f_start) % r.stride;
    if (f_lo > f_hi) continue;

    uint32_t s_lo = (uint32_t)((int32_t)f_lo - r.delta);
    uint32_t s_hi = (uint32_t)((int32_t)f_hi - r.delta);
    if (guard && fold_diff_touches(s_lo, s_hi)) {
      for (uint32_t cp = s_lo; cp <= s_hi; cp += r.stride) {
        uint32_t f = mrb_uni_case_fold(cp);
        if (f != cp && lo <= f && f <= hi) add(user, cp, cp);
      }
    }
    else if (r.stride == 1) {
      if (r.delta != 0) add(user, s_lo, s_hi);
    }
    else {
      for (uint32_t cp = s_lo; cp <= s_hi; cp += r.stride) {
        if (r.delta != 0) add(user, cp, cp);
      }
    }
  }
}

void
mrb_uni_case_unfold_range(uint32_t lo, uint32_t hi,
                          void (*add)(void *, uint32_t, uint32_t), void *user)
{
  /* An ASCII fold has the listed sources and no others, for the reason
     mrb_uni_case_unfold() gives, so the ASCII part of the span is answered
     from the list and the walks below are handed what is left of it. A class
     of nothing but ASCII, which is most classes, reaches neither walk. */
  if (lo < 128) {
#define X(src, to) if (lo <= (to) && (to) <= hi) add(user, (src), (src));
    UNI_FOLD_TO_ASCII
#undef X
    if (hi < 128) return;
    lo = 128;
  }
  unfold_range_of(&case_tables[MRB_CASE_KIND_FOLD], FALSE, lo, hi, add, user);
  unfold_range_of(&case_tables[MRB_CASE_KIND_LOWER], TRUE, lo, hi, add, user);
}

#endif  /* HAVE_MRUBY_REGEXP_GEM */

#endif  /* MRB_UTF8_STRING && !MRB_USE_ASCII_CTYPE */
