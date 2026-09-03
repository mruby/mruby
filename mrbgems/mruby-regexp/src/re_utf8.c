/*
** re_utf8.c - case folding, character types and word characters for regexp engine
**
** See Copyright Notice in mruby.h
*/

#include "re_internal.h"

#ifdef RE_UNICODE_CTYPE

#include "re_ctype.h"

/* The table numbers its bits in the order re_internal.h names them, so that
   an entry read off it is a re_ctype value as it stands. */
mrb_static_assert(RE_CTYPE_TABLE_ALPHA == RE_CTYPE_ALPHA &&
                  RE_CTYPE_TABLE_UPPER == RE_CTYPE_UPPER &&
                  RE_CTYPE_TABLE_LOWER == RE_CTYPE_LOWER &&
                  RE_CTYPE_TABLE_DIGIT == RE_CTYPE_DIGIT &&
                  RE_CTYPE_TABLE_ALNUM == RE_CTYPE_ALNUM &&
                  RE_CTYPE_TABLE_WORD  == RE_CTYPE_WORD &&
                  RE_CTYPE_TABLE_PUNCT == RE_CTYPE_PUNCT &&
                  RE_CTYPE_TABLE_SPACE == RE_CTYPE_SPACE &&
                  RE_CTYPE_TABLE_BLANK == RE_CTYPE_BLANK &&
                  RE_CTYPE_TABLE_GRAPH == RE_CTYPE_GRAPH &&
                  RE_CTYPE_TABLE_PRINT == RE_CTYPE_PRINT &&
                  RE_CTYPE_CNTRL >= (1 << RE_CTYPE_MASK_BITS),
                  "re_ctype.h and re_internal.h number the types differently");

/* The types of a codepoint above ASCII: the set of the run it falls in, which
   is the last run starting at or below it, and cntrl from its range. */
uint16_t
mrb_re_ctype(uint32_t cp)
{
  if (cp < RE_CTYPE_MIN) return 0;
  size_t lo = 0, hi = RE_CTYPE_RUN_COUNT;
  while (hi - lo > 1) {
    size_t mid = lo + (hi - lo) / 2;
    if ((re_ctype_runs[mid] >> RE_CTYPE_MASK_BITS) <= cp) lo = mid;
    else hi = mid;
  }
  uint16_t t = (uint16_t)(re_ctype_runs[lo] & ((1u << RE_CTYPE_MASK_BITS) - 1));
  if (cp >= RE_CTYPE_CNTRL_LO && cp <= RE_CTYPE_CNTRL_HI) t |= RE_CTYPE_CNTRL;
  return t;
}

/* Whether a class holds a codepoint above ASCII through the POSIX brackets in
   it, once its ranges have said nothing: yes when the codepoint's type has a
   bit of ctype_yes, or lacks a bit of ctype_no, and failing both whatever
   utf8_any says. A byte, tagged RE_CLASS_BYTE by the caller, has no type: it
   is in the class through a negated bracket and not through a positive one.

   Under /i a character is in the class when any character sharing its
   folding is, so the question is put to every one of them: a positive
   bracket wants a type any of them has, a negated one a type any of them
   lacks. The ASCII ones are left out, since what the class holds through an
   ASCII counterpart is in its ranges already; see compile_charclass(). */
mrb_bool
mrb_re_class_ctype_match(const re_charclass *cc, uint32_t cp)
{
  if (cp & RE_CLASS_BYTE) return cc->ctype_no != 0 || cc->utf8_any;
  uint16_t any = mrb_re_ctype(cp), all = any;
  if (cc->ctype_fold) {
    uint32_t alt[MRB_UNI_MAX_UNFOLD];
    int n = mrb_uni_case_unfold(cp, alt, MRB_UNI_MAX_UNFOLD);
    for (int i = 0; i < n; i++) {
      if (alt[i] < 128) continue;
      uint16_t t = mrb_re_ctype(alt[i]);
      any |= t;
      all &= t;
    }
  }
  return (any & cc->ctype_yes) || (~all & cc->ctype_no) || cc->utf8_any;
}

uint32_t
mrb_re_ctype_span(const re_charclass *cc, uint32_t lo, uint32_t hi, mrb_bool *in)
{
  size_t i = 0, j = RE_CTYPE_RUN_COUNT;
  while (j - i > 1) {
    size_t mid = i + (j - i) / 2;
    if ((re_ctype_runs[mid] >> RE_CTYPE_MASK_BITS) <= lo) i = mid;
    else j = mid;
  }
  uint32_t end = i + 1 < RE_CTYPE_RUN_COUNT
                   ? (re_ctype_runs[i + 1] >> RE_CTYPE_MASK_BITS) - 1 : 0x10ffff;
  /* The control range is not a run of its own: mrb_re_ctype() lays it over
     whatever runs it falls in, so it breaks a run the same way one does. */
  if (lo < RE_CTYPE_CNTRL_LO) {
    if (end >= RE_CTYPE_CNTRL_LO) end = RE_CTYPE_CNTRL_LO - 1;
  }
  else if (lo <= RE_CTYPE_CNTRL_HI) {
    if (end > RE_CTYPE_CNTRL_HI) end = RE_CTYPE_CNTRL_HI;
  }
  if (end > hi) end = hi;
  *in = mrb_re_class_ctype_match(cc, lo);
  return end;
}

#endif  /* RE_UNICODE_CTYPE */

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

#ifndef RE_UNICODE_CASE

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

#endif  /* !RE_UNICODE_CASE */

uint32_t
mrb_re_case_fold(uint32_t cp)
{
#ifdef RE_UNICODE_CASE
  return mrb_uni_case_fold(cp);
#else
  /* Without the option there is no table to walk, and the compiler reaches
     the same two foldings directly, since there are only two. */
  if (cp < 128) return (cp >= 'A' && cp <= 'Z') ? cp + 32 : cp;
  if (cp == RE_FOLD_LONG_S) return 's';
  if (cp == RE_FOLD_KELVIN) return 'k';
  return cp;
#endif
}
