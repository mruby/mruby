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
