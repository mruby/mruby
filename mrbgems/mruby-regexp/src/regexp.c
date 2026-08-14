/*
** regexp.c - Regexp class and MatchData class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/hash.h>
#include <mruby/error.h>
#include <mruby/internal.h>
#include "re_internal.h"

#include <string.h>

/* Regexp data type */
static void regexp_free(mrb_state *mrb, void *ptr) {
  mrb_re_free(mrb, (mrb_regexp_pattern*)ptr);
}

static const struct mrb_data_type regexp_type = { "Regexp", regexp_free };

/* MatchData */
typedef struct {
  mrb_value source;        /* source string */
  mrb_value regexp;        /* Regexp object (for named captures) */
  int *captures;           /* capture positions [start0,end0,start1,end1,...] */
  int num_captures;        /* number of capture groups (including 0) */
} mrb_match_data;

static void matchdata_free(mrb_state *mrb, void *ptr) {
  mrb_match_data *md = (mrb_match_data*)ptr;
  if (md) {
    mrb_free(mrb, md->captures);
    mrb_free(mrb, md);
  }
}

static const struct mrb_data_type matchdata_type = { "MatchData", matchdata_free };

/* Get internal flags from Regexp object */
static uint32_t
get_iflags(mrb_state *mrb, mrb_value self)
{
  mrb_value v = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@flags"));
  return mrb_nil_p(v) ? 0 : (uint32_t)mrb_integer(v);
}

/* Parse flags from string or integer */
static uint32_t
parse_flags(mrb_state *mrb, mrb_value flags_val)
{
  uint32_t flags = 0;
  if (mrb_integer_p(flags_val)) {
    mrb_int f = mrb_integer(flags_val);
    if (f & 1) flags |= RE_FLAG_IGNORECASE;
    if (f & 2) flags |= RE_FLAG_EXTENDED;
    if (f & 4) flags |= RE_FLAG_MULTILINE | RE_FLAG_DOTALL;
    return flags;
  }
  if (mrb_string_p(flags_val)) {
    const char *s = RSTRING_PTR(flags_val);
    mrb_int len = RSTRING_LEN(flags_val);
    for (mrb_int i = 0; i < len; i++) {
      switch (s[i]) {
      case 'i': flags |= RE_FLAG_IGNORECASE; break;
      case 'm': flags |= RE_FLAG_MULTILINE | RE_FLAG_DOTALL; break;
      case 'x': flags |= RE_FLAG_EXTENDED; break;
      }
    }
    return flags;
  }
  if (mrb_test(flags_val)) flags |= RE_FLAG_IGNORECASE;
  return flags;
}

/*
 * Regexp.new(pattern, flags=nil)
 * Regexp.new(regexp)
 * Regexp.compile(pattern, flags=nil)
 */
static mrb_value
regexp_init(mrb_state *mrb, mrb_value self)
{
  mrb_value pattern;
  mrb_value flags_val = mrb_nil_value();
  mrb_regexp_pattern *pat;

  mrb_get_args(mrb, "o|o", &pattern, &flags_val);

  uint32_t flags;

  /* If pattern is a Regexp, copy its source and flags */
  if (mrb_obj_is_kind_of(mrb, pattern, mrb_class_get(mrb, "Regexp"))) {
    mrb_value iflags = mrb_iv_get(mrb, pattern, mrb_intern_lit(mrb, "@flags"));
    flags = mrb_nil_p(iflags) ? 0 : (uint32_t)mrb_integer(iflags);
    pattern = mrb_iv_get(mrb, pattern, mrb_intern_lit(mrb, "@source"));
  }
  else {
    if (!mrb_string_p(pattern)) {
      mrb_raise(mrb, E_TYPE_ERROR, "wrong argument type (expected String or Regexp)");
    }
    flags = parse_flags(mrb, flags_val);
  }

  /* Set @source and @flags before mrb_re_compile() so a Regexp that survives
     a compile-time exception (e.g. picked up by ObjectSpace.each_object)
     still has usable IVs for hash/eql?/inspect. */
  mrb_iv_set(mrb, self, mrb_intern_lit(mrb, "@source"), pattern);
  mrb_iv_set(mrb, self, mrb_intern_lit(mrb, "@flags"), mrb_int_value(mrb, (mrb_int)flags));

  pat = mrb_re_compile(mrb, RSTRING_PTR(pattern), RSTRING_LEN(pattern), flags);

  DATA_TYPE(self) = &regexp_type;
  DATA_PTR(self) = pat;

  /* store named captures as hash */
  if (pat->num_named > 0) {
    mrb_value nc = mrb_hash_new_capa(mrb, pat->num_named);
    for (uint16_t i = 0; i < pat->num_named; i++) {
      mrb_value name = mrb_str_new(mrb, pat->named_captures[i].name, pat->named_captures[i].name_len);
      mrb_hash_set(mrb, nc, name, mrb_fixnum_value(pat->named_captures[i].group));
    }
    mrb_iv_set(mrb, self, mrb_intern_lit(mrb, "@named_captures"), nc);
  }

  return self;
}

/* Pre-interned symbols for $1-$9 (cached on first use) */
static mrb_sym nth_syms[9];

/* Pre-interned symbols for $&, $`, $' and $+ (cached on first use) */
enum { LAST_MATCH, PRE_MATCH, POST_MATCH, LAST_PAREN, LAST_SYM_COUNT };
static mrb_sym last_match_syms[LAST_SYM_COUNT];

static void
ensure_match_syms(mrb_state *mrb)
{
  if (nth_syms[0]) return;
  nth_syms[0] = mrb_intern_lit(mrb, "$1");
  nth_syms[1] = mrb_intern_lit(mrb, "$2");
  nth_syms[2] = mrb_intern_lit(mrb, "$3");
  nth_syms[3] = mrb_intern_lit(mrb, "$4");
  nth_syms[4] = mrb_intern_lit(mrb, "$5");
  nth_syms[5] = mrb_intern_lit(mrb, "$6");
  nth_syms[6] = mrb_intern_lit(mrb, "$7");
  nth_syms[7] = mrb_intern_lit(mrb, "$8");
  nth_syms[8] = mrb_intern_lit(mrb, "$9");
  last_match_syms[LAST_MATCH] = mrb_intern_lit(mrb, "$&");
  last_match_syms[PRE_MATCH] = mrb_intern_lit(mrb, "$`");
  last_match_syms[POST_MATCH] = mrb_intern_lit(mrb, "$'");
  last_match_syms[LAST_PAREN] = mrb_intern_lit(mrb, "$+");
}

static void
clear_match_globals(mrb_state *mrb)
{
  ensure_match_syms(mrb);
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$~"), mrb_nil_value());
  for (int i = 0; i < 9; i++) {
    mrb_gv_set(mrb, nth_syms[i], mrb_nil_value());
  }
  for (int i = 0; i < LAST_SYM_COUNT; i++) {
    mrb_gv_set(mrb, last_match_syms[i], mrb_nil_value());
  }
}

/* Byte-based substring extraction. The regexp engine records all capture
   offsets in bytes, but mrb_str_substr indexes by character under
   MRB_UTF8_STRING, which corrupts non-empty multibyte matches. Extract by
   byte range so the byte offsets are honored as-is. Returns nil for an
   out-of-range request, mirroring mrb_str_substr. */
static mrb_value
re_byte_substr(mrb_state *mrb, mrb_value str, mrb_int beg, mrb_int len)
{
  if (beg < 0 || len < 0 || beg + len > RSTRING_LEN(str)) return mrb_nil_value();
  mrb_value ret = mrb_str_new(mrb, RSTRING_PTR(str) + beg, len);
  /* a piece of a byte-read subject is bytes of it, read the same way */
  RSTR_COPY_BINARY_FLAG(mrb_str_ptr(ret), mrb_str_ptr(str));
  return ret;
}

/* Convert a byte offset into str to a character offset, so MatchData#begin
   and #end report character positions like CRuby. Engine offsets normally
   sit on character boundaries; one that does not (possible only on malformed
   UTF-8) is backed up to the start of the containing character. Negative
   offsets pass through for unmatched captures. */
static mrb_int
re_byte_to_char(mrb_state *mrb, mrb_value str, mrb_int byte_off)
{
  if (byte_off < 0) return byte_off;

  mrb_int len = RSTRING_LEN(str);
  if (byte_off > len) byte_off = len;

  mrb_int chars = mrb_str_byte_to_char(mrb, str, byte_off);
  while (chars < 0 && byte_off > 0) {
    chars = mrb_str_byte_to_char(mrb, str, --byte_off);
  }
  return chars;
}

/* Normalize Regexp#match positional argument for the regexp engine.
   For UTF-8 multibyte strings, Ruby's public pos is a character offset and
   must be converted to a byte offset. For single-byte or binary strings,
   the public pos is already byte-compatible. Returns -1 for out-of-range
   offsets, which Ruby treats as no match. */
static mrb_int
re_char_to_byte(mrb_state *mrb, mrb_value str, mrb_int char_off)
{
  mrb_int len = RSTRING_LEN(str);

  if (char_off < 0) {
    char_off += mrb_str_byte_to_char(mrb, str, len);
    if (char_off < 0) return -1;
  }

  mrb_int byte_off = mrb_str_char_to_byte(mrb, str, 0, char_off);
  if (byte_off > len) return -1;
  return byte_off;
}

static mrb_bool
re_binary_string_p(mrb_value str)
{
  return RSTR_BINARY_P(RSTRING(str));
}

/* CRuby refuses a search whose subject holds a byte that spells no character,
   and mruby answers for it. Refuse it here too, so that a program moved from
   one to the other is told about the subject rather than handed a result the
   other would not have produced.

   A binary string is exempt because it is indexed by byte throughout, so its
   bytes make no claim that could be broken. A quoted String pattern is exempt
   for a narrower reason: CRuby searches for a literal byte by byte and reads
   the subject as UTF-8 nowhere along the way, so `"a\x80b".sub("b", "!")`
   answers there while the same call with `/b/` is refused. The searches a
   literal reaches take a `checked` argument to say so.

   The check walks the whole subject, so every entry point below runs it on the
   subject it is handed and the C loops over a subject run it before the first
   turn. Two searches are driven from mrblib once per match rather than once per
   call, `__byte_search` and the `__search` the backward search steps, and they
   check too: core remembers a string it has read as valid UTF-8, so every turn
   after the first costs a flag test and not a walk. */
static void
re_check_encoding(mrb_state *mrb, mrb_value str)
{
  if (!mrb_str_valid_encoding_p(mrb, str)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid byte sequence in UTF-8");
  }
}

static mrb_value
regexp_binary_string_p(mrb_state *mrb, mrb_value self)
{
  (void)self;
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  return mrb_bool_value(re_binary_string_p(str));
}

/*
 * Regexp.__check_encoding(str)
 *
 * Internal: the check above, for the one caller that reaches no search of this
 * gem's. `String#split` hands a String or nil pattern to core's `split`, which
 * this gem keeps under `__split`, so nothing on that path passes an entry
 * point that would ask the question.
 */
static mrb_value
regexp_check_encoding(mrb_state *mrb, mrb_value self)
{
  (void)self;
  mrb_value str;
  mrb_get_args(mrb, "S", &str);
  re_check_encoding(mrb, str);
  return mrb_nil_value();
}

/* Publish `obj` and the thirteen names derived from its offsets, the
   counterpart of clear_match_globals(). Kept apart from create_matchdata() so
   that an existing MatchData can be republished without rebuilding it. */
static void
set_match_globals(mrb_state *mrb, mrb_value obj, mrb_value str, int *captures, int num_captures)
{
  ensure_match_syms(mrb);

  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$~"), obj);

  /* set $1-$9 from captures */
  for (int i = 0; i < 9; i++) {
    mrb_value val = mrb_nil_value();
    int g = i + 1;
    if (g < num_captures && captures[g*2] >= 0) {
      val = re_byte_substr(mrb, str, captures[g*2], captures[g*2+1] - captures[g*2]);
    }
    mrb_gv_set(mrb, nth_syms[i], val);
  }

  /* set $&, $` and $' from the whole-match offsets */
  mrb_gv_set(mrb, last_match_syms[LAST_MATCH],
             re_byte_substr(mrb, str, captures[0], captures[1] - captures[0]));
  mrb_gv_set(mrb, last_match_syms[PRE_MATCH],
             re_byte_substr(mrb, str, 0, captures[0]));
  mrb_gv_set(mrb, last_match_syms[POST_MATCH],
             re_byte_substr(mrb, str, captures[1], RSTRING_LEN(str) - captures[1]));

  /* set $+ from the last group that actually participated, which is not
     necessarily the last group in the pattern */
  mrb_value last_paren = mrb_nil_value();
  for (int g = num_captures - 1; g >= 1; g--) {
    if (captures[g*2] >= 0) {
      last_paren = re_byte_substr(mrb, str, captures[g*2], captures[g*2+1] - captures[g*2]);
      break;
    }
  }
  mrb_gv_set(mrb, last_match_syms[LAST_PAREN], last_paren);
}

/* Create MatchData from captures. `publish` says whether the match becomes the
   one the match globals describe; a caller that will publish a match of its
   own choosing passes FALSE and leaves them where they were. */
static mrb_value
create_matchdata(mrb_state *mrb, mrb_value regexp, mrb_value str, int *captures, int ncap,
                 mrb_bool publish)
{
  /* Snapshot the subject: MatchData reports the string as it was at match
     time, so later in-place changes to it must not be visible here. */
  str = mrb_str_dup_frozen(mrb, str);

  struct RClass *md_class = mrb_class_get(mrb, "MatchData");
  mrb_match_data *md = (mrb_match_data*)mrb_malloc(mrb, sizeof(mrb_match_data));
  md->source = str;
  md->regexp = regexp;
  md->num_captures = ncap / 2;
  md->captures = (int*)mrb_malloc(mrb, sizeof(int) * ncap);
  memcpy(md->captures, captures, sizeof(int) * ncap);

  mrb_value obj = mrb_obj_value(mrb_data_object_alloc(mrb, md_class, md, &matchdata_type));
  /* Keep `source` and `regexp` GC-reachable via instance variables.
   * The mrb_values are also held in mrb_match_data, but C-allocated
   * structs are not scanned by the GC. */
  mrb_iv_set(mrb, obj, mrb_intern_lit(mrb, "source"), str);
  mrb_iv_set(mrb, obj, mrb_intern_lit(mrb, "regexp"), regexp);

  if (publish) set_match_globals(mrb, obj, str, captures, md->num_captures);

  return obj;
}

/* Internal: the string a match operates on. A Symbol is matched against its
   name; anything else has to be a String. */
static mrb_value
match_operand(mrb_state *mrb, mrb_value obj)
{
  if (mrb_symbol_p(obj)) return mrb_sym_str(mrb, mrb_symbol(obj));
  return mrb_ensure_string_type(mrb, obj);
}

/* Internal: execute match and create MatchData.
   Returns MatchData on match, nil on no match.
   Sets $~ and $1-$9 globals, unless `publish` says the caller owns them: a
   search that publishes nothing clears nothing either, so the globals come
   out of it exactly as they went in. */
static mrb_value
exec_match(mrb_state *mrb, mrb_value self, mrb_value str, mrb_int pos, mrb_bool publish)
{
  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  int cap_size = pat->num_captures * 2;
  int *captures = (int*)mrb_malloc(mrb, sizeof(int) * cap_size);
  memset(captures, -1, sizeof(int) * cap_size);
  int ncap = mrb_re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), pos,
                     captures, cap_size, re_binary_string_p(str));

  if (ncap == 0) {
    mrb_free(mrb, captures);
    if (publish) clear_match_globals(mrb);
    return mrb_nil_value();
  }
  mrb_value md = create_matchdata(mrb, self, str, captures, cap_size, publish);
  mrb_free(mrb, captures);
  return md;
}

/*
 * Regexp#match(str, pos=0)
 */
static mrb_value
regexp_match(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_value block = mrb_nil_value();
  mrb_int pos = 0;
  mrb_value md;

  mrb_get_args(mrb, "o|i&", &str, &pos, &block);
  if (mrb_nil_p(str)) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  str = match_operand(mrb, str);
  pos = re_char_to_byte(mrb, str, pos);
  if (pos < 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }

  re_check_encoding(mrb, str);
  md = exec_match(mrb, self, str, pos, TRUE);
  if (!mrb_nil_p(md) && !mrb_nil_p(block)) {
    return mrb_yield(mrb, block, md);
  }
  return md;
}

/* The pattern of a class-method search entry point arrives as an argument
   rather than as `self`, so its type has to be established here. Every mrblib
   caller passes a pattern that already went through `Regexp.__check_pattern`
   or a `Regexp ===` guard, which makes this a backstop, not a gate. */
static void
check_regexp_arg(mrb_state *mrb, mrb_value re)
{
  if (!mrb_obj_is_kind_of(mrb, re, mrb_class_get_id(mrb, MRB_SYM(Regexp)))) {
    mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected Regexp)",
               mrb_obj_classname(mrb, re));
  }
}

/*
 * Regexp.__search(re, str, pos = 0, checked = false)
 *
 * Internal: `Regexp#match` with the pattern as an argument and no block form.
 * The String overrides in mrblib search through this so that the search never
 * dispatches on the pattern, where a singleton method would replace it; see
 * the note at the top of mrblib/string_regexp.rb. A nil subject clears the
 * match globals and answers nil, as `Regexp#match` does, which is what the
 * overrides use to report a miss.
 *
 * `checked` says the caller has settled the encoding question for the subject
 * and this search must not ask it again. `sub`, `sub!`, `gsub` and `gsub!` set
 * it when their pattern is a quoted String, which CRuby searches for without
 * reading the subject as UTF-8 at all.
 */
static mrb_value
regexp_s_search(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str;
  mrb_int pos = 0;
  mrb_bool checked = FALSE;

  mrb_get_args(mrb, "oo|ib", &re, &str, &pos, &checked);
  check_regexp_arg(mrb, re);
  if (mrb_nil_p(str)) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  str = match_operand(mrb, str);
  pos = re_char_to_byte(mrb, str, pos);
  if (pos < 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  if (!checked) re_check_encoding(mrb, str);
  return exec_match(mrb, re, str, pos, TRUE);
}

/*
 * Regexp.__byte_search(re, str, pos = 0, checked = false, publish = true)
 *
 * Internal: the byte-offset search the mrblib loops of `gsub`, `split` and
 * `byteindex` drive themselves. No position normalization, because the
 * callers already work in byte space, and no operand conversion, because
 * they always pass a String. The subject is the one the loop holds fixed, so
 * the check reads the flag core left on it after the first turn. `checked`
 * carries the same meaning as in `__search`: `gsub` sets it for a block over
 * a quoted String pattern.
 *
 * `publish` says whether the match becomes the one the match globals describe.
 * A loop that walks past a match on the way to the one it wants clears it:
 * `rindex` and its family pass FALSE and publish the match they settled on
 * with `MatchData#__set_globals`. `gsub` and `scan` cannot, since the block
 * they call reads the globals of the match it was handed.
 */
static mrb_value
regexp_s_byte_search(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str;
  mrb_int pos = 0;

  mrb_bool checked = FALSE;
  mrb_bool publish = TRUE;

  mrb_get_args(mrb, "oS|ibb", &re, &str, &pos, &checked, &publish);
  check_regexp_arg(mrb, re);
  if (!checked) re_check_encoding(mrb, str);
  return exec_match(mrb, re, str, pos, publish);
}

/* Internal: the search of `match?`, run with a NULL capture buffer so that
   it allocates no MatchData and leaves the match globals alone, which is the
   whole point of `match?`. */
static mrb_value
exec_match_p(mrb_state *mrb, mrb_value re, mrb_value str, mrb_int pos)
{
  if (mrb_nil_p(str)) return mrb_false_value();
  str = match_operand(mrb, str);
  pos = re_char_to_byte(mrb, str, pos);
  if (pos < 0) return mrb_false_value();

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  re_check_encoding(mrb, str);

  int ncap = mrb_re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), pos, NULL, 0,
                         re_binary_string_p(str));
  return mrb_bool_value(ncap > 0);
}

/*
 * Regexp#match?(str, pos=0)
 */
static mrb_value
regexp_match_p(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_int pos = 0;
  mrb_get_args(mrb, "o|i", &str, &pos);
  return exec_match_p(mrb, self, str, pos);
}

/*
 * Regexp.__search_p(re, str, pos = 0)
 *
 * Internal: `Regexp#match?` with the pattern as an argument, for the
 * `String#match?` override; the same boundary as `Regexp.__search`.
 */
static mrb_value
regexp_s_search_p(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str;
  mrb_int pos = 0;

  mrb_get_args(mrb, "oo|i", &re, &str, &pos);
  check_regexp_arg(mrb, re);
  return exec_match_p(mrb, re, str, pos);
}

/*
 * Regexp#=~(str)
 */
static mrb_value
regexp_match_op(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "o", &str);
  if (mrb_nil_p(str)) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  str = match_operand(mrb, str);
  re_check_encoding(mrb, str);

  mrb_value md = exec_match(mrb, self, str, 0, TRUE);
  if (mrb_nil_p(md)) return mrb_nil_value();

  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  return mrb_int_value(mrb, re_byte_to_char(mrb, m->source, m->captures[0]));
}

/*
 * Regexp#===(str)
 */
static mrb_value
regexp_case_match(mrb_state *mrb, mrb_value self)
{
  mrb_value str, md;
  mrb_regexp_pattern *pat;

  mrb_get_args(mrb, "o", &str);
  if (!mrb_string_p(str) && !mrb_symbol_p(str)) return mrb_false_value();
  str = match_operand(mrb, str);

  pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) return mrb_false_value();
  re_check_encoding(mrb, str);

  md = exec_match(mrb, self, str, 0, TRUE);
  return mrb_bool_value(!mrb_nil_p(md));
}

/*
 * Regexp#source
 */
static mrb_value
regexp_source(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@source"));
}

/*
 * Regexp#options - convert internal flags to Ruby constants
 * Internal: IGNORECASE=1, MULTILINE=2, DOTALL=4, EXTENDED=8
 * Ruby:     IGNORECASE=1, EXTENDED=2, MULTILINE=4
 */
static mrb_value
regexp_options(mrb_state *mrb, mrb_value self)
{
  uint32_t iflags = get_iflags(mrb, self);
  mrb_int opts = 0;
  if (iflags & RE_FLAG_IGNORECASE) opts |= 1;  /* Regexp::IGNORECASE */
  if (iflags & RE_FLAG_EXTENDED) opts |= 2;     /* Regexp::EXTENDED */
  if (iflags & RE_FLAG_MULTILINE) opts |= 4;    /* Regexp::MULTILINE */
  return mrb_fixnum_value(opts);
}

/*
 * Regexp#casefold?
 */
static mrb_value
regexp_casefold_p(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value((get_iflags(mrb, self) & RE_FLAG_IGNORECASE) != 0);
}

/* The flag letters of the displayed forms, in the order Ruby writes them.
   Regexp#to_s and Regexp#inspect both walk this table, so the two cannot
   drift apart. RE_FLAG_DOTALL is the other half of Ruby's `m` and is
   always set together with RE_FLAG_MULTILINE, so testing one of the pair
   is enough. */
static const struct {
  uint32_t bit;
  char letter;
} re_flag_letters[] = {
  { RE_FLAG_MULTILINE,  'm' },
  { RE_FLAG_IGNORECASE, 'i' },
  { RE_FLAG_EXTENDED,   'x' },
};

#define RE_FLAG_LETTER_COUNT (sizeof(re_flag_letters) / sizeof(re_flag_letters[0]))

/*
 * Regexp#to_s - (?on-off:source) format
 *
 * The flags that are off are named after a '-', and that run is left out
 * only when none of them are. Spelling them out is what keeps the result
 * meaningful once it is interpolated into another pattern: written as
 * "(?i:a)", the embedded source in /#{/a/i}b/m would pick up the
 * enclosing pattern's flags instead of carrying only its own.
 */
static mrb_value
regexp_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_value src = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@source"));
  uint32_t flags = get_iflags(mrb, self);
  char off[RE_FLAG_LETTER_COUNT];
  mrb_int noff = 0;

  mrb_value result = mrb_str_new_lit(mrb, "(?");
  for (size_t i = 0; i < RE_FLAG_LETTER_COUNT; i++) {
    if (flags & re_flag_letters[i].bit) {
      mrb_str_cat(mrb, result, &re_flag_letters[i].letter, 1);
    }
    else {
      off[noff++] = re_flag_letters[i].letter;
    }
  }
  if (noff > 0) {
    mrb_str_cat_lit(mrb, result, "-");
    mrb_str_cat(mrb, result, off, noff);
  }
  mrb_str_cat_lit(mrb, result, ":");
  mrb_str_cat_str(mrb, result, src);
  mrb_str_cat_lit(mrb, result, ")");
  return result;
}

static mrb_value
regexp_inspect(mrb_state *mrb, mrb_value self)
{
  mrb_value src = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@source"));
  uint32_t flags = get_iflags(mrb, self);

  mrb_value result = mrb_str_new_lit(mrb, "/");
  mrb_str_cat_str(mrb, result, src);
  mrb_str_cat_lit(mrb, result, "/");
  for (size_t i = 0; i < RE_FLAG_LETTER_COUNT; i++) {
    if (flags & re_flag_letters[i].bit) {
      mrb_str_cat(mrb, result, &re_flag_letters[i].letter, 1);
    }
  }
  return result;
}

/*
 * Regexp#== (and eql?)
 */
static mrb_value
regexp_eql(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  if (!mrb_obj_is_kind_of(mrb, other, mrb_class_get(mrb, "Regexp"))) {
    return mrb_false_value();
  }
  mrb_value src1 = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@source"));
  mrb_value src2 = mrb_iv_get(mrb, other, mrb_intern_lit(mrb, "@source"));
  if (!mrb_string_p(src1) || !mrb_string_p(src2)) {
    return mrb_bool_value(mrb_obj_eq(mrb, self, other));
  }
  if (!mrb_str_equal(mrb, src1, src2)) return mrb_false_value();
  return mrb_bool_value(get_iflags(mrb, self) == get_iflags(mrb, other));
}

/*
 * Regexp#hash
 */
static mrb_value
regexp_hash(mrb_state *mrb, mrb_value self)
{
  mrb_value src = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@source"));
  uint32_t h = mrb_string_p(src) ? mrb_str_hash(mrb, src) : 0;
  h ^= get_iflags(mrb, self) * 0x9e3779b9;  /* mix flags into hash */
  return mrb_int_value(mrb, (mrb_int)h);
}

/*
 * Regexp.escape(str)
 */
static mrb_value
regexp_escape(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);

  const char *s = RSTRING_PTR(str);
  mrb_int len = RSTRING_LEN(str);
  mrb_value result = mrb_str_new_capa(mrb, len + len / 4);

  for (mrb_int i = 0; i < len; i++) {
    char c = s[i];
    switch (c) {
    /* Control characters become two-character escapes so that the result
       stays printable; the rest are emitted as a backslash and the byte. */
    case '\n': mrb_str_cat_lit(mrb, result, "\\n"); break;
    case '\t': mrb_str_cat_lit(mrb, result, "\\t"); break;
    case '\r': mrb_str_cat_lit(mrb, result, "\\r"); break;
    case '\f': mrb_str_cat_lit(mrb, result, "\\f"); break;
    case '\v': mrb_str_cat_lit(mrb, result, "\\v"); break;
    case '\\': case '.': case '*': case '+': case '?': case '|':
    case '(': case ')': case '[': case ']': case '{': case '}':
    case '^': case '$':
    /* `#`, `-` and space are only special under `/x` or inside `[...]`,
       but escaping them unconditionally keeps the result literal in
       every mode. */
    case '#': case '-': case ' ':
      mrb_str_cat_lit(mrb, result, "\\");
      /* fall through */
    default:
      mrb_str_cat(mrb, result, &c, 1);
      break;
    }
  }
  return result;
}

/* --- MatchData methods --- */

/* Resolve a String or Symbol to the group it names. Shared by MatchData#[],
   #begin and #end: the three disagree about what an out-of-range integer
   means, but a name is looked up the same way for all of them. Does not
   return when the name reaches no group. */
static mrb_int
matchdata_name_to_group(mrb_state *mrb, mrb_match_data *md, mrb_value arg)
{
  const char *name;
  mrb_int name_len;
  if (mrb_symbol_p(arg)) {
    name = mrb_sym_name_len(mrb, mrb_symbol(arg), &name_len);
  }
  else {
    name = RSTRING_PTR(arg);
    name_len = RSTRING_LEN(arg);
  }
  /* look up name in regexp's named captures */
  mrb_regexp_pattern *pat = NULL;
  if (!mrb_nil_p(md->regexp)) {
    pat = DATA_GET_PTR(mrb, md->regexp, &regexp_type, mrb_regexp_pattern);
  }
  /* A stored name never exceeds RE_MAX_NAME_LEN, so a longer request can
     name no group. Rejecting it here keeps the cast in the loop lossless;
     without it the length test truncates while the memcmp() next to it does
     not. */
  if (pat && RE_NAME_LEN_FITS(name_len)) {
    for (uint16_t i = 0; i < pat->num_named; i++) {
      if (pat->named_captures[i].name_len == (uint32_t)name_len &&
          memcmp(pat->named_captures[i].name, name, name_len) == 0) {
        return pat->named_captures[i].group;
      }
    }
  }
  /* A name that resolves to no group is a mistake at the point of the call,
     not a failed match. CRuby raises here even when the pattern has no
     named group at all. */
  mrb_raisef(mrb, E_INDEX_ERROR, "undefined group name reference: %l", name, (size_t)name_len);
}

/*
 * MatchData#[](n)
 */
static mrb_value
matchdata_aref(mrb_state *mrb, mrb_value self)
{
  mrb_value arg;
  mrb_get_args(mrb, "o", &arg);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();

  mrb_int idx;
  if (mrb_string_p(arg) || mrb_symbol_p(arg)) {
    /* named capture access */
    idx = matchdata_name_to_group(mrb, md, arg);
  }
  else {
    idx = mrb_as_int(mrb, arg);
    if (idx < 0) {
      /* A negative index counts back from the last group. CRuby's
         rb_reg_nth_match() drops the result unless it is positive, so the
         lowest group a negative index reaches is 1, never the whole match:
         /(a)(b)/.match("ab")[-3] is nil, not "ab". */
      idx += md->num_captures;
      if (idx <= 0) return mrb_nil_value();
    }
  }

  if (idx >= md->num_captures) return mrb_nil_value();
  int start = md->captures[idx * 2];
  int end = md->captures[idx * 2 + 1];
  if (start < 0) return mrb_nil_value();

  return re_byte_substr(mrb, md->source, start, end - start);
}

/* Build array of capture strings from group `from` to num_captures-1 */
static mrb_value
matchdata_to_ary(mrb_state *mrb, mrb_value self, int from)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_ary_new(mrb);

  mrb_value ary = mrb_ary_new_capa(mrb, md->num_captures - from);
  for (int i = from; i < md->num_captures; i++) {
    int s = md->captures[i * 2];
    int e = md->captures[i * 2 + 1];
    if (s < 0) {
      mrb_ary_push(mrb, ary, mrb_nil_value());
    }
    else {
      mrb_ary_push(mrb, ary, re_byte_substr(mrb, md->source, s, e - s));
    }
  }
  return ary;
}

static mrb_value
matchdata_captures(mrb_state *mrb, mrb_value self)
{
  return matchdata_to_ary(mrb, self, 1);
}

static mrb_value
matchdata_to_a(mrb_state *mrb, mrb_value self)
{
  return matchdata_to_ary(mrb, self, 0);
}

/*
 * MatchData#begin(n) / MatchData#end(n)
 */

/* begin and end return an offset, and nil is not one, so an argument they
   cannot use is an error rather than a missing result. That is stricter than
   MatchData#[], which has nil to return for a group that did not participate
   and reuses it for an index out of range. A group that exists but did not
   participate is still nil here; only the argument itself raises. Does not
   return when the argument reaches no group. */
static mrb_int
matchdata_group_arg(mrb_state *mrb, mrb_match_data *md, mrb_value arg)
{
  if (mrb_string_p(arg) || mrb_symbol_p(arg)) {
    return matchdata_name_to_group(mrb, md, arg);
  }
  mrb_int idx = mrb_as_int(mrb, arg);
  if (idx < 0 || idx >= md->num_captures) {
    mrb_raisef(mrb, E_INDEX_ERROR, "index %i out of matches", idx);
  }
  return idx;
}

static mrb_value
matchdata_begin(mrb_state *mrb, mrb_value self)
{
  mrb_value arg;
  mrb_get_args(mrb, "o", &arg);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  mrb_int idx = matchdata_group_arg(mrb, md, arg);
  int pos = md->captures[idx * 2];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, re_byte_to_char(mrb, md->source, pos));
}

static mrb_value
matchdata_end(mrb_state *mrb, mrb_value self)
{
  mrb_value arg;
  mrb_get_args(mrb, "o", &arg);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  mrb_int idx = matchdata_group_arg(mrb, md, arg);
  int pos = md->captures[idx * 2 + 1];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, re_byte_to_char(mrb, md->source, pos));
}

/* Private byte-offset accessors used by String#gsub, which works in byte
   space (byteslice). begin/end report character offsets; these report the
   raw byte offsets the engine recorded. */
static mrb_value
matchdata_byte_begin(mrb_state *mrb, mrb_value self)
{
  mrb_int idx;
  mrb_get_args(mrb, "i", &idx);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || idx < 0 || idx >= md->num_captures) return mrb_nil_value();
  int pos = md->captures[idx * 2];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, pos);
}

static mrb_value
matchdata_byte_end(mrb_state *mrb, mrb_value self)
{
  mrb_int idx;
  mrb_get_args(mrb, "i", &idx);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || idx < 0 || idx >= md->num_captures) return mrb_nil_value();
  int pos = md->captures[idx * 2 + 1];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, pos);
}

/* Private: republish $~ and the thirteen names derived from it. Used by the
   mrblib loops that drive Regexp.__byte_search themselves, where the failing
   call that ends the loop clears the match the loop is supposed to leave behind.
   The names other than $~ are not assignable from Ruby, so restoring them
   has to come from here. */
static mrb_value
matchdata_set_globals(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  set_match_globals(mrb, self, md->source, md->captures, md->num_captures);
  return self;
}

/*
 * MatchData#pre_match / #post_match
 */
static mrb_value
matchdata_pre(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || md->captures[0] < 0) return mrb_nil_value();
  return re_byte_substr(mrb, md->source, 0, md->captures[0]);
}

static mrb_value
matchdata_post(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || md->captures[1] < 0) return mrb_nil_value();
  int pos = md->captures[1];
  return re_byte_substr(mrb, md->source, pos, RSTRING_LEN(md->source) - pos);
}

/*
 * MatchData#length / #size
 */
static mrb_value
matchdata_length(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_fixnum_value(0);
  return mrb_fixnum_value(md->num_captures);
}

/*
 * MatchData#named_captures
 */
static mrb_value
matchdata_named_captures(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_hash_new(mrb);

  mrb_regexp_pattern *pat = NULL;
  if (!mrb_nil_p(md->regexp)) {
    pat = DATA_GET_PTR(mrb, md->regexp, &regexp_type, mrb_regexp_pattern);
  }
  if (!pat || pat->num_named == 0) return mrb_hash_new(mrb);

  mrb_value result = mrb_hash_new_capa(mrb, pat->num_named);
  for (uint16_t i = 0; i < pat->num_named; i++) {
    mrb_value name = mrb_str_new(mrb, pat->named_captures[i].name, pat->named_captures[i].name_len);
    int group = pat->named_captures[i].group;
    mrb_value val = mrb_nil_value();
    if (group >= 0 && group < md->num_captures) {
      int s = md->captures[group * 2];
      int e = md->captures[group * 2 + 1];
      if (s >= 0) val = re_byte_substr(mrb, md->source, s, e - s);
    }
    mrb_hash_set(mrb, result, name, val);
  }
  return result;
}

/*
 * MatchData#string - the original string (frozen copy)
 */
static mrb_value
matchdata_string(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  return md->source;
}

/*
 * MatchData#regexp - the Regexp used
 */
static mrb_value
matchdata_regexp(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_nil_value();
  return md->regexp;
}

/*
 * MatchData#to_s - full match string
 */
static mrb_value
matchdata_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || md->captures[0] < 0) return mrb_nil_value();
  int s = md->captures[0];
  int e = md->captures[1];
  return re_byte_substr(mrb, md->source, s, e - s);
}

/* --- C-level gsub/sub/scan core --- */

/* Process replacement string: expand \0-\9, \&, \`, \', \+, \\ */
static void
apply_replacement(mrb_state *mrb, mrb_value result,
                  const char *rep, mrb_int rep_len,
                  const char *str, mrb_int str_len, int *captures, int ncap)
{
  mrb_int i = 0;
  while (i < rep_len) {
    if (rep[i] == '\\' && i + 1 < rep_len) {
      char c = rep[i + 1];
      if (c >= '0' && c <= '9') {
        int g = c - '0';
        if (g < ncap && captures[g * 2] >= 0) {
          int s = captures[g * 2], e = captures[g * 2 + 1];
          mrb_str_cat(mrb, result, str + s, e - s);
        }
      }
      else if (c == '&') {
        if (captures[0] >= 0) {
          mrb_str_cat(mrb, result, str + captures[0], captures[1] - captures[0]);
        }
      }
      else if (c == '`') {
        if (captures[0] >= 0) {
          mrb_str_cat(mrb, result, str, captures[0]);
        }
      }
      else if (c == '\'') {
        if (captures[1] >= 0) {
          /* post-match: bytes after the match end. Use the subject's real
             byte length, not strlen(str): the subject may contain embedded
             NUL bytes or be a non-NUL-terminated shared substring, in which
             case strlen() underflows the length (issue #6892). */
          mrb_str_cat(mrb, result, str + captures[1], str_len - captures[1]);
        }
      }
      else if (c == '+') {
        /* last successful capture */
        for (int g = ncap - 1; g >= 1; g--) {
          if (captures[g * 2] >= 0) {
            int s = captures[g * 2], e = captures[g * 2 + 1];
            mrb_str_cat(mrb, result, str + s, e - s);
            break;
          }
        }
      }
      else if (c == '\\') {
        mrb_str_cat_lit(mrb, result, "\\");
      }
      else {
        mrb_str_cat(mrb, result, rep + i, 2);  /* \x as-is */
      }
      i += 2;
    }
    else {
      /* find next backslash or end for batch copy */
      mrb_int j = i + 1;
      while (j < rep_len && rep[j] != '\\') j++;
      mrb_str_cat(mrb, result, rep + i, j - i);
      i = j;
    }
  }
}

/* Check if replacement contains backslash */
static mrb_bool
has_backslash(const char *s, mrb_int len)
{
  return memchr(s, '\\', len) != NULL;
}

/* What sub and gsub build is the subject's bytes with the replacement spliced
   in, so it is read the way the subject was; a replacement that was read as
   bytes and goes above ASCII hands its reading over the way any appended
   byte-read bytes do. A gsub that matched nothing spliced nothing, so its
   result holds the subject alone and the replacement says nothing about it.
   This is where CRuby lands on every pair it accepts. */
static void
re_mark_spliced(mrb_value result, mrb_value subject, mrb_value replacement,
                mrb_bool spliced)
{
  if (!re_binary_string_p(subject)) {
    if (!spliced || !re_binary_string_p(replacement)) return;
    const char *p = RSTRING_PTR(replacement);
    const char *e = p + RSTRING_LEN(replacement);
    while (p < e && !(*p & 0x80)) p++;
    if (p == e) return;
  }
  RSTR_SET_BINARY_FLAG(mrb_str_ptr(result));
}

/*
 * Regexp.__gsub_str(re, str, replacement, checked = false) - gsub core without block
 *
 * `checked` carries the same meaning as in `__search`.
 */
static mrb_value
regexp_s_gsub_str(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str, replacement;
  mrb_bool checked = FALSE;
  mrb_get_args(mrb, "oSS|b", &re, &str, &replacement, &checked);
  check_regexp_arg(mrb, re);

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  if (!checked) re_check_encoding(mrb, str);

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  const char *rep = RSTRING_PTR(replacement);
  mrb_int rep_len = RSTRING_LEN(replacement);
  mrb_bool need_expand = has_backslash(rep, rep_len);
  mrb_bool binary = re_binary_string_p(str);

  int ncap = pat->num_captures;
  int cap_size = ncap * 2;
  int *captures = (int*)mrb_malloc(mrb, sizeof(int) * cap_size);
  mrb_value result = mrb_str_new_capa(mrb, slen);
  int ai = mrb_gc_arena_save(mrb);

  mrb_int pos = 0;
  int last_ncap = 0;
  int last_captures[RE_MAX_CAPTURES * 2];

  while (pos <= slen) {
    memset(captures, -1, sizeof(int) * cap_size);
    int n = mrb_re_exec(mrb, pat, s, slen, pos, captures, cap_size, binary);
    if (n == 0) break;

    /* save last match for $~ */
    last_ncap = cap_size;
    memcpy(last_captures, captures, sizeof(int) * cap_size);

    /* append pre-match */
    if (captures[0] > pos) {
      mrb_str_cat(mrb, result, s + pos, captures[0] - pos);
    }

    /* append replacement */
    if (need_expand) {
      apply_replacement(mrb, result, rep, rep_len, s, slen, captures, ncap);
    }
    else {
      mrb_str_cat(mrb, result, rep, rep_len);
    }

    /* advance position. A zero-width match (start == end) must step past the
       match position -- even when it was found ahead of `pos`, e.g. `^` at the
       next line start -- otherwise the next search re-applies it there. Copy
       the whole character so multibyte text is not split. */
    if (captures[1] == captures[0]) {
      if (captures[1] < slen) {
        int clen = mrb_re_charlen(s + captures[1], s + slen, binary);
        mrb_str_cat(mrb, result, s + captures[1], clen);
        pos = captures[1] + clen;
      }
      else {
        pos = captures[1] + 1;
      }
    }
    else {
      pos = captures[1];
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  /* append remainder */
  if (pos <= slen) {
    mrb_str_cat(mrb, result, s + pos, slen - pos);
  }

  mrb_free(mrb, captures);

  /* set $~ from last match */
  if (last_ncap > 0) {
    create_matchdata(mrb, re, str, last_captures, last_ncap, TRUE);
  }
  else {
    clear_match_globals(mrb);
  }

  re_mark_spliced(result, str, replacement, last_ncap > 0);
  return result;
}

/*
 * Regexp.__sub_str(re, str, replacement, checked = false) - sub core without block
 *
 * `checked` carries the same meaning as in `__search`.
 */
static mrb_value
regexp_s_sub_str(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str, replacement;
  mrb_bool checked = FALSE;
  mrb_get_args(mrb, "oSS|b", &re, &str, &replacement, &checked);
  check_regexp_arg(mrb, re);

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  if (!checked) re_check_encoding(mrb, str);

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  const char *rep = RSTRING_PTR(replacement);
  mrb_int rep_len = RSTRING_LEN(replacement);

  int cap_size = pat->num_captures * 2;
  int *captures = (int*)mrb_malloc(mrb, sizeof(int) * cap_size);
  memset(captures, -1, sizeof(int) * cap_size);

  int n = mrb_re_exec(mrb, pat, s, slen, 0, captures, cap_size, re_binary_string_p(str));
  if (n == 0) {
    mrb_free(mrb, captures);
    clear_match_globals(mrb);
    return mrb_str_dup(mrb, str);
  }

  mrb_value result = mrb_str_new_capa(mrb, slen);

  /* pre-match */
  if (captures[0] > 0) {
    mrb_str_cat(mrb, result, s, captures[0]);
  }

  /* replacement */
  if (has_backslash(rep, rep_len)) {
    apply_replacement(mrb, result, rep, rep_len, s, slen, captures, pat->num_captures);
  }
  else {
    mrb_str_cat(mrb, result, rep, rep_len);
  }

  /* post-match */
  if (captures[1] < slen) {
    mrb_str_cat(mrb, result, s + captures[1], slen - captures[1]);
  }

  create_matchdata(mrb, re, str, captures, cap_size, TRUE);
  mrb_free(mrb, captures);
  re_mark_spliced(result, str, replacement, TRUE);
  return result;
}

/*
 * Regexp.__scan(re, str) - scan core, returns array
 */
static mrb_value
regexp_s_scan(mrb_state *mrb, mrb_value klass)
{
  mrb_value re, str;
  mrb_get_args(mrb, "oS", &re, &str);
  check_regexp_arg(mrb, re);

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, re, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");
  re_check_encoding(mrb, str);

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  mrb_bool binary = re_binary_string_p(str);
  int ncap = pat->num_captures;
  int cap_size = ncap * 2;
  int *captures = (int*)mrb_malloc(mrb, sizeof(int) * cap_size);

  mrb_value ary = mrb_ary_new(mrb);
  int ai = mrb_gc_arena_save(mrb);
  mrb_int pos = 0;
  int last_ncap = 0;
  int last_captures[RE_MAX_CAPTURES * 2];

  while (pos <= slen) {
    memset(captures, -1, sizeof(int) * cap_size);
    int n = mrb_re_exec(mrb, pat, s, slen, pos, captures, cap_size, binary);
    if (n == 0) break;

    last_ncap = cap_size;
    memcpy(last_captures, captures, sizeof(int) * cap_size);

    if (ncap <= 1) {
      /* no capture groups: push the matched string */
      mrb_ary_push(mrb, ary,
        re_byte_substr(mrb, str, captures[0], captures[1] - captures[0]));
    }
    else {
      /* one or more capture groups: push an array of the captures. CRuby
         returns an array per match whenever the pattern has any group, so a
         single group yields a one-element array (e.g. [["x"]]), not a bare
         string. */
      mrb_value sub = mrb_ary_new_capa(mrb, ncap - 1);
      for (int i = 1; i < ncap; i++) {
        if (captures[i * 2] >= 0) {
          mrb_ary_push(mrb, sub,
            re_byte_substr(mrb, str, captures[i*2], captures[i*2+1] - captures[i*2]));
        }
        else {
          mrb_ary_push(mrb, sub, mrb_nil_value());
        }
      }
      mrb_ary_push(mrb, ary, sub);
    }

    /* Advance past the match. A zero-width match (start == end) must step
       one byte forward, even when it landed ahead of `pos` (e.g. `^` found
       at the next line start), otherwise the next search re-reports the same
       position. */
    if (captures[1] == captures[0]) {
      pos = captures[1] + 1;
    }
    else {
      pos = captures[1];
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  mrb_free(mrb, captures);

  if (last_ncap > 0) {
    create_matchdata(mrb, re, str, last_captures, last_ncap, TRUE);
  }
  else {
    clear_match_globals(mrb);
  }

  return ary;
}

/* Check the pattern given to String#match, #match?, #sub, #gsub, #scan and
   #split: a Regexp or a String passes through, everything else raises. The
   test runs here rather than in Ruby so it never dispatches on the argument,
   where a redefined `is_a?` or `class` could pose as a Regexp or fake the type
   name. What to do with an accepted String is left to the caller, which
   compiles it for `match` and quotes it first for `sub` and friends, so this
   needs no callback into the VM. CRuby names `nil`, `true` and `false` by
   value and everything else by class. */
static mrb_value
regexp_check_pattern(mrb_state *mrb, mrb_value self)
{
  mrb_value re;
  mrb_get_args(mrb, "o", &re);

  if (mrb_obj_is_kind_of(mrb, re, mrb_class_get_id(mrb, MRB_SYM(Regexp)))) return re;
  if (mrb_string_p(re)) return re;

  const char *name;
  if (mrb_nil_p(re)) name = "nil";
  else if (mrb_true_p(re)) name = "true";
  else if (mrb_false_p(re)) name = "false";
  else name = mrb_obj_classname(mrb, re);
  mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected Regexp)", name);
}

/* --- Gem init --- */

void
mrb_mruby_regexp_gem_init(mrb_state *mrb)
{
  struct RClass *re = mrb_define_class(mrb, "Regexp", mrb->object_class);
  MRB_SET_INSTANCE_TT(re, MRB_TT_CDATA);

  /* Constants */
  mrb_define_const(mrb, re, "IGNORECASE", mrb_fixnum_value(1));
  mrb_define_const(mrb, re, "EXTENDED", mrb_fixnum_value(2));
  mrb_define_const(mrb, re, "MULTILINE", mrb_fixnum_value(4));

  /* Class methods */
  mrb_define_method(mrb, re, "initialize", regexp_init, MRB_ARGS_ARG(1, 2));
  /* compile is defined in Ruby (mrblib) as alias for new */
  mrb_define_class_method(mrb, re, "escape", regexp_escape, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "quote", regexp_escape, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "__binary_string?", regexp_binary_string_p, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "__check_encoding", regexp_check_encoding, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "__check_pattern", regexp_check_pattern, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, re, "__search", regexp_s_search, MRB_ARGS_ARG(2, 2));
  mrb_define_class_method(mrb, re, "__byte_search", regexp_s_byte_search, MRB_ARGS_ARG(2, 3));
  mrb_define_class_method(mrb, re, "__search_p", regexp_s_search_p, MRB_ARGS_ARG(2, 1));

  /* Instance methods */
  mrb_define_method(mrb, re, "match", regexp_match, MRB_ARGS_ARG(1, 1)|MRB_ARGS_BLOCK());
  mrb_define_method(mrb, re, "match?", regexp_match_p, MRB_ARGS_ARG(1, 1));
  mrb_define_method(mrb, re, "=~", regexp_match_op, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, re, "===", regexp_case_match, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, re, "source", regexp_source, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "inspect", regexp_inspect, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "to_s", regexp_to_s, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "==", regexp_eql, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, re, "eql?", regexp_eql, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, re, "hash", regexp_hash, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "options", regexp_options, MRB_ARGS_NONE());
  mrb_define_method(mrb, re, "casefold?", regexp_casefold_p, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, re, "__gsub_str", regexp_s_gsub_str, MRB_ARGS_ARG(3, 1));
  mrb_define_class_method(mrb, re, "__sub_str", regexp_s_sub_str, MRB_ARGS_ARG(3, 1));
  mrb_define_class_method(mrb, re, "__scan", regexp_s_scan, MRB_ARGS_REQ(2));

  /* MatchData class */
  struct RClass *md = mrb_define_class(mrb, "MatchData", mrb->object_class);
  MRB_SET_INSTANCE_TT(md, MRB_TT_CDATA);

  mrb_define_method(mrb, md, "[]", matchdata_aref, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "captures", matchdata_captures, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "to_a", matchdata_to_a, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "length", matchdata_length, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "size", matchdata_length, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "begin", matchdata_begin, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "end", matchdata_end, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "__byte_begin", matchdata_byte_begin, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "__byte_end", matchdata_byte_end, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, md, "__set_globals", matchdata_set_globals, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "pre_match", matchdata_pre, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "post_match", matchdata_post, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "named_captures", matchdata_named_captures, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "string", matchdata_string, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "regexp", matchdata_regexp, MRB_ARGS_NONE());
  mrb_define_method(mrb, md, "to_s", matchdata_to_s, MRB_ARGS_NONE());
}

void
mrb_mruby_regexp_gem_final(mrb_state *mrb)
{
}
