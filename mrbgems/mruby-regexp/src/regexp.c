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
  re_free(mrb, (mrb_regexp_pattern*)ptr);
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

  pat = re_compile(mrb, RSTRING_PTR(pattern), RSTRING_LEN(pattern), flags);

  DATA_TYPE(self) = &regexp_type;
  DATA_PTR(self) = pat;

  /* store source for #source and #inspect */
  mrb_iv_set(mrb, self, mrb_intern_lit(mrb, "@source"), pattern);
  mrb_iv_set(mrb, self, mrb_intern_lit(mrb, "@flags"), mrb_int_value(mrb, (mrb_int)flags));

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

static void
ensure_nth_syms(mrb_state *mrb)
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
}

static void
clear_match_globals(mrb_state *mrb)
{
  ensure_nth_syms(mrb);
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$~"), mrb_nil_value());
  for (int i = 0; i < 9; i++) {
    mrb_gv_set(mrb, nth_syms[i], mrb_nil_value());
  }
}

/* Create MatchData from captures */
static mrb_value
create_matchdata(mrb_state *mrb, mrb_value regexp, mrb_value str, int *captures, int ncap)
{
  ensure_nth_syms(mrb);

  struct RClass *md_class = mrb_class_get(mrb, "MatchData");
  mrb_match_data *md = (mrb_match_data*)mrb_malloc(mrb, sizeof(mrb_match_data));
  md->source = str;
  md->regexp = regexp;
  md->num_captures = ncap / 2;
  md->captures = (int*)mrb_malloc(mrb, sizeof(int) * ncap);
  memcpy(md->captures, captures, sizeof(int) * ncap);

  mrb_value obj = mrb_obj_value(mrb_data_object_alloc(mrb, md_class, md, &matchdata_type));
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$~"), obj);

  /* set $1-$9 from captures */
  for (int i = 0; i < 9; i++) {
    mrb_value val = mrb_nil_value();
    int g = i + 1;
    if (g < md->num_captures && captures[g*2] >= 0) {
      val = mrb_str_substr(mrb, str, captures[g*2], captures[g*2+1] - captures[g*2]);
    }
    mrb_gv_set(mrb, nth_syms[i], val);
  }

  return obj;
}

/* Internal: execute match and create MatchData.
   Returns MatchData on match, nil on no match.
   Sets $~ and $1-$9 globals. */
static mrb_value
exec_match(mrb_state *mrb, mrb_value self, mrb_value str, mrb_int pos)
{
  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  int cap_size = pat->num_captures * 2;
  int *captures = (int*)mrb_malloc(mrb, sizeof(int) * cap_size);
  memset(captures, -1, sizeof(int) * cap_size);
  int ncap = re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), pos,
                     captures, cap_size);

  if (ncap == 0) {
    mrb_free(mrb, captures);
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  mrb_value md = create_matchdata(mrb, self, str, captures, cap_size);
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
  mrb_int pos = 0;
  mrb_get_args(mrb, "S|i", &str, &pos);
  return exec_match(mrb, self, str, pos);
}

/*
 * Regexp#match?(str, pos=0)
 */
static mrb_value
regexp_match_p(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_int pos = 0;
  mrb_get_args(mrb, "S|i", &str, &pos);

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  int ncap = re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), pos, NULL, 0);
  return mrb_bool_value(ncap > 0);
}

/*
 * Regexp#=~(str)
 */
static mrb_value
regexp_match_op(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "o", &str);
  if (mrb_nil_p(str)) return mrb_nil_value();
  mrb_ensure_string_type(mrb, str);

  mrb_value md = exec_match(mrb, self, str, 0);
  if (mrb_nil_p(md)) return mrb_nil_value();

  mrb_match_data *m = DATA_GET_PTR(mrb, md, &matchdata_type, mrb_match_data);
  return mrb_int_value(mrb, m->captures[0]);
}

/*
 * Regexp#===(str)
 */
static mrb_value
regexp_case_match(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_regexp_pattern *pat;

  mrb_get_args(mrb, "o", &str);
  if (!mrb_string_p(str)) return mrb_false_value();

  pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) return mrb_false_value();

  int ncap = re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), 0, NULL, 0);
  return mrb_bool_value(ncap > 0);
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

/*
 * Regexp#to_s - CRuby-compatible (?flags:source) format
 */
static mrb_value
regexp_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_value src = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@source"));
  uint32_t flags = get_iflags(mrb, self);

  mrb_value result = mrb_str_new_lit(mrb, "(?");
  if (flags & RE_FLAG_IGNORECASE) mrb_str_cat_lit(mrb, result, "i");
  if (flags & RE_FLAG_MULTILINE) mrb_str_cat_lit(mrb, result, "m");
  if (flags & RE_FLAG_EXTENDED) mrb_str_cat_lit(mrb, result, "x");
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
  if (flags & RE_FLAG_IGNORECASE) mrb_str_cat_lit(mrb, result, "i");
  if (flags & RE_FLAG_MULTILINE) mrb_str_cat_lit(mrb, result, "m");
  if (flags & RE_FLAG_EXTENDED) mrb_str_cat_lit(mrb, result, "x");
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
  uint32_t h = mrb_str_hash(mrb, src);
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
    case '\\': case '.': case '*': case '+': case '?': case '|':
    case '(': case ')': case '[': case ']': case '{': case '}':
    case '^': case '$':
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
    if (pat) {
      for (uint16_t i = 0; i < pat->num_named; i++) {
        if (pat->named_captures[i].name_len == (uint16_t)name_len &&
            memcmp(pat->named_captures[i].name, name, name_len) == 0) {
          idx = pat->named_captures[i].group;
          goto found;
        }
      }
    }
    return mrb_nil_value();
  }
  else {
    idx = mrb_as_int(mrb, arg);
  }

found:
  if (idx < 0 || idx >= md->num_captures) return mrb_nil_value();
  int start = md->captures[idx * 2];
  int end = md->captures[idx * 2 + 1];
  if (start < 0) return mrb_nil_value();

  return mrb_str_substr(mrb, md->source, start, end - start);
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
      mrb_ary_push(mrb, ary, mrb_str_substr(mrb, md->source, s, e - s));
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
static mrb_value
matchdata_begin(mrb_state *mrb, mrb_value self)
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
matchdata_end(mrb_state *mrb, mrb_value self)
{
  mrb_int idx;
  mrb_get_args(mrb, "i", &idx);

  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || idx < 0 || idx >= md->num_captures) return mrb_nil_value();
  int pos = md->captures[idx * 2 + 1];
  if (pos < 0) return mrb_nil_value();
  return mrb_int_value(mrb, pos);
}

/*
 * MatchData#pre_match / #post_match
 */
static mrb_value
matchdata_pre(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || md->captures[0] < 0) return mrb_nil_value();
  return mrb_str_substr(mrb, md->source, 0, md->captures[0]);
}

static mrb_value
matchdata_post(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md || md->captures[1] < 0) return mrb_nil_value();
  int pos = md->captures[1];
  return mrb_str_substr(mrb, md->source, pos, RSTRING_LEN(md->source) - pos);
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
      if (s >= 0) val = mrb_str_substr(mrb, md->source, s, e - s);
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
  return mrb_str_substr(mrb, md->source, s, e - s);
}

/* --- C-level gsub/sub/scan core --- */

/* Process replacement string: expand \0-\9, \&, \`, \', \+, \\ */
static void
apply_replacement(mrb_state *mrb, mrb_value result,
                  const char *rep, mrb_int rep_len,
                  const char *str, int *captures, int ncap)
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
          mrb_str_cat(mrb, result, str + captures[1], strlen(str) - captures[1]);
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

/*
 * Regexp#__gsub_str(str, replacement) - gsub core without block
 */
static mrb_value
regexp_gsub_str(mrb_state *mrb, mrb_value self)
{
  mrb_value str, replacement;
  mrb_get_args(mrb, "SS", &str, &replacement);

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  const char *rep = RSTRING_PTR(replacement);
  mrb_int rep_len = RSTRING_LEN(replacement);
  mrb_bool need_expand = has_backslash(rep, rep_len);

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
    int n = re_exec(mrb, pat, s, slen, pos, captures, cap_size);
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
      apply_replacement(mrb, result, rep, rep_len, s, captures, ncap);
    }
    else {
      mrb_str_cat(mrb, result, rep, rep_len);
    }

    /* advance position */
    int match_end = captures[1];
    if (match_end == pos) {
      /* zero-length match: copy one char and advance */
      if (pos < slen) {
        mrb_str_cat(mrb, result, s + pos, 1);
      }
      pos++;
    }
    else {
      pos = match_end;
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
    create_matchdata(mrb, self, str, last_captures, last_ncap);
  }
  else {
    clear_match_globals(mrb);
  }

  return result;
}

/*
 * Regexp#__sub_str(str, replacement) - sub core without block
 */
static mrb_value
regexp_sub_str(mrb_state *mrb, mrb_value self)
{
  mrb_value str, replacement;
  mrb_get_args(mrb, "SS", &str, &replacement);

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
  const char *rep = RSTRING_PTR(replacement);
  mrb_int rep_len = RSTRING_LEN(replacement);

  int cap_size = pat->num_captures * 2;
  int *captures = (int*)mrb_malloc(mrb, sizeof(int) * cap_size);
  memset(captures, -1, sizeof(int) * cap_size);

  int n = re_exec(mrb, pat, s, slen, 0, captures, cap_size);
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
    apply_replacement(mrb, result, rep, rep_len, s, captures, pat->num_captures);
  }
  else {
    mrb_str_cat(mrb, result, rep, rep_len);
  }

  /* post-match */
  if (captures[1] < slen) {
    mrb_str_cat(mrb, result, s + captures[1], slen - captures[1]);
  }

  create_matchdata(mrb, self, str, captures, cap_size);
  mrb_free(mrb, captures);
  return result;
}

/*
 * Regexp#__scan(str) - scan core, returns array
 */
static mrb_value
regexp_scan(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_get_args(mrb, "S", &str);

  mrb_regexp_pattern *pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  const char *s = RSTRING_PTR(str);
  mrb_int slen = RSTRING_LEN(str);
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
    int n = re_exec(mrb, pat, s, slen, pos, captures, cap_size);
    if (n == 0) break;

    last_ncap = cap_size;
    memcpy(last_captures, captures, sizeof(int) * cap_size);

    if (ncap <= 1) {
      /* no captures or just group 0: push matched string */
      mrb_ary_push(mrb, ary,
        mrb_str_substr(mrb, str, captures[0], captures[1] - captures[0]));
    }
    else if (ncap == 2) {
      /* single capture group: push capture string */
      if (captures[2] >= 0) {
        mrb_ary_push(mrb, ary,
          mrb_str_substr(mrb, str, captures[2], captures[3] - captures[2]));
      }
      else {
        mrb_ary_push(mrb, ary, mrb_nil_value());
      }
    }
    else {
      /* multiple captures: push array of captures */
      mrb_value sub = mrb_ary_new_capa(mrb, ncap - 1);
      for (int i = 1; i < ncap; i++) {
        if (captures[i * 2] >= 0) {
          mrb_ary_push(mrb, sub,
            mrb_str_substr(mrb, str, captures[i*2], captures[i*2+1] - captures[i*2]));
        }
        else {
          mrb_ary_push(mrb, sub, mrb_nil_value());
        }
      }
      mrb_ary_push(mrb, ary, sub);
    }

    int match_end = captures[1];
    if (match_end == pos) {
      pos++;
    }
    else {
      pos = match_end;
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  mrb_free(mrb, captures);

  if (last_ncap > 0) {
    create_matchdata(mrb, self, str, last_captures, last_ncap);
  }
  else {
    clear_match_globals(mrb);
  }

  return ary;
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

  /* Instance methods */
  mrb_define_method(mrb, re, "match", regexp_match, MRB_ARGS_ARG(1, 1));
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
  mrb_define_method(mrb, re, "__gsub_str", regexp_gsub_str, MRB_ARGS_REQ(2));
  mrb_define_method(mrb, re, "__sub_str", regexp_sub_str, MRB_ARGS_REQ(2));
  mrb_define_method(mrb, re, "__scan", regexp_scan, MRB_ARGS_REQ(1));

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
