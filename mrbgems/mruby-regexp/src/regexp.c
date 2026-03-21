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

static void
clear_match_globals(mrb_state *mrb)
{
  static const char *nth_names[] = {
    "$1","$2","$3","$4","$5","$6","$7","$8","$9"
  };
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$~"), mrb_nil_value());
  for (int i = 0; i < 9; i++) {
    mrb_gv_set(mrb, mrb_intern_cstr(mrb, nth_names[i]), mrb_nil_value());
  }
}

/* Create MatchData from captures */
static mrb_value
create_matchdata(mrb_state *mrb, mrb_value regexp, mrb_value str, int *captures, int ncap)
{
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
  {
    static const char *nth_names[] = {
      "$1","$2","$3","$4","$5","$6","$7","$8","$9"
    };
    for (int i = 0; i < 9; i++) {
      mrb_value val = mrb_nil_value();
      int g = i + 1;
      if (g < md->num_captures && captures[g*2] >= 0) {
        val = mrb_str_substr(mrb, str, captures[g*2], captures[g*2+1] - captures[g*2]);
      }
      mrb_gv_set(mrb, mrb_intern_cstr(mrb, nth_names[i]), val);
    }
  }

  return obj;
}

/*
 * Regexp#match(str, pos=0)
 */
static mrb_value
regexp_match(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_int pos = 0;
  mrb_regexp_pattern *pat;

  mrb_get_args(mrb, "S|i", &str, &pos);
  pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  int captures[RE_MAX_CAPTURES * 2];
  memset(captures, -1, sizeof(captures));
  int ncap = re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), pos,
                     captures, pat->num_captures * 2);

  if (ncap == 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }

  return create_matchdata(mrb, self, str, captures, pat->num_captures * 2);
}

/*
 * Regexp#match?(str, pos=0)
 */
static mrb_value
regexp_match_p(mrb_state *mrb, mrb_value self)
{
  mrb_value str;
  mrb_int pos = 0;
  mrb_regexp_pattern *pat;

  mrb_get_args(mrb, "S|i", &str, &pos);
  pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
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
  mrb_regexp_pattern *pat;

  mrb_get_args(mrb, "o", &str);
  if (mrb_nil_p(str)) return mrb_nil_value();
  mrb_ensure_string_type(mrb, str);

  pat = DATA_GET_PTR(mrb, self, &regexp_type, mrb_regexp_pattern);
  if (!pat) mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized Regexp");

  int captures[RE_MAX_CAPTURES * 2];
  memset(captures, -1, sizeof(captures));
  int ncap = re_exec(mrb, pat, RSTRING_PTR(str), RSTRING_LEN(str), 0,
                     captures, pat->num_captures * 2);

  if (ncap == 0) {
    clear_match_globals(mrb);
    return mrb_nil_value();
  }
  create_matchdata(mrb, self, str, captures, pat->num_captures * 2);
  return mrb_int_value(mrb, captures[0]);
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
  mrb_value flags_val = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@flags"));
  uint32_t iflags = mrb_nil_p(flags_val) ? 0 : (uint32_t)mrb_integer(flags_val);
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
  mrb_value flags_val = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@flags"));
  uint32_t iflags = mrb_nil_p(flags_val) ? 0 : (uint32_t)mrb_integer(flags_val);
  return mrb_bool_value((iflags & RE_FLAG_IGNORECASE) != 0);
}

/*
 * Regexp#to_s - CRuby-compatible (?flags:source) format
 */
static mrb_value
regexp_to_s(mrb_state *mrb, mrb_value self)
{
  mrb_value src = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@source"));
  mrb_value flags_val = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@flags"));
  uint32_t flags = mrb_nil_p(flags_val) ? 0 : (uint32_t)mrb_integer(flags_val);

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
  mrb_value flags_val = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@flags"));
  uint32_t flags = mrb_nil_p(flags_val) ? 0 : (uint32_t)mrb_integer(flags_val);

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
  mrb_value f1 = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@flags"));
  mrb_value f2 = mrb_iv_get(mrb, other, mrb_intern_lit(mrb, "@flags"));
  mrb_int flags1 = mrb_nil_p(f1) ? 0 : mrb_integer(f1);
  mrb_int flags2 = mrb_nil_p(f2) ? 0 : mrb_integer(f2);
  return mrb_bool_value(flags1 == flags2);
}

/*
 * Regexp#hash
 */
static mrb_value
regexp_hash(mrb_state *mrb, mrb_value self)
{
  mrb_value src = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@source"));
  mrb_value flags_val = mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "@flags"));
  uint32_t flags = mrb_nil_p(flags_val) ? 0 : (uint32_t)mrb_integer(flags_val);
  uint32_t h = mrb_str_hash(mrb, src);
  h ^= flags * 0x9e3779b9;  /* mix flags into hash */
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

/*
 * MatchData#captures
 */
static mrb_value
matchdata_captures(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_ary_new(mrb);

  mrb_value ary = mrb_ary_new_capa(mrb, md->num_captures - 1);
  for (int i = 1; i < md->num_captures; i++) {
    int start = md->captures[i * 2];
    int end = md->captures[i * 2 + 1];
    if (start < 0) {
      mrb_ary_push(mrb, ary, mrb_nil_value());
    }
    else {
      mrb_ary_push(mrb, ary, mrb_str_substr(mrb, md->source, start, end - start));
    }
  }
  return ary;
}

/*
 * MatchData#to_a
 */
static mrb_value
matchdata_to_a(mrb_state *mrb, mrb_value self)
{
  mrb_match_data *md = DATA_GET_PTR(mrb, self, &matchdata_type, mrb_match_data);
  if (!md) return mrb_ary_new(mrb);

  mrb_value ary = mrb_ary_new_capa(mrb, md->num_captures);
  for (int i = 0; i < md->num_captures; i++) {
    int start = md->captures[i * 2];
    int end = md->captures[i * 2 + 1];
    if (start < 0) {
      mrb_ary_push(mrb, ary, mrb_nil_value());
    }
    else {
      mrb_ary_push(mrb, ary, mrb_str_substr(mrb, md->source, start, end - start));
    }
  }
  return ary;
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
}

void
mrb_mruby_regexp_gem_final(mrb_state *mrb)
{
}
