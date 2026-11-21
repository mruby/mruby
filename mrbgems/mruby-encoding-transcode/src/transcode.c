/*
** transcode.c - String#encode and Encoding.default_external
**
** See Copyright Notice in mruby.h
**
** mruby-encoding's strings are UTF-8 or binary, and US-ASCII names the
** strings of ASCII bytes. String#encode converts between those three, as
** CRuby does: a character the target cannot hold is undefined, and a byte
** that spells no character in the source is invalid.
*/

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/error.h>
#include <mruby/hash.h>
#include <mruby/proc.h>
#include <mruby/variable.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#include <stdio.h>

enum tc_enc { TC_UTF8, TC_BINARY, TC_ASCII };

static const char *const tc_enc_names[] = { "UTF-8", "ASCII-8BIT", "US-ASCII" };

/* the encoding a name gives, or -1 for one this gem does not convert */
static int
tc_enc_find(mrb_value name)
{
  if (MRB_STR_CASECMP_P(name, "UTF-8")) return TC_UTF8;
  if (MRB_STR_CASECMP_P(name, "ASCII-8BIT") || MRB_STR_CASECMP_P(name, "BINARY")) return TC_BINARY;
  if (MRB_STR_CASECMP_P(name, "US-ASCII") || MRB_STR_CASECMP_P(name, "ASCII")) return TC_ASCII;
  return -1;
}

static int
tc_str_enc(mrb_value str)
{
  return RSTR_BINARY_P(mrb_str_ptr(str)) ? TC_BINARY : TC_UTF8;
}

static mrb_value
tc_enc_const(mrb_state *mrb, int enc)
{
  static const mrb_sym names[] = { MRB_SYM(UTF_8), MRB_SYM(ASCII_8BIT), MRB_SYM(US_ASCII) };
  struct RClass *e = mrb_module_get_id(mrb, MRB_SYM(Encoding));
  return mrb_const_get(mrb, mrb_obj_value(e), names[enc]);
}

static mrb_bool
tc_ascii_p(const char *p, mrb_int len)
{
  for (mrb_int i = 0; i < len; i++) {
    if ((unsigned char)p[i] >= 0x80) return FALSE;
  }
  return TRUE;
}

static mrb_noreturn void
tc_not_found(mrb_state *mrb, mrb_value from, mrb_value to)
{
  struct RClass *e = mrb_module_get_id(mrb, MRB_SYM(Encoding));
  mrb_raisef(mrb, mrb_class_get_under_id(mrb, e, MRB_SYM(ConverterNotFoundError)),
             "code converter not found (%v to %v)", from, to);
}

/*
 * Encoding.default_external and default_internal
 *
 * Their values are kept on the Encoding module, under names Ruby code
 * cannot reach. mruby-io reads text as UTF-8 whatever they say.
 */
static mrb_value
enc_default_external(mrb_state *mrb, mrb_value self)
{
  mrb_value v = mrb_iv_get(mrb, self, MRB_SYM(default_external));
  return mrb_nil_p(v) ? tc_enc_const(mrb, TC_UTF8) : v;
}

static mrb_value
enc_default_internal(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, MRB_SYM(default_internal));
}

static mrb_value
enc_check_name(mrb_state *mrb, mrb_value enc)
{
  mrb_value name = mrb_ensure_string_type(mrb, enc);
  int e = tc_enc_find(name);
  if (e < 0) mrb_raisef(mrb, E_ARGUMENT_ERROR, "unknown encoding name - %v", name);
  return tc_enc_const(mrb, e);
}

static mrb_value
enc_set_default_external(mrb_state *mrb, mrb_value self)
{
  mrb_value enc = mrb_get_arg1(mrb);
  if (mrb_nil_p(enc)) mrb_raise(mrb, E_ARGUMENT_ERROR, "default external can not be nil");
  enc = enc_check_name(mrb, enc);
  mrb_iv_set(mrb, self, MRB_SYM(default_external), enc);
  return enc;
}

static mrb_value
enc_set_default_internal(mrb_state *mrb, mrb_value self)
{
  mrb_value enc = mrb_get_arg1(mrb);
  if (!mrb_nil_p(enc)) enc = enc_check_name(mrb, enc);
  mrb_iv_set(mrb, self, MRB_SYM(default_internal), enc);
  return enc;
}

/*
 * The options String#encode takes
 */
enum { TC_XML_NONE, TC_XML_TEXT, TC_XML_ATTR };
enum { TC_NL_NONE, TC_NL_UNIVERSAL, TC_NL_CRLF, TC_NL_CR };

struct tc_opts {
  mrb_bool invalid_replace;
  mrb_bool undef_replace;
  mrb_value replace;            /* nil: U+FFFD for UTF-8, "?" otherwise */
  mrb_value fallback;           /* nil: none */
  int xml;
  int newline;
};

/* an option value that is not one the option takes, by name as CRuby
   prints it */
static mrb_noreturn void
tc_bad_value(mrb_state *mrb, const char *opt, mrb_value v)
{
  if (mrb_symbol_p(v)) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unexpected value for %s option: %n", opt, mrb_symbol(v));
  }
  mrb_raisef(mrb, E_ARGUMENT_ERROR, "unexpected value for %s option: %v", opt, v);
}

static mrb_value
tc_opt(mrb_state *mrb, mrb_value opts, mrb_sym name)
{
  if (!mrb_hash_p(opts)) return mrb_nil_value();
  return mrb_hash_get(mrb, opts, mrb_symbol_value(name));
}

/* a newline option given as true; more than one is no converter */
static void
tc_newline(mrb_state *mrb, struct tc_opts *o, mrb_value *names, int nl, const char *name)
{
  if (o->newline != TC_NL_NONE && o->newline != nl) {
    mrb_str_cat_lit(mrb, *names, ",");
    mrb_str_cat_cstr(mrb, *names, name);
    struct RClass *e = mrb_module_get_id(mrb, MRB_SYM(Encoding));
    mrb_raisef(mrb, mrb_class_get_under_id(mrb, e, MRB_SYM(ConverterNotFoundError)),
               "code converter not found (%v)", *names);
  }
  if (o->newline == TC_NL_NONE) mrb_str_cat_cstr(mrb, *names, name);
  o->newline = nl;
}

static void
tc_parse_opts(mrb_state *mrb, mrb_value opts, struct tc_opts *o)
{
  mrb_value v;

  o->invalid_replace = FALSE;
  o->undef_replace = FALSE;
  o->replace = mrb_nil_value();
  o->fallback = mrb_nil_value();
  o->xml = TC_XML_NONE;
  o->newline = TC_NL_NONE;
  if (!mrb_hash_p(opts)) return;

  v = tc_opt(mrb, opts, MRB_SYM(invalid));
  if (mrb_symbol_p(v) && mrb_symbol(v) == MRB_SYM(replace)) o->invalid_replace = TRUE;
  else if (!mrb_nil_p(v)) mrb_raise(mrb, E_ARGUMENT_ERROR, "unknown value for invalid character option");

  v = tc_opt(mrb, opts, MRB_SYM(undef));
  if (mrb_symbol_p(v) && mrb_symbol(v) == MRB_SYM(replace)) o->undef_replace = TRUE;
  else if (!mrb_nil_p(v)) mrb_raise(mrb, E_ARGUMENT_ERROR, "unknown value for undefined character option");

  /* replace: alone replaces undefined characters; read a frozen copy of it,
     which fallback: cannot change even when ObjectSpace hands it out */
  v = tc_opt(mrb, opts, MRB_SYM(replace));
  if (!mrb_nil_p(v)) {
    v = mrb_ensure_string_type(mrb, v);
    if (!mrb_str_valid_encoding_p(mrb, v)) {
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "replacement string is broken: %v as %s",
                 mrb_str_inspect(mrb, v), tc_enc_names[tc_str_enc(v)]);
    }
    o->replace = mrb_str_dup_frozen(mrb, v);
    if (!o->invalid_replace) o->undef_replace = TRUE;
  }

  o->fallback = tc_opt(mrb, opts, MRB_SYM(fallback));

  v = tc_opt(mrb, opts, MRB_SYM(xml));
  if (mrb_symbol_p(v) && mrb_symbol(v) == MRB_SYM(text)) o->xml = TC_XML_TEXT;
  else if (mrb_symbol_p(v) && mrb_symbol(v) == MRB_SYM(attr)) o->xml = TC_XML_ATTR;
  else if (!mrb_nil_p(v)) tc_bad_value(mrb, "xml", v);

  /* newline: names one decorator, and the flags name the others */
  mrb_value names = mrb_str_new_capa(mrb, 32);
  v = tc_opt(mrb, opts, MRB_SYM(newline));
  if (!mrb_nil_p(v)) {
    mrb_sym s = mrb_symbol_p(v) ? mrb_symbol(v) : 0;
    if (s == MRB_SYM(universal) || s == MRB_SYM(lf)) tc_newline(mrb, o, &names, TC_NL_UNIVERSAL, "universal_newline");
    else if (s == MRB_SYM(crlf)) tc_newline(mrb, o, &names, TC_NL_CRLF, "crlf_newline");
    else if (s == MRB_SYM(cr)) tc_newline(mrb, o, &names, TC_NL_CR, "cr_newline");
    else tc_bad_value(mrb, "newline", v);
  }
  if (mrb_test(tc_opt(mrb, opts, MRB_SYM(universal_newline)))) tc_newline(mrb, o, &names, TC_NL_UNIVERSAL, "universal_newline");
  if (mrb_test(tc_opt(mrb, opts, MRB_SYM(lf_newline)))) tc_newline(mrb, o, &names, TC_NL_UNIVERSAL, "lf_newline");
  if (mrb_test(tc_opt(mrb, opts, MRB_SYM(crlf_newline)))) tc_newline(mrb, o, &names, TC_NL_CRLF, "crlf_newline");
  if (mrb_test(tc_opt(mrb, opts, MRB_SYM(cr_newline)))) tc_newline(mrb, o, &names, TC_NL_CR, "cr_newline");
}

/*
 * The conversion
 */
struct tc_conv {
  int src, dst;
  struct tc_opts *opts;
  mrb_value out;
  mrb_value replace;            /* the replacement, checked against dst */
};

/* The byte length of the maximal subpart at p, the longest prefix that could
   still have grown into a well-formed sequence, as String#scrub counts it:
   "\xE0\x80\xAF" is three invalid bytes and a truncated "\xE3\x81" is one. */
static mrb_int
tc_subpart_len(const unsigned char *p, const unsigned char *e)
{
  unsigned char c = p[0], lo = 0x80, hi = 0xBF;
  mrb_int want;

  if (c < 0xC2 || 0xF4 < c) return 1;
  if (c < 0xE0)      want = 2;
  else if (c < 0xF0) want = 3;
  else               want = 4;
  switch (c) {
  case 0xE0: lo = 0xA0; break;
  case 0xED: hi = 0x9F; break;
  case 0xF0: lo = 0x90; break;
  case 0xF4: hi = 0x8F; break;
  }

  mrb_int n = 1;
  while (n < want && p + n < e) {
    unsigned char b = p[n];
    if (n == 1 ? (b < lo || hi < b) : (b < 0x80 || 0xBF < b)) break;
    n++;
  }
  return n;
}

static mrb_value
tc_bytes(mrb_state *mrb, const unsigned char *p, mrb_int len, int enc)
{
  mrb_value s = mrb_str_new(mrb, (const char*)p, len);
  if (enc != TC_UTF8) RSTR_ENCODING_SET(mrb_str_ptr(s), MRB_STR_ENCODING_BINARY);
  return s;
}

static mrb_noreturn void
tc_invalid(mrb_state *mrb, struct tc_conv *c, const unsigned char *p, mrb_int len, const unsigned char *e)
{
  mrb_value bytes = tc_bytes(mrb, p, len, TC_BINARY);
  mrb_value insp = mrb_str_inspect(mrb, bytes);
  const char *src = tc_enc_names[c->src];
  /* a sequence cut short by the end of the string, or by the byte after it */
  mrb_bool started = c->src == TC_UTF8 && 0xC2 <= p[0] && p[0] <= 0xF4;
  mrb_bool incomplete = started && p + len == e;
  mrb_value msg;

  if (incomplete) {
    msg = mrb_format(mrb, "incomplete %v on %s", insp, src);
  }
  else if (started) {
    mrb_value next = mrb_str_inspect(mrb, tc_bytes(mrb, p + len, 1, TC_BINARY));
    msg = mrb_format(mrb, "%v followed by %v on %s", insp, next, src);
  }
  else {
    msg = mrb_format(mrb, "%v on %s", insp, src);
  }

  struct RClass *enc = mrb_module_get_id(mrb, MRB_SYM(Encoding));
  mrb_value exc = mrb_exc_new_str(mrb, mrb_class_get_under_id(mrb, enc, MRB_SYM(InvalidByteSequenceError)), msg);
  mrb_iv_set(mrb, exc, MRB_IVSYM(error_bytes), bytes);
  mrb_iv_set(mrb, exc, MRB_IVSYM(source_encoding_name), mrb_str_new_cstr(mrb, src));
  mrb_iv_set(mrb, exc, MRB_IVSYM(destination_encoding_name), mrb_str_new_cstr(mrb, tc_enc_names[c->dst]));
  mrb_iv_set(mrb, exc, MRB_IVSYM(incomplete_input), mrb_bool_value(incomplete));
  mrb_exc_raise(mrb, exc);
}

static mrb_noreturn void
tc_undefined(mrb_state *mrb, struct tc_conv *c, mrb_value ch, uint32_t cp)
{
  const char *src = tc_enc_names[c->src];
  const char *dst = tc_enc_names[c->dst];
  mrb_value msg;

  if (c->src == TC_UTF8) {
    char buf[16];
    mrb_int n = snprintf(buf, sizeof(buf), "U+%04X", (unsigned int)cp);
    msg = mrb_format(mrb, "%v from %s to %s", mrb_str_new(mrb, buf, n), src, dst);
  }
  else {
    msg = mrb_format(mrb, "%v from %s to %s", mrb_str_inspect(mrb, ch), src, dst);
  }

  struct RClass *enc = mrb_module_get_id(mrb, MRB_SYM(Encoding));
  mrb_value exc = mrb_exc_new_str(mrb, mrb_class_get_under_id(mrb, enc, MRB_SYM(UndefinedConversionError)), msg);
  mrb_iv_set(mrb, exc, MRB_IVSYM(error_char), ch);
  mrb_iv_set(mrb, exc, MRB_IVSYM(source_encoding_name), mrb_str_new_cstr(mrb, src));
  mrb_iv_set(mrb, exc, MRB_IVSYM(destination_encoding_name), mrb_str_new_cstr(mrb, dst));
  mrb_exc_raise(mrb, exc);
}

/* what fallback: gives for an undefined character, or nil */
static mrb_value
tc_fallback(mrb_state *mrb, struct tc_conv *c, mrb_value ch)
{
  mrb_value fb = c->opts->fallback;
  mrb_value v;

  if (mrb_nil_p(fb)) return mrb_nil_value();
  if (mrb_proc_p(fb) ||
      (mrb_class_defined_id(mrb, MRB_SYM(Method)) &&
       mrb_obj_is_kind_of(mrb, fb, mrb_class_get_id(mrb, MRB_SYM(Method))))) {
    v = mrb_funcall_id(mrb, fb, MRB_SYM(call), 1, ch);
  }
  else {
    v = mrb_funcall_id(mrb, fb, MRB_OPSYM(aref), 1, ch);
  }
  if (mrb_nil_p(v)) return v;
  v = mrb_ensure_string_type(mrb, v);
  /* beyond ASCII, only a string in the target's own encoding goes in as it
     is; CRuby says the same of one it cannot convert */
  if (tc_str_enc(v) != c->dst && !tc_ascii_p(RSTRING_PTR(v), RSTRING_LEN(v))) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "too big fallback string");
  }
  return v;
}

/* a character the target cannot hold: `ch` in the source's encoding, with
   its codepoint when the source is UTF-8 */
static void
tc_undef(mrb_state *mrb, struct tc_conv *c, mrb_value ch, uint32_t cp)
{
  if (c->opts->xml != TC_XML_NONE && c->src == TC_UTF8) {
    char buf[16];
    mrb_int n = snprintf(buf, sizeof(buf), "&#x%X;", (unsigned int)cp);
    mrb_str_cat(mrb, c->out, buf, n);
    return;
  }
  if (c->opts->undef_replace) {
    mrb_str_cat_str(mrb, c->out, c->replace);
    return;
  }
  mrb_value v = tc_fallback(mrb, c, ch);
  if (!mrb_nil_p(v)) {
    mrb_str_cat_str(mrb, c->out, v);
    return;
  }
  tc_undefined(mrb, c, ch, cp);
}

static void
tc_newline_out(mrb_state *mrb, struct tc_conv *c)
{
  switch (c->opts->newline) {
  case TC_NL_CRLF: mrb_str_cat_lit(mrb, c->out, "\r\n"); break;
  case TC_NL_CR:   mrb_str_cat_lit(mrb, c->out, "\r"); break;
  default:         mrb_str_cat_lit(mrb, c->out, "\n"); break;
  }
}

static void
tc_ascii_out(mrb_state *mrb, struct tc_conv *c, unsigned char b)
{
  if (c->opts->xml != TC_XML_NONE) {
    switch (b) {
    case '&': mrb_str_cat_lit(mrb, c->out, "&amp;"); return;
    case '<': mrb_str_cat_lit(mrb, c->out, "&lt;"); return;
    case '>': mrb_str_cat_lit(mrb, c->out, "&gt;"); return;
    case '"':
      if (c->opts->xml == TC_XML_ATTR) {
        mrb_str_cat_lit(mrb, c->out, "&quot;");
        return;
      }
      break;
    }
  }
  char ch = (char)b;
  mrb_str_cat(mrb, c->out, &ch, 1);
}

static mrb_value
tc_convert(mrb_state *mrb, mrb_value str, int src, int dst, struct tc_opts *opts)
{
  struct tc_conv c;

  /* fallback: runs Ruby code, which can change str under the loop: read a
     frozen copy of it, which shares the bytes until str changes and which
     the fallback cannot change even when ObjectSpace hands it out */
  if (!mrb_nil_p(opts->fallback) && !opts->undef_replace) str = mrb_str_dup_frozen(mrb, str);

  const unsigned char *p = (const unsigned char*)RSTRING_PTR(str);
  const unsigned char *e = p + RSTRING_LEN(str);

  c.src = src;
  c.dst = dst;
  c.opts = opts;
  c.out = mrb_str_new_capa(mrb, RSTRING_LEN(str));
  if (dst != TC_UTF8) RSTR_ENCODING_SET(mrb_str_ptr(c.out), MRB_STR_ENCODING_BINARY);

  /* the replacement has to be something the target holds */
  if (mrb_nil_p(opts->replace)) {
    c.replace = dst == TC_UTF8 ? mrb_str_new_lit(mrb, "\xEF\xBF\xBD") : mrb_str_new_lit(mrb, "?");
  }
  else {
    c.replace = opts->replace;
    int renc = tc_str_enc(c.replace);
    if (renc != dst && !tc_ascii_p(RSTRING_PTR(c.replace), RSTRING_LEN(c.replace))) {
      tc_not_found(mrb, mrb_str_new_cstr(mrb, tc_enc_names[renc]), mrb_str_new_cstr(mrb, tc_enc_names[dst]));
    }
  }

  /* an invalid byte is left as it is when nothing converts it */
  mrb_bool keep_invalid = src == dst && !opts->invalid_replace;
  if (opts->xml == TC_XML_ATTR) mrb_str_cat_lit(mrb, c.out, "\"");
  int ai = mrb_gc_arena_save(mrb);

  while (p < e) {
    unsigned char b = *p;

    if (b == '\r' && opts->newline == TC_NL_UNIVERSAL) {
      p += (p + 1 < e && p[1] == '\n') ? 2 : 1;
      tc_newline_out(mrb, &c);
      continue;
    }
    if (b == '\n') {
      tc_newline_out(mrb, &c);
      p++;
      continue;
    }
    if (b < 0x80) {
      tc_ascii_out(mrb, &c, b);
      p++;
      continue;
    }

    mrb_int len = 1;
    if (src == TC_UTF8) {
      uint32_t cp = mrb_utf8_decode((const char*)p, (const char*)e, &len);
      if (len == 1) {
        /* a byte at 0x80 or above that spells no character */
        len = tc_subpart_len(p, e);
        goto invalid;
      }
      if (dst == TC_UTF8) mrb_str_cat(mrb, c.out, (const char*)p, len);
      else tc_undef(mrb, &c, tc_bytes(mrb, p, len, TC_UTF8), cp);
    }
    else if (src == TC_BINARY) {
      if (dst == TC_BINARY) mrb_str_cat(mrb, c.out, (const char*)p, 1);
      else tc_undef(mrb, &c, tc_bytes(mrb, p, 1, TC_BINARY), b);
    }
    else {
      goto invalid;
    }
    p += len;
    mrb_gc_arena_restore(mrb, ai);
    continue;

  invalid:
    if (keep_invalid) mrb_str_cat(mrb, c.out, (const char*)p, len);
    else if (opts->invalid_replace) mrb_str_cat_str(mrb, c.out, c.replace);
    else tc_invalid(mrb, &c, p, len, e);
    p += len;
    mrb_gc_arena_restore(mrb, ai);
  }
  if (opts->xml == TC_XML_ATTR) mrb_str_cat_lit(mrb, c.out, "\"");
  return c.out;
}

/* the conversion `str.encode(*args, **opts)` asks for, or nil for none;
   *dstp is the target either way */
static mrb_value
str_encode_out(mrb_state *mrb, mrb_value str, int *dstp)
{
  mrb_value to = mrb_undef_value(), from = mrb_undef_value(), opts = mrb_nil_value();
  const mrb_kwargs kw = { 0, 0, NULL, NULL, &opts };
  struct tc_opts o;
  int src, dst;

  mrb_get_args(mrb, "|oo:", &to, &from, &kw);
  tc_parse_opts(mrb, opts, &o);

  if (mrb_undef_p(from)) {
    src = tc_str_enc(str);
  }
  else {
    from = mrb_ensure_string_type(mrb, from);
    src = tc_enc_find(from);
  }
  if (mrb_undef_p(to)) {
    struct RClass *e = mrb_module_get_id(mrb, MRB_SYM(Encoding));
    to = mrb_iv_get(mrb, mrb_obj_value(e), MRB_SYM(default_internal));
    if (mrb_nil_p(to)) to = tc_enc_const(mrb, src < 0 ? tc_str_enc(str) : src);
  }
  to = mrb_ensure_string_type(mrb, to);
  dst = tc_enc_find(to);
  if (src < 0 || dst < 0) {
    tc_not_found(mrb, src < 0 ? from : mrb_str_new_cstr(mrb, tc_enc_names[src]), to);
  }
  *dstp = dst;

  /* nothing to convert, and nothing to decorate */
  if (src == dst && !o.invalid_replace && o.xml == TC_XML_NONE && o.newline == TC_NL_NONE) {
    return mrb_nil_value();
  }
  return tc_convert(mrb, str, src, dst, &o);
}

/* tags s as dst, as tc_convert tags its result */
static void
tc_set_enc(mrb_value s, int dst)
{
  RSTR_ENCODING_SET(mrb_str_ptr(s), dst == TC_UTF8 ? MRB_STR_ENCODING_UTF8 : MRB_STR_ENCODING_BINARY);
}

/*
 * call-seq:
 *   str.encode(dst = Encoding.default_internal, src = str.encoding, **opts) -> new_str
 *
 * Returns a copy of str converted from src to dst, either of which is
 * UTF-8, ASCII-8BIT (BINARY) or US-ASCII. A result in US-ASCII is tagged
 * ASCII-8BIT, as mruby tags every string that is not UTF-8. With no dst and
 * no Encoding.default_internal, str is not converted, but the options still
 * apply.
 *
 * A character dst cannot hold raises Encoding::UndefinedConversionError,
 * and a byte that spells no character in src raises
 * Encoding::InvalidByteSequenceError, unless the options say otherwise:
 *
 *   invalid: :replace        replaces an invalid byte
 *   undef: :replace          replaces an undefined character
 *   replace: str             the replacement, U+FFFD for UTF-8 and "?"
 *                            otherwise
 *   fallback: obj            gives the replacement of each undefined
 *                            character, by obj[ch] or obj.call(ch)
 *   xml: :text or :attr      escapes &, < and > (and " for :attr, which is
 *                            quoted), and spells an undefined character
 *                            as &#xHH;
 *   newline: :universal, :crlf, :cr or :lf, or
 *   universal_newline:, crlf_newline:, cr_newline:, lf_newline: true
 *                            converts newlines
 *
 *   "aあ".encode("US-ASCII", undef: :replace)  #=> "a?"
 */
static mrb_value
str_encode(mrb_state *mrb, mrb_value self)
{
  int dst;
  mrb_value out = str_encode_out(mrb, self, &dst);
  if (mrb_nil_p(out)) {
    /* src may be given, and differ from how self is tagged */
    out = mrb_str_dup(mrb, self);
    tc_set_enc(out, dst);
  }
  return out;
}

/*
 * call-seq:
 *   str.encode!(dst = Encoding.default_internal, src = str.encoding, **opts) -> str
 *
 * Converts str in place, as String#encode converts a copy.
 */
static mrb_value
str_encode_bang(mrb_state *mrb, mrb_value self)
{
  mrb_check_frozen(mrb, mrb_obj_ptr(self));
  int dst;
  mrb_value out = str_encode_out(mrb, self, &dst);
  if (!mrb_nil_p(out)) mrb_funcall_id(mrb, self, MRB_SYM(replace), 1, out);
  tc_set_enc(self, dst);
  return self;
}

void
mrb_mruby_encoding_transcode_gem_init(mrb_state *mrb)
{
  struct RClass *e = mrb_module_get_id(mrb, MRB_SYM(Encoding));

  mrb_define_class_method_id(mrb, e, MRB_SYM(default_external), enc_default_external, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, e, MRB_SYM_E(default_external), enc_set_default_external, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, e, MRB_SYM(default_internal), enc_default_internal, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, e, MRB_SYM_E(default_internal), enc_set_default_internal, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, mrb->string_class, MRB_SYM(encode), str_encode, MRB_ARGS_OPT(2)|MRB_ARGS_KEY(0, 1));
  mrb_define_method_id(mrb, mrb->string_class, MRB_SYM_B(encode), str_encode_bang, MRB_ARGS_OPT(2)|MRB_ARGS_KEY(0, 1));
}

void
mrb_mruby_encoding_transcode_gem_final(mrb_state *mrb)
{
}
