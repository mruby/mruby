#include <mruby.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#define ENC_ASCII_8BIT "ASCII-8BIT"
#define ENC_BINARY     "BINARY"
#define ENC_UTF8       "UTF-8"

#define ENC_COMP_P(enc, enc_lit) \
  casecmp_p(RSTRING_PTR(enc), RSTRING_LEN(enc), enc_lit, sizeof(enc_lit"")-1)

static mrb_bool
casecmp_p(const char *s1, mrb_int len1, const char *s2, mrb_int len2)
{
  if (len1 != len2) return FALSE;

  const char *e1 = s1 + len1;
  const char *e2 = s2 + len2;
  while (s1 < e1 && s2 < e2) {
    if (*s1 != *s2 && TOUPPER(*s1) != TOUPPER(*s2)) return FALSE;
    s1++;
    s2++;
  }
  return TRUE;
}

/*
 * call-seq:
 *   string.valid_encoding? -> true or false
 *
 * Returns true for a string which is encoded correctly.
 *
 */
static mrb_value
str_valid_enc_p(mrb_state *mrb, mrb_value str)
{
#define utf8_islead(c) ((unsigned char)((c)&0xc0) != 0x80)

  struct RString *s = mrb_str_ptr(str);
  if (RSTR_SINGLE_BYTE_P(s)) return mrb_true_value();
  if (RSTR_BINARY_P(s)) return mrb_true_value();

  mrb_int byte_len = RSTR_LEN(s);
  mrb_int utf8_len = 0;
  const char *p = RSTR_PTR(s);
  const char *e = p + byte_len;
  while (p < e) {
    mrb_int len = mrb_utf8len(p, e);

    if (len == 1 && (*p & 0x80)) return mrb_false_value();
    p += len;
    utf8_len++;
  }
  if (byte_len == utf8_len) RSTR_SET_SINGLE_BYTE_FLAG(s);
  return mrb_true_value();
}

static mrb_value
get_encoding(mrb_state *mrb, mrb_sym enc)
{
  struct RClass *e = mrb_module_get_id(mrb, MRB_SYM(Encoding));
  return mrb_const_get(mrb, mrb_obj_value(e), enc);
}

static mrb_value
str_encoding(mrb_state *mrb, mrb_value self)
{
  struct RString *s = mrb_str_ptr(self);
  if (RSTR_BINARY_P(s)) {
    return get_encoding(mrb, MRB_SYM(BINARY));
  }
  return get_encoding(mrb, MRB_SYM(UTF_8));
}

static mrb_value
str_force_encoding(mrb_state *mrb, mrb_value self)
{
  mrb_value enc;

  mrb_get_args(mrb, "S", &enc);

  struct RString *s = mrb_str_ptr(self);
  if (ENC_COMP_P(enc, ENC_ASCII_8BIT) ||
      ENC_COMP_P(enc, ENC_BINARY)) {
    s->flags |= MRB_STR_BINARY;
  }
  else if (ENC_COMP_P(enc, ENC_UTF8)) {
    s->flags &= ~MRB_STR_BINARY;
  }
  else {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unknown encoding name - %v", enc);
  }
  return self;
}

void
mrb_mruby_encoding_gem_init(mrb_state* mrb)
{
  struct RClass *s = mrb->string_class;

  mrb_define_method_id(mrb, s, MRB_SYM_Q(valid_encoding), str_valid_enc_p,     MRB_ARGS_NONE());
  mrb_define_method_id(mrb, s, MRB_SYM(encoding),         str_encoding,        MRB_ARGS_NONE());
  mrb_define_method_id(mrb, s, MRB_SYM(force_encoding),   str_force_encoding,  MRB_ARGS_REQ(1));

  /* Poorman's Encoding
   *
   * Encoding - module instead of class
   * encodings - supports only UTF-8 and ASCII-8BIT (and its alias BINARY)
   * each Encoding - encoding name string instead of Encoding object
   *
   */
  struct RClass *e = mrb_define_module_id(mrb, MRB_SYM(Encoding));
  mrb_value b = mrb_str_new_lit_frozen(mrb, ENC_ASCII_8BIT);
  mrb_define_const_id(mrb, e, MRB_SYM(ASCII_8BIT), b);
  mrb_define_const_id(mrb, e, MRB_SYM(BINARY), b);
  mrb_define_const_id(mrb, e, MRB_SYM(UTF_8), mrb_str_new_lit_frozen(mrb, ENC_UTF8));
}

void
mrb_mruby_encoding_gem_final(mrb_state* mrb)
{
}
