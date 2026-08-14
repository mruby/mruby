#include <mruby.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/internal.h>

#define ENC_ASCII_8BIT "ASCII-8BIT"
#define ENC_BINARY     "BINARY"
#define ENC_UTF8       "UTF-8"

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
  return mrb_bool_value(mrb_str_valid_encoding_p(mrb, str));
}

static mrb_value
get_encoding(mrb_state *mrb, mrb_sym enc)
{
  struct RClass *e = mrb_module_get_id(mrb, MRB_SYM(Encoding));
  return mrb_const_get(mrb, mrb_obj_value(e), enc);
}

/*
 * call-seq:
 *   string.encoding -> encoding
 *
 * Returns the Encoding object that represents the encoding of the string.
 * In mruby, this returns either "UTF-8" or "ASCII-8BIT" (BINARY).
 *
 *   "hello".encoding          #=> "UTF-8"
 *   "\xff\xfe".encoding       #=> "ASCII-8BIT"
 */
static mrb_value
str_encoding(mrb_state *mrb, mrb_value self)
{
  struct RString *s = mrb_str_ptr(self);
  if (RSTR_BINARY_P(s)) {
    return get_encoding(mrb, MRB_SYM(BINARY));
  }
  return get_encoding(mrb, MRB_SYM(UTF_8));
}

/*
 * call-seq:
 *   string.force_encoding(encoding) -> string
 *
 * Changes the encoding of the string to the specified encoding.
 * This method modifies the string in place and returns self.
 * In mruby, only "UTF-8", "ASCII-8BIT", and "BINARY" are supported.
 *
 *   str = "hello"
 *   str.force_encoding("ASCII-8BIT")  #=> "hello"
 *   str.encoding                      #=> "ASCII-8BIT"
 */
static mrb_value
str_force_encoding(mrb_state *mrb, mrb_value self)
{
  mrb_value enc;

  mrb_get_args(mrb, "S", &enc);

  struct RString *s = mrb_str_ptr(self);
  if (MRB_STR_CASECMP_P(enc, ENC_ASCII_8BIT) ||
      MRB_STR_CASECMP_P(enc, ENC_BINARY)) {
    RSTR_ENCODING_SET(s, MRB_STR_ENCODING_BINARY);
  }
  else if (MRB_STR_CASECMP_P(enc, ENC_UTF8)) {
    RSTR_ENCODING_SET(s, MRB_STR_ENCODING_UTF8);
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
