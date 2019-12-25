#include <mruby.h>
#include <mruby/string.h>
#include <mruby/variable.h>

static mrb_value
enc_ascii_compatible_p(mrb_state *mrb, mrb_value self) {
  return mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "ascii_compatible"));
}

static mrb_value
enc_name(mrb_state *mrb, mrb_value self) {
  return mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "name"));
}

static mrb_value
enc_inspect(mrb_state *mrb, mrb_value self) {
  return mrb_str_cat_lit(mrb, mrb_str_cat_str(mrb, mrb_str_new_lit(mrb, "#<Encoding:"), enc_name(mrb, self)), ">");
}

static inline mrb_value
get_bin_enc(mrb_state *mrb)
{
  return mrb_const_get(mrb, mrb_obj_value(mrb_class_get(mrb, "Encoding")), mrb_intern_lit(mrb, "ASCII_8BIT"));
}

static inline mrb_value
get_utf8_enc(mrb_state *mrb)
{
  return mrb_const_get(mrb, mrb_obj_value(mrb_class_get(mrb, "Encoding")), mrb_intern_lit(mrb, "UTF_8"));
}

static mrb_value
str_encoding(mrb_state *mrb, mrb_value self) {
  if (RSTR_ASCII_P(RSTRING(self))) {
    return get_bin_enc(mrb);
  } else {
    return get_utf8_enc(mrb);
  }
}

static mrb_value
str_force_encoding(mrb_state *mrb, mrb_value self) {
  mrb_value ret, enc;

  mrb_get_args(mrb, "o", &enc);
  if (!mrb_obj_is_kind_of(mrb, enc, mrb_class_get(mrb, "Encoding"))) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "Expected Encoding object: %S", enc);
  }

  ret = mrb_obj_dup(mrb, self);
  mrb_assert(mrb_string_p(ret));
  RSTR_SET_ASCII_FLAG(RSTRING(ret));
  if (mrb_obj_ptr(enc) != mrb_obj_ptr(get_bin_enc(mrb))) {
    RSTR_UNSET_ASCII_FLAG(RSTRING(ret));
  }
  return ret;
}

void
mrb_mruby_encoding_gem_init(mrb_state *mrb)
{
  struct RClass* encoding;
  struct RBasic *ascii_8bit, *utf8;

  encoding = mrb_define_class(mrb, "Encoding", mrb->object_class);
  mrb_undef_class_method(mrb, encoding, "new");
  mrb_undef_class_method(mrb, encoding, "dup");

  mrb_define_method(mrb, encoding, "ascii_compatible?", enc_ascii_compatible_p, MRB_ARGS_NONE());
  mrb_define_method(mrb, encoding, "name", enc_name, MRB_ARGS_NONE());
  mrb_define_method(mrb, encoding, "to_s", enc_name, MRB_ARGS_NONE());
  mrb_define_method(mrb, encoding, "inspect", enc_inspect, MRB_ARGS_NONE());

  ascii_8bit = mrb_obj_alloc(mrb, MRB_TT_OBJECT, encoding);
  utf8 = mrb_obj_alloc(mrb, MRB_TT_OBJECT, encoding);

  mrb_iv_set(mrb, mrb_obj_value(ascii_8bit), mrb_intern_lit(mrb, "ascii_compatible"), mrb_true_value());
  mrb_iv_set(mrb, mrb_obj_value(ascii_8bit), mrb_intern_lit(mrb, "name"), mrb_str_new_lit(mrb, "ASCII-8BIT"));

  mrb_iv_set(mrb, mrb_obj_value(utf8), mrb_intern_lit(mrb, "ascii_compatible"), mrb_true_value());
  mrb_iv_set(mrb, mrb_obj_value(utf8), mrb_intern_lit(mrb, "name"), mrb_str_new_lit(mrb, "UTF-8"));

  mrb_obj_freeze(mrb, mrb_obj_value(ascii_8bit));
  mrb_obj_freeze(mrb, mrb_obj_value(utf8));

  mrb_define_const(mrb, encoding, "ASCII_8BIT", mrb_obj_value(ascii_8bit));
  mrb_define_const(mrb, encoding, "UTF_8", mrb_obj_value(utf8));

  mrb_define_method(mrb, mrb->string_class, "encoding", str_encoding, MRB_ARGS_NONE());
  mrb_define_method(mrb, mrb->string_class, "force_encoding", str_force_encoding, MRB_ARGS_REQ(1));
}

void
mrb_mruby_encoding_gem_final(mrb_state *mrb)
{
}
