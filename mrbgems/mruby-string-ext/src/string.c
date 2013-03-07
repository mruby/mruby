#include "mruby.h"
#include "mruby/string.h"

static mrb_value
mrb_str_getbyte(mrb_state *mrb, mrb_value str)
{
  mrb_int pos;
  mrb_get_args(mrb, "i", &pos);

  if (pos < 0)
    pos += RSTRING_LEN(str);
  if (pos < 0 ||  RSTRING_LEN(str) <= pos)
    return mrb_nil_value();

  return mrb_fixnum_value((unsigned char)RSTRING_PTR(str)[pos]);
}


void
mrb_mruby_string_ext_gem_init(mrb_state* mrb)
{
  struct RClass * s = mrb->string_class;

  mrb_define_method(mrb, s, "dump",            mrb_str_dump,            ARGS_NONE());
  mrb_define_method(mrb, s, "getbyte",         mrb_str_getbyte,         ARGS_REQ(1));
}

void
mrb_mruby_string_ext_gem_final(mrb_state* mrb)
{
}
