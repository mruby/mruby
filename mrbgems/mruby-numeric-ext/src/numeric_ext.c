#include <limits.h>
#include <math.h>

#include "mruby.h"
#include "mruby/variable.h"

static mrb_value
mrb_int_chr(mrb_state *mrb, mrb_value x)
{
  mrb_int chr;
  char c;

  chr = mrb_fixnum(x);
  if (chr >= (1 << CHAR_BIT)) {
    mrb_raisef(mrb, E_RANGE_ERROR, "%S out of char range", x);
  }
  c = (char)chr;

  return mrb_str_new(mrb, &c, 1);
}

static mrb_value
flo_angle(mrb_state *mrb, mrb_value num)
{
  mrb_float value = mrb_float(num);

  if (isnan(value)) {
    return num;  /* Float::NAN */
  }
  if (value >= 0) {
    return mrb_fixnum_value(0);
  }
  return mrb_const_get(mrb,
      mrb_obj_value(mrb_module_get(mrb, "Math")),
      mrb_intern_lit(mrb, "PI"));
}

void
mrb_mruby_numeric_ext_gem_init(mrb_state* mrb)
{
  struct RClass *i = mrb_class_get(mrb, "Integer");
  struct RClass *f = mrb_class_get(mrb, "Float");

  mrb_define_method(mrb, i, "chr", mrb_int_chr, MRB_ARGS_NONE());

  mrb_define_method(mrb, f, "angle", flo_angle, MRB_ARGS_NONE());
}

void
mrb_mruby_numeric_ext_gem_final(mrb_state* mrb)
{
}
