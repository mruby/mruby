#include <limits.h>
#include <mruby.h>

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

/*
 *  call-seq:
 *     fix.odd?  ->  true or false
 *
 *  Returns +true+ if +fix+ is an odd number.
 */
static mrb_value
mrb_fix_odd_p(mrb_state *mrb, mrb_value x)
{
  if ((mrb_fixnum(x) & 1) == 0) {
    return mrb_false_value();
  }
  return mrb_true_value();
}

/*
 *  call-seq:
 *     fix.even?  ->  true or false
 *
 *  Returns +true+ if +fix+ is an even number.
 */
static mrb_value
mrb_fix_even_p(mrb_state *mrb, mrb_value x)
{
  if ((mrb_fixnum(x) & 1) == 0) {
    return mrb_true_value();
  }
  return mrb_false_value();
}

void
mrb_mruby_numeric_ext_gem_init(mrb_state* mrb)
{
  struct RClass *i = mrb_class_get(mrb, "Integer");
  struct RClass *f = mrb_class_get(mrb, "Fixnum");

  mrb_define_method(mrb, i, "chr", mrb_int_chr, MRB_ARGS_NONE());

  mrb_define_method(mrb, f, "odd?", mrb_fix_odd_p, MRB_ARGS_NONE());
  mrb_define_method(mrb, f, "even?", mrb_fix_even_p, MRB_ARGS_NONE());
}

void
mrb_mruby_numeric_ext_gem_final(mrb_state* mrb)
{
}
