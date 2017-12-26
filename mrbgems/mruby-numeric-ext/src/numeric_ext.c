#include <limits.h>
#include <mruby.h>

static inline mrb_int
to_int(mrb_value x)
{
  double f;

  if (mrb_fixnum_p(x)) return mrb_fixnum(x);
  f = mrb_float(x);
  return (mrb_int)f;
}

/*
 *  Document-method: Integer#chr
 *  call-seq:
 *     int.chr  ->  string
 *
 *  Returns a string containing the character represented by the +int+'s value
 *  according to +encoding+.
 *
 *     65.chr    #=> "A"
 *     230.chr   #=> "\xE6"
 */
static mrb_value
mrb_int_chr(mrb_state *mrb, mrb_value x)
{
  mrb_int chr;
  char c;

  chr = to_int(x);
  if (chr >= (1 << CHAR_BIT)) {
    mrb_raisef(mrb, E_RANGE_ERROR, "%S out of char range", x);
  }
  c = (char)chr;

  return mrb_str_new(mrb, &c, 1);
}

void
mrb_mruby_numeric_ext_gem_init(mrb_state* mrb)
{
  struct RClass *i = mrb_module_get(mrb, "Integral");

  mrb_define_method(mrb, i, "chr", mrb_int_chr, MRB_ARGS_NONE());
}

void
mrb_mruby_numeric_ext_gem_final(mrb_state* mrb)
{
}
