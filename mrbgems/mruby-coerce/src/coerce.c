/*
** coerce.c - coerce protocol for Integer and Float operators
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/string.h>
#include <mruby/presym.h>
#include <mruby/internal.h>

/* `x op y` as `a op b`, where [a, b] is what `y.coerce(x)` answers; undef
   when `y` is a number mruby knows or does not answer coerce, for the
   operator to fail as it does without this gem */
mrb_value
mrb_num_coerce_bin(mrb_state *mrb, mrb_value x, mrb_value y, mrb_sym op)
{
  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
#endif
#ifdef MRB_USE_BIGINT
  case MRB_TT_BIGINT:
#endif
#ifdef MRB_USE_RATIONAL
  case MRB_TT_RATIONAL:
#endif
#ifdef MRB_USE_COMPLEX
  case MRB_TT_COMPLEX:
#endif
    return mrb_undef_value();
  default:
    break;
  }
  if (!mrb_respond_to(mrb, y, MRB_SYM(coerce))) return mrb_undef_value();

  mrb_value pair = mrb_funcall_argv(mrb, y, MRB_SYM(coerce), 1, &x);
  if (!mrb_array_p(pair) || RARRAY_LEN(pair) != 2) {
    mrb_raise(mrb, E_TYPE_ERROR, "coerce must return [x, y]");
  }
  mrb_value a = RARRAY_PTR(pair)[0];
  mrb_value b = RARRAY_PTR(pair)[1];
  return mrb_funcall_argv(mrb, a, op, 1, &b);
}

/*
 * call-seq:
 *   int.coerce(numeric)  ->  array
 *   float.coerce(numeric)  ->  array
 *
 * Returns `[numeric, self]` when `numeric` is of the receiver's class, and
 * both converted to Float otherwise.
 *
 *   1.coerce(2)      #=> [2, 1]
 *   1.coerce(2.5)    #=> [2.5, 1.0]
 *   1.5.coerce(2)    #=> [2.0, 1.5]
 */
static mrb_value
num_coerce(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);

  if (mrb_obj_class(mrb, x) == mrb_obj_class(mrb, y)) {
    return mrb_assoc_new(mrb, y, x);
  }
#ifdef MRB_NO_FLOAT
  mrb_raisef(mrb, E_TYPE_ERROR, "%T can't be coerced into %T", y, x);
#else
  if (mrb_string_p(y)) {
    y = mrb_float_value(mrb, mrb_str_to_dbl(mrb, y, TRUE));
  }
  return mrb_assoc_new(mrb, mrb_ensure_float_type(mrb, y), mrb_ensure_float_type(mrb, x));
#endif
}

void
mrb_mruby_coerce_gem_init(mrb_state *mrb)
{
  mrb_define_method_id(mrb, mrb->integer_class, MRB_SYM(coerce), num_coerce, MRB_ARGS_REQ(1));
#ifndef MRB_NO_FLOAT
  mrb_define_method_id(mrb, mrb->float_class, MRB_SYM(coerce), num_coerce, MRB_ARGS_REQ(1));
#endif
}

void
mrb_mruby_coerce_gem_final(mrb_state *mrb)
{
}
