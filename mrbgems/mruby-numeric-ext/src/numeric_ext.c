#include <mruby.h>
#include <mruby/numeric.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#ifndef MRB_NO_FLOAT
static mrb_value flo_remainder(mrb_state *mrb, mrb_value self);
#endif

/*
 *  call-seq:
 *     num.remainder(numeric)  ->  real
 *
 *  <code>x.remainder(y)</code> means <code>x-y*(x/y).truncate</code>.
 *
 *  See Numeric#divmod.
 */
static mrb_value
int_remainder(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  mrb_int a, b;

#ifdef MRB_USE_BIGINT
  if (mrb_bigint_p(x)) {
    if (mrb_integer_p(y) || mrb_bigint_p(y)) {
      return mrb_bint_rem(mrb, x, y);
    }
    return flo_remainder(mrb, mrb_float_value(mrb, mrb_as_float(mrb, x)));
  }
#endif
  a = mrb_integer(x);
  if (mrb_integer_p(y)) {
    b = mrb_integer(y);
    if (b == 0) mrb_int_zerodiv(mrb);
    if (a == MRB_INT_MIN && b == -1) return mrb_fixnum_value(0);
    return mrb_int_value(mrb, a % b);
  }
#ifdef MRB_NO_FLOAT
  mrb_raise(mrb, E_TYPE_ERROR, "non integer remainder");
#else
  return flo_remainder(mrb, mrb_float_value(mrb, mrb_as_float(mrb, x)));
#endif
}

mrb_value mrb_int_pow(mrb_state *mrb, mrb_value x, mrb_value y);

/*
 * call-seq:
 *    integer.pow(numeric)           ->  numeric
 *    integer.pow(integer, integer)  ->  integer
 *
 * Returns (modular) exponentiation as:
 *
 *   a.pow(b)     #=> same as a**b
 *   a.pow(b, m)  #=> same as (a**b) % m, but avoids huge temporary values
 */
static mrb_value
int_powm(mrb_state *mrb, mrb_value x)
{
  mrb_value m;
  mrb_int e, exp, mod, result = 1;

  if (mrb_get_argc(mrb) == 1) {
    return mrb_int_pow(mrb, x, mrb_get_arg1(mrb));
  }
  mrb_get_args(mrb, "io", &e, &m);
  if (e < 0) mrb_raise(mrb, E_ARGUMENT_ERROR, "int.pow(n,m): n must be positive");
#ifdef MRB_USE_BIGINT
  if (mrb_bigint_p(x)) {
    return mrb_bint_powm(mrb, x, e, m);
  }
  if (mrb_bigint_p(m)) {
    return mrb_bint_powm(mrb, mrb_bint_new_int(mrb, mrb_integer(x)), e, m);
  }
#endif
  if (!mrb_integer_p(m)) mrb_raise(mrb, E_TYPE_ERROR, "int.pow(n,m): m must be integer");
  mod = mrb_integer(m);
  if (mod < 0) mrb_raise(mrb, E_ARGUMENT_ERROR, "int.pow(n,m): m must be positive when 2nd argument specified");
  if (mod == 0) mrb_int_zerodiv(mrb);
  if (mod == 1) return mrb_fixnum_value(0);
  mrb_int base = mrb_integer(x);
  exp = e;
  for (;;) {
    mrb_int tmp;
    if (exp & 1) {
      if (mrb_int_mul_overflow(result, base, &tmp)) {
        result %= mod; base %= mod;
        if (mrb_int_mul_overflow(result, base, &tmp)) {
#ifdef MRB_USE_BIGINT
          return mrb_bint_powm(mrb, mrb_bint_new_int(mrb, mrb_integer(x)), e, m);
#else
          mrb_int_overflow(mrb, "pow");
#endif
        }
      }
      result = tmp % mod;
    }
    exp >>= 1;
    if (exp == 0) break;
    if (mrb_int_mul_overflow(base, base, &tmp)) {
      base %= mod;
      if (mrb_int_mul_overflow(base, base, &tmp)) {
#ifdef MRB_USE_BIGINT
        return mrb_bint_powm(mrb, mrb_bint_new_int(mrb, mrb_integer(x)), e, m);
#else
        mrb_int_overflow(mrb, "pow");
#endif
      }
    }
    base = tmp % mod;
  }
  return mrb_int_value(mrb, result);
}

#ifndef MRB_NO_FLOAT
static mrb_value
flo_remainder(mrb_state *mrb, mrb_value self)
{
  mrb_float a, b;

  a = mrb_float(self);
  mrb_get_args(mrb, "f", &b);
  if (b == 0) mrb_int_zerodiv(mrb);
  if (isinf(b)) return mrb_float_value(mrb, a);
  return mrb_float_value(mrb, a-b*trunc(a/b));
}
#endif

void
mrb_mruby_numeric_ext_gem_init(mrb_state* mrb)
{
  struct RClass *i = mrb->integer_class;

  mrb_define_alias(mrb, i, "modulo", "%");
  mrb_define_method(mrb, i, "remainder", int_remainder, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, i, MRB_SYM(pow), int_powm, MRB_ARGS_ARG(1,1));

#ifndef MRB_NO_FLOAT
  struct RClass *f = mrb->float_class;

  mrb_define_alias(mrb, f, "modulo", "%");
  mrb_define_method(mrb, f, "remainder", flo_remainder, MRB_ARGS_REQ(1));

  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(RADIX),        mrb_fixnum_value(MRB_FLT_RADIX));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(MANT_DIG),     mrb_fixnum_value(MRB_FLT_MANT_DIG));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(EPSILON),      mrb_float_value(mrb, MRB_FLT_EPSILON));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(DIG),          mrb_fixnum_value(MRB_FLT_DIG));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(MIN_EXP),      mrb_fixnum_value(MRB_FLT_MIN_EXP));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(MIN),          mrb_float_value(mrb, MRB_FLT_MIN));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(MIN_10_EXP),   mrb_fixnum_value(MRB_FLT_MIN_10_EXP));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(MAX_EXP),      mrb_fixnum_value(MRB_FLT_MAX_EXP));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(MAX),          mrb_float_value(mrb, MRB_FLT_MAX));
  mrb_define_const_id(mrb, mrb->float_class, MRB_SYM(MAX_10_EXP),   mrb_fixnum_value(MRB_FLT_MAX_10_EXP));
#endif /* MRB_NO_FLOAT */
}

void
mrb_mruby_numeric_ext_gem_final(mrb_state* mrb)
{
}
