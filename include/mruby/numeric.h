/*
** mruby/numeric.h - Numeric, Integer, Float, Fixnum class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_NUMERIC_H
#define MRUBY_NUMERIC_H

#if defined(__cplusplus)
extern "C" {
#endif

#define POSFIXABLE(f) ((f) <= MRB_INT_MAX)
#define NEGFIXABLE(f) ((f) >= MRB_INT_MIN)
#define FIXABLE(f) (POSFIXABLE(f) && NEGFIXABLE(f))

MRB_API mrb_value mrb_flo_to_fixnum(mrb_state *mrb, mrb_value val);
MRB_API mrb_value mrb_fixnum_to_str(mrb_state *mrb, mrb_value x, int base);
MRB_API mrb_float mrb_to_flo(mrb_state *mrb, mrb_value x);

mrb_value mrb_fixnum_plus(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_fixnum_minus(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_fixnum_mul(mrb_state *mrb, mrb_value x, mrb_value y);
mrb_value mrb_num_div(mrb_state *mrb, mrb_value x, mrb_value y);

#define MRB_UINT_MAKE2(n) uint ## n ## _t
#define MRB_UINT_MAKE(n) MRB_UINT_MAKE2(n)
#define mrb_uint MRB_UINT_MAKE(MRB_INT_BIT)

#ifdef MRB_WORD_BOXING
# define MRB_INT_OVERFLOW_MASK ((mrb_uint)1 << (MRB_INT_BIT - 1 - MRB_FIXNUM_SHIFT))
#else
# define MRB_INT_OVERFLOW_MASK ((mrb_uint)1 << (MRB_INT_BIT - 1))
#endif

static inline mrb_bool
mrb_int_add_overflow(mrb_int augend, mrb_int addend, mrb_int *sum)
{
  mrb_uint x = (mrb_uint)augend;
  mrb_uint y = (mrb_uint)addend;
  mrb_uint z = (mrb_uint)(x + y);
  *sum = (mrb_int)z;
  return !!(((x ^ z) & (y ^ z)) & MRB_INT_OVERFLOW_MASK);
}

static inline mrb_bool
mrb_int_sub_overflow(mrb_int minuend, mrb_int subtrahend, mrb_int *difference)
{
  mrb_uint x = (mrb_uint)minuend;
  mrb_uint y = (mrb_uint)subtrahend;
  mrb_uint z = (mrb_uint)(x - y);
  *difference = (mrb_int)z;
  return !!(((x ^ z) & (~y ^ z)) & MRB_INT_OVERFLOW_MASK);
}

#undef MRB_INT_OVERFLOW_MASK
#undef mrb_uint
#undef MRB_UINT_MAKE
#undef MRB_UINT_MAKE2

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_NUMERIC_H */
