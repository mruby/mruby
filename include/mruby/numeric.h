/**
** @file mruby/numeric.h - Numeric, Integer, Float class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_NUMERIC_H
#define MRUBY_NUMERIC_H

#include "common.h"

/**
 * Numeric class and it's sub-classes.
 *
 * Integer and Float
 */
MRB_BEGIN_DECL

#define TYPED_POSFIXABLE(f,t) ((f) <= (t)MRB_FIXNUM_MAX)
#define TYPED_NEGFIXABLE(f,t) ((f) >= (t)MRB_FIXNUM_MIN)
#define TYPED_FIXABLE(f,t) (TYPED_POSFIXABLE(f,t) && TYPED_NEGFIXABLE(f,t))
#define POSFIXABLE(f) TYPED_POSFIXABLE(f,mrb_int)
#define NEGFIXABLE(f) TYPED_NEGFIXABLE(f,mrb_int)
#define FIXABLE(f) TYPED_FIXABLE(f,mrb_int)
#ifndef MRB_NO_FLOAT
/* whether a float fits in mrb_int; an mrb_int past MRB_FIXNUM_MAX is still
   an Integer (boxed), so the bound is the type's, written as powers of two
   that a float holds exactly */
#ifdef MRB_INT64
#define FIXABLE_FLOAT(f) ((f)>=-9223372036854775808.0 && (f)<9223372036854775808.0)
#else
#define FIXABLE_FLOAT(f) ((f)>=-2147483648.0 && (f)<2147483648.0)
#endif
#endif

/* utility functions */
MRB_API mrb_value mrb_num_add(mrb_state *mrb, mrb_value x, mrb_value y);
MRB_API mrb_value mrb_num_sub(mrb_state *mrb, mrb_value x, mrb_value y);
MRB_API mrb_value mrb_num_mul(mrb_state *mrb, mrb_value x, mrb_value y);
/* obsolete old names */
#define mrb_num_plus(mrb, x, y) mrb_num_add(mrb, x, y)
#define mrb_num_minus(mrb, x, y) mrb_num_sub(mrb, x, y)

/* Answer with the Integer that holds a C integer of a width mrb_int may be
   too narrow for: a time_t, a size_t, a count.  Inside mrb_int the answer is a
   Fixnum, outside it a Bignum, and where the build carries no mruby-bigint
   there is no Integer wide enough and the call raises RangeError.  Reach for
   these rather than mrb_bint_* so a caller needs neither the gem's headers nor
   an #ifdef of its own. */
MRB_API mrb_value mrb_uint64_value(mrb_state *mrb, uint64_t v);
MRB_API mrb_value mrb_int64_value(mrb_state *mrb, int64_t v);

/* The same boundary read from the other side: the C integer an Integer holds.
   A value inside the width answers itself however it is stored, a Fixnum and
   a Bignum alike, and one outside it raises RangeError, so what went through
   the calls above comes back through these.  A Float or a Rational is taken
   as mrb_ensure_integer_type() takes it, and what has no integer to give
   raises TypeError.

   Spelled `as` because that is what this direction is called here, as in
   mrb_as_int(), mrb_as_float() and mrb_bint_as_int64(). */
MRB_API uint64_t mrb_as_uint64(mrb_state *mrb, mrb_value x);
MRB_API int64_t mrb_as_int64(mrb_state *mrb, mrb_value x);

/* The same, spelled for the C types a library counts in, whose own width
   varies by platform: a size_t is 32 bits where the time_t beside it is 64.
   A caller passes what it holds and does not have to know which of the two
   above that is today.

   Named `value_from_` rather than `_value` on purpose.  `mrb_int_value` reads
   one way only, since an int is not a property something has; `mrb_size_value`
   would read as the size of a value as readily as a value from a size, beside
   mrb_hash_size and mrb_bint_bytes_size which are exactly that.  And `mrb_ssize`
   is already a type here, an mrb_int or an intptr_t, which is not the ssize_t
   this takes: they differ in width wherever mrb_int is 32 bits. */
#define mrb_value_from_size_t(mrb, v)   mrb_uint64_value((mrb), (uint64_t)(v))
#define mrb_value_from_ssize_t(mrb, v)  mrb_int64_value((mrb), (int64_t)(v))

#ifdef MRB_USE_BIGINT
/* An Integer as the bytes a wire format spells, most significant first, with
   the sign kept apart from them: what CBOR's bignum tags and ASN.1's INTEGER
   are written in.  This is not the order the limbs sit in, which is whatever
   this machine stores an integer in and is what mrb_bint_new_bytes() reads.

   mrb_integer_to_bytes() answers the bytes the magnitude needs, and writes
   them only when `buf` is not NULL and `len` is at least that, so asking with
   NULL is how a caller learns what to allocate.  `sign` takes -1, 0 or 1, and
   may be NULL.  Zero needs no bytes and answers 0.

   mrb_integer_from_bytes() reads them back.  Leading zero bytes are allowed
   and drop out, a `sign` of 0 answers 0 whatever the bytes say, and the
   answer is a Fixnum where the value fits one.

   Declared where the conversions above are, but carried by mruby-bigint: a
   build without it has no Integer these are for. */
MRB_API size_t mrb_integer_to_bytes(mrb_state *mrb, mrb_value x, uint8_t *buf, size_t len, int *sign);
MRB_API mrb_value mrb_integer_from_bytes(mrb_state *mrb, const uint8_t *bytes, size_t len, int sign);
#endif

MRB_API mrb_value mrb_integer_to_str(mrb_state *mrb, mrb_value x, mrb_int base);
MRB_API char *mrb_int_to_cstr(char *buf, size_t len, mrb_int n, mrb_int base);

/* obsolete function(s); will be removed */
#define mrb_fixnum_to_str(mrb, x, base) mrb_integer_to_str(mrb, x, base)

#ifndef __has_builtin
  #define __has_builtin(x) 0
#endif

#if (defined(__GNUC__) && __GNUC__ >= 5) ||   \
    (__has_builtin(__builtin_add_overflow) && \
     __has_builtin(__builtin_sub_overflow) && \
     __has_builtin(__builtin_mul_overflow))
# define MRB_HAVE_TYPE_GENERIC_CHECKED_ARITHMETIC_BUILTINS
#endif

/*
// Clang 3.8 and 3.9 have problem compiling mruby in 32-bit mode, when MRB_INT64 is set
// because of missing __mulodi4 and similar functions in its runtime. We need to use custom
// implementation for them.
*/
#ifdef MRB_HAVE_TYPE_GENERIC_CHECKED_ARITHMETIC_BUILTINS
#if defined(__clang__) && (__clang_major__ == 3) && (__clang_minor__ >= 8) && \
    defined(MRB_32BIT) && defined(MRB_INT64)
#undef MRB_HAVE_TYPE_GENERIC_CHECKED_ARITHMETIC_BUILTINS
#endif
#endif

#ifdef MRB_HAVE_TYPE_GENERIC_CHECKED_ARITHMETIC_BUILTINS

static inline mrb_bool
mrb_int_add_overflow(mrb_int augend, mrb_int addend, mrb_int *sum)
{
  return __builtin_add_overflow(augend, addend, sum);
}

static inline mrb_bool
mrb_int_sub_overflow(mrb_int minuend, mrb_int subtrahend, mrb_int *difference)
{
  return __builtin_sub_overflow(minuend, subtrahend, difference);
}

static inline mrb_bool
mrb_int_mul_overflow(mrb_int multiplier, mrb_int multiplicand, mrb_int *product)
{
  return __builtin_mul_overflow(multiplier, multiplicand, product);
}

#else

#define MRB_INT_OVERFLOW_MASK ((mrb_uint)1 << (MRB_INT_BIT - 1))

static inline mrb_bool
mrb_int_add_overflow(mrb_int a, mrb_int b, mrb_int *c)
{
  mrb_uint x = (mrb_uint)a;
  mrb_uint y = (mrb_uint)b;
  mrb_uint z = (mrb_uint)(x + y);
  *c = (mrb_int)z;
  return !!(((x ^ z) & (y ^ z)) & MRB_INT_OVERFLOW_MASK);
}

static inline mrb_bool
mrb_int_sub_overflow(mrb_int a, mrb_int b, mrb_int *c)
{
  mrb_uint x = (mrb_uint)a;
  mrb_uint y = (mrb_uint)b;
  mrb_uint z = (mrb_uint)(x - y);
  *c = (mrb_int)z;
  return !!(((x ^ z) & (~y ^ z)) & MRB_INT_OVERFLOW_MASK);
}

static inline mrb_bool
mrb_int_mul_overflow(mrb_int a, mrb_int b, mrb_int *c)
{
#ifdef MRB_INT32
  int64_t n = (int64_t)a * b;
  *c = (mrb_int)n;
  return n > MRB_INT_MAX || n < MRB_INT_MIN;
#else /* MRB_INT64 */
  *c = a * b;
  if (a > 0 && b > 0 && a > MRB_INT_MAX / b) return TRUE;
  if (a < 0 && b > 0 && a < MRB_INT_MIN / b) return TRUE;
  if (a > 0 && b < 0 && b < MRB_INT_MIN / a) return TRUE;
  if (a < 0 && b < 0 && (a <= MRB_INT_MIN || b <= MRB_INT_MIN || -a > MRB_INT_MAX / -b))
    return TRUE;
  return FALSE;
#endif
}

#undef MRB_INT_OVERFLOW_MASK

#endif

#ifndef MRB_NO_FLOAT

# define MRB_FLT_RADIX          FLT_RADIX

# ifdef MRB_USE_FLOAT32
#  define MRB_FLT_MANT_DIG      FLT_MANT_DIG
#  define MRB_FLT_EPSILON       FLT_EPSILON
#  define MRB_FLT_DIG           FLT_DIG
#  define MRB_FLT_MIN_EXP       FLT_MIN_EXP
#  define MRB_FLT_MIN           FLT_MIN
#  define MRB_FLT_MIN_10_EXP    FLT_MIN_10_EXP
#  define MRB_FLT_MAX_EXP       FLT_MAX_EXP
#  define MRB_FLT_MAX           FLT_MAX
#  define MRB_FLT_MAX_10_EXP    FLT_MAX_10_EXP

# else /* not MRB_USE_FLOAT32 */
#  define MRB_FLT_MANT_DIG      DBL_MANT_DIG
#  define MRB_FLT_EPSILON       DBL_EPSILON
#  define MRB_FLT_DIG           DBL_DIG
#  define MRB_FLT_MIN_EXP       DBL_MIN_EXP
#  define MRB_FLT_MIN           DBL_MIN
#  define MRB_FLT_MIN_10_EXP    DBL_MIN_10_EXP
#  define MRB_FLT_MAX_EXP       DBL_MAX_EXP
#  define MRB_FLT_MAX           DBL_MAX
#  define MRB_FLT_MAX_10_EXP    DBL_MAX_10_EXP
# endif /* MRB_USE_FLOAT32 */

MRB_API mrb_value mrb_float_to_integer(mrb_state *mrb, mrb_value val);

/* internal functions */
mrb_float mrb_div_float(mrb_float x, mrb_float y);
mrb_value mrb_float_to_str(mrb_state *mrb, mrb_value x, const char *fmt);
int mrb_format_float(mrb_float f, char *buf, size_t buf_size, char fmt, int prec, char sign);

#endif /* MRB_NO_FLOAT */

MRB_END_DECL

#endif  /* MRUBY_NUMERIC_H */
