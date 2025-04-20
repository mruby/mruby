#include <mruby.h>
#include <mruby/class.h>
#include <mruby/numeric.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#ifndef MRB_NO_FLOAT
#include <math.h>
mrb_value mrb_complex_new(mrb_state *, mrb_float, mrb_float);
#endif
mrb_bool mrb_complex_eq(mrb_state *mrb, mrb_value, mrb_value);
mrb_value mrb_complex_add(mrb_state *mrb, mrb_value, mrb_value);
mrb_value mrb_complex_sub(mrb_state *mrb, mrb_value, mrb_value);
mrb_value mrb_complex_mul(mrb_state *mrb, mrb_value, mrb_value);
mrb_value mrb_complex_div(mrb_state *mrb, mrb_value, mrb_value);
mrb_value mrb_bint_mul_n(mrb_state *mrb, mrb_value x, mrb_value y);
void mrb_bint_reduce(mrb_state *mrb, mrb_value *x, mrb_value *y);

#ifdef MRB_USE_BIGINT
struct mrb_rational {
  union {
    struct {
      mrb_int num;
      mrb_int den;
    } i;
    struct {
      struct RBasic *num;
      struct RBasic *den;
    } b;
  };
};
#define numerator i.num
#define denominator i.den
#define RAT_BIGINT 1
#define RAT_BIGINT_P(obj) (mrb_obj_ptr(obj)->flags & RAT_BIGINT)
#else
struct mrb_rational {
  mrb_int numerator;
  mrb_int denominator;
};
#endif

#define ONE mrb_fixnum_value(1)
#define ZERO mrb_fixnum_value(0)

#if defined(MRB_INT64) && defined(MRB_32BIT)
struct RRational {
  MRB_OBJECT_HEADER;
  struct mrb_rational *p;
};

static struct mrb_rational*
rat_ptr(mrb_state *mrb, mrb_value v)
{
  struct RRational *r = (struct RRational*)mrb_obj_ptr(v);

  if (!r->p) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized rational");
  }
  return r->p;
}
#else
#define RATIONAL_INLINE
struct RRational {
  MRB_OBJECT_HEADER;
  struct mrb_rational r;
};
#define rat_ptr(mrb, v) (&((struct RRational*)mrb_obj_ptr(v))->r)
#endif

mrb_static_assert_object_size(struct RRational);

static struct mrb_rational*
rat_alloc(mrb_state *mrb, struct RClass *c, struct RBasic **obj)
{
  struct RRational *s = MRB_OBJ_ALLOC(mrb, MRB_TT_RATIONAL, c);
  struct mrb_rational *p;
#ifdef RATIONAL_INLINE
  p = &s->r;
#else
  p = s->p = (struct mrb_rational*)mrb_malloc(mrb, sizeof(struct mrb_rational));
#endif
  *obj = (struct RBasic*)s;
  return p;
}

#ifdef RAT_BIGINT
int
mrb_rational_mark(mrb_state *mrb, struct RBasic *rat)
{
  if (!(rat->flags & RAT_BIGINT)) return 0;

  mrb_value self = mrb_obj_value(rat);
  struct mrb_rational *p = rat_ptr(mrb, self);
  mrb_gc_mark(mrb, p->b.num);
  mrb_gc_mark(mrb, p->b.den);
  return 2;
}
#endif

static mrb_value
rat_numerator(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rat_ptr(mrb, self);
#ifdef RAT_BIGINT
  if (RAT_BIGINT_P(self)) {
    return mrb_obj_value(p->b.num);
  }
#endif
  return mrb_int_value(mrb, p->numerator);
}

/* normalized version of rat_numerator() */
static mrb_value
rational_numerator(mrb_state *mrb, mrb_value self)
{
  mrb_value n = rat_numerator(mrb, self);
  if (mrb_bigint_p(n)) {
    /* normalize bigint */
    return mrb_bint_mul(mrb, n, ONE);
  }
  return n;
}

static mrb_value
rat_denominator(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rat_ptr(mrb, self);
#ifdef RAT_BIGINT
  if (RAT_BIGINT_P(self)) {
    return mrb_obj_value(p->b.den);
  }
#endif
  return mrb_int_value(mrb, p->denominator);
}

/* normalized version of rat_denominator() */
static mrb_value
rational_denominator(mrb_state *mrb, mrb_value self)
{
  mrb_value n = rat_denominator(mrb, self);
  if (mrb_bigint_p(n)) {
    /* normalize bigint */
    return mrb_bint_mul(mrb, n, ONE);
  }
  return n;
}

static mrb_noreturn void
rat_overflow(mrb_state *mrb)
{
  mrb_raise(mrb, E_RANGE_ERROR, "integer overflow in rational");
}

static mrb_noreturn void
rat_zerodiv(mrb_state *mrb)
{
  mrb_raise(mrb, E_ZERODIV_ERROR, "divided by 0 in rational");
}

static mrb_noreturn void
rat_type_error(mrb_state *mrb, mrb_value x)
{
  mrb_raisef(mrb, E_TYPE_ERROR, "%T cannot be converted to Rational", x);
}

void
mrb_rational_copy(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct mrb_rational *p1 = rat_ptr(mrb, x);
  struct mrb_rational *p2 = rat_ptr(mrb, y);
#ifdef RAT_BIGINT
 struct RRational *r = (struct RRational*)mrb_obj_ptr(x);
  if (RAT_BIGINT_P(y)) {
    p1->b.num = p2->b.num;
    p1->b.den = p2->b.den;
    r->flags |= RAT_BIGINT;
    return;
  }
  r->flags &= ~RAT_BIGINT;
#endif
  p1->numerator = p2->numerator;
  p1->denominator = p2->denominator;
}

inline static mrb_int
i_gcd(mrb_int x, mrb_int y)
{
  mrb_uint u, v, t;
  int shift;

  if (x < 0)
    x = -x;
  if (y < 0)
    y = -y;

  if (x == 0)
    return y;
  if (y == 0)
    return x;

  u = (mrb_uint)x;
  v = (mrb_uint)y;
  for (shift = 0; ((u | v) & 1) == 0; shift++) {
    u >>= 1;
    v >>= 1;
  }

  while ((u & 1) == 0)
    u >>= 1;

  do {
    while ((v & 1) == 0)
      v >>= 1;

    if (u > v) {
      t = v;
      v = u;
      u = t;
    }
    v = v - u;
  } while (v != 0);

  return (mrb_int)(u << shift);
}

#ifdef RAT_BIGINT
static mrb_value
rational_new_b(mrb_state *mrb, mrb_value n, mrb_value d)
{
  /* bigint check */
  mrb_assert(mrb_bigint_p(n));
  d = mrb_as_bint(mrb, d);
  mrb_int cmp = mrb_bint_cmp(mrb, d, ZERO);
  if (cmp == 0) {
    rat_zerodiv(mrb);
  }
  /* negative */
  if (cmp < 0) {
    n = mrb_bint_neg(mrb, n);
    d = mrb_bint_neg(mrb, d);
  }
  /* normalize (n/gcd, d/gcd) */
  mrb_bint_reduce(mrb, &n, &d);
  struct RClass *c = mrb_class_get_id(mrb, MRB_SYM(Rational));
  struct RBasic *rat;
  struct mrb_rational *p = rat_alloc(mrb, c, &rat);
  rat->flags |= RAT_BIGINT;
  p->b.num = (struct RBasic*)mrb_obj_ptr(n);
  p->b.den = (struct RBasic*)mrb_obj_ptr(d);
  rat->frozen = 1;
  return mrb_obj_value(rat);
}
#endif

mrb_value
mrb_rational_new(mrb_state *mrb, mrb_int nume, mrb_int deno)
{
  if (deno == 0) {
    rat_zerodiv(mrb);
  }
  if (nume == MRB_INT_MIN || deno == MRB_INT_MIN) {
#ifdef RAT_BIGINT
    mrb_value num = mrb_as_bint(mrb, mrb_int_value(mrb, nume));
    mrb_value den = mrb_as_bint(mrb, mrb_int_value(mrb, deno));
    return rational_new_b(mrb, num, den);
#else
    rat_overflow(mrb);
#endif
  }
  if (deno < 0) {
    nume *= -1;
    deno *= -1;
  }

  mrb_int a = i_gcd(nume, deno);
  nume /= a;
  deno /= a;

  struct RClass *c = mrb_class_get_id(mrb, MRB_SYM(Rational));
  struct RBasic *rat;
  struct mrb_rational *p = rat_alloc(mrb, c, &rat);
  p->numerator = nume;
  p->denominator = deno;
  rat->frozen = 1;
  return mrb_obj_value(rat);
}

#define rational_new_i(mrb,n,d) mrb_rational_new(mrb, n, d)

#ifndef MRB_NO_FLOAT

#if defined(MRB_INT32) || defined(MRB_USE_FLOAT32)
#define frexp_rat(x,exp) frexpf((float)x, exp)
#define ldexp_rat(x,exp) ldexpf((float)x, exp)
#define RAT_MANT_DIG FLT_MANT_DIG
#define RAT_INT_LIMIT 30
#define RAT_HUGE_VAL HUGE_VALF
#else
#define frexp_rat frexp
#define ldexp_rat ldexp
#define RAT_MANT_DIG DBL_MANT_DIG
#define RAT_INT_LIMIT 62
#define RAT_HUGE_VAL HUGE_VAL
#endif

#define mrb_int_fit_p(x,t) ((t)MRB_INT_MIN <= (x) && (x) <= (t)MRB_INT_MAX)

static mrb_value
int_lshift(mrb_state *mrb, mrb_value v, mrb_int n)
{
  if (mrb_integer_p(v)) {
    mrb_float f = (mrb_float)mrb_integer(v);
    f *= 1<<n;
    if (mrb_int_fit_p(f, mrb_float))
      return mrb_int_value(mrb, (mrb_int)f);
  }
#ifndef RAT_BIGINT
  rat_overflow(mrb);
#else
  return mrb_bint_lshift(mrb, mrb_as_bint(mrb, v), n);
#endif
}

static mrb_value
rational_new_f(mrb_state *mrb, mrb_float f)
{
  mrb_check_num_exact(mrb, f);
  if (f == 0.0) {
    return rational_new_i(mrb, 0, 1);
  }
  int exp;
  // Extract mantissa and exponent
  double mantissa = frexp_rat(f, &exp);
  const mrb_int precision = ((mrb_int)1) << RAT_MANT_DIG;

  mrb_int nume = (mrb_int)(mantissa * precision);
  mrb_int deno = precision;

  if (exp > 0) {
    mrb_int temp;
    if (mrb_int_mul_overflow(nume, ((mrb_int)1)<<exp, &temp)) {
#ifndef RAT_BIGINT
      rat_overflow(mrb);
#else
      mrb_value n = int_lshift(mrb, mrb_int_value(mrb, nume), exp);
      if (mrb_bigint_p(n)) {
        return rational_new_b(mrb, n, mrb_int_value(mrb, deno));
      }
#endif
    }
    nume = temp;
  }
  else {
    deno >>= exp;
  }
  return rational_new_i(mrb, nume, deno);
}

static mrb_float
rat_float(mrb_state *mrb, mrb_value x)
{
  struct mrb_rational *p = rat_ptr(mrb, x);

#ifdef RAT_BIGINT
  if (RAT_BIGINT_P(x)) {
    return mrb_bint_as_float(mrb, mrb_obj_value(p->b.num)) / mrb_bint_as_float(mrb, mrb_obj_value(p->b.den));
  }
#endif
  return (mrb_float)p->numerator / (mrb_float)p->denominator;
}

mrb_value
mrb_rational_to_f(mrb_state *mrb, mrb_value self)
{
  mrb_float f = rat_float(mrb, self);
  return mrb_float_value(mrb, f);
}
#endif

mrb_value
mrb_rational_to_i(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rat_ptr(mrb, self);
#ifdef RAT_BIGINT
  if (RAT_BIGINT_P(self)) {
    return mrb_bint_div(mrb, mrb_obj_value(p->b.num), mrb_obj_value(p->b.den));
  }
#endif
  return mrb_int_value(mrb, p->numerator / p->denominator);
}

mrb_value
mrb_as_rational(mrb_state *mrb, mrb_value x)
{
  switch(mrb_type(x)) {
  case MRB_TT_INTEGER:
    return rational_new_i(mrb, mrb_integer(x), 1);
#ifdef RAT_BIGINT
  case MRB_TT_BIGINT:
    return rational_new_b(mrb, x, ONE);
#endif
  case MRB_TT_RATIONAL:
    return x;
#ifndef MRB_NO_FLOAT
#ifdef MRB_USE_COMPLEX
  case MRB_TT_COMPLEX:
#endif
  case MRB_TT_FLOAT:
    return rational_new_f(mrb, mrb_as_float(mrb, x));
#endif
  default:
    rat_type_error(mrb, x);
  }
}

static mrb_value
rational_negative_p(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rat_ptr(mrb, self);
#ifdef RAT_BIGINT
  if (RAT_BIGINT_P(self)) {
    mrb_int cmp = mrb_bint_cmp(mrb, mrb_obj_value(p->b.num), ZERO);
    return mrb_bool_value(cmp < 0);
  }
#endif
  return mrb_bool_value(p->numerator < 0);
}

#ifndef MRB_NO_FLOAT
static mrb_value
float_to_r(mrb_state *mrb, mrb_value self)
{
  return rational_new_f(mrb, mrb_float(self));
}
#endif

static mrb_value
int_to_r(mrb_state *mrb, mrb_value self)
{
#ifdef RAT_BIGINT
  if (mrb_bigint_p(self)) {
    return rational_new_b(mrb, self, ONE);
  }
#endif
  return rational_new_i(mrb, mrb_integer(self), 1);
}

static mrb_value
nil_to_r(mrb_state *mrb, mrb_value self)
{
  return rational_new_i(mrb, 0, 1);
}

#if !defined(MRB_NO_FLOAT) || defined(RAT_BIGINT)
static mrb_value
rational_new(mrb_state *mrb, mrb_value a, mrb_value b)
{
#ifdef MRB_NO_FLOAT
  a = mrb_as_int(mrb, a);
  b = mrb_as_int(mrb, b);
  return rational_new_i(mrb, mrb_integer(a), mrb_integer(b));
#else
  if (mrb_integer_p(a) && mrb_integer_p(b)) {
    return rational_new_i(mrb, mrb_integer(a), mrb_integer(b));
  }
#ifdef RAT_BIGINT
  else if (mrb_bigint_p(a) || mrb_bigint_p(b)) {
    return rational_new_b(mrb, mrb_as_bint(mrb, a), b);
  }
#endif
  else {
    mrb_float x = mrb_as_float(mrb, a);
    mrb_float y = mrb_as_float(mrb, b);
    return rational_new_f(mrb, x/y);
  }
#endif
}

static mrb_value
rational_m(mrb_state *mrb, mrb_value self)
{
  mrb_value a, b = ONE;
  mrb_get_args(mrb, "o|o", &a, &b);
  return rational_new(mrb, a, b);
}

#else

static mrb_value
rational_m(mrb_state *mrb, mrb_value self)
{
  mrb_int n, d = 1;
  mrb_get_args(mrb, "i|i", &n, &d);
  return rational_new_i(mrb, n, d);
}
#endif

static mrb_value
rational_eq_b(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct mrb_rational *p1 = rat_ptr(mrb, x);
  mrb_bool result;

  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
    if (p1->denominator != 1) return mrb_false_value();
    result = p1->numerator == mrb_integer(y);
    break;
#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
    result = ((double)p1->numerator/p1->denominator) == mrb_float(y);
    break;
#endif
  case MRB_TT_RATIONAL:
    {
      struct mrb_rational *p2 = rat_ptr(mrb, y);
      mrb_int a, b;

      if (p1->numerator == p2->numerator && p1->denominator == p2->denominator) {
        return mrb_true_value();
      }
      if (mrb_int_mul_overflow(p1->numerator, p2->denominator, &a) ||
          mrb_int_mul_overflow(p2->numerator, p1->denominator, &b)) {
#ifdef MRB_NO_FLOAT
        rat_overflow(mrb);
#else
        result = (double)p1->numerator*p2->denominator == (double)p2->numerator*p2->denominator;
        break;
#endif
      }
      result = a == b;
      break;
    }

#ifdef MRB_USE_COMPLEX
  case MRB_TT_COMPLEX:
   {
      result = mrb_complex_eq(mrb, y, mrb_rational_to_f(mrb, x));
      break;
    }
#endif
  default:
    result = mrb_equal(mrb, y, x);
    break;
  }
  return mrb_bool_value(result);
}

static mrb_value
rational_eq(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
#ifdef RAT_BIGINT
  if (RAT_BIGINT_P(x)) return rational_eq_b(mrb, x, y);
#endif
  struct mrb_rational *p1 = rat_ptr(mrb, x);
  mrb_bool result;

  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
    if (p1->denominator != 1) return mrb_false_value();
    result = p1->numerator == mrb_integer(y);
    break;
#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
    result = ((double)p1->numerator/p1->denominator) == mrb_float(y);
    break;
#endif
  case MRB_TT_RATIONAL:
    {
      struct mrb_rational *p2 = rat_ptr(mrb, y);
      mrb_int a, b;

      if (p1->numerator == p2->numerator && p1->denominator == p2->denominator) {
        return mrb_true_value();
      }
      if (mrb_int_mul_overflow(p1->numerator, p2->denominator, &a) ||
          mrb_int_mul_overflow(p2->numerator, p1->denominator, &b)) {
#ifdef MRB_NO_FLOAT
        rat_overflow(mrb);
#else
        result = (double)p1->numerator*p2->denominator == (double)p2->numerator*p2->denominator;
        break;
#endif
      }
      result = a == b;
      break;
    }

#ifdef MRB_USE_COMPLEX
  case MRB_TT_COMPLEX:
   {
      result = mrb_complex_eq(mrb, y, mrb_rational_to_f(mrb, x));
      break;
    }
#endif
  default:
    result = mrb_equal(mrb, y, x);
    break;
  }
  return mrb_bool_value(result);
}

static mrb_value
rational_minus(mrb_state *mrb, mrb_value x)
{
  struct mrb_rational *p = rat_ptr(mrb, x);
#ifdef RAT_BIGINT
  mrb_value num;
  if (RAT_BIGINT_P(x)) {
    num = mrb_obj_value(p->b.num);
  bint:
    return rational_new_b(mrb, mrb_bint_neg(mrb, num), mrb_obj_value(p->b.den));
  }
#endif
  mrb_int n = p->numerator;
  if (n == MRB_INT_MIN) {
#ifdef RAT_BIGINT
    num = mrb_as_bint(mrb, mrb_int_value(mrb, p->numerator));
    goto bint;
#else
    rat_overflow(mrb);
#endif
  }
  return rational_new_i(mrb, -n, p->denominator);
}

#ifdef RAT_BIGINT
static mrb_value
rat_add_b(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mrb_value num1 = rat_numerator(mrb, x);
  mrb_value den1 = rat_denominator(mrb, x);
  mrb_value num2, den2;

  switch(mrb_type(y)) {
  case MRB_TT_RATIONAL:
    num2 = rat_numerator(mrb, y);
    den2 = rat_denominator(mrb, y);
    break;
  case MRB_TT_INTEGER:
  case MRB_TT_BIGINT:
    num2 = y;
    den2 = ONE;
    break;
  default:
    /* should not happen */
    rat_type_error(mrb, y);
  }

  mrb_value a = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, num1), den2);
  mrb_value b = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, num2), den1);
  a = mrb_bint_add_n(mrb, a, b);
  b = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, den1), den2);
  return rational_new_b(mrb, a, b);
}
#endif

mrb_value
mrb_rational_add(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct mrb_rational *p1 = rat_ptr(mrb, x);

  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
#ifdef RAT_BIGINT
    if (RAT_BIGINT_P(x)) return rat_add_b(mrb, x, y);
#endif
    {
      mrb_int z = mrb_integer(y);
      if (mrb_int_mul_overflow(z, p1->denominator, &z)) rat_overflow(mrb);
      if (mrb_int_add_overflow(p1->numerator, z, &z)) rat_overflow(mrb);
      return rational_new_i(mrb, z, p1->denominator);
    }
  case MRB_TT_RATIONAL:
#ifdef RAT_BIGINT
    if (RAT_BIGINT_P(x) || RAT_BIGINT_P(y))
      return rat_add_b(mrb, x, y);
#endif
    {
      struct mrb_rational *p2 = rat_ptr(mrb, y);
      mrb_int a, b;

      if (mrb_int_mul_overflow(p1->numerator, p2->denominator, &a)) rat_overflow(mrb);
      if (mrb_int_mul_overflow(p2->numerator, p1->denominator, &b)) rat_overflow(mrb);
      if (mrb_int_add_overflow(a, b, &a)) rat_overflow(mrb);
      if (mrb_int_mul_overflow(p1->denominator, p2->denominator, &b)) rat_overflow(mrb);
      return rational_new_i(mrb, a, b);
    }

#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
    {
      mrb_float z = p1->numerator + mrb_float(y) * p1->denominator;
      return mrb_float_value(mrb, mrb_div_float(z, (mrb_float)p1->denominator));
    }
#endif

#ifdef RAT_BIGINT
  case MRB_TT_BIGINT:
    return rat_add_b(mrb, x, y);
#endif

#if defined(MRB_USE_COMPLEX)
  case MRB_TT_COMPLEX:
    return mrb_complex_add(mrb, mrb_complex_new(mrb, rat_float(mrb, x), 0), y);
#endif

  default:
    return mrb_funcall_argv(mrb, y, MRB_OPSYM(add), 1, &x);
  }
}

static mrb_value
rational_add(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_rational_add(mrb, x, y);
}

#ifdef RAT_BIGINT
static mrb_value
rat_sub_b(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mrb_value num1 = rat_numerator(mrb, x);
  mrb_value den1 = rat_denominator(mrb, x);
  mrb_value num2, den2;

  switch(mrb_type(y)) {
  case MRB_TT_RATIONAL:
    num2 = rat_numerator(mrb, y);
    den2 = rat_denominator(mrb, y);
    break;
  case MRB_TT_INTEGER:
  case MRB_TT_BIGINT:
    num2 = y;
    den2 = ONE;
    break;
  default:
    /* should not happen */
    rat_type_error(mrb, y);
  }

  mrb_value a = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, num1), den2);
  mrb_value b = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, num2), den1);
  a = mrb_bint_sub_n(mrb, a, b);
  b = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, den1), den2);
  return rational_new_b(mrb, a, b);
}
#endif

mrb_value
mrb_rational_sub(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct mrb_rational *p1 = rat_ptr(mrb, x);

  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
#ifdef RAT_BIGINT
    if (RAT_BIGINT_P(x)) return rat_sub_b(mrb, x, y);
#endif
    {
      mrb_int z = mrb_integer(y);
      if (mrb_int_mul_overflow(z, p1->denominator, &z)) rat_overflow(mrb);
      if (mrb_int_sub_overflow(p1->numerator, z, &z)) rat_overflow(mrb);
      return rational_new_i(mrb, z, p1->denominator);
    }
  case MRB_TT_RATIONAL:
#ifdef RAT_BIGINT
    if (RAT_BIGINT_P(x) || RAT_BIGINT_P(y))
      return rat_sub_b(mrb, x, y);
#endif
    {
      struct mrb_rational *p2 = rat_ptr(mrb, y);
      mrb_int a, b;

      if (mrb_int_mul_overflow(p1->numerator, p2->denominator, &a)) rat_overflow(mrb);
      if (mrb_int_mul_overflow(p2->numerator, p1->denominator, &b)) rat_overflow(mrb);
      if (mrb_int_sub_overflow(a, b, &a)) rat_overflow(mrb);
      if (mrb_int_mul_overflow(p1->denominator, p2->denominator, &b)) rat_overflow(mrb);
      return rational_new_i(mrb, a, b);
    }

#ifdef RAT_BIGINT
  case MRB_TT_BIGINT:
    return rat_sub_b(mrb, x, y);
#endif

#if defined(MRB_USE_COMPLEX)
  case MRB_TT_COMPLEX:
    return mrb_complex_sub(mrb, mrb_complex_new(mrb, rat_float(mrb, x), 0), y);
#endif

#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
  default:
    {
      mrb_float z = p1->numerator - mrb_as_float(mrb, y) * p1->denominator;
      return mrb_float_value(mrb, mrb_div_float(z, (mrb_float)p1->denominator));
    }
#else
  default:
    rat_type_error(mrb, y);
#endif
  }
}

static mrb_value
rational_sub(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_rational_sub(mrb, x, y);
}

#ifdef RAT_BIGINT
static mrb_value
rat_mul_b(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mrb_value num, den;

  switch(mrb_type(y)) {
  case MRB_TT_RATIONAL:
    num = rat_numerator(mrb, y);
    den = rat_denominator(mrb, y);
    break;
  case MRB_TT_INTEGER:
  case MRB_TT_BIGINT:
    num = y;
    den = ONE;
    break;
  default:
    /* should not happen */
    rat_type_error(mrb, y);
  }

  mrb_value a = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, rat_numerator(mrb, x)), num);
  mrb_value b = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, rat_denominator(mrb, x)), den);
  return rational_new_b(mrb, a, b);
}
#endif

mrb_value
mrb_rational_mul(mrb_state *mrb, mrb_value x, mrb_value y)
{
  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
#ifdef RAT_BIGINT
    if (RAT_BIGINT_P(x)) return rat_mul_b(mrb, x, y);
#endif
    {
      struct mrb_rational *p1 = rat_ptr(mrb, x);
      mrb_int z = mrb_integer(y);
      if (mrb_int_mul_overflow(p1->numerator, z, &z)) rat_overflow(mrb);
      return rational_new_i(mrb, z, p1->denominator);
    }
  case MRB_TT_RATIONAL:
#ifdef RAT_BIGINT
    if (RAT_BIGINT_P(x) || RAT_BIGINT_P(y))
      return rat_mul_b(mrb, x, y);
#endif
    {
      struct mrb_rational *p1 = rat_ptr(mrb, x);
      struct mrb_rational *p2 = rat_ptr(mrb, y);
      mrb_int a, b;

      if (mrb_int_mul_overflow(p1->numerator, p2->numerator, &a)) rat_overflow(mrb);
      if (mrb_int_mul_overflow(p1->denominator, p2->denominator, &b)) rat_overflow(mrb);
      return rational_new_i(mrb, a, b);
    }

#ifdef RAT_BIGINT
  case MRB_TT_BIGINT:
    return rat_mul_b(mrb, x, y);
#endif

#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
    {
      struct mrb_rational *p1 = rat_ptr(mrb, x);
      mrb_float z = p1->numerator * mrb_float(y);
      return mrb_float_value(mrb, mrb_div_float(z, (mrb_float)p1->denominator));
  }
#endif

#if defined(MRB_USE_COMPLEX)
  case MRB_TT_COMPLEX:
    return mrb_complex_mul(mrb, mrb_complex_new(mrb, rat_float(mrb, x), 0), y);
#endif

  default:
    return mrb_funcall_argv(mrb, y, MRB_OPSYM(mul), 1, &x);
  }
}

static mrb_value
rational_mul(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_rational_mul(mrb, x, y);
}

#ifdef RAT_BIGINT
static mrb_value
rat_div_b(mrb_state *mrb, mrb_value x, mrb_value y)
{
  mrb_value num, den;

  switch(mrb_type(y)) {
  case MRB_TT_RATIONAL:
    num = rat_numerator(mrb, y);
    den = rat_denominator(mrb, y);
    break;
  case MRB_TT_INTEGER:
#ifdef MRB_USE_BIGINT
  case MRB_TT_BIGINT:
#endif
    num = y;
    den = ONE;
    break;
  default:
    /* should not happen */
    rat_type_error(mrb, y);
  }

  mrb_value a = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, rat_numerator(mrb, x)), den);
  mrb_value b = mrb_bint_mul_n(mrb, mrb_as_bint(mrb, rat_denominator(mrb, x)), num);
  return rational_new_b(mrb, a, b);
}
#endif

mrb_value
mrb_rational_div(mrb_state *mrb, mrb_value x, mrb_value y)
{
  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
#ifdef RAT_BIGINT
    if (RAT_BIGINT_P(x)) return rat_div_b(mrb, x, y);
#endif
    {
      struct mrb_rational *p1 = rat_ptr(mrb, x);
      mrb_int z = mrb_integer(y);
      if (z == 0) mrb_int_zerodiv(mrb);
      if (mrb_int_mul_overflow(p1->denominator, z, &z)) rat_overflow(mrb);
      return rational_new_i(mrb, p1->numerator, z);
    }
  case MRB_TT_RATIONAL:
#ifdef RAT_BIGINT
    if (RAT_BIGINT_P(x) || RAT_BIGINT_P(y)) return rat_div_b(mrb, x, y);
#endif
    {
      struct mrb_rational *p1 = rat_ptr(mrb, x);
      struct mrb_rational *p2 = rat_ptr(mrb, y);
      mrb_int a, b;

      if (mrb_int_mul_overflow(p1->numerator, p2->denominator, &a)) rat_overflow(mrb);
      if (mrb_int_mul_overflow(p2->numerator, p1->denominator, &b)) rat_overflow(mrb);
      return rational_new_i(mrb, a, b);
    }

#ifdef RAT_BIGINT
  case MRB_TT_BIGINT:
    return rat_div_b(mrb, x, y);
#endif

#ifdef MRB_USE_COMPLEX
  case MRB_TT_COMPLEX:
    return mrb_complex_div(mrb, mrb_complex_new(mrb, rat_float(mrb, x), 0), y);
#endif

#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
    {
      struct mrb_rational *p1 = rat_ptr(mrb, x);
      mrb_float z = mrb_div_float((mrb_float)p1->numerator, mrb_as_float(mrb, y));
      return mrb_float_value(mrb, mrb_div_float(z, (mrb_float)p1->denominator));
    }
#endif

  default:
    rat_type_error(mrb, y);
    /* not reached */
    return mrb_nil_value();
  }
}

static mrb_value
rational_div(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_rational_div(mrb, x, y);
}

mrb_value mrb_int_pow(mrb_state *mrb, mrb_value x, mrb_value y);

static mrb_value
rational_pow(mrb_state *mrb, mrb_value x)
{
#ifndef MRB_NO_FLOAT
  mrb_value y = mrb_get_arg1(mrb);
  double d1 = rat_float(mrb, x);
  double d2 = mrb_as_float(mrb, y);

  d1 = pow(d1, d2);
  switch (mrb_type(y)) {
  case MRB_TT_FLOAT:
    return mrb_float_value(mrb, d1);
  case MRB_TT_INTEGER:
  case MRB_TT_RATIONAL:
    return rational_new_f(mrb, d1);
  case MRB_TT_BIGINT:
  default:
    return mrb_float_value(mrb, d1);
  }
#else
  mrb_raisef(mrb, E_NOTIMP_ERROR, "Rational#** not implemented with MRB_NO_FLOAT");
  /* not reached */
  return mrb_nil_value();
#endif
}

static mrb_value
rational_hash(mrb_state *mrb, mrb_value rat)
{
  struct mrb_rational *r = rat_ptr(mrb, rat);
  uint32_t hash;

#ifdef RAT_BIGINT
  if (RAT_BIGINT_P(rat)) {
    mrb_value tmp = mrb_bint_hash(mrb, mrb_obj_value(r->b.num));
    hash = (uint32_t)mrb_integer(tmp);
    tmp = mrb_bint_hash(mrb, mrb_obj_value(r->b.den));
    hash ^= (uint32_t)mrb_integer(tmp);
    return mrb_int_value(mrb, hash);
  }
#endif
  hash = mrb_byte_hash((uint8_t*)&r->numerator, sizeof(mrb_int));
  hash = mrb_byte_hash_step((uint8_t*)&r->denominator, sizeof(mrb_int), hash);
  return mrb_int_value(mrb, hash);
}

void mrb_mruby_rational_gem_init(mrb_state *mrb)
{
  struct RClass *rat = mrb_define_class_id(mrb, MRB_SYM(Rational), mrb_class_get_id(mrb, MRB_SYM(Numeric)));
  MRB_SET_INSTANCE_TT(rat, MRB_TT_RATIONAL);
  MRB_UNDEF_ALLOCATOR(rat);
  mrb_undef_class_method_id(mrb, rat, MRB_SYM(new));
  mrb_define_method_id(mrb, rat, MRB_SYM(numerator), rational_numerator, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, rat, MRB_SYM(denominator), rational_denominator, MRB_ARGS_NONE());
#ifndef MRB_NO_FLOAT
  mrb_define_method_id(mrb, rat, MRB_SYM(to_f), mrb_rational_to_f, MRB_ARGS_NONE());
#endif
  mrb_define_method_id(mrb, rat, MRB_SYM(to_i), mrb_rational_to_i, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, rat, MRB_SYM(to_r), mrb_obj_itself, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, rat, MRB_SYM_Q(negative), rational_negative_p, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, rat, MRB_OPSYM(eq), rational_eq, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, rat, MRB_OPSYM(minus), rational_minus, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, rat, MRB_OPSYM(add), rational_add, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, rat, MRB_OPSYM(sub), rational_sub, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, rat, MRB_OPSYM(mul), rational_mul, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, rat, MRB_OPSYM(div), rational_div, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, rat, MRB_SYM(quo), rational_div, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, rat, MRB_OPSYM(pow), rational_pow, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, rat, MRB_SYM(hash), rational_hash, MRB_ARGS_NONE());
#ifndef MRB_NO_FLOAT
  mrb_define_method_id(mrb, mrb->float_class, MRB_SYM(to_r), float_to_r, MRB_ARGS_NONE());
#endif
  mrb_define_method_id(mrb, mrb->integer_class, MRB_SYM(to_r), int_to_r, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, mrb->nil_class, MRB_SYM(to_r), nil_to_r, MRB_ARGS_NONE());
  mrb_define_private_method_id(mrb, mrb->kernel_module, MRB_SYM(Rational), rational_m, MRB_ARGS_ARG(1,1));
}

void
mrb_mruby_rational_gem_final(mrb_state* mrb)
{
}
