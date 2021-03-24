#include <mruby.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/numeric.h>
#include <mruby/presym.h>

struct mrb_rational {
  mrb_int numerator;
  mrb_int denominator;
};

#include <mruby/istruct.h>

#if defined(MRB_INT64) && defined(MRB_32BIT)
struct RRational {
  MRB_OBJECT_HEADER;
  struct mrb_rational *p;
};

static struct mrb_rational*
rational_ptr(mrb_state *mrb, mrb_value v)
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
#define rational_ptr(mrb, v) (&((struct RRational*)mrb_obj_ptr(v))->r)
#endif

static struct RBasic*
rational_alloc(mrb_state *mrb, struct RClass *c, struct mrb_rational **p)
{
  struct RRational *s;
  s = (struct RRational*)mrb_obj_alloc(mrb, MRB_TT_RATIONAL, c);
#ifdef RATIONAL_INLINE
  *p = &s->r;
#else
  *p = s->p = (struct mrb_rational*)mrb_malloc(mrb, sizeof(struct mrb_rational));
#endif
  return (struct RBasic*)s;
}

static mrb_value
rational_numerator(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(mrb, self);
  return mrb_int_value(mrb, p->numerator);
}

static mrb_value
rational_denominator(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(mrb, self);
  return mrb_int_value(mrb, p->denominator);
}

static void
rat_overflow(mrb_state *mrb)
{
  mrb_raise(mrb, E_RANGE_ERROR, "integer overflow in rational");
}

static void
rat_zerodiv(mrb_state *mrb)
{
  mrb_raise(mrb, E_ZERODIV_ERROR, "divided by 0 in rational");
}

static mrb_value
rational_new(mrb_state *mrb, mrb_int numerator, mrb_int denominator)
{
  struct RClass *c = mrb_class_get_id(mrb, MRB_SYM(Rational));
  struct mrb_rational *p;
  struct RBasic *rat;

  if (denominator == 0) {
    rat_zerodiv(mrb);
  }
  if (denominator < 0) {
    if (numerator == MRB_INT_MIN || denominator == MRB_INT_MIN) {
      rat_overflow(mrb);
    }
    numerator *= -1;
    denominator *= -1;
  }
  rat = rational_alloc(mrb, c, &p);
  p->numerator = numerator;
  p->denominator = denominator;
  MRB_SET_FROZEN_FLAG(rat);
  return mrb_obj_value(rat);
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
  for (shift = 0; ((u | v) & 1) == 0; ++shift) {
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

static mrb_value
rational_new_i(mrb_state *mrb, mrb_int n, mrb_int d)
{
  mrb_int a;

  if (d == 0) {
    rat_zerodiv(mrb);
  }
  if (n == MRB_INT_MIN || d == MRB_INT_MIN) {
    rat_overflow(mrb);
  }
  a = i_gcd(n, d);
  return rational_new(mrb, n/a, d/a);
}

#ifndef MRB_NO_FLOAT
#include <math.h>

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

static void
float_decode_internal(mrb_state *mrb, mrb_float f, mrb_float *rf, int *n)
{
  f = (mrb_float)frexp_rat(f, n);
  if (isinf(f)) rat_overflow(mrb);
  f = (mrb_float)ldexp_rat(f, RAT_MANT_DIG);
  *n -= RAT_MANT_DIG;
  *rf = f;
}

void mrb_check_num_exact(mrb_state *mrb, mrb_float num);

static mrb_value
rational_new_f(mrb_state *mrb, mrb_float f0)
{
  mrb_float f;
  int n;

  mrb_check_num_exact(mrb, f0);
  float_decode_internal(mrb, f0, &f, &n);
#if FLT_RADIX == 2
  if (n == 0)
    return rational_new(mrb, (mrb_int)f, 1);
  if (n > 0) {
    f = ldexp_rat(f, n);
    if (f == RAT_HUGE_VAL || f > (mrb_float)MRB_INT_MAX) {
      rat_overflow(mrb);
    }
    return rational_new(mrb, (mrb_uint)f, 1);
  }
  if (n < -RAT_INT_LIMIT) {
    f = ldexp_rat(f, n+RAT_INT_LIMIT);
    n = RAT_INT_LIMIT;
  }
  else {
    n = -n;
  }
  return rational_new_i(mrb, (mrb_int)f, ((mrb_int)1)<<n);
#else
  mrb_int pow = 1;
  if (n < 0) {
    n = -n;
    while (n > RAT_INT_LIMIT) {
      f /= 2;
      n--;
    }
    while (n--) {
      pow *= FLT_RADIX;
    }
    return rational_new_i(mrb, f, pow);
  }
  else {
    while (n--) {
      if (MRB_INT_MAX/FLT_RADIX < pow) {
        rat_overflow(mrb);
      }
      pow *= FLT_RADIX;
    }
    return rational_new(mrb, (mrb_int)f*pow, 1);
  }
#endif
}
#endif

static mrb_value
rational_s_new(mrb_state *mrb, mrb_value self)
{
  mrb_int numerator, denominator;

#ifdef MRB_NO_FLOAT
  mrb_get_args(mrb, "ii", &numerator, &denominator);
#else

 mrb_value numv, denomv;

  mrb_get_args(mrb, "oo", &numv, &denomv);
  if (mrb_integer_p(numv)) {
    numerator = mrb_integer(numv);

    if (mrb_integer_p(denomv)) {
      denominator = mrb_integer(denomv);
    }
    else {
      mrb_float numf = (mrb_float)numerator;
      mrb_float denomf = mrb_to_flo(mrb, denomv);

      return rational_new_f(mrb, numf/denomf);
    }
  }
  else {
    mrb_float numf = mrb_to_flo(mrb, numv);
    mrb_float denomf;

    if (mrb_integer_p(denomv)) {
      denomf = (mrb_float)mrb_integer(denomv);
    }
    else {
      denomf = mrb_to_flo(mrb, denomv);
    }
    return rational_new_f(mrb, numf/denomf);
  }
#endif
  return rational_new(mrb, numerator, denominator);
}

#ifndef MRB_NO_FLOAT
static mrb_value
rational_to_f(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(mrb, self);
  mrb_float f;

  if (p->denominator == 0.0) {
    f = INFINITY;
  }
  else {
    f = (mrb_float)p->numerator / (mrb_float)p->denominator;
  }

  return mrb_float_value(mrb, f);
}
#endif

static mrb_value
rational_to_i(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(mrb, self);
  if (p->denominator == 0) {
    rat_zerodiv(mrb);
  }
  return mrb_int_value(mrb, p->numerator / p->denominator);
}

static mrb_value
rational_to_r(mrb_state *mrb, mrb_value self)
{
  return self;
}

static mrb_value
rational_negative_p(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(mrb, self);
  if (p->numerator < 0) {
    return mrb_true_value();
  }
  return mrb_false_value();
}

static mrb_value
fix_to_r(mrb_state *mrb, mrb_value self)
{
  return rational_new(mrb, mrb_integer(self), 1);
}

static mrb_value
rational_m(mrb_state *mrb, mrb_value self)
{
#ifdef MRB_NO_FLOAT
  mrb_int n, d = 1;
  mrb_get_args(mrb, "i|i", &n, &d);
  return rational_new_i(mrb, n, d);
#else
  mrb_value a, b = mrb_fixnum_value(1);
  mrb_get_args(mrb, "o|o", &a, &b);
  if (mrb_integer_p(a) && mrb_integer_p(b)) {
    return rational_new_i(mrb, mrb_integer(a), mrb_integer(b));
  }
  else {
    mrb_float x = mrb_to_flo(mrb, a);
    mrb_float y = mrb_to_flo(mrb, b);
    return rational_new_f(mrb, x/y);
  }
#endif
}

mrb_bool
mrb_rational_eq(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct mrb_rational *p1 = rational_ptr(mrb, x);

  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
    if (p1->denominator != 1) return FALSE;
    return p1->numerator == mrb_integer(y);
#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
    return ((double)p1->numerator/p1->denominator) == mrb_float(y);
#endif
  case MRB_TT_RATIONAL:
    {
      struct mrb_rational *p2 = rational_ptr(mrb, y);
      mrb_int a, b;

      if (p1->numerator == p2->numerator && p1->denominator == p2->denominator) {
        return TRUE;
      }
      if (mrb_int_mul_overflow(p1->numerator, p2->denominator, &a) ||
          mrb_int_mul_overflow(p2->numerator, p1->denominator, &b)) {
#ifdef MRB_NO_FLOAT
        rat_overflow(mrb);
#else
        return (double)p1->numerator*p2->denominator == (double)p2->numerator*p2->denominator;
#endif
      }
      return a == b;
    }

#ifdef MRB_USE_COMPLEX
  case MRB_TT_COMPLEX:
   {
      mrb_bool mrb_complex_eq(mrb_state *mrb, mrb_value, mrb_value);
      return mrb_complex_eq(mrb, y, x);
    }
#endif
  default:
    return mrb_equal(mrb, y, x);
  }
}

static mrb_value
rational_eq(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_bool_value(mrb_rational_eq(mrb, x, y));
}

static mrb_value
rational_minus(mrb_state *mrb, mrb_value x)
{
  struct mrb_rational *p = rational_ptr(mrb, x);
  mrb_int n = p->numerator;
  if (n == MRB_INT_MIN) rat_overflow(mrb);
  return rational_new(mrb, -n, p->denominator);
}

#ifndef MRB_NO_FLOAT
mrb_float mrb_num_div_flo(mrb_state*, mrb_float, mrb_float);
#endif

static mrb_value
rational_add(mrb_state *mrb, mrb_value x)
{
  struct mrb_rational *p1 = rational_ptr(mrb, x);
  mrb_value y = mrb_get_arg1(mrb);

  switch (mrb_type(y)) {
  case MRB_TT_INTEGER:
    {
      mrb_int z = mrb_integer(y);
      if (mrb_int_mul_overflow(z, p1->denominator, &z)) rat_overflow(mrb);
      if (mrb_int_add_overflow(p1->numerator, z, &z)) rat_overflow(mrb);
      return rational_new_i(mrb, z, p1->denominator);
    }
  case MRB_TT_RATIONAL:
    {
      struct mrb_rational *p2 = rational_ptr(mrb, y);
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
      mrb_float z = p1->numerator + mrb_to_flo(mrb, y) * p1->denominator;
      return mrb_float_value(mrb, mrb_num_div_flo(mrb, z, (mrb_float)p1->denominator));
    }
#endif

  default:
    return mrb_funcall_id(mrb, y, MRB_OPSYM(add), 1, x);
  }
}

mrb_int mrb_num_div_int(mrb_state*, mrb_int, mrb_int);
mrb_value mrb_complex_new(mrb_state*, mrb_float, mrb_float);

/* 15.2.8.3.4  */
/*
 * redefine Integer#/
 */
static mrb_value
int_div(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  mrb_int a = mrb_integer(x);

  if (mrb_integer_p(y)) {
    mrb_int div = mrb_num_div_int(mrb, a, mrb_integer(y));
    return mrb_int_value(mrb, div);
  }
  switch (mrb_type(y)) {
  case MRB_TT_RATIONAL:
    x = rational_new(mrb, a, 1);
    return mrb_funcall_id(mrb, x, MRB_OPSYM(div), 1, y);
#if defined(MRB_USE_COMPLEX)
  case MRB_TT_COMPLEX:
    x = mrb_complex_new(mrb, (mrb_float)a, 0);
    return mrb_funcall_id(mrb, x, MRB_OPSYM(div), 1, y);
#endif
  case MRB_TT_FLOAT:
  default:
#ifdef MRB_NO_FLOAT
    mrb_raise(mrb, E_TYPE_ERROR, "non integer multiplication");
#else
    return mrb_float_value(mrb, mrb_num_div_flo(mrb, (mrb_float)a, mrb_to_flo(mrb, y)));
#endif
  }
}

/* 15.2.9.3.19(x) */
/*
 * redefine Integer#quo
 */

static mrb_value
int_quo(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  mrb_int a = mrb_integer(x);

  if (mrb_integer_p(y)) {
    return rational_new(mrb, a, mrb_integer(y));
  }
  switch (mrb_type(y)) {
  case MRB_TT_RATIONAL:
    x = rational_new(mrb, a, 1);
    return mrb_funcall_id(mrb, x, MRB_OPSYM(div), 1, y);
#if defined(MRB_USE_COMPLEX)
  case MRB_TT_COMPLEX:
    x = mrb_complex_new(mrb, (mrb_float)a, 0);
    return mrb_funcall_id(mrb, x, MRB_OPSYM(div), 1, y);
#endif
  default:
#ifdef MRB_NO_FLOAT
    mrb_raise(mrb, E_TYPE_ERROR, "non integer multiplication");
#else
    return mrb_float_value(mrb, mrb_num_div_flo(mrb, (mrb_float)a, mrb_to_flo(mrb, y)));
#endif
  }
}

void mrb_mruby_rational_gem_init(mrb_state *mrb)
{
  struct RClass *rat;

  rat = mrb_define_class_id(mrb, MRB_SYM(Rational), mrb_class_get_id(mrb, MRB_SYM(Numeric)));
  MRB_SET_INSTANCE_TT(rat, MRB_TT_RATIONAL);
  mrb_undef_class_method(mrb, rat, "new");
  mrb_define_class_method(mrb, rat, "_new", rational_s_new, MRB_ARGS_REQ(2));
  mrb_define_method(mrb, rat, "numerator", rational_numerator, MRB_ARGS_NONE());
  mrb_define_method(mrb, rat, "denominator", rational_denominator, MRB_ARGS_NONE());
#ifndef MRB_NO_FLOAT
  mrb_define_method(mrb, rat, "to_f", rational_to_f, MRB_ARGS_NONE());
#endif
  mrb_define_method(mrb, rat, "to_i", rational_to_i, MRB_ARGS_NONE());
  mrb_define_method(mrb, rat, "to_r", rational_to_r, MRB_ARGS_NONE());
  mrb_define_method(mrb, rat, "negative?", rational_negative_p, MRB_ARGS_NONE());
  mrb_define_method(mrb, rat, "==", rational_eq, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, rat, "-@", rational_minus, MRB_ARGS_NONE());
  mrb_define_method(mrb, rat, "+", rational_add, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, mrb->integer_class, "to_r", fix_to_r, MRB_ARGS_NONE());
  mrb_define_method(mrb, mrb->integer_class, "/", int_div, MRB_ARGS_REQ(1)); /* overrride */
  mrb_define_method(mrb, mrb->integer_class, "quo", int_quo, MRB_ARGS_REQ(1)); /* overrride */
  mrb_define_method(mrb, mrb->kernel_module, "Rational", rational_m, MRB_ARGS_ARG(1,1));
}

void
mrb_mruby_rational_gem_final(mrb_state* mrb)
{
}
