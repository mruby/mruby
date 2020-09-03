#include <mruby.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/numeric.h>

struct mrb_rational {
  mrb_int numerator;
  mrb_int denominator;
};

#if MRB_INT_MAX <= INTPTR_MAX

#define RATIONAL_USE_ISTRUCT
/* use TT_ISTRUCT */
#include <mruby/istruct.h>

#define rational_ptr(mrb, v) (struct mrb_rational*)mrb_istruct_ptr(v)

static struct RBasic*
rational_alloc(mrb_state *mrb, struct RClass *c, struct mrb_rational **p)
{
  struct RIStruct *s;

  s = (struct RIStruct*)mrb_obj_alloc(mrb, MRB_TT_ISTRUCT, c);
  *p = (struct mrb_rational*)s->inline_data;

  return (struct RBasic*)s;
}

#else
/* use TT_DATA */
#include <mruby/data.h>

static const struct mrb_data_type mrb_rational_type = {"Rational", mrb_free};

static struct RBasic*
rational_alloc(mrb_state *mrb, struct RClass *c, struct mrb_rational **p)
{
  struct RData *d;

  Data_Make_Struct(mrb, c, struct mrb_rational, &mrb_rational_type, *p, d);

  return (struct RBasic*)d;
}

static struct mrb_rational*
rational_ptr(mrb_state *mrb, mrb_value v)
{
  struct mrb_rational *p;

  p = DATA_GET_PTR(mrb, v, &mrb_rational_type, struct mrb_rational);
  if (!p) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized rational");
  }
  return p;
}
#endif

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

static mrb_value
rational_new(mrb_state *mrb, mrb_int numerator, mrb_int denominator)
{
  struct RClass *c = mrb_class_get_id(mrb, MRB_SYM(Rational));
  struct mrb_rational *p;
  struct RBasic *rat = rational_alloc(mrb, c, &p);
  if (denominator < 0) {
    numerator *= -1;
    denominator *= -1;
  }
  p->numerator = numerator;
  p->denominator = denominator;
  MRB_SET_FROZEN_FLAG(rat);
  return mrb_obj_value(rat);
}

#ifndef MRB_NO_FLOAT
#include <math.h>
/* f : number to convert.
 * num, denom: returned parts of the rational.
 * md: max denominator value.  Note that machine floating point number
 *     has a finite resolution (10e-16 ish for 64 bit double), so specifying
 *     a "best match with minimal error" is often wrong, because one can
 *     always just retrieve the significand and return that divided by 
 *     2**52, which is in a sense accurate, but generally not very useful:
 *     1.0/7.0 would be "2573485501354569/18014398509481984", for example.
 */
#ifdef MRB_INT32
typedef float rat_float;
#else
typedef double rat_float;
#endif
static mrb_value
rational_new_f(mrb_state *mrb, mrb_float f0)
{
  rat_float f = (rat_float)f0;
  mrb_int md = 1000000;
  /*  a: continued fraction coefficients. */
  mrb_int a, h[3] = { 0, 1, 0 }, k[3] = { 1, 0, 0 };
  mrb_int x, d;
  int64_t n = 1;
  int i, neg = 0;

  if (f < 0) { neg = 1; f = -f; }
  while (f != floor(f)) { n <<= 1; f *= 2; }
  d = f;
 
  /* continued fraction and check denominator each step */
  for (i = 0; i < 64; i++) {
    a = n ? d / n : 0;
    if (i && !a) break;
 
    x = d; d = n; n = x % n;
 
    x = a;
    if (k[1] * a + k[0] >= md) {
      x = (md - k[0]) / k[1];
      if (x * 2 >= a || k[1] >= md)
        i = 65;
      else
        break;
    }

    h[2] = x * h[1] + h[0]; h[0] = h[1]; h[1] = h[2];
    k[2] = x * k[1] + k[0]; k[0] = k[1]; k[1] = k[2];
  }
  return rational_new(mrb, (neg ? -h[1] : h[1]), k[1]);
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
  mrb_float f = (mrb_float)p->numerator / (mrb_float)p->denominator;

  return mrb_float_value(mrb, f);
}
#endif

static mrb_value
rational_to_i(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(mrb, self);
  if (p->denominator == 0) {
    mrb_raise(mrb, mrb->eStandardError_class, "divided by 0");
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
rational_m_int(mrb_state *mrb, mrb_int n, mrb_int d)
{
  mrb_int a, b;

  a = n;
  b = d;
  while (b != 0) {
    mrb_int tmp = b;
    b = a % b;
    a = tmp;
  }
  return rational_new(mrb, n/a, d/a);
}

static mrb_value
rational_m(mrb_state *mrb, mrb_value self)
{
#ifdef MRB_NO_FLOAT
  mrb_int n, d = 1;
  mrb_get_args(mrb, "i|i", &n, &d);
  return rational_m_int(mrb, n, d);
#else
  mrb_value a, b = mrb_fixnum_value(1);
  mrb_get_args(mrb, "o|o", &a, &b);
  if (mrb_integer_p(a) && mrb_integer_p(b)) {
    return rational_m_int(mrb, mrb_integer(a), mrb_integer(b));
  }
  else {
    mrb_float x = mrb_to_flo(mrb, a);
    mrb_float y = mrb_to_flo(mrb, b);
    return rational_new_f(mrb, x/y);
  }
#endif
}

void mrb_mruby_rational_gem_init(mrb_state *mrb)
{
  struct RClass *rat;

  rat = mrb_define_class_id(mrb, MRB_SYM(Rational), mrb_class_get_id(mrb, MRB_SYM(Numeric)));
#ifdef RATIONAL_USE_ISTRUCT
  MRB_SET_INSTANCE_TT(rat, MRB_TT_ISTRUCT);
  mrb_assert(sizeof(struct mrb_rational) < ISTRUCT_DATA_SIZE);
#else
  MRB_SET_INSTANCE_TT(rat, MRB_TT_DATA);
#endif
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
  mrb_define_method(mrb, mrb->integer_class, "to_r", fix_to_r, MRB_ARGS_NONE());
  mrb_define_method(mrb, mrb->kernel_module, "Rational", rational_m, MRB_ARGS_ARG(1,1));
}

void
mrb_mruby_rational_gem_final(mrb_state* mrb)
{
}
