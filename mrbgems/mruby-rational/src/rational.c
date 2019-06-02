#include <mruby.h>
#include <mruby/class.h>
#include <mruby/string.h>

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
  return mrb_fixnum_value(p->numerator);
}

static mrb_value
rational_denominator(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(mrb, self);
  return mrb_fixnum_value(p->denominator);
}

static mrb_value
rational_new(mrb_state *mrb, mrb_int numerator, mrb_int denominator)
{
  struct RClass *c = mrb_class_get(mrb, "Rational");
  struct mrb_rational *p;
  struct RBasic *rat = rational_alloc(mrb, c, &p);
  p->numerator = numerator;
  p->denominator = denominator;
  MRB_SET_FROZEN_FLAG(rat);
  return mrb_obj_value(rat);
}

static mrb_value
rational_s_new(mrb_state *mrb, mrb_value self)
{
  mrb_int numerator, denominator;

  mrb_get_args(mrb, "ii", &numerator, &denominator);
  return rational_new(mrb, numerator, denominator);
}

#ifndef MRB_WITHOUT_FLOAT
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
  return mrb_fixnum_value(p->numerator / p->denominator);
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
  return rational_new(mrb, mrb_fixnum(self), 1);
}

void mrb_mruby_rational_gem_init(mrb_state *mrb)
{
  struct RClass *rat;

#ifdef COMPLEX_USE_RATIONAL
  mrb_assert(sizeof(struct mrb_rational) < ISTRUCT_DATA_SIZE);
#endif
  rat = mrb_define_class(mrb, "Rational", mrb_class_get(mrb, "Numeric"));
  mrb_undef_class_method(mrb, rat, "new");
  mrb_define_class_method(mrb, rat, "_new", rational_s_new, MRB_ARGS_REQ(2));
  mrb_define_method(mrb, rat, "numerator", rational_numerator, MRB_ARGS_NONE());
  mrb_define_method(mrb, rat, "denominator", rational_denominator, MRB_ARGS_NONE());
#ifndef MRB_WITHOUT_FLOAT
  mrb_define_method(mrb, rat, "to_f", rational_to_f, MRB_ARGS_NONE());
#endif
  mrb_define_method(mrb, rat, "to_i", rational_to_i, MRB_ARGS_NONE());
  mrb_define_method(mrb, rat, "to_r", rational_to_r, MRB_ARGS_NONE());
  mrb_define_method(mrb, rat, "negative?", rational_negative_p, MRB_ARGS_NONE());
  mrb_define_method(mrb, mrb->fixnum_class, "to_r", fix_to_r, MRB_ARGS_NONE());
}

void
mrb_mruby_rational_gem_final(mrb_state* mrb)
{
}
