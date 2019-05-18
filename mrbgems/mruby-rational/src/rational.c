#include <mruby.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/istruct.h>

struct mrb_rational {
  mrb_int numerator;
  mrb_int denominator;
};

static struct mrb_rational*
rational_ptr(mrb_value v)
{
  return (struct mrb_rational*)mrb_istruct_ptr(v);
}

static mrb_value
rational_numerator(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(self);
  return mrb_fixnum_value(p->numerator);
}

static mrb_value
rational_denominator(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(self);
  return mrb_fixnum_value(p->denominator);
}

static mrb_value
rational_new(mrb_state *mrb, mrb_int numerator, mrb_int denominator)
{
  struct RClass *c = mrb_class_get(mrb, "Rational");
  struct RIStruct *s = (struct RIStruct*)mrb_obj_alloc(mrb, MRB_TT_ISTRUCT, c);
  mrb_value rat = mrb_obj_value(s);
  struct mrb_rational *p = rational_ptr(rat);
  p->numerator = numerator;
  p->denominator = denominator;
  return mrb_obj_value(s);
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
  struct mrb_rational *p = rational_ptr(self);
  mrb_float f = (mrb_float)p->numerator / (mrb_float)p->denominator;

  return mrb_float_value(mrb, f);
}
#endif

static mrb_value
rational_to_i(mrb_state *mrb, mrb_value self)
{
  struct mrb_rational *p = rational_ptr(self);
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
  struct mrb_rational *p = rational_ptr(self);
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

  mrb_assert(sizeof(struct mrb_rational) < ISTRUCT_DATA_SIZE);
  rat = mrb_define_class(mrb, "Rational", mrb_class_get(mrb, "Numeric"));
  MRB_SET_INSTANCE_TT(rat, MRB_TT_ISTRUCT);
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
