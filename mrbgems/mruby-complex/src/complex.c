#include <mruby.h>
#include <mruby/class.h>
#include <mruby/numeric.h>

struct mrb_complex {
  mrb_float real;
  mrb_float imaginary;
};

#if defined(MRB_64BIT) || defined(MRB_USE_FLOAT)

#define COMPLEX_USE_ISTRUCT
/* use TT_ISTRUCT */
#include <mruby/istruct.h>

#define complex_ptr(mrb, v) (struct mrb_complex*)mrb_istruct_ptr(v)

static mrb_value
complex_new(mrb_state *mrb, mrb_float real, mrb_float imaginary)
{
  struct RClass *c = mrb_class_get(mrb, "Complex");
  struct RIStruct *s = (struct RIStruct*)mrb_obj_alloc(mrb, MRB_TT_ISTRUCT, c);
  mrb_value comp = mrb_obj_value(s);
  struct mrb_complex *p = complex_ptr(mrb, comp);
  p->real = real;
  p->imaginary = imaginary;
  MRB_SET_FROZEN_FLAG(s);

  return comp;
}

#else
/* use TT_DATA */
#include <mruby/data.h>

static const struct mrb_data_type mrb_complex_type = {"Complex", mrb_free};

static mrb_value
complex_new(mrb_state *mrb, mrb_float real, mrb_float imaginary)
{
  struct RClass *c = mrb_class_get(mrb, "Complex");
  struct mrb_complex *p;

  p = (struct mrb_complex*)mrb_malloc(mrb, sizeof(struct mrb_complex));
  p->real = real;
  p->imaginary = imaginary;

  return mrb_obj_value(Data_Wrap_Struct(mrb, c, &mrb_complex_type, p));
}

static struct mrb_complex*
complex_ptr(mrb_state *mrb, mrb_value v)
{
  struct mrb_complex *p;

  p = DATA_GET_PTR(mrb, v, &mrb_complex_type, struct mrb_complex);
  if (!p) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized complex");
  }
  return p;
}
#endif

static mrb_value
complex_real(mrb_state *mrb, mrb_value self)
{
  struct mrb_complex *p = complex_ptr(mrb, self);
  return mrb_float_value(mrb, p->real);
}

static mrb_value
complex_imaginary(mrb_state *mrb, mrb_value self)
{
  struct mrb_complex *p = complex_ptr(mrb, self);
  return mrb_float_value(mrb, p->imaginary);
}

static mrb_value
complex_s_new(mrb_state *mrb, mrb_value self)
{
  mrb_float real, imaginary;

  mrb_get_args(mrb, "ff", &real, &imaginary);
  return complex_new(mrb, real, imaginary);
}

#ifndef MRB_WITHOUT_FLOAT
static mrb_value
complex_to_f(mrb_state *mrb, mrb_value self)
{
  struct mrb_complex *p = complex_ptr(mrb, self);

  if (p->imaginary != 0) {
    mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %S into Float", self);
  }

  return mrb_float_value(mrb, p->real);
}
#endif

static mrb_value
complex_to_i(mrb_state *mrb, mrb_value self)
{
  struct mrb_complex *p = complex_ptr(mrb, self);

  if (p->imaginary != 0) {
    mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %S into Float", self);
  }
  return mrb_int_value(mrb, p->real);
}

static mrb_value
complex_to_c(mrb_state *mrb, mrb_value self)
{
  return self;
}

void mrb_mruby_complex_gem_init(mrb_state *mrb)
{
  struct RClass *comp;

#ifdef COMPLEX_USE_ISTRUCT
  mrb_assert(sizeof(struct mrb_complex) < ISTRUCT_DATA_SIZE);
#endif
  comp = mrb_define_class(mrb, "Complex", mrb_class_get(mrb, "Numeric"));
  //MRB_SET_INSTANCE_TT(comp, MRB_TT_ISTRUCT);
  mrb_undef_class_method(mrb, comp, "new");
  mrb_define_class_method(mrb, comp, "_new", complex_s_new, MRB_ARGS_REQ(2));
  mrb_define_method(mrb, comp, "real", complex_real, MRB_ARGS_NONE());
  mrb_define_method(mrb, comp, "imaginary", complex_imaginary, MRB_ARGS_NONE());
#ifndef MRB_WITHOUT_FLOAT
  mrb_define_method(mrb, comp, "to_f", complex_to_f, MRB_ARGS_NONE());
#endif
  mrb_define_method(mrb, comp, "to_i", complex_to_i, MRB_ARGS_NONE());
  mrb_define_method(mrb, comp, "to_c", complex_to_c, MRB_ARGS_NONE());
}

void
mrb_mruby_complex_gem_final(mrb_state* mrb)
{
}
