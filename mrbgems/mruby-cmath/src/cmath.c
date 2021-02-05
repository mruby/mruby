/*
** cmath.c - Math module with complex numbers
**
** See Copyright Notice in mruby.h
*/

/*
** This `mruby-cmath` gem uses C99 _Complex features
** You need C compler that support C99+
*/

#include <mruby.h>

#ifdef MRB_NO_FLOAT
# error CMath conflicts with 'MRB_NO_FLOAT' configuration
#endif

#include <math.h>
#include <complex.h>

mrb_value mrb_complex_new(mrb_state *mrb, mrb_float real, mrb_float imag);
void mrb_complex_get(mrb_state *mrb, mrb_value cpx, mrb_float*, mrb_float*);

static mrb_bool
cmath_get_complex(mrb_state *mrb, mrb_value c, mrb_float *r, mrb_float *i)
{
  if (mrb_integer_p(c)) {
    *r = (mrb_float)mrb_integer(c);
    *i = 0;
    return FALSE;
  }
  else if (mrb_float_p(c)) {
    *r = mrb_float(c);
    *i = 0;
    return FALSE;
  }
  else if (mrb_obj_is_kind_of(mrb, c, mrb_class_get(mrb, "Complex"))) {
    mrb_complex_get(mrb, c, r, i);
    return TRUE;
  }
  else {
    mrb_raise(mrb, E_TYPE_ERROR, "Numeric required");
    return FALSE;
  }
}

#ifdef MRB_USE_FLOAT32
#define F(x) x##f
#else
#endif
#define F(x) x

#ifdef _WIN32

#ifdef MRB_USE_FLOAT32
typedef _Fcomplex mrb_complex;
#define CX(r,i) _FCbuild(r,i)
#define CXMUL(x,y) _FCmulcr(x,y)
#else
typedef _Dcomplex mrb_complex;
#define CX(r,i) _Cbuild(r,i)
#define CXMUL(x,y) _Cmulcr(x,y)
#endif

static mrb_complex
CXDIVf(mrb_complex x, mrb_float y)
{
  return CX(creal(x)/y, cimag(x)/y);
}

static mrb_complex
CXDIVc(mrb_complex x, mrb_complex y)
{
  mrb_complex n=CXMUL(x, F(conj)(y));
  mrb_complex d=_CXMUL(y, F(conj)(y));

  return CX(creal(n)/creal(d), cimag(n)/cimag(d));
}

#else

#ifdef MRB_USE_FLOAT32
typedef float _Complex mrb_complex;
#else
typedef double _Complex mrb_complex;
#endif
    
#define CX(r,i) (r+i*I)
#define CXDIVf(x,y) (x)/(y)
#define CXDIVc(x,y) (x)/(y)
  
#endif

#define DEF_CMATH_METHOD(name) \
static mrb_value \
cmath_ ## name(mrb_state *mrb, mrb_value self)\
{\
  mrb_value z = mrb_get_arg1(mrb);\
  mrb_float real, imag;\
  if (cmath_get_complex(mrb, z, &real, &imag)) {\
    mrb_complex c = CX(real,imag);\
    c = F(c ## name)(c);\
    return mrb_complex_new(mrb, creal(c), cimag(c));\
  }\
  return mrb_float_value(mrb, F(name)(real));\
}

/* exp(z): return the exponential of z */
DEF_CMATH_METHOD(exp)

/* log(z): return the natural logarithm of z, with branch cut along the negative real axis */
static mrb_value
cmath_log(mrb_state *mrb, mrb_value self) {
  mrb_value z;
  mrb_float base;
  mrb_float real, imag;

  mrb_int n = mrb_get_args(mrb, "o|f", &z, &base);
  if (n == 1) base = M_E;
  if (cmath_get_complex(mrb, z, &real, &imag) || real < 0.0) {
    mrb_complex c = CX(real,imag);
    c = F(clog)(c);
    if (n == 2) c = CXDIVc(c, F(clog)(base));
    return mrb_complex_new(mrb, creal(c), cimag(c));
  }
  if (n == 1) return mrb_float_value(mrb, F(log)(real));
  return mrb_float_value(mrb, F(log)(real)/F(log)(base));
}

/* log10(z): return the base-10 logarithm of z, with branch cut along the negative real axis */
static mrb_value
cmath_log10(mrb_state *mrb, mrb_value self) {
  mrb_value z = mrb_get_arg1(mrb);
  mrb_float real, imag;
  if (cmath_get_complex(mrb, z, &real, &imag) || real < 0.0) {
    mrb_complex c = CX(real,imag);
    c = CXDIVf(F(clog)(c),log(10));
    return mrb_complex_new(mrb, creal(c), cimag(c));
  }
  return mrb_float_value(mrb, F(log10)(real));
}

/* log2(z): return the base-2 logarithm of z, with branch cut along the negative real axis */
static mrb_value
cmath_log2(mrb_state *mrb, mrb_value self) {
  mrb_value z = mrb_get_arg1(mrb);
  mrb_float real, imag;
  if (cmath_get_complex(mrb, z, &real, &imag) || real < 0.0) {
    mrb_complex c = CX(real,imag);
    c = CXDIVf(F(clog)(c),log(2));
    return mrb_complex_new(mrb, creal(c), cimag(c));
  }
  return mrb_float_value(mrb, F(log2)(real));
}

/* sqrt(z): return square root of z */
static mrb_value
cmath_sqrt(mrb_state *mrb, mrb_value self) {
  mrb_value z = mrb_get_arg1(mrb);
  mrb_float real, imag;
  if (cmath_get_complex(mrb, z, &real, &imag) || real < 0.0) {
    mrb_complex c = CX(real,imag);
    c = F(csqrt)(c);
    return mrb_complex_new(mrb, creal(c), cimag(c));
  }
  return mrb_float_value(mrb, F(sqrt)(real));
}

/* sin(z): sine function */
DEF_CMATH_METHOD(sin)
/* cos(z): cosine function */
DEF_CMATH_METHOD(cos)
/* tan(z): tangent function */
DEF_CMATH_METHOD(tan)
/* asin(z): arc sine function */
DEF_CMATH_METHOD(asin)
/* acos(z): arc cosine function */
DEF_CMATH_METHOD(acos)
/* atan(z): arg tangent function */
DEF_CMATH_METHOD(atan)
/* sinh(z): hyperbolic sine function */
DEF_CMATH_METHOD(sinh)
/* cosh(z): hyperbolic cosine function */
DEF_CMATH_METHOD(cosh)
/* tanh(z): hyperbolic tangent function */
DEF_CMATH_METHOD(tanh)
/* asinh(z): inverse hyperbolic sine function */
DEF_CMATH_METHOD(asinh)
/* acosh(z): inverse hyperbolic cosine function */
DEF_CMATH_METHOD(acosh)
/* atanh(z): inverse hyperbolic tangent function */
DEF_CMATH_METHOD(atanh)

/* ------------------------------------------------------------------------*/

void
mrb_mruby_cmath_gem_init(mrb_state* mrb)
{
  struct RClass *cmath;
  cmath = mrb_define_module(mrb, "CMath");

  mrb_include_module(mrb, cmath, mrb_module_get(mrb, "Math"));

  mrb_define_module_function(mrb, cmath, "sin", cmath_sin, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "cos", cmath_cos, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "tan", cmath_tan, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, cmath, "asin", cmath_asin, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "acos", cmath_acos, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "atan", cmath_atan, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, cmath, "sinh", cmath_sinh, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "cosh", cmath_cosh, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "tanh", cmath_tanh, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, cmath, "asinh", cmath_asinh, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "acosh", cmath_acosh, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "atanh", cmath_atanh, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, cmath, "exp", cmath_exp, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "log", cmath_log, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, cmath, "log2", cmath_log2, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "log10", cmath_log10, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, cmath, "sqrt", cmath_sqrt, MRB_ARGS_REQ(1));
}

void
mrb_mruby_cmath_gem_final(mrb_state* mrb)
{
}
