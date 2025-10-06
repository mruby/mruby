/*
** cmath.c - Math module with complex numbers
**
** See Copyright Notice in mruby.h
*/

/*
** This `mruby-cmath` gem uses C99 _Complex features
** You need C compiler that support C99+
*/

#include <mruby.h>
#include <mruby/presym.h>

#ifdef MRB_NO_FLOAT
# error CMath conflicts with 'MRB_NO_FLOAT' configuration
#endif

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
  else if (mrb_type(c) == MRB_TT_COMPLEX) {
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
#define F(x) x
#endif

#if defined(_WIN32) && !defined(__MINGW32__)

#ifdef MRB_USE_FLOAT32
typedef _Fcomplex mrb_complex;
#define CX(r,i) _FCbuild(r,i)
#else
typedef _Dcomplex mrb_complex;
#define CX(r,i) _Cbuild(r,i)
#endif

static mrb_complex
CXDIVf(mrb_complex x, mrb_float y)
{
  return CX(creal(x)/y, cimag(x)/y);
}

static mrb_complex
CXDIVc(mrb_complex a, mrb_complex b)
{
  mrb_float ratio, den;
  mrb_float abr, abi, cr, ci;

  if ((abr = creal(b)) < 0)
    abr = - abr;
  if ((abi = cimag(b)) < 0)
    abi = - abi;
  if (abr <= abi) {
    ratio = creal(b) / cimag(b);
    den = cimag(a) * (1 + ratio*ratio);
    cr = (creal(a)*ratio + cimag(a)) / den;
    ci = (cimag(a)*ratio - creal(a)) / den;
  }
  else {
    ratio = cimag(b) / creal(b);
    den = creal(a) * (1 + ratio*ratio);
    cr = (creal(a) + cimag(a)*ratio) / den;
    ci = (cimag(a) - creal(a)*ratio) / den;
  }
  return CX(cr, ci);
}

#else

#if defined(__cplusplus) && \
    (defined(__APPLE__) || defined(__EMSCRIPTEN__) || \
     (defined(__clang__) && (defined(__FreeBSD__) || defined(__OpenBSD__))))

#ifdef MRB_USE_FLOAT32
typedef std::complex<float> mrb_complex;
#else
typedef std::complex<double> mrb_complex;
#endif  /* MRB_USE_FLOAT32 */

#define CX(r,i) mrb_complex(r,i)
#define creal(c) c.real()
#define cimag(c) c.imag()
#define FC(n) F(n)

#else  /* cpp */

#ifdef MRB_USE_FLOAT32
typedef float _Complex mrb_complex;
#else
typedef double _Complex mrb_complex;
#endif  /*  MRB_USE_FLOAT32 */

#define CX(r,i) ((r)+(i)*_Complex_I)
#endif

#define CXDIVf(x,y) (x)/(y)
#define CXDIVc(x,y) (x)/(y)

#endif

#ifndef FC
#define FC(n) F(c ## n)
#endif

#define DEF_CMATH_METHOD(name) \
static mrb_value \
cmath_ ## name(mrb_state *mrb, mrb_value self)\
{\
  mrb_value z = mrb_get_arg1(mrb);\
  mrb_float real, imag;\
  if (cmath_get_complex(mrb, z, &real, &imag)) {\
    mrb_complex c = CX(real,imag);\
    c = FC(name)(c);\
    return mrb_complex_new(mrb, creal(c), cimag(c));\
  }\
  return mrb_float_value(mrb, F(name)(real));\
}

/*
 * call-seq:
 *   CMath.exp(z) -> numeric
 *
 * Returns the exponential of `z`.
 * If `z` is a complex number, returns a complex result.
 * If `z` is real and positive, returns a float.
 *
 *   CMath.exp(1)      #=> 2.718281828459045
 *   CMath.exp(1+1i)   #=> (1.4686939399158851+2.2873552871788423i)
 */
DEF_CMATH_METHOD(exp)

/*
 * call-seq:
 *   CMath.log(z)       -> numeric
 *   CMath.log(z, base) -> numeric
 *
 * Returns the natural logarithm of `z`.
 * If a second argument `base` is given, returns the logarithm of `z` to the given base.
 * Has a branch cut along the negative real axis.
 *
 *   CMath.log(1)      #=> 0.0
 *   CMath.log(-1)     #=> (0.0+3.141592653589793i)
 *   CMath.log(8, 2)   #=> 3.0
 */
static mrb_value
cmath_log(mrb_state *mrb, mrb_value self) {
  mrb_value z;
  mrb_float base;
  mrb_float real, imag;

  mrb_int n = mrb_get_args(mrb, "o|f", &z, &base);

#ifndef M_E
#define M_E F(exp)(1.0)
#endif

  if (n == 1) base = M_E;
  if (cmath_get_complex(mrb, z, &real, &imag) || real < 0.0) {
    mrb_complex c = CX(real,imag);
    c = FC(log)(c);
    if (n == 2) c = CXDIVc(c, FC(log)(CX(base,0)));
    return mrb_complex_new(mrb, creal(c), cimag(c));
  }
  if (n == 1) return mrb_float_value(mrb, F(log)(real));
  return mrb_float_value(mrb, F(log)(real)/F(log)(base));
}

/*
 * call-seq:
 *   CMath.log10(z) -> numeric
 *
 * Returns the base-10 logarithm of `z`.
 * Has a branch cut along the negative real axis.
 *
 *   CMath.log10(100)  #=> 2.0
 *   CMath.log10(-1)   #=> (0.0+1.3643763538418412i)
 */
static mrb_value
cmath_log10(mrb_state *mrb, mrb_value self) {
  mrb_value z = mrb_get_arg1(mrb);
  mrb_float real, imag;
  if (cmath_get_complex(mrb, z, &real, &imag) || real < 0.0) {
    mrb_complex c = CX(real,imag);
    c = CXDIVf(FC(log)(c),log(10));
    return mrb_complex_new(mrb, creal(c), cimag(c));
  }
  return mrb_float_value(mrb, F(log10)(real));
}

/*
 * call-seq:
 *   CMath.log2(z) -> numeric
 *
 * Returns the base-2 logarithm of `z`.
 * Has a branch cut along the negative real axis.
 *
 *   CMath.log2(8)     #=> 3.0
 *   CMath.log2(-1)    #=> (0.0+4.532360141827194i)
 */
static mrb_value
cmath_log2(mrb_state *mrb, mrb_value self) {
  mrb_value z = mrb_get_arg1(mrb);
  mrb_float real, imag;
  if (cmath_get_complex(mrb, z, &real, &imag) || real < 0.0) {
    mrb_complex c = CX(real,imag);
    c = CXDIVf(FC(log)(c),log(2.0));
    return mrb_complex_new(mrb, creal(c), cimag(c));
  }
  return mrb_float_value(mrb, F(log2)(real));
}

/*
 * call-seq:
 *   CMath.sqrt(z) -> numeric
 *
 * Returns the square root of `z`.
 * Has a branch cut along the negative real axis.
 *
 *   CMath.sqrt(4)     #=> 2.0
 *   CMath.sqrt(-1)    #=> (0.0+1.0i)
 */
static mrb_value
cmath_sqrt(mrb_state *mrb, mrb_value self) {
  mrb_value z = mrb_get_arg1(mrb);
  mrb_float real, imag;
  if (cmath_get_complex(mrb, z, &real, &imag) || real < 0.0) {
    mrb_complex c = CX(real,imag);
    c = FC(sqrt)(c);
    return mrb_complex_new(mrb, creal(c), cimag(c));
  }
  return mrb_float_value(mrb, F(sqrt)(real));
}

/*
 * call-seq:
 *   CMath.sin(z) -> numeric
 *
 * Returns the sine of `z`.
 *
 *   CMath.sin(0)      #=> 0.0
 *   CMath.sin(1i)     #=> (0.0+1.1752011936438014i)
 */
DEF_CMATH_METHOD(sin)

/*
 * call-seq:
 *   CMath.cos(z) -> numeric
 *
 * Returns the cosine of `z`.
 *
 *   CMath.cos(0)      #=> 1.0
 *   CMath.cos(1i)     #=> (1.5430806348152437+0.0i)
 */
DEF_CMATH_METHOD(cos)

/*
 * call-seq:
 *   CMath.tan(z) -> numeric
 *
 * Returns the tangent of `z`.
 *
 *   CMath.tan(0)      #=> 0.0
 *   CMath.tan(1i)     #=> (0.0+0.7615941559557649i)
 */
DEF_CMATH_METHOD(tan)
/*
 * call-seq:
 *   CMath.asin(z) -> numeric
 *
 * Returns the arc sine of `z`.
 *
 *   CMath.asin(0)     #=> 0.0
 *   CMath.asin(2)     #=> (1.5707963267948966-1.3169578969248166i)
 */
DEF_CMATH_METHOD(asin)

/*
 * call-seq:
 *   CMath.acos(z) -> numeric
 *
 * Returns the arc cosine of `z`.
 *
 *   CMath.acos(1)     #=> 0.0
 *   CMath.acos(2)     #=> (0.0+1.3169578969248166i)
 */
DEF_CMATH_METHOD(acos)

/*
 * call-seq:
 *   CMath.atan(z) -> numeric
 *
 * Returns the arc tangent of `z`.
 *
 *   CMath.atan(0)     #=> 0.0
 *   CMath.atan(1i)    #=> (0.0+Infinity*i)
 */
DEF_CMATH_METHOD(atan)
/*
 * call-seq:
 *   CMath.sinh(z) -> numeric
 *
 * Returns the hyperbolic sine of `z`.
 *
 *   CMath.sinh(0)     #=> 0.0
 *   CMath.sinh(1i)    #=> (0.0+0.8414709848078965i)
 */
DEF_CMATH_METHOD(sinh)

/*
 * call-seq:
 *   CMath.cosh(z) -> numeric
 *
 * Returns the hyperbolic cosine of `z`.
 *
 *   CMath.cosh(0)     #=> 1.0
 *   CMath.cosh(1i)    #=> (0.5403023058681398+0.0i)
 */
DEF_CMATH_METHOD(cosh)

/*
 * call-seq:
 *   CMath.tanh(z) -> numeric
 *
 * Returns the hyperbolic tangent of `z`.
 *
 *   CMath.tanh(0)     #=> 0.0
 *   CMath.tanh(1i)    #=> (0.0+1.557407724654902i)
 */
DEF_CMATH_METHOD(tanh)
/*
 * call-seq:
 *   CMath.asinh(z) -> numeric
 *
 * Returns the inverse hyperbolic sine of `z`.
 *
 *   CMath.asinh(0)    #=> 0.0
 *   CMath.asinh(1i)   #=> (0.0+1.5707963267948966i)
 */
DEF_CMATH_METHOD(asinh)

/*
 * call-seq:
 *   CMath.acosh(z) -> numeric
 *
 * Returns the inverse hyperbolic cosine of `z`.
 * Has a branch cut at values less than 1.
 *
 *   CMath.acosh(1)    #=> 0.0
 *   CMath.acosh(0)    #=> (0.0+1.5707963267948966i)
 */
DEF_CMATH_METHOD(acosh)

/*
 * call-seq:
 *   CMath.atanh(z) -> numeric
 *
 * Returns the inverse hyperbolic tangent of `z`.
 * Has branch cuts at values less than -1 and greater than 1.
 *
 *   CMath.atanh(0)    #=> 0.0
 *   CMath.atanh(2)    #=> (0.5493061443340549+1.5707963267948966i)
 */
DEF_CMATH_METHOD(atanh)

/* ------------------------------------------------------------------------*/

void
mrb_mruby_cmath_gem_init(mrb_state* mrb)
{
  struct RClass *cmath = mrb_define_module_id(mrb, MRB_SYM(CMath));

  mrb_include_module(mrb, cmath, mrb_module_get(mrb, "Math"));

  mrb_define_module_function_id(mrb, cmath, MRB_SYM(sin), cmath_sin, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(cos), cmath_cos, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(tan), cmath_tan, MRB_ARGS_REQ(1));

  mrb_define_module_function_id(mrb, cmath, MRB_SYM(asin), cmath_asin, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(acos), cmath_acos, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(atan), cmath_atan, MRB_ARGS_REQ(1));

  mrb_define_module_function_id(mrb, cmath, MRB_SYM(sinh), cmath_sinh, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(cosh), cmath_cosh, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(tanh), cmath_tanh, MRB_ARGS_REQ(1));

  mrb_define_module_function_id(mrb, cmath, MRB_SYM(asinh), cmath_asinh, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(acosh), cmath_acosh, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(atanh), cmath_atanh, MRB_ARGS_REQ(1));

  mrb_define_module_function_id(mrb, cmath, MRB_SYM(exp), cmath_exp, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(log), cmath_log, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(log2), cmath_log2, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(log10), cmath_log10, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, cmath, MRB_SYM(sqrt), cmath_sqrt, MRB_ARGS_REQ(1));
}

void
mrb_mruby_cmath_gem_final(mrb_state* mrb)
{
}
