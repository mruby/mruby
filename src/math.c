/*
** math.c - Math module
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"

#include <math.h>

#if defined(__FreeBSD__) && __FreeBSD__ < 4
#include <floatingpoint.h>
#endif

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#ifdef HAVE_IEEEFP_H
#include <ieeefp.h>
#endif

#define SIGNED_VALUE intptr_t

#ifdef MRB_USE_FLOAT
#define floor(f) floorf(f)
#define ceil(f) ceilf(f)
#define floor(f) floorf(f)
#define fmod(x,y) fmodf(x,y)
#endif

#define numberof(array) (int)(sizeof(array) / sizeof((array)[0]))

#define domain_error(msg) \
    mrb_raise(mrb, E_RANGE_ERROR, "Numerical argument is out of domain - " #msg);


mrb_value
mrb_assoc_new(mrb_state *mrb, mrb_value car, mrb_value cdr);

/*
  TRIGONOMETRIC FUNCTIONS
*/

/*
 *  call-seq:
 *     Math.sin(x)    -> float
 *
 *  Computes the sine of <i>x</i> (expressed in radians). Returns
 *  -1..1.
 */
static mrb_value
math_sin(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = sin(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.cos(x)    -> float
 *
 *  Computes the cosine of <i>x</i> (expressed in radians). Returns
 *  -1..1.
 */
static mrb_value
math_cos(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = cos(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.tan(x)    -> float
 *
 *  Returns the tangent of <i>x</i> (expressed in radians).
 */
static mrb_value
math_tan(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = tan(x);

  return mrb_float_value(x);
}

/*
  INVERSE TRIGONOMETRIC FUNCTIONS
*/

/*
 *  call-seq:
 *     Math.asin(x)    -> float
 *
 *  Computes the arc sine of <i>x</i>. Returns -{PI/2} .. {PI/2}.
 */
static mrb_value
math_asin(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = asin(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.acos(x)    -> float
 *
 *  Computes the arc cosine of <i>x</i>. Returns 0..PI.
 */
static mrb_value
math_acos(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = acos(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.atan(x)    -> float
 *
 *  Computes the arc tangent of <i>x</i>. Returns -{PI/2} .. {PI/2}.
 */
static mrb_value
math_atan(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = atan(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.atan2(y, x)  -> float
 *
 *  Computes the arc tangent given <i>y</i> and <i>x</i>. Returns
 *  -PI..PI.
 *
 *    Math.atan2(-0.0, -1.0) #=> -3.141592653589793
 *    Math.atan2(-1.0, -1.0) #=> -2.356194490192345
 *    Math.atan2(-1.0, 0.0)  #=> -1.5707963267948966
 *    Math.atan2(-1.0, 1.0)  #=> -0.7853981633974483
 *    Math.atan2(-0.0, 1.0)  #=> -0.0
 *    Math.atan2(0.0, 1.0)   #=> 0.0
 *    Math.atan2(1.0, 1.0)   #=> 0.7853981633974483
 *    Math.atan2(1.0, 0.0)   #=> 1.5707963267948966
 *    Math.atan2(1.0, -1.0)  #=> 2.356194490192345
 *    Math.atan2(0.0, -1.0)  #=> 3.141592653589793
 *
 */
static mrb_value
math_atan2(mrb_state *mrb, mrb_value obj)
{
  mrb_float x, y;

  mrb_get_args(mrb, "ff", &x, &y);
  x = atan2(x, y);

  return mrb_float_value(x);
}



/*
  HYPERBOLIC TRIG FUNCTIONS
*/
#ifndef HAVE_SINH
double
sinh(double x)
{
    return (exp(x) - exp(-x)) / 2;
}
#endif

#ifndef HAVE_TANH
double
tanh(double x)
{
    return sinh(x) / cosh(x);
}
#endif

/*
 *  call-seq:
 *     Math.sinh(x)    -> float
 *
 *  Computes the hyperbolic sine of <i>x</i> (expressed in
 *  radians).
 */
static mrb_value
math_sinh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = sinh(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.cosh(x)    -> float
 *
 *  Computes the hyperbolic cosine of <i>x</i> (expressed in radians).
 */
static mrb_value
math_cosh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = cosh(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.tanh()    -> float
 *
 *  Computes the hyperbolic tangent of <i>x</i> (expressed in
 *  radians).
 */
static mrb_value
math_tanh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = tanh(x);

  return mrb_float_value(x);
}


/*
  INVERSE HYPERBOLIC TRIG FUNCTIONS
*/

/*
 *  call-seq:
 *     Math.asinh(x)    -> float
 *
 *  Computes the inverse hyperbolic sine of <i>x</i>.
 */
static mrb_value
math_asinh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = asinh(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.acosh(x)    -> float
 *
 *  Computes the inverse hyperbolic cosine of <i>x</i>.
 */
static mrb_value
math_acosh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = acosh(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.atanh(x)    -> float
 *
 *  Computes the inverse hyperbolic tangent of <i>x</i>.
 */
static mrb_value
math_atanh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = atanh(x);

  return mrb_float_value(x);
}

/*
  EXPONENTIALS AND LOGARITHMS
*/
#if defined __CYGWIN__
# include <cygwin/version.h>
# if CYGWIN_VERSION_DLL_MAJOR < 1005
#  define nan(x) nan()
# endif
# define log(x) ((x) < 0.0 ? nan("") : log(x))
# define log10(x) ((x) < 0.0 ? nan("") : log10(x))
#endif

#ifndef log2
#ifndef HAVE_LOG2
double
log2(double x)
{
    return log10(x)/log10(2.0);
}
#else
extern double log2(double);
#endif
#endif

/*
 *  call-seq:
 *     Math.exp(x)    -> float
 *
 *  Returns e**x.
 *
 *    Math.exp(0)       #=> 1.0
 *    Math.exp(1)       #=> 2.718281828459045
 *    Math.exp(1.5)     #=> 4.4816890703380645
 *
 */
static mrb_value
math_exp(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = exp(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.log(numeric)    -> float
 *     Math.log(num,base)   -> float
 *
 *  Returns the natural logarithm of <i>numeric</i>.
 *  If additional second argument is given, it will be the base
 *  of logarithm.
 *
 *    Math.log(1)          #=> 0.0
 *    Math.log(Math::E)    #=> 1.0
 *    Math.log(Math::E**3) #=> 3.0
 *    Math.log(12,3)       #=> 2.2618595071429146
 *
 */
static mrb_value
math_log(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = log(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.log2(numeric)    -> float
 *
 *  Returns the base 2 logarithm of <i>numeric</i>.
 *
 *    Math.log2(1)      #=> 0.0
 *    Math.log2(2)      #=> 1.0
 *    Math.log2(32768)  #=> 15.0
 *    Math.log2(65536)  #=> 16.0
 *
 */
static mrb_value
math_log2(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = log2(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.log10(numeric)    -> float
 *
 *  Returns the base 10 logarithm of <i>numeric</i>.
 *
 *    Math.log10(1)       #=> 0.0
 *    Math.log10(10)      #=> 1.0
 *    Math.log10(10**100) #=> 100.0
 *
 */
static mrb_value
math_log10(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = log10(x);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.cbrt(numeric)    -> float
 *
 *  Returns the cube root of <i>numeric</i>.
 *
 *    -9.upto(9) {|x|
 *      p [x, Math.cbrt(x), Math.cbrt(x)**3]
 *    }
 *    #=>
 *    [-9, -2.0800838230519, -9.0]
 *    [-8, -2.0, -8.0]
 *    [-7, -1.91293118277239, -7.0]
 *    [-6, -1.81712059283214, -6.0]
 *    [-5, -1.7099759466767, -5.0]
 *    [-4, -1.5874010519682, -4.0]
 *    [-3, -1.44224957030741, -3.0]
 *    [-2, -1.25992104989487, -2.0]
 *    [-1, -1.0, -1.0]
 *    [0, 0.0, 0.0]
 *    [1, 1.0, 1.0]
 *    [2, 1.25992104989487, 2.0]
 *    [3, 1.44224957030741, 3.0]
 *    [4, 1.5874010519682, 4.0]
 *    [5, 1.7099759466767, 5.0]
 *    [6, 1.81712059283214, 6.0]
 *    [7, 1.91293118277239, 7.0]
 *    [8, 2.0, 8.0]
 *    [9, 2.0800838230519, 9.0]
 *
 */
static mrb_value
math_cbrt(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = cbrt(x);

  return mrb_float_value(x);
}


/*
 *  call-seq:
 *     Math.frexp(numeric)    -> [ fraction, exponent ]
 *
 *  Returns a two-element array containing the normalized fraction (a
 *  <code>Float</code>) and exponent (a <code>Fixnum</code>) of
 *  <i>numeric</i>.
 *
 *     fraction, exponent = Math.frexp(1234)   #=> [0.6025390625, 11]
 *     fraction * 2**exponent                  #=> 1234.0
 */
static mrb_value
math_frexp(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;
  int exp;
  
  mrb_get_args(mrb, "f", &x);
  x = frexp(x, &exp);

  return mrb_assoc_new(mrb, mrb_float_value(x), mrb_fixnum_value(exp));
}

/*
 *  call-seq:
 *     Math.ldexp(flt, int) -> float
 *
 *  Returns the value of <i>flt</i>*(2**<i>int</i>).
 *
 *     fraction, exponent = Math.frexp(1234)
 *     Math.ldexp(fraction, exponent)   #=> 1234.0
 */
static mrb_value
math_ldexp(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;
  mrb_int   i;

  mrb_get_args(mrb, "fi", &x, &i);
  x = ldexp(x, i);

  return mrb_float_value(x);
}

/*
 *  call-seq:
 *     Math.hypot(x, y)    -> float
 *
 *  Returns sqrt(x**2 + y**2), the hypotenuse of a right-angled triangle
 *  with sides <i>x</i> and <i>y</i>.
 *
 *     Math.hypot(3, 4)   #=> 5.0
 */
static mrb_value
math_hypot(mrb_state *mrb, mrb_value obj)
{
  mrb_float x, y;

  mrb_get_args(mrb, "ff", &x, &y);
  x = hypot(x, y);

  return mrb_float_value(x);
}

/*
 * call-seq:
 *    Math.erf(x)  -> float
 *
 *  Calculates the error function of x.
 */
static mrb_value
math_erf(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = erf(x);

  return mrb_float_value(x);
}

/*
 * call-seq:
 *    Math.erfc(x)  -> float
 *
 *  Calculates the complementary error function of x.
 */
static mrb_value
math_erfc(mrb_state *mrb, mrb_value obj)
{
  mrb_float x;

  mrb_get_args(mrb, "f", &x);
  x = erfc(x);

  return mrb_float_value(x);
}

/*
 * call-seq:
 *    Math.gamma(x)  -> float
 *
 *  Calculates the gamma function of x.
 *
 *  Note that gamma(n) is same as fact(n-1) for integer n > 0.
 *  However gamma(n) returns float and can be an approximation.
 *
 *   def fact(n) (1..n).inject(1) {|r,i| r*i } end
 *   1.upto(26) {|i| p [i, Math.gamma(i), fact(i-1)] }
 *   #=> [1, 1.0, 1]
 *   #   [2, 1.0, 1]
 *   #   [3, 2.0, 2]
 *   #   [4, 6.0, 6]
 *   #   [5, 24.0, 24]
 *   #   [6, 120.0, 120]
 *   #   [7, 720.0, 720]
 *   #   [8, 5040.0, 5040]
 *   #   [9, 40320.0, 40320]
 *   #   [10, 362880.0, 362880]
 *   #   [11, 3628800.0, 3628800]
 *   #   [12, 39916800.0, 39916800]
 *   #   [13, 479001600.0, 479001600]
 *   #   [14, 6227020800.0, 6227020800]
 *   #   [15, 87178291200.0, 87178291200]
 *   #   [16, 1307674368000.0, 1307674368000]
 *   #   [17, 20922789888000.0, 20922789888000]
 *   #   [18, 355687428096000.0, 355687428096000]
 *   #   [19, 6.402373705728e+15, 6402373705728000]
 *   #   [20, 1.21645100408832e+17, 121645100408832000]
 *   #   [21, 2.43290200817664e+18, 2432902008176640000]
 *   #   [22, 5.109094217170944e+19, 51090942171709440000]
 *   #   [23, 1.1240007277776077e+21, 1124000727777607680000]
 *   #   [24, 2.5852016738885062e+22, 25852016738884976640000]
 *   #   [25, 6.204484017332391e+23, 620448401733239439360000]
 *   #   [26, 1.5511210043330954e+25, 15511210043330985984000000]
 *
 */
static mrb_value
math_gamma(mrb_state *mrb, mrb_value obj)
{
    static const double fact_table[] = {
        /* fact(0) */ 1.0,
        /* fact(1) */ 1.0,
        /* fact(2) */ 2.0,
        /* fact(3) */ 6.0,
        /* fact(4) */ 24.0,
        /* fact(5) */ 120.0,
        /* fact(6) */ 720.0,
        /* fact(7) */ 5040.0,
        /* fact(8) */ 40320.0,
        /* fact(9) */ 362880.0,
        /* fact(10) */ 3628800.0,
        /* fact(11) */ 39916800.0,
        /* fact(12) */ 479001600.0,
        /* fact(13) */ 6227020800.0,
        /* fact(14) */ 87178291200.0,
        /* fact(15) */ 1307674368000.0,
        /* fact(16) */ 20922789888000.0,
        /* fact(17) */ 355687428096000.0,
        /* fact(18) */ 6402373705728000.0,
        /* fact(19) */ 121645100408832000.0,
        /* fact(20) */ 2432902008176640000.0,
        /* fact(21) */ 51090942171709440000.0,
        /* fact(22) */ 1124000727777607680000.0,
        /* fact(23)=25852016738884976640000 needs 56bit mantissa which is
         * impossible to represent exactly in IEEE 754 double which have
         * 53bit mantissa. */
    };
    double intpart, fracpart;
    mrb_float x;
    mrb_get_args(mrb, "f", &x);
    
    /* check for domain error */
    if (isinf(x) && signbit(x)) domain_error("gamma");
    fracpart = modf(x, &intpart);
    if (fracpart == 0.0) {
	    if (intpart < 0) domain_error("gamma");
	    if (0 < intpart &&
	      intpart - 1 < (double)numberof(fact_table)) {
	    return mrb_float_value(fact_table[(int)intpart - 1]);
	    }
    }
    return mrb_float_value(tgamma(x));
}


/*
 * call-seq:
 *    Math.lgamma(x)  -> [float, -1 or 1]
 *
 *  Calculates the logarithmic gamma of x and
 *  the sign of gamma of x.
 *
 *  Math.lgamma(x) is same as
 *   [Math.log(Math.gamma(x).abs), Math.gamma(x) < 0 ? -1 : 1]
 *  but avoid overflow by Math.gamma(x) for large x.
 */

/* TODO: lgamma_r() is missing */

/*
static mrb_value
math_lgamma(mrb_state *mrb, mrb_value obj)
{
    double d0, d;
    int sign=1;
    mrb_float x;
    mrb_get_args(mrb, "f", &x);

    // check for domain error
    if (isinf(x)) {
	    if (signbit(x)) domain_error("lgamma");
	    return rb_assoc_new(mrb_float_value(INFINITY), mrb_fixnum_value(1));
    }
    d = lgamma_r(x, &sign);
    return mrb_assoc_new(mrb, mrb_float_value(d), mrb_fixnum_value(sign));
}
*/





/* ------------------------------------------------------------------------*/
void
mrb_init_math(mrb_state *mrb)
{
  struct RClass *mrb_math;
  mrb_math = mrb_define_module(mrb, "Math");
  
  #ifdef M_PI
      mrb_define_const(mrb, mrb_math, "PI", mrb_float_value(M_PI));
  #else
      mrb_define_const(mrb, mrb_math, "PI", mrb_float_value(atan(1.0)*4.0));
  #endif
  
  #ifdef M_E
      mrb_define_const(mrb, mrb_math, "E", mrb_float_value(M_E));
  #else
      mrb_define_const(mrb, mrb_math, "E", mrb_float_value(exp(1.0)));
  #endif
  
  mrb_define_class_method(mrb, mrb_math, "sin", math_sin, 1);
  mrb_define_class_method(mrb, mrb_math, "cos", math_cos, 1);
  mrb_define_class_method(mrb, mrb_math, "tan", math_tan, 1);

  mrb_define_class_method(mrb, mrb_math, "asin", math_asin, 1);
  mrb_define_class_method(mrb, mrb_math, "acos", math_acos, 1);
  mrb_define_class_method(mrb, mrb_math, "atan", math_atan, 1);
  mrb_define_class_method(mrb, mrb_math, "atan2", math_atan2, 2);
  
  mrb_define_class_method(mrb, mrb_math, "sinh", math_sinh, 1);
  mrb_define_class_method(mrb, mrb_math, "cosh", math_cosh, 1);
  mrb_define_class_method(mrb, mrb_math, "tanh", math_tanh, 1);

  mrb_define_class_method(mrb, mrb_math, "asinh", math_asinh, 1);
  mrb_define_class_method(mrb, mrb_math, "acosh", math_acosh, 1);
  mrb_define_class_method(mrb, mrb_math, "atanh", math_atanh, 1);

  mrb_define_class_method(mrb, mrb_math, "exp", math_exp, 1);
  mrb_define_class_method(mrb, mrb_math, "log", math_log, -1);
  mrb_define_class_method(mrb, mrb_math, "log2", math_log2, 1);
  mrb_define_class_method(mrb, mrb_math, "log10", math_log10, 1);
  mrb_define_class_method(mrb, mrb_math, "cbrt", math_cbrt, 1);

  mrb_define_class_method(mrb, mrb_math, "frexp", math_frexp, 1);
  mrb_define_class_method(mrb, mrb_math, "ldexp", math_ldexp, 2);

  mrb_define_class_method(mrb, mrb_math, "hypot", math_hypot, 2);

  mrb_define_class_method(mrb, mrb_math, "erf",  math_erf,  1);
  mrb_define_class_method(mrb, mrb_math, "erfc", math_erfc, 1);

  mrb_define_class_method(mrb, mrb_math, "gamma", math_gamma, 1);
  /* mrb_define_class_method(mrb, mrb_math, "lgamma", math_lgamma, 1); */
}
