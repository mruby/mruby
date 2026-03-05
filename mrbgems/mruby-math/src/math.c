/*
** math.c - Math module
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>

#ifdef MRB_NO_FLOAT
# error Math conflicts with 'MRB_NO_FLOAT' configuration
#endif

#include <mruby/array.h>
#include <mruby/presym.h>

static void
domain_error(mrb_state *mrb, const char *func)
{
  struct RClass *math = mrb_module_get_id(mrb, MRB_SYM(Math));
  struct RClass *domainerror = mrb_class_get_under_id(mrb, math, MRB_SYM(DomainError));
  mrb_raisef(mrb, domainerror, "Numerical argument is out of domain - %s", func);
}

/* math functions not provided by Microsoft Visual C++ 2012 or older */
#if defined _MSC_VER && _MSC_VER <= 1700

double
asinh(double x)
{
  double xa, ya, y;

  /* Basic formula loses precision for x < 0, but asinh is an odd function */
  xa = fabs(x);
  if (xa > 3.16227E+18) {
    /* Prevent x*x from overflowing; basic formula reduces to log(2*x) */
    ya = log(xa) + 0.69314718055994530942;
  }
  else {
    /* Basic formula for asinh */
    ya = log(xa + sqrt(xa*xa + 1.0));
  }

  y = _copysign(ya, x);
  return y;
}

double
acosh(double x)
{
  double y;

  if (x > 3.16227E+18) {
    /* Prevent x*x from overflowing; basic formula reduces to log(2*x) */
    y = log(x) + 0.69314718055994530942;
  }
  else {
    /* Basic formula for acosh */
    y = log(x + sqrt(x*x - 1.0));
  }

  return y;
}

double
atanh(double x)
{
  double y;

  if (fabs(x) < 1E-2) {
    /* The sums 1+x and 1-x lose precision for small x.  Use the polynomial
       instead. */
    double x2 = x * x;
    y = x*(1.0 + x2*(1.0/3.0 + x2*(1.0/5.0 + x2*(1.0/7.0))));
  }
  else {
    /* Basic formula for atanh */
    y = 0.5 * (log1p(x) - log1p(-x));
  }

  return y;
}

double
cbrt(double x)
{
  double xa, ya, y;

  /* pow(x, y) is undefined for x < 0 and y not an integer, but cbrt is an
     odd function */
  xa = fabs(x);
  ya = pow(xa, 1.0/3.0);
  y = _copysign(ya, x);
  return y;
}

/* Declaration of complementary Error function */
double erfc(double x);

/*
** Implementations of error functions
** credits to http://www.digitalmars.com/archives/cplusplus/3634.html
*/

/* Implementation of Error function */
double
erf(double x)
{
  static const double two_sqrtpi = 1.128379167095512574;
  double sum  = x;
  double term = x;
  double xsqr = x*x;
  int j= 1;
  if (fabs(x) > 2.2) {
    return 1.0 - erfc(x);
  }
  do {
    term *= xsqr/j;
    sum  -= term/(2*j+1);
    j++;
    term *= xsqr/j;
    sum  += term/(2*j+1);
    j++;
    if (sum == 0) break;
  } while (fabs(term/sum) > DBL_EPSILON);
  return two_sqrtpi*sum;
}

/* Implementation of complementary Error function */
double
erfc(double x)
{
  static const double one_sqrtpi = 0.564189583547756287;
  double a = 1;
  double b = x;
  double c = x;
  double d = x*x+0.5;
  double q1;
  double q2 = b/d;
  double n = 1.0;

  if (fabs(x) < 2.2) {
    return erfc(x);
  }
  if (x < 0.0) { /*signbit(x)*/
    return 2.0 - erfc(-x);
  }
  do {
    double t = a*n+b*x;
    a  = b;
    b  = t;
    t  = c*n+d*x;
    c  = d;
    d  = t;
    n += 0.5;
    q1 = q2;
    q2 = b/d;
  } while (fabs(q1-q2)/q2 > DBL_EPSILON);
  return one_sqrtpi*exp(-x*x)*q2;
}

#endif

#if defined __FreeBSD__ && !defined __FreeBSD_version
#include <osreldate.h> /* for __FreeBSD_version */
#endif

#if (defined _MSC_VER && _MSC_VER < 1800) || defined __ANDROID__ || (defined __FreeBSD__  &&  __FreeBSD_version < 803000)

double
log2(double x)
{
    return log10(x)/log10(2.0);
}

#endif

#define get_float_arg(mrb) mrb_as_float((mrb), mrb_get_arg1(mrb))

/*
  TRIGONOMETRIC FUNCTIONS
*/

/*
 *  call-seq:
 *     Math.sin(x)    -> float
 *
 *  Computes the sine of *x* (expressed in radians). Returns
 *  -1..1.
 */
static mrb_value
math_sin(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = sin(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.cos(x)    -> float
 *
 *  Computes the cosine of *x* (expressed in radians). Returns
 *  -1..1.
 */
static mrb_value
math_cos(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = cos(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.tan(x)    -> float
 *
 *  Returns the tangent of *x* (expressed in radians).
 */
static mrb_value
math_tan(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = tan(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
  INVERSE TRIGONOMETRIC FUNCTIONS
*/

/*
 *  call-seq:
 *     Math.asin(x)    -> float
 *
 *  Computes the arc sine of *x*.
 *  @return computed value between `-(PI/2)` and `(PI/2)`.
 */
static mrb_value
math_asin(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = get_float_arg(mrb);

  if (x < -1.0 || x > 1.0) {
    domain_error(mrb, "asin");
  }
  x = asin(x);

  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.acos(x)    -> float
 *
 *  Computes the arc cosine of *x*. Returns 0..PI.
 */
static mrb_value
math_acos(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = get_float_arg(mrb);

  if (x < -1.0 || x > 1.0) {
    domain_error(mrb, "acos");
  }
  x = acos(x);

  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.atan(x)    -> float
 *
 *  Computes the arc tangent of *x*. Returns `-(PI/2) .. (PI/2)`.
 */
static mrb_value
math_atan(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = atan(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.atan2(y, x)  -> float
 *
 *  Computes the arc tangent given *y* and *x*. Returns
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

  return mrb_float_value(mrb, x);
}

/*
  HYPERBOLIC TRIG FUNCTIONS
*/
/*
 *  call-seq:
 *     Math.sinh(x)    -> float
 *
 *  Computes the hyperbolic sine of *x* (expressed in
 *  radians).
 */
static mrb_value
math_sinh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = sinh(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.cosh(x)    -> float
 *
 *  Computes the hyperbolic cosine of *x* (expressed in radians).
 */
static mrb_value
math_cosh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = cosh(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.tanh()    -> float
 *
 *  Computes the hyperbolic tangent of *x* (expressed in
 *  radians).
 */
static mrb_value
math_tanh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = tanh(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}


/*
  INVERSE HYPERBOLIC TRIG FUNCTIONS
*/

/*
 *  call-seq:
 *     Math.asinh(x)    -> float
 *
 *  Computes the inverse hyperbolic sine of *x*.
 */
static mrb_value
math_asinh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = asinh(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.acosh(x)    -> float
 *
 *  Computes the inverse hyperbolic cosine of *x*.
 */
static mrb_value
math_acosh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = get_float_arg(mrb);

  if (x < 1.0) {
    domain_error(mrb, "acosh");
  }
  x = acosh(x);

  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.atanh(x)    -> float
 *
 *  Computes the inverse hyperbolic tangent of *x*.
 */
static mrb_value
math_atanh(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = get_float_arg(mrb);

  if (x < -1.0 || x > 1.0) {
    domain_error(mrb, "atanh");
  }
  x = atanh(x);

  return mrb_float_value(mrb, x);
}

/*
  EXPONENTIALS AND LOGARITHMS
*/

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
  mrb_float x = exp(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.expm1(x)    -> float
 *
 *  Returns exp(x) - 1.
 */
static mrb_value
math_expm1(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = expm1(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.log1p(x)    -> float
 *
 *  Returns log(1 + x).
 */
static mrb_value
math_log1p(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = get_float_arg(mrb);
  if (x < -1.0) {
    domain_error(mrb, "log1p");
  }
  x = log1p(x);
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.log(numeric)    -> float
 *     Math.log(num,base)   -> float
 *
 *  Returns the natural logarithm of *numeric*.
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
  mrb_float x, base;
  mrb_int argc = mrb_get_args(mrb, "f|f", &x, &base);

  if (x < 0.0) {
    domain_error(mrb, "log");
  }
  x = log(x);
  if (argc == 2) {
    if (base < 0.0) {
      domain_error(mrb, "log");
    }
    x /= log(base);
  }
  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.log2(numeric)    -> float
 *
 *  Returns the base 2 logarithm of *numeric*.
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
  mrb_float x = get_float_arg(mrb);

  if (x < 0.0) {
    domain_error(mrb, "log2");
  }
  x = log2(x);

  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.log10(numeric)    -> float
 *
 *  Returns the base 10 logarithm of *numeric*.
 *
 *    Math.log10(1)       #=> 0.0
 *    Math.log10(10)      #=> 1.0
 *    Math.log10(10**100) #=> 100.0
 *
 */
static mrb_value
math_log10(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = get_float_arg(mrb);

  if (x < 0.0) {
    domain_error(mrb, "log10");
  }
  x = log10(x);

  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.sqrt(numeric)    -> float
 *
 *  Returns the square root of *numeric*.
 *
 */
static mrb_value
math_sqrt(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = get_float_arg(mrb);

  if (x < 0.0) {
    domain_error(mrb, "sqrt");
  }
  x = sqrt(x);

  return mrb_float_value(mrb, x);
}


/*
 *  call-seq:
 *     Math.cbrt(numeric)    -> float
 *
 *  Returns the cube root of *numeric*.
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
  mrb_float x = cbrt(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}


/*
 *  call-seq:
 *     Math.frexp(numeric)    -> [ fraction, exponent ]
 *
 *  Returns a two-element array containing the normalized fraction (a
 *  `Float`) and exponent (a `Integer`) of
 *  *numeric*.
 *
 *     fraction, exponent = Math.frexp(1234)   #=> [0.6025390625, 11]
 *     fraction * 2**exponent                  #=> 1234.0
 */
static mrb_value
math_frexp(mrb_state *mrb, mrb_value obj)
{
  mrb_float x = get_float_arg(mrb);
  int exp;

  x = frexp(x, &exp);

  return mrb_assoc_new(mrb, mrb_float_value(mrb, x), mrb_fixnum_value(exp));
}

/*
 *  call-seq:
 *     Math.ldexp(flt, int) -> float
 *
 *  Returns the value of *flt**(2***int*).
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
  x = ldexp(x, (int)i);

  return mrb_float_value(mrb, x);
}

/*
 *  call-seq:
 *     Math.hypot(x, y)    -> float
 *
 *  Returns sqrt(x**2 + y**2), the hypotenuse of a right-angled triangle
 *  with sides *x* and *y*.
 *
 *     Math.hypot(3, 4)   #=> 5.0
 */
static mrb_value
math_hypot(mrb_state *mrb, mrb_value obj)
{
  mrb_float x, y;

  mrb_get_args(mrb, "ff", &x, &y);
  x = hypot(x, y);

  return mrb_float_value(mrb, x);
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
  mrb_float x = erf(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
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
  mrb_float x = erfc(get_float_arg(mrb));
  return mrb_float_value(mrb, x);
}

/* ------------------------------------------------------------------------*/
void
mrb_mruby_math_gem_init(mrb_state* mrb)
{
  struct RClass *math = mrb_define_module_id(mrb, MRB_SYM(Math));

  mrb_define_class_under_id(mrb, math, MRB_SYM(DomainError), E_STANDARD_ERROR);

#ifdef M_PI
  mrb_define_const_id(mrb, math, MRB_SYM(PI), mrb_float_value(mrb, M_PI));
#else
  mrb_define_const_id(mrb, math, MRB_SYM(PI), mrb_float_value(mrb, atan(1.0)*4.0));
#endif

#ifdef M_E
  mrb_define_const_id(mrb, math, MRB_SYM(E), mrb_float_value(mrb, M_E));
#else
  mrb_define_const_id(mrb, math, MRB_SYM(E), mrb_float_value(mrb, exp(1.0)));
#endif

  mrb_define_module_function_id(mrb, math, MRB_SYM(sin), math_sin, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(cos), math_cos, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(tan), math_tan, MRB_ARGS_REQ(1));

  mrb_define_module_function_id(mrb, math, MRB_SYM(asin), math_asin, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(acos), math_acos, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(atan), math_atan, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(atan2), math_atan2, MRB_ARGS_REQ(2));

  mrb_define_module_function_id(mrb, math, MRB_SYM(sinh), math_sinh, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(cosh), math_cosh, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(tanh), math_tanh, MRB_ARGS_REQ(1));

  mrb_define_module_function_id(mrb, math, MRB_SYM(asinh), math_asinh, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(acosh), math_acosh, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(atanh), math_atanh, MRB_ARGS_REQ(1));

  mrb_define_module_function_id(mrb, math, MRB_SYM(exp), math_exp, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(expm1), math_expm1, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(log1p), math_log1p, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(log), math_log, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(log2), math_log2, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(log10), math_log10, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(sqrt), math_sqrt, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(cbrt), math_cbrt, MRB_ARGS_REQ(1));

  mrb_define_module_function_id(mrb, math, MRB_SYM(frexp), math_frexp, MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(ldexp), math_ldexp, MRB_ARGS_REQ(2));

  mrb_define_module_function_id(mrb, math, MRB_SYM(hypot), math_hypot, MRB_ARGS_REQ(2));

  mrb_define_module_function_id(mrb, math, MRB_SYM(erf),  math_erf,  MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, math, MRB_SYM(erfc), math_erfc, MRB_ARGS_REQ(1));
}

void
mrb_mruby_math_gem_final(mrb_state* mrb)
{
}
