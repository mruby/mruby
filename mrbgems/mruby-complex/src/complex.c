#include <mruby.h>
#include <mruby/class.h>
#include <mruby/numeric.h>
#include <mruby/internal.h>
#include <float.h>

#ifdef MRB_NO_FLOAT
# error Complex conflicts with 'MRB_NO_FLOAT' configuration
#endif

#ifdef MRB_USE_FLOAT32
#define F(x) x##f
#else
#define F(x) x
#endif

#ifdef MRB_USE_RATIONAL
mrb_value mrb_rational_to_i(mrb_state *mrb, mrb_value self);
#endif

/* A Complex answers with the parts it was given.  Two Floats stay the two
   mrb_float they always were; any other pair from the numeric tower is held
   as two mrb_value, with COMP_VALUE on the object saying which half of the
   union is live.  MRB_COMPLEX_FLOAT_ONLY compiles the value half away and
   coerces every part through Float on the way in, which is this gem's
   historical behavior. */
struct mrb_complex {
  union {
    struct {
      mrb_float real;
      mrb_float imaginary;
    } f;
#ifndef MRB_COMPLEX_FLOAT_ONLY
    struct {
      mrb_value real;
      mrb_value imaginary;
    } v;
#endif
  };
};

#ifndef MRB_COMPLEX_FLOAT_ONLY
#define COMP_VALUE 1
#define COMP_VALUE_P(cpx) (mrb_obj_ptr(cpx)->flags & COMP_VALUE)
#else
#define COMP_VALUE_P(cpx) FALSE
#endif

#ifdef MRB_COMPLEX_INDIRECT

struct RComplex {
  MRB_OBJECT_HEADER;
  struct mrb_complex *p;
};

static struct mrb_complex*
complex_ptr(mrb_state *mrb, mrb_value v)
{
  struct RComplex *r = (struct RComplex*)mrb_obj_ptr(v);

  if (!r->p) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "uninitialized complex");
  }
  return r->p;
}

#else
#define COMPLEX_INLINE
struct RComplex {
  MRB_OBJECT_HEADER;
  struct mrb_complex r;
};
#define complex_ptr(mrb, v) (&((struct RComplex*)mrb_obj_ptr(v))->r)
#endif

mrb_static_assert_object_size(struct RComplex);

static struct RBasic*
complex_alloc(mrb_state *mrb, struct RClass *c, struct mrb_complex **p)
{
  struct RComplex *s;
  s = MRB_OBJ_ALLOC(mrb, MRB_TT_COMPLEX, c);
#ifdef COMPLEX_INLINE
  *p = &s->r;
#else
  *p = s->p = (struct mrb_complex*)mrb_malloc(mrb, sizeof(struct mrb_complex));
#endif
  return (struct RBasic*)s;
}

/* Both parts as floats, whichever half of the union is live. */
static mrb_float
comp_float_real(mrb_state *mrb, mrb_value cpx)
{
  struct mrb_complex *p = complex_ptr(mrb, cpx);
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_VALUE_P(cpx)) return mrb_as_float(mrb, p->v.real);
#endif
  return p->f.real;
}

static mrb_float
comp_float_imaginary(mrb_state *mrb, mrb_value cpx)
{
  struct mrb_complex *p = complex_ptr(mrb, cpx);
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_VALUE_P(cpx)) return mrb_as_float(mrb, p->v.imaginary);
#endif
  return p->f.imaginary;
}

/* Both parts as values: what #real and #imaginary answer. */
static mrb_value
part_real(mrb_state *mrb, mrb_value cpx)
{
  struct mrb_complex *p = complex_ptr(mrb, cpx);
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_VALUE_P(cpx)) return p->v.real;
#endif
  return mrb_float_value(mrb, p->f.real);
}

static mrb_value
part_imaginary(mrb_state *mrb, mrb_value cpx)
{
  struct mrb_complex *p = complex_ptr(mrb, cpx);
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_VALUE_P(cpx)) return p->v.imaginary;
#endif
  return mrb_float_value(mrb, p->f.imaginary);
}

void
mrb_complex_get(mrb_state *mrb, mrb_value cpx, mrb_float *r, mrb_float *i)
{
  *r = comp_float_real(mrb, cpx);
  *i = comp_float_imaginary(mrb, cpx);
}

mrb_value
mrb_complex_new(mrb_state *mrb, mrb_float real, mrb_float imaginary)
{
  struct RClass *c = mrb_class_get_id(mrb, MRB_SYM(Complex));
  struct mrb_complex *p;
  struct RBasic *comp = complex_alloc(mrb, c, &p);
  p->f.real = real;
  p->f.imaginary = imaginary;
  comp->frozen = 1;

  return mrb_obj_value(comp);
}

#define complex_new(mrb, real, imag) mrb_complex_new(mrb, real, imag)

#ifndef MRB_COMPLEX_FLOAT_ONLY

/* The set a part can be: the numeric tower as this build has it, minus
   Complex itself and Float, which has its own storage. */
static mrb_bool
part_exact_type_p(mrb_value v)
{
  switch (mrb_type(v)) {
  case MRB_TT_INTEGER:
#ifdef MRB_USE_BIGINT
  case MRB_TT_BIGINT:
#endif
#ifdef MRB_USE_RATIONAL
  case MRB_TT_RATIONAL:
#endif
    return TRUE;
  default:
    return FALSE;
  }
}

/* A member of the tower passes through as itself; everything else keeps its
   old answer, a coercion through Float.  For a Complex argument that is its
   real part when the imaginary part is zero and a RangeError otherwise. */
static mrb_value
part_coerce(mrb_state *mrb, mrb_value v)
{
  if (part_exact_type_p(v) || mrb_float_p(v)) return v;
  return mrb_float_value(mrb, mrb_as_float(mrb, v));
}

mrb_value
mrb_complex_new_value(mrb_state *mrb, mrb_value real, mrb_value imaginary)
{
  if (mrb_float_p(real) && mrb_float_p(imaginary)) {
    return mrb_complex_new(mrb, mrb_float(real), mrb_float(imaginary));
  }

  struct RClass *c = mrb_class_get_id(mrb, MRB_SYM(Complex));
  struct mrb_complex *p;
  struct RBasic *comp = complex_alloc(mrb, c, &p);
  comp->flags |= COMP_VALUE;
  p->v.real = real;
  p->v.imaginary = imaginary;
  comp->frozen = 1;

  return mrb_obj_value(comp);
}

int
mrb_complex_mark(mrb_state *mrb, struct RBasic *comp)
{
  if (!(comp->flags & COMP_VALUE)) return 0;

  struct mrb_complex *p;
#ifdef COMPLEX_INLINE
  p = &((struct RComplex*)comp)->r;
#else
  p = ((struct RComplex*)comp)->p;
  if (!p) return 0;
#endif
  int children = 0;
  if (!mrb_immediate_p(p->v.real)) {
    mrb_gc_mark(mrb, mrb_basic_ptr(p->v.real));
    children++;
  }
  if (!mrb_immediate_p(p->v.imaginary)) {
    mrb_gc_mark(mrb, mrb_basic_ptr(p->v.imaginary));
    children++;
  }
  return children;
}

/* Equality on the closed set a part can be.  Every pair lands on an exact
   C comparison; nothing here calls back into Ruby. */
static mrb_bool
part_eq(mrb_state *mrb, mrb_value a, mrb_value b)
{
#ifdef MRB_USE_RATIONAL
  if (mrb_type(a) == MRB_TT_RATIONAL) return mrb_rational_eq(mrb, a, b);
  if (mrb_type(b) == MRB_TT_RATIONAL) return mrb_rational_eq(mrb, b, a);
#endif
#ifdef MRB_USE_BIGINT
  if (mrb_bigint_p(a)) return mrb_bint_cmp(mrb, a, b) == 0;
  if (mrb_bigint_p(b)) return mrb_bint_cmp(mrb, b, a) == 0;
#endif
  if (mrb_float_p(a)) {
    if (mrb_float_p(b)) return mrb_float(a) == mrb_float(b);
    return mrb_int_float_cmp(mrb_integer(b), mrb_float(a)) == 0;
  }
  if (mrb_float_p(b)) return mrb_int_float_cmp(mrb_integer(a), mrb_float(b)) == 0;
  return mrb_integer(a) == mrb_integer(b);
}

#define part_zero_p(mrb, v) part_eq(mrb, v, mrb_fixnum_value(0))

#endif /* MRB_COMPLEX_FLOAT_ONLY */

void
mrb_complex_copy(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct mrb_complex *p1 = complex_ptr(mrb, x);
  struct mrb_complex *p2 = complex_ptr(mrb, y);
#ifndef MRB_COMPLEX_FLOAT_ONLY
  struct RBasic *b1 = (struct RBasic*)mrb_obj_ptr(x);
  if (COMP_VALUE_P(y)) {
    p1->v.real = p2->v.real;
    p1->v.imaginary = p2->v.imaginary;
    b1->flags |= COMP_VALUE;
    return;
  }
  b1->flags &= ~COMP_VALUE;
#endif
  p1->f.real = p2->f.real;
  p1->f.imaginary = p2->f.imaginary;
}

/*
 * call-seq:
 *   complex.real -> numeric
 *
 * Returns the real part of the complex number, in the class it was
 * given as.
 *
 *   Complex(3, 4).real    #=> 3
 *   Complex(-1.5).real    #=> -1.5
 */
static mrb_value
complex_real(mrb_state *mrb, mrb_value self)
{
  return part_real(mrb, self);
}

/*
 * call-seq:
 *   complex.imaginary -> numeric
 *   complex.imag      -> numeric
 *
 * Returns the imaginary part of the complex number, in the class it was
 * given as.
 *
 *   Complex(3, 4).imaginary  #=> 4
 *   Complex(5).imag          #=> 0
 */
static mrb_value
complex_imaginary(mrb_state *mrb, mrb_value self)
{
  return part_imaginary(mrb, self);
}

/*
 * call-seq:
 *   Complex.rectangular(real, imag = 0) -> complex
 *   Complex.rect(real, imag = 0)        -> complex
 *   Complex(real, imag = 0)             -> complex
 *
 * Returns a complex number with the given real and imaginary parts.
 * The imaginary part defaults to 0 if not specified.
 *
 *   Complex.rectangular(1, 2)  #=> (1+2i)
 *   Complex.rect(3)            #=> (3+0i)
 *   Complex(1, -1)             #=> (1-1i)
 */
static mrb_value
complex_s_rect(mrb_state *mrb, mrb_value self)
{
#ifdef MRB_COMPLEX_FLOAT_ONLY
  mrb_float real, imaginary = 0.0;

  mrb_get_args(mrb, "f|f", &real, &imaginary);
  return complex_new(mrb, real, imaginary);
#else
  mrb_value real, imaginary = mrb_fixnum_value(0);

  mrb_get_args(mrb, "o|o", &real, &imaginary);
  real = part_coerce(mrb, real);
  imaginary = part_coerce(mrb, imaginary);
  return mrb_complex_new_value(mrb, real, imaginary);
#endif
}

/* The Integer this float is, by the rules #to_i has always used. */
static mrb_value
complex_float_to_i(mrb_state *mrb, mrb_value self, mrb_float f)
{
#ifdef MRB_USE_BIGINT
  if (!FIXABLE_FLOAT(f)) {
    return mrb_bint_new_float(mrb, f);
  }
#else
  if (!FIXABLE_FLOAT(f)) {
    mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %v into Integer", self);
  }
#endif
  return mrb_int_value(mrb, (mrb_int)f);
}

/*
 * call-seq:
 *   complex.to_f -> float
 *
 * Returns the real part of the complex number as a float.
 * Raises RangeError if the imaginary part is not zero.
 *
 *   Complex(3, 0).to_f  #=> 3.0
 *   Complex(3, 4).to_f  #=> RangeError: can't convert (3+4i) into Float
 */
mrb_value
mrb_complex_to_f(mrb_state *mrb, mrb_value self)
{
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_VALUE_P(self)) {
    struct mrb_complex *p = complex_ptr(mrb, self);

    if (!part_zero_p(mrb, p->v.imaginary)) {
      mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %v into Float", self);
    }
    return mrb_float_value(mrb, mrb_as_float(mrb, p->v.real));
  }
#endif
  struct mrb_complex *p = complex_ptr(mrb, self);

  if (p->f.imaginary != 0) {
    mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %v into Float", self);
  }

  return mrb_float_value(mrb, p->f.real);
}

/*
 * call-seq:
 *   complex.to_i -> integer
 *
 * Returns the real part of the complex number as an integer.
 * Raises RangeError if the imaginary part is not zero.
 *
 *   Complex(3, 0).to_i  #=> 3
 *   Complex(3, 4).to_i  #=> RangeError: can't convert (3+4i) into Integer
 */
mrb_value
mrb_complex_to_i(mrb_state *mrb, mrb_value self)
{
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_VALUE_P(self)) {
    struct mrb_complex *p = complex_ptr(mrb, self);

    if (!part_zero_p(mrb, p->v.imaginary)) {
      mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %v into Integer", self);
    }
    switch (mrb_type(p->v.real)) {
    case MRB_TT_INTEGER:
#ifdef MRB_USE_BIGINT
    case MRB_TT_BIGINT:
#endif
      return p->v.real;
#ifdef MRB_USE_RATIONAL
    case MRB_TT_RATIONAL:
      return mrb_rational_to_i(mrb, p->v.real);
#endif
    default:
      return complex_float_to_i(mrb, self, mrb_float(p->v.real));
    }
  }
#endif
  struct mrb_complex *p = complex_ptr(mrb, self);

  if (p->f.imaginary != 0) {
    mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %v into Integer", self);
  }
  return complex_float_to_i(mrb, self, p->f.real);
}

mrb_bool
mrb_complex_eq(mrb_state *mrb, mrb_value x, mrb_value y)
{
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_VALUE_P(x) || (mrb_type(y) == MRB_TT_COMPLEX && COMP_VALUE_P(y))) {
    switch (mrb_type(y)) {
    case MRB_TT_COMPLEX:
      return part_eq(mrb, part_real(mrb, x), part_real(mrb, y)) &&
             part_eq(mrb, part_imaginary(mrb, x), part_imaginary(mrb, y));
    case MRB_TT_INTEGER:
    case MRB_TT_FLOAT:
#ifdef MRB_USE_BIGINT
    case MRB_TT_BIGINT:
#endif
#ifdef MRB_USE_RATIONAL
    case MRB_TT_RATIONAL:
#endif
      return part_zero_p(mrb, part_imaginary(mrb, x)) &&
             part_eq(mrb, part_real(mrb, x), y);
    default:
      return mrb_equal(mrb, y, x);
    }
  }
#endif
  struct mrb_complex *p1 = complex_ptr(mrb, x);

  switch (mrb_type(y)) {
  case MRB_TT_COMPLEX:
    {
      struct mrb_complex *p2 = complex_ptr(mrb, y);

      if (p1->f.real == p2->f.real && p1->f.imaginary == p2->f.imaginary) {
        return TRUE;
      }
      return FALSE;
    }
  case MRB_TT_INTEGER:
    if (p1->f.imaginary != 0) return FALSE;
    return p1->f.real == mrb_integer(y);
  case MRB_TT_FLOAT:
    if (p1->f.imaginary != 0) return FALSE;
    return p1->f.real == mrb_float(y);

  default:
    return mrb_equal(mrb, y, x);
  }
}

/*
 * call-seq:
 *   complex == object -> true or false
 *
 * Returns true if complex equals object. Two complex numbers are equal
 * if their real and imaginary parts are equal.
 *
 *   Complex(1, 2) == Complex(1, 2)  #=> true
 *   Complex(1, 2) == Complex(2, 1)  #=> false
 *   Complex(1, 0) == 1              #=> true
 */
static mrb_value
complex_eq(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_bool_value(mrb_complex_eq(mrb, x, y));
}

#ifndef MRB_COMPLEX_FLOAT_ONLY
static mrb_bool
part_eql(mrb_state *mrb, mrb_value a, mrb_value b)
{
  if (mrb_type(a) != mrb_type(b)) return FALSE;
  return part_eq(mrb, a, b);
}

/*
 * call-seq:
 *   complex.eql?(object) -> true or false
 *
 * Returns true if object is a Complex whose parts are of the same classes
 * and equal.  `==` converts across part classes; `eql?` must not, because
 * #hash keys each part by its class as well as its value.
 *
 *   Complex(1, 2).eql?(Complex(1, 2))      #=> true
 *   Complex(1, 2).eql?(Complex(1.0, 2.0))  #=> false
 */
static mrb_value
complex_eql(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);

  if (mrb_type(y) != MRB_TT_COMPLEX) return mrb_false_value();
  return mrb_bool_value(part_eql(mrb, part_real(mrb, x), part_real(mrb, y)) &&
                        part_eql(mrb, part_imaginary(mrb, x), part_imaginary(mrb, y)));
}

static mrb_value
complex_op_value(mrb_state *mrb, mrb_value x, mrb_value y, char op)
{
  mrb_value ar = part_real(mrb, x);
  mrb_value ai = part_imaginary(mrb, x);

  if (mrb_type(y) != MRB_TT_COMPLEX) {
    /* part-wise, not the four-product formula with a zero imaginary part:
       multiplying an Integer part by that zero would answer in whatever
       class the other side's arithmetic returns, not the part's own */
    mrb_value s = part_coerce(mrb, y);

    switch (op) {
    case '+':
      return mrb_complex_new_value(mrb, mrb_num_add(mrb, ar, s), ai);
    case '-':
      return mrb_complex_new_value(mrb, mrb_num_sub(mrb, ar, s), ai);
    default: /* '*' */
      return mrb_complex_new_value(mrb, mrb_num_mul(mrb, ar, s), mrb_num_mul(mrb, ai, s));
    }
  }

  mrb_value br = part_real(mrb, y);
  mrb_value bi = part_imaginary(mrb, y);

  switch (op) {
  case '+':
    return mrb_complex_new_value(mrb, mrb_num_add(mrb, ar, br), mrb_num_add(mrb, ai, bi));
  case '-':
    return mrb_complex_new_value(mrb, mrb_num_sub(mrb, ar, br), mrb_num_sub(mrb, ai, bi));
  default: /* '*' */
    {
      mrb_value r = mrb_num_sub(mrb, mrb_num_mul(mrb, ar, br), mrb_num_mul(mrb, ai, bi));
      mrb_value i = mrb_num_add(mrb, mrb_num_mul(mrb, ar, bi), mrb_num_mul(mrb, ai, br));
      return mrb_complex_new_value(mrb, r, i);
    }
  }
}

/* A pair of float-form operands takes the float arms below; anything
   holding a value form goes part-wise through the tower's own dispatch. */
#define COMP_OP_VALUE_P(x, y) \
  (COMP_VALUE_P(x) || (mrb_type(y) == MRB_TT_COMPLEX && COMP_VALUE_P(y)))
#endif /* MRB_COMPLEX_FLOAT_ONLY */

static mrb_value
complex_op(mrb_state *mrb, mrb_value x, mrb_value y, char op)
{
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_OP_VALUE_P(x, y)) {
    return complex_op_value(mrb, x, y, op);
  }
#endif
  struct mrb_complex *p1 = complex_ptr(mrb, x);
  mrb_float r, i;

  switch (mrb_type(y)) {
  case MRB_TT_COMPLEX: {
    struct mrb_complex *p2 = complex_ptr(mrb, y);
    r = p2->f.real;
    i = p2->f.imaginary;
    break;
  }
  default: {
    r = mrb_as_float(mrb, y);
    i = 0;
    break;
  }
  }

  switch (op) {
  case '+':
    return mrb_complex_new(mrb, p1->f.real + r, p1->f.imaginary + i);
  case '-':
    return mrb_complex_new(mrb, p1->f.real - r, p1->f.imaginary - i);
  case '*':
    return mrb_complex_new(mrb, p1->f.real * r - p1->f.imaginary * i, p1->f.real * i + p1->f.imaginary * r);
  }
  return mrb_nil_value(); /* should not happen */
}

mrb_value
mrb_complex_add(mrb_state *mrb, mrb_value x, mrb_value y)
{
  return complex_op(mrb, x, y, '+');
}

/*
 * call-seq:
 *   complex + numeric -> complex
 *
 * Returns the sum of complex and numeric. If numeric is a complex number,
 * adds both real and imaginary parts. If numeric is real, adds only to
 * the real part.
 *
 *   Complex(1, 2) + Complex(3, 4)  #=> (4+6i)
 *   Complex(1, 2) + 3              #=> (4+2i)
 */
static mrb_value
complex_add(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_complex_add(mrb, x, y);
}

mrb_value
mrb_complex_sub(mrb_state *mrb, mrb_value x, mrb_value y)
{
  return complex_op(mrb, x, y, '-');
}

/*
 * call-seq:
 *   complex - numeric -> complex
 *
 * Returns the difference of complex and numeric. If numeric is a complex number,
 * subtracts both real and imaginary parts. If numeric is real, subtracts only
 * from the real part.
 *
 *   Complex(5, 6) - Complex(1, 2)  #=> (4+4i)
 *   Complex(5, 6) - 2              #=> (3+6i)
 */
static mrb_value
complex_sub(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_complex_sub(mrb, x, y);
}

mrb_value
mrb_complex_mul(mrb_state *mrb, mrb_value x, mrb_value y)
{
  return complex_op(mrb, x, y, '*');
}

/*
 * call-seq:
 *   complex * numeric -> complex
 *
 * Returns the product of complex and numeric. Uses the standard complex
 * multiplication formula: (a+bi) * (c+di) = (ac-bd) + (ad+bc)i
 *
 *   Complex(1, 2) * Complex(3, 4)  #=> (-5+10i)
 *   Complex(1, 2) * 3              #=> (3+6i)
 */
static mrb_value
complex_mul(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_complex_mul(mrb, x, y);
}

/* Arithmetic on (significand, exponent) pairs avoids premature overflow in
   complex division */
struct float_pair {
  mrb_float s;
  int x;
};

static void
add_pair(struct float_pair *s, struct float_pair const *a,
         struct float_pair const *b)
{
  if (b->s == 0.0F) {
    *s = *a;
  }
  else if (a->s == 0.0F) {
    *s = *b;
  }
  else if (a->x >= b->x) {
    s->s = a->s + F(ldexp)(b->s, b->x - a->x);
    s->x = a->x;
  }
  else {
    s->s = F(ldexp)(a->s, a->x - b->x) + b->s;
    s->x = b->x;
  }
}

static void
mul_pair(struct float_pair *p, struct float_pair const *a,
         struct float_pair const *b)
{
  p->s = a->s * b->s;
  p->x = a->x + b->x;
}

static void
div_pair(struct float_pair *q, struct float_pair const *a,
         struct float_pair const *b)
{
  q->s = mrb_div_float(a->s, b->s);
  q->x = a->x - b->x;
}

#if !defined(MRB_COMPLEX_FLOAT_ONLY) && defined(MRB_USE_RATIONAL)
/* Exact division of two parts: what Integer#quo answers, folded back to an
   Integer when the quotient has denominator 1, the way every part of an
   exact quotient reads in CRuby. */
static mrb_value
part_quo(mrb_state *mrb, mrb_value a, mrb_value b)
{
  return mrb_rational_canonicalize(mrb, mrb_rational_div(mrb, mrb_as_rational(mrb, a), b));
}

/* One part divided by an exact real scalar: a Float part divides as a
   float, an exact part quotients exactly, the way CRuby's f_divide runs
   quo over each part and canonicalizes its scalar arm. */
static mrb_value
part_quo_scalar(mrb_state *mrb, mrb_value part, mrb_value rhs)
{
  if (mrb_float_p(part)) {
    return mrb_float_value(mrb, mrb_div_float(mrb_float(part), mrb_as_float(mrb, rhs)));
  }
  return part_quo(mrb, part, rhs);
}

static mrb_bool
comp_no_float_parts_p(mrb_state *mrb, mrb_value cpx)
{
  if (!COMP_VALUE_P(cpx)) return FALSE;
  struct mrb_complex *p = complex_ptr(mrb, cpx);
  return !mrb_float_p(p->v.real) && !mrb_float_p(p->v.imaginary);
}

/* A complex divisor stays exact only while no float is anywhere in it, the
   same condition CRuby's f_divide asks before it skips canonicalization. */
static mrb_bool
comp_div_exact_p(mrb_state *mrb, mrb_value x, mrb_value rhs)
{
  return comp_no_float_parts_p(mrb, x) && comp_no_float_parts_p(mrb, rhs);
}

static mrb_value
complex_div_exact(mrb_state *mrb, mrb_value x, mrb_value rhs)
{
  mrb_value ar = part_real(mrb, x);
  mrb_value ai = part_imaginary(mrb, x);

  mrb_value br = part_real(mrb, rhs);
  mrb_value bi = part_imaginary(mrb, rhs);
  /* multiply through by the conjugate; with exact parts there is no
     rounding for the float arm's r-scaling to save */
  mrb_value n = mrb_num_add(mrb, mrb_num_mul(mrb, br, br), mrb_num_mul(mrb, bi, bi));
  if (part_zero_p(mrb, n)) mrb_int_zerodiv(mrb);
  mrb_value zr = mrb_num_add(mrb, mrb_num_mul(mrb, ar, br), mrb_num_mul(mrb, ai, bi));
  mrb_value zi = mrb_num_sub(mrb, mrb_num_mul(mrb, ai, br), mrb_num_mul(mrb, ar, bi));
  return mrb_complex_new_value(mrb, part_quo(mrb, zr, n), part_quo(mrb, zi, n));
}
#endif /* !MRB_COMPLEX_FLOAT_ONLY && MRB_USE_RATIONAL */

mrb_value
mrb_complex_div(mrb_state *mrb, mrb_value self, mrb_value rhs)
{
#if !defined(MRB_COMPLEX_FLOAT_ONLY) && defined(MRB_USE_RATIONAL)
  /* The class check is runtime, not compile-time, because mrbtest runs each
     gem's tests in a state that initializes only its declared dependencies;
     without Rational the quotient falls to the float arms, the same answer
     Integer#quo gives there. */
  if (mrb_type(rhs) != MRB_TT_COMPLEX) {
    if (COMP_VALUE_P(self) && part_exact_type_p(rhs) &&
        mrb_class_defined_id(mrb, MRB_SYM(Rational))) {
      if (part_zero_p(mrb, rhs)) mrb_int_zerodiv(mrb);
      return mrb_complex_new_value(mrb,
                                   part_quo_scalar(mrb, part_real(mrb, self), rhs),
                                   part_quo_scalar(mrb, part_imaginary(mrb, self), rhs));
    }
  }
  else if (comp_div_exact_p(mrb, self, rhs) &&
           mrb_class_defined_id(mrb, MRB_SYM(Rational))) {
    return complex_div_exact(mrb, self, rhs);
  }
#endif
  mrb_float ar = comp_float_real(mrb, self);
  mrb_float ai = comp_float_imaginary(mrb, self);
  mrb_float r, den;

  if (mrb_type(rhs) != MRB_TT_COMPLEX) {
    if (mrb_integer_p(rhs) && mrb_integer(rhs) == 0) {
      mrb_int_zerodiv(mrb);
    }
    mrb_float f = mrb_as_float(mrb, rhs);
    if (f == 0.0) {
      mrb_int_zerodiv(mrb);
    }
    return complex_new(mrb, mrb_div_float(ar, f), mrb_div_float(ai, f));
  }

  mrb_float br = comp_float_real(mrb, rhs);
  mrb_float bi = comp_float_imaginary(mrb, rhs);
  if (br == 0 && bi == 0) {
    mrb_int_zerodiv(mrb);
  }

  if (F(fabs)(br) < DBL_MIN * F(fabs)(bi) && F(fabs)(bi) < DBL_MIN * F(fabs)(br)) {
    /* Fallback to frexp/ldexp for extreme values */
    struct float_pair ar_p, ai_p, br_p, bi_p;
    struct float_pair br2_p, bi2_p;
    struct float_pair div_p;
    struct float_pair ar_br_p, ai_bi_p;
    struct float_pair ai_br_p, ar_bi_p;
    struct float_pair zr_p, zi_p;

    ar_p.s = F(frexp)(ar, &ar_p.x);
    ai_p.s = F(frexp)(ai, &ai_p.x);
    br_p.s = F(frexp)(br, &br_p.x);
    bi_p.s = F(frexp)(bi, &bi_p.x);

    mul_pair(&br2_p, &br_p, &br_p);
    mul_pair(&bi2_p, &bi_p, &bi_p);
    add_pair(&div_p, &br2_p, &bi2_p);

    mul_pair(&ar_br_p, &ar_p, &br_p);
    mul_pair(&ai_bi_p, &ai_p, &bi_p);
    add_pair(&zr_p, &ar_br_p, &ai_bi_p);
    div_pair(&zr_p, &zr_p, &div_p);

    mul_pair(&ai_br_p, &ai_p, &br_p);
    mul_pair(&ar_bi_p, &ar_p, &bi_p);
    ar_bi_p.s = -ar_bi_p.s;
    add_pair(&zi_p, &ai_br_p, &ar_bi_p);
    div_pair(&zi_p, &zi_p, &div_p);

    return complex_new(mrb, F(ldexp)(zr_p.s, zr_p.x), F(ldexp)(zi_p.s, zi_p.x));
  }
  else {
    if (F(fabs)(br) > F(fabs)(bi)) {
      r = bi / br;
      den = br + r * bi;
      return complex_new(mrb, (ar + ai * r) / den, (ai - ar * r) / den);
    }
    else {
      r = br / bi;
      den = bi + r * br;
      return complex_new(mrb, (ar * r + ai) / den, (ai * r - ar) / den);
    }
  }
}

/*
 * call-seq:
 *   complex / numeric -> complex
 *   complex.quo(numeric) -> complex
 *
 * Returns the quotient of complex divided by numeric. With no Float
 * anywhere in it the quotient is exact; otherwise it divides with the
 * standard float algorithm.
 *
 *   Complex(10, 5) / Complex(2, 1)  #=> (5+0i)
 *   Complex(6, 4) / 2               #=> (3+2i)
 */
static mrb_value
complex_div(mrb_state *mrb, mrb_value x)
{
  mrb_value y = mrb_get_arg1(mrb);
  return mrb_complex_div(mrb, x, y);
}

#ifndef MRB_COMPLEX_FLOAT_ONLY
/* One part's contribution to #hash.  Keyed by class as well as value, the
   same distinction #eql? draws; both float zeros hash as one key because
   0.0 and -0.0 are eql?. */
static uint32_t
part_hash32(mrb_state *mrb, mrb_value v)
{
  switch (mrb_type(v)) {
  case MRB_TT_INTEGER:
    {
      mrb_int i = mrb_integer(v);
      return mrb_byte_hash((uint8_t*)&i, sizeof(i));
    }
#ifdef MRB_USE_BIGINT
  case MRB_TT_BIGINT:
    return (uint32_t)mrb_integer(mrb_bint_hash(mrb, v));
#endif
#ifdef MRB_USE_RATIONAL
  case MRB_TT_RATIONAL:
    return (uint32_t)mrb_integer(mrb_rational_hash(mrb, v));
#endif
  default:
    {
      mrb_float f = mrb_float(v);
      if (f == 0.0) f = 0.0;
      return mrb_byte_hash((uint8_t*)&f, sizeof(f));
    }
  }
}
#endif

/*
 * call-seq:
 *   complex.hash -> integer
 *
 * Returns a hash value for the complex number. Two complex numbers with
 * eql? parts will have the same hash value.
 *
 *   Complex(1, 2).hash == Complex(1, 2).hash  #=> true
 */
static mrb_value
complex_hash(mrb_state *mrb, mrb_value cpx)
{
  uint32_t hash;
#ifndef MRB_COMPLEX_FLOAT_ONLY
  if (COMP_VALUE_P(cpx)) {
    struct mrb_complex *c = complex_ptr(mrb, cpx);
    uint32_t hr = part_hash32(mrb, c->v.real);
    uint32_t hi = part_hash32(mrb, c->v.imaginary);
    hash = mrb_byte_hash((uint8_t*)&hr, sizeof(hr));
    hash = mrb_byte_hash_step((uint8_t*)&hi, sizeof(hi), hash);
    return mrb_int_value(mrb, hash);
  }
#endif
  struct mrb_complex *c = complex_ptr(mrb, cpx);
  /* -0.0 == 0.0, and Float#eql? agrees, so the two must hash alike */
  mrb_float fr = c->f.real;
  mrb_float fi = c->f.imaginary;
  if (fr == 0.0) fr = 0.0;
  if (fi == 0.0) fi = 0.0;
  hash = mrb_byte_hash((uint8_t*)&fr, sizeof(mrb_float));
  hash = mrb_byte_hash_step((uint8_t*)&fi, sizeof(mrb_float), hash);
  return mrb_int_value(mrb, hash);
}

/*
 * call-seq:
 *   nil.to_c -> complex
 *
 * Returns Complex(0, 0).
 *
 *   nil.to_c  #=> (0+0i)
 */
static mrb_value
nil_to_c(mrb_state *mrb, mrb_value self)
{
#ifdef MRB_COMPLEX_FLOAT_ONLY
  return complex_new(mrb, 0, 0);
#else
  return mrb_complex_new_value(mrb, mrb_fixnum_value(0), mrb_fixnum_value(0));
#endif
}

static mrb_value
complex_one(mrb_state *mrb)
{
#ifdef MRB_COMPLEX_FLOAT_ONLY
  return complex_new(mrb, 1, 0);
#else
  return mrb_complex_new_value(mrb, mrb_fixnum_value(1), mrb_fixnum_value(0));
#endif
}

/* Square-and-multiply, n >= 0.  Exact parts stay exact; float parts repeat
   the same float multiply, which still beats the polar arm's transcendental
   round trip: (1+2i)**2 is (-3+4i), not (-3+4.000000000000002i). */
static mrb_value
complex_pow_int(mrb_state *mrb, mrb_value x, mrb_int n)
{
  mrb_value r = complex_one(mrb);
  mrb_value z = x;
  int ai = mrb_gc_arena_save(mrb);

  while (n > 0) {
    if (n & 1) r = mrb_complex_mul(mrb, r, z);
    n >>= 1;
    if (n) z = mrb_complex_mul(mrb, z, z);
    mrb_gc_arena_restore(mrb, ai);
    mrb_gc_protect(mrb, r);
    mrb_gc_protect(mrb, z);
  }
  return r;
}

/*
 * call-seq:
 *   cmp ** numeric -> complex
 *
 * Returns the result of raising cmp to the power of numeric.
 *
 *   Complex(1, 2) ** 2            #=> (-3+4i)
 *   Complex(1, 2) ** Complex(1, 0) #=> (1+2i)
 */
static mrb_value
complex_pow(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  if (mrb_integer_p(other)) {
    mrb_int n = mrb_integer(other);
    if (n >= 0) return complex_pow_int(mrb, self, n);
    /* -MRB_INT_MIN has no mrb_int negation; that one exponent answers its
       magnitude-zero-or-infinity question in the polar arm below */
    if (n != MRB_INT_MIN) {
      return mrb_complex_div(mrb, complex_one(mrb), complex_pow_int(mrb, self, -n));
    }
  }

  mrb_float self_real = comp_float_real(mrb, self);
  mrb_float self_imaginary = comp_float_imaginary(mrb, self);

  if (mrb_type(other) == MRB_TT_COMPLEX) {
    mrb_float x = comp_float_real(mrb, other);
    mrb_float y = comp_float_imaginary(mrb, other);

    mrb_float log_abs_self = F(log)(F(hypot)(self_real, self_imaginary));
    mrb_float arg_self = F(atan2)(self_imaginary, self_real);

    mrb_float a = x * log_abs_self - y * arg_self;
    mrb_float b = x * arg_self + y * log_abs_self;

    mrb_float exp_a = F(exp)(a);
    return mrb_complex_new(mrb, exp_a * F(cos)(b), exp_a * F(sin)(b));
  }
  else {
    mrb_float other_float = mrb_as_float(mrb, other);

    mrb_float abs_self = F(hypot)(self_real, self_imaginary);
    mrb_float arg_self = F(atan2)(self_imaginary, self_real);

    mrb_float pow_abs_self = F(pow)(abs_self, other_float);
    mrb_float new_arg = arg_self * other_float;

    return mrb_complex_new(mrb, pow_abs_self * F(cos)(new_arg), pow_abs_self * F(sin)(new_arg));
  }
}

/* ---------------------------*/
static const mrb_mt_entry complex_rom_entries[] = {
  MRB_MT_ENTRY(complex_real,      MRB_SYM(real),   MRB_ARGS_NONE()),
  MRB_MT_ENTRY(complex_imaginary, MRB_SYM(imaginary), MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mrb_complex_to_f,  MRB_SYM(to_f),   MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mrb_complex_to_i,  MRB_SYM(to_i),   MRB_ARGS_NONE()),
  MRB_MT_ENTRY(mrb_obj_itself,    MRB_SYM(to_c),   MRB_ARGS_NONE()),
  MRB_MT_ENTRY(complex_add,       MRB_OPSYM(add), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(complex_sub,       MRB_OPSYM(sub), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(complex_mul,       MRB_OPSYM(mul), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(complex_div,       MRB_OPSYM(div), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(complex_div,       MRB_SYM(quo), MRB_ARGS_REQ(1)),
  MRB_MT_ENTRY(complex_eq,        MRB_OPSYM(eq), MRB_ARGS_REQ(1)),
#ifndef MRB_COMPLEX_FLOAT_ONLY
  MRB_MT_ENTRY(complex_eql,       MRB_SYM_Q(eql), MRB_ARGS_REQ(1)),
#endif
  MRB_MT_ENTRY(complex_hash,      MRB_SYM(hash),   MRB_ARGS_NONE()),
  MRB_MT_ENTRY(complex_pow,       MRB_OPSYM(pow), MRB_ARGS_REQ(1)),
};

void mrb_mruby_complex_gem_init(mrb_state *mrb)
{
  struct RClass *comp;

  comp = mrb_define_class_id(mrb, MRB_SYM(Complex), mrb_class_get_id(mrb, MRB_SYM(Numeric)));
  MRB_SET_INSTANCE_TT(comp, MRB_TT_COMPLEX);
  MRB_UNDEF_ALLOCATOR(comp);

  mrb_undef_class_method_id(mrb, comp, MRB_SYM(new));
  mrb_define_class_method_id(mrb, comp, MRB_SYM(rectangular), complex_s_rect, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, comp, MRB_SYM(rect), complex_s_rect, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));

  MRB_MT_INIT_ROM(mrb, comp, complex_rom_entries);
  mrb_define_method_id(mrb, mrb->nil_class, MRB_SYM(to_c), nil_to_c, MRB_ARGS_NONE());
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(Complex), complex_s_rect, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
}

void
mrb_mruby_complex_gem_final(mrb_state* mrb)
{
}
