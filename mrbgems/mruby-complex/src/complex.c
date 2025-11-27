#include <mruby.h>
#include <mruby/class.h>
#include <mruby/numeric.h>
#include <mruby/internal.h>
#include <mruby/presym.h>
#include <float.h>

#ifdef MRB_NO_FLOAT
# error Complex conflicts with 'MRB_NO_FLOAT' configuration
#endif

#ifdef MRB_USE_FLOAT32
#define F(x) x##f
#else
#define F(x) x
#endif

struct mrb_complex {
  mrb_float real;
  mrb_float imaginary;
};

#if defined(MRB_32BIT) && !defined(MRB_USE_FLOAT32)

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

void
mrb_complex_get(mrb_state *mrb, mrb_value cpx, mrb_float *r, mrb_float *i)
{
  struct mrb_complex *c = complex_ptr(mrb, cpx);

  *r = c->real;
  *i = c->imaginary;
}

mrb_value
mrb_complex_new(mrb_state *mrb, mrb_float real, mrb_float imaginary)
{
  struct RClass *c = mrb_class_get_id(mrb, MRB_SYM(Complex));
  struct mrb_complex *p;
  struct RBasic *comp = complex_alloc(mrb, c, &p);
  p->real = real;
  p->imaginary = imaginary;
  comp->frozen = 1;

  return mrb_obj_value(comp);
}

#define complex_new(mrb, real, imag) mrb_complex_new(mrb, real, imag)

void
mrb_complex_copy(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct mrb_complex *p1 = complex_ptr(mrb, x);
  struct mrb_complex *p2 = complex_ptr(mrb, y);
  p1->real = p2->real;
  p1->imaginary = p2->imaginary;
}

/*
 * call-seq:
 *   complex.real -> float
 *
 * Returns the real part of the complex number.
 *
 *   Complex(3, 4).real  #=> 3.0
 *   Complex(-1).real    #=> -1.0
 */
static mrb_value
complex_real(mrb_state *mrb, mrb_value self)
{
  struct mrb_complex *p = complex_ptr(mrb, self);
  return mrb_float_value(mrb, p->real);
}

/*
 * call-seq:
 *   complex.imaginary -> float
 *   complex.imag      -> float
 *
 * Returns the imaginary part of the complex number.
 *
 *   Complex(3, 4).imaginary  #=> 4.0
 *   Complex(5).imag          #=> 0.0
 */
static mrb_value
complex_imaginary(mrb_state *mrb, mrb_value self)
{
  struct mrb_complex *p = complex_ptr(mrb, self);
  return mrb_float_value(mrb, p->imaginary);
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
  mrb_float real, imaginary = 0.0;

  mrb_get_args(mrb, "f|f", &real, &imaginary);
  return complex_new(mrb, real, imaginary);
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
  struct mrb_complex *p = complex_ptr(mrb, self);

  if (p->imaginary != 0) {
    mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %v into Float", self);
  }

  return mrb_float_value(mrb, p->real);
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
  struct mrb_complex *p = complex_ptr(mrb, self);

#ifdef MRB_USE_BIGINT
  if (p->imaginary != 0) {
    mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %v into Integer", self);
  }
  if (!FIXABLE_FLOAT(p->real)) {
    return mrb_bint_new_float(mrb, p->real);
  }
#else
  if (p->imaginary != 0 || !FIXABLE_FLOAT(p->real)) {
    mrb_raisef(mrb, E_RANGE_ERROR, "can't convert %v into Integer", self);
  }
#endif
  return mrb_int_value(mrb, (mrb_int)p->real);
}

mrb_bool
mrb_complex_eq(mrb_state *mrb, mrb_value x, mrb_value y)
{
  struct mrb_complex *p1 = complex_ptr(mrb, x);

  switch (mrb_type(y)) {
  case MRB_TT_COMPLEX:
    {
      struct mrb_complex *p2 = complex_ptr(mrb, y);

      if (p1->real == p2->real && p1->imaginary == p2->imaginary) {
        return TRUE;
      }
      return FALSE;
    }
  case MRB_TT_INTEGER:
    if (p1->imaginary != 0) return FALSE;
    return p1->real == mrb_integer(y);
  case MRB_TT_FLOAT:
    if (p1->imaginary != 0) return FALSE;
    return p1->real == mrb_float(y);

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

static mrb_value
complex_op(mrb_state *mrb, mrb_value x, mrb_value y, char op)
{
  struct mrb_complex *p1 = complex_ptr(mrb, x);
  mrb_float r, i;

  switch (mrb_type(y)) {
  case MRB_TT_COMPLEX: {
    struct mrb_complex *p2 = complex_ptr(mrb, y);
    r = p2->real;
    i = p2->imaginary;
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
    return mrb_complex_new(mrb, p1->real + r, p1->imaginary + i);
  case '-':
    return mrb_complex_new(mrb, p1->real - r, p1->imaginary - i);
  case '*':
    return mrb_complex_new(mrb, p1->real * r - p1->imaginary * i, p1->real * i + p1->imaginary * r);
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

mrb_value
mrb_complex_div(mrb_state *mrb, mrb_value self, mrb_value rhs)
{
  struct mrb_complex *a, *b;
  mrb_float r, den;

  a = complex_ptr(mrb, self);
  if (mrb_type(rhs) != MRB_TT_COMPLEX) {
    if (mrb_integer_p(rhs) && mrb_integer(rhs) == 0) {
      mrb_int_zerodiv(mrb);
    }
    mrb_float f = mrb_as_float(mrb, rhs);
    if (f == 0.0) {
      mrb_int_zerodiv(mrb);
    }
    return complex_new(mrb, mrb_div_float(a->real, f), mrb_div_float(a->imaginary, f));
  }

  b = complex_ptr(mrb, rhs);
  if (b->real == 0 && b->imaginary == 0) {
    mrb_int_zerodiv(mrb);
  }

  mrb_float br = b->real;
  mrb_float bi = b->imaginary;

  if (F(fabs)(br) < DBL_MIN * F(fabs)(bi) && F(fabs)(bi) < DBL_MIN * F(fabs)(br)) {
    /* Fallback to frexp/ldexp for extreme values */
    struct float_pair ar_p, ai_p, br_p, bi_p;
    struct float_pair br2_p, bi2_p;
    struct float_pair div_p;
    struct float_pair ar_br_p, ai_bi_p;
    struct float_pair ai_br_p, ar_bi_p;
    struct float_pair zr_p, zi_p;

    ar_p.s = F(frexp)(a->real, &ar_p.x);
    ai_p.s = F(frexp)(a->imaginary, &ai_p.x);
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
      return complex_new(mrb, (a->real + a->imaginary * r) / den, (a->imaginary - a->real * r) / den);
    }
    else {
      r = br / bi;
      den = bi + r * br;
      return complex_new(mrb, (a->real * r + a->imaginary) / den, (a->imaginary * r - a->real) / den);
    }
  }
}

/*
 * call-seq:
 *   complex / numeric -> complex
 *   complex.quo(numeric) -> complex
 *
 * Returns the quotient of complex divided by numeric. Uses the standard
 * complex division formula by multiplying by the conjugate.
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

/*
 * call-seq:
 *   complex.hash -> integer
 *
 * Returns a hash value for the complex number. Two complex numbers with
 * the same real and imaginary parts will have the same hash value.
 *
 *   Complex(1, 2).hash == Complex(1, 2).hash  #=> true
 */
static mrb_value
complex_hash(mrb_state *mrb, mrb_value cpx)
{
  struct mrb_complex *c = complex_ptr(mrb, cpx);
  uint32_t hash = mrb_byte_hash((uint8_t*)&c->real, sizeof(mrb_float));
  hash = mrb_byte_hash_step((uint8_t*)&c->imaginary, sizeof(mrb_float), hash);
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
  return complex_new(mrb, 0, 0);
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
  struct mrb_complex *c_self = complex_ptr(mrb, self);
  mrb_float self_real = c_self->real;
  mrb_float self_imaginary = c_self->imaginary;

  if (mrb_type(other) == MRB_TT_COMPLEX) {
    struct mrb_complex *c_other = complex_ptr(mrb, other);
    mrb_float x = c_other->real;
    mrb_float y = c_other->imaginary;

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

void mrb_mruby_complex_gem_init(mrb_state *mrb)
{
  struct RClass *comp;

  comp = mrb_define_class_id(mrb, MRB_SYM(Complex), mrb_class_get_id(mrb, MRB_SYM(Numeric)));
  MRB_SET_INSTANCE_TT(comp, MRB_TT_COMPLEX);
  MRB_UNDEF_ALLOCATOR(comp);

  mrb_undef_class_method_id(mrb, comp, MRB_SYM(new));
  mrb_define_class_method_id(mrb, comp, MRB_SYM(rectangular), complex_s_rect, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, comp, MRB_SYM(rect), complex_s_rect, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_private_method_id(mrb, mrb->kernel_module, MRB_SYM(Complex), complex_s_rect, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, comp, MRB_SYM(real), complex_real, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, comp, MRB_SYM(imaginary), complex_imaginary, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, comp, MRB_SYM(to_f), mrb_complex_to_f, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, comp, MRB_SYM(to_i), mrb_complex_to_i, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, comp, MRB_SYM(to_c), mrb_obj_itself, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, comp, MRB_OPSYM(add), complex_add, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, comp, MRB_OPSYM(sub), complex_sub, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, comp, MRB_OPSYM(mul), complex_mul, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, comp, MRB_OPSYM(div), complex_div, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, comp, MRB_SYM(quo), complex_div, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, comp, MRB_OPSYM(eq), complex_eq, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, comp, MRB_SYM(hash), complex_hash, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, comp, MRB_OPSYM(pow), complex_pow, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, mrb->nil_class, MRB_SYM(to_c), nil_to_c, MRB_ARGS_NONE());
}

void
mrb_mruby_complex_gem_final(mrb_state* mrb)
{
}
