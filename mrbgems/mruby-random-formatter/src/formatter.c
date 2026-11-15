/*
** formatter.c - Random::Formatter
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/class.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/presym.h>
#include <mruby/internal.h>
#include <string.h>
#ifndef MRB_NO_FLOAT
#include <math.h>
#endif

/*
 * random_number and rand draw from the receiver's #bytes(n): Random#bytes
 * for a Random, Random.bytes for Random itself, and whatever bytes any
 * other object that includes the module defines, as CRuby's do.
 */

static mrb_noreturn void
invalid_argument(mrb_state *mrb, mrb_value v)
{
  mrb_raisef(mrb, E_ARGUMENT_ERROR, "invalid argument - %v", v);
}

/* fills `len` random bytes at `p` */
static void
fmt_fill(mrb_state *mrb, mrb_value self, uint8_t *p, mrb_int len)
{
  int ai = mrb_gc_arena_save(mrb);
  mrb_value b = mrb_funcall_id(mrb, self, MRB_SYM(bytes), 1, mrb_int_value(mrb, len));
  b = mrb_ensure_string_type(mrb, b);
  if (RSTRING_LEN(b) != len) {
    mrb_raisef(mrb, E_RANGE_ERROR, "random data too %s %i",
               RSTRING_LEN(b) < len ? "short" : "long", RSTRING_LEN(b));
  }
  memcpy(p, RSTRING_PTR(b), len);
  mrb_gc_arena_restore(mrb, ai);
}

/* `len` (4 or 8) random bytes as an integer */
static uint64_t
fmt_draw(mrb_state *mrb, mrb_value self, int len)
{
  uint8_t buf[8];
  uint64_t v = 0;

  fmt_fill(mrb, self, buf, len);
  while (len-- > 0) {
    v = (v << 8) | buf[len];
  }
  return v;
}

/* Uniform value in [0, span) without modulo bias, as rand_u gives; span == 0
   selects the entire domain of mrb_uint. A span that fits in 32 bits draws
   4 bytes a try, and a larger one 8. */
static mrb_uint
fmt_limited(mrb_state *mrb, mrb_value self, mrb_uint span)
{
  if (span == 0) return (mrb_uint)fmt_draw(mrb, self, sizeof(mrb_uint));

  uint64_t n = span;
  int len = n - 1 > UINT32_MAX ? 8 : 4;
  uint64_t top = len == 4 ? (uint64_t)1 << 32 : 0;  /* 2**64 wraps to 0 */
  uint64_t tail = (top - n) % n;                   /* == 2**(8*len) % n */
  uint64_t r;
  /* draw again in the biased tail at the top */
  do {
    r = fmt_draw(mrb, self, len);
  } while (tail != 0 && r >= top - tail);
  return (mrb_uint)(r % n);
}

#ifndef MRB_NO_FLOAT
/* as many random bits as mrb_float holds, 53 or 24, as a Float in [0, 1);
   one bit more would round up to 1.0 */
static mrb_float
fmt_real(mrb_state *mrb, mrb_value self)
{
#ifdef MRB_USE_FLOAT32
  return (mrb_float)(fmt_draw(mrb, self, 8) >> 40) * (1.0f/16777216.0f);
#else
  return (mrb_float)(fmt_draw(mrb, self, 8) >> 11) * (1.0/9007199254740992.0);
#endif
}

#ifdef MRB_USE_FLOAT32
# define fmt_nextafter(x, y) nextafterf(x, y)
#else
# define fmt_nextafter(x, y) nextafter(x, y)
#endif

static mrb_float
fmt_float_arg(mrb_state *mrb, mrb_value v)
{
  mrb_float f = mrb_float_p(v) ? mrb_float(v) : (mrb_float)mrb_integer(v);
  if (isinf(f) || isnan(f)) {
    mrb_raisef(mrb, E_FLOATDOMAIN_ERROR, "%v", v);
  }
  return f;
}
#endif

/* the number `arg` asks for, or nil when it asks for none */
static mrb_value
fmt_rand(mrb_state *mrb, mrb_value self, mrb_value arg)
{
  if (mrb_nil_p(arg)) return mrb_nil_value();

  if (mrb_integer_p(arg)) {
    mrb_int max = mrb_integer(arg);
    if (max <= 0) return mrb_nil_value();
    return mrb_int_value(mrb, (mrb_int)fmt_limited(mrb, self, (mrb_uint)max));
  }

#ifndef MRB_NO_FLOAT
  if (mrb_float_p(arg)) {
    mrb_float max = fmt_float_arg(mrb, arg);
    if (max <= 0) return mrb_nil_value();
    return mrb_float_value(mrb, fmt_real(mrb, self) * max);
  }
#endif

  if (mrb_range_p(arg)) {
    struct RRange *r = mrb_range_ptr(mrb, arg);
    mrb_value beg = RANGE_BEG(r), end = RANGE_END(r);
    mrb_bool excl = RANGE_EXCL(r);

    if (mrb_integer_p(beg) && mrb_integer_p(end)) {
      mrb_int b = mrb_integer(beg), e = mrb_integer(end);
      /* compare before subtracting, as rand_range_int does */
      if (b > e || (excl && b == e)) return mrb_nil_value();
      mrb_uint span = (mrb_uint)e - (mrb_uint)b + (excl ? 0 : 1);
      return mrb_int_value(mrb, (mrb_int)((mrb_uint)b + fmt_limited(mrb, self, span)));
    }
#ifndef MRB_NO_FLOAT
    if ((mrb_float_p(beg) || mrb_integer_p(beg)) &&
        (mrb_float_p(end) || mrb_integer_p(end))) {
      mrb_float b = fmt_float_arg(mrb, beg), e = fmt_float_arg(mrb, end);
      if (b > e || (excl && b == e)) return mrb_nil_value();
      mrb_float f = fmt_real(mrb, self);
      mrb_float span = e - b;
      /* a span past the largest Float, as -1e308..1e308, weighs the ends */
      /* volatile, so that an x87 compares the value it returns rather than
         the wider one in its register */
      volatile mrb_float v = isinf(span) ? b * (1 - f) + e * f : b + f * span;
      /* rounding can land on an excluded end */
      if (excl && v >= e) v = fmt_nextafter(e, b);
      return mrb_float_value(mrb, v);
    }
#endif
    invalid_argument(mrb, arg);
  }

#ifdef MRB_USE_BIGINT
  if (mrb_bigint_p(arg)) {
    if (mrb_bint_sign(mrb, arg) <= 0) return mrb_nil_value();
    mrb_int size = mrb_bint_bytes_size(mrb, arg);
    /* the draws span [0, 2**(8*size)), and those from `limit` up are the
       biased tail; `limit` is at least `arg`, so a Bignum too */
    mrb_value span = mrb_bint_lshift(mrb, mrb_bint_new_int(mrb, 1), size * 8);
    mrb_value limit = mrb_bint_sub(mrb, span, mrb_bint_mod(mrb, span, arg));
    mrb_value bytes = mrb_str_new(mrb, NULL, size);
    uint8_t *p = (uint8_t*)RSTRING_PTR(bytes);
    int ai = mrb_gc_arena_save(mrb);
    mrb_value v;
    for (;;) {
      fmt_fill(mrb, self, p, size);
      v = mrb_bint_new_bytes(mrb, p, size);
      if (mrb_bint_cmp(mrb, limit, v) > 0) break;
      mrb_gc_arena_restore(mrb, ai);
    }
    return mrb_bint_mod(mrb, v, arg);
  }
#endif

  invalid_argument(mrb, arg);
}

/*
 * call-seq:
 *   formatter.random_number -> float
 *   formatter.random_number(max) -> number
 *   formatter.random_number(range) -> number
 *   formatter.rand(...) -> number
 *
 * Returns a random number, as Random#rand does, except that a limit that
 * gives no number (nil, zero or a negative one, or an empty range) gives a
 * Float in [0, 1) instead. A Random draws from its own state, Random from
 * the default generator, and any other object from its #bytes(n), which
 * must return a String of n bytes.
 *
 *   Random.random_number(10)  #=> 7
 *   Random.random_number(0)   #=> 0.5418346412525913
 */
static mrb_value
random_fmt_rand(mrb_state *mrb, mrb_value self)
{
  mrb_value arg = mrb_nil_value();
  mrb_get_args(mrb, "|o", &arg);

  mrb_value v = fmt_rand(mrb, self, arg);
  if (mrb_nil_p(v)) {
#ifndef MRB_NO_FLOAT
    v = mrb_float_value(mrb, fmt_real(mrb, self));
#else
    v = mrb_int_value(mrb, (mrb_int)fmt_limited(mrb, self, 100));
#endif
  }
  return v;
}

void
mrb_mruby_random_formatter_gem_init(mrb_state *mrb)
{
  struct RClass *random = mrb_class_get_id(mrb, MRB_SYM(Random));
  struct RClass *fmt = mrb_define_module_under_id(mrb, random, MRB_SYM(Formatter));

  mrb_define_method_id(mrb, fmt, MRB_SYM(random_number), random_fmt_rand, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, fmt, MRB_SYM(rand), random_fmt_rand, MRB_ARGS_OPT(1));
  /* Random includes and extends it; its own rand comes first */
  mrb_include_module(mrb, random, fmt);
  mrb_include_module(mrb, mrb_singleton_class_ptr(mrb, mrb_obj_value(random)), fmt);
}

void
mrb_mruby_random_formatter_gem_final(mrb_state *mrb)
{
}
