/*
** random.c - Random module
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/variable.h>
#include <mruby/class.h>
#include <mruby/internal.h>
#include <mruby/data.h>
#include <mruby/array.h>
#include <mruby/istruct.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/internal.h>

#include <time.h>
#include <string.h>
#ifndef MRB_NO_FLOAT
#include <math.h>
#endif

/*  PCG Random Number Generation
    Based on the PCG family by Melissa O'Neill <oneill@pcg-random.org>

    This implements PCG-XSH-RR with 64-bit state and 32-bit output.
    On 32-bit platforms, uses an optimized 32-bit multiplier for better
    performance. On 64-bit platforms, uses the standard 64-bit multiplier
    for maximum statistical quality.

    See <https://www.pcg-random.org/> for details. */

/* Platform-adaptive multiplier selection:
   - 32-bit platforms: 0xf13283ad requires only 2 multiplies instead of 3
   - 64-bit platforms: standard multiplier for best statistical quality */
#ifdef MRB_32BIT
# define PCG_MULTIPLIER 0xf13283adULL
#else
# define PCG_MULTIPLIER 6364136223846793005ULL
#endif
#define PCG_INCREMENT 1442695040888963407ULL

typedef struct rand_state {
#ifdef MRB_32BIT
  /* On 32-bit platforms, split state to avoid alignment padding */
  uint32_t state_lo;
  uint32_t state_hi;
#else
  uint64_t state;
#endif
  uint32_t seed_value;  /* Track last seed for srand compatibility */
} rand_state;

/* Helper macros for 64-bit state access */
#ifdef MRB_32BIT
# define GET_STATE(t) (((uint64_t)(t)->state_hi << 32) | (t)->state_lo)
# define SET_STATE(t, val) do { \
    uint64_t v_ = (val); \
    (t)->state_lo = (uint32_t)v_; \
    (t)->state_hi = (uint32_t)(v_ >> 32); \
  } while (0)
#else
# define GET_STATE(t) ((t)->state)
# define SET_STATE(t, val) ((t)->state = (val))
#endif

static void
rand_init(rand_state *t)
{
  SET_STATE(t, 0x853c49e6748fea9bULL);
  t->seed_value = 521288629;
}

static uint32_t rand_uint32(rand_state *state);

static uint32_t
rand_seed(rand_state *t, uint32_t seed)
{
  uint32_t old_seed = t->seed_value;

  /* PCG initialization: state=0, step, add seed, step, then mix */
  SET_STATE(t, 0);
  rand_uint32(t);
  SET_STATE(t, GET_STATE(t) + seed);
  for (int i = 0; i < 10; i++) {
    rand_uint32(t);
  }

  t->seed_value = seed;
  return old_seed;
}

static uint32_t
rand_uint32(rand_state *rng)
{
  /* PCG-XSH-RR: XorShift High (xorshift), then Random Rotate */
  uint64_t oldstate = GET_STATE(rng);

  /* LCG step: advance internal state */
  SET_STATE(rng, oldstate * PCG_MULTIPLIER + PCG_INCREMENT);

  /* Output function: xorshift, then rotate by top bits */
  uint32_t xorshifted = (uint32_t)(((oldstate >> 18u) ^ oldstate) >> 27u);
  uint32_t rot = (uint32_t)(oldstate >> 59u);

  /* Rotate right by rot bits (handles rot=0 case correctly) */
  return (xorshifted >> rot) | (xorshifted << ((32 - rot) & 31));
}

#ifndef MRB_NO_FLOAT
static double
rand_real(rand_state *t)
{
  uint32_t x = rand_uint32(t);
  return x*(1.0/4294967296.0);
}
#endif

static mrb_uint rand_u(rand_state *t, mrb_uint span);

static mrb_value
random_rand(mrb_state *mrb, rand_state *t, mrb_int max)
{
  if (max == 0) {
#ifndef MRB_NO_FLOAT
    return mrb_float_value(mrb, rand_real(t));
#else
    max = 100;
#endif
  }
  /* a negative limit reaches here only from Kernel#rand, which draws from
     its magnitude; the unsigned modulus keeps that defined on any mrb_int */
  uint64_t lim = max < 0 ? 0 - (uint64_t)max : (uint64_t)max;
  if (lim > UINT32_MAX) {
    /* a 32-bit draw cannot reach the upper part of a larger limit */
    return mrb_int_value(mrb, (mrb_int)rand_u(t, (mrb_uint)lim));
  }
  return mrb_int_value(mrb, (mrb_int)(rand_uint32(t) % lim));
}

static mrb_int
rand_i(rand_state *t, mrb_int max)
{
  /* return uniform integer in [0, max) without modulo bias */
  if (max <= 0) return 0;

#ifdef MRB_INT64
  /* For large ranges that exceed 32-bit, use 64-bit random */
  if (max > (mrb_int)UINT32_MAX) {
    uint64_t umax = (uint64_t)max;
    uint64_t threshold = (uint64_t)(-(int64_t)umax) % umax;
    uint64_t r;
    do {
      /* combine two 32-bit randoms into one 64-bit */
      r = ((uint64_t)rand_uint32(t) << 32) | rand_uint32(t);
    } while (r < threshold);
    return (mrb_int)(r % umax);
  }
#endif

  uint32_t threshold = (uint32_t)(-max) % (uint32_t)max; /* power-of-two fast path => 0 */
  uint32_t r;
  do {
    r = rand_uint32(t);
  } while (r < threshold);
  return (mrb_int)(r % (uint32_t)max);
}

/* Full-width unsigned random value in [0, 2**MRB_INT_BIT). */
static mrb_uint
rand_uint(rand_state *t)
{
#ifdef MRB_INT64
  return ((mrb_uint)rand_uint32(t) << 32) | rand_uint32(t);
#else
  return (mrb_uint)rand_uint32(t);
#endif
}

/* Uniform unsigned value in [0, span) without modulo bias.
   span == 0 selects the entire domain [0, 2**MRB_INT_BIT). */
static mrb_uint
rand_u(rand_state *t, mrb_uint span)
{
  if (span == 0) return rand_uint(t);
  mrb_uint threshold = (mrb_uint)(-span) % span; /* == 2**MRB_INT_BIT % span */
  mrb_uint r;
  do {
    r = rand_uint(t);
  } while (r < threshold);
  return r % span;
}

static mrb_value
rand_range_int(mrb_state *mrb, rand_state *t, mrb_int begin,
               mrb_int end, mrb_bool excl) {
  /* Reversed or empty range -> nil (as CRuby does). Compare before
     subtracting so extreme bounds cannot overflow mrb_int. */
  if (begin > end || (excl && begin == end))
    return mrb_nil_value();

  /* Candidate count in unsigned arithmetic to avoid signed overflow.
     An inclusive full-width range wraps to 0, which rand_u reads as
     "the entire domain". */
  mrb_uint span = (mrb_uint)end - (mrb_uint)begin + (excl ? 0 : 1);
  mrb_uint r = rand_u(t, span);
  return mrb_int_value(mrb, (mrb_int)((mrb_uint)begin + r));
}

#ifndef MRB_NO_FLOAT
static mrb_value
rand_range_float(mrb_state *mrb, rand_state *t,
                 mrb_float begin, mrb_float end,
                 mrb_bool excl) {
  mrb_float span = end - begin;
  if (span <= 0.0)
    return mrb_nil_value();

  return mrb_float_value(mrb, rand_real(t) * span + begin);
}
#endif

static mrb_noreturn void
range_error(mrb_state *mrb, mrb_value v)
{
  mrb_raisef(mrb, E_TYPE_ERROR, "no implicit conversion of %Y into Integer", v);
}

static mrb_value
random_range(mrb_state *mrb, rand_state *t, mrb_value rv)
{
  struct RRange *r = mrb_range_ptr(mrb, rv);
  if (mrb_integer_p(RANGE_BEG(r)) && mrb_integer_p(RANGE_END(r))) {
    return rand_range_int(mrb, t, mrb_integer(RANGE_BEG(r)),
                          mrb_integer(RANGE_END(r)), RANGE_EXCL(r));
  }

#ifdef MRB_NO_FLOAT
  /* Without float support only integer ranges are meaningful. */
  range_error(mrb, RANGE_BEG(r));
  return mrb_nil_value();
#else
#define cast_to_float(v)                                                       \
  (mrb_float_p(v)     ? mrb_float(v)                                           \
   : mrb_integer_p(v) ? (mrb_float)mrb_integer(v)                              \
                      : (range_error(mrb, v), 0.0))

  return rand_range_float(mrb, t, cast_to_float(RANGE_BEG(r)),
                          cast_to_float(RANGE_END(r)), RANGE_EXCL(r));
#undef cast_to_float
#endif
}

static mrb_noreturn void
invalid_argument(mrb_state *mrb, mrb_value v)
{
  mrb_raisef(mrb, E_ARGUMENT_ERROR, "invalid argument - %v", v);
}

/*
 * Random.rand and Random#rand reject a negative limit, as CRuby does;
 * Kernel#rand (kernel == TRUE) uses its absolute value instead.
 */
static mrb_value
random_rand_impl(mrb_state *mrb, rand_state *t, mrb_bool kernel)
{
  mrb_value arg;
  if (mrb_get_args(mrb, "|o", &arg) == 0) {
    return random_rand(mrb, t, 0);
  }
  mrb_value orig = arg;

#ifndef MRB_NO_FLOAT
  if (mrb_float_p(arg)) {
    /* check the sign first: truncation would turn -0.5 into 0 */
    if (!kernel && mrb_float(arg) < 0) {
      invalid_argument(mrb, orig);
    }
    /* The limit is truncated through the checked conversion: a Float that
       stands for no Integer, Infinity and NaN among them, is out of range
       rather than whatever the cast makes of it, and one wider than mrb_int
       reaches the big integer limit below the way 10**30 does. */
    arg = mrb_ensure_integer_type(mrb, arg);
  }
#endif

  if (mrb_integer_p(arg)) {
    mrb_int max = mrb_integer(arg);
    if (max < 0 && !kernel) {
      invalid_argument(mrb, orig);
    }
    return random_rand(mrb, t, max);
  }

  if (mrb_range_p(arg)) {
    return random_range(mrb, t, arg);
  }

#ifdef MRB_USE_BIGINT
  if (mrb_bigint_p(arg)) {
    if (mrb_bint_sign(mrb, arg) < 0) {
      if (!kernel) {
        invalid_argument(mrb, orig);
      }
      arg = mrb_bint_abs(mrb, arg);
    }
    mrb_int size = mrb_bint_bytes_size(mrb, arg);
    mrb_value bytes = mrb_str_new(mrb, NULL, size);
    uint8_t *p = (uint8_t*)RSTRING_PTR(bytes);
    for (mrb_int i = 0; i < size; i++) {
      p[i] = (uint8_t)rand_uint32(t);
    }
    mrb_value rand_bint = mrb_bint_new_bytes(mrb, p, size);
    return mrb_bint_mod(mrb, rand_bint, arg);
  }
#endif

  range_error(mrb, arg);
}

#define ID_RANDOM MRB_SYM(mruby_Random)

static mrb_value
random_default(mrb_state *mrb)
{
  struct RClass *c = mrb_class_get_id(mrb, ID_RANDOM);
  mrb_value d = mrb_iv_get(mrb, mrb_obj_value(c), ID_RANDOM);
  if (!mrb_obj_is_kind_of(mrb, d, c)) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "[BUG] default Random replaced");
  }
  return d;
}

#define random_ptr(v) (rand_state*)mrb_istruct_ptr(v)
#define random_default_state(mrb) random_ptr(random_default(mrb))

/*
 * call-seq:
 *   Random.new(seed = nil) -> random
 *
 * Creates a new random number generator. If seed is omitted or nil,
 * the generator is initialized with a default seed. Otherwise,
 * the generator is initialized with the given seed.
 *
 *   Random.new        #=> #<Random:0x...>
 *   Random.new(1234)  #=> #<Random:0x...>
 */
static mrb_value
random_m_init(mrb_state *mrb, mrb_value self)
{
  mrb_int seed;
  rand_state *t = random_ptr(self);

  if (mrb_get_args(mrb, "|i", &seed) == 0) {
    rand_init(t);
  }
  else {
    rand_seed(t, (uint32_t)seed);
  }

  return self;
}

/*
 * call-seq:
 *   random.rand -> float
 *   random.rand(max) -> number
 *   random.rand(range) -> number
 *
 * Returns a random number. When called without arguments, returns a
 * random float between 0.0 and 1.0. When called with a positive integer,
 * returns a random integer between 0 and max-1. When called with a range,
 * returns a random number within that range. A negative max raises
 * ArgumentError.
 *
 *   prng = Random.new
 *   prng.rand         #=> 0.2725926052826416
 *   prng.rand(10)     #=> 7
 *   prng.rand(1..6)   #=> 4
 */
static mrb_value
random_m_rand(mrb_state *mrb, mrb_value self)
{
  rand_state *t = random_ptr(self);
  return random_rand_impl(mrb, t, FALSE);
}

/*
 * call-seq:
 *   random.srand(seed = nil) -> old_seed
 *
 * Seeds the random number generator with the given seed. If seed is
 * omitted or nil, uses a combination of current time and internal state.
 * Returns the previous seed value.
 *
 *   prng = Random.new
 *   prng.srand(1234)  #=> (previous seed)
 *   prng.srand        #=> 1234
 */
static mrb_value
random_m_srand(mrb_state *mrb, mrb_value self)
{
  uint32_t seed;
  mrb_int i;
  rand_state *t = random_ptr(self);

  if (mrb_get_args(mrb, "|i", &i) == 0) {
    seed = (uint32_t)time(NULL) ^ rand_uint32(t) ^ (uint32_t)(uintptr_t)t;
  }
  else {
    seed = (uint32_t)i;
  }

  uint32_t old_seed = rand_seed(t, seed);
  return mrb_int_value(mrb, (mrb_int)old_seed);
}

/*
 * call-seq:
 *   random.bytes(size) -> string
 *
 * Returns a string of random bytes of the specified size.
 *
 *   prng = Random.new
 *   prng.bytes(4)     #=> "\x8F\x12\xA3\x7C"
 *   prng.bytes(10).length  #=> 10
 */
static mrb_value
random_m_bytes(mrb_state *mrb, mrb_value self)
{
  rand_state *t = random_ptr(self);
  mrb_int i = mrb_as_int(mrb, mrb_get_arg1(mrb));
  if (i < 0) mrb_raise(mrb, E_ARGUMENT_ERROR, "negative string size");
  mrb_value bytes = mrb_str_new(mrb, NULL, i);
  uint8_t *p = (uint8_t*)RSTRING_PTR(bytes);

  /* write 4 bytes per PRNG call */
  while (i >= 4) {
    uint32_t x = rand_uint32(t);
    p[0] = (uint8_t)(x);
    p[1] = (uint8_t)(x >> 8);
    p[2] = (uint8_t)(x >> 16);
    p[3] = (uint8_t)(x >> 24);
    p += 4;
    i -= 4;
  }
  if (i > 0) {
    uint32_t x = rand_uint32(t);
    while (i-- > 0) {
      *p++ = (uint8_t)x;
      x >>= 8;
    }
  }

  return bytes;
}

static rand_state*
check_random_arg(mrb_state *mrb, mrb_value r)
{
  struct RClass *c = mrb_class_get_id(mrb, ID_RANDOM);
  rand_state *random;

  if (mrb_undef_p(r)) {
    random = random_default_state(mrb);
  }
  else if (mrb_istruct_p(r) && mrb_obj_is_kind_of(mrb, r, c)){
    random = (rand_state*)mrb_istruct_ptr(r);
  }
  else {
    mrb_raise(mrb, E_TYPE_ERROR, "Random object required");
  }
  return random;
}
/*
 *  call-seq:
 *     ary.shuffle!   ->   ary
 *
 *  Shuffles elements in self in place.
 */

static mrb_value
mrb_ary_shuffle_bang(mrb_state *mrb, mrb_value ary)
{
  if (RARRAY_LEN(ary) > 1) {
    mrb_sym kname = MRB_SYM(random);
    mrb_value r;
    const mrb_kwargs kw = {1, 0, &kname, &r, NULL};

    mrb_get_args(mrb, ":", &kw);
    rand_state *random = check_random_arg(mrb, r);
    mrb_ary_modify(mrb, mrb_ary_ptr(ary));
    mrb_int len = RARRAY_LEN(ary);
    mrb_value *ptr = RARRAY_PTR(ary);
    for (mrb_int i = len - 1; i > 0; i--)  {
      mrb_int j = rand_i(random, i + 1);
      mrb_value tmp = ptr[i];
      ptr[i] = ptr[j];
      ptr[j] = tmp;
    }
  }

  return ary;
}

/*
 *  call-seq:
 *     ary.shuffle   ->   new_ary
 *
 *  Returns a new array with elements of self shuffled.
 */

static mrb_value
mrb_ary_shuffle(mrb_state *mrb, mrb_value ary)
{
  mrb_value new_ary = mrb_ary_dup(mrb, ary);
  mrb_ary_shuffle_bang(mrb, new_ary);

  return new_ary;
}

/*
 *  call-seq:
 *     ary.sample      ->   obj
 *     ary.sample(n)   ->   new_ary
 *
 *  Choose a random element or `n` random elements from the array.
 *
 *  The elements are chosen by using random and unique indices into the array
 *  in order to ensure that an element doesn't repeat itself unless the array
 *  already contained duplicate elements.
 *
 *  If the array is empty the first form returns `nil` and the second form
 *  returns an empty array.
 */

static mrb_value
mrb_ary_sample(mrb_state *mrb, mrb_value ary)
{
  mrb_int n = 0;
  mrb_bool given;
  mrb_sym kname = MRB_SYM(random);
  mrb_value r;
  const mrb_kwargs kw = {1, 0, &kname, &r, NULL};

  mrb_get_args(mrb, "|i?:", &n, &given, &kw);
  rand_state *random = check_random_arg(mrb, r);
  mrb_int len = RARRAY_LEN(ary);
  if (!given) {                 /* pick one element */
    switch (len) {
    case 0:
      return mrb_nil_value();
    case 1:
      return RARRAY_PTR(ary)[0];
    default:
      return RARRAY_PTR(ary)[rand_i(random, len)];
    }
  }
  else {
    if (n < 0) mrb_raise(mrb, E_ARGUMENT_ERROR, "negative sample number");
    if (n > len) n = len;
    /* collect unique indices without allocating Ruby Integers */
    mrb_int *idx = (mrb_int*)mrb_alloca(mrb, sizeof(mrb_int) * (n > 0 ? n : 1));
    for (mrb_int i = 0; i < n; i++) {
      mrb_int v;
      for (;;) {
      retry:
        v = rand_i(random, len);
        for (mrb_int j = 0; j < i; j++) {
          if (idx[j] == v) goto retry; /* retry if duplicate */
        }
        break;
      }
      idx[i] = v;
    }
    mrb_value result = mrb_ary_new_capa(mrb, n);
    for (mrb_int i = 0; i < n; i++) {
      mrb_ary_push(mrb, result, RARRAY_PTR(ary)[idx[i]]);
    }

    return result;
  }
}

/*
 * call-seq:
 *   Random.rand -> float
 *   Random.rand(max) -> number
 *   Random.rand(range) -> number
 *   rand -> float
 *   rand(max) -> number
 *   rand(range) -> number
 *
 * Returns a random number using the default random number generator.
 * Equivalent to Random.new.rand. When called without arguments, returns
 * a random float between 0.0 and 1.0. When called with a positive integer,
 * returns a random integer between 0 and max-1. When called with a range,
 * returns a random number within that range. A negative max raises
 * ArgumentError for Random.rand; Kernel#rand uses its absolute value.
 *
 *   Random.rand       #=> 0.8444218515250481
 *   Random.rand(10)   #=> 5
 *   rand(1..6)        #=> 3
 */
static mrb_value
random_f_rand(mrb_state *mrb, mrb_value self)
{
  rand_state *t = random_default_state(mrb);
  return random_rand_impl(mrb, t, FALSE);
}

static mrb_value
random_k_rand(mrb_state *mrb, mrb_value self)
{
  rand_state *t = random_default_state(mrb);
  return random_rand_impl(mrb, t, TRUE);
}

/*
 * call-seq:
 *   Random.srand(seed = nil) -> old_seed
 *   srand(seed = nil) -> old_seed
 *
 * Seeds the default random number generator with the given seed.
 * If seed is omitted or nil, uses current time and internal state.
 * Returns the previous seed value.
 *
 *   Random.srand(1234)  #=> (previous seed)
 *   srand               #=> 1234
 */
static mrb_value
random_f_srand(mrb_state *mrb, mrb_value self)
{
  mrb_value random = random_default(mrb);
  return random_m_srand(mrb, random);
}

/*
 * call-seq:
 *   Random.bytes(size) -> string
 *
 * Returns a string of random bytes of the specified size using
 * the default random number generator.
 *
 *   Random.bytes(4)     #=> "\x8F\x12\xA3\x7C"
 *   Random.bytes(10).length  #=> 10
 */
static mrb_value
random_f_bytes(mrb_state *mrb, mrb_value self)
{
  mrb_value random = random_default(mrb);
  return random_m_bytes(mrb, random);
}


/*
 * Random::Formatter
 *
 * random_number and rand draw from the receiver: a Random from its own
 * state, Random itself from the default generator, and any other object
 * that includes the module from its #bytes, as CRuby's do.
 */
typedef struct fmt_src {
  rand_state *t;                /* the generator, or NULL to read #bytes */
  mrb_value obj;
} fmt_src;

static fmt_src
fmt_source(mrb_state *mrb, mrb_value self)
{
  struct RClass *c = mrb_class_get_id(mrb, ID_RANDOM);
  fmt_src s;

  s.obj = self;
  if (mrb_istruct_p(self) && mrb_obj_is_kind_of(mrb, self, c)) {
    s.t = random_ptr(self);
  }
  else if (mrb_class_p(self) && mrb_class_ptr(self) == c) {
    s.t = random_default_state(mrb);
  }
  else {
    s.t = NULL;
  }
  return s;
}

/* fills `len` random bytes at `p` */
static void
fmt_fill(mrb_state *mrb, fmt_src *s, uint8_t *p, mrb_int len)
{
  if (s->t) {
    while (len > 0) {
      uint32_t x = rand_uint32(s->t);
      for (int i = 0; i < 4 && len > 0; i++, len--) {
        *p++ = (uint8_t)x;
        x >>= 8;
      }
    }
    return;
  }

  int ai = mrb_gc_arena_save(mrb);
  mrb_value b = mrb_funcall_id(mrb, s->obj, MRB_SYM(bytes), 1, mrb_int_value(mrb, len));
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
fmt_draw(mrb_state *mrb, fmt_src *s, int len)
{
  uint8_t buf[8];
  uint64_t v = 0;

  fmt_fill(mrb, s, buf, len);
  while (len-- > 0) {
    v = (v << 8) | buf[len];
  }
  return v;
}

/* Uniform value in [0, span) without modulo bias, as rand_u gives; span == 0
   selects the entire domain of mrb_uint. A span that fits in 32 bits draws
   4 bytes a try, and a larger one 8. */
static mrb_uint
fmt_limited(mrb_state *mrb, fmt_src *s, mrb_uint span)
{
  if (span == 0) return (mrb_uint)fmt_draw(mrb, s, sizeof(mrb_uint));

  uint64_t n = span;
  int len = n - 1 > UINT32_MAX ? 8 : 4;
  uint64_t top = len == 4 ? (uint64_t)1 << 32 : 0;  /* 2**64 wraps to 0 */
  uint64_t tail = (top - n) % n;                   /* == 2**(8*len) % n */
  uint64_t r;
  /* draw again in the biased tail at the top */
  do {
    r = fmt_draw(mrb, s, len);
  } while (tail != 0 && r >= top - tail);
  return (mrb_uint)(r % n);
}

#ifndef MRB_NO_FLOAT
/* as many random bits as mrb_float holds, 53 or 24, as a Float in [0, 1);
   one bit more would round up to 1.0 */
static mrb_float
fmt_real(mrb_state *mrb, fmt_src *s)
{
#ifdef MRB_USE_FLOAT32
  return (mrb_float)(fmt_draw(mrb, s, 8) >> 40) * (1.0f/16777216.0f);
#else
  return (mrb_float)(fmt_draw(mrb, s, 8) >> 11) * (1.0/9007199254740992.0);
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
fmt_rand(mrb_state *mrb, fmt_src *s, mrb_value arg)
{
  if (mrb_nil_p(arg)) return mrb_nil_value();

  if (mrb_integer_p(arg)) {
    mrb_int max = mrb_integer(arg);
    if (max <= 0) return mrb_nil_value();
    return mrb_int_value(mrb, (mrb_int)fmt_limited(mrb, s, (mrb_uint)max));
  }

#ifndef MRB_NO_FLOAT
  if (mrb_float_p(arg)) {
    mrb_float max = fmt_float_arg(mrb, arg);
    if (max <= 0) return mrb_nil_value();
    return mrb_float_value(mrb, fmt_real(mrb, s) * max);
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
      return mrb_int_value(mrb, (mrb_int)((mrb_uint)b + fmt_limited(mrb, s, span)));
    }
#ifndef MRB_NO_FLOAT
    if ((mrb_float_p(beg) || mrb_integer_p(beg)) &&
        (mrb_float_p(end) || mrb_integer_p(end))) {
      mrb_float b = fmt_float_arg(mrb, beg), e = fmt_float_arg(mrb, end);
      if (b > e || (excl && b == e)) return mrb_nil_value();
      mrb_float f = fmt_real(mrb, s);
      mrb_float span = e - b;
      /* a span past the largest Float, as -1e308..1e308, weighs the ends */
      mrb_float v = isinf(span) ? b * (1 - f) + e * f : b + f * span;
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
      fmt_fill(mrb, s, p, size);
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

  fmt_src s = fmt_source(mrb, self);
  mrb_value v = fmt_rand(mrb, &s, arg);
  if (mrb_nil_p(v)) {
#ifndef MRB_NO_FLOAT
    v = mrb_float_value(mrb, fmt_real(mrb, &s));
#else
    v = mrb_int_value(mrb, (mrb_int)fmt_limited(mrb, &s, 100));
#endif
  }
  return v;
}

static const mrb_mt_entry random_rom_entries[] = {
  MRB_MT_ENTRY(random_m_init,  MRB_SYM(initialize), MRB_ARGS_OPT(1)),
  MRB_MT_ENTRY(random_m_rand,  MRB_SYM(rand), MRB_ARGS_OPT(1)),
  MRB_MT_ENTRY(random_m_srand, MRB_SYM(srand), MRB_ARGS_OPT(1)),
  MRB_MT_ENTRY(random_m_bytes, MRB_SYM(bytes), MRB_ARGS_REQ(1)),
};

void mrb_mruby_random_gem_init(mrb_state *mrb)
{
  struct RClass *array = mrb->array_class;

  mrb_static_assert(sizeof(rand_state) <= ISTRUCT_DATA_SIZE);

  struct RClass *random = mrb_define_class_id(mrb, MRB_SYM(Random), mrb->object_class);
  mrb_const_set(mrb, mrb_obj_value(mrb->object_class), ID_RANDOM, mrb_obj_value(random));
  MRB_SET_INSTANCE_TT(random, MRB_TT_ISTRUCT);
  mrb_define_class_method_id(mrb, random, MRB_SYM(rand), random_f_rand, MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, random, MRB_SYM(srand), random_f_srand, MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, random, MRB_SYM(bytes), random_f_bytes, MRB_ARGS_REQ(1));

  /* Random includes and extends it; its own rand comes first */
  struct RClass *fmt = mrb_define_module_under_id(mrb, random, MRB_SYM(Formatter));
  mrb_define_method_id(mrb, fmt, MRB_SYM(random_number), random_fmt_rand, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, fmt, MRB_SYM(rand), random_fmt_rand, MRB_ARGS_OPT(1));
  mrb_include_module(mrb, random, fmt);
  mrb_include_module(mrb, mrb_singleton_class_ptr(mrb, mrb_obj_value(random)), fmt);

  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(rand), random_k_rand, MRB_ARGS_OPT(1));
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(srand), random_f_srand, MRB_ARGS_OPT(1));
  MRB_MT_INIT_ROM(mrb, random, random_rom_entries);
  mrb_define_method_id(mrb, array, MRB_SYM(shuffle), mrb_ary_shuffle, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, array, MRB_SYM_B(shuffle), mrb_ary_shuffle_bang, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, array, MRB_SYM(sample), mrb_ary_sample, MRB_ARGS_OPT(2));

  mrb_value d = mrb_obj_new(mrb, random, 0, NULL);
  rand_state *t = random_ptr(d);
  mrb_iv_set(mrb, mrb_obj_value(random), ID_RANDOM, d);

  uint32_t seed = (uint32_t)time(NULL);
  rand_seed(t, seed ^ (uint32_t)(uintptr_t)t);
}

void mrb_mruby_random_gem_final(mrb_state *mrb)
{
}
