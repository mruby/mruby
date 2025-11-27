/*
** random.c - Random module
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/variable.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/array.h>
#include <mruby/istruct.h>
#include <mruby/presym.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/internal.h>

#include <time.h>

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
  return mrb_int_value(mrb, rand_uint32(t) % max);
}

static mrb_int
rand_i(rand_state *t, mrb_int max)
{
  /* return uniform integer in [0, max) without modulo bias */
  if (max <= 0) return 0;
  uint32_t threshold = (uint32_t)(-max) % (uint32_t)max; /* power-of-two fast path => 0 */
  uint32_t r;
  do {
    r = rand_uint32(t);
  } while (r < threshold);
  return (mrb_int)(r % (uint32_t)max);
}

static mrb_value
rand_range_int(mrb_state *mrb, rand_state *t, mrb_int begin,
               mrb_int end, mrb_bool excl) {
  mrb_int span = end - begin + (excl ? 0 : 1);
  if (span <= 0)
    return mrb_nil_value();

  return mrb_int_value(mrb, (rand_i(t, span)) + begin);
}

#ifndef MRB_NO_FLOAT
static mrb_value
rand_range_float(mrb_state *mrb, rand_state *t,
                 mrb_float begin, mrb_float end,
                 mrb_bool excl) {
  mrb_float span = end - begin + (excl ? 0.0 : 1.0);
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

#define cast_to_float(v)                                                       \
  (mrb_float_p(v)     ? mrb_float(v)                                           \
   : mrb_integer_p(v) ? (mrb_float)mrb_integer(v)                              \
                      : (range_error(mrb, v), 0.0))

  return rand_range_float(mrb, t, cast_to_float(RANGE_BEG(r)),
                          cast_to_float(RANGE_END(r)), RANGE_EXCL(r));
#undef cast_to_float
}

static mrb_value
random_rand_impl(mrb_state *mrb, rand_state *t, mrb_value self)
{
  mrb_value arg;
  if (mrb_get_args(mrb, "|o", &arg) == 0) {
    return random_rand(mrb, t, 0);
  }

  if (mrb_float_p(arg)) {
    return random_rand(mrb, t, (mrb_int)mrb_float(arg));
  }

  if (mrb_integer_p(arg)) {
    return random_rand(mrb, t, mrb_integer(arg));
  }

  if (mrb_range_p(arg)) {
    return random_range(mrb, t, arg);
  }

#ifdef MRB_USE_BIGINT
  if (mrb_bigint_p(arg)) {
    if (mrb_bint_sign(mrb, arg) < 0) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "negative value as random limit");
    }
    mrb_int size = mrb_bint_size(mrb, arg);
    mrb_value bytes = mrb_str_new(mrb, NULL, size);
    uint8_t *p = (uint8_t*)RSTRING_PTR(bytes);
    for (mrb_int i = 0; i < size; i++) {
      p[i] = (uint8_t)rand_uint32(t);
    }
    mrb_value rand_bint = mrb_bint_from_bytes(mrb, p, size);
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
 * returns a random number within that range.
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
  return random_rand_impl(mrb, t, self);
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
 * returns a random number within that range.
 *
 *   Random.rand       #=> 0.8444218515250481
 *   Random.rand(10)   #=> 5
 *   rand(1..6)        #=> 3
 */
static mrb_value
random_f_rand(mrb_state *mrb, mrb_value self)
{
  rand_state *t = random_default_state(mrb);
  return random_rand_impl(mrb, t, self);
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


void mrb_mruby_random_gem_init(mrb_state *mrb)
{
  struct RClass *array = mrb->array_class;

  mrb_static_assert(sizeof(rand_state) <= ISTRUCT_DATA_SIZE);

  mrb_define_private_method_id(mrb, mrb->kernel_module, MRB_SYM(rand), random_f_rand, MRB_ARGS_OPT(1));
  mrb_define_private_method_id(mrb, mrb->kernel_module, MRB_SYM(srand), random_f_srand, MRB_ARGS_OPT(1));

  struct RClass *random = mrb_define_class_id(mrb, MRB_SYM(Random), mrb->object_class);
  mrb_const_set(mrb, mrb_obj_value(mrb->object_class), ID_RANDOM, mrb_obj_value(random)); // for class check
  MRB_SET_INSTANCE_TT(random, MRB_TT_ISTRUCT);
  mrb_define_class_method_id(mrb, random, MRB_SYM(rand), random_f_rand, MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, random, MRB_SYM(srand), random_f_srand, MRB_ARGS_OPT(1));
  mrb_define_class_method_id(mrb, random, MRB_SYM(bytes), random_f_bytes, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, random, MRB_SYM(initialize), random_m_init, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, random, MRB_SYM(rand), random_m_rand, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, random, MRB_SYM(srand), random_m_srand, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, random, MRB_SYM(bytes), random_m_bytes, MRB_ARGS_REQ(1));

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
