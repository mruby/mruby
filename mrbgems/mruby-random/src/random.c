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

#include <time.h>

static char const RAND_STATE_KEY[] = "$mrb_i_rand_state";

static const struct mrb_data_type rand_state_type = {
  RAND_STATE_KEY, mrb_free,
};

typedef struct rand_state {
  uint32_t seed[4];
} rand_state;

static void
rand_init(rand_state *t)
{
  t->seed[0] = 123456789;
  t->seed[1] = 362436069;
  t->seed[2] = 521288629;
  t->seed[3] = 88675123;
}

static uint32_t
rand_seed(rand_state *t, uint32_t seed)
{
  uint32_t old_seed = t->seed[3];
  rand_init(t);
  t->seed[3] = seed;
  return old_seed;
}

static uint32_t
rand_uint32(rand_state *state)
{
  uint32_t *seed = state->seed;
  uint32_t x = seed[0];
  uint32_t y = seed[1];
  uint32_t z = seed[2];
  uint32_t w = seed[3]; 
  uint32_t t;

  t = x ^ (x << 11);
  x = y; y = z; z = w;
  w = (w ^ (w >> 19)) ^ (t ^ (t >> 8));
  seed[0] = x;
  seed[1] = y;
  seed[2] = z;
  seed[3] = w;

  return w;
}

#ifndef MRB_WITHOUT_FLOAT
static double
rand_real(rand_state *t)
{
  uint32_t x = rand_uint32(t);
  return x*(1.0/4294967295.0);
}
#endif

static mrb_value
random_rand(mrb_state *mrb, rand_state *t, mrb_value max)
{
  mrb_value value;

  if (mrb_fixnum(max) == 0) {
#ifndef MRB_WITHOUT_FLOAT
    value = mrb_float_value(mrb, rand_real(t));
#else
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Float not supported");
#endif
  }
  else {
    value = mrb_fixnum_value(rand_uint32(t) % mrb_fixnum(max));
  }

  return value;
}

static mrb_value
get_opt(mrb_state* mrb)
{
  mrb_value arg;

  arg = mrb_nil_value();
  mrb_get_args(mrb, "|o", &arg);

  if (!mrb_nil_p(arg)) {
    mrb_int i;

    arg = mrb_to_int(mrb, arg);
    i = mrb_fixnum(arg);
    if (i < 0) {
      arg = mrb_fixnum_value(0 - i);
    }
  }
  return arg;
}

static mrb_value
random_default(mrb_state *mrb) {
  return mrb_const_get(mrb,
             mrb_obj_value(mrb_class_get(mrb, "Random")),
             mrb_intern_lit(mrb, "DEFAULT"));
}

static rand_state *
random_state(mrb_state *mrb)
{
  mrb_value random_val = random_default(mrb);
  return DATA_GET_PTR(mrb, random_val, &rand_state_type, rand_state);
}

static mrb_value
random_m_init(mrb_state *mrb, mrb_value self)
{
  mrb_value seed;
  rand_state *t;

  seed = get_opt(mrb);
  /* avoid memory leaks */
  t = (rand_state*)DATA_PTR(self);
  if (t == NULL) {
    t = (rand_state *)mrb_malloc(mrb, sizeof(rand_state));
    mrb_data_init(self, t, &rand_state_type);
  }
  if (mrb_nil_p(seed)) {
    rand_init(t);
  }
  else {
    rand_seed(t, (uint32_t)mrb_fixnum(seed));
  }

  return self;
}

static mrb_value
random_m_rand(mrb_state *mrb, mrb_value self)
{
  mrb_value max;
  rand_state *t = DATA_GET_PTR(mrb, self, &rand_state_type, rand_state);

  max = get_opt(mrb);
  return random_rand(mrb, t, max);
}

static mrb_value
random_m_srand(mrb_state *mrb, mrb_value self)
{
  uint32_t seed;
  uint32_t old_seed;
  mrb_value sv;
  rand_state *t = DATA_GET_PTR(mrb, self, &rand_state_type, rand_state);

  sv = get_opt(mrb);
  if (mrb_nil_p(sv)) {
    seed = (uint32_t)time(NULL) + rand_uint32(t);
  }
  else {
    seed = (uint32_t)mrb_fixnum(sv);
  }
  old_seed = rand_seed(t, seed);

  return mrb_fixnum_value((mrb_int)old_seed);
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
  mrb_int i;
  mrb_value max;
  rand_state *random = NULL;

  if (RARRAY_LEN(ary) > 1) {
    mrb_get_args(mrb, "|d", &random, &rand_state_type);

    if (random == NULL) {
      random = random_state(mrb);
    }
    mrb_ary_modify(mrb, mrb_ary_ptr(ary));
    max = mrb_fixnum_value(RARRAY_LEN(ary));
    for (i = RARRAY_LEN(ary) - 1; i > 0; i--)  {
      mrb_int j;
      mrb_value *ptr = RARRAY_PTR(ary);
      mrb_value tmp;

      j = mrb_fixnum(random_rand(mrb, random, max));

      tmp = ptr[i];
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
  mrb_value new_ary = mrb_ary_new_from_values(mrb, RARRAY_LEN(ary), RARRAY_PTR(ary));
  mrb_ary_shuffle_bang(mrb, new_ary);

  return new_ary;
}

/*
 *  call-seq:
 *     ary.sample      ->   obj
 *     ary.sample(n)   ->   new_ary
 *
 *  Choose a random element or +n+ random elements from the array.
 *
 *  The elements are chosen by using random and unique indices into the array
 *  in order to ensure that an element doesn't repeat itself unless the array
 *  already contained duplicate elements.
 *
 *  If the array is empty the first form returns +nil+ and the second form
 *  returns an empty array.
 */

static mrb_value
mrb_ary_sample(mrb_state *mrb, mrb_value ary)
{
  mrb_int n = 0;
  mrb_bool given;
  rand_state *random = NULL;
  mrb_int len;

  mrb_get_args(mrb, "|i?d", &n, &given, &random, &rand_state_type);
  if (random == NULL) {
    random = random_state(mrb);
  }
  len = RARRAY_LEN(ary);
  if (!given) {                 /* pick one element */
    switch (len) {
    case 0:
      return mrb_nil_value();
    case 1:
      return RARRAY_PTR(ary)[0];
    default:
      return RARRAY_PTR(ary)[rand_uint32(random) % len];
    }
  }
  else {
    mrb_value result;
    mrb_int i, j;

    if (n < 0) mrb_raise(mrb, E_ARGUMENT_ERROR, "negative sample number");
    if (n > len) n = len;
    result = mrb_ary_new_capa(mrb, n);
    for (i=0; i<n; i++) {
      mrb_int r;

      for (;;) {
      retry:
        r = (mrb_int)rand_uint32(random) % len;

        for (j=0; j<i; j++) {
          if (mrb_fixnum(RARRAY_PTR(result)[j]) == r) {
            goto retry;         /* retry if duplicate */
          }
        }
        break;
      }
      mrb_ary_push(mrb, result, mrb_fixnum_value(r));
    }
    for (i=0; i<n; i++) {
      mrb_ary_set(mrb, result, i, RARRAY_PTR(ary)[mrb_fixnum(RARRAY_PTR(result)[i])]);
    }
    return result;
  }
}

static mrb_value
random_f_rand(mrb_state *mrb, mrb_value self)
{
  rand_state *t = random_state(mrb);
  return random_rand(mrb, t, get_opt(mrb));
}

static mrb_value
random_f_srand(mrb_state *mrb, mrb_value self)
{
  mrb_value random = random_default(mrb);
  return random_m_srand(mrb, random);
}


void mrb_mruby_random_gem_init(mrb_state *mrb)
{
  struct RClass *random;
  struct RClass *array = mrb->array_class;

  mrb_define_method(mrb, mrb->kernel_module, "rand", random_f_rand, MRB_ARGS_OPT(1));
  mrb_define_method(mrb, mrb->kernel_module, "srand", random_f_srand, MRB_ARGS_OPT(1));

  random = mrb_define_class(mrb, "Random", mrb->object_class);
  MRB_SET_INSTANCE_TT(random, MRB_TT_DATA);
  mrb_define_class_method(mrb, random, "rand", random_f_rand, MRB_ARGS_OPT(1));
  mrb_define_class_method(mrb, random, "srand", random_f_srand, MRB_ARGS_OPT(1));

  mrb_define_method(mrb, random, "initialize", random_m_init, MRB_ARGS_OPT(1));
  mrb_define_method(mrb, random, "rand", random_m_rand, MRB_ARGS_OPT(1));
  mrb_define_method(mrb, random, "srand", random_m_srand, MRB_ARGS_OPT(1));

  mrb_define_method(mrb, array, "shuffle", mrb_ary_shuffle, MRB_ARGS_OPT(1));
  mrb_define_method(mrb, array, "shuffle!", mrb_ary_shuffle_bang, MRB_ARGS_OPT(1));
  mrb_define_method(mrb, array, "sample", mrb_ary_sample, MRB_ARGS_OPT(2));

  mrb_const_set(mrb, mrb_obj_value(random), mrb_intern_lit(mrb, "DEFAULT"),
          mrb_obj_new(mrb, random, 0, NULL));
}

void mrb_mruby_random_gem_final(mrb_state *mrb)
{
}
