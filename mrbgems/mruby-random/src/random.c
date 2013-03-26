/*
** random.c - Random module
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#include "mruby/variable.h"
#include "mt19937ar.h"

#include <time.h>

#define GLOBAL_RAND_SEED_KEY    "$mrb_g_rand_seed"
#define INSTANCE_RAND_SEED_KEY  "$mrb_i_rand_seed"
 
static void mt_srand(unsigned long seed)
{
  init_genrand(seed);
}  

static unsigned long mt_rand()
{
  return genrand_int32();
}  

static double mt_rand_real()
{
  return genrand_real1();
}  

static mrb_value mrb_random_mt_srand(mrb_state *mrb, mrb_value seed)
{ 
  if (mrb_nil_p(seed)) {
    seed = mrb_fixnum_value(time(NULL) + mt_rand());
    if (mrb_fixnum(seed) < 0) {
      seed = mrb_fixnum_value( 0 - mrb_fixnum(seed));
    }
  }

  mt_srand((unsigned) mrb_fixnum(seed));

  return seed;
}

static mrb_value mrb_random_mt_rand(mrb_state *mrb, mrb_value max)
{ 
  mrb_value value;

  if (mrb_fixnum(max) == 0) {
    value = mrb_float_value(mt_rand_real());
  } else {
    value = mrb_fixnum_value(mt_rand() % mrb_fixnum(max));
  }

  return value;
}

static mrb_value get_opt(mrb_state* mrb)
{
  mrb_value arg;

  arg = mrb_fixnum_value(0);
  mrb_get_args(mrb, "|o", &arg);

  if (!mrb_nil_p(arg)) {
    if (!mrb_fixnum_p(arg)) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid argument type");
    }
    arg = mrb_check_convert_type(mrb, arg, MRB_TT_FIXNUM, "Fixnum", "to_int");
    if (mrb_fixnum(arg) < 0) {
      arg = mrb_fixnum_value(0 - mrb_fixnum(arg));
    }
  }
  return arg;
}

static mrb_value mrb_random_g_rand(mrb_state *mrb, mrb_value self)
{
  mrb_value max;
  mrb_value seed;

  max = get_opt(mrb);
  seed = mrb_gv_get(mrb, mrb_intern(mrb, GLOBAL_RAND_SEED_KEY));
  if (mrb_nil_p(seed)) {
    mrb_random_mt_srand(mrb, mrb_nil_value());
  }
  return mrb_random_mt_rand(mrb, max);
}

static mrb_value mrb_random_g_srand(mrb_state *mrb, mrb_value self)
{
  mrb_value seed;
  mrb_value old_seed;

  seed = get_opt(mrb);
  seed = mrb_random_mt_srand(mrb, seed);
  old_seed = mrb_gv_get(mrb, mrb_intern(mrb, GLOBAL_RAND_SEED_KEY));
  mrb_gv_set(mrb, mrb_intern(mrb, GLOBAL_RAND_SEED_KEY), seed);
  return old_seed;
}

static mrb_value mrb_random_init(mrb_state *mrb, mrb_value self)
{
  mrb_value seed;

  seed = get_opt(mrb);
  seed = mrb_random_mt_srand(mrb, seed);
  mrb_iv_set(mrb, self, mrb_intern(mrb, INSTANCE_RAND_SEED_KEY), seed);
  return self;
}

static mrb_value mrb_random_rand(mrb_state *mrb, mrb_value self)
{
  mrb_value max;

  max = get_opt(mrb);
  mrb_value seed = mrb_iv_get(mrb, self, mrb_intern(mrb, INSTANCE_RAND_SEED_KEY));
  if (mrb_nil_p(seed)) {
    mrb_random_mt_srand(mrb, mrb_nil_value());
  }
  return mrb_random_mt_rand(mrb, max);
}

static mrb_value mrb_random_srand(mrb_state *mrb, mrb_value self)
{
  mrb_value seed;

  seed = get_opt(mrb);
  seed = mrb_random_mt_srand(mrb, seed);
  mrb_value old_seed = mrb_iv_get(mrb, self, mrb_intern(mrb, INSTANCE_RAND_SEED_KEY));
  mrb_iv_set(mrb, self, mrb_intern(mrb, INSTANCE_RAND_SEED_KEY), seed);
  return old_seed;
}

void mrb_mruby_random_gem_init(mrb_state *mrb)
{
  struct RClass *random;

  mrb_define_method(mrb, mrb->kernel_module, "rand", mrb_random_g_rand, ARGS_OPT(1));
  mrb_define_method(mrb, mrb->kernel_module, "srand", mrb_random_g_srand, ARGS_OPT(1));

  random = mrb_define_class(mrb, "Random", mrb->object_class);
  mrb_define_class_method(mrb, random, "rand", mrb_random_g_rand, ARGS_OPT(1));
  mrb_define_class_method(mrb, random, "srand", mrb_random_g_srand, ARGS_OPT(1));

  mrb_define_method(mrb, random, "initialize", mrb_random_init, ARGS_OPT(1));
  mrb_define_method(mrb, random, "rand", mrb_random_rand, ARGS_OPT(1));
  mrb_define_method(mrb, random, "srand", mrb_random_srand, ARGS_OPT(1));
}

void mrb_mruby_random_gem_final(mrb_state *mrb)
{
}

