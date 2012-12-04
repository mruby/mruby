/*
** random.c - random
**
** See Copyright Notice in mruby.h
*/

#include "mruby.h"
#ifdef ENABLE_RANDOM

#include "mruby/variable.h"

#include <stdio.h>
#include <time.h>

#define RAND_SEED_KEY "$mrb_ext_rand_seed"

mrb_value
mrb_random_srand(mrb_state *mrb, mrb_value seed)
{
  if (mrb_nil_p(seed)) {
    seed = mrb_fixnum_value(time(NULL) + rand());
    if (mrb_fixnum(seed) < 0) {
      seed = mrb_fixnum_value( 0 - mrb_fixnum(seed));
    }
  }

  srand((unsigned) mrb_fixnum(seed));

  return seed;
}

mrb_value
mrb_random_rand(mrb_state *mrb, mrb_value max)
{
  int crand  = rand();
  mrb_value value;

  if (mrb_fixnum(max) == 0) {
    value = mrb_float_value(1.0 * crand / RAND_MAX);
  } else {
    value = mrb_fixnum_value(crand % mrb_fixnum(max));
  }

  return value;
}

static mrb_value
mrb_f_rand(mrb_state *mrb, mrb_value self)
{
  mrb_value *argv;
  int argc;
  mrb_value max;

  mrb_get_args(mrb, "*", &argv, &argc);

  if (argc == 0) {
    max = mrb_fixnum_value(0);
  } else if (argc == 1) {
    max = argv[0];
    if (!mrb_nil_p(max) && !mrb_fixnum_p(max)) {
      max = mrb_check_convert_type(mrb, max, MRB_TT_FIXNUM, "Fixnum", "to_int");
    }
    if (!mrb_nil_p(max) && mrb_fixnum(max) < 0) {
      max = mrb_fixnum_value(0 - mrb_fixnum(max));
    }

    if (!mrb_fixnum_p(max)) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid argument type");
      return mrb_nil_value();
    }
  } else {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (%d for 0..1)", argc);
    return mrb_nil_value();
  }

  mrb_value seed = mrb_gv_get(mrb, mrb_intern(mrb, RAND_SEED_KEY));
  if (mrb_nil_p(seed)) {
    mrb_random_srand(mrb, mrb_nil_value());
  }

  return mrb_random_rand(mrb, max);
}

static mrb_value
mrb_f_srand(mrb_state *mrb, mrb_value self)
{
  mrb_value *argv;
  int argc;
  mrb_value seed;

  mrb_get_args(mrb, "*", &argv, &argc);

  if (argc == 0) {
    seed = mrb_nil_value();
  } else if (argc == 1) {
    seed = argv[0];
    if (!mrb_nil_p(seed) && !mrb_fixnum_p(seed)) {
      seed = mrb_check_convert_type(mrb, seed, MRB_TT_FIXNUM, "Fixnum", "to_int");
    }
    if (!mrb_nil_p(seed) && mrb_fixnum(seed) < 0) {
      seed = mrb_fixnum_value(0 - mrb_fixnum(seed));
    }

    if (!mrb_fixnum_p(seed)) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid argument type");
      return mrb_nil_value();
    }
  } else {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "wrong number of arguments (%d for 0..1)", argc);
    return mrb_nil_value();
  }

  seed = mrb_random_srand(mrb, seed);
  mrb_value old_seed = mrb_gv_get(mrb, mrb_intern(mrb, RAND_SEED_KEY));
  mrb_gv_set(mrb, mrb_intern(mrb, RAND_SEED_KEY), seed);

  return old_seed;
}

void
mrb_init_random(mrb_state *mrb)
{
  struct RClass *krn;
  krn = mrb->kernel_module;

  mrb_gv_set(mrb, mrb_intern(mrb, RAND_SEED_KEY), mrb_nil_value());

  mrb_define_method(mrb, krn, "rand",                    mrb_f_rand,         ARGS_ANY());
  mrb_define_method(mrb, krn, "srand",                   mrb_f_srand,        ARGS_ANY());

}

#endif /* ENABLE_RANDOM */
