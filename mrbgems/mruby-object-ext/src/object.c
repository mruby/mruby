#include "mruby.h"
#include "mruby/array.h"

/*
 *  call-seq:
 *     nil.to_a    -> []
 *
 *  Always returns an empty array.
 */

static mrb_value
nil_to_a(mrb_state *mrb, mrb_value obj)
{
    return mrb_ary_new(mrb);
}

/*
 *  call-seq:
 *     nil.to_f    -> 0.0
 *
 *  Always returns zero.
 */

static mrb_value
nil_to_f(mrb_state *mrb, mrb_value obj)
{
    return mrb_float_value(mrb, 0.0);
}

/*
 *  call-seq:
 *     nil.to_i    -> 0
 *
 *  Always returns zero.
 */

static mrb_value
nil_to_i(mrb_state *mrb, mrb_value obj)
{
    return mrb_fixnum_value(0);
}

void
mrb_mruby_object_ext_gem_init(mrb_state* mrb)
{
  struct RClass * n = mrb->nil_class;

  mrb_define_method(mrb, n, "to_a", nil_to_a,       MRB_ARGS_NONE());
  mrb_define_method(mrb, n, "to_f", nil_to_f,       MRB_ARGS_NONE());
  mrb_define_method(mrb, n, "to_i", nil_to_i,       MRB_ARGS_NONE());
}

void
mrb_mruby_object_ext_gem_final(mrb_state* mrb)
{
}
