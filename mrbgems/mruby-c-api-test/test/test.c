#include "mruby.h"


static mrb_value
func(mrb_state *mrb, mrb_value self)
{
  return mrb_call_super(mrb, 0, NULL);
}

static mrb_value
args(mrb_state* mrb, mrb_value self)
{
  mrb_value* argv; int argc;
  mrb_get_args(mrb, "*", &argv, &argc);

  return mrb_call_super(mrb, argc, argv);
}

void
mrb_mruby_c_api_test_gem_test(mrb_state *mrb)
{
  struct RClass *base, *derived;
  base = mrb_define_class(mrb, "CallSuperBase", mrb->object_class);
  derived = mrb_define_class(mrb, "CallSuperDerived", base);
  mrb_define_method(mrb, derived, "func", &func, MRB_ARGS_NONE());
  mrb_define_method(mrb, derived, "args", &args, MRB_ARGS_NONE());
  mrb_define_method(mrb, derived, "test", &args, MRB_ARGS_NONE());
}
