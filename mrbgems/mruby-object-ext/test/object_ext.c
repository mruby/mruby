#include <mruby.h>

static mrb_value
obj_instance_exec_from_c(mrb_state *mrb, mrb_value self)
{
  mrb_int argc;
  const mrb_value *argv;
  mrb_value blk;
  mrb_get_args(mrb, "*&!", &argv, &argc, &blk);
  return mrb_funcall_with_block(mrb, self, mrb_intern_lit(mrb, "instance_exec"), argc, argv, blk);
}

void
mrb_mruby_object_ext_gem_test(mrb_state *mrb)
{
  mrb_define_method(mrb, mrb->kernel_module, "instance_exec_from_c", obj_instance_exec_from_c, MRB_ARGS_ANY());
}
