#include "mruby.h"
#include "mruby/proc.h"

static mrb_value
dummy_cfunc(mrb_state *mrb, mrb_value self)
{
  (void)mrb;
  return self;
}

void
mrb_mruby_debug_ext_gem_test(mrb_state *mrb)
{
  mrb_define_global_const(
      mrb, "CFuncProc", mrb_obj_value(mrb_proc_new_cfunc(mrb, dummy_cfunc)));
}
