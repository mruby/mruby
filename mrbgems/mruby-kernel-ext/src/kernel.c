#include "mruby.h"
#include "mruby/error.h"

void
mrb_mruby_kernel_ext_gem_init(mrb_state *mrb)
{
  struct RClass *krn = mrb->kernel_module;

  mrb_define_module_function(mrb, krn, "fail", mrb_f_raise, MRB_ARGS_NONE());
}

void
mrb_mruby_kernel_ext_gem_final(mrb_state *mrb)
{
}
