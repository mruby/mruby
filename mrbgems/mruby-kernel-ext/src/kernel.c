#include "mruby.h"
#include "mruby/error.h"

/*
 *  call-seq:
 *     __method__         -> symbol
 *
 *  Returns the name at the definition of the current method as a
 *  Symbol.
 *  If called outside of a method, it returns <code>nil</code>.
 *
 */
static mrb_value
mrb_f_method(mrb_state *mrb, mrb_value self)
{
  mrb_callinfo *ci = mrb->c->ci;
  ci--;
  if (ci->mid)
    return mrb_symbol_value(ci->mid);
  else
    return mrb_nil_value();
}

void
mrb_mruby_kernel_ext_gem_init(mrb_state *mrb)
{
  struct RClass *krn = mrb->kernel_module;

  mrb_define_module_function(mrb, krn, "fail", mrb_f_raise, MRB_ARGS_OPT(2));
  mrb_define_method(mrb, krn, "__method__", mrb_f_method, MRB_ARGS_NONE());
}

void
mrb_mruby_kernel_ext_gem_final(mrb_state *mrb)
{
}
