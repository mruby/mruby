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

/*
 *  call-seq:
 *     at_exit { block } -> proc
 *
 *  Converts _block_ to a +Proc+ object (and therefore
 *  binds it at the point of call) and registers it for execution when
 *  the program exits. If multiple handlers are registered, they are
 *  executed in reverse order of registration.
 *
 *     def do_at_exit(str1)
 *       at_exit { print str1 }
 *     end
 *     at_exit { puts "cruel world" }
 *     do_at_exit("goodbye ")
 *     exit
 *
 *  <em>produces:</em>
 *
 *     goodbye cruel world
 */
static mrb_value
mrb_f_at_exit(mrb_state *mrb, mrb_value self)
{
  mrb_value b;
  mrb_get_args(mrb, "&", &b);
  mrb_atexit_proc(mrb, b);
  return b;
}

void
mrb_mruby_kernel_ext_gem_init(mrb_state *mrb)
{
  struct RClass *krn = mrb->kernel_module;

  mrb_define_module_function(mrb, krn, "fail", mrb_f_raise, MRB_ARGS_OPT(2));
  mrb_define_module_function(mrb, krn, "at_exit", mrb_f_at_exit, MRB_ARGS_NONE() | MRB_ARGS_BLOCK());
  mrb_define_method(mrb, krn, "__method__", mrb_f_method, MRB_ARGS_NONE());
}

void
mrb_mruby_kernel_ext_gem_final(mrb_state *mrb)
{
}
