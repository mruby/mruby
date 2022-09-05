#include <stdlib.h>
#include <mruby.h>

#ifndef EXIT_SUCCESS
# define EXIT_SUCCESS 0
#endif

#ifndef EXIT_FAILURE
# define EXIT_FAILURE 1
#endif

static mrb_value
f_exit_bang(mrb_state *mrb, mrb_value self)
{
  mrb_value status = mrb_true_value();
  int istatus;

  mrb_get_args(mrb, "|o", &status);
  istatus = mrb_true_p(status) ? EXIT_SUCCESS :
            mrb_false_p(status) ? EXIT_FAILURE :
            (int)mrb_int(mrb, status);
  exit(istatus);

  /* not reached */
  return status;
}

void
mrb_mruby_exit_gem_init(mrb_state* mrb)
{
  mrb_define_method(mrb, mrb->kernel_module, "exit", f_exit_bang, MRB_ARGS_OPT(1));
  mrb_define_method(mrb, mrb->kernel_module, "exit!", f_exit_bang, MRB_ARGS_OPT(1));
}

void
mrb_mruby_exit_gem_final(mrb_state* mrb)
{
}
