#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/compile.h>
#include <mruby/error.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

/* provided by mruby-eval */
mrb_value mrb_f_eval(mrb_state *mrb, mrb_value self);

static mrb_value
mrb_binding_eval(mrb_state *mrb, mrb_value binding)
{
  mrb_callinfo *ci = mrb->c->ci;
  int argc = ci->n;
  mrb_value *argv = ci->stack + 1;

  if (argc < 15) {
    argv[0] = mrb_ary_new_from_values(mrb, argc, argv);
    argv[1] = argv[argc];       /* copy block */
    ci->n = 15;
  }
  mrb_ary_splice(mrb, argv[0], 1, 0, binding); /* insert binding as 2nd argument */
  return mrb_f_eval(mrb, binding);
}

void
mrb_mruby_binding_eval_gem_init(mrb_state *mrb)
{
  struct RClass *binding = mrb_class_get_id(mrb, MRB_SYM(Binding));
  mrb_define_method(mrb, binding, "eval", mrb_binding_eval, MRB_ARGS_ANY());
}

void
mrb_mruby_binding_eval_gem_final(mrb_state *mrb)
{
}
