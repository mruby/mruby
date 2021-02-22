#include "mruby.h"
#include "mruby/array.h"
#include "mruby/hash.h"
#include "mruby/proc.h"
#include "mruby/variable.h"

mrb_value proc_local_variables(mrb_state *mrb, struct RProc *proc);

static mrb_value
binding_local_variables(mrb_state *mrb, mrb_value self)
{
  struct RProc *proc = mrb_proc_ptr(mrb_iv_get(mrb, self, mrb_intern_lit(mrb, "proc")));
  return proc_local_variables(mrb, proc);
}

static mrb_value
mrb_f_binding(mrb_state *mrb, mrb_value self)
{
  struct RObject *obj;
  mrb_value binding;
  struct RProc *proc;
  mrb_int i;

  obj = (struct RObject*)mrb_obj_alloc(mrb, MRB_TT_OBJECT, mrb_class_get(mrb, "Binding"));
  binding = mrb_obj_value(obj);
  proc = mrb->c->ci[-1].proc;
  mrb_iv_set(mrb, binding, mrb_intern_lit(mrb, "proc"), mrb_obj_value(proc));
  mrb_iv_set(mrb, binding, mrb_intern_lit(mrb, "recv"), self);
  return binding;
}

void
mrb_mruby_binding_gem_init(mrb_state *mrb)
{
  struct RClass *binding = mrb_define_class(mrb, "Binding", mrb->object_class);
  mrb_undef_class_method(mrb, binding, "new");

  mrb_define_method(mrb, mrb->kernel_module, "binding", mrb_f_binding, MRB_ARGS_NONE());

  mrb_define_method(mrb, binding, "local_variables", binding_local_variables, MRB_ARGS_NONE());
}

void
mrb_mruby_binding_gem_final(mrb_state *mrb)
{
}
