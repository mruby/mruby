#include <mruby.h>
#include <mruby/presym.h>
#include <mruby/proc.h>
#include <mruby/variable.h>

/* provided by mruby-proc-ext */
mrb_value mrb_proc_source_location(mrb_state *mrb, const struct RProc *p);

/* provided by mruby-binding */
mrb_value mrb_binding_new(mrb_state *mrb, const struct RProc *proc, mrb_value recv, struct REnv *env);

/*
 * call-seq:
 *   prc.binding -> binding
 *
 * Returns a Binding object, which is the execution context that the proc
 * was defined in. The returned binding retains the context in which the
 * proc was created, including local variables, methods, and constants.
 *
 *   def fred(param)
 *     proc {}
 *   end
 *
 *   b = fred(99).binding
 *   b.eval("param")   #=> 99
 *   b.eval("param = 1")
 *   b.eval("param")   #=> 1
 *
 *   # Local variables in the binding's scope
 *   p = proc { |x| x + 1 }
 *   a, b, c = 1, 2, 3
 *   bind = p.binding
 *   bind.local_variables  #=> [:a, :b, :bind, :c, :p]
 */
static mrb_value
mrb_proc_binding(mrb_state *mrb, mrb_value procval)
{
  const struct RProc *proc = mrb_proc_ptr(procval);
  struct REnv *env;
  mrb_value receiver;

  if (!proc || MRB_PROC_CFUNC_P(proc) || !proc->upper || MRB_PROC_CFUNC_P(proc->upper)) {
    env = NULL;
    proc = NULL;
    receiver = mrb_nil_value();
  }
  else {
    env = MRB_PROC_ENV(proc);
    mrb_assert(env);
    proc = proc->upper;
    receiver = MRB_ENV_LEN(env) > 0 ? env->stack[0] : mrb_nil_value();
  }

  mrb_value binding = mrb_binding_new(mrb, proc, receiver, env);
  mrb_iv_set(mrb, binding, MRB_SYM(source_location), mrb_proc_source_location(mrb, mrb_proc_ptr(procval)));
  return binding;
}

/*
 * Initializes the mruby-proc-binding gem by adding the binding method
 * to the Proc class. This allows any proc object to return its execution
 * context as a Binding object.
 *
 * The binding method takes no arguments and returns a Binding object
 * that captures the lexical environment where the proc was created.
 */
void
mrb_mruby_proc_binding_gem_init(mrb_state *mrb)
{
  mrb_define_method_id(mrb, mrb->proc_class, MRB_SYM(binding), mrb_proc_binding, MRB_ARGS_NONE());
}

void
mrb_mruby_proc_binding_gem_final(mrb_state *mrb)
{
}
