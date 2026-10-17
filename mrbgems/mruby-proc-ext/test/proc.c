#include <mruby.h>
#include <mruby/array.h>
#include <mruby/proc.h>
#include <mruby/class.h>

static mrb_value
return_func_name(mrb_state *mrb, mrb_value self)
{
  return mrb_cfunc_env_get(mrb, 0);
}

static mrb_value
proc_new_cfunc_with_env(mrb_state *mrb, mrb_value self)
{
  mrb_sym n;
  mrb_value n_val;
  mrb_method_t m;
  struct RProc *p;
  mrb_get_args(mrb, "n", &n);
  n_val = mrb_symbol_value(n);
  p = mrb_proc_new_cfunc_with_env(mrb, return_func_name, 1, &n_val);
  MRB_METHOD_FROM_PROC(m, p);
  mrb_define_method_raw(mrb, mrb_class_ptr(self), n, m);
  return self;
}

static mrb_value
return_env(mrb_state *mrb, mrb_value self)
{
  mrb_int idx;
  mrb_get_args(mrb, "i", &idx);
  return mrb_cfunc_env_get(mrb, idx);
}

static mrb_value
cfunc_env_get(mrb_state *mrb, mrb_value self)
{
  mrb_sym n;
  const mrb_value *argv;
  mrb_int argc;
  mrb_get_args(mrb, "na", &n, &argv, &argc);
  struct RProc *p = mrb_proc_new_cfunc_with_env(mrb, return_env, argc, argv);
  mrb_method_t m;
  MRB_METHOD_FROM_PROC(m, p);
  mrb_define_method_raw(mrb, mrb_class_ptr(self), n, m);
  return self;
}

static mrb_value
cfunc_without_env(mrb_state *mrb, mrb_value self)
{
  return mrb_cfunc_env_get(mrb, 0);
}

/* A cfunc proc that returns [env[0], *args]. */
static mrb_value
env_and_args(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_value ary;

  mrb_get_args(mrb, "*", &argv, &argc);
  ary = mrb_ary_new_capa(mrb, argc + 1);
  mrb_ary_push(mrb, ary, mrb_cfunc_env_get(mrb, 0));
  while (argc-- > 0) {
    mrb_ary_push(mrb, ary, *argv++);
  }
  return ary;
}

static mrb_value
cfunc_proc_with_env(mrb_state *mrb, mrb_value self)
{
  mrb_value v;

  mrb_get_args(mrb, "o", &v);
  return mrb_obj_value(mrb_proc_new_cfunc_with_env(mrb, env_and_args, 1, &v));
}

static mrb_value
raise_body(mrb_state *mrb, mrb_value self)
{
  mrb_raise(mrb, E_ARGUMENT_ERROR, "from a cfunc proc");
  return mrb_nil_value();
}

static mrb_value
cfunc_proc_raising(mrb_state *mrb, mrb_value self)
{
  return mrb_obj_value(mrb_proc_new_cfunc(mrb, raise_body));
}

/* Proc#call from C: mrb_funcall enters the VM for the `call` iseq, whose
   OP_CALL then has to hand a cfunc proc's value back to C. */
static mrb_value
funcall_call(mrb_state *mrb, mrb_value self)
{
  mrb_value pr;
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "o*", &pr, &argv, &argc);
  return mrb_funcall_argv(mrb, pr, mrb_intern_lit(mrb, "call"), argc, argv);
}

void mrb_mruby_proc_ext_gem_test(mrb_state *mrb)
{
  struct RClass *cls;

  cls = mrb_define_class(mrb, "ProcExtTest", mrb->object_class);
  mrb_define_class_method(mrb, cls, "mrb_proc_new_cfunc_with_env", proc_new_cfunc_with_env, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, cls, "mrb_cfunc_env_get", cfunc_env_get, MRB_ARGS_REQ(2));
  mrb_define_method(mrb, cls, "cfunc_without_env", cfunc_without_env, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, cls, "cfunc_proc_with_env", cfunc_proc_with_env, MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, cls, "cfunc_proc_raising", cfunc_proc_raising, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, cls, "funcall_call", funcall_call, MRB_ARGS_REQ(1)|MRB_ARGS_REST());
}
