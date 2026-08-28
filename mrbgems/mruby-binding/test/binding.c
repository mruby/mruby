#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/internal.h>
#include <mruby/variable.h>

static mrb_value
binding_in_c(mrb_state *mrb, mrb_value self)
{
  return mrb_funcall_argv(mrb, mrb_obj_value(mrb->object_class), MRB_SYM(binding), 0, NULL);
}

/* The local-variable space a binding holds (binding_env_new_lvspace() in
   src/binding.c): a closed env born with the special-variable slot past
   its locals, and the env Binding#local_variable_set grows through
   mrb_proc_merge_lvar(). The four helpers below are what the shape of that
   stack can be asked about with; there is no Ruby-visible face for it. */
static struct REnv*
binding_env(mrb_state *mrb, mrb_value binding)
{
  mrb_value obj = mrb_iv_get(mrb, binding, MRB_SYM(env));

  mrb_check_type(mrb, obj, MRB_TT_ENV);
  return (struct REnv*)mrb_obj_ptr(obj);
}

static mrb_value
binding_env_svar_p(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(MRB_ENV_SVAR_P(binding_env(mrb, mrb_get_arg1(mrb))));
}

static mrb_value
binding_env_len(mrb_state *mrb, mrb_value self)
{
  return mrb_int_value(mrb, MRB_ENV_LEN(binding_env(mrb, mrb_get_arg1(mrb))));
}

/* Rewrites the env into what out-of-tree code builds by hand: a closed env
   over a heap stack of exactly its locals, carrying no slot. The stack
   shrinks for real, so a later read one past the locals is an
   out-of-bounds access a sanitizer build reports. */
static mrb_value
binding_env_drop_svar(mrb_state *mrb, mrb_value self)
{
  struct REnv *e = binding_env(mrb, mrb_get_arg1(mrb));

  if (MRB_ENV_ONSTACK_P(e) || !MRB_ENV_SVAR_P(e)) return mrb_false_value();
  size_t len = (size_t)MRB_ENV_LEN(e);
  if (len == 0) return mrb_false_value();

  e->stack = (mrb_value*)mrb_realloc(mrb, e->stack, sizeof(mrb_value) * len);
  MRB_ENV_CLEAR_SVAR(e);
  return mrb_true_value();
}

/* Puts a marker in the slot and reads it back. What the core keeps there
   has no Ruby face of its own; any marked value stands in for it here,
   which is all the merge below moves. */
static mrb_value
binding_env_slot_set(mrb_state *mrb, mrb_value self)
{
  mrb_value binding, v;
  mrb_get_args(mrb, "oo", &binding, &v);

  struct REnv *e = binding_env(mrb, binding);
  if (!MRB_ENV_SVAR_P(e)) return mrb_false_value();
  MRB_ENV_SVAR_SLOT(e->stack, MRB_ENV_LEN(e)) = v;
  mrb_write_barrier(mrb, (struct RBasic*)e);
  return mrb_true_value();
}

static mrb_value
binding_env_slot_get(mrb_state *mrb, mrb_value self)
{
  struct REnv *e = binding_env(mrb, mrb_get_arg1(mrb));

  if (!MRB_ENV_SVAR_P(e)) return mrb_nil_value();
  return MRB_ENV_SVAR_SLOT(e->stack, MRB_ENV_LEN(e));
}

void
mrb_mruby_binding_gem_test(mrb_state *mrb)
{
  struct RClass *o = mrb->object_class;

  mrb_define_method(mrb, o, "binding_in_c", binding_in_c, MRB_ARGS_NONE());
  mrb_define_method(mrb, o, "__binding_env_svar?", binding_env_svar_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__binding_env_len", binding_env_len, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__binding_env_drop_svar", binding_env_drop_svar, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__binding_env_slot_set", binding_env_slot_set, MRB_ARGS_REQ(2));
  mrb_define_method(mrb, o, "__binding_env_slot_get", binding_env_slot_get, MRB_ARGS_REQ(1));
}
