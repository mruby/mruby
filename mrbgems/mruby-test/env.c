/*
** env.c - helpers for test/t/env.rb
**
** The shape of a closed env's heap stack has no Ruby-visible face, so the
** invariants of mruby/internal.h are asked about from here: only an env
** flagged as carrying the special-variable slot has one, and a closed env
** sized without it grows into one on the first write that needs it.
*/

#include <stdlib.h>
#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/internal.h>
#include <mruby/variable.h>

void mrb_init_test_env(mrb_state *mrb);

/* The env a proc closed over, or NULL where it holds none. */
static struct REnv*
env_of_proc(mrb_state *mrb, mrb_value proc)
{
  mrb_check_type(mrb, proc, MRB_TT_PROC);
  return MRB_PROC_ENV(mrb_proc_ptr(proc));
}

/* Whether the proc's env carries the slot past its locals; nil where the
   proc closed over no env at all. */
static mrb_value
env_svar_p(mrb_state *mrb, mrb_value self)
{
  mrb_value proc = mrb_get_arg1(mrb);
  struct REnv *e = env_of_proc(mrb, proc);

  if (!e) return mrb_nil_value();
  return mrb_bool_value(MRB_ENV_SVAR_P(e));
}

/* The number of locals the env holds, which is where the slot sits when
   the env carries one. */
static mrb_value
env_len(mrb_state *mrb, mrb_value self)
{
  mrb_value proc = mrb_get_arg1(mrb);
  struct REnv *e = env_of_proc(mrb, proc);

  if (!e) return mrb_nil_value();
  return mrb_int_value(mrb, MRB_ENV_LEN(e));
}

/* What the slot holds, named rather than returned: the container is an
   internal object with no Ruby face. :none where the env carries no
   slot. */
static mrb_value
env_svar_slot(mrb_state *mrb, mrb_value self)
{
  mrb_value proc = mrb_get_arg1(mrb);
  struct REnv *e = env_of_proc(mrb, proc);

  if (!e || !MRB_ENV_SVAR_P(e)) return mrb_symbol_value(mrb_intern_lit(mrb, "none"));

  mrb_value v = MRB_ENV_SVAR_SLOT(e->stack, MRB_ENV_LEN(e));
  switch (mrb_type(v)) {
  case MRB_TT_FALSE:
    if (mrb_nil_p(v)) return mrb_symbol_value(mrb_intern_lit(mrb, "nil"));
    break;
  case MRB_TT_SVAR:
    return mrb_symbol_value(mrb_intern_lit(mrb, "svar"));
  default:
    break;
  }
  return mrb_symbol_value(mrb_intern_lit(mrb, "other"));
}

/* Rewrites the proc's env into what out-of-tree code builds by hand: a
   closed env over a heap stack of exactly its locals, carrying no slot.
   The stack shrinks for real, so a later read one past the locals is an
   out-of-bounds access a sanitizer build reports. FALSE where the env is
   not one this can be done to (on the stack, already without the slot, or
   holding no locals to keep the allocation non-empty). */
static mrb_value
env_make_legacy(mrb_state *mrb, mrb_value self)
{
  mrb_value proc = mrb_get_arg1(mrb);
  struct REnv *e = env_of_proc(mrb, proc);

  if (!e || MRB_ENV_ONSTACK_P(e) || !MRB_ENV_SVAR_P(e)) return mrb_false_value();
  size_t len = (size_t)MRB_ENV_LEN(e);
  if (len == 0) return mrb_false_value();

  e->stack = (mrb_value*)mrb_realloc(mrb, e->stack, sizeof(mrb_value) * len);
  MRB_ENV_CLEAR_SVAR(e);
  return mrb_true_value();
}

/* A special-variable read and write from a C frame, which owns no scope of
   its own, so both land on the calling Ruby scope. MRB_SVAR_LASTLINE is
   the key no global is registered for in core, so what these drive is the
   container itself rather than any one variable's semantics. */
static mrb_value
env_svar_read(mrb_state *mrb, mrb_value self)
{
  return mrb_vm_svar_get(mrb, MRB_SVAR_LASTLINE);
}

static mrb_value
env_svar_write(mrb_state *mrb, mrb_value self)
{
  mrb_value v = mrb_get_arg1(mrb);

  mrb_vm_svar_set(mrb, MRB_SVAR_LASTLINE, v);
  return v;
}

/* A proc over an env core itself builds without the slot: a C closure owns
   no Ruby scope, so mrb_proc_new_cfunc_with_env() sizes its stack at
   exactly the values handed to it. Calling the proc answers value 0 back,
   which is what a GC that walked the env correctly leaves behind. */
static mrb_value
env_cfunc_value(mrb_state *mrb, mrb_value self)
{
  return mrb_cfunc_env_get(mrb, 0);
}

static mrb_value
env_cfunc_proc_new(mrb_state *mrb, mrb_value self)
{
  mrb_value v = mrb_get_arg1(mrb);

  return mrb_obj_value(mrb_proc_new_cfunc_with_env(mrb, env_cfunc_value, 1, &v));
}

void
mrb_init_test_env(mrb_state *mrb)
{
  struct RClass *o = mrb->object_class;

  mrb_define_method(mrb, o, "__env_svar?", env_svar_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_len", env_len, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_svar_slot", env_svar_slot, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_make_legacy", env_make_legacy, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_svar_read", env_svar_read, MRB_ARGS_NONE());
  mrb_define_method(mrb, o, "__env_svar_write", env_svar_write, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_cfunc_proc", env_cfunc_proc_new, MRB_ARGS_REQ(1));
}
