/*
** env.c - helpers for test/t/env.rb
**
** The shape of a closed env's heap stack has no Ruby-visible face, so the
** invariants of mruby/internal.h are asked about from here: only an env
** flagged as carrying the special-variable slot has one, and a closed env
** sized without it grows into one on the first write that needs it.
**
** The shutdown probe at the bottom asks the same about the teardown
** mrb_close() runs, which no Ruby frame outlives.
**
** A second, unrelated probe lives here too: mruby/proc.h documents
** MRB_ENV_SET_BIDX() as public, so its idx argument must be safe to pass
** an expression with a side effect, which env_set_bidx_eval_count() below
** checks by counting.
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

/* MRB_ENV_SET_BIDX() is documented public (mruby/proc.h), so an idx
   argument with a side effect must be evaluated exactly once: mrb_assert()
   used to make the macro's own body evaluate idx a second time under
   MRB_DEBUG. The fake env never reaches the GC; only its flags word is
   touched. */
static mrb_value
env_set_bidx_eval_count(mrb_state *mrb, mrb_value self)
{
  struct REnv fake;
  int count = 0;

  fake.flags = 0;
  MRB_ENV_SET_BIDX(&fake, count++);
  return mrb_int_value(mrb, count);
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
   internal object with no Ruby face, and neither is a forwarded env.
   :none where the env carries no slot. */
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
  case MRB_TT_ENV:
    return mrb_symbol_value(mrb_intern_lit(mrb, "env"));
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

/* What mrb_close() does before it runs the atexit callbacks
   (mrb_protect_atexit() in error.c): the top-level frame's env is detached
   and the frame's special-variable container follows it into the escaped
   env, so a proc an atexit callback calls still reads what the top level
   could see. The arena is already restored there, which leaves the frame
   the container's only root until mrb_env_detach() stores it, and
   mrb_env_unshare() allocates in between; a collection in that window and
   the slot is handed a swept object.

   Asking about that window takes a state of its own, the answer being
   there only once mrb_close() has run, and an atexit callback to read it
   from, nothing Ruby-side outliving the teardown. What makes the window
   fire is the collection MRB_GC_STRESS puts in every allocation: the
   unshare's is a plain mrb_malloc_simple(), which no other build collects
   in short of running out of memory. So the probe answers nil where the
   build cannot discriminate, and the test skips rather than passing on a
   window that never opened. */
#if defined(MRBTEST_COMPILER_PRISM) && defined(MRB_GC_STRESS) && defined(MRB_DEBUG)
/* MRBTEST_COMPILER_PRISM (mrbgem.rake) says mruby-compiler is in this
   build, which is what mrb_load_string() below needs, and `global_mrb` is
   that gem's allocator hook: mrc_ccontext_new() points it at whatever
   state compiles, so the caller's has to be put back. */
#include <mruby/compile.h>

extern mrb_state *global_mrb;

struct shutdown_probe {
  mrb_bool ran;         /* the callback was reached at all */
  mrb_bool shaped;      /* the escaped env is the one the probe asks about */
  mrb_bool kept;        /* and its slot still holds the container */
};

/* Registered last, so it runs first of the state's atexit callbacks and
   sees what the detach published rather than what a later callback left. */
static void
shutdown_probe_run(mrb_state *mrb)
{
  struct shutdown_probe *probe = (struct shutdown_probe*)mrb->ud;
  mrb_value pr = mrb_gv_get(mrb, mrb_intern_lit(mrb, "$mrbtest_env_escape"));

  probe->ran = TRUE;
  if (mrb_type(pr) != MRB_TT_PROC) return;

  struct REnv *e = MRB_PROC_ENV(mrb_proc_ptr(pr));
  if (!e || MRB_ENV_ONSTACK_P(e) || !MRB_ENV_SVAR_P(e)) return;

  /* The slot holds the container the top-level scope wrote. A sweep in the
     window leaves a freed object there instead, which reads as neither a
     container nor anything else the write could have put in the slot. */
  probe->shaped = TRUE;
  probe->kept = mrb_type(MRB_ENV_SVAR_SLOT(e->stack, MRB_ENV_LEN(e))) == MRB_TT_SVAR;
}

static mrb_value
env_shutdown_svar(mrb_state *mrb, mrb_value self)
{
  struct shutdown_probe probe = { FALSE, FALSE, FALSE };
  mrb_state *caller = global_mrb;
  /* core alone: the teardown under test is core's, and the gems would only
     make the state the probe throws away costlier to build */
  mrb_state *sub = mrb_open_core();

  if (sub) {
    sub->ud = &probe;
    mrb_init_test_env(sub);
    /* Building the state is setup, not the window under test, and under
       MRB_GC_STRESS every allocation of it would collect; the teardown is
       what has to. */
    sub->gc.disabled = TRUE;
    /* a block over the top-level frame, kept by a global so the env
       outlives the teardown, and a container on the scope that block
       resolves to */
    mrb_load_string(sub,
                    "def __mrbtest_env_keep(&b); b; end\n"
                    "$mrbtest_env_escape = __mrbtest_env_keep { }\n"
                    "__env_svar_write(true)\n");
    sub->gc.disabled = FALSE;
    if (!sub->exc) {
      mrb_state_atexit(sub, shutdown_probe_run);
    }
    mrb_close(sub);
  }
  global_mrb = caller;

  if (!probe.ran || !probe.shaped) return mrb_nil_value();
  return mrb_bool_value(probe.kept);
}
#else
static mrb_value
env_shutdown_svar(mrb_state *mrb, mrb_value self)
{
  return mrb_nil_value();
}
#endif

void
mrb_init_test_env(mrb_state *mrb)
{
  struct RClass *o = mrb->object_class;

  mrb_define_method(mrb, o, "__env_svar?", env_svar_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_len", env_len, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_svar_slot", env_svar_slot, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_make_legacy", env_make_legacy, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_set_bidx_eval_count", env_set_bidx_eval_count, MRB_ARGS_NONE());
  mrb_define_method(mrb, o, "__env_svar_read", env_svar_read, MRB_ARGS_NONE());
  mrb_define_method(mrb, o, "__env_svar_write", env_svar_write, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_cfunc_proc", env_cfunc_proc_new, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, o, "__env_shutdown_svar", env_shutdown_svar, MRB_ARGS_NONE());
}
