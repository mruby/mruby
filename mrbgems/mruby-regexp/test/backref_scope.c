#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/variable.h>
#include <mruby/internal.h>

/* Runs a script through mrb_load_string() while the VM is mid-execution,
   the way an embedding host does from a C function it defines: mrb_top_run()
   pushes a fresh frame whose proc captured no scope, the frame that is
   transparent to `$~` owner resolution in svar_owner() (see vm.c). The
   Ruby-visible shape is pinned by test/backref_scope.rb. */
static mrb_value
backref_nested_load(mrb_state *mrb, mrb_value self)
{
  const char *s;
  mrb_get_args(mrb, "z", &s);
  return mrb_load_string(mrb, s);
}

/* The two below drive MRB_SVAR_LASTLINE, the container key no global is
   registered for yet, straight through the public accessors. Each is a C
   frame, so a call lands on the calling Ruby scope the way a `$_` built on
   them would; what they pin is that the second key shares `$~`'s container
   without either key disturbing the other. */
static mrb_value
svar_lastline_get(mrb_state *mrb, mrb_value self)
{
  return mrb_vm_svar_get(mrb, MRB_SVAR_LASTLINE);
}

static mrb_value
svar_lastline_set(mrb_state *mrb, mrb_value self)
{
  mrb_value v;
  mrb_get_args(mrb, "o", &v);
  mrb_vm_svar_set(mrb, MRB_SVAR_LASTLINE, v);
  return v;
}

/* Whether the calling Ruby frame carries a special-variable container,
   which is how the lazy allocation contract (a nil write allocates
   nothing) is observable at all: the container's existence has no other
   Ruby-visible face. The walk stops at the nearest non-C frame, so this
   reports on a method that calls it directly, not on the owner a block's
   resolution would reach. */
static mrb_value
svar_container_p(mrb_state *mrb, mrb_value self)
{
  mrb_callinfo *ci = mrb->c->ci;

  while (ci > mrb->c->cibase) {
    const struct RProc *p = ci->proc;
    if (p && !MRB_PROC_CFUNC_P(p)) break;
    ci--;
  }
  return mrb_bool_value(mrb_ci_svar(mrb->c, ci) != NULL);
}

void
mrb_mruby_regexp_gem_test(mrb_state *mrb)
{
  mrb_define_method(mrb, mrb->object_class, "__backref_nested_load", backref_nested_load, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, mrb->object_class, "__svar_lastline", svar_lastline_get, MRB_ARGS_NONE());
  mrb_define_method(mrb, mrb->object_class, "__svar_lastline_set", svar_lastline_set, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, mrb->object_class, "__svar_container?", svar_container_p, MRB_ARGS_NONE());
}
