/*
** refinement.c - Refinement class and `using` (MRB_USE_REFINEMENTS)
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>

#ifdef MRB_USE_REFINEMENTS

#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/hash.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/gc.h>
#include <mruby/presym.h>
#include <mruby/internal.h>

/* A refinement is a module whose `super` is the class it refines, as in
   CRuby, so a `super` written in a refined method reaches that class and
   `alias` in a refine block finds its methods.  The module that wrote it is
   kept in `__defined_at__`.

   Which refinements a piece of code sees is a lexical matter, and the
   lexical chain here is RProc.upper.  A scope proc carries its active
   refinements as an Array by index (MRB_PROC_REFSCOPE), and the Array is
   held by a weak table on the state so a proc costs no field for it: see
   mrb_vm_refinements() in proc.c for the walk and mrb_gc_clear_dead_refscopes()
   for the table's collection. */

/* --- the weak scope table --- */

struct RArray*
mrb_refscope_at(mrb_state *mrb, uint32_t idx)
{
  mrb_assert(idx > 0 && idx <= mrb->refscopes_len);
  return mrb->refscopes[idx-1];
}

static uint32_t
refscope_register(mrb_state *mrb, struct RArray *scope)
{
  uint32_t i;

  for (i = 0; i < mrb->refscopes_len; i++) {
    if (mrb->refscopes[i] == scope) return i+1;
  }
  for (i = 0; i < mrb->refscopes_len; i++) {
    if (mrb->refscopes[i] == NULL) goto found;
  }
  if (mrb->refscopes_len >= MRB_PROC_REFSCOPE_MAX) {
    /* A slot the collector has yet to notice is dead may free up.  A
       program that turned the collector off is collected once anyway,
       since the alternative is to fail it; one iterating the heap cannot
       be. */
    if (!mrb->gc.iterating) {
      mrb_bool disabled = mrb->gc.disabled;
      mrb->gc.disabled = FALSE;
      mrb_full_gc(mrb);
      mrb->gc.disabled = disabled;
    }
    for (i = 0; i < mrb->refscopes_len; i++) {
      if (mrb->refscopes[i] == NULL) goto found;
    }
    mrb_raise(mrb, E_RUNTIME_ERROR, "too many refinement scopes");
  }
  if (mrb->refscopes_len == mrb->refscopes_capa) {
    uint32_t capa = mrb->refscopes_capa ? mrb->refscopes_capa * 2 : 8;
    /* the realloc may collect; `scope` is on the arena */
    mrb->refscopes = (struct RArray**)mrb_realloc(mrb, mrb->refscopes, sizeof(struct RArray*) * capa);
    mrb->refscopes_capa = capa;
  }
  i = mrb->refscopes_len++;
 found:
  mrb->refscopes[i] = scope;
  return i+1;
}

void
mrb_proc_set_refscope(mrb_state *mrb, struct RProc *p, struct RArray *scope)
{
  uint32_t idx = refscope_register(mrb, scope);

  mrb_assert(!MRB_PROC_CFUNC_P(p));
  MRB_PROC_SET_REFSCOPE(p, idx);
  mrb_field_write_barrier(mrb, (struct RBasic*)p, (struct RBasic*)scope);
}

/* Called once marking is complete: drops every scope nothing marked, so
   the sweep may free it and the slot is reused. */
void
mrb_gc_clear_dead_refscopes(mrb_state *mrb)
{
  for (uint32_t i = 0; i < mrb->refscopes_len; i++) {
    struct RArray *a = mrb->refscopes[i];
    if (a && mrb_object_dead_p(mrb, (struct RBasic*)a)) {
      mrb->refscopes[i] = NULL;
    }
  }
}

/* A scope Proc#refined made, told from one `using` made by a bit of the
   Array's flags that mruby/array.h leaves unused (embed length 0-2, shared
   8).  A `using` written anywhere under such a proc is refused: the copy's
   refinements are fixed when it is made, as CRuby's are. */
#define REFSCOPE_OF_PROC_FL (1u << 12)
#define REFSCOPE_OF_PROC_P(a) (((a)->flags & REFSCOPE_OF_PROC_FL) != 0)

/* Whether `p` is a copy Proc#refined made. */
mrb_bool
mrb_proc_refined_p(mrb_state *mrb, const struct RProc *p)
{
  uint32_t idx = MRB_PROC_REFSCOPE(p);
  return idx != 0 && REFSCOPE_OF_PROC_P(mrb_refscope_at(mrb, idx));
}

/* Raises when `p`, or any proc it was written in up to its scope, is a
   copy Proc#refined made: a block made in such a proc carries no scope of
   its own and reads the copy's. */
static void
check_not_in_refined_proc(mrb_state *mrb, const struct RProc *p)
{
  for (; p && !MRB_PROC_CFUNC_P(p) && p->gc_color != MRB_GC_RED; p = p->upper) {
    if (mrb_proc_refined_p(mrb, p)) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "using is not permitted in a proc with refinements");
    }
    if (MRB_PROC_CREF_P(p)) break;
  }
}

/* --- refinement objects --- */

static struct RClass*
refinement_p(mrb_state *mrb, mrb_value v)
{
  if (!mrb_class_p(v) && !mrb_module_p(v)) return NULL;
  struct RClass *c = mrb_class_ptr(v);
  return MRB_CLASS_REFINEMENT_P(c) ? c : NULL;
}

/* The scope a refine block runs in and a method defined there keeps: the
   owner module's own refinements, all of them, live.  A `using` copies it;
   this Array alone is grown in place, so a method written in one refine
   block sees a refinement the owner writes later, as CRuby's does. */
static mrb_value
owner_scope(mrb_state *mrb, mrb_value owner)
{
  mrb_value a = mrb_iv_get(mrb, owner, MRB_SYM(__activated_refinements__));
  if (mrb_nil_p(a)) {
    a = mrb_ary_new(mrb);
    mrb_iv_set(mrb, owner, MRB_SYM(__activated_refinements__), a);
  }
  return a;
}

static mrb_value
refinements_hash(mrb_state *mrb, mrb_value owner)
{
  mrb_value h = mrb_iv_get(mrb, owner, MRB_SYM(__refinements__));
  if (mrb_nil_p(h)) {
    h = mrb_hash_new(mrb);
    mrb_iv_set(mrb, owner, MRB_SYM(__refinements__), h);
  }
  return h;
}

static struct RClass*
refinement_new(mrb_state *mrb, mrb_value owner, struct RClass *target)
{
  struct RClass *r = mrb_module_new(mrb);
  mrb_value rv = mrb_obj_value(r);

  r->c = mrb->refinement_class;
  r->flags |= MRB_FL_CLASS_IS_REFINEMENT;
  r->super = target;
  mrb_field_write_barrier(mrb, (struct RBasic*)r, (struct RBasic*)target);
  target->flags |= MRB_FL_CLASS_IS_REFINED;
  mrb_iv_set(mrb, rv, MRB_SYM(__refined_class__), mrb_obj_value(target));
  mrb_iv_set(mrb, rv, MRB_SYM(__defined_at__), owner);
  mrb_hash_set(mrb, refinements_hash(mrb, owner), mrb_obj_value(target), rv);
  mrb_ary_unshift(mrb, owner_scope(mrb, owner), rv);
  return r;
}

/*
 *  call-seq:
 *     refine(mod) { block }   -> refinement
 *
 *  Refines +mod+ in the receiver.  The block runs with the refinement as
 *  `self` and as the class a `def` adds to, and sees the receiver's
 *  refinements active.  Returns the refinement, made once per class.
 */
static mrb_value
mod_refine(mrb_state *mrb, mrb_value self)
{
  mrb_value target, blk;

  mrb_get_args(mrb, "o&", &target, &blk);
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }
  if (!mrb_class_p(target) && !mrb_module_p(target)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %C (expected Class or Module)", mrb_obj_class(mrb, target));
  }
  if (refinement_p(mrb, target)) {
    mrb_raise(mrb, E_TYPE_ERROR, "wrong argument type refinement (expected Class or Module)");
  }
  mrb_check_frozen(mrb, mrb_class_ptr(self));

  struct RClass *tc = mrb_class_ptr(target);
  mrb_value rv = mrb_hash_get(mrb, refinements_hash(mrb, self), target);
  struct RClass *r = mrb_nil_p(rv) ? refinement_new(mrb, self, tc) : mrb_class_ptr(rv);
  rv = mrb_obj_value(r);

  /* The block runs on a copy that carries the owner's scope: the block the
     program holds is left as it was. */
  const struct RProc *bp = mrb_proc_ptr(blk);
  if (MRB_PROC_CFUNC_P(bp)) {
    return mrb_yield_with_class(mrb, blk, 1, &rv, rv, r);
  }
  struct RProc *p = MRB_OBJ_ALLOC(mrb, MRB_TT_PROC, mrb->proc_class);
  mrb_proc_copy(mrb, p, bp);
  mrb_proc_set_refscope(mrb, p, mrb_ary_ptr(owner_scope(mrb, self)));
  mrb_yield_with_class(mrb, mrb_obj_value(p), 1, &rv, rv, r);
  return rv;
}

/*
 *  call-seq:
 *     mod.refinements   -> array
 *
 *  The refinements the receiver defined, in definition order.
 */
static mrb_value
mod_refinements(mrb_state *mrb, mrb_value self)
{
  mrb_value h = mrb_iv_get(mrb, self, MRB_SYM(__refinements__));
  if (mrb_nil_p(h)) return mrb_ary_new(mrb);
  return mrb_hash_values(mrb, h);
}

/* --- using --- */

/* Adds the refinements of `mod`, and of the modules it includes, to the
   front of `scope`; one already there is left where it is. */
static void
scope_activate(mrb_state *mrb, mrb_value scope, struct RClass *mod)
{
  /* the module's own refinements come first, the included modules' after,
     each group most recent first */
  mrb_value groups = mrb_ary_new(mrb);
  for (struct RClass *c = mod; c; c = c->super) {
    struct RClass *m = (c->tt == MRB_TT_ICLASS) ? c->c : c;
    mrb_value a = mrb_iv_get(mrb, mrb_obj_value(m), MRB_SYM(__activated_refinements__));
    if (!mrb_nil_p(a) && RARRAY_LEN(a) > 0) mrb_ary_push(mrb, groups, a);
  }
  for (mrb_int g = RARRAY_LEN(groups) - 1; g >= 0; g--) {
    mrb_value a = RARRAY_PTR(groups)[g];
    for (mrb_int i = RARRAY_LEN(a) - 1; i >= 0; i--) {
      mrb_value r = RARRAY_PTR(a)[i];
      mrb_bool present = FALSE;
      for (mrb_int j = 0; j < RARRAY_LEN(scope); j++) {
        if (mrb_obj_ptr(RARRAY_PTR(scope)[j]) == mrb_obj_ptr(r)) { present = TRUE; break; }
      }
      if (!present) mrb_ary_unshift(mrb, scope, r);
    }
  }
}

/* The scope proc the calling Ruby frame writes to, checking that `using`
   was written where CRuby allows it: called directly by `self`, and not in
   a method body or a block inside one. */
static struct RProc*
using_scope_proc(mrb_state *mrb, mrb_value self, const char *who)
{
  mrb_callinfo *ci = mrb->c->ci;

  if (ci == mrb->c->cibase || ci->cci != 0 || ci[-1].proc == NULL || MRB_PROC_CFUNC_P(ci[-1].proc) ||
      !mrb_obj_eq(mrb, ci[-1].stack[0], self)) {
    mrb_raisef(mrb, E_RUNTIME_ERROR, "%s is not called directly", who);
  }
  const struct RProc *p = ci[-1].proc;
  const struct RProc *last = p;
  /* A block given a class to run under, as `module_eval`, `Class.new` and
     `refine` give one, is the scope a `using` written in it reaches, the
     way a `def` written there lands on the given class: the frame says so
     for the block itself, and the env it leaves behind says so for a block
     made inside it (see mrb_vm_definee_class()). */
  if (MRB_CI_GIVEN_CLASS_P(&ci[-1])) {
    check_not_in_refined_proc(mrb, p);
    return (struct RProc*)p;
  }
  /* a red proc is a static one the runtime links, not a scope of the
     program's: the chain ends before it */
  check_not_in_refined_proc(mrb, p);
  while (p && !MRB_PROC_CFUNC_P(p) && p->gc_color != MRB_GC_RED) {
    if (MRB_PROC_SCOPE_P(p) && MRB_PROC_STRICT_P(p)) {
      if (mrb_obj_ptr(self) == mrb->top_self) {
        mrb_raise(mrb, E_RUNTIME_ERROR, "main.using is permitted only at toplevel");
      }
      mrb_raise(mrb, E_RUNTIME_ERROR, "Module#using is not permitted in methods");
    }
    if (MRB_PROC_CREF_P(p)) return (struct RProc*)p;
    if (MRB_PROC_ENV_P(p) && MRB_ENV_GIVEN_CLASS_P(MRB_PROC_ENV(p))) return (struct RProc*)p;
    last = p;
    p = p->upper;
  }
  /* A top-level proc a loader made without marking it a scope (a
     precompiled irep run by mrb_load_irep()) is the file's scope: the chain
     ends at it, and mrb_vm_refinements() reads it the same way. */
  return (struct RProc*)last;
}

static mrb_value
do_using(mrb_state *mrb, mrb_value self, const char *who)
{
  mrb_value mod;

  mrb_get_args(mrb, "o", &mod);
  if (refinement_p(mrb, mod)) {
    mrb_raise(mrb, E_TYPE_ERROR, "wrong argument type refinement (expected Module)");
  }
  if (!mrb_module_p(mod)) {
    mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %C (expected Module)", mrb_obj_class(mrb, mod));
  }
  struct RProc *sp = using_scope_proc(mrb, self, who);

  /* a fresh Array: the one the scope carried may be shared with the method
     bodies that copied it, and they must not see this `using` */
  uint32_t idx = MRB_PROC_REFSCOPE(sp);
  mrb_value scope = idx ? mrb_ary_dup(mrb, mrb_obj_value(mrb_refscope_at(mrb, idx))) : mrb_ary_new(mrb);
  scope_activate(mrb, scope, mrb_class_ptr(mod));
  mrb_obj_freeze(mrb, scope);
  mrb_proc_set_refscope(mrb, sp, mrb_ary_ptr(scope));
  return self;
}

/*
 *  call-seq:
 *     using(module)   -> self
 *
 *  Activates the refinements of +module+ from here to the end of the file
 *  (at top level) or of the class or module body.
 */
static mrb_value
main_using(mrb_state *mrb, mrb_value self)
{
  return do_using(mrb, self, "main.using");
}

static mrb_value
mod_using(mrb_state *mrb, mrb_value self)
{
  return do_using(mrb, self, "Module#using");
}

/*
 *  call-seq:
 *     Module.used_modules   -> array
 *
 *  The modules whose refinements are active where this is called.
 */
static mrb_value
mod_s_used_modules(mrb_state *mrb, mrb_value self)
{
  struct RArray *scope = mrb_vm_caller_refinements(mrb);
  mrb_value ary = mrb_ary_new(mrb);

  if (!scope) return ary;
  for (mrb_int i = 0; i < ARY_LEN(scope); i++) {
    mrb_value owner = mrb_iv_get(mrb, ARY_PTR(scope)[i], MRB_SYM(__defined_at__));
    mrb_bool present = FALSE;
    for (mrb_int j = 0; j < RARRAY_LEN(ary); j++) {
      if (mrb_obj_ptr(RARRAY_PTR(ary)[j]) == mrb_obj_ptr(owner)) { present = TRUE; break; }
    }
    if (!present) mrb_ary_push(mrb, ary, owner);
  }
  return ary;
}

/*
 *  call-seq:
 *     Module.used_refinements   -> array
 *
 *  The refinements active where this is called, most recent first.
 */
static mrb_value
mod_s_used_refinements(mrb_state *mrb, mrb_value self)
{
  struct RArray *scope = mrb_vm_caller_refinements(mrb);

  if (!scope) return mrb_ary_new(mrb);
  return mrb_ary_new_from_values(mrb, ARY_LEN(scope), ARY_PTR(scope));
}

/* --- Refinement --- */

static mrb_value
refinement_target(mrb_state *mrb, mrb_value self)
{
  return mrb_obj_value(mrb_class_ptr(self)->super);
}

struct import_arg {
  struct RClass *to;
  struct RClass *from;
  struct RArray *scope;
};

/* The imported body is a copy that carries the refinement's scope, so a
   call written in it sees the refinement's other methods as one written in
   the refine block does; CRuby rebuilds the body under the refinement's
   cref to the same end. */
static int
import_method(mrb_state *mrb, mrb_sym mid, mrb_method_t m, void *data)
{
  struct import_arg *arg = (struct import_arg*)data;

  if (MRB_METHOD_UNDEF_P(m)) return 0;
  if (!MRB_METHOD_PROC_P(m) || MRB_METHOD_PROC(m) == NULL || MRB_PROC_CFUNC_P(MRB_METHOD_PROC(m))) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "Can't import method which is not defined with Ruby code: %C#%n", arg->from, mid);
  }
  int ai = mrb_gc_arena_save(mrb);
  struct RProc *p = MRB_OBJ_ALLOC(mrb, MRB_TT_PROC, mrb->proc_class);
  mrb_proc_copy(mrb, p, MRB_METHOD_PROC(m));
  mrb_proc_set_refscope(mrb, p, arg->scope);
  m.as.proc = p;
  mrb_define_method_raw(mrb, arg->to, mid, m);
  mrb_gc_arena_restore(mrb, ai);
  return 0;
}

/*
 *  call-seq:
 *     refinement.import_methods(*modules)   -> refinement
 *
 *  Copies the methods written in Ruby of each module into the refinement.
 *  A method written in C cannot be imported and raises ArgumentError.
 */
static mrb_value
refinement_import_methods(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  for (mrb_int i = 0; i < argc; i++) {
    if (!mrb_module_p(argv[i]) || refinement_p(mrb, argv[i])) {
      mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %C (expected Module)", mrb_obj_class(mrb, argv[i]));
    }
  }
  mrb_value owner = mrb_iv_get(mrb, self, MRB_SYM(__defined_at__));
  for (mrb_int i = 0; i < argc; i++) {
    struct import_arg arg;
    arg.to = mrb_class_ptr(self);
    arg.from = mrb_class_ptr(argv[i]);
    arg.scope = mrb_ary_ptr(owner_scope(mrb, owner));
    mrb_mt_foreach(mrb, arg.from, import_method, &arg);
  }
  return self;
}

/*
 *  call-seq:
 *     prc.refined(*modules)   -> a_proc
 *
 *  A copy of the proc in which the refinements of the modules are active,
 *  over those the proc already sees; a module given later comes first.
 *  The copy shares the proc's environment.  Blocks and methods written in
 *  its body see the refinements; a `using` written in it raises.  With no
 *  modules the proc itself is returned.
 */
static mrb_value
proc_refined(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  const struct RProc *p = mrb_proc_ptr(self);

  mrb_get_args(mrb, "*", &argv, &argc);
  if (argc == 0) return self;
  if (MRB_PROC_CFUNC_P(p) || MRB_PROC_ALIAS_P(p) || (MRB_PROC_SCOPE_P(p) && MRB_PROC_STRICT_P(p))) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "can't apply refinements to a Proc without a Ruby block");
  }
  for (mrb_int i = 0; i < argc; i++) {
    if (refinement_p(mrb, argv[i])) {
      mrb_raise(mrb, E_TYPE_ERROR, "wrong argument type refinement (expected Module)");
    }
    if (!mrb_module_p(argv[i])) {
      mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %C (expected Module)", mrb_obj_class(mrb, argv[i]));
    }
  }

  struct RArray *cur = mrb_proc_refinements(mrb, p);
  mrb_value scope = cur ? mrb_ary_new_from_values(mrb, ARY_LEN(cur), ARY_PTR(cur)) : mrb_ary_new(mrb);
  for (mrb_int i = 0; i < argc; i++) {
    scope_activate(mrb, scope, mrb_class_ptr(argv[i]));
  }
  mrb_ary_ptr(scope)->flags |= REFSCOPE_OF_PROC_FL;
  mrb_obj_freeze(mrb, scope);

  struct RProc *np = MRB_OBJ_ALLOC(mrb, MRB_TT_PROC, mrb->proc_class);
  mrb_proc_copy(mrb, np, p);
  mrb_proc_set_refscope(mrb, np, mrb_ary_ptr(scope));
  return mrb_obj_value(np);
}

static mrb_value
refinement_no_include(mrb_state *mrb, mrb_value self)
{
  mrb_raisef(mrb, E_TYPE_ERROR, "Refinement#%n has been removed", mrb->c->ci->mid);
  return mrb_nil_value();
}

void
mrb_init_refinement(mrb_state *mrb)
{
  struct RClass *mod = mrb->module_class;
  struct RClass *ref;

  ref = mrb_define_class_id(mrb, MRB_SYM(Refinement), mod);
  mrb->refinement_class = ref;
  MRB_SET_INSTANCE_TT(ref, MRB_TT_MODULE);
  MRB_UNDEF_ALLOCATOR(ref);
  mrb_undef_class_method_id(mrb, ref, MRB_SYM(new));

  mrb_define_private_method_id(mrb, mod, MRB_SYM(refine), mod_refine, MRB_ARGS_REQ(1)|MRB_ARGS_BLOCK());
  mrb_define_private_method_id(mrb, mod, MRB_SYM(using), mod_using, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, mod, MRB_SYM(refinements), mod_refinements, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, mod, MRB_SYM(used_modules), mod_s_used_modules, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, mod, MRB_SYM(used_refinements), mod_s_used_refinements, MRB_ARGS_NONE());

  mrb_define_method_id(mrb, ref, MRB_SYM(target), refinement_target, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, ref, MRB_SYM(refined_class), refinement_target, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, ref, MRB_SYM(import_methods), refinement_import_methods, MRB_ARGS_ANY());
  mrb_define_private_method_id(mrb, ref, MRB_SYM(include), refinement_no_include, MRB_ARGS_ANY());
  mrb_define_private_method_id(mrb, ref, MRB_SYM(prepend), refinement_no_include, MRB_ARGS_ANY());
  mrb_undef_method_id(mrb, ref, MRB_SYM(refine));

  mrb_define_singleton_method_id(mrb, mrb->top_self, MRB_SYM(using), main_using, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, mrb->proc_class, MRB_SYM(refined), proc_refined, MRB_ARGS_ANY());
}

#else  /* MRB_USE_REFINEMENTS */

typedef int mrb_refinement_c_no_empty_translation_unit;

#endif
