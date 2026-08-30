/*
** vm.c - virtual machine for mruby
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/hash.h>
#include <mruby/irep.h>
#include <mruby/numeric.h>
#include <mruby/proc.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/error.h>
#include <mruby/opcode.h>
#include "value_array.h"
#include <mruby/throw.h>
#include <mruby/dump.h>
#include <mruby/internal.h>

#ifdef MRB_NO_STDIO
#if defined(__cplusplus)
extern "C" {
#endif
void abort(void);
#if defined(__cplusplus)
}  /* extern "C" */
#endif
#endif

#define STACK_INIT_SIZE 128
#define CALLINFO_INIT_SIZE 32

/* Define amount of linear stack growth. */
#ifndef MRB_STACK_GROWTH
#define MRB_STACK_GROWTH 128
#endif

/* Maximum recursive depth. Should be set lower on memory constrained systems. */
#ifdef __clang__
#if __has_feature(address_sanitizer) && !defined(__SANITIZE_ADDRESS__)
#define __SANITIZE_ADDRESS__
#endif
#endif

#ifndef MRB_CALL_LEVEL_MAX
#if defined(__SANITIZE_ADDRESS__)
#define MRB_CALL_LEVEL_MAX 128
#else
#define MRB_CALL_LEVEL_MAX 512
#endif
#endif

/* Maximum stack depth. Should be set lower on memory constrained systems.
The value below allows about 60000 recursive calls in the simplest case. */
#ifndef MRB_STACK_MAX
#define MRB_STACK_MAX (0x40000 - MRB_STACK_GROWTH)
#endif

#ifdef VM_DEBUG
# define DEBUG(x) (x)
#else
# define DEBUG(x)
#endif


#ifndef MRB_GC_FIXED_ARENA
/* Unwind the arena to `idx` and give back the memory it no longer needs.
   The caller is the epilogue of a cfunc call, whose return value is still
   only in a local of mrb_vm_exec() at this point, so the realloc below must
   not run once the arena has let go of it: under MRB_GC_STRESS every
   allocation is a full GC, and the value would be swept before the VM stores
   it into a register. The unwind therefore comes after the realloc. */
static void
mrb_gc_arena_shrink(mrb_state *mrb, int idx)
{
  mrb_gc *gc = &mrb->gc;
  int capa = gc->arena_capa;

  if (idx < capa / 4) {
    capa >>= 2;
    if (capa < MRB_GC_ARENA_SIZE) {
      capa = MRB_GC_ARENA_SIZE;
    }
    if (capa != gc->arena_capa) {
      gc->arena = (struct RBasic**)mrb_realloc(mrb, gc->arena, sizeof(struct RBasic*)*capa);
      gc->arena_capa = capa;
    }
  }
  gc->arena_idx = idx;
}
#else
#define mrb_gc_arena_shrink(mrb, idx) mrb_gc_arena_restore(mrb, idx)
#endif

#define CALL_MAXARGS 15
#define CALL_VARARGS (CALL_MAXARGS<<4 | CALL_MAXARGS)

static inline void
stack_clear(mrb_value *from, size_t count)
{
  while (count-- > 0) {
    SET_NIL_VALUE(*from);
    from++;
  }
}

static inline void
stack_copy(mrb_value *dst, const mrb_value *src, size_t size)
{
  if (!src) return;
  memcpy(dst, src, sizeof(mrb_value)*size);
}

static void
stack_init(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;

  /* mrb_assert(mrb->stack == NULL); */
  c->stbase = (mrb_value*)mrb_malloc(mrb, STACK_INIT_SIZE * sizeof(mrb_value));
  c->stend = c->stbase + STACK_INIT_SIZE;
  stack_clear(c->stbase, STACK_INIT_SIZE);

  /* mrb_assert(ci == NULL); */
  static const mrb_callinfo ci_zero = { 0 };
  c->cibase = (mrb_callinfo*)mrb_malloc(mrb, CALLINFO_INIT_SIZE * sizeof(mrb_callinfo));
  c->ciend = c->cibase + CALLINFO_INIT_SIZE;
  c->cibase[0] = ci_zero;
  c->ci = c->cibase;
  c->ci->u.target_class = mrb->object_class;
  c->ci->stack = c->stbase;
  c->ci->vis = 1;                     /* private (2-bit packed) */
}

static inline void
envadjust(mrb_state *mrb, mrb_value *oldbase, mrb_value *newbase)
{
  mrb_callinfo *ci = mrb->c->cibase;

  if (newbase == oldbase) return;

  while (ci <= mrb->c->ci) {
    struct REnv *e = mrb_vm_ci_env(ci);

    /*
     * Byte-level calculation to avoid truncation when allocator alignment is
     * smaller than sizeof(mrb_value).
     * eg: MRB_NO_BOXING + MRB_INT64 with MRB_32BIT => sizeof(mrb_value)=16
     *     And when memory allocator's alignment is 8 bytes
     * Pointer subtraction on mrb_value* would truncate (8/16 -> 0).
     * So, we use char* for pointer calculation to get the correct offset in
     * bytes, then apply that offset to mrb_value* pointers.
     */
    mrb_value *new_stack = (mrb_value *)((char *)newbase + ((char *)ci->stack - (char *)oldbase));

    if (e) {
      mrb_assert(e->cxt == mrb->c && MRB_ENV_ONSTACK_P(e));
      mrb_assert(e->stack == ci->stack);
      e->stack = new_stack;
    }
    ci->stack = new_stack;
    ci++;
  }
}

/** def rec; $deep =+ 1; if $deep > 1000; return 0; end; rec; end **/

/* Run BODY with GC disabled. Used around a stack or callinfo realloc, where the
   old buffer is freed before the base pointers are updated, so an incremental
   GC step triggered from inside the realloc would mark the freed buffer
   (use-after-free). */
static inline void*
mrb_realloc_with_gc_disabled(mrb_state *mrb, void *p, size_t size)
{
  mrb_bool gc_disabled = mrb->gc.disabled;
  mrb->gc.disabled = TRUE;
  p = mrb_realloc_simple(mrb, p, size);
  mrb->gc.disabled = gc_disabled;
  if (!p) mrb_raise_nomemory(mrb);
  return p;
}

static void
stack_extend_alloc(mrb_state *mrb, mrb_int room)
{
  mrb_value *oldbase = mrb->c->stbase;
  size_t oldsize = mrb->c->stend - mrb->c->stbase;
  size_t size = oldsize;
  /* ci->stack can sit past stend when an embedder enters the VM with an
     undersized stack, since cipush places the frame before the stack is
     extended. The base size must cover the frame offset. */
  size_t off = mrb->c->ci->stack ? mrb->c->ci->stack - oldbase : 0;

  if (off > size) size = off;
#ifdef MRB_STACK_EXTEND_DOUBLING
  if ((size_t)room <= size)
    size *= 2;
  else
    size += room;
#else
  /* Use 1.5x stack growth.
     It is slightly slower than doubling the stack space,
     but it saves memory on small devices. */
  {
    size_t newsize = size + (size >> 1); /* 1.5x growth */
    if (newsize < size + MRB_STACK_GROWTH)
      newsize = size + MRB_STACK_GROWTH;
    if (newsize < size + (size_t)room)
      newsize = size + room;
    size = newsize;
  }
#endif

  mrb_value *newstack = (mrb_value*)mrb_realloc_with_gc_disabled(mrb, mrb->c->stbase, sizeof(mrb_value) * size);
  stack_clear(&(newstack[oldsize]), size - oldsize);
  envadjust(mrb, oldbase, newstack);
  mrb->c->stbase = newstack;
  mrb->c->stend = mrb->c->stbase + size;

  /* Raise an exception if the new stack size will be too large,
     to prevent infinite recursion. However, do this only after resizing the stack, so mrb_raise has stack space to work with. */
  if (size > MRB_STACK_MAX) {
    mrb_exc_raise(mrb, mrb_obj_value(mrb->stack_err));
  }
}

static inline void
stack_extend(mrb_state *mrb, mrb_int room)
{
  if (mrb_unlikely(!mrb->c->ci->stack || mrb->c->ci->stack + room >= mrb->c->stend)) {
    stack_extend_alloc(mrb, room);
  }
}

/**
 * @brief Extends the VM stack.
 *
 * This function extends the virtual machine stack to accommodate more values.
 * If the current stack size is insufficient, it reallocates the stack
 * with a larger size.
 *
 * @param mrb The mruby state.
 * @param room The additional number of mrb_value slots required.
 */
MRB_API void
mrb_stack_extend(mrb_state *mrb, mrb_int room)
{
  stack_extend(mrb, room);
}

static void
stack_extend_adjust(mrb_state *mrb, mrb_int room, const mrb_value **argp)
{
  const struct mrb_context *c = mrb->c;
  ptrdiff_t voff = *argp - c->stbase;

  if (voff < 0 || voff >= c->stend - c->stbase) {
    stack_extend(mrb, room);
  }
  else {
    stack_extend(mrb, room);
    *argp = c->stbase + voff;
  }
}

static inline struct REnv*
uvenv(mrb_state *mrb, mrb_int up)
{
  const struct RProc *proc = mrb->c->ci->proc;

  while (up--) {
    proc = proc->upper;
    if (!proc) return NULL;
  }
  struct REnv *e = MRB_PROC_ENV(proc);
  if (e) return e;              /* proc has enclosed env */
  return NULL;
}

static inline const struct RProc*
top_proc(mrb_state *mrb, const struct RProc *proc, const struct REnv **envp)
{
  while (proc->upper) {
    if (MRB_PROC_SCOPE_P(proc) || MRB_PROC_STRICT_P(proc))
      return proc;
    *envp = proc->e.env;
    proc = proc->upper;
  }
  return proc;
}

#define CI_PROC_SET(ci, p) do {\
  ci->proc = p;\
  if (p) {\
    mrb_assert(!MRB_PROC_ALIAS_P(p));\
    ci->pc = (!MRB_PROC_CFUNC_P(p) && p->body.irep) ? p->body.irep->iseq : NULL;\
  }\
  else {\
    ci->pc = NULL;\
  }\
} while (0)

void
mrb_vm_ci_proc_set(mrb_callinfo *ci, const struct RProc *p)
{
  CI_PROC_SET(ci, p);
}

#define MRB_PROC_RESOLVE_ALIAS(ci, p) do {\
  if (MRB_PROC_ALIAS_P(p)) {\
    (ci)->mid = (p)->body.mid;\
    (p) = (p)->upper;\
  }\
} while (0)

#define CI_TARGET_CLASS(ci) (((ci)->u.env && (ci)->u.env->tt == MRB_TT_ENV)? (ci)->u.env->c : (ci)->u.target_class)

struct RClass*
mrb_vm_ci_target_class(const mrb_callinfo *ci)
{
  return CI_TARGET_CLASS(ci);
}

void
mrb_vm_ci_target_class_set(mrb_callinfo *ci, struct RClass *tc)
{
  struct REnv *e = ci->u.env;
  if (e && e->tt == MRB_TT_ENV) {
    e->c = tc;
  }
  else {
    ci->u.target_class = tc;
  }
}

#define CI_ENV(ci) (((ci)->u.env && (ci)->u.env->tt == MRB_TT_ENV)? (ci)->u.env : NULL)

struct REnv*
mrb_vm_ci_env(const mrb_callinfo *ci)
{
  return CI_ENV(ci);
}

static inline void
ci_env_set(mrb_callinfo *ci, struct REnv *e)
{
  if (ci->u.env) {
    if (ci->u.env->tt == MRB_TT_ENV) {
      if (e) {
        e->c = ci->u.env->c;
        ci->u.env = e;
      }
      else {
        ci->u.target_class = ci->u.env->c;
      }
    }
    else if (e) {
      e->c = ci->u.target_class;
      ci->u.env = e;
    }
  }
  else {
    ci->u.env = e;
  }
}

void
mrb_vm_ci_env_set(mrb_callinfo *ci, struct REnv *e)
{
  ci_env_set(ci, e);
}

MRB_API void
mrb_vm_ci_env_clear(mrb_state *mrb, mrb_callinfo *ci)
{
  struct REnv *e = ci->u.env;
  if (e && e->tt == MRB_TT_ENV) {
    ci->u.target_class = e->c;
    /* The escaping env carries the container, so a proc from the earlier
       top-level chunk keeps reading the scope it was written in. */
    mrb_env_detach(mrb, e, mrb_ci_svar(mrb->c, ci), FALSE);
  }
  /* The frame itself starts the next chunk in a fresh scope, the way each
     file `ruby -r` loads gets one of its own; a caller that wants a single
     scope across its chunks (mirb's interactive lines) does not call this,
     which is also how it keeps the top-level locals. */
  mrb_ci_svar_set(mrb, mrb->c, ci, NULL);
}

/* The local scope of a block or lambda: following p->upper toward the
 * scope proc, MRB_PROC_ENV() of each step is the env of the frame the
 * proc was defined in, instance by instance, so the env in hand when the
 * next proc up turns out to be a scope (or is missing) belongs to the
 * method scope, the way CRuby's lep walk crosses block eps into the
 * method's. NULL for a proc that captured no env. Pointer identity only:
 * the env may be closed, or live on another context's stack. */
static struct REnv*
svar_scope_env(const struct RProc *p)
{
  struct REnv *e = MRB_PROC_ENV(p);

  if (!e) return NULL;
  for (;;) {
    const struct RProc *up = p->upper;
    if (!up || MRB_PROC_CFUNC_P(up) || MRB_PROC_SCOPE_P(up)) return e;
    struct REnv *upenv = MRB_PROC_ENV(up);
    if (!upenv) return e;
    p = up;
    e = upenv;
  }
}

/* A frame with no scope of its own: an irep proc that neither is a scope
 * nor captured one. What reaches here is the top proc of a nested
 * `mrb_top_run()`, which is `mrb_load_string()` called from a C function
 * mid-execution, and an eval compiled against no Ruby scope, which is the
 * same call with no Ruby frame under it. An ordinary `eval` is not one of
 * these: its proc captures the calling frame's env, so it answers FALSE
 * and resolves as a block does, onto the scope that called it. Holding no
 * slot, a scopeless frame is as transparent to the special variables as
 * the C function that pushed it, the way CRuby's `rb_eval_string()` shares
 * the scope below; when its env escapes, the transparency outlives the
 * frame by adoption (svar_env_adopt_owner()). Blocks and lambdas always
 * capture, so they never answer TRUE. */
static mrb_bool
svar_scopeless_frame_p(const mrb_callinfo *ci)
{
  const struct RProc *p = ci->proc;

  return p && !MRB_PROC_CFUNC_P(p) &&
         !MRB_PROC_SCOPE_P(p) && svar_scope_env(p) == NULL;
}

/* An escaped scope's slot (MRB_ENV_SVAR_SLOT in internal.h):
 * mrb_env_detach() sizes the closing allocation for it and fills it,
 * through env_unshare_with_svar(), only when it actually has a container
 * or forward to install; mrb_env_unshare() on its own never asks for the
 * extra value. NULL where the env carries none, which only the flag can
 * say: a closed env whose stack stops at its locals is the ordinary result
 * of mrb_env_unshare() itself, what out-of-tree code builds by hand, and
 * what error.c's fault-time rewind and the out-of-memory arm of
 * mrb_env_unshare() leave behind, all three with no room past the locals.
 * Reads take it for a scope holding no container; the one later write that
 * cannot, a live scope's first, grows the already-closed env into the slot
 * instead (svar_slot_ensure()).
 *
 * An env still on the stack carries no slot either, its locals being the
 * VM's own stack. That test comes first because it is also what a swept
 * env looks like: mrb_env_unshare() answers TRUE without touching an env
 * the collection inside its allocation freed, and a freed cell's flags say
 * nothing, so the callers that close an env and then read its slot,
 * cipop() and mrb_env_detach(), still find such an env on the stack. */
static mrb_value*
env_svar_slot(struct REnv *e)
{
  if (MRB_ENV_ONSTACK_P(e) || !MRB_ENV_SVAR_P(e)) return NULL;
  mrb_assert(e->stack != NULL);
  return &MRB_ENV_SVAR_SLOT(e->stack, MRB_ENV_LEN(e));
}

/* The same slot, made if the env has none yet: a closed env whose stack
 * holds exactly its locals grows by one and starts carrying the flag. Only
 * the first write into such a scope comes here (svar_owner_force()); reads
 * answer nil off the missing slot and leave the env alone, so an env that
 * never owns special variables never pays the extra mrb_value.
 * The env is reachable from the roots the caller resolved it through, so a
 * collection inside the resize cannot sweep it and free the stack under
 * the realloc. Out of memory raises, as the container allocation just
 * above it does, leaving the env exactly as it was. */
static mrb_value*
svar_slot_ensure(mrb_state *mrb, struct REnv *e)
{
  mrb_value *slot = env_svar_slot(e);
  if (slot) return slot;

  mrb_assert(!MRB_ENV_ONSTACK_P(e));
  size_t len = (size_t)MRB_ENV_LEN(e);
  mrb_value *stack = (mrb_value*)mrb_realloc(mrb, e->stack, MRB_ENV_SVAR_STACK_SIZE(len));

  e->stack = stack;
  SET_NIL_VALUE(MRB_ENV_SVAR_SLOT(stack, len));
  MRB_ENV_SET_SVAR(e);
  return &MRB_ENV_SVAR_SLOT(stack, len);
}

/* The owner of the frame-scoped special variables, resolved the way CRuby
 * resolves its svar: walking down from the top, a C frame has no slot of
 * its own, a scope frame owns its own slot, a frame with no
 * scope of its own is as transparent as a C frame (see
 * `svar_scopeless_frame_p()`), and a block or lambda frame resolves to the
 * scope it was defined in, wherever that scope now is: a live frame on
 * this or on another context's stack, or the env the scope left behind. A
 * block written inside a scopeless frame resolves to that frame, which
 * owns no slot either, so the walk goes on below it.
 * One exception, CRuby's root-lep redirect: a resolution landing on the
 * very scope the running context's own root block was defined in is handed
 * the context's root slot instead (`cibase`), which is how a fiber keeps
 * its special variables to itself. Resolution ends in a live frame (*cip,
 * its context in *ocp), in the env of a scope that escaped the stack
 * (*envp, holding the container mrb_env_detach() moved off it, or the
 * owner's a scopeless frame adopted), or in neither, when the scope left
 * nothing behind: then a read answers nil and a write is dropped. A
 * scopeless frame's escaped env may instead hold the env of the scope
 * below it (what mrb_svar_frame_container() hands on where that scope
 * holds no container to share): the walk follows it, one hop per nested
 * load, the way CRuby's ep chain crosses an eval frame's ep into the scope
 * below's.
 * What resolution costs, and why it is linear: an env does not record
 * which frame it belongs to, so a defining scope still on a stack is
 * found by the downward scan below, one CI_ENV test per ci entry between
 * the reader and the scope (eight instructions each, measured), and the
 * lexical chase of svar_scope_env() adds a step per block-nesting level.
 * CRuby resolves in constant time at any call depth, its svar living on
 * the ep itself; the slot could live that way here only if the compiler
 * reserved it in every method's locals, which no compiled irep has, so
 * the scan is what keeps existing bytecode running. The term stretches
 * past a handful of entries only when a proc is carried to the bottom of
 * a deep call chain and reads its special variables there.
 * The walk starts at `top`, which is c->ci for a read or write and a
 * mid-stack frame for mrb_svar_frame_container() resolving a torn-down
 * context. */
static void
svar_owner_from(struct mrb_context *c, mrb_callinfo *top, mrb_callinfo **cip, struct mrb_context **ocp, struct REnv **envp)
{
  struct mrb_context *wc = c;   /* the context being walked; the redirect
                                   below stays the caller's */
  mrb_callinfo *ci = top;

  *cip = NULL;
  *ocp = c;
  *envp = NULL;
  while (ci > wc->cibase) {
    const struct RProc *p = ci->proc;

    if (p && !MRB_PROC_CFUNC_P(p)) {
      if (MRB_PROC_SCOPE_P(p)) {
        *cip = ci;
        *ocp = wc;
        return;
      }

      struct REnv *e = svar_scope_env(p);
      if (!e) {
        /* A scopeless frame: reads and writes pass through to the scope
           below, the way `rb_eval_string()`'s do. */
        ci--;
        continue;
      }
      for (;;) {
        const struct RProc *rp = c->cibase->proc;
        if (rp && !MRB_PROC_CFUNC_P(rp) && !MRB_PROC_SCOPE_P(rp) &&
            svar_scope_env(rp) == e) {
          *cip = c->cibase;
          *ocp = c;
          return;
        }
        if (MRB_ENV_ONSTACK_P(e)) break;
        if (!e->stack) return;
        mrb_value *slot = env_svar_slot(e);
        if (!slot) {
          /* A closed env whose stack was sized without the slot (see
             internal.h): it can hold no forward, so it is the owner
             itself, with no container yet. A read answers nil off it and
             the first write grows it into one (svar_slot_ensure()). */
          *envp = e;
          return;
        }
        mrb_value fwd = *slot;
        if (mrb_type(fwd) != MRB_TT_ENV) {
          *envp = e;
          return;
        }
        /* A scopeless frame's env, forwarded on teardown to the env of
           the scope below the load: follow it there. */
        e = (struct REnv*)mrb_obj_ptr(fwd);
      }
      struct mrb_context *oc = e->cxt;
      mrb_callinfo *s = (oc == wc) ? ci - 1 : oc->ci;
      /* Stop AT cibase rather than decrementing past it: forming a pointer
         one element before the start of an array is undefined behavior. */
      while (CI_ENV(s) != e) {
        if (s == oc->cibase) return;
        s--;
      }
      if (s > oc->cibase && svar_scopeless_frame_p(s)) {
        /* The scope the block was defined in is itself scopeless: the block
           was written inside a nested load, so its special variables belong
           to the scope that load runs against, further down, on whichever
           context that load's frame still stands. */
        wc = oc;
        ci = s - 1;
        continue;
      }
      *cip = s;
      *ocp = oc;
      return;
    }
    ci--;
  }
  *cip = wc->cibase;
  *ocp = wc;
}

static void
svar_owner(struct mrb_context *c, mrb_callinfo **cip, struct mrb_context **ocp, struct REnv **envp)
{
  svar_owner_from(c, c->ci, cip, ocp, envp);
}

/* A scope's special-variable container (struct RSvar in internal.h), made
 * on the first non-nil write into the scope. The object is allocated
 * first and reaches the GC arena with slots == NULL, so the collector a
 * slot allocation may run sees the window; gc_mark_children() guards on
 * it. A raise out of the slot allocation leaves that empty shell to the
 * GC and no owner ever holds it. */
static struct RSvar*
svar_new(mrb_state *mrb)
{
  struct RSvar *sv = MRB_OBJ_ALLOC(mrb, MRB_TT_SVAR, NULL);
  mrb_value *slots = (mrb_value*)mrb_malloc(mrb, sizeof(mrb_value) * MRB_SVAR_MAX);

  for (int i = 0; i < MRB_SVAR_MAX; i++) {
    SET_NIL_VALUE(slots[i]);
  }
  sv->slots = slots;
  return sv;
}

/* The container of the owner svar_owner() resolved to: on a live frame
 * the frame's pointer, on an escaped scope the object its env slot holds,
 * NULL where the scope has never written one (or left nothing behind).
 * Both places may instead hold a forward to the env of the scope below
 * (mrb_svar_frame_container()), but only on a frame with no scope of its
 * own, and resolution walks past those rather than ending on one, so what
 * a resolved frame holds is a container or nothing. */
static struct RSvar*
svar_owner_container(const struct mrb_context *c, mrb_callinfo *ci, struct REnv *e)
{
  if (ci) {
    struct RBasic *sv = mrb_ci_svar(c, ci);
    mrb_assert(sv == NULL || sv->tt == MRB_TT_SVAR);
    return (struct RSvar*)sv;
  }
  if (e) {
    mrb_value *slot = env_svar_slot(e);
    if (slot && mrb_type(*slot) == MRB_TT_SVAR) return (struct RSvar*)mrb_obj_ptr(*slot);
  }
  return NULL;
}

/* Resolves the current owner and answers its container, making one on the
 * spot when the scope holds none yet: the first non-nil write does this
 * (mrb_vm_svar_set()), and so does a scopeless frame whose env escapes
 * (svar_env_adopt_owner()), so the adopted slot and the scope share one
 * object before either side has written. NULL when resolution found no
 * scope that could hold one.
 * Installing a fresh container into a frame needs no barrier of its own
 * on the running or the root context, those two ci stacks being
 * unbarriered roots that final_marking_phase() re-scans atomically like
 * the value stack; a frame on any other context is marked through that
 * context's fiber, so the fiber pays one; a task context has no fiber and
 * needs none, its stack being re-scanned atomically by
 * mrb_task_mark_all(), which final_marking_phase() calls for exactly
 * that. An env slot is ordinary GC-owned storage and pays the env's own. */
static struct RSvar*
svar_owner_force(mrb_state *mrb)
{
  mrb_callinfo *ci;
  struct mrb_context *oc;
  struct REnv *e;

  svar_owner(mrb->c, &ci, &oc, &e);
  struct RSvar *sv = svar_owner_container(oc, ci, e);
  if (sv) return sv;
  sv = svar_new(mrb);
  /* The allocation may have run a collection, and the owner may have stood
     on a stack nothing else kept alive, a suspended fiber whose last
     reference was dropped: its sweep detached its envs and freed its
     callinfo array, so the frame resolved above is gone. Whether one ran
     cannot be read off mrb->gc.live, which counts objects rather than
     cycles and comes back unchanged from a sweep that freed as many as
     this allocation makes, so resolve again either way; the walk then ends
     in the closed env the sweep left behind, holding whatever container
     the detach carried. */
  svar_owner(mrb->c, &ci, &oc, &e);
  struct RSvar *moved = svar_owner_container(oc, ci, e);
  if (moved) return moved;
  if (ci) {
    mrb_ci_svar_set(mrb, oc, ci, (struct RBasic*)sv);
    if (oc != mrb->c && oc != mrb->root_c && oc->fib) {
      mrb_write_barrier(mrb, (struct RBasic*)oc->fib);
    }
    return sv;
  }
  if (e) {
    /* Grows a closed env sized without the slot into one (see internal.h);
       the resize may raise out of memory, leaving the fresh container to
       the GC the way a raise out of svar_new() does. */
    mrb_value *slot = svar_slot_ensure(mrb, e);
    *slot = mrb_obj_value(sv);
    mrb_write_barrier(mrb, (struct RBasic*)e);
    return sv;
  }
  return NULL;
}

/* What a frame's env carries into escape when its context is torn down
 * around it or marked through it: the frame's own container, or for a
 * scopeless frame the one the owner it resolved to while it ran already
 * holds. Where that owner holds none, the answer is the owner's env
 * itself, the forward svar_owner_from() follows one hop further, so that
 * the transparency of a scopeless frame survives its context by the same
 * representation an escaped scopeless env uses: neither caller may make a
 * container here, gc_mark_children() (gc.c) running mid-collection and
 * mrb_env_detach_all() mid-teardown, where cipop() materializes one
 * instead (svar_env_adopt_owner()). NULL where there is nothing to carry,
 * and for an owner that is the context's own root frame, whose container
 * dies with the context the way the root frame's own does. */
struct RBasic*
mrb_svar_frame_container(struct mrb_context *c, mrb_callinfo *ci)
{
  struct RBasic *own = mrb_ci_svar(c, ci);
  if (own || !svar_scopeless_frame_p(ci)) return own;

  mrb_callinfo *oci;
  struct mrb_context *oc;
  struct REnv *oe;
  svar_owner_from(c, ci, &oci, &oc, &oe);
  if (oc == c && oci == c->cibase) return NULL;
  struct RSvar *sv = svar_owner_container(oc, oci, oe);
  if (sv) return (struct RBasic*)sv;
  struct REnv *fwd = oci ? CI_ENV(oci) : oe;
  /* Nothing to forward to when the owner scope left no env behind: no proc
     can be holding it, so nothing can look. A frame forwarding to its own
     env would close the walk into a loop rather than a hop. */
  if (fwd == NULL || fwd == CI_ENV(ci)) return NULL;
  return (struct RBasic*)fwd;
}

/*
 * Reads one special-variable slot of the scope owning it, on a live frame
 * or in the env of a scope that returned, nil where that scope holds no
 * container.
 */
mrb_value
mrb_vm_svar_get(mrb_state *mrb, enum mrb_svar_index key)
{
  mrb_callinfo *ci;
  struct mrb_context *oc;
  struct REnv *e;

  mrb_assert(key >= 0 && key < MRB_SVAR_MAX);
  svar_owner(mrb->c, &ci, &oc, &e);
  struct RSvar *sv = svar_owner_container(oc, ci, e);
  if (!sv) return mrb_nil_value();
  return sv->slots[key];
}

/*
 * Writes one special-variable slot of the owning scope. The slot holds any
 * mrb_value; any richer contract, like the TypeError mruby-regexp's
 * virtual-global setter raises for `$~ = <not a MatchData>`, belongs to
 * the caller. The scope's container is made on the first non-nil write; a
 * nil write into a scope that has none is dropped, nil being what a
 * missing slot already reads as, so `def foo; 1 + 2; end` never allocates
 * one, and an immediate write pays no barrier, making no edge a black
 * container would have to re-scan for. A heap-value write pays the
 * backward barrier: a black container is
 * re-grayed so the final marking re-reads its slots, and until then the
 * barrier is a no-op. The forward barrier (marking the value on the spot)
 * would be wrong here, not just slower: a publish-heavy loop hands the
 * slot a fresh object every pass, and marking each one resurrects garbage
 * into the next cycle wholesale, where a re-grayed container marks only
 * what it still holds when the cycle closes.
 */
void
mrb_vm_svar_set(mrb_state *mrb, enum mrb_svar_index key, mrb_value v)
{
  struct RSvar *sv;

  mrb_assert(key >= 0 && key < MRB_SVAR_MAX);
  if (mrb_nil_p(v)) {
    mrb_callinfo *ci;
    struct mrb_context *oc;
    struct REnv *e;
    svar_owner(mrb->c, &ci, &oc, &e);
    sv = svar_owner_container(oc, ci, e);
    if (!sv) return;
  }
  else {
    sv = svar_owner_force(mrb);
    if (!sv) return;
  }
  sv->slots[key] = v;
  if (!mrb_immediate_p(v)) {
    mrb_write_barrier(mrb, (struct RBasic*)sv);
  }
}

#define CINFO_NONE    0 // called method from mruby VM (without C functions)
#define CINFO_SKIP    1 // ignited mruby VM from C
#define CINFO_DIRECT  2 // called method from C
#define CINFO_RESUMED 3 // resumed by `Fiber.yield` (probably the main call is `mrb_fiber_resume()`)

#define BLK_PTR(b) ((mrb_proc_p(b)) ? mrb_proc_ptr(b) : NULL)

/* See mrb_ci_svar() in internal.h for why these live beside the frames. */
void
mrb_ci_svar_set(mrb_state *mrb, struct mrb_context *c, mrb_callinfo *ci, struct RBasic *sv)
{
  if (!c->svars) {
    if (!sv) return;
    mrb->svar_used = TRUE;
    c->svars = (struct RBasic**)mrb_calloc(mrb, (size_t)(c->ciend - c->cibase),
                                           sizeof(struct RBasic*));
  }
  c->svars[ci - c->cibase] = sv;
}

/* See internal.h.  Nothing to do before the first write anywhere in the
   state, which is what keeps a program that never touches one from paying
   for the frames it never fills. */
void
mrb_svars_reserve(mrb_state *mrb, struct mrb_context *c)
{
  if (!mrb->svar_used || c->svars) return;
  c->svars = (struct RBasic**)mrb_calloc(mrb, (size_t)(c->ciend - c->cibase),
                                         sizeof(struct RBasic*));
}

static inline mrb_callinfo*
cipush(mrb_state *mrb, mrb_int push_stacks, uint8_t cci, struct RClass *target_class,
       const struct RProc *proc, struct RProc *blk, mrb_sym mid, uint16_t argc)
{
  struct mrb_context *c = mrb->c;
  mrb_callinfo *ci = c->ci + 1;

  if (ci < c->ciend) {
    c->ci = ci;
  }
  else {
    ptrdiff_t size = ci - c->cibase;

    if (size >= MRB_CALL_LEVEL_MAX) {
      mrb_exc_raise(mrb, mrb_obj_value(mrb->stack_err));
    }
    c->cibase = (mrb_callinfo*)mrb_realloc_with_gc_disabled(mrb, c->cibase, sizeof(mrb_callinfo)*size*2);
    c->ci = ci = c->cibase + size;
    c->ciend = c->cibase + size * 2;
    if (c->svars) {
      /* The frames moved and there are twice as many; the entries follow,
         with the half that is new zeroed. */
      c->svars = (struct RBasic**)mrb_realloc_with_gc_disabled(mrb, c->svars, sizeof(struct RBasic*)*size*2);
      memset(c->svars + size, 0, sizeof(struct RBasic*)*size);
    }
  }
  ci->mid = mid;
  CI_PROC_SET(ci, proc);
  ci->blk = blk;
  ci->stack = ci[-1].stack + push_stacks;
  ci->n = argc & 0xf;
  ci->nk = (argc>>4) & 0xf;
  ci->cci = cci;
  ci->vis = MRB_METHOD_PUBLIC_FL;
  if (c->svars) c->svars[ci - c->cibase] = NULL;
  ci->u.target_class = target_class;

  return ci;
}

static void
fiber_terminate(mrb_state *mrb, struct mrb_context *c, mrb_callinfo *ci)
{
  mrb_assert(c != mrb->root_c);

  struct REnv *env = CI_ENV(ci);
  mrb_assert(env == NULL || MRB_ENV_LEN(env) <= c->stend - ci->stack);

  c->status = MRB_FIBER_TERMINATED;
  mrb_free(mrb, c->svars);
  c->svars = NULL;
  mrb_free(mrb, c->cibase);
  c->cibase = c->ciend = c->ci = NULL;
  mrb_value *stack = c->stbase;
  c->stbase = c->stend = NULL;

  if (!env) {
    mrb_free(mrb, stack);
  }
  else {
    size_t len = (size_t)MRB_ENV_LEN(env);
    if (len == 0) {
      env->stack = NULL;
      MRB_ENV_CLOSE(env);
      MRB_ENV_CLEAR_SVAR(env);
      mrb_free(mrb, stack);
    }
    else {
      mrb_assert(stack == env->stack);
      mrb_write_barrier(mrb, (struct RBasic*)env);

      // don't call MRB_ENV_CLOSE() before mrb_realloc().
      // the reason is that env->stack may be freed by mrb_realloc() if MRB_DEBUG + MRB_GC_STRESS are enabled.
      // realloc() on a freed heap will cause double-free.

      stack = (mrb_value*)mrb_realloc(mrb, stack, sizeof(mrb_value)*len);
      if (mrb_object_dead_p(mrb, (struct RBasic*)env)) {
        mrb_free(mrb, stack);
      }
      else {
        /* No slot: the fiber's root container (CRuby's ec->root_svar) dies
           here rather than being carried, and a later write from a
           surviving closure grows one fresh (svar_slot_ensure()). */
        env->stack = stack;
        MRB_ENV_CLOSE(env);
        MRB_ENV_CLEAR_SVAR(env);
      }
    }
  }

  /* fiber termination should automatic yield or transfer to root */
  mrb->c = c->prev;
  if (!mrb->c) mrb->c = mrb->root_c;
  else c->prev = NULL;
  mrb->c->status = MRB_FIBER_RUNNING;
}

mrb_bool
mrb_env_unshare(mrb_state *mrb, struct REnv *e, mrb_bool noraise)
{
  mrb_assert(e != NULL);
  mrb_assert(MRB_ENV_ONSTACK_P(e));

  size_t len = (size_t)MRB_ENV_LEN(e);
  if (len == 0) {
    e->stack = NULL;
    MRB_ENV_CLOSE(e);
    MRB_ENV_CLEAR_SVAR(e);
    return TRUE;
  }

  size_t live = mrb->gc.live;
  mrb_value *p = (mrb_value*)mrb_malloc_simple(mrb, sizeof(mrb_value)*len);
  if (live != mrb->gc.live && mrb_object_dead_p(mrb, (struct RBasic*)e)) {
    // The e object is now subject to GC inside mrb_malloc_simple().
    // Moreover, if NULL is returned due to mrb_malloc_simple() failure, simply ignore it.
    mrb_free(mrb, p);
    return TRUE;
  }
  else if (p) {
    stack_copy(p, e->stack, len);
    e->stack = p;
    MRB_ENV_CLOSE(e);
    MRB_ENV_CLEAR_SVAR(e);
    mrb_write_barrier(mrb, (struct RBasic*)e);
    return TRUE;
  }
  else {
    e->stack = NULL;
    MRB_ENV_CLOSE(e);
    MRB_ENV_CLEAR_SVAR(e);
    MRB_ENV_SET_LEN(e, 0);
    MRB_ENV_SET_BIDX(e, 0);
    if (!noraise) {
      mrb_exc_raise(mrb, mrb_obj_value(mrb->nomem_err));
    }
    return FALSE;
  }
}

/* mrb_env_unshare(), sized and filled for the special-variable slot up
 * front instead of leaving mrb_env_detach() to grow the closed env by a
 * second, separate allocation (svar_slot_ensure()) once the first has
 * already succeeded. That second allocation used to run outside every
 * noraise contract, raising NoMemoryError straight out of the closing
 * detach: a caller relying on FALSE to decrement its own callinfo first
 * (cipop(), for the #3087 invariant) never got the chance. Folding the
 * slot into the one allocation removes the intermediate state that bug
 * needed, a scope whose locals had already detached while its slot's own
 * growth could still fail on its own: here, either both close in the same
 * allocation or neither does.
 * Structured exactly like mrb_env_unshare() above, with a slot's width
 * added to the size throughout and no len == 0 fast path: a slot is owed
 * here even where there are no locals to copy. Called only where sv is
 * known non-NULL; mrb_env_detach() takes mrb_env_unshare()'s plain path
 * itself where there is no container to carry. */
static mrb_bool
env_unshare_with_svar(mrb_state *mrb, struct REnv *e, struct RBasic *sv, mrb_bool noraise)
{
  mrb_assert(e != NULL);
  mrb_assert(MRB_ENV_ONSTACK_P(e));
  mrb_assert(sv != NULL);

  size_t len = (size_t)MRB_ENV_LEN(e);
  size_t live = mrb->gc.live;
  mrb_value *p = (mrb_value*)mrb_malloc_simple(mrb, MRB_ENV_SVAR_STACK_SIZE(len));
  if (live != mrb->gc.live && mrb_object_dead_p(mrb, (struct RBasic*)e)) {
    // See mrb_env_unshare(): e is already gone, so an allocation failure
    // here needs no handling either.
    mrb_free(mrb, p);
    return TRUE;
  }
  else if (p) {
    stack_copy(p, e->stack, len);
    e->stack = p;
    MRB_ENV_CLOSE(e);
    MRB_ENV_SET_SVAR(e);
    MRB_ENV_SVAR_SLOT(p, len) = mrb_obj_value(sv);
    mrb_write_barrier(mrb, (struct RBasic*)e);
    return TRUE;
  }
  else {
    e->stack = NULL;
    MRB_ENV_CLOSE(e);
    MRB_ENV_CLEAR_SVAR(e);
    MRB_ENV_SET_LEN(e, 0);
    MRB_ENV_SET_BIDX(e, 0);
    if (!noraise) {
      mrb_exc_raise(mrb, mrb_obj_value(mrb->nomem_err));
    }
    return FALSE;
  }
}

/* Detaches a live env from the frame that owns it: mrb_env_unshare() moves
 * the locals into a heap copy, and the frame's special-variable container
 * follows them into the slot past the locals, so a proc that outlives the
 * scope still reads what the scope could see (CRuby's svar lives on the
 * lep and survives its escape the same way). This is the one invariant of
 * every path that closes an env over a live frame: cipop() on an ordinary
 * return, the GC freeing a suspended fiber (gc.c), mruby-task tearing a
 * context down, and mrb_vm_ci_env_clear() below. Callers on a frame whose
 * container must die with its context, a fiber or task root, pass sv as
 * NULL; a scopeless frame owns none to pass, and its callers hand the
 * slot what the scope below has instead (svar_env_adopt_owner() on a
 * return, mrb_svar_frame_container() on a teardown), which is that
 * scope's container or, where it holds none, its env as a forward.
 * The two arms close the env in exactly one allocation either way (see
 * env_unshare_with_svar()), so noraise's FALSE always comes back with the
 * env's own state settled and c->ci still pointing at the frame being
 * torn down, never mid-close. */
mrb_bool
mrb_env_detach(mrb_state *mrb, struct REnv *e, struct RBasic *sv, mrb_bool noraise)
{
  if (!sv) return mrb_env_unshare(mrb, e, noraise);
  return env_unshare_with_svar(mrb, e, sv, noraise);
}

/* A scopeless frame is transparent while it runs: svar_owner() walks past
 * it to the scope below. When its env escapes, that relation would die
 * with the frame, the frame's own container being the NULL it never
 * needed, so the freshly closed env adopts the owner's container instead,
 * made on the spot when the owner holds none yet: a proc the load left
 * behind keeps reading and writing the scope below, and a match made on
 * either side after the return is seen on the other, the way CRuby's ep
 * chain crosses an eval frame after its escape. Runs after cipop()
 * decrements: resolution then starts at the scope below, and an
 * allocation failure raises out of a consistent stack, leaving the slot
 * empty. The env may already be garbage, nothing but an unreachable proc
 * holding it, and the allocation the owner may need can collect it, so
 * liveness is re-checked before the write. */
static void
svar_env_adopt_owner(mrb_state *mrb, struct REnv *e)
{
  /* MRB_ENV_ONSTACK_P(e) here is the same collected-out-from-under-unshare
     case mrb_env_detach() skips: no closed env, nothing to grow. */
  if (MRB_ENV_ONSTACK_P(e)) return;
  struct RSvar *sv = svar_owner_force(mrb);
  if (!sv) return;
  /* Whether the container allocation ran a collection cannot be read off
     mrb->gc.live (see svar_owner_force()), so re-check either way. The
     collection may even have recycled the garbage env's own slot as the
     new container: mrb_object_dead_p() would then inspect the live RSvar
     and answer alive, so test the type tag first. Nothing else is
     allocated in the window, so a reused slot can hold nothing but it. */
  struct RBasic *b = (struct RBasic*)e;
  if (b->tt != MRB_TT_ENV || mrb_object_dead_p(mrb, b)) return;
  /* Grows a closed env sized without the slot into one (see internal.h).
     No allocation has run since the dead-check just above passed, so the
     resize's own allocation still finds the object that check vouched
     for, the same one-allocation tolerance svar_owner_force()'s own
     grow relies on, immediately after its own fresh resolution. */
  mrb_value *slot = svar_slot_ensure(mrb, e);
  *slot = mrb_obj_value(sv);
  mrb_write_barrier(mrb, (struct RBasic*)e);
}

/* Detaches every live on-stack env of a context being torn down around it:
 * the GC freeing a suspended fiber (gc.c) and mruby-task ending a task both
 * come through here, so the carrying policy is stated once: each frame's
 * env escapes with the frame's own container, the root frame's dies with
 * the context (CRuby's ec->root_svar), and a scopeless frame hands on the
 * scope below's: its container where the owner holds one, else the owner's
 * own env, forwarded into the slot for the walk in svar_owner_from() to
 * follow, so two procs that shared one scope on the stack still share it
 * escaped, and the first write through either makes the one container both
 * see. `resolve` distinguishes the callers: mruby-task tears down outside a
 * collection and resolves the owner here, with the GC held off across the
 * loop because the resolution chases procs and envs of a stack nothing
 * marks any more, which a detach allocation could otherwise sweep or
 * recycle mid-loop, and because a container whose only holder is this
 * dying stack must survive to the detach that roots it in the escaping
 * env. The gc.c caller runs inside the sweep, where the walk could chase
 * objects already swept and recycled and the GC cannot be held off, so it
 * passes FALSE and relies on the frame's own entry alone, where
 * gc_mark_children() stashed the same answer at mark time, on memory the
 * sweep had not yet touched. */
void
mrb_env_detach_all(mrb_state *mrb, struct mrb_context *c, mrb_bool resolve)
{
  if (!c->cibase || !c->ci) return;

  mrb_bool gc_was = FALSE;
  if (resolve) {
    gc_was = mrb->gc.disabled;
    mrb->gc.disabled = TRUE;
  }
  /* Stop AT cibase rather than decrementing past it: forming a pointer
   * one element before the start of an array is undefined behavior. */
  for (mrb_callinfo *ci = c->ci; ; ci--) {
    struct REnv *e = ci->u.env;
    /* mrb_env_detach() allocates and can therefore run a GC cycle (with
     * `resolve` the GC is off and the sweep is merely deferred). In
     * teardown paths the context may already be unlinked, so an env that
     * no other object refers to may be swept before the walk reaches it;
     * check liveness first. */
    if (e && !mrb_object_dead_p(mrb, (struct RBasic*)e) &&
        e->tt == MRB_TT_ENV && MRB_ENV_ONSTACK_P(e)) {
      struct RBasic *sv = NULL;
      if (ci != c->cibase) {
        sv = mrb_ci_svar(c, ci);
        if (resolve && !sv) {
          sv = mrb_svar_frame_container(c, ci);
          /* A forward is only worth carrying to an env that survives the
             teardown; the GC is off across the loop, so this answer still
             holds at the detach below. */
          if (sv && sv->tt == MRB_TT_ENV && mrb_object_dead_p(mrb, sv)) sv = NULL;
        }
      }
      mrb_env_detach(mrb, e, sv, TRUE);
    }
    if (ci == c->cibase) break;
  }
  if (resolve) {
    mrb->gc.disabled = gc_was;
  }
}

static inline mrb_callinfo*
cipop(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;
  mrb_callinfo *ci = c->ci;

  /* Fast path: no env and no blk (most common for simple method calls) */
  if (mrb_likely((!ci->u.env || ci->u.env->tt != MRB_TT_ENV) && !ci->blk)) {
    c->ci--;
    return c->ci;
  }

  struct REnv *env = CI_ENV(ci);
  ci_env_set(ci, NULL); // make possible to free env by GC if not needed
  struct RProc *b = ci->blk;
  if (b && !MRB_PROC_STRICT_P(b) && MRB_PROC_ENV(b) == CI_ENV(&ci[-1])) {
    b->flags |= MRB_PROC_ORPHAN;
  }
  if (env) {
    /* The container escapes with the locals (see mrb_env_detach()); a
       frame without an env leaves no proc behind that could look. */
    struct RBasic *sv = mrb_ci_svar(c, ci);
    mrb_bool transparent = (sv == NULL && svar_scopeless_frame_p(ci));
    if (!mrb_env_detach(mrb, env, sv, TRUE)) {
      c->ci--; // exceptions are handled at the method caller; see #3087
      mrb_exc_raise(mrb, mrb_obj_value(mrb->nomem_err));
    }
    if (transparent) {
      c->ci--;
      svar_env_adopt_owner(mrb, env);
      return c->ci;
    }
  }
  c->ci--;
  return c->ci;
}

/**
 * @brief Protects a C function call from mruby exceptions.
 *
 * This function executes a C function (`body`) within a protected environment.
 * If an mruby exception occurs during the execution of `body`, this function
 * catches the exception, sets the `error` flag, and returns the exception object.
 * Otherwise, it returns the result of the `body` function and `error` remains FALSE.
 *
 * This is crucial for calling mruby-related C functions from within C code
 * that needs to handle potential mruby exceptions gracefully.
 *
 * @param mrb The mruby state.
 * @param body A pointer to the C function to be executed.
 *             The function should have the signature: `mrb_value func(mrb_state *mrb, void *userdata)`
 * @param userdata A pointer to arbitrary data that will be passed to the `body` function.
 * @param error A pointer to an mrb_bool that will be set to TRUE if an exception
 *              occurred, and FALSE otherwise. Can be NULL if not needed.
 * @return The value returned by the `body` function if no exception occurred,
 *         or the exception object if an exception occurred.
 */
MRB_API mrb_value
mrb_protect_error(mrb_state *mrb, mrb_protect_error_func *body, void *userdata, mrb_bool *error)
{
  struct mrb_jmpbuf *prev_jmp = mrb->jmp;
  struct mrb_jmpbuf c_jmp;
  mrb_value result;
  int ai = mrb_gc_arena_save(mrb);
  const struct mrb_context *c = mrb->c;
  ptrdiff_t ci_index = c->ci - c->cibase;

  if (error) { *error = FALSE; }

  MRB_TRY(&c_jmp) {
    mrb->jmp = &c_jmp;
    result = body(mrb, userdata);
    mrb->jmp = prev_jmp;
  }
  MRB_CATCH(&c_jmp) {
    mrb->jmp = prev_jmp;
    result = mrb_obj_value(mrb->exc);
    mrb->exc = NULL;
    if (error) { *error = TRUE; }
    if (mrb->c == c) {
      while (c->ci - c->cibase > ci_index) {
        cipop(mrb);
      }
    }
    else {
      // It was probably switched by mrb_fiber_resume().
      // Simply destroy all successive CINFO_DIRECTs once the fiber has been switched.
      c = mrb->c;
      while (c->ci > c->cibase && c->ci->cci == CINFO_DIRECT) {
        cipop(mrb);
      }
    }
  }
  MRB_END_EXC(&c_jmp);

  mrb_gc_arena_restore(mrb, ai);
  mrb_gc_protect(mrb, result);
  return result;
}

void mrb_exc_set(mrb_state *mrb, mrb_value exc);
static mrb_value mrb_run(mrb_state *mrb, const struct RProc* proc, mrb_value self);

#ifndef MRB_FUNCALL_ARGC_MAX
#define MRB_FUNCALL_ARGC_MAX 16
#endif

/**
 * @brief Calls a method on an object.
 *
 * This function invokes a method identified by its name on the `self` object,
 * passing the given arguments.
 *
 * @param mrb The mruby state.
 * @param self The receiver object of the method call.
 * @param name The name of the method to call (C string).
 * @param argc The number of arguments to pass to the method.
 * @param ... The variable arguments to pass to the method.
 *            Each argument must be of type `mrb_value`.
 * @return The result of the method call.
 * @raise E_ARGUMENT_ERROR if `argc` is greater than `MRB_FUNCALL_ARGC_MAX`.
 */
MRB_API mrb_value
mrb_funcall(mrb_state *mrb, mrb_value self, const char *name, mrb_int argc, ...)
{
  mrb_value argv[MRB_FUNCALL_ARGC_MAX];
  mrb_sym mid = mrb_intern_cstr(mrb, name);

  if (argc > MRB_FUNCALL_ARGC_MAX) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Too long arguments. (limit=" MRB_STRINGIZE(MRB_FUNCALL_ARGC_MAX) ")");
  }

  va_list ap;
  va_start(ap, argc);
  for (mrb_int i = 0; i < argc; i++) {
    argv[i] = va_arg(ap, mrb_value);
  }
  va_end(ap);
  return mrb_funcall_argv(mrb, self, mid, argc, argv);
}

/**
 * @brief Calls a method on an object using a method ID.
 *
 * This function invokes a method identified by its symbol ID (`mid`) on
 * the `self` object, passing the given arguments. Using a method ID
 * can be more efficient than using a string name if the method is called
 * frequently, as it avoids repeated string-to-symbol lookups.
 *
 * @param mrb The mruby state.
 * @param self The receiver object of the method call.
 * @param mid The symbol ID of the method to call.
 * @param argc The number of arguments to pass to the method.
 * @param ... The variable arguments to pass to the method.
 *            Each argument must be of type `mrb_value`.
 * @return The result of the method call.
 * @raise E_ARGUMENT_ERROR if `argc` is greater than `MRB_FUNCALL_ARGC_MAX`.
 */
MRB_API mrb_value
mrb_funcall_id(mrb_state *mrb, mrb_value self, mrb_sym mid, mrb_int argc, ...)
{
  mrb_value argv[MRB_FUNCALL_ARGC_MAX];

  if (argc > MRB_FUNCALL_ARGC_MAX) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Too long arguments. (limit=" MRB_STRINGIZE(MRB_FUNCALL_ARGC_MAX) ")");
  }

  va_list ap;
  va_start(ap, argc);
  for (mrb_int i = 0; i < argc; i++) {
    argv[i] = va_arg(ap, mrb_value);
  }
  va_end(ap);
  return mrb_funcall_argv(mrb, self, mid, argc, argv);
}

static mrb_int
mrb_ci_kidx(const mrb_callinfo *ci)
{
  if (ci->nk == 0) return -1;
  return (ci->n == CALL_MAXARGS) ? 2 : ci->n + 1;
}

static inline mrb_int
mrb_bidx(uint8_t n, uint8_t k)
{
  if (n == 15) n = 1;
  if (k == 15) n += 1;
  else n += k*2;
  return n + 1;                 /* self + args + kargs */
}

static inline mrb_int
ci_bidx(mrb_callinfo *ci)
{
  return mrb_bidx(ci->n, ci->nk);
}

mrb_int
mrb_ci_bidx(mrb_callinfo *ci)
{
  return ci_bidx(ci);
}

mrb_int
mrb_ci_nregs(mrb_callinfo *ci)
{
  if (!ci) return 4;
  mrb_int nregs = ci_bidx(ci) + 1; /* self + args + kargs + blk */
  const struct RProc *p = ci->proc;
  if (p && !MRB_PROC_CFUNC_P(p) && p->body.irep && p->body.irep->nregs > nregs) {
    return p->body.irep->nregs;
  }
  return nregs;
}

mrb_value mrb_obj_missing(mrb_state *mrb, mrb_value mod);

static mrb_method_t
prepare_missing(mrb_state *mrb, mrb_callinfo *ci, mrb_value recv, mrb_sym mid, mrb_value blk, mrb_bool super)
{
  mrb_sym missing = MRB_SYM(method_missing);
  mrb_value *argv = &ci->stack[1];
  mrb_value args;
  mrb_method_t m;

  /* pack positional arguments */
  if (ci->n == 15) args = argv[0];
  else args = mrb_ary_new_from_values(mrb, ci->n, argv);

  if (mrb_func_basic_p(mrb, recv, missing, mrb_obj_missing)) {
  method_missing:
    if (super) mrb_no_method_error(mrb, mid, args, "no superclass method '%n' for %T", mid, recv);
    else mrb_method_missing(mrb, mid, recv, args);
    /* not reached */
  }
  if (mid != missing) {
    ci->u.target_class = mrb_class(mrb, recv);
  }
  m = mrb_vm_find_method(mrb, ci->u.target_class, &ci->u.target_class, missing);
  if (MRB_METHOD_UNDEF_P(m)) goto method_missing; /* just in case */
  stack_extend(mrb, 4);

  argv = &ci->stack[1];         /* maybe reallocated */
  if (ci->nk == 0) {
    argv[1] = blk;
  }
  else {
    mrb_assert(ci->nk == 15);
    if (ci->n != CALL_MAXARGS) {
      argv[1] = argv[ci->n];    /* keyword arguments */
    }
    argv[2] = blk;
  }
  argv[0] = args;               /* must be replaced after saving argv[0] as it may be a keyword argument */
  ci->n = CALL_MAXARGS;
  /* ci->nk is already set to zero or CALL_MAXARGS */
  mrb_ary_unshift(mrb, args, mrb_symbol_value(mid));
  ci->mid = missing;
  return m;
}

mrb_value
mrb_args_pack_positional(mrb_state *mrb)
{
  mrb_callinfo *ci = mrb->c->ci;
  int argc = ci->n;
  mrb_value *argv = ci->stack + 1;

  if (argc < CALL_MAXARGS) {
    mrb_value args = mrb_ary_new_from_values(mrb, argc, argv);
    /* self, the array, a keyword hash and a block: a frame that carried
       fewer arguments than that has no room for the slots written below.
       prepare_missing() makes the same room before writing the same ones. */
    stack_extend(mrb, 4);
    argv = ci->stack + 1;       /* maybe reallocated */
    if (ci->nk == 0) {
      mrb_value block = argv[argc];
      argv[1] = block;
    }
    else {
      mrb_assert(ci->nk == CALL_MAXARGS);
      mrb_value keyword = argv[argc];
      mrb_value block = argv[argc + 1];
      argv[1] = keyword;
      argv[2] = block;
    }
    argv[0] = args;
    ci->n = CALL_MAXARGS;
  }

  return *argv;
}

static void
funcall_args_capture(mrb_state *mrb, int stoff, mrb_int argc, const mrb_value *argv, mrb_value block, mrb_callinfo *ci)
{
  if (argc < 0 || argc > INT32_MAX) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "negative or too big argc for funcall (%i)", argc);
  }

  ci->nk = 0;                   /* funcall does not support keyword arguments */
  if (argc < CALL_MAXARGS) {
    mrb_int extends = stoff + argc + 2 /* self + block */;
    stack_extend_adjust(mrb, extends, &argv);

    mrb_value *args = mrb->c->ci->stack + stoff + 1 /* self */;
    stack_copy(args, argv, argc);
    args[argc] = block;
    ci->n = (uint8_t)argc;
  }
  else {
    int extends = stoff + 3 /* self + splat + block */;
    stack_extend_adjust(mrb, extends, &argv);

    mrb_value *args = mrb->c->ci->stack + stoff + 1 /* self */;
    args[0] = mrb_ary_new_from_values(mrb, argc, argv);
    args[1] = block;
    ci->n = CALL_MAXARGS;
  }
}

static inline mrb_value
ensure_block(mrb_state *mrb, mrb_value blk)
{
  if (!mrb_nil_p(blk) && !mrb_proc_p(blk)) {
    blk = mrb_type_convert(mrb, blk, MRB_TT_PROC, MRB_SYM(to_proc));
    /* The stack might have been reallocated during mrb_type_convert(), see #3622 */
  }
  return blk;
}

/**
 * @brief Calls a method on an object with a block.
 *
 * This function invokes a method identified by its symbol ID (`mid`) on
 * the `self` object, passing the given arguments (`argv`) and a block (`blk`).
 *
 * @param mrb The mruby state.
 * @param self The receiver object of the method call.
 * @param mid The symbol ID of the method to call.
 * @param argc The number of arguments in `argv`.
 * @param argv A pointer to an array of `mrb_value` arguments.
 * @param blk The block to pass to the method. If no block is to be passed,
 *            use `mrb_nil_value()`. If `blk` is not nil and not a proc,
 *            it will be converted to a proc using `to_proc`.
 * @return The result of the method call.
 * @raise E_ARGUMENT_ERROR if `argc` is negative or too large.
 * @raise E_STACK_ERROR if the call level exceeds `MRB_CALL_LEVEL_MAX`.
 */
MRB_API mrb_value
mrb_funcall_with_block(mrb_state *mrb, mrb_value self, mrb_sym mid, mrb_int argc, const mrb_value *argv, mrb_value blk)
{
  mrb_value val;
  int ai = mrb_gc_arena_save(mrb);

  if (!mrb->jmp) {
    struct mrb_jmpbuf c_jmp;
    ptrdiff_t nth_ci = mrb->c->ci - mrb->c->cibase;

    MRB_TRY(&c_jmp) {
      mrb->jmp = &c_jmp;
      /* recursive call */
      val = mrb_funcall_with_block(mrb, self, mid, argc, argv, blk);
      mrb->jmp = NULL;
    }
    MRB_CATCH(&c_jmp) { /* error */
      while (nth_ci < (mrb->c->ci - mrb->c->cibase)) {
        cipop(mrb);
      }
      mrb->jmp = 0;
      val = mrb_obj_value(mrb->exc);
    }
    MRB_END_EXC(&c_jmp);
    mrb->jmp = NULL;
  }
  else {
    mrb_method_t m;
    mrb_callinfo *ci = mrb->c->ci;
    mrb_int n = mrb_ci_nregs(ci);

    if (!mrb->c->stbase) {
      stack_init(mrb);
    }
    if (ci - mrb->c->cibase > MRB_CALL_LEVEL_MAX) {
      mrb_exc_raise(mrb, mrb_obj_value(mrb->stack_err));
    }
    blk = ensure_block(mrb, blk);
    ci = cipush(mrb, n, CINFO_DIRECT, NULL, NULL, BLK_PTR(blk), 0, 0);
    funcall_args_capture(mrb, 0, argc, argv, blk, ci);
    ci->u.target_class = mrb_class(mrb, self);
    m = mrb_vm_find_method(mrb, ci->u.target_class, &ci->u.target_class, mid);
    if (MRB_METHOD_UNDEF_P(m)) {
      m = prepare_missing(mrb, ci, self, mid, mrb_nil_value(), FALSE);
    }
    else {
      ci->mid = mid;
    }
    ci->proc = MRB_METHOD_PROC_P(m) ? MRB_METHOD_PROC(m) : NULL;

    if (MRB_METHOD_CFUNC_P(m)) {
      mrb->exc = NULL;
      ci->stack[0] = self;
      val = MRB_METHOD_CFUNC(m)(mrb, self);
      cipop(mrb);
      if (mrb->exc != NULL) {
        mrb_exc_raise(mrb, mrb_obj_value(mrb->exc));
      }
    }
    else {
      /* handle alias */
      MRB_PROC_RESOLVE_ALIAS(ci, ci->proc);
      ci->cci = CINFO_SKIP;
      val = mrb_run(mrb, ci->proc, self);
    }
  }
  mrb_gc_arena_restore(mrb, ai);
  mrb_gc_protect(mrb, val);
  return val;
}

/**
 * @brief Calls a method on an object with an array of arguments.
 *
 * This function is similar to `mrb_funcall_with_block` but takes arguments
 * as a C array (`argv`) and does not take an explicit block argument.
 * If a block is needed, `mrb_funcall_with_block` should be used.
 * This function is essentially a convenience wrapper around
 * `mrb_funcall_with_block` with `mrb_nil_value()` for the block.
 *
 * @param mrb The mruby state.
 * @param self The receiver object of the method call.
 * @param mid The symbol ID of the method to call.
 * @param argc The number of arguments in `argv`.
 * @param argv A pointer to an array of `mrb_value` arguments.
 * @return The result of the method call.
 * @see mrb_funcall_with_block
 */
MRB_API mrb_value
mrb_funcall_argv(mrb_state *mrb, mrb_value self, mrb_sym mid, mrb_int argc, const mrb_value *argv)
{
  return mrb_funcall_with_block(mrb, self, mid, argc, argv, mrb_nil_value());
}

static void
check_argument_count(mrb_state *mrb, const mrb_callinfo *ci, mrb_aspec aspec)
{
  mrb_int argc = ci->n;
  if (mrb_unlikely(argc == CALL_MAXARGS)) {
    argc = RARRAY_LEN(ci->stack[1]);
  }
  /* keyword hash counts as positional if method doesn't accept keywords */
  if (ci->nk > 0 && MRB_ASPEC_KEY(aspec) == 0 && !MRB_ASPEC_KDICT(aspec)) {
    mrb_value kdict = ci->stack[mrb_ci_kidx(ci)];
    if (mrb_hash_p(kdict) && !mrb_hash_empty_p(mrb, kdict)) {
      argc++;
    }
  }
  int min = MRB_ASPEC_REQ(aspec) + MRB_ASPEC_POST(aspec);
  int max = MRB_ASPEC_REST(aspec) ? -1 : min + MRB_ASPEC_OPT(aspec);
  if (mrb_unlikely(argc < min || (max >= 0 && argc > max))) {
    mrb_argnum_error(mrb, argc, min, max);
  }
}

static mrb_value
exec_irep(mrb_state *mrb, mrb_value self, const struct RProc *p)
{
  mrb_callinfo *ci = mrb->c->ci;

  ci->stack[0] = self;
  /* handle alias */
  MRB_PROC_RESOLVE_ALIAS(ci, p);
  CI_PROC_SET(ci, p);
  if (MRB_PROC_CFUNC_P(p)) {
    uint32_t caspec_bits = p->flags & MRB_PROC_CASPEC_MASK;
    if (caspec_bits != 0) {
      check_argument_count(mrb, ci, mrb_proc_decompress_caspec(caspec_bits));
    }
    else if (MRB_PROC_NOARG_P(p) && (ci->n > 0 || ci->nk > 0)) {
      check_argument_count(mrb, ci, 0);
    }
    return MRB_PROC_CFUNC(p)(mrb, self);
  }
  mrb_int nregs = p->body.irep->nregs;
  mrb_int keep = ci_bidx(ci)+1;
  if (nregs < keep) {
    stack_extend(mrb, keep);
  }
  else {
    stack_extend(mrb, nregs);
    stack_clear(ci->stack+keep, nregs-keep);
  }

  cipush(mrb, 0, 0, NULL, NULL, NULL, 0, 0);

  return self;
}

mrb_value
mrb_exec_irep(mrb_state *mrb, mrb_value self, const struct RProc *p)
{
  mrb_callinfo *ci = mrb->c->ci;
  if (ci->cci == CINFO_NONE) {
    return exec_irep(mrb, self, p);
  }
  else {
    mrb_value ret;
    if (MRB_PROC_CFUNC_P(p)) {
      if (MRB_PROC_NOARG_P(p) && (ci->n > 0 || ci->nk > 0)) {
        check_argument_count(mrb, ci, 0);
      }
      ci = cipush(mrb, 0, CINFO_DIRECT, CI_TARGET_CLASS(ci), p, NULL, ci->mid, ci->n|(ci->nk<<4));
      mrb->exc = NULL;
      ret = MRB_PROC_CFUNC(p)(mrb, self);
      cipop(mrb);
    }
    else {
      mrb_int keep = ci_bidx(ci) + 1; /* receiver + block */
      ci = cipush(mrb, 0, CINFO_SKIP, CI_TARGET_CLASS(ci), p, NULL, ci->mid, ci->n|(ci->nk<<4));
      ret = mrb_vm_run(mrb, p, self, keep);
    }
    if (mrb->exc && mrb->jmp) {
      mrb_exc_raise(mrb, mrb_obj_value(mrb->exc));
    }
    return ret;
  }
}

mrb_value
mrb_object_exec(mrb_state *mrb, mrb_value self, struct RClass *target_class)
{
  mrb_callinfo *ci = mrb->c->ci;
  mrb_int bidx = ci_bidx(ci);
  mrb_value blk = ci->stack[bidx];
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }

  mrb_assert(mrb_proc_p(blk));
  mrb_gc_protect(mrb, blk);
  ci->stack[bidx] = mrb_nil_value();
  mrb_vm_ci_target_class_set(ci, target_class);
  return mrb_exec_irep(mrb, self, mrb_proc_ptr(blk));
}

static mrb_noreturn void
vis_error(mrb_state *mrb, mrb_sym mid, mrb_value args, mrb_value recv, mrb_bool priv)
{
  mrb_no_method_error(mrb, mid, args, "%s method '%n' called for %T", (priv ? "private" : "protected"), mid, recv);
}

static mrb_value
send_method(mrb_state *mrb, mrb_value self, mrb_bool pub)
{
  mrb_callinfo *ci = mrb->c->ci;
  int n = ci->n;
  mrb_sym name;

  if (ci->cci > CINFO_NONE) {
  funcall:;
    const mrb_value *argv;
    mrb_int argc;
    mrb_value block;
    mrb_get_args(mrb, "n*&", &name, &argv, &argc, &block);
    return mrb_funcall_with_block(mrb, self, name, argc, argv, block);
  }

  mrb_method_t m;
  mrb_value *regs = mrb->c->ci->stack+1;

  if (n == 0) {
  argnum_error:
    mrb_argnum_error(mrb, 0, 1, -1);
  }
  else if (n == 15) {
    if (RARRAY_LEN(regs[0]) == 0) goto argnum_error;
    name = mrb_obj_to_sym(mrb, RARRAY_PTR(regs[0])[0]);
  }
  else {
    name = mrb_obj_to_sym(mrb, regs[0]);
  }

  struct RClass *c = mrb_class(mrb, self);
  m = mrb_vm_find_method(mrb, c, &c, name);
  if (MRB_METHOD_UNDEF_P(m)) {            /* call method_missing */
    goto funcall;
  }

  if (pub) {
    mrb_bool priv = TRUE;
    if (m.flags & MRB_METHOD_PRIVATE_FL) {
    vis_err:;
      if (n == 15) {
        n = (int)(RARRAY_LEN(regs[0]) - 1);
        regs = RARRAY_PTR(regs[0]);
      }
      vis_error(mrb, name, mrb_ary_new_from_values(mrb, n, regs+1), self, priv);
    }
    else if (m.flags & MRB_METHOD_PROTECTED_FL) {
      /* `public_send` rejects protected methods unconditionally */
      priv = FALSE;
      goto vis_err;
    }
  }

  ci->mid = name;
  ci->u.target_class = c;
  /* remove first symbol from arguments */
  if (n == 15) {     /* variable length arguments */
    regs[0] = mrb_ary_subseq(mrb, regs[0], 1, RARRAY_LEN(regs[0]) - 1);
  }
  else { /* n > 0 */
    for (int i=0; i<n; i++) {
      regs[i] = regs[i+1];
    }
    regs[n] = regs[n+1];        /* copy kdict or block */
    if (ci->nk > 0) {
      regs[n+1] = regs[n+2];    /* copy block */
    }
    ci->n--;
  }

  if (MRB_METHOD_FUNC_P(m)) {
    check_argument_count(mrb, ci, MRB_MT_ASPEC(m.flags));
    return MRB_METHOD_FUNC(m)(mrb, self);
  }
  const struct RProc *p = MRB_METHOD_PROC(m);
  MRB_PROC_RESOLVE_ALIAS(ci, p);
  CI_PROC_SET(ci, p);
  if (MRB_PROC_CFUNC_P(p)) {
    uint32_t caspec_bits = p->flags & MRB_PROC_CASPEC_MASK;
    if (caspec_bits != 0) {
      check_argument_count(mrb, ci, mrb_proc_decompress_caspec(caspec_bits));
    }
    else if (MRB_PROC_NOARG_P(p) && (ci->n > 0 || ci->nk > 0)) {
      check_argument_count(mrb, ci, 0);
    }
    return MRB_PROC_CFUNC(p)(mrb, self);
  }
  return exec_irep(mrb, self, p);
}

/* 15.3.1.3.4  */
/* 15.3.1.3.44 */
/*
 *  call-seq:
 *     obj.send(symbol [, args...])        -> obj
 *     obj.__send__(symbol [, args...])      -> obj
 *
 *  Invokes the method identified by _symbol_, passing it any
 *  arguments specified. You can use `__send__` if the name
 *  `send` clashes with an existing method in _obj_.
 *
 *     class Klass
 *       def hello(*args)
 *         "Hello " + args.join(' ')
 *       end
 *     end
 *     k = Klass.new
 *     k.send :hello, "gentle", "readers"   #=> "Hello gentle readers"
 */
mrb_value
mrb_f_send(mrb_state *mrb, mrb_value self)
{
  return send_method(mrb, self, FALSE);
}

/*
 *  call-seq:
 *     obj.public_send(symbol [, args...])  -> obj
 *
 * Invokes the method identified by symbol, passing it any
 * arguments specified. Unlike send, public_send calls public methods only.
 * When the method is identified by a string, the string is converted to a
 * symbol.
 *
 *  1.public_send(:puts, "hello")  # causes NoMethodError
 */
mrb_value
mrb_f_public_send(mrb_state *mrb, mrb_value self)
{
  return send_method(mrb, self, TRUE);
}

static void
check_block(mrb_state *mrb, mrb_value blk)
{
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }
  if (!mrb_proc_p(blk)) {
    mrb_raise(mrb, E_TYPE_ERROR, "not a block");
  }
}

static mrb_value
eval_under(mrb_state *mrb, mrb_value self, mrb_value blk, struct RClass *c)
{
  check_block(mrb, blk);
  mrb_callinfo *ci = mrb->c->ci;
  if (ci->cci == CINFO_DIRECT) {
    return mrb_yield_with_class(mrb, blk, 1, &self, self, c);
  }
  ci->u.target_class = c;
  const struct RProc *p = mrb_proc_ptr(blk);
  /* just in case irep is NULL; #6065 */
  if (p->body.irep == NULL) return mrb_nil_value();
  CI_PROC_SET(ci, p);
  ci->n = 1;
  ci->nk = 0;
  ci->mid = ci[-1].mid;
  MRB_CI_SET_VISIBILITY_BREAK(ci);
  if (MRB_PROC_CFUNC_P(p)) {
    stack_extend(mrb, 4);
    mrb->c->ci->stack[0] = self;
    mrb->c->ci->stack[1] = self;
    mrb->c->ci->stack[2] = mrb_nil_value();
    return MRB_PROC_CFUNC(p)(mrb, self);
  }
  int nregs = p->body.irep->nregs;
  if (nregs < 4) nregs = 4;
  stack_extend(mrb, nregs);
  mrb->c->ci->stack[0] = self;
  mrb->c->ci->stack[1] = self;
  stack_clear(mrb->c->ci->stack+2, nregs-2);
  cipush(mrb, 0, 0, NULL, NULL, NULL, 0, 0);

  return self;
}

/* 15.2.2.4.35 */
/*
 *  call-seq:
 *     mod.class_eval {| | block }  -> obj
 *     mod.module_eval {| | block } -> obj
 *
 *  Evaluates block in the context of _mod_. This can
 *  be used to add methods to a class. `module_eval` returns
 *  the result of evaluating its argument.
 */
mrb_value
mrb_mod_module_eval(mrb_state *mrb, mrb_value mod)
{
  mrb_value a, b;

  if (mrb_get_args(mrb, "|S&", &a, &b) == 1) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "module_eval/class_eval with string not implemented");
  }
  return eval_under(mrb, mod, b, mrb_class_ptr(mod));
}

/* 15.3.1.3.18 */
/*
 *  call-seq:
 *     obj.instance_eval {| | block }                       -> obj
 *
 *  Evaluates the given block,within  the context of the receiver (_obj_).
 *  In order to set the context, the variable `self` is set to _obj_ while
 *  the code is executing, giving the code access to _obj_'s
 *  instance variables. In the version of `instance_eval`
 *  that takes a `String`, the optional second and third
 *  parameters supply a filename and starting line number that are used
 *  when reporting compilation errors.
 *
 *     class KlassWithSecret
 *       def initialize
 *         @secret = 99
 *       end
 *     end
 *     k = KlassWithSecret.new
 *     k.instance_eval { @secret }   #=> 99
 */
mrb_value
mrb_obj_instance_eval(mrb_state *mrb, mrb_value self)
{
  mrb_value a, b;

  if (mrb_get_args(mrb, "|S&", &a, &b) == 1) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "instance_eval with string not implemented");
  }
  return eval_under(mrb, self, b, mrb_singleton_class_ptr(mrb, self));
}

static mrb_value
yield_with_attr(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv, mrb_value self, struct RClass *c,
                mrb_bool vis_break)
{
  check_block(mrb, b);

  mrb_callinfo *ci = mrb->c->ci;
  mrb_int n = mrb_ci_nregs(ci);
  const struct RProc *p = mrb_proc_ptr(b);
  mrb_sym mid;

  if (MRB_PROC_ENV_P(p)) {
    mid = p->e.env->mid;
  }
  else {
    mid = ci->mid;
  }
  ci = cipush(mrb, n, CINFO_DIRECT, NULL, NULL, NULL, mid, 0);
  funcall_args_capture(mrb, 0, argc, argv, mrb_nil_value(), ci);
  ci->u.target_class = c;
  ci->proc = p;
  if (vis_break) {
    MRB_CI_SET_VISIBILITY_BREAK(ci);
  }

  mrb_value val;
  if (MRB_PROC_CFUNC_P(p)) {
    mrb->exc = NULL;
    ci->stack[0] = self;
    val = MRB_PROC_CFUNC(p)(mrb, self);
    cipop(mrb);
    if (mrb->exc && mrb->jmp) {
      mrb_exc_raise(mrb, mrb_obj_value(mrb->exc));
    }
  }
  else {
    ci->cci = CINFO_SKIP;
    val = mrb_run(mrb, p, self);
  }
  return val;
}

/**
 * @brief Yields to a block with a specific `self` object and class context.
 *
 * This function executes a given block (`b`) with the provided arguments (`argv`).
 * The `self` object within the block will be `self`, and the class context
 * will be `c`. This allows for more control over the execution environment of
 * the block. The `vis_break` flag is set to TRUE, meaning visibility checks
 * (public/private/protected) are enforced.
 *
 * @param mrb The mruby state.
 * @param b The block (proc) to yield to.
 * @param argc The number of arguments in `argv`.
 * @param argv A pointer to an array of `mrb_value` arguments to pass to the block.
 * @param self The object that will be `self` inside the block.
 * @param c The class context for the block execution.
 * @return The result of the block execution.
 * @raise E_TYPE_ERROR if `b` is not a proc or nil.
 * @see mrb_yield_argv
 * @see mrb_yield
 */
MRB_API mrb_value
mrb_yield_with_class(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv, mrb_value self, struct RClass *c)
{
  return yield_with_attr(mrb, b, argc, argv, self, c, TRUE);
}

/**
 * @brief Yields to a block with an array of arguments.
 *
 * This function executes a given block (`b`) with the provided arguments (`argv`).
 * The `self` object and class context for the block execution are determined
 * from the block itself (its captured environment).
 * Visibility checks (public/private/protected) are not strictly enforced
 * in the same way as `mrb_yield_with_class` (vis_break is FALSE).
 *
 * @param mrb The mruby state.
 * @param b The block (proc) to yield to.
 * @param argc The number of arguments in `argv`.
 * @param argv A pointer to an array of `mrb_value` arguments to pass to the block.
 * @return The result of the block execution.
 * @raise E_TYPE_ERROR if `b` is not a proc or nil.
 * @see mrb_yield_with_class
 * @see mrb_yield
 */
MRB_API mrb_value
mrb_yield_argv(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv)
{
  const struct RProc *p = mrb_proc_ptr(b);
  struct RClass *tc;
  mrb_value self = mrb_proc_get_self(mrb, p, &tc);

  return yield_with_attr(mrb, b, argc, argv, self, tc, FALSE);
}

/**
 * @brief Yields to a block with a single argument.
 *
 * This function executes a given block (`b`) with a single argument (`arg`).
 * It's a convenience function for the common case of yielding with one argument.
 * The `self` object and class context for the block execution are determined
 * from the block itself.
 * Visibility checks are not strictly enforced (vis_break is FALSE).
 *
 * @param mrb The mruby state.
 * @param b The block (proc) to yield to.
 * @param arg The single `mrb_value` argument to pass to the block.
 * @return The result of the block execution.
 * @raise E_TYPE_ERROR if `b` is not a proc or nil.
 * @see mrb_yield_with_class
 * @see mrb_yield_argv
 */
MRB_API mrb_value
mrb_yield(mrb_state *mrb, mrb_value b, mrb_value arg)
{
  const struct RProc *p = mrb_proc_ptr(b);
  struct RClass *tc;
  mrb_value self = mrb_proc_get_self(mrb, p, &tc);

  return yield_with_attr(mrb, b, 1, &arg, self, tc, FALSE);
}

mrb_value
mrb_yield_cont(mrb_state *mrb, mrb_value b, mrb_value self, mrb_int argc, const mrb_value *argv)
{
  check_block(mrb, b);

  const struct RProc *p = mrb_proc_ptr(b);
  mrb_callinfo *ci = mrb->c->ci;

  stack_extend_adjust(mrb, 4, &argv);
  mrb->c->ci->stack[1] = mrb_ary_new_from_values(mrb, argc, argv);
  mrb->c->ci->stack[2] = mrb_nil_value();
  mrb->c->ci->stack[3] = mrb_nil_value();
  ci->n = 15;
  ci->nk = 0;
  return exec_irep(mrb, self, p);
}

#define RBREAK_TAG_FOREACH(f) \
  f(RBREAK_TAG_BREAK, 0) \
  f(RBREAK_TAG_JUMP, 1) \
  f(RBREAK_TAG_STOP, 2)

#define RBREAK_TAG_DEFINE(tag, i) tag = i,
enum {
  RBREAK_TAG_FOREACH(RBREAK_TAG_DEFINE)
};
#undef RBREAK_TAG_DEFINE

#define RBREAK_TAG_BIT          3
#define RBREAK_TAG_BIT_OFF      8
#define RBREAK_TAG_MASK         (~(~UINT32_C(0) << RBREAK_TAG_BIT))

static inline uint32_t
mrb_break_tag_get(struct RBreak *brk)
{
  return (brk->flags >> RBREAK_TAG_BIT_OFF) & RBREAK_TAG_MASK;
}

static inline void
mrb_break_tag_set(struct RBreak *brk, uint32_t tag)
{
  brk->flags &= ~(RBREAK_TAG_MASK << RBREAK_TAG_BIT_OFF);
  brk->flags |= (tag & RBREAK_TAG_MASK) << RBREAK_TAG_BIT_OFF;
}

static struct RBreak*
break_new(mrb_state *mrb, uint32_t tag, const mrb_callinfo *return_ci, mrb_value val)
{
  mrb_assert((size_t)(return_ci - mrb->c->cibase) <= (size_t)(mrb->c->ci - mrb->c->cibase));

  struct RBreak *brk = MRB_OBJ_ALLOC(mrb, MRB_TT_BREAK, NULL);
  brk->ci_break_index = return_ci - mrb->c->cibase;
  mrb_break_value_set(brk, val);
  mrb_break_tag_set(brk, tag);

  return brk;
}

#define MRB_CATCH_FILTER_RESCUE (UINT32_C(1) << MRB_CATCH_RESCUE)
#define MRB_CATCH_FILTER_ENSURE (UINT32_C(1) << MRB_CATCH_ENSURE)
#define MRB_CATCH_FILTER_ALL    (MRB_CATCH_FILTER_RESCUE | MRB_CATCH_FILTER_ENSURE)

static const struct mrb_irep_catch_handler *
catch_handler_find(const mrb_irep *irep, const mrb_code *pc, uint32_t filter)
{
/* The comparison operators use `>` and `<=` because pc already points to the next instruction */
#define catch_cover_p(pc, beg, end) ((pc) > (ptrdiff_t)(beg) && (pc) <= (ptrdiff_t)(end))

  mrb_assert(irep && irep->clen > 0);
  ptrdiff_t xpc = pc - irep->iseq;
  /* If it retry at the top level, pc will be 0, so check with -1 as the start position */
  mrb_assert(catch_cover_p(xpc, -1, irep->ilen));
  if (!catch_cover_p(xpc, -1, irep->ilen)) return NULL;

  /* Currently uses a simple linear search to avoid processing complexity. */
  size_t cnt = irep->clen;
  const struct mrb_irep_catch_handler *e = mrb_irep_catch_handler_table(irep) + cnt - 1;
  for (; cnt > 0; cnt--, e--) {
    if (((UINT32_C(1) << e->type) & filter) &&
        catch_cover_p(xpc, mrb_irep_catch_handler_unpack(e->begin), mrb_irep_catch_handler_unpack(e->end))) {
      return e;
    }
  }

#undef catch_cover_p

  return NULL;
}

#define RAISE_EXC(mrb, exc) do { \
  mrb_value exc_value = (exc); \
  mrb_exc_set(mrb, exc_value); \
  goto L_RAISE; \
} while (0)

#define RAISE_LIT(mrb, c, str)          RAISE_EXC(mrb, mrb_exc_new_lit(mrb, c, str))
#define RAISE_FORMAT(mrb, c, fmt, ...)  RAISE_EXC(mrb, mrb_exc_new_str(mrb, c, mrb_format(mrb, fmt, __VA_ARGS__)))

/* return codes for extracted opcode handlers */
#define VM_NEXT       0  /* continue to next instruction */
#define VM_RAISE      1  /* exception: goto L_RAISE */
#define VM_SEND_SYM   2  /* fallback send: goto L_SEND_SYM */
#define VM_SENDB_SYM  3  /* fallback sendb: goto L_SENDB_SYM */
#define VM_RETURN_NIL 4  /* nil irep: return nil via L_OP_RETURN */

#if defined(__GNUC__) || defined(__clang__)
#define MRB_FLATTEN __attribute__((flatten))
#else
#define MRB_FLATTEN
#endif

static void
argnum_error(mrb_state *mrb, mrb_int num)
{
  mrb_int argc = mrb->c->ci->n;

  if (argc == 15) {
    mrb_value args = mrb->c->ci->stack[1];
    if (mrb_array_p(args)) {
      argc = RARRAY_LEN(args);
    }
  }
  if (argc == 0 && mrb->c->ci->nk != 0 && !mrb_hash_empty_p(mrb, mrb->c->ci->stack[1])) {
    argc++;
  }
  mrb_value str = mrb_format(mrb, "wrong number of arguments (given %i, expected %i)", argc, num);
  mrb_value exc = mrb_exc_new_str(mrb, E_ARGUMENT_ERROR, str);
  mrb_exc_set(mrb, exc);
}

static mrb_bool
break_tag_p(struct RBreak *brk, uint32_t tag)
{
  return (brk != NULL && brk->tt == MRB_TT_BREAK) ? TRUE : FALSE;
}

static void
prepare_tagged_break(mrb_state *mrb, uint32_t tag, const mrb_callinfo *return_ci, mrb_value val)
{
  if (break_tag_p((struct RBreak*)mrb->exc, tag)) {
    mrb_break_tag_set((struct RBreak*)mrb->exc, tag);
  }
  else {
    mrb->exc = (struct RObject*)break_new(mrb, tag, return_ci, val);
  }
}

#define THROW_TAGGED_BREAK(mrb, tag, return_ci, val) \
  do { \
    prepare_tagged_break(mrb, tag, return_ci, val); \
    goto L_CATCH_TAGGED_BREAK; \
  } while (0)

#define UNWIND_ENSURE(mrb, ci, pc, tag, return_ci, val) \
  do { \
    const struct RProc *proc = (ci)->proc; \
    if (proc && !MRB_PROC_CFUNC_P(proc) && (irep = proc->body.irep) && irep->clen > 0 && \
        (ch = catch_handler_find(irep, pc, MRB_CATCH_FILTER_ENSURE))) { \
      THROW_TAGGED_BREAK(mrb, tag, return_ci, val); \
    } \
  } while (0)

/*
 *  CHECKPOINT_RESTORE(tag) {
 *    This part is executed when jumping by the same "tag" of RBreak (it is not executed the first time).
 *    Write the code required (initialization of variables, etc.) for the subsequent processing.
 *  }
 *  CHECKPOINT_MAIN(tag) {
 *    This part is always executed.
 *  }
 *  CHECKPOINT_END(tag);
 *
 *  ...
 *
 *  // Jump to CHECKPOINT_RESTORE with the same "tag".
 *  goto CHECKPOINT_LABEL_MAKE(tag);
 */

#define CHECKPOINT_LABEL_MAKE(tag) L_CHECKPOINT_ ## tag

#define CHECKPOINT_RESTORE(tag) \
  do { \
    if (FALSE) { \
      CHECKPOINT_LABEL_MAKE(tag): \
      do {

#define CHECKPOINT_MAIN(tag) \
      } while (0); \
    } \
    do {

#define CHECKPOINT_END(tag) \
    } while (0); \
  } while (0)

#ifdef MRB_USE_DEBUG_HOOK
#define CODE_FETCH_HOOK(mrb, irep, pc, regs) if ((mrb)->code_fetch_hook) (mrb)->code_fetch_hook((mrb), (irep), (pc), (regs));
#else
#define CODE_FETCH_HOOK(mrb, irep, pc, regs)
#endif

#ifdef MRB_BYTECODE_DECODE_OPTION
#define BYTECODE_DECODER(x) ((mrb)->bytecode_decoder)?(mrb)->bytecode_decoder((mrb), (x)):(x)
#else
#define BYTECODE_DECODER(x) (x)
#endif

#ifndef MRB_USE_VM_SWITCH_DISPATCH
#if !defined __GNUC__ && !defined __clang__ && !defined __INTEL_COMPILER
#define MRB_USE_VM_SWITCH_DISPATCH
#endif
#endif /* ifndef MRB_USE_VM_SWITCH_DISPATCH */

#ifdef MRB_USE_VM_SWITCH_DISPATCH

#define INIT_DISPATCH for (;;) { CALL_CODE_HOOKS(); switch (insn) {
#define CASE(insn,ops) case insn: DECODE_OPERANDS(ops); L_ ## insn ## _BODY:
#define NEXT goto L_END_DISPATCH
#define JUMP NEXT
#define END_DISPATCH L_END_DISPATCH: RETURN_IF_TASK_STOPPED(mrb);}}

#else

#define INIT_DISPATCH JUMP; return mrb_nil_value();
#define CASE(insn,ops) L_ ## insn: DECODE_OPERANDS(ops); L_ ## insn ## _BODY:
#define NEXT RETURN_IF_TASK_STOPPED(mrb); CALL_CODE_HOOKS(); goto *optable[insn]
#define JUMP NEXT
#define END_DISPATCH RETURN_IF_TASK_STOPPED(mrb)

#endif

#define DECODE_OPERANDS(ops) do { const mrb_code *pc = ci->pc+1; FETCH_ ## ops (); ci->pc = pc; } while (0)
#define CALL_CODE_HOOKS() do { insn = BYTECODE_DECODER(*ci->pc); CODE_FETCH_HOOK(mrb, irep, ci->pc, regs); } while (0)

#ifdef MRB_USE_TASK_SCHEDULER
/* TRUE when the current context is executing across a C call boundary, i.e.
   a C function on the stack re-entered the VM (mrb_funcall / mrb_yield /
   mrb_vm_run). A task cannot be suspended at such a point: the C stack
   frame between the scheduler's mrb_vm_exec and the current frame cannot
   be saved or restored, and returning early from the inner mrb_vm_exec
   would leave the call-info stack drifted, tripping the assertion in
   mrb_vm_run (issues #6864, #6868). The scheduler defers the switch until
   execution unwinds back to a frame with no C boundary. This mirrors the
   cooperative guard in Task.pass, which raises rather than defers.
   cibase is excluded: it is the entry frame of this mrb_vm_exec. */
static mrb_bool
task_across_c_boundary(mrb_state *mrb)
{
  for (mrb_callinfo *ci = mrb->c->ci; ci > mrb->c->cibase; ci--) {
    if (ci->cci > 0) return TRUE;
  }
  return FALSE;
}

/* Defer task switches while a C-level ObjectSpace walk holds gc.iterating
   true. The walk runs callbacks (which may call back into mrb_vm_exec via
   mrb_yield); returning early from an inner exec while the outer C
   iteration is still active drifts the call-info stack and eventually
   crashes (issue #6862). Switches resume at the next OP boundary after
   the walk releases gc.iterating. A pending switch is also deferred while
   executing across a C call boundary (see task_across_c_boundary). A
   pending MRB_TASK_STOPPED is not deferred, since the task is going away.

   A pending switch is likewise deferred while an exception is in flight
   (mrb->exc set). The L_RAISE handler-found path repoints ci->pc at the
   catch handler and falls into NEXT; honoring the switch there returns
   early BEFORE OP_EXCEPT consumes the exception, so the scheduler's
   execute_task_vm mistakes the already-handled exception for an
   unhandled one, captures it as the task result and clears mrb->exc —
   the resumed task then runs the rescue with no exception pending and
   the begin block silently evaluates to nil. Observed in the field as a
   C extension's mrb_raise being un-rescuable whenever it fires after a
   long-blocking call (the timeslice always expires mid-call, so
   task.switching is always pending at raise time). The same window
   covers break/ensure unwinding, which carries RBreak in mrb->exc
   across NEXT. Deferral is bounded: the handler's first instruction
   consumes mrb->exc, so the switch happens one instruction later.

   mrb->jmp is restored to prev_jmp before returning, exactly as the
   normal return paths below do. mrb_vm_exec set mrb->jmp to its own
   stack-local c_jmp on entry; leaving it dangling after this early return
   means a later raise longjmps into a freed frame (issue #6863).

   A pending switch is never honored on the root context. The root context
   is not a task: it has no scheduler frame to catch the early return, so
   bailing out of its mrb_vm_exec leaves the call-info stack drifted and trips
   the assertion in mrb_vm_run. This happens when Task.pass is driven from the
   root context (the UI-loop-on-root pattern) while a stray switch flag is
   left set by background-task activity (issue #6887). switching is set from
   the timer interrupt (mrb_tick), so a genuine task always clears it on the
   next OP boundary; only the root context can observe it spuriously.

   This macro must only be expanded where prev_jmp is in scope, i.e.
   inside mrb_vm_exec (via NEXT / END_DISPATCH). */
#define RETURN_IF_TASK_STOPPED(mrb) do { \
  if (((mrb)->task.switching && (mrb)->c != (mrb)->root_c && \
       !(mrb)->exc && \
       !(mrb)->gc.iterating && !task_across_c_boundary(mrb)) || \
      (mrb)->c->status == MRB_TASK_STOPPED) { \
    (mrb)->jmp = prev_jmp; \
    return mrb_nil_value(); \
  } \
} while (0)
#define TASK_STOP(mrb) do { \
  if (mrb->c->status != MRB_TASK_STOPPED) \
    mrb->c->status = MRB_TASK_STOPPED; \
} while (0)
#define TASK_RETURN_EXCEPTION_AS_VALUE(mrb) ((mrb)->task.exception_as_result)
#else
#define RETURN_IF_TASK_STOPPED(mrb)
#define TASK_STOP(mrb)
#define TASK_RETURN_EXCEPTION_AS_VALUE(mrb) FALSE
#endif

/**
 * @brief Executes a mruby bytecode sequence (iseq) within the VM.
 *
 * This function is a core part of the mruby execution process. It sets up
 * the VM environment for executing the bytecode instructions associated with
 * the given proc (Ruby procedure/method).
 *
 * It initializes the stack if necessary, extends it to accommodate the
 * required number of registers for the proc, and then calls `mrb_vm_exec`
 * to actually execute the bytecode.
 *
 * @param mrb The mruby state.
 * @param proc The RProc object containing the bytecode (iseq) to execute.
 *             This proc represents a Ruby method or block.
 * @param self The `self` object for the context of this execution.
 * @param stack_keep The number of values to preserve on the stack from the
 *                   previous context. This is used for managing nested calls
 *                   and ensuring that arguments or local variables from the
 *                   caller are accessible if needed, or that the stack is
 *                   correctly cleared.
 * @return The result of the bytecode execution (typically the value of the
 *         last evaluated expression).
 * @see mrb_vm_exec
 * @see mrb_top_run
 */
MRB_API mrb_value
mrb_vm_run(mrb_state *mrb, const struct RProc *proc, mrb_value self, mrb_int stack_keep)
{
  const mrb_irep *irep = proc->body.irep;
  struct mrb_context *c = mrb->c;
#ifdef MRB_DEBUG
  ptrdiff_t cioff = c->ci - c->cibase;
#endif
  mrb_int nregs = irep->nregs;

  if (!c->stbase) {
    stack_init(mrb);
  }
  if (stack_keep > nregs)
    nregs = stack_keep;
  else {
    struct REnv *e = CI_ENV(mrb->c->ci);
    if (e && (stack_keep == 0 || irep->nlocals < MRB_ENV_LEN(e))) {
      ci_env_set(mrb->c->ci, NULL);
      mrb_env_detach(mrb, e, mrb_ci_svar(mrb->c, mrb->c->ci), FALSE);
    }
  }
  stack_extend(mrb, nregs);
  stack_clear(c->ci->stack + stack_keep, nregs - stack_keep);
  c->ci->stack[0] = self;
  mrb_value result = mrb_vm_exec(mrb, proc, irep->iseq);
  mrb_assert(mrb->c == c);      /* do not switch fibers via mrb_vm_run(), unlike mrb_vm_exec() */
  mrb_assert(c->ci == c->cibase || (c->ci - c->cibase) == cioff - 1);
  return result;
}

static struct RClass*
check_target_class(mrb_state *mrb)
{
  struct RClass *target = CI_TARGET_CLASS(mrb->c->ci);
  if (!target) {
    mrb_raise(mrb, E_TYPE_ERROR, "no class/module to add method");
  }
  return target;
}

#define regs (ci->stack)

static mrb_value
hash_new_from_regs(mrb_state *mrb, mrb_int argc, mrb_int idx)
{
  mrb_value hash = mrb_hash_new_capa(mrb, argc);
  mrb_callinfo *ci = mrb->c->ci;
  while (argc--) {
    mrb_hash_set(mrb, hash, regs[idx+0], regs[idx+1]);
    ci = mrb->c->ci;
    idx += 2;
  }
  return hash;
}

#define ary_new_from_regs(mrb, argc, idx) mrb_ary_new_from_values(mrb, (argc), &regs[idx]);

/* type pair for arithmetic/comparison dispatch */
#define TYPES2(a,b) ((((uint16_t)(a))<<8)|(((uint16_t)(b))&0xff))

/*
 * Extracted opcode handlers.
 * These are static functions force-inlined back into mrb_vm_exec via
 * __attribute__((flatten)). The source stays clean while the compiled
 * output is identical to having the code inline.
 *
 * Return VM_NEXT to continue, VM_RAISE when an exception has been set.
 * VM_SEND_SYM/VM_SENDB_SYM for method fallback (mid set via out-param).
 */

static int
vm_op_blkpush(mrb_state *mrb, uint32_t a, uint16_t b)
{
  mrb_callinfo *ci = mrb->c->ci;
  int m1 = (b>>11)&0x3f;
  int r  = (b>>10)&0x1;
  int m2 = (b>>5)&0x1f;
  int kd = (b>>4)&0x1;
  int lv = (b>>0)&0xf;
  int offset = m1+r+m2+kd;
  mrb_value *stack;

  if (lv == 0) stack = regs + 1;
  else {
    struct REnv *e = uvenv(mrb, lv-1);
    if (!e || (!MRB_ENV_ONSTACK_P(e) && e->mid == 0) ||
        MRB_ENV_LEN(e) <= offset+1) {
      mrb_exc_set(mrb, mrb_exc_new_lit(mrb, E_LOCALJUMP_ERROR, "unexpected yield"));
      return VM_RAISE;
    }
    stack = e->stack + 1;
  }
  if (mrb_nil_p(stack[offset])) {
    mrb_exc_set(mrb, mrb_exc_new_lit(mrb, E_LOCALJUMP_ERROR, "unexpected yield"));
    return VM_RAISE;
  }
  regs[a] = stack[offset];
  return VM_NEXT;
}

static int
vm_op_argary(mrb_state *mrb, uint32_t a, uint16_t b)
{
  mrb_callinfo *ci = mrb->c->ci;
  mrb_int m1 = (b>>11)&0x3f;
  mrb_int r  = (b>>10)&0x1;
  mrb_int m2 = (b>>5)&0x1f;
  mrb_int kd = (b>>4)&0x1;
  mrb_int lv = (b>>0)&0xf;
  mrb_value *stack;

  if (ci->mid == 0 || CI_TARGET_CLASS(ci) == NULL) {
  L_NOSUPER:
    mrb_exc_set(mrb, mrb_exc_new_lit(mrb, E_NOMETHOD_ERROR, "super called outside of method"));
    return VM_RAISE;
  }
  if (lv == 0) stack = regs + 1;
  else {
    struct REnv *e = uvenv(mrb, lv-1);
    if (!e) goto L_NOSUPER;
    if (MRB_ENV_LEN(e) <= m1+r+m2+1)
      goto L_NOSUPER;
    stack = e->stack + 1;
  }
  if (r == 0) {
    regs[a] = mrb_ary_new_from_values(mrb, m1+m2, stack);
  }
  else {
    mrb_value *pp = NULL;
    struct RArray *rest;
    mrb_int len = 0;

    if (mrb_array_p(stack[m1])) {
      struct RArray *ary = mrb_ary_ptr(stack[m1]);

      pp = ARY_PTR(ary);
      len = ARY_LEN(ary);
    }
    regs[a] = mrb_ary_new_capa(mrb, m1+len+m2);
    rest = mrb_ary_ptr(regs[a]);
    if (m1 > 0) {
      stack_copy(ARY_PTR(rest), stack, m1);
    }
    if (len > 0) {
      stack_copy(ARY_PTR(rest)+m1, pp, len);
    }
    if (m2 > 0) {
      stack_copy(ARY_PTR(rest)+m1+len, stack+m1+1, m2);
    }
    ARY_SET_LEN(rest, m1+len+m2);
  }
  if (kd) {
    regs[a+1] = stack[m1+r+m2];
    regs[a+2] = stack[m1+r+m2+1];
  }
  else {
    regs[a+1] = stack[m1+r+m2];
  }
  return VM_NEXT;
}

static int
vm_op_enter(mrb_state *mrb, uint32_t a)
{
  mrb_callinfo *ci = mrb->c->ci;
  const mrb_irep *irep = ci->proc->body.irep;
  mrb_int argc = ci->n;
  mrb_value *argv = regs+1;

  mrb_int m1 = MRB_ASPEC_REQ(a);

  /* no other args */
  if ((a & ~0x7c0001) == 0 && argc < 15 && MRB_PROC_STRICT_P(ci->proc)) {
    if (mrb_unlikely(argc+(ci->nk==15) != m1)) { /* count kdict too */
      argnum_error(mrb, m1);
      return VM_RAISE;
    }
    /* clear local (but non-argument) variables */
    mrb_int pos = m1+2;     /* self+m1+blk */
    if (irep->nlocals-pos  > 0) {
      stack_clear(&regs[pos], irep->nlocals-pos);
    }
    return VM_NEXT;
  }

  mrb_int o  = MRB_ASPEC_OPT(a);
  mrb_int r  = MRB_ASPEC_REST(a);
  mrb_int m2 = MRB_ASPEC_POST(a);
  mrb_int kd = (MRB_ASPEC_KEY(a) > 0 || MRB_ASPEC_KDICT(a))? 1 : 0;
  /* unused
  int b  = MRB_ASPEC_BLOCK(a);
  */
  mrb_int const len = m1 + o + r + m2;

  mrb_value * const argv0 = argv;
  mrb_value blk = regs[ci_bidx(ci)];

  /* &nil: reject block */
  if (MRB_ASPEC_NOBLOCK(a) && !mrb_nil_p(blk)) {
    mrb_exc_set(mrb, mrb_exc_new_lit(mrb, E_ARGUMENT_ERROR, "no block accepted"));
    return VM_RAISE;
  }

  mrb_value kdict = mrb_nil_value();

  /* keyword arguments */
  if (ci->nk == 15) {
    kdict = regs[mrb_ci_kidx(ci)];
  }
  if (!kd) {
    if (!mrb_nil_p(kdict) && mrb_hash_p(kdict) && mrb_hash_size(mrb, kdict) > 0) {
      if (argc < 14) {
        ci->n++;
        argc++;    /* include kdict in normal arguments */
      }
      else if (argc == 14) {
        /* pack arguments and kdict */
        regs[1] = mrb_ary_new_from_values(mrb, argc+1, &regs[1]);
        argc = ci->n = 15;
      }
      else {/* argc == 15 */
        /* push kdict to packed arguments */
        mrb_ary_push(mrb, regs[1], kdict);
      }
    }
    kdict = mrb_nil_value();
    ci->nk = 0;
  }
  else if (!mrb_nil_p(kdict)) {
    mrb_gc_protect(mrb, kdict);
  }

  /* arguments is passed with Array */
  if (argc == 15) {
    struct RArray *ary = mrb_ary_ptr(regs[1]);
    argv = ARY_PTR(ary);
    argc = (int)ARY_LEN(ary);
    mrb_gc_protect(mrb, regs[1]);
  }

  /* strict argument check */
  if (ci->proc && MRB_PROC_STRICT_P(ci->proc)) {
    if (mrb_unlikely(argc < m1 + m2 || (r == 0 && argc > len))) {
      argnum_error(mrb, m1+m2);
      return VM_RAISE;
    }
  }
  /* extract first argument array to arguments */
  else if (len > 1 && argc == 1 && mrb_array_p(argv[0])) {
    mrb_gc_protect(mrb, argv[0]);
    argc = (int)RARRAY_LEN(argv[0]);
    argv = RARRAY_PTR(argv[0]);
  }

  /* rest arguments */
  mrb_value rest;
  if (argc < len) {
    mrb_int mlen = m2;
    if (argc < m1+m2) {
      mlen = m1 < argc ? argc - m1 : 0;
    }

    /* copy mandatory and optional arguments */
    if (argv0 != argv && argv) {
      value_move(&regs[1], argv, argc-mlen); /* m1 + o */
    }
    if (argc < m1) {
      stack_clear(&regs[argc+1], m1-argc);
    }
    /* copy post mandatory arguments */
    if (mlen) {
      value_move(&regs[len-m2+1], &argv[argc-mlen], mlen);
    }
    if (mlen < m2) {
      stack_clear(&regs[len-m2+mlen+1], m2-mlen);
    }
    /* initialize rest arguments with empty Array */
    if (r) {
      rest = mrb_ary_new_capa(mrb, 0);
      regs[m1+o+1] = rest;
    }
    /* skip initializer of passed arguments */
    if (o > 0 && argc > m1+m2)
      ci->pc += (argc - m1 - m2)*3;
  }
  else {
    mrb_int rnum = 0;
    if (argv0 != argv) {
      mrb_gc_protect(mrb, blk);
      value_move(&regs[1], argv, m1+o);
    }
    if (r) {
      rnum = argc-m1-o-m2;
      rest = mrb_ary_new_from_values(mrb, rnum, argv+m1+o);
      regs[m1+o+1] = rest;
    }
    if (m2 > 0 && argc-m2 > m1) {
      value_move(&regs[m1+o+r+1], &argv[m1+o+rnum], m2);
    }
    ci->pc += o*3;
  }

  /* need to be update blk first to protect blk from GC */
  mrb_int const kw_pos = len + kd;    /* where kwhash should be */
  mrb_int const blk_pos = kw_pos + 1; /* where block should be */
  regs[blk_pos] = blk;                /* move block */
  if (kd) {
    if (mrb_nil_p(kdict)) {
      kdict = mrb_hash_new_capa(mrb, 0);
    }
    regs[kw_pos] = kdict;             /* set kwhash */
    ci->nk = 15;
  }

  /* format arguments for generated code */
  ci->n = (uint8_t)len;

  /* clear local (but non-argument) variables */
  if (irep->nlocals-blk_pos-1 > 0) {
    stack_clear(&regs[blk_pos+1], irep->nlocals-blk_pos-1);
  }
  return VM_NEXT;
}

static int
vm_op_getidx(mrb_state *mrb, uint32_t a, mrb_sym *midp)
{
  mrb_callinfo *ci = mrb->c->ci;
  mrb_value va = regs[a], vb = regs[a+1];
  enum mrb_vtype tt = mrb_type(va);

  /* Array case is most common - check first with branch hint */
  if (mrb_likely(tt == MRB_TT_ARRAY)) {
    struct RArray *ary = mrb_ary_ptr(va);
    /* optimize only for Array itself; a subclass or singleton may override [],
       and mrb->idx_class[] is NULL while Array#[] is overridden, so the same
       comparison rejects that too */
    if (mrb_unlikely(ary->c != mrb->idx_class[MRB_IDX_OP_ARY_AREF])) goto getidx_fallback;
    if (mrb_likely(mrb_integer_p(vb))) {
      mrb_int idx = mrb_integer(vb);
      mrb_int len;
      mrb_value *ptr;

      /* Single ARY_EMBED_P check instead of two */
#ifndef MRB_ARY_NO_EMBED
      if (ARY_EMBED_P(ary)) {
        len = ARY_EMBED_LEN(ary);
        ptr = ary->as.ary;
      }
      else
#endif
      {
        len = ary->as.heap.len;
        ptr = ary->as.heap.ptr;
      }

      /* Unsigned comparison: handles negative idx as large positive */
      if (mrb_likely((mrb_uint)idx < (mrb_uint)len)) {
        regs[a] = ptr[idx];
      }
      else {
        regs[a] = mrb_ary_entry(va, idx);
      }
      return VM_NEXT;
    }
    goto getidx_fallback;
  }
  else if (tt == MRB_TT_HASH) {
    /* optimize only for Hash itself; see the Array branch above */
    if (mrb_obj_ptr(va)->c != mrb->idx_class[MRB_IDX_OP_HASH_AREF]) goto getidx_fallback;
    int ai = mrb_gc_arena_save(mrb);
    va = mrb_hash_get(mrb, va, vb);
    ci = mrb->c->ci;
    regs[a] = va;
    mrb_gc_arena_restore(mrb, ai);
    return VM_NEXT;
  }
  else if (tt == MRB_TT_STRING) {
    /* optimize only for String itself; see the Array branch above */
    if (mrb_obj_ptr(va)->c != mrb->idx_class[MRB_IDX_OP_STR_AREF]) goto getidx_fallback;
    switch (mrb_type(vb)) {
    case MRB_TT_INTEGER:
    case MRB_TT_STRING:
    case MRB_TT_RANGE:
      {
        int ai = mrb_gc_arena_save(mrb);
        va = mrb_str_aref(mrb, va, vb, mrb_undef_value());
        regs[a] = va;
        mrb_gc_arena_restore(mrb, ai);
        return VM_NEXT;
      }
    default:
      break;
    }
  }
getidx_fallback:
  *midp = MRB_OPSYM(aref);
  return VM_SEND_SYM;
}

static int
vm_op_getidx0(mrb_state *mrb, uint32_t a, uint16_t b, mrb_sym *midp)
{
  mrb_callinfo *ci = mrb->c->ci;
  mrb_value recv = regs[b];
  enum mrb_vtype tt = mrb_type(recv);

  if (mrb_likely(tt == MRB_TT_ARRAY)) {
    struct RArray *ary = mrb_ary_ptr(recv);
    if (mrb_unlikely(ary->c != mrb->idx_class[MRB_IDX_OP_ARY_AREF])) goto getidx0_fallback;
#ifndef MRB_ARY_NO_EMBED
    if (ARY_EMBED_P(ary)) {
      regs[a] = ARY_EMBED_LEN(ary) > 0 ? ary->as.ary[0] : mrb_nil_value();
    }
    else
#endif
    {
      regs[a] = ary->as.heap.len > 0 ? ary->as.heap.ptr[0] : mrb_nil_value();
    }
    return VM_NEXT;
  }
  else if (tt == MRB_TT_HASH) {
    if (mrb_obj_ptr(recv)->c != mrb->idx_class[MRB_IDX_OP_HASH_AREF]) goto getidx0_fallback;
    {
      /* same as the Hash branch of vm_op_getidx(): mrb_hash_get() can run a
         default proc and move the stack, so take the result first and store
         it through the refreshed `regs`. The arena is restored only after
         that store, which is what roots the result. */
      int ai = mrb_gc_arena_save(mrb);
      mrb_value val = mrb_hash_get(mrb, recv, mrb_fixnum_value(0));
      ci = mrb->c->ci;
      regs[a] = val;
      mrb_gc_arena_restore(mrb, ai);
    }
    return VM_NEXT;
  }
  else if (tt == MRB_TT_STRING) {
    /* optimize only for String itself; see vm_op_getidx() */
    if (mrb_obj_ptr(recv)->c != mrb->idx_class[MRB_IDX_OP_STR_AREF]) goto getidx0_fallback;
    {
      /* mrb_str_aref() allocates, and an inline opcode never runs the cfunc
         epilogue that would shrink the arena, so save and restore it here.
         Unlike the Hash branch above, `ci` needs no refresh: this call cannot
         run Ruby code and so cannot move the stack. */
      int ai = mrb_gc_arena_save(mrb);
      mrb_value val = mrb_str_aref(mrb, recv, mrb_fixnum_value(0), mrb_undef_value());
      regs[a] = val;
      mrb_gc_arena_restore(mrb, ai);
    }
    return VM_NEXT;
  }
getidx0_fallback:
  regs[a] = recv;
  SET_FIXNUM_VALUE(regs[a+1], 0);
  *midp = MRB_OPSYM(aref);
  return VM_SEND_SYM;
}

static int
vm_op_setidx(mrb_state *mrb, uint32_t a, mrb_sym *midp)
{
  mrb_callinfo *ci = mrb->c->ci;
  mrb_value va = regs[a], vb = regs[a+1], vc = regs[a+2];
  switch (mrb_type(va)) {
  case MRB_TT_ARRAY:
    /* optimize only for Array itself; see vm_op_getidx() */
    if (mrb_obj_ptr(va)->c != mrb->idx_class[MRB_IDX_OP_ARY_ASET]) goto setidx_fallback;
    if (!mrb_integer_p(vb)) goto setidx_fallback;
    mrb_ary_set(mrb, va, mrb_integer(vb), vc);
    ci = mrb->c->ci;
    regs[a] = vc;
    return VM_NEXT;
  case MRB_TT_HASH:
    /* optimize only for Hash itself; see vm_op_getidx() */
    if (mrb_obj_ptr(va)->c != mrb->idx_class[MRB_IDX_OP_HASH_ASET]) goto setidx_fallback;
    {
      int ai = mrb_gc_arena_save(mrb);
      mrb_hash_set(mrb, va, vb, vc);
      ci = mrb->c->ci;
      regs[a] = vc;
      mrb_gc_arena_restore(mrb, ai);
    }
    return VM_NEXT;
  case MRB_TT_STRING:
    /* optimize only for String itself; see vm_op_getidx() */
    if (mrb_obj_ptr(va)->c != mrb->idx_class[MRB_IDX_OP_STR_ASET]) goto setidx_fallback;
    /* A replacement that is not a String is a TypeError rather than a store,
       and the method is where it is raised: leaving it to the send keeps the
       `String#[]=` frame the backtrace has always shown for it. */
    if (!mrb_string_p(vc)) goto setidx_fallback;
    switch (mrb_type(vb)) {
    case MRB_TT_INTEGER:
    case MRB_TT_STRING:
    case MRB_TT_RANGE:
      {
        /* mrb_str_aset() allocates, and an inline opcode never runs the cfunc
           epilogue that would shrink the arena, so save and restore it here.
           As in the String branch of vm_op_getidx(), `ci` needs no refresh:
           none of the three index types reaches a conversion that runs Ruby
           code, so the call cannot move the stack. */
        int ai = mrb_gc_arena_save(mrb);
        mrb_str_aset(mrb, va, vb, mrb_undef_value(), vc);
        regs[a] = vc;
        mrb_gc_arena_restore(mrb, ai);
      }
      return VM_NEXT;
    default:
      break;
    }
    goto setidx_fallback;
  default:
  setidx_fallback:
    SET_NIL_VALUE(regs[a+3]);
    *midp = MRB_OPSYM(aset);
    return VM_SENDB_SYM;
  }
}

static int
vm_op_div(mrb_state *mrb, uint32_t a, mrb_sym *midp)
{
  mrb_callinfo *ci = mrb->c->ci;
#ifndef MRB_NO_FLOAT
  mrb_float x, y, f;
#endif

  /* need to check if op is overridden */
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
  case TYPES2(MRB_TT_INTEGER,MRB_TT_INTEGER):
    {
      mrb_int x = mrb_integer(regs[a]);
      mrb_int y = mrb_integer(regs[a+1]);
      int ai = mrb_gc_arena_save(mrb);
      regs[a] = mrb_div_int_value(mrb, x, y);
      mrb_gc_arena_restore(mrb, ai);
    }
    return VM_NEXT;
#ifndef MRB_NO_FLOAT
  case TYPES2(MRB_TT_INTEGER,MRB_TT_FLOAT):
    x = (mrb_float)mrb_integer(regs[a]);
    y = mrb_float(regs[a+1]);
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_INTEGER):
    x = mrb_float(regs[a]);
    y = (mrb_float)mrb_integer(regs[a+1]);
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
    x = mrb_float(regs[a]);
    y = mrb_float(regs[a+1]);
    break;
#endif
  default:
    *midp = MRB_OPSYM(div);
    return VM_SEND_SYM;
  }

#ifndef MRB_NO_FLOAT
  f = mrb_div_float(x, y);
  {
    /* This is the one boxing site of OP_DIV that sits outside mrb_vm_exec(),
       so VM_SET_FLOAT_VALUE() and the arena index it restores to are not in
       scope.  Saving here instead restores to the height on entry to this
       branch rather than to the height on entry to the frame, which still
       bounds the arena across a loop and is what the Integer branch above
       already does.  The result needs no protection past the restore: it is
       stored into regs[], and the VM stack is a GC root. */
    int ai = mrb_gc_arena_save(mrb);
    SET_FLOAT_VALUE(mrb, regs[a], f);
    mrb_gc_arena_restore(mrb, ai);
  }
#endif
  return VM_NEXT;
}

static mrb_sym
vm_define_method(mrb_state *mrb, struct RClass *tc, const mrb_irep *irep, uint16_t b, uint16_t c)
{
  struct RProc *p = mrb_proc_new(mrb, irep->reps[c]);
  mrb_sym mid = irep->syms[b];
  mrb_method_t m;

  p->flags |= MRB_PROC_SCOPE | MRB_PROC_STRICT;
  MRB_METHOD_FROM_PROC(m, p);
  MRB_METHOD_SET_VISIBILITY(m, MRB_METHOD_VDEFAULT_FL);
  mrb_define_method_raw(mrb, tc, mid, m);
  mrb_method_added(mrb, tc, mid);
  return mid;
}

/* Common proc dispatch for OP_CALL and OP_BLKCALL.
   Returns VM_NEXT, VM_RAISE, or VM_RETURN_NIL. */
static int
vm_call_proc(mrb_state *mrb, const struct RProc *p, mrb_int nargs,
             const mrb_irep **irepp, int ai)
{
  mrb_callinfo *ci = mrb->c->ci;
  mrb_value recv = ci->stack[0];

  /* handle alias */
  MRB_PROC_RESOLVE_ALIAS(ci, p);
  if (MRB_PROC_ENV_P(p)) {
    ci->mid = MRB_PROC_ENV(p)->mid;
  }
  ci->u.target_class = MRB_PROC_TARGET_CLASS(p);
  CI_PROC_SET(ci, p);

  if (MRB_PROC_CFUNC_P(p)) {
    recv = MRB_PROC_CFUNC(p)(mrb, recv);
    mrb_gc_arena_shrink(mrb, ai);
    if (mrb_unlikely(mrb->exc)) return VM_RAISE;
    ci = cipop(mrb);
    ci[1].stack[0] = recv;
    *irepp = ci->proc->body.irep;
  }
  else {
    const mrb_irep *irep = p->body.irep;
    if (!irep) {
      ci->stack[0] = mrb_nil_value();
      return VM_RETURN_NIL;
    }
    if (nargs < irep->nregs) {
      stack_extend(mrb, irep->nregs);
      stack_clear(ci->stack+nargs, irep->nregs-nargs);
    }
    if (MRB_PROC_ENV_P(p)) {
      ci->stack[0] = MRB_PROC_ENV(p)->stack[0];
    }
    ci->pc = irep->iseq;
    *irepp = irep;
  }
  return VM_NEXT;
}

/* Stores an mrb_int into a register from inside mrb_vm_exec().
   SET_INT_VALUE() heap-allocates an RInteger for a value outside the fixnum
   range, and an inline opcode has no cfunc epilogue behind it to shrink the
   arena, so a boxing site in the interpreter loop has to restore for itself.
   The restore is skipped when the store produced an immediate, which is the
   fixnum fast path and allocates nothing.  Where the macro cannot allocate at
   all this expands to the bare store.  `ai` is the arena index mrb_vm_exec()
   saved on entry. */
#if defined(MRB_WORD_BOXING) || (defined(MRB_NAN_BOXING) && defined(MRB_INT64))
#define VM_SET_INT_VALUE(r,n) do {                                          \
  SET_INT_VALUE(mrb, r, n);                                                 \
  if (!mrb_immediate_p(r)) mrb_gc_arena_restore(mrb, ai);                   \
} while (0)
#else
#define VM_SET_INT_VALUE(r,n) SET_INT_VALUE(mrb,r,n)
#endif

/* The same for an mrb_float.  Under word boxing SET_FLOAT_VALUE() heap-
   allocates an RFloat for a value the mrb_value word cannot hold inline: every
   float under MRB_WORDBOX_NO_INLINE_FLOAT, and otherwise a subnormal, an
   exponent outside the inlinable range, or a rotation that would collide with
   a sentinel.  The other boxing modes store the float in the mrb_value itself
   and never allocate, so there this expands to the bare store.  As above the
   restore is skipped when the store produced an immediate, which is the inline
   float fast path, and `ai` is mrb_vm_exec()'s saved arena index. */
#ifdef MRB_WORD_BOXING
#define VM_SET_FLOAT_VALUE(r,f) do {                                        \
  SET_FLOAT_VALUE(mrb, r, f);                                               \
  if (!mrb_immediate_p(r)) mrb_gc_arena_restore(mrb, ai);                   \
} while (0)
#else
#define VM_SET_FLOAT_VALUE(r,f) SET_FLOAT_VALUE(mrb,r,f)
#endif

/**
 * @brief Executes a sequence of mruby bytecode instructions.
 *
 * This is the main bytecode interpreter loop. It takes a starting proc
 * (`begin_proc`) and a pointer to the initial instruction (`iseq`) within
 * that proc's instruction sequence. It then enters a loop, fetching and
 * dispatching bytecode operations until an OP_STOP instruction is encountered,
 * an exception occurs, or a C function call returns.
 *
 * This function handles the low-level details of instruction decoding,
 * stack manipulation, exception handling (try/catch blocks within mruby code),
 * and calling C functions or other mruby methods.
 *
 * @param mrb The mruby state.
 * @param begin_proc The initial RProc whose bytecode is to be executed.
 *                   While the name suggests it's the "beginning" proc,
 *                   execution might involve other procs called from this one.
 * @param iseq A pointer to the first bytecode instruction to execute within
 *             `begin_proc`'s instruction sequence.
 * @return The result of the execution. This could be the return value of
 *         the executed Ruby code, an exception object if an unhandled
 *         exception occurred, or the result of a fiber switch.
 * @note This function is highly complex and central to mruby's operation.
 *       It uses a jump table (`optable`) for efficient instruction dispatch
 *       when not using switch-based dispatch. It also manages the callinfo
 *       stack (`ci`) for tracking method/block calls.
 */
MRB_FLATTEN MRB_API mrb_value
mrb_vm_exec(mrb_state *mrb, const struct RProc *begin_proc, const mrb_code *iseq)
{
  /* mrb_assert(MRB_PROC_CFUNC_P(begin_proc)) */
  const mrb_irep *irep = begin_proc->body.irep;
  mrb_code insn;
  int ai = mrb_gc_arena_save(mrb);
  struct mrb_jmpbuf *prev_jmp = mrb->jmp;
  struct mrb_jmpbuf c_jmp;
  uint32_t a;
  uint16_t b;
  uint16_t c;
  mrb_sym mid;
  const struct mrb_irep_catch_handler *ch;

#ifndef MRB_USE_VM_SWITCH_DISPATCH
  static const void * const optable[] = {
#define OPCODE(x,_) &&L_OP_ ## x,
#include <mruby/ops.h>
#undef OPCODE
  };
#endif

  mrb->exc = NULL;

  mrb_callinfo *ci = mrb->c->ci;
  CI_PROC_SET(ci, begin_proc);
  ci->pc = iseq;

RETRY_TRY_BLOCK:

  MRB_TRY(&c_jmp) {

  if (mrb_unlikely(mrb->exc)) {
    mrb_gc_arena_restore(mrb, ai);
    if (mrb->exc->tt == MRB_TT_BREAK)
      goto L_BREAK;
    goto L_RAISE;
  }
  /* Intentionally store stack variable address for exception handling.
   * This is safe because the pointer is cleared before function returns.
   * Suppress GCC 12+ warning about dangling pointer. */
#if defined(__GNUC__) && !defined(__clang__)
  #if __GNUC__ >= 12
    #pragma GCC diagnostic push
    #pragma GCC diagnostic ignored "-Wdangling-pointer"
  #endif
#endif
  mrb->jmp = &c_jmp;
#if defined(__GNUC__) && !defined(__clang__)
  #if __GNUC__ >= 12
    #pragma GCC diagnostic pop
  #endif
#endif

  INIT_DISPATCH {
    CASE(OP_NOP, Z) {
      /* do nothing */
      NEXT;
    }

    CASE(OP_MOVE, BB) {
      regs[a] = regs[b];
      NEXT;
    }

    CASE(OP_LOADL, BB) {
      switch (irep->pool[b].tt) {   /* number */
      case IREP_TT_INT32:
        VM_SET_INT_VALUE(regs[a], (mrb_int)irep->pool[b].u.i32);
        break;
      case IREP_TT_INT64:
#if defined(MRB_INT64)
        VM_SET_INT_VALUE(regs[a], (mrb_int)irep->pool[b].u.i64);
        break;
#else
#if defined(MRB_64BIT)
        if (INT32_MIN <= irep->pool[b].u.i64 && irep->pool[b].u.i64 <= INT32_MAX) {
          VM_SET_INT_VALUE(regs[a], (mrb_int)irep->pool[b].u.i64);
          break;
        }
#endif
        goto L_INT_OVERFLOW;
#endif
      case IREP_TT_BIGINT:
#ifdef MRB_USE_BIGINT
        {
          const char *s = irep->pool[b].u.str;
          regs[a] = mrb_bint_new_str(mrb, s+2, (uint8_t)s[0], (int8_t)s[1]);
          mrb_gc_arena_restore(mrb, ai);
        }
        break;
#else
        goto L_INT_OVERFLOW;
#endif
#ifndef MRB_NO_FLOAT
      case IREP_TT_FLOAT:
        VM_SET_FLOAT_VALUE(regs[a], irep->pool[b].u.f);
        break;
#endif
      default:
        /* should not happen (tt:string) */
        regs[a] = mrb_nil_value();
        break;
      }
      NEXT;
    }

    CASE(OP_LOADI8, BB) {
      SET_FIXNUM_VALUE(regs[a], b);
      NEXT;
    }

    CASE(OP_LOADINEG, BB) {
      SET_FIXNUM_VALUE(regs[a], -b);
      NEXT;
    }

    CASE(OP_LOADI__1,B) goto L_LOADI;
    CASE(OP_LOADI_0,B) goto L_LOADI;
    CASE(OP_LOADI_1,B) goto L_LOADI;
    CASE(OP_LOADI_2,B) goto L_LOADI;
    CASE(OP_LOADI_3,B) goto L_LOADI;
    CASE(OP_LOADI_4,B) goto L_LOADI;
    CASE(OP_LOADI_5,B) goto L_LOADI;
    CASE(OP_LOADI_6,B) goto L_LOADI;
    CASE(OP_LOADI_7, B) {
    L_LOADI:
      SET_FIXNUM_VALUE(regs[a], (mrb_int)insn - (mrb_int)OP_LOADI_0);
      NEXT;
    }

    CASE(OP_LOADI16, BS) {
      SET_FIXNUM_VALUE(regs[a], (mrb_int)(int16_t)b);
      NEXT;
    }

    CASE(OP_LOADI32, BSS) {
      VM_SET_INT_VALUE(regs[a], (int32_t)(((uint32_t)b<<16)+c));
      NEXT;
    }

    CASE(OP_LOADSYM, BB) {
      SET_SYM_VALUE(regs[a], irep->syms[b]);
      NEXT;
    }

    CASE(OP_LOADNIL, B) {
      SET_NIL_VALUE(regs[a]);
      NEXT;
    }

    CASE(OP_LOADSELF, B) {
      regs[a] = regs[0];
      NEXT;
    }

    CASE(OP_LOADTRUE, B) {
      SET_TRUE_VALUE(regs[a]);
      NEXT;
    }

    CASE(OP_LOADFALSE, B) {
      SET_FALSE_VALUE(regs[a]);
      NEXT;
    }

    CASE(OP_GETGV, BB) {
      mrb_value val = mrb_gv_get(mrb, irep->syms[b]);
      ci = mrb->c->ci;
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETGV, BB) {
      mrb_gv_set(mrb, irep->syms[b], regs[a]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_GETSV, BB) {
      mrb_value val = mrb_vm_special_get(mrb, irep->syms[b]);
      ci = mrb->c->ci;
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETSV, BB) {
      mrb_vm_special_set(mrb, irep->syms[b], regs[a]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_GETIV, BB) {
      mrb_value recv = regs[0];
      /* shaped fast path: self is almost always a plain object here, and the
         lookup neither allocates nor raises, so ci stays valid */
      if (mrb_type(recv) == MRB_TT_OBJECT) {
        struct RObject *o = mrb_obj_ptr(recv);
        if (MRB_OBJ_SHAPED_P(o) && o->iv) {
          mrb_shaped_iv *siv = (mrb_shaped_iv*)o->iv;
          int idx = mrb_shape_lookup(mrb, siv->shape, irep->syms[b]);
          regs[a] = (idx >= 0 && !mrb_undef_p(siv->values[idx]))
                    ? siv->values[idx] : mrb_nil_value();
          NEXT;
        }
      }
      {
        /* same as OP_ARYCAT: mrb_iv_get() can run a `method_missing`-style
           callback, so the store has to go through the refreshed `regs` */
        mrb_value iv = mrb_iv_get(mrb, recv, irep->syms[b]);
        ci = mrb->c->ci;
        regs[a] = iv;
      }
      NEXT;
    }

    CASE(OP_SETIV, BB) {
      mrb_value recv = regs[0];
      /* shaped fast path: only overwrite an already-present slot on an
         unfrozen object; shape transitions, frozen errors and non-objects
         take the full path */
      if (mrb_type(recv) == MRB_TT_OBJECT) {
        struct RObject *o = mrb_obj_ptr(recv);
        if (MRB_OBJ_SHAPED_P(o) && o->iv && !mrb_frozen_p(o)) {
          mrb_shaped_iv *siv = (mrb_shaped_iv*)o->iv;
          int idx = mrb_shape_lookup(mrb, siv->shape, irep->syms[b]);
          if (idx >= 0 && !mrb_undef_p(siv->values[idx])) {
            siv->values[idx] = regs[a];
            mrb_field_write_barrier_value(mrb, (struct RBasic*)o, regs[a]);
            NEXT;
          }
        }
      }
      mrb_iv_set(mrb, recv, irep->syms[b], regs[a]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_GETCV, BB) {
      mrb_value val;
      val = mrb_vm_cv_get(mrb, irep->syms[b]);
      ci = mrb->c->ci;
      regs[a] = val;
      NEXT;
    }

    CASE(OP_SETCV, BB) {
      mrb_vm_cv_set(mrb, irep->syms[b], regs[a]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_GETIDX, B) {
      int r = vm_op_getidx(mrb, a, &mid);
      ci = mrb->c->ci;
      if (r == VM_SEND_SYM) goto L_SEND_SYM;
      NEXT;
    }

    CASE(OP_GETIDX0, BB) {
      int r = vm_op_getidx0(mrb, a, b, &mid);
      ci = mrb->c->ci;
      if (r == VM_SEND_SYM) goto L_SEND_SYM;
      NEXT;
    }

    CASE(OP_SETIDX, B) {
      int r = vm_op_setidx(mrb, a, &mid);
      ci = mrb->c->ci;
      if (r == VM_SENDB_SYM) { c = 2; goto L_SENDB_SYM; }
      NEXT;
    }

    CASE(OP_GETCONST, BB) {
#ifndef MRB_NO_CONST_CACHE
      mrb_sym sym = irep->syms[b];
      uint32_t h = mrb_int_hash_func(mrb, ((intptr_t)irep) ^ sym) & (MRB_CONST_CACHE_SIZE-1);
      struct mrb_const_cache_entry *cc = &mrb->const_cache[h];
      if (cc->irep == irep && cc->sym == sym) {
        regs[a] = cc->value;
        NEXT;
      }
#endif
      {
        mrb_value v = mrb_vm_const_get(mrb, irep->syms[b]);
        ci = mrb->c->ci;
        regs[a] = v;
#ifndef MRB_NO_CONST_CACHE
        cc->irep = irep;
        cc->sym = sym;
        cc->value = v;
#endif
      }
      NEXT;
    }

    CASE(OP_SETCONST, BB) {
      ci = mrb->c->ci;
      struct RClass *c = MRB_PROC_TARGET_CLASS(ci->proc);
      if (!c) c = mrb->object_class;
      mrb_const_set(mrb, mrb_obj_value(c), irep->syms[b], regs[a]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_GETMCNST, BB) {
      mrb_value v = mrb_const_get(mrb, regs[a], irep->syms[b]);
      ci = mrb->c->ci;
      regs[a] = v;
      NEXT;
    }

    CASE(OP_SETMCNST, BB) {
      mrb_const_set(mrb, regs[a+1], irep->syms[b], regs[a]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_GETUPVAR, BBB) {
      struct REnv *e = uvenv(mrb, c);

      if (e && b < MRB_ENV_LEN(e)) {
        regs[a] = e->stack[b];
      }
      else {
        regs[a] = mrb_nil_value();
      }
      NEXT;
    }

    CASE(OP_SETUPVAR, BBB) {
      struct REnv *e = uvenv(mrb, c);

      if (e) {
        if (b < MRB_ENV_LEN(e)) {
          e->stack[b] = regs[a];
          mrb_write_barrier(mrb, (struct RBasic*)e);
        }
      }
      NEXT;
    }

    CASE(OP_JMP, S) {
      ci->pc += (int16_t)a;
      JUMP;
    }
    CASE(OP_JMPIF, BS) {
      if (mrb_test(regs[a])) {
        ci->pc += (int16_t)b;
        JUMP;
      }
      NEXT;
    }
    CASE(OP_JMPNOT, BS) {
      if (!mrb_test(regs[a])) {
        ci->pc += (int16_t)b;
        JUMP;
      }
      NEXT;
    }
    CASE(OP_JMPNIL, BS) {
      if (mrb_nil_p(regs[a])) {
        ci->pc += (int16_t)b;
        JUMP;
      }
      NEXT;
    }

    CASE(OP_JMPUW, S) {
      a = (uint32_t)((ci->pc - irep->iseq) + (int16_t)a);
      CHECKPOINT_RESTORE(RBREAK_TAG_JUMP) {
        struct RBreak *brk = (struct RBreak*)mrb->exc;
        mrb_value target = mrb_break_value_get(brk);
        mrb_assert(mrb_integer_p(target));
        a = (uint32_t)mrb_integer(target);
        mrb_assert(a >= 0 && a < irep->ilen);
      }
      CHECKPOINT_MAIN(RBREAK_TAG_JUMP) {
        if (irep->clen > 0 &&
            (ch = catch_handler_find(irep, ci->pc, MRB_CATCH_FILTER_ENSURE))) {
          /* avoiding a jump from a catch handler into the same handler */
          if (a < mrb_irep_catch_handler_unpack(ch->begin) || a > mrb_irep_catch_handler_unpack(ch->end)) {
            THROW_TAGGED_BREAK(mrb, RBREAK_TAG_JUMP, mrb->c->ci, mrb_fixnum_value(a));
          }
        }
      }
      CHECKPOINT_END(RBREAK_TAG_JUMP);

      mrb->exc = NULL; /* clear break object */
      ci->pc = irep->iseq + a;
      JUMP;
    }

    CASE(OP_EXCEPT, B) {
      mrb_value exc;

      if (mrb->exc == NULL) {
        exc = mrb_nil_value();
      }
      else {
        switch (mrb->exc->tt) {
        case MRB_TT_BREAK:
        case MRB_TT_EXCEPTION:
          exc = mrb_obj_value(mrb->exc);
          break;
        default:
          mrb_assert(!"bad mrb_type");
          exc = mrb_nil_value();
          break;
        }
        mrb->exc = NULL;
      }
      regs[a] = exc;
      NEXT;
    }
    CASE(OP_RESCUE, BB) {
      mrb_value exc = regs[a];  /* exc on stack */
      mrb_value e = regs[b];
      struct RClass *ec;

      switch (mrb_type(e)) {
      case MRB_TT_CLASS:
      case MRB_TT_MODULE:
        break;
      default:
        RAISE_LIT(mrb, E_TYPE_ERROR, "class or module required for rescue clause");
      }
      ec = mrb_class_ptr(e);
      regs[b] = mrb_bool_value(mrb_obj_is_kind_of(mrb, exc, ec));
      NEXT;
    }

    CASE(OP_RAISEIF, B) {
      mrb_value exc;
      exc = regs[a];
      if (mrb_likely(mrb_nil_p(exc))) {
        mrb->exc = NULL;
      }
      else if (mrb_break_p(exc)) {
        struct RBreak *brk;
        mrb->exc = mrb_obj_ptr(exc);
      L_BREAK:
        brk = (struct RBreak*)mrb->exc;
        switch (mrb_break_tag_get(brk)) {
#define DISPATCH_CHECKPOINTS(n, i) case n: goto CHECKPOINT_LABEL_MAKE(n);
          RBREAK_TAG_FOREACH(DISPATCH_CHECKPOINTS)
#undef DISPATCH_CHECKPOINTS
          default:
            mrb_assert(!"wrong break tag");
        }
      }
      else {
        mrb_exc_set(mrb, exc);
      L_RAISE:
        ci = mrb->c->ci;
        while (!ci->proc || MRB_PROC_CFUNC_P(ci->proc) || !(irep = ci->proc->body.irep) || irep->clen < 1 ||
               (ch = catch_handler_find(irep, ci->pc, MRB_CATCH_FILTER_ALL)) == NULL) {
          if (ci != mrb->c->cibase) {
            ci = cipop(mrb);
            if (ci[1].cci == CINFO_SKIP) {
              mrb_assert(prev_jmp != NULL);
              mrb->jmp = prev_jmp;
              MRB_THROW(prev_jmp);
            }
          }
          else if (mrb->c == mrb->root_c) {
            ci->stack = mrb->c->stbase;
            mrb->jmp = prev_jmp;
            return mrb_obj_value(mrb->exc);
          }
          else {
            struct mrb_context *c = mrb->c;

            fiber_terminate(mrb, c, ci);
            if (mrb_unlikely(!c->vmexec)) goto L_RAISE;
            mrb->jmp = prev_jmp;
            if (TASK_RETURN_EXCEPTION_AS_VALUE(mrb)) return mrb_obj_value(mrb->exc);
            if (!prev_jmp) return mrb_obj_value(mrb->exc);
            MRB_THROW(prev_jmp);
          }
        }

        if (FALSE) {
        L_CATCH_TAGGED_BREAK: /* from THROW_TAGGED_BREAK() or UNWIND_ENSURE() */
          ci = mrb->c->ci;
        }
        irep = ci->proc->body.irep;
        stack_extend(mrb, irep->nregs);
        ci->pc = irep->iseq + mrb_irep_catch_handler_unpack(ch->target);
      }
      NEXT;
    }

    CASE(OP_MATCHERR, B) {
      if (mrb_unlikely(!mrb_test(regs[a]))) {
        RAISE_LIT(mrb, mrb_exc_get_id(mrb, MRB_ERROR_SYM(NoMatchingPatternError)), "pattern not matched");
      }
      NEXT;
    }

    CASE(OP_SSEND, BBB) {
      regs[a] = regs[0];
    }
    goto L_SENDB;

    CASE(OP_SSEND0, BB) {
      regs[a] = regs[0];
      c = 0;
    }
    goto L_SENDB;

    CASE(OP_SSENDB, BBB) {
      regs[a] = regs[0];
    }
    goto L_SENDB;

    CASE(OP_SEND, BBB)
    goto L_SENDB;

    CASE(OP_SEND0, BB) {
      c = 0;
    }
    goto L_SENDB;

    L_SEND_SYM:
    c = 1;
    /* push nil after arguments */
    SET_NIL_VALUE(regs[a+2]);
    goto L_SENDB_SYM;

    CASE(OP_SENDB, BBB)
    L_SENDB:
    mid = irep->syms[b];
    L_SENDB_SYM:
    {
      mrb_method_t m;
      mrb_value recv, blk;
      mrb_int bidx, new_bidx;

      if (mrb_likely(c < CALL_MAXARGS)) {
        /* fast path limited to fixed length arguments of less than 15 */
        bidx = a + c + 1 /* self */;
        new_bidx = bidx;
      }
      else {
        int n = c&0xf;
        int nk = (c>>4)&0xf;
        bidx = a + mrb_bidx(n,nk);
        new_bidx = bidx;
        if (nk == CALL_MAXARGS) {
          mrb_ensure_hash_type(mrb, regs[a+(n==CALL_MAXARGS?1:n)+1]);
        }
        else if (nk > 0) {  /* pack keyword arguments */
          mrb_int kidx = a+(n==CALL_MAXARGS?1:n)+1;
          mrb_value kdict = hash_new_from_regs(mrb, nk, kidx);
          ci = mrb->c->ci;
          regs[kidx] = kdict;
          nk = CALL_MAXARGS;
          c = n | (nk<<4);
          new_bidx = a+mrb_bidx(n, nk);
        }
      }

      mrb_assert(bidx < irep->nregs);
      if (insn == OP_SEND || insn == OP_SEND0 || insn == OP_SSEND || insn == OP_SSEND0) {
        /* clear block argument */
        SET_NIL_VALUE(regs[new_bidx]);
        SET_NIL_VALUE(blk);
      }
      else {
        blk = ensure_block(mrb, regs[bidx]);
        ci = mrb->c->ci;
        regs[new_bidx] = blk;
      }

      ci = cipush(mrb, a, CINFO_DIRECT, NULL, NULL, BLK_PTR(blk), 0, c);
      recv = regs[0];
      ci->u.target_class = (insn == OP_SUPER) ? CI_TARGET_CLASS(ci - 1)->super : mrb_class(mrb, recv);
      m = mrb_vm_find_method(mrb, ci->u.target_class, &ci->u.target_class, mid);
      if (mrb_unlikely(MRB_METHOD_UNDEF_P(m))) {
        m = prepare_missing(mrb, ci, recv, mid, blk, (insn == OP_SUPER));
      }
      else {
        ci->mid = mid;
        /* visibility is checked only when the method is actually found;
           the `method_missing` fallback dispatches regardless of its own
           visibility, as in CRuby */
        if (insn == OP_SEND || insn == OP_SEND0 || insn == OP_SENDB) {
          mrb_bool priv = TRUE;
          if (m.flags & MRB_METHOD_PRIVATE_FL) {
          vis_err:;
            mrb_value args = (ci->n == 15) ? regs[1] : mrb_ary_new_from_values(mrb, ci->n, regs+1);
            vis_error(mrb, mid, args, recv, priv);
          }
          /* protected methods are callable when the caller's `self` belongs
             to the class (or module) where the method is defined */
          else if ((m.flags & MRB_METHOD_PROTECTED_FL) &&
                   !mrb_obj_is_kind_of(mrb, ci[-1].stack[0], ci->u.target_class)) {
            priv = FALSE;
            goto vis_err;
          }
        }
      }
      ci->cci = CINFO_NONE;

      if (MRB_METHOD_PROC_P(m)) {
        const struct RProc *p = MRB_METHOD_PROC(m);
        /* handle alias */
        MRB_PROC_RESOLVE_ALIAS(ci, p);
        CI_PROC_SET(ci, p);
        if (MRB_PROC_CFUNC_P(p) && MRB_PROC_ENV_P(p) && !ci->blk && ci->nk == 0) {
          /* attr accessor fast path: access the ivar in place and pop the
             frame instead of doing a full cfunc call. Only for cases that
             cannot raise: reads never do; writes are limited to unfrozen
             plain objects. Arity/frozen errors fall to the normal call. */
          mrb_func_t f = MRB_PROC_CFUNC(p);
          mrb_value name;
          if (f == mrb_attr_reader && ci->n == 0 &&
              mrb_symbol_p(name = MRB_PROC_ENV(p)->stack[0])) {
            mrb_value va = mrb_iv_get(mrb, recv, mrb_symbol(name));
            mrb->c->ci--;       /* fresh frame: no env, no blk */
            ci = mrb->c->ci;
            regs[a] = va;
            NEXT;
          }
          if (f == mrb_attr_writer && ci->n == 1 &&
              mrb_type(recv) == MRB_TT_OBJECT && !mrb_obj_ptr(recv)->frozen &&
              mrb_symbol_p(name = MRB_PROC_ENV(p)->stack[0])) {
            mrb_value va = regs[1];
            mrb_obj_iv_set_force(mrb, mrb_obj_ptr(recv), mrb_symbol(name), va);
            mrb->c->ci--;       /* fresh frame: no env, no blk */
            ci = mrb->c->ci;
            regs[a] = va;
            NEXT;
          }
        }
        if (!MRB_PROC_CFUNC_P(p)) {
          /* setup environment for calling method */
          irep = p->body.irep;
          stack_extend(mrb, (irep->nregs < 4) ? 4 : irep->nregs);
          ci->pc = irep->iseq;
          JUMP;
        }
        else {
          if (MRB_PROC_NOARG_P(p) && (ci->n > 0 || ci->nk > 0)) {
            check_argument_count(mrb, ci, 0);
          }
          recv = MRB_PROC_CFUNC(p)(mrb, recv);
        }
      }
      else {
        check_argument_count(mrb, ci, MRB_MT_ASPEC(m.flags));
        recv = MRB_METHOD_FUNC(m)(mrb, recv);
      }

      /* cfunc epilogue */
      mrb_gc_arena_shrink(mrb, ai);
      if (mrb_unlikely(mrb->exc)) goto L_RAISE;
      ci = mrb->c->ci;
      if (!ci->u.keep_context) { /* return from context modifying method (resume/yield) */
        if (ci->cci == CINFO_RESUMED) {
          mrb->jmp = prev_jmp;
          return recv;
        }
        else {
          mrb_assert(!MRB_PROC_CFUNC_P(ci[-1].proc));
          irep = ci[-1].proc->body.irep;
        }
      }
      mrb_assert(ci > mrb->c->cibase);
      ci->stack[0] = recv;
      /* pop stackpos */
      ci = cipop(mrb);
      JUMP;
    }

    CASE(OP_CALL, Z) {
      const struct RProc *p = mrb_proc_ptr(ci->stack[0]);
      int r = vm_call_proc(mrb, p, ci_bidx(ci)+1, &irep, ai);
      ci = mrb->c->ci;
      if (r == VM_RAISE) goto L_RAISE;
      if (r == VM_RETURN_NIL) { a = 0; goto L_OP_RETURN_BODY; }
      JUMP;
    }

    CASE(OP_BLKCALL, BB) {
      /* Direct block call: R[a] = R[a].call(R[a+1],...,R[a+b]) */
      if (mrb_unlikely(!mrb_proc_p(regs[a]))) {
        mrb_raisef(mrb, E_TYPE_ERROR, "wrong type %T (expected Proc)", regs[a]);
      }
      const struct RProc *p = mrb_proc_ptr(regs[a]);
      /* CINFO_NONE rather than CINFO_DIRECT: the proc runs inside this
         mrb_vm_exec(), so a break crossing this frame is unwound here. The
         unwind throws past any frame it finds carrying another cci, out to a
         C caller that a call from here does not have. */
      ci = cipush(mrb, a, CINFO_NONE, NULL, NULL, NULL, 0, b);
      int r = vm_call_proc(mrb, p, b+1, &irep, ai);
      ci = mrb->c->ci;
      if (r == VM_RAISE) goto L_RAISE;
      if (r == VM_RETURN_NIL) { a = 0; goto L_OP_RETURN_BODY; }
      JUMP;
    }

    CASE(OP_SUPER, BB) {
      mrb_value recv;
      struct RClass* target_class = CI_TARGET_CLASS(ci);

      mid = ci->mid;
      if (mid == 0 || !target_class) {
        RAISE_LIT(mrb, E_NOMETHOD_ERROR, "super called outside of method");
      }
      if ((target_class->flags & MRB_FL_CLASS_IS_PREPENDED) || target_class->tt == MRB_TT_MODULE) {
        goto super_typeerror;
      }
      recv = regs[0];
      if (!mrb_obj_is_kind_of(mrb, recv, target_class)) {
      super_typeerror:
        RAISE_LIT(mrb, E_TYPE_ERROR, "self has wrong type to call super in this context");
      }

      c = b; // arg info
      regs[a] = recv;
      goto L_SENDB_SYM;
    }

    CASE(OP_ARGARY, BS) {
      if (vm_op_argary(mrb, a, b) == VM_RAISE) goto L_RAISE;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_ENTER, W) {
      if (vm_op_enter(mrb, a) == VM_RAISE) goto L_RAISE;
      ci = mrb->c->ci;
      irep = ci->proc->body.irep;
      JUMP;
    }

    CASE(OP_KARG, BB) {
      mrb_value k = mrb_symbol_value(irep->syms[b]);
      mrb_int kidx = mrb_ci_kidx(ci);
      mrb_value kdict, v;

      if (kidx < 0 || !mrb_hash_p(kdict=regs[kidx]) || !mrb_hash_key_p(mrb, kdict, k)) {
        RAISE_FORMAT(mrb, E_ARGUMENT_ERROR, "missing keyword: %v", k);
      }

      v = mrb_hash_delete_key(mrb, kdict, k);
      ci = mrb->c->ci;
      regs[a] = v;
      NEXT;
    }

    CASE(OP_KEY_P, BB) {
      mrb_value k = mrb_symbol_value(irep->syms[b]);
      mrb_int kidx = mrb_ci_kidx(ci);
      mrb_value kdict;
      mrb_bool key_p = FALSE;

      if (kidx >= 0 && mrb_hash_p(kdict=regs[kidx])) {
        key_p = mrb_hash_key_p(mrb, kdict, k);
        ci = mrb->c->ci;
      }
      regs[a] = mrb_bool_value(key_p);
      NEXT;
    }

    CASE(OP_KEYEND, Z) {
      mrb_int kidx = mrb_ci_kidx(ci);
      mrb_value kdict;

      if (kidx >= 0 && mrb_hash_p(kdict=regs[kidx]) && !mrb_hash_empty_p(mrb, kdict)) {
        mrb_value key1 = mrb_hash_first_key(mrb, kdict);
        RAISE_FORMAT(mrb, E_ARGUMENT_ERROR, "unknown keyword: %v", key1);
      }
      NEXT;
    }

    CASE(OP_BREAK, B) {
      if (MRB_PROC_STRICT_P(ci->proc)) goto L_OP_RETURN_BODY;
      if (!MRB_PROC_ORPHAN_P(ci->proc) && MRB_PROC_ENV_P(ci->proc) && ci->proc->e.env->cxt == mrb->c) {
        const struct RProc *dst = ci->proc->upper;
        for (ptrdiff_t i = ci - mrb->c->cibase; i > 0; i--, ci--) {
          if (ci[-1].proc == dst) {
            goto L_UNWINDING;
          }
        }
      }
      RAISE_LIT(mrb, E_LOCALJUMP_ERROR, "break from proc-closure");
      /* not reached */
    }
    CASE(OP_RETURN_BLK, B) {
      if (!MRB_PROC_ENV_P(ci->proc) || MRB_PROC_STRICT_P(ci->proc)) {
        goto L_OP_RETURN_BODY;
      }

      const struct REnv *env = ci->u.env;
      const struct RProc *dst = top_proc(mrb, ci->proc, &env);
      if (!MRB_PROC_ENV_P(dst) || dst->e.env->cxt == mrb->c) {
        /* check jump destination */
        for (ptrdiff_t i = ci - mrb->c->cibase; i >= 0; i--, ci--) {
          if (ci->u.env == env) {
            goto L_UNWINDING;
          }
        }
      }
      /* no jump destination */
      RAISE_LIT(mrb, E_LOCALJUMP_ERROR, "unexpected return");
      /* not reached */
    }
    CASE(OP_RETSELF, Z) {
      a = 0;
      goto L_OP_RETURN_BODY;
    }
    CASE(OP_RETNIL, Z) {
      a = 0;
      goto L_RETURN_NIL;
    }
    CASE(OP_RETTRUE, Z) {
      a = 0;
      goto L_RETURN_TRUE;
    }
    CASE(OP_RETFALSE, Z) {
      a = 0;
      goto L_RETURN_FALSE;
    }
    CASE(OP_RETURN, B) {
      mrb_int acc;
      mrb_value v;
      mrb_callinfo *return_ci;

      v = regs[a];
      goto L_RETURN;
    L_RETURN_NIL:
      v = mrb_nil_value();
      goto L_RETURN;
    L_RETURN_TRUE:
      v = mrb_true_value();
      goto L_RETURN;
    L_RETURN_FALSE:
      v = mrb_false_value();
    L_RETURN:
      /* cipop below may allocate (env unshare), and the returning frame's
         slots are no longer scanned after the pop, so keep a heap return
         value in the arena; immediates need no protection and skipping the
         call matters on integer-heavy return paths */
      if (!mrb_immediate_p(v)) mrb_gc_protect(mrb, v);
      return_ci = ci;
      CHECKPOINT_RESTORE(RBREAK_TAG_BREAK) {
        if (TRUE) {
          struct RBreak *brk = (struct RBreak*)mrb->exc;
          return_ci = &mrb->c->cibase[brk->ci_break_index];
          v = mrb_break_value_get(brk);
        }
        else {
        L_UNWINDING:
          return_ci = ci;
          ci = mrb->c->ci;
          v = ci->stack[a];
        }
        if (!mrb_immediate_p(v)) mrb_gc_protect(mrb, v);
      }
      CHECKPOINT_MAIN(RBREAK_TAG_BREAK) {
        for (;;) {
          UNWIND_ENSURE(mrb, ci, ci->pc, RBREAK_TAG_BREAK, return_ci, v);

          if (ci == return_ci) {
            break;
          }
          ci = cipop(mrb);
          if (ci[1].cci != CINFO_NONE) {
            mrb_assert(prev_jmp != NULL);
            mrb->exc = (struct RObject*)break_new(mrb, RBREAK_TAG_BREAK, return_ci, v);
            mrb_gc_arena_restore(mrb, ai);
            mrb->c->vmexec = FALSE;
            mrb->jmp = prev_jmp;
            MRB_THROW(prev_jmp);
          }
        }
      }
      CHECKPOINT_END(RBREAK_TAG_BREAK);
      mrb->exc = NULL; /* clear break object */

      if (ci == mrb->c->cibase) {
        struct mrb_context *c = mrb->c;
        if (c == mrb->root_c) {
          /* toplevel return */
          mrb_gc_arena_restore(mrb, ai);
          mrb->jmp = prev_jmp;
          return v;
        }

#ifdef MRB_USE_TASK_SCHEDULER
        if (mrb->c->status == MRB_TASK_CREATED) {
          mrb_gc_arena_restore(mrb, ai);
          mrb->jmp = prev_jmp;
          TASK_STOP(mrb);
          return v;
        }
#endif

        fiber_terminate(mrb, c, ci);
        if (c->vmexec ||
            (mrb->c == mrb->root_c && mrb->c->ci == mrb->c->cibase) /* case using Fiber#transfer in mrb_fiber_resume() */) {
          mrb_gc_arena_restore(mrb, ai);
          c->vmexec = FALSE;
          mrb->jmp = prev_jmp;
          return v;
        }
        ci = mrb->c->ci;
      }

      if (mrb->c->vmexec && !ci->u.keep_context) {
        mrb_gc_arena_restore(mrb, ai);
        mrb->c->vmexec = FALSE;
        mrb->jmp = prev_jmp;
        return v;
      }
      acc = ci->cci;
      ci = cipop(mrb);
      if (acc == CINFO_SKIP || acc == CINFO_DIRECT) {
        mrb_gc_arena_restore(mrb, ai);
        mrb->jmp = prev_jmp;
        return v;
      }
      DEBUG(fprintf(stderr, "from :%s\n", mrb_sym_name(mrb, ci->mid)));
      irep = ci->proc->body.irep;

      ci[1].stack[0] = v;
      mrb_gc_arena_restore(mrb, ai);
      JUMP;
    }

    CASE(OP_BLKPUSH, BS) {
      if (vm_op_blkpush(mrb, a, b) == VM_RAISE) goto L_RAISE;
      NEXT;
    }

#if !defined(MRB_USE_BIGINT) || defined(MRB_INT32)
  L_INT_OVERFLOW:
    RAISE_LIT(mrb, E_RANGE_ERROR, "integer overflow");
#endif

#define OP_MATH(op_name) do {                                               \
  /* need to check if op is overridden */                                   \
  uint16_t tt = TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]));              \
  if (mrb_likely(tt == TYPES2(MRB_TT_INTEGER, MRB_TT_INTEGER))) {           \
    mrb_int x = mrb_integer(regs[a]), y = mrb_integer(regs[a+1]), z;        \
    if (mrb_int_##op_name##_overflow(x, y, &z)) {                           \
      OP_MATH_OVERFLOW_INT(op_name,x,y);                                    \
    }                                                                       \
    else                                                                    \
      VM_SET_INT_VALUE(regs[a], z);                                         \
  }                                                                         \
  else switch (tt) {                                                        \
    OP_MATH_CASE_FLOAT(op_name, integer, float);                            \
    OP_MATH_CASE_FLOAT(op_name, float,  integer);                           \
    OP_MATH_CASE_FLOAT(op_name, float,  float);                             \
    OP_MATH_CASE_STRING_##op_name();                                        \
    default:                                                                \
      mid = MRB_OPSYM(op_name);                                             \
      goto L_SEND_SYM;                                                      \
  }                                                                         \
} while(0);                                                                 \
  NEXT;
#ifdef MRB_NO_FLOAT
#define OP_MATH_CASE_FLOAT(op_name, t1, t2) (void)0
#else
#define OP_MATH_CASE_FLOAT(op_name, t1, t2)                                     \
  case TYPES2(OP_MATH_TT_##t1, OP_MATH_TT_##t2):                                \
    {                                                                           \
      mrb_float z = mrb_##t1(regs[a]) OP_MATH_OP_##op_name mrb_##t2(regs[a+1]); \
      VM_SET_FLOAT_VALUE(regs[a], z);                                           \
    }                                                                           \
    break
#endif
#ifdef MRB_USE_BIGINT
#define OP_MATH_OVERFLOW_INT(op,x,y) do {                                   \
  regs[a] = mrb_bint_##op##_ii(mrb,x,y);                                    \
  mrb_gc_arena_restore(mrb, ai);                                            \
} while (0)
#else
#define OP_MATH_OVERFLOW_INT(op,x,y) goto L_INT_OVERFLOW
#endif
#define OP_MATH_CASE_STRING_add()                                           \
  case TYPES2(MRB_TT_STRING, MRB_TT_STRING):                                \
    regs[a] = mrb_str_plus(mrb, regs[a], regs[a+1]);                        \
    mrb_gc_arena_restore(mrb, ai);                                          \
    break
#define OP_MATH_CASE_STRING_sub() (void)0
#define OP_MATH_CASE_STRING_mul() (void)0
#define OP_MATH_OP_add +
#define OP_MATH_OP_sub -
#define OP_MATH_OP_mul *
#define OP_MATH_TT_integer MRB_TT_INTEGER
#define OP_MATH_TT_float   MRB_TT_FLOAT

    CASE(OP_ADD, B) {
      OP_MATH(add);
    }

    CASE(OP_SUB, B) {
      OP_MATH(sub);
    }

    CASE(OP_MUL, B) {
      OP_MATH(mul);
    }

    CASE(OP_DIV, B) {
      int r = vm_op_div(mrb, a, &mid);
      ci = mrb->c->ci;
      if (r == VM_SEND_SYM) goto L_SEND_SYM;
      NEXT;
    }

#define OP_MATHI(op_name) do {                                              \
  /* need to check if op is overridden */                                   \
  if (mrb_likely(mrb_integer_p(regs[a]))) {                                 \
    mrb_int x = mrb_integer(regs[a]), y = (mrb_int)b, z;                    \
    if (mrb_int_##op_name##_overflow(x, y, &z)) {                           \
      OP_MATH_OVERFLOW_INT(op_name,x,y);                                    \
    }                                                                       \
    else                                                                    \
      VM_SET_INT_VALUE(regs[a], z);                                         \
  }                                                                         \
  else switch (mrb_type(regs[a])) {                                         \
    OP_MATHI_CASE_FLOAT(op_name);                                           \
    default:                                                                \
      SET_INT_VALUE(mrb,regs[a+1], b);                                      \
      mid = MRB_OPSYM(op_name);                                             \
      goto L_SEND_SYM;                                                      \
  }                                                                         \
} while(0);                                                                 \
  NEXT;
#ifdef MRB_NO_FLOAT
#define OP_MATHI_CASE_FLOAT(op_name) (void)0
#else
#define OP_MATHI_CASE_FLOAT(op_name)                                        \
  case MRB_TT_FLOAT:                                                        \
    {                                                                       \
      mrb_float z = mrb_float(regs[a]) OP_MATH_OP_##op_name b;              \
      VM_SET_FLOAT_VALUE(regs[a], z);                                       \
    }                                                                       \
    break
#endif

    CASE(OP_ADDI, BB) {
      OP_MATHI(add);
    }

    CASE(OP_SUBI, BB) {
      OP_MATHI(sub);
    }

#ifdef MRB_NO_FLOAT
#define OP_MATHILV_CASE_FLOAT(op_name) (void)0
#else
#define OP_MATHILV_CASE_FLOAT(op_name)                                      \
  case MRB_TT_FLOAT:                                                        \
    {                                                                       \
      mrb_float z = mrb_float(regs[a]) OP_MATH_OP_##op_name c;              \
      VM_SET_FLOAT_VALUE(regs[a], z);                                       \
    }                                                                       \
    break
#endif
#define OP_MATHILV(op_name)                                                 \
  /* a=local, b=working space, c=immediate */                               \
  switch (mrb_type(regs[a])) {                                              \
    case MRB_TT_INTEGER:                                                    \
      {                                                                     \
        mrb_int x = mrb_integer(regs[a]), y = (mrb_int)c, z;                \
        if (mrb_int_##op_name##_overflow(x, y, &z)) {                       \
          OP_MATH_OVERFLOW_INT(op_name,x,y);                                \
        }                                                                   \
        else {                                                              \
          VM_SET_INT_VALUE(regs[a], z);                                     \
        }                                                                   \
      }                                                                     \
      break;                                                                \
    OP_MATHILV_CASE_FLOAT(op_name);                                         \
    default:                                                                \
      /* `a` is a local variable slot, not a temporary, so the L_SEND_SYM   \
         path the other OP_MATH opcodes take cannot be used: it writes the  \
         argument into regs[a+1] and lays the callee frame over the locals  \
         from regs[a] on. `b` is the working space reserved for the call,   \
         but a send set up there leaves its result in regs[b] and the       \
         `MOVE local, temp` that used to copy it back is what the fusion    \
         removed, so the call is made from C. It can move the stack, hence  \
         the `ci` refresh before storing through `regs`. */                 \
      {                                                                     \
        int ai_ = mrb_gc_arena_save(mrb);                                   \
        mrb_value arg_ = mrb_int_value(mrb, c);                             \
        mrb_value v_ = mrb_funcall_argv(mrb, regs[a],                       \
                                        MRB_OPSYM(op_name), 1, &arg_);      \
        ci = mrb->c->ci;                                                    \
        regs[a] = v_;                                                       \
        mrb_gc_arena_restore(mrb, ai_);                                     \
      }                                                                     \
      break;                                                                \
  }                                                                         \
  NEXT

    CASE(OP_ADDILV, BBB) {
      OP_MATHILV(add);
    }

    CASE(OP_SUBILV, BBB) {
      OP_MATHILV(sub);
    }

#define OP_CMP_BODY(op,v1,v2) (v1(regs[a]) op v2(regs[a+1]))

#ifdef MRB_NO_FLOAT
#define value_nan_p(v) FALSE
#define OP_CMP(op,sym) do {\
  int result;\
  /* need to check if op is overridden */\
  if (mrb_likely(TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1])) == \
                 TYPES2(MRB_TT_INTEGER,MRB_TT_INTEGER))) {\
    result = OP_CMP_BODY(op,mrb_integer,mrb_integer);\
  }\
  else {\
    mid = MRB_OPSYM(sym);\
    goto L_SEND_SYM;\
  }\
  if (result) {\
    SET_TRUE_VALUE(regs[a]);\
  }\
  else {\
    SET_FALSE_VALUE(regs[a]);\
  }\
} while(0)
#else
#define value_nan_p(v) (mrb_float_p(v) && isnan(mrb_float(v)))
/* The usual arithmetic conversions would round an `mrb_int` wider than the
   significand onto a neighbouring Float, so a mixed pair asks
   `mrb_int_float_cmp()` for the order of the two values themselves. It answers
   -2 for a NaN operand, which stands in no order with anything: every operator
   is false against it, as comparing the two as Floats would have been. */
#define OP_CMP_INT_FLOAT(op) do {\
  mrb_int c = mrb_int_float_cmp(mrb_integer(regs[a]), mrb_float(regs[a+1]));\
  result = c != -2 && (c op 0);\
} while (0)
#define OP_CMP_FLOAT_INT(op) do {\
  mrb_int c = mrb_int_float_cmp(mrb_integer(regs[a+1]), mrb_float(regs[a]));\
  result = c != -2 && (0 op c);\
} while (0)
#define OP_CMP(op, sym) do {\
  int result;\
  /* need to check if op is overridden */\
  uint16_t tt = TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]));\
  if (mrb_likely(tt == TYPES2(MRB_TT_INTEGER,MRB_TT_INTEGER))) {\
    result = OP_CMP_BODY(op,mrb_integer,mrb_integer);\
  }\
  else switch (tt) {\
  case TYPES2(MRB_TT_INTEGER,MRB_TT_FLOAT):\
    OP_CMP_INT_FLOAT(op);\
    break;\
  case TYPES2(MRB_TT_FLOAT,MRB_TT_INTEGER):\
    OP_CMP_FLOAT_INT(op);\
    break;\
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):\
    result = OP_CMP_BODY(op,mrb_float,mrb_float);\
    break;\
  default:\
    mid = MRB_OPSYM(sym);\
    goto L_SEND_SYM;\
  }\
  if (result) {\
    SET_TRUE_VALUE(regs[a]);\
  }\
  else {\
    SET_FALSE_VALUE(regs[a]);\
  }\
} while(0)
#endif

    CASE(OP_EQ, B) {
      /* Two values that hold the same thing are the same object, and an object
         is equal to itself. A NaN is the one value that holds for neither: it
         is equal to nothing at all, its own operand included. What
         `mrb_obj_eq()` reads is the representation, and a boxing that carries
         a Float in an `mrb_value` gives a NaN the same one every time, so
         without the test below `Float::NAN == Float::NAN` answered true in a
         boxed build and false in `MRB_NO_BOXING`. The pair falls through to
         the comparison instead, which reads it as numbers and answers false
         in either. */
      if (mrb_obj_eq(mrb, regs[a], regs[a+1]) && !value_nan_p(regs[a])) {
        SET_TRUE_VALUE(regs[a]);
      }
      else if (mrb_symbol_p(regs[a])) {
        SET_FALSE_VALUE(regs[a]);
      }
      else {
        OP_CMP(==,eq);
      }
      NEXT;
    }

    CASE(OP_LT, B) {
      OP_CMP(<,lt);
      NEXT;
    }

    CASE(OP_LE, B) {
      OP_CMP(<=,le);
      NEXT;
    }

    CASE(OP_GT, B) {
      OP_CMP(>,gt);
      NEXT;
    }

    CASE(OP_GE, B) {
      OP_CMP(>=,ge);
      NEXT;
    }

    CASE(OP_ARRAY, BB) {
      regs[a] = ary_new_from_regs(mrb, b, a);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }
    CASE(OP_ARRAY2, BBB) {
      regs[a] = ary_new_from_regs(mrb, c, b);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_ARYCAT, B) {
      mrb_value v = regs[a+1];
      if (mrb_nil_p(regs[a])) {
        /* becomes the argument accumulator, which OP_ARYPUSH/ARYCAT then
           append to, so it must be a fresh array independent of v.
           mrb_ary_splat() can call back into the VM (`to_a`) and move the
           stack, so take the result first and store it through the refreshed
           `regs`: the address of regs[a] is otherwise computed before the
           call and would point into the freed buffer. */
        mrb_value splat = mrb_ary_splat(mrb, v);
        ci = mrb->c->ci;
        regs[a] = splat;
      }
      else if (mrb_array_p(v)) {
        /* concat only reads v, so splat here would just dup v and copy it
           twice; concatenate straight from v (ary_concat handles v aliasing
           regs[a]) */
        mrb_ensure_array_type(mrb, regs[a]);
        mrb_ary_concat(mrb, regs[a], v);
      }
      else {
        /* non-array: to_a already yields a fresh array, no extra dup needed */
        mrb_value splat = mrb_ary_splat(mrb, v);
        ci = mrb->c->ci;
        mrb_ensure_array_type(mrb, regs[a]);
        mrb_ary_concat(mrb, regs[a], splat);
      }
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_ARYPUSH, BB) {
      mrb_ensure_array_type(mrb, regs[a]);
      for (mrb_int i=0; i<b; i++) {
        mrb_ary_push(mrb, regs[a], regs[a+i+1]);
      }
      NEXT;
    }

    CASE(OP_ARYSPLAT, B) {
      mrb_value ary = mrb_ary_splat(mrb, regs[a]);
      ci = mrb->c->ci;
      regs[a] = ary;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_AREF, BBB) {
      mrb_value v = regs[b];

      if (!mrb_array_p(v)) {
        if (c == 0) {
          regs[a] = v;
        }
        else {
          SET_NIL_VALUE(regs[a]);
        }
      }
      else {
        v = mrb_ary_ref(mrb, v, c);
        regs[a] = v;
      }
      NEXT;
    }

    CASE(OP_ASET, BBB) {
      mrb_ensure_array_type(mrb, regs[b]);
      mrb_ary_set(mrb, regs[b], c, regs[a]);
      NEXT;
    }

    CASE(OP_APOST, BBB) {
      mrb_value v = regs[a];
      int pre  = b;
      int post = c;

      if (!mrb_array_p(v)) {
        v = ary_new_from_regs(mrb, 1, a);
      }
      struct RArray *ary = mrb_ary_ptr(v);
      int len = (int)ARY_LEN(ary);
      if (len > pre + post) {
        v = mrb_ary_new_from_values(mrb, len - pre - post, ARY_PTR(ary)+pre);
        regs[a++] = v;
        while (post--) {
          regs[a++] = ARY_PTR(ary)[len-post-1];
        }
      }
      else {
        v = mrb_ary_new_capa(mrb, 0);
        regs[a++] = v;

        int idx;
        for (idx=0; idx+pre<len; idx++) {
          regs[a+idx] = ARY_PTR(ary)[pre+idx];
        }
        while (idx < post) {
          SET_NIL_VALUE(regs[a+idx]);
          idx++;
        }
      }
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_INTERN, B) {
      mrb_ensure_string_type(mrb, regs[a]);
      mrb_sym sym = mrb_intern_str(mrb, regs[a]);
      regs[a] = mrb_symbol_value(sym);
      NEXT;
    }

    CASE(OP_SYMBOL, BB) {
      size_t len;
      mrb_sym sym;

      mrb_assert((irep->pool[b].tt&IREP_TT_NFLAG)==0);
      len = irep->pool[b].tt >> 2;
      if (irep->pool[b].tt & IREP_TT_SFLAG) {
        sym = mrb_intern_static(mrb, irep->pool[b].u.str, len);
      }
      else {
        sym = mrb_intern(mrb, irep->pool[b].u.str, len);
      }
      regs[a] = mrb_symbol_value(sym);
      NEXT;
    }

    CASE(OP_STRING, BB) {
      mrb_int len;

      mrb_assert((irep->pool[b].tt&IREP_TT_NFLAG)==0);
      len = irep->pool[b].tt >> 2;
      if (irep->pool[b].tt & IREP_TT_SFLAG) {
        regs[a] = mrb_str_new_static(mrb, irep->pool[b].u.str, len);
      }
      else {
        regs[a] = mrb_str_new(mrb, irep->pool[b].u.str, len);
      }
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_STRCAT, B) {
      mrb_ensure_string_type(mrb, regs[a]);
      mrb_str_concat(mrb, regs[a], regs[a+1]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_HASH, BB) {
      mrb_value hash = mrb_hash_new_capa(mrb, b);
      int lim = a+b*2;

      for (int i=a; i<lim; i+=2) {
        mrb_hash_set(mrb, hash, regs[i], regs[i+1]);
        ci = mrb->c->ci;
      }
      regs[a] = hash;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_HASHADD, BB) {
      mrb_value hash;
      int lim = a+b*2+1;

      hash = regs[a];
      mrb_ensure_hash_type(mrb, hash);
      for (int i=a+1; i<lim; i+=2) {
        mrb_hash_set(mrb, hash, regs[i], regs[i+1]);
        ci = mrb->c->ci;
      }
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }
    CASE(OP_HASHCAT, B) {
      mrb_value hash = regs[a];

      mrb_ensure_hash_type(mrb, hash);
      mrb_hash_merge(mrb, hash, regs[a+1]);
      ci = mrb->c->ci;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_LAMBDA, BB)
    c = OP_L_LAMBDA;
    L_MAKE_LAMBDA:
    {
      struct RProc *p;
      const mrb_irep *nirep = irep->reps[b];

      if (c & OP_L_CAPTURE) {
        p = mrb_closure_new(mrb, nirep);
      }
      else {
        p = mrb_proc_new(mrb, nirep);
        p->flags |= MRB_PROC_SCOPE;
      }
      if (c & OP_L_STRICT) p->flags |= MRB_PROC_STRICT;
      regs[a] = mrb_obj_value(p);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }
    CASE(OP_BLOCK, BB) {
      c = OP_L_BLOCK;
      goto L_MAKE_LAMBDA;
    }
    CASE(OP_METHOD, BB) {
      c = OP_L_METHOD;
      goto L_MAKE_LAMBDA;
    }

    CASE(OP_RANGE_INC, B) {
      mrb_value v = mrb_range_new(mrb, regs[a], regs[a+1], FALSE);
      ci = mrb->c->ci;
      regs[a] = v;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_RANGE_EXC, B) {
      mrb_value v = mrb_range_new(mrb, regs[a], regs[a+1], TRUE);
      ci = mrb->c->ci;
      regs[a] = v;
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_OCLASS, B) {
      regs[a] = mrb_obj_value(mrb->object_class);
      NEXT;
    }

    CASE(OP_CLASS, BB) {
      struct RClass *c = 0, *baseclass;
      mrb_sym id = irep->syms[b];
      mrb_value base = regs[a];
      mrb_value super = regs[a+1];

      if (mrb_nil_p(base)) {
        baseclass = MRB_PROC_TARGET_CLASS(ci->proc);
        if (!baseclass) baseclass = mrb->object_class;
        base = mrb_obj_value(baseclass);
      }
      c = mrb_vm_define_class(mrb, base, super, id);
      ci = mrb->c->ci;
      regs[a] = mrb_obj_value(c);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_MODULE, BB) {
      struct RClass *cls = 0, *baseclass;
      mrb_sym id = irep->syms[b];
      mrb_value base = regs[a];

      if (mrb_nil_p(base)) {
        baseclass = MRB_PROC_TARGET_CLASS(ci->proc);
        if (!baseclass) baseclass = mrb->object_class;
        base = mrb_obj_value(baseclass);
      }
      cls = mrb_vm_define_module(mrb, base, id);
      ci = mrb->c->ci;
      regs[a] = mrb_obj_value(cls);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_EXEC, BB)
    {
      mrb_value recv = regs[a];
      struct RClass *c = mrb_class_ptr(recv);
      const mrb_irep *nirep = irep->reps[b];

      /* prepare closure */
      struct RProc *p = mrb_proc_new(mrb, nirep);
      p->c = NULL;
      mrb_field_write_barrier(mrb, (struct RBasic*)p, (struct RBasic*)ci->proc);
      MRB_PROC_SET_TARGET_CLASS(p, c);
      p->flags |= MRB_PROC_SCOPE;

      /* prepare call stack */
      ci = cipush(mrb, a, 0, c, p, NULL, 0, 0);

      irep = p->body.irep;
      stack_extend(mrb, irep->nregs);
      stack_clear(regs+1, irep->nregs-1);
      ci->pc = irep->iseq;
      JUMP;
    }

    CASE(OP_DEF, BB) {
      struct RClass *target = mrb_class_ptr(regs[a]);
      const struct RProc *p = mrb_proc_ptr(regs[a+1]);
      mrb_method_t m;
      mrb_sym mid = irep->syms[b];

      MRB_METHOD_FROM_PROC(m, p);
      MRB_METHOD_SET_VISIBILITY(m, MRB_METHOD_VDEFAULT_FL);
      mrb_define_method_raw(mrb, target, mid, m);
      mrb_method_added(mrb, target, mid);
      ci = mrb->c->ci;
      mrb_gc_arena_restore(mrb, ai);
      regs[a] = mrb_symbol_value(mid);
      NEXT;
    }

    CASE(OP_TDEF, BBB) {
      struct RClass *tc = check_target_class(mrb);
      if (mrb_unlikely(!tc)) goto L_RAISE;
      mid = vm_define_method(mrb, tc, irep, b, c);
      ci = mrb->c->ci;
      mrb_gc_arena_restore(mrb, ai);
      regs[a] = mrb_symbol_value(mid);
      NEXT;
    }

    CASE(OP_SDEF, BBB) {
      struct RClass *tc = mrb_class_ptr(mrb_singleton_class(mrb, regs[a]));
      mid = vm_define_method(mrb, tc, irep, b, c);
      ci = mrb->c->ci;
      mrb_gc_arena_restore(mrb, ai);
      regs[a] = mrb_symbol_value(mid);
      NEXT;
    }

    CASE(OP_SCLASS, B) {
      regs[a] = mrb_singleton_class(mrb, regs[a]);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_TCLASS, B) {
      struct RClass *target = check_target_class(mrb);
      if (mrb_unlikely(!target)) goto L_RAISE;
      regs[a] = mrb_obj_value(target);
      NEXT;
    }

    CASE(OP_ALIAS, BB) {
      struct RClass *target = check_target_class(mrb);

      if (mrb_unlikely(!target)) goto L_RAISE;
      mrb_alias_method(mrb, target, irep->syms[a], irep->syms[b]);
      mrb_method_added(mrb, target, irep->syms[a]);
      ci = mrb->c->ci;
      NEXT;
    }
    CASE(OP_UNDEF, B) {
      struct RClass *target = check_target_class(mrb);

      if (mrb_unlikely(!target)) goto L_RAISE;
      mrb_undef_method_id(mrb, target, irep->syms[a]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_DEBUG, BBB) {
#ifdef MRB_USE_DEBUG_HOOK
      if (mrb->debug_op_hook) mrb->debug_op_hook(mrb, irep, ci->pc, regs);
#else
#ifndef MRB_NO_STDIO
      printf("OP_DEBUG %d %d %d\n", a, b, c);
#else
      abort();
#endif
#endif
      NEXT;
    }

    CASE(OP_ERR, B) {
      size_t len = irep->pool[a].tt >> 2;
      mrb_value exc;

      mrb_assert((irep->pool[a].tt&IREP_TT_NFLAG)==0);
      exc = mrb_exc_new(mrb, E_LOCALJUMP_ERROR, irep->pool[a].u.str, len);
      RAISE_EXC(mrb, exc);
    }

    CASE(OP_EXT1, Z) {
      const mrb_code *pc = ci->pc;
      insn = READ_B();
      switch (insn) {
#define OPCODE(insn,ops) case OP_ ## insn: FETCH_ ## ops ## _1(); ci->pc = pc; goto L_OP_ ## insn ## _BODY;
#include <mruby/ops.h>
#undef OPCODE
      }
      NEXT;
    }
    CASE(OP_EXT2, Z) {
      const mrb_code *pc = ci->pc;
      insn = READ_B();
      switch (insn) {
#define OPCODE(insn,ops) case OP_ ## insn: FETCH_ ## ops ## _2(); ci->pc = pc; goto L_OP_ ## insn ## _BODY;
#include <mruby/ops.h>
#undef OPCODE
      }
      NEXT;
    }
    CASE(OP_EXT3, Z) {
      const mrb_code *pc = ci->pc;
      insn = READ_B();
      switch (insn) {
#define OPCODE(insn,ops) case OP_ ## insn: FETCH_ ## ops ## _3(); ci->pc = pc; goto L_OP_ ## insn ## _BODY;
#include <mruby/ops.h>
#undef OPCODE
      }
      NEXT;
    }

    CASE(OP_STOP, Z) {
      /*        stop VM */
      mrb_value v;
      v = mrb->exc ? mrb_obj_value(mrb->exc) : mrb_nil_value();
      CHECKPOINT_RESTORE(RBREAK_TAG_STOP) {
        struct RBreak *brk = (struct RBreak*)mrb->exc;
        v = mrb_break_value_get(brk);
      }
      CHECKPOINT_MAIN(RBREAK_TAG_STOP) {
        UNWIND_ENSURE(mrb, ci, ci->pc, RBREAK_TAG_STOP, ci, v);
      }
      CHECKPOINT_END(RBREAK_TAG_STOP);
      mrb->jmp = prev_jmp;
      if (!mrb_nil_p(v)) {
        mrb->exc = mrb_obj_ptr(v);
        TASK_STOP(mrb);
        return v;
      }
      mrb->exc = NULL;
      TASK_STOP(mrb);
      return regs[irep->nlocals];
    }
  }
  END_DISPATCH;
#undef regs
  }
  MRB_CATCH(&c_jmp) {
    mrb_assert(mrb->exc != NULL);

    ci = mrb->c->ci;
    while (ci > mrb->c->cibase && ci->cci == CINFO_DIRECT) {
      ci = cipop(mrb);
    }
    goto RETRY_TRY_BLOCK;
  }
  MRB_END_EXC(&c_jmp);
  /* not reached */
  return mrb_nil_value();
}

static mrb_value
mrb_run(mrb_state *mrb, const struct RProc *proc, mrb_value self)
{
  return mrb_vm_run(mrb, proc, self, ci_bidx(mrb->c->ci) + 1);
}

/**
 * @brief Executes a mruby proc in the top-level environment.
 *
 * This function is used to execute a proc (like a script loaded from a file
 * or a string) at the top level of the mruby environment. It's similar to
 * `mrb_vm_run` but is specifically designed for top-level execution.
 *
 * It ensures that if there's an existing callinfo stack, the new execution
 * is pushed on top with `CINFO_SKIP`, indicating it's a new, distinct
 * execution context rather than a nested call from within the VM.
 *
 * @param mrb The mruby state.
 * @param proc The RProc object (representing the script or code) to execute.
 * @param self The `self` object for this top-level execution. Typically,
 *             this is the main `top_self` object in mruby.
 * @param stack_keep The number of values to preserve on the stack. For
 *                   top-level execution, this is often 0 or a small number
 *                   to set up initial local variables if any.
 * @return The result of the proc's execution.
 * @see mrb_vm_run
 */
MRB_API mrb_value
mrb_top_run(mrb_state *mrb, const struct RProc *proc, mrb_value self, mrb_int stack_keep)
{
  if (mrb->c->cibase && mrb->c->ci > mrb->c->cibase) {
    cipush(mrb, 0, CINFO_SKIP, mrb->object_class, NULL, NULL, 0, 0);
  }
  return mrb_vm_run(mrb, proc, self, stack_keep);
}
#undef CASE
