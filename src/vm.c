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
#include <mruby/presym.h>

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
static void
mrb_gc_arena_shrink(mrb_state *mrb, int idx)
{
  mrb_gc *gc = &mrb->gc;
  int capa = gc->arena_capa;

  gc->arena_idx = idx;
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
  c->ci->vis = MRB_METHOD_PRIVATE_FL;
}

static inline void
envadjust(mrb_state *mrb, mrb_value *oldbase, mrb_value *newbase)
{
  mrb_callinfo *ci = mrb->c->cibase;
  ptrdiff_t delta = newbase - oldbase;

  if (delta == 0) return;
  while (ci <= mrb->c->ci) {
    struct REnv *e = mrb_vm_ci_env(ci);

    if (e) {
      mrb_assert(e->cxt == mrb->c && MRB_ENV_ONSTACK_P(e));
      mrb_assert(e->stack == ci->stack);

      e->stack += delta;
    }
    ci->stack += delta;
    ci++;
  }
}

/** def rec; $deep =+ 1; if $deep > 1000; return 0; end; rec; end **/

static void
stack_extend_alloc(mrb_state *mrb, mrb_int room)
{
  mrb_value *oldbase = mrb->c->stbase;
  size_t oldsize = mrb->c->stend - mrb->c->stbase;
  size_t size = oldsize;
  size_t off = mrb->c->ci->stack ? mrb->c->stend - mrb->c->ci->stack : 0;

  if (off > size) size = off;
#ifdef MRB_STACK_EXTEND_DOUBLING
  if ((size_t)room <= size)
    size *= 2;
  else
    size += room;
#else
  /* Use linear stack growth.
     It is slightly slower than doubling the stack space,
     but it saves memory on small devices. */
  if (room <= MRB_STACK_GROWTH)
    size += MRB_STACK_GROWTH;
  else
    size += room;
#endif

  mrb_value *newstack = (mrb_value*)mrb_realloc(mrb, mrb->c->stbase, sizeof(mrb_value) * size);
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
  if (!mrb->c->ci->stack || mrb->c->ci->stack + room >= mrb->c->stend) {
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
  mrb_assert(!p || !MRB_PROC_ALIAS_P(p));\
  ci->pc = (p && !MRB_PROC_CFUNC_P(p) && p->body.irep) ? p->body.irep->iseq : NULL;\
} while (0)

void
mrb_vm_ci_proc_set(mrb_callinfo *ci, const struct RProc *p)
{
  CI_PROC_SET(ci, p);
}

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
    mrb_env_unshare(mrb, e, FALSE);
  }
}

#define CINFO_NONE    0 // called method from mruby VM (without C functions)
#define CINFO_SKIP    1 // ignited mruby VM from C
#define CINFO_DIRECT  2 // called method from C
#define CINFO_RESUMED 3 // resumed by `Fiber.yield` (probably the main call is `mrb_fiber_resume()`)

#define BLK_PTR(b) ((mrb_proc_p(b)) ? mrb_proc_ptr(b) : NULL)

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
    c->cibase = (mrb_callinfo*)mrb_realloc(mrb, c->cibase, sizeof(mrb_callinfo)*size*2);
    c->ci = ci = c->cibase + size;
    c->ciend = c->cibase + size * 2;
  }
  ci->mid = mid;
  CI_PROC_SET(ci, proc);
  ci->blk = blk;
  ci->stack = ci[-1].stack + push_stacks;
  ci->n = argc & 0xf;
  ci->nk = (argc>>4) & 0xf;
  ci->cci = cci;
  ci->vis = MRB_METHOD_PUBLIC_FL;
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
      mrb_free(mrb, stack);
    }
    else {
      mrb_assert(stack == env->stack);
      mrb_write_barrier(mrb, (struct RBasic*)env);

      // don't call MRB_ENV_CLOSE() before mrb_realloc().
      // the reason is that env->stack may be freed by mrb_realloc() if MRB_DEBUG + MRB_GC_STRESS are enabled.
      // realloc() on a freed heap will cause double-free.

      stack = (mrb_value*)mrb_realloc(mrb, stack, len * sizeof(mrb_value));
      if (mrb_object_dead_p(mrb, (struct RBasic*)env)) {
        mrb_free(mrb, stack);
      }
      else {
        env->stack = stack;
        MRB_ENV_CLOSE(env);
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
    mrb_write_barrier(mrb, (struct RBasic*)e);
    return TRUE;
  }
  else {
    e->stack = NULL;
    MRB_ENV_CLOSE(e);
    MRB_ENV_SET_LEN(e, 0);
    MRB_ENV_SET_BIDX(e, 0);
    if (!noraise) {
      mrb_exc_raise(mrb, mrb_obj_value(mrb->nomem_err));
    }
    return FALSE;
  }
}

static inline mrb_callinfo*
cipop(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;
  mrb_callinfo *ci = c->ci;
  struct REnv *env = CI_ENV(ci);

  ci_env_set(ci, NULL); // make possible to free env by GC if not needed
  struct RProc *b = ci->blk;
  if (b && !MRB_PROC_STRICT_P(b) && MRB_PROC_ENV(b) == CI_ENV(&ci[-1])) {
    b->flags |= MRB_PROC_ORPHAN;
  }
  if (env && !mrb_env_unshare(mrb, env, TRUE)) {
    c->ci--; // exceptions are handled at the method caller; see #3087
    mrb_exc_raise(mrb, mrb_obj_value(mrb->nomem_err));
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
      if (MRB_PROC_ALIAS_P(ci->proc)) {
        ci->mid = ci->proc->body.mid;
        ci->proc = ci->proc->upper;
      }
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
check_method_noarg(mrb_state *mrb, const mrb_callinfo *ci)
{
  mrb_int argc = ci->n == CALL_MAXARGS ? RARRAY_LEN(ci->stack[1]) : ci->n;
  if (ci->nk > 0) {
    mrb_value kdict = ci->stack[mrb_ci_kidx(ci)];
    if (!(mrb_hash_p(kdict) && mrb_hash_empty_p(mrb, kdict))) {
      argc++;
    }
  }
  if (argc > 0) {
    mrb_argnum_error(mrb, argc, 0, 0);
  }
}

static mrb_value
exec_irep(mrb_state *mrb, mrb_value self, const struct RProc *p)
{
  mrb_callinfo *ci = mrb->c->ci;

  ci->stack[0] = self;
  /* handle alias */
  if (MRB_PROC_ALIAS_P(p)) {
    ci->mid = p->body.mid;
    p = p->upper;
  }
  CI_PROC_SET(ci, p);
  if (MRB_PROC_CFUNC_P(p)) {
    if (MRB_PROC_NOARG_P(p) && (ci->n > 0 || ci->nk > 0)) {
      check_method_noarg(mrb, ci);
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
        check_method_noarg(mrb, ci);
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
    else if ((m.flags & MRB_METHOD_PROTECTED_FL) && mrb_obj_is_kind_of(mrb, self, ci->u.target_class)) {
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

  const struct RProc *p;
  if (MRB_METHOD_PROC_P(m)) {
    p = MRB_METHOD_PROC(m);
    /* handle alias */
    if (MRB_PROC_ALIAS_P(p)) {
      ci->mid = p->body.mid;
      p = p->upper;
    }
    CI_PROC_SET(ci, p);
  }
  if (MRB_METHOD_CFUNC_P(m)) {
    if (MRB_METHOD_NOARG_P(m) && (ci->n > 0 || ci->nk > 0)) {
      check_method_noarg(mrb, ci);
    }
    return MRB_METHOD_CFUNC(m)(mrb, self);
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

#define INIT_DISPATCH for (;;) { insn = BYTECODE_DECODER(*ci->pc); CODE_FETCH_HOOK(mrb, irep, ci->pc, regs); switch (insn) {
#define CASE(insn,ops) case insn: { const mrb_code *pc = ci->pc+1; FETCH_ ## ops (); ci->pc = pc; } L_ ## insn ## _BODY:
#define NEXT goto L_END_DISPATCH
#define JUMP NEXT
#ifdef MRB_USE_TASK_SCHEDULER
#define END_DISPATCH L_END_DISPATCH: \
  if (mrb->task.switching || mrb->c->status == MRB_TASK_STOPPED) \
    return mrb_nil_value(); \
  }}
#else
#define END_DISPATCH L_END_DISPATCH:;}}
#endif

#else

#define INIT_DISPATCH JUMP; return mrb_nil_value();
#define CASE(insn,ops) L_ ## insn: { const mrb_code *pc = ci->pc+1; FETCH_ ## ops (); ci->pc = pc; } L_ ## insn ## _BODY:
#ifdef MRB_USE_TASK_SCHEDULER
#define NEXT if (mrb->task.switching || mrb->c->status == MRB_TASK_STOPPED) return mrb_nil_value(); \
  insn=BYTECODE_DECODER(*ci->pc); CODE_FETCH_HOOK(mrb, irep, ci->pc, regs); goto *optable[insn]
#else
#define NEXT insn=BYTECODE_DECODER(*ci->pc); CODE_FETCH_HOOK(mrb, irep, ci->pc, regs); goto *optable[insn]
#endif
#define JUMP NEXT

#ifdef MRB_USE_TASK_SCHEDULER
#define END_DISPATCH \
  if (mrb->task.switching || mrb->c->status == MRB_TASK_STOPPED) \
    return mrb_nil_value();
#else
#define END_DISPATCH
#endif

#endif

#ifdef MRB_USE_TASK_SCHEDULER
#define TASK_STOP(mrb) \
  if (mrb->c->status != MRB_TASK_STOPPED) \
    mrb->c->status = MRB_TASK_STOPPED;
#else
#define TASK_STOP(mrb)
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
      mrb_env_unshare(mrb, e, FALSE);
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
MRB_API mrb_value
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

  if (mrb->exc) {
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
        regs[a] = mrb_int_value(mrb, (mrb_int)irep->pool[b].u.i32);
        break;
      case IREP_TT_INT64:
#if defined(MRB_INT64)
        regs[a] = mrb_int_value(mrb, (mrb_int)irep->pool[b].u.i64);
        break;
#else
#if defined(MRB_64BIT)
        if (INT32_MIN <= irep->pool[b].u.i64 && irep->pool[b].u.i64 <= INT32_MAX) {
          regs[a] = mrb_int_value(mrb, (mrb_int)irep->pool[b].u.i64);
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
        }
        break;
#else
        goto L_INT_OVERFLOW;
#endif
#ifndef MRB_NO_FLOAT
      case IREP_TT_FLOAT:
        regs[a] = mrb_float_value(mrb, irep->pool[b].u.f);
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
      SET_INT_VALUE(mrb, regs[a], (int32_t)(((uint32_t)b<<16)+c));
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

    CASE(OP_LOADT, B) {
      SET_TRUE_VALUE(regs[a]);
      NEXT;
    }

    CASE(OP_LOADF, B) {
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
      regs[a] = mrb_iv_get(mrb, regs[0], irep->syms[b]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_SETIV, BB) {
      mrb_iv_set(mrb, regs[0], irep->syms[b], regs[a]);
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
      mrb_value va = regs[a], vb = regs[a+1];
      switch (mrb_type(va)) {
      case MRB_TT_ARRAY:
        if (!mrb_integer_p(vb)) goto getidx_fallback;
        else {
          mrb_int idx = mrb_integer(vb);
          if (0 <= idx && idx < RARRAY_LEN(va)) {
            regs[a] = RARRAY_PTR(va)[idx];
          }
          else {
            regs[a] = mrb_ary_entry(va, idx);
          }
        }
        break;
      case MRB_TT_HASH:
        va = mrb_hash_get(mrb, va, vb);
        ci = mrb->c->ci;
        regs[a] = va;
        break;
      case MRB_TT_STRING:
        switch (mrb_type(vb)) {
        case MRB_TT_INTEGER:
        case MRB_TT_STRING:
        case MRB_TT_RANGE:
          va = mrb_str_aref(mrb, va, vb, mrb_undef_value());
          regs[a] = va;
          break;
        default:
          goto getidx_fallback;
        }
        break;
      default:
      getidx_fallback:
        mid = MRB_OPSYM(aref);
        goto L_SEND_SYM;
      }
      NEXT;
    }

    CASE(OP_SETIDX, B) {
      c = 2;
      mid = MRB_OPSYM(aset);
      SET_NIL_VALUE(regs[a+3]);
      goto L_SENDB_SYM;
    }

    CASE(OP_GETCONST, BB) {
      mrb_value v = mrb_vm_const_get(mrb, irep->syms[b]);
      ci = mrb->c->ci;
      regs[a] = v;
      NEXT;
    }

    CASE(OP_SETCONST, BB) {
      ci = mrb->c->ci;
      struct RClass *c = MRB_PROC_TARGET_CLASS(ci->proc);
      if (!c) c = mrb->object_class;
      mrb_const_set(mrb, mrb_obj_value(c), irep->syms[b], regs[a]);
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
      if (mrb_nil_p(exc)) {
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
            if (!c->vmexec) goto L_RAISE;
            mrb->jmp = prev_jmp;
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

    CASE(OP_SSEND, BBB) {
      regs[a] = regs[0];
    }
    goto L_SENDB;

    CASE(OP_SSENDB, BBB) {
      regs[a] = regs[0];
    }
    goto L_SENDB;

    CASE(OP_SEND, BBB)
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

      if (c < CALL_MAXARGS) {
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
      if (insn == OP_SEND || insn == OP_SSEND) {
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
      if (MRB_METHOD_UNDEF_P(m)) {
        m = prepare_missing(mrb, ci, recv, mid, blk, (insn == OP_SUPER));
      }
      else {
        ci->mid = mid;
      }
      if (insn == OP_SEND || insn == OP_SENDB) {
        mrb_bool priv = TRUE;
        if (m.flags & MRB_METHOD_PRIVATE_FL) {
        vis_err:;
          mrb_value args = (ci->n == 15) ? regs[1] : mrb_ary_new_from_values(mrb, ci->n, regs+1);
          vis_error(mrb, mid, args, recv, priv);
        }
        else if ((m.flags & MRB_METHOD_PROTECTED_FL) && mrb_obj_is_kind_of(mrb, recv, ci->u.target_class)) {
          priv = FALSE;
          goto vis_err;
        }
      }
      ci->cci = CINFO_NONE;

      if (MRB_METHOD_PROC_P(m)) {
        const struct RProc *p = MRB_METHOD_PROC(m);
        /* handle alias */
        if (MRB_PROC_ALIAS_P(p)) {
          ci->mid = p->body.mid;
          p = p->upper;
        }
        CI_PROC_SET(ci, p);
        if (!MRB_PROC_CFUNC_P(p)) {
          /* setup environment for calling method */
          irep = p->body.irep;
          stack_extend(mrb, (irep->nregs < 4) ? 4 : irep->nregs);
          ci->pc = irep->iseq;
          JUMP;
        }
        else {
          if (MRB_PROC_NOARG_P(p) && (ci->n > 0 || ci->nk > 0)) {
            check_method_noarg(mrb, ci);
          }
          recv = MRB_PROC_CFUNC(p)(mrb, recv);
        }
      }
      else {
        if (MRB_METHOD_NOARG_P(m) && (ci->n > 0 || ci->nk > 0)) {
          check_method_noarg(mrb, ci);
        }
        recv = MRB_METHOD_FUNC(m)(mrb, recv);
      }

      /* cfunc epilogue */
      mrb_gc_arena_shrink(mrb, ai);
      if (mrb->exc) goto L_RAISE;
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
      mrb_value recv = ci->stack[0];
      const struct RProc *p = mrb_proc_ptr(recv);

      /* handle alias */
      if (MRB_PROC_ALIAS_P(p)) {
        ci->mid = p->body.mid;
        p = p->upper;
      }
      else if (MRB_PROC_ENV_P(p)) {
        ci->mid = MRB_PROC_ENV(p)->mid;
      }
      /* replace callinfo */
      ci->u.target_class = MRB_PROC_TARGET_CLASS(p);
      CI_PROC_SET(ci, p);

      /* prepare stack */
      if (MRB_PROC_CFUNC_P(p)) {
        recv = MRB_PROC_CFUNC(p)(mrb, recv);
        mrb_gc_arena_shrink(mrb, ai);
        if (mrb->exc) goto L_RAISE;
        /* pop stackpos */
        ci = cipop(mrb);
        ci[1].stack[0] = recv;
        irep = ci->proc->body.irep;
      }
      else {
        /* setup environment for calling method */
        irep = p->body.irep;
        if (!irep) {
          ci->stack[0] = mrb_nil_value();
          a = 0;
          goto L_OP_RETURN_BODY;
        }
        mrb_int nargs = ci_bidx(ci)+1;
        if (nargs < irep->nregs) {
          stack_extend(mrb, irep->nregs);
          stack_clear(regs+nargs, irep->nregs-nargs);
        }
        if (MRB_PROC_ENV_P(p)) {
          regs[0] = MRB_PROC_ENV(p)->stack[0];
        }
        ci->pc = irep->iseq;
      }
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
      mrb_int m1 = (b>>11)&0x3f;
      mrb_int r  = (b>>10)&0x1;
      mrb_int m2 = (b>>5)&0x1f;
      mrb_int kd = (b>>4)&0x1;
      mrb_int lv = (b>>0)&0xf;
      mrb_value *stack;

      if (ci->mid == 0 || CI_TARGET_CLASS(ci) == NULL) {
      L_NOSUPER:
        RAISE_LIT(mrb, E_NOMETHOD_ERROR, "super called outside of method");
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
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_ENTER, W) {
      mrb_int argc = ci->n;
      mrb_value *argv = regs+1;

      mrb_int m1 = MRB_ASPEC_REQ(a);

       /* no other args */
      if ((a & ~0x7c0001) == 0 && argc < 15 && MRB_PROC_STRICT_P(ci->proc)) {
        if (argc+(ci->nk==15) != m1) { /* count kdict too */
          argnum_error(mrb, m1);
          goto L_RAISE;
        }
        /* clear local (but non-argument) variables */
        mrb_int pos = m1+2;     /* self+m1+blk */
        if (irep->nlocals-pos  > 0) {
          stack_clear(&regs[pos], irep->nlocals-pos);
        }
        NEXT;
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
      mrb_value kdict = mrb_nil_value();

      /* keyword arguments */
      if (ci->nk == 15) {
        kdict = regs[mrb_ci_kidx(ci)];
      }
      if (!kd) {
        if (!mrb_nil_p(kdict) && mrb_hash_size(mrb, kdict) > 0) {
          if (argc < 14) {
            ci->n++;
            argc++;    /* include kdict in normal arguments */
          }
          else if (argc == 14) {
            /* pack arguments and kdict */
            regs[1] = ary_new_from_regs(mrb, argc+1, 1);
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
      else if (MRB_ASPEC_KEY(a) > 0 && !mrb_nil_p(kdict)) {
        kdict = mrb_hash_dup(mrb, kdict);
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
        if (argc < m1 + m2 || (r == 0 && argc > len)) {
          argnum_error(mrb, m1+m2);
          goto L_RAISE;
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
      JUMP;
    }

    CASE(OP_KARG, BB) {
      mrb_value k = mrb_symbol_value(irep->syms[b]);
      mrb_int kidx = mrb_ci_kidx(ci);
      mrb_value kdict, v;

      if (kidx < 0 || !mrb_hash_p(kdict=regs[kidx]) || !mrb_hash_key_p(mrb, kdict, k)) {
        RAISE_FORMAT(mrb, E_ARGUMENT_ERROR, "missing keyword: %v", k);
      }
      v = mrb_hash_get(mrb, kdict, k);
      ci = mrb->c->ci;
      regs[a] = v;
      mrb_hash_delete_key(mrb, kdict, k);
      ci = mrb->c->ci;
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
      if (MRB_PROC_STRICT_P(ci->proc)) goto NORMAL_RETURN;
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
        goto NORMAL_RETURN;
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
    CASE(OP_RETURN, B) {
      mrb_int acc;
      mrb_value v;
      mrb_callinfo *return_ci;

    NORMAL_RETURN:
      v = regs[a];
      mrb_gc_protect(mrb, v);
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
        mrb_gc_protect(mrb, v);
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
          RAISE_LIT(mrb, E_LOCALJUMP_ERROR, "unexpected yield");
        }
        stack = e->stack + 1;
      }
      if (mrb_nil_p(stack[offset])) {
        RAISE_LIT(mrb, E_LOCALJUMP_ERROR, "unexpected yield");
      }
      regs[a] = stack[offset];
      NEXT;
    }

#if !defined(MRB_USE_BIGINT) || defined(MRB_INT32)
  L_INT_OVERFLOW:
    RAISE_LIT(mrb, E_RANGE_ERROR, "integer overflow");
#endif

#define TYPES2(a,b) ((((uint16_t)(a))<<8)|(((uint16_t)(b))&0xff))
#define OP_MATH(op_name)                                                    \
  /* need to check if op is overridden */                                   \
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {                  \
    OP_MATH_CASE_INTEGER(op_name);                                          \
    OP_MATH_CASE_FLOAT(op_name, integer, float);                            \
    OP_MATH_CASE_FLOAT(op_name, float,  integer);                           \
    OP_MATH_CASE_FLOAT(op_name, float,  float);                             \
    OP_MATH_CASE_STRING_##op_name();                                        \
    default:                                                                \
      mid = MRB_OPSYM(op_name);                                             \
      goto L_SEND_SYM;                                                      \
  }                                                                         \
  NEXT;
#define OP_MATH_CASE_INTEGER(op_name)                                       \
  case TYPES2(MRB_TT_INTEGER, MRB_TT_INTEGER):                              \
    {                                                                       \
      mrb_int x = mrb_integer(regs[a]), y = mrb_integer(regs[a+1]), z;      \
      if (mrb_int_##op_name##_overflow(x, y, &z)) {                         \
        OP_MATH_OVERFLOW_INT(op_name,x,y);                                  \
      }                                                                     \
      else                                                                  \
        SET_INT_VALUE(mrb,regs[a], z);                                      \
    }                                                                       \
    break
#ifdef MRB_NO_FLOAT
#define OP_MATH_CASE_FLOAT(op_name, t1, t2) (void)0
#else
#define OP_MATH_CASE_FLOAT(op_name, t1, t2)                                     \
  case TYPES2(OP_MATH_TT_##t1, OP_MATH_TT_##t2):                                \
    {                                                                           \
      mrb_float z = mrb_##t1(regs[a]) OP_MATH_OP_##op_name mrb_##t2(regs[a+1]); \
      SET_FLOAT_VALUE(mrb, regs[a], z);                                         \
    }                                                                           \
    break
#endif
#ifdef MRB_USE_BIGINT
#define OP_MATH_OVERFLOW_INT(op,x,y) regs[a] = mrb_bint_##op##_ii(mrb,x,y)
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
#ifndef MRB_NO_FLOAT
      mrb_float x, y, f;
#endif

      /* need to check if op is overridden */
      switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
      case TYPES2(MRB_TT_INTEGER,MRB_TT_INTEGER):
        {
          mrb_int x = mrb_integer(regs[a]);
          mrb_int y = mrb_integer(regs[a+1]);
          regs[a] = mrb_div_int_value(mrb, x, y);
        }
        NEXT;
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
        mid = MRB_OPSYM(div);
        goto L_SEND_SYM;
      }

#ifndef MRB_NO_FLOAT
      f = mrb_div_float(x, y);
      SET_FLOAT_VALUE(mrb, regs[a], f);
#endif
      NEXT;
    }

#define OP_MATHI(op_name)                                                   \
  /* need to check if op is overridden */                                   \
  switch (mrb_type(regs[a])) {                                              \
    OP_MATHI_CASE_INTEGER(op_name);                                         \
    OP_MATHI_CASE_FLOAT(op_name);                                           \
    default:                                                                \
      SET_INT_VALUE(mrb,regs[a+1], b);                                      \
      mid = MRB_OPSYM(op_name);                                             \
      goto L_SEND_SYM;                                                      \
  }                                                                         \
  NEXT;
#define OP_MATHI_CASE_INTEGER(op_name)                                      \
  case MRB_TT_INTEGER:                                                      \
    {                                                                       \
      mrb_int x = mrb_integer(regs[a]), y = (mrb_int)b, z;                  \
      if (mrb_int_##op_name##_overflow(x, y, &z)) {                         \
        OP_MATH_OVERFLOW_INT(op_name,x,y);                                  \
      }                                                                     \
      else                                                                  \
        SET_INT_VALUE(mrb,regs[a], z);                                      \
    }                                                                       \
    break
#ifdef MRB_NO_FLOAT
#define OP_MATHI_CASE_FLOAT(op_name) (void)0
#else
#define OP_MATHI_CASE_FLOAT(op_name)                                        \
  case MRB_TT_FLOAT:                                                        \
    {                                                                       \
      mrb_float z = mrb_float(regs[a]) OP_MATH_OP_##op_name b;              \
      SET_FLOAT_VALUE(mrb, regs[a], z);                                     \
    }                                                                       \
    break
#endif

    CASE(OP_ADDI, BB) {
      OP_MATHI(add);
    }

    CASE(OP_SUBI, BB) {
      OP_MATHI(sub);
    }

#define OP_CMP_BODY(op,v1,v2) (v1(regs[a]) op v2(regs[a+1]))

#ifdef MRB_NO_FLOAT
#define OP_CMP(op,sym) do {\
  int result;\
  /* need to check if - is overridden */\
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {\
  case TYPES2(MRB_TT_INTEGER,MRB_TT_INTEGER):\
    result = OP_CMP_BODY(op,mrb_fixnum,mrb_fixnum);\
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
#else
#define OP_CMP(op, sym) do {\
  int result;\
  /* need to check if - is overridden */\
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {\
  case TYPES2(MRB_TT_INTEGER,MRB_TT_INTEGER):\
    result = OP_CMP_BODY(op,mrb_integer,mrb_integer);\
    break;\
  case TYPES2(MRB_TT_INTEGER,MRB_TT_FLOAT):\
    result = OP_CMP_BODY(op,mrb_integer,mrb_float);\
    break;\
  case TYPES2(MRB_TT_FLOAT,MRB_TT_INTEGER):\
    result = OP_CMP_BODY(op,mrb_float,mrb_integer);\
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
      if (mrb_obj_eq(mrb, regs[a], regs[a+1])) {
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
      mrb_value splat = mrb_ary_splat(mrb, regs[a+1]);
      ci = mrb->c->ci;
      if (mrb_nil_p(regs[a])) {
        regs[a] = splat;
      }
      else {
        mrb_assert(mrb_array_p(regs[a]));
        mrb_ary_concat(mrb, regs[a], splat);
      }
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_ARYPUSH, BB) {
      mrb_assert(mrb_array_p(regs[a]));
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
      mrb_assert(mrb_array_p(regs[a]));
      mrb_ary_set(mrb, regs[b], c, regs[a]);
      NEXT;
    }

    CASE(OP_APOST, BBB) {
      mrb_value v = regs[a];
      int pre  = b;
      int post = c;
      struct RArray *ary;
      int len, idx;

      if (!mrb_array_p(v)) {
        v = ary_new_from_regs(mrb, 1, a);
      }
      ary = mrb_ary_ptr(v);
      len = (int)ARY_LEN(ary);
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
      mrb_assert(mrb_string_p(regs[a]));
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
      mrb_assert(mrb_string_p(regs[a]));
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

      mrb_assert(mrb_hash_p(hash));
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

    CASE(OP_SCLASS, B) {
      regs[a] = mrb_singleton_class(mrb, regs[a]);
      mrb_gc_arena_restore(mrb, ai);
      NEXT;
    }

    CASE(OP_TCLASS, B) {
      struct RClass *target = check_target_class(mrb);
      if (!target) goto L_RAISE;
      regs[a] = mrb_obj_value(target);
      NEXT;
    }

    CASE(OP_ALIAS, BB) {
      struct RClass *target = check_target_class(mrb);

      if (!target) goto L_RAISE;
      mrb_alias_method(mrb, target, irep->syms[a], irep->syms[b]);
      mrb_method_added(mrb, target, irep->syms[a]);
      ci = mrb->c->ci;
      NEXT;
    }
    CASE(OP_UNDEF, B) {
      struct RClass *target = check_target_class(mrb);

      if (!target) goto L_RAISE;
      mrb_undef_method_id(mrb, target, irep->syms[a]);
      ci = mrb->c->ci;
      NEXT;
    }

    CASE(OP_DEBUG, Z) {
      const mrb_code *pc = ci->pc;
      FETCH_BBB();
      ci->pc = pc;
#ifdef MRB_USE_DEBUG_HOOK
      mrb->debug_op_hook(mrb, irep, ci->pc, regs);
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
