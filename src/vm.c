/*
** vm.c - virtual machine for mruby
**
** See Copyright Notice in mruby.h
*/

#include <stddef.h>
#include <stdarg.h>
#include <math.h>
#include "mruby.h"
#include "mruby/array.h"
#include "mruby/class.h"
#include "mruby/hash.h"
#include "mruby/irep.h"
#include "mruby/numeric.h"
#include "mruby/proc.h"
#include "mruby/range.h"
#include "mruby/string.h"
#include "mruby/variable.h"
#include "mruby/error.h"
#include "mruby/opcode.h"
#include "value_array.h"
#include "mrb_throw.h"
#include "class_inline.h"

#ifndef ENABLE_STDIO
#if defined(__cplusplus)
extern "C" {
#endif
void abort(void);
#if defined(__cplusplus)
}  /* extern "C" { */
#endif
#endif

#define STACK_INIT_SIZE 128
#define CALLINFO_INIT_SIZE 32

/* Define amount of linear stack growth. */
#ifndef MRB_STACK_GROWTH
#define MRB_STACK_GROWTH 128
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

//#define VM_PRINTF
#define VM_PRINTF(...) fprintf(stderr, __VA_ARGS__)

#define ARENA_RESTORE(mrb,ai) (mrb)->arena_idx = (ai)

#define strlen_str_const(str_const) (sizeof(str_const) - 1)
#define intern_str_const(mrb, str_const) (mrb_intern_static(mrb, str_const, strlen_str_const(str_const)))
#define exc_new_str_const(mrb, err, str_const) (mrb_exc_new_str(mrb, err, mrb_str_new_static(mrb, str_const, strlen_str_const(str_const))))

static const char _str_const_op_debug_format[] = "OP_DEBUG %d %d %d\n";
static const char _str_const_no_target_class[] = "no target class or module";
static const char _str_const_method_missing[] = "method_missing";
static const char _str_const_fiber_error[] = "FiberError";
static const char _str_const_proc[] = "Proc";
static const char _str_const_to_proc[] = "to_proc";
static const char _str_const_double_resume[] = "double resume";
static const char _str_const_attached[] = "__attached__";
static const char _str_const_super_outside_method[] = "super called outside of method";

void
mrb_codedump_all(mrb_state *mrb, struct RProc *proc);

static inline void
stack_clear(mrb_value *from, size_t count)
{
#ifndef MRB_NAN_BOXING
#if 0
  const mrb_value mrb_value_zero = { { 0 } };

  while (count-- > 0) {
    *from++ = mrb_value_zero;
  }
#endif
  memset(from, 0, count * sizeof(mrb_value));
#else
  while (count-- > 0) {
    SET_NIL_VALUE(*from);
    from++;
  }
#endif
}

static inline void
stack_copy(mrb_value *dst, const mrb_value *src, size_t size)
{
  while (size-- > 0) {
    *dst++ = *src++;
  }
}

static void
stack_init(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;

  /* mrb_assert(mrb->stack == NULL); */
  c->stbase = (mrb_value *)mrb_calloc(mrb, STACK_INIT_SIZE, sizeof(mrb_value));
  c->stend = c->stbase + STACK_INIT_SIZE;
  c->stack = c->stbase;

  /* mrb_assert(ci == NULL); */
  c->cibase = (mrb_callinfo *)mrb_calloc(mrb, CALLINFO_INIT_SIZE, sizeof(mrb_callinfo));
  c->ciend = c->cibase + CALLINFO_INIT_SIZE;
  c->ci = c->cibase;
  c->ci->target_class = mrb->object_class;
  c->ci->stackent = c->stack;
}

static inline void
envadjust(mrb_state *mrb, mrb_value *oldbase, mrb_value *newbase)
{
  mrb_callinfo *ci = mrb->c->cibase;

  if (newbase == oldbase) return;
  while (ci <= mrb->c->ci) {
    struct REnv *e = ci->env;
    if (e && MRB_ENV_STACK_SHARED_P(e)) {
      ptrdiff_t off = e->stack - oldbase;

      e->stack = newbase + off;
    }
    ci->stackent = newbase + (ci->stackent - oldbase);
    ci++;
  }
}

static inline void
init_new_stack_space(mrb_state *mrb, int room, int keep)
{
  if (MRB_LIKELY(room > keep)) {
    /* do not leave uninitialized malloc region */
    stack_clear(&(mrb->c->stack[keep]), room - keep);
  }
}

/** def rec ; $deep =+ 1 ; if $deep > 1000 ; return 0 ; end ; rec ; end  */

static void
stack_extend_alloc(mrb_state *mrb, int room, int keep)
{
  mrb_value *oldbase = mrb->c->stbase;
  int size = mrb->c->stend - mrb->c->stbase;
  int off = mrb->c->stack - mrb->c->stbase;

#ifdef MRB_STACK_EXTEND_DOUBLING
  if (room <= size)
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

  mrb->c->stbase = (mrb_value *)mrb_realloc(mrb, mrb->c->stbase, sizeof(mrb_value) * size);
  mrb->c->stack = mrb->c->stbase + off;
  mrb->c->stend = mrb->c->stbase + size;
  envadjust(mrb, oldbase, mrb->c->stbase);

  /* Raise an exception if the new stack size will be too large,
     to prevent infinite recursion. However, do this only after resizing the stack, so mrb_raise has stack space to work with. */
  if (size > MRB_STACK_MAX) {
    init_new_stack_space(mrb, room, keep);
    mrb_raise(mrb, E_SYSSTACK_ERROR, "stack level too deep. (limit=" MRB_STRINGIZE(MRB_STACK_MAX) ")");
  }
}

static inline void
stack_extend(mrb_state *mrb, int room, int keep)
{
  if (MRB_UNLIKELY(mrb->c->stack + room >= mrb->c->stend)) {
    stack_extend_alloc(mrb, room, keep);
  }
  init_new_stack_space(mrb, room, keep);
}

static inline struct REnv*
uvenv(mrb_state *mrb, int up)
{
  struct REnv *e = mrb->c->ci->proc->env;

  while (up--) {
    if (!e) return NULL;
    e = (struct REnv*)e->c;
  }
  return e;
}

static inline mrb_bool
is_strict(mrb_state *mrb, struct REnv *e)
{
  int cioff = e->cioff;

  if (MRB_ENV_STACK_SHARED_P(e) && mrb->c->cibase[cioff].proc &&
      MRB_PROC_STRICT_P(mrb->c->cibase[cioff].proc)) {
    return TRUE;
  }
  return FALSE;
}

static inline struct REnv*
top_env(mrb_state *mrb, struct RProc *proc)
{
  struct REnv *e = proc->env;

  if (is_strict(mrb, e)) return e;
  while (e->c) {
    e = (struct REnv*)e->c;
    if (is_strict(mrb, e)) return e;
  }
  return e;
}

#define CI_ACC_SKIP    -1
#define CI_ACC_DIRECT  -2

static mrb_callinfo*
cipush(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;
  mrb_callinfo *ci = c->ci;

  int eidx = ci->eidx;
  int ridx = ci->ridx;

  if (ci + 1 == c->ciend) {
    ptrdiff_t size = ci - c->cibase;

    c->cibase = (mrb_callinfo *)mrb_realloc(mrb, c->cibase, sizeof(mrb_callinfo)*size*2);
    c->ci = c->cibase + size;
    c->ciend = c->cibase + size * 2;
  }
  ci = ++c->ci;
  ci->eidx = eidx;
  ci->ridx = ridx;
  ci->env = 0;
  ci->pc = 0;
  ci->err = 0;
  ci->proc = 0;

  return ci;
}

static void
cipop(mrb_state *mrb)
{
  struct mrb_context *c = mrb->c;

  if (MRB_UNLIKELY(c->ci->env)) {
    struct REnv *e = c->ci->env;
    size_t len = (size_t)MRB_ENV_STACK_LEN(e);
    mrb_value *p = (mrb_value *)mrb_malloc(mrb, sizeof(mrb_value)*len);

    MRB_ENV_UNSHARE_STACK(e);
    if (len > 0) {
      stack_copy(p, e->stack, len);
    }
    e->stack = p;
    mrb_write_barrier(mrb, (struct RBasic *)e);
  }

  c->ci--;
}

static void
ecall(mrb_state *mrb, int i)
{
  struct RProc *p;
  mrb_callinfo *ci;
  mrb_value *self = mrb->c->stack;
  struct RObject *exc;

  p = mrb->c->ensure[i];
  if (!p) return;
  if (mrb->c->ci->eidx > i)
    mrb->c->ci->eidx = i;
  ci = cipush(mrb);
  ci->stackent = mrb->c->stack;
  ci->mid = ci[-1].mid;
  ci->acc = CI_ACC_SKIP;
  ci->argc = 0;
  ci->proc = p;
  ci->nregs = p->body.irep->nregs;
  ci->target_class = p->target_class;
  mrb->c->stack = mrb->c->stack + ci[-1].nregs;
  exc = mrb->exc; mrb->exc = 0;
  mrb_run(mrb, p, *self);
  mrb->c->ensure[i] = NULL;
  if (!mrb->exc) mrb->exc = exc;
}

#ifndef MRB_FUNCALL_ARGC_MAX
#define MRB_FUNCALL_ARGC_MAX 16
#endif

MRB_API mrb_value
mrb_funcall(mrb_state *mrb, mrb_value self, const char *name, mrb_int argc, ...)
{
  mrb_value argv[MRB_FUNCALL_ARGC_MAX];
  va_list ap;
  mrb_int i;
  mrb_sym mid = mrb_intern_cstr(mrb, name);

  if (argc > MRB_FUNCALL_ARGC_MAX) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Too long arguments. (limit=" MRB_STRINGIZE(MRB_FUNCALL_ARGC_MAX) ")");
  }

  va_start(ap, argc);
  for (i = 0; i < argc; i++) {
    argv[i] = va_arg(ap, mrb_value);
  }
  va_end(ap);
  return mrb_funcall_argv(mrb, self, mid, argc, argv);
}

MRB_API mrb_value
mrb_funcall_with_block(mrb_state *mrb, mrb_value self, mrb_sym mid, mrb_int argc, const mrb_value *argv, mrb_value blk)
{
  mrb_value val;

  if (!mrb->jmp) {
    struct mrb_jmpbuf c_jmp;
    ptrdiff_t nth_ci = mrb->c->ci - mrb->c->cibase;

    MRB_TRY(&c_jmp) {
      mrb->jmp = &c_jmp;
      /* recursive call */
      val = mrb_funcall_with_block(mrb, self, mid, argc, argv, blk);
      mrb->jmp = 0;
    }
    MRB_CATCH(&c_jmp) { /* error */
      while (nth_ci < (mrb->c->ci - mrb->c->cibase)) {
        mrb->c->stack = mrb->c->ci->stackent;
        cipop(mrb);
      }
      mrb->jmp = 0;
      val = mrb_obj_value(mrb->exc);
    }
    MRB_END_EXC(&c_jmp);
  }
  else {
    struct RProc *p;
    struct RClass *c;
    mrb_sym undef = 0;
    mrb_callinfo *ci;
    int n;
    ptrdiff_t voff = -1;

    if (!mrb->c->stack) {
      stack_init(mrb);
    }
    n = mrb->c->ci->nregs;
    if (argc < 0) {
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "negative argc for funcall (%S)", mrb_fixnum_value(argc));
    }
    c = mrb_class(mrb, self);
    p = mrb_method_search_vm(mrb, &c, mid);

    //fprintf(stderr, "funcall %s: %p\n", mrb_sym2name(mrb, mid), p);

    if (!p) {
      undef = mid;
      mid = intern_str_const(mrb, _str_const_method_missing);
      p = mrb_method_search_vm(mrb, &c, mid);
      n++; argc++;
    }
    ci = cipush(mrb);
    ci->mid = mid;
    ci->proc = p;
    ci->stackent = mrb->c->stack;
    ci->argc = argc;
    ci->target_class = c;
    mrb->c->stack = mrb->c->stack + n;
    if (mrb->c->stbase <= argv && argv < mrb->c->stend) {
      voff = argv - mrb->c->stbase;
    }
    if (MRB_PROC_CFUNC_P(p)) {
      ci->nregs = argc + 2;
      stack_extend(mrb, ci->nregs, 0);
    }
    else {
      ci->nregs = p->body.irep->nregs + n;
      stack_extend(mrb, ci->nregs, argc+2);
    }
    if (voff >= 0) {
      argv = mrb->c->stbase + voff;
    }
    mrb->c->stack[0] = self;
    if (undef) {
      mrb->c->stack[1] = mrb_symbol_value(undef);
      if (argc > 1) {
        stack_copy(mrb->c->stack+2, argv, argc-1);
      }
    }
    else if (argc > 0) {
      stack_copy(mrb->c->stack+1, argv, argc);
    }
    mrb->c->stack[argc+1] = blk;

    if (MRB_PROC_CFUNC_P(p)) {
      int ai = mrb_gc_arena_save(mrb);

      ci->acc = CI_ACC_DIRECT;
      val = p->body.func(mrb, self);
      mrb->c->stack = mrb->c->ci->stackent;
      cipop(mrb);
      mrb_gc_arena_restore(mrb, ai);
    }
    else {
      ci->acc = CI_ACC_SKIP;
      val = mrb_run(mrb, p, self);
    }
  }
  mrb_gc_protect(mrb, val);
  return val;
}

MRB_API mrb_value
mrb_funcall_argv(mrb_state *mrb, mrb_value self, mrb_sym mid, mrb_int argc, const mrb_value *argv)
{
  return mrb_funcall_with_block(mrb, self, mid, argc, argv, mrb_nil_value());
}

/* 15.3.1.3.4  */
/* 15.3.1.3.44 */
/*
 *  call-seq:
 *     obj.send(symbol [, args...])        -> obj
 *     obj.__send__(symbol [, args...])      -> obj
 *
 *  Invokes the method identified by _symbol_, passing it any
 *  arguments specified. You can use <code>__send__</code> if the name
 *  +send+ clashes with an existing method in _obj_.
 *
 *     class Klass
 *       def hello(*args)
 *         "Hello " + args.join(' ')
 *       end
 *     end
 *     k = Klass.new
 *     k.send :hello, "gentle", "readers"   #=> "Hello gentle readers"
 */
MRB_API mrb_value
mrb_f_send(mrb_state *mrb, mrb_value self)
{
  mrb_sym name;
  mrb_value block, *argv, *regs;
  mrb_int argc, i, len;
  struct RProc *p;
  struct RClass *c;
  mrb_callinfo *ci;

  mrb_get_args(mrb, "n*&", &name, &argv, &argc, &block);

  c = mrb_class(mrb, self);
  p = mrb_method_search_vm(mrb, &c, name);

  if (!p) {                     /* call method_mising */
    return mrb_funcall_with_block(mrb, self, name, argc, argv, block);
  }

  ci = mrb->c->ci;
  ci->mid = name;
  ci->target_class = c;
  ci->proc = p;
  regs = mrb->c->stack+1;
  /* remove first symbol from arguments */
  if (ci->argc >= 0) {
    for (i=0,len=ci->argc; i<len; i++) {
      regs[i] = regs[i+1];
    }
    ci->argc--;
  }
  else {                     /* variable length arguments */
    mrb_ary_shift(mrb, regs[0]);
  }

  if (MRB_PROC_CFUNC_P(p)) {
    return p->body.func(mrb, self);
  }

  if (ci->argc < 0) {
    stack_extend(mrb, (p->body.irep->nregs < 3) ? 3 : p->body.irep->nregs, 3);
  }
  else {
    stack_extend(mrb, p->body.irep->nregs, ci->argc+2);
  }

  ci->nregs = p->body.irep->nregs;
  ci = cipush(mrb);
  ci->nregs = 0;
  ci->target_class = 0;
  ci->pc = p->body.irep->iseq;
  ci->stackent = mrb->c->stack;
  ci->acc = 0;

  return self;
}

static mrb_value
eval_under(mrb_state *mrb, mrb_value self, mrb_value blk, struct RClass *c)
{
  struct RProc *p;
  mrb_callinfo *ci;

  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }
  ci = mrb->c->ci;
  if (ci->acc == CI_ACC_DIRECT) {
    return mrb_yield_with_class(mrb, blk, 0, 0, self, c);
  }
  ci->target_class = c;
  p = mrb_proc_ptr(blk);
  ci->proc = p;
  if (MRB_PROC_CFUNC_P(p)) {
    return p->body.func(mrb, self);
  }
  ci->nregs = p->body.irep->nregs;
  ci = cipush(mrb);
  ci->nregs = 0;
  ci->target_class = 0;
  ci->pc = p->body.irep->iseq;
  ci->stackent = mrb->c->stack;
  ci->acc = 0;

  return self;
}

/* 15.2.2.4.35 */
/*
 *  call-seq:
 *     mod.class_eval {| | block }  -> obj
 *     mod.module_eval {| | block } -> obj
 *
 *  Evaluates block in the context of _mod_. This can
 *  be used to add methods to a class. <code>module_eval</code> returns
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
 *  In order to set the context, the variable +self+ is set to _obj_ while
 *  the code is executing, giving the code access to _obj_'s
 *  instance variables. In the version of <code>instance_eval</code>
 *  that takes a +String+, the optional second and third
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
  mrb_value cv;
  struct RClass *c;

  if (mrb_get_args(mrb, "|S&", &a, &b) == 1) {
    mrb_raise(mrb, E_NOTIMP_ERROR, "instance_eval with string not implemented");
  }
  switch (mrb_type(self)) {
  case MRB_TT_SYMBOL:
  case MRB_TT_FIXNUM:
  case MRB_TT_FLOAT:
    c = 0;
    break;
  default:
    cv = mrb_singleton_class(mrb, self);
    c = mrb_class_ptr(cv);
    break;
  }
  return eval_under(mrb, self, b, c);
}

MRB_API mrb_value
mrb_yield_with_class(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv, mrb_value self, struct RClass *c)
{
  struct RProc *p;
  mrb_sym mid = mrb->c->ci->mid;
  mrb_callinfo *ci;
  int n = mrb->c->ci->nregs;
  mrb_value val;

  if (mrb_nil_p(b)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "no block given");
  }
  p = mrb_proc_ptr(b);
  ci = cipush(mrb);
  ci->mid = mid;
  ci->proc = p;
  ci->stackent = mrb->c->stack;
  ci->argc = argc;
  ci->target_class = c;
  ci->acc = CI_ACC_SKIP;
  mrb->c->stack = mrb->c->stack + n;
  if (MRB_PROC_CFUNC_P(p)) {
    ci->nregs = argc + 2;
    stack_extend(mrb, ci->nregs, 0);
  }
  else {
    ci->nregs = p->body.irep->nregs;
    stack_extend(mrb, ci->nregs, argc+2);
  }

  mrb->c->stack[0] = self;
  if (argc > 0) {
    stack_copy(mrb->c->stack+1, argv, argc);
  }
  mrb->c->stack[argc+1] = mrb_nil_value();

  if (MRB_PROC_CFUNC_P(p)) {
    val = p->body.func(mrb, self);
    mrb->c->stack = mrb->c->ci->stackent;
    cipop(mrb);
  }
  else {
    val = mrb_run(mrb, p, self);
  }
  return val;
}

MRB_API mrb_value
mrb_yield_argv(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv)
{
  struct RProc *p = mrb_proc_ptr(b);

  return mrb_yield_with_class(mrb, b, argc, argv, p->env->stack[0], p->target_class);
}

MRB_API mrb_value
mrb_yield(mrb_state *mrb, mrb_value b, mrb_value arg)
{
  struct RProc *p = mrb_proc_ptr(b);

  return mrb_yield_with_class(mrb, b, 1, &arg, p->env->stack[0], p->target_class);
}

typedef enum {
  LOCALJUMP_ERROR_RETURN = 0,
  LOCALJUMP_ERROR_BREAK = 1,
  LOCALJUMP_ERROR_YIELD = 2
} localjump_error_kind;

static void
localjump_error(mrb_state *mrb, localjump_error_kind kind)
{
  char kind_str[3][7] = { "return", "break", "yield" };
  char kind_str_len[] = { 6, 5, 5 };
  static const char lead[] = "unexpected ";
  mrb_value msg;
  mrb_value exc;

  msg = mrb_str_buf_new(mrb, sizeof(lead) + 7);
  mrb_str_cat(mrb, msg, lead, sizeof(lead) - 1);
  mrb_str_cat(mrb, msg, kind_str[kind], kind_str_len[kind]);
  exc = mrb_exc_new_str(mrb, E_LOCALJUMP_ERROR, msg);
  mrb->exc = mrb_obj_ptr(exc);
}

static void
argnum_error(mrb_state *mrb, mrb_int num)
{
  mrb_value exc;
  mrb_value str;

  if (mrb->c->ci->mid) {
    str = mrb_format(mrb, "'%S': wrong number of arguments (%S for %S)",
                  mrb_sym2str(mrb, mrb->c->ci->mid),
                  mrb_fixnum_value(mrb->c->ci->argc), mrb_fixnum_value(num));
  }
  else {
    str = mrb_format(mrb, "wrong number of arguments (%S for %S)",
                  mrb_fixnum_value(mrb->c->ci->argc), mrb_fixnum_value(num));
  }
  exc = mrb_exc_new_str(mrb, E_ARGUMENT_ERROR, str);
  mrb->exc = mrb_obj_ptr(exc);
}

#if defined(__GNUC__) || defined(__clang__) || defined(__INTEL_COMPILER)
#define FORCE_INLINE inline __attribute__((always_inline))
#define NO_INLINE __attribute__ ((noinline))
#elif defined _MSC_VER
#define FORCE_INLINE __forceinline
#define NO_INLINE __declspec(noinline)
#else
#define FORCE_INLINE inline
#define NO_INLINE
#endif


#if !defined(MRB_JIT_GEN) && !defined(MRB_VM_NO_INLINE)
#define OP_INLINE FORCE_INLINE
#define JIT_NO_INLINE
#else
#define OP_INLINE NO_INLINE
#define JIT_NO_INLINE NO_INLINE
#endif


#define ERR_PC_SET(mrb, pc) mrb->c->ci->err = pc;
#define ERR_PC_CLR(mrb)     mrb->c->ci->err = 0;
#ifdef ENABLE_DEBUG
#define CODE_FETCH_HOOK(ctx) if ((ctx.mrb)->code_fetch_hook) (ctx.mrb)->code_fetch_hook((ctx.mrb), (ctx.irep), (ctx.pc), (ctx.regs));
#else
#define CODE_FETCH_HOOK(ctx)
#endif

#if defined __GNUC__ || defined __clang__ || defined __INTEL_COMPILER
#define DIRECT_THREADED
#endif

#ifndef DIRECT_THREADED

#define INIT_DISPATCH for (;;) { ctx.i = *ctx.pc; CODE_FETCH_HOOK(ctx); switch (GET_OPCODE(ctx.i)) {
#define CASE(op) case op:
#define NEXT pc++; break
#define JUMP break
#define END_DISPATCH }}

#else

#define INIT_DISPATCH JUMP; return mrb_nil_value();
#define CASE(op) L_ ## op:
#define NEXT ctx.i=*++ctx.pc; CODE_FETCH_HOOK(ctx); goto *optable[GET_OPCODE(ctx.i)]
#define JUMP ctx.i=*ctx.pc; CODE_FETCH_HOOK(ctx); goto *optable[GET_OPCODE(ctx.i)]

#define END_DISPATCH

#endif

#define CALL_MAXARGS 127

#if defined __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wattributes"
#endif

struct op_ctx {
  struct RProc *proc;
  mrb_irep *irep;
  mrb_code *pc;
  mrb_value *regs;
  mrb_value *pool;
  mrb_sym *syms;
  struct mrb_jmpbuf *prev_jmp;
  mrb_value retval;
  struct mrb_jmpbuf *stop_jmp;
  int ai;
  mrb_state *mrb;
  mrb_code i;
#ifdef MRB_ENABLE_JIT
  uint8_t *rescue_jmp_addr;
  uint32_t op_idx;

  /* !!!!!!!!
   * needs to be last field
   * !!!!!!!!
   */
  //void *sym_tbl[124];
#endif
};


#ifdef MRB_JIT_GEN

void NO_INLINE __mrb_jit_pc_add__(mrb_code *pc, int o) {
}

void NO_INLINE __mrb_jit_pc_inc__(mrb_code *pc) {
}

static const uint16_t A;
static const uint16_t B;
static const uint8_t C;
static const uint16_t Bx;
static const int16_t sBx;
static const uint32_t Ax;
static const uint16_t b;
static const uint16_t c;

typedef int (*__arg_protect__)(int);
//#define ARG_PROTECT(x) (((__arg_protect__)(0xABBEEF))(x))
#define ARG_PROTECT(x) (x)
#define JIT_VOLATILE volatile
#undef GETARG_A
#define GETARG_A(i) ((uint16_t)(uintptr_t)(&A))
#undef GETARG_Ax
#define GETARG_Ax(i) ((uint32_t)(uintptr_t)(&Ax))
#undef GETARG_B
#define GETARG_B(i) ((uint16_t)(uintptr_t)(&Bx))
#undef GETARG_b
#define GETARG_b(i) ((uint16_t)(uintptr_t)(&b))
#undef GETARG_sBx
#define GETARG_sBx(i) ((int16_t)(uintptr_t)(&sBx))
#undef GETARG_Bx
#define GETARG_Bx(i) ((uint16_t)(uintptr_t)(&Bx))
#undef GETARG_C
#define GETARG_C(i) ((uint8_t)(uintptr_t)(&C))
#undef GETARG_c
#define GETARG_c(i) ((uint16_t)(uintptr_t)(&c))
#define PC_ADD(pc, o) (__mrb_jit_pc_add__(pc, o))
#define PC_INC(pc) (__mrb_jit_pc_inc__(pc))
#define OP_IDX(i) 0xDE0000
#else
#define PC_ADD(pc, o) (pc += o)
#define PC_INC(pc) (pc++)
#define OP_END
#define JIT_VOLATILE
#define ARG_PROTECT(x) (x)
#endif

#define CTX_I(ctx) (*(ctx->pc))

static OP_INLINE void
op_nop(struct op_ctx *ctx) {
  /* do nothing */
}

static OP_INLINE void
op_move(struct op_ctx *ctx) {
  /* A B    R(A) := R(B) */
  ctx->regs[GETARG_A(CTX_I(ctx))] = ctx->regs[GETARG_B(CTX_I(ctx))];
}

static OP_INLINE void
op_loadl(struct op_ctx *ctx) {
  /* A Bx   R(A) := Pool(Bx) */
  ctx->regs[GETARG_A(CTX_I(ctx))] = ctx->pool[GETARG_Bx(CTX_I(ctx))];
}

static OP_INLINE void
op_loadi(struct op_ctx *ctx) {
  /* A sBx  R(A) := sBx */
  SET_INT_VALUE(ctx->regs[GETARG_A(CTX_I(ctx))], GETARG_sBx(CTX_I(ctx)));
}

static OP_INLINE void
op_loadsym(struct op_ctx *ctx) {
  /* A Bx   R(A) := Syms(Bx) */
  SET_SYM_VALUE(ctx->regs[GETARG_A(CTX_I(ctx))], ctx->syms[GETARG_Bx(CTX_I(ctx))]);
}

static OP_INLINE void
op_loadself(struct op_ctx *ctx) {
  /* A      R(A) := self */
  ctx->regs[GETARG_A(CTX_I(ctx))] = ctx->regs[0];
}

static OP_INLINE void
op_loadt(struct op_ctx *ctx) {
  /* A      R(A) := true */
  SET_TRUE_VALUE(ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static OP_INLINE void
op_loadf(struct op_ctx *ctx) {
  /* A      R(A) := false */
  SET_FALSE_VALUE(ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static OP_INLINE void
op_getglobal(struct op_ctx *ctx) {
  /* A Bx   R(A) := getglobal(Syms(Bx)) */
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_gv_get(ctx->mrb, ctx->syms[GETARG_Bx(CTX_I(ctx))]);
}

static OP_INLINE void
op_setglobal(struct op_ctx *ctx) {
  /* setglobal(Syms(Bx), R(A)) */
  mrb_gv_set(ctx->mrb, ctx->syms[GETARG_Bx(CTX_I(ctx))], ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static OP_INLINE void
op_getspecial(struct op_ctx *ctx) {
  /* A Bx   R(A) := Special[Bx] */
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_vm_special_get(ctx->mrb, GETARG_Bx(CTX_I(ctx)));
}

static OP_INLINE void
op_setspecial(struct op_ctx *ctx) {
  /* A Bx   Special[Bx] := R(A) */
  mrb_vm_special_set(ctx->mrb, GETARG_Bx(CTX_I(ctx)), ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static OP_INLINE void
op_getiv(struct op_ctx *ctx) {
  /* A Bx   R(A) := ivget(Bx) */
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_vm_iv_get(ctx->mrb, ctx->syms[GETARG_Bx(CTX_I(ctx))]);
}

static OP_INLINE void
op_setiv(struct op_ctx *ctx) {
  /* ivset(Syms(Bx),R(A)) */
  mrb_vm_iv_set(ctx->mrb, ctx->syms[GETARG_Bx(CTX_I(ctx))], ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static OP_INLINE void
op_getcv(struct op_ctx *ctx) {
  /* A Bx   R(A) := cvget(Syms(Bx)) */
  ERR_PC_SET(ctx->mrb, ctx->pc);
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_vm_cv_get(ctx->mrb, ctx->syms[GETARG_Bx(CTX_I(ctx))]);
  ERR_PC_CLR(ctx->mrb);
}

static OP_INLINE void
op_setcv(struct op_ctx *ctx) {
  /* cvset(Syms(Bx),R(A)) */
  mrb_vm_cv_set(ctx->mrb, ctx->syms[GETARG_Bx(CTX_I(ctx))], ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static OP_INLINE void
op_getconst(struct op_ctx *ctx) {
  /* A Bx    R(A) := constget(Syms(Bx)) */
  mrb_value val;

  ERR_PC_SET(ctx->mrb, ctx->pc);
  val = mrb_vm_const_get(ctx->mrb, ctx->syms[GETARG_Bx(CTX_I(ctx))]);
  ERR_PC_CLR(ctx->mrb);
  ctx->regs = ctx->mrb->c->stack;
  ctx->regs[GETARG_A(CTX_I(ctx))] = val;
}

static OP_INLINE void
op_setconst(struct op_ctx *ctx) {
  /* A Bx   constset(Syms(Bx),R(A)) */
  mrb_vm_const_set(ctx->mrb, ctx->syms[GETARG_Bx(CTX_I(ctx))], ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static OP_INLINE void
op_getmcnst(struct op_ctx *ctx) {
  /* A Bx   R(A) := R(A)::Syms(Bx) */
  mrb_value val;
  int a = GETARG_A(CTX_I(ctx));

  ERR_PC_SET(ctx->mrb, ctx->pc);
  val = mrb_const_get(ctx->mrb, ctx->regs[a], ctx->syms[GETARG_Bx(CTX_I(ctx))]);
  ERR_PC_CLR(ctx->mrb);
  ctx->regs = ctx->mrb->c->stack;
  ctx->regs[a] = val;
}

static OP_INLINE void
op_setmcnst(struct op_ctx *ctx) {
  /* A Bx    R(A+1)::Syms(Bx) := R(A) */
  int a = GETARG_A(CTX_I(ctx));

  mrb_const_set(ctx->mrb, ctx->regs[a+1], ctx->syms[GETARG_Bx(CTX_I(ctx))], ctx->regs[a]);
}

static OP_INLINE void
op_getupvar(struct op_ctx *ctx) {
  /* A B C  R(A) := uvget(B,C) */
  mrb_value *regs_a = ctx->regs + GETARG_A(CTX_I(ctx));
  int up = ARG_PROTECT(GETARG_C(CTX_I(ctx)));

  struct REnv *e = uvenv(ctx->mrb, up);

  if (!e) {
    *regs_a = mrb_nil_value();
  }
  else {
    int idx = GETARG_B(CTX_I(ctx));
    *regs_a = e->stack[idx];
  }
}

static OP_INLINE void
op_setupvar(struct op_ctx *ctx) {
  /* A B C  uvset(B,C,R(A)) */
  int up = ARG_PROTECT(GETARG_C(CTX_I(ctx)));

  struct REnv *e = uvenv(ctx->mrb, up);

  if (e) {
    mrb_value *regs_a = ctx->regs + GETARG_A(CTX_I(ctx));
    int idx = GETARG_B(CTX_I(ctx));
    e->stack[idx] = *regs_a;
    mrb_write_barrier(ctx->mrb, (struct RBasic*)e);
  }
}

static OP_INLINE void
op_jmp(struct op_ctx *ctx) {
  /* sBx    pc+=sBx */
  PC_ADD(ctx->pc, GETARG_sBx(CTX_I(ctx)));
}

static OP_INLINE void
op_jmpif(struct op_ctx *ctx) {
  /* A sBx  if R(A) pc+=sBx */
  if (mrb_test(ctx->regs[GETARG_A(CTX_I(ctx))])) {
    PC_ADD(ctx->pc, GETARG_sBx(CTX_I(ctx)));
#ifdef MRB_JIT_GEN
    volatile int *p = 0xFAB;
    *p = 0xFAB;
#endif
  } else {
    PC_INC(ctx->pc);
  }
}
static const char _str_const_op_jmpnot[] = "op_jmpnot %d %d\n";
static OP_INLINE void
op_jmpnot(struct op_ctx *ctx) {
  /* A sBx  if R(A) pc+=sBx */
  if (!mrb_test(ctx->regs[GETARG_A(CTX_I(ctx))])) {
    PC_ADD(ctx->pc, GETARG_sBx(CTX_I(ctx)));
#ifdef MRB_JIT_GEN
    volatile int *p = 0xFAB;
    *p = 0xFAB;
#endif
  } else {
    PC_INC(ctx->pc);
  }
}

static const char _str_const_op_onerr[] = "op_onerr %p %ld\n";
static OP_INLINE void
op_onerr(struct op_ctx *ctx) {
  /* sBx    pc+=sBx on exception */
  if (ctx->mrb->c->rsize <= ctx->mrb->c->ci->ridx) {
    if (ctx->mrb->c->rsize == 0) ctx->mrb->c->rsize = 16;
    else ctx->mrb->c->rsize *= 2;
    ctx->mrb->c->rescue = (mrb_code **)mrb_realloc(ctx->mrb, ctx->mrb->c->rescue, sizeof(mrb_code*) * ctx->mrb->c->rsize);
  }
  //VM_PRINTF(_str_const_op_onerr,ctx->irep->iseq, GETARG_sBx(CTX_I(ctx)), ctx->irep->iseq + GETARG_sBx(CTX_I(ctx)));

  ////VM_PRINTF(_str_const_op_onerr,NULL,ctx->mrb->c->rsize , NULL);
  ////VM_PRINTF(_str_const_op_onerr,NULL,ctx->mrb->c->ci->ridx, NULL);
 // //VM_PRINTF(_str_const_op_onerr,ctx->mrb->c->rescue, 0, NULL);
//  //VM_PRINTF(_str_const_op_onerr,ctx->mrb->c->rescue[ctx->mrb->c->ci->ridx], ctx->mrb->c->ci->ridx, NULL);

  //mrb_codedump_all(ctx->mrb, ctx->proc);

#ifdef MRB_JIT_GEN
  //ctx->mrb->c->ci->rescue_idx = OP_IDX(CTX_I(ctx));
  ctx->pc = ctx->irep->iseq + OP_IDX(ctx);
#endif

  //VM_PRINTF(_str_const_op_onerr,ctx->pc, ctx->pc - ctx->irep->iseq);
  //VM_PRINTF(_str_const_op_onerr, ctx->pc, ctx->mrb->c->ci->ridx);
  ctx->mrb->c->rescue[ctx->mrb->c->ci->ridx++] = ctx->pc + GETARG_sBx(CTX_I(ctx));

}


static char _str_const_op_rescue[] = "op_rescue %d %p\n";
static OP_INLINE void
op_rescue(struct op_ctx *ctx) {
  /* A      R(A) := exc; clear(exc) */
  int a = GETARG_A(CTX_I(ctx));
  //VM_PRINTF(_str_const_op_rescue, a, ctx->rescue_jmp_addr);
  SET_OBJ_VALUE(ctx->regs[a], ctx->mrb->exc);
  ctx->mrb->exc = 0;
}

static OP_INLINE void
op_poperr(struct op_ctx *ctx) {
  /* A      A.times{rescue_pop()} */
  int a = GETARG_A(CTX_I(ctx));

  while (a--) {
    ctx->mrb->c->ci->ridx--;
  }
}

static void
_op_stop(struct op_ctx *ctx) {
  //VM_PRINTF("_op_stop\n");
  {
    int eidx_stop = ctx->mrb->c->ci == ctx->mrb->c->cibase ? 0 : ctx->mrb->c->ci[-1].eidx;
    int eidx = ctx->mrb->c->ci->eidx;
    while (eidx > eidx_stop) {
      ecall(ctx->mrb, --eidx);
    }
  }

  ERR_PC_CLR(ctx->mrb);
  ctx->mrb->jmp = ctx->prev_jmp;
  if (ctx->mrb->exc) {
    ctx->retval = mrb_obj_value(ctx->mrb->exc);
  }
  ctx->retval = ctx->regs[ctx->irep->nlocals];

  MRB_THROW(ctx->stop_jmp);
}

static inline void
_op_rescue(struct op_ctx *ctx, mrb_callinfo *ci) {
  if (ci->ridx == 0) {
    //VM_PRINTF("_op_rescue: stopping\n");
    return _op_stop(ctx);
  }
  ctx->proc = ci->proc;
  ctx->irep = ctx->proc->body.irep;
  ctx->pool = ctx->irep->pool;
  ctx->syms = ctx->irep->syms;
  ctx->regs = ctx->mrb->c->stack = ci[1].stackent;
  ctx->pc = ctx->mrb->c->rescue[--ci->ridx];

#ifdef MRB_ENABLE_JIT
    //uint32_t rescue_idx = ctx->pc - ctx->irep->iseq;
    //uintptr_t jit_jmp_off = ctx->irep->jit_ctx.text_off_tbl[ctx->pc - ctx->irep->iseq];
    ////VM_PRINTF("_op_rescue: settings jmp addr for jit to: %p\n", jit_jmp_off);
    //ctx->rescue_jmp_addr = ctx->irep->jit_ctx.text + jit_jmp_off;
    ////VM_PRINTF("_op_rescue: settings jmp addr for jit to: %p %p\n", ctx->rescue_jmp_addr, jit_jmp_off);
    ////VM_PRINTF("_op_rescue: settings jmp addr to nth op: %d\n", ctx->pc - ctx->irep->iseq);
#endif
}

static inline void
_op_raise(struct op_ctx *ctx) {

  //VM_PRINTF("_op_raise\n");

  mrb_callinfo *ci;
  int eidx;

  ci = ctx->mrb->c->ci;
  mrb_obj_iv_ifnone(ctx->mrb, ctx->mrb->exc, mrb_intern_lit(ctx->mrb, "lastpc"), mrb_cptr_value(ctx->mrb, ctx->pc));
  mrb_obj_iv_ifnone(ctx->mrb, ctx->mrb->exc, mrb_intern_lit(ctx->mrb, "ciidx"), mrb_fixnum_value(ci - ctx->mrb->c->cibase));
  eidx = ci->eidx;
  if (ci == ctx->mrb->c->cibase) {
    //VM_PRINTF("calling _op_rescue 1\n");
    return _op_rescue(ctx, ci);
  }
  while (eidx > ci[-1].eidx) {
    ecall(ctx->mrb, --eidx);
  }
  while (ci[0].ridx == ci[-1].ridx) {
    cipop(ctx->mrb);
    ci = ctx->mrb->c->ci;
    ctx->mrb->c->stack = ci[1].stackent;
    if (ci[1].acc == CI_ACC_SKIP && ctx->prev_jmp) {
      ctx->mrb->jmp = ctx->prev_jmp;
      //VM_PRINTF("_op_send: throwing\n");
      MRB_THROW(ctx->prev_jmp);
    }
    if (ci == ctx->mrb->c->cibase) {
      if (ci->ridx == 0) {
        if (ctx->mrb->c == ctx->mrb->root_c) {
          ctx->regs = ctx->mrb->c->stack = ctx->mrb->c->stbase;
          //VM_PRINTF("_op_send: stopping\n");
          return _op_stop(ctx);
        }
        else {
          struct mrb_context *c = ctx->mrb->c;

          ctx->mrb->c = c->prev;
          c->prev = NULL;
          return _op_raise(ctx);
        }
      }
      break;
    }
    /* call ensure only when we skip this callinfo */
    if (ci[0].ridx == ci[-1].ridx) {
      while (eidx > ci[-1].eidx) {
        ecall(ctx->mrb, --eidx);
      }
    }
  }
  
  //VM_PRINTF("calling _op_rescue -1\n");
  return _op_rescue(ctx, ci);
}

static OP_INLINE void
op_raise(struct op_ctx *ctx) {
  /* A      raise(R(A)) */
  ctx->mrb->exc = mrb_obj_ptr(ctx->regs[GETARG_A(CTX_I(ctx))]);
  return _op_raise(ctx);
}

static OP_INLINE void
op_epush(struct op_ctx *ctx) {
  /* Bx     ensure_push(SEQ[Bx]) */
  struct RProc *p;

  p = mrb_closure_new(ctx->mrb, ctx->irep->reps[GETARG_Bx(CTX_I(ctx))]);
  /* push ensure_stack */
  if (ctx->mrb->c->esize <= ctx->mrb->c->ci->eidx) {
    if (ctx->mrb->c->esize == 0) ctx->mrb->c->esize = 16;
    else ctx->mrb->c->esize *= 2;
    ctx->mrb->c->ensure = (struct RProc **)mrb_realloc(ctx->mrb, ctx->mrb->c->ensure, sizeof(struct RProc*) * ctx->mrb->c->esize);
  }
  ctx->mrb->c->ensure[ctx->mrb->c->ci->eidx++] = p;
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static OP_INLINE void
op_epop(struct op_ctx *ctx) {
  /* A      A.times{ensure_pop().call} */
  int a = GETARG_A(CTX_I(ctx));
  mrb_callinfo *ci = ctx->mrb->c->ci;
  int n, eidx = ci->eidx;

  for (n=0; n<a && eidx > ci[-1].eidx; n++) {
    ecall(ctx->mrb, --eidx);
    ARENA_RESTORE(ctx->mrb, ctx->ai);
  }
}

static OP_INLINE void
op_loadnil(struct op_ctx *ctx) {
  /* A     R(A) := nil */
  SET_NIL_VALUE(ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static char _str_const_op_send[] = "op_send %d\n";
static char _str_const_op_send3[] = "/op_send\n";
static char _str_const_op_send2[] = "op_send2 %d\n";
static inline void
_op_send_static(struct op_ctx *ctx, mrb_value recv, struct RClass *c, mrb_sym mid, struct RProc *m, int opcode, int a, int n) {
  /* A B C  R(A) := call(R(A),Syms(B),R(A+1),...,R(A+C)) */

  mrb_callinfo *ci;
  mrb_value result;

  if (MRB_LIKELY(opcode != OP_SENDB)) {
    if (MRB_UNLIKELY(n == CALL_MAXARGS)) {
      SET_NIL_VALUE(ctx->regs[a+2]);
    }
    else {
      SET_NIL_VALUE(ctx->regs[a+n+1]);
    }
  }

  //VM_PRINTF("_op_send %s: %p %d %d %d %d\n", mrb_sym2name(ctx->mrb, mid), ctx, opcode, a, b, n);
#if 0
  recv = ctx->regs[a];
  if (MRB_LIKELY(opcode != OP_SENDB)) {
    if (MRB_UNLIKELY(n == CALL_MAXARGS)) {
      SET_NIL_VALUE(ctx->regs[a+2]);
    }
    else {
      SET_NIL_VALUE(ctx->regs[a+n+1]);
    }
  }
#endif

  ////VM_PRINTF("_op_send %s\n", mrb_sym2name(ctx->mrb, mid));

  /* push callinfo */
  ci = cipush(ctx->mrb);
  ci->mid = mid;
  ci->proc = m;
  ci->stackent = ctx->mrb->c->stack;
  ci->target_class = c;

  ci->pc = ctx->pc + 1;
  ci->acc = a;

  /* prepare stack */
  ctx->mrb->c->stack += a;

  if (MRB_PROC_CFUNC_P(m)) {
    mrb_bool flow_modified = FALSE;

    //VM_PRINTF("_op_send: cfunc (ctx = %p)\n", ctx);
    if (MRB_UNLIKELY(n == CALL_MAXARGS)) {
      ci->argc = -1;
      ci->nregs = 3;
    }
    else {
      ci->argc = n;
      //printf(_str_const_op_send, ci->argc);
      ci->nregs = n + 2;
    }

    // make op context accessible to C functions
    // that need to call back into the VM code
#ifdef MRB_ENABLE_JIT
    ctx->mrb->op_ctx = ctx;
#endif

    result = m->body.func(ctx->mrb, recv);
    ctx->mrb->c->stack[0] = result;
    mrb_gc_arena_restore(ctx->mrb, ctx->ai);
    if (MRB_UNLIKELY(ctx->mrb->exc)) {
      //VM_PRINTF("calling _op_raise from send\n");
      return _op_raise(ctx);
    }
    /* pop stackpos */
    ci = ctx->mrb->c->ci;
    if (MRB_UNLIKELY(!ci->target_class)) { /* return from context modifying method (resume/yield) */
      if (!MRB_PROC_CFUNC_P(ci[-1].proc)) {
        ctx->proc = ci[-1].proc;
        ctx->irep = ctx->proc->body.irep;
        ctx->pool = ctx->irep->pool;
        ctx->syms = ctx->irep->syms;
        flow_modified = TRUE;
      }
    }
    ctx->regs = ctx->mrb->c->stack = ci->stackent;
    ctx->pc = ci->pc;
    cipop(ctx->mrb);


#ifdef MRB_ENABLE_JIT
    /* the cfunc called above modified execution context.
     * If the new PC points to the beginning of a proc we
     * jit and run from the start.
     * Otherwise (a resume), we need to jump to the corresponding PC
     * position in native code (TODO).
     */
    if(MRB_UNLIKELY(flow_modified)) {
      if(ctx->pc == ctx->irep->iseq) {
        //VM_PRINTF("_op_send: call into jit (resume)\n");
        mrb_proc_call_jit(ctx->mrb, ctx->proc, ctx);
        //VM_PRINTF("/_op_send: call into jit (resume)\n");
      }
    }
#endif


  }
  else {

    //VM_PRINTF("_op_send: no cfunc\n");
    /* setup environment for calling method */
    ctx->proc = ctx->mrb->c->ci->proc = m;
    ctx->irep = m->body.irep;
    ctx->pool = ctx->irep->pool;
    ctx->syms = ctx->irep->syms;
    ci->nregs = ctx->irep->nregs;
    if (MRB_UNLIKELY(n == CALL_MAXARGS)) {
      ci->argc = -1;
      stack_extend(ctx->mrb, (ctx->irep->nregs < 3) ? 3 : ctx->irep->nregs, 3);
    }
    else {
      ci->argc = n;
      stack_extend(ctx->mrb, ctx->irep->nregs,  n+2);
    }
    ctx->regs = ctx->mrb->c->stack;
    ctx->pc = ctx->irep->iseq;


    ////VM_PRINTF("_op_send: pc set\n");

#ifdef MRB_ENABLE_JIT
    //VM_PRINTF("_op_send: call into jit (send)\n");
    mrb_proc_call_jit(ctx->mrb, m, ctx);
    //VM_PRINTF("/_op_send: call into jit (send)\n");
#endif
  }

  //VM_PRINTF("_op_send %s end\n", mrb_sym2name(ctx->mrb, mid));
}

static inline void
_op_send(struct op_ctx *ctx, int opcode, int a, int b, int n) {
  struct RProc *m;
  struct RClass *c;
  mrb_callinfo *ci;
  mrb_value recv, result;
  mrb_sym mid = ctx->syms[b];

  recv = ctx->regs[a];

  ////VM_PRINTF("_op_send: class\n");
  c = mrb_class(ctx->mrb, recv);
  ////VM_PRINTF("_op_send: class %p\n", c);

  m = mrb_method_search_vm_proc(ctx->mrb, ctx->proc, &c, mid);
  ////VM_PRINTF("_op_send: %p\n", m);

  if (MRB_UNLIKELY(!m)) {
    mrb_p(ctx->mrb, recv);
    mrb_value sym = mrb_symbol_value(mid);

    mid = intern_str_const(ctx->mrb, _str_const_method_missing);
    m = mrb_method_search_vm(ctx->mrb, &c, mid);
    if (n == CALL_MAXARGS) {
      mrb_ary_unshift(ctx->mrb, ctx->regs[a+1], sym);
    }
    else {
      value_move(ctx->regs+a+2, ctx->regs+a+1, ++n);
      ctx->regs[a+1] = sym;
    }
  }

  return _op_send_static(ctx, recv, c, mid, m, opcode, a, n);
}

static OP_INLINE void
op_send(struct op_ctx *ctx) {
  /* A B C R(A) := call(R(A),Syms(B),R(A+1),...,R(A+C)) */

  int a = ARG_PROTECT(GETARG_A(CTX_I(ctx)));
  int b = GETARG_B(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));

  //printf(_str_const_op_send2, n);


#ifdef MRB_JIT_GEN
  mrb_callinfo *ci = ctx->mrb->c->ci;
  //ci->send_idx = OP_IDX(CTX_I(ctx));
  ctx->pc = ctx->irep->iseq + OP_IDX(ctx);
#endif

  //VM_PRINTF(_str_const_op_send2);
  _op_send(ctx, OP_SEND, a, b, n);
  //VM_PRINTF(_str_const_op_send3);

#ifdef MRB_JIT_GEN
  if(ctx->mrb->c->ci < ci) {
    typedef void (*__op_send_exit__)();
    ((__op_send_exit__)(0xFAB))();
  }/* else if(ctx->rescue_jmp_addr != NULL) {
    typedef void (*__op_send_exit__)(struct op_ctx *, uint8_t *off);
    //((__op_send_exit__)(0xBAF))(ctx, ctx->rescue_jmp_addr);
    ctx->rescue_jmp_addr = NULL;
    //VM_PRINTF(_str_const_op_send, ctx->rescue_jmp_addr);
  }*/
#endif
}

static char _str_const_op_sendb[] = "op_sendb %d %d %d\n";
static OP_INLINE void
op_sendb(struct op_ctx *ctx) {
  /* A B C  R(A) := call(R(A),Syms(B),R(A+1),...,R(A+C),&R(A+C+1))*/

  int a = ARG_PROTECT(GETARG_A(CTX_I(ctx)));
  int b = GETARG_B(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));

 ////VM_PRINTF(_str_const_op_sendb, a, b, n);
  _op_send(ctx, OP_SENDB, a, b, n);
}

static OP_INLINE void
op_fsend(struct op_ctx *ctx) {
  /* A B C  R(A) := fcall(R(A),Syms(B),R(A+1),... ,R(A+C-1)) */
}

static inline void
_op_return(struct op_ctx *ctx, int a, int b) {
  mrb_state *mrb = ctx->mrb;
  if (MRB_UNLIKELY(ctx->mrb->exc)) {
    return _op_raise(ctx);
  } else {
    mrb_callinfo *ci = ctx->mrb->c->ci;
    int acc, eidx = ctx->mrb->c->ci->eidx;
    mrb_value v = ctx->regs[a];

    switch (b) {
    case OP_R_RETURN:
      /* Fall through to OP_R_NORMAL otherwise */
      if (ctx->proc->env && !MRB_PROC_STRICT_P(ctx->proc)) {
        struct REnv *e = top_env(mrb, ctx->proc);

        if (!MRB_ENV_STACK_SHARED_P(e)) {
          localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
          return _op_raise(ctx);
        }
        ci = mrb->c->cibase + e->cioff;
        if (ci == mrb->c->cibase) {
          localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
          return _op_raise(ctx);
        }
        mrb->c->stack = mrb->c->ci->stackent;
        mrb->c->ci = ci;
        break;
      }
    case OP_R_NORMAL:
      if (ci == mrb->c->cibase) {
        if (!mrb->c->prev) { /* toplevel return */
          localjump_error(mrb, LOCALJUMP_ERROR_RETURN);
          return _op_raise(ctx);
        }
        if (mrb->c->prev->ci == mrb->c->prev->cibase) {
          mrb_value exc = exc_new_str_const(mrb, E_FIBER_ERROR, _str_const_double_resume);
          mrb->exc = mrb_obj_ptr(exc);
          return _op_raise(ctx);
        }
        /* automatic yield at the end */
        mrb->c->status = MRB_FIBER_TERMINATED;
        mrb->c = mrb->c->prev;
        mrb->c->status = MRB_FIBER_RUNNING;
      }
      ci = mrb->c->ci;
      break;
    case OP_R_BREAK:
     ////VM_PRINTF("break\n");
      if (!ctx->proc->env || !MRB_ENV_STACK_SHARED_P(ctx->proc->env)) {
        localjump_error(mrb, LOCALJUMP_ERROR_BREAK);
        return _op_raise(ctx);
      }
      /* break from fiber block */
      if (mrb->c->ci == mrb->c->cibase && mrb->c->ci->pc) {
        struct mrb_context *c = mrb->c;

        mrb->c = c->prev;
        c->prev = NULL;
      }
      ci = mrb->c->ci;
      mrb->c->stack = ci->stackent;
      mrb->c->ci = mrb->c->cibase + ctx->proc->env->cioff + 1;
      while (ci > mrb->c->ci) {
        if (ci[-1].acc == CI_ACC_SKIP) {
          mrb->c->ci = ci;
          break;
        }
        ci--;
      }
      break;
    default:
      /* cannot happen */
      break;
    }
    while (eidx > mrb->c->ci[-1].eidx) {
      ecall(mrb, --eidx);
    }
    cipop(mrb);
    acc = ci->acc;
    ctx->pc = ci->pc;
    ctx->regs = mrb->c->stack = ci->stackent;
    if (acc == CI_ACC_SKIP) {
      mrb->jmp = ctx->prev_jmp;
      ctx->retval = v;
      MRB_THROW(ctx->stop_jmp);
    }
    //VM_PRINTF("from :%s\n", mrb_sym2name(mrb, ci->mid));
    ctx->proc = mrb->c->ci->proc;
    ctx->irep = ctx->proc->body.irep;
    ctx->pool = ctx->irep->pool;
    ctx->syms = ctx->irep->syms;
    ctx->regs[acc] = v;

    //VM_PRINTF("new pc: %p %d\n", ctx->pc, GET_OPCODE(*ctx->pc));

  }
}

static OP_INLINE void
op_break(struct op_ctx *ctx) {
  /* A B     return R(A) (B=normal,in-block return/break) */
 ////VM_PRINTF(_str_const_op_return, ctx->mrb->c->ci);
  _op_return(ctx, GETARG_A(CTX_I(ctx)), GETARG_B(CTX_I(ctx)));
 ////VM_PRINTF(_str_const_op_return, ctx->mrb->c->ci);
}

static const char _str_const_op_return[] = "op_return: %p\n";
static OP_INLINE void
op_return(struct op_ctx *ctx) {
  /* A B     return R(A) (B=normal,in-block return/break) */
 ////VM_PRINTF(_str_const_op_return, ctx->mrb->c->ci);
  _op_return(ctx, GETARG_A(CTX_I(ctx)), OP_R_NORMAL);
 ////VM_PRINTF(_str_const_op_return, ctx->mrb->c->ci);
}

static const char _str_const_op_call[] = "op_call: %d\n";
static inline void
_op_call(struct op_ctx *ctx, int a) {
  /* A      R(A) := self.call(frame.argc, frame.argv) */
  mrb_callinfo *ci;
  mrb_value recv = ctx->mrb->c->stack[0];
  struct RProc *m = mrb_proc_ptr(recv);

  ////VM_PRINTF("_op_call: %d\n", a);

  /* replace callinfo */
  ci = ctx->mrb->c->ci;
  ci->target_class = m->target_class;
  ci->proc = m;
  if (m->env) {
    if (m->env->mid) {
      ci->mid = m->env->mid;
    }
    if (!m->env->stack) {
      m->env->stack = ctx->mrb->c->stack;
    }
  }

  /* prepare stack */
  if (MRB_PROC_CFUNC_P(m)) {
    recv = m->body.func(ctx->mrb, recv);
    mrb_gc_arena_restore(ctx->mrb, ctx->ai);
    if (ctx->mrb->exc) return _op_raise(ctx);
    /* pop stackpos */
    ci = ctx->mrb->c->ci;
    ctx->regs = ctx->mrb->c->stack = ci->stackent;
    ctx->regs[ci->acc] = recv;
    ctx->pc = ci->pc;
    cipop(ctx->mrb);
    ctx->irep = ctx->mrb->c->ci->proc->body.irep;
    ctx->pool = ctx->irep->pool;
    ctx->syms = ctx->irep->syms;
  }
  else {
    /* setup environment for calling method */
    ctx->proc = m;
    ctx->irep = m->body.irep;
    if (!ctx->irep) {
      ctx->mrb->c->stack[0] = mrb_nil_value();
      return _op_return(ctx, a, OP_R_NORMAL);
    }

    ctx->pool = ctx->irep->pool;
    ctx->syms = ctx->irep->syms;
    ci->nregs = ctx->irep->nregs;

    if (ci->argc < 0) {
      stack_extend(ctx->mrb, (ctx->irep->nregs < 3) ? 3 : ctx->irep->nregs, 3);
    }
    else {
      stack_extend(ctx->mrb, ctx->irep->nregs, ci->argc+2);
    }
    ctx->regs = ctx->mrb->c->stack;
    ctx->regs[0] = m->env->stack[0];
    ctx->pc = ctx->irep->iseq;

#ifdef MRB_ENABLE_JIT
    mrb_proc_call_jit(ctx->mrb, ctx->proc, ctx);
#endif
  }
  ////VM_PRINTF("_op_call: end\n");
}

static OP_INLINE void
op_call(struct op_ctx *ctx) {
  _op_call(ctx, GETARG_A(CTX_I(ctx)));
  ////VM_PRINTF(_str_const_op_call, GETARG_A(CTX_I(ctx)));
}

static OP_INLINE void
op_super(struct op_ctx *ctx) {
  /* A C  R(A) := super(R(A+1),... ,R(A+C+1)) */
  mrb_value recv;
  mrb_callinfo *ci = ctx->mrb->c->ci;
  struct RProc *m;
  struct RClass *c;
  mrb_sym mid = ci->mid;
  int a = GETARG_A(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));

  if (mid == 0) {
    mrb_value exc;
    mrb_state *mrb = ctx->mrb;
    exc = exc_new_str_const(ctx->mrb, E_NOMETHOD_ERROR, _str_const_super_outside_method);
    mrb->exc = mrb_obj_ptr(exc);
    return _op_raise(ctx);
  }

  recv = ctx->regs[0];
  c = ctx->mrb->c->ci->target_class->super;
  m = mrb_method_search_vm_proc(ctx->mrb, ctx->proc, &c, mid);
  if (!m) {
    mid = intern_str_const(ctx->mrb, _str_const_method_missing);
    m = mrb_method_search_vm(ctx->mrb, &c, mid);
    if (n == CALL_MAXARGS) {
      mrb_ary_unshift(ctx->mrb, ctx->regs[a+1], mrb_symbol_value(ci->mid));
    }
    else {
      value_move(ctx->regs+a+2, ctx->regs+a+1, ++n);
      SET_SYM_VALUE(ctx->regs[a+1], ci->mid);
    }
  }

  /* push callinfo */
  ci = cipush(ctx->mrb);
  ci->mid = mid;
  ci->proc = m;
  ci->stackent = ctx->mrb->c->stack;
  if (n == CALL_MAXARGS) {
    ci->argc = -1;
  }
  else {
    ci->argc = n;
  }
  ci->target_class = c;
  ci->pc = ctx->pc + 1;

  /* prepare stack */
  ctx->mrb->c->stack += a;
  ctx->mrb->c->stack[0] = recv;

  if (MRB_PROC_CFUNC_P(m)) {
    if (n == CALL_MAXARGS) {
      ci->nregs = 3;
    }
    else {
      ci->nregs = n + 2;
    }
    ctx->mrb->c->stack[0] = m->body.func(ctx->mrb, recv);
    mrb_gc_arena_restore(ctx->mrb, ctx->ai);
    if (ctx->mrb->exc) return _op_raise(ctx);
    /* pop stackpos */
    ctx->regs = ctx->mrb->c->stack = ctx->mrb->c->ci->stackent;
    cipop(ctx->mrb);
    PC_INC(ctx->pc);
  }
  else {
    /* fill callinfo */
    ci->acc = a;

    /* setup environment for calling method */
    ci->proc = m;
    ctx->irep = m->body.irep;
    ctx->pool = ctx->irep->pool;
    ctx->syms = ctx->irep->syms;
    ci->nregs = ctx->irep->nregs;
    if (n == CALL_MAXARGS) {
      stack_extend(ctx->mrb, (ctx->irep->nregs < 3) ? 3 : ctx->irep->nregs, 3);
    }
    else {
      stack_extend(ctx->mrb, ctx->irep->nregs, ci->argc+2);
    }
    ctx->regs = ctx->mrb->c->stack;
    ctx->pc = ctx->irep->iseq;

#ifdef MRB_ENABLE_JIT
    mrb_proc_call_jit(ctx->mrb, m, ctx);
#endif

  }
}

static OP_INLINE void
op_argary(struct op_ctx *ctx) {
  /* A Bx   R(A) := argument array (16=6:1:5:4) */
  int a = GETARG_A(CTX_I(ctx));
  JIT_VOLATILE int bx = GETARG_Bx(CTX_I(ctx));
  int m1 = (bx>>10)&0x3f;
  int r  = (bx>>9)&0x1;
  int m2 = (bx>>4)&0x1f;
  int lv = (bx>>0)&0xf;
  mrb_value *stack;

  if (lv == 0) stack = ctx->regs + 1;
  else {
    struct REnv *e = uvenv(ctx->mrb, lv-1);
    if (!e) {
      mrb_value exc;
      mrb_state *mrb = ctx->mrb;
      exc = exc_new_str_const(ctx->mrb, E_NOMETHOD_ERROR, _str_const_super_outside_method);

      mrb->exc = mrb_obj_ptr(exc);
      return _op_raise(ctx);
    }
    stack = e->stack + 1;
  }
  if (r == 0) {
    ctx->regs[a] = mrb_ary_new_from_values(ctx->mrb, m1+m2, stack);
  }
  else {
    mrb_value *pp = NULL;
    struct RArray *rest;
    int len = 0;

    if (mrb_array_p(stack[m1])) {
      struct RArray *ary = mrb_ary_ptr(stack[m1]);

      pp = ary->ptr;
      len = ary->len;
    }
    ctx->regs[a] = mrb_ary_new_capa(ctx->mrb, m1+len+m2);
    rest = mrb_ary_ptr(ctx->regs[a]);
    if (m1 > 0) {
      stack_copy(rest->ptr, stack, m1);
    }
    if (len > 0) {
      stack_copy(rest->ptr+m1, pp, len);
    }
    if (m2 > 0) {
      stack_copy(rest->ptr+m1+len, stack+m1+1, m2);
    }
    rest->len = m1+len+m2;
  }
  ctx->regs[a+1] = stack[m1+r+m2];
  ARENA_RESTORE(ctx->mrb, ctx->ai);
  PC_INC(ctx->pc);
}

static OP_INLINE void
op_enter_method_m(struct op_ctx *ctx) {
  /* Ax             arg setup according to flags (23=5:5:1:5:5:1:1) */
  mrb_aspec ax = ARG_PROTECT(GETARG_Ax(CTX_I(ctx)));
  int m1 = MRB_ASPEC_REQ(ax);

  /* unused
  int k  = MRB_ASPEC_KEY(ax);
  int kd = MRB_ASPEC_KDICT(ax);
  int b  = MRB_ASPEC_BLOCK(ax);
  */

  int argc = ctx->mrb->c->ci->argc;
  mrb_value *argv = ctx->regs+1;
  mrb_value *argv0 = argv;
  int paramc = m1;
  mrb_value *blk = &argv[argc < 0 ? 1 : argc];
  int rnum = 0;

#ifdef MRB_JIT_GEN
  uint16_t jit_jmp_off = 0;
#endif

  if (MRB_UNLIKELY(!mrb_nil_p(*blk) && mrb_type(*blk) != MRB_TT_PROC)) {
    *blk = mrb_convert_type(ctx->mrb, *blk, MRB_TT_PROC, _str_const_proc, _str_const_to_proc);
  }
  if (argc < 0) {
    struct RArray *ary = mrb_ary_ptr(ctx->regs[1]);
    argv = ary->ptr;
    argc = ary->len;
    mrb_gc_protect(ctx->mrb, ctx->regs[1]);
  } else {
    if (argc < m1) {
      argnum_error(ctx->mrb, m1);
      return _op_raise(ctx);
    }
  }

  ctx->mrb->c->ci->argc = paramc;

  ctx->regs[paramc+1] = *blk; /* move block */
  if (argv0 != argv) {
    value_move(&ctx->regs[1], argv, m1);
  }

  PC_INC(ctx->pc);
#ifdef MRB_JIT_GEN
  jit_jmp_off = ctx->irep->jit_ctx.text_off_tbl[1];
#endif


#ifdef MRB_JIT_GEN
  ////VM_PRINTF(_str_const_op_enter, ((uintptr_t)ctx->irep->jit_ctx.text) + jit_jmp_off, jit_jmp_off);
  typedef void (*__op_enter_exit__)(struct op_ctx *, uintptr_t off);
  ((__op_enter_exit__)(0xFAB))(ctx, ((uintptr_t)ctx->irep->jit_ctx.text) + jit_jmp_off);
#endif
}


static const char _str_const_op_enter[] = "op_enter: %p (%d)\n";
static OP_INLINE void
op_enter(struct op_ctx *ctx) {
  /* Ax             arg setup according to flags (23=5:5:1:5:5:1:1) */
  mrb_aspec ax = ARG_PROTECT(GETARG_Ax(CTX_I(ctx)));
  int m1 = MRB_ASPEC_REQ(ax);
  int o  = MRB_ASPEC_OPT(ax);
  int r  = MRB_ASPEC_REST(ax);
  int m2 = MRB_ASPEC_POST(ax);
  /* unused
  int k  = MRB_ASPEC_KEY(ax);
  int kd = MRB_ASPEC_KDICT(ax);
  int b  = MRB_ASPEC_BLOCK(ax);
  */

  int argc = ctx->mrb->c->ci->argc;
  mrb_value *argv = ctx->regs+1;
  mrb_value *argv0 = argv;
  int paramc = m1 + o + r + m2;
  mrb_value *blk = &argv[argc < 0 ? 1 : argc];

#ifdef MRB_JIT_GEN
  uint16_t jit_jmp_off = 0;
#endif

  if ((!mrb_nil_p(*blk) && mrb_type(*blk) != MRB_TT_PROC)) {
    *blk = mrb_convert_type(ctx->mrb, *blk, MRB_TT_PROC, _str_const_proc, _str_const_to_proc);
  }

  printf(_str_const_op_enter, blk, ax);

  if (argc < 0) {
    struct RArray *ary = mrb_ary_ptr(ctx->regs[1]);
    argv = ary->ptr;
    argc = ary->len;
    mrb_gc_protect(ctx->mrb, ctx->regs[1]);
  }
  if (ctx->mrb->c->ci->proc && MRB_PROC_STRICT_P(ctx->mrb->c->ci->proc)) {
    if (argc >= 0) {
      if (argc < m1 + m2 || (r == 0 && argc > paramc)) {
        argnum_error(ctx->mrb, m1+m2);
        return _op_raise(ctx);
      }
    }
  }
  else if (paramc > 1 && argc == 1 && mrb_array_p(argv[0])) {
    mrb_gc_protect(ctx->mrb, argv[0]);
    argc = mrb_ary_ptr(argv[0])->len;
    argv = mrb_ary_ptr(argv[0])->ptr;
  }
  ctx->mrb->c->ci->argc = paramc;
  if (argc < paramc) {
    int mlen;
    if (argc < m1+m2) {
      if (argc <= m1)
        mlen = 0;
      else
        mlen = argc - m1;
    } else {
      mlen = m2;
    }
    ctx->regs[paramc+1] = *blk; /* move block */
    SET_NIL_VALUE(ctx->regs[argc+1]);
    if (argv0 != argv) {
      value_move(&ctx->regs[1], argv, argc-mlen); /* m1 + o */
    }
    if (mlen) {
      value_move(&ctx->regs[paramc-m2+1], &argv[argc-mlen], mlen);
    }
    if (r) {
      ctx->regs[m1+o+1] = mrb_ary_new_capa(ctx->mrb, 0);
    }
    if (o == 0 || argc < m1+m2) {
      PC_INC(ctx->pc);
#ifdef MRB_JIT_GEN
      jit_jmp_off = ctx->irep->jit_ctx.text_off_tbl[1];
#endif
    }
    else {
      PC_ADD(ctx->pc, ctx->irep->oa_off[argc - m1 - m2]);
#ifdef MRB_JIT_GEN
      jit_jmp_off = ctx->irep->jit_ctx.text_off_tbl[argc - m1 - m2 + 1];
#endif
    }
  }
  else {
    int rnum = 0;
    if (argv0 != argv) {
      ctx->regs[paramc+1] = *blk; /* move block */
      value_move(&ctx->regs[1], argv, m1+o);
    }
    if (r) {
      rnum = argc-m1-o-m2;
      ctx->regs[m1+o+1] = mrb_ary_new_from_values(ctx->mrb, rnum, argv+m1+o);
    }
    if (m2) {
      if (argc-m2 > m1) {
        value_move(&ctx->regs[m1+o+r+1], &argv[m1+o+rnum], m2);
      }
    }
    if (argv0 == argv) {
      ctx->regs[paramc+1] = *blk; /* move block */
    }

    if(o > 0) {
      PC_ADD(ctx->pc, ctx->irep->oa_off[o]);
#ifdef MRB_JIT_GEN
      jit_jmp_off = ctx->irep->jit_ctx.text_off_tbl[o + 1];
#endif
    }
    else {
      PC_INC(ctx->pc);
#ifdef MRB_JIT_GEN
      jit_jmp_off = ctx->irep->jit_ctx.text_off_tbl[1];
#endif
    }

  }


#ifdef MRB_JIT_GEN
  ////VM_PRINTF(_str_const_op_enter, ((uintptr_t)ctx->irep->jit_ctx.text) + jit_jmp_off, jit_jmp_off);
  typedef void (*__op_enter_exit__)(struct op_ctx *, uintptr_t off);
  ((__op_enter_exit__)(0xFAB))(ctx, ((uintptr_t)ctx->irep->jit_ctx.text) + jit_jmp_off);
#endif
}

static OP_INLINE void
op_karg(struct op_ctx *ctx) {
  /* A B C          R(A) := kdict[Syms(B)]; if C kdict.rm(Syms(B)) */
  /* if C == 2; raise unless kdict.empty? */
  /* OP_JMP should follow to skip init code */
}

static OP_INLINE void
op_kdict(struct op_ctx *ctx) {
  /* A C            R(A) := kdict */
}

static OP_INLINE void
op_tailcall(struct op_ctx *ctx) {
  /* A B C  return call(R(A),Syms(B),R(A+1),... ,R(A+C+1)) */

  mrb_state *mrb = ctx->mrb;

  int a = GETARG_A(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));
  struct RProc *m;
  struct RClass *c;
  mrb_callinfo *ci;
  mrb_value recv;
  mrb_sym mid = ctx->syms[GETARG_B(CTX_I(ctx))];

  recv = ctx->regs[a];
  c = mrb_class(ctx->mrb, recv);
  m = mrb_method_search_vm_proc(mrb, ctx->proc, &c, mid);
  if (!m) {
    mrb_value sym = mrb_symbol_value(mid);

    mid = intern_str_const(mrb, _str_const_method_missing);
    m = mrb_method_search_vm(mrb, &c, mid);
    if (n == CALL_MAXARGS) {
      mrb_ary_unshift(mrb, ctx->regs[a+1], sym);
    }
    else {
      value_move(ctx->regs+a+2, ctx->regs+a+1, ++n);
      ctx->regs[a+1] = sym;
    }
  }

  /* replace callinfo */
  ci = mrb->c->ci;
  ci->mid = mid;
  ci->target_class = c;
  if (n == CALL_MAXARGS) {
    ci->argc = -1;
  }
  else {
    ci->argc = n;
  }

  /* move stack */
  value_move(mrb->c->stack, &ctx->regs[a], ci->argc+1);

  if (MRB_PROC_CFUNC_P(m)) {
    mrb->c->stack[0] = m->body.func(mrb, recv);
    mrb_gc_arena_restore(mrb, ctx->ai);
    return _op_return(ctx, a, OP_R_NORMAL);
  }
  else {
    /* setup environment for calling method */
    ctx->irep = m->body.irep;
    ctx->pool = ctx->irep->pool;
    ctx->syms = ctx->irep->syms;
    if (ci->argc < 0) {
      stack_extend(mrb, (ctx->irep->nregs < 3) ? 3 : ctx->irep->nregs, 3);
    }
    else {
      stack_extend(mrb, ctx->irep->nregs, ci->argc+2);
    }
    ctx->regs = mrb->c->stack;
    ctx->pc = ctx->irep->iseq;

#ifdef MRB_ENABLE_JIT
    mrb_proc_call_jit(ctx->mrb, m, ctx);
#endif

  }
}

static OP_INLINE void
op_blkpush(struct op_ctx *ctx) {
  /* A Bx   R(A) := block (16=6:1:5:4) */

  mrb_state *mrb = ctx->mrb;
  int a = GETARG_A(CTX_I(ctx));
  JIT_VOLATILE int bx = GETARG_Bx(CTX_I(ctx));
  int m1 = (bx>>10)&0x3f;
  int r  = (bx>>9)&0x1;
  int m2 = (bx>>4)&0x1f;
  int lv = (bx>>0)&0xf;
  mrb_value *stack;

  if (lv == 0) stack = ctx->regs + 1;
  else {
    struct REnv *e = uvenv(ctx->mrb, lv-1);
    if (!e) {
      localjump_error(mrb, LOCALJUMP_ERROR_YIELD);
      return _op_raise(ctx);
    }
    stack = e->stack + 1;
  }
  ctx->regs[a] = stack[m1+r+m2];
  PC_INC(ctx->pc);
}

#define TYPES2(a,b) ((((uint16_t)(a))<<8)|(((uint16_t)(b))&0xff))
#define OP_MATH_BODY(op,v1,v2) do {\
  v1(regs[a]) = v1(regs[a]) op v2(regs[a+1]);\
} while(0)

static OP_INLINE void
op_add(struct op_ctx *ctx) {

  /* A B C  R(A) := R(A)+R(A+1) (Syms[B]=:+,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  mrb_state *mrb = ctx->mrb;
  mrb_value *regs = ctx->regs;

  /* need to check if op is overridden */
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):
    {
      mrb_int x, y, z;
      mrb_value *regs_a = regs + a;

      x = mrb_fixnum(regs_a[0]);
      y = mrb_fixnum(regs_a[1]);
      if (mrb_int_add_overflow(x, y, &z)) {
        SET_FLOAT_VALUE(mrb, regs_a[0], (mrb_float)x + (mrb_float)y);
        break;
      }
      SET_INT_VALUE(regs[a], z);
    }
    break;
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):
    {
      mrb_int x = mrb_fixnum(regs[a]);
      mrb_float y = mrb_float(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x + y);
    }
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      mrb_int y = mrb_fixnum(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], x + y);
    }
#else
    OP_MATH_BODY(+,mrb_float,mrb_fixnum);
#endif
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      mrb_float y = mrb_float(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], x + y);
    }
#else
    OP_MATH_BODY(+,mrb_float,mrb_float);
#endif
    break;
  case TYPES2(MRB_TT_STRING,MRB_TT_STRING):
    regs[a] = mrb_str_plus(mrb, regs[a], regs[a+1]);
    break;
  default:
    return op_send(ctx);
  }
  ARENA_RESTORE(mrb, ctx->ai);
  PC_INC(ctx->pc);
}


static OP_INLINE void
op_sub(struct op_ctx *ctx) {

  /* A B C  R(A) := R(A)-R(A+1) (Syms[B]=:-,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  mrb_value *regs = ctx->regs;
  mrb_state *mrb = ctx->mrb;

  /* need to check if op is overridden */
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):
    {
      mrb_int x, y, z;

      x = mrb_fixnum(regs[a]);
      y = mrb_fixnum(regs[a+1]);
      if (mrb_int_sub_overflow(x, y, &z)) {
        SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x - (mrb_float)y);
        break;
      }
      SET_INT_VALUE(regs[a], z);
    }
    break;
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):
    {
      mrb_int x = mrb_fixnum(regs[a]);
      mrb_float y = mrb_float(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x - y);
    }
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      mrb_int y = mrb_fixnum(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], x - y);
    }
#else
    OP_MATH_BODY(-,mrb_float,mrb_fixnum);
#endif
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      mrb_float y = mrb_float(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], x - y);
    }
#else
    OP_MATH_BODY(-,mrb_float,mrb_float);
#endif
    break;
  default:
    return op_send(ctx);
  }
  PC_INC(ctx->pc);
}


static OP_INLINE void
op_mul(struct op_ctx *ctx) {

  /* A B C  R(A) := R(A)*R(A+1) (Syms[B]=:*,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  mrb_state *mrb = ctx->mrb;
  mrb_value *regs = ctx->regs;

  /* need to check if op is overridden */
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):
    {
      mrb_value z;

      z = mrb_fixnum_mul(mrb, regs[a], regs[a+1]);

      switch (mrb_type(z)) {
      case MRB_TT_FIXNUM:
        {
          SET_INT_VALUE(regs[a], mrb_fixnum(z));
        }
        break;
      case MRB_TT_FLOAT:
        {
          SET_FLOAT_VALUE(mrb, regs[a], mrb_float(z));
        }
        break;
      default:
        /* cannot happen */
        break;
      }
    }
    break;
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):
    {
      mrb_int x = mrb_fixnum(regs[a]);
      mrb_float y = mrb_float(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x * y);
    }
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      mrb_int y = mrb_fixnum(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], x * y);
    }
#else
    OP_MATH_BODY(*,mrb_float,mrb_fixnum);
#endif
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      mrb_float y = mrb_float(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], x * y);
    }
#else
    OP_MATH_BODY(*,mrb_float,mrb_float);
#endif
    break;
  default:
    return op_send(ctx);
  }
  PC_INC(ctx->pc);
}

static OP_INLINE void
op_div(struct op_ctx *ctx) {

  /* A B C  R(A) := R(A)/R(A+1) (Syms[B]=:/,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  mrb_value *regs = ctx->regs;
  mrb_state *mrb = ctx->mrb;

  /* need to check if op is overridden */
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):
    {
      mrb_int x = mrb_fixnum(regs[a]);
      mrb_int y = mrb_fixnum(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x / (mrb_float)y);
    }
    break;
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):
    {
      mrb_int x = mrb_fixnum(regs[a]);
      mrb_float y = mrb_float(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], (mrb_float)x / y);
    }
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      mrb_int y = mrb_fixnum(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], x / y);
    }
#else
    OP_MATH_BODY(/,mrb_float,mrb_fixnum);
#endif
    break;
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      mrb_float y = mrb_float(regs[a+1]);
      SET_FLOAT_VALUE(mrb, regs[a], x / y);
    }
#else
    OP_MATH_BODY(/,mrb_float,mrb_float);
#endif
    break;
  default:
    return op_send(ctx);
  }
#ifdef MRB_NAN_BOXING
  if (isnan(mrb_float(regs[a]))) {
    regs[a] = mrb_float_value(ctx->mrb, mrb_float(regs[a]));
  }
#endif
  PC_INC(ctx->pc);
}

static const char _str_const_op_addi[] = "op_addi %d\n";
static OP_INLINE void
op_addi(struct op_ctx *ctx) {
  /* A B C  R(A) := R(A)+C (Syms[B]=:+)*/
  int a = GETARG_A(CTX_I(ctx));
  int c = ARG_PROTECT(GETARG_C(CTX_I(ctx)));
  //volatile int *ptr = (int *)0xFABBA;
  //int c = *ptr;

  mrb_value *regs_a = ctx->regs + a;
  mrb_state *mrb = ctx->mrb;

  /* need to check if + is overridden */
  switch (mrb_type(regs_a[0])) {
  case MRB_TT_FIXNUM:
    {
      mrb_int x = mrb_fixnum(regs_a[0]);
      mrb_int y = c;
      mrb_int z;

      if (mrb_int_add_overflow(x, y, &z)) {
        SET_FLOAT_VALUE(mrb, regs_a[0], (mrb_float)x + (mrb_float)y);
        break;
      }
      SET_INT_VALUE(regs_a[0], z);
    }
    break;
  case MRB_TT_FLOAT:
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs_a[0]);
      SET_FLOAT_VALUE(mrb, regs_a[0], x + c);
    }
#else
    mrb_float(regs_a[0]) += c;
#endif
    break;
  default:
    SET_INT_VALUE(regs_a[1], c);
   ////VM_PRINTF(_str_const_op_addi, a);
    return _op_send(ctx, OP_SEND, a, GETARG_B(CTX_I(ctx)), 1);
   ////VM_PRINTF(_str_const_op_addi, a);
  }
  PC_INC(ctx->pc);
}

char _str_const_op_subi[] = "op_subi %ld\n";
char _str_const_op_subi2[] = "op_subi type %ld\n";
static OP_INLINE void
op_subi(struct op_ctx *ctx) {
  /* A B C  R(A) := R(A)+C (Syms[B]=:+)*/
  /* A B C  R(A) := R(A)-C (Syms[B]=:-)*/
  int a = GETARG_A(CTX_I(ctx));
  int c = ARG_PROTECT(GETARG_C(CTX_I(ctx)));
  mrb_value *regs = ctx->regs;
  mrb_value *regs_a = regs + a;
  mrb_state *mrb = ctx->mrb;

 ////VM_PRINTF(_str_const_op_subi, a);

  /* need to check if + is overridden */
  switch (mrb_type(regs_a[0])) {
  case MRB_TT_FIXNUM:
    {
      mrb_int x = mrb_fixnum(regs_a[0]);
      mrb_int y = c;
      mrb_int z;

      if (mrb_int_sub_overflow(x, y, &z)) {
        SET_FLOAT_VALUE(mrb, regs_a[0], (mrb_float)x - (mrb_float)y);
      }
      else {
        SET_INT_VALUE(regs_a[0], z);
      }
    }
    break;
  case MRB_TT_FLOAT:
#ifdef MRB_WORD_BOXING
    {
      mrb_float x = mrb_float(regs[a]);
      SET_FLOAT_VALUE(mrb, regs[a], x - c);
    }
#else
    mrb_float(regs_a[0]) -= c;
#endif
    break;
  default:
    SET_INT_VALUE(regs_a[1], c);
    return _op_send(ctx, OP_SEND, a, GETARG_B(CTX_I(ctx)), 1);
  }
  PC_INC(ctx->pc);
}

#define OP_CMP_BODY(op,v1,v2) (v1(regs[a]) op v2(regs[a+1]))

#define OP_CMP(op) do {\
  int result;\
  /* need to check if - is overridden */\
  switch (TYPES2(mrb_type(regs[a]),mrb_type(regs[a+1]))) {\
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FIXNUM):\
    result = OP_CMP_BODY(op,mrb_fixnum,mrb_fixnum);\
    break;\
  case TYPES2(MRB_TT_FIXNUM,MRB_TT_FLOAT):\
    result = OP_CMP_BODY(op,mrb_fixnum,mrb_float);\
    break;\
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FIXNUM):\
    result = OP_CMP_BODY(op,mrb_float,mrb_fixnum);\
    break;\
  case TYPES2(MRB_TT_FLOAT,MRB_TT_FLOAT):\
    result = OP_CMP_BODY(op,mrb_float,mrb_float);\
    break;\
  default:\
    return _op_send(ctx, OP_SEND, a, b, n);\
    return op_send(ctx);\
  }\
  if (result) {\
    SET_TRUE_VALUE(regs[a]);\
  }\
  else {\
    SET_FALSE_VALUE(regs[a]);\
  }\
} while(0)


DEBUG(static char _str_const_op_eq[] = "op_eq %ld\n");
static OP_INLINE void
op_eq(struct op_ctx *ctx) {
  /* A B C  R(A) := R(A)==R(A+1) (Syms[B]=:==,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  int b = GETARG_B(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));

  mrb_value *regs = ctx->regs;
  mrb_value *regs_a = regs + a;

  //VM_PRINTF(_str_const_op_eq, a));

  if (mrb_obj_eq(ctx->mrb, regs_a[0], regs_a[1])) {
    SET_TRUE_VALUE(regs_a[0]);
  }
  else {
    OP_CMP(==);
  }

  PC_INC(ctx->pc);
}

static char _str_const_op_lt[] = "op_lt %ld\n";
static OP_INLINE void
op_lt(struct op_ctx *ctx) {
  /* A B C  R(A) := R(A)<R(A+1) (Syms[B]=:<,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  int b = GETARG_B(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));

 ////VM_PRINTF(_str_const_op_lt, a);

  mrb_value *regs = ctx->regs;
  OP_CMP(<);


  PC_INC(ctx->pc);
}

static OP_INLINE void
op_le(struct op_ctx *ctx) {
  /* A B C  R(A) := R(A)<=R(A+1) (Syms[B]=:<=,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  int b = GETARG_B(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));

  mrb_value *regs = ctx->regs;
  OP_CMP(<=);
  PC_INC(ctx->pc);
}

static OP_INLINE void
op_gt(struct op_ctx *ctx) {
  /* A B C  R(A) := R(A)>R(A+1) (Syms[B]=:>,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  int b = GETARG_B(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));

  mrb_value *regs = ctx->regs;
  OP_CMP(>);
  PC_INC(ctx->pc);
}

static OP_INLINE void
op_ge(struct op_ctx *ctx) {
  /* A B C  R(A) := R(A)>=R(A+1) (Syms[B]=:>=,C=1)*/
  int a = GETARG_A(CTX_I(ctx));
  int b = GETARG_B(CTX_I(ctx));
  int n = GETARG_C(CTX_I(ctx));

  mrb_value *regs = ctx->regs;
  OP_CMP(>=);
  PC_INC(ctx->pc);
}

static OP_INLINE void
op_array(struct op_ctx *ctx) {
  /* A B C          R(A) := ary_new(R(B),R(B+1)..R(B+C)) */
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_ary_new_from_values(ctx->mrb, GETARG_C(CTX_I(ctx)), &ctx->regs[GETARG_B(CTX_I(ctx))]);
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static OP_INLINE void
op_arycat(struct op_ctx *ctx) {
  /* A B            mrb_ary_concat(R(A),R(B)) */
  mrb_ary_concat(ctx->mrb, ctx->regs[GETARG_A(CTX_I(ctx))],
                 mrb_ary_splat(ctx->mrb, ctx->regs[GETARG_B(CTX_I(ctx))]));
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static OP_INLINE void
op_arypush(struct op_ctx *ctx) {
  /* A B            R(A).push(R(B)) */
  mrb_ary_push(ctx->mrb, ctx->regs[GETARG_A(CTX_I(ctx))], ctx->regs[GETARG_B(CTX_I(ctx))]);
}

static OP_INLINE void
op_aref(struct op_ctx *ctx) {
  /* A B C          R(A) := R(B)[C] */
  int a = GETARG_A(CTX_I(ctx));
  int c = GETARG_C(CTX_I(ctx));
  mrb_value v = ctx->regs[GETARG_B(CTX_I(ctx))];

  if (!mrb_array_p(v)) {
    if (c == 0) {
      ctx->regs[GETARG_A(CTX_I(ctx))] = v;
    }
    else {
      SET_NIL_VALUE(ctx->regs[a]);
    }
  }
  else {
    ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_ary_ref(ctx->mrb, v, c);
  }
}

static OP_INLINE void
op_aset(struct op_ctx *ctx) {
  /* A B C          R(B)[C] := R(A) */
  mrb_ary_set(ctx->mrb, ctx->regs[GETARG_B(CTX_I(ctx))], GETARG_C(CTX_I(ctx)), ctx->regs[GETARG_A(CTX_I(ctx))]);
}

static OP_INLINE void
op_apost(struct op_ctx *ctx) {
  /* A B C  *R(A),R(A+1)..R(A+C) := R(A) */
  int a = GETARG_A(CTX_I(ctx));
  mrb_value v = ctx->regs[a];
  int pre  = GETARG_B(CTX_I(ctx));
  int post = GETARG_C(CTX_I(ctx));

  if (!mrb_array_p(v)) {
    ctx->regs[a++] = mrb_ary_new_capa(ctx->mrb, 0);
    while (post--) {
      SET_NIL_VALUE(ctx->regs[a]);
      a++;
    }
  }
  else {
    struct RArray *ary = mrb_ary_ptr(v);
    int len = ary->len;
    int idx;

    if (len > pre + post) {
      ctx->regs[a++] = mrb_ary_new_from_values(ctx->mrb, len - pre - post, ary->ptr+pre);
      while (post--) {
        ctx->regs[a++] = ary->ptr[len-post-1];
      }
    }
    else {
      ctx->regs[a++] = mrb_ary_new_capa(ctx->mrb, 0);
      for (idx=0; idx+pre<len; idx++) {
        ctx->regs[a+idx] = ary->ptr[pre+idx];
      }
      while (idx < post) {
        SET_NIL_VALUE(ctx->regs[a+idx]);
        idx++;
      }
    }
  }
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

DEBUG(static char _str_const_op_string[] = "op_string\n");
static OP_INLINE void
op_string(struct op_ctx *ctx) {
  /* A Bx           R(A) := str_new(Lit(Bx)) */
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_str_dup(ctx->mrb, ctx->pool[GETARG_Bx(CTX_I(ctx))]);
  //VM_PRINTF(_str_const_op_string);
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static OP_INLINE void
op_strcat(struct op_ctx *ctx) {
  /* A B    R(A).concat(R(B)) */
  mrb_str_concat(ctx->mrb, ctx->regs[GETARG_A(CTX_I(ctx))], ctx->regs[GETARG_B(CTX_I(ctx))]);
}

static OP_INLINE void
op_hash(struct op_ctx *ctx) {
  /* A B C   R(A) := hash_new(R(B),R(B+1)..R(B+C)) */
  JIT_VOLATILE int b = GETARG_B(CTX_I(ctx));
  JIT_VOLATILE int c = GETARG_C(CTX_I(ctx));
  int lim = b+c*2;
  mrb_value hash = mrb_hash_new_capa(ctx->mrb, c);

  while (b < lim) {
    mrb_hash_set(ctx->mrb, hash, ctx->regs[b], ctx->regs[b+1]);
    b+=2;
  }
  ctx->regs[GETARG_A(CTX_I(ctx))] = hash;
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static JIT_NO_INLINE void
_op_lambda(struct op_ctx *ctx, int a, int b, int c) {
  struct RProc *p;

  if (ARG_PROTECT(c) & OP_L_CAPTURE) {
    p = mrb_closure_new(ctx->mrb, ctx->irep->reps[b]);
  }
  else {
    p = mrb_proc_new(ctx->mrb, ctx->irep->reps[b]);
    if (c & OP_L_METHOD) {
      if (p->target_class->tt == MRB_TT_SCLASS) {
        mrb_value klass;
        klass = mrb_obj_iv_get(ctx->mrb,
                                (struct RObject *)p->target_class,
                                intern_str_const(ctx->mrb, _str_const_attached));
        p->target_class = mrb_class_ptr(klass);
      }
    }
  }
  if (c & OP_L_STRICT) p->flags |= MRB_PROC_STRICT;
  ctx->regs[a] = mrb_obj_value(p);
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static OP_INLINE void
op_lambda(struct op_ctx *ctx) {
  /* A b c  R(A) := lambda(SEQ[b],c) (b:c = 14:2) */
  _op_lambda(ctx, GETARG_A(CTX_I(ctx)), GETARG_b(CTX_I(ctx)), GETARG_c(CTX_I(ctx)));
}

static OP_INLINE void
op_oclass(struct op_ctx *ctx) {
  /* A      R(A) := ::Object */
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_obj_value(ctx->mrb->object_class);
}

static char _str_const_op_class[] = "op_class %p\n";
static OP_INLINE void
op_class(struct op_ctx *ctx) {
  /* A B    R(A) := newclass(R(A),Syms(B),R(A+1)) */
  //printf(_str_const_op_class, ctx);

  struct RClass *c = 0;
  int a = GETARG_A(CTX_I(ctx));
  mrb_value base, super;
  mrb_sym id = ctx->syms[GETARG_B(CTX_I(ctx))];

  base = ctx->regs[a];
  super = ctx->regs[a+1];
  if (mrb_nil_p(base)) {
    base = mrb_obj_value(ctx->mrb->c->ci->target_class);
  }
  c = mrb_vm_define_class(ctx->mrb, base, super, id);
  ctx->regs[a] = mrb_obj_value(c);
  ARENA_RESTORE(ctx->mrb, ctx->ai);
  //printf(_str_const_op_class, ctx);
}

static OP_INLINE void
op_module(struct op_ctx *ctx) {
  /* A B            R(A) := newmodule(R(A),Syms(B)) */
  struct RClass *c = 0;
  int a = GETARG_A(CTX_I(ctx));
  mrb_value base;
  mrb_sym id = ctx->syms[GETARG_B(CTX_I(ctx))];

  base = ctx->regs[a];
  if (mrb_nil_p(base)) {
    base = mrb_obj_value(ctx->mrb->c->ci->target_class);
  }
  c = mrb_vm_define_module(ctx->mrb, base, id);
  ctx->regs[a] = mrb_obj_value(c);
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static char _str_const_op_exec[] = "op_exec %d %d\n";
static OP_INLINE void
op_exec(struct op_ctx *ctx) {
  /* A Bx   R(A) := blockexec(R(A),SEQ[Bx]) */
  int a = GETARG_A(CTX_I(ctx));
  mrb_callinfo *ci;
  mrb_value recv = ctx->regs[a];
  struct RProc *p;

  ////VM_PRINTF(_str_const_op_exec, GETARG_A(CTX_I(ctx)), GETARG_Bx(CTX_I(ctx)));

  /* prepare stack */
  ci = cipush(ctx->mrb);
  ci->pc = ctx->pc + 1;
  ci->acc = a;
  ci->mid = 0;
  ci->stackent = ctx->mrb->c->stack;
  ci->argc = 0;
  ci->target_class = mrb_class_ptr(recv);

  /* prepare stack */
  ctx->mrb->c->stack += a;

  p = mrb_proc_new(ctx->mrb, ctx->irep->reps[GETARG_Bx(CTX_I(ctx))]);
  p->target_class = ci->target_class;
  ci->proc = p;

  if (MRB_PROC_CFUNC_P(p)) {
    ci->nregs = 0;
    ctx->mrb->c->stack[0] = p->body.func(ctx->mrb, recv);
    mrb_gc_arena_restore(ctx->mrb, ctx->ai);
    if (ctx->mrb->exc) return _op_raise(ctx);
    /* pop stackpos */
    ctx->regs = ctx->mrb->c->stack = ctx->mrb->c->ci->stackent;
    cipop(ctx->mrb);
    PC_INC(ctx->pc);
  }
  else {
    ctx->irep = p->body.irep;
    ctx->pool = ctx->irep->pool;
    ctx->syms = ctx->irep->syms;
    stack_extend(ctx->mrb, ctx->irep->nregs, 1);
    ci->nregs = ctx->irep->nregs;
    ctx->regs = ctx->mrb->c->stack;
    ctx->pc = ctx->irep->iseq;

#ifdef MRB_JIT_GEN
    mrb_proc_call_jit(ctx->mrb, p, ctx);
#endif

  }
}

static const char _str_const_op_method[] = "op_method: %s = %p\n";

static OP_INLINE void
op_method(struct op_ctx *ctx) {
  /* A B            R(A).newmethod(Syms(B),R(A+1)) */
  int a = GETARG_A(CTX_I(ctx));
  struct RClass *c = mrb_class_ptr(ctx->regs[a]);

  mrb_define_method_vm(ctx->mrb, c, ctx->syms[GETARG_B(CTX_I(ctx))], ctx->regs[a+1]);

  ////VM_PRINTF(_str_const_op_method, mrb_sym2name(ctx->mrb, ctx->syms[GETARG_B(CTX_I(ctx))]),ctx->regs[a+1]);

  //mrb_codedump_all(ctx->mrb, mrb_proc_ptr(ctx->regs[a+1]));

  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static OP_INLINE void
op_sclass(struct op_ctx *ctx) {
  /* A B    R(A) := R(B).singleton_class */
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_singleton_class(ctx->mrb, ctx->regs[GETARG_B(CTX_I(ctx))]);
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static OP_INLINE void
op_tclass(struct op_ctx *ctx) {
  /* A      R(A) := target_class */
  mrb_state *mrb = ctx->mrb;
  if (!mrb->c->ci->target_class) {
    mrb_value exc = exc_new_str_const(mrb, E_TYPE_ERROR, _str_const_no_target_class);
    ctx->mrb->exc = mrb_obj_ptr(exc);
    return _op_raise(ctx);
  }
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_obj_value(ctx->mrb->c->ci->target_class);
  PC_INC(ctx->pc);
}

static OP_INLINE void
op_range(struct op_ctx *ctx) {
  /* A B C  R(A) := range_new(R(B),R(B+1),C) */
  int b = GETARG_B(CTX_I(ctx));
  ctx->regs[GETARG_A(CTX_I(ctx))] = mrb_range_new(ctx->mrb, ctx->regs[b], ctx->regs[b+1], !!GETARG_C(CTX_I(ctx)));
  ARENA_RESTORE(ctx->mrb, ctx->ai);
}

static OP_INLINE void
op_debug(struct op_ctx *ctx) {
  /* A B C    debug print R(A),R(B),R(C) */
#ifdef ENABLE_DEBUG
  ctx->mrb->debug_op_hook(ctx->mrb, ctx->irep, ctx->pc, ctx->regs);
#else
#ifdef ENABLE_STDIO
  printf(_str_const_op_debug_format, GETARG_A(CTX_I(ctx)), GETARG_B(CTX_I(ctx)), GETARG_C(CTX_I(ctx)));
#else
  abort();
#endif
#endif
}

static char _str_const_op_stop[] = "op_stop\n";
static OP_INLINE void
op_stop(struct op_ctx *ctx) {
  //VM_PRINTF(_str_const_op_stop);
  return _op_stop(ctx);
}

static OP_INLINE void
op_err(struct op_ctx *ctx) {
  /* Bx     raise RuntimeError with message Lit(Bx) */

  mrb_state *mrb = ctx->mrb;
  mrb_value msg = mrb_str_dup(mrb, ctx->pool[GETARG_Bx(CTX_I(ctx))]);
  mrb_value exc;

  if (GETARG_A(CTX_I(ctx)) == 0) {
    exc = mrb_exc_new_str(mrb, E_RUNTIME_ERROR, msg);
  }
  else {
    exc = mrb_exc_new_str(mrb, E_LOCALJUMP_ERROR, msg);
  }
  ctx->mrb->exc = mrb_obj_ptr(exc);
  return _op_raise(ctx);
}

#if defined __GNUC__
#pragma GCC diagnostic pop
#endif

#ifdef MRB_JIT_GEN
static void init_linker(){};
static void *link_funcs[1];
#elif MRB_ENABLE_JIT
#if defined(MRB_NAN_BOXING)
#include "jit/linker_nan_boxing.h"
#elif defined(MRB_WORD_BOXING)
#include "jit/linker_word_boxing.h"
#else
#include "jit/linker_no_boxing.h"
#endif
#endif


MRB_API mrb_value
mrb_context_run(mrb_state *mrb, struct RProc *proc, mrb_value self, unsigned int stack_keep)
{
  /* mrb_assert(mrb_proc_cfunc_p(proc)) */
  /*mrb_irep *irep = proc->body.irep;
  mrb_code *pc = irep->iseq;
  mrb_value *pool = irep->pool;
  mrb_sym *syms = irep->syms;
  mrb_value *regs = NULL;
  mrb_code i;
  int ai = mrb_gc_arena_save(mrb);
  struct mrb_jmpbuf *prev_jmp = mrb->jmp;*/

  struct mrb_jmpbuf c_jmp;
  struct mrb_jmpbuf stop_jmp;

  mrb_value retval;

  struct op_ctx ctx;

#ifdef DIRECT_THREADED
  static void *optable[] = {
    &&L_OP_NOP, &&L_OP_MOVE,
    &&L_OP_LOADL, &&L_OP_LOADI, &&L_OP_LOADSYM, &&L_OP_LOADNIL,
    &&L_OP_LOADSELF, &&L_OP_LOADT, &&L_OP_LOADF,
    &&L_OP_GETGLOBAL, &&L_OP_SETGLOBAL, &&L_OP_GETSPECIAL, &&L_OP_SETSPECIAL,
    &&L_OP_GETIV, &&L_OP_SETIV, &&L_OP_GETCV, &&L_OP_SETCV,
    &&L_OP_GETCONST, &&L_OP_SETCONST, &&L_OP_GETMCNST, &&L_OP_SETMCNST,
    &&L_OP_GETUPVAR, &&L_OP_SETUPVAR,
    &&L_OP_JMP, &&L_OP_JMPIF, &&L_OP_JMPNOT,
    &&L_OP_ONERR, &&L_OP_RESCUE, &&L_OP_POPERR, &&L_OP_RAISE, &&L_OP_EPUSH, &&L_OP_EPOP,
    &&L_OP_SEND, &&L_OP_SENDB, &&L_OP_FSEND,
    &&L_OP_CALL, &&L_OP_SUPER, &&L_OP_ARGARY, &&L_OP_ENTER, &&L_OP_ENTER_METHOD_M,
    &&L_OP_KARG, &&L_OP_KDICT, &&L_OP_RETURN, &&L_OP_BREAK,
    &&L_OP_TAILCALL, &&L_OP_BLKPUSH,
    &&L_OP_ADD, &&L_OP_ADDI, &&L_OP_SUB, &&L_OP_SUBI, &&L_OP_MUL, &&L_OP_DIV,
    &&L_OP_EQ, &&L_OP_LT, &&L_OP_LE, &&L_OP_GT, &&L_OP_GE,
    &&L_OP_ARRAY, &&L_OP_ARYCAT, &&L_OP_ARYPUSH, &&L_OP_AREF, &&L_OP_ASET, &&L_OP_APOST,
    &&L_OP_STRING, &&L_OP_STRCAT, &&L_OP_HASH,
    &&L_OP_LAMBDA, &&L_OP_RANGE, &&L_OP_OCLASS,
    &&L_OP_CLASS, &&L_OP_MODULE, &&L_OP_EXEC,
    &&L_OP_METHOD, &&L_OP_SCLASS, &&L_OP_TCLASS,
    &&L_OP_DEBUG, &&L_OP_STOP, &&L_OP_ERR,
  };
#endif

  ctx.proc = proc;
  ctx.irep = proc->body.irep;
  ctx.pc   = ctx.irep->iseq;
  ctx.regs = NULL;
  ctx.pool = proc->body.irep->pool;
  ctx.syms = proc->body.irep->syms;
  ctx.ai = mrb_gc_arena_save(mrb);
  ctx.prev_jmp = mrb->jmp;
  ctx.stop_jmp = &stop_jmp;
  ctx.mrb = mrb;

#ifdef MRB_ENABLE_JIT
  ctx.rescue_jmp_addr = NULL;
#endif

  //VM_PRINTF("mrb_context_run %d\n", jit);
  //mrb_codedump_all(mrb, proc);

#ifdef MRB_ENABLE_JIT
  init_linker();

  //init_symtbl();
  //memcpy(ctx.sym_tbl, symtbl, sizeof(symtbl));
#endif

  MRB_TRY(&stop_jmp) {

  mrb_bool exc_catched = FALSE;
RETRY_TRY_BLOCK:

  MRB_TRY(&c_jmp) {

  if (exc_catched) {
    exc_catched = FALSE;
    _op_raise(&ctx);

    //VM_PRINTF("back from longjmp raise: next %d\n", GET_OPCODE(*ctx.pc));

#ifdef MRB_ENABLE_JIT
    typedef void (*op_t)(struct op_ctx *ctx);
    uint8_t *addr;
    uint32_t idx;
    
    {
      idx = ctx.pc - ctx.irep->iseq;
      //VM_PRINTF("(rescue) jumping to %dth op\n", idx, addr);
      addr = ctx.irep->jit_ctx.text + ctx.irep->jit_ctx.text_off_tbl[idx];
      //VM_PRINTF("(rescue) jumping to %dth op at %p\n", idx, addr);
      ((op_t)(addr))(&ctx);
    }
    {
      idx = ctx.pc - ctx.irep->iseq;
      addr = ctx.irep->jit_ctx.text + ctx.irep->jit_ctx.text_off_tbl[idx];
      //VM_PRINTF("(send) jumping to %dth op at %p\n", idx, addr);
      ((op_t)(addr))(&ctx);
    }
#else
    {
      int op = ctx.pc - ctx.irep->iseq;
      //VM_PRINTF("bcalling rescue handler, jumping to %dth op\n", op);
      JUMP;
    }
#endif
  }
  mrb->jmp = &c_jmp;
  if (!mrb->c->stack) {
    stack_init(mrb);
  }
  stack_extend(mrb, ctx.irep->nregs, stack_keep);
  mrb->c->ci->proc = proc;
  mrb->c->ci->nregs = ctx.irep->nregs;
  ctx.regs = mrb->c->stack;
  ctx.regs[0] = self;

#ifdef MRB_ENABLE_JIT
  goto jit;
#endif

dispatch:

  INIT_DISPATCH {
    CASE(OP_NOP) {
      op_nop(&ctx);
      NEXT;
    }

    CASE(OP_MOVE) {
      op_move(&ctx);
      NEXT;
    }

    CASE(OP_LOADL) {
      op_loadl(&ctx);
      NEXT;
    }

    CASE(OP_LOADI) {
      op_loadi(&ctx);
      NEXT;
    }

    CASE(OP_LOADSYM) {
      op_loadsym(&ctx);
      NEXT;
    }

    CASE(OP_LOADSELF) {
      op_loadself(&ctx);
      NEXT;
    }

    CASE(OP_LOADT) {
      op_loadt(&ctx);
      NEXT;
    }

    CASE(OP_LOADF) {
      op_loadf(&ctx);
      NEXT;
    }

    CASE(OP_GETGLOBAL) {
      op_getglobal(&ctx);
      NEXT;
    }

    CASE(OP_SETGLOBAL) {
      op_setglobal(&ctx);
      NEXT;
    }

    CASE(OP_GETSPECIAL) {
      op_getspecial(&ctx);
      NEXT;
    }

    CASE(OP_SETSPECIAL) {
      op_setspecial(&ctx);
      NEXT;
    }

    CASE(OP_GETIV) {
      op_getiv(&ctx);
      NEXT;
    }

    CASE(OP_SETIV) {
      op_setiv(&ctx);
      NEXT;
    }

    CASE(OP_GETCV) {
      op_getcv(&ctx);
      NEXT;
    }

    CASE(OP_SETCV) {
      op_setcv(&ctx);
      NEXT;
    }

    CASE(OP_GETCONST) {
      op_getconst(&ctx);
      NEXT;
    }

    CASE(OP_SETCONST) {
      op_setconst(&ctx);
      NEXT;
    }

    CASE(OP_GETMCNST) {
      op_getmcnst(&ctx);
      NEXT;
    }

    CASE(OP_SETMCNST) {
      op_setmcnst(&ctx);
      NEXT;
    }

    CASE(OP_GETUPVAR) {
      op_getupvar(&ctx);
      NEXT;
    }

    CASE(OP_SETUPVAR) {
      op_setupvar(&ctx);
      NEXT;
    }

    CASE(OP_JMP) {
      op_jmp(&ctx);
      JUMP;
    }

    CASE(OP_JMPIF) {
      op_jmpif(&ctx);
      JUMP;
    }

    CASE(OP_JMPNOT) {
      op_jmpnot(&ctx);
      JUMP;
    }

    CASE(OP_ONERR) {
      op_onerr(&ctx);
      NEXT;
    }

    CASE(OP_RESCUE) {
      op_rescue(&ctx);
      NEXT;
    }

    CASE(OP_POPERR) {
      op_poperr(&ctx);
      NEXT;
    }

    CASE(OP_RAISE) {
      op_raise(&ctx);
      JUMP;
    }

    CASE(OP_EPUSH) {
      op_epush(&ctx);
      NEXT;
    }

    CASE(OP_EPOP) {
      op_epop(&ctx);
      NEXT;
    }

    CASE(OP_LOADNIL) {
      op_loadnil(&ctx);
      NEXT;
    }

    CASE(OP_SENDB) {
      op_sendb(&ctx);
      JUMP;
    };

    CASE(OP_SEND) {
      op_send(&ctx);
      JUMP;
    }

    CASE(OP_FSEND) {
      op_fsend(&ctx);
      NEXT;
    }

    CASE(OP_CALL) {
      op_call(&ctx);
      JUMP;
    }

    CASE(OP_SUPER) {
      op_super(&ctx);
      JUMP;
    }

    CASE(OP_ARGARY) {
      op_argary(&ctx);
      JUMP;
    }

    CASE(OP_ENTER) {
      op_enter(&ctx);
      JUMP;
    }

    CASE(OP_ENTER_METHOD_M) {
      op_enter_method_m(&ctx);
      JUMP;
    }

    CASE(OP_KARG) {
      op_karg(&ctx);
      NEXT;
    }

    CASE(OP_KDICT) {
      op_kdict(&ctx);
      NEXT;
    }

    CASE(OP_RETURN) {
      op_return(&ctx);
      JUMP;
    }

    CASE(OP_BREAK) {
      op_break(&ctx);
      JUMP;
    }

    CASE(OP_TAILCALL) {
      op_tailcall(&ctx);
      JUMP;
    }

    CASE(OP_BLKPUSH) {
      op_blkpush(&ctx);
      JUMP;
    }

    CASE(OP_ADD) {
      op_add(&ctx);
      JUMP;
    }

    CASE(OP_SUB) {
      op_sub(&ctx);
      JUMP;
    }

    CASE(OP_MUL) {
      op_mul(&ctx);
      JUMP;
    }

    CASE(OP_DIV) {
      op_div(&ctx);
      JUMP;
    }

    CASE(OP_ADDI) {
      op_addi(&ctx);
      JUMP;
    }

    CASE(OP_SUBI) {
      op_subi(&ctx);
      JUMP;
    }

    CASE(OP_EQ) {
      op_eq(&ctx);
      JUMP;
    }

    CASE(OP_LT) {
      op_lt(&ctx);
      JUMP;
    }

    CASE(OP_LE) {
      op_le(&ctx);
      JUMP;
    }

    CASE(OP_GT) {
      op_gt(&ctx);
      JUMP;
    }

    CASE(OP_GE) {
      op_ge(&ctx);
      JUMP;
    }

    CASE(OP_ARRAY) {
      op_array(&ctx);
      NEXT;
    }

    CASE(OP_ARYCAT) {
      op_arycat(&ctx);
      NEXT;
    }

    CASE(OP_ARYPUSH) {
      op_arypush(&ctx);
      NEXT;
    }

    CASE(OP_AREF) {
      op_aref(&ctx);
      NEXT;
    }

    CASE(OP_ASET) {
      op_aset(&ctx);
      NEXT;
    }

    CASE(OP_APOST) {
      op_apost(&ctx);
      NEXT;
    }

    CASE(OP_STRING) {
      op_string(&ctx);
      NEXT;
    }

    CASE(OP_STRCAT) {
      op_strcat(&ctx);
      NEXT;
    }

    CASE(OP_HASH) {
      op_hash(&ctx);
      NEXT;
    }

    CASE(OP_LAMBDA) {
      op_lambda(&ctx);
      NEXT;
    }

    CASE(OP_OCLASS) {
      op_oclass(&ctx);
      NEXT;
    }

    CASE(OP_CLASS) {
      op_class(&ctx);
      NEXT;
    }

    CASE(OP_MODULE) {
      op_module(&ctx);
      NEXT;
    }

    CASE(OP_EXEC) {
      op_exec(&ctx);
      JUMP;
    }

    CASE(OP_METHOD) {
      op_method(&ctx);
      NEXT;
    }

    CASE(OP_SCLASS) {
      op_sclass(&ctx);
      NEXT;
    }

    CASE(OP_TCLASS) {
      op_tclass(&ctx);
      JUMP;
    }

    CASE(OP_RANGE) {
      op_range(&ctx);
      NEXT;
    }

    CASE(OP_DEBUG) {
      op_debug(&ctx);
      NEXT;
    }

    CASE(OP_STOP) {
      op_stop(&ctx);
      /* jumps to MRB_CATCH(&stop_jmp) */
      JUMP;
    }

    CASE(OP_ERR) {
      op_err(&ctx);
      JUMP;
    }
  }
  END_DISPATCH;

#ifdef MRB_ENABLE_JIT
jit:
  ctx.i = *ctx.pc;

  if(!mrb_proc_call_jit(mrb, proc, &ctx)) {
    goto dispatch;
  }
#endif

  }
  MRB_CATCH(&c_jmp) {
    exc_catched = TRUE;
    goto RETRY_TRY_BLOCK;
  }
  MRB_END_EXC(&c_jmp);

  }
  MRB_CATCH(&stop_jmp) {
    retval = ctx.retval;
  }
  MRB_END_EXC(&stop_jmp);

  return retval;
}

MRB_API mrb_value
mrb_run(mrb_state *mrb, struct RProc *proc, mrb_value self)
{
  return mrb_context_run(mrb, proc, self, mrb->c->ci->argc + 2); /* argc + 2 (receiver and block) */
}

MRB_API mrb_value
mrb_toplevel_run_keep(mrb_state *mrb, struct RProc *proc, unsigned int stack_keep)
{
  mrb_callinfo *ci;
  mrb_value v;

  if (!mrb->c->cibase || mrb->c->ci == mrb->c->cibase) {
    return mrb_context_run(mrb, proc, mrb_top_self(mrb), stack_keep);
  }
  ci = cipush(mrb);
  ci->nregs = 1;   /* protect the receiver */
  ci->acc = CI_ACC_SKIP;
  ci->target_class = mrb->object_class;
  v = mrb_context_run(mrb, proc, mrb_top_self(mrb), stack_keep);
  cipop(mrb);

  return v;
}

MRB_API mrb_value
mrb_toplevel_run(mrb_state *mrb, struct RProc *proc)
{
  return mrb_toplevel_run_keep(mrb, proc, 0);
}
