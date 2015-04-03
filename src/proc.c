/*
** proc.c - Proc class
**
** See Copyright Notice in mruby.h
*/

#define _DEFAULT_SOURCE
#include "mruby.h"
#include "mruby/class.h"
#include "mruby/proc.h"
#include "mruby/opcode.h"

static mrb_code call_iseq[] = {
  MKOP_A(OP_CALL, 0),
};

struct RProc *
mrb_proc_new(mrb_state *mrb, mrb_irep *irep)
{
  struct RProc *p;
  mrb_callinfo *ci = mrb->c->ci;

  p = (struct RProc*)mrb_obj_alloc(mrb, MRB_TT_PROC, mrb->proc_class);
  p->target_class = 0;
  if (ci) {
    if (ci->proc)
      p->target_class = ci->proc->target_class;
    if (!p->target_class)
      p->target_class = ci->target_class;
  }
  p->body.irep = irep;
  p->env = 0;
  mrb_irep_incref(mrb, irep);

  return p;
}

static struct REnv*
env_new(mrb_state *mrb, int nlocals)
{
  struct REnv *e;

  e = (struct REnv*)mrb_obj_alloc(mrb, MRB_TT_ENV, (struct RClass*)mrb->c->ci->proc->env);
  MRB_SET_ENV_STACK_LEN(e, nlocals);
  e->mid = mrb->c->ci->mid;
  e->cioff = mrb->c->ci - mrb->c->cibase;
  e->stack = mrb->c->stack;

  return e;
}

static void
closure_setup(mrb_state *mrb, struct RProc *p, int nlocals)
{
  struct REnv *e;

  if (!mrb->c->ci->env) {
    e = env_new(mrb, nlocals);
    mrb->c->ci->env = e;
  }
  else {
    e = mrb->c->ci->env;
  }
  p->env = e;
}

struct RProc *
mrb_closure_new(mrb_state *mrb, mrb_irep *irep)
{
  struct RProc *p = mrb_proc_new(mrb, irep);

  closure_setup(mrb, p, mrb->c->ci->proc->body.irep->nlocals);
  return p;
}

MRB_API struct RProc *
mrb_proc_new_cfunc(mrb_state *mrb, mrb_func_t func)
{
  struct RProc *p;

  p = (struct RProc*)mrb_obj_alloc(mrb, MRB_TT_PROC, mrb->proc_class);
  p->body.func = func;
  p->flags |= MRB_PROC_CFUNC;
  p->env = 0;

  return p;
}

MRB_API struct RProc *
mrb_proc_new_cfunc_with_env(mrb_state *mrb, mrb_func_t func, mrb_int argc, const mrb_value *argv)
{
  struct RProc *p = mrb_proc_new_cfunc(mrb, func);
  struct REnv *e;
  int i;

  p->env = e = env_new(mrb, argc);
  MRB_ENV_UNSHARE_STACK(e);
  e->stack = (mrb_value*)mrb_malloc(mrb, sizeof(mrb_value) * argc);
  if (argv) {
    for (i = 0; i < argc; ++i) {
      e->stack[i] = argv[i];
    }
  }
  else {
    for (i = 0; i < argc; ++i) {
      SET_NIL_VALUE(e->stack[i]);
    }
  }
  return p;
}

MRB_API struct RProc *
mrb_closure_new_cfunc(mrb_state *mrb, mrb_func_t func, int nlocals)
{
  return mrb_proc_new_cfunc_with_env(mrb, func, nlocals, NULL);
}

MRB_API mrb_value
mrb_proc_cfunc_env_get(mrb_state *mrb, mrb_int idx)
{
  struct RProc *p = mrb->c->ci->proc;
  struct REnv *e = p->env;

  if (!MRB_PROC_CFUNC_P(p)) {
    mrb_raise(mrb, E_TYPE_ERROR, "Can't get cfunc env from non-cfunc proc.");
  }
  if (!e) {
    mrb_raise(mrb, E_TYPE_ERROR, "Can't get cfunc env from cfunc Proc without REnv.");
  }
  if (idx < 0 || MRB_ENV_STACK_LEN(e) <= idx) {
    mrb_raisef(mrb, E_INDEX_ERROR, "Env index out of range: %S (expected: 0 <= index < %S)",
               mrb_fixnum_value(idx), mrb_fixnum_value(MRB_ENV_STACK_LEN(e)));
  }

  return e->stack[idx];
}

MRB_API void
mrb_proc_copy(struct RProc *a, struct RProc *b)
{
  a->flags = b->flags;
  a->body = b->body;
  if (!MRB_PROC_CFUNC_P(a)) {
    a->body.irep->refcnt++;
  }
  a->target_class = b->target_class;
  a->env = b->env;
}


#include "ops_x64.h"


#if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
#include <sys/mman.h>
#include <unistd.h>

static size_t
jit_page_size() {
  return sysconf(_SC_PAGESIZE);
}

static struct mrb_jit_page *
jit_page_alloc(size_t size) {
  size_t page_size = jit_page_size();
  size = (size + page_size - 1) & ~(page_size - 1);

  fprintf(stderr, "allocating page of size %d\n", size);

  struct mrb_jit_page *page = (struct mrb_jit_page *) mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  page->size = size;

  return page;
}

static void
jit_page_prot_exec(struct mrb_jit_page *page) {
  mprotect(page, page->size, PROT_READ | PROT_EXEC);
}
#endif

void
mrb_proc_jit_call(struct RProc *proc, void *ctx) {
  volatile uint64_t padd[4] = {0};
  void (*f)(void *) = (void *)proc->jit_page->data;
  return (*f)(ctx);
}



static int jmp_offset(mrb_code *iseq, int i);
static size_t op_size(mrb_code *iseq, int i) {
  mrb_code c = iseq[i];
  size_t size = op_sizes[GET_OPCODE(c)];
  uint8_t buf[16];
  uint8_t *jmp_off;
  int opcode = GET_OPCODE(c);

  switch(opcode) {
    case OP_JMPNOT:
      size += jit_jump_if(buf, jmp_offset(iseq, i)) - buf;
      break;
    case OP_JMPIF:
      size += jit_jump_not(buf, jmp_offset(iseq, i)) - buf;
      break;
    case OP_JMP:
      size += jit_jump(buf, jmp_offset(iseq, i)) - buf;
      break;
  }

  return size;
}

static int jmp_offset(mrb_code *iseq, int i) {
  mrb_code c = iseq[i];
  int jit_off = 0;
  int b, e, m, j;
  int op_off = GETARG_sBx(c);

  fprintf(stderr, "jumpnot: %d\n", op_off);

  if (op_off > 0) {
    m = 1;
    b = i + 1;
    e = i + op_off;
  } else if(op_off < 0) {
    m = -1;
    b = i - op_off + 1;
    e = i;
  } else {
  }

  for(j = b; j < e; j++) {
    jit_off += op_size(iseq, j);
    fprintf(stderr, "jumpnot: to %s %d -> %p\n", op_names[GET_OPCODE(iseq[j])], jit_off);
  }

  jit_off *= m;
}

mrb_bool
mrb_proc_jit(struct RProc *proc)
{
  if (MRB_PROC_CFUNC_P(proc)) {
    return FALSE;
  } else {
    size_t size = 0;
    unsigned i  = 0;
    mrb_irep *irep = proc->body.irep;

    init_ops();

    for (i = 0; i < irep->ilen; i++) {
      size += op_size(irep->iseq, i);
    }
    fprintf(stderr, "need %d bytes for jit code\n", size);
    struct mrb_jit_page *page = jit_page_alloc(size);
    size_t off = 0;
    for (i = 0; i < irep->ilen; i++) {
      mrb_code c = irep->iseq[i];
      int opcode = GET_OPCODE(c);
//      if(opcode == OP_ENTER || opcode == OP_LOADNIL) continue;
//      if(opcode == OP_SEND || GET_OPCODE(irep->iseq[i]) == OP_ENTER) continue;
      size_t size = op_size(irep->iseq, i);
      fprintf(stderr, "copying opcode:%s (%d) to offset %d (%d bytes) (addr: %p)\n", op_names[opcode], opcode, off, size, page->data + off);
      memcpy(page->data + off, ops[opcode], size);

      arg_funcs[opcode](page->data + off, c);

      off += size;

      if (opcode == OP_JMPNOT || opcode == OP_JMPIF || opcode == OP_JMP) {
        int op_off = GETARG_sBx(c);
        uint8_t *jmp_off;
        int jit_off = jmp_offset(irep->iseq, i);

        if (opcode == OP_JMPNOT) {
          jmp_off = jit_jump_if(page->data + off, jit_off);
        } else if(opcode == OP_JMPIF){
          jmp_off = jit_jump_not(page->data + off, jit_off);
        } else {
          jmp_off = jit_jump(page->data + off, jit_off);
        }
        fprintf(stderr, "jumpnot: jit off %d %p\n", jit_off, jmp_off);

        off += jmp_off - (page->data + off);
      }
    }


for (i = 0; i < off; i++)
{
  if (i > 0) printf(" ");
  printf("%02X", page->data[i]);
}
printf("\n");

    proc->flags |= MRB_PROC_JITTED;
    proc->jit_page = page;
    jit_page_prot_exec(page);
    return TRUE;
  }
}

void
mrb_proc_jit_prepare(struct RProc *proc) {

  if (MRB_PROC_CFUNC_P(proc)) {
    return;
  }

  int i, j;
  uint16_t base = 0;
  uint16_t off = base + op_sizes[OP_ENTER];
  mrb_irep *irep = proc->body.irep;

  proc->jit_oa_off[0] = off;
  for(i = 1; i < irep->oalen; i++) {
    uint16_t off = 0;
    int j;
    for(j = 1; j < irep->oa_off[i]; j++) {
      mrb_code c = irep->iseq[j];
      off += op_sizes[GET_OPCODE(c)];
    }
    proc->jit_oa_off[i] = off;
  }

  for(i = 0; i < irep->oalen; i++) {
    fprintf(stderr, "enter off: %d -> %d (%d)\n", i, proc->jit_oa_off[i], proc->jit_oa_off[i] - base);
  }
}

static mrb_value
mrb_proc_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;

  mrb_get_args(mrb, "&", &blk);
  if (mrb_nil_p(blk)) {
    /* Calling Proc.new without a block is not implemented yet */
    mrb_raise(mrb, E_ARGUMENT_ERROR, "tried to create Proc object without a block");
  }
  else {
    mrb_proc_copy(mrb_proc_ptr(self), mrb_proc_ptr(blk));
  }
  return self;
}

static mrb_value
mrb_proc_init_copy(mrb_state *mrb, mrb_value self)
{
  mrb_value proc;

  mrb_get_args(mrb, "o", &proc);
  if (mrb_type(proc) != MRB_TT_PROC) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "not a proc");
  }
  mrb_proc_copy(mrb_proc_ptr(self), mrb_proc_ptr(proc));
  return self;
}

int
mrb_proc_cfunc_p(struct RProc *p)
{
  return MRB_PROC_CFUNC_P(p);
}

mrb_value
mrb_proc_call_cfunc(mrb_state *mrb, struct RProc *p, mrb_value self)
{
  return (p->body.func)(mrb, self);
}

mrb_code*
mrb_proc_iseq(mrb_state *mrb, struct RProc *p)
{
  return p->body.irep->iseq;
}

/* 15.2.17.4.2 */
static mrb_value
mrb_proc_arity(mrb_state *mrb, mrb_value self)
{
  struct RProc *p = mrb_proc_ptr(self);
  mrb_code *iseq = mrb_proc_iseq(mrb, p);
  mrb_aspec aspec;
  int ma, ra, pa, arity;

  if (MRB_PROC_CFUNC_P(p)) {
    /* TODO cfunc aspec not implemented yet */
    return mrb_fixnum_value(-1);
  }

  /* arity is depend on OP_ENTER */
  if (GET_OPCODE(*iseq) != OP_ENTER) {
    return mrb_fixnum_value(0);
  }

  aspec = GETARG_Ax(*iseq);
  ma = MRB_ASPEC_REQ(aspec);
  ra = MRB_ASPEC_REST(aspec);
  pa = MRB_ASPEC_POST(aspec);
  arity = ra ? -(ma + pa + 1) : ma + pa;

  return mrb_fixnum_value(arity);
}

/* 15.3.1.2.6  */
/* 15.3.1.3.27 */
/*
 * call-seq:
 *   lambda { |...| block }  -> a_proc
 *
 * Equivalent to <code>Proc.new</code>, except the resulting Proc objects
 * check the number of parameters passed when called.
 */
static mrb_value
proc_lambda(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;
  struct RProc *p;

  mrb_get_args(mrb, "&", &blk);
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "tried to create Proc object without a block");
  }
  p = mrb_proc_ptr(blk);
  if (!MRB_PROC_STRICT_P(p)) {
    struct RProc *p2 = (struct RProc*)mrb_obj_alloc(mrb, MRB_TT_PROC, p->c);
    mrb_proc_copy(p2, p);
    p2->flags |= MRB_PROC_STRICT;
    return mrb_obj_value(p2);
  }
  return blk;
}

void
mrb_init_proc(mrb_state *mrb)
{
  struct RProc *m;
  mrb_irep *call_irep = (mrb_irep *)mrb_malloc(mrb, sizeof(mrb_irep));
  static const mrb_irep mrb_irep_zero = { 0 };

  *call_irep = mrb_irep_zero;
  call_irep->flags = MRB_ISEQ_NO_FREE;
  call_irep->iseq = call_iseq;
  call_irep->ilen = 1;

  mrb_define_method(mrb, mrb->proc_class, "initialize", mrb_proc_initialize, MRB_ARGS_NONE());
  mrb_define_method(mrb, mrb->proc_class, "initialize_copy", mrb_proc_init_copy, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, mrb->proc_class, "arity", mrb_proc_arity, MRB_ARGS_NONE());

  m = mrb_proc_new(mrb, call_irep);
  mrb_define_method_raw(mrb, mrb->proc_class, mrb_intern_lit(mrb, "call"), m);
  mrb_define_method_raw(mrb, mrb->proc_class, mrb_intern_lit(mrb, "[]"), m);

  mrb_define_class_method(mrb, mrb->kernel_module, "lambda", proc_lambda, MRB_ARGS_NONE()); /* 15.3.1.2.6  */
  mrb_define_method(mrb, mrb->kernel_module,       "lambda", proc_lambda, MRB_ARGS_NONE()); /* 15.3.1.3.27 */
}
