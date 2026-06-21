
#include <string.h>
#include "../include/mrc_irep.h"
#include "../include/mrc_ccontext.h"
#include "../include/mrc_parser_util.h"
#include "../include/mrc_throw.h"
#include "../include/mrc_opcode.h"
#include "../include/mrc_presym.h"
#include "../include/mrc_pool.h"
#include "../include/mrc_dump.h"
#include "../include/mrc_debug.h"
#include "../include/mrc_irep_pool_type.h"

#if defined(MRC_TARGET_MRUBY)
#include "../include/mrc_proc.h"
#endif

#ifdef MRBC_REQUIRE_32BIT_ALIGNMENT
#include <mrubyc.h>
#define printf(...) console_printf(__VA_ARGS__)
#else
#define printf(n) ((void)0)
#endif

#if defined(MRC_INT64)
# define MRC_INT_BIT 64
# define MRC_INT_MIN INT64_MIN
# define MRC_INT_MAX INT64_MAX
# define MRC_PRIo PRIo64
# define MRC_PRId PRId64
# define MRC_PRIx PRIx64
#else
  typedef int32_t mrc_int;
  typedef uint32_t mrc_uint;
# define MRC_INT_BIT 32
# define MRC_INT_MIN INT32_MIN
# define MRC_INT_MAX INT32_MAX
# define MRC_PRIo PRIo32
# define MRC_PRId PRId32
# define MRC_PRIx PRIx32
#endif

/**
 * Function requires n arguments.
 *
 * @param n
 *      The number of required arguments.
 */
#define MRC_ARGS_REQ(n)     ((mrc_aspec)((n)&0x1f) << 18)

/**
 * Function takes n optional arguments
 *
 * @param n
 *      The number of optional arguments.
 */
#define MRC_ARGS_OPT(n)     ((mrc_aspec)((n)&0x1f) << 13)

/**
 * Function takes n1 mandatory arguments and n2 optional arguments
 *
 * @param n1
 *      The number of required arguments.
 * @param n2
 *      The number of optional arguments.
 */
#define MRC_ARGS_ARG(n1,n2)   (MRC_ARGS_REQ(n1)|MRC_ARGS_OPT(n2))

/** rest argument */
#define MRC_ARGS_REST()     ((mrc_aspec)(1 << 12))

/** required arguments after rest */
#define MRC_ARGS_POST(n)    ((mrc_aspec)((n)&0x1f) << 7)

/** keyword arguments (n of keys, kdict) */
#define MRC_ARGS_KEY(n1,n2) ((mrc_aspec)((((n1)&0x1f) << 2) | ((n2)?(1<<1):0)))

/**
 * Function takes a block argument
 */
#define MRC_ARGS_BLOCK()    ((mrc_aspec)1)

/**
 * Function accepts any number of arguments
 */
#define MRC_ARGS_ANY()      MRC_ARGS_REST()

/**
 * Function accepts no arguments
 */
#define MRC_ARGS_NONE()     ((mrc_aspec)0)


#define MRC_INT_OVERFLOW_MASK ((mrc_uint)1 << (MRC_INT_BIT - 1))

static inline mrc_bool
mrc_int_add_overflow(mrc_int a, mrc_int b, mrc_int *c)
{
  mrc_uint x = (mrc_uint)a;
  mrc_uint y = (mrc_uint)b;
  mrc_uint z = (mrc_uint)(x + y);
  *c = (mrc_int)z;
  return !!(((x ^ z) & (y ^ z)) & MRC_INT_OVERFLOW_MASK);
}

static inline mrc_bool
mrc_int_sub_overflow(mrc_int a, mrc_int b, mrc_int *c)
{
  mrc_uint x = (mrc_uint)a;
  mrc_uint y = (mrc_uint)b;
  mrc_uint z = (mrc_uint)(x - y);
  *c = (mrc_int)z;
  return !!(((x ^ z) & (~y ^ z)) & MRC_INT_OVERFLOW_MASK);
}

static inline mrc_bool
mrc_int_mul_overflow(mrc_int a, mrc_int b, mrc_int *c)
{
#ifdef MRC_INT32
  int64_t n = (int64_t)a * b;
  *c = (mrc_int)n;
  return n > MRC_INT_MAX || n < MRC_INT_MIN;
#else /* MRC_INT64 */
  if (a > 0 && b > 0 && a > MRC_INT_MAX / b) return TRUE;
  if (a < 0 && b > 0 && a < MRC_INT_MIN / b) return TRUE;
  if (a > 0 && b < 0 && b < MRC_INT_MIN / a) return TRUE;
  if (a < 0 && b < 0 && (a <= MRC_INT_MIN || b <= MRC_INT_MIN || -a > MRC_INT_MAX / -b))
    return TRUE;
  *c = a * b;
  return FALSE;
#endif
}

static mrc_int
mrc_div_int(mrc_int x, mrc_int y)
{
  mrc_int div = x / y;

  if ((x ^ y) < 0 && x != div * y) {
    div -= 1;
  }
  return div;
}

#define NUMERIC_SHIFT_WIDTH_MAX (MRC_INT_BIT-1)

static mrc_bool
mrc_num_shift(mrc_int val, mrc_int width, mrc_int *num)
{
  if (width < 0) {              /* rshift */
    if (width == MRC_INT_MIN || -width >= NUMERIC_SHIFT_WIDTH_MAX) {
      if (val < 0) {
        *num = -1;
      }
      else {
        *num = 0;
      }
    }
    else {
      *num = val >> -width;
    }
  }
  else if (val > 0) {
    if ((width > NUMERIC_SHIFT_WIDTH_MAX) ||
        (val   > (MRC_INT_MAX >> width))) {
      return FALSE;
    }
    *num = val << width;
  }
  else {
    if ((width > NUMERIC_SHIFT_WIDTH_MAX) ||
        (val   < (MRC_INT_MIN >> width))) {
      return FALSE;
    }
    if (width == NUMERIC_SHIFT_WIDTH_MAX)
      *num = MRC_INT_MIN;
    else
      *num = val * ((mrc_int)1 << width);
  }
  return TRUE;
}

#ifdef MRC_ENDIAN_BIG
# define MRC_ENDIAN_LOHI(a,b) a b
#else
# define MRC_ENDIAN_LOHI(a,b) b a
#endif
#ifndef MRC_CODEGEN_LEVEL_MAX
#define MRC_CODEGEN_LEVEL_MAX 256
#endif

enum looptype {
  LOOP_NORMAL,
  LOOP_BLOCK,
  LOOP_FOR,
  LOOP_BEGIN,
  LOOP_RESCUE,
};

struct loopinfo {
  enum looptype type;
  uint32_t pc0;                 /* `next` destination */
  uint32_t pc1;                 /* `redo` destination */
  uint32_t pc2;                 /* `break` destination */
  int reg;                      /* destination register */
  struct loopinfo *prev;
};

typedef struct scope {
  mrc_pool *mpool; // -> *page

  struct scope *prev;

  mrc_constant_id_list *lv;

  uint16_t sp;
  uint32_t pc;
  uint32_t lastpc;
  uint32_t lastlabel;
  uint16_t ainfo:15;
  mrc_bool mscope:1;

  struct loopinfo *loop;
  //mrc_sym filename_sym;
  const char *filename;
  uint16_t lineno;

  mrc_code *iseq;
  uint16_t *lines;
  uint32_t icapa;

  mrc_irep *irep;
  mrc_pool_value *pool;
  mrc_sym *syms;
  mrc_irep **reps;
  struct mrc_irep_catch_handler *catch_table;
  uint32_t pcapa, scapa, rcapa;

  uint16_t nlocals;
  uint16_t nregs;
  int ai;

  int debug_start_pos;
  uint16_t filename_index;
  mrc_ccontext* c;

  int rlev;                     /* recursion levels */
  uint16_t for_depth;           /* number of for-loop scopes above */
} mrc_codegen_scope;

static void codegen(mrc_codegen_scope *s, mrc_node *tree, int val);

static void
codegen_error(mrc_codegen_scope *s, const char *message)
{
  if (!s) return;
  s->c->capture_errors = TRUE;

  mrc_diagnostic_list_append(s->c, 0, message, MRC_GENERATOR_ERROR);

#ifndef MRC_NO_STDIO
  if (s->filename && s->lineno) {
    const char *filename = (const char *)s->filename;
    fprintf(stderr, "%s:%d: %s\n", filename, s->lineno, message);
  }
  else {
    fprintf(stderr, "%s\n", message);
  }

#endif
  while (s->prev) {
    mrc_codegen_scope *tmp = s->prev;
    if (s->irep) {
      mrc_free(s->c, s->iseq);
      for (int i=0; i<s->irep->plen; i++) {
        mrc_pool_value *pv = &s->pool[i];
        if ((pv->tt & 0x3) == IREP_TT_STR || pv->tt == IREP_TT_BIGINT) {
          mrc_free(s->c, (void*)pv->u.str);
        }
      }
      mrc_free(s->c, s->pool);
      mrc_free(s->c, s->syms);
      mrc_free(s->c, s->catch_table);
      if (s->reps) {
        /* copied from mrc_irep_free() in state.c */
        //for (int i=0; i<s->irep->rlen; i++) {
        //  if (s->reps[i])
        //    mrc_irep_decref(s->mrb, (mrc_irep*)s->reps[i]);
        //}
        mrc_free(s->c, s->reps);
      }
      mrc_free(s->c, s->lines);
    }
    mrc_pool_close(s->mpool);
    s = tmp;
  }
  MRC_THROW(s->c->jmp);
}

static void*
codegen_palloc(mrc_codegen_scope *s, size_t len)
{
  void *p = mrc_pool_alloc(s->mpool, len);

  if (!p) codegen_error(s, "pool memory allocation");
  return p;
}

static void*
codegen_realloc(mrc_codegen_scope *s, void *p, size_t oldlen, size_t newlen)
{
  p = mrc_pool_realloc(s->mpool, p, oldlen, newlen);

  if (!p && 0 < newlen) codegen_error(s, "pool memory reallocation");
  return p;
}

static void
check_no_ext_ops(mrc_codegen_scope *s, uint16_t a, uint16_t b)
{
  if (s->c->no_ext_ops && (a | b) > 0xff) {
    codegen_error(s, "need OP_EXTs instruction (currently OP_EXTs are prohibited)");
  }
}

static int
new_label(mrc_codegen_scope *s)
{
  return s->lastlabel = s->pc;
}

static void
emit_B(mrc_codegen_scope *s, uint32_t pc, uint8_t i)
{
  if (pc >= s->icapa) {
    if (pc == UINT32_MAX) {
      codegen_error(s, "too big code block");
    }
    if (pc >= UINT32_MAX / 2) {
      pc = UINT32_MAX;
    }
    else {
      s->icapa *= 2;
    }
    s->iseq = (mrc_code*)mrc_realloc(s->c, s->iseq, sizeof(mrc_code)*s->icapa);
    if (s->lines) {
      s->lines = (uint16_t*)mrc_realloc(s->c, s->lines, sizeof(uint16_t)*s->icapa);
    }
  }
  if (s->lines) {
    if (s->lineno > 0 || pc == 0)
      s->lines[pc] = s->lineno;
    else
      s->lines[pc] = s->lines[pc-1];
  }
  s->iseq[pc] = i;
}

static void
emit_S(mrc_codegen_scope *s, int pc, uint16_t i)
{
  uint8_t hi = i>>8;
  uint8_t lo = i&0xff;

  emit_B(s, pc,   hi);
  emit_B(s, pc+1, lo);
}

static void
gen_B(mrc_codegen_scope *s, uint8_t i)
{
  emit_B(s, s->pc, i);
  s->pc++;
}

static void
gen_S(mrc_codegen_scope *s, uint16_t i)
{
  emit_S(s, s->pc, i);
  s->pc += 2;
}

static void
genop_0(mrc_codegen_scope *s, mrc_code i)
{
  s->lastpc = s->pc;
  gen_B(s, i);
}

static void
genop_1(mrc_codegen_scope *s, mrc_code i, uint16_t a)
{
  s->lastpc = s->pc;
  check_no_ext_ops(s, a, 0);
  if (a > 0xff) {
    gen_B(s, OP_EXT1);
    gen_B(s, i);
    gen_S(s, a);
  }
  else {
    gen_B(s, i);
    gen_B(s, (uint8_t)a);
  }
}

static void
genop_2(mrc_codegen_scope *s, mrc_code i, uint16_t a, uint16_t b)
{
  s->lastpc = s->pc;
  check_no_ext_ops(s, a, b);
  if (a > 0xff && b > 0xff) {
    gen_B(s, OP_EXT3);
    gen_B(s, i);
    gen_S(s, a);
    gen_S(s, b);
  }
  else if (b > 0xff) {
    gen_B(s, OP_EXT2);
    gen_B(s, i);
    gen_B(s, (uint8_t)a);
    gen_S(s, b);
  }
  else if (a > 0xff) {
    gen_B(s, OP_EXT1);
    gen_B(s, i);
    gen_S(s, a);
    gen_B(s, (uint8_t)b);
  }
  else {
    gen_B(s, i);
    gen_B(s, (uint8_t)a);
    gen_B(s, (uint8_t)b);
  }
}

static void
genop_3(mrc_codegen_scope *s, mrc_code i, uint16_t a, uint16_t b, uint16_t c)
{
  genop_2(s, i, a, b);
  gen_B(s, (uint8_t)c);
}

static void
genop_2S(mrc_codegen_scope *s, mrc_code i, uint16_t a, uint16_t b)
{
  genop_1(s, i, a);
  gen_S(s, b);
}

static void
genop_2SS(mrc_codegen_scope *s, mrc_code i, uint16_t a, uint32_t b)
{
  genop_1(s, i, a);
  gen_S(s, b>>16);
  gen_S(s, b&0xffff);
}

static void
genop_W(mrc_codegen_scope *s, mrc_code i, uint32_t a)
{
  uint8_t a1 = (a>>16) & 0xff;
  uint8_t a2 = (a>>8) & 0xff;
  uint8_t a3 = a & 0xff;

  s->lastpc = s->pc;
  gen_B(s, i);
  gen_B(s, a1);
  gen_B(s, a2);
  gen_B(s, a3);
}

#define NOVAL  0
#define VAL    1

#define nregs_update do {if (s->sp > s->nregs) s->nregs = s->sp;} while (0)
static void
push_n_(mrc_codegen_scope *s, int n)
{
  if (s->sp+n >= 0xffff) {
    codegen_error(s, "too complex expression");
  }
  s->sp+=n;
  nregs_update;
}

static void
pop_n_(mrc_codegen_scope *s, int n)
{
  if ((int)s->sp-n < 0) {
    codegen_error(s, "stack pointer underflow");
  }
  s->sp-=n;
}

#define push() push_n_(s,1)
#define push_n(n) push_n_(s,n)
#define pop() pop_n_(s,1)
#define pop_n(n) pop_n_(s,n)
#define cursp() (s->sp)

static mrc_irep*
mrc_add_irep(mrc_ccontext *c)
{
  static const mrc_irep mrc_irep_zero = { 0 };
  mrc_irep *irep = (mrc_irep *)mrc_malloc(c, sizeof(mrc_irep));
  *irep = mrc_irep_zero;
  irep->refcnt = 1;
  return irep;
}

static void
scope_add_irep(mrc_codegen_scope *s)
{
  mrc_irep *irep;
  mrc_codegen_scope *prev = s->prev;
  if (prev->irep == NULL) {
    irep = mrc_add_irep(s->c);
    prev->irep = s->irep = irep;
    return;
  }
  else {
    if (prev->irep->rlen == UINT16_MAX) {
      codegen_error(s, "too many nested blocks/methods");
    }
    s->irep = irep = mrc_add_irep(s->c);
    if (prev->irep->rlen == prev->rcapa) {
      prev->rcapa *= 2;
      prev->reps = (mrc_irep **)mrc_realloc(s->c, prev->reps, sizeof(mrc_irep *)*prev->rcapa);
    }
    prev->reps[prev->irep->rlen++] = irep;
  }
}

static mrc_codegen_scope *
scope_new(mrc_ccontext *c, mrc_codegen_scope *prev, mrc_constant_id_list *nlv)
{
  static const mrc_codegen_scope codegen_scope_zero = { 0 };
  mrc_pool *pool = mrc_pool_open(c);
  mrc_codegen_scope *s = (mrc_codegen_scope *)mrc_pool_alloc(pool, sizeof(mrc_codegen_scope));
  if (!s) {
    if (prev)
      codegen_error(prev, "unexpected scope");
    return NULL;
  }
  *s = codegen_scope_zero;
  if (prev) {
    s->c = prev->c;
  } else {
    s->c = c;
  }
  s->mpool = pool;
  if (!prev) return s;
  s->prev = prev;
  s->ainfo = 0;
  s->mscope = 0;

  scope_add_irep(s);

  s->rcapa = 8;
  s->reps = (mrc_irep **)mrc_malloc(c, sizeof(mrc_irep *)*s->rcapa);
  s->icapa = 1024;
  s->iseq = (mrc_code *)mrc_malloc(c, sizeof(mrc_code)*s->icapa);
  s->pcapa = 32;
  s->pool = (mrc_pool_value *)mrc_malloc(c, sizeof(mrc_pool_value)*s->pcapa);
  s->scapa = 256;
  s->syms = (mrc_sym *)mrc_malloc(c, sizeof(mrc_sym)*s->scapa);
  if (nlv == NULL) {
    /* for-loop scope: empty lv so search_upvar skips this scope,
       but mirror parent's register layout */
    s->lv = NULL;
    s->sp = prev->nlocals;
    s->nlocals = s->nregs = s->sp;
    if (prev->irep->lv) {
      size_t lv_size = sizeof(mrc_sym) * (s->nlocals - 1);
      s->irep->lv = (mrc_sym *)mrc_malloc(c, lv_size);
      memcpy(s->irep->lv, prev->irep->lv, lv_size);
    }
    else {
      s->irep->lv = NULL;
    }
  }
  else {
    s->lv = nlv;
    s->sp += nlv->size + 1; /* add self */
    s->nlocals = s->nregs = s->sp;
    mrc_sym *lv;
    size_t size = sizeof(mrc_sym) * nlv->size;
    if (0 < size) {
      s->irep->lv = lv = (mrc_sym *)mrc_malloc(c, sizeof(mrc_sym) * (s->nlocals - 1));
      memcpy(lv, nlv->ids, size);
    }
    else {
      s->irep->lv = lv = NULL;
    }
    mrc_assert(nlv->size < UINT16_MAX);
  }

  int ai = mrc_gc_arena_save(c);
  s->ai = ai;
  s->filename = prev->filename;
  if (s->filename) {
    s->lines = (uint16_t *)mrc_malloc(c, sizeof(uint16_t)*s->icapa);
  }
  s->lineno = prev->lineno;

  /* degug info */
  s->debug_start_pos = 0;
  if (s->filename) {
    mrc_debug_info_alloc(c, s->irep);
  }
  else {
    s->irep->debug_info = NULL;
  }
  s->c = prev->c;
  s->filename_index = prev->filename_index;
  s->rlev = prev->rlev + 1;
  return s;
}

static mrc_bool
no_optimize(mrc_codegen_scope *s)
{
  if (s && s->c && s->c->no_optimize)
    return TRUE;
  return FALSE;
}

struct mrc_insn_data
mrc_decode_insn(const mrc_code *pc)
{
  struct mrc_insn_data data = { 0 };
  if (pc == 0) return data;
  data.addr = pc;
  mrc_code insn = READ_B();
  uint16_t a = 0;
  uint16_t b = 0;
  uint16_t cc = 0;

  switch (insn) {
#define FETCH_Z() /* empty */
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x (); break;
#include "mrc_ops.h"
#undef OPCODE
  }
  switch (insn) {
  case OP_EXT1:
    insn = READ_B();
    switch (insn) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _1 (); break;
#include "mrc_ops.h"
#undef OPCODE
    }
    break;
  case OP_EXT2:
    insn = READ_B();
    switch (insn) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _2 (); break;
#include "mrc_ops.h"
#undef OPCODE
    }
    break;
  case OP_EXT3:
    insn = READ_B();
    switch (insn) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _3 (); break;
#include "mrc_ops.h"
#undef OPCODE
    }
    break;
  default:
    break;
  }
  data.insn = insn;
  data.a = a;
  data.b = b;
  data.cc = cc;
  return data;
}

#undef OPCODE
#define Z 1
#define S 3
#define W 4
#define OPCODE(_,x) x,
/* instruction sizes */
static uint8_t mrc_insn_size[] = {
#define B 2
#define BB 3
#define BBB 4
#define BS 4
#define BSS 6
#include "mrc_ops.h"
#undef B
#undef BB
#undef BBB
#undef BS
#undef BSS
};
/* EXT1 instruction sizes */
static uint8_t mrc_insn_size1[] = {
#define B 3
#define BB 4
#define BBB 5
#define BS 5
#define BSS 7
#include "mrc_ops.h"
#undef B
#undef BS
#undef BSS
};
/* EXT2 instruction sizes */
static uint8_t mrc_insn_size2[] = {
#define B 2
#define BS 4
#define BSS 6
#include "mrc_ops.h"
#undef B
#undef BB
#undef BBB
#undef BS
#undef BSS
};
/* EXT3 instruction sizes */
#define B 3
#define BB 5
#define BBB 6
#define BS 5
#define BSS 7
static uint8_t mrc_insn_size3[] = {
#include "mrc_ops.h"
};
#undef B
#undef BB
#undef BBB
#undef BS
#undef BSS
#undef OPCODE

static const mrc_code*
mrc_prev_pc(mrc_codegen_scope *s, const mrc_code *pc)
{
  const mrc_code *prev_pc = NULL;
  const mrc_code *i = s->iseq;

  while (i<pc) {
    uint8_t insn = i[0];
    prev_pc = i;
    switch (insn) {
    case OP_EXT1:
      i += mrc_insn_size1[i[1]] + 1;
      break;
    case OP_EXT2:
      i += mrc_insn_size2[i[1]] + 1;
      break;
    case OP_EXT3:
      i += mrc_insn_size3[i[1]] + 1;
      break;
    default:
      i += mrc_insn_size[insn];
      break;
    }
  }
  return prev_pc;
}

#define pc_addr(s) &((s)->iseq[(s)->pc])
#define addr_pc(s, addr) (uint32_t)((addr) - s->iseq)
#define rewind_pc(s) s->pc = s->lastpc

static struct mrc_insn_data
mrc_last_insn(mrc_codegen_scope *s)
{
  if (s->pc == 0) {
    struct mrc_insn_data data = { OP_NOP, 0, 0, 0, NULL };
    return data;
  }
  return mrc_decode_insn(&s->iseq[s->lastpc]);
}

static mrc_bool
get_int_operand(mrc_codegen_scope *s, struct mrc_insn_data *data, mrc_int *n)
{
  switch (data->insn) {
  case OP_LOADI__1:
    *n = -1;
    return TRUE;

  case OP_LOADINEG:
    *n = -data->b;
    return TRUE;

  case OP_LOADI_0: case OP_LOADI_1: case OP_LOADI_2: case OP_LOADI_3:
  case OP_LOADI_4: case OP_LOADI_5: case OP_LOADI_6: case OP_LOADI_7:
    *n = data->insn - OP_LOADI_0;
    return TRUE;

  case OP_LOADI8:
  case OP_LOADI16:
    *n = (int16_t)data->b;
    return TRUE;

  case OP_LOADI32:
    *n = (int32_t)((uint32_t)data->b<<16)+data->cc;
    return TRUE;

  case OP_LOADL:
    {
      mrc_pool_value *pv = &s->pool[data->b];

      if (pv->tt == IREP_TT_INT32) {
        *n = (mrc_int)pv->u.i32;
      }
#ifdef MRC_INT64
      else if (pv->tt == IREP_TT_INT64) {
        *n = (mrc_int)pv->u.i64;
      }
#endif
      else {
        return FALSE;
      }
    }
    return TRUE;

  default:
    return FALSE;
  }
}

static mrc_bool
no_peephole(mrc_codegen_scope *s)
{
  return no_optimize(s) || s->lastlabel == s->pc || s->pc == 0 || s->pc == s->lastpc;
}

#define JMPLINK_START UINT32_MAX

static void
gen_jmpdst(mrc_codegen_scope *s, uint32_t pc)
{

  if (pc == JMPLINK_START) {
    pc = 0;
  }
  uint32_t pos2 = s->pc+2;
  int32_t off = pc - pos2;

  if (off > INT16_MAX || INT16_MIN > off) {
    codegen_error(s, "too big jump offset");
  }
  gen_S(s, (uint16_t)off);
}

static uint32_t
genjmp(mrc_codegen_scope *s, mrc_code i, uint32_t pc)
{
  uint32_t pos;

  genop_0(s, i);
  pos = s->pc;
  gen_jmpdst(s, pc);
  return pos;
}

#define genjmp_0(s,i) genjmp(s,i,JMPLINK_START)

static uint32_t
genjmp2(mrc_codegen_scope *s, mrc_code i, uint16_t a, uint32_t pc, int val)
{
  uint32_t pos;

  if (!no_peephole(s) && !val) {
    struct mrc_insn_data data = mrc_last_insn(s);

    switch (data.insn) {
    case OP_MOVE:
      if (data.a == a && data.a > s->nlocals) {
        rewind_pc(s);
        a = data.b;
      }
      break;
    case OP_LOADNIL:
    case OP_LOADFALSE:
      if (data.a == a || data.a > s->nlocals) {
        s->pc = addr_pc(s, data.addr);
        if (i == OP_JMPNOT || (i == OP_JMPNIL && data.insn == OP_LOADNIL)) {
          return genjmp(s, OP_JMP, pc);
        }
        else {                  /* OP_JMPIF */
          return JMPLINK_START;
        }
      }
      break;
    case OP_LOADTRUE: case OP_LOADI8: case OP_LOADINEG: case OP_LOADI__1:
    case OP_LOADI_0: case OP_LOADI_1: case OP_LOADI_2: case OP_LOADI_3:
    case OP_LOADI_4: case OP_LOADI_5: case OP_LOADI_6: case OP_LOADI_7:
      if (data.a == a || data.a > s->nlocals) {
        s->pc = addr_pc(s, data.addr);
        if (i == OP_JMPIF) {
          return genjmp(s, OP_JMP, pc);
        }
        else {                  /* OP_JMPNOT and OP_JMPNIL */
          return JMPLINK_START;
        }
      }
      break;
    }
  }

  if (a > 0xff) {
    check_no_ext_ops(s, a, 0);
    gen_B(s, OP_EXT1);
    genop_0(s, i);
    gen_S(s, a);
  }
  else {
    genop_0(s, i);
    gen_B(s, (uint8_t)a);
  }
  pos = s->pc;
  gen_jmpdst(s, pc);
  return pos;
}

#define genjmp2_0(s,i,a,val) genjmp2(s,i,a,JMPLINK_START,val)

static int new_lit_int(mrc_codegen_scope *s, mrc_int num);

static void
gen_int(mrc_codegen_scope *s, uint16_t dst, mrc_int i)
{
  if (i < 0) {
    if (i == -1) genop_1(s, OP_LOADI__1, dst);
    else if (i >= -0xff) genop_2(s, OP_LOADINEG, dst, (uint16_t)-i);
    else if (i >= INT16_MIN) genop_2S(s, OP_LOADI16, dst, (uint16_t)i);
    else if (i >= INT32_MIN) genop_2SS(s, OP_LOADI32, dst, (uint32_t)i);
    else goto int_lit;
  }
  else if (i < 8) genop_1(s, OP_LOADI_0 + (uint8_t)i, dst);
  else if (i <= 0xff) genop_2(s, OP_LOADI8, dst, (uint16_t)i);
  else if (i <= INT16_MAX) genop_2S(s, OP_LOADI16, dst, (uint16_t)i);
  else if (i <= INT32_MAX) genop_2SS(s, OP_LOADI32, dst, (uint32_t)i);
  else {
  int_lit:
    genop_2(s, OP_LOADL, dst, new_lit_int(s, i));
  }
}

static void
gen_move(mrc_codegen_scope *s, uint16_t dst, uint16_t src, int nopeep)
{
  if (nopeep || no_peephole(s)) goto normal;
  else if (dst == src) return;
  else {
    struct mrc_insn_data data = mrc_last_insn(s);

    switch (data.insn) {
    case OP_MOVE:
      if (dst == src) return;   /* remove useless MOVE */
      if (data.a == src) {
        if (data.b == dst)      /* skip swapping MOVE */
          return;
        if (data.a < s->nlocals) goto normal;
        rewind_pc(s);
        s->lastpc = addr_pc(s, mrc_prev_pc(s, data.addr));
        gen_move(s, dst, data.b, FALSE);
        return;
      }
      if (dst == data.a) {      /* skip overwritten move */
        rewind_pc(s);
        s->lastpc = addr_pc(s, mrc_prev_pc(s, data.addr));
        gen_move(s, dst, src, FALSE);
        return;
      }
      goto normal;
    case OP_LOADNIL: case OP_LOADSELF: case OP_LOADTRUE: case OP_LOADFALSE:
    case OP_LOADI__1:
    case OP_LOADI_0: case OP_LOADI_1: case OP_LOADI_2: case OP_LOADI_3:
    case OP_LOADI_4: case OP_LOADI_5: case OP_LOADI_6: case OP_LOADI_7:
      if (data.a != src || data.a < s->nlocals) goto normal;
      rewind_pc(s);
      genop_1(s, data.insn, dst);
      return;
    case OP_HASH:
      if (data.b != 0) goto normal;
      /* fall through */
    case OP_LOADI8: case OP_LOADINEG:
    case OP_LOADL: case OP_LOADSYM:
    case OP_GETGV: case OP_GETSV: case OP_GETIV: case OP_GETCV:
    case OP_GETCONST: case OP_STRING:
    case OP_LAMBDA: case OP_BLOCK: case OP_METHOD: case OP_BLKPUSH:
      if (data.a != src || data.a < s->nlocals) goto normal;
      rewind_pc(s);
      genop_2(s, data.insn, dst, data.b);
      return;
    case OP_LOADI16:
      if (data.a != src || data.a < s->nlocals) goto normal;
      rewind_pc(s);
      genop_2S(s, data.insn, dst, data.b);
      return;
    case OP_LOADI32:
      if (data.a != src || data.a < s->nlocals) goto normal;
      else {
        uint32_t i = (uint32_t)data.b<<16|data.cc;
        rewind_pc(s);
        genop_2SS(s, data.insn, dst, i);
      }
      return;
    case OP_ARRAY:
      if (data.a != src || data.a < s->nlocals || data.a < dst) goto normal;
      rewind_pc(s);
      if (data.b == 0 || dst == data.a)
        genop_2(s, OP_ARRAY, dst, 0);
      else
        genop_3(s, OP_ARRAY2, dst, data.a, data.b);
      return;
    case OP_ARRAY2:
      if (data.a != src || data.a < s->nlocals || data.a < dst) goto normal;
      rewind_pc(s);
      genop_3(s, OP_ARRAY2, dst, data.b, data.cc);
      return;
    case OP_AREF:
    case OP_GETUPVAR:
      if (data.a != src || data.a < s->nlocals) goto normal;
      rewind_pc(s);
      genop_3(s, data.insn, dst, data.b, data.cc);
      return;
    case OP_ADDI: case OP_SUBI:
      if (addr_pc(s, data.addr) == s->lastlabel || data.a != src || data.a < s->nlocals) goto normal;
      else {
        struct mrc_insn_data data0 = mrc_decode_insn(mrc_prev_pc(s, data.addr));
        if (data0.insn != OP_MOVE || data0.a != data.a || data0.b != dst) goto normal;
        s->pc = addr_pc(s, data0.addr);
        if (addr_pc(s, data0.addr) != s->lastlabel) {
          /* constant folding */
          struct mrc_insn_data data1 = mrc_decode_insn(mrc_prev_pc(s, data0.addr));
          mrc_int n;
          if (data1.a == dst && get_int_operand(s, &data1, &n)) {
            if ((data.insn == OP_ADDI && !mrc_int_add_overflow(n, data.b, &n)) ||
                (data.insn == OP_SUBI && !mrc_int_sub_overflow(n, data.b, &n))) {
              s->pc = addr_pc(s, data1.addr);
              gen_int(s, dst, n);
              return;
            }
          }
        }
        /* ADDILV/SUBILV fusion: MOVE temp local; ADDI/SUBI temp imm; MOVE local temp */
        /* -> ADDILV/SUBILV local temp imm (temp is working space for method fallback) */
        genop_3(s, data.insn == OP_ADDI ? OP_ADDILV : OP_SUBILV, dst, data.a, data.b);
        return;
      }
      genop_2(s, data.insn, dst, data.b);
      return;
    default:
      break;
    }
  }
 normal:
  genop_2(s, OP_MOVE, dst, src);
  return;
}

static int
lv_idx(mrc_codegen_scope *s, mrc_sym id)
{
  if (!s->lv) return 0;
  for (size_t n = 0; n < s->lv->size; n++) {
    if (s->lv->ids[n] == id) return n + 1;
  }
  return 0;
}


#define MRC_PROC_CFUNC_FL 128
#define MRC_PROC_CFUNC_P(p) (((p)->flags & MRC_PROC_CFUNC_FL) != 0)
#define MRC_PROC_SCOPE 2048
#define MRC_PROC_SCOPE_P(p) (((p)->flags & MRC_PROC_SCOPE) != 0)

static int
search_upvar(mrc_codegen_scope *s, mrc_sym id, int *idx)
{
  int lv = 0;
  mrc_codegen_scope *up = s->prev;

  while (up) {
    *idx = lv_idx(up, id);
    if (*idx > 0) {
      return lv;
    }
    lv++;
    up = up->prev;
  }

#if defined(MRC_TARGET_MRUBY)
  const struct RProc *u;

  if (lv < 1) lv = 1;
  u = s->c->upper;
  if (id != PM_CONSTANT_ID_UNSET && id <= s->c->p->constant_pool.size) {
    pm_constant_t *constant = pm_constant_pool_id_to_constant(&s->c->p->constant_pool, id);
    mrc_sym intern = mrb_intern(s->c->mrb, (const char *)constant->start, constant->length);
    while (u && !MRC_PROC_CFUNC_P(u)) {
      const struct mrc_irep *ir = u->body.irep;
      uint_fast16_t n = ir->nlocals;
      int i;
      const mrc_sym *v = ir->lv;
      if (v) {
        for (i=1; n > 1; n--, v++, i++) {
          if (*v == intern) {
            *idx = i;
            return lv - 1;
          }
        }
      }
      if (MRC_PROC_SCOPE_P(u)) break;
      u = u->upper;
      lv++;
    }
  }
#endif

  if (id == MRC_OPSYM_2(and)) {
    codegen_error(s, "No anonymous block parameter");
  }
  else if (id == MRC_OPSYM_2(mul)) {
    codegen_error(s, "No anonymous rest parameter");
  }
  else if (id == MRC_OPSYM_2(pow)) {
    codegen_error(s, "No anonymous keyword rest parameter");
  }
  else {
    codegen_error(s, "Can't find local variables");
  }
  return -1; /* not reached */
}

static void
gen_getupvar(mrc_codegen_scope *s, uint16_t dst, mrc_sym id, int depth)
{
  int idx;
  int lv = search_upvar(s, id, &idx);

  mrc_assert(lv == depth-1);

  if (!no_peephole(s)) {
    struct mrc_insn_data data = mrc_last_insn(s);
    if (data.insn == OP_SETUPVAR && data.a == dst && data.b == idx && data.cc == lv) {
      /* skip GETUPVAR right after SETUPVAR */
      return;
    }
  }
  genop_3(s, OP_GETUPVAR, dst, idx, lv);
}

static void
gen_setupvar(mrc_codegen_scope *s, uint16_t dst, mrc_sym id, int depth)
{
  int idx;
  int lv = search_upvar(s, id, &idx);

  mrc_assert(lv == depth-1);

  if (!no_peephole(s)) {
    struct mrc_insn_data data = mrc_last_insn(s);
    if (data.insn == OP_MOVE && data.a == dst) {
      dst = data.b;
      rewind_pc(s);
    }
  }
  genop_3(s, OP_SETUPVAR, dst, idx, lv);
}

static void
gen_return(mrc_codegen_scope *s, uint8_t op, uint16_t src)
{
  if (no_peephole(s)) {
    genop_1(s, op, src);
  }
  else {
    struct mrc_insn_data data = mrc_last_insn(s);

    if (data.insn == OP_MOVE && src == data.a) {
      rewind_pc(s);
      genop_1(s, op, data.b);
    }
    else if (data.insn == OP_LOADSELF && src == data.a && op == OP_RETURN) {
      /* LOADSELF + RETURN -> RETSELF */
      rewind_pc(s);
      genop_0(s, OP_RETSELF);
    }
    else if (data.insn == OP_LOADNIL && src == data.a && op == OP_RETURN) {
      /* LOADNIL + RETURN -> RETNIL */
      rewind_pc(s);
      genop_0(s, OP_RETNIL);
    }
    else if (data.insn == OP_LOADTRUE && src == data.a && op == OP_RETURN) {
      /* LOADTRUE + RETURN -> RETTRUE */
      rewind_pc(s);
      genop_0(s, OP_RETTRUE);
    }
    else if (data.insn == OP_LOADFALSE && src == data.a && op == OP_RETURN) {
      /* LOADFALSE + RETURN -> RETFALSE */
      rewind_pc(s);
      genop_0(s, OP_RETFALSE);
    }
    else if (data.insn != OP_RETURN && data.insn != OP_RETSELF && data.insn != OP_RETNIL &&
             data.insn != OP_RETTRUE && data.insn != OP_RETFALSE) {
      genop_1(s, op, src);
    }
  }
}

static void*
simple_realloc(mrc_ccontext *c, void *p, size_t len)
{
  if (len == 0) return p;
  return mrc_realloc(c, p, len);
}

static const char*
mrc_parser_get_filename(mrc_ccontext *c, uint16_t idx) {
  if (idx >= c->filename_table_length) return 0;
  else {
    return c->filename_table[idx].filename;
  }
}

static void
scope_finish(mrc_codegen_scope *s)
{
  mrc_irep *irep = s->irep;

  if (0xff < s->nlocals) {
    codegen_error(s, "too many local variables");
  }
  irep->flags = 0;
  if (s->iseq) {
    size_t catchsize = sizeof(struct mrc_irep_catch_handler) * irep->clen;
    irep->iseq = (const mrc_code *)mrc_realloc(s->c, s->iseq, sizeof(mrc_code)*s->pc + catchsize);
    irep->ilen = s->pc;
    if (0 < irep->clen) {
      memcpy((void *)(irep->iseq + irep->ilen), s->catch_table, catchsize);
    }
  }
  else {
    irep->clen = 0;
  }
  mrc_free(s->c, s->catch_table);
  s->catch_table = NULL;
  irep->pool = (const mrc_pool_value *)simple_realloc(s->c, s->pool, sizeof(mrc_pool_value)*irep->plen);
  irep->syms = (const mrc_sym *)simple_realloc(s->c, s->syms, sizeof(mrc_sym)*irep->slen);
  irep->reps = (const mrc_irep **)simple_realloc(s->c, s->reps, sizeof(mrc_irep *)*irep->rlen);
  if (s->filename) {
    const char *filename = mrc_parser_get_filename(s->c, s->filename_index);
    mrc_debug_info_append_file(s->c, s->irep->debug_info,
                               filename, s->lines, s->debug_start_pos, s->pc);
  }
  mrc_free(s->c, s->lines);
  irep->nlocals = s->nlocals;
  irep->nregs = s->nregs;

  mrc_gc_arena_restore(s->c, s->ai);
  mrc_pool_close(s->mpool);
}

static mrc_pool_value*
lit_pool_extend(mrc_codegen_scope *s)
{
  if (s->irep->plen == s->pcapa) {
    s->pcapa *= 2;
    s->pool = (mrc_pool_value*)mrc_realloc(s->c, s->pool, sizeof(mrc_pool_value)*s->pcapa);
  }

  return &s->pool[s->irep->plen++];
}

#ifndef MRC_NO_FLOAT
static int
new_lit_float(mrc_codegen_scope *s, mrc_float num)
{
  int i;
  mrc_pool_value *pv;

  for (i=0; i<s->irep->plen; i++) {
    mrc_float f;
    pv = &s->pool[i];
    if (pv->tt != IREP_TT_FLOAT) continue;
    f = pv->u.f;
    if (f == num && !signbit(f) == !signbit(num)) return i;
  }

  pv = lit_pool_extend(s);

  pv->tt = IREP_TT_FLOAT;
  pv->u.f = num;

  return i;
}
#endif

static int
new_sym(mrc_codegen_scope *s, mrc_sym sym)
{
  int i, len;

  mrc_assert(s->irep);

  len = s->irep->slen;
  for (i=0; i<len; i++) {
    if (s->syms[i] == sym) return i;
  }
  if (s->irep->slen >= s->scapa) {
    s->scapa *= 2;
    if (s->scapa > 0xffff) {
      codegen_error(s, "too many symbols");
    }
    s->syms = (mrc_sym*)mrc_realloc(s->c, s->syms, sizeof(mrc_sym)*s->scapa);
  }
  s->syms[s->irep->slen] = sym;
  return s->irep->slen++;
}

static void
gen_addsub(mrc_codegen_scope *s, uint8_t op, uint16_t dst)
{
  if (no_peephole(s)) {
  normal:
    genop_1(s, op, dst);
    return;
  }
  else {
    struct mrc_insn_data data = mrc_last_insn(s);
    mrc_int n;

    if (!get_int_operand(s, &data, &n)) {
      /* not integer immediate */
      goto normal;
    }
    struct mrc_insn_data data0 = mrc_decode_insn(mrc_prev_pc(s, data.addr));
    mrc_int n0;
    if (addr_pc(s, data.addr) == s->lastlabel || !get_int_operand(s, &data0, &n0)) {
      /* Fold to OP_ADDI/OP_SUBI only for non-negative 8-bit n; flipping op
         for negative n would change the method sent on user override (#2557). */
      if (n < 0 || n > UINT8_MAX) goto normal;
      rewind_pc(s);
      if (n == 0) return;
      if (op == OP_ADD) genop_2(s, OP_ADDI, dst, (uint16_t)n);
      else genop_2(s, OP_SUBI, dst, (uint16_t)n);
      return;
    }
    if (op == OP_ADD) {
      if (mrc_int_add_overflow(n0, n, &n)) goto normal;
    }
    else { /* OP_SUB */
      if (mrc_int_sub_overflow(n0, n, &n)) goto normal;
    }
    s->pc = addr_pc(s, data0.addr);
    gen_int(s, dst, n);
  }
}

static void
gen_muldiv(mrc_codegen_scope *s, uint8_t op, uint16_t dst)
{
  if (no_peephole(s)) {
  normal:
    genop_1(s, op, dst);
    return;
  }
  else {
    struct mrc_insn_data data = mrc_last_insn(s);
    mrc_int n, n0;
    if (addr_pc(s, data.addr) == s->lastlabel || !get_int_operand(s, &data, &n)) {
      /* not integer immediate */
      goto normal;
    }
    struct mrc_insn_data data0 = mrc_decode_insn(mrc_prev_pc(s, data.addr));
    if (!get_int_operand(s, &data0, &n0)) {
      goto normal;
    }
    if (op == OP_MUL) {
      if (mrc_int_mul_overflow(n0, n, &n)) goto normal;
    }
    else { /* OP_DIV */
      if (n == 0) goto normal;
      if (n0 == MRC_INT_MIN && n == -1) goto normal;
      n = mrc_div_int(n0, n);
    }
    s->pc = addr_pc(s, data0.addr);
    gen_int(s, dst, n);
  }
}

static mrc_bool
gen_uniop(mrc_codegen_scope *s, mrc_sym sym, uint16_t dst)
{
  if (no_peephole(s)) return FALSE;
  struct mrc_insn_data data = mrc_last_insn(s);
  mrc_int n;

  if (!get_int_operand(s, &data, &n)) return FALSE;
  if (sym == MRC_OPSYM_2(add)) {
    /* unary plus does nothing */
  }
  else if (sym == MRC_OPSYM_2(sub)) {
    if (n == MRC_INT_MIN) return FALSE;
    n = -n;
  }
  else if (sym == MRC_OPSYM_2(neg)) {
    n = ~n;
  }
  else {
    return FALSE;
  }
  s->pc = addr_pc(s, data.addr);
  gen_int(s, dst, n);
  return TRUE;
}

static mrc_bool
gen_binop(mrc_codegen_scope *s, mrc_sym op, uint16_t dst)
{
  if (no_peephole(s)) return FALSE;
  else if (op == MRC_OPSYM_2(aref)) {
    /* GETIDX0 fusion: MOVE dst arr; LOADI_0 dst+1 -> GETIDX0 dst arr */
    struct mrc_insn_data data = mrc_last_insn(s);
    if (data.insn == OP_LOADI_0 && data.a == dst+1 && addr_pc(s, data.addr) != s->lastlabel) {
      struct mrc_insn_data data0 = mrc_decode_insn(mrc_prev_pc(s, data.addr));
      if (data0.insn == OP_MOVE && data0.a == dst && data0.b != dst) {
        s->pc = addr_pc(s, data0.addr);
        genop_2(s, OP_GETIDX0, dst, data0.b);
        return TRUE;
      }
    }
    genop_1(s, OP_GETIDX, dst);
    return TRUE;
  }
  else {
    struct mrc_insn_data data = mrc_last_insn(s);
    mrc_int n, n0;
    if (addr_pc(s, data.addr) == s->lastlabel || !get_int_operand(s, &data, &n)) {
      /* not integer immediate */
      return FALSE;
    }
    struct mrc_insn_data data0 = mrc_decode_insn(mrc_prev_pc(s, data.addr));
    if (!get_int_operand(s, &data0, &n0)) {
      return FALSE;
    }
    if (op == MRC_OPSYM_2(lshift)) {
      if (!mrc_num_shift(n0, n, &n)) return FALSE;
    }
    else if (op == MRC_OPSYM_2(rshift)) {
      if (n == MRC_INT_MIN) return FALSE;
      if (!mrc_num_shift(n0, -n, &n)) return FALSE;
    }
    else if (op == MRC_OPSYM_2(mod) && n != 0) {
      if (n0 == MRC_INT_MIN && n == -1) {
        n = 0;
      }
      else {
        mrc_int n1 = n0 % n;
        if ((n0 < 0) != (n < 0) && n1 != 0) {
          n1 += n;
        }
        n = n1;
      }
    }
    else if (op == MRC_OPSYM_2(and)) {
      n = n0 & n;
    }
    else if (op == MRC_OPSYM_2(or)) {
      n = n0 | n;
    }
    else if (op == MRC_OPSYM_2(xor)) {
      n = n0 ^ n;
    }
    else {
      return FALSE;
    }
    s->pc = addr_pc(s, data0.addr);
    gen_int(s, dst, n);
    return TRUE;
  }
}

#define JMPLINK_START UINT32_MAX

static uint32_t
dispatch(mrc_codegen_scope *s, uint32_t pos0)
{
  int32_t pos1;
  int32_t offset;
  int16_t newpos;

  if (pos0 == JMPLINK_START) return 0;

  pos1 = pos0 + 2;
  offset = s->pc - pos1;
  if (offset > INT16_MAX) {
    codegen_error(s, "too big jmp offset");
  }
  s->lastlabel = s->pc;
  newpos = (int16_t)PEEK_S(s->iseq+pos0);
  emit_S(s, pos0, (uint16_t)offset);
  if (newpos == 0) return 0;
  return pos1+newpos;
}

static void
dispatch_linked(mrc_codegen_scope *s, uint32_t pos)
{
  if (pos==JMPLINK_START) return;
  for (;;) {
    pos = dispatch(s, pos);
    if (pos==0) break;
  }
}

static int
new_litbint(mrc_codegen_scope *s, const char *p, int base, mrc_bool neg)
{
  int i;
  size_t plen;
  mrc_pool_value *pv;

  plen = strlen(p);
  if (plen > 255) {
    codegen_error(s, "integer too big");
  }
  for (i=0; i<s->irep->plen; i++) {
    size_t len;
    pv = &s->pool[i];
    if (pv->tt != IREP_TT_BIGINT) continue;
    len = pv->u.str[0];
    /* str[1] encodes the sign as -base for negative values, so compare the
       signed base; otherwise a negative literal would dedup onto a positive
       one of the same magnitude and lose its sign. */
    if (len == plen && pv->u.str[1] == (neg ? -base : base) && memcmp(pv->u.str+2, p, len) == 0)
      return i;
  }

  pv = lit_pool_extend(s);

  char *buf;
  pv->tt = IREP_TT_BIGINT;
  buf = (char*)mrc_realloc(s->c, NULL, plen+3);
  buf[0] = (char)plen;
  if (neg) buf[1] = -base;
  else buf[1] = base;
  memcpy(buf+2, p, plen);
  buf[plen+2] = '\0';
  pv->u.str = buf;

  return i;
}

static int
new_lit_str(mrc_codegen_scope *s, const char *str, mrc_int len)
{
  int i;
  mrc_pool_value *pv;

  for (i=0; i<s->irep->plen; i++) {
    pv = &s->pool[i];
    if (pv->tt & IREP_TT_NFLAG) continue;
    mrc_int plen = pv->tt>>2;
    if (len != plen) continue;
    if (memcmp(pv->u.str, str, plen) == 0)
      return i;
  }

  pv = lit_pool_extend(s);

  //if (mrb_ro_data_p(str)) {
  //  pv->tt = (uint32_t)(len<<2) | IREP_TT_SSTR;
  //  pv->u.str = str;
  //}
  //else {
    char *p;
    pv->tt = (uint32_t)(len<<2) | IREP_TT_STR;
    p = (char*)mrc_realloc(s->c, NULL, len+1);
    memcpy(p, str, len);
    p[len] = '\0';
    pv->u.str = p;
  //}

  return i;
}

static int
new_lit_cstr(mrc_codegen_scope *s, const char *str)
{
  return new_lit_str(s, str, (mrc_int)strlen(str));
}

static int
new_lit_int(mrc_codegen_scope *s, mrc_int num)
{
  int i;
  mrc_pool_value *pv;

  for (i=0; i<s->irep->plen; i++) {
    pv = &s->pool[i];
    if (pv->tt == IREP_TT_INT32) {
      if (num == pv->u.i32) return i;
    }
#ifdef MRC_64BIT
    else if (pv->tt == IREP_TT_INT64) {
      if (num == pv->u.i64) return i;
    }
    continue;
#endif
  }

  pv = lit_pool_extend(s);

#ifdef MRC_INT64
  pv->tt = IREP_TT_INT64;
  pv->u.i64 = num;
#else
  pv->tt = IREP_TT_INT32;
  pv->u.i32 = num;
#endif

  return i;
}

static int
catch_handler_new(mrc_codegen_scope *s)
{
  size_t newsize = sizeof(struct mrc_irep_catch_handler) * (s->irep->clen + 1);
  s->catch_table = (struct mrc_irep_catch_handler*)mrc_realloc(s->c, (void*)s->catch_table, newsize);
  return s->irep->clen++;
}

static void
catch_handler_set(mrc_codegen_scope *s, int ent, enum mrc_catch_type type, uint32_t begin, uint32_t end, uint32_t target)
{
  struct mrc_irep_catch_handler *e;
  mrc_assert(ent >= 0 && ent < s->irep->clen);
  e = &s->catch_table[ent];
  mrc_uint8_to_bin(type, &e->type);
  mrc_irep_catch_handler_pack(begin, e->begin);
  mrc_irep_catch_handler_pack(end, e->end);
  mrc_irep_catch_handler_pack(target, e->target);
}

static void
raise_error(mrc_codegen_scope *s, const char *msg)
{
  int idx = new_lit_cstr(s, msg);

  genop_1(s, OP_ERR, idx);
}

static struct loopinfo*
loop_push(mrc_codegen_scope *s, enum looptype t)
{
  struct loopinfo *p = (struct loopinfo*)codegen_palloc(s, sizeof(struct loopinfo));

  p->type = t;
  p->pc0 = p->pc1 = p->pc2 = JMPLINK_START;
  p->prev = s->loop;
  p->reg = cursp();
  s->loop = p;

  return p;
}

static void gen_retval(mrc_codegen_scope *s, mrc_node *tree);

static void
loop_break(mrc_codegen_scope *s, mrc_node *tree)
{
  if (!s->loop) {
    codegen(s, tree, NOVAL);
    raise_error(s, "unexpected break");
  }
  else {
    struct loopinfo *loop;

    loop = s->loop;
    if (tree) {
      if (loop->reg < 0) {
        codegen(s, tree, NOVAL);
      }
      else {
        gen_retval(s, tree);
      }
    }
    while (loop) {
      if (loop->type == LOOP_BEGIN) {
        loop = loop->prev;
      }
      else if (loop->type == LOOP_RESCUE) {
        loop = loop->prev;
      }
      else{
        break;
      }
    }
    if (!loop) {
      raise_error(s, "unexpected break");
      return;
    }

    if (loop->type == LOOP_NORMAL) {
      int tmp;

      if (loop->reg >= 0) {
        if (tree) {
          gen_move(s, loop->reg, cursp(), 0);
        }
        else {
          genop_1(s, OP_LOADNIL, loop->reg);
        }
      }
      tmp = genjmp(s, OP_JMPUW, loop->pc2);
      loop->pc2 = tmp;
    }
    else {
      if (!tree) {
        genop_1(s, OP_LOADNIL, cursp());
      }
      gen_return(s, OP_BREAK, cursp());
    }
  }
}

static void
loop_pop(mrc_codegen_scope *s, int val)
{
  if (val) {
    genop_1(s, OP_LOADNIL, cursp());
  }
  dispatch_linked(s, s->loop->pc2);
  s->loop = s->loop->prev;
  if (val) push();
}

static void
gen_blkmove(mrc_codegen_scope *s, uint16_t ainfo, int lv)
{
  int m1 = (ainfo>>7)&0x3f;
  int r  = (ainfo>>6)&0x1;
  int m2 = (ainfo>>1)&0x1f;
  int kd = (ainfo)&0x1;
  int off = m1+r+m2+kd+1;
  if (lv == 0) {
    gen_move(s, cursp(), off, 0);
  }
  else {
    genop_3(s, OP_GETUPVAR, cursp(), off, lv);
  }
  push();
}

static void
gen_setxv(mrc_codegen_scope *s, uint8_t op, uint16_t dst, mrc_sym sym, int val)
{
  int idx = new_sym(s, sym);
  if (!val && !no_peephole(s)) {
    struct mrc_insn_data data = mrc_last_insn(s);
    if (data.insn == OP_MOVE && data.a == dst) {
      dst = data.b;
      rewind_pc(s);
    }
  }
  genop_2(s, op, dst, idx);
}

static mrc_irep *
generate_code(mrc_ccontext *c, mrc_node *node, int val)
{
  mrc_codegen_scope *scope = scope_new(c, NULL, NULL);
  struct mrc_jmpbuf *prev_jmp = c->jmp;
  struct mrc_jmpbuf jmpbuf;

  c->jmp = &jmpbuf;

  scope->c = c;
  scope->filename_index = 0;
  scope->filename = (const char *)c->filename_table[0].filename;

  MRC_TRY(c->jmp) {
    codegen(scope, node, val);
    // TODO: mrc_ccontext has an upper Proc if MRC_TARGET_MRUBY
    //proc->c = NULL;
    //if (mrb->c->cibase && mrb->c->cibase->proc == proc->upper) {
    //  proc->upper = NULL;
    //}
    //mrc_irep_free(c, scope->irep);
    mrc_irep *irep = scope->irep;
    mrc_pool_close(scope->mpool);
    c->jmp = prev_jmp;
    return irep;
  }
  MRC_CATCH(c->jmp) {
    // TODO?
    //mrc_irep_free(c, scope->irep);
    mrc_pool_close(scope->mpool);
    c->jmp = prev_jmp;
    return NULL;
  }
  MRC_END_EXC(c->jmp);
}

MRC_API mrc_irep *
mrc_generate_code(mrc_ccontext *c, mrc_node *node)
{
  return generate_code(c, node, VAL);
}

#define CALL_MAXARGS 15
#define GEN_LIT_ARY_MAX 64
#define GEN_VAL_STACK_MAX 99

/*--------------------------------------------------------------------------
 * Prism dependent code
 *------------------------------------------------------------------------*/

#define nint(node) PM_NODE_TYPE(node)

#define CAST3(name, from, to) \
  pm_##name##_node_t *to = (pm_##name##_node_t *)from
#define CAST(name) CAST3(name,tree,cast)

static void gen_massignment(mrc_codegen_scope *s, mrc_node *tree, int rhs, int val);
static void gen_lvar(mrc_codegen_scope *s, mrc_sym name, int depth);
static void codegen_pattern(mrc_codegen_scope *s, mrc_node *pattern, int target,
                            uint32_t *fail_pos, int known_array_len);

static mrc_sym
nsym(mrc_parser_state *p, const uint8_t *start, size_t length)
{
  if (length == 0 || (start >= p->start && start < p->end)) {
    /* Source-backed bytes stay valid for the parser's lifetime. */
    return pm_constant_pool_insert_constant(&p->constant_pool, start, length);
  }
  /* Node-owned bytes (e.g. an unescaped symbol name containing escapes) are
     freed together with the AST, before the generated irep is dumped; copy
     them into pool-owned memory so the constant id stays valid. */
  uint8_t *copy = (uint8_t *)xmalloc(length);
  memcpy(copy, start, length);
  return pm_constant_pool_insert_owned(&p->constant_pool, copy, length);
}

static int32_t
node_lineno(mrc_ccontext *c, mrc_node *node)
{
  pm_location_t *loc = &((pm_node_t *)node)->location;
  int32_t abs_line = pm_newline_list_line(&c->p->newline_list, loc->start, 1);
  int32_t line_offset = c->lineno > 0 ? c->lineno - 1 : 0;
  uint32_t node_pos = (uint32_t)(loc->start - c->p->start);
  uint32_t file_start = 0;
  for (uint16_t i = 1; i < c->filename_table_length; i++) {
    if (c->filename_table[i].start <= node_pos) {
      file_start = c->filename_table[i].start;
    } else {
      break;
    }
  }
  if (file_start == 0) return abs_line + line_offset;
  int32_t file_start_line = pm_newline_list_line(&c->p->newline_list, c->p->start + file_start, 1);
  return abs_line - file_start_line + 1 + line_offset;
}

static mrc_bool
true_always(mrc_node *tree)
{
  switch (nint(tree)) {
  case PM_TRUE_NODE:
  case PM_INTEGER_NODE:
  case PM_STRING_NODE:
  case PM_SYMBOL_NODE:
    return TRUE;
  default:
    return FALSE;
  }
}

static mrc_bool
false_always(mrc_node *tree)
{
  switch (nint(tree)) {
  case PM_FALSE_NODE:
  case PM_NIL_NODE:
    return TRUE;
  default:
    return FALSE;
  }
}

static void
gen_retval(mrc_codegen_scope *s, mrc_node *tree)
{
  CAST(arguments);
  if (cast->arguments.size == 1 ) {
    if (nint(cast->arguments.nodes[0]) == PM_SPLAT_NODE) {
      pm_splat_node_t *splat = (pm_splat_node_t *)cast->arguments.nodes[0];
      if (splat->expression) {
        codegen(s, (mrc_node *)splat->expression, VAL);
      } else {
        /* anonymous splat: load local variable '*' */
        pm_constant_id_t astr = MRC_OPSYM_2(mul);
        gen_lvar(s, astr, 0);
      }
      pop();
      genop_1(s, OP_ARYSPLAT, cursp());
    }
    else {
      codegen(s, cast->arguments.nodes[0], VAL);
      pop();
    }
  }
  else {
    codegen(s, tree, VAL);
    pop();
  }
}

static void
gen_assignment_lvar(mrc_codegen_scope *s, int sp, mrc_sym name, int depth, int val)
{
  if (depth == 0) {
    int idx = lv_idx(s, name);
    if (idx != sp) {
      gen_move(s, idx, sp, val);
    }
  }
  else {
    gen_setupvar(s, sp, name, depth);
  }
}

static int
gen_values(mrc_codegen_scope *s, mrc_node *tree, int val, int limit)
{
  CAST(arguments);
  mrc_node *t;

  int n = 0;
  int first = 1;
  int slimit = GEN_VAL_STACK_MAX;

  if (limit == 0) limit = GEN_LIT_ARY_MAX;
  if (cursp() >= slimit) slimit = INT16_MAX;

  if (!val) {
    for (size_t i = 0; i < cast->arguments.size; i++) {
      t = (mrc_node *)cast->arguments.nodes[i];
      codegen(s, t, NOVAL);
      n++;
    }
    return n;
  }

  for (size_t i = 0; i < cast->arguments.size; i++) {
    t = (mrc_node *)cast->arguments.nodes[i];
    if (nint(t) == PM_KEYWORD_HASH_NODE) break;
    int is_splat = nint(t) == PM_SPLAT_NODE;
    int is_forwarding = nint(t) == PM_FORWARDING_ARGUMENTS_NODE;

    if (is_splat || is_forwarding || cursp() >= slimit) { /* flush stack */
      pop_n(n);
      if (first) {
        if (n == 0) {
          genop_1(s, OP_LOADNIL, cursp());
        }
        else {
          genop_2(s, OP_ARRAY, cursp(), n);
        }
        push();
        first = 0;
        limit = GEN_LIT_ARY_MAX;
      }
      else if (n > 0) {
        pop();
        genop_2(s, OP_ARYPUSH, cursp(), n);
        push();
      }
      n = 0;
    }
    if (is_splat) {
      pm_splat_node_t *splat = (pm_splat_node_t *)t;
      if (splat->expression) {
        CAST3(array, splat->expression, a);
        codegen(s, (mrc_node *)a, val);
      } else {
        /* anonymous splat: load local variable '*' */
        pm_constant_id_t astr = MRC_OPSYM_2(mul);
        gen_lvar(s, astr, 0);
      }
      pop(); pop();
      genop_1(s, OP_ARYCAT, cursp());
      push();
    }
    else if (is_forwarding) {
      int idx;
      /* ARYCAT rest args (*) into the flushed array */
      idx = lv_idx(s, MRC_OPSYM_2(mul));
      assert(idx != 0);
      gen_move(s, cursp(), idx, val);
      pop();
      genop_1(s, OP_ARYCAT, cursp());
      push();
      /* ** keyword hash */
      genop_2(s, OP_HASH, cursp(), 0);
      push();
      idx = lv_idx(s, MRC_OPSYM_2(pow));
      assert(idx != 0);
      gen_move(s, cursp(), idx, val);
      pop();
      genop_1(s, OP_HASHCAT, cursp());
      push();
      /* & block */
      idx = lv_idx(s, MRC_OPSYM_2(and));
      assert(idx != 0);
      gen_move(s, cursp(), idx, val);
      break;
    }
    else {
      codegen(s, t, val);
      n++;
    }
  }
  if (!first) {
    pop();
    if (n > 0) {
      pop_n(n);
      genop_2(s, OP_ARYPUSH, cursp(), n);
    }
    return -1;                  /* variable length */
  }
  else if (n > limit) {
    pop_n(n);
    genop_2(s, OP_ARRAY, cursp(), n);
    return -1;
  }
  return n;
}

static void
gen_assignment(mrc_codegen_scope *s, mrc_node *tree, mrc_node *rhs, int sp, int val)
{
  int idx;

  switch (nint(tree)) {
    case PM_LOCAL_VARIABLE_WRITE_NODE:
    case PM_LOCAL_VARIABLE_TARGET_NODE:
    case PM_INSTANCE_VARIABLE_WRITE_NODE:
    case PM_INSTANCE_VARIABLE_TARGET_NODE:
    case PM_CONSTANT_WRITE_NODE:
    case PM_CONSTANT_TARGET_NODE:
    case PM_GLOBAL_VARIABLE_WRITE_NODE:
    case PM_GLOBAL_VARIABLE_TARGET_NODE:
    case PM_CLASS_VARIABLE_WRITE_NODE:
    case PM_CLASS_VARIABLE_TARGET_NODE:
    case PM_MULTI_TARGET_NODE:
    case PM_REQUIRED_PARAMETER_NODE:
    case PM_INDEX_TARGET_NODE:
    case PM_CALL_TARGET_NODE:
    {
      if (rhs) {
        codegen(s, rhs, VAL);
        pop();
        sp = cursp();
      }
      break;
    }
    case PM_CONSTANT_PATH_WRITE_NODE:
      break;
    default:
    {
      codegen_error(s, "Not implemented (#1)");
      break;
    }
  }

  switch (nint(tree)) {
    case PM_LOCAL_VARIABLE_WRITE_NODE:
    case PM_LOCAL_VARIABLE_TARGET_NODE:
    case PM_REQUIRED_PARAMETER_NODE:
    {
      CAST(local_variable_write);
      gen_assignment_lvar(s, sp, cast->name, cast->depth + s->for_depth, val);
      break;
    }
    case PM_INSTANCE_VARIABLE_WRITE_NODE:
    case PM_INSTANCE_VARIABLE_TARGET_NODE:
    {
      CAST(instance_variable_write);
      gen_setxv(s, OP_SETIV, sp, cast->name, val);
      break;
    }
    case PM_CONSTANT_WRITE_NODE:
    case PM_CONSTANT_TARGET_NODE:
    {
      CAST(constant_write);
      gen_setxv(s, OP_SETCONST, sp, cast->name, val);
      break;
    }
    case PM_CONSTANT_PATH_WRITE_NODE:
    case PM_CONSTANT_PATH_TARGET_NODE:
    {
      CAST(constant_path_write);
      if (sp) {
        gen_move(s, cursp(), sp, 0);
      }
      sp = cursp();
      push();
      if (cast->target->parent) {
        codegen(s, cast->target->parent, VAL);
        idx = new_sym(s, cast->target->name);
      }
      else {   /* NODE_COLON3 */
        genop_1(s, OP_OCLASS, cursp());
        push();
        idx = new_sym(s, cast->target->name);
      }
      if (rhs) {
        codegen(s, rhs, VAL); pop();
        gen_move(s, sp, cursp(), 0);
      }
      pop_n(2);
      genop_2(s, OP_SETMCNST, sp, idx);
      break;
    }
    case PM_GLOBAL_VARIABLE_WRITE_NODE:
    case PM_GLOBAL_VARIABLE_TARGET_NODE:
    {
      CAST(global_variable_read);
      gen_setxv(s, OP_SETGV, sp, cast->name, val);
      break;
    }
    case PM_CLASS_VARIABLE_WRITE_NODE:
    case PM_CLASS_VARIABLE_TARGET_NODE:
    {
      CAST(class_variable_read);
      gen_setxv(s, OP_SETCV, sp, cast->name, val);
      break;
    }
    case PM_MULTI_TARGET_NODE:
    {
      gen_massignment(s, tree, sp, val);
      break;
    }
    case PM_INDEX_TARGET_NODE:
    {
      CAST(index_target);
      codegen(s, cast->receiver, VAL);
      int n = gen_values(s, (mrc_node *)cast->arguments, VAL, 14);
      genop_2(s, OP_MOVE, cursp(), cursp() - n * 2 + 1);
      if (n == 1) {
        pop_n(2);
        genop_1(s, OP_SETIDX, cursp());
      }
      else {
        pop_n(n+1);
        genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_OPSYM_2(aset)), n+1);
      }
      break;
    }
    case PM_CALL_TARGET_NODE:
    {
      CAST(call_target);
      codegen(s, cast->receiver, VAL);
      genop_2(s, OP_MOVE, cursp(), sp);
      pop();
      genop_3(s, OP_SEND, cursp(), new_sym(s, cast->name), 1);
      break;
    }
    default:
    {
      codegen_error(s, "Not implemented (#2)");
      break;
    }
  }
  if (val) push();
}

static int
scope_body(mrc_codegen_scope *s, mrc_node *tree, int val)
{
  mrc_constant_id_list *nlv;
  mrc_node *statements;
  switch (nint(tree)) {
    case PM_PROGRAM_NODE:
    {
      CAST3(program, tree, program);
      nlv = &program->locals;
      statements = (mrc_node *)program->statements;
      break;
    }
    case PM_CLASS_NODE:
    {
      CAST3(class, tree, class);
      nlv = &class->locals;
      statements = class->body;
      break;
    }
    case PM_SINGLETON_CLASS_NODE:
    {
      CAST3(singleton_class, tree, sclass);
      nlv = &sclass->locals;
      statements = sclass->body;
      break;
    }
    case PM_MODULE_NODE:
    {
      CAST3(module, tree, module);
      nlv = &module->locals;
      statements = module->body;
      break;
    }
    default:
    {
      codegen_error(s, "Not implemented (#3)");
      statements = NULL;
      nlv = NULL;
      break;
    }
  }
  mrc_codegen_scope *scope = scope_new(s->c, s, nlv);

  codegen(scope, statements, VAL);

  // For PICOIRB
  s->c->scope_sp = scope->sp - 1;

  gen_return(scope, OP_RETURN, scope->sp-1);
  if (!s->iseq) {
    genop_0(scope, OP_STOP);
  }
  scope_finish(scope);
  if (!s->irep) {
    /* should not happen */
    return 0;
  }
  return s->irep->rlen - 1;
}


static int
gen_hash(mrc_codegen_scope *s, mrc_node *tree, int val, int limit)
{
  struct pm_node_list elements;
  if (nint(tree) == PM_HASH_NODE) {
    CAST(hash);
    elements = cast->elements;
  }
  else {
    CAST(keyword_hash);
    elements = cast->elements;
  }

  int slimit = GEN_VAL_STACK_MAX;
  if (cursp() >= GEN_LIT_ARY_MAX) slimit = INT16_MAX;
  int len = 0;
  mrc_bool update = FALSE;
  mrc_bool first = TRUE;

  //while (tree) {
  for (size_t i = 0; i < elements.size; i++) {
    if (nint(elements.nodes[i]) == PM_ASSOC_SPLAT_NODE) {
      CAST3(assoc_splat, elements.nodes[i], assocsplat);
      if (val && first) {
        genop_2(s, OP_HASH, cursp(), 0);
        push();
        update = TRUE;
      }
      else if (val && len > 0) {
        pop_n(len*2);
        if (!update) {
          genop_2(s, OP_HASH, cursp(), len);
        }
        else {
          pop();
          genop_2(s, OP_HASHADD, cursp(), len);
        }
        push();
      }
      if (assocsplat->value) {
        codegen(s, assocsplat->value, val);
      } else if (val) {
        /* anonymous keyword splat: load local variable '**' */
        pm_constant_id_t dastr = MRC_OPSYM_2(pow);
        gen_lvar(s, dastr, 0);
      }
      if (val && (len > 0 || update)) {
        pop(); pop();
        genop_1(s, OP_HASHCAT, cursp());
        push();
      }
      update = TRUE;
      len = 0;
    }
    else {
      CAST3(assoc, elements.nodes[i], assoc);
      codegen(s, assoc->key, val);
      codegen(s, assoc->value, val);
      len++;
    }
    if (val && cursp() >= slimit) {
      pop_n(len*2);
      if (!update) {
        genop_2(s, OP_HASH, cursp(), len);
      }
      else {
        pop();
        genop_2(s, OP_HASHADD, cursp(), len);
      }
      push();
      update = TRUE;
      len = 0;
    }
    first = FALSE;
  }
  if (val && len > limit) {
    pop_n(len*2);
    genop_2(s, OP_HASH, cursp(), len);
    push();
    return -1;
  }
  if (update) {
    if (val && len > 0) {
      pop_n(len*2+1);
      genop_2(s, OP_HASHADD, cursp(), len);
      push();
    }
    return -1;                  /* variable length */
  }
  return len;
}

#if defined(MRC_TARGET_MRUBY)
static mrc_bool
mrc_mruby_numbered_parameter_upvar(mrc_codegen_scope *s, mrc_sym id, int *lv, int *idx)
{
  if (id == PM_CONSTANT_ID_UNSET || id > s->c->p->constant_pool.size) {
    return FALSE;
  }

  pm_constant_t *constant = pm_constant_pool_id_to_constant(&s->c->p->constant_pool, id);
  if (constant->length != 2 || constant->start[0] != '_' ||
      constant->start[1] < '1' || constant->start[1] > '9') {
    return FALSE;
  }

  mrc_sym intern = mrb_intern(s->c->mrb, (const char *)constant->start, constant->length);
  const struct RProc *u = s->c->upper;
  *lv = 0;
  while (u && !MRC_PROC_CFUNC_P(u)) {
    const struct mrc_irep *ir = u->body.irep;
    uint_fast16_t n = ir->nlocals;
    const mrc_sym *v = ir->lv;
    int number = constant->start[1] - '0';
    if (v) {
      for (int i = 1; n > 1; n--, v++, i++) {
        if (*v == intern) {
          *idx = i;
          return TRUE;
        }
      }
    }
    else if (number < ir->nlocals) {
      *idx = number;
      return TRUE;
    }
    if (MRC_PROC_SCOPE_P(u)) break;
    u = u->upper;
    (*lv)++;
  }
  return FALSE;
}
#endif

/* Attribute assignment (`recv.attr = v`, `recv[i] = v`) as an expression.
   Prism bundles the RHS as the last positional argument of the call node.
   The whole expression must evaluate to that RHS, not to the setter's
   return value, so the RHS is copied into a reserved slot below the call
   frame and used as the result while the SEND result is discarded. */
static void
gen_call_assign(mrc_codegen_scope *s, mrc_node *tree, int val, int safe)
{
  CAST(call);
  const mrc_sym sym = cast->name;
  int skip = 0, n = 0, noself = 0, noop = no_optimize(s);
  int top, callsp, opt_op = 0;

  if (!noop && sym == MRC_OPSYM_2(aset)) opt_op = OP_SETIDX;

  top = cursp();
  push();                    /* room for retval */
  callsp = cursp();

  /* receiver (an attribute write always has an explicit receiver; an
     explicit `self` must be materialized so OP_SETIDX can read it) */
  if (cast->receiver == NULL) {
    noself = 1;
    push();
  }
  else {
    codegen(s, cast->receiver, VAL);
  }
  if (safe) {
    int recv = cursp()-1;
    gen_move(s, cursp(), recv, 1);
    skip = genjmp2_0(s, OP_JMPNIL, cursp(), val);
  }

  /* positional arguments, the last of which is the RHS */
  CAST3(arguments, cast->arguments, arguments);
  if (arguments) {
    for (size_t i = 0; i < arguments->arguments.size; i++) {
      codegen(s, (mrc_node *)arguments->arguments.nodes[i], VAL);
      n++;
    }
  }
  if (val) {
    /* nopeep: keep the RHS in its argument slot for the SEND, while also
       copying it to the reserved result slot */
    gen_move(s, top, cursp()-1, 1);   /* preserve the RHS as the result */
  }

  push(); pop();
  s->sp = callsp;

  if (opt_op == OP_SETIDX && n == 2) {
    genop_1(s, OP_SETIDX, cursp());
  }
  else if (noself) {
    genop_3(s, OP_SSEND, cursp(), new_sym(s, sym), n);
  }
  else {
    genop_3(s, OP_SEND, cursp(), new_sym(s, sym), n);
  }

  if (safe) {
    dispatch(s, skip);
  }

  s->sp = top;
  if (val) {
    push();
  }
}

/* Are the call arguments simple enough for gen_call_assign (no splat,
   keyword hash, or forwarding that would obscure the RHS position)? */
static mrc_bool
attr_assign_simple_args(pm_call_node_t *cast)
{
  if (cast->arguments == NULL) return FALSE;
  pm_arguments_node_t *arguments = (pm_arguments_node_t *)cast->arguments;
  if (arguments->arguments.size == 0) return FALSE;
  for (size_t i = 0; i < arguments->arguments.size; i++) {
    int t = nint((mrc_node *)arguments->arguments.nodes[i]);
    if (t == PM_SPLAT_NODE || t == PM_KEYWORD_HASH_NODE ||
        t == PM_FORWARDING_ARGUMENTS_NODE) {
      return FALSE;
    }
  }
  return TRUE;
}

static void
gen_call(mrc_codegen_scope *s, mrc_node *tree, int val, int safe)
{
  CAST(call);
  const mrc_sym sym = cast->name;

  if (val && (cast->base.flags & PM_CALL_NODE_FLAGS_ATTRIBUTE_WRITE) &&
      attr_assign_simple_args(cast)) {
    gen_call_assign(s, tree, val, safe);
    return;
  }
  int skip = 0, n = 0, nk = 0, noop = no_optimize(s), noself = 0, blk = 0, sp_save = cursp();

#if defined(MRC_TARGET_MRUBY)
  if (cast->receiver == NULL && cast->arguments == NULL && cast->block == NULL) {
    int lv, idx;
    if (mrc_mruby_numbered_parameter_upvar(s, sym, &lv, &idx)) {
      if (val) {
        genop_3(s, OP_GETUPVAR, cursp(), idx, lv);
        push();
      }
      return;
    }
  }
#endif

  if (cast->receiver == NULL) {
    noself = noop = 1;
    push();
  }
  else if (nint(cast->receiver) == PM_SELF_NODE) {
    noself = noop = 1;
    push();
  }
  else {
    codegen(s, cast->receiver, VAL); /* receiver */
  }
  if (safe) {
    int recv = cursp()-1;
    gen_move(s, cursp(), recv, 1);
    skip = genjmp2_0(s, OP_JMPNIL, cursp(), val);
  }
  CAST3(arguments, cast->arguments, arguments);
  if (arguments) {
    if (0 < arguments->arguments.size) {            /* positional arguments */
      n = gen_values(s, (mrc_node *)arguments, VAL, 14);
      if (n < 0) {              /* variable length */
        noop = 1;               /* not operator */
        n = 15;
        push();
      }
    }
    for (size_t i = 0; i < arguments->arguments.size; i++) {
      mrc_node *t = (mrc_node *)arguments->arguments.nodes[i];
      if (nint(t) == PM_KEYWORD_HASH_NODE) {       /* keyword arguments */
        noop = 1;
        nk = gen_hash(s, t, VAL, 14);
        if (nk < 0) nk = 15;
      }
    }
  }
  if (cast->block) {
    codegen(s, cast->block, VAL);
    pop();
    noop = 1;
    blk = 1;
  }
  if (cast->arguments && cast->arguments->base.flags &PM_ARGUMENTS_NODE_FLAGS_CONTAINS_FORWARDING) {
    blk = 1;
    n = 0xFF;
  }
  push();pop();
  s->sp = sp_save;

  if (!noop && sym == MRC_OPSYM_2(add) && n == 1)  {
    gen_addsub(s, OP_ADD, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(sub) && n == 1)  {
    gen_addsub(s, OP_SUB, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(mul) && n == 1)  {
    gen_muldiv(s, OP_MUL, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(div) && n == 1)  {
    gen_muldiv(s, OP_DIV, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(lt) && n == 1)  {
    genop_1(s, OP_LT, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(le) && n == 1)  {
    genop_1(s, OP_LE, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(gt) && n == 1)  {
    genop_1(s, OP_GT, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(ge) && n == 1)  {
    genop_1(s, OP_GE, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(eq) && n == 1)  {
    genop_1(s, OP_EQ, cursp());
  }
  else if (!noop && sym == MRC_OPSYM_2(aset) && n == 2)  {
    genop_1(s, OP_SETIDX, cursp());
  }
  else if (!noop && n == 0 && gen_uniop(s, sym, cursp())) {
    /* constant folding succeeded */
  }
  else if (!noop && n == 1 && gen_binop(s, sym, cursp())) {
    /* constant folding succeeded */
  }
  else if (noself) {
    if (!blk && n == 0 && nk == 0) {
      genop_2(s, OP_SSEND0, cursp(), new_sym(s, sym));
    }
    else {
      genop_3(s, blk ? OP_SSENDB : OP_SSEND, cursp(), new_sym(s, sym), n|(nk<<4));
    }
  }
  else if (!blk && n == 0 && nk == 0) {
    genop_2(s, OP_SEND0, cursp(), new_sym(s, sym));
  }
  else {
    genop_3(s, blk ? OP_SENDB : OP_SEND, cursp(), new_sym(s, sym), n|(nk<<4));
  }
  if (safe) {
    dispatch(s, skip);
  }
  if (val) {
    push();
  }
}

static void
gen_massignment(mrc_codegen_scope *s, mrc_node *tree, int rhs, int val)
{
  CAST(multi_write);
  int n = cast->lefts.size, post = cast->rights.size;
  int has_rest = cast->rest && nint(cast->rest) != PM_IMPLICIT_REST_NODE;

  if (0 < n) { /* pre */
    for (int i = 0; i < n; i++) {
      int sp = cursp();
      genop_3(s, OP_AREF, sp, rhs, i);
      push();
      gen_assignment(s, cast->lefts.nodes[i], NULL, sp, NOVAL);
      pop();
    }
  }
  if (has_rest || 0 < post) {
    gen_move(s, cursp(), rhs, val);
    push_n(post+1);
    pop_n(post+1);
    genop_3(s, OP_APOST, cursp(), n, post);
    if (has_rest) { /* rest */
      pm_node_t *rest_expr = ((pm_splat_node_t *)cast->rest)->expression;
      if (rest_expr) {
        gen_assignment(s, rest_expr, NULL, cursp(), NOVAL);
      }
    }
    for (int i = 0; i < post; i++) {
      gen_assignment(s, cast->rights.nodes[i], NULL, cursp()+i+1, NOVAL);
    }
    if (val) {
      gen_move(s, cursp(), rhs, 0);
    }
  }
}

/* Generate pattern matching code for a single pattern.
 * target: stack position of the value being matched
 * fail_pos: linked list of jump positions for pattern match failure
 * known_array_len: -1 if unknown, >= 0 if target is known to be an array of that length
 */
static void
codegen_pattern(mrc_codegen_scope *s, mrc_node *pattern, int target, uint32_t *fail_pos, int known_array_len)
{
  uint32_t tmp;

  /* Handle guard clause wrapper (PM_IF_NODE wrapping the actual pattern) */
  if (nint(pattern) == PM_IF_NODE) {
    pm_if_node_t *if_n = (pm_if_node_t *)pattern;
    /* In Prism: in pattern if condition
     * The IfNode.statements contains the actual pattern
     * The IfNode.predicate contains the guard condition */
    if (if_n->statements) {
      pm_statements_node_t *stmts = if_n->statements;
      if (stmts->body.size > 0) {
        /* Extract and match the inner pattern first */
        mrc_node *inner_pattern = stmts->body.nodes[0];
        codegen_pattern(s, inner_pattern, target, fail_pos, known_array_len);
      }
    }
    /* Generate the guard condition */
    if (if_n->predicate) {
      codegen(s, (mrc_node *)if_n->predicate, VAL);
      pop();
      /* if guard: fail if guard is false */
      tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 0);
      *fail_pos = tmp;
    }
    return;
  }

  /* Handle unless guard clause (PM_UNLESS_NODE wrapping the actual pattern) */
  if (nint(pattern) == PM_UNLESS_NODE) {
    pm_unless_node_t *unless_n = (pm_unless_node_t *)pattern;
    /* In Prism: in pattern unless condition
     * The UnlessNode.statements contains the actual pattern
     * The UnlessNode.predicate contains the guard condition */
    if (unless_n->statements) {
      pm_statements_node_t *stmts = unless_n->statements;
      if (stmts->body.size > 0) {
        /* Extract and match the inner pattern first */
        mrc_node *inner_pattern = stmts->body.nodes[0];
        codegen_pattern(s, inner_pattern, target, fail_pos, known_array_len);
      }
    }
    /* Generate the guard condition */
    if (unless_n->predicate) {
      codegen(s, (mrc_node *)unless_n->predicate, VAL);
      pop();
      /* unless guard: fail if guard is true (inverted from if) */
      tmp = genjmp2(s, OP_JMPIF, cursp(), *fail_pos, 0);
      *fail_pos = tmp;
    }
    return;
  }

  switch (nint(pattern)) {
  /* Value patterns: literals */
  case PM_INTEGER_NODE:
  case PM_FLOAT_NODE:
  case PM_RATIONAL_NODE:
  case PM_IMAGINARY_NODE:
  case PM_STRING_NODE:
  case PM_INTERPOLATED_STRING_NODE:
  case PM_X_STRING_NODE:
  case PM_SYMBOL_NODE:
  case PM_INTERPOLATED_SYMBOL_NODE:
  case PM_REGULAR_EXPRESSION_NODE:
  case PM_INTERPOLATED_REGULAR_EXPRESSION_NODE:
  case PM_RANGE_NODE:
  case PM_TRUE_NODE:
  case PM_FALSE_NODE:
  case PM_NIL_NODE:
  case PM_CONSTANT_READ_NODE:
  case PM_CONSTANT_PATH_NODE:
    {
      /* Generate: pattern_value === target */
      codegen(s, pattern, VAL);
      gen_move(s, cursp(), target, 0);
      push(); push(); pop(); pop(); pop();
      genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_OPSYM_2(eqq)), 1);
      /* Jump to fail if not matched */
      tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 1);
      *fail_pos = tmp;
    }
    break;

  case PM_LOCAL_VARIABLE_TARGET_NODE:
    {
      CAST3(local_variable_target, pattern, var_target);
      /* Bind the matched value to the variable */
      int idx = lv_idx(s, var_target->name);
      if (idx > 0) {
        gen_move(s, idx, target, 1);  /* nopeep=1 to prevent optimization */
      }
      /* Variable pattern always matches */
    }
    break;

  case PM_IMPLICIT_NODE:
    {
      /* Unwrap implicit node and process inner value */
      pm_implicit_node_t *implicit = (pm_implicit_node_t *)pattern;
      codegen_pattern(s, (mrc_node *)implicit->value, target, fail_pos, known_array_len);
    }
    break;

  case PM_ALTERNATION_PATTERN_NODE:
    {
      CAST3(alternation_pattern, pattern, pat_alt);
      uint32_t left_fail = JMPLINK_START;
      uint32_t success_pos = JMPLINK_START;

      /* Try left pattern */
      codegen_pattern(s, (mrc_node *)pat_alt->left, target, &left_fail, known_array_len);

      /* Optimize JMPNOT+JMP to JMPIF when possible */
      if (nint(pat_alt->left) != PM_ALTERNATION_PATTERN_NODE &&
          left_fail != JMPLINK_START && left_fail >= 2 && left_fail + 2 == s->pc) {
        /* Extract the previous link from the JMPNOT chain */
        int16_t prev_offset = (int16_t)PEEK_S(s->iseq + left_fail);
        int32_t next_addr = (int32_t)(left_fail + 2) + prev_offset;
        uint32_t prev_link = (next_addr == 0) ? JMPLINK_START : (uint32_t)next_addr;
        /* Convert JMPNOT to JMPIF */
        s->iseq[left_fail - 2] = OP_JMPIF;
        /* Clear offset to mark end of success chain */
        emit_S(s, left_fail, 0);
        success_pos = left_fail;
        /* Continue with remaining fail chain */
        left_fail = prev_link;
      }
      else {
        /* Left succeeded - jump to success */
        tmp = genjmp(s, OP_JMP, success_pos);
        success_pos = tmp;
      }

      /* Left failed - try right pattern */
      if (left_fail != JMPLINK_START) {
        dispatch_linked(s, left_fail);
      }
      codegen_pattern(s, (mrc_node *)pat_alt->right, target, fail_pos, known_array_len);

      /* Dispatch success jumps */
      if (success_pos != JMPLINK_START) {
        dispatch_linked(s, success_pos);
      }
    }
    break;

  case PM_CAPTURE_PATTERN_NODE:
    {
      CAST3(capture_pattern, pattern, pat_as);
      /* First match the inner pattern */
      codegen_pattern(s, (mrc_node *)pat_as->value, target, fail_pos, known_array_len);
      /* Then bind the value to the variable */
      CAST3(local_variable_target, pat_as->target, var_target);
      int idx = lv_idx(s, var_target->name);
      if (idx > 0) {
        gen_move(s, idx, target, 0);
      }
    }
    break;

  case PM_PINNED_VARIABLE_NODE:
    {
      CAST3(pinned_variable, pattern, pat_pin);
      /* Get the variable based on its type */
      mrc_node *var_node = (mrc_node *)pat_pin->variable;
      mrc_sym var_name = 0;

      if (nint(var_node) == PM_LOCAL_VARIABLE_READ_NODE) {
        pm_local_variable_read_node_t *lvar = (pm_local_variable_read_node_t *)var_node;
        var_name = lvar->name;
      }

      if (var_name) {
        int idx = lv_idx(s, var_name);
        if (idx > 0) {
          /* Compare: pinned_value === target */
          gen_move(s, cursp(), idx, 0);  /* Load pinned variable */
          push();
          gen_move(s, cursp(), target, 0);  /* Load target */
          push(); push(); pop(); pop(); pop();
          genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_OPSYM_2(eqq)), 1);
          /* Jump to fail if not matched */
          tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 1);
          *fail_pos = tmp;
        }
        else {
          /* Variable not found - fail the match */
          tmp = genjmp(s, OP_JMP, *fail_pos);
          *fail_pos = tmp;
        }
      }
      else {
        /* Unable to extract variable name - fail the match */
        tmp = genjmp(s, OP_JMP, *fail_pos);
        *fail_pos = tmp;
      }
    }
    break;

  case PM_ARRAY_PATTERN_NODE:
    {
      CAST3(array_pattern, pattern, pat_arr);
      int pre_len = pat_arr->requireds.size;
      int post_len = pat_arr->posts.size;
      int arr_reg;
      int i;

      /* Optimization: if we know the target is an array, skip deconstruct */
      if (known_array_len >= 0) {
        /* Use target directly as array register */
        arr_reg = target;

        /* Compile-time size check */
        if (pat_arr->rest == NULL) {
          /* No rest: exact length match required */
          if (known_array_len != pre_len + post_len) {
            /* Size mismatch - always fail */
            tmp = genjmp(s, OP_JMP, *fail_pos);
            *fail_pos = tmp;
            break;
          }
          /* Size matches, no runtime check needed */
        }
        else {
          /* Has rest: minimum length check */
          int min_len = pre_len + post_len;
          if (known_array_len < min_len) {
            /* Size too small - always fail */
            tmp = genjmp(s, OP_JMP, *fail_pos);
            *fail_pos = tmp;
            break;
          }
          /* Size sufficient, no runtime check needed */
        }

        /* Match pre-rest elements using AREF */
        for (i = 0; i < pre_len; i++) {
          /* Get arr[i] using AREF */
          int sp = cursp();
          genop_3(s, OP_AREF, sp, arr_reg, i);
          push();
          /* Element is now at sp */
          /* Match element pattern (elements are not known arrays) */
          codegen_pattern(s, pat_arr->requireds.nodes[i], sp, fail_pos, -1);
          pop();
        }

        /* Bind rest elements if rest is a variable */
        if (pat_arr->rest && nint(pat_arr->rest) == PM_SPLAT_NODE) {
          pm_splat_node_t *splat = (pm_splat_node_t *)pat_arr->rest;
          if (splat->expression && nint(splat->expression) == PM_LOCAL_VARIABLE_TARGET_NODE) {
            pm_local_variable_target_node_t *rest_var = (pm_local_variable_target_node_t *)splat->expression;
            int var_idx = lv_idx(s, rest_var->name);
            /* Generate: arr[pre_len..-(post_len+1)] or arr[pre_len..-1] if no post */
            int sp_save = cursp();
            gen_move(s, cursp(), arr_reg, 0);
            push();
            gen_int(s, cursp(), pre_len);
            push();
            if (post_len > 0) {
              gen_int(s, cursp(), -(post_len + 1));
            }
            else {
              gen_int(s, cursp(), -1);
            }
            /* Create inclusive range */
            genop_1(s, OP_RANGE_INC, cursp() - 1);
            push(); pop();  /* touch block slot */
            s->sp = sp_save;
            genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_OPSYM_2(aref)), 1);
            if (var_idx > 0) {
              gen_move(s, var_idx, cursp(), 1);
            }
          }
        }

        /* Match post-rest elements using negative indices (GETIDX) */
        for (i = 0; i < post_len; i++) {
          gen_move(s, cursp(), arr_reg, 0);
          push();
          gen_int(s, cursp(), -(post_len - i));
          genop_1(s, OP_GETIDX, cursp() - 1);
          /* Element is now at cursp-1 */
          codegen_pattern(s, pat_arr->posts.nodes[i], cursp() - 1, fail_pos, -1);
          pop();
        }
      }
      else {
        /* Call target.deconstruct() */
        gen_move(s, cursp(), target, 0);
        push(); pop();  /* touch block slot for max stack */
        genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_SYM_1(deconstruct)), 0);
        arr_reg = cursp();
        push();  /* protect arr_reg on stack */

        /* Check if deconstruct returned nil */
        tmp = genjmp2(s, OP_JMPNIL, arr_reg, *fail_pos, 0);
        *fail_pos = tmp;

        /* Runtime size check: arr.size() == or >= expected */
        {
          int chk = cursp();
          gen_move(s, chk, arr_reg, 0);
          push(); pop();  /* touch block slot */
          genop_3(s, OP_SEND, chk, new_sym(s, MRC_SYM_1(size)), 0);
          /* R[chk] = size */
          gen_int(s, chk + 1, pre_len + post_len);
          if (pat_arr->rest == NULL) {
            genop_1(s, OP_EQ, chk);
          }
          else {
            genop_1(s, OP_GE, chk);
          }
          tmp = genjmp2(s, OP_JMPNOT, chk, *fail_pos, 1);
          *fail_pos = tmp;
        }

        /* Match pre-rest elements */
        for (i = 0; i < pre_len; i++) {
          int sp = cursp();
          genop_3(s, OP_AREF, sp, arr_reg, i);
          push();
          codegen_pattern(s, pat_arr->requireds.nodes[i], sp, fail_pos, -1);
          pop();
        }

        /* Bind rest elements if rest is a variable */
        if (pat_arr->rest && nint(pat_arr->rest) == PM_SPLAT_NODE) {
          pm_splat_node_t *splat = (pm_splat_node_t *)pat_arr->rest;
          if (splat->expression && nint(splat->expression) == PM_LOCAL_VARIABLE_TARGET_NODE) {
            pm_local_variable_target_node_t *rest_var = (pm_local_variable_target_node_t *)splat->expression;
            int var_idx = lv_idx(s, rest_var->name);
            int sp_save = cursp();
            gen_move(s, cursp(), arr_reg, 0);
            push();
            gen_int(s, cursp(), pre_len);
            push();
            if (post_len > 0) {
              gen_int(s, cursp(), -(post_len + 1));
            }
            else {
              gen_int(s, cursp(), -1);
            }
            genop_1(s, OP_RANGE_INC, cursp() - 1);
            push(); pop();  /* touch block slot */
            s->sp = sp_save;
            genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_OPSYM_2(aref)), 1);
            /* Result at R[sp_save] */
            if (var_idx > 0) {
              gen_move(s, var_idx, cursp(), 1);
            }
          }
        }

        /* Match post-rest elements using negative indices (GETIDX) */
        for (i = 0; i < post_len; i++) {
          gen_move(s, cursp(), arr_reg, 0);
          push();
          gen_int(s, cursp(), -(post_len - i));
          genop_1(s, OP_GETIDX, cursp() - 1);
          codegen_pattern(s, pat_arr->posts.nodes[i], cursp() - 1, fail_pos, -1);
          pop();
        }
        pop();  /* release arr_reg */
      }
    }
    break;

  case PM_HASH_PATTERN_NODE:
    {
      CAST3(hash_pattern, pattern, pat_hash);
      int hash_reg;
      int num_keys = 0;

      /* Count regular (non-rest) keys */
      for (size_t i = 0; i < pat_hash->elements.size; i++) {
        if (nint(pat_hash->elements.nodes[i]) == PM_ASSOC_NODE) num_keys++;
      }

      int has_rest = pat_hash->rest != NULL;
      int has_double_nil = has_rest && nint(pat_hash->rest) == PM_NO_KEYWORDS_PARAMETER_NODE;

      /* Call target.deconstruct_keys(keys_array or nil).
       * Pass keys_array only when no rest pattern (partial-match optimization).
       * Pass nil when any ** is present so deconstruct_keys returns all keys. */
      hash_reg = cursp();
      gen_move(s, hash_reg, target, 0);
      push(); /* protect receiver */

      if (!has_rest && num_keys > 0) {
        int keys_base = cursp();
        for (size_t i = 0; i < pat_hash->elements.size; i++) {
          mrc_node *elem = pat_hash->elements.nodes[i];
          if (nint(elem) == PM_ASSOC_NODE) {
            pm_assoc_node_t *assoc = (pm_assoc_node_t *)elem;
            codegen(s, (mrc_node *)assoc->key, VAL);
          }
        }
        pop_n(num_keys);
        genop_2(s, OP_ARRAY, keys_base, num_keys);
        push(); /* protect keys arg */
        push(); pop(); /* touch block slot */
        s->sp = hash_reg;
        genop_3(s, OP_SEND, hash_reg, new_sym(s, MRC_SYM_1(deconstruct_keys)), 1);
      }
      else {
        genop_1(s, OP_LOADNIL, cursp());
        push(); /* protect nil arg */
        push(); pop(); /* touch block slot */
        s->sp = hash_reg;
        genop_3(s, OP_SEND, hash_reg, new_sym(s, MRC_SYM_1(deconstruct_keys)), 1);
      }
      push(); /* protect hash_reg */

      /* Fail if deconstruct_keys returned nil */
      tmp = genjmp2(s, OP_JMPNIL, hash_reg, *fail_pos, 0);
      *fail_pos = tmp;

      /* Check all keys exist and get values via __pat_values.
       * __pat_values(keys_array) returns an array of values in key order,
       * or false if any key is missing. */
      if (num_keys > 0) {
        int vals_reg = cursp();
        gen_move(s, vals_reg, hash_reg, 0);
        push(); /* protect receiver */

        int keys_base = cursp();
        for (size_t i = 0; i < pat_hash->elements.size; i++) {
          mrc_node *elem = pat_hash->elements.nodes[i];
          if (nint(elem) == PM_ASSOC_NODE) {
            pm_assoc_node_t *assoc = (pm_assoc_node_t *)elem;
            codegen(s, (mrc_node *)assoc->key, VAL);
          }
        }
        pop_n(num_keys);
        genop_2(s, OP_ARRAY, keys_base, num_keys);
        push(); /* protect keys arg */
        push(); pop(); /* touch block slot */
        s->sp = vals_reg;
        genop_3(s, OP_SEND, vals_reg, new_sym(s, MRC_SYM_1(__pat_values)), 1);
        push(); /* protect vals_reg */

        /* __pat_values returns false when a key is missing */
        tmp = genjmp2(s, OP_JMPNOT, vals_reg, *fail_pos, 1);
        *fail_pos = tmp;

        /* Match each value against its sub-pattern using vals_reg[i] */
        int loop_sp = cursp();
        int key_idx = 0;
        for (size_t i = 0; i < pat_hash->elements.size; i++) {
          mrc_node *elem = pat_hash->elements.nodes[i];
          if (nint(elem) == PM_ASSOC_NODE) {
            pm_assoc_node_t *assoc = (pm_assoc_node_t *)elem;

            int val_reg = cursp();
            gen_move(s, val_reg, vals_reg, 0);
            push(); /* protect receiver */
            gen_int(s, cursp(), key_idx);
            push(); pop(); /* touch block slot */
            s->sp = val_reg;
            genop_3(s, OP_SEND, val_reg, new_sym(s, MRC_OPSYM_2(aref)), 1);

            if (assoc->value) {
              codegen_pattern(s, (mrc_node *)assoc->value, val_reg, fail_pos, -1);
            }
            else {
              /* Shorthand form {a:} - bind to variable with same name as key */
              if (nint(assoc->key) == PM_SYMBOL_NODE) {
                pm_symbol_node_t *sym_node = (pm_symbol_node_t *)assoc->key;
                mrc_sym var_name = nsym(s->c->p, sym_node->unescaped.source, sym_node->unescaped.length);
                int idx = lv_idx(s, var_name);
                if (idx > 0) {
                  gen_move(s, idx, val_reg, 1);
                }
              }
            }
            s->sp = loop_sp;
            key_idx++;
          }
        }
        pop(); /* release vals_reg */
      }

      /* Handle rest pattern */
      if (has_double_nil || (!has_rest && num_keys == 0)) {
        /* **nil or empty {}: exact match - verify hash.size == num_keys */
        int chk = cursp();
        gen_move(s, chk, hash_reg, 0);
        push(); pop(); /* touch block slot */
        genop_3(s, OP_SEND, chk, new_sym(s, MRC_SYM_1(size)), 0);
        gen_int(s, chk + 1, num_keys);
        genop_1(s, OP_EQ, chk);
        tmp = genjmp2(s, OP_JMPNOT, chk, *fail_pos, 1);
        *fail_pos = tmp;
      }
      else if (has_rest && !has_double_nil) {
        if (nint(pat_hash->rest) == PM_ASSOC_SPLAT_NODE) {
          pm_assoc_splat_node_t *splat = (pm_assoc_splat_node_t *)pat_hash->rest;
          /* Named **var: capture remaining keys via hash.__except(keys_array) */
          if (splat->value && nint(splat->value) == PM_LOCAL_VARIABLE_TARGET_NODE) {
            pm_local_variable_target_node_t *rest_var = (pm_local_variable_target_node_t *)splat->value;
            int var_idx = lv_idx(s, rest_var->name);
            if (var_idx > 0) {
              int recv = cursp();
              gen_move(s, recv, hash_reg, 0);
              push(); /* protect receiver */
              if (num_keys > 0) {
                int keys_base = cursp();
                for (size_t i = 0; i < pat_hash->elements.size; i++) {
                  mrc_node *elem = pat_hash->elements.nodes[i];
                  if (nint(elem) == PM_ASSOC_NODE) {
                    pm_assoc_node_t *assoc = (pm_assoc_node_t *)elem;
                    codegen(s, (mrc_node *)assoc->key, VAL);
                  }
                }
                pop_n(num_keys);
                genop_2(s, OP_ARRAY, keys_base, num_keys);
                push(); /* protect keys arg */
                push(); pop(); /* touch block slot */
                s->sp = recv;
                genop_3(s, OP_SEND, recv, new_sym(s, MRC_SYM_1(__except)), 1);
              }
              else {
                push(); pop(); /* touch block slot */
                s->sp = recv;
                genop_3(s, OP_SEND, recv, new_sym(s, MRC_SYM_1(dup)), 0);
              }
              gen_move(s, var_idx, recv, 1);
              pop(); /* release recv */
            }
          }
          /* Anonymous **: do nothing */
        }
      }

      pop(); /* release hash_reg */
    }
    break;

  case PM_FIND_PATTERN_NODE:
    {
      /* Find pattern: [*pre, elem1, elem2, ..., *post]
       * Searches for elems anywhere in the array. */
      CAST3(find_pattern, pattern, pat_find);
      int elems_len = pat_find->requireds.size;
      int arr_reg = cursp();
      int idx_reg;
      uint32_t loop_start, match_fail, loop_end;

      /* Call deconstruct on target */
      gen_move(s, cursp(), target, 0);
      push(); pop();
      genop_3(s, OP_SEND, arr_reg, new_sym(s, MRC_SYM_1(deconstruct)), 0);
      push(); /* protect arr_reg */

      /* Check if deconstruct returned nil */
      tmp = genjmp2(s, OP_JMPNIL, arr_reg, *fail_pos, 0);
      *fail_pos = tmp;

      /* Check minimum length: arr.size >= elems_len */
      gen_move(s, cursp(), arr_reg, 0);
      push(); pop();
      genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_SYM_1(size)), 0);
      gen_int(s, cursp() + 1, elems_len);
      genop_1(s, OP_GE, cursp());
      tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 1);
      *fail_pos = tmp;

      /* Initialize index to 0 */
      idx_reg = cursp();
      gen_int(s, idx_reg, 0);
      push();

      /* Loop: try matching at each position */
      loop_start = new_label(s);
      match_fail = JMPLINK_START;

      /* Check if idx <= arr.size - elems_len */
      gen_move(s, cursp(), arr_reg, 0);
      push(); pop();
      genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_SYM_1(size)), 0);
      gen_int(s, cursp() + 1, elems_len);
      genop_1(s, OP_SUB, cursp());
      gen_move(s, cursp() + 1, idx_reg, 0);
      genop_1(s, OP_GE, cursp());
      tmp = genjmp2(s, OP_JMPNOT, cursp(), *fail_pos, 1);
      *fail_pos = tmp;

      /* Try to match each middle element at idx+offset */
      for (size_t i = 0; i < pat_find->requireds.size; i++) {
        /* Get arr[idx + offset] */
        gen_move(s, cursp(), arr_reg, 0);
        push();
        if (i == 0) {
          gen_move(s, cursp(), idx_reg, 0);
        }
        else {
          gen_move(s, cursp(), idx_reg, 0);
          gen_int(s, cursp() + 1, (int)i);
          genop_1(s, OP_ADD, cursp());
        }
        genop_1(s, OP_GETIDX, cursp() - 1);
        int elem_reg = cursp() - 1;
        codegen_pattern(s, pat_find->requireds.nodes[i], elem_reg, &match_fail, -1);
        pop();
      }

      /* All elements matched - bind pre and post if named */
      if (pat_find->left && nint((mrc_node *)pat_find->left) == PM_SPLAT_NODE) {
        pm_splat_node_t *pre_splat = (pm_splat_node_t *)pat_find->left;
        if (pre_splat->expression &&
            nint(pre_splat->expression) == PM_LOCAL_VARIABLE_TARGET_NODE) {
          pm_local_variable_target_node_t *pre_var =
              (pm_local_variable_target_node_t *)pre_splat->expression;
          int var_idx = lv_idx(s, pre_var->name);
          if (var_idx > 0) {
            /* pre = arr[0...idx] */
            gen_move(s, cursp(), arr_reg, 0);
            push();
            gen_int(s, cursp(), 0);
            push();
            gen_move(s, cursp(), idx_reg, 0);
            genop_1(s, OP_RANGE_EXC, cursp() - 1);
            pop(); pop();
            genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_OPSYM_2(aref)), 1);
            gen_move(s, var_idx, cursp(), 1);
          }
        }
      }

      if (pat_find->right && nint((mrc_node *)pat_find->right) == PM_SPLAT_NODE) {
        pm_splat_node_t *post_splat = (pm_splat_node_t *)pat_find->right;
        if (post_splat->expression &&
            nint(post_splat->expression) == PM_LOCAL_VARIABLE_TARGET_NODE) {
          pm_local_variable_target_node_t *post_var =
              (pm_local_variable_target_node_t *)post_splat->expression;
          int var_idx = lv_idx(s, post_var->name);
          if (var_idx > 0) {
            /* post = arr[(idx+elems_len)..-1] */
            gen_move(s, cursp(), arr_reg, 0);
            push();
            gen_move(s, cursp(), idx_reg, 0);
            gen_int(s, cursp() + 1, elems_len);
            genop_1(s, OP_ADD, cursp());
            push();
            gen_int(s, cursp(), -1);
            genop_1(s, OP_RANGE_INC, cursp() - 1);
            pop(); pop();
            genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_OPSYM_2(aref)), 1);
            gen_move(s, var_idx, cursp(), 1);
          }
        }
      }

      /* Match succeeded - jump to end */
      tmp = genjmp(s, OP_JMP, JMPLINK_START);
      loop_end = tmp;

      /* Match failed at this position - try next index */
      if (match_fail != JMPLINK_START) {
        dispatch_linked(s, match_fail);
      }

      /* idx++ */
      genop_2(s, OP_ADDI, idx_reg, 1);

      /* Jump back to loop start */
      genjmp(s, OP_JMP, loop_start);

      /* Dispatch loop_end jump */
      dispatch(s, loop_end);

      /* Clean up stack */
      pop(); /* idx_reg */
      pop(); /* arr_reg */
    }
    break;

  default:
    /* Unimplemented pattern type - for now, always fail */
    tmp = genjmp(s, OP_JMP, *fail_pos);
    *fail_pos = tmp;
    break;
  }
}

static void
for_body(mrc_codegen_scope *s, mrc_node *tree)
{
  mrc_codegen_scope *prev = s;
  int idx;
  struct loopinfo *lp;
  mrc_node *n2;

  CAST(for);

  /* generate receiver */
  codegen(s, (mrc_node *)cast->collection, VAL);
  /* generate loop-block */
  s = scope_new(s->c, s, NULL);
  s->for_depth = prev->for_depth + 1;

  push();                       /* push for a block parameter */

  /* generate loop variable */
  n2 = cast->index;
  genop_W(s, OP_ENTER, 0x40000);
  if (nint(n2) == PM_MULTI_TARGET_NODE) {
    gen_massignment(s, n2, 1, VAL);
  }
  else {
    gen_assignment(s, n2, NULL, 1, NOVAL);
  }
  /* construct loop */
  lp = loop_push(s, LOOP_FOR);
  lp->pc1 = new_label(s);
  genop_0(s, OP_NOP); /* for redo */

  /* loop body */
  codegen(s, (mrc_node *)cast->statements, VAL);
  pop();
  gen_return(s, OP_RETURN, cursp());
  loop_pop(s, NOVAL);
  scope_finish(s);
  s = prev;
  genop_2(s, OP_BLOCK, cursp(), s->irep->rlen-1);
  push();pop(); /* space for a block */
  pop();
  idx = new_sym(s, MRC_SYM_1(each));
  genop_3(s, OP_SENDB, cursp(), idx, 0);
}

static void
mrc_constant_id_list_init_capacity(mrc_codegen_scope *s, pm_constant_id_list_t *list, size_t capacity)
{
  list->ids = (pm_constant_id_t *)codegen_palloc(s, capacity * sizeof(pm_constant_id_t));
  if (list->ids == NULL) codegen_error(s, "memory allocation error");
  list->size = 0;
  list->capacity = capacity;
}

static mrc_bool
mrc_constant_id_list_append(mrc_codegen_scope *s, pm_constant_id_list_t *list, pm_constant_id_t id)
{
  if (list->size >= list->capacity) {
    size_t oldlen = sizeof(pm_constant_id_t) * list->capacity;
    list->capacity = list->capacity == 0 ? 8 : list->capacity * 2;
    size_t newlen = sizeof(pm_constant_id_t) * list->capacity;
    list->ids = (pm_constant_id_t *)codegen_realloc(s, list->ids, oldlen, newlen);
    if (list->ids == NULL) return FALSE;
  }
  list->ids[list->size++] = id;
  return TRUE;
}

static int
lambda_body(mrc_codegen_scope *s, mrc_node *tree, mrc_node *body, pm_constant_id_list_t *locals, int blk)
{
  mrc_codegen_scope *parent = s;
  pm_parameters_node_t *parameters = NULL;
  int na = 0;
  if (tree) {
    switch nint(tree) {
      case PM_DEF_NODE:
        parameters = ((pm_def_node_t *)tree)->parameters;
        break;
      case PM_BLOCK_PARAMETERS_NODE:
        parameters = ((pm_block_parameters_node_t *)tree)->parameters;
        break;
      case PM_NUMBERED_PARAMETERS_NODE:
        parameters = NULL;
        na = ((pm_numbered_parameters_node_t *)tree)->maximum;
        break;
      case PM_IT_PARAMETERS_NODE:
        parameters = NULL;
        na = 1;
        break;
      default:
        codegen_error(s, "should not happen");
    }
  }
  else {
    parameters = NULL;
  }


  size_t i, ma, mma, oa, ra, pa, ppa, ka, kd, ba, forwarding;;
  forwarding = 0;
  int block_reg = 0;
  pm_constant_id_list_t *lv = (pm_constant_id_list_t *)codegen_palloc(s, sizeof(pm_constant_id_list_t));
  pm_constant_id_t null_mark = pm_constant_pool_insert_constant(&s->c->p->constant_pool, NULL, 0);

  // Create lv regs from Prism's locals
  if (parameters == NULL) {
    mrc_constant_id_list_init_capacity(s, lv, locals->size + 1);
    if (!tree || nint(tree) != PM_NUMBERED_PARAMETERS_NODE) {
      /* empty block or `it`: insert null_mark as a placeholder for slot 1 */
      mrc_constant_id_list_append(s, lv, null_mark);
    }
    for (i = 0; i < locals->size; i++) {
      mrc_constant_id_list_append(s, lv, locals->ids[i]);
    }
    ma = mma = oa = ra = pa = ppa = ka = kd = ba = 0;
  }
  else {
    int nregs;
    /* mandatory arguments */
    ma = parameters->requireds.size;
    mma = 0;
    for (i = 0; i < ma; i++) {
      if (nint(parameters->requireds.nodes[i]) == PM_MULTI_TARGET_NODE) {
        CAST3(multi_target, parameters->requireds.nodes[i], m);
        mma += m->lefts.size;
      }
    }
    oa = parameters->optionals.size;
    ra = parameters->rest ? 1 : 0;
    pa = parameters->posts.size;
    ppa = 0;
    for (i = 0; i < pa; i++) {
      if (nint(parameters->posts.nodes[i]) == PM_MULTI_TARGET_NODE) {
        CAST3(multi_target, parameters->posts.nodes[i], m);
        ppa += m->lefts.size;
      }
    }
    ka = parameters->keywords.size;
    kd = parameters->keyword_rest ? 1 : 0;
    ba = parameters->block ? 1 : 0;
    nregs = ma + mma + oa + ra + pa + ppa + ka + kd + ba;
    mrc_constant_id_list_init_capacity(s, lv, nregs);
    // mandatory
    for (i = 0; i < ma; i++) {
      if (nint(parameters->requireds.nodes[i]) == PM_MULTI_TARGET_NODE) {
        mrc_constant_id_list_append(s, lv, null_mark);
      } else {
        mrc_constant_id_list_append(s, lv, ((pm_required_parameter_node_t *)parameters->requireds.nodes[i])->name);
      }
    }
    // optional
    for (i = 0; i < oa; i++) {
      mrc_constant_id_list_append(s, lv, ((pm_optional_parameter_node_t *)parameters->optionals.nodes[i])->name);
    }
    // rest
    if (ra) {
      if (((pm_rest_parameter_node_t *)parameters->rest)->name) {
        mrc_constant_id_list_append(s, lv, ((pm_rest_parameter_node_t *)parameters->rest)->name);
      } else {
        pm_constant_id_t astr = MRC_OPSYM_2(mul);
        mrc_constant_id_list_append(s, lv, astr);
      }
    }
    // post
    for (i = 0; i < pa; i++) {
      if (nint(parameters->posts.nodes[i]) == PM_MULTI_TARGET_NODE) {
        mrc_constant_id_list_append(s, lv, null_mark);
      } else {
        mrc_constant_id_list_append(s, lv, ((pm_required_parameter_node_t *)parameters->posts.nodes[i])->name);
      }
    }
    // keywords and block
    if (ka || kd || ba) {
      // keyword rest
      mrc_bool write_dastr = false;
      if (ka || kd) {
        write_dastr = true;
      }
      if (kd) {
        switch (nint(parameters->keyword_rest)) {
          case PM_KEYWORD_REST_PARAMETER_NODE: {
            if (((pm_keyword_rest_parameter_node_t *)parameters->keyword_rest)->name) {
              mrc_constant_id_list_append(s, lv, ((pm_keyword_rest_parameter_node_t *)parameters->keyword_rest)->name);
              write_dastr = false;
            } else {
              pm_constant_id_t dastr = MRC_OPSYM_2(pow);
              mrc_constant_id_list_append(s, lv, dastr);
              write_dastr = false;
            }
            break;
          }
          case PM_FORWARDING_PARAMETER_NODE: {
            forwarding = 1;
            write_dastr = false;
            pm_constant_id_t astr = MRC_OPSYM_2(mul);
            mrc_constant_id_list_append(s, lv, astr);
            pm_constant_id_t dastr = MRC_OPSYM_2(pow);
            mrc_constant_id_list_append(s, lv, dastr);
            mrc_constant_id_list_append(s, lv, null_mark);
            pm_constant_id_t and = MRC_OPSYM_2(and);
            mrc_constant_id_list_append(s, lv, and);
            block_reg = lv->size;
            break;
          }
          default:
            codegen_error(s, "Unknown node");
        }
      }
      if (write_dastr) {
        pm_constant_id_t dastr = MRC_OPSYM_2(pow);
        mrc_constant_id_list_append(s, lv, dastr);
      }
    }
    if (forwarding == 0) {
      mrc_constant_id_list_append(s, lv, null_mark);
    }
    // block
    if (ba) {
      if (((pm_block_parameter_node_t *)parameters->block)->name) {
        mrc_constant_id_list_append(s, lv, ((pm_block_parameter_node_t *)parameters->block)->name);
      }
      else {
        pm_constant_id_t and = MRC_OPSYM_2(and);
        mrc_constant_id_list_append(s, lv, and);
      }
      block_reg = lv->size;
    }
    // keywords
    for (i = 0; i < ka; i++) {
      if (nint(parameters->keywords.nodes[i]) == PM_REQUIRED_KEYWORD_PARAMETER_NODE) {
        mrc_constant_id_list_append(s, lv, ((pm_required_keyword_parameter_node_t *)parameters->keywords.nodes[i])->name);
      }
      else {
        mrc_constant_id_list_append(s, lv, ((pm_optional_keyword_parameter_node_t *)parameters->keywords.nodes[i])->name);
      }
    }
    for (i = 0; i < ma; i++) {
      if (nint(parameters->requireds.nodes[i]) == PM_MULTI_TARGET_NODE) {
        CAST3(multi_target, parameters->requireds.nodes[i], m);
        for (size_t j = 0; j < m->lefts.size; j++) {
          mrc_constant_id_list_append(s, lv, ((pm_required_parameter_node_t *)m->lefts.nodes[j])->name);
        }
      }
    }
    for (i = 0; i < pa; i++) {
      if (nint(parameters->posts.nodes[i]) == PM_MULTI_TARGET_NODE) {
        CAST3(multi_target, parameters->posts.nodes[i], m);
        for (size_t j = 0; j < m->lefts.size; j++) {
          mrc_constant_id_list_append(s, lv, ((pm_required_parameter_node_t *)m->lefts.nodes[j])->name);
        }
      }
    }
  }
  if (locals) {
    for (i = 0; i < locals->size; i++) {
      if (!pm_constant_id_list_includes(lv, locals->ids[i])) {
        mrc_constant_id_list_append(s, lv, locals->ids[i]);
      }
    }
    // free Prism's locals
    pm_constant_id_list_free(locals);
    locals->ids = NULL;
    locals->size = locals->capacity = 0;
  }

  s = scope_new(s->c, s, lv);

  s->mscope = !blk;
  if (blk) {
    s->for_depth = s->prev->for_depth; /* inherit for-depth from enclosing scope */
    struct loopinfo *lp = loop_push(s, LOOP_BLOCK);
    lp->pc0 = new_label(s);
  }
  if (parameters == NULL) { /* empty parameter OR numbered parameters */
    genop_W(s, OP_ENTER, MRC_ARGS_REQ(na));
    s->ainfo = (na & 0x3f) << 7;
  }
  else {
    mrc_aspec a;
    uint32_t pos;
    mrc_node **margs, **pargs;

    /* mandatory arguments */
    margs = parameters->requireds.nodes;
    /* mandatory arguments after rest argument */
    pargs = parameters->posts.nodes;

    if (ma > 0x1f || oa > 0x1f || pa > 0x1f || ka > 0x1f) {
      codegen_error(s, "too many formal arguments");
    }
    /* (23bits = 5:5:1:5:5:1:1) */
    ra = ra|forwarding;
    ba = ba|forwarding;
    a = MRC_ARGS_REQ(ma)
      | MRC_ARGS_OPT(oa)
      | (ra? MRC_ARGS_REST() : 0)
      | MRC_ARGS_POST(pa)
      | MRC_ARGS_KEY(ka, kd)
      | (ba? MRC_ARGS_BLOCK() : 0);
    genop_W(s, OP_ENTER, a);
    /* (12bits = 5:1:5:1) */
    s->ainfo = (((ma+oa) & 0x3f) << 7)
      | ((ra & 0x1) << 6)
      | ((pa & 0x1f) << 1)
      | ((ka | kd) ? 1 : 0);
    /* generate jump table for optional arguments initializer */
    pos = new_label(s);
    for (i=0; i<oa; i++) {
      new_label(s);
      genjmp_0(s, OP_JMP);
    }
    if (oa > 0) {
      genjmp_0(s, OP_JMP);
    }
    for (i = 0; i < oa; i++) {
      CAST3(optional_parameter, parameters->optionals.nodes[i], opt);
      int idx;
      mrc_sym id = opt->name;

      dispatch(s, pos+i*3+1);
      codegen(s, opt->value, VAL);
      pop();
      idx = lv_idx(s, id);
      if (idx > 0) {
        gen_move(s, idx, cursp(), 0);
      }
      else {
        mrc_assert(0 && "should not happen: keyword parameter must be local variable");
      }
    }
    if (oa > 0) {
      dispatch(s, pos+i*3+1);
    }

    /* keyword arguments */
    if (ka) {
      for (i = 0; i < ka; i++) {
        int jmpif_key_p, jmp_def_set = -1;
        CAST3(required_keyword_parameter, parameters->keywords.nodes[i], kwd);
        mrc_sym kwd_sym = kwd->name;

        if (nint((mrc_node *)kwd) == PM_OPTIONAL_KEYWORD_PARAMETER_NODE) {
          int idx;
          genop_2(s, OP_KEY_P, lv_idx(s, kwd_sym), new_sym(s, kwd_sym));
          jmpif_key_p = genjmp2_0(s, OP_JMPIF, lv_idx(s, kwd_sym), NOVAL);
          codegen(s, ((pm_optional_keyword_parameter_node_t *)kwd)->value, VAL);
          pop();
          idx = lv_idx(s, kwd_sym);
          if (idx > 0) {
            gen_move(s, idx, cursp(), 0);
          }
          else {
            mrc_assert(0 && "should not happen: keyword parameter must be local variable");
          }
          jmp_def_set = genjmp_0(s, OP_JMP);
          dispatch(s, jmpif_key_p);
        }
        genop_2(s, OP_KARG, lv_idx(s, kwd_sym), new_sym(s, kwd_sym));
        if (jmp_def_set != -1) {
          dispatch(s, jmp_def_set);
        }
      }
      if (!kd) {
        genop_0(s, OP_KEYEND);
      }
    }

    /* block argument */
    if (block_reg) {
      gen_move(s, block_reg, block_reg-1, 0);
    }

    /* argument destructuring */
    if (margs) {
      pos = 1;
      for (i = 0; i < ma; i++) {
        if (nint(margs[i]) == PM_MULTI_TARGET_NODE) {
          gen_massignment(s, margs[i], pos, NOVAL);
          // Enabling the following three lines would generate VM code equivalent to mruby-compiler,
          // but it would not work in mruby/c.
          // It appears to work correctly even when commented out, so it is left commented out
          uint16_t n = ((pm_multi_target_node_t *)margs[i])->lefts.size;
          gen_move(s, cursp(), pos, 0);
          genop_3(s, OP_APOST, cursp(), n, 0);
        }
        pos++;
      }
    }
    if (pargs) {
      pos = ma+oa+ra+1;
      for (i = 0; i < pa; i++) {
        if (nint(pargs[i]) == PM_MULTI_TARGET_NODE) {
          gen_massignment(s, pargs[i], pos, NOVAL);
          // Enabling the following three lines would generate VM code equivalent to mruby-compiler,
          // but it would not work in mruby/c.
          // It appears to work correctly even when commented out, so it is left commented out
          uint16_t n = ((pm_multi_target_node_t *)pargs[i])->lefts.size;
          gen_move(s, cursp(), pos, 0);
          genop_3(s, OP_APOST, cursp(), n, 0);
        }
        pos++;
      }
    }
  }

  codegen(s, body, VAL);
  pop();
  if (s->pc > 0) {
    gen_return(s, OP_RETURN, cursp());
  }
  if (blk) {
    loop_pop(s, NOVAL);
  }
  scope_finish(s);
  return parent->irep->rlen - 1;
}

static void
gen_lvar(mrc_codegen_scope *s, mrc_sym name, int depth)
{
  if (depth == 0) {
    gen_move(s, cursp(), lv_idx(s, name), 1);
  }
  else {
    gen_getupvar(s, cursp(), name, depth);
  }
  push();
}

static void
gen_binary_operator(mrc_codegen_scope *s, mrc_sym binary_operator)
{
  if (binary_operator == MRC_OPSYM_2(add)) {
    gen_addsub(s, OP_ADD, cursp());
  }
  else if (binary_operator == MRC_OPSYM_2(sub)) {
    gen_addsub(s, OP_SUB, cursp());
  }
  else if (binary_operator == MRC_OPSYM_2(mul)) {
    genop_1(s, OP_MUL, cursp());
  }
  else if (binary_operator == MRC_OPSYM_2(div)) {
    genop_1(s, OP_DIV, cursp());
  }
  else {
    int idx = new_sym(s, binary_operator);
    genop_3(s, OP_SEND, cursp(), idx, 1);
  }
}

static void
regex_set_flags(pm_node_flags_t flags, char *p2, char *p3)
{
  int p2_len = 0;
  if (flags&PM_REGULAR_EXPRESSION_FLAGS_IGNORE_CASE) p2[p2_len++] = 'i';
  if (flags&PM_REGULAR_EXPRESSION_FLAGS_EXTENDED)    p2[p2_len++] = 'x';
  if (flags&PM_REGULAR_EXPRESSION_FLAGS_MULTI_LINE)  p2[p2_len++] = 'm';
  // mruby does not support once-only subexpression
  // if (flags|PM_REGULAR_EXPRESSION_FLAGS_ONCE)     p2[p2_len++] = 'o';
  if (flags&PM_REGULAR_EXPRESSION_FLAGS_EUC_JP)           p3[0] = 'e';
  else if (flags&PM_REGULAR_EXPRESSION_FLAGS_ASCII_8BIT)  p3[0] = 'n';
  else if (flags&PM_REGULAR_EXPRESSION_FLAGS_WINDOWS_31J) p3[0] = 's';
  else if (flags&PM_REGULAR_EXPRESSION_FLAGS_UTF_8)       p3[0] = 'u';
  // TODO???
  // /** internal bytes forced the encoding to UTF-8 */
  // PM_REGULAR_EXPRESSION_FLAGS_FORCED_UTF8_ENCODING
  // /** internal bytes forced the encoding to binary */
  // PM_REGULAR_EXPRESSION_FLAGS_FORCED_BINARY_ENCODING
  // /** internal bytes forced the encoding to US-ASCII */
  // PM_REGULAR_EXPRESSION_FLAGS_FORCED_US_ASCII_ENCODING
}

static void
gen_begin(mrc_codegen_scope *s, mrc_node *tree, int val)
{
  CAST(begin);
  if (val && !cast->statements) {
    genop_1(s, OP_LOADNIL, cursp());
    push();
  }
  if (cast->statements) {
    size_t last_index = cast->statements->body.size;
    for (uint32_t i = 0; i < last_index; i++) {
      codegen(s, (mrc_node *)cast->statements->body.nodes[i], (i+1 < last_index) ? NOVAL : val);
    }
  }
}

static void
gen_rescue(mrc_codegen_scope *s, mrc_node *tree, uint32_t *pos1, int *exc, uint32_t *extend, int val)
{
  CAST3(rescue, tree, rescue);
  if (nint((mrc_node *)rescue) != PM_RESCUE_NODE) {
    codegen_error(s, "should not happen");
  }
  size_t i;
  uint32_t pos2, tmp;

  dispatch(s, *pos1);
  pos2 = JMPLINK_START;

  /* handle classes */
  if (rescue->exceptions.size == 0) {
    genop_2(s, OP_GETCONST, cursp(), new_sym(s, MRC_SYM_1(StandardError)));
    push();
    pop();
    genop_2(s, OP_RESCUE, *exc, cursp());
    tmp = genjmp2(s, OP_JMPIF, cursp(), pos2, val);
    pos2 = tmp;
  }
  else {
    for (i = 0; i < rescue->exceptions.size; i++) {
      if (nint((mrc_node *)rescue->exceptions.nodes[i]) == PM_SPLAT_NODE) {
        codegen(s, (mrc_node *)rescue->exceptions.nodes[i], VAL);
        gen_move(s, cursp(), *exc, 0);
        push_n(2); pop_n(2); /* space for one arg and a block */
        pop();
        genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_SYM_1(__case_eqq)), 1);
      }
      else {
        codegen(s, (mrc_node *)rescue->exceptions.nodes[i], VAL);
        pop();
        genop_2(s, OP_RESCUE, *exc, cursp());
      }
      tmp = genjmp2(s, OP_JMPIF, cursp(), pos2, val);
      pos2 = tmp;
    }
  }
  *pos1 = genjmp_0(s, OP_JMP);
  dispatch_linked(s, pos2);

  pop();
  /* exc_var: `=> e` */
  if (rescue->reference) {
    gen_assignment(s, rescue->reference, NULL, *exc, NOVAL);
  }
  /* handle body */
  codegen(s, (mrc_node *)rescue->statements, val);
  if (val) pop();
  tmp = genjmp(s, OP_JMP, *extend);
  *extend = tmp;
  push();
  /* rest of rescue(s) */
  if (rescue->subsequent) {
    gen_rescue(s, (mrc_node *)rescue->subsequent, pos1, exc, extend, val);
  }
}

static void
gen_ensure(mrc_codegen_scope *s, mrc_node *tree, uint32_t catch_entry, uint32_t begin)
{
  CAST3(ensure, tree, ensure);
  int ensure_end, ensure_target;
  int idx;
  push();
  ensure_end = ensure_target = s->pc;
  push();
  idx = cursp();
  genop_1(s, OP_EXCEPT, idx);
  push();
  codegen(s, (mrc_node *)ensure->statements, NOVAL);
  pop();
  genop_1(s, OP_RAISEIF, idx);
  pop();
  catch_handler_set(s, catch_entry, MRC_CATCH_ENSURE, begin, ensure_end, ensure_target);
}

/* Load the integer described by a pm_integer_t into the current stack slot,
   handling the small (uint32), 64-bit, and bignum-literal cases. Shared by
   PM_INTEGER_NODE and the rational literal codegen. */
static void
gen_pm_integer(mrc_codegen_scope *s, const pm_integer_t *iv)
{
  if (iv->length == 0) {
    if (!iv->negative) {
      gen_int(s, cursp(), (mrc_int)iv->value);
    }
    else {
      gen_int(s, cursp(), (mrc_int)iv->value * -1);
    }
    return;
  }
#ifdef MRC_INT64
  if (iv->length == 2) {
    mrc_uint value = ((mrc_uint)iv->values[0])|((mrc_uint)iv->values[1] << 32);
    mrc_bool fits = TRUE;
    if (!iv->negative && MRC_INT_MAX < value) fits = FALSE;
    if (iv->negative) {
      if (value > (mrc_uint)MRC_INT_MIN) fits = FALSE;
      else value *= -1;
    }
    if (fits) {
      gen_int(s, cursp(), value);
      return;
    }
  }
#endif
  {
    pm_buffer_t buf = {0};
    pm_integer_string(&buf, iv);
    buf.value[buf.length] = '\0';
    if (iv->negative) {
      memmove(buf.value, buf.value+1, buf.length);
      buf.length--;
    }
    int off = new_litbint(s, buf.value, 10, iv->negative);
    genop_2(s, OP_LOADL, cursp(), off);
    pm_buffer_free(&buf);
  }
}

static void
codegen(mrc_codegen_scope *s, mrc_node *tree, int val)
{
  int rlev = s->rlev;

  if (!tree) {
    if (val) {
      genop_1(s, OP_LOADNIL, cursp());
      push();
    }
    return;
  }

  s->rlev++;
  if (s->rlev > MRC_CODEGEN_LEVEL_MAX) {
    codegen_error(s, "too complex expression");
  }
  uint32_t token_pos = (uint32_t)(tree->location.start - s->c->p->start);

  if (s->filename_index+1 < s->c->filename_table_length) {
    if (s->c->filename_table[s->filename_index+1].start <= token_pos) {
      mrc_debug_info_append_file(s->c, s->irep->debug_info,
                                s->filename, s->lines, s->debug_start_pos, s->pc);
      s->debug_start_pos = s->pc;
      s->filename_index++;
      s->filename = (const char *)s->c->filename_table[s->filename_index].filename;
    }
  }

  int nt = nint(tree);

  s->lineno = node_lineno(s->c, tree);

  switch (nt) {
    case PM_PROGRAM_NODE: {
      scope_body(s, tree, val);
      break;
    }
    case PM_STATEMENTS_NODE:
    {
      CAST(statements);
      size_t last_index = cast->body.size;
      if (last_index == 0) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
        break;
      }
      for (uint32_t i = 0; i < last_index; i++) {
        codegen(s, (mrc_node *)cast->body.nodes[i], (i+1 < last_index) ? NOVAL : val);
      }
      break;
    }
    case PM_INSTANCE_VARIABLE_READ_NODE:
    {
      CAST(instance_variable_read);
      int sym = new_sym(s, cast->name);

      genop_2(s, OP_GETIV, cursp(), sym);
      if (val) push();
      break;
    }
    case PM_LOCAL_VARIABLE_READ_NODE:
    {
      if (val) {
        CAST(local_variable_read);
        gen_lvar(s, cast->name, cast->depth + s->for_depth);
      }
      break;
    }
    case PM_IT_LOCAL_VARIABLE_READ_NODE:
    {
      if (val) {
        gen_move(s, cursp(), 1, 1); /* `it` is always at slot 1 */
        push();
      }
      break;
    }
    case PM_GLOBAL_VARIABLE_READ_NODE:
    {
      CAST(global_variable_read);
      int sym = new_sym(s, cast->name);
      genop_2(s, OP_GETGV, cursp(), sym);
      if (val) push();
      break;
    }
    case PM_CLASS_VARIABLE_READ_NODE:
    {
      CAST(class_variable_read);
      int sym = new_sym(s, cast->name);
      genop_2(s, OP_GETCV, cursp(), sym);
      if (val) push();
      break;
    }
    case PM_CONSTANT_READ_NODE:
    {
      CAST(constant_read);
      int sym = new_sym(s, cast->name);
      genop_2(s, OP_GETCONST, cursp(), sym);
      if (val) push();
      break;
    }
#define case_WRITE_NODE(NODE_TYPE, CAST_TYPE) \
  case NODE_TYPE##_WRITE_NODE: \
    { \
      CAST_TYPE##_write_node_t *cast = (CAST_TYPE##_write_node_t *)tree; \
      gen_assignment(s, tree, (mrc_node *)cast->value, 0, val); \
      break; \
    } \
  case NODE_TYPE##_TARGET_NODE: \
    { \
      gen_assignment(s, tree, NULL, 0, val); \
      break; \
    }
    case_WRITE_NODE(PM_INSTANCE_VARIABLE, pm_instance_variable)
    case_WRITE_NODE(PM_LOCAL_VARIABLE, pm_local_variable)
    case_WRITE_NODE(PM_CONSTANT, pm_constant)
    case_WRITE_NODE(PM_GLOBAL_VARIABLE, pm_global_variable)
    case_WRITE_NODE(PM_CLASS_VARIABLE, pm_global_variable)
    case_WRITE_NODE(PM_CONSTANT_PATH, pm_constant_path)
    case PM_MULTI_WRITE_NODE:
    {
      CAST(multi_write);
      size_t len = 0, n = 0, post = 0;
      CAST3(array, cast->value, t);
      int rhs = cursp();

      if (!val && nint((mrc_node *)t) == PM_ARRAY_NODE && !(t->base.flags & PM_ARRAY_NODE_FLAGS_CONTAINS_SPLAT) ) {
        /* fixed rhs */
        len = t->elements.size;
        for (size_t i = 0; i < len; i++) {
          codegen(s, t->elements.nodes[i], VAL);
        }
        if (0 < cast->lefts.size) {
          n = 0;
          for (size_t i = 0; i < cast->lefts.size; i++) {
            if (i < len) {
              gen_assignment(s, cast->lefts.nodes[i], NULL, rhs+n, NOVAL);
              n++;
            }
            else {
              genop_1(s, OP_LOADNIL, rhs+n);
              gen_assignment(s, cast->lefts.nodes[i], NULL, rhs+n, NOVAL);
            }
          }
        }
        post = cast->rights.size;
        if (cast->rest && nint(cast->rest) != PM_IMPLICIT_REST_NODE) {
          int rn;
          if (len < post + n) {
            rn = 0;
          }
          else {
            rn = len - post - n;
          }
          if (cursp() == rhs+n) {
            genop_2(s, OP_ARRAY, cursp(), rn);
          }
          else {
            genop_3(s, OP_ARRAY2, cursp(), rhs+n, rn);
          }
          if (((pm_splat_node_t *)cast->rest)->expression) {
            gen_assignment(s, ((pm_splat_node_t *)cast->rest)->expression, NULL, cursp(), NOVAL);
          }
          n += rn;
        }
        else if (cast->rest && nint(cast->rest) == PM_IMPLICIT_REST_NODE) {
          /* trailing comma: just consume the rest without assignment */
          int rn;
          if (len < post + n) {
            rn = 0;
          }
          else {
            rn = len - post - n;
          }
          n += rn;
        }
        if (0 < post) {
          for (size_t i = 0; i < post; i++) {
            if (n < len) {
              gen_assignment(s, cast->rights.nodes[i], NULL, rhs+n, NOVAL);
            }
            else {
              genop_1(s, OP_LOADNIL, cursp());
              gen_assignment(s, cast->rights.nodes[i], NULL, cursp(), NOVAL);
              n++;
            }
          }
        }
        pop_n(len);
      }
      else {
        /* variable rhs */
        codegen(s, cast->value, VAL);
        gen_massignment(s, tree, rhs, val);
        if (!val) {
          pop();
        }
      }
      break;
    }
    case PM_CONSTANT_PATH_OPERATOR_WRITE_NODE:
    {
      codegen_error(s, "constant re-assignment");
      break;
    }
    case PM_LOCAL_VARIABLE_OPERATOR_WRITE_NODE:
    case PM_GLOBAL_VARIABLE_OPERATOR_WRITE_NODE:
    case PM_INSTANCE_VARIABLE_OPERATOR_WRITE_NODE:
    case PM_CLASS_VARIABLE_OPERATOR_WRITE_NODE:
    case PM_CONSTANT_OPERATOR_WRITE_NODE:
    {
      mrc_sym name = -1, binary_operator = -1;
      mrc_node *value = NULL;
      int op_set = -1, op_get = -1, depth = -1;
#define CAST_OP_WRITE(type) \
  CAST(type); \
  name = cast->name; \
  value = cast->value; \
  binary_operator = cast->binary_operator
      switch (nt) {
        case PM_LOCAL_VARIABLE_OPERATOR_WRITE_NODE:
          {
            CAST_OP_WRITE(local_variable_operator_write);
            depth = cast->depth + s->for_depth;
            break;
          }
        case PM_GLOBAL_VARIABLE_OPERATOR_WRITE_NODE:
          {
            CAST_OP_WRITE(global_variable_operator_write);
            op_set = OP_SETGV; op_get = OP_GETGV;
            break;
          }
        case PM_CLASS_VARIABLE_OPERATOR_WRITE_NODE:
          {
            CAST_OP_WRITE(class_variable_operator_write);
            op_set = OP_SETCV; op_get = OP_GETCV;
            break;
          }
        case PM_INSTANCE_VARIABLE_OPERATOR_WRITE_NODE:
          {
            CAST_OP_WRITE(instance_variable_operator_write);
            op_set = OP_SETIV; op_get = OP_GETIV;
            break;
          }
        case PM_CONSTANT_OPERATOR_WRITE_NODE:
          {
            CAST_OP_WRITE(constant_operator_write);
            op_set = OP_SETCONST; op_get = OP_GETCONST;
            break;
          }
        default: codegen_error(s, "Not implemented (#5)");
      }
      switch (nt) {
        case PM_LOCAL_VARIABLE_OPERATOR_WRITE_NODE:
          gen_lvar(s, name, depth);
          break;
        case PM_GLOBAL_VARIABLE_OPERATOR_WRITE_NODE:
        case PM_INSTANCE_VARIABLE_OPERATOR_WRITE_NODE:
        case PM_CLASS_VARIABLE_OPERATOR_WRITE_NODE:
        case PM_CONSTANT_OPERATOR_WRITE_NODE:
          genop_2(s, op_get, cursp(), new_sym(s, name));
          push();
          break;
        default: codegen_error(s, "Not implemented (#6)");
      }
      codegen(s, (mrc_node *)value, VAL);
      push(); pop();
      pop(); pop();

      gen_binary_operator(s, binary_operator);
      switch (nt) {
        case PM_LOCAL_VARIABLE_OPERATOR_WRITE_NODE:
          gen_assignment_lvar(s, cursp(), name, depth, val);
          break;
        case PM_GLOBAL_VARIABLE_OPERATOR_WRITE_NODE:
        case PM_INSTANCE_VARIABLE_OPERATOR_WRITE_NODE:
        case PM_CLASS_VARIABLE_OPERATOR_WRITE_NODE:
        case PM_CONSTANT_OPERATOR_WRITE_NODE:
          gen_setxv(s, op_set, cursp(), name, val);
          break;
        default: codegen_error(s, "Not implemented (#7)");
      }
      if (val) push();
      break;
    }
    case PM_CALL_OPERATOR_WRITE_NODE:
    case PM_CALL_OR_WRITE_NODE:
    case PM_CALL_AND_WRITE_NODE:
    {
#define CAST_CALL_WRITE(type) \
  CAST(type); \
  receiver = (mrc_node *)cast->receiver; \
  value = (mrc_node *)cast->value; \
  read_name = cast->read_name; \
  write_name = cast->write_name;
      mrc_node *receiver = NULL, *value = NULL;
      mrc_sym read_name = -1, write_name = -1, binary_operator = -1, op_jmp = -1;
      uint32_t pos = -1;
      switch (nt) {
        case PM_CALL_OPERATOR_WRITE_NODE:
          {
            CAST_CALL_WRITE(call_operator_write);
            binary_operator = cast->binary_operator;
            break;
          }
        case PM_CALL_OR_WRITE_NODE:
          {
            CAST_CALL_WRITE(call_or_write);
            op_jmp = OP_JMPIF;
            break;
          }
        case PM_CALL_AND_WRITE_NODE:
          {
            CAST_CALL_WRITE(call_and_write);
            op_jmp = OP_JMPNOT;
            break;
          }
        default: codegen_error(s, "Not implemented (call_operator|or|and_write)");
      }
      int base;
      int idx, vsp = -1;
      if (val) {
        vsp = cursp();
        push();
      }
      codegen(s, receiver, VAL);
      idx = new_sym(s, read_name);
      base = cursp()-1;
      /* copy receiver and arguments */
      gen_move(s, cursp(), base, 1);
      push_n(2); pop_n(2); /* space for receiver, arguments and a block */
      genop_3(s, OP_SEND, cursp(), idx, 0);

      if (-1 != (int32_t)binary_operator) {
        push();
        codegen(s, value, VAL);
        push(); pop();
        pop(); pop();
        gen_binary_operator(s, binary_operator);
      }
      else { /* OR or AND */
        if (0 <= vsp) {
          gen_move(s, vsp, cursp(), 0);
        }
        pos = genjmp2_0(s, op_jmp, cursp(), val);
        codegen(s, value, VAL);
        pop();
      }
      if (0 <= vsp) {
        gen_move(s, vsp, cursp(), 0);
      }
      pop();
      idx = new_sym(s, write_name);
      genop_3(s, OP_SEND, cursp(), idx, 1);
      if (0 < pos) { dispatch(s, pos); }
      break;
    }
    case PM_INDEX_OPERATOR_WRITE_NODE:
    case PM_INDEX_OR_WRITE_NODE:
    case PM_INDEX_AND_WRITE_NODE:
    {
#define CAST_INDEX_WRITE(type) \
  CAST(type); \
  receiver = (mrc_node *)cast->receiver; \
  value = (mrc_node *)cast->value; \
  arguments = (mrc_node *)cast->arguments;
      mrc_node *receiver, *value, *arguments;
      mrc_sym binary_operator = -1;
      mrc_sym op_jmp = -1;
      switch (nt) {
        case PM_INDEX_OPERATOR_WRITE_NODE:
          {
            CAST_INDEX_WRITE(index_operator_write);
            binary_operator = cast->binary_operator;
            break;
          }
        case PM_INDEX_OR_WRITE_NODE:
          {
            CAST_INDEX_WRITE(index_or_write);
            op_jmp = OP_JMPIF;
            break;
          }
        case PM_INDEX_AND_WRITE_NODE:
          {
            CAST_INDEX_WRITE(index_and_write);
            op_jmp = OP_JMPNOT;
            break;
          }
        default:
          {
            codegen_error(s, "Not implemented (index_operator|or|and_write)");
            return;
          }
      }
      int base, nargs = 0;
      int idx, callargs = -1, vsp = -1;
      int32_t pos = -1;
      if (val) {
        vsp = cursp();
        push();
      }
      codegen(s, (mrc_node *)receiver, VAL);
      idx = new_sym(s, MRC_OPSYM_2(aref));
      base = cursp()-1;
      nargs = gen_values(s, (mrc_node *)arguments, VAL, 13);
      if (nargs >= 0) {
        callargs = nargs;
      }
      else { /* varargs */
        push();
        nargs = 1;
        callargs = CALL_MAXARGS;
      }
      /* copy receiver and arguments */
      gen_move(s, cursp(), base, 1);
      for (int i = 0; i < nargs; i++) {
        gen_move(s, cursp()+i+1, base+i+1, 1);
      }
      push_n(nargs + 2); pop_n(nargs + 2); /* space for receiver, arguments and a block */
      genop_3(s, OP_SEND, cursp(), idx, callargs);
      if (-1 != (int32_t)binary_operator) {
        push();
        codegen(s, value, VAL);
        push(); pop();
        pop(); pop();
        gen_binary_operator(s, binary_operator);
      }
      else { /* OR or AND */
        if (0 <= vsp) {
          gen_move(s, vsp, cursp(), 0);
        }
        pos = genjmp2_0(s, op_jmp, cursp(), val);
        codegen(s, value, VAL);
        pop();
        dispatch(s, pos);
      }
      if (val && vsp >= 0) {
        gen_move(s, vsp, cursp(), 0);
      }
      if (callargs == CALL_MAXARGS) {
        pop();
        genop_2(s, OP_ARYPUSH, cursp(), 1);
      }
      else {
        pop_n(callargs);
        callargs++;
      }
      pop();
      idx = new_sym(s, MRC_OPSYM_2(aset));
      genop_3(s, OP_SEND, cursp(), idx, callargs);
      if (0 <= pos) { dispatch(s, pos); }
      break;
    }
    case PM_LOCAL_VARIABLE_OR_WRITE_NODE:
    case PM_LOCAL_VARIABLE_AND_WRITE_NODE:
    case PM_INSTANCE_VARIABLE_OR_WRITE_NODE:
    case PM_INSTANCE_VARIABLE_AND_WRITE_NODE:
    case PM_CLASS_VARIABLE_OR_WRITE_NODE:
    case PM_CLASS_VARIABLE_AND_WRITE_NODE:
    case PM_GLOBAL_VARIABLE_OR_WRITE_NODE:
    case PM_GLOBAL_VARIABLE_AND_WRITE_NODE:
    case PM_CONSTANT_OR_WRITE_NODE:
    case PM_CONSTANT_AND_WRITE_NODE:
    {
      mrc_sym name = 0;
      mrc_node *value = NULL;
      int op_set = -1, op_get = -1, depth = -1;
      int op_jmp = OP_JMPNOT;
#define CAST_OR_WRITE(type) \
  CAST(type); \
  name = cast->name; \
  value = cast->value
      switch (nt) {
        case PM_LOCAL_VARIABLE_OR_WRITE_NODE: op_jmp = OP_JMPIF; /* fall through */
        case PM_LOCAL_VARIABLE_AND_WRITE_NODE:
          {
            CAST_OR_WRITE(local_variable_or_write);
            depth = cast->depth + s->for_depth;
            break;
          }
        case PM_INSTANCE_VARIABLE_OR_WRITE_NODE: op_jmp = OP_JMPIF; /* fall through */
        case PM_INSTANCE_VARIABLE_AND_WRITE_NODE:
          {
            CAST_OR_WRITE(instance_variable_or_write);
            op_set = OP_SETIV; op_get = OP_GETIV;
            break;
          }
        case PM_GLOBAL_VARIABLE_OR_WRITE_NODE: op_jmp = OP_JMPIF; /* fall through */
        case PM_GLOBAL_VARIABLE_AND_WRITE_NODE:
          {
            CAST_OR_WRITE(global_variable_or_write);
            op_set = OP_SETGV; op_get = OP_GETGV;
            break;
          }
        case PM_CLASS_VARIABLE_OR_WRITE_NODE: op_jmp = OP_JMPIF; /* fall through */
        case PM_CLASS_VARIABLE_AND_WRITE_NODE:
          {
            CAST_OR_WRITE(class_variable_or_write);
            op_set = OP_SETCV; op_get = OP_GETCV;
            break;
          }
        case PM_CONSTANT_OR_WRITE_NODE: op_jmp = OP_JMPIF; /* fall through */
        case PM_CONSTANT_AND_WRITE_NODE:
          {
            CAST_OR_WRITE(constant_or_write);
            op_set = OP_SETCONST; op_get = OP_GETCONST;
            break;
          }
        default: codegen_error(s, "Not implemented (or_write_node)");
      }
      switch (nt) {
        case PM_LOCAL_VARIABLE_OR_WRITE_NODE:
        case PM_LOCAL_VARIABLE_AND_WRITE_NODE:
          gen_lvar(s, name, depth);
          break;
        case PM_GLOBAL_VARIABLE_OR_WRITE_NODE:
        case PM_GLOBAL_VARIABLE_AND_WRITE_NODE:
        case PM_INSTANCE_VARIABLE_OR_WRITE_NODE:
        case PM_INSTANCE_VARIABLE_AND_WRITE_NODE:
        case PM_CLASS_VARIABLE_AND_WRITE_NODE:
        case PM_CONSTANT_AND_WRITE_NODE:
          genop_2(s, op_get, cursp(), new_sym(s, name));
          push();
          break;
        case PM_CLASS_VARIABLE_OR_WRITE_NODE:
        case PM_CONSTANT_OR_WRITE_NODE:
          {
            int catch_entry, begin, end;
            int noexc, exc;
            struct loopinfo *lp;

            lp = loop_push(s, LOOP_BEGIN);
            lp->pc0 = new_label(s);
            catch_entry = catch_handler_new(s);
            begin = s->pc;
            exc = cursp();
            genop_2(s, op_get, cursp(), new_sym(s, name));
            push();
            end = s->pc;
            noexc = genjmp_0(s, OP_JMP);
            lp->type = LOOP_RESCUE;
            catch_handler_set(s, catch_entry, MRC_CATCH_RESCUE, begin, end, s->pc);
            genop_1(s, OP_EXCEPT, exc);
            genop_1(s, OP_LOADFALSE, exc);
            dispatch(s, noexc);
            loop_pop(s, NOVAL);
            break;
          }
        default: codegen_error(s, "Not implemented (or_write_node)");
      }
      uint32_t pos;
      pop();
      pos = genjmp2_0(s, op_jmp, cursp(), val);
      codegen(s, value, VAL);
      pop();
      switch (nt) {
        case PM_LOCAL_VARIABLE_OR_WRITE_NODE:
        case PM_LOCAL_VARIABLE_AND_WRITE_NODE:
          gen_assignment_lvar(s, cursp(), name, depth, val);
          break;
        case PM_GLOBAL_VARIABLE_OR_WRITE_NODE:
        case PM_GLOBAL_VARIABLE_AND_WRITE_NODE:
        case PM_INSTANCE_VARIABLE_OR_WRITE_NODE:
        case PM_INSTANCE_VARIABLE_AND_WRITE_NODE:
        case PM_CLASS_VARIABLE_OR_WRITE_NODE:
        case PM_CLASS_VARIABLE_AND_WRITE_NODE:
        case PM_CONSTANT_OR_WRITE_NODE:
        case PM_CONSTANT_AND_WRITE_NODE:
          gen_setxv(s, op_set, cursp(), name, val);
          push();
          break;
        default: codegen_error(s, "Not implemented (or_write)");
      }
      dispatch(s, pos);
      break;
    }
    case PM_INTEGER_NODE:
    {
      if (val) {
        CAST(integer);
        gen_pm_integer(s, &cast->value);
        push();
      }
      break;
    }
    case PM_RATIONAL_NODE:
    {
      /* `Nr` literal -> Rational(numerator, denominator). Prism normalizes
         even float forms (1.5r -> 3/2), so both parts are integers. */
      if (val) {
        CAST(rational);
        int recv = cursp();
        push();                                  /* receiver (self) slot */
        gen_pm_integer(s, &cast->numerator); push();
        gen_pm_integer(s, &cast->denominator); push();
        pop_n(3);
        genop_3(s, OP_SSEND, recv,
                new_sym(s, nsym(s->c->p, (const uint8_t*)"Rational", 8)), 2);
        push();
      }
      break;
    }
    case PM_IMAGINARY_NODE:
    {
      /* `Ni` literal -> Complex(0, numeric). */
      if (val) {
        CAST(imaginary);
        int recv = cursp();
        push();                                  /* receiver (self) slot */
        gen_int(s, cursp(), 0); push();          /* real part */
        codegen(s, (mrc_node*)cast->numeric, VAL); /* imaginary part */
        pop_n(3);
        genop_3(s, OP_SSEND, recv,
                new_sym(s, nsym(s->c->p, (const uint8_t*)"Complex", 7)), 2);
        push();
      }
      break;
    }
#ifndef MRC_NO_FLOAT
  case PM_FLOAT_NODE:
  {
    if (val) {
      CAST(float);
      int off = new_lit_float(s, (mrc_float)cast->value);
      genop_2(s, OP_LOADL, cursp(), off);
      push();
    }
    break;
  }
#endif
    case PM_CALL_NODE:
    {
      CAST(call);
      gen_call(s, tree, val, (cast->base.flags & PM_CALL_NODE_FLAGS_SAFE_NAVIGATION) ? 1 : 0);
      break;
    }
    case PM_ARRAY_NODE:
    case PM_ARGUMENTS_NODE:
    {
      int n;
      n = gen_values(s, tree, val, 0);
      if (val) {
        if (n >= 0) {
          pop_n(n);
          genop_2(s, OP_ARRAY, cursp(), n);
        }
        push();
      }
      break;
    }
    case PM_SYMBOL_NODE:
    {
      if (val) {
        CAST(symbol);
        int sym = new_sym(s, nsym(s->c->p, cast->unescaped.source, cast->unescaped.length));

        genop_2(s, OP_LOADSYM, cursp(), sym);
        push();
      }
      break;
    }
    case PM_KEYWORD_HASH_NODE:
    case PM_HASH_NODE:
    {
      int nk = gen_hash(s, tree, val, GEN_LIT_ARY_MAX);
      if (val && nk >= 0) {
        pop_n(nk*2);
        genop_2(s, OP_HASH, cursp(), nk);
        push();
      }
      break;
    }
    case PM_IMPLICIT_NODE:
    {
      CAST(implicit);
      codegen(s, (mrc_node *)cast->value, val);
      break;
    }
    case PM_SPLAT_NODE:
    {
      CAST(splat);
      if (cast->expression) {
        codegen(s, (mrc_node *)cast->expression, val);
      } else if (val) {
        /* anonymous splat: load local variable '*' */
        pm_constant_id_t astr = MRC_OPSYM_2(mul);
        gen_lvar(s, astr, 0);
      }
      break;
    }
    case PM_STRING_NODE:
    {
      if (val) {
        CAST(string);
        char *p = (char *)cast->unescaped.source;
        mrc_int len = cast->unescaped.length;
        int off = new_lit_str(s, p, len);

        genop_2(s, OP_STRING, cursp(), off);
        push();
      }
      break;
    }
    case PM_X_STRING_NODE:
    {
      CAST(x_string);
      char *p = (char *)cast->unescaped.source;
      mrc_int len = cast->unescaped.length;
      int off = new_lit_str(s, p, len);
      int sym = new_sym(s, MRC_OPSYM_2(tick));

      genop_1(s, OP_LOADSELF, cursp());
      push();
      genop_2(s, OP_STRING, cursp(), off);
      push(); push();
      pop_n(3);
      /* SSEND: backtick is a private Kernel method, call it on self */
      genop_3(s, OP_SSEND, cursp(), sym, 1);
      if (val) push();
      break;
    }
    case PM_REGULAR_EXPRESSION_NODE:
    {
      if (val) {
        CAST(regular_expression);
        char *p1 = (char *)cast->unescaped.source;
        char p2[4] = {0, 0, 0, 0};
        char p3[2] = {0, 0};
        regex_set_flags(cast->base.flags, p2, p3);
        int sym = new_sym(s, MRC_SYM_1(Regexp));
        int off = new_lit_str(s, p1, cast->unescaped.length);
        int argc = 1;

        genop_1(s, OP_OCLASS, cursp());
        genop_2(s, OP_GETMCNST, cursp(), sym);
        push();
        genop_2(s, OP_STRING, cursp(), off);
        push();
        if (p2[0] || p3[0]) {
          if (p2[0]) { /* opt */
            off = new_lit_cstr(s, p2);
            genop_2(s, OP_STRING, cursp(), off);
          }
          else {
            genop_1(s, OP_LOADNIL, cursp());
          }
          push();
          argc++;
          if (p3[0]) { /* enc */
            off = new_lit_str(s, p3, 1);
            genop_2(s, OP_STRING, cursp(), off);
            push();
            argc++;
          }
        }
        push(); /* space for a block */
        pop_n(argc+2);
        sym = new_sym(s, MRC_SYM_1(compile));
        genop_3(s, OP_SEND, cursp(), sym, argc);
        push();
      }
      break;
    }
    case PM_INTERPOLATED_REGULAR_EXPRESSION_NODE:
    {
      CAST(interpolated_regular_expression);
      if (val) {
        int sym = new_sym(s, MRC_SYM_1(Regexp));
        int argc = 1;

        genop_1(s, OP_OCLASS, cursp());
        genop_2(s, OP_GETMCNST, cursp(), sym);
        push();

        mrc_bool str_begin = FALSE;
        if (nint(cast->parts.nodes[0]) != PM_STRING_NODE) {
          genop_2(s, OP_STRING, cursp(), new_lit_cstr(s, ""));
          push();
          str_begin = TRUE;
        }
        for (size_t i = 0; i < cast->parts.size; i++) {
          codegen(s, cast->parts.nodes[i], VAL);
          pop();
          if (str_begin || 0 < i) {
            pop();
            genop_1(s, OP_STRCAT, cursp());
          }
          push();
        }

        char p2[4] = {0, 0, 0, 0};
        char p3[2] = {0, 0};
        regex_set_flags(cast->base.flags, p2, p3);
        if (p2[0]) { /* opt */
          genop_2(s, OP_STRING, cursp(), new_lit_cstr(s, p2));
          push();
          argc++;
        }
        if (p3[0]) { /* enc */
          genop_2(s, OP_STRING, cursp(), new_lit_cstr(s, p3));
          push();
          argc++;
        }
        push(); /* space for a block */
        pop_n(argc+2);
        sym = new_sym(s, MRC_SYM_1(compile));
        genop_3(s, OP_SEND, cursp(), sym, argc);
        push();
      }
      else {
        for (size_t i = 0; i < cast->parts.size; i++) {
          if (nint(cast->parts.nodes[i]) != PM_STRING_NODE) {
            codegen(s, cast->parts.nodes[i], NOVAL);
          }
        }
      }
      break;
    }
    case PM_BACK_REFERENCE_READ_NODE:
    {
      if (val) {
        CAST(back_reference_read);
        int sym = new_sym(s, cast->name);
        genop_2(s, OP_GETGV, cursp(), sym);
        push();
      }
      break;
    }
    case PM_NUMBERED_REFERENCE_READ_NODE:
    {
      if (val) {
        CAST(numbered_reference_read);
        char buf[16];
        buf[0] = '$';
        int n = snprintf(buf + 1, sizeof(buf) - 1, "%u", (unsigned int)cast->number);
        size_t len = (size_t)(1 + n);  // leading '$' + digits
        uint8_t *name = (uint8_t *)mrc_malloc(s->c, len);
        memcpy(name, buf, len);
        int sym = new_sym(s, pm_constant_pool_insert_owned(&s->c->p->constant_pool, name, len));
        genop_2(s, OP_GETGV, cursp(), sym);
        push();
      }
      break;
    }
    case PM_EMBEDDED_STATEMENTS_NODE:
    {
      CAST(embedded_statements);
      codegen(s, (mrc_node *)cast->statements, val);
      break;
    }
    case PM_INTERPOLATED_STRING_NODE:
    case PM_INTERPOLATED_SYMBOL_NODE:
    {
      size_t i;
      mrc_node **nodes;
      uint32_t size;
      if (nt == PM_INTERPOLATED_SYMBOL_NODE) {
        CAST(interpolated_symbol);
        nodes = (mrc_node **)cast->parts.nodes;
        size = cast->parts.size;
      }
      else {
        CAST(interpolated_string);
        nodes = (mrc_node **)cast->parts.nodes;
        size = cast->parts.size;
      }
      mrc_bool str_begin = FALSE;
      if (val) {
        if (nint(nodes[0]) != PM_STRING_NODE) {
          genop_2(s, OP_STRING, cursp(), new_lit_cstr(s, ""));
          push();
          str_begin = TRUE;
        }
        for (i = 0; i < size; i++) {
          codegen(s, nodes[i], VAL);
          pop();
          if (str_begin || 0 < i) {
            pop();
            genop_1(s, OP_STRCAT, cursp());
          }
          push();
        }
      }
      else {
        /* example:
         *  def my_method
         *    "Hey, #{something} happens!"
         *    return 1
         *  end
         * # The return value of the interpolated string will not be used.
         * # (This is a case when val is FALSE)
         * # So we ignore `'Hey, '` and `' happens!'`.
         * # However, we need to evaluate `something` as it may have side effects.
         */
        for (i = 0; i < size; i++) {
          if (nint(nodes[i]) != PM_STRING_NODE) {
            codegen(s, nodes[i], NOVAL);
            pop();
          }
        }
      }
      if (nt == PM_INTERPOLATED_SYMBOL_NODE) {
        if (val) {
          pop();
          if (!no_peephole(s)) {
            struct mrc_insn_data data = mrc_last_insn(s);
            if (data.insn == OP_STRING && data.a == cursp()) {
              rewind_pc(s);
              genop_2(s, OP_SYMBOL, data.a, data.b);
              push();
              break;
            }
          }
          genop_1(s, OP_INTERN, cursp());
          push();
        }
      }
      break;
    }
    case PM_INTERPOLATED_X_STRING_NODE:
    {
      size_t i;
      CAST(interpolated_x_string);
      int sym = new_sym(s, MRC_SYM_1(Kernel));

      genop_1(s, OP_LOADSELF, cursp());
      push();
      /*
       * Use the same pattern as PM_INTERPOLATED_STRING_NODE to avoid
       * mutating shared string references via OP_STRCAT.
       * When the first part is not a string literal (e.g. a variable),
       * prepend an empty string as a safe base for concatenation.
       */
      mrc_bool str_begin = FALSE;
      if (nint(cast->parts.nodes[0]) != PM_STRING_NODE) {
        genop_2(s, OP_STRING, cursp(), new_lit_cstr(s, ""));
        push();
        str_begin = TRUE;
      }
      for (i = 0; i < cast->parts.size; i++) {
        codegen(s, (mrc_node *)cast->parts.nodes[i], VAL);
        pop();
        if (str_begin || 0 < i) {
          pop();
          genop_1(s, OP_STRCAT, cursp());
        }
        push();
      }
      push();
      pop_n(3);
      sym = new_sym(s, MRC_OPSYM_2(tick));
      /* SSEND: backtick is a private Kernel method, call it on self */
      genop_3(s, OP_SSEND, cursp(), sym, 1);
      if (val) push();
      break;
    }
    case PM_SINGLETON_CLASS_NODE:
    {
      CAST(singleton_class);
      int idx;
      codegen(s, cast->expression, VAL);
      pop();
      genop_1(s, OP_SCLASS, cursp());
      if (cast->body == NULL) {
        genop_1(s, OP_LOADNIL, cursp());
      }
      else {
        idx = scope_body(s, tree, val);
        genop_2(s, OP_EXEC, cursp(), idx);
      }
      if (val) {
        push();
      }
      break;
    }
    case PM_DEF_NODE:
    {
      CAST(def);
      int sym = new_sym(s, cast->name);
      int idx = lambda_body(s, (mrc_node *)cast, cast->body, &cast->locals, 0);

      if (cast->receiver == NULL) {
        if (idx <= 0xff) {
          /* TDEF fusion: TCLASS + METHOD + DEF -> TDEF */
          genop_3(s, OP_TDEF, cursp(), sym, idx);
        }
        else {
          genop_1(s, OP_TCLASS, cursp());
          push();
          genop_2(s, OP_METHOD, cursp(), idx);
          push(); pop();
          pop();
          genop_2(s, OP_DEF, cursp(), sym);
        }
      }
      else {
        codegen(s, cast->receiver, VAL);
        pop();
        if (idx <= 0xff) {
          /* SDEF fusion: SCLASS + METHOD + DEF -> SDEF */
          genop_3(s, OP_SDEF, cursp(), sym, idx);
        }
        else {
          genop_1(s, OP_SCLASS, cursp());
          push();
          genop_2(s, OP_METHOD, cursp(), idx);
          push(); pop();
          pop();
          genop_2(s, OP_DEF, cursp(), sym);
        }
      }
      if (val) push();
      break;
    }
    case PM_LAMBDA_NODE:
    {
      if (val) {
        CAST(lambda);
        mrc_node *parameters = NULL;
        if ((pm_block_parameters_node_t *)cast->parameters) {
          parameters = (mrc_node *)cast->parameters;
        }
        int idx = lambda_body(s, parameters, cast->body, &cast->locals, 1);
        genop_2(s, OP_LAMBDA, cursp(), idx);
        push();
      }
      break;
    }
    case PM_BLOCK_NODE:
    {
      if (val) {
        CAST(block);
        mrc_node *parameters = NULL;
        if ((pm_block_parameters_node_t *)cast->parameters) {
          parameters = (mrc_node *)cast->parameters;
        }
        int idx = lambda_body(s, parameters, cast->body, &cast->locals, 1);
        genop_2(s, OP_BLOCK, cursp(), idx);
        push();
      }
      break;
    }
    case PM_IF_NODE:
    case PM_UNLESS_NODE:
    {
      mrc_node *predicate, *subsequent, *statements;
      if (nt == PM_IF_NODE) {
        CAST(if);
        predicate = (mrc_node *)cast->predicate;
        subsequent = (mrc_node *)cast->subsequent;
        statements = (mrc_node *)cast->statements;
      }
      else { /* unless */
        CAST(unless);
        predicate = (mrc_node *)cast->predicate;
        subsequent = (mrc_node *)cast->statements; /* opposite */
        statements = (mrc_node *)cast->else_clause; /* opposite */
      }
      uint32_t pos1, pos2;
      mrc_bool nil_p = FALSE;

      if (!predicate) {
        codegen(s, subsequent, val);
        goto exit;
      }
      if (true_always(predicate)) {
        codegen(s, statements, val);
        goto exit;
      }
      if (false_always(predicate)) {
        codegen(s, subsequent, val);
        goto exit;
      }
      if (nint(predicate) == PM_CALL_NODE) {
        pm_call_node_t *n = (pm_call_node_t *)predicate;
        mrc_sym mid = n->name;
        mrc_sym sym_nil_p = MRC_SYM_2(nil_p);
        if (mid == sym_nil_p && n->arguments == NULL) {
          nil_p = TRUE;
          if (n->receiver) {
            codegen(s, (mrc_node *)n->receiver, VAL);
          }
          else {
            /* implicit receiver: bare `nil?` means `self.nil?` (#6874) */
            genop_1(s, OP_LOADSELF, cursp());
            push();
          }
        }
      }
      if (!nil_p) {
        codegen(s, predicate, VAL);
      }
      pop();
      if (val || statements) {
        if (nil_p) {
          pos2 = genjmp2_0(s, OP_JMPNIL, cursp(), val);
          pos1 = genjmp_0(s, OP_JMP);
          dispatch(s, pos2);
        }
        else {
          pos1 = genjmp2_0(s, OP_JMPNOT, cursp(), val);
        }
        codegen(s, statements, val);
        if (val) pop();
        if (subsequent || val) {
          pos2 = genjmp_0(s, OP_JMP);
          dispatch(s, pos1);
          codegen(s, subsequent, val);
          dispatch(s, pos2);
        }
        else {
          dispatch(s, pos1);
        }
      }
      else {                   /* empty then-part */
        if (subsequent) {
          if (nil_p) {
            pos1 = genjmp2_0(s, OP_JMPNIL, cursp(), val);
          }
          else {
            pos1 = genjmp2_0(s, OP_JMPIF, cursp(), val);
          }
          codegen(s, subsequent, val);
          dispatch(s, pos1);
        }
        else if (val && !nil_p) {
          genop_1(s, OP_LOADNIL, cursp());
          push();
        }
      }
      break;
    }
    case PM_ELSE_NODE:
    {
      CAST(else);
      codegen(s, (mrc_node *)cast->statements, val);
      break;
    }
    case PM_AND_NODE:
    {
      CAST(and);
      uint32_t pos;
      if (true_always(cast->left)) {
        codegen(s, cast->right, val);
        goto exit;
      }
      if (false_always(cast->left)) {
        codegen(s, cast->left, val);
        goto exit;
      }
      codegen(s, cast->left, VAL);
      pop();
      pos = genjmp2_0(s, OP_JMPNOT, cursp(), val);
      codegen(s, cast->right, val);
      dispatch(s, pos);
      break;
    }
    case PM_OR_NODE:
    {
      CAST(or);
      uint32_t pos;
      if (true_always(cast->left)) {
        codegen(s, cast->left, val);
        goto exit;
      }
      if (false_always(cast->left)) {
        codegen(s, cast->right, val);
        goto exit;
      }
      codegen(s, cast->left, VAL);
      pop();
      pos = genjmp2_0(s, OP_JMPIF, cursp(), val);
      codegen(s, cast->right, val);
      dispatch(s, pos);
      break;
    }
    case PM_PARENTHESES_NODE:
    {
      CAST(parentheses);
      codegen(s, cast->body, val);
      break;
    }
    case PM_WHILE_NODE:
    case PM_UNTIL_NODE:
    {
      CAST(while); /* Compatible with until? */
      if (cast->base.flags & PM_LOOP_FLAGS_BEGIN_MODIFIER) {
        /* do-while: `begin ... end while/until cond` runs the body once
           before testing the condition (mirrors codegen_loop_mod). */
        mrc_bool is_until = (nt == PM_UNTIL_NODE);
        if (is_until ? true_always(cast->predicate) : false_always(cast->predicate)) {
          /* execute body once then exit */
          codegen(s, (mrc_node *)cast->statements, val);
          if (val) push();
          goto exit;
        }
        if (is_until ? false_always(cast->predicate) : true_always(cast->predicate)) {
          /* infinite loop after first execution */
          struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
          if (!val) lp->reg = -1;
          uint32_t pos0 = genjmp_0(s, OP_JMP);
          lp->pc0 = new_label(s);
          lp->pc1 = new_label(s);
          genop_0(s, OP_NOP); /* for redo */
          dispatch(s, pos0);
          codegen(s, (mrc_node *)cast->statements, NOVAL);
          genjmp(s, OP_JMP, lp->pc0);
          loop_pop(s, val);
          break;
        }
        struct loopinfo *lp = loop_push(s, LOOP_NORMAL);
        if (!val) lp->reg = -1;
        uint32_t pos0 = genjmp_0(s, OP_JMP);
        lp->pc0 = new_label(s);
        codegen(s, cast->predicate, VAL);
        pop();
        uint32_t pos = genjmp2_0(s, is_until ? OP_JMPIF : OP_JMPNOT, cursp(), NOVAL);
        lp->pc1 = new_label(s);
        genop_0(s, OP_NOP); /* for redo */
        dispatch(s, pos0);
        codegen(s, (mrc_node *)cast->statements, NOVAL);
        genjmp(s, OP_JMP, lp->pc0);
        dispatch(s, pos);
        loop_pop(s, val);
        break;
      }
      if (true_always(cast->predicate)) {
        if (nt == PM_UNTIL_NODE) {
          if (val) {
            genop_1(s, OP_LOADNIL, cursp());
            push();
          }
          goto exit;
        }
      }
      else if (false_always(cast->predicate)) {
        if (nt == PM_WHILE_NODE) {
          if (val) {
            genop_1(s, OP_LOADNIL, cursp());
            push();
          }
          goto exit;
        }
      }

      uint32_t pos = JMPLINK_START;
      struct loopinfo *lp = loop_push(s, LOOP_NORMAL);

      if (!val) lp->reg = -1;
      lp->pc0 = new_label(s);
      codegen(s, cast->predicate, VAL);
      pop();
      if (nt == PM_WHILE_NODE) {
        pos = genjmp2_0(s, OP_JMPNOT, cursp(), NOVAL);
      }
      else {
        pos = genjmp2_0(s, OP_JMPIF, cursp(), NOVAL);
      }
      lp->pc1 = new_label(s);
      genop_0(s, OP_NOP); /* for redo */
      codegen(s, (mrc_node *)cast->statements, NOVAL);
      genjmp(s, OP_JMP, lp->pc0);
      dispatch(s, pos);
      loop_pop(s, val);
      break;
    }
    case PM_FOR_NODE:
    {
      for_body(s, tree);
      if (val) push();
      break;
    }
    case PM_CASE_NODE:
    {
      CAST(case);
      int head = 0;
      uint32_t pos1, pos2, pos3, tmp;

      pos3 = JMPLINK_START;
      if (cast->predicate) {
        head = cursp();
        codegen(s, (mrc_node *)cast->predicate, VAL);
      }
      for (size_t i = 0; i < cast->conditions.size; i++) {
        pm_when_node_t *when = (pm_when_node_t *)cast->conditions.nodes[i];
        pos1 = pos2 = JMPLINK_START;
        for (size_t j = 0; j < when->conditions.size; j++) {
          mrc_node *cond = when->conditions.nodes[j];
          mrc_bool splat = FALSE;
          if (nint(cond) == PM_SPLAT_NODE) {
            splat = TRUE;
            codegen(s, (mrc_node *)((pm_splat_node_t *)cond)->expression, VAL);
          }
          else {
            codegen(s, cond, VAL);
          }
          if (head) {
            gen_move(s, cursp(), head, 0);
            push(); push(); pop(); pop(); pop();
            if (splat) {
              genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_SYM_1(__case_eqq)), 1);
            }
            else {
              genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_OPSYM_2(eqq)), 1);
            }
          }
          else {
            pop();
          }
          tmp = genjmp2(s, OP_JMPIF, cursp(), pos2, !head);
          pos2 = tmp;
        }
        pos1 = genjmp_0(s, OP_JMP);
        dispatch_linked(s, pos2);
        codegen(s, (mrc_node *)when->statements, val);
        if (val) pop();
        tmp = genjmp(s, OP_JMP, pos3);
        pos3 = tmp;
        dispatch(s, pos1);
      }
      if (cast->else_clause) {
        codegen(s, (mrc_node *)cast->else_clause, val);
        if (val) pop();
        tmp = genjmp(s, OP_JMP, pos3);
        pos3 = tmp;
      }
      if (val) {
        uint32_t pos = cursp();
        genop_1(s, OP_LOADNIL, pos);
        if (pos3 != JMPLINK_START) dispatch_linked(s, pos3);
        if (head) pop();
        if (cursp() != pos) {
          gen_move(s, cursp(), pos, 0);
        }
        push();
      }
      else {
        if (pos3 != JMPLINK_START) dispatch_linked(s, pos3);
        if (head) pop();
      }
      break;
    }
    case PM_MATCH_WRITE_NODE:
    {
      /* `regexp =~ string` whose regexp has named captures. mruby does not
         bind the named captures to local variables (the bison compiler does
         not either), so just emit the underlying =~ call and run the match. */
      CAST(match_write);
      codegen(s, (mrc_node *)cast->call, val);
      break;
    }
    case PM_MATCH_PREDICATE_NODE:
    {
      /* one-line `expr in pattern` -> true / false */
      CAST(match_predicate);
      int head = cursp();
      codegen(s, (mrc_node *)cast->value, VAL);
      uint32_t fail_pos = JMPLINK_START;
      codegen_pattern(s, (mrc_node *)cast->pattern, head, &fail_pos, -1);
      genop_1(s, OP_LOADTRUE, head);
      uint32_t done = genjmp(s, OP_JMP, JMPLINK_START);
      if (fail_pos != JMPLINK_START) dispatch_linked(s, fail_pos);
      genop_1(s, OP_LOADFALSE, head);
      dispatch(s, done);
      if (!val) pop();
      break;
    }
    case PM_MATCH_REQUIRED_NODE:
    {
      /* one-line `expr => pattern`: binds on match, raises
         NoMatchingPatternError otherwise; evaluates to nil. */
      CAST(match_required);
      int head = cursp();
      codegen(s, (mrc_node *)cast->value, VAL);
      uint32_t fail_pos = JMPLINK_START;
      codegen_pattern(s, (mrc_node *)cast->pattern, head, &fail_pos, -1);
      uint32_t ok = genjmp(s, OP_JMP, JMPLINK_START);
      if (fail_pos != JMPLINK_START) dispatch_linked(s, fail_pos);
      genop_1(s, OP_LOADFALSE, cursp());
      genop_1(s, OP_MATCHERR, cursp());
      dispatch(s, ok);
      pop();
      if (val) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      break;
    }
    case PM_CASE_MATCH_NODE:
    {
      CAST(case_match);
      int head = 0;
      uint32_t case_end_jumps = JMPLINK_START;
      uint32_t tmp;

      /* Optimization: detect if predicate is array literal */
      int known_array_len = -1;
      if (cast->predicate && nint(cast->predicate) == PM_ARRAY_NODE) {
        pm_array_node_t *arr = (pm_array_node_t *)cast->predicate;
        int has_splat = 0;
        for (size_t i = 0; i < arr->elements.size; i++) {
          if (nint(arr->elements.nodes[i]) == PM_SPLAT_NODE) {
            has_splat = 1;
            break;
          }
        }
        if (!has_splat) {
          known_array_len = arr->elements.size;
        }
      }

      /* Generate code for the case value */
      if (cast->predicate) {
        head = cursp();
        codegen(s, (mrc_node *)cast->predicate, VAL);
      }

      /* Iterate through in clauses */
      for (size_t i = 0; i < cast->conditions.size; i++) {
        pm_in_node_t *in_n = (pm_in_node_t *)cast->conditions.nodes[i];
        uint32_t fail_pos = JMPLINK_START;

        /* Generate pattern matching code */
        if (in_n->pattern) {
          codegen_pattern(s, (mrc_node *)in_n->pattern, head, &fail_pos, known_array_len);
        }

        /* Guard clauses on patterns are handled inside codegen_pattern (via PM_IF_NODE/PM_UNLESS_NODE wrappers) */

        /* Generate in-clause body */
        codegen(s, (mrc_node *)in_n->statements, val);
        if (val) pop();

        /* Jump to end of case/in */
        tmp = genjmp(s, OP_JMP, case_end_jumps);
        case_end_jumps = tmp;

        /* Dispatch fail jumps to next in-clause */
        if (fail_pos != JMPLINK_START) {
          dispatch_linked(s, fail_pos);
        }
      }

      /* Handle else clause */
      if (cast->else_clause) {
        codegen(s, (mrc_node *)cast->else_clause, val);
        if (val) pop();
      }
      else {
        /* No pattern matched: raise NoMatchingPatternError */
        genop_1(s, OP_LOADFALSE, cursp());
        genop_1(s, OP_MATCHERR, cursp());
      }

      /* Dispatch all end jumps */
      if (case_end_jumps != JMPLINK_START) {
        dispatch_linked(s, case_end_jumps);
      }

      if (val) {
        /* Move result to original case value position */
        if (head) {
          gen_move(s, head, cursp(), 0);
          pop();
        }
        push();
      }
      else {
        if (head) pop();
      }
      break;
    }
    case PM_SELF_NODE:
    {
      if (val) {
        genop_1(s, OP_LOADSELF, cursp());
        push();
      }
      break;
    }
    case PM_TRUE_NODE:
    {
      if (val) {
        genop_1(s, OP_LOADTRUE, cursp());
        push();
      }
      break;
    }
    case PM_FALSE_NODE:
    {
      if (val) {
        genop_1(s, OP_LOADFALSE, cursp());
        push();
      }
      break;
    }
    case PM_NIL_NODE:
    {
      if (val) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      break;
    }
    case PM_CONSTANT_PATH_NODE:
    {
      CAST(constant_path);
      int sym = new_sym(s, cast->name);
      if (cast->parent) {
        codegen(s, cast->parent, VAL);
        pop();
      }
      else { /* NODE_COLON3 */
        genop_1(s, OP_OCLASS, cursp());
      }
      genop_2(s, OP_GETMCNST, cursp(), sym);
      if (val) push();
      break;
    }
    case PM_CLASS_NODE:
    {
      int idx;
      CAST(class);
      mrc_node *cpath = (mrc_node *)cast->constant_path;
      switch (nint(cpath)) {
        case PM_CONSTANT_READ_NODE:
          {
            genop_1(s, OP_LOADNIL, cursp());
            push();
            break;
          }
        case PM_CONSTANT_PATH_NODE:
          {
            mrc_node *parent = ((pm_constant_path_node_t *)cpath)->parent;
            if (parent) {
              codegen(s, parent, VAL);
            }
            else { /* ::ClassName - root namespace */
              genop_1(s, OP_OCLASS, cursp());
              push();
            }
            break;
          }
        default:
          codegen_error(s, "Invalid constant path node");
      }

      if (cast->superclass) {
        codegen(s, cast->superclass, VAL);
      }
      else {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      pop(); pop();
      idx = new_sym(s, cast->name);
      genop_2(s, OP_CLASS, cursp(), idx);
      if (!cast->body) {
        genop_1(s, OP_LOADNIL, cursp());
      }
      else {
        idx = scope_body(s, tree, val);
        genop_2(s, OP_EXEC, cursp(), idx);
      }
      if (val) {
        push();
      }
      break;
    }
    case PM_MODULE_NODE:
    {
      int idx;
      CAST(module);
      switch (nint(cast->constant_path)) {
        case PM_CONSTANT_PATH_NODE:
          {
            CAST3(constant_path, cast->constant_path, cpath);
            if (cpath->parent) {
              codegen(s, cpath->parent, VAL);
            }
            else { /* ::ModuleName - root namespace */
              genop_1(s, OP_OCLASS, cursp());
              push();
            }
            break;
          }
        case PM_CONSTANT_READ_NODE:
          genop_1(s, OP_LOADNIL, cursp());
          push();
          break;
        default:
          codegen_error(s, "Invalid constant path node");
      }
      pop();
      idx = new_sym(s, cast->name);
      genop_2(s, OP_MODULE, cursp(), idx);
      if (!cast->body) {
        genop_1(s, OP_LOADNIL, cursp());
      }
      else {
        idx = scope_body(s, tree, val);
        genop_2(s, OP_EXEC, cursp(), idx);
      }
      if (val) {
        push();
      }
      break;
    }
    case PM_ALIAS_METHOD_NODE:
    {
      CAST(alias_method);
      CAST3(symbol, cast->new_name, new_name);
      CAST3(symbol, cast->old_name, old_name);
      int a = new_sym(s, nsym(s->c->p, new_name->unescaped.source, new_name->unescaped.length));
      int b = new_sym(s, nsym(s->c->p, old_name->unescaped.source, old_name->unescaped.length));
      genop_2(s, OP_ALIAS, a, b);
      if (val) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      break;
    }
    case PM_UNDEF_NODE:
    {
      CAST(undef);
      for (size_t i = 0; i < cast->names.size; i++) {
        CAST3(symbol, cast->names.nodes[i], name);
        int symbol = new_sym(s, nsym(s->c->p, name->unescaped.source, name->unescaped.length));
        genop_1(s, OP_UNDEF, symbol);
      }
      if (val) {
        genop_1(s, OP_LOADNIL, cursp());
        push();
      }
      break;
    }
    case PM_SUPER_NODE:
    {
      CAST(super);
      mrc_codegen_scope *s2 = s;
      int lv = 0;
      int n = 0, nk = 0, st = 0;

      push();
      while (!s2->mscope) {
        lv++;
        s2 = s2->prev;
        if (!s2) break;
      }
      CAST3(arguments, cast->arguments, arguments);
      if (arguments) {
        st = n = gen_values(s, (mrc_node *)arguments, VAL, 14);
        if (n < 0) {
          st = 1; n = 15;
          push();
        }
        /* keyword arguments */
        for (size_t i = 0; i < arguments->arguments.size; i++) {
          mrc_node *t = (mrc_node *)arguments->arguments.nodes[i];
          if (nint(t) == PM_KEYWORD_HASH_NODE) {
            nk = gen_hash(s, t, VAL, 14);
            if (nk < 0) {st++; nk = 15;}
            else st += nk*2;
            n |= nk<<4;
          }
        }
        /* block argument */
        if (cast->block) {
          codegen(s, (mrc_node *)cast->block, VAL);
        }
        else if (s2) gen_blkmove(s, s2->ainfo, lv);
        else {
          genop_1(s, OP_LOADNIL, cursp());
          push();
        }
      }
      else { /* `super()` parentheses without argument */
        /* block argument */
        if (cast->block) {
          codegen(s, (mrc_node *)cast->block, VAL);
        }
        else if (s2) gen_blkmove(s, s2->ainfo, lv);
        else {
          genop_1(s, OP_LOADNIL, cursp());
          push();
        }
      }
      st++;
      pop_n(st+1);
      genop_2(s, OP_SUPER, cursp(), n);
      if (val) push();
      break;
    }
    case PM_FORWARDING_SUPER_NODE:
    {
      CAST(forwarding_super);
      mrc_codegen_scope *s2 = s;
      int lv = 0;
      uint16_t ainfo = 0;
      int n = CALL_MAXARGS;
      int sp = cursp();

      push();        /* room for receiver */
      while (!s2->mscope) {
        lv++;
        s2 = s2->prev;
        if (!s2) break;
      }
      if (s2 && s2->ainfo > 0) {
        ainfo = s2->ainfo;
      }
      if (ainfo > 0) {
        genop_2S(s, OP_ARGARY, cursp(), (ainfo<<4)|(lv & 0xf));
        push(); push(); push();   /* ARGARY pushes 3 values at most */
        pop(); pop(); pop();
        /* keyword arguments */
        if (ainfo & 0x1) {
          n |= CALL_MAXARGS<<4;
          push();
        }
        /* block argument */
        if (cast->block) {
          push();
          codegen(s, (mrc_node *)cast->block, VAL);
        }
      }
      else {
        /* block argument */
        if (cast->block) {
          codegen(s, (mrc_node *)cast->block, VAL);
        }
        else {
          gen_blkmove(s, 0, lv);
        }
        n = 0;
      }
      s->sp = sp;
      genop_2(s, OP_SUPER, cursp(), n);
      if (val) push();
      break;
    }
    case PM_RETURN_NODE:
    {
      CAST(return);
      if (cast->arguments) {
        gen_retval(s, (mrc_node *)cast->arguments);
      }
      else {
        genop_1(s, OP_LOADNIL, cursp());
      }
      if (s->loop) {
        gen_return(s, OP_RETURN_BLK, cursp());
      }
      else {
        gen_return(s, OP_RETURN, cursp());
      }
      if (val) push();
      break;
    }
    case PM_YIELD_NODE:
    {
      CAST(yield);
      mrc_codegen_scope *s2 = s;
      int lv = 0, ainfo = -1;
      int n = 0, sendv = 0;

      while (!s2->mscope) {
        lv++;
        s2 = s2->prev;
        if (!s2) break;
      }
      if (s2) {
        ainfo = (int)s2->ainfo;
      }
      if (ainfo < 0) codegen_error(s, "invalid yield (SyntaxError)");
      push();
      if (cast->arguments) {
        n = gen_values(s, (mrc_node *)cast->arguments, VAL, 14);
        if (n < 0) {
          n = sendv = 1;
          push();
        }
      }
      push(); pop(); /* space for a block */
      pop_n(n+1);
      genop_2S(s, OP_BLKPUSH, cursp(), (ainfo<<4)|(lv & 0xf));
      if (sendv) n = CALL_MAXARGS;
      if (n < 15) {
        /* fast path: direct block call without method dispatch */
        genop_2(s, OP_BLKCALL, cursp(), n);
      }
      else {
        /* fallback: use SEND for splat */
        genop_3(s, OP_SEND, cursp(), new_sym(s, MRC_SYM_1(call)), n);
      }
      if (val) push();
      break;
    }
    case PM_BREAK_NODE:
    {
      CAST(break);
      loop_break(s, (mrc_node *)cast->arguments);
      if (val) push();
      break;
    }
    case PM_NEXT_NODE:
    {
      CAST(next);
      if (!s->loop) {
        raise_error(s, "unexpected next");
      }
      else if (s->loop->type == LOOP_NORMAL) {
        codegen(s, (mrc_node *)cast->arguments, NOVAL);
        genjmp(s, OP_JMPUW, s->loop->pc0);
      }
      else {
        if ((mrc_node *)cast->arguments) {
          codegen(s, (mrc_node *)cast->arguments, VAL);
          pop();
        }
        else {
          genop_1(s, OP_LOADNIL, cursp());
        }
        gen_return(s, OP_RETURN, cursp());
      }
      if (val) push();
      break;
    }
    case PM_REDO_NODE:
    {
      struct loopinfo *lp = s->loop;
      while (lp && (lp->type == LOOP_BEGIN || lp->type == LOOP_RESCUE)) {
        lp = lp->prev;
      }
      if (!lp) {
        raise_error(s, "unexpected redo");
      }
      else {
        genjmp(s, OP_JMPUW, lp->pc1);
      }
      if (val) push();
      break;
    }
    case PM_RETRY_NODE:
    {
      const char *msg = "unexpected retry";
      const struct loopinfo *lp = s->loop;
      while (lp && lp->type != LOOP_RESCUE) {
        lp = lp->prev;
      }
      if (!lp) {
        raise_error(s, msg);
      }
      else {
        genjmp(s, OP_JMPUW, lp->pc0);
      }
      if (val) push();
      break;
    }
    case PM_BEGIN_NODE:
    {
      CAST(begin);
      if (cast->rescue_clause == NULL && cast->else_clause == NULL &&
          cast->ensure_clause == NULL) {
        /* plain begin/end: no exception machinery, just the body. Emitting
           the rescue scaffold here left a dangling jump and an unbalanced
           loop entry that corrupted following code. */
        gen_begin(s, (mrc_node *)cast, val);
        break;
      }
      int noexc;
      uint32_t exend, pos1;
      struct loopinfo *lp;
      int catch_entry, begin, end;

      /* for ensure */
      int ensure_catch_entry = -1, ensure_begin = 0;
      if (cast->ensure_clause && cast->ensure_clause->statements) {
        ensure_catch_entry = catch_handler_new(s);
        ensure_begin = s->pc;
      }

      lp = loop_push(s, LOOP_BEGIN);
      lp->pc0 = new_label(s);
      catch_entry = catch_handler_new(s);
      begin = s->pc;
      /* begin */
      gen_begin(s, (mrc_node *)cast, VAL);
      pop();
      lp->type = LOOP_RESCUE;
      end = s->pc;
      noexc = genjmp_0(s, OP_JMP);
      catch_handler_set(s, catch_entry, MRC_CATCH_RESCUE, begin, end, s->pc);
      exend = JMPLINK_START;
      pos1 = JMPLINK_START;
      if (cast->rescue_clause) {
        int exc = cursp();
        genop_1(s, OP_EXCEPT, exc);
        push();
        /* rescue */
        gen_rescue(s, (mrc_node *)cast->rescue_clause, &pos1, &exc, &exend, val);
        if (pos1 != JMPLINK_START) {
          dispatch(s, pos1);
          genop_1(s, OP_RAISEIF, exc);
        }
      }
      pop();
      dispatch(s, noexc);
      if (cast->else_clause) {
        codegen(s, (mrc_node *)cast->else_clause, val);
      }
      else if (val) {
        push();
      }
      dispatch_linked(s, exend);
      loop_pop(s, NOVAL);

      /* ensure */
      if (cast->ensure_clause && cast->ensure_clause->statements) {
        /* When rescue is present with val=1, cursp is 1 higher than the no-rescue case.
         * Normalize before gen_ensure so that the exception register lands consistently. */
        if (cast->rescue_clause && val) pop();
        gen_ensure(s, (mrc_node *)cast->ensure_clause, ensure_catch_entry, ensure_begin);
      }
      else {
        /* empty ensure ignored */
      }
      break;
    }
    case PM_RESCUE_MODIFIER_NODE:
    {
      CAST(rescue_modifier);
      int catch_entry, begin_pos, end_pos;
      struct loopinfo *lp;

      lp = loop_push(s, LOOP_BEGIN);
      lp->pc0 = new_label(s);
      catch_entry = catch_handler_new(s);
      begin_pos = s->pc;

      /* evaluate main expression */
      codegen(s, cast->expression, val);
      if (val) pop();

      lp->type = LOOP_RESCUE;
      end_pos = s->pc;
      int noexc = genjmp_0(s, OP_JMP);
      catch_handler_set(s, catch_entry, MRC_CATCH_RESCUE, begin_pos, end_pos, s->pc);

      /* rescue expression - only catches StandardError */
      int exc = cursp();
      genop_1(s, OP_EXCEPT, exc);
      push();
      /* check if exception is StandardError */
      genop_2(s, OP_GETCONST, cursp(), new_sym(s, MRC_SYM_1(StandardError)));
      push();
      pop();
      genop_2(s, OP_RESCUE, exc, cursp());
      int rescue_jmp = genjmp2_0(s, OP_JMPIF, cursp(), val);
      /* not StandardError - re-raise */
      genop_1(s, OP_RAISEIF, exc);
      /* StandardError - execute rescue expression */
      dispatch(s, rescue_jmp);
      pop();
      codegen(s, cast->rescue_expression, val);
      if (val) pop();

      dispatch(s, noexc);
      if (val) push();
      loop_pop(s, NOVAL);
      break;
    }
    case PM_BLOCK_ARGUMENT_NODE:
    {
      CAST(block_argument);
      if (!cast->expression) {
        mrc_sym and = MRC_OPSYM_2(and);
        int idx = lv_idx(s, and);
        if (idx == 0) {
          int depth = search_upvar(s, and, &idx);
          gen_getupvar(s, cursp(), and, depth + 1);
        }
        else {
          gen_move(s, cursp(), idx, val);
        }
        if (val) push();
      }
      else {
        codegen(s, cast->expression, val);
      }
      break;
    }
    case PM_POST_EXECUTION_NODE:
    {
      mrc_diagnostic_list_append(s->c, tree->location.start, "END not supported", MRC_GENERATOR_ERROR);
      break;
    }
    case PM_RANGE_NODE:
    {
      CAST(range);
      codegen(s, cast->left, val);
      codegen(s, cast->right, val);
      if (val) {
        mrc_code op;
        if (cast->base.flags & PM_RANGE_FLAGS_EXCLUDE_END) {
          op = OP_RANGE_EXC;
        }
        else {
          op = OP_RANGE_INC;
        }
        pop(); pop();
        genop_1(s, op, cursp());
        push();
      }
      break;
    }
    case PM_SOURCE_FILE_NODE:
    {
      if (val) {
        CAST(source_file);
        char *p = (char *)cast->filepath.source;
        mrc_int len = cast->filepath.length;
        int off = new_lit_str(s, p, len);
        genop_2(s, OP_STRING, cursp(), off);
        push();
      }
      break;
    }
    case PM_SOURCE_LINE_NODE:
    {
      if (val) {
        int line = node_lineno(s->c, tree);
        gen_int(s, cursp(), (mrc_int)line);
        push();
      }
      break;
    }
    case PM_SOURCE_ENCODING_NODE:
    {
      genop_3(s, OP_SSEND, cursp(), new_sym(s, MRC_SYM_1(__ENCODING__)), 0);
      push();
      { // Workaround: increase nregs in case __ENCODING__ called alone
        // (maybe it is a useless use of a literal in void context)
        push();
        pop();
      }
      break;
    }
    case PM_FORWARDING_ARGUMENTS_NODE:
    {
      //CAST(forwarding_arguments);
      if (val) {
        int idx;
        genop_1(s, OP_LOADNIL, cursp());
        push();
        // *
        idx = lv_idx(s, MRC_OPSYM_2(mul));
        assert(idx != 0);
        gen_move(s, cursp(), idx, val);
        pop();
        genop_1(s, OP_ARYCAT, cursp());
        push();
        // **
        genop_2(s, OP_HASH, cursp(), 0);
        push();
        idx = lv_idx(s, MRC_OPSYM_2(pow));
        assert(idx != 0);
        gen_move(s, cursp(), idx, val);
        pop();
        genop_1(s, OP_HASHCAT, cursp());
        push();
        // &
        idx = lv_idx(s, MRC_OPSYM_2(and));
        assert(idx != 0);
        gen_move(s, cursp(), idx, val);
      }
      break;
    }
    case PM_DEFINED_NODE:
    {
      CAST(defined);
      push();
      codegen(s, cast->value, VAL);
      pop();
      pop();
      genop_3(s, OP_SSEND, cursp(), new_sym(s, MRC_SYM_2(defined_p)), 1);
      push();
      break;
    }
    default:
    {
      char buf[256];
      snprintf(buf, sizeof(buf), "Not implemented: %s", pm_node_type_to_str(nt));
      codegen_error(s, buf);
      break;
    }
  }
 exit:
  s->rlev = rlev;
}
