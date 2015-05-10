/*
** jit.c - JIT related code
**
** See Copyright Notice in mruby.h
*/

#ifdef MRB_ENABLE_JIT
#define _DEFAULT_SOURCE

#include "mruby/jit.h"
#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/opcode.h"

#ifndef DEBUG
#define DEBUG(x)
#endif

#include <string.h>

#if defined(__linux__)
#if defined(__x86_64__)
#if defined(MRB_NAN_BOXING)
#include "jit/ops_x86_64-unknown-linux-gnu-nan_boxing.h"
#elif defined(MRB_WORD_BOXING)
#include "jit/ops_x86_64-unknown-linux-gnu-word_boxing.h"
#else
#include "jit/ops_x86_64-unknown-linux-gnu-no_boxing.h"
#endif
#elif defined(__i386)
#if defined(MRB_NAN_BOXING)
#include "jit/ops_x86-unknown-linux-gnu-nan_boxing.h"
#elif defined(MRB_WORD_BOXING)
#include "jit/ops_x86-unknown-linux-gnu-word_boxing.h"
#else
#include "jit/ops_x86-unknown-linux-gnu-no_boxing.h"
#endif
#endif
#else
#error Platform not yet supported
#endif

#if !defined(_WIN32) && \
    (defined(__unix__) || defined(__unix) ||\
    (defined(__APPLE__) && defined(__MACH__)))
#include <sys/mman.h>
#include <unistd.h>
#include <alloca.h>
#include <stdlib.h>

#define ALIGN(s, a) (((s) + (a) - 1) & ~((a) - 1))
#define JIT_PRINTF(...) fprintf(stderr, __VA_ARGS__)
//#define JIT_PRINTF

static size_t
jit_page_size()
{
  return sysconf(_SC_PAGESIZE);
}

static mrb_bool
jit_ctx_alloc(mrb_state *mrb, struct mrb_jit_ctx *ctx, size_t text_size, size_t rodata_size)
{
  size_t page_size = jit_page_size();
  size_t size = ALIGN(text_size + rodata_size, page_size);
  uint8_t *addr, *mem;

  JIT_PRINTF( "page counter is %d | page size is %d\n", mrb->jit_page_counter, page_size);
  addr = (uint8_t *) ALIGN(INT32_MAX - (500 - mrb->jit_page_counter++) * size, page_size);
  JIT_PRINTF( "allocating page of size %d (text:%d, rodata: %d) (at %p)\n", size, text_size,rodata_size ,addr);

  mem = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);

  ctx->text = mem;
  ctx->rodata = mem + text_size;

  JIT_PRINTF("allocated page at %p\n", ctx->text);
  ctx->text_size = text_size;
  ctx->rodata_size = rodata_size;
  ctx->size = size;

  mrb_assert((mem + size) < (uint8_t *)INT32_MAX);

  return mem != MAP_FAILED;
}

static void
jit_ctx_prot_exec(struct mrb_jit_ctx *ctx)
{
  mprotect(ctx->text, ctx->text_size, PROT_READ | PROT_EXEC);
}
#endif

void
mrb_irep_jit_call(mrb_state *mrb, mrb_irep *irep, void *ctx)
{
  void (*f)(void *) = (void *)irep->jit_ctx.text;
  (*f)(ctx);
}

static size_t
jump_size(mrb_code c, int off, int force_rel16)
{
  uint8_t buf[64];
  switch(GET_OPCODE(c)) {
    case OP_JMPNOT:
      /* needs je, i.e. jump if equals zero, which means value is false */
      return jit_jump_if(buf, off, force_rel16) - buf;
    case OP_JMPIF:
      return jit_jump_not(buf, off, force_rel16) - buf;
    case OP_JMP:
      return jit_jump(buf, off, force_rel16) - buf;
    default:
      mrb_assert(0 && "invalid jump opcode");
      return 0;
  }
}

static size_t
return_size()
{
  uint8_t buf[64];
  return jit_return(buf) - buf;
}

static mrb_bool
is_jump(int opcode)
{
  return opcode == OP_JMPNOT || opcode == OP_JMPIF || opcode == OP_JMP;
}

static void
relax_text_off_tbl(mrb_irep *irep, int32_t *tbl)
{
  int i;
  mrb_bool rerelax = FALSE;

  tbl[0] = 0;

  for(i = 0; i < irep->ilen; i++) {
    mrb_code c = irep->iseq[i];
    int opcode = GET_OPCODE(c);

    if (is_jump(opcode)) {
      int op_off = GETARG_sBx(c);
      int cur_jmp_size = tbl[i + 1] - tbl[i];
      int new_jmp_size = op_sizes_text[opcode];
      int jmp_off;

      if(op_off >= 0) {
        jmp_off = tbl[i + op_off] - tbl[i + 1];
        new_jmp_size += jump_size(c, jmp_off, FALSE);
      }
      else {
        int off_wo_self = tbl[i] - tbl[i + op_off];
        int jmp_size_wo_self = jump_size(c, off_wo_self, FALSE);
        int off_w_self = off_wo_self + jmp_size_wo_self;
        int jmp_size_w_self = jump_size(c, off_w_self, FALSE);

        if(jmp_size_wo_self == jmp_size_w_self) {
          new_jmp_size += jmp_size_w_self;
          jmp_off = off_w_self;
        }
        else {
          jmp_off = off_wo_self + jmp_size_w_self;
          new_jmp_size += jump_size(c, jmp_off, FALSE);
        }
      }

      if(cur_jmp_size > new_jmp_size) {
        int diff = cur_jmp_size - new_jmp_size;
        int j;
        JIT_PRINTF("jumping %d (%d ops) can be relaxed from %d to %d\n", jmp_off, op_off, cur_jmp_size, new_jmp_size);
        for(j = i + 1; j <= irep->ilen; j++) {
          tbl[j] -= diff;
        }
        rerelax = TRUE;
      }

    }
  }
  if(rerelax) {
    JIT_PRINTF("rerelaxing\n");
    relax_text_off_tbl(irep, tbl);
  }

  JIT_PRINTF("/relaxing\n");
}

static int32_t
build_rodata_off_tbl(mrb_state *mrb, mrb_irep *irep)
{
  int i;
  int32_t *tbl = (int32_t *) mrb_malloc(mrb, (irep->ilen + 1) * sizeof(int32_t));
  uint8_t *rodata = irep->jit_ctx.rodata;

  for(i = 0; i < irep->ilen; i++) {
    mrb_code c = irep->iseq[i];
    int opcode = GET_OPCODE(c);
    size_t size = op_sizes_rodata[opcode];
    uint8_t align = op_algn_rodata[opcode];

    if(align > 1) {
      rodata = (uint8_t *) ALIGN((uintptr_t)rodata, align);
    }

    tbl[i] = rodata - irep->jit_ctx.rodata;

    rodata += size;
  }

  tbl[irep->ilen] = rodata - irep->jit_ctx.rodata;

  irep->jit_ctx.rodata_off_tbl = tbl;
  return tbl[irep->ilen];
}

static int32_t
build_text_off_tbl(mrb_state *mrb, mrb_irep *irep)
{
  int i;
  int32_t *tbl = (int32_t *) mrb_malloc(mrb, (irep->ilen + 1) * sizeof(int32_t));

  tbl[0] = 0;

  for(i = 0; i < irep->ilen; i++) {
    mrb_code c = irep->iseq[i];
    int opcode = GET_OPCODE(c);
    int32_t next_off = tbl[i] + op_sizes_text[opcode];
    if (is_jump(opcode)) {
      JIT_PRINTF("jump: adding extra %d\n", jump_size(irep->iseq[i], 0, TRUE));
      next_off += jump_size(irep->iseq[i], 0, TRUE);
    }
    tbl[i + 1] = next_off;
  }

  relax_text_off_tbl(irep, tbl);

  irep->jit_ctx.text_off_tbl = tbl;
  return tbl[irep->ilen];
}

static mrb_bool
mrb_irep_jit_prepare(mrb_state *mrb, mrb_irep *irep)
{
  if (MRB_IREP_JITTED_P(irep)) {
    return TRUE;
  }
  else {
    size_t text_size = 0;
    size_t rodata_size = 0;

    init_ops();

    text_size = build_text_off_tbl(mrb, irep) + return_size();
    rodata_size = build_rodata_off_tbl(mrb, irep);

    jit_ctx_alloc(mrb, &irep->jit_ctx, text_size, rodata_size);

    JIT_PRINTF( "need %d bytes for jit code (%d for the final return)\n", text_size, return_size());
/*
    off = op_sizes_text[OP_ENTER];
    proc->jit_oa_off[0] = off;
    for(i = 1; i < irep->oalen; i++) {
      uint16_t off = 0;
      int j;
      for(j = 1; j < irep->oa_off[i]; j++) {
        mrb_code c = irep->iseq[j];
        off += op_sizes_text[GET_OPCODE(c)];
      }
      proc->jit_oa_off[i] = off;
    }

    for(i = 0; i < irep->oalen; i++) {
      DEBUG(JIT_PRINTF( "op_enter offsets: %d -> %d (%d)\n", i, proc->jit_oa_off[i], proc->jit_oa_off[i] - base));
    }
    */
  }

  return TRUE;
}
void
mrb_irep_codedump(mrb_state *mrb, mrb_irep *irep);

mrb_bool
mrb_irep_jit(mrb_state *mrb, mrb_irep *irep)
{
  if (MRB_IREP_JITTED_P(irep)) {
    return TRUE;
  }
  else {
    unsigned i  = 0;
    struct mrb_jit_ctx *ctx;
    int32_t *text_off_tbl;
    int32_t *rodata_off_tbl;
    //int ilen = irep->ilen;
    //mrb_code *iseq = irep->iseq;

    if(!mrb_irep_jit_prepare(mrb, irep)) {
      return FALSE;
    }

    JIT_PRINTF( "jitting irep %p\n", irep);
    mrb_irep_codedump(mrb, irep);
    
    ctx = &irep->jit_ctx;
    text_off_tbl = ctx->text_off_tbl;
    rodata_off_tbl = ctx->rodata_off_tbl;

    for (i = 0; i < irep->ilen; i++) {
      mrb_code c = irep->iseq[i];
      int opcode = GET_OPCODE(c);
      int32_t off =  text_off_tbl[i];
      int32_t rodata_off = rodata_off_tbl[i];

      JIT_PRINTF( "copying %dth opcode:%s (%d) to offset %d (%d bytes) (addr: %p/%p)\n", i, op_names[opcode], opcode, off, op_sizes_text[opcode], ctx->text + off, off);


      memcpy(ctx->text + off, ops_text[opcode], op_sizes_text[opcode]);
      memcpy(ctx->rodata + rodata_off, ops_rodata[opcode], op_sizes_rodata[opcode]);

      /* link code */
      link_funcs[opcode](ctx->text + off, ctx->rodata + rodata_off, c);

      /* set operands */
      //arg_funcs[opcode](ctx->text + off, c, i);

      if(opcode == OP_RESCUE) {
        JIT_PRINTF( "jitting op_rescue: %d\n", GETARG_A(c));
      } else if(opcode == OP_SEND) {
        JIT_PRINTF( "jitting op_send: %d %d %d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      }

      if (opcode == OP_JMPNOT || opcode == OP_JMPIF || opcode == OP_JMP) {
        int op_off = GETARG_sBx(c);
        uint8_t *jmp_off;
        int jit_off = text_off_tbl[i + op_off] - text_off_tbl[i + 1];

        (void) jmp_off;

        if (opcode == OP_JMPNOT) {
          jmp_off = jit_jump_if(ctx->text + off + op_sizes_text[opcode], jit_off, FALSE);
        }
        else if(opcode == OP_JMPIF){
          jmp_off = jit_jump_not(ctx->text + off + op_sizes_text[opcode], jit_off, FALSE);
        }
        else {
          jmp_off = jit_jump(ctx->text + off + op_sizes_text[opcode], jit_off, FALSE);
        }
        JIT_PRINTF( "jump to (%d / %ld) %p\n", op_off, jit_off, jmp_off + jit_off);
      }
    }

    JIT_PRINTF( "inserting final ret: to offset %d (addr: %p)\n",  text_off_tbl[i], ctx->text + text_off_tbl[i]);
    jit_return(ctx->text + text_off_tbl[i]);

    for (i = 0; i < text_off_tbl[irep->ilen] + 1; i++)
    {
      if (i > 0) JIT_PRINTF(" ");
      JIT_PRINTF("%02X", ctx->text[i]);
    }
    JIT_PRINTF("\n");

    irep->flags |= MRB_IREP_JITTED;
    jit_ctx_prot_exec(ctx);

    return TRUE;
  }
}
#endif
