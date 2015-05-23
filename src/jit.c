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

#include <string.h>


#if defined(__linux__)
#  if defined(__x86_64__)
#    define MRB_JIT_PAGE_BASE ((uint8_t *)(INT32_MAX))
#    if defined(MRB_NAN_BOXING)
#      include "jit/x86_64-unknown-linux-gnu-nan_boxing/ops.h"
#    elif defined(MRB_WORD_BOXING)
#      include "jit/x86_64-unknown-linux-gnu-word_boxing/ops.h"
#    else
#      include "jit/x86_64-unknown-linux-gnu-no_boxing/ops.h"
#    endif
#  elif defined(__i386)
#    if defined(MRB_NAN_BOXING)
#      include "jit/x86-unknown-linux-gnu-nan_boxing/ops.h"
#    elif defined(MRB_WORD_BOXING)
#      include "jit/x86-unknown-linux-gnu-word_boxing/ops.h"
#    else
#      include "jit/x86-unknown-linux-gnu-no_boxing/ops.h"
#    endif
#  endif
#else
#  error Platform not yet supported
#endif

//#define JIT_DEBUG

#ifdef JIT_DEBUG
#define JIT_PRINTF(...) fprintf(stderr, __VA_ARGS__)
#else
#define JIT_PRINTF(...)
#endif

#define ALIGN(s, a) (((s) + (a) - 1) & ~((a) - 1))

#ifndef MRB_JIT_PAGE_MAP_SIZE
#define MRB_JIT_PAGE_MAP_SIZE 2048
#endif

#define MRB_JIT_PAGE_FIND_MAX_ATTEMPTS 32

typedef enum {
  PAGE_MARK_UNKOWN,
  PAGE_MARK_FREE,
  PAGE_MARK_ALLOCED
} page_mark_t;

static uint8_t page_marks[MRB_JIT_PAGE_MAP_SIZE];

static void
mark_page(uint8_t *addr, size_t size, size_t page_size, page_mark_t mark) {
  size_t n_pages = size / page_size;
  int i = (MRB_JIT_PAGE_BASE - addr) / (page_size);
  int j;

  JIT_PRINTF("marking pages %d - %ld as %d\n", i, i + n_pages, mark);
  for(j = 0; j < n_pages; j++) {
     page_marks[i + j] = mark;
  }

  for(i = 0; i < MRB_JIT_PAGE_MAP_SIZE; i++) {
    if(page_marks[i] == PAGE_MARK_ALLOCED) {
      JIT_PRINTF("X");
    }
    else if(page_marks[i] == PAGE_MARK_FREE) {
      JIT_PRINTF(".");
    } else {
      JIT_PRINTF("?");
    }
  }
  JIT_PRINTF("\n");
}

static uint8_t *
find_page(size_t size, size_t page_size)
{
  size_t n_pages = size / page_size;
  int i;

  JIT_PRINTF("finding free page\n");
  for(i = 0; i < MRB_JIT_PAGE_MAP_SIZE; i++) {
    if(page_marks[i] == PAGE_MARK_ALLOCED) {
      JIT_PRINTF("X");
    }
    else if(page_marks[i] == PAGE_MARK_FREE) {
      JIT_PRINTF(".");
    } else {
      JIT_PRINTF("?");
    }
  }
  JIT_PRINTF("\n");


  for(i = 0; i < MRB_JIT_PAGE_MAP_SIZE; i++) {
    if(page_marks[i] != PAGE_MARK_ALLOCED) {
      int j;
      for(j = 1; j < n_pages; j++) {
        if(page_marks[i + j] == PAGE_MARK_ALLOCED) {
          i += j + 1;
          goto outer;
        }
      }
      goto found;
    }
outer:;
  }

  JIT_PRINTF("no page found!!\n");
  return NULL;

found:
  {
    JIT_PRINTF("found free page: %d - %ld\n", i, i + n_pages);
    return (uint8_t *) (MRB_JIT_PAGE_BASE - i * page_size);
  }
}


#if !defined(_WIN32) && \
    (defined(__unix__) || defined(__unix) ||\
    (defined(__APPLE__) && defined(__MACH__)))
#include <sys/mman.h>
#include <unistd.h>
#include <alloca.h>
#include <stdlib.h>
#include <errno.h>

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
  unsigned tries = 0;

retry:
  addr = find_page(size, page_size);
  if(!addr) {
    fprintf(stderr, "JIT: no free address range for page found. Consider increasing MRB_JIT_PAGE_MAP_SIZE\n");
    abort();
  }

  JIT_PRINTF( "allocating page of size %zu (text:%zu, rodata: %zu) (at %p)\n", size, text_size,rodata_size ,addr);

  mem = mmap(addr, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);

  ctx->text = mem;
  ctx->rodata = mem + text_size;

  JIT_PRINTF("allocated page at %p\n", ctx->text);
  ctx->text_size = text_size;
  ctx->rodata_size = rodata_size;
  ctx->size = size;

  if(mem != MAP_FAILED) {
    if((mem + size) >= (uint8_t *)INT32_MAX) {
      tries++;
      if (tries >= MRB_JIT_PAGE_FIND_MAX_ATTEMPTS) {
        fprintf(stderr, "JIT: allocating memory for SCM failed (requested %p, receviced %p).\n", addr, mem);
        abort();
      } else {
        JIT_PRINTF("got invalid page %p, retrying...\n", mem);
        mark_page(addr, size, page_size, PAGE_MARK_ALLOCED);
        munmap(mem, size);

        goto retry;
      }
    }
    else {
      mark_page(ctx->text, ctx->size, page_size, PAGE_MARK_ALLOCED);
    }
    return TRUE;
  }

  return FALSE;
}

static void
jit_ctx_free(mrb_state *mrb, struct mrb_jit_ctx *ctx)
{
  size_t page_size = jit_page_size();
  JIT_PRINTF("deallocating page of size %zu (%ld pages)\n", ctx->size, ctx->size / page_size );
  if(munmap(ctx->text, ctx->size) < 0) {
    fprintf(stderr, "munmap failed: %s\n", strerror(errno));
  } else {
    mark_page(ctx->text, ctx->size, page_size, PAGE_MARK_FREE);
  }
}

static void
jit_ctx_prot_exec(struct mrb_jit_ctx *ctx)
{
  mprotect(ctx->text, ctx->text_size, PROT_READ | PROT_EXEC);
}
#endif

typedef void (*op_func_t)(void *);

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
      JIT_PRINTF("jump: adding extra %zu\n", jump_size(irep->iseq[i], 0, TRUE));
      next_off += jump_size(irep->iseq[i], 0, TRUE);
    }
    tbl[i + 1] = next_off;
  }

  relax_text_off_tbl(irep, tbl);

  irep->jit_ctx.text_off_tbl = tbl;
  return tbl[irep->ilen];
}


mrb_bool
mrb_jit_release(mrb_state *mrb, mrb_irep *irep) {
  if (MRB_IREP_JITTED_P(irep)) {
    jit_ctx_free(mrb, &irep->jit_ctx);
    return TRUE;
  }

  return FALSE;
}

static mrb_bool
mrb_jit_prepare(mrb_state *mrb, mrb_irep *irep)
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

    JIT_PRINTF("need %zu bytes for jit code (%zu for the final return)\n", text_size, return_size());
  }

  return TRUE;
}

void
mrb_irep_codedump(mrb_state *mrb, mrb_irep *irep);

static mrb_bool
mrb_jit_compile(mrb_state *mrb, mrb_irep *irep)
{
  if (MRB_IREP_JITTED_P(irep)) {
    return TRUE;
  }
  else {
    unsigned i  = 0;
    struct mrb_jit_ctx *ctx;
    int32_t *text_off_tbl;
    int32_t *rodata_off_tbl;

    if(!mrb_jit_prepare(mrb, irep)) {
      return FALSE;
    }

    JIT_PRINTF( "jitting irep %p\n", irep);

#ifdef JIT_DEBUG
    mrb_irep_codedump(mrb, irep);
#endif

    ctx = &irep->jit_ctx;
    text_off_tbl = ctx->text_off_tbl;
    rodata_off_tbl = ctx->rodata_off_tbl;

    for (i = 0; i < irep->ilen; i++) {
      mrb_code c = irep->iseq[i];
      int opcode = GET_OPCODE(c);
      int32_t off =  text_off_tbl[i];
      int32_t rodata_off = rodata_off_tbl[i];

      JIT_PRINTF( "copying %dth opcode:%s (%d) to offset %d (%zu bytes) (addr: %p/%p)\n", i, op_names[opcode], opcode, off, op_sizes_text[opcode], ctx->text + off, (void *)(intptr_t)off);


      memcpy(ctx->text + off, ops_text[opcode], op_sizes_text[opcode]);
      memcpy(ctx->rodata + rodata_off, ops_rodata[opcode], op_sizes_rodata[opcode]);

      /* link code */
      link_funcs[opcode](ctx->text + off, ctx->rodata + rodata_off, irep->iseq + i);

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
        JIT_PRINTF( "jump to (%d / %d) %p\n", op_off, jit_off, jmp_off + jit_off);
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

mrb_bool
mrb_jit_enter(mrb_state *mrb, struct mrb_irep *irep, void *ctx, mrb_code *pc)
{
  if(MRB_UNLIKELY(!MRB_IREP_JITTED_P(irep))) {
    if(!mrb_jit_compile(mrb, irep)) {
      return FALSE;
    }
  }

  JIT_PRINTF("JIT: entering irep: %p at %p (%ld) %d\n", irep, pc, pc - irep->iseq, GET_OPCODE(*pc));
  MRB_JIT_CALL(irep, pc, ctx);

  /* never reached */
  return TRUE;
}
#endif
