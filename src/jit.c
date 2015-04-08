/*
** jit.c - JIT related code
**
** See Copyright Notice in mruby.h
*/

#define _DEFAULT_SOURCE

#include "mruby/jit.h"
#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/opcode.h"

#ifndef DEBUG
#define DEBUG(x)
#endif

#include <string.h>
#include "ops_x64.h"

#if !defined(_WIN32) && (defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__)))
#include <sys/mman.h>
#include <unistd.h>

static size_t
jit_page_size()
{
  return sysconf(_SC_PAGESIZE);
}

static mrb_bool
jit_page_alloc(struct mrb_jit_page *page, size_t size)
{
  size_t page_size = jit_page_size();
  size = (size + page_size - 1) & ~(page_size - 1);

  fprintf(stderr, "allocating page of size %d\n", size);

  page->data = mmap(NULL, size, PROT_READ | PROT_WRITE, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);
  page->size = size;

  return TRUE;
}

static void
jit_page_prot_exec(struct mrb_jit_page *page)
{
  mprotect(page->data, page->size, PROT_READ | PROT_EXEC);
}
#endif

void
mrb_irep_jit_call(mrb_state *mrb, mrb_irep *irep, void *ctx)
{
  void (*f)(void *) = (void *)irep->jit_page.data;
  return (*f)(ctx);
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
relax_off_tbl(mrb_irep *irep, int32_t *tbl)
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
      int new_jmp_size = op_sizes[opcode];
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
        fprintf(stderr,"jumping %d (%d ops) can be relaxed from %d to %d\n", jmp_off, op_off, cur_jmp_size, new_jmp_size);
        for(j = i + 1; j <= irep->ilen; j++) {
          tbl[j] -= diff;
        }
        rerelax = TRUE;
      }

    }
  }
  if(rerelax) {
    fprintf(stderr,"rerelaxing\n");
    relax_off_tbl(irep, tbl);
  }

  fprintf(stderr,"/relaxing\n");
}

static int32_t
build_off_tbl(mrb_state *mrb, mrb_irep *irep)
{
  int i;
  int32_t *tbl = (int32_t *) mrb_malloc(mrb, (irep->ilen + 1) * sizeof(int32_t));

  tbl[0] = 0;

  for(i = 0; i < irep->ilen; i++) {
    mrb_code c = irep->iseq[i];
    int opcode = GET_OPCODE(c);
    int32_t next_off = tbl[i] + op_sizes[opcode];
    if (is_jump(opcode)) {
      fprintf(stderr,"jump: adding extra %d\n", jump_size(irep->iseq[i], 0, TRUE));
      next_off += jump_size(irep->iseq[i], 0, TRUE);
    }
    tbl[i + 1] = next_off;
  }

  relax_off_tbl(irep, tbl);

  irep->jit_page.off_tbl = tbl;
  return tbl[irep->ilen];
}

static mrb_bool
mrb_irep_jit_prepare(mrb_state *mrb, mrb_irep *irep)
{
  if (MRB_IREP_JITTED_P(irep)) {
    return TRUE;
  }
  else {
    size_t size = 0;

    init_ops();

    size = build_off_tbl(mrb, irep) + return_size();
    jit_page_alloc(&irep->jit_page, size);

    fprintf(stderr, "need %d bytes for jit code (%d for the final return)\n", size, return_size());
/*
    off = op_sizes[OP_ENTER];
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
      DEBUG(fprintf(stderr, "op_enter offsets: %d -> %d (%d)\n", i, proc->jit_oa_off[i], proc->jit_oa_off[i] - base));
    }
    */
  }

  return TRUE;
}

mrb_bool
mrb_irep_jit(mrb_state *mrb, mrb_irep *irep)
{
  if (MRB_IREP_JITTED_P(irep)) {
    return TRUE;
  }
  else {
    unsigned i  = 0;
    struct mrb_jit_page *page;
    int32_t *off_tbl;

    if(!mrb_irep_jit_prepare(mrb, irep)) {
      return FALSE;
    }

    fprintf(stderr, "jitting irep %p\n", irep);
    page = &irep->jit_page;
    off_tbl = page->off_tbl;

    for (i = 0; i < irep->ilen; i++) {
      mrb_code c = irep->iseq[i];
      int opcode = GET_OPCODE(c);
      int32_t off =  off_tbl[i];

      fprintf(stderr, "copying opcode:%s (%d) to offset %d (%d bytes) (addr: %p/%p)\n", op_names[opcode], opcode, off, op_sizes[opcode], page->data + off, off);


      memcpy(page->data + off, ops[opcode], op_sizes[opcode]);

      arg_funcs[opcode](page->data + off, c);

      if (opcode == OP_JMPNOT || opcode == OP_JMPIF || opcode == OP_JMP) {
        int op_off = GETARG_sBx(c);
        uint8_t *jmp_off;
        int jit_off = off_tbl[i + op_off] - off_tbl[i + 1];

        (void) jmp_off;

        if (opcode == OP_JMPNOT) {
          jmp_off = jit_jump_if(page->data + off + op_sizes[opcode], jit_off, FALSE);
        }
        else if(opcode == OP_JMPIF){
          jmp_off = jit_jump_not(page->data + off + op_sizes[opcode], jit_off, FALSE);
        }
        else {
          jmp_off = jit_jump(page->data + off + op_sizes[opcode], jit_off, FALSE);
        }
        fprintf(stderr, "jump to (%d / %ld) %p\n", op_off, jit_off, jmp_off + jit_off);
      }
    }

    fprintf(stderr, "inserting final ret: to offset %d (addr: %p)\n",  off_tbl[i], page->data + off_tbl[i]);
    jit_return(page->data + off_tbl[i]);

    for (i = 0; i < off_tbl[irep->ilen] + 1; i++)
    {
      if (i > 0) fprintf(stderr," ");
      fprintf(stderr,"%02X", page->data[i]);
    }
    fprintf(stderr,"\n");

    irep->flags |= MRB_IREP_JITTED;
    jit_page_prot_exec(page);

    return TRUE;
  }
}
