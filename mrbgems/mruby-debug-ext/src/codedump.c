#define MRB_DEBUG_DUMP_FUNCTIONS

#include "src/opcode.h"
#include "debug-ext.h"

#include "mruby.h"
#include "mruby/compile.h"
#include "mruby/irep.h"
#include "mruby/proc.h"
#include "mruby/string.h"

static void
codedump(mrb_state *mrb, mrb_irep *irep)
{
  int i;
  int ai;
  mrb_code c;
  FILE* out;

  out = mrb_debug_output(mrb);
  if(!out) {
    return;
  }

  if (!irep) return;
  fprintf(out, "irep %p nregs=%d nlocals=%d pools=%d syms=%d reps=%d\n", irep,
         irep->nregs, irep->nlocals, (int)irep->plen, (int)irep->slen, (int)irep->rlen);

  mrb_assert(irep->ilen <= INT_MAX);
  for (i = 0; i < (int)(irep->ilen); i++) {
    ai = mrb_gc_arena_save(mrb);
    fprintf(out, "%03d ", i);
    c = irep->iseq[i];
    switch (GET_OPCODE(c)) {
    case OP_NOP:
      fprintf(out, "OP_NOP\n");
      break;
    case OP_MOVE:
      fprintf(out, "OP_MOVE\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_LOADL:
      fprintf(out, "OP_LOADL\tR%d\tL(%d)\n", GETARG_A(c), GETARG_Bx(c));
      break;
    case OP_LOADI:
      fprintf(out, "OP_LOADI\tR%d\t%d\n", GETARG_A(c), GETARG_sBx(c));
      break;
    case OP_LOADSYM:
      fprintf(out, "OP_LOADSYM\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_LOADNIL:
      fprintf(out, "OP_LOADNIL\tR%d\n", GETARG_A(c));
      break;
    case OP_LOADSELF:
      fprintf(out, "OP_LOADSELF\tR%d\n", GETARG_A(c));
      break;
    case OP_LOADT:
      fprintf(out, "OP_LOADT\tR%d\n", GETARG_A(c));
      break;
    case OP_LOADF:
      fprintf(out, "OP_LOADF\tR%d\n", GETARG_A(c));
      break;
    case OP_GETGLOBAL:
      fprintf(out, "OP_GETGLOBAL\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETGLOBAL:
      fprintf(out, "OP_SETGLOBAL\t:%s\tR%d\n",
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_GETCONST:
      fprintf(out, "OP_GETCONST\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETCONST:
      fprintf(out, "OP_SETCONST\t:%s\tR%d\n",
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_GETMCNST:
      fprintf(out, "OP_GETMCNST\tR%d\tR%d::%s\n", GETARG_A(c), GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETMCNST:
      fprintf(out, "OP_SETMCNST\tR%d::%s\tR%d\n", GETARG_A(c)+1,
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_GETIV:
      fprintf(out, "OP_GETIV\tR%d\t%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETIV:
      fprintf(out, "OP_SETIV\t%s\tR%d\n",
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_GETUPVAR:
      fprintf(out, "OP_GETUPVAR\tR%d\t%d\t%d\n",
             GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_SETUPVAR:
      fprintf(out, "OP_SETUPVAR\tR%d\t%d\t%d\n",
             GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_GETCV:
      fprintf(out, "OP_GETCV\tR%d\t%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]));
      break;
    case OP_SETCV:
      fprintf(out, "OP_SETCV\t%s\tR%d\n",
             mrb_sym2name(mrb, irep->syms[GETARG_Bx(c)]),
             GETARG_A(c));
      break;
    case OP_JMP:
      fprintf(out, "OP_JMP\t\t%03d\n", i+GETARG_sBx(c));
      break;
    case OP_JMPIF:
      fprintf(out, "OP_JMPIF\tR%d\t%03d\n", GETARG_A(c), i+GETARG_sBx(c));
      break;
    case OP_JMPNOT:
      fprintf(out, "OP_JMPNOT\tR%d\t%03d\n", GETARG_A(c), i+GETARG_sBx(c));
      break;
    case OP_SEND:
      fprintf(out, "OP_SEND\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_SENDB:
      fprintf(out, "OP_SENDB\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_TAILCALL:
      fprintf(out, "OP_TAILCALL\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_SUPER:
      fprintf(out, "OP_SUPER\tR%d\t%d\n", GETARG_A(c),
             GETARG_C(c));
      break;
    case OP_ARGARY:
      fprintf(out, "OP_ARGARY\tR%d\t%d:%d:%d:%d\n", GETARG_A(c),
             (GETARG_Bx(c)>>10)&0x3f,
             (GETARG_Bx(c)>>9)&0x1,
             (GETARG_Bx(c)>>4)&0x1f,
             (GETARG_Bx(c)>>0)&0xf);
      break;

    case OP_ENTER:
      fprintf(out, "OP_ENTER\t%d:%d:%d:%d:%d:%d:%d\n",
             (GETARG_Ax(c)>>18)&0x1f,
             (GETARG_Ax(c)>>13)&0x1f,
             (GETARG_Ax(c)>>12)&0x1,
             (GETARG_Ax(c)>>7)&0x1f,
             (GETARG_Ax(c)>>2)&0x1f,
             (GETARG_Ax(c)>>1)&0x1,
             GETARG_Ax(c) & 0x1);
      break;
    case OP_RETURN:
      fprintf(out, "OP_RETURN\tR%d", GETARG_A(c));
      switch (GETARG_B(c)) {
      case OP_R_NORMAL:
        fprintf(out, "\n"); break;
      case OP_R_RETURN:
        fprintf(out, "\treturn\n"); break;
      case OP_R_BREAK:
        fprintf(out, "\tbreak\n"); break;
      default:
        fprintf(out, "\tbroken\n"); break;
        break;
      }
      break;
    case OP_BLKPUSH:
      fprintf(out, "OP_BLKPUSH\tR%d\t%d:%d:%d:%d\n", GETARG_A(c),
             (GETARG_Bx(c)>>10)&0x3f,
             (GETARG_Bx(c)>>9)&0x1,
             (GETARG_Bx(c)>>4)&0x1f,
             (GETARG_Bx(c)>>0)&0xf);
      break;

    case OP_LAMBDA:
      fprintf(out, "OP_LAMBDA\tR%d\tI(%+d)\t%d\n", GETARG_A(c), GETARG_b(c)+1, GETARG_c(c));
      break;
    case OP_RANGE:
      fprintf(out, "OP_RANGE\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_METHOD:
      fprintf(out, "OP_METHOD\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
      break;

    case OP_ADD:
      fprintf(out, "OP_ADD\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_ADDI:
      fprintf(out, "OP_ADDI\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_SUB:
      fprintf(out, "OP_SUB\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_SUBI:
      fprintf(out, "OP_SUBI\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_MUL:
      fprintf(out, "OP_MUL\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_DIV:
      fprintf(out, "OP_DIV\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_LT:
      fprintf(out, "OP_LT\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_LE:
      fprintf(out, "OP_LE\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_GT:
      fprintf(out, "OP_GT\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_GE:
      fprintf(out, "OP_GE\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;
    case OP_EQ:
      fprintf(out, "OP_EQ\tR%d\t:%s\t%d\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]),
             GETARG_C(c));
      break;

    case OP_STOP:
      fprintf(out, "OP_STOP\n");
      break;

    case OP_ARRAY:
      fprintf(out, "OP_ARRAY\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_ARYCAT:
      fprintf(out, "OP_ARYCAT\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_ARYPUSH:
      fprintf(out, "OP_ARYPUSH\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_AREF:
      fprintf(out, "OP_AREF\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_APOST:
      fprintf(out, "OP_APOST\tR%d\t%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    case OP_STRING:
      {
        mrb_value v = irep->pool[GETARG_Bx(c)];
        mrb_value s = mrb_str_dump(mrb, mrb_str_new(mrb, RSTRING_PTR(v), RSTRING_LEN(v)));
        fprintf(out, "OP_STRING\tR%d\t%s\n", GETARG_A(c), RSTRING_PTR(s));
      }
      break;
    case OP_STRCAT:
      fprintf(out, "OP_STRCAT\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_HASH:
      fprintf(out, "OP_HASH\tR%d\tR%d\t%d\n", GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;

    case OP_OCLASS:
      fprintf(out, "OP_OCLASS\tR%d\n", GETARG_A(c));
      break;
    case OP_CLASS:
      fprintf(out, "OP_CLASS\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
      break;
    case OP_MODULE:
      fprintf(out, "OP_MODULE\tR%d\t:%s\n", GETARG_A(c),
             mrb_sym2name(mrb, irep->syms[GETARG_B(c)]));
      break;
    case OP_EXEC:
      fprintf(out, "OP_EXEC\tR%d\tI(%+d)\n", GETARG_A(c), GETARG_Bx(c)+1);
      break;
    case OP_SCLASS:
      fprintf(out, "OP_SCLASS\tR%d\tR%d\n", GETARG_A(c), GETARG_B(c));
      break;
    case OP_TCLASS:
      fprintf(out, "OP_TCLASS\tR%d\n", GETARG_A(c));
      break;
    case OP_ERR:
      {
        mrb_value v = irep->pool[GETARG_Bx(c)];
        mrb_value s = mrb_str_dump(mrb, mrb_str_new(mrb, RSTRING_PTR(v), RSTRING_LEN(v)));
        fprintf(out, "OP_ERR\t%s\n", RSTRING_PTR(s));
      }
      break;
    case OP_EPUSH:
      fprintf(out, "OP_EPUSH\t:I(%+d)\n", GETARG_Bx(c)+1);
      break;
    case OP_ONERR:
      fprintf(out, "OP_ONERR\t%03d\n", i+GETARG_sBx(c));
      break;
    case OP_RESCUE:
      fprintf(out, "OP_RESCUE\tR%d\n", GETARG_A(c));
      break;
    case OP_RAISE:
      fprintf(out, "OP_RAISE\tR%d\n", GETARG_A(c));
      break;
    case OP_POPERR:
      fprintf(out, "OP_POPERR\t%d\n", GETARG_A(c));
      break;
    case OP_EPOP:
      fprintf(out, "OP_EPOP\t%d\n", GETARG_A(c));
      break;

    default:
      fprintf(out, "OP_unknown %d\t%d\t%d\t%d\n", GET_OPCODE(c),
             GETARG_A(c), GETARG_B(c), GETARG_C(c));
      break;
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  fprintf(out, "\n");
}

static void
codedump_recur(mrb_state *mrb, mrb_irep *irep)
{
  size_t i;

  codedump(mrb, irep);
  for (i=0; i<irep->rlen; i++) {
    codedump_recur(mrb, irep->reps[i]);
  }
}

void
mrb_codedump_all(mrb_state *mrb, struct RProc *proc)
{
  codedump_recur(mrb, proc->body.irep);
}
