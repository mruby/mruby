#include <stdio.h>
#include "../include/mrc_ccontext.h"
#include "../include/mrc_irep.h"
#include "../include/mrc_dump.h"
#include "../include/mrc_opcode.h"
#include "../include/mrc_parser_util.h"
#include "../include/mrc_pool.h"
#include "../include/mrc_debug.h"
#include "../include/mrc_irep_pool_type.h"
#include <inttypes.h>

const char *
mrc_sym_dump(mrc_ccontext *c, mrc_sym sym)
{
  mrc_int lenp;
  const char *name = mrc_sym_name_len(c, sym, &lenp);
  if (!name) {
    return NULL;
  }
  if (lenp < 0) {
    return NULL;
  }
  size_t len = (size_t)lenp;
  if (len == SIZE_MAX) {
    return NULL;
  }
  char *buf = (char*)mrc_pool_alloc(c->pool, len + 1);
  if (!buf) {
    return NULL;
  }
  memcpy(buf, name, len);
  buf[len] = '\0';
  return buf;
}

static inline const struct mrc_irep_catch_handler *
mrc_irep_catch_handler_table(const mrc_irep *irep)
{
  if (irep->clen > 0) {
    return (const struct mrc_irep_catch_handler*)(irep->iseq + irep->ilen);
  }
  else {
    return (const struct mrc_irep_catch_handler*)NULL;
  }
}

#ifndef MRC_NO_STDIO
static void
print_r(mrc_ccontext *c, const mrc_irep *irep, size_t n, FILE *out)
{
  if (n == 0) return;
  if (n >= irep->nlocals) return;
  if (!irep->lv[n-1]) return;
  fprintf(out, " R%d:%s", (int)n, mrc_sym_dump(c, irep->lv[n-1]));
}

static void
print_lv_a(mrc_ccontext *c, const mrc_irep *irep, uint16_t a, FILE *out)
{
  if (!irep->lv || a >= irep->nlocals || a == 0) {
    fprintf(out, "\n");
    return;
  }
  fprintf(out, "\t;");
  print_r(c, irep, a, out);
  fprintf(out, "\n");
}

static void
print_lv_ab(mrc_ccontext *c, const mrc_irep *irep, uint16_t a, uint16_t b, FILE *out)
{
  if (!irep->lv || (a >= irep->nlocals && b >= irep->nlocals) || a+b == 0) {
    fprintf(out, "\n");
    return;
  }
  fprintf(out, "\t;");
  if (a > 0) print_r(c, irep, a, out);
  if (b > 0) print_r(c, irep, b, out);
  fprintf(out, "\n");
}

static void
print_header(mrc_ccontext *c, const mrc_irep *irep, ptrdiff_t i, FILE *out)
{
  int32_t line;

  mrc_assert(i <= UINT32_MAX);
  line = mrc_debug_get_line(c, irep, (uint32_t)i);
  if (line < 0) {
    fprintf(out, "      ");
  }
  else {
    fprintf(out, "%5d ", line);
  }

  fprintf(out, "%03d ", (int)i);
}

static void
print_args(uint16_t i, FILE *out)
{
  mrc_assert(i <= 255);
  uint8_t n = i&0xf;
  uint8_t nk = (i>>4)&0xf;

  if (n == 15) {
    fprintf(out, "n=*");
  }
  else {
    fprintf(out, "n=%d", n);
  }
  if (nk > 0) {
    fprintf(out, "|");
    if (nk == 15) {
      fprintf(out, "nk=*");
    }
    else {
      fprintf(out, "nk=%d", nk);
    }
  }
  fprintf(out, "\n");
}

#define CASE(insn,ops) case insn: FETCH_ ## ops (); L_ ## insn

static void
codedump(mrc_ccontext *c, const mrc_irep *irep, FILE *out)
{
//  int ai;
  const mrc_code *pc, *pcend;
  mrc_code ins;
  const char *file = NULL, *next_file;

  if (!irep) return;
  fprintf(out, "irep %p nregs=%d nlocals=%d pools=%d syms=%d reps=%d ilen=%d\n", (void*)irep,
          irep->nregs, irep->nlocals, (int)irep->plen, (int)irep->slen, (int)irep->rlen, (int)irep->ilen);

  if (irep->lv) {
    int head = FALSE;

    for (int i = 1; i < irep->nlocals; i++) {
      char const *s = mrc_sym_dump(c, irep->lv[i - 1]);
      if (s) {
        if (!head) {
          head = TRUE;
          fprintf(out, "local variable names:\n");
        }
        fprintf(out, "  R%d:%s\n", i, s);
      }
    }
  }

  if (irep->clen > 0) {
    const struct mrc_irep_catch_handler *e = mrc_irep_catch_handler_table(irep);

    for (int i = irep->clen; i > 0; i--,e++) {
      uint32_t begin = mrc_irep_catch_handler_unpack(e->begin);
      uint32_t end = mrc_irep_catch_handler_unpack(e->end);
      uint32_t target = mrc_irep_catch_handler_unpack(e->target);
      char buf[20];
      const char *type;

      switch (e->type) {
        case MRC_CATCH_RESCUE:
          type = "rescue";
          break;
        case MRC_CATCH_ENSURE:
          type = "ensure";
          break;
        default:
          buf[0] = '\0';
          snprintf(buf, sizeof(buf), "0x%02x <unknown>", (int)e->type);
          type = buf;
          break;
      }
      fprintf(out, "catch type: %-8s begin: %04" PRIu32 " end: %04" PRIu32 " target: %04" PRIu32 "\n", type, begin, end, target);
    }
  }

  pc = irep->iseq;
  pcend = pc + irep->ilen;
  while (pc < pcend) {
    ptrdiff_t i;
    uint32_t a;
    uint16_t b;
    uint16_t cc;

    int ai = mrc_gc_arena_save(c);

    i = pc - irep->iseq;
    next_file = mrc_debug_get_filename(c, irep, (uint32_t)i);
    if (next_file && file != next_file) {
      fprintf(out, "file: %s\n", next_file);
      file = next_file;
    }
    print_header(c, irep, i, out);
    ins = READ_B();
    switch (ins) {
    CASE(OP_NOP, Z):
      fprintf(out, "NOP\n");
      break;
    CASE(OP_MOVE, BB):
      fprintf(out, "MOVE\t\tR%d\tR%d\t", a, b);
      print_lv_ab(c, irep, a, b, out);
      break;

    CASE(OP_LOADL, BB):
      switch (irep->pool[b].tt) {
#ifndef MRC_NO_FLOAT
      case IREP_TT_FLOAT:
        fprintf(out, "LOADL\t\tR%d\tL[%d]\t; %f", a, b, (double)irep->pool[b].u.f);
        break;
#endif
      case IREP_TT_INT32:
        fprintf(out, "LOADL\t\tR%d\tL[%d]\t; %" PRId32, a, b, irep->pool[b].u.i32);
        break;
#ifdef MRC_64BIT
      case IREP_TT_INT64:
        fprintf(out, "LOADL\t\tR%d\tL[%d]\t; %" PRId64, a, b, irep->pool[b].u.i64);
        break;
#endif
      default:
        fprintf(out, "LOADL\t\tR%d\tL[%d]\t", a, b);
        break;
      }
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADI8, BB):
      fprintf(out, "LOADI8\t\tR%d\t%d\t", a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADINEG, BB):
      fprintf(out, "LOADINEG\tR%d\t-%d\t", a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADI16, BS):
      fprintf(out, "LOADI16\tR%d\t%d\t", a, (int)(int16_t)b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADI32, BSS):
      fprintf(out, "LOADI32\tR%d\t%d\t", a, (int32_t)(((uint32_t)b<<16)+cc));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADI__1, B):
      fprintf(out, "LOADI__1\tR%d\t(-1)\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADI_0, B): goto L_LOADI;
    CASE(OP_LOADI_1, B): goto L_LOADI;
    CASE(OP_LOADI_2, B): goto L_LOADI;
    CASE(OP_LOADI_3, B): goto L_LOADI;
    CASE(OP_LOADI_4, B): goto L_LOADI;
    CASE(OP_LOADI_5, B): goto L_LOADI;
    CASE(OP_LOADI_6, B): goto L_LOADI;
    CASE(OP_LOADI_7, B):
    L_LOADI:
      b = ins-(int)OP_LOADI_0;
      fprintf(out, "LOADI_%d\tR%d\t(%d)\t", b, a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADSYM, BB):
      fprintf(out, "LOADSYM\tR%d\t:%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADNIL, B):
      fprintf(out, "LOADNIL\tR%d\t(nil)\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADSELF, B):
      fprintf(out, "LOADSELF\tR%d\t(R0)\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADTRUE, B):
      fprintf(out, "LOADTRUE\tR%d\t(true)\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LOADFALSE, B):
      fprintf(out, "LOADFALSE\tR%d\t(false)\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_GETGV, BB):
      fprintf(out, "GETGV\t\tR%d\t%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SETGV, BB):
      fprintf(out, "SETGV\t\t%s\tR%d\t", mrc_sym_dump(c, irep->syms[b]), a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_GETSV, BB):
      fprintf(out, "GETSV\t\tR%d\t%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SETSV, BB):
      fprintf(out, "SETSV\t\t%s\tR%d\t", mrc_sym_dump(c, irep->syms[b]), a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_GETCONST, BB):
      fprintf(out, "GETCONST\tR%d\t%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SETCONST, BB):
      fprintf(out, "SETCONST\t%s\tR%d\t", mrc_sym_dump(c, irep->syms[b]), a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_GETMCNST, BB):
      fprintf(out, "GETMCNST\tR%d\tR%d::%s\t", a, a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SETMCNST, BB):
      fprintf(out, "SETMCNST\tR%d::%s\tR%d\t", a+1, mrc_sym_dump(c, irep->syms[b]), a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_GETIV, BB):
      fprintf(out, "GETIV\t\tR%d\t%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SETIV, BB):
      fprintf(out, "SETIV\t\t%s\tR%d\t", mrc_sym_dump(c, irep->syms[b]), a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_GETUPVAR, BBB):
      fprintf(out, "GETUPVAR\tR%d\t%d\t%d\t", a, b, cc);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SETUPVAR, BBB):
      fprintf(out, "SETUPVAR\tR%d\t%d\t%d\t", a, b, cc);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_GETCV, BB):
      fprintf(out, "GETCV\t\tR%d\t%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SETCV, BB):
      fprintf(out, "SETCV\t\t%s\tR%d\t", mrc_sym_dump(c, irep->syms[b]), a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_GETIDX, B):
      fprintf(out, "GETIDX\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_GETIDX0, BB):
      fprintf(out, "GETIDX0\tR%d\tR%d[0]\n", a, b);
      break;
    CASE(OP_SETIDX, B):
      fprintf(out, "SETIDX\tR%d\tR%d\tR%d\n", a, a+1, a+2);
      break;
    CASE(OP_JMP, S):
      i = pc - irep->iseq;
      fprintf(out, "JMP\t\t%03d\n", (int)i+(int16_t)a);
      break;
    CASE(OP_JMPUW, S):
      i = pc - irep->iseq;
      fprintf(out, "JMPUW\t\t%03d\n", (int)i+(int16_t)a);
      break;
    CASE(OP_JMPIF, BS):
      i = pc - irep->iseq;
      fprintf(out, "JMPIF\t\tR%d\t%03d\t", a, (int)i+(int16_t)b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_JMPNOT, BS):
      i = pc - irep->iseq;
      fprintf(out, "JMPNOT\tR%d\t%03d\t", a, (int)i+(int16_t)b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_JMPNIL, BS):
      i = pc - irep->iseq;
      fprintf(out, "JMPNIL\tR%d\t%03d\t", a, (int)i+(int16_t)b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SSEND, BBB):
      fprintf(out, "SSEND\t\tR%d\t:%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_args(cc, out);
      break;
    CASE(OP_SSEND0, BB):
      fprintf(out, "SSEND0\tR%d\t:%s\n", a, mrc_sym_dump(c, irep->syms[b]));
      break;
    CASE(OP_SSENDB, BBB):
      fprintf(out, "SSENDB\tR%d\t:%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_args(cc, out);
      break;
    CASE(OP_SEND, BBB):
      fprintf(out, "SEND\t\tR%d\t:%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_args(cc, out);
      break;
    CASE(OP_SEND0, BB):
      fprintf(out, "SEND0\t\tR%d\t:%s\n", a, mrc_sym_dump(c, irep->syms[b]));
      break;
    CASE(OP_SENDB, BBB):
      fprintf(out, "SENDB\t\tR%d\t:%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_args(cc, out);
      break;
    CASE(OP_CALL, Z):
      fprintf(out, "CALL\n");
      break;
    CASE(OP_BLKCALL, BB):
      fprintf(out, "BLKCALL\t\tR%d\t%d\n", a, b);
      break;
    CASE(OP_SUPER, BB):
      fprintf(out, "SUPER\t\tR%d\t", a);
      print_args(b, out);
      break;
    CASE(OP_ARGARY, BS):
      fprintf(out, "ARGARY\tR%d\t%d:%d:%d:%d (%d)\t", a,
             (b>>11)&0x3f,
             (b>>10)&0x1,
             (b>>5)&0x1f,
             (b>>4)&0x1,
             (b>>0)&0xf);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_ENTER, W):
      fprintf(out, "ENTER\t\t%d:%d:%d:%d:%d:%d:%d:%d (0x%x)\n",
              MRC_ASPEC_NOBLOCK(a),
              MRC_ASPEC_REQ(a),
              MRC_ASPEC_OPT(a),
              MRC_ASPEC_REST(a),
              MRC_ASPEC_POST(a),
              MRC_ASPEC_KEY(a),
              MRC_ASPEC_KDICT(a),
              MRC_ASPEC_BLOCK(a), a);
      break;
    CASE(OP_KEY_P, BB):
      fprintf(out, "KEY_P\t\tR%d\t:%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_KEYEND, Z):
      fprintf(out, "KEYEND\n");
      break;
    CASE(OP_KARG, BB):
      fprintf(out, "KARG\t\tR%d\t:%s\t", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_RETURN, B):
      fprintf(out, "RETURN\tR%d\t\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_RETURN_BLK, B):
      fprintf(out, "RETURN_BLK\tR%d\t\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_RETSELF, Z):
      fprintf(out, "RETSELF\n");
      break;
    CASE(OP_RETNIL, Z):
      fprintf(out, "RETNIL\n");
      break;
    CASE(OP_RETTRUE, Z):
      fprintf(out, "RETTRUE\n");
      break;
    CASE(OP_RETFALSE, Z):
      fprintf(out, "RETFALSE\n");
      break;
    CASE(OP_BREAK, B):
      fprintf(out, "BREAK\t\tR%d\t\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_BLKPUSH, BS):
      fprintf(out, "BLKPUSH\tR%d\t%d:%d:%d:%d (%d)\t", a,
             (b>>11)&0x3f,
             (b>>10)&0x1,
             (b>>5)&0x1f,
             (b>>4)&0x1,
             (b>>0)&0xf);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_LAMBDA, BB):
      fprintf(out, "LAMBDA\tR%d\tI[%d]\n", a, b);
      break;
    CASE(OP_BLOCK, BB):
      fprintf(out, "BLOCK\t\tR%d\tI[%d]\n", a, b);
      break;
    CASE(OP_METHOD, BB):
      fprintf(out, "METHOD\tR%d\tI[%d]\n", a, b);
      break;
    CASE(OP_RANGE_INC, B):
      fprintf(out, "RANGE_INC\tR%d\n", a);
      break;
    CASE(OP_RANGE_EXC, B):
      fprintf(out, "RANGE_EXC\tR%d\n", a);
      break;
    CASE(OP_DEF, BB):
      fprintf(out, "DEF\t\tR%d\t:%s\n", a, mrc_sym_dump(c, irep->syms[b]));
      break;
    CASE(OP_TDEF, BBB):
      fprintf(out, "TDEF\t\tR%d\t:%s\tI[%d]\n", a, mrc_sym_dump(c, irep->syms[b]), cc);
      break;
    CASE(OP_SDEF, BBB):
      fprintf(out, "SDEF\t\tR%d\t:%s\tI[%d]\n", a, mrc_sym_dump(c, irep->syms[b]), cc);
      break;
    CASE(OP_UNDEF, B):
      fprintf(out, "UNDEF\t\t:%s\n", mrc_sym_dump(c, irep->syms[a]));
      break;
    CASE(OP_ALIAS, BB):
      fprintf(out, "ALIAS\t\t:%s\t%s\n", mrc_sym_dump(c, irep->syms[a]), mrc_sym_dump(c, irep->syms[b]));
      break;
    CASE(OP_ADD, B):
      fprintf(out, "ADD\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_ADDI, BB):
      fprintf(out, "ADDI\t\tR%d\t%d\t", a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SUB, B):
      fprintf(out, "SUB\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_SUBI, BB):
      fprintf(out, "SUBI\t\tR%d\t%d\t", a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_ADDILV, BBB):
      fprintf(out, "ADDILV\tR%d\tR%d\t%d", a, b, cc);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SUBILV, BBB):
      fprintf(out, "SUBILV\tR%d\tR%d\t%d", a, b, cc);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_MUL, B):
      fprintf(out, "MUL\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_DIV, B):
      fprintf(out, "DIV\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_LT, B):
      fprintf(out, "LT\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_LE, B):
      fprintf(out, "LE\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_GT, B):
      fprintf(out, "GT\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_GE, B):
      fprintf(out, "GE\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_EQ, B):
      fprintf(out, "EQ\t\tR%d\tR%d\n", a, a+1);
      break;
    CASE(OP_ARRAY, BB):
      fprintf(out, "ARRAY\t\tR%d\tR%d\t%d", a, a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_ARRAY2, BBB):
      fprintf(out, "ARRAY\t\tR%d\tR%d\t%d", a, b, cc);
      print_lv_ab(c, irep, a, b, out);
      break;
    CASE(OP_ARYCAT, B):
      fprintf(out, "ARYCAT\tR%d\tR%d\t", a, a+1);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_ARYPUSH, BB):
      fprintf(out, "ARYPUSH\tR%d\t%d\t", a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_ARYSPLAT, B):
      fprintf(out, "ARYSPLAT\tR%d\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_AREF, BBB):
      fprintf(out, "AREF\t\tR%d\tR%d\t%d", a, b, cc);
      print_lv_ab(c, irep, a, b, out);
      break;
    CASE(OP_ASET, BBB):
      fprintf(out, "ASET\t\tR%d\tR%d\t%d", a, b, cc);
      print_lv_ab(c, irep, a, b, out);
      break;
    CASE(OP_APOST, BBB):
      fprintf(out, "APOST\t\tR%d\t%d\t%d", a, b, cc);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_INTERN, B):
      fprintf(out, "INTERN\tR%d\t\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SYMBOL, BB):
      mrc_assert((irep->pool[b].tt&IREP_TT_NFLAG)==0);
      fprintf(out, "SYMBOL\tR%d\tL[%d]\t; %s", a, b, irep->pool[b].u.str);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_STRING, BB):
      mrc_assert((irep->pool[b].tt&IREP_TT_NFLAG)==0);
      fprintf(out, "STRING\tR%d\tL[%d]\t; %s", a, b, irep->pool[b].u.str);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_STRCAT, B):
      fprintf(out, "STRCAT\tR%d\tR%d\t", a, a+1);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_HASH, BB):
      fprintf(out, "HASH\t\tR%d\t%d\t", a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_HASHADD, BB):
      fprintf(out, "HASHADD\tR%d\t%d\t", a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_HASHCAT, B):
      fprintf(out, "HASHCAT\tR%d\tR%d\t", a, a+1);
      print_lv_a(c, irep, a, out);
      break;

    CASE(OP_OCLASS, B):
      fprintf(out, "OCLASS\tR%d\t\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_CLASS, BB):
      fprintf(out, "CLASS\t\tR%d\t:%s", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_MODULE, BB):
      fprintf(out, "MODULE\tR%d\t:%s", a, mrc_sym_dump(c, irep->syms[b]));
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_EXEC, BB):
      fprintf(out, "EXEC\t\tR%d\tI[%d]", a, b);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_SCLASS, B):
      fprintf(out, "SCLASS\tR%d\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_TCLASS, B):
      fprintf(out, "TCLASS\tR%d\t\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_ERR, B):
      if ((irep->pool[a].tt & IREP_TT_NFLAG) == 0) {
        fprintf(out, "ERR\t\t%s\n", irep->pool[a].u.str);
      }
      else {
        fprintf(out, "ERR\tL[%d]\n", a);
      }
      break;
    CASE(OP_EXCEPT, B):
      fprintf(out, "EXCEPT\tR%d\t\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_RESCUE, BB):
      fprintf(out, "RESCUE\tR%d\tR%d", a, b);
      print_lv_ab(c, irep, a, b, out);
      break;
    CASE(OP_RAISEIF, B):
      fprintf(out, "RAISEIF\tR%d\t\t", a);
      print_lv_a(c, irep, a, out);
      break;
    CASE(OP_MATCHERR, B):
      fprintf(out, "MATCHERR\tR%d\n", a);
      break;

    CASE(OP_DEBUG, BBB):
      fprintf(out, "DEBUG\t\t%d\t%d\t%d\n", a, b, cc);
      break;

    CASE(OP_STOP, Z):
      fprintf(out, "STOP\n");
      break;

    CASE(OP_EXT1, Z):
      fprintf(out, "EXT1\n");
      print_header(c, irep, pc-irep->iseq, out);
      ins = READ_B();
      switch (ins) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _1 (); goto L_OP_ ## i;
#include "mrc_ops.h"
#undef OPCODE
      }
      break;
    CASE(OP_EXT2, Z):
      fprintf(out, "EXT2\n");
      print_header(c, irep, pc-irep->iseq, out);
      ins = READ_B();
      switch (ins) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _2 (); goto L_OP_ ## i;
#include "mrc_ops.h"
#undef OPCODE
      }
      break;
    CASE(OP_EXT3, Z):
      fprintf(out, "EXT3\n");
      print_header(c, irep, pc-irep->iseq, out);
      ins = READ_B();
      switch (ins) {
#define OPCODE(i,x) case OP_ ## i: FETCH_ ## x ## _3 (); goto L_OP_ ## i;
#include "mrc_ops.h"
#undef OPCODE
      }
      break;

    default:
      fprintf(out, "unknown_op (0x%x)\n", ins);
      break;
    }
    mrc_gc_arena_restore(c, ai);
  }
  fprintf(out, "\n");
}

static void
codedump_recur(mrc_ccontext *c, const mrc_irep *irep, FILE *out)
{
  codedump(c, irep, out);
  if (irep->reps) {
    for (int i=0; i<irep->rlen; i++) {
      codedump_recur(c, irep->reps[i], out);
    }
  }
}

void
mrc_codedump_all_file(mrc_ccontext *c, mrc_irep *irep, FILE *out)
{
  c->pool = mrc_pool_open(c);
  codedump_recur(c, irep, out);
  fflush(out);
  mrc_pool_close(c->pool);
}

#endif // MRC_NO_STDIO

void
mrc_codedump_all(mrc_ccontext *c, mrc_irep *irep)
{
#ifndef MRC_NO_STDIO
  mrc_codedump_all_file(c, irep, stdout);
#endif
}
