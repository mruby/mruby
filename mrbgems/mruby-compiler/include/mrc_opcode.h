/**
** @file mrc_opcode.h - RiteVM operation codes (compiler-side view)
**
** See Copyright Notice in mruby.h
**
** This header deliberately has no include guard of its own: everything
** below is idempotent, and re-including it must re-establish the
** compiler's FETCH_* macros (which decode into a/b/cc) after mruby's
** <mruby/opcode.h> may have defined the a/b/c flavor. The enum and the
** definitions shared verbatim with <mruby/opcode.h> are protected by
** MRUBY_OPCODE_H so both headers can coexist in one translation unit
** (e.g. the amalgamated build).
*/

/* Instruction operand fetchers. Unlike <mruby/opcode.h> these decode the
   third operand into `cc`, not `c`: in compiler sources `c` is the
   conventional name of the mrc_ccontext parameter. Redefine
   unconditionally so this header wins at every include site. */
#undef FETCH_Z
#undef FETCH_B
#undef FETCH_BB
#undef FETCH_BBB
#undef FETCH_BS
#undef FETCH_BSS
#undef FETCH_S
#undef FETCH_W

#define FETCH_Z() /* nothing */
#define FETCH_B() do {a=READ_B();} while (0)
#define FETCH_BB() do {a=READ_B(); b=READ_B();} while (0)
#define FETCH_BBB() do {a=READ_B(); b=READ_B(); cc=READ_B();} while (0)
#define FETCH_BS() do {a=READ_B(); b=READ_S();} while (0)
#define FETCH_BSS() do {a=READ_B(); b=READ_S(); cc=READ_S();} while (0)
#define FETCH_S() do {a=READ_S();} while (0)
#define FETCH_W() do {a=READ_W();} while (0)

/* with OP_EXT1 (1st 16bit) */
#undef FETCH_Z_1
#undef FETCH_B_1
#undef FETCH_BB_1
#undef FETCH_BBB_1
#undef FETCH_BS_1
#undef FETCH_BSS_1
#undef FETCH_S_1
#undef FETCH_W_1

#define FETCH_Z_1() FETCH_Z()
#define FETCH_B_1() FETCH_S()
#define FETCH_BB_1() do {a=READ_S(); b=READ_B();} while (0)
#define FETCH_BBB_1() do {a=READ_S(); b=READ_B(); cc=READ_B();} while (0)
#define FETCH_BS_1() do {a=READ_S(); b=READ_S();} while (0)
#define FETCH_BSS_1() do {a=READ_S(); b=READ_S();cc=READ_S();} while (0)
#define FETCH_S_1() FETCH_S()
#define FETCH_W_1() FETCH_W()

/* with OP_EXT2 (2nd 16bit) */
#undef FETCH_Z_2
#undef FETCH_B_2
#undef FETCH_BB_2
#undef FETCH_BBB_2
#undef FETCH_BS_2
#undef FETCH_BSS_2
#undef FETCH_S_2
#undef FETCH_W_2

#define FETCH_Z_2() FETCH_Z()
#define FETCH_B_2() FETCH_B()
#define FETCH_BB_2() do {a=READ_B(); b=READ_S();} while (0)
#define FETCH_BBB_2() do {a=READ_B(); b=READ_S(); cc=READ_B();} while (0)
#define FETCH_BS_2() FETCH_BS()
#define FETCH_BSS_2() FETCH_BSS()
#define FETCH_S_2() FETCH_S()
#define FETCH_W_2() FETCH_W()

/* with OP_EXT3 (1st & 2nd 16bit) */
#undef FETCH_Z_3
#undef FETCH_B_3
#undef FETCH_BB_3
#undef FETCH_BBB_3
#undef FETCH_BS_3
#undef FETCH_BSS_3
#undef FETCH_S_3
#undef FETCH_W_3

#define FETCH_Z_3() FETCH_Z()
#define FETCH_B_3() FETCH_B()
#define FETCH_BB_3() do {a=READ_S(); b=READ_S();} while (0)
#define FETCH_BBB_3() do {a=READ_S(); b=READ_S(); cc=READ_B();} while (0)
#define FETCH_BS_3() do {a=READ_S(); b=READ_S();} while (0)
#define FETCH_BSS_3() FETCH_BSS_1()
#define FETCH_S_3() FETCH_S()
#define FETCH_W_3() FETCH_W()

#ifndef OP_LOADI
#define OP_LOADI OP_LOADI8
#endif

/* Definitions shared verbatim with <mruby/opcode.h> */
#ifndef MRUBY_OPCODE_H
#define MRUBY_OPCODE_H

enum mrb_insn {
#define OPCODE(x,_) OP_ ## x,
#include "mrc_ops.h"
#undef OPCODE
};

/* backward compatibility aliases */
#define OP_LOADT OP_LOADTRUE
#define OP_LOADF OP_LOADFALSE

#define OP_L_STRICT  1
#define OP_L_CAPTURE 2
#define OP_L_METHOD  OP_L_STRICT
#define OP_L_LAMBDA  (OP_L_STRICT|OP_L_CAPTURE)
#define OP_L_BLOCK   OP_L_CAPTURE

#define PEEK_B(pc) (*(pc))
#define PEEK_S(pc) ((pc)[0]<<8|(pc)[1])
#define PEEK_W(pc) ((pc)[0]<<16|(pc)[1]<<8|(pc)[2])

#define READ_B() PEEK_B(pc++)
#define READ_S() (pc+=2, PEEK_S(pc-2))
#define READ_W() (pc+=3, PEEK_W(pc-3))

#endif  /* MRUBY_OPCODE_H */
