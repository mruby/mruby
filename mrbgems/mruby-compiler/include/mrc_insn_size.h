#ifndef MRC_INSN_SIZE_H
#define MRC_INSN_SIZE_H

#define Z 1
#define S 3
#define W 4
#define OPCODE(_,x) x,
/* instruction sizes */
static const uint8_t mrc_insn_size[] = {
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
static const uint8_t mrc_insn_size1[] = {
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
static const uint8_t mrc_insn_size2[] = {
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
static const uint8_t mrc_insn_size3[] = {
#include "mrc_ops.h"
};
#undef B
#undef BB
#undef BBB
#undef BS
#undef BSS

#endif  /* MRC_INSN_SIZE_H */
