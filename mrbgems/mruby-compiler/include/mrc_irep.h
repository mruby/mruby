/**
** @file mruby/irep.h - mrc_irep structure
**
** See Copyright Notice in mruby.h
*/

#ifndef MRC_IREP_H
#define MRC_IREP_H

#include "mrc_ccontext.h"

/**
 * Compiled mruby scripts.
 */
MRC_BEGIN_DECL

#define IREP_TT_NFLAG 1       /* number (non string) flag */
#define IREP_TT_SFLAG 2       /* static string flag */

typedef struct mrc_pool_value {
  uint32_t tt;     /* packed type and length (for string) */
  union {
    const char *str;
    int32_t i32;
    int64_t i64;
#ifndef MRC_NO_FLOAT
    mrc_float f;
#endif
  } u;
} mrc_pool_value;

enum mrc_catch_type {
  MRC_CATCH_RESCUE = 0,
  MRC_CATCH_ENSURE = 1,
};

struct mrc_irep_catch_handler {
  uint8_t type;         /* enum mrc_catch_type */
  uint8_t begin[4];     /* The starting address to match the handler. Includes this. */
  uint8_t end[4];       /* The endpoint address that matches the handler. Not Includes this. */
  uint8_t target[4];    /* The address to jump to if a match is made. */
};

/* Program data array struct */
typedef struct mrc_irep {
  uint16_t nlocals;        /* Number of local variables */
  uint16_t nregs;          /* Number of register variables */
  uint16_t clen;           /* Number of catch handlers */
  uint8_t flags;

  const mrc_code *iseq;
  /*
   * A catch handler table is placed after the iseq entity.
   * The reason it doesn't add fields to the structure is to keep the mrc_irep structure from bloating.
   * The catch handler table can be obtained with `mrc_irep_catch_handler_table(irep)`.
   */
  const mrc_pool_value *pool;
  const mrc_sym *syms;
  const struct mrc_irep * const *reps;

  mrc_sym *lv;  // Remove const for mrc_resolve_intern
  /* debug info */
  struct mrc_irep_debug_info* debug_info;

  uint32_t ilen;
  uint16_t plen, slen;
  uint16_t rlen;
  uint16_t refcnt;
} mrc_irep;

#define MRC_ISEQ_NO_FREE 1
#define MRC_IREP_NO_FREE 2

struct mrc_insn_data {
  uint8_t insn;
  uint32_t a;
  uint16_t b;
  uint16_t cc;
  const mrc_code *addr;
};

#define mrc_irep_catch_handler_pack(n, v)   mrc_uint32_to_bin(n, v)
#define mrc_irep_catch_handler_unpack(v)    mrc_bin_to_uint32(v)

void mrc_irep_remove_lv(mrc_ccontext *c, mrc_irep *irep);
void mrc_irep_free(mrc_ccontext *c, mrc_irep *irep);

#define MRC_ASPEC_REQ(a)          (((a) >> 18) & 0x1f)
#define MRC_ASPEC_OPT(a)          (((a) >> 13) & 0x1f)
#define MRC_ASPEC_REST(a)         (((a) >> 12) & 0x1)
#define MRC_ASPEC_POST(a)         (((a) >> 7) & 0x1f)
#define MRC_ASPEC_KEY(a)          (((a) >> 2) & 0x1f)
#define MRC_ASPEC_KDICT(a)        (((a) >> 1) & 0x1)
#define MRC_ASPEC_BLOCK(a)        ((a) & 1)
#define MRC_ASPEC_NOBLOCK(a)      (((a) >> 23) & 0x1)

MRC_END_DECL

#endif  /* MRC_IREP_H */
