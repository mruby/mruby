/**
** @file mruby/irep.h - mrb_irep structure
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_IREP_H
#define MRUBY_IREP_H

#include "common.h"
#include <mruby/compile.h>

/**
 * Compiled mruby scripts.
 */
MRB_BEGIN_DECL

enum irep_pool_type {
  IREP_TT_STR   = 0,          /* string (need free) */
  IREP_TT_SSTR  = 2,          /* string (static) */
  IREP_TT_INT32 = 1,          /* 32bit integer */
  IREP_TT_INT64 = 3,          /* 64bit integer */
  IREP_TT_FLOAT = 5,          /* float (double/float) */
};

#define IREP_TT_NFLAG 1       /* number (non string) flag */
#define IREP_TT_SFLAG 2       /* static string flag */
#define IREP_TT_SFLAG 2       /* static string flag */

typedef struct mrb_pool_value {
  uint32_t tt;     /* packed type and length (for string) */
  union {
    const char *str;
    int32_t i32;
#ifdef MRB_INT64
    int64_t i64;
#endif
    mrb_float f;
  } u;
} mrb_pool_value;

struct mrb_lvinfo {        /* local variable info (name, idx) */
  mrb_sym name;
  uint16_t r;
};

/* Program data array struct */
typedef struct mrb_irep {
  uint16_t nlocals;        /* Number of local variables */
  uint16_t nregs;          /* Number of register variables */
  uint8_t flags;

  const mrb_code *iseq;
  const mrb_pool_value *pool;
  const mrb_sym *syms;
  const struct mrb_irep **reps;

  const struct mrb_lvinfo *lv;
  /* debug info */
  struct mrb_irep_debug_info* debug_info;

  uint16_t ilen, plen, slen, rlen;
  uint32_t refcnt;
} mrb_irep;

#define MRB_ISEQ_NO_FREE 1
#define MRB_IREP_NO_FREE 2

MRB_API mrb_irep *mrb_add_irep(mrb_state *mrb);

/** load mruby bytecode functions
* Please note! Currently due to interactions with the GC calling these functions will
* leak one RProc object per function call.
* To prevent this save the current memory arena before calling and restore the arena
* right after, like so
* int ai = mrb_gc_arena_save(mrb);
* mrb_value status = mrb_load_irep(mrb, buffer);
* mrb_gc_arena_restore(mrb, ai);
*/

/* @param [const uint8_t*] irep code, expected as a literal */
MRB_API mrb_value mrb_load_irep(mrb_state*, const uint8_t*);

/*
 * @param [const void*] irep code
 * @param [size_t] size of irep buffer. If -1 is given, it is considered unrestricted.
 */
MRB_API mrb_value mrb_load_irep_buf(mrb_state*, const void*, size_t);

/* @param [const uint8_t*] irep code, expected as a literal */
MRB_API mrb_value mrb_load_irep_cxt(mrb_state*, const uint8_t*, mrbc_context*);

/*
 * @param [const void*] irep code
 * @param [size_t] size of irep buffer. If -1 is given, it is considered unrestricted.
 */
MRB_API mrb_value mrb_load_irep_buf_cxt(mrb_state*, const void*, size_t, mrbc_context*);

void mrb_irep_free(mrb_state*, struct mrb_irep*);
void mrb_irep_incref(mrb_state*, struct mrb_irep*);
void mrb_irep_decref(mrb_state*, struct mrb_irep*);
void mrb_irep_cutref(mrb_state*, struct mrb_irep*);
void mrb_irep_remove_lv(mrb_state *mrb, mrb_irep *irep);

struct mrb_insn_data {
  uint8_t insn;
  uint16_t a;
  uint16_t b;
  uint8_t c;
};

struct mrb_insn_data mrb_decode_insn(const mrb_code *pc);

MRB_END_DECL

#endif  /* MRUBY_IREP_H */
