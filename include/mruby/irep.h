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
  IREP_TT_STRING,
  IREP_TT_FIXNUM,
  IREP_TT_FLOAT,
};

enum mrb_catch_type {
  MRB_CATCH_RESCUE = 0,
  MRB_CATCH_ENSURE = 1,
};

struct mrb_locals {
  mrb_sym name;
  uint16_t r;
};

struct mrb_irep_catch_hander {
  uint8_t type;         /* enum mrb_catch_type */
  uint8_t begin[2];     /* The starting address to match the hander. Includes this. */
  uint8_t end[2];       /* The endpoint address that matches the hander. Not Includes this. */
  uint8_t target[2];    /* The address to jump to if a match is made. */
};

/* Program data array struct */
typedef struct mrb_irep {
  uint16_t nlocals;        /* Number of local variables */
  uint16_t nregs;          /* Number of register variables */
  uint16_t clen;           /* Number of catch handlers */
  uint8_t flags;

  const mrb_code *iseq;
  /*
   * A catch handler table is placed after the iseq entity.
   * The reason it doesn't add fields to the structure is to keep the mrb_irep structure from bloating.
   * The catch handler table can be obtained with `mrb_irep_catch_handler_table(irep)`.
   */
  mrb_value *pool;
  mrb_sym *syms;
  struct mrb_irep **reps;

  struct mrb_locals *lv;
  /* debug info */
  struct mrb_irep_debug_info* debug_info;

  uint16_t ilen, plen, slen, rlen;
  uint32_t refcnt;
} mrb_irep;

#define MRB_ISEQ_NO_FREE 1

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

static inline const struct mrb_irep_catch_hander *
mrb_irep_catch_handler_table(const struct mrb_irep *irep)
{
  if (irep->clen > 0) {
    return (const struct mrb_irep_catch_hander *)(irep->iseq + irep->ilen);
  }
  else {
    return (const struct mrb_irep_catch_hander *)NULL;
  }
}

MRB_END_DECL

#endif  /* MRUBY_IREP_H */
