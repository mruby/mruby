/*
** mruby/irep.h - mrb_irep structure
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_IREP_H
#define MRUBY_IREP_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "mrbconf.h"
#include "mruby/compile.h"

#ifdef MRB_ENABLE_JIT
#include "mruby/jit.h"
#endif

#define MRB_OPT_ARGC_MAX 15
#define MRB_IREP_AOFF_LEN (MRB_OPT_ARGC_MAX + 1)

enum irep_pool_type {
  IREP_TT_STRING,
  IREP_TT_FIXNUM,
  IREP_TT_FLOAT,
};

struct mrb_locals {
  mrb_sym name;
  uint16_t r;
};

/* Program data array struct */
typedef struct mrb_irep {
  uint16_t nlocals;        /* Number of local variables */
  uint16_t nregs;          /* Number of register variables */
  uint8_t flags;
  uint8_t oalen;

  mrb_code *iseq;
  mrb_value *pool;
  mrb_sym *syms;
  struct mrb_irep **reps;

  struct mrb_locals *lv;
  /* debug info */
  const char *filename;
  uint16_t *lines;
  struct mrb_irep_debug_info* debug_info;

  size_t ilen, plen, slen, rlen, refcnt;

  uint16_t oa_off[MRB_IREP_AOFF_LEN];
#ifdef MRB_ENABLE_JIT
  mrb_jit_page jit_page;
#endif
} mrb_irep;

#define MRB_IREP_ISEQ_NO_FREE 1
#define MRB_IREP_JITTED 2
#define MRB_IREP_JITTED_P(ir) (((ir)->flags & MRB_IREP_JITTED) != 0)

MRB_API mrb_irep *mrb_add_irep(mrb_state *mrb);
MRB_API mrb_value mrb_load_irep(mrb_state*, const uint8_t*);
MRB_API mrb_value mrb_load_irep_cxt(mrb_state*, const uint8_t*, mrbc_context*);
void mrb_irep_free(mrb_state*, struct mrb_irep*);
void mrb_irep_incref(mrb_state*, struct mrb_irep*);
void mrb_irep_decref(mrb_state*, struct mrb_irep*);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_IREP_H */
