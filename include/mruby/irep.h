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

typedef struct mrb_irep {
  int idx:16;
  int nlocals:16;
  int nregs:16;
  int flags:8;

  mrb_code *iseq;
  mrb_value *pool;
  mrb_sym *syms;

  /* debug info */
  const char *filename;
  short *lines;

  size_t ilen, plen, slen;
} mrb_irep;

#define MRB_ISEQ_NO_FREE 1

mrb_irep *mrb_add_irep(mrb_state *mrb);
mrb_value mrb_load_irep(mrb_state*, const uint8_t*);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_IREP_H */
