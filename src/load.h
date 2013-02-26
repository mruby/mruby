/*
** load.h - mruby binary loader
**
** See Copyright Notice in mruby.h
*/

#ifndef MRB_LOAD_H_
#define MRB_LOAD_H_
#include <stdint.h>

extern const char mrb_internal_hex2bin[];
extern void mrb_irep_load_error(mrb_state *, int);

#endif
