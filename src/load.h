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

static inline uint32_t
bin_to_uint32(unsigned char bin[])
{
  return (uint32_t)bin[0] << 24 |
         (uint32_t)bin[1] << 16 |
         (uint32_t)bin[2] << 8  |
         (uint32_t)bin[3];
}

static inline uint16_t
bin_to_uint16(unsigned char bin[])
{
  return (uint16_t)bin[0] << 8 |
         (uint16_t)bin[1];
}

#endif
