/*
** mruby/inline.h - Inline structures
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_INLINE_H
#define MRUBY_INLINE_H

#include "common.h"
#include <string.h>

/**
 * Inline structures that fit in RVALUE
 *
 * They cannot have finalizer, and cannot have instance variables.
 */
MRB_BEGIN_DECL

#define INLINE_DATA_SIZE (sizeof(void*) * 3)

struct RInline {
  MRB_OBJECT_HEADER;
  char inline_data[INLINE_DATA_SIZE];
};

#define RINLINE(obj)         ((struct RInline*)(mrb_ptr(obj)))
#define INLINE_PTR(obj)      (RINLINE(obj)->inline_data)

MRB_INLINE mrb_int mrb_inline_size()
{
  return INLINE_DATA_SIZE;
}

MRB_INLINE void* mrb_inline_ptr(mrb_value object)
{
  return INLINE_PTR(object);
}

MRB_INLINE void mrb_inline_copy(mrb_value dest, mrb_value src)
{
  memcpy(INLINE_PTR(dest), INLINE_PTR(src), INLINE_DATA_SIZE);
}

MRB_END_DECL

#endif /* MRUBY_INLINE_H */
