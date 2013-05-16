/*
** gc.c - garbage collector for mruby
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_GC_H
#define MRUBY_GC_H

#if defined(__cplusplus)
extern "C" {
#endif

#include "mruby.h"
#include "mruby/value.h"

typedef int each_object_callback(mrb_state *mrb, struct RBasic obj, void *data);
void mrb_objspace_each_objects(mrb_state *mrb, each_object_callback* callback, void *data);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_GC_H */
