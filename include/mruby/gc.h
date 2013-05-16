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
#include "mruby/array.h"
#include "mruby/class.h"
#include "mruby/data.h"
#include "mruby/hash.h"
#include "mruby/proc.h"
#include "mruby/range.h"
#include "mruby/string.h"
#include "mruby/variable.h"

struct free_obj {
  MRB_OBJECT_HEADER;
  struct RBasic *next;
};

struct RVALUE {
  union {
    struct free_obj free;
    struct RBasic basic;
    struct RObject object;
    struct RClass klass;
    struct RString string;
    struct RArray array;
    struct RHash hash;
    struct RRange range;
    struct RData data;
    struct RProc proc;
  } as;
};

typedef struct RVALUE RVALUE;

typedef int each_object_callback(RVALUE *obj, void *data);
void mrb_objspace_each_objects(mrb_state *mrb, each_object_callback* callback, void *data);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_GC_H */
