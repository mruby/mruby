/**
** @file mruby/object.h - mruby object definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_OBJECT_H
#define MRUBY_OBJECT_H

#define MRB_OBJECT_HEADER \
  struct RClass *c;       \
  struct RBasic *gcnext;  \
  enum mrb_vtype tt:8;    \
  unsigned int gc_color:3; \
  unsigned int frozen:1;  \
  uint32_t flags:20

#define MRB_FLAG_TEST(obj, flag) ((obj)->flags & (flag))

struct RBasic {
  MRB_OBJECT_HEADER;
};
#define mrb_basic_ptr(v) ((struct RBasic*)(mrb_ptr(v)))

#define MRB_OBJ_IS_FROZEN 1
#define mrb_frozen_p(o) ((o)->frozen)

struct RObject {
  MRB_OBJECT_HEADER;
  struct iv_tbl *iv;
};
#define mrb_obj_ptr(v)   ((struct RObject*)(mrb_ptr(v)))

#define mrb_special_const_p(x) mrb_immediate_p(x)

struct RFiber {
  MRB_OBJECT_HEADER;
  struct mrb_context *cxt;
};

#define mrb_static_assert_object_size(st) \
  mrb_static_assert(sizeof(st) <= sizeof(void*) * 6, \
                    #st " size must be within 6 words")

#endif  /* MRUBY_OBJECT_H */
