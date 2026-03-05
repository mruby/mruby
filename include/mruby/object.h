/**
** @file mruby/object.h - mruby object definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_OBJECT_H
#define MRUBY_OBJECT_H

#define MRB_OBJECT_HEADER \
  struct RClass *c;       \
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

/* Object shape flag -- when set, obj->iv is shaped, not iv_tbl* */
/* Bit 5: avoids conflict with MRB_INSTANCE_TT_MASK (bits 0-4);
   but conflicts with MRB_HASH_AR_EA_N_USED on 32-bit, so the
   predicate must also check tt to avoid false positives */
#define MRB_FL_OBJ_SHAPED (1 << 5)
#define MRB_OBJ_SHAPED_P(o) ((o)->tt == MRB_TT_OBJECT && ((o)->flags & MRB_FL_OBJ_SHAPED))

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
  mrb_static_assert(sizeof(st) <= sizeof(void*) * 5, \
                    #st " size must be within 5 words")

#endif  /* MRUBY_OBJECT_H */
