/*
** range.h - Range class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_RANGE_H
#define MRUBY_RANGE_H

struct RRange {
  MRUBY_OBJECT_HEADER;
  struct mrb_range_edges {
    mrb_value beg;
    mrb_value end;
  } *edges;
  int excl;
};

#define mrb_range_ptr(v)    ((struct RRange*)((v).value.p))
#define mrb_range_value(p)  mrb_obj_value((void*)(p))

mrb_value mrb_range_new(mrb_state*, mrb_value, mrb_value, int);
mrb_int mrb_range_beg_len(mrb_state *mrb, mrb_value range, mrb_int *begp, mrb_int *lenp, mrb_int len, mrb_int err);
int mrb_obj_is_instance_of(mrb_state *mrb, mrb_value obj, struct RClass* c);
struct RClass* mrb_class_real(struct RClass* cl);

#endif  /* MRUBY_RANGE_H */
