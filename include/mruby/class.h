/*
** mruby/class.h - Class class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_CLASS_H
#define MRUBY_CLASS_H

#if defined(__cplusplus)
extern "C" {
#endif

struct RClass {
  MRB_OBJECT_HEADER;
  struct iv_tbl *iv;
  struct kh_mt *mt;
  struct RClass *super;
};

#define mrb_class_ptr(v)    ((struct RClass*)(mrb_ptr(v)))
#define RCLASS_SUPER(v)     (((struct RClass*)(mrb_ptr(v)))->super)
#define RCLASS_IV_TBL(v)    (((struct RClass*)(mrb_ptr(v)))->iv)
#define RCLASS_M_TBL(v)     (((struct RClass*)(mrb_ptr(v)))->mt)

static inline struct RClass*
mrb_class(mrb_state *mrb, mrb_value v)
{
  switch (mrb_type(v)) {
  case MRB_TT_FALSE:
    if (mrb_fixnum(v))
      return mrb->false_class;
    return mrb->nil_class;
  case MRB_TT_TRUE:
    return mrb->true_class;
  case MRB_TT_SYMBOL:
    return mrb->symbol_class;
  case MRB_TT_FIXNUM:
    return mrb->fixnum_class;
  case MRB_TT_FLOAT:
    return mrb->float_class;
  case MRB_TT_CPTR:
    return mrb->object_class;
  case MRB_TT_ENV:
    return NULL;
  default:
    return mrb_obj_ptr(v)->c;
  }
}

#define MRB_SET_INSTANCE_TT(c, tt) c->flags = ((c->flags & ~0xff) | (char)tt)
#define MRB_INSTANCE_TT(c) (enum mrb_vtype)(c->flags & 0xff)

struct RClass* mrb_define_class_id(mrb_state*, mrb_sym, struct RClass*);
struct RClass* mrb_define_module_id(mrb_state*, mrb_sym);
struct RClass *mrb_vm_define_class(mrb_state*, mrb_value, mrb_value, mrb_sym);
struct RClass *mrb_vm_define_module(mrb_state*, mrb_value, mrb_sym);
void mrb_define_method_vm(mrb_state*, struct RClass*, mrb_sym, mrb_value);
void mrb_define_method_raw(mrb_state*, struct RClass*, mrb_sym, struct RProc *);
void mrb_define_method_id(mrb_state *mrb, struct RClass *c, mrb_sym mid, mrb_func_t func, mrb_aspec aspec);
void mrb_alias_method(mrb_state *mrb, struct RClass *c, mrb_sym a, mrb_sym b);

struct RClass *mrb_class_outer_module(mrb_state*, struct RClass *);
struct RProc *mrb_method_search_vm(mrb_state*, struct RClass**, mrb_sym);
struct RProc *mrb_method_search(mrb_state*, struct RClass*, mrb_sym);

struct RClass* mrb_class_real(struct RClass* cl);

void mrb_gc_mark_mt(mrb_state*, struct RClass*);
size_t mrb_gc_mark_mt_size(mrb_state*, struct RClass*);
void mrb_gc_free_mt(mrb_state*, struct RClass*);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_CLASS_H */
