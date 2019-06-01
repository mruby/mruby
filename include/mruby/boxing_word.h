/*
** mruby/boxing_word.h - word boxing mrb_value definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_BOXING_WORD_H
#define MRUBY_BOXING_WORD_H

#if defined(MRB_INT16)
# error MRB_INT16 is too small for MRB_WORD_BOXING.
#endif

#ifndef MRB_WITHOUT_FLOAT
struct RFloat {
  MRB_OBJECT_HEADER;
  mrb_float f;
};
#endif

struct RCptr {
  MRB_OBJECT_HEADER;
  void *p;
};

#define MRB_FIXNUM_SHIFT 1
#ifdef MRB_WITHOUT_FLOAT
#define MRB_TT_HAS_BASIC MRB_TT_CPTR
#else
#define MRB_TT_HAS_BASIC MRB_TT_FLOAT
#endif

enum mrb_special_consts {
  MRB_Qnil    = 0,
  MRB_Qfalse  = 2,
  MRB_Qtrue   = 4,
  MRB_Qundef  = 6,
};

#define MRB_FIXNUM_FLAG   0x01
#define MRB_SYMBOL_FLAG   0x0e
#define MRB_SPECIAL_SHIFT 8

#if defined(MRB_64BIT)
#define MRB_SYMBOL_BITSIZE  (sizeof(mrb_sym) * CHAR_BIT)
#define MRB_SYMBOL_MAX      UINT32_MAX
#else
#define MRB_SYMBOL_BITSIZE  (sizeof(mrb_sym) * CHAR_BIT - MRB_SPECIAL_SHIFT)
#define MRB_SYMBOL_MAX      (UINT32_MAX >> MRB_SPECIAL_SHIFT)
#endif

typedef union mrb_value {
  union {
    void *p;
    struct {
      unsigned int i_flag : MRB_FIXNUM_SHIFT;
      mrb_int i : (MRB_INT_BIT - MRB_FIXNUM_SHIFT);
    };
    struct {
      unsigned int sym_flag : MRB_SPECIAL_SHIFT;
      mrb_sym sym : MRB_SYMBOL_BITSIZE;
    };
    struct RBasic *bp;
#ifndef MRB_WITHOUT_FLOAT
    struct RFloat *fp;
#endif
    struct RCptr *vp;
  } value;
  unsigned long w;
} mrb_value;

MRB_API mrb_value mrb_word_boxing_cptr_value(struct mrb_state*, void*);
#ifndef MRB_WITHOUT_FLOAT
MRB_API mrb_value mrb_word_boxing_float_value(struct mrb_state*, mrb_float);
MRB_API mrb_value mrb_word_boxing_float_pool(struct mrb_state*, mrb_float);
#endif

#ifndef MRB_WITHOUT_FLOAT
#define mrb_float_pool(mrb,f) mrb_word_boxing_float_pool(mrb,f)
#endif

#define mrb_ptr(o)     (o).value.p
#define mrb_cptr(o)    (o).value.vp->p
#ifndef MRB_WITHOUT_FLOAT
#define mrb_float(o)   (o).value.fp->f
#endif
#define mrb_fixnum(o)  ((mrb_int)(o).value.i)
#define mrb_symbol(o)  (o).value.sym

MRB_INLINE enum mrb_vtype
mrb_type(mrb_value o)
{
  switch (o.w) {
  case MRB_Qfalse:
  case MRB_Qnil:
    return MRB_TT_FALSE;
  case MRB_Qtrue:
    return MRB_TT_TRUE;
  case MRB_Qundef:
    return MRB_TT_UNDEF;
  }
  if (o.value.i_flag == MRB_FIXNUM_FLAG) {
    return MRB_TT_FIXNUM;
  }
  if (o.value.sym_flag == MRB_SYMBOL_FLAG) {
    return MRB_TT_SYMBOL;
  }
  return o.value.bp->tt;
}

#define mrb_bool(o)    ((o).w != MRB_Qnil && (o).w != MRB_Qfalse)
#define mrb_fixnum_p(o) ((o).value.i_flag == MRB_FIXNUM_FLAG)
#define mrb_undef_p(o) ((o).w == MRB_Qundef)
#define mrb_nil_p(o)  ((o).w == MRB_Qnil)
#define mrb_false_p(o) ((o).w == MRB_Qfalse)
#define mrb_true_p(o)  ((o).w == MRB_Qtrue)

#ifndef MRB_WITHOUT_FLOAT
#define SET_FLOAT_VALUE(mrb,r,v) ((r) = mrb_word_boxing_float_value(mrb, v))
#endif
#define SET_CPTR_VALUE(mrb,r,v) ((r) = mrb_word_boxing_cptr_value(mrb, v))
#define SET_UNDEF_VALUE(r) ((r).w = MRB_Qundef)
#define SET_NIL_VALUE(r) ((r).w = MRB_Qnil)
#define SET_FALSE_VALUE(r) ((r).w = MRB_Qfalse)
#define SET_TRUE_VALUE(r) ((r).w = MRB_Qtrue)
#define SET_BOOL_VALUE(r,b) ((b) ? SET_TRUE_VALUE(r) : SET_FALSE_VALUE(r))
#define SET_INT_VALUE(r,n) do {                                             \
  (r).w = 0;                                                                \
  (r).value.i_flag = MRB_FIXNUM_FLAG;                                       \
  (r).value.i = (n);                                                        \
} while (0)
#define SET_SYM_VALUE(r,v)  do {                                            \
  (r).w = 0;                                                                \
  (r).value.sym_flag = MRB_SYMBOL_FLAG;                                     \
  (r).value.sym = (v);                                                      \
} while (0)
#define SET_OBJ_VALUE(r,v) do {                                             \
  (r).w = 0;                                                                \
  (r).value.p = (v);                                                        \
  if ((r).value.bp) (r).value.bp->tt = ((struct RObject*)(v))->tt;          \
} while (0)

#endif  /* MRUBY_BOXING_WORD_H */
