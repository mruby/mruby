/**
** @file mruby/boxing_nan.h - nan boxing mrb_value definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_BOXING_NAN_H
#define MRUBY_BOXING_NAN_H

#ifdef MRB_USE_FLOAT32
# error ---->> MRB_NAN_BOXING and MRB_USE_FLOAT32 conflict <<----
#endif

#ifdef MRB_NO_FLOAT
# error ---->> MRB_NAN_BOXING and MRB_NO_FLOAT conflict <<----
#endif

#ifdef MRB_INT64
# error ---->> MRB_NAN_BOXING and MRB_INT64 conflict <<----
#endif

#define MRB_FIXNUM_SHIFT 0
#define MRB_SYMBOL_SHIFT 0
#define MRB_FIXNUM_MIN INT32_MIN
#define MRB_FIXNUM_MAX INT32_MAX

enum mrb_nanbox_tt_inline {
  MRB_NANBOX_TT_POINTER = 1,
  MRB_NANBOX_TT_INTEGER,
  MRB_NANBOX_TT_SYMBOL,
  MRB_NANBOX_TT_MISC,
#ifndef MRB_NO_FLOAT
  MRB_NANBOX_TT_FLOAT,
#endif
};

/* value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   object: 1111111111110001 PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 1111111111110010 0000000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   sym   : 1111111111110011 0100000000000000 SSSSSSSSSSSSSSSS SSSSSSSSSSSSSSSS
 *   misc  : 1111111111110100 0100000000000000 0000000000000000 TTTTTT000000MMMM
 */
typedef struct mrb_value {
  uint64_t u;
} mrb_value;

union mrb_value_ {
  mrb_float f;
  uint64_t u;
#ifdef MRB_64BIT
  void *p;
#endif
  mrb_value value;
};

mrb_static_assert1(sizeof(mrb_value) == sizeof(union mrb_value_));

static inline union mrb_value_
mrb_val_union(mrb_value v)
{
  union mrb_value_ x;
  x.u = v.u;
  return x;
}

static inline mrb_float
mrb_float(mrb_value v)
{
  union {
    mrb_float f;
    uint64_t u;
  } x;
  x.u = v.u;
  return x.f;
}

#define mrb_tt_(o)       ((enum mrb_nanbox_tt_inline)((o).u >> 48)&0xf)

MRB_INLINE enum mrb_vtype
mrb_type(mrb_value o)
{
  switch (mrb_tt_(o)) {
  case MRB_NANBOX_TT_POINTER:
    return RBASIC(o)->tt;
  case MRB_NANBOX_TT_INTEGER:
    return MRB_TT_INTEGER;
  case MRB_NANBOX_TT_SYMBOL:
    return MRB_TT_SYMBOL;
  case MRB_NANBOX_TT_MISC:
    return (enum mrb_vtype)(o.u >> 10) & 0x3f;
#ifndef MRB_NO_FLOAT
  default:
    return MRB_TT_FLOAT;
#endif
  }
  return MRB_TT_UNDEF;
}

#define mrb_symbol(o)   ((mrb_sym)((o).u & 0x3fffffff))

#ifdef MRB_INT64
#define mrb_fixnum(o)   ((mrb_int)((o).u & 0xffffffffffffL))
#define mrb_integer(o)  ((mrb_tt(o)==MRB_NANBOX_TT_POINTER)?(((struct RInteger*)mrb_ptr(o))->i):mrb_fixnum(o)))
#else  /* MRB_INT32 */
#define mrb_fixnum(o)   ((mrb_int)((o).u & 0xffffffff))
#define mrb_integer(o)  mrb_fixnum(o)
#endif

#ifdef MRB_64BIT
#define mrb_ptr(o)      ((void*)(((uintptr_t)(o).u) & 0xffffffffffff))
#else
#define mrb_ptr(o)      ((void*)(((uintptr_t)(o).u) & 0xffffffff))
#endif
#define mrb_cptr(o)     mrb_ptr(o)

#define NANBOX_SET_VALUE(o, tt, attr, v) do { \
  union mrb_value_ mrb_value_union_variable; \
  mrb_value_union_variable.attr = (v);\
  mrb_value_union_variable.ttt = 0xfff00000 | (((tt)+1)<<14);\
  o = mrb_value_union_variable.value;\
} while (0)

#ifdef MRB_64BIT
#define NANBOX_SET_OBJ_VALUE(o, tt, v) do {\
  union mrb_value_ mrb_value_union_variable;\
  mrb_value_union_variable.p = (void*)((uintptr_t)(v)>>2);\
  mrb_value_union_variable.ttt = (0xfff00000|(((tt)+1)<<14)|NANBOX_SHIFT_LONG_POINTER(v));\
  o = mrb_value_union_variable.value;\
} while (0)
#else
#define NANBOX_SET_OBJ_VALUE(o, tt, v) NANBOX_SET_VALUE(o, tt, i, (uint32_t)v)
#endif

#define SET_FLOAT_VALUE(mrb,r,v) do { \
  union mrb_value_ mrb_value_union_variable; \
  if ((v) != (v)) { /* NaN */ \
    mrb_value_union_variable.u = 0x7ff8000000000000UL; \
  } \
  else { \
    mrb_value_union_variable.f = (v); \
  } \
  r = mrb_value_union_variable.value; \
} while(0)

#define NANBOX_SET_MISC_VALUE(o, tt, attr, v) do { \
  union mrb_value_ mrb_value_union_variable; \
  mrb_value_union_variable.attr = (v);\
  mrb_value_union_variable.ttt = 0xfff00000 | (((tt)+1)<<14);\
  o = mrb_value_union_variable.value;\
} while (0)

#define SET_NIL_VALUE(r) NANBOX_SET_VALUE(r, MRB_TT_FALSE, i, 0)
#define SET_FALSE_VALUE(r) NANBOX_SET_VALUE(r, MRB_TT_FALSE, i, 1)
#define SET_TRUE_VALUE(r) NANBOX_SET_VALUE(r, MRB_TT_TRUE, i, 1)
#define SET_BOOL_VALUE(r,b) NANBOX_SET_VALUE(r, b ? MRB_TT_TRUE : MRB_TT_FALSE, i, 1)
#define SET_INT_VALUE(mrb, r,n) NANBOX_SET_VALUE(r, MRB_TT_INTEGER, i, (uint32_t)(n))
#define SET_FIXNUM_VALUE(r,n) NANBOX_SET_VALUE(r, MRB_TT_INTEGER, i, (uint32_t)(n))
#define SET_SYM_VALUE(r,v) NANBOX_SET_VALUE(r, MRB_TT_SYMBOL, i, (uint32_t)(v))
#define SET_OBJ_VALUE(r,v) NANBOX_SET_OBJ_VALUE(r, (((struct RObject*)(v))->tt), (v))
#ifdef MRB_64BIT
MRB_API mrb_value mrb_nan_boxing_cptr_value(struct mrb_state*, void*);
#define SET_CPTR_VALUE(mrb,r,v) ((r) = mrb_nan_boxing_cptr_value(mrb, v))
#else
#define SET_CPTR_VALUE(mrb,r,v) NANBOX_SET_VALUE(r, MRB_TT_CPTR, p, v)
#endif
#define SET_UNDEF_VALUE(r) NANBOX_SET_VALUE(r, MRB_TT_UNDEF, i, 0)

#endif  /* MRUBY_BOXING_NAN_H */
