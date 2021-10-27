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
  MRB_NANBOX_TT_OBJECT = 1,
  MRB_NANBOX_TT_INTEGER,
  MRB_NANBOX_TT_SYMBOL,
  MRB_NANBOX_TT_POINTER,
  MRB_NANBOX_TT_MISC,
};

/* value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   object: 1111111111110001 PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 1111111111110010 0000000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   sym   : 1111111111110011 0000000000000000 SSSSSSSSSSSSSSSS SSSSSSSSSSSSSSSS
 *   ptr   : 1111111111110100 0000000000000000 SSSSSSSSSSSSSSSS SSSSSSSSSSSSSSSS
 *   misc  : 1111111111110101 0000000000000000 0000000000000000 TTTTTT000000MMMM
 */
typedef struct mrb_value {
  uint64_t u;
} mrb_value;

union mrb_value_ {
  mrb_float f;
  uint64_t u;
#ifdef MRB_64BIT
  void *p;
# define NANBOX_IMMEDIATE_VALUE uint32_t i
#else
# define NANBOX_IMMEDIATE_VALUE union { uint32_t i; void *p; }
#endif
  struct {
    MRB_ENDIAN_LOHI(
      uint32_t ttt;
      ,NANBOX_IMMEDIATE_VALUE;
    )
  };
  mrb_value value;
};

mrb_static_assert(sizeof(mrb_value) == sizeof(union mrb_value_));

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

#define SET_FLOAT_VALUE(mrb,r,v) do { \
  union { \
    mrb_float f; \
    uint64_t u; \
  } float_uint_union; \
  if ((v) != (v)) { /* NaN */ \
    float_uint_union.u = 0x7ff8000000000000UL; \
  } \
  else { \
    float_uint_union.f = (v); \
  } \
  r.u = float_uint_union.u; \
} while(0)

#define NANBOX_SET_VALUE(o, tt, v) do { \
  (o).u = ((0xfff0|(tt))<<48) | (v).u; \
} while (0)

MRB_INLINE enum mrb_vtype
mrb_type(mrb_value o)
{
  if ((o.u >> 52) == 0xfff) return MRB_TT_FLOAT;
  switch ((o.u >> 48) & 7) {
  case MRB_NANBOX_TT_OBJECT: {
    uintptr_t u = o.u & ~((~0)<<48);
    return ((struct RBasic *)u)->tt;
  }
  case MRB_NANBOX_TT_INTEGER:
    return MRB_TT_INTEGER;
  case MRB_NANBOX_TT_SYMBOL:
    return MRB_TT_SYMBOL;
  case MRB_NANBOX_TT_POINTER:
    return MRB_TT_CPTR;
  case MRB_NANBOX_TT_MISC:
    return (enum mrb_vtype)(o.u >> 5) & 0x1f;
  default:
    return MRB_TT_FLOAT;
  }
}

#define mrb_fixnum(o)   ((mrb_int)((uintptr_t)0xffffffff)&((o).u))
#define mrb_integer(o)  mrb_fixnum(o)
#define mrb_symbol(o)   ((mrb_sym)((uintptr_t)0xffffffff)&((o).u))
#define mrb_ptr(o)      ((void*)(((uintptr_t)0xfffffffffff)&((o).u)))
#define mrb_cptr(o)     mrb_ptr(o)

#define SET_NIL_VALUE(r) NANBOX_SET_MISC_VALUE(r, MRB_TT_FALSE, 0)
#define SET_FALSE_VALUE(r) NANBOX_SET_MISC_VALUE(r, MRB_TT_FALSE, 1)
#define SET_TRUE_VALUE(r) NANBOX_SET_MISC_VALUE(r, MRB_TT_TRUE, 1)
#define SET_BOOL_VALUE(r,b) NANBOX_SET_MISC_VALUE(r, b ? MRB_TT_TRUE : MRB_TT_FALSE, 1)
#define SET_INT_VALUE(mrb,r,n) SET_FIXNUM_VALUE(r,n)
#define SET_FIXNUM_VALUE(r,n) NANBOX_SET_VALUE(r, MRB_NANBOX_TT_INTEGER, (uint32_t)(n))
#define SET_SYM_VALUE(r,v) NANBOX_SET_VALUE(r, MRB_NANBOX_TT_SYMBOL, (uint32_t)(v))
#define SET_OBJ_VALUE(r,v) NANBOX_SET_VALUE(o, MRB_NANBOT_TT_OBJECT, v)
#define SET_CPTR_VALUE(mrb,r,v) NANBOX_SET_VALUE(o, MRB_NANBOT_TT_POINTER, v)
#define SET_UNDEF_VALUE(r) NANBOX_SET_MISC_VALUE(r, MRB_TT_UNDEF, 4)

#endif  /* MRUBY_BOXING_NAN_H */
