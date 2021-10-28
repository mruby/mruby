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
  MRB_NANBOX_TT_POINTER = 0,
  MRB_NANBOX_TT_INTEGER = 1,
  MRB_NANBOX_TT_SYMBOL = 2,
  MRB_NANBOX_TT_MISC = 3,
};

/* value representation by nan-boxing:
 *   float : SEEEEEEE EEEEFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF
 *   +/-inf: S1111111 11110000 00000000 00000000 00000000 00000000 00000000 00000000
 *   nan   : 01111111 11111000 00000000 00000000 00000000 00000000 00000000 00000000
 *   int   : 01111111 11111001 00000000 00000000 IIIIIIII IIIIIIII IIIIIIII IIIIIIII
 *   sym   : 01111111 11111110 00000000 00000000 SSSSSSSS SSSSSSSS SSSSSSSS SSSSSSSS
 *   misc  : 01111111 11111111 00000000 00000000 00000000 00000000 00TTTTTT 0000MMMM
 *   object: 01111111 11111100 PPPPPPPP PPPPPPPP PPPPPPPP PPPPPPPP PPPPPPPP PPPPPP00
 *   ptr   : 01111111 11111100 PPPPPPPP PPPPPPPP PPPPPPPP PPPPPPPP PPPPPPPP PPPPPP01
 * Stored as O = R + 0x8004000000000000, retrieved as R = O - 0x8004000000000000.
 * This makes pointers have all zeros in the top 32 bits.
 * Small-ints and strs have 1 as LSB to make sure they don't look like pointers
 * to the garbage collector.
 */
typedef struct mrb_value {
  uint64_t u;
} mrb_value;

static inline mrb_float
mrb_nan_boxing_value_float(mrb_value v)
{
  union {
    mrb_float f;
    uint64_t u;
  } x;
  x.u = v.u - 0x8004000000000000;
  return x.f;
}

#define SET_FLOAT_VALUE(mrb,r,f) do { \
  union { \
    mrb_float f; \
    uint64_t u; \
  } float_uint_union; \
  if ((f) != (f)) { /* NaN */ \
    float_uint_union.u = 0x7ff8000000000000UL; \
  } \
  else { \
    float_uint_union.f = (f); \
  } \
  r.u = float_uint_union.u + 0x8004000000000000; \
} while(0)

#define NANBOX_SET_VALUE(o, tt, v) do { \
  (o).u = ((uint64_t)tt<<48) | (uint64_t)(v); \
} while (0)

#define mrb_float_p(o) (((uint64_t)(o.u)&0xfffc000000000000) != 0)

MRB_INLINE enum mrb_vtype
mrb_type(mrb_value o)
{
  if (mrb_float_p(o)) return MRB_TT_FLOAT;

  uint64_t u = o.u;
  switch ((enum mrb_nanbox_tt_inline)((u >> 48) & 3)) {
  case MRB_NANBOX_TT_POINTER: {
    if (u == 0) return MRB_TT_FALSE;
    if (u & 1) return MRB_TT_CPTR;
    return ((struct RBasic*)(uintptr_t)u)->tt;
  }
  case MRB_NANBOX_TT_INTEGER:
    return MRB_TT_INTEGER;
  case MRB_NANBOX_TT_SYMBOL:
    return MRB_TT_SYMBOL;
  case MRB_NANBOX_TT_MISC:
    return (enum mrb_vtype)(o.u >> 8) & 0x1f;
  default:
    /* never happen */
    return MRB_TT_FLOAT;
  }
}

#define NANBOX_SET_MISC_VALUE(r,t,i) NANBOX_SET_VALUE(r, MRB_NANBOX_TT_MISC, ((t)<<8) | i)

#define mrb_float(o)   mrb_nan_boxing_value_float(o)
#define mrb_fixnum(o)  ((mrb_int)(((uintptr_t)0xffffffff)&((o).u)))
#define mrb_integer(o) mrb_fixnum(o)
#define mrb_symbol(o)  ((mrb_sym)((uintptr_t)0xffffffff)&((o).u))
#define mrb_ptr(o)     ((void*)(uintptr_t)(o).u)
#define mrb_cptr(o)    ((void*)(uintptr_t)(0xfffffffffffe&(o).u))

#define SET_NIL_VALUE(r) NANBOX_SET_MISC_VALUE(r, MRB_TT_FALSE, 0)
#define SET_FALSE_VALUE(r) NANBOX_SET_MISC_VALUE(r, MRB_TT_FALSE, 1)
#define SET_TRUE_VALUE(r) NANBOX_SET_MISC_VALUE(r, MRB_TT_TRUE, 1)
#define SET_BOOL_VALUE(r,b) NANBOX_SET_MISC_VALUE(r, (b) ? MRB_TT_TRUE : MRB_TT_FALSE, 1)
#define SET_INT_VALUE(mrb,r,n) SET_FIXNUM_VALUE(r,n)
#define SET_FIXNUM_VALUE(r,n) NANBOX_SET_VALUE(r, MRB_NANBOX_TT_INTEGER, (uint32_t)(n))
#define SET_SYM_VALUE(r,v) NANBOX_SET_VALUE(r, MRB_NANBOX_TT_SYMBOL, (uint32_t)(v))
#define SET_OBJ_VALUE(r,v) do {(r).u = (uint64_t)(uintptr_t)v;} while (0)
#define SET_CPTR_VALUE(mrb,r,v) do {(r).u = ((uint64_t)(uintptr_t)v) | 1;} while (0)
#define SET_UNDEF_VALUE(r) NANBOX_SET_MISC_VALUE(r, MRB_TT_UNDEF, 4)

#define mrb_nil_p(o)  (mrb_type(o) == MRB_TT_FALSE && ((o).u & 3) == 0)
#define mrb_false_p(o) (mrb_type(o) == MRB_TT_FALSE && ((o).u & 2) == 0)

#endif  /* MRUBY_BOXING_NAN_H */
