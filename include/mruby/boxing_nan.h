/**
** @file mruby/boxing_nan.h - nan boxing mrb_value definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_BOXING_NAN_H
#define MRUBY_BOXING_NAN_H

#ifdef MRB_USE_FLOAT
# error ---->> MRB_NAN_BOXING and MRB_USE_FLOAT conflict <<----
#endif

#ifdef MRB_WITHOUT_FLOAT
# error ---->> MRB_NAN_BOXING and MRB_WITHOUT_FLOAT conflict <<----
#endif

#ifdef MRB_INT64
# error ---->> MRB_NAN_BOXING and MRB_INT64 conflict <<----
#endif

#define MRB_FIXNUM_SHIFT 0
#define MRB_SYMBOL_SHIFT 0

/* value representation by nan-boxing:
 *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   object: 111111111111TTTT TTPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP PPPPPPPPPPPPPPPP
 *   int   : 1111111111110001 0000000000000000 IIIIIIIIIIIIIIII IIIIIIIIIIIIIIII
 *   sym   : 1111111111110001 0100000000000000 SSSSSSSSSSSSSSSS SSSSSSSSSSSSSSSS
 * In order to get enough bit size to save TT, all pointers are shifted 2 bits
 * in the right direction. Also, TTTTTT is the mrb_vtype + 1;
 */
typedef uint64_t mrb_value;

union mrb_value {
  mrb_float f;
  uint64_t u;
#ifdef MRB_64BIT
  void *p;
#endif
  struct {
    MRB_ENDIAN_LOHI(
      uint32_t ttt;
      ,union {
        mrb_int i;
        mrb_sym sym;
#ifdef MRB_32BIT
        void *p;
#endif
      };
    )
  };
};

static inline union mrb_value
mrb_val_union(mrb_value v)
{
  union mrb_value x;
  x.u = v;
  return x;
}

#define mrb_tt(o)       ((enum mrb_vtype)((mrb_val_union(o).ttt & 0xfc000)>>14)-1)
#define mrb_type(o)     (enum mrb_vtype)((uint32_t)0xfff00000 < mrb_val_union(o).ttt ? mrb_tt(o) : MRB_TT_FLOAT)
#define mrb_float(o)    mrb_val_union(o).f
#define mrb_fixnum(o)   mrb_val_union(o).i
#define mrb_symbol(o)   mrb_val_union(o).sym

#ifdef MRB_64BIT
#define mrb_ptr(o)      ((void*)((((uintptr_t)0x3fffffffffff)&((uintptr_t)(mrb_val_union(o).p)))<<2))
#define mrb_cptr(o)     (((struct RCptr*)mrb_ptr(o))->p)
#define BOXNAN_SHIFT_LONG_POINTER(v) (((uintptr_t)(v)>>34)&0x3fff)
#else
#define mrb_ptr(o)      (mrb_val_union(o).p)
#define mrb_cptr(o)     mrb_ptr(o)
#define BOXNAN_SHIFT_LONG_POINTER(v) 0
#endif

#define BOXNAN_SET_VALUE(o, tt, attr, v) do { \
  union mrb_value mrb_value_union_variable; \
  mrb_value_union_variable.attr = (v);\
  mrb_value_union_variable.ttt = 0xfff00000 | (((tt)+1)<<14);\
  o = mrb_value_union_variable.u;\
} while (0)

#ifdef MRB_64BIT
#define BOXNAN_SET_OBJ_VALUE(o, tt, v) do {\
  union mrb_value mrb_value_union_variable;\
  mrb_value_union_variable.p = (void*)((uintptr_t)(v)>>2);\
  mrb_value_union_variable.ttt = (0xfff00000|(((tt)+1)<<14)|BOXNAN_SHIFT_LONG_POINTER(v));\
  o = mrb_value_union_variable.u;\
} while (0)
#else
#define BOXNAN_SET_OBJ_VALUE(o, tt, v) BOXNAN_SET_VALUE(o, tt, p, v)
#endif

#define SET_FLOAT_VALUE(mrb,r,v) do { \
  union mrb_value mrb_value_union_variable; \
  if ((v) != (v)) { /* NaN */ \
    mrb_value_union_variable.ttt = 0x7ff80000; \
    mrb_value_union_variable.i = 0; \
  } \
  else { \
    mrb_value_union_variable.f = (v); \
  } \
  r = mrb_value_union_variable.u; \
} while(0)

#define SET_NIL_VALUE(r) BOXNAN_SET_VALUE(r, MRB_TT_FALSE, i, 0)
#define SET_FALSE_VALUE(r) BOXNAN_SET_VALUE(r, MRB_TT_FALSE, i, 1)
#define SET_TRUE_VALUE(r) BOXNAN_SET_VALUE(r, MRB_TT_TRUE, i, 1)
#define SET_BOOL_VALUE(r,b) BOXNAN_SET_VALUE(r, b ? MRB_TT_TRUE : MRB_TT_FALSE, i, 1)
#define SET_INT_VALUE(r,n) BOXNAN_SET_VALUE(r, MRB_TT_FIXNUM, i, (n))
#define SET_SYM_VALUE(r,v) BOXNAN_SET_VALUE(r, MRB_TT_SYMBOL, sym, (v))
#define SET_OBJ_VALUE(r,v) BOXNAN_SET_OBJ_VALUE(r, (((struct RObject*)(v))->tt), (v))
#ifdef MRB_64BIT
MRB_API mrb_value mrb_nan_boxing_cptr_value(struct mrb_state*, void*);
#define SET_CPTR_VALUE(mrb,r,v) ((r) = mrb_nan_boxing_cptr_value(mrb, v))
#else
#define SET_CPTR_VALUE(mrb,r,v) BOXNAN_SET_VALUE(r, MRB_TT_CPTR, p, v)
#endif
#define SET_UNDEF_VALUE(r) BOXNAN_SET_VALUE(r, MRB_TT_UNDEF, i, 0)

#endif  /* MRUBY_BOXING_NAN_H */
