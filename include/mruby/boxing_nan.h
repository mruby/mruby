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

struct mrb_value {
  union {
    mrb_float f;
    uint64_t u;
    union {
      void *p;
      struct {
        MRB_ENDIAN_LOHI(
          uint32_t ttt;
          ,union {
            mrb_int i;
            mrb_sym sym;
          };
        )
      };
    } value;
  };
};

static inline struct mrb_value
mrb_val_stru(mrb_value v)
{
  struct mrb_value mrb_value_struct_variable = {.u = v};
  return mrb_value_struct_variable;
}

#define mrb_tt(o)       ((enum mrb_vtype)((mrb_val_stru(o).value.ttt & 0xfc000)>>14)-1)
#define mrb_type(o)     (enum mrb_vtype)((uint32_t)0xfff00000 < mrb_val_stru(o).value.ttt ? mrb_tt(o) : MRB_TT_FLOAT)
#define mrb_ptr(o)      ((void*)((((uintptr_t)0x3fffffffffff)&((uintptr_t)(mrb_val_stru(o).value.p)))<<2))
#define mrb_float(o)    mrb_val_stru(o).f
#define mrb_fixnum(o)   mrb_val_stru(o).value.i
#define mrb_symbol(o)   mrb_val_stru(o).value.sym

#ifdef MRB_64BIT
MRB_API mrb_value mrb_nan_boxing_cptr_value(struct mrb_state*, void*);
#define mrb_cptr(o)     (((struct RCptr*)mrb_ptr(o))->p)
#define BOXNAN_SHIFT_LONG_POINTER(v) (((uintptr_t)(v)>>34)&0x3fff)
#else
#define mrb_cptr(o)     (mrb_val_stru(o).value.p)
#define BOXNAN_SHIFT_LONG_POINTER(v) 0
#endif

#define BOXNAN_SET_VALUE(o, tt, attr, v) do { \
  struct mrb_value mrb_value_struct_variable; \
  mrb_value_struct_variable.attr = (v);\
  mrb_value_struct_variable.value.ttt = 0xfff00000 | (((tt)+1)<<14);\
  o = mrb_value_struct_variable.u;\
} while (0)

#define BOXNAN_SET_OBJ_VALUE(o, tt, v) do {\
  struct mrb_value mrb_value_struct_variable;\
  mrb_value_struct_variable.value.p = (void*)((uintptr_t)(v)>>2);\
  mrb_value_struct_variable.value.ttt = (0xfff00000|(((tt)+1)<<14)|BOXNAN_SHIFT_LONG_POINTER(v));\
  o = mrb_value_struct_variable.u;\
} while (0)

#define SET_FLOAT_VALUE(mrb,r,v) do { \
  struct mrb_value mrb_value_struct_variable; \
  if ((v) != (v)) { /* NaN */ \
    mrb_value_struct_variable.value.ttt = 0x7ff80000; \
    mrb_value_struct_variable.value.i = 0; \
  } \
  else { \
    mrb_value_struct_variable.f = (v); \
  } \
  r = mrb_value_struct_variable.u; \
} while(0)

#define SET_NIL_VALUE(r) BOXNAN_SET_VALUE(r, MRB_TT_FALSE, value.i, 0)
#define SET_FALSE_VALUE(r) BOXNAN_SET_VALUE(r, MRB_TT_FALSE, value.i, 1)
#define SET_TRUE_VALUE(r) BOXNAN_SET_VALUE(r, MRB_TT_TRUE, value.i, 1)
#define SET_BOOL_VALUE(r,b) BOXNAN_SET_VALUE(r, b ? MRB_TT_TRUE : MRB_TT_FALSE, value.i, 1)
#define SET_INT_VALUE(r,n) BOXNAN_SET_VALUE(r, MRB_TT_FIXNUM, value.i, (n))
#define SET_SYM_VALUE(r,v) BOXNAN_SET_VALUE(r, MRB_TT_SYMBOL, value.sym, (v))
#define SET_OBJ_VALUE(r,v) BOXNAN_SET_OBJ_VALUE(r, (((struct RObject*)(v))->tt), (v))
#ifdef MRB_64BIT
#define SET_CPTR_VALUE(mrb,r,v) ((r) = mrb_nan_boxing_cptr_value(mrb, v))
#else
#define SET_CPTR_VALUE(mrb,r,v) BOXNAN_SET_VALUE(r, MRB_TT_CPTR, value.p, v)
#endif
#define SET_UNDEF_VALUE(r) BOXNAN_SET_VALUE(r, MRB_TT_UNDEF, value.i, 0)

#endif  /* MRUBY_BOXING_NAN_H */
