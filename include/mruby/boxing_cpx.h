/*
** mruby/complex.h - mrb_value for complex numbers
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_BOXING_CPX_H
#define MRUBY_BOXING_CPX_H

#ifdef MRB_NAN_BOXING
# error MRB_COMPLEX not compatible with MRB_NAN_BOXING
#endif

#if defined(MRB_USE_FLOAT) && defined(MRB_INT64)
# error MRB_COMPLEX boxing not usable with both MRB_USE_FLOAT and MRB_INT64
#endif

#define MRB_FIXNUM_SHIFT 0
#define MRB_TT_HAS_BASIC  MRB_TT_OBJECT
#define MRB_VALUE_NIL_NONZERO

/* When not word boxing, Complex() numbers are represented by boxing
 * the imaginary number so it can also represent the value type.
 * For example, the float version:
 *   imaginary : FFFFFFFF FFFFFFFF FFFFFFFF FFFFFFFF
 *   mrb_vtype : 11111111 11111111 TTTTTTTT TTTTTTTT
 * The double version:
 *   imaginary : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
 *   mrb_vtype : 1111111111111111 1111111111111111 TTTTTTTTTTTTTTTT TTTTTTTTTTTTTTTT
 * The real part is a union of all types.
 */

#ifdef MRB_USE_FLOAT

typedef struct mrb_value {
  union {
    mrb_float f;
    void *p;
    mrb_int i;
    mrb_sym sym;
  } value;
  union {
    mrb_float fi;
    struct {
      MRB_ENDIAN_LOHI (
        uint16_t tt_nan;,
        uint16_t tt;
        )
    };
  };
} mrb_value;

#define mrb_complex_p(o) (0xFFFF != (o).tt_nan)
#define mrb_type(o)     (0xFFFF == (o).tt_nan ? (o).tt : MRB_TT_COMPLEX)

#define BOXCPX_SET_VALUE(o, ttt, attr, v) do {\
  (o).tt_nan = 0xFFFF;\
  (o).tt = ttt;\
  (o).attr = v;\
} while (0)

#define SET_COMPLEX_VALUE(mrb, o, real, imag) do {\
  if (imag != imag) {\
    (o).tt_nan = 0x7FC0;\
    (o).tt = 0;\
  } else {\
    (o).fi = imag;\
  }\
  (o).value.f = real;\
} while(0)

#else

typedef struct mrb_value {
  union {
    mrb_float f;
    void *p;
    mrb_int i;
    mrb_sym sym;
  } value;
  union {
    mrb_float fi;
    struct {
      MRB_ENDIAN_LOHI (
        uint32_t tt_nan;,
        uint32_t tt;
        )
    };
  };
} mrb_value;

#define mrb_complex_p(o) (0xFFFFFFFF != (o).tt_nan)
#define mrb_type(o)     (0xFFFFFFFF == (o).tt_nan ? (o).tt : MRB_TT_COMPLEX)

#define BOXCPX_SET_VALUE(o, ttt, attr, v) do {\
  (o).tt_nan = 0xFFFFFFFF;\
  (o).tt = ttt;\
  (o).attr = v;\
} while (0)

#define SET_COMPLEX_VALUE(mrb, o, real, imag) do {\
  if (imag != imag) {\
    (o).tt_nan = 0x7FF80000;\
    (o).tt = 0;\
  } else {\
    (o).fi = imag;\
  }\
  (o).value.f = real;\
} while(0)

#endif

#define mrb_float_pool(mrb,f) mrb_float_value(mrb,f)

#define mrb_ptr(o)      (o).value.p
#define mrb_cptr(o)     mrb_ptr(o)
#define mrb_float(o)    (o).value.f
#define mrb_fixnum(o)   (o).value.i
#define mrb_symbol(o)   (o).value.sym
#define mrb_real(o)     (o).value.f
#define mrb_imag(o)     (o).fi

#define SET_NIL_VALUE(r) BOXCPX_SET_VALUE(r, MRB_TT_FALSE, value.i, 0)
#define SET_FALSE_VALUE(r) BOXCPX_SET_VALUE(r, MRB_TT_FALSE, value.i, 1)
#define SET_TRUE_VALUE(r) BOXCPX_SET_VALUE(r, MRB_TT_TRUE, value.i, 1)
#define SET_BOOL_VALUE(r,b) BOXCPX_SET_VALUE(r, b ? MRB_TT_TRUE : MRB_TT_FALSE, value.i, 1)
#define SET_INT_VALUE(r,n) BOXCPX_SET_VALUE(r, MRB_TT_FIXNUM, value.i, (n))
#define SET_FLOAT_VALUE(mrb,r,v) BOXCPX_SET_VALUE(r, MRB_TT_FLOAT, value.f, (v))
#define SET_SYM_VALUE(r,v) BOXCPX_SET_VALUE(r, MRB_TT_SYMBOL, value.sym, (v))
#define SET_OBJ_VALUE(r,v) BOXCPX_SET_VALUE(r, (((struct RObject*)(v))->tt), value.p, (v))
#define SET_PROC_VALUE(r,v) BOXCPX_SET_VALUE(r, MRB_TT_PROC, value.p, v)
#define SET_CPTR_VALUE(mrb,r,v) BOXCPX_SET_VALUE(r, MRB_TT_CPTR, value.p, v)
#define SET_UNDEF_VALUE(r) BOXCPX_SET_VALUE(r, MRB_TT_UNDEF, value.i, 0)

#endif /* MRUBY_BOXING_CPX_H */
