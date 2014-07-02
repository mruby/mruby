/*
** mruby/value.h - mrb_value definition
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_VALUE_H
#define MRUBY_VALUE_H

#ifdef MRB_USE_FLOAT
  typedef float mrb_float;
# define mrb_float_to_str(buf, i) sprintf(buf, "%.7e", i)
# define str_to_mrb_float(buf) strtof(buf, NULL)
#else
  typedef double mrb_float;
# define mrb_float_to_str(buf, i) sprintf(buf, "%.16e", i)
# define str_to_mrb_float(buf) strtod(buf, NULL)
#endif

#if defined(MRB_INT16) && defined(MRB_INT64)
# error "You can't define MRB_INT16 and MRB_INT64 at the same time."
#endif

#if defined(MRB_INT64)
# if defined(MRB_NAN_BOXING) and !defined(MRB_COMPLEX)
#  error Cannot use NaN boxing without complex type when mrb_int is 64bit
# else
   typedef int64_t mrb_int;
#  define MRB_INT_BIT 64
#  ifdef MRB_WORD_BOXING
#   define MRB_INT_MIN (INT64_MIN>>MRB_FIXNUM_SHIFT)
#   define MRB_INT_MAX (INT64_MAX>>MRB_FIXNUM_SHIFT)
#  else
#   define MRB_INT_MIN INT64_MIN
#   define MRB_INT_MAX INT64_MAX
#  endif
#  define PRIdMRB_INT PRId64
#  define PRIiMRB_INT PRIi64
#  define PRIoMRB_INT PRIo64
#  define PRIxMRB_INT PRIx64
#  define PRIXMRB_INT PRIX64
# endif
#elif defined(MRB_INT16)
# ifdef MRB_WORD_BOXING
# error "MRB_INT16 is too small for MRB_WORD_BOXING."
# endif
  typedef int16_t mrb_int;
# define MRB_INT_BIT 16
# define MRB_INT_MIN INT16_MIN
# define MRB_INT_MAX INT16_MAX
#else
  typedef int32_t mrb_int;
# define MRB_INT_BIT 32
# ifdef MRB_WORD_BOXING
#  define MRB_INT_MIN (INT32_MIN>>MRB_FIXNUM_SHIFT)
#  define MRB_INT_MAX (INT32_MAX>>MRB_FIXNUM_SHIFT)
# else
#  define MRB_INT_MIN INT32_MIN
#  define MRB_INT_MAX INT32_MAX
# endif
# define PRIdMRB_INT PRId32
# define PRIiMRB_INT PRIi32
# define PRIoMRB_INT PRIo32
# define PRIxMRB_INT PRIx32
# define PRIXMRB_INT PRIX32
#endif
typedef short mrb_sym;

#ifdef _MSC_VER
# ifndef __cplusplus
#  define inline __inline
# endif
# if _MSC_VER < 1900
#  define snprintf _snprintf
# endif
# if _MSC_VER < 1800
#  include <float.h>
#  define isfinite(n) _finite(n)
#  define isnan _isnan
#  define isinf(n) (!_finite(n) && !_isnan(n))
#  define signbit(n) (_copysign(1.0, (n)) < 0.0)
#  define strtoll _strtoi64
#  define strtof (float)strtod
#  define PRId32 "I32d"
#  define PRIi32 "I32i"
#  define PRIo32 "I32o"
#  define PRIx32 "I32x"
#  define PRIX32 "I32X"
#  define PRId64 "I64d"
#  define PRIi64 "I64i"
#  define PRIo64 "I64o"
#  define PRIx64 "I64x"
#  define PRIX64 "I64X"
static const unsigned int IEEE754_INFINITY_BITS_SINGLE = 0x7F800000;
#  define INFINITY (*(float *)&IEEE754_INFINITY_BITS_SINGLE)
#  define NAN ((float)(INFINITY - INFINITY))
# else
#  include <inttypes.h>
# endif
#else
# include <inttypes.h>
#endif

typedef uint8_t mrb_bool;
struct mrb_state;

enum mrb_vtype {
  MRB_TT_FALSE = 0,   /*   0 */
  MRB_TT_FREE,        /*   1 */
  MRB_TT_TRUE,        /*   2 */
  MRB_TT_FIXNUM,      /*   3 */
  MRB_TT_SYMBOL,      /*   4 */
  MRB_TT_UNDEF,       /*   5 */
  MRB_TT_FLOAT,       /*   6 */
  MRB_TT_COMPLEX,     /*   7 */
  MRB_TT_CPTR,        /*   8 */
  MRB_TT_OBJECT,      /*   9 */
  MRB_TT_CLASS,       /*  10 */
  MRB_TT_MODULE,      /*  11 */
  MRB_TT_ICLASS,      /*  12 */
  MRB_TT_SCLASS,      /*  13 */
  MRB_TT_PROC,        /*  14 */
  MRB_TT_ARRAY,       /*  15 */
  MRB_TT_HASH,        /*  16 */
  MRB_TT_STRING,      /*  17 */
  MRB_TT_RANGE,       /*  18 */
  MRB_TT_EXCEPTION,   /*  19 */
  MRB_TT_FILE,        /*  20 */
  MRB_TT_ENV,         /*  21 */
  MRB_TT_DATA,        /*  21 */
  MRB_TT_FIBER,       /*  22 */
  MRB_TT_MAXDEFINE    /*  23 */
};

#ifdef MRB_NAN_BOXING

#ifdef MRB_USE_FLOAT
# error ---->> MRB_NAN_BOXING and MRB_USE_FLOAT conflict <<----
#endif

#define MRB_TT_HAS_BASIC  MRB_TT_OBJECT

#ifdef MRB_ENDIAN_BIG
#define MRB_ENDIAN_LOHI(a,b) a b
#else
#define MRB_ENDIAN_LOHI(a,b) b a
#endif

#define mrb_tt(o)       (enum mrb_vtype)(o).value.tt
#define mrb_ptr(o)      ((o).value.p)
#define mrb_float(o)    (o).f.real

#define MRB_SET_VALUE(o, ttt, attr, v) do {\
	(o).value.tt_nan = 0xffff;\
  (o).value.tt = ttt;\
	(o).attr = v;\
} while (0)

 /* type and value representation by nan-boxing:
  *   float : FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF FFFFFFFFFFFFFFFF
  *   other : 1111111111111111 TTTTTTTTTTTTTTTT VVVVVVVVVVVVVVVV VVVVVVVVVVVVVVVV
  */

#ifdef MRB_COMPLEX

typedef struct mrb_value {
  union {
		struct {
	    mrb_float real;
			mrb_float imag;
		} f;
    struct {
			union {
				void *p;
		    mrb_int i;
		    mrb_sym sym;
				uint64_t ensure_alignment; /* padding, not used */
			};
      MRB_ENDIAN_LOHI(
        uint16_t tt_nan;,
        uint16_t tt;
      )
    } value;
  };
} mrb_value;

#define mrb_real(o)     (o).f.real
#define mrb_imag(o)     (o).f.imag
#define mrb_type(o)     ((uint32_t)0xffff == (o).value.tt_nan ? mrb_tt(o) : MRB_TT_COMPLEX)

static inline mrb_value
mrb_float_value(struct mrb_state *mrb, mrb_float f)
{
  mrb_value v;
  (void) mrb;

  MRB_SET_VALUE(v, MRB_TT_FLOAT, f.real, f);
  return v;
}
#define mrb_float_pool(mrb,f) mrb_float_value(mrb,f)

static inline mrb_value
mrb_complex_value(struct mrb_state *mrb, mrb_float real, mrb_float imag)
{
  mrb_value v;
  (void) mrb;

  v.f.real = real;
  if (imag != imag) {
    v.value.tt_nan = 0x7ff8;
    v.value.tt = 0;
    v.value.i = 0;
  } else {
    v.f.imag = imag;
  }
  return v;
}

#else /* not MRB_COMPLEX */

#define mrb_type(o)     ((uint32_t)0xffff == (o).value.tt_nan ? mrb_tt(o) : MRB_TT_FLOAT)

typedef struct mrb_value {
  union {
		struct {
	    mrb_float real;
		} f;
    union {
      struct {
        MRB_ENDIAN_LOHI(
	        MRB_ENDIAN_LOHI(
		        uint16_t tt_nan;,
		        uint16_t tt;
					)
          ,union {
						void *p;
				    mrb_int i;
				    mrb_sym sym;
          };
        )
      };
    } value;
  };
} mrb_value;

static inline mrb_value
mrb_float_value(struct mrb_state *mrb, mrb_float f)
{
  mrb_value v;

  if (f != f) {
    v.value.tt_nan = 0x7ff8;
    v.value.tt = 0;
    v.value.i = 0;
  } else {
    v.f.real = f;
  }
  return v;
}
#define mrb_float_pool(mrb,f) mrb_float_value(mrb,f)

#endif /* not MRB_COMPLEX */

#else /* not MRB_NAN_BOXING */

#ifdef MRB_WORD_BOXING

#include <limits.h>
#define MRB_TT_HAS_BASIC  MRB_TT_FLOAT

enum mrb_special_consts {
  MRB_Qnil    = 0,
  MRB_Qfalse  = 2,
  MRB_Qtrue   = 4,
  MRB_Qundef  = 6,
};

#define MRB_FIXNUM_FLAG   0x01
#define MRB_FIXNUM_SHIFT  1
#define MRB_SYMBOL_FLAG   0x0e
#define MRB_SPECIAL_SHIFT 8

typedef union mrb_value {
  union {
    void *p;
    struct {
      unsigned int i_flag : MRB_FIXNUM_SHIFT;
      mrb_int i : (MRB_INT_BIT - MRB_FIXNUM_SHIFT);
    };
    struct {
      unsigned int sym_flag : MRB_SPECIAL_SHIFT;
      int sym : (sizeof(mrb_sym) * CHAR_BIT);
    };
    struct RBasic *bp;
    struct RFloat *fp;
    struct RComplex *cp;
    struct RCptr *vp;
  } value;
  unsigned long w;
} mrb_value;

#define mrb_ptr(o)    (o).value.p
#define mrb_float(o)  (o).value.fp->f
#define mrb_real(o)   (o).value.cp->real
#define mrb_imag(o)   (o).value.cp->imag

#define MRB_SET_VALUE(o, ttt, attr, v) do {\
  (o).w = 0;\
  (o).attr = (v);\
  switch (ttt) {\
  case MRB_TT_FALSE:  (o).w = (v) ? MRB_Qfalse : MRB_Qnil; break;\
  case MRB_TT_TRUE:   (o).w = MRB_Qtrue; break;\
  case MRB_TT_UNDEF:  (o).w = MRB_Qundef; break;\
  case MRB_TT_FIXNUM: (o).value.i_flag = MRB_FIXNUM_FLAG; break;\
  case MRB_TT_SYMBOL: (o).value.sym_flag = MRB_SYMBOL_FLAG; break;\
  default:            if ((o).value.bp) (o).value.bp->tt = ttt; break;\
  }\
} while (0)

mrb_value mrb_float_value(struct mrb_state *mrb, mrb_float f);
mrb_value mrb_float_pool(struct mrb_state *mrb, mrb_float f);
mrb_value mrb_complex_value(struct mrb_state *mrb, mrb_float real, mrb_float imag);

#else /* No MRB_xxx_BOXING */

#define MRB_TT_HAS_BASIC  MRB_TT_OBJECT

typedef struct mrb_value {
  union {
		struct {
	    mrb_float real;
#ifdef MRB_COMPLEX
			mrb_float imag;
#endif
		} f;
    void *p;
    mrb_int i;
    mrb_sym sym;
  } value;
  enum mrb_vtype tt;
} mrb_value;

#define mrb_type(o)     (o).tt
#define mrb_ptr(o)      (o).value.p
#define mrb_float(o)    (o).value.f.real

#define MRB_SET_VALUE(o, ttt, attr, v) do {\
  (o).tt = ttt;\
  (o).attr = v;\
} while (0)

static inline mrb_value
mrb_float_value(struct mrb_state *mrb, mrb_float f)
{
  mrb_value v;
  (void) mrb;

  MRB_SET_VALUE(v, MRB_TT_FLOAT, value.f.real, f);
  return v;
}
#define mrb_float_pool(mrb,f) mrb_float_value(mrb,f)

#ifdef MRB_COMPLEX
#define mrb_real(o)     (o).value.f.real
#define mrb_imag(o)     (o).value.f.imag
static inline mrb_value
mrb_complex_value(struct mrb_state *mrb, mrb_float real, mrb_float imag)
{
  mrb_value v;
  (void) mrb;

  v.tt = MRB_TT_COMPLEX;
  v.value.f.real = real;
  v.value.f.imag = imag;
  return v;
}
#endif

#endif  /* no boxing */

#endif /* not MRB_NAN_BOXING */

#ifdef MRB_WORD_BOXING

#define mrb_cptr(o) (o).value.vp->p
#define mrb_fixnum_p(o) ((o).value.i_flag == MRB_FIXNUM_FLAG)
#define mrb_undef_p(o) ((o).w == MRB_Qundef)
#define mrb_nil_p(o)  ((o).w == MRB_Qnil)
#define mrb_bool(o)   ((o).w != MRB_Qnil && (o).w != MRB_Qfalse)

#else

#define mrb_cptr(o) mrb_ptr(o)
#define mrb_fixnum_p(o) (mrb_type(o) == MRB_TT_FIXNUM)
#define mrb_undef_p(o) (mrb_type(o) == MRB_TT_UNDEF)
#define mrb_nil_p(o)  (mrb_type(o) == MRB_TT_FALSE && !(o).value.i)
#define mrb_bool(o)   (mrb_type(o) != MRB_TT_FALSE)

#endif

#define mrb_fixnum(o) (o).value.i
#define mrb_symbol(o) (o).value.sym
#define mrb_float_p(o) (mrb_type(o) == MRB_TT_FLOAT)
#define mrb_symbol_p(o) (mrb_type(o) == MRB_TT_SYMBOL)
#define mrb_array_p(o) (mrb_type(o) == MRB_TT_ARRAY)
#define mrb_string_p(o) (mrb_type(o) == MRB_TT_STRING)
#define mrb_hash_p(o) (mrb_type(o) == MRB_TT_HASH)
#define mrb_cptr_p(o) (mrb_type(o) == MRB_TT_CPTR)
#define mrb_test(o)   mrb_bool(o)
mrb_bool mrb_regexp_p(struct mrb_state*, mrb_value);

#define MRB_OBJECT_HEADER \
  enum mrb_vtype tt:8;\
  uint32_t color:3;\
  uint32_t flags:21;\
  struct RClass *c;\
  struct RBasic *gcnext

/* white: 011, black: 100, gray: 000 */
#define MRB_GC_GRAY 0
#define MRB_GC_WHITE_A 1
#define MRB_GC_WHITE_B (1 << 1)
#define MRB_GC_BLACK (1 << 2)
#define MRB_GC_WHITES (MRB_GC_WHITE_A | MRB_GC_WHITE_B)
#define MRB_GC_COLOR_MASK 7

#define paint_gray(o) ((o)->color = MRB_GC_GRAY)
#define paint_black(o) ((o)->color = MRB_GC_BLACK)
#define paint_white(o) ((o)->color = MRB_GC_WHITES)
#define paint_partial_white(s, o) ((o)->color = (s)->current_white_part)
#define is_gray(o) ((o)->color == MRB_GC_GRAY)
#define is_white(o) ((o)->color & MRB_GC_WHITES)
#define is_black(o) ((o)->color & MRB_GC_BLACK)
#define is_dead(s, o) (((o)->color & other_white_part(s) & MRB_GC_WHITES) || (o)->tt == MRB_TT_FREE)
#define flip_white_part(s) ((s)->current_white_part = other_white_part(s))
#define other_white_part(s) ((s)->current_white_part ^ MRB_GC_WHITES)

struct RBasic {
  MRB_OBJECT_HEADER;
};
#define mrb_basic_ptr(v) ((struct RBasic*)(mrb_ptr(v)))
/* obsolete macro mrb_basic; will be removed soon */
#define mrb_basic(v)     mrb_basic_ptr(v)

struct RObject {
  MRB_OBJECT_HEADER;
  struct iv_tbl *iv;
};
#define mrb_obj_ptr(v)   ((struct RObject*)(mrb_ptr(v)))
/* obsolete macro mrb_object; will be removed soon */
#define mrb_object(o) mrb_obj_ptr(o)
#define mrb_immediate_p(x) (mrb_type(x) < MRB_TT_HAS_BASIC)
#define mrb_special_const_p(x) mrb_immediate_p(x)

struct RFiber {
  MRB_OBJECT_HEADER;
  struct mrb_context *cxt;
};

#ifdef MRB_WORD_BOXING
struct RFloat {
  MRB_OBJECT_HEADER;
  mrb_float f;
};

struct RComplex {
  MRB_OBJECT_HEADER;
  mrb_float real;
  mrb_float imag;
};

struct RCptr {
  MRB_OBJECT_HEADER;
  void *p;
};

static inline enum mrb_vtype
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
#endif  /* MRB_WORD_BOXING */

static inline mrb_value
mrb_fixnum_value(mrb_int i)
{
  mrb_value v;

  MRB_SET_VALUE(v, MRB_TT_FIXNUM, value.i, i);
  return v;
}

static inline mrb_value
mrb_symbol_value(mrb_sym i)
{
  mrb_value v;

  MRB_SET_VALUE(v, MRB_TT_SYMBOL, value.sym, i);
  return v;
}

static inline mrb_value
mrb_obj_value(void *p)
{
  mrb_value v;
  struct RBasic *b = (struct RBasic*)p;

  MRB_SET_VALUE(v, b->tt, value.p, p);
  return v;
}

#ifdef MRB_WORD_BOXING
mrb_value
mrb_cptr_value(struct mrb_state *mrb, void *p);
#else
static inline mrb_value
mrb_cptr_value(struct mrb_state *mrb, void *p)
{
  mrb_value v;
  (void) mrb;

  MRB_SET_VALUE(v, MRB_TT_CPTR, value.p, p);
  return v;
}
#endif
/* obsolete macros; will be removed */
#define MRB_TT_VOIDP MRB_TT_CPTR
#define mrb_voidp_value(m,p) mrb_cptr_value((m),(p))
#define mrb_voidp(o) mrb_cptr(o)
#define mrb_voidp_p(o) mrb_cptr_p(o)

#define MRB_TT_HAS_BASIC_P(tt) ((tt) >= MRB_TT_HAS_BASIC)

static inline mrb_value
mrb_false_value(void)
{
  mrb_value v;

  MRB_SET_VALUE(v, MRB_TT_FALSE, value.i, 1);
  return v;
}

static inline mrb_value
mrb_nil_value(void)
{
  mrb_value v;

  MRB_SET_VALUE(v, MRB_TT_FALSE, value.i, 0);
  return v;
}

static inline mrb_value
mrb_true_value(void)
{
  mrb_value v;

  MRB_SET_VALUE(v, MRB_TT_TRUE, value.i, 1);
  return v;
}

static inline mrb_value
mrb_undef_value(void)
{
  mrb_value v;

  MRB_SET_VALUE(v, MRB_TT_UNDEF, value.i, 0);
  return v;
}

static inline mrb_value
mrb_bool_value(mrb_bool boolean)
{
  mrb_value v;

  MRB_SET_VALUE(v, boolean ? MRB_TT_TRUE : MRB_TT_FALSE, value.i, 1);
  return v;
}

#endif  /* MRUBY_VALUE_H */
