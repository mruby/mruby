/*
** etc.c
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/string.h>
#include <mruby/data.h>
#include <mruby/class.h>
#include <mruby/numeric.h>
#include <mruby/internal.h>

/*
 * Allocates an RData structure, initializes it with the given pointer and type,
 * and assigns it to the given class.
 */
MRB_API struct RData*
mrb_data_object_alloc(mrb_state *mrb, struct RClass *klass, void *ptr, const mrb_data_type *type)
{
  struct RData *data = MRB_OBJ_ALLOC(mrb, MRB_TT_CDATA, klass);

  data->data = ptr;
  data->type = type;

  return data;
}

/*
 * Checks if the given mrb_value is a data object (MRB_TT_CDATA) and if its
 * mrb_data_type matches the provided type.
 * Raises an error if the checks fail.
 */
MRB_API void
mrb_data_check_type(mrb_state *mrb, mrb_value obj, const mrb_data_type *type)
{
  if (!mrb_data_p(obj)) {
    mrb_check_type(mrb, obj, MRB_TT_CDATA);
  }
  if (DATA_TYPE(obj) != type) {
    const mrb_data_type *t2 = DATA_TYPE(obj);

    if (t2) {
      mrb_raisef(mrb, E_TYPE_ERROR, "wrong argument type %s (expected %s)",
                 t2->struct_name, type->struct_name);
    }
    else {
      mrb_raisef(mrb, E_TYPE_ERROR, "uninitialized %t (expected %s)",
                 obj, type->struct_name);
    }
  }
}

/*
 * Checks if the given mrb_value is a data object and if its mrb_data_type
 * matches the provided type.
 * Returns a pointer to the data if the checks pass, otherwise returns NULL.
 */
MRB_API void*
mrb_data_check_get_ptr(mrb_state *mrb, mrb_value obj, const mrb_data_type *type)
{
  if (!mrb_data_p(obj)) {
    return NULL;
  }
  if (DATA_TYPE(obj) != type) {
    return NULL;
  }
  return DATA_PTR(obj);
}

/*
 * Retrieves a pointer to the data within a data object.
 * Calls `mrb_data_check_type` to ensure the object is of the correct type,
 * raising an error if the type check fails.
 */
MRB_API void*
mrb_data_get_ptr(mrb_state *mrb, mrb_value obj, const mrb_data_type *type)
{
  mrb_data_check_type(mrb, obj, type);
  return DATA_PTR(obj);
}

/*
 * Converts an object to a symbol.
 * If the object is already a symbol, it is returned directly.
 * If the object is a string, it is interned to a symbol.
 * Otherwise, a type error is raised.
 */
MRB_API mrb_sym
mrb_obj_to_sym(mrb_state *mrb, mrb_value name)
{
  if (mrb_symbol_p(name)) return mrb_symbol(name);
  if (mrb_string_p(name)) return mrb_intern_str(mrb, name);
  mrb_raisef(mrb, E_TYPE_ERROR, "%!v is not a symbol nor a string", name);
  return 0;  /* not reached */
}

#if !defined(MRB_NO_FLOAT) && !defined(MRB_NAN_BOXING)
static mrb_int
mrb_float_id(mrb_float f)
{
  /* normalize -0.0 to 0.0 */
  if (f == 0) f = 0.0;
  return (mrb_int)mrb_byte_hash((uint8_t*)&f, sizeof(f));
}
#endif

/*
 * Returns a unique identifier (mrb_int) for the given object.
 * The method of generating the ID varies based on the object's type and
 * boxing model (NaN boxing, word boxing, or no boxing).
 */
MRB_API mrb_int
mrb_obj_id(mrb_value obj)
{
#if defined(MRB_NAN_BOXING)
#ifdef MRB_INT64
  return obj.u;
#else
  uint64_t u = obj.u;
  return (mrb_int)(u>>32)^u;
#endif
#elif defined(MRB_WORD_BOXING)
  if (!mrb_immediate_p(obj)) {
    if (mrb_integer_p(obj)) return mrb_integer(obj);
#ifndef MRB_NO_FLOAT
    if (mrb_float_p(obj)) {
      return mrb_float_id(mrb_float(obj));
    }
#endif
  }
  return (mrb_int)obj.w;
#else  /* MRB_NO_BOXING */

#define MakeID(p,t) (mrb_int)(((intptr_t)(p))^(t))

  enum mrb_vtype tt = mrb_type(obj);

  switch (tt) {
  case MRB_TT_FREE:
  case MRB_TT_UNDEF:
    return MakeID(0, tt); /* should not happen */
  case MRB_TT_FALSE:
    if (mrb_nil_p(obj))
      return MakeID(4, tt);
    else
      return MakeID(0, tt);
  case MRB_TT_TRUE:
    return MakeID(2, tt);
  case MRB_TT_SYMBOL:
    return MakeID(mrb_symbol(obj), tt);
  case MRB_TT_INTEGER:
    return MakeID(mrb_integer(obj), tt);
#ifndef MRB_NO_FLOAT
  case MRB_TT_FLOAT:
    return MakeID(mrb_float_id(mrb_float(obj)), tt);
#endif
  case MRB_TT_STRING:
  case MRB_TT_OBJECT:
  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
  case MRB_TT_ICLASS:
  case MRB_TT_SCLASS:
  case MRB_TT_PROC:
  case MRB_TT_ARRAY:
  case MRB_TT_HASH:
  case MRB_TT_RANGE:
  case MRB_TT_EXCEPTION:
  case MRB_TT_CDATA:
  case MRB_TT_ISTRUCT:
  default:
    return MakeID(mrb_ptr(obj), tt);
  }
#endif
}

#ifdef MRB_WORD_BOXING
#ifndef MRB_NO_FLOAT
/*
 * Boxes a `mrb_float` into an `mrb_value` using word boxing.
 * - If `MRB_WORDBOX_NO_INLINE_FLOAT` is defined, it allocates a new
 *   RFloat object on the heap.
 * - If `MRB_64BIT` and `MRB_USE_FLOAT32` are defined, it stores the float
 *   in the lower bits of the word, shifted and tagged.
 * - 64-bit float64: rotation encoding, lossless for exponents [-255, +256].
 * - 32-bit float32: rotation encoding, lossless for exponents [-32, +31].
 *   Floats outside the inline range are heap-allocated as RFloat.
 */

#if !defined(MRB_WORDBOX_NO_INLINE_FLOAT) && \
    (!defined(MRB_USE_FLOAT32) || !defined(MRB_64BIT))
/*
 * Rotation-based float encoding (shared between 64-bit float64 and
 * 32-bit float32 paths).
 *
 * Encode: rotl(bits - ADDEND, 3) produces a tagged value with bottom
 *         2 bits == 10 (WORDBOX_FLOAT_FLAG).
 * Decode: rotl(tagged_value, N-3) + ADDEND recovers the original bits.
 *
 * Special values (0.0, -0.0, +Inf, -Inf, NaN) are encoded as small
 * sentinel constants that also have bottom 2 bits == 10.  This avoids
 * heap allocation for these common values.
 */
#define WORDBOX_FLOAT_ROTATE      3

/* sentinel values for special floats (all have & 3 == 2) */
#define WORDBOX_FLOAT_PZERO       0x02  /* +0.0 */
#define WORDBOX_FLOAT_NZERO       0x06  /* -0.0 */
#define WORDBOX_FLOAT_PINF        0x0a  /* +Infinity */
#define WORDBOX_FLOAT_NINF        0x0e  /* -Infinity */
#define WORDBOX_FLOAT_NAN         0x12  /* NaN (all NaN bit patterns normalize to this) */
#define WORDBOX_FLOAT_SENTINEL_MAX  WORDBOX_FLOAT_NAN

#if defined(MRB_USE_FLOAT32) && !defined(MRB_64BIT)
/*
 * 32-bit + float32 rotation.
 *
 * Biased exponents [95, 158] (actual [-32, +31]) produce the correct
 * tag pattern after rotation, covering values ~2.3e-10 to ~4.3e9.
 * Out-of-range floats are heap-allocated as RFloat.
 */
#define WORDBOX_FLOAT32_EXP_MIN   95   /* biased, actual -32 */
#define WORDBOX_FLOAT32_EXP_MAX   158  /* biased, actual +31 */
#define WORDBOX_FLOAT32_ADDEND    ((uint32_t)(WORDBOX_FLOAT32_EXP_MIN - (WORDBOX_FLOAT_FLAG << 6)) << 23)

static uint32_t
wordbox_rotl32(uint32_t a, int n)
{
  return (a << n) | (a >> (32 - n));
}

static uint32_t
wordbox_float32_to_u32(float f)
{
  union { float f; uint32_t u; } u;
  u.f = f;
  return u.u;
}

static float
wordbox_u32_to_float32(uint32_t v)
{
  union { float f; uint32_t u; } u;
  u.u = v;
  return u.f;
}

#else /* 64-bit + float64 */
/*
 * 64-bit + float64 rotation.
 *
 * Biased exponents [768, 1279] (actual [-255, +256]) produce the
 * correct tag pattern.  Obscure floats whose rotation would collide
 * with a sentinel are heap-allocated instead.
 */
#define WORDBOX_FLOAT_EXP_MIN     (1023 - 255)  /* 768 */
#define WORDBOX_FLOAT_EXP_MAX     (1023 + 256)  /* 1279 */
#define WORDBOX_FLOAT_ADDEND      ((uint64_t)(WORDBOX_FLOAT_EXP_MIN - (WORDBOX_FLOAT_FLAG << 9)) << 52)

static uint64_t
wordbox_rotl64(uint64_t a, int n)
{
  return (a << n) | (a >> (64 - n));
}

static uint64_t
wordbox_float64_to_u64(double d)
{
  union { double d; uint64_t u; } u;
  u.d = d;
  return u.u;
}

static double
wordbox_u64_to_float64(uint64_t v)
{
  union { double d; uint64_t u; } u;
  u.u = v;
  return u.d;
}
#endif /* MRB_USE_FLOAT32 && !MRB_64BIT */
#endif

MRB_API mrb_value
mrb_word_boxing_float_value(mrb_state *mrb, mrb_float f)
{
  union mrb_value_ v;

#ifdef MRB_WORDBOX_NO_INLINE_FLOAT
  v.p = mrb_obj_alloc(mrb, MRB_TT_FLOAT, mrb->float_class);
  mrb_rfloat_set(v.fp, f);
  v.bp->frozen = 1;
#elif defined(MRB_64BIT) && defined(MRB_USE_FLOAT32)
  v.w = 0;
  v.f = f;
  v.w = (v.w<<2) | 2;
#elif defined(MRB_64BIT)
  {
    uint64_t bits = wordbox_float64_to_u64((double)f);
    uint64_t exp = (bits >> 52) & 0x7FF;
    if (exp == 0) {
      /* +0.0 or -0.0 (subnormals also fall here, go to heap) */
      if (bits == UINT64_C(0))
        v.w = WORDBOX_FLOAT_PZERO;
      else if (bits == UINT64_C(0x8000000000000000))
        v.w = WORDBOX_FLOAT_NZERO;
      else goto float_heap;
    }
    else if (exp == 0x7FF) {
      /* +Inf, -Inf, or NaN */
      if (bits == UINT64_C(0x7FF0000000000000))
        v.w = WORDBOX_FLOAT_PINF;
      else if (bits == UINT64_C(0xFFF0000000000000))
        v.w = WORDBOX_FLOAT_NINF;
      else
        v.w = WORDBOX_FLOAT_NAN;
    }
    else if (exp >= WORDBOX_FLOAT_EXP_MIN && exp <= WORDBOX_FLOAT_EXP_MAX) {
      uintptr_t w = (uintptr_t)wordbox_rotl64(bits - WORDBOX_FLOAT_ADDEND, WORDBOX_FLOAT_ROTATE);
      if (w <= WORDBOX_FLOAT_SENTINEL_MAX) goto float_heap;
      v.w = w;
    }
    else {
    float_heap:
      v.p = mrb_obj_alloc(mrb, MRB_TT_FLOAT, mrb->float_class);
      mrb_rfloat_set(v.fp, f);
      v.bp->frozen = 1;
    }
  }
#else
  /* 32-bit + float32: rotation encoding */
  {
    uint32_t bits = wordbox_float32_to_u32(f);
    uint32_t exp = (bits >> 23) & 0xFF;
    if (exp == 0) {
      /* +0.0 or -0.0 (subnormals also fall here, go to heap) */
      if (bits == 0u)
        v.w = WORDBOX_FLOAT_PZERO;
      else if (bits == 0x80000000u)
        v.w = WORDBOX_FLOAT_NZERO;
      else goto float_heap;
    }
    else if (exp == 0xFF) {
      /* +Inf, -Inf, or NaN */
      if (bits == 0x7F800000u)
        v.w = WORDBOX_FLOAT_PINF;
      else if (bits == 0xFF800000u)
        v.w = WORDBOX_FLOAT_NINF;
      else
        v.w = WORDBOX_FLOAT_NAN;
    }
    else if (exp >= WORDBOX_FLOAT32_EXP_MIN && exp <= WORDBOX_FLOAT32_EXP_MAX) {
      uintptr_t w = (uintptr_t)wordbox_rotl32(bits - WORDBOX_FLOAT32_ADDEND, WORDBOX_FLOAT_ROTATE);
      if (w <= WORDBOX_FLOAT_SENTINEL_MAX) goto float_heap;
      v.w = w;
    }
    else {
    float_heap:
      v.p = mrb_obj_alloc(mrb, MRB_TT_FLOAT, mrb->float_class);
      mrb_rfloat_set(v.fp, f);
      v.bp->frozen = 1;
    }
  }
#endif
  return v.value;
}


#ifndef MRB_WORDBOX_NO_INLINE_FLOAT
/*
 * Unboxes an `mrb_value` to an `mrb_float`.
 * - 64-bit + float32: right-shift by 2 to retrieve the float.
 * - 64-bit + float64 / 32-bit + float32 (rotation encoding):
 *   decode inline floats via rotation, or read from heap RFloat.
 */
MRB_API mrb_float
mrb_word_boxing_value_float(mrb_value v)
{
#if defined(MRB_64BIT) && defined(MRB_USE_FLOAT32)
  union mrb_value_ u;
  u.value = v;
  u.w >>= 2;
  return u.f;
#elif defined(MRB_64BIT)
  if ((v.w & WORDBOX_FLOAT_MASK) == WORDBOX_FLOAT_FLAG) {
    if (v.w <= WORDBOX_FLOAT_SENTINEL_MAX) {
      switch (v.w) {
      case WORDBOX_FLOAT_PZERO: return (mrb_float)( 0.0);
      case WORDBOX_FLOAT_NZERO: return (mrb_float)(-0.0);
      case WORDBOX_FLOAT_PINF:  return (mrb_float)( INFINITY);
      case WORDBOX_FLOAT_NINF:  return (mrb_float)(-INFINITY);
      case WORDBOX_FLOAT_NAN:   return (mrb_float)  NAN;
      default: break;  /* not reached */
      }
    }
    return (mrb_float)wordbox_u64_to_float64(
      wordbox_rotl64((uint64_t)v.w, 64 - WORDBOX_FLOAT_ROTATE) + WORDBOX_FLOAT_ADDEND);
  }
  else {
    union mrb_value_ u;
    u.value = v;
    return mrb_rfloat_value(u.fp);
  }
#else
  /* 32-bit + float32: rotation decoding */
  if ((v.w & WORDBOX_FLOAT_MASK) == WORDBOX_FLOAT_FLAG) {
    if (v.w <= WORDBOX_FLOAT_SENTINEL_MAX) {
      switch (v.w) {
      case WORDBOX_FLOAT_PZERO: return (mrb_float)( 0.0f);
      case WORDBOX_FLOAT_NZERO: return (mrb_float)(-0.0f);
      case WORDBOX_FLOAT_PINF:  return (mrb_float)( INFINITY);
      case WORDBOX_FLOAT_NINF:  return (mrb_float)(-INFINITY);
      case WORDBOX_FLOAT_NAN:   return (mrb_float)  NAN;
      default: break;  /* not reached */
      }
    }
    return (mrb_float)wordbox_u32_to_float32(
      wordbox_rotl32((uint32_t)v.w, 32 - WORDBOX_FLOAT_ROTATE) + WORDBOX_FLOAT32_ADDEND);
  }
  else {
    union mrb_value_ u;
    u.value = v;
    return mrb_rfloat_value(u.fp);
  }
#endif
}
#endif
#endif  /* MRB_NO_FLOAT */

/*
 * Boxes a C pointer (void*) into an `mrb_value` using word boxing.
 * It allocates an `RCptr` object, sets its internal pointer `p` to the
 * given C pointer, and then sets the `mrb_value` to this `RCptr` object.
 */
MRB_API mrb_value
mrb_word_boxing_cptr_value(mrb_state *mrb, void *p)
{
  struct RCptr *cptr = MRB_OBJ_ALLOC(mrb, MRB_TT_CPTR, mrb->object_class);
  mrb_value v;

  SET_OBJ_VALUE(v, cptr);
  cptr->p = p;
  return v;
}
#endif  /* MRB_WORD_BOXING */

#if defined(MRB_WORD_BOXING) || (defined(MRB_NAN_BOXING) && defined(MRB_INT64))
/*
 * Boxes an `mrb_int` into an `mrb_value`.
 * If the integer `n` can be represented as a fixnum (checked by `FIXABLE(n)`),
 * it returns a fixnum-tagged `mrb_value`. Otherwise, it allocates an
 * `RInteger` object on the heap, stores `n` in it, marks the object as
 * frozen, and returns an object-tagged `mrb_value`.
 * This function is used when word boxing is enabled or when NaN boxing is
 * enabled for 64-bit integers.
 */
MRB_API mrb_value
mrb_boxing_int_value(mrb_state *mrb, mrb_int n)
{
  if (FIXABLE(n)) return mrb_fixnum_value(n);
  else {
    mrb_value v;
    struct RInteger *p = (struct RInteger*)mrb_obj_alloc(mrb, MRB_TT_INTEGER, mrb->integer_class);
    p->i = n;
    p->frozen = 1;
    SET_OBJ_VALUE(v, p);
    return v;
  }
}
#endif

#if defined _MSC_VER && _MSC_VER < 1900

#ifndef va_copy
static void
mrb_msvc_va_copy(va_list *dest, va_list src)
{
  *dest = src;
}
#define va_copy(dest, src) mrb_msvc_va_copy(&(dest), src)
#endif

MRB_API int
mrb_msvc_vsnprintf(char *s, size_t n, const char *format, va_list arg)
{
  int cnt;
  va_list argcp;
  va_copy(argcp, arg);
  if (n == 0 || (cnt = _vsnprintf_s(s, n, _TRUNCATE, format, argcp)) < 0) {
    cnt = _vscprintf(format, arg);
  }
  va_end(argcp);
  return cnt;
}

MRB_API int
mrb_msvc_snprintf(char *s, size_t n, const char *format, ...)
{
  va_list arg;
  va_start(arg, format);

  int ret = mrb_msvc_vsnprintf(s, n, format, arg);
  va_end(arg);
  return ret;
}

#endif  /* defined _MSC_VER && _MSC_VER < 1900 */
