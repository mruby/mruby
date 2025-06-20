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
 * - If `MRB_WORDBOX_NO_FLOAT_TRUNCATE` is defined, it allocates a new
 *   RFloat object on the heap.
 * - If `MRB_64BIT` and `MRB_USE_FLOAT32` are defined, it stores the float
 *   in the lower bits of the word, shifted and tagged.
 * - Otherwise, it stores the float directly in the word, tagged.
 */
MRB_API mrb_value
mrb_word_boxing_float_value(mrb_state *mrb, mrb_float f)
{
  union mrb_value_ v;

#ifdef MRB_WORDBOX_NO_FLOAT_TRUNCATE
  v.p = mrb_obj_alloc(mrb, MRB_TT_FLOAT, mrb->float_class);
  v.fp->f = f;
  v.bp->frozen = 1;
#elif defined(MRB_64BIT) && defined(MRB_USE_FLOAT32)
  v.w = 0;
  v.f = f;
  v.w = (v.w<<2) | 2;
#else
  v.f = f;
  v.w = (v.w & ~3) | 2;
#endif
  return v.value;
}


#ifndef MRB_WORDBOX_NO_FLOAT_TRUNCATE
/*
 * Unboxes an `mrb_value` to an `mrb_float` when word boxing is used and
 * float truncation (`MRB_WORDBOX_NO_FLOAT_TRUNCATE`) is not disabled.
 * The function extracts the float value from the `mrb_value`'s union
 * representation (`u.value`).
 * - If `MRB_64BIT` and `MRB_USE_FLOAT32` are defined, the word (`u.w`)
 *   is right-shifted by 2 bits to retrieve the float.
 * - Otherwise, the lower 2 bits of the word (`u.w`) are cleared to
 *   retrieve the float.
 */
MRB_API mrb_float
mrb_word_boxing_value_float(mrb_value v)
{
  union mrb_value_ u;
  u.value = v;
#if defined(MRB_64BIT) && defined(MRB_USE_FLOAT32)
  u.w >>= 2;
#else
  u.w &= ~3;
#endif
  return u.f;
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
  mrb_value v;
  struct RCptr *cptr = MRB_OBJ_ALLOC(mrb, MRB_TT_CPTR, mrb->object_class);

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
