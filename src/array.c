/*
** array.c - Array class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/string.h>
#include <mruby/range.h>
#include <mruby/proc.h>
#include <mruby/internal.h>
#include <mruby/presym.h>
#include "value_array.h"

#define ARY_DEFAULT_LEN   4
#define ARY_SHRINK_RATIO  5 /* must be larger than 2 */
#define ARY_C_MAX_SIZE (SIZE_MAX / sizeof(mrb_value))
#ifndef MRB_ARY_LENGTH_MAX
#define MRB_ARY_LENGTH_MAX 131072
#endif
#define ARY_MAX_SIZE ((mrb_int)((ARY_C_MAX_SIZE < (size_t)MRB_INT_MAX) ? ARY_C_MAX_SIZE : MRB_INT_MAX-1))

static void
ary_too_big(mrb_state *mrb)
{
  mrb_raise(mrb, E_ARGUMENT_ERROR, "array size too big");
}

static inline void
ary_check_too_big(mrb_state *mrb, mrb_int a, mrb_int b)
{
  if (a > ARY_MAX_SIZE - b || a < 0)
    ary_too_big(mrb);
#if MRB_ARY_LENGTH_MAX != 0
  if (a > MRB_ARY_LENGTH_MAX - b || a < 0)
    ary_too_big(mrb);
#endif
}

static struct RArray*
ary_new_capa(mrb_state *mrb, mrb_int capa)
{
  ary_check_too_big(mrb, capa, 0);

  size_t blen = capa * sizeof(mrb_value);
  struct RArray *a = MRB_OBJ_ALLOC(mrb, MRB_TT_ARRAY, mrb->array_class);

  if (capa <= MRB_ARY_EMBED_LEN_MAX) {
    ARY_SET_EMBED_LEN(a, 0);
  }
  else {
    a->as.heap.ptr = (mrb_value *)mrb_malloc(mrb, blen);
    a->as.heap.aux.capa = capa;
    a->as.heap.len = 0;
  }

  return a;
}

/**
 * Creates a new array with a specified initial capacity.
 *
 * This function allocates an array that can hold at least `capa` elements
 * without needing to immediately reallocate memory. If `capa` is 0,
 * it may still allocate a small default capacity.
 *
 * @param mrb The mruby state.
 * @param capa The initial capacity desired for the array.
 * @return A new mrb_value representing the created array.
 */
MRB_API mrb_value
mrb_ary_new_capa(mrb_state *mrb, mrb_int capa)
{
  struct RArray *a = ary_new_capa(mrb, capa);
  return mrb_obj_value(a);
}

/**
 * Creates a new, empty array.
 *
 * This function is equivalent to calling `mrb_ary_new_capa` with a capacity of 0.
 * The array will dynamically resize as elements are added.
 *
 * @param mrb The mruby state.
 * @return A new mrb_value representing the created empty array.
 */
MRB_API mrb_value
mrb_ary_new(mrb_state *mrb)
{
  return mrb_ary_new_capa(mrb, 0);
}

/*
 * To copy array, use this instead of memcpy because of portability
 * * gcc on ARM may fail optimization of memcpy
 *   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=56620
 * * gcc on MIPS also fail
 *   https://gcc.gnu.org/bugzilla/show_bug.cgi?id=39755
 * * memcpy doesn't exist on freestanding environment
 *
 * If you optimize for binary size, use memcpy instead of this at your own risk
 * of above portability issue.
 *
 * See also https://togetter.com/li/462898 (Japanese)
 */
static inline void
array_copy(mrb_value *dst, const mrb_value *src, mrb_int size)
{
  for (mrb_int i = 0; i < size; i++) {
    dst[i] = src[i];
  }
}

static struct RArray*
ary_new_from_values(mrb_state *mrb, mrb_int size, const mrb_value *vals)
{
  struct RArray *a = ary_new_capa(mrb, size);

  array_copy(ARY_PTR(a), vals, size);
  ARY_SET_LEN(a, size);

  return a;
}

/**
 * Creates a new array initialized with a given sequence of values.
 *
 * This function allocates an array and copies `size` elements from the `vals`
 * pointer into the new array.
 *
 * @param mrb The mruby state.
 * @param size The number of values to initialize the array with.
 * @param vals A pointer to an array of `mrb_value`s to copy into the new array.
 * @return A new mrb_value representing the created array.
 */
MRB_API mrb_value
mrb_ary_new_from_values(mrb_state *mrb, mrb_int size, const mrb_value *vals)
{
  struct RArray *a = ary_new_from_values(mrb, size, vals);
  return mrb_obj_value(a);
}

/**
 * Creates a new array of size 2, typically used to represent an association (key-value pair).
 *
 * The first element of the array is `car` (often the key), and the second element
 * is `cdr` (often the value).
 *
 * @param mrb The mruby state.
 * @param car The first value to be placed in the array.
 * @param cdr The second value to be placed in the array.
 * @return A new mrb_value representing the created 2-element array.
 */
MRB_API mrb_value
mrb_assoc_new(mrb_state *mrb, mrb_value car, mrb_value cdr)
{
  struct RArray *a = ary_new_capa(mrb, 2);
  mrb_value *p = ARY_PTR(a);

  p[0] = car;
  p[1] = cdr;
  ARY_SET_LEN(a, 2);
  return mrb_obj_value(a);
}

static void
ary_fill_with_nil(mrb_value *ptr, mrb_int size)
{
  mrb_value nil = mrb_nil_value();

  while (size--) {
    *ptr++ = nil;
  }
}

#define ary_modify_check(mrb, a) mrb_check_frozen((mrb), (a))

static void
ary_modify(mrb_state *mrb, struct RArray *a)
{
  ary_modify_check(mrb, a);

  if (ARY_SHARED_P(a)) {
    mrb_shared_array *shared = a->as.heap.aux.shared;

    if (shared->refcnt == 1 && a->as.heap.ptr == shared->ptr) {
      a->as.heap.ptr = shared->ptr;
      a->as.heap.aux.capa = a->as.heap.len;
      mrb_free(mrb, shared);
    }
    else {
      mrb_value *p = a->as.heap.ptr;
      mrb_value *ptr = (mrb_value*)mrb_malloc(mrb, a->as.heap.len * sizeof(mrb_value));

      if (p) {
        array_copy(ptr, p, a->as.heap.len);
      }
      a->as.heap.ptr = ptr;
      a->as.heap.aux.capa = a->as.heap.len;
      mrb_ary_decref(mrb, shared);
    }
    ARY_UNSET_SHARED_FLAG(a);
  }
}

/**
 * Prepares an array for modification.
 *
 * This function ensures that the array is not frozen and is not shared.
 * If the array is shared and has multiple references, this function will
 * duplicate the array data to ensure that modifications do not affect
 * other references. It also triggers a write barrier for the garbage collector.
 *
 * @param mrb The mruby state.
 * @param a A pointer to the RArray structure to modify.
 */
MRB_API void
mrb_ary_modify(mrb_state *mrb, struct RArray* a)
{
  mrb_write_barrier(mrb, (struct RBasic*)a);
  ary_modify(mrb, a);
}

static void
ary_make_shared(mrb_state *mrb, struct RArray *a)
{
  if (!ARY_SHARED_P(a) && !ARY_EMBED_P(a)) {
    mrb_shared_array *shared = (mrb_shared_array*)mrb_malloc(mrb, sizeof(mrb_shared_array));
    mrb_value *ptr = a->as.heap.ptr;
    mrb_int len = a->as.heap.len;

    shared->refcnt = 1;
    if (a->as.heap.aux.capa > len) {
      a->as.heap.ptr = shared->ptr = (mrb_value*)mrb_realloc(mrb, ptr, sizeof(mrb_value)*len+1);
    }
    else {
      shared->ptr = ptr;
    }
    shared->len = len;
    a->as.heap.aux.shared = shared;
    ARY_SET_SHARED_FLAG(a);
  }
}

static void
ary_expand_capa(mrb_state *mrb, struct RArray *a, mrb_int len)
{
  mrb_int capa = ARY_CAPA(a);

  ary_check_too_big(mrb, len, 0);
  if (capa < ARY_DEFAULT_LEN) {
    capa = ARY_DEFAULT_LEN;
  }
  while (capa < len) {
    if (capa <= ARY_MAX_SIZE / 2) {
      capa *= 2;
    }
    else {
      capa = len;
    }
  }
  if (capa > ARY_MAX_SIZE) {
    ary_too_big(mrb);
  }

  if (ARY_EMBED_P(a)) {
    mrb_value *ptr = ARY_EMBED_PTR(a);
    mrb_int slen = ARY_EMBED_LEN(a);
    mrb_value *expanded_ptr = (mrb_value*)mrb_malloc(mrb, sizeof(mrb_value)*capa);

    ARY_UNSET_EMBED_FLAG(a);
    array_copy(expanded_ptr, ptr, slen);
    a->as.heap.len = slen;
    a->as.heap.aux.capa = capa;
    a->as.heap.ptr = expanded_ptr;
  }
  else if (capa > a->as.heap.aux.capa) {
    mrb_value *expanded_ptr = (mrb_value*)mrb_realloc(mrb, a->as.heap.ptr, sizeof(mrb_value)*capa);

    a->as.heap.aux.capa = capa;
    a->as.heap.ptr = expanded_ptr;
  }
}

static void
ary_shrink_capa(mrb_state *mrb, struct RArray *a)
{
  if (ARY_EMBED_P(a)) return;

  mrb_int capa = a->as.heap.aux.capa;

  if (capa < ARY_DEFAULT_LEN * 2) return;
  if (capa <= a->as.heap.len * ARY_SHRINK_RATIO) return;

  do {
    capa /= 2;
    if (capa < ARY_DEFAULT_LEN) {
      capa = ARY_DEFAULT_LEN;
      break;
    }
  } while (capa > a->as.heap.len * ARY_SHRINK_RATIO);

  if (capa > a->as.heap.len && capa < a->as.heap.aux.capa) {
    a->as.heap.aux.capa = capa;
    a->as.heap.ptr = (mrb_value*)mrb_realloc(mrb, a->as.heap.ptr, sizeof(mrb_value)*capa);
  }
}

/**
 * Resizes an array to a new length.
 *
 * If `new_len` is smaller than the current length, the array is truncated.
 * If `new_len` is larger than the current length, the array is expanded,
 * and new elements are filled with `nil`.
 * This function modifies the array in place.
 *
 * @param mrb The mruby state.
 * @param ary The array (mrb_value) to resize.
 * @param new_len The desired new length of the array.
 * @return The resized array (the same mrb_value as `ary`).
 */
MRB_API mrb_value
mrb_ary_resize(mrb_state *mrb, mrb_value ary, mrb_int new_len)
{
  struct RArray *a = mrb_ary_ptr(ary);

  ary_modify(mrb, a);
  mrb_int old_len = RARRAY_LEN(ary);
  if (old_len != new_len) {
    if (new_len < old_len) {
      ary_shrink_capa(mrb, a);
    }
    else {
      ary_expand_capa(mrb, a, new_len);
      ary_fill_with_nil(ARY_PTR(a) + old_len, new_len - old_len);
    }
    ARY_SET_LEN(a, new_len);
  }

  return ary;
}

static mrb_value
mrb_ary_s_create(mrb_state *mrb, mrb_value klass)
{
  const mrb_value *vals;
  mrb_int len;

  mrb_get_args(mrb, "*!", &vals, &len);
  mrb_value ary = mrb_ary_new_from_values(mrb, len, vals);
  struct RArray *a = mrb_ary_ptr(ary);
  a->c = mrb_class_ptr(klass);

  return ary;
}

static void ary_replace(mrb_state*, struct RArray*, struct RArray*);

static mrb_value
mrb_ary_init(mrb_state *mrb, mrb_value ary)
{
  mrb_value ss = mrb_fixnum_value(0);
  mrb_value obj = mrb_nil_value();
  mrb_value blk = mrb_nil_value();

  mrb_get_args(mrb, "|oo&", &ss, &obj, &blk);

  if (mrb_array_p(ss) && mrb_nil_p(obj) && mrb_nil_p(blk)) {
    ary_replace(mrb, mrb_ary_ptr(ary), mrb_ary_ptr(ss));
    return ary;
  }

  mrb_int size = mrb_as_int(mrb, ss);
  struct RArray *a = mrb_ary_ptr(ary);

  if (ARY_CAPA(a) < size) {
    ary_expand_capa(mrb, a, size);
  }

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i=0; i<size; i++) {
    mrb_value val;
    if (mrb_nil_p(blk)) {
      val = obj;
    }
    else {
      val = mrb_yield(mrb, blk, mrb_fixnum_value(i));
    }
    mrb_ary_set(mrb, ary, i, val);
    mrb_gc_arena_restore(mrb, ai); // for mrb_funcall
  }
  return ary;
}

static void
ary_concat(mrb_state *mrb, struct RArray *a, struct RArray *a2)
{
  mrb_int len = ARY_LEN(a);

  if (len == 0) {
    ary_replace(mrb, a, a2);
    return;
  }

  mrb_int len2 = ARY_LEN(a2);
  ary_check_too_big(mrb, len2, len);
  ary_modify(mrb, a);

  mrb_int newlen = len + len2;
  if (ARY_CAPA(a) < newlen) {
    ary_expand_capa(mrb, a, newlen);
  }
  array_copy(ARY_PTR(a)+len, ARY_PTR(a2), len2);
  mrb_write_barrier(mrb, (struct RBasic*)a);
  ARY_SET_LEN(a, newlen);
}

/**
 * Concatenates one array to another.
 *
 * Appends all elements from the `other` array to the `self` array.
 * This function modifies the `self` array in place.
 *
 * @param mrb The mruby state.
 * @param self The array (mrb_value) to which elements will be added.
 * @param other The array (mrb_value) whose elements will be appended.
 */
MRB_API void
mrb_ary_concat(mrb_state *mrb, mrb_value self, mrb_value other)
{
  struct RArray *a2 = mrb_ary_ptr(other);

  ary_concat(mrb, mrb_ary_ptr(self), a2);
}

/*
 *  call-seq:
 *    array.concat(*other_arrays) -> self
 *
 *  Adds to +array+ all elements from each \Array in +other_arrays+; returns +self+:
 *
 *    a = [0, 1]
 *    a.concat([2, 3], [4, 5]) # => [0, 1, 2, 3, 4, 5]
 */

static mrb_value
mrb_ary_concat_m(mrb_state *mrb, mrb_value self)
{
  mrb_value *args;
  mrb_int len;

  mrb_get_args(mrb, "*!", &args, &len);
  for (int i=0; i<len; i++) {
    mrb_ensure_array_type(mrb, args[i]);
  }
  for (int i=0; i<len; i++) {
    mrb_ary_concat(mrb, self, args[i]);
  }
  return self;
}

static mrb_value
mrb_ary_plus(mrb_state *mrb, mrb_value self)
{
  struct RArray *a1 = mrb_ary_ptr(self);
  const mrb_value *ptr;
  mrb_int blen;

  mrb_get_args(mrb, "a", &ptr, &blen);
  ary_check_too_big(mrb, ARY_LEN(a1), blen);
  mrb_int len1 = ARY_LEN(a1);
  struct RArray *a2 = ary_new_capa(mrb, len1 + blen);
  array_copy(ARY_PTR(a2), ARY_PTR(a1), len1);
  array_copy(ARY_PTR(a2) + len1, ptr, blen);
  ARY_SET_LEN(a2, len1+blen);

  return mrb_obj_value(a2);
}

#define ARY_REPLACE_SHARED_MIN 20

static void
ary_replace(mrb_state *mrb, struct RArray *a, struct RArray *b)
{
  mrb_int len = ARY_LEN(b);

  ary_modify_check(mrb, a);
  if (a == b) return;
  if (ARY_SHARED_P(a)) {
    mrb_ary_decref(mrb, a->as.heap.aux.shared);
    a->as.heap.aux.capa = 0;
    a->as.heap.len = 0;
    a->as.heap.ptr = NULL;
    ARY_UNSET_SHARED_FLAG(a);
  }
  if (ARY_SHARED_P(b)) {
  shared_b:
    if (ARY_EMBED_P(a)) {
      ARY_UNSET_EMBED_FLAG(a);
    }
    else {
      mrb_free(mrb, a->as.heap.ptr);
    }
    a->as.heap.ptr = b->as.heap.ptr;
    a->as.heap.len = len;
    a->as.heap.aux.shared = b->as.heap.aux.shared;
    a->as.heap.aux.shared->refcnt++;
    ARY_SET_SHARED_FLAG(a);
    mrb_write_barrier(mrb, (struct RBasic*)a);
    return;
  }
  if (!mrb_frozen_p(b) && len > ARY_REPLACE_SHARED_MIN) {
    ary_make_shared(mrb, b);
    goto shared_b;
  }
  if (ARY_CAPA(a) < len)
    ary_expand_capa(mrb, a, len);
  array_copy(ARY_PTR(a), ARY_PTR(b), len);
  mrb_write_barrier(mrb, (struct RBasic*)a);
  ARY_SET_LEN(a, len);
}

/**
 * Replaces the contents of an array with the contents of another array.
 *
 * After this operation, the `self` array will contain the same elements
 * as the `other` array. This function modifies the `self` array in place.
 *
 * @param mrb The mruby state.
 * @param self The array (mrb_value) whose contents will be replaced.
 * @param other The array (mrb_value) from which to copy the elements.
 */
MRB_API void
mrb_ary_replace(mrb_state *mrb, mrb_value self, mrb_value other)
{
  struct RArray *a1 = mrb_ary_ptr(self);
  struct RArray *a2 = mrb_ary_ptr(other);

  if (a1 != a2) {
    ary_replace(mrb, a1, a2);
  }
}

static mrb_value
mrb_ary_replace_m(mrb_state *mrb, mrb_value self)
{
  mrb_value other;

  mrb_get_args(mrb, "A", &other);
  mrb_ary_replace(mrb, self, other);

  return self;
}

static mrb_value
mrb_ary_times(mrb_state *mrb, mrb_value self)
{
  struct RArray *a1 = mrb_ary_ptr(self);

  mrb_value arg = mrb_get_arg1(mrb);
  mrb_value tmp = mrb_check_string_type(mrb, arg);
  if (!mrb_nil_p(tmp)) {
    return mrb_ary_join(mrb, self, tmp);
  }

  mrb_int times = mrb_as_int(mrb, arg);
  if (times < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative argument");
  }
  if (times == 0) return mrb_ary_new(mrb);
  if (ARY_MAX_SIZE / times < ARY_LEN(a1)) {
    ary_too_big(mrb);
  }

  mrb_int len1 = ARY_LEN(a1);
  struct RArray *a2 = ary_new_capa(mrb, len1 * times);
  ARY_SET_LEN(a2, len1 * times);

  mrb_value *ptr = ARY_PTR(a2);
  while (times--) {
    array_copy(ptr, ARY_PTR(a1), len1);
    ptr += len1;
  }

  return mrb_obj_value(a2);
}

static mrb_value
mrb_ary_reverse_bang(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int len = ARY_LEN(a);

  if (len > 1) {
    ary_modify(mrb, a);

    mrb_value *p1 = ARY_PTR(a);
    mrb_value *p2 = p1 + len - 1;

    while (p1 < p2) {
      mrb_value tmp = *p1;
      *p1++ = *p2;
      *p2-- = tmp;
    }
  }
  return self;
}

static mrb_value
mrb_ary_reverse(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self), *b = ary_new_capa(mrb, ARY_LEN(a));
  mrb_int len = ARY_LEN(a);

  if (len > 0) {
    mrb_value *p1 = ARY_PTR(a);
    mrb_value *e  = p1 + len;
    mrb_value *p2 = ARY_PTR(b) + len - 1;
    while (p1 < e) {
      *p2-- = *p1++;
    }
    ARY_SET_LEN(b, len);
  }
  return mrb_obj_value(b);
}

/**
 * Pushes an element onto the end of an array.
 *
 * This function appends `elem` to the `ary` array, increasing its length by one.
 * The array capacity may be expanded if necessary.
 * This function modifies the array in place.
 *
 * @param mrb The mruby state.
 * @param ary The array (mrb_value) to push the element onto.
 * @param elem The mrb_value to append to the array.
 */
MRB_API void
mrb_ary_push(mrb_state *mrb, mrb_value ary, mrb_value elem)
{
  struct RArray *a = mrb_ary_ptr(ary);
  mrb_int len = ARY_LEN(a);

  ary_modify(mrb, a);
  if (len == ARY_CAPA(a))
    ary_expand_capa(mrb, a, len + 1);
  ARY_PTR(a)[len] = elem;
  ARY_SET_LEN(a, len+1);
  mrb_field_write_barrier_value(mrb, (struct RBasic*)a, elem);
}

static mrb_value
mrb_ary_push_m(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb);
  if (argc == 1) {
    mrb_ary_push(mrb, self, mrb_get_argv(mrb)[0]);
    return self;
  }
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int len = ARY_LEN(a);
  mrb_int len2 = len + argc;
  ary_modify(mrb, a);
  if (ARY_CAPA(a) < len2) {
    ary_expand_capa(mrb, a, len2);
  }
  const mrb_value *argv = mrb_get_argv(mrb);
  array_copy(ARY_PTR(a)+len, argv, argc);
  ARY_SET_LEN(a, len2);
  while (argc--) {
    mrb_field_write_barrier_value(mrb, (struct RBasic*)a, *argv);
    argv++;
  }
  return self;
}

/**
 * Removes and returns the last element from an array.
 *
 * If the array is empty, returns `nil`.
 * This function modifies the array in place.
 *
 * @param mrb The mruby state.
 * @param ary The array (mrb_value) from which to pop the element.
 * @return The last element of the array, or `nil` if the array is empty.
 */
MRB_API mrb_value
mrb_ary_pop(mrb_state *mrb, mrb_value ary)
{
  struct RArray *a = mrb_ary_ptr(ary);
  mrb_int len = ARY_LEN(a);

  ary_modify_check(mrb, a);
  if (len == 0) return mrb_nil_value();
  ARY_SET_LEN(a, len-1);
  return ARY_PTR(a)[len-1];
}

#define ARY_SHIFT_SHARED_MIN 10

/**
 * Removes and returns the first element from an array.
 *
 * If the array is empty, returns `nil`.
 * All other elements are shifted down by one index.
 * This function modifies the array in place.
 *
 * @param mrb The mruby state.
 * @param self The array (mrb_value) from which to shift the element.
 * @return The first element of the array, or `nil` if the array is empty.
 */
MRB_API mrb_value
mrb_ary_shift(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int len = ARY_LEN(a);

  ary_modify_check(mrb, a);
  if (len == 0) return mrb_nil_value();
  if (ARY_SHARED_P(a)) {
  L_SHIFT:
    a->as.heap.ptr++;
    a->as.heap.len--;
    return a->as.heap.ptr[-1];
  }
  else if (len > ARY_SHIFT_SHARED_MIN) {
    ary_make_shared(mrb, a);
    goto L_SHIFT;
  }
  else {
    mrb_value *ptr = ARY_PTR(a);
    mrb_int size = len;
    mrb_value val = *ptr;

    while (--size) {
      *ptr = *(ptr+1);
      ptr++;
    }
    ARY_SET_LEN(a, len-1);
    return val;
  }
}

static mrb_value
mrb_ary_shift_m(mrb_state *mrb, mrb_value self)
{

  if (mrb_get_argc(mrb) == 0) {
    return mrb_ary_shift(mrb, self);
  }

  mrb_int n = mrb_as_int(mrb,  mrb_get_arg1(mrb));
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int len = ARY_LEN(a);

  ary_modify_check(mrb, a);
  if (len == 0 || n == 0) return mrb_ary_new(mrb);
  if (n < 0) mrb_raise(mrb, E_ARGUMENT_ERROR, "negative array shift");
  if (n > len) n = len;
  mrb_value val = mrb_ary_new_from_values(mrb, n, ARY_PTR(a));
  if (ARY_SHARED_P(a)) {
  L_SHIFT:
    a->as.heap.ptr+=n;
    a->as.heap.len-=n;
    return val;
  }
  if (len > ARY_SHIFT_SHARED_MIN) {
    ary_make_shared(mrb, a);
    goto L_SHIFT;
  }
  else if (len == n) {
    ARY_SET_LEN(a, 0);
  }
  else {
    mrb_value *ptr = ARY_PTR(a);
    mrb_int size = len-n;

    while (size--) {
      *ptr = *(ptr+n);
      ptr++;
    }
    ARY_SET_LEN(a, len-n);
  }
  return val;
}

/* self = [1,2,3]
   item = 0
   self.unshift item
   p self #=> [0, 1, 2, 3] */
/**
 * Prepends an element to the beginning of an array.
 *
 * This function adds `item` to the front of the `self` array,
 * shifting all existing elements up by one index.
 * The array capacity may be expanded if necessary.
 * This function modifies the array in place.
 *
 * @param mrb The mruby state.
 * @param self The array (mrb_value) to unshift the element onto.
 * @param item The mrb_value to prepend to the array.
 * @return The modified array (the same mrb_value as `self`).
 */
MRB_API mrb_value
mrb_ary_unshift(mrb_state *mrb, mrb_value self, mrb_value item)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int len = ARY_LEN(a);

  if (ARY_SHARED_P(a)
      && a->as.heap.aux.shared->refcnt == 1 /* shared only referenced from this array */
      && a->as.heap.ptr - a->as.heap.aux.shared->ptr >= 1) /* there's room for unshifted item */ {
    a->as.heap.ptr--;
    a->as.heap.ptr[0] = item;
  }
  else {
    mrb_value *ptr;

    ary_modify(mrb, a);
    if (ARY_CAPA(a) < len + 1)
      ary_expand_capa(mrb, a, len + 1);
    ptr = ARY_PTR(a);
    value_move(ptr + 1, ptr, len);
    ptr[0] = item;
  }
  ARY_SET_LEN(a, len+1);
  mrb_field_write_barrier_value(mrb, (struct RBasic*)a, item);

  return self;
}

/*
 *  call-seq:
 *    array.unshift(*objects) -> self
 *
 *  Prepends the given +objects+ to +self+:
 *
 *    a = [:foo, 'bar', 2]
 *    a.unshift(:bam, :bat) # => [:bam, :bat, :foo, "bar", 2]
 *
 *  Array#prepend is an alias for Array#unshift.
 *
 *  Related: #push, #pop, #shift.
 */

static mrb_value
mrb_ary_unshift_m(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_value *ptr;

  mrb_int alen = mrb_get_argc(mrb);

  if (alen == 0) {
    ary_modify_check(mrb, a);
    return self;
  }
  const mrb_value *vals = mrb_get_argv(mrb);
  mrb_int len = ARY_LEN(a);
  if (alen > ARY_MAX_SIZE - len) {
    ary_too_big(mrb);
  }
  if (ARY_SHARED_P(a)
      && a->as.heap.aux.shared->refcnt == 1 /* shared only referenced from this array */
      && a->as.heap.ptr - a->as.heap.aux.shared->ptr >= alen) /* there's room for unshifted item */ {
    ary_modify_check(mrb, a);
    a->as.heap.ptr -= alen;
    ptr = a->as.heap.ptr;
  }
  else {
    mrb_bool same = vals == ARY_PTR(a);
    ary_modify(mrb, a);
    if (ARY_CAPA(a) < len + alen)
      ary_expand_capa(mrb, a, len + alen);
    ptr = ARY_PTR(a);
    value_move(ptr + alen, ptr, len);
    if (same) vals = ptr;
  }
  array_copy(ptr, vals, alen);
  ARY_SET_LEN(a, len+alen);
  while (alen--) {
    mrb_field_write_barrier_value(mrb, (struct RBasic*)a, vals[alen]);
  }

  return self;
}

/**
 * Sets the element at a given index in an array.
 *
 * If `n` is within the current bounds of the array, the element at that index
 * is replaced with `val`.
 * If `n` is beyond the current bounds, the array is expanded to accommodate
 * the new element, and any intermediate elements are filled with `nil`.
 * If `n` is negative, it counts from the end of the array.
 * An IndexError is raised if a negative index points past the beginning of the array.
 * This function modifies the array in place.
 *
 * @param mrb The mruby state.
 * @param ary The array (mrb_value) to modify.
 * @param n The index at which to set the element.
 * @param val The mrb_value to set at the specified index.
 */
MRB_API void
mrb_ary_set(mrb_state *mrb, mrb_value ary, mrb_int n, mrb_value val)
{
  struct RArray *a = mrb_ary_ptr(ary);
  mrb_int len = ARY_LEN(a);

  ary_modify(mrb, a);
  /* range check */
  if (n < 0) {
    n += len;
    if (n < 0) {
      mrb_raisef(mrb, E_INDEX_ERROR, "index %i out of array", n - len);
    }
  }
  if (n >= ARY_MAX_SIZE) {
    mrb_raise(mrb, E_INDEX_ERROR, "index too big");
  }
  if (len <= n) {
    if (ARY_CAPA(a) <= n)
      ary_expand_capa(mrb, a, n + 1);
    ary_fill_with_nil(ARY_PTR(a) + len, n + 1 - len);
    ARY_SET_LEN(a, n+1);
  }

  ARY_PTR(a)[n] = val;
  mrb_field_write_barrier_value(mrb, (struct RBasic*)a, val);
}

static struct RArray*
ary_dup(mrb_state *mrb, struct RArray *a)
{
  return ary_new_from_values(mrb, ARY_LEN(a), ARY_PTR(a));
}

MRB_API mrb_value
mrb_ary_dup(mrb_state *mrb, mrb_value ary)
{
  return mrb_obj_value(ary_dup(mrb, mrb_ary_ptr(ary)));
}

/**
 * Replaces a portion of an array with elements from another array or a single value.
 *
 * Removes `len` elements from `ary` starting at `head` index, and inserts
 * the elements from `rpl` (if `rpl` is an array) or `rpl` itself (if it's not an array)
 * at that position.
 * If `head` is negative, it counts from the end of the array.
 * If `len` is negative, an IndexError is raised.
 * If `rpl` is `mrb_undef_p()`, then the elements are removed without replacement.
 * This function modifies the `ary` array in place.
 *
 * @param mrb The mruby state.
 * @param ary The array (mrb_value) to modify.
 * @param head The starting index for the splice operation.
 * @param len The number of elements to remove.
 * @param rpl The mrb_value to insert (can be an array or a single value, or mrb_undef_p()).
 * @return The modified array (the same mrb_value as `ary`).
 */
MRB_API mrb_value
mrb_ary_splice(mrb_state *mrb, mrb_value ary, mrb_int head, mrb_int len, mrb_value rpl)
{
  struct RArray *a = mrb_ary_ptr(ary);
  mrb_int alen = ARY_LEN(a);
  const mrb_value *argv;
  mrb_int argc;
  mrb_int tail;

  ary_modify(mrb, a);

  /* len check */
  if (len < 0) mrb_raisef(mrb, E_INDEX_ERROR, "negative length (%i)", len);

  /* range check */
  if (head < 0) {
    head += alen;
    if (head < 0) goto out_of_range;
  }
  if (head > ARY_MAX_SIZE - len) {
  out_of_range:
    mrb_raisef(mrb, E_INDEX_ERROR, "index %i is out of array", head);
  }
  tail = head + len;
  if (alen < len || alen < tail) {
    len = alen - head;
    tail = head + len;
  }

  /* size check */
  if (mrb_array_p(rpl)) {
    argc = RARRAY_LEN(rpl);
    argv = RARRAY_PTR(rpl);
    if (argv == ARY_PTR(a)) {
      struct RArray *r;

      if (argc > 32767) {
        mrb_raise(mrb, E_ARGUMENT_ERROR, "too big recursive splice");
      }
      r = ary_dup(mrb, a);
      argv = ARY_PTR(r);
    }
  }
  else if (mrb_undef_p(rpl)) {
    argc = 0;
    argv = NULL;
  }
  else {
    argc = 1;
    argv = &rpl;
  }
  if (head >= alen) {
    if (head > ARY_MAX_SIZE - argc) goto out_of_range;
    len = head + argc;
    if (len > ARY_CAPA(a)) {
      ary_expand_capa(mrb, a, len);
    }
    ary_fill_with_nil(ARY_PTR(a) + alen, head - alen);
    if (argc > 0) {
      array_copy(ARY_PTR(a) + head, argv, argc);
    }
    ARY_SET_LEN(a, len);
  }
  else {
    if (alen - len > ARY_MAX_SIZE - argc) {
      head = alen + argc - len;
      goto out_of_range;
    }
    mrb_int newlen = alen + argc - len;
    if (newlen > ARY_CAPA(a)) {
      ary_expand_capa(mrb, a, newlen);
    }

    if (len != argc) {
      mrb_value *ptr = ARY_PTR(a);
      value_move(ptr + head + argc, ptr + tail, alen - tail);
      ARY_SET_LEN(a, newlen);
    }
    if (argc > 0) {
      value_move(ARY_PTR(a) + head, argv, argc);
    }
  }
  mrb_write_barrier(mrb, (struct RBasic*)a);
  return ary;
}

void
mrb_ary_decref(mrb_state *mrb, mrb_shared_array *shared)
{
  shared->refcnt--;
  if (shared->refcnt == 0) {
    mrb_free(mrb, shared->ptr);
    mrb_free(mrb, shared);
  }
}

static mrb_value
ary_subseq(mrb_state *mrb, struct RArray *a, mrb_int beg, mrb_int len)
{
  struct RArray *b;

  if (!ARY_SHARED_P(a) && len <= ARY_SHIFT_SHARED_MIN) {
    return mrb_ary_new_from_values(mrb, len, ARY_PTR(a)+beg);
  }
  ary_make_shared(mrb, a);
  b = MRB_OBJ_ALLOC(mrb, MRB_TT_ARRAY, mrb->array_class);
  b->as.heap.ptr = a->as.heap.ptr + beg;
  b->as.heap.len = len;
  b->as.heap.aux.shared = a->as.heap.aux.shared;
  b->as.heap.aux.shared->refcnt++;
  ARY_SET_SHARED_FLAG(b);

  return mrb_obj_value(b);
}

/**
 * Creates a new array that is a subsequence of an existing array.
 *
 * The new array contains `len` elements, starting from index `beg` of the
 * original `ary`.
 * This function attempts to create a shared array if appropriate for efficiency.
 *
 * @param mrb The mruby state.
 * @param ary The original array (mrb_value).
 * @param beg The starting index of the subsequence.
 * @param len The length of the subsequence.
 * @return A new mrb_value representing the subsequence array.
 */
mrb_value
mrb_ary_subseq(mrb_state *mrb, mrb_value ary, mrb_int beg, mrb_int len)
{
  struct RArray *a = mrb_ary_ptr(ary);
  return ary_subseq(mrb, a, beg, len);
}

static mrb_int
aget_index(mrb_state *mrb, mrb_value index)
{
  if (mrb_integer_p(index)) {
    return mrb_integer(index);
  }
#ifndef MRB_NO_FLOAT
  else if (mrb_float_p(index)) {
    return (mrb_int)mrb_float(index);
  }
#endif
  else {
    mrb_int i, argc;
    const mrb_value *argv;

    mrb_get_args(mrb, "i*!", &i, &argv, &argc);
    return i;
  }
}

/*
 *  call-seq:
 *     ary[index]                -> obj     or nil
 *     ary[start, length]        -> new_ary or nil
 *     ary[range]                -> new_ary or nil
 *     ary.slice(index)          -> obj     or nil
 *     ary.slice(start, length)  -> new_ary or nil
 *     ary.slice(range)          -> new_ary or nil
 *
 *  Element Reference --- Returns the element at +index+, or returns a
 *  subarray starting at the +start+ index and continuing for +length+
 *  elements, or returns a subarray specified by +range+ of indices.
 *
 *  Negative indices count backward from the end of the array (-1 is the last
 *  element).  For +start+ and +range+ cases the starting index is just before
 *  an element.  Additionally, an empty array is returned when the starting
 *  index for an element range is at the end of the array.
 *
 *  Returns +nil+ if the index (or starting index) are out of range.
 *
 *  a = [ "a", "b", "c", "d", "e" ]
 *  a[1]     => "b"
 *  a[1,2]   => ["b", "c"]
 *  a[1..-2] => ["b", "c", "d"]
 *
 */

static mrb_value
mrb_ary_aget(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int i, len;
  mrb_value index;

  if (mrb_get_argc(mrb) == 1) {
    index = mrb_get_arg1(mrb);
    switch (mrb_type(index)) {
      /* a[n..m] */
    case MRB_TT_RANGE:
      if (mrb_range_beg_len(mrb, index, &i, &len, ARY_LEN(a), TRUE) == MRB_RANGE_OK) {
        return ary_subseq(mrb, a, i, len);
      }
      else {
        return mrb_nil_value();
      }
    case MRB_TT_INTEGER:
      return mrb_ary_ref(mrb, self, mrb_integer(index));
    default:
      return mrb_ary_ref(mrb, self, aget_index(mrb, index));
    }
  }

  mrb_get_args(mrb, "oi", &index, &len);
  i = aget_index(mrb, index);
  mrb_int alen = ARY_LEN(a);
  if (i < 0) i += alen;
  if (i < 0 || alen < i) return mrb_nil_value();
  if (len < 0) return mrb_nil_value();
  if (alen == i) return mrb_ary_new(mrb);
  if (len > alen - i) len = alen - i;

  return ary_subseq(mrb, a, i, len);
}

/*
 *  call-seq:
 *     ary[index]         = obj                      ->  obj
 *     ary[start, length] = obj or other_ary or nil  ->  obj or other_ary or nil
 *     ary[range]         = obj or other_ary or nil  ->  obj or other_ary or nil
 *
 *  Element Assignment --- Sets the element at +index+, or replaces a subarray
 *  from the +start+ index for +length+ elements, or replaces a subarray
 *  specified by the +range+ of indices.
 *
 *  If indices are greater than the current capacity of the array, the array
 *  grows automatically.  Elements are inserted into the array at +start+ if
 *  +length+ is zero.
 *
 *  Negative indices will count backward from the end of the array.  For
 *  +start+ and +range+ cases the starting index is just before an element.
 *
 *  An IndexError is raised if a negative index points past the beginning of
 *  the array.
 *
 *  See also Array#push, and Array#unshift.
 *
 *     a = Array.new
 *     a[4] = "4";                 #=> [nil, nil, nil, nil, "4"]
 *     a[0, 3] = [ 'a', 'b', 'c' ] #=> ["a", "b", "c", nil, "4"]
 *     a[1..2] = [ 1, 2 ]          #=> ["a", 1, 2, nil, "4"]
 *     a[0, 2] = "?"               #=> ["?", 2, nil, "4"]
 *     a[0..2] = "A"               #=> ["A", "4"]
 *     a[-1]   = "Z"               #=> ["A", "Z"]
 *     a[1..-1] = nil              #=> ["A", nil]
 *     a[1..-1] = []               #=> ["A"]
 *     a[0, 0] = [ 1, 2 ]          #=> [1, 2, "A"]
 *     a[3, 0] = "B"               #=> [1, 2, "A", "B"]
 */

static mrb_value
mrb_ary_aset(mrb_state *mrb, mrb_value self)
{
  mrb_value v1, v2, v3;

  if (mrb_get_argc(mrb) == 2) {
    mrb_int i, len;
    const mrb_value *vs = mrb_get_argv(mrb);
    v1 = vs[0]; v2 = vs[1];

    /* a[n..m] = v */
    switch (mrb_range_beg_len(mrb, v1, &i, &len, RARRAY_LEN(self), FALSE)) {
    case MRB_RANGE_TYPE_MISMATCH:
      mrb_ary_set(mrb, self, aget_index(mrb, v1), v2);
      break;
    case MRB_RANGE_OK:
      mrb_ary_splice(mrb, self, i, len, v2);
      break;
    case MRB_RANGE_OUT:
      mrb_raisef(mrb, E_RANGE_ERROR, "%v out of range", v1);
      break;
    }
    return v2;
  }

  mrb_get_args(mrb, "ooo", &v1, &v2, &v3);
  /* a[n,m] = v */
  mrb_ary_splice(mrb, self, aget_index(mrb, v1), aget_index(mrb, v2), v3);
  return v3;
}

mrb_value
mrb_ary_delete_at(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);

  mrb_int index = mrb_as_int(mrb, mrb_get_arg1(mrb));
  mrb_int alen = ARY_LEN(a);
  if (index < 0) index += alen;
  if (index < 0 || alen <= index) return mrb_nil_value();

  ary_modify(mrb, a);
  mrb_value *ptr = ARY_PTR(a);
  mrb_value val = ptr[index];

  ptr += index;
  mrb_int len = alen - index;
  while (--len) {
    *ptr = *(ptr+1);
    ptr++;
  }
  ARY_SET_LEN(a, alen-1);

  ary_shrink_capa(mrb, a);

  return val;
}

static mrb_value
mrb_ary_first(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int size;

  if (mrb_get_argc(mrb) == 0) {
    if (ARY_LEN(a) > 0) return ARY_PTR(a)[0];
    return mrb_nil_value();
  }
  mrb_get_args(mrb, "|i", &size);
  if (size < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative array size");
  }

  mrb_int alen = ARY_LEN(a);
  if (size > alen) size = alen;
  if (ARY_SHARED_P(a)) {
    return ary_subseq(mrb, a, 0, size);
  }
  return mrb_ary_new_from_values(mrb, size, ARY_PTR(a));
}

static mrb_value
mrb_ary_last(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int alen = ARY_LEN(a);

  if (mrb_get_argc(mrb) == 0) {
    if (alen > 0) return ARY_PTR(a)[alen - 1];
    return mrb_nil_value();
  }

  mrb_int size = mrb_integer(mrb_get_arg1(mrb));
  if (size < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative array size");
  }
  if (size > alen) size = alen;
  if (ARY_SHARED_P(a) || size > ARY_DEFAULT_LEN) {
    return ary_subseq(mrb, a, alen - size, size);
  }
  return mrb_ary_new_from_values(mrb, size, ARY_PTR(a) + alen - size);
}

/*
 *  call-seq:
 *     ary.index(val)            -> int or nil
 *     ary.index {|item| block } -> int or nil
 *     array.index -> enumerator
 *
 *  Returns the _index_ of the first object in +ary+ such that the object is
 *  <code>==</code> to +obj+.
 *
 *  If a block is given instead of an argument, returns the _index_ of the
 *  first object for which the block returns +true+. Returns +nil+ if no
 *  match is found.
 *
 * ISO 15.2.12.5.14
 */
static mrb_value
mrb_ary_index_m(mrb_state *mrb, mrb_value self)
{
  mrb_value obj, blk;

  if (mrb_get_args(mrb, "|o&", &obj, &blk) == 0 && mrb_nil_p(blk)) {
    return mrb_funcall_id(mrb, self, MRB_SYM(to_enum), 1, mrb_symbol_value(MRB_SYM(index)));
  }

  if (mrb_nil_p(blk)) {
    for (mrb_int i = 0; i < RARRAY_LEN(self); i++) {
      if (mrb_equal(mrb, RARRAY_PTR(self)[i], obj)) {
        return mrb_int_value(mrb, i);
      }
    }
  }
  else {
    for (mrb_int i = 0; i < RARRAY_LEN(self); i++) {
      mrb_value eq = mrb_yield(mrb, blk, RARRAY_PTR(self)[i]);
      if (mrb_test(eq)) {
        return mrb_int_value(mrb, i);
      }
    }
  }
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     ary.rindex(val)            -> int or nil
 *     ary.rindex {|item| block } -> int or nil
 *     array.rindex -> enumerator
 *
 *  Returns the _index_ of the first object in +ary+ such that the object is
 *  <code>==</code> to +obj+.
 *
 *  If a block is given instead of an argument, returns the _index_ of the
 *  first object for which the block returns +true+. Returns +nil+ if no
 *  match is found.
 *
 * ISO 15.2.12.5.26
 */
static mrb_value
mrb_ary_rindex_m(mrb_state *mrb, mrb_value self)
{
  mrb_value obj, blk;

  if (mrb_get_args(mrb, "|o&", &obj, &blk) == 0 && mrb_nil_p(blk)) {
    return mrb_funcall_id(mrb, self, MRB_SYM(to_enum), 1, mrb_symbol_value(MRB_SYM(rindex)));
  }

  for (mrb_int i = RARRAY_LEN(self) - 1; i >= 0; i--) {
    if (mrb_nil_p(blk)) {
      if (mrb_equal(mrb, RARRAY_PTR(self)[i], obj)) {
      return mrb_int_value(mrb, i);
      }
    }
    else {
      mrb_value eq = mrb_yield(mrb, blk, RARRAY_PTR(self)[i]);
      if (mrb_test(eq)) return mrb_int_value(mrb, i);
    }
    mrb_int len = RARRAY_LEN(self);
    if (i > len) {
      i = len;
    }
  }
  return mrb_nil_value();
}

/**
 * Creates a new array from a given value, performing a "splat" operation.
 *
 * If `v` is already an array, a duplicate of `v` is returned.
 * If `v` responds to `to_a`, it is called, and if the result is an array,
 * a duplicate of that result is returned. If `to_a` returns `nil` or something
 * other than an array, `v` itself is wrapped in a new, single-element array.
 * Otherwise (if `v` is not an array and does not respond to `to_a`),
 * `v` itself is wrapped in a new, single-element array.
 *
 * @param mrb The mruby state.
 * @param v The mrb_value to convert into an array.
 * @return A new mrb_value representing the "splatted" array.
 */
MRB_API mrb_value
mrb_ary_splat(mrb_state *mrb, mrb_value v)
{
  struct RArray *a;

  if (mrb_array_p(v)) {
    a = ary_dup(mrb, mrb_ary_ptr(v));
    return mrb_obj_value(a);
  }

  if (!mrb_respond_to(mrb, v, MRB_SYM(to_a))) {
    return mrb_ary_new_from_values(mrb, 1, &v);
  }

  mrb_value ary = mrb_funcall_argv(mrb, v, MRB_SYM(to_a), 0, NULL);
  if (mrb_nil_p(ary)) {
    return mrb_ary_new_from_values(mrb, 1, &v);
  }
  mrb_ensure_array_type(mrb, ary);
  a = mrb_ary_ptr(ary);
  a = ary_dup(mrb, a);
  return mrb_obj_value(a);
}

static mrb_value
mrb_ary_size(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);

  return mrb_int_value(mrb, ARY_LEN(a));
}

/**
 * Removes all elements from an array, making it empty.
 *
 * This function modifies the array in place.
 *
 * @param mrb The mruby state.
 * @param self The array (mrb_value) to clear.
 * @return The cleared (now empty) array (the same mrb_value as `self`).
 */
MRB_API mrb_value
mrb_ary_clear(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);

  ary_modify(mrb, a);
  if (ARY_SHARED_P(a)) {
    mrb_ary_decref(mrb, a->as.heap.aux.shared);
    ARY_UNSET_SHARED_FLAG(a);
  }
  else if (!ARY_EMBED_P(a)){
    mrb_free(mrb, a->as.heap.ptr);
  }
  if (MRB_ARY_EMBED_LEN_MAX > 0) {
    ARY_SET_EMBED_LEN(a, 0);
  }
  else {
    a->as.heap.ptr = NULL;
    a->as.heap.aux.capa = 0;
    ARY_SET_LEN(a, 0);
  }
  return self;
}

static mrb_value
mrb_ary_empty_p(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);

  return mrb_bool_value(ARY_LEN(a) == 0);
}

/**
 * Retrieves an element from an array at a specific index.
 * This is a direct (unsafe) equivalent of `RARRAY_PTR(ary)[n]`.
 *
 * If `n` is negative, it counts from the end of the array.
 * Returns `nil` if the index is out of bounds.
 * This function does not perform a bounds check before accessing the element if the index is positive.
 * Prefer using `mrb_ary_ref` for safe access or ensure `n` is within bounds.
 *
 * @param ary The array (mrb_value) from which to retrieve the element.
 * @param n The index of the element to retrieve.
 * @return The mrb_value at the specified index, or `nil` if out of bounds.
 */
MRB_API mrb_value
mrb_ary_entry(mrb_value ary, mrb_int n)
{
  struct RArray *a = mrb_ary_ptr(ary);
  mrb_int len = ARY_LEN(a);

  /* range check */
  if (n < 0) n += len;
  if (n < 0 || len <= n) return mrb_nil_value();

  return ARY_PTR(a)[n];
}

static mrb_value
join_ary(mrb_state *mrb, mrb_value ary, mrb_value sep, mrb_value list)
{
  /* check recursive */
  for (mrb_int i=0; i<RARRAY_LEN(list); i++) {
    if (mrb_obj_equal(mrb, ary, RARRAY_PTR(list)[i])) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "recursive array join");
    }
  }

  mrb_ary_push(mrb, list, ary);

  mrb_value result = mrb_str_new_capa(mrb, 64);

  for (mrb_int i=0; i<RARRAY_LEN(ary); i++) {
    if (i > 0 && !mrb_nil_p(sep)) {
      mrb_str_cat_str(mrb, result, sep);
    }

    mrb_value val = RARRAY_PTR(ary)[i];

    switch (mrb_type(val)) {
    case MRB_TT_ARRAY:
    ary_join:
      val = join_ary(mrb, val, sep, list);
      /* fall through */

    case MRB_TT_STRING:
    str_join:
      mrb_str_cat_str(mrb, result, val);
      break;

    default:
      if (!mrb_immediate_p(val)) {
        mrb_value tmp = mrb_check_string_type(mrb, val);
        if (!mrb_nil_p(tmp)) {
          val = tmp;
          goto str_join;
        }
        tmp = mrb_check_array_type(mrb, val);
        if (!mrb_nil_p(tmp)) {
          val = tmp;
          goto ary_join;
        }
      }
      val = mrb_obj_as_string(mrb, val);
      goto str_join;
    }
  }

  mrb_ary_pop(mrb, list);

  return result;
}

/**
 * Joins the elements of an array into a string, separated by a given separator.
 *
 * Each element of `ary` is converted to a string. These strings are then
 * concatenated, with the string representation of `sep` inserted between
 * adjacent elements.
 * If `sep` is `nil`, no separator is used.
 * This function handles recursive array joins by raising an E_ARGUMENT_ERROR.
 *
 * @param mrb The mruby state.
 * @param ary The array (mrb_value) whose elements are to be joined.
 * @param sep The separator (mrb_value) to use between elements. Can be `nil`.
 * @return A new mrb_value string representing the joined array elements.
 */
MRB_API mrb_value
mrb_ary_join(mrb_state *mrb, mrb_value ary, mrb_value sep)
{
  if (!mrb_nil_p(sep)) {
    sep = mrb_obj_as_string(mrb, sep);
  }
  return join_ary(mrb, ary, sep, mrb_ary_new(mrb));
}

/*
 *  call-seq:
 *     ary.join(sep="")    -> str
 *
 *  Returns a string created by converting each element of the array to
 *  a string, separated by <i>sep</i>.
 *
 *     [ "a", "b", "c" ].join        #=> "abc"
 *     [ "a", "b", "c" ].join("-")   #=> "a-b-c"
 */

static mrb_value
mrb_ary_join_m(mrb_state *mrb, mrb_value ary)
{
  mrb_value sep = mrb_nil_value();

  mrb_get_args(mrb, "|S!", &sep);
  return mrb_ary_join(mrb, ary, sep);
}

/*
 * call-seq:
 *    ary.to_s    -> string
 *    ary.inspect -> string
 *
 * Return the contents of this array as a string.
 */
static mrb_value
mrb_ary_to_s(mrb_state *mrb, mrb_value self)
{
  mrb->c->ci->mid = MRB_SYM(inspect);
  mrb_value ret = mrb_str_new_lit(mrb, "[");
  int ai = mrb_gc_arena_save(mrb);
  if (MRB_RECURSIVE_UNARY_P(mrb, MRB_SYM(inspect), self)) {
    mrb_str_cat_lit(mrb, ret, "...]");
    return ret;
  }
  for (mrb_int i=0; i<RARRAY_LEN(self); i++) {
    if (i>0) mrb_str_cat_lit(mrb, ret, ", ");
    mrb_str_cat_str(mrb, ret, mrb_inspect(mrb, RARRAY_PTR(self)[i]));
    mrb_gc_arena_restore(mrb, ai);
  }
  mrb_str_cat_lit(mrb, ret, "]");

  return ret;
}

/* check array equality: 1=equal,0=not_equal,-1=need_elements_check */
static mrb_int
ary_eq(mrb_state *mrb, mrb_value ary1, mrb_value ary2)
{
  if (mrb_obj_equal(mrb, ary1, ary2)) return 1;
  if (!mrb_array_p(ary2)) return 0;
  if (RARRAY_LEN(ary1) != RARRAY_LEN(ary2)) return 0;

  return -1;
}

/*
 * call-seq:
 *   array == other   -> true or false
 *
 *  Equality---Two arrays are equal if they contain the same number
 *  of elements and if each element is equal to (according to
 *  Object.==) the corresponding element in the other array.
 *
 */
static mrb_value
mrb_ary_eq(mrb_state *mrb, mrb_value ary1)
{
  mrb_value ary2 = mrb_get_arg1(mrb);
  mrb_int n = ary_eq(mrb, ary1, ary2);

  if (n == 1) return mrb_true_value();
  if (n == 0) return mrb_false_value();

  /* Check for recursion */
  if (MRB_RECURSIVE_BINARY_P(mrb, MRB_OPSYM(eq), ary1, ary2)) {
    return mrb_false_value();
  }

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i=0; i<RARRAY_LEN(ary1); i++) {
    mrb_value eq = mrb_funcall_id(mrb, mrb_ary_entry(ary1, i), MRB_OPSYM(eq), 1, mrb_ary_entry(ary2, i));
    if (!mrb_test(eq)) return mrb_false_value();
    mrb_gc_arena_restore(mrb, ai);
  }
  return mrb_true_value();
}

/*
 * call-seq:
 *   array.eql? other_array -> true or false
 *
 *  Returns <code>true</code> if +self+ and _other_ are the same object,
 *  or are both arrays with the same content.
 *
 */
static mrb_value
mrb_ary_eql(mrb_state *mrb, mrb_value ary1)
{
  mrb_value ary2 = mrb_get_arg1(mrb);
  mrb_int n = ary_eq(mrb, ary1, ary2);

  if (n == 1) return mrb_true_value();
  if (n == 0) return mrb_false_value();

  /* Check for recursion */
  if (MRB_RECURSIVE_BINARY_P(mrb, MRB_SYM_Q(eql), ary1, ary2)) {
    return mrb_false_value();
  }

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i=0; i<RARRAY_LEN(ary1); i++) {
    mrb_value eq = mrb_funcall_id(mrb, mrb_ary_entry(ary1, i), MRB_SYM_Q(eql), 1, mrb_ary_entry(ary2, i));
    if (!mrb_test(eq)) return mrb_false_value();
    mrb_gc_arena_restore(mrb, ai);
  }
  return mrb_true_value();
}

/*
 * call-seq:
 *   array <=> other_array -> -1, 0, or 1
 *
 *  Comparison---Returns an integer (-1, 0, or +1)
 *  if this array is less than, equal to, or greater than <i>other_ary</i>.
 *  Each object in each array is compared (using <=>). If any value isn't
 *  equal, then that inequality is the return value. If all the
 *  values found are equal, then the return is based on a
 *  comparison of the array lengths. Thus, two arrays are
 *  "equal" according to <code>Array*<=></code> if and only if they have
 *  the same length and the value of each element is equal to the
 *  value of the corresponding element in the other array.
 */
static mrb_value
mrb_ary_cmp(mrb_state *mrb, mrb_value ary1)
{
  mrb_value ary2 = mrb_get_arg1(mrb);

  if (mrb_obj_equal(mrb, ary1, ary2)) return mrb_fixnum_value(0);
  if (!mrb_array_p(ary2)) return mrb_nil_value();

  for (mrb_int i=0; i<RARRAY_LEN(ary1) && i<RARRAY_LEN(ary2); i++) {
    mrb_int n = mrb_cmp(mrb, RARRAY_PTR(ary1)[i], RARRAY_PTR(ary2)[i]);
    if (n == -2) return mrb_nil_value();
    if (n != 0) return mrb_fixnum_value(n);
  }
  mrb_int len = RARRAY_LEN(ary1) - RARRAY_LEN(ary2);
  if (len == 0) return mrb_fixnum_value(0);
  else if (len > 0) return mrb_fixnum_value(1);
  else return mrb_fixnum_value(-1);
}

/* internal method to convert multi-value to single value */
static mrb_value
mrb_ary_svalue(mrb_state *mrb, mrb_value ary)
{
  switch (RARRAY_LEN(ary)) {
  case 0:
    return mrb_nil_value();
  case 1:
    return RARRAY_PTR(ary)[0];
  default:
    return ary;
  }
}

/*
 * call-seq:
 *   array.delete(obj) -> deleted_object
 *   array.delete(obj) {|nosuch| ... } -> deleted_object or block_return
 *
 * Removes zero or more elements from self; returns self.
 *
 * When no block is given, removes from self each element e such
 * that e == obj; returns the last deleted element
 *
 * Returns nil if no elements removed.
 *
 * When a block is given, removes from self each element e such
 * that e == obj. If any such elements are found, ignores the block and
 * returns the last. Otherwise, returns the block's return value.
 */
static mrb_value
mrb_ary_delete(mrb_state *mrb, mrb_value self)
{
  mrb_value obj, blk;

  mrb_get_args(mrb, "o&", &obj, &blk);

  struct RArray *ary = RARRAY(self);
  mrb_value ret = obj;
  int ai = mrb_gc_arena_save(mrb);
  mrb_int i = 0;
  mrb_int j = 0;
  for (; i < ARY_LEN(ary); i++) {
    mrb_value elem = ARY_PTR(ary)[i];

    if (mrb_equal(mrb, elem, obj)) {
      mrb_gc_arena_restore(mrb, ai);
      mrb_gc_protect(mrb, elem);
      ret = elem;
      continue;
    }

    if (i != j) {
      if (j >= ARY_LEN(ary)) {
        // Since breaking here will further change the array length,
        // there is no choice but to raise an exception or return.
        mrb_raise(mrb, E_RUNTIME_ERROR, "array modified during delete");
      }
      ary_modify(mrb, ary);
      ARY_PTR(ary)[j] = elem;
    }

    j++;
  }

  if (i == j) {
    if (mrb_nil_p(blk)) return mrb_nil_value();
    return mrb_yield(mrb, blk, obj);
  }

  ARY_SET_LEN(ary, j);
  return ret;
}


#define SMALL_ARRAY_SORT_THRESHOLD 16


static mrb_bool
sort_cmp(mrb_state *mrb, mrb_value ary, mrb_value *p, mrb_value a_val, mrb_value b_val, mrb_value blk)
{
  mrb_int cmp;
  int ai = mrb_gc_arena_save(mrb);

  if (mrb_nil_p(blk)) {
    enum mrb_vtype type_a = mrb_type(a_val);
    enum mrb_vtype type_b = mrb_type(b_val);

    if (type_a == type_b) {
      switch (type_a) {
      case MRB_TT_FIXNUM:
        cmp = (mrb_fixnum(a_val) > mrb_fixnum(b_val)) ? 1 : (mrb_fixnum(a_val) < mrb_fixnum(b_val)) ? -1 : 0;
        break;
#ifndef MRB_NO_FLOAT
      case MRB_TT_FLOAT:
        cmp = (mrb_float(a_val) > mrb_float(b_val)) ? 1 : (mrb_float(a_val) < mrb_float(b_val)) ? -1 : 0;
        break;
#endif
      case MRB_TT_STRING:
        cmp = mrb_str_cmp(mrb, a_val, b_val);
        break;
      default:
        cmp = mrb_cmp(mrb, a_val, b_val);
        break;
      }
    }
    else {
      cmp = mrb_cmp(mrb, a_val, b_val);
    }
  }
  else {
    mrb_value args[2] = {a_val, b_val};
    mrb_value c = mrb_yield_argv(mrb, blk, 2, args);
    if (mrb_nil_p(c) || !mrb_fixnum_p(c)) {
      cmp = -2;
    }
    else {
      cmp = mrb_fixnum(c);
    }
  }
  mrb_gc_arena_restore(mrb, ai);
  if (cmp == -2) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "comparison failed");
  }
  if (RARRAY_PTR(ary) != p) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "array modified during sort");
  }
  return cmp > 0;
}

static void
heapify(mrb_state *mrb, mrb_value ary, mrb_value *a, mrb_int index, mrb_int size, mrb_value blk)
{
  /* Iterative heapify to avoid stack overflow on memory-constrained devices */
  while (1) {
    mrb_int max = index;
    mrb_int left_index = 2 * index + 1;
    mrb_int right_index = left_index + 1;

    if (left_index < size && sort_cmp(mrb, ary, a, a[left_index], a[max], blk)) {
      max = left_index;
    }
    if (right_index < size && sort_cmp(mrb, ary, a, a[right_index], a[max], blk)) {
      max = right_index;
    }

    if (max == index) {
      /* Heap property satisfied, no more swaps needed */
      break;
    }

    /* Swap elements and continue heapifying down the affected subtree */
    mrb_value tmp = a[max];
    a[max] = a[index];
    a[index] = tmp;

    /* Continue with the affected child subtree */
    index = max;
  }
}

static void
insertion_sort(mrb_state *mrb, mrb_value ary, mrb_value *a, mrb_int size, mrb_value blk)
{
  for (mrb_int i = 1; i < size; i++) {
    mrb_value key = a[i];
    mrb_int j = i - 1;

    /* Move elements that are greater than key to one position ahead */
    while (j >= 0 && sort_cmp(mrb, ary, a, a[j], key, blk)) {
      a[j + 1] = a[j];
      j--;
    }
    a[j + 1] = key;
  }
}

/*
 *  call-seq:
 *    array.sort! -> self
 *    array.sort! {|a, b| ... } -> self
 *
 *  Sort all elements and replace +self+ with these
 *  elements.
 */
static mrb_value
mrb_ary_sort_bang(mrb_state *mrb, mrb_value ary)
{
  mrb_value blk;

  mrb_int n = RARRAY_LEN(ary);
  if (n < 2) return ary;

  ary_modify(mrb, mrb_ary_ptr(ary));
  mrb_get_args(mrb, "&", &blk);

  mrb_value *a = RARRAY_PTR(ary);

  /* Algorithm selection based on array size */
  if (n <= SMALL_ARRAY_SORT_THRESHOLD) {
    /* Use insertion sort for small arrays */
    insertion_sort(mrb, ary, a, n, blk);
  }
  else {
    /* Use heap sort for larger arrays */
    for (mrb_int i = n / 2 - 1; i >= 0; i--) {
      heapify(mrb, ary, a, i, n, blk);
    }
    for (mrb_int i = n - 1; i > 0; i--) {
      mrb_value tmp = a[0];
      a[0] = a[i];
      a[i] = tmp;
      heapify(mrb, ary, a, 0, i, blk);
    }
  }
  return ary;
}

/*
 *  call-seq:
 *    array.to_a -> self
 *
 *  Returns self. If called on a subclass of Array, converts
 *  the receiver to an Array object.
 */
static mrb_value
mrb_ary_to_a(mrb_state *mrb, mrb_value self)
{
  if (mrb_obj_class(mrb, self) != mrb->array_class) {
    /* Convert subclass to Array */
    return mrb_ary_dup(mrb, self);
  }
  return self;
}

void
mrb_init_array(mrb_state *mrb)
{
  struct RClass *a;

  mrb->array_class = a = mrb_define_class_id(mrb, MRB_SYM(Array), mrb->object_class);              /* 15.2.12 */
  MRB_SET_INSTANCE_TT(a, MRB_TT_ARRAY);

  mrb_define_class_method_id(mrb, a, MRB_OPSYM(aref),    mrb_ary_s_create,     MRB_ARGS_ANY());    /* 15.2.12.4.1 */

  mrb_define_method_id(mrb, a, MRB_OPSYM(add),           mrb_ary_plus,         MRB_ARGS_REQ(1));   /* 15.2.12.5.1  */
  mrb_define_method_id(mrb, a, MRB_OPSYM(mul),           mrb_ary_times,        MRB_ARGS_REQ(1));   /* 15.2.12.5.2  */
  mrb_define_method_id(mrb, a, MRB_OPSYM(lshift),        mrb_ary_push_m,       MRB_ARGS_REQ(1));   /* 15.2.12.5.3  */
  mrb_define_method_id(mrb, a, MRB_OPSYM(aref),          mrb_ary_aget,         MRB_ARGS_ARG(1,1)); /* 15.2.12.5.4  */
  mrb_define_method_id(mrb, a, MRB_OPSYM(aset),          mrb_ary_aset,         MRB_ARGS_ARG(2,1)); /* 15.2.12.5.5  */
  mrb_define_method_id(mrb, a, MRB_SYM(clear),           mrb_ary_clear,        MRB_ARGS_NONE());   /* 15.2.12.5.6  */
  mrb_define_method_id(mrb, a, MRB_OPSYM(cmp),           mrb_ary_cmp,          MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(concat),          mrb_ary_concat_m,     MRB_ARGS_REQ(1));   /* 15.2.12.5.8  */
  mrb_define_method_id(mrb, a, MRB_SYM(delete),          mrb_ary_delete,       MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(delete_at),       mrb_ary_delete_at,    MRB_ARGS_REQ(1));   /* 15.2.12.5.9  */
  mrb_define_method_id(mrb, a, MRB_SYM_Q(empty),         mrb_ary_empty_p,      MRB_ARGS_NONE());   /* 15.2.12.5.12 */
  mrb_define_method_id(mrb, a, MRB_OPSYM(eq),            mrb_ary_eq,           MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM_Q(eql),           mrb_ary_eql,          MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(first),           mrb_ary_first,        MRB_ARGS_OPT(1));   /* 15.2.12.5.13 */
  mrb_define_method_id(mrb, a, MRB_SYM(index),           mrb_ary_index_m,      MRB_ARGS_REQ(1));   /* 15.2.12.5.14 */
  mrb_define_method_id(mrb, a, MRB_SYM(initialize),      mrb_ary_init,         MRB_ARGS_OPT(2));   /* 15.2.12.5.15 */
  mrb_define_private_method_id(mrb, a, MRB_SYM(initialize_copy), mrb_ary_replace_m, MRB_ARGS_REQ(1)); /* 15.2.12.5.16 */
  mrb_define_method_id(mrb, a, MRB_SYM(join),            mrb_ary_join_m,       MRB_ARGS_OPT(1));   /* 15.2.12.5.17 */
  mrb_define_method_id(mrb, a, MRB_SYM(last),            mrb_ary_last,         MRB_ARGS_OPT(1));   /* 15.2.12.5.18 */
  mrb_define_method_id(mrb, a, MRB_SYM(length),          mrb_ary_size,         MRB_ARGS_NONE());   /* 15.2.12.5.19 */
  mrb_define_method_id(mrb, a, MRB_SYM(pop),             mrb_ary_pop,          MRB_ARGS_NONE());   /* 15.2.12.5.21 */
  mrb_define_method_id(mrb, a, MRB_SYM(push),            mrb_ary_push_m,       MRB_ARGS_ANY());    /* 15.2.12.5.22 */
  mrb_define_method_id(mrb, a, MRB_SYM(replace),         mrb_ary_replace_m,    MRB_ARGS_REQ(1));   /* 15.2.12.5.23 */
  mrb_define_method_id(mrb, a, MRB_SYM(reverse),         mrb_ary_reverse,      MRB_ARGS_NONE());   /* 15.2.12.5.24 */
  mrb_define_method_id(mrb, a, MRB_SYM_B(reverse),       mrb_ary_reverse_bang, MRB_ARGS_NONE());   /* 15.2.12.5.25 */
  mrb_define_method_id(mrb, a, MRB_SYM(rindex),          mrb_ary_rindex_m,     MRB_ARGS_REQ(1));   /* 15.2.12.5.26 */
  mrb_define_method_id(mrb, a, MRB_SYM(shift),           mrb_ary_shift_m,      MRB_ARGS_OPT(1));   /* 15.2.12.5.27 */
  mrb_define_method_id(mrb, a, MRB_SYM(size),            mrb_ary_size,         MRB_ARGS_NONE());   /* 15.2.12.5.28 */
  mrb_define_method_id(mrb, a, MRB_SYM(slice),           mrb_ary_aget,         MRB_ARGS_ARG(1,1)); /* 15.2.12.5.29 */
  mrb_define_method_id(mrb, a, MRB_SYM(unshift),         mrb_ary_unshift_m,    MRB_ARGS_ANY());    /* 15.2.12.5.30 */
  mrb_define_method_id(mrb, a, MRB_SYM(to_a),            mrb_ary_to_a,         MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM(entries),         mrb_ary_to_a,         MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM(to_s),            mrb_ary_to_s,         MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM(inspect),         mrb_ary_to_s,         MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM_B(sort),          mrb_ary_sort_bang,    MRB_ARGS_NONE());

  mrb_define_method_id(mrb, a, MRB_SYM(__svalue),        mrb_ary_svalue,       MRB_ARGS_NONE());
}
