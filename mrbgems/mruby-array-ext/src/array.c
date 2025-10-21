#include <mruby.h>
#include <string.h>
#include <mruby/value.h>
#include <mruby/array.h>
#include <mruby/range.h>
#include <mruby/hash.h>
#include <mruby/data.h>
#include <mruby/internal.h>
#include <mruby/presym.h>
#include <mruby/khash.h>
#include <mruby/error.h>

/* khash set for temporary array operations */
static inline khint_t
ary_set_hash_func(mrb_state *mrb, mrb_value key)
{
  return (khint_t)mrb_obj_hash_code(mrb, key);
}

static inline mrb_bool
ary_set_equal_func(mrb_state *mrb, mrb_value a, mrb_value b)
{
  return mrb_eql(mrb, a, b);
}

KHASH_DECLARE(ary_set, mrb_value, char, 0)
KHASH_DEFINE(ary_set, mrb_value, char, 0, ary_set_hash_func, ary_set_equal_func)

typedef khash_t(ary_set) ary_set_t;

/* Combination state structure for repeated_combination optimization */
struct mrb_combination_state {
  mrb_int *indices;
  mrb_int n;
  mrb_int array_size;
  mrb_bool permutation;
  mrb_bool finished;
};

static void
mrb_combination_state_free(mrb_state *mrb, void *ptr)
{
  struct mrb_combination_state *state = (struct mrb_combination_state*)ptr;
  if (state) {
    if (state->indices) {
      mrb_free(mrb, state->indices);
    }
    mrb_free(mrb, state);
  }
}

static struct mrb_data_type mrb_combination_state_type = {
  "CombinationState", mrb_combination_state_free
};

/*
 *  call-seq:
 *     ary.assoc(obj)   -> new_ary  or  nil
 *
 *  Searches through an array whose elements are also arrays
 *  comparing _obj_ with the first element of each contained array
 *  using obj.==.
 *  Returns the first contained array that matches (that
 *  is, the first associated array),
 *  or `nil` if no match is found.
 *  See also `Array#rassoc`.
 *
 *     s1 = [ "colors", "red", "blue", "green" ]
 *     s2 = [ "letters", "a", "b", "c" ]
 *     s3 = "foo"
 *     a  = [ s1, s2, s3 ]
 *     a.assoc("letters")  #=> [ "letters", "a", "b", "c" ]
 *     a.assoc("foo")      #=> nil
 */

static mrb_value
ary_assoc(mrb_state *mrb, mrb_value ary)
{
  mrb_value k = mrb_get_arg1(mrb);

  for (mrb_int i = 0; i < RARRAY_LEN(ary); i++) {
    mrb_value v = mrb_check_array_type(mrb, RARRAY_PTR(ary)[i]);
    if (!mrb_nil_p(v) && RARRAY_LEN(v) > 0 &&
        mrb_equal(mrb, RARRAY_PTR(v)[0], k))
      return v;
  }
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     ary.rassoc(obj) -> new_ary or nil
 *
 *  Searches through the array whose elements are also arrays. Compares
 *  _obj_ with the second element of each contained array using
 *  `==`. Returns the first contained array that matches. See
 *  also `Array#assoc`.
 *
 *     a = [ [ 1, "one"], [2, "two"], [3, "three"], ["ii", "two"] ]
 *     a.rassoc("two")    #=> [2, "two"]
 *     a.rassoc("four")   #=> nil
 */

static mrb_value
ary_rassoc(mrb_state *mrb, mrb_value ary)
{
  mrb_value value = mrb_get_arg1(mrb);

  for (mrb_int i = 0; i < RARRAY_LEN(ary); i++) {
    mrb_value v = RARRAY_PTR(ary)[i];
    if (mrb_array_p(v) &&
        RARRAY_LEN(v) > 1 &&
        mrb_equal(mrb, RARRAY_PTR(v)[1], value))
      return v;
  }
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     ary.at(index)   ->   obj  or nil
 *
 *  Returns the element at _index_. A
 *  negative index counts from the end of `self`.  Returns `nil`
 *  if the index is out of range. See also `Array#[]`.
 *
 *     a = [ "a", "b", "c", "d", "e" ]
 *     a.at(0)     #=> "a"
 *     a.at(-1)    #=> "e"
 */

static mrb_value
ary_at(mrb_state *mrb, mrb_value ary)
{
  mrb_int pos = mrb_as_int(mrb,  mrb_get_arg1(mrb));

  return mrb_ary_entry(ary, pos);
}

/* Helper function for values_at - returns element at index n */
static mrb_value
ary_ref(mrb_state *mrb, mrb_value ary, mrb_int n)
{
  return mrb_ary_entry(ary, n);
}

/*
 *  call-seq:
 *     ary.values_at(selector, ...)  -> new_ary
 *
 *  Returns an array containing the elements in `self` corresponding to the
 *  given `selector`(s). The selectors may be either integer indices or ranges.
 *
 *     a = %w{ a b c d e f }
 *     a.values_at(1, 3, 5)          # => ["b", "d", "f"]
 *     a.values_at(1, 3, 5, 7)       # => ["b", "d", "f", nil]
 *     a.values_at(-1, -2, -2, -7)   # => ["f", "e", "e", nil]
 *     a.values_at(4..6, 3...6)      # => ["e", "f", nil, "d", "e", "f"]
 */

static mrb_value
ary_values_at(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb);
  const mrb_value *argv = mrb_get_argv(mrb);

  return mrb_get_values_at(mrb, self, RARRAY_LEN(self), argc, argv, ary_ref);
}

mrb_value mrb_ary_delete_at(mrb_state *mrb, mrb_value self);

/*
 *  call-seq:
 *     ary.slice!(index)         -> obj or nil
 *     ary.slice!(start, length) -> new_ary or nil
 *     ary.slice!(range)         -> new_ary or nil
 *
 *  Deletes the element(s) given by an `index` (optionally up to `length`
 *  elements) or by a `range`.
 *
 *  Returns the deleted object (or objects), or `nil` if the `index` is out of
 *  range.
 *
 *     a = [ "a", "b", "c" ]
 *     a.slice!(1)     #=> "b"
 *     a               #=> ["a", "c"]
 *     a.slice!(-1)    #=> "c"
 *     a               #=> ["a"]
 *     a.slice!(100)   #=> nil
 *     a               #=> ["a"]
 */

static mrb_value
ary_slice_bang(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int i, len;

  mrb_ary_modify(mrb, a);

  if (mrb_get_argc(mrb) == 1) {
    mrb_value index = mrb_get_arg1(mrb);

    if (mrb_type(index) == MRB_TT_RANGE) {
      if (mrb_range_beg_len(mrb, index, &i, &len, ARY_LEN(a), TRUE) != MRB_RANGE_OK) {
        return mrb_nil_value();
      }
    }
    else {
      return mrb_ary_delete_at(mrb, self);
    }
  }
  else {
    mrb_get_args(mrb, "ii", &i, &len);
  }

  mrb_int alen = ARY_LEN(a);
  if (i < 0) i += alen;
  if (i < 0 || alen < i) return mrb_nil_value();
  if (len < 0) return mrb_nil_value();
  if (alen == i) return mrb_ary_new(mrb);
  if (len > alen - i) len = alen - i;

  mrb_value ary = mrb_ary_new_from_values(mrb, len, ARY_PTR(a) + i);

  /* refresh pointer after mrb_ary_new_from_values */
  a = mrb_ary_ptr(self);

  for (int j = i; j < alen - len; j++) {
    ARY_PTR(a)[j] = ARY_PTR(a)[j+len];
  }

  mrb_ary_resize(mrb, self, alen - len);
  return ary;
}

/*
 * call-seq:
 *    ary.compact!    -> ary  or  nil
 *
 * Removes `nil` elements from the array.
 * Returns `nil` if no changes were made, otherwise returns
 * *ary*.
 *
 *    [ "a", nil, "b", nil, "c" ].compact! #=> [ "a", "b", "c" ]
 *    [ "a", "b", "c" ].compact!           #=> nil
 */
static mrb_value
ary_compact_bang(mrb_state *mrb, mrb_value self)
{
  struct RArray *a = mrb_ary_ptr(self);
  mrb_int i, j = 0;
  mrb_int len = ARY_LEN(a);

  mrb_ary_modify(mrb, a);
  /* a is still valid here, as mrb_ary_modify only modifies the RArray struct, not reallocates it */
  /* Hoist pointer retrieval outside loop to avoid repeated conditionals */
  mrb_value *ptr = RARRAY_PTR(self);
  for (i = 0; i < len; i++) {
    if (!mrb_nil_p(ptr[i])) {
      if (i != j) ptr[j] = ptr[i];
      j++;
    }
  }
  if (i == j) return mrb_nil_value();
  ARY_SET_LEN(RARRAY(self), j);
  return self;
}

/*
 * call-seq:
 *    ary.compact     -> new_ary
 *
 * Returns a copy of `self` with all `nil` elements removed.
 *
 *   [ "a", nil, "b", nil, "c", nil ].compact
 *                      #=> [ "a", "b", "c" ]
 */

static mrb_value
ary_compact(mrb_state *mrb, mrb_value self)
{
  mrb_value ary = mrb_ary_dup(mrb, self);
  ary_compact_bang(mrb, ary);
  return ary;
}


/*
 *  call-seq:
 *     ary.rotate(count=1)    -> new_ary
 *
 *  Returns a new array by rotating `self` so that the element at `count` is
 *  the first element of the new array.
 *
 *  If `count` is negative then it rotates in the opposite direction, starting
 *  from the end of `self` where +-1+ is the last element.
 *
 *     a = [ "a", "b", "c", "d" ]
 *     a.rotate         #=> ["b", "c", "d", "a"]
 *     a                #=> ["a", "b", "c", "d"]
 *     a.rotate(2)      #=> ["c", "d", "a", "b"]
 *     a.rotate(-3)     #=> ["b", "c", "d", "a"]
 */
static mrb_value
ary_rotate(mrb_state *mrb, mrb_value self)
{
  mrb_int count=1;
  mrb_get_args(mrb, "|i", &count);

  mrb_value ary = mrb_ary_new(mrb);
  mrb_int len = RARRAY_LEN(self);
  mrb_int idx;

  if (len <= 0) return ary;
  if (count < 0) {
    idx = len - (~count % len) - 1;
  }
  else {
    idx = count % len;
  }
  /* Hoist pointer retrieval outside loop */
  mrb_value *ptr = RARRAY_PTR(self);
  for (mrb_int i = 0; i<len; i++) {
    mrb_ary_push(mrb, ary, ptr[idx++]);
    if (idx == len) idx = 0;
  }
  return ary;
}

/* Helper function to reverse array elements in-place between beg and end indices */
static void
rev(mrb_value *p, mrb_int beg, mrb_int end)
{
  for (mrb_int i=beg,j=end-1; i<j; i++,j--) {
    mrb_value v = p[i];
    p[i] = p[j];
    p[j] = v;
  }
}

/*
 *  call-seq:
 *     ary.rotate!(count=1)   -> ary
 *
 *  Rotates `self` in place so that the element at `count` comes first, and
 *  returns `self`.
 *
 *  If `count` is negative then it rotates in the opposite direction, starting
 *  from the end of the array where `-1` is the last element.
 *
 *     a = [ "a", "b", "c", "d" ]
 *     a.rotate!        #=> ["b", "c", "d", "a"]
 *     a                #=> ["b", "c", "d", "a"]
 *     a.rotate!(2)     #=> ["d", "a", "b", "c"]
 *     a.rotate!(-3)    #=> ["a", "b", "c", "d"]
 */
static mrb_value
ary_rotate_bang(mrb_state *mrb, mrb_value self)
{
  mrb_int count=1;
  mrb_get_args(mrb, "|i", &count);

  struct RArray *a = mrb_ary_ptr(self);
  mrb_int len = ARY_LEN(a);
  mrb_int idx;

  mrb_ary_modify(mrb, a);
  mrb_value *p = ARY_PTR(a);
  if (len == 0 || count == 0) return self;
  if (count == 1) {
    mrb_value v = p[0];
    for (mrb_int i=1; i<len; i++) {
      p[i-1] = p[i];
    }
    p[len-1] = v;
    return self;
  }
  if (count < 0) {
    idx = len - (~count % len) - 1;
  }
  else {
    idx = count % len;
  }
  /* e.g. [1,2,3,4,5].rotate!(2) -> [3,4,5,1,2] */
  /* first, reverse the whole array */
  /* [1,2,3,4,5] -> [5,4,3,2,1] */
  rev(p, 0, len);
  /* then, re-reverse part before idx */
  /* [5,4,3,2,1] -> [3,4,5,2,1] */
  /*        ^idx     ~~~~~      */
  rev(p, 0, len-idx);
  /* finally, re-reverse part after idx */
  /* [3,4,5,2,1] -> [3,4,5,1,2] */
  /*        ^idx           ~~~  */
  rev(p, len-idx, len);
  return self;
}

#define SET_OP_HASH_THRESHOLD 32

/* Helper functions for temporary khash sets */
static void
ary_init_temp_set(mrb_state *mrb, ary_set_t *set, mrb_int capacity)
{
  kh_init_data(ary_set, mrb, set, (khint_t)(capacity > 0 ? capacity : 8));
}

static void
ary_populate_temp_set(mrb_state *mrb, ary_set_t *set, mrb_value ary)
{
  mrb_int len = RARRAY_LEN(ary);
  for (mrb_int i = 0; i < len; i++) {
    kh_put(ary_set, mrb, set, RARRAY_PTR(ary)[i]);
  }
}

static void
ary_destroy_temp_set(mrb_state *mrb, ary_set_t *set)
{
  if (set) {
    kh_destroy_data(ary_set, mrb, set);
  }
}


static mrb_int
ary_get_array_args(mrb_state *mrb, mrb_int argc, const mrb_value **argv_ptr)
{
  mrb_int total_len = 0;
  const mrb_value *argv = *argv_ptr;
  mrb_value *converted_argv = (mrb_value *)mrb_alloca(mrb, sizeof(mrb_value) * argc);

  for (mrb_int i = 0; i < argc; i++) {
    mrb_value other = mrb_check_array_type(mrb, argv[i]);
    if (mrb_nil_p(other)) {
      mrb_raise(mrb, E_TYPE_ERROR, "can't convert passed argument to Array");
    }
    converted_argv[i] = other;
    total_len += RARRAY_LEN(other);
  }
  *argv_ptr = converted_argv;
  return total_len;
}

struct ary_subtract_ctx {
  ary_set_t *set;
  mrb_value self;
  mrb_value result;
  const mrb_value *argv;
  mrb_int argc;
};

static mrb_value
ary_subtract_body(mrb_state *mrb, mrb_value data)
{
  struct ary_subtract_ctx *ctx = (struct ary_subtract_ctx *)mrb_cptr(data);

  for (mrb_int i = 0; i < ctx->argc; i++) {
    ary_populate_temp_set(mrb, ctx->set, ctx->argv[i]);
  }

  for (mrb_int i = 0; i < RARRAY_LEN(ctx->self); i++) {
    mrb_value p = RARRAY_PTR(ctx->self)[i];
    khiter_t k = kh_get(ary_set, mrb, ctx->set, p);
    if (kh_is_end(ctx->set, k)) {  /* key doesn't exist in any ary */
      mrb_ary_push(mrb, ctx->result, p);
    }
  }

  return ctx->result;
}

static mrb_value
ary_subtract_ensure(mrb_state *mrb, mrb_value data)
{
  struct ary_subtract_ctx *ctx = (struct ary_subtract_ctx *)mrb_cptr(data);
  ary_destroy_temp_set(mrb, ctx->set);
  return mrb_nil_value();
}

static mrb_value
ary_subtract_internal(mrb_state *mrb, mrb_value self, mrb_int argc, const mrb_value *argv)
{
  if (argc == 0) {
    return mrb_ary_dup(mrb, self);
  }

  mrb_int total_len = ary_get_array_args(mrb, argc, &argv);

  mrb_value result = mrb_ary_new(mrb);

  if (total_len > SET_OP_HASH_THRESHOLD) {
    /* Create shared copies to protect elements during khash operations */
    mrb_value *argv_copies = (mrb_value *)mrb_alloca(mrb, sizeof(mrb_value) * argc);
    for (mrb_int i = 0; i < argc; i++) {
      argv_copies[i] = mrb_ary_make_shared_copy(mrb, argv[i]);
    }

    ary_set_t set_struct;
    ary_set_t *set = &set_struct;
    ary_init_temp_set(mrb, set, total_len);

    struct ary_subtract_ctx ctx = { set, self, result, argv_copies, argc };
    mrb_ensure(mrb, ary_subtract_body, mrb_cptr_value(mrb, &ctx),
                    ary_subtract_ensure, mrb_cptr_value(mrb, &ctx));
  }
  else {
    mrb_int self_len = RARRAY_LEN(self);
    for (mrb_int i = 0; i < self_len; i++) {
      mrb_value p = RARRAY_PTR(self)[i];
      mrb_bool found = FALSE;
      for (mrb_int j = 0; j < argc; j++) {
        mrb_int len = RARRAY_LEN(argv[j]);
        for (mrb_int k = 0; k < len; k++) {
          if (mrb_equal(mrb, p, RARRAY_PTR(argv[j])[k])) {
            found = TRUE;
            break;
          }
        }
        if (found) break;
      }
      if (!found) {
        mrb_ary_push(mrb, result, p);
      }
    }
  }

  return result;
}

/*
 *  call-seq:
 *     ary - other_ary   -> new_ary
 *
 *  Returns a new array that is a copy of the original array, with any items
 *  that also appear in `other_ary` removed.
 *
 *     [ 1, 1, 2, 2, 3, 3, 4, 5 ] - [ 1, 2, 4 ]  #=> [ 3, 3, 5 ]
 */

static mrb_value
ary_sub(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "A", &other);
  return ary_subtract_internal(mrb, self, 1, &other);
}

/*
 *  call-seq:
 *     ary.difference(other_ary, ...)   -> new_ary
 *
 *  Returns a new array that is a copy of the original array, removing all
 *  occurrences of any item that also appear in any of the `other_ary`s.
 *  The order is preserved from the original array.
 *
 *    [1, 2, 3, 4, 5].difference([2, 4], [1, 5])  #=> [3]
 */
static mrb_value
ary_difference(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_get_args(mrb, "*", &argv, &argc);
  return ary_subtract_internal(mrb, self, argc, argv);
}


static void
add_uniq(mrb_state *mrb, mrb_value item, mrb_value result)
{
  const mrb_int len = RARRAY_LEN(result);
  for (mrb_int i = 0; i < len; i++) {
    if (mrb_eql(mrb, item, RARRAY_PTR(result)[i])) {
      return;
    }
  }
  mrb_ary_push(mrb, result, item);
}

struct ary_union_ctx {
  ary_set_t *set;
  mrb_value self_copy;
  mrb_value result;
  const mrb_value *argv;
  mrb_int argc;
};

static mrb_value
ary_union_body(mrb_state *mrb, mrb_value data)
{
  struct ary_union_ctx *ctx = (struct ary_union_ctx *)mrb_cptr(data);

  /* Add unique elements from self */
  for (mrb_int i = 0; i < RARRAY_LEN(ctx->self_copy); i++) {
    mrb_value elem = RARRAY_PTR(ctx->self_copy)[i];
    khiter_t k = kh_get(ary_set, mrb, ctx->set, elem);
    if (kh_is_end(ctx->set, k)) {
      kh_put(ary_set, mrb, ctx->set, elem);
      mrb_ary_push(mrb, ctx->result, elem);
    }
  }

  /* Add unique elements from others */
  for (mrb_int i = 0; i < ctx->argc; i++) {
    mrb_value other = ctx->argv[i];
    for (mrb_int j = 0; j < RARRAY_LEN(other); j++) {
      mrb_value elem = RARRAY_PTR(other)[j];
      khiter_t k = kh_get(ary_set, mrb, ctx->set, elem);
      if (kh_is_end(ctx->set, k)) {
        kh_put(ary_set, mrb, ctx->set, elem);
        mrb_ary_push(mrb, ctx->result, elem);
      }
    }
  }

  return ctx->result;
}

static mrb_value
ary_union_ensure(mrb_state *mrb, mrb_value data)
{
  struct ary_union_ctx *ctx = (struct ary_union_ctx *)mrb_cptr(data);
  ary_destroy_temp_set(mrb, ctx->set);
  return mrb_nil_value();
}

static mrb_value
ary_union_internal(mrb_state *mrb, mrb_value self, mrb_int argc, const mrb_value *argv)
{
  mrb_int total_len = ary_get_array_args(mrb, argc, &argv) + RARRAY_LEN(self);

  mrb_value result = mrb_ary_new(mrb);

  if (total_len > SET_OP_HASH_THRESHOLD) {
    /* Create shared copies to protect elements during khash operations */
    mrb_value self_copy = mrb_ary_make_shared_copy(mrb, self);
    mrb_value *argv_copies = (mrb_value *)mrb_alloca(mrb, sizeof(mrb_value) * argc);
    for (mrb_int i = 0; i < argc; i++) {
      argv_copies[i] = mrb_ary_make_shared_copy(mrb, argv[i]);
    }

    ary_set_t set_struct;
    ary_set_t *set = &set_struct;
    ary_init_temp_set(mrb, set, total_len);

    struct ary_union_ctx ctx = { set, self_copy, result, argv_copies, argc };
    mrb_ensure(mrb, ary_union_body, mrb_cptr_value(mrb, &ctx),
                    ary_union_ensure, mrb_cptr_value(mrb, &ctx));
  }
  else {
    /* Use linear search for small arrays */
    /* Add unique elements from self */
    mrb_int alen = RARRAY_LEN(self);
    for (mrb_int i = 0; i < alen; i++) {
      add_uniq(mrb, RARRAY_PTR(self)[i], result);
    }

    /* Add unique elements from others */
    for (mrb_int i = 0; i < argc; i++) {
      mrb_value other = argv[i];
      mrb_int olen = RARRAY_LEN(other);
      for (mrb_int j = 0; j < olen; j++) {
        add_uniq(mrb, RARRAY_PTR(other)[j], result);
      }
    }
  }

  return result;
}

/*
 *  call-seq:
 *     ary | other_ary     -> new_ary
 *
 *  Set Union---Returns a new array by joining this array with
 *  `other_ary`, removing duplicates.
 *
 *     [ "a", "b", "c" ] | [ "c", "d", "a" ]
 *           #=> [ "a", "b", "c", "d" ]
 */

static mrb_value
ary_union(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "A", &other);
  return ary_union_internal(mrb, self, 1, &other);
}

/*
 *  call-seq:
 *    ary.union(other_ary,...)  -> new_ary
 *
 *  Set Union---Returns a new array by joining this array with
 *  `other_ary`s, removing duplicates.
 *
 *    ["a", "b", "c"].union(["c", "d", "a"], ["a", "c", "e"])
 *           #=> ["a", "b", "c", "d", "e"]
 */
static mrb_value
ary_union_multi(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_get_args(mrb, "*", &argv, &argc);
  return ary_union_internal(mrb, self, argc, argv);
}

struct ary_intersection_ctx {
  ary_set_t *set;
  mrb_value self;
  mrb_value result;
  const mrb_value *argv;
  mrb_int argc;
};

static mrb_value
ary_intersection_body(mrb_state *mrb, mrb_value data)
{
  struct ary_intersection_ctx *ctx = (struct ary_intersection_ctx *)mrb_cptr(data);

  for (mrb_int i = 0; i < ctx->argc; i++) {
    ary_populate_temp_set(mrb, ctx->set, ctx->argv[i]);
  }

  for (mrb_int i = 0; i < RARRAY_LEN(ctx->self); i++) {
    mrb_value p = RARRAY_PTR(ctx->self)[i];
    khiter_t k = kh_get(ary_set, mrb, ctx->set, p);
    if (!kh_is_end(ctx->set, k)) {
      mrb_ary_push(mrb, ctx->result, p);
      kh_del(ary_set, mrb, ctx->set, k);
    }
  }

  return ctx->result;
}

static mrb_value
ary_intersection_ensure(mrb_state *mrb, mrb_value data)
{
  struct ary_intersection_ctx *ctx = (struct ary_intersection_ctx *)mrb_cptr(data);
  ary_destroy_temp_set(mrb, ctx->set);
  return mrb_nil_value();
}

static mrb_value
ary_intersection_internal(mrb_state *mrb, mrb_value self, mrb_int argc, const mrb_value *argv)
{
  if (argc == 0) {
    return mrb_ary_new(mrb);
  }

  mrb_int total_len = ary_get_array_args(mrb, argc, &argv);

  mrb_value result = mrb_ary_new(mrb);

  if (total_len > SET_OP_HASH_THRESHOLD) {
    /* Create shared copies to protect elements during khash operations */
    mrb_value *argv_copies = (mrb_value *)mrb_alloca(mrb, sizeof(mrb_value) * argc);
    for (mrb_int i = 0; i < argc; i++) {
      argv_copies[i] = mrb_ary_make_shared_copy(mrb, argv[i]);
    }

    ary_set_t set_struct;
    ary_set_t *set = &set_struct;
    ary_init_temp_set(mrb, set, total_len);

    struct ary_intersection_ctx ctx = { set, self, result, argv_copies, argc };
    mrb_ensure(mrb, ary_intersection_body, mrb_cptr_value(mrb, &ctx),
                    ary_intersection_ensure, mrb_cptr_value(mrb, &ctx));
  }
  else {
    mrb_int self_len = RARRAY_LEN(self);
    for (mrb_int i = 0; i < self_len; i++) {
      mrb_value p = RARRAY_PTR(self)[i];
      mrb_bool found_in_all = TRUE;

      for (mrb_int j = 0; j < argc; j++) {
        mrb_bool found_in_current_other = FALSE;
        mrb_int len = RARRAY_LEN(argv[j]);
        for (mrb_int k = 0; k < len; k++) {
          if (mrb_equal(mrb, p, RARRAY_PTR(argv[j])[k])) {
            found_in_current_other = TRUE;
            break;
          }
        }
        if (!found_in_current_other) {
          found_in_all = FALSE;
          break;
        }
      }

      if (found_in_all) {
        mrb_bool already_added = FALSE;
        mrb_int result_len = RARRAY_LEN(result);
        for (mrb_int j = 0; j < result_len; j++) {
          if (mrb_equal(mrb, p, RARRAY_PTR(result)[j])) {
            already_added = TRUE;
            break;
          }
        }
        if (!already_added) {
          mrb_ary_push(mrb, result, p);
        }
      }
    }
  }
  return result;
}

/*
 *  call-seq:
 *     ary & other_ary      -> new_ary
 *
 *  Set Intersection---Returns a new array
 *  containing elements common to the two arrays, with no duplicates.
 *
 *     [ 1, 1, 3, 5 ] & [ 1, 2, 3 ]   #=> [ 1, 3 ]
 */

static mrb_value
ary_intersection(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "A", &other);
  return ary_intersection_internal(mrb, self, 1, &other);
}

/*
 *  call-seq:
 *    ary.intersection(other_ary,...)  -> new_ary
 *
 *  Set Intersection---Returns a new array containing elements common to
 *  this array and `other_ary`s, removing duplicates. The order is
 *  preserved from the original array.
 *
 *    [1, 2, 3].intersection([3, 4, 1], [1, 3, 5])  #=> [1, 3]
 */
static mrb_value
ary_intersection_multi(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;
  mrb_get_args(mrb, "*", &argv, &argc);
  return ary_intersection_internal(mrb, self, argc, argv);
}

/*
 *  call-seq:
 *    ary.intersect?(other_ary)   -> true or false
 *
 *  Returns `true` if the array and `other_ary` have at least one element in
 *  common, otherwise returns `false`.
 *
 *     a = [ 1, 2, 3 ]
 *     b = [ 3, 4, 5 ]
 *     c = [ 5, 6, 7 ]
 *     a.intersect?(b)   #=> true
 *     a.intersect?(c)   #=> false
 */

struct ary_intersect_p_ctx {
  ary_set_t *set;
  mrb_value shorter_ary_copy;
  mrb_value longer_ary;
  mrb_bool *found;
};

static mrb_value
ary_intersect_p_body(mrb_state *mrb, mrb_value data)
{
  struct ary_intersect_p_ctx *ctx = (struct ary_intersect_p_ctx *)mrb_cptr(data);

  ary_populate_temp_set(mrb, ctx->set, ctx->shorter_ary_copy);

  for (mrb_int i = 0; i < RARRAY_LEN(ctx->longer_ary); i++) {
    khiter_t k = kh_get(ary_set, mrb, ctx->set, RARRAY_PTR(ctx->longer_ary)[i]);
    if (!kh_is_end(ctx->set, k)) {
      *ctx->found = TRUE;
      break;
    }
  }

  return mrb_nil_value();
}

static mrb_value
ary_intersect_p_ensure(mrb_state *mrb, mrb_value data)
{
  struct ary_intersect_p_ctx *ctx = (struct ary_intersect_p_ctx *)mrb_cptr(data);
  ary_destroy_temp_set(mrb, ctx->set);
  return mrb_nil_value();
}

static mrb_value
ary_intersect_p(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "A", &other);

  mrb_value shorter_ary, longer_ary;
  if (RARRAY_LEN(self) > RARRAY_LEN(other)) {
    shorter_ary = other;
    longer_ary = self;
  }
  else {
    shorter_ary = self;
    longer_ary = other;
  }

  if (RARRAY_LEN(shorter_ary) == 0 || RARRAY_LEN(longer_ary) == 0) {
    return mrb_false_value();
  }

  if (RARRAY_LEN(shorter_ary) > SET_OP_HASH_THRESHOLD) {
    mrb_value shorter_ary_copy = mrb_ary_make_shared_copy(mrb, shorter_ary);

    ary_set_t set_struct;
    ary_set_t *set = &set_struct;
    ary_init_temp_set(mrb, set, RARRAY_LEN(shorter_ary_copy));

    mrb_bool found = FALSE;

    struct ary_intersect_p_ctx ctx = { set, shorter_ary_copy, longer_ary, &found };
    mrb_ensure(mrb, ary_intersect_p_body, mrb_cptr_value(mrb, &ctx),
                    ary_intersect_p_ensure, mrb_cptr_value(mrb, &ctx));

    if (found) {
      return mrb_true_value();
    }
  }
  else {
    for (mrb_int i = 0; i < RARRAY_LEN(longer_ary); i++) {
      for (mrb_int j = 0; j < RARRAY_LEN(shorter_ary); j++) {
        if (mrb_equal(mrb, RARRAY_PTR(longer_ary)[i], RARRAY_PTR(shorter_ary)[j])) {
          return mrb_true_value();
        }
      }
    }
  }

  return mrb_false_value();
}

/*
 *  Internal helper for Array#fill that handles all the complex
 *  argument parsing logic including ranges, negative indices, etc.
 *  Returns normalized [start, length] array for use by ary_fill_exec.
 */

static mrb_value
ary_fill_parse_arg(mrb_state *mrb, mrb_value self)
{
  mrb_value arg0 = mrb_nil_value(), arg1 = mrb_nil_value(), arg2 = mrb_nil_value();
  mrb_value block = mrb_nil_value();
  mrb_int argc = mrb_get_args(mrb, "|ooo&", &arg0, &arg1, &arg2, &block);

  struct RArray *ary = mrb_ary_ptr(self);
  mrb_int ary_len = ARY_LEN(ary);
  mrb_int start = 0, length = 0;

  if (!mrb_nil_p(block)) {
    if (argc == 0 || (argc >= 1 && mrb_nil_p(arg0))) {
      /* fill { |index| block } */
      start = 0;
      length = ary_len;
    }
    else if (argc >= 1 && mrb_range_p(arg0)) {
      /* fill(range) { |index| block } */
      mrb_int range_beg, range_end;

      if (mrb_range_beg_len(mrb, arg0, &range_beg, &range_end, ary_len, 1)) {
        start = range_beg;
        length = range_end;
      }
    }
    else if (argc >= 1 && !mrb_nil_p(arg0)) {
      /* fill(start [, length]) { |index| block } */
      start = mrb_int(mrb, arg0);
      if (start < 0) start += ary_len;
      if (start < 0) start = 0;

      if (argc == 1 || mrb_nil_p(arg1)) {
        length = ary_len - start;
      }
      else {
        length = mrb_int(mrb, arg1);
        if (length < 0) length = 0;
      }
    }
  }
  else {
    if (argc >= 1 && !mrb_nil_p(arg0)) {
      if (argc == 1 || (argc >= 2 && mrb_nil_p(arg1) && mrb_nil_p(arg2))) {
        /* fill(obj) */
        start = 0;
        length = ary_len;
      }
      else if (argc >= 2 && mrb_range_p(arg1)) {
        /* fill(obj, range) */
        mrb_int range_beg, range_end;

        if (mrb_range_beg_len(mrb, arg1, &range_beg, &range_end, ary_len, 1)) {
          start = range_beg;
          length = range_end;
        }
      }
      else if (argc >= 2 && !mrb_nil_p(arg1)) {
        /* fill(obj, start [, length]) */
        start = mrb_int(mrb, arg1);
        if (start < 0) start += ary_len;
        if (start < 0) start = 0;

        if (argc == 2 || mrb_nil_p(arg2)) {
          length = ary_len - start;
        }
        else {
          length = mrb_int(mrb, arg2);
          if (length < 0) length = 0;
        }
      }
    }
  }

  /* Return [start, length] array */
  mrb_value result = mrb_ary_new_capa(mrb, 2);
  mrb_ary_push(mrb, result, mrb_fixnum_value(start));
  mrb_ary_push(mrb, result, mrb_fixnum_value(length));
  return result;
}

/*
 *  Internal helper that fills a specific range of the array
 *  with the given object. Handles array extension if necessary.
 *  Used by Ruby-level Array#fill method.
 */

static mrb_value
ary_fill_exec(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_int start, length;

  mrb_get_args(mrb, "iio", &start, &length, &obj);

  if (start < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative start index");
  }
  if (length < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "negative length");
  }

  struct RArray *ary = mrb_ary_ptr(self);
  mrb_int ary_len = ARY_LEN(ary);

  /* Extend array if necessary */
  if (start + length > ary_len) {
    mrb_ary_resize(mrb, self, start + length);
    ary = mrb_ary_ptr(self);  /* refresh pointer after resize */
  }

  /* Ensure we don't go beyond array bounds */
  if (start >= ARY_LEN(ary) || length <= 0) return self;
  if (start + length > ARY_LEN(ary)) {
    length = ARY_LEN(ary) - start;
  }

  /* Fill the array */
  mrb_value *ptr = ARY_PTR(ary) + start;
  for (mrb_int i = 0; i < length; i++) {
    ptr[i] = obj;
  }

  return self;
}

/*
 *  Internal helper for Array#uniq! without blocks.
 *  Modifies array in-place, returns nil if no changes.
 */
struct ary_uniq_bang_ctx {
  ary_set_t *set;
  mrb_value self_copy;
  mrb_value self;
  mrb_int *write_pos;
  mrb_int len;
};

static mrb_value
ary_uniq_bang_body(mrb_state *mrb, mrb_value data)
{
  struct ary_uniq_bang_ctx *ctx = (struct ary_uniq_bang_ctx *)mrb_cptr(data);

  ary_populate_temp_set(mrb, ctx->set, ctx->self_copy);

  for (mrb_int read_pos = 0; read_pos < ctx->len; read_pos++) {
    mrb_value elem = RARRAY_PTR(ctx->self)[read_pos];
    khiter_t k = kh_get(ary_set, mrb, ctx->set, elem);
    if (!kh_is_end(ctx->set, k)) {
      if (*ctx->write_pos != read_pos) {
        RARRAY_PTR(ctx->self)[*ctx->write_pos] = elem;
      }
      (*ctx->write_pos)++;
      kh_del(ary_set, mrb, ctx->set, k);
    }
  }

  return mrb_nil_value();
}

static mrb_value
ary_uniq_bang_ensure(mrb_state *mrb, mrb_value data)
{
  struct ary_uniq_bang_ctx *ctx = (struct ary_uniq_bang_ctx *)mrb_cptr(data);
  ary_destroy_temp_set(mrb, ctx->set);
  return mrb_nil_value();
}

static mrb_value
ary_uniq_bang(mrb_state *mrb, mrb_value self)
{
  mrb_int len = RARRAY_LEN(self);

  if (len <= 1) {
    return mrb_nil_value();
  }

  mrb_ary_modify(mrb, mrb_ary_ptr(self));
  mrb_int write_pos = 0;

  if (len > SET_OP_HASH_THRESHOLD) {
    /* Create shared copy to protect elements during khash operations */
    mrb_value self_copy = mrb_ary_make_shared_copy(mrb, self);

    ary_set_t set_struct;
    ary_set_t *set = &set_struct;
    ary_init_temp_set(mrb, set, len);

    struct ary_uniq_bang_ctx ctx = { set, self_copy, self, &write_pos, len };
    mrb_ensure(mrb, ary_uniq_bang_body, mrb_cptr_value(mrb, &ctx),
                    ary_uniq_bang_ensure, mrb_cptr_value(mrb, &ctx));
  }
  else {
    for (mrb_int read_pos = 0; read_pos < len; read_pos++) {
      mrb_value elem = RARRAY_PTR(self)[read_pos];
      mrb_bool found = FALSE;
      for (mrb_int j = 0; j < write_pos; j++) {
        if (mrb_equal(mrb, elem, RARRAY_PTR(self)[j])) {
          found = TRUE;
          break;
        }
      }
      if (!found) {
        if (write_pos != read_pos) {
          RARRAY_PTR(self)[write_pos] = elem;
        }
        write_pos++;
      }
    }
  }

  if (write_pos == len) {
    return mrb_nil_value();
  }

  mrb_ary_resize(mrb, self, write_pos);
  return self;
}

/*
 *  Internal helper for Array#uniq without blocks.
 *  Uses hash-based deduplication for large arrays,
 *  linear search for small arrays.
 */
static mrb_value
ary_uniq(mrb_state *mrb, mrb_value self)
{
  mrb_value ary = mrb_ary_dup(mrb, self);
  ary_uniq_bang(mrb, ary);
  return ary;
}

/* Internal helper for flatten operations using iterative stack-based approach */
static mrb_value
flatten_internal(mrb_state *mrb, mrb_value self, mrb_int level, mrb_bool *modified)
{
  *modified = FALSE;
  mrb_value result = mrb_ary_new(mrb);
  mrb_value stack = mrb_ary_new(mrb);
  mrb_ary_push(mrb, stack, self);
  mrb_ary_push(mrb, stack, mrb_fixnum_value(0)); // index
  mrb_ary_push(mrb, stack, mrb_fixnum_value(1)); // depth

  while (RARRAY_LEN(stack) > 0) {
    mrb_int depth = mrb_fixnum(mrb_ary_pop(mrb, stack));
    mrb_int idx = mrb_fixnum(mrb_ary_pop(mrb, stack));
    mrb_value ary = mrb_ary_pop(mrb, stack);

    while (idx < RARRAY_LEN(ary)) {
      mrb_value e = mrb_ary_entry(ary, idx);
      idx++;

      if (mrb_array_p(e) && (level < 0 || depth <= level)) {
        *modified = TRUE;
        // Push current state back
        mrb_ary_push(mrb, stack, ary);
        mrb_ary_push(mrb, stack, mrb_fixnum_value(idx));
        mrb_ary_push(mrb, stack, mrb_fixnum_value(depth));

        // Push new array to process
        ary = e;
        idx = 0;
        depth++;
      }
      else {
        mrb_ary_push(mrb, result, e);
      }
    }
  }
  return result;
}

/*
 *  call-seq:
 *     ary.flatten -> new_ary
 *     ary.flatten(level) -> new_ary
 *
 *  Returns a new array that is a one-dimensional flattening of this
 *  array (recursively). That is, for every element that is an array,
 *  extract its elements into the new array. If the optional
 *  `level` argument determines the level of recursion to flatten.
 *
 *    s = [ 1, 2, 3 ]           #=> [1, 2, 3]
 *    t = [ 4, 5, 6, [7, 8] ]   #=> [4, 5, 6, [7, 8]]
 *    a = [ s, t, 9, 10 ]       #=> [[1, 2, 3], [4, 5, 6, [7, 8]], 9, 10]
 *    a.flatten                 #=> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
 *    a = [ 1, 2, [3, [4, 5] ] ]
 *    a.flatten(1)              #=> [1, 2, 3, [4, 5]]
 */
static mrb_value
ary_flatten(mrb_state *mrb, mrb_value self)
{
  mrb_int level = -1;
  mrb_get_args(mrb, "|i", &level);
  mrb_bool modified; // dummy
  return flatten_internal(mrb, self, level, &modified);
}

/*
 *  Internal helper for index normalization and bounds checking.
 *  Returns normalized index if in bounds, nil if out of bounds.
 *  Used by Ruby-level array methods.
 */

static mrb_value
ary_normalize_index(mrb_state *mrb, mrb_value self)
{
  mrb_value index_val;
  mrb_get_args(mrb, "o", &index_val);

  mrb_int index = mrb_as_int(mrb, index_val);
  struct RArray *ary = mrb_ary_ptr(self);
  mrb_int len = ARY_LEN(ary);

  // Handle negative indices
  if (index < 0) {
    index += len;
  }

  // Check bounds
  if (index >= 0 && index < len) {
    return mrb_fixnum_value(index);
  }
  else {
    return mrb_nil_value();
  }
}

/*
 *  Internal helper for Array#fetch without blocks.
 *  Returns the element at index, or default if out of bounds.
 *  Raises IndexError if out of bounds and default equals none.
 */

static mrb_value
ary_fetch(mrb_state *mrb, mrb_value self)
{
  mrb_value index_val, default_val, none;
  mrb_get_args(mrb, "ooo", &index_val, &default_val, &none);

  // Convert index to integer
  mrb_int index = mrb_as_int(mrb, index_val);
  mrb_int original_index = index;  // Keep original for error message

  struct RArray *ary = mrb_ary_ptr(self);
  mrb_int len = ARY_LEN(ary);

  // Handle negative indices
  if (index < 0) {
    index += len;
  }

  // Check bounds
  if (index < 0 || index >= len) {
    // Check if default is the NONE sentinel (means no default provided)
    if (mrb_obj_equal(mrb, default_val, none)) {
      // No default provided - raise IndexError
      mrb_raisef(mrb, E_INDEX_ERROR,
                 "index %i outside of array bounds: %i...%i",
                 original_index, -len, len);
    }
    return default_val;
  }

  // Return element at index
  return ARY_PTR(ary)[index];
}

/*
 *  call-seq:
 *     ary.flatten!        -> ary or nil
 *     ary.flatten!(level) -> array or nil
 *
 *  Flattens `self` in place.  Returns `nil` if no modifications were made
 *  (i.e., *ary* contains no subarrays.) If the optional `level` argument
 *  determines the level of recursion to flatten.
 *
 *    a = [ 1, 2, [3, [4, 5] ] ]
 *    a.flatten!   #=> [1, 2, 3, 4, 5]
 *    a.flatten!   #=> nil
 *    a            #=> [1, 2, 3, 4, 5]
 *    a = [ 1, 2, [3, [4, 5] ] ]
 *    a.flatten!(1) #=> [1, 2, 3, [4, 5]]
 */
static mrb_value
ary_flatten_bang(mrb_state *mrb, mrb_value self)
{
  mrb_int level = -1;
  mrb_get_args(mrb, "|i", &level);

  mrb_ary_modify(mrb, mrb_ary_ptr(self));
  mrb_bool modified;
  mrb_value result = flatten_internal(mrb, self, level, &modified);

  if (!modified) {
    return mrb_nil_value();
  }
  mrb_ary_replace(mrb, self, result);
  return self;
}

/*
 *  call-seq:
 *     ary.insert(index, obj...)    -> ary
 *
 *  Inserts the given values before the element with the given index.
 *
 *  Negative indices count backwards from the end of the array, where -1
 *  is the last element. If a negative index is used, the elements are
 *  inserted after that element.
 *
 *  If the index is greater than the length of the array, the array is
 *  extended with nil elements.
 *
 *     a = %w{ a b c d }
 *     a.insert(2, 99)         #=> ["a", "b", 99, "c", "d"]
 *     a.insert(-2, 1, 2, 3)   #=> ["a", "b", 99, "c", 1, 2, 3, "d"]
 */
static mrb_value
ary_insert(mrb_state *mrb, mrb_value self)
{
  mrb_int idx;
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "i*", &idx, &argv, &argc);

  if (argc == 0) {
    return self;
  }

  mrb_int len = RARRAY_LEN(self);

  if (idx < 0) {
    idx += len + 1;
    if (idx < 0) {
       mrb_raisef(mrb, E_INDEX_ERROR, "index %i outside of array bounds", idx - (len + 1));
    }
  }

  mrb_ary_modify(mrb, mrb_ary_ptr(self));

  mrb_int new_len = (idx > len ? idx : len) + argc;
  mrb_ary_resize(mrb, self, new_len);

  if (idx < len) {
    memmove(RARRAY_PTR(self) + idx + argc, RARRAY_PTR(self) + idx, (len - idx) * sizeof(mrb_value));
  }

  for (mrb_int i = 0; i < argc; i++) {
    mrb_ary_set(mrb, self, idx + i, argv[i]);
  }

  return self;
}

/*
 *  Internal helper for Array#product to construct a group array.
 *  Takes the base array (self), the array of other arrays (arys),
 *  the current iteration index (current_i), and the desired length
 *  of the group array (group_len).
 */
static mrb_value
ary_product_group(mrb_state *mrb, mrb_value self_ary)
{
  mrb_value arys_ary;
  mrb_int current_i, group_len;
  mrb_get_args(mrb, "Aii", &arys_ary, &current_i, &group_len);

  mrb_value group = mrb_ary_new_capa(mrb, group_len);
  mrb_int j = RARRAY_LEN(arys_ary); // Corresponds to 'size' in Ruby
  mrb_int n = current_i;

  while (j > 0) {
    j -= 1;
    mrb_value a = RARRAY_PTR(arys_ary)[j]; // arys[j]
    mrb_check_type(mrb, a, MRB_TT_ARRAY);
    mrb_int b = RARRAY_LEN(a);             // a.size
    mrb_ary_set(mrb, group, j + 1, RARRAY_PTR(a)[n % b]);
    n /= b;
  }
  mrb_ary_set(mrb, group, 0, RARRAY_PTR(self_ary)[n]);

  return group;
}

/*
 *  call-seq:
 *     ary.deconstruct -> ary
 *
 *  Returns the array itself for pattern matching.
 *
 *  This method is used by pattern matching to deconstruct arrays.
 *  It simply returns the array itself, allowing pattern matching
 *  to work with array elements.
 *
 *     a = [1, 2, 3]
 *     a.deconstruct   #=> [1, 2, 3]
 *
 *  Pattern matching usage:
 *     case [1, 2, 3]
 *     in [x, y, z]
 *       # x=1, y=2, z=3
 *     end
 */
static mrb_value
ary_deconstruct(mrb_state *mrb, mrb_value ary)
{
  return ary;
}


/*
 *  Internal method to initialize combination state.
 *  Returns opaque state object for use by __combination_next.
 */
static mrb_value
ary_combination_init(mrb_state *mrb, mrb_value self)
{
  mrb_int n;
  mrb_bool permutation;

  mrb_get_args(mrb, "ib", &n, &permutation);
#if MRB_INT_MAX > SIZE_MAX
  if (n > SIZE_MAX) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "number too large");
  }
#endif

  struct RData *d;
  struct mrb_combination_state *state;
  Data_Make_Struct(mrb, mrb->object_class, struct mrb_combination_state,
                   &mrb_combination_state_type, state, d);

  state->n = n;
  state->array_size = RARRAY_LEN(self);
  state->permutation = permutation;
  state->finished = (n <= 0 && n != 0);

  if (n > 0) {
    state->indices = (mrb_int*)mrb_calloc(mrb, n, sizeof(mrb_int));
  }

  return mrb_obj_value(d);
}

/*
 *  Internal method to get next combination as index array.
 *  Returns array of indices or nil when iteration is complete.
 */
static mrb_value
ary_combination_next(mrb_state *mrb, mrb_value self)
{
  mrb_value state_obj;
  mrb_get_args(mrb, "o", &state_obj);

  struct mrb_combination_state *state;

  /* Validate state object type and get data */
  state = (struct mrb_combination_state*)mrb_data_check_and_get(mrb, state_obj, &mrb_combination_state_type);
  if (!state) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid combination state");
  }

  /* Check if iteration is complete */
  if (state->finished) return mrb_nil_value();

  /* Validate array hasn't been modified during iteration */
  if (RARRAY_LEN(self) != state->array_size) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "array modified during iteration");
  }

  /* Edge case: empty array */
  if (state->array_size == 0) {
    state->finished = TRUE;
    return mrb_nil_value();
  }

  /* Validate current indices are still in bounds */
  for (mrb_int i = 0; i < state->n; i++) {
    if (state->indices[i] >= state->array_size) {
      state->finished = TRUE;
      return mrb_nil_value();
    }
  }

  /* Build current combination indices */
  mrb_value result = mrb_ary_new_capa(mrb, state->n);
  for (mrb_int i = 0; i < state->n; i++) {
    mrb_ary_push(mrb, result, mrb_fixnum_value(state->indices[i]));
  }

  mrb_int pos = state->n - 1;

  while (pos >= 0) {
    state->indices[pos]++;
    if (state->indices[pos] < state->array_size) break;
    pos--;
  }

  if (pos < 0) {
    state->finished = TRUE;
  }
  else {
    /* Reset dependent indices */
    for (mrb_int i = pos + 1; i < state->n; i++) {
      if (state->permutation) {
        state->indices[i] = 0;
      }
      else {
        state->indices[i] = state->indices[i - 1];
      }
    }
  }

  return result;
}

void
mrb_mruby_array_ext_gem_init(mrb_state* mrb)
{
  struct RClass * a = mrb->array_class;

  mrb_define_method_id(mrb, a, MRB_SYM(assoc), ary_assoc,  MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(at), ary_at,     MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(rassoc), ary_rassoc, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(values_at), ary_values_at, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, a, MRB_SYM_B(slice), ary_slice_bang, MRB_ARGS_ARG(1,1));
  mrb_define_method_id(mrb, a, MRB_SYM(compact), ary_compact, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM_B(compact), ary_compact_bang, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM(rotate), ary_rotate, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, a, MRB_SYM_B(rotate), ary_rotate_bang, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, a, MRB_OPSYM(sub), ary_sub, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(difference), ary_difference, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, a, MRB_OPSYM(or), ary_union, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(union), ary_union_multi, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, a, MRB_OPSYM(and), ary_intersection, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(intersection), ary_intersection_multi, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, a, MRB_SYM_Q(intersect), ary_intersect_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(__fill_parse_arg), ary_fill_parse_arg, MRB_ARGS_ARG(0,4));
  mrb_define_method_id(mrb, a, MRB_SYM(__fill_exec), ary_fill_exec, MRB_ARGS_REQ(3));
  mrb_define_method_id(mrb, a, MRB_SYM(__uniq), ary_uniq, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM_B(__uniq), ary_uniq_bang, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM(flatten), ary_flatten, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, a, MRB_SYM_B(flatten), ary_flatten_bang, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, a, MRB_SYM(__normalize_index), ary_normalize_index, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, a, MRB_SYM(__fetch), ary_fetch, MRB_ARGS_REQ(3));
  mrb_define_method_id(mrb, a, MRB_SYM(insert), ary_insert, MRB_ARGS_ARG(1, -1));
  mrb_define_method_id(mrb, a, MRB_SYM(deconstruct), ary_deconstruct, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, a, MRB_SYM(__product_group), ary_product_group, MRB_ARGS_REQ(4));
  mrb_define_method_id(mrb, a, MRB_SYM(__combination_init), ary_combination_init, MRB_ARGS_REQ(2));
  mrb_define_method_id(mrb, a, MRB_SYM(__combination_next), ary_combination_next, MRB_ARGS_REQ(1));
}

void
mrb_mruby_array_ext_gem_final(mrb_state* mrb)
{
}
