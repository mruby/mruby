#include <mruby.h>
#include <mruby/value.h>
#include <mruby/array.h>
#include <mruby/range.h>
#include <mruby/hash.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

/*
 *  call-seq:
 *     ary.assoc(obj)   -> new_ary  or  nil
 *
 *  Searches through an array whose elements are also arrays
 *  comparing _obj_ with the first element of each contained array
 *  using obj.==.
 *  Returns the first contained array that matches (that
 *  is, the first associated array),
 *  or +nil+ if no match is found.
 *  See also <code>Array#rassoc</code>.
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
  mrb_int i;
  mrb_value v;
  mrb_value k = mrb_get_arg1(mrb);

  for (i = 0; i < RARRAY_LEN(ary); i++) {
    v = mrb_check_array_type(mrb, RARRAY_PTR(ary)[i]);
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
 *  <code>==</code>. Returns the first contained array that matches. See
 *  also <code>Array#assoc</code>.
 *
 *     a = [ [ 1, "one"], [2, "two"], [3, "three"], ["ii", "two"] ]
 *     a.rassoc("two")    #=> [2, "two"]
 *     a.rassoc("four")   #=> nil
 */

static mrb_value
ary_rassoc(mrb_state *mrb, mrb_value ary)
{
  mrb_int i;
  mrb_value v;
  mrb_value value = mrb_get_arg1(mrb);

  for (i = 0; i < RARRAY_LEN(ary); i++) {
    v = RARRAY_PTR(ary)[i];
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
 *  negative index counts from the end of +self+.  Returns +nil+
 *  if the index is out of range. See also <code>Array#[]</code>.
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

static mrb_value
ary_ref(mrb_state *mrb, mrb_value ary, mrb_int n)
{
  return mrb_ary_entry(ary, n);
}

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
 *  Deletes the element(s) given by an +index+ (optionally up to +length+
 *  elements) or by a +range+.
 *
 *  Returns the deleted object (or objects), or +nil+ if the +index+ is out of
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
  mrb_int i, j, len, alen;
  mrb_value *ptr;
  mrb_value ary;

  mrb_ary_modify(mrb, a);

  if (mrb_get_argc(mrb) == 1) {
    mrb_value index = mrb_get_arg1(mrb);

    if (mrb_type(index) == MRB_TT_RANGE) {
      if (mrb_range_beg_len(mrb, index, &i, &len, ARY_LEN(a), TRUE) == MRB_RANGE_OK) {
        goto delete_pos_len;
      }
      return mrb_nil_value();
    }
    return mrb_ary_delete_at(mrb, self);
  }

  mrb_get_args(mrb, "ii", &i, &len);
 delete_pos_len:
  alen = ARY_LEN(a);
  if (i < 0) i += alen;
  if (i < 0 || alen < i) return mrb_nil_value();
  if (len < 0) return mrb_nil_value();
  if (alen == i) return mrb_ary_new(mrb);
  if (len > alen - i) len = alen - i;

  ptr = ARY_PTR(a) + i;
  ary = mrb_ary_new_from_values(mrb, len, ptr);

  for (j = i; j < alen - len; j++) {
    *ptr = *(ptr+len);
    ptr++;
  }

  mrb_ary_resize(mrb, self, alen - len);
  return ary;
}

/*
 * call-seq:
 *    ary.compact     -> new_ary
 *
 * Returns a copy of +self+ with all +nil+ elements removed.
 *
 *   [ "a", nil, "b", nil, "c", nil ].compact
 *                      #=> [ "a", "b", "c" ]
 */

static mrb_value
ary_compact(mrb_state *mrb, mrb_value self)
{
  mrb_value ary = mrb_ary_new(mrb);
  mrb_int len = RARRAY_LEN(self);
  mrb_value *p = RARRAY_PTR(self);

  for (mrb_int i = 0; i < len; i++) {
    if (!mrb_nil_p(p[i])) {
      mrb_ary_push(mrb, ary, p[i]);
    }
  }
  return ary;
}

/*
 * call-seq:
 *    ary.compact!    -> ary  or  nil
 *
 * Removes +nil+ elements from the array.
 * Returns +nil+ if no changes were made, otherwise returns
 * <i>ary</i>.
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
  mrb_value *p = ARY_PTR(a);
  for (i = 0; i < len; i++) {
    if (!mrb_nil_p(p[i])) {
      if (i != j) p[j] = p[i];
      j++;
    }
  }
  if (i == j) return mrb_nil_value();
  ARY_SET_LEN(RARRAY(self), j);
  return self;
}


/*
 *  call-seq:
 *     ary.rotate(count=1)    -> new_ary
 *
 *  Returns a new array by rotating +self+ so that the element at +count+ is
 *  the first element of the new array.
 *
 *  If +count+ is negative then it rotates in the opposite direction, starting
 *  from the end of +self+ where +-1+ is the last element.
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
  mrb_value *p = RARRAY_PTR(self);
  mrb_int idx;

  if (len <= 0) return ary;
  if (count < 0) {
    idx = len - (~count % len) - 1;
  }
  else {
    idx = count % len;
  }
  for (mrb_int i = 0; i<len; i++) {
    mrb_ary_push(mrb, ary, p[idx++]);
    if (idx == len) idx = 0;
  }
  return ary;
}

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
 *  Rotates +self+ in place so that the element at +count+ comes first, and
 *  returns +self+.
 *
 *  If +count+ is negative then it rotates in the opposite direction, starting
 *  from the end of the array where +-1+ is the last element.
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

static mrb_value
ary_subtract_internal(mrb_state *mrb, mrb_value self, mrb_int other_argc, const mrb_value *other_argv)
{
  mrb_value result_ary;
  struct RArray *self_ary;
  mrb_value *p, *p_end;
  mrb_int total_other_len = 0;

  if (other_argc == 0) {
    return mrb_ary_new_from_values(mrb, RARRAY_LEN(self), RARRAY_PTR(self));
  }

  for (mrb_int i = 0; i < other_argc; i++) {
    mrb_value other = mrb_check_array_type(mrb, other_argv[i]);
    if (mrb_nil_p(other)) {
      mrb_raise(mrb, E_TYPE_ERROR, "can't convert passed argument to Array");
    }
    total_other_len += RARRAY_LEN(other);
  }

  self_ary = mrb_ary_ptr(self);
  p = ARY_PTR(self_ary);
  p_end = p + ARY_LEN(self_ary);
  result_ary = mrb_ary_new(mrb);

  if (total_other_len > SET_OP_HASH_THRESHOLD) {
    mrb_value hash = mrb_hash_new_capa(mrb, total_other_len);

    for (mrb_int i = 0; i < other_argc; i++) {
      struct RArray *other_ary = mrb_ary_ptr(other_argv[i]);
      mrb_value *other_p = ARY_PTR(other_ary);
      mrb_value *other_p_end = other_p + ARY_LEN(other_ary);
      while (other_p < other_p_end) {
        mrb_hash_set(mrb, hash, *other_p, mrb_true_value());
        other_p++;
      }
    }

    while (p < p_end) {
      mrb_value val = mrb_hash_get(mrb, hash, *p);
      if (mrb_nil_p(val)) {  /* key doesn't exist in any other_ary */
        mrb_ary_push(mrb, result_ary, *p);
      }
      p++;
    }
  }
  else {
    while (p < p_end) {
      mrb_bool found = FALSE;
      for (mrb_int i = 0; i < other_argc; i++) {
        struct RArray *other_ary = mrb_ary_ptr(other_argv[i]);
        mrb_value *other_p = ARY_PTR(other_ary);
        mrb_value *other_p_end = other_p + ARY_LEN(other_ary);
        while (other_p < other_p_end) {
          if (mrb_equal(mrb, *p, *other_p)) {
            found = TRUE;
            break;
          }
          other_p++;
        }
        if (found) break;
      }
      if (!found) {
        mrb_ary_push(mrb, result_ary, *p);
      }
      p++;
    }
  }

  return result_ary;
}

/*
 *  call-seq:
 *     ary - other_ary   -> new_ary
 *
 *  Returns a new array that is a copy of the original array, with any items
 *  that also appear in +other_ary+ removed.
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
 *  occurrences of any item that also appear in any of the +other_ary+s.
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

/*
 *  call-seq:
 *     ary | other_ary     -> new_ary
 *
 *  Set Union---Returns a new array by joining this array with
 *  <i>other_ary</i>, removing duplicates.
 *
 *     [ "a", "b", "c" ] | [ "c", "d", "a" ]
 *           #=> [ "a", "b", "c", "d" ]
 */

static mrb_value
ary_union_internal(mrb_state *mrb, mrb_value self, mrb_int other_argc, const mrb_value *other_argv)
{
  mrb_value result_ary;
  mrb_int total_len = RARRAY_LEN(self);

  for (mrb_int i = 0; i < other_argc; i++) {
    mrb_value other = mrb_check_array_type(mrb, other_argv[i]);
    if (mrb_nil_p(other)) {
      mrb_raise(mrb, E_TYPE_ERROR, "can't convert passed argument to Array");
    }
    total_len += RARRAY_LEN(other);
  }

  result_ary = mrb_ary_new(mrb);

  if (total_len > SET_OP_HASH_THRESHOLD) {
    mrb_value hash = mrb_hash_new_capa(mrb, total_len);

    /* Add elements from self */
    struct RArray *self_ary = mrb_ary_ptr(self);
    mrb_value *p = ARY_PTR(self_ary);
    mrb_value *p_end = p + ARY_LEN(self_ary);
    while (p < p_end) {
      mrb_value val = mrb_hash_get(mrb, hash, *p);
      if (mrb_nil_p(val)) {  /* key doesn't exist */
        mrb_hash_set(mrb, hash, *p, mrb_true_value());
        mrb_ary_push(mrb, result_ary, *p);
      }
      p++;
    }

    /* Add elements from others */
    for (mrb_int i = 0; i < other_argc; i++) {
      struct RArray *other_ary = mrb_ary_ptr(other_argv[i]);
      mrb_value *other_p = ARY_PTR(other_ary);
      mrb_value *other_p_end = other_p + ARY_LEN(other_ary);
      while (other_p < other_p_end) {
        mrb_value val = mrb_hash_get(mrb, hash, *other_p);
        if (mrb_nil_p(val)) {  /* key doesn't exist */
          mrb_hash_set(mrb, hash, *other_p, mrb_true_value());
          mrb_ary_push(mrb, result_ary, *other_p);
        }
        other_p++;
      }
    }
  }
  else {
    /* Use linear search for small arrays */
    /* Add unique elements from self */
    struct RArray *self_ary = mrb_ary_ptr(self);
    mrb_value *p = ARY_PTR(self_ary);
    mrb_value *p_end = p + ARY_LEN(self_ary);
    while (p < p_end) {
        mrb_bool found = FALSE;
        mrb_int result_len = RARRAY_LEN(result_ary);
        mrb_value *result_ptr = ARY_PTR(RARRAY(result_ary));
        for (mrb_int j = 0; j < result_len; j++) {
            if (mrb_equal(mrb, *p, result_ptr[j])) {
                found = TRUE;
                break;
            }
        }
        if (!found) {
            mrb_ary_push(mrb, result_ary, *p);
        }
        p++;
    }

    /* Add unique elements from others */
    for (mrb_int i = 0; i < other_argc; i++) {
      mrb_value other = other_argv[i];
      mrb_value *other_p = ARY_PTR(RARRAY(other));
      mrb_value *other_p_end = other_p + ARY_LEN(RARRAY(other));
      while (other_p < other_p_end) {
        mrb_bool found = FALSE;
        mrb_int result_len = RARRAY_LEN(result_ary);
        mrb_value *result_ptr = ARY_PTR(RARRAY(result_ary));
        for (mrb_int j = 0; j < result_len; j++) {
          if (mrb_equal(mrb, *other_p, result_ptr[j])) {
            found = TRUE;
            break;
          }
        }
        if (!found) {
          mrb_ary_push(mrb, result_ary, *other_p);
        }
        other_p++;
      }
    }
  }

  return result_ary;
}

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
 *  <i>other_ary</i>s, removing duplicates.
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
  mrb_value other, result_ary;
  struct RArray *self_ary, *other_ary;
  mrb_value *p, *p_end, *other_p, *other_p_end;

  mrb_get_args(mrb, "A", &other);

  self_ary = mrb_ary_ptr(self);
  other_ary = mrb_ary_ptr(other);

  result_ary = mrb_ary_new(mrb);

  if (ARY_LEN(other_ary) > SET_OP_HASH_THRESHOLD) {
    /* Use hash for large arrays to achieve O(n) performance */
    mrb_value hash = mrb_hash_new_capa(mrb, ARY_LEN(other_ary));

    /* Populate hash with elements from other_ary */
    other_p = ARY_PTR(other_ary);
    other_p_end = other_p + ARY_LEN(other_ary);
    while (other_p < other_p_end) {
      mrb_hash_set(mrb, hash, *other_p, mrb_true_value());
      other_p++;
    }

    /* Check elements from self against hash */
    p = ARY_PTR(self_ary);
    p_end = p + ARY_LEN(self_ary);
    while (p < p_end) {
      mrb_value val = mrb_hash_get(mrb, hash, *p);
      if (!mrb_nil_p(val)) {  /* key exists in other_ary */
        mrb_ary_push(mrb, result_ary, *p);
        mrb_hash_delete_key(mrb, hash, *p);  /* remove to ensure uniqueness */
      }
      p++;
    }
  }
  else {
    /* Use linear search for small arrays */
    p = ARY_PTR(self_ary);
    p_end = p + ARY_LEN(self_ary);
    while (p < p_end) {
      /* Check if element exists in other_ary */
      other_p = ARY_PTR(other_ary);
      other_p_end = other_p + ARY_LEN(other_ary);
      mrb_bool found = FALSE;

      while (other_p < other_p_end) {
        if (mrb_equal(mrb, *p, *other_p)) {
          found = TRUE;
          break;
        }
        other_p++;
      }

      if (found) {
        /* Check if already in result to ensure uniqueness */
        mrb_int result_len = RARRAY_LEN(result_ary);
        mrb_value *result_ptr = RARRAY_PTR(result_ary);
        mrb_bool already_added = FALSE;

        for (mrb_int i = 0; i < result_len; i++) {
          if (mrb_equal(mrb, *p, result_ptr[i])) {
            already_added = TRUE;
            break;
          }
        }

        if (!already_added) {
          mrb_ary_push(mrb, result_ary, *p);
        }
      }
      p++;
    }
  }

  return result_ary;
}

/*
 *  call-seq:
 *    ary.intersect?(other_ary)   -> true or false
 *
 *  Returns +true+ if the array and +other_ary+ have at least one element in
 *  common, otherwise returns +false+.
 *
 *     a = [ 1, 2, 3 ]
 *     b = [ 3, 4, 5 ]
 *     c = [ 5, 6, 7 ]
 *     a.intersect?(b)   #=> true
 *     a.intersect?(c)   #=> false
 */

static mrb_value
ary_intersect_p(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  struct RArray *self_ary, *other_ary, *shorter_ary, *longer_ary;
  mrb_value *shorter_p, *shorter_p_end, *longer_p, *longer_p_end;

  mrb_get_args(mrb, "A", &other);

  self_ary = mrb_ary_ptr(self);
  other_ary = mrb_ary_ptr(other);

  /* Choose shorter array for hash, longer for iteration (optimization) */
  if (ARY_LEN(self_ary) > ARY_LEN(other_ary)) {
    shorter_ary = other_ary;
    longer_ary = self_ary;
  }
  else {
    shorter_ary = self_ary;
    longer_ary = other_ary;
  }

  /* Early termination for empty arrays */
  if (ARY_LEN(shorter_ary) == 0 || ARY_LEN(longer_ary) == 0) {
    return mrb_false_value();
  }

  if (ARY_LEN(shorter_ary) > SET_OP_HASH_THRESHOLD) {
    /* Use hash for large arrays to achieve O(n) performance */
    mrb_value hash = mrb_hash_new_capa(mrb, ARY_LEN(shorter_ary));

    /* Populate hash with elements from shorter array */
    shorter_p = ARY_PTR(shorter_ary);
    shorter_p_end = shorter_p + ARY_LEN(shorter_ary);
    while (shorter_p < shorter_p_end) {
      mrb_hash_set(mrb, hash, *shorter_p, mrb_true_value());
      shorter_p++;
    }

    /* Check elements from longer array against hash with early termination */
    longer_p = ARY_PTR(longer_ary);
    longer_p_end = longer_p + ARY_LEN(longer_ary);
    while (longer_p < longer_p_end) {
      mrb_value val = mrb_hash_get(mrb, hash, *longer_p);
      if (!mrb_nil_p(val)) {  /* key exists in shorter array */
        return mrb_true_value();  /* Early termination */
      }
      longer_p++;
    }
  }
  else {
    /* Use linear search for small arrays */
    longer_p = ARY_PTR(longer_ary);
    longer_p_end = longer_p + ARY_LEN(longer_ary);
    while (longer_p < longer_p_end) {
      /* Check if element exists in shorter array */
      shorter_p = ARY_PTR(shorter_ary);
      shorter_p_end = shorter_p + ARY_LEN(shorter_ary);

      while (shorter_p < shorter_p_end) {
        if (mrb_equal(mrb, *longer_p, *shorter_p)) {
          return mrb_true_value();  /* Early termination */
        }
        shorter_p++;
      }
      longer_p++;
    }
  }

  return mrb_false_value();
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
  mrb_define_method_id(mrb, a, MRB_SYM_Q(intersect), ary_intersect_p, MRB_ARGS_REQ(1));
}

void
mrb_mruby_array_ext_gem_final(mrb_state* mrb)
{
}
