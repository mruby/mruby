#include "mruby.h"
#include "mruby/value.h"
#include "mruby/array.h"
#include "mruby/range.h"
#include "mruby/hash.h"

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
mrb_ary_assoc(mrb_state *mrb, mrb_value ary)
{
  mrb_int i;
  mrb_value v, k;

  mrb_get_args(mrb, "o", &k);

  for (i = 0; i < RARRAY_LEN(ary); ++i) {
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
mrb_ary_rassoc(mrb_state *mrb, mrb_value ary)
{
  mrb_int i;
  mrb_value v, value;

  mrb_get_args(mrb, "o", &value);

  for (i = 0; i < RARRAY_LEN(ary); ++i) {
    v = RARRAY_PTR(ary)[i];
    if (mrb_type(v) == MRB_TT_ARRAY &&
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
mrb_ary_at(mrb_state *mrb, mrb_value ary)
{
  mrb_int pos;
  mrb_get_args(mrb, "i", &pos);

  return mrb_ary_entry(ary, pos);
}

static mrb_value
mrb_ary_values_at(mrb_state *mrb, mrb_value self)
{
  mrb_int argc;
  mrb_value *argv;

  mrb_get_args(mrb, "*", &argv, &argc);

  return mrb_get_values_at(mrb, self, RARRAY_LEN(self), argc, argv, mrb_ary_ref);
}

/*
 *  call-seq:
 *     ary.to_h   ->   Hash
 *
 *  Returns the result of interpreting <i>aray</i> as an array of
 *  <tt>[key, value]</tt> paris.
 *
 *     [[:foo, :bar], [1, 2]].to_h
 *       # => {:foo => :bar, 1 => 2}
 */

static mrb_value
mrb_ary_to_h(mrb_state *mrb, mrb_value ary)
{
  mrb_int i;
  mrb_value v, hash;

  hash = mrb_hash_new_capa(mrb, 0);

  for (i = 0; i < RARRAY_LEN(ary); ++i) {
    v = mrb_check_array_type(mrb, RARRAY_PTR(ary)[i]);

    if (mrb_nil_p(v)) {
      mrb_raisef(mrb, E_TYPE_ERROR, "wrong element type %S at %S (expected array)",
        mrb_str_new_cstr(mrb,  mrb_obj_classname(mrb, RARRAY_PTR(ary)[i])),
        mrb_fixnum_value(i)
      );
    }

    if (RARRAY_LEN(v) != 2) {
      mrb_raisef(mrb, E_ARGUMENT_ERROR, "wrong array length at %S (expected 2, was %S)",
        mrb_fixnum_value(i),
        mrb_fixnum_value(RARRAY_LEN(v))
      );
    }

    mrb_hash_set(mrb, hash, RARRAY_PTR(v)[0], RARRAY_PTR(v)[1]);
  }

  return hash;
}

/*
 *  call-seq:
 *     ary.bsearch {|x| block }  -> elem
 *
 *  By using binary search, finds a value from this array which meets
 *  the given condition in O(log n) where n is the size of the array.
 *
 *  You can use this method in two use cases: a find-minimum mode and
 *  a find-any mode.  In either case, the elements of the array must be
 *  monotone (or sorted) with respect to the block.
 *
 *  In find-minimum mode (this is a good choice for typical use case),
 *  the block must return true or false, and there must be an index i
 *  (0 <= i <= ary.size) so that:
 *
 *  - the block returns false for any element whose index is less than
 *    i, and
 *  - the block returns true for any element whose index is greater
 *    than or equal to i.
 *
 *  This method returns the i-th element.  If i is equal to ary.size,
 *  it returns nil.
 *
 *     ary = [0, 4, 7, 10, 12]
 *     ary.bsearch {|x| x >=   4 } #=> 4
 *     ary.bsearch {|x| x >=   6 } #=> 7
 *     ary.bsearch {|x| x >=  -1 } #=> 0
 *     ary.bsearch {|x| x >= 100 } #=> nil
 *
 *  In find-any mode (this behaves like libc's bsearch(3)), the block
 *  must return a number, and there must be two indices i and j
 *  (0 <= i <= j <= ary.size) so that:
 *
 *  - the block returns a positive number for ary[k] if 0 <= k < i,
 *  - the block returns zero for ary[k] if i <= k < j, and
 *  - the block returns a negative number for ary[k] if
 *    j <= k < ary.size.
 *
 *  Under this condition, this method returns any element whose index
 *  is within i...j.  If i is equal to j (i.e., there is no element
 *  that satisfies the block), this method returns nil.
 *
 *     ary = [0, 4, 7, 10, 12]
 *     # try to find v such that 4 <= v < 8
 *     ary.bsearch {|x| 1 - (x / 4).truncate } #=> 4 or 7
 *     # try to find v such that 8 <= v < 10
 *     ary.bsearch {|x| 4 - (x / 2).truncate } #=> nil
 *
 *  You must not mix the two modes at a time; the block must always
 *  return either true/false, or always return a number.  It is
 *  undefined which value is actually picked up at each iteration.
 */

static mrb_value
mrb_ary_bsearch(mrb_state *mrb, mrb_value ary)
{
  struct RArray *a = mrb_ary_ptr(ary);
  mrb_int low = 0;
  mrb_int high = a->len;
  mrb_int mid = 0;
  mrb_bool smaller = FALSE;
  mrb_bool satisfied = FALSE;
  mrb_value v;
  mrb_value val;
  mrb_value blk;

  mrb_get_args(mrb, "&", &blk);

  if (mrb_nil_p(blk)) {
    return mrb_funcall(mrb, ary, "to_enum", 1, mrb_symbol_value(mrb_intern_lit(mrb, "bsearch")));
  }

  while (low < high) {
    mid = low + ((high - low) / 2);
    val = mrb_ary_entry(ary, mid);
    v = mrb_yield_argv(mrb, blk, 1, &val);
    if (mrb_fixnum_p(v)) {
      if (mrb_fixnum(v) == 0) return val;
      smaller = mrb_fixnum(v) < 0;
    }
    else if (mrb_type(v) == MRB_TT_TRUE) {
      satisfied = TRUE;
      smaller = TRUE;
    }
    else if (mrb_type(v) == MRB_TT_FALSE) {
      smaller = FALSE;
    }
    if (smaller) {
      high = mid;
    }
    else {
      low = mid + 1;
    }
  }

  if (low == a->len) return mrb_nil_value();
  if (!satisfied) return mrb_nil_value();
  return mrb_ary_entry(ary, low);
}

void
mrb_mruby_array_ext_gem_init(mrb_state* mrb)
{
  struct RClass * a = mrb->array_class;

  mrb_define_method(mrb, a, "assoc",  mrb_ary_assoc,  MRB_ARGS_REQ(1));
  mrb_define_method(mrb, a, "at",     mrb_ary_at,     MRB_ARGS_REQ(1));
  mrb_define_method(mrb, a, "rassoc", mrb_ary_rassoc, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, a, "values_at", mrb_ary_values_at, MRB_ARGS_ANY());
  mrb_define_method(mrb, a, "to_h",   mrb_ary_to_h, MRB_ARGS_REQ(0));
  mrb_define_method(mrb, a, "bsearch", mrb_ary_bsearch, MRB_ARGS_REQ(1));
}

void
mrb_mruby_array_ext_gem_final(mrb_state* mrb)
{
}
