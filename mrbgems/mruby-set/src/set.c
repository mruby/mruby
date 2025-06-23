/*
** set.c - Set class
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/hash.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/internal.h>
#include <mruby/presym.h>
#include <mruby/khash.h>

KHASH_DECLARE(set, mrb_value, char, FALSE)
KHASH_DEFINE(set, mrb_value, char, FALSE, mrb_obj_hash_code, mrb_eql)

static void
set_copy_elements(mrb_state *mrb, khash_t(set) *target_kh, khash_t(set) *source_kh)
{
  if (!source_kh || !target_kh) return;

  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, source_kh, k) {
    kh_put(set, mrb, target_kh, kh_key(source_kh, k));
    mrb_gc_arena_restore(mrb, ai);
  }
}

#define SET_KHASH_IV MRB_SYM(khash)

static void
set_free(mrb_state *mrb, void *ptr)
{
  khash_t(set) *kh = (khash_t(set)*)ptr;
  if (kh) {
    kh_destroy(set, mrb, kh);
  }
}

static const struct mrb_data_type set_data_type = {
  "Set", set_free
};

static void
set_set_khash(mrb_state *mrb, mrb_value self, khash_t(set) *kh)
{
  mrb_data_init(self, kh, &set_data_type);
}

static khash_t(set) *
set_get_khash(mrb_state *mrb, mrb_value self)
{
  return (khash_t(set)*)mrb_data_get_ptr(mrb, self, &set_data_type);
}

/* Helper function to check if a value is a Set and return a boolean result */
static mrb_bool
set_is_set(mrb_state *mrb, mrb_value obj)
{
  return mrb_obj_is_kind_of(mrb, obj, mrb_class_get(mrb, "Set"));
}

/* Helper function to check if a value is a Set and raise an error if not */
static void
set_check_type(mrb_state *mrb, mrb_value obj)
{
  if (!set_is_set(mrb, obj)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "value must be a set");
  }
}

static mrb_value
set_init(mrb_state *mrb, mrb_value self)
{
  khash_t(set) *kh = kh_init(set, mrb);
  set_set_khash(mrb, self, kh);
  return self;
}

/*
 * call-seq:
 *   set.initialize_copy(orig)
  * Copy constructor.
 */
static mrb_value
set_init_copy(mrb_state *mrb, mrb_value self)
{
  mrb_value orig = mrb_get_arg1(mrb);
  khash_t(set) *kh;

  if (mrb_type(orig) != MRB_TT_CDATA || (DATA_TYPE(self) && DATA_TYPE(self) != DATA_TYPE(orig))) {
    mrb_raise(mrb, E_TYPE_ERROR, "initialize_copy should take same class object");
  }

  kh = set_get_khash(mrb, orig);
  if (!kh) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid Set object");
  }

  kh = kh_copy(set, mrb, kh);
  set_set_khash(mrb, self, kh);

  return self;
}

/*
 * call-seq:
 *   set.size -> integer
 *   set.length -> integer
 *
 * Returns the number of elements.
 */
static mrb_value
set_size(mrb_state *mrb, mrb_value self)
{
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) return mrb_fixnum_value(0);
  return mrb_fixnum_value(kh_size(kh));
}

/*
 * call-seq:
 *   set.empty? -> true or false
 *
 * Returns true if the set contains no elements.
 */
static mrb_value
set_empty_p(mrb_state *mrb, mrb_value self)
{
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) return mrb_true_value();
  return mrb_bool_value(kh_size(kh) == 0);
}

/*
 * call-seq:
 *   set.clear -> self
 *
 * Removes all elements and returns self.
 */
static mrb_value
set_clear(mrb_state *mrb, mrb_value self)
{
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (kh) {
    kh_clear(set, mrb, kh);
  }
  return self;
}

/*
 * call-seq:
 *   set.to_a -> array
 *
 * Converts the set to an array.
 */
static mrb_value
set_to_a(mrb_state *mrb, mrb_value self)
{
  khash_t(set) *kh = set_get_khash(mrb, self);

  if (!kh) return mrb_ary_new(mrb);

  mrb_value ary = mrb_ary_new_capa(mrb, kh_size(kh));

  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, kh, k) {
    mrb_ary_push(mrb, ary, kh_key(kh, k));
    mrb_gc_arena_restore(mrb, ai);
  }

  return ary;
}

/*
 * call-seq:
 *   set.include?(object) -> true or false
 *   set.member?(object) -> true or false
 *   set === object -> true or false
 *
 * Returns true if the set contains the given object.
 */
static mrb_value
set_include_p(mrb_state *mrb, mrb_value self)
{
  mrb_value obj = mrb_get_arg1(mrb);
  khash_t(set) *kh;
  khiter_t k;
  kh = set_get_khash(mrb, self);
  if (!kh) return mrb_false_value();

  k = kh_get(set, mrb, kh, obj);
  return mrb_bool_value(k != kh_end(kh));
}

/*
 * call-seq:
 *   set.add(object) -> self
 *   set << object -> self
 *
 * Adds the given object to the set and returns self.
 */
static mrb_value
set_add(mrb_state *mrb, mrb_value self)
{
  mrb_value obj = mrb_get_arg1(mrb);
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Set");
  }

  kh_put(set, mrb, kh, obj);
  return self;
}

/*
 * call-seq:
 *   set.add?(object) -> self or nil
 *
 * Adds the given object to the set and returns self. If the object is already
 * in the set, returns nil.
 */
static mrb_value
set_add_p(mrb_state *mrb, mrb_value self)
{
  mrb_value obj = mrb_get_arg1(mrb);
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Set");
  }

  int ret;
  kh_put2(set, mrb, kh, obj, &ret);
  if (ret == 0) {
    /* Key already exists */
    return mrb_nil_value();
  }
  else {
    /* Key was added */
    return self;
  }
}

/*
 * call-seq:
 *   set.delete(object) -> self
 *
 * Deletes the given object from the set and returns self.
 */
static mrb_value
set_delete(mrb_state *mrb, mrb_value self)
{
  mrb_value obj = mrb_get_arg1(mrb);
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) return self;

  khiter_t k = kh_get(set, mrb, kh, obj);
  if (k != kh_end(kh)) {
    kh_del(set, mrb, kh, k);
  }
  return self;
}

/*
 * call-seq:
 *   set.delete?(object) -> self or nil
 *
 * Deletes the given object from the set and returns self. If the object is not
 * in the set, returns nil.
 */
static mrb_value
set_delete_p(mrb_state *mrb, mrb_value self)
{
  mrb_value obj = mrb_get_arg1(mrb);
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) return mrb_nil_value();

  khiter_t k = kh_get(set, mrb, kh, obj);
  if (k != kh_end(kh)) {
    kh_del(set, mrb, kh, k);
    return self;
  }
  else {
    return mrb_nil_value();
  }
}

/*
 * Core implementation of Set-to-Set merge (mutating version)
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_merge(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  if (!self_kh) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Set");
  }

  khash_t(set) *other_kh = set_get_khash(mrb, other);
  if (!other_kh) return self;

  /* Add all elements from other set */
  set_copy_elements(mrb, self_kh, other_kh);

  return self;
}

/*
 * Core implementation of Set-to-Set subtraction (mutating version)
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_subtract(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  if (!self_kh) return self;

  khash_t(set) *other_kh = set_get_khash(mrb, other);
  if (!other_kh) return self;

  /* Remove all elements that are in other set */
  KHASH_FOREACH(mrb, other_kh, k) {
    mrb_value key = kh_key(other_kh, k);
    khiter_t self_k = kh_get(set, mrb, self_kh, key);
    if (self_k != kh_end(self_kh)) {
      kh_del(set, mrb, self_kh, self_k);
    }
  }

  return self;
}

/*
 * Core implementation of Set-to-Set union
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_union(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Create a new set by duplicating self */
  mrb_value result_set = mrb_obj_dup(mrb, self);
  khash_t(set) *result_kh = set_get_khash(mrb, result_set);
  if (!result_kh) {
    /* If self is empty, create a new empty set */
    result_kh = kh_init(set, mrb);
    set_set_khash(mrb, result_set, result_kh);
  }

  /* Add all elements from other set */
  khash_t(set) *other_kh = set_get_khash(mrb, other);
  set_copy_elements(mrb, result_kh, other_kh);

  return result_set;
}

/*
 * Core implementation of Set-to-Set difference
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_difference(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Create a new set by duplicating self */
  mrb_value result_set = mrb_obj_dup(mrb, self);
  khash_t(set) *result_kh = set_get_khash(mrb, result_set);
  if (!result_kh) {
    /* If self is empty, return an empty set */
    return result_set;
  }

  /* Remove all elements that are in other set */
  khash_t(set) *other_kh = set_get_khash(mrb, other);
  if (other_kh) {
    KHASH_FOREACH(mrb, other_kh, k) {
      mrb_value key = kh_key(other_kh, k);
      khiter_t result_k = kh_get(set, mrb, result_kh, key);
      if (result_k != kh_end(result_kh)) {
        kh_del(set, mrb, result_kh, result_k);
      }
    }
  }

  return result_set;
}


/*
 * Core implementation of Set-to-Set intersection
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_intersection(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Create a new empty set of the same class as self */
  mrb_value result_set = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  khash_t(set) *result_kh = set_get_khash(mrb, result_set);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  if (!self_kh) return result_set;

  khash_t(set) *other_kh = set_get_khash(mrb, other);
  if (!other_kh) return result_set;

  KHASH_FOREACH(mrb, other_kh, k) {
    mrb_value key = kh_key(other_kh, k);
    khiter_t self_k = kh_get(set, mrb, self_kh, key);

    /* If key exists in self, add it to result */
    if (self_k != kh_end(self_kh)) {
      kh_put(set, mrb, result_kh, key);
    }
  }

  return result_set;
}


/*
 * Core implementation of Set-to-Set XOR (symmetric difference)
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_xor(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Create a new empty set of the same class as self */
  mrb_value result_set = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  khash_t(set) *result_kh = set_get_khash(mrb, result_set);
  khash_t(set) *self_kh = set_get_khash(mrb, self);
  khash_t(set) *other_kh = set_get_khash(mrb, other);

  if (!self_kh || kh_size(self_kh) == 0) {
    /* If self is empty, return a copy of other */
    if (other_kh) {
      set_copy_elements(mrb, result_kh, other_kh);
    }
    return result_set;
  }

  if (!other_kh || kh_size(other_kh) == 0) {
    /* If other is empty, return a copy of self */
    set_copy_elements(mrb, result_kh, self_kh);
    return result_set;
  }

  /* Add elements from self that are not in other */
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, self_kh, k) {
    mrb_value key = kh_key(self_kh, k);
    khiter_t other_k = kh_get(set, mrb, other_kh, key);

    /* Add to result if not in other */
    if (other_k == kh_end(other_kh)) {
      kh_put(set, mrb, result_kh, key);
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  /* Add elements from other that are not in self */
  KHASH_FOREACH(mrb, other_kh, k) {
    mrb_value key = kh_key(other_kh, k);
    khiter_t self_k = kh_get(set, mrb, self_kh, key);

    /* Add to result if not in self */
    if (self_k == kh_end(self_kh)) {
      kh_put(set, mrb, result_kh, key);
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return result_set;
}

/*
 * call-seq:
 *   set == other -> true or false
 *
 * Returns true if two sets are equal.
 */
static mrb_value
set_equal(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Fast path: same object */
  if (mrb_obj_equal(mrb, self, other)) {
    return mrb_true_value();
  }

  /* Only compare with other Set objects */
  if (!set_is_set(mrb, other)) {
    return mrb_false_value();
  }

  khash_t(set) *kh1 = set_get_khash(mrb, self);
  khash_t(set) *kh2 = set_get_khash(mrb, other);

  /* Fast path: both empty */
  if ((!kh1 || kh_size(kh1) == 0) && (!kh2 || kh_size(kh2) == 0)) {
    return mrb_true_value();
  }

  /* Fast path: different sizes */
  if (!kh1 || !kh2 || kh_size(kh1) != kh_size(kh2)) {
    return mrb_false_value();
  }

  /* Compare elements: iterate through the smaller hash for efficiency */
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, kh1, k) {
    khiter_t k2 = kh_get(set, mrb, kh2, kh_key(kh1, k));
    if (k2 == kh_end(kh2)) {
      mrb_gc_arena_restore(mrb, ai);
      return mrb_false_value(); /* Element in self not found in other */
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return mrb_true_value();
}


/*
 * call-seq:
 *   set.hash -> integer
 *
 * Compute a hash-code for this set.
 * Uses an improved hash algorithm for better distribution.
 */
static mrb_value
set_hash_m(mrb_state *mrb, mrb_value self)
{
  khash_t(set) *kh = set_get_khash(mrb, self);

  /* Use FNV-1a hash algorithm with better distribution properties */
  uint64_t hash = 0xcbf29ce484222325ULL; /* FNV offset basis */
  const uint64_t fnv_prime = 0x100000001b3ULL; /* FNV prime */

  /* Include the size of the set in the hash */
  size_t size = kh ? kh_size(kh) : 0;
  hash ^= size;
  hash *= fnv_prime;

  if (kh && size > 0) {
    /* Process each element */
    int ai = mrb_gc_arena_save(mrb);
    KHASH_FOREACH(mrb, kh, k) {
      /* Get element's hash code */
      khint_t elem_hash = (khint_t)mrb_obj_hash_code(mrb, kh_key(kh, k));

      /* Mix using FNV-1a algorithm */
      hash ^= elem_hash;
      hash *= fnv_prime;

      mrb_gc_arena_restore(mrb, ai);
    }
  }

  /* Final mixing to improve avalanche effect */
  hash ^= hash >> 32;

  return mrb_fixnum_value((mrb_int)hash);
}

/*
 * call-seq:
 *   set.eql?(other) -> true or false
 *
 * Returns true if the set and the given object are members of the same class
 * and their members are eql? to each other.
 */
static mrb_value
set_eql(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Fast path: same object */
  if (mrb_obj_equal(mrb, self, other)) {
    return mrb_true_value();
  }

  /* Only compare with other Set objects */
  if (!set_is_set(mrb, other)) {
    return mrb_false_value();
  }

  khash_t(set) *kh1 = set_get_khash(mrb, self);
  khash_t(set) *kh2 = set_get_khash(mrb, other);

  /* Fast path: both empty */
  if ((!kh1 || kh_size(kh1) == 0) && (!kh2 || kh_size(kh2) == 0)) {
    return mrb_true_value();
  }

  /* Fast path: different sizes */
  if (!kh1 || !kh2 || kh_size(kh1) != kh_size(kh2)) {
    return mrb_false_value();
  }

  /* Compare elements */
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, kh1, k) {
    khiter_t other_k = kh_get(set, mrb, kh2, kh_key(kh1, k));
    if (other_k == kh_end(kh2)) {
      return mrb_false_value(); /* Element in self not found in other */
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return mrb_true_value();
}

/*
 * call-seq:
 *   set.superset?(other) -> true or false
 *   set >= other -> true or false
 *
 * Returns true if the set is a superset of the given set.
 */
static mrb_value
set_superset_p(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Check if other is a Set */
  set_check_type(mrb, other);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  khash_t(set) *other_kh = set_get_khash(mrb, other);

  /* Handle empty sets */
  if (!other_kh || kh_size(other_kh) == 0) {
    return mrb_true_value(); /* Empty set is a subset of any set */
  }

  if (!self_kh) {
    return mrb_false_value(); /* Empty set is not a superset of a non-empty set */
  }

  /* Check size first - a superset must be at least as large as the subset */
  if (kh_size(self_kh) < kh_size(other_kh)) {
    return mrb_false_value();
  }

  /* Check if all elements in other are in self */
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, other_kh, k) {
    khiter_t self_k = kh_get(set, mrb, self_kh, kh_key(other_kh, k));
    if (self_k == kh_end(self_kh)) {
      return mrb_false_value(); /* Element in other not found in self */
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return mrb_true_value();
}

/*
 * call-seq:
 *   set.proper_superset?(other) -> true or false
 *   set > other -> true or false
 *
 * Returns true if the set is a proper superset of the given set.
 */
static mrb_value
set_proper_superset_p(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Check if other is a Set */
  set_check_type(mrb, other);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  khash_t(set) *other_kh = set_get_khash(mrb, other);

  /* Handle empty sets */
  if (!other_kh || kh_size(other_kh) == 0) {
    /* Empty set is a proper subset of any non-empty set */
    return self_kh && kh_size(self_kh) > 0 ? mrb_true_value() : mrb_false_value();
  }

  if (!self_kh) {
    return mrb_false_value(); /* Empty set is not a proper superset of any set */
  }

  /* For a proper superset, self must be strictly larger than other */
  if (kh_size(self_kh) <= kh_size(other_kh)) {
    return mrb_false_value();
  }

  /* Check if all elements in other are in self */
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, other_kh, k) {
    khiter_t self_k = kh_get(set, mrb, self_kh, kh_key(other_kh, k));
    if (self_k == kh_end(self_kh)) {
      return mrb_false_value(); /* Element in other not found in self */
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return mrb_true_value();
}

/*
 * call-seq:
 *   set.subset?(other) -> true or false
 *   set <= other -> true or false
 *
 * Returns true if the set is a subset of the given set.
 */
static mrb_value
set_subset_p(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Check if other is a Set */
  set_check_type(mrb, other);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  khash_t(set) *other_kh = set_get_khash(mrb, other);

  /* Handle empty sets */
  if (!self_kh || kh_size(self_kh) == 0) {
    return mrb_true_value(); /* Empty set is a subset of any set */
  }

  if (!other_kh) {
    return mrb_false_value(); /* Non-empty set is not a subset of an empty set */
  }

  /* Check size first - a subset cannot be larger than its superset */
  if (kh_size(other_kh) < kh_size(self_kh)) {
    return mrb_false_value();
  }

  /* Check if all elements in self are in other */
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, self_kh, k) {
    khiter_t other_k = kh_get(set, mrb, other_kh, kh_key(self_kh, k));
    if (other_k == kh_end(other_kh)) {
      return mrb_false_value(); /* Element in self not found in other */
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return mrb_true_value();
}

/*
 * call-seq:
 *   set.proper_subset?(other) -> true or false
 *   set < other -> true or false
 *
 * Returns true if the set is a proper subset of the given set.
 */
static mrb_value
set_proper_subset_p(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Check if other is a Set */
  set_check_type(mrb, other);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  khash_t(set) *other_kh = set_get_khash(mrb, other);

  /* Handle empty sets */
  if (!self_kh || kh_size(self_kh) == 0) {
    /* Empty set is a proper subset of any non-empty set */
    return other_kh && kh_size(other_kh) > 0 ? mrb_true_value() : mrb_false_value();
  }

  if (!other_kh) {
    return mrb_false_value(); /* Non-empty set is not a proper subset of an empty set */
  }

  /* For a proper subset, self must be strictly smaller than other */
  if (kh_size(other_kh) <= kh_size(self_kh)) {
    return mrb_false_value();
  }

  /* Check if all elements in self are in other */
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, self_kh, k) {
    khiter_t other_k = kh_get(set, mrb, other_kh, kh_key(self_kh, k));
    if (other_k == kh_end(other_kh)) {
      return mrb_false_value(); /* Element in self not found in other */
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return mrb_true_value();
}

/*
 * call-seq:
 *   set.intersect?(other) -> true or false
 *
 * Returns true if the set and the given set have at least one element in common.
 */
static mrb_value
set_intersect_p(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  /* Check if other is a Set */
  set_check_type(mrb, other);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  khash_t(set) *other_kh = set_get_khash(mrb, other);

  /* Handle empty sets */
  if (!self_kh || !other_kh || kh_size(self_kh) == 0 || kh_size(other_kh) == 0) {
    return mrb_false_value(); /* Empty sets have no elements in common */
  }

  /* Iterate through the smaller set for efficiency */
  int ai = mrb_gc_arena_save(mrb);
  if (kh_size(self_kh) < kh_size(other_kh)) {
    KHASH_FOREACH(mrb, self_kh, k) {
      khiter_t other_k = kh_get(set, mrb, other_kh, kh_key(self_kh, k));
      if (other_k != kh_end(other_kh)) {
        return mrb_true_value(); /* Found a common element */
      }
      mrb_gc_arena_restore(mrb, ai);
    }
  } else {
    KHASH_FOREACH(mrb, other_kh, k) {
      khiter_t self_k = kh_get(set, mrb, self_kh, kh_key(other_kh, k));
      if (self_k != kh_end(self_kh)) {
        return mrb_true_value(); /* Found a common element */
      }
      mrb_gc_arena_restore(mrb, ai);
    }
  }

  return mrb_false_value(); /* No common elements found */
}

/*
 * call-seq:
 *   set.disjoint?(other) -> true or false
 *
 * Returns true if the set and the given set have no elements in common.
 */
static mrb_value
set_disjoint_p(mrb_state *mrb, mrb_value self)
{
  mrb_value result = set_intersect_p(mrb, self);
  return mrb_bool_value(!mrb_test(result));
}

/*
 * call-seq:
 *   set <=> other -> -1, 0, +1, or nil
 *
 * Compares this set with another set.
 * Returns -1 if this set is a proper subset of the other set,
 * +1 if this set is a proper superset of the other set,
 * 0 if the sets are equal,
 * or nil if the sets cannot be compared (they are neither subsets nor supersets).
 */
static mrb_value
set_cmp(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  if (!set_is_set(mrb, other)) {
    return mrb_nil_value();
  }

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  khash_t(set) *other_kh = set_get_khash(mrb, other);

  /* Handle empty sets */
  if (!self_kh || kh_size(self_kh) == 0) {
    if (!other_kh || kh_size(other_kh) == 0) {
      return mrb_fixnum_value(0); /* Both empty, they're equal */
    }
    return mrb_fixnum_value(-1); /* Empty set is a proper subset of any non-empty set */
  }

  if (!other_kh || kh_size(other_kh) == 0) {
    return mrb_fixnum_value(1); /* Any non-empty set is a proper superset of an empty set */
  }

  /* Compare sizes */
  int size_cmp = kh_size(self_kh) - kh_size(other_kh);

  if (size_cmp < 0) {
    /* self might be a proper subset of other */
    int ai = mrb_gc_arena_save(mrb);
    KHASH_FOREACH(mrb, self_kh, k) {
      khiter_t other_k = kh_get(set, mrb, other_kh, kh_key(self_kh, k));
      if (other_k == kh_end(other_kh)) {
        /* Not a subset */
        mrb_gc_arena_restore(mrb, ai);
        return mrb_nil_value(); /* Not comparable */
      }
      mrb_gc_arena_restore(mrb, ai);
    }

    /* All elements of self are in other, and self is smaller than other */
    return mrb_fixnum_value(-1); /* self is a proper subset of other */
  }
  else if (size_cmp > 0) {
    /* self might be a proper superset of other */
    int ai = mrb_gc_arena_save(mrb);
    KHASH_FOREACH(mrb, other_kh, k) {
      khiter_t self_k = kh_get(set, mrb, self_kh, kh_key(other_kh, k));
      if (self_k == kh_end(self_kh)) {
        /* Not a superset */
        mrb_gc_arena_restore(mrb, ai);
        return mrb_nil_value(); /* Not comparable */
      }
      mrb_gc_arena_restore(mrb, ai);
    }

    /* All elements of other are in self, and self is larger than other */
    return mrb_fixnum_value(1); /* self is a proper superset of other */
  }
  else {
    /* Same size, check if they're equal */
    mrb_bool is_equal = TRUE;

    int ai3 = mrb_gc_arena_save(mrb);
    KHASH_FOREACH(mrb, self_kh, k) {
      khiter_t other_k = kh_get(set, mrb, other_kh, kh_key(self_kh, k));
      if (other_k == kh_end(other_kh)) {
        is_equal = FALSE;
        break;
      }
      mrb_gc_arena_restore(mrb, ai3);
    }

    if (is_equal) {
      return mrb_fixnum_value(0); /* Sets are equal */
    }
  }

  /* Sets are not comparable */
  return mrb_nil_value();
}

/*
 * call-seq:
 *   set.join(separator = nil) -> string
 *
 * Returns a string created by converting each element of the set to a string,
 * separated by the given separator.
 */
static mrb_value
set_join(mrb_state *mrb, mrb_value self)
{
  mrb_value separator = mrb_nil_value();
  mrb_get_args(mrb, "|S", &separator);

  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh || kh_size(kh) == 0) {
    return mrb_str_new_lit(mrb, "");
  }

  /* Get separator string */
  const char *sep_ptr = "";
  mrb_int sep_len = 0;
  if (!mrb_nil_p(separator)) {
    sep_ptr = RSTRING_PTR(separator);
    sep_len = RSTRING_LEN(separator);
  }

  /* Create result string */
  mrb_value result = mrb_str_new_capa(mrb, 64);  /* Initial capacity */
  mrb_bool first = TRUE;

  /* Iterate through all elements */
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, kh, k) {
    /* Add separator between elements */
    if (!first) {
      mrb_str_cat(mrb, result, sep_ptr, sep_len);
    } else {
      first = FALSE;
    }

    /* Convert element to string and append */
    mrb_value elem = kh_key(kh, k);
    mrb_value str = mrb_obj_as_string(mrb, elem);
    mrb_str_cat_str(mrb, result, str);

    /* Manage GC arena to prevent memory leaks */
    mrb_gc_arena_restore(mrb, ai);
  }

  return result;
}

/*
 * call-seq:
 *   set.inspect -> string
 *   set.to_s -> string
 *
 * Returns a string representation of the set.
 */
static mrb_value
set_inspect(mrb_state *mrb, mrb_value self)
{
  struct RClass* c = mrb_obj_class(mrb, self);
  const char* classname = mrb_class_name(mrb, c);
  khash_t(set) *kh = set_get_khash(mrb, self);

  /* Handle empty set */
  if (!kh || kh_size(kh) == 0) {
    return mrb_format(mrb, "#<%s: {}>", classname);
  }

  /* Handle recursive inspection */
  if (mrb_inspect_recursive_p(mrb, self)) {
    return mrb_format(mrb, "#<%s: {...}>", classname);
  }

  /* Estimate buffer size based on set size */
  size_t size = kh_size(kh);
  size_t buffer_size = 16 + strlen(classname) + (size * 8); /* Rough estimate */

  /* Create the beginning of the string with pre-allocated capacity */
  mrb_value result_str = mrb_str_new_capa(mrb, buffer_size);
  mrb_str_cat_lit(mrb, result_str, "#<");
  mrb_str_cat_cstr(mrb, result_str, classname);
  mrb_str_cat_lit(mrb, result_str, ": {");

  /* Iterate through all elements */
  mrb_bool first = TRUE;
  int ai = mrb_gc_arena_save(mrb);

  KHASH_FOREACH(mrb, kh, k) {
    /* Add comma between elements */
    if (!first) {
      mrb_str_cat_lit(mrb, result_str, ", ");
    } else {
      first = FALSE;
    }

    /* Get element and its string representation */
    mrb_value elem = kh_key(kh, k);
    mrb_value entry_str = mrb_inspect(mrb, elem);
    mrb_str_cat_str(mrb, result_str, entry_str);

    mrb_gc_arena_restore(mrb, ai);
  }

  /* Add the closing part */
  mrb_str_cat_lit(mrb, result_str, "}>");

  return result_str;
}

/*
 * call-seq:
 *   set.reset -> self
 *
 * Resets the internal state after modification to existing elements.
 * This is necessary when the hash value of objects in the set has changed.
 * It rebuilds the hash table to ensure all elements can be found.
 */
static mrb_value
set_reset(mrb_state *mrb, mrb_value self)
{
  mrb_check_frozen_value(mrb, self);

  khash_t(set) *old_kh = set_get_khash(mrb, self);
  if (old_kh && kh_size(old_kh) > 0) {
    /* Create a new hash table by copying the old one */
    khash_t(set) *new_kh = kh_copy(set, mrb, old_kh);

    /* Replace the old table with the new one */
    set_set_khash(mrb, self, new_kh);

    /* Destroy the old table */
    kh_destroy(set, mrb, old_kh);
  }

  return self;
}

/*
 * call-seq:
 *   set.add_all(*objects) -> self
 *
 * Adds multiple objects to the set and returns self.
 */
static mrb_value
set_add_all(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Set");
  }

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < argc; i++) {
    kh_put(set, mrb, kh, argv[i]);
    mrb_gc_arena_restore(mrb, ai);
  }

  return self;
}

/*
 * Optimized implementation for flattening sets
 * Uses a more efficient algorithm with minimal memory usage
 */

/* Small array for tracking seen object IDs to detect cycles */
#define MAX_NESTED_DEPTH 16

/*
 * Recursively flattens a set by merging nested sets into the target set.
 * This is an internal helper function that does not call back to the VM.
 *
 * @param mrb The mruby state
 * @param target_kh The target hash table to add elements to
 * @param source_kh The source hash table to flatten
 * @param seen_ids Array of object IDs to track seen sets
 * @param seen_count Pointer to the current count of seen sets
 * @return 0 on success, -1 on error
 */
static int
set_flatten_recursive(mrb_state *mrb, khash_t(set) *target_kh, khash_t(set) *source_kh,
                     mrb_int *seen_ids, int *seen_count)
{
  if (!source_kh || !target_kh) return 0;
  if (*seen_count >= MAX_NESTED_DEPTH) return -1;

  int ai = mrb_gc_arena_save(mrb);

  /* Process each element in the source set */
  KHASH_FOREACH(mrb, source_kh, k) {
    mrb_value elem = kh_key(source_kh, k);

    /* Check if element is a Set */
    if (set_is_set(mrb, elem)) {
      /* Get the object ID to track recursion */
      mrb_int obj_id = mrb_obj_id(elem);

      /* Check if we've seen this set before to prevent infinite recursion */
      for (int i = 0; i < *seen_count; i++) {
        if (seen_ids[i] == obj_id) {
          mrb_raise(mrb, E_ARGUMENT_ERROR, "tried to flatten recursive Set");
        }
      }

      /* Mark this set as seen */
      seen_ids[(*seen_count)++] = obj_id;

      /* Recursively flatten the nested set */
      khash_t(set) *nested_kh = set_get_khash(mrb, elem);
      if (nested_kh) {
        set_flatten_recursive(mrb, target_kh, nested_kh, seen_ids, seen_count);
      }

      /* Remove from seen array after processing */
      (*seen_count)--;
    } else {
      /* Add non-Set element directly */
      kh_put(set, mrb, target_kh, elem);
    }
  }

  mrb_gc_arena_restore(mrb, ai);
  return 0;
}

/*
 * call-seq:
 *   set.flatten -> new_set
 *
 * Returns a new set that is a flattened version of this set.
 * Recursively flattens nested sets.
 */
static mrb_value
set_flatten(mrb_state *mrb, mrb_value self)
{
  khash_t(set) *self_kh = set_get_khash(mrb, self);

  /* Fast path for empty sets */
  if (!self_kh || kh_size(self_kh) == 0) {
    return mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  }

  /* Fast path: check if there are any nested sets */
  mrb_bool has_nested_sets = FALSE;
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, self_kh, k) {
    if (set_is_set(mrb, kh_key(self_kh, k))) {
      has_nested_sets = TRUE;
      break;
    }
  }
  mrb_gc_arena_restore(mrb, ai);

  /* If no nested sets, just return a duplicate */
  if (!has_nested_sets) {
    return mrb_obj_dup(mrb, self);
  }

  /* Create a new set of the same class */
  mrb_value result_set = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  khash_t(set) *result_kh = set_get_khash(mrb, result_set);

  /* Use a small array for tracking seen object IDs */
  mrb_int seen_ids[MAX_NESTED_DEPTH];
  int seen_count = 0;

  /* Flatten the set */
  set_flatten_recursive(mrb, result_kh, self_kh, seen_ids, &seen_count);

  return result_set;
}

/*
 * call-seq:
 *   set.flatten! -> self or nil
 *
 * Replaces the contents of this set with a flattened version of itself.
 * Returns self if flattened, nil if no changes were made.
 */
static mrb_value
set_flatten_bang(mrb_state *mrb, mrb_value self)
{
  mrb_check_frozen_value(mrb, self);

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  if (!self_kh || kh_size(self_kh) == 0) {
    return mrb_nil_value(); /* No changes needed for empty set */
  }

  /* First, check if there are any nested sets */
  mrb_bool has_nested_sets = FALSE;
  int ai = mrb_gc_arena_save(mrb);
  KHASH_FOREACH(mrb, self_kh, k) {
    mrb_value elem = kh_key(self_kh, k);
    if (set_is_set(mrb, elem)) {
      has_nested_sets = TRUE;
      break;
    }
  }
  mrb_gc_arena_restore(mrb, ai);

  if (!has_nested_sets) {
    return mrb_nil_value(); /* No nested sets, no changes needed */
  }

  /* Create a temporary hash table for the flattened result */
  khash_t(set) *new_kh = kh_init(set, mrb);

  /* Use a small array for tracking seen object IDs */
  mrb_int seen_ids[MAX_NESTED_DEPTH];
  int seen_count = 0;

  /* Flatten the set into the new hash table */
  set_flatten_recursive(mrb, new_kh, self_kh, seen_ids, &seen_count);

  /* Replace the old hash table with the new one */
  set_set_khash(mrb, self, new_kh);

  /* Clean up the old hash table */
  kh_destroy(set, mrb, self_kh);

  return self;
}

/*
 * call-seq:
 *   set.delete_all(*objects) -> self
 *
 * Deletes multiple objects from the set and returns self.
 */
static mrb_value
set_delete_all(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) return self;

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < argc; i++) {
    khiter_t k = kh_get(set, mrb, kh, argv[i]);
    if (k != kh_end(kh)) {
      kh_del(set, mrb, kh, k);
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return self;
}

/*
 * call-seq:
 *   set.include_all?(*objects) -> true or false
 *
 * Returns true if the set contains all of the given objects.
 */
static mrb_value
set_include_all_p(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) return mrb_false_value();

  for (mrb_int i = 0; i < argc; i++) {
    khiter_t k = kh_get(set, mrb, kh, argv[i]);
    if (k == kh_end(kh)) {
      return mrb_false_value();
    }
  }

  return mrb_true_value();
}

/*
 * call-seq:
 *   set.include_any?(*objects) -> true or false
 *
 * Returns true if the set contains any of the given objects.
 */
static mrb_value
set_include_any_p(mrb_state *mrb, mrb_value self)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);
  khash_t(set) *kh = set_get_khash(mrb, self);
  if (!kh) return mrb_false_value();

  for (mrb_int i = 0; i < argc; i++) {
    khiter_t k = kh_get(set, mrb, kh, argv[i]);
    if (k != kh_end(kh)) {
      return mrb_true_value();
    }
  }

  return mrb_false_value();
}

/*
 * call-seq:
 *   Set[*ary] -> new_set
 *
 * Creates a new set containing the given objects.
 */
static mrb_value
set_s_create(mrb_state *mrb, mrb_value klass)
{
  const mrb_value *argv;
  mrb_int argc;

  mrb_get_args(mrb, "*", &argv, &argc);

  /* Optimized direct creation */
  mrb_value set = mrb_obj_new(mrb, mrb_class_ptr(klass), 0, NULL);
  khash_t(set) *kh = set_get_khash(mrb, set);

  for (mrb_int i = 0; i < argc; i++) {
    kh_put(set, mrb, kh, argv[i]);
  }

  return set;
}

void
mrb_mruby_set_gem_init(mrb_state *mrb)
{
  struct RClass *set;

  set = mrb_define_class(mrb, "Set", mrb->object_class);
  MRB_SET_INSTANCE_TT(set, MRB_TT_CDATA); /* Set instances will hold a C pointer (khash) */

  mrb_include_module(mrb, set, mrb_module_get(mrb, "Enumerable"));

  mrb_define_class_method(mrb, set, "[]", set_s_create, MRB_ARGS_ANY());

  mrb_define_private_method(mrb, set, "initialize_copy", set_init_copy, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "size", set_size, MRB_ARGS_NONE());
  mrb_define_method(mrb, set, "length", set_size, MRB_ARGS_NONE());
  mrb_define_method(mrb, set, "empty?", set_empty_p, MRB_ARGS_NONE());
  mrb_define_method(mrb, set, "clear", set_clear, MRB_ARGS_NONE());
  mrb_define_method(mrb, set, "to_a", set_to_a, MRB_ARGS_NONE());

  mrb_define_method(mrb, set, "include?", set_include_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "member?", set_include_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "===", set_include_p, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "add", set_add, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "<<", set_add, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "add?", set_add_p, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "delete", set_delete, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "delete?", set_delete_p, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "__init", set_init, MRB_ARGS_NONE());
  mrb_define_method(mrb, set, "__merge", set_core_merge, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "__subtract", set_core_subtract, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "__union", set_core_union, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "__difference", set_core_difference, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "__intersection", set_core_intersection, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "__xor", set_core_xor, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "==", set_equal, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "hash", set_hash_m, MRB_ARGS_NONE());
  mrb_define_method(mrb, set, "eql?", set_eql, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "join", set_join, MRB_ARGS_OPT(1));
  mrb_define_method(mrb, set, "inspect", set_inspect, MRB_ARGS_NONE());
  mrb_define_method(mrb, set, "to_s", set_inspect, MRB_ARGS_NONE());

  mrb_define_method(mrb, set, "reset", set_reset, MRB_ARGS_NONE());

  /* Bulk operation methods */
  mrb_define_method(mrb, set, "add_all", set_add_all, MRB_ARGS_ANY());
  mrb_define_method(mrb, set, "delete_all", set_delete_all, MRB_ARGS_ANY());
  mrb_define_method(mrb, set, "include_all?", set_include_all_p, MRB_ARGS_ANY());
  mrb_define_method(mrb, set, "include_any?", set_include_any_p, MRB_ARGS_ANY());

  /* Register our new C implementations */
  mrb_define_method(mrb, set, "superset?", set_superset_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, ">=", set_superset_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "proper_superset?", set_proper_superset_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, ">", set_proper_superset_p, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "subset?", set_subset_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "<=", set_subset_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "proper_subset?", set_proper_subset_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "<", set_proper_subset_p, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "intersect?", set_intersect_p, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "disjoint?", set_disjoint_p, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "<=>", set_cmp, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "flatten", set_flatten, MRB_ARGS_NONE());
  mrb_define_method(mrb, set, "flatten!", set_flatten_bang, MRB_ARGS_NONE());
}

void
mrb_mruby_set_gem_final(mrb_state *mrb)
{
}
