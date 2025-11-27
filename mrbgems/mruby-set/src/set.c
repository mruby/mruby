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

/* Use khash.h for set implementation - set mode (no values, only keys) */
KHASH_DECLARE(set_val, mrb_value, char, FALSE)  /* FALSE = set mode */

/* Hash and equality functions for mrb_value keys */
static inline khint_t
kset_hash_value(mrb_state *mrb, mrb_value key)
{
  return (khint_t)mrb_obj_hash_code(mrb, key);
}

static inline mrb_bool
kset_equal_value(mrb_state *mrb, mrb_value a, mrb_value b)
{
  return mrb_eql(mrb, a, b);
}

KHASH_DEFINE(set_val, mrb_value, char, FALSE, kset_hash_value, kset_equal_value)

#define KSET_INITIAL_SIZE 4
#define GOLDEN_RATIO_PRIME 0x9e3779b97f4a7c15ULL

/* Compatibility layer and type definitions */
typedef kh_set_val_t kset_t;
typedef khint_t kset_iter_t;

/* API Aliases to khash.h */
#define kset_init(mrb) kh_init(set_val, mrb)
#define kset_init_data(mrb, s, sz) kh_init_data(set_val, mrb, s, sz)
#define kset_destroy_data(mrb, s) kh_destroy_data(set_val, mrb, s)
#define kset_clear(mrb, s) kh_clear(set_val, mrb, s)
#define kset_resize(mrb, s, sz) kh_resize(set_val, mrb, s, sz)
#define kset_put(mrb, s, k) kh_put(set_val, mrb, s, k)
#define kset_put2(mrb, s, k, r) kh_put2(set_val, mrb, s, k, r)
#define kset_get(mrb, s, k) kh_get(set_val, mrb, s, k)
#define kset_del(mrb, s, k) kh_del(set_val, mrb, s, k)
#define kset_exist(s, k) kh_exist(set_val, s, k)
#define kset_key(s, k) kh_key(set_val, s, k)
#define kset_size(s) kh_size(s)
#define kset_end(s) kh_end(s)
#define kset_is_end(s, k) kh_is_end(s, k)

#define KSET_FOREACH(s, k) KHASH_FOREACH(set_val, s, k)

/* Helper macros for set state checking */
#define kset_is_uninitialized(s) ((s)->data == NULL)
#define kset_is_empty(s) (kset_is_uninitialized(s) || kset_size(s) == 0)

/* Embedded set structure in RSet - exactly 3 pointers */
struct RSet {
  MRB_OBJECT_HEADER;
  kset_t set; /* Embedded directly, not a pointer */
};

mrb_static_assert_object_size(struct RSet);

#define mrb_set_ptr(o) ((struct RSet*)mrb_obj_ptr(o))

/* Get pointer to embedded set */
static kset_t*
set_get_kset(mrb_state *mrb, mrb_value self)
{
  mrb_check_type(mrb, self, MRB_TT_SET);
  return &mrb_set_ptr(self)->set;
}

/* Get RSet pointer from embedded kset_t pointer */
#define kset_to_rset(kset) ((struct RBasic*)((char*)(kset) - offsetof(struct RSet, set)))

/* Copy all elements from src to dst (merge operation) */
static void
kset_copy_merge(mrb_state *mrb, kset_t *dst, kset_t *src)
{
  if (!kset_is_empty(src)) {
    struct RBasic *dst_obj = kset_to_rset(dst);
    int ai = mrb_gc_arena_save(mrb);
    KSET_FOREACH(src, k) {
      mrb_value key = kset_key(src, k);
      kset_put(mrb, dst, key);
      mrb_field_write_barrier_value(mrb, dst_obj, key);
      mrb_gc_arena_restore(mrb, ai);
    }
  }
}

/* Helper function to ensure set is initialized */
static void
set_ensure_initialized(mrb_state *mrb, kset_t *set)
{
  if (kset_is_uninitialized(set)) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Set");
  }
}

/* Mark function for Set instances */
size_t
mrb_gc_mark_set(mrb_state *mrb, struct RBasic *obj)
{
  struct RSet *s = (struct RSet*)obj;
  kset_t *set = &s->set;
  if (kset_is_empty(set)) return 0;

  KSET_FOREACH(set, k) {
    mrb_gc_mark_value(mrb, kset_key(set, k));
  }
  return set->size;
}

void
mrb_gc_free_set(mrb_state *mrb, struct RBasic *obj)
{
  struct RSet *s = (struct RSet*)obj;
  kset_destroy_data(mrb, &s->set);
}

size_t
mrb_set_memsize(mrb_value set)
{
  size_t size = sizeof(struct RSet);
  struct RSet *s = mrb_set_ptr(set);
  kset_t *kset = &s->set;
  if (kset->data) {
    /* New khash layout: keys + flags in single allocation */
    size += sizeof(mrb_value) * kset->n_buckets; /* keys */
    size += kset->n_buckets / 4; /* flags */
  }
  return size;
}

/* Helper function to check if a value is a Set and return a boolean result */
static mrb_bool
set_is_set(mrb_value obj)
{
  return mrb_type(obj) == MRB_TT_SET;
}

/* Helper function to check if a value is a Set and raise an error if not */
static void
set_check_type(mrb_state *mrb, mrb_value obj)
{
  if (!set_is_set(obj)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "value must be a set");
  }
}

static mrb_value
set_init(mrb_state *mrb, mrb_value self)
{
  kset_t *set = set_get_kset(mrb, self);
  if (!kset_is_uninitialized(set)) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "already initialized set");
  }
  kset_init_data(mrb, set, KSET_INITIAL_SIZE);
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

  if (mrb_type(orig) != MRB_TT_SET) {
    mrb_raise(mrb, E_TYPE_ERROR, "initialize_copy should take a Set object");
  }
  if (mrb_obj_class(mrb, self) != mrb_obj_class(mrb, orig)) {
    mrb_raise(mrb, E_TYPE_ERROR, "initialize_copy should take same class object");
  }

  kset_t *orig_set = set_get_kset(mrb, orig);
  set_ensure_initialized(mrb, orig_set);

  kset_t *self_set = set_get_kset(mrb, self);
  /* Free existing data if already initialized (for replace semantics) */
  if (!kset_is_uninitialized(self_set)) {
    kset_destroy_data(mrb, self_set);
  }
  kset_init_data(mrb, self_set, kset_size(orig_set));
  kh_replace(set_val, mrb, self_set, orig_set);

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
  kset_t *set = set_get_kset(mrb, self);
  if (kset_is_empty(set)) return mrb_fixnum_value(0);
  return mrb_fixnum_value(kset_size(set));
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
  kset_t *set = set_get_kset(mrb, self);
  return mrb_bool_value(kset_is_empty(set));
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
  kset_t *set = set_get_kset(mrb, self);
  if (!kset_is_empty(set)) {
    kset_clear(mrb, set);
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
  kset_t *set = set_get_kset(mrb, self);

  if (kset_is_empty(set)) return mrb_ary_new(mrb);

  mrb_value ary = mrb_ary_new_capa(mrb, kset_size(set));

  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(set, k) {
    mrb_ary_push(mrb, ary, kset_key(set, k));
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
  kset_t *set = set_get_kset(mrb, self);
  if (kset_is_empty(set)) return mrb_false_value();

  return mrb_bool_value(!kset_is_end(set, kset_get(mrb, set, obj)));
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
  kset_t *set = set_get_kset(mrb, self);
  set_ensure_initialized(mrb, set);

  kset_put(mrb, set, obj);
  mrb_field_write_barrier_value(mrb, kset_to_rset(set), obj);
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
  kset_t *set = set_get_kset(mrb, self);
  set_ensure_initialized(mrb, set);

  int ret;
  kset_put2(mrb, set, obj, &ret);
  mrb_field_write_barrier_value(mrb, kset_to_rset(set), obj);
  return (ret == 0) ? mrb_nil_value() : self;
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
  kset_t *set = set_get_kset(mrb, self);
  if (kset_is_empty(set)) return self;

  kset_iter_t k = kset_get(mrb, set, obj);
  if (!kset_is_end(set, k)) {
    kset_del(mrb, set, k);
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
  kset_t *set = set_get_kset(mrb, self);
  if (kset_is_empty(set)) return mrb_nil_value();

  kset_iter_t k = kset_get(mrb, set, obj);
  if (!kset_is_end(set, k)) {
    kset_del(mrb, set, k);
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

  if (!set_is_set(other)) {
    return mrb_false_value();
  }

  kset_t *self_set = set_get_kset(mrb, self);
  kset_t *other_set = set_get_kset(mrb, other);

  set_ensure_initialized(mrb, self_set);
  if (!kset_is_empty(other_set)) {
    kset_copy_merge(mrb, self_set, other_set);
  }

  return mrb_true_value();
}

/*
 * Core implementation of Set-to-Set subtraction (mutating version)
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_subtract(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  if (!set_is_set(other)) {
    return mrb_false_value();
  }

  kset_t *self_set = set_get_kset(mrb, self);
  if (kset_is_empty(self_set)) return mrb_true_value();

  kset_t *other_set = set_get_kset(mrb, other);
  if (kset_is_empty(other_set)) return mrb_true_value();

  /* Remove all elements that are in other set */
  KSET_FOREACH(other_set, k) {
    mrb_value key = kset_key(other_set, k);
    kset_iter_t self_k = kset_get(mrb, self_set, key);
    if (!kset_is_end(self_set, self_k)) {
      kset_del(mrb, self_set, self_k);
    }
  }

  return mrb_true_value();
}

/*
 * Core implementation of Set-to-Set union
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_union(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  if (!set_is_set(other)) {
    return mrb_nil_value();
  }

  /* Create a new set by duplicating self */
  mrb_value result = mrb_obj_dup(mrb, self);
  kset_t *result_set = set_get_kset(mrb, result);
  if (kset_is_uninitialized(result_set)) {
    /* If self is empty, initialize the set */
    kset_init_data(mrb, result_set, KSET_INITIAL_SIZE);
  }

  /* Add all elements from other set */
  kset_t *other_set = set_get_kset(mrb, other);
  if (!kset_is_uninitialized(other_set)) {
    kset_copy_merge(mrb, result_set, other_set);
  }

  return result;
}

/*
 * Core implementation of Set-to-Set difference
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_difference(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  if (!set_is_set(other)) {
    return mrb_nil_value();
  }

  /* Create a new set by duplicating self */
  mrb_value result = mrb_obj_dup(mrb, self);
  kset_t *result_set = set_get_kset(mrb, result);
  if (kset_is_uninitialized(result_set)) {
    /* If self is empty, return an empty set */
    return result;
  }

  /* Remove all elements that are in other set */
  kset_t *other_set = set_get_kset(mrb, other);
  if (!kset_is_uninitialized(other_set)) {
    KSET_FOREACH(other_set, k) {
      mrb_value key = kset_key(other_set, k);
      kset_iter_t result_k = kset_get(mrb, result_set, key);
      if (!kset_is_end(result_set, result_k)) {
        kset_del(mrb, result_set, result_k);
      }
    }
  }

  return result;
}


/*
 * Core implementation of Set-to-Set intersection
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_intersection(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  if (!set_is_set(other)) {
    return mrb_nil_value();
  }

  /* Create a new empty set of the same class as self */
  mrb_value result = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  kset_t *result_set = set_get_kset(mrb, result);

  kset_t *self_set = set_get_kset(mrb, self);
  if (kset_is_uninitialized(self_set)) return result;

  kset_t *other_set = set_get_kset(mrb, other);
  if (kset_is_uninitialized(other_set)) return result;

  KSET_FOREACH(other_set, k) {
    mrb_value key = kset_key(other_set, k);
    kset_iter_t self_k = kset_get(mrb, self_set, key);

    /* If key exists in self, add it to result */
    if (!kset_is_end(self_set, self_k)) {
      kset_put(mrb, result_set, key);
      mrb_field_write_barrier_value(mrb, kset_to_rset(result_set), key);
    }
  }

  return result;
}


/*
 * Core implementation of Set-to-Set XOR (symmetric difference)
 * This is an internal method that will be called from Ruby
 */
static mrb_value
set_core_xor(mrb_state *mrb, mrb_value self)
{
  mrb_value other = mrb_get_arg1(mrb);

  if (!set_is_set(other)) {
    return mrb_nil_value();
  }

  mrb_value result = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  kset_t *result_set = set_get_kset(mrb, result);
  kset_t *self_set, *other_set;

  self_set = set_get_kset(mrb, self);
  other_set = set_get_kset(mrb, other);

  /* Handle empty sets */
  if (kset_is_empty(self_set)) {
    if (!kset_is_empty(other_set)) {
      kset_copy_merge(mrb, result_set, other_set);
    }
    return result;
  }
  if (kset_is_empty(other_set)) {
    kh_replace(set_val, mrb, result_set, self_set);
    return result;
  }

  /* Add elements from self that are not in other */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    mrb_value key = kset_key(self_set, k);
    kset_iter_t other_k = kset_get(mrb, other_set, key);

    /* Add to result if not in other */
    if (kset_is_end(other_set, other_k)) {
      kset_put(mrb, result_set, key);
      mrb_field_write_barrier_value(mrb, kset_to_rset(result_set), key);
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  /* Add elements from other that are not in self */
  KSET_FOREACH(other_set, k) {
    mrb_value key = kset_key(other_set, k);
    kset_iter_t self_k = kset_get(mrb, self_set, key);

    /* Add to result if not in self */
    if (kset_is_end(self_set, self_k)) {
      kset_put(mrb, result_set, key);
      mrb_field_write_barrier_value(mrb, kset_to_rset(result_set), key);
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  return result;
}

/*
 * call-seq:
 *   set == other -> true or false
 *
 * Returns true if two sets are equal.
 */
/*
 * call-seq:
 *   set.eql?(other) -> true or false
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
  if (!set_is_set(other)) {
    return mrb_false_value();
  }

  kset_t *self_set = set_get_kset(mrb, self);
  kset_t *other_set = set_get_kset(mrb, other);

  /* Fast path: both empty */
  if (kset_is_empty(self_set) && kset_is_empty(other_set)) {
    return mrb_true_value();
  }

  /* Fast path: different sizes */
  if (kset_size(self_set) != kset_size(other_set)) {
    return mrb_false_value();
  }

  /* Compare elements: iterate through the smaller hash for efficiency */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    kset_iter_t k2 = kset_get(mrb, other_set, kset_key(self_set, k));
    if (kset_is_end(other_set, k2)) {
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
  kset_t *set = set_get_kset(mrb, self);

  /* Use order-independent hash algorithm for sets */
  uint64_t hash = 0; /* Start with zero for XOR accumulation */

  /* Include the size of the set in the hash */
  size_t size = kset_size(set);
  hash ^= size * GOLDEN_RATIO_PRIME;

  if (!kset_is_uninitialized(set) && size > 0) {
    /* Process each element - order independent using XOR */
    int ai = mrb_gc_arena_save(mrb);
    KSET_FOREACH(set, k) {
      /* Get element's hash code */
      khint_t elem_hash = (khint_t)mrb_obj_hash_code(mrb, kset_key(set, k));

      /* XOR is commutative, so order doesn't matter */
      hash ^= elem_hash * GOLDEN_RATIO_PRIME;

      mrb_gc_arena_restore(mrb, ai);
    }
  }

  /* Final mixing to improve distribution */
  hash ^= hash >> 32;

  return mrb_fixnum_value((mrb_int)hash);
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

  kset_t *self_set = set_get_kset(mrb, self);
  kset_t *other_set = set_get_kset(mrb, other);

  /* Handle empty sets */
  if (kset_is_empty(other_set)) {
    return mrb_true_value(); /* Empty set is a subset of any set */
  }

  if (kset_is_uninitialized(self_set)) {
    return mrb_false_value(); /* Empty set is not a superset of a non-empty set */
  }

  /* Check size first - a superset must be at least as large as the subset */
  if (kset_size(self_set) < kset_size(other_set)) {
    return mrb_false_value();
  }

  /* Check if all elements in other are in self */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(other_set, k) {
    kset_iter_t self_k = kset_get(mrb, self_set, kset_key(other_set, k));
    if (kset_is_end(self_set, self_k)) {
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

  kset_t *self_set = set_get_kset(mrb, self);
  kset_t *other_set = set_get_kset(mrb, other);

  /* Handle empty sets */
  if (kset_is_empty(other_set)) {
    /* Empty set is a proper subset of any non-empty set */
    return !kset_is_empty(self_set) ? mrb_true_value() : mrb_false_value();
  }

  if (kset_is_uninitialized(self_set)) {
    return mrb_false_value(); /* Empty set is not a proper superset of any set */
  }

  /* For a proper superset, self must be strictly larger than other */
  if (kset_size(self_set) <= kset_size(other_set)) {
    return mrb_false_value();
  }

  /* Check if all elements in other are in self */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(other_set, k) {
    kset_iter_t self_k = kset_get(mrb, self_set, kset_key(other_set, k));
    if (kset_is_end(self_set, self_k)) {
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

  kset_t *self_set = set_get_kset(mrb, self);
  kset_t *other_set = set_get_kset(mrb, other);

  /* Handle empty sets */
  if (kset_is_empty(self_set)) {
    return mrb_true_value(); /* Empty set is a subset of any set */
  }

  if (kset_is_uninitialized(other_set)) {
    return mrb_false_value(); /* Non-empty set is not a subset of an empty set */
  }

  /* Check size first - a subset cannot be larger than its superset */
  if (kset_size(other_set) < kset_size(self_set)) {
    return mrb_false_value();
  }

  /* Check if all elements in self are in other */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
    if (kset_is_end(other_set, other_k)) {
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

  kset_t *self_set = set_get_kset(mrb, self);
  kset_t *other_set = set_get_kset(mrb, other);

  /* Handle empty sets */
  if (kset_is_empty(self_set)) {
    /* Empty set is a proper subset of any non-empty set */
    return !kset_is_empty(other_set) ? mrb_true_value() : mrb_false_value();
  }

  if (kset_is_uninitialized(other_set)) {
    return mrb_false_value(); /* Non-empty set is not a proper subset of an empty set */
  }

  /* For a proper subset, self must be strictly smaller than other */
  if (kset_size(other_set) <= kset_size(self_set)) {
    return mrb_false_value();
  }

  /* Check if all elements in self are in other */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
    if (kset_is_end(other_set, other_k)) {
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

  kset_t *self_set = set_get_kset(mrb, self);
  kset_t *other_set = set_get_kset(mrb, other);

  /* Handle empty sets */
  if (kset_is_empty(self_set) || kset_is_empty(other_set)) {
    return mrb_false_value(); /* Empty sets have no elements in common */
  }

  /* Iterate through the smaller set for efficiency */
  int ai = mrb_gc_arena_save(mrb);
  if (kset_size(self_set) < kset_size(other_set)) {
    KSET_FOREACH(self_set, k) {
      kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
      if (!kset_is_end(other_set, other_k)) {
        return mrb_true_value(); /* Found a common element */
      }
      mrb_gc_arena_restore(mrb, ai);
    }
  }
  else {
    KSET_FOREACH(other_set, k) {
      kset_iter_t self_k = kset_get(mrb, self_set, kset_key(other_set, k));
      if (!kset_is_end(self_set, self_k)) {
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

  if (!set_is_set(other)) {
    return mrb_nil_value();
  }

  kset_t *self_set = set_get_kset(mrb, self);
  kset_t *other_set = set_get_kset(mrb, other);

  /* Handle empty sets */
  if (kset_is_empty(self_set)) {
    if (kset_is_empty(other_set)) {
      return mrb_fixnum_value(0); /* Both empty, they're equal */
    }
    return mrb_fixnum_value(-1); /* Empty set is a proper subset of any non-empty set */
  }

  if (kset_is_empty(other_set)) {
    return mrb_fixnum_value(1); /* Any non-empty set is a proper superset of an empty set */
  }

  /* Compare sizes */
  mrb_int size_diff = kset_size(self_set) - kset_size(other_set);

  if (size_diff < 0) {
    /* self might be a proper subset of other */
    int ai = mrb_gc_arena_save(mrb);
    KSET_FOREACH(self_set, k) {
      kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
      if (kset_is_end(other_set, other_k)) {
        /* Not a subset */
        return mrb_nil_value(); /* Not comparable */
      }
      mrb_gc_arena_restore(mrb, ai);
    }

    /* All elements of self are in other, and self is smaller than other */
    return mrb_fixnum_value(-1); /* self is a proper subset of other */
  }
  else if (size_diff > 0) {
    /* self might be a proper superset of other */
    int ai = mrb_gc_arena_save(mrb);
    KSET_FOREACH(other_set, k) {
      kset_iter_t self_k = kset_get(mrb, self_set, kset_key(other_set, k));
      if (kset_is_end(self_set, self_k)) {
        /* Not a superset */
        return mrb_nil_value(); /* Not comparable */
      }
      mrb_gc_arena_restore(mrb, ai);
    }

    /* All elements of other are in self, and self is larger than other */
    return mrb_fixnum_value(1); /* self is a proper superset of other */
  }
  else { /* size_diff == 0 */
    /* Same size, check if they're equal */
    mrb_bool is_equal = TRUE;

    int ai3 = mrb_gc_arena_save(mrb);
    KSET_FOREACH(self_set, k) {
      kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
      if (kset_is_end(other_set, other_k)) {
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

  kset_t *set = set_get_kset(mrb, self);
  if (kset_is_empty(set)) {
    return mrb_str_new_lit(mrb, "");
  }

  /* Create result string */
  mrb_value result = mrb_str_new_capa(mrb, 64);  /* Initial capacity */
  mrb_bool first = TRUE;

  /* Iterate through all elements */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(set, k) {
    if (!first) {
      if (!mrb_nil_p(separator)) {
        mrb_str_cat(mrb, result, RSTRING_PTR(separator), RSTRING_LEN(separator));
      }
    }
    else {
      first = FALSE;
    }

    mrb_value elem = kset_key(set, k);
    mrb_value str = mrb_obj_as_string(mrb, elem);
    mrb_str_cat_str(mrb, result, str);

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
 * Format: Set[elem1, elem2, ...]
 */
static mrb_value
set_inspect(mrb_state *mrb, mrb_value self)
{
  struct RClass* c = mrb_obj_class(mrb, self);
  const char* classname = mrb_class_name(mrb, c);
  kset_t *set = set_get_kset(mrb, self);

  /* Handle empty set */
  if (kset_is_empty(set)) {
    return mrb_format(mrb, "%s[]", classname);
  }

  /* Handle recursive inspection */
  if (MRB_RECURSIVE_UNARY_P(mrb, MRB_SYM(inspect), self)) {
    return mrb_format(mrb, "%s[...]", classname);
  }

  /* Estimate buffer size based on set size */
  size_t size = kset_size(set);
  size_t buffer_size = 16 + strlen(classname) + (size * 8); /* Rough estimate */

  /* Create the beginning of the string with pre-allocated capacity */
  mrb_value result_str = mrb_str_new_capa(mrb, buffer_size);
  mrb_str_cat_cstr(mrb, result_str, classname);
  mrb_str_cat_lit(mrb, result_str, "[");

  /* Iterate through all elements */
  mrb_bool first = TRUE;
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(set, k) {
    if (!first) {
      mrb_str_cat_lit(mrb, result_str, ", ");
    }
    else {
      first = FALSE;
    }

    mrb_value elem = kset_key(set, k);
    mrb_value entry_str = mrb_inspect(mrb, elem);
    mrb_str_cat_str(mrb, result_str, entry_str);

    mrb_gc_arena_restore(mrb, ai);
  }

  /* Add the closing part */
  mrb_str_cat_lit(mrb, result_str, "]");

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

  kset_t *set = set_get_kset(mrb, self);
  if (!kset_is_empty(set)) {
    kset_resize(mrb, set, kset_size(set));
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
  kset_t *set = set_get_kset(mrb, self);
  set_ensure_initialized(mrb, set);

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < argc; i++) {
    kset_put(mrb, set, argv[i]);
    mrb_field_write_barrier_value(mrb, kset_to_rset(set), argv[i]);
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
 * @param target The target set table to add elements to
 * @param source The source set table to flatten
 * @param seen_count Pointer to the current count of seen sets (recursion depth)
 * @return 0 on success, -1 if recursion depth exceeds maximum
 */
static int
set_flatten_recursive(mrb_state *mrb, kset_t *target, kset_t *source, int *seen_count)
{
  if (!source || !target) return 0;
  if (*seen_count >= MAX_NESTED_DEPTH) return -1;

  struct RBasic *target_obj = kset_to_rset(target);
  int ai = mrb_gc_arena_save(mrb);
  /* Process each element in the source set */
  KSET_FOREACH(source, k) {
    mrb_value elem = kset_key(source, k);

    /* Check if element is a Set */
    if (set_is_set(elem)) {
      /* Increment recursion depth */
      (*seen_count)++;

      /* Recursively flatten the nested set */
      kset_t *nested_set = set_get_kset(mrb, elem);
      if (nested_set) {
        int nested_result = set_flatten_recursive(mrb, target, nested_set, seen_count);
        if (nested_result < 0) {
          return nested_result; /* Propagate error code */
        }
      }

      /* Decrement recursion depth */
      (*seen_count)--;
    }
    else {
      /* Add non-Set element directly */
      kset_put(mrb, target, elem);
      mrb_field_write_barrier_value(mrb, target_obj, elem);
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  return 0;
}

/*
 * Helper function: Check if a set has any nested sets
 * Returns TRUE if nested sets found, FALSE otherwise
 */
static mrb_bool
set_has_nested_sets(mrb_state *mrb, kset_t *set)
{
  if (kset_is_empty(set)) return FALSE;

  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(set, k) {
    if (set_is_set(kset_key(set, k))) {
      return TRUE;
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  return FALSE;
}

/*
 * Helper function: Perform the actual flattening operation
 * Returns the flattened set (creates a new kset_t*)
 */
static void
set_do_flatten(mrb_state *mrb, kset_t *result_set, kset_t *source_set)
{
  int seen_count = 0;

  if (set_flatten_recursive(mrb, result_set, source_set, &seen_count) < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "flatten recursion depth too deep");
  }
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
  kset_t *self_set = set_get_kset(mrb, self);

  /* Fast path for empty sets */
  if (kset_is_empty(self_set)) {
    return mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  }

  /* Fast path: check if there are any nested sets */
  if (!set_has_nested_sets(mrb, self_set)) {
    return mrb_obj_dup(mrb, self);
  }

  /* Create a new set and flatten into it */
  mrb_value result = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  kset_t *result_set = set_get_kset(mrb, result);

  set_do_flatten(mrb, result_set, self_set);

  return result;
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

  kset_t *self_set = set_get_kset(mrb, self);
  if (kset_is_empty(self_set)) {
    return mrb_nil_value(); /* No changes needed for empty set */
  }

  /* Check if there are any nested sets */
  if (!set_has_nested_sets(mrb, self_set)) {
    return mrb_nil_value(); /* No nested sets, no changes needed */
  }

  /* Create a temporary set to flatten into (GC-protected) */
  mrb_value temp = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  kset_t *temp_set = set_get_kset(mrb, temp);

  set_do_flatten(mrb, temp_set, self_set);

  /* Swap the data between self and temp */
  kset_t temp_data = *self_set;
  *self_set = *temp_set;
  *temp_set = temp_data;

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
  kset_t *ks = set_get_kset(mrb, self);
  if (kset_is_uninitialized(ks)) return self;

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < argc; i++) {
    kset_iter_t k = kset_get(mrb, ks, argv[i]);
    if (!kset_is_end(ks, k)) {
      kset_del(mrb, ks, k);
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
  kset_t *ks = set_get_kset(mrb, self);
  if (kset_is_uninitialized(ks)) return mrb_false_value();

  for (mrb_int i = 0; i < argc; i++) {
    kset_iter_t k = kset_get(mrb, ks, argv[i]);
    if (kset_is_end(ks, k)) {
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
  kset_t *ks = set_get_kset(mrb, self);
  if (kset_is_empty(ks)) return mrb_false_value();

  for (mrb_int i = 0; i < argc; i++) {
    kset_iter_t k = kset_get(mrb, ks, argv[i]);
    if (!kset_is_end(ks, k)) {
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
  kset_t *ks = set_get_kset(mrb, set);

  for (mrb_int i = 0; i < argc; i++) {
    kset_put(mrb, ks, argv[i]);
    mrb_field_write_barrier_value(mrb, kset_to_rset(ks), argv[i]);
  }

  return set;
}

void
mrb_mruby_set_gem_init(mrb_state *mrb)
{
  struct RClass *set;

  set = mrb_define_class(mrb, "Set", mrb->object_class);
  MRB_SET_INSTANCE_TT(set, MRB_TT_SET);

  mrb_include_module(mrb, set, mrb_module_get(mrb, "Enumerable"));

  mrb_define_class_method(mrb, set, "[]", set_s_create, MRB_ARGS_ANY());

  mrb_define_private_method(mrb, set, "initialize_copy", set_init_copy, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM(size), set_size, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, set, MRB_SYM(length), set_size, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, set, MRB_SYM_Q(empty), set_empty_p, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, set, MRB_SYM(clear), set_clear, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, set, MRB_SYM(to_a), set_to_a, MRB_ARGS_NONE());

  mrb_define_method_id(mrb, set, MRB_SYM_Q(include), set_include_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM_Q(member), set_include_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_OPSYM(eqq), set_include_p, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM(add), set_add, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_OPSYM(lshift), set_add, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM_Q(add), set_add_p, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM(delete), set_delete, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM_Q(delete), set_delete_p, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM(__init), set_init, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, set, MRB_SYM(__merge), set_core_merge, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM(__subtract), set_core_subtract, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM(__union), set_core_union, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM(__difference), set_core_difference, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM(__intersection), set_core_intersection, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM(__xor), set_core_xor, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_OPSYM(eq), set_equal, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM(hash), set_hash_m, MRB_ARGS_NONE());
  mrb_define_alias(mrb, set, "eql?", "==");

  mrb_define_method_id(mrb, set, MRB_SYM(join), set_join, MRB_ARGS_OPT(1));
  mrb_define_method_id(mrb, set, MRB_SYM(inspect), set_inspect, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, set, MRB_SYM(to_s), set_inspect, MRB_ARGS_NONE());

  mrb_define_method_id(mrb, set, MRB_SYM(reset), set_reset, MRB_ARGS_NONE());

  /* Bulk operation methods */
  mrb_define_method_id(mrb, set, MRB_SYM(add_all), set_add_all, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, set, MRB_SYM(delete_all), set_delete_all, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, set, MRB_SYM_Q(include_all), set_include_all_p, MRB_ARGS_ANY());
  mrb_define_method_id(mrb, set, MRB_SYM_Q(include_any), set_include_any_p, MRB_ARGS_ANY());

  /* Register our new C implementations */
  mrb_define_method_id(mrb, set, MRB_SYM_Q(superset), set_superset_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_OPSYM(ge), set_superset_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM_Q(proper_superset), set_proper_superset_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_OPSYM(gt), set_proper_superset_p, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM_Q(subset), set_subset_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_OPSYM(le), set_subset_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM_Q(proper_subset), set_proper_subset_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_OPSYM(lt), set_proper_subset_p, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM_Q(intersect), set_intersect_p, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, set, MRB_SYM_Q(disjoint), set_disjoint_p, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_OPSYM(cmp), set_cmp, MRB_ARGS_REQ(1));

  mrb_define_method_id(mrb, set, MRB_SYM(flatten), set_flatten, MRB_ARGS_NONE());
  mrb_define_method_id(mrb, set, MRB_SYM_B(flatten), set_flatten_bang, MRB_ARGS_NONE());
}

void
mrb_mruby_set_gem_final(mrb_state *mrb)
{
}
