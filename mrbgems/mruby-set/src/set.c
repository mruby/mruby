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

/* Helper function to iterate over a khash and call a callback for each entry */
typedef void (*khash_each_func)(mrb_state *mrb, khash_t(set) *kh, khiter_t k, void *data);

static void
set_khash_foreach(mrb_state *mrb, khash_t(set) *kh, khash_each_func func, void *data)
{
  if (!kh) return;

  int ai = mrb_gc_arena_save(mrb);
  for (khiter_t k = kh_begin(kh); k != kh_end(kh); k++) {
    if (kh_exist(kh, k)) {
      func(mrb, kh, k, data);
      mrb_gc_arena_restore(mrb, ai);
    }
  }
}

/* Helper function to copy all elements from source_kh to target_kh */
static void
set_copy_key_callback(mrb_state *mrb, khash_t(set) *source_kh, khiter_t k, void *data)
{
  khash_t(set) *target_kh = (khash_t(set)*)data;
  kh_put(set, mrb, target_kh, kh_key(source_kh, k));
}

static void
set_copy_elements(mrb_state *mrb, khash_t(set) *target_kh, khash_t(set) *source_kh)
{
  if (!source_kh || !target_kh) return;
  set_khash_foreach(mrb, source_kh, set_copy_key_callback, target_kh);
}

/* Helper function to convert a khash to an array */
static void
set_to_array_callback(mrb_state *mrb, khash_t(set) *kh, khiter_t k, void *data)
{
  mrb_value ary = *(mrb_value*)data;
  mrb_ary_push(mrb, ary, kh_key(kh, k));
}

/* Helper function to check if a key exists in another khash and delete if found */
typedef struct {
  khash_t(set) *target_kh;
  mrb_bool delete_if_found;
} set_check_key_data;

static void
set_check_key_callback(mrb_state *mrb, khash_t(set) *source_kh, khiter_t k, void *data)
{
  set_check_key_data *check_data = (set_check_key_data*)data;
  mrb_value key = kh_key(source_kh, k);
  khiter_t target_k = kh_get(set, mrb, check_data->target_kh, key);

  if (check_data->delete_if_found) {
    /* Delete from target if found */
    if (target_k != kh_end(check_data->target_kh)) {
      kh_del(set, mrb, check_data->target_kh, target_k);
    }
  }
  else {
    /* Add to target if not found */
    if (target_k == kh_end(check_data->target_kh)) {
      kh_put(set, mrb, check_data->target_kh, key);
    }
  }
}

/* Helper function for set_core_xor to add elements from source to target if not in exclude */
typedef struct {
  khash_t(set) *target_kh;
  khash_t(set) *result_kh;
} set_operation_data;

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

/*
 * call-seq:
 *   Set.new(enum = nil)
 *   Set.new(enum = nil) { |o| block }
 *
 * Creates a new set containing the members of the given enumerable object.
 */
static mrb_value
set_init(mrb_state *mrb, mrb_value self)
{
  mrb_value enum_obj = mrb_nil_value();
  mrb_value block = mrb_nil_value();

  mrb_get_args(mrb, "|o&", &enum_obj, &block);

  /* Initialize the khash and associate it with the Ruby object */
  khash_t(set) *kh = kh_init(set, mrb);

  /* Associate the khash with the Ruby object so it will be freed when the object is GC'd */
  set_set_khash(mrb, self, kh);

  if (mrb_nil_p(enum_obj)) {
    return self;
  }

  if (!mrb_nil_p(block)) {
    /* Block given - validate with the temporary set first */
    mrb_value args[1] = { enum_obj };
    /* If we get here, no exception was raised, so we can safely proceed with our main set */
    mrb_funcall_with_block(mrb, self, MRB_SYM(__init_with_block), 1, args, block);
  }
  else {
    /* If we get here, no exception was raised, so we can safely proceed with our main set */
    mrb_funcall_id(mrb, self, MRB_SYM(merge), 1, enum_obj);
  }

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
  set_khash_foreach(mrb, kh, set_to_array_callback, &ary);
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
  set_check_key_data data = {self_kh, TRUE};

  set_khash_foreach(mrb, other_kh, set_check_key_callback, &data);

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
    set_check_key_data data = {result_kh, TRUE};
    set_khash_foreach(mrb, other_kh, set_check_key_callback, &data);
  }

  return result_set;
}

/* Helper function for intersection */
static void
set_intersection_callback(mrb_state *mrb, khash_t(set) *source_kh, khiter_t k, void *data)
{
  set_operation_data *intersect_data = (set_operation_data*)data;
  mrb_value key = kh_key(source_kh, k);
  khiter_t target_k = kh_get(set, mrb, intersect_data->target_kh, key);

  /* If key exists in target, add it to result */
  if (target_k != kh_end(intersect_data->target_kh)) {
    kh_put(set, mrb, intersect_data->result_kh, key);
  }
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

  set_operation_data data = {self_kh, result_kh};
  set_khash_foreach(mrb, other_kh, set_intersection_callback, &data);

  return result_set;
}

static void
set_xor_callback(mrb_state *mrb, khash_t(set) *source_kh, khiter_t k, void *data)
{
  set_operation_data *xor_data = (set_operation_data*)data;
  mrb_value key = kh_key(source_kh, k);
  khiter_t exclude_k = kh_get(set, mrb, xor_data->target_kh, key);

  /* Add to result if not in exclude */
  if (exclude_k == kh_end(xor_data->target_kh)) {
    kh_put(set, mrb, xor_data->result_kh, key);
  }
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

  /* Use our helper function to add elements from self that are not in other */
  set_operation_data self_xor_data = { other_kh, result_kh };
  set_khash_foreach(mrb, self_kh, set_xor_callback, &self_xor_data);

  /* Use our helper function to add elements from other that are not in self */
  set_operation_data other_xor_data = { self_kh, result_kh };
  set_khash_foreach(mrb, other_kh, set_xor_callback, &other_xor_data);

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

  if (mrb_obj_equal(mrb, self, other)) {
    return mrb_true_value();
  }

  if (mrb_obj_is_kind_of(mrb, other, mrb_obj_class(mrb, self))) {
    khash_t(set) *kh1 = set_get_khash(mrb, self);
    khash_t(set) *kh2 = set_get_khash(mrb, other);

    if (kh1 && kh2 && kh_size(kh1) == kh_size(kh2)) {
      /* Check if all elements in self exist in other */
      mrb_bool all_found = TRUE;

      /* Use traditional C loop for compatibility */
      for (khiter_t k = kh_begin(kh1); k != kh_end(kh1); k++) {
        if (kh_exist(kh1, k)) {
          khiter_t k2 = kh_get(set, mrb, kh2, kh_key(kh1, k));
          if (k2 == kh_end(kh2)) {
            all_found = FALSE;
            break;
          }
        }
      }

      return mrb_bool_value(all_found);
    }
    else if (!kh1 && !kh2) {
      return mrb_true_value(); /* Both empty */
    }
  }
  return mrb_false_value();
}

/* Helper function for hash computation */
static void
set_hash_callback(mrb_state *mrb, khash_t(set) *kh, khiter_t k, void *data)
{
  khint_t *hash_val = (khint_t*)data;
  *hash_val ^= (khint_t)mrb_obj_hash_code(mrb, kh_key(kh, k));
}

/*
 * call-seq:
 *   set.hash -> integer
 *
 * Compute a hash-code for this set.
 */
static mrb_value
set_hash_m(mrb_state *mrb, mrb_value self)
{
  khash_t(set) *kh = set_get_khash(mrb, self);
  khint_t hash_val = 0x1234;

  if (kh) {
    set_khash_foreach(mrb, kh, set_hash_callback, &hash_val);
  }

  return mrb_fixnum_value((mrb_int)hash_val);
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

  if (!mrb_obj_is_kind_of(mrb, other, mrb_obj_class(mrb, self))) {
    return mrb_false_value();
  }

  khash_t(set) *self_kh = set_get_khash(mrb, self);
  khash_t(set) *other_kh = set_get_khash(mrb, other);

  if (!self_kh && !other_kh) {
    return mrb_true_value(); /* Both empty */
  }

  if (!self_kh || !other_kh || kh_size(self_kh) != kh_size(other_kh)) {
    return mrb_false_value();
  }

  /* Check if all elements are eql - reuse the same logic as set_equal */
  mrb_bool all_found = TRUE;

  for (khiter_t k = kh_begin(self_kh); k != kh_end(self_kh); k++) {
    if (kh_exist(self_kh, k)) {
      khiter_t other_k = kh_get(set, mrb, other_kh, kh_key(self_kh, k));
      if (other_k == kh_end(other_kh)) {
        all_found = FALSE;
        break;
      }
    }
  }

  if (!all_found) {
    return mrb_false_value();
  }

  return mrb_true_value();
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
  return mrb_ary_join(mrb, set_to_a(mrb, self), separator);
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

  if (mrb_test(set_empty_p(mrb, self))) {
    return mrb_format(mrb, "#<%s: {}>", classname);
  }
  if (mrb_inspect_recursive_p(mrb, self)) {
    return mrb_format(mrb, "#<%s: {...}>", classname);
  }
  mrb_value ary = set_to_a(mrb, self);
  mrb_value result_str = mrb_str_new_lit(mrb, "#<");
  mrb_str_cat_cstr(mrb, result_str, classname);
  mrb_str_cat_lit(mrb, result_str, ": {");
  for (mrb_int i = 0; i < RARRAY_LEN(ary); i++) {
    if (i > 0) mrb_str_cat_lit(mrb, result_str, ", ");
    mrb_value entry_str = mrb_inspect(mrb, mrb_ary_entry(ary, i));
    mrb_str_cat_str(mrb, result_str, entry_str);
  }
  mrb_str_cat_lit(mrb, result_str, "}>");
  return result_str;
}

/*
 * call-seq:
 *   set.reset -> self
 *
 * Resets the internal state after modification to existing elements.
 */
static mrb_value
set_reset(mrb_state *mrb, mrb_value self)
{
  mrb_check_frozen_value(mrb, self);

  khash_t(set) *kh = set_get_khash(mrb, self);
  if (kh) {
    /* For khash, we don't need to do anything special for rehashing
       as the hash function is deterministic based on object identity */
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

  mrb_define_method(mrb, set, "initialize", set_init, MRB_ARGS_OPT(1) | MRB_ARGS_BLOCK());
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

  mrb_define_method(mrb, set, "__set_merge", set_core_merge, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "__set_subtract", set_core_subtract, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "__set_union", set_core_union, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "__set_difference", set_core_difference, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "__set_intersection", set_core_intersection, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "__set_xor", set_core_xor, MRB_ARGS_REQ(1));

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
}

void
mrb_mruby_set_gem_final(mrb_state *mrb)
{
}
