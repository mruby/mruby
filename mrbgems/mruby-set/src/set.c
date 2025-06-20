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
  mrb_value orig;
  khash_t(set) *kh;

  mrb_get_args(mrb, "o", &orig);

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
  for (khiter_t k = kh_begin(kh); k != kh_end(kh); k++) {
    if (kh_exist(kh, k)) {
      mrb_ary_push(mrb, ary, kh_key(kh, k));
    }
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
  mrb_value obj;
  khash_t(set) *kh;
  khiter_t k;

  mrb_get_args(mrb, "o", &obj);
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
  mrb_value obj;
  khash_t(set) *kh;

  mrb_get_args(mrb, "o", &obj);
  kh = set_get_khash(mrb, self);
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
  mrb_value obj;
  khash_t(set) *kh;
  int ret;

  mrb_get_args(mrb, "o", &obj);
  kh = set_get_khash(mrb, self);
  if (!kh) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Set");
  }

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
  mrb_value obj;
  khash_t(set) *kh;
  khiter_t k;

  mrb_get_args(mrb, "o", &obj);
  kh = set_get_khash(mrb, self);
  if (!kh) return self;

  k = kh_get(set, mrb, kh, obj);
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
  mrb_value obj;
  khash_t(set) *kh;
  khiter_t k;

  mrb_get_args(mrb, "o", &obj);
  kh = set_get_khash(mrb, self);
  if (!kh) return mrb_nil_value();

  k = kh_get(set, mrb, kh, obj);
  if (k != kh_end(kh)) {
    kh_del(set, mrb, kh, k);
    return self;
  }
  else {
    return mrb_nil_value();
  }
}

/*
 * call-seq:
 *   set.replace(enum) -> self
 *
 * Replaces the contents of the set with the contents of the given enumerable
 * object and returns self.
 */
static mrb_value
set_replace(mrb_state *mrb, mrb_value self)
{
  mrb_value enum_obj;

  mrb_get_args(mrb, "o", &enum_obj);
  mrb_funcall_id(mrb, self, MRB_SYM(clear), 0);
  return mrb_funcall_id(mrb, self, MRB_SYM(merge), 1, enum_obj);
}

/*
 * call-seq:
 *   set.merge(enum) -> self
 *
 * Merges the elements of the given enumerable object to the set and returns
 * self.
 */
static mrb_value
set_merge(mrb_state *mrb, mrb_value self)
{
  mrb_value enum_obj;
  khash_t(set) *kh;

  mrb_get_args(mrb, "o", &enum_obj);
  kh = set_get_khash(mrb, self);
  if (!kh) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Set");
  }

  if (mrb_obj_class(mrb, enum_obj) == mrb_obj_class(mrb, self)) {
    /* Optimized path for Set objects - direct khash merge */
    khash_t(set) *other_kh = set_get_khash(mrb, enum_obj);
    if (other_kh) {
      khiter_t k;
      int ai = mrb_gc_arena_save(mrb);
      for (k = kh_begin(other_kh); k != kh_end(other_kh); k++) {
        if (kh_exist(other_kh, k)) {
          kh_put(set, mrb, kh, kh_key(other_kh, k));
          mrb_gc_arena_restore(mrb, ai);
        }
      }
    }
  }
  else {
    /* General enumerable path - delegate to Ruby */
    return mrb_funcall_with_block(mrb, self, MRB_SYM(__merge_enum), 1, &enum_obj, mrb_nil_value());
  }

  return self;
}

/*
 * call-seq:
 *   set.subtract(enum) -> self
 *
 * Deletes every element that appears in the given enumerable object and
 * returns self.
 */
static mrb_value
set_subtract(mrb_state *mrb, mrb_value self)
{
  mrb_value enum_obj;
  khash_t(set) *kh;

  mrb_get_args(mrb, "o", &enum_obj);
  kh = set_get_khash(mrb, self);
  if (!kh) return self;

  if (mrb_obj_class(mrb, enum_obj) == mrb_obj_class(mrb, self)) {
    /* Optimized path for Set objects */
    khash_t(set) *other_kh = set_get_khash(mrb, enum_obj);
    if (other_kh) {
      khiter_t k, del_k;
      int ai = mrb_gc_arena_save(mrb);
      for (k = kh_begin(other_kh); k != kh_end(other_kh); k++) {
        if (kh_exist(other_kh, k)) {
          del_k = kh_get(set, mrb, kh, kh_key(other_kh, k));
          if (del_k != kh_end(kh)) {
            kh_del(set, mrb, kh, del_k);
          }
          mrb_gc_arena_restore(mrb, ai);
        }
      }
    }
  }
  else {
    /* General enumerable path - delegate to Ruby */
    return mrb_funcall_with_block(mrb, self, MRB_SYM(__subtract_enum), 1, &enum_obj, mrb_nil_value());
  }

  return self;
}

/*
 * call-seq:
 *   set | enum -> new_set
 *   set + enum -> new_set
 *   set.union(enum) -> new_set
 *
 * Returns a new set built by merging the set and the elements of the given
 * enumerable object.
 */
static mrb_value
set_union(mrb_state *mrb, mrb_value self)
{
  mrb_value enum_obj;
  mrb_value new_set;

  mrb_get_args(mrb, "o", &enum_obj);
  new_set = mrb_funcall_id(mrb, self, MRB_SYM(dup), 0);
  return mrb_funcall_id(mrb, new_set, MRB_SYM(merge), 1, enum_obj);
}

/*
 * call-seq:
 *   set - enum -> new_set
 *   set.difference(enum) -> new_set
 *
 * Returns a new set built by duplicating the set, removing every element that
 * appears in the given enumerable object.
 */
static mrb_value
set_difference(mrb_state *mrb, mrb_value self)
{
  mrb_value enum_obj;
  mrb_value new_set;

  mrb_get_args(mrb, "o", &enum_obj);
  new_set = mrb_funcall_id(mrb, self, MRB_SYM(dup), 0);
  return mrb_funcall_id(mrb, new_set, MRB_SYM(subtract), 1, enum_obj);
}

/*
 * call-seq:
 *   set & enum -> new_set
 *   set.intersection(enum) -> new_set
 *
 * Returns a new set containing elements common to the set and the given
 * enumerable object.
 */
static mrb_value
set_intersection(mrb_state *mrb, mrb_value self)
{
  mrb_value enum_obj;
  mrb_value result_set;
  khash_t(set) *result_kh, *self_kh;

  mrb_get_args(mrb, "o", &enum_obj);

  result_set = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  result_kh = set_get_khash(mrb, result_set);

  self_kh = set_get_khash(mrb, self);
  if (!self_kh) return result_set;

  if (mrb_obj_class(mrb, enum_obj) == mrb_obj_class(mrb, self)) {
    /* Optimized path for Set objects */
    khash_t(set) *other_kh = set_get_khash(mrb, enum_obj);
    if (other_kh) {
      khiter_t k, self_k;
      int ai = mrb_gc_arena_save(mrb);
      for (k = kh_begin(other_kh); k != kh_end(other_kh); k++) {
        if (kh_exist(other_kh, k)) {
          mrb_value key = kh_key(other_kh, k);
          self_k = kh_get(set, mrb, self_kh, key);
          if (self_k != kh_end(self_kh)) {
            kh_put(set, mrb, result_kh, key);
          }
          mrb_gc_arena_restore(mrb, ai);
        }
      }
    }
  }
  else {
    /* General enumerable path - delegate to Ruby */
    return mrb_funcall_with_block(mrb, self, MRB_SYM(__intersection_enum), 1, &enum_obj, mrb_nil_value());
  }

  return result_set;
}

/*
 * call-seq:
 *   set ^ enum -> new_set
 *
 * Returns a new set containing elements exclusive between the set and the
 * given enumerable object.
 */
static mrb_value
set_xor(mrb_state *mrb, mrb_value self)
{
  mrb_value enum_obj;
  mrb_value union_set, intersection_set;

  mrb_get_args(mrb, "o", &enum_obj);
  union_set = mrb_funcall_id(mrb, self, MRB_OPSYM(or), 1, enum_obj);
  intersection_set = mrb_funcall_id(mrb, self, MRB_OPSYM(and), 1, enum_obj);
  return mrb_funcall_id(mrb, union_set, MRB_OPSYM(sub), 1, intersection_set);
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
      /* check if all elements in self exist in other */
      for (khiter_t k = kh_begin(kh1); k != kh_end(kh1); k++) {
        if (kh_exist(kh1, k)) {
          khiter_t k2 = kh_get(set, mrb, kh2, kh_key(kh1, k));
          if (k2 == kh_end(kh2)) {
            return mrb_false_value();
          }
        }
      }
      return mrb_true_value();
    }
    else if (!kh1 && !kh2) {
      return mrb_true_value(); /* Both empty */
    }
  }
  return mrb_false_value();
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
    khiter_t k;
    /* Simple hash combining all element hashes */
    for (k = kh_begin(kh); k != kh_end(kh); k++) {
      if (kh_exist(kh, k)) {
        hash_val ^= (khint_t)mrb_obj_hash_code(mrb, kh_key(kh, k));
      }
    }
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
  mrb_value other;
  khash_t(set) *self_kh, *other_kh;

  mrb_get_args(mrb, "o", &other);

  if (!mrb_obj_is_kind_of(mrb, other, mrb_obj_class(mrb, self))) {
    return mrb_false_value();
  }

  self_kh = set_get_khash(mrb, self);
  other_kh = set_get_khash(mrb, other);

  if (!self_kh && !other_kh) {
    return mrb_true_value(); /* Both empty */
  }

  if (!self_kh || !other_kh || kh_size(self_kh) != kh_size(other_kh)) {
    return mrb_false_value();
  }

  /* Check if all elements are eql */
  khiter_t k;
  for (k = kh_begin(self_kh); k != kh_end(self_kh); k++) {
    if (kh_exist(self_kh, k)) {
      khiter_t other_k = kh_get(set, mrb, other_kh, kh_key(self_kh, k));
      if (other_k == kh_end(other_kh)) {
        return mrb_false_value();
      }
    }
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
  mrb_value array;

  mrb_get_args(mrb, "|S", &separator);
  array = set_to_a(mrb, self);
  return mrb_ary_join(mrb, array, separator);
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
  khash_t(set) *kh;

  mrb_check_frozen_value(mrb, self);

  kh = set_get_khash(mrb, self);
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
  khash_t(set) *kh;
  int ai;

  mrb_get_args(mrb, "*", &argv, &argc);
  kh = set_get_khash(mrb, self);
  if (!kh) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "uninitialized Set");
  }

  ai = mrb_gc_arena_save(mrb);
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
  khash_t(set) *kh;
  int ai;

  mrb_get_args(mrb, "*", &argv, &argc);
  kh = set_get_khash(mrb, self);
  if (!kh) return self;

  ai = mrb_gc_arena_save(mrb);
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
  khash_t(set) *kh;

  mrb_get_args(mrb, "*", &argv, &argc);
  kh = set_get_khash(mrb, self);
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
  khash_t(set) *kh;

  mrb_get_args(mrb, "*", &argv, &argc);
  kh = set_get_khash(mrb, self);
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
  mrb_value set;
  khash_t(set) *kh;

  mrb_get_args(mrb, "*", &argv, &argc);

  /* Optimized direct creation */
  set = mrb_obj_new(mrb, mrb_class_ptr(klass), 0, NULL);
  kh = set_get_khash(mrb, set);

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

  mrb_define_method(mrb, set, "replace", set_replace, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "merge", set_merge, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "subtract", set_subtract, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "|", set_union, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "+", set_union, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "union", set_union, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "-", set_difference, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "difference", set_difference, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "&", set_intersection, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, set, "intersection", set_intersection, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, set, "^", set_xor, MRB_ARGS_REQ(1));

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
