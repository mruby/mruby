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

/* Compact set implementation - memory optimized for struct RSet embedding */
typedef uint32_t kset_int_t;
typedef kset_int_t kset_iter_t;

#ifndef KSET_DEFAULT_SIZE
# define KSET_DEFAULT_SIZE 8
#endif
#define KSET_MIN_SIZE 8

#define KSET_UPPER_BOUND(x) ((x)>>2|(x)>>1)

/* Flag masks for empty/deleted status - 2 bits per bucket */
static const uint8_t kset_empty_mask[]  = {0x02, 0x08, 0x20, 0x80};
static const uint8_t kset_del_mask[]    = {0x01, 0x04, 0x10, 0x40};
static const uint8_t kset_either_mask[] = {0x03, 0x0c, 0x30, 0xc0};

#define KSET_IS_EMPTY(flags, i) (flags[(i)/4] & kset_empty_mask[(i)%4])
#define KSET_IS_DEL(flags, i) (flags[(i)/4] & kset_del_mask[(i)%4])
#define KSET_IS_EITHER(flags, i) (flags[(i)/4] & kset_either_mask[(i)%4])

#define kset_power2(v) do { \
  v--; \
  v |= v >> 1; \
  v |= v >> 2; \
  v |= v >> 4; \
  v |= v >> 8; \
  v |= v >> 16; \
  v++; \
} while (0)

#define kset_mask(s) ((s)->n_buckets - 1)
#define kset_upper_bound(s) (KSET_UPPER_BOUND((s)->n_buckets))
#define kset_end(s) ((s)->n_buckets)

/* Compact set structure - exactly 3 pointers in size */
typedef struct kset {
  void *data;           /* Combined keys + flags memory block */
  kset_int_t n_buckets; /* Number of buckets (power of 2) */
  kset_int_t size;      /* Number of elements */
} kset_t;

/* Memory layout: [keys...][flags...] */
#define kset_keys(s) ((mrb_value*)(s)->data)
#define kset_flags(s) ((uint8_t*)((s)->data) + sizeof(mrb_value) * (s)->n_buckets)

/* Fill flags with pattern */
static inline void
kset_fill_flags(uint8_t *p, uint8_t c, size_t len)
{
  while (len-- > 0) {
    *p++ = c;
  }
}

/* Hash function for mrb_value */
static inline kset_int_t
kset_hash_value(mrb_state *mrb, mrb_value key)
{
  return (kset_int_t)mrb_obj_hash_code(mrb, key);
}

/* Equality function for mrb_value */
static inline mrb_bool
kset_equal_value(mrb_state *mrb, mrb_value a, mrb_value b)
{
  return mrb_eql(mrb, a, b);
}

/*
 * Inserts a key into the provided hash table arrays (keys and flags).
 * This function encapsulates the core logic of finding a slot and inserting the key.
 *
 * Parameters:
 *   mrb: mrb_state pointer
 *   key: mrb_value to insert
 *   keys_array: pointer to the keys array
 *   flags_array: pointer to the flags array
 *   n_buckets_val: number of buckets in the arrays
 *   size_ptr: pointer to the size counter, which is incremented on successful insertion of a new element
 *   ret_status: pointer to an int to store the status of the operation.
 *               - 0 if the key already exists.
 *               - 1 if the key was inserted into a new empty slot.
 *               - 2 if the key was inserted into a previously deleted slot.
 *               If NULL, status is not reported.
 *
 * Returns:
 *   The iterator (index) of the key in the keys_array.
 */
static inline kset_iter_t
kset_raw_put(mrb_state *mrb, mrb_value key, mrb_value *keys_array, uint8_t *flags_array,
             kset_int_t n_buckets_val, kset_int_t *size_ptr, int *ret_status)
{
  kset_int_t k, del_k, step = 0;
  kset_int_t mask = n_buckets_val - 1;

  k = kset_hash_value(mrb, key) & mask;
  del_k = n_buckets_val; /* Represents an invalid/not-found slot initially */

  while (!KSET_IS_EMPTY(flags_array, k)) {
    if (!KSET_IS_DEL(flags_array, k)) {
      if (kset_equal_value(mrb, keys_array[k], key)) {
        if (ret_status != NULL) { *ret_status = 0; } /* Key already exists */
        return k;
      }
    }
    else if (del_k == n_buckets_val) { /* Found a deleted slot, mark it if not already marked */
      del_k = k;
    }
    k = (k + (++step)) & mask;
  }

  if (del_k != n_buckets_val) {
    /* Use the previously found deleted slot */
    keys_array[del_k] = key;
    flags_array[del_k/4] &= ~kset_del_mask[del_k%4]; /* Clear only the deleted flag bit */
    (*size_ptr)++;
    if (ret_status != NULL) { *ret_status = 2; } /* Used deleted slot */
    return del_k;
  }
  else {
    /* Use the new empty slot found */
    keys_array[k] = key;
    flags_array[k/4] &= ~kset_empty_mask[k%4]; /* Clear only the empty flag bit */
    (*size_ptr)++;
    if (ret_status != NULL) { *ret_status = 1; } /* Used empty slot */
    return k;
  }
}

/* Convenience macros for common operations */
#define kset_is_uninitialized(s) (!(s)->data)
#define kset_is_empty(s) (!(s)->data || (s)->size == 0)

/* Macro for iterating over all elements in a kset */
#define KSET_FOREACH(s, k) \
  for (kset_iter_t k = 0; k != kset_end(s); k++) \
    if (kset_exist(s, k))

/* Initialize set with specific size */
static kset_t*
kset_init_size(mrb_state *mrb, kset_int_t size)
{
  kset_t *s = (kset_t*)mrb_calloc(mrb, 1, sizeof(kset_t));

  if (size < KSET_MIN_SIZE) {
    size = KSET_MIN_SIZE;
  }
  kset_power2(size);

  s->n_buckets = size;
  s->size = 0;

  /* Allocate combined memory block for keys and flags */
  size_t keys_size = sizeof(mrb_value) * size;
  size_t flags_size = size / 4;
  s->data = mrb_malloc(mrb, keys_size + flags_size);

  /* Initialize flags to empty (0xaa pattern) */
  kset_fill_flags(kset_flags(s), 0xaa, flags_size);

  return s;
}

/* Initialize empty set */
static kset_t*
kset_init(mrb_state *mrb)
{
  return kset_init_size(mrb, KSET_DEFAULT_SIZE);
}

/* Destroy set */
static void
kset_destroy(mrb_state *mrb, kset_t *s)
{
  if (s) {
    if (s->data) {
      mrb_free(mrb, s->data);
    }
    mrb_free(mrb, s);
  }
}

/* Clear set */
static void
kset_clear(mrb_state *mrb, kset_t *s)
{
  (void)mrb;
  if (s && s->data) {
    kset_fill_flags(kset_flags(s), 0xaa, s->n_buckets / 4);
    s->size = 0;
  }
}

/* Find key in set */
static kset_iter_t
kset_get(mrb_state *mrb, kset_t *s, mrb_value key)
{
  kset_int_t k = kset_hash_value(mrb, key) & kset_mask(s);
  kset_int_t step = 0;
  uint8_t *flags = kset_flags(s);
  mrb_value *keys = kset_keys(s);

  while (!KSET_IS_EMPTY(flags, k)) {
    if (!KSET_IS_DEL(flags, k)) {
      if (kset_equal_value(mrb, keys[k], key)) {
        return k;
      }
    }
    k = (k + (++step)) & kset_mask(s);
  }
  return kset_end(s);
}

/* Resize set */
static void
kset_resize(mrb_state *mrb, kset_t *s, kset_int_t new_n_buckets)
{
  if (new_n_buckets < KSET_MIN_SIZE) {
    new_n_buckets = KSET_MIN_SIZE;
  }
  kset_power2(new_n_buckets);

  if (s->n_buckets == new_n_buckets) return; /* No change needed */

  /* Save old data references */
  void *old_data_ptr = s->data;
  kset_int_t old_n_buckets = s->n_buckets;
  mrb_value *old_keys = (mrb_value*)old_data_ptr; /* Equivalent to kset_keys(s) before s->data is changed */
  uint8_t *old_flags = (uint8_t*)old_data_ptr + sizeof(mrb_value) * old_n_buckets; /* Equivalent to kset_flags(s) */

  /* Allocate new data block */
  size_t new_keys_bytes = sizeof(mrb_value) * new_n_buckets;
  size_t new_flags_bytes = new_n_buckets / 4;
  void *new_data_ptr = mrb_malloc(mrb, new_keys_bytes + new_flags_bytes);

  mrb_value *new_keys = (mrb_value*)new_data_ptr;
  uint8_t *new_flags = (uint8_t*)new_data_ptr + new_keys_bytes;

  /* Initialize new flags to empty (0xaa pattern) */
  kset_fill_flags(new_flags, 0xaa, new_flags_bytes);

  kset_int_t new_size = 0;
  kset_iter_t dummy_iter; /* kset_raw_put requires an iterator, but it's not used here */

  /* Rehash old elements into the new data arrays */
  /* Iterate only if old_data_ptr is valid (set was not empty/uninitialized) */
  if (old_data_ptr) {
    for (kset_int_t i = 0; i < old_n_buckets; i++) {
      if (!KSET_IS_EITHER(old_flags, i)) {
        /* Use kset_raw_put to insert the key into new_keys and new_flags */
        /* Pass NULL for ret_status as kset_resize doesn't use the status */
        dummy_iter = kset_raw_put(mrb, old_keys[i], new_keys, new_flags, new_n_buckets, &new_size, NULL);
      }
    }
  }
  (void)dummy_iter; /* Mark as intentionally unused to suppress warning */

  /* Free the old data block */
  if (old_data_ptr) {
    mrb_free(mrb, old_data_ptr);
  }

  /* Update the set structure with the new data block and properties */
  s->data = new_data_ptr;
  s->n_buckets = new_n_buckets;
  s->size = new_size;
}

/* Resize set (rehash with current bucket size, mainly for re-compacting deleted slots) */
static void
kset_rehash(mrb_state *mrb, kset_t *s)
{
  kset_resize(mrb, s, s->n_buckets);
}

/* Add key to set with return status */
static kset_iter_t
kset_put2(mrb_state *mrb, kset_t *s, mrb_value key, int *ret)
{
  kset_iter_t result_iter;

  if (s->size >= kset_upper_bound(s)) {
    kset_resize(mrb, s, s->n_buckets * 2);
  }

  /* Use the kset_raw_put function to handle the insertion logic */
  result_iter = kset_raw_put(mrb, key, kset_keys(s), kset_flags(s), s->n_buckets, &s->size, ret);

  return result_iter;
}

/* Add key to set */
static kset_iter_t
kset_put(mrb_state *mrb, kset_t *s, mrb_value key)
{
  return kset_put2(mrb, s, key, NULL);
}

/* Delete key from set */
static void
kset_del(mrb_state *mrb, kset_t *s, kset_iter_t x)
{
  (void)mrb;
  mrb_assert(x != s->n_buckets && !KSET_IS_EITHER(kset_flags(s), x));
  kset_flags(s)[x/4] |= kset_del_mask[x%4];
  s->size--;
}

/* Check if iterator exists */
static inline mrb_bool
kset_exist(kset_t *s, kset_iter_t x)
{
  return !KSET_IS_EITHER(kset_flags(s), x);
}

/* Get key at iterator */
static inline mrb_value
kset_key(kset_t *s, kset_iter_t x)
{
  return kset_keys(s)[x];
}

/* Initialize embedded set */
static void
kset_init_embedded(mrb_state *mrb, kset_t *s)
{
  kset_int_t size = KSET_DEFAULT_SIZE;
  if (size < KSET_MIN_SIZE) {
    size = KSET_MIN_SIZE;
  }
  kset_power2(size);

  s->n_buckets = size;
  s->size = 0;

  /* Allocate combined memory block for keys and flags */
  size_t keys_size = sizeof(mrb_value) * size;
  size_t flags_size = size / 4;
  s->data = mrb_malloc(mrb, keys_size + flags_size);

  /* Initialize flags to empty (0xaa pattern) */
  kset_fill_flags(kset_flags(s), 0xaa, flags_size);
}

/* Destroy embedded set */
static void
kset_destroy_embedded(mrb_state *mrb, kset_t *s)
{
  if (s && s->data) {
    mrb_free(mrb, s->data);
    s->data = NULL;
    s->n_buckets = 0;
    s->size = 0;
  }
}

/* Copy elements from one set to another */
static void
kset_copy_elements(mrb_state *mrb, kset_t *target, kset_t *source)
{
  if (!source || !target) return;

  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(source, k) {
    kset_put(mrb, target, kset_key(source, k));
    mrb_gc_arena_restore(mrb, ai);
  }
}

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
  kset_destroy_embedded(mrb, &s->set);
}

size_t
mrb_set_memsize(mrb_value set)
{
  size_t size = mrb_objspace_page_slot_size();
  struct RSet *s = mrb_set_ptr(set);
  kset_t *kset = &s->set;
  if (kset->data) {
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
  kset_init_embedded(mrb, set);
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
  kset_init_embedded(mrb, self_set);
  kset_copy_elements(mrb, self_set, orig_set);

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
  return mrb_fixnum_value(set->size);
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

  mrb_value ary = mrb_ary_new_capa(mrb, set->size);

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

  kset_iter_t k = kset_get(mrb, set, obj);
  return mrb_bool_value(k != kset_end(set));
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
  if (k != kset_end(set)) {
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
  if (k != kset_end(set)) {
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
    kset_copy_elements(mrb, self_set, other_set);
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
    if (self_k != kset_end(self_set)) {
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
  if (!result_set->data) {
    /* If self is empty, initialize the set */
    kset_init_embedded(mrb, result_set);
  }

  /* Add all elements from other set */
  kset_t *other_set = set_get_kset(mrb, other);
  if (other_set->data) {
    kset_copy_elements(mrb, result_set, other_set);
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
  if (!result_set->data) {
    /* If self is empty, return an empty set */
    return result;
  }

  /* Remove all elements that are in other set */
  kset_t *other_set = set_get_kset(mrb, other);
  if (other_set->data) {
    KSET_FOREACH(other_set, k) {
      mrb_value key = kset_key(other_set, k);
      kset_iter_t result_k = kset_get(mrb, result_set, key);
      if (result_k != kset_end(result_set)) {
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
  if (!self_set->data) return result;

  kset_t *other_set = set_get_kset(mrb, other);
  if (!other_set->data) return result;

  KSET_FOREACH(other_set, k) {
    mrb_value key = kset_key(other_set, k);
    kset_iter_t self_k = kset_get(mrb, self_set, key);

    /* If key exists in self, add it to result */
    if (self_k != kset_end(self_set)) {
      kset_put(mrb, result_set, key);
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
      kset_copy_elements(mrb, result_set, other_set);
    }
    return result;
  }
  if (kset_is_empty(other_set)) {
    kset_copy_elements(mrb, result_set, self_set);
    return result;
  }

  /* Add elements from self that are not in other */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    mrb_value key = kset_key(self_set, k);
    kset_iter_t other_k = kset_get(mrb, other_set, key);

    /* Add to result if not in other */
    if (other_k == kset_end(other_set)) {
      kset_put(mrb, result_set, key);
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  /* Add elements from other that are not in self */
  KSET_FOREACH(other_set, k) {
    mrb_value key = kset_key(other_set, k);
    kset_iter_t self_k = kset_get(mrb, self_set, key);

    /* Add to result if not in self */
    if (self_k == kset_end(self_set)) {
      kset_put(mrb, result_set, key);
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
  if ((!self_set->data || self_set->size == 0) && (!other_set->data || other_set->size == 0)) {
    return mrb_true_value();
  }

  /* Fast path: different sizes */
  if (!self_set->data || !other_set->data || self_set->size != other_set->size) {
    return mrb_false_value();
  }

  /* Compare elements: iterate through the smaller hash for efficiency */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    kset_iter_t k2 = kset_get(mrb, other_set, kset_key(self_set, k));
    if (k2 == kset_end(other_set)) {
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

  /* Use FNV-1a hash algorithm with better distribution properties */
  uint64_t hash = 0xcbf29ce484222325ULL; /* FNV offset basis */
  const uint64_t fnv_prime = 0x100000001b3ULL; /* FNV prime */

  /* Include the size of the set in the hash */
  size_t size = set ? set->size : 0;
  hash ^= size;
  hash *= fnv_prime;

  if (set->data && size > 0) {
    /* Process each element */
    int ai = mrb_gc_arena_save(mrb);
    KSET_FOREACH(set, k) {
      /* Get element's hash code */
      kset_int_t elem_hash = (kset_int_t)mrb_obj_hash_code(mrb, kset_key(set, k));

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

  if (!self_set->data) {
    return mrb_false_value(); /* Empty set is not a superset of a non-empty set */
  }

  /* Check size first - a superset must be at least as large as the subset */
  if (self_set->size < other_set->size) {
    return mrb_false_value();
  }

  /* Check if all elements in other are in self */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(other_set, k) {
    kset_iter_t self_k = kset_get(mrb, self_set, kset_key(other_set, k));
    if (self_k == kset_end(self_set)) {
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
    return self_set->data && self_set->size > 0 ? mrb_true_value() : mrb_false_value();
  }

  if (!self_set->data) {
    return mrb_false_value(); /* Empty set is not a proper superset of any set */
  }

  /* For a proper superset, self must be strictly larger than other */
  if (self_set->size <= other_set->size) {
    return mrb_false_value();
  }

  /* Check if all elements in other are in self */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(other_set, k) {
    kset_iter_t self_k = kset_get(mrb, self_set, kset_key(other_set, k));
    if (self_k == kset_end(self_set)) {
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

  if (!other_set->data) {
    return mrb_false_value(); /* Non-empty set is not a subset of an empty set */
  }

  /* Check size first - a subset cannot be larger than its superset */
  if (other_set->size < self_set->size) {
    return mrb_false_value();
  }

  /* Check if all elements in self are in other */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
    if (other_k == kset_end(other_set)) {
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
    return other_set->data && other_set->size > 0 ? mrb_true_value() : mrb_false_value();
  }

  if (!other_set->data) {
    return mrb_false_value(); /* Non-empty set is not a proper subset of an empty set */
  }

  /* For a proper subset, self must be strictly smaller than other */
  if (other_set->size <= self_set->size) {
    return mrb_false_value();
  }

  /* Check if all elements in self are in other */
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
    if (other_k == kset_end(other_set)) {
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
  if (self_set->size < other_set->size) {
    KSET_FOREACH(self_set, k) {
      kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
      if (other_k != kset_end(other_set)) {
        return mrb_true_value(); /* Found a common element */
      }
      mrb_gc_arena_restore(mrb, ai);
    }
  }
  else {
    KSET_FOREACH(other_set, k) {
      kset_iter_t self_k = kset_get(mrb, self_set, kset_key(other_set, k));
      if (self_k != kset_end(self_set)) {
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
  int size_cmp = self_set->size - other_set->size;

  if (size_cmp < 0) {
    /* self might be a proper subset of other */
    int ai = mrb_gc_arena_save(mrb);
    KSET_FOREACH(self_set, k) {
      kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
      if (other_k == kset_end(other_set)) {
        /* Not a subset */
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
    KSET_FOREACH(other_set, k) {
      kset_iter_t self_k = kset_get(mrb, self_set, kset_key(other_set, k));
      if (self_k == kset_end(self_set)) {
        /* Not a superset */
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
    KSET_FOREACH(self_set, k) {
      kset_iter_t other_k = kset_get(mrb, other_set, kset_key(self_set, k));
      if (other_k == kset_end(other_set)) {
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
  KSET_FOREACH(set, k) {
    if (!first) {
      mrb_str_cat(mrb, result, sep_ptr, sep_len);
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
  size_t size = set->size;
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
    /* Create a new set by copying the old one */
    kset_rehash(mrb, set);
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
    }
    mrb_gc_arena_restore(mrb, ai);
  }
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
  kset_t *self_set = set_get_kset(mrb, self);

  /* Fast path for empty sets */
  if (kset_is_empty(self_set)) {
    return mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  }

  /* Fast path: check if there are any nested sets */
  mrb_bool has_nested_sets = FALSE;
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    if (set_is_set(kset_key(self_set, k))) {
      has_nested_sets = TRUE;
      break;
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  /* If no nested sets, just return a duplicate */
  if (!has_nested_sets) {
    return mrb_obj_dup(mrb, self);
  }

  /* Create a new set of the same class */
  mrb_value result = mrb_obj_new(mrb, mrb_obj_class(mrb, self), 0, NULL);
  kset_t *result_set = set_get_kset(mrb, result);

  /* Track recursion depth */
  int seen_count = 0;

  /* Flatten the set */
  if (set_flatten_recursive(mrb, result_set, self_set, &seen_count) < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "flatten recursion depth too deep");
  }

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

  /* First, check if there are any nested sets */
  mrb_bool has_nested_sets = FALSE;
  int ai = mrb_gc_arena_save(mrb);
  KSET_FOREACH(self_set, k) {
    mrb_value elem = kset_key(self_set, k);
    if (set_is_set(elem)) {
      has_nested_sets = TRUE;
      break;
    }
    mrb_gc_arena_restore(mrb, ai);
  }

  if (!has_nested_sets) {
    return mrb_nil_value(); /* No nested sets, no changes needed */
  }

  /* Create a temporary set for the flattened result */
  kset_t *new_set = kset_init(mrb);

  /* Track recursion depth */
  int seen_count = 0;

  /* Flatten the set into the new set */
  if (set_flatten_recursive(mrb, new_set, self_set, &seen_count) < 0) {
    /* Clean up the new set if an error occurred */
    kset_destroy(mrb, new_set);

    /* Raise appropriate exception */
    mrb_raise(mrb, E_ARGUMENT_ERROR, "flatten recursion depth too deep");
  }

  /* Replace the old data with the new one */
  kset_destroy_embedded(mrb, self_set);
  *self_set = *new_set;
  mrb_free(mrb, new_set);

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
  if (!ks->data) return self;

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < argc; i++) {
    kset_iter_t k = kset_get(mrb, ks, argv[i]);
    if (k != kset_end(ks)) {
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
  if (!ks->data) return mrb_false_value();

  for (mrb_int i = 0; i < argc; i++) {
    kset_iter_t k = kset_get(mrb, ks, argv[i]);
    if (k == kset_end(ks)) {
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
    if (k != kset_end(ks)) {
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
