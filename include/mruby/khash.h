/**
** @file mruby/khash.h - Hash for mruby
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_KHASH_H
#define MRUBY_KHASH_H

#include <string.h>

#include <mruby.h>
#include "common.h"

/**
 * khash definitions used in mruby's hash table.
 */
MRB_BEGIN_DECL

typedef uint32_t khint_t;
typedef khint_t khiter_t;

#ifndef KHASH_DEFAULT_SIZE
# define KHASH_DEFAULT_SIZE 8
#endif
#define KHASH_MIN_SIZE 8
#define KHASH_SMALL_THRESHOLD 4

#define KH_UPPER_BOUND(x) ((x) - ((x)>>3))   /* 87.5% load factor */

/* extern uint8_t __m[]; */

/* mask for flags */
static const uint8_t __m_empty[]  = {0x02, 0x08, 0x20, 0x80};
static const uint8_t __m_del[]    = {0x01, 0x04, 0x10, 0x40};
static const uint8_t __m_either[] = {0x03, 0x0c, 0x30, 0xc0};


#define __ac_isempty(ed_flag, i) (ed_flag[(i)/4]&__m_empty[(i)%4])
#define __ac_isdel(ed_flag, i) (ed_flag[(i)/4]&__m_del[(i)%4])
#define __ac_iseither(ed_flag, i) (ed_flag[(i)/4]&__m_either[(i)%4])
#define khash_power2(v) do { \
  v--;\
  v |= v >> 1;\
  v |= v >> 2;\
  v |= v >> 4;\
  v |= v >> 8;\
  v |= v >> 16;\
  v++;\
} while (0)
#define khash_mask(h) ((h)->n_buckets-1)
#define khash_upper_bound(h) (KH_UPPER_BOUND((h)->n_buckets))

/* BREAKING CHANGE: khash structure optimized for 50% memory reduction
 *
 * The structure now uses a single data pointer instead of separate keys,
 * vals, and ed_flags pointers, reducing size from 32 to 16 bytes.
 *
 * MIGRATION REQUIRED for field access macros:
 * - OLD: kh_key(h, x)      NEW: kh_key(typename, h, x)
 * - OLD: kh_val(h, x)      NEW: kh_val(typename, h, x)
 * - OLD: kh_exist(h, x)    NEW: kh_exist(typename, h, x)
 * - OLD: KHASH_FOREACH()   NEW: KHASH_FOREACH(typename, ...)
 *
 * Function-style macros (kh_get, kh_put, etc.) remain unchanged.
 */

/* declare struct kh_xxx and kh_xxx_funcs

   name: hash name
   khkey_t: key data type
   khval_t: value data type
   kh_is_map: (0: hash set / 1: hash map)
*/
#define KHASH_DECLARE(name, khkey_t, khval_t, kh_is_map)                \
  typedef struct kh_##name {                                            \
    void *data;          /* Single allocation: [keys][vals][flags] */   \
    khint_t n_buckets;   /* Number of buckets (power of 2) */           \
    khint_t size;        /* Number of elements */                       \
  } kh_##name##_t;                                                      \
  /* Address calculation functions for optimized memory layout */       \
  static inline khkey_t* kh_keys_##name(const kh_##name##_t *h) {       \
    return (khkey_t*)(h)->data;                                         \
  }                                                                     \
  static inline khval_t* kh_vals_##name(const kh_##name##_t *h) {       \
    return kh_is_map ?                                                  \
      (khval_t*)((uint8_t*)(h)->data + sizeof(khkey_t) * (h)->n_buckets) : NULL; \
  }                                                                     \
  static inline uint8_t* kh_flags_##name(const kh_##name##_t *h) {      \
    return (uint8_t*)(h)->data +                                        \
      (sizeof(khkey_t) + (kh_is_map ? sizeof(khval_t) : 0)) * (h)->n_buckets; \
  }                                                                     \
  void kh_alloc_##name(mrb_state *mrb, kh_##name##_t *h);               \
  kh_##name##_t *kh_init_##name##_size(mrb_state *mrb, khint_t size);   \
  kh_##name##_t *kh_init_##name(mrb_state *mrb);                        \
  void kh_destroy_##name(mrb_state *mrb, kh_##name##_t *h);             \
  void kh_clear_##name(mrb_state *mrb, kh_##name##_t *h);               \
  khint_t kh_get_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key);           \
  khint_t kh_put_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key, int *ret); \
  void kh_resize_##name(mrb_state *mrb, kh_##name##_t *h, khint_t new_n_buckets); \
  void kh_del_##name(mrb_state *mrb, kh_##name##_t *h, khint_t x);                \
  kh_##name##_t *kh_copy_##name(mrb_state *mrb, kh_##name##_t *h);

/* define kh_xxx_funcs

   name: hash name
   khkey_t: key data type
   khval_t: value data type
   kh_is_map: (0: hash set / 1: hash map)
   __hash_func: hash function
   __hash_equal: hash comparison function
*/
#define KHASH_DEFINE(name, khkey_t, khval_t, kh_is_map, __hash_func, __hash_equal) \
  mrb_noreturn void mrb_raise_nomemory(mrb_state *mrb);                 \
  /* Small table optimization functions */                              \
  static inline int kh_is_small_##name(const kh_##name##_t *h) {        \
    return h->n_buckets == 0;  /* Small table marker */                 \
  }                                                                     \
  static inline khint_t kh_get_small_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key) { \
    khkey_t *keys = kh_keys_##name(h);                                  \
    for (khint_t i = 0; i < h->size; i++) {                             \
      if (__hash_equal(mrb, keys[i], key)) return i;                    \
    }                                                                   \
    return h->size;  /* Not found - return end position */              \
  }                                                                     \
  static inline khint_t kh_put_small_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key, int *ret) { \
    /* First check if key exists */                                     \
    khint_t pos = kh_get_small_##name(mrb, h, key);                     \
    if (pos < h->size) {                                                \
      if (ret) *ret = 0;  /* Key exists */                              \
      return pos;                                                       \
    }                                                                   \
    /* Check if we need to convert to hash table */                     \
    if (h->size >= KHASH_SMALL_THRESHOLD) {                             \
      /* Convert from small table to hash table (inlined) */            \
      khkey_t *old_keys = kh_keys_##name(h);                            \
      khval_t *old_vals = kh_vals_##name(h);                            \
      khint_t old_size = h->size;                                       \
      void *old_data = h->data;                                         \
      /* Allocate proper hash table */                                  \
      h->n_buckets = KHASH_MIN_SIZE;                                    \
      h->size = 0;                                                      \
      kh_alloc_##name(mrb, h);                                          \
      /* Rehash existing elements */                                    \
      for (khint_t i = 0; i < old_size; i++) {                          \
        khint_t k = kh_put_##name(mrb, h, old_keys[i], NULL);           \
        if (kh_is_map) {                                                \
          khval_t *new_vals = kh_vals_##name(h);                        \
          new_vals[k] = old_vals[i];                                    \
        }                                                               \
      }                                                                 \
      mrb_free(mrb, old_data);                                          \
      /* Now add the new key using regular hash table */                \
      return kh_put_##name(mrb, h, key, ret);                           \
    }                                                                   \
    /* Add new element to small table */                                \
    khkey_t *keys = kh_keys_##name(h);                                  \
    keys[h->size] = key;                                                \
    h->size++;                                                          \
    if (ret) *ret = 1;  /* New key */                                   \
    return h->size - 1;                                                 \
  }                                                                     \
  static inline int kh_alloc_small_##name(mrb_state *mrb, kh_##name##_t *h) { \
    size_t key_size = sizeof(khkey_t) * KHASH_SMALL_THRESHOLD;          \
    size_t val_size = kh_is_map ? sizeof(khval_t) * KHASH_SMALL_THRESHOLD : 0; \
    h->data = mrb_malloc_simple(mrb, key_size + val_size);              \
    if (!h->data) return 1;                                             \
    h->size = 0;                                                        \
    return 0;                                                           \
  }                                                                     \
  int kh_alloc_simple_##name(mrb_state *mrb, kh_##name##_t *h)          \
  {                                                                     \
    khint_t sz = h->n_buckets;                                          \
    size_t len = sizeof(khkey_t) + (kh_is_map ? sizeof(khval_t) : 0);   \
    uint8_t *p = (uint8_t*)mrb_malloc_simple(mrb, sizeof(uint8_t)*sz/4+len*sz); \
    if (!p) { return 1; }                                               \
    h->size = 0;                                                        \
    h->data = p;  /* Single data pointer for optimized layout */        \
    memset(kh_flags_##name(h), 0xaa, sz/4);                             \
    return 0;                                                           \
  }                                                                     \
  void kh_alloc_##name(mrb_state *mrb, kh_##name##_t *h)                \
  {                                                                     \
    if (kh_alloc_simple_##name(mrb, h)) {                               \
      mrb_raise_nomemory(mrb);                                          \
    }                                                                   \
  }                                                                     \
  kh_##name##_t *kh_init_##name##_size(mrb_state *mrb, khint_t size) {  \
    kh_##name##_t *h = (kh_##name##_t*)mrb_calloc(mrb, 1, sizeof(kh_##name##_t)); \
    if (size <= KHASH_SMALL_THRESHOLD) {                                \
      /* Start as small table */                                        \
      h->n_buckets = 0;  /* Small table marker */                       \
      if (kh_alloc_small_##name(mrb, h)) {                              \
        mrb_free(mrb, h);                                               \
        mrb_raise_nomemory(mrb);                                        \
      }                                                                 \
    } else {                                                            \
      /* Start as regular hash table */                                 \
      if (size < KHASH_MIN_SIZE)                                        \
        size = KHASH_MIN_SIZE;                                          \
      khash_power2(size);                                               \
      h->n_buckets = size;                                              \
      if (kh_alloc_simple_##name(mrb, h)) {                             \
        mrb_free(mrb, h);                                               \
        mrb_raise_nomemory(mrb);                                        \
      }                                                                 \
    }                                                                   \
    return h;                                                           \
  }                                                                     \
  kh_##name##_t *kh_init_##name(mrb_state *mrb) {                       \
    return kh_init_##name##_size(mrb, KHASH_DEFAULT_SIZE);              \
  }                                                                     \
  void kh_destroy_##name(mrb_state *mrb, kh_##name##_t *h)              \
  {                                                                     \
    if (h) {                                                            \
      mrb_free(mrb, h->data);  /* Free single data allocation */       \
      mrb_free(mrb, h);                                                 \
    }                                                                   \
  }                                                                     \
  void kh_clear_##name(mrb_state *mrb, kh_##name##_t *h)                \
  {                                                                     \
    (void)mrb;                                                          \
    if (h && h->data) {                                                 \
      memset(kh_flags_##name(h), 0xaa, h->n_buckets/4);                 \
      h->size = 0;                                                      \
    }                                                                   \
  }                                                                     \
  khint_t kh_get_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key)  \
  {                                                                     \
    if (kh_is_small_##name(h)) {                                        \
      return kh_get_small_##name(mrb, h, key);                          \
    }                                                                   \
    /* Cache calculated pointers for performance */                     \
    khkey_t *keys = kh_keys_##name(h);                                  \
    uint8_t *ed_flags = kh_flags_##name(h);                             \
    khint_t k = __hash_func(mrb,key) & khash_mask(h), step = 0;         \
    (void)mrb;                                                          \
    while (!__ac_isempty(ed_flags, k)) {                                \
      if (!__ac_isdel(ed_flags, k)) {                                   \
        if (__hash_equal(mrb, keys[k], key)) return k;                  \
      }                                                                 \
      k = (k+(++step)) & khash_mask(h);                                 \
    }                                                                   \
    return kh_end(h);                                                   \
  }                                                                     \
  void kh_resize_##name(mrb_state *mrb, kh_##name##_t *h, khint_t new_n_buckets) \
  {                                                                     \
    if (new_n_buckets < KHASH_MIN_SIZE)                                 \
      new_n_buckets = KHASH_MIN_SIZE;                                   \
    khash_power2(new_n_buckets);                                        \
    {                                                                   \
      kh_##name##_t hh;                                                 \
      /* Cache old data pointer and calculate addresses */              \
      void *old_data = h->data;                                         \
      khkey_t *old_keys = kh_keys_##name(h);                            \
      khval_t *old_vals = kh_vals_##name(h);                            \
      uint8_t *old_ed_flags = kh_flags_##name(h);                       \
      khint_t old_n_buckets = h->n_buckets;                             \
      khint_t i;                                                        \
      hh.n_buckets = new_n_buckets;                                     \
      kh_alloc_##name(mrb, &hh);                                        \
      /* relocate */                                                    \
      for (i=0; i<old_n_buckets; i++) {                                 \
        if (!__ac_iseither(old_ed_flags, i)) {                          \
          khint_t k = kh_put_##name(mrb, &hh, old_keys[i], NULL);       \
          if (kh_is_map) {                                              \
            khval_t *new_vals = kh_vals_##name(&hh);                    \
            new_vals[k] = old_vals[i];                                  \
          }                                                             \
        }                                                               \
      }                                                                 \
      /* copy hh to h */                                                \
      *h = hh;                                                          \
      mrb_free(mrb, old_data);  /* Free old data allocation */          \
    }                                                                   \
  }                                                                     \
  khint_t kh_put_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key, int *ret) \
  {                                                                     \
    if (kh_is_small_##name(h)) {                                        \
      return kh_put_small_##name(mrb, h, key, ret);                     \
    }                                                                   \
    khint_t k, del_k, step = 0;                                         \
    if (h->size >= khash_upper_bound(h)) {                              \
      kh_resize_##name(mrb, h, h->n_buckets*2);                         \
    }                                                                   \
    /* Cache calculated pointers for performance */                     \
    khkey_t *keys = kh_keys_##name(h);                                  \
    uint8_t *ed_flags = kh_flags_##name(h);                             \
    k = __hash_func(mrb,key) & khash_mask(h);                           \
    del_k = kh_end(h);                                                  \
    while (!__ac_isempty(ed_flags, k)) {                                \
      if (!__ac_isdel(ed_flags, k)) {                                   \
        if (__hash_equal(mrb, keys[k], key)) {                          \
          if (ret) *ret = 0;                                            \
          return k;                                                     \
        }                                                               \
      }                                                                 \
      else if (del_k == kh_end(h)) {                                    \
        del_k = k;                                                      \
      }                                                                 \
      k = (k+(++step)) & khash_mask(h);                                 \
    }                                                                   \
    if (del_k != kh_end(h)) {                                           \
      /* put at del */                                                  \
      keys[del_k] = key;                                                \
      ed_flags[del_k/4] &= ~__m_del[del_k%4];                           \
      h->size++;                                                        \
      if (ret) *ret = 2;                                                \
      return del_k;                                                     \
    }                                                                   \
    else {                                                              \
      /* put at empty */                                                \
      keys[k] = key;                                                    \
      ed_flags[k/4] &= ~__m_empty[k%4];                                 \
      h->size++;                                                        \
      if (ret) *ret = 1;                                                \
      return k;                                                         \
    }                                                                   \
  }                                                                     \
  void kh_del_##name(mrb_state *mrb, kh_##name##_t *h, khint_t x)       \
  {                                                                     \
    uint8_t *ed_flags = kh_flags_##name(h);                             \
    (void)mrb;                                                          \
    mrb_assert(x != h->n_buckets && !__ac_iseither(ed_flags, x));       \
    ed_flags[x/4] |= __m_del[x%4];                                      \
    h->size--;                                                          \
  }                                                                     \
  kh_##name##_t *kh_copy_##name(mrb_state *mrb, kh_##name##_t *h)       \
  {                                                                     \
    kh_##name##_t *h2;                                                  \
    khiter_t k, k2;                                                     \
    /* Cache source hash addresses */                                   \
    khkey_t *keys = kh_keys_##name(h);                                  \
    khval_t *vals = kh_vals_##name(h);                                  \
    uint8_t *ed_flags = kh_flags_##name(h);                             \
                                                                        \
    h2 = kh_init_##name(mrb);                                           \
    for (k = kh_begin(h); k != kh_end(h); k++) {                        \
      if (!__ac_iseither(ed_flags, k)) {                                \
        k2 = kh_put_##name(mrb, h2, keys[k], NULL);                     \
        if (kh_is_map) {                                                \
          khval_t *new_vals = kh_vals_##name(h2);                       \
          new_vals[k2] = vals[k];                                       \
        }                                                               \
      }                                                                 \
    }                                                                   \
    return h2;                                                          \
  }


#define khash_t(name) kh_##name##_t

#define kh_init_size(name,mrb,size) kh_init_##name##_size(mrb,size)
#define kh_init(name,mrb) kh_init_##name(mrb)
#define kh_destroy(name, mrb, h) kh_destroy_##name(mrb, h)
#define kh_clear(name, mrb, h) kh_clear_##name(mrb, h)
#define kh_resize(name, mrb, h, s) kh_resize_##name(mrb, h, s)
#define kh_put(name, mrb, h, k) kh_put_##name(mrb, h, k, NULL)
#define kh_put2(name, mrb, h, k, r) kh_put_##name(mrb, h, k, r)
#define kh_get(name, mrb, h, k) kh_get_##name(mrb, h, k)
#define kh_del(name, mrb, h, k) kh_del_##name(mrb, h, k)
#define kh_copy(name, mrb, h) kh_copy_##name(mrb, h)

/* BREAKING CHANGE: Field access macros now require type name as first parameter
 * The macros keep their familiar names but now need the hash type name.
 *
 * MIGRATION: Add type name as first parameter:
 *   kh_key(h, x)      -> kh_key(typename, h, x)
 *   kh_val(h, x)      -> kh_val(typename, h, x)
 *   kh_exist(h, x)    -> kh_exist(typename, h, x)
 *   kh_value(h, x)    -> kh_value(typename, h, x)
 */

/* Type-aware access macros - same names, now with type parameter */
#define kh_exist(name, h, x) (!__ac_iseither(kh_flags_##name(h), (x)))
#define kh_key(name, h, x) (kh_keys_##name(h)[x])
#define kh_val(name, h, x) (kh_vals_##name(h)[x])
#define kh_value(name, h, x) (kh_vals_##name(h)[x])
#define kh_begin(h) (khint_t)(0)
#define kh_end(h) ((h)->n_buckets == 0 ? (h)->size : (h)->n_buckets)
#define kh_size(h) ((h)->size)
#define kh_n_buckets(h) ((h)->n_buckets)

#define kh_int_hash_func(mrb,key) mrb_int_hash_func(mrb,key)
#define kh_int_hash_equal(mrb,a, b) (a == b)
#define kh_int64_hash_func(mrb,key) (khint_t)((key)>>33^(key)^(key)<<11)
#define kh_int64_hash_equal(mrb,a, b) (a == b)
static inline khint_t __ac_X31_hash_string(const char *s)
{
    khint_t h = *s;
    if (h) for (++s; *s; ++s) h = (h << 5) - h + *s;
    return h;
}
#define kh_str_hash_func(mrb,key) __ac_X31_hash_string(key)
#define kh_str_hash_equal(mrb,a, b) (strcmp(a, b) == 0)

typedef const char *kh_cstr_t;

MRB_END_DECL

/**
 * Macro for iterating over all elements in a khash.
 *
 * Usage:
 *   KHASH_FOREACH(mrb, kh, k) {
 *     // k is the khiter_t iterator
 *     // Access the key with kh_key(kh, k)
 *     // Access the value with kh_val(kh, k) if applicable
 *     // Your code here
 *   }
 *
 * @param mrb The mrb_state
 * @param kh  The khash to iterate over
 * @param k   The name to use for the khiter_t iterator variable
 */
/* BREAKING CHANGE: KHASH_FOREACH now requires type name as first parameter
 * OLD: KHASH_FOREACH(mrb, kh, k)
 * NEW: KHASH_FOREACH(name, mrb, kh, k)
 */
#define KHASH_FOREACH(name, mrb, kh, k) \
  if (kh) \
    for (khiter_t k = kh_begin(kh); k != kh_end(kh); k++) \
      if (kh_exist(name, kh, k))

#endif  /* MRUBY_KHASH_H */
