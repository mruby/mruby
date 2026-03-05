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

#ifndef KHASH_INITIAL_SIZE
# define KHASH_INITIAL_SIZE 8
#endif
#define KHASH_MIN_SIZE 8
#define KHASH_SMALL_LIMIT 4

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
    return (uint8_t*)(h)->data + sizeof(khkey_t) * (h)->n_buckets +     \
           (kh_is_map ? sizeof(khval_t) * (h)->n_buckets : 0);          \
  }                                                                     \
  kh_##name##_t *kh_init_##name##_size(mrb_state *mrb, khint_t size);   \
  kh_##name##_t *kh_init_##name(mrb_state *mrb);                        \
  void kh_destroy_##name(mrb_state *mrb, kh_##name##_t *h);             \
  void kh_clear_##name(mrb_state *mrb, kh_##name##_t *h);               \
  khint_t kh_get_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key);           \
  khint_t kh_put_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key, int *ret); \
  void kh_resize_##name(mrb_state *mrb, kh_##name##_t *h, khint_t new_n_buckets); \
  void kh_del_##name(mrb_state *mrb, kh_##name##_t *h, khint_t x);                \
  kh_##name##_t *kh_copy_##name(mrb_state *mrb, kh_##name##_t *h);                \
  void kh_init_data_##name(mrb_state *mrb, kh_##name##_t *h, khint_t size);       \
  void kh_destroy_data_##name(mrb_state *mrb, kh_##name##_t *h);                  \
  void kh_replace_##name(mrb_state *mrb, kh_##name##_t *dst, const kh_##name##_t *src);

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
  /* Internal helper functions */                                       \
  static inline size_t kh__kv_size_##name(khint_t count) {              \
    return sizeof(khkey_t) * count +                                    \
           (kh_is_map ? sizeof(khval_t) * count : 0);                   \
  }                                                                     \
  static inline size_t kh__htable_size_##name(khint_t n_buckets) {      \
    return kh__kv_size_##name(n_buckets) + n_buckets / 4;               \
  }                                                                     \
  static inline void kh__mark_occupied_##name(kh_##name##_t *h, khint_t i) { \
    uint8_t *flags = kh_flags_##name(h);                                \
    flags[i/4] &= ~__m_either[i%4];  /* Clear both empty and deleted bits */ \
  }                                                                     \
  static inline void kh__mark_deleted_##name(kh_##name##_t *h, khint_t i) {  \
    uint8_t *flags = kh_flags_##name(h);                                \
    flags[i/4] |= __m_del[i%4];      /* Set deleted bit */              \
  }                                                                     \
  static inline khint_t kh__key_idx_##name(mrb_state *mrb, khkey_t key, kh_##name##_t *h) { \
    return __hash_func(mrb, key) & khash_mask(h);                       \
  }                                                                     \
  static inline khint_t kh__next_probe_##name(khint_t k, khint_t *step, kh_##name##_t *h) { \
    return (k+(++(*step))) & khash_mask(h);                             \
  }                                                                     \
  static inline khint_t kh__insert_key_##name(kh_##name##_t *h, khint_t index, khkey_t key) { \
    khkey_t *keys = kh_keys_##name(h);                                  \
    keys[index] = key;                                                  \
    kh__mark_occupied_##name(h, index);                                 \
    h->size++;                                                          \
    return index;                                                       \
  }                                                                     \
  static inline void kh__clear_flags_##name(kh_##name##_t *h, khint_t n_buckets) { \
    memset(kh_flags_##name(h), 0xaa, n_buckets/4);                      \
  }                                                                     \
  static inline void kh__alloc_##name(mrb_state *mrb, kh_##name##_t *h) { \
    khint_t sz = h->n_buckets;                                          \
    uint8_t *p = (uint8_t*)mrb_malloc(mrb, kh__htable_size_##name(sz)); \
    h->size = 0;                                                        \
    h->data = p;  /* Single data pointer for optimized layout */        \
    kh__clear_flags_##name(h, sz);                                      \
  }                                                                     \
  /* Small table optimization functions */                              \
  static inline int kh__is_small_##name(const kh_##name##_t *h) {       \
    return h->n_buckets == 0;  /* Small table marker */                 \
  }                                                                     \
  static inline khint_t kh__get_small_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key) { \
    khkey_t *keys = kh_keys_##name(h);                                  \
    for (khint_t i = 0; i < h->size; i++) {                             \
      if (__hash_equal(mrb, keys[i], key)) return i;                    \
    }                                                                   \
    return h->size;  /* Not found - return end position */              \
  }                                                                     \
  static inline void kh__rebuild_##name(mrb_state *mrb, kh_##name##_t *h, khint_t new_n_buckets) { \
    kh_##name##_t hh;                                                   \
    hh.data = NULL;                                                     \
    hh.size = 0;                                                        \
    kh_init_data_##name(mrb, &hh, new_n_buckets);                       \
    /* Rehash from old 'h' to 'hh' */                                   \
    khkey_t *old_keys = kh_keys_##name(h);                              \
    khval_t *old_vals = kh_vals_##name(h);                              \
    uint8_t *old_flags = kh__is_small_##name(h) ? NULL : kh_flags_##name(h); \
    khint_t limit = old_flags ? h->n_buckets : h->size;                 \
    for (khint_t i = 0; i < limit; i++) {                               \
      if (old_flags && __ac_iseither(old_flags, i)) continue;           \
      khint_t k = kh_put_##name(mrb, &hh, old_keys[i], NULL);           \
      if (kh_is_map) {                                                  \
        kh_val(name, &hh, k) = old_vals[i];                             \
      }                                                                 \
    }                                                                   \
    /* Final Swap */                                                    \
    mrb_free(mrb, h->data);                                             \
    h->data = hh.data;                                                  \
    h->n_buckets = hh.n_buckets;                                        \
    h->size = hh.size;                                                  \
  }                                                                     \
  static inline khint_t kh__put_small_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key, int *ret) { \
    /* First check if key exists */                                     \
    khint_t pos = kh__get_small_##name(mrb, h, key);                    \
    if (pos < h->size) {                                                \
      if (ret) *ret = 0;  /* Key exists */                              \
      return pos;                                                       \
    }                                                                   \
    /* Check if we need to convert to hash table */                     \
    if (h->size >= KHASH_SMALL_LIMIT) {                                 \
      /* Convert from small table to hash table */                      \
      kh__rebuild_##name(mrb, h, KHASH_MIN_SIZE);                       \
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
  kh_##name##_t *kh_init_##name##_size(mrb_state *mrb, khint_t size) {  \
    kh_##name##_t *h = (kh_##name##_t*)mrb_calloc(mrb, 1, sizeof(kh_##name##_t)); \
    kh_init_data_##name(mrb, h, size);                                  \
    return h;                                                           \
  }                                                                     \
  kh_##name##_t *kh_init_##name(mrb_state *mrb) {                       \
    return kh_init_##name##_size(mrb, KHASH_INITIAL_SIZE);              \
  }                                                                     \
  void kh_destroy_##name(mrb_state *mrb, kh_##name##_t *h)              \
  {                                                                     \
    kh_destroy_data_##name(mrb, h);                                     \
    mrb_free(mrb, h);                                                   \
  }                                                                     \
  void kh_clear_##name(mrb_state *mrb, kh_##name##_t *h)                \
  {                                                                     \
    (void)mrb;                                                          \
    if (h && h->data) {                                                 \
      kh__clear_flags_##name(h, h->n_buckets);                          \
      h->size = 0;                                                      \
    }                                                                   \
  }                                                                     \
  khint_t kh_get_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key)  \
  {                                                                     \
    if (kh__is_small_##name(h)) {                                       \
      return kh__get_small_##name(mrb, h, key);                         \
    }                                                                   \
    /* Cache calculated pointers for performance */                     \
    khkey_t *keys = kh_keys_##name(h);                                  \
    uint8_t *ed_flags = kh_flags_##name(h);                             \
    khint_t k = kh__key_idx_##name(mrb, key, h), step = 0;              \
    (void)mrb;                                                          \
    while (!__ac_isempty(ed_flags, k)) {                                \
      if (!__ac_isdel(ed_flags, k)) {                                   \
        if (__hash_equal(mrb, keys[k], key)) return k;                  \
      }                                                                 \
      k = kh__next_probe_##name(k, &step, h);                           \
    }                                                                   \
    return kh_end(h);                                                   \
  }                                                                     \
  void kh_resize_##name(mrb_state *mrb, kh_##name##_t *h, khint_t new_n_buckets) \
  {                                                                     \
    if (new_n_buckets < KHASH_MIN_SIZE)                                 \
      new_n_buckets = KHASH_MIN_SIZE;                                   \
    khash_power2(new_n_buckets);                                        \
    kh__rebuild_##name(mrb, h, new_n_buckets);                          \
  }                                                                     \
  khint_t kh_put_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key, int *ret) \
  {                                                                     \
    if (kh__is_small_##name(h)) {                                       \
      return kh__put_small_##name(mrb, h, key, ret);                    \
    }                                                                   \
    khint_t k, del_k, step = 0;                                         \
    if (h->size >= khash_upper_bound(h)) {                              \
      kh_resize_##name(mrb, h, h->n_buckets*2);                         \
    }                                                                   \
    /* Cache calculated pointers for performance */                     \
    khkey_t *keys = kh_keys_##name(h);                                  \
    uint8_t *ed_flags = kh_flags_##name(h);                             \
    k = kh__key_idx_##name(mrb, key, h);                                \
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
      k = kh__next_probe_##name(k, &step, h);                           \
    }                                                                   \
    if (del_k != kh_end(h)) {                                           \
      /* put at del */                                                  \
      kh__insert_key_##name(h, del_k, key);                             \
      if (ret) *ret = 2;                                                \
      return del_k;                                                     \
    }                                                                   \
    else {                                                              \
      /* put at empty */                                                \
      kh__insert_key_##name(h, k, key);                                 \
      if (ret) *ret = 1;                                                \
      return k;                                                         \
    }                                                                   \
  }                                                                     \
  void kh_del_##name(mrb_state *mrb, kh_##name##_t *h, khint_t x)       \
  {                                                                     \
    (void)mrb;                                                          \
    if (kh__is_small_##name(h)) {                                       \
      /* Small table deletion: shift elements down */                   \
      mrb_assert(x < h->size);                                          \
      khkey_t *keys = kh_keys_##name(h);                                \
      khval_t *vals = kh_vals_##name(h);                                \
      for (khint_t i = x; i < h->size - 1; i++) {                       \
        keys[i] = keys[i + 1];                                          \
        if (kh_is_map) vals[i] = vals[i + 1];                           \
      }                                                                 \
      h->size--;                                                        \
    }                                                                   \
    else {                                                              \
      /* Regular hash table deletion */                                 \
      mrb_assert(x != h->n_buckets && !__ac_iseither(kh_flags_##name(h), x)); \
      kh__mark_deleted_##name(h, x);                                    \
      h->size--;                                                        \
    }                                                                   \
  }                                                                     \
  kh_##name##_t *kh_copy_##name(mrb_state *mrb, kh_##name##_t *h)       \
  {                                                                     \
    kh_##name##_t *h2 = (kh_##name##_t*)mrb_calloc(mrb, 1, sizeof(kh_##name##_t)); \
    kh_replace_##name(mrb, h2, h);                                      \
    return h2;                                                          \
  }                                                                     \
  void kh_init_data_##name(mrb_state *mrb, kh_##name##_t *h, khint_t size) { \
    if (size <= KHASH_SMALL_LIMIT) {                                    \
      /* Start as small table */                                        \
      h->n_buckets = 0;  /* Small table marker */                       \
      h->data = mrb_malloc(mrb, kh__kv_size_##name(KHASH_SMALL_LIMIT)); \
      h->size = 0;                                                      \
    }                                                                   \
    else {                                                              \
      /* Start as regular hash table */                                 \
      if (size < KHASH_MIN_SIZE)                                        \
        size = KHASH_MIN_SIZE;                                          \
      khash_power2(size);                                               \
      h->n_buckets = size;                                              \
      kh__alloc_##name(mrb, h);                                         \
    }                                                                   \
  }                                                                     \
  void kh_destroy_data_##name(mrb_state *mrb, kh_##name##_t *h)         \
  {                                                                     \
    if (h && h->data) {                                                 \
      mrb_free(mrb, h->data);  /* Free only the data allocation */      \
      h->data = NULL;                                                   \
    }                                                                   \
  }                                                                     \
  void kh_replace_##name(mrb_state *mrb, kh_##name##_t *dst, const kh_##name##_t *src) \
  {                                                                     \
    if (!src || (src->n_buckets == 0 && src->size == 0)) {              \
      /* Empty source */                                                \
      kh_destroy_data_##name(mrb, dst);                                 \
      dst->data = NULL;                                                 \
      dst->n_buckets = 0;                                               \
      dst->size = 0;                                                    \
    }                                                                   \
    else if (src->n_buckets == 0) {                                     \
      /* Small table case */                                            \
      size_t data_size = kh__kv_size_##name(KHASH_SMALL_LIMIT);         \
      dst->data = mrb_realloc(mrb, dst->data, data_size);               \
      dst->size = src->size;                                            \
      dst->n_buckets = 0;                                               \
      /* Copy only the used portion of keys and values */               \
      size_t copy_size = kh__kv_size_##name(src->size);                 \
      memcpy(dst->data, src->data, copy_size);                          \
    }                                                                   \
    else {                                                              \
      /* Regular hash table case */                                     \
      size_t data_size = kh__htable_size_##name(src->n_buckets);        \
      dst->data = mrb_realloc(mrb, dst->data, data_size);               \
      dst->size = src->size;                                            \
      dst->n_buckets = src->n_buckets;                                  \
      /* Copy the entire data block: [keys][vals][flags] */             \
      memcpy(dst->data, src->data, data_size);                          \
    }                                                                   \
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
#define kh_init_data(name, mrb, h, size) kh_init_data_##name(mrb, h, size)
#define kh_destroy_data(name, mrb, h) kh_destroy_data_##name(mrb, h)
#define kh_replace(name, mrb, dst, src) kh_replace_##name(mrb, dst, src)

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
#define kh_exist(name, h, x) ((h)->n_buckets == 0 ? ((x) < (h)->size) : (!__ac_iseither(kh_flags_##name(h), (x))))
#define kh_key(name, h, x) (kh_keys_##name(h)[x])
#define kh_val(name, h, x) (kh_vals_##name(h)[x])
#define kh_value(name, h, x) (kh_vals_##name(h)[x])
#define kh_begin(h) (khint_t)(0)
#define kh_end(h) ((h)->n_buckets == 0 ? (h)->size : (h)->n_buckets)
#define kh_is_end(h, i) ((i) >= kh_end(h))
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
 *   KHASH_FOREACH(typename, kh, k) {
 *     // k is the khiter_t iterator
 *     // Access the key with kh_key(typename, kh, k)
 *     // Access the value with kh_val(typename, kh, k) if applicable
 *     // Your code here
 *   }
 *
 * @param name The hash type name
 * @param kh   The khash to iterate over
 * @param k    The name to use for the khiter_t iterator variable
 */
/* BREAKING CHANGE: KHASH_FOREACH now requires type name as first parameter
 * OLD: KHASH_FOREACH(mrb, kh, k)
 * NEW: KHASH_FOREACH(name, kh, k)
 */
#define KHASH_FOREACH(name, kh, k) \
  if (kh) \
    for (khiter_t k = kh_begin(kh); !kh_is_end(kh, k); k++) \
      if (kh_exist(name, kh, k))

#endif  /* MRUBY_KHASH_H */
