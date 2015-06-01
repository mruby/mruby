# mruby/khash.h
Template to create hash table.

## KHASH_MIN_SIZE
Macro defining minimum size of hash table.

## KHASH_DEFAULT_SIZE
Macro defining default size of hash table.
See mrbconf for detail.

## KHASH_DECLARE
```C
#define KHASH_DECLARE(identifier, key type, value type, mrb_bool is_map) \
  // declares
```
Declares hash table types and functions.
(`name` is the identifier of declare.)

If 4th argument is `TRUE` hash table would be treated as hash map, otherwise hash set.

Defined types:
```C
struct kh_##name##_t;
```

Defined functions:

```C
kh_##name##_t *kh_init_##name##_size(mrb_state *mrb, khint_t size);
```
Creates hash table with size `size`.

```C
kh_##name##_t *kh_init_##name(mrb_state *mrb);
```
Creates hash table with default size.

```C
void kh_destroy_##name(mrb_state *mrb, kh_##name##_t *h);
```
Destroys hash table `h`.

```C
void kh_clear_##name(mrb_state *mrb, kh_##name##_t *h);
```
Clears all keys in hash table `h`.

```C
khint_t kh_get_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key);
```
Gets iterator of `key` in hash table `h`.
Returns `kh_end(h)` if `key` doesn't exist in `h`.

```C
khint_t kh_put_##name(mrb_state *mrb, kh_##name##_t *h, khkey_t key, int *ret);
```
Gets iterator of `key` in hash table `h`.
The variable pointed by `ret` would be non-zero if position is created.

```C
void kh_resize_##name(mrb_state *mrb, kh_##name##_t *h, khint_t new_n_buckets);
```
Resizes hash table `h` to `new_n_buckets`.

```C
void kh_del_##name(mrb_state *mrb, kh_##name##_t *h, khint_t x);
```
Deletes value of `x` in hash table `h`.

```C
kh_##name##_t *kh_copy_##name(mrb_state *mrb, kh_##name##_t *h);
```
Creates a copy of hash table `h`.

## KHASH_DEFINE
```C
#define KHASH_DEFINE(identifier, key type, value type, mrb_bool is_map, hash function, hash compare function) \
  // function definition of hash table
```
Arguments from 1st to 4th is same as `KHASH_DECLARE`.

5th argument takes function like macro name that is in a form like:
```C
khint_t hash_func(mrb_state *mrb, khkey_t key);
```

6th argument takes function like macro name that is in a form like:
```C
mrb_bool hash_cmp_func(mrb_state *mrb, khkey_t a, khkey_t b);
```

## khash_t(name)
Expands to hash table type name.

## kh_init_size
```C
khash_t(name)* kh_init_size(name, mrb_state *mrb, khint_t size);
```
Creates hash table of `name` with initial size `size`.

## kh_init
```C
khash_t(name)* kh_init(name, mrb_state *mrb);
```
Creates hash table of `name` with default initial size.

## kh_destroy
```C
void kh_destroy(name, mrb_state *mrb, khash_t(name)* h);
```
Destroys hash table `h`.

## kh_clear
```C
void kh_clear(name, mrb_state *mrb, khash_t(name) *h);
```
Clears all keys from hash table `h`.

## kh_resize
```C
void kh_resize(name, mrb_state *mrb, khash_t(name)* h, khint_t size);
```
Resizes hash table `h` to `size`.

## kh_put
```C
void kh_put(name, mrb_state *mrb, khash_t(name)* h, khkey_t key);
```
Creates `key` if it doesn't exist in hash table `h` and returns iterator.

## kh_put2
```C
khint_t kh_put2(name, mrb_state *mrb, khash_t(name)* h, khkey_t key, int *result);
```
Creates `key` if it doesn't exit in hash table `h` and returns iterator.
If a `key` is created variable pointed by `result` would be `TRUE`.

## kh_get
```C
khint_t kh_get(name, mrb_state *mrb, khash_t(name)* h, khkey_t key);
```
Gets iterator of `key` in hash table `h`.
If not found return `kh_end(name)`.

## kh_del
```C
void kh_del(name, mrb_state *mrb, khash_t(name) *h, khkey_t key);
```
Deletes `key` in hash table `h`.

## kh_copy
```C
khash_t(name) *kh_copy(name, mrb_state *mrb, khash_t(name) *h);
```
Creates copy of hash table `h`.

## kh_exist
```C
mrb_bool kh_exist(khash_t *h, khint_t x);
```
Checks whether `x` is a existing iterator.

## kh_key
```C
khkey_t kh_key(khash_t *h, khint_t x);
```
Returns key of iterator `x`.
Iterator `x` must be a existing iterator.

## kh_val
```C
khval_t& kh_value(khash_t *h, khint_t x);
```
Returns reference to value of iterator `x`.
Iterator `x` must be a existing iterator.

## kh_begin
```C
khint_t kh_begin(khash_t *h);
```
Returns beginning iterator of hash table `h`.

## kh_end
```C
khint_t kh_end(khash_t *h);
```
Returns ending iterator of hash table `h`.

## kh_size
```C
khint_t kh_size(khash_t *h);
```
Returns size of hash table `h`.

## kh_n_buckets
```C
khint_t kh_n_buckets(khash_t *h);
```
Returns number of buckets in hash table `h`.

## kh_int_hash_func
Function like hash macro for `int` keys.

## kh_int_hash_equal
Function like key compare macro for `int` keys.

## kh_int64_hash_func
Function like hash macro for `int64_t` keys.

## kh_int64_hash_equal
Function like key compare macro for `int64_t` keys.

## kh_str_hash_func
Function like hash macro for string keys.

## kh_str_hash_equal
Function like key compare macro for string keys.
