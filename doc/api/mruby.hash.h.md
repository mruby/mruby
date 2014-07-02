# mruby/hash.h

## struct RHash
Subclass of `RBasic` to represent `Hash`.

## Macros.

### Getting `struct RHash*` from `mrb_value`
```C
struct RHash *mrb_hash_ptr(mrb_value v);
struct RHash *RHASH(mrb_state obj);
```
Takes `struct RHash*` from `mrb_value`.
`v`'s type tag must be `MRB_TT_HASH`.

### mrb_hash_value
```C
mrb_value mrb_hash_value(struct RHash* h);
```
Creates `mrb_value` from `struct RHash*`.

### RHASH_TBL
```C
struct kh_ht *RHASH_TBL(mrb_value v);
```
Takes `struct kh_ht*` from `mrb_value`.
Returns `NULL` if uninitialized.
`v`'s type tag must be `MRB_TT_HASH`.

### Getting if none values.
```C
mrb_value RHASH_IFNONE(mrb_value h);
mrb_value RHASH_PROCDEFAULT(mrb_value h);
```
Gets `ifnone` value of `h`.
Variable `mrb` with type `mrb_state*` must be defined.
`v`'s type tag must be `MRB_TT_HASH`.

### MRB_RHASH_PROCDEFAULT_P
```C
mrb_bool MRB_RHASH_PROCDEFAULT_P(mrb_value h);
```
Checks whether `h`'s default value is retrieved from proc.
`v`'s type tag must be `MRB_TT_HASH`.

## Creating Hash object.
```C
mrb_value mrb_hash_new(mrb_state *mrb);
mrb_value mrb_hash_new_capa(mrb_state* mrb, int capa);
```
Creates new `Hash` object.
In `mrb_hash_new_capa` creates hash with capacity `capa`.

## mrb_hash_set
```C
void mrb_hash_set(mrb_state *mrb, mrb_value hash, mrb_value key, mrb_value val);
```
Associates the `val` to key `key` in `hash`.
`hash`'s type tag must be `MRB_TT_HASH`.

## Getting associated value.
```C
mrb_value mrb_hash_get(mrb_state *mrb, mrb_value hash, mrb_value key);
mrb_value mrb_hash_fetch(mrb_state *mrb, mrb_value hash, mrb_value key, mrb_value def);
```
Returns value associated to key `key`.
In `mrb_hash_fetch` return value of `def` instead if `key` isn't found.
`hash`'s type tag must be `MRB_TT_HASH`.

## mrb_hash_delete_key
```C
mrb_value mrb_hash_delete_key(mrb_state *mrb, mrb_value hash, mrb_value key);
```
Deletes and returns value associated to `key` in `hash`.
Returns `nil` if `key` isn't found in `hash`.
`hash`'s type tag must be `MRB_TT_HASH`.

## mrb_hash_keys
```C
mrb_value mrb_hash_keys(mrb_state *mrb, mrb_value hash);
```
Returns all existing keys in `hash`.
`hash`'s type tag must be `MRB_TT_HASH`.

## mrb_check_hash_type
```C
mrb_value mrb_check_hash_type(mrb_state *mrb, mrb_value hash);
```
Converts `hash` to `Hash` object.
Returns `nil` if it isn't convertable.
`hash`'s type tag must be `MRB_TT_HASH`.

## mrb_hash_empty_p
```C
mrb_value mrb_hash_empty_p(mrb_state *mrb, mrb_value hash);
```
Returns `true` object if `hash` is empty.
Otherwise returns `false` object.
`hash`'s type tag must be `MRB_TT_HASH`.

## mrb_hash_clear
```C
mrb_value mrb_hash_clear(mrb_state *mrb, mrb_value hash);
```
Clear all keys in `hash`.
Returns value of `hash`.
`hash`'s type tag must be `MRB_TT_HASH`.

## mrb_hash_tbl
```C
struct kh_ht * mrb_hash_tbl(mrb_state *mrb, mrb_value hash);
```
Returns substance of `hash`.
Creates substance if `RHASH_TBL(hash)` is `NULL`.
`hash`'s type tag must be `MRB_TT_HASH`.

## mrb_gc_mark_hash
```C
void mrb_gc_mark_hash(mrb_state* mrb, struct RHash* h);
```
Marks `h`.

## mrb_gc_mark_hash_size
```C
mrb_gc_mark_hash_size(mrb_state *mrb, struct RHash *h);
```
Returns marking `mrb_value` count under `h`.
Usually `(size of hash) * 2` since there is key and value.

## mrb_gc_free_hash
```C
void mrb_gc_free_hash(mrb_state* mrb, struct RHash* h);
```
Frees `h`.
