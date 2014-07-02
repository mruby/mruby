# mruby/array.h

## struct RArray
Subclass of `RBasic` that represents `Array` object.
`RArray` has **shared mode** to reduce copy.

## struct mrb_shared_array
Substance of shared array.
Access to it with `aux.shared`.

## Macros

### Getting `struct RArray*`.
```C
struct RArray *mrb_ary_ptr(mrb_value v);
struct RArray *RARRAY(mrb_value v);
```
Macro to take `struct RArray*` from `mrb_value`.
`v`'s type tag must be `MRB_TT_ARRAY`.

### mrb_ary_value
```C
mrb_value mrb_ary_value(struct RArray *ary);
```
Create `mrb_value` object from `struct RArray*`.

### RARRAY_LEN
```C
mrb_int RARRAY_LEN(mrb_value v);
```
Gets length of Array object.
`v`'s type tag must be `MRB_TT_ARRAY`.

### RARRAY_PTR
```C
mrb_value *RARRAY_PTR(mrb_value v);
```
Gets pointer to first element of Array.
`v`'s type tag must be `MRB_TT_ARRAY`.

## mrb_ary_modify
```C
void mrb_ary_modify(mrb_state* mrb, struct RArray *ary);
```
Make `ary` modifiable.
If `ary` isn't a shared Array it does nothing.

## mrb_ary_decref
```C
void mrb_ary_decref(mrb_state *mrb, mrb_shared_array *ary);
```
Decrement reference count of shared Array `ary`.
`ary` must be a shared Array.

## mrb_ary_new_capa
```C
mrb_value mrb_ary_new_capa(mrb_state *mrb, mrb_int capa);
```
Creates a new Array object with capacity `capa`.
If the array size you need is known in advance use this API to reduce allocation.
Raises `ArgumentError` when `capa` is too big.

## mrb_ary_new
```C
mrb_value mrb_ary_new(mrb_state *mrb);
```
Creates a new Array object with capacity `0`.

## mrb_ary_new_from_values
```C
mrb_value mrb_ary_new_from_values(mrb_state *mrb, mrb_int size, const mrb_value *vals);
```
Creates a new Array object from `mrb_value` array of `size`.

## mrb_assoc_new
```C
mrb_value mrb_assoc_new(mrb_state *mrb, mrb_value car, mrb_value cdr);
```
Creates a new Array object of length `2` with value `[car, cdr]`.

## mrb_ary_concat
```C
void mrb_ary_concat(mrb_state *mrb, mrb_value rhs, mrb_value lhs);
```
Concatenates `lhs` to `rhs`.
Type tag of `lhs` and `rhs` must be `MRB_TT_ARRAY`.

## mrb_ary_splat
```C
mrb_value mrb_ary_splat(mrb_state* mrb, mrb_value obj);
```
Splats `obj`.
* If `obj` is `Array` just returns `obj`.
* If `obj` has method `to_a` returns its result.
* Else returns a array with single element `obj`.

## mrb_ary_push
```C
void mrb_ary_push(mrb_state* mrb, mrb_value ary, mrb_value obj);
```
Pushes `obj` to Array object `ary`.
`ary`'s type tag must be `MRB_TT_ARRAY`.

## mrb_ary_pop
```C
mrb_value mrb_ary_pop(mrb_state *mrb, mrb_value ary);
```
Returns poped value from Array object `ary`.
If length of `ary` is `0` returns `nil` value instead.
`ary`'s type tag must be `MRB_TT_ARRAY`.

## mrb_ary_ref
```C
mrb_value mrb_ary_ref(mrb_state *mrb, mrb_value ary, mrb_int n);
```
Returns `n`th element of Array object `ary`.
If `n` is out of range [-(length of `ary`), (length of `ary`)) returns `nil` value instead.
`ary`'s type tag must be `MRB_TT_ARRAY`.

## mrb_ary_set
```C
mrb_ary_set(mrb_state *mrb, mrb_value ary, mrb_int n, mrb_value val);
```
Set value of index `n` in `ary` to `val`.
If `n` is larger than `ary` length it will expand.
Raises `IndexError` if `n` is smaller than minus of `ary` length.
`ary`'s type tag must be `MRB_TT_ARRAY`.

## mrb_ary_replace
```C
void mrb_ary_replace(mrb_state *mrb, mrb_value a, mrb_value b);
```
Replaces Array `a` with values of Array `b`.
Type tag of `a` and `b` must be `MRB_TT_ARRAY`.

## mrb_check_array_type
```C
mrb_value mrb_check_array_type(mrb_state *mrb, mrb_value ary);
```
Checks whether `ary` is an Array or convertable to Array.

## mrb_ary_unshift
```C
mrb_value mrb_ary_unshift(mrb_state *mrb, mrb_value ary, mrb_value item);
```
Inserts `item` to first position of Array object `ary`.
Type tag of `ary` must be `MRB_TT_ARRAY`.

## mrb_ary_entry
```C
mrb_value mrb_ary_entry(mrb_value ary, mrb_int offset);
```
Returns `n`th element of Array object `ary`.
If `n` is out of range [-(length of `ary`), (length of `ary`)) returns `nil` value instead.
`ary`'s type tag must be `MRB_TT_ARRAY`.

## mrb_ary_shift
```C
mrb_value mrb_ary_shift(mrb_state *mrb, mrb_value ary);
```
Removes and returns first value of `ary`.
If `ary` is empty return `nil` value instead.
`ary`'s type tag must be `MRB_TT_ARRAY`.

## mrb_ary_clear
```C
mrb_value mrb_ary_clear(mrb_state *mrb, mrb_value ary);
```
Clears all elements of `ary` and frees value of `RARRAY_PTR(ary)` too.

## mrb_ary_join
```C
mrb_value mrb_ary_join(mrb_state *mrb, mrb_value ary, mrb_value sep);
```
Concatenates elements and all sub-elements of Array object `ary` with separator `sep`.
Raises `ArgumentError` if recursive is found.

## mrb_ary_len
```C
static inline mrb_int mrb_ary_len(mrb_state *mrb, mrb_value ary);
```
Returns length of Array object `ary`.
Has `mrb_assert` check unlike `RARRAY_LEN`.
