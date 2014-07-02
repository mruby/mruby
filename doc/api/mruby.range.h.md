# mruby/range.h

## struct RRange
Subclass of RBasic to represent `Range` object.

### Fields.
* `mrb_range_edges *edges;`
  * Has `beg` and `end` field of type `mrb_value` in `struct mrb_range_edges` type.
* `mrb_bool excl;`
  * If true excludes `end`.

## Macros.

### mrb_range_ptr
```C
struct RRange *mrb_range_ptr(mrb_value v);
```
Macro to take `struct RRange*` from `mrb_value`.

### mrb_range_value
```C
mrb_value mrb_range_value(struct RRange* p);
```
Macro to create `mrb_value` from `struct RRange*`.

## mrb_range_new
```C
mrb_value mrb_range_new(mrb_state* mrb, mrb_value beg, mrb_value end, mrb_bool excl);
```
Creates `Range` object begins with `beg` and ends with `end`.
Excludes last value of range if `excl` is true.

## mrb_range_beg_len
```C
mrb_bool mrb_range_beg_len(mrb_state *mrb, mrb_value range, mrb_int *begp, mrb_int *lenp, mrb_int len);
```
Set begining index to `begp` and range length to `lenp` of `range`.
Returns `FALSE` if it failed to set values.
Value set to `lenp` is limited to value of `len`.

## mrb_get_values_at
```C
mrb_value mrb_get_values_at(mrb_state *mrb, mrb_value obj, mrb_int olen, mrb_int argc, const mrb_value *argv, mrb_value (*func)(mrb_state*, mrb_value, mrb_int));
```
C API for method `values_at`.
Returns array of objects selected from `obj` with selectors in `argv`.

`func` is function to get object in a index from `obj`.
`obj` is passed as 2nd argument and index is passed as 3rd argument.
