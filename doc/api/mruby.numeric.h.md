# mruby/numeric.h
Declares functions for arithmetic calculations.

## mrb_flo_to_fixnum
```C
mrb_value mrb_flo_to_fixnum(mrb_state *mrb, mrb_value val);
```
Converts float object `val` to fixnum object.
Raises `TypeError` when type tag of `val` isn't `MRB_TT_FLOAT`.
Raises `FloatDomainError` when float value of `val` is inifity or NaN.

## mrb_fixnum_to_str
```C
mrb_value mrb_fixnum_to_str(mrb_state *mrb, mrb_value x, int base);
```
Converts fixnum value of `x` to string with radix `base`.
Type tag of `x` must be `MRB_TT_FIXNUM`.
Raises `ArgumentError` if `base` is out of range [2, 36].

## mrb_fixnum_plus
```C
mrb_value mrb_fixnum_plus(mrb_state *mrb, mrb_value x, mrb_value y);
```
Returns addition result of fixnum `x` and numeric `y`.
When addition overflowed or `y` isn't fixnum, returns float value.
Type tag of `x` must be `MRB_TT_FIXNUM`.
Raises `TypeError` when `y` isn't a numeric object.

## mrb_fixnum_minus
```C
mrb_value mrb_fixnum_minus(mrb_state *mrb, mrb_value x, mrb_value y);
```
Returns subtraction result of fixnum `x` and numeric `y`.
When subtraction overflowed or `y` isn't fixnum, returns float value.
Type tag of `x` must be `MRB_TT_FIXNUM`.
Raises `TypeError` when `y` isn't a numeric object.

## mrb_fixnum_mul
```C
mrb_value mrb_fixnum_mul(mrb_state *mrb, mrb_value x, mrb_value y);
```
Returns multiplication result if fixnum `x` and numeric `y`.
When multiplication overflowed or `y` isn't fixnum, returns float value.
Type tag of `x` must be `MRB_TT_FIXNUM`.
Raises `TypeError` when `y` isn't a numeric object.

## mrb_num_div
```C
mrb_value mrb_num_div(mrb_state *mrb, mrb_value x, mrb_value y);
```
Returns division result of numeric `x` and `y`.
Currently `x` and `y` is always treated as float so the result is always float value.
Raises `TypeError` when either or both of `x` and `y` isn't numeric object.

## mrb_to_flo
```C
mrb_float mrb_to_flo(mrb_state *mrb, mrb_value x);
```
Converts numeric object `x` to `mrb_float`.
Raises `TypeError` when `x` isn't a numeric object.

## mrb_int_add_overflow
```C
static inline mrb_bool
mrb_int_add_overflow(mrb_int augend, mrb_int addend, mrb_int *sum);
```
Returns `TRUE` if addition of `augend` and `addend` overflowed, otherwise `FALSE`.
The result is always stored to `sum`.

## mrb_int_sub_overflow
```C
static inline mrb_bool
mrb_int_sub_overflow(mrb_int minuend, mrb_int subtrahend, mrb_int *difference)
```
Returns `TRUE` if addition of `minuend` and `subtrahend` overflowed, otherwise `FALSE`.
The result is always stored to `difference`.
