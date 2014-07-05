# mruby/value.h

## Types

### mrb_float
```C
typedef /* float or double */ mrb_float;
```
Defines type of floating point used in mruby.

### mrb_int
```C
typedef /* int16_t, int32_t or int64_t */ mrb_int;
```
Defines type of integer used in mruby.

### mrb_sym
```C
typedef short mrb_sym;
```
Defines type of symbol identifier.

### mrb_bool
```C
typedef /* platform dependent bool type */ mrb_bool;
```
Defines boolean type used in mruby.

### struct RBasic
Most basic GC managed object.
Any GC managed object could be cast to this type.

### struct RObject
GC managed object that represents instance created from class without specific type tag.
Only instance table is defined.

## Macros.

### mrb_float_to_str
```C
int mrb_float_to_str(char *buf, mrb_float f);
```
Format float value `f` to buffer `buf`.
Returns size of buffer used.
Used to format float value in float dumping.

### str_to_mrb_float
```C
mrb_float str_to_mrb_float(const char *buf);
```
Converts formatted float string `buf` to `mrb_float`.

### MRB_INT_BIT
Macro to define size of `mrb_int` in bits.

### MRB_INT_MIN
Macro to define minimum value that could be stored to `mrb_int`.

### MRB_INT_MAX
Macro to define maximum value that could be stored to `mrb_int`.

### MRB_TT_HAS_BASIC_P
```C
mrb_bool MRB_TT_HAS_BASIC_P(enum mrb_vtype v);
```
Checks whether type of `v` has `struct RBasic*`.

### mrb_ptr
```C
void *mrb_ptr(mrb_value v);
```
Takes pointer data from `mrb_value`.

### mrb_type
```C
enum mrb_value mrb_type(mrb_value v);
```
Takes type tag of `mrb_value`.

### mrb_float
```C
mrb_float mrb_float(mrb_value v);
```
Takes `mrb_float` from `mrb_value`.
Type tag of `v` must be `MRB_TT_FLOAT`.

### mrb_cptr
```C
void *mrb_cptr(mrb_value v);
void *mrb_voidp(mrb_value v);
```
Takes c pointer from cptr object `v`.
Type tag of `v` must be `MRB_TT_CPTR`.

### mrb_fixnum_p
```C
mrb_bool mrb_fixnum_p(mrb_value v);
```
Checks whether type tag of `v` is `MRB_TT_FIXNUM`.

### mrb_undef_p
```C
mrb_bool mrb_undef_p(mrb_value v);
```
Checks whether type tag of `v` is `MRB_TT_UNDEF`.

### mrb_nil_p
```C
mrb_bool mrb_nil_p(mrb_value v);
```
Checks whether `v` is `nil` object.

### Conversion to boolean.
```C
mrb_bool mrb_bool(mrb_value v);
mrb_bool mrb_test(mrb_value v);
```
Returns conversion to boolean from `mrb_value`.

### mrb_fixnum
```C
mrb_int mrb_fixnum(mrb_value v);
```
Takes fixnum value from `mrb_value`.
Type tag of `v` must be `MRB_TT_FIXNUM`.

### mrb_symbol
```C
mrb_sym mrb_symbol(mrb_value v);
```
Takes symbol from `mrb_value`.
Type tag of `v` must be `MRB_TT_SYMBOL`.

### mrb_float_p
```C
mrb_bool mrb_float_p(mrb_value v);
```
Checks whether type tag of `v` is `MRB_TT_FLOAT`.

### mrb_symbol_p
```C
mrb_bool mrb_symbol_p(mrb_value v);
```
Checks whether type tag of `v` is `MRB_TT_SYMBOL`.

### mrb_array_p
```C
mrb_bool mrb_array_p(mrb_value v);
```
Checks whether type tag of `v` is `MRB_TT_ARRAY`.

### mrb_string_p
```C
mrb_bool mrb_string_p(mrb_value v);
```
Checks whether type tag of `v` is `MRB_TT_STRING`.

### mrb_hash_p
```C
mrb_bool mrb_hash_p(mrb_value v);
```
Checks whether type tag of `v` is `MRB_TT_HASH`.

### mrb_cptr_p
```C
mrb_bool mrb_cptr_p(mrb_value v);
mrb_bool mrb_voidp_p(mrb_value v);
```
Checks whether type tag of `v` is `MRB_TT_CPTR`.

### Taking `struct RBasic*`
```C
struct RBasic *mrb_basic_ptr(mrb_value v);
struct RBasic *mrb_basic(mrb_value v);
```
Takes `struct RBasic*` from `mrb_value`.
`MRB_TT_HAS_BASIC_P(v)` must be `TRUE`.

### Taking `struct RObject*`
```C
struct RObject *mrb_object_ptr(mrb_value v);
struct RObject *mrb_object(mrb_value v);
```
Takes `struct RObject*` from `mrb_value`.
Type of `v` must be a type compatible with `struct RObject`.

### Checks immediate type.
```C
mrb_bool mrb_immediate_p(mrb_value v);
mrb_bool mrb_special_const_p(mrb_value v);
```
Checks whether `v` is a immediate type.

### mrb_special_const_p

## enum mrb_vtype
Type tags used in `mrb_value` and `struct RBasic`.

Name | Corresponding type
=====|===================
`MRB_TT_FALSE` | `FalseClass`
`MRB_TT_FREE` | For object that isn't used.
`MRB_TT_TRUE` | `TrueClass`
`MRB_TT_FIXNUM` | `Fixnum`
`MRB_TT_SYMBOL` | `Symbol`
`MRB_TT_UNDEF` | For undefined value.
`MRB_TT_FLOAT` | `Float`
`MRB_TT_CPTR` | For object to store raw C pointer. `MRB_TT_VOIDP` is a compatibility macro.
`MRB_TT_OBJECT` | `Object`
`MRB_TT_CLASS` | `Class`
`MRB_TT_MODULE` | `Module`
`MRB_TT_ICLASS` | Used in internal object for module including.
`MRB_TT_SCLASS` | Represents singleton class.
`MRB_TT_PROC` | `Proc`
`MRB_TT_ARRAY` | `Array` or `Struct`
`MRB_TT_HASH` | `Hash`
`MRB_TT_STRING` | `String`
`MRB_TT_RANGE` | `Range`
`MRB_TT_EXCEPTION` | Not used.
`MRB_TT_FILE` | Not used.
`MRB_TT_ENV` | For closure environment.
`MRB_TT_DATA` | For user defined native handle.
`MRB_TT_FIBER` | `Fiber`
`MRB_TT_MAXDEFINE` | Count of type tag kinds.

## mrb_float_value
```C
mrb_value mrb_float_value(mrb_state *mrb, mrb_float f);
```
Creates float object from `f`.

## mrb_float_pool
```C
mrb_value mrb_float_pool(mrb_state *mrb, mrb_float f);
```

## mrb_regexp_p
```C
mrb_bool mrb_regexp_p(struct mrb_state *mrb, mrb_value v);
```
Checks whether `v` is a instance of `Regexp` class.

## mrb_fixnum_value
```C
mrb_value mrb_fixnum_value(mrb_int i);
```
Creates `mrb_value` from fixnum value `i`.

## mrb_symbol_value
```C
mrb_value mrb_symbol_value(mrb_sym s);
```
Creates `mrb_value` from symbol `s`.

## mrb_obj_value
```C
mrb_value mrb_obj_value(void *p);
```
Creates `mrb_value` from `p`.
Object pointed by `p` must be compatible with `struct RBasic`.

## mrb_cptr_value
```C
mrb_value mrb_cptr_value(struct mrb_state *mrb, void *p);
mrb_value mrb_voidp_value(struct mrb_state *mrb, void *p);
```
Creates `mrb_value` from raw C pointer `p`.

## mrb_false_value
```C
mrb_value mrb_false_value();
```
Creates `false` object.

## mrb_nil_value
```C
mrb_value mrb_nil_value();
```
Creates `nil` object.

## mrb_true_value
```C
mrb_value mrb_true_value();
```
Creates `true` object

## mrb_undef_value
```C
mrb_value mrb_undef_value();
```
Creates undefined object.

## mrb_bool_value
```C
mrb_value mrb_bool_value(mrb_bool b);
```
Creates boolean object from `b`.
