# mruby/data.h

## struct mrb_data_type
Type that represents user data type.
When passing pointer of this, its value must be unique value(or should be a static object)
since it's used in type checking.

### Members.
* `const char *struct_name`
  * Type name used in exception message of type checking.
* `void (*dfree)(mrb_state *mrb, void* ptr);`
  * Deleter of user defined type.

## struct RData
Subclass of `RBasic` to represent object with user data.
When initializing a `RData` in `initialize` method,
set the user data pointer and user data type with `DATA_PTR` and `DATA_TYPE` macro.

## Macros.

### RDATA
```C
struct RData *RDATA(mrb_value v);
```
Macro to take `struct RData*` from `mrb_value`.

### DATA_PTR
```C
void *DATA_PTR(mrb_value d);
```
Macro to take user data from `mrb_value`.
`d`'s type tag must be `MRB_TT_DATA`.

### DATA_TYPE
```C
const mrb_data_type *DATA_TYPE(mrb_value d);
```
Macro to take `mrb_data_type` from `mrb_value`.
`d`'s type tag must be `MRB_TT_DATA`.

### Data_Wrap_Struct
```C
struct RData *Data_Wrap_Struct(mrb_state *mrb, struct RClass *klass, const mrb_data_type *type, void *ptr);
```
Macro for CRuby compatibility. See `mrb_data_object_alloc` for detail.

### Data_Make_Struct
```C
void Data_Make_Struct(mrb_state *mrb, struct RClass *klass,
                      type of struct, const mrb_data_type *type,
                      variable name of pointer of struct type to store created object,
                      variable name to store created struct RData*);
```
Macro to create `struct RData*` with user data allocated with `mrb_malloc`.

### DATA_GET_PTR
```C
(pointer of 4th argument) DATA_GET_PTR(mrb_state *mrb, mrb_value obj, const mrb_data_type *dtype, type name of return type):
```
Macro to get user data of 4th argument type from `obj`.
See `mrb_data_get_ptr` for raising exceptions.

### DATA_CHECK_GET_PTR
```C
(pointer of 4th argument) DATA_CHECK_GET_PTR(mrb_state *mrb, mrb_value obj, const mrb_data_type *dtype, type name of return type);
```
Macro to get user data of 4th argument type from `obj`.
Returns `NULL` if type didn't match.

### mrb_data_get_ptr compatibility macro.
```C
void *mrb_data_check_and_get(mrb_state *mrb, mrb_value obj, const mrb_data_type *dtype);
void *mrb_get_datatype(mrb_state *mrb, mrb_value obj, const mrb_data_type *dtype);
void *mrb_check_datatype(mrb_state *mrb, mrb_value obj, const mrb_data_type *dtype);
```
Macro for compatibility. See `mrb_data_get_ptr`.

### Data_Get_Struct
```C
void Data_Get_Struct(mrb_state *mrb, mrb_value obj, const mrb_data_type *type, variable name of result);
```
Sets result of `mrb_data_get_ptr` to 4th argument variable.

## mrb_data_object_alloc
```C
struct RData *mrb_data_object_alloc(mrb_state *mrb, struct RClass* klass, void *datap, const mrb_data_type *type);
```
Creates `struct RData*` of class `klass`, user data `datap`, user data type `type`.
Wrap with `mrb_obj_value` if you need `mrb_value`.

## mrb_data_check_type
```C
void mrb_data_check_type(mrb_state *mrb, mrb_value obj, const mrb_data_type* dtype);
```
Checks data type of `obj` with `dtype`.
Raises `TypeError` if type check failed.

## mrb_data_get_ptr
```C
void *mrb_data_get_ptr(mrb_state *mrb, mrb_value obj, const mrb_data_type* dtype);
```
Gets user data of `obj` with type check.
Raises `TypeError` if type check failed.

## mrb_data_check_get_ptr
```C
void *mrb_data_check_get_ptr(mrb_state *mrb, mrb_value, const mrb_data_type*);
```
Gets user data of `obj`.
Returns `NULL` if type check failed.
