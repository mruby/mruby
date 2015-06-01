# mruby/class.h

## struct RClass
Subclass of RBasic to represent `Class`, `Module`, singleton class, include class(`MRB_TT_ICLASS`).

## Macros.
Codes are psuedo prototype of macro.

### mrb_class_ptr
```C
struct RClass* mrb_class_ptr(mrb_value v);
```
Macro to cast `RClass` object `mrb_value` to `struct RClass*`.

### RCLASS_SUPER
```C
struct RClass* RCLASS_SUPER(mrb_value v);
```
Macro to take super class from `RClass` object `mrb_value`.

### RCLASS_IV_TBL
```C
struct iv_tbl* RCLASS_IV_TBL(mrb_value v);
```
Macro to take instance variable table from `RClass` object `mrb_value`.

### RCLASS_M_TBL
```C
struct kh_mt* RCLASS_M_TBL(mrb_value v);
```
Macro to take method table from `RClass` object `mrb_value`.

### MRB_SET_INSTANCE_TT
```C
void MRB_SET_INSTANCE_TT(struct RClass *c, enum mrb_vtype tt);
```
Sets type tag of instance created from `c`.
Call this macro as soon as `struct RClass*` is created.
Type tag set with this macro is inherited in subclasses.

### MRB_INSTANCE_TT
```C
enum mrb_value MRB_INSTANCE_TT(struct RClass *c);
```
Returns instance type tag of `c`.

## mrb_class
```C
struct RClass* mrb_class(mrb_state *mrb, mrb_value v);
```
Inline function to get class of `v`.
Returns `NULL` if type tag of `v` is `MRB_TT_ENV`.
`MRB_TT_CPTR` type tagged `mrb_value` is treated as `Object` class instance.

## mrb_define_class_id
```C
struct RClass* mrb_define_class_id(mrb_state* mrb, mrb_sym sym, struct RClass* super);
```
Defines and returns class of name `sym` with super class `super`.
Uses `mrb_sym` instead of `const char*` as 2nd argument.

## mrb_define_module_id
```C
struct RClass* mrb_define_module_id(mrb_state* mrb, mrb_sym sym);
```
Defines and return module of name `sym`.
Uses `mrb_sym` instead of `const char*` as 2nd argument.

## mrb_vm_define_class
```C
struct RClass *mrb_vm_define_class(mrb_state* mrb, mrb_value outer, mrb_value super, mrb_sym id);
```
Defines and returns class of name `id` under `outer` with super class `super`.
Calls `inherited` method of `super` unlike other class defining API.

## mrb_vm_define_module
```C
struct RClass *mrb_vm_define_module(mrb_state *mrb, mrb_value outer, mrb_sym id);
```
Defines and returns module of name `id` under `outer`.

## mrb_define_method_vm
```C
void mrb_define_method_vm(mrb_state *mrb, struct RClass *c, mrb_sym sym, mrb_value proc);
```
Defines method `proc` under class `c` of name `sym`.
`proc` must be a `mrb_value` of `struct RProc*`.

## mrb_define_method_raw
```C
void mrb_define_method_raw(mrb_state *mrb, struct RClass *c, mrb_sym sym, struct RProc *proc);
```
Defines method `proc` under class `c` of name `sym`.
This function is the most basic function to define method to `struct RClass*`.

## mrb_define_method_id
```C
void mrb_define_method_id(mrb_state *mrb, struct RClass *c, mrb_sym mid, mrb_func_t func, mrb_aspec aspec);
```
Defines method under class `c` of name `mid`.
`struct RProc*` object would be created from `func` and `aspec`.

## mrb_alias_method
```C
void mrb_alias_method(mrb_state *mrb, struct RClass *c, mrb_sym a, mrb_sym b);
```
Defines alias method of name `a` from method name `b` in class `c`.

## mrb_class_outer_module
```C
struct RClass *mrb_class_outer_module(mrb_state *mrb, struct RClass *c);
```
Returns outer `struct RClass*` of class `c`.

## mrb_method_search_vm
```C
struct RProc *mrb_method_search_vm(mrb_state* mrb, struct RClass** cp, mrb_sym sym);
```
Searches method of name `sym` from `struct RClass*` pointed by `cp`.
If method isn't found returns `NULL`.
Otherwise returns found `struct RProc*`.

## mrb_method_search
```C
struct RProc *mrb_method_search(mrb_state* mrb, struct RClass* c, mrb_sym sym);
```
Searches method of name `sym` from `c`.
Raises `NameError` if method isn't found.

## mrb_class_real
```C
struct RClass* mrb_class_real(struct RClass* cl);
```
Returns `struct RClass*` without type tag `MRB_TT_SCLASS` or `MRB_TT_ICLASS`.

## mrb_gc_mark_mt
```C
void mrb_gc_mark_mt(mrb_state *mrb, struct RClass* c);
```
Mark method table of class `c`.

## mrb_gc_mark_mt_size
```C
size_t mrb_gc_mark_mt_size(mrb_state* mrb, struct RClass* c);
```
Returns method table size of `c`.

## mrb_gc_free_mt
```C
void mrb_gc_free_mt(mrb_state* mrb, struct RClass* c);
```
Free method table of `c`.
