# mruby/variable.h
Declares variable getter/setter functions.

## mrb_vm_special_get
```C
mrb_value mrb_vm_special_get(mrb_state* mrb, mrb_sym sym);
```
Gets VM special variable named `sym`.
Currently not implemented.

## mrb_vm_special_set
```C
void mrb_vm_special_set(mrb_state *mrb, mrb_sym sym, mrb_value v);
```
Sets VM special variable named `sym` to `v`.
Currently not implemented.

## mrb_vm_iv_get
```C
mrb_value mrb_vm_iv_get(mrb_state *mrb, mrb_sym sym);
```
Gets instance variable named `sym` from current `self` in VM.

## mrb_vm_iv_set
```C
void mrb_vm_iv_set(mrb_state* mrb, mrb_sym sym, mrb_value v);
```
Sets instance variable named `sym` of current `self` in VM to `v`.

## mrb_vm_cv_get
```C
mrb_value mrb_vm_cv_get(mrb_state *mrb, mrb_sym sym);
```
Gets class variable named `sym` from current `self` in VM.

## mrb_vm_cv_set
```C
void mrb_vm_cv_set(mrb_state *mrb, mrb_sym sym, mrb_value v);
```
Sets class variable named `sym` of current `self` in VM to `v`.

## mrb_vm_const_get
```C
mrb_value mrb_vm_const_get(mrb_state *mrb, mrb_sym sym);
```
Gets constant named `sym` from current `self` in VM.

## mrb_vm_const_set
```C
void mrb_vm_const_set(mrb_state *mrb, mrb_sym sym, mrb_value v);
```
Sets constant named `sym` of current `self` in VM to `v`.

## mrb_const_get
```C
mrb_value mrb_const_get(mrb_state *mrb, mrb_value mod, mrb_sym sym);
```
Gets constant named `sym` from module `mod`.
Raises `TypeError` if type of `mod` isn't module.

## mrb_const_set
```C
void mrb_const_set(mrb_state *mrb, mrb_value mod, mrb_sym sym, mrb_value v);
```
Sets constant named `sym` of module `mod` to `v`.
Raises `TypeError` if type of `mod` isn't module.

## mrb_const_defined
```C
mrb_bool mrb_const_defined(mrb_state *mrb, mrb_value mod, mrb_sym sym);
```
Checks whether constant named `sym` is defined in module `mod`.
Raises `TypeError` if type of `mod` isn't module.

## mrb_const_remove
```C
void mrb_const_remove(mrb_state *mrb, mrb_value mod, mrb_sym sym);
```
Remove constant named `sym` in module `mod` if defined.
Raises `TypeError` if type of `mod` isn't module.

## mrb_obj_iv_get
```C
mrb_value mrb_obj_iv_get(mrb_state *mrb, struct RObject *obj, mrb_sym sym);
```
Gets instance variable named `sym` from `obj`.
Returns `nil` instead if `sym` isn't defined.

## mrb_obj_iv_set
```C
void mrb_obj_iv_set(mrb_state *mrb, struct RObject *obj, mrb_sym sym, mrb_value v);
```
Sets instance variable named `sym` of `obj` to `v`.

## mrb_obj_iv_defined
```C
mrb_bool mrb_obj_iv_defined(mrb_state *mrb, struct RObject *obj, mrb_sym sym);
```
Checks whether instance variable named `sym` is defined in `obj`.

## mrb_obj_iv_ifnone
```C
void mrb_obj_iv_ifnone(mrb_state *mrb, struct RObject *obj, mrb_sym sym, mrb_value v);
```
Sets instance variable named `sym` of `obj` to `v` if it isn't defined.
If it's already defined variable won't be modified.

## mrb_iv_get
```C
mrb_value mrb_iv_get(mrb_state *mrb, mrb_value obj, mrb_sym sym);
```
Gets instance variable named `sym` from `obj`.
Returns `nil` if the variable isn't defined or type of `obj` doesn't have instance variable table.

## mrb_iv_set
```C
void mrb_iv_set(mrb_state *mrb, mrb_value obj, mrb_sym sym, mrb_value v);
```
Sets instance variable named `sym` of `obj` to `v`.
Raises `ArgumentError` if type of `obj` doesn't have instance variable table.

## mrb_iv_defined
```C
mrb_bool mrb_iv_defined(mrb_state *mrb, mrb_value obj, mrb_sym sym);
```
Checks whether instance variable named `sym` is defined in `obj`.

## mrb_iv_remove
```C
mrb_value mrb_iv_remove(mrb_state *mrb, mrb_value obj, mrb_sym sym);
```
Removes and returns instance variable named `sym` of `obj`.
Returns `mrb_undef_value()` if it isn't defined.

## mrb_iv_copy
```C
void mrb_iv_copy(mrb_state *mrb, mrb_value dst, mrb_value src);
```
Copies instance variable table of `src` to `dst`.
Type of `dst` and `src` must have instance variable.

## mrb_const_defined_at
```C
int mrb_const_defined_at(mrb_state *mrb, struct RClass *c, mrb_sym id);
```
Checks whether constant named `id` is defined in module `c`.

## mrb_mod_constants
```C
mrb_value mrb_mod_constants(mrb_state *mrb, mrb_value mod);
```
Returns an array of all names defined in module `mod` in `mod`.

## mrb_f_global_variables
```C
mrb_value mrb_f_global_variables(mrb_state *mrb, mrb_value self);
```
Returns an array of all global varaible names.
2nd argument is ignored.

## mrb_gv_get
```C
mrb_value mrb_gv_get(mrb_state *mrb, mrb_sym sym);
```
Gets global variable named `sym`.
Returns `nil` if it isn't defined.

## mrb_gv_set
```C
void mrb_gv_set(mrb_state *mrb, mrb_sym sym, mrb_value val);
```
Sets global variable named `sym` to `val`.

## mrb_gv_remove
```C
void mrb_gv_remove(mrb_state *mrb, mrb_sym sym);
```
Removes global variable named `sym`.

## mrb_obj_instance_variables
```C
mrb_value mrb_obj_instance_variables(mrb_state *mrb, mrb_value obj);
```
Returns array of instance variable names in `obj`.

## mrb_obj_iv_inspect
```C
mrb_value mrb_obj_iv_inspect(mrb_state *mrb, struct RObject *obj);
```
Creates inspect string with instance variables from `obj`.

## mrb_class_sym
```C
mrb_sym mrb_class_sym(mrb_state *mrb, struct RClass *c, struct RClass *outer);
```
Function to get name of class `c`.
Returns internal class instance variable named `:__classid__` if defined.
Otherwise searches constant name storing `c` in class `outer`.

## mrb_mod_class_variables
```C
mrb_value mrb_mod_class_variables(mrb_state *mrb, mrb_value mod);
```
Returns an array of all class variables defined in module `mod`.

## mrb_mod_cv_get
```C
mrb_value mrb_mod_cv_get(mrb_state *mrb, struct RClass * c, mrb_sym sym);
```
Gets class variable named `sym` from module `c`.
Raises `NameError` when it isn't found.

## mrb_cv_get
```C
mrb_value mrb_cv_get(mrb_state *mrb, mrb_value mod, mrb_sym sym);
```
Gets class variables named `sym` from module object `mod`.
Raises `NameError` when it isn't found.
Type of `mod` must be module.

## mrb_mod_cv_set
```C
void mrb_mod_cv_set(mrb_state *mrb, struct RClass * c, mrb_sym sym, mrb_value v);
```
Sets class variable named `sym` of module `c` to `v`.

## mrb_cv_set
```C
void mrb_cv_set(mrb_state *mrb, mrb_value mod, mrb_sym sym, mrb_value v);
```
Sets class variable named `sym` of module object `mod` to `v`.
Type of `mod` must be module.

## mrb_mod_cv_defined
```C
mrb_bool mrb_mod_cv_defined(mrb_state *mrb, struct RClass * c, mrb_sym sym);
```
Checks whether class variable named `sym` in module `c` is defined.

## mrb_cv_defined
```C
mrb_bool mrb_cv_defined(mrb_state *mrb, mrb_value mod, mrb_sym sym);
```
Checks whether class variable named `sym` in module object `mod` is defined.
Type of `mod` must be module.

## mrb_gc_mark_gv
```C
void mrb_gc_mark_gv(mrb_state *mrb);
```
Marks global variables of VM.

## mrb_gc_free_gv
```C
void mrb_gc_free_gv(mrb_state *mrb);
```
Frees global variable table.

## mrb_gc_mark_iv
```C
void mrb_gc_mark_iv(mrb_state *mrb, struct RObject *obj);
```
Marks instance variables of `obj`.

## mrb_gc_mark_iv_size
```C
size_t mrb_gc_mark_iv_size(mrb_state *mrb, struct RObject *obj);
```
Returns size of instance variable table.

## mrb_gc_free_iv
```C
void mrb_gc_free_iv(mrb_state *mrb, struct RObject *obj);
```
Frees instance variable table of `obj`.
