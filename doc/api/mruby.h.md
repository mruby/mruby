# mruby.h

Basic header of mruby.
It includes **mrbconf.h**, **mruby/value.h**, **mruby/version.h** internally.

## mrb_code
```C
typedef uint32_t mrb_code;
```
Type of mruby opcode storage.

## mrb_context
Execution context of mruby VM.
This object is substance of `Fiber` object.

## `mrb_state` management

### Fields.
Most is read only.
* `struct RObject *exc;`
  * Pointer to current raised exception.
  * `NULL` if none of exception is raised.
* `struct RClass *object_class;`
  * Class object of `Object`.
* `struct RClass *class_class;`
  * Class object of `Class`.
* `struct RClass *module_class;`
  * Class object of `Module`.
* `struct RClass *proc_class;`
  * Class object of `Proc`.
* `struct RClass *string_class;`
  * Class object of `String`.
* `struct RClass *array_class;`
  * Class object of `Array`.
* `struct RClass *hash_class;`
  * Class object of `Hash`.
* `struct RClass *float_class;`
  * Class object of `Float`.
* `struct RClass *fixnum_class;`
  * Class object of `Fixnum`.
* `struct RClass *true_class;`
  * Class object of `TrueClass`.
* `struct RClass *false_class;`
  * Class object of `FalseClass`.
* `struct RClass *nil_class;`
  * Class object of `NilClass`.
* `struct RClass *symbol_class;`
  * Class object of `Symbol`.
* `struct RClass *eException_class;`
  * Class object of `Exception`.
* `struct RClass *eStandardError_class;`
  * Class object of `StandardError`.
* `struct RClass *kernel_class;`
  * Module object of `Kernel`.
* `size_t live;`
  * Count of live objects.
* `enum gc_state gc_state;`
  * State of GC.
* `void (*code_fetch_hook)(struct mrb_state* mrb, struct mrb_irep *irep, mrb_code *pc, mrb_value *regs);`
  * Could be modified by user.
  * Hook which is called every `mrb_code` fetch.
  * `ENABLE_DEBUG` must be defined to use this.
* `void (*debug_op_hook)(struct mrb_state* mrb, struct mrb_irep *irep, mrb_code *pc, mrb_value *regs);`
  * Could be modified by user.
  * Hook which is called when `OP_DEBUG` is fetched.
  * `ENABLE_DEBUG` must be defined to use this.
* `mrb_bool gc_disabled;`
  * Could be modified by user.
  * `TRUE` if GC is disabled.
* `mrb_bool is_generational_gc_mode;`
  * Could be modified by user.
  * If `TRUE` GC is generational mode. Otherwise GC is normal mode.
* `int gc_interval_ratio;`
  * Could be modified by user.
  * Value of GC ratio.
  * Unit is percent(%).
* `int gc_step_ratio;`
  * Could be modified by user.
  * Value of step span ratio of incremental GC.
  * Unit is percent(%).
* `mrb_bool out_of_memory;`
  * If `TRUE` allocator failed to allocate memory.
* `struct RObject *nomem_err;`
  * Pre-allocated exception object for allocator failure.

### mrb_open
```C
mrb_state* mrb_open();
```
Creates new `mrb_state`.

### mrb_allocf
```C
typedef void* (*mrb_allocf) (struct mrb_state *mrb, void *ptr, size_t s, void *ud);
```
Function pointer type of custom allocator used in `mrb_open_allocf`.

The function pointing it must behave similarly as `realloc` except:
* If `ptr` is `NULL` it must allocate new space.
* If `s` is `NULL`, `ptr` must be freed.

### mrb_default_allocf
```C
void* mrb_default_allocf(mrb_state *mrb, void *ptr, size_t size, void* ud);
```
Default `allocf` implemented with standard C functions that will be used in `mrb_open`.
It doesn't require `ud` so if you're creating `mrb_state*` with this, pass `NULL` as `ud`.

### mrb_open_allocf
```C
mrb_state* mrb_open_allocf(mrb_allocf f, void *ud);
```
Create new `mrb_state` with custom allocator.
`ud` will be passed to custom allocator `f`.
If user data isn't required just pass `NULL`.
Function pointer `f` must satisfy requirements of its type.

### mrb_close
```C
void mrb_close(mrb_state *mrb);
```
Deletes `mrb_state`.

### mrb_open_core
```C
mrb_state* mrb_open_core(mrb_allocf, void *ud);
```
Creates new `mrb_state` without mrbgems.
Useful when creating lighter `mrb_state`.

### mrb_atexit_func
```C
typedef void (*mrb_atexit_func)(struct mrb_state*);
```
Type of callback called on `mrb_state*` close.

### mrb_state_atexit
```C
void mrb_state_atexit(mrb_state *mrb, mrb_atexit_func func);
```
Add callback `func` to be called on `mrb` close to atexit stack.

## mrb_define_global_const
```C
void mrb_define_global_const(mrb_state *mrb, const char *name, mrb_value val);
```
Defines global constant `val` as name `name`.

## Useful macros.

### mrb_noreturn
```C
#define mrb_noreturn /* platform specific value */
```
Macro of noreturn attribute.
When function is declared with this attribute it won't return.
Used in function that only raises exceptions.

### mrb_strlen_lit
```C
#define mrb_strlen_lit(lit) (sizeof(lit "") - 1)
```
`strlen` for character string literals (use with caution or `strlen` instead)
Adjacent string literals are concatenated in C/C++ in translation phase 6.
If `lit` is not one, the compiler will report a syntax error:
* MSVC: "error C2143: syntax error : missing ')' before 'string'"
* GCC:  "error: expected ')' before string constant"

Used in `mrb_*_lit` APIs.

## Argument specifier

### mrb_aspec
```C
typedef uint32_t mrb_aspec;
```
Type of argument specifier value.
Combine arguments specifiers with `|` operator.

### Macros.

macro | description
`MRB_ARGS_BLOCK()`| Accept block argument.
`MRB_ARGS_ANY()`| Accept any number of arguments.
`MRB_ARGS_REST()`| Same as `MRB_ARGS_ANY`.
`MRB_ARGS_NONE()`| Accept no arguments.
`MRB_ARGS_REQ(n)`| Specifies required arguments number `n`.
`MRB_ARGS_OPT(n)`| Specifies optional arguments number `n`.
`MRB_ARGS_ARG(req, opt)`| Specifies required arguments number `req` and optinal arguments number `opt`.
`MRB_ARGS_POST(n)`| Specifies required arguments number `n` after rest.
`MRB_ARGS_KEY(n1, n2)`| Keyword arguments(n of keys, kdict).

### Compatibility macros.
In **mruby.h** it defines argument specifier macro without `MRB_` prefix for compatibility.

## C interface.

### mrb_func_t
```C
typedef mrb_value (*mrb_func_t)(mrb_state *mrb, mrb_value);
```
C interface function pointer type.
Unlike CRuby arguments aren't passed.
Use `mrb_get_args` to get arguments.

### mrb_get_args
```C
int mrb_get_args(mrb_state *mrb, const char *format, ...);
```
Retrieve arguments from `mrb_state`.
Use it inside a function pointed by `mrb_func_t`.
It returns number of function retrieved.
`format` is a list of following format specifier:

char|mruby type|retrieve types|note
:---:|----------|--------------|---
`o`|`Object`|`mrb_value`|Could be used to retrieve any type of argument
`C`|`Class`/`Module`|`mrb_value`|
`S`|`String`|`mrb_value`|
`A`|`Array`|`mrb_value`|
`H`|`Hash`|`mrb_value`|
`s`|`String`|`char*`, `mrb_int`|
`z`|`String`|`char*`|
`a`|`Array`|`mrb_value*`, `mrb_int`|
`f`|`Float`|`mrb_float`|
`i`|`Integer`|`mrb_int`|
`b`|boolean|`mrb_bool`|
`n`|`Symbol`|`mrb_sym`|
`&`|block|`mrb_value`|
`*`|rest arguments|`mrb_value*`, `mrb_int`|Receive the rest of arguments as an array.
<code>&#124;</code>|optional||After this spec following specs would be optional.
`?`|optional given|`mrb_bool`|True if preceding argument is given. Used to check optional argument is given.

The passing variadic arguments must be a pointer of retrieving type.

## Class / Module

### Defining class.
```C
struct RClass *mrb_define_class(mrb_state *mrb, const char *name, struct RClass *super);
struct RClass *mrb_define_class_under(mrb_state *mrb, struct RClass *outer, const char *name, struct RClass *super);
```
Defines class with class name of `name` and super class of `super`.
If it needs to be defined under a class or module `outer` use `mrb_define_class_under`.
It'll raise `TypeError` when it's already defined and super class doesn't match.

### Defining module.
```C
struct RClass *mrb_define_module(mrb_state *mrb, const char *name);
struct RClass *mrb_define_module_under(mrb_state *mrb, struct RClass *outer, const char *name);
```
Defines module with module name of `name`.
If it needs to be defined under a class or module `outer` use `mrb_define_module_under`.

### Defining method.
```C
void mrb_define_method(mrb_state *mrb, struct RClass *cls, const char *name, mrb_func_t func, mrb_aspec args);
void mrb_define_class_method(mrb_state *mrb, struct RClass *cls, const char *name, mrb_func_t func, mrb_aspec args);
void mrb_define_module_function(mrb_state *mrb, struct RClass *cls, const char* name, mrb_func_t func, mrb_aspec args);
```
Defines method with `name` under `cls`.
`func` is the C interface and `args` argument is the argument spec.

`mrb_define_method` defines instance method.
`mrb_define_class_method` defines class method.
`mrb_define_module_function` defines private instance method and class method.
(Currently visibility isn't implemented so some checks won't work like CRuby.)

### mrb_include_module
```C
void mrb_include_module(mrb_state *mrb, struct RClass *a, struct RClass *b);
```
Include module `b` to `a`.

### mrb_define_const
```C
void mrb_define_const(mrb_state* mrb, struct RClass *c, const char *name, mrb_value v);
```
Defines constant `v` of `name` to class or module `c`.

### mrb_undef_method
```C
void mrb_undef_method(mrb_state *mrb, struct RClass *c, const char* name);
```
Undefine method of `name` from `c`.
Raises `NameError` if `name` isn't defined.

### mrb_undef_class_method
```C
void mrb_undef_class_method(mrb_state *mrb, struct RClass *c, const char* name);
```
Undefine class method of `name` from `c`.
Raises `NameError` if `name` isn't defined.

### Creating a instance from class.
```C
mrb_value mrb_obj_new(mrb_state *mrb, struct RClass *c, mrb_int argc, const mrb_value *argv);
mrb_value mrb_class_new_instance(mrb_state *mrb, mrb_int argc, const mrb_value *argv, struct RClass *c);
mrb_value mrb_instance_new(mrb_state *mrb, mrb_value cv);
```
Creates new instance with arguments from class `c`.
`mrb_class_new_instance` is a alias macro to `mrb_obj_new`.
`mrb_instance_new` takes argument from `mrb_get_args` instead of taking `argc` and `argv`.

### mrb_class_new
```C
struct RClass * mrb_class_new(mrb_state *mrb, struct RClass *super);
```
Creates new class with super class `super`.

### mrb_module_new
```C
struct RClass * mrb_module_new(mrb_state *mrb);
```
Creates new module.

### mrb_class_defined
```C
mrb_bool mrb_class_defined(mrb_state *mrb, const char *name);
```
Returns `TRUE` if class of `name` is defined.

### Getting class.
```C
struct RClass * mrb_class_get(mrb_state *mrb, const char *name);
struct RClass * mrb_class_get_under(mrb_state *mrb, struct RClass *outer, const char *name);
```
Returns class of `name`.
In `mrb_class_get_under` returns class of `name` under `outer` instead.
Raises `TypeError` if found constant is not `Class`.
Use `mrb_module_get*` instead to get module.(In mruby newer than 1.0.0)

### Getting module.
```C
struct RClass * mrb_module_get(mrb_state *mrb, const char *name);
struct RClass * mrb_module_get_under(mrb_state *mrb, struct RClass *outer, const char *name);
```
Returns module of `name`.
In `mrb_module_get_under` returns module of `name` under `outer` instead.
Raises `TypeError` if found constant is not `Module`.

### mrb_obj_respond_to
```C
mrb_bool mrb_obj_respond_to(mrb_state *mrb, struct RClass* c, mrb_sym mid);
```
Returns `TRUE` if `c` has method `mid`, otherwise `FALSE`.

### Getting class name.
```C
const char *mrb_class_name(mrb_state *mrb, struct RClass *c);
mrb_value mrb_class_path(mrb_state *mrb, struct RClass *c);
```
Returns name of class `c`. `mrb_class_path` returns string object instead.

### mrb_define_alias
```C
void mrb_define_alias(mrb_state *mrb, struct RClass *c, const char *dup, const char *orig);
```
Define alias `dup` from `orig` in class `c`.

## Singleton object.

### mrb_define_singleton_method
```C
void mrb_define_singleton_method(mrb_state *mrb, struct RObject *obj, const char *name, mrb_func_t func, mrb_aspec args);
```
Defines singleton method of `name` to mruby object `obj`.

### mrb_singleton_class
```C
mrb_value mrb_singleton_class(mrb_state* mrb, mrb_value obj);
```
Returns singleton class of `obj`.
If its singleton class isn't defined it will be created.

## Object

### Conversion.

#### mrb_check_convert_type
```C
mrb_value mrb_check_convert_type(mrb_state *mrb, mrb_value val, enum mrb_vtype type, const char *tname, const char *method);
```
Converts `val` to type tagged `type` using method named `method`.
Returns `nil` if conversion failed.
`tname` is for compatibility to `mrb_convert_type`.

#### mrb_convert_type
```C
mrb_value mrb_convert_type(mrb_state *mrb, mrb_value val, enum mrb_vtype type, const char *tname, const char *method);
```
Converts `val` to type tagged `type` using method named `method`.
Raised `TypeError` with message using `tname` when conversion failed.

#### mrb_check_type
```C
void mrb_check_type(mrb_state *mrb, mrb_value x, enum mrb_vtype t);
```
Checks `x`'s type tag is `t`.
Raises `TypeError` when check failed.
This function is used in conversion function.

#### mrb_check_to_integer
```C
mrb_value mrb_check_to_integer(mrb_state *mrb, mrb_value val, const char *method);
```
Converts `val` to `Integer` type using method named `method`.
Returns `nil` if conversion failed.

#### mrb_Integer
```C
mrb_value mrb_Integer(mrb_state *mrb, mrb_value val);
```
Converts `val` to `Integer` type using `to_int` or `to_i` method.
Raises `TypeError` if conversion failed.

#### mrb_to_int
```C
mrb_value mrb_to_int(mrb_state *mrb, mrb_value val);
```
Converts `val` to `Integer` type using `to_int` method.
Raises `TypeError` when conversion failed.

#### mrb_int
```C
mrb_int mrb_int(mrb_state *mrb, mrb_value val);
```
Macro to convert `val` to `mrb_int` type using `mrb_to_int` function.

#### mrb_Float
```C
mrb_value mrb_Float(mrb_state *mrb, mrb_value val);
```
Converts `val` to `Float` type using `to_f` method.
Raises `TypeError` if conversion failed.

#### mrb_obj_to_sym
```C
mrb_sym mrb_obj_to_sym(mrb_state *mrb, mrb_value val);
```
Converts `val` to `Symbol` type.
Unlike float or integer `to_sym` method won't be referred.
Raises `TypeError` if conversion failed.

#### mrb_any_to_s
```C
mrb_value mrb_any_to_s(mrb_state *mrb, mrb_value obj);
```
Returns string object formatted like `"#<ClassName:0xDEADBEAF>"`.

#### mrb_inspect
```C
mrb_value mrb_inspect(mrb_state *mrb, mrb_value obj);
```
Returns `inspect` call result of `obj`.

#### mrb_obj_inspect
```C
mrb_value mrb_obj_inspect(mrb_state *mrb, mrb_value obj);
```
Returns default `inspect` call result of `obj`.
User defined `inspect` method won't be emitted in this function.

### Comparing objects.

#### mrb_obj_eq
```C
mrb_bool mrb_obj_eq(mrb_state *mrb, mrb_value lhs, mrb_value rhs);
```
Compares `lhs` and `rhs` without `==` method.

#### mrb_obj_equal
```C
mrb_bool mrb_obj_equal(mrb_state*, mrb_value, mrb_value);
```
Currently alias to `mrb_obj_eq`.

#### mrb_equal
```C
mrb_bool mrb_equal(mrb_state *mrb, mrb_value obj1, mrb_value obj2);
```
Compares `obj1` and `obj2` using `mrb_obj_eq` and `==` method fallback.

#### mrb_eql
```C
mrb_bool mrb_eql(mrb_state *mrb, mrb_value obj1, mrb_value obj2);
```
Compares `obj1` and `obj2` using `mrb_obj_eq` and `eql?` method fallback.

### mrb_obj_dup
```C
mrb_value mrb_obj_dup(mrb_state *mrb, mrb_value obj);
```
Creates duplicate of `obj`.
Duplicate isn't a strict copy of `obj` as cloned one.

### mrb_obj_clone
```C
mrb_value mrb_obj_clone(mrb_state *mrb, mrb_value self);
```
Creates clone of `obj`.

### mrb_attr_get
```C
mrb_value mrb_attr_get(mrb_state *mrb, mrb_value obj, mrb_sym id);
```
Gets instance variable named `id` from `obj`.
Alias of `mrb_iv_get`.

### mrb_respond_to
```C
mrb_bool mrb_respond_to(mrb_state *mrb, mrb_value obj, mrb_sym mid);
```
Checks whether `obj` has method of name `mid`.

### mrb_obj_is_kind_of
```C
mrb_bool mrb_obj_is_kind_of(mrb_state *mrb, mrb_value obj, struct RClass *c);
```
Checks whether `obj` is kind of class `c`.

### mrb_obj_is_instance_of
```C
mrb_bool mrb_obj_is_instance_of(mrb_state *mrb, mrb_value obj, struct RClass* c);
```
Checks whether `obj` is directly instance of class `c`.
Unlike `mrb_obj_is_kind_of` returns `FALSE` when `c` is a super class instace's class.

### mrb_obj_id
```C
mrb_int mrb_obj_id(mrb_value obj);
```
Returns object id of `obj`.
Unlike other ruby implementation the returned value isn't unique.

### mrb_obj_classname
```C
const char * mrb_obj_classname(mrb_state *mrb, mrb_value obj);
```
Returns class name of object `obj`.

### mrb_obj_class
```C
struct RClass* mrb_obj_class(mrb_state *mrb, mrb_value obj);
```
Returns class of object `obj`.
Unlike `mrb_class` function singleton class won't be returned.

## String

### Creating string object.
```C
mrb_value mrb_str_new(mrb_state *mrb, const char *p, size_t len);
mrb_value mrb_str_new_cstr(mrb_state* mrb, const char* p);
mrb_value mrb_str_new_static(mrb_state *mrb, const char *p, size_t len);
mrb_value mrb_str_new_lit(mrb_state *mrb, string literal);
```
Creates string from memory region `p` with length `len`.
In `mrb_str_new_cstr` and `mrb_str_new_lit`, `len` is calculated with `strlen` and `sizeof`.
`mrb_str_new_static` doesn't copy [`p`, `p + len`) so the region must live longer than `mrb`.

## Symbol

### Getting `mrb_sym`.
```C
mrb_sym mrb_intern_cstr(mrb_state* mrb, const char* str);
mrb_sym mrb_intern(mrb_state* mrb, const char* str, size_t size);
mrb_sym mrb_intern_static(mrb_state* mrb, const char* str, size_t size);
mrb_sym mrb_intern_lit(mrb_state *mrb, string literal lit);
mrb_sym mrb_intern_str(mrb_state*, mrb_value str_obj);
```
Returns `mrb_sym` from `str` and `size`.
`mrb_intern_static` and `mrb_intern_lit` appropriate region [`str`, `str` + `size`)
so `str` must live longer than `mrb`.

In `mrb_intern_cstr`, `size` is calculated with `strlen` and
in `mrb_intern_lit`, `size` is calculated with `sizeof`.

`mrb_intern_str` uses pointer and length of `RString` and call `mrb_intern`.
`str_obj`'s type tag must be `MRB_TT_STRING`.

### Getting existing symbol
```C
mrb_value mrb_check_intern_cstr(mrb_state *mrb, const char* str);
mrb_value mrb_check_intern(mrb_state* mrb, const char* str, size_t size);
mrb_value mrb_check_intern_str(mrb_state* mrb, mrb_value str_obj);
```
Returns `mrb_sym` wrapped with `mrb_value`
if symbol corresponding to passed string value exist in `mrb`.
Otherwise returns `nil` value.

### Getting substance of `Symbol`
```C
const char *mrb_sym2name(mrb_state* mrb, mrb_sym sym);
const char *mrb_sym2name_len(mrb_state* mrb, mrb_sym sym, mrb_int *size);
mrb_value mrb_sym2str(mrb_state* mrb, mrb_sym sym);
```
Returns substance of `sym`.

`mrb_sym2name_len` returns raw string with size.
If `size` is `NULL` it won't return size of symbol.

`mrb_sym2name` returns dumped string of symbol name.
If you need raw string use `mrb_sym2name_len` instead.

`mrb_sym2str` returns mruby string object instead.

## Fiber

### mrb_fiber_state
```C
enum mrb_fiber_state {
  MRB_FIBER_CREATED = 0,
  MRB_FIBER_RUNNING,
  MRB_FIBER_RESUMING,
  MRB_FIBER_SUSPENDED,
  MRB_FIBER_TRANSFERRED,
  MRB_FIBER_TERMINATED,
};
```
Represents fiber states.
Used in `mrb_context` which is the internal object of `Fiber`.

### E_FIBER_ERROR
```C
#define E_FIBER_ERROR (mrb_class_get(mrb, "FiberError"))
```
Returns `FiberError` class object.
Variable `mrb` of type `mrb_state*` must be defined in the context.

### mrb_fiber_yield
```C
mrb_value mrb_fiber_yield(mrb_state *mrb, mrb_int argc, const mrb_value *argv);
```
Yields current running fiber with arguments.
After calling this function it must return to mruby VM as soon as possible.

## Memory allocator.
```C
void *mrb_malloc(mrb_state* mrb, size_t s);
void *mrb_calloc(mrb_state* mrb, size_t s, size_t s);
void *mrb_realloc(mrb_state* mrb, void* ptr, size_t s);
```
The usage is same as standard C function excepts it takes `mrb` argument and
raises `RuntimeError` when memory ran out.

### Simple version.
```C
void *mrb_realloc_simple(mrb_state* mrb, void* ptr, size_t s);
void *mrb_malloc_simple(mrb_state* mrb, size_t s);
```
Returns `NULL` if it failed to allocate memory.
Perform full GC in first allocation failure.

### mrb_free
```C
void mrb_free(mrb_state* mrb, void* ptr);
```
Frees `ptr`.

### mrb_alloca
```C
void* mrb_alloca(mrb_state *mrb, size_t s);
```
Allocates memory region of size `s` that lives until `mrb` dies.

## Running.

### Calling method
```C
mrb_value mrb_funcall(mrb_state* mrb, mrb_value recv, const char* name, mrb_int argc,...);
mrb_value mrb_funcall_argv(mrb_state* mrb, mrb_value recv, mrb_sym sym, mrb_int argc, const mrb_value* argv);
mrb_value mrb_funcall_with_block(mrb_state* mrb, mrb_value recv, mrb_sym sym, mrb_int argc, const mrb_value* argv, mrb_value block);
```
Call method of name `sym` or `name` in receiver `recv` with arguments and returns result.
In `mrb_funcall` pass arguments of type `mrb_value` to rest arguments.
The number of rest argument must be equal to `argc`.
If you need to pass a block use `mrb_funcall_with_block`.

### Calling block
```C
mrb_value mrb_yield(mrb_state *mrb, mrb_value b, mrb_value arg);
mrb_value mrb_yield_argv(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv);
mrb_value mrb_yield_with_class(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv, mrb_value recv, struct RClass *c);
```
Call block `b` with arguments and returns result.
If there is only single argument to pass use `mrb_yield`.

In `mrb_yield_with_class` it could call Proc with receiver `recv` that is treated as class `c`.

### mrb_top_self
```C
mrb_value mrb_top_self(mrb_state *mrb);
```
Returns top level receiver of script execution.
Returned value would be conveted `"main"` when converting to string.

### Starting execution.
```C
mrb_value mrb_run(mrb_state* mrb, struct RProc* proc, mrb_value recv);
mrb_value mrb_toplevel_run(mrb_state* mrb, struct RProc* proc);
mrb_value mrb_context_run(mrb_state* mrb, struct RProc* proc, mrb_value recv, unsigned int stack_keep);
```
Starts executing `proc`.
In `mrb_toplevel_run` `recv` value would be value of `mrb_top_self`
which means it will run on top level scope.

`stack_keep` is a initial stack size of `mrb`.
Normally it's `2 + mrb->c->ci->argc`(2 means receiver and block).

## Error handling.

### Assertion macros.

#### mrb_assert
```C
void mrb_assert(exp);
```
If `MRB_DEBUG` is defined it is alias of `assert` macro from `<assert.h>`.
Otherwise it would be expand to NOP.

#### mrb_assert_int_fit
```C
void mrb_assert_int_fit(type of checking value, checking value, type of fitting type, max value);
```
References `mrb_assert` in checking.
Assertion fails when 2nd argument couldn't fit to 3rd argument type or [0, max value].

#### mrb_static_assert
```
void mrb_static_assert(exp, msg);
```
Run static assert using C11 `_Static_assert`.
Currently in non C11 compiler fallbacks to `mrb_assert` macro instead.
(See discussions in [issue #2382](https://github.com/mruby/mruby/pull/2382) for detail).

### Exception class getting macros.
These macros requires `mrb_state*` variable named `mrb`.

macro name | getting class name
-----------|-------------------
`E_RUNTIME_ERROR` | `RuntimeError`
`E_TYPE_ERROR` | `TypeError`
`E_ARGUMENT_ERROR` | `ArgumentError`
`E_INDEX_ERROR` | `IndexError`
`E_RANGE_ERROR` | `RangeError`
`E_NAME_ERROR` | `NameError`
`E_NOMETHOD_ERROR` | `NoMemoryError`
`E_SCRIPT_ERROR` | `ScriptError`
`E_SYNTAX_ERROR` | `SyntaxError`
`E_LOCALJUMP_ERROR` | `LocalJumpError`
`E_REGEXP_ERROR` | `RegexpError`
`E_NOTIMP_ERROR` | `NotImplementedError`
`E_FLOATDOMAIN_ERROR` | `FloatDomainError`
`E_KEY_ERROR` | `KeyError`

### mrb_exc_new
```C
mrb_value mrb_exc_new(mrb_state *mrb, struct RClass *c, const char *ptr, long len);
```
Creates exception object from class `c` with message created from `ptr` and its length `len`.

### mrb_exc_raise
```C
void mrb_exc_raise(mrb_state *mrb, mrb_value exc);
```
Raise exception object `exc`.

### mrb_raise
```C
void mrb_raise(mrb_state *mrb, struct RClass *c, const char *msg);
```
Raise exception object of class `c` with message `msg`.

### mrb_raisef
```C
void mrb_raisef(mrb_state *mrb, struct RClass *c, const char *fmt, ...);
```
Raises exception object of class `c`
with formatted message created from `fmt` and rest arguments.

### mrb_name_error
```C
void mrb_name_error(mrb_state *mrb, mrb_sym id, const char *fmt, ...);
```
Raises `NameError` exception with name `id` and
formatted message created from `fmt` and rest arguments.

### mrb_warn
```C
void mrb_warn(mrb_state *mrb, const char *fmt, ...);
```
Prints formatted warning message created from `fmt` and rest arguments to `stderr`.

### mrb_bug
```C
void mrb_bug(mrb_state *mrb, const char *fmt, ...);
```
Prints formatted error message created from `fmt` and rest arguments to `stderr`
and exit with failure status.

### mrb_print_backtrace
```C
void mrb_print_backtrace(mrb_state *mrb);
```
Prints backtrace of `mrb->exc` to `stderr`.

### mrb_print_error
```C
void mrb_print_error(mrb_state *mrb);
```
Prints backtrace and `inspect` result of `mrb->exc` to `stderr`.

### mrb_p
```C
void mrb_p(mrb_state* mrb, mrb_value obj);
```
Prints `inspect` result of `obj` to `stdout`.

### mrb_format
```C
mrb_value mrb_format(mrb_state *mrb, const char *format, ...);
```
Creates formatted string from `format` and rest arguments.

### mrb_vformat
```C
#include <stdarg.h>

mrb_value mrb_vformat(mrb_state *mrb, const char *format, va_list ap);
```

Creates formatted string from `format` and `ap`.

This function isn't defined in **mruby.h** to avoid header dependency.
Copy the above code to use it.

## Garbage collector.

### enum gc_state
Value of `gc_state` field of `mrb_state`.
* `GC_STATE_NONE`
  * GC is on none of the following states.
* `GC_STATE_MARK`
  * GC is on mark phase.
* `GC_STATE_SWEEP`
  * GC is on sweep phase.

### Full garbage collection.
```C
void mrb_garbage_collect(mrb_state *mrb);
void mrb_full_gc(mrb_state *mrb);
```
Runs full garbage collection.

### mrb_incremental_gc
```C
void mrb_incremental_gc(mrb_state *mrb);
```
Runs incremental garbage collection.

### mrb_gc_arena_save
```C
int mrb_gc_arena_save(mrb_state* mrb);
```
Returns current size of GC arena.
Save this value in a beginning of or before loop scope.

### mrb_gc_arena_restore
```C
void mrb_gc_arena_restore(mrb_state *mrb, int idx);
```
Resize arena to `idx`.
`idx` should be a value returned from `mrb_gc_arena_save`.
Call this function on a end of scope if there is save arena index.

### mrb_gc_protect
```C
void mrb_gc_protect(mrb_state *mrb, mrb_value obj);
```
Push `obj` to GC arena.
GC arena is a stack to store temporary or moving around objects
(such as just created object, value popped from array, etc.).
It will bahave like Lua stack to protect those objects from GC.

### mrb_gc_mark
```C
void mrb_gc_mark(mrb_state *mrb,struct RBasic* obj);
```
Marks `obj`.

### mrb_gc_mark_value
```C
void mrb_gc_mark_value(mrb_state *mrb, mrb_value val);
```
Macro to mark `val` if its type is subclass of `RBasic`.

### mrb_field_write_barrier
```C
void mrb_field_write_barrier(mrb_state *mrb, struct RBasic *obj, struct RBasic *val);
```
Creates a write barrier to `obj` so that `val` would be marked.

### mrb_field_write_barrier_value
```C
void mrb_field_write_barrier_value(mrb_state *mrb, struct RBasic *obj, struct RBasic *val);
```
Marcro to create a write barrier of `obj` if `val`'s type is subclass of `RBasic`.

### mrb_write_barrier
```C
void mrb_write_barrier(mrb_state *mrb, struct RBasic *obj);
```
Create a write barrier of `obj`.

### mrb_obj_alloc
```C
struct RBasic *mrb_obj_alloc(mrb_state* mrb, enum mrb_vtype tt, struct RClass* c);
```
Creates `struct RBasic*` with type tag `tt` and class `c`.
Size of returned `struct RBasic` could satisfy all subclass of `struct RBasic`
so by processing particularly it could be any object in mruby.

## Memory pool.

### struct mrb_pool
Memory pool type.
`typedef`ed as `mrb_pool` too.

### mrb_pool_open
```C
struct mrb_pool* mrb_pool_open(mrb_state* mrb);
```
Creates a memory pool.
Returned memory pool must be deleted before `mrb` dies.

### mrb_pool_close
```C
void mrb_pool_close(struct mrb_pool* p);
```
Deletes memory pool `p`.

### mrb_pool_alloc
```C
void* mrb_pool_alloc(struct mrb_pool* p, size_t s);
```
Allocates memory region of size `s` from memory pool `p`.
Returned memory is aligned with `POOL_ALIGNMENT`.

### mrb_pool_realloc
```C
void* mrb_pool_realloc(struct mrb_pool* p, void* ptr, size_t oldlen, size_t newlen);
```
Reallocates memory region `ptr` of size `oldlen`
with new memory region with size `newlen` from memory pool `p`.

### mrb_pool_can_realloc
```C
mrb_bool mrb_pool_can_realloc(struct mrb_pool* p, void* ptr, size_t s);
```
Returns `TRUE` if `ptr` could be reallocated with size `s` in memory pool `p`,
otherwise `FALSE`.
