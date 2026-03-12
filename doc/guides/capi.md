<!-- summary: C API Reference -->

# C API Reference

This document covers the mruby C API for embedding and extending mruby.

**Contents:**
[Headers](#headers) |
[State Management](#state-management) |
[Values](#values) |
[Defining Classes and Modules](#defining-classes-and-modules) |
[Defining Methods](#defining-methods) |
[Parsing Arguments](#parsing-arguments) |
[Calling Ruby Methods from C](#calling-ruby-methods-from-c) |
[String Operations](#string-operations) |
[Array Operations](#array-operations) |
[Hash Operations](#hash-operations) |
[Wrapping C Structures](#wrapping-c-structures) |
[Exception Handling](#exception-handling) |
[Method Visibility](#method-visibility) |
[Proc and Block Handling](#proc-and-block-handling) |
[Fiber API](#fiber-api) |
[Compilation Contexts](#compilation-contexts) |
[Precompiled Bytecode](#precompiled-bytecode) |
[GC Arena](#gc-arena) |
[Memory Allocation](#memory-allocation)

## Headers

```c
#include <mruby.h>            /* core types, state, class/method definition */
#include <mruby/compile.h>    /* mrb_load_string, mrb_load_file */
#include <mruby/string.h>     /* string operations */
#include <mruby/array.h>      /* array operations */
#include <mruby/hash.h>       /* hash operations */
#include <mruby/data.h>       /* wrapping C structs */
#include <mruby/class.h>      /* class inspection */
#include <mruby/value.h>      /* value type macros */
#include <mruby/irep.h>       /* loading precompiled bytecode */
#include <mruby/error.h>      /* error handling (mrb_protect etc.) */
#include <mruby/variable.h>   /* instance/class/global variables */
```

## State Management

```c
mrb_state *mrb = mrb_open();       /* create state with all gems */
mrb_state *mrb = mrb_open_core();  /* create state without gems */
mrb_close(mrb);                    /* close and free state */
```

`mrb_open()` returns `NULL` on allocation failure. Always check the
return value.

## Values

All Ruby values are represented as `mrb_value` in C.

### Creating Values

```c
mrb_nil_value()                          /* nil */
mrb_true_value()                         /* true */
mrb_false_value()                        /* false */
mrb_bool_value(mrb_bool b)              /* true or false */
mrb_fixnum_value(mrb_int i)             /* Integer */
mrb_float_value(mrb_state *mrb, mrb_float f)  /* Float */
mrb_symbol_value(mrb_sym sym)           /* Symbol */
mrb_obj_value(void *p)                  /* object pointer to value */
mrb_cptr_value(mrb_state *mrb, void *p) /* C pointer */
```

### Type Checking

```c
mrb_type(v)          /* returns enum mrb_vtype */
mrb_nil_p(v)         /* true if nil */
mrb_integer_p(v)     /* true if Integer */
mrb_float_p(v)       /* true if Float */
mrb_symbol_p(v)      /* true if Symbol */
mrb_string_p(v)      /* true if String */
mrb_array_p(v)       /* true if Array */
mrb_hash_p(v)        /* true if Hash */
mrb_true_p(v)        /* true if true */
mrb_false_p(v)       /* true if false */
mrb_undef_p(v)       /* true if undefined */
mrb_immediate_p(v)   /* true if not a heap object */
```

### Extracting C Values

```c
mrb_integer(v)               /* mrb_int from Integer value */
mrb_float(v)                 /* mrb_float from Float value */
mrb_symbol(v)                /* mrb_sym from Symbol value */
mrb_ptr(v)                   /* void* from object value */
mrb_str_to_cstr(mrb, v)     /* const char* from String value */
```

### Value Types

| `mrb_vtype`        | Ruby Class          | Notes                    |
| ------------------ | ------------------- | ------------------------ |
| `MRB_TT_FALSE`     | FalseClass/NilClass | `nil` has `MRB_TT_FALSE` |
| `MRB_TT_TRUE`      | TrueClass           |                          |
| `MRB_TT_INTEGER`   | Integer             | Immediate value          |
| `MRB_TT_FLOAT`     | Float               | May be immediate         |
| `MRB_TT_SYMBOL`    | Symbol              | Immediate value          |
| `MRB_TT_STRING`    | String              | Heap object              |
| `MRB_TT_ARRAY`     | Array               | Heap object              |
| `MRB_TT_HASH`      | Hash                | Heap object              |
| `MRB_TT_OBJECT`    | Object              | User-defined classes     |
| `MRB_TT_CLASS`     | Class               |                          |
| `MRB_TT_MODULE`    | Module              |                          |
| `MRB_TT_PROC`      | Proc                |                          |
| `MRB_TT_CDATA`     | (C data)            | Wrapped C structs        |
| `MRB_TT_EXCEPTION` | Exception           |                          |
| `MRB_TT_FIBER`     | Fiber               |                          |

## Defining Classes and Modules

```c
/* Define a class under Object */
struct RClass *my_class = mrb_define_class(mrb, "MyClass", mrb->object_class);

/* Define a class under another class/module */
struct RClass *inner = mrb_define_class_under(mrb, outer, "Inner", mrb->object_class);

/* Define a module */
struct RClass *my_mod = mrb_define_module(mrb, "MyModule");
struct RClass *inner_mod = mrb_define_module_under(mrb, outer, "InnerMod");

/* Include/prepend a module */
mrb_include_module(mrb, my_class, my_mod);
mrb_prepend_module(mrb, my_class, my_mod);

/* Look up existing class/module */
struct RClass *c = mrb_class_get(mrb, "String");
struct RClass *m = mrb_module_get(mrb, "Kernel");

/* Define a constant */
mrb_define_const(mrb, my_class, "VERSION", mrb_str_new_lit(mrb, "1.0"));
```

## Defining Methods

All C methods have the same signature:

```c
static mrb_value
my_method(mrb_state *mrb, mrb_value self)
{
  /* self is the receiver */
  return mrb_nil_value();
}
```

Register with:

```c
mrb_define_method(mrb, klass, "name", my_method, MRB_ARGS_NONE());
mrb_define_class_method(mrb, klass, "name", my_method, MRB_ARGS_REQ(1));
mrb_define_module_function(mrb, mod, "name", my_method, MRB_ARGS_ANY());
```

### Argument Specifiers

| Macro                  | Meaning                               |
| ---------------------- | ------------------------------------- |
| `MRB_ARGS_NONE()`      | No arguments                          |
| `MRB_ARGS_REQ(n)`      | `n` required arguments                |
| `MRB_ARGS_OPT(n)`      | `n` optional arguments                |
| `MRB_ARGS_ARG(r,o)`    | `r` required + `o` optional           |
| `MRB_ARGS_REST()`      | Splat (`*args`)                       |
| `MRB_ARGS_BLOCK()`     | Block (`&block`)                      |
| `MRB_ARGS_ANY()`       | Any number (same as REST)             |
| `MRB_ARGS_KEY(n,rest)` | `n` keyword args, `rest`=1 for `**kw` |

These can be combined with `|`:

```c
MRB_ARGS_REQ(1) | MRB_ARGS_OPT(2) | MRB_ARGS_BLOCK()
```

## Parsing Arguments

`mrb_get_args()` extracts arguments from the Ruby call stack:

```c
mrb_int mrb_get_args(mrb_state *mrb, const char *format, ...);
```

### Format Specifiers

| Spec | Ruby Type     | C Type(s)                   | Notes                            |
| ---- | ------------- | --------------------------- | -------------------------------- |
| `o`  | any           | `mrb_value`                 | No type check                    |
| `i`  | Numeric       | `mrb_int`                   | Coerces to integer               |
| `f`  | Numeric       | `mrb_float`                 | Coerces to float                 |
| `b`  | any           | `mrb_bool`                  | Truthiness                       |
| `n`  | String/Symbol | `mrb_sym`                   | Converts to symbol               |
| `s`  | String        | `const char*, mrb_int`      | Pointer + length                 |
| `z`  | String        | `const char*`               | Null-terminated                  |
| `S`  | String        | `mrb_value`                 | String value                     |
| `A`  | Array         | `mrb_value`                 | Array value                      |
| `H`  | Hash          | `mrb_value`                 | Hash value                       |
| `C`  | Class         | `mrb_value`                 | Class/Module value               |
| `c`  | Class         | `struct RClass*`            | Class pointer                    |
| `a`  | Array         | `const mrb_value*, mrb_int` | Array pointer + length           |
| `d`  | C Data        | `void*`                     | Requires `mrb_data_type*`        |
| `&`  | Block         | `mrb_value`                 | Block argument                   |
| `*`  | rest          | `const mrb_value*, mrb_int` | Rest arguments                   |
| `\|` | —             | —                           | Following args are optional      |
| `?`  | —             | `mrb_bool`                  | Was previous optional arg given? |
| `:`  | keywords      | `mrb_kwargs`                | Keyword arguments                |

Adding `!` to `S`, `A`, `H`, `C`, `c`, `s`, `z`, `a`, `d` allows `nil`
(returns NULL/zero for nil).

### Examples

```c
/* def method(name, count) */
const char *name; mrb_int len, count;
mrb_get_args(mrb, "si", &name, &len, &count);

/* def method(required, optional=nil) */
mrb_value req, opt = mrb_nil_value();
mrb_get_args(mrb, "o|o", &req, &opt);

/* def method(*args) */
const mrb_value *args; mrb_int argc;
mrb_get_args(mrb, "*", &args, &argc);

/* def method(&block) */
mrb_value block;
mrb_get_args(mrb, "&", &block);

/* def method(name:, age: 0) */
mrb_sym kw_names[] = { mrb_intern_lit(mrb, "name"), mrb_intern_lit(mrb, "age") };
mrb_value kw_values[2];
mrb_kwargs kw = { 2, 1, kw_names, kw_values, NULL };
mrb_get_args(mrb, ":", &kw);
/* kw_values[0] = name (required), kw_values[1] = age (optional, undef if not given) */
```

## Calling Ruby Methods from C

```c
/* Call obj.method(arg1, arg2) */
mrb_funcall(mrb, obj, "method", 2, arg1, arg2);

/* Call with symbol (faster, no string lookup) */
mrb_funcall_id(mrb, obj, mrb_intern_lit(mrb, "method"), 2, arg1, arg2);

/* Call with argv array */
mrb_value argv[] = { arg1, arg2 };
mrb_funcall_argv(mrb, obj, mrb_intern_lit(mrb, "method"), 2, argv);

/* Call with block */
mrb_funcall_with_block(mrb, obj, mid, argc, argv, block);

/* Yield to block */
mrb_yield(mrb, block, arg);
mrb_yield_argv(mrb, block, argc, argv);
```

## String Operations

```c
/* Creation */
mrb_str_new_lit(mrb, "hello")              /* from string literal */
mrb_str_new(mrb, ptr, len)                 /* from pointer + length */
mrb_str_new_cstr(mrb, cstr)               /* from null-terminated C string */
mrb_str_new_static(mrb, ptr, len)          /* from static data (no copy) */

/* Access */
RSTRING_PTR(str)                           /* char* pointer */
RSTRING_LEN(str)                           /* length */
mrb_str_to_cstr(mrb, str)                 /* null-terminated (may copy) */

/* Modification */
mrb_str_cat(mrb, str, ptr, len)            /* append bytes */
mrb_str_cat_cstr(mrb, str, cstr)           /* append C string */
mrb_str_cat_str(mrb, str, str2)            /* append String */

/* Comparison */
mrb_str_equal(mrb, str1, str2)             /* equality */
mrb_str_cmp(mrb, str1, str2)              /* comparison (-1, 0, 1) */
```

## Array Operations

```c
/* Creation */
mrb_ary_new(mrb)                           /* empty array */
mrb_ary_new_capa(mrb, capa)               /* preallocated */
mrb_ary_new_from_values(mrb, n, vals)     /* from C array */

/* Access */
RARRAY_PTR(ary)                            /* mrb_value* pointer */
RARRAY_LEN(ary)                            /* length */
mrb_ary_entry(ary, idx)                    /* get element (no mrb needed) */

/* Modification */
mrb_ary_push(mrb, ary, val)               /* append */
mrb_ary_pop(mrb, ary)                     /* remove last */
mrb_ary_shift(mrb, ary)                   /* remove first */
mrb_ary_unshift(mrb, ary, val)            /* prepend */
mrb_ary_set(mrb, ary, idx, val)           /* set element */
mrb_ary_splice(mrb, ary, pos, len, rpl)   /* splice */
mrb_ary_concat(mrb, ary, other)           /* extend */
```

## Hash Operations

```c
/* Creation */
mrb_hash_new(mrb)                          /* empty hash */

/* Access */
mrb_hash_get(mrb, hash, key)              /* get value */
mrb_hash_fetch(mrb, hash, key, def)       /* get with default */
mrb_hash_key_p(mrb, hash, key)            /* key exists? */
mrb_hash_empty_p(mrb, hash)               /* empty? */
mrb_hash_size(mrb, hash)                  /* number of entries */

/* Modification */
mrb_hash_set(mrb, hash, key, val)         /* set key-value */
mrb_hash_delete_key(mrb, hash, key)       /* delete key */
mrb_hash_merge(mrb, hash1, hash2)         /* merge hash2 into hash1 */

/* Iteration */
mrb_hash_keys(mrb, hash)                  /* Array of keys */
mrb_hash_values(mrb, hash)                /* Array of values */
```

## Wrapping C Structures

To expose a C struct to Ruby, use `mrb_data_type` and `Data_Wrap_Struct`:

```c
/* 1. Define the data type with a name and destructor */
static void point_free(mrb_state *mrb, void *p) {
  mrb_free(mrb, p);
}

static const mrb_data_type point_type = {
  "Point", point_free
};

/* 2. Allocate and initialize */
static mrb_value
point_init(mrb_state *mrb, mrb_value self)
{
  mrb_float x, y;
  mrb_get_args(mrb, "ff", &x, &y);

  double *data = (double*)mrb_malloc(mrb, sizeof(double) * 2);
  data[0] = x;
  data[1] = y;

  DATA_PTR(self) = data;
  DATA_TYPE(self) = &point_type;

  return self;
}

/* 3. Access the wrapped data */
static mrb_value
point_x(mrb_state *mrb, mrb_value self)
{
  double *data = (double*)mrb_data_get_ptr(mrb, self, &point_type);
  return mrb_float_value(mrb, data[0]);
}

/* 4. Register the class */
struct RClass *point = mrb_define_class(mrb, "Point", mrb->object_class);
MRB_SET_INSTANCE_TT(point, MRB_TT_CDATA);
mrb_define_method(mrb, point, "initialize", point_init, MRB_ARGS_REQ(2));
mrb_define_method(mrb, point, "x", point_x, MRB_ARGS_NONE());
```

## Exception Handling

### Raising Exceptions

```c
mrb_raise(mrb, E_RUNTIME_ERROR, "something went wrong");
mrb_raisef(mrb, E_ARGUMENT_ERROR, "expected %d, got %d", expected, actual);
mrb_raise(mrb, E_TYPE_ERROR, "wrong type");
```

Common exception classes: `E_RUNTIME_ERROR`, `E_TYPE_ERROR`,
`E_ARGUMENT_ERROR`, `E_RANGE_ERROR`, `E_NAME_ERROR`,
`E_NOMETHOD_ERROR`, `E_NOTIMP_ERROR`, `E_KEY_ERROR`.

### Catching Exceptions

```c
/* Check after mrb_load_string or mrb_funcall */
mrb_value result = mrb_load_string(mrb, code);
if (mrb->exc) {
  mrb_print_error(mrb);
  mrb->exc = NULL;  /* clear exception */
}
```

### Protected Execution

`mrb_protect()` executes a function under protection. If an
exception is raised, it is captured as a return value instead of
propagating:

```c
static mrb_value
safe_operation(mrb_state *mrb, mrb_value data)
{
  /* This function might raise an exception */
  return mrb_funcall(mrb, data, "do_something", 0);
}

mrb_bool error;
mrb_value result = mrb_protect(mrb, safe_operation, data, &error);
if (error) {
  /* result contains the exception object; mrb->exc is cleared */
  mrb_value inspect = mrb_inspect(mrb, result);
  fprintf(stderr, "Error: %s\n", mrb_str_to_cstr(mrb, inspect));
}
```

**Note:** `mrb_protect` clears `mrb->exc` after catching the
exception. The exception is returned as `result`. Do not use
`mrb_print_error()` after `mrb_protect` — it reads `mrb->exc`
which is already `NULL`.

For lower-level protection with a `void*` callback:

```c
static mrb_value
body(mrb_state *mrb, void *userdata)
{
  /* ... */
}

mrb_bool error;
mrb_value result = mrb_protect_error(mrb, body, userdata, &error);
```

### Rescue

`mrb_rescue()` catches `StandardError` (like Ruby's `rescue`):

```c
static mrb_value
body_func(mrb_state *mrb, mrb_value body_data)
{
  return mrb_funcall(mrb, body_data, "risky_method", 0);
}

static mrb_value
rescue_func(mrb_state *mrb, mrb_value rescue_data)
{
  /* handle error, rescue_data is the data passed in */
  return mrb_nil_value();
}

mrb_value result = mrb_rescue(mrb, body_func, body_data,
                              rescue_func, rescue_data);
```

To rescue specific exception classes:

```c
struct RClass *classes[] = {
  E_ARGUMENT_ERROR,
  mrb_class_get(mrb, "IOError")
};
mrb_value result = mrb_rescue_exceptions(mrb, body_func, body_data,
                                         rescue_func, rescue_data,
                                         2, classes);
```

### Ensure

`mrb_ensure()` guarantees cleanup runs regardless of exceptions
(like Ruby's `ensure`):

```c
static mrb_value
body_func(mrb_state *mrb, mrb_value data)
{
  return mrb_funcall(mrb, data, "process", 0);
}

static mrb_value
cleanup_func(mrb_state *mrb, mrb_value data)
{
  mrb_funcall(mrb, data, "close", 0);
  return mrb_nil_value();
}

mrb_value result = mrb_ensure(mrb, body_func, body_data,
                              cleanup_func, cleanup_data);
```

The ensure function always executes. If the body raises an
exception, the ensure runs and then the exception is re-raised.

### Error State Management

```c
mrb_bool mrb_check_error(mrb_state *mrb);  /* check and clear mrb->exc */
void mrb_clear_error(mrb_state *mrb);      /* clear mrb->exc */
```

## Method Visibility

```c
/* Public (default) */
mrb_define_method(mrb, klass, "name", func, MRB_ARGS_NONE());

/* Private - only callable without explicit receiver */
mrb_define_private_method(mrb, klass, "name", func, MRB_ARGS_NONE());

/* Class method (singleton method on the class object) */
mrb_define_class_method(mrb, klass, "name", func, MRB_ARGS_NONE());

/* Module function (both module method and private instance method) */
mrb_define_module_function(mrb, mod, "name", func, MRB_ARGS_NONE());

/* Singleton method on a specific object */
mrb_define_singleton_method(mrb, obj, "name", func, MRB_ARGS_NONE());

/* Method alias: alias new_name old_name */
mrb_define_alias(mrb, klass, "new_name", "old_name");

/* Remove a method */
mrb_undef_method(mrb, klass, "name");
mrb_undef_class_method(mrb, klass, "name");
```

All `_method` variants have `_id` counterparts that accept
`mrb_sym` instead of `const char*` for better performance.

## Proc and Block Handling

### Creating Procs from C Functions

```c
/* Simple C function proc */
struct RProc *proc = mrb_proc_new_cfunc(mrb, my_func);

/* C closure with captured local variables */
struct RProc *proc = mrb_closure_new_cfunc(mrb, my_func, nlocals);
```

### C Functions with Environment (requires mruby-proc-ext)

Store values in a proc's environment, accessible from the C
function:

```c
mrb_value env_values[] = { mrb_fixnum_value(42) };
struct RProc *proc = mrb_proc_new_cfunc_with_env(mrb, my_func, 1, env_values);

/* Inside my_func, retrieve environment values */
static mrb_value my_func(mrb_state *mrb, mrb_value self)
{
  mrb_value val = mrb_proc_cfunc_env_get(mrb, 0); /* index 0 */
  return val;
}
```

## Fiber API

```c
#include <mruby.h>    /* fiber types and functions */
```

### Creating and Using Fibers

```c
/* Create a fiber from a proc */
mrb_value fiber = mrb_fiber_new(mrb, proc);

/* Resume the fiber with arguments */
mrb_value args[] = { mrb_fixnum_value(1) };
mrb_value result = mrb_fiber_resume(mrb, fiber, 1, args);

/* Check if fiber is still alive */
mrb_bool alive = mrb_test(mrb_fiber_alive_p(mrb, fiber));
```

### Yielding from C

`mrb_fiber_yield()` can only be used as the return value of a C
function — no code may execute after it:

```c
static mrb_value
my_yield_method(mrb_state *mrb, mrb_value self)
{
  mrb_value yield_args[] = { mrb_str_new_lit(mrb, "yielded") };
  return mrb_fiber_yield(mrb, 1, yield_args);  /* must be returned directly */
}
```

### Fiber States

| State                   | Meaning                          |
| ----------------------- | -------------------------------- |
| `MRB_FIBER_CREATED`     | Created but not yet resumed      |
| `MRB_FIBER_RUNNING`     | Currently executing              |
| `MRB_FIBER_RESUMED`     | Resumed another fiber            |
| `MRB_FIBER_SUSPENDED`   | Yielded, waiting to resume       |
| `MRB_FIBER_TRANSFERRED` | Transferred via `Fiber#transfer` |
| `MRB_FIBER_TERMINATED`  | Finished execution               |

**Limitation:** fibers cannot yield across C function boundaries.
You cannot call `mrb_fiber_yield` from within a C-implemented
method, except via `mrb_fiber_yield` at function return.

## Compilation Contexts

For advanced compilation control, use `mrb_ccontext`:

```c
#include <mruby/compile.h>

mrb_ccontext *cxt = mrb_ccontext_new(mrb);

/* Set source filename for error messages and debug info */
mrb_ccontext_filename(mrb, cxt, "my_script.rb");

/* Compile and execute with context */
mrb_value result = mrb_load_string_cxt(mrb, "1 + 2", cxt);

/* Clean up */
mrb_ccontext_free(mrb, cxt);
```

### Context Options

The `mrb_ccontext` structure provides several flags:

| Field            | Purpose                                 |
| ---------------- | --------------------------------------- |
| `capture_errors` | Collect parse errors instead of raising |
| `no_exec`        | Compile without executing (get RProc)   |
| `no_optimize`    | Disable peephole optimizations          |
| `no_ext_ops`     | Disable extended operand instructions   |
| `keep_lv`        | Preserve local variables across loads   |

### Loading with Context

```c
mrb_load_string_cxt(mrb, code, cxt);         /* string + context */
mrb_load_nstring_cxt(mrb, code, len, cxt);   /* with explicit length */
mrb_load_file_cxt(mrb, fp, cxt);             /* file + context */
mrb_load_detect_file_cxt(mrb, fp, cxt);      /* auto-detect .mrb or .rb */
```

## Precompiled Bytecode

Load `.mrb` files compiled by `mrbc`:

```c
#include <mruby/irep.h>

/* From byte array (generated by mrbc -B) */
mrb_value result = mrb_load_irep(mrb, bytecode);

/* From buffer with explicit size (safer, bounds-checked) */
mrb_value result = mrb_load_irep_buf(mrb, buf, size);

/* From file */
FILE *fp = fopen("script.mrb", "rb");
mrb_value result = mrb_load_irep_file(mrb, fp);
fclose(fp);

/* Load without executing (returns irep for inspection) */
mrb_irep *irep = mrb_read_irep(mrb, bytecode);
```

All `_irep` loading functions have `_cxt` variants that accept
a compilation context.

### Deployment Pattern

Ahead-of-time compilation eliminates the need for the compiler gem
at runtime:

```shell
# Compile to C array
mrbc -Bscript_bytecode script.rb

# This generates a C header with:
# const uint8_t script_bytecode[];
```

```c
#include "script.mrb.h"

mrb_state *mrb = mrb_open_core();  /* no compiler needed */
mrb_load_irep(mrb, script_bytecode);
```

**Important:** wrap bytecode loading in arena save/restore when
loading multiple scripts:

```c
int ai = mrb_gc_arena_save(mrb);
mrb_load_irep(mrb, script1);
mrb_gc_arena_restore(mrb, ai);

ai = mrb_gc_arena_save(mrb);
mrb_load_irep(mrb, script2);
mrb_gc_arena_restore(mrb, ai);
```

## Symbols

```c
/* Create symbol from string */
mrb_sym sym = mrb_intern_lit(mrb, "name");         /* from literal */
mrb_sym sym = mrb_intern_cstr(mrb, cstr);          /* from C string */
mrb_sym sym = mrb_intern(mrb, ptr, len);           /* from pointer + length */

/* Symbol to string */
const char *name = mrb_sym_name(mrb, sym);
mrb_int len;
const char *name = mrb_sym_name_len(mrb, sym, &len);
```

## Instance Variables

```c
/* Get/set instance variables on an object */
mrb_iv_get(mrb, obj, mrb_intern_lit(mrb, "@x"));
mrb_iv_set(mrb, obj, mrb_intern_lit(mrb, "@x"), val);
mrb_iv_defined(mrb, obj, mrb_intern_lit(mrb, "@x"));
mrb_iv_remove(mrb, obj, mrb_intern_lit(mrb, "@x"));
```

## Global Variables

```c
mrb_gv_get(mrb, mrb_intern_lit(mrb, "$verbose"));
mrb_gv_set(mrb, mrb_intern_lit(mrb, "$verbose"), mrb_true_value());
```

## Class Variables

```c
mrb_cv_get(mrb, klass, mrb_intern_lit(mrb, "@@count"));
mrb_cv_set(mrb, klass, mrb_intern_lit(mrb, "@@count"), mrb_fixnum_value(0));
```

## Loading and Executing Code

```c
/* Load and execute a string (requires mruby-compiler gem) */
mrb_value result = mrb_load_string(mrb, "1 + 2");

/* Load and execute a file */
FILE *f = fopen("script.rb", "r");
mrb_value result = mrb_load_file(mrb, f);
fclose(f);

/* Load precompiled bytecode (no compiler needed) */
mrb_value result = mrb_load_irep(mrb, bytecode_array);
```

## GC Arena

When creating many temporary Ruby objects in C, use the GC arena to
prevent them from being collected prematurely:

```c
int ai = mrb_gc_arena_save(mrb);
/* create temporary objects here */
mrb_gc_arena_restore(mrb, ai);
```

See [gc-arena-howto.md](gc-arena-howto.md) for details.

## Memory Allocation

```c
void *p = mrb_malloc(mrb, size);           /* raises on failure */
void *p = mrb_calloc(mrb, nmemb, size);    /* zero-initialized */
void *p = mrb_realloc(mrb, ptr, size);     /* resize */
mrb_free(mrb, p);                          /* free */

/* NULL-returning variants (for custom error handling) */
void *p = mrb_malloc_simple(mrb, size);
void *p = mrb_realloc_simple(mrb, ptr, size);
```

## Type Conversion

```c
mrb_obj_as_string(mrb, val)      /* to_s */
mrb_inspect(mrb, val)            /* inspect */
mrb_any_to_s(mrb, val)           /* default to_s */
mrb_str_to_integer(mrb, str, base, badcheck)  /* String to Integer */
mrb_str_to_dbl(mrb, str, badcheck)            /* String to Float */
mrb_ensure_float_type(mrb, val)               /* ensure Float */
```

## Object Comparison

```c
mrb_equal(mrb, a, b)    /* Ruby == */
mrb_eql(mrb, a, b)      /* Ruby eql? */
mrb_obj_eq(mrb, a, b)   /* Ruby equal? (identity) */
mrb_cmp(mrb, a, b)      /* Ruby <=> (returns mrb_int) */
```

## Object Inspection

```c
mrb_obj_classname(mrb, obj)          /* class name as C string */
mrb_obj_class(mrb, obj)              /* class as RClass* */
mrb_obj_is_kind_of(mrb, obj, klass)  /* is_a? / kind_of? */
mrb_obj_respond_to(mrb, klass, mid)  /* respond_to? */
mrb_obj_id(obj)                      /* object_id */
mrb_obj_freeze(mrb, obj)             /* freeze */
mrb_obj_dup(mrb, obj)                /* dup */
```

## Compile-Time Flags

When compiling C code that uses mruby, you must use the same flags as
the library was built with. Use `mruby-config` to get them:

```console
$ build/host/bin/mruby-config --cflags    # compiler flags
$ build/host/bin/mruby-config --ldflags   # linker flags
$ build/host/bin/mruby-config --libs      # libraries
```

Key macros that affect ABI:

| Macro             | Effect                                   |
| ----------------- | ---------------------------------------- |
| `MRB_NO_BOXING`   | Struct-based values (larger, debuggable) |
| `MRB_WORD_BOXING` | Single-word values (fast, 32-bit safe)   |
| `MRB_NAN_BOXING`  | NaN-tagged values (default on 32-bit)    |
| `MRB_NO_FLOAT`    | Disable Float support                    |
| `MRB_INT64`       | 64-bit integers                          |
| `MRB_USE_FLOAT32` | 32-bit floats                            |

Mismatching these between library and application causes silent
data corruption.
