# mruby.h

Basic header of mruby.
It includes **mrbconf.h**, **mruby/value.h**, **mruby/version.h** internally.

## `mrb_state` management

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

## mrb_noreturn
```C
#define mrb_noreturn /* platform specific value */
```
Macro of noreturn attribute.
When function is declared with this attribute it won't return.
Used in function that only raises exceptions.

## Argument specifier

### mrb_aspec
```C
typedef uint32_t mrb_aspec;
```
Type of argument specifier value.
Combine arugments specifiers with `|` operator.

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
`o`|`Object`|`mrb_value`|Could be used to retreive any type of argument
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
The usage is same as std C function except it takes `mrb` argument and
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

## Undocumented
```C
typedef uint32_t mrb_code;

typedef struct {
mrb_sym mid;
struct RProc *proc;
mrb_value *stackent;
int nregs;
int ridx;
int eidx;
struct REnv *env;
mrb_code *pc;                 /* return address */
mrb_code *err;                /* error position */
int argc;
int acc;
struct RClass *target_class;
} mrb_callinfo;

struct mrb_context {
struct mrb_context *prev;

mrb_value *stack;                       /* stack of virtual machine */
mrb_value *stbase, *stend;

mrb_callinfo *ci;
mrb_callinfo *cibase, *ciend;

mrb_code **rescue;                      /* exception handler stack */
int rsize;
struct RProc **ensure;                  /* ensure handler stack */
int esize;

enum mrb_fiber_state status;
struct RFiber *fib;
};

enum gc_state {
GC_STATE_NONE = 0,
GC_STATE_MARK,
GC_STATE_SWEEP
};

struct mrb_jmpbuf;

typedef void (*mrb_atexit_func)(struct mrb_state*);

typedef struct mrb_state {
struct mrb_jmpbuf *jmp;

mrb_allocf allocf;                      /* memory allocation function */

struct mrb_context *c;
struct mrb_context *root_c;

struct RObject *exc;                    /* exception */
struct iv_tbl *globals;                 /* global variable table */

struct RObject *top_self;
struct RClass *object_class;            /* Object class */
struct RClass *class_class;
struct RClass *module_class;
struct RClass *proc_class;
struct RClass *string_class;
struct RClass *array_class;
struct RClass *hash_class;

struct RClass *float_class;
struct RClass *fixnum_class;
struct RClass *true_class;
struct RClass *false_class;
struct RClass *nil_class;
struct RClass *symbol_class;
struct RClass *kernel_module;

struct heap_page *heaps;                /* heaps for GC */
struct heap_page *sweeps;
struct heap_page *free_heaps;
size_t live; /* count of live objects */
#ifdef MRB_GC_FIXED_ARENA
struct RBasic *arena[MRB_GC_ARENA_SIZE]; /* GC protection array */
#else
struct RBasic **arena;                   /* GC protection array */
int arena_capa;
#endif
int arena_idx;

enum gc_state gc_state; /* state of gc */
int current_white_part; /* make white object by white_part */
struct RBasic *gray_list; /* list of gray objects to be traversed incrementally */
struct RBasic *atomic_gray_list; /* list of objects to be traversed atomically */
size_t gc_live_after_mark;
size_t gc_threshold;
int gc_interval_ratio;
int gc_step_ratio;
mrb_bool gc_disabled:1;
mrb_bool gc_full:1;
mrb_bool is_generational_gc_mode:1;
mrb_bool out_of_memory:1;
size_t majorgc_old_threshold;
struct alloca_header *mems;

mrb_sym symidx;
struct kh_n2s *name2sym;      /* symbol table */

#ifdef ENABLE_DEBUG
void (*code_fetch_hook)(struct mrb_state* mrb, struct mrb_irep *irep, mrb_code *pc, mrb_value *regs);
void (*debug_op_hook)(struct mrb_state* mrb, struct mrb_irep *irep, mrb_code *pc, mrb_value *regs);
#endif

struct RClass *eException_class;
struct RClass *eStandardError_class;
struct RObject *nomem_err;              /* pre-allocated NoMemoryError */

void *ud; /* auxiliary data */

#ifdef MRB_FIXED_STATE_ATEXIT_STACK
mrb_atexit_func atexit_stack[MRB_FIXED_STATE_ATEXIT_STACK_SIZE];
#else
mrb_atexit_func *atexit_stack;
#endif
mrb_int atexit_stack_len;
} mrb_state;

void mrb_include_module(mrb_state*, struct RClass*, struct RClass*);

void mrb_define_const(mrb_state*, struct RClass*, const char *name, mrb_value);
void mrb_undef_method(mrb_state*, struct RClass*, const char*);
void mrb_undef_class_method(mrb_state*, struct RClass*, const char*);
mrb_value mrb_obj_new(mrb_state *mrb, struct RClass *c, mrb_int argc, const mrb_value *argv);
#define mrb_class_new_instance(mrb,argc,argv,c) mrb_obj_new(mrb,c,argc,argv)
mrb_value mrb_instance_new(mrb_state *mrb, mrb_value cv);
struct RClass * mrb_class_new(mrb_state *mrb, struct RClass *super);
struct RClass * mrb_module_new(mrb_state *mrb);
mrb_bool mrb_class_defined(mrb_state *mrb, const char *name);
struct RClass * mrb_class_get(mrb_state *mrb, const char *name);
struct RClass * mrb_class_get_under(mrb_state *mrb, struct RClass *outer, const char *name);
struct RClass * mrb_module_get(mrb_state *mrb, const char *name);
struct RClass * mrb_module_get_under(mrb_state *mrb, struct RClass *outer, const char *name);

mrb_value mrb_obj_dup(mrb_state *mrb, mrb_value obj);
mrb_value mrb_check_to_integer(mrb_state *mrb, mrb_value val, const char *method);
mrb_bool mrb_obj_respond_to(mrb_state *mrb, struct RClass* c, mrb_sym mid);

/* `strlen` for character string literals (use with caution or `strlen` instead)
    Adjacent string literals are concatenated in C/C++ in translation phase 6.
    If `lit` is not one, the compiler will report a syntax error:
     MSVC: "error C2143: syntax error : missing ')' before 'string'"
     GCC:  "error: expected ')' before string constant"
*/
#define mrb_strlen_lit(lit) (sizeof(lit "") - 1)

mrb_value mrb_funcall(mrb_state*, mrb_value, const char*, mrb_int,...);
mrb_value mrb_funcall_argv(mrb_state*, mrb_value, mrb_sym, mrb_int, const mrb_value*);
mrb_value mrb_funcall_with_block(mrb_state*, mrb_value, mrb_sym, mrb_int, const mrb_value*, mrb_value);
mrb_sym mrb_intern_cstr(mrb_state*,const char*);
mrb_sym mrb_intern(mrb_state*,const char*,size_t);
mrb_sym mrb_intern_static(mrb_state*,const char*,size_t);
#define mrb_intern_lit(mrb, lit) mrb_intern_static(mrb, lit, mrb_strlen_lit(lit))
mrb_sym mrb_intern_str(mrb_state*,mrb_value);
mrb_value mrb_check_intern_cstr(mrb_state*,const char*);
mrb_value mrb_check_intern(mrb_state*,const char*,size_t);
mrb_value mrb_check_intern_str(mrb_state*,mrb_value);
const char *mrb_sym2name(mrb_state*,mrb_sym);
const char *mrb_sym2name_len(mrb_state*,mrb_sym,mrb_int*);
mrb_value mrb_sym2str(mrb_state*,mrb_sym);

struct RBasic *mrb_obj_alloc(mrb_state*, enum mrb_vtype, struct RClass*);

mrb_value mrb_str_new(mrb_state *mrb, const char *p, size_t len);
mrb_value mrb_str_new_cstr(mrb_state*, const char*);
mrb_value mrb_str_new_static(mrb_state *mrb, const char *p, size_t len);
#define mrb_str_new_lit(mrb, lit) mrb_str_new_static(mrb, (lit), mrb_strlen_lit(lit))

mrb_state* mrb_open_core(mrb_allocf, void *ud);

void* mrb_default_allocf(mrb_state*, void*, size_t, void*);

mrb_value mrb_top_self(mrb_state *);
mrb_value mrb_run(mrb_state*, struct RProc*, mrb_value);
mrb_value mrb_toplevel_run(mrb_state*, struct RProc*);
mrb_value mrb_context_run(mrb_state*, struct RProc*, mrb_value, unsigned int);

void mrb_p(mrb_state*, mrb_value);
mrb_int mrb_obj_id(mrb_value obj);
mrb_sym mrb_obj_to_sym(mrb_state *mrb, mrb_value name);

mrb_bool mrb_obj_eq(mrb_state*, mrb_value, mrb_value);
mrb_bool mrb_obj_equal(mrb_state*, mrb_value, mrb_value);
mrb_bool mrb_equal(mrb_state *mrb, mrb_value obj1, mrb_value obj2);
mrb_value mrb_Integer(mrb_state *mrb, mrb_value val);
mrb_value mrb_Float(mrb_state *mrb, mrb_value val);
mrb_value mrb_inspect(mrb_state *mrb, mrb_value obj);
mrb_bool mrb_eql(mrb_state *mrb, mrb_value obj1, mrb_value obj2);

void mrb_garbage_collect(mrb_state*);
void mrb_full_gc(mrb_state*);
void mrb_incremental_gc(mrb_state *);
int mrb_gc_arena_save(mrb_state*);
void mrb_gc_arena_restore(mrb_state*,int);
void mrb_gc_mark(mrb_state*,struct RBasic*);
#define mrb_gc_mark_value(mrb,val) do {\
  if (MRB_TT_HAS_BASIC_P(mrb_type(val))) mrb_gc_mark((mrb), mrb_basic_ptr(val)); \
} while (0)
void mrb_field_write_barrier(mrb_state *, struct RBasic*, struct RBasic*);
#define mrb_field_write_barrier_value(mrb, obj, val) do{\
  if (MRB_TT_HAS_BASIC_P(mrb_type(val))) mrb_field_write_barrier((mrb), (obj), mrb_basic_ptr(val)); \
} while (0)
void mrb_write_barrier(mrb_state *, struct RBasic*);

mrb_value mrb_check_convert_type(mrb_state *mrb, mrb_value val, enum mrb_vtype type, const char *tname, const char *method);
mrb_value mrb_any_to_s(mrb_state *mrb, mrb_value obj);
const char * mrb_obj_classname(mrb_state *mrb, mrb_value obj);
struct RClass* mrb_obj_class(mrb_state *mrb, mrb_value obj);
mrb_value mrb_class_path(mrb_state *mrb, struct RClass *c);
mrb_value mrb_convert_type(mrb_state *mrb, mrb_value val, enum mrb_vtype type, const char *tname, const char *method);
mrb_bool mrb_obj_is_kind_of(mrb_state *mrb, mrb_value obj, struct RClass *c);
mrb_value mrb_obj_inspect(mrb_state *mrb, mrb_value self);
mrb_value mrb_obj_clone(mrb_state *mrb, mrb_value self);

mrb_value mrb_exc_new(mrb_state *mrb, struct RClass *c, const char *ptr, long len);
mrb_noreturn void mrb_exc_raise(mrb_state *mrb, mrb_value exc);

mrb_noreturn void mrb_raise(mrb_state *mrb, struct RClass *c, const char *msg);
mrb_noreturn void mrb_raisef(mrb_state *mrb, struct RClass *c, const char *fmt, ...);
mrb_noreturn void mrb_name_error(mrb_state *mrb, mrb_sym id, const char *fmt, ...);
void mrb_warn(mrb_state *mrb, const char *fmt, ...);
mrb_noreturn void mrb_bug(mrb_state *mrb, const char *fmt, ...);
void mrb_print_backtrace(mrb_state *mrb);
void mrb_print_error(mrb_state *mrb);

/* macros to get typical exception objects
   note:
   + those E_* macros requires mrb_state* variable named mrb.
   + exception objects obtained from those macros are local to mrb
*/
#define E_RUNTIME_ERROR             (mrb_class_get(mrb, "RuntimeError"))
#define E_TYPE_ERROR                (mrb_class_get(mrb, "TypeError"))
#define E_ARGUMENT_ERROR            (mrb_class_get(mrb, "ArgumentError"))
#define E_INDEX_ERROR               (mrb_class_get(mrb, "IndexError"))
#define E_RANGE_ERROR               (mrb_class_get(mrb, "RangeError"))
#define E_NAME_ERROR                (mrb_class_get(mrb, "NameError"))
#define E_NOMETHOD_ERROR            (mrb_class_get(mrb, "NoMethodError"))
#define E_SCRIPT_ERROR              (mrb_class_get(mrb, "ScriptError"))
#define E_SYNTAX_ERROR              (mrb_class_get(mrb, "SyntaxError"))
#define E_LOCALJUMP_ERROR           (mrb_class_get(mrb, "LocalJumpError"))
#define E_REGEXP_ERROR              (mrb_class_get(mrb, "RegexpError"))

#define E_NOTIMP_ERROR              (mrb_class_get(mrb, "NotImplementedError"))
#define E_FLOATDOMAIN_ERROR         (mrb_class_get(mrb, "FloatDomainError"))

#define E_KEY_ERROR                 (mrb_class_get(mrb, "KeyError"))

mrb_value mrb_yield(mrb_state *mrb, mrb_value b, mrb_value arg);
mrb_value mrb_yield_argv(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv);
mrb_value mrb_yield_with_class(mrb_state *mrb, mrb_value b, mrb_int argc, const mrb_value *argv, mrb_value self, struct RClass *c);

void mrb_gc_protect(mrb_state *mrb, mrb_value obj);
mrb_value mrb_to_int(mrb_state *mrb, mrb_value val);
#define mrb_int(mrb, val) mrb_fixnum(mrb_to_int(mrb, val))
void mrb_check_type(mrb_state *mrb, mrb_value x, enum mrb_vtype t);

typedef enum call_type {
  CALL_PUBLIC,
  CALL_FCALL,
  CALL_VCALL,
  CALL_TYPE_MAX
} call_type;

void mrb_define_alias(mrb_state *mrb, struct RClass *klass, const char *name1, const char *name2);
const char *mrb_class_name(mrb_state *mrb, struct RClass* klass);
void mrb_define_global_const(mrb_state *mrb, const char *name, mrb_value val);

mrb_value mrb_attr_get(mrb_state *mrb, mrb_value obj, mrb_sym id);

mrb_bool mrb_respond_to(mrb_state *mrb, mrb_value obj, mrb_sym mid);
mrb_bool mrb_obj_is_instance_of(mrb_state *mrb, mrb_value obj, struct RClass* c);

/* memory pool implementation */
typedef struct mrb_pool mrb_pool;
struct mrb_pool* mrb_pool_open(mrb_state*);
void mrb_pool_close(struct mrb_pool*);
void* mrb_pool_alloc(struct mrb_pool*, size_t);
void* mrb_pool_realloc(struct mrb_pool*, void*, size_t oldlen, size_t newlen);
mrb_bool mrb_pool_can_realloc(struct mrb_pool*, void*, size_t);
void* mrb_alloca(mrb_state *mrb, size_t);

void mrb_state_atexit(mrb_state *mrb, mrb_atexit_func func);

#ifdef MRB_DEBUG
#include <assert.h>
#define mrb_assert(p) assert(p)
#define mrb_assert_int_fit(t1,n,t2,max) assert((n)>=0 && ((sizeof(n)<=sizeof(t2))||(n<=(t1)(max))))
#else
#define mrb_assert(p) ((void)0)
#define mrb_assert_int_fit(t1,n,t2,max) ((void)0)
#endif

#if __STDC_VERSION__ >= 201112L
#define mrb_static_assert(exp, str) _Static_assert(exp, str)
#else
#define mrb_static_assert(exp, str) mrb_assert(exp)
#endif

mrb_value mrb_format(mrb_state *mrb, const char *format, ...);
```
