# mruby/proc.h

## struct REnv
Subclass of `RBasic` to represent closure environment.
The usage of some `RBasic` field is different from normal use.

### Members.
* `mrb_value *stack;`
  * mruby stack of environment.
* `mrb_sym mid;`
  * Current method name.
* `ptrdiff_t cioff;`
  * Call information offset of environment.
  * If this value is `-1` the environment isn't part of current live call stack.

## struct RProc
Subclass of `RBasic` to represent `Proc`.

## Macros.

### MRB_ENV_STACK_LEN
```C
uint32_t MRB_ENV_STACK_LEN(struct REnv *e);
```
Macro to take stack length of `e`.

### MRB_ENV_UNSHARE_STACK
```C
void MRB_ENV_UNSHARE_STACK(struct REnv *e);
```
Macro to mark `e` not part of current live stack.

### MRB_ENV_STACK_SHARED_P
```C
mrb_bool MRB_ENV_STACK_SHARED_P(struct REnv *e);
```
Macro to get whether `e` is part of current live stack.

### Argument specifier accessing macros.

macro | description
======|============
`mrb_int MRB_ASPEC_REQ(mrb_aspec a);` | Get required arguments.
`mrb_int MRB_ASPEC_OPT(mrb_aspec a);` | Get optional arguments.
`mrb_bool MRB_ASPEC_REST(mrb_aspec a);` | Get whether rest arguments is enabled.
`mrb_int MRB_ASPEC_POST(mrb_aspec a);` | Get post arguments.
`mrb_bool MRB_ASPEC_KEY(mrb_aspec a);` | Get whether keyword arguments is enabled.
`mrb_int MRB_ASPEC_KDICT(mrb_aspec a);` | Get keyword arguments.
`mrb_bool MRB_ASPEC_BLOCK(mrb_aspec a);` | Get whether block argument is enabled.

### MRB_PROC_CFUNC_P
```C
mrb_bool MRB_PROC_CFUNC_P(struct RProc *p);
```
Macro to check `p` is a c function Proc.

### MRB_PROC_STRICT_P
```C
mrb_bool MRB_PROC_STRICT_P(struct RProc *p);
```
Macro to check `p` is a strict argument checked Proc.

### mrb_proc_ptr
```C
struct RProc *mrb_proc_ptr(mrb_value v);
```
Macro to take `struct RProc*` from `mrb_value`.
`v`'s type tag must be `MRB_TT_PROC`.

## mrb_proc_new
```C
struct RProc *mrb_proc_new(mrb_state* mrb, mrb_irep* irep);
```
Creates `struct RProc*` from compiled mruby script.

## mrb_proc_new_cfunc
```C
struct RProc *mrb_proc_new_cfunc(mrb_state *mrb, mrb_func_t func);
```
Creates `struct RProc*` from C interface.

## mrb_closure_new
```C
struct RProc *mrb_closure_new(mrb_state *mrb, mrb_irep *irep);
```
Creates `struct RProc*` from compiled mruby script with closure environment.

## mrb_closure_new_cfunc
```C
struct RProc *mrb_closure_new_cfunc(mrb_state *mrb, mrb_func_t func, int nlocals);
```
Creates `struct RProc*` from C interface with closure environment.

## mrb_proc_copy
```C
void mrb_proc_copy(struct RProc *a, struct RProc *b);
```
Copies Proc `b` to Proc `a`.
`a` must be an uninitialized `struct RProc*`.

## mrb_proc_new_cfunc_with_env
```C
struct RProc *mrb_proc_new_cfunc_with_env(mrb_state* mrb, mrb_func_t func, mrb_int argc, const mrb_value* argv);
```
Creates `struct RProc*` from C interface with user defined environment.

## mrb_cfunc_env_get
```C
mrb_value mrb_cfunc_env_get(mrb_state* mrb, mrb_int index);
```
Get environment value for C interface.
`index` is the stack index of environment.
