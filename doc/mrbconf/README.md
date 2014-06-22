# mruby configuration macros.

## How to use these macros.
You can use mrbconfs with following ways:
* Write them in `mrbconf.h`.
 * Using compiler flags is prefered  when building a cross binaries or multiple mruby binaries
 since it's easier to use different mrbconf per each `MRuby::Build`.
 * Most flags can be enabled by just commenting in.
* Pass them as compiler flags.
 * Make sure you pass the same flags to all compilers since some mrbconf(e.g., `MRB_GC_FIXED_ARENA`)
 changes `struct` layout and cause memory access error when C and other language(e.g., C++) is mixed.

## stdio setting.
`ENABLE_STDIO`
* Will be defined automatically if `DISABLE_STDIO` isn't defined.
* Uses `<stdio.h>` functions.

`DISABLE_STDIO`
* When defined `<stdio.h>` functions won't be used.

## Debug macros.
`ENABLE_DEBUG`
* When defined code fetch hook and debug OP hook will be enabled.
* When using any of the hook set function pointer `code_fetch_hook` and/or `debug_op_hook` of `mrb_state`.
* Fetch hook will be called before any OP.
* Debug OP hook will be called when dispatching `OP_DEBUG`.

`DISABLE_DEBUG`
* Will be define automatically if `ENABLE_DEBUG` isn't defined.

`MRB_DEBUG`
* When defined `mrb_assert*` macro will be defined with macros from `<assert.h>`.
* Could be enabled via `enable_debug` method of `MRuby::Build`.

## Stack configuration

`MRB_STACK_EXTEND_DOUBLING`
* If defined doubles the stack size when extending it.
* Else extends stack with `MRB_STACK_GROWTH`.

`MRB_STACK_GROWTH`
* Default value is `128`.
* Used in stack extending.
* Ignored when `MRB_STACK_EXTEND_DOUBLING` is defined.

`MRB_STACK_MAX`
* Default value is `0x40000 - MRB_STACK_GROWTH`.
* Raises `RuntimeError` when stack size exceeds this value.

## Primitive type configuration.

`MRB_USE_FLOAT`
* When defined single precision floating point type(C type `float`) is used as `mrb_float`.
* Else double precision floating point type(C type `double`) is used as `mrb_float`.

`MRB_INT16`
* When defined `int16_t` will be defined as `mrb_int`.
* Conflicts with `MRB_INT64`.

`MRB_INT64`
* When defined `int64_t` will be defined as `mrb_int`.
* Conflicts with `MRB_INT16`.
* When `MRB_INT16` or `MRB_INT64` isn't defined `int`(most of the times 32-bit integer)
will be defined as `mrb_int`.

## Garbage collector configuration.

`MRB_GC_STRESS`
* When defined full GC is emitted per each `RBasic` allocation.
* Mainly used in memory manager debugging.

`MRB_GC_TURN_OFF_GENERATIONAL`
* When defined turns generational GC by default.

`MRB_GC_FIXED_ARENA`
* When defined used fixed size GC arena.
* Raises `RuntimeError` when this is defined and GC arena size exceeds `MRB_GC_ARENA_SIZE`.
* Useful tracking unnecessary mruby object allocation.

`MRB_GC_ARENA_SIZE`
* Default value is 100.
* Ignored when `MRB_GC_FIXED_ARENA` isn't defined.
* Defines fixed GC arena size.
