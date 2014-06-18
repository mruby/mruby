# mruby configuration macros.

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
