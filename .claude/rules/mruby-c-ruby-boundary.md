---
paths:
  - "src/**/*.{c,h}"
  - "include/**/*.h"
  - "mrblib/**/*.rb"
  - "mrbgems/*/src/*.{c,h}"
  - "mrbgems/*/mrblib/*.rb"
---

# The C/Ruby boundary in mruby

This rule states a convention that `CONTRIBUTING.md` leaves implicit. The Coding
conventions there cover C99 conformance, keeping library dependencies minimal,
the newline after a return type, and ISO/IEC 30170:2012 conformance for Ruby
code. They say nothing about re-entering the VM.

## Basic principle

When a C function is designed as a type check, an argument check, a state
check, a manipulation of an internal representation, or a low-level primitive,
keep that work inside C as far as possible.

Do not move object construction, method dispatch, policy decisions, or
high-level control flow into C for convenience alone when Ruby can express them
naturally.

## Avoid re-entering the VM

Do not re-enter the Ruby VM from a C helper whose purpose is argument
validation, type determination, normalization, or inspection of internal state.

In particular, do not perform any of the following inside a validation or
normalization helper.

- Ruby method dispatch through `mrb_funcall*`
- Block invocation through `mrb_yield*`
- Running user-definable initialization through `mrb_obj_new`,
  `mrb_class_new_instance`, or an equivalent
- Running Ruby code, bytecode, a `Proc`, or a `Fiber`
- Conversion or object construction that calls a user-definable method

For example, verify in C that an argument is a `String` or a `Regexp`, and
perform the `String` to `Regexp` conversion in mrblib.

The test is not whether the operation is a conversion, but whether it involves
method dispatch.

## Division of responsibility

Take the following as the default split.

### C side

- Determining the actual type of an object
- Access to internal representations
- Low-level data manipulation
- Conversions closed over internal representations, such as encoding
  conversion, numeric representation conversion, or reading and writing struct
  fields, that involve no method dispatch
- Validation of arguments and state
- Primitive operations that need no VM dispatch
- Raising consistent exceptions

### Ruby side

- Calling Ruby methods
- High-level construction of Ruby objects
- Overridable behavior
- Control flow that composes several primitives
- Conversion or delegation that calls a user-definable method, such as
  `Regexp.new`, `to_s`, or `to_a`

## Exemptions

This rule does not forbid calling into the Ruby VM from C outright.

Entering the VM from C is acceptable when any of the following holds.

- The very meaning of the API is Ruby method dispatch or block invocation
- The same division of responsibility cannot be achieved on the Ruby side
- The callback is intended as a public API or as an established design contract
- Exception propagation, non-local control flow, re-entrancy, GC roots, and the
  consistency of intermediate state have all been considered

### Existing exempt cases

None of the following violates this rule. Do not report them as violations.

- `convert_type` in `src/object.c`, along with `mrb_convert_type`,
  `mrb_check_convert_type`, and `mrb_check_string_type`. For implicit
  conversion protocols such as `to_str`, the dispatch itself is the
  specification.
- `mrb_obj_new` in `src/class.c`, which calls `initialize`, and the hook
  notifications `inherited`, `included`, `prepended`, `extended`,
  `method_added`, and `method_missing`
- The default proc invocation and the `Hash#default` delegation in `src/hash.c`
- The `to_a` and `to_enum` delegations and the element comparisons in `==` and
  `eql?` in `src/array.c`

The earlier item about running initialization through `mrb_obj_new` forbids
such calls from validation and normalization helpers. It does not cover the
implementation of `mrb_obj_new` itself, nor APIs whose purpose is construction.

When relying on an exemption, record why the call into the VM is necessary in a
code comment or in the pull request description.

## Review requirements

When adding or changing code that enters the Ruby VM from C, check the
following first.

1. Is the call required by the meaning of the API, or is it merely an
   implementation convenience?
2. Can the call be moved to the mrblib side?
3. Is the method being called immune to redefinition, inheritance, `prepend`,
   and monkey patching?

If the VM call is kept, continue with the following.

4. Does intermediate state survive if Ruby code re-enters the same C API?
5. Do Ruby exceptions and non-local control flow such as `break` propagate
   correctly?
6. Are the values that matter rooted against allocation during VM execution
   (`mrb_gc_arena_save`, `mrb_gc_arena_restore`, `mrb_gc_protect`)? See
   `mrb_gc_arena_restore(mrb, ai); /* for mrb_funcall */` in `src/array.c` for
   a live example.
7. Do argument validation, conversion, side effects, and the order in which
   exceptions are raised preserve Ruby compatibility?
8. Are there tests covering subclasses, method redefinition, exceptions,
   re-entrancy, and block control flow?

The mere presence of `mrb_funcall*` is not a problem in itself. Judge by the
responsibility of the function in question and by whether the VM call is
semantically necessary.
