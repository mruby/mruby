# mruby-error

The `mruby-error` mrbgem provides a set of C-level APIs for structured exception handling within mruby. These functions allow C extensions or embedded mruby code to implement error handling patterns similar to Ruby's `begin`, `rescue`, and `ensure` keywords. This is particularly useful when writing C code that needs to interact with mruby's exception system in a robust way.

## `mrb_protect`

The `mrb_protect` function is used to execute a C function (`body`) and capture any exceptions that might be raised during its execution. This allows you to run potentially unsafe operations and handle errors gracefully.

**C Signature:**

```c
mrb_value mrb_protect(mrb_state *mrb, mrb_func_t body, mrb_value data, mrb_bool *state);
```

**Parameters:**

- `mrb_state *mrb`: The current mruby state.
- `mrb_func_t body`: A function pointer to the C function to be executed. This function should have the signature `mrb_value (*body)(mrb_state *mrb, mrb_value data)`.
- `mrb_value data`: A `mrb_value` that will be passed as an argument to the `body` function.
- `mrb_bool *state`: A pointer to a boolean. After `mrb_protect` returns, this boolean will be:
  - `FALSE` (0) if the `body` function executed without raising an exception.
  - `TRUE` (1) if an exception was raised within the `body` function.

**Return Value:**

- If no exception occurs (`*state` is `FALSE`), `mrb_protect` returns the value returned by the `body` function.
- If an exception occurs (`*state` is `TRUE`), `mrb_protect` returns the exception object.

## `mrb_ensure`

The `mrb_ensure` function ensures that a specific C function (`ensure`) is executed, regardless of whether another C function (`body`) completes normally or raises an exception. This is analogous to Ruby's `ensure` clause.

**C Signature:**

```c
mrb_value mrb_ensure(mrb_state *mrb, mrb_func_t body, mrb_value b_data, mrb_func_t ensure, mrb_value e_data);
```

**Parameters:**

- `mrb_state *mrb`: The current mruby state.
- `mrb_func_t body`: A function pointer to the main C function to be executed.
- `mrb_value b_data`: A `mrb_value` passed as data to the `body` function.
- `mrb_func_t ensure`: A function pointer to the C function that will always be executed after the `body` function.
- `mrb_value e_data`: A `mrb_value` passed as data to the `ensure` function.

**Behavior:**
The `body` function is executed first. After its completion (either normally or due to an exception), the `ensure` function is executed. If the `body` function raised an exception, that exception is re-thrown after the `ensure` function has finished. The return value of `mrb_ensure` is the result of the `body` function if no exception occurred.

## `mrb_rescue`

The `mrb_rescue` function executes a C function (`body`) and, if an exception that is a `StandardError` (or a subclass of `StandardError`) is raised, it executes a specified `rescue` C function. This is similar to a `rescue` clause in Ruby that doesn't specify a particular exception type (which defaults to `StandardError`).

**C Signature:**

```c
mrb_value mrb_rescue(mrb_state *mrb, mrb_func_t body, mrb_value b_data, mrb_func_t rescue, mrb_value r_data);
```

**Parameters:**

- `mrb_state *mrb`: The current mruby state.
- `mrb_func_t body`: A function pointer to the main C function to be executed.
- `mrb_value b_data`: A `mrb_value` passed as data to the `body` function.
- `mrb_func_t rescue`: A function pointer to the C function that will be executed if a `StandardError` (or its subclass) is caught.
- `mrb_value r_data`: A `mrb_value` passed as data to the `rescue` function.

**Behavior:**
If the `body` function executes without raising an exception, its result is returned. If a `StandardError` (or one of its descendants) is raised, the `rescue` function is executed, and its result becomes the return value of `mrb_rescue`. If an exception occurs that is not a `StandardError` (or its subclass), it is not caught by this function and will propagate up the call stack. Similarly, if the `rescue` block itself raises an exception, that exception will propagate.

## `mrb_rescue_exceptions`

The `mrb_rescue_exceptions` function provides more fine-grained exception handling than `mrb_rescue`. It executes a C function (`body`) and, if an exception matching one of the specified classes is raised, it executes a `rescue` C function. This is analogous to Ruby's `rescue SpecificError1, SpecificError2 => e` syntax.

**C Signature:**

```c
mrb_value mrb_rescue_exceptions(mrb_state *mrb, mrb_func_t body, mrb_value b_data, mrb_func_t rescue, mrb_value r_data, mrb_int len, struct RClass **classes);
```

**Parameters:**

- `mrb_state *mrb`: The current mruby state.
- `mrb_func_t body`: A function pointer to the main C function to be executed.
- `mrb_value b_data`: A `mrb_value` passed as data to the `body` function.
- `mrb_func_t rescue`: A function pointer to the C function that will be executed if a matching exception is caught.
- `mrb_value r_data`: A `mrb_value` passed as data to the `rescue` function.
- `mrb_int len`: The number of exception classes provided in the `classes` array.
- `struct RClass **classes`: An array of pointers to mruby `RClass` objects representing the exception classes to be rescued.

**Behavior:**
If the `body` function executes without raising an exception, its result is returned. If an exception is raised that is an instance of one of the classes specified in the `classes` array (or a subclass of one of them), the `rescue` function is executed, and its result becomes the return value of `mrb_rescue_exceptions`. If an exception occurs that does not match any of the specified classes, it is not caught and will propagate. If the `rescue` block itself raises an exception, that exception will propagate.

## Usage Examples

The C functions provided by this mrbgem are typically used when writing mruby C extensions that need to interact with Ruby code or manage resources carefully in the presence of potential exceptions.

For concrete examples of how these functions are used, please refer to the test files within this mrbgem:

- `test/exception.c`: Shows how these C functions are called directly.
- `test/exception.rb`: Demonstrates the behavior of these C functions from the Ruby side, through the `ExceptionTest` module (defined in `test/exception.c`).

These tests illustrate how to set up callback functions and how the error handling mechanisms behave in practice.
