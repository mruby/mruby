# mruby/error.h

## mrb_sys_fail
```C
void mrb_sys_fail(mrb_state *mrb, const char *mesg);
```
If `SystemCallError` is defined raises `SystemCallError.new(errno[, mesg])`
(`mesg` isn't passed if its value is `NULL`, `errno` is the current system error number value).
Otherwise raises `RuntimeError` of `mesg`.

## mrb_exc_new_str
```C
mrb_value mrb_exc_new_str(mrb_state *mrb, struct RClass* c, mrb_value str);
```
Creates and returns exception from class `c` of message `str`.

## mrb_exc_new_str_lit
```C
#define mrb_exc_new_str_lit(mrb, c, lit) mrb_exc_new_str(mrb, c, mrb_str_new_lit(mrb, lit))
```
Creates and returns exception from class `c` of message created from string literal `lit`.

## mrb_make_exception
```C
mrb_value mrb_make_exception(mrb_state *mrb, int argc, const mrb_value *argv);
```
Creates and returns exception with arguments.
* If `argc` is `0` returns `nil` object.
* If `argc` is `1` and `argv[0]` is string, returns `RuntimeError` with message `argv[0]`.
* If `argc` is greater than `0` and `argv[0]` is subclass of `Exception`,
returns exception of `argv[0]` that is created from `new` method with the rest of `argv`.
* Otherwise raises `TypeError` or `ArgumentError`.

## mrb_print_backtrace
```C
void mrb_print_backtrace(mrb_state *mrb);
```
Print backtrace of current raised exception.
`mrb->exc` must not be `NULL` when calling this API.

## mrb_exc_backtrace
```C
mrb_value mrb_exc_backtrace(mrb_state *mrb, mrb_value exc);
```
Returns array of backtrace created from exception object `exc`.

## mrb_get_backtrace
```C
mrb_value mrb_get_backtrace(mrb_state *mrb);
```
Returns array of backtrace in current execution point.

## mrb_no_method_error
```C
mrb_noreturn void mrb_no_method_error(mrb_state *mrb, mrb_sym id, mrb_int argc, const mrb_value *argv, const char *fmt, ...);
```
Creates `NoMethodError` from formatted message, method name `id`,
and arguments array created from `argc` and `argv`.
