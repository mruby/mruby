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

## Method

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
