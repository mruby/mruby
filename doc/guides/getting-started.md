<!-- summary: Getting Started with mruby -->

# Getting Started with mruby

This guide walks you through building mruby, running your first Ruby program,
and embedding mruby in a C application.

## Prerequisites

You need:

- C compiler (`gcc` or `clang`)
- Ruby 2.5 or later (for the build system)
- `rake` (bundled with Ruby)
- `git` (optional, for cloning the source)

## Building mruby

Clone the repository and build:

```console
$ git clone https://github.com/mruby/mruby.git
$ cd mruby
$ rake
```

This compiles the default configuration and produces:

- `bin/mruby` — Ruby script interpreter
- `bin/mirb` — interactive Ruby shell
- `bin/mrbc` — bytecode compiler
- `build/host/lib/libmruby.a` — library for embedding

## Running Ruby Code

### Interactive shell

```console
$ bin/mirb
mirb - Pair interactive mruby
> puts "Hello, mruby!"
Hello, mruby!
 => nil
> 1 + 2
 => 3
```

### Running a script file

Create `hello.rb`:

```ruby
puts "Hello from mruby!"
```

Run it:

```console
$ bin/mruby hello.rb
Hello from mruby!
```

### One-liner

```console
$ bin/mruby -e 'puts "Hello!"'
Hello!
```

## Compiling to Bytecode

mruby can compile Ruby scripts to bytecode (`.mrb` files) for faster
loading and deployment without source code:

```console
$ bin/mrbc hello.rb        # produces hello.mrb
$ bin/mruby -b hello.mrb   # run bytecode
Hello from mruby!
```

You can also generate C source from Ruby scripts:

```console
$ bin/mrbc -Bhello_code hello.rb   # produces hello.c with byte array
```

This generates a C file with a `const uint8_t hello_code[]` array that
can be loaded with `mrb_load_irep()` in your C application.

## Embedding mruby in C

The primary use case of mruby is embedding in C/C++ applications.

### Minimal example

Create `embed.c`:

```c
#include <mruby.h>
#include <mruby/compile.h>

int main(void)
{
  mrb_state *mrb = mrb_open();
  if (!mrb) return 1;

  mrb_load_string(mrb, "puts 'Hello from embedded mruby!'");
  if (mrb->exc) {
    mrb_print_error(mrb);
  }

  mrb_close(mrb);
  return 0;
}
```

### Compile and link

Use `mruby-config` to get the correct compiler and linker flags:

```console
$ gcc -I include `build/host/bin/mruby-config --cflags` embed.c \
    `build/host/bin/mruby-config --ldflags --libs` -o embed
$ ./embed
Hello from embedded mruby!
```

**Important**: Always use `mruby-config --cflags` when compiling code
that uses mruby. The build configuration may define macros (such as
`MRB_NO_BOXING` or `MRB_USE_BIGINT`) that change the internal data
layout. Compiling without these flags causes silent data corruption.

### Calling Ruby from C

```c
#include <stdio.h>
#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/string.h>

int main(void)
{
  mrb_state *mrb = mrb_open();

  /* Define a Ruby method */
  mrb_load_string(mrb, "def greet(name) \"Hello, #{name}!\" end");

  /* Call it from C */
  mrb_value result = mrb_funcall(mrb, mrb_top_self(mrb),
                                 "greet", 1, mrb_str_new_lit(mrb, "World"));
  printf("%s\n", mrb_str_to_cstr(mrb, result));

  mrb_close(mrb);
  return 0;
}
```

### Defining C functions callable from Ruby

```c
#include <mruby.h>
#include <mruby/compile.h>

static mrb_value
my_add(mrb_state *mrb, mrb_value self)
{
  mrb_int a, b;
  mrb_get_args(mrb, "ii", &a, &b);
  return mrb_fixnum_value(a + b);
}

int main(void)
{
  mrb_state *mrb = mrb_open();

  /* Define method on Kernel (available everywhere) */
  mrb_define_method(mrb, mrb->kernel_module, "my_add",
                    my_add, MRB_ARGS_REQ(2));

  mrb_load_string(mrb, "puts my_add(3, 4)");  /* prints 7 */

  mrb_close(mrb);
  return 0;
}
```

## Loading Precompiled Bytecode

For deployment without the compiler gem, precompile your Ruby code:

```console
$ bin/mrbc -Bruby_code app.rb
```

Then load in C:

```c
#include <mruby.h>
#include <mruby/irep.h>
#include "app.c"  /* contains ruby_code[] */

int main(void)
{
  mrb_state *mrb = mrb_open();
  mrb_load_irep(mrb, ruby_code);
  if (mrb->exc) {
    mrb_print_error(mrb);
  }
  mrb_close(mrb);
  return 0;
}
```

This approach does not require the `mruby-compiler` gem, resulting in
a smaller binary.

## Customizing the Build

mruby's functionality is controlled by the build configuration file.
The default is `build_config/default.rb`.

### Using a custom configuration

```console
$ MRUBY_CONFIG=build_config/minimal.rb rake
```

### Selecting gems

Gems add features to mruby. A minimal configuration:

```ruby
MRuby::Build.new do |conf|
  conf.toolchain :gcc

  # Core language extensions
  conf.gem core: 'mruby-array-ext'
  conf.gem core: 'mruby-string-ext'
  conf.gem core: 'mruby-hash-ext'

  # Tools
  conf.gem core: 'mruby-bin-mruby'   # mruby command
  conf.gem core: 'mruby-bin-mirb'    # interactive shell
  conf.gem core: 'mruby-bin-mrbc'    # bytecode compiler

  # Compiler (needed for mrb_load_string)
  conf.gem core: 'mruby-compiler'
end
```

### Using a gembox

Gemboxes are predefined collections of gems:

```ruby
MRuby::Build.new do |conf|
  conf.toolchain :gcc
  conf.gembox 'default'    # standard set of gems
end
```

## Amalgamation (Single-File Build)

For the simplest integration, use amalgamation to combine all mruby
source into a single `mruby.c` and `mruby.h`:

```console
$ rake amalgam
$ gcc -I build/host/amalgam your_app.c build/host/amalgam/mruby.c -o your_app -lm
```

See [amalgamation.md](amalgamation.md) for details.

## What's Next

- [Language Features](language.md) — Ruby subset supported by mruby
- [C API Reference](capi.md) — values, classes, methods, error handling
- [Compile](compile.md) — full build system reference
- [mrbgems](mrbgems.md) — creating and using gems
- [Linking](link.md) — linking `libmruby` to applications
- [Build-time Configurations](mrbconf.md) — compile-time options
- [GC Arena](gc-arena-howto.md) — managing GC arena in C extensions
- [Limitations](../limitations.md) — differences from CRuby
