<!-- summary: About Amalgamation (Single-File Build) -->

# Amalgamation

Amalgamation combines all mruby source files into a single `mruby.c` and
`mruby.h` for easy embedding, similar to SQLite's distribution model.

## Benefits

- **Simple integration**: Just two files to add to your project
- **Single compilation unit**: Enables better compiler optimization
- **No build system required**: Compile directly with any C compiler
- **Portable**: No external dependencies beyond standard C library
  (but see [Platform-Dependent Gems](#platform-dependent-gems) below)

## Generating Amalgamation

```console
rake amalgam
```

Output files are generated in `build/<target>/amalgam/`:

- `mruby.h` - All headers concatenated in dependency order
- `mruby.c` - All sources concatenated (core + gems + mrblib)

### With Custom Configuration

The amalgamation includes gems specified in your build configuration:

```console
MRUBY_CONFIG=build_config/minimal.rb rake amalgam
```

## Using the Amalgamation

### Basic Usage

```c
#include "mruby.h"

int main(void) {
  mrb_state *mrb = mrb_open();
  mrb_load_string(mrb, "puts 'Hello from mruby!'");
  mrb_close(mrb);
  return 0;
}
```

### Compiling

```console
gcc -I./build/host/amalgam your_app.c ./build/host/amalgam/mruby.c -o your_app -lm
```

For optimized builds:

```console
gcc -O2 -DNDEBUG -I./build/host/amalgam your_app.c ./build/host/amalgam/mruby.c -o your_app -lm
```

## Gem Compatibility

### Known Working Gems

The following gems work with amalgamation:

- `mruby-compiler` - Required for `mrb_load_string`
- `mruby-eval` - `eval`, `Binding`
- `mruby-array-ext`, `mruby-string-ext`, `mruby-hash-ext`
- `mruby-numeric-ext`, `mruby-range-ext`, `mruby-symbol-ext`
- `mruby-proc-ext`, `mruby-kernel-ext`, `mruby-object-ext`, `mruby-class-ext`
- `mruby-enum-ext`, `mruby-compar-ext`
- `mruby-error`, `mruby-math`, `mruby-struct`
- `mruby-bigint`, `mruby-rational`, `mruby-complex`
- `mruby-io` (with the active `ports/<name>/` HAL)
- `mruby-task` (with the active `ports/<name>/` HAL)

### Platform-Dependent Gems

Gems that use a HAL (Hardware Abstraction Layer) include
platform-specific code in the amalgamation. For example, if
`mruby-io` selects its POSIX port on Linux, the generated `mruby.c`
contains POSIX-specific code and cannot be compiled on Windows.

If you need amalgamated files for multiple platforms, generate them
separately for each target platform (or cross-build configuration).

### Excluded Gems

Binary gems (`mruby-bin-*`) are automatically excluded as they contain
their own `main()` function. The amalgamation produces a library, not
an executable.

## Example Configuration

A minimal configuration for amalgamation:

```ruby
# build_config/amalgam.rb
MRuby::Build.new do |conf|
  conf.toolchain :gcc

  conf.gem core: 'mruby-compiler'
  conf.gem core: 'mruby-error'
  conf.gem core: 'mruby-eval'
  conf.gem core: 'mruby-array-ext'
  conf.gem core: 'mruby-string-ext'
  conf.gem core: 'mruby-hash-ext'
  conf.gem core: 'mruby-io'
end
```

Generate with:

```console
MRUBY_CONFIG=build_config/amalgam.rb rake amalgam
```

## Output Sizes

Typical sizes depend on included gems:

- `mruby.h`: 200-500 KB
- `mruby.c`: 2-4 MB

## Technical Details

### Header Processing

- Include guards are stripped to allow concatenation
- Headers are ordered by dependency (foundation types first)
- Internal includes are commented out (already in `mruby.h`)

### Source Processing

- Sources are concatenated in proper initialization order
- X-macro headers (like `mruby/ops.h`) are inlined at each use
- Local includes (`.cstub` files) are automatically inlined
- Generated files (`mrblib.c`, `gem_init.c`) are included

### The `MRB_AMALGAMATION` Define

Both generated `.c` files define `MRB_AMALGAMATION` ahead of everything else.
It marks a translation unit that holds the whole runtime rather than one
source file, so that a source asking the compiler for work proportional to
the translation unit can keep the request scoped the way a per-file build
scopes it.

The VM uses it for exactly that. `mrb_vm_exec()` carries
`__attribute__((flatten))`, which inlines every call it makes, recursively,
as deep as the translation unit allows. A per-file build stops it at the VM's
own static opcode handlers, which is what the attribute is for. With the whole
runtime in one translation unit it instead reaches the cycle that allocation,
collection and exception raising form with each other, and the descent never
converges. GCC exhausts memory in its inliner before emitting a single
function, so under GCC the amalgamation drops the attribute and
`mrb_vm_exec()` is optimized like any other function there.

Clang keeps the descent bounded, compiles the same file in seconds, and runs
faster with the attribute than without it, so it keeps it.

### Build Configuration Defines

The defines the build compiles with are written at the top of `mruby.h`,
ahead of its first `#include`, so that including it is enough to get the same
`mrb_value` layout, integer width and feature set as the build the
amalgamation was generated from. Both the defines the build configuration
names (`conf.cc.defines`) and the ones gems contribute (`spec.build.defines`,
`spec.cc.defines`) are emitted. Each is wrapped in `#ifndef`, so passing the
same define on the command line is not a redefinition.

Not every name a gem lists in `spec.cc.defines` reaches the header. `MRB_*`
and `MRBGEM_*` are dropped: a define that changes how `mruby.h` itself is
read belongs to the whole build rather than to one gem, and the `MRBGEM_*`
version strings are not C constants. `__STDC_*` is dropped as well, the
preamble writing the ones the amalgamation needs itself. A gem whose `MRB_*`
define belongs in the header declares it in `spec.build.defines`.

Their position ahead of every `#include` mirrors a normal build, where each
`-D` precedes every header. That is what makes libc feature test macros work:
a gem that uses a GNU extension declares `spec.cc.defines << '_GNU_SOURCE'`
in its `mrbgem.rake` and the declaration reaches the amalgamation consumer's
plain compiler invocation. Writing `#define _GNU_SOURCE` at the top of a gem
source file does not survive amalgamation: the file lands in the middle of
the combined translation unit, after the first libc header has been read.

Note that in the combined translation unit a gem's defines apply to every
source file, not only to the gem's own objects as in a normal build. Macros
that add declarations (`_GNU_SOURCE`, `_DEFAULT_SOURCE`) are safe there;
macros that restrict the libc feature set (`_POSIX_C_SOURCE`,
`_XOPEN_SOURCE`) can hide declarations the other sources rely on.

Two kinds are not written, and are left to whoever compiles the amalgamation:

- `MRB_DEBUG`, which only decides whether `mrb_assert` checks. `-DNDEBUG`
  above is the same kind of choice.
- `MRB_USE_CXX_EXCEPTION` and `MRB_USE_CXX_ABI`, which require a C++
  compiler. `mruby.c` is C, so a header that demanded them could not be
  compiled as generated.

### Build Order

1. Core sources (`src/*.c`)
2. Gem sources (`mrbgems/*/src/*.c` or `core/*.c`)
3. Generated mrblib (`build/*/mrblib/mrblib.c`)
4. Gem initialization (`build/*/mrbgems/gem_init.c`)
