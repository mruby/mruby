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

### Build Configuration Defines

The defines the build compiles with are written at the top of `mruby.h`, so
that including it is enough to get the same `mrb_value` layout, integer width
and feature set as the build the amalgamation was generated from. Both the
defines the build configuration names (`conf.cc.defines`) and the ones gems
contribute (`spec.build.defines`) are emitted. Each is wrapped in `#ifndef`,
so passing the same define on the command line is not a redefinition.

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
