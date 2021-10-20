# User visible changes in `mruby3` from `mruby2` (as of `mruby3.1`)

# Build System

## `build_config` directory

Typical build configuration files are located in `build_config`
directory. For examples:

* `default`: the default configuration
* `host-gprof`: compiles with `gprof` for performance tuning
* `host-m32`: compiles in gcc 32bit mode on 64bit platforms
* `boxing`: compiles all three boxing options
* `clang-asan`: compiles with `clang`'s Address Sanitizer

You can specify the build configuration file with the
`MRUBY_CONFIG` environment variable (or `CONFIG` in short).
If the value specified by `MRUBY_CONFIG` is not the path to
the configuration file, `build_config/${MRUBY_CONFIG}.rb` is
used.  So you can specify it as `rake MRUBY_CONFIG=boxing`,
for example.

# Build Configuration Contribution

When you write a new build configuration description, please
contribute. We welcome your contribution as a GitHub
pull-request.

# Language Changes

## New Syntax

We have ported some new syntax from CRuby.

* Single line pattern matching (`12 => x`);
  mruby matches only with local variables at the moment
* Numbered block parameter (`x.map{_1 * 2}`)
* End-less `def` (`def double(x) = x*2`)

# Configuration Options Changed

## Renamed for consistency

Some configuration macro names are changed for consistency (use `MRB_USE_XXX`
or `MRB_NO_XXX`).

|             mruby2             |          mruby3           |
|--------------------------------|---------------------------|
| `MRB_ENABLE_ALL_SYMBOLS`       | `MRB_USE_ALL_SYMBOLS`     |
| `MRB_ENABLE_CXX_ABI`           | `MRB_USE_CXX_ABI`         |
| `MRB_ENABLE_CXX_EXCEPTION`     | `MRB_USE_CXX_EXCEPTION`   |
| `MRB_ENABLE_DEBUG_HOOK`        | `MRB_USE_DEBUG_HOOK`      |
| `MRB_DISABLE_DIRECT_THREADING` | `MRB_NO_DIRECT_THREADING` |
| `MRB_DISABLE_STDIO`            | `MRB_NO_STDIO`            |
| `MRB_METHOD_T_STRUCT`          | `MRB_USE_METHOD_T_STRUCT` |
| `MRB_USE_FLOAT`                | `MRB_USE_FLOAT32`         |
| `MRB_WITHOUT_FLOAT`            | `MRB_NO_FLOAT`            |
| `ENABLE_LINENOISE`             | `MRB_USE_LINENOISE`       |
| `ENABLE_READLINE`              | `MRB_USE_READLINE`        |
| `DISABLE_MIRB_UNDERSCORE`      | `MRB_NO_MIRB_UNDERSCORE`  |

* `MRB_USE_FLOAT32` is changed from `MRB_USE_FLOAT` to make sure `float` here
  means using single precision float, and not the opposite of `MRB_NO_FLOAT`.
* `MRB_USE_METHOD_T_STRUCT` uses `struct` version of `mrb_method_t`. More
  portable but consumes more memory. Turned on by default on 32bit platforms.
* `MRB_` prefix is added to those without.

## `MRB_NO_BOXING`

Uses `struct` to represent `mrb_value`. Consumes more memory
but easier to investigate the internal and to debug. It used
to be default `mrb_value` representation. Now the default is
`MRB_WORD_BOXING`.

## `MRB_WORD_BOXING`

Pack `mrb_value` in an `intptr_t` integer. Consumes less
memory compared to `MRB_NO_BOXING` especially on 32-bit
platforms. Inlined integer size is 31 bits, so some `mrb_int`
values does not fit in `mrb_value`. Those integers are allocated
in the object heap as `struct RInteger`.

## `MRB_NAN_BOXING`

Pack `mrb_value` in a floating-point number. Nothing
changed from previous versions.

## `MRB_USE_MALLOC_TRIM`

Call `malloc_trim(0)` from mrb_full_gc() if this macro is defined.
If you are using glibc malloc, this macro could reduce memory consumption.

# Command Line Program

## `bin/mruby` (by mrbgems/mruby-bin-mruby)

The mruby3 now automatically detects `*.mrb` files without the `-b`
switch. Therefore, it can be mixed with the `*.rb` file in combination
with the `-r` switch and specified at the same time.
Here's an example that works fine:

```console
$ bin/mruby app.mrb
$ bin/mruby -r lib1.mrb -r lib2.rb app.rb
$ bin/mruby -r lib1.rb -r lib2.rb < app.mrb
```

# Internal Changes

## New Instructions

`mruby3` introduces a few new instructions.

### `OP_LOADI16` and `OP_LOADI32`

Load a 16/32-bit integer.

### `OP_JMPUW`

Unwinds jump table for rescue/ensure.

### `OP_RAISEIF`

Renamed from `OP_RAISE`

### `OP_SYMBOL`

Generates a symbol from the pool string. This is a combination of `OP_STRING` and `OP_INTERN`.

### `OP_GETIDX` and `OP_SETIDX`

Execute `obj[int]` and `obj[int] = value` respectively, where `obj` is `string|array|hash`.

### `OP_SSEND` and `OP_SSENDB`

They are similar to `OP_SEND` and `OP_SENDB` respectively. They initialize the `R[a]` by `self` so that we can skip one `OP_LOADSEND` instruction for each call.

## Changed Instructions

### Jump instructions

Jump addresses used to be specified by absolute offset from the start of `iseq`. Now they are relative offset from the address of the next instruction.

### `OP_SEND` and `OP_SENDB`

Method calling instructions are unified. Now `OP_SEND` and `OP_SENDB` (method call with a block) can support both splat arguments and keyword arguments as well.

The brief description of the instructions:

|`OP_SEND`   | BBB | `R(a) = call(R(a),Syms(b),R(a+1..n),R(a+n+1),R(a+n+2)..nk) c=n|nk<<4`                    |
|`OP_SENDB`  | BBB | `R(a) = call(R(a),Syms(b),R(a+1..n),R(a+n+1..nk),R(a+n+2..nk),&R(a+n+2*nk+2)) c=n|nk<<4` |

When `n == 15`, the method takes arguments packed in an array. When `nk == 15`, the method takes keyword arguments packed in a hash.

### `OP_ARYPUSH`

Now takes 2 operands and pushes multiple entries to an array.

## Removed Instructions

Instructions for old exception handling

* `OP_ONERR`
* `OP_POPERR`
* `OP_EPUSH`
* `OP_EPOP`

Instructions for method calls with variable number of arguments. They are covered by `OP_SEND` instruction with `n=15`.

* `OP_SENDV`
* `OP_SENDVB`

## `Random` now use `xoshiro128++`

For better and faster random number generation.

## Preallocated Symbol

Preallocated symbols are interned at compile-time. They can be accessed via symbols macros (e.g. `MRB_SYM()`).

See [Symbols](./guides/symbol.md).
