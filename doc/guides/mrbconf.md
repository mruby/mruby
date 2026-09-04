<!-- summary: About Build-time Configurations -->

# mruby configuration macros

## The configuration file

You can do the build configuration in the build configuration file. The default
configuration file is `build_config/default.rb`.

You can specify your own configuration file by the `MRUBY_CONFIG` environment
variable (you can use `CONFIG` for shorthand for `MRUBY_CONFIG`). If the path
doesn't exist, `build_config/${MRUBY_CONFIG}.rb` is used.

## How to use these macros

Just add the configuration value to the `MRuby::Build#defines` attribute.
This is the same for `MRuby::CrossBuild`.

```ruby
# build_config.rb

MRuby::Build.new do |conf|
  ...
  conf.defines << 'MRB_GC_FIXED_ARENA'
  conf.defines << 'MRB_NO_METHOD_CACHE'
  ...
end
```

**_NOTE_**

- Use common definitions (`conf.defines`) instead of per-compiler definitions (e.g., `conf.cc.defines`) unless there is a special reason not to.
- It is now deprecated to edit the `include/mruby/mrbconf.h` file or give it directly as a compiler flag, as was the case before.

## stdio setting

`MRB_NO_STDIO`

- When defined `<stdio.h>` functions won't be used.
- Some features will be disabled when this is enabled:
  - `mrb_irep` load/dump from/to file.
  - Compiling mruby script from a file.
  - Printing features in **src/print.c**.

## Debug macros

`MRB_USE_DEBUG_HOOK`

- When defined code fetch hook and debug OP hook will be enabled.
- When using any of the hook set function pointer `code_fetch_hook` and/or `debug_op_hook` of `mrb_state`.
- Fetch hook will be called before any OP.
- Debug OP hook will be called when dispatching `OP_DEBUG`.

`MRB_DEBUG`

- When defined `mrb_assert*` macro will be defined with macros from `<assert.h>`.
- Could be enabled via `enable_debug` method of `MRuby::Build`.

## Stack configuration

`MRB_STACK_EXTEND_DOUBLING`

- If defined doubles the stack size when extending it.
- Otherwise extends stack with 1.5x growth (minimum `MRB_STACK_GROWTH`).

`MRB_STACK_GROWTH`

- Default value is `128`.
- Minimum stack growth size when extending.
- Ignored when `MRB_STACK_EXTEND_DOUBLING` is defined.

`MRB_STACK_MAX`

- Default value is `0x40000 - MRB_STACK_GROWTH`.
- Raises `RuntimeError` when stack size exceeds this value.

## Primitive type configuration

`MRB_USE_FLOAT32`

- When defined single-precision floating-point type(C type `float`) is used as `mrb_float`.
- Otherwise, double-precision floating-point type(C type `double`) is used as `mrb_float`.

`MRB_NO_FLOAT`

- When defined removes floating-point numbers from mruby.
- It makes mruby easier to handle in "Micro-controller without FPU" and "Kernel Space".
- A floating-point literal in Ruby source is read as the Integer `0`, with a compiler warning.

`MRB_INT32`

- When defined, or `MRB_INT64` are not defined on 32-bit CPU mode, `mrb_int` will be defined as `int32_t`.
- Conflicts with `MRB_INT64`.

`MRB_INT64`

- When defined, or `MRB_INT32` are not defined on 64-bit CPU mode, `mrb_int` will be defined as `int64_t`.
- Conflicts with `MRB_INT32`.
- On 32-bit platforms, `MRB_INT64` requires `MRB_NO_BOXING` because heap-allocated `RInteger` needs 8-byte alignment that the GC heap may not guarantee with word or NaN boxing.

## Garbage collector configuration

`MRB_GC_STRESS`

- When defined full GC is emitted per each `RBasic` allocation.
- Mainly used in memory manager debugging.
- If defined at the same time as `MRB_DEBUG`, full GC is emitted also per each heap allocation (`mrb_malloc()` or etc.).
  This configuration slows down mruby execution by a factor of 2 to 3 or even more.

`MRB_GC_TURN_OFF_GENERATIONAL`

- When defined turns generational GC off by default.

`MRB_GC_FIXED_ARENA`

- When defined used fixed size GC arena.
- Raises `RuntimeError` when this is defined and GC arena size exceeds `MRB_GC_ARENA_SIZE`.
- Useful tracking unnecessary mruby object allocation.

`MRB_GC_ARENA_SIZE`

- Default value is `100`.
- Ignored when `MRB_GC_FIXED_ARENA` isn't defined.
- Defines fixed GC arena size.

`MRB_HEAP_PAGE_SIZE`

- Default value is `1024`.
- Specifies number of `RBasic` per each heap page.

`MRB_GC_MALLOC_THRESHOLD`

- Default value is `16777216` (16MiB), the figure CRuby gives `malloc_limit`,
  or `SIZE_MAX/4` on a target whose `size_t` cannot represent that.
- Bytes of `mrb_realloc()` growth that schedule a collection, independent of
  how many objects those bytes belong to.
- Sets the initial value of `GC.malloc_threshold`, which can be changed at
  run time; `0` disables byte-driven collection.
- The counter behind it is cleared at the end of every collection cycle, so it
  only accumulates while object-count scheduling is idle. A workload made of
  ordinary objects never gets near the default: two million small allocations
  peak at 188KB of accumulated growth, `benchmark/bm_ao_render.rb` at 710KB.
  What reaches it is large buffers, which is what it exists to catch.
- Lower it on a target whose memory budget is smaller than the default, or the
  byte axis will never fire there. A figure on the order of the budget is the
  right scale: on the two million allocation workload above, `65536` leaves
  the collection count unchanged and `32768` adds 3% more minor collections.

## Memory pool configuration

`POOL_ALIGNMENT`

- Default value is `4`.
- If you're allocating data types that requires alignment more than default value define the
  largest value of required alignment.

`POOL_PAGE_SIZE`

- Default value is `16000`.
- Specifies page size of pool page.
- Smaller the value is increases memory overhead.

## State atexit configuration

`MRB_FIXED_STATE_ATEXIT_STACK`

- If defined enables fixed size `mrb_state` atexit stack.
- Raises `RuntimeError` when `mrb_state_atexit` call count to same `mrb_state` exceeds
  `MRB_FIXED_STATE_ATEXIT_STACK_SIZE`'s value.

`MRB_FIXED_STATE_ATEXIT_STACK_SIZE`

- Default value is `5`.
- If `MRB_FIXED_STATE_ATEXIT_STACK` isn't defined this macro is ignored.

## `mrb_value` configuration

`MRB_ENDIAN_BIG`

- If defined compiles mruby for big endian machines.
- Used in `MRB_NAN_BOXING`.
- Some mrbgem use this mrbconf.

`MRB_NAN_BOXING`

- If defined represent `mrb_value` in boxed `double`.
- Conflicts with `MRB_USE_FLOAT32` and `MRB_NO_FLOAT`.

`MRB_WORD_BOXING`

- If defined represent `mrb_value` as a word (natural unit of data for the processor).
- Default boxing mode when none is specified.
- On 64-bit platforms, floats are inlined using rotation encoding.
- On 32-bit platforms, floats are heap-allocated as `RFloat` objects.

`MRB_NO_BOXING`

- If defined represent `mrb_value` as a C struct (occupies 2 words).
- Most portable but least memory-efficient representation.
- Required for `MRB_INT64` on 32-bit platforms.
- Default for `host-debug` configuration.

`MRB_WORDBOX_NO_INLINE_FLOAT`

- If defined disables inline float values in word boxing.
- All floats are heap-allocated as `RFloat` objects.
- Automatically defined on 32-bit platforms (64-bit `double` cannot fit in a 32-bit word).
- Only meaningful with `MRB_WORD_BOXING`.

## Reduce heap memory configuration

`MRB_USE_ETEXT_RO_DATA_P`

- Use `etext` and `edata` section addresses defined by the linker to detect read-only data.
- Those addresses are widely available, but not portable, nor standardized.
- Defined by default on User-mode Linux.

`MRB_NO_DEFAULT_RO_DATA_P`

- Define this macro when the default `mrb_ro_data_p()` does not work for any reason.

`MRB_USE_CUSTOM_RO_DATA_P`

- Define to provide your own `mrb_ro_data_p()` implementation.
- The prototype declaration is `mrb_bool mrb_ro_data_p(const char *ptr)`.
- Return `TRUE` if `ptr` is in the read-only section, otherwise return `FALSE`.

## Other configuration

`MRB_USE_MALLOC_TRIM`

- Call `malloc_trim(0)` for each `mrb_full_gc()` call.

`MRB_UTF8_STRING`

- Adds UTF-8 encoding support to character-oriented String instance methods.
- Case conversion follows Unicode: `String#downcase`, `#upcase`, `#capitalize`
  and `#swapcase` map every character Unicode gives a case, and a mapping may
  spell several characters (`"ß".upcase` is `"SS"`). `String#casecmp?` folds
  by the same data rather than converting.
- A string read as bytes (`String#b`) converts and folds ASCII alone, and one
  holding bytes that spell no character is refused with `ArgumentError`.
- The regexp `i` flag reads the same data, folding every character Unicode
  pairs with one other. Without this macro it folds ASCII letters, and a
  pattern holding a character that needs one of the Unicode foldings raises
  `RegexpError` rather than answering as if the character had no case.
- The regexp POSIX brackets classify by Unicode above ASCII: `[[:alpha:]]`
  holds a letter of any script and `[[:^alpha:]]` rejects it, as in CRuby.
  Without this macro a bracket holds its ASCII and no character above it.
- `String#succ` steps a letter or a digit above ASCII within its own run of
  them and wraps at the end of it, as in CRuby (`"ת".succ` is `"אא"`).
  Without this macro nothing above ASCII is a letter or a digit, and the last
  character steps as a character.
- `MRB_USE_ASCII_CTYPE` narrows the case, the brackets and `String#succ` back
  to ASCII, taking the refusal with them and leaving the indexing.
- If it isn't defined, they only support the US-ASCII encoding.

`MRB_USE_ASCII_CTYPE`

- Narrows the character classification of `MRB_UTF8_STRING` back to ASCII
  while keeping its indexing: `String#downcase`, `#upcase`, `#capitalize`,
  `#swapcase` and `#casecmp?` answer for `'A'` to `'Z'` and hand every other
  character back as it stands, a regexp POSIX bracket holds its ASCII and no
  character above it, and `String#succ` finds no letter and no digit above
  ASCII to step.
- Drops the Unicode tables the build would otherwise carry, core's case table,
  mruby-regexp's type table and mruby-string-ext's table of the letters and
  the digits. That is what the option is for: a target counting its bytes buys
  the UTF-8 indexing of `MRB_UTF8_STRING` without the tables beside it.
- Bytes that spell no character are handed back as they stand rather than
  refused with `ArgumentError`. That refusal belongs to the walk over
  characters, which is the walk this narrows away: what converts instead reads
  bytes, and reading bytes asks nothing about what they spell.
- The regexp `i` flag reads that table too, so it narrows with the rest: it
  folds ASCII letters, and a pattern holding a character that needs one of the
  Unicode foldings raises `RegexpError` rather than answering as if the
  character had no case.
- Nothing to narrow without `MRB_UTF8_STRING`: a build reading its strings as
  bytes converts ASCII alone whatever this says.

`MRB_STR_LENGTH_MAX`

- The maximum length of strings (default 1048576).
- Set this value to zero to skip the check.

`MRB_ARY_LENGTH_MAX`

- The maximum length of arrays (default 131072).
- Set this value to zero to skip the check.

`MRB_FUNCALL_ARGC_MAX`

- Default value is `16`.
- Specifies 4th argument(`argc`) max value of `mrb_funcall`.
- Raises `ArgumentError` when the `argc` argument is bigger then this value `mrb_funcall`.

`KHASH_INITIAL_SIZE`

- Default value is `32`.
- Specifies initial size of khash table bucket.
- Used in `kh_init_ ## name` function.

`MRB_NO_METHOD_CACHE`

- Disable method cache to save memory.

`MRB_METHOD_CACHE_SIZE`

- Default value is `256`.
- Ignored if `MRB_NO_METHOD_CACHE` is defined.
- Need to be the power of 2.

`MRB_USE_ALL_SYMBOLS`

- Make it available `Symbol.all_symbols` in `mrbgems/mruby-symbol-ext`
- Increase heap memory usage.

`MRB_USE_VM_SWITCH_DISPATCH`

- Turn on switch dispatch in VM loop.
- Otherwise, computed goto (direct threading) is used when supported by the compiler.

`MRB_SYMBOL_LINEAR_THRESHOLD`

- Default value is `256`.
- Threshold for switching symbol table from linear search to hash table.

`MRUBY_REVISION`, `MRUBY_FULL_REVISION`

- The revision the build was made from: `MRUBY_FULL_REVISION` the whole commit
  hash, which is what the `MRUBY_REVISION` constant holds, and `MRUBY_REVISION`
  the ten characters a version string can name it with.
- The build reads them for itself, out of the repository the source sits in or
  out of the `.revision` a source release carries, and writes what it read to a
  generated `mruby/revision.h`. Define them here where it can read neither and
  the revision is known anyway, as a package built from a source drop is.
- What a build config says wins over what the build read.
- A build with nothing to read and nothing defined here reports `"HEAD"`.
- Both hold a string, so the value carries quotes of its own:

```ruby
conf.defines << 'MRUBY_REVISION=\"0123456789\"'
conf.defines << 'MRUBY_FULL_REVISION=\"0123456789abcdef0123456789abcdef01234567\"'
```

## Platform name

`MRUBY_PLATFORM`

- The name of the platform the built binary runs on, in the `cpu-os` form
  CRuby's `RUBY_PLATFORM` uses: `x86_64-linux`, `arm64-darwin`,
  `x64-mingw-ucrt`. It is what the `MRUBY_PLATFORM` global constant carries.
- The default is read from the compiler's own predefined macros, so a cross
  build names its target rather than the machine that ran the build. A target
  the detection does not know reports `unknown` for the CPU half; one with no
  operating system under it (bare metal, or an RTOS the compiler does not
  announce) reports `none`, as a triple would (`arm-none`, `xtensa-none`).
- Define this to name the platform outright. The value is a C string literal,
  so its quotes have to reach the compiler:

```ruby
conf.defines << 'MRUBY_PLATFORM=\"esp32-freertos\"'
```

`MRUBY_PLATFORM_CPU`, `MRUBY_PLATFORM_OS`

- The two halves `MRUBY_PLATFORM` is built from. Define either one, the same
  way, to correct one half and leave the other to the detection.

## Tuning profiles

Predefined profiles adjust several macros together for specific
deployment targets. Define one of the following:

`MRB_CONSTRAINED_BASELINE_PROFILE`

- For micro controllers.
- Enables `MRB_NO_METHOD_CACHE`, reduces `KHASH_INITIAL_SIZE` to `16`,
  and `MRB_HEAP_PAGE_SIZE` to `256`.

`MRB_BASELINE_PROFILE`

- Default mruby profile. No additional changes.

`MRB_MAIN_PROFILE`

- For desktop computers or workstations.
- Increases `MRB_METHOD_CACHE_SIZE` to `1024` and `MRB_HEAP_PAGE_SIZE`
  to `4096`.

`MRB_HIGH_PROFILE`

- For long-lived server processes.
- Increases `MRB_METHOD_CACHE_SIZE` to `4096` and `MRB_HEAP_PAGE_SIZE`
  to `4096`.
