# User visible changes in `mruby3.1` from `mruby3.0`

# Build System

## `build_config` directory

Several configurations for new platforms are added:

* `cross-mingw-winetest.rb`
* `cross-mingw.rb`
* `nintendo_switch.rb`
* `serenity.rb`

Some new configurations for added for convenience.

* `minimal`: minimal configuration
* `host-f32`: compiles with `mrb_float` as 32 bit `float`
* `host-nofloat`: compiles with no float configuration

And `android_arm64-v8a.rb` was renamed to `android_arm64_v8a.rb` for consistency.

# Configuration Options Changed

Some configuration macros are available:

* `MRB_WORDBOX_NO_FLOAT_TRUNCATE`: by default, float values are packed in the word if possible, but define this macro to allocate float values in the heap.
* `MRB_USE_RO_DATA_P_ETEXT`: define this macro if `_etext` is available on your platform.
* `MRB_NO_DEFAULT_RO_DATA_P`: define this macro to avoid using predefined `mrb_ro_data_p()` function

# Language Changes

## Keyword Arguments

CRuby3.0 compatible keyword arguments are introduced.
Keyword arguments are basically separated from ordinal arguments

## New Methods

* `Array#product`
* `Array#repeated_combination`
* `Array#repeated_permutation`
* `Random.bytes`
* `Random#bytes`

# Internal Changes

## Reintroduced Instructions

`mruby3.0` removed `OP_EXT1`, `OP_EXT2`, `OP_EXT3` for operand extension. But the operand size limitations was too tight for real-world application.
`mruby3.1` reintroduces those extension instructions.

## Removed Instructions

`mruby3.1` removed following instructions.

* `OP_LOADL16`
* `OP_LOADSYM16`
* `OP_STRING16`
* `OP_LAMBDA16`
* `OP_BLOCK16`
* `OP_METHOD16`
* `OP_EXEC16`

Those instructions are no longer needed by reintroduction of extension instructions.

* `OP_SENDV`
* `OP_SENDVB`

Those instructions for method calls with variable number of arguments are no longer needed. They are covered by `OP_SEND` instruction with `n=15`.

## New Instructions

`mruby3.1` introduces following new instructions.

* `OP_GETIDX`: takes 2 operands `a[b]`
* `OP_SETIDX`: takes 3 operands `a[b]=c`
* `OP_SSEND`: takes 3 operands `a=self.b(c...)`; see `OP_SEND`
* `OP_SSENDB`: takes 3 operands `a=self.b(c...){...}`; see `OP_SEND`

### `OP_GETIDX` and `OP_SETIDX`

Execute `obj[int]` and `obj[int] = value` respectively, where `obj` is `string|array|hash`.

### `OP_SSEND` and `OP_SSENDB`

They are similar to `OP_SEND` and `OP_SENDB` respectively. They initialize the `R[a]` by `self` first so that we can skip one `OP_LOADSELF` instruction for each call.

## Changed Instructions

### `OP_SEND` and `OP_SENDB`

Method calling instructions are unified. Now `OP_SEND` and `OP_SENDB` (method call with a block) can support both splat arguments and keyword arguments as well.

The brief description of the instructions:

|`OP_SEND`   | BBB | `R[a] = R[a].call(Syms[b],R[a+1..n],R[a+n+1],R[a+n+2]..nk) c=n|nk<<4`                    |
|`OP_SENDB`  | BBB | `R[a] = R[a].call(Syms[b],R[a+1..n],R[a+n+1..nk],R[a+n+2..nk],&R[a+n+2*nk+2]) c=n|nk<<4` |

Operand C specifies the number of arguments. Lower 4 bits (`n`) represents the number of ordinal arguments, and higher 4 bits (`nk`) represents the number of keyword arguments.
When `n == 15`, the method takes arguments packed in an array. When `nk == 15`, the method takes keyword arguments are packed in a hash.

### `OP_ARYPUSH`

Now takes 2 operands and pushes multiple entries to an array.

## Boxing Updated

### Word Boxing

`MRB_WORD_BOXING` now packs floating point numbers in the word, if the size of `mrb_float` is equal or smaller than the size of `mrb_int` by default.
If the size of `mrb_float` and `mrb_int` are same, the last 2 bits in the `mrb_float` are trimmed and used as flags. If you need full precision, you need to define `MRB_WORDBOX_NO_FLOAT_TRUNCATE` as described above.

### NaN Boxing

Previous NaN boxing packs values in NaN representation, but pointer retrievals are far more frequent than floating point number references. So we add constant offset to NaN representation to clear higher bits of pointer representation. This representation is called "Favor Pointer" NaN Boxing.

Also, previous NaN boxing limit the size of `mrb_int` to 4 bytes (32 bits) to fit in NaN values. Now we allocates integer values in the heap, if the value does not fit in the 32 bit range, just like we did in Word Boxing.

## Constant Folding

The code generator was updated to reduce the number of instructions, e.g.

```
a = 2 * 5
```

will be interpreted as

```
a = 10
```

In addition, we have improved peephole optimizations, for example:

```
GETIV R4 :@foo
MOVE R1 R4
```

to

```
GETIV R1 :@foo
```

## `String#hash` now use `FNV1a` algorithm

For better and faster hash values.
