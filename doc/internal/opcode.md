<!-- summary: About mruby Virtual Machine Instructions -->

# mruby Bytecode

mruby uses 8-bit instruction opcodes. Each instruction is a single byte,
allowing up to 256 opcodes. Instructions can take 0 to 3 operands.

## Operands

The size of operands can be either 8 bits, 16 bits, or 24 bits.
In the instruction table below, the operand type field describes
the size of each operand.

- `Z`: no operand
- `B`: 8 bits
- `S`: 16 bits
- `W`: 24 bits

If the first and second operands are of type `B` (8 bits), they may be
extended to 16 bits by the operand extension instruction immediately
preceding them.
See also `OP_EXT1`, `OP_EXT2` and `OP_EXT3`.

## Instruction Table

| No. | Instruction Name | Operand type | Semantics                                                         |
| --: | ---------------- | ------------ | ----------------------------------------------------------------- |
|   0 | `OP_NOP`         | `Z`          | no operation                                                      |
|   1 | `OP_MOVE`        | `BB`         | `R[a] = R[b]`                                                     |
|   2 | `OP_LOADL`       | `BB`         | `R[a] = Pool[b]`                                                  |
|   3 | `OP_LOADI8`      | `BB`         | `R[a] = mrb_int(b)`                                               |
|   4 | `OP_LOADINEG`    | `BB`         | `R[a] = mrb_int(-b)`                                              |
|   5 | `OP_LOADI__1`    | `B`          | `R[a] = mrb_int(-1)`                                              |
|   6 | `OP_LOADI_0`     | `B`          | `R[a] = mrb_int(0)`                                               |
|   7 | `OP_LOADI_1`     | `B`          | `R[a] = mrb_int(1)`                                               |
|   8 | `OP_LOADI_2`     | `B`          | `R[a] = mrb_int(2)`                                               |
|   9 | `OP_LOADI_3`     | `B`          | `R[a] = mrb_int(3)`                                               |
|  10 | `OP_LOADI_4`     | `B`          | `R[a] = mrb_int(4)`                                               |
|  11 | `OP_LOADI_5`     | `B`          | `R[a] = mrb_int(5)`                                               |
|  12 | `OP_LOADI_6`     | `B`          | `R[a] = mrb_int(6)`                                               |
|  13 | `OP_LOADI_7`     | `B`          | `R[a] = mrb_int(7)`                                               |
|  14 | `OP_LOADI16`     | `BS`         | `R[a] = mrb_int(b)`                                               |
|  15 | `OP_LOADI32`     | `BSS`        | `R[a] = mrb_int((b<<16)+c)`                                       |
|  16 | `OP_LOADSYM`     | `BB`         | `R[a] = Syms[b]`                                                  |
|  17 | `OP_LOADNIL`     | `B`          | `R[a] = nil`                                                      |
|  18 | `OP_LOADSELF`    | `B`          | `R[a] = self`                                                     |
|  19 | `OP_LOADTRUE`    | `B`          | `R[a] = true`                                                     |
|  20 | `OP_LOADFALSE`   | `B`          | `R[a] = false`                                                    |
|  21 | `OP_GETGV`       | `BB`         | `R[a] = getglobal(Syms[b])`                                       |
|  22 | `OP_SETGV`       | `BB`         | `setglobal(Syms[b], R[a])`                                        |
|  23 | `OP_GETSV`       | `BB`         | `R[a] = Special[Syms[b]]`                                         |
|  24 | `OP_SETSV`       | `BB`         | `Special[Syms[b]] = R[a]`                                         |
|  25 | `OP_GETIV`       | `BB`         | `R[a] = ivget(Syms[b])`                                           |
|  26 | `OP_SETIV`       | `BB`         | `ivset(Syms[b],R[a])`                                             |
|  27 | `OP_GETCV`       | `BB`         | `R[a] = cvget(Syms[b])`                                           |
|  28 | `OP_SETCV`       | `BB`         | `cvset(Syms[b],R[a])`                                             |
|  29 | `OP_GETCONST`    | `BB`         | `R[a] = constget(Syms[b])`                                        |
|  30 | `OP_SETCONST`    | `BB`         | `constset(Syms[b],R[a])`                                          |
|  31 | `OP_GETMCNST`    | `BB`         | `R[a] = R[a]::Syms[b]`                                            |
|  32 | `OP_SETMCNST`    | `BB`         | `R[a+1]::Syms[b] = R[a]`                                          |
|  33 | `OP_GETUPVAR`    | `BBB`        | `R[a] = uvget(b,c)`                                               |
|  34 | `OP_SETUPVAR`    | `BBB`        | `uvset(b,c,R[a])`                                                 |
|  35 | `OP_GETIDX`      | `B`          | `R[a] = R[a][R[a+1]]`                                             |
|  36 | `OP_GETIDX0`     | `BB`         | `R[a] = R[b][0]`                                                  |
|  37 | `OP_SETIDX`      | `B`          | `R[a][R[a+1]] = R[a+2]`                                           |
|  38 | `OP_JMP`         | `S`          | `pc += a`                                                         |
|  39 | `OP_JMPIF`       | `BS`         | `if R[a] pc += b`                                                 |
|  40 | `OP_JMPNOT`      | `BS`         | `if !R[a] pc += b`                                                |
|  41 | `OP_JMPNIL`      | `BS`         | `if R[a]==nil pc += b`                                            |
|  42 | `OP_JMPUW`       | `S`          | `unwind_and_jump_to(a)`                                           |
|  43 | `OP_EXCEPT`      | `B`          | `R[a] = exc`                                                      |
|  44 | `OP_RESCUE`      | `BB`         | `R[b] = R[a].isa?(R[b])`                                          |
|  45 | `OP_RAISEIF`     | `B`          | `raise(R[a]) if R[a]`                                             |
|  46 | `OP_MATCHERR`    | `B`          | `raise NoMatchingPatternError unless R[a]`                        |
|  47 | `OP_SSEND`       | `BBB`        | `R[a] = self.send(Syms[b],R[a+1]..,R[a+n+1]:R[a+n+2]..) (c=n\     |
|  48 | `OP_SSEND0`      | `BB`         | `R[a] = self.send(Syms[b])` (no args)                             |
|  49 | `OP_SSENDB`      | `BBB`        | `R[a] = self.send(Syms[b],R[a+1]..,&R[a+n+2k+1])`                 |
|  50 | `OP_SEND`        | `BBB`        | `R[a] = R[a].send(Syms[b],R[a+1]..,R[a+n+1]:R[a+n+2]..) (c=n\     |
|  51 | `OP_SEND0`       | `BB`         | `R[a] = R[a].send(Syms[b])` (no args)                             |
|  52 | `OP_SENDB`       | `BBB`        | `R[a] = R[a].send(Syms[b],R[a+1]..,&R[a+n+2k+1])`                 |
|  53 | `OP_CALL`        | `Z`          | `self.call(*, **, &)` (tailcall)                                  |
|  54 | `OP_BLKCALL`     | `BB`         | `R[a] = R[a].call(R[a+1],...,R[a+b])` (direct block call)         |
|  55 | `OP_SUPER`       | `BB`         | `R[a] = super(R[a+1],...,R[a+b+1])`                               |
|  56 | `OP_ARGARY`      | `BS`         | `R[a] = argument array (16=m5:r1:m5:d1:lv4)`                      |
|  57 | `OP_ENTER`       | `W`          | `arg setup according to flags (24=n1:m5:o5:r1:m5:k5:d1:b1)`       |
|  58 | `OP_KEY_P`       | `BB`         | `R[a] = kdict.key?(Syms[b])`                                      |
|  59 | `OP_KEYEND`      | `Z`          | `raise unless kdict.empty?`                                       |
|  60 | `OP_KARG`        | `BB`         | `R[a] = kdict[Syms[b]]; kdict.delete(Syms[b])`                    |
|  61 | `OP_RETURN`      | `B`          | `return R[a]` (normal)                                            |
|  62 | `OP_RETURN_BLK`  | `B`          | `return R[a]` (in-block return)                                   |
|  63 | `OP_RETSELF`     | `Z`          | `return self`                                                     |
|  64 | `OP_RETNIL`      | `Z`          | `return nil`                                                      |
|  65 | `OP_RETTRUE`     | `Z`          | `return true`                                                     |
|  66 | `OP_RETFALSE`    | `Z`          | `return false`                                                    |
|  67 | `OP_BREAK`       | `B`          | `break R[a]`                                                      |
|  68 | `OP_BLKPUSH`     | `BS`         | `R[a] = block (16=m5:r1:m5:d1:lv4)`                               |
|  69 | `OP_ADD`         | `B`          | `R[a] = R[a] + R[a+1]`                                            |
|  70 | `OP_ADDI`        | `BB`         | `R[a] = R[a] + mrb_int(b)`                                        |
|  71 | `OP_SUB`         | `B`          | `R[a] = R[a] - R[a+1]`                                            |
|  72 | `OP_SUBI`        | `BB`         | `R[a] = R[a] - mrb_int(b)`                                        |
|  73 | `OP_ADDILV`      | `BBB`        | `R[a] = R[a] + mrb_int(c)` (with local variable fallback)         |
|  74 | `OP_SUBILV`      | `BBB`        | `R[a] = R[a] - mrb_int(c)` (with local variable fallback)         |
|  75 | `OP_MUL`         | `B`          | `R[a] = R[a] * R[a+1]`                                            |
|  76 | `OP_DIV`         | `B`          | `R[a] = R[a] / R[a+1]`                                            |
|  77 | `OP_EQ`          | `B`          | `R[a] = R[a] == R[a+1]`                                           |
|  78 | `OP_LT`          | `B`          | `R[a] = R[a] < R[a+1]`                                            |
|  79 | `OP_LE`          | `B`          | `R[a] = R[a] <= R[a+1]`                                           |
|  80 | `OP_GT`          | `B`          | `R[a] = R[a] > R[a+1]`                                            |
|  81 | `OP_GE`          | `B`          | `R[a] = R[a] >= R[a+1]`                                           |
|  82 | `OP_ARRAY`       | `BB`         | `R[a] = ary_new(R[a],R[a+1]..R[a+b])`                             |
|  83 | `OP_ARRAY2`      | `BBB`        | `R[a] = ary_new(R[b],R[b+1]..R[b+c])`                             |
|  84 | `OP_ARYCAT`      | `B`          | `ary_cat(R[a],R[a+1])`                                            |
|  85 | `OP_ARYPUSH`     | `BB`         | `ary_push(R[a],R[a+1]..R[a+b])`                                   |
|  86 | `OP_ARYSPLAT`    | `B`          | `R[a] = ary_splat(R[a])`                                          |
|  87 | `OP_AREF`        | `BBB`        | `R[a] = R[b][c]`                                                  |
|  88 | `OP_ASET`        | `BBB`        | `R[b][c] = R[a]`                                                  |
|  89 | `OP_APOST`       | `BBB`        | `*R[a],R[a+1]..R[a+c] = R[a][b..]`                                |
|  90 | `OP_INTERN`      | `B`          | `R[a] = intern(R[a])`                                             |
|  91 | `OP_SYMBOL`      | `BB`         | `R[a] = intern(Pool[b])`                                          |
|  92 | `OP_STRING`      | `BB`         | `R[a] = str_dup(Pool[b])`                                         |
|  93 | `OP_STRCAT`      | `B`          | `str_cat(R[a],R[a+1])`                                            |
|  94 | `OP_HASH`        | `BB`         | `R[a] = hash_new(R[a],R[a+1]..R[a+b*2-1])`                        |
|  95 | `OP_HASHADD`     | `BB`         | `hash_push(R[a],R[a+1]..R[a+b*2])`                                |
|  96 | `OP_HASHCAT`     | `B`          | `R[a] = hash_cat(R[a],R[a+1])`                                    |
|  97 | `OP_LAMBDA`      | `BB`         | `R[a] = lambda(Irep[b],L_LAMBDA)`                                 |
|  98 | `OP_BLOCK`       | `BB`         | `R[a] = lambda(Irep[b],L_BLOCK)`                                  |
|  99 | `OP_METHOD`      | `BB`         | `R[a] = lambda(Irep[b],L_METHOD)`                                 |
| 100 | `OP_RANGE_INC`   | `B`          | `R[a] = range_new(R[a],R[a+1],FALSE)`                             |
| 101 | `OP_RANGE_EXC`   | `B`          | `R[a] = range_new(R[a],R[a+1],TRUE)`                              |
| 102 | `OP_OCLASS`      | `B`          | `R[a] = ::Object`                                                 |
| 103 | `OP_CLASS`       | `BB`         | `R[a] = newclass(R[a],Syms[b],R[a+1])`                            |
| 104 | `OP_MODULE`      | `BB`         | `R[a] = newmodule(R[a],Syms[b])`                                  |
| 105 | `OP_EXEC`        | `BB`         | `R[a] = blockexec(R[a],Irep[b])`                                  |
| 106 | `OP_DEF`         | `BB`         | `R[a].newmethod(Syms[b],R[a+1]); R[a] = Syms[b]`                  |
| 107 | `OP_TDEF`        | `BBB`        | `target_class.newmethod(Syms[b],Irep[c]); R[a] = Syms[b]`         |
| 108 | `OP_SDEF`        | `BBB`        | `R[a].singleton_class.newmethod(Syms[b],Irep[c]); R[a] = Syms[b]` |
| 109 | `OP_ALIAS`       | `BB`         | `alias_method(target_class,Syms[a],Syms[b])`                      |
| 110 | `OP_UNDEF`       | `B`          | `undef_method(target_class,Syms[a])`                              |
| 111 | `OP_SCLASS`      | `B`          | `R[a] = R[a].singleton_class`                                     |
| 112 | `OP_TCLASS`      | `B`          | `R[a] = target_class`                                             |
| 113 | `OP_DEBUG`       | `BBB`        | `print a,b,c`                                                     |
| 114 | `OP_ERR`         | `B`          | `raise(LocalJumpError, Pool[a])`                                  |
| 115 | `OP_EXT1`        | `Z`          | make 1st operand (a) 16 bit                                       |
| 116 | `OP_EXT2`        | `Z`          | make 2nd operand (b) 16 bit                                       |
| 117 | `OP_EXT3`        | `Z`          | make 1st and 2nd operands 16 bit                                  |
| 118 | `OP_STOP`        | `Z`          | stop VM                                                           |

## Notes

### OP_SEND0 / OP_SSEND0

These are optimized versions of `OP_SEND` / `OP_SSEND` for zero-argument
method calls (no operand `c` needed).

### OP_RETSELF / OP_RETNIL / OP_RETTRUE / OP_RETFALSE

These are optimized return instructions that avoid loading a value into
a register before returning. Common patterns like `attr_reader` methods
(`return self.@x`) and predicate methods (`return true`/`return false`)
benefit from these specialized opcodes.

### OP_BLKCALL

Direct block invocation that bypasses method dispatch. Used when calling
a block argument directly (e.g., `yield` or `block.call`).

### OP_ADDILV / OP_SUBILV

Optimized integer increment/decrement that keeps operands for method
call fallback when the receiver is not a Fixnum.

### OP_TDEF / OP_SDEF

Optimized method definition. `OP_TDEF` defines a method on the
`target_class` directly from an irep without creating an intermediate
`RProc`. `OP_SDEF` does the same for singleton methods.

### OP_MATCHERR

Raises `NoMatchingPatternError` when a pattern match fails. Used by
the `case`/`in` pattern matching syntax.

### OP_GETIDX / OP_GETIDX0 / OP_SETIDX Optimization

These instructions optimize `[]` and `[]=` access for Array, Hash, and String.

**OP_GETIDX** uses direct function calls:

- `Array`: `mrb_ary_entry()` (integer index only)
- `Hash`: `mrb_hash_get()`
- `String`: `mrb_str_aref()` (integer/string/range index)

**OP_GETIDX0** is a specialized variant for index 0 (e.g., `ary[0]`).

**OP_SETIDX** uses direct function calls:

- `Array`: `mrb_ary_set()` (integer index only)
- `Hash`: `mrb_hash_set()`

**Fallback to method dispatch** occurs when:

- Object is a subclass (e.g., `MyArray < Array`)
- Object has a singleton class (singleton methods defined)
- Index type is not supported (e.g., non-integer for Array)

This allows subclasses to override `[]`/`[]=` while base classes remain optimized.
