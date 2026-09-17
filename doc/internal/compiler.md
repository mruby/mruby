<!-- summary: Compiler Pipeline Internals -->

# Compiler Pipeline Internals

This document describes mruby's compilation pipeline for developers
working on the parser, code generator, or bytecode format.

**Read this if you are:** following a Ruby construct from the parse
tree to the opcodes, debugging codegen issues (wrong registers,
missing opcodes), working with the `.mrb` binary format, or teaching
the code generator a node Prism already parses.

## Pipeline Overview

```text
Ruby source
    |
    v
 Parser (Prism)
    |
    v
   AST (pm_node_t)
    |
    v
 Code Generator (codegen.c)
    |
    v
 Bytecode (mrc_irep)
    |
    v
 VM execution  -or-  .mrb binary file
```

## Stage 1: Parser

mruby parses with [Prism](https://github.com/ruby/prism), the parser CRuby
uses, vendored as the `lib/prism` submodule. The build generates Prism's
templated sources into `<build>/prism` and compiles them into the
`mruby-compiler` gem (`mrbgems/mruby-compiler/mrbgem.rake`).

### Compiler Context

One compilation is held in `mrc_ccontext`
(`mrbgems/mruby-compiler/include/mrc_ccontext.h`):

- **p**: the Prism parser (`pm_parser_t`)
- **options**: parse options (`pm_options_t`), carrying the enclosing scopes'
  local variable names for `eval` and `binding`
- **filename_table**: where in the joined source each input file begins
- **diagnostic_list**: parser and code generator errors and warnings
- **prism_arena**: the arena Prism allocates from (see
  `mrbgems/mruby-compiler/include/prism_xallocator.h`)
- **no_optimize**, **no_ext_ops**, **keep_lv**, **dump_ast**: switches taken
  from the command line or from an `mrb_ccontext`

### Parsing

`mrc_parse_string_cxt()` and `mrc_parse_file_cxt()` initialize the parser and
call `pm_parse()`, which returns a `pm_node_t` tree (`mrc_node`). Several
input files are concatenated into a single source and parsed together; the
filename table tells which file a position came from, and a lexer callback
moves `p->filepath` across the boundaries.

The same callback counts the brackets the lexer has opened and hands the
parser an EOF token past `PRISM_DEPTH_MAXIMUM` (256, defined in
`mrbgem.rake`), because Prism checks its own depth only where it parses an
expression.

Prism's `error_list` and `warning_list` are copied into the context's
`mrc_diagnostic_list` once the parse is over.

The tree belongs to the Prism arena and is given back with it when the
context is freed.

### Local Variables

Prism resolves local variables while parsing. Each scope node
(`pm_program_node_t`, `pm_def_node_t`, `pm_block_node_t`, and so on) carries
a `pm_constant_id_list_t` of the names declared in it, which the code
generator takes as the scope's `lv`.

Names are interned in Prism's constant pool. `mrc_init_presym()` seeds that
pool with the literals the code generator itself needs
(`mrbgems/mruby-compiler/include/mrc_presym.inc`), so a `pm_constant_id_t`
can be used as a symbol without another lookup.

### Legacy Parser API

`struct mrb_parser_state`, `mrb_parse_string()` and `mrb_generate_code()`
(`include/mruby/compile.h`) remain as public C API. They are a shim over the
above in `mrbgems/mruby-compiler/src/mruby_compat.c`: the fields describing
the old lexer are unused, `p->ylval` holds the `mrc_ccontext`, and `p->tree`
holds the compiled `mrc_irep`.

## Stage 2: Code Generator

The code generator (`mrbgems/mruby-compiler/src/codegen.c`) walks the
Prism tree and emits bytecode into `mrc_irep` structures.

### Codegen Scope

Each lexical scope (method, block, class body) has its own
`mrc_codegen_scope`:

```text
mrc_codegen_scope
+-- sp             current register index (stack pointer)
+-- pc             current instruction count
+-- nlocals        number of local variables
+-- nregs          maximum register index used
+-- lv             local variable list (pm_constant_id_list_t)
+-- aspec          the operand of this scope's OP_ENTER
+-- iseq[]         instruction sequence (grows dynamically)
+-- pool[]         literal pool (strings, numbers)
+-- syms[]         symbol table (method/variable names)
+-- reps[]         child ireps (nested methods/blocks)
+-- catch_table[]  exception handler entries
+-- loop           current loop context stack
+-- rlev           recursion level of the walk
+-- prev           parent scope
+-- mscope         true if method/module/class scope
```

Scopes nest for blocks, method definitions, and class/module bodies.
Each scope produces one `mrc_irep`.

### Register Allocation

The code generator uses a simple stack-based register allocator:

- Register 0 is always `self`
- Registers 1..nlocals-1 are local variables, in the order Prism lists
  them for the scope
- Registers nlocals..nregs-1 are temporaries

`push()` increments `sp` and tracks the high-water mark in `nregs`.
`pop()` decrements `sp`. The allocator is linear - it does not
reuse temporaries within an expression.

### Instruction Emission

Instructions are emitted via helper functions:

- `genop_0(opcode)`: no operands
- `genop_1(opcode, a)`: one operand (auto-extends with OP_EXT1
  if a > 255)
- `genop_2(opcode, a, b)`: two operands (auto-extends with
  OP_EXT1/2/3 as needed)
- `genop_3(opcode, a, b, c)`: three operands
- `genop_W(opcode, a)`: 24-bit operand
- `genop_2S(opcode, a, b)`: one 8-bit + one 16-bit operand

### Peephole Optimization

The code generator performs limited peephole optimizations, such as
removing redundant `OP_MOVE` instructions and combining consecutive
literal loads. Optimization is disabled at jump targets and when
`no_optimize` is set in the compilation context.

### Loop Context

Loop constructs (`while`, `until`, `for`, blocks) push a
`loopinfo` structure that tracks jump destinations:

- `pc0`: destination for `next`
- `pc1`: destination for `redo`
- `pc2`: destination for `break`

Loop types (`LOOP_NORMAL`, `LOOP_BLOCK`, `LOOP_FOR`, `LOOP_BEGIN`,
`LOOP_RESCUE`) determine how `break`/`next`/`redo` behave.

## IRep Structure

The code generator builds `mrc_irep`
(`mrbgems/mruby-compiler/include/mrc_irep.h`), laid out field for field like
the VM's `mrb_irep` (Instruction REPresentation):

```text
mrb_irep
+-- iseq[]      instruction sequence (mrb_code array)
+-- pool[]      literal pool (mrb_irep_pool entries)
+-- syms[]      symbol table (mrb_sym array)
+-- reps[]      child ireps (nested scopes)
+-- lv[]        local variable names (for debugging)
+-- nlocals     local variable count
+-- nregs       register count (locals + temporaries)
+-- ilen        instruction count
+-- plen        pool entry count
+-- slen        symbol count
+-- rlen        child irep count
+-- clen        catch handler count
+-- debug_info  source file/line mapping
```

### Literal Pool

Pool entries store constants referenced by instructions:

| Type             | Tag | Description                     |
| ---------------- | --- | ------------------------------- |
| `IREP_TT_STR`    | 0   | Dynamic string (heap allocated) |
| `IREP_TT_SSTR`   | 2   | Static string (read-only)       |
| `IREP_TT_INT32`  | 1   | 32-bit integer                  |
| `IREP_TT_INT64`  | 3   | 64-bit integer                  |
| `IREP_TT_FLOAT`  | 5   | Floating-point number           |
| `IREP_TT_BIGINT` | 7   | Arbitrary-precision integer     |

The code generator deduplicates pool entries: identical strings
and equal numeric values share the same pool index.

### Catch Handler Table

Exception handler entries are appended after the instruction
sequence in memory:

```text
mrb_irep_catch_handler
+-- type       MRB_CATCH_RESCUE (0) or MRB_CATCH_ENSURE (1)
+-- begin[4]   start PC of protected range
+-- end[4]     end PC of protected range
+-- target[4]  jump target when handler fires
```

During exception unwinding, handlers are searched in reverse order
(last to first) for the current PC.

## Operand Encoding

Standard instructions use 8-bit operands. When a value exceeds
255, extension prefixes widen operands to 16 bits:

| Prefix    | Effect                            |
| --------- | --------------------------------- |
| `OP_EXT1` | First operand (a) becomes 16-bit  |
| `OP_EXT2` | Second operand (b) becomes 16-bit |
| `OP_EXT3` | Both a and b become 16-bit        |

Instruction formats:

| Format | Layout                        | Size    |
| ------ | ----------------------------- | ------- |
| Z      | opcode only                   | 1 byte  |
| B      | opcode + a(8)                 | 2 bytes |
| BB     | opcode + a(8) + b(8)          | 3 bytes |
| BBB    | opcode + a(8) + b(8) + c(8)   | 4 bytes |
| BS     | opcode + a(8) + b(16)         | 4 bytes |
| BSS    | opcode + a(8) + b(16) + c(16) | 6 bytes |
| S      | opcode + a(16)                | 3 bytes |
| W      | opcode + a(24)                | 4 bytes |

See [opcode.md](opcode.md) for the full instruction table.

## OP_ENTER: Argument Specification

`OP_ENTER` encodes a method's argument layout in a 24-bit value
(W format). The bit fields are defined by the `MRB_ARGS_*` macros:

```text
Bits 23       no-block flag
Bits 18-22    required argument count (5 bits, 0-31)
Bits 13-17    optional argument count (5 bits, 0-31)
Bit  12       rest argument flag (*args)
Bits 7-11     post-rest argument count (5 bits, 0-31)
Bits 2-6      keyword argument count (5 bits, 0-31)
Bit  1        keyword rest flag (**kwargs)
Bit  0        block argument flag (&block)
```

Example: `def foo(a, b=1, *rest, &block)` produces an aspec with
1 required, 1 optional, rest flag set, and block flag set.

## Presym: Compile-Time Symbols

The presym system pre-allocates symbol IDs at build time for
frequently used method names and operators. This avoids runtime
string interning for common symbols.

Generated by `lib/mruby/presym.rb` from a scan of the preprocessed
sources, the presym table maps symbol names to compile-time
constants. The numbers are given in layers: the core's symbols first,
in (length, bytes) order, then the symbols each gem adds, in the order
the build lists the gems, so that a source's numbers depend on the
parts of the build before its own and not on the ones after. `id.h`
carries the numbers as macros (as an enum under `MRB_PRESYM_ENUM`,
which `src/symbol.c` defines so that a debugger can show a symbol
number by its name), and `table.h`, which `src/symbol.c` alone
includes, carries the names and lengths by number, the numbers in
sorted order for the binary search of a name, and `MRB_PRESYM_MAX`;
the other sources read the count through `mrb_presym_max()`.

| Macro             | Example               | Symbol        |
| ----------------- | --------------------- | ------------- |
| `MRB_SYM(name)`   | `MRB_SYM(initialize)` | `:initialize` |
| `MRB_SYM_B(name)` | `MRB_SYM_B(map)`      | `:map!`       |
| `MRB_SYM_Q(name)` | `MRB_SYM_Q(nil)`      | `:nil?`       |
| `MRB_SYM_E(name)` | `MRB_SYM_E(name)`     | `:name=`      |
| `MRB_OPSYM(op)`   | `MRB_OPSYM(add)`      | `:+`          |
| `MRB_IVSYM(name)` | `MRB_IVSYM(name)`     | `:@name`      |
| `MRB_CVSYM(name)` | `MRB_CVSYM(count)`    | `:@@count`    |
| `MRB_GVSYM(name)` | `MRB_GVSYM(stdout)`   | `:$stdout`    |

## Binary Format (.mrb)

Precompiled bytecode is stored in the RITE binary format:

```text
Header: "RITE" magic + format version ("0400") + size + compiler name
Section IREP: instruction sequences, pools, symbols
Section DBG:  debug info (optional, filename/line mapping)
Section LVAR: local variable names (optional)
Footer: "END\0"
```

Loading functions:

- `mrb_load_irep(mrb, bin)`: load and execute from byte array
- `mrb_load_irep_buf(mrb, buf, len)`: load with explicit size
  (safer)
- `mrb_read_irep(mrb, bin)`: load without executing (returns
  `mrb_irep*`)
- `mrb_load_irep_file(mrb, fp)`: load from file

`mrb_generate_code()` turns an `mrc_irep` into the `mrb_irep` the VM runs
by dumping it in this format and reading it back, with debug information
always kept (`mruby_compat.c`).

The `mrbc` command-line tool performs ahead-of-time compilation:

```shell
mrbc -o output.mrb source.rb      # binary format
mrbc -Boutput source.rb           # C array format
```

## Compilation Limits

| Limit                  | Value                         |
| ---------------------- | ----------------------------- |
| Max parse depth        | 256 (`PRISM_DEPTH_MAXIMUM`)   |
| Max codegen recursion  | 256 (`MRC_CODEGEN_LEVEL_MAX`) |
| Max local variables    | 255 (uint16 `nlocals`)        |
| Max symbols per irep   | 65535                         |
| Max operand (standard) | 255 (8-bit)                   |
| Max operand (extended) | 65535 (16-bit)                |

## Source Files

| File                                            | Contents                  |
| ----------------------------------------------- | ------------------------- |
| `lib/prism/`                                    | Prism parser (submodule)  |
| `mrbgems/mruby-compiler/src/compile.c`          | Parse and compile entry   |
| `mrbgems/mruby-compiler/src/codegen.c`          | Code generator            |
| `mrbgems/mruby-compiler/src/dump.c`             | `.mrb` writer for `mrc`   |
| `mrbgems/mruby-compiler/src/mruby_compat.c`     | Legacy parser API shim    |
| `mrbgems/mruby-compiler/include/mrc_irep.h`     | `mrc_irep` definition     |
| `mrbgems/mruby-compiler/include/mrc_ccontext.h` | Compiler context          |
| `include/mruby/irep.h`                          | IRep structure definition |
| `include/mruby/compile.h`                       | Compiler context API      |
| `include/mruby/ops.h`                           | Opcode definitions        |
| `src/load.c`                                    | Binary format loader      |
| `src/dump.c`                                    | Binary format writer      |
| `lib/mruby/presym.rb`                           | Presym table generator    |
