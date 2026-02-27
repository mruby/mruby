<!-- summary: Compiler Pipeline Internals -->

# Compiler Pipeline Internals

This document describes mruby's compilation pipeline for developers
working on the parser, code generator, or bytecode format.

## Pipeline Overview

```text
Ruby source
    |
    v
 Lexer/Parser (parse.y)
    |
    v
   AST (mrb_ast_node)
    |
    v
 Code Generator (codegen.c)
    |
    v
 Bytecode (mrb_irep)
    |
    v
 VM execution  -or-  .mrb binary file
```

## Stage 1: Lexer and Parser

The lexer and parser are combined in a single Lrama/Bison grammar
file: `mrbgems/mruby-compiler/core/parse.y`.

### Parser State

The parser maintains extensive state in `mrb_parser_state`:

- **lstate**: current lexer state (EXPR\_BEG, EXPR\_END, EXPR\_ARG,
  EXPR\_DOT, EXPR\_FNAME, etc.). Controls how tokens like `+`/`-`
  are interpreted (sign vs operator) and whether newlines are
  significant.
- **locals**: stack of local variable lists (one per scope), stored
  as cons-lists of symbols.
- **lex\_strterm**: string/heredoc parsing state for handling nested
  interpolation.
- **cond\_stack**, **cmdarg\_stack**: bit stacks tracking
  conditional and command argument contexts.
- **tree**: root AST node after successful parse.
- **error\_buffer**: accumulated parse errors.

### AST Nodes

The parser produces an AST using two node types:

- **Cons-list nodes**: traditional binary tree pairs (car/cdr)
- **Variable-sized nodes**: have a header with `node_type`, `lineno`,
  and `filename_index`

Key node types include `NODE_SCOPE` (new variable scope),
`NODE_STMTS` (statement sequence), `NODE_IF`, `NODE_WHILE`,
`NODE_CALL` (method call), `NODE_DEF` (method definition),
`NODE_CLASS`, `NODE_RESCUE`, `NODE_ENSURE`, etc. See
`mrbgems/mruby-compiler/core/node.h` for the complete list.

### Local Variable Tracking

Local variables are tracked per-scope during parsing:

- `local_add(sym)`: register a new local variable in current scope
- `local_var_p(sym)`: check if a symbol is a local variable (affects
  whether an identifier is parsed as a method call or variable
  reference)

## Stage 2: Code Generator

The code generator (`mrbgems/mruby-compiler/core/codegen.c`) walks
the AST and emits bytecode into `mrb_irep` structures.

### Codegen Scope

Each lexical scope (method, block, class body) has its own
`codegen_scope`:

```text
codegen_scope
+-- sp             current register index (stack pointer)
+-- pc             current instruction count
+-- nlocals        number of local variables
+-- nregs          maximum register index used
+-- lv             local variable list
+-- iseq[]         instruction sequence (grows dynamically)
+-- pool[]         literal pool (strings, numbers)
+-- syms[]         symbol table (method/variable names)
+-- reps[]         child ireps (nested methods/blocks)
+-- catch_table[]  exception handler entries
+-- loop           current loop context stack
+-- prev           parent scope
+-- mscope         true if method/module/class scope
```

Scopes nest for blocks, method definitions, and class/module bodies.
Each scope produces one `mrb_irep`.

### Register Allocation

The code generator uses a simple stack-based register allocator:

- Register 0 is always `self`
- Registers 1..nlocals-1 are local variables (in declaration order)
- Registers nlocals..nregs-1 are temporaries

`push()` increments `sp` and tracks the high-water mark in `nregs`.
`pop()` decrements `sp`. The allocator is linear - it does not
reuse temporaries within an expression.

### Instruction Emission

Instructions are emitted via helper functions:

- `genop_0(opcode)`: no operands
- `genop_1(opcode, a)`: one operand (auto-extends with OP\_EXT1
  if a > 255)
- `genop_2(opcode, a, b)`: two operands (auto-extends with
  OP\_EXT1/2/3 as needed)
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

The compiled bytecode is stored in `mrb_irep` (Instruction
REPresentation):

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

| Type | Tag | Description |
| ---- | --- | ----------- |
| `IREP_TT_STR` | 0 | Dynamic string (heap allocated) |
| `IREP_TT_SSTR` | 2 | Static string (read-only) |
| `IREP_TT_INT32` | 1 | 32-bit integer |
| `IREP_TT_INT64` | 3 | 64-bit integer |
| `IREP_TT_FLOAT` | 5 | Floating-point number |
| `IREP_TT_BIGINT` | 7 | Arbitrary-precision integer |

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

| Prefix | Effect |
| ------ | ------ |
| `OP_EXT1` | First operand (a) becomes 16-bit |
| `OP_EXT2` | Second operand (b) becomes 16-bit |
| `OP_EXT3` | Both a and b become 16-bit |

Instruction formats:

| Format | Layout | Size |
| ------ | ------ | ---- |
| Z | opcode only | 1 byte |
| B | opcode + a(8) | 2 bytes |
| BB | opcode + a(8) + b(8) | 3 bytes |
| BBB | opcode + a(8) + b(8) + c(8) | 4 bytes |
| BS | opcode + a(8) + b(16) | 4 bytes |
| BSS | opcode + a(8) + b(16) + c(16) | 6 bytes |
| S | opcode + a(16) | 3 bytes |
| W | opcode + a(24) | 4 bytes |

See [opcode.md](opcode.md) for the full instruction table.

## OP\_ENTER: Argument Specification

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

Generated by `lib/mruby/presym.rb`, the presym table maps symbol
names to compile-time constants:

| Macro | Example | Symbol |
| ----- | ------- | ------ |
| `MRB_SYM(name)` | `MRB_SYM(initialize)` | `:initialize` |
| `MRB_SYM_B(name)` | `MRB_SYM_B(map)` | `:map!` |
| `MRB_SYM_Q(name)` | `MRB_SYM_Q(nil)` | `:nil?` |
| `MRB_SYM_E(name)` | `MRB_SYM_E(name)` | `:name=` |
| `MRB_OPSYM(op)` | `MRB_OPSYM(add)` | `:+` |
| `MRB_IVSYM(name)` | `MRB_IVSYM(name)` | `:@name` |
| `MRB_CVSYM(name)` | `MRB_CVSYM(count)` | `:@@count` |
| `MRB_GVSYM(name)` | `MRB_GVSYM(stdout)` | `:$stdout` |

## Binary Format (.mrb)

Precompiled bytecode is stored in the RITE binary format:

```text
Header: "RITE" magic + version ("0400") + CRC + size
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

The `mrbc` command-line tool performs ahead-of-time compilation:

```shell
mrbc -o output.mrb source.rb      # binary format
mrbc -Boutput source.rb           # C array format
```

## Compilation Limits

| Limit | Value |
| ----- | ----- |
| Max nesting depth | 256 (`MRB_CODEGEN_LEVEL_MAX`) |
| Max local variables | 255 (uint16 `nlocals`) |
| Max symbols per irep | 65535 |
| Max operand (standard) | 255 (8-bit) |
| Max operand (extended) | 65535 (16-bit) |

## Source Files

| File | Contents |
| ---- | -------- |
| `mrbgems/mruby-compiler/core/parse.y` | Lrama/Bison grammar |
| `mrbgems/mruby-compiler/core/y.tab.c` | Generated parser |
| `mrbgems/mruby-compiler/core/codegen.c` | Code generator |
| `mrbgems/mruby-compiler/core/node.h` | AST node types |
| `include/mruby/irep.h` | IRep structure definition |
| `include/mruby/compile.h` | Compiler context API |
| `include/mruby/ops.h` | Opcode definitions |
| `src/load.c` | Binary format loader |
| `src/dump.c` | Binary format writer |
| `lib/mruby/presym.rb` | Presym table generator |
