<!-- summary: About mruby Architecture -->

# mruby Architecture

This document provides a map of mruby's internals for developers who
want to understand, debug, or contribute to the codebase.

## Overview

mruby's execution pipeline:

```text
Ruby source → Parser → AST → Code Generator → Bytecode (irep)
                                                    ↓
                                                   VM → Result
```

The design priority is **memory > performance > readability**.

## Object Model

All heap-allocated Ruby objects share a common header (`MRB_OBJECT_HEADER`):

```text
struct RBasic (8 bytes on 64-bit)
┌──────────────┬─────┬──────────┬────────┬───────┐
│ RClass *c    │ tt  │ gc_color │ frozen │ flags │
│ (class ptr)  │ 8b  │ 3b       │ 1b     │ 20b   │
└──────────────┴─────┴──────────┴────────┴───────┘
```

All object structs embed this header via `MRB_OBJECT_HEADER`:

| Struct       | Ruby Type        | Extra Fields                       |
| ------------ | ---------------- | ---------------------------------- |
| `RObject`    | Object instances | `iv` (instance variables)          |
| `RClass`     | Class/Module     | `iv`, `mt` (method table), `super` |
| `RString`    | String           | embedded or heap buffer, length    |
| `RArray`     | Array            | embedded or heap buffer, length    |
| `RHash`      | Hash             | hash table or k-v array            |
| `RProc`      | Proc/Lambda      | `irep` or C function, environment  |
| `RData`      | C data wrapper   | `void *data`, `mrb_data_type`      |
| `RFiber`     | Fiber            | `mrb_context`                      |
| `RException` | Exception        | `iv`                               |

Immediate values (Integer, Symbol, `true`, `false`, `nil`) are encoded
directly in `mrb_value` without heap allocation. The encoding depends on
the boxing mode (see [boxing.md](boxing.md)).

Objects must fit within 5 words (`mrb_static_assert_object_size`).

## Virtual Machine

The VM is register-based, using two stacks: a **value stack** for
registers (locals, temporaries, arguments) and a **call info stack**
for tracking method/block call frames. Each method call pushes a
`mrb_callinfo` frame with the method symbol, proc, PC, and argument
counts.

The dispatch loop in `mrb_vm_run()` decodes opcodes and operates on
registers. Method dispatch looks up the receiver's class method table
(with a per-state method cache), then either calls a C function
directly or pushes a new call frame for Ruby methods.

Exception handling uses `setjmp`/`longjmp` (or C++ exceptions if
configured). Rescue/ensure handler tables are stored in each irep
and searched during stack unwinding.

See [vm.md](vm.md) for detailed VM internals, [opcode.md](opcode.md)
for the full instruction set.

## Garbage Collector

The GC uses **tri-color incremental mark-and-sweep** with an optional
**generational mode**. Objects are colored white (unmarked), gray
(marked, children pending), black (fully marked), or red (static/ROM).

The three-phase cycle (root scan, incremental marking, sweep) runs
in small steps between VM instructions to avoid long pauses. Write
barriers (`mrb_field_write_barrier`, `mrb_write_barrier`) maintain
correctness during incremental marking.

The GC arena protects newly created objects in C code. Heap regions
(`mrb_gc_add_region`) support embedded systems with fixed memory banks.

See [gc.md](gc.md) for detailed GC internals,
[../guides/gc-arena-howto.md](../guides/gc-arena-howto.md) for arena
usage patterns, [../guides/memory.md](../guides/memory.md) for memory
management.

## Compiler Pipeline

The compiler transforms Ruby source code through three stages:

1. **Parser** (`parse.y`): Lrama/Bison grammar produces an AST of
   `mrb_ast_node` structures, tracking lexer state and local scopes.
2. **Code Generator** (`codegen.c`): walks the AST and emits bytecode
   into `mrb_irep` structures (instruction sequence, literal pool,
   symbol table, child ireps).
3. **Execution**: the irep is wrapped in an `RProc` and executed by
   the VM, or serialized to `.mrb` binary format.

Alternative loading paths include `mrb_load_string()` (compile and
run), `mrb_load_irep()` (load precompiled bytecode), and `mrbc`
(ahead-of-time compilation).

See [compiler.md](compiler.md) for detailed compiler internals,
[opcode.md](opcode.md) for the instruction set.

## Source File Map

### Core (`src/`)

| File          | Responsibility                                 |
| ------------- | ---------------------------------------------- |
| `vm.c`        | Bytecode dispatch loop, method invocation      |
| `state.c`     | `mrb_state` init/close, irep management        |
| `gc.c`        | Garbage collector (mark-sweep, incremental)    |
| `class.c`     | Class/module definition, method tables         |
| `object.c`    | Core object operations                         |
| `variable.c`  | Instance/class/global variables, object shapes |
| `proc.c`      | Proc/Lambda/closure handling                   |
| `array.c`     | Array implementation                           |
| `string.c`    | String implementation (embedded, shared, heap) |
| `hash.c`      | Hash implementation (open addressing)          |
| `numeric.c`   | Integer/Float arithmetic                       |
| `symbol.c`    | Symbol table and interning                     |
| `range.c`     | Range implementation                           |
| `error.c`     | Exception creation, raise, backtrace           |
| `kernel.c`    | Kernel module methods                          |
| `load.c`      | `.mrb` bytecode loading                        |
| `dump.c`      | Bytecode serialization (write `.mrb`)          |
| `print.c`     | Print/puts/p output                            |
| `backtrace.c` | Stack trace generation                         |

### Compiler (`mrbgems/mruby-compiler/core/`)

| File        | Responsibility                  |
| ----------- | ------------------------------- |
| `parse.y`   | Yacc grammar → AST              |
| `y.tab.c`   | Generated parser (from parse.y) |
| `codegen.c` | AST → bytecode (irep)           |
| `node.h`    | AST node type definitions       |

### Key Headers (`include/mruby/`)

| Header       | Contents                              |
| ------------ | ------------------------------------- |
| `mruby.h`    | `mrb_state`, core API declarations    |
| `value.h`    | `mrb_value`, type enums, value macros |
| `object.h`   | `RBasic`, `RObject`, object header    |
| `class.h`    | `RClass`, method table types          |
| `string.h`   | `RString`, string macros              |
| `array.h`    | `RArray`, array macros                |
| `hash.h`     | `RHash`, hash API                     |
| `data.h`     | `RData`, C data wrapping              |
| `irep.h`     | `mrb_irep`, bytecode structures       |
| `compile.h`  | Compiler context, `mrb_load_string`   |
| `boxing_*.h` | Value boxing implementations          |

## mrbgems System

Gems are the module system for mruby. Each gem lives in
`mrbgems/mruby-*/` and contains:

```text
mruby-example/
├── mrbgem.rake       gem specification (name, deps, bins)
├── src/              C source files
├── mrblib/           Ruby source files (compiled to bytecode)
├── include/          C headers
├── test/             mrbtest test files
└── bintest/          binary test files (CRuby)
```

At build time, gem Ruby files are compiled with `mrbc` and linked into
`libmruby.a`. Gem initialization runs in dependency order via
`gem_init.c` (auto-generated).

GemBoxes (`mrbgems/*.gembox`) define named collections of gems
(e.g., `default.gembox` includes `stdlib`, `stdlib-ext`, `stdlib-io`,
`math`, `metaprog`, and binary tools).
