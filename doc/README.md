# mruby Documentation

## Getting Started

New to mruby? Start here:

| Document | Description |
| -------- | ----------- |
| [Getting Started](guides/getting-started.md) | Build mruby and run your first program |
| [Language Features](guides/language.md) | Ruby subset supported by mruby |
| [Limitations](limitations.md) | Behavioral differences from CRuby |

## Guides (for embedders and gem authors)

### Embedding mruby in C

| Document | Description |
| -------- | ----------- |
| [C API Reference](guides/capi.md) | Values, classes, methods, error handling, fibers |
| [GC Arena](guides/gc-arena-howto.md) | Managing temporary objects in C extensions |
| [Linking](guides/link.md) | Linking with `libmruby` |
| [Amalgamation](guides/amalgamation.md) | Single-file build for easy integration |
| [Precompiled Symbols](guides/symbol.md) | Compile-time symbol allocation |

### Building and Configuring

| Document | Description |
| -------- | ----------- |
| [Compilation](guides/compile.md) | Build system, cross-compilation, toolchains |
| [Build Configuration](guides/mrbconf.md) | Compile-time macros (`MRB_*` flags) |
| [mrbgems](guides/mrbgems.md) | Creating and managing gems |
| [Memory](guides/memory.md) | Allocator customization and heap regions |

### Tools

| Document | Description |
| -------- | ----------- |
| [Debugger](guides/debugger.md) | Using `mrdb` for debugging |
| [ROM Method Tables](guides/rom-method-table.md) | Read-only method tables for constrained devices |

### Reference

| Document | Description |
| -------- | ----------- |
| [Directory Structure](guides/hier.md) | Source tree layout |

## Internals (for mruby contributors)

Start with [Architecture](internal/architecture.md) for an overview,
then dive into the subsystem you need:

| Document | Description |
| -------- | ----------- |
| [Architecture](internal/architecture.md) | Overview of object model, VM, GC, compiler |
| [Virtual Machine](internal/vm.md) | Dispatch loop, call frames, method lookup, fibers |
| [Garbage Collector](internal/gc.md) | Tri-color marking, write barriers, generational GC |
| [Compiler Pipeline](internal/compiler.md) | Parser, code generator, IRep, binary format |
| [Opcodes](internal/opcode.md) | VM instruction set reference |
| [Value Boxing](internal/boxing.md) | How `mrb_value` encodes types |

## Release Notes

- [mruby 3.4](mruby3.4.md)
- [mruby 3.3](mruby3.3.md)
- [mruby 3.2](mruby3.2.md)
- [mruby 3.1](mruby3.1.md)
- [mruby 3.0](mruby3.0.md)
