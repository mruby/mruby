# mruby-compiler / mruby-compiler2

This project is a Prism-based Ruby parser and bytecode compiler for mruby, PicoRuby, and FemtoRuby.

Read this note first:

- If this file is under `mruby/mrbgems/mruby-compiler`, you are reading the canonical mruby core mgem. This is where development happens.
- If this file is at the top of `picoruby/mruby-compiler2`, you are reading the standalone mirror. That repository exists for projects that need to use the compiler independently from mruby and pin it at an arbitrary commit.
- Please send patches to `mruby/mruby`, not to `picoruby/mruby-compiler2`. The standalone repository is synchronized from mruby after changes land there.

The compiler must remain usable without fundamentally depending on mruby. mruby uses it as `mruby-compiler`; PicoRuby and FemtoRuby use the same compiler through the standalone `mruby-compiler2` mirror.

## Using mruby-compiler2 as a standalone mgem

Projects that want to pin the compiler independently from mruby can use:

```ruby
MRuby::Build.new do |conf|
  conf.gem github: "picoruby/mruby-compiler2", commit: "..."
end
```

`picoruby/mruby-compiler2` is synchronized from
`mruby/mruby/mrbgems/mruby-compiler`. It may lag behind mruby until the sync workflow opens and merges a mirror PR.

## Using Prism in mruby

Prism support in mruby is opt-in:

```sh
MRB_COMPILER_PRISM=yes rake test
```

The top-level mruby `Rakefile` maps `MRB_COMPILER_PRISM=yes` to `MRUBY_CONFIG=prism` only when neither `MRUBY_CONFIG` nor `CONFIG` is already set. Explicit config selection continues to win:

```sh
MRUBY_CONFIG=ci/gcc-clang rake test
MRUBY_CONFIG=prism rake test
```

This keeps the normal mruby build and CI path compatible with the existing `mruby-compiler` gem while Prism support is still being aligned.

The Prism route currently includes:

- `mruby-compiler`
- `mruby-bin-mrbc`
- `mruby-bin-mruby`
- `mruby-bin-mirb`
- `mruby-eval`

## Prism submodule and bootstrap

`lib/prism` is a git submodule pointing at `ruby/prism`. Pin it at a released tag, Prism 2.1.0 or later.

Prism 2 keeps the parser, the options and the constant pool opaque. The compiler reads the internal headers (`prism/internal/*.h`) for the constant pool and the parser state, so the compiler and the vendored Prism must always be built together. Do not point the submodule at a Prism the compiler was not tested with.

The gem bootstrap runs before `mrbgem.rake` collects Prism C sources with `Dir.glob`. This matters for fresh checkouts, where the submodule may not yet be initialized and generated Prism files may be absent.

During normal build setup, `mrbgem.rake` does the following:

1. If `lib/prism/templates/template.rb` is missing, run:

   ```sh
   git submodule update --init lib/prism
   ```

2. If generated Prism files such as `src/node.c`, `src/serialize.c`, `include/prism/ast.h`, or `include/prism/internal/diagnostic.h` are missing, run:

   ```sh
   ruby templates/template.rb
   ```

This bootstrap belongs in the compiler gem because the same source tree is used from mruby, PicoRuby, FemtoRuby, and the standalone mirror.

## Build macros

`mrbgem.rake` sets most macros from the build configuration. A build configuration adds the others with `conf.cc.defines`. The macros reach both the compiler and the vendored Prism, because every Prism translation unit includes `include/prism_xallocator.h` through `prism/internal/allocator.h`.

### Macros the gem passes to Prism

- `PRISM_XALLOCATOR`. Always set. Prism takes `xmalloc()` and friends from `include/prism_xallocator.h` instead of libc. The header maps them to the allocator of the target (see below).
- `PRISM_DEPTH_MAXIMUM=256`. Set unless the build configuration sets its own value. Prism is a recursive-descent parser and mruby has no backstop for a machine stack overflow. The default limit of 10000 overflows the C stack before it trips. The value 256 matches `MRC_CODEGEN_LEVEL_MAX`, so it rejects nothing the codegen can compile. A target with a small stack can lower it. A build that raises it must also raise `MRC_CODEGEN_LEVEL_MAX` and have the C stack to match.
- `PRISM_BUILD_MINIMAL`. Set unless `MRC_DEBUG` is set. Prism leaves out the pretty printer, the serializer and the JSON output. The compiler does not call them.

### Prism macros a build configuration can set

Prism takes its memory in arena blocks. A parser has a metadata arena for its own state, a scratch arena for data that lives only through the parse, and it builds the tree in the node arena that the compiler context owns. Each arena takes blocks from `xmalloc()` and gives all of them back at once.

- `PM_ARENA_INITIAL_SIZE`. The data size in bytes of the first blocks of every arena. The Prism default is 8192. A smaller value lowers the memory that a parse of a short source holds: every arena takes at least one block, and the parse of one REPL line holds two of them (the metadata arena and the node arena) after it completes. A long source is not affected, since `pm_parser_init()` reserves blocks from the source size. Choose the value from the heap of the target. As one example, the PicoRuby builds for RP2040 and RP2350 set 2048, and a parse of one REPL line then holds 6 KB instead of 16 KB.
- `PM_CONSTANT_POOL_MIN_CAPACITY`. The lower bound of the constant pool capacity that `pm_parser_init()` computes from the source size. The Prism default is 4. The compiler interns its pre-defined symbols (the presyms, 58 of them) into the pool before every parse. The pool doubles when it is 3/4 full and the outgrown arrays stay in the metadata arena until the parser is cleaned up. A value of 128 or more holds the presyms without a resize. The PicoRuby builds above set 128.

### Target selection

- `MRC_TARGET_MRUBY`. The compiler is built for the mruby VM. It uses `mruby.h`, the mruby allocator and the mruby irep layout. `mrbgem.rake` sets it when `PICORB_VM_MRUBY` is set, or when neither `PICORB_VM_MRUBYC` nor `MRB_NO_GEMS` is set (the normal mruby build).
- `MRC_TARGET_MRUBYC`. The compiler is built for the mruby/c VM (FemtoRuby). `mrbgem.rake` sets it when `PICORB_VM_MRUBYC` is set.
- Neither macro: the standalone `mrbc` of PicoRuby. The compiler uses `mrbconf.h` for the integer width and libc for memory.

### Memory

- Default (`MRC_TARGET_MRUBY`): Prism's blocks come from the mruby state through `global_mrb`. The executable or the embedding runtime owns `global_mrb`; the compiler library does not define it. `mrb_malloc()` answers a size of zero with NULL, so the header asks for one byte in that case.
- `MRC_PRISM_LIBC`. Prism's blocks come from libc; the compiler itself keeps the mruby allocator. `mrbgem.rake` sets it for an `MRB_USE_CXX_ABI` build: Prism is compiled as C there, and `mrb_malloc()` raises on failure, which would throw through Prism's frames.
- `MRC_ALLOC_LIBC`. Both Prism and the compiler use libc on the mruby target.
- `MRBC_ALLOC_LIBC`. Both Prism and the compiler use libc on the mruby/c target. Without it they use `mrbc_raw_alloc()` and friends.

### Integer and float width

The compiler dumps irep for one target. `mrc_int` must be as wide as the target's `mrb_int` and no wider, because the loader rejects an `IREP_TT_INT64` entry unless it was built with `MRB_INT64`.

- `MRC_INT32`. `mrc_int` is 32 bits. `mrc_common.h` sets it when `MRB_INT32` is set. Otherwise `MRC_INT64` is set.
- `MRC_32BIT`. The target has 32-bit pointers. Nothing sets it by default; `mrc_common.h` sets `MRC_64BIT` when it is absent.
- `MRC_NO_FLOAT`. No float literals. Set from `MRB_NO_FLOAT`.
- `MRC_USE_FLOAT32`. `mrc_float` is `float`. Set from `MRB_USE_FLOAT32`.

### Debugging and output

- `MRC_DEBUG`. Debug assertions and the AST dump. `mrbgem.rake` sets it from `MRB_DEBUG`. It also turns `PRISM_BUILD_MINIMAL` off.
- `MRC_DUMP_PRETTY`. `mruby -v` prints the Prism AST with `pm_prettyprint()`. Set together with `MRC_DEBUG`, since `PRISM_BUILD_MINIMAL` stubs the printer out.
- `MRC_NO_STDIO`. No functions that read or write a `FILE`: the file loader, the C dump and the bytecode listing are left out. Set it on a target without stdio.
- `MRC_CODEGEN_LEVEL_MAX`. The maximum nesting of the codegen, 256 by default. See `PRISM_DEPTH_MAXIMUM`.
- `MRC_COMMIT_HASH`, `MRC_COMMIT_BRANCH`, `MRC_COMMIT_TIMESTAMP`. Strings for the version banner. The PicoRuby build sets them from git. mruby leaves them at `"unknown"`.

## Compatibility requirements

The compiler is shared by multiple runtimes. Changes made for mruby must preserve these boundaries.

1. Do not introduce a fundamental dependency on mruby.

   The compiler should continue to build for both `MRC_TARGET_MRUBY` and `MRC_TARGET_MRUBYC`. mruby-only compatibility code must stay behind `MRC_TARGET_MRUBY`.

2. The compiler library must not define `global_mrb`.

   The executable or embedding runtime owns `global_mrb` when the mruby allocator path needs it. This avoids duplicate-symbol conflicts in PicoRuby and r2p2. In standalone mruby, `mrbc-prism`, `mruby-prism`, `mirb-prism`, and `mrbtest` provide the owner when building the Prism route.

3. `PICORB_VM_MRUBY` and `PICORB_VM_MRUBYC` must remain respected.

   PicoRuby builds the mruby VM path. FemtoRuby builds the mruby/c VM path. The gem configuration must not accidentally select `MRC_TARGET_MRUBY` while building FemtoRuby.

4. Public compiler headers are the API boundary for PicoRuby and FemtoRuby.

   Keep the C API usable without depending on mruby internals:
   - `mrc_common.h`
   - `mrc_ccontext.h`
   - `mrc_compile.h`
   - `mrc_dump.h`

5. Do not hard-code the PicoRuby submodule path.

   PicoRuby intentionally uses top-level gems such as `mrbgems/mruby-compiler-prism` or the standalone `mruby-compiler2` mirror and synchronizes them at chosen times.

6. `mruby-eval-prism` is for the mruby VM path.

   FemtoRuby uses `picoruby-eval`, loaded as a prebuilt gem. On FemtoRuby, `eval` is available after:

   ```ruby
   require "eval"
   ```

## Current status

`MRB_COMPILER_PRISM=yes rake test:build` currently passes in the local port. The Prism build links:

- `mrbc-prism`
- `mruby-prism`
- `mirb-prism`
- `mrbtest`

Basic execution works:

```sh
build/host/bin/mruby-prism -e 'p 1 + 2'
#=> 3

build/host/bin/mruby-prism -e 'p eval("1 + 2")'
#=> 3
```

Known remaining work:

- `MRB_COMPILER_PRISM=yes rake test:run:lib` reached `mrbtest` locally but failed in the socket tests with an AF_UNIX bind error. This should be rechecked in upstream CI before treating it as a Prism compiler issue.
- `MRB_COMPILER_PRISM=yes rake test:run:bin` still has Prism-specific bintest failures around diagnostics, verbose dump output, top-level locals, and `mirb-prism` multi-line behavior.
- `mruby-bin-strip-prism` does not exist yet.
- Some non-Prism gems and gemboxes still refer directly to `mruby-compiler`.

## License

MIT License. See `LICENSE`.
