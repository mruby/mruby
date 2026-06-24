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

`lib/prism` is a git submodule pointing at `ruby/prism`.

The gem bootstrap runs before `mrbgem.rake` collects Prism C sources with `Dir.glob`. This matters for fresh checkouts, where the submodule may not yet be initialized and generated Prism files may be absent.

During normal build setup, `mrbgem.rake` does the following:

1. If `lib/prism/templates/template.rb` is missing, run:

   ```sh
   git submodule update --init lib/prism
   ```

2. If generated Prism files such as `src/node.c`, `src/serialize.c`, `include/prism/ast.h`, or `include/prism/diagnostic.h` are missing, run:

   ```sh
   ruby templates/template.rb
   ```

This bootstrap belongs in the compiler gem because the same source tree is used from mruby, PicoRuby, FemtoRuby, and the standalone mirror.

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
