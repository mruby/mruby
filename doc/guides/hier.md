<!-- summary: About the mruby directory structure -->

# The mruby directory structure

```text
+- 💎 mruby/                    The top directory of mruby.
    |
    +- 📁 .github/              GitHub configuration files for mruby project management.
    |
    +- 📁 benchmark/            Benchmarking files for mruby.
    |
    +- 📁 bin/                  Links to temporary executables after build. Auto-created.
    |
    +- 📁 build/                Default build output destination for mruby. Auto-created.
    |   |
    |   +- 📁 repos/            The git clone destination directory for GEMs that depend on the build configuration.
    |   |
    |   +- 📁 host/             The "host" build output directory.
    |
    +- 📁 build_config/         Build configuration files for various environments.
    |
    +- 📁 doc/                  Documentation for mruby.
    |   |
    |   +- 📁 guides/           Documentation for general users.
    |   |
    |   +- 📁 internal/         Documentation for internal implementations for developers.
    |
    +- 📁 examples/             Examples of mruby usages.
    |   |
    |   +- 📁 mrbgems/          Examples for creating custom GEM for mruby.
    |
    +- 📁 include/              C header files required when using mruby.
    |
    +- 📁 lib/                  Ruby scripts used for building mruby.
    |
    +- 📁 mrbgems/              A library collection of features not provided by mruby core only.
    |   |                       See doc/guides/mrbgems.md file
    |   |
    |   +- 📁 mruby-*/          The directory of each GEMs.
    |   |
    |   +- 📃 *.gembox          A collection of GEMs grouped by features and purposes.
    |
    +- 📁 mrblib/               The core Ruby scripts that makes up the main body of mruby.
    |
    +- 📁 oss-fuzz/             Source code for The fuzzing-test.
    |                           See https://github.com/google/oss-fuzz
    |
    +- 📁 src/                  The core C source code that makes up the main body of mruby.
    |
    +- 📁 tasks/                Rake tasks at build-time.
    |   |
    |   +- 📁 toolchains/       Definitions for the compiler, linker, archiver, etc. for each toolchain.
    |
    +- 📁 test/                 Ruby scripts needed for testing mruby.
    |   |
    |   +- 📁 t/                mruby test cases.
    |
    +- 📁 tools/                External programs used for the mruby project.
        |
        +- 📁 lrama/            LALR parser generator as an alternative to bison.
                                Import from https://github.com/ruby/lrama
```
