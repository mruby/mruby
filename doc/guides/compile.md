<!-- summary: About the Compile -->

# Compile

mruby uses Rake to compile and cross-compile all libraries and
binaries.

## Prerequisites

To compile mruby out of the source code you need the following tools:

- C Compiler (e.g. `gcc` or `clang`)
- Linker (e.g. `gcc` or `clang`)
- Archive utility (e.g. `ar`)
- Ruby 2.5 or later (e.g. `ruby` or `jruby`)

Optional:

- Git (to update mruby source and integrate mrbgems easier)
- C++ compiler (to use mrbgems which include `*.cpp`, `*.cxx`, `*.cc`)
- Bison (to compile `mrbgems/mruby-compiler/core/parse.y`)
- gperf (to compile `mrbgems/mruby-compiler/core/keywords`)

Note that `bison` bundled with macOS is too old to compile `mruby`.
Try `brew install bison` and follow the instruction shown to update
the `$PATH` to compile `mruby`. We also encourage you to upgrade `ruby`
on macOS in similar manner.

## Build

To compile `mruby` with the default build configuration, just invoke `rake`
inside of the mruby source root. To generate and execute the test tools call
`rake test`. To clean all build files call `rake clean`. To see full command
line on build, call `rake -v`.

Every target ends its build by writing a `compile_commands.json` of its own
compiles into its build directory, for editors and clang tools, out of the
command lines it records beside its objects. No tracer such as `bear` is
involved. `rake compile_commands.json` is that same build, asked for by the
name of the file it leaves behind. A target says
`conf.disable_compile_commands` to keep none.

A database in a build directory is what `clangd --compile-commands-dir` and
its like are pointed at, so reading the tree as a cross target is one option
away. What a tool finds without being pointed anywhere is the copy at the
source root, and that one describes a single target: the one whose
configuration has a target named `host`, or failing that the first target the
configuration declares. `MRUBY_CDB_TARGET` names another for one run.

A configuration with several targets says which of them it means by declaring
that one first. Where the order is fixed for another reason,
`conf.enable_compile_commands default: true` names the target instead; no
configuration in this tree needs it.

A source no build in the tree compiled, one of a gem the configuration leaves
out for instance, has no entry; `compile_flags.txt` and `.clangd` at the
source root are what answer for those.

Every target also leaves a `size.json` in its build directory: the byte
counts of `libmruby.a` and the executables, text, data and bss sections and
all, each with the object files it is made of, so that two builds can be
subtracted down to the object that grew. The file names the commit it was
built from, and `rake size.json` is the build asked for by that name. The
`size` program is found by the C compiler's prefix, or named with
`conf.size = "arm-none-eabi-size"`; a build whose objects none can read
keeps its file sizes and carries `null` sections.

You can specify your own configuration file by the `MRUBY_CONFIG` environment
variable (you can use `CONFIG` for shorthand for `MRUBY_CONFIG`). If the path
doesn't exist, `build_config/${MRUBY_CONFIG}.rb` is used. The default
configuration is defined in the `build_config/default.rb` file.

Those build configuration files contain the build configuration of mruby, for
example:

```ruby
MRuby::Build.new do |conf|
  conf.toolchain :gcc
end
```

All tools necessary to compile mruby can be set or modified here.

## Build Configuration

We wish you submit a pull-request to `build_config/PLATFORM.rb`, once you
created a new configuration for a new platform.

Inside the configuration file, the following options can be
configured based on your environment.

### Toolchains

The mruby build system already contains a set of toolchain templates which
configure the build environment for specific compiler infrastructures.

#### GCC

Toolchain configuration for the GNU C Compiler.

```ruby
conf.toolchain :gcc
```

#### clang

Toolchain configuration for the LLVM C Compiler clang. Mainly equal to the
GCC toolchain.

```ruby
conf.toolchain :clang
```

#### Visual Studio 2010, 2012 and 2013

Toolchain configuration for Visual Studio on Windows. If you use the
[Visual Studio Command Prompt](<https://msdn.microsoft.com/en-us/library/ms229859(v=vs.110).aspx>),
you normally do not have to specify this manually, since it gets automatically detected by our build process.

```ruby
conf.toolchain :visualcpp
```

#### Android

Toolchain configuration for Android.

```ruby
conf.toolchain :android
```

Requires the custom standalone Android NDK and the toolchain path
in `ANDROID_STANDALONE_TOOLCHAIN`.

### Binaries

It is possible to select which tools should be compiled during the compilation
process. For example,

- `mruby`
- `mirb`

The configuration are done via `mrbgems`. See `Mrbgems` section.

### File Separator

Some environments require a different file separator character. It is possible to
set the character via `conf.file_separator`.

```ruby
conf.file_separator = '/'
```

### Name of library directory

In some environments, the `libmruby.a` file requires a different directory name than `lib`.
You can be changed to any name by the `conf.libdir_name` accessor.

```ruby
conf.libdir_name = 'lib64'
```

Alternatively, it can be changed via the environment variable `MRUBY_SYSTEM_LIBDIR_NAME` when
the `rake` command is run.

```console
$ export MRUBY_SYSTEM_LIBDIR_NAME=lib64
$ rake clean all
```

NOTES:

- This environment variable `MRUBY_SYSTEM_LIBDIR_NAME` does not affect `MRuby::CrossBuild`.
  In other words, if you want to change it for `MRuby::CrossBuild`, you must set it with `MRuby::CrossBuild#libdir_name=`.
- If you want to switch this environment variable `MRUBY_SYSTEM_LIBDIR_NAME`, you must do `rake clean`.

  A bad usage example is shown below.

  ```console
  $ rake clean all
  $ rake MRUBY_SYSTEM_LIBDIR_NAME=lib64 install
  ```

### C Compiler

Configuration of the C compiler binary, flags and include paths.

```ruby
conf.cc do |cc|
  cc.command = ...
  cc.flags = ...
  cc.include_paths = ...
  cc.defines = ...
  cc.option_include_path = ...
  cc.option_define = ...
  cc.compile_options = ...
end
```

C Compiler has header searcher to detect installed library.

If you need an include path of header file use `search_header_path`:

```ruby
# Searches `iconv.h`.
# If found it will return include path of the header file.
# Otherwise it will return nil.
fail 'iconv.h not found' unless conf.cc.search_header_path 'iconv.h'
```

If you need a full filename of header file use `search_header`:

```ruby
# Searches `iconv.h`.
# If found it will return full path of the header file.
# Otherwise it will return nil.
iconv_h = conf.cc.search_header 'iconv.h'
print "iconv.h found: #{iconv_h}\n"
```

Header searcher uses compiler's `include_paths` by default.
When you are using GCC toolchain (including clang toolchain since its base is gcc toolchain)
it will use compiler specific include paths too. (For example `/usr/local/include`, `/usr/include`)

If you need a special header search paths define a singleton method `header_search_paths` to C compiler:

```ruby
def conf.cc.header_search_paths
  ['/opt/local/include'] + include_paths
end
```

The header searcher answers whether a file is there, which is not the same
question as whether the compiler will accept it: it looks the name up in the
search paths, and the flags this build compiles with are no part of that
lookup. Where a build passes `-m32`, a `--sysroot`, or anything else that
moves the compiler's idea of its target, ask the compiler instead.

#### Asking the compiler

`check_header` compiles `#include <name>` with the flags this build compiles
with, and answers whether that compiled. It is how a build settles a question
the preprocessor cannot answer on its own: finding out whether a header is
there means reading it, so a `#if` guarding the `#include` runs too late to
help.

```ruby
# `<sys/resource.h>` is an XSI extension, not part of base POSIX, so a host
# either has it or does not and only the compiler knows which.
spec.build_settings do |spec|
  spec.cc.defines << 'HAVE_SYS_RESOURCE_H' if spec.cc.check_header('sys/resource.h')
end
```

`check_func` asks whether a name is declared once a header is included, or is
a macro spelled that way:

```ruby
spec.build_settings do |spec|
  spec.cc.defines << 'HAVE_GETRUSAGE' if spec.cc.check_func('getrusage', header: 'sys/resource.h')
end
```

A gem asks from a `spec.build_settings` block, as both of those do. It runs
after every gem's `mrbgem.rake` body and before the rules are defined, which
is what lets the defines an answer is turned into reach the compile.

`try_compile` takes the source itself, for a question neither of the two
spells. A build configuration asks its own compiler where it stands, having no
gem lifecycle to wait on:

```ruby
conf.cc.defines << 'HAVE_BUILTIN_CLZ' if conf.cc.try_compile(<<~SOURCE)
  int mrb_probe(void);
  int mrb_probe(void) { return __builtin_clz(1u); }
SOURCE
```

The three compile and never link, so a target with no library to link against
still answers, and a `check_func` answer is about the declaration a compile
can see rather than a symbol a link would resolve.

Each answer is kept for the life of the `rake` process, keyed by everything
that goes into the compile: the command, the option string and the source
extension it is spelled with, the flags, and the source. The extension is what
tells a compiler whether it is reading C or C++, and can be all that separates
two of a build's compilers, a toolchain being free to give them one command
and one set of flags.

An answer holds for the compiler that gave it. `rake amalgam` embeds the
defines a gem writes to its own `cc` into the generated `mruby.h`, so an
amalgam carries the answers the build that generated it got, the way it
already carries every other define a gem writes.

#### The define log

A build opens by printing every define it will compile with and the file and
line that wrote it, one table per target:

```console
Defines of 'host':
  HAVE_SYS_RESOURCE_H  mruby-process cc    mrbgems/mruby-process/mrbgem.rake:37
  MRB_DEBUG            compilers internal  build_config/host-debug.rb:5 (via enable_debug)
  MRB_USE_BIGINT       conf                mrbgems/mruby-bigint/mrbgem.rake:5
```

The middle column says who carries the define: `conf` is `conf.defines`, a
compiler name is that compiler's own list (`compilers` when every compiler
carries it, `internal` for what the build added from one of its own
switches), and a gem name is the gem's own compiler. An add made through a
switch such as `enable_debug` is charged to the configuration line that asked
for it. The mechanical `MRBGEM_*_VERSION` defines are left out.

When one name is held with two values, the losing rows are marked with what
beats them: the last `-D` of a name on a compile line is the one in effect,
and `conf.defines` comes after the compilers' lists. An unmarked row is what
objects compile with; `[FOO=1 wins]` on a row says every object gets `FOO=1`
instead, and `[FOO=1 wins for mruby-x cc]` says only that gem's objects do,
the gem's own compiler having redefined a build-wide name.

`rake defines` prints the same tables without building anything, and
`conf.disable_define_log` leaves a build out of the log.

### Linker

Configuration of the Linker binary, flags and library paths.

```ruby
conf.linker do |linker|
  linker.command = ...
  linker.flags = ...
  linker.flags_before_libraries = ...
  linker.libraries = ...
  linker.flags_after_libraries = ...
  linker.library_paths = ...
  linker.option_library = ...
  linker.option_library_path = ...
  linker.link_options = ...
end
```

### Archiver

Configuration of the Archiver binary and flags.

```ruby
conf.archiver do |archiver|
  archiver.command = ...
  archiver.archive_options = ...
end
```

### Parser Generator

Configuration of the Parser Generator binary and flags.

```ruby
conf.yacc do |yacc|
  yacc.command = ...
  yacc.compile_options = ...
end
```

### GPerf

Configuration of the GPerf binary and flags.

```ruby
conf.gperf do |gperf|
  gperf.command = ...
  gperf.compile_options = ...
end
```

### File Extensions

```ruby
conf.exts do |exts|
  exts.object = ...
  exts.executable = ...
  exts.library = ...
end
```

### Preallocated Symbols

Preallocated symbols are always enabled. Symbol IDs used in C source code
(via `MRB_SYM()` etc.) are resolved to compile-time constants during the
build process.

### Mrbgems

`mruby` comes with the (sort of) packaging system named `mrbgems`. To
specify `gem`, you can use `conf.gem` in the configuration file.

```ruby
# Integrate a bundled Gem you see in `mrbgems` directory
conf.gem :core => 'mruby-something'

# Integrate a Gem from GitHub
conf.gem :github => 'someone/mruby-another'

# Integrate a mruby binary Gem
conf.gem :core => 'mruby-bin-mruby'

# Integrate a interactive mruby binary Gem
conf.gem :core => 'mruby-bin-mirb'

# Integrate GemBox (set of Gems)
conf.gembox "default"

# ... and take one back out of it
conf.gems.delete "mruby-socket"
```

A GemBox is a set of Gems defined in `mrbgems/default.gembox` for example.
It's just a set of `mrbgem` configurations.

`conf.gems.delete` removes a Gem the configuration has already added, so a
build can say "this GemBox, minus one" without restating the box. It has to
come after the `gembox` line that brought the Gem in, and naming a Gem that
is not in the build fails, so a misspelled name does not pass for a build
that quietly keeps the Gem. `conf.gems.reject!` takes a block instead and
removes every Gem it matches, returning `nil` when it matches none.

A Gem that another Gem in the build declares as a dependency cannot be
removed this way: dependency resolution loads it again, and reports

```
gem 'mruby-string-ext' can't be removed; mruby-regexp depends on it
```

Removing the Gem that depends on it as well is what makes it go.

There is a `RubyGem` (gem for CRuby) named `mgem` that help you to
manage `mrbgems`. Try `gem install mgem`. `mgem` can show you the list
of registered `mrbgems`.

See [doc/guides/mrbgems.md](mrbgems.md) for more option about mrbgems.

### Mrbtest

Configuration Mrbtest build process.

If you want `mrbtest.a` only, You should set `conf.build_mrbtest_lib_only`

```ruby
conf.build_mrbtest_lib_only
```

### Bintest

Tests for mrbgem tools using CRuby.
To have bintests place `*.rb` scripts to `bintest/` directory of mrbgems.
See `mruby-bin-*/bintest/*.rb` if you need examples.
If you want a temporary files use `tempfile` module of CRuby instead of `/tmp/`.

You can enable it with following:

```ruby
conf.enable_bintest
```

### C++ ABI

By default, mruby uses setjmp/longjmp to implement its
exceptions. But it doesn't release C++ stack object
correctly. To support mrbgems written in C++, mruby can be
configured to use C++ exception.

There are two levels of C++ exception handling. The one is
`enable_cxx_exception` that enables C++ exception, but
uses C ABI. The other is `enable_cxx_abi` where all
files are compiled by C++ compiler.

When you mix C++ code, C++ exception would be enabled automatically.
If you need to enable C++ exception explicitly add the following:

```ruby
conf.enable_cxx_exception
```

#### C++ exception disabling

If your compiler does not support C++, and you want to ensure
you don't use mrbgem written in C++, you can explicitly disable
C++ exception, add following:

```ruby
conf.disable_cxx_exception
```

and you will get an error when you try to use C++ gem.
Note that it must be called before `enable_cxx_exception` or `gem` method.

### Debugging mode

To enable debugging mode add the following:

```ruby
conf.enable_debug
```

When debugging mode is enabled

- Macro `MRB_DEBUG` would be defined.
  - Which means `mrb_assert()` macro is enabled.
- Debug information of irep would be generated by `mrbc`.
  - Because `-g` flag would be added to `mrbc` runner.
    - You can have better backtrace of mruby scripts with this.

### File prefix map

Where the mruby tree and the build directory sit would reach what a build
compiles: `__FILE__`, which is what `mrb_assert` reports through `assert`, the
debug information that the `-g` of the `gcc` and `clang` toolchains writes, and
the file names `mrbc` records for the backtrace of an mruby script under
`enable_debug`. Every build keeps them out of it on its own.

It compiles the sources by the names they have from the tree, `src/vm.c` and
not the path of the checkout, and names the two directories they come from for
whatever a name cannot carry: the tree, written as `.`, and the build
directory, written as `build`. The build directory is written as the place it
takes when nothing moves it, so that a build with `MRUBY_BUILD_DIR` pointing
anywhere else compiles what a build inside the tree compiles. The names are
written with `-ffile-prefix-map`, except for the directory a compiler records
as the one it compiled in, which `clang` is told by `-ffile-compilation-dir`.

Two builds of the same commit in two checkouts therefore compile the same
thing, and a compiler cache keyed on the command line, `ccache` or `sccache`,
answers for one from what it learned of the other with nothing configured for
it on the machine.

To write the two names yourself:

```ruby
conf.enable_file_prefix_map source: "mruby", build: "mruby/build"
```

A name other than `.` for the tree is one no name from the tree carries, so
such a build compiles with the paths as they are and writes the names through
the map alone. A cache has nothing to carry from one checkout to another
there, and a debugger looks for the sources under the name that was asked for.

Any other directory is mapped one at a time, which is how the path of a
toolchain or of a gem outside the tree is written:

```ruby
conf.file_prefix_map "/opt/toolchain", "toolchain"
```

To compile with the paths as they are:

```ruby
conf.disable_file_prefix_map
```

which is what a build to be debugged from outside the mruby tree wants: a
debugger looks for the sources under the names the build wrote, and finds them
only from the tree they are named against. Either tell the debugger where they
are (`set substitute-path . /path/to/mruby` in gdb), run it from the tree, or
take the names off this way.

Note that

- A compiler that takes neither option, which `cl` and every compiler older
  than GCC 8 or clang 10 are, still compiles by the names of the tree, and the
  directory it records as the one it compiled in stays as it is. The build asks
  the compiler before it writes either option and leaves it out where the
  answer is no.
- The flags a build exports in `libmruby.flags.mak` name every directory in
  full: they are read where the package was installed, which is not where it
  was built, and whoever compiles against it rewrites them.
- A directory the build cannot name from the tree, a gem or a build directory
  somewhere else, reaches the compiler as this machine spells it, and is
  written through the map.

## Cross-Compilation

mruby can also be cross-compiled from one platform to another. To achieve
cross-compilation, the build configuration needs to contain an instance of
`MRuby::CrossBuild`. This instance defines the compilation tools and flags
for the target platform. An example could look like this:

```ruby
MRuby::CrossBuild.new('32bit') do |conf|
  conf.toolchain :gcc

  conf.cc.flags << "-m32"
  conf.linker.flags << "-m32"
end
```

All configuration options of `MRuby::Build` can also be used in
`MRuby::CrossBuild`. You can find examples under the `build_config`
directory.

### Mrbtest in Cross-Compilation

In cross compilation, you can run `mrbtest` on an emulator if
you have it by changing configuration of test runner.

```ruby
conf.test_runner do |t|
  t.command = ... # set emulator. this value must be non nil or false
  t.flags = ... # set flags of emulator

  def t.run(bin) # override `run` if you need to change the behavior of it
    ... # `bin` is the full path of mrbtest
  end
end
```

## Build process

During the build process the `build` directory will be created in the
root directory. The structure of this directory will look like this:

```
+- build
    |
    +- host
        |
        +- LEGAL        <- License description
        |
        +- bin          <- Binaries (mirb, mrbc and mruby)
        |
        +- lib          <- Libraries (libmruby.a)
        |
        +- mrbc         <- Minimal mrbc place
        |
        +- mrbgems      <- Compilation result from mrbgems
        |
        +- mrblib       <- Compilation result from mrblib
        |
        +- src          <- Compilation result from C sources
```

The compilation workflow will look like this:

- compile minimal `mrbc` from `src` and `mrblib` sources
  - compile `mruby-compiler` gem
  - create `build/host/mrbc/bin/mrbc` via `mruby-bin-mrbc` gem
- compile all files under `src` and store result in `build/host/src`
- create `build/host/mrblib/mrblib.c` by compiling all `*.rb` files under `mrblib` with `build/host/mrbc/bin/mrbc`
- compile `build/host/mrblib/mrblib.c` to `build/host/mrblib/mrblib.o`
- create `build/host/lib/libmruby.a` out of all object files (C and Ruby)
- compile (normal) mrbgems specified in the configuration file
- create `build/host/lib/libmruby.a` from object files from gems and from `src`
- create binary commands according to binary gems (e.g. `mirb` and `mruby`)
- copy binaries under `build/host/bin` to `bin` directory

```
 _____    _____    ______    ____    ____    _____    _____    ____
| CC  |->|GEN  |->|AR    |->|CC  |->|CC  |->|AR   |->|CC   |->|CC  |
| *.c |  |y.tab|  |core.a|  |mrbc|  |*.rb|  |lib.a|  |mruby|  |mirb|
 -----    -----    ------    ----    ----    -----    -----    ----
```

### Cross-Compilation

In case of a cross-compilation to `i386` the `build` directory structure looks
like this:

```
+- build
    |
    +- host
    |   |
    |   +- bin           <- Native Binaries
    |   |
    |   +- lib           <- Native Libraries
    |   |
    |   +- mrbgems
    |   |
    |   +- src
    |
    +- i386
        |
        +- bin            <- Cross-compiled Binaries
        |
        +- include        <- Header Directory
        |
        +- lib            <- Cross-compiled Libraries
        |
        +- mrbgems
        |
        +- mrblib
        |
        +- src
```

An extra directory is created for the target platform. In case you
compile for `i386` a directory called `i386` is created under the
build directory.

The cross compilation workflow starts in the same way as the normal
compilation by compiling all _native_ libraries and binaries, except
for we don't have `host/mrbc` directory (`host` directory itself works
as placeholder for `mrbc`). Afterwards the cross compilation process
proceeds like this:

- cross-compile all files under `src` and store result in `build/i386/src`
- create `build/i386/mrblib/mrblib.c` by compiling all `*.rb` files under `mrblib` with native `build/host/bin/mrbc`
- cross-compile `build/i386/mrblib/mrblib.c` to `build/i386/mrblib/mrblib.o`
- create `build/i386/lib/libmruby.a` from object files from gems and from `src`
- create binary commands according to binary gems (e.g. `mirb` and `mruby`)
- copy binaries under `build/host/bin` to `bin` directory

```
 _______________________________________________________________
|              Native Compilation for Host System               |
|  _____      ______      _____      ____      ____      _____  |
| | CC  | -> |AR    | -> |GEN  | -> |CC  | -> |CC  | -> |AR   | |
| | *.c |    |core.a|    |y.tab|    |mrbc|    |*.rb|    |lib.a| |
|  -----      ------      -----      ----      ----      -----  |
 ---------------------------------------------------------------
                                ||
                               \||/
                                \/
 ________________________________________________________________
|             Cross Compilation for Target System                |
|  _____      _____      _____      ____      ______      _____  |
| | CC  | -> |AR   | -> |CC   | -> |CC  | -> |AR    | -> |CC   | |
| | *.c |    |lib.a|    |mruby|    |mirb|    |core.a|    |mrbc | |
|  -----      -----      -----      ----      ------      -----  |
 ----------------------------------------------------------------
```

## Build Configuration Examples

### Minimal Library

To build a minimal mruby library you need to use the Cross Compiling
feature due to the reason that there are functions (e.g. stdio) which
can't be disabled for the main build.

```ruby
MRuby::CrossBuild.new('minimal') do |conf|
  conf.toolchain :gcc
  conf.cc.defines << 'MRB_NO_STDIO'
end
```

This configuration defines a cross compile build called 'minimal' which
is using the GCC and compiles for the host machine. It also disables
all usages of stdio and doesn't compile any binaries (e.g. `mrbc`).

## Test Environment

mruby's build process includes a test environment. In case you start the testing
of mruby, a native binary called `mrbtest` will be generated and executed.
This binary contains all test cases which are defined under `test/t`. In case
of a cross-compilation an additional cross-compiled `mrbtest` binary is
generated. You can copy this binary and run on your target system.

## Embedding `mruby` in Your Application

After the build, you will get `libmruby.a`. You can link it to your application.

For compiler options and library path, you can use `mruby-config` command for
convenience. `mruby-config` command prints the configuration used for `libmruby.a`.

```console
$ mruby-config --help
Usage: mruby-config [switches]
  switches:
  --cc                        print compiler name
  --cflags                    print flags passed to compiler
  --ld                        print linker name
  --ldflags                   print flags passed to linker
  --ldflags-before-libs       print flags passed to linker before linked libraries
  --libs                      print linked libraries
  --libmruby-path             print libmruby path
  --help                      print this help
```

For example, when you have a C source file (`c.c`) and try to
compile and link it with `libmruby.a`, you can run the following command,

```
`mruby-config --cc --cflags` c.c `mruby-config --ldflags --libs`
```

When you use `make`, add following lines in `Makefile`

```
MRB_CONFIG = <path-to-mruby-config>
CFLAGS = `$(MRB_CONFIG) --cflags`
LDFLAGS = `$(MRB_CONFIG) --ldflags`
LIBS = `$(MRB_CONFIG) --libs`
```

## Install

To install the files in the `bin`, `include` and `lib` directories generated by the "host" build target into a system directory, do the following:

```console
$ rake install
```

If there are multiple build targets in the build configuration file, to install the products of all build targets, do the following:

```console
$ rake install:full
```

To install only one of several build targets, e.g., the "its-mine" build target, do the following:

```console
$ rake install:full:its-mine
```

To install only the executable files, do the following:

```console
$ rake install_bin              # only "host" build target
$ rake install:bin              # all build targets
$ rake install:bin:its-mine     # only "its-mine" build target
```

### Installation Directory

The installation directory is `/usr/local` for the "host" build target and `/usr/local/mruby/<build-name>` for the others.
To change them, you can set the environment variable `PREFIX` or use `MRuby::Build#install_prefix = dir` in your build configuration file.

The `PREFIX` environment variable affects all build targets and changes the `/usr/local` part.

The `MRuby::Build#install_prefix` can be set for each individual build target.
In this case, the environment variable `PREFIX` is ignored.

Also, if the environment variable `DESTDIR` is set, it will prepend to the path obtained by `install_prefix` to determine the final write directory.
This is intended for temporary file expansion by the user's package work.

---

To summarize:

- The default value of the environment variable `PREFIX` is `/usr/local`.
- For the "host" build target, the default value of `MRuby::Build#install_prefix` is `<PREFIX>`.
- For a build target other than "host", the default value of `MRuby::Build#install_prefix` is `<PREFIX>/mruby/<build-name>`.
- If the environment variable `DESTDIR` is set, the actual write directory is `<DESTDIR>/<MRuby::Build#install_prefix>`.

### Excluded files

In some cases there are files that you do not want to install.
In such cases, add a file path filter to the array object `MRuby::Build#install_excludes` to exclude them.

The following is an object that can be defined as a file path filter.
The `path` variable that appears is a relative path based on `MRuby::Build#build_dir`.

- string objects: files matched by `string.match?(path)` are excluded.
- regexp object: files matched by `regexp.match?(path)` are excluded.
- proc object: files which return true with `proc.call(path)` are excluded.

```ruby
# exclude bin/mrbc
conf.install_excludes << exefile("bin/mrbc")

# exclude all files under lib/ directory
conf.install_excludes << %r(^lib/)

# exclude bin/mrbtest, but in this case it is recommended to use string instead of proc
conf.install_excludes << proc { |path|
  path == exefile("bin/mrbtest")
}
```

## Tips

- If you see compilation troubles, try `rake clean` first.
