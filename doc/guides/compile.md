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

By far, preallocated symbols are highly compatible with the previous versions, so
we expect you won't see any problem with them. But just in case you face any
issue, you can disable preallocated symbols by specifying `conf.disable_presym`.

In the build process, `mrbc` under cross compiling environment will be compiled
with this configuration.

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
```

A GemBox is a set of Gems defined in `mrbgems/default.gembox` for example.
It's just a set of `mrbgem` configurations.

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
        +- lib          <- Libraries (libmruby.a and libmruby_core.a)
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
  - compile all files under `src` (object files will be stored in `build/host/mrbc/src`)
  - compile `mruby-compiler` gem
  - create `build/host/mrbc/lib/libmruby_core.a` out of all object files (C only)
  - create `build/host/mrbc/bin/mrbc` via `mruby-bin-mrbc` gem
- compile all files under `src` and store result in `build/host/src`
- create `build/host/mrblib/mrblib.c` by compiling all `*.rb` files under `mrblib` with `build/host/mrbc/bin/mrbc`
- compile `build/host/mrblib/mrblib.c` to `build/host/mrblib/mrblib.o`
- create `build/host/lib/libmruby.a` out of all object files (C and Ruby)
- compile (normal) mrbgems specified in the configuration file
- create `build/host/lib/libmruby.a` from object files from gems and `libmruby_core.a`
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
- create `build/i386/lib/libmruby_core.a` out of C object files
- create `build/i386/mrblib/mrblib.c` by compiling all `*.rb` files under `mrblib` with native `build/host/bin/mrbc`
- cross-compile `build/i386/mrblib/mrblib.c` to `build/i386/mrblib/mrblib.o`
- create `build/i386/lib/libmruby.a` from object files from gems and `libmruby_core.a`
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

By default, it contains only a proc object to exclude `libmruby_core`.

## Tips

- If you see compilation troubles, try `rake clean` first.
