# User visible changes in `mruby3.3` from `mruby3.2`

# The language

- aliases work properly with `super`
- `callee` method work differently with aliases in mruby
- define `Kernel#respond_to_missing?` method
- `_inspect` method (`inspect` with recursive check) is removed
- `__printstr` method is removed; use `print` instead

# Configuration

- mruby can be built using Docker now. Try `docker-compose build` for example.
- New Platform: Nintendo Wii
- Improved Platforms: Android, Dreamcast

# mruby memory API

- `mrb_default_allocf` can be overridden by the application
- `mrb_open_allocf` will be deprecated

# mruby VM and bytecode

- [#5870](https://github.com/mruby/mruby/issues/5870) Use OP_LOADINEG instead of OP_LOADI

# Changes in mrbgems

- mruby-bin-config: new options --cxx,--cxxflags,--as,--asflags,--objc,--objcflags
- mruby-binding-eval (merged with mruby-eval) #5989
- mruby-binding: renamed from mruby-binding-core
- mruby-enumerator: remove internal attribute methods obj, args, kwd, meth, fib.
- mruby-fiber: Add a new mrb_fiber_new() with MRB_API
- mruby-fiber: Allows calling `Fiber#resume` from C
- mruby-fiber: `Fiber#to_s` format changed
- mruby-io: Add "x" mode option for IO.open
- mruby-method: `Method#to_s` format changed
- mruby-pack: support new directives j,J,b,B
- mruby-range-ext: new method `Range#overlap?`
- mruby-string-ext: Add `String#valid_encoding?` method

# Bugs Fixed

- [#5712](https://github.com/mruby/mruby/issues/5712) No "make install"
- [#5724](https://github.com/mruby/mruby/issues/5724) Rational#\*\* is missing
- [#5725](https://github.com/mruby/mruby/issues/5725) weird const_missing exceptions in mrblib code
- [#5789](https://github.com/mruby/mruby/issues/5789) No memory release of backtrace information due to stack error
- [#5943](https://github.com/mruby/mruby/issues/5943) TCPSocket#write is failed
- [#5944](https://github.com/mruby/mruby/issues/5944) Behavior of calling method with a hash variable
- [#5949](https://github.com/mruby/mruby/issues/5949) Caller appears to report wrong line when block passed and brackets omitted
- [#5974](https://github.com/mruby/mruby/issues/5974) Invalid escape sequences in gem_init.c on windows
- [#5975](https://github.com/mruby/mruby/issues/5975) Equals comparison fails on extreme ends of 64-bit integers
- [#5985](https://github.com/mruby/mruby/issues/5985) Sign extension with OP_LOADI32 in get_int_operand()
- [#5986](https://github.com/mruby/mruby/issues/5986) Fix bugs in String#bytesplice
- [#5987](https://github.com/mruby/mruby/issues/5987) ~(-1 << 64) is incorrect
- [#5991](https://github.com/mruby/mruby/issues/5991) 'gets' method not working in mruby-3.2.0
- [#5995](https://github.com/mruby/mruby/issues/5995) One seemingly unnecessary parameter is passed in the block parameters
- [#6029](https://github.com/mruby/mruby/issues/6029) mruby build fails under mrbgems directory
- [#6041](https://github.com/mruby/mruby/issues/6041) GC Performance may have degraded
- [#6051](https://github.com/mruby/mruby/issues/6051) Null pointer dereference in mrb_addrinfo_unix_path
- [#6052](https://github.com/mruby/mruby/issues/6052) Null pointer dereference while handling the Proc class
- [#6065](https://github.com/mruby/mruby/issues/6065) Null pointer dereference while handling the Proc class
- [#6066](https://github.com/mruby/mruby/issues/6066) Null pointer dereference involving Struct.new()
- [#6067](https://github.com/mruby/mruby/issues/6067) Null pointer dereference in mrb_string_value_cstr
- [#6068](https://github.com/mruby/mruby/issues/6068) Stack overflow in mrb_vm_exec
- [#6087](https://github.com/mruby/mruby/issues/6087) 'Remote branch HEAD not found in upstream origin' error on build
- [#6089](https://github.com/mruby/mruby/issues/6089) binding.eval() handles def expressions differently from CRuby
- [#6098](https://github.com/mruby/mruby/issues/6098) Fails to call superclass of wrapped method
- [#6108](https://github.com/mruby/mruby/issues/6108) VM crashes with break
- [#6134](https://github.com/mruby/mruby/issues/6134) String#unpack1 returns an array instead of a single string

# Pull Requests (User Visible Ones)

- [#5870](https://github.com/mruby/mruby/pull/5870) Use OP_LOADINEG instead of OP_LOADI
- [#5966](https://github.com/mruby/mruby/pull/5966) Update default.gembox add mruby debugger mrdb
- [#5979](https://github.com/mruby/mruby/pull/5979) Allow Class#allocate to be prohibited
- [#5989](https://github.com/mruby/mruby/pull/5989) Integrate mruby-binding-eval into mruby-eval
- [#5961](https://github.com/mruby/mruby/pull/5961) Add Docker to build and run all mruby tests. Run pre-commit and generate YARD docs with Docker
- [#6008](https://github.com/mruby/mruby/pull/6008) Make "bintest" independent of directory
- [#6009](https://github.com/mruby/mruby/pull/6009) Avoid adding <MRUBY_ROOT>/bintest which does not exist
- [#6012](https://github.com/mruby/mruby/pull/6012) Allow tests to be disabled for specific gems; warn about disabled tests
- [#6013](https://github.com/mruby/mruby/pull/6013) Fix Android toolchain
- [#6032](https://github.com/mruby/mruby/pull/6032) Rake: update task clean to remove bin and build folders
- [#6045](https://github.com/mruby/mruby/pull/6045) Fixes escape sequence bug and enhancements in Presym scanning
- [#6076](https://github.com/mruby/mruby/pull/6076) Fixed unwinding block that could point to invalid PC
- [#6081](https://github.com/mruby/mruby/pull/6081) Add "x" mode option for IO.open
- [#6086](https://github.com/mruby/mruby/pull/6086) Add build config for Nintendo Wii
- [#6097](https://github.com/mruby/mruby/pull/6097) Add a new mrb_fiber_new() with MRB_API
- [#6106](https://github.com/mruby/mruby/pull/6106) Ease fiber limitations
- [#6118](https://github.com/mruby/mruby/pull/6118) Fixed IO#read with buf
- [#6120](https://github.com/mruby/mruby/pull/6120) Set EBADF if check_file_descriptor() fails
