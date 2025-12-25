# User visible changes in `mruby3.5` from `mruby3.4`

"**_NOTE_**:" are changes to be aware of.

# The language

## Pattern Matching

mruby now supports pattern matching (case/in) syntax:

- Basic pattern matching with `case`/`in` syntax ([dadfac6](https://github.com/mruby/mruby/commit/dadfac6))
- Array pattern matching ([ec67fd9](https://github.com/mruby/mruby/commit/ec67fd9))
- Hash pattern matching ([2147263](https://github.com/mruby/mruby/commit/2147263))
- Find pattern matching (`[*pre, target, *post]`) ([6c4d98b](https://github.com/mruby/mruby/commit/6c4d98b))
- Pin operator (`^variable`) ([1de6340](https://github.com/mruby/mruby/commit/1de6340))
- Guard clauses (`if`/`unless` conditions) ([07ac110](https://github.com/mruby/mruby/commit/07ac110))
- One-line pattern matching (`expr in pattern`) ([e76ce24](https://github.com/mruby/mruby/commit/e76ce24))
- Brace-less hash pattern support ([e8096bf](https://github.com/mruby/mruby/commit/e8096bf))

## Other Language Changes

- Array/Hash/String subclasses can now override `[]` and `[]=` methods ([#6675](https://github.com/mruby/mruby/pull/6675))
- `OP_SETIDX` optimization for Array and Hash ([ddd8fe1](https://github.com/mruby/mruby/commit/ddd8fe1))

# Changes in C API

- **_NOTE_**: `mruby/ext/io.h` renamed to `mruby/io.h` ([2813f79](https://github.com/mruby/mruby/commit/2813f79))
- `mrb_time_get_tm()` API for accessing struct tm ([daaaafe](https://github.com/mruby/mruby/commit/daaaafe))
- `MRB_OPEN_FAILURE()` macro for checking mrb_open result ([40b0cb9](https://github.com/mruby/mruby/commit/40b0cb9))
- `mrb_print_error()` now handles NULL gracefully ([8e50a45](https://github.com/mruby/mruby/commit/8e50a45))
- `mrb_open()` returns mrb_state with exc set on init failure ([05ffe0c](https://github.com/mruby/mruby/commit/05ffe0c))
- `mrb_utf8_to_buf()` for UTF-8 encoding consolidation ([7e28e68](https://github.com/mruby/mruby/commit/7e28e68))
- `kh_is_end()` macro for safe khash iteration ([893cc75](https://github.com/mruby/mruby/commit/893cc75))

# Build & Configuration

- New Platform: Cosmopolitan Libc ([#6681](https://github.com/mruby/mruby/pull/6681))
- Emscripten: use native WASM exception handling ([ca364e3](https://github.com/mruby/mruby/commit/ca364e3))
- HAL (Hardware Abstraction Layer) for platform abstraction in mruby-io, mruby-socket, mruby-dir, mruby-task ([74ca22f](https://github.com/mruby/mruby/commit/74ca22f))
- `MRUBY_MIRB_READLINE` environment variable to control readline library selection ([0aafb83](https://github.com/mruby/mruby/commit/0aafb83))
- Inter-gem headers separated from external API headers ([#6671](https://github.com/mruby/mruby/pull/6671))

# Changes in mrbgems

## New Gems

- **mruby-task**: Cooperative multitasking with preemptive scheduling ([ae0d7a0](https://github.com/mruby/mruby/commit/ae0d7a0))
- **mruby-benchmark**: Benchmarking gem ([2f40f3d](https://github.com/mruby/mruby/commit/2f40f3d))
- **mruby-strftime**: Time#strftime implementation ([b31e22f](https://github.com/mruby/mruby/commit/b31e22f))

## mruby-bin-mirb Improvements

- Custom multi-line editor replacing readline ([527018c](https://github.com/mruby/mruby/commit/527018c))
- Tab completion support ([2f15282](https://github.com/mruby/mruby/commit/2f15282))
- Colored output for prompts and errors ([b36e0b4](https://github.com/mruby/mruby/commit/b36e0b4))
- Auto-indentation for continuation lines ([d52f318](https://github.com/mruby/mruby/commit/d52f318))
- TAB auto-indentation ([fa74186](https://github.com/mruby/mruby/commit/fa74186))
- Auto-dedent for `else`, `elsif`, `when`, `rescue`, `ensure`, `in` ([e901b6d](https://github.com/mruby/mruby/commit/e901b6d), [814b0cc](https://github.com/mruby/mruby/commit/814b0cc))
- Command history with Up/Down navigation ([5f85c1b](https://github.com/mruby/mruby/commit/5f85c1b))
- Line numbers in multi-line prompts ([5a3f0e2](https://github.com/mruby/mruby/commit/5a3f0e2))
- Enhanced syntax error display ([ac39b7d](https://github.com/mruby/mruby/commit/ac39b7d))

## Other Gem Changes

- **mruby-array-ext**: Add `Array#find` and `Array#rfind` methods
- **mruby-io**: Add `IO#putc` and `Kernel#putc` ([baff6e6](https://github.com/mruby/mruby/commit/baff6e6))
- **mruby-random**: Replace xoshiro with PCG for better memory efficiency ([f1bab01](https://github.com/mruby/mruby/commit/f1bab01))
- **mruby-compiler**: Variable-sized AST nodes for reduced memory usage
- **mruby-compiler**: `no_return_value` context flag for script optimization ([613b03a](https://github.com/mruby/mruby/commit/613b03a))

# Compiler Improvements

- Variable-sized AST nodes for reduced memory consumption ([821b989](https://github.com/mruby/mruby/commit/821b989))
- Pattern matching bytecode optimizations ([21d4135](https://github.com/mruby/mruby/commit/21d4135))
- Optimized masgn to generate literals directly into target registers ([fb5d966](https://github.com/mruby/mruby/commit/fb5d966))
- Optimized splat of literal arrays in args/literals ([1cb8d73](https://github.com/mruby/mruby/commit/1cb8d73))

# Fixed GitHub Issues

- [#6675](https://github.com/mruby/mruby/issues/6675) Allow Hash#[] to be aliased again
- [#6674](https://github.com/mruby/mruby/issues/6674) Document pattern matching limitations
- [#6671](https://github.com/mruby/mruby/issues/6671) Separate inter-gem headers from external API headers
- [#6668](https://github.com/mruby/mruby/issues/6668) Method#== for aliased methods and comparison bug
- [#6666](https://github.com/mruby/mruby/issues/6666) Regexp literal with encoding
- [#6664](https://github.com/mruby/mruby/issues/6664) Set#flatten memory leak
- [#6662](https://github.com/mruby/mruby/issues/6662) Array set operations use-after-free
- [#6660](https://github.com/mruby/mruby/issues/6660) Regression on struct/array/hash == override with super
- [#6659](https://github.com/mruby/mruby/issues/6659) Super with keyword arguments
- [#6657](https://github.com/mruby/mruby/issues/6657) Exception handling for ||= on class variables
- [#6652](https://github.com/mruby/mruby/issues/6652) Array comparison use-after-realloc
- [#6650](https://github.com/mruby/mruby/issues/6650) Array#fill validation
- [#6649](https://github.com/mruby/mruby/issues/6649) Array#sort! use-after-realloc
- [#6648](https://github.com/mruby/mruby/issues/6648) sprintf buffer overread
- [#6647](https://github.com/mruby/mruby/issues/6647) IO#ungetc buffer overflow
- [#6646](https://github.com/mruby/mruby/issues/6646) IO#gets negative length
- [#6645](https://github.com/mruby/mruby/issues/6645) Set memory leak from double initialization
- [#6642](https://github.com/mruby/mruby/issues/6642) Task segfault when sleep called from C
- [#6637](https://github.com/mruby/mruby/issues/6637) arm64 mingw64 builtin setjmp/longjmp
- [#6635](https://github.com/mruby/mruby/issues/6635), [#6636](https://github.com/mruby/mruby/issues/6636) Colon3 constant lookup
- [#6613](https://github.com/mruby/mruby/issues/6613) const_added hook during bootstrapping
- [#6570](https://github.com/mruby/mruby/issues/6570) instance_eval crash
- [#6554](https://github.com/mruby/mruby/issues/6554) Socket private #initialize
- [#6516](https://github.com/mruby/mruby/issues/6516) Private backquote
- [#6515](https://github.com/mruby/mruby/issues/6515) y.tab.c in repository
- [#6509](https://github.com/mruby/mruby/issues/6509) scope_new nregs initialization
- [#6508](https://github.com/mruby/mruby/issues/6508) Enumerable#sum index
- [#6507](https://github.com/mruby/mruby/issues/6507) tally multi-values
- [#6506](https://github.com/mruby/mruby/issues/6506) Constant lookup in singleton class
- [#5531](https://github.com/mruby/mruby/issues/5531) Hash recursion detection

# Merged Pull Requests

- [#6418](https://github.com/mruby/mruby/pull/6418) Add `ls-lint` with GitHub Actions
- [#6492](https://github.com/mruby/mruby/pull/6492) fix a typo, update specs
- [#6493](https://github.com/mruby/mruby/pull/6493) Fix TYPO in memory.md
- [#6495](https://github.com/mruby/mruby/pull/6495) Remove `MRB_ENDIAN_LOHI()` that is no longer in use
- [#6497](https://github.com/mruby/mruby/pull/6497) gha: update `build.yml` try `windows-2025` image
- [#6498](https://github.com/mruby/mruby/pull/6498) Clean up and standardize the pre-commit config
- [#6501](https://github.com/mruby/mruby/pull/6501) Update pre-commit Node.js version to `v22.14.0 LTS`
- [#6502](https://github.com/mruby/mruby/pull/6502) pre-commit: update prettier to the latest version
- [#6503](https://github.com/mruby/mruby/pull/6503) misc: fix typos
- [#6505](https://github.com/mruby/mruby/pull/6505) mrbgems: fix spelling
- [#6510](https://github.com/mruby/mruby/pull/6510) Fixed class method visibility via `module_function`
- [#6511](https://github.com/mruby/mruby/pull/6511) Exclude the external project "lrama" from pre-commit
- [#6513](https://github.com/mruby/mruby/pull/6513) mruby 3.4.0 released
- [#6517](https://github.com/mruby/mruby/pull/6517) core/codegen.c: remove unneeded duplicate semicolon
- [#6518](https://github.com/mruby/mruby/pull/6518) Change mrbc_args.flags bit width from 2 to 3
- [#6519](https://github.com/mruby/mruby/pull/6519) Add `tools/lrama` to `.prettierignore`
- [#6520](https://github.com/mruby/mruby/pull/6520) pre-commit: autoupdate and update node LTS version
- [#6521](https://github.com/mruby/mruby/pull/6521) Add codespell config file `.codespellrc`
- [#6522](https://github.com/mruby/mruby/pull/6522) gha: label more files
- [#6523](https://github.com/mruby/mruby/pull/6523) add `rand(Range)` and unify implementations of `Random#rand` and `Kernel#rand`
- [#6524](https://github.com/mruby/mruby/pull/6524) Fix Kernel#p when no argument
- [#6525](https://github.com/mruby/mruby/pull/6525) Skip adding empty input to mirb history
- [#6526](https://github.com/mruby/mruby/pull/6526) Add build config for Luckfox Pico embedded SBC
- [#6528](https://github.com/mruby/mruby/pull/6528) misc: fix spelling
- [#6530](https://github.com/mruby/mruby/pull/6530) Revert "class.c (find_visibility_scope): when callinfo returns, *ep == NULL; #6512"
- [#6531](https://github.com/mruby/mruby/pull/6531) Improve method table performance by rehashing at 75% load factor
- [#6532](https://github.com/mruby/mruby/pull/6532) Reverted method table optimizations to prioritize memory savings
- [#6533](https://github.com/mruby/mruby/pull/6533) Fix calling `extended` callback
- [#6534](https://github.com/mruby/mruby/pull/6534) Add descriptive comment to mrb_read_float function
- [#6535](https://github.com/mruby/mruby/pull/6535) Added descriptive comments for functions/macros in src/mempool.c
- [#6536](https://github.com/mruby/mruby/pull/6536) Add descriptive comments to public functions in src/debug.c
- [#6537](https://github.com/mruby/mruby/pull/6537) Updated comments in `cdump.c` to remove the `@brief` tag
- [#6539](https://github.com/mruby/mruby/pull/6539) Add descriptive comments for functions in src/load.c
- [#6540](https://github.com/mruby/mruby/pull/6540) Add descriptive comments to MRB_API functions in object.c
- [#6541](https://github.com/mruby/mruby/pull/6541) Add descriptive comments for MRB_API functions in src/array.c
- [#6542](https://github.com/mruby/mruby/pull/6542) Add descriptive comments to MRB_API functions in src/symbol.c
- [#6543](https://github.com/mruby/mruby/pull/6543) Add descriptive comments to several functions in src/dump.c
- [#6544](https://github.com/mruby/mruby/pull/6544) Fix build strings that must be mutable
- [#6545](https://github.com/mruby/mruby/pull/6545) Add descriptive comments for MRB_API functions in src/class.c
- [#6548](https://github.com/mruby/mruby/pull/6548) Add descriptive comments to MRB_API functions in src/etc.c
- [#6549](https://github.com/mruby/mruby/pull/6549) Add descriptive comments to kernel functions
- [#6550](https://github.com/mruby/mruby/pull/6550) Add descriptive comments for MRB_API functions in src/proc.c
- [#6551](https://github.com/mruby/mruby/pull/6551) Add descriptive comments for MRB_API functions in src/state.c
- [#6552](https://github.com/mruby/mruby/pull/6552) Fix: Correct placement of comments in src/variable.c
- [#6553](https://github.com/mruby/mruby/pull/6553) Add descriptive comments for MRB_API functions in src/vm.c
- [#6555](https://github.com/mruby/mruby/pull/6555) `mrb_mt_foreach()` needs to update the pointer at each loop
- [#6556](https://github.com/mruby/mruby/pull/6556) `iv_foreach()` needs to update the pointer at each loop
- [#6560](https://github.com/mruby/mruby/pull/6560) Refactor: Improve Set GC marking and freeing
- [#6561](https://github.com/mruby/mruby/pull/6561) pre-commit updates and fix prettier entrypoint
- [#6562](https://github.com/mruby/mruby/pull/6562) misc: fix spelling word case
- [#6563](https://github.com/mruby/mruby/pull/6563) pre-commit add rubocop with one rule spaces for indentation
- [#6564](https://github.com/mruby/mruby/pull/6564) Remove jumanjihouse pre-commit hooks no longer maintained
- [#6565](https://github.com/mruby/mruby/pull/6565) Rubocop: fix target Ruby version; add two more cops; fix lint error
- [#6566](https://github.com/mruby/mruby/pull/6566) Removed unreferenced variables in `CrossBuild#run_bintest`
- [#6567](https://github.com/mruby/mruby/pull/6567) Avoid array object creation in `cmd_bin` method in bintest
- [#6568](https://github.com/mruby/mruby/pull/6568) mruby-bin-debugger depends on mruby-bin-mrbc in bintest
- [#6569](https://github.com/mruby/mruby/pull/6569) sed s/Mruby/MRuby/g
- [#6571](https://github.com/mruby/mruby/pull/6571) Update limitations.md to add behavior on small hash
- [#6572](https://github.com/mruby/mruby/pull/6572) Add Claude Code GitHub Workflow
- [#6573](https://github.com/mruby/mruby/pull/6573) pre-commit fixes and updates
- [#6574](https://github.com/mruby/mruby/pull/6574) Support initializing structs via keyword arguments
- [#6575](https://github.com/mruby/mruby/pull/6575) Fix typo in file time methods
- [#6581](https://github.com/mruby/mruby/pull/6581) Merge `mrb_obj_iv_inspect()` into `mrb_obj_inspect()`
- [#6582](https://github.com/mruby/mruby/pull/6582) Stricter type tag in `mrb_obj_alloc()`
- [#6583](https://github.com/mruby/mruby/pull/6583) Add fallback to local build_config.rb before using default configuration
- [#6585](https://github.com/mruby/mruby/pull/6585) Fix typo in mruby3.2 docs
- [#6586](https://github.com/mruby/mruby/pull/6586) Makefile: refactor add docs and add command line `help` target
- [#6587](https://github.com/mruby/mruby/pull/6587) Add Set#hash tests
- [#6588](https://github.com/mruby/mruby/pull/6588) Add CodeQL Analysis for GitHub Actions
- [#6589](https://github.com/mruby/mruby/pull/6589) Add pre-commit hook `check-zip-file-is-not-committed`
- [#6591](https://github.com/mruby/mruby/pull/6591) mruby-eval fix license link in README
- [#6593](https://github.com/mruby/mruby/pull/6593) README: Add Contributors Avatars, Star History, Table of Contents
- [#6599](https://github.com/mruby/mruby/pull/6599) pre-commit: run `markdown-link-check`, `oxipng`, `prettier` manually
- [#6600](https://github.com/mruby/mruby/pull/6600) `dreamcast_shelf build config`: update to use KallistiOS wrappers
- [#6601](https://github.com/mruby/mruby/pull/6601) fix: skip local build_config.rb when working in MRUBY_ROOT
- [#6602](https://github.com/mruby/mruby/pull/6602) Improved iseq annotations for `new` and `!=`
- [#6604](https://github.com/mruby/mruby/pull/6604) pre-commit config updates
- [#6607](https://github.com/mruby/mruby/pull/6607) fix bigint on raspberry pi
- [#6610](https://github.com/mruby/mruby/pull/6610) Extract golden ratio prime into constant
- [#6614](https://github.com/mruby/mruby/pull/6614) Fix uninitialized variable in io_gets causing segmentation fault
- [#6617](https://github.com/mruby/mruby/pull/6617) Fix various minor problems and speed up build
- [#6618](https://github.com/mruby/mruby/pull/6618) Stop generating unnecessary C++ files in mruby-bin-mruby
- [#6621](https://github.com/mruby/mruby/pull/6621) Set up all GEMS before mruby core tasks definition
- [#6624](https://github.com/mruby/mruby/pull/6624) Fixed wrong `MRuby::Build.current` at the top level of `mrbgem.rake`
- [#6628](https://github.com/mruby/mruby/pull/6628) Revert `File.absolute_path` logic
- [#6629](https://github.com/mruby/mruby/pull/6629) pre-commit update
- [#6631](https://github.com/mruby/mruby/pull/6631) Revert "Rakefile: make the whole thing parallel unless SERIAL=1"
- [#6633](https://github.com/mruby/mruby/pull/6633) Fix a heap-buffer-overflow in str strip! methods
- [#6643](https://github.com/mruby/mruby/pull/6643) Fix crash caused by an incorrect node type check in `codegen_masgn`
- [#6651](https://github.com/mruby/mruby/pull/6651) Address stack-use-after-return in the mruby bigint implementation
- [#6653](https://github.com/mruby/mruby/pull/6653) Improve HAL-related components for MinGW
- [#6655](https://github.com/mruby/mruby/pull/6655) Preventing Memory Leaks in `Array#__combination_init`
- [#6656](https://github.com/mruby/mruby/pull/6656) Fix integer overflow in allocation size calculation
- [#6663](https://github.com/mruby/mruby/pull/6663) Added the `kh_is_end()` macro function
- [#6665](https://github.com/mruby/mruby/pull/6665) Fixed use-after-free with `Set#join`
- [#6670](https://github.com/mruby/mruby/pull/6670) Arranging VM dispatch macros
- [#6673](https://github.com/mruby/mruby/pull/6673) Adjust broken license links; clean up Markdown
- [#6677](https://github.com/mruby/mruby/pull/6677) gha: run pre-commit with `--color=always`
- [#6678](https://github.com/mruby/mruby/pull/6678) Put ls-lint and pre-commit in separate workflow files
- [#6679](https://github.com/mruby/mruby/pull/6679) pre-commit autoupdate; update node and prettier
- [#6681](https://github.com/mruby/mruby/pull/6681) Add Cosmopolitan Libc build configuration

# Security Fixes

- Buffer overflow in bigint uadd ([3f2611e](https://github.com/mruby/mruby/commit/3f2611e))
- Buffer overflow in pack_uu encoding ([2993302](https://github.com/mruby/mruby/commit/2993302))
- Buffer overflow in IO#ungetc ([01ab2ff](https://github.com/mruby/mruby/commit/01ab2ff))
- Use-after-free in Set operations ([a6b55e7](https://github.com/mruby/mruby/commit/a6b55e7))
- Use-after-free in Array set operations ([729b84c](https://github.com/mruby/mruby/commit/729b84c))
- Use-after-free in Set#join ([0e653eb](https://github.com/mruby/mruby/commit/0e653eb))
- Use-after-realloc in Array#sort! ([eb39897](https://github.com/mruby/mruby/commit/eb39897))
- Multiple memory leak fixes in bigint, Set, Array, and Task gems
