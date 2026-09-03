# User visible changes in `mruby4.1` from `mruby4.0`

"**_NOTE_**:" are changes to be aware of.

# The language

- Brace-less variable interpolation in strings: `"#$global"`, `"#@ivar"` and
  `"#@@cvar"` ([d4aad5d](https://github.com/mruby/mruby/commit/d4aad5d))
- **_NOTE_**: `protected` methods are called with the caller's `self` checked
  against the receiver's class, as CRuby does ([534efe0](https://github.com/mruby/mruby/commit/534efe0))
- **_NOTE_**: a private `method_missing` no longer breaks dynamic dispatch, and
  `Kernel.<method>` calls reach their methods again ([#6974](https://github.com/mruby/mruby/issues/6974), [#6975](https://github.com/mruby/mruby/issues/6975), [#6976](https://github.com/mruby/mruby/issues/6976))
- Strings can be used in pattern matching ([#6830](https://github.com/mruby/mruby/issues/6830))
- Array splatting in patterns matches what CRuby matches ([#6854](https://github.com/mruby/mruby/issues/6854))
- `break` with an argument behaves as it does in CRuby ([#6927](https://github.com/mruby/mruby/issues/6927))
- Numbered parameters no longer read as `nil` ([#6921](https://github.com/mruby/mruby/issues/6921))
- `nil?` as a condition behaves as `self.nil?` does ([#6874](https://github.com/mruby/mruby/issues/6874))

# Regular expressions

mruby has regular expressions. `mruby-regexp` is a new gem, in the `stdlib`
gembox, carrying an engine written for this tree rather than a bundled Onigmo:
a Pike VM answers a pattern whose shape allows it, and a backtracker answers
the rest. It is not in `default`, so a build asks for it.

## What it provides

- `Regexp` and `MatchData`, and the literal syntax the compiler already read
- On `Regexp`: `=~`, `===`, `match`, `match?`, `source`, `options`,
  `casefold?`, `to_s`, `inspect`, `names`, `named_captures`, `union`, `escape`
  and `last_match`
- On `MatchData`: `[]` (by index, name, or a start and length or Range),
  `begin`, `end`, `offset`, `captures`, `named_captures`, `names`,
  `pre_match`, `post_match`, `values_at`, `to_a`, `string` and `regexp`
- On `String`: the regexp forms of `=~`, `[]`, `[]=`, `slice`, `slice!`,
  `index`, `rindex`, `byteindex`, `byterindex`, `match`, `match?`, `scan`,
  `split`, `sub`, `sub!`, `gsub`, `gsub!`, `partition`, `rpartition` and
  `start_with?`
- On `Symbol`: `match`, `match?` and `=~`
- The match globals `$~`, `` $` ``, `$'`, `$&` and `$1` to `$9`

## Syntax

- Character classes, POSIX brackets, and nested classes read as the union
  they are ([#7469](https://github.com/mruby/mruby/pull/7469))
- Greedy, lazy and possessive quantifiers, and atomic groups `(?>...)`
- Lookahead `(?=...)`, `(?!...)` and lookbehind `(?<=...)`, `(?<!...)`, the
  latter with each branch of the body carrying its own width ([#7456](https://github.com/mruby/mruby/pull/7456))
- Named groups, backreferences `\k<name>` and subexpression calls `\g<name>`
- The absent repeater `(?~...)` ([#7464](https://github.com/mruby/mruby/pull/7464)) and the conditional
  `(?(cond)yes|no)` ([#7467](https://github.com/mruby/mruby/pull/7467)), which complete the `(?...)` forms CRuby reads
- Comment groups `(?#...)` ([#7055](https://github.com/mruby/mruby/pull/7055)), inline options and free-spacing mode
- UTF-8 subjects are matched by character where the build indexes by
  character, and by byte where it does not

## Limitations

`mrbgems/mruby-regexp/README.md` lists these in full. The ones worth knowing:

- Character class intersection `[a&&b]` is not read
- A collating element `[[.a.]]` and an equivalence class `[[=a=]]` raise
- A pattern that recurses past what the engine can carry raises rather than
  answering wrongly ([#7280](https://github.com/mruby/mruby/pull/7280))
- The `tools/difftest` corpus records where this engine and CRuby's differ on
  purpose, and checks the rest against a running CRuby

# Strings and encoding

- **_NOTE_**: what a string's bytes read as is held as a two-bit coderange
  beside a one-bit encoding index, rather than as three separate flags
  ([#7158](https://github.com/mruby/mruby/pull/7158), [#7169](https://github.com/mruby/mruby/pull/7169), [#7170](https://github.com/mruby/mruby/pull/7170), [#7173](https://github.com/mruby/mruby/pull/7173))
- **_NOTE_**: `MRB_STR_SINGLE_BYTE` means every byte is ASCII, and no longer
  leaks into `scrub`, `ord` or `codepoints` ([#7131](https://github.com/mruby/mruby/pull/7131))
- A binary string is measured, inspected and chopped by byte ([#7080](https://github.com/mruby/mruby/pull/7080), [#7081](https://github.com/mruby/mruby/pull/7081), [#7083](https://github.com/mruby/mruby/pull/7083), [#7084](https://github.com/mruby/mruby/pull/7084))
- UTF-8 sequences forbidden by RFC 3629 are rejected ([#7093](https://github.com/mruby/mruby/pull/7093))
- `String#succ` carries across characters and steps the Unicode letters ([#7288](https://github.com/mruby/mruby/pull/7288))
- `String#scrub` was added ([#6859](https://github.com/mruby/mruby/issues/6859))
- `Regexp.escape` escapes whitespace, `#` and `-` ([#7047](https://github.com/mruby/mruby/pull/7047))

# Changes in mrbgems

## New Gems

- `mruby-regexp`: `Regexp` and `MatchData` over an engine written for this
  tree, in the `stdlib` gembox. See "Regular expressions" above
- `mruby-process`: a `Process` module over a process HAL, with
  `Process.clock_gettime`, `Process.clock_getres`, `Process.times`,
  `Process::Tms`, `Process::Status`, `Process.kill` and the `wait` family;
  in the `stdlib-io` gembox ([#7301](https://github.com/mruby/mruby/pull/7301), [#7421](https://github.com/mruby/mruby/pull/7421), [#7447](https://github.com/mruby/mruby/pull/7447))
- `mruby-signal`: a `Signal` module and the platform's signal table, which
  `Process.kill` and `Process::Status#to_s` both read; in `stdlib-io`
- `mruby-env`: `ENV` for environment variable access; in `stdlib-io`
- `mruby-string-bitops`: bit operations on `String`
- `mruby-bin-mrb`: an mruby runtime command with no compiler in it, for
  running bytecode where the compiler is not wanted

**_NOTE_**: the compiler is Prism. `mruby-compiler`, `mruby-eval` and
`mruby-bin-{mrbc,mruby,mirb}` are built on it, and the lrama-based compiler is
gone with the parser generator that fed it, the `parse.y` grammar, the
`lrama` and `full-core-lrama` gemboxes and `build_config/lrama.rb`
([aad25e7](https://github.com/mruby/mruby/commit/aad25e7)). A build that named
any of those needs updating.

## Other Gem Changes

- `mruby-task` gained a GLib HAL, a `timeout_ms` keyword on `Task::Queue#pop`
  and `mrb_hal_task_switch_hook` ([#6834](https://github.com/mruby/mruby/pull/6834), [#6918](https://github.com/mruby/mruby/pull/6918), [#6942](https://github.com/mruby/mruby/pull/6942))
- `mruby-io` handles UTF-8 output on Windows consoles and defines `IO.pipe`
  where the port has one ([#6996](https://github.com/mruby/mruby/pull/6996), [#7314](https://github.com/mruby/mruby/pull/7314))
- `mruby-dir` supports UTF-8 directory paths on Windows ([#6967](https://github.com/mruby/mruby/pull/6967))
- `mruby-benchmark` measures CPU time through `Process.times` ([#7452](https://github.com/mruby/mruby/pull/7452))

# Changes in C API

- `MRB_API mrb_task_queue_push()` ([#6955](https://github.com/mruby/mruby/pull/6955))
- **_NOTE_**: `mrb_gc_register()` counts its registrations, so a second
  unregister no longer drops another owner's pin ([#7277](https://github.com/mruby/mruby/issues/7277))
- `mrb_load_irep_cxt()` and `mrb_generate_code()` report a refused irep
  ([#7202](https://github.com/mruby/mruby/pull/7202), [#7204](https://github.com/mruby/mruby/pull/7204))
- Deprecated `mrb_data_check_and_get()` is no longer used internally ([#6774](https://github.com/mruby/mruby/pull/6774))

# GC and Memory

- Special variables of a frame are held beside the callinfo stack rather than
  in it, keeping `mrb_callinfo` at its size ([5993adb](https://github.com/mruby/mruby/commit/5993adb))
- The symbol GC marks the symbols an irep holds ([38d4283](https://github.com/mruby/mruby/commit/38d4283))
- Incremental GC is never driven from a `realloc` ([#6932](https://github.com/mruby/mruby/pull/6932))

# Build & Configuration

- A build compiles by the names the sources have from the tree, so two
  checkouts of one commit produce the same binary ([#7374](https://github.com/mruby/mruby/pull/7374), [#7439](https://github.com/mruby/mruby/pull/7439))
- Prism's generated sources are written under the build directory rather than
  into the submodule ([#7462](https://github.com/mruby/mruby/issues/7462))
- Every build writes a `size.json` of its artifacts ([#7450](https://github.com/mruby/mruby/pull/7450))
- `rake defines` reports where every define came from ([#7453](https://github.com/mruby/mruby/pull/7453))
- A build can ask the compiler whether a header is there ([#7432](https://github.com/mruby/mruby/pull/7432))
- A generated output another configuration left behind is rebuilt ([#7236](https://github.com/mruby/mruby/pull/7236))
- CI gained 32-bit x86 ([2fd16f3](https://github.com/mruby/mruby/commit/2fd16f3)),
  32-bit Arm ([5ee3bfc](https://github.com/mruby/mruby/commit/5ee3bfc)) and
  AArch64 ([43450e9](https://github.com/mruby/mruby/commit/43450e9)) jobs, and
  builds without `Float` and without bigint ([#7454](https://github.com/mruby/mruby/pull/7454))

# Security Fixes

- Use-after-free in `String#lstrip!`, `#rstrip!` and `#strip!` ([#7082](https://github.com/mruby/mruby/pull/7082))
- Use-after-free driving incremental GC from a `realloc` ([#6932](https://github.com/mruby/mruby/pull/6932))
- Heap-buffer-overflow read in `MatchData#[]` for an over-long group name ([#7002](https://github.com/mruby/mruby/pull/7002))
- Out-of-bounds write in a regexp character class range ([#6937](https://github.com/mruby/mruby/issues/6937))
- Out-of-bounds reads in `case` splat from a callback replacing the array ([#7092](https://github.com/mruby/mruby/issues/7092))
- Storage pointers retained across Ruby callbacks in `Hash#__except`,
  `Struct#==` and `Struct#eql?` ([#7100](https://github.com/mruby/mruby/issues/7100), [#7101](https://github.com/mruby/mruby/issues/7101))
- Signed overflow in `pack_uu` line count ([#6978](https://github.com/mruby/mruby/pull/6978))
- `mrb_int` overflow in `ary_insert` and `ary_fill_exec` ([#6986](https://github.com/mruby/mruby/pull/6986))
- Integer overflows and out-of-bounds accesses in `read_debug_record` ([#6949](https://github.com/mruby/mruby/issues/6949), [#6950](https://github.com/mruby/mruby/issues/6950), [#6951](https://github.com/mruby/mruby/issues/6951))
- Unbounded `trunc` counter in `mrb_read_float()` ([#6958](https://github.com/mruby/mruby/issues/6958))
- NULL-pointer dereference in `assign_class_name` ([#6842](https://github.com/mruby/mruby/issues/6842))
- NULL dereference in `mt_put` when `protected` copies an inherited method ([#7293](https://github.com/mruby/mruby/issues/7293))
- Double free and use-after-free in `mrb_debug_set_break_method` ([#6851](https://github.com/mruby/mruby/issues/6851))
- Double free during GC sweep ([#6316](https://github.com/mruby/mruby/issues/6316))
- Undefined behaviour in `mpn_zero` from Montgomery modpow squaring ([#6849](https://github.com/mruby/mruby/issues/6849))
- `ary_make_shared()` operator precedence in its allocation size ([#7422](https://github.com/mruby/mruby/issues/7422))

# Fixed GitHub Issues

- [#6239](https://github.com/mruby/mruby/issues/6239) Build option to build a shared library and link it with the resulting binaries
- [#6316](https://github.com/mruby/mruby/issues/6316) Double free during GC sweep
- [#6345](https://github.com/mruby/mruby/issues/6345) If the original block of a non-orphan block becomes an orphan block, the copy object does not become an orphan block (incompatibility with CRuby).
- [#6764](https://github.com/mruby/mruby/issues/6764) Proc#arity and Proc#parameters for cfunc methods: storing aspec on RProc
- [#6815](https://github.com/mruby/mruby/issues/6815) `MRB_NAN_BOXING` cannot be built on 32-bit systems
- [#6819](https://github.com/mruby/mruby/issues/6819) Usage of GetLastError where WSAGetLastError must be used
- [#6830](https://github.com/mruby/mruby/issues/6830) Strings cannot be used in pattern matching.
- [#6831](https://github.com/mruby/mruby/issues/6831) Failed unit tests for mruby-bin-mrbc
- [#6832](https://github.com/mruby/mruby/issues/6832) Failed unit tests for mruby-bin-mruby
- [#6842](https://github.com/mruby/mruby/issues/6842) NULL-pointer deref in assign_class_name
- [#6849](https://github.com/mruby/mruby/issues/6849) UB nonnull violation in mpn_zero from Montgomery modpow squaring
- [#6851](https://github.com/mruby/mruby/issues/6851) [mrdb] Missing return in mrb_debug_set_break_method leads to Double Free / UAF
- [#6853](https://github.com/mruby/mruby/issues/6853) mruby-regexp: /\Aa(b)?c?\z/ does not match "a"
- [#6854](https://github.com/mruby/mruby/issues/6854) Incompatibility in pattern matching between CRuby and mruby with array splatting
- [#6858](https://github.com/mruby/mruby/issues/6858) Is the exposure of linkage symbols without the “mrb_” prefix intentional?
- [#6859](https://github.com/mruby/mruby/issues/6859) Add String#scrub to mruby-string-ext
- [#6860](https://github.com/mruby/mruby/issues/6860) The unit tests for mruby-string-ext are failing
- [#6862](https://github.com/mruby/mruby/issues/6862) Assertion failed: (c->ci == c->cibase || (c->ci - c->cibase) == cioff - 1), function mrb_vm_run
- [#6863](https://github.com/mruby/mruby/issues/6863) mruby-task: segmentation fault
- [#6865](https://github.com/mruby/mruby/issues/6865) mruby-task: segmentation fault
- [#6868](https://github.com/mruby/mruby/issues/6868) Assertion failed: (c->ci == c->cibase || (c->ci - c->cibase) == cioff - 1), function mrb_vm_run, file .../mruby/src/vm.c, line 1664.
- [#6870](https://github.com/mruby/mruby/issues/6870) Assertion failed: ((obj)->tt != MRB_TT_FREE), function mrb_gc_mark
- [#6874](https://github.com/mruby/mruby/issues/6874) `nil?` condition does not behave the same as `self.nil?`
- [#6883](https://github.com/mruby/mruby/issues/6883) Regexp#=== does not capture groups
- [#6886](https://github.com/mruby/mruby/issues/6886) Assertion failed: ((obj)->tt != MRB_TT_FREE), function mrb_gc_mark
- [#6887](https://github.com/mruby/mruby/issues/6887) mrb_vm_run assert after Task.pass from root
- [#6889](https://github.com/mruby/mruby/issues/6889) Rake Task to only load deps
- [#6892](https://github.com/mruby/mruby/issues/6892) `String#sub`/`#gsub` with `\'` replacement token crashes on subjects containing embedded NUL bytes
- [#6921](https://github.com/mruby/mruby/issues/6921) numbered parameter can become nil
- [#6927](https://github.com/mruby/mruby/issues/6927) Incompatibility between CRuby and mruby when break has an argument
- [#6929](https://github.com/mruby/mruby/issues/6929) building amalgamation with mruby-compiler
- [#6937](https://github.com/mruby/mruby/issues/6937) [BUG] regexp_class_add_range_oob_write
- [#6940](https://github.com/mruby/mruby/issues/6940) problem with mruby 4.0.0 on gcc 16
- [#6944](https://github.com/mruby/mruby/issues/6944) When the C++ ABI is enabled in mruby-compiler, a macro redefinition warning is output
- [#6948](https://github.com/mruby/mruby/issues/6948) Potential signed integer overflow in mrb_str_len_to_integer radix normalization
- [#6949](https://github.com/mruby/mruby/issues/6949) Potential integer overflow causing out-of-bounds accesses in read_debug_record
- [#6950](https://github.com/mruby/mruby/issues/6950) Potential out-of-bounds access in read_debug_record array line decoding
- [#6951](https://github.com/mruby/mruby/issues/6951) Potential out-of-bounds reads of debug record headers in read_debug_record
- [#6958](https://github.com/mruby/mruby/issues/6958) mrb_read_float(): unbounded signed `trunc` counter can overflow on extremely long digit strings
- [#6959](https://github.com/mruby/mruby/issues/6959) mruby-test `str_match_p()` recurses on brace patterns with no depth limit (C stack exhaustion)
- [#6960](https://github.com/mruby/mruby/issues/6960) Addrinfo.getaddrinfo silently truncates mrb_int family/socktype/protocol/flags to int
- [#6969](https://github.com/mruby/mruby/issues/6969) When running `build/host/bin/mruby -vce ...`, the AST is not displayed, and irep is output twice
- [#6971](https://github.com/mruby/mruby/issues/6971) Excessive compilation of the `build/host/mrbc` task
- [#6972](https://github.com/mruby/mruby/issues/6972) A build error occurs if the setup block for `mruby-compiler` is not called before `mruby-bin-mrbc`
- [#6974](https://github.com/mruby/mruby/issues/6974) A user-defined private `method_missing` breaks every dynamic dispatch on the receiver
- [#6975](https://github.com/mruby/mruby/issues/6975) Since 3.4.0, `protected` methods cannot be called with an explicit receiver, even from the same class
- [#6976](https://github.com/mruby/mruby/issues/6976) Since 3.4.0, `Kernel.puts` and most other `Kernel.<method>` calls raise NoMethodError
- [#7003](https://github.com/mruby/mruby/issues/7003) mruby-regexp: String#split dispatches to_int where the rest of mruby does not, and the respond_to? guarding it is redefinable
- [#7012](https://github.com/mruby/mruby/issues/7012) mruby-compiler: generator error, Can't find local variables
- [#7014](https://github.com/mruby/mruby/issues/7014) An argument that defines `__to_int` is accepted where `to_int` is rejected
- [#7015](https://github.com/mruby/mruby/issues/7015) `Enumerable#take` dispatches `to_i` on the result of `__to_int`
- [#7032](https://github.com/mruby/mruby/issues/7032) [Bug]mruby-compiler/Prism parser passes NULL to nonnull string functions (UBSan undefined behavior, CWE-476)
- [#7044](https://github.com/mruby/mruby/issues/7044) `OP_ADDILV` and `OP_SUBILV` set the method call fallback up on the local variables, corrupting them
- [#7090](https://github.com/mruby/mruby/issues/7090) mruby-io: partial IO#dup failure leaves a stale descriptor owner
- [#7092](https://github.com/mruby/mruby/issues/7092) [BUG] Array replacement from callbacks causes out-of-bounds reads in case splats and IO#puts
- [#7100](https://github.com/mruby/mruby/issues/7100) Hash#__except retains Array and Hash storage across Ruby callbacks
- [#7101](https://github.com/mruby/mruby/issues/7101) Struct#== and Struct#eql? retain member-storage pointers across Ruby callbacks
- [#7111](https://github.com/mruby/mruby/issues/7111) mrbgems/mruby-test fails to link without mruby-compiler
- [#7112](https://github.com/mruby/mruby/issues/7112) mrbgems/mruby-rational fails to compile without mruby-bigint
- [#7277](https://github.com/mruby/mruby/issues/7277) Proposal: per-object pin flag for leaf objects (Strings/Data)
- [#7290](https://github.com/mruby/mruby/issues/7290) Compiling top-level 'super' produces invalid OP_GETUPVAR bytecode
- [#7293](https://github.com/mruby/mruby/issues/7293) NULL dereference in `mt_put` when `protected` copies an inherited method onto a singleton class with no method table
- [#7346](https://github.com/mruby/mruby/issues/7346) mrb_gc_(un)register hangs after a few iterations
- [#7422](https://github.com/mruby/mruby/issues/7422) Bug: ary_make_shared() allocates sizeof(mrb_value)*len+1 -operator precedence adds 1 byte instead of 1 element
- [#7462](https://github.com/mruby/mruby/issues/7462) Build process writes Prism related files to source directory with out-of-source build

# Merged Pull Requests

- [#6218](https://github.com/mruby/mruby/pull/6218) Reduced description of `mrb_init_core()`
- [#6334](https://github.com/mruby/mruby/pull/6334) Remove `iterating` variable from `mrb_objspace_each_objects()`
- [#6576](https://github.com/mruby/mruby/pull/6576) Share array entities if possible with `ary.replace(frozen_ary)`
- [#6577](https://github.com/mruby/mruby/pull/6577) Sharing arrays with `ary_dup()`
- [#6580](https://github.com/mruby/mruby/pull/6580)  Improve `File.basename`
- [#6596](https://github.com/mruby/mruby/pull/6596) pre-commit add official meta hook `check-useless-excludes`
- [#6691](https://github.com/mruby/mruby/pull/6691) pre-commit: add hook to ensure Makefiles are indented with tabs
- [#6774](https://github.com/mruby/mruby/pull/6774) Avoid using the deprecated function `mrb_data_check_and_get()`
- [#6775](https://github.com/mruby/mruby/pull/6775) Improve the calculation of the next index for `Array#__combination_next`
- [#6776](https://github.com/mruby/mruby/pull/6776) Return nil if a number less than 1 is passed to `Array#__combination_init`
- [#6777](https://github.com/mruby/mruby/pull/6777) Make `Array#__combination_next` return an array of elements
- [#6785](https://github.com/mruby/mruby/pull/6785) Store compressed aspec on cfunc RProc for correct arity/parameters
- [#6786](https://github.com/mruby/mruby/pull/6786) Early conversion of `mesg` to a string object in `mrb_sys_fail()`
- [#6787](https://github.com/mruby/mruby/pull/6787) Supplement to #6781
- [#6799](https://github.com/mruby/mruby/pull/6799) Define the typedef for `mrb_state` earlier
- [#6800](https://github.com/mruby/mruby/pull/6800) mruby 4.0.0 released
- [#6803](https://github.com/mruby/mruby/pull/6803) Disable some gems on build_config for playstationportable
- [#6812](https://github.com/mruby/mruby/pull/6812) Add new fuzzing harness to be consumed by OSS-Fuzz
- [#6814](https://github.com/mruby/mruby/pull/6814) Don't use `#puts` in bintest for "mruby-bin-mrb"
- [#6816](https://github.com/mruby/mruby/pull/6816) Rename the members of the `mrb_combination_state` structure
- [#6817](https://github.com/mruby/mruby/pull/6817) Integrate `Array#{permutation,combination}` into `Array#__combination`
- [#6820](https://github.com/mruby/mruby/pull/6820) Fix Class inherited hook ordering
- [#6822](https://github.com/mruby/mruby/pull/6822) Run prek; remove trailing whitespace; cleanup markdown
- [#6823](https://github.com/mruby/mruby/pull/6823) Pin remaining actions to hash
- [#6829](https://github.com/mruby/mruby/pull/6829) add IO#autoclose= and IO#autoclose?
- [#6833](https://github.com/mruby/mruby/pull/6833) Improve wakeup tick condition check
- [#6834](https://github.com/mruby/mruby/pull/6834) mruby-task GLib HAL
- [#6835](https://github.com/mruby/mruby/pull/6835) Introduce Task::Queue
- [#6836](https://github.com/mruby/mruby/pull/6836) Fix mruby-task: keep join waiter wakeup under one IRQ critical section
- [#6837](https://github.com/mruby/mruby/pull/6837) Avoid surrounding `#if` when using `mrb_const_cache_clear()`
- [#6838](https://github.com/mruby/mruby/pull/6838) Use `MRB_SYM()` in `gc_stat()`
- [#6841](https://github.com/mruby/mruby/pull/6841) Small refactor for `incremental_sweep_phase()`
- [#6856](https://github.com/mruby/mruby/pull/6856) test/bintest.rb: tokenize ENV['EMULATOR'] via Shellwords.split
- [#6866](https://github.com/mruby/mruby/pull/6866) task: return `nil` when given a nested call to `Task.run`
- [#6869](https://github.com/mruby/mruby/pull/6869) Separate the union of timeslice and result in struct mrb_task
- [#6871](https://github.com/mruby/mruby/pull/6871) Fix execute_task() so unhandled task exceptions become task results
- [#6876](https://github.com/mruby/mruby/pull/6876) [stable] backport: cap IO#puts recursion depth on cyclic arrays (mruby-io)
- [#6877](https://github.com/mruby/mruby/pull/6877) Free the index array immediately at the end of `ary_combination_next()`
- [#6879](https://github.com/mruby/mruby/pull/6879) Add alias support for mruby-method and mruby-proc-ext
- [#6884](https://github.com/mruby/mruby/pull/6884) regexp: support capture groups in Regexp#===
- [#6891](https://github.com/mruby/mruby/pull/6891) mruby-task: return task result when mrb_task_run_once stops a task
- [#6893](https://github.com/mruby/mruby/pull/6893) mruby-regexp: Fix String#gsub block handling for zero-width matches
- [#6895](https://github.com/mruby/mruby/pull/6895) Fix build when `MRB_NO_FLOAT` is defined
- [#6896](https://github.com/mruby/mruby/pull/6896) Add opt-in "mruby-*-prism" mgem family
- [#6898](https://github.com/mruby/mruby/pull/6898) mruby-task: Use MRB_IVSYM instead of mrb_intern_lit
- [#6902](https://github.com/mruby/mruby/pull/6902) mruby-bin-mrbc: define global_mrb for non-mruby targets
- [#6909](https://github.com/mruby/mruby/pull/6909) Fix Documentation and Comments
- [#6912](https://github.com/mruby/mruby/pull/6912) mruby-regexp: Fix String#split compatibility
- [#6913](https://github.com/mruby/mruby/pull/6913) mruby-regexp: improve Regexp match compatibility
- [#6915](https://github.com/mruby/mruby/pull/6915) mruby-regexp: correct regexp offset conversion for UTF-8 and binary strings
- [#6917](https://github.com/mruby/mruby/pull/6917) Add new actions runner images to `build.yml`
- [#6918](https://github.com/mruby/mruby/pull/6918) mruby-task: Introduce timeout_ms keyword to Task::Queue#pop
- [#6926](https://github.com/mruby/mruby/pull/6926) gc.c: Add gc->collecting flag and gc_drive()
- [#6928](https://github.com/mruby/mruby/pull/6928)  vm.c: defer pending task switch while an exception is in flight
- [#6930](https://github.com/mruby/mruby/pull/6930) mruby-task: survive OOM during task context initialization
- [#6931](https://github.com/mruby/mruby/pull/6931) mruby-task: exclude the tick IRQ while GC marks the task queues
- [#6932](https://github.com/mruby/mruby/pull/6932) Fix: never drive incremental GC from a realloc (use-after-free)
- [#6933](https://github.com/mruby/mruby/pull/6933) gc: try to reclaim before growing the object heap
- [#6934](https://github.com/mruby/mruby/pull/6934) mruby-task: don't accumulate a GC arena entry per execution slice
- [#6935](https://github.com/mruby/mruby/pull/6935) mruby-task: assert that task.irq_nesting < UINT8_MAX before increment
- [#6938](https://github.com/mruby/mruby/pull/6938) Feature: Scheduler Driven GC
- [#6941](https://github.com/mruby/mruby/pull/6941) Fix maybe-uninitialized warning
- [#6942](https://github.com/mruby/mruby/pull/6942) mruby-task: add mrb_hal_task_switch_hook to the HAL contract
- [#6943](https://github.com/mruby/mruby/pull/6943) Fix Task::Queue backing array growth after long FIFO use
- [#6947](https://github.com/mruby/mruby/pull/6947) Feature: Task#close
- [#6952](https://github.com/mruby/mruby/pull/6952) Make scheduler-driven GC pending less conservative
- [#6953](https://github.com/mruby/mruby/pull/6953) optimize `mrb_type` into a LUT-based approach
- [#6955](https://github.com/mruby/mruby/pull/6955) Add MRB_API mrb_task_queue_push()
- [#6957](https://github.com/mruby/mruby/pull/6957) load.c: bounds-check symbol length in read_section_lv
- [#6966](https://github.com/mruby/mruby/pull/6966) vm.c: fix envadjust rebasing stack pointers off a freed pointer
- [#6967](https://github.com/mruby/mruby/pull/6967) mruby-dir: support UTF-8 directory paths on Windows
- [#6978](https://github.com/mruby/mruby/pull/6978) mruby-pack: avoid signed overflow in pack_uu line count
- [#6979](https://github.com/mruby/mruby/pull/6979) Fix a build error occurs if the setup block for mruby-compiler is not called before mruby-bin-mrbc
- [#6981](https://github.com/mruby/mruby/pull/6981) Omitting mruby-compiler in `MRuby::Build#build_mrbc_exec`
- [#6982](https://github.com/mruby/mruby/pull/6982) Add mrb_task_set_scheduler_hook for pre-scheduling deferred work. Remove  mrb_hal_task_switch_hook instead
- [#6983](https://github.com/mruby/mruby/pull/6983) mruby-task: Detach envs from a task stack before freeing it
- [#6984](https://github.com/mruby/mruby/pull/6984) Restore `gc.disabled` when even out of memory
- [#6986](https://github.com/mruby/mruby/pull/6986) guard mrb_int overflow in ary_insert and ary_fill_exec
- [#6987](https://github.com/mruby/mruby/pull/6987) Omit the backtrace if a pre-generated `NoMemoryError` is raised
- [#6988](https://github.com/mruby/mruby/pull/6988) mruby-regexp: raise TypeError for a String argument to String#=~
- [#6989](https://github.com/mruby/mruby/pull/6989) mruby-regexp: fix String#sub/#gsub argument handling
- [#6991](https://github.com/mruby/mruby/pull/6991) mruby-regexp: pass the block through String#match
- [#6992](https://github.com/mruby/mruby/pull/6992) mruby-regexp: test break out of the Regexp#match block
- [#6993](https://github.com/mruby/mruby/pull/6993) mruby-regexp: add Symbol#match, #match? and #=~
- [#6994](https://github.com/mruby/mruby/pull/6994) mruby-regexp: reject non-Regexp patterns in `String#match` and `#match?`
- [#6995](https://github.com/mruby/mruby/pull/6995) mruby-regexp: accept a Symbol on the Regexp side of a match
- [#6996](https://github.com/mruby/mruby/pull/6996) mruby-io: fix UTF-8 output on Windows consoles
- [#6997](https://github.com/mruby/mruby/pull/6997) ci: skip CIFuzz on forks
- [#6999](https://github.com/mruby/mruby/pull/6999) Add `NilClass#=~`
- [#7000](https://github.com/mruby/mruby/pull/7000) mruby-regexp: fix `MatchData#[]` for a negative index and an unknown name
- [#7001](https://github.com/mruby/mruby/pull/7001) mruby-regexp: type-check the `sub`, `gsub`, `scan`, `split` and `=~` pattern
- [#7002](https://github.com/mruby/mruby/pull/7002) mruby-regexp: fix a heap-buffer-overflow read in MatchData#[] for an over-long group name
- [#7004](https://github.com/mruby/mruby/pull/7004) mruby-regexp: read the real type of `String#split`'s limit
- [#7005](https://github.com/mruby/mruby/pull/7005) mruby-regexp: rename `__match_pattern` to `__check_pattern`
- [#7008](https://github.com/mruby/mruby/pull/7008) Remove broken Star History from README
- [#7009](https://github.com/mruby/mruby/pull/7009) Update for editorconfig-checker
- [#7011](https://github.com/mruby/mruby/pull/7011) Add mruby-string-bitops gem
- [#7013](https://github.com/mruby/mruby/pull/7013) CONTRIBUTING.md: name a protocol that `convert_type()` still dispatches
- [#7016](https://github.com/mruby/mruby/pull/7016) mruby-range-ext: pass the converted argument to `Array#last`
- [#7017](https://github.com/mruby/mruby/pull/7017) array.c: type check the argument of `Array#last`
- [#7018](https://github.com/mruby/mruby/pull/7018) symbol.c: skip tombstones when migrating to the hash table
- [#7019](https://github.com/mruby/mruby/pull/7019) mruby-regexp: bound the numeric \k backreference accumulator
- [#7020](https://github.com/mruby/mruby/pull/7020) symbol.c: copy the name of a symbol that symbol GC can free
- [#7021](https://github.com/mruby/mruby/pull/7021) mruby-regexp: reject an empty group name
- [#7022](https://github.com/mruby/mruby/pull/7022) vm.c: restore the GC arena in the allocating inline opcodes
- [#7023](https://github.com/mruby/mruby/pull/7023) vm.c: refresh regs before storing the OP_GETIDX0 Hash result
- [#7024](https://github.com/mruby/mruby/pull/7024) mruby-regexp: do not truncate a POSIX bracket class name length
- [#7025](https://github.com/mruby/mruby/pull/7025) mruby-regexp: set $&, $`, $' and $+ after a match
- [#7026](https://github.com/mruby/mruby/pull/7026) mruby-regexp: return Arrays from `Regexp#named_captures`
- [#7027](https://github.com/mruby/mruby/pull/7027) mruby-regexp: add `Regexp#names` and `MatchData#names`
- [#7031](https://github.com/mruby/mruby/pull/7031) mruby-regexp: quote the pattern as written in `RegexpError` messages
- [#7034](https://github.com/mruby/mruby/pull/7034) Add WASI toolchain & build_config
- [#7035](https://github.com/mruby/mruby/pull/7035) mruby-regexp: do not truncate a stored capture name length
- [#7037](https://github.com/mruby/mruby/pull/7037) Fix `Integer#==` reading a Bigint receiver as `mrb_int`
- [#7038](https://github.com/mruby/mruby/pull/7038) string.c: check the append size before mrb_str_cat() modifies
- [#7039](https://github.com/mruby/mruby/pull/7039) string.c: append into a shared buffer instead of copying it
- [#7040](https://github.com/mruby/mruby/pull/7040) vm.c: answer str[0] from C in OP_GETIDX0
- [#7041](https://github.com/mruby/mruby/pull/7041) mruby-regexp: keep a character class open past `[:name:]` and a leading `]`
- [#7042](https://github.com/mruby/mruby/pull/7042) vm.c: restore the GC arena where the Integer boxing macro allocates
- [#7043](https://github.com/mruby/mruby/pull/7043) string.c: record the source overlap before str_modify_cat() runs
- [#7045](https://github.com/mruby/mruby/pull/7045) mruby-regexp: convert a Bigint `String#split` limit
- [#7046](https://github.com/mruby/mruby/pull/7046) mruby-regexp: apply `/i` to the backreference comparison
- [#7047](https://github.com/mruby/mruby/pull/7047) mruby-regexp: escape whitespace, `#` and `-` in `Regexp.escape`
- [#7048](https://github.com/mruby/mruby/pull/7048) mruby-regexp: fix `MatchData#begin` / `#end` for a group name and an out-of-range index
- [#7049](https://github.com/mruby/mruby/pull/7049) mruby-regexp: apply `/i` to character classes
- [#7050](https://github.com/mruby/mruby/pull/7050) mruby-regexp: keep `$~` after `String#gsub` with a block
- [#7051](https://github.com/mruby/mruby/pull/7051) string.c: compare the append source as an address, not as a pointer
- [#7052](https://github.com/mruby/mruby/pull/7052) mruby-regexp: split a character class range at the ASCII boundary
- [#7053](https://github.com/mruby/mruby/pull/7053) mruby-regexp: snapshot the subject string in `create_matchdata()`
- [#7054](https://github.com/mruby/mruby/pull/7054) mruby-regexp: add the regexp form of `String#[]` and `String#slice`
- [#7055](https://github.com/mruby/mruby/pull/7055) mruby-regexp: support `(?#...)` comment groups
- [#7056](https://github.com/mruby/mruby/pull/7056) mruby-regexp: make a multibyte literal one atom
- [#7057](https://github.com/mruby/mruby/pull/7057) mruby-regexp: stop capturing plain groups once a pattern has a named group
- [#7058](https://github.com/mruby/mruby/pull/7058) mruby-regexp: Unicode simple case folding for /i behind an option
- [#7059](https://github.com/mruby/mruby/pull/7059) mruby-regexp: let a byte that starts no character begin a match
- [#7060](https://github.com/mruby/mruby/pull/7060) mruby-regexp: let mrbtest exercise `Symbol#[]` with a regexp
- [#7061](https://github.com/mruby/mruby/pull/7061) mruby-regexp: add the regexp form of `String#sub!` and `String#gsub!`
- [#7062](https://github.com/mruby/mruby/pull/7062) mruby-regexp: write the disabled flags in Regexp#to_s
- [#7063](https://github.com/mruby/mruby/pull/7063) mruby-regexp: add the regexp form of String#[]= and String#slice!
- [#7064](https://github.com/mruby/mruby/pull/7064) mruby-regexp: refuse a pattern too large for its jump targets
- [#7065](https://github.com/mruby/mruby/pull/7065) mruby-regexp: fix wrong comments on the String#split override
- [#7066](https://github.com/mruby/mruby/pull/7066) mruby-regexp: make an escaped multibyte literal one atom
- [#7067](https://github.com/mruby/mruby/pull/7067) mruby-regexp: document what a pattern still decides in the String overrides
- [#7068](https://github.com/mruby/mruby/pull/7068) mruby-regexp: reject overlong and out-of-range UTF-8 sequences
- [#7069](https://github.com/mruby/mruby/pull/7069) mruby-regexp: reject a lookbehind over a multibyte character class
- [#7070](https://github.com/mruby/mruby/pull/7070) mruby-regexp: a match may not end inside a character
- [#7071](https://github.com/mruby/mruby/pull/7071) mruby-regexp: stop a repetition on its empty iteration
- [#7072](https://github.com/mruby/mruby/pull/7072) mruby-regexp: test the F4 upper bound of the four-byte UTF-8 range
- [#7073](https://github.com/mruby/mruby/pull/7073) mruby-regexp: keep the pike VM's visited keys from wrapping
- [#7074](https://github.com/mruby/mruby/pull/7074) mruby-regexp: read the \u escape
- [#7075](https://github.com/mruby/mruby/pull/7075) mruby-regexp: accept a Regexp in String#index, #partition, #start_with? and their siblings
- [#7076](https://github.com/mruby/mruby/pull/7076) mruby-regexp: list `String#sub!` and `#gsub!` in the README
- [#7078](https://github.com/mruby/mruby/pull/7078) mruby-regexp: a byte that starts no character is a byte in a class
- [#7079](https://github.com/mruby/mruby/pull/7079) mruby-regexp: close the pattern dispatch in the String overrides
- [#7080](https://github.com/mruby/mruby/pull/7080) string.c: a copy of a binary string is binary
- [#7081](https://github.com/mruby/mruby/pull/7081) string.c: a binary string is measured in bytes
- [#7082](https://github.com/mruby/mruby/pull/7082) fix use-after-free in String#lstrip!/rstrip!/strip!
- [#7083](https://github.com/mruby/mruby/pull/7083) string.c: a binary string is inspected byte by byte
- [#7084](https://github.com/mruby/mruby/pull/7084) string.c: `String#chop!` takes a byte off a binary string
- [#7085](https://github.com/mruby/mruby/pull/7085) mruby-regexp: `String#split` steps by a byte through a binary subject
- [#7086](https://github.com/mruby/mruby/pull/7086) style/performance fixes
- [#7087](https://github.com/mruby/mruby/pull/7087) mruby-regexp: rewind a lookbehind by characters
- [#7088](https://github.com/mruby/mruby/pull/7088) mruby-regexp: split the test file by subject
- [#7089](https://github.com/mruby/mruby/pull/7089) mruby-socket: skip the AF_INET6 test where IPv6 is unavailable
- [#7093](https://github.com/mruby/mruby/pull/7093) string.c: reject UTF-8 sequences forbidden by RFC 3629
- [#7094](https://github.com/mruby/mruby/pull/7094) mruby-regexp: drop the private UTF-8 encoder in favor of `mrb_utf8_to_buf`
- [#7095](https://github.com/mruby/mruby/pull/7095) mruby-encoding: drop the unused `utf8_islead` macro
- [#7096](https://github.com/mruby/mruby/pull/7096) string.c: `String#length` stops at the end of a shared substring
- [#7097](https://github.com/mruby/mruby/pull/7097) Share character/byte offset conversion between core and mruby-regexp
- [#7098](https://github.com/mruby/mruby/pull/7098) string.c: bound check `mrb_str_byte_to_char` before use
- [#7099](https://github.com/mruby/mruby/pull/7099) string.c: search bytes in `String#byterindex` and in `String#rindex` on a binary string
- [#7102](https://github.com/mruby/mruby/pull/7102) Move the `String#valid_encoding?` body into core as `mrb_str_valid_encoding_p`
- [#7103](https://github.com/mruby/mruby/pull/7103) mruby-string-ext: `String#slice!` cuts a multibyte string by characters
- [#7104](https://github.com/mruby/mruby/pull/7104) Share the character count between core and mruby-string-ext
- [#7105](https://github.com/mruby/mruby/pull/7105) mruby-string-ext: validate UTF-8 sequences with `mrb_utf8len`
- [#7106](https://github.com/mruby/mruby/pull/7106) `String#chars` splits where `String#length` counts
- [#7107](https://github.com/mruby/mruby/pull/7107) string.c: step `String#rindex` by the characters `String#length` counts
- [#7108](https://github.com/mruby/mruby/pull/7108) internal.h: shorten the `mrb_str_valid_encoding_p` comment
- [#7109](https://github.com/mruby/mruby/pull/7109) Share UTF-8 byte lengths and character heads between core and mruby-regexp
- [#7113](https://github.com/mruby/mruby/pull/7113) mruby-sprintf: reject a `%c` argument with no UTF-8 encoding
- [#7114](https://github.com/mruby/mruby/pull/7114) mruby-pack: check the `pack("U")` range before the cast to `uint32_t`
- [#7115](https://github.com/mruby/mruby/pull/7115) mruby-regexp: ask the engine's byte questions of a byte-indexed subject too
- [#7116](https://github.com/mruby/mruby/pull/7116) mruby-regexp: ask the malformed sequence cases of a byte-indexed subject too
- [#7117](https://github.com/mruby/mruby/pull/7117) test: build wide shifts from a variable so the file still compiles
- [#7118](https://github.com/mruby/mruby/pull/7118) mruby-socket: reject a big integer getaddrinfo hint instead of ignoring it
- [#7120](https://github.com/mruby/mruby/pull/7120) mruby-regexp: ask the byte cases of a byte-indexed subject alone
- [#7121](https://github.com/mruby/mruby/pull/7121) mruby-regexp: say at each block why only a byte-indexed subject can answer
- [#7122](https://github.com/mruby/mruby/pull/7122) string.c: read String#chop! back from the last byte
- [#7123](https://github.com/mruby/mruby/pull/7123) mruby-bin-mirb: read UTF-8 through the core scanner
- [#7124](https://github.com/mruby/mruby/pull/7124) boxing_nan.h: keep `nil` out of `mrb_false_p`
- [#7125](https://github.com/mruby/mruby/pull/7125) mruby-regexp: measure a lookbehind in the characters its bytes spell
- [#7128](https://github.com/mruby/mruby/pull/7128) Share the UTF-8 decoder between core, mruby-regexp and mruby-string-ext
- [#7129](https://github.com/mruby/mruby/pull/7129) Check the Unicode range in `mrb_utf8_to_buf` instead of in its four callers
- [#7131](https://github.com/mruby/mruby/pull/7131) Stop a broken string from being read one byte at a time
- [#7132](https://github.com/mruby/mruby/pull/7132) Stop `Integer#chr` from calling a stray byte a character
- [#7133](https://github.com/mruby/mruby/pull/7133) mruby-regexp: refuse a broken subject at the entry to `split`
- [#7135](https://github.com/mruby/mruby/pull/7135) Add a .clangd so headers resolve their own types when opened directly
- [#7136](https://github.com/mruby/mruby/pull/7136) Read a copy of a byte-read string's bytes the way they were read
- [#7137](https://github.com/mruby/mruby/pull/7137) Read a string built out of two strings off both of them
- [#7138](https://github.com/mruby/mruby/pull/7138) ci: cover a build without MRB_UTF8_STRING
- [#7139](https://github.com/mruby/mruby/pull/7139) mruby-string-ext: skip four UTF-8 tests where they cannot run
- [#7140](https://github.com/mruby/mruby/pull/7140) ci: name the bintest build in `ci/gcc-clang`
- [#7141](https://github.com/mruby/mruby/pull/7141) coverage: keep same-named functions from different builds apart
- [#7143](https://github.com/mruby/mruby/pull/7143) Read what sprintf builds the way its bytes were read
- [#7144](https://github.com/mruby/mruby/pull/7144) string.c: remember that a string was read as broken
- [#7145](https://github.com/mruby/mruby/pull/7145) Write a byte escape in upper case hex, as CRuby does
- [#7146](https://github.com/mruby/mruby/pull/7146) Read the pieces `partition` cuts the way the whole was read
- [#7147](https://github.com/mruby/mruby/pull/7147) string.h: drop the unused `MRB_STR_STATE_MASK`
- [#7148](https://github.com/mruby/mruby/pull/7148) Walk the backward regexp search by byte
- [#7149](https://github.com/mruby/mruby/pull/7149) Publish only the match a backward search answers with
- [#7150](https://github.com/mruby/mruby/pull/7150) Write `MRB_STR_BINARY` through accessors, as the other flags are
- [#7151](https://github.com/mruby/mruby/pull/7151) test: seed the group a backward search miss has to clear
- [#7152](https://github.com/mruby/mruby/pull/7152) mruby-regexp: refuse a byte search offset inside a character
- [#7153](https://github.com/mruby/mruby/pull/7153) mruby-regexp: answer a byte search before the subject with a miss
- [#7154](https://github.com/mruby/mruby/pull/7154) mrbgems: let a build take a gem back out of a gembox
- [#7155](https://github.com/mruby/mruby/pull/7155) ci: read the byte-indexed side on full-core minus mruby-encoding
- [#7156](https://github.com/mruby/mruby/pull/7156) string.c: write `MRB_STR_SINGLE_BYTE` through the accessors it has
- [#7157](https://github.com/mruby/mruby/pull/7157) string.c: ask a string what encoding it is, not whether a bit is set
- [#7158](https://github.com/mruby/mruby/pull/7158) string.c: keep one answer about a string's bytes, not three flags
- [#7159](https://github.com/mruby/mruby/pull/7159) build: give `spec.build.defines` a reader that cannot lie
- [#7160](https://github.com/mruby/mruby/pull/7160) mruby-test: regenerate the per-gem test wrapper when a `mrbgem.rake` it is generated from changes
- [#7161](https://github.com/mruby/mruby/pull/7161) mruby-regexp: read a pattern the way the build reads a String
- [#7162](https://github.com/mruby/mruby/pull/7162) vm.c: remove the unexpanded integer case macros
- [#7163](https://github.com/mruby/mruby/pull/7163) numeric.c: make `Integer#eql?` answer false for a Float, big integers included
- [#7164](https://github.com/mruby/mruby/pull/7164) mruby-compiler: reject a failed `ftell` before sizing the input buffer
- [#7165](https://github.com/mruby/mruby/pull/7165) mruby-regexp: share the pattern-skipping rules between the two prescans
- [#7166](https://github.com/mruby/mruby/pull/7166) mruby-regexp: always allocate the capture name arena
- [#7167](https://github.com/mruby/mruby/pull/7167) test: growing a shared string detaches its buffer
- [#7168](https://github.com/mruby/mruby/pull/7168) vm.c: restore the GC arena where the Float boxing macro allocates
- [#7169](https://github.com/mruby/mruby/pull/7169) string.h: hold a string's encoding as an index, not as one bit
- [#7170](https://github.com/mruby/mruby/pull/7170) string.h: fold the coderange's three bits into a two-bit field
- [#7171](https://github.com/mruby/mruby/pull/7171) mruby-regexp, mruby-task: ask the build what it defines
- [#7172](https://github.com/mruby/mruby/pull/7172) mruby-bin-mrbc: take the target define from the compiler gem
- [#7173](https://github.com/mruby/mruby/pull/7173) string.h: give the flags word its final field order
- [#7174](https://github.com/mruby/mruby/pull/7174) mruby-sprintf: set the result's length through RSTR_SET_LEN()
- [#7175](https://github.com/mruby/mruby/pull/7175) build: hold the build's own defines in a separate list
- [#7176](https://github.com/mruby/mruby/pull/7176) amalgam: write the build's defines into the generated header
- [#7177](https://github.com/mruby/mruby/pull/7177) mruby-string-ext: read `String#casecmp`'s bytes as unsigned
- [#7178](https://github.com/mruby/mruby/pull/7178) string.h: name what makes a character index a byte index
- [#7179](https://github.com/mruby/mruby/pull/7179) internal.h: move writing a string's encoding and coderange inside
- [#7180](https://github.com/mruby/mruby/pull/7180) string.c: keep what a string reads as across a write that cannot change it
- [#7181](https://github.com/mruby/mruby/pull/7181) string.h: drop `mrb_str_modify_keep_ascii()`
- [#7182](https://github.com/mruby/mruby/pull/7182) string.c: convert case the way the string is read, not the way bytes are
- [#7183](https://github.com/mruby/mruby/pull/7183) unicase.c: keep one Unicode case table rather than two
- [#7184](https://github.com/mruby/mruby/pull/7184) presym: generate valid C when the scan finds no symbols
- [#7185](https://github.com/mruby/mruby/pull/7185) string.c: follow an ASCII run only as far as the index asks
- [#7186](https://github.com/mruby/mruby/pull/7186) string.c: compare the first byte before `memcmp()` searching backward
- [#7187](https://github.com/mruby/mruby/pull/7187) string.c: walk to a substring's range rather than count the string
- [#7188](https://github.com/mruby/mruby/pull/7188) tools: build the Unicode tables from the character database
- [#7189](https://github.com/mruby/mruby/pull/7189) mruby-string-ext: read a `String#casecmp?` operand, not a copy of it
- [#7190](https://github.com/mruby/mruby/pull/7190) string.c: let a build index by character and convert case by ASCII
- [#7191](https://github.com/mruby/mruby/pull/7191) string.c: drop a case walk's check that nothing reaches
- [#7192](https://github.com/mruby/mruby/pull/7192) tools: give the generated Unicode headers an include guard
- [#7193](https://github.com/mruby/mruby/pull/7193) Prevent switch statement warning for `MRB_TT_SET`
- [#7194](https://github.com/mruby/mruby/pull/7194) mruby-regexp: take away the MatchData constructor
- [#7195](https://github.com/mruby/mruby/pull/7195) build_config: give each host build config a name of its own
- [#7196](https://github.com/mruby/mruby/pull/7196) mruby-bigint: compare a big integer against a Float exactly
- [#7197](https://github.com/mruby/mruby/pull/7197) build_config: give the clang-asan config a name of its own
- [#7198](https://github.com/mruby/mruby/pull/7198) vm.c: honor a `[]` redefinition installed on a core class
- [#7199](https://github.com/mruby/mruby/pull/7199) build_config: give the mrbc config a name of its own
- [#7200](https://github.com/mruby/mruby/pull/7200) build_config: give the glib_hal_test config a name of its own
- [#7201](https://github.com/mruby/mruby/pull/7201) mruby-compiler: make `mrc_int` as wide as the VM's `mrb_int`
- [#7202](https://github.com/mruby/mruby/pull/7202) load.c: report a refused irep from `mrb_load_irep_cxt()`
- [#7203](https://github.com/mruby/mruby/pull/7203) build: keep the linker options of a gem that is in libmruby.a
- [#7204](https://github.com/mruby/mruby/pull/7204) mruby-compiler: report a failed irep round trip in `mrb_generate_code()`
- [#7205](https://github.com/mruby/mruby/pull/7205) mruby-compiler: check the three RITE counts the codegen does not
- [#7206](https://github.com/mruby/mruby/pull/7206) load.c: reject undersized binary_size in read_binary_header
- [#7207](https://github.com/mruby/mruby/pull/7207) mruby-regexp: hold a character class's ranges sorted and free of overlaps
- [#7208](https://github.com/mruby/mruby/pull/7208) string.c: write the case walk's answer into the buffer it is building
- [#7209](https://github.com/mruby/mruby/pull/7209) string.c: take the single-byte path for a string of nothing but ASCII
- [#7210](https://github.com/mruby/mruby/pull/7210) mruby-bigint: normalize the result of `mrb_bint_pow()`
- [#7211](https://github.com/mruby/mruby/pull/7211) numeric.c: make `Numeric#eql?` require the same type
- [#7212](https://github.com/mruby/mruby/pull/7212) mruby-bigint: let `mpz_get_int()` answer the smallest Integer
- [#7213](https://github.com/mruby/mruby/pull/7213) mruby-regexp: relocate the lookaround offsets with the code they name
- [#7214](https://github.com/mruby/mruby/pull/7214) mruby-compiler: compare integers of the same signedness
- [#7215](https://github.com/mruby/mruby/pull/7215) vm.c: answer `str[i] = x` from `OP_SETIDX`
- [#7216](https://github.com/mruby/mruby/pull/7216) numeric.c: compare an Integer with a Float without rounding either one
- [#7217](https://github.com/mruby/mruby/pull/7217) mruby-bigint: count the limbs an mrb_int needs in mpz_set_int()
- [#7218](https://github.com/mruby/mruby/pull/7218) boxing_word.h: let one flag answer whether floats are inlined
- [#7219](https://github.com/mruby/mruby/pull/7219) mruby-regexp: test a lookbehind over a class that holds a stray byte
- [#7220](https://github.com/mruby/mruby/pull/7220) build_config: let the asan config use the compiler that is present
- [#7221](https://github.com/mruby/mruby/pull/7221) mruby-compiler, mruby-bin-mruby: refuse a program file that cannot be read
- [#7222](https://github.com/mruby/mruby/pull/7222) string.c: answer an ASCII-case build from the header's fallback
- [#7223](https://github.com/mruby/mruby/pull/7223) test: ask what case conversion answers where it follows ASCII
- [#7224](https://github.com/mruby/mruby/pull/7224) string.c: keep what the bytes read as across `String#reverse!`
- [#7225](https://github.com/mruby/mruby/pull/7225) unicase.c: unfold an ASCII character without reading a table
- [#7226](https://github.com/mruby/mruby/pull/7226) string.c: ask a broken string again after `String#reverse!`
- [#7227](https://github.com/mruby/mruby/pull/7227) mruby-regexp: declare a named group as `(?'name'...)` too
- [#7228](https://github.com/mruby/mruby/pull/7228) mruby-regexp: say which `\k` reference failure it was
- [#7230](https://github.com/mruby/mruby/pull/7230) build: give a cross build's `mrbc` the target's answer on floats
- [#7231](https://github.com/mruby/mruby/pull/7231) doc: say what a build converting case by ASCII stops refusing
- [#7232](https://github.com/mruby/mruby/pull/7232) mruby-regexp: give the pattern its buffers before the compile starts
- [#7233](https://github.com/mruby/mruby/pull/7233) mruby-regexp: answer the backward search from the end of the subject
- [#7234](https://github.com/mruby/mruby/pull/7234) mruby-regexp: ask the backward search about a long multibyte subject
- [#7235](https://github.com/mruby/mruby/pull/7235) build_config: drop the stale asan.rb reference from gcc-asan.rb
- [#7236](https://github.com/mruby/mruby/pull/7236) build: rebuild an output another configuration left in the build directory
- [#7238](https://github.com/mruby/mruby/pull/7238) build: ask the build too whether a cross target compiles without floats
- [#7239](https://github.com/mruby/mruby/pull/7239) build: let a build config declare `host` after a cross build
- [#7240](https://github.com/mruby/mruby/pull/7240) build: pass over a `host` that has no `mrbc` to lend
- [#7241](https://github.com/mruby/mruby/pull/7241) mruby-compiler: read a float literal as Integer 0 under `MRB_NO_FLOAT`
- [#7243](https://github.com/mruby/mruby/pull/7243) build: give the presym preprocess the header dependencies of its object
- [#7244](https://github.com/mruby/mruby/pull/7244) build: write the archive from the objects of now
- [#7245](https://github.com/mruby/mruby/pull/7245) build_config: give the cosmopolitan config a name of its own
- [#7246](https://github.com/mruby/mruby/pull/7246) build_config: give the msvc build a name of its own
- [#7247](https://github.com/mruby/mruby/pull/7247) build_config: drop the host build from the IntelEdison config
- [#7248](https://github.com/mruby/mruby/pull/7248) build: build the Prism objects through the rules
- [#7249](https://github.com/mruby/mruby/pull/7249) mruby-bin-mirb: drop the readline and linenoise completion adapters
- [#7250](https://github.com/mruby/mruby/pull/7250) build: build the gem loader object through the rules
- [#7251](https://github.com/mruby/mruby/pull/7251) presym: skip the table of a build without libmruby
- [#7252](https://github.com/mruby/mruby/pull/7252) mruby-regexp: scope the inline x option the way i and m are scoped
- [#7253](https://github.com/mruby/mruby/pull/7253) build: build the test objects through the rules
- [#7254](https://github.com/mruby/mruby/pull/7254) build: keep a generated source that a new `mrbc` writes the same
- [#7255](https://github.com/mruby/mruby/pull/7255) build: write a generated source again when the list of its inputs changes
- [#7256](https://github.com/mruby/mruby/pull/7256) mruby-regexp: support the atomic group `(?>...)`
- [#7257](https://github.com/mruby/mruby/pull/7257) mruby-regexp: match `\Z` under the backtracking engine
- [#7258](https://github.com/mruby/mruby/pull/7258) vm.c: end `mrb_vm_exec()` with a return the retry loop never reaches
- [#7259](https://github.com/mruby/mruby/pull/7259) win32: add code page conversion helpers
- [#7260](https://github.com/mruby/mruby/pull/7260) mruby-regexp: reject a hex escape with no digit
- [#7261](https://github.com/mruby/mruby/pull/7261) mruby-string-ext: strip leading NUL in `strip` and `lstrip`
- [#7262](https://github.com/mruby/mruby/pull/7262) mruby-regexp: share one class per codepoint for /i literals
- [#7263](https://github.com/mruby/mruby/pull/7263) mruby-regexp: close a class range at the first codepoint of a `\u{...}` list
- [#7264](https://github.com/mruby/mruby/pull/7264) mruby-regexp: fold a byte-indexed subject by ASCII under a `/i` backreference
- [#7265](https://github.com/mruby/mruby/pull/7265) mruby-regexp: keep the word class inside ASCII under /i
- [#7267](https://github.com/mruby/mruby/pull/7267) mruby-regexp: follow a block that changes the receiver in `gsub`, `sub!` and `scan`
- [#7268](https://github.com/mruby/mruby/pull/7268) mruby-regexp: read a digit escape as octal when it names no group
- [#7269](https://github.com/mruby/mruby/pull/7269) mruby-regexp: stop a repetition on an empty iteration in the backtracker
- [#7270](https://github.com/mruby/mruby/pull/7270) string.c: name the ASCII option for what it narrows
- [#7271](https://github.com/mruby/mruby/pull/7271) mruby-regexp: stop looking up `MatchData`, `$~` and the `Regexp` ivars by string on every match
- [#7272](https://github.com/mruby/mruby/pull/7272) mruby-regexp: skip free-spacing whitespace in the parser and leave the pre-pass with comments and escape widths
- [#7273](https://github.com/mruby/mruby/pull/7273) mruby-regexp: undo what a lookaround captured when the match backtracks past it
- [#7274](https://github.com/mruby/mruby/pull/7274) mruby-regexp: speed up String#sub and #gsub
- [#7275](https://github.com/mruby/mruby/pull/7275) mruby-regexp: let an empty group take a quantifier
- [#7276](https://github.com/mruby/mruby/pull/7276) mruby-regexp: number atomic groups so a possessive repeat cuts as its own
- [#7278](https://github.com/mruby/mruby/pull/7278) mruby-regexp: classify a POSIX bracket by Unicode above ASCII
- [#7279](https://github.com/mruby/mruby/pull/7279) mruby-task: fix VM stack overflow when a larger proc is set on an existing task
- [#7280](https://github.com/mruby/mruby/pull/7280) mruby-regexp: raise RegexpError where a search gives up at a limit
- [#7281](https://github.com/mruby/mruby/pull/7281) mruby-regexp: publish `$~` alone and derive `$&`, `` $` ``, `$'`, `$+` and `$1` onward on read
- [#7282](https://github.com/mruby/mruby/pull/7282) mruby-regexp: read `\k<name>` in a sub or gsub replacement
- [#7283](https://github.com/mruby/mruby/pull/7283) mruby-regexp: turn `\1` through `\9` off in a replacement that names a group
- [#7284](https://github.com/mruby/mruby/pull/7284) mruby-numeric-ext: guard the bigint arm of `Integer#remainder` under `MRB_NO_FLOAT`
- [#7285](https://github.com/mruby/mruby/pull/7285) mruby-rational: build under MRB_NO_FLOAT with MRB_USE_BIGINT, and compare bigint-backed Rationals exactly
- [#7286](https://github.com/mruby/mruby/pull/7286) Simplify some control flow in `vm.c`
- [#7287](https://github.com/mruby/mruby/pull/7287) load.c: bounds-check length fields in debug/lv section readers
- [#7288](https://github.com/mruby/mruby/pull/7288) mruby-string-ext: carry `String#succ` across characters and step the Unicode letters and digits
- [#7289](https://github.com/mruby/mruby/pull/7289) mruby-regexp: take every search's capture buffer from the stack
- [#7291](https://github.com/mruby/mruby/pull/7291) unify implementations of `mrb_ary_unshift*`
- [#7294](https://github.com/mruby/mruby/pull/7294) Remove unused functions from mruby-eval
- [#7295](https://github.com/mruby/mruby/pull/7295) Added the internal function `mrb_args_pack_positional()`
- [#7296](https://github.com/mruby/mruby/pull/7296) mruby-regexp: cover `a{n}?` as an optional wrapper around the interval
- [#7297](https://github.com/mruby/mruby/pull/7297) mruby-regexp: read an option group that names no letter
- [#7298](https://github.com/mruby/mruby/pull/7298) mruby-regexp: refuse a group name that is a number or holds a `)`
- [#7299](https://github.com/mruby/mruby/pull/7299) mruby-regexp: cover the interval whose braces hold free-spacing whitespace
- [#7300](https://github.com/mruby/mruby/pull/7300) mruby-regexp: raise when a repeat range's upper bound is below its lower
- [#7301](https://github.com/mruby/mruby/pull/7301) mruby-process: add a Process module over a process HAL
- [#7302](https://github.com/mruby/mruby/pull/7302) mruby-regexp: fail a backreference to a group that has not closed
- [#7303](https://github.com/mruby/mruby/pull/7303) mruby-io: let go of the write descriptor IO#close_write closed
- [#7304](https://github.com/mruby/mruby/pull/7304) mruby-process: separate two sentences with a single space
- [#7305](https://github.com/mruby/mruby/pull/7305) vm.c: make stack_extend_alloc cover the frame offset
- [#7306](https://github.com/mruby/mruby/pull/7306) build_config: make the MinGW wine test config build and pass its tests
- [#7307](https://github.com/mruby/mruby/pull/7307) mruby-regexp: move the backtracking stack off the C stack
- [#7308](https://github.com/mruby/mruby/pull/7308) mruby-task: size the proc_set stack from the task's frame
- [#7309](https://github.com/mruby/mruby/pull/7309) mruby-task: mark the main task's values during GC
- [#7310](https://github.com/mruby/mruby/pull/7310) mruby-regexp: give back what a refused search took, and stop the capture pool growing with the subject
- [#7311](https://github.com/mruby/mruby/pull/7311) mruby-regexp: take a MatchData in one block, into an object that owns it
- [#7312](https://github.com/mruby/mruby/pull/7312) gc.c (gc_mark_children): drop the duplicate method table mark
- [#7313](https://github.com/mruby/mruby/pull/7313) mruby-io: leave the stream not open for writing after IO#close_write
- [#7314](https://github.com/mruby/mruby/pull/7314) mruby-io: define IO.pipe where the port has one
- [#7315](https://github.com/mruby/mruby/pull/7315) mruby-io: name the errors these tests are about
- [#7316](https://github.com/mruby/mruby/pull/7316) mruby-process: wait for a child under the names Ruby has for it
- [#7317](https://github.com/mruby/mruby/pull/7317) build_config: name the Windows port in the MinGW config
- [#7318](https://github.com/mruby/mruby/pull/7318) build_config: rewrite only the `z:` that is a path
- [#7319](https://github.com/mruby/mruby/pull/7319) build_config: collect Wine output through files
- [#7320](https://github.com/mruby/mruby/pull/7320) mruby-io: run these Windows-skipped tests on Windows
- [#7321](https://github.com/mruby/mruby/pull/7321) headers: unify C linkage guards on MRB_BEGIN_DECL
- [#7322](https://github.com/mruby/mruby/pull/7322) mruby-rational: cross-multiply exactly in rational_eq()'s overflow fallback
- [#7323](https://github.com/mruby/mruby/pull/7323) mruby-rational: demote a reduced Rational's halves that fit `mrb_int`
- [#7324](https://github.com/mruby/mruby/pull/7324) object.c: gate only the Float arm of `mrb_ensure_integer_type()` behind `MRB_NO_FLOAT`
- [#7325](https://github.com/mruby/mruby/pull/7325) mruby-io: close the child's stream where IO#close_write has no write end
- [#7326](https://github.com/mruby/mruby/pull/7326) test: cover three untested behaviours of Unicode case conversion
- [#7327](https://github.com/mruby/mruby/pull/7327) class.c: fix mrb_mod_visibility()'s copy into a prepended class's method table
- [#7328](https://github.com/mruby/mruby/pull/7328) mruby-regexp: append the flag suffix to a RegexpError message
- [#7329](https://github.com/mruby/mruby/pull/7329) symbol.c: mark the globals, a fiber's stack and a Range's ends
- [#7330](https://github.com/mruby/mruby/pull/7330) mruby-regexp: let a refused capture block be `backtrack_exec()`'s answer
- [#7331](https://github.com/mruby/mruby/pull/7331) build_config: let the program under test have the runner's stdin
- [#7332](https://github.com/mruby/mruby/pull/7332) ci: give the mingw jobs timeout headroom
- [#7333](https://github.com/mruby/mruby/pull/7333) mruby-io: let a closed stream be closed again
- [#7334](https://github.com/mruby/mruby/pull/7334) class.c: check frozen before mrb_define_method_raw() walks to the origin
- [#7335](https://github.com/mruby/mruby/pull/7335) promote `ary_unshift_values` to `MRB_API`
- [#7336](https://github.com/mruby/mruby/pull/7336) gc.c: let byte pressure advance an in-progress collection cycle
- [#7337](https://github.com/mruby/mruby/pull/7337) string.c: let growth at the end of a shared string append in place
- [#7338](https://github.com/mruby/mruby/pull/7338) gc.c: schedule collection on malloc growth by default
- [#7339](https://github.com/mruby/mruby/pull/7339) gc.h, mruby-task: say what mrb_gc_scheduler_pending() actually tests
- [#7340](https://github.com/mruby/mruby/pull/7340) string, array, hash: check a frozen receiver where a destructive call writes nothing
- [#7341](https://github.com/mruby/mruby/pull/7341) mruby-compiler: report a source file that cannot be read
- [#7342](https://github.com/mruby/mruby/pull/7342) mruby-bin-mrb, mruby-bin-mruby: fail when a -r library does not load
- [#7343](https://github.com/mruby/mruby/pull/7343) mruby-bin-mirb, mruby-bin-debugger: refuse a source file that cannot be read
- [#7344](https://github.com/mruby/mruby/pull/7344) test: invoke the tools in the bintests without a shell, and report one that does not run
- [#7345](https://github.com/mruby/mruby/pull/7345) mruby-regexp: check the engine against CRuby over a pattern corpus
- [#7347](https://github.com/mruby/mruby/pull/7347) string.c: make each write answer for what the bytes it leaves read as
- [#7348](https://github.com/mruby/mruby/pull/7348) numeric.c: answer a comparison against a NaN as unordered
- [#7349](https://github.com/mruby/mruby/pull/7349) vm.c: answer `==` against a NaN by value, not by representation
- [#7350](https://github.com/mruby/mruby/pull/7350) range.c: refuse a range bounded by a NaN
- [#7351](https://github.com/mruby/mruby/pull/7351) object.c: skip the send when `eql?` is the identity
- [#7352](https://github.com/mruby/mruby/pull/7352) mruby-array-ext: compare with `eql?` on the linear set-operation paths
- [#7353](https://github.com/mruby/mruby/pull/7353) enum.rb: ask for the block once rather than once an element
- [#7355](https://github.com/mruby/mruby/pull/7355) mruby-compiler: give an assignment to a captured local its value
- [#7356](https://github.com/mruby/mruby/pull/7356) fp_uscale.c: measure the shortest round trip against `mrb_float`
- [#7357](https://github.com/mruby/mruby/pull/7357) test: state the sprintf and bigint float expectations for the build's own width
- [#7359](https://github.com/mruby/mruby/pull/7359) array.c: take an `Array` element for equal to itself in `#==`
- [#7360](https://github.com/mruby/mruby/pull/7360) mruby-array-ext: walk an Array in C for `include?` and `member?`
- [#7361](https://github.com/mruby/mruby/pull/7361) mruby-enum-ext: count an Array in C
- [#7362](https://github.com/mruby/mruby/pull/7362) object.c: compare what a Float holds bit for bit under `MRB_NO_BOXING`
- [#7363](https://github.com/mruby/mruby/pull/7363) etc.c: give every NaN an identity of its own
- [#7364](https://github.com/mruby/mruby/pull/7364) version.c: define MRUBY_REVISION from the source repository
- [#7365](https://github.com/mruby/mruby/pull/7365) mruby-regexp: keep a Regexp's source and compiled pattern in step
- [#7366](https://github.com/mruby/mruby/pull/7366) mruby-regexp: give an inline toggle the alternatives after it
- [#7367](https://github.com/mruby/mruby/pull/7367) mruby-regexp: look the match up in a Hash replacement
- [#7368](https://github.com/mruby/mruby/pull/7368) mruby-array-ext: narrow `Array#intersection` by every argument
- [#7369](https://github.com/mruby/mruby/pull/7369) array.c: take a comparison block's answer in every form it has
- [#7370](https://github.com/mruby/mruby/pull/7370) mruby-regexp: fold a leading option group into what `Regexp#to_s` prints
- [#7371](https://github.com/mruby/mruby/pull/7371) mruby-array-ext: build `-` a set only when both sides can repay it
- [#7372](https://github.com/mruby/mruby/pull/7372) mruby-array-ext: write `uniq!`, `|` and `&` once against `ary_memb`, with the threshold at 8
- [#7373](https://github.com/mruby/mruby/pull/7373) mruby-array-ext: write `-` and `intersect?` once, against a membership oracle
- [#7374](https://github.com/mruby/mruby/pull/7374) build: keep the paths of the build out of what it compiles
- [#7375](https://github.com/mruby/mruby/pull/7375) build: write the same archive from the same objects
- [#7376](https://github.com/mruby/mruby/pull/7376) mruby-regexp: let `\k<n>` name a group written after it
- [#7377](https://github.com/mruby/mruby/pull/7377) mruby-regexp: range-check `\k<-n>` before refusing it as numbered
- [#7378](https://github.com/mruby/mruby/pull/7378) mruby-signal: add a `Signal` module and retire `Process::Status._signame`
- [#7379](https://github.com/mruby/mruby/pull/7379) doc: list `ENV` in the standard library tables
- [#7380](https://github.com/mruby/mruby/pull/7380) mruby-process: refuse a non-signal by its class
- [#7381](https://github.com/mruby/mruby/pull/7381) compar.rb: order through `<=>` in `between?` and the `_by` family
- [#7382](https://github.com/mruby/mruby/pull/7382) mruby-rational: guard the tests a narrow `mrb_int` cannot answer
- [#7383](https://github.com/mruby/mruby/pull/7383) numeric.rb: negate through a product so `-x` can reach a `-0.0`
- [#7384](https://github.com/mruby/mruby/pull/7384) mruby-regexp: refuse the nest level a `\k` reference may carry
- [#7386](https://github.com/mruby/mruby/pull/7386) mruby-complex: take the `to_s` separator from the rendered part
- [#7387](https://github.com/mruby/mruby/pull/7387) mruby-complex: hold the exact parts the numeric tower hands over
- [#7388](https://github.com/mruby/mruby/pull/7388) mruby-regexp: implement `\g` subexpression calls
- [#7390](https://github.com/mruby/mruby/pull/7390) mruby-compiler: avoid pushing discarded local assignment values
- [#7391](https://github.com/mruby/mruby/pull/7391) mruby-process: file the `wait` row with the implemented methods
- [#7392](https://github.com/mruby/mruby/pull/7392) clangd: read the tree without a compile_commands.json
- [#7393](https://github.com/mruby/mruby/pull/7393) mruby-regexp: move the String and Symbol overrides from mrblib to C
- [#7394](https://github.com/mruby/mruby/pull/7394) mruby-regexp: tell an omitted `last_match` argument from an explicit nil
- [#7395](https://github.com/mruby/mruby/pull/7395) build: write a compile_commands.json from the records of the compiles
- [#7396](https://github.com/mruby/mruby/pull/7396) string: snapshot shared append sources
- [#7397](https://github.com/mruby/mruby/pull/7397) kernel.c: negate `=~` from C rather than from a Ruby frame of its own
- [#7398](https://github.com/mruby/mruby/pull/7398) mruby-symbol-ext: slice a symbol's name from C rather than from a Ruby frame of its own
- [#7399](https://github.com/mruby/mruby/pull/7399) mruby-process: read a `Process::Status` subclass as a status in `#==`
- [#7400](https://github.com/mruby/mruby/pull/7400) string: copy a static string before checking its terminator
- [#7401](https://github.com/mruby/mruby/pull/7401) mruby-regexp: read a mutual recursion from the body that ends
- [#7405](https://github.com/mruby/mruby/pull/7405) mruby-regexp: collapse repeated capture-free assertions
- [#7406](https://github.com/mruby/mruby/pull/7406) kernel.c: answer `respond_to?` false for a method unimplemented here
- [#7407](https://github.com/mruby/mruby/pull/7407) class.c: raise from `mrb_notimplement()` even with no method name
- [#7416](https://github.com/mruby/mruby/pull/7416) mruby-process: name the signal in a `kill` refusal past the lookup width
- [#7419](https://github.com/mruby/mruby/pull/7419) numeric.c: round an Integer to a negative number of digits correctly
- [#7421](https://github.com/mruby/mruby/pull/7421) mruby-process: add `Process.clock_gettime` and `Process.clock_getres`
- [#7427](https://github.com/mruby/mruby/pull/7427) mruby-regexp: add MatchData#values_at
- [#7430](https://github.com/mruby/mruby/pull/7430) mruby-struct: let a bare Struct.new answer a memberless struct
- [#7431](https://github.com/mruby/mruby/pull/7431) mruby-struct: raise the ordinary arity error for a keyword_init struct
- [#7432](https://github.com/mruby/mruby/pull/7432) build: ask the compiler whether a header is there
- [#7433](https://github.com/mruby/mruby/pull/7433) mruby-dir: drop a guard that has never let its header in
- [#7434](https://github.com/mruby/mruby/pull/7434) mruby-regexp: add `MatchData#offset`
- [#7436](https://github.com/mruby/mruby/pull/7436) mruby-struct: add Struct.keyword_init? and follow CRuby keyword argument handling
- [#7437](https://github.com/mruby/mruby/pull/7437) mruby-regexp: slice `MatchData#[]` with a start and length or a Range
- [#7438](https://github.com/mruby/mruby/pull/7438) mruby-process, mruby-task: one definition of NSEC_PER_SEC
- [#7439](https://github.com/mruby/mruby/pull/7439) build: compile by the names the sources have from the tree
- [#7440](https://github.com/mruby/mruby/pull/7440) mruby-regexp: combine patterns with `Regexp.union`
- [#7441](https://github.com/mruby/mruby/pull/7441) mruby-compiler: make Prism `#line` directives resolve from the tree
- [#7442](https://github.com/mruby/mruby/pull/7442) mruby-regexp: scan for first-byte candidates with memchr
- [#7443](https://github.com/mruby/mruby/pull/7443) ci: split each job's build and test into separate steps
- [#7444](https://github.com/mruby/mruby/pull/7444) appveyor: fetch the queued commit instead of the branch tip
- [#7445](https://github.com/mruby/mruby/pull/7445) mruby-process: trim README prose duplicated in source comments
- [#7446](https://github.com/mruby/mruby/pull/7446) mruby-regexp: report every group of a duplicate name in `named_captures`
- [#7447](https://github.com/mruby/mruby/pull/7447) mruby-process: add Process.times and Process::Tms
- [#7448](https://github.com/mruby/mruby/pull/7448) mruby-regexp: scan only the positions an anchored pattern can start at
- [#7449](https://github.com/mruby/mruby/pull/7449) mruby-struct: take a C fast path through `.new` for the default `initialize`
- [#7450](https://github.com/mruby/mruby/pull/7450) build: write a size.json of every build's artifacts
- [#7451](https://github.com/mruby/mruby/pull/7451) mruby-regexp: show the groups in `MatchData#inspect`
- [#7452](https://github.com/mruby/mruby/pull/7452) mruby-benchmark: measure CPU time through `Process.times`
- [#7453](https://github.com/mruby/mruby/pull/7453) build: log where every define came from as a build starts
- [#7454](https://github.com/mruby/mruby/pull/7454) ci: add a build without Float and one without bigint
- [#7455](https://github.com/mruby/mruby/pull/7455) string.c: start the byte-at-a-time tail where the word loop stopped
- [#7456](https://github.com/mruby/mruby/pull/7456) mruby-regexp: give each lookbehind branch its own rewind
- [#7457](https://github.com/mruby/mruby/pull/7457) mruby-process: work until the CPU time reading moves
- [#7458](https://github.com/mruby/mruby/pull/7458) mruby-regexp: scan for both ends of a literal prefix
- [#7459](https://github.com/mruby/mruby/pull/7459) amalgam: emit the baked defines ahead of the system includes
- [#7460](https://github.com/mruby/mruby/pull/7460) ci: check out Prism with the source and log each build's defines
- [#7461](https://github.com/mruby/mruby/pull/7461) ci: bound the cross-toolchain installs at 5 minutes
- [#7463](https://github.com/mruby/mruby/pull/7463) mruby-compiler: exclude arm64 from mingw64 builtin setjmp/longjmp
- [#7464](https://github.com/mruby/mruby/pull/7464) mruby-regexp: implement the absent repeater `(?~...)`
- [#7466](https://github.com/mruby/mruby/pull/7466) mruby-regexp: read the subject in one place
- [#7467](https://github.com/mruby/mruby/pull/7467) mruby-regexp: implement the conditional `(?(cond)yes|no)`
- [#7468](https://github.com/mruby/mruby/pull/7468) mruby-regexp: read the subject before the pattern in every search
- [#7469](https://github.com/mruby/mruby/pull/7469) mruby-regexp: read a nested character class as the union it is
