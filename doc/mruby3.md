User visible changes in `mruby3`
===

= Build System

You can specify `TARGET` option to `rake` via a command line
option, or via an environment variable, e.g.

`rake TARGET=host all`

or

`TARGET=host rake all`

It's much easier to switch multiple targets than the
previous `build_config.rb` system.

== `presym` target

The first compilation of `mruby` may require generation of a
static symbol table named `build/presym`. You can generate
the table by `rake gensym`.

== `target` directory

Build target specification files are loaded from `target`
directory. The default `TARGET` is `host` which is described
in `target/host.rb`. There are many other targets for example:

* `host-gprof`: compiles with `gprof` for performance tuning
* `host-m32`: compiles in gcc 32bit mode on 64bit platforms
* `boxing`: compiles all three boxing options
* `clang-asan`: compiles with `clang`'s Address Sanitizer

`target/host.rb` comes  with comments to help  writing a new
target description.

If you want to have your target description out of the
source tree, you can specify the path to the description
file in `MRUBY_CONFIG`.

= Build Target Contribution

When you write a new target description, please
contribute. We welcome your contribution as a GitHub
pull-request.

= Languge Changes

== New Syntax

We have ported some new syntax from CRuby.

* R-assignment (`12 => x`)
* Numbered block parameter (`x.map{_1 * 2}`)
* End-less `def` (`def double(x) = x*2`)

= Configuration Options Changed

Some configuration macro names are changed for consistency

== `MRB_NO_FLOAT`

Changed from `MRB_WITHOUT_FLOAT` to conform `USE_XXX` naming
convention.

== `MRB_USE_FLOAT32`

Changed from `MRB_USE_FLOAT` to make sure `float` here means
using single precision float, and not the opposite of
`MRB_NO_FLOAT`.

== `MRB_USE_METHOD_T_STRUCT`

Changed from `MRB_METHOD_T_STRUCT`.

To use `struct` version of `mrb_method_t`. More portable but consumes more memory.
Turned on by default on 32bit platforms.

== `MRB_NO_BOXING`

Uses `struct` to represent `mrb_value`. Consumes more memory
but easier to inveticate the internal and to debug. It used
to be default `mrb_value` representation. Now the default is
`MRB_WORD_BOXING`.

== `MRB_WORD_BOXING`

Pack `mrb_value` in an `intptr_t` integer. Consumes less
memory compared to `MRB_NO_BOXING` especially on 32 bit
platforms. `Fixnum` size is 31 bits so some integer values
does not fit in `Fixnum` integers.

== `MRB_NAN_BOXING`

Pack `mrb_value` in a floating pointer number. Nothing
changed from previous versions.

== `MRB_USE_MALLOC_TRIM`

Call `malloc_trim(0)` from mrb_full_gc() if this macro is defined.
If you are using glibc malloc, this macro could reduce memory consumption.

= Internal Changes

== `Random` now use `xoshiro128++`.

For better and faster random number generation.
