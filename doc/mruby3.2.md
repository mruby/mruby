# User visible changes in `mruby3.2` from `mruby3.1`

# The language

* Now `a::B = c` should evaluate `a` then `c`.
* Anonymous arguments `*`, `**`, `&` can be passed for forwarding.

# Tools

## `mruby`

* `-b` only specifies the script is the binary. The files loaded by `-r` are not affected by the option.
* `mruby` now loads complied binary if the suffix is `.mrb`.

## `mrbc`

* Add `--no-optimize` option to disable optimization.

# mrbgems

## mruby-errno

* `mruby-errno` gem is now bundled.

## mruby-class-ext

* Add `Class#subclasses` method.
* Add `Module#undefined_instance_methods` method.
