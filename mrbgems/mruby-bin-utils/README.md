# mruby-bin-utils

This is a command line program that serves as a wrapper to compile and link source code requiring libmruby.

The name is a play on GNU Binutils.

## How to build and use it

Add this `mruby-bin-utils` to your build configuration file.

```ruby
# in your build configuration file

MRuby::Build.new do |conf|
  ...
  conf.gem core: "mruby-bin-utils"
  ...
end
```

Building with this configuration will generate several executable programs.
To each of those programs, you can add switches depending on the corresponding source code and compiler.

For example, `mruby-cc` takes C source code and generates an executable file.
If the toolchain used to build mruby is "gcc", you can use the gcc switches, and if the toolchain is "visualcpp", you can use the VisualC++ switches.

For mruby built with the gcc toolchain, you can use `mruby-cc` as follows

```console
$ bin/mruby-cc yourcode.c               # Compile to generate a.out
$ bin/mruby-cc -o exec yourcode.c       # Compile to generate exec
$ bin/mruby-cc -o exec yourcode.c -lzip # Compile and link with libzip to generate exec
```

## Generated programs and corresponding files

| Program name      | File type
| ----------------- | --------
| `bin/mruby-cc`    | C source file
| `bin/mruby-c++`   | C++ source file
| `bin/mruby-as`    | Assembly source file
| `bin/mruby-objc`  | Objective-C source file
| `bin/mruby-ld`    | Object file
