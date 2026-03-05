# mruby-compiler

This mrbgem provides the mruby compiler, which is responsible for parsing Ruby
code and generating mruby bytecode.

## Functionality

The `mruby-compiler` gem includes the following components:

- **Parser:** Translates Ruby source code into an abstract syntax tree (AST).
- **Code Generator:** Traverses the AST to produce executable mruby bytecode.
- **`mrbc` executable:** A command-line tool for compiling `.rb` files into
  `.mrb` bytecode files.

## Usage

The `mrbc` (mruby-bin-mrbc) executable will generate compiled binary from Ruby
programs via this gem.

Example of using `mrbc`:

```sh
# Compile a Ruby script to bytecode
bin/mrbc my_script.rb

# Run the compiled script
bin/mruby my_script.mrb
```
