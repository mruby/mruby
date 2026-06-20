# @picoruby/picorbc

Cross-platform PicoRuby Compiler (picorbc) - Compile Ruby scripts to mruby bytecode (.mrb files)

## Installation

```bash
npm install -g @picoruby/picorbc
```

## Usage

```bash
picorbc script.rb
```

This will generate `script.mrb` in the same directory.

### Options

```bash
picorbc [switches] programfile...

switches:
  -c           check syntax only
  -o<outfile>  place the output into <outfile>; required for multi-files
  -v           print version number, then turn on verbose mode
  -g           produce debugging information
  -B<symbol>   binary <symbol> output in C language format
  -S           dump C struct (requires -B)
  -s           define <symbol> as static variable
  --remove-lv  remove local variables
  --no-ext-ops prohibit using OP_EXTs
  --no-optimize disable peephole optimization
  --verbose    run at verbose mode
  --version    print the version
  --copyright  print the copyright
```

## Examples

```bash
# Compile a single Ruby file
picorbc hello.rb

# Specify output file
picorbc -o output.mrb script.rb

# Compile multiple files
picorbc -o combined.mrb file1.rb file2.rb file3.rb

# Check syntax only
picorbc -c script.rb

# Verbose mode
picorbc -v script.rb
```

## About PicoRuby

PicoRuby is an mruby ecosystem which is:
- Lightweight and memory efficient
- Suitable for embedded systems like microcontrollers and browser
- Compatible with mruby bytecode

Learn more at [https://github.com/picoruby/picoruby](https://github.com/picoruby/picoruby)

## License

MIT
