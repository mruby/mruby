# mruby-bin-mrbc

mrbc is the mruby compiler that compiles Ruby source files into bytecode.

## Usage

```
mrbc [switches] programfile...
```

### Options

- `-c` - check syntax only
- `-o<outfile>` - place the output into `<outfile>`; required for multi-files
- `-v` - print version number, then turn on verbose mode
- `-g` - produce debugging information
- `-B<symbol>` - binary `<symbol>` output in C language format
- `-S` - dump C struct (requires `-B`)
- `-s` - define `<symbol>` as static variable
- `--remove-lv` - remove local variables
- `--no-ext-ops` - prohibit using OP_EXTs
- `--no-optimize` - disable peephole optimization
- `--verbose` - run at verbose mode
- `--version` - print the version
- `--copyright` - print the copyright

## Examples

```bash
# Compile a Ruby script to bytecode
mrbc script.rb
# Creates script.mrb

# Specify output file
mrbc -o output.mrb script.rb

# Compile multiple files into one
mrbc -o combined.mrb file1.rb file2.rb

# Generate C source with symbol name
mrbc -Bscript_bytecode script.rb
# Creates script.c with const uint8_t script_bytecode[]

# Check syntax only
mrbc -c script.rb

# Compile with debug information
mrbc -g script.rb
```

## Output Formats

- `.mrb` - RiteBinary format (default), executable by `mruby -b`
- `.c` - C source file (with `-B` option), for embedding in C programs

## License

MIT License - see the mruby LICENSE file.
