# mruby-bin-mruby

mruby is the main interpreter for executing Ruby scripts with mruby.

## Usage

```
mruby [switches] [programfile] [arguments]
```

### Options

- `-b` - load and execute RiteBinary (mrb) file
- `-c` - check syntax only
- `-d` - set debugging flags (set `$DEBUG` to true)
- `-e 'command'` - one line of script
- `-r library` - load the library before executing your script
- `-v` - print version number, then run in verbose mode
- `--verbose` - run in verbose mode
- `--version` - print the version
- `--copyright` - print the copyright

## Examples

```bash
# Execute a Ruby script
mruby script.rb

# Execute inline code
mruby -e 'puts "Hello, mruby!"'

# Check syntax without executing
mruby -c script.rb

# Execute a compiled binary
mruby -b script.mrb

# Load a library before running script
mruby -r mruby-io script.rb

# Pass arguments to script
mruby script.rb arg1 arg2
```

## License

MIT License - see the mruby LICENSE file.
