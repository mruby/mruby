# mruby-bin-config

mruby-config outputs the configuration used to build mruby, useful for compiling C extensions.

## Usage

```
mruby-config [switches]
```

### Options

- `--cc` - print C compiler name
- `--cflags` - print flags passed to C compiler
- `--cxx` - print C++ compiler name
- `--cxxflags` - print flags passed to C++ compiler
- `--as` - print assembler name
- `--asflags` - print flags passed to assembler
- `--objc` - print Objective C compiler name
- `--objcflags` - print flags passed to Objective C compiler
- `--ld` - print linker name
- `--ldflags` - print flags passed to linker
- `--ldflags-before-libs` - print flags passed to linker before linked libraries
- `--libs` - print linked libraries
- `--libmruby-path` - print libmruby path
- `--help` - print help

## Examples

```bash
# Get C compiler
mruby-config --cc

# Get compiler flags for building extensions
mruby-config --cflags

# Compile a C extension
gcc $(mruby-config --cflags) -c extension.c

# Link with mruby
gcc extension.o $(mruby-config --ldflags) $(mruby-config --libs)
```

## License

MIT License - see the mruby LICENSE file.
