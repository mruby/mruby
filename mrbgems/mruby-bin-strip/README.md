# mruby-bin-strip

mruby-strip removes debug information from compiled mruby bytecode files to reduce file size.

## Usage

```
mruby-strip [switches] irepfiles
```

### Options

- `-l, --lvar` - remove LVAR section too (local variable names)

## Examples

```bash
# Strip debug info from a compiled file
mruby-strip script.mrb

# Strip debug info and local variable names
mruby-strip -l script.mrb
mruby-strip --lvar script.mrb

# Strip multiple files
mruby-strip file1.mrb file2.mrb file3.mrb
```

## Notes

- The input files are modified in-place
- Stripping removes debug symbols used by the debugger (mrdb)
- The `-l` option additionally removes local variable names, which are needed for some reflection features
- Stripped files will still execute correctly but cannot be debugged effectively

## License

MIT License - see the mruby LICENSE file.
