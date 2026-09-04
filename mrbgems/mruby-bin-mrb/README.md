# mruby-bin-mrb

`mrb` is a lightweight mruby runtime that executes only precompiled
RiteBinary (.mrb) files. Unlike the full `mruby` command, it does not
depend on `mruby-compiler`, resulting in a significantly smaller binary.

This gem is intended for embedded deployments where Ruby scripts are
precompiled on a development machine and only the runtime is needed on
the target device.

## Size comparison

By excluding `mruby-compiler` (and gems that depend on it such as
`mruby-eval`, `mruby-binding`, and `mruby-bin-mirb`), the text segment
can be reduced by approximately 300KB or more, depending on the build
configuration.

## Usage

```
mrb [switches] programfile.mrb [arguments]
```

### Options

- `-d` - set debugging flags (set `$DEBUG` to true)
- `-r library` - load a library (.mrb) before executing your script
- `-v` - print version number, then run in verbose mode
- `--verbose` - run in verbose mode
- `--version` - print the version
- `--copyright` - print the copyright

## Workflow

```bash
# On the development machine (with full mruby + mrbc):
mrbc -o program.mrb program.rb

# On the target device (with mrb only):
mrb program.mrb

# Load a precompiled library before the main program:
mrb -r lib.mrb program.mrb

# Pass arguments to the script:
mrb program.mrb arg1 arg2
```

## Build configuration example

To build a minimal mruby with only the `mrb` runtime:

```ruby
# build_config/runtime.rb
MRuby::Build.new do |conf|
  conf.toolchain

  # Use a gembox that does not pull in the compiler.
  # For example, the default gembox does not require it.
  conf.gembox 'default'

  # The runtime-only executor (no compiler dependency)
  conf.gem :core => 'mruby-bin-mrb'

  # Do NOT include these (they require mruby-compiler):
  #   conf.gem :core => 'mruby-bin-mruby'
  #   conf.gem :core => 'mruby-bin-mirb'
  #   conf.gem :core => 'mruby-eval'
  #   conf.gem :core => 'mruby-binding'
end
```

## Differences from `mruby` command

| Feature                 | `mruby` | `mrb`                      |
| ----------------------- | ------- | -------------------------- |
| Execute .rb files       | yes     | no                         |
| Execute .mrb files      | yes     | yes                        |
| `-e` inline code        | yes     | no                         |
| `-c` syntax check       | yes     | no                         |
| `-b` force binary mode  | yes     | not needed (always binary) |
| Requires mruby-compiler | yes     | **no**                     |

## License

MIT License - see the mruby LICENSE file.
