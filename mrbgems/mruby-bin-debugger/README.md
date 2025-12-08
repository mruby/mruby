# mruby-bin-debugger

mrdb is the mruby debugger for debugging Ruby scripts.

## Usage

```
mrdb [switches] programfile
```

### Options

- `-b` - load and execute RiteBinary (mrb) file
- `-d` - specify source directory
- `--version` - print the version
- `--copyright` - print the copyright

## Debugger Commands

| Command            | Abbreviation | Description                  |
| ------------------ | ------------ | ---------------------------- |
| `break`            | `b`          | Set a breakpoint             |
| `continue`         | `c`          | Continue execution           |
| `delete`           | `d`          | Delete breakpoints           |
| `disable`          | `dis`        | Disable breakpoints          |
| `enable`           | `en`         | Enable breakpoints           |
| `eval`             | `ev`         | Evaluate expression          |
| `help`             | `h`          | Show help                    |
| `info breakpoints` | `i b`        | Show breakpoint information  |
| `info locals`      | `i l`        | Show local variables         |
| `list`             | `l`          | List source code             |
| `print`            | `p`          | Print expression value       |
| `quit`             | `q`          | Quit debugger                |
| `run`              | `r`          | Run program                  |
| `step`             | `s`          | Step into                    |
| `next`             | `n`          | Step over                    |

## Examples

```bash
# Start debugging a script
mrdb script.rb

# Debug a compiled binary with source directory
mrdb -b -d /path/to/source script.mrb
```

### Debugging Session Example

```
$ mrdb script.rb
(mrdb) b 10           # Set breakpoint at line 10
(mrdb) r              # Run the program
(mrdb) p variable     # Print variable value
(mrdb) n              # Step to next line
(mrdb) i l            # Show local variables
(mrdb) c              # Continue execution
(mrdb) q              # Quit
```

## License

MIT License - see the mruby LICENSE file.
