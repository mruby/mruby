# mruby-bin-mirb

mirb (mruby interactive) is an interactive Ruby shell for mruby.

## Usage

```
mirb [options]
```

### Options

- `-v` - print version and exit
- `-d` - set `$DEBUG` to true
- `-r library` - load the library before executing
- `--verbose` - verbose mode

## Tab Completion

mirb supports context-aware tab completion when built with a readline library.

### Supported Completions

- **Methods on objects**: Type an expression followed by `.` and press Tab

  ```
  > "hello".up<Tab>
  upcase  upcase!  upto
  ```

- **Local variables**: Variables defined in the session

  ```
  > my_var = 123
  > my<Tab>
  my_var
  ```

- **Global variables**: Press Tab after `$`

  ```
  > $std<Tab>
  $stdout  $stderr  $stdin
  ```

- **Constants and classes**: Capital letter followed by Tab

  ```
  > Str<Tab>
  String  Struct
  ```

- **Ruby keywords**: At the start of expressions

  ```
  > cla<Tab>
  class
  ```

### Readline Library Support

Tab completion works with:

- **GNU readline** (default on Linux)
- **libedit** (default on macOS/BSD)
- **linenoise** (lightweight alternative)

### Configuration

The readline library can be configured via the `MRUBY_MIRB_READLINE` environment variable:

```bash
# Auto-detect (default)
rake

# Force specific library
MRUBY_MIRB_READLINE=readline rake    # GNU readline
MRUBY_MIRB_READLINE=libedit rake     # libedit
MRUBY_MIRB_READLINE=linenoise rake   # linenoise

# Disable readline (plain input mode)
MRUBY_MIRB_READLINE=none rake
```

### Notes

- Completion evaluates receiver expressions to determine available methods
- Only simple receivers (variable names, constants) are evaluated for safety
- Complex expressions like `obj.method().` are not completed to avoid side effects
- File path completion in `require`/`load` statements is planned for future versions

## License

MIT License - see the mruby LICENSE file.
