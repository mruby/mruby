# mruby-bin-mirb-prism

mirb-prism (mruby interactive) is an interactive Ruby shell for mruby, using the Prism compiler.

## Usage

### Options

- `-v` - print version and exit
- `-d` - set `$DEBUG` to true
- `-r library` - load the library before executing
- `--verbose` - verbose mode

## Tab Completion

mirb supports context-aware tab completion in its built-in multi-line editor.
The editor is used when standard input is a terminal; no external line-editing
library is required or consulted.

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

### Behavior

- A single match is inserted in place of the typed prefix
- Multiple matches sharing a longer common prefix extend the input to that prefix
- Otherwise the candidates are listed below the input line

### Notes

- Completion evaluates receiver expressions to determine available methods
- Only simple receivers (variable names, constants) are evaluated for safety
- Complex expressions like `obj.method().` are not completed to avoid side effects
- File path completion in `require`/`load` statements is planned for future versions

## License

MIT License - see the mruby LICENSE file.
