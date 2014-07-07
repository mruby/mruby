# mruby/compile.h

## struct mrbc_context
mruby compiler context.
Local variables and line number are inherited in same context.

### Fields of struct mrbc_context.
Only documenting fields that user can use.
* `short lineno = 0;`
  * Current line number of compiler context.
  * If this is set before parsing starts it would be the initial line number.
* `void *partial_data = NULL;`
  * User data of partial hook.
  * Will be set with partial hook in `mrbc_partial_hook`.
* `struct RClass *target_class = NULL;`
  * Target class when loading compiled script.
  * Used in `mrb_load_*` functions.
* `mrb_bool capture_errors = FALSE;`
  * Flag to print verbose diagnostics.
  * If true prints parser diagnostics to `stderr` and
  raises syntax error with diagnostic in `mrb_load_*` functions..
* `mrb_bool dump_result = FALSE;`
  * Flag to print verbose compile result.
  * If true prints AST in `mrb_parser_parse` and prints generated code in `mrb_load_*` functions.
* `mrb_bool no_exec = FALSE;`
  * Flag to print compile only.
  * If true won't execute compiled script.
  * `mrb_load_*_cxt` functions will Return compiled `Proc` object instead of execution result.

### mrbc_context_new
```C
mrbc_context* mrbc_context_new(mrb_state *mrb);
```
Creates new `mrbc_context`.

### mrbc_context_free
```C
void mrbc_context_free(mrb_state *mrb, mrbc_context *cxt);
```
Release context `cxt`.

### mrbc_partial_hook
```C
void mrbc_partial_hook(mrb_state *mrb, mrbc_context *c, int (*partial_hook)(struct mrb_parser_state*), void*data);
```
Sets partial hook of context `c`.
Partial hook is a hook called when lexer reaches end of current source.

`data` argument is a user data that will be set to `partial_data` field of `c`.
Partial hook must return `0` to tell reading can continue or `-1` to tell the reading ended.

### mrbc_filename
```C
const char *mrbc_filename(mrb_state *mrb, mrbc_context *c, const char *s);
```
If `s` is `NULL` returns current file name of context `c`.
Otherwise sets current file name of the context `c` to `s` and
returns the copied string.

Mainly used to set initial file name in parser.
If you need to change file name in partial hook use `mrb_parser_set_filename` instead.

## enum mrb_lexer_state_enum
Represents current lexer state in parser state.

Name | Description
`EXPR_BEG` | Beginning of a statement.
`EXPR_END` | An end of expression.
`EXPR_ENDARG` | Closing parenthese.
`EXPR_ENDFN` | End of definition.
`EXPR_ARG` | The last token was an argument.
`EXPR_CMDARG` |
`EXPR_MID` | Keyword `break`, `rescue`, `return`, `next`
`EXPR_FNAME` | Requiring method name for next token.
`EXPR_DOT` | Dot was the last token.
`EXPR_CLASS` | `class` keyword was the last token
`EXPR_VALUE` | Last token was keyword that requires following expression.
`EXPR_MAX_STATE` | Number of lexer states.

## struct mrb_parser_state
States of parser.

### Fields.
All the fields are read only from users.

* `mrb_state *mrb;`
  * `mrb_state` of parser state.
* `struct mrb_pool *pool;`
  * Memory pool for parser.
  * `mrb_parser_state` itself is allocated from this pool.
* `const char *s, *send;`
  * Parsing string of parser.
  * If `f` is non^`NULL` these would be ignored.
  * `send` is the end position of parsing string.
* `FILE *f;`
  * mruby script source of parser.
  * Only available when `ENABLE_STDIO` is defined.
* `mrbc_context *cxt;`
  * mruby compiler context of this parser.
* `char const *filename;`
  * Current lexing file name.
  * Would be set via `mrb_parser_set_filename`.
* `int lineno;`
  * Current lexing line number.
* `int column;`
  * Current lexing column number.
* `enum mrb_lex_state_enum lstate;`
  * Current lexer state.
  * Used in mirb to check a code block is opened.
* `mrb_ast_node *lex_strterm;`
  * List of `(type nest_level beg . end)`
* `mrb_ast_node *all_heredocs;`
  * List of `mrb_parser_heredoc_info`.
* `mrb_ast_node *heredocs_from_nextline;`
* `mrb_ast_node *parsing_heredoc;`
  * Current parsing heredoc.
  * Would be a non-`NULL` value if it's currently parsing heredoc.
* `mrb_ast_node *lex_strterm_before_heredoc;`
* `mrb_bool heredoc_end_now;`
  * Flag for mirb to check heredoc is continueing.
  * `TRUE` when a end of heredoc was parsed.
* `size_t nerr, nwarn;`
  * Count of error and warning.
* `struct mrb_parser_message error_buffer[10], warn_buffer[10];`
  * Buffer of errors and warning.
* `mrb_ast_node *tree;`
  * Parsed AST.

### mrb_parser_new
```C
struct mrb_parser_state* mrb_parser_new(mrb_state* mrb);
```
Creates parser state.
All object created from a `mrb_parser_state` is created from a single memory pool.
(Even `mrb_parser_state` itself.)

### mrb_parser_free
```C
void mrb_parser_free(struct mrb_parser_state *p);
```
Frees parser state `p`.

### mrb_parser_parse
```C
void mrb_parser_parse(struct mrb_parser_state *p, mrbc_context *cxt);
```
Parses mruby script using data in context `cxt` with parse state `p`.
Pass `NULL` to `cxt`if there is no context.

### mrb_parser_set_filename
```C
void mrb_parser_set_filename(struct mrb_parser_state *p, char const *f);
```
Sets current file name of parser state `p` to `f`.
Use this function in partial hook or parser state initialization.

### mrb_parser_get_filename
```C
char const* mrb_parser_get_filename(struct mrb_parser_state *p, uint16_t idx);
```
Gets file name from file name index `idx` in parser state `p`.
Returns `NULL` if not found.

### mrb_parse_file
```C
struct mrb_parser_state* mrb_parse_file(mrb_state *mrb, FILE *f, mrbc_context *cxt);
```
Parses mruby script read from file object `f` in context `cxt` and returns parser state as result.
Pass `NULL` to `cxt`if there is no context.

### mrb_parse_string
```C
struct mrb_parser_state* mrb_parse_string(mrb_state *mrb, const char* str, mrbc_context *cxt);
```
Parse mruby script `str` in context `cxt` and returns parser state as result.
Pass `NULL` to `cxt`if there is no context.

### mrb_parse_nstring
```C
struct mrb_parser_state* mrb_parse_nstring(mrb_state *mrb,const char *str, int len, mrbc_context *cxt);
```
Parse mruby script `str` of length `len` in context `cxt` and returns parser state as result.
Pass `NULL` to `cxt`if there is no context.

### mrb_generate_code
```C
struct RProc* mrb_generate_code(mrb_state *mrb, struct mrb_parser_state *p);
```
Generate IREP wrapped with `struct RProc` from parse result `p`.
Returns `NULL` if it failed to generate code.

## struct mrb_parser_message
Diagnostics of parse result.
Stored to `warn_buffer` or `error_buffer` of `mrb_parser_state`.

### Fields
* `int lineno;`
  * Line number of diagnostic.
* `int column;`
  * Column of diagnostic.
* `char *message;`
  * Diagnostic message.

## struct mrb_ast_node
Represents AST node used in mruby parser.

### Fields
* `struct mrb_ast_node *car, *cdr;`
* `uint16_t lineno;`
  * Line number of node.
* `uint16_t filename_index;`
  * Index of file name table in `mrb_parser_state`.
  * File name could be retreive with `mrb_parser_get_filename` function.

## struct mrb_parser_heredoc_info
Heredoc information.

### Fields
* `mrb_bool allow_indent;`
  * `TRUE` if heredoc begin with `<<-` syntax.
* `mrb_bool line_head;`
  * `TRUE` if lexer's current read point is beginning of line.
  * Used to check end of heredoc.
* `const char *term;`
  * Terminator of heredoc.
* `int term_len;`
  * Length of string `term`.
* `mrb_ast_node *doc;`
  * Parse result of heredoc.

## mrb_toplevel_run_keep
```C
mrb_value mrb_toplevel_run_keep(mrb_state *mrb, struct RProc *proc, unsigned int keep);
```
Runs `proc` keeping stack `keep`.

## enum mrb_lex_state_enum
* Lexer state.

## MRB_PARSER_BUF_SIZE
Length of parser buffer.
Limit token size.

## Loading functions
```C
mrb_value mrb_load_file(mrb_state *mrb, FILE *fp);
mrb_value mrb_load_string(mrb_state *mrb, const char *s);
mrb_value mrb_load_nstring(mrb_state *mrb, const char *s, int len);
mrb_value mrb_load_file_cxt(mrb_state *mrb, FILE *fp, mrbc_context *cxt);
mrb_value mrb_load_string_cxt(mrb_state *mrb, const char *s, mrbc_context *cxt);
mrb_value mrb_load_nstring_cxt(mrb_state *mrb, const char *s, int len, mrbc_context *cxt);
```
Load mruby script from file object `fp` or string `s`.
String script length would be calculated with `strlen` function if there is no `len` argument.
Returns compiled script if there is `cxt` argument and `cxt->no_exec` flag is `TRUE`.
Otherwise returns loaded result.

If the script compilation failed it will return `mrb_undef_value()` instead.
`SyntaxError` is set to `mrb->exc` when script parsing failed,
First parsing error is set to exception message of `SyntaxError` if `cxt->capture_errors` is `TRUE`.
`ScriptError` is set to `mrb->exc` when script code generating failed.

When a exception is raised in script execution,
sets `mrb->exc` to non-`NULL` value and returns `nil` instead.

If `cxt->dump_result` is `TRUE` it will print parsed AST and
generated mruby codes using `mrb_parser_dump` and `mrb_codedump_all`.

When `cxt->target_class` is non-`NULL`, executes script on class `cxt->target_class`.
