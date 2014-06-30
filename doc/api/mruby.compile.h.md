# mruby/compile.h

## mrbc_context
mruby compiler context.

### Fields of mrbc_context.
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

## mrb_parser_state

## Undocumented

```C
mrb_value mrb_toplevel_run_keep(mrb_state*, struct RProc*, unsigned int);

/* AST node structure */
typedef struct mrb_ast_node {
  struct mrb_ast_node *car, *cdr;
  uint16_t lineno, filename_index;
} mrb_ast_node;

/* lexer states */
enum mrb_lex_state_enum {
  EXPR_BEG,                   /* ignore newline, +/- is a sign. */
  EXPR_END,                   /* newline significant, +/- is an operator. */
  EXPR_ENDARG,                /* ditto, and unbound braces. */
  EXPR_ENDFN,                 /* ditto, and unbound braces. */
  EXPR_ARG,                   /* newline significant, +/- is an operator. */
  EXPR_CMDARG,                /* newline significant, +/- is an operator. */
  EXPR_MID,                   /* newline significant, +/- is an operator. */
  EXPR_FNAME,                 /* ignore newline, no reserved words. */
  EXPR_DOT,                   /* right after `.' or `::', no reserved words. */
  EXPR_CLASS,                 /* immediate after `class', no here document. */
  EXPR_VALUE,                 /* alike EXPR_BEG but label is disallowed. */
  EXPR_MAX_STATE
};

/* saved error message */
struct mrb_parser_message {
  int lineno;
  int column;
  char* message;
};

#define STR_FUNC_PARSING 0x01
#define STR_FUNC_EXPAND  0x02
#define STR_FUNC_REGEXP  0x04
#define STR_FUNC_WORD    0x08
#define STR_FUNC_SYMBOL  0x10
#define STR_FUNC_ARRAY   0x20
#define STR_FUNC_HEREDOC 0x40
#define STR_FUNC_XQUOTE  0x80

enum mrb_string_type {
  str_not_parsing  = (0),
  str_squote   = (STR_FUNC_PARSING),
  str_dquote   = (STR_FUNC_PARSING|STR_FUNC_EXPAND),
  str_regexp   = (STR_FUNC_PARSING|STR_FUNC_REGEXP|STR_FUNC_EXPAND),
  str_sword    = (STR_FUNC_PARSING|STR_FUNC_WORD|STR_FUNC_ARRAY),
  str_dword    = (STR_FUNC_PARSING|STR_FUNC_WORD|STR_FUNC_ARRAY|STR_FUNC_EXPAND),
  str_ssym     = (STR_FUNC_PARSING|STR_FUNC_SYMBOL),
  str_ssymbols = (STR_FUNC_PARSING|STR_FUNC_SYMBOL|STR_FUNC_ARRAY),
  str_dsymbols = (STR_FUNC_PARSING|STR_FUNC_SYMBOL|STR_FUNC_ARRAY|STR_FUNC_EXPAND),
  str_heredoc  = (STR_FUNC_PARSING|STR_FUNC_HEREDOC),
  str_xquote   = (STR_FUNC_PARSING|STR_FUNC_XQUOTE|STR_FUNC_EXPAND),
};

/* heredoc structure */
struct mrb_parser_heredoc_info {
  mrb_bool allow_indent:1;
  mrb_bool line_head:1;
  enum mrb_string_type type;
  const char *term;
  int term_len;
  mrb_ast_node *doc;
};

#define MRB_PARSER_BUF_SIZE 1024

/* parser structure */
struct mrb_parser_state {
  mrb_state *mrb;
  struct mrb_pool *pool;
  mrb_ast_node *cells;
  const char *s, *send;
#ifdef ENABLE_STDIO
  FILE *f;
#endif
  mrbc_context *cxt;
  char const *filename;
  int lineno;
  int column;

  enum mrb_lex_state_enum lstate;
  mrb_ast_node *lex_strterm; /* (type nest_level beg . end) */

  unsigned int cond_stack;
  unsigned int cmdarg_stack;
  int paren_nest;
  int lpar_beg;
  int in_def, in_single;
  mrb_bool cmd_start:1;
  mrb_ast_node *locals;

  mrb_ast_node *pb;
  char buf[MRB_PARSER_BUF_SIZE];
  int bidx;

  mrb_ast_node *all_heredocs;	/* list of mrb_parser_heredoc_info* */
  mrb_ast_node *heredocs_from_nextline;
  mrb_ast_node *parsing_heredoc;
  mrb_ast_node *lex_strterm_before_heredoc;
  mrb_bool heredoc_end_now:1; /* for mirb */

  void *ylval;

  size_t nerr;
  size_t nwarn;
  mrb_ast_node *tree;

  mrb_bool capture_errors:1;
  struct mrb_parser_message error_buffer[10];
  struct mrb_parser_message warn_buffer[10];

  mrb_sym* filename_table;
  size_t filename_table_length;
  int current_filename_index;

  struct mrb_jmpbuf* jmp;
};

struct mrb_parser_state* mrb_parser_new(mrb_state*);
void mrb_parser_free(struct mrb_parser_state*);
void mrb_parser_parse(struct mrb_parser_state*,mrbc_context*);

void mrb_parser_set_filename(struct mrb_parser_state*, char const*);
char const* mrb_parser_get_filename(struct mrb_parser_state*, uint16_t idx);

/* utility functions */
#ifdef ENABLE_STDIO
struct mrb_parser_state* mrb_parse_file(mrb_state*,FILE*,mrbc_context*);
#endif
struct mrb_parser_state* mrb_parse_string(mrb_state*,const char*,mrbc_context*);
struct mrb_parser_state* mrb_parse_nstring(mrb_state*,const char*,int,mrbc_context*);
struct RProc* mrb_generate_code(mrb_state*, struct mrb_parser_state*);

/* program load functions */
#ifdef ENABLE_STDIO
mrb_value mrb_load_file(mrb_state*,FILE*);
#endif
mrb_value mrb_load_string(mrb_state *mrb, const char *s);
mrb_value mrb_load_nstring(mrb_state *mrb, const char *s, int len);
#ifdef ENABLE_STDIO
mrb_value mrb_load_file_cxt(mrb_state*,FILE*, mrbc_context *cxt);
#endif
mrb_value mrb_load_string_cxt(mrb_state *mrb, const char *s, mrbc_context *cxt);
mrb_value mrb_load_nstring_cxt(mrb_state *mrb, const char *s, int len, mrbc_context *cxt);
```
