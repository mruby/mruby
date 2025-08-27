/*
** node.h - nodes of abstract syntax tree
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_COMPILER_NODE_H
#define MRUBY_COMPILER_NODE_H

enum node_type {
  NODE_METHOD,
  NODE_SCOPE,
  NODE_BLOCK,
  NODE_IF,
  NODE_CASE,
  NODE_WHEN,
  NODE_WHILE,
  NODE_UNTIL,
  NODE_WHILE_MOD,
  NODE_UNTIL_MOD,
  NODE_ITER,
  NODE_FOR,
  NODE_BREAK,
  NODE_NEXT,
  NODE_REDO,
  NODE_RETRY,
  NODE_STMTS,
  NODE_BEGIN,
  NODE_RESCUE,
  NODE_ENSURE,
  NODE_AND,
  NODE_OR,
  NODE_NOT,
  NODE_MASGN,
  NODE_ASGN,
  NODE_CDECL,
  NODE_CVASGN,
  NODE_CVDECL,
  NODE_OP_ASGN,
  NODE_CALL,
  NODE_SCALL,
  NODE_FCALL,
  NODE_SUPER,
  NODE_ZSUPER,
  NODE_ARRAY,
  NODE_ZARRAY,
  NODE_HASH,
  NODE_KW_HASH,
  NODE_RETURN,
  NODE_YIELD,
  NODE_LVAR,
  NODE_DVAR,
  NODE_GVAR,
  NODE_IVAR,
  NODE_CONST,
  NODE_CVAR,
  NODE_NVAR,
  NODE_NTH_REF,
  NODE_BACK_REF,
  NODE_MATCH,
  NODE_INT,
  NODE_FLOAT,
  NODE_NEGATE,
  NODE_LAMBDA,
  NODE_SYM,
  NODE_STR,
  NODE_DSTR,
  NODE_XSTR,
  NODE_DXSTR,
  NODE_REGX,
  NODE_DREGX,
  NODE_DREGX_ONCE,
  NODE_ARG,
  NODE_ARGS_TAIL,
  NODE_KW_ARG,
  NODE_KW_REST_ARGS,
  NODE_SPLAT,
  NODE_TO_ARY,
  NODE_SVALUE,
  NODE_BLOCK_ARG,
  NODE_DEF,
  NODE_SDEF,
  NODE_ALIAS,
  NODE_UNDEF,
  NODE_CLASS,
  NODE_MODULE,
  NODE_SCLASS,
  NODE_COLON2,
  NODE_COLON3,
  NODE_DOT2,
  NODE_DOT3,
  NODE_SELF,
  NODE_NIL,
  NODE_TRUE,
  NODE_FALSE,
  NODE_DEFINED,
  NODE_POSTEXE,
  NODE_DSYM,
  NODE_HEREDOC,
  NODE_LITERAL_DELIM,
  NODE_WORDS,
  NODE_SYMBOLS,
  NODE_VARIABLE,
  NODE_LAST
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
  mrb_bool remove_indent:1;
  mrb_bool line_head:1;
  size_t indent;
  mrb_ast_node *indented;
  enum mrb_string_type type;
  const char *term;
  int term_len;
  mrb_ast_node *doc;
};

/* AST node structures - Head-only location optimization */

/* Structure nodes - only car/cdr, ignores location fields */
struct mrb_ast_node {
  struct mrb_ast_node *car, *cdr;
  /* No location fields - saves 4 bytes per structure node */
};

/* Head nodes - with location info, used by cons_head() */
struct mrb_ast_head_node {
  struct mrb_ast_node *car, *cdr;
  uint16_t lineno, filename_index;
};

/* Variable-sized AST nodes - Phase 1 Infrastructure */

/* Variable node header - common to all variable-sized nodes */
struct mrb_ast_var_header {
  uint16_t lineno;           /* Line number information */
  uint16_t filename_index;   /* File index information */
  uint8_t node_type;         /* NODE_INT, NODE_STR, NODE_SYM, etc. */
  uint8_t size_class;        /* Size class for allocation/deallocation */
  uint16_t flags;            /* Type-specific flags and metadata */
  /* Total: 8 bytes header for all variable nodes */
};

/* Size class enumeration */
enum mrb_ast_size_class {
  SIZE_CLASS_TINY    = 0,  /* 8-16 bytes  - symbols, small values */
  SIZE_CLASS_SMALL   = 1,  /* 16-32 bytes - most nodes */
  SIZE_CLASS_MEDIUM  = 2,  /* 32-64 bytes - complex expressions */
  SIZE_CLASS_LARGE   = 3,  /* 64-128 bytes - large arrays/calls */
  SIZE_CLASS_XLARGE  = 4,  /* 128+ bytes - very large constructs */
  SIZE_CLASS_COUNT   = 5
};

/* Size class limits for allocation decisions */
#define SIZE_CLASS_LIMITS { 16, 32, 64, 128, 256 }

/* Node type flags */
#define VAR_NODE_FLAG_INLINE_DATA    0x0001  /* Data stored inline */
#define VAR_NODE_FLAG_HEAP_ALLOCATED 0x0002  /* Large data on heap */
#define VAR_NODE_FLAG_CACHED         0x0004  /* Node is cached/reusable */

/* Phase 1 Variable Node Structures */

/* Variable-sized symbol node */
struct mrb_ast_sym_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  mrb_sym symbol;                    /* Direct symbol reference */
  /* Total: 12-16 bytes vs previous 20+ bytes + indirection */
};

/* Variable-sized string node with inline storage */
struct mrb_ast_str_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  size_t len;                        /* String length */
  char data[];                       /* Flexible array - inline string storage */
  /* Total: Variable (16 + string_length) vs previous 20+ bytes + separate allocation */
};

/* Variable-sized integer node */
struct mrb_ast_int_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  int32_t value;                     /* Direct 32-bit integer storage */
};

/* Variable-sized node for variables (lvar, ivar, etc.) */
struct mrb_ast_var_node {
  struct mrb_ast_var_header header;
  mrb_sym symbol;
};

/* Phase 2 Variable Node Structures */

/* Variable-sized call node with inline argument storage */
struct mrb_ast_call_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *receiver;     /* Receiver object */
  mrb_sym method_name;               /* Method name symbol */
  uint8_t argc;                      /* Number of regular arguments */
  uint8_t has_kwargs:1;              /* Has keyword arguments */
  uint8_t has_block:1;               /* Has block argument */
  uint8_t safe_call:1;               /* Safe navigation (&.) */
  uint8_t reserved:5;                /* Reserved for future flags */
  /* Followed by variable data:
   * - argc * sizeof(struct mrb_ast_node*) for regular arguments
   * - struct mrb_ast_node* for kwargs (if has_kwargs)
   * - struct mrb_ast_node* for block (if has_block)
   */
  struct mrb_ast_node *args[];       /* Flexible array for arguments */
};

/* Variable-sized array node with inline element storage */
struct mrb_ast_array_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  uint16_t len;                      /* Number of elements */
  uint16_t flags;                    /* Array-specific flags */
  struct mrb_ast_node *elements[];   /* Flexible array for elements */
};

/* Variable-sized hash node with inline key-value storage */
struct mrb_ast_hash_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  uint16_t len;                      /* Number of key-value pairs */
  uint16_t flags;                    /* Hash-specific flags */
  /* Interleaved key-value pairs: key0, value0, key1, value1, ... */
  struct mrb_ast_node *pairs[];      /* Flexible array for key-value pairs */
};

/* Phase 3 Variable Node Structures - Control Flow */

/* Variable-sized method definition node */
struct mrb_ast_def_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  mrb_sym name;                      /* Method name */
  struct mrb_ast_node *args;         /* Arguments node */
  struct mrb_ast_node *body;         /* Method body */
};

/* Variable-sized class definition node */
struct mrb_ast_class_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *name;         /* Class name (NODE_CONST or NODE_COLON2) */
  struct mrb_ast_node *superclass;   /* Superclass (can be NULL) */
  struct mrb_ast_node *body;         /* Class body */
};

/* Variable-sized module definition node */
struct mrb_ast_module_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *name;         /* Module name (NODE_CONST or NODE_COLON2) */
  struct mrb_ast_node *body;         /* Module body */
};

/* Variable-sized singleton class definition node */
struct mrb_ast_sclass_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *obj;          /* Object for singleton class */
  struct mrb_ast_node *body;         /* Singleton class body */
};

/* Variable-sized if node */
struct mrb_ast_if_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *condition;    /* Condition expression */
  struct mrb_ast_node *then_body;    /* Then branch */
  struct mrb_ast_node *else_body;    /* Else branch (can be NULL) */
};

/* Variable-sized while node */
struct mrb_ast_while_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *condition;    /* Loop condition */
  struct mrb_ast_node *body;         /* Loop body */
};

/* Variable-sized until node */
struct mrb_ast_until_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *condition;    /* Loop condition */
  struct mrb_ast_node *body;         /* Loop body */
};

/* Variable-sized case node with variable when clauses */
struct mrb_ast_case_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *value;        /* Case value expression */
  uint16_t when_count;               /* Number of when clauses */
  uint16_t flags;                    /* Case-specific flags */
  struct mrb_ast_node *else_body;    /* Else clause (can be NULL) */
  struct mrb_ast_node *when_clauses[]; /* Flexible array for when clauses */
};

/* Variable-sized for node */
struct mrb_ast_for_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *var;          /* Loop variable */
  struct mrb_ast_node *iterable;     /* Object to iterate over */
  struct mrb_ast_node *body;         /* Loop body */
};

/* Assignment Node Structures */

/* Variable-sized assignment node */
struct mrb_ast_asgn_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *lhs;          /* Left-hand side (target) */
  struct mrb_ast_node *rhs;          /* Right-hand side (value) */
};

/* Variable-sized multiple assignment node */
struct mrb_ast_masgn_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *lhs;          /* Left-hand side (multiple targets) */
  struct mrb_ast_node *rhs;          /* Right-hand side (values) */
};

/* Variable-sized operator assignment node */
struct mrb_ast_op_asgn_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *lhs;          /* Left-hand side (target) */
  mrb_sym operator;                  /* Assignment operator (e.g., +=, -=, etc.) */
  struct mrb_ast_node *rhs;          /* Right-hand side (value) */
};

/* String storage strategy thresholds */
#define STR_INLINE_THRESHOLD  48   /* Inline strings <= 48 bytes */
#define STR_SMALL_THRESHOLD   128  /* Small strings <= 128 bytes */

#define VAR_NODE_TYPE(n) ((enum node_type)(((struct mrb_ast_var_header*)(n))->node_type))
#define VAR_NODE_CLASS(n) (((struct mrb_ast_var_header*)(n))->size_class)

/* Type-safe casting macros */
#define var_header(n) ((struct mrb_ast_var_header*)(n))

/* Common type casting macros used by parser and codegen */
#define node_to_sym(x) ((mrb_sym)(intptr_t)(x))
#define sym_to_node(x) ((node*)(intptr_t)(x))
#define int_to_node(x) ((node*)(intptr_t)(x))
#define head(x) ((struct mrb_ast_head_node*)(x))
#define node_to_int(x) ((int)(intptr_t)(x))
#define node_to_type(x) ((enum node_type)(intptr_t)(x))
#define node_to_char(x) ((char)(intptr_t)(x))

// Macros for variable-sized nodes
#define NODE_TYPE(n) ((enum node_type)(intptr_t)((n)->car))
#define NODE_VAR_NODE_PTR(n) ((struct mrb_ast_var_header*)((n)->cdr))



/* Phase 1 node casting macros */
#define sym_node(n) ((struct mrb_ast_sym_node*)(n))
#define str_node(n) ((struct mrb_ast_str_node*)(n))
#define int_node(n) ((struct mrb_ast_int_node*)(n))
#define var_node(n) ((struct mrb_ast_var_node*)(n))

/* Phase 2 node casting macros */
#define call_node(n) ((struct mrb_ast_call_node*)(n))
#define array_node(n) ((struct mrb_ast_array_node*)(n))
#define hash_node(n) ((struct mrb_ast_hash_node*)(n))

/* Phase 3 node casting macros */
#define def_node(n) ((struct mrb_ast_def_node*)(n))
#define class_node(n) ((struct mrb_ast_class_node*)(n))
#define module_node(n) ((struct mrb_ast_module_node*)(n))
#define sclass_node(n) ((struct mrb_ast_sclass_node*)(n))
#define if_node(n) ((struct mrb_ast_if_node*)(n))
#define while_node(n) ((struct mrb_ast_while_node*)(n))
#define until_node(n) ((struct mrb_ast_until_node*)(n))
#define case_node_ctrl(n) ((struct mrb_ast_case_node*)(n))
#define for_node(n) ((struct mrb_ast_for_node*)(n))
#define asgn_node(n) ((struct mrb_ast_asgn_node*)(n))
#define masgn_node(n) ((struct mrb_ast_masgn_node*)(n))
#define op_asgn_node(n) ((struct mrb_ast_op_asgn_node*)(n))

/* Phase 1 value access macros */
#define SYM_NODE_VALUE(n) (sym_node(n)->symbol)
#define STR_NODE_PTR(n) (str_node(n)->data)
#define STR_NODE_LEN(n) (str_node(n)->len)
#define STR_NODE_INLINE_P(n) (var_header(n)->flags & VAR_NODE_FLAG_INLINE_DATA)
#define INT_NODE_VALUE(n) (int_node(n)->value)
#define VAR_NODE_SYMBOL(n) (var_node(n)->symbol)

/* Phase 2 value access macros */
#define CALL_NODE_RECEIVER(n) (call_node(n)->receiver)
#define CALL_NODE_METHOD(n) (call_node(n)->method_name)
#define CALL_NODE_ARGC(n) (call_node(n)->argc)
#define CALL_NODE_ARGS(n) (call_node(n)->args)
#define CALL_NODE_HAS_KWARGS(n) (call_node(n)->has_kwargs)
#define CALL_NODE_HAS_BLOCK(n) (call_node(n)->has_block)
#define CALL_NODE_SAFE(n) (call_node(n)->safe_call)

#define ARRAY_NODE_LEN(n) (array_node(n)->len)
#define ARRAY_NODE_ELEMENTS(n) (array_node(n)->elements)

#define HASH_NODE_LEN(n) (hash_node(n)->len)
#define HASH_NODE_PAIRS(n) (hash_node(n)->pairs)

/* Phase 3 value access macros */
#define IF_NODE_CONDITION(n) (if_node(n)->condition)
#define IF_NODE_THEN(n) (if_node(n)->then_body)
#define IF_NODE_ELSE(n) (if_node(n)->else_body)

#define WHILE_NODE_CONDITION(n) (while_node(n)->condition)
#define WHILE_NODE_BODY(n) (while_node(n)->body)

#define UNTIL_NODE_CONDITION(n) (until_node(n)->condition)
#define UNTIL_NODE_BODY(n) (until_node(n)->body)

#define CASE_NODE_VALUE(n) (case_node_ctrl(n)->value)
#define CASE_NODE_WHEN_COUNT(n) (case_node_ctrl(n)->when_count)
#define CASE_NODE_ELSE(n) (case_node_ctrl(n)->else_body)
#define CASE_NODE_WHENS(n) (case_node_ctrl(n)->when_clauses)

#define FOR_NODE_VAR(n) (for_node(n)->var)
#define FOR_NODE_ITERABLE(n) (for_node(n)->iterable)
#define FOR_NODE_BODY(n) (for_node(n)->body)

/* Definition node value access macros */
#define DEF_NODE_NAME(n) (def_node(n)->name)
#define DEF_NODE_ARGS(n) (def_node(n)->args)
#define DEF_NODE_BODY(n) (def_node(n)->body)

#define CLASS_NODE_NAME(n) (class_node(n)->name)
#define CLASS_NODE_SUPERCLASS(n) (class_node(n)->superclass)
#define CLASS_NODE_BODY(n) (class_node(n)->body)

#define MODULE_NODE_NAME(n) (module_node(n)->name)
#define MODULE_NODE_BODY(n) (module_node(n)->body)

#define SCLASS_NODE_OBJ(n) (sclass_node(n)->obj)
#define SCLASS_NODE_BODY(n) (sclass_node(n)->body)

/* Assignment node value access macros */
#define ASGN_NODE_LHS(n) (asgn_node(n)->lhs)
#define ASGN_NODE_RHS(n) (asgn_node(n)->rhs)

#define MASGN_NODE_LHS(n) (masgn_node(n)->lhs)
#define MASGN_NODE_RHS(n) (masgn_node(n)->rhs)

#define OP_ASGN_NODE_LHS(n) (op_asgn_node(n)->lhs)
#define OP_ASGN_NODE_OP(n) (op_asgn_node(n)->operator)
#define OP_ASGN_NODE_RHS(n) (op_asgn_node(n)->rhs)

#endif  /* MRUBY_COMPILER_NODE_H */
