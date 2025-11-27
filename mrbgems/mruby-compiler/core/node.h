/*
** node.h - nodes of abstract syntax tree
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_COMPILER_NODE_H
#define MRUBY_COMPILER_NODE_H

enum node_type {
  NODE_SCOPE,
  NODE_BLOCK,
  NODE_IF,
  NODE_CASE,
  NODE_WHILE,
  NODE_UNTIL,
  NODE_WHILE_MOD,
  NODE_UNTIL_MOD,
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
  NODE_MARG,
  NODE_ASGN,
  NODE_OP_ASGN,
  NODE_CALL,
  NODE_SUPER,
  NODE_ZSUPER,
  NODE_ARRAY,
  NODE_ZARRAY,
  NODE_HASH,
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
  NODE_INT,
  NODE_BIGINT,
  NODE_FLOAT,
  NODE_NEGATE,
  NODE_LAMBDA,
  NODE_SYM,
  NODE_STR,
  NODE_XSTR,
  NODE_REGX,
  NODE_SPLAT,
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
  NODE_WORDS,
  NODE_SYMBOLS,
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

/* Variable-sized AST nodes */

/* Variable node header - common to all variable-sized nodes */
struct mrb_ast_var_header {
  uint16_t lineno;           /* Line number information */
  uint16_t filename_index;   /* File index information */
  uint8_t node_type;         /* NODE_INT, NODE_SYM, NODE_STR, etc. */
  /* Total: 6 bytes header for all variable nodes */
};

/* Literal value nodes */

/* Variable-sized symbol node */
struct mrb_ast_sym_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  mrb_sym symbol;                    /* Direct symbol reference */
  /* Total: 12-16 bytes vs previous 20+ bytes + indirection */
};

/* Variable-sized string node with cons list */
struct mrb_ast_str_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *list;
};

/* Variable-sized integer node */
struct mrb_ast_int_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  int32_t value;                     /* Direct 32-bit integer storage */
};

/* Variable-sized big integer node */
struct mrb_ast_bigint_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  char *string;                      /* String representation of big number */
  int base;                          /* Number base (8, 10, 16) */
};

/* Variable-sized node for variables (lvar, ivar, etc.) */
struct mrb_ast_var_node {
  struct mrb_ast_var_header header;
  mrb_sym symbol;
};

/* Expression and operation nodes */

/* Variable-sized call node with inline argument storage */
struct mrb_ast_call_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *receiver;     /* Receiver object */
  mrb_sym method_name;               /* Method name symbol */
  uint8_t safe_call:1;               /* Safe navigation (&.) */
  struct mrb_ast_node *args;         /* Arguments Information */
};

/* Variable-sized array node */
struct mrb_ast_array_node {
  struct mrb_ast_var_header header;    /* 8 bytes */
  struct mrb_ast_node *elements;       /* Elements list (cons list) */
};

/* Variable-sized hash node */
struct mrb_ast_hash_node {
  struct mrb_ast_var_header header;    /* 8 bytes */
  struct mrb_ast_node *pairs;          /* Key-value pairs (cons list) */
};

/* Control flow and definition nodes */

/* Variable-sized method definition node */
struct mrb_ast_def_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  mrb_sym name;                      /* Method name */
  struct mrb_ast_args *args;         /* Method arguments */
  struct mrb_ast_node *body;         /* Method body */
  struct mrb_ast_node *locals;       /* Local Variables */
} ;

/* Variable-sized singleton method definition node */
struct mrb_ast_sdef_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  mrb_sym name;                      /* Method name */
  struct mrb_ast_args *args;         /* Method arguments */
  struct mrb_ast_node *body;         /* Method body */
  struct mrb_ast_node *locals;       /* Local Variables */
  struct mrb_ast_node *obj;          /* receiver */
};

/* variable-sized class definition node */
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

/* Variable-sized case node */
struct mrb_ast_case_node {
  struct mrb_ast_var_header header;    /* 8 bytes */
  struct mrb_ast_node *value;          /* Case value expression */
  struct mrb_ast_node *body;           /* When/else clauses (cons list) */
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
  struct mrb_ast_node *pre;          /* Pre-splat variables (cons list) */
  struct mrb_ast_node *rest;         /* Splat variable (single node or -1) */
  struct mrb_ast_node *post;         /* Post-splat variables (cons list) */
  struct mrb_ast_node *rhs;          /* Right-hand side (values) */
};

/* Variable-sized operator assignment node */
struct mrb_ast_op_asgn_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *lhs;          /* Left-hand side (target) */
  mrb_sym op;                        /* Assignment operator (e.g., +=, -=, etc.) */
  struct mrb_ast_node *rhs;          /* Right-hand side (value) */
};

/* Expression Node Structures */

/* Variable-sized AND node */
struct mrb_ast_and_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *left;         /* Left operand */
  struct mrb_ast_node *right;        /* Right operand */
};

/* Variable-sized OR node */
struct mrb_ast_or_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *left;         /* Left operand */
  struct mrb_ast_node *right;        /* Right operand */
};

/* Variable-sized RETURN node */
struct mrb_ast_return_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *args;         /* Return arguments (can be NULL) */
};

/* Variable-sized YIELD node */
struct mrb_ast_yield_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *args;         /* Yield arguments (can be NULL) */
};

/* Variable-sized SUPER node */
struct mrb_ast_super_node {
  struct mrb_ast_var_header header;  /* 8 bytes */
  struct mrb_ast_node *args;         /* Super arguments (can be NULL) */
};

#define VAR_NODE_TYPE(n) ((enum node_type)(((struct mrb_ast_var_header*)(n))->node_type))

/* Type-safe casting macros */
#define var_header(n) ((struct mrb_ast_var_header*)(n))

/* Common type casting macros used by parser and codegen */
#define node_to_sym(x) ((mrb_sym)(intptr_t)(x))
#define sym_to_node(x) ((node*)(intptr_t)(x))
#define int_to_node(x) ((node*)(intptr_t)(x))
#define node_to_int(x) ((int)(intptr_t)(x))
#define node_to_type(x) ((enum node_type)(intptr_t)(x))
#define node_to_char(x) ((char)(intptr_t)(x))

/* Literal value node casting macros */
#define sym_node(n) ((struct mrb_ast_sym_node*)(n))
#define str_node(n) ((struct mrb_ast_str_node*)(n))
#define int_node(n) ((struct mrb_ast_int_node*)(n))
#define bigint_node(n) ((struct mrb_ast_bigint_node*)(n))
#define var_node(n) ((struct mrb_ast_var_node*)(n))

/* Expression and operation node casting macros */
#define call_node(n) ((struct mrb_ast_call_node*)(n))
#define array_node(n) ((struct mrb_ast_array_node*)(n))
#define hash_node(n) ((struct mrb_ast_hash_node*)(n))

/* Control flow and definition node casting macros */
#define def_node(n) ((struct mrb_ast_def_node*)(n))
#define class_node(n) ((struct mrb_ast_class_node*)(n))
#define module_node(n) ((struct mrb_ast_module_node*)(n))
#define sclass_node(n) ((struct mrb_ast_sclass_node*)(n))
#define if_node(n) ((struct mrb_ast_if_node*)(n))
#define while_node(n) ((struct mrb_ast_while_node*)(n))
#define until_node(n) ((struct mrb_ast_until_node*)(n))
#define case_node(n) ((struct mrb_ast_case_node*)(n))
#define for_node(n) ((struct mrb_ast_for_node*)(n))
#define asgn_node(n) ((struct mrb_ast_asgn_node*)(n))
#define masgn_node(n) ((struct mrb_ast_masgn_node*)(n))
#define op_asgn_node(n) ((struct mrb_ast_op_asgn_node*)(n))
#define and_node(n) ((struct mrb_ast_and_node*)(n))
#define or_node(n) ((struct mrb_ast_or_node*)(n))
#define return_node(n) ((struct mrb_ast_return_node*)(n))
#define yield_node(n) ((struct mrb_ast_yield_node*)(n))
#define super_node(n) ((struct mrb_ast_super_node*)(n))

/* Variable-sized literal node structures */

struct mrb_ast_dot2_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *left;
  struct mrb_ast_node *right;
};

struct mrb_ast_dot3_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *left;
  struct mrb_ast_node *right;
};

struct mrb_ast_float_node {
  struct mrb_ast_var_header header;
  const char *value;
};

/* Literal node casting macros */
#define dot2_node(n) ((struct mrb_ast_dot2_node*)(n))
#define dot3_node(n) ((struct mrb_ast_dot3_node*)(n))
#define float_node(n) ((struct mrb_ast_float_node*)(n))

/* Variable-sized simple node structures */
struct mrb_ast_self_node {
  struct mrb_ast_var_header header;
};

struct mrb_ast_nil_node {
  struct mrb_ast_var_header header;
};

struct mrb_ast_true_node {
  struct mrb_ast_var_header header;
};

struct mrb_ast_false_node {
  struct mrb_ast_var_header header;
};

struct mrb_ast_const_node {
  struct mrb_ast_var_header header;
  mrb_sym symbol;
};

/* Simple node casting macros */
#define self_node(n) ((struct mrb_ast_self_node*)(n))
#define nil_node(n) ((struct mrb_ast_nil_node*)(n))
#define true_node(n) ((struct mrb_ast_true_node*)(n))
#define false_node(n) ((struct mrb_ast_false_node*)(n))
#define const_node(n) ((struct mrb_ast_const_node*)(n))

/* Variable-sized advanced node structures */
struct mrb_ast_rescue_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *body;
  struct mrb_ast_node *rescue_clauses;
  struct mrb_ast_node *else_clause;
};

struct mrb_ast_block_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *locals;
  struct mrb_ast_args *args;
  struct mrb_ast_node *body;
};

/* Unified argument structure - eliminates args_tail_node allocation */
struct mrb_ast_args {
  /* Core argument lists (parser builds these naturally) */
  struct mrb_ast_node *mandatory_args;      /* Cons list of mandatory arguments */
  struct mrb_ast_node *optional_args;       /* Cons list of optional arguments */
  struct mrb_ast_node *post_mandatory_args; /* Cons list of post-mandatory arguments */
  struct mrb_ast_node *keyword_args;        /* Cons list of keyword arguments */

  /* Special arguments (directly embedded) */
  mrb_sym rest_arg;                         /* Rest argument symbol (0 = none) */
  mrb_sym kwrest_arg;                       /* Keyword rest argument (0 = none) */
  mrb_sym block_arg;                        /* Block argument symbol (0 = none) */
};

/* Call arguments structure - replaces cons-based new_callargs */
struct mrb_ast_callargs {
  struct mrb_ast_node *regular_args;    /* Cons list of regular arguments (preserves splat compatibility) */
  struct mrb_ast_node *keyword_args;    /* Keyword arguments hash node */
  struct mrb_ast_node *block_arg;       /* Block argument node */
};

/* Advanced node casting macros */
#define rescue_node(n) ((struct mrb_ast_rescue_node*)(n))
#define block_node(n) ((struct mrb_ast_block_node*)(n))
#define callargs_node(n) ((struct mrb_ast_callargs*)(n))

/* Control flow statement nodes */
struct mrb_ast_break_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *value;
};

struct mrb_ast_next_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *value;
};

struct mrb_ast_redo_node {
  struct mrb_ast_var_header header;
};

struct mrb_ast_retry_node {
  struct mrb_ast_var_header header;
};

#define break_node(n) ((struct mrb_ast_break_node*)(n))
#define next_node(n) ((struct mrb_ast_next_node*)(n))
#define redo_node(n) ((struct mrb_ast_redo_node*)(n))
#define retry_node(n) ((struct mrb_ast_retry_node*)(n))

/* String and regex variant nodes */
struct mrb_ast_xstr_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *list;
};

struct mrb_ast_regx_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *list;
  const char *flags;
  const char *encoding;
};

struct mrb_ast_heredoc_node {
  struct mrb_ast_var_header header;
  struct mrb_parser_heredoc_info info;
};

#define xstr_node(n) ((struct mrb_ast_xstr_node*)(n))
#define regx_node(n) ((struct mrb_ast_regx_node*)(n))
#define heredoc_node(n) ((struct mrb_ast_heredoc_node*)(n))
#define dsym_node(n) ((struct mrb_ast_str_node*)(n))

/* Reference and special variable nodes */
struct mrb_ast_nth_ref_node {
  struct mrb_ast_var_header header;
  int nth;
};

struct mrb_ast_back_ref_node {
  struct mrb_ast_var_header header;
  int type;
};

struct mrb_ast_nvar_node {
  struct mrb_ast_var_header header;
  int num;
};

struct mrb_ast_dvar_node {
  struct mrb_ast_var_header header;
  mrb_sym name;
};

#define nth_ref_node(n) ((struct mrb_ast_nth_ref_node*)(n))
#define back_ref_node(n) ((struct mrb_ast_back_ref_node*)(n))
#define nvar_node(n) ((struct mrb_ast_nvar_node*)(n))
#define dvar_node(n) ((struct mrb_ast_dvar_node*)(n))

/* Unary operator and scope resolution nodes */
struct mrb_ast_not_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *operand;
};

struct mrb_ast_negate_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *operand;
};

struct mrb_ast_colon2_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *base;
  mrb_sym name;
};

struct mrb_ast_colon3_node {
  struct mrb_ast_var_header header;
  mrb_sym name;
};

struct mrb_ast_defined_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *expr;
};

#define not_node(n) ((struct mrb_ast_not_node*)(n))
#define negate_node(n) ((struct mrb_ast_negate_node*)(n))
#define colon2_node(n) ((struct mrb_ast_colon2_node*)(n))
#define colon3_node(n) ((struct mrb_ast_colon3_node*)(n))
#define defined_node(n) ((struct mrb_ast_defined_node*)(n))

/* Lambda nodes */
struct mrb_ast_lambda_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *locals;
  struct mrb_ast_args *args;
  struct mrb_ast_node *body;
};

/* Array literal variant nodes */
struct mrb_ast_zarray_node {
  struct mrb_ast_var_header header;
};

struct mrb_ast_words_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *args;
};

struct mrb_ast_symbols_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *args;
};

/* Argument and parameter nodes */

struct mrb_ast_splat_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *value;
};

struct mrb_ast_block_arg_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *value;
};

/* Structural and block nodes */

struct mrb_ast_scope_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *locals;
  struct mrb_ast_node *body;
};

struct mrb_ast_begin_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *body;
};

struct mrb_ast_ensure_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *body;
  struct mrb_ast_node *ensure_clause;
};

struct mrb_ast_stmts_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *stmts;        /* Cons-list of statements */
};

struct mrb_ast_iter_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *vars;
  struct mrb_ast_node *body;
};

/* Declaration nodes */

struct mrb_ast_alias_node {
  struct mrb_ast_var_header header;
  mrb_sym new_name;
  mrb_sym old_name;
};

struct mrb_ast_undef_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *syms;
};

struct mrb_ast_postexe_node {
  struct mrb_ast_var_header header;
  struct mrb_ast_node *body;
};

#define zsuper_node(n) ((struct mrb_ast_super_node*)(n))
#define lambda_node(n) ((struct mrb_ast_lambda_node*)(n))
#define zarray_node(n) ((struct mrb_ast_zarray_node*)(n))
#define words_node(n) ((struct mrb_ast_words_node*)(n))
#define symbols_node(n) ((struct mrb_ast_symbols_node*)(n))
#define splat_node(n) ((struct mrb_ast_splat_node*)(n))
#define block_arg_node(n) ((struct mrb_ast_block_arg_node*)(n))
#define scope_node(n) ((struct mrb_ast_scope_node*)(n))
#define begin_node(n) ((struct mrb_ast_begin_node*)(n))
#define ensure_node(n) ((struct mrb_ast_ensure_node*)(n))
#define stmts_node(n) ((struct mrb_ast_stmts_node*)(n))
#define iter_node(n) ((struct mrb_ast_iter_node*)(n))
#define alias_node(n) ((struct mrb_ast_alias_node*)(n))
#define undef_node(n) ((struct mrb_ast_undef_node*)(n))
#define postexe_node(n) ((struct mrb_ast_postexe_node*)(n))
#define sdef_node(n) ((struct mrb_ast_sdef_node*)(n))

#endif  /* MRUBY_COMPILER_NODE_H */
