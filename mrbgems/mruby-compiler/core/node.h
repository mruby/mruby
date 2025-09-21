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
  NODE_ASGN,
  NODE_OP_ASGN,
  NODE_CALL,
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
  NODE_BIGINT,
  NODE_FLOAT,
  NODE_NEGATE,
  NODE_LAMBDA,
  NODE_SYM,
  NODE_STR,
  NODE_XSTR,
  NODE_REGX,
  NODE_DREGX,
  NODE_CALLARGS,
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
  uint8_t node_type;         /* NODE_INT, NODE_SYM, NODE_STR, etc. */
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

/* Variable-sized string node with cons list */
struct mrb_ast_str_node {
  struct mrb_ast_var_header hdr;
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
  struct mrb_ast_node *args;         /* Arguments Information */
};

/* Variable-sized array node - revert to original cons list approach */
struct mrb_ast_array_node {
  struct mrb_ast_var_header header;    /* 8 bytes */
  struct mrb_ast_node *elements;       /* Original elements list */
};

/* Variable-sized hash node - revert to original cons list approach */
struct mrb_ast_hash_node {
  struct mrb_ast_var_header header;    /* 8 bytes */
  struct mrb_ast_node *pairs;          /* Original pairs list */
};

/* Phase 3 Variable Node Structures - Control Flow */

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

/* Variable-sized case node - revert to original cons list approach */
struct mrb_ast_case_node {
  struct mrb_ast_var_header header;    /* 8 bytes */
  struct mrb_ast_node *value;          /* Case value expression */
  struct mrb_ast_node *body;           /* Original when/else body list */
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
#define bigint_node(n) ((struct mrb_ast_bigint_node*)(n))
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

/* Phase 1 value access macros */
#define SYM_NODE_VALUE(n) (sym_node(n)->symbol)
#define STR_NODE_PTR(n) (str_node(n)->data)
#define STR_NODE_LEN(n) (str_node(n)->len)
#define STR_NODE_INLINE_P(n) (var_header(n)->flags & VAR_NODE_FLAG_INLINE_DATA)
#define INT_NODE_VALUE(n) (int_node(n)->value)
#define BIGINT_NODE_STRING(n) (bigint_node(n)->string)
#define BIGINT_NODE_BASE(n) (bigint_node(n)->base)
#define VAR_NODE_SYMBOL(n) (var_node(n)->symbol)

/* Phase 2 value access macros */
#define CALL_NODE_RECEIVER(n) (call_node(n)->receiver)
#define CALL_NODE_METHOD(n) (call_node(n)->method_name)
#define CALL_NODE_ARGC(n) (call_node(n)->argc)
#define CALL_NODE_ARGS(n) (call_node(n)->args)
#define CALL_NODE_HAS_KWARGS(n) (call_node(n)->has_kwargs)
#define CALL_NODE_HAS_BLOCK(n) (call_node(n)->has_block)
#define CALL_NODE_SAFE(n) (call_node(n)->safe_call)

#define ARRAY_NODE_ELEMENTS(n) (array_node(n)->elements)

#define HASH_NODE_PAIRS(n) (hash_node(n)->pairs)

/* Phase 3 value access macros */
#define IF_NODE_CONDITION(n) (if_node(n)->condition)
#define IF_NODE_THEN(n) (if_node(n)->then_body)
#define IF_NODE_ELSE(n) (if_node(n)->else_body)

#define WHILE_NODE_CONDITION(n) (while_node(n)->condition)
#define WHILE_NODE_BODY(n) (while_node(n)->body)

#define UNTIL_NODE_CONDITION(n) (until_node(n)->condition)
#define UNTIL_NODE_BODY(n) (until_node(n)->body)

#define CASE_NODE_VALUE(n) (case_node(n)->value)
#define CASE_NODE_BODY(n) (case_node(n)->body)

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
#define OP_ASGN_NODE_OP(n) (op_asgn_node(n)->op)
#define OP_ASGN_NODE_RHS(n) (op_asgn_node(n)->rhs)

/* Expression node value access macros */
#define AND_NODE_LEFT(n) (and_node(n)->left)
#define AND_NODE_RIGHT(n) (and_node(n)->right)

#define OR_NODE_LEFT(n) (or_node(n)->left)
#define OR_NODE_RIGHT(n) (or_node(n)->right)

#define RETURN_NODE_ARGS(n) (return_node(n)->args)

#define YIELD_NODE_ARGS(n) (yield_node(n)->args)

#define SUPER_NODE_ARGS(n) (super_node(n)->args)

/* Variable-sized literal node structures */

struct mrb_ast_regx_node {
  struct mrb_ast_var_header hdr;
  const char *pattern;
  int pattern_len;
  const char *flags;
  const char *encoding;
};

struct mrb_ast_dot2_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *left;
  struct mrb_ast_node *right;
};

struct mrb_ast_dot3_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *left;
  struct mrb_ast_node *right;
};

struct mrb_ast_float_node {
  struct mrb_ast_var_header hdr;
  const char *value;
};

/* Literal node casting macros */
#define regx_node(n) ((struct mrb_ast_regx_node*)(n))
#define dot2_node(n) ((struct mrb_ast_dot2_node*)(n))
#define dot3_node(n) ((struct mrb_ast_dot3_node*)(n))
#define float_node(n) ((struct mrb_ast_float_node*)(n))

/* Literal node value access macros */
#define STR_NODE_LIST(n) (str_node(n)->list)

#define REGX_NODE_PATTERN(n) (regx_node(n)->pattern)
#define REGX_NODE_FLAGS(n) (regx_node(n)->flags)
#define REGX_NODE_ENCODING(n) (regx_node(n)->encoding)

#define DOT2_NODE_LEFT(n) (dot2_node(n)->left)
#define DOT2_NODE_RIGHT(n) (dot2_node(n)->right)

#define DOT3_NODE_LEFT(n) (dot3_node(n)->left)
#define DOT3_NODE_RIGHT(n) (dot3_node(n)->right)

#define FLOAT_NODE_VALUE(n) (float_node(n)->value)

/* Variable-sized simple node structures */
struct mrb_ast_self_node {
  struct mrb_ast_var_header hdr;
};

struct mrb_ast_nil_node {
  struct mrb_ast_var_header hdr;
};

struct mrb_ast_true_node {
  struct mrb_ast_var_header hdr;
};

struct mrb_ast_false_node {
  struct mrb_ast_var_header hdr;
};

struct mrb_ast_const_node {
  struct mrb_ast_var_header hdr;
  mrb_sym symbol;
};

/* Simple node casting macros */
#define self_node(n) ((struct mrb_ast_self_node*)(n))
#define nil_node(n) ((struct mrb_ast_nil_node*)(n))
#define true_node(n) ((struct mrb_ast_true_node*)(n))
#define false_node(n) ((struct mrb_ast_false_node*)(n))
#define const_node(n) ((struct mrb_ast_const_node*)(n))

/* Simple node value access macros */
#define CONST_NODE_SYMBOL(n) (const_node(n)->symbol)

/* Variable-sized advanced node structures */
struct mrb_ast_rescue_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *body;
  struct mrb_ast_node *rescue_clauses;
  struct mrb_ast_node *else_clause;
};

struct mrb_ast_block_node {
  struct mrb_ast_var_header hdr;
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

/* Advanced node value access macros */
#define RESCUE_NODE_BODY(n) (rescue_node(n)->body)
#define RESCUE_NODE_RESCUE_CLAUSES(n) (rescue_node(n)->rescue_clauses)
#define RESCUE_NODE_ELSE_CLAUSE(n) (rescue_node(n)->else_clause)

#define BLOCK_NODE_LOCALS(n) (block_node(n)->locals)
#define BLOCK_NODE_ARGS(n) (block_node(n)->args)
#define BLOCK_NODE_BODY(n) (block_node(n)->body)

#define ARGS_NODE_MANDATORY(n) (args_node(n)->mandatory)
#define ARGS_NODE_OPTIONAL(n) (args_node(n)->optional)
#define ARGS_NODE_REST(n) (args_node(n)->rest)
#define ARGS_NODE_MANDATORY_AFTER_REST(n) (args_node(n)->mandatory_after_rest)
#define ARGS_NODE_TAIL(n) (args_node(n)->tail)

#define ARGS_TAIL_NODE_KEYWORDS(n) (args_tail_node(n)->keywords)
#define ARGS_TAIL_NODE_KWREST(n) (args_tail_node(n)->kwrest)
#define ARGS_TAIL_NODE_BLOCK(n) (args_tail_node(n)->block)

#define CALLARGS_NODE_REGULAR(n) (callargs_node(n)->regular_args)
#define CALLARGS_NODE_KEYWORDS(n) (callargs_node(n)->keyword_args)
#define CALLARGS_NODE_BLOCK(n) (callargs_node(n)->block_arg)

// Group 8: Control Flow Statements
struct mrb_ast_break_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *value;
};

struct mrb_ast_next_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *value;
};

struct mrb_ast_redo_node {
  struct mrb_ast_var_header hdr;
};

struct mrb_ast_retry_node {
  struct mrb_ast_var_header hdr;
};

#define break_node(n) ((struct mrb_ast_break_node*)(n))
#define next_node(n) ((struct mrb_ast_next_node*)(n))
#define redo_node(n) ((struct mrb_ast_redo_node*)(n))
#define retry_node(n) ((struct mrb_ast_retry_node*)(n))
#define BREAK_NODE_VALUE(n) (break_node(n)->value)
#define NEXT_NODE_VALUE(n) (next_node(n)->value)

// Group 9: String and Regex Variants
struct mrb_ast_xstr_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *list;
};

struct mrb_ast_dregx_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *list;
  const char *flags;
  const char *encoding;
};


struct mrb_ast_heredoc_node {
  struct mrb_ast_var_header hdr;
  struct mrb_parser_heredoc_info info;
};

struct mrb_ast_dsym_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *list;
};

#define xstr_node(n) ((struct mrb_ast_xstr_node*)(n))
#define dregx_node(n) ((struct mrb_ast_dregx_node*)(n))
#define heredoc_node(n) ((struct mrb_ast_heredoc_node*)(n))
#define dsym_node(n) ((struct mrb_ast_dsym_node*)(n))

#define XSTR_NODE_LIST(n) (xstr_node(n)->list)
#define DREGX_NODE_LIST(n) (dregx_node(n)->list)
#define DREGX_NODE_OPTIONS(n) (dregx_node(n)->options)
#define HEREDOC_NODE_NAME(n) (heredoc_node(n)->name)
#define DSYM_NODE_LIST(n) (dsym_node(n)->list)

// Group 10: References and Variables
struct mrb_ast_nth_ref_node {
  struct mrb_ast_var_header hdr;
  int nth;
};

struct mrb_ast_back_ref_node {
  struct mrb_ast_var_header hdr;
  int type;
};

struct mrb_ast_nvar_node {
  struct mrb_ast_var_header hdr;
  int num;
};

struct mrb_ast_dvar_node {
  struct mrb_ast_var_header hdr;
  mrb_sym name;
};

struct mrb_ast_match_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *pattern;
};

#define nth_ref_node(n) ((struct mrb_ast_nth_ref_node*)(n))
#define back_ref_node(n) ((struct mrb_ast_back_ref_node*)(n))
#define nvar_node(n) ((struct mrb_ast_nvar_node*)(n))
#define dvar_node(n) ((struct mrb_ast_dvar_node*)(n))
#define match_node(n) ((struct mrb_ast_match_node*)(n))

#define NTH_REF_NODE_NTH(n) (nth_ref_node(n)->nth)
#define BACK_REF_NODE_TYPE(n) (back_ref_node(n)->type)
#define NVAR_NODE_NUM(n) (nvar_node(n)->num)
#define DVAR_NODE_NAME(n) (dvar_node(n)->name)
#define MATCH_NODE_PATTERN(n) (match_node(n)->pattern)

// Group 11: Operators and Expressions
struct mrb_ast_not_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *operand;
};

struct mrb_ast_negate_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *operand;
};

struct mrb_ast_colon2_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *base;
  mrb_sym name;
};

struct mrb_ast_colon3_node {
  struct mrb_ast_var_header hdr;
  mrb_sym name;
};

struct mrb_ast_defined_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *expr;
};

#define not_node(n) ((struct mrb_ast_not_node*)(n))
#define negate_node(n) ((struct mrb_ast_negate_node*)(n))
#define colon2_node(n) ((struct mrb_ast_colon2_node*)(n))
#define colon3_node(n) ((struct mrb_ast_colon3_node*)(n))
#define defined_node(n) ((struct mrb_ast_defined_node*)(n))

#define NOT_NODE_OPERAND(n) (not_node(n)->operand)
#define NEGATE_NODE_OPERAND(n) (negate_node(n)->operand)
#define COLON2_NODE_BASE(n) (colon2_node(n)->base)
#define COLON2_NODE_NAME(n) (colon2_node(n)->name)
#define COLON3_NODE_NAME(n) (colon3_node(n)->name)
#define DEFINED_NODE_EXPR(n) (defined_node(n)->expr)

// Group 12: Function Calls and Special Forms
struct mrb_ast_lambda_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *locals;
  struct mrb_ast_args *args;
  struct mrb_ast_node *body;
};

// Group 13: Containers and Collections
struct mrb_ast_zarray_node {
  struct mrb_ast_var_header hdr;
};

struct mrb_ast_kw_hash_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *args;
};

struct mrb_ast_words_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *args;
};

struct mrb_ast_symbols_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *args;
};

// Group 14: Arguments and Parameters

struct mrb_ast_splat_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *value;
};

struct mrb_ast_to_ary_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *value;
};

struct mrb_ast_svalue_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *value;
};

struct mrb_ast_block_arg_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *value;
};

// Group 15: Structural Nodes
struct mrb_ast_method_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *body;
};

struct mrb_ast_scope_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *locals;
  struct mrb_ast_node *body;
};

struct mrb_ast_begin_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *body;
};

struct mrb_ast_ensure_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *body;
  struct mrb_ast_node *ensure_clause;
};

struct mrb_ast_stmts_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *stmts;        /* Cons-list of statements */
};

struct mrb_ast_iter_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *vars;
  struct mrb_ast_node *body;
};

struct mrb_ast_when_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *cond;
  struct mrb_ast_node *body;
  struct mrb_ast_node *next_when;
};

// Group 16: Declarations and Definitions

struct mrb_ast_alias_node {
  struct mrb_ast_var_header hdr;
  mrb_sym new_name;
  mrb_sym old_name;
};

struct mrb_ast_undef_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *syms;
};

struct mrb_ast_postexe_node {
  struct mrb_ast_var_header hdr;
  struct mrb_ast_node *body;
};

#define zsuper_node(n) ((struct mrb_ast_super_node*)(n))
#define lambda_node(n) ((struct mrb_ast_lambda_node*)(n))
#define zarray_node(n) ((struct mrb_ast_zarray_node*)(n))
#define kw_hash_node(n) ((struct mrb_ast_kw_hash_node*)(n))
#define words_node(n) ((struct mrb_ast_words_node*)(n))
#define symbols_node(n) ((struct mrb_ast_symbols_node*)(n))
#define splat_node(n) ((struct mrb_ast_splat_node*)(n))
#define to_ary_node(n) ((struct mrb_ast_to_ary_node*)(n))
#define svalue_node(n) ((struct mrb_ast_svalue_node*)(n))
#define block_arg_node(n) ((struct mrb_ast_block_arg_node*)(n))
#define method_node(n) ((struct mrb_ast_method_node*)(n))
#define scope_node(n) ((struct mrb_ast_scope_node*)(n))
#define begin_node(n) ((struct mrb_ast_begin_node*)(n))
#define ensure_node(n) ((struct mrb_ast_ensure_node*)(n))
#define stmts_node(n) ((struct mrb_ast_stmts_node*)(n))
#define iter_node(n) ((struct mrb_ast_iter_node*)(n))
#define when_node(n) ((struct mrb_ast_when_node*)(n))
#define alias_node(n) ((struct mrb_ast_alias_node*)(n))
#define undef_node(n) ((struct mrb_ast_undef_node*)(n))
#define postexe_node(n) ((struct mrb_ast_postexe_node*)(n))
#define sdef_node(n) ((struct mrb_ast_sdef_node*)(n))

#define LAMBDA_NODE_LOCALS(n) (lambda_node(n)->locals)
#define LAMBDA_NODE_ARGS(n) (lambda_node(n)->args)
#define LAMBDA_NODE_BODY(n) (lambda_node(n)->body)
#define KW_HASH_NODE_ARGS(n) (kw_hash_node(n)->args)
#define WORDS_NODE_ARGS(n) (words_node(n)->args)
#define SYMBOLS_NODE_ARGS(n) (symbols_node(n)->args)
#define SPLAT_NODE_VALUE(n) (splat_node(n)->value)
#define TO_ARY_NODE_VALUE(n) (to_ary_node(n)->value)
#define SVALUE_NODE_VALUE(n) (svalue_node(n)->value)
#define BLOCK_ARG_NODE_VALUE(n) (block_arg_node(n)->value)
#define METHOD_NODE_BODY(n) (method_node(n)->body)
#define SCOPE_NODE_LOCALS(n) (scope_node(n)->locals)
#define SCOPE_NODE_BODY(n) (scope_node(n)->body)
#define STMTS_NODE_STMTS(n) (stmts_node(n)->stmts)
#define BEGIN_NODE_BODY(n) (begin_node(n)->body)
#define ENSURE_NODE_BODY(n) (ensure_node(n)->body)
#define ENSURE_NODE_ENSURE_CLAUSE(n) (ensure_node(n)->ensure_clause)
#define ITER_NODE_VARS(n) (iter_node(n)->vars)
#define ITER_NODE_BODY(n) (iter_node(n)->body)
#define WHEN_NODE_COND(n) (when_node(n)->cond)
#define WHEN_NODE_BODY(n) (when_node(n)->body)
#define WHEN_NODE_NEXT_WHEN(n) (when_node(n)->next_when)

#endif  /* MRUBY_COMPILER_NODE_H */
