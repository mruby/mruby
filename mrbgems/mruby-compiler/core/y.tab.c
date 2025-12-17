/* A Bison parser, made by Lrama 0.7.0.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2021 Free Software Foundation,
   Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <https://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output, and Bison version.  */
#define YYBISON 30802

/* Bison version string.  */
#define YYBISON_VERSION "3.8.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 1

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* First part of user prologue.  */
#line 7 "mrbgems/mruby-compiler/core/parse.y"

#undef PARSER_DEBUG
#ifdef PARSER_DEBUG
# define YYDEBUG 1
#endif
#define YYSTACK_USE_ALLOCA 1

#include <ctype.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/error.h>
#include <mruby/throw.h>
#include <mruby/string.h>
#include <mruby/dump.h>
#include <mruby/internal.h>
#include <mruby/presym.h>
#include "node.h"

#define YYLEX_PARAM p

#define mrbc_malloc(s) mrb_basic_alloc_func(NULL,(s))
#define mrbc_realloc(p,s) mrb_basic_alloc_func((p),(s))
#define mrbc_free(p) mrb_basic_alloc_func((p),0)

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;
typedef struct mrb_parser_heredoc_info parser_heredoc_info;

static int yyparse(parser_state *p);
static int yylex(void *lval, void *lp, parser_state *p);
static void yyerror(void *lp, parser_state *p, const char *s);
static void yywarning(parser_state *p, const char *s);
static void backref_error(parser_state *p, node *n);
static void void_expr_error(parser_state *p, node *n);
static void tokadd(parser_state *p, int32_t c);
static const char* tok(parser_state *p);
static int toklen(parser_state *p);

/* Forward declarations for variable-sized simple node functions */

/* Forward declarations for variable-sized advanced node functions */

/* Helper function to check node type for both traditional and variable-sized nodes */
static mrb_bool node_type_p(node *n, enum node_type type);

#define identchar(c) (ISALNUM(c) || (c) == '_' || !ISASCII(c))

typedef unsigned int stack_type;

#define BITSTACK_PUSH(stack, n) ((stack) = ((stack)<<1)|((n)&1))
#define BITSTACK_POP(stack)     ((stack) = (stack) >> 1)
#define BITSTACK_LEXPOP(stack)  ((stack) = ((stack) >> 1) | ((stack) & 1))
#define BITSTACK_SET_P(stack)   ((stack)&1)

#define COND_PUSH(n)    BITSTACK_PUSH(p->cond_stack, (n))
#define COND_POP()      BITSTACK_POP(p->cond_stack)
#define COND_LEXPOP()   BITSTACK_LEXPOP(p->cond_stack)
#define COND_P()        BITSTACK_SET_P(p->cond_stack)

#define CMDARG_PUSH(n)  BITSTACK_PUSH(p->cmdarg_stack, (n))
#define CMDARG_POP()    BITSTACK_POP(p->cmdarg_stack)
#define CMDARG_LEXPOP() BITSTACK_LEXPOP(p->cmdarg_stack)
#define CMDARG_P()      BITSTACK_SET_P(p->cmdarg_stack)

#define SET_LINENO(c,n) (((struct mrb_ast_var_header*)(c))->lineno = (n))

#define NUM_SUFFIX_R   (1<<0)
#define NUM_SUFFIX_I   (1<<1)

static inline mrb_sym
intern_cstr_gen(parser_state *p, const char *s)
{
  return mrb_intern_cstr(p->mrb, s);
}
#define intern_cstr(s) intern_cstr_gen(p,(s))

static inline mrb_sym
intern_gen(parser_state *p, const char *s, size_t len)
{
  return mrb_intern(p->mrb, s, len);
}
#define intern(s,len) intern_gen(p,(s),(len))

#define intern_op(op) MRB_OPSYM_2(p->mrb, op)

static mrb_sym
intern_numparam_gen(parser_state *p, int num)
{
  char buf[3];
  buf[0] = '_'; buf[1] = '0'+num; buf[2] = '\0';
  return intern(buf, 2);
}
#define intern_numparam(n) intern_numparam_gen(p,(n))

static void
cons_free_gen(parser_state *p, node *cons)
{
  cons->cdr = p->cells;
  p->cells = cons;
}
#define cons_free(c) cons_free_gen(p, (c))

static void*
parser_palloc(parser_state *p, size_t size)
{
  void *m = mempool_alloc(p->pool, size);

  if (!m) {
    MRB_THROW(p->mrb->jmp);
  }
  return m;
}

#define parser_pfree(ptr) do { if (sizeof(node) <= sizeof(*(ptr))) cons_free((node*)ptr);} while (0)

static node*
cons_gen(parser_state *p, node *car, node *cdr)
{
  struct mrb_ast_node *c;

  /* Try to reuse from free list first - only for 16-byte nodes */
  if (p->cells) {
    c = (struct mrb_ast_node*)p->cells;
    p->cells = p->cells->cdr;
  }
  else {
    c = (struct mrb_ast_node*)parser_palloc(p, sizeof(struct mrb_ast_node));
  }
  c->car = car;
  c->cdr = cdr;
  /* Don't initialize location fields for structure nodes - saves CPU */
  return (node*)c;
}

/* Head-only location optimization: separate functions for head vs structure nodes */
#define cons(a,b) cons_gen(p,(a),(b))           /* Structure nodes - no location */
/* Initialize variable node header */
static void
init_var_header(struct mrb_ast_var_header *header, parser_state *p, enum node_type type)
{
  header->lineno = p->lineno;
  header->filename_index = p->current_filename_index;
  header->node_type = (uint8_t)type;

  /* Handle file boundary edge case */
  if (p->lineno == 0 && p->current_filename_index > 0) {
    header->filename_index--;
  }
}

/* Combined allocate + init header helper */
static inline void*
new_node(parser_state *p, size_t size, enum node_type type)
{
  void *n = parser_palloc(p, size);
  init_var_header((struct mrb_ast_var_header*)n, p, type);
  return n;
}

/* Type-safe macro wrapper for node allocation */
#define NEW_NODE(type_name, node_type) \
  (struct mrb_ast_##type_name##_node*)new_node(p, sizeof(struct mrb_ast_##type_name##_node), node_type)

static node*
list1_gen(parser_state *p, node *a)
{
  return cons(a, 0);
}
#define list1(a) list1_gen(p, (a))

static node*
list2_gen(parser_state *p, node *a, node *b)
{
  return cons(a, cons(b, 0));
}
#define list2(a,b) list2_gen(p, (a),(b))

static node*
list3_gen(parser_state *p, node *a, node *b, node *c)
{
  return cons(a, cons(b, cons(c, 0)));
}
#define list3(a,b,c) list3_gen(p, (a),(b),(c))

static node*
append_gen(parser_state *p, node *a, node *b)
{
  node *c = a;

  if (!a) return b;
  if (!b) return a;
  while (c->cdr) {
    c = c->cdr;
  }
  c->cdr = b;
  return a;
}
#define append(a,b) append_gen(p,(a),(b))
#define push(a,b) append_gen(p,(a),list1(b))

static char*
parser_strndup(parser_state *p, const char *s, size_t len)
{
  char *b = (char*)parser_palloc(p, len+1);

  memcpy(b, s, len);
  b[len] = '\0';
  return b;
}
#undef strndup
#define strndup(s,len) parser_strndup(p, s, len)

static char*
parser_strdup(parser_state *p, const char *s)
{
  return parser_strndup(p, s, strlen(s));
}
#undef strdup
#define strdup(s) parser_strdup(p, s)

static void
dump_int(uint16_t i, char *s)
{
  char *p = s;
  char *t = s;

  while (i > 0) {
    *p++ = (i % 10)+'0';
    i /= 10;
  }
  if (p == s) *p++ = '0';
  *p = 0;
  p--;  /* point the last char */
  while (t < p) {
    char c = *t;
    *t++ = *p;
    *p-- = c;
  }
}

/* xxx ----------------------------- */

static node*
local_switch(parser_state *p)
{
  node *prev = p->locals;

  p->locals = cons(0, 0);
  return prev;
}

static void
local_resume(parser_state *p, node *prev)
{
  p->locals = prev;
}

static void
local_nest(parser_state *p)
{
  p->locals = cons(0, p->locals);
}

static void
local_unnest(parser_state *p)
{
  if (p->locals) {
    p->locals = p->locals->cdr;
  }
}

static mrb_bool
local_var_p(parser_state *p, mrb_sym sym)
{
  const struct RProc *u;
  node *l = p->locals;

  while (l) {
    node *n = l->car;
    while (n) {
      if (node_to_sym(n->car) == sym) return TRUE;
      n = n->cdr;
    }
    l = l->cdr;
  }

  u = p->upper;
  while (u && !MRB_PROC_CFUNC_P(u)) {
    const struct mrb_irep *ir = u->body.irep;
    const mrb_sym *v = ir->lv;
    int i;

    if (v) {
      for (i=0; i+1 < ir->nlocals; i++) {
        if (v[i] == sym) return TRUE;
      }
    }
    if (MRB_PROC_SCOPE_P(u)) break;
    u = u->upper;
  }
  return FALSE;
}

static void
local_add_f(parser_state *p, mrb_sym sym)
{
  if (p->locals) {
    node *n = p->locals->car;
    while (n) {
      if (node_to_sym(n->car) == sym) {
        mrb_int len;
        const char* name = mrb_sym_name_len(p->mrb, sym, &len);
        if (len > 0 && name[0] != '_') {
          yyerror(NULL, p, "duplicated argument name");
          return;
        }
      }
      n = n->cdr;
    }
    p->locals->car = push(p->locals->car, sym_to_node(sym));
  }
}

static void
local_add(parser_state *p, mrb_sym sym)
{
  if (!local_var_p(p, sym)) {
    local_add_f(p, sym);
  }
}

/* allocate register for block */
#define local_add_blk(p) local_add_f(p, 0)

static void
local_add_kw(parser_state *p, mrb_sym kwd)
{
  /* allocate register for keywords hash */
  local_add_f(p, kwd ? kwd : intern_op(pow));
}

static node*
locals_node(parser_state *p)
{
  return p->locals ? p->locals->car : NULL;
}

/* Helper function to check node type for both traditional and variable-sized nodes */
static mrb_bool
node_type_p(node *n, enum node_type type)
{
  if (!n) return FALSE;

  /* Check if this is a variable-sized node */
  struct mrb_ast_var_header *header = (struct mrb_ast_var_header*)n;
  return ((enum node_type)header->node_type == type);
}

/* Helper functions for variable-sized node detection */
static enum node_type
get_node_type(node *n)
{
  if (!n) return (enum node_type)0;

  /* Try to interpret as variable-sized node */
  struct mrb_ast_var_header *header = (struct mrb_ast_var_header*)n;
  enum node_type type = (enum node_type)header->node_type;

  /* Validate that the node type is within valid range for variable-sized nodes */
  if (type >= NODE_SCOPE && type < NODE_LAST) {
    return type;
  }

  /* If node type is invalid, this is likely a cons-list node */
  /* Return a special sentinel value to indicate cons-list fallback */
  return NODE_LAST; /* Use NODE_LAST as sentinel for cons-list nodes */
}

static void
nvars_nest(parser_state *p)
{
  p->nvars = cons(int_to_node(0), p->nvars);
}

static void
nvars_block(parser_state *p)
{
  p->nvars = cons(int_to_node(-2), p->nvars);
}

static void
nvars_unnest(parser_state *p)
{
  p->nvars = p->nvars->cdr;
}

/* struct: scope_node(locals, body) */
static node*
new_scope(parser_state *p, node *body)
{
  struct mrb_ast_scope_node *scope_node = NEW_NODE(scope, NODE_SCOPE);
  scope_node->locals = locals_node(p);
  scope_node->body = body;
  return (node*)scope_node;
}

/* struct: stmts_node(stmts) - uses cons list */
static node*
new_stmts(parser_state *p, node *body)
{
  struct mrb_ast_stmts_node *n = NEW_NODE(stmts, NODE_STMTS);
  n->stmts = body ? list1(body) : 0;  /* Wrap single statement in cons-list */

  return (node*)n;
}

/* Helper: push statement to stmts node */
static node*
stmts_push(parser_state *p, node *stmts, node *stmt)
{
  struct mrb_ast_stmts_node *n = stmts_node(stmts);
  n->stmts = push(n->stmts, stmt);
  return stmts;
}

/* struct: begin_node(body) */
static node*
new_begin(parser_state *p, node *body)
{
  struct mrb_ast_begin_node *begin_node = NEW_NODE(begin, NODE_BEGIN);
  begin_node->body = body;
  return (node*)begin_node;
}

#define newline_node(n) (n)

/* struct: rescue_node(body, rescue_clauses, else_clause) */
static node*
new_rescue(parser_state *p, node *body, node *resq, node *els)
{
  struct mrb_ast_rescue_node *n = NEW_NODE(rescue, NODE_RESCUE);
  n->body = body;
  n->rescue_clauses = resq;
  n->else_clause = els;

  return (node*)n;
}

static node*
new_mod_rescue(parser_state *p, node *body, node *resq)
{
  return new_rescue(p, body, list1(list3(0, 0, resq)), 0);
}

/* struct: ensure_node(body, ensure_clause) */
static node*
new_ensure(parser_state *p, node *a, node *b)
{
  struct mrb_ast_ensure_node *ensure_node = NEW_NODE(ensure, NODE_ENSURE);
  ensure_node->body = a;
  ensure_node->ensure_clause = b;
  return (node*)ensure_node;
}

/* struct: nil_node() */
static node*
new_nil(parser_state *p)
{
  struct mrb_ast_nil_node *n = NEW_NODE(nil, NODE_NIL);

  return (node*)n;
}

/* struct: true_node() */
static node*
new_true(parser_state *p)
{
  struct mrb_ast_true_node *n = NEW_NODE(true, NODE_TRUE);

  return (node*)n;
}

/* struct: false_node() */
static node*
new_false(parser_state *p)
{
  struct mrb_ast_false_node *n = NEW_NODE(false, NODE_FALSE);

  return (node*)n;
}

/* struct: alias_node(new_name, old_name) */
static node*
new_alias(parser_state *p, mrb_sym a, mrb_sym b)
{
  struct mrb_ast_alias_node *alias_node = NEW_NODE(alias, NODE_ALIAS);
  alias_node->new_name = a;
  alias_node->old_name = b;
  return (node*)alias_node;
}

/* struct: if_node(cond, then_body, else_body) */
static node*
new_if(parser_state *p, node *condition, node *then_body, node *else_body)
{
  void_expr_error(p, condition);

  struct mrb_ast_if_node *n = NEW_NODE(if, NODE_IF);
  n->condition = condition;
  n->then_body = then_body;
  n->else_body = else_body;

  return (node*)n;
}

/* struct: while_node(cond, body) */
static node*
new_while(parser_state *p, node *condition, node *body)
{
  void_expr_error(p, condition);

  struct mrb_ast_while_node *n = NEW_NODE(while, NODE_WHILE);
  n->condition = condition;
  n->body = body;

  return (node*)n;
}

/* struct: until_node(cond, body) */
static node*
new_until(parser_state *p, node *condition, node *body)
{
  void_expr_error(p, condition);

  struct mrb_ast_until_node *n = NEW_NODE(until, NODE_UNTIL);
  n->condition = condition;
  n->body = body;

  return (node*)n;
}

/* struct: while_node(cond, body) */
static node*
new_while_mod(parser_state *p, node *condition, node *body)
{
  node *while_node = new_while(p, condition, body);
  struct mrb_ast_while_node *n = (struct mrb_ast_while_node*)while_node;
  n->header.node_type = NODE_WHILE_MOD;
  return while_node;
}

/* struct: until_node(cond, body) */
static node*
new_until_mod(parser_state *p, node *a, node *b)
{
  node *until_node = new_until(p, a, b);
  struct mrb_ast_until_node *n = (struct mrb_ast_until_node*)until_node;
  n->header.node_type = NODE_UNTIL_MOD;
  return until_node;
}


/* struct: for_node(var, obj, body) */
static node*
new_for(parser_state *p, node *v, node *o, node *b)
{
  void_expr_error(p, o);

  struct mrb_ast_for_node *n = NEW_NODE(for, NODE_FOR);
  n->var = v;
  n->iterable = o;
  n->body = b;

  return (node*)n;
}

/* struct: case_node(expr, when_clauses) - uses cons list */
static node*
new_case(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);

  struct mrb_ast_case_node *n = NEW_NODE(case, NODE_CASE);
  n->value = a;
  n->body = b;

  return (node*)n;
}

/* Pattern matching case/in expression */
static node*
new_case_match(parser_state *p, node *val, node *in_clauses)
{
  void_expr_error(p, val);

  struct mrb_ast_case_match_node *n = NEW_NODE(case_match, NODE_CASE_MATCH);
  n->value = val;
  n->in_clauses = in_clauses;

  return (node*)n;
}

/* Create value pattern node */
static node*
new_pat_value(parser_state *p, node *val)
{
  struct mrb_ast_pat_value_node *n = NEW_NODE(pat_value, NODE_PAT_VALUE);
  n->value = val;
  return (node*)n;
}

/* Create variable pattern node */
static node*
new_pat_var(parser_state *p, mrb_sym name)
{
  struct mrb_ast_pat_var_node *n = NEW_NODE(pat_var, NODE_PAT_VAR);
  n->name = name;
  /* Register as local variable if not wildcard */
  if (name) {
    local_add(p, name);
  }
  return (node*)n;
}

/* Create pin pattern node (^var) */
static node*
new_pat_pin(parser_state *p, mrb_sym name)
{
  struct mrb_ast_pat_pin_node *n = NEW_NODE(pat_pin, NODE_PAT_PIN);
  n->name = name;
  /* Pin operator references existing variable, does not create new binding */
  return (node*)n;
}

/* Create as pattern node (pattern => var) */
static node*
new_pat_as(parser_state *p, node *pattern, mrb_sym name)
{
  struct mrb_ast_pat_as_node *n = NEW_NODE(pat_as, NODE_PAT_AS);
  n->pattern = pattern;
  n->name = name;
  local_add(p, name);
  return (node*)n;
}

/* Create alternative pattern node (pat1 | pat2) */
static node*
new_pat_alt(parser_state *p, node *left, node *right)
{
  struct mrb_ast_pat_alt_node *n = NEW_NODE(pat_alt, NODE_PAT_ALT);
  n->left = left;
  n->right = right;
  return (node*)n;
}

/* Create array pattern node [a, b, *rest, c] */
static node*
new_pat_array(parser_state *p, node *pre, node *rest, node *post)
{
  struct mrb_ast_pat_array_node *n = NEW_NODE(pat_array, NODE_PAT_ARRAY);
  n->pre = pre;
  n->rest = rest;
  n->post = post;
  return (node*)n;
}

/* Create hash pattern node {a:, b: x, **rest} */
static node*
new_pat_hash(parser_state *p, node *pairs, node *rest)
{
  struct mrb_ast_pat_hash_node *n = NEW_NODE(pat_hash, NODE_PAT_HASH);
  n->pairs = pairs;
  n->rest = rest;
  return (node*)n;
}

/* Create one-line pattern matching node (expr in pattern / expr => pattern) */
static node*
new_match_pat(parser_state *p, node *value, node *pattern, mrb_bool raise_on_fail)
{
  struct mrb_ast_match_pat_node *n = NEW_NODE(match_pat, NODE_MATCH_PAT);
  n->value = value;
  n->pattern = pattern;
  n->raise_on_fail = raise_on_fail;
  return (node*)n;
}

/* Create in-clause node for case/in */
static node*
new_in(parser_state *p, node *pattern, node *guard, node *body, mrb_bool guard_is_unless)
{
  struct mrb_ast_in_node *n = NEW_NODE(in, NODE_IN);
  n->pattern = pattern;
  n->guard = guard;
  n->body = body;
  n->guard_is_unless = guard_is_unless;
  return (node*)n;
}

/* struct: postexe_node(body) */
static node*
new_postexe(parser_state *p, node *a)
{
  struct mrb_ast_postexe_node *postexe_node = NEW_NODE(postexe, NODE_POSTEXE);
  postexe_node->body = a;
  return (node*)postexe_node;
}

/* struct: self_node() */
static node*
new_self(parser_state *p)
{
  struct mrb_ast_self_node *n = NEW_NODE(self, NODE_SELF);

  return (node*)n;
}

/* struct: call_node(receiver, method, args) */
static node*
new_call(parser_state *p, node *receiver, mrb_sym method, node *args, int pass)
{
  /* Calculate size needed (fixed size now) */  struct mrb_ast_call_node *n = NEW_NODE(call, NODE_CALL);
  n->receiver = receiver;
  n->method_name = method;
  n->safe_call = (pass == 0); /* pass == 0 means safe call (&.) */

  /* Store args pointer directly - no need to unpack and repack */
  n->args = args;

  void_expr_error(p, receiver);
  return (node*)n;
}

/* struct: fcall_node(method, args) */
static node*
new_fcall(parser_state *p, mrb_sym b, node *c)
{
  return new_call(p, NULL, b, c, '.');
}

/* (a b . c) */
static node*
new_callargs(parser_state *p, node *a, node *b, node *c)
{
  /* Allocate struct mrb_ast_callargs (fixed size, like new_args) */
  struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)parser_palloc(p, sizeof(struct mrb_ast_callargs));

  /* Initialize members directly */
  callargs->regular_args = a;   /* Cons list of regular arguments (preserves splat compatibility) */
  callargs->keyword_args = b;   /* Keyword arguments hash node */
  callargs->block_arg = c;      /* Block argument node */

  /* Return direct cast to node (like new_args) */
  return (node*)callargs;
}

/* struct: super_node(args) */
static node*
new_super(parser_state *p, node *c)
{
  struct mrb_ast_super_node *n = NEW_NODE(super, NODE_SUPER);
  n->args = c;

  return (node*)n;
}

/* struct: zsuper_node() */
static node*
new_zsuper(parser_state *p)
{
  struct mrb_ast_super_node *n = NEW_NODE(super, NODE_ZSUPER);
  n->args = NULL;  /* zsuper initially has no args, but may be added by call_with_block */
  return (node*)n;
}

/* struct: yield_node(args) */
static node*
new_yield(parser_state *p, node *c)
{
  /* Handle callargs structure - direct casting like new_args() */
  if (c) {
    struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)c;
    if (callargs->block_arg) {
      yyerror(NULL, p, "both block arg and actual block given");
    }
  }  struct mrb_ast_yield_node *n = NEW_NODE(yield, NODE_YIELD);
  n->args = c;

  return (node*)n;
}

/* struct: return_node(value) */
static node*
new_return(parser_state *p, node *c)
{
  struct mrb_ast_return_node *n = NEW_NODE(return, NODE_RETURN);
  n->args = c;

  return (node*)n;
}

/* struct: break_node(value) */
static node*
new_break(parser_state *p, node *c)
{
  struct mrb_ast_break_node *n = NEW_NODE(break, NODE_BREAK);
  n->value = c;
  return (node*)n;
}

/* struct: next_node(value) */
static node*
new_next(parser_state *p, node *c)
{
  struct mrb_ast_next_node *n = NEW_NODE(next, NODE_NEXT);
  n->value = c;
  return (node*)n;
}

/* struct: redo_node() */
static node*
new_redo(parser_state *p)
{
  struct mrb_ast_redo_node *n = NEW_NODE(redo, NODE_REDO);
  return (node*)n;
}

/* struct: retry_node() */
static node*
new_retry(parser_state *p)
{
  struct mrb_ast_retry_node *n = NEW_NODE(retry, NODE_RETRY);
  return (node*)n;
}

/* struct: dot2_node(beg, end) */
static node*
new_dot2(parser_state *p, node *a, node *b)
{
  struct mrb_ast_dot2_node *n = NEW_NODE(dot2, NODE_DOT2);
  n->left = a;
  n->right = b;

  return (node*)n;
}

/* struct: dot3_node(beg, end) */
static node*
new_dot3(parser_state *p, node *a, node *b)
{
  struct mrb_ast_dot3_node *n = NEW_NODE(dot3, NODE_DOT3);
  n->left = a;
  n->right = b;

  return (node*)n;
}

/* struct: colon2_node(base, name) */
static node*
new_colon2(parser_state *p, node *b, mrb_sym c)
{
  void_expr_error(p, b);

  struct mrb_ast_colon2_node *colon2_node = NEW_NODE(colon2, NODE_COLON2);
  colon2_node->base = b;
  colon2_node->name = c;
  return (node*)colon2_node;
}

/* struct: colon3_node(name) */
static node*
new_colon3(parser_state *p, mrb_sym c)
{
  struct mrb_ast_colon3_node *colon3_node = NEW_NODE(colon3, NODE_COLON3);
  colon3_node->name = c;
  return (node*)colon3_node;
}

/* struct: and_node(left, right) */
static node*
new_and(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);

  struct mrb_ast_and_node *n = NEW_NODE(and, NODE_AND);
  n->left = a;
  n->right = b;

  return (node*)n;
}

/* struct: or_node(left, right) */
static node*
new_or(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);

  struct mrb_ast_or_node *n = NEW_NODE(or, NODE_OR);
  n->left = a;
  n->right = b;

  return (node*)n;
}

/* struct: array_node(elements) - uses cons list */
static node*
new_array(parser_state *p, node *a)
{
  struct mrb_ast_array_node *n = NEW_NODE(array, NODE_ARRAY);
  n->elements = a;

  return (node*)n;
}

/* struct: splat_node(value) */
static node*
new_splat(parser_state *p, node *a)
{
  void_expr_error(p, a);

  struct mrb_ast_splat_node *splat_node = NEW_NODE(splat, NODE_SPLAT);
  splat_node->value = a;
  return (node*)splat_node;
}

/* struct: hash_node(pairs) - uses cons list */
static node*
new_hash(parser_state *p, node *a)
{
  struct mrb_ast_hash_node *n = NEW_NODE(hash, NODE_HASH);
  n->pairs = a;

  return (node*)n;
}

/* (:sym . a) */
/* Symbol node creation - supports both variable and legacy modes */
static node*
new_sym(parser_state *p, mrb_sym sym)
{
  struct mrb_ast_sym_node *n = NEW_NODE(sym, NODE_SYM);
  n->symbol = sym;

  return (node*)n;
}

static node*
new_xvar(parser_state *p, mrb_sym sym, enum node_type type)
{
  struct mrb_ast_var_node *n = NEW_NODE(var, type);
  n->symbol = sym;

  return (node*)n;
}

#define new_lvar(p, sym) new_xvar(p, sym, NODE_LVAR)
#define new_ivar(p, sym) new_xvar(p, sym, NODE_IVAR)
#define new_gvar(p, sym) new_xvar(p, sym, NODE_GVAR)
#define new_cvar(p, sym) new_xvar(p, sym, NODE_CVAR)

static mrb_sym
new_strsym(parser_state *p, node* str)
{
  size_t len = (size_t)str->car;
  const char *s = (const char*)str->cdr;

  return mrb_intern(p->mrb, s, len);
}

/* (:nvar . a) */
static node*
new_nvar(parser_state *p, int num)
{
  int nvar;
  node *nvars = p->nvars->cdr;
  while (nvars) {
    nvar = node_to_int(nvars->car);
    if (nvar == -2) break; /* top of the scope */
    if (nvar > 0) {
      yyerror(NULL, p, "numbered parameter used in outer block");
      break;
    }
    nvars->car = int_to_node(-1);
    nvars = nvars->cdr;
  }
  nvar = node_to_int(p->nvars->car);
  if (nvar == -1) {
    yyerror(NULL, p, "numbered parameter used in inner block");
  }
  else {
    p->nvars->car = int_to_node(nvar > num ? nvar : num);
  }
  struct mrb_ast_nvar_node *n = NEW_NODE(nvar, NODE_NVAR);
  n->num = num;
  return (node*)n;
}

/* struct: const_node(name) */
static node*
new_const(parser_state *p, mrb_sym sym)
{
  struct mrb_ast_const_node *n = NEW_NODE(const, NODE_CONST);
  n->symbol = sym;

  return (node*)n;
}

/* struct: undef_node(syms) - uses cons list */
static node*
new_undef(parser_state *p, node *syms)
{
  struct mrb_ast_undef_node *undef_node = NEW_NODE(undef, NODE_UNDEF);
  undef_node->syms = syms;
  return (node*)undef_node;
}

/* struct: class_node(path, super, body) */
static node*
new_class(parser_state *p, node *c, node *s, node *b)
{
  void_expr_error(p, s);

  struct mrb_ast_class_node *n = NEW_NODE(class, NODE_CLASS);
  n->name = c;
  n->superclass = s;
  n->body = cons(locals_node(p), b);

  return (node*)n;
}

/* struct: sclass_node(obj, body) */
static node*
new_sclass(parser_state *p, node *o, node *b)
{
  void_expr_error(p, o);

  struct mrb_ast_sclass_node *n = NEW_NODE(sclass, NODE_SCLASS);
  n->obj = o;
  n->body = cons(locals_node(p), b);

  return (node*)n;
}

/* struct: module_node(path, body) */
static node*
new_module(parser_state *p, node *m, node *b)
{
  struct mrb_ast_module_node *n = NEW_NODE(module, NODE_MODULE);
  n->name = m;
  n->body = cons(locals_node(p), b);

  return (node*)n;
}

/* struct: def_node(name, args, body) */
static node*
new_def(parser_state *p, mrb_sym name)
{
  struct mrb_ast_def_node *n = NEW_NODE(def, NODE_DEF);
  n->name = name;
  n->args = (struct mrb_ast_args *)int_to_node(p->cmdarg_stack);
  n->locals = local_switch(p);
  n->body = NULL;

  return (node*)n;
}

static void
defn_setup(parser_state *p, node *d, node *a, node *b)
{
  struct mrb_ast_def_node *n = def_node(d);
  node *locals = n->locals;

  n->locals = locals_node(p);
  p->cmdarg_stack = node_to_int(n->args);
  n->args = (struct mrb_ast_args *)a;
  n->body = b;
  local_resume(p, locals);
}

/* struct: sdef_node(obj, name, args, body) */
static node*
new_sdef(parser_state *p, node *o, mrb_sym name)
{
  void_expr_error(p, o);

  struct mrb_ast_sdef_node *sdef_node = NEW_NODE(sdef, NODE_SDEF);
  sdef_node->obj = o;
  sdef_node->name = name;
  sdef_node->args = (struct mrb_ast_args *)int_to_node(p->cmdarg_stack);
  sdef_node->locals = local_switch(p);
  sdef_node->body = NULL;
  return (node*)sdef_node;
}

static void
local_add_margs(parser_state *p, node *n)
{
  while (n) {
    if (get_node_type(n->car) == NODE_MARG) {
      struct mrb_ast_masgn_node *masgn_n = (struct mrb_ast_masgn_node*)n->car;
      node *rhs = masgn_n->rhs;

      /* For parameter destructuring, rhs contains the locals */
      if (rhs) {
        node *t = rhs;
        while (t) {
          local_add_f(p, node_to_sym(t->car));
          t = t->cdr;
        }
        /* Clear cons list RHS immediately after use */
        masgn_n->rhs = NULL;
      }

      /* Process nested destructuring in lhs components */
      if (masgn_n->pre) {
        local_add_margs(p, masgn_n->pre);
      }
      if (masgn_n->post) {
        local_add_margs(p, masgn_n->post);
      }
    }
    n = n->cdr;
  }
}


static void
local_add_lv(parser_state *p, node *lv)
{
  while (lv) {
    local_add_f(p, node_to_sym(lv->car));
    lv = lv->cdr;
  }
}

/* (m o r m2 tail) */
/* m: (a b c) */
/* o: ((a . e1) (b . e2)) */
/* r: a */
/* m2: (a b c) */
/* b: a */
static node*
new_args(parser_state *p, node *m, node *opt, mrb_sym rest, node *m2, node *tail)
{
  local_add_margs(p, m);
  local_add_margs(p, m2);

  /* Save original optional arguments before processing */
  node *orig_opt = opt;

  /* Process optional arguments (keep original side effects) */
  while (opt) {
    /* opt: (sym . (opt . lv)) -> (sym . opt) */
    local_add_lv(p, opt->car->cdr->cdr);
    opt->car->cdr = opt->car->cdr->car;
    opt = opt->cdr;
  }

  /* Allocate struct mrb_ast_args (no hdr) */
  struct mrb_ast_args *args = (struct mrb_ast_args*)parser_palloc(p, sizeof(struct mrb_ast_args));

  /* Initialize members */
  args->mandatory_args = m;
  args->optional_args = orig_opt;
  args->rest_arg = rest;
  args->post_mandatory_args = m2;

  /* Deconstruct tail cons list: (kws . (kwrest . blk)) */
  if (tail) {
    args->keyword_args = (node*)tail->car;          /* kws */
    args->kwrest_arg = (mrb_sym)(intptr_t)tail->cdr->car; /* kwrest */
    args->block_arg = (mrb_sym)(intptr_t)tail->cdr->cdr;  /* blk */
    cons_free(tail->cdr);
    cons_free(tail);
  }
  else {
    args->keyword_args = NULL;
    args->kwrest_arg = 0;
    args->block_arg = 0;
  }

  return (node*)args;
}

/* struct: args_tail_node(kwargs, kwrest, block) */
static node*
new_args_tail(parser_state *p, node *kws, mrb_sym kwrest, mrb_sym blk)
{
  node *k;

  if (kws || kwrest) {
    local_add_kw(p, kwrest);
  }

  local_add_blk(p);
  if (blk) local_add_f(p, blk);

  /* allocate register for keywords arguments */
  /* order is for Proc#parameters */
  for (k = kws; k; k = k->cdr) {
    if (!k->car->cdr) { /* allocate required keywords - simplified structure: (key . NULL) */
      local_add_f(p, node_to_sym(k->car->car));
    }
  }
  for (k = kws; k; k = k->cdr) {
    if (k->car->cdr) { /* allocate keywords with default - simplified structure: (key . value) */
      local_add_lv(p, k->car->cdr->cdr);  /* value->cdr for default args */
      k->car->cdr = k->car->cdr->car;    /* value->car for default args */
      local_add_f(p, node_to_sym(k->car->car));
    }
  }

  /* Return cons list: (keyword . (kwrest . blk)) */
  return cons(kws, cons(sym_to_node(kwrest), sym_to_node(blk)));
}

/* (kw_sym . def_arg) - simplified from NODE_KW_ARG wrapper */
static node*
new_kw_arg(parser_state *p, mrb_sym kw, node *def_arg)
{
  mrb_assert(kw);
  return cons(sym_to_node(kw), def_arg);
}

/* (:kw_rest_args . a) */
static node*
new_kw_rest_args(parser_state *p, mrb_sym sym)
{
  return sym_to_node(intern_op(pow));  /* Use ** symbol as direct marker */
}

static node*
new_args_dots(parser_state *p, node *m)
{
  mrb_sym r = intern_op(mul);
  mrb_sym k = intern_op(pow);
  mrb_sym b = intern_op(and);
  local_add_f(p, r);
  return new_args(p, m, 0, r, 0, new_args_tail(p, NULL, k, b));
}

/* struct: block_arg_node(value) */
static node*
new_block_arg(parser_state *p, node *a)
{
  struct mrb_ast_block_arg_node *block_arg_node = NEW_NODE(block_arg, NODE_BLOCK_ARG);
  block_arg_node->value = a;
  return (node*)block_arg_node;
}

static node*
setup_numparams(parser_state *p, node *a)
{
  int nvars = node_to_int(p->nvars->car);
  if (nvars > 0) {
    int i;
    mrb_sym sym;
    // Check if any arguments are already defined
    struct mrb_ast_args *args = (struct mrb_ast_args *)a;
    if (a && (args->mandatory_args || args->optional_args || args->rest_arg ||
              args->post_mandatory_args || args->keyword_args || args->kwrest_arg)) {
      yyerror(NULL, p, "ordinary parameter is defined");
    }
    else if (p->locals) {
      /* p->locals should not be NULL unless error happens before the point */
      node* args = 0;
      for (i = nvars; i > 0; i--) {
        char buf[3];

        buf[0] = '_';
        buf[1] = i+'0';
        buf[2] = '\0';
        sym = intern_cstr(buf);
        args = cons(new_lvar(p, sym), args);
        p->locals->car = cons(sym_to_node(sym), p->locals->car);
      }
      a = new_args(p, args, 0, 0, 0, 0);
    }
  }
  return a;
}

/* struct: block_node(args, body) */
static node*
new_block(parser_state *p, node *a, node *b)
{
  a = setup_numparams(p, a);  struct mrb_ast_block_node *n = NEW_NODE(block, NODE_BLOCK);
  n->locals = locals_node(p);
  n->args = (struct mrb_ast_args *)a;
  n->body = b;

  return (node*)n;
}

/* struct: lambda_node(args, body) */
static node*
new_lambda(parser_state *p, node *a, node *b)
{
  a = setup_numparams(p, a);  struct mrb_ast_lambda_node *lambda_node = NEW_NODE(lambda, NODE_LAMBDA);
  lambda_node->locals = locals_node(p);
  lambda_node->args = (struct mrb_ast_args *)a;
  lambda_node->body = b;
  return (node*)lambda_node;
}

/* struct: asgn_node(lhs, rhs) */
static node*
new_asgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);

  struct mrb_ast_asgn_node *n = NEW_NODE(asgn, NODE_ASGN);
  n->lhs = a;
  n->rhs = b;

  return (node*)n;
}

/* Helper function to create MASGN/MARG nodes */
static node*
new_masgn_helper(parser_state *p, node *a, node *b, enum node_type node_type)
{
  struct mrb_ast_masgn_node *n = NEW_NODE(masgn, node_type);

  /* Extract pre, rest, post from cons list structure (a b c) */
  if (a) {
    n->pre = a->car;  /* Pre-splat variables */
    if (a->cdr) {
      n->rest = a->cdr->car;  /* Splat variable (or -1 for anonymous) */
      if (a->cdr->cdr) {
        n->post = a->cdr->cdr->car;  /* Post-splat variables */
        cons_free(a->cdr->cdr);
      }
      else {
        n->post = NULL;
      }
      cons_free(a->cdr);
    }
    else {
      n->rest = NULL;
      n->post = NULL;
    }
    cons_free(a);
  }
  else {
    n->pre = NULL;
    n->rest = NULL;
    n->post = NULL;
  }
  n->rhs = b;

  return (node*)n;
}

/* struct: masgn_node(lhs, rhs) */
static node*
new_masgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return new_masgn_helper(p, a, b, NODE_MASGN);
}

/* (:marg mlhs mrhs) no check - for parameter destructuring */
static node*
new_marg(parser_state *p, node *a)
{
  return new_masgn_helper(p, a, p->locals->car, NODE_MARG);
}

/* struct: op_asgn_node(lhs, op, rhs) */
static node*
new_op_asgn(parser_state *p, node *a, mrb_sym op, node *b)
{
  void_expr_error(p, b);

  struct mrb_ast_op_asgn_node *n = NEW_NODE(op_asgn, NODE_OP_ASGN);
  n->lhs = a;
  n->op = op;
  n->rhs = b;
  return (node*)n;
}

static node*
new_int_n(parser_state *p, int32_t val)
{
  struct mrb_ast_int_node *n = NEW_NODE(int, NODE_INT);
  n->value = val;

  return (node*)n;
}

static node*
new_imaginary(parser_state *p, node *imaginary)
{
  return new_fcall(p, MRB_SYM_2(p->mrb, Complex),
                   new_callargs(p, list2(new_int_n(p, 0), imaginary), 0, 0));
}

static node*
new_rational(parser_state *p, node *rational)
{
  return new_fcall(p, MRB_SYM_2(p->mrb, Rational), new_callargs(p, list1(rational), 0, 0));
}

/* Read integer into int32_t with overflow detection */
static mrb_bool
read_int32(const char *p, int base, int32_t *result)
{
  const char *e = p + strlen(p);
  int32_t value = 0;
  mrb_bool neg = FALSE;

  if (base < 2 || base > 16) {
    return FALSE;
  }

  if (*p == '+') {
    p++;
  }
  else if (*p == '-') {
    neg = TRUE;
    p++;
  }

  while (p < e) {
    int n;
    char c = *p;

    /* Skip underscores */
    if (c == '_') {
      p++;
      continue;
    }

    /* Parse digit */
    if (c >= '0' && c <= '9') {
      n = c - '0';
    }
    else if (c >= 'a' && c <= 'f') {
      n = c - 'a' + 10;
    }
    else if (c >= 'A' && c <= 'F') {
      n = c - 'A' + 10;
    }
    else {
      /* Invalid character */
      return FALSE;
    }

    if (n >= base) {
      /* Digit not valid for this base */
      return FALSE;
    }

    /* Check for multiplication overflow */
    if (value > INT32_MAX / base) {
      return FALSE;
    }

    value *= base;

    /* Check for addition overflow */
    if (value > INT32_MAX - n) {
      /* Special case: -INT32_MIN is valid */
      if (neg && value == (INT32_MAX - n + 1) && p + 1 == e) {
        *result = INT32_MIN;
        return TRUE;
      }
      return FALSE;
    }

    value += n;
    p++;
  }

  *result = neg ? -value : value;
  return TRUE;
}

static node*
new_int(parser_state *p, const char *s, int base, int suffix)
{
  int32_t val;
  node* result;

  /* Try to parse as int32_t first */
  if (read_int32(s, base, &val)) {
    result = new_int_n(p, val);
  }
  else {
    /* Big integer - create NODE_BIGINT */
    struct mrb_ast_bigint_node *n = NEW_NODE(bigint, NODE_BIGINT);
    n->string = strdup(s);
    n->base = base;

    result = (node*)n;
  }

  /* Handle suffix modifiers */
  if (suffix & NUM_SUFFIX_R) {
    result = new_rational(p, result);
  }
  if (suffix & NUM_SUFFIX_I) {
    result = new_imaginary(p, result);
  }

  return result;
}

#ifndef MRB_NO_FLOAT
/* struct: float_node(value) */
static node*
new_float(parser_state *p, const char *s, int suffix)
{
  struct mrb_ast_float_node *n = NEW_NODE(float, NODE_FLOAT);
  n->value = strdup(s);

  node* result = (node*)n;

  if (suffix & NUM_SUFFIX_R) {
    result = new_rational(p, result);
  }
  if (suffix & NUM_SUFFIX_I) {
    result = new_imaginary(p, result);
  }
  return result;
}
#endif

/* Create string node from cons list */
/* struct: str_node(str) */
static node*
new_str(parser_state *p, node *a)
{
  struct mrb_ast_str_node *n = NEW_NODE(str, NODE_STR);
  n->list = a;

  return (node*)n;
}

/* struct: xstr_node(str) */
static node*
new_xstr(parser_state *p, node *a)
{
  struct mrb_ast_xstr_node *n = NEW_NODE(xstr, NODE_XSTR);
  n->list = a;
  return (node*)n;
}

/* struct: dsym_node(parts) - uses cons list */
static node*
new_dsym(parser_state *p, node *a)
{
  struct mrb_ast_str_node *n = NEW_NODE(str, NODE_DSYM);
  n->list = a;
  return (node*)n;
}

/* struct: regx_node(pattern, flags, encoding) */
static node*
new_regx(parser_state *p, node *list, const char *flags, const char *encoding)
{
  struct mrb_ast_regx_node *n = NEW_NODE(regx, NODE_REGX);
  n->list = list;
  n->flags = flags;
  n->encoding = encoding;
  return (node*)n;
}

/* struct: back_ref_node(n) */
static node*
new_back_ref(parser_state *p, int n)
{
  struct mrb_ast_back_ref_node *backref_node = NEW_NODE(back_ref, NODE_BACK_REF);
  backref_node->type = n;
  return (node*)backref_node;
}

/* struct: nth_ref_node(n) */
static node*
new_nth_ref(parser_state *p, int n)
{
  struct mrb_ast_nth_ref_node *nthref_node = NEW_NODE(nth_ref, NODE_NTH_REF);
  nthref_node->nth = n;
  return (node*)nthref_node;
}

/* struct: heredoc_node(str) */
static node*
new_heredoc(parser_state *p, struct mrb_parser_heredoc_info **infop)
{
  struct mrb_ast_heredoc_node *n = NEW_NODE(heredoc, NODE_HEREDOC);

  /* Initialize embedded heredoc info struct */
  n->info.allow_indent = FALSE;
  n->info.remove_indent = FALSE;
  n->info.line_head = FALSE;
  n->info.indent = 0;
  n->info.indented = NULL;
  n->info.type = str_not_parsing;  // Will be set by heredoc processing
  n->info.term = NULL;  // Will be set by heredoc processing
  n->info.term_len = 0;
  n->info.doc = NULL;

  /* Return pointer to embedded info if requested */
  *infop = &n->info;

  return (node*)n;
}

static void
new_bv(parser_state *p, mrb_sym id)
{
}

static node*
new_literal_delim(parser_state *p)
{
  return cons((node*)0, (node*)0);
}

/* Helper for creating string representation cons (length . string_ptr) */
static node*
new_str_rep(parser_state *p, const char *str, int len)
{
  return cons(int_to_node(len), (node*)strndup(str, len));
}

/* Helper for creating string representation from current token */
static node*
new_str_tok(parser_state *p)
{
  return new_str_rep(p, tok(p), toklen(p));
}

/* Helper for creating empty string representation */
static node*
new_str_empty(parser_state *p)
{
  return new_str_rep(p, "", 0);
}

/* (:words . a) */
static node*
new_words(parser_state *p, node *a)
{
  struct mrb_ast_words_node *words_node = NEW_NODE(words, NODE_WORDS);
  words_node->args = a;
  return (node*)words_node;
}

/* (:symbols . a) */
static node*
new_symbols(parser_state *p, node *a)
{
  struct mrb_ast_symbols_node *symbols_node = NEW_NODE(symbols, NODE_SYMBOLS);
  symbols_node->args = a;
  return (node*)symbols_node;
}

/* xxx ----------------------------- */

/* (:call a op) */
static node*
call_uni_op(parser_state *p, node *recv, const char *m)
{
  void_expr_error(p, recv);
  return new_call(p, recv, intern_cstr(m), 0, '.');
}

/* (:call a op b) */
static node*
call_bin_op(parser_state *p, node *recv, const char *m, node *arg1)
{
  return new_call(p, recv, intern_cstr(m), new_callargs(p, list1(arg1), 0, 0), '.');
}

static void
args_with_block(parser_state *p, node *a, node *b)
{
  if (b) {
    /* Handle callargs structure - direct casting like new_args() */
    struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)a;
    if (callargs->block_arg) {
      yyerror(NULL, p, "both block arg and actual block given");
    }
    callargs->block_arg = b;
  }
}

static void
endless_method_name(parser_state *p, node *defn)
{
  struct mrb_ast_def_node *def = (struct mrb_ast_def_node*)defn;
  mrb_sym sym = def->name;
  mrb_int len;
  const char *name = mrb_sym_name_len(p->mrb, sym, &len);

  if (len > 1 && name[len-1] == '=') {
    for (int i=0; i<len-1; i++) {
      if (!identchar(name[i])) return;
    }
    yyerror(NULL, p, "setter method cannot be defined by endless method definition");
  }
}

static void
call_with_block(parser_state *p, node *a, node *b)
{
  if (!a) return;

  /* Handle direct variable-sized nodes */
  struct mrb_ast_var_header *header = (struct mrb_ast_var_header*)a;

  enum node_type var_type = (enum node_type)header->node_type;
  switch (var_type) {
  case NODE_SUPER:
  case NODE_ZSUPER:
    /* For variable-sized super/zsuper nodes, update the args field directly */
    {
      struct mrb_ast_super_node *super_n = super_node(a);
      if (!super_n->args) {
        super_n->args = new_callargs(p, 0, 0, b);
      }
      else {
        args_with_block(p, super_n->args, b);
      }
    }
    break;
  case NODE_YIELD:
    /* Variable-sized yield nodes should generate an error when given a block */
    yyerror(NULL, p, "block given to yield");
    break;
  case NODE_RETURN:
    /* Variable-sized return nodes - recursively call with args */
    {
      struct mrb_ast_return_node *return_n = return_node(a);
      if (return_n->args != NULL) {
        call_with_block(p, return_n->args, b);
      }
    }
    break;
  case NODE_BREAK:
    /* Variable-sized break nodes - recursively call with value */
    {
      struct mrb_ast_break_node *break_n = (struct mrb_ast_break_node*)a;
      if (break_n->value != NULL) {
        call_with_block(p, break_n->value, b);
      }
    }
    break;
  case NODE_NEXT:
    /* Variable-sized next nodes - recursively call with value */
    {
      struct mrb_ast_next_node *next_n = (struct mrb_ast_next_node*)a;
      if (next_n->value != NULL) {
        call_with_block(p, next_n->value, b);
      }
    }
    break;
  case NODE_CALL:
    /* Variable-sized call nodes - add block to existing args */
    {
      struct mrb_ast_call_node *call = call_node(a);

      if (call->args && callargs_node(call->args)->block_arg) {
        yyerror(NULL, p, "both block arg and actual block given");
        return;
      }

      /* Use existing args and add block */
      if (call->args) {
        /* Modify existing callargs structure to add block */
        args_with_block(p, call->args, b);
      }
      else {
        /* Create new callargs with just the block */
        call->args = new_callargs(p, NULL, NULL, b);
      }
    }
    break;
  default:
    /* For other variable-sized nodes, do nothing */
    break;
  }
}

static node*
new_negate(parser_state *p, node *n)
{
  struct mrb_ast_negate_node *negate_node = NEW_NODE(negate, NODE_NEGATE);
  negate_node->operand = n;
  return (node*)negate_node;
}

static node*
cond(node *n)
{
  return n;
}

static node*
ret_args(parser_state *p, node *n)
{
  /* Handle callargs structure - direct casting like new_args() */
  struct mrb_ast_callargs *callargs = (struct mrb_ast_callargs*)n;
  if (callargs->block_arg) {
    yyerror(NULL, p, "block argument should not be given");
    return NULL;
  }
  if (!callargs->regular_args) return NULL;
  if (!callargs->regular_args->cdr) return callargs->regular_args->car;
  return new_array(p, callargs->regular_args);
}

static void
assignable(parser_state *p, node *lhs)
{
  switch (get_node_type(lhs)) {
  case NODE_LVAR:
    local_add(p, var_node(lhs)->symbol);
    break;
  case NODE_CONST:
    if (p->in_def)
      yyerror(NULL, p, "dynamic constant assignment");
    break;
  default:
    /* Other node types don't need special handling in assignable */
    break;
  }
}

static node*
var_reference(parser_state *p, node *lhs)
{
  /* Check if this is a variable-sized node */
  if (node_type_p(lhs, NODE_LVAR)) {
    mrb_sym sym = var_node(lhs)->symbol;
    if (!local_var_p(p, sym)) {
      node *n = new_fcall(p, sym, 0);
      /* Don't free variable-sized nodes - they're managed by the parser allocator */
      return n;
    }
  }
  return lhs;
}

static node*
label_reference(parser_state *p, mrb_sym sym)
{
  const char *name = mrb_sym_name(p->mrb, sym);

  if (local_var_p(p, sym)) {
    return new_lvar(p, sym);
  }
  else if (ISUPPER(name[0])) {
    return new_const(p, sym);
  }
  else {
    return new_fcall(p, sym, 0);
  }
}

typedef enum mrb_string_type  string_type;

typedef struct parser_lex_strterm {
  int type;
  int level;
  int term;
  int paren;
  struct parser_lex_strterm *prev;
} parser_lex_strterm;

static parser_lex_strterm*
new_strterm(parser_state *p, string_type type, int term, int paren)
{
  parser_lex_strterm *lex = (parser_lex_strterm*)parser_palloc(p, sizeof(parser_lex_strterm));
  lex->type = type;
  lex->level = 0;
  lex->term = term;
  lex->paren = paren;
  lex->prev = p->lex_strterm;
  return lex;
}

static void
end_strterm(parser_state *p)
{
  parser_lex_strterm *term = p->lex_strterm->prev;
  parser_pfree(p->lex_strterm);
  p->lex_strterm = term;
}

static node*
push_strterm(parser_state *p)
{
  node *n = cons((node*)p->lex_strterm, p->parsing_heredoc);
  p->lex_strterm = NULL;
  return n;
}

static void
pop_strterm(parser_state *p, node *n)
{
  p->lex_strterm = (parser_lex_strterm*)n->car;
  p->parsing_heredoc = n->cdr;
  cons_free(n);
}

static parser_heredoc_info *
parsing_heredoc_info(parser_state *p)
{
  node *nd = p->parsing_heredoc;
  if (nd == NULL) return NULL;
  /* mrb_assert(nd->car->car == NODE_HEREDOC); */
  if (get_node_type(nd->car) == NODE_HEREDOC) {
    /* Variable-sized heredoc node - return address of embedded info struct */
    struct mrb_ast_heredoc_node *heredoc = (struct mrb_ast_heredoc_node*)nd->car;
    return &heredoc->info;
  }
  return (parser_heredoc_info*)nd->car->cdr;
}

static void
heredoc_treat_nextline(parser_state *p)
{
  if (p->heredocs_from_nextline == NULL) return;
  if (p->parsing_heredoc && p->lex_strterm) {
    append(p->heredocs_from_nextline, p->parsing_heredoc);
  }
  p->parsing_heredoc = p->heredocs_from_nextline;
  p->lex_strterm = new_strterm(p, parsing_heredoc_info(p)->type, 0, 0);
  p->heredocs_from_nextline = NULL;
}

static void
heredoc_end(parser_state *p)
{
  p->parsing_heredoc = p->parsing_heredoc->cdr;
  if (p->parsing_heredoc == NULL) {
    p->lstate = EXPR_BEG;
    end_strterm(p);
  }
  else {
    /* next heredoc */
    p->lex_strterm->type = parsing_heredoc_info(p)->type;
  }
}
#define is_strterm_type(p,str_func) ((p)->lex_strterm->type & (str_func))

static void
prohibit_literals(parser_state *p, node *n)
{
  if (n == 0) {
    yyerror(NULL, p, "can't define singleton method for ().");
  }
  else {
    enum node_type nt = get_node_type(n);
    switch (nt) {
    case NODE_INT:
    case NODE_STR:
    case NODE_XSTR:
    case NODE_REGX:
    case NODE_FLOAT:
    case NODE_ARRAY:
    case NODE_HEREDOC:
      yyerror(NULL, p, "can't define singleton method for literals");
    default:
      break;
    }
  }
}

/* xxx ----------------------------- */


#line 2051 "mrbgems/mruby-compiler/core/y.tab.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG && !defined(yydebug)
extern int yydebug;
#endif


/* Token kinds.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    YYEMPTY = -2,
    YYEOF = 0,                     /* "end of file"  */
    YYerror = 256,                 /* error  */
    YYUNDEF = 257,                 /* "invalid token"  */
    keyword_class = 258,           /* "'class'"  */
    keyword_module = 259,          /* "'module'"  */
    keyword_def = 260,             /* "'def'"  */
    keyword_begin = 261,           /* "'begin'"  */
    keyword_if = 262,              /* "'if'"  */
    keyword_unless = 263,          /* "'unless'"  */
    keyword_while = 264,           /* "'while'"  */
    keyword_until = 265,           /* "'until'"  */
    keyword_for = 266,             /* "'for'"  */
    keyword_undef = 267,           /* "'undef'"  */
    keyword_rescue = 268,          /* "'rescue'"  */
    keyword_ensure = 269,          /* "'ensure'"  */
    keyword_end = 270,             /* "'end'"  */
    keyword_then = 271,            /* "'then'"  */
    keyword_elsif = 272,           /* "'elsif'"  */
    keyword_else = 273,            /* "'else'"  */
    keyword_case = 274,            /* "'case'"  */
    keyword_when = 275,            /* "'when'"  */
    keyword_break = 276,           /* "'break'"  */
    keyword_next = 277,            /* "'next'"  */
    keyword_redo = 278,            /* "'redo'"  */
    keyword_retry = 279,           /* "'retry'"  */
    keyword_in = 280,              /* "'in'"  */
    keyword_do = 281,              /* "'do'"  */
    keyword_do_cond = 282,         /* "'do' for condition"  */
    keyword_do_block = 283,        /* "'do' for block"  */
    keyword_do_LAMBDA = 284,       /* "'do' for lambda"  */
    keyword_return = 285,          /* "'return'"  */
    keyword_yield = 286,           /* "'yield'"  */
    keyword_super = 287,           /* "'super'"  */
    keyword_self = 288,            /* "'self'"  */
    keyword_nil = 289,             /* "'nil'"  */
    keyword_true = 290,            /* "'true'"  */
    keyword_false = 291,           /* "'false'"  */
    keyword_and = 292,             /* "'and'"  */
    keyword_or = 293,              /* "'or'"  */
    keyword_not = 294,             /* "'not'"  */
    modifier_if = 295,             /* "'if' modifier"  */
    modifier_unless = 296,         /* "'unless' modifier"  */
    modifier_while = 297,          /* "'while' modifier"  */
    modifier_until = 298,          /* "'until' modifier"  */
    modifier_rescue = 299,         /* "'rescue' modifier"  */
    keyword_alias = 300,           /* "'alias'"  */
    keyword_BEGIN = 301,           /* "'BEGIN'"  */
    keyword_END = 302,             /* "'END'"  */
    keyword__LINE__ = 303,         /* "'__LINE__'"  */
    keyword__FILE__ = 304,         /* "'__FILE__'"  */
    keyword__ENCODING__ = 305,     /* "'__ENCODING__'"  */
    tIDENTIFIER = 306,             /* "local variable or method"  */
    tFID = 307,                    /* "method"  */
    tGVAR = 308,                   /* "global variable"  */
    tIVAR = 309,                   /* "instance variable"  */
    tCONSTANT = 310,               /* "constant"  */
    tCVAR = 311,                   /* "class variable"  */
    tLABEL_TAG = 312,              /* "label"  */
    tINTEGER = 313,                /* "integer literal"  */
    tFLOAT = 314,                  /* "float literal"  */
    tCHAR = 315,                   /* "character literal"  */
    tXSTRING = 316,                /* tXSTRING  */
    tREGEXP = 317,                 /* tREGEXP  */
    tSTRING = 318,                 /* tSTRING  */
    tSTRING_PART = 319,            /* tSTRING_PART  */
    tSTRING_MID = 320,             /* tSTRING_MID  */
    tNTH_REF = 321,                /* tNTH_REF  */
    tBACK_REF = 322,               /* tBACK_REF  */
    tREGEXP_END = 323,             /* tREGEXP_END  */
    tNUMPARAM = 324,               /* "numbered parameter"  */
    tUPLUS = 325,                  /* "unary plus"  */
    tUMINUS = 326,                 /* "unary minus"  */
    tCMP = 327,                    /* "<=>"  */
    tEQ = 328,                     /* "=="  */
    tEQQ = 329,                    /* "==="  */
    tNEQ = 330,                    /* "!="  */
    tGEQ = 331,                    /* ">="  */
    tLEQ = 332,                    /* "<="  */
    tANDOP = 333,                  /* "&&"  */
    tOROP = 334,                   /* "||"  */
    tMATCH = 335,                  /* "=~"  */
    tNMATCH = 336,                 /* "!~"  */
    tDOT2 = 337,                   /* ".."  */
    tDOT3 = 338,                   /* "..."  */
    tBDOT2 = 339,                  /* tBDOT2  */
    tBDOT3 = 340,                  /* tBDOT3  */
    tAREF = 341,                   /* tAREF  */
    tASET = 342,                   /* tASET  */
    tLSHFT = 343,                  /* "<<"  */
    tRSHFT = 344,                  /* ">>"  */
    tCOLON2 = 345,                 /* "::"  */
    tCOLON3 = 346,                 /* tCOLON3  */
    tOP_ASGN = 347,                /* tOP_ASGN  */
    tASSOC = 348,                  /* "=>"  */
    tLPAREN = 349,                 /* tLPAREN  */
    tLPAREN_ARG = 350,             /* "("  */
    tRPAREN = 351,                 /* ")"  */
    tLBRACK = 352,                 /* "["  */
    tLBRACE = 353,                 /* tLBRACE  */
    tLBRACE_ARG = 354,             /* "{"  */
    tSTAR = 355,                   /* "*"  */
    tPOW = 356,                    /* tPOW  */
    tDSTAR = 357,                  /* "**"  */
    tAMPER = 358,                  /* "&"  */
    tLAMBDA = 359,                 /* "->"  */
    tANDDOT = 360,                 /* "&."  */
    tSYMBEG = 361,                 /* "symbol"  */
    tSTRING_BEG = 362,             /* "string literal"  */
    tXSTRING_BEG = 363,            /* tXSTRING_BEG  */
    tSTRING_DVAR = 364,            /* tSTRING_DVAR  */
    tREGEXP_BEG = 365,             /* tREGEXP_BEG  */
    tWORDS_BEG = 366,              /* tWORDS_BEG  */
    tSYMBOLS_BEG = 367,            /* tSYMBOLS_BEG  */
    tLAMBEG = 368,                 /* tLAMBEG  */
    tHEREDOC_BEG = 369,            /* "here document"  */
    tHEREDOC_END = 370,            /* tHEREDOC_END  */
    tLITERAL_DELIM = 371,          /* tLITERAL_DELIM  */
    tHD_LITERAL_DELIM = 372,       /* tHD_LITERAL_DELIM  */
    tHD_STRING_PART = 373,         /* tHD_STRING_PART  */
    tHD_STRING_MID = 374,          /* tHD_STRING_MID  */
    tLOWEST = 375,                 /* tLOWEST  */
    tUMINUS_NUM = 376,             /* tUMINUS_NUM  */
    tLAST_TOKEN = 377              /* tLAST_TOKEN  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 1994 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 2230 "mrbgems/mruby-compiler/core/y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif

/* Location type.  */
#if ! defined YYLTYPE && ! defined YYLTYPE_IS_DECLARED
typedef struct YYLTYPE YYLTYPE;
struct YYLTYPE
{
  int first_line;
  int first_column;
  int last_line;
  int last_column;
};
# define YYLTYPE_IS_DECLARED 1
# define YYLTYPE_IS_TRIVIAL 1
#endif




int yyparse (parser_state *p);




/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_keyword_class = 3,              /* "'class'"  */
  YYSYMBOL_keyword_module = 4,             /* "'module'"  */
  YYSYMBOL_keyword_def = 5,                /* "'def'"  */
  YYSYMBOL_keyword_begin = 6,              /* "'begin'"  */
  YYSYMBOL_keyword_if = 7,                 /* "'if'"  */
  YYSYMBOL_keyword_unless = 8,             /* "'unless'"  */
  YYSYMBOL_keyword_while = 9,              /* "'while'"  */
  YYSYMBOL_keyword_until = 10,             /* "'until'"  */
  YYSYMBOL_keyword_for = 11,               /* "'for'"  */
  YYSYMBOL_keyword_undef = 12,             /* "'undef'"  */
  YYSYMBOL_keyword_rescue = 13,            /* "'rescue'"  */
  YYSYMBOL_keyword_ensure = 14,            /* "'ensure'"  */
  YYSYMBOL_keyword_end = 15,               /* "'end'"  */
  YYSYMBOL_keyword_then = 16,              /* "'then'"  */
  YYSYMBOL_keyword_elsif = 17,             /* "'elsif'"  */
  YYSYMBOL_keyword_else = 18,              /* "'else'"  */
  YYSYMBOL_keyword_case = 19,              /* "'case'"  */
  YYSYMBOL_keyword_when = 20,              /* "'when'"  */
  YYSYMBOL_keyword_break = 21,             /* "'break'"  */
  YYSYMBOL_keyword_next = 22,              /* "'next'"  */
  YYSYMBOL_keyword_redo = 23,              /* "'redo'"  */
  YYSYMBOL_keyword_retry = 24,             /* "'retry'"  */
  YYSYMBOL_keyword_in = 25,                /* "'in'"  */
  YYSYMBOL_keyword_do = 26,                /* "'do'"  */
  YYSYMBOL_keyword_do_cond = 27,           /* "'do' for condition"  */
  YYSYMBOL_keyword_do_block = 28,          /* "'do' for block"  */
  YYSYMBOL_keyword_do_LAMBDA = 29,         /* "'do' for lambda"  */
  YYSYMBOL_keyword_return = 30,            /* "'return'"  */
  YYSYMBOL_keyword_yield = 31,             /* "'yield'"  */
  YYSYMBOL_keyword_super = 32,             /* "'super'"  */
  YYSYMBOL_keyword_self = 33,              /* "'self'"  */
  YYSYMBOL_keyword_nil = 34,               /* "'nil'"  */
  YYSYMBOL_keyword_true = 35,              /* "'true'"  */
  YYSYMBOL_keyword_false = 36,             /* "'false'"  */
  YYSYMBOL_keyword_and = 37,               /* "'and'"  */
  YYSYMBOL_keyword_or = 38,                /* "'or'"  */
  YYSYMBOL_keyword_not = 39,               /* "'not'"  */
  YYSYMBOL_modifier_if = 40,               /* "'if' modifier"  */
  YYSYMBOL_modifier_unless = 41,           /* "'unless' modifier"  */
  YYSYMBOL_modifier_while = 42,            /* "'while' modifier"  */
  YYSYMBOL_modifier_until = 43,            /* "'until' modifier"  */
  YYSYMBOL_modifier_rescue = 44,           /* "'rescue' modifier"  */
  YYSYMBOL_keyword_alias = 45,             /* "'alias'"  */
  YYSYMBOL_keyword_BEGIN = 46,             /* "'BEGIN'"  */
  YYSYMBOL_keyword_END = 47,               /* "'END'"  */
  YYSYMBOL_keyword__LINE__ = 48,           /* "'__LINE__'"  */
  YYSYMBOL_keyword__FILE__ = 49,           /* "'__FILE__'"  */
  YYSYMBOL_keyword__ENCODING__ = 50,       /* "'__ENCODING__'"  */
  YYSYMBOL_tIDENTIFIER = 51,               /* "local variable or method"  */
  YYSYMBOL_tFID = 52,                      /* "method"  */
  YYSYMBOL_tGVAR = 53,                     /* "global variable"  */
  YYSYMBOL_tIVAR = 54,                     /* "instance variable"  */
  YYSYMBOL_tCONSTANT = 55,                 /* "constant"  */
  YYSYMBOL_tCVAR = 56,                     /* "class variable"  */
  YYSYMBOL_tLABEL_TAG = 57,                /* "label"  */
  YYSYMBOL_tINTEGER = 58,                  /* "integer literal"  */
  YYSYMBOL_tFLOAT = 59,                    /* "float literal"  */
  YYSYMBOL_tCHAR = 60,                     /* "character literal"  */
  YYSYMBOL_tXSTRING = 61,                  /* tXSTRING  */
  YYSYMBOL_tREGEXP = 62,                   /* tREGEXP  */
  YYSYMBOL_tSTRING = 63,                   /* tSTRING  */
  YYSYMBOL_tSTRING_PART = 64,              /* tSTRING_PART  */
  YYSYMBOL_tSTRING_MID = 65,               /* tSTRING_MID  */
  YYSYMBOL_tNTH_REF = 66,                  /* tNTH_REF  */
  YYSYMBOL_tBACK_REF = 67,                 /* tBACK_REF  */
  YYSYMBOL_tREGEXP_END = 68,               /* tREGEXP_END  */
  YYSYMBOL_tNUMPARAM = 69,                 /* "numbered parameter"  */
  YYSYMBOL_tUPLUS = 70,                    /* "unary plus"  */
  YYSYMBOL_tUMINUS = 71,                   /* "unary minus"  */
  YYSYMBOL_tCMP = 72,                      /* "<=>"  */
  YYSYMBOL_tEQ = 73,                       /* "=="  */
  YYSYMBOL_tEQQ = 74,                      /* "==="  */
  YYSYMBOL_tNEQ = 75,                      /* "!="  */
  YYSYMBOL_tGEQ = 76,                      /* ">="  */
  YYSYMBOL_tLEQ = 77,                      /* "<="  */
  YYSYMBOL_tANDOP = 78,                    /* "&&"  */
  YYSYMBOL_tOROP = 79,                     /* "||"  */
  YYSYMBOL_tMATCH = 80,                    /* "=~"  */
  YYSYMBOL_tNMATCH = 81,                   /* "!~"  */
  YYSYMBOL_tDOT2 = 82,                     /* ".."  */
  YYSYMBOL_tDOT3 = 83,                     /* "..."  */
  YYSYMBOL_tBDOT2 = 84,                    /* tBDOT2  */
  YYSYMBOL_tBDOT3 = 85,                    /* tBDOT3  */
  YYSYMBOL_tAREF = 86,                     /* tAREF  */
  YYSYMBOL_tASET = 87,                     /* tASET  */
  YYSYMBOL_tLSHFT = 88,                    /* "<<"  */
  YYSYMBOL_tRSHFT = 89,                    /* ">>"  */
  YYSYMBOL_tCOLON2 = 90,                   /* "::"  */
  YYSYMBOL_tCOLON3 = 91,                   /* tCOLON3  */
  YYSYMBOL_tOP_ASGN = 92,                  /* tOP_ASGN  */
  YYSYMBOL_tASSOC = 93,                    /* "=>"  */
  YYSYMBOL_tLPAREN = 94,                   /* tLPAREN  */
  YYSYMBOL_tLPAREN_ARG = 95,               /* "("  */
  YYSYMBOL_tRPAREN = 96,                   /* ")"  */
  YYSYMBOL_tLBRACK = 97,                   /* "["  */
  YYSYMBOL_tLBRACE = 98,                   /* tLBRACE  */
  YYSYMBOL_tLBRACE_ARG = 99,               /* "{"  */
  YYSYMBOL_tSTAR = 100,                    /* "*"  */
  YYSYMBOL_tPOW = 101,                     /* tPOW  */
  YYSYMBOL_tDSTAR = 102,                   /* "**"  */
  YYSYMBOL_tAMPER = 103,                   /* "&"  */
  YYSYMBOL_tLAMBDA = 104,                  /* "->"  */
  YYSYMBOL_tANDDOT = 105,                  /* "&."  */
  YYSYMBOL_tSYMBEG = 106,                  /* "symbol"  */
  YYSYMBOL_tSTRING_BEG = 107,              /* "string literal"  */
  YYSYMBOL_tXSTRING_BEG = 108,             /* tXSTRING_BEG  */
  YYSYMBOL_tSTRING_DVAR = 109,             /* tSTRING_DVAR  */
  YYSYMBOL_tREGEXP_BEG = 110,              /* tREGEXP_BEG  */
  YYSYMBOL_tWORDS_BEG = 111,               /* tWORDS_BEG  */
  YYSYMBOL_tSYMBOLS_BEG = 112,             /* tSYMBOLS_BEG  */
  YYSYMBOL_tLAMBEG = 113,                  /* tLAMBEG  */
  YYSYMBOL_tHEREDOC_BEG = 114,             /* "here document"  */
  YYSYMBOL_tHEREDOC_END = 115,             /* tHEREDOC_END  */
  YYSYMBOL_tLITERAL_DELIM = 116,           /* tLITERAL_DELIM  */
  YYSYMBOL_tHD_LITERAL_DELIM = 117,        /* tHD_LITERAL_DELIM  */
  YYSYMBOL_tHD_STRING_PART = 118,          /* tHD_STRING_PART  */
  YYSYMBOL_tHD_STRING_MID = 119,           /* tHD_STRING_MID  */
  YYSYMBOL_tLOWEST = 120,                  /* tLOWEST  */
  YYSYMBOL_121_ = 121,                     /* '='  */
  YYSYMBOL_122_ = 122,                     /* '?'  */
  YYSYMBOL_123_ = 123,                     /* ':'  */
  YYSYMBOL_124_ = 124,                     /* '>'  */
  YYSYMBOL_125_ = 125,                     /* '<'  */
  YYSYMBOL_126_ = 126,                     /* '|'  */
  YYSYMBOL_127_ = 127,                     /* '^'  */
  YYSYMBOL_128_ = 128,                     /* '&'  */
  YYSYMBOL_129_ = 129,                     /* '+'  */
  YYSYMBOL_130_ = 130,                     /* '-'  */
  YYSYMBOL_131_ = 131,                     /* '*'  */
  YYSYMBOL_132_ = 132,                     /* '/'  */
  YYSYMBOL_133_ = 133,                     /* '%'  */
  YYSYMBOL_tUMINUS_NUM = 134,              /* tUMINUS_NUM  */
  YYSYMBOL_135_ = 135,                     /* '!'  */
  YYSYMBOL_136_ = 136,                     /* '~'  */
  YYSYMBOL_tLAST_TOKEN = 137,              /* tLAST_TOKEN  */
  YYSYMBOL_138_ = 138,                     /* '{'  */
  YYSYMBOL_139_ = 139,                     /* '}'  */
  YYSYMBOL_140_ = 140,                     /* '['  */
  YYSYMBOL_141_ = 141,                     /* ']'  */
  YYSYMBOL_142_ = 142,                     /* ','  */
  YYSYMBOL_143_ = 143,                     /* '`'  */
  YYSYMBOL_144_ = 144,                     /* '('  */
  YYSYMBOL_145_ = 145,                     /* ')'  */
  YYSYMBOL_146_ = 146,                     /* ';'  */
  YYSYMBOL_147_ = 147,                     /* '.'  */
  YYSYMBOL_148_n_ = 148,                   /* '\n'  */
  YYSYMBOL_YYACCEPT = 149,                 /* $accept  */
  YYSYMBOL_150_1 = 150,                    /* $@1  */
  YYSYMBOL_program = 151,                  /* program  */
  YYSYMBOL_top_compstmt = 152,             /* top_compstmt  */
  YYSYMBOL_top_stmts = 153,                /* top_stmts  */
  YYSYMBOL_top_stmt = 154,                 /* top_stmt  */
  YYSYMBOL_155_2 = 155,                    /* @2  */
  YYSYMBOL_bodystmt = 156,                 /* bodystmt  */
  YYSYMBOL_compstmt = 157,                 /* compstmt  */
  YYSYMBOL_stmts = 158,                    /* stmts  */
  YYSYMBOL_159_3 = 159,                    /* $@3  */
  YYSYMBOL_stmt = 160,                     /* stmt  */
  YYSYMBOL_command_asgn = 161,             /* command_asgn  */
  YYSYMBOL_command_rhs = 162,              /* command_rhs  */
  YYSYMBOL_expr = 163,                     /* expr  */
  YYSYMBOL_164_4 = 164,                    /* $@4  */
  YYSYMBOL_165_5 = 165,                    /* $@5  */
  YYSYMBOL_defn_head = 166,                /* defn_head  */
  YYSYMBOL_167_6 = 167,                    /* $@6  */
  YYSYMBOL_defs_head = 168,                /* defs_head  */
  YYSYMBOL_expr_value = 169,               /* expr_value  */
  YYSYMBOL_command_call = 170,             /* command_call  */
  YYSYMBOL_block_command = 171,            /* block_command  */
  YYSYMBOL_172_7 = 172,                    /* $@7  */
  YYSYMBOL_cmd_brace_block = 173,          /* cmd_brace_block  */
  YYSYMBOL_command = 174,                  /* command  */
  YYSYMBOL_mlhs = 175,                     /* mlhs  */
  YYSYMBOL_mlhs_inner = 176,               /* mlhs_inner  */
  YYSYMBOL_mlhs_basic = 177,               /* mlhs_basic  */
  YYSYMBOL_mlhs_item = 178,                /* mlhs_item  */
  YYSYMBOL_mlhs_list = 179,                /* mlhs_list  */
  YYSYMBOL_mlhs_post = 180,                /* mlhs_post  */
  YYSYMBOL_mlhs_node = 181,                /* mlhs_node  */
  YYSYMBOL_lhs = 182,                      /* lhs  */
  YYSYMBOL_cname = 183,                    /* cname  */
  YYSYMBOL_cpath = 184,                    /* cpath  */
  YYSYMBOL_fname = 185,                    /* fname  */
  YYSYMBOL_fsym = 186,                     /* fsym  */
  YYSYMBOL_undef_list = 187,               /* undef_list  */
  YYSYMBOL_188_8 = 188,                    /* $@8  */
  YYSYMBOL_op = 189,                       /* op  */
  YYSYMBOL_reswords = 190,                 /* reswords  */
  YYSYMBOL_arg = 191,                      /* arg  */
  YYSYMBOL_aref_args = 192,                /* aref_args  */
  YYSYMBOL_arg_rhs = 193,                  /* arg_rhs  */
  YYSYMBOL_paren_args = 194,               /* paren_args  */
  YYSYMBOL_opt_paren_args = 195,           /* opt_paren_args  */
  YYSYMBOL_opt_call_args = 196,            /* opt_call_args  */
  YYSYMBOL_call_args = 197,                /* call_args  */
  YYSYMBOL_198_9 = 198,                    /* @9  */
  YYSYMBOL_command_args = 199,             /* command_args  */
  YYSYMBOL_block_arg = 200,                /* block_arg  */
  YYSYMBOL_opt_block_arg = 201,            /* opt_block_arg  */
  YYSYMBOL_comma = 202,                    /* comma  */
  YYSYMBOL_args = 203,                     /* args  */
  YYSYMBOL_mrhs = 204,                     /* mrhs  */
  YYSYMBOL_primary = 205,                  /* primary  */
  YYSYMBOL_206_10 = 206,                   /* @10  */
  YYSYMBOL_207_11 = 207,                   /* @11  */
  YYSYMBOL_208_12 = 208,                   /* $@12  */
  YYSYMBOL_209_13 = 209,                   /* $@13  */
  YYSYMBOL_210_14 = 210,                   /* @14  */
  YYSYMBOL_211_15 = 211,                   /* @15  */
  YYSYMBOL_212_16 = 212,                   /* $@16  */
  YYSYMBOL_213_17 = 213,                   /* $@17  */
  YYSYMBOL_214_18 = 214,                   /* $@18  */
  YYSYMBOL_215_19 = 215,                   /* $@19  */
  YYSYMBOL_216_20 = 216,                   /* $@20  */
  YYSYMBOL_217_21 = 217,                   /* $@21  */
  YYSYMBOL_218_22 = 218,                   /* @22  */
  YYSYMBOL_219_23 = 219,                   /* @23  */
  YYSYMBOL_220_24 = 220,                   /* @24  */
  YYSYMBOL_221_25 = 221,                   /* @25  */
  YYSYMBOL_primary_value = 222,            /* primary_value  */
  YYSYMBOL_then = 223,                     /* then  */
  YYSYMBOL_do = 224,                       /* do  */
  YYSYMBOL_if_tail = 225,                  /* if_tail  */
  YYSYMBOL_opt_else = 226,                 /* opt_else  */
  YYSYMBOL_for_var = 227,                  /* for_var  */
  YYSYMBOL_f_margs = 228,                  /* f_margs  */
  YYSYMBOL_229_26 = 229,                   /* $@26  */
  YYSYMBOL_block_args_tail = 230,          /* block_args_tail  */
  YYSYMBOL_opt_block_args_tail = 231,      /* opt_block_args_tail  */
  YYSYMBOL_block_param = 232,              /* block_param  */
  YYSYMBOL_opt_block_param = 233,          /* opt_block_param  */
  YYSYMBOL_234_27 = 234,                   /* $@27  */
  YYSYMBOL_block_param_def = 235,          /* block_param_def  */
  YYSYMBOL_opt_bv_decl = 236,              /* opt_bv_decl  */
  YYSYMBOL_bv_decls = 237,                 /* bv_decls  */
  YYSYMBOL_bvar = 238,                     /* bvar  */
  YYSYMBOL_f_larglist = 239,               /* f_larglist  */
  YYSYMBOL_lambda_body = 240,              /* lambda_body  */
  YYSYMBOL_241_28 = 241,                   /* @28  */
  YYSYMBOL_do_block = 242,                 /* do_block  */
  YYSYMBOL_block_call = 243,               /* block_call  */
  YYSYMBOL_method_call = 244,              /* method_call  */
  YYSYMBOL_245_29 = 245,                   /* @29  */
  YYSYMBOL_brace_block = 246,              /* brace_block  */
  YYSYMBOL_247_30 = 247,                   /* @30  */
  YYSYMBOL_case_body = 248,                /* case_body  */
  YYSYMBOL_cases = 249,                    /* cases  */
  YYSYMBOL_in_clauses = 250,               /* in_clauses  */
  YYSYMBOL_251_31 = 251,                   /* $@31  */
  YYSYMBOL_252_32 = 252,                   /* $@32  */
  YYSYMBOL_253_33 = 253,                   /* $@33  */
  YYSYMBOL_p_expr = 254,                   /* p_expr  */
  YYSYMBOL_p_args_head = 255,              /* p_args_head  */
  YYSYMBOL_p_args_post = 256,              /* p_args_post  */
  YYSYMBOL_p_as = 257,                     /* p_as  */
  YYSYMBOL_p_alt = 258,                    /* p_alt  */
  YYSYMBOL_p_value = 259,                  /* p_value  */
  YYSYMBOL_p_array = 260,                  /* p_array  */
  YYSYMBOL_p_array_body = 261,             /* p_array_body  */
  YYSYMBOL_p_array_elems = 262,            /* p_array_elems  */
  YYSYMBOL_p_rest = 263,                   /* p_rest  */
  YYSYMBOL_p_hash = 264,                   /* p_hash  */
  YYSYMBOL_p_hash_body = 265,              /* p_hash_body  */
  YYSYMBOL_p_hash_elems = 266,             /* p_hash_elems  */
  YYSYMBOL_p_hash_elem = 267,              /* p_hash_elem  */
  YYSYMBOL_p_kwrest = 268,                 /* p_kwrest  */
  YYSYMBOL_p_var = 269,                    /* p_var  */
  YYSYMBOL_opt_rescue = 270,               /* opt_rescue  */
  YYSYMBOL_exc_list = 271,                 /* exc_list  */
  YYSYMBOL_exc_var = 272,                  /* exc_var  */
  YYSYMBOL_opt_ensure = 273,               /* opt_ensure  */
  YYSYMBOL_literal = 274,                  /* literal  */
  YYSYMBOL_string = 275,                   /* string  */
  YYSYMBOL_string_fragment = 276,          /* string_fragment  */
  YYSYMBOL_string_rep = 277,               /* string_rep  */
  YYSYMBOL_string_interp = 278,            /* string_interp  */
  YYSYMBOL_279_34 = 279,                   /* @34  */
  YYSYMBOL_xstring = 280,                  /* xstring  */
  YYSYMBOL_regexp = 281,                   /* regexp  */
  YYSYMBOL_heredoc = 282,                  /* heredoc  */
  YYSYMBOL_heredoc_bodies = 283,           /* heredoc_bodies  */
  YYSYMBOL_heredoc_body = 284,             /* heredoc_body  */
  YYSYMBOL_heredoc_string_rep = 285,       /* heredoc_string_rep  */
  YYSYMBOL_heredoc_string_interp = 286,    /* heredoc_string_interp  */
  YYSYMBOL_287_35 = 287,                   /* @35  */
  YYSYMBOL_words = 288,                    /* words  */
  YYSYMBOL_symbol = 289,                   /* symbol  */
  YYSYMBOL_basic_symbol = 290,             /* basic_symbol  */
  YYSYMBOL_sym = 291,                      /* sym  */
  YYSYMBOL_symbols = 292,                  /* symbols  */
  YYSYMBOL_numeric = 293,                  /* numeric  */
  YYSYMBOL_variable = 294,                 /* variable  */
  YYSYMBOL_var_lhs = 295,                  /* var_lhs  */
  YYSYMBOL_var_ref = 296,                  /* var_ref  */
  YYSYMBOL_backref = 297,                  /* backref  */
  YYSYMBOL_superclass = 298,               /* superclass  */
  YYSYMBOL_299_36 = 299,                   /* $@36  */
  YYSYMBOL_f_opt_arglist_paren = 300,      /* f_opt_arglist_paren  */
  YYSYMBOL_f_arglist_paren = 301,          /* f_arglist_paren  */
  YYSYMBOL_f_arglist = 302,                /* f_arglist  */
  YYSYMBOL_f_label = 303,                  /* f_label  */
  YYSYMBOL_f_kw = 304,                     /* f_kw  */
  YYSYMBOL_f_block_kw = 305,               /* f_block_kw  */
  YYSYMBOL_f_block_kwarg = 306,            /* f_block_kwarg  */
  YYSYMBOL_f_kwarg = 307,                  /* f_kwarg  */
  YYSYMBOL_kwrest_mark = 308,              /* kwrest_mark  */
  YYSYMBOL_f_kwrest = 309,                 /* f_kwrest  */
  YYSYMBOL_args_tail = 310,                /* args_tail  */
  YYSYMBOL_opt_args_tail = 311,            /* opt_args_tail  */
  YYSYMBOL_f_args = 312,                   /* f_args  */
  YYSYMBOL_f_bad_arg = 313,                /* f_bad_arg  */
  YYSYMBOL_f_norm_arg = 314,               /* f_norm_arg  */
  YYSYMBOL_f_arg_item = 315,               /* f_arg_item  */
  YYSYMBOL_316_37 = 316,                   /* @37  */
  YYSYMBOL_f_arg = 317,                    /* f_arg  */
  YYSYMBOL_f_opt_asgn = 318,               /* f_opt_asgn  */
  YYSYMBOL_f_opt = 319,                    /* f_opt  */
  YYSYMBOL_f_block_opt = 320,              /* f_block_opt  */
  YYSYMBOL_f_block_optarg = 321,           /* f_block_optarg  */
  YYSYMBOL_f_optarg = 322,                 /* f_optarg  */
  YYSYMBOL_restarg_mark = 323,             /* restarg_mark  */
  YYSYMBOL_f_rest_arg = 324,               /* f_rest_arg  */
  YYSYMBOL_blkarg_mark = 325,              /* blkarg_mark  */
  YYSYMBOL_f_block_arg = 326,              /* f_block_arg  */
  YYSYMBOL_opt_f_block_arg = 327,          /* opt_f_block_arg  */
  YYSYMBOL_singleton = 328,                /* singleton  */
  YYSYMBOL_329_38 = 329,                   /* $@38  */
  YYSYMBOL_assoc_list = 330,               /* assoc_list  */
  YYSYMBOL_assocs = 331,                   /* assocs  */
  YYSYMBOL_assoc = 332,                    /* assoc  */
  YYSYMBOL_operation = 333,                /* operation  */
  YYSYMBOL_operation2 = 334,               /* operation2  */
  YYSYMBOL_operation3 = 335,               /* operation3  */
  YYSYMBOL_dot_or_colon = 336,             /* dot_or_colon  */
  YYSYMBOL_call_op = 337,                  /* call_op  */
  YYSYMBOL_call_op2 = 338,                 /* call_op2  */
  YYSYMBOL_opt_terms = 339,                /* opt_terms  */
  YYSYMBOL_opt_nl = 340,                   /* opt_nl  */
  YYSYMBOL_rparen = 341,                   /* rparen  */
  YYSYMBOL_trailer = 342,                  /* trailer  */
  YYSYMBOL_term = 343,                     /* term  */
  YYSYMBOL_nl = 344,                       /* nl  */
  YYSYMBOL_terms = 345,                    /* terms  */
  YYSYMBOL_none = 346                      /* none  */
};
typedef enum yysymbol_kind_t yysymbol_kind_t;




#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

/* Work around bug in HP-UX 11.23, which defines these macros
   incorrectly for preprocessor constants.  This workaround can likely
   be removed in 2023, as HPE has promised support for HP-UX 11.23
   (aka HP-UX 11i v2) only through the end of 2022; see Table 2 of
   <https://h20195.www2.hpe.com/V2/getpdf.aspx/4AA4-7673ENW.pdf>.  */
#ifdef __hpux
# undef UINT_LEAST8_MAX
# undef UINT_LEAST16_MAX
# define UINT_LEAST8_MAX 255
# define UINT_LEAST16_MAX 65535
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))


/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif


#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YY_USE(E) ((void) (E))
#else
# define YY_USE(E) /* empty */
#endif

/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
#if defined __GNUC__ && ! defined __ICC && 406 <= __GNUC__ * 100 + __GNUC_MINOR__
# if __GNUC__ * 100 + __GNUC_MINOR__ < 407
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")
# else
#  define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                           \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# endif
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

#if 1

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* 1 */

#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL \
             && defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
  YYLTYPE yyls_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE) \
             + YYSIZEOF (YYLTYPE)) \
      + 2 * YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  106
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   17015

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  198
/* YYNRULES -- Number of rules.  */
#define YYNRULES  688
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1219

/* YYMAXUTOK -- Last valid token kind.  */
#define YYMAXUTOK   377


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK                     \
   ? YY_CAST (yysymbol_kind_t, yytranslate[YYX])        \
   : YYSYMBOL_YYUNDEF)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     148,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   135,     2,     2,     2,   133,   128,     2,
     144,   145,   131,   129,   142,   130,   147,   132,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   123,   146,
     125,   121,   124,   122,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   140,     2,   141,   127,     2,   143,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   138,   126,   139,   136,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   134,   137
};

#if YYDEBUG
/* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,  2168,  2168,  2168,  2178,  2184,  2188,  2192,  2196,  2202,
    2204,  2203,  2217,  2243,  2249,  2253,  2257,  2261,  2267,  2267,
    2271,  2275,  2279,  2283,  2292,  2301,  2305,  2310,  2311,  2315,
    2319,  2323,  2327,  2330,  2334,  2338,  2342,  2346,  2350,  2355,
    2359,  2368,  2377,  2386,  2395,  2402,  2403,  2407,  2410,  2411,
    2415,  2419,  2423,  2427,  2427,  2433,  2433,  2439,  2442,  2452,
    2451,  2466,  2475,  2476,  2479,  2480,  2487,  2486,  2501,  2505,
    2510,  2514,  2519,  2523,  2528,  2532,  2536,  2540,  2544,  2550,
    2554,  2560,  2561,  2567,  2571,  2575,  2579,  2583,  2587,  2591,
    2595,  2599,  2603,  2609,  2610,  2616,  2620,  2626,  2630,  2636,
    2640,  2644,  2648,  2652,  2656,  2662,  2668,  2675,  2679,  2683,
    2687,  2691,  2695,  2701,  2707,  2712,  2718,  2722,  2725,  2729,
    2733,  2740,  2741,  2742,  2743,  2748,  2755,  2756,  2759,  2763,
    2763,  2769,  2770,  2771,  2772,  2773,  2774,  2775,  2776,  2777,
    2778,  2779,  2780,  2781,  2782,  2783,  2784,  2785,  2786,  2787,
    2788,  2789,  2790,  2791,  2792,  2793,  2794,  2795,  2796,  2797,
    2798,  2801,  2801,  2801,  2802,  2802,  2803,  2803,  2803,  2804,
    2804,  2804,  2804,  2805,  2805,  2805,  2806,  2806,  2806,  2807,
    2807,  2807,  2807,  2808,  2808,  2808,  2808,  2809,  2809,  2809,
    2809,  2810,  2810,  2810,  2810,  2811,  2811,  2811,  2811,  2812,
    2812,  2815,  2819,  2823,  2827,  2831,  2835,  2839,  2844,  2849,
    2854,  2858,  2862,  2866,  2870,  2874,  2878,  2882,  2886,  2890,
    2894,  2898,  2902,  2906,  2910,  2914,  2918,  2922,  2926,  2930,
    2934,  2938,  2942,  2946,  2950,  2954,  2958,  2962,  2966,  2970,
    2974,  2978,  2982,  2986,  2990,  2994,  2998,  3002,  3011,  3020,
    3029,  3038,  3044,  3045,  3049,  3053,  3059,  3063,  3070,  3074,
    3083,  3100,  3101,  3104,  3105,  3106,  3110,  3114,  3120,  3125,
    3129,  3133,  3137,  3143,  3143,  3154,  3158,  3164,  3168,  3174,
    3177,  3182,  3186,  3190,  3195,  3199,  3205,  3210,  3214,  3220,
    3221,  3225,  3229,  3230,  3231,  3232,  3233,  3238,  3237,  3249,
    3253,  3248,  3258,  3258,  3262,  3266,  3270,  3274,  3278,  3282,
    3286,  3290,  3294,  3298,  3302,  3303,  3309,  3316,  3308,  3329,
    3337,  3345,  3345,  3345,  3352,  3352,  3352,  3359,  3365,  3369,
    3378,  3387,  3397,  3399,  3396,  3408,  3406,  3424,  3429,  3422,
    3446,  3444,  3460,  3470,  3481,  3485,  3489,  3493,  3499,  3506,
    3507,  3508,  3511,  3512,  3515,  3516,  3524,  3525,  3531,  3535,
    3538,  3542,  3546,  3550,  3555,  3559,  3563,  3567,  3573,  3572,
    3582,  3586,  3590,  3594,  3600,  3605,  3610,  3614,  3618,  3622,
    3626,  3630,  3634,  3638,  3642,  3646,  3650,  3654,  3658,  3662,
    3666,  3672,  3677,  3684,  3684,  3688,  3693,  3699,  3703,  3709,
    3710,  3713,  3718,  3721,  3725,  3731,  3735,  3742,  3741,  3758,
    3763,  3767,  3772,  3779,  3783,  3787,  3791,  3795,  3799,  3803,
    3807,  3811,  3818,  3817,  3832,  3831,  3847,  3855,  3864,  3869,
    3873,  3873,  3878,  3878,  3883,  3883,  3893,  3894,  3898,  3902,
    3906,  3910,  3914,  3919,  3924,  3932,  3936,  3943,  3947,  3953,
    3954,  3960,  3961,  3967,  3968,  3972,  3976,  3980,  3984,  3988,
    3992,  3996,  4000,  4004,  4005,  4006,  4013,  4017,  4024,  4029,
    4034,  4039,  4044,  4052,  4056,  4063,  4067,  4075,  4079,  4086,
    4090,  4094,  4101,  4105,  4113,  4118,  4123,  4131,  4135,  4140,
    4147,  4153,  4160,  4163,  4167,  4168,  4171,  4175,  4178,  4182,
    4185,  4186,  4187,  4188,  4191,  4192,  4198,  4203,  4208,  4213,
    4219,  4220,  4226,  4232,  4231,  4243,  4247,  4253,  4257,  4263,
    4272,  4283,  4286,  4287,  4290,  4296,  4302,  4303,  4306,  4313,
    4312,  4327,  4331,  4339,  4343,  4355,  4362,  4369,  4370,  4371,
    4372,  4373,  4377,  4383,  4387,  4395,  4396,  4397,  4401,  4407,
    4411,  4415,  4419,  4423,  4429,  4433,  4439,  4443,  4447,  4451,
    4455,  4459,  4463,  4471,  4478,  4484,  4485,  4489,  4493,  4492,
    4509,  4510,  4513,  4519,  4523,  4529,  4530,  4534,  4538,  4544,
    4548,  4554,  4560,  4567,  4573,  4580,  4584,  4590,  4594,  4600,
    4601,  4604,  4608,  4614,  4618,  4622,  4626,  4632,  4637,  4642,
    4646,  4650,  4654,  4658,  4662,  4666,  4670,  4674,  4678,  4682,
    4686,  4690,  4694,  4699,  4705,  4710,  4715,  4720,  4725,  4732,
    4736,  4743,  4748,  4747,  4759,  4763,  4769,  4777,  4785,  4793,
    4797,  4803,  4807,  4813,  4814,  4817,  4822,  4829,  4830,  4833,
    4837,  4843,  4847,  4853,  4859,  4859,  4866,  4867,  4873,  4877,
    4883,  4889,  4894,  4898,  4903,  4908,  4924,  4929,  4935,  4936,
    4937,  4940,  4941,  4942,  4943,  4946,  4947,  4948,  4951,  4952,
    4955,  4959,  4965,  4966,  4972,  4973,  4976,  4977,  4980,  4983,
    4984,  4985,  4988,  4989,  4992,  4997,  5000,  5001,  5005
};
#endif

/** Accessing symbol of state STATE.  */
#define YY_ACCESSING_SYMBOL(State) YY_CAST (yysymbol_kind_t, yystos[State])

#if 1
/* The user-facing name of the symbol whose (internal) number is
   YYSYMBOL.  No bounds checking.  */
static const char *yysymbol_name (yysymbol_kind_t yysymbol) YY_ATTRIBUTE_UNUSED;

/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "\"invalid token\"", "\"'class'\"",
  "\"'module'\"", "\"'def'\"", "\"'begin'\"", "\"'if'\"", "\"'unless'\"",
  "\"'while'\"", "\"'until'\"", "\"'for'\"", "\"'undef'\"", "\"'rescue'\"",
  "\"'ensure'\"", "\"'end'\"", "\"'then'\"", "\"'elsif'\"", "\"'else'\"",
  "\"'case'\"", "\"'when'\"", "\"'break'\"", "\"'next'\"", "\"'redo'\"",
  "\"'retry'\"", "\"'in'\"", "\"'do'\"", "\"'do' for condition\"",
  "\"'do' for block\"", "\"'do' for lambda\"", "\"'return'\"",
  "\"'yield'\"", "\"'super'\"", "\"'self'\"", "\"'nil'\"", "\"'true'\"",
  "\"'false'\"", "\"'and'\"", "\"'or'\"", "\"'not'\"", "\"'if' modifier\"",
  "\"'unless' modifier\"", "\"'while' modifier\"", "\"'until' modifier\"",
  "\"'rescue' modifier\"", "\"'alias'\"", "\"'BEGIN'\"", "\"'END'\"",
  "\"'__LINE__'\"", "\"'__FILE__'\"", "\"'__ENCODING__'\"",
  "\"local variable or method\"", "\"method\"", "\"global variable\"",
  "\"instance variable\"", "\"constant\"", "\"class variable\"",
  "\"label\"", "\"integer literal\"", "\"float literal\"",
  "\"character literal\"", "tXSTRING", "tREGEXP", "tSTRING",
  "tSTRING_PART", "tSTRING_MID", "tNTH_REF", "tBACK_REF", "tREGEXP_END",
  "\"numbered parameter\"", "\"unary plus\"", "\"unary minus\"", "\"<=>\"",
  "\"==\"", "\"===\"", "\"!=\"", "\">=\"", "\"<=\"", "\"&&\"", "\"||\"",
  "\"=~\"", "\"!~\"", "\"..\"", "\"...\"", "tBDOT2", "tBDOT3", "tAREF",
  "tASET", "\"<<\"", "\">>\"", "\"::\"", "tCOLON3", "tOP_ASGN", "\"=>\"",
  "tLPAREN", "\"(\"", "\")\"", "\"[\"", "tLBRACE", "\"{\"", "\"*\"",
  "tPOW", "\"**\"", "\"&\"", "\"->\"", "\"&.\"", "\"symbol\"",
  "\"string literal\"", "tXSTRING_BEG", "tSTRING_DVAR", "tREGEXP_BEG",
  "tWORDS_BEG", "tSYMBOLS_BEG", "tLAMBEG", "\"here document\"",
  "tHEREDOC_END", "tLITERAL_DELIM", "tHD_LITERAL_DELIM", "tHD_STRING_PART",
  "tHD_STRING_MID", "tLOWEST", "'='", "'?'", "':'", "'>'", "'<'", "'|'",
  "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'", "tUMINUS_NUM", "'!'",
  "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "']'", "','", "'`'", "'('",
  "')'", "';'", "'.'", "'\\n'", "$accept", "$@1", "program",
  "top_compstmt", "top_stmts", "top_stmt", "@2", "bodystmt", "compstmt",
  "stmts", "$@3", "stmt", "command_asgn", "command_rhs", "expr", "$@4",
  "$@5", "defn_head", "$@6", "defs_head", "expr_value", "command_call",
  "block_command", "$@7", "cmd_brace_block", "command", "mlhs",
  "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list", "mlhs_post",
  "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym", "undef_list",
  "$@8", "op", "reswords", "arg", "aref_args", "arg_rhs", "paren_args",
  "opt_paren_args", "opt_call_args", "call_args", "@9", "command_args",
  "block_arg", "opt_block_arg", "comma", "args", "mrhs", "primary", "@10",
  "@11", "$@12", "$@13", "@14", "@15", "$@16", "$@17", "$@18", "$@19",
  "$@20", "$@21", "@22", "@23", "@24", "@25", "primary_value", "then",
  "do", "if_tail", "opt_else", "for_var", "f_margs", "$@26",
  "block_args_tail", "opt_block_args_tail", "block_param",
  "opt_block_param", "$@27", "block_param_def", "opt_bv_decl", "bv_decls",
  "bvar", "f_larglist", "lambda_body", "@28", "do_block", "block_call",
  "method_call", "@29", "brace_block", "@30", "case_body", "cases",
  "in_clauses", "$@31", "$@32", "$@33", "p_expr", "p_args_head",
  "p_args_post", "p_as", "p_alt", "p_value", "p_array", "p_array_body",
  "p_array_elems", "p_rest", "p_hash", "p_hash_body", "p_hash_elems",
  "p_hash_elem", "p_kwrest", "p_var", "opt_rescue", "exc_list", "exc_var",
  "opt_ensure", "literal", "string", "string_fragment", "string_rep",
  "string_interp", "@34", "xstring", "regexp", "heredoc", "heredoc_bodies",
  "heredoc_body", "heredoc_string_rep", "heredoc_string_interp", "@35",
  "words", "symbol", "basic_symbol", "sym", "symbols", "numeric",
  "variable", "var_lhs", "var_ref", "backref", "superclass", "$@36",
  "f_opt_arglist_paren", "f_arglist_paren", "f_arglist", "f_label", "f_kw",
  "f_block_kw", "f_block_kwarg", "f_kwarg", "kwrest_mark", "f_kwrest",
  "args_tail", "opt_args_tail", "f_args", "f_bad_arg", "f_norm_arg",
  "f_arg_item", "@37", "f_arg", "f_opt_asgn", "f_opt", "f_block_opt",
  "f_block_optarg", "f_optarg", "restarg_mark", "f_rest_arg",
  "blkarg_mark", "f_block_arg", "opt_f_block_arg", "singleton", "$@38",
  "assoc_list", "assocs", "assoc", "operation", "operation2", "operation3",
  "dot_or_colon", "call_op", "call_op2", "opt_terms", "opt_nl", "rparen",
  "trailer", "term", "nl", "terms", "none", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-969)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-689)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -969,  4457,   111, 11373,  3656, 14067,  9423,  -969, 13143, 13143,
    -969,  -969, 13725, 10605,  9039, 11609, 11609,  -969,  -969, 11609,
    5386,  4570,  -969,  -969,  -969,  -969,   -10, 10605,  -969,   -17,
    -969,  -969,  -969,  9565,  4100,  -969,  -969,  9707,  -969,  -969,
    -969,  -969,  -969,  -969,  -969,   287, 13261, 13261, 13261, 13261,
      90,  7860,  1522, 12081, 12435, 10887,  -969, 10323,   852,   427,
    1214,  1010,  1081,  -969,   268, 13379, 13261,  -969,   934,  -969,
    1019,  -969,   477,  1722,  1722,  -969,  -969,   139,    59,  -969,
      67, 13839,  -969,   133,  6746,   540,   571,    83,    80,  -969,
     426,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
     299,   119,  -969,   454,    61,  -969,  -969,  -969,  -969,  -969,
    -969,   129,   129,   -10,   497,   760,  -969, 13143,   367,  7979,
     498,  1928,  1928,  -969,   150,  -969,   636,  -969,  -969,    61,
    -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
    -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
    -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,    72,    79,
     120,   181,  -969,  -969,  -969,  -969,  -969,  -969,   182,   211,
     229,   250,  -969,   253,  -969,  -969,  -969,  -969,  -969,  -969,
    -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
    -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
    -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,   297,
    6892,   232,   477,  1722,  1722,   185,   172,   748,   343,   214,
     377,   185, 13143, 13143,   749,   259,  -969,  -969,   789,   296,
      49,    92,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
    -969, 10464,  -969,  -969,   196,  -969,  -969,  -969,  -969,  -969,
    -969,   934,  -969,   387,  -969,   325,  -969,  -969,   934,  4706,
      51, 13261, 13261, 13261, 13261,  -969, 16802,  -969,  -969,   263,
     317,   263,  -969,  -969,  -969, 11727,  -969,  -969, 11609,  -969,
    -969,  -969,  -969,  9039,  9277,  -969,   258,  8098,  -969,   858,
     310,  3142,  3142,   467, 11491,  7860,   305,   934,  1019,   934,
     341,  -969, 11491,   934,   330,  1700,  1700,  -969, 16802,   342,
    1700,  -969,   435, 14181,   354,   870,   950,   952,  1378,  -969,
    -969,  -969,  -969,  -969,  1391,  -969,  -969,  -969,  -969,  -969,
    -969,   630,  1394,  -969,  -969,   838,  -969,  1353,  -969,  1403,
    -969,  1424,   407,   415,  -969,  -969,  -969,  -969,  8363, 13143,
   13143, 13143, 13143, 11491, 13143, 13143,    63,  -969,  -969,  -969,
    -969,   461,   934,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
    2248,   401,   405,  6892, 13261,  -969,   396,   478,   419,  -969,
     934,  -969,  -969,  -969,   422, 13261,  -969,   428,   528,   431,
     569,  -969,  -969,   421,  6892,  -969,  -969, 12553,  -969,  7860,
   11001,   484, 12553,  -969, 13261, 13261, 13261, 13261, 13261, 13261,
   13261, 13261, 13261, 13261, 13261, 13261, 13261, 13261,  -969, 13261,
   13261, 13261, 13261, 13261, 13261, 13261, 13261, 13261, 13261, 13261,
   13261, 14709,  -969, 11609,  -969, 14795,  -969,  -969,  4306,  -969,
    -969,  -969,  -969, 13379, 13379,  -969,   537,  -969,   477,  -969,
     989,  -969,  -969,  -969,  -969,  -969,  -969, 14881, 11609, 14967,
    6892, 13143,  -969,  -969,  -969,   635,   634,   474,   532,   536,
    -969,  7038,   651, 13261, 15053, 11609, 15139, 13261, 13261,  7476,
     697,   697,   113, 15225, 11609, 15311,  -969,   608,  -969,  8098,
     222,  -969,  -969, 12671,   660,  -969, 13261, 13261, 16864, 16864,
   16864, 13261,  -969,  -969, 11845,  -969, 13261,  -969, 12199,  9158,
     545,   934,   263,   263,  -969,  -969,  1138,   552,  -969,  -969,
    -969, 10605,  7595,   563, 15053, 15139, 13261,  1019,   934,  -969,
    -969,  8482,   557,  1019,  -969,  -969, 12317,  -969,   934, 12435,
    -969,  -969,  -969,   989,    67, 14181,  -969, 14181, 15397, 11609,
   15483,  2085,  -969,  -969,   566,  -969,  1452,  8098,   630,  -969,
    -969,  -969,  -969,  -969,  -969,  -969, 13261, 13261,  -969,  -969,
    -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
    1502,   934,   934,   572, 13379,   689, 16864,  1224,  -969,  -969,
    -969,     1,  -969,  -969,  2331,  -969, 16864,  2085,  -969,  -969,
    1529,  -969,  -969, 13379,   701,    91, 13261,  -969,  3470,   263,
    -969,   934, 14181,   597,  -969,  -969,  -969,   681,   623,  1800,
    -969,  -969,  1103,   480, 13497,  3060,  3060,  3060,  3060,  1623,
    1623, 16882,  3952,  3060,  3060,  3142,  3142,   857,   857, 13497,
     310, 16864,  1623,  1623,  1732,  1732,  1807,   204,   204,   310,
     310,   310,  5522, 10063,  6474, 10181,  -969,   129,  -969,   611,
     263,   521,  -969,   522,  -969,  -969,  4842,  -969,  -969,  2804,
      91,    91,  -969,  2547,  -969,  -969,  -969,  -969,  -969,   934,
   13143,  6892,   856,   311,  -969,   129,   612,   129,   740,  1138,
   10746,  -969, 12789,   747,  -969, 13261, 13261,   588,  -969,  9825,
    9944,   628,   491,   518,   747,  -969,  -969,  -969,  -969,    37,
      87,   631,   122,   141, 13143, 10605,   640, 13497,   756, 16864,
     117,  -969, 16864, 16864, 16864,   382, 13261, 16802,  -969,   263,
   16864,  -969,  -969,  -969,  -969, 11963, 12199,  -969,  -969,  -969,
     643,  -969,  -969,   101,  1019,   934,  1700,   484,  -969,   856,
     311,   644,   966,  1012,  -969,   153,  2085,  -969,   653,  -969,
     310,   310,  -969,  -969,  1329,   934,   656,  -969,  -969,  2352,
     751, 16158,  -969,   742,   461,  -969,   419,  -969,   934,  -969,
    -969,   659,   663,   666,  -969,   675,   742,   666,   774, 16220,
    -969,  -969,  2085,  6892,  -969,  -969, 16589, 12907,  -969,  -969,
   14181, 11491, 13379, 13261, 15569, 11609, 15655,  5794,  5930,  6066,
     717,  4978,  5250,   764, 11115, 11254,   770,   339,   772,  1123,
    -969, 13611,   684,    39,  -969,  -969,   685,  -969,   688,  -969,
    -969,  -969,  2251,  6202,  -969,   710, 13379, 13379,  -969,   537,
     589, 11845, 13379, 13379,  -969,   537,    80,   139,  6892,  8098,
      91,  -969,   934,   817,  -969,  -969,  -969,  -969,  3470,  -969,
     741,  -969,  7741,   828,  -969, 13143,   829,  -969, 13261, 13261,
     535, 13261, 13261,   831,  8244,  8244,   143,   697,  -969,  -969,
     632,  -969, 13025,  7184, 16864,  -969,  9158,   263,  -969,  -969,
    -969,  1162,   702,  1528,  6892,  8098,  -969,  -969,  -969,   706,
    -969,  1610,   934, 13261, 13261,  -969,  -969,  2085,  -969,  1529,
    -969,  1529,  -969,  1529,  -969,  -969, 13261, 13261,  -969,  -969,
    -969, 14295,  -969,   709,   419,   718, 14295,  -969,   724,   726,
    -969,   841, 13261, 16660,  -969,  -969, 16864,  5658,  6610,   711,
     602,   609, 13953,   989, 14457,   816,  8747,  -969,  1231,  -969,
     732,   743,   745,  6338,   254,  8893,  -969,   735,   766,  -969,
   16513,  -969,  -969,  -969,  -969, 15741,  5114,   767,   769,  -969,
     832, 13953, 13953,   366, 13953, 13261, 13261,  -969,  -969,  -969,
    -969,  -969, 13379,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
     891,   781,  8098,  6892,  -969,  -969, 14409,   185,  -969,  -969,
    8244,  -969,  -969,   185,  -969, 13261,  -969,   906,   915,  -969,
   13143, 13143,  7330, 16864,   178,  -969, 12199,  -969,  1629,   916,
     794,  1482,  1482,  1340,  -969, 16864, 16864,   666,   792,   666,
     666, 16864, 16864,   809,   811,   884,  1252,  1224,  -969,  -969,
    1995,  -969,  1252,  2085,  -969,  1529,  -969,  -969, 16731,   621,
    -969, 16359, 14534, 15827,  -969, 13611, 13953,  8601, 16436, 14611,
    -969,   366,   796,   311,  -969, 13953,  -969,  -969,   801,  -969,
     888,  -969,  -969,   854,  -969, 16864, 16864,  -969,  -969,  -969,
    -969,   818,   949,   911,  -969,  1264,   950,   952,  6892,  -969,
    7038,  -969,  -969,  8244,   185,   185,   227,  -969,  -969,  -969,
    -969,   834,  -969,  -969,  -969,  -969,   835,   835,  1482,   839,
    -969,  1529,  -969,  -969,  -969,  -969,  -969,  -969, 15913,  -969,
     419,  1224,  -969,  -969,   851,   859,   862,  -969,   874,   862,
   16282,  -969,   880,   885,  -969,   801, 13953,  -969,  -969,   989,
   15999, 11609, 16085,   634,   588,   959,  7330,  7330, 13497,  -969,
     984,  1629,   382,  1482,   835,  1482,   666,   868,  -969,  2085,
    -969,  1529,  -969,  1529,  -969,  1529,  -969,  -969, 13953, 13953,
    -969,   856,   311,   887,   198,   618,  -969,  -969,  -969,   227,
     227,   591,  -969,  -969,   835,  -969,   862,   889,   862,   862,
     885,  1162,  1018,  1021,   185,   994,   998,  -969,  1529,  -969,
    -969,  -969,  -969,  -969,  7330, 13143, 13143,   862,   227,   185,
     185,  -969,  -969,  7330,  7330,   227,   227,  -969,  -969
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     0,     0,     0,     0,   297,     0,     0,
     321,   324,     0,     0,   674,   344,   345,   346,   347,   309,
     273,   273,   559,   558,   560,   561,   676,     0,    10,     0,
     563,   562,   564,   549,   660,   551,   550,   553,   552,   545,
     546,   506,   507,   565,   566,   557,     0,     0,     0,     0,
       0,     0,   299,   688,   688,    91,   316,     0,     0,     0,
       0,     0,     0,   521,     0,     0,     0,     3,   674,     6,
       9,    27,    32,   613,   613,    48,    63,    62,     0,    79,
       0,    83,    93,     0,     0,   251,     0,    64,   314,   289,
     290,   504,   291,   292,   293,   502,   501,   533,   503,   500,
     556,     0,   294,   295,   273,     5,     1,     8,   344,   345,
     309,   688,   420,     0,   116,   117,   557,     0,     0,     0,
       0,   613,   613,   119,   567,   348,     0,   556,   295,     0,
     340,   171,   181,   172,   168,   197,   198,   199,   200,   179,
     194,   187,   177,   176,   192,   175,   174,   170,   195,   169,
     182,   186,   188,   180,   173,   189,   196,   191,   190,   183,
     193,   178,   167,   185,   184,   166,   164,   165,   161,   162,
     163,   121,   123,   122,   156,   157,   134,   135,   136,   143,
     140,   142,   137,   138,   158,   159,   144,   145,   149,   152,
     153,   139,   141,   131,   132,   133,   146,   147,   148,   150,
     151,   154,   155,   160,   644,    58,   124,   125,   643,     0,
       0,     0,    61,   613,   613,     0,     0,     0,   556,     0,
     295,     0,     0,     0,   115,     0,   359,   358,     0,     0,
     556,   295,   190,   183,   193,   178,   161,   162,   163,   121,
     122,     0,   126,   128,    20,   127,   524,   529,   528,   682,
     684,   674,   685,     0,   526,     0,   686,   683,   675,   658,
     557,   281,   657,   276,     0,   268,   280,    77,   272,   688,
     504,   688,   648,    78,    76,   688,   262,   310,     0,    75,
     261,   419,    74,   674,     0,    18,     0,     0,   224,     0,
     225,   212,   215,   306,     0,     0,     0,   674,    15,   674,
      81,    14,     0,   674,     0,   679,   679,   252,     0,     0,
     679,   646,     0,     0,    89,     0,    99,   106,   613,   539,
     538,   540,   541,   535,     0,   537,   536,   508,   513,   512,
     515,     0,     0,   510,   517,     0,   519,     0,   531,     0,
     543,     0,   547,   548,    52,   239,   240,     4,   675,     0,
       0,     0,     0,     0,     0,     0,   620,   616,   615,   614,
     617,   618,     0,   622,   634,   589,   590,   638,   637,   633,
     613,     0,   575,     0,   582,   587,   688,   592,   688,   612,
       0,   619,   621,   624,   598,     0,   631,   598,   636,   598,
     640,   596,   571,     0,     0,   407,   409,     0,    95,     0,
      87,    84,     0,    55,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   211,   214,     0,     0,    53,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   671,   688,   670,     0,   673,   672,     0,   424,
     422,   315,   505,     0,     0,   413,    68,   313,   337,   116,
     117,   118,   547,   548,   575,   568,   335,     0,   688,     0,
       0,     0,   669,   668,    59,     0,   688,   306,     0,     0,
     350,     0,   349,     0,     0,   688,     0,     0,     0,     0,
       0,     0,   306,     0,   688,     0,   332,     0,   129,     0,
       0,   525,   527,     0,     0,   687,   652,   653,   282,   656,
     275,     0,   676,   269,     0,   278,     0,   270,     0,   674,
       0,   674,   688,   688,   263,   274,   674,     0,   312,    51,
     677,     0,     0,     0,     0,     0,     0,    17,   674,   304,
      13,   675,    80,   300,   303,   307,   681,   253,   680,   681,
     255,   308,   647,   105,    97,     0,    92,     0,     0,   688,
       0,   613,   317,   404,   598,   542,     0,     0,   516,   522,
     509,   511,   518,   520,   532,   544,     0,     0,     7,    21,
      22,    23,    24,    25,    49,    50,   579,   626,   580,   578,
       0,   674,   674,   598,     0,     0,   581,     0,   594,   642,
     591,     0,   595,   576,     0,   605,   627,     0,   608,   635,
       0,   610,   639,     0,     0,   688,   281,    28,    30,     0,
      31,   674,     0,    85,    96,    47,    33,    45,     0,   256,
     201,    29,     0,   295,     0,   229,   234,   235,   236,   231,
     233,   243,   244,   237,   238,   210,   213,   241,   242,     0,
     221,   676,   230,   232,   226,   227,   228,   216,   217,   218,
     219,   220,   661,   666,   662,   667,   418,   273,   416,     0,
     688,   661,   663,   662,   664,   417,   273,   661,   662,   273,
     688,   688,    34,   256,   202,    44,   209,    66,    69,     0,
       0,     0,   116,   117,   120,     0,     0,   688,     0,   674,
       0,   298,   688,   688,   492,     0,     0,   688,   351,   665,
     305,     0,   661,   662,   688,   353,   322,   352,   325,   665,
     305,     0,   661,   662,     0,     0,     0,     0,     0,   280,
       0,   328,   651,   654,   650,   279,   284,   283,   277,   688,
     655,   649,   260,   258,   264,   265,   267,   311,   678,    19,
       0,    26,   208,    82,    16,   674,   679,    98,    90,   102,
     104,     0,   101,   103,   676,     0,     0,   534,     0,   523,
     222,   223,   620,   618,   367,   674,   360,   574,   572,     0,
      40,   247,   342,     0,     0,   588,   688,   641,     0,   597,
     625,   598,   598,   598,   632,   598,   620,   598,    42,   249,
     343,   395,   393,     0,   392,   391,   288,     0,    94,    88,
       0,     0,     0,     0,     0,   688,     0,     0,     0,     0,
     490,     0,     0,     0,     0,     0,   476,   489,     0,     0,
      56,     0,   436,   449,   451,   463,   440,   464,   442,   482,
     444,   453,     0,     0,    54,     0,     0,     0,   415,    72,
     421,   265,     0,     0,   414,    70,   410,    65,     0,     0,
     688,   338,     0,     0,   421,   341,   645,    60,   493,   494,
     688,   495,     0,   688,   356,     0,     0,   354,     0,     0,
     421,     0,     0,     0,     0,     0,   421,     0,   130,   530,
       0,   327,     0,     0,   285,   271,   674,   688,    11,   301,
     254,   100,     0,   397,     0,     0,   318,   514,   368,   365,
     623,     0,   674,     0,     0,   593,   577,     0,   601,     0,
     603,     0,   609,     0,   606,   611,     0,     0,   390,   676,
     676,   584,   585,   688,   688,   375,     0,   629,   375,   375,
     373,     0,   284,   286,    86,    46,   257,   661,   662,     0,
     661,   662,   485,   462,     0,     0,     0,   467,     0,   473,
       0,   468,   471,     0,   549,     0,   478,     0,   479,   481,
       0,   475,   488,   487,   465,     0,     0,   437,   438,   445,
       0,     0,     0,     0,     0,     0,     0,    39,   206,    38,
     207,    73,     0,    36,   204,    37,   205,    71,   411,   412,
       0,     0,     0,     0,   569,   336,     0,     0,   497,   357,
       0,    12,   499,     0,   319,     0,   320,     0,     0,   333,
       0,     0,     0,   283,   688,   259,   266,   403,     0,     0,
       0,     0,     0,   363,   573,    41,   248,   598,   598,   598,
     598,    43,   250,     0,     0,     0,   583,     0,   371,   372,
     375,   383,   628,     0,   386,     0,   388,   408,   287,   421,
     484,     0,     0,     0,   466,     0,     0,     0,     0,     0,
     477,     0,   665,   461,   446,     0,   450,   452,   441,   447,
       0,   483,   443,     0,   486,   246,   245,    35,   203,   425,
     423,     0,     0,     0,   496,     0,   107,   114,     0,   498,
       0,   323,   326,     0,     0,     0,   688,   427,   428,   426,
     401,   676,   399,   402,   406,   405,   369,   366,     0,   361,
     602,     0,   599,   604,   607,   396,   394,   306,     0,   586,
     688,     0,   374,   381,   375,   375,   375,   630,   375,   375,
       0,   474,   469,   472,   480,   439,     0,    67,   339,   113,
       0,   688,     0,   688,   688,     0,     0,     0,     0,   429,
       0,     0,   398,     0,   364,     0,   598,   305,   370,     0,
     378,     0,   380,     0,   387,     0,   384,   389,     0,     0,
     448,   110,   112,     0,   661,   662,   491,   355,   334,   688,
     688,   430,   329,   400,   362,   600,   375,   375,   375,   375,
     470,   108,     0,     0,     0,     0,     0,   379,     0,   376,
     382,   385,   330,   331,     0,     0,     0,   375,   688,     0,
       0,   377,   431,     0,     0,   688,   688,   433,   435
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -969,  -969,  -969,   519,  -969,    24,  -969,  -266,   473,  -969,
    -969,    40,  -389,  -397,    -2,  -969,  -969,  1150,  -969,  1713,
      15,   -40,  -969,  -969,  -521,    19,  1032,  -192,    13,   -60,
    -287,  -498,   -18,  2581,   -88,  1040,     3,   -26,  -969,  -969,
      69,  -969,  3468,  -969,   863,   104,  -346,  -419,   103,  -969,
     -11,  -447,  -263,  -129,    17,  -362,    93,  -969,  -969,  -969,
    -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,  -969,
    -969,  -969,  -969,    76,  -203,  -470,   -93,  -558,  -969,  -969,
    -969,   262,   400,  -969,  -604,  -969,  -969,  -199,  -969,   -66,
    -969,  -969,  -969,   244,  -969,  -969,  -969,   -83,  -969,  -474,
    -969,   393,  -969,  -969,  -969,  -581,  -969,    28,   135,  -969,
     125,  -969,  -969,  -878,  -743,  -969,  -969,   285,  -890,  -746,
    -969,   -38,  -969,  -969,  -969,  -969,  -969,   562,    99,  -135,
    -969,  -969,  -969,  -969,  -969,  -280,  -969,   850,  -969,  -969,
     837,     4,  -969,  -969,   560,  2439,  2955,  1102,  2171,  -969,
    -969,    26,   613,    27,   251,   527,    84,  -969,  -969,  -969,
     190,   714,  -345,  -195,  -968,  -716,  -579,  -969,   583,  -764,
    -552,  -967,    82,  -500,  -969,  -508,  -969,    75,  -374,  -969,
    -969,  -969,   170,  -440,   964,  -322,  -969,  -969,   -64,  -969,
      88,   -23,  -148,  -247,   -71,  -282,   -48,    -1
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,     2,    67,    68,    69,   286,   465,   466,   297,
     521,   298,    71,   616,    72,   639,   624,   213,   690,   214,
     215,    75,    76,   850,   678,    77,    78,   299,    79,    80,
      81,   546,    82,   216,   123,   124,   242,   243,   244,   715,
     655,   207,    84,   304,   620,   656,   277,   510,   511,   278,
     279,   268,   503,   539,   660,   610,    85,   210,   302,   745,
     303,   318,   755,   222,   874,   223,   875,   714,  1093,   681,
     679,   993,   460,   289,   471,   706,   866,  1149,   229,   765,
    1021,  1122,  1041,   919,   793,   920,   794,   892,  1101,  1102,
     552,   896,   605,   396,    87,    88,   671,   447,   670,   494,
    1099,  1150,  1194,  1195,  1196,   820,   821,  1068,   822,   823,
     824,   825,   950,   951,   826,   827,   957,   828,   829,   830,
     831,   693,   860,   997,  1001,    89,    90,    91,   332,   333,
     557,    92,    93,    94,   558,   252,   253,   254,   489,    95,
      96,    97,   326,    98,    99,   218,   219,   102,   220,   456,
     680,   371,   372,   373,   374,   375,   922,   923,   376,   377,
     378,   779,   595,   380,   381,   382,   383,   580,   384,   385,
     386,   927,   928,   387,   388,   389,   390,   391,   588,   209,
     461,   309,   513,   272,   129,   685,   658,   464,   459,   438,
     517,   893,   518,   537,   256,   257,   258,   301
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     105,   285,   520,   284,   592,   441,   212,   212,   507,   205,
     282,   708,   212,   615,   659,   780,   718,   245,   479,   280,
     348,   401,   435,   437,   221,   344,   545,   107,   926,   251,
     451,   245,   269,   269,   265,   265,   269,   314,   265,   686,
     621,    70,   598,    70,   601,   784,   672,   675,   899,   748,
    1103,   559,   307,   311,   615,   615,   701,   728,   834,   540,
     325,   728,  -110,   542,   300,   711,   848,   849,   731,   959,
     305,   952,   392,   392,  -107,   206,  1127,    86,   968,    86,
     126,   126,   206,  1071,   217,   217,   782,   439,   228,   785,
     217,   217,   217,   446,   781,   217,   206,   125,   125,   731,
     393,   394,   255,   528,   367,   125,   439,   585,   497,   657,
     280,   106,  -112,   666,   799,   448,   669,  -114,   267,   273,
     576,   287,   274,   553,   276,   281,   206,    86,   604,   368,
     751,   315,   970,   470,   283,   863,   880,   687,  -113,   867,
     504,   217,   508,  -555,   472,   293,   873,  -109,   125,   394,
     472,   532,   657,   476,   666,   534,   347,   315,   335,   337,
     339,   341,  -559,   687,   485,   971,  -111,   395,  -108,  -558,
     791,  1071,  -115,   436,   125,   582,   536,   780,  1133,  -102,
     397,  -661,   894,  1103,   577,   271,   271,   495,   432,   271,
     780,   -99,  1127,   217,   688,    86,   862,   561,   493,   440,
     561,   470,   561,   687,   561,   275,   561,   611,   445,   398,
    -560,   443,   392,   392,  -109,   276,   281,   792,   440,  -559,
     212,   212,   -80,   306,   310,   525,  -558,  1072,   687,  -104,
     434,  -662,   246,   445,  -106,   247,   248,   480,   481,   468,
     469,   394,   493,   -94,   325,   862,   992,   717,   782,   531,
     507,   550,  1148,   544,   402,  -105,   781,   538,   538,   502,
     545,   782,   538,   249,  -101,   250,   895,  -560,   505,   781,
     505,  -561,  -563,   275,   514,   455,   926,   495,   759,   926,
    -658,   516,   519,  -103,   929,  -100,    86,   467,   728,   728,
    1190,   579,   512,   473,   265,   269,   731,   265,   217,   217,
     246,  -562,   934,   247,   248,   419,   477,  1109,   300,   593,
     206,  1057,  1132,  -109,   482,  1134,  -109,  -109,   981,  -564,
     844,   486,   780,   846,   987,   545,   342,   343,  -561,  -563,
     859,   249,   780,   250,   527,   428,   429,   430,   488,   490,
    -549,   844,   533,  -553,  -109,   493,  -109,   212,   212,   212,
     212,   217,   574,   575,   217,   784,   754,   607,  -562,   217,
     217,   732,   617,    86,   569,   570,   571,   572,   737,   684,
      86,    86,   568,   962,   506,   589,  -564,   589,    86,  -555,
     743,   515,   613,   735,   736,   530,   939,   462,    70,   315,
     963,  -554,  -658,   573,   728,   926,   522,  -549,  -658,  1028,
    -553,  -305,   905,   617,   617,   502,   125,  1009,  -115,   707,
     707,   419,   300,   615,   609,   853,  -305,  1070,   449,   609,
    -107,   561,   450,   556,    86,   217,   217,   217,   217,    86,
     217,   217,   514,   767,   768,  -554,   908,   910,   912,   977,
     914,   -99,   915,   520,   463,   983,   985,   615,   271,    86,
     529,  -305,   265,   615,   615,  -662,  1097,   514,  -305,   689,
     495,   780,   -79,   798,  -107,   694,   885,   495,   817,   478,
      86,   535,    57,   217,   514,    86,   315,   265,   622,   725,
     797,   541,   657,   514,   666,   747,    41,   544,   334,    42,
     543,   328,   329,   125,   265,   739,   547,   246,  -114,   890,
     247,   248,   491,   265,   664,   247,   248,   664,   566,   217,
     720,   505,   505,   545,   354,   355,   567,   883,   578,   622,
     622,   105,   584,  -658,   296,   245,  -570,   931,   664,   590,
     250,   841,  1125,    58,   217,  1128,    86,   217,   587,   665,
    1098,   856,   603,   330,   331,   664,   444,    86,   514,  1038,
    1039,   217,   544,   520,   664,    86,   452,   453,   806,   526,
     217,   591,    70,   665,   594,    86,   526,  1181,   265,   728,
     597,   744,   444,   600,   780,  -114,   731,   270,   270,   599,
     665,   270,   990,   871,   780,  1077,   867,  -549,  -113,   665,
     206,   882,   296,   615,   664,  -113,  -106,   889,    86,   734,
     508,  -114,  -549,   770,   795,   865,   862,    86,   851,  -105,
     872,   520,  -109,   842,   843,   270,   270,   900,   835,   664,
     602,   315,   788,   315,   885,   217,   614,  1005,  1019,   665,
    -348,  -432,  -434,    86,  -111,  -658,   677,  -549,   125,  -111,
     125,  -658,  -109,  -111,  -549,  -348,   839,   692,   470,   472,
     691,  1187,   442,   695,   665,   845,  -108,   696,   847,   505,
     217,   431,   777,  -101,  -103,   280,   777,   698,   280,   795,
     795,   555,  1010,  1011,   729,   721,   432,  1012,   212,   217,
    -348,   982,  1110,  1112,  1113,  1114,   280,  -348,   315,   878,
     733,   861,   864,   857,   842,   852,   864,   738,   538,   -94,
     819,   843,   741,   864,   772,   125,   746,   906,   756,   609,
    -108,   433,   212,   982,   769,   819,   790,   125,   434,   245,
    1033,  1034,  1173,  -109,   705,   801,   457,  1082,   505,   877,
    -111,  -100,   125,  -111,   454,   454,  -111,  -111,  1015,   800,
     544,   432,  -108,  -658,   802,   246,  1158,   246,   247,   248,
     247,   248,   840,   854,  1024,   855,   217,    86,  1016,   206,
     523,   838,   617,   988,  -111,   862,  -111,   975,   296,   870,
     276,   881,   876,   276,   942,   589,   458,   776,   249,   879,
     250,   994,   888,   434,   206,   891,  -659,   379,   379,   838,
     217,   276,   897,   819,  1088,   903,   617,   979,   901,   576,
    1090,   907,   617,   617,   514,   909,   707,  -549,   911,   472,
     125,  1185,   246,   307,   311,   247,   248,   913,   916,   943,
     687,   961,  -549,   964,   265,   246,   969,   972,   247,   248,
     973,   305,   995,   976,   996,   379,   379,   270,   474,  -557,
     270,   935,  1000,   249,  1004,   250,  1006,  1017,  1022,   795,
    -553,  1037,  1049,   432,  -557,  -658,  1047,  -549,   250,   998,
    1040,  -658,  1002,   212,  -549,  -553,  1043,   930,  1045,    86,
     520,  1051,   296,  1054,  1060,   664,   315,    86,   622,   483,
    1003,   217,  -665,  1066,   525,  1055,   505,  1056,   475,  -557,
     948,  1146,  1147,   125,   432,   434,  -557,   819,  -659,   562,
    -553,   554,   328,   329,  -659,   887,  1079,  -553,  1061,  1064,
     665,  1065,   622,   217,   125,   327,   328,   329,   622,   622,
    1080,  1091,   589,   589,    86,    86,   472,   379,   379,   484,
    1092,  1104,   472,  1105,  1111,  1115,   434,  1116,    86,  1117,
    -661,   217,   617,  1136,   697,   942,  -665,   974,   524,   949,
      86,    86,   704,   583,   330,   331,   967,  1137,   419,    86,
     548,  -665,   716,   432,  1138,   104,  1139,   104,   330,   331,
      86,    86,   104,   104,  1178,   432,  1151,  1153,   104,   104,
     104,  1155,   924,   104,   306,   310,   426,   427,   428,   429,
     430,  1204,  -661,  1159,  -665,   270,  -665,  1036,   475,  1182,
    -661,  1161,  1042,  -665,  1163,   434,  1213,  1214,   212,   212,
     549,   887,  -662,   864,   125,   104,  1165,   434,   819,   125,
     270,  1142,  1168,   472,   472,  1094,  1095,  1169,  1191,   104,
     758,  1198,   379,  1202,  1205,   125,  1203,   270,  -662,  1206,
    -556,   740,  -295,   921,   226,   130,   270,   819,   819,   246,
     819,  1177,   247,   248,   918,  -556,  -661,  -295,   622,   349,
     350,   351,   352,   353,   125,   125,   270,   125,    86,    86,
     270,  -661,  1085,   338,   328,   329,    86,  1050,  1152,  -306,
     249,   104,   250,   104,   379,  1183,   217,   217,    86,   125,
    -556,   989,  -295,  1135,  -306,   864,  1067,  -556,   270,  -295,
     958,   270,  -662,   492,  -661,  1176,  -661,  1069,   208,  1074,
    -661,   270,   777,  -661,   775,   930,     0,  -662,   930,   589,
     930,  1119,  1124,   472,     0,     0,   330,   331,     0,  -306,
       0,   819,   819,   948,   554,     0,  -306,     0,   472,   472,
     514,   819,   694,   864,   340,   328,   329,     0,   125,   125,
    -662,    73,  -662,    73,   121,   121,  -662,     0,   125,  -662,
     265,     0,   121,   766,    86,     0,    86,     0,     0,    86,
       0,     0,     0,     0,   104,   354,   355,     0,   864,   864,
     783,     0,     0,   787,   833,     0,   104,   104,  -421,     0,
    1131,   949,  1050,   804,     0,     0,   930,   330,   331,   833,
    1069,    73,     0,   212,   212,   121,     0,   864,   432,     0,
       0,   664,   819,   965,   864,   864,     0,   217,     0,     0,
    1209,  1210,    86,    86,   819,     0,     0,  1120,   432,   125,
     924,   121,     0,   924,   930,   924,   930,     0,   930,   104,
     930,   125,   104,   805,   819,   819,   665,   104,   104,     0,
     434,   104,  -421,   246,     0,     0,   247,   248,   104,   104,
       0,   125,   125,   458,     0,   379,   104,  -421,     0,    73,
     434,  1170,     0,   930,     0,   773,   336,   833,   328,   329,
      86,   217,   217,     0,   249,     0,   250,     0,   921,    86,
      86,   921,     0,   774,   921,     0,   921,   270,   270,     0,
    -421,     0,  -421,   949,  1131,     0,   674,   676,     0,  -421,
       0,   924,   104,   104,   104,   104,   104,   104,   104,   104,
       0,  1053,   991,     0,     0,   365,   366,   367,  1044,  1046,
     330,   331,     0,     0,     0,   999,   432,   104,     0,     0,
     674,   676,  1118,     0,     0,     0,     0,  1007,  1008,   924,
       0,   924,   368,   924,  1140,   924,  1014,   432,   104,     0,
      73,   104,     0,   104,     0,     0,   104,   270,  1020,   432,
       0,   475,   921,     0,   833,   925,   270,   270,   434,     0,
     762,   833,   357,   358,   359,   360,     0,     0,   924,   742,
       0,   762,   458,   357,   358,   359,   360,   104,   763,   434,
       0,     0,     0,   270,  1141,     0,     0,   104,   104,   763,
     921,   434,   921,     0,   921,   563,   921,   328,   329,     0,
       0,     0,   104,     0,   104,   104,     0,     0,     0,   356,
       0,   357,   358,   359,   360,   104,     0,    73,     0,   104,
    1123,     0,     0,   104,    73,    73,     0,   361,   104,   921,
       0,     0,    73,   104,   555,   328,   329,   560,   328,   329,
       0,   832,     0,   121,     0,  1081,   564,   328,   329,   330,
     331,   898,   363,  1089,     0,     0,   832,     0,   364,   365,
     366,   367,  1108,     0,     0,  1096,   104,   565,   328,   329,
    1027,     0,  1029,     0,     0,   104,  1030,     0,    73,     0,
       0,     0,   833,    73,     0,     0,   368,   330,   331,   369,
     330,   331,     0,   104,     0,   757,   328,   329,     0,   330,
     331,   104,   551,    73,  1160,  1162,  1164,     0,  1166,  1167,
       0,   833,   833,   762,   833,   357,   358,   359,   360,     0,
     330,   331,     0,     0,    73,     0,     0,     0,   104,    73,
     121,   763,    73,   762,   832,   357,   358,   359,   360,     0,
       0,  1143,     0,  1144,     0,     0,  1145,   104,   330,   331,
       0,   763,  1192,  1193,     0,     0,   363,     0,   270,     0,
     786,     0,   357,   358,   359,   360,  1197,  1199,  1200,  1201,
       0,     0,     0,    73,    73,     0,   363,     0,   361,     0,
       0,  1212,   764,     0,  1106,  1107,     0,  1211,  1217,  1218,
      73,     0,     0,     0,     0,   833,   833,   833,     0,  1179,
    1180,    73,     0,   363,     0,   833,  1126,     0,  1129,    73,
     365,   366,   367,     0,     0,     0,     0,  -302,     0,    73,
    -302,  -302,     0,   246,   104,   104,   247,   248,     0,     0,
       0,   953,   960,     0,     0,     0,     0,   368,   953,     0,
       0,   762,     0,   357,   358,   359,   360,  -302,  -302,     0,
    -302,     0,    73,     0,  1018,     0,   250,  1208,   104,   763,
    1100,    73,   357,   358,   359,   360,  1215,  1216,     0,     0,
       0,  1154,     0,     0,  1156,   121,   833,   121,   763,   978,
     980,     0,     0,   270,   363,   984,   986,    73,   833,     0,
    1023,   416,   417,     0,    74,     0,    74,   122,   122,     0,
       0,     0,     0,     0,   419,   122,     0,     0,   833,   833,
       0,   978,   980,     0,   984,   986,     0,     0,  1184,     0,
       0,     0,  1186,     0,  1188,     0,     0,     0,  1189,   423,
     424,   425,   426,   427,   428,   429,   430,   104,     0,     0,
       0,     0,   121,     0,    74,   104,   104,     0,   122,   104,
       0,     0,     0,   356,   121,   357,   358,   359,   360,   953,
       0,  1207,     0,     0,     0,     0,     0,     0,     0,   121,
       0,   361,     0,     0,   122,     0,     0,     0,     0,     0,
     104,   104,     0,     0,     0,   362,   104,   104,   953,   953,
    1073,   953,   104,   104,     0,   246,   363,     0,   247,   248,
     416,   417,   364,   365,   366,   367,   104,     0,     0,   104,
       0,    73,    74,   419,     0,     0,     0,     0,   104,   104,
       0,     0,   502,  -688,   803,  1078,   249,   104,   250,     0,
     368,     0,     0,   369,     0,     0,     0,     0,   104,   104,
     425,   426,   427,   428,   429,   430,   370,   121,  1078,     0,
       0,     0,   404,   405,   406,   407,   408,   409,   410,   411,
     412,   413,   414,   415,     0,     0,     0,     0,   416,   417,
       0,     0,   953,   953,   953,   416,   417,     0,  1073,     0,
       0,   419,   953,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   420,    74,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,     0,     0,   426,   427,   428,   429,
     430,     0,  -280,    73,     0,     0,   104,     0,     0,     0,
     121,    73,    73,     0,     0,     0,   104,   104,     0,     0,
       0,     0,     0,     0,   104,     0,     0,     0,     0,     0,
       0,   121,     0,   953,   104,   104,   104,     0,     0,   356,
       0,   357,   358,   359,   360,   832,    73,     0,     0,     0,
       0,     0,    73,    73,     0,     0,     0,   361,    73,    73,
      74,     0,     0,     0,     0,   953,   953,    74,    74,     0,
       0,   362,    73,     0,     0,    74,     0,     0,     0,     0,
       0,     0,   363,     0,    73,    73,   122,     0,   364,   365,
     366,   367,     0,    73,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    73,   356,     0,   357,   358,
     359,   360,   104,     0,   104,     0,   368,   104,     0,   369,
       0,    74,     0,     0,   361,     0,    74,     0,     0,     0,
       0,   121,   370,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,   363,
       0,     0,   121,     0,     0,   364,   365,   366,   367,     0,
       0,     0,     0,     0,     0,   104,     0,    74,     0,     0,
     104,   104,    74,   122,     0,    74,     0,     0,     0,     0,
       0,   121,   121,   368,   121,     0,   369,     0,     0,     0,
       0,     0,    73,     0,     0,     0,   356,  1121,   357,   358,
     359,   360,    73,    73,     0,     0,   121,     0,     0,     0,
      73,     0,     0,     0,   361,     0,    74,    74,     0,     0,
       0,     0,    73,     0,     0,     0,     0,     0,   104,   104,
     104,     0,   103,    74,   103,   128,   128,   104,   104,   363,
       0,     0,     0,   231,    74,   364,   365,   366,   367,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,   121,   121,     0,     0,     0,
       0,     0,     0,   368,     0,   121,   369,     0,     0,     0,
       0,     0,   103,     0,     0,     0,   317,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,    73,     0,
      73,     0,     0,    73,    74,     0,     0,     0,     0,     0,
       0,  -455,   317,     0,     0,     0,     0,     0,   122,     0,
     122,     0,     0,     0,  -455,  -455,  -455,  -455,  -455,  -455,
      74,  -455,     0,     0,     0,     0,  -455,     0,  -455,     0,
       0,     0,     0,     0,     0,     0,   121,     0,  -455,  -455,
     103,  -455,  -455,  -455,  -455,  -455,    73,    73,   121,   356,
       0,   357,   358,   359,   360,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   361,   121,   121,
       0,     0,     0,     0,     0,   122,     0,     0,     0,     0,
       0,     0,     0,   581,     0,     0,     0,   122,     0,     0,
       0,  -501,   363,     0,   974,     0,     0,     0,   364,   365,
     366,   367,   122,     0,    73,     0,  -501,     0,     0,     0,
       0,     0,     0,    73,    73,     0,  -455,     0,     0,  -455,
    -455,     0,     0,     0,     0,     0,   368,  -455,     0,   369,
       0,   103,   356,     0,   357,   358,   359,   360,     0,     0,
    -455,  -501,     0,  -455,    74,     0,  -455,  -455,  -501,  -455,
     361,     0,     0,   356,     0,   357,   358,   359,   360,     0,
       0,     0,     0,     0,     0,     0,   778,     0,     0,     0,
       0,   361,     0,     0,     0,   363,     0,     0,     0,     0,
     122,   364,   365,   366,   367,     0,     0,   902,     0,     0,
     100,     0,   100,   127,   127,   127,   363,     0,     0,     0,
       0,   230,   364,   365,   366,   367,     0,     0,   103,   368,
       0,     0,   369,     0,     0,   103,   103,     0,     0,     0,
       0,     0,     0,   103,     0,     0,     0,     0,     0,     0,
     368,     0,     0,   369,   317,     0,     0,     0,     0,     0,
     100,     0,     0,     0,   316,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
       0,     0,     0,   122,    74,    74,     0,     0,     0,   103,
     316,     0,     0,     0,   103,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,     0,     0,     0,     0,    74,
       0,     0,     0,     0,     0,    74,    74,     0,   100,     0,
       0,    74,    74,     0,     0,   103,     0,     0,     0,     0,
     103,   317,     0,   623,     0,    74,     0,     0,     0,     0,
       0,     0,    83,     0,    83,     0,     0,    74,    74,     0,
       0,   803,     0,   227,     0,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    74,    74,     0,
       0,     0,     0,     0,   623,   623,     0,     0,     0,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   103,    83,     0,   122,   416,   417,     0,     0,   122,
       0,     0,   103,     0,     0,     0,     0,     0,   419,   100,
     103,     0,     0,     0,     0,   122,     0,     0,     0,     0,
     103,     0,     0,     0,     0,     0,     0,     0,     0,   420,
       0,   421,   422,   423,   424,   425,   426,   427,   428,   429,
     430,     0,     0,     0,   122,   122,     0,   122,     0,     0,
       0,     0,     0,   103,     0,    74,     0,     0,     0,     0,
      83,     0,   103,     0,     0,    74,    74,     0,     0,   122,
       0,     0,     0,    74,     0,     0,   317,     0,   317,     0,
       0,     0,     0,     0,     0,    74,   100,     0,   103,     0,
       0,     0,     0,   100,   100,     0,     0,     0,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   316,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   122,   122,
       0,     0,     0,     0,     0,     0,     0,     0,   122,     0,
       0,     0,     0,   317,     0,     0,     0,   100,     0,     0,
       0,    83,   100,     0,     0,   128,     0,     0,     0,     0,
       0,    74,     0,    74,  -688,     0,    74,     0,     0,     0,
     128,     0,   100,     0,     0,     0,     0,  -688,  -688,  -688,
    -688,  -688,  -688,     0,  -688,     0,     0,     0,     0,  -688,
    -688,  -688,     0,   100,     0,     0,     0,     0,   100,   316,
       0,  -688,  -688,     0,  -688,  -688,  -688,  -688,  -688,   122,
       0,     0,   103,     0,     0,     0,     0,     0,     0,    74,
      74,   122,     0,     0,     0,     0,     0,     0,    83,     0,
       0,     0,     0,     0,     0,    83,    83,     0,     0,     0,
       0,   122,   122,    83,     0,     0,     0,     0,   128,     0,
       0,     0,     0,     0,  -688,     0,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -688,
     100,     0,     0,     0,     0,     0,     0,    74,   100,  -688,
       0,     0,  -688,  -688,     0,     0,    74,    74,   100,    83,
       0,     0,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,  -688,  -688,     0,     0,     0,     0,   275,  -688,
    -688,  -688,  -688,     0,    83,     0,   101,     0,   101,     0,
       0,   100,     0,     0,   103,     0,     0,     0,     0,     0,
     100,   317,   103,   623,     0,    83,     0,     0,     0,     0,
      83,     0,     0,   618,   316,     0,   316,     0,     0,     0,
       0,     0,   128,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,   623,     0,     0,
       0,     0,     0,   623,   623,     0,     0,     0,     0,   103,
     103,     0,     0,     0,   618,   618,     0,     0,     0,     0,
       0,     0,     0,   103,     0,     0,     0,     0,     0,     0,
       0,    83,     0,     0,     0,   103,   103,     0,     0,     0,
       0,   316,    83,     0,   103,     0,     0,     0,     0,     0,
      83,     0,     0,   127,     0,   103,   103,     0,     0,     0,
      83,     0,     0,     0,   101,     0,     0,     0,   127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   128,     0,     0,     0,     0,   128,     0,     0,
       0,     0,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,    83,   128,     0,     0,     0,     0,     0,     0,
     100,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -689,  -689,  -689,  -689,   408,   409,    83,     0,
    -689,  -689,   128,   128,     0,   128,     0,     0,   416,   417,
       0,     0,     0,   623,     0,     0,   127,     0,     0,     0,
       0,   419,     0,   103,   103,   101,     0,  1087,     0,     0,
       0,   103,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   404,   405,   406,   407,   408,   409,
     410,   411,   412,   413,  -689,  -689,   128,   128,     0,     0,
     416,   417,   100,     0,     0,     0,   128,     0,     0,   316,
     100,     0,   101,   419,     0,     0,     0,     0,     0,   101,
     101,     0,     0,     0,     0,     0,     0,   101,     0,   103,
     127,   103,    83,     0,   103,     0,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   100,   100,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   100,     0,   101,     0,     0,     0,   128,   101,     0,
       0,     0,     0,   100,   100,     0,     0,   103,   103,   128,
       0,     0,   100,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   100,   100,     0,     0,     0,     0,   128,
     128,     0,     0,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,   101,     0,     0,   101,     0,     0,
     127,     0,     0,     0,     0,   127,     0,     0,     0,     0,
       0,     0,     0,     0,    83,   103,     0,     0,     0,     0,
       0,   127,    83,   618,   103,   103,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   101,   101,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     127,   127,     0,   127,     0,   101,     0,   618,     0,     0,
       0,     0,     0,   618,   618,     0,   101,     0,     0,    83,
      83,   100,   100,     0,   101,  1086,     0,     0,     0,   100,
       0,     0,     0,    83,   101,     0,     0,     0,     0,     0,
       0,   100,     0,     0,     0,    83,    83,     0,     0,     0,
       0,     0,     0,     0,    83,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    83,   101,     0,     0,
       0,     0,     0,   266,   266,     0,   101,   266,     0,     0,
       0,     0,     0,     0,   127,   127,     0,     0,     0,     0,
       0,     0,     0,     0,   127,     0,     0,     0,     0,     0,
       0,     0,   101,     0,   288,   290,   291,   292,     0,     0,
       0,   266,   308,     0,     0,     0,     0,   100,     0,   100,
       0,     0,   100,   345,   346,     0,     0,     0,     0,     0,
       0,     0,   404,   405,   406,   407,   408,   409,   410,   411,
     412,   413,   414,   415,     0,     0,     0,     0,   416,   417,
       0,     0,     0,   618,     0,     0,     0,     0,     0,     0,
       0,   419,     0,    83,    83,   127,     0,  1084,     0,     0,
       0,    83,     0,     0,     0,   100,   100,   127,     0,     0,
       0,     0,   420,    83,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,     0,     0,     0,   127,   127,     0,
       0,     0,  -280,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
       0,     0,   100,   100,     0,     0,     0,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,     0,    83,
       0,    83,     0,     0,    83,    14,     0,   108,   109,    17,
      18,     0,     0,     0,     0,     0,   110,   111,   112,    22,
      23,    24,    25,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,   114,    34,    35,
      36,   115,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,   116,     0,    83,    83,   498,
     499,   500,   345,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   266,   117,     0,   266,   118,   101,     0,
     119,    52,     0,    53,    54,     0,   101,   101,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,     0,     0,     0,     0,
     120,   101,     0,     0,    83,    83,     0,   101,   101,     0,
       0,     0,     0,   101,   101,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   101,
     101,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,   586,     0,     0,     0,     0,     0,     0,   101,
     101,     0,     0,   596,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   608,     0,     0,     0,     0,
     619,     0,   625,   626,   627,   628,   629,   630,   631,   632,
     633,   634,   635,   636,   637,   638,     0,   640,   641,   642,
     643,   644,   645,   646,   647,   648,   649,   650,   651,     0,
       0,   266,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   673,   673,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   266,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,   673,     0,   266,     0,   673,   673,   101,   101,     0,
       0,     0,   266,     0,     0,   101,     0,     0,     0,     0,
       0,   719,     0,     0,   722,   723,     0,   101,     0,   724,
       0,     0,   727,     0,   730,     0,   308,   292,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   673,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   727,     0,     0,   308,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   266,     0,     0,
       0,     0,     0,     0,   404,   405,   406,   407,   408,   409,
     410,     0,   412,   413,   760,   761,     0,     0,     0,     0,
     416,   417,     0,   101,     0,   101,     0,     0,   101,     0,
       0,     0,   771,   419,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   789,     0,     0,   796,     0,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -296,   101,   101,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -296,  -296,  -296,  -296,  -296,  -296,     0,
    -296,     0,     0,     0,     0,  -296,     0,  -296,  -296,  -296,
       0,     0,     0,     0,     0,     0,     0,  -296,  -296,     0,
    -296,  -296,  -296,  -296,  -296,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -296,     0,   101,
     858,     0,     0,   771,   789,     0,     0,     0,   101,   101,
       0,     0,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,     0,     0,     0,     0,  -296,  -296,
    -296,     0,     0,  -296,   884,     0,     0,     0,     0,  -296,
       0,  -296,     0,   727,   308,  -296,     0,     0,     0,     0,
       0,     0,     0,  -296,     0,  -296,     0,     0,  -296,  -296,
       0,     0,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,     0,     0,     0,     0,     0,  -296,
    -296,  -296,  -296,     0,     0,  -296,  -296,  -296,  -296,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   933,     0,     0,     0,     0,
     673,   936,     0,   266,     0,     0,     0,     0,     0,     0,
       0,     0,   266,   308,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   673,   673,     0,     0,     0,   727,
     673,   673,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   673,   673,     0,   673,
     673,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1013,     0,     0,     0,   292,     0,     0,   667,   662,     0,
       0,   668,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1025,  1026,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,  1031,  1032,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
    1048,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,   498,     0,     0,     0,     0,     0,
       0,     0,     0,   499,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,  1075,  1076,     0,     0,     0,     0,   203,
     673,     0,     0,     0,     0,     0,     0,  -688,     3,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,   673,     0,     0,    14,     0,    15,    16,
      17,    18,     0,     0,   308,     0,     0,    19,    20,    21,
      22,    23,    24,    25,     0,     0,    26,     0,     0,     0,
       0,     0,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,   722,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,    50,     0,
       0,    51,    52,     0,    53,    54,     0,    55,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
    -420,    63,  -688,     0,     0,  -688,  -688,     0,     0,     0,
       0,     0,     0,  -420,  -420,  -420,  -420,  -420,  -420,     0,
    -420,    64,    65,    66,     0,  -420,  -420,  -420,  -420,     0,
       0,     0,     0,  -688,     0,  -688,     0,  -420,  -420,   266,
    -420,  -420,  -420,  -420,  -420,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -420,  -420,  -420,  -420,  -420,  -420,  -420,  -420,
    -420,  -420,  -420,  -420,     0,     0,     0,     0,  -420,  -420,
    -420,     0,     0,  -420,     0,     0,     0,     0,     0,  -420,
       0,  -420,     0,     0,     0,  -420,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -420,     0,     0,  -420,  -420,
       0,     0,  -420,     0,  -420,  -420,  -420,  -420,  -420,  -420,
    -420,  -420,  -420,  -420,     0,     0,  -549,     0,  -420,  -420,
    -420,  -420,  -420,     0,   275,  -420,  -420,  -420,  -420,  -549,
    -549,  -549,  -549,  -549,  -549,     0,  -549,     0,     0,     0,
       0,  -549,     0,  -549,  -549,     0,     0,     0,     0,     0,
       0,     0,     0,  -549,  -549,     0,  -549,  -549,  -549,  -549,
    -549,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   496,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -549,  -549,
    -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,
       0,     0,     0,     0,  -549,  -549,  -549,     0,  -549,  -549,
       0,     0,     0,     0,     0,  -549,     0,  -549,     0,     0,
       0,  -549,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -549,     0,     0,  -549,  -549,     0,  -549,  -549,     0,
    -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,
       0,     0,  -688,     0,     0,  -549,  -549,  -549,  -549,     0,
       0,  -549,  -549,  -549,  -549,  -688,  -688,  -688,  -688,  -688,
    -688,     0,  -688,     0,     0,     0,     0,  -688,  -688,  -688,
    -688,     0,     0,     0,     0,     0,     0,     0,     0,  -688,
    -688,     0,  -688,  -688,  -688,  -688,  -688,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -688,  -688,  -688,  -688,  -688,  -688,
    -688,  -688,  -688,  -688,  -688,  -688,     0,     0,     0,     0,
    -688,  -688,  -688,     0,     0,  -688,     0,     0,     0,     0,
       0,  -688,     0,  -688,     0,     0,     0,  -688,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -688,     0,     0,
    -688,  -688,     0,     0,  -688,     0,  -688,  -688,  -688,  -688,
    -688,  -688,  -688,  -688,  -688,  -688,     0,     0,  -460,     0,
    -688,  -688,  -688,  -688,  -688,     0,   275,  -688,  -688,  -688,
    -688,  -460,  -460,  -460,  -460,  -460,  -460,     0,  -460,     0,
       0,     0,     0,  -460,  -659,  -460,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -460,  -460,     0,  -460,  -460,
    -460,  -460,  -460,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -553,  -553,  -553,  -553,  -553,  -553,  -553,  -553,  -553,  -553,
    -553,  -553,     0,     0,     0,     0,  -553,  -553,  -553,     0,
    -553,  -460,     0,     0,     0,     0,     0,     0,     0,  -553,
       0,     0,     0,  -553,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -460,     0,     0,  -460,  -460,     0,  -553,
    -553,     0,  -553,  -553,  -460,  -553,  -553,  -553,  -553,  -553,
    -553,  -553,     0,     0,  -490,     0,  -659,  -460,  -553,  -460,
    -460,     0,  -659,  -460,  -460,  -553,  -460,  -490,  -490,  -490,
    -490,  -490,  -490,     0,  -490,     0,     0,     0,     0,  -490,
    -658,  -490,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -490,  -490,     0,  -490,  -490,  -490,  -490,  -490,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -549,  -549,  -549,  -549,
    -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,     0,     0,
       0,     0,  -549,  -549,  -549,     0,  -549,  -490,     0,     0,
       0,     0,     0,     0,     0,  -549,     0,     0,     0,  -549,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -490,
       0,     0,  -490,  -490,     0,  -549,  -549,     0,  -549,  -549,
    -490,  -549,  -549,  -549,  -549,  -549,  -549,  -549,     0,     0,
    -456,     0,  -658,  -490,  -549,  -490,  -490,     0,  -658,  -490,
    -490,  -549,  -490,  -456,  -456,  -456,  -456,  -456,  -456,     0,
    -456,     0,     0,     0,     0,  -456,     0,  -456,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -456,  -456,     0,
    -456,  -456,  -456,  -456,  -456,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -507,     0,     0,
    -507,     0,     0,  -507,     0,     0,     0,     0,     0,     0,
       0,     0,  -507,  -507,  -507,  -507,  -507,  -507,  -507,  -507,
    -507,  -507,  -507,  -507,     0,     0,     0,     0,  -507,  -507,
    -507,     0,     0,  -456,     0,     0,     0,     0,     0,     0,
       0,  -507,     0,     0,     0,  -507,     0,  -507,     0,     0,
       0,     0,     0,     0,     0,  -456,     0,     0,  -456,  -456,
       0,     0,  -507,     0,  -507,  -507,  -456,  -507,  -507,  -507,
    -507,  -507,  -507,  -507,     0,     0,  -688,     0,     0,  -456,
    -507,  -456,  -456,     0,     0,  -456,  -456,  -507,  -456,  -688,
    -688,  -688,  -688,  -688,  -688,     0,  -688,     0,     0,     0,
       0,  -688,     0,  -688,  -688,     0,     0,     0,     0,     0,
       0,     0,     0,  -688,  -688,     0,  -688,  -688,  -688,  -688,
    -688,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -688,  -688,
    -688,  -688,  -688,  -688,  -688,  -688,  -688,  -688,  -688,  -688,
       0,     0,     0,     0,  -688,  -688,  -688,     0,     0,  -688,
       0,     0,     0,     0,     0,  -688,     0,  -688,     0,     0,
       0,  -688,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -688,     0,     0,  -688,  -688,     0,     0,  -688,     0,
    -688,  -688,  -688,  -688,  -688,  -688,  -688,  -688,  -688,  -688,
       0,     0,  -665,     0,     0,  -688,  -688,  -688,  -688,     0,
     275,  -688,  -688,  -688,  -688,  -665,  -665,  -665,     0,  -665,
    -665,     0,  -665,     0,     0,     0,     0,  -665,  -665,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -665,
    -665,     0,  -665,  -665,  -665,  -665,  -665,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,     0,     0,     0,     0,
    -665,  -665,  -665,     0,   836,  -665,     0,     0,     0,     0,
       0,     0,     0,  -665,     0,     0,     0,  -665,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -665,     0,     0,
    -665,  -665,     0,  -110,  -665,     0,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,  -665,     0,     0,  -665,     0,
    -665,  -665,  -665,     0,  -102,     0,     0,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,     0,  -665,  -665,     0,  -665,     0,
       0,     0,     0,  -665,  -665,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -665,  -665,     0,  -665,  -665,
    -665,  -665,  -665,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,     0,     0,     0,     0,  -665,  -665,  -665,     0,
     836,  -665,     0,     0,     0,     0,     0,     0,     0,  -665,
       0,     0,     0,  -665,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -665,     0,     0,  -665,  -665,     0,  -110,
    -665,     0,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
    -665,  -665,     0,     0,  -457,     0,  -665,  -665,  -665,     0,
    -665,     0,     0,  -665,  -665,  -665,  -665,  -457,  -457,  -457,
    -457,  -457,  -457,     0,  -457,     0,     0,     0,     0,  -457,
       0,  -457,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -457,  -457,     0,  -457,  -457,  -457,  -457,  -457,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -558,  -558,  -558,  -558,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,     0,     0,
       0,     0,  -558,  -558,  -558,     0,     0,  -457,     0,     0,
       0,     0,     0,     0,     0,  -558,     0,     0,     0,  -558,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -457,
       0,     0,  -457,  -457,     0,     0,  -558,     0,  -558,  -558,
    -457,  -558,  -558,  -558,  -558,  -558,  -558,  -558,     0,     0,
    -458,     0,     0,  -457,  -558,  -457,  -457,     0,     0,  -457,
    -457,  -558,  -457,  -458,  -458,  -458,  -458,  -458,  -458,     0,
    -458,     0,     0,     0,     0,  -458,     0,  -458,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -458,  -458,     0,
    -458,  -458,  -458,  -458,  -458,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -560,  -560,  -560,  -560,  -560,  -560,  -560,  -560,
    -560,  -560,  -560,  -560,     0,     0,     0,     0,  -560,  -560,
    -560,     0,     0,  -458,     0,     0,     0,     0,     0,     0,
       0,  -560,     0,     0,     0,  -560,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -458,     0,     0,  -458,  -458,
       0,     0,  -560,     0,  -560,  -560,  -458,  -560,  -560,  -560,
    -560,  -560,  -560,  -560,     0,     0,  -459,     0,     0,  -458,
    -560,  -458,  -458,     0,     0,  -458,  -458,  -560,  -458,  -459,
    -459,  -459,  -459,  -459,  -459,     0,  -459,     0,     0,     0,
       0,  -459,     0,  -459,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -459,  -459,     0,  -459,  -459,  -459,  -459,
    -459,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -561,  -561,
    -561,  -561,  -561,  -561,  -561,  -561,  -561,  -561,  -561,  -561,
       0,     0,     0,     0,  -561,  -561,  -561,     0,     0,  -459,
       0,     0,     0,     0,     0,     0,     0,  -561,     0,     0,
       0,  -561,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -459,     0,     0,  -459,  -459,     0,     0,  -561,     0,
    -561,  -561,  -459,  -561,  -561,  -561,  -561,  -561,  -561,  -561,
       0,     0,  -454,     0,     0,  -459,  -561,  -459,  -459,     0,
       0,  -459,  -459,  -561,  -459,  -454,  -454,  -454,  -454,  -454,
    -454,     0,  -454,     0,     0,     0,     0,  -454,     0,  -454,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -454,
    -454,     0,  -454,  -454,  -454,  -454,  -454,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -500,  -500,  -500,  -500,  -500,  -500,
    -500,  -500,  -500,  -500,  -500,  -500,     0,     0,     0,     0,
    -500,  -500,  -500,     0,     0,  -454,     0,     0,     0,     0,
       0,     0,     0,  -500,     0,     0,     0,  -500,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -454,     0,     0,
    -454,  -454,     0,     0,  -500,     0,  -500,  -500,  -454,  -500,
    -500,  -500,  -500,  -500,  -500,  -500,     0,     0,  -455,     0,
       0,  -454,  -500,  -454,  -454,     0,     0,  -454,  -454,  -500,
    -454,  -455,  -455,  -455,  -455,  -455,  -455,     0,  -455,     0,
       0,     0,     0,  -455,     0,  -455,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -455,  -455,     0,  -455,  -455,
    -455,  -455,  -455,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -501,  -501,  -501,  -501,  -501,  -501,  -501,  -501,  -501,  -501,
    -501,  -501,     0,     0,     0,     0,  -501,  -501,  -501,     0,
       0,  -455,     0,     0,     0,     0,     0,     0,     0,  -501,
       0,     0,     0,  -501,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -455,     0,     0,  -455,  -455,     0,     0,
    -501,     0,  -501,  -501,  -455,  -501,  -501,  -501,  -501,  -501,
    -501,  -501,     0,     0,  -305,     0,     0,  -455,  -501,  -455,
    -455,     0,     0,  -455,  -455,  -501,  -455,  -305,  -305,  -305,
       0,  -305,  -305,     0,  -305,     0,     0,     0,     0,  -305,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -305,  -305,     0,  -305,  -305,  -305,  -305,  -305,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -305,  -305,  -305,  -305,
    -305,  -305,  -305,  -305,  -305,  -305,  -305,  -305,     0,     0,
       0,     0,  -305,  -305,  -305,     0,   837,  -305,     0,     0,
       0,     0,     0,     0,     0,  -305,     0,     0,     0,  -305,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -305,
       0,     0,  -305,  -305,     0,  -112,  -305,     0,  -305,  -305,
    -305,  -305,  -305,  -305,  -305,  -305,  -305,  -305,     0,     0,
    -305,     0,     0,  -305,  -305,     0,  -104,     0,     0,  -305,
    -305,  -305,  -305,  -305,  -305,  -305,     0,  -305,  -305,     0,
    -305,     0,     0,     0,     0,  -305,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -305,  -305,     0,
    -305,  -305,  -305,  -305,  -305,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -305,  -305,  -305,  -305,  -305,  -305,  -305,  -305,
    -305,  -305,  -305,  -305,     0,     0,     0,     0,  -305,  -305,
    -305,     0,   837,  -305,     0,     0,     0,     0,     0,     0,
       0,  -305,     0,     0,     0,  -305,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -305,     0,     0,  -305,  -305,
       0,  -112,  -305,     0,  -305,  -305,  -305,  -305,  -305,  -305,
    -305,  -305,  -305,  -305,     0,     0,   -57,     0,     0,  -305,
    -305,     0,  -305,     0,     0,  -305,  -305,  -305,  -305,   -57,
     -57,   -57,   -57,   -57,   -57,     0,   -57,     0,     0,     0,
       0,   403,     0,   -57,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   -57,   -57,     0,   -57,   -57,   -57,   -57,
     -57,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   413,   414,   415,
       0,     0,     0,     0,   416,   417,     0,     0,     0,   418,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   -57,     0,     0,   -57,   -57,     0,     0,   420,     0,
     421,   422,   423,   424,   425,   426,   427,   428,   429,   430,
       0,     0,     0,     0,     0,   -57,     0,     0,     0,     0,
       0,   -57,   -57,   294,   -57,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,  -688,  -688,  -688,     0,     0,
    -688,    14,     0,    15,    16,    17,    18,     0,     0,     0,
       0,     0,    19,    20,    21,    22,    23,    24,    25,     0,
       0,    26,     0,     0,     0,     0,     0,    27,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,  -688,     0,     0,
    -688,  -688,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -688,   294,
    -688,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,  -688,     0,  -688,  -688,    14,     0,    15,
      16,    17,    18,     0,     0,     0,     0,     0,    19,    20,
      21,    22,    23,    24,    25,     0,     0,    26,     0,     0,
       0,     0,     0,    27,     0,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,  -688,     0,     0,  -688,  -688,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -688,   294,  -688,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,  -688,
       0,     0,  -688,    14,  -688,    15,    16,    17,    18,     0,
       0,     0,     0,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,    27,
       0,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,  -688,
       0,     0,  -688,  -688,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -688,   294,  -688,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,  -688,     0,     0,  -688,    14,
       0,    15,    16,    17,    18,  -688,     0,     0,     0,     0,
      19,    20,    21,    22,    23,    24,    25,     0,     0,    26,
       0,     0,     0,     0,     0,    27,     0,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,  -688,     0,     0,  -688,  -688,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,    65,    66,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -688,   294,  -688,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,  -688,     0,     0,  -688,    14,     0,    15,    16,    17,
      18,     0,     0,     0,     0,     0,    19,    20,    21,    22,
      23,    24,    25,     0,     0,    26,     0,     0,     0,     0,
       0,    27,     0,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,    50,     0,     0,
      51,    52,     0,    53,    54,     0,    55,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,  -688,     0,     0,  -688,  -688,     3,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
      64,    65,    66,     0,    14,     0,    15,    16,    17,    18,
       0,     0,  -688,     0,  -688,    19,    20,    21,    22,    23,
      24,    25,     0,     0,    26,     0,     0,     0,     0,     0,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
    -688,     0,     0,  -688,  -688,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
      65,    66,     0,     0,  -688,     0,     0,     0,     0,     0,
       0,  -688,   294,  -688,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,  -688,  -688,     0,     0,     0,
      14,     0,    15,    16,    17,    18,     0,     0,     0,     0,
       0,    19,    20,    21,    22,    23,    24,    25,     0,     0,
      26,     0,     0,     0,     0,     0,    27,     0,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,  -688,     0,     0,  -688,
    -688,   294,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,    64,    65,    66,     0,    14,
       0,    15,    16,    17,    18,     0,     0,  -688,     0,  -688,
      19,    20,    21,    22,    23,    24,    25,     0,     0,    26,
       0,     0,     0,     0,     0,    27,     0,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,    50,     0,     0,   295,    52,     0,    53,    54,     0,
      55,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,  -688,     0,     0,  -688,  -688,
     294,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,    64,    65,    66,     0,    14,     0,
      15,    16,    17,    18,     0,  -688,  -688,     0,  -688,    19,
      20,    21,    22,    23,    24,    25,     0,     0,    26,     0,
       0,     0,     0,     0,    27,     0,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
      50,     0,     0,    51,    52,     0,    53,    54,     0,    55,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,  -688,     0,     0,  -688,  -688,   294,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,    64,    65,    66,     0,    14,     0,    15,
      16,    17,    18,     0,  -688,  -688,     0,  -688,    19,    20,
      21,    22,    23,    24,    25,     0,     0,    26,     0,     0,
       0,     0,     0,    27,     0,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,  -688,     0,     0,  -688,  -688,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,    65,    66,     0,     0,  -688,     0,     0,
       0,     0,     0,     0,  -688,   294,  -688,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,  -688,
       0,     0,     0,    14,     0,    15,    16,    17,    18,     0,
       0,     0,     0,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,    27,
       0,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,  -688,
       0,     0,  -688,  -688,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,    64,    65,
      66,     0,    14,     0,    15,    16,    17,    18,     0,     0,
    -688,     0,  -688,    19,    20,    21,    22,    23,    24,    25,
       0,     0,    26,     0,     0,     0,     0,     0,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,    50,     0,     0,    51,    52,     0,
      53,    54,     0,    55,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,   246,     0,
       0,   247,   248,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,    64,    65,    66,
       0,    14,     0,    15,    16,    17,    18,     0,     0,   249,
       0,   250,    19,    20,    21,    22,    23,    24,    25,     0,
       0,    26,     0,     0,     0,     0,     0,    27,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,   246,     0,     0,
     247,   248,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,    64,    65,    66,     0,
      14,     0,   108,   109,    17,    18,     0,     0,   249,     0,
     250,   110,   111,   112,    22,   807,   808,   809,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,   966,    34,    35,    36,   811,    38,     0,    39,
      40,    41,     0,     0,   812,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,   945,     0,     0,   119,    52,     0,   814,   815,
       0,     0,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,  -652,     0,     0,  -652,
    -652,     0,     0,     0,     0,     0,     0,     0,   818,     0,
       0,     0,     0,     0,     0,    64,   264,    66,     0,     0,
    -485,     0,     0,  -485,     0,     0,     0,  -652,     0,  -652,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,     0,     0,     0,    14,     0,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,  1052,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,  -281,     0,     0,  -281,  -281,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,   264,    66,     0,     0,     0,     0,  -281,  -281,
       0,     0,     0,  -281,     0,  -281,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,     0,
       0,     0,    14,     0,   108,   109,    17,    18,     0,     0,
       0,     0,     0,   110,   111,   112,    22,  1058,    24,    25,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,    32,  1059,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    52,     0,
      53,    54,     0,     0,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,  -657,     0,
       0,  -657,  -657,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,   264,    66,
       0,     0,  -489,     0,     0,  -657,     0,     0,     0,  -657,
       0,  -657,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,     0,     0,     0,    14,     0,
      15,    16,    17,    18,     0,     0,     0,     0,     0,    19,
      20,    21,    22,    23,    24,    25,     0,     0,    26,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,   246,     0,     0,   247,   248,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,    64,    65,    66,     0,    14,     0,   108,
     109,    17,    18,     0,     0,   249,     0,   250,   110,   111,
     112,    22,    23,    24,    25,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    52,     0,    53,    54,     0,     0,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,   246,     0,     0,   247,   248,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,    64,   264,    66,     0,    14,     0,    15,    16,
      17,    18,     0,     0,   249,     0,   250,    19,    20,    21,
      22,    23,    24,    25,     0,     0,    26,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,   246,     0,     0,   247,   248,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   250,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
       0,     0,     0,   155,   156,   157,   158,   159,   160,   161,
     162,   163,   164,     0,     0,     0,     0,     0,   165,   166,
     167,   168,   169,   170,   171,   172,    35,    36,   173,    38,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   116,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
       0,     0,     0,     0,     0,     0,   203,   204,  -658,  -658,
    -658,  -658,  -658,  -658,  -658,  -658,  -658,     0,     0,     0,
       0,     0,     0,     0,  -658,     0,  -658,  -658,  -658,  -658,
       0,  -658,     0,     0,     0,  -658,  -658,  -658,  -658,  -658,
    -658,  -658,     0,     0,  -658,     0,     0,     0,     0,     0,
       0,     0,     0,  -658,  -658,  -658,  -658,  -658,  -658,  -658,
    -658,  -658,     0,  -658,  -658,  -658,     0,     0,  -658,     0,
       0,  -658,  -658,     0,  -658,  -658,  -658,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -658,
    -658,     0,     0,     0,     0,     0,  -658,     0,     0,  -658,
    -658,     0,  -658,  -658,     0,  -658,     0,  -658,  -658,  -658,
       0,  -658,  -658,  -658,     0,  -658,  -658,  -658,     0,  -658,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -658,
    -658,  -658,     0,  -658,     0,     0,     0,     0,     0,  -658,
    -659,  -659,  -659,  -659,  -659,  -659,  -659,  -659,  -659,     0,
       0,     0,     0,     0,     0,     0,  -659,     0,  -659,  -659,
    -659,  -659,     0,  -659,     0,     0,     0,  -659,  -659,  -659,
    -659,  -659,  -659,  -659,     0,     0,  -659,     0,     0,     0,
       0,     0,     0,     0,     0,  -659,  -659,  -659,  -659,  -659,
    -659,  -659,  -659,  -659,     0,  -659,  -659,  -659,     0,     0,
    -659,     0,     0,  -659,  -659,     0,  -659,  -659,  -659,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -659,  -659,     0,     0,     0,     0,     0,  -659,     0,
       0,  -659,  -659,     0,  -659,  -659,     0,  -659,     0,  -659,
    -659,  -659,     0,  -659,  -659,  -659,     0,  -659,  -659,  -659,
       0,  -659,     0,     0,     0,     0,     0,     0,  -661,  -661,
    -661,  -661,  -661,  -661,  -661,  -661,  -661,     0,     0,     0,
       0,  -659,  -659,  -659,  -661,  -659,  -661,  -661,  -661,  -661,
       0,  -659,     0,     0,     0,  -661,  -661,  -661,  -661,  -661,
    -661,  -661,     0,     0,  -661,     0,     0,     0,     0,     0,
       0,     0,     0,  -661,  -661,  -661,  -661,  -661,  -661,  -661,
    -661,  -661,     0,  -661,  -661,  -661,     0,     0,  -661,     0,
       0,  -661,  -661,     0,  -661,  -661,  -661,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -661,
    -661,     0,     0,     0,     0,     0,  -661,   868,     0,  -661,
    -661,     0,  -661,  -661,     0,  -661,     0,  -661,  -661,  -661,
       0,  -661,  -661,  -661,     0,  -661,  -661,  -661,     0,  -661,
       0,     0,     0,     0,     0,     0,  -110,  -662,  -662,  -662,
    -662,  -662,  -662,  -662,  -662,  -662,     0,     0,     0,  -661,
    -661,  -661,     0,  -662,     0,  -662,  -662,  -662,  -662,  -661,
       0,     0,     0,     0,  -662,  -662,  -662,  -662,  -662,  -662,
    -662,     0,     0,  -662,     0,     0,     0,     0,     0,     0,
       0,     0,  -662,  -662,  -662,  -662,  -662,  -662,  -662,  -662,
    -662,     0,  -662,  -662,  -662,     0,     0,  -662,     0,     0,
    -662,  -662,     0,  -662,  -662,  -662,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -662,  -662,
       0,     0,     0,     0,     0,  -662,   869,     0,  -662,  -662,
       0,  -662,  -662,     0,  -662,     0,  -662,  -662,  -662,     0,
    -662,  -662,  -662,     0,  -662,  -662,  -662,     0,  -662,     0,
       0,     0,     0,     0,     0,  -112,  -663,  -663,  -663,  -663,
    -663,  -663,  -663,  -663,  -663,     0,     0,     0,  -662,  -662,
    -662,     0,  -663,     0,  -663,  -663,  -663,  -663,  -662,     0,
       0,     0,     0,  -663,  -663,  -663,  -663,  -663,  -663,  -663,
       0,     0,  -663,     0,     0,     0,     0,     0,     0,     0,
       0,  -663,  -663,  -663,  -663,  -663,  -663,  -663,  -663,  -663,
       0,  -663,  -663,  -663,     0,     0,  -663,     0,     0,  -663,
    -663,     0,  -663,  -663,  -663,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -663,  -663,     0,
       0,     0,     0,     0,  -663,     0,     0,  -663,  -663,     0,
    -663,  -663,     0,  -663,     0,  -663,  -663,  -663,     0,  -663,
    -663,  -663,     0,  -663,  -663,  -663,     0,  -663,     0,     0,
       0,     0,     0,     0,  -664,  -664,  -664,  -664,  -664,  -664,
    -664,  -664,  -664,     0,     0,     0,     0,  -663,  -663,  -663,
    -664,     0,  -664,  -664,  -664,  -664,     0,  -663,     0,     0,
       0,  -664,  -664,  -664,  -664,  -664,  -664,  -664,     0,     0,
    -664,     0,     0,     0,     0,     0,     0,     0,     0,  -664,
    -664,  -664,  -664,  -664,  -664,  -664,  -664,  -664,     0,  -664,
    -664,  -664,     0,     0,  -664,     0,     0,  -664,  -664,     0,
    -664,  -664,  -664,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -664,  -664,     0,     0,     0,
       0,     0,  -664,     0,     0,  -664,  -664,     0,  -664,  -664,
       0,  -664,     0,  -664,  -664,  -664,     0,  -664,  -664,  -664,
       0,  -664,  -664,  -664,     0,  -664,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -664,  -664,  -664,     0,     0,
       0,     0,     0,     0,     0,  -664,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
       0,     0,     0,   155,   156,   157,   232,   233,   234,   235,
     162,   163,   164,     0,     0,     0,     0,     0,   165,   166,
     167,   236,   237,   238,   239,   172,   319,   320,   240,   321,
       0,     0,     0,     0,     0,     0,   322,     0,     0,     0,
       0,     0,   323,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
     324,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
       0,     0,     0,     0,     0,     0,   203,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,     0,     0,     0,   155,   156,   157,   232,   233,   234,
     235,   162,   163,   164,     0,     0,     0,     0,     0,   165,
     166,   167,   236,   237,   238,   239,   172,   319,   320,   240,
     321,     0,     0,     0,     0,     0,     0,   322,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,   487,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,     0,     0,     0,     0,     0,     0,   203,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     153,   154,     0,     0,     0,   155,   156,   157,   232,   233,
     234,   235,   162,   163,   164,     0,     0,     0,     0,     0,
     165,   166,   167,   236,   237,   238,   239,   172,     0,     0,
     240,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,   241,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,     0,     0,     0,     0,     0,     0,   203,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,     0,     0,     0,   155,   156,   157,   232,
     233,   234,   235,   162,   163,   164,     0,     0,     0,     0,
       0,   165,   166,   167,   236,   237,   238,   239,   172,     0,
       0,   240,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,     0,     0,     0,     0,     0,     0,   203,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,     0,     0,     0,    14,     0,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   312,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,     0,     0,     0,     0,
      14,   120,   108,   109,    17,    18,     0,     0,     0,   313,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
     116,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   312,     0,     0,   119,    52,     0,    53,    54,
       0,     0,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,     0,     0,     0,    14,   120,   108,   109,    17,    18,
       0,     0,     0,   612,     0,   110,   111,   112,    22,   807,
     808,   809,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,   944,    34,    35,    36,
     811,    38,     0,    39,    40,    41,     0,     0,   812,     0,
       0,    43,    44,     0,   260,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,   945,     0,     0,   119,
      52,     0,   814,   815,     0,   946,     0,   262,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   818,     0,     0,     0,     0,     0,     0,    64,
     264,    66,     0,     0,     0,     0,   947,     4,     5,     6,
       7,     8,     9,    10,    11,    12,     0,     0,     0,     0,
       0,     0,     0,    14,     0,   108,   109,    17,    18,     0,
       0,     0,     0,     0,   110,   111,   112,    22,    23,    24,
      25,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,    32,   954,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,   260,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    52,
       0,    53,    54,     0,     0,     0,   955,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,     0,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,    64,   264,
      66,     0,    14,   956,    15,    16,    17,    18,     0,     0,
       0,     0,     0,    19,    20,    21,    22,    23,    24,    25,
       0,     0,    26,     0,     0,     0,     0,     0,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,    50,     0,     0,    51,    52,     0,
      53,    54,     0,    55,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,     0,     0,
       0,     0,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,    64,    65,    66,
      14,     0,    15,    16,    17,    18,     0,     0,     0,     0,
       0,    19,    20,    21,    22,    23,    24,    25,     0,     0,
      26,     0,     0,     0,     0,     0,    27,     0,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     0,     0,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,    64,    65,    66,    14,     0,
      15,    16,    17,    18,     0,     0,     0,     0,     0,    19,
      20,    21,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
     259,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,   260,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,   261,
       0,   262,   263,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,    64,   264,    66,    14,     0,    15,    16,
      17,    18,     0,     0,     0,     0,     0,    19,    20,    21,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,   259,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,   260,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,   509,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,   261,     0,   262,
     263,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     0,     0,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,    64,   264,    66,    14,     0,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,   259,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,   260,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      52,     0,    53,    54,     0,   726,     0,   262,   263,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,    64,
     264,    66,    14,     0,   108,   109,    17,    18,     0,     0,
       0,     0,     0,   110,   111,   112,    22,    23,    24,    25,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,    32,   259,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,   260,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,   886,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    52,     0,
      53,    54,     0,   726,     0,   262,   263,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,     0,     0,
       0,     0,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,     0,    64,   264,    66,
      14,     0,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,   259,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
     260,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    52,     0,    53,    54,
       0,   261,     0,   262,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     0,     0,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,    64,   264,    66,    14,     0,
     108,   109,    17,    18,     0,     0,     0,     0,     0,   110,
     111,   112,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
     259,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,   260,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,   262,   263,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,    64,   264,    66,    14,     0,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,   259,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,   260,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,   726,     0,   262,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     0,     0,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,    64,   264,    66,    14,     0,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,   259,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,   260,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      52,     0,    53,    54,     0,     0,     0,   262,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,    64,
     264,    66,    14,     0,    15,    16,    17,    18,     0,     0,
       0,     0,     0,    19,    20,    21,    22,    23,    24,    25,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    52,     0,
      53,    54,     0,   606,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,     0,     0,
       0,     0,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,     0,    64,   264,    66,
      14,     0,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    52,     0,    53,    54,
       0,   261,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     0,     0,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,    64,   264,    66,    14,     0,
     108,   109,    17,    18,     0,     0,     0,     0,     0,   110,
     111,   112,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,   606,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,    64,   264,    66,    14,     0,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,   932,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     0,     0,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,    64,   264,    66,    14,     0,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      52,     0,    53,    54,     0,   726,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,    64,
     264,    66,    14,     0,    15,    16,    17,    18,     0,     0,
       0,     0,     0,    19,    20,    21,    22,    23,    24,    25,
       0,     0,    26,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    52,     0,
      53,    54,     0,     0,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,     0,     0,
       0,     0,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,     0,    64,    65,    66,
      14,     0,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    52,     0,    53,    54,
       0,     0,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     0,     0,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,    64,   264,    66,    14,     0,
      15,    16,    17,    18,     0,     0,     0,     0,     0,    19,
      20,    21,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,    64,   264,    66,    14,     0,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,   807,   808,   809,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,   810,    34,
      35,    36,   811,    38,     0,    39,    40,    41,     0,     0,
     812,     0,     0,    43,    44,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   813,     0,
       0,   119,    52,     0,   814,   815,     0,   816,     0,   817,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,   818,     0,     0,     0,     0,     0,
      14,   120,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,   807,   808,   809,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,   966,    34,    35,    36,   811,    38,     0,    39,
      40,    41,     0,     0,   812,     0,     0,    43,    44,     0,
     116,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   813,     0,     0,   119,    52,     0,   814,   815,
       0,   816,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,   818,     0,
       0,     0,     0,     0,    14,   120,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,   224,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   225,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,     0,     0,     0,    14,   120,
     108,   109,    17,    18,     0,     0,     0,     0,     0,   110,
     111,   112,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,   116,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     312,     0,     0,   399,    52,     0,    53,    54,     0,   400,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,     0,
       0,     0,    14,   120,   108,   109,    17,    18,     0,     0,
       0,     0,     0,   110,   111,   112,    22,   807,   808,   809,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,    32,   966,    34,    35,    36,   811,    38,
       0,    39,    40,    41,     0,     0,   812,     0,     0,    43,
      44,     0,   116,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   813,     0,     0,   119,    52,     0,
     814,   815,     0,     0,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
     818,     0,     0,     0,     0,     0,    14,   120,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,   114,    34,
      35,    36,   115,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,     0,     0,     0,     0,
      14,   120,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
     116,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   312,     0,     0,   399,    52,     0,    53,    54,
       0,     0,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,     0,     0,     0,    14,   120,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,   116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1035,     0,     0,   119,
      52,     0,    53,    54,     0,     0,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,     0,     0,     0,    14,   120,
     108,   109,    17,    18,     0,     0,     0,     0,     0,   110,
     111,   112,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,   224,     0,
       0,     0,     0,  -658,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1083,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,   496,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,  -549,
    -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,
    -549,     0,     0,   120,     0,  -549,  -549,  -549,     0,  -549,
    -490,     0,     0,     0,     0,     0,     0,     0,  -549,     0,
    -658,     0,  -549,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -549,     0,     0,  -549,  -549,     0,  -549,  -549,
       0,  -549,  -549,  -490,  -549,  -549,  -549,  -549,  -549,  -549,
    -549,     0,     0,     0,     0,  -658,     0,  -549,  -490,  -490,
       0,  -658,     0,  -549,  -549,  -549,  -549,  -549,  -549,  -549,
    -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,     0,     0,
       0,     0,  -549,  -549,  -549,     0,  -549,     0,     0,     0,
       0,     0,     0,     0,     0,  -549,     0,  -658,     0,  -549,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -549,
       0,     0,  -549,  -549,     0,  -549,  -549,     0,  -549,  -549,
    -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,     0,     0,
       0,     0,  -658,     0,  -549,  -475,  -475,     0,  -658,     0,
    -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,  -549,
    -549,  -549,  -549,  -549,  -549,     0,     0,     0,     0,  -549,
    -549,  -549,     0,  -549,     0,     0,     0,     0,     0,     0,
       0,     0,  -549,     0,     0,     0,  -549,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -549,     0,     0,  -549,
    -549,     0,  -549,  -549,     0,  -549,  -549,  -549,  -549,  -549,
    -549,  -549,  -549,  -549,  -549,     0,     0,     0,     0,  -658,
    -487,  -549,     0,  -549,     0,  -658,     0,  -549,  -549,  -549,
     652,   653,     0,     0,   654,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   661,   662,     0,     0,
     663,     0,   203,   275,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,   682,   653,     0,     0,   683,     0,   203,   275,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   667,   662,
       0,     0,   668,     0,   203,   275,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   699,   653,     0,     0,   700,     0,
     203,   275,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
     702,   662,     0,     0,   703,     0,   203,   275,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   709,   653,     0,     0,
     710,     0,   203,   275,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,   712,   662,     0,     0,   713,     0,   203,   275,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   749,   653,
       0,     0,   750,     0,   203,   275,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   752,   662,     0,     0,   753,     0,
     203,   275,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
     937,   653,     0,     0,   938,     0,   203,   275,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   940,   662,     0,     0,
     941,     0,   203,   275,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,  1062,   653,     0,     0,  1063,     0,   203,   275,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   699,   653,
       0,     0,  1130,     0,   203,   275,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,  1062,   653,     0,     0,  1157,     0,
     203,   275,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
    1171,   653,     0,     0,  1172,     0,   203,   275,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,  1174,   662,     0,     0,
    1175,     0,   203,   275,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   904,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,     0,     0,     0,     0,     0,     0,   203,   275,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,   415,     0,     0,     0,     0,   416,   417,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,   917,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,     0,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,   404,   405,   406,   407,   408,   409,   410,   411,
     412,   413,   414,   415,     0,     0,     0,     0,   416,   417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   420,     0,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,  -305,  -305,  -305,  -305,  -305,  -305,
    -305,  -305,  -305,  -305,  -305,  -305,     0,     0,     0,     0,
    -305,  -305,  -305,     0,   869,  -305,     0,     0,     0,     0,
       0,     0,     0,  -305,     0,     0,     0,  -305,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -305,     0,     0,
    -305,  -305,     0,  -112,  -305,     0,  -305,  -305,  -305,  -305,
    -305,  -305,  -305,  -305,  -305,  -305,     0,     0,     0,     0,
       0,  -305,  -305,  -305,  -305,     0,  -662,     0,  -305,  -305,
    -305,  -306,  -306,  -306,  -306,  -306,  -306,  -306,  -306,  -306,
    -306,  -306,  -306,     0,     0,     0,     0,  -306,  -306,  -306,
       0,   526,  -306,     0,     0,     0,     0,     0,     0,     0,
    -306,     0,     0,     0,  -306,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -306,     0,     0,  -306,  -306,     0,
    -113,  -306,     0,  -306,  -306,  -306,  -306,  -306,  -306,  -306,
    -306,  -306,  -306,     0,     0,     0,     0,     0,  -306,  -306,
    -306,  -306,     0,     0,     0,  -306,  -306,  -306,  -558,  -558,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
       0,     0,     0,     0,  -558,  -558,  -558,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -558,     0,     0,
       0,  -558,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -558,     0,     0,  -558,  -558,     0,     0,  -558,     0,
    -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,  -558,
       0,     0,     0,     0,     0,  -488,  -558,     0,  -558,     0,
       0,     0,  -558,  -558,  -558,  -501,  -501,  -501,  -501,  -501,
    -501,  -501,  -501,  -501,  -501,  -501,  -501,     0,     0,     0,
       0,  -501,  -501,  -501,     0,     0,   974,     0,     0,     0,
       0,     0,     0,     0,  -501,     0,     0,     0,  -501,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -501,     0,  -501,  -501,  -501,
    -501,  -501,  -501,  -501,  -501,  -501,  -501,     0,     0,     0,
       0,     0,     0,  -501,     0,     0,     0,     0,     0,     0,
    -501,   404,   405,   406,   407,   408,   409,   410,   411,   412,
     413,   414,   415,     0,     0,     0,     0,   416,   417,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   420,     0,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,     0,     0,     0,     0,     0,     0,     0,
       0,  -282,   404,   405,   406,   407,   408,   409,   410,   411,
     412,   413,   414,   415,     0,     0,     0,     0,   416,   417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   420,     0,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,     0,     0,     0,     0,     0,     0,
       0,     0,  -283,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,   415,     0,     0,     0,     0,   416,
     417,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   420,     0,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,     0,     0,     0,     0,     0,
       0,     0,     0,  -285,   404,   405,   406,   407,   408,   409,
     410,   411,   412,   413,   414,   415,     0,     0,     0,     0,
     416,   417,     0,     0,     0,   501,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   420,     0,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   404,   405,   406,   407,
     408,   409,   410,   411,   412,   413,   414,   415,     0,     0,
       0,     0,   416,   417,   404,   405,   406,   407,   408,   409,
       0,     0,   412,   413,     0,   419,     0,     0,     0,     0,
     416,   417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,   420,     0,   421,   422,
     423,   424,   425,   426,   427,   428,   429,   430,     0,     0,
       0,     0,     0,     0,     0,     0,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430
};

static const yytype_int16 yycheck[] =
{
       1,    27,   284,    26,   378,    88,     8,     9,   271,     6,
      21,   481,    14,   402,   433,   594,   490,    13,   221,    20,
      68,    81,    86,    87,     9,    65,   313,     3,   792,    14,
     118,    27,    15,    16,    15,    16,    19,    55,    19,   458,
     402,     1,   387,     3,   389,   597,   443,   444,   764,   547,
    1018,   331,    53,    54,   443,   444,   475,   504,   639,   306,
      57,   508,    25,   310,    51,   484,   670,   671,   508,   815,
      53,   814,    73,    74,    25,     6,  1043,     1,   821,     3,
       4,     5,    13,   973,     8,     9,   594,    26,    12,   597,
      14,    15,    16,   104,   594,    19,    27,     4,     5,   539,
      74,    74,    14,   295,   103,    12,    26,   373,    57,   431,
     111,     0,    25,   435,   612,   117,   438,    25,    15,    16,
      57,   138,    19,   318,    20,    21,    57,    51,   394,   128,
     549,    55,    93,    16,   144,   693,   717,   459,    25,   697,
     269,    65,   271,    92,   215,    55,   704,    25,    55,   122,
     221,   299,   474,   217,   476,   303,    68,    81,    59,    60,
      61,    62,    90,   485,   228,   126,    25,    28,    25,    90,
      79,  1061,   121,    90,    81,   370,   305,   756,  1056,   142,
     121,   144,    29,  1151,   121,    15,    16,   258,   105,    19,
     769,   142,  1159,   117,   460,   119,    18,   332,    20,   138,
     335,    16,   337,   525,   339,   144,   341,   399,   104,   142,
      90,    92,   213,   214,    16,   111,   112,   126,   138,   147,
     222,   223,   121,    53,    54,   289,   147,   973,   550,   142,
     147,   144,   115,   129,   142,   118,   119,   222,   223,   213,
     214,   214,    20,   142,   241,    18,   850,    25,   756,   297,
     513,   315,    25,   313,   121,   142,   756,   305,   306,   142,
     547,   769,   310,   146,   142,   148,   113,   147,   269,   769,
     271,    90,    90,   144,   275,   125,  1040,   348,   558,  1043,
      26,   283,   284,   142,   792,   142,   210,    55,   735,   736,
    1168,   362,   275,   121,   275,   278,   736,   278,   222,   223,
     115,    90,   800,   118,   119,   101,    92,  1023,   295,   380,
     241,    57,  1055,   115,    55,  1061,   118,   119,   839,    90,
     666,    25,   901,   669,   845,   612,    58,    59,   147,   147,
     692,   146,   911,   148,   294,   131,   132,   133,   142,   251,
      90,   687,   302,    90,   146,    20,   148,   349,   350,   351,
     352,   275,   354,   355,   278,   907,   551,   397,   147,   283,
     284,   509,   402,   287,   349,   350,   351,   352,   516,   457,
     294,   295,   348,    34,    57,   376,   147,   378,   302,    92,
     528,   278,   400,   512,   513,   297,   805,    90,   348,   313,
      51,    92,   138,   353,   841,  1159,   138,   147,   144,   907,
     147,    90,   776,   443,   444,   142,   313,   877,   121,   480,
     481,   101,   399,   802,   397,   681,   105,    51,    51,   402,
     121,   556,    55,   324,   348,   349,   350,   351,   352,   353,
     354,   355,   433,   581,   582,    92,   781,   782,   783,   836,
     785,   142,   787,   725,   147,   842,   843,   836,   278,   373,
     145,   140,   433,   842,   843,   144,  1014,   458,   147,   461,
     531,  1040,   121,   611,   121,   466,   729,   538,   102,    92,
     394,   141,   106,   397,   475,   399,   400,   458,   402,   502,
     609,   139,   804,   484,   806,   545,    60,   547,    61,    63,
      55,    64,    65,   400,   475,   521,   142,   115,   121,   746,
     118,   119,   115,   484,   435,   118,   119,   438,   101,   433,
     493,   512,   513,   800,    37,    38,   101,   720,    57,   443,
     444,   522,   121,    26,    51,   521,   121,   793,   459,    51,
     148,   660,  1040,   107,   458,  1043,   460,   461,   142,   435,
    1014,   689,   121,   116,   117,   476,    92,   471,   549,   923,
     924,   475,   612,   835,   485,   479,    58,    59,   622,    92,
     484,   142,   522,   459,   142,   489,    92,  1148,   549,  1016,
     142,   531,    92,   142,  1153,   121,  1016,    15,    16,    51,
     476,    19,   848,    92,  1163,   982,  1144,    90,   121,   485,
     521,   720,   119,   982,   525,   121,   142,   745,   522,   511,
     729,   121,   105,   584,   605,    17,    18,   531,   679,   142,
      92,   893,   121,    92,    92,    53,    54,   765,   641,   550,
      51,   545,   603,   547,   887,   549,   142,    92,   894,   525,
      90,    40,    41,   557,    16,   138,    99,   140,   545,   121,
     547,   144,   121,   121,   147,   105,   657,    13,    16,   720,
      15,  1159,    90,   121,   550,   666,   121,   121,   669,   660,
     584,    90,   587,   142,   142,   666,   591,    16,   669,   670,
     671,    63,    40,    41,   504,    15,   105,   880,   680,   603,
     140,    92,  1027,  1028,  1029,  1030,   687,   147,   612,   715,
     145,   692,   693,   690,    92,   680,   697,   145,   746,   142,
     624,    92,   139,   704,    15,   612,   536,   778,   142,   692,
     121,   140,   714,    92,   142,   639,    15,   624,   147,   715,
     919,   920,  1141,   121,    27,    44,    90,   993,   729,   714,
     121,   142,   639,   115,   121,   122,   118,   119,   886,   142,
     800,   105,   121,    26,   121,   115,  1120,   115,   118,   119,
     118,   119,   141,   141,   902,    15,   680,   681,   887,   690,
     287,   657,   802,   846,   146,    18,   148,    57,   295,   141,
     666,    15,   141,   669,    57,   776,   140,   587,   146,   139,
     148,   852,   139,   147,   715,   141,    26,    73,    74,   685,
     714,   687,   139,   717,   997,    44,   836,   837,   142,    57,
    1003,   142,   842,   843,   805,   142,   877,    90,   142,   880,
     717,  1156,   115,   814,   815,   118,   119,   142,    44,    55,
    1142,    51,   105,    51,   805,   115,   142,   142,   118,   119,
     142,   814,    15,   123,    93,   121,   122,   275,    90,    90,
     278,   801,    14,   146,    15,   148,    15,   145,   142,   850,
      90,   142,   141,   105,   105,   138,    15,   140,   148,   860,
     142,   144,   863,   865,   147,   105,   142,   792,   142,   793,
    1152,    55,   399,   141,   139,   806,   800,   801,   802,    90,
     865,   805,    26,    51,   948,   142,   887,   142,   140,   140,
     814,  1094,  1095,   800,   105,   147,   147,   821,   138,    61,
     140,   318,    64,    65,   144,   735,    15,   147,   142,   142,
     806,   142,   836,   837,   821,    63,    64,    65,   842,   843,
     139,    15,   923,   924,   848,   849,   997,   213,   214,   140,
      15,    15,  1003,   139,   142,   126,   147,   126,   862,    55,
     144,   865,   982,   142,   471,    57,    90,    93,    90,   814,
     874,   875,   479,   370,   116,   117,   821,   139,   101,   883,
      90,   105,   489,   105,    15,     1,    55,     3,   116,   117,
     894,   895,     8,     9,    15,   105,   142,   142,    14,    15,
      16,   142,   792,    19,   814,   815,   129,   130,   131,   132,
     133,  1194,    26,   142,   138,   433,   140,   921,   140,    15,
     144,   142,   926,   147,   142,   147,  1209,  1210,  1010,  1011,
     140,   841,   144,  1014,   921,    51,   142,   147,   942,   926,
     458,  1085,   142,  1094,  1095,  1010,  1011,   142,   141,    65,
     557,   142,   318,    15,    40,   942,    15,   475,    26,    41,
      90,   522,    90,   792,    12,     5,   484,   971,   972,   115,
     974,  1144,   118,   119,   792,   105,    90,   105,   982,    40,
      41,    42,    43,    44,   971,   972,   504,   974,   992,   993,
     508,   105,   996,    63,    64,    65,  1000,   942,  1101,    90,
     146,   117,   148,   119,   370,  1151,  1010,  1011,  1012,   996,
     140,   847,   140,  1065,   105,  1096,   971,   147,   536,   147,
     815,   539,    90,   253,   138,  1143,   140,   972,     6,   974,
     144,   549,  1037,   147,   587,  1040,    -1,   105,  1043,  1120,
    1045,  1037,  1040,  1194,    -1,    -1,   116,   117,    -1,   140,
      -1,  1055,  1056,  1057,   551,    -1,   147,    -1,  1209,  1210,
    1141,  1065,  1143,  1144,    63,    64,    65,    -1,  1055,  1056,
     138,     1,   140,     3,     4,     5,   144,    -1,  1065,   147,
    1141,    -1,    12,   580,  1088,    -1,  1090,    -1,    -1,  1093,
      -1,    -1,    -1,    -1,   210,    37,    38,    -1,  1179,  1180,
     597,    -1,    -1,   600,   624,    -1,   222,   223,    26,    -1,
    1055,  1056,  1057,    90,    -1,    -1,  1121,   116,   117,   639,
    1065,    51,    -1,  1205,  1206,    55,    -1,  1208,   105,    -1,
      -1,  1142,  1136,    90,  1215,  1216,    -1,  1141,    -1,    -1,
    1205,  1206,  1146,  1147,  1148,    -1,    -1,  1037,   105,  1136,
    1040,    81,    -1,  1043,  1159,  1045,  1161,    -1,  1163,   275,
    1165,  1148,   278,   140,  1168,  1169,  1142,   283,   284,    -1,
     147,   287,    90,   115,    -1,    -1,   118,   119,   294,   295,
      -1,  1168,  1169,   140,    -1,   551,   302,   105,    -1,   119,
     147,  1136,    -1,  1198,    -1,    51,    62,   717,    64,    65,
    1204,  1205,  1206,    -1,   146,    -1,   148,    -1,  1037,  1213,
    1214,  1040,    -1,    69,  1043,    -1,  1045,   735,   736,    -1,
     138,    -1,   140,  1168,  1169,    -1,   443,   444,    -1,   147,
      -1,  1121,   348,   349,   350,   351,   352,   353,   354,   355,
      -1,    90,   849,    -1,    -1,   101,   102,   103,   928,   929,
     116,   117,    -1,    -1,    -1,   862,   105,   373,    -1,    -1,
     477,   478,    90,    -1,    -1,    -1,    -1,   874,   875,  1159,
      -1,  1161,   128,  1163,    90,  1165,   883,   105,   394,    -1,
     210,   397,    -1,   399,    -1,    -1,   402,   805,   895,   105,
      -1,   140,  1121,    -1,   814,   792,   814,   815,   147,    -1,
      51,   821,    53,    54,    55,    56,    -1,    -1,  1198,   526,
      -1,    51,   140,    53,    54,    55,    56,   433,    69,   147,
      -1,    -1,    -1,   841,   140,    -1,    -1,   443,   444,    69,
    1159,   147,  1161,    -1,  1163,    62,  1165,    64,    65,    -1,
      -1,    -1,   458,    -1,   460,   461,    -1,    -1,    -1,    51,
      -1,    53,    54,    55,    56,   471,    -1,   287,    -1,   475,
    1040,    -1,    -1,   479,   294,   295,    -1,    69,   484,  1198,
      -1,    -1,   302,   489,    63,    64,    65,    63,    64,    65,
      -1,   624,    -1,   313,    -1,   992,    63,    64,    65,   116,
     117,   142,    94,  1000,    -1,    -1,   639,    -1,   100,   101,
     102,   103,   142,    -1,    -1,  1012,   522,    63,    64,    65,
     907,    -1,   909,    -1,    -1,   531,   913,    -1,   348,    -1,
      -1,    -1,   942,   353,    -1,    -1,   128,   116,   117,   131,
     116,   117,    -1,   549,    -1,    63,    64,    65,    -1,   116,
     117,   557,   144,   373,  1124,  1125,  1126,    -1,  1128,  1129,
      -1,   971,   972,    51,   974,    53,    54,    55,    56,    -1,
     116,   117,    -1,    -1,   394,    -1,    -1,    -1,   584,   399,
     400,    69,   402,    51,   717,    53,    54,    55,    56,    -1,
      -1,  1088,    -1,  1090,    -1,    -1,  1093,   603,   116,   117,
      -1,    69,  1179,  1180,    -1,    -1,    94,    -1,  1016,    -1,
      51,    -1,    53,    54,    55,    56,  1186,  1187,  1188,  1189,
      -1,    -1,    -1,   443,   444,    -1,    94,    -1,    69,    -1,
      -1,  1208,   100,    -1,  1021,  1022,    -1,  1207,  1215,  1216,
     460,    -1,    -1,    -1,    -1,  1055,  1056,  1057,    -1,  1146,
    1147,   471,    -1,    94,    -1,  1065,  1043,    -1,  1045,   479,
     101,   102,   103,    -1,    -1,    -1,    -1,   115,    -1,   489,
     118,   119,    -1,   115,   680,   681,   118,   119,    -1,    -1,
      -1,   814,   815,    -1,    -1,    -1,    -1,   128,   821,    -1,
      -1,    51,    -1,    53,    54,    55,    56,   145,   146,    -1,
     148,    -1,   522,    -1,   146,    -1,   148,  1204,   714,    69,
      51,   531,    53,    54,    55,    56,  1213,  1214,    -1,    -1,
      -1,  1108,    -1,    -1,  1111,   545,  1136,   547,    69,   836,
     837,    -1,    -1,  1141,    94,   842,   843,   557,  1148,    -1,
     100,    88,    89,    -1,     1,    -1,     3,     4,     5,    -1,
      -1,    -1,    -1,    -1,   101,    12,    -1,    -1,  1168,  1169,
      -1,   868,   869,    -1,   871,   872,    -1,    -1,  1155,    -1,
      -1,    -1,  1159,    -1,  1161,    -1,    -1,    -1,  1165,   126,
     127,   128,   129,   130,   131,   132,   133,   793,    -1,    -1,
      -1,    -1,   612,    -1,    51,   801,   802,    -1,    55,   805,
      -1,    -1,    -1,    51,   624,    53,    54,    55,    56,   942,
      -1,  1198,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   639,
      -1,    69,    -1,    -1,    81,    -1,    -1,    -1,    -1,    -1,
     836,   837,    -1,    -1,    -1,    83,   842,   843,   971,   972,
     973,   974,   848,   849,    -1,   115,    94,    -1,   118,   119,
      88,    89,   100,   101,   102,   103,   862,    -1,    -1,   865,
      -1,   681,   119,   101,    -1,    -1,    -1,    -1,   874,   875,
      -1,    -1,   142,   121,    44,   982,   146,   883,   148,    -1,
     128,    -1,    -1,   131,    -1,    -1,    -1,    -1,   894,   895,
     128,   129,   130,   131,   132,   133,   144,   717,  1005,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      -1,    -1,  1055,  1056,  1057,    88,    89,    -1,  1061,    -1,
      -1,   101,  1065,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   210,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,   129,   130,   131,   132,
     133,    -1,   142,   793,    -1,    -1,   982,    -1,    -1,    -1,
     800,   801,   802,    -1,    -1,    -1,   992,   993,    -1,    -1,
      -1,    -1,    -1,    -1,  1000,    -1,    -1,    -1,    -1,    -1,
      -1,   821,    -1,  1136,  1010,  1011,  1012,    -1,    -1,    51,
      -1,    53,    54,    55,    56,  1148,   836,    -1,    -1,    -1,
      -1,    -1,   842,   843,    -1,    -1,    -1,    69,   848,   849,
     287,    -1,    -1,    -1,    -1,  1168,  1169,   294,   295,    -1,
      -1,    83,   862,    -1,    -1,   302,    -1,    -1,    -1,    -1,
      -1,    -1,    94,    -1,   874,   875,   313,    -1,   100,   101,
     102,   103,    -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   894,   895,    51,    -1,    53,    54,
      55,    56,  1088,    -1,  1090,    -1,   128,  1093,    -1,   131,
      -1,   348,    -1,    -1,    69,    -1,   353,    -1,    -1,    -1,
      -1,   921,   144,    -1,    -1,    -1,   926,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   373,    -1,    -1,    94,
      -1,    -1,   942,    -1,    -1,   100,   101,   102,   103,    -1,
      -1,    -1,    -1,    -1,    -1,  1141,    -1,   394,    -1,    -1,
    1146,  1147,   399,   400,    -1,   402,    -1,    -1,    -1,    -1,
      -1,   971,   972,   128,   974,    -1,   131,    -1,    -1,    -1,
      -1,    -1,   982,    -1,    -1,    -1,    51,   142,    53,    54,
      55,    56,   992,   993,    -1,    -1,   996,    -1,    -1,    -1,
    1000,    -1,    -1,    -1,    69,    -1,   443,   444,    -1,    -1,
      -1,    -1,  1012,    -1,    -1,    -1,    -1,    -1,  1204,  1205,
    1206,    -1,     1,   460,     3,     4,     5,  1213,  1214,    94,
      -1,    -1,    -1,    12,   471,   100,   101,   102,   103,    -1,
      -1,    -1,   479,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   489,    -1,    -1,  1055,  1056,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,  1065,   131,    -1,    -1,    -1,
      -1,    -1,    51,    -1,    -1,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   522,    -1,    -1,  1088,    -1,
    1090,    -1,    -1,  1093,   531,    -1,    -1,    -1,    -1,    -1,
      -1,     0,    81,    -1,    -1,    -1,    -1,    -1,   545,    -1,
     547,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
     557,    20,    -1,    -1,    -1,    -1,    25,    -1,    27,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1136,    -1,    37,    38,
     119,    40,    41,    42,    43,    44,  1146,  1147,  1148,    51,
      -1,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,  1168,  1169,
      -1,    -1,    -1,    -1,    -1,   612,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    -1,    -1,    -1,   624,    -1,    -1,
      -1,    90,    94,    -1,    93,    -1,    -1,    -1,   100,   101,
     102,   103,   639,    -1,  1204,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,  1213,  1214,    -1,   115,    -1,    -1,   118,
     119,    -1,    -1,    -1,    -1,    -1,   128,   126,    -1,   131,
      -1,   210,    51,    -1,    53,    54,    55,    56,    -1,    -1,
     139,   140,    -1,   142,   681,    -1,   145,   146,   147,   148,
      69,    -1,    -1,    51,    -1,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    -1,    -1,    -1,
      -1,    69,    -1,    -1,    -1,    94,    -1,    -1,    -1,    -1,
     717,   100,   101,   102,   103,    -1,    -1,    85,    -1,    -1,
       1,    -1,     3,     4,     5,     6,    94,    -1,    -1,    -1,
      -1,    12,   100,   101,   102,   103,    -1,    -1,   287,   128,
      -1,    -1,   131,    -1,    -1,   294,   295,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,   131,   313,    -1,    -1,    -1,    -1,    -1,
      51,    -1,    -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,    -1,    -1,
      -1,    -1,    -1,   800,   801,   802,    -1,    -1,    -1,   348,
      81,    -1,    -1,    -1,   353,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   821,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   373,    -1,    -1,    -1,    -1,   836,
      -1,    -1,    -1,    -1,    -1,   842,   843,    -1,   119,    -1,
      -1,   848,   849,    -1,    -1,   394,    -1,    -1,    -1,    -1,
     399,   400,    -1,   402,    -1,   862,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,     3,    -1,    -1,   874,   875,    -1,
      -1,    44,    -1,    12,    -1,    -1,   883,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   894,   895,    -1,
      -1,    -1,    -1,    -1,   443,   444,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,   460,    51,    -1,   921,    88,    89,    -1,    -1,   926,
      -1,    -1,   471,    -1,    -1,    -1,    -1,    -1,   101,   210,
     479,    -1,    -1,    -1,    -1,   942,    -1,    -1,    -1,    -1,
     489,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,    -1,   971,   972,    -1,   974,    -1,    -1,
      -1,    -1,    -1,   522,    -1,   982,    -1,    -1,    -1,    -1,
     119,    -1,   531,    -1,    -1,   992,   993,    -1,    -1,   996,
      -1,    -1,    -1,  1000,    -1,    -1,   545,    -1,   547,    -1,
      -1,    -1,    -1,    -1,    -1,  1012,   287,    -1,   557,    -1,
      -1,    -1,    -1,   294,   295,    -1,    -1,    -1,    -1,    -1,
      -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1055,  1056,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1065,    -1,
      -1,    -1,    -1,   612,    -1,    -1,    -1,   348,    -1,    -1,
      -1,   210,   353,    -1,    -1,   624,    -1,    -1,    -1,    -1,
      -1,  1088,    -1,  1090,     0,    -1,  1093,    -1,    -1,    -1,
     639,    -1,   373,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    25,
      26,    27,    -1,   394,    -1,    -1,    -1,    -1,   399,   400,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,  1136,
      -1,    -1,   681,    -1,    -1,    -1,    -1,    -1,    -1,  1146,
    1147,  1148,    -1,    -1,    -1,    -1,    -1,    -1,   287,    -1,
      -1,    -1,    -1,    -1,    -1,   294,   295,    -1,    -1,    -1,
      -1,  1168,  1169,   302,    -1,    -1,    -1,    -1,   717,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    -1,    -1,   460,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
     471,    -1,    -1,    -1,    -1,    -1,    -1,  1204,   479,   115,
      -1,    -1,   118,   119,    -1,    -1,  1213,  1214,   489,   348,
      -1,    -1,    -1,    -1,   353,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   138,   139,    -1,    -1,    -1,    -1,   144,   145,
     146,   147,   148,    -1,   373,    -1,     1,    -1,     3,    -1,
      -1,   522,    -1,    -1,   793,    -1,    -1,    -1,    -1,    -1,
     531,   800,   801,   802,    -1,   394,    -1,    -1,    -1,    -1,
     399,    -1,    -1,   402,   545,    -1,   547,    -1,    -1,    -1,
      -1,    -1,   821,    -1,    -1,    -1,   557,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,   836,    -1,    -1,
      -1,    -1,    -1,   842,   843,    -1,    -1,    -1,    -1,   848,
     849,    -1,    -1,    -1,   443,   444,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   862,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   460,    -1,    -1,    -1,   874,   875,    -1,    -1,    -1,
      -1,   612,   471,    -1,   883,    -1,    -1,    -1,    -1,    -1,
     479,    -1,    -1,   624,    -1,   894,   895,    -1,    -1,    -1,
     489,    -1,    -1,    -1,   119,    -1,    -1,    -1,   639,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   921,    -1,    -1,    -1,    -1,   926,    -1,    -1,
      -1,    -1,    -1,   522,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   531,   942,    -1,    -1,    -1,    -1,    -1,    -1,
     681,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,   557,    -1,
      80,    81,   971,   972,    -1,   974,    -1,    -1,    88,    89,
      -1,    -1,    -1,   982,    -1,    -1,   717,    -1,    -1,    -1,
      -1,   101,    -1,   992,   993,   210,    -1,   996,    -1,    -1,
      -1,  1000,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1012,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,  1055,  1056,    -1,    -1,
      88,    89,   793,    -1,    -1,    -1,  1065,    -1,    -1,   800,
     801,    -1,   287,   101,    -1,    -1,    -1,    -1,    -1,   294,
     295,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,  1088,
     821,  1090,   681,    -1,  1093,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   848,   849,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   862,    -1,   348,    -1,    -1,    -1,  1136,   353,    -1,
      -1,    -1,    -1,   874,   875,    -1,    -1,  1146,  1147,  1148,
      -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,   373,    -1,
      -1,    -1,    -1,   894,   895,    -1,    -1,    -1,    -1,  1168,
    1169,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   394,
      -1,    -1,    -1,    -1,   399,    -1,    -1,   402,    -1,    -1,
     921,    -1,    -1,    -1,    -1,   926,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   793,  1204,    -1,    -1,    -1,    -1,
      -1,   942,   801,   802,  1213,  1214,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   443,   444,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     971,   972,    -1,   974,    -1,   460,    -1,   836,    -1,    -1,
      -1,    -1,    -1,   842,   843,    -1,   471,    -1,    -1,   848,
     849,   992,   993,    -1,   479,   996,    -1,    -1,    -1,  1000,
      -1,    -1,    -1,   862,   489,    -1,    -1,    -1,    -1,    -1,
      -1,  1012,    -1,    -1,    -1,   874,   875,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   894,   895,   522,    -1,    -1,
      -1,    -1,    -1,    15,    16,    -1,   531,    19,    -1,    -1,
      -1,    -1,    -1,    -1,  1055,  1056,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1065,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    46,    47,    48,    49,    -1,    -1,
      -1,    53,    54,    -1,    -1,    -1,    -1,  1088,    -1,  1090,
      -1,    -1,  1093,    65,    66,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      -1,    -1,    -1,   982,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,   992,   993,  1136,    -1,   996,    -1,    -1,
      -1,  1000,    -1,    -1,    -1,  1146,  1147,  1148,    -1,    -1,
      -1,    -1,   122,  1012,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,  1168,  1169,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   681,    -1,    -1,    -1,
      -1,    -1,    -1,  1204,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1213,  1214,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,  1088,
      -1,  1090,    -1,    -1,  1093,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    -1,  1146,  1147,   261,
     262,   263,   264,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   275,    88,    -1,   278,    91,   793,    -1,
      94,    95,    -1,    97,    98,    -1,   801,   802,    -1,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1204,    -1,    -1,    -1,    -1,
     134,   836,    -1,    -1,  1213,  1214,    -1,   842,   843,    -1,
      -1,    -1,    -1,   848,   849,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   862,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   874,
     875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,    -1,
      -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,    -1,   894,
     895,    -1,    -1,   385,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   397,    -1,    -1,    -1,    -1,
     402,    -1,   404,   405,   406,   407,   408,   409,   410,   411,
     412,   413,   414,   415,   416,   417,    -1,   419,   420,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,    -1,
      -1,   433,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   443,   444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   458,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   982,    -1,    -1,
      -1,   473,    -1,   475,    -1,   477,   478,   992,   993,    -1,
      -1,    -1,   484,    -1,    -1,  1000,    -1,    -1,    -1,    -1,
      -1,   493,    -1,    -1,   496,   497,    -1,  1012,    -1,   501,
      -1,    -1,   504,    -1,   506,    -1,   508,   509,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   526,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   536,    -1,    -1,   539,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   549,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    -1,    80,    81,   566,   567,    -1,    -1,    -1,    -1,
      88,    89,    -1,  1088,    -1,  1090,    -1,    -1,  1093,    -1,
      -1,    -1,   584,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   603,    -1,    -1,   606,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       0,  1146,  1147,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    25,    -1,    27,    28,    29,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,  1204,
     692,    -1,    -1,   695,   696,    -1,    -1,    -1,  1213,  1214,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      90,    -1,    -1,    93,   726,    -1,    -1,    -1,    -1,    99,
      -1,   101,    -1,   735,   736,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,   139,
     140,   141,   142,    -1,    -1,   145,   146,   147,   148,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   797,    -1,    -1,    -1,    -1,
     802,   803,    -1,   805,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   814,   815,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   836,   837,    -1,    -1,    -1,   841,
     842,   843,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   868,   869,    -1,   871,
     872,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     882,    -1,    -1,    -1,   886,    -1,    -1,    51,    52,    -1,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   903,   904,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,   916,   917,    80,    81,    -1,    -1,
      -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
     932,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,
      -1,    -1,    -1,    -1,   946,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   955,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,   975,   976,    -1,    -1,    -1,    -1,   143,
     982,    -1,    -1,    -1,    -1,    -1,    -1,     0,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    -1,  1005,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,  1016,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,  1057,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
       0,   114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   134,   135,   136,    -1,    25,    26,    27,    28,    -1,
      -1,    -1,    -1,   146,    -1,   148,    -1,    37,    38,  1141,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      90,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    99,
      -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,   138,   139,
     140,   141,   142,    -1,   144,   145,   146,   147,   148,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    25,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    90,    -1,    92,    93,
      -1,    -1,    -1,    -1,    -1,    99,    -1,   101,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,   121,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,     0,    -1,    -1,   139,   140,   141,   142,    -1,
      -1,   145,   146,   147,   148,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    25,    26,    27,
      28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    -1,    93,    -1,    -1,    -1,    -1,
      -1,    99,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,     0,    -1,
     138,   139,   140,   141,   142,    -1,   144,   145,   146,   147,
     148,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    25,    26,    27,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,
      92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,     0,    -1,   138,   139,   140,   141,
     142,    -1,   144,   145,   146,   147,   148,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    25,
      26,    27,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    90,    -1,    92,    93,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,   121,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
       0,    -1,   138,   139,   140,   141,   142,    -1,   144,   145,
     146,   147,   148,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    25,    -1,    27,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,
      60,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      90,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,   139,
     140,   141,   142,    -1,    -1,   145,   146,   147,   148,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    25,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    90,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    -1,    99,    -1,   101,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,     0,    -1,    -1,   139,   140,   141,   142,    -1,
     144,   145,   146,   147,   148,    13,    14,    15,    -1,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    25,    26,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    92,    93,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    -1,   121,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,     0,    -1,
     138,   139,   140,    -1,   142,    -1,    -1,   145,   146,   147,
     148,    13,    14,    15,    -1,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    25,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,
      92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,     0,    -1,   138,   139,   140,    -1,
     142,    -1,    -1,   145,   146,   147,   148,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    25,
      -1,    27,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    90,    -1,    -1,    93,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,    -1,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
       0,    -1,    -1,   139,   140,   141,   142,    -1,    -1,   145,
     146,   147,   148,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    25,    -1,    27,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      90,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,   139,
     140,   141,   142,    -1,    -1,   145,   146,   147,   148,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    25,    -1,    27,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    90,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,     0,    -1,    -1,   139,   140,   141,   142,    -1,
      -1,   145,   146,   147,   148,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    25,    -1,    27,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    -1,    93,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,     0,    -1,
      -1,   139,   140,   141,   142,    -1,    -1,   145,   146,   147,
     148,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    25,    -1,    27,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,
      -1,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,     0,    -1,    -1,   139,   140,   141,
     142,    -1,    -1,   145,   146,   147,   148,    13,    14,    15,
      -1,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    25,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    90,    -1,    92,    93,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,   121,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
       0,    -1,    -1,   139,   140,    -1,   142,    -1,    -1,   145,
     146,   147,   148,    13,    14,    15,    -1,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      90,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,
      -1,   121,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,   139,
     140,    -1,   142,    -1,    -1,   145,   146,   147,   148,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    25,    -1,    27,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,    -1,    -1,    -1,   139,    -1,    -1,    -1,    -1,
      -1,   145,   146,     1,   148,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    -1,    -1,
      18,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,   115,    -1,    -1,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,     1,
     148,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    -1,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   146,     1,   148,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      -1,    -1,    18,    19,    20,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,   115,
      -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,     1,   148,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    -1,    -1,    18,    19,
      -1,    21,    22,    23,    24,    25,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,   115,    -1,    -1,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   146,     1,   148,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    -1,    -1,    18,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,   115,    -1,    -1,   118,   119,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
     134,   135,   136,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,   146,    -1,   148,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
     115,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
     135,   136,    -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,
      -1,   146,     1,   148,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    14,    15,    -1,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,   115,    -1,    -1,   118,
     119,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,   134,   135,   136,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,   146,    -1,   148,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,   115,    -1,    -1,   118,   119,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,   134,   135,   136,    -1,    19,    -1,
      21,    22,    23,    24,    -1,   145,   146,    -1,   148,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,   115,    -1,    -1,   118,   119,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,
      22,    23,    24,    -1,   145,   146,    -1,   148,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,   135,   136,    -1,    -1,   139,    -1,    -1,
      -1,    -1,    -1,    -1,   146,     1,   148,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    15,
      -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,   115,
      -1,    -1,   118,   119,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,   134,   135,
     136,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
     146,    -1,   148,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,   115,    -1,
      -1,   118,   119,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,   134,   135,   136,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,   146,
      -1,   148,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,   115,    -1,    -1,
     118,   119,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,   134,   135,   136,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,   146,    -1,
     148,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,   115,    -1,    -1,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   127,    -1,
      -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,
     139,    -1,    -1,   142,    -1,    -1,    -1,   146,    -1,   148,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,   135,   136,    -1,    -1,    -1,    -1,   141,   142,
      -1,    -1,    -1,   146,    -1,   148,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,   115,    -1,
      -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,
      -1,    -1,   139,    -1,    -1,   142,    -1,    -1,    -1,   146,
      -1,   148,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,   146,    -1,   148,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,   146,    -1,   148,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   148,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      -1,    -1,    -1,    -1,    -1,    -1,   143,   144,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,   102,   103,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
     135,   136,    -1,   138,    -1,    -1,    -1,    -1,    -1,   144,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,   102,
     103,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   134,   135,   136,    19,   138,    21,    22,    23,    24,
      -1,   144,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,   102,   103,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,   121,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,   134,
     135,   136,    -1,    19,    -1,    21,    22,    23,    24,   144,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    92,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,   102,   103,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,   121,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,   134,   135,
     136,    -1,    19,    -1,    21,    22,    23,    24,   144,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,   102,   103,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,   144,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,   102,   103,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,
      -1,    -1,    69,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,
     107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      -1,    -1,    -1,    -1,    -1,    -1,   143,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    -1,    -1,    -1,    -1,    -1,    -1,   143,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    -1,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,
      -1,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
      -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,   134,    21,    22,    23,    24,    -1,    -1,    -1,   142,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   134,    21,    22,    23,    24,
      -1,    -1,    -1,   142,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,   102,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   127,    -1,    -1,    -1,    -1,    -1,    -1,   134,
     135,   136,    -1,    -1,    -1,    -1,   141,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,    -1,    -1,   102,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,   134,   135,
     136,    -1,    19,   139,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,
      -1,   102,   103,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,   102,
     103,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   134,   135,   136,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,   102,   103,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,
     135,   136,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,   102,   103,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,   102,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,
      -1,   102,   103,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,   102,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   134,   135,   136,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,    -1,    -1,   102,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,
     135,   136,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   134,   135,   136,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,
     135,   136,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,   102,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,   127,    -1,    -1,    -1,    -1,    -1,
      19,   134,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,   127,    -1,
      -1,    -1,    -1,    -1,    19,   134,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   134,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
     127,    -1,    -1,    -1,    -1,    -1,    19,   134,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,   134,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   134,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,
      -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,
      -1,    -1,    -1,   104,    57,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,   134,    -1,    88,    89,    90,    -1,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      26,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,   122,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,    -1,    -1,   138,    -1,   140,   141,   142,
      -1,   144,    -1,   146,   147,   148,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    90,    -1,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    26,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,   121,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
      -1,    -1,   138,    -1,   140,   141,   142,    -1,   144,    -1,
     146,   147,   148,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    90,    -1,    92,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,
     119,    -1,   121,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,   138,
     139,   140,    -1,   142,    -1,   144,    -1,   146,   147,   148,
      51,    52,    -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    51,    52,    -1,    -1,
      55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    51,    52,    -1,    -1,    55,    -1,   143,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    51,    52,
      -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    51,    52,    -1,    -1,
      55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    51,    52,    -1,    -1,    55,    -1,   143,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    51,    52,
      -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    51,    52,    -1,    -1,
      55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    51,    52,    -1,    -1,    55,    -1,   143,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    51,    52,
      -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    51,    52,    -1,    -1,
      55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    92,    93,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    -1,   121,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,   139,   140,   141,   142,    -1,   144,    -1,   146,   147,
     148,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,
      -1,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,
     121,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,   139,   140,
     141,   142,    -1,    -1,    -1,   146,   147,   148,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,    -1,    -1,    -1,   139,   140,    -1,   142,    -1,
      -1,    -1,   146,   147,   148,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    90,    -1,    -1,    93,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,    -1,   140,    -1,    -1,    -1,    -1,    -1,    -1,
     147,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,    -1,   101,    -1,    -1,    -1,    -1,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   150,   151,     1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    19,    21,    22,    23,    24,    30,
      31,    32,    33,    34,    35,    36,    39,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    58,
      59,    60,    63,    66,    67,    69,    70,    71,    84,    85,
      91,    94,    95,    97,    98,   100,   104,   106,   107,   108,
     110,   111,   112,   114,   134,   135,   136,   152,   153,   154,
     160,   161,   163,   166,   168,   170,   171,   174,   175,   177,
     178,   179,   181,   182,   191,   205,   222,   243,   244,   274,
     275,   276,   280,   281,   282,   288,   289,   290,   292,   293,
     294,   295,   296,   297,   333,   346,     0,   154,    21,    22,
      30,    31,    32,    39,    51,    55,    69,    88,    91,    94,
     134,   166,   168,   183,   184,   205,   222,   294,   297,   333,
     184,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    55,    70,    71,    72,    73,    74,    75,
      76,    77,    80,    81,    86,    87,    88,    89,   100,   101,
     102,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   135,   136,   143,   144,   185,   189,   190,   296,   328,
     206,    91,   163,   166,   168,   169,   182,   222,   294,   295,
     297,   169,   212,   214,    69,    91,   175,   182,   222,   227,
     294,   297,    33,    34,    35,    36,    48,    49,    50,    51,
      55,   106,   185,   186,   187,   290,   115,   118,   119,   146,
     148,   169,   284,   285,   286,   339,   343,   344,   345,    51,
      69,   100,   102,   103,   135,   174,   191,   197,   200,   203,
     276,   331,   332,   197,   197,   144,   194,   195,   198,   199,
     346,   194,   199,   144,   340,   186,   155,   138,   191,   222,
     191,   191,   191,    55,     1,    94,   157,   158,   160,   176,
     177,   346,   207,   209,   192,   203,   331,   346,   191,   330,
     331,   346,    91,   142,   181,   222,   294,   297,   210,    53,
      54,    56,    63,    69,   107,   185,   291,    63,    64,    65,
     116,   117,   277,   278,    61,   277,    62,   277,    63,   277,
      63,   277,    58,    59,   170,   191,   191,   339,   345,    40,
      41,    42,    43,    44,    37,    38,    51,    53,    54,    55,
      56,    69,    83,    94,   100,   101,   102,   103,   128,   131,
     144,   300,   301,   302,   303,   304,   307,   308,   309,   310,
     312,   313,   314,   315,   317,   318,   319,   322,   323,   324,
     325,   326,   346,   300,   302,    28,   242,   121,   142,    94,
     100,   178,   121,    25,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    88,    89,    93,   101,
     122,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    90,   105,   140,   147,   337,    90,   337,   338,    26,
     138,   246,   276,    92,    92,   194,   199,   246,   163,    51,
      55,   183,    58,    59,   301,   125,   298,    90,   140,   337,
     221,   329,    90,   147,   336,   156,   157,    55,   300,   300,
      16,   223,   343,   121,    90,   140,   337,    92,    92,   223,
     169,   169,    55,    90,   140,   337,    25,   107,   142,   287,
     339,   115,   286,    20,   248,   343,    57,    57,   191,   191,
     191,    93,   142,   201,   202,   346,    57,   201,   202,    85,
     196,   197,   203,   331,   346,   197,   163,   339,   341,   163,
     344,   159,   138,   157,    90,   337,    92,   160,   176,   145,
     339,   345,   341,   160,   341,   141,   202,   342,   345,   202,
     342,   139,   342,    55,   178,   179,   180,   142,    90,   140,
     337,   144,   239,   312,   317,    63,   277,   279,   283,   284,
      63,   278,    61,    62,    63,    63,   101,   101,   154,   169,
     169,   169,   169,   160,   163,   163,    57,   121,    57,   343,
     316,    85,   312,   317,   121,   156,   191,   142,   327,   346,
      51,   142,   327,   343,   142,   311,   191,   142,   311,    51,
     142,   311,    51,   121,   156,   241,   100,   170,   191,   203,
     204,   176,   142,   181,   142,   161,   162,   170,   182,   191,
     193,   204,   222,   297,   165,   191,   191,   191,   191,   191,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   164,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,    51,    52,    55,   189,   194,   334,   335,   196,
     203,    51,    52,    55,   189,   194,   334,    51,    55,   334,
     247,   245,   162,   191,   193,   162,   193,    99,   173,   219,
     299,   218,    51,    55,   183,   334,   196,   334,   156,   163,
     167,    15,    13,   270,   346,   121,   121,   157,    16,    51,
      55,   196,    51,    55,   157,    27,   224,   343,   224,    51,
      55,   196,    51,    55,   216,   188,   157,    25,   248,   191,
     203,    15,   191,   191,   191,   340,   100,   191,   200,   331,
     191,   332,   341,   145,   339,   202,   202,   341,   145,   186,
     152,   139,   193,   341,   160,   208,   331,   178,   180,    51,
      55,   196,    51,    55,   312,   211,   142,    63,   157,   284,
     191,   191,    51,    69,   100,   228,   317,   341,   341,   142,
     174,   191,    15,    51,    69,   304,   309,   326,    85,   310,
     315,   322,   324,   317,   319,   324,    51,   317,   174,   191,
      15,    79,   126,   233,   235,   346,   191,   202,   341,   180,
     142,    44,   121,    44,    90,   140,   337,    34,    35,    36,
      51,    55,    63,    91,    97,    98,   100,   102,   127,   222,
     254,   255,   257,   258,   259,   260,   263,   264,   266,   267,
     268,   269,   289,   293,   254,   340,    92,    92,   194,   199,
     141,   202,    92,    92,   195,   199,   195,   199,   233,   233,
     172,   343,   169,   156,   141,    15,   341,   185,   191,   204,
     271,   346,    18,   226,   346,    17,   225,   226,    92,    92,
     141,    92,    92,   226,   213,   215,   141,   169,   186,   139,
     254,    15,   202,   223,   191,   201,    85,   331,   139,   341,
     342,   141,   236,   340,    29,   113,   240,   139,   142,   314,
     341,   142,    85,    44,    44,   327,   343,   142,   311,   142,
     311,   142,   311,   142,   311,   311,    44,    44,   230,   232,
     234,   303,   305,   306,   309,   317,   318,   320,   321,   324,
     326,   156,   100,   191,   180,   160,   191,    51,    55,   196,
      51,    55,    57,    55,    51,    91,   100,   141,   222,   257,
     261,   262,   263,   289,    51,   102,   139,   265,   266,   268,
     289,    51,    34,    51,    51,    90,    51,   257,   263,   142,
      93,   126,   142,   142,    93,    57,   123,   162,   193,   170,
     193,   173,    92,   162,   193,   162,   193,   173,   246,   242,
     156,   157,   233,   220,   343,    15,    93,   272,   346,   157,
      14,   273,   346,   169,    15,    92,    15,   157,   157,   224,
      40,    41,   223,   191,   157,   341,   202,   145,   146,   156,
     157,   229,   142,   100,   341,   191,   191,   317,   324,   317,
     317,   191,   191,   236,   236,    91,   222,   142,   327,   327,
     142,   231,   222,   142,   231,   142,   231,    15,   191,   141,
     257,    55,    51,    90,   141,   142,   142,    57,    34,    51,
     139,   142,    51,    55,   142,   142,    51,   259,   256,   257,
      51,   267,   268,   289,   257,   191,   191,   162,   193,    15,
     139,   157,   156,    91,   182,   222,   294,   297,   223,   157,
     223,    15,    15,   217,   169,   169,   157,   226,   248,   249,
      51,   237,   238,   313,    15,   139,   317,   317,   142,   314,
     311,   142,   311,   311,   311,   126,   126,    55,    90,   305,
     309,   142,   230,   231,   321,   324,   317,   320,   324,   317,
      55,   257,   263,   262,   268,   256,   142,   139,    15,    55,
      90,   140,   337,   157,   157,   157,   223,   223,    25,   226,
     250,   142,   340,   142,   317,   142,   317,    55,   327,   142,
     231,   142,   231,   142,   231,   142,   231,   231,   142,   142,
     257,    51,    55,   196,    51,    55,   270,   225,    15,   157,
     157,   254,    15,   238,   317,   311,   317,   324,   317,   317,
     262,   141,   250,   250,   251,   252,   253,   231,   142,   231,
     231,   231,    15,    15,   223,    40,    41,   317,   157,   169,
     169,   231,   250,   223,   223,   157,   157,   250,   250
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   149,   150,   151,   152,   153,   153,   153,   153,   154,
     155,   154,   156,   157,   158,   158,   158,   158,   159,   160,
     160,   160,   160,   160,   160,   160,   160,   160,   160,   160,
     160,   160,   160,   161,   161,   161,   161,   161,   161,   161,
     161,   161,   161,   161,   161,   162,   162,   162,   163,   163,
     163,   163,   163,   164,   163,   165,   163,   163,   166,   167,
     168,   169,   170,   170,   171,   171,   172,   173,   174,   174,
     174,   174,   174,   174,   174,   174,   174,   174,   174,   175,
     175,   176,   176,   177,   177,   177,   177,   177,   177,   177,
     177,   177,   177,   178,   178,   179,   179,   180,   180,   181,
     181,   181,   181,   181,   181,   181,   181,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   183,   183,   184,   184,
     184,   185,   185,   185,   185,   185,   186,   186,   187,   188,
     187,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,   192,   192,   192,   192,   193,   193,   194,   194,
     194,   195,   195,   196,   196,   196,   196,   196,   197,   197,
     197,   197,   197,   198,   199,   200,   200,   201,   201,   202,
     203,   203,   203,   203,   203,   203,   204,   204,   204,   205,
     205,   205,   205,   205,   205,   205,   205,   206,   205,   207,
     208,   205,   209,   205,   205,   205,   205,   205,   205,   205,
     205,   205,   205,   205,   205,   205,   210,   211,   205,   205,
     205,   212,   213,   205,   214,   215,   205,   205,   205,   205,
     205,   205,   216,   217,   205,   218,   205,   219,   220,   205,
     221,   205,   205,   205,   205,   205,   205,   205,   222,   223,
     223,   223,   224,   224,   225,   225,   226,   226,   227,   227,
     228,   228,   228,   228,   228,   228,   228,   228,   229,   228,
     230,   230,   230,   230,   231,   231,   232,   232,   232,   232,
     232,   232,   232,   232,   232,   232,   232,   232,   232,   232,
     232,   233,   233,   234,   235,   235,   235,   236,   236,   237,
     237,   238,   238,   239,   239,   240,   240,   241,   242,   243,
     243,   243,   243,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   245,   246,   247,   246,   248,   249,   249,   250,
     251,   250,   252,   250,   253,   250,   254,   254,   254,   254,
     254,   254,   254,   254,   254,   255,   255,   256,   256,   257,
     257,   258,   258,   259,   259,   259,   259,   259,   259,   259,
     259,   259,   259,   259,   259,   259,   260,   260,   261,   261,
     261,   261,   261,   262,   262,   263,   263,   264,   264,   265,
     265,   265,   266,   266,   267,   267,   267,   268,   268,   268,
     269,   270,   270,   271,   271,   271,   272,   272,   273,   273,
     274,   274,   274,   274,   275,   275,   276,   276,   276,   276,
     277,   277,   278,   279,   278,   278,   278,   280,   280,   281,
     281,   282,   283,   283,   284,   284,   285,   285,   286,   287,
     286,   288,   288,   289,   289,   289,   290,   291,   291,   291,
     291,   291,   291,   292,   292,   293,   293,   293,   293,   294,
     294,   294,   294,   294,   295,   295,   296,   296,   296,   296,
     296,   296,   296,   296,   296,   297,   297,   298,   299,   298,
     300,   300,   301,   301,   301,   302,   302,   302,   302,   303,
     303,   304,   304,   305,   305,   306,   306,   307,   307,   308,
     308,   309,   309,   310,   310,   310,   310,   311,   311,   312,
     312,   312,   312,   312,   312,   312,   312,   312,   312,   312,
     312,   312,   312,   312,   313,   313,   313,   313,   313,   314,
     314,   315,   316,   315,   317,   317,   318,   319,   320,   321,
     321,   322,   322,   323,   323,   324,   324,   325,   325,   326,
     326,   327,   327,   328,   329,   328,   330,   330,   331,   331,
     332,   332,   332,   332,   332,   332,   332,   332,   333,   333,
     333,   334,   334,   334,   334,   335,   335,   335,   336,   336,
     337,   337,   338,   338,   339,   339,   340,   340,   341,   342,
     342,   342,   343,   343,   344,   344,   345,   345,   346
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     1,     3,     3,     6,     5,     5,     5,     5,
       4,     6,     4,     6,     3,     1,     3,     1,     1,     3,
       3,     3,     2,     0,     4,     0,     4,     1,     2,     0,
       5,     1,     1,     1,     1,     4,     0,     5,     2,     3,
       4,     5,     4,     5,     2,     2,     2,     2,     2,     1,
       3,     1,     3,     1,     2,     3,     5,     2,     4,     2,
       4,     1,     3,     1,     3,     2,     3,     1,     2,     1,
       4,     3,     3,     3,     3,     2,     1,     1,     4,     3,
       3,     3,     3,     2,     1,     1,     1,     1,     2,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       4,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     3,     6,     5,     5,     5,     5,     4,     3,
       3,     2,     2,     3,     2,     2,     3,     3,     3,     3,
       3,     3,     4,     4,     2,     2,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     2,
       2,     3,     3,     3,     3,     6,     6,     4,     6,     4,
       6,     1,     1,     2,     4,     2,     1,     3,     3,     5,
       3,     1,     1,     1,     2,     2,     4,     2,     1,     2,
       2,     4,     1,     0,     2,     2,     1,     2,     1,     2,
       1,     1,     2,     3,     3,     4,     3,     4,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     4,     0,
       0,     5,     0,     3,     3,     3,     2,     3,     3,     1,
       2,     4,     3,     2,     1,     2,     0,     0,     5,     6,
       6,     0,     0,     7,     0,     0,     7,     5,     4,     9,
      11,    11,     0,     0,     9,     0,     6,     0,     0,     8,
       0,     5,     4,     4,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     5,     1,     2,     1,     1,
       1,     4,     6,     3,     5,     2,     4,     1,     0,     4,
       4,     2,     2,     1,     2,     0,     6,     8,     4,     6,
       4,     3,     6,     2,     4,     6,     2,     4,     2,     4,
       1,     1,     1,     0,     4,     1,     4,     1,     4,     1,
       3,     1,     1,     4,     1,     3,     3,     0,     5,     2,
       4,     5,     5,     2,     4,     4,     3,     3,     3,     2,
       1,     4,     0,     5,     0,     5,     5,     1,     1,     1,
       0,     6,     0,     8,     0,     8,     1,     2,     2,     4,
       1,     3,     1,     3,     1,     2,     3,     1,     3,     1,
       3,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     2,     1,     1,     2,     3,     2,     1,     3,
       5,     1,     3,     1,     3,     2,     1,     3,     2,     1,
       3,     1,     1,     3,     3,     2,     3,     2,     2,     1,
       1,     6,     1,     1,     1,     1,     2,     1,     2,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     2,     3,
       1,     2,     1,     0,     4,     1,     2,     2,     3,     2,
       3,     1,     1,     2,     1,     2,     1,     2,     1,     0,
       4,     2,     3,     1,     4,     2,     2,     1,     1,     1,
       1,     1,     2,     2,     3,     1,     1,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     0,     4,
       1,     1,     3,     5,     3,     1,     2,     4,     2,     2,
       2,     2,     1,     2,     1,     1,     3,     1,     3,     1,
       1,     2,     1,     4,     2,     2,     1,     2,     0,     6,
       8,     4,     6,     4,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     1,     3,     2,     2,     2,     1,
       3,     1,     3,     1,     1,     2,     1,     1,     1,     2,
       1,     2,     1,     1,     0,     4,     1,     2,     1,     3,
       3,     3,     2,     2,     3,     3,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     0,     2,     2,     0,
       1,     1,     1,     1,     1,     1,     1,     2,     0
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab
#define YYNOMEM         goto yyexhaustedlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
  do                                                              \
    if (yychar == YYEMPTY)                                        \
      {                                                           \
        yychar = (Token);                                         \
        yylval = (Value);                                         \
        YYPOPSTACK (yylen);                                       \
        yystate = *yyssp;                                         \
        goto yybackup;                                            \
      }                                                           \
    else                                                          \
      {                                                           \
        yyerror (&yylloc, p, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF

/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)                                \
    do                                                                  \
      if (N)                                                            \
        {                                                               \
          (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;        \
          (Current).first_column = YYRHSLOC (Rhs, 1).first_column;      \
          (Current).last_line    = YYRHSLOC (Rhs, N).last_line;         \
          (Current).last_column  = YYRHSLOC (Rhs, N).last_column;       \
        }                                                               \
      else                                                              \
        {                                                               \
          (Current).first_line   = (Current).last_line   =              \
            YYRHSLOC (Rhs, 0).last_line;                                \
          (Current).first_column = (Current).last_column =              \
            YYRHSLOC (Rhs, 0).last_column;                              \
        }                                                               \
    while (0)
#endif

#define YYRHSLOC(Rhs, K) ((Rhs)[K])


/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)


/* YYLOCATION_PRINT -- Print the location on the stream.
   This macro was not mandated originally: define only if we know
   we won't break user code: when these are the locations we know.  */

# ifndef YYLOCATION_PRINT

#  if defined YY_LOCATION_PRINT

   /* Temporary convenience wrapper in case some people defined the
      undocumented and private YY_LOCATION_PRINT macros.  */
#   define YYLOCATION_PRINT(File, Loc, p)  YY_LOCATION_PRINT(File, *(Loc), p)

#  elif defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL

/* Print *YYLOCP on YYO.  Private, do not rely on its existence. */

YY_ATTRIBUTE_UNUSED
static int
yy_location_print_ (FILE *yyo, YYLTYPE const * const yylocp)
{
  int res = 0;
  int end_col = 0 != yylocp->last_column ? yylocp->last_column - 1 : 0;
  if (0 <= yylocp->first_line)
    {
      res += YYFPRINTF (yyo, "%d", yylocp->first_line);
      if (0 <= yylocp->first_column)
        res += YYFPRINTF (yyo, ".%d", yylocp->first_column);
    }
  if (0 <= yylocp->last_line)
    {
      if (yylocp->first_line < yylocp->last_line)
        {
          res += YYFPRINTF (yyo, "-%d", yylocp->last_line);
          if (0 <= end_col)
            res += YYFPRINTF (yyo, ".%d", end_col);
        }
      else if (0 <= end_col && yylocp->first_column < end_col)
        res += YYFPRINTF (yyo, "-%d", end_col);
    }
  return res;
}

#   define YYLOCATION_PRINT  yy_location_print_

    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT(File, Loc, p)  YYLOCATION_PRINT(File, &(Loc), p)

#  else

#   define YYLOCATION_PRINT(File, Loc, p) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location, p) \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, p);          \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, parser_state *p)
{
  FILE *yyoutput = yyo;
  YY_USE (yyoutput);
  YY_USE (yylocationp);
  YY_USE (p);
  if (!yyvaluep)
    return;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
switch (yykind)
    {
      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, YYLTYPE const * const yylocationp, parser_state *p)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  YYLOCATION_PRINT (yyo, yylocationp, p);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, p);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop, parser_state *p)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top, p)     \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top), p);    \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, YYLTYPE *yylsp,
                 int yyrule, parser_state *p)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       YY_ACCESSING_SYMBOL (+yyssp[yyi + 1 - yynrhs]),
                       &yyvsp[(yyi + 1) - (yynrhs)],
                       &(yylsp[(yyi + 1) - (yynrhs)]), p);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule, p) \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, p); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
#ifndef yydebug
int yydebug;
#endif
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location, p)
# define YY_STACK_PRINT(Bottom, Top, p)
# define YY_REDUCE_PRINT(Rule, p)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


/* Context of a parse error.  */
typedef struct
{
  yy_state_t *yyssp;
  yysymbol_kind_t yytoken;
  YYLTYPE *yylloc;
} yypcontext_t;

/* Put in YYARG at most YYARGN of the expected tokens given the
   current YYCTX, and return the number of tokens stored in YYARG.  If
   YYARG is null, return the number of expected tokens (guaranteed to
   be less than YYNTOKENS).  Return YYENOMEM on memory exhaustion.
   Return 0 if there are more than YYARGN expected tokens, yet fill
   YYARG up to YYARGN. */
static int
yypcontext_expected_tokens (const yypcontext_t *yyctx,
                            yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  int yyn = yypact[+*yyctx->yyssp];
  if (!yypact_value_is_default (yyn))
    {
      /* Start YYX at -YYN if negative to avoid negative indexes in
         YYCHECK.  In other words, skip the first -YYN actions for
         this state because they are default actions.  */
      int yyxbegin = yyn < 0 ? -yyn : 0;
      /* Stay within bounds of both yycheck and yytname.  */
      int yychecklim = YYLAST - yyn + 1;
      int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
      int yyx;
      for (yyx = yyxbegin; yyx < yyxend; ++yyx)
        if (yycheck[yyx + yyn] == yyx && yyx != YYSYMBOL_YYerror
            && !yytable_value_is_error (yytable[yyx + yyn]))
          {
            if (!yyarg)
              ++yycount;
            else if (yycount == yyargn)
              return 0;
            else
              yyarg[yycount++] = YY_CAST (yysymbol_kind_t, yyx);
          }
    }
  if (yyarg && yycount == 0 && 0 < yyargn)
    yyarg[0] = YYSYMBOL_YYEMPTY;
  return yycount;
}




#ifndef yystrlen
# if defined __GLIBC__ && defined _STRING_H
#  define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
# else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
# endif
#endif

#ifndef yystpcpy
# if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#  define yystpcpy stpcpy
# else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
# endif
#endif

#ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
      char const *yyp = yystr;
      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            else
              goto append;

          append:
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
}
#endif


static int
yy_syntax_error_arguments (const yypcontext_t *yyctx,
                           yysymbol_kind_t yyarg[], int yyargn)
{
  /* Actual size of YYARG. */
  int yycount = 0;
  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yyctx->yytoken != YYSYMBOL_YYEMPTY)
    {
      int yyn;
      if (yyarg)
        yyarg[yycount] = yyctx->yytoken;
      ++yycount;
      yyn = yypcontext_expected_tokens (yyctx,
                                        yyarg ? yyarg + 1 : yyarg, yyargn - 1);
      if (yyn == YYENOMEM)
        return YYENOMEM;
      else
        yycount += yyn;
    }
  return yycount;
}

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return -1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return YYENOMEM if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                const yypcontext_t *yyctx, parser_state *p)
{
  enum { YYARGS_MAX = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  yysymbol_kind_t yyarg[YYARGS_MAX];
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

  /* Actual size of YYARG. */
  int yycount = yy_syntax_error_arguments (yyctx, yyarg, YYARGS_MAX);
  if (yycount == YYENOMEM)
    return YYENOMEM;

  switch (yycount)
    {
#define YYCASE_(N, S)                       \
      case N:                               \
        yyformat = S;                       \
        break
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
#undef YYCASE_
    }

  /* Compute error message size.  Don't count the "%s"s, but reserve
     room for the terminator.  */
  yysize = yystrlen (yyformat) - 2 * yycount + 1;
  {
    int yyi;
    for (yyi = 0; yyi < yycount; ++yyi)
      {
        YYPTRDIFF_T yysize1
          = yysize + yytnamerr (YY_NULLPTR, yytname[yyarg[yyi]]);
        if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
          yysize = yysize1;
        else
          return YYENOMEM;
      }
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return -1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yytname[yyarg[yyi++]]);
          yyformat += 2;
        }
      else
        {
          ++yyp;
          ++yyformat;
        }
  }
  return 0;
}


/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg,
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, YYLTYPE *yylocationp, parser_state *p)
{
  YY_USE (yyvaluep);
  YY_USE (yylocationp);
  YY_USE (p);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp, p);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  switch (yykind)
    {
      default:
        break;
    }
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}






/*----------.
| yyparse.  |
`----------*/

int
yyparse (parser_state *p)
{
/* Lookahead token kind.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
#ifdef __cplusplus
static const YYSTYPE yyval_default = {};
(void) yyval_default;
#else
YY_INITIAL_VALUE (static const YYSTYPE yyval_default;)
#endif
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static const YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;
    YY_USE (yynerrs); /* Silence compiler warning.  */

    yy_state_fast_t yystate = 0;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus = 0;

    /* Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* Their size.  */
    YYPTRDIFF_T yystacksize = YYINITDEPTH;

    /* The state stack: array, bottom, top.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss = yyssa;
    yy_state_t *yyssp = yyss;

    /* The semantic value stack: array, bottom, top.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs = yyvsa;
    YYSTYPE *yyvsp = yyvs;

    /* The location stack: array, bottom, top.  */
    YYLTYPE yylsa[YYINITDEPTH];
    YYLTYPE *yyls = yylsa;
    YYLTYPE *yylsp = yyls;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead symbol kind.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;
  YYLTYPE yyloc;

  /* The locations where the error started and ended.  */
  YYLTYPE yyerror_range[3];

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N), yylsp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yychar = YYEMPTY; /* Cause a token to be read.  */



#line 7840 "mrbgems/mruby-compiler/core/y.tab.c"

  yylsp[0] = yylloc;
  goto yysetstate;


/*------------------------------------------------------------.
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END
  YY_STACK_PRINT (yyss, yyssp, p);

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    YYNOMEM;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;
        YYLTYPE *yyls1 = yyls;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yyls1, yysize * YYSIZEOF (*yylsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
        yyls = yyls1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        YYNOMEM;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          YYNOMEM;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
        YYSTACK_RELOCATE (yyls_alloc, yyls);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;
      yylsp = yyls + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */


  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;


/*-----------.
| yybackup.  |
`-----------*/
yybackup:
  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, &yylloc, p);
    }

  if (yychar <= YYEOF)
    {
      yychar = YYEOF;
      yytoken = YYSYMBOL_YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else if (yychar == YYerror)
    {
      /* The scanner already issued an error message, process directly
         to error recovery.  But do not keep the error token as
         lookahead, it is too special and may lead us to an endless
         loop in error recovery. */
      yychar = YYUNDEF;
      yytoken = YYSYMBOL_YYerror;
      yyerror_range[1] = yylloc;
      goto yyerrlab1;
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc, p);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc, p);
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END
  *++yylsp = yylloc;


  /* Discard the shifted token.  */
  yychar = YYEMPTY;
  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  /* Default location. */
  YYLLOC_DEFAULT (yyloc, (yylsp - yylen), yylen);
  yyerror_range[1] = yyloc;
  YY_REDUCE_PRINT (yyn, p);
  switch (yyn)
    {
  case 2: /* $@1: %empty  */
#line 2168 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 8058 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3: /* program: $@1 top_compstmt  */
#line 2173 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                    }
#line 8066 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4: /* top_compstmt: top_stmts opt_terms  */
#line 2179 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8074 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5: /* top_stmts: none  */
#line 2185 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8082 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6: /* top_stmts: top_stmt  */
#line 2189 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 8090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7: /* top_stmts: top_stmts terms top_stmt  */
#line 2193 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = stmts_push(p, (yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 8098 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8: /* top_stmts: error top_stmt  */
#line 2197 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8106 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10: /* @2: %empty  */
#line 2204 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8115 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11: /* top_stmt: "'BEGIN'" @2 '{' top_compstmt '}'  */
#line 2209 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 8126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12: /* bodystmt: compstmt opt_rescue opt_else opt_ensure  */
#line 2221 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd)) {
                        (yyval.nd) = new_rescue(p, (yyvsp[-3].nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      }
                      else if ((yyvsp[-1].nd)) {
                        yywarning(p, "else without rescue is useless");
                        (yyval.nd) = stmts_push(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      }
                      else {
                        (yyval.nd) = (yyvsp[-3].nd);
                      }
                      if ((yyvsp[0].nd)) {
                        if ((yyval.nd)) {
                          (yyval.nd) = new_ensure(p, (yyval.nd), (yyvsp[0].nd));
                        }
                        else {
                          (yyval.nd) = push((yyvsp[0].nd), new_nil(p));
                        }
                      }
                    }
#line 8151 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13: /* compstmt: stmts opt_terms  */
#line 2244 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8159 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14: /* stmts: none  */
#line 2250 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8167 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15: /* stmts: stmt  */
#line 2254 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 8175 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16: /* stmts: stmts terms stmt  */
#line 2258 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = stmts_push(p, (yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 8183 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17: /* stmts: error stmt  */
#line 2262 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 8191 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18: /* $@3: %empty  */
#line 2267 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 8197 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19: /* stmt: "'alias'" fsym $@3 fsym  */
#line 2268 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 8205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20: /* stmt: "'undef'" undef_list  */
#line 2272 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].nd));
                    }
#line 8213 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21: /* stmt: stmt "'if' modifier" expr_value  */
#line 2276 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 8221 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22: /* stmt: stmt "'unless' modifier" expr_value  */
#line 2280 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), 0, (yyvsp[-2].nd));
                    }
#line 8229 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23: /* stmt: stmt "'while' modifier" expr_value  */
#line 2284 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd) && node_type_p((yyvsp[-2].nd), NODE_BEGIN)) {
                        (yyval.nd) = new_while_mod(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                      else {
                        (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                    }
#line 8242 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24: /* stmt: stmt "'until' modifier" expr_value  */
#line 2293 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd) && node_type_p((yyvsp[-2].nd), NODE_BEGIN)) {
                        (yyval.nd) = new_until_mod(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                      else {
                        (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                    }
#line 8255 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25: /* stmt: stmt "'rescue' modifier" stmt  */
#line 2302 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8263 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26: /* stmt: "'END'" '{' compstmt '}'  */
#line 2306 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 8272 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28: /* stmt: mlhs '=' command_call  */
#line 2312 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29: /* stmt: lhs '=' mrhs  */
#line 2316 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 8288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30: /* stmt: mlhs '=' arg  */
#line 2320 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31: /* stmt: mlhs '=' mrhs  */
#line 2324 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 8304 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 33: /* command_asgn: lhs '=' command_rhs  */
#line 2331 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34: /* command_asgn: var_lhs tOP_ASGN command_rhs  */
#line 2335 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8320 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35: /* command_asgn: primary_value '[' opt_call_args ']' tOP_ASGN command_rhs  */
#line 2339 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8328 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36: /* command_asgn: primary_value call_op "local variable or method" tOP_ASGN command_rhs  */
#line 2343 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8336 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37: /* command_asgn: primary_value call_op "constant" tOP_ASGN command_rhs  */
#line 2347 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8344 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38: /* command_asgn: primary_value "::" "constant" tOP_ASGN command_call  */
#line 2351 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 8353 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39: /* command_asgn: primary_value "::" "local variable or method" tOP_ASGN command_rhs  */
#line 2356 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8361 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40: /* command_asgn: defn_head f_opt_arglist_paren '=' command  */
#line 2360 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8374 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41: /* command_asgn: defn_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 2369 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42: /* command_asgn: defs_head f_opt_arglist_paren '=' command  */
#line 2378 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43: /* command_asgn: defs_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 2387 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8413 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44: /* command_asgn: backref tOP_ASGN command_rhs  */
#line 2396 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 46: /* command_rhs: command_call "'rescue' modifier" stmt  */
#line 2404 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 49: /* expr: expr "'and'" expr  */
#line 2412 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50: /* expr: expr "'or'" expr  */
#line 2416 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51: /* expr: "'not'" opt_nl expr  */
#line 2420 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 8454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52: /* expr: '!' command_call  */
#line 2424 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 8462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53: /* $@4: %empty  */
#line 2427 "mrbgems/mruby-compiler/core/parse.y"
                             {p->in_kwarg++;}
#line 8468 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 54: /* expr: arg "=>" $@4 p_expr  */
#line 2428 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* expr => pattern (raises NoMatchingPatternError on failure) */
                      p->in_kwarg--;
                      (yyval.nd) = new_match_pat(p, (yyvsp[-3].nd), (yyvsp[0].nd), TRUE);
                    }
#line 8478 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55: /* $@5: %empty  */
#line 2433 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->in_kwarg++;}
#line 8484 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56: /* expr: arg "'in'" $@5 p_expr  */
#line 2434 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* expr in pattern (returns true/false) */
                      p->in_kwarg--;
                      (yyval.nd) = new_match_pat(p, (yyvsp[-3].nd), (yyvsp[0].nd), FALSE);
                    }
#line 8494 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58: /* defn_head: "'def'" fname  */
#line 2443 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 8505 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 59: /* $@6: %empty  */
#line 2452 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 8513 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 60: /* defs_head: "'def'" singleton dot_or_colon $@6 fname  */
#line 2456 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 8526 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 61: /* expr_value: expr  */
#line 2467 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 8537 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65: /* block_command: block_call call_op2 operation2 command_args  */
#line 2481 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8545 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66: /* $@7: %empty  */
#line 2487 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8554 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67: /* cmd_brace_block: "{" $@7 opt_block_param compstmt '}'  */
#line 2494 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8564 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68: /* command: operation command_args  */
#line 2502 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8572 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69: /* command: operation command_args cmd_brace_block  */
#line 2506 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 8581 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70: /* command: primary_value call_op operation2 command_args  */
#line 2511 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8589 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71: /* command: primary_value call_op operation2 command_args cmd_brace_block  */
#line 2515 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 8598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72: /* command: primary_value "::" operation2 command_args  */
#line 2520 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8606 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73: /* command: primary_value "::" operation2 command_args cmd_brace_block  */
#line 2524 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 8615 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74: /* command: "'super'" command_args  */
#line 2529 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8623 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75: /* command: "'yield'" command_args  */
#line 2533 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 8631 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76: /* command: "'return'" call_args  */
#line 2537 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 8639 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77: /* command: "'break'" call_args  */
#line 2541 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 8647 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 78: /* command: "'next'" call_args  */
#line 2545 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 8655 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79: /* mlhs: mlhs_basic  */
#line 2551 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8663 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80: /* mlhs: tLPAREN mlhs_inner rparen  */
#line 2555 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82: /* mlhs_inner: tLPAREN mlhs_inner rparen  */
#line 2562 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8679 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83: /* mlhs_basic: mlhs_list  */
#line 2568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8687 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84: /* mlhs_basic: mlhs_list mlhs_item  */
#line 2572 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 8695 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85: /* mlhs_basic: mlhs_list "*" mlhs_node  */
#line 2576 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8703 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86: /* mlhs_basic: mlhs_list "*" mlhs_node ',' mlhs_post  */
#line 2580 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8711 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87: /* mlhs_basic: mlhs_list "*"  */
#line 2584 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 8719 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88: /* mlhs_basic: mlhs_list "*" ',' mlhs_post  */
#line 2588 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 8727 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89: /* mlhs_basic: "*" mlhs_node  */
#line 2592 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 8735 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 90: /* mlhs_basic: "*" mlhs_node ',' mlhs_post  */
#line 2596 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8743 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91: /* mlhs_basic: "*"  */
#line 2600 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 8751 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92: /* mlhs_basic: "*" ',' mlhs_post  */
#line 2604 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 8759 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94: /* mlhs_item: tLPAREN mlhs_inner rparen  */
#line 2611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 8767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95: /* mlhs_list: mlhs_item ','  */
#line 2617 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 8775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96: /* mlhs_list: mlhs_list mlhs_item ','  */
#line 2621 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 8783 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97: /* mlhs_post: mlhs_item  */
#line 2627 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98: /* mlhs_post: mlhs_list mlhs_item  */
#line 2631 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99: /* mlhs_node: variable  */
#line 2637 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100: /* mlhs_node: primary_value '[' opt_call_args ']'  */
#line 2641 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8815 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101: /* mlhs_node: primary_value call_op "local variable or method"  */
#line 2645 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8823 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102: /* mlhs_node: primary_value "::" "local variable or method"  */
#line 2649 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8831 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103: /* mlhs_node: primary_value call_op "constant"  */
#line 2653 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8839 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104: /* mlhs_node: primary_value "::" "constant"  */
#line 2657 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8849 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105: /* mlhs_node: tCOLON3 "constant"  */
#line 2663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8859 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106: /* mlhs_node: backref  */
#line 2669 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 8868 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107: /* lhs: variable  */
#line 2676 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8876 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108: /* lhs: primary_value '[' opt_call_args ']'  */
#line 2680 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109: /* lhs: primary_value call_op "local variable or method"  */
#line 2684 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110: /* lhs: primary_value "::" "local variable or method"  */
#line 2688 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111: /* lhs: primary_value call_op "constant"  */
#line 2692 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8908 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112: /* lhs: primary_value "::" "constant"  */
#line 2696 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8918 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113: /* lhs: tCOLON3 "constant"  */
#line 2702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8928 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 114: /* lhs: backref  */
#line 2708 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 8937 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115: /* lhs: "numbered parameter"  */
#line 2713 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 8945 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116: /* cname: "local variable or method"  */
#line 2719 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "class/module name must be CONSTANT");
                    }
#line 8953 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 118: /* cpath: tCOLON3 cname  */
#line 2726 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(int_to_node(1), sym_to_node((yyvsp[0].id)));
                    }
#line 8961 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 119: /* cpath: cname  */
#line 2730 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(int_to_node(0), sym_to_node((yyvsp[0].id)));
                    }
#line 8969 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 120: /* cpath: primary_value "::" cname  */
#line 2734 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), sym_to_node((yyvsp[0].id)));
                    }
#line 8978 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 124: /* fname: op  */
#line 2744 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8987 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125: /* fname: reswords  */
#line 2749 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128: /* undef_list: fsym  */
#line 2760 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(sym_to_node((yyvsp[0].id)), 0);
                    }
#line 9004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129: /* $@8: %empty  */
#line 2763 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 9010 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130: /* undef_list: undef_list ',' $@8 fsym  */
#line 2764 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), sym_to_node((yyvsp[0].id)));
                    }
#line 9018 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131: /* op: '|'  */
#line 2769 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(or);     }
#line 9024 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132: /* op: '^'  */
#line 2770 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(xor);    }
#line 9030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133: /* op: '&'  */
#line 2771 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(and);    }
#line 9036 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134: /* op: "<=>"  */
#line 2772 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(cmp);    }
#line 9042 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135: /* op: "=="  */
#line 2773 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eq);     }
#line 9048 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136: /* op: "==="  */
#line 2774 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eqq);    }
#line 9054 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137: /* op: "=~"  */
#line 2775 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(match);  }
#line 9060 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138: /* op: "!~"  */
#line 2776 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(nmatch); }
#line 9066 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139: /* op: '>'  */
#line 2777 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(gt);     }
#line 9072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140: /* op: ">="  */
#line 2778 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(ge);     }
#line 9078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141: /* op: '<'  */
#line 2779 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lt);     }
#line 9084 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142: /* op: "<="  */
#line 2780 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(le);     }
#line 9090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143: /* op: "!="  */
#line 2781 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neq);    }
#line 9096 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144: /* op: "<<"  */
#line 2782 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lshift); }
#line 9102 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145: /* op: ">>"  */
#line 2783 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(rshift); }
#line 9108 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146: /* op: '+'  */
#line 2784 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(add);    }
#line 9114 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147: /* op: '-'  */
#line 2785 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(sub);    }
#line 9120 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148: /* op: '*'  */
#line 2786 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 9126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149: /* op: "*"  */
#line 2787 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 9132 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150: /* op: '/'  */
#line 2788 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(div);    }
#line 9138 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151: /* op: '%'  */
#line 2789 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mod);    }
#line 9144 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152: /* op: tPOW  */
#line 2790 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 9150 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153: /* op: "**"  */
#line 2791 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 9156 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154: /* op: '!'  */
#line 2792 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(not);    }
#line 9162 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155: /* op: '~'  */
#line 2793 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neg);    }
#line 9168 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156: /* op: "unary plus"  */
#line 2794 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(plus);   }
#line 9174 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157: /* op: "unary minus"  */
#line 2795 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(minus);  }
#line 9180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 158: /* op: tAREF  */
#line 2796 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aref);   }
#line 9186 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 159: /* op: tASET  */
#line 2797 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aset);   }
#line 9192 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 160: /* op: '`'  */
#line 2798 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(tick);   }
#line 9198 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201: /* arg: lhs '=' arg_rhs  */
#line 2816 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9206 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202: /* arg: var_lhs tOP_ASGN arg_rhs  */
#line 2820 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 9214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203: /* arg: primary_value '[' opt_call_args ']' tOP_ASGN arg_rhs  */
#line 2824 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 9222 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204: /* arg: primary_value call_op "local variable or method" tOP_ASGN arg_rhs  */
#line 2828 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 9230 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205: /* arg: primary_value call_op "constant" tOP_ASGN arg_rhs  */
#line 2832 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 9238 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206: /* arg: primary_value "::" "local variable or method" tOP_ASGN arg_rhs  */
#line 2836 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 9246 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207: /* arg: primary_value "::" "constant" tOP_ASGN arg_rhs  */
#line 2840 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 9255 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208: /* arg: tCOLON3 "constant" tOP_ASGN arg_rhs  */
#line 2845 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "constant re-assignment");
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 9264 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209: /* arg: backref tOP_ASGN arg_rhs  */
#line 2850 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 9273 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210: /* arg: arg ".." arg  */
#line 2855 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9281 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211: /* arg: arg ".."  */
#line 2859 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 9289 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212: /* arg: tBDOT2 arg  */
#line 2863 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 9297 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213: /* arg: arg "..." arg  */
#line 2867 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9305 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214: /* arg: arg "..."  */
#line 2871 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 9313 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215: /* arg: tBDOT3 arg  */
#line 2875 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 9321 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216: /* arg: arg '+' arg  */
#line 2879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 9329 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217: /* arg: arg '-' arg  */
#line 2883 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 9337 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218: /* arg: arg '*' arg  */
#line 2887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 9345 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219: /* arg: arg '/' arg  */
#line 2891 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 9353 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220: /* arg: arg '%' arg  */
#line 2895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 9361 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221: /* arg: arg tPOW arg  */
#line 2899 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 9369 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222: /* arg: tUMINUS_NUM "integer literal" tPOW arg  */
#line 2903 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 9377 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223: /* arg: tUMINUS_NUM "float literal" tPOW arg  */
#line 2907 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 9385 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224: /* arg: "unary plus" arg  */
#line 2911 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 9393 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225: /* arg: "unary minus" arg  */
#line 2915 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 9401 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226: /* arg: arg '|' arg  */
#line 2919 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 9409 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227: /* arg: arg '^' arg  */
#line 2923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 9417 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228: /* arg: arg '&' arg  */
#line 2927 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 9425 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229: /* arg: arg "<=>" arg  */
#line 2931 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 9433 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230: /* arg: arg '>' arg  */
#line 2935 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 9441 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231: /* arg: arg ">=" arg  */
#line 2939 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 9449 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232: /* arg: arg '<' arg  */
#line 2943 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 9457 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233: /* arg: arg "<=" arg  */
#line 2947 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 9465 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234: /* arg: arg "==" arg  */
#line 2951 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 9473 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235: /* arg: arg "===" arg  */
#line 2955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 9481 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236: /* arg: arg "!=" arg  */
#line 2959 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 9489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237: /* arg: arg "=~" arg  */
#line 2963 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 9497 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238: /* arg: arg "!~" arg  */
#line 2967 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 9505 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239: /* arg: '!' arg  */
#line 2971 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 9513 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240: /* arg: '~' arg  */
#line 2975 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 9521 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241: /* arg: arg "<<" arg  */
#line 2979 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 9529 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242: /* arg: arg ">>" arg  */
#line 2983 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 9537 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243: /* arg: arg "&&" arg  */
#line 2987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9545 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244: /* arg: arg "||" arg  */
#line 2991 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9553 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245: /* arg: arg '?' arg opt_nl ':' arg  */
#line 2995 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 9561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246: /* arg: arg '?' arg opt_nl "label" arg  */
#line 2999 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 9569 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247: /* arg: defn_head f_opt_arglist_paren '=' arg  */
#line 3003 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 9582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248: /* arg: defn_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 3012 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 9595 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 249: /* arg: defs_head f_opt_arglist_paren '=' arg  */
#line 3021 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 9608 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250: /* arg: defs_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 3030 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 9621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251: /* arg: primary  */
#line 3039 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9629 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253: /* aref_args: args trailer  */
#line 3046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9637 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254: /* aref_args: args comma assocs trailer  */
#line 3050 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd)));
                    }
#line 9645 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255: /* aref_args: assocs trailer  */
#line 3054 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_hash(p, (yyvsp[-1].nd)), 0);
                    }
#line 9653 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256: /* arg_rhs: arg  */
#line 3060 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9661 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257: /* arg_rhs: arg "'rescue' modifier" arg  */
#line 3064 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 258: /* paren_args: '(' opt_call_args ')'  */
#line 3071 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9678 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 259: /* paren_args: '(' args comma tBDOT3 rparen  */
#line 3075 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      (yyval.nd) = new_callargs(p, push((yyvsp[-3].nd), new_splat(p, new_lvar(p, r))),
                                        list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k))),
                                        new_block_arg(p, new_lvar(p, b)));
                    }
#line 9691 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 260: /* paren_args: '(' tBDOT3 rparen  */
#line 3084 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      if (local_var_p(p, r) && local_var_p(p, k) && local_var_p(p, b)) {
                        (yyval.nd) = new_callargs(p, list1(new_splat(p, new_lvar(p, r))),
                                          list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k))),
                                          new_block_arg(p, new_lvar(p, b)));
                      }
                      else {
                        yyerror(&(yylsp[-2]), p, "unexpected argument forwarding ...");
                        (yyval.nd) = 0;
                      }
                    }
#line 9710 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265: /* opt_call_args: args comma  */
#line 3107 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-1].nd),0,0);
                    }
#line 9718 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266: /* opt_call_args: args comma assocs comma  */
#line 3111 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-3].nd),(yyvsp[-1].nd),0);
                    }
#line 9726 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267: /* opt_call_args: assocs comma  */
#line 3115 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,0,(yyvsp[-1].nd),0);
                    }
#line 9734 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268: /* call_args: command  */
#line 3121 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_callargs(p, list1((yyvsp[0].nd)), 0, 0);
                    }
#line 9743 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269: /* call_args: args opt_block_arg  */
#line 3126 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-1].nd), 0, (yyvsp[0].nd));
                    }
#line 9751 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270: /* call_args: assocs opt_block_arg  */
#line 3130 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9759 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 271: /* call_args: args comma assocs opt_block_arg  */
#line 3134 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272: /* call_args: block_arg  */
#line 3138 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, 0, (yyvsp[0].nd));
                    }
#line 9775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273: /* @9: %empty  */
#line 3143 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 9784 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274: /* command_args: @9 call_args  */
#line 3148 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9793 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275: /* block_arg: "&" arg  */
#line 3155 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 9801 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 276: /* block_arg: "&"  */
#line 3159 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, 0);
                    }
#line 9809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277: /* opt_block_arg: comma block_arg  */
#line 3165 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9817 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278: /* opt_block_arg: none  */
#line 3169 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9825 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280: /* args: arg  */
#line 3178 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9834 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281: /* args: "*"  */
#line 3183 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 9842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282: /* args: "*" arg  */
#line 3187 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 9850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 283: /* args: args comma arg  */
#line 3191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9859 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 284: /* args: args comma "*"  */
#line 3196 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 9867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 285: /* args: args comma "*" arg  */
#line 3200 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 9875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 286: /* mrhs: args comma arg  */
#line 3206 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 287: /* mrhs: args comma "*" arg  */
#line 3211 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 9892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 288: /* mrhs: "*" arg  */
#line 3215 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 9900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 290: /* primary: string  */
#line 3222 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_str(p, (yyvsp[0].nd));
                    }
#line 9908 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 291: /* primary: xstring  */
#line 3226 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_xstr(p, (yyvsp[0].nd));
                    }
#line 9916 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296: /* primary: "method"  */
#line 3234 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 9924 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297: /* @10: %empty  */
#line 3238 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298: /* primary: "'begin'" @10 bodystmt "'end'"  */
#line 3244 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = new_begin(p, (yyvsp[-1].nd));
                    }
#line 9942 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299: /* @11: %empty  */
#line 3249 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9951 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300: /* $@12: %empty  */
#line 3253 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 9957 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301: /* primary: "(" @11 stmt $@12 rparen  */
#line 3254 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 9966 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302: /* $@13: %empty  */
#line 3258 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 9972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303: /* primary: "(" $@13 rparen  */
#line 3259 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304: /* primary: tLPAREN compstmt ')'  */
#line 3263 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305: /* primary: primary_value "::" "constant"  */
#line 3267 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 9996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306: /* primary: tCOLON3 "constant"  */
#line 3271 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 10004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307: /* primary: "[" aref_args ']'  */
#line 3275 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                    }
#line 10012 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308: /* primary: tLBRACE assoc_list '}'  */
#line 3279 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                    }
#line 10020 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309: /* primary: "'return'"  */
#line 3283 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 10028 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310: /* primary: "'yield'" opt_paren_args  */
#line 3287 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 10036 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 311: /* primary: "'not'" '(' expr rparen  */
#line 3291 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 10044 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312: /* primary: "'not'" '(' rparen  */
#line 3295 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 10052 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313: /* primary: operation brace_block  */
#line 3299 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), new_callargs(p, 0, 0, (yyvsp[0].nd)));
                    }
#line 10060 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315: /* primary: method_call brace_block  */
#line 3304 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10069 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316: /* @14: %empty  */
#line 3309 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 10080 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317: /* @15: %empty  */
#line 3316 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 10089 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318: /* primary: "->" @14 f_larglist @15 lambda_body  */
#line 3321 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 10102 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319: /* primary: "'if'" expr_value then compstmt if_tail "'end'"  */
#line 3333 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 10111 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320: /* primary: "'unless'" expr_value then compstmt opt_else "'end'"  */
#line 3341 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd), (yyvsp[-2].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 10120 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321: /* $@16: %empty  */
#line 3345 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 10126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322: /* $@17: %empty  */
#line 3345 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 10132 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323: /* primary: "'while'" $@16 expr_value do $@17 compstmt "'end'"  */
#line 3348 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 10141 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324: /* $@18: %empty  */
#line 3352 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 10147 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325: /* $@19: %empty  */
#line 3352 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 10153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326: /* primary: "'until'" $@18 expr_value do $@19 compstmt "'end'"  */
#line 3355 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 10162 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327: /* primary: "'case'" expr_value opt_terms case_body "'end'"  */
#line 3362 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 10170 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328: /* primary: "'case'" opt_terms case_body "'end'"  */
#line 3366 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 10178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329: /* primary: "'case'" expr_value opt_terms "'in'" p_expr then compstmt in_clauses "'end'"  */
#line 3374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-4].nd), NULL, (yyvsp[-2].nd), FALSE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-7].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 10187 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330: /* primary: "'case'" expr_value opt_terms "'in'" p_expr "'if' modifier" expr_value then compstmt in_clauses "'end'"  */
#line 3383 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-4].nd), (yyvsp[-2].nd), FALSE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-9].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 10196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331: /* primary: "'case'" expr_value opt_terms "'in'" p_expr "'unless' modifier" expr_value then compstmt in_clauses "'end'"  */
#line 3392 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-4].nd), (yyvsp[-2].nd), TRUE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-9].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 10205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332: /* $@20: %empty  */
#line 3397 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 10211 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333: /* $@21: %empty  */
#line 3399 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 10217 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334: /* primary: "'for'" for_var "'in'" $@20 expr_value do $@21 compstmt "'end'"  */
#line 3402 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 10226 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335: /* @22: %empty  */
#line 3408 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 10237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336: /* primary: "'class'" cpath superclass @22 bodystmt "'end'"  */
#line 3416 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 10248 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337: /* @23: %empty  */
#line 3424 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 10257 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338: /* @24: %empty  */
#line 3429 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), int_to_node(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 10267 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 339: /* primary: "'class'" "<<" expr @23 term @24 bodystmt "'end'"  */
#line 3436 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = node_to_int((yyvsp[-2].nd)->cdr);
                    }
#line 10280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340: /* @25: %empty  */
#line 3446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 10291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 341: /* primary: "'module'" cpath @25 bodystmt "'end'"  */
#line 3454 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 10302 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 342: /* primary: defn_head f_arglist bodystmt "'end'"  */
#line 3464 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 10313 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 343: /* primary: defs_head f_arglist bodystmt "'end'"  */
#line 3474 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 10325 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 344: /* primary: "'break'"  */
#line 3482 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 10333 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 345: /* primary: "'next'"  */
#line 3486 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 10341 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 346: /* primary: "'redo'"  */
#line 3490 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 10349 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 347: /* primary: "'retry'"  */
#line 3494 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 10357 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 348: /* primary_value: primary  */
#line 3500 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 10366 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355: /* if_tail: "'elsif'" expr_value then compstmt if_tail  */
#line 3519 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10374 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357: /* opt_else: "'else'" compstmt  */
#line 3526 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10382 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358: /* for_var: lhs  */
#line 3532 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 10390 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360: /* f_margs: f_arg  */
#line 3539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 10398 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361: /* f_margs: f_arg ',' "*" f_norm_arg  */
#line 3543 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_lvar(p, (yyvsp[0].id)), 0);
                    }
#line 10406 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362: /* f_margs: f_arg ',' "*" f_norm_arg ',' f_arg  */
#line 3547 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_lvar(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 10414 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363: /* f_margs: f_arg ',' "*"  */
#line 3551 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3((yyvsp[-2].nd), int_to_node(-1), 0);
                    }
#line 10423 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364: /* f_margs: f_arg ',' "*" ',' f_arg  */
#line 3556 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), int_to_node(-1), (yyvsp[0].nd));
                    }
#line 10431 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365: /* f_margs: "*" f_norm_arg  */
#line 3560 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_lvar(p, (yyvsp[0].id)), 0);
                    }
#line 10439 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366: /* f_margs: "*" f_norm_arg ',' f_arg  */
#line 3564 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_lvar(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 10447 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367: /* f_margs: "*"  */
#line 3568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3(0, int_to_node(-1), 0);
                    }
#line 10456 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368: /* $@26: %empty  */
#line 3573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                    }
#line 10464 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369: /* f_margs: "*" ',' $@26 f_arg  */
#line 3577 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, int_to_node(-1), (yyvsp[0].nd));
                    }
#line 10472 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370: /* block_args_tail: f_block_kwarg ',' f_kwrest opt_f_block_arg  */
#line 3583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 10480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371: /* block_args_tail: f_block_kwarg opt_f_block_arg  */
#line 3587 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 10488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372: /* block_args_tail: f_kwrest opt_f_block_arg  */
#line 3591 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 10496 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373: /* block_args_tail: f_block_arg  */
#line 3595 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 10504 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374: /* opt_block_args_tail: ',' block_args_tail  */
#line 3601 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10512 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375: /* opt_block_args_tail: %empty  */
#line 3605 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 10520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3615 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378: /* block_param: f_arg ',' f_block_optarg opt_block_args_tail  */
#line 3619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 10544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379: /* block_param: f_arg ',' f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3623 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380: /* block_param: f_arg ',' f_rest_arg opt_block_args_tail  */
#line 3627 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381: /* block_param: f_arg ',' opt_block_args_tail  */
#line 3631 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382: /* block_param: f_arg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3635 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10576 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383: /* block_param: f_arg opt_block_args_tail  */
#line 3639 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10584 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384: /* block_param: f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3643 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10592 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385: /* block_param: f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3647 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10600 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386: /* block_param: f_block_optarg opt_block_args_tail  */
#line 3651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 10608 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387: /* block_param: f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3655 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10616 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388: /* block_param: f_rest_arg opt_block_args_tail  */
#line 3659 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389: /* block_param: f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390: /* block_param: block_args_tail  */
#line 3667 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10640 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 391: /* opt_block_param: none  */
#line 3673 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 10649 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392: /* opt_block_param: block_param_def  */
#line 3678 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10658 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393: /* $@27: %empty  */
#line 3684 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p);}
#line 10664 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 394: /* block_param_def: '|' $@27 opt_bv_decl '|'  */
#line 3685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10672 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395: /* block_param_def: "||"  */
#line 3689 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 10681 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396: /* block_param_def: '|' block_param opt_bv_decl '|'  */
#line 3694 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 10689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397: /* opt_bv_decl: opt_nl  */
#line 3700 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10697 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398: /* opt_bv_decl: opt_nl ';' bv_decls opt_nl  */
#line 3704 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10705 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401: /* bvar: "local variable or method"  */
#line 3714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 10714 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403: /* f_larglist: '(' f_args opt_bv_decl ')'  */
#line 3722 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 10722 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404: /* f_larglist: f_args  */
#line 3726 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10730 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405: /* lambda_body: tLAMBEG compstmt '}'  */
#line 3732 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10738 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406: /* lambda_body: "'do' for lambda" bodystmt "'end'"  */
#line 3736 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407: /* @28: %empty  */
#line 3742 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10756 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408: /* do_block: "'do' for block" @28 opt_block_param bodystmt "'end'"  */
#line 3750 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409: /* block_call: command do_block  */
#line 3759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10776 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410: /* block_call: block_call call_op2 operation2 opt_paren_args  */
#line 3764 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 10784 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411: /* block_call: block_call call_op2 operation2 opt_paren_args brace_block  */
#line 3768 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 10793 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412: /* block_call: block_call call_op2 operation2 command_args do_block  */
#line 3773 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 10802 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413: /* method_call: operation paren_args  */
#line 3780 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 10810 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414: /* method_call: primary_value call_op operation2 opt_paren_args  */
#line 3784 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 10818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415: /* method_call: primary_value "::" operation2 paren_args  */
#line 3788 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 10826 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416: /* method_call: primary_value "::" operation3  */
#line 3792 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 10834 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417: /* method_call: primary_value call_op paren_args  */
#line 3796 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 10842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418: /* method_call: primary_value "::" paren_args  */
#line 3800 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), tCOLON2);
                    }
#line 10850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419: /* method_call: "'super'" paren_args  */
#line 3804 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 10858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 420: /* method_call: "'super'"  */
#line 3808 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 10866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 421: /* method_call: primary_value '[' opt_call_args ']'  */
#line 3812 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 10874 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 422: /* @29: %empty  */
#line 3818 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423: /* brace_block: '{' @29 opt_block_param compstmt '}'  */
#line 3825 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10895 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 424: /* @30: %empty  */
#line 3832 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 425: /* brace_block: "'do'" @30 opt_block_param bodystmt "'end'"  */
#line 3839 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10916 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 426: /* case_body: "'when'" args then compstmt cases  */
#line 3850 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 10924 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 427: /* cases: opt_else  */
#line 3856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 10937 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 429: /* in_clauses: opt_else  */
#line 3870 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd) ? list1(new_in(p, NULL, NULL, (yyvsp[0].nd), FALSE)) : 0;
                    }
#line 10945 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 430: /* $@31: %empty  */
#line 3873 "mrbgems/mruby-compiler/core/parse.y"
                                    {p->in_kwarg--;}
#line 10951 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 431: /* in_clauses: "'in'" p_expr $@31 then compstmt in_clauses  */
#line 3874 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-4].nd), NULL, (yyvsp[-1].nd), FALSE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10960 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 432: /* $@32: %empty  */
#line 3878 "mrbgems/mruby-compiler/core/parse.y"
                                    {p->in_kwarg--;}
#line 10966 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 433: /* in_clauses: "'in'" p_expr $@32 "'if' modifier" expr_value then compstmt in_clauses  */
#line 3879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-3].nd), (yyvsp[-1].nd), FALSE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10975 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 434: /* $@33: %empty  */
#line 3883 "mrbgems/mruby-compiler/core/parse.y"
                                    {p->in_kwarg--;}
#line 10981 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435: /* in_clauses: "'in'" p_expr $@33 "'unless' modifier" expr_value then compstmt in_clauses  */
#line 3884 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-3].nd), (yyvsp[-1].nd), TRUE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10990 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 437: /* p_expr: p_args_head p_as  */
#line 3895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, push((yyvsp[-1].nd), (yyvsp[0].nd)), 0, 0);
                    }
#line 10998 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438: /* p_expr: p_args_head p_rest  */
#line 3899 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, (yyvsp[-1].nd), (yyvsp[0].nd), 0);
                    }
#line 11006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439: /* p_expr: p_args_head p_rest ',' p_args_post  */
#line 3903 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, (yyvsp[-3].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11014 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440: /* p_expr: p_rest  */
#line 3907 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[0].nd), 0);
                    }
#line 11022 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441: /* p_expr: p_rest ',' p_args_post  */
#line 3911 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442: /* p_expr: p_hash_elems  */
#line 3915 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Brace-less hash pattern: in a:, b: x */
                      (yyval.nd) = new_pat_hash(p, (yyvsp[0].nd), 0);
                    }
#line 11039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443: /* p_expr: p_hash_elems ',' p_kwrest  */
#line 3920 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Brace-less hash pattern with kwrest: in a:, **rest */
                      (yyval.nd) = new_pat_hash(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11048 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444: /* p_expr: p_kwrest  */
#line 3925 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Brace-less kwrest only: in **rest */
                      (yyval.nd) = new_pat_hash(p, 0, (yyvsp[0].nd));
                    }
#line 11057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445: /* p_args_head: p_as ','  */
#line 3933 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 11065 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446: /* p_args_head: p_args_head p_as ','  */
#line 3937 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 11073 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447: /* p_args_post: p_as  */
#line 3944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11081 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 448: /* p_args_post: p_args_post ',' p_as  */
#line 3948 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11089 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450: /* p_as: p_alt "=>" "local variable or method"  */
#line 3955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_as(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 11097 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 452: /* p_alt: p_alt '|' p_value  */
#line 3962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_alt(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 454: /* p_value: numeric  */
#line 3969 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 11113 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 455: /* p_value: symbol  */
#line 3973 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 11121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456: /* p_value: tSTRING  */
#line 3977 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 11129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457: /* p_value: "'nil'"  */
#line 3981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_nil(p));
                    }
#line 11137 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458: /* p_value: "'true'"  */
#line 3985 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_true(p));
                    }
#line 11145 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459: /* p_value: "'false'"  */
#line 3989 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_false(p));
                    }
#line 11153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460: /* p_value: "constant"  */
#line 3993 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_const(p, (yyvsp[0].id)));
                    }
#line 11161 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461: /* p_value: primary_value "::" "constant"  */
#line 3997 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id)));
                    }
#line 11169 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 462: /* p_value: tCOLON3 "constant"  */
#line 4001 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_colon3(p, (yyvsp[0].id)));
                    }
#line 11177 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465: /* p_value: '^' "local variable or method"  */
#line 4007 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_pin(p, (yyvsp[0].id));
                    }
#line 11185 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 466: /* p_array: "[" p_array_body ']'  */
#line 4014 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11193 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467: /* p_array: "[" ']'  */
#line 4018 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, 0, 0, 0);
                    }
#line 11201 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 468: /* p_array_body: p_array_elems  */
#line 4025 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Just pre elements, no rest */
                      (yyval.nd) = new_pat_array(p, (yyvsp[0].nd), 0, 0);
                    }
#line 11210 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 469: /* p_array_body: p_array_elems ',' p_rest  */
#line 4030 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Pre elements + rest, no post */
                      (yyval.nd) = new_pat_array(p, (yyvsp[-2].nd), (yyvsp[0].nd), 0);
                    }
#line 11219 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 470: /* p_array_body: p_array_elems ',' p_rest ',' p_array_elems  */
#line 4035 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Pre + rest + post */
                      (yyval.nd) = new_pat_array(p, (yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 471: /* p_array_body: p_rest  */
#line 4040 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Just rest, no pre or post */
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[0].nd), 0);
                    }
#line 11237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 472: /* p_array_body: p_rest ',' p_array_elems  */
#line 4045 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Rest + post, no pre */
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11246 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473: /* p_array_elems: p_as  */
#line 4053 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11254 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474: /* p_array_elems: p_array_elems ',' p_as  */
#line 4057 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11262 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475: /* p_rest: "*" "local variable or method"  */
#line 4064 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 11270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476: /* p_rest: "*"  */
#line 4068 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Anonymous rest pattern */
                      (yyval.nd) = (node*)-1;
                    }
#line 11279 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 477: /* p_hash: tLBRACE p_hash_body '}'  */
#line 4076 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11287 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478: /* p_hash: tLBRACE '}'  */
#line 4080 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_hash(p, 0, 0);
                    }
#line 11295 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479: /* p_hash_body: p_hash_elems  */
#line 4087 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_hash(p, (yyvsp[0].nd), 0);
                    }
#line 11303 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480: /* p_hash_body: p_hash_elems ',' p_kwrest  */
#line 4091 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_hash(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11311 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481: /* p_hash_body: p_kwrest  */
#line 4095 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_hash(p, 0, (yyvsp[0].nd));
                    }
#line 11319 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482: /* p_hash_elems: p_hash_elem  */
#line 4102 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11327 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483: /* p_hash_elems: p_hash_elems ',' p_hash_elem  */
#line 4106 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11335 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484: /* p_hash_elem: "local variable or method" "label" p_as  */
#line 4114 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* {key: pattern} */
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 11344 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485: /* p_hash_elem: "local variable or method" "label"  */
#line 4119 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* {key:} shorthand - binds to variable with same name */
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), new_pat_var(p, (yyvsp[-1].id)));
                    }
#line 11353 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486: /* p_hash_elem: symbol "=>" p_as  */
#line 4124 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* {:"key" => pattern} or {:key => pattern} */
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11362 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487: /* p_kwrest: "**" "local variable or method"  */
#line 4132 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 11370 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488: /* p_kwrest: "**" "'nil'"  */
#line 4136 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* **nil - exact match, no extra keys allowed */
                      (yyval.nd) = (node*)-1;
                    }
#line 11379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489: /* p_kwrest: "**"  */
#line 4141 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* ** - anonymous rest, discards extra keys */
                      (yyval.nd) = (node*)-2;
                    }
#line 11388 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 490: /* p_var: "local variable or method"  */
#line 4148 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 11396 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491: /* opt_rescue: "'rescue'" exc_list exc_var then compstmt opt_rescue  */
#line 4156 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 11405 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 493: /* exc_list: arg  */
#line 4164 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11413 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 496: /* exc_var: "=>" lhs  */
#line 4172 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 11421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 498: /* opt_ensure: "'ensure'" compstmt  */
#line 4179 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 11429 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505: /* string: string string_fragment  */
#line 4193 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11437 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506: /* string_fragment: "character literal"  */
#line 4199 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* tCHAR is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507: /* string_fragment: tSTRING  */
#line 4204 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* tSTRING is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11455 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508: /* string_fragment: "string literal" tSTRING  */
#line 4209 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* $2 is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11464 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509: /* string_fragment: "string literal" string_rep tSTRING  */
#line 4214 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11472 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511: /* string_rep: string_rep string_interp  */
#line 4221 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512: /* string_interp: tSTRING_MID  */
#line 4227 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* $1 is already in (len . str) format */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513: /* @34: %empty  */
#line 4232 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 11497 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514: /* string_interp: tSTRING_PART @34 compstmt '}'  */
#line 4237 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p,(yyvsp[-2].nd));
                      /* $1 is already in (len . str) format, create (-1 . node) for expression */
                      node *expr_elem = cons(int_to_node(-1), (yyvsp[-1].nd));
                      (yyval.nd) = list2((yyvsp[-3].nd), expr_elem);
                    }
#line 11508 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515: /* string_interp: tLITERAL_DELIM  */
#line 4244 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 11516 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516: /* string_interp: tHD_LITERAL_DELIM heredoc_bodies  */
#line 4248 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 11524 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517: /* xstring: tXSTRING_BEG tXSTRING  */
#line 4254 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = cons((yyvsp[0].nd), (node*)NULL);
                    }
#line 11532 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518: /* xstring: tXSTRING_BEG string_rep tXSTRING  */
#line 4258 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11540 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519: /* regexp: tREGEXP_BEG tREGEXP  */
#line 4264 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *data = (yyvsp[0].nd);  /* ((len . pattern) . (flags . encoding)) */
                      const char *flags = (const char*)data->cdr->car;
                      const char *encoding = (const char*)data->cdr->cdr;
                      /* Use data->car directly as pattern_list: (len . pattern) */
                      node *pattern_list = cons(data->car, (node*)NULL);
                      (yyval.nd) = new_regx(p, pattern_list, flags, encoding);
                    }
#line 11553 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520: /* regexp: tREGEXP_BEG string_rep tREGEXP  */
#line 4273 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *data = (yyvsp[0].nd);  /* ((len . pattern) . (flags . encoding)) */
                      const char *flags = (const char*)data->cdr->car;
                      const char *encoding = (const char*)data->cdr->cdr;
                      /* Append the pattern from $3->car to the string list $2 */
                      node *complete_list = push((yyvsp[-1].nd), data->car);
                      (yyval.nd) = new_regx(p, complete_list, flags, encoding);
                    }
#line 11566 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524: /* heredoc_body: tHEREDOC_END  */
#line 4291 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, new_str_empty(p));
                      heredoc_end(p);
                    }
#line 11576 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525: /* heredoc_body: heredoc_string_rep tHEREDOC_END  */
#line 4297 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 11584 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528: /* heredoc_string_interp: tHD_STRING_MID  */
#line 4307 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 11594 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529: /* @35: %empty  */
#line 4313 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 11602 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530: /* heredoc_string_interp: tHD_STRING_PART @35 compstmt '}'  */
#line 4318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p, (yyvsp[-2].nd));
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      /* $1 is already in (len . str) format, create (-1 . node) for expression */
                      node *expr_elem = cons(int_to_node(-1), (yyvsp[-1].nd));
                      info->doc = push(push(info->doc, (yyvsp[-3].nd)), expr_elem);
                    }
#line 11614 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531: /* words: tWORDS_BEG tSTRING  */
#line 4328 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 11622 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532: /* words: tWORDS_BEG string_rep tSTRING  */
#line 4332 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      n = push(n, (yyvsp[0].nd));
                      (yyval.nd) = new_words(p, n);
                    }
#line 11632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533: /* symbol: basic_symbol  */
#line 4340 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 11640 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534: /* symbol: "symbol" "string literal" string_rep tSTRING  */
#line 4344 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      p->lstate = EXPR_ENDARG;
                      if (node_to_int((yyvsp[0].nd)->car) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      else {
                        cons_free((yyvsp[0].nd));
                      }
                      (yyval.nd) = new_dsym(p, n);
                    }
#line 11656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535: /* symbol: "symbol" "numbered parameter"  */
#line 4356 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[0].num));
                      (yyval.nd) = new_sym(p, sym);
                    }
#line 11665 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536: /* basic_symbol: "symbol" sym  */
#line 4363 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_END;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11674 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541: /* sym: tSTRING  */
#line 4374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 11682 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542: /* sym: "string literal" tSTRING  */
#line 4378 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 11690 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543: /* symbols: tSYMBOLS_BEG tSTRING  */
#line 4384 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 11698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544: /* symbols: tSYMBOLS_BEG string_rep tSTRING  */
#line 4388 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      n = push(n, (yyvsp[0].nd));
                      (yyval.nd) = new_symbols(p, n);
                    }
#line 11708 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547: /* numeric: tUMINUS_NUM "integer literal"  */
#line 4398 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 11716 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548: /* numeric: tUMINUS_NUM "float literal"  */
#line 4402 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 11724 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549: /* variable: "local variable or method"  */
#line 4408 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 11732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550: /* variable: "instance variable"  */
#line 4412 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 11740 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551: /* variable: "global variable"  */
#line 4416 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 11748 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552: /* variable: "class variable"  */
#line 4420 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 11756 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553: /* variable: "constant"  */
#line 4424 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 11764 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554: /* var_lhs: variable  */
#line 4430 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 11772 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555: /* var_lhs: "numbered parameter"  */
#line 4434 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 11780 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 556: /* var_ref: variable  */
#line 4440 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 11788 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557: /* var_ref: "numbered parameter"  */
#line 4444 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 11796 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558: /* var_ref: "'nil'"  */
#line 4448 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 11804 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559: /* var_ref: "'self'"  */
#line 4452 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 11812 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560: /* var_ref: "'true'"  */
#line 4456 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 11820 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561: /* var_ref: "'false'"  */
#line 4460 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 11828 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562: /* var_ref: "'__FILE__'"  */
#line 4464 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, cons(cons(int_to_node(strlen(fn)), (node*)fn), (node*)NULL));
                    }
#line 11840 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563: /* var_ref: "'__LINE__'"  */
#line 4472 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 11851 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564: /* var_ref: "'__ENCODING__'"  */
#line 4479 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, MRB_SYM_2(p->mrb, __ENCODING__), 0);
                    }
#line 11859 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 567: /* superclass: %empty  */
#line 4489 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 11867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 568: /* $@36: %empty  */
#line 4493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 11876 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 569: /* superclass: '<' $@36 expr_value term  */
#line 4498 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572: /* f_arglist_paren: '(' f_args rparen  */
#line 4514 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 11894 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573: /* f_arglist_paren: '(' f_arg ',' tBDOT3 rparen  */
#line 4520 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 11902 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574: /* f_arglist_paren: '(' tBDOT3 rparen  */
#line 4524 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 11910 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 576: /* f_arglist: f_args term  */
#line 4531 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11918 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 577: /* f_arglist: f_arg ',' tBDOT3 term  */
#line 4535 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 11926 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 578: /* f_arglist: "..." term  */
#line 4539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 11934 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 579: /* f_label: "local variable or method" "label"  */
#line 4545 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 11942 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 580: /* f_label: "numbered parameter" "label"  */
#line 4549 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 11950 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 581: /* f_kw: f_label arg  */
#line 4555 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11960 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 582: /* f_kw: f_label  */
#line 4561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 11969 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 583: /* f_block_kw: f_label primary_value  */
#line 4568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11979 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 584: /* f_block_kw: f_label  */
#line 4574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 11988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 585: /* f_block_kwarg: f_block_kw  */
#line 4581 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 586: /* f_block_kwarg: f_block_kwarg ',' f_block_kw  */
#line 4585 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 12004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 587: /* f_kwarg: f_kw  */
#line 4591 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 12012 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 588: /* f_kwarg: f_kwarg ',' f_kw  */
#line 4595 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 12020 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 591: /* f_kwrest: kwrest_mark "local variable or method"  */
#line 4605 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 12028 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 592: /* f_kwrest: kwrest_mark  */
#line 4609 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(pow);
                    }
#line 12036 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 593: /* args_tail: f_kwarg ',' f_kwrest opt_f_block_arg  */
#line 4615 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 12044 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 594: /* args_tail: f_kwarg opt_f_block_arg  */
#line 4619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 12052 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 595: /* args_tail: f_kwrest opt_f_block_arg  */
#line 4623 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 12060 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 596: /* args_tail: f_block_arg  */
#line 4627 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 12068 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 597: /* opt_args_tail: ',' args_tail  */
#line 4633 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 12076 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 598: /* opt_args_tail: %empty  */
#line 4637 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 12084 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 599: /* f_args: f_arg ',' f_optarg ',' f_rest_arg opt_args_tail  */
#line 4643 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 12092 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 600: /* f_args: f_arg ',' f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4647 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 12100 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 601: /* f_args: f_arg ',' f_optarg opt_args_tail  */
#line 4651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 12108 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 602: /* f_args: f_arg ',' f_optarg ',' f_arg opt_args_tail  */
#line 4655 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 12116 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 603: /* f_args: f_arg ',' f_rest_arg opt_args_tail  */
#line 4659 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 12124 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 604: /* f_args: f_arg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 12132 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 605: /* f_args: f_arg opt_args_tail  */
#line 4667 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 12140 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 606: /* f_args: f_optarg ',' f_rest_arg opt_args_tail  */
#line 4671 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 12148 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 607: /* f_args: f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4675 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 12156 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 608: /* f_args: f_optarg opt_args_tail  */
#line 4679 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 12164 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 609: /* f_args: f_optarg ',' f_arg opt_args_tail  */
#line 4683 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 12172 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 610: /* f_args: f_rest_arg opt_args_tail  */
#line 4687 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 12180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 611: /* f_args: f_rest_arg ',' f_arg opt_args_tail  */
#line 4691 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 12188 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 612: /* f_args: args_tail  */
#line 4695 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 12196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 613: /* f_args: %empty  */
#line 4699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 12205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 614: /* f_bad_arg: "constant"  */
#line 4706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 12214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 615: /* f_bad_arg: "instance variable"  */
#line 4711 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 12223 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 616: /* f_bad_arg: "global variable"  */
#line 4716 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 12232 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 617: /* f_bad_arg: "class variable"  */
#line 4721 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 12241 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 618: /* f_bad_arg: "numbered parameter"  */
#line 4726 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 12250 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 619: /* f_norm_arg: f_bad_arg  */
#line 4733 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 12258 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 620: /* f_norm_arg: "local variable or method"  */
#line 4737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 12267 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 621: /* f_arg_item: f_norm_arg  */
#line 4744 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 12275 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 622: /* @37: %empty  */
#line 4748 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 12283 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 623: /* f_arg_item: tLPAREN @37 f_margs rparen  */
#line 4752 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_marg(p, (yyvsp[-1].nd));
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 12293 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 624: /* f_arg: f_arg_item  */
#line 4760 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 12301 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 625: /* f_arg: f_arg ',' f_arg_item  */
#line 4764 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 12309 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 626: /* f_opt_asgn: "local variable or method" '='  */
#line 4770 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 12319 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 627: /* f_opt: f_opt_asgn arg  */
#line 4778 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(sym_to_node((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 12329 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 628: /* f_block_opt: f_opt_asgn primary_value  */
#line 4786 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(sym_to_node((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 12339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 629: /* f_block_optarg: f_block_opt  */
#line 4794 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 12347 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 630: /* f_block_optarg: f_block_optarg ',' f_block_opt  */
#line 4798 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 12355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 631: /* f_optarg: f_opt  */
#line 4804 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 12363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 632: /* f_optarg: f_optarg ',' f_opt  */
#line 4808 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 12371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 635: /* f_rest_arg: restarg_mark "local variable or method"  */
#line 4818 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 12380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 636: /* f_rest_arg: restarg_mark  */
#line 4823 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(mul);
                      local_add_f(p, (yyval.id));
                    }
#line 12389 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 639: /* f_block_arg: blkarg_mark "local variable or method"  */
#line 4834 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 12397 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 640: /* f_block_arg: blkarg_mark  */
#line 4838 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(and);
                    }
#line 12405 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 641: /* opt_f_block_arg: ',' f_block_arg  */
#line 4844 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 12413 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 642: /* opt_f_block_arg: none  */
#line 4848 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 12421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 643: /* singleton: var_ref  */
#line 4854 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 12431 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 644: /* $@38: %empty  */
#line 4859 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 12437 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 645: /* singleton: '(' $@38 expr rparen  */
#line 4860 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[-1].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 12446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 647: /* assoc_list: assocs trailer  */
#line 4868 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 12454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 648: /* assocs: assoc  */
#line 4874 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 12462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 649: /* assocs: assocs comma assoc  */
#line 4878 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 12470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 650: /* assoc: arg "=>" arg  */
#line 4884 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 12480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 651: /* assoc: "local variable or method" "label" arg  */
#line 4890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 12489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 652: /* assoc: "local variable or method" "label"  */
#line 4895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), label_reference(p, (yyvsp[-1].id)));
                    }
#line 12497 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 653: /* assoc: "numbered parameter" "label"  */
#line 4899 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[-1].num));
                      (yyval.nd) = cons(new_sym(p, sym), label_reference(p, sym));
                    }
#line 12506 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 654: /* assoc: "numbered parameter" "label" arg  */
#line 4904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, intern_numparam((yyvsp[-2].num))), (yyvsp[0].nd));
                    }
#line 12515 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 655: /* assoc: string_fragment "label" arg  */
#line 4909 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if ((yyvsp[-2].nd)->cdr) {
                        /* Multiple fragments - create dynamic symbol */
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else if (node_to_int((yyvsp[-2].nd)->car->car) < 0) {
                        /* Single fragment but it's an expression (-1 . node) - create dynamic symbol */
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        /* Single string fragment - create simple symbol */
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd)->car)), (yyvsp[0].nd));
                      }
                    }
#line 12535 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 656: /* assoc: "**" arg  */
#line 4925 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 12544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 657: /* assoc: "**"  */
#line 4930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), new_lvar(p, intern_op(pow)));
                    }
#line 12552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 670: /* call_op: '.'  */
#line 4956 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 12560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 671: /* call_op: "&."  */
#line 4960 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 12568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 673: /* call_op2: "::"  */
#line 4967 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 12576 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 682: /* term: ';'  */
#line 4988 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 12582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 684: /* nl: '\n'  */
#line 4993 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 12591 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 688: /* none: %empty  */
#line 5005 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 12599 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 12603 "mrbgems/mruby-compiler/core/y.tab.c"

      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc, p);

  YYPOPSTACK (yylen);

  yylen = 0;

  *++yyvsp = yyval;
  *++yylsp = yyloc;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYSYMBOL_YYEMPTY : YYTRANSLATE (yychar);
  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
      {
        yypcontext_t yyctx
          = {yyssp, yytoken, &yylloc};
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx, p);
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == -1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *,
                             YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (yymsg)
              {
                yysyntax_error_status
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx, p);
                yymsgp = yymsg;
              }
            else
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = YYENOMEM;
              }
          }
        yyerror (&yylloc, p, yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          YYNOMEM;
      }
    }

  yyerror_range[1] = yylloc;
  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval, &yylloc, p);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;
  ++yynerrs;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);

  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp, p);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  /* Pop stack until we find a state that shifts the error token.  */
  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYSYMBOL_YYerror;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYSYMBOL_YYerror)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;

      yyerror_range[1] = *yylsp;
      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, yylsp, p);
      YYPOPSTACK (1);

      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp, p);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp, p);


  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturnlab;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturnlab;


/*-----------------------------------------------------------.
| yyexhaustedlab -- YYNOMEM (memory exhaustion) comes here.  |
`-----------------------------------------------------------*/
yyexhaustedlab:
  yyerror (&yylloc, p, YY_("memory exhausted"));
  yyresult = 2;
  goto yyreturnlab;


/*----------------------------------------------------------.
| yyreturnlab -- parsing is finished, clean up and return.  |
`----------------------------------------------------------*/
yyreturnlab:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, &yylloc, p);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp, p);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, yylsp, p);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
  return yyresult;
}

#line 5009 "mrbgems/mruby-compiler/core/parse.y"

#define pylval  (*((YYSTYPE*)(p->ylval)))

static void
yyerror(void *lp, parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_NO_STDIO
    if (p->filename_sym) {
      const char *filename = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
      fprintf(stderr, "%s:%d:%d: %s\n", filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nerr < sizeof(p->error_buffer) / sizeof(p->error_buffer[0])) {
    n = strlen(s);
    c = (char*)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->error_buffer[p->nerr].message = c;
    p->error_buffer[p->nerr].lineno = p->lineno;
    p->error_buffer[p->nerr].column = p->column;
  }
  p->nerr++;
}

static void
yyerror_c(parser_state *p, const char *msg, char c)
{
  char buf[256];

  strncpy(buf, msg, sizeof(buf) - 2);
  buf[sizeof(buf) - 2] = '\0';
  strncat(buf, &c, 1);
  yyerror(NULL, p, buf);
}

static void
yywarning(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_NO_STDIO
    if (p->filename_sym) {
      const char *filename = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
      fprintf(stderr, "%s:%d:%d: warning: %s\n", filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: warning: %s\n", p->lineno, p->column, s);
    }
#endif
  }
  else if (p->nwarn < sizeof(p->warn_buffer) / sizeof(p->warn_buffer[0])) {
    n = strlen(s);
    c = (char*)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->warn_buffer[p->nwarn].message = c;
    p->warn_buffer[p->nwarn].lineno = p->lineno;
    p->warn_buffer[p->nwarn].column = p->column;
  }
  p->nwarn++;
}

static void
yywarning_s(parser_state *p, const char *msg, const char *s)
{
  char buf[256];

  strncpy(buf, msg, sizeof(buf) - 1);
  buf[sizeof(buf) - 1] = '\0';
  strncat(buf, ": ", sizeof(buf) - strlen(buf) - 1);
  strncat(buf, s, sizeof(buf) - strlen(buf) - 1);
  yywarning(p, buf);
}

static void
backref_error(parser_state *p, node *n)
{
  int c;

  c = node_to_int(n->car);

  if (c == NODE_NTH_REF) {
    yyerror_c(p, "can't set variable $", (char)node_to_int(n->cdr)+'0');
  }
  else if (c == NODE_BACK_REF) {
    yyerror_c(p, "can't set variable $", (char)node_to_int(n->cdr));
  }
  else {
    yyerror(NULL, p, "Internal error in backref_error()");
  }
}

static void
void_expr_error(parser_state *p, node *n)
{
  if (n == NULL) return;

  /* Check if this is a variable-sized node first */
  struct mrb_ast_var_header *header = (struct mrb_ast_var_header*)n;
  if (header) {
    /* Handle variable-sized nodes */
    switch ((enum node_type)header->node_type) {
    case NODE_BREAK:
    case NODE_RETURN:
    case NODE_NEXT:
    case NODE_REDO:
    case NODE_RETRY:
      yyerror(NULL, p, "void value expression");
      return;
    case NODE_AND:
    case NODE_OR:
      {
        struct mrb_ast_and_node *and_n = (struct mrb_ast_and_node*)n;
        void_expr_error(p, (node*)and_n->left);
        void_expr_error(p, (node*)and_n->right);
      }
      return;
    case NODE_STMTS:
      {
        struct mrb_ast_stmts_node *stmts = (struct mrb_ast_stmts_node*)n;
        node *last = stmts->stmts;
        if (last) {
          /* Find the last statement in the cons list */
          while (last->cdr) {
            last = last->cdr;
          }
          void_expr_error(p, last->car);
        }
      }
      return;
    case NODE_BEGIN:
      {
        struct mrb_ast_begin_node *begin_n = (struct mrb_ast_begin_node*)n;
        if (begin_n->body) {
          void_expr_error(p, (node*)begin_n->body);
        }
      }
      return;
    default:
      /* Other variable-sized nodes are OK */
      return;
    }
  }

  /* Should not reach here - all nodes should be variable-sized now */
}

static void pushback(parser_state *p, int c);
static mrb_bool peeks(parser_state *p, const char *s);
static mrb_bool skips(parser_state *p, const char *s);

static inline int
nextc0(parser_state *p)
{
  if (p->s && p->s < p->send) {
    return (unsigned char)*p->s++;
  }
  else {
#ifndef MRB_NO_STDIO
    int c;

    if (p->f) {
      c = fgetc(p->f);
      if (!feof(p->f)) return c;
    }
#endif
    return -1;
  }
}

static inline int
nextc(parser_state *p)
{
  int c;

  if (p->pb) {
    node *tmp;

    c = node_to_int(p->pb->car);
    tmp = p->pb;
    p->pb = p->pb->cdr;
    cons_free(tmp);
  }
  else {
    c = nextc0(p);
    if (c < 0) goto eof;
  }
  if (c >= 0) {
    p->column++;
  }
  if (c == '\r') {
    const int lf = nextc0(p);
    if (lf == '\n') {
      return '\n';
    }
    if (lf > 0) pushback(p, lf);
  }
  return c;

  eof:
  if (!p->cxt) return -1;
  else {
    if (p->cxt->partial_hook(p) < 0)
      return -1;                /* end of program(s) */
    return -2;                  /* end of a file in the program files */
  }
}

static void
pushback(parser_state *p, int c)
{
  if (c >= 0) {
    p->column--;
  }
  p->pb = cons(int_to_node(c), p->pb);
}

static void
skip(parser_state *p, char term)
{
  int c;

  for (;;) {
    c = nextc(p);
    if (c < 0) break;
    if (c == term) break;
  }
}

static int
peekc_n(parser_state *p, int n)
{
  node *list = 0;
  int c0;

  do {
    c0 = nextc(p);
    if (c0 == -1) return c0;    /* do not skip partial EOF */
    if (c0 >= 0) --p->column;
    list = push(list, int_to_node(c0));
  } while(n--);
  if (p->pb) {
    p->pb = append(list, p->pb);
  }
  else {
    p->pb = list;
  }
  return c0;
}

static mrb_bool
peek_n(parser_state *p, int c, int n)
{
  return peekc_n(p, n) == c && c >= 0;
}
#define peek(p,c) peek_n((p), (c), 0)

static mrb_bool
peeks(parser_state *p, const char *s)
{
  size_t len = strlen(s);

#ifndef MRB_NO_STDIO
  if (p->f) {
    int n = 0;
    while (*s) {
      if (!peek_n(p, *s++, n++)) return FALSE;
    }
    return TRUE;
  }
  else
#endif
    if (p->s && p->s + len <= p->send) {
      if (memcmp(p->s, s, len) == 0) return TRUE;
    }
  return FALSE;
}

static mrb_bool
skips(parser_state *p, const char *s)
{
  int c;

  for (;;) {
    /* skip until first char */
    for (;;) {
      c = nextc(p);
      if (c < 0) return FALSE;
      if (c == '\n') {
        p->lineno++;
        p->column = 0;
      }
      if (c == *s) break;
    }
    s++;
    if (peeks(p, s)) {
      size_t len = strlen(s);

      while (len--) {
        if (nextc(p) == '\n') {
          p->lineno++;
          p->column = 0;
        }
      }
      return TRUE;
    }
    else {
      s--;
    }
  }
  return FALSE;
}

static int
newtok(parser_state *p)
{
  if (p->tokbuf != p->buf) {
    mrbc_free(p->tokbuf);
    p->tokbuf = p->buf;
    p->tsiz = MRB_PARSER_TOKBUF_SIZE;
  }
  p->tidx = 0;
  return p->column - 1;
}

static void
tokadd(parser_state *p, int32_t c)
{
  char utf8[4];
  int i, len;

  /* mrb_assert(-0x10FFFF <= c && c <= 0xFF); */
  if (c >= 0) {
    /* Single byte from source or non-Unicode escape */
    utf8[0] = (char)c;
    len = 1;
  }
  else {
    /* Unicode character */
    c = -c;
    if (c < 0x80) {
      utf8[0] = (char)c;
      len = 1;
    }
    else if (c < 0x800) {
      utf8[0] = (char)(0xC0 | (c >> 6));
      utf8[1] = (char)(0x80 | (c & 0x3F));
      len = 2;
    }
    else if (c < 0x10000) {
      utf8[0] = (char)(0xE0 |  (c >> 12)        );
      utf8[1] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[2] = (char)(0x80 | ( c        & 0x3F));
      len = 3;
    }
    else {
      utf8[0] = (char)(0xF0 |  (c >> 18)        );
      utf8[1] = (char)(0x80 | ((c >> 12) & 0x3F));
      utf8[2] = (char)(0x80 | ((c >>  6) & 0x3F));
      utf8[3] = (char)(0x80 | ( c        & 0x3F));
      len = 4;
    }
  }
  if (p->tidx+len >= p->tsiz) {
    if (p->tsiz >= MRB_PARSER_TOKBUF_MAX) {
      p->tidx += len;
      return;
    }
    p->tsiz *= 2;
    if (p->tokbuf == p->buf) {
      p->tokbuf = (char*)mrbc_malloc(p->tsiz);
      memcpy(p->tokbuf, p->buf, MRB_PARSER_TOKBUF_SIZE);
    }
    else {
      p->tokbuf = (char*)mrbc_realloc(p->tokbuf, p->tsiz);
    }
  }
  for (i = 0; i < len; i++) {
    p->tokbuf[p->tidx++] = utf8[i];
  }
}

static int
toklast(parser_state *p)
{
  return p->tokbuf[p->tidx-1];
}

static void
tokfix(parser_state *p)
{
  if (p->tidx >= MRB_PARSER_TOKBUF_MAX) {
    p->tidx = MRB_PARSER_TOKBUF_MAX-1;
    yyerror(NULL, p, "string too long (truncated)");
  }
  p->tokbuf[p->tidx] = '\0';
}

static const char*
tok(parser_state *p)
{
  return p->tokbuf;
}

static int
toklen(parser_state *p)
{
  return p->tidx;
}

#define IS_ARG() (p->lstate == EXPR_ARG || p->lstate == EXPR_CMDARG)
#define IS_END() (p->lstate == EXPR_END || p->lstate == EXPR_ENDARG || p->lstate == EXPR_ENDFN)
#define IS_BEG() (p->lstate == EXPR_BEG || p->lstate == EXPR_MID || p->lstate == EXPR_VALUE || p->lstate == EXPR_CLASS)
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() ((p->lstate == EXPR_BEG && !cmd_state) || IS_ARG() || p->lstate == EXPR_VALUE)
#define IS_LABEL_SUFFIX(n) (peek_n(p, ':',(n)) && !peek_n(p, ':', (n)+1))

static int32_t
scan_oct(const int *start, int len, int *retlen)
{
  const int *s = start;
  int32_t retval = 0;

  /* mrb_assert(len <= 3) */
  while (len-- && *s >= '0' && *s <= '7') {
    retval <<= 3;
    retval |= *s++ - '0';
  }
  *retlen = (int)(s - start);

  return retval;
}

static int32_t
scan_hex(parser_state *p, const int *start, int len, int *retlen)
{
  static const char hexdigit[] = "0123456789abcdef0123456789ABCDEF";
  const int *s = start;
  uint32_t retval = 0;
  char *tmp;

  /* mrb_assert(len <= 8) */
  while (len-- && *s && (tmp = (char*)strchr(hexdigit, *s))) {
    retval <<= 4;
    retval |= (tmp - hexdigit) & 15;
    s++;
  }
  *retlen = (int)(s - start);

  return (int32_t)retval;
}

static int32_t
read_escape_unicode(parser_state *p, int limit)
{
  int buf[9];
  int i;
  int32_t hex;

  /* Look for opening brace */
  i = 0;
  buf[0] = nextc(p);
  if (buf[0] < 0) {
  eof:
    yyerror(NULL, p, "invalid escape character syntax");
    return -1;
  }
  if (ISXDIGIT(buf[0])) {
    /* \uxxxx form */
    for (i=1; i<limit; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
  }
  else {
    pushback(p, buf[0]);
  }
  hex = scan_hex(p, buf, i, &i);
  if (i == 0 || hex > 0x10FFFF || (hex & 0xFFFFF800) == 0xD800) {
    yyerror(NULL, p, "invalid Unicode code point");
    return -1;
  }
  return hex;
}

/* Return negative to indicate Unicode code point */
static int32_t
read_escape(parser_state *p)
{
  int32_t c;

  switch (c = nextc(p)) {
  case '\\':/* Backslash */
    return c;

  case 'n':/* newline */
    return '\n';

  case 't':/* horizontal tab */
    return '\t';

  case 'r':/* carriage-return */
    return '\r';

  case 'f':/* form-feed */
    return '\f';

  case 'v':/* vertical tab */
    return '\13';

  case 'a':/* alarm(bell) */
    return '\007';

  case 'e':/* escape */
    return 033;

  case '0': case '1': case '2': case '3': /* octal constant */
  case '4': case '5': case '6': case '7':
  {
    int buf[3];
    int i;

    buf[0] = c;
    for (i=1; i<3; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (buf[i] < '0' || '7' < buf[i]) {
        pushback(p, buf[i]);
        break;
      }
    }
    c = scan_oct(buf, i, &i);
  }
  return c;

  case 'x':     /* hex constant */
  {
    int buf[2];
    int i;

    for (i=0; i<2; i++) {
      buf[i] = nextc(p);
      if (buf[i] < 0) goto eof;
      if (!ISXDIGIT(buf[i])) {
        pushback(p, buf[i]);
        break;
      }
    }
    if (i == 0) {
      yyerror(NULL, p, "invalid hex escape");
      return -1;
    }
    return scan_hex(p, buf, i, &i);
  }

  case 'u':     /* Unicode */
    if (peek(p, '{')) {
      /* \u{xxxxxxxx} form */
      nextc(p);
      c = read_escape_unicode(p, 8);
      if (c < 0) return 0;
      if (nextc(p) != '}') goto eof;
    }
    else {
      c = read_escape_unicode(p, 4);
      if (c < 0) return 0;
    }
    return -c;

  case 'b':/* backspace */
    return '\010';

  case 's':/* space */
    return ' ';

  case 'M':
    if ((c = nextc(p)) != '-') {
      yyerror(NULL, p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
    if ((c = nextc(p)) == '\\') {
      return read_escape(p) | 0x80;
    }
    else if (c < 0) goto eof;
    else {
      return ((c & 0xff) | 0x80);
    }

  case 'C':
    if ((c = nextc(p)) != '-') {
      yyerror(NULL, p, "Invalid escape character syntax");
      pushback(p, c);
      return '\0';
    }
  case 'c':
    if ((c = nextc(p))== '\\') {
      c = read_escape(p);
    }
    else if (c == '?')
      return 0177;
    else if (c < 0) goto eof;
    return c & 0x9f;

    eof:
  case -1:
  case -2:                      /* end of a file */
    yyerror(NULL, p, "Invalid escape character syntax");
    return '\0';

  default:
    return c;
  }
}

static void
heredoc_count_indent(parser_heredoc_info *hinfo, const char *str, size_t len, size_t spaces, size_t *offset)
{
  size_t indent = 0;
  *offset = 0;
  for (size_t i = 0; i < len; i++) {
    size_t size;
    if (str[i] == '\n')
      break;
    else if (str[i] == '\t')
      size = 8;
    else if (ISSPACE(str[i]))
      size = 1;
    else
      break;
    size_t nindent = indent + size;
    if (nindent > spaces || nindent > hinfo->indent)
      break;
    indent = nindent;
    *offset += 1;
  }
}

static void
heredoc_remove_indent(parser_state *p, parser_heredoc_info *hinfo)
{
  if (!hinfo->remove_indent || hinfo->indent == 0)
    return;
  node *indented, *n, *pair, *escaped, *nspaces;
  const char *str;
  size_t len, spaces, offset, start, end;
  indented = hinfo->indented;
  while (indented) {
    n = indented->car;
    pair = n->car;
    len = (size_t)pair->car;
    str = (char*)pair->cdr;
    escaped = n->cdr->car;
    nspaces = n->cdr->cdr;
    if (escaped) {
      char *newstr = strndup(str, len);
      size_t newlen = 0;
      start = 0;
      while (start < len) {
        end = escaped ? (size_t)escaped->car : len;
        if (end > len) end = len;
        spaces = (size_t)nspaces->car;
        size_t esclen = end - start;
        heredoc_count_indent(hinfo, str + start, esclen, spaces, &offset);
        esclen -= offset;
        memcpy(newstr + newlen, str + start + offset, esclen);
        newlen += esclen;
        start = end;
        if (escaped)
          escaped = escaped->cdr;
        nspaces = nspaces->cdr;
      }
      if (newlen < len)
        newstr[newlen] = '\0';
      pair->car = (node*)newlen;
      pair->cdr = (node*)newstr;
    }
    else {
      spaces = (size_t)nspaces->car;
      heredoc_count_indent(hinfo, str, len, spaces, &offset);
      pair->car = (node*)(len - offset);
      pair->cdr = (node*)(str + offset);
    }
    indented = indented->cdr;
  }
}

static void
heredoc_push_indented(parser_state *p, parser_heredoc_info *hinfo, node *pair, node *escaped, node *nspaces, mrb_bool empty_line)
{
  hinfo->indented = push(hinfo->indented, cons(pair, cons(escaped, nspaces)));
  while (nspaces) {
    size_t tspaces = (size_t)nspaces->car;
    if ((hinfo->indent == ~0U || tspaces < hinfo->indent) && !empty_line)
      hinfo->indent = tspaces;
    nspaces = nspaces->cdr;
  }
}

static int
parse_string(parser_state *p)
{
  int c;
  string_type type = (string_type)p->lex_strterm->type;
  int nest_level = p->lex_strterm->level;
  int beg = p->lex_strterm->paren;
  int end = p->lex_strterm->term;
  parser_heredoc_info *hinfo = (type & STR_FUNC_HEREDOC) ? parsing_heredoc_info(p) : NULL;

  mrb_bool unindent = hinfo && hinfo->remove_indent;
  mrb_bool head = hinfo && hinfo->line_head;
  mrb_bool empty = TRUE;
  size_t spaces = 0;
  size_t pos = -1;
  node *escaped = NULL;
  node *nspaces = NULL;

  if (beg == 0) beg = -3;       /* should never happen */
  if (end == 0) end = -3;
  newtok(p);
  while ((c = nextc(p)) != end || nest_level != 0) {
    pos++;
    if (hinfo && (c == '\n' || c < 0)) {
      mrb_bool line_head;
      tokadd(p, '\n');
      tokfix(p);
      p->lineno++;
      p->column = 0;
      line_head = hinfo->line_head;
      hinfo->line_head = TRUE;
      if (line_head) {
        /* check whether end of heredoc */
        const char *s = tok(p);
        int len = toklen(p);
        if (hinfo->allow_indent) {
          while (ISSPACE(*s) && len > 0) {
            s++;
            len--;
          }
        }
        if (hinfo->term_len > 0 && len-1 == hinfo->term_len && strncmp(s, hinfo->term, len-1) == 0) {
          heredoc_remove_indent(p, hinfo);
          return tHEREDOC_END;
        }
      }
      if (c < 0) {
        char buf[256];
        const char s1[] = "can't find heredoc delimiter \"";
        const char s2[] = "\" anywhere before EOF";

        if (sizeof(s1)+sizeof(s2)+strlen(hinfo->term)+1 >= sizeof(buf)) {
          yyerror(NULL, p, "can't find heredoc delimiter anywhere before EOF");
        }
        else {
          strcpy(buf, s1);
          strcat(buf, hinfo->term);
          strcat(buf, s2);
          yyerror(NULL, p, buf);
        }
        return 0;
      }
      pylval.nd = new_str_tok(p);
      if (unindent && head) {
        nspaces = push(nspaces, int_to_node(spaces));
        heredoc_push_indented(p, hinfo, pylval.nd, escaped, nspaces, empty && line_head);
      }
      return tHD_STRING_MID;
    }
    if (unindent && empty) {
      if (c == '\t')
        spaces += 8;
      else if (ISSPACE(c))
        spaces++;
      else
        empty = FALSE;
    }
    if (c < 0) {
      yyerror(NULL, p, "unterminated string meets end of file");
      return 0;
    }
    else if (c == beg) {
      nest_level++;
      p->lex_strterm->level = nest_level;
    }
    else if (c == end) {
      nest_level--;
      p->lex_strterm->level = nest_level;
    }
    else if (c == '\\') {
      c = nextc(p);
      if (type & STR_FUNC_EXPAND) {
        if (c == end || c == beg) {
          tokadd(p, c);
        }
        else if (c == '\n') {
          p->lineno++;
          p->column = 0;
          if (unindent) {
            nspaces = push(nspaces, int_to_node(spaces));
            escaped = push(escaped, int_to_node(pos));
            pos--;
            empty = TRUE;
            spaces = 0;
          }
          if (type & STR_FUNC_ARRAY) {
            tokadd(p, '\n');
          }
        }
        else if (type & STR_FUNC_REGEXP) {
          tokadd(p, '\\');
          tokadd(p, c);
        }
        else if (c == 'u' && peek(p, '{')) {
          /* \u{xxxx xxxx xxxx} form */
          nextc(p);
          while (1) {
            do c = nextc(p); while (ISSPACE(c));
            if (c == '}') break;
            pushback(p, c);
            c = read_escape_unicode(p, 8);
            if (c < 0) break;
            tokadd(p, -c);
          }
          if (hinfo)
            hinfo->line_head = FALSE;
        }
        else {
          pushback(p, c);
          tokadd(p, read_escape(p));
          if (hinfo)
            hinfo->line_head = FALSE;
        }
      }
      else {
        if (c != beg && c != end) {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
          }
          if (!(c == '\\' || ((type & STR_FUNC_ARRAY) && ISSPACE(c)))) {
            tokadd(p, '\\');
          }
        }
        tokadd(p, c);
      }
      continue;
    }
    else if ((c == '#') && (type & STR_FUNC_EXPAND)) {
      c = nextc(p);
      if (c == '{') {
        tokfix(p);
        p->lstate = EXPR_BEG;
        p->cmd_start = TRUE;
        pylval.nd = new_str_tok(p);
        if (hinfo) {
          if (unindent && head) {
            nspaces = push(nspaces, int_to_node(spaces));
            heredoc_push_indented(p, hinfo, pylval.nd, escaped, nspaces, FALSE);
          }
          hinfo->line_head = FALSE;
          return tHD_STRING_PART;
        }
        return tSTRING_PART;
      }
      tokadd(p, '#');
      pushback(p, c);
      continue;
    }
    if ((type & STR_FUNC_ARRAY) && ISSPACE(c)) {
      if (toklen(p) == 0) {
        do {
          if (c == '\n') {
            p->lineno++;
            p->column = 0;
            heredoc_treat_nextline(p);
            if (p->parsing_heredoc != NULL) {
              return tHD_LITERAL_DELIM;
            }
          }
          c = nextc(p);
        } while (ISSPACE(c));
        pushback(p, c);
        return tLITERAL_DELIM;
      }
      else {
        pushback(p, c);
        tokfix(p);
        pylval.nd = new_str_tok(p);
        return tSTRING_MID;
      }
    }
    if (c == '\n') {
      p->lineno++;
      p->column = 0;
    }
    tokadd(p, c);
  }

  tokfix(p);
  p->lstate = EXPR_END;
  end_strterm(p);

  if (type & STR_FUNC_XQUOTE) {
    pylval.nd = new_str_tok(p);
    return tXSTRING;
  }

  if (type & STR_FUNC_REGEXP) {
    int f = 0;
    int re_opt;
    int pattern_len = toklen(p);
    char *s = strndup(tok(p), pattern_len);
    char flags[3];
    char *flag = flags;
    char enc = '\0';
    char *encp;
    char *dup;

    newtok(p);
    while (re_opt = nextc(p), re_opt >= 0 && ISALPHA(re_opt)) {
      switch (re_opt) {
      case 'i': f |= 1; break;
      case 'x': f |= 2; break;
      case 'm': f |= 4; break;
      case 'u': f |= 16; break;
      case 'n': f |= 32; break;
      case 'o': break;
      default: tokadd(p, re_opt); break;
      }
    }
    pushback(p, re_opt);
    if (toklen(p)) {
      char msg[128];

      strcpy(msg, "unknown regexp option");
      tokfix(p);
      if (toklen(p) > 1) {
        strcat(msg, "s");
      }
      strcat(msg, " - ");
      strncat(msg, tok(p), sizeof(msg) - strlen(msg) - 1);
      yyerror(NULL, p, msg);
    }
    if (f != 0) {
      if (f & 1) *flag++ = 'i';
      if (f & 2) *flag++ = 'x';
      if (f & 4) *flag++ = 'm';
      if (f & 16) enc = 'u';
      if (f & 32) enc = 'n';
    }
    if (flag > flags) {
      dup = strndup(flags, (size_t)(flag - flags));
    }
    else {
      dup = NULL;
    }
    if (enc) {
      encp = strndup(&enc, 1);
    }
    else {
      encp = NULL;
    }
    pylval.nd = cons(cons(int_to_node(pattern_len), (node*)s), cons((node*)dup, (node*)encp));

    return tREGEXP;
  }
  pylval.nd = new_str_tok(p);

  return tSTRING;
}

static int
number_literal_suffix(parser_state *p)
{
  int c, result = 0;
  node *list = 0;
  int column = p->column;
  int mask = NUM_SUFFIX_R|NUM_SUFFIX_I;

  while ((c = nextc(p)) != -1) {
    list = push(list, int_to_node(c));

    if ((mask & NUM_SUFFIX_I) && c == 'i') {
      result |= (mask & NUM_SUFFIX_I);
      mask &= ~NUM_SUFFIX_I;
      /* r after i, rational of complex is disallowed */
      mask &= ~NUM_SUFFIX_R;
      continue;
    }
    if ((mask & NUM_SUFFIX_R) && c == 'r') {
      result |= (mask & NUM_SUFFIX_R);
      mask &= ~NUM_SUFFIX_R;
      continue;
    }
    if (!ISASCII(c) || ISALPHA(c) || c == '_') {
      p->column = column;
      if (p->pb) {
        p->pb = append(list, p->pb);
      }
      else {
        p->pb = list;
      }
      return 0;
    }
    pushback(p, c);
    break;
  }
  return result;
}

static int
heredoc_identifier(parser_state *p)
{
  int c;
  int type = str_heredoc;
  mrb_bool indent = FALSE;
  mrb_bool squiggly = FALSE;
  mrb_bool quote = FALSE;
  node *newnode;
  parser_heredoc_info *info;

  c = nextc(p);
  if (ISSPACE(c) || c == '=') {
    pushback(p, c);
    return 0;
  }
  if (c == '-' || c == '~') {
    if (c == '-')
      indent = TRUE;
    if (c == '~')
      squiggly = TRUE;
    c = nextc(p);
  }
  if (c == '\'' || c == '"') {
    int term = c;
    if (c == '\'')
      quote = TRUE;
    newtok(p);
    while ((c = nextc(p)) >= 0 && c != term) {
      if (c == '\n') {
        c = -1;
        break;
      }
      tokadd(p, c);
    }
    if (c < 0) {
      yyerror(NULL, p, "unterminated here document identifier");
      return 0;
    }
  }
  else {
    if (c < 0) {
      return 0;                 /* missing here document identifier */
    }
    if (! identchar(c)) {
      pushback(p, c);
      if (indent) pushback(p, '-');
      if (squiggly) pushback(p, '~');
      return 0;
    }
    newtok(p);
    do {
      tokadd(p, c);
    } while ((c = nextc(p)) >= 0 && identchar(c));
    pushback(p, c);
  }
  tokfix(p);
  newnode = new_heredoc(p, &info);
  info->term = strndup(tok(p), toklen(p));
  info->term_len = toklen(p);
  if (! quote)
    type |= STR_FUNC_EXPAND;
  info->type = (string_type)type;
  info->allow_indent = indent || squiggly;
  info->remove_indent = squiggly;
  info->indent = ~0U;
  info->indented = NULL;
  info->line_head = TRUE;
  info->doc = NULL;
  p->heredocs_from_nextline = push(p->heredocs_from_nextline, newnode);
  p->lstate = EXPR_END;

  pylval.nd = newnode;
  return tHEREDOC_BEG;
}

static int
arg_ambiguous(parser_state *p)
{
  yywarning(p, "ambiguous first argument; put parentheses or even spaces");
  return 1;
}

#include "lex.def"

static int
parser_yylex(parser_state *p)
{
  int32_t c;
  int nlines = 1;
  int space_seen = 0;
  int cmd_state;
  enum mrb_lex_state_enum last_state;
  int token_column;

  if (p->lex_strterm) {
    if (is_strterm_type(p, STR_FUNC_HEREDOC)) {
      if (p->parsing_heredoc != NULL)
        return parse_string(p);
    }
    else
      return parse_string(p);
  }
  cmd_state = p->cmd_start;
  p->cmd_start = FALSE;
  retry:
  last_state = p->lstate;
  switch (c = nextc(p)) {
  case '\004':  /* ^D */
  case '\032':  /* ^Z */
  case '\0':    /* NUL */
  case -1:      /* end of script. */
    if (p->heredocs_from_nextline)
      goto maybe_heredoc;
    return 0;

  /* white spaces */
  case ' ': case '\t': case '\f': case '\r':
  case '\13':   /* '\v' */
    space_seen = 1;
    goto retry;

  case '#':     /* it's a comment */
    skip(p, '\n');
    /* fall through */
  case -2:      /* end of a file */
  case '\n':
  maybe_heredoc:
    heredoc_treat_nextline(p);
    p->column = 0;
    switch (p->lstate) {
    case EXPR_BEG:
    case EXPR_FNAME:
    case EXPR_DOT:
    case EXPR_CLASS:
    case EXPR_VALUE:
      p->lineno++;
      if (p->parsing_heredoc != NULL) {
        if (p->lex_strterm) {
          return parse_string(p);
        }
      }
      goto retry;
    default:
      break;
    }
    if (p->parsing_heredoc != NULL) {
      pylval.num = nlines;
      return '\n';
    }
    while ((c = nextc(p))) {
      switch (c) {
      case ' ': case '\t': case '\f': case '\r':
      case '\13': /* '\v' */
        space_seen = 1;
        break;
      case '#': /* comment as a whitespace */
        skip(p, '\n');
        nlines++;
        break;
      case '.':
        if (!peek(p, '.')) {
          pushback(p, '.');
          p->lineno+=nlines; nlines=1;
          goto retry;
        }
        pushback(p, c);
        goto normal_newline;
      case '&':
        if (peek(p, '.')) {
          pushback(p, '&');
          p->lineno+=nlines; nlines=1;
          goto retry;
        }
        pushback(p, c);
        goto normal_newline;
      case -1:                  /* EOF */
      case -2:                  /* end of a file */
        goto normal_newline;
      default:
        pushback(p, c);
        goto normal_newline;
      }
    }
  normal_newline:
    p->cmd_start = TRUE;
    p->lstate = EXPR_BEG;
    pylval.num = nlines;
    return '\n';

  case '*':
    if ((c = nextc(p)) == '*') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern_op(pow);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      if (IS_SPCARG(c)) {
        yywarning(p, "'**' interpreted as argument prefix");
        c = tDSTAR;
      }
      else if (IS_BEG()) {
        c = tDSTAR;
      }
      else {
        c = tPOW; /* "**", "argument prefix" */
      }
    }
    else {
      if (c == '=') {
        pylval.id = intern_op(mul);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      if (IS_SPCARG(c)) {
        yywarning(p, "'*' interpreted as argument prefix");
        c = tSTAR;
      }
      else if (IS_BEG()) {
        c = tSTAR;
      }
      else {
        c = '*';
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '!':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return '!';
      }
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if (c == '=') {
      return tNEQ;
    }
    if (c == '~') {
      return tNMATCH;
    }
    pushback(p, c);
    return '!';

  case '=':
    if (p->column == 1) {
      static const char begin[] = "begin";
      static const char end[] = "\n=end";
      if (peeks(p, begin)) {
        c = peekc_n(p, sizeof(begin)-1);
        if (c < 0 || ISSPACE(c)) {
          do {
            if (!skips(p, end)) {
              yyerror(NULL, p, "embedded document meets end of file");
              return 0;
            }
            c = nextc(p);
          } while (!(c < 0 || ISSPACE(c)));
          if (c != '\n') skip(p, '\n');
          p->lineno+=nlines; nlines=1;
          p->column = 0;
          goto retry;
        }
      }
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      if ((c = nextc(p)) == '=') {
        return tEQQ;
      }
      pushback(p, c);
      return tEQ;
    }
    if (c == '~') {
      return tMATCH;
    }
    else if (c == '>') {
      return tASSOC;
    }
    pushback(p, c);
    return '=';

  case '<':
    c = nextc(p);
    if (c == '<' &&
        p->lstate != EXPR_DOT &&
        p->lstate != EXPR_CLASS &&
        !IS_END() &&
        (!IS_ARG() || space_seen)) {
      int token = heredoc_identifier(p);
      if (token)
        return token;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
      if (p->lstate == EXPR_CLASS) {
        p->cmd_start = TRUE;
      }
    }
    if (c == '=') {
      if ((c = nextc(p)) == '>') {
        return tCMP;
      }
      pushback(p, c);
      return tLEQ;
    }
    if (c == '<') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern_op(lshift);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tLSHFT;
    }
    pushback(p, c);
    return '<';

  case '>':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    if ((c = nextc(p)) == '=') {
      return tGEQ;
    }
    if (c == '>') {
      if ((c = nextc(p)) == '=') {
        pylval.id = intern_op(rshift);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tRSHFT;
    }
    pushback(p, c);
    return '>';

  case '"':
    p->lex_strterm = new_strterm(p, str_dquote, '"', 0);
    return tSTRING_BEG;

  case '\'':
    p->lex_strterm = new_strterm(p, str_squote, '\'', 0);
    return parse_string(p);

  case '`':
    if (p->lstate == EXPR_FNAME) {
      p->lstate = EXPR_ENDFN;
      return '`';
    }
    if (p->lstate == EXPR_DOT) {
      if (cmd_state)
        p->lstate = EXPR_CMDARG;
      else
        p->lstate = EXPR_ARG;
      return '`';
    }
    p->lex_strterm = new_strterm(p, str_xquote, '`', 0);
    return tXSTRING_BEG;

  case '?':
    if (IS_END()) {
      p->lstate = EXPR_VALUE;
      return '?';
    }
    c = nextc(p);
    if (c < 0) {
      yyerror(NULL, p, "incomplete character syntax");
      return 0;
    }
    if (ISSPACE(c)) {
      if (!IS_ARG()) {
        int c2;
        switch (c) {
        case ' ':
          c2 = 's';
          break;
        case '\n':
          c2 = 'n';
          break;
        case '\t':
          c2 = 't';
          break;
        case '\v':
          c2 = 'v';
          break;
        case '\r':
          c2 = 'r';
          break;
        case '\f':
          c2 = 'f';
          break;
        default:
          c2 = 0;
          break;
        }
        if (c2) {
          char buf[256];
          char cc[] = { (char)c2, '\0' };

          strcpy(buf, "invalid character syntax; use ?\\");
          strncat(buf, cc, 2);
          yyerror(NULL, p, buf);
        }
      }
      ternary:
      pushback(p, c);
      p->lstate = EXPR_VALUE;
      return '?';
    }
    newtok(p);
    /* need support UTF-8 if configured */
    if ((ISALNUM(c) || c == '_')) {
      int c2 = nextc(p);
      pushback(p, c2);
      if ((ISALNUM(c2) || c2 == '_')) {
        goto ternary;
      }
    }
    if (c == '\\') {
      c = read_escape(p);
      tokadd(p, c);
    }
    else {
      tokadd(p, c);
    }
    tokfix(p);
    pylval.nd = new_str_tok(p);
    p->lstate = EXPR_END;
    return tCHAR;

  case '&':
    if ((c = nextc(p)) == '&') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        pylval.id = intern_op(andand);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tANDOP;
    }
    else if (c == '.') {
      p->lstate = EXPR_DOT;
      return tANDDOT;
    }
    else if (c == '=') {
      pylval.id = intern_op(and);
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      yywarning(p, "'&' interpreted as argument prefix");
      c = tAMPER;
    }
    else if (IS_BEG()) {
      c = tAMPER;
    }
    else {
      c = '&';
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return c;

  case '|':
    if ((c = nextc(p)) == '|') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
        pylval.id = intern_op(oror);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tOROP;
    }
    if (c == '=') {
      pylval.id = intern_op(or);
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '|';

  case '+':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUPLUS;
      }
      pushback(p, c);
      return '+';
    }
    if (c == '=') {
      pylval.id = intern_op(add);
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        c = '+';
        goto start_num;
      }
      return tUPLUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '+';

  case '-':
    c = nextc(p);
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      if (c == '@') {
        return tUMINUS;
      }
      pushback(p, c);
      return '-';
    }
    if (c == '=') {
      pylval.id = intern_op(sub);
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (c == '>') {
      p->lstate = EXPR_ENDFN;
      return tLAMBDA;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        return tUMINUS_NUM;
      }
      return tUMINUS;
    }
    p->lstate = EXPR_BEG;
    pushback(p, c);
    return '-';

  case '.':
    {
      int is_beg = IS_BEG();
      p->lstate = EXPR_MID;
      if ((c = nextc(p)) == '.') {
        if ((c = nextc(p)) == '.') {
          return is_beg ? tBDOT3 : tDOT3;
        }
        pushback(p, c);
        return is_beg ? tBDOT2 : tDOT2;
      }
      pushback(p, c);
      p->lstate = EXPR_BEG;
      if (c >= 0 && ISDIGIT(c)) {
        yyerror(NULL, p, "no .<digit> floating literal anymore; put 0 before dot");
      }
      p->lstate = EXPR_DOT;
      return '.';
    }

    start_num:
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
  {
    int is_float, seen_point, seen_e, nondigit;
    int suffix = 0;

    is_float = seen_point = seen_e = nondigit = 0;
    p->lstate = EXPR_END;
    newtok(p);
    if (c == '-') {
      tokadd(p, c);
      c = nextc(p);
    }
    else if (c == '+') {
      c = nextc(p);
    }
    if (c == '0') {
#define no_digits() do {yyerror(NULL, p,"numeric literal without digits"); return 0;} while (0)
      int start = toklen(p);
      c = nextc(p);
      if (c == 'x' || c == 'X') {
        /* hexadecimal */
        c = nextc(p);
        if (c >= 0 && ISXDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISXDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, tolower(c));
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        suffix = number_literal_suffix(p);
        pylval.nd = new_int(p, tok(p), 16, suffix);
        return tINTEGER;
      }
      if (c == 'b' || c == 'B') {
        /* binary */
        c = nextc(p);
        if (c == '0' || c == '1') {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (c != '0' && c != '1') break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        suffix = number_literal_suffix(p);
        pylval.nd = new_int(p, tok(p), 2, suffix);
        return tINTEGER;
      }
      if (c == 'd' || c == 'D') {
        /* decimal */
        c = nextc(p);
        if (c >= 0 && ISDIGIT(c)) {
          do {
            if (c == '_') {
              if (nondigit) break;
              nondigit = c;
              continue;
            }
            if (!ISDIGIT(c)) break;
            nondigit = 0;
            tokadd(p, c);
          } while ((c = nextc(p)) >= 0);
        }
        pushback(p, c);
        tokfix(p);
        if (toklen(p) == start) {
          no_digits();
        }
        else if (nondigit) goto trailing_uc;
        suffix = number_literal_suffix(p);
        pylval.nd = new_int(p, tok(p), 10, suffix);
        return tINTEGER;
      }
      if (c == '_') {
        /* 0_0 */
        goto octal_number;
      }
      if (c == 'o' || c == 'O') {
        /* prefixed octal */
        c = nextc(p);
        if (c < 0 || c == '_' || !ISDIGIT(c)) {
          no_digits();
        }
      }
      if (c >= '0' && c <= '7') {
        /* octal */
        octal_number:
        do {
          if (c == '_') {
            if (nondigit) break;
            nondigit = c;
            continue;
          }
          if (c < '0' || c > '9') break;
          if (c > '7') goto invalid_octal;
          nondigit = 0;
          tokadd(p, c);
        } while ((c = nextc(p)) >= 0);

        if (toklen(p) > start) {
          pushback(p, c);
          tokfix(p);
          if (nondigit) goto trailing_uc;
          suffix = number_literal_suffix(p);
          pylval.nd = new_int(p, tok(p), 8, suffix);
          return tINTEGER;
        }
        if (nondigit) {
          pushback(p, c);
          goto trailing_uc;
        }
      }
      if (c > '7' && c <= '9') {
        invalid_octal:
        yyerror(NULL, p, "Invalid octal digit");
      }
      else if (c == '.' || c == 'e' || c == 'E') {
        tokadd(p, '0');
      }
      else {
        pushback(p, c);
        suffix = number_literal_suffix(p);
        pylval.nd = new_int(p, "0", 10, suffix);
        return tINTEGER;
      }
    }

    for (;;) {
      switch (c) {
      case '0': case '1': case '2': case '3': case '4':
      case '5': case '6': case '7': case '8': case '9':
        nondigit = 0;
        tokadd(p, c);
        break;

      case '.':
        if (nondigit) goto trailing_uc;
        if (seen_point || seen_e) {
          goto decode_num;
        }
        else {
          int c0 = nextc(p);
          if (c0 < 0 || !ISDIGIT(c0)) {
            pushback(p, c0);
            goto decode_num;
          }
          c = c0;
        }
        tokadd(p, '.');
        tokadd(p, c);
        is_float++;
        seen_point++;
        nondigit = 0;
        break;

      case 'e':
      case 'E':
        if (nondigit) {
          pushback(p, c);
          c = nondigit;
          goto decode_num;
        }
        if (seen_e) {
          goto decode_num;
        }
        tokadd(p, c);
        seen_e++;
        is_float++;
        nondigit = c;
        c = nextc(p);
        if (c != '-' && c != '+') continue;
        tokadd(p, c);
        nondigit = c;
        break;

      case '_':       /* '_' in number just ignored */
        if (nondigit) goto decode_num;
        nondigit = c;
        break;

      default:
        goto decode_num;
      }
      c = nextc(p);
    }

    decode_num:
    pushback(p, c);
    if (nondigit) {
      trailing_uc:
      yyerror_c(p, "trailing non digit in number: ", (char)nondigit);
    }
    tokfix(p);
    if (is_float) {
#ifdef MRB_NO_FLOAT
      yywarning_s(p, "floating-point numbers are not supported", tok(p));
      pylval.nd = new_int(p, "0", 10, 0);
      return tINTEGER;
#else
      double d;

      if (!mrb_read_float(tok(p), NULL, &d)) {
        yywarning_s(p, "corrupted float value", tok(p));
      }
      suffix = number_literal_suffix(p);
      if (seen_e && (suffix & NUM_SUFFIX_R)) {
        pushback(p, 'r');
        suffix &= ~NUM_SUFFIX_R;
      }
      pylval.nd = new_float(p, tok(p), suffix);
      return tFLOAT;
#endif
    }
    suffix = number_literal_suffix(p);
    pylval.nd = new_int(p, tok(p), 10, suffix);
    return tINTEGER;
  }

  case ')':
  case ']':
    p->paren_nest--;
    /* fall through */
  case '}':
    COND_LEXPOP();
    CMDARG_LEXPOP();
    if (c == ')')
      p->lstate = EXPR_ENDFN;
    else
      p->lstate = EXPR_END;
    return c;

  case ':':
    c = nextc(p);
    if (c == ':') {
      if (IS_BEG() || p->lstate == EXPR_CLASS || IS_SPCARG(-1)) {
        p->lstate = EXPR_BEG;
        return tCOLON3;
      }
      p->lstate = EXPR_DOT;
      return tCOLON2;
    }
    if (!space_seen && IS_END()) {
      pushback(p, c);
      /* In pattern matching context, use EXPR_ARG so newlines are significant */
      p->lstate = p->in_kwarg ? EXPR_ARG : EXPR_BEG;
      return tLABEL_TAG;
    }
    if (IS_END() || ISSPACE(c) || c == '#') {
      pushback(p, c);
      p->lstate = EXPR_BEG;
      return ':';
    }
    pushback(p, c);
    p->lstate = EXPR_FNAME;
    return tSYMBEG;

  case '/':
    if (IS_BEG()) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_op(div);
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
      return tREGEXP_BEG;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '/';

  case '^':
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_op(xor);
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '^';

  case ';':
    p->lstate = EXPR_BEG;
    return ';';

  case ',':
    p->lstate = EXPR_BEG;
    return ',';

  case '~':
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      if ((c = nextc(p)) != '@') {
        pushback(p, c);
      }
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    return '~';

  case '(':
    if (IS_BEG()) {
      c = tLPAREN;
    }
    else if (IS_SPCARG(-1)) {
      c = tLPAREN_ARG;
    }
    else if (p->lstate == EXPR_END && space_seen) {
      c = tLPAREN_ARG;
    }
    p->paren_nest++;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '[':
    p->paren_nest++;
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
      p->paren_nest--;
      if ((c = nextc(p)) == ']') {
        if ((c = nextc(p)) == '=') {
          return tASET;
        }
        pushback(p, c);
        return tAREF;
      }
      pushback(p, c);
      return '[';
    }
    else if (IS_BEG()) {
      c = tLBRACK;
    }
    else if (IS_ARG() && space_seen) {
      c = tLBRACK;
    }
    p->lstate = EXPR_BEG;
    COND_PUSH(0);
    CMDARG_PUSH(0);
    return c;

  case '{':
    if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
      p->lstate = EXPR_BEG;
      p->lpar_beg = 0;
      p->paren_nest--;
      COND_PUSH(0);
      CMDARG_PUSH(0);
      return tLAMBEG;
    }
    if (IS_ARG() || p->lstate == EXPR_END || p->lstate == EXPR_ENDFN)
      c = '{';          /* block (primary) */
    else if (p->lstate == EXPR_ENDARG)
      c = tLBRACE_ARG;  /* block (expr) */
    else
      c = tLBRACE;      /* hash */
    COND_PUSH(0);
    CMDARG_PUSH(0);
    p->lstate = EXPR_BEG;
    return c;

  case '\\':
    c = nextc(p);
    if (c == '\n') {
      p->lineno+=nlines; nlines=1;
      p->column = 0;
      space_seen = 1;
      goto retry; /* skip \\n */
    }
    pushback(p, c);
    return '\\';

  case '%':
    if (IS_BEG()) {
      int term;
      int paren;

      c = nextc(p);
      quotation:
      if (c < 0 || !ISALNUM(c)) {
        term = c;
        c = 'Q';
      }
      else {
        term = nextc(p);
        if (ISALNUM(term)) {
          yyerror(NULL, p, "unknown type of %string");
          return 0;
        }
      }
      if (c < 0 || term < 0) {
        yyerror(NULL, p, "unterminated quoted string meets end of file");
        return 0;
      }
      paren = term;
      if (term == '(') term = ')';
      else if (term == '[') term = ']';
      else if (term == '{') term = '}';
      else if (term == '<') term = '>';
      else paren = 0;

      switch (c) {
      case 'Q':
        p->lex_strterm = new_strterm(p, str_dquote, term, paren);
        return tSTRING_BEG;

      case 'q':
        p->lex_strterm = new_strterm(p, str_squote, term, paren);
        return parse_string(p);

      case 'W':
        p->lex_strterm = new_strterm(p, str_dword, term, paren);
        return tWORDS_BEG;

      case 'w':
        p->lex_strterm = new_strterm(p, str_sword, term, paren);
        return tWORDS_BEG;

      case 'x':
        p->lex_strterm = new_strterm(p, str_xquote, term, paren);
        return tXSTRING_BEG;

      case 'r':
        p->lex_strterm = new_strterm(p, str_regexp, term, paren);
        return tREGEXP_BEG;

      case 's':
        p->lex_strterm = new_strterm(p, str_ssym, term, paren);
        return tSYMBEG;

      case 'I':
        p->lex_strterm = new_strterm(p, str_dsymbols, term, paren);
        return tSYMBOLS_BEG;

      case 'i':
        p->lex_strterm = new_strterm(p, str_ssymbols, term, paren);
        return tSYMBOLS_BEG;

      default:
        yyerror(NULL, p, "unknown type of %string");
        return 0;
      }
    }
    if ((c = nextc(p)) == '=') {
      pylval.id = intern_op(mod);
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_SPCARG(c)) {
      goto quotation;
    }
    if (p->lstate == EXPR_FNAME || p->lstate == EXPR_DOT) {
      p->lstate = EXPR_ARG;
    }
    else {
      p->lstate = EXPR_BEG;
    }
    pushback(p, c);
    return '%';

  case '$':
    p->lstate = EXPR_END;
    token_column = newtok(p);
    c = nextc(p);
    if (c < 0) {
      yyerror(NULL, p, "incomplete global variable syntax");
      return 0;
    }
    switch (c) {
    case '_':     /* $_: last read line string */
      c = nextc(p);
      if (c >= 0 && identchar(c)) { /* if there is more after _ it is a variable */
        tokadd(p, '$');
        tokadd(p, c);
        break;
      }
      pushback(p, c);
      c = '_';
      /* fall through */
    case '~':     /* $~: match-data */
    case '*':     /* $*: argv */
    case '$':     /* $$: pid */
    case '?':     /* $?: last status */
    case '!':     /* $!: error string */
    case '@':     /* $@: error position */
    case '/':     /* $/: input record separator */
    case '\\':    /* $\: output record separator */
    case ';':     /* $;: field separator */
    case ',':     /* $,: output field separator */
    case '.':     /* $.: last read line number */
    case '=':     /* $=: ignorecase */
    case ':':     /* $:: load path */
    case '<':     /* $<: reading filename */
    case '>':     /* $>: default output handle */
    case '\"':    /* $": already loaded files */
      tokadd(p, '$');
      tokadd(p, c);
      tokfix(p);
      pylval.id = intern(tok(p), toklen(p));
      return tGVAR;

    case '-':
      tokadd(p, '$');
      tokadd(p, c);
      c = nextc(p);
      pushback(p, c);
      gvar:
      tokfix(p);
      pylval.id = intern(tok(p), toklen(p));
      return tGVAR;

    case '&':     /* $&: last match */
    case '`':     /* $`: string before last match */
    case '\'':    /* $': string after last match */
    case '+':     /* $+: string matches last pattern */
      if (last_state == EXPR_FNAME) {
        tokadd(p, '$');
        tokadd(p, c);
        goto gvar;
      }
      pylval.nd = new_back_ref(p, c);
      return tBACK_REF;

    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9':
      do {
        tokadd(p, c);
        c = nextc(p);
      } while (c >= 0 && ISDIGIT(c));
      pushback(p, c);
      if (last_state == EXPR_FNAME) goto gvar;
      tokfix(p);
      {
        mrb_int n;
        if (!mrb_read_int(tok(p), NULL, NULL, &n)) {
          yywarning(p, "capture group index too big; always nil");
          return keyword_nil;
        }
        pylval.nd = new_nth_ref(p, (int)n);
      }
      return tNTH_REF;

    default:
      if (!identchar(c)) {
        pushback(p,  c);
        return '$';
      }
      /* fall through */
    case '0':
      tokadd(p, '$');
    }
    break;

    case '@':
      c = nextc(p);
      token_column = newtok(p);
      tokadd(p, '@');
      if (c == '@') {
        tokadd(p, '@');
        c = nextc(p);
      }
      if (c < 0) {
        if (p->tidx == 1) {
          yyerror(NULL, p, "incomplete instance variable syntax");
        }
        else {
          yyerror(NULL, p, "incomplete class variable syntax");
        }
        return 0;
      }
      else if (ISDIGIT(c)) {
        if (p->tidx == 1) {
          yyerror_c(p, "wrong instance variable name: @", c);
        }
        else {
          yyerror_c(p, "wrong class variable name: @@", c);
        }
        return 0;
      }
      if (!identchar(c)) {
        pushback(p, c);
        return '@';
      }
      break;

    case '_':
      token_column = newtok(p);
      break;

    default:
      if (!identchar(c)) {
        char buf[36];
        const char s[] = "Invalid char in expression: 0x";
        const char hexdigits[] = "0123456789ABCDEF";

        strcpy(buf, s);
        buf[sizeof(s)-1] = hexdigits[(c & 0xf0) >> 4];
        buf[sizeof(s)]   = hexdigits[(c & 0x0f)];
        buf[sizeof(s)+1] = 0;
        yyerror(NULL, p, buf);
        goto retry;
      }

      token_column = newtok(p);
      break;
  }

  do {
    tokadd(p, c);
    c = nextc(p);
    if (c < 0) break;
  } while (identchar(c));
  if (token_column == 0 && toklen(p) == 7 && (c < 0 || c == '\n') &&
      strncmp(tok(p), "__END__", toklen(p)) == 0)
    return -1;

  switch (tok(p)[0]) {
  case '@': case '$':
    pushback(p, c);
    break;
  default:
    if ((c == '!' || c == '?') && !peek(p, '=')) {
      tokadd(p, c);
    }
    else {
      pushback(p, c);
    }
  }
  tokfix(p);
  {
    int result = 0;

    switch (tok(p)[0]) {
    case '$':
      p->lstate = EXPR_END;
      result = tGVAR;
      break;
    case '@':
      p->lstate = EXPR_END;
      if (tok(p)[1] == '@')
        result = tCVAR;
      else
        result = tIVAR;
      break;

    case '_':
      if (toklen(p) == 2 && ISDIGIT(tok(p)[1]) && p->nvars) {
        int n = tok(p)[1] - '0';
        int nvar;

        if (n > 0) {
          nvar = node_to_int(p->nvars->car);
          if (nvar != -2) {     /* numbered parameters never appear on toplevel */
            pylval.num = n;
            p->lstate = EXPR_END;
            return tNUMPARAM;
          }
        }
      }
      /* fall through */
    default:
      if (toklast(p) == '!' || toklast(p) == '?') {
        result = tFID;
      }
      else {
        if (p->lstate == EXPR_FNAME) {
          if ((c = nextc(p)) == '=' && !peek(p, '~') && !peek(p, '>') &&
              (!peek(p, '=') || (peek_n(p, '>', 1)))) {
            result = tIDENTIFIER;
            tokadd(p, c);
            tokfix(p);
          }
          else {
            pushback(p, c);
          }
          if ((c = nextc(p)) == '=' && !peek(p, '~') && !peek(p, '>') &&
              (!peek(p, '=') || (peek_n(p, '>', 1)))) {
            result = tIDENTIFIER;
            tokadd(p, c);
            tokfix(p);
          }
          else {
            pushback(p, c);
          }
        }
        if (result == 0 && ISUPPER(tok(p)[0])) {
          result = tCONSTANT;
        }
        else {
          result = tIDENTIFIER;
        }
      }

      if (IS_LABEL_POSSIBLE()) {
        if (IS_LABEL_SUFFIX(0)) {
          p->lstate = EXPR_END;
          tokfix(p);
          pylval.id = intern(tok(p), toklen(p));
          return tIDENTIFIER;
        }
      }
      if (p->lstate != EXPR_DOT) {
        const struct kwtable *kw;

        /* See if it is a reserved word.  */
        kw = mrb_reserved_word(tok(p), toklen(p));
        if (kw) {
          enum mrb_lex_state_enum state = p->lstate;
          pylval.num = p->lineno;
          p->lstate = kw->state;
          if (state == EXPR_FNAME) {
            pylval.id = intern_cstr(kw->name);
            return kw->id[0];
          }
          if (p->lstate == EXPR_BEG) {
            p->cmd_start = TRUE;
          }
          if (kw->id[0] == keyword_do) {
            if (p->lpar_beg && p->lpar_beg == p->paren_nest) {
              p->lpar_beg = 0;
              p->paren_nest--;
              return keyword_do_LAMBDA;
            }
            if (COND_P()) return keyword_do_cond;
            if (CMDARG_P() && state != EXPR_CMDARG)
              return keyword_do_block;
            if (state == EXPR_ENDARG || state == EXPR_BEG)
              return keyword_do_block;
            return keyword_do;
          }
          if (kw->id[0] == keyword_in) {
            /* Set in_kwarg for pattern matching context */
            p->in_kwarg++;
          }
          if (state == EXPR_BEG || state == EXPR_VALUE || state == EXPR_CLASS)
            return kw->id[0];
          else {
            if (kw->id[0] != kw->id[1])
              p->lstate = EXPR_BEG;
            return kw->id[1];
          }
        }
      }

      if (IS_BEG() || p->lstate == EXPR_DOT || IS_ARG()) {
        if (cmd_state) {
          p->lstate = EXPR_CMDARG;
        }
        else {
          p->lstate = EXPR_ARG;
        }
      }
      else if (p->lstate == EXPR_FNAME) {
        p->lstate = EXPR_ENDFN;
      }
      else {
        p->lstate = EXPR_END;
      }
    }
    {
      mrb_sym ident = intern(tok(p), toklen(p));

      pylval.id = ident;
      if (last_state != EXPR_DOT && ISLOWER(tok(p)[0]) && local_var_p(p, ident)) {
        p->lstate = EXPR_END;
      }
    }
    return result;
  }
}

static int
yylex(void *lval, void *lp, parser_state *p)
{
  p->ylval = lval;
  return parser_yylex(p);
}

static void
parser_init_cxt(parser_state *p, mrb_ccontext *cxt)
{
  if (!cxt) return;
  if (cxt->filename) mrb_parser_set_filename(p, cxt->filename);
  if (cxt->lineno) p->lineno = cxt->lineno;
  if (cxt->syms) {
    int i;

    p->locals = cons(0,0);
    for (i=0; i<cxt->slen; i++) {
      local_add_f(p, cxt->syms[i]);
    }
  }
  p->capture_errors = cxt->capture_errors;
  p->no_optimize = cxt->no_optimize;
  p->no_ext_ops = cxt->no_ext_ops;
  p->upper = cxt->upper;
  if (cxt->partial_hook) {
    p->cxt = cxt;
  }
}

static void
parser_update_cxt(parser_state *p, mrb_ccontext *cxt)
{
  node *n, *n0;
  int i = 0;

  if (!cxt) return;
  if (!p->tree) return;
  if (!node_type_p(p->tree, NODE_SCOPE)) return;

  /* Extract locals from variable-sized NODE_SCOPE */
  struct mrb_ast_scope_node *scope = scope_node(p->tree);
  n0 = n = scope->locals;
  while (n) {
    i++;
    n = n->cdr;
  }
  cxt->syms = (mrb_sym*)mrbc_realloc(cxt->syms, i*sizeof(mrb_sym));
  cxt->slen = i;
  for (i=0, n=n0; n; i++,n=n->cdr) {
    cxt->syms[i] = node_to_sym(n->car);
  }
}

static void dump_node(mrb_state *mrb, node *tree, int offset);

MRB_API void
mrb_parser_parse(parser_state *p, mrb_ccontext *c)
{
  struct mrb_jmpbuf buf1;
  struct mrb_jmpbuf *prev = p->mrb->jmp;
  p->mrb->jmp = &buf1;

  MRB_TRY(p->mrb->jmp) {
    int n = 1;

    p->cmd_start = TRUE;
    p->in_def = p->in_single = 0;
    p->nerr = p->nwarn = 0;
    p->lex_strterm = NULL;
    parser_init_cxt(p, c);

    n = yyparse(p);
    if (n != 0 || p->nerr > 0) {
      p->tree = 0;
      p->mrb->jmp = prev;
      return;
    }
    parser_update_cxt(p, c);
    if (c && c->dump_result) {
      dump_node(p->mrb, p->tree, 0);
    }
  }
  MRB_CATCH(p->mrb->jmp) {
    p->nerr++;
    if (p->mrb->exc == NULL) {
      yyerror(NULL, p, "memory allocation error");
      p->nerr++;
      p->tree = 0;
    }
  }
  MRB_END_EXC(p->jmp);
  p->mrb->jmp = prev;
}

MRB_API parser_state*
mrb_parser_new(mrb_state *mrb)
{
  mempool *pool;
  parser_state *p;
  static const parser_state parser_state_zero = { 0 };

  pool = mempool_open();
  if (!pool) return NULL;
  p = (parser_state*)mempool_alloc(pool, sizeof(parser_state));
  if (!p) return NULL;

  *p = parser_state_zero;
  p->mrb = mrb;
  p->pool = pool;

  p->s = p->send = NULL;
#ifndef MRB_NO_STDIO
  p->f = NULL;
#endif

  p->cmd_start = TRUE;
  p->in_def = p->in_single = 0;

  p->capture_errors = FALSE;
  p->lineno = 1;
  p->column = 0;
#if defined(PARSER_TEST) || defined(PARSER_DEBUG)
  yydebug = 1;
#endif
  p->tsiz = MRB_PARSER_TOKBUF_SIZE;
  p->tokbuf = p->buf;

  p->lex_strterm = NULL;

  p->current_filename_index = -1;
  p->filename_table = NULL;
  p->filename_table_length = 0;

  return p;
}

MRB_API void
mrb_parser_free(parser_state *p) {
  if (p->tokbuf != p->buf) {
    mrbc_free(p->tokbuf);
  }
  mempool_close(p->pool);
}

MRB_API mrb_ccontext*
mrb_ccontext_new(mrb_state *mrb)
{
  static const mrb_ccontext cc_zero = { 0 };
  mrb_ccontext *cc = (mrb_ccontext*)mrbc_malloc(sizeof(mrb_ccontext));
  *cc = cc_zero;
  return cc;
}

MRB_API void
mrb_ccontext_free(mrb_state *mrb, mrb_ccontext *cxt)
{
  mrbc_free(cxt->filename);
  mrbc_free(cxt->syms);
  mrbc_free(cxt);
}

MRB_API const char*
mrb_ccontext_filename(mrb_state *mrb, mrb_ccontext *c, const char *s)
{
  if (s) {
    size_t len = strlen(s);
    char *p = (char*)mrbc_malloc(len + 1);

    if (p == NULL) return NULL;
    memcpy(p, s, len + 1);
    if (c->filename) {
      mrbc_free(c->filename);
    }
    c->filename = p;
  }
  return c->filename;
}

MRB_API void
mrb_ccontext_partial_hook(mrb_ccontext *c, int (*func)(struct mrb_parser_state*), void *data)
{
  c->partial_hook = func;
  c->partial_data = data;
}

MRB_API void
mrb_ccontext_cleanup_local_variables(mrb_ccontext *c)
{
  if (c->syms) {
    mrbc_free(c->syms);
    c->syms = NULL;
    c->slen = 0;
  }
  c->keep_lv = FALSE;
}

MRB_API void
mrb_parser_set_filename(struct mrb_parser_state *p, const char *f)
{
  mrb_sym sym;
  uint16_t i;
  mrb_sym* new_table;

  sym = mrb_intern_cstr(p->mrb, f);
  p->filename_sym = sym;
  p->lineno = (p->filename_table_length > 0)? 0 : 1;

  for (i = 0; i < p->filename_table_length; i++) {
    if (p->filename_table[i] == sym) {
      p->current_filename_index = i;
      return;
    }
  }

  if (p->filename_table_length == UINT16_MAX) {
    yyerror(NULL, p, "too many files to compile");
    return;
  }
  p->current_filename_index = p->filename_table_length++;

  new_table = (mrb_sym*)parser_palloc(p, sizeof(mrb_sym) * p->filename_table_length);
  if (p->filename_table) {
    memmove(new_table, p->filename_table, sizeof(mrb_sym) * p->current_filename_index);
  }
  p->filename_table = new_table;
  p->filename_table[p->filename_table_length - 1] = sym;
}

MRB_API mrb_sym
mrb_parser_get_filename(struct mrb_parser_state* p, uint16_t idx) {
  if (idx >= p->filename_table_length) return 0;
  else {
    return p->filename_table[idx];
  }
}

#ifndef MRB_NO_STDIO
static struct mrb_parser_state *
mrb_parse_file_continue(mrb_state *mrb, FILE *f, const void *prebuf, size_t prebufsize, mrb_ccontext *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  if (prebuf) {
    p->s = (const char*)prebuf;
    p->send = (const char*)prebuf + prebufsize;
  }
  else {
    p->s = p->send = NULL;
  }
  p->f = f;

  mrb_parser_parse(p, c);
  return p;
}

MRB_API parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrb_ccontext *c)
{
  return mrb_parse_file_continue(mrb, f, NULL, 0, c);
}
#endif

MRB_API parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, size_t len, mrb_ccontext *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = s;
  p->send = s + len;

  mrb_parser_parse(p, c);
  return p;
}

MRB_API parser_state*
mrb_parse_string(mrb_state *mrb, const char *s, mrb_ccontext *c)
{
  return mrb_parse_nstring(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_exec(mrb_state *mrb, struct mrb_parser_state *p, mrb_ccontext *c)
{
  struct RClass *target = mrb->object_class;
  struct RProc *proc;
  mrb_value v;
  mrb_int keep = 0;

  if (!p) {
    return mrb_undef_value();
  }
  if (!p->tree || p->nerr) {
    if (c) c->parser_nerr = p->nerr;
    if (p->capture_errors) {
      char buf[256];

      strcpy(buf, "line ");
      dump_int(p->error_buffer[0].lineno, buf+5);
      strcat(buf, ": ");
      strncat(buf, p->error_buffer[0].message, sizeof(buf) - strlen(buf) - 1);
      mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SYNTAX_ERROR, buf, strlen(buf)));
      mrb_parser_free(p);
      return mrb_undef_value();
    }
    else {
      if (mrb->exc == NULL) {
        mrb->exc = mrb_obj_ptr(mrb_exc_new_lit(mrb, E_SYNTAX_ERROR, "syntax error"));
      }
      mrb_parser_free(p);
      return mrb_undef_value();
    }
  }
  proc = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  if (proc == NULL) {
    if (mrb->exc == NULL) {
      mrb->exc = mrb_obj_ptr(mrb_exc_new_lit(mrb, E_SCRIPT_ERROR, "codegen error"));
    }
    return mrb_undef_value();
  }
  if (c) {
    if (c->dump_result) mrb_codedump_all(mrb, proc);
    if (c->no_exec) return mrb_obj_value(proc);
    if (c->target_class) {
      target = c->target_class;
    }
    if (c->keep_lv) {
      keep = c->slen + 1;
    }
    else {
      c->keep_lv = TRUE;
    }
  }
  MRB_PROC_SET_TARGET_CLASS(proc, target);
  if (mrb->c->ci) {
    mrb_vm_ci_target_class_set(mrb->c->ci, target);
  }
  v = mrb_top_run(mrb, proc, mrb_top_self(mrb), keep);
  if (mrb->exc) return mrb_nil_value();
  return v;
}

#ifndef MRB_NO_STDIO
MRB_API mrb_value
mrb_load_file_cxt(mrb_state *mrb, FILE *f, mrb_ccontext *c)
{
  return mrb_load_exec(mrb, mrb_parse_file(mrb, f, c), c);
}

MRB_API mrb_value
mrb_load_file(mrb_state *mrb, FILE *f)
{
  return mrb_load_file_cxt(mrb, f, NULL);
}

#define DETECT_SIZE 64

/*
 * In order to be recognized as a `.mrb` file, the following three points must be satisfied:
 * - File starts with "RITE"
 * - At least `sizeof(struct rite_binary_header)` bytes can be read
 * - `NUL` is included in the first 64 bytes of the file
 */
MRB_API mrb_value
mrb_load_detect_file_cxt(mrb_state *mrb, FILE *fp, mrb_ccontext *c)
{
  union {
    char b[DETECT_SIZE];
    struct rite_binary_header h;
  } leading;
  size_t bufsize;

  if (mrb == NULL || fp == NULL) {
    return mrb_nil_value();
  }

  bufsize = fread(leading.b, sizeof(char), sizeof(leading), fp);
  if (bufsize < sizeof(leading.h) ||
      memcmp(leading.h.binary_ident, RITE_BINARY_IDENT, sizeof(leading.h.binary_ident)) != 0 ||
      memchr(leading.b, '\0', bufsize) == NULL) {
    return mrb_load_exec(mrb, mrb_parse_file_continue(mrb, fp, leading.b, bufsize, c), c);
  }
  else {
    mrb_int binsize = bin_to_uint32(leading.h.binary_size);
    mrb_value bin_obj = mrb_str_new(mrb, NULL, binsize);
    uint8_t *bin = (uint8_t*)RSTRING_PTR(bin_obj);
    if ((size_t)binsize > bufsize) {
      memcpy(bin, leading.b, bufsize);
      if (fread(bin + bufsize, binsize - bufsize, 1, fp) == 0) {
        binsize = bufsize;
        /* The error is reported by mrb_load_irep_buf_cxt() */
      }
    }

    mrb_value result = mrb_load_irep_buf_cxt(mrb, bin, binsize, c);
    if (mrb_string_p(bin_obj)) mrb_str_resize(mrb, bin_obj, 0);
    return result;
  }
}
#endif

MRB_API mrb_value
mrb_load_nstring_cxt(mrb_state *mrb, const char *s, size_t len, mrb_ccontext *c)
{
  return mrb_load_exec(mrb, mrb_parse_nstring(mrb, s, len, c), c);
}

MRB_API mrb_value
mrb_load_nstring(mrb_state *mrb, const char *s, size_t len)
{
  return mrb_load_nstring_cxt(mrb, s, len, NULL);
}

MRB_API mrb_value
mrb_load_string_cxt(mrb_state *mrb, const char *s, mrb_ccontext *c)
{
  return mrb_load_nstring_cxt(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_string(mrb_state *mrb, const char *s)
{
  return mrb_load_string_cxt(mrb, s, NULL);
}

#ifndef MRB_NO_STDIO

static void
dump_prefix(int offset, uint16_t lineno)
{
  printf("%05d ", lineno);
  while (offset--) {
    putc(' ', stdout);
    putc(' ', stdout);
  }
}

static void
dump_recur(mrb_state *mrb, node *tree, int offset)
{
  while (tree) {
    dump_node(mrb, tree->car, offset);
    tree = tree->cdr;
  }
}

static void
dump_locals(mrb_state *mrb, node *tree, int offset, uint16_t lineno)
{
  if (!tree || (!tree->car && !tree->cdr)) return;

  dump_prefix(offset, lineno);
  printf("locals:\n");
  dump_prefix(offset+1, lineno);
  while (tree) {
    if (tree->car) {
      mrb_sym sym = node_to_sym(tree->car);
      if (sym != 0) {
        const char *name = mrb_sym_name(mrb, sym);
        if (name && strlen(name) > 0 && name[0] != '!' && name[0] != '@' && name[0] != '$') {
          printf(" %s", mrb_sym_dump(mrb, sym));
        }
        else {
          printf(" (invalid symbol: %s)", name ? name : "(null)");
        }
      }
      else {
        printf(" (anonymous)");
      }
    }
    tree = tree->cdr;
  }
  printf("\n");
}

static void
dump_cpath(mrb_state *mrb, node *tree, int offset, uint16_t lineno)
{
  dump_prefix(offset, lineno);
  printf("cpath: ");
  if (!tree) {
    printf("(null)\n");
  }
  else if (node_to_int(tree->car) == 0) {
    printf("(null)\n");
  }
  else if (node_to_int(tree->car) == 1) {
    printf("Object\n");
  }
  else {
    printf("\n");
    dump_node(mrb, tree->car, offset+1);
  }
  dump_prefix(offset, lineno);
  printf("name: %s\n", mrb_sym_dump(mrb, node_to_sym(tree->cdr)));
}

/*
 * This function restores the GC arena on return.
 * For this reason, if a process that further generates an object is
 * performed at the caller, the string pointer returned as the return
 * value may become invalid.
 */
static const char*
str_dump(mrb_state *mrb, const char *str, int len)
{
  int ai = mrb_gc_arena_save(mrb);
  mrb_value s = mrb_str_new(mrb, str, (mrb_int)len);
  s = mrb_str_dump(mrb, s);
  mrb_gc_arena_restore(mrb, ai);
  return RSTRING_PTR(s);
}

static void
dump_str(mrb_state *mrb, node *n, int offset, uint16_t lineno)
{
  while (n) {
    dump_prefix(offset, lineno);
    int len = node_to_int(n->car->car);
    if (len >= 0) {
      printf("str: %s\n", str_dump(mrb, (char*)n->car->cdr, len));
    }
    else {
      printf("interpolation:\n");
      dump_node(mrb, n->car->cdr, offset+1);
    }
    n = n->cdr;
  }
}

static void
dump_args(mrb_state *mrb, struct mrb_ast_args *args, int offset, uint16_t lineno)
{
  if (args->mandatory_args) {
    dump_prefix(offset, lineno);
    printf("mandatory args:\n");
    dump_recur(mrb, args->mandatory_args, offset+1);
  }
  if (args->optional_args) {
    dump_prefix(offset, lineno);
    printf("optional args:\n");
    {
      node *n = args->optional_args;
      while (n) {
        dump_prefix(offset+1, lineno);
        printf("%s=\n", mrb_sym_name(mrb, node_to_sym(n->car->car)));
        dump_node(mrb, n->car->cdr, offset+2);
        n = n->cdr;
      }
    }
  }
  if (args->rest_arg) {
    mrb_sym rest = args->rest_arg;

    dump_prefix(offset, lineno);
    if (rest == MRB_OPSYM(mul))
      printf("rest=*\n");
    else
      printf("rest=*%s\n", mrb_sym_name(mrb, rest));
  }
  if (args->post_mandatory_args) {
    dump_prefix(offset, lineno);
    printf("post mandatory args:\n");
    dump_recur(mrb, args->post_mandatory_args, offset+1);
  }
  if (args->keyword_args) {
    dump_prefix(offset, lineno);
    printf("keyword args:\n");
    {
      node *n = args->keyword_args;
      while (n) {
        dump_prefix(offset+1, lineno);
        printf("%s:\n", mrb_sym_name(mrb, node_to_sym(n->car->car)));
        dump_node(mrb, n->car->cdr, offset+2);
        n = n->cdr;
      }
    }
  }
  if (args->kwrest_arg) {
    mrb_sym rest = args->kwrest_arg;

    dump_prefix(offset, lineno);
    if (rest == MRB_OPSYM(pow))
      printf("kwrest=**\n");
    else
      printf("kwrest=**%s\n", mrb_sym_name(mrb, rest));
  }
  if (args->block_arg) {
    mrb_sym blk = args->block_arg;

    dump_prefix(offset, lineno);
    if (blk == MRB_OPSYM(and))
      printf("blk=&\n");
    else
      printf("blk=&%s\n", mrb_sym_name(mrb, blk));
  }
}

static void
dump_callargs(mrb_state *mrb, node *n, int offset, uint16_t lineno)
{
  if (!n) return;

  struct mrb_ast_callargs *args = (struct mrb_ast_callargs*)n;
  if (args->regular_args) {
    dump_prefix(offset+1, lineno);
    printf("args:\n");
    dump_recur(mrb, args->regular_args, offset+2);
  }
  if (args->keyword_args) {
    dump_prefix(offset+1, lineno);
    printf("kw_args:\n");
    node *kw = args->keyword_args;
    while (kw) {
      dump_prefix(offset+2, lineno);
      printf("key:\n");
      if (node_to_sym(kw->car->car) == MRB_OPSYM(pow)) {
        dump_prefix(offset+3, lineno);
        printf("**:\n");
      }
      else {
        dump_node(mrb, kw->car->car, offset+3);
      }
      dump_prefix(offset+2, lineno);
      printf("value:\n");
      dump_node(mrb, kw->car->cdr, offset+3);
      kw = kw->cdr;
    }
  }
  if (args->block_arg) {
    dump_prefix(offset+1, lineno);
    printf("block:\n");
    dump_node(mrb, args->block_arg, offset+2);
  }
}

#endif

void
dump_node(mrb_state *mrb, node *tree, int offset)
{
#ifndef MRB_NO_STDIO
  enum node_type nodetype;
  uint16_t lineno = 0;

  if (!tree) return;

  /* Extract line number from variable-sized node header */
  if (get_node_type(tree) != NODE_LAST) {
    lineno = ((struct mrb_ast_var_header*)tree)->lineno;
  }

  dump_prefix(offset, lineno);

  /* All nodes are now variable-sized nodes with headers */
  nodetype = get_node_type(tree);

  switch (nodetype) {
  /* Variable-sized node cases */
  case NODE_SCOPE:
    printf("NODE_SCOPE:\n");
    if (scope_node(tree)->locals) {
      dump_locals(mrb, scope_node(tree)->locals, offset+1, lineno);
    }
    if (scope_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, scope_node(tree)->body, offset+2);
    }
    break;

  case NODE_INT:
    printf("NODE_INT: %d\n", int_node(tree)->value);
    break;

  case NODE_BIGINT:
    printf("NODE_BIGINT: %s (base %d)\n", bigint_node(tree)->string, bigint_node(tree)->base);
    break;

  case NODE_FLOAT:
    printf("NODE_FLOAT: %s\n", float_node(tree)->value);
    break;

  case NODE_STR:
    printf("NODE_STR:\n");
    dump_str(mrb, str_node(tree)->list, offset+1, lineno);
    break;

  case NODE_XSTR:
    printf("NODE_XSTR:\n");
    dump_str(mrb, xstr_node(tree)->list, offset+1, lineno);
    break;

  case NODE_SYM:
    printf("NODE_SYM: %s\n", mrb_sym_dump(mrb, sym_node(tree)->symbol));
    break;

  case NODE_DSYM:
    printf("NODE_DSYM:\n");
    dump_str(mrb, str_node(tree)->list, offset+1, lineno);
    break;

  case NODE_LVAR:
    printf("NODE_LVAR: %s\n", mrb_sym_dump(mrb, var_node(tree)->symbol));
    break;

  case NODE_GVAR:
    printf("NODE_GVAR: %s\n", mrb_sym_dump(mrb, var_node(tree)->symbol));
    break;

  case NODE_IVAR:
    printf("NODE_IVAR: %s\n", mrb_sym_dump(mrb, var_node(tree)->symbol));
    break;

  case NODE_CVAR:
    printf("NODE_CVAR: %s\n", mrb_sym_dump(mrb, var_node(tree)->symbol));
    break;

  case NODE_NVAR:
    printf("NODE_NVAR: %d\n", nvar_node(tree)->num);
    break;

  case NODE_CONST:
    printf("NODE_CONST: %s\n", mrb_sym_dump(mrb, var_node(tree)->symbol));
    break;

  case NODE_CALL:
    printf("NODE_CALL: %s\n", mrb_sym_dump(mrb, call_node(tree)->method_name));
    if (call_node(tree)->receiver) {
      dump_prefix(offset+1, lineno);
      printf("receiver:\n");
      dump_node(mrb, call_node(tree)->receiver, offset+2);
    }
    if (call_node(tree)->args) {
      dump_callargs(mrb, call_node(tree)->args, offset, lineno);
    }
    break;

  case NODE_ARRAY:
    printf("NODE_ARRAY:\n");
    if (array_node(tree)->elements) {
      dump_recur(mrb, array_node(tree)->elements, offset+1);
    }
    break;

  case NODE_TRUE:
    printf("NODE_TRUE\n");
    break;

  case NODE_FALSE:
    printf("NODE_FALSE\n");
    break;

  case NODE_NIL:
    printf("NODE_NIL\n");
    break;

  case NODE_SELF:
    printf("NODE_SELF\n");
    break;

  case NODE_IF:
    printf("NODE_IF:\n");
    if (if_node(tree)->condition) {
      dump_prefix(offset+1, lineno);
      printf("cond:\n");
      dump_node(mrb, if_node(tree)->condition, offset+2);
    }
    if (if_node(tree)->then_body) {
      dump_prefix(offset+1, lineno);
      printf("then:\n");
      dump_node(mrb, if_node(tree)->then_body, offset+2);
    }
    if (if_node(tree)->else_body) {
      dump_prefix(offset+1, lineno);
      printf("else:\n");
      dump_node(mrb, if_node(tree)->else_body, offset+2);
    }
    break;

  case NODE_DEF:
    printf("NODE_DEF: %s\n", mrb_sym_dump(mrb, def_node(tree)->name));
    if (def_node(tree)->args) {
      dump_args(mrb, sdef_node(tree)->args, offset+1, lineno);
    }
    if (def_node(tree)->locals) {
      dump_locals(mrb, def_node(tree)->locals, offset+1, lineno);
    }
    if (def_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, def_node(tree)->body, offset+2);
    }
    break;

  case NODE_ASGN:
    printf("NODE_ASGN:\n");
    if (asgn_node(tree)->lhs) {
      dump_prefix(offset+1, lineno);
      printf("lhs:\n");
      dump_node(mrb, asgn_node(tree)->lhs, offset+2);
    }
    if (asgn_node(tree)->rhs) {
      dump_prefix(offset+1, lineno);
      printf("rhs:\n");
      dump_node(mrb, asgn_node(tree)->rhs, offset+2);
    }
    break;

  case NODE_MASGN:
  case NODE_MARG:
    printf("%s:\n", get_node_type(tree) == NODE_MASGN ? "NODE_MASGN" : "NODE_MARG");
    /* Handle pre-splat variables */
    if (masgn_node(tree)->pre) {
      dump_prefix(offset+1, lineno);
      printf("pre:\n");
      dump_recur(mrb, masgn_node(tree)->pre, offset+2);
    }
    /* Handle splat variable (can be -1 sentinel for anonymous splat) */
    if (masgn_node(tree)->rest) {
      if ((intptr_t)masgn_node(tree)->rest == -1) {
        dump_prefix(offset+1, lineno);
        printf("rest: *<anonymous>\n");
      }
      else {
        dump_prefix(offset+1, lineno);
        printf("rest:\n");
        dump_node(mrb, masgn_node(tree)->rest, offset+2);
      }
    }
    /* Handle post-splat variables */
    if (masgn_node(tree)->post) {
      dump_prefix(offset+1, lineno);
      printf("post:\n");
      dump_recur(mrb, masgn_node(tree)->post, offset+2);
    }
    if (masgn_node(tree)->rhs) {
      dump_prefix(offset+1, lineno);
      printf("rhs:\n");
      dump_node(mrb, masgn_node(tree)->rhs, offset+2);
    }
    break;

  case NODE_RETURN:
    printf("NODE_RETURN:\n");
    if (return_node(tree)->args) {
      dump_node(mrb, return_node(tree)->args, offset);
    }
    break;

  case NODE_BREAK:
    printf("NODE_BREAK:\n");
    if (break_node(tree)->value) {
      dump_prefix(offset+1, lineno);
      printf("value:\n");
      dump_node(mrb, break_node(tree)->value, offset+2);
    }
    break;

  case NODE_NEXT:
    printf("NODE_NEXT:\n");
    if (next_node(tree)->value) {
      dump_prefix(offset+1, lineno);
      printf("value:\n");
      dump_node(mrb, next_node(tree)->value, offset+2);
    }
    break;

  case NODE_NEGATE:
    printf("NODE_NEGATE:\n");
    if (negate_node(tree)->operand) {
      dump_prefix(offset+1, lineno);
      printf("operand:\n");
      dump_node(mrb, negate_node(tree)->operand, offset+2);
    }
    break;

  case NODE_STMTS:
    printf("NODE_STMTS:\n");
    if (stmts_node(tree)->stmts) {
      dump_recur(mrb, stmts_node(tree)->stmts, offset+1);
    }
    break;

  case NODE_BEGIN:
    printf("NODE_BEGIN:\n");
    if (begin_node(tree)->body) {
      dump_node(mrb, begin_node(tree)->body, offset+1);
    }
    break;

  case NODE_RESCUE:
    printf("NODE_RESCUE:\n");
    if (rescue_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, rescue_node(tree)->body, offset+2);
    }
    if (rescue_node(tree)->rescue_clauses) {
      node *n2 = rescue_node(tree)->rescue_clauses;
      dump_prefix(offset+1, lineno);
      printf("rescue:\n");
      while (n2) {
        node *n3 = n2->car;
        if (n3->car) {
          dump_prefix(offset+2, lineno);
          printf("handle classes:\n");
          dump_recur(mrb, n3->car, offset+3);
        }
        if (n3->cdr->car) {
          dump_prefix(offset+2, lineno);
          printf("exc_var:\n");
          dump_node(mrb, n3->cdr->car, offset+3);
        }
        if (n3->cdr->cdr->car) {
          dump_prefix(offset+2, lineno);
          printf("rescue body:\n");
          dump_node(mrb, n3->cdr->cdr->car, offset+3);
        }
        n2 = n2->cdr;
      }
    }
    if (rescue_node(tree)->else_clause) {
      dump_prefix(offset+1, lineno);
      printf("else:\n");
      dump_node(mrb, rescue_node(tree)->else_clause, offset+2);
    }
    break;

  case NODE_ENSURE:
    printf("NODE_ENSURE:\n");
    if (ensure_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, ensure_node(tree)->body, offset+2);
    }
    if (ensure_node(tree)->ensure_clause) {
      dump_prefix(offset+1, lineno);
      printf("ensure:\n");
      dump_node(mrb, ensure_node(tree)->ensure_clause, offset+2);
    }
    break;

  case NODE_LAMBDA:
    printf("NODE_LAMBDA:\n");
    goto block;

  case NODE_BLOCK:
    printf("NODE_BLOCK:\n");
  block:
    if (block_node(tree)->locals) {
      dump_locals(mrb, block_node(tree)->locals, offset+1, lineno);
    }
    if (block_node(tree)->args) {
      dump_args(mrb, block_node(tree)->args, offset+1, lineno);
    }
    dump_prefix(offset+1, lineno);
    printf("body:\n");
    dump_node(mrb, block_node(tree)->body, offset+2);
    break;

  case NODE_AND:
    printf("NODE_AND:\n");
    dump_node(mrb, and_node(tree)->left, offset+1);
    dump_node(mrb, and_node(tree)->right, offset+1);
    break;

  case NODE_OR:
    printf("NODE_OR:\n");
    dump_node(mrb, or_node(tree)->left, offset+1);
    dump_node(mrb, or_node(tree)->right, offset+1);
    break;

  case NODE_CASE:
    printf("NODE_CASE:\n");
    if (case_node(tree)->value) {
      dump_prefix(offset+1, lineno);
      printf("value:\n");
      dump_node(mrb, case_node(tree)->value, offset+2);
    }
    if (case_node(tree)->body) {
      node *when_node = case_node(tree)->body;
      while (when_node) {
        dump_prefix(offset+1, lineno);
        printf("when:\n");
        node *when_clause = when_node->car;
        if (when_clause && when_clause->car) {
          dump_prefix(offset+2, lineno);
          printf("cond:\n");
          dump_recur(mrb, when_clause->car, offset+3);
        }
        if (when_clause && when_clause->cdr) {
          dump_prefix(offset+2, lineno);
          printf("body:\n");
          dump_node(mrb, when_clause->cdr, offset+3);
        }
        when_node = when_node->cdr;
      }
    }
    break;

  case NODE_WHILE:
    printf("NODE_WHILE:\n");
    goto dump_loop_node;
  case NODE_UNTIL:
    printf("NODE_UNTIL:\n");
    goto dump_loop_node;
  case NODE_WHILE_MOD:
    printf("NODE_WHILE_MOD:\n");
    goto dump_loop_node;
  case NODE_UNTIL_MOD:
    printf("NODE_UNTIL_MOD:\n");

  dump_loop_node:
    dump_prefix(offset+1, lineno);
    printf("cond:\n");
    dump_node(mrb, while_node(tree)->condition, offset+2);
    dump_prefix(offset+1, lineno);
    printf("body:\n");
    dump_node(mrb, while_node(tree)->body, offset+2);
    break;

  case NODE_FOR:
    printf("NODE_FOR:\n");
    if (for_node(tree)->var) {
      dump_prefix(offset+1, lineno);
      printf("var:\n");
      /* FOR_NODE_VAR structure:
       * var_list->car: cons-list of pre-splat variables
       * var_list->cdr->car: splat varnode (not a cons-list)
       * var_list->cdr->cdr->car: cons-list of post-splat variables */
      node *var_list = for_node(tree)->var;
      if (var_list) {
        dump_recur(mrb, var_list->car, offset+2);
        if (var_list && var_list->cdr) {
          /* Second element is a varnode, not a cons-list */
          dump_prefix(offset+1, lineno);
          printf("splat var:\n");
          dump_node(mrb, var_list->cdr->car, offset+2);
          if (var_list->cdr->cdr) {
           /* Third element is a cons-list of post-splat variables */
           dump_prefix(offset+1, lineno);
           printf("post var:\n");
           dump_recur(mrb, var_list->cdr->cdr->car, offset+2);
          }
        }
      }
    }
    if (for_node(tree)->iterable) {
      dump_prefix(offset+1, lineno);
      printf("iterable:\n");
      dump_node(mrb, for_node(tree)->iterable, offset+2);
    }
    if (for_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, for_node(tree)->body, offset+2);
    }
    break;

  case NODE_DOT2:
    printf("NODE_DOT2:\n");
    {
      if (dot2_node(tree)->left) {
        dump_prefix(offset+1, lineno);
        printf("left:\n");
        dump_node(mrb, dot2_node(tree)->left, offset+2);
      }
      if (dot2_node(tree)->right) {
        dump_prefix(offset+1, lineno);
        printf("right:\n");
        dump_node(mrb, dot2_node(tree)->right, offset+2);
      }
    }
    break;

  case NODE_DOT3:
    printf("NODE_DOT3:\n");
    {
      if (dot3_node(tree)->left) {
        dump_prefix(offset+1, lineno);
        printf("left:\n");
        dump_node(mrb, dot3_node(tree)->left, offset+2);
      }
      if (dot3_node(tree)->right) {
        dump_prefix(offset+1, lineno);
        printf("right:\n");
        dump_node(mrb, dot3_node(tree)->right, offset+2);
      }
    }
    break;

  case NODE_COLON2:
    printf("NODE_COLON2:\n");
    if (colon2_node(tree)->base) {
      dump_prefix(offset+1, lineno);
      printf("base:\n");
      dump_node(mrb, colon2_node(tree)->base, offset+2);
    }
    dump_prefix(offset+1, lineno);
    printf("name: %s\n", mrb_sym_name(mrb, colon2_node(tree)->name));
    break;

  case NODE_COLON3:
    printf("NODE_COLON3: ::%s\n", mrb_sym_name(mrb, colon3_node(tree)->name));
    break;

  case NODE_HASH:
    printf("NODE_HASH:\n");
    {
      node *pairs = hash_node(tree)->pairs;
      while (pairs) {
        dump_prefix(offset+1, lineno);
        printf("key:\n");
        if (node_to_sym(pairs->car->car) == MRB_OPSYM(pow)) {
          dump_prefix(offset+2, lineno);
          printf("**\n");
        }
        else {
          dump_node(mrb, pairs->car->car, offset+2);
        }
        dump_prefix(offset+1, lineno);
        printf("value:\n");
        dump_node(mrb, pairs->car->cdr, offset+2);
        pairs = pairs->cdr;
      }
    }
    break;

  case NODE_SPLAT:
    printf("NODE_SPLAT:\n");
    dump_node(mrb, splat_node(tree)->value, offset+1);
    break;

  case NODE_OP_ASGN:
    printf("NODE_OP_ASGN:\n");
    dump_prefix(offset+1, lineno);
    printf("lhs:\n");
    dump_node(mrb, op_asgn_node(tree)->lhs, offset+2);
    dump_prefix(offset+1, lineno);
    printf("op='%s' (%d)\n", mrb_sym_name(mrb, op_asgn_node(tree)->op), (int)op_asgn_node(tree)->op);
    dump_node(mrb, op_asgn_node(tree)->rhs, offset+1);
    break;

  case NODE_SUPER:
    printf("NODE_SUPER:\n");
    if (super_node(tree)->args) {
      dump_callargs(mrb, super_node(tree)->args, offset, lineno);
    }
    break;

  case NODE_ZSUPER:
    printf("NODE_ZSUPER:\n");
    if (super_node(tree)->args) {
      dump_callargs(mrb, super_node(tree)->args, offset, lineno);
    }
    break;

  case NODE_YIELD:
    printf("NODE_YIELD:\n");
    if (yield_node(tree)->args) {
      dump_callargs(mrb, yield_node(tree)->args, offset, lineno);
    }
    break;

  case NODE_REDO:
    printf("NODE_REDO\n");
    break;

  case NODE_RETRY:
    printf("NODE_RETRY\n");
    break;

  case NODE_BACK_REF:
    printf("NODE_BACK_REF: $%c\n", node_to_int(tree));
    break;

  case NODE_NTH_REF:
    printf("NODE_NTH_REF: $%d\n", node_to_int(tree));
    break;

  case NODE_BLOCK_ARG:
    printf("NODE_BLOCK_ARG:\n");
    dump_node(mrb, block_arg_node(tree)->value, offset+1);
    break;

  case NODE_REGX:
    printf("NODE_REGX:\n");
    if (regx_node(tree)->list) {
      dump_str(mrb, regx_node(tree)->list, offset+1, lineno);
    }
    if (regx_node(tree)->flags) {
      dump_prefix(offset+1, lineno);
      printf("flags: %s\n", regx_node(tree)->flags);
    }
    if (regx_node(tree)->encoding) {
      dump_prefix(offset+1, lineno);
      printf("encoding: %s\n", regx_node(tree)->encoding);
    }
    break;

  case NODE_WORDS:
    printf("NODE_WORDS:\n");
    if (words_node(tree)->args) {
      node *list = words_node(tree)->args;
      while (list && list->car) {
        node *item = list->car;
        if (item->car == 0 && item->cdr == 0) {
          /* Skip separator (0 . 0) */
        }
        else if (item->car && item->cdr) {
          /* String item: (len . str) */
          dump_prefix(offset+1, lineno);
          int len = node_to_int(item->car);
          if (len >= 0 && len < 1000 && item->cdr) {
            printf("word: \"%.*s\"\n", len, (char*)item->cdr);
          }
        }
        list = list->cdr;
      }
    }
    break;

  case NODE_SYMBOLS:
    printf("NODE_SYMBOLS:\n");
    if (symbols_node(tree)->args) {
      node *list = symbols_node(tree)->args;
      while (list && list->car) {
        node *item = list->car;
        if (item->car == 0 && item->cdr == 0) {
          /* Skip separator (0 . 0) */
        } else if (item->car && item->cdr) {
          /* String item: (len . str) */
          dump_prefix(offset+1, lineno);
          int len = node_to_int(item->car);
          if (len >= 0 && len < 1000 && item->cdr) {
            printf("symbol: \"%.*s\"\n", len, (char*)item->cdr);
          }
        }
        list = list->cdr;
      }
    }
    break;

  case NODE_ALIAS:
    printf("NODE_ALIAS %s %s:\n",
           mrb_sym_dump(mrb, node_to_sym(tree->car)),
           mrb_sym_dump(mrb, node_to_sym(tree->cdr)));
    break;

  case NODE_UNDEF:
    printf("NODE_UNDEF");
    {
      node *t = tree;
      while (t) {
        printf(" %s", mrb_sym_dump(mrb, node_to_sym(t->car)));
        t = t->cdr;
      }
    }
    printf(":\n");
    break;

  case NODE_CLASS:
    printf("NODE_CLASS:\n");
    if (class_node(tree)->name) {
      dump_cpath(mrb, module_node(tree)->name, offset+1, lineno);
    }
    if (class_node(tree)->superclass) {
      dump_prefix(offset+1, lineno);
      printf("super:\n");
      dump_node(mrb, class_node(tree)->superclass, offset+2);
    }
    if (class_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, class_node(tree)->body->cdr, offset+2);
    }
    break;

  case NODE_MODULE:
    printf("NODE_MODULE:\n");
    if (module_node(tree)->name) {
      dump_cpath(mrb, module_node(tree)->name, offset+1, lineno);
    }
    if (module_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, module_node(tree)->body->cdr, offset+2);
    }
    break;

  case NODE_SCLASS:
    printf("NODE_SCLASS:\n");
    if (sclass_node(tree)->obj) {
      dump_prefix(offset+1, lineno);
      printf("obj:\n");
      dump_node(mrb, sclass_node(tree)->obj, offset+2);
    }
    if (sclass_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, sclass_node(tree)->body->cdr, offset+2);
    }
    break;

  case NODE_SDEF:
    printf("NODE_SDEF: %s\n", mrb_sym_dump(mrb, def_node(tree)->name));
    if (sdef_node(tree)->obj) {
      dump_prefix(offset+1, lineno);
      printf("recv:\n");
      dump_node(mrb, sdef_node(tree)->obj, offset+2);
    }
    if (sdef_node(tree)->args) {
      dump_args(mrb, sdef_node(tree)->args, offset+1, lineno);
    }
    if (sdef_node(tree)->locals) {
      dump_locals(mrb, sdef_node(tree)->locals, offset+1, lineno);
    }
    if (sdef_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, sdef_node(tree)->body, offset+2);
    }
    break;

  case NODE_POSTEXE:
    printf("NODE_POSTEXE:\n");
    dump_node(mrb, tree, offset+1);
    break;

  case NODE_HEREDOC:
    printf("NODE_HEREDOC:\n");
    if (heredoc_node(tree)->info.term) {
      dump_prefix(offset+1, lineno);
      printf("terminator: \"%s\"\n", heredoc_node(tree)->info.term);
    }
    if (heredoc_node(tree)->info.doc) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_str(mrb, heredoc_node(tree)->info.doc, offset+2, lineno);
    }
    if (heredoc_node(tree)->info.allow_indent) {
      dump_prefix(offset+1, lineno);
      printf("allow_indent: true\n");
    }
    if (heredoc_node(tree)->info.remove_indent) {
      dump_prefix(offset+1, lineno);
      printf("remove_indent: true\n");
    }
    break;

  case NODE_CASE_MATCH:
    printf("NODE_CASE_MATCH:\n");
    if (case_match_node(tree)->value) {
      dump_prefix(offset+1, lineno);
      printf("value:\n");
      dump_node(mrb, case_match_node(tree)->value, offset+2);
    }
    if (case_match_node(tree)->in_clauses) {
      node *in_clause = case_match_node(tree)->in_clauses;
      while (in_clause) {
        dump_node(mrb, in_clause->car, offset+1);
        in_clause = in_clause->cdr;
      }
    }
    break;

  case NODE_IN:
    printf("NODE_IN:\n");
    if (in_node(tree)->pattern) {
      dump_prefix(offset+1, lineno);
      printf("pattern:\n");
      dump_node(mrb, in_node(tree)->pattern, offset+2);
    }
    if (in_node(tree)->guard) {
      dump_prefix(offset+1, lineno);
      printf("guard (%s):\n", in_node(tree)->guard_is_unless ? "unless" : "if");
      dump_node(mrb, in_node(tree)->guard, offset+2);
    }
    if (in_node(tree)->body) {
      dump_prefix(offset+1, lineno);
      printf("body:\n");
      dump_node(mrb, in_node(tree)->body, offset+2);
    }
    break;

  case NODE_PAT_VALUE:
    printf("NODE_PAT_VALUE:\n");
    if (pat_value_node(tree)->value) {
      dump_node(mrb, pat_value_node(tree)->value, offset+1);
    }
    break;

  case NODE_PAT_VAR:
    if (pat_var_node(tree)->name) {
      printf("NODE_PAT_VAR: %s\n", mrb_sym_dump(mrb, pat_var_node(tree)->name));
    }
    else {
      printf("NODE_PAT_VAR: _ (wildcard)\n");
    }
    break;

  case NODE_PAT_PIN:
    printf("NODE_PAT_PIN: ^%s\n", mrb_sym_dump(mrb, pat_pin_node(tree)->name));
    break;

  case NODE_PAT_AS:
    printf("NODE_PAT_AS: => %s\n", mrb_sym_dump(mrb, pat_as_node(tree)->name));
    if (pat_as_node(tree)->pattern) {
      dump_prefix(offset+1, lineno);
      printf("pattern:\n");
      dump_node(mrb, pat_as_node(tree)->pattern, offset+2);
    }
    break;

  case NODE_PAT_ALT:
    printf("NODE_PAT_ALT:\n");
    if (pat_alt_node(tree)->left) {
      dump_prefix(offset+1, lineno);
      printf("left:\n");
      dump_node(mrb, pat_alt_node(tree)->left, offset+2);
    }
    if (pat_alt_node(tree)->right) {
      dump_prefix(offset+1, lineno);
      printf("right:\n");
      dump_node(mrb, pat_alt_node(tree)->right, offset+2);
    }
    break;

  case NODE_PAT_ARRAY:
    printf("NODE_PAT_ARRAY:\n");
    if (pat_array_node(tree)->pre) {
      dump_prefix(offset+1, lineno);
      printf("pre:\n");
      dump_recur(mrb, pat_array_node(tree)->pre, offset+2);
    }
    if (pat_array_node(tree)->rest) {
      dump_prefix(offset+1, lineno);
      if (pat_array_node(tree)->rest == (node*)-1) {
        printf("rest: * (anonymous)\n");
      }
      else {
        printf("rest:\n");
        dump_node(mrb, pat_array_node(tree)->rest, offset+2);
      }
    }
    if (pat_array_node(tree)->post) {
      dump_prefix(offset+1, lineno);
      printf("post:\n");
      dump_recur(mrb, pat_array_node(tree)->post, offset+2);
    }
    break;

  case NODE_PAT_HASH:
    printf("NODE_PAT_HASH:\n");
    if (pat_hash_node(tree)->pairs) {
      dump_prefix(offset+1, lineno);
      printf("pairs:\n");
      dump_recur(mrb, pat_hash_node(tree)->pairs, offset+2);
    }
    if (pat_hash_node(tree)->rest) {
      dump_prefix(offset+1, lineno);
      if (pat_hash_node(tree)->rest == (node*)-1) {
        printf("rest: **nil\n");
      }
      else {
        printf("rest:\n");
        dump_node(mrb, pat_hash_node(tree)->rest, offset+2);
      }
    }
    break;

  case NODE_MATCH_PAT:
    printf("NODE_MATCH_PAT%s:\n", match_pat_node(tree)->raise_on_fail ? " (=>)" : " (in)");
    dump_prefix(offset+1, lineno);
    printf("value:\n");
    dump_node(mrb, match_pat_node(tree)->value, offset+2);
    dump_prefix(offset+1, lineno);
    printf("pattern:\n");
    dump_node(mrb, match_pat_node(tree)->pattern, offset+2);
    break;

  default:
    /* Fallback: unknown node type - skip like codegen.c does */
    printf("unknown node type %d (0x%x)\n", nodetype, (unsigned)nodetype);
    break;
  }
#endif
}

void
mrb_parser_dump(mrb_state *mrb, node *tree, int offset)
{
  dump_node(mrb, tree, offset);
}

typedef mrb_bool mrb_parser_foreach_top_variable_func(mrb_state *mrb, mrb_sym sym, void *user);
void mrb_parser_foreach_top_variable(mrb_state *mrb, struct mrb_parser_state *p, mrb_parser_foreach_top_variable_func *func, void *user);

void
mrb_parser_foreach_top_variable(mrb_state *mrb, struct mrb_parser_state *p, mrb_parser_foreach_top_variable_func *func, void *user)
{
  const mrb_ast_node *n = p->tree;
  if (node_type_p((node*)n, NODE_SCOPE)) {
    /* Extract locals from variable-sized NODE_SCOPE */
    struct mrb_ast_scope_node *scope = scope_node(n);
    n = scope->locals;
    for (; n; n = n->cdr) {
      mrb_sym sym = node_to_sym(n->car);
      if (sym != 0) {
        if (!func(mrb, sym, user)) break;
      }
    }
  }
}
