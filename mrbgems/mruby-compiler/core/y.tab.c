/* A Bison parser, made by GNU Bison 3.8.2.  */

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
node_type(node *n)
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

/* Create find pattern node [*pre, elems, *post] */
static node*
new_pat_find(parser_state *p, node *pre, node *elems, node *post)
{
  struct mrb_ast_pat_find_node *n = NEW_NODE(pat_find, NODE_PAT_FIND);
  n->pre = pre;
  n->elems = elems;
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
    if (node_type(n->car) == NODE_MARG) {
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
  switch (node_type(lhs)) {
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
  if (node_type(nd->car) == NODE_HEREDOC) {
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
    enum node_type nt = node_type(n);
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


#line 2064 "mrbgems/mruby-compiler/core/y.tab.c"

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


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
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
#line 2005 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 2241 "mrbgems/mruby-compiler/core/y.tab.c"

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
  YYSYMBOL_program = 150,                  /* program  */
  YYSYMBOL_151_1 = 151,                    /* $@1  */
  YYSYMBOL_top_compstmt = 152,             /* top_compstmt  */
  YYSYMBOL_top_stmts = 153,                /* top_stmts  */
  YYSYMBOL_top_stmt = 154,                 /* top_stmt  */
  YYSYMBOL_155_2 = 155,                    /* @2  */
  YYSYMBOL_bodystmt = 156,                 /* bodystmt  */
  YYSYMBOL_compstmt = 157,                 /* compstmt  */
  YYSYMBOL_stmts = 158,                    /* stmts  */
  YYSYMBOL_stmt = 159,                     /* stmt  */
  YYSYMBOL_160_3 = 160,                    /* $@3  */
  YYSYMBOL_command_asgn = 161,             /* command_asgn  */
  YYSYMBOL_command_rhs = 162,              /* command_rhs  */
  YYSYMBOL_expr = 163,                     /* expr  */
  YYSYMBOL_164_4 = 164,                    /* $@4  */
  YYSYMBOL_165_5 = 165,                    /* $@5  */
  YYSYMBOL_defn_head = 166,                /* defn_head  */
  YYSYMBOL_defs_head = 167,                /* defs_head  */
  YYSYMBOL_168_6 = 168,                    /* $@6  */
  YYSYMBOL_expr_value = 169,               /* expr_value  */
  YYSYMBOL_command_call = 170,             /* command_call  */
  YYSYMBOL_block_command = 171,            /* block_command  */
  YYSYMBOL_cmd_brace_block = 172,          /* cmd_brace_block  */
  YYSYMBOL_173_7 = 173,                    /* $@7  */
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
  YYSYMBOL_command_args = 198,             /* command_args  */
  YYSYMBOL_199_9 = 199,                    /* @9  */
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
  YYSYMBOL_block_param_def = 234,          /* block_param_def  */
  YYSYMBOL_235_27 = 235,                   /* $@27  */
  YYSYMBOL_opt_bv_decl = 236,              /* opt_bv_decl  */
  YYSYMBOL_bv_decls = 237,                 /* bv_decls  */
  YYSYMBOL_bvar = 238,                     /* bvar  */
  YYSYMBOL_f_larglist = 239,               /* f_larglist  */
  YYSYMBOL_lambda_body = 240,              /* lambda_body  */
  YYSYMBOL_do_block = 241,                 /* do_block  */
  YYSYMBOL_242_28 = 242,                   /* @28  */
  YYSYMBOL_block_call = 243,               /* block_call  */
  YYSYMBOL_method_call = 244,              /* method_call  */
  YYSYMBOL_brace_block = 245,              /* brace_block  */
  YYSYMBOL_246_29 = 246,                   /* @29  */
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
  YYSYMBOL_p_const = 264,                  /* p_const  */
  YYSYMBOL_p_hash = 265,                   /* p_hash  */
  YYSYMBOL_p_hash_body = 266,              /* p_hash_body  */
  YYSYMBOL_p_hash_elems = 267,             /* p_hash_elems  */
  YYSYMBOL_p_hash_elem = 268,              /* p_hash_elem  */
  YYSYMBOL_p_kwrest = 269,                 /* p_kwrest  */
  YYSYMBOL_p_var = 270,                    /* p_var  */
  YYSYMBOL_opt_rescue = 271,               /* opt_rescue  */
  YYSYMBOL_exc_list = 272,                 /* exc_list  */
  YYSYMBOL_exc_var = 273,                  /* exc_var  */
  YYSYMBOL_opt_ensure = 274,               /* opt_ensure  */
  YYSYMBOL_literal = 275,                  /* literal  */
  YYSYMBOL_string = 276,                   /* string  */
  YYSYMBOL_string_fragment = 277,          /* string_fragment  */
  YYSYMBOL_string_rep = 278,               /* string_rep  */
  YYSYMBOL_string_interp = 279,            /* string_interp  */
  YYSYMBOL_280_34 = 280,                   /* @34  */
  YYSYMBOL_xstring = 281,                  /* xstring  */
  YYSYMBOL_regexp = 282,                   /* regexp  */
  YYSYMBOL_heredoc = 283,                  /* heredoc  */
  YYSYMBOL_heredoc_bodies = 284,           /* heredoc_bodies  */
  YYSYMBOL_heredoc_body = 285,             /* heredoc_body  */
  YYSYMBOL_heredoc_string_rep = 286,       /* heredoc_string_rep  */
  YYSYMBOL_heredoc_string_interp = 287,    /* heredoc_string_interp  */
  YYSYMBOL_288_35 = 288,                   /* @35  */
  YYSYMBOL_words = 289,                    /* words  */
  YYSYMBOL_symbol = 290,                   /* symbol  */
  YYSYMBOL_basic_symbol = 291,             /* basic_symbol  */
  YYSYMBOL_sym = 292,                      /* sym  */
  YYSYMBOL_symbols = 293,                  /* symbols  */
  YYSYMBOL_numeric = 294,                  /* numeric  */
  YYSYMBOL_variable = 295,                 /* variable  */
  YYSYMBOL_var_lhs = 296,                  /* var_lhs  */
  YYSYMBOL_var_ref = 297,                  /* var_ref  */
  YYSYMBOL_backref = 298,                  /* backref  */
  YYSYMBOL_superclass = 299,               /* superclass  */
  YYSYMBOL_300_36 = 300,                   /* $@36  */
  YYSYMBOL_f_opt_arglist_paren = 301,      /* f_opt_arglist_paren  */
  YYSYMBOL_f_arglist_paren = 302,          /* f_arglist_paren  */
  YYSYMBOL_f_arglist = 303,                /* f_arglist  */
  YYSYMBOL_f_label = 304,                  /* f_label  */
  YYSYMBOL_f_kw = 305,                     /* f_kw  */
  YYSYMBOL_f_block_kw = 306,               /* f_block_kw  */
  YYSYMBOL_f_block_kwarg = 307,            /* f_block_kwarg  */
  YYSYMBOL_f_kwarg = 308,                  /* f_kwarg  */
  YYSYMBOL_kwrest_mark = 309,              /* kwrest_mark  */
  YYSYMBOL_f_kwrest = 310,                 /* f_kwrest  */
  YYSYMBOL_args_tail = 311,                /* args_tail  */
  YYSYMBOL_opt_args_tail = 312,            /* opt_args_tail  */
  YYSYMBOL_f_args = 313,                   /* f_args  */
  YYSYMBOL_f_bad_arg = 314,                /* f_bad_arg  */
  YYSYMBOL_f_norm_arg = 315,               /* f_norm_arg  */
  YYSYMBOL_f_arg_item = 316,               /* f_arg_item  */
  YYSYMBOL_317_37 = 317,                   /* @37  */
  YYSYMBOL_f_arg = 318,                    /* f_arg  */
  YYSYMBOL_f_opt_asgn = 319,               /* f_opt_asgn  */
  YYSYMBOL_f_opt = 320,                    /* f_opt  */
  YYSYMBOL_f_block_opt = 321,              /* f_block_opt  */
  YYSYMBOL_f_block_optarg = 322,           /* f_block_optarg  */
  YYSYMBOL_f_optarg = 323,                 /* f_optarg  */
  YYSYMBOL_restarg_mark = 324,             /* restarg_mark  */
  YYSYMBOL_f_rest_arg = 325,               /* f_rest_arg  */
  YYSYMBOL_blkarg_mark = 326,              /* blkarg_mark  */
  YYSYMBOL_f_block_arg = 327,              /* f_block_arg  */
  YYSYMBOL_opt_f_block_arg = 328,          /* opt_f_block_arg  */
  YYSYMBOL_singleton = 329,                /* singleton  */
  YYSYMBOL_330_38 = 330,                   /* $@38  */
  YYSYMBOL_assoc_list = 331,               /* assoc_list  */
  YYSYMBOL_assocs = 332,                   /* assocs  */
  YYSYMBOL_assoc = 333,                    /* assoc  */
  YYSYMBOL_operation = 334,                /* operation  */
  YYSYMBOL_operation2 = 335,               /* operation2  */
  YYSYMBOL_operation3 = 336,               /* operation3  */
  YYSYMBOL_dot_or_colon = 337,             /* dot_or_colon  */
  YYSYMBOL_call_op = 338,                  /* call_op  */
  YYSYMBOL_call_op2 = 339,                 /* call_op2  */
  YYSYMBOL_opt_terms = 340,                /* opt_terms  */
  YYSYMBOL_opt_nl = 341,                   /* opt_nl  */
  YYSYMBOL_rparen = 342,                   /* rparen  */
  YYSYMBOL_trailer = 343,                  /* trailer  */
  YYSYMBOL_term = 344,                     /* term  */
  YYSYMBOL_nl = 345,                       /* nl  */
  YYSYMBOL_terms = 346,                    /* terms  */
  YYSYMBOL_none = 347                      /* none  */
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
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   13672

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  199
/* YYNRULES -- Number of rules.  */
#define YYNRULES  689
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1203

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
       0,  2179,  2179,  2179,  2189,  2195,  2199,  2203,  2207,  2213,
    2215,  2214,  2228,  2254,  2260,  2264,  2268,  2272,  2278,  2278,
    2282,  2286,  2290,  2294,  2303,  2312,  2316,  2321,  2322,  2326,
    2330,  2334,  2338,  2341,  2345,  2349,  2353,  2357,  2361,  2366,
    2370,  2379,  2388,  2397,  2406,  2413,  2414,  2418,  2421,  2422,
    2426,  2430,  2434,  2438,  2438,  2444,  2444,  2450,  2453,  2463,
    2462,  2477,  2486,  2487,  2490,  2491,  2498,  2497,  2512,  2516,
    2521,  2525,  2530,  2534,  2539,  2543,  2547,  2551,  2555,  2561,
    2565,  2571,  2572,  2578,  2582,  2586,  2590,  2594,  2598,  2602,
    2606,  2610,  2614,  2620,  2621,  2627,  2631,  2637,  2641,  2647,
    2651,  2655,  2659,  2663,  2667,  2673,  2679,  2686,  2690,  2694,
    2698,  2702,  2706,  2712,  2718,  2723,  2729,  2733,  2736,  2740,
    2744,  2751,  2752,  2753,  2754,  2759,  2766,  2767,  2770,  2774,
    2774,  2780,  2781,  2782,  2783,  2784,  2785,  2786,  2787,  2788,
    2789,  2790,  2791,  2792,  2793,  2794,  2795,  2796,  2797,  2798,
    2799,  2800,  2801,  2802,  2803,  2804,  2805,  2806,  2807,  2808,
    2809,  2812,  2812,  2812,  2813,  2813,  2814,  2814,  2814,  2815,
    2815,  2815,  2815,  2816,  2816,  2816,  2817,  2817,  2817,  2818,
    2818,  2818,  2818,  2819,  2819,  2819,  2819,  2820,  2820,  2820,
    2820,  2821,  2821,  2821,  2821,  2822,  2822,  2822,  2822,  2823,
    2823,  2826,  2830,  2834,  2838,  2842,  2846,  2850,  2855,  2860,
    2865,  2869,  2873,  2877,  2881,  2885,  2889,  2893,  2897,  2901,
    2905,  2909,  2913,  2917,  2921,  2925,  2929,  2933,  2937,  2941,
    2945,  2949,  2953,  2957,  2961,  2965,  2969,  2973,  2977,  2981,
    2985,  2989,  2993,  2997,  3001,  3005,  3009,  3013,  3022,  3031,
    3040,  3049,  3055,  3056,  3060,  3064,  3070,  3074,  3081,  3085,
    3094,  3111,  3112,  3115,  3116,  3117,  3121,  3125,  3131,  3136,
    3140,  3144,  3148,  3154,  3154,  3165,  3169,  3175,  3179,  3185,
    3188,  3193,  3197,  3201,  3206,  3210,  3216,  3221,  3225,  3231,
    3232,  3236,  3240,  3241,  3242,  3243,  3244,  3249,  3248,  3260,
    3264,  3259,  3269,  3269,  3273,  3277,  3281,  3285,  3289,  3293,
    3297,  3301,  3305,  3309,  3313,  3314,  3320,  3327,  3319,  3340,
    3348,  3356,  3356,  3356,  3363,  3363,  3363,  3370,  3376,  3380,
    3389,  3398,  3408,  3410,  3407,  3419,  3417,  3435,  3440,  3433,
    3457,  3455,  3471,  3481,  3492,  3496,  3500,  3504,  3510,  3517,
    3518,  3519,  3522,  3523,  3526,  3527,  3535,  3536,  3542,  3546,
    3549,  3553,  3557,  3561,  3566,  3570,  3574,  3578,  3584,  3583,
    3593,  3597,  3601,  3605,  3611,  3616,  3621,  3625,  3629,  3633,
    3637,  3641,  3645,  3649,  3653,  3657,  3661,  3665,  3669,  3673,
    3677,  3683,  3688,  3695,  3695,  3699,  3704,  3710,  3714,  3720,
    3721,  3724,  3729,  3732,  3736,  3742,  3746,  3753,  3752,  3769,
    3774,  3778,  3783,  3790,  3794,  3798,  3802,  3806,  3810,  3814,
    3818,  3822,  3829,  3828,  3843,  3842,  3858,  3866,  3875,  3880,
    3884,  3884,  3889,  3889,  3894,  3894,  3904,  3905,  3909,  3913,
    3917,  3921,  3925,  3930,  3935,  3943,  3947,  3954,  3958,  3964,
    3965,  3971,  3972,  3978,  3979,  3983,  3987,  3991,  3995,  3999,
    4003,  4007,  4008,  4009,  4016,  4020,  4027,  4032,  4037,  4042,
    4047,  4052,  4060,  4064,  4071,  4075,  4083,  4087,  4091,  4098,
    4102,  4109,  4113,  4117,  4124,  4128,  4137,  4142,  4150,  4154,
    4159,  4166,  4172,  4179,  4182,  4186,  4187,  4190,  4194,  4197,
    4201,  4204,  4205,  4206,  4207,  4210,  4211,  4217,  4222,  4227,
    4232,  4238,  4239,  4245,  4251,  4250,  4262,  4266,  4272,  4276,
    4282,  4291,  4302,  4305,  4306,  4309,  4315,  4321,  4322,  4325,
    4332,  4331,  4346,  4350,  4358,  4362,  4374,  4381,  4388,  4389,
    4390,  4391,  4392,  4396,  4402,  4406,  4414,  4415,  4416,  4420,
    4426,  4430,  4434,  4438,  4442,  4448,  4452,  4458,  4462,  4466,
    4470,  4474,  4478,  4482,  4490,  4497,  4503,  4504,  4508,  4512,
    4511,  4528,  4529,  4532,  4538,  4542,  4548,  4549,  4553,  4557,
    4563,  4567,  4573,  4579,  4586,  4592,  4599,  4603,  4609,  4613,
    4619,  4620,  4623,  4627,  4633,  4637,  4641,  4645,  4651,  4656,
    4661,  4665,  4669,  4673,  4677,  4681,  4685,  4689,  4693,  4697,
    4701,  4705,  4709,  4713,  4718,  4724,  4729,  4734,  4739,  4744,
    4751,  4755,  4762,  4767,  4766,  4778,  4782,  4788,  4796,  4804,
    4812,  4816,  4822,  4826,  4832,  4833,  4836,  4841,  4848,  4849,
    4852,  4856,  4862,  4866,  4872,  4878,  4878,  4885,  4886,  4892,
    4896,  4902,  4908,  4913,  4917,  4922,  4927,  4943,  4948,  4954,
    4955,  4956,  4959,  4960,  4961,  4962,  4965,  4966,  4967,  4970,
    4971,  4974,  4978,  4984,  4985,  4991,  4992,  4995,  4996,  4999,
    5002,  5003,  5004,  5007,  5008,  5011,  5016,  5019,  5020,  5024
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
  "')'", "';'", "'.'", "'\\n'", "$accept", "program", "$@1",
  "top_compstmt", "top_stmts", "top_stmt", "@2", "bodystmt", "compstmt",
  "stmts", "stmt", "$@3", "command_asgn", "command_rhs", "expr", "$@4",
  "$@5", "defn_head", "defs_head", "$@6", "expr_value", "command_call",
  "block_command", "cmd_brace_block", "$@7", "command", "mlhs",
  "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list", "mlhs_post",
  "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym", "undef_list",
  "$@8", "op", "reswords", "arg", "aref_args", "arg_rhs", "paren_args",
  "opt_paren_args", "opt_call_args", "call_args", "command_args", "@9",
  "block_arg", "opt_block_arg", "comma", "args", "mrhs", "primary", "@10",
  "@11", "$@12", "$@13", "@14", "@15", "$@16", "$@17", "$@18", "$@19",
  "$@20", "$@21", "@22", "@23", "@24", "@25", "primary_value", "then",
  "do", "if_tail", "opt_else", "for_var", "f_margs", "$@26",
  "block_args_tail", "opt_block_args_tail", "block_param",
  "opt_block_param", "block_param_def", "$@27", "opt_bv_decl", "bv_decls",
  "bvar", "f_larglist", "lambda_body", "do_block", "@28", "block_call",
  "method_call", "brace_block", "@29", "@30", "case_body", "cases",
  "in_clauses", "$@31", "$@32", "$@33", "p_expr", "p_args_head",
  "p_args_post", "p_as", "p_alt", "p_value", "p_array", "p_array_body",
  "p_array_elems", "p_rest", "p_const", "p_hash", "p_hash_body",
  "p_hash_elems", "p_hash_elem", "p_kwrest", "p_var", "opt_rescue",
  "exc_list", "exc_var", "opt_ensure", "literal", "string",
  "string_fragment", "string_rep", "string_interp", "@34", "xstring",
  "regexp", "heredoc", "heredoc_bodies", "heredoc_body",
  "heredoc_string_rep", "heredoc_string_interp", "@35", "words", "symbol",
  "basic_symbol", "sym", "symbols", "numeric", "variable", "var_lhs",
  "var_ref", "backref", "superclass", "$@36", "f_opt_arglist_paren",
  "f_arglist_paren", "f_arglist", "f_label", "f_kw", "f_block_kw",
  "f_block_kwarg", "f_kwarg", "kwrest_mark", "f_kwrest", "args_tail",
  "opt_args_tail", "f_args", "f_bad_arg", "f_norm_arg", "f_arg_item",
  "@37", "f_arg", "f_opt_asgn", "f_opt", "f_block_opt", "f_block_optarg",
  "f_optarg", "restarg_mark", "f_rest_arg", "blkarg_mark", "f_block_arg",
  "opt_f_block_arg", "singleton", "$@38", "assoc_list", "assocs", "assoc",
  "operation", "operation2", "operation3", "dot_or_colon", "call_op",
  "call_op2", "opt_terms", "opt_nl", "rparen", "trailer", "term", "nl",
  "terms", "none", YY_NULLPTR
};

static const char *
yysymbol_name (yysymbol_kind_t yysymbol)
{
  return yytname[yysymbol];
}
#endif

#define YYPACT_NINF (-967)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-690)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -967,   111,  3701,  -967,  9032, 11156, 11498,  7340,  -967, 10802,
   10802,  -967,  -967, 11270,  8522,  6956,  9268,  9268,  -967,  -967,
    9268,  4555,  4147,  -967,  -967,  -967,  -967,   -52,  8522,  -967,
      -4,  -967,  -967,  -967,  7482,  3998,  -967,  -967,  7624,  -967,
    -967,  -967,  -967,  -967,  -967,  -967,    79, 10920, 10920, 10920,
   10920,   102,  6215,    31,  9740, 10094,  8804,  -967,  8240,   879,
     434,   410,   912,   987,  -967,   603, 11038, 10920,  -967,  1330,
    -967,   741,  -967,   577,  1960,  1960,  -967,  -967,   133,    26,
    -967,    27, 11384,  -967,    71,  2668,   346,   630,   151,    68,
    -967,   389,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    -967,    23,   110,  -967,    38,   142,  -967,  -967,  -967,  -967,
    -967,    99,    99,   -52,   464,   928,  -967, 10802,   307,  6334,
     641,  2122,  2122,  -967,   160,  -967,   719,  -967,  -967,   142,
    -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,    37,    76,
     168,   189,  -967,  -967,  -967,  -967,  -967,  -967,   213,   248,
     267,   296,  -967,   300,  -967,  -967,  -967,  -967,  -967,  -967,
    -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,   303,
    5247,   235,   577,  1960,  1960,   799,   176,   727,    86,   220,
     219,   799, 10802, 10802,   739,   265,  -967,  -967,   866,   318,
      43,    92,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    -967,  8381,  -967,  -967,   263,  -967,  -967,  -967,  -967,  -967,
    -967,  1330,  -967,   518,  -967,   311,  -967,  -967,  1330,  4283,
      98, 10920, 10920, 10920, 10920,  -967, 13325,  -967,  -967,   268,
     364,   268,  -967,  -967,  -967,  9386,  -967,  -967,  -967,  9268,
    -967,  -967,  -967,  6956,  7194,  -967,   287,  6453,  -967,   927,
     327, 13449, 13449,   306,  9150,  6215,   308,  1330,   741,  1330,
     345,  -967,  9150,  1330,   329,   967,   967,  -967, 13325,   339,
     967,  -967,   426, 11612,   379,   948,  1039,  1103,  2188,  -967,
    -967,  -967,  -967,  -967,  1350,  -967,  -967,  -967,  -967,  -967,
    -967,   619,  1446,  -967,  -967,   782,  -967,   832,  -967,  1503,
    -967,  1510,   382,   433,  -967,  -967,  -967,  -967,  6718, 10802,
   10802, 10802, 10802,  9150, 10802, 10802,    78,  -967,  -967,  -967,
    -967,   485,  1330,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    1553,   439,   459,  5247, 10920,  -967,   446,   545,   448,  -967,
    1330,  -967,  -967,  -967,   470, 10920,  -967,   478,   555,   489,
     576,  -967,  -967,   521,  5247,  -967,  -967, 10212,  -967,  6215,
    8918,   504, 10212,  -967, 10920, 10920, 10920, 10920, 10920, 10920,
   10920, 10920, 10920, 10920, 10920, 10920, 10920, 10920,  -967, 10920,
   10920, 10920, 10920, 10920, 10920, 10920, 10920, 10920, 10920, 10920,
   10920,  2883,  -967,  9268,  -967,  3307,  -967,  -967, 13008,  -967,
    -967,  -967,  -967, 11038, 11038,  -967,   552,  -967,   577,  -967,
    1208,  -967,  -967,  -967,  -967,  -967,  -967, 11890,  9268, 11976,
    5247, 10802,  -967,  -967,  -967,   642,   660,   334,   567,   573,
    -967,  5393,   700, 10920, 12062,  9268, 12148, 10920, 10920,  5831,
     541,   541,   123, 12234,  9268, 12320,  -967,   613,  -967,  6453,
     195,  -967,  -967, 10330,   707,  -967, 10920, 10920, 13387, 13387,
   13387, 10920,  -967,  -967,  9504,  -967, 10920,  -967,  9858,  7075,
     591,  1330,   268,   268,  -967,  -967,  1251,   598,  -967,  -967,
    -967,  8522,  5950,   607, 12062, 12148, 10920,   741,  1330,  -967,
    -967,  6837,   617,   741,  -967,  -967,  9976,  -967,  1330, 10094,
    -967,  -967,  -967,  1208,    27, 11612,  -967, 11612, 12406,  9268,
   12492,  2306,  -967,  -967,   621,  -967,  1529,  6453,   619,  -967,
    -967,  -967,  -967,  -967,  -967,  -967, 10920, 10920,  -967,  -967,
    -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    1737,  1330,  1330,   623, 11038,   753, 13387,   659,  -967,  -967,
    -967,   167,  -967,  -967,  2286,  -967, 13387,  2306,  -967,  -967,
    1637,  -967,  -967, 11038,   761,   119, 10920,  -967, 13041,   268,
    -967,  1330, 11612,   644,  -967,  -967,  -967,   744,   672,  2791,
    -967,  -967,  1227,   367,  1426, 13529, 13529, 13529, 13529,  1908,
    1908, 13539, 13467, 13529, 13529, 13449, 13449,  1968,  1968,  1426,
     327, 13387,  1908,  1908,  1889,  1889,  1979,   539,   539,   327,
     327,   327,  4691,  7980,  4963,  8098,  -967,    99,  -967,   654,
     268,   325,  -967,   370,  -967,  -967,  4419,  -967,  -967,  2441,
     119,   119,  -967,  3042,  -967,  -967,  -967,  -967,  -967,  1330,
   10802,  5247,  1155,   247,  -967,    99,   669,    99,   785,  1251,
    8663,  -967, 10448,   798,  -967, 10920, 10920,   701,  -967,  7742,
    7861,   679,   392,   437,   798,  -967,  -967,  -967,  -967,    49,
     117,   680,   126,   145, 10802,  8522,   694,  1426,   813, 13387,
     689,  -967, 13387, 13387, 13387,   851, 10920, 13325,  -967,   268,
   13387,  -967,  -967,  -967,  -967,  9622,  9858,  -967,  -967,  -967,
     697,  -967,  -967,   192,   741,  1330,   967,   504,  -967,  1155,
     247,   699,  1165,  1247,  -967,    41,  2306,  -967,   702,  -967,
     327,   327,  -967,  -967,  1102,  1330,   703,  -967,  -967,  2463,
     806,  3157,  -967,   796,   485,  -967,   448,  -967,  1330,  -967,
    -967,   712,   726,   730,  -967,   738,   796,   730,   837,  3778,
    -967,  -967,  2306,  5247,  -967,  -967, 13112, 10566,  -967,  -967,
   11612,  9150, 11038, 10920, 12578,  9268, 12664,  -967,  -967,  -967,
     831,  -967,  -967,   834,  1395,    56,   839,   154,   844,  -967,
    1719,   758,   286,  -967,  -967,   764,   817,  -967,   766,  -967,
    -967,  -967,  -967,  -967,  -967,   606, 11038, 11038,  -967,   552,
     373,  9504, 11038, 11038,  -967,   552,    68,   133,  5247,  6453,
     119,  -967,  1330,   895,  -967,  -967,  -967,  -967, 13041,  -967,
     822,  -967,  6096,   902,  -967, 10802,   908,  -967, 10920, 10920,
     486, 10920, 10920,   909,  6599,  6599,   149,   541,  -967,  -967,
     786,  -967, 10684,  5539, 13387,  -967,  7075,   268,  -967,  -967,
    -967,   773,   790,  1517,  5247,  6453,  -967,  -967,  -967,   804,
    -967,  1809,  1330, 10920, 10920,  -967,  -967,  2306,  -967,  1637,
    -967,  1637,  -967,  1637,  -967,  -967, 10920, 10920,  -967,  -967,
    -967, 11726,  -967,   811,   448,   825, 11726,  -967,   836,   838,
    -967,   940, 10920, 13183,  -967,  -967, 13387,  4827,  5099,   842,
     492,   531,  1822,  -967,  -967,  -967,  -967,   847,   845,   849,
     831,  -967,   788,   858,  -967,  -967,  -967,  -967,  -967,   861,
     863,  -967,   922,  1822,  1822,   934,    62, 10920, 10920,  -967,
    -967,  -967,  -967,  -967, 11038,  -967,  -967,  -967,  -967,  -967,
    -967,  -967,   992,   875,  6453,  5247,  -967,  -967, 11840,   799,
    -967,  -967,  6599,  -967,  -967,   799,  -967, 10920,  -967,  1001,
    1005,  -967, 10802, 10802,  5685, 13387,   443,  -967,  9858,  -967,
    2020,  1006,   886,  1931,  1931,  1267,  -967, 13387, 13387,   730,
     884,   730,   730, 13387, 13387,   905,   910,   979,  1278,   659,
    -967,  -967,  1486,  -967,  1278,  2306,  -967,  1637,  -967,  -967,
   13254,   556,  -967,  -967,  1719,  1822,  -967,    62,  -967,  1822,
    -967,  -967,   898,  -967,  -967,  -967,  -967, 13387, 13387,  -967,
    -967,  -967,  -967,   896,  1027,   988,  -967,  1300,  1039,  1103,
    5247,  -967,  5393,  -967,  -967,  6599,   799,   799,   341,  -967,
    -967,  -967,  -967,   904,  -967,  -967,  -967,  -967,   906,   906,
    1931,   914,  -967,  1637,  -967,  -967,  -967,  -967,  -967,  -967,
   12750,  -967,   448,   659,  -967,  -967,   915,   920,   921,  -967,
     931,   921,  -967,   937,   938,  -967,   898,  1822,  -967,  -967,
    1208, 12836,  9268, 12922,   660,   701,  1032,  5685,  5685,  1426,
    -967,  1068,  2020,   851,  1931,   906,  1931,   730,   947,   950,
    -967,  2306,  -967,  1637,  -967,  1637,  -967,  1637,  -967,  -967,
    1822,  1719,  -967,  1155,   247,   913,   846,   893,  -967,  -967,
    -967,   341,   341,   709,  -967,  -967,   906,  -967,   921,   959,
     921,   921,   960,  -967,   773,  1092,  1093,   799,  1070,  1071,
    -967,  1637,  -967,  -967,  -967,  1822,  -967,  -967,  5685, 10802,
   10802,   921,   341,   799,   799,  -967,  -967,  5685,  5685,   341,
     341,  -967,  -967
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   297,     0,
       0,   321,   324,     0,     0,   675,   344,   345,   346,   347,
     309,   273,   273,   560,   559,   561,   562,   677,     0,    10,
       0,   564,   563,   565,   550,   661,   552,   551,   554,   553,
     546,   547,   507,   508,   566,   567,   558,     0,     0,     0,
       0,     0,     0,   299,   689,   689,    91,   316,     0,     0,
       0,     0,     0,     0,   522,     0,     0,     0,     3,   675,
       6,     9,    27,    32,   614,   614,    48,    63,    62,     0,
      79,     0,    83,    93,     0,    57,   251,     0,    64,   314,
     289,   290,   505,   291,   292,   293,   503,   502,   534,   504,
     501,   557,     0,   294,   295,   273,     5,     8,   344,   345,
     309,   689,   420,     0,   116,   117,   558,     0,     0,     0,
       0,   614,   614,   119,   568,   348,     0,   557,   295,     0,
     340,   171,   181,   172,   168,   197,   198,   199,   200,   179,
     194,   187,   177,   176,   192,   175,   174,   170,   195,   169,
     182,   186,   188,   180,   173,   189,   196,   191,   190,   183,
     193,   178,   167,   185,   184,   166,   164,   165,   161,   162,
     163,   121,   123,   122,   156,   157,   134,   135,   136,   143,
     140,   142,   137,   138,   158,   159,   144,   145,   149,   152,
     153,   139,   141,   131,   132,   133,   146,   147,   148,   150,
     151,   154,   155,   160,   645,    58,   124,   125,   644,     0,
       0,     0,    61,   614,   614,     0,     0,     0,   557,     0,
     295,     0,     0,     0,   115,     0,   359,   358,     0,     0,
     557,   295,   190,   183,   193,   178,   161,   162,   163,   121,
     122,     0,   126,   128,    20,   127,   525,   530,   529,   683,
     685,   675,   686,     0,   527,     0,   687,   684,   676,   659,
     558,   281,   658,   276,     0,   268,   280,    77,   272,   689,
     505,   689,   649,    78,    76,   689,   262,   310,    75,     0,
     261,   419,    74,   675,     0,    18,     0,     0,   224,     0,
     225,   212,   215,   306,     0,     0,     0,   675,    15,   675,
      81,    14,     0,   675,     0,   680,   680,   252,     0,     0,
     680,   647,     0,     0,    89,     0,    99,   106,   614,   540,
     539,   541,   542,   536,     0,   538,   537,   509,   514,   513,
     516,     0,     0,   511,   518,     0,   520,     0,   532,     0,
     544,     0,   548,   549,    52,   239,   240,     4,   676,     0,
       0,     0,     0,     0,     0,     0,   621,   617,   616,   615,
     618,   619,     0,   623,   635,   590,   591,   639,   638,   634,
     614,     0,   576,     0,   583,   588,   689,   593,   689,   613,
       0,   620,   622,   625,   599,     0,   632,   599,   637,   599,
     641,   597,   572,     0,     0,   407,   409,     0,    95,     0,
      87,    84,     0,    55,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   211,   214,     0,     0,    53,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   672,   689,   671,     0,   674,   673,     0,   424,
     422,   315,   506,     0,     0,   413,    68,   313,   337,   116,
     117,   118,   548,   549,   576,   569,   335,     0,   689,     0,
       0,     0,   670,   669,    59,     0,   689,   306,     0,     0,
     350,     0,   349,     0,     0,   689,     0,     0,     0,     0,
       0,     0,   306,     0,   689,     0,   332,     0,   129,     0,
       0,   526,   528,     0,     0,   688,   653,   654,   282,   657,
     275,     0,   677,   269,     0,   278,     0,   270,     0,   675,
       0,   675,   689,   689,   263,   274,   675,     0,   312,    51,
     678,     0,     0,     0,     0,     0,     0,    17,   675,   304,
      13,   676,    80,   300,   303,   307,   682,   253,   681,   682,
     255,   308,   648,   105,    97,     0,    92,     0,     0,   689,
       0,   614,   317,   404,   599,   543,     0,     0,   517,   523,
     510,   512,   519,   521,   533,   545,     0,     0,     7,    21,
      22,    23,    24,    25,    49,    50,   580,   627,   581,   579,
       0,   675,   675,   599,     0,     0,   582,     0,   595,   643,
     592,     0,   596,   577,     0,   606,   628,     0,   609,   636,
       0,   611,   640,     0,     0,   689,   281,    28,    30,     0,
      31,   675,     0,    85,    96,    47,    33,    45,     0,   256,
     201,    29,     0,   295,     0,   229,   234,   235,   236,   231,
     233,   243,   244,   237,   238,   210,   213,   241,   242,     0,
     221,   677,   230,   232,   226,   227,   228,   216,   217,   218,
     219,   220,   662,   667,   663,   668,   418,   273,   416,     0,
     689,   662,   664,   663,   665,   417,   273,   662,   663,   273,
     689,   689,    34,   256,   202,    44,   209,    66,    69,     0,
       0,     0,   116,   117,   120,     0,     0,   689,     0,   675,
       0,   298,   689,   689,   493,     0,     0,   689,   351,   666,
     305,     0,   662,   663,   689,   353,   322,   352,   325,   666,
     305,     0,   662,   663,     0,     0,     0,     0,     0,   280,
       0,   328,   652,   655,   651,   279,   284,   283,   277,   689,
     656,   650,   260,   258,   264,   265,   267,   311,   679,    19,
       0,    26,   208,    82,    16,   675,   680,    98,    90,   102,
     104,     0,   101,   103,   677,     0,     0,   535,     0,   524,
     222,   223,   621,   619,   367,   675,   360,   575,   573,     0,
      40,   247,   342,     0,     0,   589,   689,   642,     0,   598,
     626,   599,   599,   599,   633,   599,   621,   599,    42,   249,
     343,   395,   393,     0,   392,   391,   288,     0,    94,    88,
       0,     0,     0,     0,     0,   689,     0,   457,   458,   459,
     491,   476,   456,     0,     0,     0,   475,   490,     0,    56,
       0,   436,   449,   451,   461,   440,   460,   462,   442,   484,
     444,   453,   455,   454,    54,     0,     0,     0,   415,    72,
     421,   265,     0,     0,   414,    70,   410,    65,     0,     0,
     689,   338,     0,     0,   421,   341,   646,    60,   494,   495,
     689,   496,     0,   689,   356,     0,     0,   354,     0,     0,
     421,     0,     0,     0,     0,     0,   421,     0,   130,   531,
       0,   327,     0,     0,   285,   271,   675,   689,    11,   301,
     254,   100,     0,   397,     0,     0,   318,   515,   368,   365,
     624,     0,   675,     0,     0,   594,   578,     0,   602,     0,
     604,     0,   610,     0,   607,   612,     0,     0,   390,   677,
     677,   585,   586,   689,   689,   375,     0,   630,   375,   375,
     373,     0,   284,   286,    86,    46,   257,   662,   663,     0,
     662,   663,   487,   478,   491,   465,   472,     0,   466,   469,
       0,   480,     0,   481,   483,   474,   489,   488,   463,   437,
     438,   445,     0,     0,     0,     0,     0,     0,     0,    39,
     206,    38,   207,    73,     0,    36,   204,    37,   205,    71,
     411,   412,     0,     0,     0,     0,   570,   336,     0,     0,
     498,   357,     0,    12,   500,     0,   319,     0,   320,     0,
       0,   333,     0,     0,     0,   283,   689,   259,   266,   403,
       0,     0,     0,     0,     0,   363,   574,    41,   248,   599,
     599,   599,   599,    43,   250,     0,     0,     0,   584,     0,
     371,   372,   375,   383,   629,     0,   386,     0,   388,   408,
     287,   421,   486,   464,     0,     0,   479,     0,   446,     0,
     450,   452,   441,   447,   477,   485,   443,   246,   245,    35,
     203,   425,   423,     0,     0,     0,   497,     0,   107,   114,
       0,   499,     0,   323,   326,     0,     0,     0,   689,   427,
     428,   426,   401,   677,   399,   402,   406,   405,   369,   366,
       0,   361,   603,     0,   600,   605,   608,   396,   394,   306,
       0,   587,   689,     0,   374,   381,   375,   375,   375,   631,
     375,   375,   473,   467,   470,   482,   439,     0,    67,   339,
     113,     0,   689,     0,   689,   689,     0,     0,     0,     0,
     429,     0,     0,   398,     0,   364,     0,   599,   666,   305,
     370,     0,   378,     0,   380,     0,   387,     0,   384,   389,
       0,     0,   448,   110,   112,     0,   662,   663,   492,   355,
     334,   689,   689,   430,   329,   400,   362,   601,   375,   375,
     375,   375,   468,   471,   108,     0,     0,     0,     0,     0,
     379,     0,   376,   382,   385,     0,   330,   331,     0,     0,
       0,   375,   689,     0,     0,   377,   431,     0,     0,   689,
     689,   433,   435
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -967,  -967,  -967,   589,  -967,    32,  -967,  -293,    77,  -967,
      22,  -967,  -257,  -413,    -1,  -967,  -967,  1277,  1502,  -967,
      -5,   -24,  -967,  -656,  -967,    44,  1101,  -196,     4,   -60,
    -270,  -508,     2,  2202,   -89,  1115,    13,   -17,  -967,  -967,
     124,  -967,  3092,  -967,  1133,   272,  -144,  -408,   120,   -16,
    -967,  -426,  -258,   586,   106,  -385,    42,  -967,  -967,  -967,
    -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,  -967,
    -967,  -967,  -967,   222,  -184,  -466,     6,  -602,  -967,  -967,
    -967,   340,   328,  -967,  -584,  -967,  -967,  -164,  -967,     1,
    -967,  -967,   288,  -967,  -967,  -967,   -88,  -967,  -967,  -457,
    -967,    39,  -967,  -967,  -967,  -611,  -967,    85,  -717,  -967,
     177,  -967,  -967,  -949,  -755,  -967,  -967,  -967,   326,  -866,
    -722,  -967,    21,  -967,  -967,  -967,  -967,  -967,  1158,    58,
    -227,  -967,  -967,  -967,  -967,  -967,  -274,  -967,   911,  -967,
    -967,   783,    -7,  -967,  -967,   881,   244,  2418,  1160,  1834,
    -967,  -967,     0,   651,    50,   198,   581,   143,  -967,  -967,
    -967,  -351,    88,   343,  -237,  -959,  -698,  -540,  -967,  -162,
    -702,  -585,  -966,   141,  -332,  -967,  -400,  -967,   371,  -360,
    -967,  -967,  -967,    29,  -476,   867,  -397,  -967,  -967,   -85,
    -967,    70,   -23,  -276,  -266,    60,  -268,   -34,    -2
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,     2,    68,    69,    70,   286,   465,   466,   297,
     298,   521,    72,   616,    73,   639,   624,   213,   214,   690,
     215,    76,    77,   678,   850,    78,    79,   299,    80,    81,
      82,   546,    83,   216,   123,   124,   242,   243,   244,   715,
     655,   207,    85,   304,   620,   656,   277,   510,   511,   278,
     279,   268,   503,   539,   660,   610,    86,   210,   302,   745,
     303,   318,   755,   222,   874,   223,   875,   714,  1075,   681,
     679,   985,   460,   289,   471,   706,   866,  1130,   229,   765,
    1013,  1104,  1033,   919,   793,   794,   920,   892,  1083,  1084,
     552,   896,   396,   605,    88,    89,   447,   671,   670,   494,
    1081,  1131,  1177,  1178,  1179,   819,   820,  1052,   821,   822,
     823,   824,   947,   948,   825,   826,   827,   952,   828,   829,
     830,   831,   693,   860,   989,   993,    90,    91,    92,   332,
     333,   557,    93,    94,    95,   558,   252,   253,   254,   489,
      96,    97,    98,   326,    99,   100,   218,   219,   103,   220,
     456,   680,   371,   372,   373,   374,   375,   922,   923,   376,
     377,   378,   779,   595,   380,   381,   382,   383,   580,   384,
     385,   386,   927,   928,   387,   388,   389,   390,   391,   588,
     209,   461,   309,   513,   272,   129,   685,   658,   464,   459,
     438,   517,   893,   518,   537,   256,   257,   258,   301
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     106,   441,   435,   437,   284,   221,   282,   245,   212,   212,
     251,   285,   784,   507,   212,   708,   520,   621,   592,   280,
     205,   245,   401,   532,    71,   659,    71,   534,   834,   451,
     672,   675,   731,   718,   657,   348,   107,   479,   666,   748,
     540,   669,   344,   545,   542,   271,   271,   125,   125,   271,
     686,  1085,   307,   311,   780,   125,   300,   559,   314,   949,
     265,   265,   687,   731,   265,   960,   899,   701,  -107,  1109,
     894,   325,   392,   392,  -110,   393,   711,   657,   728,   666,
     585,   553,   728,   306,   310,   255,   848,   849,   687,   446,
     926,   863,   283,   954,   439,   867,  1114,   946,   125,   528,
    1055,   604,   873,   959,   799,   561,   880,   950,   561,   280,
     561,     3,   561,   950,   561,  -555,   448,  -114,   335,   337,
     339,   341,   269,   269,   125,   394,   269,  -560,   687,   296,
     444,   206,   476,   582,   287,   576,   267,   273,   206,   347,
     274,   751,  -112,   485,  -107,   615,  -302,   397,  -113,  -302,
    -302,  -109,   206,   687,   895,   497,   554,   293,   817,  -114,
     305,   395,   379,   379,   817,   -99,  -559,   688,   439,   398,
    -111,  -556,   394,  1085,  -108,  1109,  -302,  -302,  -555,  -302,
    -106,  1055,   206,   973,  -560,   -99,   615,   615,   956,   979,
    -556,  -102,   402,  -662,   782,   951,   296,   785,   791,   577,
    -115,  1172,   443,   611,   525,   957,   440,  -107,   583,   379,
     379,   392,   392,   468,   469,   493,   780,   480,   481,  -115,
     717,   212,   212,  -559,    87,  1042,    87,   126,   126,   780,
     550,   217,   217,   732,  -106,   228,   776,   217,   217,   217,
     737,   436,   217,   275,  1056,   792,   101,  1053,   101,   127,
     127,   127,   743,   544,   325,   507,   432,   230,  -561,  -104,
     731,  -663,   781,   531,   394,  -105,   984,   505,  -101,   505,
     367,   538,   538,   514,    87,   472,   538,   545,   315,  -562,
     440,   472,   516,   519,   759,   455,   275,  -103,   217,  1113,
     467,  -100,   934,   276,   281,   368,   101,   473,   434,   300,
     316,   379,   379,  -564,   315,   767,   768,   859,   271,   728,
     728,   478,   477,   -80,   754,  -561,   527,  1091,   495,   265,
     482,   490,   784,   265,   533,  1115,   316,  1112,   946,   561,
     926,   493,  1053,   926,   -94,   798,  -562,  -305,  -563,   217,
    -114,    87,   545,   486,   569,   570,   571,   572,   212,   212,
     212,   212,  -305,   574,   575,   125,   782,  -565,   449,   862,
    -564,   780,   450,   101,   523,   206,  1129,   530,   684,   782,
      71,   780,   296,   607,   589,   573,   589,   445,   617,   962,
     568,   512,   556,   276,   281,   269,  -550,  -305,   853,   554,
    -554,  -663,   929,   462,  -305,  -563,  1173,   939,   526,   515,
    1152,   445,   613,   300,  1079,   488,   379,   657,   495,   666,
     502,  1001,   963,   856,  -565,   728,   905,   842,   766,   617,
     617,   506,   579,   969,   781,   522,   526,  -113,   419,   975,
     977,   514,    87,   946,  1112,   783,  -348,   781,   787,   926,
     593,   924,   125,  -550,   217,   217,  -109,  -554,  -105,    42,
     463,  -348,    43,   529,   101,  -113,   514,   520,   379,   444,
     689,   862,   843,   493,   694,   974,   -79,  -101,  1112,   889,
     535,   885,   336,   514,   328,   329,   296,   265,   541,   725,
     890,   543,   514,   566,   871,   747,  -348,   544,  -114,   900,
    -659,  -111,   780,  -348,  -108,   334,    59,   217,   328,   329,
     931,   217,   265,   609,   739,   217,   217,  1020,   609,    87,
     505,   505,  -103,  -109,   245,  -100,    87,    87,  1163,   265,
     106,   547,   844,   867,    87,   846,   330,   331,   265,   872,
     545,   101,   731,   729,   567,   315,   883,   806,   101,   101,
     707,   707,   578,   844,    71,   615,   101,   514,   697,  1080,
     330,   331,   544,   744,  -550,   982,   704,   316,  -111,   664,
     584,  1059,   664,  1030,  1031,   746,   716,   520,   705,  -550,
      87,   217,   217,   217,   217,    87,   217,   217,   997,   615,
    -571,   734,   728,   664,   842,   615,   615,   125,   587,   125,
     591,   495,   101,   265,   780,    87,   590,   101,   495,   720,
     664,  1011,  -659,   795,  -550,   780,   599,  -108,  -659,   664,
    1007,  -550,   594,  -109,   354,   355,    87,   101,   835,   217,
     597,    87,   315,   843,   622,   520,  1016,   602,   770,   885,
     925,   600,  1107,   491,   758,  1110,   247,   248,   101,   379,
     419,   839,   603,   101,   316,   206,   614,   788,   974,   664,
     845,   677,  -111,   847,   125,   217,   246,   691,   505,   247,
     248,   342,   343,   967,   280,   622,   622,   280,   795,   795,
     428,   429,   430,   692,   664,   852,   555,  -108,  1102,   212,
     217,   924,    87,   217,   924,   280,   924,   249,   695,   250,
     861,   864,  1064,    87,   696,   864,  1004,   217,   878,   452,
     453,    87,   864,   857,   101,   470,   217,   665,   245,   877,
     773,    87,   538,   212,  1155,   101,   698,   615,   865,   862,
     431,   246,   721,   101,   247,   248,   687,   505,   774,   968,
     598,   665,   601,   101,   246,   432,   733,   247,   248,   851,
     544,  1169,  1140,   738,    87,  1019,   741,  1021,   665,  -432,
    -434,  1022,   924,    87,   250,  1025,  1026,   665,   980,   -94,
     365,   366,   367,   756,   887,   769,   101,   315,   772,   315,
     433,   217,   454,   454,   589,   101,   790,   434,   617,    87,
     472,   349,   350,   351,   352,   353,   800,   368,   801,   316,
     924,   316,   924,   802,   924,   840,   924,   665,   609,  -421,
     855,   101,   470,   514,   246,  1070,   217,   247,   248,   457,
     854,  1072,   617,   971,   206,   470,   862,   474,   617,   617,
     870,   876,   665,   935,   432,   217,  1002,  1003,   881,  -558,
     924,   502,   432,   879,   315,   249,   888,   250,   906,   206,
     891,   897,   125,   562,  -558,   901,   328,   329,   795,   265,
     903,  1088,  1089,   576,   907,   504,   316,   508,   990,   458,
     995,   994,  -109,  -421,   212,   520,   434,   475,   909,   105,
     887,   105,   911,  1108,   434,  1111,   105,   105,  -421,  -558,
     913,   916,   105,   105,   105,   505,  -558,   105,   942,   943,
     955,   536,  1127,  1128,   563,   958,   328,   329,   330,   331,
     961,   246,   217,    87,   247,   248,   964,   965,   966,  -111,
     987,  -421,   986,  -421,   246,   988,   992,   247,   248,   105,
    -421,   589,   589,   996,   998,   101,   983,  1046,  1135,   838,
     664,  1137,   249,   105,   250,  1009,   217,   707,   276,   991,
     472,   276,   327,   328,   329,   249,  1014,   250,   330,   331,
     617,   999,  1000,  1029,  -660,  1039,   483,   838,   777,   276,
    1006,  -109,   777,   125,  -109,  -109,   246,  1032,   125,   247,
     248,   432,  1012,  1050,  1166,   338,   328,   329,  1035,  1168,
    1037,  1170,  1123,  1041,   105,  1171,   105,  1044,  1043,  1054,
     921,  1045,  -109,  1188,  -109,   330,   331,  1076,  1077,   250,
    1047,   212,   212,  1048,   864,  1049,   484,  1061,  -111,  1197,
    1198,  -111,  -111,   434,  1062,    87,  1073,   524,  -554,  1191,
    1074,  1086,   315,    87,   622,  1087,  1093,   217,   330,   331,
     125,  1097,   432,  -554,  1099,  1118,  1098,   101,   548,  -111,
    1117,  -111,  1119,  1120,   316,   101,  1132,  1160,  1134,   472,
     340,   328,   329,   432,  1174,   472,  1136,  1141,   622,   217,
    1133,  1063,  1143,  1145,   622,   622,  -660,   475,  -554,  1071,
      87,    87,  -660,  1147,   434,  -554,   864,   105,   665,  1150,
    1151,  1078,   246,  1164,    87,   247,   248,   217,   549,   105,
     105,  -662,   101,   101,  -663,   434,    87,    87,   735,   736,
     589,  1181,  1185,   330,   331,    87,   101,  1186,  1187,   502,
    1189,   740,  1190,   249,   226,   250,    87,    87,   101,   101,
     514,   130,   694,   864,   908,   910,   912,   101,   914,  -557,
     915,  1159,   918,  1165,  1116,   981,   472,   472,   101,   101,
    1051,   953,   105,  1028,  -557,  1158,   105,  1124,  1034,  1125,
     105,   105,  1126,   762,   105,   357,   358,   359,   360,   864,
     864,   105,   105,   930,   492,   127,   265,   208,   775,   105,
     127,   763,  1101,  1106,   270,   270,     0,     0,   270,  -557,
       0,  -666,     0,     0,  1193,  1194,  -557,     0,   212,   212,
     864,  -662,     0,  -295,     0,   797,   622,   864,   864,     0,
    1175,  1176,     0,     0,  1161,  1162,    87,    87,  -295,     0,
    1067,     0,   270,   270,    87,   105,   105,   105,   105,   105,
     105,   105,   105,     0,   217,   217,    87,   921,   101,   101,
     921,  1196,  1068,   921,     0,   921,   101,   472,  1201,  1202,
     105,     0,     0,  -295,   898,  -666,   841,   664,   101,   442,
    -295,     0,     0,   472,   472,  -662,  1036,  1038,     0,     0,
    -666,   105,     0,     0,   105,  1192,   105,     0,     0,   105,
    -662,     0,     0,  -663,  1199,  1200,     0,     0,     0,    74,
       0,    74,   121,   121,     0,     0,     0,     0,   354,   355,
     121,     0,    87,  -666,    87,  -666,     0,    87,  -306,  -662,
     105,   921,  -666,  -662,     0,  -662,   882,     0,     0,  -662,
     105,   105,  -662,  -306,   101,   508,   101,   804,   762,   101,
     357,   358,   359,   360,     0,   105,     0,   105,   105,    74,
       0,     0,   432,   121,     0,     0,   763,  -663,   105,   921,
       0,   921,   105,   921,   217,   921,   105,     0,  -306,    87,
      87,   105,  -663,     0,     0,  -306,   105,     0,     0,   121,
    1105,     0,  1092,  1094,  1095,  1096,   246,   805,  1100,   247,
     248,   101,   101,     0,   434,     0,     0,     0,     0,   921,
       0,     0,     0,   432,     0,  -663,     0,  -663,     0,   105,
    1121,  -663,     0,     0,  -663,   665,    74,   249,   105,   250,
     777,     0,     0,   930,     0,   432,   930,   832,   930,  1090,
      87,   217,   217,   555,   328,   329,   105,     0,   458,    87,
      87,     0,   832,     0,   105,   434,     0,     0,     0,   807,
     808,   809,   101,   270,  1142,  1144,  1146,   270,  1148,  1149,
    1122,   101,   101,     0,     0,   246,   944,   434,   247,   248,
     811,   105,     0,    40,    41,     0,     0,     0,   812,     0,
     807,   808,   809,     0,     0,     0,   330,   331,     0,     0,
     105,     0,     0,  1008,   930,     0,   249,   810,   250,     0,
    1167,   811,     0,     0,    40,    41,   813,    74,     0,   812,
       0,     0,   814,   815,     0,   816,  1180,  1182,  1183,  1184,
     832,    58,     0,     0,    75,   833,    75,   122,   122,   560,
     328,   329,   930,     0,   930,   122,   930,   813,   930,  1195,
     833,     0,   818,   814,   815,     0,   816,     0,   817,   120,
       0,     0,    58,     0,     0,     0,   945,   356,     0,   357,
     358,   359,   360,     0,     0,     0,     0,   105,   105,     0,
       0,     0,   930,   818,    75,   361,     0,     0,   122,     0,
     120,     0,   330,   331,    74,     0,   564,   328,   329,     0,
       0,    74,    74,   565,   328,   329,   674,   676,     0,    74,
     363,   105,     0,     0,   122,     0,   364,   365,   366,   367,
     121,   270,   757,   328,   329,     0,     0,   832,   833,     0,
       0,     0,     0,   832,   356,     0,   357,   358,   359,   360,
     674,   676,     0,     0,   368,     0,   270,   369,     0,   330,
     331,    75,   361,     0,     0,    74,   330,   331,  1103,     0,
      74,     0,   246,   270,     0,   247,   248,     0,   581,     0,
       0,     0,   270,     0,     0,   330,   331,   363,     0,     0,
      74,     0,     0,   364,   365,   366,   367,     0,     0,   742,
     105,     0,   270,  1010,     0,   250,   270,     0,   105,   105,
       0,    74,   105,     0,     0,     0,    74,   121,     0,    74,
       0,   368,     0,     0,   369,     0,     0,     0,   786,     0,
     357,   358,   359,   360,   270,   833,     0,   270,     0,     0,
       0,   833,     0,   105,   105,     0,   361,   270,     0,   105,
     105,     0,    75,     0,     0,   105,   105,     0,     0,     0,
      74,    74,     0,     0,     0,   832,     0,     0,     0,   105,
       0,   363,   105,     0,     0,     0,     0,    74,   365,   366,
     367,   105,   105,     0,     0,     0,   832,   832,    74,     0,
     105,     0,     0,   807,   808,   809,    74,     0,     0,     0,
       0,   105,   105,     0,     0,   368,    74,     0,     0,     0,
     944,     0,     0,     0,   811,     0,     0,    40,    41,     0,
       0,     0,   812,     0,     0,     0,     0,     0,   762,    75,
     357,   358,   359,   360,     0,     0,    75,    75,     0,    74,
       0,     0,     0,     0,    75,     0,   763,     0,    74,     0,
     813,     0,     0,     0,     0,   122,   814,   815,     0,   816,
       0,     0,   121,   833,   121,    58,     0,   832,   832,     0,
       0,   363,   832,     0,    74,     0,   104,   764,   104,   128,
     128,   105,     0,     0,   833,   833,   818,   231,     0,     0,
      75,   105,   105,   120,     0,    75,   807,   808,   809,   105,
     762,     0,   357,   358,   359,   360,     0,     0,     0,   105,
     105,   105,     0,   944,     0,    75,     0,   811,   763,     0,
      40,    41,     0,     0,     0,   812,   104,     0,     0,   121,
     317,     0,     0,   270,   270,     0,    75,     0,     0,     0,
     832,    75,   122,   363,    75,     0,     0,     0,     0,  1015,
       0,     0,   832,   813,     0,     0,   317,     0,     0,   814,
     815,     0,     0,     0,     0,   833,   833,     0,    58,     0,
     833,     0,     0,   832,   832,     0,     0,   105,     0,   105,
       0,     0,   105,     0,     0,    75,    75,     0,     0,   818,
       0,     0,     0,   104,     0,     0,   120,     0,    74,     0,
       0,     0,    75,   270,     0,     0,     0,     0,   832,   970,
     972,     0,     0,    75,     0,   976,   978,   416,   417,     0,
       0,    75,   762,     0,   357,   358,   359,   360,     0,   105,
     419,    75,     0,     0,   105,   105,   416,   417,   833,   270,
     763,   970,   972,     0,   976,   978,     0,     0,     0,   419,
     833,   356,     0,   357,   358,   359,   360,   425,   426,   427,
     428,   429,   430,     0,    75,   363,     0,     0,     0,   361,
       0,   833,   833,    75,   423,   424,   425,   426,   427,   428,
     429,   430,     0,   362,   104,     0,     0,   122,     0,   122,
       0,     0,     0,     0,   363,   105,   105,   105,     0,    75,
     364,   365,   366,   367,   105,   105,   833,   416,   417,   419,
      74,  1082,     0,   357,   358,   359,   360,   121,    74,    74,
     419,  -689,     0,     0,     0,     0,     0,     0,   368,   763,
       0,   369,     0,     0,     0,     0,     0,   426,   427,   428,
     429,   430,     0,     0,   370,     0,     0,  1060,   426,   427,
     428,   429,   430,    74,   122,     0,     0,     0,     0,    74,
      74,   104,     0,     0,     0,    74,    74,     0,   104,   104,
    1060,     0,     0,     0,     0,     0,   104,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,   317,     0,     0,
       0,    74,    74,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,   270,     0,     0,     0,
       0,    74,    74,   356,     0,   357,   358,   359,   360,     0,
       0,     0,   104,    75,     0,     0,     0,   104,     0,     0,
       0,   361,     0,     0,     0,     0,     0,     0,   121,     0,
       0,     0,     0,   121,    84,   362,    84,   104,     0,     0,
       0,     0,     0,     0,     0,   227,   363,     0,     0,     0,
       0,     0,   364,   365,   366,   367,     0,     0,   104,     0,
       0,     0,     0,   104,   317,     0,   623,     0,     0,   356,
       0,   357,   358,   359,   360,     0,     0,     0,     0,     0,
     368,    74,     0,   369,    84,     0,     0,   361,     0,     0,
       0,    74,    74,     0,     0,   121,   370,     0,     0,    74,
       0,     0,     0,     0,     0,     0,     0,   623,   623,     0,
     270,    74,   363,     0,     0,     0,     0,     0,   364,   365,
     366,   367,     0,     0,   104,    75,     0,     0,     0,     0,
       0,     0,   122,    75,    75,   104,     0,     0,     0,     0,
       0,     0,     0,   104,     0,     0,   368,     0,     0,   369,
       0,    84,     0,   104,     0,     0,     0,     0,     0,     0,
       0,     0,   551,     0,     0,     0,     0,   356,    75,   357,
     358,   359,   360,     0,    75,    75,     0,    74,     0,    74,
      75,    75,    74,     0,     0,   361,   104,   356,     0,   357,
     358,   359,   360,     0,    75,   104,     0,     0,     0,     0,
       0,   778,     0,     0,     0,   361,    75,    75,     0,   317,
     363,   317,     0,     0,     0,    75,   364,   365,   366,   367,
       0,   104,     0,     0,     0,     0,    75,    75,     0,     0,
     363,     0,     0,     0,    74,    74,   364,   365,   366,   367,
       0,     0,    84,     0,   368,     0,     0,   369,     0,     0,
     102,     0,   102,   122,     0,     0,     0,     0,   122,     0,
       0,     0,     0,     0,   368,     0,     0,   369,     0,     0,
       0,  -689,     0,     0,     0,     0,   317,     0,     0,     0,
       0,     0,     0,     0,  -689,  -689,  -689,  -689,  -689,  -689,
       0,  -689,     0,     0,     0,    74,  -689,  -689,  -689,     0,
     102,     0,     0,     0,    74,    74,    75,     0,  -689,  -689,
       0,  -689,  -689,  -689,  -689,  -689,    75,    75,     0,    84,
     122,     0,     0,     0,    75,     0,    84,    84,     0,     0,
       0,     0,     0,     0,    84,     0,    75,     0,     0,     0,
       0,     0,     0,     0,   356,   104,   357,   358,   359,   360,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -689,   361,     0,     0,     0,     0,   102,     0,     0,
       0,     0,     0,     0,     0,     0,  -689,     0,   902,     0,
      84,     0,     0,     0,     0,    84,  -689,   363,     0,  -689,
    -689,     0,     0,   364,   365,   366,   367,     0,     0,     0,
       0,     0,    75,     0,    75,    84,     0,    75,     0,  -689,
    -689,     0,     0,     0,     0,   275,  -689,  -689,  -689,  -689,
       0,   368,     0,     0,   369,     0,    84,     0,     0,     0,
       0,    84,     0,     0,   618,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   104,   102,    75,
      75,     0,     0,     0,   317,   104,   623,     0,     0,     0,
       0,     0,     0,     0,     0,   618,   618,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    84,     0,     0,     0,     0,     0,     0,     0,
     623,     0,     0,    84,     0,     0,   623,   623,     0,     0,
       0,    84,   104,   104,     0,     0,     0,     0,     0,     0,
      75,    84,     0,   403,     0,     0,   104,     0,     0,    75,
      75,     0,     0,     0,     0,   102,     0,     0,   104,   104,
       0,     0,   102,   102,     0,     0,     0,   104,     0,     0,
     102,     0,     0,     0,    84,     0,     0,     0,   104,   104,
       0,     0,     0,    84,     0,     0,     0,     0,     0,     0,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,   415,     0,     0,     0,   128,   416,   417,     0,    84,
     128,   418,     0,     0,     0,     0,   102,     0,     0,   419,
       0,   102,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,   102,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,     0,     0,     0,     0,     0,     0,   623,     0,
       0,     0,   102,     0,     0,     0,     0,   102,   104,   104,
     102,     0,  1069,     0,     0,     0,   104,     0,     0,     0,
       0,     0,     0,     0,     0,   803,     0,     0,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   102,   102,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,   415,     0,     0,     0,   102,   416,
     417,     0,     0,    84,     0,     0,     0,     0,     0,   102,
       0,     0,   419,     0,     0,     0,     0,   102,     0,     0,
       0,     0,     0,     0,   104,     0,   104,   102,     0,   104,
       0,     0,     0,   420,     0,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,     0,     0,     0,     0,     0,
       0,     0,     0,  -280,   652,   653,     0,     0,   654,     0,
     102,     0,     0,     0,     0,     0,     0,     0,     0,   102,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,   104,   104,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,   102,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    84,     0,     0,     0,     0,
       0,     0,     0,    84,   618,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
       0,     0,   104,     0,     0,     0,   203,   275,     0,     0,
       0,   104,   104,     0,     0,     0,     0,     0,   618,     0,
       0,     0,     0,     0,   618,   618,     0,     0,     0,     0,
      84,    84,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    84,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    84,     0,     0,
       0,     0,     0,     0,     0,    84,   803,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    84,     0,   102,
       0,     0,     0,     0,     0,     0,     0,     0,   266,   266,
       0,     0,   266,     0,   404,   405,   406,   407,   408,   409,
     410,   411,   412,   413,   414,   415,     0,     0,     0,     0,
     416,   417,     0,     0,     0,     0,     0,     0,     0,   288,
     290,   291,   292,   419,     0,     0,   266,   308,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   345,   346,
       0,     0,     0,     0,   420,     0,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   618,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,    84,     0,     0,
    1066,     0,     0,     0,    84,     0,     0,     0,     0,     0,
       0,   904,     0,     0,     0,     0,    84,     0,     0,     0,
       0,   102,     0,     0,     0,     0,     0,     0,     0,   102,
     102,     0,     0,     0,     0,     0,     0,     0,     0,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,     0,     0,     0,     0,   416,   417,     0,     0,     0,
       0,     0,     0,     0,   102,     0,     0,     0,   419,     0,
     102,   102,     0,     0,     0,     0,   102,   102,     0,     0,
       0,     0,    84,     0,    84,     0,     0,    84,     0,   420,
     102,   421,   422,   423,   424,   425,   426,   427,   428,   429,
     430,     0,   102,   102,     0,     0,     0,     0,     0,     0,
       0,   102,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   102,   102,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    84,
      84,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   498,   499,   500,   345,     0,   661,   662,
       0,     0,   663,     0,     0,     0,     0,   266,     0,     0,
       0,   266,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
      84,     0,   102,   184,   185,   186,   187,     0,     0,    84,
      84,     0,   102,   102,     0,     0,     0,   188,   189,   190,
     102,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   102,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,     0,     0,     0,     0,     0,     0,
     203,   275,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   586,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   596,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   102,   608,
     102,     0,     0,   102,   619,     0,   625,   626,   627,   628,
     629,   630,   631,   632,   633,   634,   635,   636,   637,   638,
       0,   640,   641,   642,   643,   644,   645,   646,   647,   648,
     649,   650,   651,     0,     0,   266,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   673,   673,     0,     0,     0,
       0,     0,     0,     0,     0,   102,   102,     0,     0,     0,
     266,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   673,     0,   266,     0,   673,
     673,     0,     0,     0,     0,     0,   266,     0,     0,     0,
       0,     0,     0,     0,     0,   719,     0,     0,   722,   723,
       0,     0,     0,   724,     0,     0,   727,     0,   730,     0,
     308,   292,     0,     0,     0,     0,   102,     0,     0,     0,
       0,     0,     0,     0,     0,   102,   102,     0,   673,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   727,     0,
       0,   308,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   266,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   760,   761,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   771,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   789,     0,     0,   796,     0,
       0,  -689,     4,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,     0,     0,     0,     0,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   858,    49,    50,   771,   789,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,  -689,     0,   884,  -689,
    -689,     0,   917,     0,     0,     0,     0,   727,   308,     0,
       0,     0,     0,     0,     0,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -689,     0,  -689,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,   415,     0,     0,     0,     0,   416,   417,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   933,
       0,     0,     0,     0,   673,   936,     0,   266,     0,     0,
     420,     0,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   673,   673,
       0,     0,     0,   727,   673,   673,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     673,   673,     0,   673,   673,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1005,     0,     0,     0,   292,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1017,  1018,     0,  -296,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1023,  1024,
       0,  -296,  -296,  -296,  -296,  -296,  -296,     0,  -296,     0,
       0,     0,     0,  -296,  1040,  -296,  -296,  -296,     0,     0,
       0,     0,     0,     0,     0,  -296,  -296,     0,  -296,  -296,
    -296,  -296,  -296,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -296,     0,     0,     0,  1057,
    1058,     0,     0,     0,     0,     0,   673,     0,     0,     0,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
    -296,  -296,     0,     0,     0,     0,  -296,  -296,  -296,   673,
       0,  -296,     0,     0,     0,     0,     0,  -296,     0,  -296,
     308,     0,     0,  -296,     0,     0,     0,     0,     0,     0,
       0,  -296,     0,  -296,     0,     0,  -296,  -296,     0,     0,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
    -296,  -296,     0,     0,     0,     0,     0,  -296,  -296,  -296,
    -296,     0,     0,  -296,  -296,  -296,  -296,  -420,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -420,  -420,  -420,  -420,  -420,  -420,     0,  -420,     0,     0,
       0,     0,  -420,  -420,  -420,  -420,     0,     0,     0,     0,
       0,     0,     0,     0,  -420,  -420,     0,  -420,  -420,  -420,
    -420,  -420,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   266,     0,     0,     0,     0,  -420,
    -420,  -420,  -420,  -420,  -420,  -420,  -420,  -420,  -420,  -420,
    -420,     0,     0,     0,     0,  -420,  -420,  -420,     0,     0,
    -420,     0,     0,     0,     0,     0,  -420,     0,  -420,     0,
       0,     0,  -420,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -420,     0,     0,  -420,  -420,     0,     0,  -420,
       0,  -420,  -420,  -420,  -420,  -420,  -420,  -420,  -420,  -420,
    -420,     0,     0,  -550,     0,  -420,  -420,  -420,  -420,  -420,
       0,   275,  -420,  -420,  -420,  -420,  -550,  -550,  -550,  -550,
    -550,  -550,     0,  -550,     0,     0,     0,     0,  -550,     0,
    -550,  -550,     0,     0,     0,     0,     0,     0,     0,     0,
    -550,  -550,     0,  -550,  -550,  -550,  -550,  -550,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     496,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -550,  -550,  -550,  -550,  -550,
    -550,  -550,  -550,  -550,  -550,  -550,  -550,     0,     0,     0,
       0,  -550,  -550,  -550,     0,  -550,  -550,     0,     0,     0,
       0,     0,  -550,     0,  -550,     0,     0,     0,  -550,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -550,     0,
       0,  -550,  -550,     0,  -550,  -550,     0,  -550,  -550,  -550,
    -550,  -550,  -550,  -550,  -550,  -550,  -550,     0,     0,  -689,
       0,     0,  -550,  -550,  -550,  -550,     0,     0,  -550,  -550,
    -550,  -550,  -689,  -689,  -689,  -689,  -689,  -689,     0,  -689,
       0,     0,     0,     0,  -689,  -689,  -689,  -689,     0,     0,
       0,     0,     0,     0,     0,     0,  -689,  -689,     0,  -689,
    -689,  -689,  -689,  -689,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -689,  -689,  -689,  -689,  -689,  -689,  -689,  -689,  -689,
    -689,  -689,  -689,     0,     0,     0,     0,  -689,  -689,  -689,
       0,     0,  -689,     0,     0,     0,     0,     0,  -689,     0,
    -689,     0,     0,     0,  -689,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -689,     0,     0,  -689,  -689,     0,
       0,  -689,     0,  -689,  -689,  -689,  -689,  -689,  -689,  -689,
    -689,  -689,  -689,     0,     0,  -689,     0,  -689,  -689,  -689,
    -689,  -689,     0,   275,  -689,  -689,  -689,  -689,  -689,  -689,
    -689,  -689,  -689,  -689,     0,  -689,     0,     0,     0,     0,
    -689,     0,  -689,  -689,     0,     0,     0,     0,     0,     0,
       0,     0,  -689,  -689,     0,  -689,  -689,  -689,  -689,  -689,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -689,  -689,  -689,
    -689,  -689,  -689,  -689,  -689,  -689,  -689,  -689,  -689,     0,
       0,     0,     0,  -689,  -689,  -689,     0,     0,  -689,     0,
       0,     0,     0,     0,  -689,     0,  -689,     0,     0,     0,
    -689,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -689,     0,     0,  -689,  -689,     0,     0,  -689,     0,  -689,
    -689,  -689,  -689,  -689,  -689,  -689,  -689,  -689,  -689,     0,
       0,  -666,     0,     0,  -689,  -689,  -689,  -689,     0,   275,
    -689,  -689,  -689,  -689,  -666,  -666,  -666,     0,  -666,  -666,
       0,  -666,     0,     0,     0,     0,  -666,  -666,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -666,  -666,
       0,  -666,  -666,  -666,  -666,  -666,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -666,  -666,  -666,  -666,  -666,  -666,  -666,
    -666,  -666,  -666,  -666,  -666,     0,     0,     0,     0,  -666,
    -666,  -666,     0,   836,  -666,     0,     0,     0,     0,     0,
       0,     0,  -666,     0,     0,     0,  -666,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -666,     0,     0,  -666,
    -666,     0,  -110,  -666,     0,  -666,  -666,  -666,  -666,  -666,
    -666,  -666,  -666,  -666,  -666,     0,     0,  -666,     0,  -666,
    -666,  -666,     0,  -102,     0,     0,  -666,  -666,  -666,  -666,
    -666,  -666,  -666,     0,  -666,  -666,     0,  -666,     0,     0,
       0,     0,  -666,  -666,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -666,  -666,     0,  -666,  -666,  -666,
    -666,  -666,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -666,
    -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,
    -666,     0,     0,     0,     0,  -666,  -666,  -666,     0,   836,
    -666,     0,     0,     0,     0,     0,     0,     0,  -666,     0,
       0,     0,  -666,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -666,     0,     0,  -666,  -666,     0,  -110,  -666,
       0,  -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,  -666,
    -666,     0,     0,  -305,     0,  -666,  -666,  -666,     0,  -666,
       0,     0,  -666,  -666,  -666,  -666,  -305,  -305,  -305,     0,
    -305,  -305,     0,  -305,     0,     0,     0,     0,  -305,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -305,  -305,     0,  -305,  -305,  -305,  -305,  -305,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -305,  -305,  -305,  -305,  -305,
    -305,  -305,  -305,  -305,  -305,  -305,  -305,     0,     0,     0,
       0,  -305,  -305,  -305,     0,   837,  -305,     0,     0,     0,
       0,     0,     0,     0,  -305,     0,     0,     0,  -305,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -305,     0,
       0,  -305,  -305,     0,  -112,  -305,     0,  -305,  -305,  -305,
    -305,  -305,  -305,  -305,  -305,  -305,  -305,     0,     0,  -305,
       0,     0,  -305,  -305,     0,  -104,     0,     0,  -305,  -305,
    -305,  -305,  -305,  -305,  -305,     0,  -305,  -305,     0,  -305,
       0,     0,     0,     0,  -305,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -305,  -305,     0,  -305,
    -305,  -305,  -305,  -305,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -305,  -305,  -305,  -305,  -305,  -305,  -305,  -305,  -305,
    -305,  -305,  -305,     0,     0,     0,     0,  -305,  -305,  -305,
       0,   837,  -305,     0,     0,     0,     0,     0,     0,     0,
    -305,     0,     0,     0,  -305,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -305,     0,     0,  -305,  -305,     0,
    -112,  -305,     0,  -305,  -305,  -305,  -305,  -305,  -305,  -305,
    -305,  -305,  -305,     0,     0,     0,     0,     0,  -305,  -305,
       0,  -305,     0,     0,  -305,  -305,  -305,  -305,   294,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    -689,  -689,  -689,     0,     0,  -689,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,    51,     0,
       0,    52,    53,     0,    54,    55,     0,    56,     0,     0,
       0,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,  -689,     0,     0,  -689,  -689,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    65,    66,    67,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -689,   294,  -689,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,  -689,     0,
    -689,  -689,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,  -689,     0,
       0,  -689,  -689,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -689,
     294,  -689,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,  -689,     0,     0,  -689,    15,  -689,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,  -689,     0,     0,  -689,  -689,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -689,   294,  -689,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
    -689,     0,     0,  -689,    15,     0,    16,    17,    18,    19,
    -689,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
    -689,     0,     0,  -689,  -689,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    65,
      66,    67,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -689,   294,  -689,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,  -689,     0,     0,  -689,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,  -689,     0,     0,  -689,
    -689,     4,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    65,    66,    67,     0,    15,
       0,    16,    17,    18,    19,     0,     0,  -689,     0,  -689,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,    51,     0,     0,    52,    53,     0,    54,    55,     0,
      56,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,  -689,     0,     0,  -689,  -689,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,    66,    67,     0,     0,  -689,
       0,     0,     0,     0,     0,     0,  -689,   294,  -689,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
    -689,  -689,     0,     0,     0,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,    51,     0,     0,
      52,    53,     0,    54,    55,     0,    56,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,  -689,     0,     0,  -689,  -689,   294,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
      65,    66,    67,     0,    15,     0,    16,    17,    18,    19,
       0,     0,  -689,     0,  -689,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,   295,
      53,     0,    54,    55,     0,    56,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
    -689,     0,     0,  -689,  -689,   294,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    65,
      66,    67,     0,    15,     0,    16,    17,    18,    19,     0,
    -689,  -689,     0,  -689,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,    51,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,  -689,
       0,     0,  -689,  -689,   294,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,    65,    66,
      67,     0,    15,     0,    16,    17,    18,    19,     0,  -689,
    -689,     0,  -689,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,  -689,     0,
       0,  -689,  -689,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,    67,
       0,     0,  -689,     0,     0,     0,     0,     0,     0,  -689,
     294,  -689,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,  -689,     0,     0,     0,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,  -689,     0,     0,  -689,  -689,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,    65,    66,    67,     0,    15,     0,    16,
      17,    18,    19,     0,     0,  -689,     0,  -689,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,    51,
       0,     0,    52,    53,     0,    54,    55,     0,    56,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,   246,     0,     0,   247,   248,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    65,    66,    67,     0,    15,     0,    16,    17,
      18,    19,     0,     0,   249,     0,   250,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,    51,     0,
       0,    52,    53,     0,    54,    55,     0,    56,     0,     0,
       0,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,   246,     0,     0,   247,   248,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,    65,    66,    67,     0,    15,     0,    16,    17,    18,
      19,     0,     0,   249,     0,   250,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    53,     0,    54,    55,     0,     0,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,   246,     0,     0,   247,   248,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
      65,    66,    67,     0,    15,     0,   108,   109,    18,    19,
       0,     0,   249,     0,   250,   110,   111,   112,    23,    24,
      25,    26,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      53,     0,    54,    55,     0,     0,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
     246,     0,     0,   247,   248,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,    65,
     264,    67,     0,    15,     0,    16,    17,    18,    19,     0,
       0,   249,     0,   250,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,   246,
       0,     0,   247,   248,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   250,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,     0,     0,     0,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
       0,     0,     0,     0,     0,   165,   166,   167,   168,   169,
     170,   171,   172,    36,    37,   173,    39,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   116,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,     0,     0,     0,
       0,     0,     0,   203,   204,  -659,  -659,  -659,  -659,  -659,
    -659,  -659,  -659,  -659,     0,     0,     0,     0,     0,     0,
       0,  -659,     0,  -659,  -659,  -659,  -659,     0,  -659,     0,
       0,     0,  -659,  -659,  -659,  -659,  -659,  -659,  -659,     0,
       0,  -659,     0,     0,     0,     0,     0,     0,     0,     0,
    -659,  -659,  -659,  -659,  -659,  -659,  -659,  -659,  -659,     0,
    -659,  -659,  -659,     0,     0,  -659,     0,     0,  -659,  -659,
       0,  -659,  -659,  -659,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -659,  -659,     0,     0,
       0,     0,     0,  -659,     0,     0,  -659,  -659,     0,  -659,
    -659,     0,  -659,     0,  -659,  -659,  -659,     0,  -659,  -659,
    -659,     0,  -659,  -659,  -659,     0,  -659,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -659,  -659,  -659,     0,
    -659,     0,     0,     0,     0,     0,  -659,  -660,  -660,  -660,
    -660,  -660,  -660,  -660,  -660,  -660,     0,     0,     0,     0,
       0,     0,     0,  -660,     0,  -660,  -660,  -660,  -660,     0,
    -660,     0,     0,     0,  -660,  -660,  -660,  -660,  -660,  -660,
    -660,     0,     0,  -660,     0,     0,     0,     0,     0,     0,
       0,     0,  -660,  -660,  -660,  -660,  -660,  -660,  -660,  -660,
    -660,     0,  -660,  -660,  -660,     0,     0,  -660,     0,     0,
    -660,  -660,     0,  -660,  -660,  -660,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -660,  -660,
       0,     0,     0,     0,     0,  -660,     0,     0,  -660,  -660,
       0,  -660,  -660,     0,  -660,     0,  -660,  -660,  -660,     0,
    -660,  -660,  -660,     0,  -660,  -660,  -660,     0,  -660,     0,
       0,     0,     0,     0,     0,  -662,  -662,  -662,  -662,  -662,
    -662,  -662,  -662,  -662,     0,     0,     0,     0,  -660,  -660,
    -660,  -662,  -660,  -662,  -662,  -662,  -662,     0,  -660,     0,
       0,     0,  -662,  -662,  -662,  -662,  -662,  -662,  -662,     0,
       0,  -662,     0,     0,     0,     0,     0,     0,     0,     0,
    -662,  -662,  -662,  -662,  -662,  -662,  -662,  -662,  -662,     0,
    -662,  -662,  -662,     0,     0,  -662,     0,     0,  -662,  -662,
       0,  -662,  -662,  -662,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -662,  -662,     0,     0,
       0,     0,     0,  -662,   868,     0,  -662,  -662,     0,  -662,
    -662,     0,  -662,     0,  -662,  -662,  -662,     0,  -662,  -662,
    -662,     0,  -662,  -662,  -662,     0,  -662,     0,     0,     0,
       0,     0,     0,  -110,  -663,  -663,  -663,  -663,  -663,  -663,
    -663,  -663,  -663,     0,     0,     0,  -662,  -662,  -662,     0,
    -663,     0,  -663,  -663,  -663,  -663,  -662,     0,     0,     0,
       0,  -663,  -663,  -663,  -663,  -663,  -663,  -663,     0,     0,
    -663,     0,     0,     0,     0,     0,     0,     0,     0,  -663,
    -663,  -663,  -663,  -663,  -663,  -663,  -663,  -663,     0,  -663,
    -663,  -663,     0,     0,  -663,     0,     0,  -663,  -663,     0,
    -663,  -663,  -663,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -663,  -663,     0,     0,     0,
       0,     0,  -663,   869,     0,  -663,  -663,     0,  -663,  -663,
       0,  -663,     0,  -663,  -663,  -663,     0,  -663,  -663,  -663,
       0,  -663,  -663,  -663,     0,  -663,     0,     0,     0,     0,
       0,     0,  -112,  -664,  -664,  -664,  -664,  -664,  -664,  -664,
    -664,  -664,     0,     0,     0,  -663,  -663,  -663,     0,  -664,
       0,  -664,  -664,  -664,  -664,  -663,     0,     0,     0,     0,
    -664,  -664,  -664,  -664,  -664,  -664,  -664,     0,     0,  -664,
       0,     0,     0,     0,     0,     0,     0,     0,  -664,  -664,
    -664,  -664,  -664,  -664,  -664,  -664,  -664,     0,  -664,  -664,
    -664,     0,     0,  -664,     0,     0,  -664,  -664,     0,  -664,
    -664,  -664,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -664,  -664,     0,     0,     0,     0,
       0,  -664,     0,     0,  -664,  -664,     0,  -664,  -664,     0,
    -664,     0,  -664,  -664,  -664,     0,  -664,  -664,  -664,     0,
    -664,  -664,  -664,     0,  -664,     0,     0,     0,     0,     0,
       0,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,  -665,
       0,     0,     0,     0,  -664,  -664,  -664,  -665,     0,  -665,
    -665,  -665,  -665,     0,  -664,     0,     0,     0,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,     0,     0,  -665,     0,     0,
       0,     0,     0,     0,     0,     0,  -665,  -665,  -665,  -665,
    -665,  -665,  -665,  -665,  -665,     0,  -665,  -665,  -665,     0,
       0,  -665,     0,     0,  -665,  -665,     0,  -665,  -665,  -665,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -665,  -665,     0,     0,     0,     0,     0,  -665,
       0,     0,  -665,  -665,     0,  -665,  -665,     0,  -665,     0,
    -665,  -665,  -665,     0,  -665,  -665,  -665,     0,  -665,  -665,
    -665,     0,  -665,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -665,  -665,  -665,     0,     0,     0,     0,     0,
       0,     0,  -665,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,     0,     0,     0,
     155,   156,   157,   232,   233,   234,   235,   162,   163,   164,
       0,     0,     0,     0,     0,   165,   166,   167,   236,   237,
     238,   239,   172,   319,   320,   240,   321,     0,     0,     0,
       0,     0,     0,   322,     0,     0,     0,     0,     0,   323,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,   324,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,     0,     0,     0,
       0,     0,     0,   203,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,     0,     0,
       0,   155,   156,   157,   232,   233,   234,   235,   162,   163,
     164,     0,     0,     0,     0,     0,   165,   166,   167,   236,
     237,   238,   239,   172,   319,   320,   240,   321,     0,     0,
       0,     0,     0,     0,   322,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,   487,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,     0,     0,
       0,     0,     0,     0,   203,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,     0,
       0,     0,   155,   156,   157,   232,   233,   234,   235,   162,
     163,   164,     0,     0,     0,     0,     0,   165,   166,   167,
     236,   237,   238,   239,   172,     0,     0,   240,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,   241,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,     0,
       0,     0,     0,     0,     0,   203,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
       0,     0,     0,   155,   156,   157,   232,   233,   234,   235,
     162,   163,   164,     0,     0,     0,     0,     0,   165,   166,
     167,   236,   237,   238,   239,   172,     0,     0,   240,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
       0,     0,     0,     0,     0,     0,   203,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,     0,    15,     0,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   312,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,   120,   108,
     109,    18,    19,     0,     0,     0,   313,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   312,
       0,     0,   119,    53,     0,    54,    55,     0,     0,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,     0,     0,     0,
       0,    15,   120,    16,    17,    18,    19,     0,     0,     0,
     612,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,    51,     0,     0,    52,    53,     0,    54,
      55,     0,    56,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,     0,    65,    66,    67,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,    51,     0,     0,    52,    53,     0,    54,    55,     0,
      56,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,    66,    67,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   259,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   260,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    53,     0,    54,    55,     0,   261,     0,
     262,   263,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,   264,    67,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,   259,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   260,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,   509,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    53,     0,    54,    55,     0,   261,     0,   262,   263,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   264,    67,    15,     0,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   259,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   260,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,   726,     0,   262,   263,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   264,
      67,    15,     0,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,   259,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,   260,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,   886,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    53,     0,    54,
      55,     0,   726,     0,   262,   263,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    65,   264,    67,    15,
       0,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   259,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,   260,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    53,     0,    54,    55,     0,
     261,     0,   262,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,   264,    67,    15,     0,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   259,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   260,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    53,     0,    54,    55,     0,     0,     0,
     262,   263,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,   264,    67,    15,     0,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,   259,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   260,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    53,     0,    54,    55,     0,   726,     0,   262,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   264,    67,    15,     0,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   259,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   260,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,   262,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   264,
      67,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    53,     0,    54,
      55,     0,   606,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    65,   264,    67,    15,
       0,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    53,     0,    54,    55,     0,
     261,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,   264,    67,    15,     0,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    53,     0,    54,    55,     0,   606,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,   264,    67,    15,     0,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    53,     0,    54,    55,     0,   932,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   264,    67,    15,     0,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,   726,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   264,
      67,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    53,     0,    54,
      55,     0,     0,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    65,    66,    67,    15,
       0,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    53,     0,    54,    55,     0,
       0,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,   264,    67,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    53,     0,    54,    55,     0,     0,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,   264,    67,    15,     0,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,   114,    35,    36,
      37,   115,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   117,     0,     0,   118,     0,     0,
     119,    53,     0,    54,    55,     0,     0,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
     120,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,   224,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   225,     0,     0,    52,    53,     0,    54,    55,     0,
      56,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,     0,    15,   120,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   312,     0,     0,   399,    53,
       0,    54,    55,     0,   400,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,   120,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   114,
      35,    36,    37,   115,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   118,
       0,     0,   119,    53,     0,    54,    55,     0,     0,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,    15,   120,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,   116,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   312,     0,     0,   399,    53,     0,    54,
      55,     0,     0,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   120,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1027,     0,     0,
     119,    53,     0,    54,    55,     0,     0,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
     120,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,   224,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1065,     0,     0,   119,    53,     0,    54,    55,     0,
       0,   682,   653,     0,    57,   683,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,   120,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   667,   662,     0,
       0,   668,     0,   203,   275,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   699,   653,     0,     0,   700,     0,   203,
     275,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   702,
     662,     0,     0,   703,     0,   203,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   709,   653,     0,     0,   710,
       0,   203,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,   712,   662,     0,     0,   713,     0,   203,   275,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   749,   653,     0,
       0,   750,     0,   203,   275,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   752,   662,     0,     0,   753,     0,   203,
     275,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   937,
     653,     0,     0,   938,     0,   203,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   940,   662,     0,     0,   941,
       0,   203,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,  1138,   653,     0,     0,  1139,     0,   203,   275,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,  1153,   653,     0,
       0,  1154,     0,   203,   275,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,  1156,   662,     0,     0,  1157,     0,   203,
     275,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   667,
     662,     0,     0,   668,     0,   203,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,   415,     0,     0,     0,     0,   416,
     417,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,   419,   201,   202,     0,     0,     0,     0,     0,
       0,   203,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   420,     0,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,     0,     0,     0,     0,     0,
       0,     0,     0,  -280,   404,   405,   406,   407,   408,   409,
     410,   411,   412,   413,   414,   415,     0,     0,     0,     0,
     416,   417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   420,     0,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,     0,     0,     0,     0,
       0,     0,     0,     0,  -282,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,     0,     0,     0,
       0,   416,   417,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,     0,   421,   422,   423,
     424,   425,   426,   427,   428,   429,   430,     0,     0,     0,
       0,     0,     0,     0,     0,  -283,   404,   405,   406,   407,
     408,   409,   410,   411,   412,   413,   414,   415,     0,     0,
       0,     0,   416,   417,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   420,     0,   421,   422,
     423,   424,   425,   426,   427,   428,   429,   430,     0,     0,
       0,     0,     0,     0,     0,     0,  -285,   404,   405,   406,
     407,   408,   409,   410,   411,   412,   413,   414,   415,     0,
       0,     0,     0,   416,   417,     0,     0,     0,   501,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   420,     0,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,     0,     0,     0,     0,   416,   417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   420,
       0,   421,   422,   423,   424,   425,   426,   427,   428,   429,
     430,   404,   405,   406,   407,   408,   409,   410,   411,   412,
     413,  -690,  -690,     0,     0,     0,     0,   416,   417,   404,
     405,   406,   407,   408,   409,   410,     0,   412,   413,     0,
     419,     0,     0,     0,     0,   416,   417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,     0,     0,     0,     0,     0,     0,     0,
       0,   421,   422,   423,   424,   425,   426,   427,   428,   429,
     430,  -690,  -690,  -690,  -690,   408,   409,     0,     0,  -690,
    -690,   404,   405,   406,   407,   408,   409,   416,   417,   412,
     413,     0,     0,     0,     0,     0,     0,   416,   417,     0,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430
};

static const yytype_int16 yycheck[] =
{
       2,    89,    87,    88,    27,    10,    22,    14,     9,    10,
      15,    28,   597,   271,    15,   481,   284,   402,   378,    21,
       7,    28,    82,   299,     2,   433,     4,   303,   639,   118,
     443,   444,   508,   490,   431,    69,     4,   221,   435,   547,
     306,   438,    66,   313,   310,    16,    17,     5,     6,    20,
     458,  1010,    54,    55,   594,    13,    52,   331,    56,   814,
      16,    17,   459,   539,    20,   820,   764,   475,    25,  1035,
      29,    58,    74,    75,    25,    75,   484,   474,   504,   476,
     373,   318,   508,    54,    55,    15,   670,   671,   485,   105,
     792,   693,   144,   815,    26,   697,  1045,   814,    56,   295,
     966,   394,   704,   820,   612,   332,   717,    51,   335,   111,
     337,     0,   339,    51,   341,    92,   117,    25,    60,    61,
      62,    63,    16,    17,    82,    75,    20,    90,   525,    52,
      92,     7,   217,   370,   138,    57,    16,    17,    14,    69,
      20,   549,    25,   228,   121,   402,   115,   121,    25,   118,
     119,    25,    28,   550,   113,    57,   318,    55,   102,   121,
      54,    28,    74,    75,   102,   142,    90,   460,    26,   142,
      25,    92,   122,  1132,    25,  1141,   145,   146,    92,   148,
     142,  1047,    58,   839,   147,   142,   443,   444,    34,   845,
      92,   142,   121,   144,   594,   139,   119,   597,    79,   121,
     121,  1150,    92,   399,   289,    51,   138,   121,   370,   121,
     122,   213,   214,   213,   214,    20,   756,   222,   223,   121,
      25,   222,   223,   147,     2,   942,     4,     5,     6,   769,
     315,     9,    10,   509,   142,    13,   587,    15,    16,    17,
     516,    90,    20,   144,   966,   126,     2,   964,     4,     5,
       6,     7,   528,   313,   241,   513,   105,    13,    90,   142,
     736,   144,   594,   297,   214,   142,   850,   269,   142,   271,
     103,   305,   306,   275,    52,   215,   310,   547,    56,    90,
     138,   221,   283,   284,   558,   125,   144,   142,    66,  1044,
      55,   142,   800,    21,    22,   128,    52,   121,   147,   295,
      56,   213,   214,    90,    82,   581,   582,   692,   279,   735,
     736,    92,    92,   121,   551,   147,   294,  1015,   258,   275,
      55,   251,   907,   279,   302,  1047,    82,  1044,  1045,   556,
    1032,    20,  1049,  1035,   142,   611,   147,    90,    90,   117,
     121,   119,   612,    25,   349,   350,   351,   352,   349,   350,
     351,   352,   105,   354,   355,   313,   756,    90,    51,    18,
     147,   901,    55,   119,   287,   241,    25,   297,   457,   769,
     348,   911,   295,   397,   376,   353,   378,   105,   402,    93,
     348,   275,   324,   111,   112,   279,    90,   140,   681,   551,
      90,   144,   792,    90,   147,   147,  1151,   805,    92,   279,
    1117,   129,   400,   399,  1006,   142,   318,   804,   348,   806,
     142,   877,   126,   689,   147,   841,   776,    92,   580,   443,
     444,    57,   362,   836,   756,   138,    92,   121,   101,   842,
     843,   433,   210,  1150,  1151,   597,    90,   769,   600,  1141,
     380,   792,   400,   147,   222,   223,   121,   147,   142,    60,
     147,   105,    63,   145,   210,   121,   458,   725,   370,    92,
     461,    18,    92,    20,   466,    92,   121,   142,  1185,   745,
     141,   729,    62,   475,    64,    65,   399,   433,   139,   502,
     746,    55,   484,   101,    92,   545,   140,   547,   121,   765,
      26,   121,  1032,   147,   121,    61,   107,   275,    64,    65,
     793,   279,   458,   397,   521,   283,   284,   907,   402,   287,
     512,   513,   142,   121,   521,   142,   294,   295,  1129,   475,
     522,   142,   666,  1125,   302,   669,   116,   117,   484,    92,
     800,   287,  1008,   504,   101,   313,   720,   622,   294,   295,
     480,   481,    57,   687,   522,   802,   302,   549,   471,  1006,
     116,   117,   612,   531,    90,   848,   479,   313,   121,   435,
     121,   974,   438,   923,   924,   536,   489,   835,    27,   105,
     348,   349,   350,   351,   352,   353,   354,   355,    92,   836,
     121,   511,  1008,   459,    92,   842,   843,   545,   142,   547,
     142,   531,   348,   549,  1134,   373,    51,   353,   538,   493,
     476,   894,   138,   605,   140,  1145,    51,   121,   144,   485,
     886,   147,   142,   121,    37,    38,   394,   373,   641,   397,
     142,   399,   400,    92,   402,   893,   902,    51,   584,   887,
     792,   142,  1032,   115,   557,  1035,   118,   119,   394,   551,
     101,   657,   121,   399,   400,   521,   142,   603,    92,   525,
     666,    99,   121,   669,   612,   433,   115,    15,   660,   118,
     119,    58,    59,    57,   666,   443,   444,   669,   670,   671,
     131,   132,   133,    13,   550,   680,    63,   121,  1029,   680,
     458,  1032,   460,   461,  1035,   687,  1037,   146,   121,   148,
     692,   693,   985,   471,   121,   697,   880,   475,   715,    58,
      59,   479,   704,   690,   460,    16,   484,   435,   715,   714,
      51,   489,   746,   714,  1122,   471,    16,   974,    17,    18,
      90,   115,    15,   479,   118,   119,  1123,   729,    69,   123,
     387,   459,   389,   489,   115,   105,   145,   118,   119,   679,
     800,  1141,  1102,   145,   522,   907,   139,   909,   476,    40,
      41,   913,  1103,   531,   148,   919,   920,   485,   846,   142,
     101,   102,   103,   142,   735,   142,   522,   545,    15,   547,
     140,   549,   121,   122,   776,   531,    15,   147,   802,   557,
     720,    40,    41,    42,    43,    44,   142,   128,    44,   545,
    1141,   547,  1143,   121,  1145,   141,  1147,   525,   692,    26,
      15,   557,    16,   805,   115,   989,   584,   118,   119,    90,
     141,   995,   836,   837,   690,    16,    18,    90,   842,   843,
     141,   141,   550,   801,   105,   603,    40,    41,    15,    90,
    1181,   142,   105,   139,   612,   146,   139,   148,   778,   715,
     141,   139,   800,    61,   105,   142,    64,    65,   850,   805,
      44,  1013,  1014,    57,   142,   269,   612,   271,   860,   140,
     865,   863,    16,    90,   865,  1133,   147,   140,   142,     2,
     841,     4,   142,  1035,   147,  1037,     9,    10,   105,   140,
     142,    44,    15,    16,    17,   887,   147,    20,    57,    55,
      51,   305,  1076,  1077,    62,    51,    64,    65,   116,   117,
     142,   115,   680,   681,   118,   119,   142,    90,   142,    16,
      15,   138,   852,   140,   115,    93,    14,   118,   119,    52,
     147,   923,   924,    15,    15,   681,   849,   139,  1090,   657,
     806,  1093,   146,    66,   148,   145,   714,   877,   666,   862,
     880,   669,    63,    64,    65,   146,   142,   148,   116,   117,
     974,   874,   875,   142,    26,    15,    90,   685,   587,   687,
     883,   115,   591,   921,   118,   119,   115,   142,   926,   118,
     119,   105,   895,    51,  1136,    63,    64,    65,   142,  1141,
     142,  1143,  1067,   141,   117,  1147,   119,   142,   141,    55,
     792,   142,   146,  1177,   148,   116,   117,  1002,  1003,   148,
     142,  1002,  1003,   142,  1006,   142,   140,    15,   115,  1193,
    1194,   118,   119,   147,   139,   793,    15,    90,    90,  1181,
      15,    15,   800,   801,   802,   139,   142,   805,   116,   117,
     988,   126,   105,   105,    55,   139,   126,   793,    90,   146,
     142,   148,    15,    55,   800,   801,   142,    15,   142,   989,
      63,    64,    65,   105,   141,   995,   142,   142,   836,   837,
    1083,   984,   142,   142,   842,   843,   138,   140,   140,   992,
     848,   849,   144,   142,   147,   147,  1078,   210,   806,   142,
     142,  1004,   115,    15,   862,   118,   119,   865,   140,   222,
     223,   144,   848,   849,   144,   147,   874,   875,   512,   513,
    1102,   142,   142,   116,   117,   883,   862,    15,    15,   142,
      40,   522,    41,   146,    13,   148,   894,   895,   874,   875,
    1122,     6,  1124,  1125,   781,   782,   783,   883,   785,    90,
     787,  1125,   792,  1132,  1049,   847,  1076,  1077,   894,   895,
     963,   815,   275,   921,   105,  1124,   279,  1070,   926,  1072,
     283,   284,  1075,    51,   287,    53,    54,    55,    56,  1161,
    1162,   294,   295,   792,   253,   921,  1122,     7,   587,   302,
     926,    69,  1029,  1032,    16,    17,    -1,    -1,    20,   140,
      -1,    26,    -1,    -1,  1189,  1190,   147,    -1,  1189,  1190,
    1192,    26,    -1,    90,    -1,   609,   974,  1199,  1200,    -1,
    1161,  1162,    -1,    -1,  1127,  1128,   984,   985,   105,    -1,
     988,    -1,    54,    55,   992,   348,   349,   350,   351,   352,
     353,   354,   355,    -1,  1002,  1003,  1004,  1029,   984,   985,
    1032,  1192,   988,  1035,    -1,  1037,   992,  1177,  1199,  1200,
     373,    -1,    -1,   140,   142,    90,   660,  1123,  1004,    91,
     147,    -1,    -1,  1193,  1194,    90,   928,   929,    -1,    -1,
     105,   394,    -1,    -1,   397,  1188,   399,    -1,    -1,   402,
     105,    -1,    -1,    26,  1197,  1198,    -1,    -1,    -1,     2,
      -1,     4,     5,     6,    -1,    -1,    -1,    -1,    37,    38,
      13,    -1,  1070,   138,  1072,   140,    -1,  1075,    90,   144,
     433,  1103,   147,   138,    -1,   140,   720,    -1,    -1,   144,
     443,   444,   147,   105,  1070,   729,  1072,    90,    51,  1075,
      53,    54,    55,    56,    -1,   458,    -1,   460,   461,    52,
      -1,    -1,   105,    56,    -1,    -1,    69,    90,   471,  1141,
      -1,  1143,   475,  1145,  1122,  1147,   479,    -1,   140,  1127,
    1128,   484,   105,    -1,    -1,   147,   489,    -1,    -1,    82,
    1032,    -1,  1019,  1020,  1021,  1022,   115,   140,    90,   118,
     119,  1127,  1128,    -1,   147,    -1,    -1,    -1,    -1,  1181,
      -1,    -1,    -1,   105,    -1,   138,    -1,   140,    -1,   522,
      90,   144,    -1,    -1,   147,  1123,   119,   146,   531,   148,
    1029,    -1,    -1,  1032,    -1,   105,  1035,   624,  1037,   142,
    1188,  1189,  1190,    63,    64,    65,   549,    -1,   140,  1197,
    1198,    -1,   639,    -1,   557,   147,    -1,    -1,    -1,    34,
      35,    36,  1188,   275,  1106,  1107,  1108,   279,  1110,  1111,
     140,  1197,  1198,    -1,    -1,   115,    51,   147,   118,   119,
      55,   584,    -1,    58,    59,    -1,    -1,    -1,    63,    -1,
      34,    35,    36,    -1,    -1,    -1,   116,   117,    -1,    -1,
     603,    -1,    -1,   887,  1103,    -1,   146,    51,   148,    -1,
    1137,    55,    -1,    -1,    58,    59,    91,   210,    -1,    63,
      -1,    -1,    97,    98,    -1,   100,  1168,  1169,  1170,  1171,
     717,   106,    -1,    -1,     2,   624,     4,     5,     6,    63,
      64,    65,  1141,    -1,  1143,    13,  1145,    91,  1147,  1191,
     639,    -1,   127,    97,    98,    -1,   100,    -1,   102,   134,
      -1,    -1,   106,    -1,    -1,    -1,   141,    51,    -1,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,   680,   681,    -1,
      -1,    -1,  1181,   127,    52,    69,    -1,    -1,    56,    -1,
     134,    -1,   116,   117,   287,    -1,    63,    64,    65,    -1,
      -1,   294,   295,    63,    64,    65,   443,   444,    -1,   302,
      94,   714,    -1,    -1,    82,    -1,   100,   101,   102,   103,
     313,   433,    63,    64,    65,    -1,    -1,   814,   717,    -1,
      -1,    -1,    -1,   820,    51,    -1,    53,    54,    55,    56,
     477,   478,    -1,    -1,   128,    -1,   458,   131,    -1,   116,
     117,   119,    69,    -1,    -1,   348,   116,   117,   142,    -1,
     353,    -1,   115,   475,    -1,   118,   119,    -1,    85,    -1,
      -1,    -1,   484,    -1,    -1,   116,   117,    94,    -1,    -1,
     373,    -1,    -1,   100,   101,   102,   103,    -1,    -1,   526,
     793,    -1,   504,   146,    -1,   148,   508,    -1,   801,   802,
      -1,   394,   805,    -1,    -1,    -1,   399,   400,    -1,   402,
      -1,   128,    -1,    -1,   131,    -1,    -1,    -1,    51,    -1,
      53,    54,    55,    56,   536,   814,    -1,   539,    -1,    -1,
      -1,   820,    -1,   836,   837,    -1,    69,   549,    -1,   842,
     843,    -1,   210,    -1,    -1,   848,   849,    -1,    -1,    -1,
     443,   444,    -1,    -1,    -1,   942,    -1,    -1,    -1,   862,
      -1,    94,   865,    -1,    -1,    -1,    -1,   460,   101,   102,
     103,   874,   875,    -1,    -1,    -1,   963,   964,   471,    -1,
     883,    -1,    -1,    34,    35,    36,   479,    -1,    -1,    -1,
      -1,   894,   895,    -1,    -1,   128,   489,    -1,    -1,    -1,
      51,    -1,    -1,    -1,    55,    -1,    -1,    58,    59,    -1,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    51,   287,
      53,    54,    55,    56,    -1,    -1,   294,   295,    -1,   522,
      -1,    -1,    -1,    -1,   302,    -1,    69,    -1,   531,    -1,
      91,    -1,    -1,    -1,    -1,   313,    97,    98,    -1,   100,
      -1,    -1,   545,   942,   547,   106,    -1,  1044,  1045,    -1,
      -1,    94,  1049,    -1,   557,    -1,     2,   100,     4,     5,
       6,   974,    -1,    -1,   963,   964,   127,    13,    -1,    -1,
     348,   984,   985,   134,    -1,   353,    34,    35,    36,   992,
      51,    -1,    53,    54,    55,    56,    -1,    -1,    -1,  1002,
    1003,  1004,    -1,    51,    -1,   373,    -1,    55,    69,    -1,
      58,    59,    -1,    -1,    -1,    63,    52,    -1,    -1,   612,
      56,    -1,    -1,   735,   736,    -1,   394,    -1,    -1,    -1,
    1117,   399,   400,    94,   402,    -1,    -1,    -1,    -1,   100,
      -1,    -1,  1129,    91,    -1,    -1,    82,    -1,    -1,    97,
      98,    -1,    -1,    -1,    -1,  1044,  1045,    -1,   106,    -1,
    1049,    -1,    -1,  1150,  1151,    -1,    -1,  1070,    -1,  1072,
      -1,    -1,  1075,    -1,    -1,   443,   444,    -1,    -1,   127,
      -1,    -1,    -1,   119,    -1,    -1,   134,    -1,   681,    -1,
      -1,    -1,   460,   805,    -1,    -1,    -1,    -1,  1185,   836,
     837,    -1,    -1,   471,    -1,   842,   843,    88,    89,    -1,
      -1,   479,    51,    -1,    53,    54,    55,    56,    -1,  1122,
     101,   489,    -1,    -1,  1127,  1128,    88,    89,  1117,   841,
      69,   868,   869,    -1,   871,   872,    -1,    -1,    -1,   101,
    1129,    51,    -1,    53,    54,    55,    56,   128,   129,   130,
     131,   132,   133,    -1,   522,    94,    -1,    -1,    -1,    69,
      -1,  1150,  1151,   531,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    83,   210,    -1,    -1,   545,    -1,   547,
      -1,    -1,    -1,    -1,    94,  1188,  1189,  1190,    -1,   557,
     100,   101,   102,   103,  1197,  1198,  1185,    88,    89,   101,
     793,    51,    -1,    53,    54,    55,    56,   800,   801,   802,
     101,   121,    -1,    -1,    -1,    -1,    -1,    -1,   128,    69,
      -1,   131,    -1,    -1,    -1,    -1,    -1,   129,   130,   131,
     132,   133,    -1,    -1,   144,    -1,    -1,   974,   129,   130,
     131,   132,   133,   836,   612,    -1,    -1,    -1,    -1,   842,
     843,   287,    -1,    -1,    -1,   848,   849,    -1,   294,   295,
     997,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,   862,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   313,    -1,    -1,
      -1,   874,   875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     883,    -1,    -1,    -1,    -1,    -1,  1008,    -1,    -1,    -1,
      -1,   894,   895,    51,    -1,    53,    54,    55,    56,    -1,
      -1,    -1,   348,   681,    -1,    -1,    -1,   353,    -1,    -1,
      -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,   921,    -1,
      -1,    -1,    -1,   926,     2,    83,     4,   373,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    13,    94,    -1,    -1,    -1,
      -1,    -1,   100,   101,   102,   103,    -1,    -1,   394,    -1,
      -1,    -1,    -1,   399,   400,    -1,   402,    -1,    -1,    51,
      -1,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
     128,   974,    -1,   131,    52,    -1,    -1,    69,    -1,    -1,
      -1,   984,   985,    -1,    -1,   988,   144,    -1,    -1,   992,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   443,   444,    -1,
    1122,  1004,    94,    -1,    -1,    -1,    -1,    -1,   100,   101,
     102,   103,    -1,    -1,   460,   793,    -1,    -1,    -1,    -1,
      -1,    -1,   800,   801,   802,   471,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   479,    -1,    -1,   128,    -1,    -1,   131,
      -1,   119,    -1,   489,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   144,    -1,    -1,    -1,    -1,    51,   836,    53,
      54,    55,    56,    -1,   842,   843,    -1,  1070,    -1,  1072,
     848,   849,  1075,    -1,    -1,    69,   522,    51,    -1,    53,
      54,    55,    56,    -1,   862,   531,    -1,    -1,    -1,    -1,
      -1,    85,    -1,    -1,    -1,    69,   874,   875,    -1,   545,
      94,   547,    -1,    -1,    -1,   883,   100,   101,   102,   103,
      -1,   557,    -1,    -1,    -1,    -1,   894,   895,    -1,    -1,
      94,    -1,    -1,    -1,  1127,  1128,   100,   101,   102,   103,
      -1,    -1,   210,    -1,   128,    -1,    -1,   131,    -1,    -1,
       2,    -1,     4,   921,    -1,    -1,    -1,    -1,   926,    -1,
      -1,    -1,    -1,    -1,   128,    -1,    -1,   131,    -1,    -1,
      -1,     0,    -1,    -1,    -1,    -1,   612,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,  1188,    25,    26,    27,    -1,
      52,    -1,    -1,    -1,  1197,  1198,   974,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,   984,   985,    -1,   287,
     988,    -1,    -1,    -1,   992,    -1,   294,   295,    -1,    -1,
      -1,    -1,    -1,    -1,   302,    -1,  1004,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    51,   681,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    69,    -1,    -1,    -1,    -1,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    85,    -1,
     348,    -1,    -1,    -1,    -1,   353,   115,    94,    -1,   118,
     119,    -1,    -1,   100,   101,   102,   103,    -1,    -1,    -1,
      -1,    -1,  1070,    -1,  1072,   373,    -1,  1075,    -1,   138,
     139,    -1,    -1,    -1,    -1,   144,   145,   146,   147,   148,
      -1,   128,    -1,    -1,   131,    -1,   394,    -1,    -1,    -1,
      -1,   399,    -1,    -1,   402,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   793,   210,  1127,
    1128,    -1,    -1,    -1,   800,   801,   802,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   443,   444,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   460,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     836,    -1,    -1,   471,    -1,    -1,   842,   843,    -1,    -1,
      -1,   479,   848,   849,    -1,    -1,    -1,    -1,    -1,    -1,
    1188,   489,    -1,    25,    -1,    -1,   862,    -1,    -1,  1197,
    1198,    -1,    -1,    -1,    -1,   287,    -1,    -1,   874,   875,
      -1,    -1,   294,   295,    -1,    -1,    -1,   883,    -1,    -1,
     302,    -1,    -1,    -1,   522,    -1,    -1,    -1,   894,   895,
      -1,    -1,    -1,   531,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,   921,    88,    89,    -1,   557,
     926,    93,    -1,    -1,    -1,    -1,   348,    -1,    -1,   101,
      -1,   353,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   373,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,   974,    -1,
      -1,    -1,   394,    -1,    -1,    -1,    -1,   399,   984,   985,
     402,    -1,   988,    -1,    -1,    -1,   992,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,  1004,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   443,   444,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,   460,    88,
      89,    -1,    -1,   681,    -1,    -1,    -1,    -1,    -1,   471,
      -1,    -1,   101,    -1,    -1,    -1,    -1,   479,    -1,    -1,
      -1,    -1,    -1,    -1,  1070,    -1,  1072,   489,    -1,  1075,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    51,    52,    -1,    -1,    55,    -1,
     522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   531,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,  1127,  1128,    80,    81,    -1,    -1,    -1,    -1,    86,
      87,    88,    89,    -1,    -1,   557,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   793,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   801,   802,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      -1,    -1,  1188,    -1,    -1,    -1,   143,   144,    -1,    -1,
      -1,  1197,  1198,    -1,    -1,    -1,    -1,    -1,   836,    -1,
      -1,    -1,    -1,    -1,   842,   843,    -1,    -1,    -1,    -1,
     848,   849,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   874,   875,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   883,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   894,   895,    -1,   681,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    16,    17,
      -1,    -1,    20,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    47,
      48,    49,    50,   101,    -1,    -1,    54,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    66,    67,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   974,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   984,   985,    -1,    -1,
     988,    -1,    -1,    -1,   992,    -1,    -1,    -1,    -1,    -1,
      -1,    44,    -1,    -1,    -1,    -1,  1004,    -1,    -1,    -1,
      -1,   793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   801,
     802,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   836,    -1,    -1,    -1,   101,    -1,
     842,   843,    -1,    -1,    -1,    -1,   848,   849,    -1,    -1,
      -1,    -1,  1070,    -1,  1072,    -1,    -1,  1075,    -1,   122,
     862,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   874,   875,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   894,   895,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1127,
    1128,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   261,   262,   263,   264,    -1,    51,    52,
      -1,    -1,    55,    -1,    -1,    -1,    -1,   275,    -1,    -1,
      -1,   279,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
    1188,    -1,   974,    86,    87,    88,    89,    -1,    -1,  1197,
    1198,    -1,   984,   985,    -1,    -1,    -1,   100,   101,   102,
     992,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1004,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   385,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1070,   397,
    1072,    -1,    -1,  1075,   402,    -1,   404,   405,   406,   407,
     408,   409,   410,   411,   412,   413,   414,   415,   416,   417,
      -1,   419,   420,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,    -1,    -1,   433,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   443,   444,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1127,  1128,    -1,    -1,    -1,
     458,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   473,    -1,   475,    -1,   477,
     478,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   493,    -1,    -1,   496,   497,
      -1,    -1,    -1,   501,    -1,    -1,   504,    -1,   506,    -1,
     508,   509,    -1,    -1,    -1,    -1,  1188,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1197,  1198,    -1,   526,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   536,    -1,
      -1,   539,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   549,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   566,   567,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   584,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   603,    -1,    -1,   606,    -1,
      -1,     0,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   692,    84,    85,   695,   696,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,   115,    -1,   726,   118,
     119,    -1,    44,    -1,    -1,    -1,    -1,   735,   736,    -1,
      -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,   148,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   797,
      -1,    -1,    -1,    -1,   802,   803,    -1,   805,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   836,   837,
      -1,    -1,    -1,   841,   842,   843,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     868,   869,    -1,   871,   872,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   882,    -1,    -1,    -1,   886,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   903,   904,    -1,     0,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   916,   917,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    25,   932,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,   967,
     968,    -1,    -1,    -1,    -1,    -1,   974,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,   997,
      -1,    93,    -1,    -1,    -1,    -1,    -1,    99,    -1,   101,
    1008,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,   139,   140,   141,
     142,    -1,    -1,   145,   146,   147,   148,     0,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    -1,
      -1,    -1,    25,    26,    27,    28,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1122,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,    -1,
      93,    -1,    -1,    -1,    -1,    -1,    99,    -1,   101,    -1,
      -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,     0,    -1,   138,   139,   140,   141,   142,
      -1,   144,   145,   146,   147,   148,    13,    14,    15,    16,
      17,    18,    -1,    20,    -1,    -1,    -1,    -1,    25,    -1,
      27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    90,    -1,    92,    93,    -1,    -1,    -1,
      -1,    -1,    99,    -1,   101,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,
      -1,   118,   119,    -1,   121,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,     0,
      -1,    -1,   139,   140,   141,   142,    -1,    -1,   145,   146,
     147,   148,    13,    14,    15,    16,    17,    18,    -1,    20,
      -1,    -1,    -1,    -1,    25,    26,    27,    28,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,
      -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    99,    -1,
     101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,
      -1,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,     0,    -1,   138,   139,   140,
     141,   142,    -1,   144,   145,   146,   147,   148,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,
      25,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    90,    -1,    -1,    93,    -1,
      -1,    -1,    -1,    -1,    99,    -1,   101,    -1,    -1,    -1,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,   118,   119,    -1,    -1,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,     0,    -1,    -1,   139,   140,   141,   142,    -1,   144,
     145,   146,   147,   148,    13,    14,    15,    -1,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    25,    26,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    90,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,
     119,    -1,   121,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,     0,    -1,   138,
     139,   140,    -1,   142,    -1,    -1,   145,   146,   147,   148,
      13,    14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,
      -1,    -1,    25,    26,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,    92,
      93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,   122,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,     0,    -1,   138,   139,   140,    -1,   142,
      -1,    -1,   145,   146,   147,   148,    13,    14,    15,    -1,
      17,    18,    -1,    20,    -1,    -1,    -1,    -1,    25,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    90,    -1,    92,    93,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,
      -1,   118,   119,    -1,   121,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,     0,
      -1,    -1,   139,   140,    -1,   142,    -1,    -1,   145,   146,
     147,   148,    13,    14,    15,    -1,    17,    18,    -1,    20,
      -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,
      -1,    92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,
     121,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,   139,   140,
      -1,   142,    -1,    -1,   145,   146,   147,   148,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    -1,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   146,     1,   148,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    15,    -1,
      17,    18,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,   115,    -1,
      -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,
       1,   148,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    15,    -1,    -1,    18,    19,    20,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   146,     1,   148,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    -1,    -1,    18,    19,    -1,    21,    22,    23,    24,
      25,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
     115,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
     135,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   146,     1,   148,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    -1,    -1,    18,
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
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,   115,    -1,    -1,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,   139,
      -1,    -1,    -1,    -1,    -1,    -1,   146,     1,   148,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      14,    15,    -1,    -1,    -1,    19,    -1,    21,    22,    23,
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
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
     115,    -1,    -1,   118,   119,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,   134,
     135,   136,    -1,    19,    -1,    21,    22,    23,    24,    -1,
     145,   146,    -1,   148,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,   115,
      -1,    -1,   118,   119,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,   134,   135,
     136,    -1,    19,    -1,    21,    22,    23,    24,    -1,   145,
     146,    -1,   148,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,   115,    -1,
      -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,
      -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,    -1,   146,
       1,   148,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    15,    -1,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,   146,    -1,   148,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,   146,    -1,   148,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,   134,   135,   136,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,   146,    -1,   148,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,   115,    -1,    -1,   118,   119,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
     134,   135,   136,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,   146,    -1,   148,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
     115,    -1,    -1,   118,   119,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,   134,
     135,   136,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,   146,    -1,   148,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,   115,
      -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   148,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,   143,   144,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,   102,   103,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,
     138,    -1,    -1,    -1,    -1,    -1,   144,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,   102,   103,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,
     136,    19,   138,    21,    22,    23,    24,    -1,   144,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    92,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,   102,   103,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,   121,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,   134,   135,   136,    -1,
      19,    -1,    21,    22,    23,    24,   144,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    92,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,   102,   103,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,   121,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,   134,   135,   136,    -1,    19,
      -1,    21,    22,    23,    24,   144,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,   102,   103,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,   144,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
     102,   103,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   144,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    69,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,   102,    -1,    -1,    -1,    -1,   107,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,   143,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,    -1,    -1,    -1,    -1,   107,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    -1,    -1,
      -1,    -1,    -1,    -1,   143,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    -1,    -1,    55,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,   102,    -1,    -1,    -1,   106,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    -1,
      -1,    -1,    -1,    -1,    -1,   143,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,    15,    16,
      17,    18,    19,    20,    21,    22,    23,    24,    25,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    -1,    -1,    55,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,
      87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      -1,    -1,    -1,    -1,    -1,    -1,   143,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,    21,
      22,    23,    24,    -1,    -1,    -1,   142,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   134,    21,    22,    23,    24,    -1,    -1,    -1,
     142,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,   134,   135,   136,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
     102,   103,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,   102,   103,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     134,   135,   136,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,   102,   103,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,
     136,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,   102,   103,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,   102,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,
     102,   103,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,   102,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     134,   135,   136,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,    -1,    -1,   102,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,
     136,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     134,   135,   136,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,
     136,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     134,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,   134,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   134,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,   134,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     134,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
      -1,    51,    52,    -1,   104,    55,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,   134,    -1,    86,    87,    88,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    51,    52,    -1,
      -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
      -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    51,
      52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    -1,    -1,    80,    81,
      -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    51,    52,    -1,    -1,    55,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    51,    52,    -1,
      -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
      -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    51,
      52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    -1,    -1,    80,    81,
      -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    51,    52,    -1,    -1,    55,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,    -1,
      80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    51,    52,    -1,
      -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
      -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    51,
      52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    -1,    -1,    80,    81,
      -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,
     102,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   101,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,   143,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   142,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    93,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    72,
      73,    74,    75,    76,    77,    78,    -1,    80,    81,    -1,
     101,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    72,    73,    74,    75,    76,    77,    88,    89,    80,
      81,    -1,    -1,    -1,    -1,    -1,    -1,    88,    89,    -1,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133
};

/* YYSTOS[STATE-NUM] -- The symbol kind of the accessing symbol of
   state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   150,   151,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    19,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      58,    59,    60,    63,    66,    67,    69,    70,    71,    84,
      85,    91,    94,    95,    97,    98,   100,   104,   106,   107,
     108,   110,   111,   112,   114,   134,   135,   136,   152,   153,
     154,   159,   161,   163,   166,   167,   170,   171,   174,   175,
     177,   178,   179,   181,   182,   191,   205,   222,   243,   244,
     275,   276,   277,   281,   282,   283,   289,   290,   291,   293,
     294,   295,   296,   297,   298,   334,   347,   154,    21,    22,
      30,    31,    32,    39,    51,    55,    69,    88,    91,    94,
     134,   166,   167,   183,   184,   205,   222,   295,   298,   334,
     184,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    55,    70,    71,    72,    73,    74,    75,
      76,    77,    80,    81,    86,    87,    88,    89,   100,   101,
     102,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   135,   136,   143,   144,   185,   189,   190,   297,   329,
     206,    91,   163,   166,   167,   169,   182,   222,   295,   296,
     298,   169,   212,   214,    69,    91,   175,   182,   222,   227,
     295,   298,    33,    34,    35,    36,    48,    49,    50,    51,
      55,   106,   185,   186,   187,   291,   115,   118,   119,   146,
     148,   169,   285,   286,   287,   340,   344,   345,   346,    51,
      69,   100,   102,   103,   135,   174,   191,   197,   200,   203,
     277,   332,   333,   197,   197,   144,   194,   195,   198,   199,
     347,   194,   198,   144,   341,   186,   155,   138,   191,   222,
     191,   191,   191,    55,     1,    94,   157,   158,   159,   176,
     177,   347,   207,   209,   192,   203,   332,   347,   191,   331,
     332,   347,    91,   142,   181,   222,   295,   298,   210,    53,
      54,    56,    63,    69,   107,   185,   292,    63,    64,    65,
     116,   117,   278,   279,    61,   278,    62,   278,    63,   278,
      63,   278,    58,    59,   170,   191,   191,   340,   346,    40,
      41,    42,    43,    44,    37,    38,    51,    53,    54,    55,
      56,    69,    83,    94,   100,   101,   102,   103,   128,   131,
     144,   301,   302,   303,   304,   305,   308,   309,   310,   311,
     313,   314,   315,   316,   318,   319,   320,   323,   324,   325,
     326,   327,   347,   301,   303,    28,   241,   121,   142,    94,
     100,   178,   121,    25,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    88,    89,    93,   101,
     122,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    90,   105,   140,   147,   338,    90,   338,   339,    26,
     138,   245,   277,    92,    92,   194,   198,   245,   163,    51,
      55,   183,    58,    59,   302,   125,   299,    90,   140,   338,
     221,   330,    90,   147,   337,   156,   157,    55,   301,   301,
      16,   223,   344,   121,    90,   140,   338,    92,    92,   223,
     169,   169,    55,    90,   140,   338,    25,   107,   142,   288,
     340,   115,   287,    20,   248,   344,    57,    57,   191,   191,
     191,    93,   142,   201,   202,   347,    57,   201,   202,    85,
     196,   197,   203,   332,   347,   197,   163,   340,   342,   163,
     345,   160,   138,   157,    90,   338,    92,   159,   176,   145,
     340,   346,   342,   159,   342,   141,   202,   343,   346,   202,
     343,   139,   343,    55,   178,   179,   180,   142,    90,   140,
     338,   144,   239,   313,   318,    63,   278,   280,   284,   285,
      63,   279,    61,    62,    63,    63,   101,   101,   154,   169,
     169,   169,   169,   159,   163,   163,    57,   121,    57,   344,
     317,    85,   313,   318,   121,   156,   191,   142,   328,   347,
      51,   142,   328,   344,   142,   312,   191,   142,   312,    51,
     142,   312,    51,   121,   156,   242,   100,   170,   191,   203,
     204,   176,   142,   181,   142,   161,   162,   170,   182,   191,
     193,   204,   222,   298,   165,   191,   191,   191,   191,   191,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   164,
     191,   191,   191,   191,   191,   191,   191,   191,   191,   191,
     191,   191,    51,    52,    55,   189,   194,   335,   336,   196,
     203,    51,    52,    55,   189,   194,   335,    51,    55,   335,
     247,   246,   162,   191,   193,   162,   193,    99,   172,   219,
     300,   218,    51,    55,   183,   335,   196,   335,   156,   163,
     168,    15,    13,   271,   347,   121,   121,   157,    16,    51,
      55,   196,    51,    55,   157,    27,   224,   344,   224,    51,
      55,   196,    51,    55,   216,   188,   157,    25,   248,   191,
     203,    15,   191,   191,   191,   341,   100,   191,   200,   332,
     191,   333,   342,   145,   340,   202,   202,   342,   145,   186,
     152,   139,   193,   342,   159,   208,   332,   178,   180,    51,
      55,   196,    51,    55,   313,   211,   142,    63,   157,   285,
     191,   191,    51,    69,   100,   228,   318,   342,   342,   142,
     174,   191,    15,    51,    69,   305,   310,   327,    85,   311,
     316,   323,   325,   318,   320,   325,    51,   318,   174,   191,
      15,    79,   126,   233,   234,   347,   191,   202,   342,   180,
     142,    44,   121,    44,    90,   140,   338,    34,    35,    36,
      51,    55,    63,    91,    97,    98,   100,   102,   127,   254,
     255,   257,   258,   259,   260,   263,   264,   265,   267,   268,
     269,   270,   290,   294,   254,   341,    92,    92,   194,   198,
     141,   202,    92,    92,   195,   198,   195,   198,   233,   233,
     173,   344,   169,   156,   141,    15,   342,   185,   191,   204,
     272,   347,    18,   226,   347,    17,   225,   226,    92,    92,
     141,    92,    92,   226,   213,   215,   141,   169,   186,   139,
     254,    15,   202,   223,   191,   201,    85,   332,   139,   342,
     343,   141,   236,   341,    29,   113,   240,   139,   142,   315,
     342,   142,    85,    44,    44,   328,   344,   142,   312,   142,
     312,   142,   312,   142,   312,   312,    44,    44,   230,   232,
     235,   304,   306,   307,   310,   318,   319,   321,   322,   325,
     327,   156,   100,   191,   180,   159,   191,    51,    55,   196,
      51,    55,    57,    55,    51,   141,   257,   261,   262,   263,
      51,   139,   266,   267,   269,    51,    34,    51,    51,   257,
     263,   142,    93,   126,   142,    90,   142,    57,   123,   162,
     193,   170,   193,   172,    92,   162,   193,   162,   193,   172,
     245,   241,   156,   157,   233,   220,   344,    15,    93,   273,
     347,   157,    14,   274,   347,   169,    15,    92,    15,   157,
     157,   224,    40,    41,   223,   191,   157,   342,   202,   145,
     146,   156,   157,   229,   142,   100,   342,   191,   191,   318,
     325,   318,   318,   191,   191,   236,   236,    91,   222,   142,
     328,   328,   142,   231,   222,   142,   231,   142,   231,    15,
     191,   141,   257,   141,   142,   142,   139,   142,   142,   142,
      51,   259,   256,   257,    55,   268,   269,   191,   191,   162,
     193,    15,   139,   157,   156,    91,   182,   222,   295,   298,
     223,   157,   223,    15,    15,   217,   169,   169,   157,   226,
     248,   249,    51,   237,   238,   314,    15,   139,   318,   318,
     142,   315,   312,   142,   312,   312,   312,   126,   126,    55,
      90,   306,   310,   142,   230,   231,   322,   325,   318,   321,
     325,   318,   257,   263,   262,   269,   256,   142,   139,    15,
      55,    90,   140,   338,   157,   157,   157,   223,   223,    25,
     226,   250,   142,   341,   142,   318,   142,   318,    51,    55,
     328,   142,   231,   142,   231,   142,   231,   142,   231,   231,
     142,   142,   257,    51,    55,   196,    51,    55,   271,   225,
      15,   157,   157,   254,    15,   238,   318,   312,   318,   325,
     318,   318,   262,   263,   141,   250,   250,   251,   252,   253,
     231,   142,   231,   231,   231,   142,    15,    15,   223,    40,
      41,   318,   157,   169,   169,   231,   250,   223,   223,   157,
     157,   250,   250
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   149,   151,   150,   152,   153,   153,   153,   153,   154,
     155,   154,   156,   157,   158,   158,   158,   158,   160,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   161,   161,   161,   161,   161,   161,   161,
     161,   161,   161,   161,   161,   162,   162,   162,   163,   163,
     163,   163,   163,   164,   163,   165,   163,   163,   166,   168,
     167,   169,   170,   170,   171,   171,   173,   172,   174,   174,
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
     197,   197,   197,   199,   198,   200,   200,   201,   201,   202,
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
     232,   233,   233,   235,   234,   234,   234,   236,   236,   237,
     237,   238,   238,   239,   239,   240,   240,   242,   241,   243,
     243,   243,   243,   244,   244,   244,   244,   244,   244,   244,
     244,   244,   246,   245,   247,   245,   248,   249,   249,   250,
     251,   250,   252,   250,   253,   250,   254,   254,   254,   254,
     254,   254,   254,   254,   254,   255,   255,   256,   256,   257,
     257,   258,   258,   259,   259,   259,   259,   259,   259,   259,
     259,   259,   259,   259,   260,   260,   261,   261,   261,   261,
     261,   261,   262,   262,   263,   263,   264,   264,   264,   265,
     265,   266,   266,   266,   267,   267,   268,   268,   269,   269,
     269,   270,   271,   271,   272,   272,   272,   273,   273,   274,
     274,   275,   275,   275,   275,   276,   276,   277,   277,   277,
     277,   278,   278,   279,   280,   279,   279,   279,   281,   281,
     282,   282,   283,   284,   284,   285,   285,   286,   286,   287,
     288,   287,   289,   289,   290,   290,   290,   291,   292,   292,
     292,   292,   292,   292,   293,   293,   294,   294,   294,   294,
     295,   295,   295,   295,   295,   296,   296,   297,   297,   297,
     297,   297,   297,   297,   297,   297,   298,   298,   299,   300,
     299,   301,   301,   302,   302,   302,   303,   303,   303,   303,
     304,   304,   305,   305,   306,   306,   307,   307,   308,   308,
     309,   309,   310,   310,   311,   311,   311,   311,   312,   312,
     313,   313,   313,   313,   313,   313,   313,   313,   313,   313,
     313,   313,   313,   313,   313,   314,   314,   314,   314,   314,
     315,   315,   316,   317,   316,   318,   318,   319,   320,   321,
     322,   322,   323,   323,   324,   324,   325,   325,   326,   326,
     327,   327,   328,   328,   329,   330,   329,   331,   331,   332,
     332,   333,   333,   333,   333,   333,   333,   333,   333,   334,
     334,   334,   335,   335,   335,   335,   336,   336,   336,   337,
     337,   338,   338,   339,   339,   340,   340,   341,   341,   342,
     343,   343,   343,   344,   344,   345,   345,   346,   346,   347
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
       1,     1,     1,     2,     3,     2,     1,     3,     5,     1,
       3,     5,     1,     3,     2,     1,     1,     3,     2,     3,
       2,     1,     3,     1,     1,     3,     3,     2,     2,     2,
       1,     1,     6,     1,     1,     1,     1,     2,     1,     2,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     2,
       3,     1,     2,     1,     0,     4,     1,     2,     2,     3,
       2,     3,     1,     1,     2,     1,     2,     1,     2,     1,
       0,     4,     2,     3,     1,     4,     2,     2,     1,     1,
       1,     1,     1,     2,     2,     3,     1,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     0,
       4,     1,     1,     3,     5,     3,     1,     2,     4,     2,
       2,     2,     2,     1,     2,     1,     1,     3,     1,     3,
       1,     1,     2,     1,     4,     2,     2,     1,     2,     0,
       6,     8,     4,     6,     4,     6,     2,     4,     6,     2,
       4,     2,     4,     1,     0,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     4,     1,     3,     2,     2,     2,
       1,     3,     1,     3,     1,     1,     2,     1,     1,     1,
       2,     1,     2,     1,     1,     0,     4,     1,     2,     1,
       3,     3,     3,     2,     2,     3,     3,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     0,     2,     2,
       0,     1,     1,     1,     1,     1,     1,     1,     2,     0
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
#   define YYLOCATION_PRINT(File, Loc)  YY_LOCATION_PRINT(File, *(Loc))

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
#   define YY_LOCATION_PRINT(File, Loc)  YYLOCATION_PRINT(File, &(Loc))

#  else

#   define YYLOCATION_PRINT(File, Loc) ((void) 0)
    /* Temporary convenience wrapper in case some people defined the
       undocumented and private YY_LOCATION_PRINT macros.  */
#   define YY_LOCATION_PRINT  YYLOCATION_PRINT

#  endif
# endif /* !defined YYLOCATION_PRINT */


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, Location, p); \
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
  YY_USE (yykind);
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

  YYLOCATION_PRINT (yyo, yylocationp);
  YYFPRINTF (yyo, ": ");
  yy_symbol_value_print (yyo, yykind, yyvaluep, yylocationp, p);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
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

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, yylsp, Rule, p); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args) ((void) 0)
# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
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
                const yypcontext_t *yyctx)
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
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YY_USE (yykind);
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
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

/* Location data for the lookahead symbol.  */
static YYLTYPE yyloc_default
# if defined YYLTYPE_IS_TRIVIAL && YYLTYPE_IS_TRIVIAL
  = { 1, 1, 1, 1 }
# endif
;
YYLTYPE yylloc = yyloc_default;

    /* Number of syntax errors so far.  */
    int yynerrs = 0;

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
  YY_STACK_PRINT (yyss, yyssp);

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
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
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
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);
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
  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2: /* $@1: %empty  */
#line 2179 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 7377 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3: /* program: $@1 top_compstmt  */
#line 2184 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                    }
#line 7385 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4: /* top_compstmt: top_stmts opt_terms  */
#line 2190 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7393 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5: /* top_stmts: none  */
#line 2196 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7401 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6: /* top_stmts: top_stmt  */
#line 2200 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7409 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7: /* top_stmts: top_stmts terms top_stmt  */
#line 2204 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = stmts_push(p, (yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 7417 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8: /* top_stmts: error top_stmt  */
#line 2208 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7425 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10: /* @2: %empty  */
#line 2215 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7434 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11: /* top_stmt: "'BEGIN'" @2 '{' top_compstmt '}'  */
#line 2220 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 7445 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12: /* bodystmt: compstmt opt_rescue opt_else opt_ensure  */
#line 2232 "mrbgems/mruby-compiler/core/parse.y"
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
#line 7470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13: /* compstmt: stmts opt_terms  */
#line 2255 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7478 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14: /* stmts: none  */
#line 2261 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15: /* stmts: stmt  */
#line 2265 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7494 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16: /* stmts: stmts terms stmt  */
#line 2269 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = stmts_push(p, (yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 7502 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17: /* stmts: error stmt  */
#line 2273 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7510 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18: /* $@3: %empty  */
#line 2278 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 7516 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19: /* stmt: "'alias'" fsym $@3 fsym  */
#line 2279 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 7524 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20: /* stmt: "'undef'" undef_list  */
#line 2283 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].nd));
                    }
#line 7532 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21: /* stmt: stmt "'if' modifier" expr_value  */
#line 2287 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 7540 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22: /* stmt: stmt "'unless' modifier" expr_value  */
#line 2291 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), 0, (yyvsp[-2].nd));
                    }
#line 7548 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23: /* stmt: stmt "'while' modifier" expr_value  */
#line 2295 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd) && node_type_p((yyvsp[-2].nd), NODE_BEGIN)) {
                        (yyval.nd) = new_while_mod(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                      else {
                        (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                    }
#line 7561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24: /* stmt: stmt "'until' modifier" expr_value  */
#line 2304 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd) && node_type_p((yyvsp[-2].nd), NODE_BEGIN)) {
                        (yyval.nd) = new_until_mod(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                      else {
                        (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                    }
#line 7574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25: /* stmt: stmt "'rescue' modifier" stmt  */
#line 2313 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26: /* stmt: "'END'" '{' compstmt '}'  */
#line 2317 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 7591 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28: /* stmt: mlhs '=' command_call  */
#line 2323 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7599 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29: /* stmt: lhs '=' mrhs  */
#line 2327 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 7607 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30: /* stmt: mlhs '=' arg  */
#line 2331 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7615 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31: /* stmt: mlhs '=' mrhs  */
#line 2335 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 7623 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 33: /* command_asgn: lhs '=' command_rhs  */
#line 2342 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7631 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34: /* command_asgn: var_lhs tOP_ASGN command_rhs  */
#line 2346 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7639 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35: /* command_asgn: primary_value '[' opt_call_args ']' tOP_ASGN command_rhs  */
#line 2350 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7647 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36: /* command_asgn: primary_value call_op "local variable or method" tOP_ASGN command_rhs  */
#line 2354 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7655 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37: /* command_asgn: primary_value call_op "constant" tOP_ASGN command_rhs  */
#line 2358 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7663 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38: /* command_asgn: primary_value "::" "constant" tOP_ASGN command_call  */
#line 2362 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 7672 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39: /* command_asgn: primary_value "::" "local variable or method" tOP_ASGN command_rhs  */
#line 2367 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40: /* command_asgn: defn_head f_opt_arglist_paren '=' command  */
#line 2371 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7693 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41: /* command_asgn: defn_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 2380 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7706 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42: /* command_asgn: defs_head f_opt_arglist_paren '=' command  */
#line 2389 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7719 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43: /* command_asgn: defs_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 2398 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44: /* command_asgn: backref tOP_ASGN command_rhs  */
#line 2407 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7741 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 46: /* command_rhs: command_call "'rescue' modifier" stmt  */
#line 2415 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7749 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 49: /* expr: expr "'and'" expr  */
#line 2423 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7757 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50: /* expr: expr "'or'" expr  */
#line 2427 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7765 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51: /* expr: "'not'" opt_nl expr  */
#line 2431 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52: /* expr: '!' command_call  */
#line 2435 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7781 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53: /* $@4: %empty  */
#line 2438 "mrbgems/mruby-compiler/core/parse.y"
                             {p->in_kwarg++;}
#line 7787 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 54: /* expr: arg "=>" $@4 p_expr  */
#line 2439 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* expr => pattern (raises NoMatchingPatternError on failure) */
                      p->in_kwarg--;
                      (yyval.nd) = new_match_pat(p, (yyvsp[-3].nd), (yyvsp[0].nd), TRUE);
                    }
#line 7797 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55: /* $@5: %empty  */
#line 2444 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->in_kwarg++;}
#line 7803 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56: /* expr: arg "'in'" $@5 p_expr  */
#line 2445 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* expr in pattern (returns true/false) */
                      p->in_kwarg--;
                      (yyval.nd) = new_match_pat(p, (yyvsp[-3].nd), (yyvsp[0].nd), FALSE);
                    }
#line 7813 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58: /* defn_head: "'def'" fname  */
#line 2454 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 7824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 59: /* $@6: %empty  */
#line 2463 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 7832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 60: /* defs_head: "'def'" singleton dot_or_colon $@6 fname  */
#line 2467 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 7845 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 61: /* expr_value: expr  */
#line 2478 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 7856 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65: /* block_command: block_call call_op2 operation2 command_args  */
#line 2492 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7864 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66: /* $@7: %empty  */
#line 2498 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 7873 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67: /* cmd_brace_block: "{" $@7 opt_block_param compstmt '}'  */
#line 2505 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 7883 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68: /* command: operation command_args  */
#line 2513 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7891 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69: /* command: operation command_args cmd_brace_block  */
#line 2517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 7900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70: /* command: primary_value call_op operation2 command_args  */
#line 2522 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7908 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71: /* command: primary_value call_op operation2 command_args cmd_brace_block  */
#line 2526 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 7917 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72: /* command: primary_value "::" operation2 command_args  */
#line 2531 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 7925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73: /* command: primary_value "::" operation2 command_args cmd_brace_block  */
#line 2535 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 7934 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74: /* command: "'super'" command_args  */
#line 2540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 7942 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75: /* command: "'yield'" command_args  */
#line 2544 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7950 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76: /* command: "'return'" call_args  */
#line 2548 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7958 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77: /* command: "'break'" call_args  */
#line 2552 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7966 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 78: /* command: "'next'" call_args  */
#line 2556 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7974 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79: /* mlhs: mlhs_basic  */
#line 2562 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7982 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80: /* mlhs: tLPAREN mlhs_inner rparen  */
#line 2566 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7990 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82: /* mlhs_inner: tLPAREN mlhs_inner rparen  */
#line 2573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7998 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83: /* mlhs_basic: mlhs_list  */
#line 2579 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84: /* mlhs_basic: mlhs_list mlhs_item  */
#line 2583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 8014 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85: /* mlhs_basic: mlhs_list "*" mlhs_node  */
#line 2587 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8022 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86: /* mlhs_basic: mlhs_list "*" mlhs_node ',' mlhs_post  */
#line 2591 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87: /* mlhs_basic: mlhs_list "*"  */
#line 2595 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 8038 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88: /* mlhs_basic: mlhs_list "*" ',' mlhs_post  */
#line 2599 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 8046 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89: /* mlhs_basic: "*" mlhs_node  */
#line 2603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 8054 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 90: /* mlhs_basic: "*" mlhs_node ',' mlhs_post  */
#line 2607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8062 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91: /* mlhs_basic: "*"  */
#line 2611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 8070 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92: /* mlhs_basic: "*" ',' mlhs_post  */
#line 2615 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 8078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94: /* mlhs_item: tLPAREN mlhs_inner rparen  */
#line 2622 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 8086 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95: /* mlhs_list: mlhs_item ','  */
#line 2628 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 8094 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96: /* mlhs_list: mlhs_list mlhs_item ','  */
#line 2632 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 8102 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97: /* mlhs_post: mlhs_item  */
#line 2638 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8110 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98: /* mlhs_post: mlhs_list mlhs_item  */
#line 2642 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8118 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99: /* mlhs_node: variable  */
#line 2648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100: /* mlhs_node: primary_value '[' opt_call_args ']'  */
#line 2652 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8134 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101: /* mlhs_node: primary_value call_op "local variable or method"  */
#line 2656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8142 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102: /* mlhs_node: primary_value "::" "local variable or method"  */
#line 2660 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8150 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103: /* mlhs_node: primary_value call_op "constant"  */
#line 2664 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8158 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104: /* mlhs_node: primary_value "::" "constant"  */
#line 2668 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8168 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105: /* mlhs_node: tCOLON3 "constant"  */
#line 2674 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106: /* mlhs_node: backref  */
#line 2680 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 8187 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107: /* lhs: variable  */
#line 2687 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8195 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108: /* lhs: primary_value '[' opt_call_args ']'  */
#line 2691 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8203 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109: /* lhs: primary_value call_op "local variable or method"  */
#line 2695 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8211 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110: /* lhs: primary_value "::" "local variable or method"  */
#line 2699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8219 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111: /* lhs: primary_value call_op "constant"  */
#line 2703 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8227 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112: /* lhs: primary_value "::" "constant"  */
#line 2707 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113: /* lhs: tCOLON3 "constant"  */
#line 2713 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8247 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 114: /* lhs: backref  */
#line 2719 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 8256 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115: /* lhs: "numbered parameter"  */
#line 2724 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 8264 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116: /* cname: "local variable or method"  */
#line 2730 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "class/module name must be CONSTANT");
                    }
#line 8272 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 118: /* cpath: tCOLON3 cname  */
#line 2737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(int_to_node(1), sym_to_node((yyvsp[0].id)));
                    }
#line 8280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 119: /* cpath: cname  */
#line 2741 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(int_to_node(0), sym_to_node((yyvsp[0].id)));
                    }
#line 8288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 120: /* cpath: primary_value "::" cname  */
#line 2745 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), sym_to_node((yyvsp[0].id)));
                    }
#line 8297 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 124: /* fname: op  */
#line 2755 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8306 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125: /* fname: reswords  */
#line 2760 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128: /* undef_list: fsym  */
#line 2771 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(sym_to_node((yyvsp[0].id)), 0);
                    }
#line 8323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129: /* $@8: %empty  */
#line 2774 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 8329 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130: /* undef_list: undef_list ',' $@8 fsym  */
#line 2775 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), sym_to_node((yyvsp[0].id)));
                    }
#line 8337 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131: /* op: '|'  */
#line 2780 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(or);     }
#line 8343 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132: /* op: '^'  */
#line 2781 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(xor);    }
#line 8349 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133: /* op: '&'  */
#line 2782 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(and);    }
#line 8355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134: /* op: "<=>"  */
#line 2783 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(cmp);    }
#line 8361 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135: /* op: "=="  */
#line 2784 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eq);     }
#line 8367 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136: /* op: "==="  */
#line 2785 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eqq);    }
#line 8373 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137: /* op: "=~"  */
#line 2786 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(match);  }
#line 8379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138: /* op: "!~"  */
#line 2787 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(nmatch); }
#line 8385 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139: /* op: '>'  */
#line 2788 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(gt);     }
#line 8391 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140: /* op: ">="  */
#line 2789 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(ge);     }
#line 8397 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141: /* op: '<'  */
#line 2790 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lt);     }
#line 8403 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142: /* op: "<="  */
#line 2791 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(le);     }
#line 8409 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143: /* op: "!="  */
#line 2792 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neq);    }
#line 8415 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144: /* op: "<<"  */
#line 2793 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lshift); }
#line 8421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145: /* op: ">>"  */
#line 2794 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(rshift); }
#line 8427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146: /* op: '+'  */
#line 2795 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(add);    }
#line 8433 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147: /* op: '-'  */
#line 2796 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(sub);    }
#line 8439 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148: /* op: '*'  */
#line 2797 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 8445 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149: /* op: "*"  */
#line 2798 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 8451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150: /* op: '/'  */
#line 2799 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(div);    }
#line 8457 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151: /* op: '%'  */
#line 2800 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mod);    }
#line 8463 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152: /* op: tPOW  */
#line 2801 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 8469 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153: /* op: "**"  */
#line 2802 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 8475 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154: /* op: '!'  */
#line 2803 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(not);    }
#line 8481 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155: /* op: '~'  */
#line 2804 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neg);    }
#line 8487 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156: /* op: "unary plus"  */
#line 2805 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(plus);   }
#line 8493 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157: /* op: "unary minus"  */
#line 2806 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(minus);  }
#line 8499 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 158: /* op: tAREF  */
#line 2807 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aref);   }
#line 8505 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 159: /* op: tASET  */
#line 2808 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aset);   }
#line 8511 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 160: /* op: '`'  */
#line 2809 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(tick);   }
#line 8517 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201: /* arg: lhs '=' arg_rhs  */
#line 2827 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8525 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202: /* arg: var_lhs tOP_ASGN arg_rhs  */
#line 2831 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8533 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203: /* arg: primary_value '[' opt_call_args ']' tOP_ASGN arg_rhs  */
#line 2835 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8541 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204: /* arg: primary_value call_op "local variable or method" tOP_ASGN arg_rhs  */
#line 2839 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8549 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205: /* arg: primary_value call_op "constant" tOP_ASGN arg_rhs  */
#line 2843 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8557 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206: /* arg: primary_value "::" "local variable or method" tOP_ASGN arg_rhs  */
#line 2847 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8565 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207: /* arg: primary_value "::" "constant" tOP_ASGN arg_rhs  */
#line 2851 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208: /* arg: tCOLON3 "constant" tOP_ASGN arg_rhs  */
#line 2856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "constant re-assignment");
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8583 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209: /* arg: backref tOP_ASGN arg_rhs  */
#line 2861 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8592 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210: /* arg: arg ".." arg  */
#line 2866 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8600 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211: /* arg: arg ".."  */
#line 2870 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 8608 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212: /* arg: tBDOT2 arg  */
#line 2874 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 8616 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213: /* arg: arg "..." arg  */
#line 2878 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214: /* arg: arg "..."  */
#line 2882 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 8632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215: /* arg: tBDOT3 arg  */
#line 2886 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 8640 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216: /* arg: arg '+' arg  */
#line 2890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 8648 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217: /* arg: arg '-' arg  */
#line 2894 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 8656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218: /* arg: arg '*' arg  */
#line 2898 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 8664 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219: /* arg: arg '/' arg  */
#line 2902 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 8672 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220: /* arg: arg '%' arg  */
#line 2906 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 8680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221: /* arg: arg tPOW arg  */
#line 2910 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 8688 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222: /* arg: tUMINUS_NUM "integer literal" tPOW arg  */
#line 2914 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 8696 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223: /* arg: tUMINUS_NUM "float literal" tPOW arg  */
#line 2918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 8704 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224: /* arg: "unary plus" arg  */
#line 2922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 8712 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225: /* arg: "unary minus" arg  */
#line 2926 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 8720 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226: /* arg: arg '|' arg  */
#line 2930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 8728 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227: /* arg: arg '^' arg  */
#line 2934 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 8736 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228: /* arg: arg '&' arg  */
#line 2938 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 8744 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229: /* arg: arg "<=>" arg  */
#line 2942 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 8752 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230: /* arg: arg '>' arg  */
#line 2946 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 8760 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231: /* arg: arg ">=" arg  */
#line 2950 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 8768 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232: /* arg: arg '<' arg  */
#line 2954 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 8776 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233: /* arg: arg "<=" arg  */
#line 2958 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 8784 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234: /* arg: arg "==" arg  */
#line 2962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 8792 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235: /* arg: arg "===" arg  */
#line 2966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 8800 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236: /* arg: arg "!=" arg  */
#line 2970 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 8808 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237: /* arg: arg "=~" arg  */
#line 2974 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 8816 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238: /* arg: arg "!~" arg  */
#line 2978 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 8824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239: /* arg: '!' arg  */
#line 2982 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 8832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240: /* arg: '~' arg  */
#line 2986 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 8840 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241: /* arg: arg "<<" arg  */
#line 2990 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 8848 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242: /* arg: arg ">>" arg  */
#line 2994 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 8856 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243: /* arg: arg "&&" arg  */
#line 2998 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8864 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244: /* arg: arg "||" arg  */
#line 3002 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8872 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245: /* arg: arg '?' arg opt_nl ':' arg  */
#line 3006 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 8880 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246: /* arg: arg '?' arg opt_nl "label" arg  */
#line 3010 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 8888 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247: /* arg: defn_head f_opt_arglist_paren '=' arg  */
#line 3014 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8901 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248: /* arg: defn_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 3023 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8914 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 249: /* arg: defs_head f_opt_arglist_paren '=' arg  */
#line 3032 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8927 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250: /* arg: defs_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 3041 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8940 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251: /* arg: primary  */
#line 3050 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8948 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253: /* aref_args: args trailer  */
#line 3057 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254: /* aref_args: args comma assocs trailer  */
#line 3061 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd)));
                    }
#line 8964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255: /* aref_args: assocs trailer  */
#line 3065 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_hash(p, (yyvsp[-1].nd)), 0);
                    }
#line 8972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256: /* arg_rhs: arg  */
#line 3071 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257: /* arg_rhs: arg "'rescue' modifier" arg  */
#line 3075 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8989 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 258: /* paren_args: '(' opt_call_args ')'  */
#line 3082 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 259: /* paren_args: '(' args comma tBDOT3 rparen  */
#line 3086 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      (yyval.nd) = new_callargs(p, push((yyvsp[-3].nd), new_splat(p, new_lvar(p, r))),
                                        list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k))),
                                        new_block_arg(p, new_lvar(p, b)));
                    }
#line 9010 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 260: /* paren_args: '(' tBDOT3 rparen  */
#line 3095 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265: /* opt_call_args: args comma  */
#line 3118 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-1].nd),0,0);
                    }
#line 9037 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266: /* opt_call_args: args comma assocs comma  */
#line 3122 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-3].nd),(yyvsp[-1].nd),0);
                    }
#line 9045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267: /* opt_call_args: assocs comma  */
#line 3126 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,0,(yyvsp[-1].nd),0);
                    }
#line 9053 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268: /* call_args: command  */
#line 3132 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_callargs(p, list1((yyvsp[0].nd)), 0, 0);
                    }
#line 9062 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269: /* call_args: args opt_block_arg  */
#line 3137 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-1].nd), 0, (yyvsp[0].nd));
                    }
#line 9070 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270: /* call_args: assocs opt_block_arg  */
#line 3141 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 271: /* call_args: args comma assocs opt_block_arg  */
#line 3145 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9086 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272: /* call_args: block_arg  */
#line 3149 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, 0, (yyvsp[0].nd));
                    }
#line 9094 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273: /* @9: %empty  */
#line 3154 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 9103 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274: /* command_args: @9 call_args  */
#line 3159 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9112 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275: /* block_arg: "&" arg  */
#line 3166 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 9120 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 276: /* block_arg: "&"  */
#line 3170 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, 0);
                    }
#line 9128 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277: /* opt_block_arg: comma block_arg  */
#line 3176 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9136 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278: /* opt_block_arg: none  */
#line 3180 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9144 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280: /* args: arg  */
#line 3189 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281: /* args: "*"  */
#line 3194 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 9161 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282: /* args: "*" arg  */
#line 3198 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 9169 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 283: /* args: args comma arg  */
#line 3202 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 284: /* args: args comma "*"  */
#line 3207 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 9186 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 285: /* args: args comma "*" arg  */
#line 3211 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 9194 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 286: /* mrhs: args comma arg  */
#line 3217 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9203 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 287: /* mrhs: args comma "*" arg  */
#line 3222 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 9211 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 288: /* mrhs: "*" arg  */
#line 3226 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 9219 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 290: /* primary: string  */
#line 3233 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_str(p, (yyvsp[0].nd));
                    }
#line 9227 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 291: /* primary: xstring  */
#line 3237 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_xstr(p, (yyvsp[0].nd));
                    }
#line 9235 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296: /* primary: "method"  */
#line 3245 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 9243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297: /* @10: %empty  */
#line 3249 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9252 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298: /* primary: "'begin'" @10 bodystmt "'end'"  */
#line 3255 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = new_begin(p, (yyvsp[-1].nd));
                    }
#line 9261 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299: /* @11: %empty  */
#line 3260 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300: /* $@12: %empty  */
#line 3264 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 9276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301: /* primary: "(" @11 stmt $@12 rparen  */
#line 3265 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 9285 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302: /* $@13: %empty  */
#line 3269 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 9291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303: /* primary: "(" $@13 rparen  */
#line 3270 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9299 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304: /* primary: tLPAREN compstmt ')'  */
#line 3274 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9307 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305: /* primary: primary_value "::" "constant"  */
#line 3278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 9315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306: /* primary: tCOLON3 "constant"  */
#line 3282 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 9323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307: /* primary: "[" aref_args ']'  */
#line 3286 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                    }
#line 9331 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308: /* primary: tLBRACE assoc_list '}'  */
#line 3290 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                    }
#line 9339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309: /* primary: "'return'"  */
#line 3294 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 9347 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310: /* primary: "'yield'" opt_paren_args  */
#line 3298 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 9355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 311: /* primary: "'not'" '(' expr rparen  */
#line 3302 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 9363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312: /* primary: "'not'" '(' rparen  */
#line 3306 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 9371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313: /* primary: operation brace_block  */
#line 3310 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), new_callargs(p, 0, 0, (yyvsp[0].nd)));
                    }
#line 9379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315: /* primary: method_call brace_block  */
#line 3315 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9388 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316: /* @14: %empty  */
#line 3320 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 9399 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317: /* @15: %empty  */
#line 3327 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9408 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318: /* primary: "->" @14 f_larglist @15 lambda_body  */
#line 3332 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 9421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319: /* primary: "'if'" expr_value then compstmt if_tail "'end'"  */
#line 3344 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 9430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320: /* primary: "'unless'" expr_value then compstmt opt_else "'end'"  */
#line 3352 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd), (yyvsp[-2].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 9439 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321: /* $@16: %empty  */
#line 3356 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 9445 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322: /* $@17: %empty  */
#line 3356 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 9451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323: /* primary: "'while'" $@16 expr_value do $@17 compstmt "'end'"  */
#line 3359 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 9460 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324: /* $@18: %empty  */
#line 3363 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 9466 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325: /* $@19: %empty  */
#line 3363 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 9472 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326: /* primary: "'until'" $@18 expr_value do $@19 compstmt "'end'"  */
#line 3366 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 9481 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327: /* primary: "'case'" expr_value opt_terms case_body "'end'"  */
#line 3373 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 9489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328: /* primary: "'case'" opt_terms case_body "'end'"  */
#line 3377 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 9497 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329: /* primary: "'case'" expr_value opt_terms "'in'" p_expr then compstmt in_clauses "'end'"  */
#line 3385 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-4].nd), NULL, (yyvsp[-2].nd), FALSE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-7].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 9506 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330: /* primary: "'case'" expr_value opt_terms "'in'" p_expr "'if' modifier" expr_value then compstmt in_clauses "'end'"  */
#line 3394 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-4].nd), (yyvsp[-2].nd), FALSE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-9].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 9515 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331: /* primary: "'case'" expr_value opt_terms "'in'" p_expr "'unless' modifier" expr_value then compstmt in_clauses "'end'"  */
#line 3403 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-4].nd), (yyvsp[-2].nd), TRUE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-9].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 9524 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332: /* $@20: %empty  */
#line 3408 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 9530 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333: /* $@21: %empty  */
#line 3410 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 9536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334: /* primary: "'for'" for_var "'in'" $@20 expr_value do $@21 compstmt "'end'"  */
#line 3413 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 9545 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335: /* @22: %empty  */
#line 3419 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 9556 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336: /* primary: "'class'" cpath superclass @22 bodystmt "'end'"  */
#line 3427 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 9567 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337: /* @23: %empty  */
#line 3435 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 9576 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338: /* @24: %empty  */
#line 3440 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), int_to_node(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 9586 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 339: /* primary: "'class'" "<<" expr @23 term @24 bodystmt "'end'"  */
#line 3447 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = node_to_int((yyvsp[-2].nd)->cdr);
                    }
#line 9599 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340: /* @25: %empty  */
#line 3457 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 9610 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 341: /* primary: "'module'" cpath @25 bodystmt "'end'"  */
#line 3465 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 9621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 342: /* primary: defn_head f_arglist bodystmt "'end'"  */
#line 3475 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 9632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 343: /* primary: defs_head f_arglist bodystmt "'end'"  */
#line 3485 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 9644 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 344: /* primary: "'break'"  */
#line 3493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 9652 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 345: /* primary: "'next'"  */
#line 3497 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 9660 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 346: /* primary: "'redo'"  */
#line 3501 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 9668 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 347: /* primary: "'retry'"  */
#line 3505 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 9676 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 348: /* primary_value: primary  */
#line 3511 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9685 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355: /* if_tail: "'elsif'" expr_value then compstmt if_tail  */
#line 3530 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9693 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357: /* opt_else: "'else'" compstmt  */
#line 3537 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9701 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358: /* for_var: lhs  */
#line 3543 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 9709 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360: /* f_margs: f_arg  */
#line 3550 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 9717 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361: /* f_margs: f_arg ',' "*" f_norm_arg  */
#line 3554 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_lvar(p, (yyvsp[0].id)), 0);
                    }
#line 9725 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362: /* f_margs: f_arg ',' "*" f_norm_arg ',' f_arg  */
#line 3558 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_lvar(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9733 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363: /* f_margs: f_arg ',' "*"  */
#line 3562 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3((yyvsp[-2].nd), int_to_node(-1), 0);
                    }
#line 9742 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364: /* f_margs: f_arg ',' "*" ',' f_arg  */
#line 3567 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), int_to_node(-1), (yyvsp[0].nd));
                    }
#line 9750 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365: /* f_margs: "*" f_norm_arg  */
#line 3571 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_lvar(p, (yyvsp[0].id)), 0);
                    }
#line 9758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366: /* f_margs: "*" f_norm_arg ',' f_arg  */
#line 3575 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_lvar(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9766 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367: /* f_margs: "*"  */
#line 3579 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3(0, int_to_node(-1), 0);
                    }
#line 9775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368: /* $@26: %empty  */
#line 3584 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                    }
#line 9783 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369: /* f_margs: "*" ',' $@26 f_arg  */
#line 3588 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, int_to_node(-1), (yyvsp[0].nd));
                    }
#line 9791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370: /* block_args_tail: f_block_kwarg ',' f_kwrest opt_f_block_arg  */
#line 3594 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 9799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371: /* block_args_tail: f_block_kwarg opt_f_block_arg  */
#line 3598 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372: /* block_args_tail: f_kwrest opt_f_block_arg  */
#line 3602 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 9815 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373: /* block_args_tail: f_block_arg  */
#line 3606 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9823 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374: /* opt_block_args_tail: ',' block_args_tail  */
#line 3612 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9831 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375: /* opt_block_args_tail: %empty  */
#line 3616 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9839 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3622 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9847 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3626 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9855 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378: /* block_param: f_arg ',' f_block_optarg opt_block_args_tail  */
#line 3630 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9863 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379: /* block_param: f_arg ',' f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3634 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9871 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380: /* block_param: f_arg ',' f_rest_arg opt_block_args_tail  */
#line 3638 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9879 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381: /* block_param: f_arg ',' opt_block_args_tail  */
#line 3642 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9887 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382: /* block_param: f_arg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3646 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9895 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383: /* block_param: f_arg opt_block_args_tail  */
#line 3650 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384: /* block_param: f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3654 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9911 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385: /* block_param: f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3658 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9919 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386: /* block_param: f_block_optarg opt_block_args_tail  */
#line 3662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9927 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387: /* block_param: f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3666 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9935 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388: /* block_param: f_rest_arg opt_block_args_tail  */
#line 3670 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9943 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389: /* block_param: f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3674 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9951 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390: /* block_param: block_args_tail  */
#line 3678 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9959 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 391: /* opt_block_param: none  */
#line 3684 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 9968 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392: /* opt_block_param: block_param_def  */
#line 3689 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9977 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393: /* $@27: %empty  */
#line 3695 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p);}
#line 9983 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 394: /* block_param_def: '|' $@27 opt_bv_decl '|'  */
#line 3696 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9991 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395: /* block_param_def: "||"  */
#line 3700 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 10000 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396: /* block_param_def: '|' block_param opt_bv_decl '|'  */
#line 3705 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 10008 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397: /* opt_bv_decl: opt_nl  */
#line 3711 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10016 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398: /* opt_bv_decl: opt_nl ';' bv_decls opt_nl  */
#line 3715 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10024 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401: /* bvar: "local variable or method"  */
#line 3725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 10033 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403: /* f_larglist: '(' f_args opt_bv_decl ')'  */
#line 3733 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 10041 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404: /* f_larglist: f_args  */
#line 3737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10049 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405: /* lambda_body: tLAMBEG compstmt '}'  */
#line 3743 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406: /* lambda_body: "'do' for lambda" bodystmt "'end'"  */
#line 3747 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10065 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407: /* @28: %empty  */
#line 3753 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10075 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408: /* do_block: "'do' for block" @28 opt_block_param bodystmt "'end'"  */
#line 3761 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10086 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409: /* block_call: command do_block  */
#line 3770 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10095 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410: /* block_call: block_call call_op2 operation2 opt_paren_args  */
#line 3775 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 10103 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411: /* block_call: block_call call_op2 operation2 opt_paren_args brace_block  */
#line 3779 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 10112 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412: /* block_call: block_call call_op2 operation2 command_args do_block  */
#line 3784 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 10121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413: /* method_call: operation paren_args  */
#line 3791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 10129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414: /* method_call: primary_value call_op operation2 opt_paren_args  */
#line 3795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 10137 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415: /* method_call: primary_value "::" operation2 paren_args  */
#line 3799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 10145 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416: /* method_call: primary_value "::" operation3  */
#line 3803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 10153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417: /* method_call: primary_value call_op paren_args  */
#line 3807 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 10161 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418: /* method_call: primary_value "::" paren_args  */
#line 3811 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), tCOLON2);
                    }
#line 10169 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419: /* method_call: "'super'" paren_args  */
#line 3815 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 10177 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 420: /* method_call: "'super'"  */
#line 3819 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 10185 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 421: /* method_call: primary_value '[' opt_call_args ']'  */
#line 3823 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 10193 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 422: /* @29: %empty  */
#line 3829 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10203 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423: /* brace_block: '{' @29 opt_block_param compstmt '}'  */
#line 3836 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 424: /* @30: %empty  */
#line 3843 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10224 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 425: /* brace_block: "'do'" @30 opt_block_param bodystmt "'end'"  */
#line 3850 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10235 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 426: /* case_body: "'when'" args then compstmt cases  */
#line 3861 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 10243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 427: /* cases: opt_else  */
#line 3867 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 10256 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 429: /* in_clauses: opt_else  */
#line 3881 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd) ? list1(new_in(p, NULL, NULL, (yyvsp[0].nd), FALSE)) : 0;
                    }
#line 10264 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 430: /* $@31: %empty  */
#line 3884 "mrbgems/mruby-compiler/core/parse.y"
                                    {p->in_kwarg--;}
#line 10270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 431: /* in_clauses: "'in'" p_expr $@31 then compstmt in_clauses  */
#line 3885 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-4].nd), NULL, (yyvsp[-1].nd), FALSE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10279 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 432: /* $@32: %empty  */
#line 3889 "mrbgems/mruby-compiler/core/parse.y"
                                    {p->in_kwarg--;}
#line 10285 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 433: /* in_clauses: "'in'" p_expr $@32 "'if' modifier" expr_value then compstmt in_clauses  */
#line 3890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-3].nd), (yyvsp[-1].nd), FALSE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10294 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 434: /* $@33: %empty  */
#line 3894 "mrbgems/mruby-compiler/core/parse.y"
                                    {p->in_kwarg--;}
#line 10300 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435: /* in_clauses: "'in'" p_expr $@33 "'unless' modifier" expr_value then compstmt in_clauses  */
#line 3895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-3].nd), (yyvsp[-1].nd), TRUE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10309 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 437: /* p_expr: p_args_head p_as  */
#line 3906 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, push((yyvsp[-1].nd), (yyvsp[0].nd)), 0, 0);
                    }
#line 10317 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438: /* p_expr: p_args_head p_rest  */
#line 3910 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, (yyvsp[-1].nd), (yyvsp[0].nd), 0);
                    }
#line 10325 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439: /* p_expr: p_args_head p_rest ',' p_args_post  */
#line 3914 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, (yyvsp[-3].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10333 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440: /* p_expr: p_rest  */
#line 3918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[0].nd), 0);
                    }
#line 10341 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441: /* p_expr: p_rest ',' p_args_post  */
#line 3922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10349 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442: /* p_expr: p_hash_elems  */
#line 3926 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Brace-less hash pattern: in a:, b: x */
                      (yyval.nd) = new_pat_hash(p, (yyvsp[0].nd), 0);
                    }
#line 10358 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443: /* p_expr: p_hash_elems ',' p_kwrest  */
#line 3931 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Brace-less hash pattern with kwrest: in a:, **rest */
                      (yyval.nd) = new_pat_hash(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10367 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444: /* p_expr: p_kwrest  */
#line 3936 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Brace-less kwrest only: in **rest */
                      (yyval.nd) = new_pat_hash(p, 0, (yyvsp[0].nd));
                    }
#line 10376 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445: /* p_args_head: p_as ','  */
#line 3944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 10384 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446: /* p_args_head: p_args_head p_as ','  */
#line 3948 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 10392 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447: /* p_args_post: p_as  */
#line 3955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 448: /* p_args_post: p_args_post ',' p_as  */
#line 3959 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10408 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450: /* p_as: p_alt "=>" "local variable or method"  */
#line 3966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_as(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 10416 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 452: /* p_alt: p_alt '|' p_value  */
#line 3973 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_alt(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10424 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 454: /* p_value: numeric  */
#line 3980 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10432 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 455: /* p_value: symbol  */
#line 3984 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10440 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456: /* p_value: tSTRING  */
#line 3988 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_str(p, list1((yyvsp[0].nd))));
                    }
#line 10448 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457: /* p_value: "'nil'"  */
#line 3992 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_nil(p));
                    }
#line 10456 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458: /* p_value: "'true'"  */
#line 3996 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_true(p));
                    }
#line 10464 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459: /* p_value: "'false'"  */
#line 4000 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_false(p));
                    }
#line 10472 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460: /* p_value: p_const  */
#line 4004 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 463: /* p_value: '^' "local variable or method"  */
#line 4010 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_pin(p, (yyvsp[0].id));
                    }
#line 10488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 464: /* p_array: "[" p_array_body ']'  */
#line 4017 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10496 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465: /* p_array: "[" ']'  */
#line 4021 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, 0, 0, 0);
                    }
#line 10504 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 466: /* p_array_body: p_array_elems  */
#line 4028 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Just pre elements, no rest */
                      (yyval.nd) = new_pat_array(p, (yyvsp[0].nd), 0, 0);
                    }
#line 10513 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467: /* p_array_body: p_array_elems ',' p_rest  */
#line 4033 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Pre elements + rest, no post */
                      (yyval.nd) = new_pat_array(p, (yyvsp[-2].nd), (yyvsp[0].nd), 0);
                    }
#line 10522 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 468: /* p_array_body: p_array_elems ',' p_rest ',' p_array_elems  */
#line 4038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Pre + rest + post */
                      (yyval.nd) = new_pat_array(p, (yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10531 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 469: /* p_array_body: p_rest  */
#line 4043 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Just rest, no pre or post */
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[0].nd), 0);
                    }
#line 10540 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 470: /* p_array_body: p_rest ',' p_array_elems  */
#line 4048 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Rest + post, no pre */
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10549 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 471: /* p_array_body: p_rest ',' p_array_elems ',' p_rest  */
#line 4053 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Find pattern: [*pre, elems, *post] */
                      (yyval.nd) = new_pat_find(p, (yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10558 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 472: /* p_array_elems: p_as  */
#line 4061 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10566 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473: /* p_array_elems: p_array_elems ',' p_as  */
#line 4065 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474: /* p_rest: "*" "local variable or method"  */
#line 4072 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 10582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475: /* p_rest: "*"  */
#line 4076 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Anonymous rest pattern */
                      (yyval.nd) = (node*)-1;
                    }
#line 10591 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476: /* p_const: "constant"  */
#line 4084 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 10599 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 477: /* p_const: p_const "::" "constant"  */
#line 4088 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 10607 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478: /* p_const: tCOLON3 "constant"  */
#line 4092 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 10615 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479: /* p_hash: tLBRACE p_hash_body '}'  */
#line 4099 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10623 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480: /* p_hash: tLBRACE '}'  */
#line 4103 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_hash(p, 0, 0);
                    }
#line 10631 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481: /* p_hash_body: p_hash_elems  */
#line 4110 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_hash(p, (yyvsp[0].nd), 0);
                    }
#line 10639 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482: /* p_hash_body: p_hash_elems ',' p_kwrest  */
#line 4114 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_hash(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10647 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483: /* p_hash_body: p_kwrest  */
#line 4118 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_hash(p, 0, (yyvsp[0].nd));
                    }
#line 10655 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484: /* p_hash_elems: p_hash_elem  */
#line 4125 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10663 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485: /* p_hash_elems: p_hash_elems ',' p_hash_elem  */
#line 4129 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486: /* p_hash_elem: "local variable or method" "label" p_as  */
#line 4138 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* {key: pattern} */
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 10680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487: /* p_hash_elem: "local variable or method" "label"  */
#line 4143 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* {key:} shorthand - binds to variable with same name */
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), new_pat_var(p, (yyvsp[-1].id)));
                    }
#line 10689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488: /* p_kwrest: "**" "local variable or method"  */
#line 4151 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 10697 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489: /* p_kwrest: "**" "'nil'"  */
#line 4155 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* **nil - exact match, no extra keys allowed */
                      (yyval.nd) = (node*)-1;
                    }
#line 10706 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 490: /* p_kwrest: "**"  */
#line 4160 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* ** - anonymous rest, discards extra keys */
                      (yyval.nd) = (node*)-2;
                    }
#line 10715 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491: /* p_var: "local variable or method"  */
#line 4167 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 10723 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492: /* opt_rescue: "'rescue'" exc_list exc_var then compstmt opt_rescue  */
#line 4175 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 10732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 494: /* exc_list: arg  */
#line 4183 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10740 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 497: /* exc_var: "=>" lhs  */
#line 4191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10748 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499: /* opt_ensure: "'ensure'" compstmt  */
#line 4198 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10756 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506: /* string: string string_fragment  */
#line 4212 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10764 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507: /* string_fragment: "character literal"  */
#line 4218 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* tCHAR is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508: /* string_fragment: tSTRING  */
#line 4223 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* tSTRING is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509: /* string_fragment: "string literal" tSTRING  */
#line 4228 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* $2 is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510: /* string_fragment: "string literal" string_rep tSTRING  */
#line 4233 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512: /* string_rep: string_rep string_interp  */
#line 4240 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513: /* string_interp: tSTRING_MID  */
#line 4246 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* $1 is already in (len . str) format */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10816 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514: /* @34: %empty  */
#line 4251 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 10824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515: /* string_interp: tSTRING_PART @34 compstmt '}'  */
#line 4256 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p,(yyvsp[-2].nd));
                      /* $1 is already in (len . str) format, create (-1 . node) for expression */
                      node *expr_elem = cons(int_to_node(-1), (yyvsp[-1].nd));
                      (yyval.nd) = list2((yyvsp[-3].nd), expr_elem);
                    }
#line 10835 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516: /* string_interp: tLITERAL_DELIM  */
#line 4263 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 10843 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517: /* string_interp: tHD_LITERAL_DELIM heredoc_bodies  */
#line 4267 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 10851 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518: /* xstring: tXSTRING_BEG tXSTRING  */
#line 4273 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = cons((yyvsp[0].nd), (node*)NULL);
                    }
#line 10859 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519: /* xstring: tXSTRING_BEG string_rep tXSTRING  */
#line 4277 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520: /* regexp: tREGEXP_BEG tREGEXP  */
#line 4283 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *data = (yyvsp[0].nd);  /* ((len . pattern) . (flags . encoding)) */
                      const char *flags = (const char*)data->cdr->car;
                      const char *encoding = (const char*)data->cdr->cdr;
                      /* Use data->car directly as pattern_list: (len . pattern) */
                      node *pattern_list = cons(data->car, (node*)NULL);
                      (yyval.nd) = new_regx(p, pattern_list, flags, encoding);
                    }
#line 10880 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521: /* regexp: tREGEXP_BEG string_rep tREGEXP  */
#line 4292 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *data = (yyvsp[0].nd);  /* ((len . pattern) . (flags . encoding)) */
                      const char *flags = (const char*)data->cdr->car;
                      const char *encoding = (const char*)data->cdr->cdr;
                      /* Append the pattern from $3->car to the string list $2 */
                      node *complete_list = push((yyvsp[-1].nd), data->car);
                      (yyval.nd) = new_regx(p, complete_list, flags, encoding);
                    }
#line 10893 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525: /* heredoc_body: tHEREDOC_END  */
#line 4310 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, new_str_empty(p));
                      heredoc_end(p);
                    }
#line 10903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526: /* heredoc_body: heredoc_string_rep tHEREDOC_END  */
#line 4316 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 10911 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529: /* heredoc_string_interp: tHD_STRING_MID  */
#line 4326 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 10921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530: /* @35: %empty  */
#line 4332 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 10929 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531: /* heredoc_string_interp: tHD_STRING_PART @35 compstmt '}'  */
#line 4337 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p, (yyvsp[-2].nd));
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      /* $1 is already in (len . str) format, create (-1 . node) for expression */
                      node *expr_elem = cons(int_to_node(-1), (yyvsp[-1].nd));
                      info->doc = push(push(info->doc, (yyvsp[-3].nd)), expr_elem);
                    }
#line 10941 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532: /* words: tWORDS_BEG tSTRING  */
#line 4347 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 10949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533: /* words: tWORDS_BEG string_rep tSTRING  */
#line 4351 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      n = push(n, (yyvsp[0].nd));
                      (yyval.nd) = new_words(p, n);
                    }
#line 10959 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534: /* symbol: basic_symbol  */
#line 4359 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 10967 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535: /* symbol: "symbol" "string literal" string_rep tSTRING  */
#line 4363 "mrbgems/mruby-compiler/core/parse.y"
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
#line 10983 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536: /* symbol: "symbol" "numbered parameter"  */
#line 4375 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[0].num));
                      (yyval.nd) = new_sym(p, sym);
                    }
#line 10992 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537: /* basic_symbol: "symbol" sym  */
#line 4382 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_END;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11001 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542: /* sym: tSTRING  */
#line 4393 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 11009 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543: /* sym: "string literal" tSTRING  */
#line 4397 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 11017 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544: /* symbols: tSYMBOLS_BEG tSTRING  */
#line 4403 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 11025 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545: /* symbols: tSYMBOLS_BEG string_rep tSTRING  */
#line 4407 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      n = push(n, (yyvsp[0].nd));
                      (yyval.nd) = new_symbols(p, n);
                    }
#line 11035 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548: /* numeric: tUMINUS_NUM "integer literal"  */
#line 4417 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 11043 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549: /* numeric: tUMINUS_NUM "float literal"  */
#line 4421 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 11051 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550: /* variable: "local variable or method"  */
#line 4427 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 11059 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551: /* variable: "instance variable"  */
#line 4431 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 11067 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552: /* variable: "global variable"  */
#line 4435 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 11075 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553: /* variable: "class variable"  */
#line 4439 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 11083 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554: /* variable: "constant"  */
#line 4443 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 11091 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555: /* var_lhs: variable  */
#line 4449 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 11099 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 556: /* var_lhs: "numbered parameter"  */
#line 4453 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 11107 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557: /* var_ref: variable  */
#line 4459 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 11115 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558: /* var_ref: "numbered parameter"  */
#line 4463 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 11123 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559: /* var_ref: "'nil'"  */
#line 4467 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 11131 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560: /* var_ref: "'self'"  */
#line 4471 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 11139 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561: /* var_ref: "'true'"  */
#line 4475 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 11147 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562: /* var_ref: "'false'"  */
#line 4479 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 11155 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563: /* var_ref: "'__FILE__'"  */
#line 4483 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, cons(cons(int_to_node(strlen(fn)), (node*)fn), (node*)NULL));
                    }
#line 11167 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564: /* var_ref: "'__LINE__'"  */
#line 4491 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 11178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 565: /* var_ref: "'__ENCODING__'"  */
#line 4498 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, MRB_SYM_2(p->mrb, __ENCODING__), 0);
                    }
#line 11186 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 568: /* superclass: %empty  */
#line 4508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 11194 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 569: /* $@36: %empty  */
#line 4512 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 11203 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 570: /* superclass: '<' $@36 expr_value term  */
#line 4517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11211 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573: /* f_arglist_paren: '(' f_args rparen  */
#line 4533 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 11221 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574: /* f_arglist_paren: '(' f_arg ',' tBDOT3 rparen  */
#line 4539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 11229 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 575: /* f_arglist_paren: '(' tBDOT3 rparen  */
#line 4543 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 11237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 577: /* f_arglist: f_args term  */
#line 4550 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11245 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 578: /* f_arglist: f_arg ',' tBDOT3 term  */
#line 4554 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 11253 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 579: /* f_arglist: "..." term  */
#line 4558 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 11261 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 580: /* f_label: "local variable or method" "label"  */
#line 4564 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 11269 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 581: /* f_label: "numbered parameter" "label"  */
#line 4568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 11277 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 582: /* f_kw: f_label arg  */
#line 4574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11287 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 583: /* f_kw: f_label  */
#line 4580 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 11296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 584: /* f_block_kw: f_label primary_value  */
#line 4587 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11306 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 585: /* f_block_kw: f_label  */
#line 4593 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 11315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 586: /* f_block_kwarg: f_block_kw  */
#line 4600 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 587: /* f_block_kwarg: f_block_kwarg ',' f_block_kw  */
#line 4604 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11331 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 588: /* f_kwarg: f_kw  */
#line 4610 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 589: /* f_kwarg: f_kwarg ',' f_kw  */
#line 4614 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11347 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 592: /* f_kwrest: kwrest_mark "local variable or method"  */
#line 4624 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 593: /* f_kwrest: kwrest_mark  */
#line 4628 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(pow);
                    }
#line 11363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 594: /* args_tail: f_kwarg ',' f_kwrest opt_f_block_arg  */
#line 4634 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 11371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 595: /* args_tail: f_kwarg opt_f_block_arg  */
#line 4638 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 11379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 596: /* args_tail: f_kwrest opt_f_block_arg  */
#line 4642 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 11387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 597: /* args_tail: f_block_arg  */
#line 4646 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 11395 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 598: /* opt_args_tail: ',' args_tail  */
#line 4652 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 11403 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 599: /* opt_args_tail: %empty  */
#line 4656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 11411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 600: /* f_args: f_arg ',' f_optarg ',' f_rest_arg opt_args_tail  */
#line 4662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11419 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 601: /* f_args: f_arg ',' f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4666 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 602: /* f_args: f_arg ',' f_optarg opt_args_tail  */
#line 4670 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 11435 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 603: /* f_args: f_arg ',' f_optarg ',' f_arg opt_args_tail  */
#line 4674 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 604: /* f_args: f_arg ',' f_rest_arg opt_args_tail  */
#line 4678 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 605: /* f_args: f_arg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4682 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 606: /* f_args: f_arg opt_args_tail  */
#line 4686 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 11467 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 607: /* f_args: f_optarg ',' f_rest_arg opt_args_tail  */
#line 4690 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11475 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 608: /* f_args: f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4694 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11483 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 609: /* f_args: f_optarg opt_args_tail  */
#line 4698 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 11491 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 610: /* f_args: f_optarg ',' f_arg opt_args_tail  */
#line 4702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11499 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 611: /* f_args: f_rest_arg opt_args_tail  */
#line 4706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11507 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 612: /* f_args: f_rest_arg ',' f_arg opt_args_tail  */
#line 4710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11515 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 613: /* f_args: args_tail  */
#line 4714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 11523 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 614: /* f_args: %empty  */
#line 4718 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 11532 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 615: /* f_bad_arg: "constant"  */
#line 4725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 11541 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 616: /* f_bad_arg: "instance variable"  */
#line 4730 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 11550 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 617: /* f_bad_arg: "global variable"  */
#line 4735 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 11559 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 618: /* f_bad_arg: "class variable"  */
#line 4740 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 11568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 619: /* f_bad_arg: "numbered parameter"  */
#line 4745 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 11577 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 620: /* f_norm_arg: f_bad_arg  */
#line 4752 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 11585 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 621: /* f_norm_arg: "local variable or method"  */
#line 4756 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11594 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 622: /* f_arg_item: f_norm_arg  */
#line 4763 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 11602 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 623: /* @37: %empty  */
#line 4767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 11610 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 624: /* f_arg_item: tLPAREN @37 f_margs rparen  */
#line 4771 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_marg(p, (yyvsp[-1].nd));
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 11620 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 625: /* f_arg: f_arg_item  */
#line 4779 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11628 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 626: /* f_arg: f_arg ',' f_arg_item  */
#line 4783 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11636 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 627: /* f_opt_asgn: "local variable or method" '='  */
#line 4789 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 11646 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 628: /* f_opt: f_opt_asgn arg  */
#line 4797 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(sym_to_node((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 629: /* f_block_opt: f_opt_asgn primary_value  */
#line 4805 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(sym_to_node((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11666 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 630: /* f_block_optarg: f_block_opt  */
#line 4813 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11674 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 631: /* f_block_optarg: f_block_optarg ',' f_block_opt  */
#line 4817 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11682 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 632: /* f_optarg: f_opt  */
#line 4823 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11690 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 633: /* f_optarg: f_optarg ',' f_opt  */
#line 4827 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 636: /* f_rest_arg: restarg_mark "local variable or method"  */
#line 4837 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11707 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 637: /* f_rest_arg: restarg_mark  */
#line 4842 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(mul);
                      local_add_f(p, (yyval.id));
                    }
#line 11716 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 640: /* f_block_arg: blkarg_mark "local variable or method"  */
#line 4853 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11724 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 641: /* f_block_arg: blkarg_mark  */
#line 4857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(and);
                    }
#line 11732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 642: /* opt_f_block_arg: ',' f_block_arg  */
#line 4863 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11740 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 643: /* opt_f_block_arg: none  */
#line 4867 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 11748 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 644: /* singleton: var_ref  */
#line 4873 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 11758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 645: /* $@38: %empty  */
#line 4878 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 11764 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 646: /* singleton: '(' $@38 expr rparen  */
#line 4879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[-1].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 648: /* assoc_list: assocs trailer  */
#line 4887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11781 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 649: /* assocs: assoc  */
#line 4893 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11789 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 650: /* assocs: assocs comma assoc  */
#line 4897 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11797 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 651: /* assoc: arg "=>" arg  */
#line 4903 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 652: /* assoc: "local variable or method" "label" arg  */
#line 4909 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 11816 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 653: /* assoc: "local variable or method" "label"  */
#line 4914 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), label_reference(p, (yyvsp[-1].id)));
                    }
#line 11824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 654: /* assoc: "numbered parameter" "label"  */
#line 4918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[-1].num));
                      (yyval.nd) = cons(new_sym(p, sym), label_reference(p, sym));
                    }
#line 11833 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 655: /* assoc: "numbered parameter" "label" arg  */
#line 4923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, intern_numparam((yyvsp[-2].num))), (yyvsp[0].nd));
                    }
#line 11842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 656: /* assoc: string_fragment "label" arg  */
#line 4928 "mrbgems/mruby-compiler/core/parse.y"
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
#line 11862 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 657: /* assoc: "**" arg  */
#line 4944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 11871 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 658: /* assoc: "**"  */
#line 4949 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), new_lvar(p, intern_op(pow)));
                    }
#line 11879 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 671: /* call_op: '.'  */
#line 4975 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 11887 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 672: /* call_op: "&."  */
#line 4979 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 11895 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 674: /* call_op2: "::"  */
#line 4986 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 11903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 683: /* term: ';'  */
#line 5007 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 11909 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 685: /* nl: '\n'  */
#line 5012 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 11918 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 689: /* none: %empty  */
#line 5024 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 11926 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 11930 "mrbgems/mruby-compiler/core/y.tab.c"

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
  YY_SYMBOL_PRINT ("-> $$ =", YY_CAST (yysymbol_kind_t, yyr1[yyn]), &yyval, &yyloc);

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
        yysyntax_error_status = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
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
                  = yysyntax_error (&yymsg_alloc, &yymsg, &yyctx);
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
  YY_STACK_PRINT (yyss, yyssp);
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
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  yyerror_range[2] = yylloc;
  ++yylsp;
  YYLLOC_DEFAULT (*yylsp, yyerror_range, 2);

  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

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
  YY_STACK_PRINT (yyss, yyssp);
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

#line 5028 "mrbgems/mruby-compiler/core/parse.y"

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
    /* Unicode character (negative c indicates codepoint) */
    len = (int)mrb_utf8_to_buf(utf8, (uint32_t)(-c));
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
  p->no_return_value = cxt->no_return_value;
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
  if (node_type(tree) != NODE_LAST) {
    lineno = ((struct mrb_ast_var_header*)tree)->lineno;
  }

  dump_prefix(offset, lineno);

  /* All nodes are now variable-sized nodes with headers */
  nodetype = node_type(tree);

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
    printf("%s:\n", node_type(tree) == NODE_MASGN ? "NODE_MASGN" : "NODE_MARG");
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
