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


#line 2020 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 1963 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 2199 "mrbgems/mruby-compiler/core/y.tab.c"

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
  YYSYMBOL_defn_head = 164,                /* defn_head  */
  YYSYMBOL_165_4 = 165,                    /* $@4  */
  YYSYMBOL_defs_head = 166,                /* defs_head  */
  YYSYMBOL_expr_value = 167,               /* expr_value  */
  YYSYMBOL_command_call = 168,             /* command_call  */
  YYSYMBOL_block_command = 169,            /* block_command  */
  YYSYMBOL_170_5 = 170,                    /* $@5  */
  YYSYMBOL_cmd_brace_block = 171,          /* cmd_brace_block  */
  YYSYMBOL_command = 172,                  /* command  */
  YYSYMBOL_mlhs = 173,                     /* mlhs  */
  YYSYMBOL_mlhs_inner = 174,               /* mlhs_inner  */
  YYSYMBOL_mlhs_basic = 175,               /* mlhs_basic  */
  YYSYMBOL_mlhs_item = 176,                /* mlhs_item  */
  YYSYMBOL_mlhs_list = 177,                /* mlhs_list  */
  YYSYMBOL_mlhs_post = 178,                /* mlhs_post  */
  YYSYMBOL_mlhs_node = 179,                /* mlhs_node  */
  YYSYMBOL_lhs = 180,                      /* lhs  */
  YYSYMBOL_cname = 181,                    /* cname  */
  YYSYMBOL_cpath = 182,                    /* cpath  */
  YYSYMBOL_fname = 183,                    /* fname  */
  YYSYMBOL_fsym = 184,                     /* fsym  */
  YYSYMBOL_undef_list = 185,               /* undef_list  */
  YYSYMBOL_186_6 = 186,                    /* $@6  */
  YYSYMBOL_op = 187,                       /* op  */
  YYSYMBOL_reswords = 188,                 /* reswords  */
  YYSYMBOL_arg = 189,                      /* arg  */
  YYSYMBOL_aref_args = 190,                /* aref_args  */
  YYSYMBOL_arg_rhs = 191,                  /* arg_rhs  */
  YYSYMBOL_paren_args = 192,               /* paren_args  */
  YYSYMBOL_opt_paren_args = 193,           /* opt_paren_args  */
  YYSYMBOL_opt_call_args = 194,            /* opt_call_args  */
  YYSYMBOL_call_args = 195,                /* call_args  */
  YYSYMBOL_196_7 = 196,                    /* @7  */
  YYSYMBOL_command_args = 197,             /* command_args  */
  YYSYMBOL_block_arg = 198,                /* block_arg  */
  YYSYMBOL_opt_block_arg = 199,            /* opt_block_arg  */
  YYSYMBOL_comma = 200,                    /* comma  */
  YYSYMBOL_args = 201,                     /* args  */
  YYSYMBOL_mrhs = 202,                     /* mrhs  */
  YYSYMBOL_primary = 203,                  /* primary  */
  YYSYMBOL_204_8 = 204,                    /* @8  */
  YYSYMBOL_205_9 = 205,                    /* @9  */
  YYSYMBOL_206_10 = 206,                   /* $@10  */
  YYSYMBOL_207_11 = 207,                   /* $@11  */
  YYSYMBOL_208_12 = 208,                   /* @12  */
  YYSYMBOL_209_13 = 209,                   /* @13  */
  YYSYMBOL_210_14 = 210,                   /* $@14  */
  YYSYMBOL_211_15 = 211,                   /* $@15  */
  YYSYMBOL_212_16 = 212,                   /* $@16  */
  YYSYMBOL_213_17 = 213,                   /* $@17  */
  YYSYMBOL_214_18 = 214,                   /* $@18  */
  YYSYMBOL_215_19 = 215,                   /* $@19  */
  YYSYMBOL_216_20 = 216,                   /* @20  */
  YYSYMBOL_217_21 = 217,                   /* @21  */
  YYSYMBOL_218_22 = 218,                   /* @22  */
  YYSYMBOL_219_23 = 219,                   /* @23  */
  YYSYMBOL_primary_value = 220,            /* primary_value  */
  YYSYMBOL_then = 221,                     /* then  */
  YYSYMBOL_do = 222,                       /* do  */
  YYSYMBOL_if_tail = 223,                  /* if_tail  */
  YYSYMBOL_opt_else = 224,                 /* opt_else  */
  YYSYMBOL_for_var = 225,                  /* for_var  */
  YYSYMBOL_f_margs = 226,                  /* f_margs  */
  YYSYMBOL_227_24 = 227,                   /* $@24  */
  YYSYMBOL_block_args_tail = 228,          /* block_args_tail  */
  YYSYMBOL_opt_block_args_tail = 229,      /* opt_block_args_tail  */
  YYSYMBOL_block_param = 230,              /* block_param  */
  YYSYMBOL_opt_block_param = 231,          /* opt_block_param  */
  YYSYMBOL_232_25 = 232,                   /* $@25  */
  YYSYMBOL_block_param_def = 233,          /* block_param_def  */
  YYSYMBOL_opt_bv_decl = 234,              /* opt_bv_decl  */
  YYSYMBOL_bv_decls = 235,                 /* bv_decls  */
  YYSYMBOL_bvar = 236,                     /* bvar  */
  YYSYMBOL_f_larglist = 237,               /* f_larglist  */
  YYSYMBOL_lambda_body = 238,              /* lambda_body  */
  YYSYMBOL_239_26 = 239,                   /* @26  */
  YYSYMBOL_do_block = 240,                 /* do_block  */
  YYSYMBOL_block_call = 241,               /* block_call  */
  YYSYMBOL_method_call = 242,              /* method_call  */
  YYSYMBOL_243_27 = 243,                   /* @27  */
  YYSYMBOL_brace_block = 244,              /* brace_block  */
  YYSYMBOL_245_28 = 245,                   /* @28  */
  YYSYMBOL_case_body = 246,                /* case_body  */
  YYSYMBOL_cases = 247,                    /* cases  */
  YYSYMBOL_in_clauses = 248,               /* in_clauses  */
  YYSYMBOL_p_expr = 249,                   /* p_expr  */
  YYSYMBOL_p_as = 250,                     /* p_as  */
  YYSYMBOL_p_alt = 251,                    /* p_alt  */
  YYSYMBOL_p_value = 252,                  /* p_value  */
  YYSYMBOL_p_array = 253,                  /* p_array  */
  YYSYMBOL_p_array_body = 254,             /* p_array_body  */
  YYSYMBOL_p_array_elems = 255,            /* p_array_elems  */
  YYSYMBOL_p_rest = 256,                   /* p_rest  */
  YYSYMBOL_p_var = 257,                    /* p_var  */
  YYSYMBOL_opt_rescue = 258,               /* opt_rescue  */
  YYSYMBOL_exc_list = 259,                 /* exc_list  */
  YYSYMBOL_exc_var = 260,                  /* exc_var  */
  YYSYMBOL_opt_ensure = 261,               /* opt_ensure  */
  YYSYMBOL_literal = 262,                  /* literal  */
  YYSYMBOL_string = 263,                   /* string  */
  YYSYMBOL_string_fragment = 264,          /* string_fragment  */
  YYSYMBOL_string_rep = 265,               /* string_rep  */
  YYSYMBOL_string_interp = 266,            /* string_interp  */
  YYSYMBOL_267_29 = 267,                   /* @29  */
  YYSYMBOL_xstring = 268,                  /* xstring  */
  YYSYMBOL_regexp = 269,                   /* regexp  */
  YYSYMBOL_heredoc = 270,                  /* heredoc  */
  YYSYMBOL_heredoc_bodies = 271,           /* heredoc_bodies  */
  YYSYMBOL_heredoc_body = 272,             /* heredoc_body  */
  YYSYMBOL_heredoc_string_rep = 273,       /* heredoc_string_rep  */
  YYSYMBOL_heredoc_string_interp = 274,    /* heredoc_string_interp  */
  YYSYMBOL_275_30 = 275,                   /* @30  */
  YYSYMBOL_words = 276,                    /* words  */
  YYSYMBOL_symbol = 277,                   /* symbol  */
  YYSYMBOL_basic_symbol = 278,             /* basic_symbol  */
  YYSYMBOL_sym = 279,                      /* sym  */
  YYSYMBOL_symbols = 280,                  /* symbols  */
  YYSYMBOL_numeric = 281,                  /* numeric  */
  YYSYMBOL_variable = 282,                 /* variable  */
  YYSYMBOL_var_lhs = 283,                  /* var_lhs  */
  YYSYMBOL_var_ref = 284,                  /* var_ref  */
  YYSYMBOL_backref = 285,                  /* backref  */
  YYSYMBOL_superclass = 286,               /* superclass  */
  YYSYMBOL_287_31 = 287,                   /* $@31  */
  YYSYMBOL_f_opt_arglist_paren = 288,      /* f_opt_arglist_paren  */
  YYSYMBOL_f_arglist_paren = 289,          /* f_arglist_paren  */
  YYSYMBOL_f_arglist = 290,                /* f_arglist  */
  YYSYMBOL_f_label = 291,                  /* f_label  */
  YYSYMBOL_f_kw = 292,                     /* f_kw  */
  YYSYMBOL_f_block_kw = 293,               /* f_block_kw  */
  YYSYMBOL_f_block_kwarg = 294,            /* f_block_kwarg  */
  YYSYMBOL_f_kwarg = 295,                  /* f_kwarg  */
  YYSYMBOL_kwrest_mark = 296,              /* kwrest_mark  */
  YYSYMBOL_f_kwrest = 297,                 /* f_kwrest  */
  YYSYMBOL_args_tail = 298,                /* args_tail  */
  YYSYMBOL_opt_args_tail = 299,            /* opt_args_tail  */
  YYSYMBOL_f_args = 300,                   /* f_args  */
  YYSYMBOL_f_bad_arg = 301,                /* f_bad_arg  */
  YYSYMBOL_f_norm_arg = 302,               /* f_norm_arg  */
  YYSYMBOL_f_arg_item = 303,               /* f_arg_item  */
  YYSYMBOL_304_32 = 304,                   /* @32  */
  YYSYMBOL_f_arg = 305,                    /* f_arg  */
  YYSYMBOL_f_opt_asgn = 306,               /* f_opt_asgn  */
  YYSYMBOL_f_opt = 307,                    /* f_opt  */
  YYSYMBOL_f_block_opt = 308,              /* f_block_opt  */
  YYSYMBOL_f_block_optarg = 309,           /* f_block_optarg  */
  YYSYMBOL_f_optarg = 310,                 /* f_optarg  */
  YYSYMBOL_restarg_mark = 311,             /* restarg_mark  */
  YYSYMBOL_f_rest_arg = 312,               /* f_rest_arg  */
  YYSYMBOL_blkarg_mark = 313,              /* blkarg_mark  */
  YYSYMBOL_f_block_arg = 314,              /* f_block_arg  */
  YYSYMBOL_opt_f_block_arg = 315,          /* opt_f_block_arg  */
  YYSYMBOL_singleton = 316,                /* singleton  */
  YYSYMBOL_317_33 = 317,                   /* $@33  */
  YYSYMBOL_assoc_list = 318,               /* assoc_list  */
  YYSYMBOL_assocs = 319,                   /* assocs  */
  YYSYMBOL_assoc = 320,                    /* assoc  */
  YYSYMBOL_operation = 321,                /* operation  */
  YYSYMBOL_operation2 = 322,               /* operation2  */
  YYSYMBOL_operation3 = 323,               /* operation3  */
  YYSYMBOL_dot_or_colon = 324,             /* dot_or_colon  */
  YYSYMBOL_call_op = 325,                  /* call_op  */
  YYSYMBOL_call_op2 = 326,                 /* call_op2  */
  YYSYMBOL_opt_terms = 327,                /* opt_terms  */
  YYSYMBOL_opt_nl = 328,                   /* opt_nl  */
  YYSYMBOL_rparen = 329,                   /* rparen  */
  YYSYMBOL_trailer = 330,                  /* trailer  */
  YYSYMBOL_term = 331,                     /* term  */
  YYSYMBOL_nl = 332,                       /* nl  */
  YYSYMBOL_terms = 333,                    /* terms  */
  YYSYMBOL_none = 334                      /* none  */
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
#define YYLAST   15578

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  186
/* YYNRULES -- Number of rules.  */
#define YYNRULES  655
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1166

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
       0,  2137,  2137,  2137,  2147,  2153,  2157,  2161,  2165,  2171,
    2173,  2172,  2186,  2212,  2218,  2222,  2226,  2230,  2236,  2236,
    2240,  2244,  2248,  2252,  2261,  2270,  2274,  2279,  2280,  2284,
    2288,  2292,  2296,  2302,  2305,  2309,  2313,  2317,  2321,  2325,
    2330,  2334,  2343,  2352,  2361,  2370,  2377,  2378,  2382,  2385,
    2386,  2390,  2394,  2398,  2402,  2405,  2415,  2414,  2429,  2438,
    2439,  2442,  2443,  2450,  2449,  2464,  2468,  2473,  2477,  2482,
    2486,  2491,  2495,  2499,  2503,  2507,  2513,  2517,  2523,  2524,
    2530,  2534,  2538,  2542,  2546,  2550,  2554,  2558,  2562,  2566,
    2572,  2573,  2579,  2583,  2589,  2593,  2599,  2603,  2607,  2611,
    2615,  2619,  2625,  2631,  2638,  2642,  2646,  2650,  2654,  2658,
    2664,  2670,  2675,  2681,  2685,  2688,  2692,  2696,  2703,  2704,
    2705,  2706,  2711,  2718,  2719,  2722,  2726,  2726,  2732,  2733,
    2734,  2735,  2736,  2737,  2738,  2739,  2740,  2741,  2742,  2743,
    2744,  2745,  2746,  2747,  2748,  2749,  2750,  2751,  2752,  2753,
    2754,  2755,  2756,  2757,  2758,  2759,  2760,  2761,  2764,  2764,
    2764,  2765,  2765,  2766,  2766,  2766,  2767,  2767,  2767,  2767,
    2768,  2768,  2768,  2769,  2769,  2769,  2770,  2770,  2770,  2770,
    2771,  2771,  2771,  2771,  2772,  2772,  2772,  2772,  2773,  2773,
    2773,  2773,  2774,  2774,  2774,  2774,  2775,  2775,  2778,  2782,
    2786,  2790,  2794,  2798,  2802,  2807,  2812,  2817,  2821,  2825,
    2829,  2833,  2837,  2841,  2845,  2849,  2853,  2857,  2861,  2865,
    2869,  2873,  2877,  2881,  2885,  2889,  2893,  2897,  2901,  2905,
    2909,  2913,  2917,  2921,  2925,  2929,  2933,  2937,  2941,  2945,
    2949,  2953,  2957,  2961,  2965,  2974,  2983,  2992,  3001,  3007,
    3008,  3012,  3016,  3022,  3026,  3033,  3037,  3046,  3063,  3064,
    3067,  3068,  3069,  3073,  3077,  3083,  3088,  3092,  3096,  3100,
    3106,  3106,  3117,  3121,  3127,  3131,  3137,  3140,  3145,  3149,
    3153,  3158,  3162,  3168,  3173,  3177,  3183,  3184,  3188,  3192,
    3193,  3194,  3195,  3196,  3201,  3200,  3212,  3216,  3211,  3221,
    3221,  3225,  3229,  3233,  3237,  3241,  3245,  3249,  3253,  3257,
    3261,  3265,  3266,  3272,  3279,  3271,  3292,  3300,  3308,  3308,
    3308,  3315,  3315,  3315,  3322,  3328,  3332,  3341,  3350,  3360,
    3362,  3359,  3371,  3369,  3387,  3392,  3385,  3409,  3407,  3423,
    3433,  3444,  3448,  3452,  3456,  3462,  3469,  3470,  3471,  3474,
    3475,  3478,  3479,  3487,  3488,  3494,  3498,  3501,  3505,  3509,
    3513,  3518,  3522,  3526,  3530,  3536,  3535,  3545,  3549,  3553,
    3557,  3563,  3568,  3573,  3577,  3581,  3585,  3589,  3593,  3597,
    3601,  3605,  3609,  3613,  3617,  3621,  3625,  3629,  3635,  3640,
    3647,  3647,  3651,  3656,  3662,  3666,  3672,  3673,  3676,  3681,
    3684,  3688,  3694,  3698,  3705,  3704,  3721,  3726,  3730,  3735,
    3742,  3746,  3750,  3754,  3758,  3762,  3766,  3770,  3774,  3781,
    3780,  3795,  3794,  3810,  3818,  3827,  3831,  3835,  3840,  3845,
    3853,  3856,  3857,  3863,  3864,  3870,  3871,  3875,  3879,  3883,
    3887,  3891,  3895,  3899,  3903,  3907,  3911,  3915,  3922,  3927,
    3932,  3937,  3942,  3950,  3954,  3961,  3965,  3972,  3978,  3985,
    3988,  3992,  3993,  3996,  4000,  4003,  4007,  4010,  4011,  4012,
    4013,  4016,  4017,  4023,  4028,  4033,  4038,  4044,  4045,  4051,
    4057,  4056,  4068,  4072,  4078,  4082,  4088,  4097,  4108,  4111,
    4112,  4115,  4121,  4127,  4128,  4131,  4138,  4137,  4152,  4156,
    4164,  4168,  4180,  4187,  4194,  4195,  4196,  4197,  4198,  4202,
    4208,  4212,  4220,  4221,  4222,  4226,  4232,  4236,  4240,  4244,
    4248,  4254,  4258,  4264,  4268,  4272,  4276,  4280,  4284,  4288,
    4296,  4303,  4309,  4310,  4314,  4318,  4317,  4334,  4335,  4338,
    4344,  4348,  4354,  4355,  4359,  4363,  4369,  4373,  4379,  4385,
    4392,  4398,  4405,  4409,  4415,  4419,  4425,  4426,  4429,  4433,
    4439,  4443,  4447,  4451,  4457,  4462,  4467,  4471,  4475,  4479,
    4483,  4487,  4491,  4495,  4499,  4503,  4507,  4511,  4515,  4519,
    4524,  4530,  4535,  4540,  4545,  4550,  4557,  4561,  4568,  4573,
    4572,  4584,  4588,  4594,  4602,  4610,  4618,  4622,  4628,  4632,
    4638,  4639,  4642,  4647,  4654,  4655,  4658,  4662,  4668,  4672,
    4678,  4684,  4684,  4691,  4692,  4698,  4702,  4708,  4714,  4719,
    4723,  4728,  4733,  4749,  4754,  4760,  4761,  4762,  4765,  4766,
    4767,  4768,  4771,  4772,  4773,  4776,  4777,  4780,  4784,  4790,
    4791,  4797,  4798,  4801,  4802,  4805,  4808,  4809,  4810,  4813,
    4814,  4817,  4822,  4825,  4826,  4830
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
  "stmts", "$@3", "stmt", "command_asgn", "command_rhs", "expr",
  "defn_head", "$@4", "defs_head", "expr_value", "command_call",
  "block_command", "$@5", "cmd_brace_block", "command", "mlhs",
  "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list", "mlhs_post",
  "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym", "undef_list",
  "$@6", "op", "reswords", "arg", "aref_args", "arg_rhs", "paren_args",
  "opt_paren_args", "opt_call_args", "call_args", "@7", "command_args",
  "block_arg", "opt_block_arg", "comma", "args", "mrhs", "primary", "@8",
  "@9", "$@10", "$@11", "@12", "@13", "$@14", "$@15", "$@16", "$@17",
  "$@18", "$@19", "@20", "@21", "@22", "@23", "primary_value", "then",
  "do", "if_tail", "opt_else", "for_var", "f_margs", "$@24",
  "block_args_tail", "opt_block_args_tail", "block_param",
  "opt_block_param", "$@25", "block_param_def", "opt_bv_decl", "bv_decls",
  "bvar", "f_larglist", "lambda_body", "@26", "do_block", "block_call",
  "method_call", "@27", "brace_block", "@28", "case_body", "cases",
  "in_clauses", "p_expr", "p_as", "p_alt", "p_value", "p_array",
  "p_array_body", "p_array_elems", "p_rest", "p_var", "opt_rescue",
  "exc_list", "exc_var", "opt_ensure", "literal", "string",
  "string_fragment", "string_rep", "string_interp", "@29", "xstring",
  "regexp", "heredoc", "heredoc_bodies", "heredoc_body",
  "heredoc_string_rep", "heredoc_string_interp", "@30", "words", "symbol",
  "basic_symbol", "sym", "symbols", "numeric", "variable", "var_lhs",
  "var_ref", "backref", "superclass", "$@31", "f_opt_arglist_paren",
  "f_arglist_paren", "f_arglist", "f_label", "f_kw", "f_block_kw",
  "f_block_kwarg", "f_kwarg", "kwrest_mark", "f_kwrest", "args_tail",
  "opt_args_tail", "f_args", "f_bad_arg", "f_norm_arg", "f_arg_item",
  "@32", "f_arg", "f_opt_asgn", "f_opt", "f_block_opt", "f_block_optarg",
  "f_optarg", "restarg_mark", "f_rest_arg", "blkarg_mark", "f_block_arg",
  "opt_f_block_arg", "singleton", "$@33", "assoc_list", "assocs", "assoc",
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

#define YYPACT_NINF (-948)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-656)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -948,  4572,   126,  9723, 12338, 12794,  8024,  -948, 11493, 11493,
    -948,  -948, 12452,  9206,  7640,  9959,  9959,  -948,  -948,  9959,
    5093,  4685,  -948,  -948,  -948,  -948,    28,  9206,  -948,   -79,
    -948,  -948,  -948,  8166,  4218,  -948,  -948,  8308,  -948,  -948,
    -948,  -948,  -948,  -948,  -948,   199, 11611, 11611, 11611, 11611,
      74,  6753,  1310, 10431, 10785,  3167,  -948,  8924,  1285,  1191,
     535,  1303,  1329,  -948,   620, 11729, 11611,  -948,  1325,  -948,
    1146,  -948,   655,  1381,  1381,  -948,  -948,   156,    99,  -948,
      95, 12566,  -948,   161, 15259,   167,   203,    83,    70,  -948,
     259,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,
     246,   115,  -948,   376,    42,  -948,  -948,  -948,  -948,  -948,
    -948,   144,   144,    28,   101,   569,  -948, 11493,   270,  6872,
     660,  1441,  1441,  -948,   174,  -948,   287,  -948,  -948,    42,
    -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,
    -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,
    -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,    48,    88,
     127,   150,  -948,  -948,  -948,  -948,  -948,  -948,   153,   157,
     176,   228,  -948,   247,  -948,  -948,  -948,  -948,  -948,  -948,
    -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,
    -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,
    -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,   251,
    5785,   257,   655,  1381,  1381,   908,   221, 15383,   311,   276,
     263,   294,   908, 11493, 11493,   385,   328,  -948,  -948,   405,
     324,    32,    68,  -948,  -948,  -948,  -948,  -948,  -948,  -948,
    -948,  -948,  9065,  -948,  -948,   254,  -948,  -948,  -948,  -948,
    -948,  -948,  1325,  -948,   789,  -948,   382,  -948,  -948,  1325,
    4821,   112, 11611, 11611, 11611, 11611,  -948, 15321,  -948,  -948,
     269,   348,   269,  -948,  -948,  -948, 10077,  -948,  -948,  9959,
    -948,  -948,  -948,  -948,  7640,  7878,  -948,   282,  6991,  -948,
     434,   321, 15445, 15445,   399,  9841,  6753,   283,  1325,  1146,
    1325,   322,  -948,  9841,  1325,   313,  1213,  1213,  -948, 15321,
     325,  1213,  -948,   427, 12908,   350,   674,   679,   793,  1884,
    -948,  -948,  -948,  -948,  -948,  1384,  -948,  -948,  -948,  -948,
    -948,  -948,   823,  1389,  -948,  -948,  1217,  -948,   619,  -948,
    1397,  -948,  1473,   395,   402,  -948,  -948,  -948,  -948,  7256,
   11493, 11493, 11493, 11493,  9841, 11493, 11493,    71,  -948,  -948,
    -948,  -948,   450,  1325,  -948,  -948,  -948,  -948,  -948,  -948,
    -948,  2072,   388,   414,  5785, 11611,  -948,   389,   492,   408,
    -948,  1325,  -948,  -948,  -948,   412, 11611,  -948,   438,   505,
     442,   541,  -948,  -948,   465,  5785,  -948,  -948, 10903,  -948,
    6753,  9488,   452, 10903, 11611, 11611, 11611, 11611, 11611, 11611,
   11611, 11611, 11611, 11611, 11611, 11611, 11611, 11611,   552, 11611,
   11611, 11611, 11611, 11611, 11611, 11611, 11611, 11611, 11611, 11611,
   11611,  2565,  -948,  9959,  -948,  4416,  -948,  -948, 14749,  -948,
    -948,  -948,  -948, 11729, 11729,  -948,   519,  -948,   655,  -948,
     860,  -948,  -948,  -948,  -948,  -948,  -948, 13459,  9959, 13545,
    5785, 11493,  -948,  -948,  -948,   606,   612,   352,   508,   525,
    -948,  5931,   632, 11611, 13631,  9959, 13717, 11611, 11611,  6369,
      81,    81,    84, 13803,  9959, 13889,  -948,   590,  -948,  6991,
     281,  -948,  -948, 11021,   658,  -948, 11611, 11611, 15383, 15383,
   15383, 11611,  -948,  -948, 10195,  -948, 11611,  -948, 10549,  7759,
     517,  1325,   269,   269,  -948,  -948,  1020,   543,  -948,  -948,
    -948,  9206,  6488,   558, 13631, 13717, 11611,  1146,  1325,  -948,
    -948,  7375,   559,  1146,  -948,  -948, 10667,  -948,  1325, 10785,
    -948,  -948,  -948,   860,    95, 12908,  -948, 12908, 13975,  9959,
   14061,  1642,  -948,  -948,   563,  -948,  1529,  6991,   823,  -948,
    -948,  -948,  -948,  -948,  -948,  -948, 11611, 11611,  -948,  -948,
    -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,
    1769,  1325,  1325,   580, 11729,   696, 15383,   979,  -948,  -948,
    -948,    53,  -948,  -948,  2357,  -948, 15383,  1642,  -948,  -948,
    1476,  -948,  -948, 11729,   718,    33, 11611,  -948, 14975,   269,
    -948,  1325, 12908,   599,  -948,  -948,  -948,   700,   625,  1543,
    -948,  -948,   884,   430,  2647,  2647,  2647,  2647,  1843,  1843,
    4075,  3712,  2647,  2647, 15445, 15445,  1048,  1048,  -948,   321,
   15383,  1843,  1843,  1860,  1860,  1674,   384,   384,   321,   321,
     321,  5229,  8664,  5501,  8782,  -948,   144,  -948,   608,   269,
     467,  -948,   646,  -948,  -948,  4957,  -948,  -948,  1771,    33,
      33,  -948,  2175,  -948,  -948,  -948,  -948,  -948,  1325, 11493,
    5785,   727,   500,  -948,   144,   615,   144,   743,  1020,  9347,
    -948, 11139,   760,  -948, 11611, 11611,   707,  -948,  8426,  8545,
     639,   481,   485,   760,  -948,  -948,  -948,  -948,   100,   114,
     641,   111,   119, 11493,  9206,   651, 13022,   770, 15383,   198,
    -948, 15383, 15383, 15383,    46, 11611, 15321,  -948,   269, 15383,
    -948,  -948,  -948,  -948, 10313, 10549,  -948,  -948,  -948,   654,
    -948,  -948,   194,  1146,  1325,  1213,   452,  -948,   727,   500,
     657,  1029,  1159,  -948,   105,  1642,  -948,   661,  -948,   321,
     321,  -948,  -948,   706,  1325,   659,  -948,  -948,  2368,   763,
    2975,  -948,   768,   450,  -948,   408,  -948,  1325,  -948,  -948,
     685,   687,   692,  -948,   693,   768,   692,   792,  3438,  -948,
    -948,  1642,  5785,  -948,  -948, 15046, 11257,  -948,  -948, 12908,
    9841, 11729, 11611, 14147,  9959, 14233,   839, 11729, 11729,  -948,
     519,   647, 10195, 11729, 11729,  -948,   519,    70,   156,  5785,
    6991,    33,  -948,  1325,   830,  -948,  -948,  -948,  -948, 14975,
    -948,   754,  -948,  6634,   837,  -948, 11493,   838,  -948, 11611,
   11611,   490, 11611, 11611,   841,  7137,  7137,   123,    81,  -948,
    -948,  3606, 11952, 12032,   101, 11778, 11872,   797,  9602,   961,
     731,  -948,    59,  -948,  -948,  -948, 12112, 12192,  -948, 11375,
    6077, 15383,  -948,  7759,   269,  -948,  -948,  -948,   309,   715,
    1488,  5785,  6991,  -948,  -948,  -948,   719,  -948,  1788,  1325,
   11611, 11611,  -948,  -948,  1642,  -948,  1476,  -948,  1476,  -948,
    1476,  -948,  -948, 11611, 11611,  -948,  -948,  -948, 13136,  -948,
     724,   408,   726, 13136,  -948,   728,   733,  -948,   861, 11611,
   15117,  -948,  -948, 15383,  5365,  5637,   739,   545,   565, 11611,
   11611,  -948,  -948,  -948,  -948,  -948, 11729,  -948,  -948,  -948,
    -948,  -948,  -948,  -948,   866,   745,  6991,  5785,  -948,  -948,
   13250,   908,  -948,  -948,  7137,  -948,  -948,   908,  -948, 11611,
    -948,   867,   880,  -948,   860, 13298,   847,  7494,  -948,  1051,
    -948,   762,   769,   771, 14319, 11493, 11493,  6223,   864, 13022,
   15383,   234,  -948, 10549,  -948,  1042,   897,   778,  1653,  1653,
     755,  -948, 15383, 15383,   692,   784,   692,   692, 15383, 15383,
     794,   801,   896,  1060,   979,  -948,  -948,  2017,  -948,  1060,
    1642,  -948,  1476,  -948,  -948, 15188,   579, 15383, 15383,  -948,
    -948,  -948,  -948,   816,   944,   906,  -948,  1070,   679,   793,
    5785,  -948,  5931,  -948,  -948,  7137, 14898, 13375, 14405,  -948,
   12680, 13022,   820,   500,   908,   908,   152,  -948,  -948,  -948,
    -948,  -948,  -948,   824,  -948,  -948,  -948,  -948,   827,   827,
    1653,   833,  -948,  1476,  -948,  -948,  -948,  -948,  -948,  -948,
   14491,  -948,   408,   979,  -948,  -948,   840,   844,   848,  -948,
     850,   848,  -948,  -948,   860, 14577,  9959, 14663,   612,   707,
     957, 14821,   928,  -948,   853,   856,  6223,  6223, 13022,  -948,
     966,  1042,    46,  1653,   827,  1653,   692,   857,  -948,  1642,
    -948,  1476,  -948,  1476,  -948,  1476,  -948,  -948,   727,   500,
     865,   931,   954,  -948,  -948,  -948,  -948, 13022, 13022,   152,
     152,   994,  -948,  -948,   827,  -948,   848,   863,   848,   848,
     309,   856,   996,   997, 11493, 11493,  6223,  -948,  1476,  -948,
    -948,  -948,  -948,  -948,   908,   908,   152,   848,  6223,  6223,
    -948,  -948,   152,   152,  -948,  -948
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     0,     0,     0,     0,   294,     0,     0,
     318,   321,     0,     0,   641,   341,   342,   343,   344,   306,
     270,   270,   526,   525,   527,   528,   643,     0,    10,     0,
     530,   529,   531,   516,   627,   518,   517,   520,   519,   512,
     513,   473,   474,   532,   533,   524,     0,     0,     0,     0,
       0,     0,   296,   655,   655,    88,   313,     0,     0,     0,
       0,     0,     0,   488,     0,     0,     0,     3,   641,     6,
       9,    27,    33,   580,   580,    49,    60,    59,     0,    76,
       0,    80,    90,     0,    54,   248,     0,    61,   311,   286,
     287,   471,   288,   289,   290,   469,   468,   500,   470,   467,
     523,     0,   291,   292,   270,     5,     1,     8,   341,   342,
     306,   655,   417,     0,   113,   114,   524,     0,     0,     0,
       0,   580,   580,   116,   534,   345,     0,   523,   292,     0,
     337,   168,   178,   169,   165,   194,   195,   196,   197,   176,
     191,   184,   174,   173,   189,   172,   171,   167,   192,   166,
     179,   183,   185,   177,   170,   186,   193,   188,   187,   180,
     190,   175,   164,   182,   181,   163,   161,   162,   158,   159,
     160,   118,   120,   119,   153,   154,   131,   132,   133,   140,
     137,   139,   134,   135,   155,   156,   141,   142,   146,   149,
     150,   136,   138,   128,   129,   130,   143,   144,   145,   147,
     148,   151,   152,   157,   611,    55,   121,   122,   610,     0,
       0,     0,    58,   580,   580,     0,     0,    54,     0,   523,
       0,   292,     0,     0,     0,   112,     0,   356,   355,     0,
       0,   523,   292,   187,   180,   190,   175,   158,   159,   160,
     118,   119,     0,   123,   125,    20,   124,   491,   496,   495,
     649,   651,   641,   652,     0,   493,     0,   653,   650,   642,
     625,   524,   278,   624,   273,     0,   265,   277,    74,   269,
     655,   471,   655,   615,    75,    73,   655,   259,   307,     0,
      72,   258,   416,    71,   641,     0,    18,     0,     0,   221,
       0,   222,   209,   212,   303,     0,     0,     0,   641,    15,
     641,    78,    14,     0,   641,     0,   646,   646,   249,     0,
       0,   646,   613,     0,     0,    86,     0,    96,   103,   580,
     506,   505,   507,   508,   502,     0,   504,   503,   475,   480,
     479,   482,     0,     0,   477,   484,     0,   486,     0,   498,
       0,   510,     0,   514,   515,    53,   236,   237,     4,   642,
       0,     0,     0,     0,     0,     0,     0,   587,   583,   582,
     581,   584,   585,     0,   589,   601,   556,   557,   605,   604,
     600,   580,     0,   542,     0,   549,   554,   655,   559,   655,
     579,     0,   586,   588,   591,   565,     0,   598,   565,   603,
     565,   607,   563,   538,     0,     0,   404,   406,     0,    92,
       0,    84,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   208,   211,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   638,   655,   637,     0,   640,   639,     0,   421,
     419,   312,   472,     0,     0,   410,    65,   310,   334,   113,
     114,   115,   514,   515,   542,   535,   332,     0,   655,     0,
       0,     0,   636,   635,    56,     0,   655,   303,     0,     0,
     347,     0,   346,     0,     0,   655,     0,     0,     0,     0,
       0,     0,   303,     0,   655,     0,   329,     0,   126,     0,
       0,   492,   494,     0,     0,   654,   619,   620,   279,   623,
     272,     0,   643,   266,     0,   275,     0,   267,     0,   641,
       0,   641,   655,   655,   260,   271,   641,     0,   309,    52,
     644,     0,     0,     0,     0,     0,     0,    17,   641,   301,
      13,   642,    77,   297,   300,   304,   648,   250,   647,   648,
     252,   305,   614,   102,    94,     0,    89,     0,     0,   655,
       0,   580,   314,   401,   565,   509,     0,     0,   483,   489,
     476,   478,   485,   487,   499,   511,     0,     0,     7,    21,
      22,    23,    24,    25,    50,    51,   546,   593,   547,   545,
       0,   641,   641,   565,     0,     0,   548,     0,   561,   609,
     558,     0,   562,   543,     0,   572,   594,     0,   575,   602,
       0,   577,   606,     0,     0,   655,   278,    28,    30,     0,
      31,   641,     0,    82,    93,    48,    34,    46,     0,   253,
     198,    29,     0,   292,   226,   231,   232,   233,   228,   230,
     240,   241,   234,   235,   207,   210,   238,   239,    32,   218,
     643,   227,   229,   223,   224,   225,   213,   214,   215,   216,
     217,   628,   633,   629,   634,   415,   270,   413,     0,   655,
     628,   630,   629,   631,   414,   270,   628,   629,   270,   655,
     655,    35,   253,   199,    45,   206,    63,    66,     0,     0,
       0,   113,   114,   117,     0,     0,   655,     0,   641,     0,
     295,   655,   655,   459,     0,     0,   655,   348,   632,   302,
       0,   628,   629,   655,   350,   319,   349,   322,   632,   302,
       0,   628,   629,     0,     0,     0,     0,     0,   277,     0,
     325,   618,   621,   617,   276,   281,   280,   274,   655,   622,
     616,   257,   255,   261,   262,   264,   308,   645,    19,     0,
      26,   205,    79,    16,   641,   646,    95,    87,    99,   101,
       0,    98,   100,   643,     0,     0,   501,     0,   490,   219,
     220,   587,   585,   364,   641,   357,   541,   539,     0,    41,
     244,   339,     0,     0,   555,   655,   608,     0,   564,   592,
     565,   565,   565,   599,   565,   587,   565,    43,   246,   340,
     392,   390,     0,   389,   388,   285,     0,    91,    85,     0,
       0,     0,     0,     0,   655,     0,     0,     0,     0,   412,
      69,   418,   262,     0,     0,   411,    67,   407,    62,     0,
       0,   655,   335,     0,     0,   418,   338,   612,    57,   460,
     461,   655,   462,     0,   655,   353,     0,     0,   351,     0,
       0,   418,     0,     0,     0,     0,     0,   418,     0,   127,
     497,     0,     0,     0,   457,     0,     0,     0,     0,     0,
       0,   430,   431,   433,   445,   435,     0,     0,   324,     0,
       0,   282,   268,   641,   655,    11,   298,   251,    97,     0,
     394,     0,     0,   315,   481,   365,   362,   590,     0,   641,
       0,     0,   560,   544,     0,   568,     0,   570,     0,   576,
       0,   573,   578,     0,     0,   387,   643,   643,   551,   552,
     655,   655,   372,     0,   596,   372,   372,   370,     0,   281,
     283,    83,    47,   254,   628,   629,     0,   628,   629,     0,
       0,    40,   203,    39,   204,    70,     0,    37,   201,    38,
     202,    68,   408,   409,     0,     0,     0,     0,   536,   333,
       0,     0,   464,   354,     0,    12,   466,     0,   316,     0,
     317,     0,     0,   330,   444,     0,     0,     0,   447,     0,
     453,     0,   448,   451,     0,     0,     0,     0,     0,     0,
     280,   655,   256,   263,   400,     0,     0,     0,     0,     0,
     360,   540,    42,   245,   565,   565,   565,   565,    44,   247,
       0,     0,     0,   550,     0,   368,   369,   372,   380,   595,
       0,   383,     0,   385,   405,   284,   418,   243,   242,    36,
     200,   422,   420,     0,     0,     0,   463,     0,   104,   111,
       0,   465,     0,   320,   323,     0,     0,     0,     0,   446,
       0,     0,   632,   443,     0,     0,   655,   432,   434,   424,
     425,   423,   398,   643,   396,   399,   403,   402,   366,   363,
       0,   358,   569,     0,   566,   571,   574,   393,   391,   303,
       0,   553,   655,     0,   371,   378,   372,   372,   372,   597,
     372,   372,    64,   336,   110,     0,   655,     0,   655,   655,
       0,     0,   456,   454,   449,   452,     0,     0,     0,   426,
       0,     0,   395,     0,   361,     0,   565,   302,   367,     0,
     375,     0,   377,     0,   384,     0,   381,   386,   107,   109,
       0,   628,   629,   458,   352,   331,   455,     0,     0,   655,
     655,     0,   326,   397,   359,   567,   372,   372,   372,   372,
     105,   450,     0,     0,     0,     0,     0,   376,     0,   373,
     379,   382,   327,   328,     0,     0,   655,   372,     0,     0,
     427,   374,   655,   655,   428,   429
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -948,  -948,  -948,   496,  -948,    15,  -948,  -232,   171,  -948,
    -948,    31,  -194,  -313,     1,  1288,  -948,  1559,     8,   195,
    -948,  -948,  -650,   -12,  1007,  -179,   -16,   -76,  -285,  -496,
     -53,  2555,   -85,  1016,    21,   -15,  -948,  -948,    17,  -948,
    3591,  -948,   411,    64,    87,  -378,   139,  -948,     7,  -405,
    -258,   -83,     5,  -296,    65,  -948,  -948,  -948,  -948,  -948,
    -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,  -948,
    -948,    86,  -206,  -455,   -64,  -636,  -948,  -948,  -948,   238,
     410,  -948,  -605,  -948,  -948,  -130,  -948,   -63,  -948,  -948,
    -948,   224,  -948,  -948,  -948,   -77,  -948,  -477,  -948,  -739,
    -667,  -948,  -948,    73,  -948,  -948,  -908,     3,  -948,   -41,
    -948,  -948,  -948,  -948,  -948,   279,    62,  -193,  -948,  -948,
    -948,  -948,  -948,  -271,  -948,   805,  -948,  -948,  -236,    12,
    -948,  -948,  -182,  2515,  2950,  1054,  1995,  -948,  -948,   -31,
     691,    -3,   273,   480,    66,  -948,  -948,  -948,   229,   582,
    -154,  -283,  -903,  -717,  -479,  -948,   363,  -705,  -566,  -947,
      61,  -553,  -948,  -444,  -948,   406,  -342,  -948,  -948,  -948,
      60,  -404,   920,  -393,  -948,  -948,   -39,  -948,    -6,   -25,
    -103,  -257,   877,  -279,   -28,    -1
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,     2,    67,    68,    69,   287,   465,   466,   298,
     521,   299,    71,   616,    72,   213,   689,   214,   215,    75,
      76,   821,   677,    77,    78,   300,    79,    80,    81,   546,
      82,   216,   123,   124,   243,   244,   245,   714,   654,   207,
      84,   305,   620,   655,   278,   510,   511,   279,   280,   269,
     503,   539,   659,   610,    85,   210,   303,   744,   304,   319,
     754,   223,   845,   224,   846,   713,  1035,   680,   678,   947,
     460,   290,   471,   705,   837,  1099,   230,   764,   988,  1074,
    1008,   906,   792,   907,   793,   879,  1053,  1054,   552,   883,
     605,   397,    87,    88,   670,   447,   669,   494,  1051,  1100,
     970,   861,   862,   863,   864,   971,   972,   973,   865,   692,
     831,   951,   955,    89,    90,    91,   333,   334,   557,    92,
      93,    94,   558,   253,   254,   255,   489,    95,    96,    97,
     327,    98,    99,   219,   220,   102,   221,   456,   679,   372,
     373,   374,   375,   376,   909,   910,   377,   378,   379,   778,
     595,   381,   382,   383,   384,   580,   385,   386,   387,   914,
     915,   388,   389,   390,   391,   392,   588,   209,   461,   310,
     513,   273,   129,   684,   657,   464,   459,   438,   517,   880,
     518,   537,   257,   258,   259,   302
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     105,   285,   315,   266,   266,   402,   520,   266,   256,   212,
     212,   441,   286,   717,   507,   212,   479,   222,   107,   281,
     270,   270,   252,   206,   270,   246,   707,   205,   283,   545,
     206,   783,    70,   451,    70,   301,   553,   592,   656,   246,
     349,   780,   665,   394,   206,   668,   886,   435,   437,   860,
     540,   747,   308,   312,   542,   658,   834,  -104,   306,   288,
     838,   559,   348,  1079,   819,   820,   686,   844,   439,   125,
     125,   395,   393,   393,   206,   272,   272,   125,   326,   272,
     685,   656,  1055,   665,   277,   282,   913,    86,   582,    86,
     126,   126,   686,  -111,   218,   218,   439,   700,   229,   727,
     218,   218,   218,   727,   730,   218,   710,   621,   704,  -110,
     281,   446,   790,   307,   311,   779,   798,   528,   448,   395,
     125,   336,   338,   340,   342,  -107,   106,  -625,   576,   294,
     671,   674,   686,  1095,   881,   730,  -106,    86,  -526,  -109,
     561,   316,   585,   561,  -108,   561,   125,   561,  -105,   561,
     781,   218,   978,   784,   268,   274,   368,   686,   275,   791,
     935,   247,  1079,   604,   248,   249,   941,   316,   445,   497,
     833,   750,   284,   436,   -96,   277,   282,  1098,  -525,   476,
     440,   369,   468,   469,   396,   979,   276,   504,   432,   508,
     485,  -516,   577,   445,   251,  -526,   247,   532,  1055,   248,
     249,   534,   780,   218,  -522,    86,  -516,   443,   440,   615,
    -103,   395,   393,   393,   470,   780,   946,  -527,   882,  1141,
     398,   611,   297,   536,   212,   212,  -102,   250,   687,   251,
     434,   480,   481,  -112,   598,  -525,   601,   399,   544,  -625,
    -528,  -516,   -99,  -530,  -628,  -625,   490,  -529,  -516,   615,
     615,   525,   833,   -98,   493,   507,  -101,  -345,  -629,   206,
     345,  -100,   545,   326,   266,   -97,  -531,   266,   753,   505,
     531,   505,  -345,  1061,  -527,   514,   779,   550,   538,   538,
     301,   512,   403,   538,   270,   516,   519,   758,   276,   779,
     297,  -522,   530,   431,   271,   271,    86,  -528,   271,   455,
    -530,   493,   913,   921,  -529,   913,   716,  -345,   432,   218,
     218,   781,   467,   247,  -345,   -77,   248,   249,  -516,    41,
    -112,   449,    42,  -531,   781,   450,   527,   545,   783,   727,
     727,   730,   271,   271,   533,  -418,   -91,  -520,  -521,   272,
     502,   462,   473,   433,   250,  1049,   251,   916,   613,   486,
     434,   212,   212,   212,   212,   477,   574,   575,   569,   570,
     571,   572,   218,   561,   568,   218,    58,  -104,  -521,   442,
     218,   218,   683,  1093,    86,  -516,   589,   457,   589,   125,
      70,    86,    86,   482,   301,   573,   478,   556,   -96,    86,
    1142,  1143,   432,   963,  -520,   830,   488,  -104,   463,  -418,
     316,   474,   493,   609,   913,   506,   731,   727,   609,   779,
     656,   502,   665,   736,  -418,  -111,   432,  1160,   515,   779,
     522,   266,   419,  1164,  1165,   742,   926,   458,   529,   734,
     735,  1131,   514,   892,   434,    86,   218,   218,   218,   218,
      86,   218,   218,   -76,   526,   520,   266,  -418,   824,  -418,
     995,   475,   663,   838,   535,   663,  -418,   514,   434,   523,
      86,  1093,   688,   266,   541,   693,   125,   297,   444,   746,
     872,   544,   266,  -110,   514,  -524,   663,   724,   766,   767,
     866,    86,   543,   514,   218,   419,    86,   316,   877,   622,
    -524,   526,   547,   663,   931,   483,   566,  -111,   719,   664,
     937,   939,   663,   567,  1050,   733,   738,   578,   797,   584,
     432,   505,   505,   870,   545,   428,   429,   430,  -103,   218,
    -110,   105,   444,   664,   524,  -524,   796,   520,   779,   622,
     622,   587,  -524,   246,   867,  -537,   544,   266,   206,   432,
     664,  -102,   663,   590,   218,   484,    86,   218,   514,   664,
     591,  -111,   434,    70,   594,   271,   599,    86,   271,   813,
     918,   218,   743,  1077,   728,    86,  1080,   663,  1005,  1006,
     218,   297,   769,   842,   475,    86,   812,   843,   727,   730,
     597,   434,   959,   805,   600,   827,   603,   944,  -106,   664,
    -302,   787,   602,   607,   614,  -626,   745,   337,   617,   329,
     330,   520,  -106,   638,   794,  -302,  -108,   615,    86,   -98,
     125,  -105,   125,   615,   664,   806,   872,    86,   676,   615,
     615,   690,   866,  1019,   779,   691,   895,   897,   899,   694,
     901,   316,   902,   316,   779,   218,   869,   813,   617,   617,
    -302,   876,   696,    86,  -629,   508,   695,  -302,   697,   986,
     703,   331,   332,   555,   977,   380,   380,   814,   505,  -520,
     715,   887,   732,   810,   281,  1137,  -106,   281,   794,   794,
     218,   936,   816,   720,  -520,   818,   867,   125,   343,   344,
     212,   563,   554,   329,   330,   281,  -108,   823,   737,   218,
     832,   835,   355,   356,   686,   835,   609,   740,   316,   849,
    -105,   -91,   835,   380,   380,   755,   206,  -626,  1120,  -520,
     828,   771,   271,  -626,   212,  1024,  -520,   538,   452,   453,
     809,   848,   768,   544,   836,   833,   246,   505,   757,   277,
    1108,   206,   277,   789,   583,   331,   332,   271,   814,   936,
     942,   799,   615,   866,   800,  1030,   801,   470,   809,   811,
     277,  1032,   815,  -632,   271,   817,   825,   761,   826,   358,
     359,   360,   361,   271,   548,   218,    86,  -108,  -105,  -523,
     982,   975,   976,   815,   589,   762,  1000,  1001,   833,   432,
     841,   125,   847,   271,  -523,   868,   991,   271,  -100,   -97,
     850,   983,   266,   875,   874,   380,   380,   867,   878,   218,
     884,   888,   859,   514,   866,   866,   761,   890,   358,   359,
     360,   361,   454,   454,   549,   271,   775,  -632,   271,  -523,
     794,   434,   663,   520,   762,   576,  -523,   894,   271,   896,
     952,   922,  -632,   956,   898,   900,   903,   212,  1096,  1097,
    1062,  1064,  1065,  1066,   957,   949,   247,   950,   885,   248,
     249,   954,   964,   958,   673,   675,   960,   308,   867,   867,
     984,   989,   866,   306,   125,  -632,  1004,  -632,  1007,   664,
    1010,  -628,   874,   505,  -632,  1012,  1014,   250,    86,   251,
    1016,  1021,  1033,  -292,  1022,   316,    86,   622,   673,   675,
     218,   866,   866,   622,   218,  1034,   929,  1060,  -292,   622,
     622,   380,  1036,  1039,   491,    86,    86,   248,   249,   589,
     589,  1040,  1056,  1041,   554,  1047,   867,  1057,   307,    86,
    1067,   104,   218,   104,   470,  1146,  1063,  1068,   104,   104,
     525,    86,    86,  -292,   104,   104,   104,   741,   247,   104,
    -292,   248,   249,   765,   969,   867,   867,  -106,  1158,  1159,
    -303,  1069,  1135,   380,   247,  1082,    86,   248,   249,  1083,
     782,  1084,   930,   786,  -628,  -303,  1101,    86,    86,  1103,
    -108,   104,  1125,   125,   803,  1105,   212,   212,   125,  1126,
     835,  1132,  1109,  1044,  1045,   104,  1111,   251,  1087,   432,
    1113,   945,  1115,   776,  1003,  1127,   617,   776,  1128,  1009,
    -303,  -629,   617,   933,   953,  1148,  1140,  -303,   617,   617,
     470,  1152,  1153,   271,   271,   125,   961,   962,   739,   227,
     911,   130,   622,   247,   804,  1124,   248,   249,  1102,   905,
     772,   434,    86,    86,  1144,  1145,  1027,   104,  1133,   104,
      86,   981,   943,  1094,   125,   835,  -106,  1123,   773,  -106,
    -106,   974,  1048,   987,   250,  -628,   251,   355,   356,   492,
     208,   218,   218,    86,   908,   859,   432,   774,  1076,  -108,
    1071,   589,  -108,  -108,   266,     0,     0,  -106,     0,  -106,
     366,   367,   368,   271,     0,   514,     0,   693,   835,     0,
       0,   271,   472,  1052,     0,   358,   359,   360,   361,   472,
    -108,   458,  -108,     0,   663,   125,   125,   369,   434,   247,
       0,   762,   248,   249,     0,     0,    86,  1023,    86,  -628,
       0,    86,     0,     0,     0,  1031,   859,   859,   835,   835,
     104,   617,     0,   380,  -628,   247,   495,   271,   248,   249,
     250,  1038,   251,   104,   104,   212,   212,     0,  1046,   419,
    1070,   664,  1154,  1155,   912,   835,   432,     0,     0,     0,
    1085,   835,   835,   125,     0,   432,   250,  -628,   251,  -628,
       0,     0,   218,  -628,     0,   432,  -628,   426,   427,   428,
     429,   430,    86,    86,   859,  -629,   350,   351,   352,   353,
     354,   475,   125,   125,     0,     0,   104,   917,   434,   104,
     458,  1088,     0,  1089,   104,   104,  1090,   434,   104,     0,
    1086,     0,     0,   859,   859,   104,   104,   434,   932,   934,
       0,     0,     0,   104,   938,   940,   495,     0,     0,     0,
     218,   218,    86,  1072,     0,     0,   911,     0,     0,   911,
     579,   911,     0,     0,    86,    86,     0,     0,     0,  -629,
     932,   934,   335,   938,   940,   329,   330,   994,   593,   996,
       0,     0,   271,   997,  -629,     0,     0,  1129,  1130,   104,
     104,   104,   104,   104,   104,   104,   104,   908,   562,     0,
     908,   329,   330,   908,     0,   908,     0,     0,     0,    73,
       0,    73,   121,   121,   104,     0,     0,  -629,     0,  -629,
     121,     0,   911,  -629,     0,     0,  -629,   331,   332,     0,
       0,     0,     0,     0,     0,   104,     0,  1156,   104,     0,
     104,     0,     0,   104,     0,  1011,  1013,     0,   247,  1162,
    1163,   248,   249,   331,   332,     0,     0,     0,   911,    73,
     911,     0,   911,   121,   911,     0,   908,  1020,   328,   329,
     330,  1058,  1059,   104,     0,   502,     0,   706,   706,   250,
       0,   251,     0,   104,   104,   271,   339,   329,   330,   121,
    1020,     0,     0,  1078,     0,  1081,     0,   911,   104,     0,
     104,   104,   908,     0,   908,     0,   908,     0,   908,     0,
       0,   104,   341,   329,   330,   104,     0,     0,     0,   104,
       0,   331,   332,     0,   104,     0,     0,    73,   495,   104,
     776,     0,     0,   917,     0,   495,   917,  1075,   917,   331,
     332,   908,     0,  1104,     0,  -299,  1106,     0,  -299,  -299,
       0,     0,   357,     0,   358,   359,   360,   361,     0,     0,
     247,     0,   104,   248,   249,   331,   332,   555,   329,   330,
     362,   104,   560,   329,   330,  -299,  -299,     0,  -299,     0,
     564,   329,   330,     0,   363,     0,     0,     0,  1134,   104,
       0,   250,  1136,   251,  1138,   364,     0,   104,  1139,   917,
       0,   365,   366,   367,   368,     0,  1110,  1112,  1114,     0,
    1116,  1117,   357,     0,   358,   359,   360,   361,    73,     0,
     331,   332,  -655,     0,   104,   331,   332,     0,     0,   369,
     362,  1157,   370,   331,   332,   917,     0,   917,     0,   917,
       0,   917,     0,   104,   363,   371,     0,   785,     0,   358,
     359,   360,   361,     0,     0,   364,   565,   329,   330,     0,
       0,   365,   366,   367,   368,   362,  1147,  1149,  1150,  1151,
       0,     0,     0,     0,   917,   822,     0,     0,     0,     0,
      74,     0,    74,   122,   122,     0,     0,  1161,     0,   369,
     364,   122,   370,     0,     0,     0,    73,   366,   367,   368,
       0,     0,     0,    73,    73,   371,     0,   802,     0,   331,
     332,    73,   756,   329,   330,     0,   472,     0,     0,   104,
     104,     0,   121,   247,   369,     0,   248,   249,     0,     0,
      74,     0,     0,     0,   122,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,     0,     0,     0,
       0,   416,   417,   104,   985,     0,   251,    73,     0,     0,
     122,     0,    73,     0,   419,   331,   332,     0,     0,     0,
       0,     0,     0,     0,   893,     0,     0,     0,     0,     0,
       0,     0,    73,     0,     0,   420,     0,   421,   422,   423,
     424,   425,   426,   427,   428,   429,   430,     0,    74,     0,
       0,     0,     0,    73,     0,  -277,     0,     0,    73,   121,
       0,    73,     0,   357,     0,   358,   359,   360,   361,     0,
     948,     0,     0,     0,   761,     0,   358,   359,   360,   361,
       0,   362,   104,     0,     0,     0,     0,     0,     0,     0,
     104,   104,   762,     0,   104,   706,     0,   104,   104,     0,
       0,    73,    73,   104,   104,     0,   364,   472,     0,   104,
     104,     0,   365,   366,   367,   368,     0,   364,    73,     0,
       0,     0,     0,   104,     0,     0,   104,     0,     0,    73,
       0,     0,   416,   417,     0,   104,   104,    73,     0,    74,
     369,  -655,     0,   370,     0,   419,     0,    73,     0,     0,
       0,     0,     0,     0,  -655,  -655,  -655,  -655,  -655,  -655,
     104,  -655,     0,     0,     0,     0,  -655,  -655,  -655,     0,
       0,   104,   104,   426,   427,   428,   429,   430,  -655,  -655,
      73,  -655,  -655,  -655,  -655,  -655,     0,     0,     0,    73,
     761,     0,   358,   359,   360,   361,     0,     0,   472,     0,
       0,     0,     0,   121,   472,   121,     0,     0,   762,   761,
       0,   358,   359,   360,   361,    73,     0,    74,     0,     0,
       0,     0,     0,     0,    74,    74,   104,   762,     0,     0,
       0,  -655,    74,   364,     0,     0,   104,   104,     0,   763,
       0,     0,     0,   122,   104,     0,  -655,     0,     0,     0,
       0,     0,   364,     0,     0,     0,  -655,     0,   990,  -655,
    -655,     0,     0,     0,     0,   104,   104,   104,     0,     0,
     121,     0,     0,     0,     0,     0,     0,     0,    74,  -655,
    -655,     0,     0,    74,     0,   276,  -655,  -655,  -655,  -655,
       0,   472,   472,     0,     0,     0,     0,     0,     0,     0,
       0,   416,   417,    74,     0,   357,     0,   358,   359,   360,
     361,     0,     0,     0,   419,     0,     0,     0,   416,   417,
     104,     0,   104,   362,    74,   104,     0,     0,     0,    74,
     122,   419,    74,     0,     0,     0,     0,     0,    73,   423,
     424,   425,   426,   427,   428,   429,   430,     0,   364,     0,
       0,     0,     0,     0,   365,   366,   367,   368,   425,   426,
     427,   428,   429,   430,     0,     0,   103,     0,   103,   128,
     128,     0,    74,    74,   121,     0,   104,   232,   472,     0,
       0,     0,   369,     0,     0,   370,   104,   104,     0,    74,
       0,     0,     0,     0,     0,     0,     0,     0,   551,     0,
      74,   472,   472,     0,     0,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,   103,     0,    74,     0,
     318,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   104,   104,   104,     0,   357,     0,
     358,   359,   360,   361,     0,     0,   318,     0,   104,   104,
      73,    74,     0,     0,     0,     0,   362,   121,    73,    73,
      74,     0,     0,     0,     0,    73,     0,     0,     0,     0,
       0,    73,    73,     0,   122,     0,   122,    73,    73,     0,
       0,   364,     0,     0,   103,     0,    74,   365,   366,   367,
     368,    73,     0,   357,     0,   358,   359,   360,   361,     0,
       0,     0,     0,    73,    73,     0,     0,     0,     0,     0,
       0,   362,     0,     0,     0,   369,     0,     0,   370,     0,
       0,     0,     0,     0,     0,     0,     0,   581,    73,  1073,
       0,     0,     0,     0,     0,     0,   364,     0,     0,    73,
      73,   122,   365,   366,   367,   368,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
     369,   121,     0,   370,     0,   103,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   802,
       0,     0,     0,     0,    73,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    73,    73,     0,     0,   121,    74,
       0,     0,    73,     0,     0,     0,     0,   404,   405,   406,
     407,   408,   409,   410,   411,   412,   413,   414,   415,     0,
       0,     0,     0,   416,   417,    73,     0,   121,     0,     0,
       0,     0,     0,     0,     0,   122,   419,     0,     0,     0,
       0,     0,     0,   103,     0,     0,     0,     0,     0,     0,
     103,   103,     0,     0,     0,     0,     0,   420,   103,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,   318,
       0,     0,     0,     0,     0,     0,     0,     0,    73,     0,
      73,     0,     0,    73,     0,     0,     0,     0,   121,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,     0,     0,     0,     0,   103,
       0,    74,     0,     0,     0,     0,     0,     0,   122,    74,
      74,     0,     0,     0,     0,     0,    74,     0,     0,   103,
       0,     0,    74,    74,     0,     0,     0,     0,    74,    74,
       0,     0,     0,     0,    73,    73,   121,     0,     0,     0,
     103,     0,    74,     0,     0,   103,   318,     0,   623,     0,
       0,     0,     0,     0,    74,    74,     0,     0,   357,     0,
     358,   359,   360,   361,     0,   121,   121,     0,     0,   357,
       0,   358,   359,   360,   361,     0,   362,     0,     0,    74,
       0,     0,     0,     0,    73,     0,     0,   362,   623,   623,
      74,    74,   777,     0,     0,     0,    73,    73,     0,     0,
       0,   364,     0,   889,     0,   103,     0,   365,   366,   367,
     368,     0,   364,     0,     0,     0,   103,   122,   365,   366,
     367,   368,   122,     0,   103,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   369,     0,     0,   370,     0,
       0,     0,     0,     0,     0,    74,   369,     0,     0,   370,
       0,     0,     0,     0,     0,    74,    74,     0,     0,   122,
       0,     0,     0,    74,     0,     0,   100,   103,   100,   127,
     127,   127,     0,     0,     0,     0,   103,   231,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,   122,     0,
     318,     0,   318,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,     0,     0,     0,    83,     0,    83,     0,
       0,     0,     0,     0,     0,     0,   100,   228,     0,     0,
     317,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
       0,    74,     0,     0,    74,     0,   317,     0,     0,   122,
     122,     0,     0,     0,     0,     0,    83,   318,     0,     0,
       0,     0,     0,     0,     0,     0,   651,   652,     0,     0,
     653,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   100,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,    74,    74,   122,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,    83,   103,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,   122,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,     0,     0,     0,    74,     0,     0,   203,   276,
       0,   128,     0,     0,     0,     0,     0,    74,    74,  -656,
    -656,  -656,  -656,   408,   409,   100,     0,  -656,  -656,     0,
       0,     0,     0,     0,     0,   416,   417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,     0,     0,     0,     0,
       0,   421,   422,   423,   424,   425,   426,   427,   428,   429,
     430,     0,     0,     0,     0,     0,     0,   103,     0,     0,
       0,     0,     0,     0,   318,   103,   623,     0,     0,     0,
       0,     0,   623,   100,     0,     0,     0,     0,   623,   623,
     100,   100,     0,     0,   103,   103,     0,     0,   100,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   317,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,   103,     0,    83,     0,     0,     0,     0,     0,     0,
      83,    83,     0,     0,     0,     0,     0,     0,    83,     0,
       0,     0,     0,     0,   100,   103,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   103,   103,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   128,    83,     0,     0,     0,   128,    83,
     100,     0,     0,     0,     0,   100,   317,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    83,
       0,   623,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   103,   103,     0,     0,  1029,     0,     0,     0,   103,
      83,   101,     0,   101,     0,    83,     0,     0,   618,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,     0,   128,   100,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,     0,   100,     0,     0,     0,   618,   618,
       0,   101,     0,     0,   100,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,     0,     0,     0,   891,
       0,     0,     0,     0,     0,   103,    83,   103,     0,     0,
     103,     0,     0,     0,    83,   128,   128,   100,     0,     0,
       0,     0,     0,     0,    83,     0,   100,   404,   405,   406,
     407,   408,   409,   410,   411,   412,   413,   414,   415,     0,
     317,     0,   317,   416,   417,     0,     0,     0,     0,   101,
       0,     0,   100,     0,     0,     0,   419,    83,     0,     0,
       0,     0,     0,     0,     0,     0,    83,     0,     0,     0,
       0,   103,   103,   128,     0,     0,     0,   420,     0,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,     0,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   128,   128,     0,     0,     0,   317,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   103,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   103,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,     0,     0,     0,    14,     0,   108,   109,
      17,    18,     0,     0,     0,   100,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,   127,     0,    43,    44,    83,   116,     0,   101,     0,
       0,     0,     0,     0,     0,   101,   101,     0,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,   313,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   101,
       0,   120,     0,     0,   101,     0,     0,   100,     0,   314,
       0,     0,     0,     0,   317,   100,     0,     0,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   100,   100,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,     0,    83,   100,     0,
     101,     0,     0,   101,     0,    83,   618,     0,     0,     0,
     100,   100,   618,     0,     0,     0,     0,     0,   618,   618,
       0,     0,     0,     0,    83,    83,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   100,     0,     0,    83,     0,
       0,     0,     0,   101,   101,     0,   100,   100,     0,     0,
      83,    83,     0,     0,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   101,     0,   127,     0,    83,     0,     0,   127,   101,
       0,     0,     0,     0,     0,     0,    83,    83,     0,   101,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   100,   100,     0,     0,  1028,     0,     0,     0,   100,
       0,     0,   101,     0,     0,     0,     0,     0,     0,     0,
       0,   101,   904,     0,     0,     0,     0,     0,     0,     0,
       0,   618,   100,     0,   127,     0,     0,     0,     0,     0,
       0,    83,    83,     0,     0,  1026,     0,   101,     0,    83,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,   415,     0,     0,     0,     0,   416,   417,     0,     0,
       0,     0,    83,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,   100,     0,   100,     0,     0,
     100,     0,     0,     0,     0,   127,   127,     0,     0,     0,
     420,     0,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,     0,    83,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,   217,
     217,     0,     0,     0,     0,   217,   267,   267,     0,     0,
     267,   100,   100,   127,     0,     0,     0,     0,     0,     0,
       0,     0,  -439,     0,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,   289,   291,   292,
     293,     0,   127,   127,   267,   309,  -439,  -439,     0,     0,
       0,    83,    83,     0,     0,     0,   346,   347,     0,     0,
       0,   100,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   100,   100,     0,     0,     0,  -525,  -525,
    -525,  -525,  -525,  -525,  -525,  -525,  -525,  -525,  -525,  -525,
       0,     0,     0,     0,  -525,  -525,  -525,     0,     0,  -439,
       0,    83,     0,     0,     0,     0,     0,  -525,   217,     0,
       0,  -525,     0,    83,    83,     0,     0,     0,     0,     0,
       0,  -439,     0,     0,  -439,  -439,     0,     0,  -525,     0,
    -525,  -525,  -439,  -525,  -525,  -525,  -525,  -525,  -525,  -525,
       0,     0,   101,     0,     0,     0,  -525,  -439,  -439,     0,
     101,   101,  -439,  -525,  -439,     0,     0,   101,     0,     0,
       0,     0,     0,   101,   101,     0,     0,     0,     0,   101,
     101,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   101,   404,   405,   406,   407,   408,   409,
     410,     0,   412,   413,     0,   101,   101,     0,     0,     0,
     416,   417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,   217,   217,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   101,   101,     0,     0,     0,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,     0,     0,     0,     0,
       0,     0,     0,   498,   499,   500,   346,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   267,     0,     0,
     267,     0,     0,     0,     0,   217,   217,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,   101,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   217,   217,   217,   217,     0,   217,   217,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   586,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   596,     0,     0,
     101,     0,   101,     0,     0,   101,     0,     0,     0,   608,
       0,     0,     0,     0,   619,   624,   625,   626,   627,   628,
     629,   630,   631,   632,   633,   634,   635,   636,   637,     0,
     639,   640,   641,   642,   643,   644,   645,   646,   647,   648,
     649,   650,     0,     0,   267,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   672,   672,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,   101,     0,   267,
       0,     0,   217,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   672,     0,   267,     0,   672,   672,
       0,     0,     0,     0,     0,   267,     0,     0,     0,     0,
       0,     0,     0,     0,   718,     0,     0,   721,   722,     0,
       0,     0,   723,     0,     0,   726,   101,   729,     0,   309,
     293,     0,     0,     0,     0,     0,     0,     0,   101,   101,
       0,     0,     0,     0,     0,     0,     0,   672,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   726,     0,     0,
     309,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     267,     0,     0,     0,     0,     0,     0,   404,   405,   406,
     407,   408,   409,     0,     0,   412,   413,   759,   760,     0,
       0,     0,     0,   416,   417,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   770,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   788,     0,     0,   795,     0,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -293,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -293,  -293,  -293,  -293,  -293,  -293,     0,  -293,     0,
       0,     0,     0,  -293,     0,  -293,  -293,  -293,     0,     0,
       0,     0,     0,     0,     0,  -293,  -293,     0,  -293,  -293,
    -293,  -293,  -293,     0,     0,     0,     0,     0,     0,     0,
     217,     0,     0,     0,     0,  -293,     0,     0,     0,     0,
       0,     0,   829,     0,     0,   770,   788,     0,     0,     0,
    -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,
    -293,  -293,     0,     0,   217,     0,  -293,  -293,  -293,     0,
       0,  -293,     0,     0,     0,     0,   871,  -293,     0,  -293,
       0,     0,     0,  -293,     0,   726,   309,     0,     0,     0,
       0,  -293,     0,  -293,     0,     0,  -293,  -293,     0,     0,
    -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,
    -293,  -293,     0,     0,     0,     0,     0,  -293,  -293,  -293,
    -293,     0,     0,  -293,  -293,  -293,  -293,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   920,     0,     0,
       0,     0,   672,   923,     0,   267,     0,     0,   672,   672,
       0,     0,     0,   726,   672,   672,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   217,     0,     0,
     672,   672,     0,   672,   672,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   267,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     980,     0,     0,     0,   293,     0,     0,   660,   661,     0,
       0,   662,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   992,   993,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,   998,   999,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
    1015,     0,     0,     0,     0,     0,   188,   189,   190,     0,
    1017,  1018,     0,     0,     0,     0,     0,   672,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     672,   201,   202,     0,     0,     0,     0,     0,   498,   203,
     276,     0,     0,     0,     0,     0,   217,   217,     0,     0,
       0,     0,  -655,     3,   309,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,    14,     0,    15,    16,    17,    18,     0,     0,     0,
       0,     0,    19,    20,    21,    22,    23,    24,    25,     0,
       0,    26,     0,     0,     0,     0,     0,    27,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,   267,    57,    58,
      59,     0,    60,    61,    62,  -417,    63,  -655,     0,     0,
    -655,  -655,     0,     0,     0,     0,     0,     0,  -417,  -417,
    -417,  -417,  -417,  -417,     0,  -417,    64,    65,    66,     0,
    -417,  -417,  -417,  -417,     0,     0,     0,     0,  -655,     0,
    -655,     0,  -417,  -417,     0,  -417,  -417,  -417,  -417,  -417,
       0,     0,     0,     0,     0,   217,   217,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -417,  -417,  -417,
    -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,     0,
       0,     0,     0,  -417,  -417,  -417,     0,     0,  -417,     0,
       0,     0,     0,     0,  -417,     0,  -417,     0,     0,     0,
    -417,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -417,     0,     0,  -417,  -417,     0,     0,  -417,     0,  -417,
    -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,  -417,     0,
       0,  -516,     0,  -417,  -417,  -417,  -417,  -417,     0,   276,
    -417,  -417,  -417,  -417,  -516,  -516,  -516,  -516,  -516,  -516,
       0,  -516,     0,     0,     0,     0,  -516,     0,  -516,  -516,
       0,     0,     0,     0,     0,     0,     0,     0,  -516,  -516,
       0,  -516,  -516,  -516,  -516,  -516,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   496,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -516,  -516,  -516,  -516,  -516,  -516,  -516,
    -516,  -516,  -516,  -516,  -516,     0,     0,     0,     0,  -516,
    -516,  -516,     0,  -516,  -516,     0,     0,     0,     0,     0,
    -516,     0,  -516,     0,     0,     0,  -516,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -516,     0,     0,  -516,
    -516,     0,  -516,  -516,     0,  -516,  -516,  -516,  -516,  -516,
    -516,  -516,  -516,  -516,  -516,     0,     0,  -655,     0,     0,
    -516,  -516,  -516,  -516,     0,     0,  -516,  -516,  -516,  -516,
    -655,  -655,  -655,  -655,  -655,  -655,     0,  -655,     0,     0,
       0,     0,  -655,  -655,  -655,  -655,     0,     0,     0,     0,
       0,     0,     0,     0,  -655,  -655,     0,  -655,  -655,  -655,
    -655,  -655,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -655,
    -655,  -655,  -655,  -655,  -655,  -655,  -655,  -655,  -655,  -655,
    -655,     0,     0,     0,     0,  -655,  -655,  -655,     0,     0,
    -655,     0,     0,     0,     0,     0,  -655,     0,  -655,     0,
       0,     0,  -655,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -655,     0,     0,  -655,  -655,     0,     0,  -655,
       0,  -655,  -655,  -655,  -655,  -655,  -655,  -655,  -655,  -655,
    -655,     0,     0,  -655,     0,  -655,  -655,  -655,  -655,  -655,
       0,   276,  -655,  -655,  -655,  -655,  -655,  -655,  -655,  -655,
    -655,  -655,     0,  -655,     0,     0,     0,     0,  -655,     0,
    -655,  -655,     0,     0,     0,     0,     0,     0,     0,     0,
    -655,  -655,     0,  -655,  -655,  -655,  -655,  -655,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -655,  -655,  -655,  -655,  -655,
    -655,  -655,  -655,  -655,  -655,  -655,  -655,     0,     0,     0,
       0,  -655,  -655,  -655,     0,     0,  -655,     0,     0,     0,
       0,     0,  -655,     0,  -655,     0,     0,     0,  -655,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -655,     0,
       0,  -655,  -655,     0,     0,  -655,     0,  -655,  -655,  -655,
    -655,  -655,  -655,  -655,  -655,  -655,  -655,     0,     0,  -632,
       0,     0,  -655,  -655,  -655,  -655,     0,   276,  -655,  -655,
    -655,  -655,  -632,  -632,  -632,     0,  -632,  -632,     0,  -632,
       0,     0,     0,     0,  -632,  -632,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -632,  -632,     0,  -632,
    -632,  -632,  -632,  -632,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,
    -632,  -632,  -632,     0,     0,     0,     0,  -632,  -632,  -632,
       0,   807,  -632,     0,     0,     0,     0,     0,     0,     0,
    -632,     0,     0,     0,  -632,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -632,     0,     0,  -632,  -632,     0,
    -107,  -632,     0,  -632,  -632,  -632,  -632,  -632,  -632,  -632,
    -632,  -632,  -632,     0,     0,  -632,     0,  -632,  -632,  -632,
       0,   -99,     0,     0,  -632,  -632,  -632,  -632,  -632,  -632,
    -632,     0,  -632,  -632,     0,  -632,     0,     0,     0,     0,
    -632,  -632,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -632,  -632,     0,  -632,  -632,  -632,  -632,  -632,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -632,  -632,  -632,
    -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,     0,
       0,     0,     0,  -632,  -632,  -632,     0,   807,  -632,     0,
       0,     0,     0,     0,     0,     0,  -632,     0,     0,     0,
    -632,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -632,     0,     0,  -632,  -632,     0,  -107,  -632,     0,  -632,
    -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,  -632,     0,
       0,  -302,     0,  -632,  -632,  -632,     0,  -632,     0,     0,
    -632,  -632,  -632,  -632,  -302,  -302,  -302,     0,  -302,  -302,
       0,  -302,     0,     0,     0,     0,  -302,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -302,  -302,
       0,  -302,  -302,  -302,  -302,  -302,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,     0,     0,     0,     0,  -302,
    -302,  -302,     0,   808,  -302,     0,     0,     0,     0,     0,
       0,     0,  -302,     0,     0,     0,  -302,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -302,     0,     0,  -302,
    -302,     0,  -109,  -302,     0,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,     0,     0,  -302,     0,     0,
    -302,  -302,     0,  -101,     0,     0,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,     0,  -302,  -302,     0,  -302,     0,     0,
       0,     0,  -302,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -302,  -302,     0,  -302,  -302,  -302,
    -302,  -302,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,     0,     0,     0,     0,  -302,  -302,  -302,     0,   808,
    -302,     0,     0,     0,     0,     0,     0,     0,  -302,     0,
       0,     0,  -302,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -302,     0,     0,  -302,  -302,     0,  -109,  -302,
       0,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,     0,     0,     0,     0,     0,  -302,  -302,     0,  -302,
       0,     0,  -302,  -302,  -302,  -302,   295,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,  -655,  -655,
    -655,     0,     0,  -655,    14,     0,    15,    16,    17,    18,
       0,     0,     0,     0,     0,    19,    20,    21,    22,    23,
      24,    25,     0,     0,    26,     0,     0,     0,     0,     0,
      27,     0,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
    -655,     0,     0,  -655,  -655,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -655,   295,  -655,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,  -655,     0,  -655,  -655,
      14,     0,    15,    16,    17,    18,     0,     0,     0,     0,
       0,    19,    20,    21,    22,    23,    24,    25,     0,     0,
      26,     0,     0,     0,     0,     0,    27,     0,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,  -655,     0,     0,  -655,
    -655,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,    65,    66,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -655,   295,  -655,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,  -655,     0,     0,  -655,    14,  -655,    15,    16,
      17,    18,     0,     0,     0,     0,     0,    19,    20,    21,
      22,    23,    24,    25,     0,     0,    26,     0,     0,     0,
       0,     0,    27,     0,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,    50,     0,
       0,    51,    52,     0,    53,    54,     0,    55,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,  -655,     0,     0,  -655,  -655,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    64,    65,    66,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -655,   295,  -655,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,  -655,     0,
       0,  -655,    14,     0,    15,    16,    17,    18,  -655,     0,
       0,     0,     0,    19,    20,    21,    22,    23,    24,    25,
       0,     0,    26,     0,     0,     0,     0,     0,    27,     0,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,    50,     0,     0,    51,    52,     0,
      53,    54,     0,    55,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,  -655,     0,
       0,  -655,  -655,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    64,    65,    66,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -655,
     295,  -655,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,  -655,     0,     0,  -655,    14,     0,
      15,    16,    17,    18,     0,     0,     0,     0,     0,    19,
      20,    21,    22,    23,    24,    25,     0,     0,    26,     0,
       0,     0,     0,     0,    27,     0,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
      50,     0,     0,    51,    52,     0,    53,    54,     0,    55,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,  -655,     0,     0,  -655,  -655,     3,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,    64,    65,    66,     0,    14,     0,    15,
      16,    17,    18,     0,     0,  -655,     0,  -655,    19,    20,
      21,    22,    23,    24,    25,     0,     0,    26,     0,     0,
       0,     0,     0,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,  -655,     0,     0,  -655,  -655,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,    65,    66,     0,     0,  -655,     0,     0,
       0,     0,     0,     0,  -655,   295,  -655,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,  -655,  -655,
       0,     0,     0,    14,     0,    15,    16,    17,    18,     0,
       0,     0,     0,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,    27,
       0,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,  -655,
       0,     0,  -655,  -655,   295,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,    64,    65,
      66,     0,    14,     0,    15,    16,    17,    18,     0,     0,
    -655,     0,  -655,    19,    20,    21,    22,    23,    24,    25,
       0,     0,    26,     0,     0,     0,     0,     0,    27,     0,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,    50,     0,     0,   296,    52,     0,
      53,    54,     0,    55,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,  -655,     0,
       0,  -655,  -655,   295,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,    64,    65,    66,
       0,    14,     0,    15,    16,    17,    18,     0,  -655,  -655,
       0,  -655,    19,    20,    21,    22,    23,    24,    25,     0,
       0,    26,     0,     0,     0,     0,     0,    27,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,  -655,     0,     0,
    -655,  -655,   295,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,    64,    65,    66,     0,
      14,     0,    15,    16,    17,    18,     0,  -655,  -655,     0,
    -655,    19,    20,    21,    22,    23,    24,    25,     0,     0,
      26,     0,     0,     0,     0,     0,    27,     0,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,  -655,     0,     0,  -655,
    -655,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,    65,    66,     0,     0,
    -655,     0,     0,     0,     0,     0,     0,  -655,   295,  -655,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,  -655,     0,     0,     0,    14,     0,    15,    16,
      17,    18,     0,     0,     0,     0,     0,    19,    20,    21,
      22,    23,    24,    25,     0,     0,    26,     0,     0,     0,
       0,     0,    27,     0,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,    50,     0,
       0,    51,    52,     0,    53,    54,     0,    55,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,  -655,     0,     0,  -655,  -655,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,    64,    65,    66,     0,    14,     0,    15,    16,    17,
      18,     0,     0,  -655,     0,  -655,    19,    20,    21,    22,
      23,    24,    25,     0,     0,    26,     0,     0,     0,     0,
       0,    27,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,    50,     0,     0,
      51,    52,     0,    53,    54,     0,    55,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,   247,     0,     0,   248,   249,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
      64,    65,    66,     0,    14,     0,    15,    16,    17,    18,
       0,     0,   250,     0,   251,    19,    20,    21,    22,    23,
      24,    25,     0,     0,    26,     0,     0,     0,     0,     0,
      27,     0,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
     247,     0,     0,   248,   249,     0,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,     0,     0,     0,    64,
      65,    66,     0,    14,     0,   108,   109,    17,    18,     0,
       0,   250,     0,   251,   110,   111,   112,    22,    23,    24,
      25,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,    32,  1037,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    52,
       0,    53,    54,     0,     0,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,  -278,
       0,     0,  -278,  -278,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,   265,
      66,     0,     0,     0,     0,  -278,  -278,     0,     0,     0,
    -278,     0,  -278,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,     0,     0,     0,     0,    14,
       0,    15,    16,    17,    18,     0,     0,     0,     0,     0,
      19,    20,    21,    22,    23,    24,    25,     0,     0,    26,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    52,     0,    53,    54,     0,
       0,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,   247,     0,     0,   248,   249,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,    64,    65,    66,     0,    14,     0,
     108,   109,    17,    18,     0,     0,   250,     0,   251,   110,
     111,   112,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,   247,     0,     0,   248,   249,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,    64,   265,    66,     0,    14,     0,    15,
      16,    17,    18,     0,     0,   250,     0,   251,    19,    20,
      21,    22,    23,    24,    25,     0,     0,    26,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    52,     0,    53,    54,     0,     0,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,   247,     0,     0,   248,   249,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   251,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,     0,     0,     0,   155,   156,   157,   158,   159,   160,
     161,   162,   163,   164,     0,     0,     0,     0,     0,   165,
     166,   167,   168,   169,   170,   171,   172,    35,    36,   173,
      38,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   116,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,     0,     0,     0,     0,     0,     0,   203,   204,  -625,
    -625,  -625,  -625,  -625,  -625,  -625,  -625,  -625,     0,     0,
       0,     0,     0,     0,     0,  -625,     0,  -625,  -625,  -625,
    -625,     0,  -625,     0,     0,     0,  -625,  -625,  -625,  -625,
    -625,  -625,  -625,     0,     0,  -625,     0,     0,     0,     0,
       0,     0,     0,     0,  -625,  -625,  -625,  -625,  -625,  -625,
    -625,  -625,  -625,     0,  -625,  -625,  -625,     0,     0,  -625,
       0,     0,  -625,  -625,     0,  -625,  -625,  -625,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -625,  -625,     0,     0,     0,     0,     0,  -625,     0,     0,
    -625,  -625,     0,  -625,  -625,     0,  -625,     0,  -625,  -625,
    -625,     0,  -625,  -625,  -625,     0,  -625,  -625,  -625,     0,
    -625,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -625,  -625,  -625,     0,  -625,     0,     0,     0,     0,     0,
    -625,  -626,  -626,  -626,  -626,  -626,  -626,  -626,  -626,  -626,
       0,     0,     0,     0,     0,     0,     0,  -626,     0,  -626,
    -626,  -626,  -626,     0,  -626,     0,     0,     0,  -626,  -626,
    -626,  -626,  -626,  -626,  -626,     0,     0,  -626,     0,     0,
       0,     0,     0,     0,     0,     0,  -626,  -626,  -626,  -626,
    -626,  -626,  -626,  -626,  -626,     0,  -626,  -626,  -626,     0,
       0,  -626,     0,     0,  -626,  -626,     0,  -626,  -626,  -626,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -626,  -626,     0,     0,     0,     0,     0,  -626,
       0,     0,  -626,  -626,     0,  -626,  -626,     0,  -626,     0,
    -626,  -626,  -626,     0,  -626,  -626,  -626,     0,  -626,  -626,
    -626,     0,  -626,     0,     0,     0,     0,     0,     0,  -628,
    -628,  -628,  -628,  -628,  -628,  -628,  -628,  -628,     0,     0,
       0,     0,  -626,  -626,  -626,  -628,  -626,  -628,  -628,  -628,
    -628,     0,  -626,     0,     0,     0,  -628,  -628,  -628,  -628,
    -628,  -628,  -628,     0,     0,  -628,     0,     0,     0,     0,
       0,     0,     0,     0,  -628,  -628,  -628,  -628,  -628,  -628,
    -628,  -628,  -628,     0,  -628,  -628,  -628,     0,     0,  -628,
       0,     0,  -628,  -628,     0,  -628,  -628,  -628,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -628,  -628,     0,     0,     0,     0,     0,  -628,   839,     0,
    -628,  -628,     0,  -628,  -628,     0,  -628,     0,  -628,  -628,
    -628,     0,  -628,  -628,  -628,     0,  -628,  -628,  -628,     0,
    -628,     0,     0,     0,     0,     0,     0,  -107,  -629,  -629,
    -629,  -629,  -629,  -629,  -629,  -629,  -629,     0,     0,     0,
    -628,  -628,  -628,     0,  -629,     0,  -629,  -629,  -629,  -629,
    -628,     0,     0,     0,     0,  -629,  -629,  -629,  -629,  -629,
    -629,  -629,     0,     0,  -629,     0,     0,     0,     0,     0,
       0,     0,     0,  -629,  -629,  -629,  -629,  -629,  -629,  -629,
    -629,  -629,     0,  -629,  -629,  -629,     0,     0,  -629,     0,
       0,  -629,  -629,     0,  -629,  -629,  -629,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -629,
    -629,     0,     0,     0,     0,     0,  -629,   840,     0,  -629,
    -629,     0,  -629,  -629,     0,  -629,     0,  -629,  -629,  -629,
       0,  -629,  -629,  -629,     0,  -629,  -629,  -629,     0,  -629,
       0,     0,     0,     0,     0,     0,  -109,  -630,  -630,  -630,
    -630,  -630,  -630,  -630,  -630,  -630,     0,     0,     0,  -629,
    -629,  -629,     0,  -630,     0,  -630,  -630,  -630,  -630,  -629,
       0,     0,     0,     0,  -630,  -630,  -630,  -630,  -630,  -630,
    -630,     0,     0,  -630,     0,     0,     0,     0,     0,     0,
       0,     0,  -630,  -630,  -630,  -630,  -630,  -630,  -630,  -630,
    -630,     0,  -630,  -630,  -630,     0,     0,  -630,     0,     0,
    -630,  -630,     0,  -630,  -630,  -630,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -630,  -630,
       0,     0,     0,     0,     0,  -630,     0,     0,  -630,  -630,
       0,  -630,  -630,     0,  -630,     0,  -630,  -630,  -630,     0,
    -630,  -630,  -630,     0,  -630,  -630,  -630,     0,  -630,     0,
       0,     0,     0,     0,     0,  -631,  -631,  -631,  -631,  -631,
    -631,  -631,  -631,  -631,     0,     0,     0,     0,  -630,  -630,
    -630,  -631,     0,  -631,  -631,  -631,  -631,     0,  -630,     0,
       0,     0,  -631,  -631,  -631,  -631,  -631,  -631,  -631,     0,
       0,  -631,     0,     0,     0,     0,     0,     0,     0,     0,
    -631,  -631,  -631,  -631,  -631,  -631,  -631,  -631,  -631,     0,
    -631,  -631,  -631,     0,     0,  -631,     0,     0,  -631,  -631,
       0,  -631,  -631,  -631,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -631,  -631,     0,     0,
       0,     0,     0,  -631,     0,     0,  -631,  -631,     0,  -631,
    -631,     0,  -631,     0,  -631,  -631,  -631,     0,  -631,  -631,
    -631,     0,  -631,  -631,  -631,     0,  -631,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -631,  -631,  -631,     0,
       0,     0,     0,     0,     0,     0,  -631,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,     0,     0,     0,   155,   156,   157,   233,   234,   235,
     236,   162,   163,   164,     0,     0,     0,     0,     0,   165,
     166,   167,   237,   238,   239,   240,   172,   320,   321,   241,
     322,     0,     0,     0,     0,     0,     0,   323,     0,     0,
       0,     0,     0,   324,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,   325,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,     0,     0,     0,     0,     0,     0,   203,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     153,   154,     0,     0,     0,   155,   156,   157,   233,   234,
     235,   236,   162,   163,   164,     0,     0,     0,     0,     0,
     165,   166,   167,   237,   238,   239,   240,   172,   320,   321,
     241,   322,     0,     0,     0,     0,     0,     0,   323,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,   487,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,     0,     0,     0,     0,     0,     0,   203,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,     0,     0,     0,   155,   156,   157,   233,
     234,   235,   236,   162,   163,   164,     0,     0,     0,     0,
       0,   165,   166,   167,   237,   238,   239,   240,   172,     0,
       0,   241,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,   242,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,     0,     0,     0,     0,     0,     0,   203,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,     0,     0,     0,   155,   156,   157,
     233,   234,   235,   236,   162,   163,   164,     0,     0,     0,
       0,     0,   165,   166,   167,   237,   238,   239,   240,   172,
       0,     0,   241,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,     0,     0,     0,     0,     0,     0,
     203,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,     0,     0,     0,     0,    14,     0,   108,
     109,    17,    18,     0,     0,     0,     0,     0,   110,   111,
     112,    22,    23,    24,    25,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   313,
       0,     0,   119,    52,     0,    53,    54,     0,     0,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,     0,     0,     0,     0,     0,     0,
       0,    14,   120,   108,   109,    17,    18,     0,     0,     0,
     612,     0,   110,   111,   112,    22,   851,   852,   853,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,    32,   965,    34,    35,    36,   855,    38,     0,
      39,    40,    41,     0,     0,   856,     0,     0,    43,    44,
       0,   261,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,   966,     0,     0,   119,    52,     0,   858,
      54,     0,   967,     0,   263,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    64,   265,    66,     0,
       0,     0,    14,   968,    15,    16,    17,    18,     0,     0,
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
     260,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,   261,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,   262,
       0,   263,   264,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,    64,   265,    66,    14,     0,    15,    16,
      17,    18,     0,     0,     0,     0,     0,    19,    20,    21,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,   260,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,   261,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,   509,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,   262,     0,   263,
     264,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     0,     0,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,    64,   265,    66,    14,     0,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,   260,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,   261,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      52,     0,    53,    54,     0,   725,     0,   263,   264,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,    64,
     265,    66,    14,     0,   108,   109,    17,    18,     0,     0,
       0,     0,     0,   110,   111,   112,    22,    23,    24,    25,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,    32,   260,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,   261,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,   873,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    52,     0,
      53,    54,     0,   725,     0,   263,   264,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,     0,     0,
       0,     0,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,     0,    64,   265,    66,
      14,     0,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,   260,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
     261,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    52,     0,    53,    54,
       0,   262,     0,   263,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     0,     0,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,    64,   265,    66,    14,     0,
     108,   109,    17,    18,     0,     0,     0,     0,     0,   110,
     111,   112,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
     260,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,   261,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,   263,   264,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,    64,   265,    66,    14,     0,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,   260,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,   261,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,   725,     0,   263,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     0,     0,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,    64,   265,    66,    14,     0,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,   260,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,   261,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      52,     0,    53,    54,     0,     0,     0,   263,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,    64,
     265,    66,    14,     0,    15,    16,    17,    18,     0,     0,
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
      10,    11,    12,     0,     0,     0,     0,    64,   265,    66,
      14,     0,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    52,     0,    53,    54,
       0,   262,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     0,     0,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,    64,   265,    66,    14,     0,
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
       0,     0,     0,    64,   265,    66,    14,     0,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,   919,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     0,     0,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,    64,   265,    66,    14,     0,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      52,     0,    53,    54,     0,   725,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,    64,
     265,    66,    14,     0,    15,    16,    17,    18,     0,     0,
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
      12,     0,     0,     0,     0,    64,   265,    66,    14,     0,
      15,    16,    17,    18,     0,     0,     0,     0,     0,    19,
      20,    21,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,  -442,    43,    44,     0,    45,    46,
      47,     0,     0,     0,  -626,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,  -442,  -442,
     211,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,     0,
    -520,  -520,  -520,  -520,  -520,  -520,  -520,  -520,  -520,  -520,
    -520,  -520,     0,    64,   265,    66,  -520,  -520,  -520,     0,
    -520,  -442,     0,     0,     0,     0,     0,     0,     0,  -520,
       0,     0,     0,  -520,     0,     0,     0,     0,  -438,     0,
       0,     0,     0,  -442,     0,     0,  -442,  -442,     0,  -520,
    -520,     0,  -520,  -520,  -442,  -520,  -520,  -520,  -520,  -520,
    -520,  -520,  -438,  -438,     0,     0,  -626,     0,  -520,  -442,
    -442,     0,  -626,     0,  -442,  -520,  -442,     0,     0,  -474,
       0,     0,  -474,     0,     0,  -474,     0,     0,     0,     0,
       0,     0,     0,     0,  -474,  -474,  -474,  -474,  -474,  -474,
    -474,  -474,  -474,  -474,  -474,  -474,     0,     0,     0,     0,
    -474,  -474,  -474,     0,     0,  -438,     0,     0,  -440,     0,
       0,     0,     0,  -474,     0,     0,     0,  -474,     0,  -474,
       0,     0,     0,     0,     0,     0,     0,  -438,     0,     0,
    -438,  -438,  -440,  -440,  -474,     0,  -474,  -474,  -438,  -474,
    -474,  -474,  -474,  -474,  -474,  -474,     0,     0,     0,     0,
       0,     0,  -474,  -438,  -438,     0,     0,     0,  -438,  -474,
    -438,     0,     0,     0,  -527,  -527,  -527,  -527,  -527,  -527,
    -527,  -527,  -527,  -527,  -527,  -527,     0,     0,     0,     0,
    -527,  -527,  -527,     0,     0,  -440,     0,     0,  -441,     0,
       0,     0,     0,  -527,     0,     0,     0,  -527,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -440,     0,     0,
    -440,  -440,  -441,  -441,  -527,     0,  -527,  -527,  -440,  -527,
    -527,  -527,  -527,  -527,  -527,  -527,     0,     0,     0,     0,
       0,     0,  -527,  -440,  -440,     0,     0,     0,  -440,  -527,
    -440,     0,     0,     0,  -528,  -528,  -528,  -528,  -528,  -528,
    -528,  -528,  -528,  -528,  -528,  -528,     0,     0,     0,     0,
    -528,  -528,  -528,     0,     0,  -441,     0,     0,  -437,     0,
       0,     0,     0,  -528,     0,     0,     0,  -528,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -441,     0,     0,
    -441,  -441,  -437,  -437,  -528,     0,  -528,  -528,  -441,  -528,
    -528,  -528,  -528,  -528,  -528,  -528,     0,     0,     0,     0,
       0,     0,  -528,  -441,  -441,     0,     0,     0,  -441,  -528,
    -441,     0,     0,     0,  -468,  -468,  -468,  -468,  -468,  -468,
    -468,  -468,  -468,  -468,  -468,  -468,     0,     0,     0,     0,
    -468,  -468,  -468,     0,     0,  -437,     0,     0,  -436,     0,
       0,     0,     0,  -468,     0,     0,     0,  -468,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -437,     0,     0,
    -437,  -437,  -436,  -436,  -468,     0,  -468,  -468,  -437,  -468,
    -468,  -468,  -468,  -468,  -468,  -468,     0,     0,     0,     0,
       0,     0,  -468,  -437,  -437,     0,     0,     0,  -437,  -468,
    -437,     0,     0,     0,  -467,  -467,  -467,  -467,  -467,  -467,
    -467,  -467,  -467,  -467,  -467,  -467,     0,     0,     0,     0,
    -467,  -467,  -467,     0,     0,  -436,     0,     0,     0,     0,
       0,     0,     0,  -467,     0,     0,     0,  -467,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -436,     0,     0,
    -436,  -436,     0,     0,  -467,     0,  -467,  -467,  -436,  -467,
    -467,  -467,  -467,  -467,  -467,  -467,     0,     0,     0,     0,
       0,     0,  -467,  -436,  -436,     0,     0,     0,  -436,  -467,
    -436,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,     0,     0,     0,     0,    14,     0,   108,
     109,    17,    18,     0,     0,     0,     0,     0,   110,   111,
     112,    22,    23,    24,    25,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,   114,
      34,    35,    36,   115,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   117,     0,     0,   118,
       0,     0,   119,    52,     0,    53,    54,     0,     0,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,     0,     0,     0,     0,     0,     0,
       0,    14,   120,   108,   109,    17,    18,     0,     0,     0,
       0,     0,   110,   111,   112,    22,    23,    24,    25,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,   225,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   226,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,     0,     0,
       0,     0,     0,     0,     0,    14,   120,   108,   109,    17,
      18,     0,     0,     0,     0,     0,   110,   111,   112,    22,
      23,    24,    25,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   313,     0,     0,
     400,    52,     0,    53,    54,     0,   401,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,     0,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,     0,     0,     0,     0,    14,
     120,   108,   109,    17,    18,     0,     0,     0,     0,     0,
     110,   111,   112,    22,   851,   852,   853,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,   854,    34,    35,    36,   855,    38,     0,    39,    40,
      41,     0,     0,   856,     0,     0,    43,    44,     0,   116,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   857,     0,     0,   119,    52,     0,   858,    54,     0,
    1092,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,     0,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,     0,     0,     0,     0,
       0,     0,     0,    14,   120,   108,   109,    17,    18,     0,
       0,     0,     0,     0,   110,   111,   112,    22,    23,    24,
      25,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,    32,   114,    34,    35,    36,   115,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,   119,    52,
       0,    53,    54,     0,     0,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,     0,     0,     0,     0,    14,   120,   108,
     109,    17,    18,     0,     0,     0,     0,     0,   110,   111,
     112,    22,    23,    24,    25,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   313,
       0,     0,   400,    52,     0,    53,    54,     0,     0,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,     0,     0,     0,     0,     0,     0,
       0,    14,   120,   108,   109,    17,    18,     0,     0,     0,
       0,     0,   110,   111,   112,    22,   851,   852,   853,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,    32,   854,    34,    35,    36,   855,    38,     0,
      39,    40,    41,     0,     0,   856,     0,     0,    43,    44,
       0,   116,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   857,     0,     0,   119,    52,     0,   858,
      54,     0,     0,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,     0,     0,
       0,     0,     0,     0,     0,    14,   120,   108,   109,    17,
      18,     0,     0,     0,     0,     0,   110,   111,   112,    22,
      23,    24,    25,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1002,     0,     0,
     119,    52,     0,    53,    54,     0,     0,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,     0,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,     0,     0,     0,     0,    14,
     120,   108,   109,    17,    18,     0,     0,     0,     0,     0,
     110,   111,   112,    22,    23,    24,    25,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,   225,
       0,     0,     0,     0,  -625,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1025,     0,     0,   119,    52,     0,    53,    54,     0,
       0,     0,     0,     0,    56,   496,    57,    58,    59,     0,
      60,    61,    62,     0,    63,     0,     0,     0,     0,     0,
    -516,  -516,  -516,  -516,  -516,  -516,  -516,  -516,  -516,  -516,
    -516,  -516,     0,     0,   120,     0,  -516,  -516,  -516,     0,
    -516,  -457,     0,     0,     0,     0,     0,     0,     0,  -516,
       0,  -625,     0,  -516,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -516,     0,     0,  -516,  -516,     0,  -516,
    -516,     0,  -516,  -516,  -457,  -516,  -516,  -516,  -516,  -516,
    -516,  -516,     0,     0,     0,     0,  -625,     0,  -516,  -457,
    -457,     0,  -625,     0,  -516,  -516,  -516,  -516,  -516,  -516,
    -516,  -516,  -516,  -516,  -516,  -516,  -516,  -516,  -516,     0,
       0,     0,     0,  -516,  -516,  -516,     0,  -516,     0,     0,
       0,     0,     0,     0,     0,     0,  -516,     0,     0,     0,
    -516,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -516,     0,     0,  -516,  -516,     0,  -516,  -516,     0,  -516,
    -516,  -516,  -516,  -516,  -516,  -516,  -516,  -516,  -516,     0,
     681,   652,     0,  -625,   682,  -516,  -455,  -455,     0,  -625,
       0,  -516,  -516,  -516,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   666,   661,     0,     0,
     667,     0,   203,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,   698,   652,     0,     0,   699,     0,   203,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   701,   661,
       0,     0,   702,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   708,   652,     0,     0,   709,     0,
     203,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
     711,   661,     0,     0,   712,     0,   203,   276,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   748,   652,     0,     0,
     749,     0,   203,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,   751,   661,     0,     0,   752,     0,   203,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   924,   652,
       0,     0,   925,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   927,   661,     0,     0,   928,     0,
     203,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
    1042,   652,     0,     0,  1043,     0,   203,   276,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   698,   652,     0,     0,
    1091,     0,   203,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,  1042,   652,     0,     0,  1107,     0,   203,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,  1118,   652,
       0,     0,  1119,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,  1121,   661,     0,     0,  1122,     0,
     203,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
     666,   661,     0,     0,   667,     0,   203,   276,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,     0,     0,     0,     0,
       0,     0,   203,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,     0,     0,     0,     0,  -302,
    -302,  -302,     0,   840,  -302,     0,     0,     0,     0,     0,
       0,     0,  -302,     0,     0,     0,  -302,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -302,     0,     0,  -302,
    -302,     0,  -109,  -302,     0,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,     0,     0,     0,     0,     0,
       0,  -302,  -302,  -302,     0,  -629,     0,  -302,  -302,  -302,
    -303,  -303,  -303,  -303,  -303,  -303,  -303,  -303,  -303,  -303,
    -303,  -303,     0,     0,     0,     0,  -303,  -303,  -303,     0,
     526,  -303,     0,     0,     0,     0,     0,     0,     0,  -303,
       0,     0,     0,  -303,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -303,     0,     0,  -303,  -303,     0,  -110,
    -303,     0,  -303,  -303,  -303,  -303,  -303,  -303,  -303,  -303,
    -303,  -303,     0,     0,     0,     0,     0,     0,  -303,  -303,
    -303,     0,     0,     0,  -303,  -303,  -303,   404,   405,   406,
     407,   408,   409,   410,   411,   412,   413,   414,   415,     0,
       0,     0,     0,   416,   417,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   420,     0,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,     0,
       0,     0,     0,     0,     0,     0,     0,  -277,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   413,   414,   415,
       0,     0,     0,     0,   416,   417,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   420,     0,
     421,   422,   423,   424,   425,   426,   427,   428,   429,   430,
       0,     0,     0,     0,     0,     0,     0,     0,  -279,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,     0,     0,     0,     0,   416,   417,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   420,
       0,   421,   422,   423,   424,   425,   426,   427,   428,   429,
     430,     0,     0,     0,     0,     0,     0,     0,     0,  -280,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,   415,     0,     0,     0,     0,   416,   417,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,     0,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,     0,     0,     0,     0,     0,     0,     0,     0,
    -282,   404,   405,   406,   407,   408,   409,   410,   411,   412,
     413,   414,   415,     0,     0,     0,     0,   416,   417,     0,
       0,     0,   418,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   420,     0,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,   415,     0,     0,     0,     0,   416,
     417,     0,     0,     0,   501,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   420,     0,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,     0,     0,     0,
       0,   416,   417,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,     0,   421,   422,   423,
     424,   425,   426,   427,   428,   429,   430,   404,   405,   406,
     407,   408,   409,   410,   411,   412,   413,  -656,  -656,     0,
       0,     0,     0,   416,   417,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430
};

static const yytype_int16 yycheck[] =
{
       1,    26,    55,    15,    16,    81,   285,    19,    14,     8,
       9,    88,    27,   490,   272,    14,   222,     9,     3,    20,
      15,    16,    14,     6,    19,    13,   481,     6,    21,   314,
      13,   597,     1,   118,     3,    51,   319,   379,   431,    27,
      68,   594,   435,    74,    27,   438,   763,    86,    87,   716,
     307,   547,    53,    54,   311,   433,   692,    25,    53,   138,
     696,   332,    68,  1010,   669,   670,   459,   703,    26,     4,
       5,    74,    73,    74,    57,    15,    16,    12,    57,    19,
     458,   474,   985,   476,    20,    21,   791,     1,   371,     3,
       4,     5,   485,    25,     8,     9,    26,   475,    12,   504,
      14,    15,    16,   508,   508,    19,   484,   403,    27,    25,
     111,   104,    79,    53,    54,   594,   612,   296,   117,   122,
      55,    59,    60,    61,    62,    25,     0,    26,    57,    55,
     443,   444,   525,  1041,    29,   539,    25,    51,    90,    25,
     333,    55,   374,   336,    25,   338,    81,   340,    25,   342,
     594,    65,    93,   597,    15,    16,   103,   550,    19,   126,
     810,   115,  1109,   395,   118,   119,   816,    81,   104,    57,
      18,   549,   144,    90,   142,   111,   112,    25,    90,   218,
     138,   128,   213,   214,    28,   126,   144,   270,   105,   272,
     229,    90,   121,   129,   148,   147,   115,   300,  1101,   118,
     119,   304,   755,   117,    92,   119,   105,    92,   138,   403,
     142,   214,   213,   214,    16,   768,   821,    90,   113,  1127,
     121,   400,    51,   306,   223,   224,   142,   146,   460,   148,
     147,   223,   224,   121,   388,   147,   390,   142,   314,   138,
      90,   140,   142,    90,   144,   144,   252,    90,   147,   443,
     444,   290,    18,   142,    20,   513,   142,    90,   144,   242,
      65,   142,   547,   242,   276,   142,    90,   279,   551,   270,
     298,   272,   105,   990,   147,   276,   755,   316,   306,   307,
     296,   276,   121,   311,   279,   284,   285,   558,   144,   768,
     119,    92,   298,    90,    15,    16,   210,   147,    19,   125,
     147,    20,  1007,   799,   147,  1010,    25,   140,   105,   223,
     224,   755,    55,   115,   147,   121,   118,   119,    90,    60,
     121,    51,    63,   147,   768,    55,   295,   612,   894,   734,
     735,   735,    53,    54,   303,    26,   142,    90,    92,   279,
     142,    90,   121,   140,   146,   981,   148,   791,   401,    25,
     147,   350,   351,   352,   353,    92,   355,   356,   350,   351,
     352,   353,   276,   556,   349,   279,   107,   121,    92,    90,
     284,   285,   457,  1040,   288,   147,   377,    90,   379,   314,
     349,   295,   296,    55,   400,   354,    92,   325,   142,   303,
    1129,  1130,   105,   848,   147,   691,   142,   121,   147,    90,
     314,    90,    20,   398,  1109,    57,   509,   812,   403,   888,
     803,   142,   805,   516,   105,   121,   105,  1156,   279,   898,
     138,   433,   101,  1162,  1163,   528,   804,   140,   145,   512,
     513,  1098,   433,   775,   147,   349,   350,   351,   352,   353,
     354,   355,   356,   121,    92,   724,   458,   138,   680,   140,
     894,   140,   435,  1089,   141,   438,   147,   458,   147,   288,
     374,  1128,   461,   475,   139,   466,   401,   296,    92,   545,
     728,   547,   484,   121,   475,    90,   459,   502,   581,   582,
     716,   395,    55,   484,   398,   101,   400,   401,   745,   403,
     105,    92,   142,   476,   807,    90,   101,   121,   493,   435,
     813,   814,   485,   101,   981,   511,   521,    57,   611,   121,
     105,   512,   513,   719,   799,   131,   132,   133,   142,   433,
     121,   522,    92,   459,    90,   140,   609,   806,  1007,   443,
     444,   142,   147,   521,   716,   121,   612,   549,   521,   105,
     476,   142,   525,    51,   458,   140,   460,   461,   549,   485,
     142,   121,   147,   522,   142,   276,    51,   471,   279,    92,
     792,   475,   531,  1007,   504,   479,  1010,   550,   910,   911,
     484,   400,   584,    92,   140,   489,   659,    92,   983,   983,
     142,   147,    92,   622,   142,   688,   121,   819,   121,   525,
      90,   603,    51,   398,   142,    26,   536,    62,   403,    64,
      65,   880,   121,    51,   605,   105,   121,   801,   522,   142,
     545,   121,   547,   807,   550,   640,   874,   531,    99,   813,
     814,    15,   858,   936,  1103,    13,   780,   781,   782,   121,
     784,   545,   786,   547,  1113,   549,   719,    92,   443,   444,
     140,   744,   471,   557,   144,   728,   121,   147,    16,   881,
     479,   116,   117,    63,   860,    73,    74,    92,   659,    90,
     489,   764,   145,   656,   665,  1109,   121,   668,   669,   670,
     584,    92,   665,    15,   105,   668,   858,   612,    58,    59,
     679,    62,   319,    64,    65,   686,   121,   679,   145,   603,
     691,   692,    37,    38,  1087,   696,   691,   139,   612,   714,
     121,   142,   703,   121,   122,   142,   689,   138,  1086,   140,
     689,    15,   433,   144,   713,   947,   147,   745,    58,    59,
     656,   713,   142,   799,    17,    18,   714,   728,   557,   665,
    1072,   714,   668,    15,   371,   116,   117,   458,    92,    92,
     817,   142,   936,   979,    44,   951,   121,    16,   684,   141,
     686,   957,   665,    26,   475,   668,   141,    51,    15,    53,
      54,    55,    56,   484,    90,   679,   680,   121,   121,    90,
     873,    40,    41,   686,   775,    69,   906,   907,    18,   105,
     141,   716,   141,   504,   105,    15,   889,   508,   142,   142,
     139,   874,   804,   139,   734,   213,   214,   979,   141,   713,
     139,   142,   716,   804,  1040,  1041,    51,    44,    53,    54,
      55,    56,   121,   122,   140,   536,   587,    90,   539,   140,
     821,   147,   805,  1102,    69,    57,   147,   142,   549,   142,
     831,   800,   105,   834,   142,   142,    44,   836,  1044,  1045,
     994,   995,   996,   997,   836,    15,   115,    93,   142,   118,
     119,    14,    55,    15,   443,   444,    15,   858,  1040,  1041,
     145,   142,  1098,   858,   799,   138,   142,   140,   142,   805,
     142,   144,   812,   874,   147,   142,    15,   146,   792,   148,
     141,    15,    15,    90,   139,   799,   800,   801,   477,   478,
     804,  1127,  1128,   807,   808,    15,    57,   142,   105,   813,
     814,   319,    55,   141,   115,   819,   820,   118,   119,   910,
     911,   142,    15,   142,   551,    51,  1098,   139,   858,   833,
     126,     1,   836,     3,    16,  1131,   142,   126,     8,     9,
     969,   845,   846,   140,    14,    15,    16,   526,   115,    19,
     147,   118,   119,   580,   858,  1127,  1128,    16,  1154,  1155,
      90,    55,  1106,   371,   115,   139,   870,   118,   119,    15,
     597,    55,   123,   600,   144,   105,   142,   881,   882,   142,
      16,    51,    15,   908,    90,   142,   975,   976,   913,    51,
     981,    15,   142,   975,   976,    65,   142,   148,  1027,   105,
     142,   820,   142,   587,   908,   142,   801,   591,   142,   913,
     140,   144,   807,   808,   833,   142,   141,   147,   813,   814,
      16,    15,    15,   734,   735,   950,   845,   846,   522,    12,
     791,     5,   936,   115,   140,  1089,   118,   119,  1053,   791,
      51,   147,   946,   947,    40,    41,   950,   117,  1101,   119,
     954,   870,   818,  1040,   979,  1046,   115,  1088,    69,   118,
     119,    90,   979,   882,   146,    26,   148,    37,    38,   254,
       6,   975,   976,   977,   791,   979,   105,   587,  1007,   115,
    1004,  1072,   118,   119,  1086,    -1,    -1,   146,    -1,   148,
     101,   102,   103,   804,    -1,  1086,    -1,  1088,  1089,    -1,
      -1,   812,   215,    51,    -1,    53,    54,    55,    56,   222,
     146,   140,   148,    -1,  1087,  1040,  1041,   128,   147,   115,
      -1,    69,   118,   119,    -1,    -1,  1030,   946,  1032,    90,
      -1,  1035,    -1,    -1,    -1,   954,  1040,  1041,  1129,  1130,
     210,   936,    -1,   551,   105,   115,   259,   858,   118,   119,
     146,    90,   148,   223,   224,  1144,  1145,    -1,   977,   101,
      90,  1087,  1144,  1145,   791,  1156,   105,    -1,    -1,    -1,
      90,  1162,  1163,  1098,    -1,   105,   146,   138,   148,   140,
      -1,    -1,  1086,   144,    -1,   105,   147,   129,   130,   131,
     132,   133,  1096,  1097,  1098,    26,    40,    41,    42,    43,
      44,   140,  1127,  1128,    -1,    -1,   276,   791,   147,   279,
     140,  1030,    -1,  1032,   284,   285,  1035,   147,   288,    -1,
     140,    -1,    -1,  1127,  1128,   295,   296,   147,   807,   808,
      -1,    -1,    -1,   303,   813,   814,   349,    -1,    -1,    -1,
    1144,  1145,  1146,  1004,    -1,    -1,  1007,    -1,    -1,  1010,
     363,  1012,    -1,    -1,  1158,  1159,    -1,    -1,    -1,    90,
     839,   840,    61,   842,   843,    64,    65,   894,   381,   896,
      -1,    -1,   983,   900,   105,    -1,    -1,  1096,  1097,   349,
     350,   351,   352,   353,   354,   355,   356,  1004,    61,    -1,
    1007,    64,    65,  1010,    -1,  1012,    -1,    -1,    -1,     1,
      -1,     3,     4,     5,   374,    -1,    -1,   138,    -1,   140,
      12,    -1,  1073,   144,    -1,    -1,   147,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,   395,    -1,  1146,   398,    -1,
     400,    -1,    -1,   403,    -1,   915,   916,    -1,   115,  1158,
    1159,   118,   119,   116,   117,    -1,    -1,    -1,  1109,    51,
    1111,    -1,  1113,    55,  1115,    -1,  1073,   936,    63,    64,
      65,   988,   989,   433,    -1,   142,    -1,   480,   481,   146,
      -1,   148,    -1,   443,   444,  1086,    63,    64,    65,    81,
     959,    -1,    -1,  1010,    -1,  1012,    -1,  1148,   458,    -1,
     460,   461,  1109,    -1,  1111,    -1,  1113,    -1,  1115,    -1,
      -1,   471,    63,    64,    65,   475,    -1,    -1,    -1,   479,
      -1,   116,   117,    -1,   484,    -1,    -1,   119,   531,   489,
    1004,    -1,    -1,  1007,    -1,   538,  1010,  1007,  1012,   116,
     117,  1148,    -1,  1060,    -1,   115,  1063,    -1,   118,   119,
      -1,    -1,    51,    -1,    53,    54,    55,    56,    -1,    -1,
     115,    -1,   522,   118,   119,   116,   117,    63,    64,    65,
      69,   531,    63,    64,    65,   145,   146,    -1,   148,    -1,
      63,    64,    65,    -1,    83,    -1,    -1,    -1,  1105,   549,
      -1,   146,  1109,   148,  1111,    94,    -1,   557,  1115,  1073,
      -1,   100,   101,   102,   103,    -1,  1076,  1077,  1078,    -1,
    1080,  1081,    51,    -1,    53,    54,    55,    56,   210,    -1,
     116,   117,   121,    -1,   584,   116,   117,    -1,    -1,   128,
      69,  1148,   131,   116,   117,  1109,    -1,  1111,    -1,  1113,
      -1,  1115,    -1,   603,    83,   144,    -1,    51,    -1,    53,
      54,    55,    56,    -1,    -1,    94,    63,    64,    65,    -1,
      -1,   100,   101,   102,   103,    69,  1136,  1137,  1138,  1139,
      -1,    -1,    -1,    -1,  1148,   678,    -1,    -1,    -1,    -1,
       1,    -1,     3,     4,     5,    -1,    -1,  1157,    -1,   128,
      94,    12,   131,    -1,    -1,    -1,   288,   101,   102,   103,
      -1,    -1,    -1,   295,   296,   144,    -1,    44,    -1,   116,
     117,   303,    63,    64,    65,    -1,   719,    -1,    -1,   679,
     680,    -1,   314,   115,   128,    -1,   118,   119,    -1,    -1,
      51,    -1,    -1,    -1,    55,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,   713,   146,    -1,   148,   349,    -1,    -1,
      81,    -1,   354,    -1,   101,   116,   117,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   777,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   374,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   119,    -1,
      -1,    -1,    -1,   395,    -1,   142,    -1,    -1,   400,   401,
      -1,   403,    -1,    51,    -1,    53,    54,    55,    56,    -1,
     823,    -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,
      -1,    69,   792,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     800,   801,    69,    -1,   804,   848,    -1,   807,   808,    -1,
      -1,   443,   444,   813,   814,    -1,    94,   860,    -1,   819,
     820,    -1,   100,   101,   102,   103,    -1,    94,   460,    -1,
      -1,    -1,    -1,   833,    -1,    -1,   836,    -1,    -1,   471,
      -1,    -1,    88,    89,    -1,   845,   846,   479,    -1,   210,
     128,     0,    -1,   131,    -1,   101,    -1,   489,    -1,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
     870,    20,    -1,    -1,    -1,    -1,    25,    26,    27,    -1,
      -1,   881,   882,   129,   130,   131,   132,   133,    37,    38,
     522,    40,    41,    42,    43,    44,    -1,    -1,    -1,   531,
      51,    -1,    53,    54,    55,    56,    -1,    -1,   951,    -1,
      -1,    -1,    -1,   545,   957,   547,    -1,    -1,    69,    51,
      -1,    53,    54,    55,    56,   557,    -1,   288,    -1,    -1,
      -1,    -1,    -1,    -1,   295,   296,   936,    69,    -1,    -1,
      -1,    90,   303,    94,    -1,    -1,   946,   947,    -1,   100,
      -1,    -1,    -1,   314,   954,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    94,    -1,    -1,    -1,   115,    -1,   100,   118,
     119,    -1,    -1,    -1,    -1,   975,   976,   977,    -1,    -1,
     612,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   349,   138,
     139,    -1,    -1,   354,    -1,   144,   145,   146,   147,   148,
      -1,  1044,  1045,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    89,   374,    -1,    51,    -1,    53,    54,    55,
      56,    -1,    -1,    -1,   101,    -1,    -1,    -1,    88,    89,
    1030,    -1,  1032,    69,   395,  1035,    -1,    -1,    -1,   400,
     401,   101,   403,    -1,    -1,    -1,    -1,    -1,   680,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    94,    -1,
      -1,    -1,    -1,    -1,   100,   101,   102,   103,   128,   129,
     130,   131,   132,   133,    -1,    -1,     1,    -1,     3,     4,
       5,    -1,   443,   444,   716,    -1,  1086,    12,  1131,    -1,
      -1,    -1,   128,    -1,    -1,   131,  1096,  1097,    -1,   460,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,
     471,  1154,  1155,    -1,    -1,    -1,    -1,    -1,   479,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    -1,   489,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1144,  1145,  1146,    -1,    51,    -1,
      53,    54,    55,    56,    -1,    -1,    81,    -1,  1158,  1159,
     792,   522,    -1,    -1,    -1,    -1,    69,   799,   800,   801,
     531,    -1,    -1,    -1,    -1,   807,    -1,    -1,    -1,    -1,
      -1,   813,   814,    -1,   545,    -1,   547,   819,   820,    -1,
      -1,    94,    -1,    -1,   119,    -1,   557,   100,   101,   102,
     103,   833,    -1,    51,    -1,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,   845,   846,    -1,    -1,    -1,    -1,    -1,
      -1,    69,    -1,    -1,    -1,   128,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,   870,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    94,    -1,    -1,   881,
     882,   612,   100,   101,   102,   103,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   908,    -1,    -1,    -1,
     128,   913,    -1,   131,    -1,   210,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,
      -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   946,   947,    -1,    -1,   950,   680,
      -1,    -1,   954,    -1,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,   977,    -1,   979,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   716,   101,    -1,    -1,    -1,
      -1,    -1,    -1,   288,    -1,    -1,    -1,    -1,    -1,    -1,
     295,   296,    -1,    -1,    -1,    -1,    -1,   122,   303,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   314,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1030,    -1,
    1032,    -1,    -1,  1035,    -1,    -1,    -1,    -1,  1040,  1041,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   349,    -1,    -1,    -1,    -1,   354,
      -1,   792,    -1,    -1,    -1,    -1,    -1,    -1,   799,   800,
     801,    -1,    -1,    -1,    -1,    -1,   807,    -1,    -1,   374,
      -1,    -1,   813,   814,    -1,    -1,    -1,    -1,   819,   820,
      -1,    -1,    -1,    -1,  1096,  1097,  1098,    -1,    -1,    -1,
     395,    -1,   833,    -1,    -1,   400,   401,    -1,   403,    -1,
      -1,    -1,    -1,    -1,   845,   846,    -1,    -1,    51,    -1,
      53,    54,    55,    56,    -1,  1127,  1128,    -1,    -1,    51,
      -1,    53,    54,    55,    56,    -1,    69,    -1,    -1,   870,
      -1,    -1,    -1,    -1,  1146,    -1,    -1,    69,   443,   444,
     881,   882,    85,    -1,    -1,    -1,  1158,  1159,    -1,    -1,
      -1,    94,    -1,    85,    -1,   460,    -1,   100,   101,   102,
     103,    -1,    94,    -1,    -1,    -1,   471,   908,   100,   101,
     102,   103,   913,    -1,   479,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   489,   128,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,   936,   128,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,   946,   947,    -1,    -1,   950,
      -1,    -1,    -1,   954,    -1,    -1,     1,   522,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,   531,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   977,    -1,   979,    -1,
     545,    -1,   547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,    -1,     1,    -1,     3,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    12,    -1,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1030,
      -1,  1032,    -1,    -1,  1035,    -1,    81,    -1,    -1,  1040,
    1041,    -1,    -1,    -1,    -1,    -1,    51,   612,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    51,    52,    -1,    -1,
      55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   119,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    88,    89,  1096,  1097,  1098,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   680,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1127,  1128,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    -1,    -1,    -1,  1146,    -1,    -1,   143,   144,
      -1,   716,    -1,    -1,    -1,    -1,    -1,  1158,  1159,    72,
      73,    74,    75,    76,    77,   210,    -1,    80,    81,    -1,
      -1,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   210,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,    -1,    -1,    -1,    -1,   792,    -1,    -1,
      -1,    -1,    -1,    -1,   799,   800,   801,    -1,    -1,    -1,
      -1,    -1,   807,   288,    -1,    -1,    -1,    -1,   813,   814,
     295,   296,    -1,    -1,   819,   820,    -1,    -1,   303,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   833,   314,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     845,   846,    -1,   288,    -1,    -1,    -1,    -1,    -1,    -1,
     295,   296,    -1,    -1,    -1,    -1,    -1,    -1,   303,    -1,
      -1,    -1,    -1,    -1,   349,   870,    -1,    -1,    -1,   354,
      -1,    -1,    -1,    -1,    -1,    -1,   881,   882,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   908,   349,    -1,    -1,    -1,   913,   354,
     395,    -1,    -1,    -1,    -1,   400,   401,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   374,
      -1,   936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   946,   947,    -1,    -1,   950,    -1,    -1,    -1,   954,
     395,     1,    -1,     3,    -1,   400,    -1,    -1,   403,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   977,    -1,   979,   460,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   471,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   479,    -1,    -1,    -1,   443,   444,
      -1,    51,    -1,    -1,   489,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   460,    -1,    -1,    -1,    44,
      -1,    -1,    -1,    -1,    -1,  1030,   471,  1032,    -1,    -1,
    1035,    -1,    -1,    -1,   479,  1040,  1041,   522,    -1,    -1,
      -1,    -1,    -1,    -1,   489,    -1,   531,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
     545,    -1,   547,    88,    89,    -1,    -1,    -1,    -1,   119,
      -1,    -1,   557,    -1,    -1,    -1,   101,   522,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   531,    -1,    -1,    -1,
      -1,  1096,  1097,  1098,    -1,    -1,    -1,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,    -1,   557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1127,  1128,    -1,    -1,    -1,   612,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1158,  1159,    -1,    -1,    -1,    -1,    -1,
     210,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,   680,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,   716,    -1,    66,    67,   680,    69,    -1,   288,    -1,
      -1,    -1,    -1,    -1,    -1,   295,   296,    -1,    -1,    -1,
      -1,    -1,    -1,   303,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   349,
      -1,   134,    -1,    -1,   354,    -1,    -1,   792,    -1,   142,
      -1,    -1,    -1,    -1,   799,   800,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   374,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   819,   820,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   395,    -1,   792,   833,    -1,
     400,    -1,    -1,   403,    -1,   800,   801,    -1,    -1,    -1,
     845,   846,   807,    -1,    -1,    -1,    -1,    -1,   813,   814,
      -1,    -1,    -1,    -1,   819,   820,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   870,    -1,    -1,   833,    -1,
      -1,    -1,    -1,   443,   444,    -1,   881,   882,    -1,    -1,
     845,   846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     460,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   471,    -1,   908,    -1,   870,    -1,    -1,   913,   479,
      -1,    -1,    -1,    -1,    -1,    -1,   881,   882,    -1,   489,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   946,   947,    -1,    -1,   950,    -1,    -1,    -1,   954,
      -1,    -1,   522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   531,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   936,   977,    -1,   979,    -1,    -1,    -1,    -1,    -1,
      -1,   946,   947,    -1,    -1,   950,    -1,   557,    -1,   954,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,
      -1,    -1,   977,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,    -1,    -1,  1030,    -1,  1032,    -1,    -1,
    1035,    -1,    -1,    -1,    -1,  1040,  1041,    -1,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  1030,    -1,  1032,    -1,    -1,
    1035,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     8,
       9,    -1,    -1,    -1,    -1,    14,    15,    16,    -1,    -1,
      19,  1096,  1097,  1098,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     680,    -1,    -1,    -1,    -1,    -1,    -1,    46,    47,    48,
      49,    -1,  1127,  1128,    53,    54,    40,    41,    -1,    -1,
      -1,  1096,  1097,    -1,    -1,    -1,    65,    66,    -1,    -1,
      -1,  1146,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1158,  1159,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    90,    -1,    -1,    93,
      -1,  1146,    -1,    -1,    -1,    -1,    -1,   101,   117,    -1,
      -1,   105,    -1,  1158,  1159,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,   792,    -1,    -1,    -1,   140,   141,   142,    -1,
     800,   801,   146,   147,   148,    -1,    -1,   807,    -1,    -1,
      -1,    -1,    -1,   813,   814,    -1,    -1,    -1,    -1,   819,
     820,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   833,    72,    73,    74,    75,    76,    77,
      78,    -1,    80,    81,    -1,   845,   846,    -1,    -1,    -1,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   223,   224,    -1,    -1,    -1,    -1,
     870,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   881,   882,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   262,   263,   264,   265,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   276,    -1,    -1,
     279,    -1,    -1,    -1,    -1,   284,   285,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   946,   947,    -1,    -1,
      -1,    -1,    -1,    -1,   954,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   977,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   350,   351,   352,   353,    -1,   355,   356,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   375,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   386,    -1,    -1,
    1030,    -1,  1032,    -1,    -1,  1035,    -1,    -1,    -1,   398,
      -1,    -1,    -1,    -1,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,   416,   417,    -1,
     419,   420,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,    -1,    -1,   433,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   443,   444,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  1096,  1097,    -1,   458,
      -1,    -1,   461,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   473,    -1,   475,    -1,   477,   478,
      -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   493,    -1,    -1,   496,   497,    -1,
      -1,    -1,   501,    -1,    -1,   504,  1146,   506,    -1,   508,
     509,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1158,  1159,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   526,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   536,    -1,    -1,
     539,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     549,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,   566,   567,    -1,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   584,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   603,    -1,    -1,   606,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    25,    -1,    27,    28,    29,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     679,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,   691,    -1,    -1,   694,   695,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,   713,    -1,    88,    89,    90,    -1,
      -1,    93,    -1,    -1,    -1,    -1,   725,    99,    -1,   101,
      -1,    -1,    -1,   105,    -1,   734,   735,    -1,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,   139,   140,   141,
     142,    -1,    -1,   145,   146,   147,   148,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   796,    -1,    -1,
      -1,    -1,   801,   802,    -1,   804,    -1,    -1,   807,   808,
      -1,    -1,    -1,   812,   813,   814,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   836,    -1,    -1,
     839,   840,    -1,   842,   843,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   858,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     869,    -1,    -1,    -1,   873,    -1,    -1,    51,    52,    -1,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   890,   891,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,   903,   904,    80,    81,    -1,    -1,
      -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
     919,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,
     929,   930,    -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     959,   135,   136,    -1,    -1,    -1,    -1,    -1,   967,   143,
     144,    -1,    -1,    -1,    -1,    -1,   975,   976,    -1,    -1,
      -1,    -1,     0,     1,   983,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,    -1,    -1,   104,  1086,   106,   107,
     108,    -1,   110,   111,   112,     0,   114,   115,    -1,    -1,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,    -1,    20,   134,   135,   136,    -1,
      25,    26,    27,    28,    -1,    -1,    -1,    -1,   146,    -1,
     148,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,  1144,  1145,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    90,    -1,    -1,    93,    -1,
      -1,    -1,    -1,    -1,    99,    -1,   101,    -1,    -1,    -1,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,   118,   119,    -1,    -1,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,     0,    -1,   138,   139,   140,   141,   142,    -1,   144,
     145,   146,   147,   148,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    25,    -1,    27,    28,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    90,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,
      99,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,
     119,    -1,   121,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,
     139,   140,   141,   142,    -1,    -1,   145,   146,   147,   148,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    -1,
      -1,    -1,    25,    26,    27,    28,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    90,    -1,    -1,    93,    -1,    -1,    -1,
      -1,    -1,    99,    -1,   101,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,
      -1,   118,   119,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,     0,
      -1,    -1,   139,   140,   141,   142,    -1,   144,   145,   146,
     147,   148,    13,    14,    15,    -1,    17,    18,    -1,    20,
      -1,    -1,    -1,    -1,    25,    26,    -1,    -1,    -1,    -1,
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
     131,   132,   133,    -1,    -1,     0,    -1,   138,   139,   140,
      -1,   142,    -1,    -1,   145,   146,   147,   148,    13,    14,
      15,    -1,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,
      25,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    90,    -1,    92,    93,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,   118,   119,    -1,   121,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,     0,    -1,   138,   139,   140,    -1,   142,    -1,    -1,
     145,   146,   147,   148,    13,    14,    15,    -1,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    25,    -1,    -1,    -1,
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
     129,   130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,
     139,   140,    -1,   142,    -1,    -1,   145,   146,   147,   148,
      13,    14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,
      -1,    -1,    25,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     133,    -1,    -1,    -1,    -1,    -1,   139,   140,    -1,   142,
      -1,    -1,   145,   146,   147,   148,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    -1,    -1,    18,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
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
       9,    10,    11,    12,    -1,    -1,    15,    -1,    17,    18,
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
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,     1,   148,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    -1,    -1,    18,    19,    20,    21,    22,
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
      -1,    18,    19,    -1,    21,    22,    23,    24,    25,    -1,
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
      11,    12,    -1,    -1,    15,    -1,    -1,    18,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,   135,   136,    -1,    -1,   139,    -1,    -1,
      -1,    -1,    -1,    -1,   146,     1,   148,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    14,    15,
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
      -1,    -1,   118,   119,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,   134,   135,
     136,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
     146,    -1,   148,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,   115,    -1,
      -1,   118,   119,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,   134,   135,   136,
      -1,    19,    -1,    21,    22,    23,    24,    -1,   145,   146,
      -1,   148,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,   115,    -1,    -1,
     118,   119,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,   134,   135,   136,    -1,
      19,    -1,    21,    22,    23,    24,    -1,   145,   146,    -1,
     148,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,   115,    -1,    -1,   118,
     119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,
     139,    -1,    -1,    -1,    -1,    -1,    -1,   146,     1,   148,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    15,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,   134,   135,   136,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,   146,    -1,   148,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,   115,    -1,    -1,   118,   119,    -1,    -1,     3,     4,
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
     136,    -1,    -1,    -1,    -1,   141,   142,    -1,    -1,    -1,
     146,    -1,   148,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,   115,    -1,    -1,   118,   119,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,   134,   135,   136,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,   146,    -1,   148,    30,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   148,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    72,    73,    74,    75,
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,   102,   103,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,   135,   136,    -1,   138,    -1,    -1,    -1,    -1,    -1,
     144,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    26,    -1,    -1,    -1,    30,    31,
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
      -1,    -1,   134,   135,   136,    19,   138,    21,    22,    23,
      24,    -1,   144,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    92,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,   102,   103,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,   121,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
     134,   135,   136,    -1,    19,    -1,    21,    22,    23,    24,
     144,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
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
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,   102,   103,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,
     136,    19,    -1,    21,    22,    23,    24,    -1,   144,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,   144,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,
      -1,    -1,    -1,    69,    70,    71,    72,    73,    74,    75,
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
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    63,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,
      -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,   106,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,
     143,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
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
     142,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,   100,    -1,   102,    -1,   104,    -1,   106,   107,
     108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,   134,   135,   136,    -1,
      -1,    -1,    19,   141,    21,    22,    23,    24,    -1,    -1,
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
      -1,    -1,    63,    -1,    16,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    40,    41,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,   134,   135,   136,    88,    89,    90,    -1,
      92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    16,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    40,    41,    -1,    -1,   138,    -1,   140,   141,
     142,    -1,   144,    -1,   146,   147,   148,    -1,    -1,    57,
      -1,    -1,    60,    -1,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    -1,    93,    -1,    -1,    16,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,   107,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    40,    41,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,   140,   141,   142,    -1,    -1,    -1,   146,   147,
     148,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    -1,    93,    -1,    -1,    16,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    40,    41,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,   140,   141,   142,    -1,    -1,    -1,   146,   147,
     148,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    -1,    93,    -1,    -1,    16,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    40,    41,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,   140,   141,   142,    -1,    -1,    -1,   146,   147,
     148,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    -1,    93,    -1,    -1,    16,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    40,    41,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,   140,   141,   142,    -1,    -1,    -1,   146,   147,
     148,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    90,    -1,    -1,    93,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,   140,   141,   142,    -1,    -1,    -1,   146,   147,
     148,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,
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
      98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,
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
      94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,
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
      -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,
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
      -1,    -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
      -1,    -1,    -1,    -1,   104,    57,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,   134,    -1,    88,    89,    90,    -1,
      92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    26,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,   138,    -1,   140,   141,
     142,    -1,   144,    -1,   146,   147,   148,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    90,    -1,    92,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,   118,   119,    -1,   121,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      51,    52,    -1,   138,    55,   140,   141,   142,    -1,   144,
      -1,   146,   147,   148,    -1,    -1,    -1,    -1,    -1,    70,
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
     131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,   143,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    90,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,
     119,    -1,   121,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,   140,   141,   142,    -1,   144,    -1,   146,   147,   148,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,
      92,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,   140,   141,
     142,    -1,    -1,    -1,   146,   147,   148,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,
      -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133
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
     160,   161,   163,   164,   166,   168,   169,   172,   173,   175,
     176,   177,   179,   180,   189,   203,   220,   241,   242,   262,
     263,   264,   268,   269,   270,   276,   277,   278,   280,   281,
     282,   283,   284,   285,   321,   334,     0,   154,    21,    22,
      30,    31,    32,    39,    51,    55,    69,    88,    91,    94,
     134,   164,   166,   181,   182,   203,   220,   282,   285,   321,
     182,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    55,    70,    71,    72,    73,    74,    75,
      76,    77,    80,    81,    86,    87,    88,    89,   100,   101,
     102,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   135,   136,   143,   144,   183,   187,   188,   284,   316,
     204,    91,   163,   164,   166,   167,   180,   189,   220,   282,
     283,   285,   167,   210,   212,    69,    91,   173,   180,   220,
     225,   282,   285,    33,    34,    35,    36,    48,    49,    50,
      51,    55,   106,   183,   184,   185,   278,   115,   118,   119,
     146,   148,   167,   272,   273,   274,   327,   331,   332,   333,
      51,    69,   100,   102,   103,   135,   172,   189,   195,   198,
     201,   264,   319,   320,   195,   195,   144,   192,   193,   196,
     197,   334,   192,   197,   144,   328,   184,   155,   138,   189,
     220,   189,   189,   189,    55,     1,    94,   157,   158,   160,
     174,   175,   334,   205,   207,   190,   201,   319,   334,   189,
     318,   319,   334,    91,   142,   179,   220,   282,   285,   208,
      53,    54,    56,    63,    69,   107,   183,   279,    63,    64,
      65,   116,   117,   265,   266,    61,   265,    62,   265,    63,
     265,    63,   265,    58,    59,   168,   189,   189,   327,   333,
      40,    41,    42,    43,    44,    37,    38,    51,    53,    54,
      55,    56,    69,    83,    94,   100,   101,   102,   103,   128,
     131,   144,   288,   289,   290,   291,   292,   295,   296,   297,
     298,   300,   301,   302,   303,   305,   306,   307,   310,   311,
     312,   313,   314,   334,   288,   290,    28,   240,   121,   142,
      94,   100,   176,   121,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    88,    89,    93,   101,
     122,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    90,   105,   140,   147,   325,    90,   325,   326,    26,
     138,   244,   264,    92,    92,   192,   197,   244,   163,    51,
      55,   181,    58,    59,   289,   125,   286,    90,   140,   325,
     219,   317,    90,   147,   324,   156,   157,    55,   288,   288,
      16,   221,   331,   121,    90,   140,   325,    92,    92,   221,
     167,   167,    55,    90,   140,   325,    25,   107,   142,   275,
     327,   115,   274,    20,   246,   331,    57,    57,   189,   189,
     189,    93,   142,   199,   200,   334,    57,   199,   200,    85,
     194,   195,   201,   319,   334,   195,   163,   327,   329,   163,
     332,   159,   138,   157,    90,   325,    92,   160,   174,   145,
     327,   333,   329,   160,   329,   141,   200,   330,   333,   200,
     330,   139,   330,    55,   176,   177,   178,   142,    90,   140,
     325,   144,   237,   300,   305,    63,   265,   267,   271,   272,
      63,   266,    61,    62,    63,    63,   101,   101,   154,   167,
     167,   167,   167,   160,   163,   163,    57,   121,    57,   331,
     304,    85,   300,   305,   121,   156,   189,   142,   315,   334,
      51,   142,   315,   331,   142,   299,   189,   142,   299,    51,
     142,   299,    51,   121,   156,   239,   100,   168,   189,   201,
     202,   174,   142,   179,   142,   161,   162,   168,   180,   189,
     191,   202,   220,   285,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,    51,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,    51,    52,    55,   187,   192,   322,   323,   194,   201,
      51,    52,    55,   187,   192,   322,    51,    55,   322,   245,
     243,   162,   189,   191,   162,   191,    99,   171,   217,   287,
     216,    51,    55,   181,   322,   194,   322,   156,   163,   165,
      15,    13,   258,   334,   121,   121,   157,    16,    51,    55,
     194,    51,    55,   157,    27,   222,   331,   222,    51,    55,
     194,    51,    55,   214,   186,   157,    25,   246,   189,   201,
      15,   189,   189,   189,   328,   100,   189,   198,   319,   189,
     320,   329,   145,   327,   200,   200,   329,   145,   184,   152,
     139,   191,   329,   160,   206,   319,   176,   178,    51,    55,
     194,    51,    55,   300,   209,   142,    63,   157,   272,   189,
     189,    51,    69,   100,   226,   305,   329,   329,   142,   172,
     189,    15,    51,    69,   292,   297,   314,    85,   298,   303,
     310,   312,   305,   307,   312,    51,   305,   172,   189,    15,
      79,   126,   231,   233,   334,   189,   200,   329,   178,   142,
      44,   121,    44,    90,   140,   325,   328,    92,    92,   192,
     197,   141,   200,    92,    92,   193,   197,   193,   197,   231,
     231,   170,   331,   167,   156,   141,    15,   329,   183,   189,
     202,   259,   334,    18,   224,   334,    17,   223,   224,    92,
      92,   141,    92,    92,   224,   211,   213,   141,   167,   184,
     139,    34,    35,    36,    51,    55,    63,    91,    97,   220,
     249,   250,   251,   252,   253,   257,   277,   281,    15,   200,
     221,   189,   199,    85,   319,   139,   329,   330,   141,   234,
     328,    29,   113,   238,   139,   142,   302,   329,   142,    85,
      44,    44,   315,   331,   142,   299,   142,   299,   142,   299,
     142,   299,   299,    44,    44,   228,   230,   232,   291,   293,
     294,   297,   305,   306,   308,   309,   312,   314,   156,   100,
     189,   178,   160,   189,    51,    55,   194,    51,    55,    57,
     123,   162,   191,   168,   191,   171,    92,   162,   191,   162,
     191,   171,   244,   240,   156,   157,   231,   218,   331,    15,
      93,   260,   334,   157,    14,   261,   334,   167,    15,    92,
      15,   157,   157,   222,    55,    51,    91,   100,   141,   220,
     249,   254,   255,   256,    90,    40,    41,   221,    93,   126,
     189,   157,   329,   200,   145,   146,   156,   157,   227,   142,
     100,   329,   189,   189,   305,   312,   305,   305,   189,   189,
     234,   234,    91,   220,   142,   315,   315,   142,   229,   220,
     142,   229,   142,   229,    15,   189,   141,   189,   189,   162,
     191,    15,   139,   157,   156,    91,   180,   220,   282,   285,
     221,   157,   221,    15,    15,   215,    55,    51,    90,   141,
     142,   142,    51,    55,   167,   167,   157,    51,   252,   224,
     246,   247,    51,   235,   236,   301,    15,   139,   305,   305,
     142,   302,   299,   142,   299,   299,   299,   126,   126,    55,
      90,   293,   297,   142,   228,   229,   309,   312,   305,   308,
     312,   305,   139,    15,    55,    90,   140,   325,   157,   157,
     157,    55,   100,   249,   256,   255,   221,   221,    25,   224,
     248,   142,   328,   142,   305,   142,   305,    55,   315,   142,
     229,   142,   229,   142,   229,   142,   229,   229,    51,    55,
     194,    51,    55,   258,   223,    15,    51,   142,   142,   157,
     157,   249,    15,   236,   305,   299,   305,   312,   305,   305,
     141,   255,   248,   248,    40,    41,   221,   229,   142,   229,
     229,   229,    15,    15,   167,   167,   157,   305,   221,   221,
     248,   229,   157,   157,   248,   248
};

/* YYR1[RULE-NUM] -- Symbol kind of the left-hand side of rule RULE-NUM.  */
static const yytype_int16 yyr1[] =
{
       0,   149,   150,   151,   152,   153,   153,   153,   153,   154,
     155,   154,   156,   157,   158,   158,   158,   158,   159,   160,
     160,   160,   160,   160,   160,   160,   160,   160,   160,   160,
     160,   160,   160,   160,   161,   161,   161,   161,   161,   161,
     161,   161,   161,   161,   161,   161,   162,   162,   162,   163,
     163,   163,   163,   163,   163,   164,   165,   166,   167,   168,
     168,   169,   169,   170,   171,   172,   172,   172,   172,   172,
     172,   172,   172,   172,   172,   172,   173,   173,   174,   174,
     175,   175,   175,   175,   175,   175,   175,   175,   175,   175,
     176,   176,   177,   177,   178,   178,   179,   179,   179,   179,
     179,   179,   179,   179,   180,   180,   180,   180,   180,   180,
     180,   180,   180,   181,   181,   182,   182,   182,   183,   183,
     183,   183,   183,   184,   184,   185,   186,   185,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   190,
     190,   190,   190,   191,   191,   192,   192,   192,   193,   193,
     194,   194,   194,   194,   194,   195,   195,   195,   195,   195,
     196,   197,   198,   198,   199,   199,   200,   201,   201,   201,
     201,   201,   201,   202,   202,   202,   203,   203,   203,   203,
     203,   203,   203,   203,   204,   203,   205,   206,   203,   207,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   208,   209,   203,   203,   203,   210,   211,
     203,   212,   213,   203,   203,   203,   203,   203,   203,   214,
     215,   203,   216,   203,   217,   218,   203,   219,   203,   203,
     203,   203,   203,   203,   203,   220,   221,   221,   221,   222,
     222,   223,   223,   224,   224,   225,   225,   226,   226,   226,
     226,   226,   226,   226,   226,   227,   226,   228,   228,   228,
     228,   229,   229,   230,   230,   230,   230,   230,   230,   230,
     230,   230,   230,   230,   230,   230,   230,   230,   231,   231,
     232,   233,   233,   233,   234,   234,   235,   235,   236,   236,
     237,   237,   238,   238,   239,   240,   241,   241,   241,   241,
     242,   242,   242,   242,   242,   242,   242,   242,   242,   243,
     244,   245,   244,   246,   247,   247,   248,   248,   248,   248,
     249,   250,   250,   251,   251,   252,   252,   252,   252,   252,
     252,   252,   252,   252,   252,   252,   253,   253,   254,   254,
     254,   254,   254,   255,   255,   256,   256,   257,   258,   258,
     259,   259,   259,   260,   260,   261,   261,   262,   262,   262,
     262,   263,   263,   264,   264,   264,   264,   265,   265,   266,
     267,   266,   266,   266,   268,   268,   269,   269,   270,   271,
     271,   272,   272,   273,   273,   274,   275,   274,   276,   276,
     277,   277,   277,   278,   279,   279,   279,   279,   279,   279,
     280,   280,   281,   281,   281,   281,   282,   282,   282,   282,
     282,   283,   283,   284,   284,   284,   284,   284,   284,   284,
     284,   284,   285,   285,   286,   287,   286,   288,   288,   289,
     289,   289,   290,   290,   290,   290,   291,   291,   292,   292,
     293,   293,   294,   294,   295,   295,   296,   296,   297,   297,
     298,   298,   298,   298,   299,   299,   300,   300,   300,   300,
     300,   300,   300,   300,   300,   300,   300,   300,   300,   300,
     300,   301,   301,   301,   301,   301,   302,   302,   303,   304,
     303,   305,   305,   306,   307,   308,   309,   309,   310,   310,
     311,   311,   312,   312,   313,   313,   314,   314,   315,   315,
     316,   317,   316,   318,   318,   319,   319,   320,   320,   320,
     320,   320,   320,   320,   320,   321,   321,   321,   322,   322,
     322,   322,   323,   323,   323,   324,   324,   325,   325,   326,
     326,   327,   327,   328,   328,   329,   330,   330,   330,   331,
     331,   332,   332,   333,   333,   334
};

/* YYR2[RULE-NUM] -- Number of symbols on the right-hand side of rule RULE-NUM.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     3,     1,     3,     3,     6,     5,     5,     5,
       5,     4,     6,     4,     6,     3,     1,     3,     1,     1,
       3,     3,     3,     2,     1,     2,     0,     5,     1,     1,
       1,     1,     4,     0,     5,     2,     3,     4,     5,     4,
       5,     2,     2,     2,     2,     2,     1,     3,     1,     3,
       1,     2,     3,     5,     2,     4,     2,     4,     1,     3,
       1,     3,     2,     3,     1,     2,     1,     4,     3,     3,
       3,     3,     2,     1,     1,     4,     3,     3,     3,     3,
       2,     1,     1,     1,     1,     2,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     4,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       6,     5,     5,     5,     5,     4,     3,     3,     2,     2,
       3,     2,     2,     3,     3,     3,     3,     3,     3,     4,
       4,     2,     2,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     2,     2,     3,     3,
       3,     3,     6,     6,     4,     6,     4,     6,     1,     1,
       2,     4,     2,     1,     3,     3,     5,     3,     1,     1,
       1,     2,     2,     4,     2,     1,     2,     2,     4,     1,
       0,     2,     2,     1,     2,     1,     2,     1,     1,     2,
       3,     3,     4,     3,     4,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     4,     0,     0,     5,     0,
       3,     3,     3,     2,     3,     3,     1,     2,     4,     3,
       2,     1,     2,     0,     0,     5,     6,     6,     0,     0,
       7,     0,     0,     7,     5,     4,     9,    11,    11,     0,
       0,     9,     0,     6,     0,     0,     8,     0,     5,     4,
       4,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     5,     1,     2,     1,     1,     1,     4,     6,
       3,     5,     2,     4,     1,     0,     4,     4,     2,     2,
       1,     2,     0,     6,     8,     4,     6,     4,     3,     6,
       2,     4,     6,     2,     4,     2,     4,     1,     1,     1,
       0,     4,     1,     4,     1,     4,     1,     3,     1,     1,
       4,     1,     3,     3,     0,     5,     2,     4,     5,     5,
       2,     4,     4,     3,     3,     3,     2,     1,     4,     0,
       5,     0,     5,     5,     1,     1,     1,     5,     7,     7,
       1,     1,     3,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     2,     1,     3,     2,     1,     3,
       5,     1,     3,     1,     3,     2,     1,     1,     6,     1,
       1,     1,     1,     2,     1,     2,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     2,     3,     1,     2,     1,
       0,     4,     1,     2,     2,     3,     2,     3,     1,     1,
       2,     1,     2,     1,     2,     1,     0,     4,     2,     3,
       1,     4,     2,     2,     1,     1,     1,     1,     1,     2,
       2,     3,     1,     1,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     0,     4,     1,     1,     3,
       5,     3,     1,     2,     4,     2,     2,     2,     2,     1,
       2,     1,     1,     3,     1,     3,     1,     1,     2,     1,
       4,     2,     2,     1,     2,     0,     6,     8,     4,     6,
       4,     6,     2,     4,     6,     2,     4,     2,     4,     1,
       0,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       4,     1,     3,     2,     2,     2,     1,     3,     1,     3,
       1,     1,     2,     1,     1,     1,     2,     1,     2,     1,
       1,     0,     4,     1,     2,     1,     3,     3,     3,     2,
       2,     3,     3,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     0,     2,     2,     0,     1,     1,     1,
       1,     1,     1,     1,     2,     0
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



#line 7482 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 2137 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 7700 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3: /* program: $@1 top_compstmt  */
#line 2142 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                    }
#line 7708 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4: /* top_compstmt: top_stmts opt_terms  */
#line 2148 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7716 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5: /* top_stmts: none  */
#line 2154 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7724 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6: /* top_stmts: top_stmt  */
#line 2158 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7: /* top_stmts: top_stmts terms top_stmt  */
#line 2162 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = stmts_push(p, (yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 7740 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8: /* top_stmts: error top_stmt  */
#line 2166 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7748 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10: /* @2: %empty  */
#line 2173 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7757 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11: /* top_stmt: "'BEGIN'" @2 '{' top_compstmt '}'  */
#line 2178 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 7768 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12: /* bodystmt: compstmt opt_rescue opt_else opt_ensure  */
#line 2190 "mrbgems/mruby-compiler/core/parse.y"
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
#line 7793 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13: /* compstmt: stmts opt_terms  */
#line 2213 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7801 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14: /* stmts: none  */
#line 2219 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15: /* stmts: stmt  */
#line 2223 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7817 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16: /* stmts: stmts terms stmt  */
#line 2227 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = stmts_push(p, (yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 7825 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17: /* stmts: error stmt  */
#line 2231 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7833 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18: /* $@3: %empty  */
#line 2236 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 7839 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19: /* stmt: "'alias'" fsym $@3 fsym  */
#line 2237 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 7847 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20: /* stmt: "'undef'" undef_list  */
#line 2241 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].nd));
                    }
#line 7855 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21: /* stmt: stmt "'if' modifier" expr_value  */
#line 2245 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 7863 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22: /* stmt: stmt "'unless' modifier" expr_value  */
#line 2249 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), 0, (yyvsp[-2].nd));
                    }
#line 7871 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23: /* stmt: stmt "'while' modifier" expr_value  */
#line 2253 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd) && node_type_p((yyvsp[-2].nd), NODE_BEGIN)) {
                        (yyval.nd) = new_while_mod(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                      else {
                        (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                    }
#line 7884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24: /* stmt: stmt "'until' modifier" expr_value  */
#line 2262 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd) && node_type_p((yyvsp[-2].nd), NODE_BEGIN)) {
                        (yyval.nd) = new_until_mod(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                      else {
                        (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                    }
#line 7897 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25: /* stmt: stmt "'rescue' modifier" stmt  */
#line 2271 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26: /* stmt: "'END'" '{' compstmt '}'  */
#line 2275 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 7914 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28: /* stmt: mlhs '=' command_call  */
#line 2281 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7922 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29: /* stmt: lhs '=' mrhs  */
#line 2285 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 7930 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30: /* stmt: mlhs '=' arg  */
#line 2289 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7938 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31: /* stmt: mlhs '=' mrhs  */
#line 2293 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 7946 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 32: /* stmt: arg "=>" "local variable or method"  */
#line 2297 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *lhs = new_lvar(p, (yyvsp[0].id));
                      assignable(p, lhs);
                      (yyval.nd) = new_asgn(p, lhs, (yyvsp[-2].nd));
                    }
#line 7956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34: /* command_asgn: lhs '=' command_rhs  */
#line 2306 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35: /* command_asgn: var_lhs tOP_ASGN command_rhs  */
#line 2310 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36: /* command_asgn: primary_value '[' opt_call_args ']' tOP_ASGN command_rhs  */
#line 2314 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37: /* command_asgn: primary_value call_op "local variable or method" tOP_ASGN command_rhs  */
#line 2318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38: /* command_asgn: primary_value call_op "constant" tOP_ASGN command_rhs  */
#line 2322 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39: /* command_asgn: primary_value "::" "constant" tOP_ASGN command_call  */
#line 2326 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 8005 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40: /* command_asgn: primary_value "::" "local variable or method" tOP_ASGN command_rhs  */
#line 2331 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41: /* command_asgn: defn_head f_opt_arglist_paren '=' command  */
#line 2335 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8026 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42: /* command_asgn: defn_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 2344 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43: /* command_asgn: defs_head f_opt_arglist_paren '=' command  */
#line 2353 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8052 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44: /* command_asgn: defs_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 2362 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8065 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 45: /* command_asgn: backref tOP_ASGN command_rhs  */
#line 2371 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8074 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47: /* command_rhs: command_call "'rescue' modifier" stmt  */
#line 2379 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8082 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50: /* expr: expr "'and'" expr  */
#line 2387 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51: /* expr: expr "'or'" expr  */
#line 2391 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8098 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52: /* expr: "'not'" opt_nl expr  */
#line 2395 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 8106 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53: /* expr: '!' command_call  */
#line 2399 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 8114 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55: /* defn_head: "'def'" fname  */
#line 2406 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 8125 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56: /* $@4: %empty  */
#line 2415 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 8133 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 57: /* defs_head: "'def'" singleton dot_or_colon $@4 fname  */
#line 2419 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 8146 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58: /* expr_value: expr  */
#line 2430 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 8157 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62: /* block_command: block_call call_op2 operation2 command_args  */
#line 2444 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8165 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63: /* $@5: %empty  */
#line 2450 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8174 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64: /* cmd_brace_block: "{" $@5 opt_block_param compstmt '}'  */
#line 2457 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8184 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65: /* command: operation command_args  */
#line 2465 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8192 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66: /* command: operation command_args cmd_brace_block  */
#line 2469 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 8201 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67: /* command: primary_value call_op operation2 command_args  */
#line 2474 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8209 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68: /* command: primary_value call_op operation2 command_args cmd_brace_block  */
#line 2478 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 8218 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69: /* command: primary_value "::" operation2 command_args  */
#line 2483 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8226 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70: /* command: primary_value "::" operation2 command_args cmd_brace_block  */
#line 2487 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 8235 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71: /* command: "'super'" command_args  */
#line 2492 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72: /* command: "'yield'" command_args  */
#line 2496 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 8251 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73: /* command: "'return'" call_args  */
#line 2500 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 8259 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74: /* command: "'break'" call_args  */
#line 2504 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 8267 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75: /* command: "'next'" call_args  */
#line 2508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 8275 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76: /* mlhs: mlhs_basic  */
#line 2514 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8283 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77: /* mlhs: tLPAREN mlhs_inner rparen  */
#line 2518 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79: /* mlhs_inner: tLPAREN mlhs_inner rparen  */
#line 2525 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8299 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80: /* mlhs_basic: mlhs_list  */
#line 2531 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8307 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81: /* mlhs_basic: mlhs_list mlhs_item  */
#line 2535 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 8315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82: /* mlhs_basic: mlhs_list "*" mlhs_node  */
#line 2539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83: /* mlhs_basic: mlhs_list "*" mlhs_node ',' mlhs_post  */
#line 2543 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8331 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84: /* mlhs_basic: mlhs_list "*"  */
#line 2547 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 8339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85: /* mlhs_basic: mlhs_list "*" ',' mlhs_post  */
#line 2551 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 8347 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86: /* mlhs_basic: "*" mlhs_node  */
#line 2555 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 8355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87: /* mlhs_basic: "*" mlhs_node ',' mlhs_post  */
#line 2559 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88: /* mlhs_basic: "*"  */
#line 2563 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 8371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89: /* mlhs_basic: "*" ',' mlhs_post  */
#line 2567 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 8379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91: /* mlhs_item: tLPAREN mlhs_inner rparen  */
#line 2574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 8387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92: /* mlhs_list: mlhs_item ','  */
#line 2580 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 8395 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93: /* mlhs_list: mlhs_list mlhs_item ','  */
#line 2584 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 8403 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94: /* mlhs_post: mlhs_item  */
#line 2590 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95: /* mlhs_post: mlhs_list mlhs_item  */
#line 2594 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8419 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96: /* mlhs_node: variable  */
#line 2600 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97: /* mlhs_node: primary_value '[' opt_call_args ']'  */
#line 2604 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8435 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98: /* mlhs_node: primary_value call_op "local variable or method"  */
#line 2608 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99: /* mlhs_node: primary_value "::" "local variable or method"  */
#line 2612 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100: /* mlhs_node: primary_value call_op "constant"  */
#line 2616 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101: /* mlhs_node: primary_value "::" "constant"  */
#line 2620 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8469 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102: /* mlhs_node: tCOLON3 "constant"  */
#line 2626 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8479 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103: /* mlhs_node: backref  */
#line 2632 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 8488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104: /* lhs: variable  */
#line 2639 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8496 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105: /* lhs: primary_value '[' opt_call_args ']'  */
#line 2643 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8504 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106: /* lhs: primary_value call_op "local variable or method"  */
#line 2647 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8512 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107: /* lhs: primary_value "::" "local variable or method"  */
#line 2651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108: /* lhs: primary_value call_op "constant"  */
#line 2655 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109: /* lhs: primary_value "::" "constant"  */
#line 2659 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8538 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110: /* lhs: tCOLON3 "constant"  */
#line 2665 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8548 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111: /* lhs: backref  */
#line 2671 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 8557 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112: /* lhs: "numbered parameter"  */
#line 2676 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 8565 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113: /* cname: "local variable or method"  */
#line 2682 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "class/module name must be CONSTANT");
                    }
#line 8573 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115: /* cpath: tCOLON3 cname  */
#line 2689 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(int_to_node(1), sym_to_node((yyvsp[0].id)));
                    }
#line 8581 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116: /* cpath: cname  */
#line 2693 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(int_to_node(0), sym_to_node((yyvsp[0].id)));
                    }
#line 8589 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117: /* cpath: primary_value "::" cname  */
#line 2697 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), sym_to_node((yyvsp[0].id)));
                    }
#line 8598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121: /* fname: op  */
#line 2707 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8607 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122: /* fname: reswords  */
#line 2712 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8616 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125: /* undef_list: fsym  */
#line 2723 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(sym_to_node((yyvsp[0].id)), 0);
                    }
#line 8624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126: /* $@6: %empty  */
#line 2726 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 8630 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127: /* undef_list: undef_list ',' $@6 fsym  */
#line 2727 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), sym_to_node((yyvsp[0].id)));
                    }
#line 8638 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128: /* op: '|'  */
#line 2732 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(or);     }
#line 8644 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129: /* op: '^'  */
#line 2733 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(xor);    }
#line 8650 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130: /* op: '&'  */
#line 2734 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(and);    }
#line 8656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131: /* op: "<=>"  */
#line 2735 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(cmp);    }
#line 8662 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132: /* op: "=="  */
#line 2736 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eq);     }
#line 8668 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133: /* op: "==="  */
#line 2737 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eqq);    }
#line 8674 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134: /* op: "=~"  */
#line 2738 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(match);  }
#line 8680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135: /* op: "!~"  */
#line 2739 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(nmatch); }
#line 8686 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136: /* op: '>'  */
#line 2740 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(gt);     }
#line 8692 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137: /* op: ">="  */
#line 2741 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(ge);     }
#line 8698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138: /* op: '<'  */
#line 2742 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lt);     }
#line 8704 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139: /* op: "<="  */
#line 2743 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(le);     }
#line 8710 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140: /* op: "!="  */
#line 2744 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neq);    }
#line 8716 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141: /* op: "<<"  */
#line 2745 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lshift); }
#line 8722 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142: /* op: ">>"  */
#line 2746 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(rshift); }
#line 8728 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143: /* op: '+'  */
#line 2747 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(add);    }
#line 8734 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144: /* op: '-'  */
#line 2748 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(sub);    }
#line 8740 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145: /* op: '*'  */
#line 2749 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 8746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146: /* op: "*"  */
#line 2750 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 8752 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147: /* op: '/'  */
#line 2751 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(div);    }
#line 8758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148: /* op: '%'  */
#line 2752 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mod);    }
#line 8764 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149: /* op: tPOW  */
#line 2753 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 8770 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150: /* op: "**"  */
#line 2754 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 8776 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151: /* op: '!'  */
#line 2755 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(not);    }
#line 8782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152: /* op: '~'  */
#line 2756 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neg);    }
#line 8788 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153: /* op: "unary plus"  */
#line 2757 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(plus);   }
#line 8794 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154: /* op: "unary minus"  */
#line 2758 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(minus);  }
#line 8800 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155: /* op: tAREF  */
#line 2759 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aref);   }
#line 8806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156: /* op: tASET  */
#line 2760 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aset);   }
#line 8812 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157: /* op: '`'  */
#line 2761 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(tick);   }
#line 8818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198: /* arg: lhs '=' arg_rhs  */
#line 2779 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8826 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199: /* arg: var_lhs tOP_ASGN arg_rhs  */
#line 2783 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8834 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200: /* arg: primary_value '[' opt_call_args ']' tOP_ASGN arg_rhs  */
#line 2787 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201: /* arg: primary_value call_op "local variable or method" tOP_ASGN arg_rhs  */
#line 2791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202: /* arg: primary_value call_op "constant" tOP_ASGN arg_rhs  */
#line 2795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203: /* arg: primary_value "::" "local variable or method" tOP_ASGN arg_rhs  */
#line 2799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204: /* arg: primary_value "::" "constant" tOP_ASGN arg_rhs  */
#line 2803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205: /* arg: tCOLON3 "constant" tOP_ASGN arg_rhs  */
#line 2808 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "constant re-assignment");
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206: /* arg: backref tOP_ASGN arg_rhs  */
#line 2813 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8893 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207: /* arg: arg ".." arg  */
#line 2818 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8901 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208: /* arg: arg ".."  */
#line 2822 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 8909 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209: /* arg: tBDOT2 arg  */
#line 2826 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 8917 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210: /* arg: arg "..." arg  */
#line 2830 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211: /* arg: arg "..."  */
#line 2834 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 8933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212: /* arg: tBDOT3 arg  */
#line 2838 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 8941 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213: /* arg: arg '+' arg  */
#line 2842 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 8949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214: /* arg: arg '-' arg  */
#line 2846 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 8957 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215: /* arg: arg '*' arg  */
#line 2850 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 8965 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216: /* arg: arg '/' arg  */
#line 2854 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 8973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217: /* arg: arg '%' arg  */
#line 2858 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 8981 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218: /* arg: arg tPOW arg  */
#line 2862 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 8989 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219: /* arg: tUMINUS_NUM "integer literal" tPOW arg  */
#line 2866 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 8997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220: /* arg: tUMINUS_NUM "float literal" tPOW arg  */
#line 2870 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 9005 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221: /* arg: "unary plus" arg  */
#line 2874 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 9013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222: /* arg: "unary minus" arg  */
#line 2878 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 9021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223: /* arg: arg '|' arg  */
#line 2882 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 9029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224: /* arg: arg '^' arg  */
#line 2886 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 9037 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225: /* arg: arg '&' arg  */
#line 2890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 9045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226: /* arg: arg "<=>" arg  */
#line 2894 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 9053 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227: /* arg: arg '>' arg  */
#line 2898 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 9061 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228: /* arg: arg ">=" arg  */
#line 2902 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 9069 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229: /* arg: arg '<' arg  */
#line 2906 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 9077 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230: /* arg: arg "<=" arg  */
#line 2910 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 9085 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231: /* arg: arg "==" arg  */
#line 2914 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 9093 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232: /* arg: arg "===" arg  */
#line 2918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 9101 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233: /* arg: arg "!=" arg  */
#line 2922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 9109 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234: /* arg: arg "=~" arg  */
#line 2926 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 9117 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235: /* arg: arg "!~" arg  */
#line 2930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 9125 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236: /* arg: '!' arg  */
#line 2934 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 9133 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237: /* arg: '~' arg  */
#line 2938 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 9141 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238: /* arg: arg "<<" arg  */
#line 2942 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 9149 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239: /* arg: arg ">>" arg  */
#line 2946 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 9157 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240: /* arg: arg "&&" arg  */
#line 2950 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9165 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241: /* arg: arg "||" arg  */
#line 2954 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9173 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242: /* arg: arg '?' arg opt_nl ':' arg  */
#line 2958 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 9181 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243: /* arg: arg '?' arg opt_nl "label" arg  */
#line 2962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 9189 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244: /* arg: defn_head f_opt_arglist_paren '=' arg  */
#line 2966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 9202 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245: /* arg: defn_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 2975 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 9215 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246: /* arg: defs_head f_opt_arglist_paren '=' arg  */
#line 2984 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 9228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247: /* arg: defs_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 2993 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 9241 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248: /* arg: primary  */
#line 3002 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9249 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250: /* aref_args: args trailer  */
#line 3009 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9257 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251: /* aref_args: args comma assocs trailer  */
#line 3013 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd)));
                    }
#line 9265 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252: /* aref_args: assocs trailer  */
#line 3017 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_hash(p, (yyvsp[-1].nd)), 0);
                    }
#line 9273 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253: /* arg_rhs: arg  */
#line 3023 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9281 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254: /* arg_rhs: arg "'rescue' modifier" arg  */
#line 3027 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9290 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255: /* paren_args: '(' opt_call_args ')'  */
#line 3034 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9298 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256: /* paren_args: '(' args comma tBDOT3 rparen  */
#line 3038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      (yyval.nd) = new_callargs(p, push((yyvsp[-3].nd), new_splat(p, new_lvar(p, r))),
                                        list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k))),
                                        new_block_arg(p, new_lvar(p, b)));
                    }
#line 9311 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257: /* paren_args: '(' tBDOT3 rparen  */
#line 3047 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9330 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262: /* opt_call_args: args comma  */
#line 3070 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-1].nd),0,0);
                    }
#line 9338 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263: /* opt_call_args: args comma assocs comma  */
#line 3074 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-3].nd),(yyvsp[-1].nd),0);
                    }
#line 9346 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264: /* opt_call_args: assocs comma  */
#line 3078 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,0,(yyvsp[-1].nd),0);
                    }
#line 9354 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265: /* call_args: command  */
#line 3084 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_callargs(p, list1((yyvsp[0].nd)), 0, 0);
                    }
#line 9363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266: /* call_args: args opt_block_arg  */
#line 3089 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-1].nd), 0, (yyvsp[0].nd));
                    }
#line 9371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267: /* call_args: assocs opt_block_arg  */
#line 3093 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268: /* call_args: args comma assocs opt_block_arg  */
#line 3097 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269: /* call_args: block_arg  */
#line 3101 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, 0, (yyvsp[0].nd));
                    }
#line 9395 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270: /* @7: %empty  */
#line 3106 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 9404 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 271: /* command_args: @7 call_args  */
#line 3111 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9413 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272: /* block_arg: "&" arg  */
#line 3118 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 9421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273: /* block_arg: "&"  */
#line 3122 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, 0);
                    }
#line 9429 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274: /* opt_block_arg: comma block_arg  */
#line 3128 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9437 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275: /* opt_block_arg: none  */
#line 3132 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9445 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277: /* args: arg  */
#line 3141 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278: /* args: "*"  */
#line 3146 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 9462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279: /* args: "*" arg  */
#line 3150 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 9470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280: /* args: args comma arg  */
#line 3154 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9479 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281: /* args: args comma "*"  */
#line 3159 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 9487 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282: /* args: args comma "*" arg  */
#line 3163 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 9495 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 283: /* mrhs: args comma arg  */
#line 3169 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9504 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 284: /* mrhs: args comma "*" arg  */
#line 3174 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 9512 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 285: /* mrhs: "*" arg  */
#line 3178 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 9520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 287: /* primary: string  */
#line 3185 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_str(p, (yyvsp[0].nd));
                    }
#line 9528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 288: /* primary: xstring  */
#line 3189 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_xstr(p, (yyvsp[0].nd));
                    }
#line 9536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293: /* primary: "method"  */
#line 3197 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 9544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294: /* @8: %empty  */
#line 3201 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9553 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295: /* primary: "'begin'" @8 bodystmt "'end'"  */
#line 3207 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = new_begin(p, (yyvsp[-1].nd));
                    }
#line 9562 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296: /* @9: %empty  */
#line 3212 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9571 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297: /* $@10: %empty  */
#line 3216 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 9577 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298: /* primary: "(" @9 stmt $@10 rparen  */
#line 3217 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 9586 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299: /* $@11: %empty  */
#line 3221 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 9592 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300: /* primary: "(" $@11 rparen  */
#line 3222 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9600 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301: /* primary: tLPAREN compstmt ')'  */
#line 3226 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9608 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302: /* primary: primary_value "::" "constant"  */
#line 3230 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 9616 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303: /* primary: tCOLON3 "constant"  */
#line 3234 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 9624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304: /* primary: "[" aref_args ']'  */
#line 3238 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                    }
#line 9632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305: /* primary: tLBRACE assoc_list '}'  */
#line 3242 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                    }
#line 9640 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306: /* primary: "'return'"  */
#line 3246 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 9648 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307: /* primary: "'yield'" opt_paren_args  */
#line 3250 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 9656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308: /* primary: "'not'" '(' expr rparen  */
#line 3254 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 9664 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309: /* primary: "'not'" '(' rparen  */
#line 3258 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 9672 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310: /* primary: operation brace_block  */
#line 3262 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), new_callargs(p, 0, 0, (yyvsp[0].nd)));
                    }
#line 9680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312: /* primary: method_call brace_block  */
#line 3267 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313: /* @12: %empty  */
#line 3272 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 9700 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314: /* @13: %empty  */
#line 3279 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9709 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315: /* primary: "->" @12 f_larglist @13 lambda_body  */
#line 3284 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 9722 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316: /* primary: "'if'" expr_value then compstmt if_tail "'end'"  */
#line 3296 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 9731 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317: /* primary: "'unless'" expr_value then compstmt opt_else "'end'"  */
#line 3304 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd), (yyvsp[-2].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 9740 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318: /* $@14: %empty  */
#line 3308 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 9746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319: /* $@15: %empty  */
#line 3308 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 9752 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320: /* primary: "'while'" $@14 expr_value do $@15 compstmt "'end'"  */
#line 3311 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 9761 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321: /* $@16: %empty  */
#line 3315 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 9767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322: /* $@17: %empty  */
#line 3315 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 9773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323: /* primary: "'until'" $@16 expr_value do $@17 compstmt "'end'"  */
#line 3318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 9782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324: /* primary: "'case'" expr_value opt_terms case_body "'end'"  */
#line 3325 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 9790 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325: /* primary: "'case'" opt_terms case_body "'end'"  */
#line 3329 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 9798 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326: /* primary: "'case'" expr_value opt_terms "'in'" p_expr then compstmt in_clauses "'end'"  */
#line 3337 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-4].nd), NULL, (yyvsp[-2].nd), FALSE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-7].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 9807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327: /* primary: "'case'" expr_value opt_terms "'in'" p_expr "'if' modifier" expr_value then compstmt in_clauses "'end'"  */
#line 3346 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-4].nd), (yyvsp[-2].nd), FALSE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-9].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 9816 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328: /* primary: "'case'" expr_value opt_terms "'in'" p_expr "'unless' modifier" expr_value then compstmt in_clauses "'end'"  */
#line 3355 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-6].nd), (yyvsp[-4].nd), (yyvsp[-2].nd), TRUE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-9].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 9825 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329: /* $@18: %empty  */
#line 3360 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 9831 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330: /* $@19: %empty  */
#line 3362 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 9837 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331: /* primary: "'for'" for_var "'in'" $@18 expr_value do $@19 compstmt "'end'"  */
#line 3365 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 9846 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332: /* @20: %empty  */
#line 3371 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 9857 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333: /* primary: "'class'" cpath superclass @20 bodystmt "'end'"  */
#line 3379 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 9868 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334: /* @21: %empty  */
#line 3387 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 9877 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335: /* @22: %empty  */
#line 3392 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), int_to_node(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 9887 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336: /* primary: "'class'" "<<" expr @21 term @22 bodystmt "'end'"  */
#line 3399 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = node_to_int((yyvsp[-2].nd)->cdr);
                    }
#line 9900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337: /* @23: %empty  */
#line 3409 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 9911 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338: /* primary: "'module'" cpath @23 bodystmt "'end'"  */
#line 3417 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 9922 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 339: /* primary: defn_head f_arglist bodystmt "'end'"  */
#line 3427 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 9933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340: /* primary: defs_head f_arglist bodystmt "'end'"  */
#line 3437 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 9945 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 341: /* primary: "'break'"  */
#line 3445 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 9953 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 342: /* primary: "'next'"  */
#line 3449 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 9961 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 343: /* primary: "'redo'"  */
#line 3453 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 9969 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 344: /* primary: "'retry'"  */
#line 3457 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 9977 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 345: /* primary_value: primary  */
#line 3463 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9986 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352: /* if_tail: "'elsif'" expr_value then compstmt if_tail  */
#line 3482 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9994 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354: /* opt_else: "'else'" compstmt  */
#line 3489 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10002 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355: /* for_var: lhs  */
#line 3495 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 10010 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357: /* f_margs: f_arg  */
#line 3502 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 10018 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358: /* f_margs: f_arg ',' "*" f_norm_arg  */
#line 3506 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_lvar(p, (yyvsp[0].id)), 0);
                    }
#line 10026 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359: /* f_margs: f_arg ',' "*" f_norm_arg ',' f_arg  */
#line 3510 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_lvar(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 10034 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360: /* f_margs: f_arg ',' "*"  */
#line 3514 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3((yyvsp[-2].nd), int_to_node(-1), 0);
                    }
#line 10043 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361: /* f_margs: f_arg ',' "*" ',' f_arg  */
#line 3519 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), int_to_node(-1), (yyvsp[0].nd));
                    }
#line 10051 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362: /* f_margs: "*" f_norm_arg  */
#line 3523 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_lvar(p, (yyvsp[0].id)), 0);
                    }
#line 10059 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363: /* f_margs: "*" f_norm_arg ',' f_arg  */
#line 3527 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_lvar(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 10067 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364: /* f_margs: "*"  */
#line 3531 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3(0, int_to_node(-1), 0);
                    }
#line 10076 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365: /* $@24: %empty  */
#line 3536 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                    }
#line 10084 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366: /* f_margs: "*" ',' $@24 f_arg  */
#line 3540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, int_to_node(-1), (yyvsp[0].nd));
                    }
#line 10092 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367: /* block_args_tail: f_block_kwarg ',' f_kwrest opt_f_block_arg  */
#line 3546 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 10100 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368: /* block_args_tail: f_block_kwarg opt_f_block_arg  */
#line 3550 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 10108 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369: /* block_args_tail: f_kwrest opt_f_block_arg  */
#line 3554 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 10116 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370: /* block_args_tail: f_block_arg  */
#line 3558 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 10124 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371: /* opt_block_args_tail: ',' block_args_tail  */
#line 3564 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10132 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372: /* opt_block_args_tail: %empty  */
#line 3568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 10140 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10148 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10156 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375: /* block_param: f_arg ',' f_block_optarg opt_block_args_tail  */
#line 3582 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 10164 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376: /* block_param: f_arg ',' f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3586 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10172 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377: /* block_param: f_arg ',' f_rest_arg opt_block_args_tail  */
#line 3590 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378: /* block_param: f_arg ',' opt_block_args_tail  */
#line 3594 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10188 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379: /* block_param: f_arg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3598 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380: /* block_param: f_arg opt_block_args_tail  */
#line 3602 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381: /* block_param: f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3606 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10212 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382: /* block_param: f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3610 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10220 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383: /* block_param: f_block_optarg opt_block_args_tail  */
#line 3614 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 10228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384: /* block_param: f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3618 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10236 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385: /* block_param: f_rest_arg opt_block_args_tail  */
#line 3622 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10244 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386: /* block_param: f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3626 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10252 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387: /* block_param: block_args_tail  */
#line 3630 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10260 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388: /* opt_block_param: none  */
#line 3636 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 10269 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389: /* opt_block_param: block_param_def  */
#line 3641 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10278 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390: /* $@25: %empty  */
#line 3647 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p);}
#line 10284 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 391: /* block_param_def: '|' $@25 opt_bv_decl '|'  */
#line 3648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10292 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392: /* block_param_def: "||"  */
#line 3652 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 10301 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393: /* block_param_def: '|' block_param opt_bv_decl '|'  */
#line 3657 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 10309 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 394: /* opt_bv_decl: opt_nl  */
#line 3663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10317 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395: /* opt_bv_decl: opt_nl ';' bv_decls opt_nl  */
#line 3667 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10325 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398: /* bvar: "local variable or method"  */
#line 3677 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 10334 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400: /* f_larglist: '(' f_args opt_bv_decl ')'  */
#line 3685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 10342 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401: /* f_larglist: f_args  */
#line 3689 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10350 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402: /* lambda_body: tLAMBEG compstmt '}'  */
#line 3695 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10358 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403: /* lambda_body: "'do' for lambda" bodystmt "'end'"  */
#line 3699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10366 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404: /* @26: %empty  */
#line 3705 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10376 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405: /* do_block: "'do' for block" @26 opt_block_param bodystmt "'end'"  */
#line 3713 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406: /* block_call: command do_block  */
#line 3722 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10396 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407: /* block_call: block_call call_op2 operation2 opt_paren_args  */
#line 3727 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 10404 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408: /* block_call: block_call call_op2 operation2 opt_paren_args brace_block  */
#line 3731 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 10413 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409: /* block_call: block_call call_op2 operation2 command_args do_block  */
#line 3736 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 10422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410: /* method_call: operation paren_args  */
#line 3743 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 10430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411: /* method_call: primary_value call_op operation2 opt_paren_args  */
#line 3747 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 10438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412: /* method_call: primary_value "::" operation2 paren_args  */
#line 3751 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 10446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413: /* method_call: primary_value "::" operation3  */
#line 3755 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 10454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414: /* method_call: primary_value call_op paren_args  */
#line 3759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 10462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415: /* method_call: primary_value "::" paren_args  */
#line 3763 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), tCOLON2);
                    }
#line 10470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416: /* method_call: "'super'" paren_args  */
#line 3767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 10478 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417: /* method_call: "'super'"  */
#line 3771 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 10486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418: /* method_call: primary_value '[' opt_call_args ']'  */
#line 3775 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 10494 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419: /* @27: %empty  */
#line 3781 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10504 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 420: /* brace_block: '{' @27 opt_block_param compstmt '}'  */
#line 3788 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10515 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 421: /* @28: %empty  */
#line 3795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10525 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 422: /* brace_block: "'do'" @28 opt_block_param bodystmt "'end'"  */
#line 3802 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423: /* case_body: "'when'" args then compstmt cases  */
#line 3813 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 10544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 424: /* cases: opt_else  */
#line 3819 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 10557 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 426: /* in_clauses: opt_else  */
#line 3832 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd) ? list1(new_in(p, NULL, NULL, (yyvsp[0].nd), FALSE)) : 0;
                    }
#line 10565 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 427: /* in_clauses: "'in'" p_expr then compstmt in_clauses  */
#line 3836 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-3].nd), NULL, (yyvsp[-1].nd), FALSE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 428: /* in_clauses: "'in'" p_expr "'if' modifier" expr_value then compstmt in_clauses  */
#line 3841 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].nd), FALSE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10583 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 429: /* in_clauses: "'in'" p_expr "'unless' modifier" expr_value then compstmt in_clauses  */
#line 3846 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].nd), TRUE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10592 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 432: /* p_as: p_alt "=>" "local variable or method"  */
#line 3858 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_as(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 10600 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 434: /* p_alt: p_alt '|' p_value  */
#line 3865 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_alt(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10608 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 436: /* p_value: numeric  */
#line 3872 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10616 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 437: /* p_value: symbol  */
#line 3876 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438: /* p_value: tSTRING  */
#line 3880 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439: /* p_value: "'nil'"  */
#line 3884 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_nil(p));
                    }
#line 10640 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440: /* p_value: "'true'"  */
#line 3888 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_true(p));
                    }
#line 10648 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441: /* p_value: "'false'"  */
#line 3892 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_false(p));
                    }
#line 10656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442: /* p_value: "constant"  */
#line 3896 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_const(p, (yyvsp[0].id)));
                    }
#line 10664 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443: /* p_value: primary_value "::" "constant"  */
#line 3900 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id)));
                    }
#line 10672 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444: /* p_value: tCOLON3 "constant"  */
#line 3904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_colon3(p, (yyvsp[0].id)));
                    }
#line 10680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446: /* p_array: "[" p_array_body ']'  */
#line 3912 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10688 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447: /* p_array: "[" ']'  */
#line 3916 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_array(p, 0, 0, 0);
                    }
#line 10696 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 448: /* p_array_body: p_array_elems  */
#line 3923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Just pre elements, no rest */
                      (yyval.nd) = new_pat_array(p, (yyvsp[0].nd), 0, 0);
                    }
#line 10705 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 449: /* p_array_body: p_array_elems ',' p_rest  */
#line 3928 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Pre elements + rest, no post */
                      (yyval.nd) = new_pat_array(p, (yyvsp[-2].nd), (yyvsp[0].nd), 0);
                    }
#line 10714 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450: /* p_array_body: p_array_elems ',' p_rest ',' p_array_elems  */
#line 3933 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Pre + rest + post */
                      (yyval.nd) = new_pat_array(p, (yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10723 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 451: /* p_array_body: p_rest  */
#line 3938 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Just rest, no pre or post */
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[0].nd), 0);
                    }
#line 10732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 452: /* p_array_body: p_rest ',' p_array_elems  */
#line 3943 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Rest + post, no pre */
                      (yyval.nd) = new_pat_array(p, 0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10741 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 453: /* p_array_elems: p_expr  */
#line 3951 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10749 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 454: /* p_array_elems: p_array_elems ',' p_expr  */
#line 3955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10757 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 455: /* p_rest: "*" "local variable or method"  */
#line 3962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 10765 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456: /* p_rest: "*"  */
#line 3966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* Anonymous rest pattern */
                      (yyval.nd) = (node*)-1;
                    }
#line 10774 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457: /* p_var: "local variable or method"  */
#line 3973 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 10782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458: /* opt_rescue: "'rescue'" exc_list exc_var then compstmt opt_rescue  */
#line 3981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 10791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460: /* exc_list: arg  */
#line 3989 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 463: /* exc_var: "=>" lhs  */
#line 3997 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465: /* opt_ensure: "'ensure'" compstmt  */
#line 4004 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10815 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 472: /* string: string string_fragment  */
#line 4018 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10823 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473: /* string_fragment: "character literal"  */
#line 4024 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* tCHAR is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474: /* string_fragment: tSTRING  */
#line 4029 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* tSTRING is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10841 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475: /* string_fragment: "string literal" tSTRING  */
#line 4034 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* $2 is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476: /* string_fragment: "string literal" string_rep tSTRING  */
#line 4039 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478: /* string_rep: string_rep string_interp  */
#line 4046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479: /* string_interp: tSTRING_MID  */
#line 4052 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* $1 is already in (len . str) format */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480: /* @29: %empty  */
#line 4057 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 10883 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481: /* string_interp: tSTRING_PART @29 compstmt '}'  */
#line 4062 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p,(yyvsp[-2].nd));
                      /* $1 is already in (len . str) format, create (-1 . node) for expression */
                      node *expr_elem = cons(int_to_node(-1), (yyvsp[-1].nd));
                      (yyval.nd) = list2((yyvsp[-3].nd), expr_elem);
                    }
#line 10894 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482: /* string_interp: tLITERAL_DELIM  */
#line 4069 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 10902 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483: /* string_interp: tHD_LITERAL_DELIM heredoc_bodies  */
#line 4073 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 10910 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484: /* xstring: tXSTRING_BEG tXSTRING  */
#line 4079 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = cons((yyvsp[0].nd), (node*)NULL);
                    }
#line 10918 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485: /* xstring: tXSTRING_BEG string_rep tXSTRING  */
#line 4083 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10926 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486: /* regexp: tREGEXP_BEG tREGEXP  */
#line 4089 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *data = (yyvsp[0].nd);  /* ((len . pattern) . (flags . encoding)) */
                      const char *flags = (const char*)data->cdr->car;
                      const char *encoding = (const char*)data->cdr->cdr;
                      /* Use data->car directly as pattern_list: (len . pattern) */
                      node *pattern_list = cons(data->car, (node*)NULL);
                      (yyval.nd) = new_regx(p, pattern_list, flags, encoding);
                    }
#line 10939 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487: /* regexp: tREGEXP_BEG string_rep tREGEXP  */
#line 4098 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *data = (yyvsp[0].nd);  /* ((len . pattern) . (flags . encoding)) */
                      const char *flags = (const char*)data->cdr->car;
                      const char *encoding = (const char*)data->cdr->cdr;
                      /* Append the pattern from $3->car to the string list $2 */
                      node *complete_list = push((yyvsp[-1].nd), data->car);
                      (yyval.nd) = new_regx(p, complete_list, flags, encoding);
                    }
#line 10952 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491: /* heredoc_body: tHEREDOC_END  */
#line 4116 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, new_str_empty(p));
                      heredoc_end(p);
                    }
#line 10962 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492: /* heredoc_body: heredoc_string_rep tHEREDOC_END  */
#line 4122 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 10970 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495: /* heredoc_string_interp: tHD_STRING_MID  */
#line 4132 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 10980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 496: /* @30: %empty  */
#line 4138 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 10988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 497: /* heredoc_string_interp: tHD_STRING_PART @30 compstmt '}'  */
#line 4143 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p, (yyvsp[-2].nd));
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      /* $1 is already in (len . str) format, create (-1 . node) for expression */
                      node *expr_elem = cons(int_to_node(-1), (yyvsp[-1].nd));
                      info->doc = push(push(info->doc, (yyvsp[-3].nd)), expr_elem);
                    }
#line 11000 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 498: /* words: tWORDS_BEG tSTRING  */
#line 4153 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 11008 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499: /* words: tWORDS_BEG string_rep tSTRING  */
#line 4157 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      n = push(n, (yyvsp[0].nd));
                      (yyval.nd) = new_words(p, n);
                    }
#line 11018 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 500: /* symbol: basic_symbol  */
#line 4165 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 11026 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501: /* symbol: "symbol" "string literal" string_rep tSTRING  */
#line 4169 "mrbgems/mruby-compiler/core/parse.y"
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
#line 11042 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502: /* symbol: "symbol" "numbered parameter"  */
#line 4181 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[0].num));
                      (yyval.nd) = new_sym(p, sym);
                    }
#line 11051 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 503: /* basic_symbol: "symbol" sym  */
#line 4188 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_END;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11060 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508: /* sym: tSTRING  */
#line 4199 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 11068 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509: /* sym: "string literal" tSTRING  */
#line 4203 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 11076 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510: /* symbols: tSYMBOLS_BEG tSTRING  */
#line 4209 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 11084 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511: /* symbols: tSYMBOLS_BEG string_rep tSTRING  */
#line 4213 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      n = push(n, (yyvsp[0].nd));
                      (yyval.nd) = new_symbols(p, n);
                    }
#line 11094 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514: /* numeric: tUMINUS_NUM "integer literal"  */
#line 4223 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 11102 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515: /* numeric: tUMINUS_NUM "float literal"  */
#line 4227 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 11110 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516: /* variable: "local variable or method"  */
#line 4233 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 11118 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517: /* variable: "instance variable"  */
#line 4237 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 11126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518: /* variable: "global variable"  */
#line 4241 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 11134 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519: /* variable: "class variable"  */
#line 4245 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 11142 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520: /* variable: "constant"  */
#line 4249 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 11150 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521: /* var_lhs: variable  */
#line 4255 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 11158 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 522: /* var_lhs: "numbered parameter"  */
#line 4259 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 11166 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523: /* var_ref: variable  */
#line 4265 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 11174 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524: /* var_ref: "numbered parameter"  */
#line 4269 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 11182 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525: /* var_ref: "'nil'"  */
#line 4273 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 11190 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526: /* var_ref: "'self'"  */
#line 4277 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 11198 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527: /* var_ref: "'true'"  */
#line 4281 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 11206 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528: /* var_ref: "'false'"  */
#line 4285 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 11214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529: /* var_ref: "'__FILE__'"  */
#line 4289 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, cons(cons(int_to_node(strlen(fn)), (node*)fn), (node*)NULL));
                    }
#line 11226 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530: /* var_ref: "'__LINE__'"  */
#line 4297 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 11237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531: /* var_ref: "'__ENCODING__'"  */
#line 4304 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, MRB_SYM_2(p->mrb, __ENCODING__), 0);
                    }
#line 11245 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534: /* superclass: %empty  */
#line 4314 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 11253 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535: /* $@31: %empty  */
#line 4318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 11262 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536: /* superclass: '<' $@31 expr_value term  */
#line 4323 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539: /* f_arglist_paren: '(' f_args rparen  */
#line 4339 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 11280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540: /* f_arglist_paren: '(' f_arg ',' tBDOT3 rparen  */
#line 4345 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 11288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541: /* f_arglist_paren: '(' tBDOT3 rparen  */
#line 4349 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 11296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543: /* f_arglist: f_args term  */
#line 4356 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11304 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544: /* f_arglist: f_arg ',' tBDOT3 term  */
#line 4360 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 11312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545: /* f_arglist: "..." term  */
#line 4364 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 11320 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546: /* f_label: "local variable or method" "label"  */
#line 4370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 11328 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547: /* f_label: "numbered parameter" "label"  */
#line 4374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 11336 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548: /* f_kw: f_label arg  */
#line 4380 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11346 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549: /* f_kw: f_label  */
#line 4386 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 11355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550: /* f_block_kw: f_label primary_value  */
#line 4393 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11365 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551: /* f_block_kw: f_label  */
#line 4399 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 11374 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552: /* f_block_kwarg: f_block_kw  */
#line 4406 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11382 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553: /* f_block_kwarg: f_block_kwarg ',' f_block_kw  */
#line 4410 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11390 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554: /* f_kwarg: f_kw  */
#line 4416 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11398 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555: /* f_kwarg: f_kwarg ',' f_kw  */
#line 4420 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11406 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558: /* f_kwrest: kwrest_mark "local variable or method"  */
#line 4430 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11414 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559: /* f_kwrest: kwrest_mark  */
#line 4434 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(pow);
                    }
#line 11422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560: /* args_tail: f_kwarg ',' f_kwrest opt_f_block_arg  */
#line 4440 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 11430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561: /* args_tail: f_kwarg opt_f_block_arg  */
#line 4444 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 11438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562: /* args_tail: f_kwrest opt_f_block_arg  */
#line 4448 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 11446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563: /* args_tail: f_block_arg  */
#line 4452 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 11454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564: /* opt_args_tail: ',' args_tail  */
#line 4458 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 11462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 565: /* opt_args_tail: %empty  */
#line 4462 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 11470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 566: /* f_args: f_arg ',' f_optarg ',' f_rest_arg opt_args_tail  */
#line 4468 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11478 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 567: /* f_args: f_arg ',' f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4472 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 568: /* f_args: f_arg ',' f_optarg opt_args_tail  */
#line 4476 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 11494 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 569: /* f_args: f_arg ',' f_optarg ',' f_arg opt_args_tail  */
#line 4480 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11502 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 570: /* f_args: f_arg ',' f_rest_arg opt_args_tail  */
#line 4484 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11510 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 571: /* f_args: f_arg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4488 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11518 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572: /* f_args: f_arg opt_args_tail  */
#line 4492 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 11526 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573: /* f_args: f_optarg ',' f_rest_arg opt_args_tail  */
#line 4496 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11534 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574: /* f_args: f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4500 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11542 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 575: /* f_args: f_optarg opt_args_tail  */
#line 4504 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 11550 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 576: /* f_args: f_optarg ',' f_arg opt_args_tail  */
#line 4508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11558 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 577: /* f_args: f_rest_arg opt_args_tail  */
#line 4512 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11566 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 578: /* f_args: f_rest_arg ',' f_arg opt_args_tail  */
#line 4516 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 579: /* f_args: args_tail  */
#line 4520 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 11582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 580: /* f_args: %empty  */
#line 4524 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 11591 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 581: /* f_bad_arg: "constant"  */
#line 4531 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 11600 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 582: /* f_bad_arg: "instance variable"  */
#line 4536 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 11609 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 583: /* f_bad_arg: "global variable"  */
#line 4541 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 11618 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 584: /* f_bad_arg: "class variable"  */
#line 4546 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 11627 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 585: /* f_bad_arg: "numbered parameter"  */
#line 4551 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 11636 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 586: /* f_norm_arg: f_bad_arg  */
#line 4558 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 11644 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 587: /* f_norm_arg: "local variable or method"  */
#line 4562 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11653 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 588: /* f_arg_item: f_norm_arg  */
#line 4569 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 11661 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 589: /* @32: %empty  */
#line 4573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 11669 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 590: /* f_arg_item: tLPAREN @32 f_margs rparen  */
#line 4577 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_marg(p, (yyvsp[-1].nd));
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 11679 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 591: /* f_arg: f_arg_item  */
#line 4585 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11687 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 592: /* f_arg: f_arg ',' f_arg_item  */
#line 4589 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11695 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 593: /* f_opt_asgn: "local variable or method" '='  */
#line 4595 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 11705 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 594: /* f_opt: f_opt_asgn arg  */
#line 4603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(sym_to_node((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11715 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 595: /* f_block_opt: f_opt_asgn primary_value  */
#line 4611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(sym_to_node((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11725 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 596: /* f_block_optarg: f_block_opt  */
#line 4619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11733 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 597: /* f_block_optarg: f_block_optarg ',' f_block_opt  */
#line 4623 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11741 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 598: /* f_optarg: f_opt  */
#line 4629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11749 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 599: /* f_optarg: f_optarg ',' f_opt  */
#line 4633 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11757 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 602: /* f_rest_arg: restarg_mark "local variable or method"  */
#line 4643 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11766 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 603: /* f_rest_arg: restarg_mark  */
#line 4648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(mul);
                      local_add_f(p, (yyval.id));
                    }
#line 11775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 606: /* f_block_arg: blkarg_mark "local variable or method"  */
#line 4659 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11783 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 607: /* f_block_arg: blkarg_mark  */
#line 4663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(and);
                    }
#line 11791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 608: /* opt_f_block_arg: ',' f_block_arg  */
#line 4669 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 609: /* opt_f_block_arg: none  */
#line 4673 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 11807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 610: /* singleton: var_ref  */
#line 4679 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 11817 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 611: /* $@33: %empty  */
#line 4684 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 11823 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 612: /* singleton: '(' $@33 expr rparen  */
#line 4685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[-1].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 614: /* assoc_list: assocs trailer  */
#line 4693 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11840 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 615: /* assocs: assoc  */
#line 4699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11848 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 616: /* assocs: assocs comma assoc  */
#line 4703 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11856 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 617: /* assoc: arg "=>" arg  */
#line 4709 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 618: /* assoc: "local variable or method" "label" arg  */
#line 4715 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 11875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 619: /* assoc: "local variable or method" "label"  */
#line 4720 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), label_reference(p, (yyvsp[-1].id)));
                    }
#line 11883 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 620: /* assoc: "numbered parameter" "label"  */
#line 4724 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[-1].num));
                      (yyval.nd) = cons(new_sym(p, sym), label_reference(p, sym));
                    }
#line 11892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 621: /* assoc: "numbered parameter" "label" arg  */
#line 4729 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, intern_numparam((yyvsp[-2].num))), (yyvsp[0].nd));
                    }
#line 11901 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 622: /* assoc: string_fragment "label" arg  */
#line 4734 "mrbgems/mruby-compiler/core/parse.y"
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
#line 11921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 623: /* assoc: "**" arg  */
#line 4750 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 11930 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 624: /* assoc: "**"  */
#line 4755 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), new_lvar(p, intern_op(pow)));
                    }
#line 11938 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 637: /* call_op: '.'  */
#line 4781 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 11946 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 638: /* call_op: "&."  */
#line 4785 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 11954 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 640: /* call_op2: "::"  */
#line 4792 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 11962 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 649: /* term: ';'  */
#line 4813 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 11968 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 651: /* nl: '\n'  */
#line 4818 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 11977 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 655: /* none: %empty  */
#line 4830 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 11985 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 11989 "mrbgems/mruby-compiler/core/y.tab.c"

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

#line 4834 "mrbgems/mruby-compiler/core/parse.y"

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
#define IS_LABEL_POSSIBLE() ((p->lstate == EXPR_BEG && !cmd_state) || IS_ARG())
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
      p->lstate = EXPR_BEG;
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
