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


#line 2009 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 1952 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 2188 "mrbgems/mruby-compiler/core/y.tab.c"

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
  YYSYMBOL_p_var = 253,                    /* p_var  */
  YYSYMBOL_opt_rescue = 254,               /* opt_rescue  */
  YYSYMBOL_exc_list = 255,                 /* exc_list  */
  YYSYMBOL_exc_var = 256,                  /* exc_var  */
  YYSYMBOL_opt_ensure = 257,               /* opt_ensure  */
  YYSYMBOL_literal = 258,                  /* literal  */
  YYSYMBOL_string = 259,                   /* string  */
  YYSYMBOL_string_fragment = 260,          /* string_fragment  */
  YYSYMBOL_string_rep = 261,               /* string_rep  */
  YYSYMBOL_string_interp = 262,            /* string_interp  */
  YYSYMBOL_263_29 = 263,                   /* @29  */
  YYSYMBOL_xstring = 264,                  /* xstring  */
  YYSYMBOL_regexp = 265,                   /* regexp  */
  YYSYMBOL_heredoc = 266,                  /* heredoc  */
  YYSYMBOL_heredoc_bodies = 267,           /* heredoc_bodies  */
  YYSYMBOL_heredoc_body = 268,             /* heredoc_body  */
  YYSYMBOL_heredoc_string_rep = 269,       /* heredoc_string_rep  */
  YYSYMBOL_heredoc_string_interp = 270,    /* heredoc_string_interp  */
  YYSYMBOL_271_30 = 271,                   /* @30  */
  YYSYMBOL_words = 272,                    /* words  */
  YYSYMBOL_symbol = 273,                   /* symbol  */
  YYSYMBOL_basic_symbol = 274,             /* basic_symbol  */
  YYSYMBOL_sym = 275,                      /* sym  */
  YYSYMBOL_symbols = 276,                  /* symbols  */
  YYSYMBOL_numeric = 277,                  /* numeric  */
  YYSYMBOL_variable = 278,                 /* variable  */
  YYSYMBOL_var_lhs = 279,                  /* var_lhs  */
  YYSYMBOL_var_ref = 280,                  /* var_ref  */
  YYSYMBOL_backref = 281,                  /* backref  */
  YYSYMBOL_superclass = 282,               /* superclass  */
  YYSYMBOL_283_31 = 283,                   /* $@31  */
  YYSYMBOL_f_opt_arglist_paren = 284,      /* f_opt_arglist_paren  */
  YYSYMBOL_f_arglist_paren = 285,          /* f_arglist_paren  */
  YYSYMBOL_f_arglist = 286,                /* f_arglist  */
  YYSYMBOL_f_label = 287,                  /* f_label  */
  YYSYMBOL_f_kw = 288,                     /* f_kw  */
  YYSYMBOL_f_block_kw = 289,               /* f_block_kw  */
  YYSYMBOL_f_block_kwarg = 290,            /* f_block_kwarg  */
  YYSYMBOL_f_kwarg = 291,                  /* f_kwarg  */
  YYSYMBOL_kwrest_mark = 292,              /* kwrest_mark  */
  YYSYMBOL_f_kwrest = 293,                 /* f_kwrest  */
  YYSYMBOL_args_tail = 294,                /* args_tail  */
  YYSYMBOL_opt_args_tail = 295,            /* opt_args_tail  */
  YYSYMBOL_f_args = 296,                   /* f_args  */
  YYSYMBOL_f_bad_arg = 297,                /* f_bad_arg  */
  YYSYMBOL_f_norm_arg = 298,               /* f_norm_arg  */
  YYSYMBOL_f_arg_item = 299,               /* f_arg_item  */
  YYSYMBOL_300_32 = 300,                   /* @32  */
  YYSYMBOL_f_arg = 301,                    /* f_arg  */
  YYSYMBOL_f_opt_asgn = 302,               /* f_opt_asgn  */
  YYSYMBOL_f_opt = 303,                    /* f_opt  */
  YYSYMBOL_f_block_opt = 304,              /* f_block_opt  */
  YYSYMBOL_f_block_optarg = 305,           /* f_block_optarg  */
  YYSYMBOL_f_optarg = 306,                 /* f_optarg  */
  YYSYMBOL_restarg_mark = 307,             /* restarg_mark  */
  YYSYMBOL_f_rest_arg = 308,               /* f_rest_arg  */
  YYSYMBOL_blkarg_mark = 309,              /* blkarg_mark  */
  YYSYMBOL_f_block_arg = 310,              /* f_block_arg  */
  YYSYMBOL_opt_f_block_arg = 311,          /* opt_f_block_arg  */
  YYSYMBOL_singleton = 312,                /* singleton  */
  YYSYMBOL_313_33 = 313,                   /* $@33  */
  YYSYMBOL_assoc_list = 314,               /* assoc_list  */
  YYSYMBOL_assocs = 315,                   /* assocs  */
  YYSYMBOL_assoc = 316,                    /* assoc  */
  YYSYMBOL_operation = 317,                /* operation  */
  YYSYMBOL_operation2 = 318,               /* operation2  */
  YYSYMBOL_operation3 = 319,               /* operation3  */
  YYSYMBOL_dot_or_colon = 320,             /* dot_or_colon  */
  YYSYMBOL_call_op = 321,                  /* call_op  */
  YYSYMBOL_call_op2 = 322,                 /* call_op2  */
  YYSYMBOL_opt_terms = 323,                /* opt_terms  */
  YYSYMBOL_opt_nl = 324,                   /* opt_nl  */
  YYSYMBOL_rparen = 325,                   /* rparen  */
  YYSYMBOL_trailer = 326,                  /* trailer  */
  YYSYMBOL_term = 327,                     /* term  */
  YYSYMBOL_nl = 328,                       /* nl  */
  YYSYMBOL_terms = 329,                    /* terms  */
  YYSYMBOL_none = 330                      /* none  */
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
#define YYLAST   13722

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  182
/* YYNRULES -- Number of rules.  */
#define YYNRULES  639
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1118

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
       0,  2126,  2126,  2126,  2136,  2142,  2146,  2150,  2154,  2160,
    2162,  2161,  2175,  2201,  2207,  2211,  2215,  2219,  2225,  2225,
    2229,  2233,  2237,  2241,  2250,  2259,  2263,  2268,  2269,  2273,
    2277,  2281,  2285,  2291,  2294,  2298,  2302,  2306,  2310,  2314,
    2319,  2323,  2332,  2341,  2350,  2359,  2366,  2367,  2371,  2374,
    2375,  2379,  2383,  2387,  2391,  2394,  2404,  2403,  2418,  2427,
    2428,  2431,  2432,  2439,  2438,  2453,  2457,  2462,  2466,  2471,
    2475,  2480,  2484,  2488,  2492,  2496,  2502,  2506,  2512,  2513,
    2519,  2523,  2527,  2531,  2535,  2539,  2543,  2547,  2551,  2555,
    2561,  2562,  2568,  2572,  2578,  2582,  2588,  2592,  2596,  2600,
    2604,  2608,  2614,  2620,  2627,  2631,  2635,  2639,  2643,  2647,
    2653,  2659,  2664,  2670,  2674,  2677,  2681,  2685,  2692,  2693,
    2694,  2695,  2700,  2707,  2708,  2711,  2715,  2715,  2721,  2722,
    2723,  2724,  2725,  2726,  2727,  2728,  2729,  2730,  2731,  2732,
    2733,  2734,  2735,  2736,  2737,  2738,  2739,  2740,  2741,  2742,
    2743,  2744,  2745,  2746,  2747,  2748,  2749,  2750,  2753,  2753,
    2753,  2754,  2754,  2755,  2755,  2755,  2756,  2756,  2756,  2756,
    2757,  2757,  2757,  2758,  2758,  2758,  2759,  2759,  2759,  2759,
    2760,  2760,  2760,  2760,  2761,  2761,  2761,  2761,  2762,  2762,
    2762,  2762,  2763,  2763,  2763,  2763,  2764,  2764,  2767,  2771,
    2775,  2779,  2783,  2787,  2791,  2796,  2801,  2806,  2810,  2814,
    2818,  2822,  2826,  2830,  2834,  2838,  2842,  2846,  2850,  2854,
    2858,  2862,  2866,  2870,  2874,  2878,  2882,  2886,  2890,  2894,
    2898,  2902,  2906,  2910,  2914,  2918,  2922,  2926,  2930,  2934,
    2938,  2942,  2946,  2950,  2954,  2963,  2972,  2981,  2990,  2996,
    2997,  3001,  3005,  3011,  3015,  3022,  3026,  3035,  3052,  3053,
    3056,  3057,  3058,  3062,  3066,  3072,  3077,  3081,  3085,  3089,
    3095,  3095,  3106,  3110,  3116,  3120,  3126,  3129,  3134,  3138,
    3142,  3147,  3151,  3157,  3162,  3166,  3172,  3173,  3177,  3181,
    3182,  3183,  3184,  3185,  3190,  3189,  3201,  3205,  3200,  3210,
    3210,  3214,  3218,  3222,  3226,  3230,  3234,  3238,  3242,  3246,
    3250,  3254,  3255,  3261,  3268,  3260,  3281,  3289,  3297,  3297,
    3297,  3304,  3304,  3304,  3311,  3317,  3321,  3331,  3333,  3330,
    3342,  3340,  3358,  3363,  3356,  3380,  3378,  3394,  3404,  3415,
    3419,  3423,  3427,  3433,  3440,  3441,  3442,  3445,  3446,  3449,
    3450,  3458,  3459,  3465,  3469,  3472,  3476,  3480,  3484,  3489,
    3493,  3497,  3501,  3507,  3506,  3516,  3520,  3524,  3528,  3534,
    3539,  3544,  3548,  3552,  3556,  3560,  3564,  3568,  3572,  3576,
    3580,  3584,  3588,  3592,  3596,  3600,  3606,  3611,  3618,  3618,
    3622,  3627,  3633,  3637,  3643,  3644,  3647,  3652,  3655,  3659,
    3665,  3669,  3676,  3675,  3692,  3697,  3701,  3706,  3713,  3717,
    3721,  3725,  3729,  3733,  3737,  3741,  3745,  3752,  3751,  3766,
    3765,  3781,  3789,  3798,  3802,  3806,  3814,  3817,  3818,  3824,
    3825,  3831,  3832,  3836,  3840,  3844,  3848,  3852,  3856,  3860,
    3864,  3870,  3876,  3883,  3886,  3890,  3891,  3894,  3898,  3901,
    3905,  3908,  3909,  3910,  3911,  3914,  3915,  3921,  3926,  3931,
    3936,  3942,  3943,  3949,  3955,  3954,  3966,  3970,  3976,  3980,
    3986,  3995,  4006,  4009,  4010,  4013,  4019,  4025,  4026,  4029,
    4036,  4035,  4050,  4054,  4062,  4066,  4078,  4085,  4092,  4093,
    4094,  4095,  4096,  4100,  4106,  4110,  4118,  4119,  4120,  4124,
    4130,  4134,  4138,  4142,  4146,  4152,  4156,  4162,  4166,  4170,
    4174,  4178,  4182,  4186,  4194,  4201,  4207,  4208,  4212,  4216,
    4215,  4232,  4233,  4236,  4242,  4246,  4252,  4253,  4257,  4261,
    4267,  4271,  4277,  4283,  4290,  4296,  4303,  4307,  4313,  4317,
    4323,  4324,  4327,  4331,  4337,  4341,  4345,  4349,  4355,  4360,
    4365,  4369,  4373,  4377,  4381,  4385,  4389,  4393,  4397,  4401,
    4405,  4409,  4413,  4417,  4422,  4428,  4433,  4438,  4443,  4448,
    4455,  4459,  4466,  4471,  4470,  4482,  4486,  4492,  4500,  4508,
    4516,  4520,  4526,  4530,  4536,  4537,  4540,  4545,  4552,  4553,
    4556,  4560,  4566,  4570,  4576,  4582,  4582,  4589,  4590,  4596,
    4600,  4606,  4612,  4617,  4621,  4626,  4631,  4647,  4652,  4658,
    4659,  4660,  4663,  4664,  4665,  4666,  4669,  4670,  4671,  4674,
    4675,  4678,  4682,  4688,  4689,  4695,  4696,  4699,  4700,  4703,
    4706,  4707,  4708,  4711,  4712,  4715,  4720,  4723,  4724,  4728
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
  "in_clauses", "p_expr", "p_as", "p_alt", "p_value", "p_var",
  "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal", "string",
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

#define YYPACT_NINF (-889)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-640)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -889,  4018,   146,  8794, 10918, 11260,  7335,  -889, 10564, 10564,
    -889,  -889, 11032,  8398,  6951,  9030,  9030,  -889,  -889,  9030,
    4550,  4142,  -889,  -889,  -889,  -889,   -33,  8398,  -889,    70,
    -889,  -889,  -889,  7477,  3586,  -889,  -889,  7619,  -889,  -889,
    -889,  -889,  -889,  -889,  -889,   235, 10682, 10682, 10682, 10682,
      82,  6210,  1617,  9502,  9856,  3737,  -889,  8116,  1063,   998,
    1348,  1353,  1388,  -889,   366, 10800, 10682,  -889,  1145,  -889,
     995,  -889,   410,  1489,  1489,  -889,  -889,   193,    57,  -889,
      93, 11146,  -889,   141, 13403,   156,   349,    76,    45,  -889,
     262,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,
     229,   174,  -889,   302,   139,  -889,  -889,  -889,  -889,  -889,
    -889,   180,   180,   -33,    58,   493,  -889, 10564,   119,  6329,
     491,  1340,  1340,  -889,   184,  -889,   412,  -889,  -889,   139,
    -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,
    -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,
    -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,    44,    62,
      87,    94,  -889,  -889,  -889,  -889,  -889,  -889,   100,   102,
     170,   186,  -889,   200,  -889,  -889,  -889,  -889,  -889,  -889,
    -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,
    -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,
    -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,   220,
    5242,   271,   410,  1489,  1489,    96,   215, 13527,   438,   298,
     250,   325,    96, 10564, 10564,   463,   291,  -889,  -889,   512,
     324,    89,   126,  -889,  -889,  -889,  -889,  -889,  -889,  -889,
    -889,  -889,  8257,  -889,  -889,   269,  -889,  -889,  -889,  -889,
    -889,  -889,  1145,  -889,   732,  -889,   372,  -889,  -889,  1145,
    4278,   165, 10682, 10682, 10682, 10682,  -889, 13465,  -889,  -889,
     273,   363,   273,  -889,  -889,  -889,  9148,  -889,  -889,  9030,
    -889,  -889,  -889,  -889,  6951,  7189,  -889,   288,  6448,  -889,
     567,   327, 13589, 13589,   308,  8912,  6210,   285,  1145,   995,
    1145,   313,  -889,  8912,  1145,   315,  1431,  1431,  -889, 13465,
     321,  1431,  -889,   407, 11374,   322,   657,   659,   823,  1646,
    -889,  -889,  -889,  -889,  -889,  1468,  -889,  -889,  -889,  -889,
    -889,  -889,   745,  1498,  -889,  -889,  1281,  -889,  1371,  -889,
    1532,  -889,  1539,   365,   367,  -889,  -889,  -889,  -889,  6713,
   10564, 10564, 10564, 10564,  8912, 10564, 10564,    59,  -889,  -889,
    -889,  -889,   416,  1145,  -889,  -889,  -889,  -889,  -889,  -889,
    -889,  1234,   350,   401,  5242, 10682,  -889,   333,   475,   391,
    -889,  1145,  -889,  -889,  -889,   415, 10682,  -889,   427,   521,
     434,   533,  -889,  -889,   478,  5242,  -889,  -889,  9974,  -889,
    6210,  8680,   474,  9974, 10682, 10682, 10682, 10682, 10682, 10682,
   10682, 10682, 10682, 10682, 10682, 10682, 10682, 10682,   568, 10682,
   10682, 10682, 10682, 10682, 10682, 10682, 10682, 10682, 10682, 10682,
   10682,  3111,  -889,  9030,  -889, 11766,  -889,  -889, 13056,  -889,
    -889,  -889,  -889, 10800, 10800,  -889,   525,  -889,   410,  -889,
     859,  -889,  -889,  -889,  -889,  -889,  -889, 11852,  9030, 11938,
    5242, 10564,  -889,  -889,  -889,   614,   619,   426,   515,   522,
    -889,  5388,   630, 10682, 12024,  9030, 12110, 10682, 10682,  5826,
     389,   389,   136, 12196,  9030, 12282,  -889,   592,  -889,  6448,
     286,  -889,  -889, 10092,   645,  -889, 10682, 10682, 13527, 13527,
   13527, 10682,  -889,  -889,  9266,  -889, 10682,  -889,  9620,  7070,
     528,  1145,   273,   273,  -889,  -889,  1049,   538,  -889,  -889,
    -889,  8398,  5945,   547, 12024, 12110, 10682,   995,  1145,  -889,
    -889,  6832,   550,   995,  -889,  -889,  9738,  -889,  1145,  9856,
    -889,  -889,  -889,   859,    93, 11374,  -889, 11374, 12368,  9030,
   12454,  1911,  -889,  -889,   552,  -889,  1544,  6448,   745,  -889,
    -889,  -889,  -889,  -889,  -889,  -889, 10682, 10682,  -889,  -889,
    -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,
    1672,  1145,  1145,   558, 10800,   689, 13527,   893,  -889,  -889,
    -889,    32,  -889,  -889,  1426,  -889, 13527,  1911,  -889,  -889,
    1298,  -889,  -889, 10800,   691,    74, 10682,  -889,  3867,   273,
    -889,  1145, 11374,   566,  -889,  -889,  -889,   669,   603,  1549,
    -889,  -889,   993,   453,  2480,  2480,  2480,  2480,  1794,  1794,
    3255,  2118,  2480,  2480, 13589, 13589,  1759,  1759,  -889,   327,
   13527,  1794,  1794,  1654,  1654,  1817,   354,   354,   327,   327,
     327,  4686,  7856,  4958,  7974,  -889,   180,  -889,   585,   273,
     371,  -889,   444,  -889,  -889,  4414,  -889,  -889,  1916,    74,
      74,  -889,  2769,  -889,  -889,  -889,  -889,  -889,  1145, 10564,
    5242,   651,   230,  -889,   180,   593,   180,   733,  1049,  8539,
    -889, 10210,   736,  -889, 10682, 10682,   574,  -889,  2908,  7737,
     628,   535,   559,   736,  -889,  -889,  -889,  -889,    55,   111,
     634,   137,   147, 10564,  8398,   621, 11488,   761, 13527,   303,
    -889, 13527, 13527, 13527,   803, 10682, 13465,  -889,   273, 13527,
    -889,  -889,  -889,  -889,  9384,  9620,  -889,  -889,  -889,   641,
    -889,  -889,    34,   995,  1145,  1431,   474,  -889,   651,   230,
     647,   667,   697,  -889,    72,  1911,  -889,   643,  -889,   327,
     327,  -889,  -889,   594,  1145,   648,  -889,  -889,  1753,   741,
    3780,  -889,   735,   416,  -889,   391,  -889,  1145,  -889,  -889,
     654,   658,   682,  -889,   685,   735,   682,   782, 13128,  -889,
    -889,  1911,  5242,  -889,  -889, 13190, 10328,  -889,  -889, 11374,
    8912, 10800, 10682, 12540,  9030, 12626,   853, 10800, 10800,  -889,
     525,   469,  9266, 10800, 10800,  -889,   525,    45,   193,  5242,
    6448,    74,  -889,  1145,   816,  -889,  -889,  -889,  -889,  3867,
    -889,   739,  -889,  6091,   820,  -889, 10564,   831,  -889, 10682,
   10682,   609, 10682, 10682,   841,  6594,  6594,   150,   389,  -889,
    -889,  1047,  1052,  1079,    58,   493,   789,   812,  1098,    96,
    -889,   113,  -889,  -889,  1115,  1168,  -889, 10446,  5534, 13527,
    -889,  7070,   273,  -889,  -889,  -889,   194,   728,  1550,  5242,
    6448,  -889,  -889,  -889,   734,  -889,  1725,  1145, 10682, 10682,
    -889,  -889,  1911,  -889,  1298,  -889,  1298,  -889,  1298,  -889,
    -889, 10682, 10682,  -889,  -889,  -889, 11602,  -889,   742,   391,
     755, 11602,  -889,   756,   757,  -889,   860, 10682, 13261,  -889,
    -889, 13527,  4822,  5094,   740,   652,   660, 10682, 10682,  -889,
    -889,  -889,  -889,  -889, 10800,  -889,  -889,  -889,  -889,  -889,
    -889,  -889,   868,   748,  6448,  5242,  -889,  -889, 11716,    96,
    -889,  -889,  6594,  -889,  -889,    96,  -889, 10682,  -889,   888,
     890,  -889,   859, 12712,  5680,   855, 11488, 13527,   225,  -889,
    9620,  -889,  1742,   896,   775,  1934,  1934,  1065,  -889, 13527,
   13527,   682,   774,   682,   682, 13527, 13527,   793,   797,   869,
    1171,   893,  -889,  -889,  1181,  -889,  1171,  1911,  -889,  1298,
    -889,  -889, 13332,   694, 13527, 13527,  -889,  -889,  -889,  -889,
     788,   915,   878,  -889,  1226,   659,   823,  5242,  -889,  5388,
    -889,  -889,  6594,   791,   230,   129,  -889,  -889,  -889,  -889,
    -889,  -889,   796,  -889,  -889,  -889,  -889,   799,   799,  1934,
     800,  -889,  1298,  -889,  -889,  -889,  -889,  -889,  -889, 12798,
    -889,   391,   893,  -889,  -889,   806,   817,   824,  -889,   827,
     824,  -889,  -889,   859, 12884,  9030, 12970,   619,   574,   924,
   11488,  -889,   930,  1742,   803,  1934,   799,  1934,   682,   821,
    -889,  1911,  -889,  1298,  -889,  1298,  -889,  1298,  -889,  -889,
     651,   230,   838,   747,   837,  -889,  -889,  -889,    96,  -889,
    -889,   799,  -889,   824,   847,   824,   824,   194,  5680,  -889,
    1298,  -889,  -889,  -889,   129,   824,  -889,  -889
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     0,     0,     0,     0,   294,     0,     0,
     318,   321,     0,     0,   625,   339,   340,   341,   342,   306,
     270,   270,   510,   509,   511,   512,   627,     0,    10,     0,
     514,   513,   515,   500,   611,   502,   501,   504,   503,   496,
     497,   457,   458,   516,   517,   508,     0,     0,     0,     0,
       0,     0,   296,   639,   639,    88,   313,     0,     0,     0,
       0,     0,     0,   472,     0,     0,     0,     3,   625,     6,
       9,    27,    33,   564,   564,    49,    60,    59,     0,    76,
       0,    80,    90,     0,    54,   248,     0,    61,   311,   286,
     287,   455,   288,   289,   290,   453,   452,   484,   454,   451,
     507,     0,   291,   292,   270,     5,     1,     8,   339,   340,
     306,   639,   415,     0,   113,   114,   508,     0,     0,     0,
       0,   564,   564,   116,   518,   343,     0,   507,   292,     0,
     335,   168,   178,   169,   165,   194,   195,   196,   197,   176,
     191,   184,   174,   173,   189,   172,   171,   167,   192,   166,
     179,   183,   185,   177,   170,   186,   193,   188,   187,   180,
     190,   175,   164,   182,   181,   163,   161,   162,   158,   159,
     160,   118,   120,   119,   153,   154,   131,   132,   133,   140,
     137,   139,   134,   135,   155,   156,   141,   142,   146,   149,
     150,   136,   138,   128,   129,   130,   143,   144,   145,   147,
     148,   151,   152,   157,   595,    55,   121,   122,   594,     0,
       0,     0,    58,   564,   564,     0,     0,    54,     0,   507,
       0,   292,     0,     0,     0,   112,     0,   354,   353,     0,
       0,   507,   292,   187,   180,   190,   175,   158,   159,   160,
     118,   119,     0,   123,   125,    20,   124,   475,   480,   479,
     633,   635,   625,   636,     0,   477,     0,   637,   634,   626,
     609,   508,   278,   608,   273,     0,   265,   277,    74,   269,
     639,   455,   639,   599,    75,    73,   639,   259,   307,     0,
      72,   258,   414,    71,   625,     0,    18,     0,     0,   221,
       0,   222,   209,   212,   303,     0,     0,     0,   625,    15,
     625,    78,    14,     0,   625,     0,   630,   630,   249,     0,
       0,   630,   597,     0,     0,    86,     0,    96,   103,   564,
     490,   489,   491,   492,   486,     0,   488,   487,   459,   464,
     463,   466,     0,     0,   461,   468,     0,   470,     0,   482,
       0,   494,     0,   498,   499,    53,   236,   237,     4,   626,
       0,     0,     0,     0,     0,     0,     0,   571,   567,   566,
     565,   568,   569,     0,   573,   585,   540,   541,   589,   588,
     584,   564,     0,   526,     0,   533,   538,   639,   543,   639,
     563,     0,   570,   572,   575,   549,     0,   582,   549,   587,
     549,   591,   547,   522,     0,     0,   402,   404,     0,    92,
       0,    84,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   208,   211,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   622,   639,   621,     0,   624,   623,     0,   419,
     417,   312,   456,     0,     0,   408,    65,   310,   332,   113,
     114,   115,   498,   499,   526,   519,   330,     0,   639,     0,
       0,     0,   620,   619,    56,     0,   639,   303,     0,     0,
     345,     0,   344,     0,     0,   639,     0,     0,     0,     0,
       0,     0,   303,     0,   639,     0,   327,     0,   126,     0,
       0,   476,   478,     0,     0,   638,   603,   604,   279,   607,
     272,     0,   627,   266,     0,   275,     0,   267,     0,   625,
       0,   625,   639,   639,   260,   271,   625,     0,   309,    52,
     628,     0,     0,     0,     0,     0,     0,    17,   625,   301,
      13,   626,    77,   297,   300,   304,   632,   250,   631,   632,
     252,   305,   598,   102,    94,     0,    89,     0,     0,   639,
       0,   564,   314,   399,   549,   493,     0,     0,   467,   473,
     460,   462,   469,   471,   483,   495,     0,     0,     7,    21,
      22,    23,    24,    25,    50,    51,   530,   577,   531,   529,
       0,   625,   625,   549,     0,     0,   532,     0,   545,   593,
     542,     0,   546,   527,     0,   556,   578,     0,   559,   586,
       0,   561,   590,     0,     0,   639,   278,    28,    30,     0,
      31,   625,     0,    82,    93,    48,    34,    46,     0,   253,
     198,    29,     0,   292,   226,   231,   232,   233,   228,   230,
     240,   241,   234,   235,   207,   210,   238,   239,    32,   218,
     627,   227,   229,   223,   224,   225,   213,   214,   215,   216,
     217,   612,   617,   613,   618,   413,   270,   411,     0,   639,
     612,   614,   613,   615,   412,   270,   612,   613,   270,   639,
     639,    35,   253,   199,    45,   206,    63,    66,     0,     0,
       0,   113,   114,   117,     0,     0,   639,     0,   625,     0,
     295,   639,   639,   443,     0,     0,   639,   346,   616,   302,
       0,   612,   613,   639,   348,   319,   347,   322,   616,   302,
       0,   612,   613,     0,     0,     0,     0,     0,   277,     0,
     325,   602,   605,   601,   276,   281,   280,   274,   639,   606,
     600,   257,   255,   261,   262,   264,   308,   629,    19,     0,
      26,   205,    79,    16,   625,   630,    95,    87,    99,   101,
       0,    98,   100,   627,     0,     0,   485,     0,   474,   219,
     220,   571,   569,   362,   625,   355,   525,   523,     0,    41,
     244,   337,     0,     0,   539,   639,   592,     0,   548,   576,
     549,   549,   549,   583,   549,   571,   549,    43,   246,   338,
     390,   388,     0,   387,   386,   285,     0,    91,    85,     0,
       0,     0,     0,     0,   639,     0,     0,     0,     0,   410,
      69,   416,   262,     0,     0,   409,    67,   405,    62,     0,
       0,   639,   333,     0,     0,   416,   336,   596,    57,   444,
     445,   639,   446,     0,   639,   351,     0,     0,   349,     0,
       0,   416,     0,     0,     0,     0,     0,   416,     0,   127,
     481,   435,   436,   437,   441,   438,   434,     0,     0,     0,
     426,   427,   429,   431,   433,   432,   324,     0,     0,   282,
     268,   625,   639,    11,   298,   251,    97,     0,   392,     0,
       0,   315,   465,   363,   360,   574,     0,   625,     0,     0,
     544,   528,     0,   552,     0,   554,     0,   560,     0,   557,
     562,     0,     0,   385,   627,   627,   535,   536,   639,   639,
     370,     0,   580,   370,   370,   368,     0,   281,   283,    83,
      47,   254,   612,   613,     0,   612,   613,     0,     0,    40,
     203,    39,   204,    70,     0,    37,   201,    38,   202,    68,
     406,   407,     0,     0,     0,     0,   520,   331,     0,     0,
     448,   352,     0,    12,   450,     0,   316,     0,   317,     0,
       0,   328,   440,     0,     0,     0,     0,   280,   639,   256,
     263,   398,     0,     0,     0,     0,     0,   358,   524,    42,
     245,   549,   549,   549,   549,    44,   247,     0,     0,     0,
     534,     0,   366,   367,   370,   378,   579,     0,   381,     0,
     383,   403,   284,   416,   243,   242,    36,   200,   420,   418,
       0,     0,     0,   447,     0,   104,   111,     0,   449,     0,
     320,   323,     0,   616,   439,   639,   428,   430,   422,   423,
     421,   396,   627,   394,   397,   401,   400,   364,   361,     0,
     356,   553,     0,   550,   555,   558,   391,   389,   303,     0,
     537,   639,     0,   369,   376,   370,   370,   370,   581,   370,
     370,    64,   334,   110,     0,   639,     0,   639,   639,     0,
       0,   424,     0,     0,   393,     0,   359,     0,   549,   302,
     365,     0,   373,     0,   375,     0,   382,     0,   379,   384,
     107,   109,     0,   612,   613,   442,   350,   329,     0,   326,
     395,   357,   551,   370,   370,   370,   370,   105,     0,   374,
       0,   371,   377,   380,   639,   370,   425,   372
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -889,  -889,  -889,   471,  -889,    40,  -889,  -292,   279,  -889,
    -889,    48,  -179,  -206,   662,  1029,  -889,  1356,     2,   -30,
    -889,  -889,  -502,    39,   979,  -197,    -9,   -66,  -275,  -497,
     -26,  2270,   -72,   997,    31,   -18,  -889,  -889,    56,  -889,
    3086,  -889,   414,    75,  -320,  -399,   114,  -889,    19,  -406,
    -246,   -54,   103,  -311,    26,  -889,  -889,  -889,  -889,  -889,
    -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,  -889,
    -889,     9,  -220,  -476,   -68,  -624,  -889,  -889,  -889,   212,
     -12,  -889,  -565,  -889,  -889,  -166,  -889,   -65,  -889,  -889,
    -889,   191,  -889,  -889,  -889,   -85,  -889,  -463,  -889,  -103,
     -51,  -889,  -889,    50,  -889,   -47,  -889,  -889,  -889,  -889,
    -889,   105,    83,  -169,  -889,  -889,  -889,  -889,  -889,  -276,
    -889,   768,  -889,  -889,  -675,    -5,  -889,  -889,  -671,  1864,
    2466,  1017,  1683,  -889,  -889,    15,   644,     3,   219,   440,
      51,  -889,  -889,  -889,   159,    67,    88,  -244,  -872,  -676,
    -455,  -889,   380,  -743,  -553,  -888,    52,  -528,  -889,  -561,
    -889,    91,  -314,  -889,  -889,  -889,    78,  -461,   702,  -368,
    -889,  -889,   -80,  -889,    18,   -25,   684,  -250,   181,  -281,
     -48,    -1
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
     754,   223,   845,   224,   846,   713,  1022,   680,   678,   945,
     460,   290,   471,   705,   837,   838,   230,   764,   975,  1053,
     995,   904,   792,   905,   793,   877,  1032,  1033,   552,   881,
     605,   397,    87,    88,   670,   447,   669,   494,  1030,  1072,
     859,   860,   861,   862,   863,   692,   831,   949,   953,    89,
      90,    91,   333,   334,   557,    92,    93,    94,   558,   253,
     254,   255,   489,    95,    96,    97,   327,    98,    99,   219,
     220,   102,   221,   456,   679,   372,   373,   374,   375,   376,
     907,   908,   377,   378,   379,   778,   595,   381,   382,   383,
     384,   580,   385,   386,   387,   912,   913,   388,   389,   390,
     391,   392,   588,   209,   461,   310,   513,   273,   129,   684,
     657,   464,   459,   438,   517,   878,   518,   537,   257,   258,
     259,   302
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     105,   285,   479,   441,   520,   707,   435,   437,   246,   286,
      86,   222,    86,   126,   126,   402,   252,   218,   218,   281,
     349,   229,   246,   218,   218,   218,   507,   717,   218,   315,
     125,   125,   256,   781,   658,   345,   784,   205,   125,   545,
     283,   864,   301,   107,   783,   865,   451,   730,   911,    70,
     747,    70,   308,   312,   266,   266,   559,   540,   266,   685,
      86,   542,   206,   656,   316,   592,   780,   665,   834,   206,
     668,   439,   393,   393,   218,   553,   700,   395,   730,   844,
    -107,   125,   585,   206,  -609,   710,   348,   884,   326,   394,
     316,   686,   621,   272,   272,   277,   282,   272,   727,   528,
    1034,   879,   727,   604,   819,   820,   656,   125,   665,  1058,
     281,   284,   470,   206,  -104,   798,   576,   686,   270,   270,
     271,   271,   270,   446,   271,   395,   218,   582,    86,   268,
     274,   307,   311,   275,  -510,   368,  -109,   294,   476,   779,
     380,   380,   336,   338,   340,   342,   106,   833,  -500,   485,
     750,  -111,  -509,   790,  1070,   -77,   306,   686,   271,   271,
     369,  -110,  -106,  -500,   561,   439,   436,   561,   687,   561,
     449,   561,  -108,   561,   450,  -105,   -91,  -511,   398,   445,
     577,   432,   686,   440,  -512,   880,   277,   282,   380,   380,
    -514,  -510,  -513,  1058,   781,   442,  -609,   -99,  -500,  -612,
     791,  1034,  -609,   611,   445,  -500,   965,   781,   288,  -509,
     525,   247,   393,   393,   248,   249,   504,   395,   508,    86,
    -416,   396,   497,   434,   615,   480,   481,   780,   468,   469,
     914,   -96,   218,   218,  -511,   399,   550,   671,   674,   966,
     780,  -512,   250,   833,   251,   493,  -343,  -514,   544,  -513,
     531,   911,   536,  -101,   911,  -613,   944,  -506,   538,   538,
    -515,  -343,   403,   538,   615,   615,   443,   507,  -103,   505,
     490,   505,   545,   326,   730,   514,  -500,   440,  -102,   -98,
     380,   380,   758,   276,  -416,   218,  -112,   301,   218,  -100,
    -504,   864,   -97,   218,   218,   865,  -343,    86,   206,  -416,
     779,  1040,   919,  -343,    86,    86,   493,   753,   933,   455,
     462,   716,    86,   779,   939,   266,   530,  -515,   266,   470,
    -302,  -505,    41,   316,   276,    42,   467,  -506,   727,   727,
     297,   982,  -416,  -500,  -416,  -302,   473,   545,   911,   783,
     125,  -416,   477,   527,  1028,   815,   482,  -504,   817,   486,
    -104,   533,   569,   570,   571,   572,  -112,   272,    86,   218,
     218,   218,   218,    86,   218,   218,   815,   463,   607,    58,
    -302,   -96,   961,   617,  -613,   613,   589,  -302,   589,   512,
     830,   271,   270,    86,   271,   683,   380,   561,   824,   568,
    -505,   301,   493,   515,   444,   864,   472,    70,   297,   865,
     526,  1071,   573,   472,    86,   924,   727,   218,   556,    86,
     316,   488,   622,   617,   617,   502,   704,   478,   247,  -104,
     506,   248,   249,  -111,   343,   344,   522,   125,   419,  -110,
     529,   779,   514,  1056,   -76,   656,  1059,   665,   380,   431,
     495,   779,   218,   520,  -103,   502,  -111,   355,   356,   250,
    -102,   251,   622,   622,   432,   419,   535,   514,   734,   735,
     541,   890,   543,   813,   547,   693,   566,   218,   567,    86,
     218,   584,   266,   578,   514,   587,   598,   724,   601,   746,
      86,   544,   870,   514,   218,   428,   429,   430,    86,   433,
    1071,   663,  -106,   218,   663,   875,   434,   266,    86,   868,
     916,   609,   457,   738,   247,  1029,   609,   248,   249,   730,
     664,   505,   505,   -98,   266,   663,   246,   432,   526,  -610,
    1104,   105,  -521,   266,   545,   520,   590,   942,   474,   733,
     495,    86,   663,   591,   664,   250,   814,   251,   271,   779,
      86,   663,   805,   432,   579,   444,   544,  -110,   514,   452,
     453,   664,   458,  -508,   316,   796,   316,   594,   218,   434,
     664,   934,   593,   271,   727,  -108,    86,   523,  -508,   597,
      70,   125,   599,   125,  -111,   297,   600,   206,   475,   743,
     271,   663,   728,  -504,   602,   434,  -100,   973,   266,   271,
    -105,   836,   833,   218,   992,   993,   719,   520,  -504,   603,
     664,   929,   483,  -508,   794,   812,   663,   935,   937,   271,
    -508,   -97,   218,   271,   745,   806,   614,   432,   380,   638,
     779,   316,   615,   769,   676,   664,   870,   842,   615,   690,
     779,  -610,   691,  -504,   615,   615,   694,  -610,   125,   964,
    -504,   271,   787,   695,   271,   761,   697,   358,   359,   360,
     361,   843,   484,  1011,   271,   555,  -106,   524,   505,   434,
     720,   706,   706,   762,   281,   867,  1092,   281,   794,   794,
     212,   212,   432,   732,   508,   810,   212,  -616,   776,   297,
    -108,   823,   776,   737,   816,   281,   740,   818,   218,    86,
     832,   835,   -91,  -612,   755,   835,   849,   538,   686,   554,
     768,   957,   835,   104,   771,   104,   789,   475,   799,   246,
     104,   104,   495,   800,   434,   848,   104,   104,   104,   495,
     828,   104,   218,  -613,   801,   858,   811,   505,  1006,  1017,
    -105,   809,   940,   544,   825,  1019,   883,  1080,   987,   988,
     277,  -616,   125,   277,   813,   206,   775,   548,   826,  -507,
     696,   583,   814,   104,   833,   615,  -616,  -612,   703,   809,
     850,   277,   432,  -106,  -507,   454,   454,   104,   715,   841,
     206,   617,  -612,  -106,   589,   847,   866,   617,   931,   448,
     873,  -108,   882,   617,   617,   888,   934,  -613,   876,  -616,
     886,  -616,   576,   520,   609,  -612,   892,   549,  -616,  -507,
     894,    86,  -613,   514,   434,  -612,  -507,  -612,   316,    86,
     622,  -612,   872,   218,  -612,  -105,   622,   218,   970,   104,
     794,   104,   622,   622,   896,   125,   901,   898,    86,    86,
     950,   947,   948,   954,   952,  -613,   757,  -613,   955,   271,
     271,  -613,    86,   266,  -613,   218,   956,   491,   920,  -458,
     248,   249,  -458,  -108,    86,    86,   958,   673,   675,   822,
     247,   663,  -106,   248,   249,  -106,  -106,   962,   893,   895,
     897,   505,   899,   971,   900,  1001,   976,    86,  1108,  -458,
     664,  1003,   915,  1008,   991,   212,   212,  1009,    86,    86,
     872,   673,   675,  -106,  -458,  -106,  -458,   994,   997,   999,
     472,   998,  1000,  1020,   617,  1021,  1026,   589,   589,   271,
     927,  1035,   104,  -292,  1036,   990,  1042,   271,   247,  1046,
     996,   248,   249,  1047,  1048,   104,   104,  1061,  -292,  -458,
    1062,   554,   125,  1063,  1066,  -612,  -458,   125,  1073,  1097,
     741,  1075,  1077,   622,   772,  1099,   516,   519,  1081,  -303,
     909,   251,  -108,    86,    86,  -108,  -108,  1014,   891,  1083,
     765,    86,   773,  -292,  -303,  -613,  1085,   835,   247,  1087,
    -292,   248,   249,    86,   125,   858,   928,   782,   104,  1107,
     786,   104,  1054,  -108,   532,  -108,   104,   104,   534,  1110,
     104,   227,   125,   739,   366,   367,   368,   104,   104,  -303,
    1096,   251,   130,   903,   946,   104,  -303,  1074,  1100,   941,
     906,  1116,   212,   212,   212,   212,  1027,   574,   575,  1098,
    1095,   369,   492,   208,   835,     0,    86,   774,    86,   706,
      73,    86,    73,   121,   121,   350,   351,   352,   353,   354,
     472,   121,  1050,  1082,  1084,  1086,  1055,  1088,  1089,     0,
     589,   104,   104,   104,   104,   104,   104,   104,   104,   335,
       0,     0,   329,   330,   514,     0,   693,   835,     0,  1041,
    1043,  1044,  1045,     0,   218,   271,   104,     0,     0,   858,
      73,     0,   776,   803,   121,   915,   355,   356,   915,     0,
     915,  1109,  1111,  1112,  1113,     0,   125,   104,   432,   943,
     104,     0,   104,  1117,   266,   104,     0,     0,     0,     0,
     121,     0,   951,   835,   331,   332,   761,    86,   358,   359,
     360,   361,   663,   688,   959,   960,   328,   329,   330,     0,
     472,     0,     0,   804,   762,   104,   472,  -509,     0,     0,
     434,   664,  -511,   915,     0,   104,   104,   968,    73,     0,
    1051,     0,  -509,   909,     0,     0,   909,  -511,   909,   974,
     104,     0,   104,   104,   247,     0,  1102,   248,   249,  -512,
     271,   910,   915,   104,   915,     0,   915,   104,   915,   331,
     332,   104,     0,     0,  -512,     0,   104,  -509,   963,     0,
       0,   104,  -511,   731,  -509,   250,     0,   251,     0,  -511,
     736,   915,     0,   432,     0,  -452,     0,  1039,     0,     0,
     906,   909,   742,   906,     0,     0,   906,     0,   906,  -512,
    -452,   930,   932,  1010,   104,     0,  -512,   936,   938,     0,
       0,  1018,   357,   104,   358,   359,   360,   361,   458,    73,
     909,     0,   909,  1025,   909,   434,   909,     0,     0,     0,
     362,   104,     0,   930,   932,  -452,   936,   938,  -451,   104,
     247,  1049,  -452,   248,   249,   766,   767,     0,     0,   909,
       0,   906,   981,  -451,   983,   364,   432,     0,   984,   472,
       0,   365,   366,   367,   368,   357,   104,   358,   359,   360,
     361,   250,     0,   251,     0,   797,  1067,     0,  1068,     0,
     906,  1069,   906,   362,   906,   104,   906,     0,  -451,   369,
       0,   458,   370,     0,     0,  -451,  1064,    73,   434,   581,
       0,     0,     0,  1052,    73,    73,     0,     0,   364,   906,
       0,   432,    73,     0,   365,   366,   367,   368,     0,     0,
       0,   212,   562,   121,     0,   329,   330,     0,  1007,   785,
       0,   358,   359,   360,   361,  1037,  1038,    74,     0,    74,
     122,   122,   369,     0,     0,   370,  1065,   362,   122,     0,
       0,  1007,   827,   434,     0,   212,     0,  1057,    73,  1060,
       0,   104,   104,    73,     0,     0,     0,  1114,     0,     0,
       0,   357,   364,   358,   359,   360,   361,   331,   332,   366,
     367,   368,     0,    73,     0,     0,     0,    74,     0,   362,
     337,   122,   329,   330,     0,   104,   339,   329,   330,  1076,
       0,     0,  1078,   363,    73,     0,   369,     0,   874,    73,
     121,     0,    73,   563,   364,   329,   330,   122,     0,     0,
     365,   366,   367,   368,     0,     0,     0,     0,   885,     0,
       0,   341,   329,   330,     0,     0,     0,  1101,     0,     0,
       0,  1103,     0,  1105,   331,   332,     0,  1106,   369,   331,
     332,   370,    73,    73,     0,    74,     0,   357,     0,   358,
     359,   360,   361,     0,   371,     0,     0,   331,   332,    73,
    1115,     0,     0,     0,   104,   362,     0,     0,   212,     0,
      73,     0,   104,   104,   331,   332,   104,     0,    73,   104,
     104,   777,     0,     0,     0,   104,   104,     0,    73,     0,
     364,   104,   104,     0,     0,     0,   365,   366,   367,   368,
       0,   555,   329,   330,     0,   104,     0,     0,   104,     0,
     357,     0,   358,   359,   360,   361,   247,   104,   104,   248,
     249,    73,     0,     0,   369,   969,     0,   370,   362,     0,
      73,   560,   329,   330,     0,     0,    74,     0,     0,     0,
     104,   978,   363,   502,   121,     0,   121,   250,     0,   251,
       0,   104,   104,   364,   331,   332,    73,     0,     0,   365,
     366,   367,   368,   802,     0,   564,   329,   330,     0,     0,
       0,     0,   565,   329,   330,     0,     0,   756,   329,   330,
    -639,     0,     0,     0,   331,   332,     0,   369,     0,     0,
     370,   404,   405,   406,   407,   408,   409,   410,   411,   412,
     413,   414,   415,   371,     0,     0,   104,   416,   417,     0,
       0,   121,     0,     0,    74,     0,   104,   104,   331,   332,
     419,    74,    74,     0,   104,   331,   332,     0,     0,    74,
     331,   332,     0,     0,     0,   247,   104,     0,   248,   249,
     122,   420,     0,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,     0,   103,     0,   103,   128,   128,     0,
       0,  -277,     0,     0,     0,   232,   972,   357,   251,   358,
     359,   360,   361,     0,     0,    74,     0,     0,     0,    73,
      74,     0,     0,     0,     0,   362,     0,     0,     0,   104,
       0,   104,     0,   761,   104,   358,   359,   360,   361,     0,
      74,     0,  -299,     0,   103,  -299,  -299,     0,   318,     0,
     364,   762,   416,   417,     0,   121,   365,   366,   367,   368,
       0,    74,     0,     0,     0,   419,    74,   122,     0,    74,
       0,     0,  -299,  -299,   318,  -299,   364,   104,     0,     0,
       0,     0,   763,     0,   369,     0,   761,   370,   358,   359,
     360,   361,   425,   426,   427,   428,   429,   430,     0,     0,
     551,     0,     0,  1031,   762,   358,   359,   360,   361,    74,
      74,     0,   103,     0,   357,     0,   358,   359,   360,   361,
     104,   762,     0,     0,     0,     0,    74,     0,     0,   364,
       0,    73,   362,     0,     0,   977,     0,    74,   121,    73,
      73,     0,     0,     0,     0,    74,    73,     0,   887,     0,
       0,     0,    73,    73,     0,    74,     0,   364,    73,    73,
       0,     0,     0,   365,   366,   367,   368,     0,     0,     0,
     419,     0,    73,     0,     0,   100,     0,   100,   127,   127,
     127,     0,     0,     0,    73,    73,   231,     0,    74,     0,
       0,   369,   416,   417,   370,     0,     0,    74,   426,   427,
     428,   429,   430,   103,     0,   419,     0,    73,     0,     0,
       0,   122,     0,   122,     0,   416,   417,     0,    73,    73,
       0,     0,     0,    74,     0,   100,  -639,     0,   419,   317,
     423,   424,   425,   426,   427,   428,   429,   430,     0,  -639,
    -639,  -639,  -639,  -639,  -639,   121,  -639,     0,     0,     0,
     121,  -639,  -639,  -639,     0,   317,   426,   427,   428,   429,
     430,     0,     0,  -639,  -639,     0,  -639,  -639,  -639,  -639,
    -639,     0,   357,    73,   358,   359,   360,   361,   122,     0,
       0,   103,     0,    73,    73,     0,     0,   121,   103,   103,
     362,    73,     0,   100,     0,   761,   103,   358,   359,   360,
     361,     0,     0,    73,     0,   121,     0,   318,     0,     0,
       0,     0,     0,   762,     0,   364,  -639,     0,     0,     0,
       0,   365,   366,   367,   368,     0,     0,     0,     0,     0,
       0,  -639,     0,     0,     0,     0,     0,     0,   364,     0,
       0,  -639,   103,     0,  -639,  -639,    74,   103,     0,   369,
       0,     0,   370,     0,     0,     0,    73,     0,    73,     0,
       0,    73,     0,     0,  -639,  -639,     0,   103,     0,     0,
     276,  -639,  -639,  -639,  -639,     0,     0,     0,     0,     0,
       0,     0,   122,     0,   100,     0,     0,     0,   103,     0,
       0,     0,     0,   103,   318,     0,   623,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   121,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   623,   623,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    73,     0,     0,
       0,     0,     0,   103,     0,     0,     0,     0,    74,     0,
       0,     0,   100,     0,   103,   122,    74,    74,     0,   100,
     100,     0,   103,    74,     0,     0,     0,   100,     0,    74,
      74,     0,   103,     0,     0,    74,    74,     0,   317,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    74,
     404,   405,   406,   407,   408,   409,   410,     0,   412,   413,
       0,    74,    74,     0,     0,   103,   416,   417,     0,     0,
       0,     0,     0,   100,   103,     0,     0,     0,   100,   419,
       0,     0,     0,     0,    74,     0,     0,     0,   318,     0,
     318,     0,     0,     0,     0,    74,    74,     0,   100,     0,
     103,     0,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,     0,     0,     0,     0,     0,     0,     0,   100,
       0,     0,   122,     0,   100,   317,     0,   122,     0,     0,
       0,    83,     0,    83,     0,     0,     0,     0,     0,     0,
       0,     0,   228,     0,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,   318,     0,     0,     0,     0,
      74,    74,     0,     0,   122,     0,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      74,    83,   122,     0,   100,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   100,     0,     0,     0,     0,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   100,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   103,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,    74,     0,     0,    74,     0,
       0,     0,     0,     0,     0,     0,   100,     0,     0,    83,
       0,     0,     0,     0,     0,   100,     0,     0,     0,   128,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   317,
       0,   317,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   100,     0,     0,     0,     0,   122,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,   101,     0,   101,
       0,     0,     0,     0,     0,   103,   317,     0,     0,     0,
      83,     0,   318,   103,   623,     0,     0,     0,     0,     0,
     623,     0,     0,     0,     0,     0,   623,   623,     0,     0,
       0,     0,   103,   103,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   103,   101,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   103,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,   103,  -640,  -640,  -640,  -640,   408,   409,    83,     0,
    -640,  -640,   103,   103,     0,    83,    83,     0,   416,   417,
       0,     0,     0,    83,     0,     0,     0,     0,     0,     0,
     127,   419,     0,     0,     0,   101,     0,     0,     0,   128,
       0,     0,     0,     0,   128,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,     0,     0,     0,   623,     0,    83,
       0,     0,     0,     0,    83,     0,     0,   103,   103,     0,
       0,  1016,     0,     0,     0,   103,     0,     0,     0,     0,
       0,     0,     0,     0,    83,     0,     0,   103,     0,   128,
       0,     0,     0,     0,     0,     0,   100,     0,     0,     0,
       0,     0,     0,   317,   100,    83,     0,     0,     0,     0,
      83,     0,     0,   618,     0,     0,   101,     0,     0,     0,
       0,     0,     0,   100,   100,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   100,     0,     0,
     103,     0,   103,     0,     0,   103,     0,     0,     0,   100,
     100,     0,     0,   618,   618,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      83,     0,   100,     0,     0,     0,     0,     0,     0,     0,
       0,    83,     0,   100,   100,     0,     0,     0,     0,    83,
       0,     0,     0,   128,   101,     0,     0,     0,     0,    83,
       0,   101,   101,     0,     0,     0,     0,     0,     0,   101,
     127,     0,     0,     0,     0,   127,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   103,    83,     0,     0,     0,     0,     0,     0,     0,
       0,    83,     0,     0,     0,     0,     0,     0,   100,   100,
       0,     0,  1015,   802,     0,   101,   100,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,    83,   100,     0,
     127,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     101,   404,   405,   406,   407,   408,   409,   410,   411,   412,
     413,   414,   415,     0,     0,     0,     0,   416,   417,     0,
       0,   101,     0,     0,     0,     0,   101,     0,     0,   101,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   100,     0,   100,     0,     0,   100,     0,     0,     0,
       0,   420,     0,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,     0,     0,     0,     0,     0,     0,   101,
     101,  -612,  -612,  -612,  -612,  -612,  -612,  -612,  -612,  -612,
       0,     0,     0,     0,     0,     0,   101,  -612,     0,  -612,
    -612,  -612,  -612,     0,   127,     0,     0,   101,  -612,  -612,
    -612,  -612,  -612,  -612,  -612,   101,     0,  -612,     0,     0,
      83,     0,     0,     0,     0,   101,  -612,  -612,  -612,  -612,
    -612,  -612,  -612,  -612,  -612,     0,  -612,  -612,  -612,     0,
       0,  -612,   100,     0,  -612,  -612,     0,  -612,  -612,  -612,
       0,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,  -612,  -612,     0,     0,     0,   101,     0,  -612,
     839,     0,  -612,  -612,     0,  -612,  -612,     0,  -612,     0,
    -612,  -612,  -612,     0,  -612,  -612,  -612,     0,  -612,  -612,
    -612,     0,  -612,   101,     0,     0,     0,     0,     0,  -107,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -612,  -612,  -612,     0,     0,     0,     0,     0,
       0,     0,  -612,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
      83,   618,     0,     0,     0,     0,     0,   618,     0,     0,
       0,     0,     0,   618,   618,     0,     0,     0,     0,    83,
      83,     0,     0,     0,   217,   217,     0,     0,     0,     0,
     217,   267,   267,    83,     0,   267,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,    83,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   289,   291,   292,   293,     0,     0,    83,   267,
     309,     0,     0,     0,     0,     0,   101,     0,     0,    83,
      83,   346,   347,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   651,   652,     0,     0,   653,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,   217,   618,     0,     0,     0,     0,     0,
       0,   188,   189,   190,    83,    83,     0,     0,  1013,     0,
       0,     0,    83,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    83,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,     0,     0,
       0,     0,     0,     0,   203,   276,     0,     0,   101,     0,
       0,     0,     0,     0,     0,     0,   101,   101,     0,     0,
       0,     0,     0,   101,     0,     0,     0,     0,     0,   101,
     101,     0,     0,     0,     0,   101,   101,    83,     0,    83,
       0,     0,    83,     0,     0,     0,     0,     0,     0,   101,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   217,
     217,   101,   101,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   404,   405,   406,
     407,   408,   409,     0,   101,   412,   413,     0,     0,     0,
       0,     0,     0,   416,   417,   101,   101,     0,   498,   499,
     500,   346,     0,     0,     0,     0,   419,     0,     0,     0,
       0,     0,   267,     0,     0,   267,     0,     0,     0,     0,
     217,   217,     0,     0,     0,     0,     0,     0,    83,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     101,   101,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,   217,   217,   217,   217,
       0,   217,   217,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   586,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   596,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   101,   608,   101,     0,     0,   101,   619,
     624,   625,   626,   627,   628,   629,   630,   631,   632,   633,
     634,   635,   636,   637,     0,   639,   640,   641,   642,   643,
     644,   645,   646,   647,   648,   649,   650,     0,     0,   267,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   672,
     672,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   267,     0,     0,   217,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   672,
       0,   267,     0,   672,   672,     0,     0,     0,     0,     0,
     267,     0,     0,     0,   101,     0,     0,     0,     0,   718,
       0,     0,   721,   722,     0,     0,  -293,   723,     0,     0,
     726,     0,   729,     0,   309,   293,     0,     0,     0,  -293,
    -293,  -293,  -293,  -293,  -293,     0,  -293,     0,     0,     0,
       0,  -293,   672,  -293,  -293,  -293,     0,     0,     0,     0,
       0,     0,   726,  -293,  -293,   309,  -293,  -293,  -293,  -293,
    -293,     0,     0,     0,     0,   267,     0,     0,     0,     0,
       0,     0,     0,  -293,     0,     0,     0,     0,     0,     0,
       0,     0,   759,   760,     0,     0,     0,     0,  -293,  -293,
    -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,
     770,     0,     0,     0,  -293,  -293,  -293,     0,     0,  -293,
       0,     0,     0,     0,     0,  -293,     0,  -293,     0,   788,
       0,  -293,   795,     0,     0,     0,     0,     0,     0,  -293,
       0,  -293,     0,     0,  -293,  -293,     0,     0,  -293,  -293,
    -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,
       0,     0,     0,     0,     0,  -293,  -293,  -293,  -293,     0,
       0,  -293,  -293,  -293,  -293,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,     0,     0,     0,    14,     0,   108,   109,
      17,    18,     0,     0,     0,   217,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,   829,     0,     0,
     770,   788,     0,     0,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,   217,
      42,     0,     0,    43,    44,     0,   116,     0,     0,     0,
       0,   869,     0,     0,     0,     0,     0,     0,     0,     0,
     726,   309,     0,     0,   889,     0,     0,     0,   313,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,   404,   405,   406,   407,   408,   409,   410,   411,
     412,   413,   414,   415,     0,     0,     0,     0,   416,   417,
       0,   120,     0,     0,     0,     0,     0,     0,     0,   314,
       0,   419,   918,     0,     0,     0,     0,   672,   921,     0,
     267,     0,     0,   672,   672,     0,     0,     0,   726,   672,
     672,     0,   420,     0,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,     0,     0,     0,     0,     0,     0,
       0,     0,   217,     0,     0,   672,   672,     0,   672,   672,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,     0,     0,   967,     0,   416,   417,   293,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   419,     0,
       0,     0,     0,     0,   979,   980,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   985,   986,   420,
       0,   421,   422,   423,   424,   425,   426,   427,   428,   429,
     430,     0,     0,  1002,     0,     0,     0,     0,     0,  -277,
       0,     0,     0,  1004,  1005,     0,     0,     0,  -639,     3,
     672,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,    14,     0,    15,
      16,    17,    18,   672,     0,     0,     0,     0,    19,    20,
      21,    22,    23,    24,    25,     0,   309,    26,     0,     0,
       0,     0,     0,    27,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,  -639,     0,     0,  -639,  -639,     0,     0,
       0,     0,  -415,     0,     0,     0,     0,     0,     0,     0,
       0,   267,    64,    65,    66,  -415,  -415,  -415,  -415,  -415,
    -415,     0,  -415,     0,  -639,     0,  -639,  -415,  -415,  -415,
    -415,     0,     0,     0,     0,     0,     0,     0,     0,  -415,
    -415,     0,  -415,  -415,  -415,  -415,  -415,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -415,  -415,  -415,  -415,  -415,  -415,
    -415,  -415,  -415,  -415,  -415,  -415,     0,     0,     0,     0,
    -415,  -415,  -415,     0,     0,  -415,     0,     0,     0,     0,
       0,  -415,     0,  -415,     0,     0,     0,  -415,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -415,     0,     0,
    -415,  -415,     0,     0,  -415,     0,  -415,  -415,  -415,  -415,
    -415,  -415,  -415,  -415,  -415,  -415,     0,     0,  -500,     0,
    -415,  -415,  -415,  -415,  -415,     0,   276,  -415,  -415,  -415,
    -415,  -500,  -500,  -500,  -500,  -500,  -500,     0,  -500,     0,
       0,     0,     0,  -500,     0,  -500,  -500,     0,     0,     0,
       0,     0,     0,     0,     0,  -500,  -500,     0,  -500,  -500,
    -500,  -500,  -500,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   496,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -500,  -500,  -500,  -500,  -500,  -500,  -500,  -500,  -500,  -500,
    -500,  -500,     0,     0,     0,     0,  -500,  -500,  -500,     0,
    -500,  -500,     0,     0,     0,     0,     0,  -500,     0,  -500,
       0,     0,     0,  -500,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -500,     0,     0,  -500,  -500,     0,  -500,
    -500,     0,  -500,  -500,  -500,  -500,  -500,  -500,  -500,  -500,
    -500,  -500,     0,     0,  -639,     0,     0,  -500,  -500,  -500,
    -500,     0,     0,  -500,  -500,  -500,  -500,  -639,  -639,  -639,
    -639,  -639,  -639,     0,  -639,     0,     0,     0,     0,  -639,
    -639,  -639,  -639,     0,     0,     0,     0,     0,     0,     0,
       0,  -639,  -639,     0,  -639,  -639,  -639,  -639,  -639,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -639,  -639,  -639,  -639,
    -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,     0,     0,
       0,     0,  -639,  -639,  -639,     0,     0,  -639,     0,     0,
       0,     0,     0,  -639,     0,  -639,     0,     0,     0,  -639,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -639,
       0,     0,  -639,  -639,     0,     0,  -639,     0,  -639,  -639,
    -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,     0,     0,
    -639,     0,  -639,  -639,  -639,  -639,  -639,     0,   276,  -639,
    -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,     0,
    -639,     0,     0,     0,     0,  -639,     0,  -639,  -639,     0,
       0,     0,     0,     0,     0,     0,     0,  -639,  -639,     0,
    -639,  -639,  -639,  -639,  -639,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -639,  -639,  -639,  -639,  -639,  -639,  -639,  -639,
    -639,  -639,  -639,  -639,     0,     0,     0,     0,  -639,  -639,
    -639,     0,     0,  -639,     0,     0,     0,     0,     0,  -639,
       0,  -639,     0,     0,     0,  -639,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -639,     0,     0,  -639,  -639,
       0,     0,  -639,     0,  -639,  -639,  -639,  -639,  -639,  -639,
    -639,  -639,  -639,  -639,     0,     0,  -616,     0,     0,  -639,
    -639,  -639,  -639,     0,   276,  -639,  -639,  -639,  -639,  -616,
    -616,  -616,     0,  -616,  -616,     0,  -616,     0,     0,     0,
       0,  -616,  -616,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -616,  -616,     0,  -616,  -616,  -616,  -616,
    -616,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -616,  -616,
    -616,  -616,  -616,  -616,  -616,  -616,  -616,  -616,  -616,  -616,
       0,     0,     0,     0,  -616,  -616,  -616,     0,   807,  -616,
       0,     0,     0,     0,     0,     0,     0,  -616,     0,     0,
       0,  -616,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -616,     0,     0,  -616,  -616,     0,  -107,  -616,     0,
    -616,  -616,  -616,  -616,  -616,  -616,  -616,  -616,  -616,  -616,
       0,     0,  -616,     0,  -616,  -616,  -616,     0,   -99,     0,
       0,  -616,  -616,  -616,  -616,  -616,  -616,  -616,     0,  -616,
    -616,     0,  -616,     0,     0,     0,     0,  -616,  -616,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -616,
    -616,     0,  -616,  -616,  -616,  -616,  -616,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -616,  -616,  -616,  -616,  -616,  -616,
    -616,  -616,  -616,  -616,  -616,  -616,     0,     0,     0,     0,
    -616,  -616,  -616,     0,   807,  -616,     0,     0,     0,     0,
       0,     0,     0,  -616,     0,     0,     0,  -616,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -616,     0,     0,
    -616,  -616,     0,  -107,  -616,     0,  -616,  -616,  -616,  -616,
    -616,  -616,  -616,  -616,  -616,  -616,     0,     0,  -302,     0,
    -616,  -616,  -616,     0,  -616,     0,     0,  -616,  -616,  -616,
    -616,  -302,  -302,  -302,     0,  -302,  -302,     0,  -302,     0,
       0,     0,     0,  -302,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -302,  -302,     0,  -302,  -302,
    -302,  -302,  -302,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,     0,     0,     0,     0,  -302,  -302,  -302,     0,
     808,  -302,     0,     0,     0,     0,     0,     0,     0,  -302,
       0,     0,     0,  -302,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -302,     0,     0,  -302,  -302,     0,  -109,
    -302,     0,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,     0,     0,  -302,     0,     0,  -302,  -302,     0,
    -101,     0,     0,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
       0,  -302,  -302,     0,  -302,     0,     0,     0,     0,  -302,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -302,  -302,     0,  -302,  -302,  -302,  -302,  -302,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,     0,     0,
       0,     0,  -302,  -302,  -302,     0,   808,  -302,     0,     0,
       0,     0,     0,     0,     0,  -302,     0,     0,     0,  -302,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -302,
       0,     0,  -302,  -302,     0,  -109,  -302,     0,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,     0,     0,
       0,     0,     0,  -302,  -302,     0,  -302,     0,     0,  -302,
    -302,  -302,  -302,   295,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,  -639,  -639,  -639,     0,     0,
    -639,    14,     0,    15,    16,    17,    18,     0,     0,     0,
       0,     0,    19,    20,    21,    22,    23,    24,    25,     0,
       0,    26,     0,     0,     0,     0,     0,    27,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,  -639,     0,     0,
    -639,  -639,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    64,    65,    66,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -639,   295,
    -639,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,  -639,     0,  -639,  -639,    14,     0,    15,
      16,    17,    18,     0,     0,     0,     0,     0,    19,    20,
      21,    22,    23,    24,    25,     0,     0,    26,     0,     0,
       0,     0,     0,    27,     0,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,  -639,     0,     0,  -639,  -639,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,    65,    66,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -639,   295,  -639,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,  -639,
       0,     0,  -639,    14,  -639,    15,    16,    17,    18,     0,
       0,     0,     0,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,    27,
       0,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,  -639,
       0,     0,  -639,  -639,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -639,   295,  -639,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,  -639,     0,     0,  -639,    14,
       0,    15,    16,    17,    18,  -639,     0,     0,     0,     0,
      19,    20,    21,    22,    23,    24,    25,     0,     0,    26,
       0,     0,     0,     0,     0,    27,     0,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,  -639,     0,     0,  -639,  -639,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,    65,    66,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -639,   295,  -639,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,  -639,     0,     0,  -639,    14,     0,    15,    16,    17,
      18,     0,     0,     0,     0,     0,    19,    20,    21,    22,
      23,    24,    25,     0,     0,    26,     0,     0,     0,     0,
       0,    27,     0,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,    50,     0,     0,
      51,    52,     0,    53,    54,     0,    55,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,  -639,     0,     0,  -639,  -639,     3,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
      64,    65,    66,     0,    14,     0,    15,    16,    17,    18,
       0,     0,  -639,     0,  -639,    19,    20,    21,    22,    23,
      24,    25,     0,     0,    26,     0,     0,     0,     0,     0,
      27,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
    -639,     0,     0,  -639,  -639,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
      65,    66,     0,     0,  -639,     0,     0,     0,     0,     0,
       0,  -639,   295,  -639,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,  -639,  -639,     0,     0,     0,
      14,     0,    15,    16,    17,    18,     0,     0,     0,     0,
       0,    19,    20,    21,    22,    23,    24,    25,     0,     0,
      26,     0,     0,     0,     0,     0,    27,     0,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,  -639,     0,     0,  -639,
    -639,   295,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,    64,    65,    66,     0,    14,
       0,    15,    16,    17,    18,     0,     0,  -639,     0,  -639,
      19,    20,    21,    22,    23,    24,    25,     0,     0,    26,
       0,     0,     0,     0,     0,    27,     0,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,    50,     0,     0,   296,    52,     0,    53,    54,     0,
      55,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,  -639,     0,     0,  -639,  -639,
     295,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,    64,    65,    66,     0,    14,     0,
      15,    16,    17,    18,     0,  -639,  -639,     0,  -639,    19,
      20,    21,    22,    23,    24,    25,     0,     0,    26,     0,
       0,     0,     0,     0,    27,     0,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
      50,     0,     0,    51,    52,     0,    53,    54,     0,    55,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,  -639,     0,     0,  -639,  -639,   295,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,    64,    65,    66,     0,    14,     0,    15,
      16,    17,    18,     0,  -639,  -639,     0,  -639,    19,    20,
      21,    22,    23,    24,    25,     0,     0,    26,     0,     0,
       0,     0,     0,    27,     0,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,  -639,     0,     0,  -639,  -639,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    64,    65,    66,     0,     0,  -639,     0,     0,
       0,     0,     0,     0,  -639,   295,  -639,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,  -639,
       0,     0,     0,    14,     0,    15,    16,    17,    18,     0,
       0,     0,     0,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,    27,
       0,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,  -639,
       0,     0,  -639,  -639,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,    64,    65,
      66,     0,    14,     0,    15,    16,    17,    18,     0,     0,
    -639,     0,  -639,    19,    20,    21,    22,    23,    24,    25,
       0,     0,    26,     0,     0,     0,     0,     0,    27,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,    45,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,    49,     0,
       0,     0,     0,     0,    50,     0,     0,    51,    52,     0,
      53,    54,     0,    55,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,   247,     0,
       0,   248,   249,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,    64,    65,    66,
       0,    14,     0,    15,    16,    17,    18,     0,     0,   250,
       0,   251,    19,    20,    21,    22,    23,    24,    25,     0,
       0,    26,     0,     0,     0,     0,     0,    27,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,   247,     0,     0,
     248,   249,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,    64,    65,    66,     0,
      14,     0,    15,    16,    17,    18,     0,     0,   250,     0,
     251,    19,    20,    21,    22,    23,    24,    25,     0,     0,
      26,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    52,     0,    53,    54,
       0,     0,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,   247,     0,     0,   248,
     249,     0,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,    64,    65,    66,     0,    14,
       0,   108,   109,    17,    18,     0,     0,   250,     0,   251,
     110,   111,   112,    22,    23,    24,    25,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    52,     0,    53,    54,     0,
       0,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,   247,     0,     0,   248,   249,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,    64,   265,    66,     0,    14,     0,
      15,    16,    17,    18,     0,     0,   250,     0,   251,    19,
      20,    21,    22,    23,    24,    25,     0,     0,    26,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,   247,     0,     0,   248,   249,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    64,    65,    66,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   251,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,   149,   150,   151,   152,
     153,   154,     0,     0,     0,   155,   156,   157,   158,   159,
     160,   161,   162,   163,   164,     0,     0,     0,     0,     0,
     165,   166,   167,   168,   169,   170,   171,   172,    35,    36,
     173,    38,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   116,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,     0,     0,     0,     0,     0,     0,   203,   204,
    -609,  -609,  -609,  -609,  -609,  -609,  -609,  -609,  -609,     0,
       0,     0,     0,     0,     0,     0,  -609,     0,  -609,  -609,
    -609,  -609,     0,  -609,     0,     0,     0,  -609,  -609,  -609,
    -609,  -609,  -609,  -609,     0,     0,  -609,     0,     0,     0,
       0,     0,     0,     0,     0,  -609,  -609,  -609,  -609,  -609,
    -609,  -609,  -609,  -609,     0,  -609,  -609,  -609,     0,     0,
    -609,     0,     0,  -609,  -609,     0,  -609,  -609,  -609,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -609,  -609,     0,     0,     0,     0,     0,  -609,     0,
       0,  -609,  -609,     0,  -609,  -609,     0,  -609,     0,  -609,
    -609,  -609,     0,  -609,  -609,  -609,     0,  -609,  -609,  -609,
       0,  -609,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -609,  -609,  -609,     0,  -609,     0,     0,     0,     0,
       0,  -609,  -610,  -610,  -610,  -610,  -610,  -610,  -610,  -610,
    -610,     0,     0,     0,     0,     0,     0,     0,  -610,     0,
    -610,  -610,  -610,  -610,     0,  -610,     0,     0,     0,  -610,
    -610,  -610,  -610,  -610,  -610,  -610,     0,     0,  -610,     0,
       0,     0,     0,     0,     0,     0,     0,  -610,  -610,  -610,
    -610,  -610,  -610,  -610,  -610,  -610,     0,  -610,  -610,  -610,
       0,     0,  -610,     0,     0,  -610,  -610,     0,  -610,  -610,
    -610,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -610,  -610,     0,     0,     0,     0,     0,
    -610,     0,     0,  -610,  -610,     0,  -610,  -610,     0,  -610,
       0,  -610,  -610,  -610,     0,  -610,  -610,  -610,     0,  -610,
    -610,  -610,     0,  -610,     0,     0,     0,     0,     0,     0,
    -613,  -613,  -613,  -613,  -613,  -613,  -613,  -613,  -613,     0,
       0,     0,     0,  -610,  -610,  -610,  -613,  -610,  -613,  -613,
    -613,  -613,     0,  -610,     0,     0,     0,  -613,  -613,  -613,
    -613,  -613,  -613,  -613,     0,     0,  -613,     0,     0,     0,
       0,     0,     0,     0,     0,  -613,  -613,  -613,  -613,  -613,
    -613,  -613,  -613,  -613,     0,  -613,  -613,  -613,     0,     0,
    -613,     0,     0,  -613,  -613,     0,  -613,  -613,  -613,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -613,  -613,     0,     0,     0,     0,     0,  -613,   840,
       0,  -613,  -613,     0,  -613,  -613,     0,  -613,     0,  -613,
    -613,  -613,     0,  -613,  -613,  -613,     0,  -613,  -613,  -613,
       0,  -613,     0,     0,     0,     0,     0,     0,  -109,  -614,
    -614,  -614,  -614,  -614,  -614,  -614,  -614,  -614,     0,     0,
       0,  -613,  -613,  -613,     0,  -614,     0,  -614,  -614,  -614,
    -614,  -613,     0,     0,     0,     0,  -614,  -614,  -614,  -614,
    -614,  -614,  -614,     0,     0,  -614,     0,     0,     0,     0,
       0,     0,     0,     0,  -614,  -614,  -614,  -614,  -614,  -614,
    -614,  -614,  -614,     0,  -614,  -614,  -614,     0,     0,  -614,
       0,     0,  -614,  -614,     0,  -614,  -614,  -614,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -614,  -614,     0,     0,     0,     0,     0,  -614,     0,     0,
    -614,  -614,     0,  -614,  -614,     0,  -614,     0,  -614,  -614,
    -614,     0,  -614,  -614,  -614,     0,  -614,  -614,  -614,     0,
    -614,     0,     0,     0,     0,     0,     0,  -615,  -615,  -615,
    -615,  -615,  -615,  -615,  -615,  -615,     0,     0,     0,     0,
    -614,  -614,  -614,  -615,     0,  -615,  -615,  -615,  -615,     0,
    -614,     0,     0,     0,  -615,  -615,  -615,  -615,  -615,  -615,
    -615,     0,     0,  -615,     0,     0,     0,     0,     0,     0,
       0,     0,  -615,  -615,  -615,  -615,  -615,  -615,  -615,  -615,
    -615,     0,  -615,  -615,  -615,     0,     0,  -615,     0,     0,
    -615,  -615,     0,  -615,  -615,  -615,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -615,  -615,
       0,     0,     0,     0,     0,  -615,     0,     0,  -615,  -615,
       0,  -615,  -615,     0,  -615,     0,  -615,  -615,  -615,     0,
    -615,  -615,  -615,     0,  -615,  -615,  -615,     0,  -615,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -615,  -615,
    -615,     0,     0,     0,     0,     0,     0,     0,  -615,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,     0,     0,     0,   155,   156,   157,   233,
     234,   235,   236,   162,   163,   164,     0,     0,     0,     0,
       0,   165,   166,   167,   237,   238,   239,   240,   172,   320,
     321,   241,   322,     0,     0,     0,     0,     0,     0,   323,
       0,     0,     0,     0,     0,   324,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,   325,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,     0,     0,     0,     0,     0,     0,   203,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,     0,     0,     0,   155,   156,   157,
     233,   234,   235,   236,   162,   163,   164,     0,     0,     0,
       0,     0,   165,   166,   167,   237,   238,   239,   240,   172,
     320,   321,   241,   322,     0,     0,     0,     0,     0,     0,
     323,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,   487,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,     0,     0,     0,     0,     0,     0,
     203,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,   153,   154,     0,     0,     0,   155,   156,
     157,   233,   234,   235,   236,   162,   163,   164,     0,     0,
       0,     0,     0,   165,   166,   167,   237,   238,   239,   240,
     172,     0,     0,   241,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,   242,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,     0,     0,     0,     0,     0,
       0,   203,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,     0,     0,     0,   155,
     156,   157,   233,   234,   235,   236,   162,   163,   164,     0,
       0,     0,     0,     0,   165,   166,   167,   237,   238,   239,
     240,   172,     0,     0,   241,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,     0,     0,     0,     0,
       0,     0,   203,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,     0,     0,     0,     0,    14,
       0,   108,   109,    17,    18,     0,     0,     0,     0,     0,
     110,   111,   112,    22,    23,    24,    25,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,   116,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   313,     0,     0,   119,    52,     0,    53,    54,     0,
       0,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,     0,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,    14,   120,    15,    16,    17,    18,     0,
       0,     0,   612,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,    27,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,     0,
       0,     0,     0,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,    64,    65,
      66,    14,     0,    15,    16,    17,    18,     0,     0,     0,
       0,     0,    19,    20,    21,    22,    23,    24,    25,     0,
       0,    26,     0,     0,     0,     0,     0,    27,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,     0,     0,     0,
       0,     0,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,     0,    64,    65,    66,    14,
       0,    15,    16,    17,    18,     0,     0,     0,     0,     0,
      19,    20,    21,    22,    23,    24,    25,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,   260,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,   261,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    52,     0,    53,    54,     0,
     262,     0,   263,   264,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,     0,     0,     0,     0,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,     0,    64,   265,    66,    14,     0,    15,
      16,    17,    18,     0,     0,     0,     0,     0,    19,    20,
      21,    22,    23,    24,    25,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,   260,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,   261,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,   509,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    52,     0,    53,    54,     0,   262,     0,
     263,   264,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,     0,     0,     0,     0,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,     0,     0,
       0,     0,    64,   265,    66,    14,     0,   108,   109,    17,
      18,     0,     0,     0,     0,     0,   110,   111,   112,    22,
      23,    24,    25,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,   260,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,   261,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    52,     0,    53,    54,     0,   725,     0,   263,   264,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,     0,     0,     0,     0,     0,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,     0,     0,     0,     0,
      64,   265,    66,    14,     0,   108,   109,    17,    18,     0,
       0,     0,     0,     0,   110,   111,   112,    22,    23,    24,
      25,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,    32,   260,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,   261,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,   871,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    52,
       0,    53,    54,     0,   725,     0,   263,   264,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,     0,
       0,     0,     0,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,     0,     0,     0,     0,    64,   265,
      66,    14,     0,   108,   109,    17,    18,     0,     0,     0,
       0,     0,   110,   111,   112,    22,    23,    24,    25,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,    32,   260,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,   261,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    52,     0,    53,
      54,     0,   262,     0,   263,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,     0,     0,     0,
       0,     0,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,     0,    64,   265,    66,    14,
       0,   108,   109,    17,    18,     0,     0,     0,     0,     0,
     110,   111,   112,    22,    23,    24,    25,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,   260,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,   261,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    52,     0,    53,    54,     0,
       0,     0,   263,   264,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,     0,     0,     0,     0,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,     0,    64,   265,    66,    14,     0,   108,
     109,    17,    18,     0,     0,     0,     0,     0,   110,   111,
     112,    22,    23,    24,    25,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,   260,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,   261,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    52,     0,    53,    54,     0,   725,     0,
     263,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,     0,     0,     0,     0,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,     0,     0,
       0,     0,    64,   265,    66,    14,     0,   108,   109,    17,
      18,     0,     0,     0,     0,     0,   110,   111,   112,    22,
      23,    24,    25,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,   260,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,   261,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    52,     0,    53,    54,     0,     0,     0,   263,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,     0,     0,     0,     0,     0,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,     0,     0,     0,     0,
      64,   265,    66,    14,     0,    15,    16,    17,    18,     0,
       0,     0,     0,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    52,
       0,    53,    54,     0,   606,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,     0,
       0,     0,     0,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,     0,     0,     0,     0,    64,   265,
      66,    14,     0,   108,   109,    17,    18,     0,     0,     0,
       0,     0,   110,   111,   112,    22,    23,    24,    25,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    52,     0,    53,
      54,     0,   262,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,     0,     0,     0,
       0,     0,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,     0,    64,   265,    66,    14,
       0,   108,   109,    17,    18,     0,     0,     0,     0,     0,
     110,   111,   112,    22,    23,    24,    25,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    52,     0,    53,    54,     0,
     606,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,     0,     0,     0,     0,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,     0,    64,   265,    66,    14,     0,   108,
     109,    17,    18,     0,     0,     0,     0,     0,   110,   111,
     112,    22,    23,    24,    25,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    52,     0,    53,    54,     0,   917,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,     0,     0,     0,     0,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,     0,     0,
       0,     0,    64,   265,    66,    14,     0,   108,   109,    17,
      18,     0,     0,     0,     0,     0,   110,   111,   112,    22,
      23,    24,    25,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    52,     0,    53,    54,     0,   725,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,     0,     0,     0,     0,     0,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,     0,     0,     0,     0,
      64,   265,    66,    14,     0,    15,    16,    17,    18,     0,
       0,     0,     0,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    52,
       0,    53,    54,     0,     0,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,     0,
       0,     0,     0,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,     0,     0,     0,     0,    64,    65,
      66,    14,     0,   108,   109,    17,    18,     0,     0,     0,
       0,     0,   110,   111,   112,    22,    23,    24,    25,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    52,     0,    53,
      54,     0,     0,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,     0,     0,     0,
       0,     0,     0,     4,     5,     6,     7,     8,     9,    10,
      11,    12,     0,     0,     0,     0,    64,   265,    66,    14,
       0,    15,    16,    17,    18,     0,     0,     0,     0,     0,
      19,    20,    21,    22,    23,    24,    25,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    52,     0,    53,    54,     0,
       0,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,     0,     0,     0,     0,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,     0,    64,   265,    66,    14,     0,   108,
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
     110,   111,   112,    22,    23,    24,    25,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    30,    31,
      32,   114,    34,    35,    36,   115,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,   116,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   118,     0,     0,   119,    52,     0,    53,    54,     0,
       0,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,     0,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,     0,     0,     0,     0,
       0,     0,     0,    14,   120,   108,   109,    17,    18,     0,
       0,     0,     0,     0,   110,   111,   112,    22,    23,    24,
      25,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   313,     0,     0,   400,    52,
       0,    53,    54,     0,     0,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
       0,     0,     0,     0,     0,     0,     0,    14,   120,   108,
     109,    17,    18,     0,     0,     0,     0,     0,   110,   111,
     112,    22,   851,   852,   853,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    30,    31,    32,   854,
      34,    35,    36,   855,    38,     0,    39,    40,    41,     0,
       0,   856,     0,     0,    43,    44,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   857,
       0,     0,   119,    52,     0,    53,    54,     0,     0,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,     0,     0,     4,     5,     6,     7,     8,
       9,    10,    11,    12,     0,     0,     0,     0,     0,     0,
       0,    14,   120,   108,   109,    17,    18,     0,     0,     0,
       0,     0,   110,   111,   112,    22,    23,    24,    25,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,   116,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   989,     0,     0,   119,    52,     0,    53,
      54,     0,     0,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,     0,     0,
       0,     0,     0,     0,     0,    14,   120,   108,   109,    17,
      18,     0,     0,     0,     0,     0,   110,   111,   112,    22,
      23,    24,    25,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,   225,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1012,     0,     0,
     119,    52,     0,    53,    54,     0,     0,   660,   661,     0,
      56,   662,    57,    58,    59,     0,    60,    61,    62,     0,
      63,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
     120,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   681,   652,     0,     0,   682,     0,   203,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   666,
     661,     0,     0,   667,     0,   203,   276,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   698,   652,     0,     0,   699,
       0,   203,   276,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,   701,   661,     0,     0,   702,     0,   203,   276,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   708,   652,     0,
       0,   709,     0,   203,   276,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   711,   661,     0,     0,   712,     0,   203,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   748,
     652,     0,     0,   749,     0,   203,   276,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   751,   661,     0,     0,   752,
       0,   203,   276,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,   922,   652,     0,     0,   923,     0,   203,   276,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   925,   661,     0,
       0,   926,     0,   203,   276,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,  1023,   652,     0,     0,  1024,     0,   203,
     276,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,  1023,
     652,     0,     0,  1079,     0,   203,   276,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,  1090,   652,     0,     0,  1091,
       0,   203,   276,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,  1093,   661,     0,     0,  1094,     0,   203,   276,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   666,   661,     0,
       0,   667,     0,   203,   276,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   902,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,     0,     0,     0,     0,     0,     0,   203,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,   415,     0,     0,     0,     0,   416,   417,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     420,     0,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,   404,   405,   406,   407,   408,   409,   410,   411,
     412,   413,   414,   415,     0,     0,     0,     0,   416,   417,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   420,     0,   421,   422,   423,   424,   425,   426,
     427,   428,   429,   430,     0,     0,     0,     0,     0,     0,
       0,     0,  -279,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,   415,     0,     0,     0,     0,   416,
     417,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
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
       0,   416,   417,     0,     0,     0,   418,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,     0,   421,   422,   423,
     424,   425,   426,   427,   428,   429,   430,   404,   405,   406,
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
     413,  -640,  -640,     0,     0,     0,     0,   416,   417,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430
};

static const yytype_int16 yycheck[] =
{
       1,    26,   222,    88,   285,   481,    86,    87,    13,    27,
       1,     9,     3,     4,     5,    81,    14,     8,     9,    20,
      68,    12,    27,    14,    15,    16,   272,   490,    19,    55,
       4,     5,    14,   594,   433,    65,   597,     6,    12,   314,
      21,   716,    51,     3,   597,   716,   118,   508,   791,     1,
     547,     3,    53,    54,    15,    16,   332,   307,    19,   458,
      51,   311,     6,   431,    55,   379,   594,   435,   692,    13,
     438,    26,    73,    74,    65,   319,   475,    74,   539,   703,
      25,    55,   374,    27,    26,   484,    68,   763,    57,    74,
      81,   459,   403,    15,    16,    20,    21,    19,   504,   296,
     972,    29,   508,   395,   669,   670,   474,    81,   476,   997,
     111,   144,    16,    57,    25,   612,    57,   485,    15,    16,
      15,    16,    19,   104,    19,   122,   117,   371,   119,    15,
      16,    53,    54,    19,    90,   103,    25,    55,   218,   594,
      73,    74,    59,    60,    61,    62,     0,    18,    90,   229,
     549,    25,    90,    79,    25,   121,    53,   525,    53,    54,
     128,    25,    25,   105,   333,    26,    90,   336,   460,   338,
      51,   340,    25,   342,    55,    25,   142,    90,   121,   104,
     121,   105,   550,   138,    90,   113,   111,   112,   121,   122,
      90,   147,    90,  1081,   755,    90,   138,   142,   140,   144,
     126,  1073,   144,   400,   129,   147,    93,   768,   138,   147,
     290,   115,   213,   214,   118,   119,   270,   214,   272,   210,
      26,    28,    57,   147,   403,   223,   224,   755,   213,   214,
     791,   142,   223,   224,   147,   142,   316,   443,   444,   126,
     768,   147,   146,    18,   148,    20,    90,   147,   314,   147,
     298,   994,   306,   142,   997,   144,   821,    92,   306,   307,
      90,   105,   121,   311,   443,   444,    92,   513,   142,   270,
     252,   272,   547,   242,   735,   276,    90,   138,   142,   142,
     213,   214,   558,   144,    90,   276,   121,   296,   279,   142,
      90,   966,   142,   284,   285,   966,   140,   288,   242,   105,
     755,   977,   799,   147,   295,   296,    20,   551,   810,   125,
      90,    25,   303,   768,   816,   276,   298,   147,   279,    16,
      90,    92,    60,   314,   144,    63,    55,    92,   734,   735,
      51,   892,   138,   147,   140,   105,   121,   612,  1081,   892,
     314,   147,    92,   295,   968,   665,    55,   147,   668,    25,
     121,   303,   350,   351,   352,   353,   121,   279,   349,   350,
     351,   352,   353,   354,   355,   356,   686,   147,   398,   107,
     140,   142,   848,   403,   144,   401,   377,   147,   379,   276,
     691,   276,   279,   374,   279,   457,   319,   556,   680,   349,
      92,   400,    20,   279,    92,  1070,   215,   349,   119,  1070,
      92,  1025,   354,   222,   395,   804,   812,   398,   325,   400,
     401,   142,   403,   443,   444,   142,    27,    92,   115,   121,
      57,   118,   119,   121,    58,    59,   138,   401,   101,   121,
     145,   886,   433,   994,   121,   803,   997,   805,   371,    90,
     259,   896,   433,   724,   142,   142,   121,    37,    38,   146,
     142,   148,   443,   444,   105,   101,   141,   458,   512,   513,
     139,   775,    55,    92,   142,   466,   101,   458,   101,   460,
     461,   121,   433,    57,   475,   142,   388,   502,   390,   545,
     471,   547,   728,   484,   475,   131,   132,   133,   479,   140,
    1114,   435,   121,   484,   438,   745,   147,   458,   489,   719,
     792,   398,    90,   521,   115,   968,   403,   118,   119,   970,
     435,   512,   513,   142,   475,   459,   521,   105,    92,    26,
    1081,   522,   121,   484,   799,   806,    51,   819,    90,   511,
     349,   522,   476,   142,   459,   146,    92,   148,   433,   994,
     531,   485,   622,   105,   363,    92,   612,   121,   549,    58,
      59,   476,   140,    90,   545,   609,   547,   142,   549,   147,
     485,    92,   381,   458,   970,   121,   557,   288,   105,   142,
     522,   545,    51,   547,   121,   296,   142,   521,   140,   531,
     475,   525,   504,    90,    51,   147,   142,   879,   549,   484,
     121,    17,    18,   584,   908,   909,   493,   878,   105,   121,
     525,   807,    90,   140,   605,   659,   550,   813,   814,   504,
     147,   142,   603,   508,   536,   640,   142,   105,   551,    51,
    1075,   612,   801,   584,    99,   550,   872,    92,   807,    15,
    1085,   138,    13,   140,   813,   814,   121,   144,   612,   859,
     147,   536,   603,   121,   539,    51,    16,    53,    54,    55,
      56,    92,   140,   945,   549,    63,   121,    90,   659,   147,
      15,   480,   481,    69,   665,   719,  1065,   668,   669,   670,
       8,     9,   105,   145,   728,   656,    14,    26,   587,   400,
     121,   679,   591,   145,   665,   686,   139,   668,   679,   680,
     691,   692,   142,    26,   142,   696,   714,   745,  1066,   319,
     142,    92,   703,     1,    15,     3,    15,   140,   142,   714,
       8,     9,   531,    44,   147,   713,    14,    15,    16,   538,
     689,    19,   713,    26,   121,   716,   141,   728,   934,   949,
     121,   656,   817,   799,   141,   955,   142,  1051,   904,   905,
     665,    90,   716,   668,    92,   689,   587,    90,    15,    90,
     471,   371,    92,    51,    18,   934,   105,    90,   479,   684,
     139,   686,   105,    16,   105,   121,   122,    65,   489,   141,
     714,   801,   105,   121,   775,   141,    15,   807,   808,   117,
     139,   121,   139,   813,   814,    44,    92,    90,   141,   138,
     142,   140,    57,  1074,   691,   144,   142,   140,   147,   140,
     142,   792,   105,   804,   147,   138,   147,   140,   799,   800,
     801,   144,   734,   804,   147,   121,   807,   808,   872,   117,
     821,   119,   813,   814,   142,   799,    44,   142,   819,   820,
     831,    15,    93,   834,    14,   138,   557,   140,   836,   734,
     735,   144,   833,   804,   147,   836,    15,   115,   800,    60,
     118,   119,    63,    16,   845,   846,    15,   443,   444,   678,
     115,   805,   115,   118,   119,   118,   119,    55,   780,   781,
     782,   872,   784,   145,   786,    15,   142,   868,  1098,    90,
     805,   141,   791,    15,   142,   223,   224,   139,   879,   880,
     812,   477,   478,   146,   105,   148,   107,   142,   142,   142,
     719,   913,   914,    15,   934,    15,    51,   908,   909,   804,
      57,    15,   210,    90,   139,   906,   142,   812,   115,   126,
     911,   118,   119,   126,    55,   223,   224,   139,   105,   140,
      15,   551,   906,    55,  1014,   144,   147,   911,   142,    15,
     526,   142,   142,   934,    51,    15,   284,   285,   142,    90,
     791,   148,   115,   944,   945,   118,   119,   948,   777,   142,
     580,   952,    69,   140,   105,   144,   142,   968,   115,   142,
     147,   118,   119,   964,   948,   966,   123,   597,   276,   141,
     600,   279,   994,   146,   300,   148,   284,   285,   304,   142,
     288,    12,   966,   522,   101,   102,   103,   295,   296,   140,
    1068,   148,     5,   791,   823,   303,   147,  1032,  1073,   818,
     791,  1114,   350,   351,   352,   353,   966,   355,   356,  1070,
    1067,   128,   254,     6,  1025,    -1,  1017,   587,  1019,   848,
       1,  1022,     3,     4,     5,    40,    41,    42,    43,    44,
     859,    12,   991,  1055,  1056,  1057,   994,  1059,  1060,    -1,
    1051,   349,   350,   351,   352,   353,   354,   355,   356,    61,
      -1,    -1,    64,    65,  1065,    -1,  1067,  1068,    -1,   981,
     982,   983,   984,    -1,  1065,   970,   374,    -1,    -1,  1070,
      51,    -1,   991,    90,    55,   994,    37,    38,   997,    -1,
     999,  1103,  1104,  1105,  1106,    -1,  1070,   395,   105,   820,
     398,    -1,   400,  1115,  1065,   403,    -1,    -1,    -1,    -1,
      81,    -1,   833,  1114,   116,   117,    51,  1108,    53,    54,
      55,    56,  1066,   461,   845,   846,    63,    64,    65,    -1,
     949,    -1,    -1,   140,    69,   433,   955,    90,    -1,    -1,
     147,  1066,    90,  1052,    -1,   443,   444,   868,   119,    -1,
     991,    -1,   105,   994,    -1,    -1,   997,   105,   999,   880,
     458,    -1,   460,   461,   115,    -1,  1078,   118,   119,    90,
    1065,   791,  1081,   471,  1083,    -1,  1085,   475,  1087,   116,
     117,   479,    -1,    -1,   105,    -1,   484,   140,    90,    -1,
      -1,   489,   140,   509,   147,   146,    -1,   148,    -1,   147,
     516,  1110,    -1,   105,    -1,    90,    -1,   142,    -1,    -1,
     991,  1052,   528,   994,    -1,    -1,   997,    -1,   999,   140,
     105,   807,   808,   944,   522,    -1,   147,   813,   814,    -1,
      -1,   952,    51,   531,    53,    54,    55,    56,   140,   210,
    1081,    -1,  1083,   964,  1085,   147,  1087,    -1,    -1,    -1,
      69,   549,    -1,   839,   840,   140,   842,   843,    90,   557,
     115,    90,   147,   118,   119,   581,   582,    -1,    -1,  1110,
      -1,  1052,   892,   105,   894,    94,   105,    -1,   898,  1098,
      -1,   100,   101,   102,   103,    51,   584,    53,    54,    55,
      56,   146,    -1,   148,    -1,   611,  1017,    -1,  1019,    -1,
    1081,  1022,  1083,    69,  1085,   603,  1087,    -1,   140,   128,
      -1,   140,   131,    -1,    -1,   147,    90,   288,   147,    85,
      -1,    -1,    -1,   142,   295,   296,    -1,    -1,    94,  1110,
      -1,   105,   303,    -1,   100,   101,   102,   103,    -1,    -1,
      -1,   679,    61,   314,    -1,    64,    65,    -1,   934,    51,
      -1,    53,    54,    55,    56,   975,   976,     1,    -1,     3,
       4,     5,   128,    -1,    -1,   131,   140,    69,    12,    -1,
      -1,   957,   688,   147,    -1,   713,    -1,   997,   349,   999,
      -1,   679,   680,   354,    -1,    -1,    -1,  1108,    -1,    -1,
      -1,    51,    94,    53,    54,    55,    56,   116,   117,   101,
     102,   103,    -1,   374,    -1,    -1,    -1,    51,    -1,    69,
      62,    55,    64,    65,    -1,   713,    63,    64,    65,  1039,
      -1,    -1,  1042,    83,   395,    -1,   128,    -1,   744,   400,
     401,    -1,   403,    62,    94,    64,    65,    81,    -1,    -1,
     100,   101,   102,   103,    -1,    -1,    -1,    -1,   764,    -1,
      -1,    63,    64,    65,    -1,    -1,    -1,  1077,    -1,    -1,
      -1,  1081,    -1,  1083,   116,   117,    -1,  1087,   128,   116,
     117,   131,   443,   444,    -1,   119,    -1,    51,    -1,    53,
      54,    55,    56,    -1,   144,    -1,    -1,   116,   117,   460,
    1110,    -1,    -1,    -1,   792,    69,    -1,    -1,   836,    -1,
     471,    -1,   800,   801,   116,   117,   804,    -1,   479,   807,
     808,    85,    -1,    -1,    -1,   813,   814,    -1,   489,    -1,
      94,   819,   820,    -1,    -1,    -1,   100,   101,   102,   103,
      -1,    63,    64,    65,    -1,   833,    -1,    -1,   836,    -1,
      51,    -1,    53,    54,    55,    56,   115,   845,   846,   118,
     119,   522,    -1,    -1,   128,   871,    -1,   131,    69,    -1,
     531,    63,    64,    65,    -1,    -1,   210,    -1,    -1,    -1,
     868,   887,    83,   142,   545,    -1,   547,   146,    -1,   148,
      -1,   879,   880,    94,   116,   117,   557,    -1,    -1,   100,
     101,   102,   103,    44,    -1,    63,    64,    65,    -1,    -1,
      -1,    -1,    63,    64,    65,    -1,    -1,    63,    64,    65,
     121,    -1,    -1,    -1,   116,   117,    -1,   128,    -1,    -1,
     131,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,   144,    -1,    -1,   934,    88,    89,    -1,
      -1,   612,    -1,    -1,   288,    -1,   944,   945,   116,   117,
     101,   295,   296,    -1,   952,   116,   117,    -1,    -1,   303,
     116,   117,    -1,    -1,    -1,   115,   964,    -1,   118,   119,
     314,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,     1,    -1,     3,     4,     5,    -1,
      -1,   142,    -1,    -1,    -1,    12,   146,    51,   148,    53,
      54,    55,    56,    -1,    -1,   349,    -1,    -1,    -1,   680,
     354,    -1,    -1,    -1,    -1,    69,    -1,    -1,    -1,  1017,
      -1,  1019,    -1,    51,  1022,    53,    54,    55,    56,    -1,
     374,    -1,   115,    -1,    51,   118,   119,    -1,    55,    -1,
      94,    69,    88,    89,    -1,   716,   100,   101,   102,   103,
      -1,   395,    -1,    -1,    -1,   101,   400,   401,    -1,   403,
      -1,    -1,   145,   146,    81,   148,    94,  1065,    -1,    -1,
      -1,    -1,   100,    -1,   128,    -1,    51,   131,    53,    54,
      55,    56,   128,   129,   130,   131,   132,   133,    -1,    -1,
     144,    -1,    -1,    51,    69,    53,    54,    55,    56,   443,
     444,    -1,   119,    -1,    51,    -1,    53,    54,    55,    56,
    1108,    69,    -1,    -1,    -1,    -1,   460,    -1,    -1,    94,
      -1,   792,    69,    -1,    -1,   100,    -1,   471,   799,   800,
     801,    -1,    -1,    -1,    -1,   479,   807,    -1,    85,    -1,
      -1,    -1,   813,   814,    -1,   489,    -1,    94,   819,   820,
      -1,    -1,    -1,   100,   101,   102,   103,    -1,    -1,    -1,
     101,    -1,   833,    -1,    -1,     1,    -1,     3,     4,     5,
       6,    -1,    -1,    -1,   845,   846,    12,    -1,   522,    -1,
      -1,   128,    88,    89,   131,    -1,    -1,   531,   129,   130,
     131,   132,   133,   210,    -1,   101,    -1,   868,    -1,    -1,
      -1,   545,    -1,   547,    -1,    88,    89,    -1,   879,   880,
      -1,    -1,    -1,   557,    -1,    51,     0,    -1,   101,    55,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    13,
      14,    15,    16,    17,    18,   906,    20,    -1,    -1,    -1,
     911,    25,    26,    27,    -1,    81,   129,   130,   131,   132,
     133,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    51,   934,    53,    54,    55,    56,   612,    -1,
      -1,   288,    -1,   944,   945,    -1,    -1,   948,   295,   296,
      69,   952,    -1,   119,    -1,    51,   303,    53,    54,    55,
      56,    -1,    -1,   964,    -1,   966,    -1,   314,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    94,    90,    -1,    -1,    -1,
      -1,   100,   101,   102,   103,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    94,    -1,
      -1,   115,   349,    -1,   118,   119,   680,   354,    -1,   128,
      -1,    -1,   131,    -1,    -1,    -1,  1017,    -1,  1019,    -1,
      -1,  1022,    -1,    -1,   138,   139,    -1,   374,    -1,    -1,
     144,   145,   146,   147,   148,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   716,    -1,   210,    -1,    -1,    -1,   395,    -1,
      -1,    -1,    -1,   400,   401,    -1,   403,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  1070,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   443,   444,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1108,    -1,    -1,
      -1,    -1,    -1,   460,    -1,    -1,    -1,    -1,   792,    -1,
      -1,    -1,   288,    -1,   471,   799,   800,   801,    -1,   295,
     296,    -1,   479,   807,    -1,    -1,    -1,   303,    -1,   813,
     814,    -1,   489,    -1,    -1,   819,   820,    -1,   314,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   833,
      72,    73,    74,    75,    76,    77,    78,    -1,    80,    81,
      -1,   845,   846,    -1,    -1,   522,    88,    89,    -1,    -1,
      -1,    -1,    -1,   349,   531,    -1,    -1,    -1,   354,   101,
      -1,    -1,    -1,    -1,   868,    -1,    -1,    -1,   545,    -1,
     547,    -1,    -1,    -1,    -1,   879,   880,    -1,   374,    -1,
     557,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   395,
      -1,    -1,   906,    -1,   400,   401,    -1,   911,    -1,    -1,
      -1,     1,    -1,     3,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    12,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     934,    -1,    -1,    -1,    -1,   612,    -1,    -1,    -1,    -1,
     944,   945,    -1,    -1,   948,    -1,    -1,    -1,   952,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     964,    51,   966,    -1,   460,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   471,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   479,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   489,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   680,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1017,    -1,  1019,    -1,    -1,  1022,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   522,    -1,    -1,   119,
      -1,    -1,    -1,    -1,    -1,   531,    -1,    -1,    -1,   716,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   545,
      -1,   547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   557,    -1,    -1,    -1,    -1,  1070,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  1108,    -1,    -1,     1,    -1,     3,
      -1,    -1,    -1,    -1,    -1,   792,   612,    -1,    -1,    -1,
     210,    -1,   799,   800,   801,    -1,    -1,    -1,    -1,    -1,
     807,    -1,    -1,    -1,    -1,    -1,   813,   814,    -1,    -1,
      -1,    -1,   819,   820,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   833,    51,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   845,   846,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   680,    -1,    -1,    -1,    -1,    -1,
      -1,   868,    72,    73,    74,    75,    76,    77,   288,    -1,
      80,    81,   879,   880,    -1,   295,   296,    -1,    88,    89,
      -1,    -1,    -1,   303,    -1,    -1,    -1,    -1,    -1,    -1,
     716,   101,    -1,    -1,    -1,   119,    -1,    -1,    -1,   906,
      -1,    -1,    -1,    -1,   911,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,   934,    -1,   349,
      -1,    -1,    -1,    -1,   354,    -1,    -1,   944,   945,    -1,
      -1,   948,    -1,    -1,    -1,   952,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   374,    -1,    -1,   964,    -1,   966,
      -1,    -1,    -1,    -1,    -1,    -1,   792,    -1,    -1,    -1,
      -1,    -1,    -1,   799,   800,   395,    -1,    -1,    -1,    -1,
     400,    -1,    -1,   403,    -1,    -1,   210,    -1,    -1,    -1,
      -1,    -1,    -1,   819,   820,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   833,    -1,    -1,
    1017,    -1,  1019,    -1,    -1,  1022,    -1,    -1,    -1,   845,
     846,    -1,    -1,   443,   444,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     460,    -1,   868,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   471,    -1,   879,   880,    -1,    -1,    -1,    -1,   479,
      -1,    -1,    -1,  1070,   288,    -1,    -1,    -1,    -1,   489,
      -1,   295,   296,    -1,    -1,    -1,    -1,    -1,    -1,   303,
     906,    -1,    -1,    -1,    -1,   911,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1108,   522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   531,    -1,    -1,    -1,    -1,    -1,    -1,   944,   945,
      -1,    -1,   948,    44,    -1,   349,   952,    -1,    -1,    -1,
     354,    -1,    -1,    -1,    -1,    -1,    -1,   557,   964,    -1,
     966,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     374,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,
      -1,   395,    -1,    -1,    -1,    -1,   400,    -1,    -1,   403,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1017,    -1,  1019,    -1,    -1,  1022,    -1,    -1,    -1,
      -1,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,   443,
     444,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,   460,    19,    -1,    21,
      22,    23,    24,    -1,  1070,    -1,    -1,   471,    30,    31,
      32,    33,    34,    35,    36,   479,    -1,    39,    -1,    -1,
     680,    -1,    -1,    -1,    -1,   489,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,  1108,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   522,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,   531,    -1,    91,
      92,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
     102,   103,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,   557,    -1,    -1,    -1,    -1,    -1,   121,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   792,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     800,   801,    -1,    -1,    -1,    -1,    -1,   807,    -1,    -1,
      -1,    -1,    -1,   813,   814,    -1,    -1,    -1,    -1,   819,
     820,    -1,    -1,    -1,     8,     9,    -1,    -1,    -1,    -1,
      14,    15,    16,   833,    -1,    19,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   845,   846,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    46,    47,    48,    49,    -1,    -1,   868,    53,
      54,    -1,    -1,    -1,    -1,    -1,   680,    -1,    -1,   879,
     880,    65,    66,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    52,    -1,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,
      89,    -1,    -1,   117,   934,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,   944,   945,    -1,    -1,   948,    -1,
      -1,    -1,   952,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   964,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    -1,    -1,
      -1,    -1,    -1,    -1,   143,   144,    -1,    -1,   792,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   800,   801,    -1,    -1,
      -1,    -1,    -1,   807,    -1,    -1,    -1,    -1,    -1,   813,
     814,    -1,    -1,    -1,    -1,   819,   820,  1017,    -1,  1019,
      -1,    -1,  1022,    -1,    -1,    -1,    -1,    -1,    -1,   833,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   223,
     224,   845,   846,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    -1,   868,    80,    81,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    89,   879,   880,    -1,   262,   263,
     264,   265,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,    -1,   276,    -1,    -1,   279,    -1,    -1,    -1,    -1,
     284,   285,    -1,    -1,    -1,    -1,    -1,    -1,  1108,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     934,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     944,   945,    -1,    -1,    -1,    -1,    -1,    -1,   952,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     964,    -1,    -1,    -1,    -1,    -1,   350,   351,   352,   353,
      -1,   355,   356,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   375,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   386,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  1017,   398,  1019,    -1,    -1,  1022,   403,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,   415,   416,   417,    -1,   419,   420,   421,   422,   423,
     424,   425,   426,   427,   428,   429,   430,    -1,    -1,   433,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   443,
     444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   458,    -1,    -1,   461,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   473,
      -1,   475,    -1,   477,   478,    -1,    -1,    -1,    -1,    -1,
     484,    -1,    -1,    -1,  1108,    -1,    -1,    -1,    -1,   493,
      -1,    -1,   496,   497,    -1,    -1,     0,   501,    -1,    -1,
     504,    -1,   506,    -1,   508,   509,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    25,   526,    27,    28,    29,    -1,    -1,    -1,    -1,
      -1,    -1,   536,    37,    38,   539,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,   549,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   566,   567,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
     584,    -1,    -1,    -1,    88,    89,    90,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    -1,    99,    -1,   101,    -1,   603,
      -1,   105,   606,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,    -1,    -1,    -1,   139,   140,   141,   142,    -1,
      -1,   145,   146,   147,   148,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,   679,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,   691,    -1,    -1,
     694,   695,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,   713,
      63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,
      -1,   725,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     734,   735,    -1,    -1,    44,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,   101,   796,    -1,    -1,    -1,    -1,   801,   802,    -1,
     804,    -1,    -1,   807,   808,    -1,    -1,    -1,   812,   813,
     814,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   836,    -1,    -1,   839,   840,    -1,   842,   843,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,   867,    -1,    88,    89,   871,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,    -1,   888,   889,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   901,   902,   122,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,   917,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,   927,   928,    -1,    -1,    -1,     0,     1,
     934,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,   957,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,   970,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,
      -1,    -1,     0,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  1065,   134,   135,   136,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,   146,    -1,   148,    25,    26,    27,
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
      -1,    -1,    -1,    25,    -1,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,
      92,    93,    -1,    -1,    -1,    -1,    -1,    99,    -1,   101,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,     0,    -1,    -1,   139,   140,   141,
     142,    -1,    -1,   145,   146,   147,   148,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    25,
      26,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    90,    -1,    -1,    93,    -1,    -1,
      -1,    -1,    -1,    99,    -1,   101,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,    -1,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
       0,    -1,   138,   139,   140,   141,   142,    -1,   144,   145,
     146,   147,   148,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    25,    -1,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      90,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    99,
      -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,   139,
     140,   141,   142,    -1,   144,   145,   146,   147,   148,    13,
      14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    25,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    90,    -1,    92,    93,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,   121,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,     0,    -1,   138,   139,   140,    -1,   142,    -1,
      -1,   145,   146,   147,   148,    13,    14,    15,    -1,    17,
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
      -1,    -1,    -1,    25,    -1,    -1,    -1,    -1,    -1,    -1,
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
     132,   133,    -1,    -1,     0,    -1,    -1,   139,   140,    -1,
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
      -1,    -1,    -1,   139,   140,    -1,   142,    -1,    -1,   145,
     146,   147,   148,     1,    -1,     3,     4,     5,     6,     7,
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
     119,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,   134,   135,   136,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,   146,    -1,   148,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,
      -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,
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
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,   135,   136,    -1,   138,    -1,    -1,    -1,    -1,
      -1,   144,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    26,    -1,    -1,    -1,    30,
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
      -1,    -1,    -1,   134,   135,   136,    19,   138,    21,    22,
      23,    24,    -1,   144,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    92,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,   102,
     103,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,   121,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,   134,   135,   136,    -1,    19,    -1,    21,    22,    23,
      24,   144,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
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
     144,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,   102,   103,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    63,
      -1,    -1,    -1,    -1,    -1,    69,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
      -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,
      -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      63,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
      -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,
      -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,
     143,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    -1,    -1,    80,    81,
      -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,   101,
     102,    -1,    -1,    -1,   106,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,   143,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,   143,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    19,   134,    21,    22,    23,    24,    -1,
      -1,    -1,   142,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,   134,   135,
     136,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
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
     100,    -1,   102,   103,   104,    -1,   106,   107,   108,    -1,
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
      98,    -1,   100,    -1,   102,    -1,   104,    -1,   106,   107,
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
      -1,    -1,   102,   103,   104,    -1,   106,   107,   108,    -1,
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
     102,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
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
      94,    95,    -1,    97,    98,    -1,    -1,    -1,   102,    -1,
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
      -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,
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
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
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
      94,    95,    -1,    97,    98,    -1,    -1,    51,    52,    -1,
     104,    55,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
     134,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
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
      -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    72,    73,    74,    75,    76,    77,    78,    79,
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
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,    -1,    93,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    72,    73,    74,
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
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133
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
     176,   177,   179,   180,   189,   203,   220,   241,   242,   258,
     259,   260,   264,   265,   266,   272,   273,   274,   276,   277,
     278,   279,   280,   281,   317,   330,     0,   154,    21,    22,
      30,    31,    32,    39,    51,    55,    69,    88,    91,    94,
     134,   164,   166,   181,   182,   203,   220,   278,   281,   317,
     182,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    55,    70,    71,    72,    73,    74,    75,
      76,    77,    80,    81,    86,    87,    88,    89,   100,   101,
     102,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   135,   136,   143,   144,   183,   187,   188,   280,   312,
     204,    91,   163,   164,   166,   167,   180,   189,   220,   278,
     279,   281,   167,   210,   212,    69,    91,   173,   180,   220,
     225,   278,   281,    33,    34,    35,    36,    48,    49,    50,
      51,    55,   106,   183,   184,   185,   274,   115,   118,   119,
     146,   148,   167,   268,   269,   270,   323,   327,   328,   329,
      51,    69,   100,   102,   103,   135,   172,   189,   195,   198,
     201,   260,   315,   316,   195,   195,   144,   192,   193,   196,
     197,   330,   192,   197,   144,   324,   184,   155,   138,   189,
     220,   189,   189,   189,    55,     1,    94,   157,   158,   160,
     174,   175,   330,   205,   207,   190,   201,   315,   330,   189,
     314,   315,   330,    91,   142,   179,   220,   278,   281,   208,
      53,    54,    56,    63,    69,   107,   183,   275,    63,    64,
      65,   116,   117,   261,   262,    61,   261,    62,   261,    63,
     261,    63,   261,    58,    59,   168,   189,   189,   323,   329,
      40,    41,    42,    43,    44,    37,    38,    51,    53,    54,
      55,    56,    69,    83,    94,   100,   101,   102,   103,   128,
     131,   144,   284,   285,   286,   287,   288,   291,   292,   293,
     294,   296,   297,   298,   299,   301,   302,   303,   306,   307,
     308,   309,   310,   330,   284,   286,    28,   240,   121,   142,
      94,   100,   176,   121,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    88,    89,    93,   101,
     122,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    90,   105,   140,   147,   321,    90,   321,   322,    26,
     138,   244,   260,    92,    92,   192,   197,   244,   163,    51,
      55,   181,    58,    59,   285,   125,   282,    90,   140,   321,
     219,   313,    90,   147,   320,   156,   157,    55,   284,   284,
      16,   221,   327,   121,    90,   140,   321,    92,    92,   221,
     167,   167,    55,    90,   140,   321,    25,   107,   142,   271,
     323,   115,   270,    20,   246,   327,    57,    57,   189,   189,
     189,    93,   142,   199,   200,   330,    57,   199,   200,    85,
     194,   195,   201,   315,   330,   195,   163,   323,   325,   163,
     328,   159,   138,   157,    90,   321,    92,   160,   174,   145,
     323,   329,   325,   160,   325,   141,   200,   326,   329,   200,
     326,   139,   326,    55,   176,   177,   178,   142,    90,   140,
     321,   144,   237,   296,   301,    63,   261,   263,   267,   268,
      63,   262,    61,    62,    63,    63,   101,   101,   154,   167,
     167,   167,   167,   160,   163,   163,    57,   121,    57,   327,
     300,    85,   296,   301,   121,   156,   189,   142,   311,   330,
      51,   142,   311,   327,   142,   295,   189,   142,   295,    51,
     142,   295,    51,   121,   156,   239,   100,   168,   189,   201,
     202,   174,   142,   179,   142,   161,   162,   168,   180,   189,
     191,   202,   220,   281,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,    51,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,    51,    52,    55,   187,   192,   318,   319,   194,   201,
      51,    52,    55,   187,   192,   318,    51,    55,   318,   245,
     243,   162,   189,   191,   162,   191,    99,   171,   217,   283,
     216,    51,    55,   181,   318,   194,   318,   156,   163,   165,
      15,    13,   254,   330,   121,   121,   157,    16,    51,    55,
     194,    51,    55,   157,    27,   222,   327,   222,    51,    55,
     194,    51,    55,   214,   186,   157,    25,   246,   189,   201,
      15,   189,   189,   189,   324,   100,   189,   198,   315,   189,
     316,   325,   145,   323,   200,   200,   325,   145,   184,   152,
     139,   191,   325,   160,   206,   315,   176,   178,    51,    55,
     194,    51,    55,   296,   209,   142,    63,   157,   268,   189,
     189,    51,    69,   100,   226,   301,   325,   325,   142,   172,
     189,    15,    51,    69,   288,   293,   310,    85,   294,   299,
     306,   308,   301,   303,   308,    51,   301,   172,   189,    15,
      79,   126,   231,   233,   330,   189,   200,   325,   178,   142,
      44,   121,    44,    90,   140,   321,   324,    92,    92,   192,
     197,   141,   200,    92,    92,   193,   197,   193,   197,   231,
     231,   170,   327,   167,   156,   141,    15,   325,   183,   189,
     202,   255,   330,    18,   224,   330,    17,   223,   224,    92,
      92,   141,    92,    92,   224,   211,   213,   141,   167,   184,
     139,    34,    35,    36,    51,    55,    63,    91,   220,   249,
     250,   251,   252,   253,   273,   277,    15,   200,   221,   189,
     199,    85,   315,   139,   325,   326,   141,   234,   324,    29,
     113,   238,   139,   142,   298,   325,   142,    85,    44,    44,
     311,   327,   142,   295,   142,   295,   142,   295,   142,   295,
     295,    44,    44,   228,   230,   232,   287,   289,   290,   293,
     301,   302,   304,   305,   308,   310,   156,   100,   189,   178,
     160,   189,    51,    55,   194,    51,    55,    57,   123,   162,
     191,   168,   191,   171,    92,   162,   191,   162,   191,   171,
     244,   240,   156,   157,   231,   218,   327,    15,    93,   256,
     330,   157,    14,   257,   330,   167,    15,    92,    15,   157,
     157,   222,    55,    90,   221,    93,   126,   189,   157,   325,
     200,   145,   146,   156,   157,   227,   142,   100,   325,   189,
     189,   301,   308,   301,   301,   189,   189,   234,   234,    91,
     220,   142,   311,   311,   142,   229,   220,   142,   229,   142,
     229,    15,   189,   141,   189,   189,   162,   191,    15,   139,
     157,   156,    91,   180,   220,   278,   281,   221,   157,   221,
      15,    15,   215,    51,    55,   157,    51,   252,   224,   246,
     247,    51,   235,   236,   297,    15,   139,   301,   301,   142,
     298,   295,   142,   295,   295,   295,   126,   126,    55,    90,
     289,   293,   142,   228,   229,   305,   308,   301,   304,   308,
     301,   139,    15,    55,    90,   140,   321,   157,   157,   157,
      25,   224,   248,   142,   324,   142,   301,   142,   301,    55,
     311,   142,   229,   142,   229,   142,   229,   142,   229,   229,
      51,    55,   194,    51,    55,   254,   223,    15,   249,    15,
     236,   301,   295,   301,   308,   301,   301,   141,   221,   229,
     142,   229,   229,   229,   157,   301,   248,   229
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
     203,   212,   213,   203,   203,   203,   203,   214,   215,   203,
     216,   203,   217,   218,   203,   219,   203,   203,   203,   203,
     203,   203,   203,   220,   221,   221,   221,   222,   222,   223,
     223,   224,   224,   225,   225,   226,   226,   226,   226,   226,
     226,   226,   226,   227,   226,   228,   228,   228,   228,   229,
     229,   230,   230,   230,   230,   230,   230,   230,   230,   230,
     230,   230,   230,   230,   230,   230,   231,   231,   232,   233,
     233,   233,   234,   234,   235,   235,   236,   236,   237,   237,
     238,   238,   239,   240,   241,   241,   241,   241,   242,   242,
     242,   242,   242,   242,   242,   242,   242,   243,   244,   245,
     244,   246,   247,   247,   248,   248,   249,   250,   250,   251,
     251,   252,   252,   252,   252,   252,   252,   252,   252,   252,
     252,   253,   254,   254,   255,   255,   255,   256,   256,   257,
     257,   258,   258,   258,   258,   259,   259,   260,   260,   260,
     260,   261,   261,   262,   263,   262,   262,   262,   264,   264,
     265,   265,   266,   267,   267,   268,   268,   269,   269,   270,
     271,   270,   272,   272,   273,   273,   273,   274,   275,   275,
     275,   275,   275,   275,   276,   276,   277,   277,   277,   277,
     278,   278,   278,   278,   278,   279,   279,   280,   280,   280,
     280,   280,   280,   280,   280,   280,   281,   281,   282,   283,
     282,   284,   284,   285,   285,   285,   286,   286,   286,   286,
     287,   287,   288,   288,   289,   289,   290,   290,   291,   291,
     292,   292,   293,   293,   294,   294,   294,   294,   295,   295,
     296,   296,   296,   296,   296,   296,   296,   296,   296,   296,
     296,   296,   296,   296,   296,   297,   297,   297,   297,   297,
     298,   298,   299,   300,   299,   301,   301,   302,   303,   304,
     305,   305,   306,   306,   307,   307,   308,   308,   309,   309,
     310,   310,   311,   311,   312,   313,   312,   314,   314,   315,
     315,   316,   316,   316,   316,   316,   316,   316,   316,   317,
     317,   317,   318,   318,   318,   318,   319,   319,   319,   320,
     320,   321,   321,   322,   322,   323,   323,   324,   324,   325,
     326,   326,   326,   327,   327,   328,   328,   329,   329,   330
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
       7,     0,     0,     7,     5,     4,     9,     0,     0,     9,
       0,     6,     0,     0,     8,     0,     5,     4,     4,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       5,     1,     2,     1,     1,     1,     4,     6,     3,     5,
       2,     4,     1,     0,     4,     4,     2,     2,     1,     2,
       0,     6,     8,     4,     6,     4,     3,     6,     2,     4,
       6,     2,     4,     2,     4,     1,     1,     1,     0,     4,
       1,     4,     1,     4,     1,     3,     1,     1,     4,     1,
       3,     3,     0,     5,     2,     4,     5,     5,     2,     4,
       4,     3,     3,     3,     2,     1,     4,     0,     5,     0,
       5,     5,     1,     1,     1,     5,     1,     1,     3,     1,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     3,
       2,     1,     6,     1,     1,     1,     1,     2,     1,     2,
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



#line 7075 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 2126 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 7293 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3: /* program: $@1 top_compstmt  */
#line 2131 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                    }
#line 7301 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4: /* top_compstmt: top_stmts opt_terms  */
#line 2137 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7309 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5: /* top_stmts: none  */
#line 2143 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7317 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6: /* top_stmts: top_stmt  */
#line 2147 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7325 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7: /* top_stmts: top_stmts terms top_stmt  */
#line 2151 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = stmts_push(p, (yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 7333 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8: /* top_stmts: error top_stmt  */
#line 2155 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7341 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10: /* @2: %empty  */
#line 2162 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7350 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11: /* top_stmt: "'BEGIN'" @2 '{' top_compstmt '}'  */
#line 2167 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 7361 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12: /* bodystmt: compstmt opt_rescue opt_else opt_ensure  */
#line 2179 "mrbgems/mruby-compiler/core/parse.y"
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
#line 7386 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13: /* compstmt: stmts opt_terms  */
#line 2202 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7394 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14: /* stmts: none  */
#line 2208 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7402 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15: /* stmts: stmt  */
#line 2212 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7410 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16: /* stmts: stmts terms stmt  */
#line 2216 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = stmts_push(p, (yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 7418 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17: /* stmts: error stmt  */
#line 2220 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_stmts(p, (yyvsp[0].nd));
                    }
#line 7426 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18: /* $@3: %empty  */
#line 2225 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 7432 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19: /* stmt: "'alias'" fsym $@3 fsym  */
#line 2226 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 7440 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20: /* stmt: "'undef'" undef_list  */
#line 2230 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].nd));
                    }
#line 7448 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21: /* stmt: stmt "'if' modifier" expr_value  */
#line 2234 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 7456 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22: /* stmt: stmt "'unless' modifier" expr_value  */
#line 2238 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), 0, (yyvsp[-2].nd));
                    }
#line 7464 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23: /* stmt: stmt "'while' modifier" expr_value  */
#line 2242 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd) && node_type_p((yyvsp[-2].nd), NODE_BEGIN)) {
                        (yyval.nd) = new_while_mod(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                      else {
                        (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                    }
#line 7477 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24: /* stmt: stmt "'until' modifier" expr_value  */
#line 2251 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd) && node_type_p((yyvsp[-2].nd), NODE_BEGIN)) {
                        (yyval.nd) = new_until_mod(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                      else {
                        (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                      }
                    }
#line 7490 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25: /* stmt: stmt "'rescue' modifier" stmt  */
#line 2260 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26: /* stmt: "'END'" '{' compstmt '}'  */
#line 2264 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 7507 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28: /* stmt: mlhs '=' command_call  */
#line 2270 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7515 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29: /* stmt: lhs '=' mrhs  */
#line 2274 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 7523 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30: /* stmt: mlhs '=' arg  */
#line 2278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7531 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31: /* stmt: mlhs '=' mrhs  */
#line 2282 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 7539 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 32: /* stmt: arg "=>" "local variable or method"  */
#line 2286 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *lhs = new_lvar(p, (yyvsp[0].id));
                      assignable(p, lhs);
                      (yyval.nd) = new_asgn(p, lhs, (yyvsp[-2].nd));
                    }
#line 7549 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34: /* command_asgn: lhs '=' command_rhs  */
#line 2295 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7557 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35: /* command_asgn: var_lhs tOP_ASGN command_rhs  */
#line 2299 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7565 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36: /* command_asgn: primary_value '[' opt_call_args ']' tOP_ASGN command_rhs  */
#line 2303 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7573 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37: /* command_asgn: primary_value call_op "local variable or method" tOP_ASGN command_rhs  */
#line 2307 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7581 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38: /* command_asgn: primary_value call_op "constant" tOP_ASGN command_rhs  */
#line 2311 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7589 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39: /* command_asgn: primary_value "::" "constant" tOP_ASGN command_call  */
#line 2315 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 7598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40: /* command_asgn: primary_value "::" "local variable or method" tOP_ASGN command_rhs  */
#line 2320 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7606 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41: /* command_asgn: defn_head f_opt_arglist_paren '=' command  */
#line 2324 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7619 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42: /* command_asgn: defn_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 2333 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43: /* command_asgn: defs_head f_opt_arglist_paren '=' command  */
#line 2342 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7645 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44: /* command_asgn: defs_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 2351 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7658 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 45: /* command_asgn: backref tOP_ASGN command_rhs  */
#line 2360 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 7667 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47: /* command_rhs: command_call "'rescue' modifier" stmt  */
#line 2368 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7675 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50: /* expr: expr "'and'" expr  */
#line 2376 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7683 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51: /* expr: expr "'or'" expr  */
#line 2380 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7691 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52: /* expr: "'not'" opt_nl expr  */
#line 2384 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7699 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53: /* expr: '!' command_call  */
#line 2388 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7707 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55: /* defn_head: "'def'" fname  */
#line 2395 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 7718 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56: /* $@4: %empty  */
#line 2404 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 7726 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 57: /* defs_head: "'def'" singleton dot_or_colon $@4 fname  */
#line 2408 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 7739 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58: /* expr_value: expr  */
#line 2419 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 7750 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62: /* block_command: block_call call_op2 operation2 command_args  */
#line 2433 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63: /* $@5: %empty  */
#line 2439 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 7767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64: /* cmd_brace_block: "{" $@5 opt_block_param compstmt '}'  */
#line 2446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 7777 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65: /* command: operation command_args  */
#line 2454 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7785 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66: /* command: operation command_args cmd_brace_block  */
#line 2458 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 7794 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67: /* command: primary_value call_op operation2 command_args  */
#line 2463 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7802 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68: /* command: primary_value call_op operation2 command_args cmd_brace_block  */
#line 2467 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 7811 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69: /* command: primary_value "::" operation2 command_args  */
#line 2472 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 7819 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70: /* command: primary_value "::" operation2 command_args cmd_brace_block  */
#line 2476 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 7828 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71: /* command: "'super'" command_args  */
#line 2481 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 7836 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72: /* command: "'yield'" command_args  */
#line 2485 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7844 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73: /* command: "'return'" call_args  */
#line 2489 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7852 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74: /* command: "'break'" call_args  */
#line 2493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7860 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75: /* command: "'next'" call_args  */
#line 2497 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7868 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76: /* mlhs: mlhs_basic  */
#line 2503 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7876 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77: /* mlhs: tLPAREN mlhs_inner rparen  */
#line 2507 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79: /* mlhs_inner: tLPAREN mlhs_inner rparen  */
#line 2514 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80: /* mlhs_basic: mlhs_list  */
#line 2520 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81: /* mlhs_basic: mlhs_list mlhs_item  */
#line 2524 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 7908 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82: /* mlhs_basic: mlhs_list "*" mlhs_node  */
#line 2528 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7916 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83: /* mlhs_basic: mlhs_list "*" mlhs_node ',' mlhs_post  */
#line 2532 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7924 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84: /* mlhs_basic: mlhs_list "*"  */
#line 2536 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 7932 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85: /* mlhs_basic: mlhs_list "*" ',' mlhs_post  */
#line 2540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 7940 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86: /* mlhs_basic: "*" mlhs_node  */
#line 2544 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 7948 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87: /* mlhs_basic: "*" mlhs_node ',' mlhs_post  */
#line 2548 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88: /* mlhs_basic: "*"  */
#line 2552 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 7964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89: /* mlhs_basic: "*" ',' mlhs_post  */
#line 2556 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 7972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91: /* mlhs_item: tLPAREN mlhs_inner rparen  */
#line 2563 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 7980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92: /* mlhs_list: mlhs_item ','  */
#line 2569 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 7988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93: /* mlhs_list: mlhs_list mlhs_item ','  */
#line 2573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 7996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94: /* mlhs_post: mlhs_item  */
#line 2579 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95: /* mlhs_post: mlhs_list mlhs_item  */
#line 2583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8012 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96: /* mlhs_node: variable  */
#line 2589 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8020 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97: /* mlhs_node: primary_value '[' opt_call_args ']'  */
#line 2593 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8028 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98: /* mlhs_node: primary_value call_op "local variable or method"  */
#line 2597 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8036 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99: /* mlhs_node: primary_value "::" "local variable or method"  */
#line 2601 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8044 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100: /* mlhs_node: primary_value call_op "constant"  */
#line 2605 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8052 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101: /* mlhs_node: primary_value "::" "constant"  */
#line 2609 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8062 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102: /* mlhs_node: tCOLON3 "constant"  */
#line 2615 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103: /* mlhs_node: backref  */
#line 2621 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 8081 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104: /* lhs: variable  */
#line 2628 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8089 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105: /* lhs: primary_value '[' opt_call_args ']'  */
#line 2632 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8097 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106: /* lhs: primary_value call_op "local variable or method"  */
#line 2636 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107: /* lhs: primary_value "::" "local variable or method"  */
#line 2640 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8113 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108: /* lhs: primary_value call_op "constant"  */
#line 2644 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 8121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109: /* lhs: primary_value "::" "constant"  */
#line 2648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8131 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110: /* lhs: tCOLON3 "constant"  */
#line 2654 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8141 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111: /* lhs: backref  */
#line 2660 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 8150 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112: /* lhs: "numbered parameter"  */
#line 2665 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 8158 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113: /* cname: "local variable or method"  */
#line 2671 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "class/module name must be CONSTANT");
                    }
#line 8166 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115: /* cpath: tCOLON3 cname  */
#line 2678 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(int_to_node(1), sym_to_node((yyvsp[0].id)));
                    }
#line 8174 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116: /* cpath: cname  */
#line 2682 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(int_to_node(0), sym_to_node((yyvsp[0].id)));
                    }
#line 8182 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117: /* cpath: primary_value "::" cname  */
#line 2686 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), sym_to_node((yyvsp[0].id)));
                    }
#line 8191 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121: /* fname: op  */
#line 2696 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8200 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122: /* fname: reswords  */
#line 2701 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8209 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125: /* undef_list: fsym  */
#line 2712 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(sym_to_node((yyvsp[0].id)), 0);
                    }
#line 8217 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126: /* $@6: %empty  */
#line 2715 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 8223 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127: /* undef_list: undef_list ',' $@6 fsym  */
#line 2716 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), sym_to_node((yyvsp[0].id)));
                    }
#line 8231 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128: /* op: '|'  */
#line 2721 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(or);     }
#line 8237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129: /* op: '^'  */
#line 2722 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(xor);    }
#line 8243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130: /* op: '&'  */
#line 2723 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(and);    }
#line 8249 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131: /* op: "<=>"  */
#line 2724 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(cmp);    }
#line 8255 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132: /* op: "=="  */
#line 2725 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eq);     }
#line 8261 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133: /* op: "==="  */
#line 2726 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eqq);    }
#line 8267 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134: /* op: "=~"  */
#line 2727 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(match);  }
#line 8273 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135: /* op: "!~"  */
#line 2728 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(nmatch); }
#line 8279 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136: /* op: '>'  */
#line 2729 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(gt);     }
#line 8285 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137: /* op: ">="  */
#line 2730 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(ge);     }
#line 8291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138: /* op: '<'  */
#line 2731 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lt);     }
#line 8297 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139: /* op: "<="  */
#line 2732 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(le);     }
#line 8303 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140: /* op: "!="  */
#line 2733 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neq);    }
#line 8309 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141: /* op: "<<"  */
#line 2734 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lshift); }
#line 8315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142: /* op: ">>"  */
#line 2735 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(rshift); }
#line 8321 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143: /* op: '+'  */
#line 2736 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(add);    }
#line 8327 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144: /* op: '-'  */
#line 2737 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(sub);    }
#line 8333 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145: /* op: '*'  */
#line 2738 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 8339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146: /* op: "*"  */
#line 2739 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 8345 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147: /* op: '/'  */
#line 2740 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(div);    }
#line 8351 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148: /* op: '%'  */
#line 2741 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mod);    }
#line 8357 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149: /* op: tPOW  */
#line 2742 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 8363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150: /* op: "**"  */
#line 2743 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 8369 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151: /* op: '!'  */
#line 2744 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(not);    }
#line 8375 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152: /* op: '~'  */
#line 2745 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neg);    }
#line 8381 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153: /* op: "unary plus"  */
#line 2746 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(plus);   }
#line 8387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154: /* op: "unary minus"  */
#line 2747 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(minus);  }
#line 8393 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155: /* op: tAREF  */
#line 2748 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aref);   }
#line 8399 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156: /* op: tASET  */
#line 2749 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aset);   }
#line 8405 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157: /* op: '`'  */
#line 2750 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(tick);   }
#line 8411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198: /* arg: lhs '=' arg_rhs  */
#line 2768 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8419 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199: /* arg: var_lhs tOP_ASGN arg_rhs  */
#line 2772 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200: /* arg: primary_value '[' opt_call_args ']' tOP_ASGN arg_rhs  */
#line 2776 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8435 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201: /* arg: primary_value call_op "local variable or method" tOP_ASGN arg_rhs  */
#line 2780 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202: /* arg: primary_value call_op "constant" tOP_ASGN arg_rhs  */
#line 2784 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203: /* arg: primary_value "::" "local variable or method" tOP_ASGN arg_rhs  */
#line 2788 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204: /* arg: primary_value "::" "constant" tOP_ASGN arg_rhs  */
#line 2792 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8468 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205: /* arg: tCOLON3 "constant" tOP_ASGN arg_rhs  */
#line 2797 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "constant re-assignment");
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8477 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206: /* arg: backref tOP_ASGN arg_rhs  */
#line 2802 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_stmts(p, 0);
                    }
#line 8486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207: /* arg: arg ".." arg  */
#line 2807 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8494 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208: /* arg: arg ".."  */
#line 2811 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 8502 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209: /* arg: tBDOT2 arg  */
#line 2815 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 8510 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210: /* arg: arg "..." arg  */
#line 2819 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8518 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211: /* arg: arg "..."  */
#line 2823 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 8526 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212: /* arg: tBDOT3 arg  */
#line 2827 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 8534 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213: /* arg: arg '+' arg  */
#line 2831 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 8542 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214: /* arg: arg '-' arg  */
#line 2835 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 8550 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215: /* arg: arg '*' arg  */
#line 2839 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 8558 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216: /* arg: arg '/' arg  */
#line 2843 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 8566 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217: /* arg: arg '%' arg  */
#line 2847 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 8574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218: /* arg: arg tPOW arg  */
#line 2851 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 8582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219: /* arg: tUMINUS_NUM "integer literal" tPOW arg  */
#line 2855 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 8590 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220: /* arg: tUMINUS_NUM "float literal" tPOW arg  */
#line 2859 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 8598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221: /* arg: "unary plus" arg  */
#line 2863 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 8606 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222: /* arg: "unary minus" arg  */
#line 2867 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 8614 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223: /* arg: arg '|' arg  */
#line 2871 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 8622 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224: /* arg: arg '^' arg  */
#line 2875 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 8630 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225: /* arg: arg '&' arg  */
#line 2879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 8638 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226: /* arg: arg "<=>" arg  */
#line 2883 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 8646 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227: /* arg: arg '>' arg  */
#line 2887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 8654 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228: /* arg: arg ">=" arg  */
#line 2891 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 8662 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229: /* arg: arg '<' arg  */
#line 2895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 8670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230: /* arg: arg "<=" arg  */
#line 2899 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 8678 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231: /* arg: arg "==" arg  */
#line 2903 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 8686 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232: /* arg: arg "===" arg  */
#line 2907 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 8694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233: /* arg: arg "!=" arg  */
#line 2911 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 8702 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234: /* arg: arg "=~" arg  */
#line 2915 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 8710 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235: /* arg: arg "!~" arg  */
#line 2919 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 8718 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236: /* arg: '!' arg  */
#line 2923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 8726 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237: /* arg: '~' arg  */
#line 2927 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 8734 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238: /* arg: arg "<<" arg  */
#line 2931 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 8742 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239: /* arg: arg ">>" arg  */
#line 2935 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 8750 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240: /* arg: arg "&&" arg  */
#line 2939 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241: /* arg: arg "||" arg  */
#line 2943 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8766 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242: /* arg: arg '?' arg opt_nl ':' arg  */
#line 2947 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 8774 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243: /* arg: arg '?' arg opt_nl "label" arg  */
#line 2951 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 8782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244: /* arg: defn_head f_opt_arglist_paren '=' arg  */
#line 2955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8795 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245: /* arg: defn_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 2964 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8808 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246: /* arg: defs_head f_opt_arglist_paren '=' arg  */
#line 2973 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8821 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247: /* arg: defs_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 2982 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8834 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248: /* arg: primary  */
#line 2991 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250: /* aref_args: args trailer  */
#line 2998 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251: /* aref_args: args comma assocs trailer  */
#line 3002 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd)));
                    }
#line 8858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252: /* aref_args: assocs trailer  */
#line 3006 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_hash(p, (yyvsp[-1].nd)), 0);
                    }
#line 8866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253: /* arg_rhs: arg  */
#line 3012 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8874 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254: /* arg_rhs: arg "'rescue' modifier" arg  */
#line 3016 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8883 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255: /* paren_args: '(' opt_call_args ')'  */
#line 3023 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8891 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256: /* paren_args: '(' args comma tBDOT3 rparen  */
#line 3027 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      (yyval.nd) = new_callargs(p, push((yyvsp[-3].nd), new_splat(p, new_lvar(p, r))),
                                        list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k))),
                                        new_block_arg(p, new_lvar(p, b)));
                    }
#line 8904 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257: /* paren_args: '(' tBDOT3 rparen  */
#line 3036 "mrbgems/mruby-compiler/core/parse.y"
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
#line 8923 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262: /* opt_call_args: args comma  */
#line 3059 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-1].nd),0,0);
                    }
#line 8931 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263: /* opt_call_args: args comma assocs comma  */
#line 3063 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-3].nd),(yyvsp[-1].nd),0);
                    }
#line 8939 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264: /* opt_call_args: assocs comma  */
#line 3067 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,0,(yyvsp[-1].nd),0);
                    }
#line 8947 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265: /* call_args: command  */
#line 3073 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_callargs(p, list1((yyvsp[0].nd)), 0, 0);
                    }
#line 8956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266: /* call_args: args opt_block_arg  */
#line 3078 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-1].nd), 0, (yyvsp[0].nd));
                    }
#line 8964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267: /* call_args: assocs opt_block_arg  */
#line 3082 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268: /* call_args: args comma assocs opt_block_arg  */
#line 3086 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269: /* call_args: block_arg  */
#line 3090 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, 0, (yyvsp[0].nd));
                    }
#line 8988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270: /* @7: %empty  */
#line 3095 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 8997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 271: /* command_args: @7 call_args  */
#line 3100 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272: /* block_arg: "&" arg  */
#line 3107 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 9014 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273: /* block_arg: "&"  */
#line 3111 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, 0);
                    }
#line 9022 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274: /* opt_block_arg: comma block_arg  */
#line 3117 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275: /* opt_block_arg: none  */
#line 3121 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9038 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277: /* args: arg  */
#line 3130 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9047 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278: /* args: "*"  */
#line 3135 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 9055 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279: /* args: "*" arg  */
#line 3139 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 9063 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280: /* args: args comma arg  */
#line 3143 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281: /* args: args comma "*"  */
#line 3148 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 9080 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282: /* args: args comma "*" arg  */
#line 3152 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 9088 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 283: /* mrhs: args comma arg  */
#line 3158 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9097 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 284: /* mrhs: args comma "*" arg  */
#line 3163 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 9105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 285: /* mrhs: "*" arg  */
#line 3167 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 9113 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 287: /* primary: string  */
#line 3174 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_str(p, (yyvsp[0].nd));
                    }
#line 9121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 288: /* primary: xstring  */
#line 3178 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_xstr(p, (yyvsp[0].nd));
                    }
#line 9129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293: /* primary: "method"  */
#line 3186 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 9137 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294: /* @8: %empty  */
#line 3190 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9146 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295: /* primary: "'begin'" @8 bodystmt "'end'"  */
#line 3196 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = new_begin(p, (yyvsp[-1].nd));
                    }
#line 9155 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296: /* @9: %empty  */
#line 3201 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9164 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297: /* $@10: %empty  */
#line 3205 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 9170 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298: /* primary: "(" @9 stmt $@10 rparen  */
#line 3206 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 9179 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299: /* $@11: %empty  */
#line 3210 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 9185 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300: /* primary: "(" $@11 rparen  */
#line 3211 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9193 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301: /* primary: tLPAREN compstmt ')'  */
#line 3215 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9201 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302: /* primary: primary_value "::" "constant"  */
#line 3219 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 9209 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303: /* primary: tCOLON3 "constant"  */
#line 3223 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 9217 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304: /* primary: "[" aref_args ']'  */
#line 3227 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                    }
#line 9225 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305: /* primary: tLBRACE assoc_list '}'  */
#line 3231 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                    }
#line 9233 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306: /* primary: "'return'"  */
#line 3235 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 9241 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307: /* primary: "'yield'" opt_paren_args  */
#line 3239 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 9249 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308: /* primary: "'not'" '(' expr rparen  */
#line 3243 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 9257 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309: /* primary: "'not'" '(' rparen  */
#line 3247 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 9265 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310: /* primary: operation brace_block  */
#line 3251 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), new_callargs(p, 0, 0, (yyvsp[0].nd)));
                    }
#line 9273 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312: /* primary: method_call brace_block  */
#line 3256 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9282 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313: /* @12: %empty  */
#line 3261 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 9293 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314: /* @13: %empty  */
#line 3268 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 9302 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315: /* primary: "->" @12 f_larglist @13 lambda_body  */
#line 3273 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 9315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316: /* primary: "'if'" expr_value then compstmt if_tail "'end'"  */
#line 3285 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 9324 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317: /* primary: "'unless'" expr_value then compstmt opt_else "'end'"  */
#line 3293 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd), (yyvsp[-2].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 9333 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318: /* $@14: %empty  */
#line 3297 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 9339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319: /* $@15: %empty  */
#line 3297 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 9345 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320: /* primary: "'while'" $@14 expr_value do $@15 compstmt "'end'"  */
#line 3300 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 9354 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321: /* $@16: %empty  */
#line 3304 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 9360 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322: /* $@17: %empty  */
#line 3304 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 9366 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323: /* primary: "'until'" $@16 expr_value do $@17 compstmt "'end'"  */
#line 3307 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 9375 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324: /* primary: "'case'" expr_value opt_terms case_body "'end'"  */
#line 3314 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 9383 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325: /* primary: "'case'" opt_terms case_body "'end'"  */
#line 3318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 9391 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326: /* primary: "'case'" expr_value opt_terms "'in'" p_expr then compstmt in_clauses "'end'"  */
#line 3326 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-4].nd), NULL, (yyvsp[-2].nd), FALSE);
                      (yyval.nd) = new_case_match(p, (yyvsp[-7].nd), cons(in_clause, (yyvsp[-1].nd)));
                    }
#line 9400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327: /* $@18: %empty  */
#line 3331 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 9406 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328: /* $@19: %empty  */
#line 3333 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 9412 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329: /* primary: "'for'" for_var "'in'" $@18 expr_value do $@19 compstmt "'end'"  */
#line 3336 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 9421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330: /* @20: %empty  */
#line 3342 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 9432 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331: /* primary: "'class'" cpath superclass @20 bodystmt "'end'"  */
#line 3350 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 9443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332: /* @21: %empty  */
#line 3358 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 9452 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333: /* @22: %empty  */
#line 3363 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), int_to_node(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 9462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334: /* primary: "'class'" "<<" expr @21 term @22 bodystmt "'end'"  */
#line 3370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = node_to_int((yyvsp[-2].nd)->cdr);
                    }
#line 9475 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335: /* @23: %empty  */
#line 3380 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 9486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336: /* primary: "'module'" cpath @23 bodystmt "'end'"  */
#line 3388 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 9497 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337: /* primary: defn_head f_arglist bodystmt "'end'"  */
#line 3398 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 9508 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338: /* primary: defs_head f_arglist bodystmt "'end'"  */
#line 3408 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 9520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 339: /* primary: "'break'"  */
#line 3416 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 9528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340: /* primary: "'next'"  */
#line 3420 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 9536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 341: /* primary: "'redo'"  */
#line 3424 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 9544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 342: /* primary: "'retry'"  */
#line 3428 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 9552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 343: /* primary_value: primary  */
#line 3434 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 350: /* if_tail: "'elsif'" expr_value then compstmt if_tail  */
#line 3453 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9569 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352: /* opt_else: "'else'" compstmt  */
#line 3460 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9577 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 353: /* for_var: lhs  */
#line 3466 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 9585 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355: /* f_margs: f_arg  */
#line 3473 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 9593 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356: /* f_margs: f_arg ',' "*" f_norm_arg  */
#line 3477 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_lvar(p, (yyvsp[0].id)), 0);
                    }
#line 9601 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357: /* f_margs: f_arg ',' "*" f_norm_arg ',' f_arg  */
#line 3481 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_lvar(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9609 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358: /* f_margs: f_arg ',' "*"  */
#line 3485 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3((yyvsp[-2].nd), int_to_node(-1), 0);
                    }
#line 9618 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359: /* f_margs: f_arg ',' "*" ',' f_arg  */
#line 3490 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), int_to_node(-1), (yyvsp[0].nd));
                    }
#line 9626 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360: /* f_margs: "*" f_norm_arg  */
#line 3494 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_lvar(p, (yyvsp[0].id)), 0);
                    }
#line 9634 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361: /* f_margs: "*" f_norm_arg ',' f_arg  */
#line 3498 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_lvar(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9642 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362: /* f_margs: "*"  */
#line 3502 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3(0, int_to_node(-1), 0);
                    }
#line 9651 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363: /* $@24: %empty  */
#line 3507 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                    }
#line 9659 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364: /* f_margs: "*" ',' $@24 f_arg  */
#line 3511 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, int_to_node(-1), (yyvsp[0].nd));
                    }
#line 9667 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365: /* block_args_tail: f_block_kwarg ',' f_kwrest opt_f_block_arg  */
#line 3517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 9675 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366: /* block_args_tail: f_block_kwarg opt_f_block_arg  */
#line 3521 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9683 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367: /* block_args_tail: f_kwrest opt_f_block_arg  */
#line 3525 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 9691 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368: /* block_args_tail: f_block_arg  */
#line 3529 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9699 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369: /* opt_block_args_tail: ',' block_args_tail  */
#line 3535 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9707 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370: /* opt_block_args_tail: %empty  */
#line 3539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9715 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3545 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9723 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3549 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9731 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373: /* block_param: f_arg ',' f_block_optarg opt_block_args_tail  */
#line 3553 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9739 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374: /* block_param: f_arg ',' f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3557 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9747 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375: /* block_param: f_arg ',' f_rest_arg opt_block_args_tail  */
#line 3561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9755 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376: /* block_param: f_arg ',' opt_block_args_tail  */
#line 3565 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9763 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377: /* block_param: f_arg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3569 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9771 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378: /* block_param: f_arg opt_block_args_tail  */
#line 3573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9779 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379: /* block_param: f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3577 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9787 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380: /* block_param: f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3581 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9795 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381: /* block_param: f_block_optarg opt_block_args_tail  */
#line 3585 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9803 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382: /* block_param: f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3589 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9811 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383: /* block_param: f_rest_arg opt_block_args_tail  */
#line 3593 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9819 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384: /* block_param: f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3597 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9827 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385: /* block_param: block_args_tail  */
#line 3601 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9835 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386: /* opt_block_param: none  */
#line 3607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 9844 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387: /* opt_block_param: block_param_def  */
#line 3612 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9853 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388: /* $@25: %empty  */
#line 3618 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p);}
#line 9859 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389: /* block_param_def: '|' $@25 opt_bv_decl '|'  */
#line 3619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390: /* block_param_def: "||"  */
#line 3623 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 9876 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 391: /* block_param_def: '|' block_param opt_bv_decl '|'  */
#line 3628 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 9884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392: /* opt_bv_decl: opt_nl  */
#line 3634 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393: /* opt_bv_decl: opt_nl ';' bv_decls opt_nl  */
#line 3638 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396: /* bvar: "local variable or method"  */
#line 3648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 9909 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398: /* f_larglist: '(' f_args opt_bv_decl ')'  */
#line 3656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 9917 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399: /* f_larglist: f_args  */
#line 3660 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400: /* lambda_body: tLAMBEG compstmt '}'  */
#line 3666 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401: /* lambda_body: "'do' for lambda" bodystmt "'end'"  */
#line 3670 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9941 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402: /* @26: %empty  */
#line 3676 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 9951 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403: /* do_block: "'do' for block" @26 opt_block_param bodystmt "'end'"  */
#line 3684 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 9962 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404: /* block_call: command do_block  */
#line 3693 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9971 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405: /* block_call: block_call call_op2 operation2 opt_paren_args  */
#line 3698 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 9979 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406: /* block_call: block_call call_op2 operation2 opt_paren_args brace_block  */
#line 3702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 9988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407: /* block_call: block_call call_op2 operation2 command_args do_block  */
#line 3707 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 9997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408: /* method_call: operation paren_args  */
#line 3714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 10005 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409: /* method_call: primary_value call_op operation2 opt_paren_args  */
#line 3718 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 10013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410: /* method_call: primary_value "::" operation2 paren_args  */
#line 3722 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 10021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411: /* method_call: primary_value "::" operation3  */
#line 3726 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 10029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412: /* method_call: primary_value call_op paren_args  */
#line 3730 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 10037 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413: /* method_call: primary_value "::" paren_args  */
#line 3734 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), tCOLON2);
                    }
#line 10045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414: /* method_call: "'super'" paren_args  */
#line 3738 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 10053 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415: /* method_call: "'super'"  */
#line 3742 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 10061 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416: /* method_call: primary_value '[' opt_call_args ']'  */
#line 3746 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 10069 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417: /* @27: %empty  */
#line 3752 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10079 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418: /* brace_block: '{' @27 opt_block_param compstmt '}'  */
#line 3759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419: /* @28: %empty  */
#line 3766 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 10100 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 420: /* brace_block: "'do'" @28 opt_block_param bodystmt "'end'"  */
#line 3773 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 10111 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 421: /* case_body: "'when'" args then compstmt cases  */
#line 3784 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 10119 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 422: /* cases: opt_else  */
#line 3790 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 10132 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 424: /* in_clauses: opt_else  */
#line 3803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd) ? list1(new_in(p, NULL, NULL, (yyvsp[0].nd), FALSE)) : 0;
                    }
#line 10140 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 425: /* in_clauses: "'in'" p_expr then compstmt in_clauses  */
#line 3807 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *in_clause = new_in(p, (yyvsp[-3].nd), NULL, (yyvsp[-1].nd), FALSE);
                      (yyval.nd) = cons(in_clause, (yyvsp[0].nd));
                    }
#line 10149 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 428: /* p_as: p_alt "=>" "local variable or method"  */
#line 3819 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_as(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 10157 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 430: /* p_alt: p_alt '|' p_value  */
#line 3826 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_alt(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10165 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 432: /* p_value: numeric  */
#line 3833 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10173 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 433: /* p_value: symbol  */
#line 3837 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10181 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 434: /* p_value: tSTRING  */
#line 3841 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, (yyvsp[0].nd));
                    }
#line 10189 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435: /* p_value: "'nil'"  */
#line 3845 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_nil(p));
                    }
#line 10197 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 436: /* p_value: "'true'"  */
#line 3849 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_true(p));
                    }
#line 10205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 437: /* p_value: "'false'"  */
#line 3853 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_false(p));
                    }
#line 10213 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438: /* p_value: "constant"  */
#line 3857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_const(p, (yyvsp[0].id)));
                    }
#line 10221 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439: /* p_value: primary_value "::" "constant"  */
#line 3861 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id)));
                    }
#line 10229 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440: /* p_value: tCOLON3 "constant"  */
#line 3865 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_value(p, new_colon3(p, (yyvsp[0].id)));
                    }
#line 10237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441: /* p_var: "local variable or method"  */
#line 3871 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_pat_var(p, (yyvsp[0].id));
                    }
#line 10245 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442: /* opt_rescue: "'rescue'" exc_list exc_var then compstmt opt_rescue  */
#line 3879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 10254 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444: /* exc_list: arg  */
#line 3887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10262 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447: /* exc_var: "=>" lhs  */
#line 3895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 449: /* opt_ensure: "'ensure'" compstmt  */
#line 3902 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10278 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456: /* string: string string_fragment  */
#line 3916 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10286 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457: /* string_fragment: "character literal"  */
#line 3922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* tCHAR is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10295 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458: /* string_fragment: tSTRING  */
#line 3927 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* tSTRING is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10304 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459: /* string_fragment: "string literal" tSTRING  */
#line 3932 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* $2 is (len . str), wrap as cons list */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10313 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460: /* string_fragment: "string literal" string_rep tSTRING  */
#line 3937 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10321 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 462: /* string_rep: string_rep string_interp  */
#line 3944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10329 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 463: /* string_interp: tSTRING_MID  */
#line 3950 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      /* $1 is already in (len . str) format */
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10338 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 464: /* @29: %empty  */
#line 3955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 10346 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465: /* string_interp: tSTRING_PART @29 compstmt '}'  */
#line 3960 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p,(yyvsp[-2].nd));
                      /* $1 is already in (len . str) format, create (-1 . node) for expression */
                      node *expr_elem = cons(int_to_node(-1), (yyvsp[-1].nd));
                      (yyval.nd) = list2((yyvsp[-3].nd), expr_elem);
                    }
#line 10357 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 466: /* string_interp: tLITERAL_DELIM  */
#line 3967 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 10365 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467: /* string_interp: tHD_LITERAL_DELIM heredoc_bodies  */
#line 3971 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 10373 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 468: /* xstring: tXSTRING_BEG tXSTRING  */
#line 3977 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = cons((yyvsp[0].nd), (node*)NULL);
                    }
#line 10381 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 469: /* xstring: tXSTRING_BEG string_rep tXSTRING  */
#line 3981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10389 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 470: /* regexp: tREGEXP_BEG tREGEXP  */
#line 3987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *data = (yyvsp[0].nd);  /* ((len . pattern) . (flags . encoding)) */
                      const char *flags = (const char*)data->cdr->car;
                      const char *encoding = (const char*)data->cdr->cdr;
                      /* Use data->car directly as pattern_list: (len . pattern) */
                      node *pattern_list = cons(data->car, (node*)NULL);
                      (yyval.nd) = new_regx(p, pattern_list, flags, encoding);
                    }
#line 10402 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 471: /* regexp: tREGEXP_BEG string_rep tREGEXP  */
#line 3996 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *data = (yyvsp[0].nd);  /* ((len . pattern) . (flags . encoding)) */
                      const char *flags = (const char*)data->cdr->car;
                      const char *encoding = (const char*)data->cdr->cdr;
                      /* Append the pattern from $3->car to the string list $2 */
                      node *complete_list = push((yyvsp[-1].nd), data->car);
                      (yyval.nd) = new_regx(p, complete_list, flags, encoding);
                    }
#line 10415 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475: /* heredoc_body: tHEREDOC_END  */
#line 4014 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, new_str_empty(p));
                      heredoc_end(p);
                    }
#line 10425 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476: /* heredoc_body: heredoc_string_rep tHEREDOC_END  */
#line 4020 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 10433 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479: /* heredoc_string_interp: tHD_STRING_MID  */
#line 4030 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 10443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480: /* @30: %empty  */
#line 4036 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 10451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481: /* heredoc_string_interp: tHD_STRING_PART @30 compstmt '}'  */
#line 4041 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p, (yyvsp[-2].nd));
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      /* $1 is already in (len . str) format, create (-1 . node) for expression */
                      node *expr_elem = cons(int_to_node(-1), (yyvsp[-1].nd));
                      info->doc = push(push(info->doc, (yyvsp[-3].nd)), expr_elem);
                    }
#line 10463 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482: /* words: tWORDS_BEG tSTRING  */
#line 4051 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 10471 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483: /* words: tWORDS_BEG string_rep tSTRING  */
#line 4055 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      n = push(n, (yyvsp[0].nd));
                      (yyval.nd) = new_words(p, n);
                    }
#line 10481 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484: /* symbol: basic_symbol  */
#line 4063 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 10489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485: /* symbol: "symbol" "string literal" string_rep tSTRING  */
#line 4067 "mrbgems/mruby-compiler/core/parse.y"
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
#line 10505 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486: /* symbol: "symbol" "numbered parameter"  */
#line 4079 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[0].num));
                      (yyval.nd) = new_sym(p, sym);
                    }
#line 10514 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487: /* basic_symbol: "symbol" sym  */
#line 4086 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_END;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 10523 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492: /* sym: tSTRING  */
#line 4097 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 10531 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 493: /* sym: "string literal" tSTRING  */
#line 4101 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 10539 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 494: /* symbols: tSYMBOLS_BEG tSTRING  */
#line 4107 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 10547 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495: /* symbols: tSYMBOLS_BEG string_rep tSTRING  */
#line 4111 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      n = push(n, (yyvsp[0].nd));
                      (yyval.nd) = new_symbols(p, n);
                    }
#line 10557 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 498: /* numeric: tUMINUS_NUM "integer literal"  */
#line 4121 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 10565 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499: /* numeric: tUMINUS_NUM "float literal"  */
#line 4125 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 10573 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 500: /* variable: "local variable or method"  */
#line 4131 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 10581 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501: /* variable: "instance variable"  */
#line 4135 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 10589 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502: /* variable: "global variable"  */
#line 4139 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 10597 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 503: /* variable: "class variable"  */
#line 4143 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 10605 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504: /* variable: "constant"  */
#line 4147 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 10613 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505: /* var_lhs: variable  */
#line 4153 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 10621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506: /* var_lhs: "numbered parameter"  */
#line 4157 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 10629 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507: /* var_ref: variable  */
#line 4163 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 10637 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508: /* var_ref: "numbered parameter"  */
#line 4167 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 10645 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509: /* var_ref: "'nil'"  */
#line 4171 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 10653 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510: /* var_ref: "'self'"  */
#line 4175 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 10661 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511: /* var_ref: "'true'"  */
#line 4179 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 10669 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512: /* var_ref: "'false'"  */
#line 4183 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 10677 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513: /* var_ref: "'__FILE__'"  */
#line 4187 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, cons(cons(int_to_node(strlen(fn)), (node*)fn), (node*)NULL));
                    }
#line 10689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514: /* var_ref: "'__LINE__'"  */
#line 4195 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 10700 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515: /* var_ref: "'__ENCODING__'"  */
#line 4202 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, MRB_SYM_2(p->mrb, __ENCODING__), 0);
                    }
#line 10708 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518: /* superclass: %empty  */
#line 4212 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10716 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519: /* $@31: %empty  */
#line 4216 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 10725 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520: /* superclass: '<' $@31 expr_value term  */
#line 4221 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10733 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523: /* f_arglist_paren: '(' f_args rparen  */
#line 4237 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 10743 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524: /* f_arglist_paren: '(' f_arg ',' tBDOT3 rparen  */
#line 4243 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 10751 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525: /* f_arglist_paren: '(' tBDOT3 rparen  */
#line 4247 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 10759 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527: /* f_arglist: f_args term  */
#line 4254 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528: /* f_arglist: f_arg ',' tBDOT3 term  */
#line 4258 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 10775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529: /* f_arglist: "..." term  */
#line 4262 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 10783 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530: /* f_label: "local variable or method" "label"  */
#line 4268 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 10791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531: /* f_label: "numbered parameter" "label"  */
#line 4272 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 10799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532: /* f_kw: f_label arg  */
#line 4278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 10809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533: /* f_kw: f_label  */
#line 4284 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 10818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534: /* f_block_kw: f_label primary_value  */
#line 4291 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 10828 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535: /* f_block_kw: f_label  */
#line 4297 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 10837 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536: /* f_block_kwarg: f_block_kw  */
#line 4304 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10845 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537: /* f_block_kwarg: f_block_kwarg ',' f_block_kw  */
#line 4308 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10853 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538: /* f_kwarg: f_kw  */
#line 4314 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10861 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539: /* f_kwarg: f_kwarg ',' f_kw  */
#line 4318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10869 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542: /* f_kwrest: kwrest_mark "local variable or method"  */
#line 4328 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 10877 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543: /* f_kwrest: kwrest_mark  */
#line 4332 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(pow);
                    }
#line 10885 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544: /* args_tail: f_kwarg ',' f_kwrest opt_f_block_arg  */
#line 4338 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 10893 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545: /* args_tail: f_kwarg opt_f_block_arg  */
#line 4342 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 10901 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546: /* args_tail: f_kwrest opt_f_block_arg  */
#line 4346 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].id), (yyvsp[0].id));
                    }
#line 10909 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547: /* args_tail: f_block_arg  */
#line 4350 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 10917 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548: /* opt_args_tail: ',' args_tail  */
#line 4356 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549: /* opt_args_tail: %empty  */
#line 4360 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 10933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550: /* f_args: f_arg ',' f_optarg ',' f_rest_arg opt_args_tail  */
#line 4366 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10941 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551: /* f_args: f_arg ',' f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552: /* f_args: f_arg ',' f_optarg opt_args_tail  */
#line 4374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 10957 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553: /* f_args: f_arg ',' f_optarg ',' f_arg opt_args_tail  */
#line 4378 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10965 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554: /* f_args: f_arg ',' f_rest_arg opt_args_tail  */
#line 4382 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555: /* f_args: f_arg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4386 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10981 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 556: /* f_args: f_arg opt_args_tail  */
#line 4390 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10989 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557: /* f_args: f_optarg ',' f_rest_arg opt_args_tail  */
#line 4394 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558: /* f_args: f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 4398 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11005 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559: /* f_args: f_optarg opt_args_tail  */
#line 4402 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 11013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560: /* f_args: f_optarg ',' f_arg opt_args_tail  */
#line 4406 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561: /* f_args: f_rest_arg opt_args_tail  */
#line 4410 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 11029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562: /* f_args: f_rest_arg ',' f_arg opt_args_tail  */
#line 4414 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 11037 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563: /* f_args: args_tail  */
#line 4418 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 11045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564: /* f_args: %empty  */
#line 4422 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 11054 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 565: /* f_bad_arg: "constant"  */
#line 4429 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 11063 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 566: /* f_bad_arg: "instance variable"  */
#line 4434 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 11072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 567: /* f_bad_arg: "global variable"  */
#line 4439 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 11081 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 568: /* f_bad_arg: "class variable"  */
#line 4444 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 11090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 569: /* f_bad_arg: "numbered parameter"  */
#line 4449 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 11099 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 570: /* f_norm_arg: f_bad_arg  */
#line 4456 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 11107 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 571: /* f_norm_arg: "local variable or method"  */
#line 4460 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11116 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572: /* f_arg_item: f_norm_arg  */
#line 4467 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 11124 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573: /* @32: %empty  */
#line 4471 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 11132 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574: /* f_arg_item: tLPAREN @32 f_margs rparen  */
#line 4475 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_marg(p, (yyvsp[-1].nd));
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 11142 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 575: /* f_arg: f_arg_item  */
#line 4483 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11150 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 576: /* f_arg: f_arg ',' f_arg_item  */
#line 4487 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11158 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 577: /* f_opt_asgn: "local variable or method" '='  */
#line 4493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 11168 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 578: /* f_opt: f_opt_asgn arg  */
#line 4501 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(sym_to_node((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 579: /* f_block_opt: f_opt_asgn primary_value  */
#line 4509 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(sym_to_node((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 11188 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 580: /* f_block_optarg: f_block_opt  */
#line 4517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 581: /* f_block_optarg: f_block_optarg ',' f_block_opt  */
#line 4521 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 582: /* f_optarg: f_opt  */
#line 4527 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11212 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 583: /* f_optarg: f_optarg ',' f_opt  */
#line 4531 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11220 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 586: /* f_rest_arg: restarg_mark "local variable or method"  */
#line 4541 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11229 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 587: /* f_rest_arg: restarg_mark  */
#line 4546 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(mul);
                      local_add_f(p, (yyval.id));
                    }
#line 11238 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 590: /* f_block_arg: blkarg_mark "local variable or method"  */
#line 4557 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11246 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 591: /* f_block_arg: blkarg_mark  */
#line 4561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(and);
                    }
#line 11254 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 592: /* opt_f_block_arg: ',' f_block_arg  */
#line 4567 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 11262 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 593: /* opt_f_block_arg: none  */
#line 4571 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 11270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 594: /* singleton: var_ref  */
#line 4577 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 11280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 595: /* $@33: %empty  */
#line 4582 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 11286 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 596: /* singleton: '(' $@33 expr rparen  */
#line 4583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[-1].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11295 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 598: /* assoc_list: assocs trailer  */
#line 4591 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 11303 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 599: /* assocs: assoc  */
#line 4597 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 11311 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 600: /* assocs: assocs comma assoc  */
#line 4601 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11319 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 601: /* assoc: arg "=>" arg  */
#line 4607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 11329 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 602: /* assoc: "local variable or method" "label" arg  */
#line 4613 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 11338 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 603: /* assoc: "local variable or method" "label"  */
#line 4618 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), label_reference(p, (yyvsp[-1].id)));
                    }
#line 11346 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 604: /* assoc: "numbered parameter" "label"  */
#line 4622 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[-1].num));
                      (yyval.nd) = cons(new_sym(p, sym), label_reference(p, sym));
                    }
#line 11355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 605: /* assoc: "numbered parameter" "label" arg  */
#line 4627 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, intern_numparam((yyvsp[-2].num))), (yyvsp[0].nd));
                    }
#line 11364 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 606: /* assoc: string_fragment "label" arg  */
#line 4632 "mrbgems/mruby-compiler/core/parse.y"
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
#line 11384 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 607: /* assoc: "**" arg  */
#line 4648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 11393 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 608: /* assoc: "**"  */
#line 4653 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), new_lvar(p, intern_op(pow)));
                    }
#line 11401 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 621: /* call_op: '.'  */
#line 4679 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 11409 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 622: /* call_op: "&."  */
#line 4683 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 11417 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 624: /* call_op2: "::"  */
#line 4690 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 11425 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 633: /* term: ';'  */
#line 4711 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 11431 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 635: /* nl: '\n'  */
#line 4716 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 11440 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 639: /* none: %empty  */
#line 4728 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 11448 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 11452 "mrbgems/mruby-compiler/core/y.tab.c"

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

#line 4732 "mrbgems/mruby-compiler/core/parse.y"

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
