/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
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
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

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

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

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
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/error.h>
#include <mruby/throw.h>
#include <mruby/string.h>
#include <mruby/dump.h>
#include "node.h"

#define YYLEX_PARAM p

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;
typedef struct mrb_parser_heredoc_info parser_heredoc_info;

static int yyparse(parser_state *p);
static int yylex(void *lval, parser_state *p);
static void yyerror(parser_state *p, const char *s);
static void yywarn(parser_state *p, const char *s);
static void yywarning(parser_state *p, const char *s);
static void backref_error(parser_state *p, node *n);
static void void_expr_error(parser_state *p, node *n);
static void tokadd(parser_state *p, int32_t c);

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

#define SET_LINENO(c,n) ((c)->lineno = (n))
#define NODE_LINENO(c,n) do {\
  if (n) {\
     (c)->filename_index = (n)->filename_index;\
     (c)->lineno = (n)->lineno;\
  }\
} while (0)

#define sym(x) ((mrb_sym)(intptr_t)(x))
#define nsym(x) ((node*)(intptr_t)(x))
#define nint(x) ((node*)(intptr_t)(x))
#define intn(x) ((int)(intptr_t)(x))
#define typen(x) ((enum node_type)(intptr_t)(x))

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
  void *m = mrb_pool_alloc(p->pool, size);

  if (!m) {
    MRB_THROW(p->jmp);
  }
  return m;
}

static node*
cons_gen(parser_state *p, node *car, node *cdr)
{
  node *c;

  if (p->cells) {
    c = p->cells;
    p->cells = p->cells->cdr;
  }
  else {
    c = (node *)parser_palloc(p, sizeof(mrb_ast_node));
  }

  c->car = car;
  c->cdr = cdr;
  c->lineno = p->lineno;
  c->filename_index = p->current_filename_index;
  /* beginning of next partial file; need to point the previous file */
  if (p->lineno == 0 && p->current_filename_index > 0) {
    c->filename_index-- ;
  }
  return c;
}
#define cons(a,b) cons_gen(p,(a),(b))

static node*
list1_gen(parser_state *p, node *a)
{
  return cons(a, 0);
}
#define list1(a) list1_gen(p, (a))

static node*
list2_gen(parser_state *p, node *a, node *b)
{
  return cons(a, cons(b,0));
}
#define list2(a,b) list2_gen(p, (a),(b))

static node*
list3_gen(parser_state *p, node *a, node *b, node *c)
{
  return cons(a, cons(b, cons(c,0)));
}
#define list3(a,b,c) list3_gen(p, (a),(b),(c))

static node*
list4_gen(parser_state *p, node *a, node *b, node *c, node *d)
{
  return cons(a, cons(b, cons(c, cons(d, 0))));
}
#define list4(a,b,c,d) list4_gen(p, (a),(b),(c),(d))

static node*
list5_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, 0)))));
}
#define list5(a,b,c,d,e) list5_gen(p, (a),(b),(c),(d),(e))

static node*
list6_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e, node *f)
{
  return cons(a, cons(b, cons(c, cons(d, cons(e, cons(f, 0))))));
}
#define list6(a,b,c,d,e,f) list6_gen(p, (a),(b),(c),(d),(e),(f))

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
  char *b = (char *)parser_palloc(p, len+1);

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
      if (sym(n->car) == sym) return TRUE;
      n = n->cdr;
    }
    l = l->cdr;
  }

  u = p->upper;
  while (u && !MRB_PROC_CFUNC_P(u)) {
    const struct mrb_irep *ir = u->body.irep;
    const mrb_sym *v = ir->lv;
    int i;

    if (!v) break;
    for (i=0; i+1 < ir->nlocals; i++) {
      if (v[i] == sym) return TRUE;
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
    p->locals->car = push(p->locals->car, nsym(sym));
  }
}

static void
local_add(parser_state *p, mrb_sym sym)
{
  if (!local_var_p(p, sym)) {
    local_add_f(p, sym);
  }
}

static void
local_add_blk(parser_state *p, mrb_sym blk)
{
  /* allocate register for block */
  local_add_f(p, blk ? blk : intern_op(and));
}

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

static void
nvars_nest(parser_state *p)
{
  p->nvars = cons(nint(0), p->nvars);
}

static void
nvars_block(parser_state *p)
{
  p->nvars = cons(nint(-2), p->nvars);
}

static void
nvars_unnest(parser_state *p)
{
  p->nvars = p->nvars->cdr;
}

/* (:scope (vars..) (prog...)) */
static node*
new_scope(parser_state *p, node *body)
{
  return cons((node*)NODE_SCOPE, cons(locals_node(p), body));
}

/* (:begin prog...) */
static node*
new_begin(parser_state *p, node *body)
{
  if (body) {
    return list2((node*)NODE_BEGIN, body);
  }
  return cons((node*)NODE_BEGIN, 0);
}

#define newline_node(n) (n)

/* (:rescue body rescue else) */
static node*
new_rescue(parser_state *p, node *body, node *resq, node *els)
{
  return list4((node*)NODE_RESCUE, body, resq, els);
}

static node*
new_mod_rescue(parser_state *p, node *body, node *resq)
{
  return new_rescue(p, body, list1(list3(0, 0, resq)), 0);
}

/* (:ensure body ensure) */
static node*
new_ensure(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_ENSURE, cons(a, cons(0, b)));
}

/* (:nil) */
static node*
new_nil(parser_state *p)
{
  return list1((node*)NODE_NIL);
}

/* (:true) */
static node*
new_true(parser_state *p)
{
  return list1((node*)NODE_TRUE);
}

/* (:false) */
static node*
new_false(parser_state *p)
{
  return list1((node*)NODE_FALSE);
}

/* (:alias new old) */
static node*
new_alias(parser_state *p, mrb_sym a, mrb_sym b)
{
  return cons((node*)NODE_ALIAS, cons(nsym(a), nsym(b)));
}

/* (:if cond then else) */
static node*
new_if(parser_state *p, node *a, node *b, node *c)
{
  void_expr_error(p, a);
  return list4((node*)NODE_IF, a, b, c);
}

/* (:unless cond then else) */
static node*
new_unless(parser_state *p, node *a, node *b, node *c)
{
  void_expr_error(p, a);
  return list4((node*)NODE_IF, a, c, b);
}

/* (:while cond body) */
static node*
new_while(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
  return cons((node*)NODE_WHILE, cons(a, b));
}

/* (:until cond body) */
static node*
new_until(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
  return cons((node*)NODE_UNTIL, cons(a, b));
}

/* (:for var obj body) */
static node*
new_for(parser_state *p, node *v, node *o, node *b)
{
  void_expr_error(p, o);
  return list4((node*)NODE_FOR, v, o, b);
}

/* (:case a ((when ...) body) ((when...) body)) */
static node*
new_case(parser_state *p, node *a, node *b)
{
  node *n = list2((node*)NODE_CASE, a);
  node *n2 = n;

  void_expr_error(p, a);
  while (n2->cdr) {
    n2 = n2->cdr;
  }
  n2->cdr = b;
  return n;
}

/* (:postexe a) */
static node*
new_postexe(parser_state *p, node *a)
{
  return cons((node*)NODE_POSTEXE, a);
}

/* (:self) */
static node*
new_self(parser_state *p)
{
  return list1((node*)NODE_SELF);
}

/* (:call a b c) */
static node*
new_call(parser_state *p, node *a, mrb_sym b, node *c, int pass)
{
  node *n = list4(nint(pass?NODE_CALL:NODE_SCALL), a, nsym(b), c);
  void_expr_error(p, a);
  NODE_LINENO(n, a);
  return n;
}

/* (:fcall self mid args) */
static node*
new_fcall(parser_state *p, mrb_sym b, node *c)
{
  node *n = new_self(p);
  NODE_LINENO(n, c);
  n = list4((node*)NODE_FCALL, n, nsym(b), c);
  NODE_LINENO(n, c);
  return n;
}

/* (:super . c) */
static node*
new_super(parser_state *p, node *c)
{
  return cons((node*)NODE_SUPER, c);
}

/* (:zsuper) */
static node*
new_zsuper(parser_state *p)
{
  return list1((node*)NODE_ZSUPER);
}

/* (:yield . c) */
static node*
new_yield(parser_state *p, node *c)
{
  if (c) {
    if (c->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    return cons((node*)NODE_YIELD, c->car);
  }
  return cons((node*)NODE_YIELD, 0);
}

/* (:return . c) */
static node*
new_return(parser_state *p, node *c)
{
  return cons((node*)NODE_RETURN, c);
}

/* (:break . c) */
static node*
new_break(parser_state *p, node *c)
{
  return cons((node*)NODE_BREAK, c);
}

/* (:next . c) */
static node*
new_next(parser_state *p, node *c)
{
  return cons((node*)NODE_NEXT, c);
}

/* (:redo) */
static node*
new_redo(parser_state *p)
{
  return list1((node*)NODE_REDO);
}

/* (:retry) */
static node*
new_retry(parser_state *p)
{
  return list1((node*)NODE_RETRY);
}

/* (:dot2 a b) */
static node*
new_dot2(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT2, cons(a, b));
}

/* (:dot3 a b) */
static node*
new_dot3(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT3, cons(a, b));
}

/* (:colon2 b c) */
static node*
new_colon2(parser_state *p, node *b, mrb_sym c)
{
  void_expr_error(p, b);
  return cons((node*)NODE_COLON2, cons(b, nsym(c)));
}

/* (:colon3 . c) */
static node*
new_colon3(parser_state *p, mrb_sym c)
{
  return cons((node*)NODE_COLON3, nsym(c));
}

/* (:and a b) */
static node*
new_and(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_AND, cons(a, b));
}

/* (:or a b) */
static node*
new_or(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_OR, cons(a, b));
}

/* (:array a...) */
static node*
new_array(parser_state *p, node *a)
{
  return cons((node*)NODE_ARRAY, a);
}

/* (:splat . a) */
static node*
new_splat(parser_state *p, node *a)
{
  return cons((node*)NODE_SPLAT, a);
}

/* (:hash (k . v) (k . v)...) */
static node*
new_hash(parser_state *p, node *a)
{
  return cons((node*)NODE_HASH, a);
}

/* (:kw_hash (k . v) (k . v)...) */
static node*
new_kw_hash(parser_state *p, node *a)
{
  return cons((node*)NODE_KW_HASH, a);
}

/* (:sym . a) */
static node*
new_sym(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_SYM, nsym(sym));
}

static mrb_sym
new_strsym(parser_state *p, node* str)
{
  const char *s = (const char*)str->cdr->car;
  size_t len = (size_t)str->cdr->cdr;

  return mrb_intern(p->mrb, s, len);
}

/* (:lvar . a) */
static node*
new_lvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_LVAR, nsym(sym));
}

/* (:gvar . a) */
static node*
new_gvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_GVAR, nsym(sym));
}

/* (:ivar . a) */
static node*
new_ivar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_IVAR, nsym(sym));
}

/* (:cvar . a) */
static node*
new_cvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CVAR, nsym(sym));
}

/* (:nvar . a) */
static node*
new_nvar(parser_state *p, int num)
{
  int nvars = intn(p->nvars->car);

  p->nvars->car = nint(nvars > num ? nvars : num);
  return cons((node*)NODE_NVAR, nint(num));
}

/* (:const . a) */
static node*
new_const(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CONST, nsym(sym));
}

/* (:undef a...) */
static node*
new_undef(parser_state *p, mrb_sym sym)
{
  return list2((node*)NODE_UNDEF, nsym(sym));
}

/* (:class class super body) */
static node*
new_class(parser_state *p, node *c, node *s, node *b)
{
  void_expr_error(p, s);
  return list4((node*)NODE_CLASS, c, s, cons(locals_node(p), b));
}

/* (:sclass obj body) */
static node*
new_sclass(parser_state *p, node *o, node *b)
{
  void_expr_error(p, o);
  return list3((node*)NODE_SCLASS, o, cons(locals_node(p), b));
}

/* (:module module body) */
static node*
new_module(parser_state *p, node *m, node *b)
{
  return list3((node*)NODE_MODULE, m, cons(locals_node(p), b));
}

/* (:def m lv (arg . body)) */
static node*
new_def(parser_state *p, mrb_sym m, node *a, node *b)
{
  return list5((node*)NODE_DEF, nsym(m), 0, a, b);
}

static void
defn_setup(parser_state *p, node *d, node *a, node *b)
{
  node *n = d->cdr->cdr;

  n->car = locals_node(p);
  p->cmdarg_stack = intn(n->cdr->car);
  n->cdr->car = a;
  local_resume(p, n->cdr->cdr->car);
  n->cdr->cdr->car = b;
}

/* (:sdef obj m lv (arg . body)) */
static node*
new_sdef(parser_state *p, node *o, mrb_sym m, node *a, node *b)
{
  void_expr_error(p, o);
  return list6((node*)NODE_SDEF, o, nsym(m), 0, a, b);
}

static void
defs_setup(parser_state *p, node *d, node *a, node *b)
{
  node *n = d->cdr->cdr->cdr;

  n->car = locals_node(p);
  p->cmdarg_stack = intn(n->cdr->car);
  n->cdr->car = a;
  local_resume(p, n->cdr->cdr->car);
  n->cdr->cdr->car = b;
}

/* (:arg . sym) */
static node*
new_arg(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_ARG, nsym(sym));
}

static void
local_add_margs(parser_state *p, node *n)
{
  while (n) {
    if (typen(n->car->car) == NODE_MASGN) {
      node *t = n->car->cdr->cdr;

      n->car->cdr->cdr = NULL;
      while (t) {
        local_add_f(p, sym(t->car));
        t = t->cdr;
      }
      local_add_margs(p, n->car->cdr->car->car);
      local_add_margs(p, n->car->cdr->car->cdr->cdr->car);
    }
    n = n->cdr;
  }
}

static void
local_add_lv(parser_state *p, node *lv)
{
  while (lv) {
    local_add_f(p, sym(lv->car));
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
  node *n;

  local_add_margs(p, m);
  local_add_margs(p, m2);
  n = cons(m2, tail);
  n = cons(nsym(rest), n);
  n = cons(opt, n);
  while (opt) {
    /* opt: (sym . (opt . lv)) -> (sym . opt) */
    local_add_lv(p, opt->car->cdr->cdr);
    opt->car->cdr = opt->car->cdr->car;
    opt = opt->cdr;
  }
  return cons(m, n);
}

/* (:args_tail keywords rest_keywords_sym block_sym) */
static node*
new_args_tail(parser_state *p, node *kws, node *kwrest, mrb_sym blk)
{
  node *k;

  if (kws || kwrest) {
    local_add_kw(p, (kwrest && kwrest->cdr)? sym(kwrest->cdr) : 0);
  }

  local_add_blk(p, blk);

  /* allocate register for keywords arguments */
  /* order is for Proc#parameters */
  for (k = kws; k; k = k->cdr) {
    if (!k->car->cdr->cdr->car) { /* allocate required keywords */
      local_add_f(p, sym(k->car->cdr->car));
    }
  }
  for (k = kws; k; k = k->cdr) {
    if (k->car->cdr->cdr->car) { /* allocate keywords with default */
      local_add_lv(p, k->car->cdr->cdr->car->cdr);
      k->car->cdr->cdr->car = k->car->cdr->cdr->car->car;
      local_add_f(p, sym(k->car->cdr->car));
    }
  }

  return list4((node*)NODE_ARGS_TAIL, kws, kwrest, nsym(blk));
}

/* (:kw_arg kw_sym def_arg) */
static node*
new_kw_arg(parser_state *p, mrb_sym kw, node *def_arg)
{
  mrb_assert(kw);
  return list3((node*)NODE_KW_ARG, nsym(kw), def_arg);
}

/* (:kw_rest_args . a) */
static node*
new_kw_rest_args(parser_state *p, node *a)
{
  return cons((node*)NODE_KW_REST_ARGS, a);
}

/* (:block_arg . a) */
static node*
new_block_arg(parser_state *p, node *a)
{
  return cons((node*)NODE_BLOCK_ARG, a);
}

static node*
setup_numparams(parser_state *p, node *a)
{
  int nvars = intn(p->nvars->car);
  if (nvars > 0) {
    int i;
    mrb_sym sym;
    // m || opt || rest || tail
    if (a && (a->car || (a->cdr && a->cdr->car) || (a->cdr->cdr && a->cdr->cdr->car) || (a->cdr->cdr->cdr->cdr && a->cdr->cdr->cdr->cdr->car))) {
      yyerror(p, "ordinary parameter is defined");
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
        args = cons(new_arg(p, sym), args);
        p->locals->car = cons(nsym(sym), p->locals->car);
      }
      a = new_args(p, args, 0, 0, 0, 0);
    }
  }
  return a;
}

/* (:block arg body) */
static node*
new_block(parser_state *p, node *a, node *b)
{
  a = setup_numparams(p, a);
  return list4((node*)NODE_BLOCK, locals_node(p), a, b);
}

/* (:lambda arg body) */
static node*
new_lambda(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_LAMBDA, locals_node(p), a, b);
}

/* (:asgn lhs rhs) */
static node*
new_asgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return cons((node*)NODE_ASGN, cons(a, b));
}

/* (:masgn mlhs=(pre rest post)  mrhs) */
static node*
new_masgn(parser_state *p, node *a, node *b)
{
  void_expr_error(p, b);
  return cons((node*)NODE_MASGN, cons(a, b));
}

/* (:masgn mlhs mrhs) no check */
static node*
new_masgn_param(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_MASGN, cons(a, b));
}

/* (:asgn lhs rhs) */
static node*
new_op_asgn(parser_state *p, node *a, mrb_sym op, node *b)
{
  void_expr_error(p, b);
  return list4((node*)NODE_OP_ASGN, a, nsym(op), b);
}

static node*
new_imaginary(parser_state *p, node *imaginary)
{
  return new_call(p, new_const(p, MRB_SYM_2(p->mrb, Kernel)), MRB_SYM_2(p->mrb, Complex), list1(list2(list3((node*)NODE_INT, (node*)strdup("0"), nint(10)), imaginary)), 1);
}

static node*
new_rational(parser_state *p, node *rational)
{
  return new_call(p, new_const(p, MRB_SYM_2(p->mrb, Kernel)), MRB_SYM_2(p->mrb, Rational), list1(list1(rational)), 1);
}

/* (:int . i) */
static node*
new_int(parser_state *p, const char *s, int base, int suffix)
{
  node* result = list3((node*)NODE_INT, (node*)strdup(s), nint(base));
  if (suffix & NUM_SUFFIX_R) {
    result = new_rational(p, result);
  }
  if (suffix & NUM_SUFFIX_I) {
    result = new_imaginary(p, result);
  }
  return result;
}

#ifndef MRB_NO_FLOAT
/* (:float . i) */
static node*
new_float(parser_state *p, const char *s, int suffix)
{
  node* result = cons((node*)NODE_FLOAT, (node*)strdup(s));
  if (suffix & NUM_SUFFIX_R) {
    result = new_rational(p, result);
  }
  if (suffix & NUM_SUFFIX_I) {
    result = new_imaginary(p, result);
  }
  return result;
}
#endif

/* (:str . (s . len)) */
static node*
new_str(parser_state *p, const char *s, size_t len)
{
  return cons((node*)NODE_STR, cons((node*)strndup(s, len), nint(len)));
}

/* (:dstr . a) */
static node*
new_dstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DSTR, a);
}

static int
string_node_p(node *n)
{
  return (int)(typen(n->car) == NODE_STR);
}

static node*
composite_string_node(parser_state *p, node *a, node *b)
{
  size_t newlen = (size_t)a->cdr + (size_t)b->cdr;
  char *str = (char*)mrb_pool_realloc(p->pool, a->car, (size_t)a->cdr + 1, newlen + 1);
  memcpy(str + (size_t)a->cdr, b->car, (size_t)b->cdr);
  str[newlen] = '\0';
  a->car = (node*)str;
  a->cdr = (node*)newlen;
  cons_free(b);
  return a;
}

static node*
concat_string(parser_state *p, node *a, node *b)
{
  if (string_node_p(a)) {
    if (string_node_p(b)) {
      /* a == NODE_STR && b == NODE_STR */
      composite_string_node(p, a->cdr, b->cdr);
      cons_free(b);
      return a;
    }
    else {
      /* a == NODE_STR && b == NODE_DSTR */

      if (string_node_p(b->cdr->car)) {
        /* a == NODE_STR && b->[NODE_STR, ...] */
        composite_string_node(p, a->cdr, b->cdr->car->cdr);
        cons_free(b->cdr->car);
        b->cdr->car = a;
        return b;
      }
    }
  }
  else {
    node *c; /* last node of a */
    for (c = a; c->cdr != NULL; c = c->cdr) ;

    if (string_node_p(b)) {
      /* a == NODE_DSTR && b == NODE_STR */
      if (string_node_p(c->car)) {
        /* a->[..., NODE_STR] && b == NODE_STR */
        composite_string_node(p, c->car->cdr, b->cdr);
        cons_free(b);
        return a;
      }

      push(a, b);
      return a;
    }
    else {
      /* a == NODE_DSTR && b == NODE_DSTR */
      if (string_node_p(c->car) && string_node_p(b->cdr->car)) {
        /* a->[..., NODE_STR] && b->[NODE_STR, ...] */
        node *d = b->cdr;
        cons_free(b);
        composite_string_node(p, c->car->cdr, d->car->cdr);
        cons_free(d->car);
        c->cdr = d->cdr;
        cons_free(d);
        return a;
      }
      else {
        c->cdr = b->cdr;
        cons_free(b);
        return a;
      }
    }
  }

  return new_dstr(p, list2(a, b));
}

/* (:str . (s . len)) */
static node*
new_xstr(parser_state *p, const char *s, int len)
{
  return cons((node*)NODE_XSTR, cons((node*)strndup(s, len), nint(len)));
}

/* (:xstr . a) */
static node*
new_dxstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DXSTR, a);
}

/* (:dsym . a) */
static node*
new_dsym(parser_state *p, node *a)
{
  return cons((node*)NODE_DSYM, a);
}

/* (:regx . (s . (opt . enc))) */
static node*
new_regx(parser_state *p, const char *p1, const char* p2, const char* p3)
{
  return cons((node*)NODE_REGX, cons((node*)p1, cons((node*)p2, (node*)p3)));
}

/* (:dregx . (a . b)) */
static node*
new_dregx(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DREGX, cons(a, b));
}

/* (:backref . n) */
static node*
new_back_ref(parser_state *p, int n)
{
  return cons((node*)NODE_BACK_REF, nint(n));
}

/* (:nthref . n) */
static node*
new_nth_ref(parser_state *p, int n)
{
  return cons((node*)NODE_NTH_REF, nint(n));
}

/* (:heredoc . a) */
static node*
new_heredoc(parser_state *p)
{
  parser_heredoc_info *inf = (parser_heredoc_info *)parser_palloc(p, sizeof(parser_heredoc_info));
  return cons((node*)NODE_HEREDOC, (node*)inf);
}

static void
new_bv(parser_state *p, mrb_sym id)
{
}

static node*
new_literal_delim(parser_state *p)
{
  return cons((node*)NODE_LITERAL_DELIM, 0);
}

/* (:words . a) */
static node*
new_words(parser_state *p, node *a)
{
  return cons((node*)NODE_WORDS, a);
}

/* (:symbols . a) */
static node*
new_symbols(parser_state *p, node *a)
{
  return cons((node*)NODE_SYMBOLS, a);
}

/* xxx ----------------------------- */

/* (:call a op) */
static node*
call_uni_op(parser_state *p, node *recv, const char *m)
{
  void_expr_error(p, recv);
  return new_call(p, recv, intern_cstr(m), 0, 1);
}

/* (:call a op b) */
static node*
call_bin_op(parser_state *p, node *recv, const char *m, node *arg1)
{
  return new_call(p, recv, intern_cstr(m), list1(list1(arg1)), 1);
}

static void
args_with_block(parser_state *p, node *a, node *b)
{
  if (b) {
    if (a->cdr) {
      yyerror(p, "both block arg and actual block given");
    }
    a->cdr = b;
  }
}

static void
call_with_block(parser_state *p, node *a, node *b)
{
  node *n;

  switch (typen(a->car)) {
  case NODE_SUPER:
  case NODE_ZSUPER:
    if (!a->cdr) a->cdr = cons(0, b);
    else {
      args_with_block(p, a->cdr, b);
    }
    break;
  case NODE_CALL:
  case NODE_FCALL:
  case NODE_SCALL:
    n = a->cdr->cdr->cdr;
    if (!n->car) n->car = cons(0, b);
    else {
      args_with_block(p, n->car, b);
    }
    break;
  default:
    break;
  }
}

static node*
negate_lit(parser_state *p, node *n)
{
  return cons((node*)NODE_NEGATE, n);
}

static node*
cond(node *n)
{
  return n;
}

static node*
ret_args(parser_state *p, node *n)
{
  if (n->cdr) {
    yyerror(p, "block argument should not be given");
    return NULL;
  }
  if (!n->car->cdr) return n->car->car;
  return new_array(p, n->car);
}

static void
assignable(parser_state *p, node *lhs)
{
  if (intn(lhs->car) == NODE_LVAR) {
    local_add(p, sym(lhs->cdr));
  }
}

static node*
var_reference(parser_state *p, node *lhs)
{
  node *n;

  if (intn(lhs->car) == NODE_LVAR) {
    if (!local_var_p(p, sym(lhs->cdr))) {
      n = new_fcall(p, sym(lhs->cdr), 0);
      cons_free(lhs);
      return n;
    }
  }

  return lhs;
}

typedef enum mrb_string_type  string_type;

static node*
new_strterm(parser_state *p, string_type type, int term, int paren)
{
  return cons(nint(type), cons(nint(0), cons(nint(paren), nint(term))));
}

static void
end_strterm(parser_state *p)
{
  cons_free(p->lex_strterm->cdr->cdr);
  cons_free(p->lex_strterm->cdr);
  cons_free(p->lex_strterm);
  p->lex_strterm = NULL;
}

static parser_heredoc_info *
parsing_heredoc_inf(parser_state *p)
{
  node *nd = p->parsing_heredoc;
  if (nd == NULL)
    return NULL;
  /* mrb_assert(nd->car->car == NODE_HEREDOC); */
  return (parser_heredoc_info*)nd->car->cdr;
}

static void
heredoc_treat_nextline(parser_state *p)
{
  if (p->heredocs_from_nextline == NULL)
    return;
  if (p->parsing_heredoc == NULL) {
    node *n;
    p->parsing_heredoc = p->heredocs_from_nextline;
    p->lex_strterm_before_heredoc = p->lex_strterm;
    p->lex_strterm = new_strterm(p, parsing_heredoc_inf(p)->type, 0, 0);
    n = p->all_heredocs;
    if (n) {
      while (n->cdr)
        n = n->cdr;
      n->cdr = p->parsing_heredoc;
    }
    else {
      p->all_heredocs = p->parsing_heredoc;
    }
  }
  else {
    node *n, *m;
    m = p->heredocs_from_nextline;
    while (m->cdr)
      m = m->cdr;
    n = p->all_heredocs;
    mrb_assert(n != NULL);
    if (n == p->parsing_heredoc) {
      m->cdr = n;
      p->all_heredocs = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
    else {
      while (n->cdr != p->parsing_heredoc) {
        n = n->cdr;
        mrb_assert(n != NULL);
      }
      m->cdr = n->cdr;
      n->cdr = p->heredocs_from_nextline;
      p->parsing_heredoc = p->heredocs_from_nextline;
    }
  }
  p->heredocs_from_nextline = NULL;
}

static void
heredoc_end(parser_state *p)
{
  p->parsing_heredoc = p->parsing_heredoc->cdr;
  if (p->parsing_heredoc == NULL) {
    p->lstate = EXPR_BEG;
    end_strterm(p);
    p->lex_strterm = p->lex_strterm_before_heredoc;
    p->lex_strterm_before_heredoc = NULL;
  }
  else {
    /* next heredoc */
    p->lex_strterm->car = nint(parsing_heredoc_inf(p)->type);
  }
}
#define is_strterm_type(p,str_func) (intn((p)->lex_strterm->car) & (str_func))

/* xxx ----------------------------- */


#line 1436 "mrbgems/mruby-compiler/core/y.tab.c"

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

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif


/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    keyword_class = 258,
    keyword_module = 259,
    keyword_def = 260,
    keyword_begin = 261,
    keyword_if = 262,
    keyword_unless = 263,
    keyword_while = 264,
    keyword_until = 265,
    keyword_for = 266,
    keyword_undef = 267,
    keyword_rescue = 268,
    keyword_ensure = 269,
    keyword_end = 270,
    keyword_then = 271,
    keyword_elsif = 272,
    keyword_else = 273,
    keyword_case = 274,
    keyword_when = 275,
    keyword_break = 276,
    keyword_next = 277,
    keyword_redo = 278,
    keyword_retry = 279,
    keyword_in = 280,
    keyword_do = 281,
    keyword_do_cond = 282,
    keyword_do_block = 283,
    keyword_do_LAMBDA = 284,
    keyword_return = 285,
    keyword_yield = 286,
    keyword_super = 287,
    keyword_self = 288,
    keyword_nil = 289,
    keyword_true = 290,
    keyword_false = 291,
    keyword_and = 292,
    keyword_or = 293,
    keyword_not = 294,
    modifier_if = 295,
    modifier_unless = 296,
    modifier_while = 297,
    modifier_until = 298,
    modifier_rescue = 299,
    keyword_alias = 300,
    keyword_BEGIN = 301,
    keyword_END = 302,
    keyword__LINE__ = 303,
    keyword__FILE__ = 304,
    keyword__ENCODING__ = 305,
    tIDENTIFIER = 306,
    tFID = 307,
    tGVAR = 308,
    tIVAR = 309,
    tCONSTANT = 310,
    tCVAR = 311,
    tLABEL_TAG = 312,
    tINTEGER = 313,
    tFLOAT = 314,
    tCHAR = 315,
    tXSTRING = 316,
    tREGEXP = 317,
    tSTRING = 318,
    tSTRING_PART = 319,
    tSTRING_MID = 320,
    tNTH_REF = 321,
    tBACK_REF = 322,
    tREGEXP_END = 323,
    tNUMPARAM = 324,
    tUPLUS = 325,
    tUMINUS = 326,
    tPOW = 327,
    tCMP = 328,
    tEQ = 329,
    tEQQ = 330,
    tNEQ = 331,
    tGEQ = 332,
    tLEQ = 333,
    tANDOP = 334,
    tOROP = 335,
    tMATCH = 336,
    tNMATCH = 337,
    tDOT2 = 338,
    tDOT3 = 339,
    tBDOT2 = 340,
    tBDOT3 = 341,
    tAREF = 342,
    tASET = 343,
    tLSHFT = 344,
    tRSHFT = 345,
    tCOLON2 = 346,
    tCOLON3 = 347,
    tOP_ASGN = 348,
    tASSOC = 349,
    tLPAREN = 350,
    tLPAREN_ARG = 351,
    tRPAREN = 352,
    tLBRACK = 353,
    tLBRACE = 354,
    tLBRACE_ARG = 355,
    tSTAR = 356,
    tDSTAR = 357,
    tAMPER = 358,
    tLAMBDA = 359,
    tANDDOT = 360,
    tSYMBEG = 361,
    tREGEXP_BEG = 362,
    tWORDS_BEG = 363,
    tSYMBOLS_BEG = 364,
    tSTRING_BEG = 365,
    tXSTRING_BEG = 366,
    tSTRING_DVAR = 367,
    tLAMBEG = 368,
    tHEREDOC_BEG = 369,
    tHEREDOC_END = 370,
    tLITERAL_DELIM = 371,
    tHD_LITERAL_DELIM = 372,
    tHD_STRING_PART = 373,
    tHD_STRING_MID = 374,
    tLOWEST = 375,
    tUMINUS_NUM = 376,
    tLAST_TOKEN = 377
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 1378 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1616 "mrbgems/mruby-compiler/core/y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int yyparse (parser_state *p);





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
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
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

#if ! defined yyoverflow || YYERROR_VERBOSE

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
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

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
#define YYLAST   12398

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  176
/* YYNRULES -- Number of rules.  */
#define YYNRULES  605
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1061

#define YYUNDEFTOK  2
#define YYMAXUTOK   377


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

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
       0,  1537,  1537,  1537,  1548,  1554,  1558,  1563,  1567,  1573,
    1575,  1574,  1588,  1615,  1621,  1625,  1630,  1634,  1640,  1640,
    1644,  1648,  1652,  1656,  1660,  1664,  1668,  1673,  1674,  1678,
    1682,  1686,  1690,  1697,  1700,  1704,  1708,  1712,  1716,  1720,
    1725,  1729,  1736,  1737,  1741,  1745,  1746,  1750,  1754,  1758,
    1762,  1766,  1776,  1775,  1790,  1799,  1800,  1803,  1804,  1811,
    1810,  1825,  1829,  1834,  1838,  1843,  1847,  1852,  1856,  1860,
    1864,  1868,  1874,  1878,  1884,  1885,  1891,  1895,  1899,  1903,
    1907,  1911,  1915,  1919,  1923,  1927,  1933,  1934,  1940,  1944,
    1950,  1954,  1960,  1964,  1968,  1972,  1976,  1980,  1986,  1992,
    1999,  2003,  2007,  2011,  2015,  2019,  2025,  2031,  2036,  2042,
    2046,  2049,  2053,  2057,  2064,  2065,  2066,  2067,  2072,  2079,
    2080,  2083,  2087,  2087,  2093,  2094,  2095,  2096,  2097,  2098,
    2099,  2100,  2101,  2102,  2103,  2104,  2105,  2106,  2107,  2108,
    2109,  2110,  2111,  2112,  2113,  2114,  2115,  2116,  2117,  2118,
    2119,  2120,  2121,  2122,  2125,  2125,  2125,  2126,  2126,  2127,
    2127,  2127,  2128,  2128,  2128,  2128,  2129,  2129,  2129,  2130,
    2130,  2130,  2131,  2131,  2131,  2131,  2132,  2132,  2132,  2132,
    2133,  2133,  2133,  2133,  2134,  2134,  2134,  2134,  2135,  2135,
    2135,  2135,  2136,  2136,  2139,  2143,  2147,  2151,  2155,  2159,
    2163,  2168,  2173,  2178,  2182,  2186,  2190,  2194,  2198,  2202,
    2206,  2210,  2214,  2218,  2222,  2226,  2230,  2234,  2238,  2242,
    2246,  2250,  2254,  2258,  2262,  2266,  2270,  2274,  2278,  2282,
    2286,  2290,  2294,  2298,  2302,  2306,  2310,  2314,  2318,  2322,
    2326,  2334,  2343,  2352,  2362,  2368,  2369,  2374,  2378,  2385,
    2389,  2397,  2401,  2417,  2443,  2444,  2447,  2448,  2449,  2454,
    2459,  2466,  2472,  2477,  2482,  2487,  2494,  2494,  2505,  2511,
    2515,  2521,  2522,  2525,  2531,  2537,  2542,  2549,  2554,  2559,
    2566,  2567,  2568,  2569,  2570,  2571,  2572,  2573,  2577,  2582,
    2581,  2593,  2597,  2592,  2602,  2602,  2606,  2610,  2614,  2618,
    2623,  2628,  2632,  2636,  2640,  2644,  2648,  2649,  2655,  2661,
    2654,  2673,  2681,  2689,  2689,  2689,  2696,  2696,  2696,  2703,
    2709,  2714,  2716,  2713,  2725,  2723,  2741,  2746,  2739,  2763,
    2761,  2777,  2787,  2798,  2802,  2806,  2810,  2816,  2823,  2824,
    2825,  2828,  2829,  2832,  2833,  2841,  2842,  2848,  2852,  2855,
    2859,  2863,  2867,  2872,  2876,  2880,  2884,  2890,  2889,  2899,
    2903,  2907,  2911,  2917,  2922,  2927,  2931,  2935,  2939,  2943,
    2947,  2951,  2955,  2959,  2963,  2967,  2971,  2975,  2979,  2983,
    2989,  2994,  3001,  3001,  3005,  3010,  3017,  3021,  3027,  3028,
    3031,  3036,  3039,  3043,  3049,  3053,  3060,  3059,  3074,  3084,
    3088,  3093,  3100,  3104,  3108,  3112,  3116,  3120,  3124,  3128,
    3132,  3139,  3138,  3153,  3152,  3168,  3176,  3185,  3188,  3195,
    3198,  3202,  3203,  3206,  3210,  3213,  3217,  3220,  3221,  3222,
    3223,  3226,  3227,  3233,  3234,  3235,  3239,  3245,  3246,  3252,
    3257,  3256,  3267,  3271,  3277,  3281,  3287,  3291,  3297,  3300,
    3301,  3304,  3310,  3316,  3317,  3320,  3327,  3326,  3340,  3344,
    3351,  3356,  3363,  3369,  3370,  3371,  3372,  3373,  3377,  3383,
    3387,  3393,  3394,  3395,  3399,  3405,  3409,  3413,  3417,  3421,
    3427,  3431,  3437,  3441,  3445,  3449,  3453,  3457,  3465,  3472,
    3483,  3484,  3488,  3492,  3491,  3507,  3513,  3531,  3551,  3552,
    3558,  3564,  3570,  3577,  3582,  3589,  3593,  3599,  3603,  3609,
    3610,  3613,  3617,  3623,  3627,  3631,  3635,  3641,  3646,  3651,
    3655,  3659,  3663,  3667,  3671,  3675,  3679,  3683,  3687,  3691,
    3695,  3699,  3703,  3708,  3714,  3719,  3724,  3729,  3734,  3741,
    3745,  3752,  3757,  3756,  3768,  3772,  3778,  3786,  3794,  3802,
    3806,  3812,  3816,  3822,  3823,  3826,  3831,  3838,  3839,  3842,
    3848,  3852,  3858,  3863,  3863,  3888,  3889,  3895,  3900,  3906,
    3907,  3910,  3916,  3921,  3931,  3938,  3939,  3940,  3943,  3944,
    3945,  3946,  3949,  3950,  3951,  3954,  3955,  3958,  3962,  3968,
    3969,  3975,  3976,  3979,  3980,  3983,  3986,  3987,  3988,  3991,
    3992,  3993,  3996,  4003,  4004,  4008
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "keyword_class", "keyword_module",
  "keyword_def", "keyword_begin", "keyword_if", "keyword_unless",
  "keyword_while", "keyword_until", "keyword_for", "keyword_undef",
  "keyword_rescue", "keyword_ensure", "keyword_end", "keyword_then",
  "keyword_elsif", "keyword_else", "keyword_case", "keyword_when",
  "keyword_break", "keyword_next", "keyword_redo", "keyword_retry",
  "keyword_in", "keyword_do", "keyword_do_cond", "keyword_do_block",
  "keyword_do_LAMBDA", "keyword_return", "keyword_yield", "keyword_super",
  "keyword_self", "keyword_nil", "keyword_true", "keyword_false",
  "keyword_and", "keyword_or", "keyword_not", "modifier_if",
  "modifier_unless", "modifier_while", "modifier_until", "modifier_rescue",
  "keyword_alias", "keyword_BEGIN", "keyword_END", "keyword__LINE__",
  "keyword__FILE__", "keyword__ENCODING__", "tIDENTIFIER", "tFID", "tGVAR",
  "tIVAR", "tCONSTANT", "tCVAR", "tLABEL_TAG", "tINTEGER", "tFLOAT",
  "tCHAR", "tXSTRING", "tREGEXP", "tSTRING", "tSTRING_PART", "tSTRING_MID",
  "tNTH_REF", "tBACK_REF", "tREGEXP_END", "tNUMPARAM", "tUPLUS", "tUMINUS",
  "tPOW", "tCMP", "tEQ", "tEQQ", "tNEQ", "tGEQ", "tLEQ", "tANDOP", "tOROP",
  "tMATCH", "tNMATCH", "tDOT2", "tDOT3", "tBDOT2", "tBDOT3", "tAREF",
  "tASET", "tLSHFT", "tRSHFT", "tCOLON2", "tCOLON3", "tOP_ASGN", "tASSOC",
  "tLPAREN", "tLPAREN_ARG", "tRPAREN", "tLBRACK", "tLBRACE", "tLBRACE_ARG",
  "tSTAR", "tDSTAR", "tAMPER", "tLAMBDA", "tANDDOT", "tSYMBEG",
  "tREGEXP_BEG", "tWORDS_BEG", "tSYMBOLS_BEG", "tSTRING_BEG",
  "tXSTRING_BEG", "tSTRING_DVAR", "tLAMBEG", "tHEREDOC_BEG",
  "tHEREDOC_END", "tLITERAL_DELIM", "tHD_LITERAL_DELIM", "tHD_STRING_PART",
  "tHD_STRING_MID", "tLOWEST", "'='", "'?'", "':'", "'>'", "'<'", "'|'",
  "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'", "tUMINUS_NUM", "'!'",
  "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "']'", "','", "'`'", "'('",
  "')'", "';'", "'.'", "'\\n'", "$accept", "program", "$@1",
  "top_compstmt", "top_stmts", "top_stmt", "@2", "bodystmt", "compstmt",
  "stmts", "stmt", "$@3", "command_asgn", "command_rhs", "expr",
  "defn_head", "defs_head", "$@4", "expr_value", "command_call",
  "block_command", "cmd_brace_block", "$@5", "command", "mlhs",
  "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list", "mlhs_post",
  "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym", "undef_list",
  "$@6", "op", "reswords", "arg", "aref_args", "arg_rhs", "paren_args",
  "opt_paren_args", "opt_call_args", "call_args", "command_args", "@7",
  "block_arg", "opt_block_arg", "comma", "args", "mrhs", "primary", "@8",
  "@9", "$@10", "$@11", "@12", "@13", "$@14", "$@15", "$@16", "$@17",
  "$@18", "$@19", "@20", "@21", "@22", "@23", "primary_value", "then",
  "do", "if_tail", "opt_else", "for_var", "f_margs", "$@24",
  "block_args_tail", "opt_block_args_tail", "block_param",
  "opt_block_param", "block_param_def", "$@25", "opt_bv_decl", "bv_decls",
  "bvar", "f_larglist", "lambda_body", "do_block", "$@26", "block_call",
  "method_call", "brace_block", "@27", "@28", "case_body", "cases",
  "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal", "string",
  "string_fragment", "string_rep", "string_interp", "@29", "xstring",
  "regexp", "heredoc", "heredoc_bodies", "heredoc_body",
  "heredoc_string_rep", "heredoc_string_interp", "@30", "words", "symbol",
  "basic_symbol", "sym", "symbols", "numeric", "variable", "var_lhs",
  "var_ref", "backref", "superclass", "$@31", "f_arglist_paren",
  "f_arglist", "f_label", "f_kw", "f_block_kw", "f_block_kwarg", "f_kwarg",
  "kwrest_mark", "f_kwrest", "args_tail", "opt_args_tail", "f_args",
  "f_bad_arg", "f_norm_arg", "f_arg_item", "@32", "f_arg", "f_opt_asgn",
  "f_opt", "f_block_opt", "f_block_optarg", "f_optarg", "restarg_mark",
  "f_rest_arg", "blkarg_mark", "f_block_arg", "opt_f_block_arg",
  "singleton", "$@33", "assoc_list", "assocs", "label_tag", "assoc",
  "operation", "operation2", "operation3", "dot_or_colon", "call_op",
  "call_op2", "opt_terms", "opt_nl", "rparen", "trailer", "term", "nl",
  "terms", "none", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,    61,    63,    58,    62,    60,   124,    94,    38,    43,
      45,    42,    47,    37,   376,    33,   126,   377,   123,   125,
      91,    93,    44,    96,    40,    41,    59,    46,    10
};
# endif

#define YYPACT_NINF (-838)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-606)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -838,   121,  2722,  -838,  7590,  9714, 10056,  5898,  -838,  9360,
    9360,  -838,  -838,  9828,  7080,  5633,  7826,  7826,  -838,  -838,
    7826,  3378,  2970,  -838,  -838,  -838,  -838,   113,  7080,  -838,
      84,  -838,  -838,  -838,  6040,  2834,  -838,  -838,  6182,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,    52,  9478,  9478,  9478,
    9478,   123,  4892,  1100,  8298,  8652,  7362,  -838,  6798,   235,
    1030,  1163,  1204,    90,  -838,   370,  9596,  9478,  -838,  1086,
    -838,  1423,  -838,   610,  1382,  1382,  -838,  -838,   183,   125,
    -838,   150,  9942,  -838,   197, 12161,    77,   656,   234,   107,
    -838,   127,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,
    -838,    81,   230,  -838,   285,   106,  -838,  -838,  -838,  -838,
    -838,   236,   236,   245,   188,   296,  -838,  9360,   367,  5011,
     599,  1382,  1382,  -838,   267,  -838,   760,  -838,  -838,   106,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,    48,   168,
     172,   191,  -838,  -838,  -838,  -838,  -838,  -838,   272,   319,
     320,   329,  -838,   351,  -838,  -838,  -838,  -838,  -838,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,   358,
    4070,   349,   610,    73,   288, 12246,   766,    63,   322,   213,
      73,  9360,  9360,   770,   445,  -838,  -838,   829,   388,    83,
     124,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,
    6939,  -838,  -838,   371,  -838,  -838,  -838,  -838,  -838,  -838,
    1086,  -838,   212,  -838,   489,  -838,  -838,  1086,  3106,  9478,
    9478,  9478,  9478,  -838, 12184,  -838,  -838,   374,   466,   374,
    -838,  -838,  -838,  7944,  -838,  -838,  -838,  7826,  -838,  -838,
    -838,  5633,  9360,  -838,  -838,   391,  5130,  -838,   834,   467,
   12265, 12265,   474,  7708,  4892,   397,  1086,  1423,  1086,   433,
    -838,  7708,  1086,   418,  1334,  1334,  -838, 12184,   425,  1334,
    -838,   522, 10170,   458,   855,   881,   941,  1603,  -838,  -838,
    -838,  -838,  1244,  -838,  -838,  -838,  -838,  -838,  -838,   534,
     889,  -838,  -838,  1280,  -838,  1283,  -838,  1293,  -838,   821,
     531,   537,  -838,  -838,  -838,  -838,  5395,  9360,  9360,  9360,
    9360,  7708,  9360,  9360,    78,  -838,  -838,  -838,  -838,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  1656,   501,  4070,
    9478,  -838,   494,   612,   508,  -838,  1086,  -838,  -838,  -838,
     528,  9478,  -838,   532,   633,   547,   640,  -838,   545,  4070,
    -838,  -838,  8770,  -838,  4892,  7476,   564,  8770,  9478,  9478,
    9478,  9478,  9478,  9478,  9478,  9478,  9478,  9478,  9478,  9478,
    9478,  9478,  9478,   658,  9478,  9478,  9478,  9478,  9478,  9478,
    9478,  9478,  9478,  9478,  9478, 10448,  -838,  7826,  -838, 10534,
    -838,  -838, 11738,  -838,  -838,  -838,  -838,  9596,  9596,  -838,
     616,  -838,   610,  -838,   945,  -838,  -838,  -838,  -838,  -838,
    -838, 10620,  7826, 10706,  4070,  9360,  -838,  -838,  -838,   704,
     716,   295,  -838,  4216,   719,  9478, 10792,  7826, 10878,  9478,
    9478,  4508,   607,   607,   128, 10964,  7826, 11050,  -838,   676,
    -838,  5130,   489,  -838,  -838,  8888,   731,  -838,   534,  9478,
   12246, 12246, 12246,  9478,   776,  -838,  8062,  -838,  9478,  -838,
    8416,  5752,   603,  1086,   374,   374,  -838,  -838,   825,   606,
    -838,  -838,  7080,  4627,   625, 10792, 10878,  9478,  1423,  1086,
    -838,  -838,  5514,   623,  1423,  -838,  -838,  8534,  -838,  1086,
    8652,  -838,  -838,  -838,   945,   150, 10170,  -838, 10170, 11136,
    7826, 11222,  1948,  -838,  -838,  -838,  1306,  5130,   534,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  9478,  9478,  -838,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  1417,  1086,
    1086,   627,  9478,   755, 12246,   541,  -838,  -838,  -838,    44,
    -838,  -838,  1948,  -838, 12246,  1948,  -838,  -838,  1746,  -838,
    -838,  9478,   756,   109,  9478,  -838, 11981,   374,  -838,  1086,
   10170,   631,  -838,  -838,  -838,   730,   657,  2579,  -838,  -838,
     998,   302,   467, 10470, 10470, 10470, 10470,  1372,  1372, 10556,
    2464, 10470, 10470, 12265, 12265,   926,   926,  -838, 11919,  1372,
    1372,  1054,  1054,   570,   216,   216,   467,   467,   467,  3514,
    6538,  3786,  6656,  -838,   236,  -838,   639,   374,   481,  -838,
     485,  -838,  -838,  3242,  -838,  -838,  2460,   109,   109,  -838,
   11810,  -838,  -838,  -838,  -838,  -838,  1086,  9360,  4070,   446,
     193,  -838,   236,   641,   236,   775,   825,  7221,  -838,  9006,
     779,  -838,   706,  -838,  6300,  6419,   652,   303,   441,   779,
    -838,  -838,  -838,  -838,    19,    99,   661,   134,   135,  9360,
    7080,   666,   795, 12246,    94,  -838,   534, 12246, 12246,   534,
    9478, 12184,  -838,   374, 12246,  -838,  -838,  -838,  -838,  8180,
    8416,  -838,  -838,  -838,   679,  -838,  -838,   352,  1423,  1086,
    1334,   564,  -838,   446,   193,   678,   524,   684,   681,    88,
    -838,   687,  -838,   467,   467,  -838,  1066,  1086,   692,  -838,
    -838,  1724, 11831,  -838,   782,  -838,   508,  -838,  -838,  -838,
     699,   702,   703,  -838,   705,   782,   703, 11900,  -838,  -838,
    1948,  4070,  -838,  -838, 12000,  9124,  -838,  -838, 10170,  7708,
    9596,  9478, 11308,  7826, 11394,   105,  9596,  9596,  -838,   616,
     486,  8062,  9596,  9596,  -838,   616,   107,   183,  4070,  5130,
     109,  -838,  1086,   841,  -838,  -838,  -838,  -838, 11981,  -838,
     773,  -838,  4773,   835,  -838,  9360,   857,  -838,  9478,  9478,
     452,  9478,  9478,   858,  5276,  5276,   145,   607,  -838,  -838,
    -838,  9242,  4362,   534, 12246,  -838,  5752,   374,  -838,  -838,
    -838,   788,   725,   732,  4070,  5130,  -838,  -838,  -838,   735,
    -838,  1782,  1086,  9478,  -838,  1948,  -838,  1746,  -838,  1746,
    -838,  1746,  -838,  -838,  9478,  -838,   681,   681, 10284,  -838,
     741,   508,   745, 10284,  -838,   747,   748,  -838,   883,  9478,
   12071,  -838,  -838, 12246,  3650,  3922,   758,   505,   518,  9478,
    9478,  -838,  -838,  -838,  -838,  -838,  9596,  -838,  -838,  -838,
    -838,  -838,  -838,  -838,   887,   764,  5130,  4070,  -838,  -838,
   10398,    73,  -838,  -838,  5276,  -838,  -838,    73,  -838,  9478,
    -838,   890,   896,  -838, 12246,   161,  -838,  8416,  -838,  1570,
     906,   790,  1321,  1321,  1186,  -838, 12246,   703,   785,   703,
     703, 12246,   805,   806,   878,  1104,   541,  -838,  -838,  1221,
    -838,  1104,  1948,  -838,  1746,  -838,  -838, 12090,   519, 12246,
   12246,  -838,  -838,  -838,  -838,   797,   927,   886,  -838,  1109,
     881,   941,  4070,  -838,  4216,  -838,  -838,  5276,  -838,  -838,
    -838,  -838,   163,  -838,  -838,  -838,  -838,   803,   803,  1321,
     807,  -838,  1746,  -838,  -838,  -838,  -838,  -838,  -838, 11480,
    -838,   508,   541,  -838,  -838,   810,   814,   815,  -838,   817,
     815,  -838,  -838,   945, 11566,  7826, 11652,   716,   706,   932,
    1570,  -838,  1321,   803,  1321,   703,   804,   833,  -838,  1948,
    -838,  1746,  -838,  1746,  -838,  1746,  -838,  -838,   446,   193,
     842,    85,   453,  -838,  -838,  -838,  -838,   803,  -838,   815,
     845,   815,   815,   788,  -838,  1746,  -838,  -838,  -838,   815,
    -838
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   289,     0,
       0,   313,   316,     0,     0,   591,   333,   334,   335,   336,
     301,   266,   266,   484,   483,   485,   486,   593,     0,    10,
       0,   488,   487,   489,   475,   577,   477,   476,   479,   478,
     471,   472,   433,   434,   490,   491,   287,     0,     0,     0,
       0,     0,     0,   291,   605,   605,    84,   308,     0,     0,
       0,     0,     0,     0,   448,     0,     0,     0,     3,   591,
       6,     9,    27,    33,   533,   533,    45,    56,    55,     0,
      72,     0,    76,    86,     0,    50,   244,     0,    57,   306,
     280,   281,   431,   282,   283,   284,   429,   428,   460,   430,
     427,   482,     0,   285,   286,   266,     5,     8,   333,   334,
     301,   605,   409,     0,   109,   110,   287,     0,     0,     0,
       0,   533,   533,   112,   492,   337,     0,   482,   286,     0,
     329,   164,   174,   165,   161,   190,   191,   192,   193,   172,
     187,   180,   170,   169,   185,   168,   167,   163,   188,   162,
     175,   179,   181,   173,   166,   182,   189,   184,   183,   176,
     186,   171,   160,   178,   177,   159,   157,   158,   154,   155,
     156,   114,   116,   115,   149,   150,   145,   127,   128,   129,
     136,   133,   135,   130,   131,   151,   152,   137,   138,   142,
     146,   132,   134,   124,   125,   126,   139,   140,   141,   143,
     144,   147,   148,   153,   563,    51,   117,   118,   562,     0,
       0,     0,    54,     0,     0,    50,     0,   482,     0,   286,
       0,     0,     0,   108,     0,   348,   347,     0,     0,   482,
     286,   183,   176,   186,   171,   154,   155,   156,   114,   115,
       0,   119,   121,    20,   120,   451,   456,   455,   599,   602,
     591,   601,     0,   453,     0,   603,   600,   592,   575,     0,
       0,     0,     0,   261,   273,    70,   265,   605,   431,   605,
     567,    71,    69,   605,   255,   302,    68,     0,   254,   408,
      67,   591,     0,   594,    18,     0,     0,   217,     0,   218,
     205,   208,   298,     0,     0,     0,   591,    15,   591,    74,
      14,     0,   591,     0,   596,   596,   245,     0,     0,   596,
     565,     0,     0,    82,     0,    92,    99,   533,   465,   464,
     466,   467,     0,   463,   462,   446,   440,   439,   442,     0,
       0,   437,   458,     0,   469,     0,   435,     0,   444,     0,
     473,   474,    49,   232,   233,     4,   592,     0,     0,     0,
       0,     0,     0,     0,   540,   536,   535,   534,   537,   538,
     509,   542,   554,   510,   558,   557,   553,   533,   498,     0,
     502,   507,   605,   512,   605,   532,     0,   539,   541,   544,
     518,     0,   551,   518,   556,   518,     0,   516,   498,     0,
     396,   398,     0,    88,     0,    80,    77,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   204,
     207,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   588,   605,   587,     0,
     590,   589,     0,   413,   411,   307,   432,     0,     0,   402,
      61,   305,   326,   109,   110,   111,   473,   474,   498,   493,
     324,     0,   605,     0,     0,     0,   586,   585,    52,     0,
     605,   298,   339,     0,   338,     0,     0,   605,     0,     0,
       0,     0,     0,     0,   298,     0,   605,     0,   321,     0,
     122,     0,     0,   452,   454,     0,     0,   604,   569,     0,
     274,   574,   268,     0,   271,   262,     0,   270,     0,   263,
       0,   591,     0,   591,   605,   605,   256,   267,   591,     0,
     304,    48,     0,     0,     0,     0,     0,     0,    17,   591,
     296,    13,   592,    73,   292,   295,   299,   598,   246,   597,
     598,   248,   300,   566,    98,    90,     0,    85,     0,     0,
     605,     0,   533,   309,   393,   468,     0,     0,   443,   449,
     447,   438,   459,   470,   436,   445,     0,     0,     7,    21,
      22,    23,    24,    25,    46,    47,   500,   546,     0,   591,
     591,   518,     0,     0,   501,     0,   514,   561,   511,     0,
     515,   499,     0,   525,   547,     0,   528,   555,     0,   530,
     559,     0,     0,   605,     0,    28,    30,     0,    31,   591,
       0,    78,    89,    44,    34,    42,     0,   249,   194,    29,
       0,   286,   214,   222,   227,   228,   229,   224,   226,   236,
     237,   230,   231,   203,   206,   234,   235,    32,   593,   223,
     225,   219,   220,   221,   209,   210,   211,   212,   213,   578,
     583,   579,   584,   407,   266,   405,     0,   605,   578,   580,
     579,   581,   406,   266,   578,   579,   266,   605,   605,    35,
     249,   195,    41,   202,    59,    62,     0,     0,     0,   109,
     110,   113,     0,     0,   605,     0,   591,     0,   290,   605,
     605,   419,   605,   340,   582,   297,     0,   578,   579,   605,
     342,   314,   341,   317,   582,   297,     0,   578,   579,     0,
       0,     0,     0,   273,     0,   320,   570,   572,   571,     0,
       0,   275,   269,   605,   573,   568,   253,   251,   257,   258,
     260,   303,   595,    19,     0,    26,   201,    75,    16,   591,
     596,    91,    83,    95,    97,     0,    94,    96,   593,     0,
     461,     0,   450,   215,   216,   540,   356,   591,   349,   497,
     495,     0,   240,   331,     0,   508,   605,   560,   517,   545,
     518,   518,   518,   552,   518,   540,   518,   242,   332,   384,
     382,     0,   381,   380,   279,     0,    87,    81,     0,     0,
       0,     0,     0,   605,     0,     0,     0,     0,   404,    65,
     410,   258,     0,     0,   403,    63,   399,    58,     0,     0,
     605,   327,     0,     0,   410,   330,   564,    53,   420,   421,
     605,   422,     0,   605,   345,     0,     0,   343,     0,     0,
     410,     0,     0,     0,     0,     0,   410,     0,   123,   457,
     319,     0,     0,   272,   276,   264,   591,   605,    11,   293,
     247,    93,     0,   386,     0,     0,   310,   441,   357,   354,
     543,     0,   591,     0,   513,     0,   521,     0,   523,     0,
     529,     0,   526,   531,     0,   379,   593,   593,   504,   505,
     605,   605,   364,     0,   549,   364,   364,   362,     0,     0,
     277,    79,    43,   250,   578,   579,     0,   578,   579,     0,
       0,    40,   199,    39,   200,    66,     0,    37,   197,    38,
     198,    64,   400,   401,     0,     0,     0,     0,   494,   325,
       0,     0,   424,   346,     0,    12,   426,     0,   311,     0,
     312,     0,     0,   322,   275,   605,   252,   259,   392,     0,
       0,     0,     0,     0,   352,   496,   241,   518,   518,   518,
     518,   243,     0,     0,     0,   503,     0,   360,   361,   364,
     372,   548,     0,   375,     0,   377,   397,   278,   410,   239,
     238,    36,   196,   414,   412,     0,     0,     0,   423,     0,
     100,   107,     0,   425,     0,   315,   318,     0,   416,   417,
     415,   390,   593,   388,   391,   395,   394,   358,   355,     0,
     350,   522,     0,   519,   524,   527,   385,   383,   298,     0,
     506,   605,     0,   363,   370,   364,   364,   364,   550,   364,
     364,    60,   328,   106,     0,   605,     0,   605,   605,     0,
       0,   387,     0,   353,     0,   518,   582,   297,   359,     0,
     367,     0,   369,     0,   376,     0,   373,   378,   103,   105,
       0,   578,   579,   418,   344,   323,   389,   351,   520,   364,
     364,   364,   364,   101,   368,     0,   365,   371,   374,   364,
     366
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -838,  -838,  -838,   476,  -838,    53,  -838,  -318,   202,  -838,
      75,  -838,  -211,  -338,   757,    82,   152,  -838,    -6,   -30,
    -838,  -542,  -838,    30,   977,  -214,    -3,   -37,  -221,  -466,
     -29,  1575,   -53,   987,     9,   -21,  -838,  -838,    15,  -838,
    1144,  -838,   347,    64,  -113,  -369,    96,    89,  -838,  -374,
    -227,  -119,    98,  -371,   192,  -838,  -838,  -838,  -838,  -838,
    -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,  -838,
    -838,     8,  -190,  -458,   -10,  -611,  -838,  -838,  -838,   227,
     282,  -838,  -561,  -838,  -838,  -123,  -838,   -17,  -838,  -838,
     214,  -838,  -838,  -838,   -83,  -838,  -838,  -474,  -838,     0,
    -838,  -838,  -838,  -838,  -838,   -15,    43,  -193,  -838,  -838,
    -838,  -838,  -413,  -268,  -838,   767,  -838,  -838,  -838,    40,
    -838,  -838,  -838,  1599,  1788,  1003,  1376,  -838,  -838,   685,
     309,    42,   443,    76,  -838,  -838,  -838,   284,     7,  -242,
    -247,  -837,  -662,  -218,  -838,   270,  -639,  -551,  -833,    80,
    -541,  -838,  -519,  -838,   271,  -363,  -838,  -838,  -838,    39,
     762,  -468,   615,   647,  -838,  -838,   -50,  -838,    33,    -7,
     582,  -274,   -90,   -24,   -36,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    68,    69,    70,   285,   459,   460,   296,
     297,   512,    72,   604,    73,    74,    75,   677,   213,    76,
      77,   665,   800,    78,    79,   298,    80,    81,    82,   537,
      83,   214,   123,   124,   241,   242,   243,   700,   642,   207,
      85,   303,   608,   643,   275,   502,   503,   276,   277,   266,
     495,   530,   647,   598,    86,   210,   301,   729,   302,   317,
     739,   221,   824,   222,   825,   699,   977,   668,   666,   907,
     454,   288,   463,   691,   816,   817,   228,   747,   932,  1003,
     950,   866,   771,   772,   867,   842,   982,   983,   543,   846,
     391,   593,    88,    89,   441,   658,   657,   486,   980,   680,
     810,   911,   915,    90,    91,    92,   330,   331,   547,    93,
      94,    95,   548,   251,   252,   253,   481,    96,    97,    98,
     324,    99,   100,   217,   218,   103,   219,   450,   667,   448,
     369,   370,   371,   869,   870,   372,   373,   374,   758,   583,
     376,   377,   378,   379,   568,   380,   381,   382,   874,   875,
     383,   384,   385,   386,   387,   576,   209,   455,   308,   505,
     489,   270,   129,   672,   645,   458,   453,   432,   509,   843,
     510,   528,   255,   256,   257,   300
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     106,   268,   268,   283,   220,   268,   435,   284,   702,   250,
      87,   580,    87,   126,   126,   693,   205,   216,   216,   278,
     282,   227,   206,   216,   216,   216,   609,   313,   216,   206,
     471,   531,   715,   346,   763,   533,   342,   429,   431,   268,
     268,   760,   499,   206,  -103,   396,   263,   263,   254,   299,
     263,   573,   306,   310,   244,   269,   269,   107,   646,   269,
      87,   549,   715,   761,   314,   445,   764,   323,   244,   813,
     544,   592,   732,   206,   216,   706,   436,    71,   823,    71,
     519,   375,   375,   673,   849,   274,   279,   121,   121,   462,
     314,   536,   984,   305,   309,   121,   798,   799,   686,   659,
     662,  -102,   345,   333,   335,   337,   339,   696,  -100,   278,
     462,   280,   265,   271,   267,   267,   272,   844,   267,  1008,
     570,     3,   712,   464,  -105,   216,   712,    87,   375,   375,
     464,   873,   433,   433,   777,   566,   675,   551,   121,  -484,
     551,   586,   551,   589,   551,  -481,   551,   364,   496,  -107,
     500,   338,   304,  -106,   326,   327,  -480,   122,   122,  -102,
    -104,   -95,   889,  -578,   121,   122,   468,   487,  -337,   439,
    -101,   735,   365,  -108,  -480,   274,   279,   477,   292,   812,
     599,   485,  -337,   984,  -100,   527,   603,    42,   245,   769,
      43,   246,   247,   439,   440,  -484,  1008,   125,   125,   567,
    -102,   845,  -100,  -102,  -102,   125,   328,   329,   122,   245,
     760,   390,   246,   247,  -575,   472,   473,  -337,    87,   248,
     549,   249,   286,   -92,  -337,   -92,   603,   603,   890,   216,
     216,  -102,   761,  -102,   122,   770,   494,    62,   516,   906,
     248,   -97,   249,  -579,   434,   434,   392,   895,   125,   323,
     273,   876,   715,   901,   295,   206,   487,   281,   268,  -483,
     522,   249,   268,  -485,   541,   497,   -99,   497,   529,   529,
     -98,   506,   990,   529,   125,   535,   -94,   -96,   499,  -475,
     742,   216,  -486,   482,  -297,   216,   581,   -93,   398,   216,
     216,   299,   393,  -475,    87,   738,   833,   325,  -297,   326,
     327,    87,    87,   263,   763,  1020,   470,   263,   809,    87,
     873,   249,   881,   873,   978,  -483,   269,   536,   397,  -485,
     314,   295,  -576,   437,   375,   430,  -575,   483,  -475,   521,
     246,   247,  -575,  -297,  -107,  -475,   938,  -579,  -486,   426,
    -297,   559,   560,   561,   562,   712,   712,   422,   423,   424,
     803,   328,   329,   551,    87,   216,   216,   216,   216,    87,
     216,   216,   595,  -488,   759,   546,   601,   605,   518,   923,
     577,   504,   577,   507,   375,   267,   524,    87,   438,   536,
     273,   428,   692,   692,   389,   719,   720,  -479,   517,   281,
     873,   299,   449,   854,   121,   438,   821,    87,   671,   558,
     216,  -479,    87,   314,   461,   610,  -107,   605,   605,   465,
    -487,  -489,   268,   478,   886,   469,  -106,   712,   443,  -488,
    -475,    71,   444,  -107,  -102,   506,   563,   -99,   340,   341,
    1006,   389,   487,  1009,  -576,   216,  -479,   268,   742,   487,
    -576,   549,  -479,  -479,   651,   610,   610,   651,   891,   456,
     506,   979,   268,   878,   897,   899,   840,   263,   681,   715,
     216,   268,    87,   216,   122,   506,  -487,  -489,   651,  -104,
     283,    87,  -582,   -73,   506,   216,  -475,   121,   775,    87,
     904,   268,   263,   651,   216,   268,   835,   709,   514,    87,
     597,   723,   651,   652,   -87,   597,   295,   263,  -479,   731,
     474,   535,   497,   497,   125,   457,   263,   947,   948,   485,
    1050,   106,   268,   480,   832,   268,   494,   652,   856,   858,
     860,    87,   862,   488,   863,   268,   930,   206,   791,   513,
      87,   651,   652,   759,   822,   713,   718,  -582,   506,   398,
     794,   652,   520,   796,   314,   919,   314,   122,   216,   375,
    -578,  -582,   244,   712,   -72,    87,   651,   536,   961,   526,
     784,   794,  -104,   535,   532,   742,   730,   517,  -104,   603,
     263,  -104,  -104,  -101,   792,   603,   801,   534,   793,   896,
     652,   603,   603,   704,  -582,   831,  -582,   125,    71,   966,
    -578,   773,   754,  -582,   500,  -106,   295,   728,   792,  -104,
     538,  -104,  -102,   556,   283,   652,  -104,  -101,   314,   557,
     835,   793,   896,   360,   464,  -578,   -98,   105,   121,   105,
     121,   785,   572,   -94,   105,   105,  -102,   -96,   -93,  -578,
     105,   105,   105,   759,   690,   105,   575,   571,  1028,  -104,
    -101,   759,   398,   363,   364,   497,  1040,   352,   353,   245,
     579,   278,   246,   247,   278,   773,   773,   446,   447,   411,
     412,   802,  -578,   578,  -578,   682,   591,   105,  -578,   365,
     582,  -578,   278,   689,   585,   216,    87,   811,   814,   828,
     814,   105,   121,   701,   587,   603,   807,   814,   122,   588,
     122,   590,   206,   827,   529,   991,   993,   994,   995,   420,
     421,   422,   423,   424,   268,   268,   602,   216,   788,   627,
    -579,   497,   908,   902,   283,   206,   664,   274,   927,   678,
     274,   972,   245,   815,   812,   246,   247,   974,   125,   679,
     125,   759,   105,   789,   105,   683,   788,   692,   274,   545,
     244,   535,   795,   942,   943,   797,   705,   425,   717,   741,
     605,   722,   122,   248,   577,   249,   605,   893,   837,   368,
     388,   426,   605,   605,   725,   -87,   212,   212,   268,   751,
     753,   768,   212,   778,   779,  -579,   268,   597,   780,    87,
     790,   506,   804,  1048,   661,   663,   314,    87,   610,  -579,
     805,   216,   125,   820,   610,   216,   427,   812,   773,   651,
     610,   610,   826,   428,   759,   829,    87,    87,   912,   917,
     830,   916,   868,   263,  -410,   759,   661,   663,   838,   841,
      87,   464,  -579,   216,  -579,   105,   847,   464,  -579,   249,
     837,  -579,    87,    87,   851,   497,   105,   105,   748,   566,
      87,   855,   283,   283,   857,   859,   757,   861,   652,   914,
     757,   451,    87,    87,   882,   762,   909,   466,   766,   756,
     121,  -287,   352,   353,   726,   426,   605,   910,   577,   577,
     928,   426,   918,   920,   442,  -287,   945,   933,   929,  -410,
     523,   951,   555,   946,   525,   326,   327,   949,   105,   952,
     954,  -593,   105,  -410,  -593,  -593,   105,   105,   956,   958,
     452,   105,   963,   964,   610,   975,   467,   428,   105,   105,
    -287,   976,   268,   428,    87,    87,   105,  -287,   969,  1016,
     475,   985,    87,   814,   249,   515,  -410,   992,  -410,   986,
     122,   996,   997,   998,   426,  -410,  1011,   328,   329,   426,
     245,  1013,  1012,   246,   247,  1022,   539,  1045,  -578,  1024,
     121,   550,  1029,   326,   327,   121,  1031,  1033,   283,  1035,
     426,   105,   105,   105,   105,   105,   105,   105,   105,   476,
     125,   248,  -482,   249,   467,  1021,   428,  -579,   212,   212,
      87,   428,    87,  1053,   105,    87,  -482,  1055,   868,   724,
     225,   868,   121,   130,   868,   540,   868,   865,   398,   577,
     268,   905,   428,  1046,   105,   328,   329,   105,  1044,   105,
     208,   903,   105,   506,   913,   681,   814,  1043,   755,   484,
     122,  -482,  1000,   216,     0,   122,   921,   922,  -482,  1005,
     498,   651,  -286,     0,   925,     0,  -298,     0,   508,   511,
     872,   877,   105,     0,   868,   263,  -286,   931,     0,     0,
    -298,     0,   105,   105,   871,   420,   421,   422,   423,   424,
     125,     0,   122,     0,     0,   125,     0,   105,     0,   105,
     105,   868,   644,   868,     0,   868,   653,   868,   105,   656,
     652,  -286,   105,   716,     0,  -298,   105,     0,  -286,   782,
     721,   105,  -298,   332,   326,   327,   105,   868,     0,     0,
     674,   727,   125,   426,   212,   212,   212,   212,   965,   564,
     565,     0,     0,   644,     0,   653,   973,   745,     0,   355,
     356,   357,   358,     0,   674,   937,   398,   939,   105,     0,
       0,   940,     0,   892,   894,   359,     0,   105,   783,   898,
     900,     0,     0,   411,   412,   428,   328,   329,     0,     0,
       0,   749,   750,   215,   215,   105,     0,   953,   955,   215,
     264,   264,   105,   674,   264,   892,   894,     0,   898,   900,
       0,     0,     0,     0,  1017,     0,  1018,     0,     0,  1019,
       0,   776,   419,   420,   421,   422,   423,   424,   674,     0,
       0,   287,   289,   290,   291,   999,     0,     0,   264,   307,
    1014,   245,   987,   988,   246,   247,     0,     0,   848,   426,
     343,   344,   676,     0,   426,  -294,     0,   757,  -294,  -294,
     877,     0,  1007,   877,  1010,   877,   334,   326,   327,     0,
    1001,  1004,   248,   871,   249,     0,   871,   745,   871,   355,
     356,   357,   358,   962,   452,  -294,  -294,     0,  -294,  1015,
       0,   428,     0,     0,     0,   359,   428,     0,   806,  1023,
       0,   215,  1025,     0,     0,     0,   962,   336,   326,   327,
       0,     0,   354,   877,   355,   356,   357,   358,     0,   328,
     329,     0,   105,   105,     0,     0,   871,  1030,  1032,  1034,
     359,  1036,  1037,   360,  1047,     0,     0,     0,     0,  1049,
     877,  1051,   877,     0,   877,  1052,   877,   545,   326,   327,
       0,   839,     0,   871,   105,   871,   361,   871,     0,   871,
     328,   329,   362,   363,   364,  1059,   877,     0,   989,   850,
       0,  1054,  1056,  1057,  1058,     0,     0,     0,     0,   871,
       0,  1060,     0,   552,   326,   327,   553,   326,   327,   365,
       0,     0,   366,     0,     0,     0,   554,   326,   327,     0,
     328,   329,     0,  1002,     0,   215,   215,     0,     0,   740,
     326,   327,   745,     0,   355,   356,   357,   358,   104,     0,
     104,   128,   128,     0,     0,     0,   105,     0,     0,   230,
     359,     0,     0,     0,   105,   105,   328,   329,   105,   328,
     329,   105,   105,   490,   491,   492,   343,   105,   105,   328,
     329,     0,     0,   105,   105,     0,   361,   264,   926,     0,
       0,   264,   328,   329,   212,   215,   215,   105,   104,   644,
     105,   653,   316,   354,   935,   355,   356,   357,   358,   105,
     105,     0,     0,     0,   398,     0,     0,   105,     0,   245,
       0,   359,   246,   247,   360,     0,   212,     0,   316,   105,
     105,   411,   412,   347,   348,   349,   350,   351,   745,     0,
     355,   356,   357,   358,     0,     0,   494,   361,     0,     0,
     248,     0,   249,   362,   363,   364,   359,     0,     0,     0,
       0,   215,   215,   215,   215,   104,   215,   215,   417,   418,
     419,   420,   421,   422,   423,   424,     0,     0,     0,     0,
     365,   105,   361,   366,   574,     0,     0,     0,   746,     0,
       0,   105,   105,     0,     0,   584,   367,     0,     0,   105,
       0,     0,     0,     0,     0,     0,   596,     0,     0,     0,
       0,   607,   612,   613,   614,   615,   616,   617,   618,   619,
     620,   621,   622,   623,   624,   625,   626,     0,   628,   629,
     630,   631,   632,   633,   634,   635,   636,   637,   638,     0,
       0,   264,   212,     0,     0,     0,     0,    84,     0,    84,
       0,   660,   660,     0,     0,     0,   104,   105,   226,   105,
       0,     0,   105,     0,     0,     0,   264,     0,     0,   215,
       0,   101,     0,   101,   127,   127,   127,     0,     0,   660,
       0,   264,   229,   660,   660,     0,     0,     0,     0,     0,
     264,   981,     0,   355,   356,   357,   358,    84,     0,   703,
     105,     0,     0,   707,     0,     0,     0,   708,     0,   359,
     711,     0,   714,     0,   307,   291,     0,     0,     0,     0,
       0,   101,     0,     0,   354,   315,   355,   356,   357,   358,
       0,   660,   104,   674,     0,     0,     0,     0,     0,   104,
     104,   711,   359,     0,   307,   360,     0,   104,     0,     0,
       0,   315,     0,     0,   264,     0,     0,     0,   316,     0,
       0,     0,     0,     0,    84,     0,     0,     0,   361,     0,
     743,   744,     0,     0,   362,   363,   364,   354,     0,   355,
     356,   357,   358,     0,     0,     0,   752,     0,   101,     0,
       0,     0,   104,     0,     0,   359,     0,   104,   360,     0,
       0,   365,     0,     0,   366,   767,     0,     0,   774,     0,
       0,     0,   569,     0,     0,   104,     0,   542,     0,     0,
       0,   361,     0,     0,     0,     0,     0,   362,   363,   364,
       0,     0,     0,     0,     0,   104,     0,     0,     0,     0,
     104,   316,     0,   611,     0,   354,     0,   355,   356,   357,
     358,     0,     0,     0,   365,    84,     0,   366,     0,     0,
     102,     0,   102,   359,     0,     0,   360,   765,     0,   355,
     356,   357,   358,     0,     0,     0,     0,     0,     0,   101,
     852,   215,     0,   611,   611,   359,     0,     0,   360,   361,
       0,     0,     0,   808,     0,   362,   363,   364,     0,     0,
     104,     0,     0,   745,     0,   355,   356,   357,   358,   104,
     102,   361,     0,   215,     0,     0,     0,   104,   363,   364,
       0,   359,   365,     0,   834,   366,     0,   104,     0,     0,
       0,    84,     0,   711,   307,     0,     0,     0,    84,    84,
       0,     0,     0,     0,   365,     0,    84,   361,     0,     0,
       0,     0,     0,   934,     0,   101,     0,     0,     0,   104,
       0,     0,   101,   101,     0,     0,     0,     0,   104,     0,
     101,     0,     0,     0,     0,     0,     0,   102,     0,     0,
       0,   315,   316,     0,   316,     0,     0,     0,     0,   880,
       0,    84,     0,   104,   660,   883,    84,   264,     0,     0,
     660,   660,     0,     0,     0,   711,   660,   660,     0,     0,
       0,     0,     0,     0,    84,   101,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,     0,   215,
       0,     0,   660,   660,    84,   660,   660,     0,   101,    84,
       0,     0,   606,     0,     0,   924,   316,     0,     0,     0,
     291,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,   101,   315,     0,     0,   936,   102,   354,
       0,   355,   356,   357,   358,     0,     0,     0,   941,     0,
       0,     0,   606,   606,     0,     0,     0,   359,     0,     0,
     360,     0,     0,   957,     0,     0,     0,     0,     0,    84,
       0,     0,     0,   959,   960,     0,     0,     0,    84,     0,
     660,     0,     0,   361,   104,     0,    84,     0,     0,   362,
     363,   364,     0,   101,     0,     0,    84,     0,     0,     0,
       0,     0,   101,   660,     0,     0,     0,     0,     0,     0,
     101,   307,     0,     0,   102,     0,   365,     0,     0,   366,
     101,   102,   102,     0,     0,     0,     0,     0,    84,   102,
       0,     0,     0,     0,     0,     0,     0,    84,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,     0,     0,     0,
       0,   101,    84,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   102,   315,     0,   315,     0,   102,
       0,     0,     0,     0,     0,     0,   101,   104,     0,     0,
       0,     0,     0,     0,   316,   104,   611,   102,     0,   264,
       0,     0,   611,     0,     0,     0,     0,     0,   611,   611,
       0,     0,     0,     0,   104,   104,     0,   102,     0,     0,
       0,     0,   102,     0,     0,   102,     0,     0,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   315,
     104,   104,     0,     0,     0,     0,     0,     0,   104,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     104,   104,     0,     0,     0,   102,   102,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   102,    84,   128,     0,     0,     0,     0,   128,
       0,   102,     0,     0,     0,     0,     0,     0,     0,   102,
       0,     0,     0,     0,     0,     0,     0,   101,     0,   102,
       0,     0,   611,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   104,   104,     0,     0,   971,     0,     0,     0,
     104,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   102,     0,     0,     0,     0,     0,     0,     0,     0,
     102,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   102,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    84,     0,   104,     0,
     104,     0,     0,   104,    84,   606,     0,     0,     0,     0,
       0,   606,     0,     0,     0,     0,     0,   606,   606,     0,
     101,     0,     0,    84,    84,     0,     0,   315,   101,     0,
       0,     0,     0,     0,     0,     0,     0,    84,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   101,   101,    84,
      84,     0,     0,     0,     0,     0,     0,    84,     0,     0,
       0,   101,     0,     0,     0,     0,     0,     0,     0,    84,
      84,     0,     0,   101,   101,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   101,   101,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   102,     0,     0,     0,
    -605,     0,     0,     0,     0,     0,     0,   127,     0,     0,
       0,   606,   127,  -605,  -605,  -605,  -605,  -605,  -605,     0,
    -605,    84,    84,     0,     0,   968,  -605,  -605,     0,    84,
       0,     0,     0,     0,     0,     0,     0,  -605,  -605,     0,
    -605,  -605,  -605,  -605,  -605,   101,   101,     0,     0,   970,
       0,     0,     0,   101,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   398,   399,   400,   401,
     402,   403,   404,   405,     0,   407,   408,    84,     0,    84,
       0,  -605,    84,   411,   412,     0,     0,     0,     0,   102,
       0,     0,     0,     0,     0,  -605,     0,   102,   102,     0,
       0,   101,     0,   101,   102,  -605,   101,     0,  -605,  -605,
     102,   102,     0,     0,     0,     0,   102,   102,   415,   416,
     417,   418,   419,   420,   421,   422,   423,   424,  -605,  -605,
     102,     0,     0,     0,   273,  -605,  -605,  -605,  -605,     0,
       0,     0,   102,   102,     0,     0,     0,     0,     0,     0,
     102,     0,     0,   781,     0,     0,     0,     0,     0,     0,
       0,     0,   102,   102,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   398,   399,   400,   401,   402,   403,   404,   405,   406,
     407,   408,   409,   410,     0,     0,     0,     0,   411,   412,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   102,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   102,   102,     0,     0,     0,     0,
       0,   414,   102,   415,   416,   417,   418,   419,   420,   421,
     422,   423,   424,     0,     0,     0,     0,     0,     0,     0,
       0,  -273,  -605,     4,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,     0,     0,     0,
       0,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
     102,    27,   102,     0,     0,   102,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,  -288,     0,    64,  -605,     0,     0,
    -605,  -605,     0,     0,     0,     0,     0,  -288,  -288,  -288,
    -288,  -288,  -288,     0,  -288,     0,    65,    66,    67,     0,
       0,  -288,  -288,  -288,     0,     0,     0,     0,  -605,     0,
    -605,  -288,  -288,     0,  -288,  -288,  -288,  -288,  -288,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -288,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -288,  -288,  -288,  -288,
    -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,     0,
       0,     0,     0,  -288,  -288,  -288,     0,     0,  -288,     0,
       0,     0,     0,     0,  -288,     0,     0,     0,     0,  -288,
       0,     0,     0,     0,     0,     0,     0,  -288,     0,  -288,
       0,     0,  -288,  -288,     0,     0,  -288,  -288,  -288,  -288,
    -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,     0,     0,
    -409,     0,     0,  -288,  -288,  -288,  -288,     0,     0,  -288,
    -288,  -288,  -288,  -409,  -409,  -409,  -409,  -409,  -409,     0,
    -409,     0,     0,     0,     0,     0,  -409,  -409,  -409,     0,
       0,     0,     0,     0,     0,     0,     0,  -409,  -409,     0,
    -409,  -409,  -409,  -409,  -409,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -409,  -409,  -409,  -409,  -409,  -409,  -409,  -409,
    -409,  -409,  -409,  -409,  -409,     0,     0,     0,     0,  -409,
    -409,  -409,     0,     0,  -409,     0,     0,     0,     0,     0,
    -409,     0,     0,     0,     0,  -409,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -409,     0,     0,  -409,  -409,
       0,     0,  -409,     0,  -409,  -409,  -409,  -409,  -409,  -409,
    -409,  -409,  -409,  -409,     0,     0,  -475,     0,  -409,  -409,
    -409,  -409,  -409,     0,   273,  -409,  -409,  -409,  -409,  -475,
    -475,  -475,  -475,  -475,  -475,     0,  -475,     0,     0,     0,
       0,     0,     0,  -475,  -475,     0,     0,     0,     0,     0,
       0,     0,     0,  -475,  -475,     0,  -475,  -475,  -475,  -475,
    -475,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   488,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -475,  -475,
    -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,
    -475,     0,     0,     0,     0,  -475,  -475,  -475,     0,  -475,
    -475,     0,     0,     0,     0,     0,  -475,     0,     0,     0,
       0,  -475,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -475,     0,     0,  -475,  -475,     0,  -475,  -475,     0,
    -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,
       0,     0,  -605,     0,     0,  -475,  -475,  -475,  -475,     0,
       0,  -475,  -475,  -475,  -475,  -605,  -605,  -605,  -605,  -605,
    -605,     0,  -605,     0,     0,     0,     0,     0,  -605,  -605,
    -605,     0,     0,     0,     0,     0,     0,     0,     0,  -605,
    -605,     0,  -605,  -605,  -605,  -605,  -605,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -605,  -605,  -605,  -605,  -605,  -605,
    -605,  -605,  -605,  -605,  -605,  -605,  -605,     0,     0,     0,
       0,  -605,  -605,  -605,     0,     0,  -605,     0,     0,     0,
       0,     0,  -605,     0,     0,     0,     0,  -605,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -605,     0,     0,
    -605,  -605,     0,     0,  -605,     0,  -605,  -605,  -605,  -605,
    -605,  -605,  -605,  -605,  -605,  -605,     0,     0,  -605,     0,
    -605,  -605,  -605,  -605,  -605,     0,   273,  -605,  -605,  -605,
    -605,  -605,  -605,  -605,  -605,  -605,  -605,     0,  -605,     0,
       0,     0,     0,     0,     0,  -605,  -605,     0,     0,     0,
       0,     0,     0,     0,     0,  -605,  -605,     0,  -605,  -605,
    -605,  -605,  -605,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -605,  -605,  -605,  -605,  -605,  -605,  -605,  -605,  -605,  -605,
    -605,  -605,  -605,     0,     0,     0,     0,  -605,  -605,  -605,
       0,     0,  -605,     0,     0,     0,     0,     0,  -605,     0,
       0,     0,     0,  -605,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -605,     0,     0,  -605,  -605,     0,     0,
    -605,     0,  -605,  -605,  -605,  -605,  -605,  -605,  -605,  -605,
    -605,  -605,     0,     0,  -582,     0,     0,  -605,  -605,  -605,
    -605,     0,   273,  -605,  -605,  -605,  -605,  -582,  -582,  -582,
       0,  -582,  -582,     0,  -582,     0,     0,     0,     0,     0,
    -582,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -582,  -582,     0,  -582,  -582,  -582,  -582,  -582,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,     0,
       0,     0,     0,  -582,  -582,  -582,     0,   786,  -582,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -582,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -582,
       0,     0,  -582,  -582,     0,  -103,  -582,     0,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,     0,     0,
    -582,     0,  -582,  -582,  -582,     0,   -95,     0,     0,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,     0,  -582,  -582,     0,
    -582,     0,     0,     0,     0,     0,  -582,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -582,  -582,     0,
    -582,  -582,  -582,  -582,  -582,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,     0,     0,     0,     0,  -582,
    -582,  -582,     0,   786,  -582,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -582,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -582,     0,     0,  -582,  -582,
       0,  -103,  -582,     0,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,     0,     0,  -297,     0,  -582,  -582,
    -582,     0,  -582,     0,     0,  -582,  -582,  -582,  -582,  -297,
    -297,  -297,     0,  -297,  -297,     0,  -297,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -297,  -297,     0,  -297,  -297,  -297,  -297,
    -297,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -297,  -297,
    -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,
    -297,     0,     0,     0,     0,  -297,  -297,  -297,     0,   787,
    -297,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -297,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -297,     0,     0,  -297,  -297,     0,  -105,  -297,     0,
    -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,
       0,     0,  -297,     0,     0,  -297,  -297,     0,   -97,     0,
       0,  -297,  -297,  -297,  -297,  -297,  -297,  -297,     0,  -297,
    -297,     0,  -297,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -297,
    -297,     0,  -297,  -297,  -297,  -297,  -297,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -297,  -297,  -297,  -297,  -297,  -297,
    -297,  -297,  -297,  -297,  -297,  -297,  -297,     0,     0,     0,
       0,  -297,  -297,  -297,     0,   787,  -297,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -297,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -297,     0,     0,
    -297,  -297,     0,  -105,  -297,     0,  -297,  -297,  -297,  -297,
    -297,  -297,  -297,  -297,  -297,  -297,     0,     0,     0,     0,
       0,  -297,  -297,     0,  -297,     0,     0,  -297,  -297,  -297,
    -297,   293,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,  -605,  -605,  -605,     0,     0,  -605,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,  -605,     0,     0,  -605,  -605,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,    66,    67,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -605,   293,  -605,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,  -605,     0,  -605,  -605,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,    51,     0,
       0,    52,    53,     0,    54,    55,     0,    56,     0,     0,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,  -605,     0,     0,  -605,  -605,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,    67,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -605,   293,  -605,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,  -605,     0,     0,
    -605,    15,  -605,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,  -605,     0,     0,
    -605,  -605,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -605,   293,
    -605,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -605,     0,     0,  -605,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,  -605,     0,     0,  -605,  -605,     4,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    65,    66,    67,     0,    15,     0,    16,    17,
      18,    19,     0,     0,  -605,     0,  -605,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,    51,
       0,     0,    52,    53,     0,    54,    55,     0,    56,     0,
       0,    57,     0,    58,    59,    60,    61,    62,    63,     0,
       0,    64,  -605,     0,     0,  -605,  -605,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    65,    66,    67,     0,     0,  -605,     0,     0,     0,
       0,     0,     0,  -605,   293,  -605,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,  -605,  -605,     0,
       0,     0,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,    51,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,  -605,     0,
       0,  -605,  -605,   293,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    65,    66,    67,
       0,    15,     0,    16,    17,    18,    19,     0,     0,  -605,
       0,  -605,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,   294,    53,     0,
      54,    55,     0,    56,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,  -605,     0,     0,
    -605,  -605,   293,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    65,    66,    67,     0,
      15,     0,    16,    17,    18,    19,     0,  -605,  -605,     0,
    -605,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,    51,     0,     0,    52,    53,     0,    54,
      55,     0,    56,     0,     0,    57,     0,    58,    59,    60,
      61,    62,    63,     0,     0,    64,  -605,     0,     0,  -605,
    -605,   293,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    65,    66,    67,     0,    15,
       0,    16,    17,    18,    19,     0,  -605,  -605,     0,  -605,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,  -605,     0,     0,  -605,  -605,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,    66,    67,     0,     0,  -605,
       0,     0,     0,     0,     0,     0,  -605,   293,  -605,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,  -605,     0,     0,     0,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,    51,     0,
       0,    52,    53,     0,    54,    55,     0,    56,     0,     0,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,  -605,     0,     0,  -605,  -605,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
      65,    66,    67,     0,    15,     0,    16,    17,    18,    19,
       0,     0,  -605,     0,  -605,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,    51,     0,     0,
      52,    53,     0,    54,    55,     0,    56,     0,     0,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
     245,     0,     0,   246,   247,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    65,
      66,    67,     0,    15,     0,    16,    17,    18,    19,     0,
       0,   248,     0,   249,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,   245,
       0,     0,   246,   247,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,    65,    66,
      67,     0,    15,     0,    16,    17,    18,    19,     0,     0,
     248,     0,   249,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,   245,     0,
       0,   246,   247,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,    65,    66,    67,
       0,    15,     0,   108,   109,    18,    19,     0,     0,   248,
       0,   249,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    53,     0,
      54,    55,     0,     0,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,   245,     0,     0,
     246,   247,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,   262,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   248,     0,
     249,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,   153,   154,     0,     0,     0,   155,   156,
     157,   158,   159,   160,   161,   162,   163,   164,     0,     0,
       0,     0,     0,   165,   166,   167,   168,   169,   170,   171,
     172,    36,    37,   173,    39,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,   182,     0,     0,   183,
     184,     0,     0,     0,     0,   185,   186,   187,   188,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,     0,     0,     0,     0,     0,
       0,   203,   204,  -575,  -575,  -575,  -575,  -575,  -575,  -575,
    -575,  -575,     0,     0,     0,     0,     0,     0,     0,  -575,
       0,  -575,  -575,  -575,  -575,     0,  -575,     0,     0,     0,
    -575,  -575,  -575,  -575,  -575,  -575,  -575,     0,     0,  -575,
       0,     0,     0,     0,     0,     0,     0,     0,  -575,  -575,
    -575,  -575,  -575,  -575,  -575,  -575,  -575,     0,  -575,  -575,
    -575,     0,     0,  -575,     0,     0,  -575,  -575,     0,  -575,
    -575,  -575,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -575,  -575,     0,     0,     0,
       0,     0,  -575,     0,     0,  -575,  -575,     0,  -575,  -575,
       0,  -575,  -575,  -575,  -575,     0,  -575,  -575,  -575,  -575,
    -575,  -575,     0,     0,  -575,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -575,  -575,  -575,     0,  -575,     0,
       0,     0,     0,     0,  -575,  -576,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  -576,     0,     0,     0,     0,     0,     0,
       0,  -576,     0,  -576,  -576,  -576,  -576,     0,  -576,     0,
       0,     0,  -576,  -576,  -576,  -576,  -576,  -576,  -576,     0,
       0,  -576,     0,     0,     0,     0,     0,     0,     0,     0,
    -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,     0,
    -576,  -576,  -576,     0,     0,  -576,     0,     0,  -576,  -576,
       0,  -576,  -576,  -576,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -576,  -576,     0,
       0,     0,     0,     0,  -576,     0,     0,  -576,  -576,     0,
    -576,  -576,     0,  -576,  -576,  -576,  -576,     0,  -576,  -576,
    -576,  -576,  -576,  -576,     0,     0,  -576,     0,     0,     0,
       0,     0,     0,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,     0,     0,     0,     0,  -576,  -576,  -576,  -578,
    -576,  -578,  -578,  -578,  -578,     0,  -576,     0,     0,     0,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,     0,     0,  -578,
       0,     0,     0,     0,     0,     0,     0,     0,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,     0,  -578,  -578,
    -578,     0,     0,  -578,     0,     0,  -578,  -578,     0,  -578,
    -578,  -578,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -578,  -578,     0,     0,     0,
       0,     0,  -578,   818,     0,  -578,  -578,     0,  -578,  -578,
       0,  -578,  -578,  -578,  -578,     0,  -578,  -578,  -578,  -578,
    -578,  -578,     0,     0,  -578,     0,     0,     0,     0,     0,
       0,  -103,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,     0,     0,     0,  -578,  -578,  -578,     0,  -579,     0,
    -579,  -579,  -579,  -579,  -578,     0,     0,     0,     0,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,     0,     0,  -579,     0,
       0,     0,     0,     0,     0,     0,     0,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,     0,  -579,  -579,  -579,
       0,     0,  -579,     0,     0,  -579,  -579,     0,  -579,  -579,
    -579,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -579,  -579,     0,     0,     0,     0,
       0,  -579,   819,     0,  -579,  -579,     0,  -579,  -579,     0,
    -579,  -579,  -579,  -579,     0,  -579,  -579,  -579,  -579,  -579,
    -579,     0,     0,  -579,     0,     0,     0,     0,     0,     0,
    -105,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
       0,     0,     0,  -579,  -579,  -579,     0,  -580,     0,  -580,
    -580,  -580,  -580,  -579,     0,     0,     0,     0,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,     0,     0,  -580,     0,     0,
       0,     0,     0,     0,     0,     0,  -580,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,     0,  -580,  -580,  -580,     0,
       0,  -580,     0,     0,  -580,  -580,     0,  -580,  -580,  -580,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -580,  -580,     0,     0,     0,     0,     0,
    -580,     0,     0,  -580,  -580,     0,  -580,  -580,     0,  -580,
    -580,  -580,  -580,     0,  -580,  -580,  -580,  -580,  -580,  -580,
       0,     0,  -580,     0,     0,     0,     0,     0,     0,  -581,
    -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,     0,     0,
       0,     0,  -580,  -580,  -580,  -581,     0,  -581,  -581,  -581,
    -581,     0,  -580,     0,     0,     0,  -581,  -581,  -581,  -581,
    -581,  -581,  -581,     0,     0,  -581,     0,     0,     0,     0,
       0,     0,     0,     0,  -581,  -581,  -581,  -581,  -581,  -581,
    -581,  -581,  -581,     0,  -581,  -581,  -581,     0,     0,  -581,
       0,     0,  -581,  -581,     0,  -581,  -581,  -581,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -581,  -581,     0,     0,     0,     0,     0,  -581,     0,
       0,  -581,  -581,     0,  -581,  -581,     0,  -581,  -581,  -581,
    -581,     0,  -581,  -581,  -581,  -581,  -581,  -581,     0,     0,
    -581,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -581,  -581,  -581,     0,     0,     0,     0,     0,     0,     0,
    -581,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,   153,   154,     0,     0,     0,   155,   156,
     157,   231,   232,   233,   234,   162,   163,   164,     0,     0,
       0,     0,     0,   165,   166,   167,   235,   236,   237,   238,
     172,   318,   319,   239,   320,     0,     0,     0,     0,     0,
       0,   321,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,   182,     0,     0,   183,
     184,     0,     0,     0,     0,   185,   186,   187,   188,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,   322,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,     0,     0,     0,     0,     0,
       0,   203,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,     0,     0,     0,   155,
     156,   157,   231,   232,   233,   234,   162,   163,   164,     0,
       0,     0,     0,     0,   165,   166,   167,   235,   236,   237,
     238,   172,   318,   319,   239,   320,     0,     0,     0,     0,
       0,     0,   321,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,   182,     0,     0,
     183,   184,     0,     0,     0,     0,   185,   186,   187,   188,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     189,   190,     0,     0,     0,     0,     0,     0,     0,   479,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,     0,     0,     0,     0,
       0,     0,   203,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,     0,     0,     0,
     155,   156,   157,   231,   232,   233,   234,   162,   163,   164,
       0,     0,     0,     0,     0,   165,   166,   167,   235,   236,
     237,   238,   172,     0,     0,   239,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,   182,     0,
       0,   183,   184,     0,     0,     0,     0,   185,   186,   187,
     188,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,   240,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,     0,     0,     0,
       0,     0,     0,   203,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,     0,     0,
       0,   155,   156,   157,   231,   232,   233,   234,   162,   163,
     164,     0,     0,     0,     0,     0,   165,   166,   167,   235,
     236,   237,   238,   172,     0,     0,   239,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,   182,
       0,     0,   183,   184,     0,     0,     0,     0,   185,   186,
     187,   188,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,     0,     0,
       0,     0,     0,     0,   203,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,    15,     0,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,   116,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   311,     0,     0,   119,    53,     0,
      54,    55,     0,     0,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   120,   108,   109,    18,
      19,     0,     0,     0,   312,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   311,     0,
       0,   119,    53,     0,    54,    55,     0,     0,     0,     0,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,     0,     0,     0,     0,    15,
     120,    16,    17,    18,    19,     0,     0,     0,   600,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,     0,    65,    66,    67,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,    66,    67,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,   258,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    53,     0,    54,    55,     0,   259,   260,   261,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   262,    67,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   258,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
     501,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      53,     0,    54,    55,     0,   259,   260,   261,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   262,
      67,    15,     0,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,   258,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    53,     0,
      54,    55,     0,   710,   260,   261,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    65,   262,    67,    15,
       0,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   258,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,   836,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    53,     0,    54,    55,
       0,   710,   260,   261,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,   262,    67,    15,     0,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   258,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    53,     0,    54,    55,     0,   259,
     260,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,   262,    67,    15,     0,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,   258,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    53,     0,    54,    55,     0,     0,   260,   261,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   262,    67,    15,     0,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   258,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      53,     0,    54,    55,     0,   710,   260,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   262,
      67,    15,     0,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,   258,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    53,     0,
      54,    55,     0,     0,   260,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    65,   262,    67,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    53,     0,    54,    55,
       0,   594,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,   262,    67,    15,     0,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    53,     0,    54,    55,     0,   259,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,   262,    67,    15,     0,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    53,     0,    54,    55,     0,   594,     0,     0,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   262,    67,    15,     0,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      53,     0,    54,    55,     0,   879,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   262,
      67,    15,     0,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    53,     0,
      54,    55,     0,   710,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    65,   262,    67,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    53,     0,    54,    55,
       0,     0,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,    66,    67,    15,     0,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    53,     0,    54,    55,     0,     0,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,   262,    67,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    53,     0,    54,    55,     0,     0,     0,     0,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   262,    67,    15,     0,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   114,    35,    36,    37,   115,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   117,     0,     0,   118,     0,     0,   119,
      53,     0,    54,    55,     0,     0,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,   120,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   223,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     224,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,    15,   120,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,   116,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   311,     0,     0,   394,    53,     0,
      54,    55,     0,   395,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   120,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,   114,    35,    36,
      37,   115,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   116,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,   119,    53,     0,    54,    55,     0,     0,     0,     0,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
     120,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,   116,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   311,     0,     0,   394,    53,     0,    54,    55,
       0,     0,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,     0,    15,   120,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   944,     0,     0,   119,
      53,     0,    54,    55,     0,     0,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,   120,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   223,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     967,     0,     0,   119,    53,     0,    54,    55,     0,   639,
     640,     0,    57,   641,    58,    59,    60,    61,    62,    63,
       0,     0,    64,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,   182,     0,     0,   183,
     184,     0,   120,     0,     0,   185,   186,   187,   188,     0,
       0,     0,   398,  -606,  -606,  -606,  -606,   403,   404,   189,
     190,  -606,  -606,     0,     0,     0,     0,     0,     0,   411,
     412,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   648,   649,     0,     0,   650,
       0,   203,   273,     0,   415,   416,   417,   418,   419,   420,
     421,   422,   423,   424,   174,   175,   176,   177,   178,   179,
     180,   181,   182,     0,     0,   183,   184,     0,     0,     0,
       0,   185,   186,   187,   188,     0,     0,     0,   398,   399,
     400,   401,   402,   403,   404,   189,   190,   407,   408,     0,
       0,     0,     0,     0,     0,   411,   412,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,   669,   640,     0,     0,   670,     0,   203,   273,     0,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     174,   175,   176,   177,   178,   179,   180,   181,   182,     0,
       0,   183,   184,     0,     0,     0,     0,   185,   186,   187,
     188,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   654,   649,     0,
       0,   655,     0,   203,   273,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,   182,     0,     0,   183,   184,     0,
       0,     0,     0,   185,   186,   187,   188,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   684,   640,     0,     0,   685,     0,   203,
     273,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
     182,     0,     0,   183,   184,     0,     0,     0,     0,   185,
     186,   187,   188,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   687,
     649,     0,     0,   688,     0,   203,   273,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,   182,     0,     0,   183,
     184,     0,     0,     0,     0,   185,   186,   187,   188,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   694,   640,     0,     0,   695,
       0,   203,   273,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,   182,     0,     0,   183,   184,     0,     0,     0,
       0,   185,   186,   187,   188,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,   697,   649,     0,     0,   698,     0,   203,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,   182,     0,
       0,   183,   184,     0,     0,     0,     0,   185,   186,   187,
     188,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   733,   640,     0,
       0,   734,     0,   203,   273,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,   182,     0,     0,   183,   184,     0,
       0,     0,     0,   185,   186,   187,   188,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   736,   649,     0,     0,   737,     0,   203,
     273,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
     182,     0,     0,   183,   184,     0,     0,     0,     0,   185,
     186,   187,   188,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   884,
     640,     0,     0,   885,     0,   203,   273,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,   182,     0,     0,   183,
     184,     0,     0,     0,     0,   185,   186,   187,   188,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   887,   649,     0,     0,   888,
       0,   203,   273,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,   182,     0,     0,   183,   184,     0,     0,     0,
       0,   185,   186,   187,   188,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,  1026,   640,     0,     0,  1027,     0,   203,   273,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,   182,     0,
       0,   183,   184,     0,     0,     0,     0,   185,   186,   187,
     188,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,  1038,   640,     0,
       0,  1039,     0,   203,   273,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,   182,     0,     0,   183,   184,     0,
       0,     0,     0,   185,   186,   187,   188,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,  1041,   649,     0,     0,  1042,     0,   203,
     273,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
     182,     0,     0,   183,   184,     0,     0,     0,     0,   185,
     186,   187,   188,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   654,
     649,     0,     0,   655,     0,   203,   273,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,   182,     0,     0,   183,
     184,     0,     0,     0,     0,   185,   186,   187,   188,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   781,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   853,     0,     0,     0,     0,
       0,   203,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,     0,     0,     0,     0,   411,
     412,     0,     0,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,     0,     0,     0,     0,
     411,   412,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   414,     0,   415,   416,   417,   418,   419,   420,
     421,   422,   423,   424,   864,     0,     0,     0,     0,     0,
       0,     0,     0,   414,     0,   415,   416,   417,   418,   419,
     420,   421,   422,   423,   424,     0,     0,     0,     0,     0,
       0,     0,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,     0,     0,     0,     0,   411,
     412,   398,   399,   400,   401,   402,   403,   404,   405,   406,
     407,   408,   409,   410,     0,     0,     0,     0,   411,   412,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   414,     0,   415,   416,   417,   418,   419,   420,
     421,   422,   423,   424,     0,     0,     0,     0,     0,     0,
       0,   414,     0,   415,   416,   417,   418,   419,   420,   421,
     422,   423,   424,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,     0,   249,     0,     0,
     411,   412,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,     0,     0,     0,     0,   411,
     412,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   414,     0,   415,   416,   417,   418,   419,
     420,   421,   422,   423,   424,     0,     0,     0,     0,     0,
       0,     0,   414,  -273,   415,   416,   417,   418,   419,   420,
     421,   422,   423,   424,     0,     0,     0,     0,     0,     0,
       0,     0,  -274,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,     0,     0,     0,     0,
     411,   412,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,     0,     0,     0,     0,   411,
     412,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   414,     0,   415,   416,   417,   418,   419,
     420,   421,   422,   423,   424,     0,     0,     0,     0,     0,
       0,     0,   414,  -275,   415,   416,   417,   418,   419,   420,
     421,   422,   423,   424,     0,     0,     0,     0,     0,     0,
       0,     0,  -276,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,     0,     0,     0,     0,
     411,   412,     0,     0,     0,   413,   398,   399,   400,   401,
     402,   403,   404,   405,   406,   407,   408,   409,   410,     0,
       0,     0,     0,   411,   412,     0,     0,     0,   493,     0,
       0,     0,     0,   414,     0,   415,   416,   417,   418,   419,
     420,   421,   422,   423,   424,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   414,     0,   415,   416,
     417,   418,   419,   420,   421,   422,   423,   424,   398,   399,
     400,   401,   402,   403,   404,   405,   406,   407,   408,   409,
     410,     0,     0,     0,     0,   411,   412,   398,   399,   400,
     401,   402,   403,   404,   405,   406,   407,   408,  -606,  -606,
       0,     0,     0,     0,   411,   412,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   414,     0,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   415,
     416,   417,   418,   419,   420,   421,   422,   423,   424
};

static const yytype_int16 yycheck[] =
{
       2,    16,    17,    27,    10,    20,    89,    28,   482,    15,
       2,   374,     4,     5,     6,   473,     7,     9,    10,    21,
      27,    13,     7,    15,    16,    17,   397,    56,    20,    14,
     220,   305,   500,    69,   585,   309,    66,    87,    88,    54,
      55,   582,   269,    28,    25,    82,    16,    17,    15,    52,
      20,   369,    54,    55,    14,    16,    17,     4,   427,    20,
      52,   329,   530,   582,    56,   118,   585,    58,    28,   680,
     317,   389,   538,    58,    66,   488,    91,     2,   689,     4,
     294,    74,    75,   452,   746,    21,    22,     5,     6,    16,
      82,   312,   929,    54,    55,    13,   657,   658,   467,   437,
     438,    16,    69,    60,    61,    62,    63,   476,    25,   111,
      16,    22,    16,    17,    16,    17,    20,    29,    20,   952,
     367,     0,   496,   213,    25,   117,   500,   119,   121,   122,
     220,   770,    26,    26,   600,    57,   454,   330,    56,    91,
     333,   383,   335,   385,   337,    93,   339,   103,   267,    25,
     269,    61,    54,    25,    64,    65,    93,     5,     6,    25,
      25,   142,    57,   144,    82,    13,   216,   257,    91,   105,
      25,   540,   128,   121,    93,   111,   112,   227,    55,    18,
     394,    20,   105,  1020,   121,   304,   397,    60,   115,    80,
      63,   118,   119,   129,   105,   147,  1029,     5,     6,   121,
     115,   113,   121,   118,   119,    13,   116,   117,    56,   115,
     751,    28,   118,   119,    26,   221,   222,   140,   210,   146,
     488,   148,   138,   142,   147,   142,   437,   438,   123,   221,
     222,   146,   751,   148,    82,   126,   142,   110,   288,   800,
     146,   142,   148,   144,   138,   138,   121,   789,    56,   240,
     144,   770,   720,   795,    52,   240,   346,   144,   273,    91,
     296,   148,   277,    91,   314,   267,   142,   269,   304,   305,
     142,   273,   934,   309,    82,   312,   142,   142,   505,    91,
     548,   273,    91,   250,    91,   277,   376,   142,    72,   281,
     282,   294,   142,   105,   286,   542,   709,    62,   105,    64,
      65,   293,   294,   273,   855,   142,    93,   277,   679,   301,
     949,   148,   778,   952,   925,   147,   277,   538,   121,   147,
     312,   119,    26,    93,   317,    91,   138,   115,   140,   296,
     118,   119,   144,   140,   121,   147,   855,   144,   147,   105,
     147,   347,   348,   349,   350,   719,   720,   131,   132,   133,
     668,   116,   117,   546,   346,   347,   348,   349,   350,   351,
     352,   353,   392,    91,   582,   322,   395,   397,   293,   827,
     372,   273,   374,   277,   367,   277,   301,   369,    93,   600,
     144,   147,   472,   473,    75,   504,   505,    91,    93,   144,
    1029,   394,   125,   756,   312,    93,    93,   389,   451,   346,
     392,   105,   394,   395,    55,   397,   121,   437,   438,   121,
      91,    91,   427,    25,   783,    93,   121,   791,    51,   147,
      91,   346,    55,   121,   121,   427,   351,   142,    58,    59,
     949,   122,   522,   952,   138,   427,   140,   452,   706,   529,
     144,   709,    91,   147,   429,   437,   438,   432,   786,    91,
     452,   925,   467,   771,   792,   793,   730,   427,   460,   927,
     452,   476,   454,   455,   312,   467,   147,   147,   453,    16,
     494,   463,    26,   121,   476,   467,   147,   395,   597,   471,
     798,   496,   452,   468,   476,   500,   713,   494,   286,   481,
     392,   512,   477,   429,   142,   397,   294,   467,   147,   536,
      55,   538,   504,   505,   312,   147,   476,   870,   871,    20,
    1029,   513,   527,   142,   704,   530,   142,   453,   760,   761,
     762,   513,   764,    57,   766,   540,   844,   512,   647,   138,
     522,   516,   468,   751,    93,   496,   503,    91,   540,    72,
     653,   477,   145,   656,   536,    93,   538,   395,   540,   542,
      26,   105,   512,   927,   121,   547,   541,   778,   896,   141,
     610,   674,   121,   600,   139,   833,   527,    93,   115,   780,
     540,   118,   119,   121,    93,   786,   666,    55,    93,    93,
     516,   792,   793,   485,   138,   704,   140,   395,   513,   907,
     144,   593,    51,   147,   713,   121,   394,   522,    93,   146,
     142,   148,   121,    72,   628,   541,   121,   121,   600,    72,
     837,    93,    93,    72,   704,    91,   142,     2,   536,     4,
     538,   628,   121,   142,     9,    10,   121,   142,   142,   105,
      15,    16,    17,   851,    27,    20,   142,   367,  1001,   121,
     121,   859,    72,   102,   103,   647,  1015,    37,    38,   115,
     142,   653,   118,   119,   656,   657,   658,    58,    59,    89,
      90,   667,   138,    51,   140,   463,   121,    52,   144,   128,
     142,   147,   674,   471,   142,   667,   668,   679,   680,   700,
     682,    66,   600,   481,    51,   896,   677,   689,   536,   142,
     538,    51,   677,   699,   730,   937,   938,   939,   940,   129,
     130,   131,   132,   133,   719,   720,   142,   699,   644,    51,
      26,   713,   802,   796,   738,   700,   100,   653,   837,    15,
     656,   911,   115,    17,    18,   118,   119,   917,   536,    13,
     538,   949,   117,   644,   119,    16,   672,   827,   674,    63,
     700,   778,   653,   866,   867,   656,    15,    91,   145,   547,
     780,   145,   600,   146,   756,   148,   786,   787,   719,    74,
      75,   105,   792,   793,   139,   142,     9,    10,   783,   142,
      15,    15,    15,   142,    44,    91,   791,   679,   121,   771,
     141,   783,   141,  1025,   437,   438,   778,   779,   780,   105,
      15,   783,   600,   141,   786,   787,   140,    18,   800,   784,
     792,   793,   141,   147,  1022,   139,   798,   799,   810,   815,
      15,   813,   770,   783,    26,  1033,   469,   470,   139,   141,
     812,   911,   138,   815,   140,   210,   139,   917,   144,   148,
     791,   147,   824,   825,   142,   837,   221,   222,   568,    57,
     832,   142,   866,   867,   142,   142,   575,   142,   784,    14,
     579,    91,   844,   845,   779,   585,    15,    91,   588,   575,
     778,    91,    37,    38,   517,   105,   896,    94,   870,   871,
     145,   105,    15,    15,   117,   105,   868,   142,   146,    91,
     298,   873,    61,   142,   302,    64,    65,   142,   273,   142,
     142,   115,   277,   105,   118,   119,   281,   282,    15,   141,
     140,   286,    15,   139,   896,    15,   140,   147,   293,   294,
     140,    15,   927,   147,   906,   907,   301,   147,   910,   969,
      91,    15,   914,   925,   148,    91,   138,   142,   140,   139,
     778,   126,   126,    55,   105,   147,   139,   116,   117,   105,
     115,    55,    15,   118,   119,   142,    91,    15,   144,   142,
     868,    62,   142,    64,    65,   873,   142,   142,   982,   142,
     105,   346,   347,   348,   349,   350,   351,   352,   353,   140,
     778,   146,    91,   148,   140,   982,   147,   144,   221,   222,
     972,   147,   974,   141,   369,   977,   105,   142,   946,   513,
      13,   949,   910,     6,   952,   140,   954,   770,    72,  1001,
    1015,   799,   147,  1020,   389,   116,   117,   392,  1018,   394,
       7,   797,   397,  1015,   812,  1017,  1018,  1017,   575,   252,
     868,   140,   946,  1015,    -1,   873,   824,   825,   147,   949,
     268,  1016,    91,    -1,   832,    -1,    91,    -1,   281,   282,
     770,   770,   427,    -1,  1002,  1015,   105,   845,    -1,    -1,
     105,    -1,   437,   438,   770,   129,   130,   131,   132,   133,
     868,    -1,   910,    -1,    -1,   873,    -1,   452,    -1,   454,
     455,  1029,   425,  1031,    -1,  1033,   429,  1035,   463,   432,
    1016,   140,   467,   501,    -1,   140,   471,    -1,   147,    91,
     508,   476,   147,    63,    64,    65,   481,  1055,    -1,    -1,
     453,   519,   910,   105,   347,   348,   349,   350,   906,   352,
     353,    -1,    -1,   466,    -1,   468,   914,    51,    -1,    53,
      54,    55,    56,    -1,   477,   855,    72,   857,   513,    -1,
      -1,   861,    -1,   786,   787,    69,    -1,   522,   140,   792,
     793,    -1,    -1,    89,    90,   147,   116,   117,    -1,    -1,
      -1,   569,   570,     9,    10,   540,    -1,   875,   876,    15,
      16,    17,   547,   516,    20,   818,   819,    -1,   821,   822,
      -1,    -1,    -1,    -1,   972,    -1,   974,    -1,    -1,   977,
      -1,   599,   128,   129,   130,   131,   132,   133,   541,    -1,
      -1,    47,    48,    49,    50,    91,    -1,    -1,    54,    55,
      91,   115,   932,   933,   118,   119,    -1,    -1,   142,   105,
      66,    67,   455,    -1,   105,   115,    -1,   946,   118,   119,
     949,    -1,   952,   952,   954,   954,    63,    64,    65,    -1,
     946,   949,   146,   949,   148,    -1,   952,    51,   954,    53,
      54,    55,    56,   896,   140,   145,   146,    -1,   148,   140,
      -1,   147,    -1,    -1,    -1,    69,   147,    -1,   676,   989,
      -1,   117,   992,    -1,    -1,    -1,   919,    63,    64,    65,
      -1,    -1,    51,  1002,    53,    54,    55,    56,    -1,   116,
     117,    -1,   667,   668,    -1,    -1,  1002,  1005,  1006,  1007,
      69,  1009,  1010,    72,  1024,    -1,    -1,    -1,    -1,  1029,
    1029,  1031,  1031,    -1,  1033,  1035,  1035,    63,    64,    65,
      -1,   729,    -1,  1029,   699,  1031,    95,  1033,    -1,  1035,
     116,   117,   101,   102,   103,  1055,  1055,    -1,   142,   747,
      -1,  1049,  1050,  1051,  1052,    -1,    -1,    -1,    -1,  1055,
      -1,  1059,    -1,    63,    64,    65,    63,    64,    65,   128,
      -1,    -1,   131,    -1,    -1,    -1,    63,    64,    65,    -1,
     116,   117,    -1,   142,    -1,   221,   222,    -1,    -1,    63,
      64,    65,    51,    -1,    53,    54,    55,    56,     2,    -1,
       4,     5,     6,    -1,    -1,    -1,   771,    -1,    -1,    13,
      69,    -1,    -1,    -1,   779,   780,   116,   117,   783,   116,
     117,   786,   787,   259,   260,   261,   262,   792,   793,   116,
     117,    -1,    -1,   798,   799,    -1,    95,   273,   836,    -1,
      -1,   277,   116,   117,   667,   281,   282,   812,    52,   782,
     815,   784,    56,    51,   852,    53,    54,    55,    56,   824,
     825,    -1,    -1,    -1,    72,    -1,    -1,   832,    -1,   115,
      -1,    69,   118,   119,    72,    -1,   699,    -1,    82,   844,
     845,    89,    90,    40,    41,    42,    43,    44,    51,    -1,
      53,    54,    55,    56,    -1,    -1,   142,    95,    -1,    -1,
     146,    -1,   148,   101,   102,   103,    69,    -1,    -1,    -1,
      -1,   347,   348,   349,   350,   119,   352,   353,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
     128,   896,    95,   131,   370,    -1,    -1,    -1,   101,    -1,
      -1,   906,   907,    -1,    -1,   381,   144,    -1,    -1,   914,
      -1,    -1,    -1,    -1,    -1,    -1,   392,    -1,    -1,    -1,
      -1,   397,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,    -1,   414,   415,
     416,   417,   418,   419,   420,   421,   422,   423,   424,    -1,
      -1,   427,   815,    -1,    -1,    -1,    -1,     2,    -1,     4,
      -1,   437,   438,    -1,    -1,    -1,   210,   972,    13,   974,
      -1,    -1,   977,    -1,    -1,    -1,   452,    -1,    -1,   455,
      -1,     2,    -1,     4,     5,     6,     7,    -1,    -1,   465,
      -1,   467,    13,   469,   470,    -1,    -1,    -1,    -1,    -1,
     476,    51,    -1,    53,    54,    55,    56,    52,    -1,   485,
    1015,    -1,    -1,   489,    -1,    -1,    -1,   493,    -1,    69,
     496,    -1,   498,    -1,   500,   501,    -1,    -1,    -1,    -1,
      -1,    52,    -1,    -1,    51,    56,    53,    54,    55,    56,
      -1,   517,   286,  1016,    -1,    -1,    -1,    -1,    -1,   293,
     294,   527,    69,    -1,   530,    72,    -1,   301,    -1,    -1,
      -1,    82,    -1,    -1,   540,    -1,    -1,    -1,   312,    -1,
      -1,    -1,    -1,    -1,   119,    -1,    -1,    -1,    95,    -1,
     556,   557,    -1,    -1,   101,   102,   103,    51,    -1,    53,
      54,    55,    56,    -1,    -1,    -1,   572,    -1,   119,    -1,
      -1,    -1,   346,    -1,    -1,    69,    -1,   351,    72,    -1,
      -1,   128,    -1,    -1,   131,   591,    -1,    -1,   594,    -1,
      -1,    -1,    86,    -1,    -1,   369,    -1,   144,    -1,    -1,
      -1,    95,    -1,    -1,    -1,    -1,    -1,   101,   102,   103,
      -1,    -1,    -1,    -1,    -1,   389,    -1,    -1,    -1,    -1,
     394,   395,    -1,   397,    -1,    51,    -1,    53,    54,    55,
      56,    -1,    -1,    -1,   128,   210,    -1,   131,    -1,    -1,
       2,    -1,     4,    69,    -1,    -1,    72,    51,    -1,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,   210,
      86,   667,    -1,   437,   438,    69,    -1,    -1,    72,    95,
      -1,    -1,    -1,   679,    -1,   101,   102,   103,    -1,    -1,
     454,    -1,    -1,    51,    -1,    53,    54,    55,    56,   463,
      52,    95,    -1,   699,    -1,    -1,    -1,   471,   102,   103,
      -1,    69,   128,    -1,   710,   131,    -1,   481,    -1,    -1,
      -1,   286,    -1,   719,   720,    -1,    -1,    -1,   293,   294,
      -1,    -1,    -1,    -1,   128,    -1,   301,    95,    -1,    -1,
      -1,    -1,    -1,   101,    -1,   286,    -1,    -1,    -1,   513,
      -1,    -1,   293,   294,    -1,    -1,    -1,    -1,   522,    -1,
     301,    -1,    -1,    -1,    -1,    -1,    -1,   119,    -1,    -1,
      -1,   312,   536,    -1,   538,    -1,    -1,    -1,    -1,   775,
      -1,   346,    -1,   547,   780,   781,   351,   783,    -1,    -1,
     786,   787,    -1,    -1,    -1,   791,   792,   793,    -1,    -1,
      -1,    -1,    -1,    -1,   369,   346,    -1,    -1,    -1,    -1,
     351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   815,
      -1,    -1,   818,   819,   389,   821,   822,    -1,   369,   394,
      -1,    -1,   397,    -1,    -1,   831,   600,    -1,    -1,    -1,
     836,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   389,    -1,
      -1,    -1,    -1,   394,   395,    -1,    -1,   853,   210,    51,
      -1,    53,    54,    55,    56,    -1,    -1,    -1,   864,    -1,
      -1,    -1,   437,   438,    -1,    -1,    -1,    69,    -1,    -1,
      72,    -1,    -1,   879,    -1,    -1,    -1,    -1,    -1,   454,
      -1,    -1,    -1,   889,   890,    -1,    -1,    -1,   463,    -1,
     896,    -1,    -1,    95,   668,    -1,   471,    -1,    -1,   101,
     102,   103,    -1,   454,    -1,    -1,   481,    -1,    -1,    -1,
      -1,    -1,   463,   919,    -1,    -1,    -1,    -1,    -1,    -1,
     471,   927,    -1,    -1,   286,    -1,   128,    -1,    -1,   131,
     481,   293,   294,    -1,    -1,    -1,    -1,    -1,   513,   301,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   522,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   522,   547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   346,   536,    -1,   538,    -1,   351,
      -1,    -1,    -1,    -1,    -1,    -1,   547,   771,    -1,    -1,
      -1,    -1,    -1,    -1,   778,   779,   780,   369,    -1,  1015,
      -1,    -1,   786,    -1,    -1,    -1,    -1,    -1,   792,   793,
      -1,    -1,    -1,    -1,   798,   799,    -1,   389,    -1,    -1,
      -1,    -1,   394,    -1,    -1,   397,    -1,    -1,   812,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   600,
     824,   825,    -1,    -1,    -1,    -1,    -1,    -1,   832,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     844,   845,    -1,    -1,    -1,   437,   438,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   454,   668,   868,    -1,    -1,    -1,    -1,   873,
      -1,   463,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   471,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   668,    -1,   481,
      -1,    -1,   896,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   906,   907,    -1,    -1,   910,    -1,    -1,    -1,
     914,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   547,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   771,    -1,   972,    -1,
     974,    -1,    -1,   977,   779,   780,    -1,    -1,    -1,    -1,
      -1,   786,    -1,    -1,    -1,    -1,    -1,   792,   793,    -1,
     771,    -1,    -1,   798,   799,    -1,    -1,   778,   779,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   798,   799,   824,
     825,    -1,    -1,    -1,    -1,    -1,    -1,   832,    -1,    -1,
      -1,   812,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   844,
     845,    -1,    -1,   824,   825,    -1,    -1,    -1,    -1,    -1,
      -1,   832,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   844,   845,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   668,    -1,    -1,    -1,
       0,    -1,    -1,    -1,    -1,    -1,    -1,   868,    -1,    -1,
      -1,   896,   873,    13,    14,    15,    16,    17,    18,    -1,
      20,   906,   907,    -1,    -1,   910,    26,    27,    -1,   914,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,   906,   907,    -1,    -1,   910,
      -1,    -1,    -1,   914,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    -1,    81,    82,   972,    -1,   974,
      -1,    91,   977,    89,    90,    -1,    -1,    -1,    -1,   771,
      -1,    -1,    -1,    -1,    -1,   105,    -1,   779,   780,    -1,
      -1,   972,    -1,   974,   786,   115,   977,    -1,   118,   119,
     792,   793,    -1,    -1,    -1,    -1,   798,   799,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,   138,   139,
     812,    -1,    -1,    -1,   144,   145,   146,   147,   148,    -1,
      -1,    -1,   824,   825,    -1,    -1,    -1,    -1,    -1,    -1,
     832,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   844,   845,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   896,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   906,   907,    -1,    -1,    -1,    -1,
      -1,   122,   914,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,     0,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
     972,    39,   974,    -1,    -1,   977,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,     0,    -1,   114,   115,    -1,    -1,
     118,   119,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,   134,   135,   136,    -1,
      -1,    27,    28,    29,    -1,    -1,    -1,    -1,   146,    -1,
     148,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    -1,    -1,    89,    90,    91,    -1,    -1,    94,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,
      -1,    -1,   118,   119,    -1,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
       0,    -1,    -1,   139,   140,   141,   142,    -1,    -1,   145,
     146,   147,   148,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    -1,    26,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,
      90,    91,    -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,
     100,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,   138,   139,
     140,   141,   142,    -1,   144,   145,   146,   147,   148,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    -1,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    -1,    -1,    89,    90,    91,    -1,    93,
      94,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,   121,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,     0,    -1,    -1,   139,   140,   141,   142,    -1,
      -1,   145,   146,   147,   148,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    26,    27,
      28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    -1,
      -1,    89,    90,    91,    -1,    -1,    94,    -1,    -1,    -1,
      -1,    -1,   100,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,     0,    -1,
     138,   139,   140,   141,   142,    -1,   144,   145,   146,   147,
     148,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,    91,
      -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,     0,    -1,    -1,   139,   140,   141,
     142,    -1,   144,   145,   146,   147,   148,    13,    14,    15,
      -1,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,
      26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    -1,    -1,    89,    90,    91,    -1,    93,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,   121,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
       0,    -1,   138,   139,   140,    -1,   142,    -1,    -1,   145,
     146,   147,   148,    13,    14,    15,    -1,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,
      90,    91,    -1,    93,    94,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,
      -1,   121,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,   138,   139,
     140,    -1,   142,    -1,    -1,   145,   146,   147,   148,    13,
      14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    -1,    -1,    89,    90,    91,    -1,    93,
      94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,   121,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,     0,    -1,    -1,   139,   140,    -1,   142,    -1,
      -1,   145,   146,   147,   148,    13,    14,    15,    -1,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    -1,
      -1,    89,    90,    91,    -1,    93,    94,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    -1,   121,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,   139,   140,    -1,   142,    -1,    -1,   145,   146,   147,
     148,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    -1,    -1,    18,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,   101,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,   115,    -1,    -1,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   146,     1,   148,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    -1,    17,    18,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,   101,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,     1,   148,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    -1,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,   115,    -1,    -1,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,     1,
     148,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    -1,    -1,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,   115,    -1,    -1,   118,   119,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,   146,    -1,   148,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,
      -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,    -1,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,    -1,
      -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   134,   135,   136,    -1,    -1,   139,    -1,    -1,    -1,
      -1,    -1,    -1,   146,     1,   148,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    14,    15,    -1,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,   115,    -1,
      -1,   118,   119,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,   134,   135,   136,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,   146,
      -1,   148,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,   115,    -1,    -1,
     118,   119,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,   134,   135,   136,    -1,
      19,    -1,    21,    22,    23,    24,    -1,   145,   146,    -1,
     148,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,
      99,    -1,   101,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,    -1,    -1,   114,   115,    -1,    -1,   118,
     119,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,   134,   135,   136,    -1,    19,
      -1,    21,    22,    23,    24,    -1,   145,   146,    -1,   148,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,   101,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,   115,    -1,    -1,   118,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,   139,
      -1,    -1,    -1,    -1,    -1,    -1,   146,     1,   148,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    -1,    -1,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,   101,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,   115,    -1,    -1,   118,   119,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
     134,   135,   136,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,   146,    -1,   148,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,   101,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
     115,    -1,    -1,   118,   119,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,   134,
     135,   136,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,   146,    -1,   148,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,
      96,    -1,    98,    99,    -1,   101,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,    -1,    -1,   114,   115,
      -1,    -1,   118,   119,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,   134,   135,
     136,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
     146,    -1,   148,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,   115,    -1,
      -1,   118,   119,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,   134,   135,   136,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,   146,
      -1,   148,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,   115,    -1,    -1,
     118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   146,    -1,
     148,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,   143,   144,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,   101,   102,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,   135,   136,    -1,   138,    -1,
      -1,    -1,    -1,    -1,   144,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,   101,   102,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,
     138,    21,    22,    23,    24,    -1,   144,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    93,    -1,    95,    96,    -1,    98,    99,
      -1,   101,   102,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,   121,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,   134,   135,   136,    -1,    19,    -1,
      21,    22,    23,    24,   144,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,
      -1,    92,    93,    -1,    95,    96,    -1,    98,    99,    -1,
     101,   102,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
     121,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,
      22,    23,    24,   144,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,
     102,   103,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,   144,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,   101,   102,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,
      -1,   143,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   110,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,   143,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,    -1,    -1,   106,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,   143,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    -1,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    -1,    -1,
      -1,    -1,    -1,    -1,   143,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,   134,    21,    22,    23,
      24,    -1,    -1,    -1,   142,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     134,    21,    22,    23,    24,    -1,    -1,    -1,   142,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,   101,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,   101,   102,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     134,   135,   136,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,
      96,    -1,    98,    99,    -1,   101,   102,   103,   104,    -1,
     106,   107,   108,   109,   110,   111,    -1,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,
     136,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,   101,   102,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,   101,   102,   103,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,
     102,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,    -1,   102,   103,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     134,   135,   136,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,
      96,    -1,    98,    99,    -1,   101,   102,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,    -1,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,
     136,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,    -1,   102,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,   101,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,   101,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     134,   135,   136,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,
      96,    -1,    98,    99,    -1,   101,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,    -1,    -1,   114,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,
     136,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     134,   135,   136,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    89,    -1,    -1,    92,    -1,    -1,    95,
      96,    -1,    98,    99,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,    -1,    -1,   114,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   134,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,
      98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,   134,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     134,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,    -1,    -1,   114,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,   134,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,
      96,    -1,    98,    99,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,    -1,    -1,   114,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,    51,
      52,    -1,   104,    55,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,   134,    -1,    -1,    87,    88,    89,    90,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,   101,
     102,    81,    82,    -1,    -1,    -1,    -1,    -1,    -1,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    51,    52,    -1,    -1,    55,
      -1,   143,   144,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,
      -1,    87,    88,    89,    90,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,   101,   102,    81,    82,    -1,
      -1,    -1,    -1,    -1,    -1,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    51,    52,    -1,
      -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    -1,    -1,    87,    88,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    51,
      52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    51,    52,    -1,    -1,    55,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,
      -1,    87,    88,    89,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    51,    52,    -1,
      -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    -1,    -1,    87,    88,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    51,
      52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    51,    52,    -1,    -1,    55,
      -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,
      -1,    87,    88,    89,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    51,    52,    -1,
      -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    -1,    -1,    87,    88,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,   143,
     144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    51,
      52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
     102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,   135,   136,    44,    -1,    -1,    -1,    -1,
      -1,   143,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,
      90,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,
      90,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,   148,    -1,    -1,
      89,    90,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   142,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,
      89,    90,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   142,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   142,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,
      89,    90,    -1,    -1,    -1,    94,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    -1,    -1,    89,    90,    -1,    -1,    -1,    94,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    -1,    -1,    89,    90,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   150,   151,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    19,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      58,    59,    60,    63,    66,    67,    69,    70,    71,    85,
      86,    92,    95,    96,    98,    99,   101,   104,   106,   107,
     108,   109,   110,   111,   114,   134,   135,   136,   152,   153,
     154,   159,   161,   163,   164,   165,   168,   169,   172,   173,
     175,   176,   177,   179,   180,   189,   203,   220,   241,   242,
     252,   253,   254,   258,   259,   260,   266,   267,   268,   270,
     271,   272,   273,   274,   275,   311,   324,   154,    21,    22,
      30,    31,    32,    39,    51,    55,    69,    89,    92,    95,
     134,   164,   165,   181,   182,   203,   220,   272,   275,   311,
     182,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    55,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    81,    82,    87,    88,    89,    90,   101,
     102,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   135,   136,   143,   144,   183,   187,   188,   274,   305,
     204,    92,   163,   167,   180,   189,   220,   272,   273,   275,
     167,   210,   212,    69,    92,   173,   180,   220,   225,   272,
     275,    33,    34,    35,    36,    48,    49,    50,    51,    55,
     106,   183,   184,   185,   268,   115,   118,   119,   146,   148,
     167,   262,   263,   264,   317,   321,   322,   323,    51,   101,
     102,   103,   135,   172,   189,   195,   198,   201,   254,   308,
     310,   195,   195,   144,   192,   193,   196,   197,   324,   192,
     196,   144,   318,   322,   184,   155,   138,   189,   220,   189,
     189,   189,    55,     1,    95,   157,   158,   159,   174,   175,
     324,   205,   207,   190,   201,   308,   324,   189,   307,   308,
     324,    92,   142,   179,   220,   272,   275,   208,    53,    54,
      56,    63,   110,   183,   269,    62,    64,    65,   116,   117,
     255,   256,    63,   255,    63,   255,    63,   255,    61,   255,
      58,    59,   168,   189,   189,   317,   323,    40,    41,    42,
      43,    44,    37,    38,    51,    53,    54,    55,    56,    69,
      72,    95,   101,   102,   103,   128,   131,   144,   278,   279,
     280,   281,   284,   285,   286,   287,   289,   290,   291,   292,
     294,   295,   296,   299,   300,   301,   302,   303,   278,   279,
      28,   239,   121,   142,    95,   101,   176,   121,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    89,    90,    94,   122,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    91,   105,   140,   147,   315,
      91,   315,   316,    26,   138,   243,   254,    93,    93,   192,
     196,   243,   163,    51,    55,   181,    58,    59,   278,   125,
     276,    91,   140,   315,   219,   306,    91,   147,   314,   156,
     157,    55,    16,   221,   321,   121,    91,   140,   315,    93,
      93,   221,   167,   167,    55,    91,   140,   315,    25,   110,
     142,   265,   317,   115,   264,    20,   246,   321,    57,   309,
     189,   189,   189,    94,   142,   199,   200,   324,   309,   199,
     200,    86,   194,   195,   201,   308,   324,   195,   163,   317,
     319,   163,   160,   138,   157,    91,   315,    93,   159,   174,
     145,   317,   323,   319,   159,   319,   141,   200,   320,   323,
     200,   320,   139,   320,    55,   176,   177,   178,   142,    91,
     140,   315,   144,   237,   289,    63,   255,   257,   261,   262,
      62,   256,    63,    63,    63,    61,    72,    72,   154,   167,
     167,   167,   167,   159,   163,   163,    57,   121,   293,    86,
     289,   294,   121,   156,   189,   142,   304,   324,    51,   142,
     304,   321,   142,   288,   189,   142,   288,    51,   142,   288,
      51,   121,   156,   240,   101,   168,   189,   201,   202,   174,
     142,   179,   142,   161,   162,   168,   180,   189,   191,   202,
     220,   275,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,    51,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,    51,
      52,    55,   187,   192,   312,   313,   194,   201,    51,    52,
      55,   187,   192,   312,    51,    55,   312,   245,   244,   162,
     189,   191,   162,   191,   100,   170,   217,   277,   216,    51,
      55,   181,   312,   194,   312,   156,   163,   166,    15,    13,
     248,   324,   157,    16,    51,    55,   194,    51,    55,   157,
      27,   222,   321,   222,    51,    55,   194,    51,    55,   214,
     186,   157,   246,   189,   201,    15,   261,   189,   189,   318,
     101,   189,   198,   308,   189,   310,   319,   145,   317,   200,
     200,   319,   145,   184,   152,   139,   191,   319,   159,   206,
     308,   176,   178,    51,    55,   194,    51,    55,   289,   209,
      63,   157,   262,   189,   189,    51,   101,   226,   294,   319,
     319,   142,   189,    15,    51,   281,   286,   303,   287,   292,
     299,   301,   294,   296,   301,    51,   294,   189,    15,    80,
     126,   231,   232,   324,   189,   200,   319,   178,   142,    44,
     121,    44,    91,   140,   315,   318,    93,    93,   192,   196,
     141,   200,    93,    93,   193,   196,   193,   196,   231,   231,
     171,   321,   167,   156,   141,    15,   319,   183,   189,   202,
     249,   324,    18,   224,   324,    17,   223,   224,    93,    93,
     141,    93,    93,   224,   211,   213,   141,   167,   184,   139,
      15,   200,   221,   261,   189,   199,    86,   308,   139,   319,
     320,   141,   234,   318,    29,   113,   238,   139,   142,   291,
     319,   142,    86,    44,   304,   142,   288,   142,   288,   142,
     288,   142,   288,   288,    44,   228,   230,   233,   280,   282,
     283,   286,   294,   295,   297,   298,   301,   303,   156,   101,
     189,   178,   159,   189,    51,    55,   194,    51,    55,    57,
     123,   162,   191,   168,   191,   170,    93,   162,   191,   162,
     191,   170,   243,   239,   156,   157,   231,   218,   321,    15,
      94,   250,   324,   157,    14,   251,   324,   167,    15,    93,
      15,   157,   157,   222,   189,   157,   319,   200,   145,   146,
     156,   157,   227,   142,   101,   319,   189,   294,   301,   294,
     294,   189,   234,   234,    92,   220,   142,   304,   304,   142,
     229,   220,   142,   229,   142,   229,    15,   189,   141,   189,
     189,   162,   191,    15,   139,   157,   156,    92,   180,   220,
     272,   275,   221,   157,   221,    15,    15,   215,   224,   246,
     247,    51,   235,   236,   290,    15,   139,   294,   294,   142,
     291,   288,   142,   288,   288,   288,   126,   126,    55,    91,
     282,   286,   142,   228,   229,   298,   301,   294,   297,   301,
     294,   139,    15,    55,    91,   140,   315,   157,   157,   157,
     142,   318,   142,   294,   142,   294,    51,    55,   304,   142,
     229,   142,   229,   142,   229,   142,   229,   229,    51,    55,
     194,    51,    55,   248,   223,    15,   236,   294,   288,   294,
     301,   294,   294,   141,   229,   142,   229,   229,   229,   294,
     229
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   149,   151,   150,   152,   153,   153,   153,   153,   154,
     155,   154,   156,   157,   158,   158,   158,   158,   160,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   159,   161,   161,   161,   161,   161,   161,
     161,   161,   162,   162,   162,   163,   163,   163,   163,   163,
     163,   164,   166,   165,   167,   168,   168,   169,   169,   171,
     170,   172,   172,   172,   172,   172,   172,   172,   172,   172,
     172,   172,   173,   173,   174,   174,   175,   175,   175,   175,
     175,   175,   175,   175,   175,   175,   176,   176,   177,   177,
     178,   178,   179,   179,   179,   179,   179,   179,   179,   179,
     180,   180,   180,   180,   180,   180,   180,   180,   180,   181,
     181,   182,   182,   182,   183,   183,   183,   183,   183,   184,
     184,   185,   186,   185,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   190,   190,   190,   190,   191,
     191,   192,   192,   192,   193,   193,   194,   194,   194,   194,
     194,   195,   195,   195,   195,   195,   197,   196,   198,   199,
     199,   200,   200,   201,   201,   201,   201,   202,   202,   202,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   204,
     203,   205,   206,   203,   207,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   208,   209,
     203,   203,   203,   210,   211,   203,   212,   213,   203,   203,
     203,   214,   215,   203,   216,   203,   217,   218,   203,   219,
     203,   203,   203,   203,   203,   203,   203,   220,   221,   221,
     221,   222,   222,   223,   223,   224,   224,   225,   225,   226,
     226,   226,   226,   226,   226,   226,   226,   227,   226,   228,
     228,   228,   228,   229,   229,   230,   230,   230,   230,   230,
     230,   230,   230,   230,   230,   230,   230,   230,   230,   230,
     231,   231,   233,   232,   232,   232,   234,   234,   235,   235,
     236,   236,   237,   237,   238,   238,   240,   239,   241,   241,
     241,   241,   242,   242,   242,   242,   242,   242,   242,   242,
     242,   244,   243,   245,   243,   246,   247,   247,   248,   248,
     249,   249,   249,   250,   250,   251,   251,   252,   252,   252,
     252,   253,   253,   254,   254,   254,   254,   255,   255,   256,
     257,   256,   256,   256,   258,   258,   259,   259,   260,   261,
     261,   262,   262,   263,   263,   264,   265,   264,   266,   266,
     267,   267,   268,   269,   269,   269,   269,   269,   269,   270,
     270,   271,   271,   271,   271,   272,   272,   272,   272,   272,
     273,   273,   274,   274,   274,   274,   274,   274,   274,   274,
     275,   275,   276,   277,   276,   278,   278,   278,   279,   279,
     280,   281,   281,   282,   282,   283,   283,   284,   284,   285,
     285,   286,   286,   287,   287,   287,   287,   288,   288,   289,
     289,   289,   289,   289,   289,   289,   289,   289,   289,   289,
     289,   289,   289,   289,   290,   290,   290,   290,   290,   291,
     291,   292,   293,   292,   294,   294,   295,   296,   297,   298,
     298,   299,   299,   300,   300,   301,   301,   302,   302,   303,
     304,   304,   305,   306,   305,   307,   307,   308,   308,   309,
     309,   310,   310,   310,   310,   311,   311,   311,   312,   312,
     312,   312,   313,   313,   313,   314,   314,   315,   315,   316,
     316,   317,   317,   318,   318,   319,   320,   320,   320,   321,
     321,   321,   322,   323,   323,   324
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     3,     1,     3,     3,     6,     5,     5,     5,
       5,     3,     1,     3,     1,     1,     3,     3,     3,     2,
       1,     2,     0,     5,     1,     1,     1,     1,     4,     0,
       5,     2,     3,     4,     5,     4,     5,     2,     2,     2,
       2,     2,     1,     3,     1,     3,     1,     2,     3,     5,
       2,     4,     2,     4,     1,     3,     1,     3,     2,     3,
       1,     2,     1,     4,     3,     3,     3,     3,     2,     1,
       1,     4,     3,     3,     3,     3,     2,     1,     1,     1,
       1,     2,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     3,     6,     5,     5,     5,
       5,     4,     3,     3,     2,     2,     3,     2,     2,     3,
       3,     3,     3,     3,     3,     4,     4,     2,     2,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     3,     3,     3,     3,     6,     6,
       4,     6,     4,     6,     1,     1,     2,     4,     2,     1,
       3,     3,     5,     3,     1,     1,     1,     2,     2,     4,
       2,     1,     2,     2,     4,     1,     0,     2,     2,     2,
       1,     1,     3,     1,     2,     3,     4,     3,     4,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       4,     0,     0,     5,     0,     3,     3,     3,     2,     3,
       3,     1,     2,     4,     3,     2,     1,     2,     0,     0,
       5,     6,     6,     0,     0,     7,     0,     0,     7,     5,
       4,     0,     0,     9,     0,     6,     0,     0,     8,     0,
       5,     4,     4,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     1,     1,     5,     1,     2,     1,     1,     1,
       4,     6,     3,     5,     2,     4,     1,     0,     4,     4,
       2,     2,     1,     2,     0,     6,     8,     4,     6,     4,
       3,     6,     2,     4,     6,     2,     4,     2,     4,     1,
       1,     1,     0,     4,     1,     4,     1,     4,     1,     3,
       1,     1,     4,     1,     3,     3,     0,     5,     2,     4,
       5,     5,     2,     4,     4,     3,     3,     3,     2,     1,
       4,     0,     5,     0,     5,     5,     1,     1,     6,     1,
       1,     1,     1,     2,     1,     2,     1,     1,     1,     1,
       1,     1,     2,     1,     1,     2,     3,     1,     2,     1,
       0,     4,     1,     2,     2,     3,     2,     3,     1,     1,
       2,     1,     2,     1,     2,     1,     0,     4,     2,     3,
       1,     4,     2,     1,     1,     1,     1,     1,     2,     2,
       3,     1,     1,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     0,     4,     3,     5,     3,     1,     2,
       2,     2,     1,     2,     1,     1,     3,     1,     3,     1,
       1,     2,     1,     4,     2,     2,     1,     2,     0,     6,
       8,     4,     6,     4,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     1,     3,     2,     2,     2,     1,
       3,     1,     3,     1,     1,     2,     1,     1,     1,     2,
       2,     1,     1,     0,     4,     1,     2,     1,     3,     1,
       2,     3,     3,     3,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     0,     1,     2,     0,     1,     1,     1,
       1,     1,     1,     1,     2,     0
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


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
        yyerror (p, YY_("syntax error: cannot back up")); \
        YYERROR;                                                  \
      }                                                           \
  while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



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

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value, p); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (p);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep, parser_state *p)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep, p);
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule, parser_state *p)
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
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
                                              , p);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, p); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
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


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
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
#  endif
# endif

# ifndef yytnamerr
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
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

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
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
      yyarg[yycount++] = yytname[yytoken];
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
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
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
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
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
          yyp += yytnamerr (yyp, yyarg[yyi++]);
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
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, parser_state *p)
{
  YYUSE (yyvaluep);
  YYUSE (p);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/*----------.
| yyparse.  |
`----------*/

int
yyparse (parser_state *p)
{
/* The lookahead symbol.  */
int yychar;


/* The semantic value of the lookahead symbol.  */
/* Default value used for initialization, for pacifying older GCCs
   or non-GCC compilers.  */
YY_INITIAL_VALUE (static YYSTYPE yyval_default;)
YYSTYPE yylval YY_INITIAL_VALUE (= yyval_default);

    /* Number of syntax errors so far.  */
    int yynerrs;

    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
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

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
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

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

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

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex (&yylval, p);
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
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


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
  case 2:
#line 1537 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 5879 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3:
#line 1542 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 5888 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4:
#line 1549 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5896 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5:
#line 1555 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5904 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6:
#line 1559 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7:
#line 1564 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8:
#line 1568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5929 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10:
#line 1575 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 5938 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11:
#line 1580 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 5949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12:
#line 1592 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd)) {
                        (yyval.nd) = new_rescue(p, (yyvsp[-3].nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                        NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                      }
                      else if ((yyvsp[-1].nd)) {
                        yywarn(p, "else without rescue is useless");
                        (yyval.nd) = push((yyvsp[-3].nd), (yyvsp[-1].nd));
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
#line 5975 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13:
#line 1616 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5983 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14:
#line 1622 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5991 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15:
#line 1626 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6000 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16:
#line 1631 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6008 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17:
#line 1635 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 6016 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18:
#line 1640 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 6022 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19:
#line 1641 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 6030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20:
#line 1645 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6038 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21:
#line 1649 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6046 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22:
#line 1653 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6054 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23:
#line 1657 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6062 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24:
#line 1661 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6070 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25:
#line 1665 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26:
#line 1669 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 6087 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28:
#line 1675 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6095 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29:
#line 1679 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6103 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30:
#line 1683 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6111 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31:
#line 1687 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6119 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 32:
#line 1691 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *lhs = new_lvar(p, (yyvsp[0].id));
                      void_expr_error(p, (yyvsp[-2].nd));
                      assignable(p, lhs);
                      (yyval.nd) = new_asgn(p, lhs, (yyvsp[-2].nd));
                    }
#line 6130 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34:
#line 1701 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6138 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35:
#line 1705 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6146 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36:
#line 1709 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6154 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37:
#line 1713 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6162 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38:
#line 1717 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6170 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39:
#line 1721 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 6179 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40:
#line 1726 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6187 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41:
#line 1730 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43:
#line 1738 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 46:
#line 1747 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6212 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47:
#line 1751 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6220 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 48:
#line 1755 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 49:
#line 1759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6236 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51:
#line 1767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 6247 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52:
#line 1776 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 6255 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53:
#line 1780 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 6268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 54:
#line 1791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 6279 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58:
#line 1805 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6287 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 59:
#line 1811 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 6296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 60:
#line 1818 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 6306 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 61:
#line 1826 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6314 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62:
#line 1830 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 6323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63:
#line 1835 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6331 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64:
#line 1839 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 6340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65:
#line 1844 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 6348 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66:
#line 1848 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 6357 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67:
#line 1853 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 6365 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68:
#line 1857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6373 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69:
#line 1861 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6381 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70:
#line 1865 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6389 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71:
#line 1869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6397 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72:
#line 1875 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6405 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73:
#line 1879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6413 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75:
#line 1886 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76:
#line 1892 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6429 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77:
#line 1896 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 6437 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 78:
#line 1900 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6445 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79:
#line 1904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6453 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80:
#line 1908 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 6461 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81:
#line 1912 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 6469 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82:
#line 1916 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 6477 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83:
#line 1920 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6485 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84:
#line 1924 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 6493 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85:
#line 1928 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 6501 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87:
#line 1935 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 6509 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88:
#line 1941 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 6517 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89:
#line 1945 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 6525 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 90:
#line 1951 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6533 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91:
#line 1955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 6541 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92:
#line 1961 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6549 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93:
#line 1965 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 6557 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94:
#line 1969 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6565 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95:
#line 1973 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6573 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96:
#line 1977 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6581 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97:
#line 1981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6591 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98:
#line 1987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6601 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99:
#line 1993 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6610 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100:
#line 2000 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6618 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101:
#line 2004 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 6626 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102:
#line 2008 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6634 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103:
#line 2012 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6642 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104:
#line 2016 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6650 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105:
#line 2020 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6660 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106:
#line 2026 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107:
#line 2032 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6679 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108:
#line 2037 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 6687 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109:
#line 2043 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 6695 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111:
#line 2050 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(nint(1), nsym((yyvsp[0].id)));
                    }
#line 6703 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112:
#line 2054 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(nint(0), nsym((yyvsp[0].id)));
                    }
#line 6711 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113:
#line 2058 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 6720 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117:
#line 2068 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6729 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 118:
#line 2073 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6738 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121:
#line 2084 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 6746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122:
#line 2087 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 6752 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 123:
#line 2088 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 6760 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 124:
#line 2093 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(or);     }
#line 6766 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125:
#line 2094 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(xor);    }
#line 6772 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126:
#line 2095 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(and);    }
#line 6778 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127:
#line 2096 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(cmp);    }
#line 6784 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128:
#line 2097 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eq);     }
#line 6790 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129:
#line 2098 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eqq);    }
#line 6796 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130:
#line 2099 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(match);  }
#line 6802 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131:
#line 2100 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(nmatch); }
#line 6808 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132:
#line 2101 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(gt);     }
#line 6814 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133:
#line 2102 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(ge);     }
#line 6820 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134:
#line 2103 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lt);     }
#line 6826 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135:
#line 2104 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(le);     }
#line 6832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136:
#line 2105 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neq);    }
#line 6838 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137:
#line 2106 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lshift); }
#line 6844 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138:
#line 2107 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(rshift); }
#line 6850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139:
#line 2108 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(add);    }
#line 6856 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140:
#line 2109 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(sub);    }
#line 6862 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141:
#line 2110 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 6868 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142:
#line 2111 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 6874 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143:
#line 2112 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(div);    }
#line 6880 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144:
#line 2113 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mod);    }
#line 6886 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145:
#line 2114 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 6892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146:
#line 2115 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 6898 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147:
#line 2116 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(not);    }
#line 6904 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148:
#line 2117 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neg);    }
#line 6910 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149:
#line 2118 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(plus);   }
#line 6916 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150:
#line 2119 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(minus);  }
#line 6922 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151:
#line 2120 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aref);   }
#line 6928 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152:
#line 2121 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aset);   }
#line 6934 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153:
#line 2122 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(tick);   }
#line 6940 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 194:
#line 2140 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6948 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 195:
#line 2144 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 196:
#line 2148 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 197:
#line 2152 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198:
#line 2156 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199:
#line 2160 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200:
#line 2164 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201:
#line 2169 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202:
#line 2174 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7015 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203:
#line 2179 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7023 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204:
#line 2183 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7031 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205:
#line 2187 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206:
#line 2191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7047 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207:
#line 2195 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7055 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208:
#line 2199 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7063 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209:
#line 2203 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 7071 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210:
#line 2207 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 7079 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211:
#line 2211 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 7087 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212:
#line 2215 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 7095 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213:
#line 2219 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 7103 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214:
#line 2223 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 7111 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215:
#line 2227 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7119 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216:
#line 2231 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7127 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217:
#line 2235 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 7135 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218:
#line 2239 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "-@");
                    }
#line 7143 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219:
#line 2243 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 7151 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220:
#line 2247 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 7159 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221:
#line 2251 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 7167 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222:
#line 2255 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 7175 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223:
#line 2259 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 7183 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224:
#line 2263 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 7191 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225:
#line 2267 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 7199 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226:
#line 2271 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 7207 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227:
#line 2275 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 7215 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228:
#line 2279 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 7223 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229:
#line 2283 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 7231 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230:
#line 2287 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 7239 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231:
#line 2291 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 7247 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232:
#line 2295 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7255 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233:
#line 2299 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 7263 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234:
#line 2303 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 7271 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235:
#line 2307 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 7279 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236:
#line 2311 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7287 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237:
#line 2315 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7295 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238:
#line 2319 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7303 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239:
#line 2323 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7311 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240:
#line 2327 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241:
#line 2335 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7336 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242:
#line 2344 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7349 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243:
#line 2353 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244:
#line 2363 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246:
#line 2370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247:
#line 2375 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd)));
                    }
#line 7388 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248:
#line 2379 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7397 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 249:
#line 2386 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7405 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250:
#line 2390 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7415 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251:
#line 2398 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7423 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252:
#line 2402 "mrbgems/mruby-compiler/core/parse.y"
                    {
#if 1
                      mrb_sym r = intern_op(mul);
                      mrb_sym b = intern_op(and);
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_splat(p, new_lvar(p, r))),
                                new_block_arg(p, new_lvar(p, b)));
#else
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      (yyval.nd) = cons(list2(push((yyvsp[-3].nd), new_splat(p, new_lvar(p, r))),
                                      new_kw_hash(p, list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k))))),
                                new_block_arg(p, new_lvar(p, b)));
#endif
                    }
#line 7443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253:
#line 2418 "mrbgems/mruby-compiler/core/parse.y"
                    {
#if 1
                      mrb_sym r = intern_op(mul);
                      mrb_sym b = intern_op(and);
                      if (local_var_p(p, r)  && local_var_p(p, b)) {
                        (yyval.nd) = cons(list1(new_splat(p, new_lvar(p, r))),
                                  new_block_arg(p, new_lvar(p, b)));
                      }
#else
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      if (local_var_p(p, r) && local_var_p(p, k) && local_var_p(p, b)) {
                        (yyval.nd) = cons(list2(new_splat(p, new_lvar(p, r)),
                                        new_kw_hash(p, list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k))))),
                                  new_block_arg(p, new_lvar(p, b)));
                      }
#endif
                      else {
                        yyerror(p, "unexpected argument forwarding ...");
                        (yyval.nd) = 0;
                      }
                    }
#line 7471 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 258:
#line 2450 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 259:
#line 2455 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 260:
#line 2460 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 261:
#line 2467 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(list1((yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7508 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262:
#line 2473 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7517 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263:
#line 2478 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7526 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264:
#line 2483 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7535 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265:
#line 2488 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266:
#line 2494 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 7553 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267:
#line 2499 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7562 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268:
#line 2506 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 7570 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269:
#line 2512 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7578 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270:
#line 2516 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 7586 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273:
#line 2526 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[0].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7596 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274:
#line 2532 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7606 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275:
#line 2538 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7615 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 276:
#line 2543 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277:
#line 2550 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7633 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278:
#line 2555 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7642 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279:
#line 2560 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 7651 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 287:
#line 2574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 7659 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 288:
#line 2578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 7667 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 289:
#line 2582 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7676 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 290:
#line 2588 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7685 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 291:
#line 2593 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 292:
#line 2597 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 7700 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293:
#line 2598 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7709 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294:
#line 2602 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 7715 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295:
#line 2603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 7723 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296:
#line 2607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7731 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297:
#line 2611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 7739 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298:
#line 2615 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 7747 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299:
#line 2619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7756 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300:
#line 2624 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7765 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301:
#line 2629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 7773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302:
#line 2633 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7781 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303:
#line 2637 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 7789 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304:
#line 2641 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 7797 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305:
#line 2645 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), cons(0, (yyvsp[0].nd)));
                    }
#line 7805 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307:
#line 2650 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7814 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308:
#line 2655 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 7824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309:
#line 2661 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7833 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310:
#line 2666 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 7845 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 311:
#line 2677 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7854 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312:
#line 2685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7863 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313:
#line 2689 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7869 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314:
#line 2689 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315:
#line 2692 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316:
#line 2696 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7890 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317:
#line 2696 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7896 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318:
#line 2699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319:
#line 2706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320:
#line 2710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 7921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321:
#line 2714 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 7927 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322:
#line 2716 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 7933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323:
#line 2719 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 7942 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324:
#line 2725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7953 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325:
#line 2733 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 7964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326:
#line 2741 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 7973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327:
#line 2746 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 7983 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328:
#line 2753 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 7996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329:
#line 2763 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8007 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330:
#line 2771 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8018 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331:
#line 2781 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332:
#line 2791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8041 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333:
#line 2799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 8049 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334:
#line 2803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 8057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335:
#line 2807 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 8065 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336:
#line 2811 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 8073 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337:
#line 2817 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 8082 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 344:
#line 2836 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 346:
#line 2843 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8098 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 347:
#line 2849 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 8106 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 349:
#line 2856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 8114 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 350:
#line 2860 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8122 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 351:
#line 2864 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8130 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352:
#line 2868 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3((yyvsp[-2].nd), nint(-1), 0);
                    }
#line 8139 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 353:
#line 2873 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), nint(-1), (yyvsp[0].nd));
                    }
#line 8147 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354:
#line 2877 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8155 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355:
#line 2881 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8163 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356:
#line 2885 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3(0, nint(-1), 0);
                    }
#line 8172 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357:
#line 2890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                    }
#line 8180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358:
#line 2894 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, nint(-1), (yyvsp[0].nd));
                    }
#line 8188 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359:
#line 2900 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360:
#line 2904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 8204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361:
#line 2908 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8212 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362:
#line 2912 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 8220 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363:
#line 2918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364:
#line 2922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 8236 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365:
#line 2928 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8244 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366:
#line 2932 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8252 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367:
#line 2936 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8260 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368:
#line 2940 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369:
#line 2944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370:
#line 2948 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8284 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371:
#line 2952 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8292 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372:
#line 2956 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8300 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373:
#line 2960 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8308 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374:
#line 2964 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8316 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375:
#line 2968 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8324 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376:
#line 2972 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8332 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377:
#line 2976 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378:
#line 2980 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8348 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379:
#line 2984 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8356 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380:
#line 2990 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8365 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381:
#line 2995 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8374 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382:
#line 3001 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p, 0);}
#line 8380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383:
#line 3002 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8388 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384:
#line 3006 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8397 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385:
#line 3011 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8405 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386:
#line 3018 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8413 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387:
#line 3022 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390:
#line 3032 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 8430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392:
#line 3040 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393:
#line 3044 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 394:
#line 3050 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395:
#line 3054 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396:
#line 3060 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8471 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397:
#line 3067 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8481 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398:
#line 3075 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (typen((yyvsp[-1].nd)->car) == NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8495 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399:
#line 3085 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8503 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400:
#line 3089 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8512 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401:
#line 3094 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8521 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402:
#line 3101 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8529 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403:
#line 3105 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8537 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404:
#line 3109 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8545 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405:
#line 3113 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8553 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406:
#line 3117 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 8561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407:
#line 3121 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), tCOLON2);
                    }
#line 8569 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408:
#line 3125 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8577 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409:
#line 3129 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 8585 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410:
#line 3133 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8593 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411:
#line 3139 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8603 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412:
#line 3146 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8614 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413:
#line 3153 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414:
#line 3160 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8635 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415:
#line 3171 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 8643 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416:
#line 3177 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 8656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418:
#line 3191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 8665 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 420:
#line 3199 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8673 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423:
#line 3207 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8681 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 425:
#line 3214 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 432:
#line 3228 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = concat_string(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8697 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435:
#line 3236 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8705 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 436:
#line 3240 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8713 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438:
#line 3247 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8721 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439:
#line 3253 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8729 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440:
#line 3257 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8738 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441:
#line 3263 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lex_strterm = (yyvsp[-2].nd);
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8747 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442:
#line 3268 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8755 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443:
#line 3272 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8763 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444:
#line 3278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8771 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445:
#line 3282 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8779 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446:
#line 3288 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8787 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447:
#line 3292 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8795 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 451:
#line 3305 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 8805 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 452:
#line 3311 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 8813 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 455:
#line 3321 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 8823 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456:
#line 3327 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457:
#line 3333 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[-2].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 8842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458:
#line 3341 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 8850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459:
#line 3345 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460:
#line 3352 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 8867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461:
#line 3357 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_dsym(p, new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd))));
                    }
#line 8876 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 462:
#line 3364 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467:
#line 3374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 468:
#line 3378 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 469:
#line 3384 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 8908 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 470:
#line 3388 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8916 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473:
#line 3396 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8924 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474:
#line 3400 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8932 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475:
#line 3406 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 8940 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476:
#line 3410 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 8948 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 477:
#line 3414 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 8956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478:
#line 3418 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 8964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479:
#line 3422 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 8972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480:
#line 3428 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481:
#line 3432 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 8988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482:
#line 3438 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 8996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483:
#line 3442 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484:
#line 3446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 9012 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485:
#line 3450 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 9020 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486:
#line 3454 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 9028 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487:
#line 3458 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 9040 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488:
#line 3466 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 9051 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489:
#line 3473 "mrbgems/mruby-compiler/core/parse.y"
                    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    }
#line 9064 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492:
#line 3488 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 493:
#line 3492 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9081 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 494:
#line 3497 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9089 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495:
#line 3508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9099 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 496:
#line 3514 "mrbgems/mruby-compiler/core/parse.y"
                    {
#if 1
                      /* til real keyword args implemented */
                      mrb_sym r = intern_op(mul);
                      mrb_sym b = intern_op(and);
                      local_add_f(p, r);
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, r, 0,
                                    new_args_tail(p, 0, 0, b));
#else
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      local_add_f(p, r); local_add_f(p, k);
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, r, 0,
                                    new_args_tail(p, 0, new_kw_rest_args(p, nsym(k)), b));
#endif
                    }
#line 9121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 497:
#line 3532 "mrbgems/mruby-compiler/core/parse.y"
                    {
#if 1
                      /* til real keyword args implemented */
                      mrb_sym r = intern_op(mul);
                      mrb_sym b = intern_op(and);
                      local_add_f(p, r);
                      (yyval.nd) = new_args(p, 0, 0, r, 0,
                                    new_args_tail(p, 0, 0, b));
#else
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      local_add_f(p, r); local_add_f(p, k);
                      (yyval.nd) = new_args(p, 0, 0, r, 0,
                                    new_args_tail(p, 0, new_kw_rest_args(p, nsym(k)), b));
#endif
                    }
#line 9143 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499:
#line 3553 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9151 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 500:
#line 3559 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 9159 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501:
#line 3565 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9169 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502:
#line 3571 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 503:
#line 3578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9187 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504:
#line 3583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505:
#line 3590 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506:
#line 3594 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9212 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507:
#line 3600 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9220 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508:
#line 3604 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511:
#line 3614 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, nsym((yyvsp[0].id)));
                    }
#line 9236 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512:
#line 3618 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, 0);
                    }
#line 9244 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513:
#line 3624 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9252 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514:
#line 3628 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9260 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515:
#line 3632 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516:
#line 3636 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517:
#line 3642 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9284 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518:
#line 3646 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9292 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519:
#line 3652 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9300 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520:
#line 3656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9308 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521:
#line 3660 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9316 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 522:
#line 3664 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9324 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523:
#line 3668 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9332 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524:
#line 3672 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525:
#line 3676 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9348 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526:
#line 3680 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9356 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527:
#line 3684 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9364 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528:
#line 3688 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9372 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529:
#line 3692 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530:
#line 3696 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9388 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531:
#line 3700 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9396 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532:
#line 3704 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9404 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533:
#line 3708 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(and));
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 9413 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534:
#line 3715 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 9422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535:
#line 3720 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 9431 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536:
#line 3725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 9440 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537:
#line 3730 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 9449 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538:
#line 3735 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 9458 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539:
#line 3742 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9466 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540:
#line 3746 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9475 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541:
#line 3753 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 9483 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542:
#line 3757 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 9491 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543:
#line 3761 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn_param(p, (yyvsp[-1].nd), p->locals->car);
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 9501 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544:
#line 3769 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9509 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545:
#line 3773 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9517 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546:
#line 3779 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 9527 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547:
#line 3787 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9537 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548:
#line 3795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9547 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549:
#line 3803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9555 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550:
#line 3807 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9563 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551:
#line 3813 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9571 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552:
#line 3817 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9579 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555:
#line 3827 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9588 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 556:
#line 3832 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.id) = -1;
                    }
#line 9597 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559:
#line 3843 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9605 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560:
#line 3849 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9613 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561:
#line 3853 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562:
#line 3859 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9630 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563:
#line 3863 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 9636 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564:
#line 3864 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-1].nd) == 0) {
                        yyerror(p, "can't define singleton method for ().");
                      }
                      else {
                        switch (typen((yyvsp[-1].nd)->car)) {
                        case NODE_STR:
                        case NODE_DSTR:
                        case NODE_XSTR:
                        case NODE_DXSTR:
                        case NODE_DREGX:
                        case NODE_MATCH:
                        case NODE_FLOAT:
                        case NODE_ARRAY:
                        case NODE_HEREDOC:
                          yyerror(p, "can't define singleton method for literals");
                        default:
                          break;
                        }
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9663 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 566:
#line 3890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 567:
#line 3896 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 9680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 568:
#line 3901 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9688 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 571:
#line 3911 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572:
#line 3917 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9707 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573:
#line 3922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if (typen((yyvsp[-2].nd)->car) == NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 9721 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574:
#line 3932 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 9730 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 587:
#line 3959 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 9738 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 588:
#line 3963 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 9746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 590:
#line 3970 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 9754 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 599:
#line 3991 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 9760 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 602:
#line 3997 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 9769 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 605:
#line 4008 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9777 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 9781 "mrbgems/mruby-compiler/core/y.tab.c"

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
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

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
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (p, YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (p, yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



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
                      yytoken, &yylval, p);
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

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp, p);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;


/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;


#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (p, YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval, p);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[+*yyssp], yyvsp, p);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 4012 "mrbgems/mruby-compiler/core/parse.y"

#define pylval  (*((YYSTYPE*)(p->ylval)))

static void
yyerror(parser_state *p, const char *s)
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
    c = (char *)parser_palloc(p, n + 1);
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
  yyerror(p, buf);
}

static void
yywarn(parser_state *p, const char *s)
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
    c = (char *)parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->warn_buffer[p->nwarn].message = c;
    p->warn_buffer[p->nwarn].lineno = p->lineno;
    p->warn_buffer[p->nwarn].column = p->column;
  }
  p->nwarn++;
}

static void
yywarning(parser_state *p, const char *s)
{
  yywarn(p, s);
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

  c = intn(n->car);

  if (c == NODE_NTH_REF) {
    yyerror_c(p, "can't set variable $", (char)intn(n->cdr)+'0');
  }
  else if (c == NODE_BACK_REF) {
    yyerror_c(p, "can't set variable $", (char)intn(n->cdr));
  }
  else {
    mrb_bug(p->mrb, "Internal error in backref_error() : n=>car == %d", c);
  }
}

static void
void_expr_error(parser_state *p, node *n)
{
  int c;

  if (n == NULL) return;
  c = intn(n->car);
  switch (c) {
  case NODE_BREAK:
  case NODE_RETURN:
  case NODE_NEXT:
  case NODE_REDO:
  case NODE_RETRY:
    yyerror(p, "void value expression");
    break;
  case NODE_AND:
  case NODE_OR:
    if (n->cdr) {
      void_expr_error(p, n->cdr->car);
      void_expr_error(p, n->cdr->cdr);
    }
    break;
  case NODE_BEGIN:
    if (n->cdr) {
      while (n->cdr) {
        n = n->cdr;
      }
      void_expr_error(p, n->car);
    }
    break;
  default:
    break;
  }
}

static void pushback(parser_state *p, int c);
static mrb_bool peeks(parser_state *p, const char *s);
static mrb_bool skips(parser_state *p, const char *s);

static inline int
nextc0(parser_state *p)
{
  int c;

  if (p->s && p->s < p->send) {
    c = (unsigned char)*p->s++;
  }
  else {
#ifndef MRB_NO_STDIO
    if (p->f) {
      c = fgetc(p->f);
      if (feof(p->f)) return -1;
    }
    else
#endif
      return -1;
  }
  return c;
}

static inline int
nextc(parser_state *p)
{
  int c;

  if (p->pb) {
    node *tmp;

    c = intn(p->pb->car);
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
  p->pb = cons(nint(c), p->pb);
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
    list = push(list, nint(c0));
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
    else{
      s--;
    }
  }
  return FALSE;
}


static int
newtok(parser_state *p)
{
  if (p->tokbuf != p->buf) {
    mrb_free(p->mrb, p->tokbuf);
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
      p->tokbuf = (char*)mrb_malloc(p->mrb, p->tsiz);
      memcpy(p->tokbuf, p->buf, MRB_PARSER_TOKBUF_SIZE);
    }
    else {
      p->tokbuf = (char*)mrb_realloc(p->mrb, p->tokbuf, p->tsiz);
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
    yyerror(p, "string too long (truncated)");
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
    yyerror(p, "invalid escape character syntax");
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
    yyerror(p, "invalid Unicode code point");
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
      yyerror(p, "invalid hex escape");
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
      yyerror(p, "Invalid escape character syntax");
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
      yyerror(p, "Invalid escape character syntax");
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
    yyerror(p, "Invalid escape character syntax");
    return '\0';

  default:
    return c;
  }
}

static void
heredoc_count_indent(parser_heredoc_info *hinf, const char *str, size_t len, size_t spaces, size_t *offset)
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
    if (nindent > spaces || nindent > hinf->indent)
      break;
    indent = nindent;
    *offset += 1;
  }
}

static void
heredoc_remove_indent(parser_state *p, parser_heredoc_info *hinf)
{
  if (!hinf->remove_indent || hinf->indent == 0)
    return;
  node *indented, *n, *pair, *escaped, *nspaces;
  const char *str;
  size_t len, spaces, offset, start, end;
  indented = hinf->indented;
  while (indented) {
    n = indented->car;
    pair = n->car;
    str = (char*)pair->car;
    len = (size_t)pair->cdr;
    escaped = n->cdr->car;
    nspaces = n->cdr->cdr;
    if (escaped) {
      char *newstr = strndup(str, len);
      size_t newlen = 0;
      start = 0;
      while (start < len) {
        end = escaped ? (size_t)escaped->car : len;
        spaces = (size_t)nspaces->car;
        size_t esclen = end - start;
        heredoc_count_indent(hinf, str + start, esclen, spaces, &offset);
        esclen -= offset;
        memcpy(newstr + newlen, str + start + offset, esclen);
        newlen += esclen;
        start = end;
        if (escaped)
          escaped = escaped->cdr;
        nspaces = nspaces->cdr;
      }
      newstr[newlen] = '\0';
      pair->car = (node*)newstr;
      pair->cdr = (node*)newlen;
    } else {
      spaces = (size_t)nspaces->car;
      heredoc_count_indent(hinf, str, len, spaces, &offset);
      pair->car = (node*)(str + offset);
      pair->cdr = (node*)(len - offset);
    }
    indented = indented->cdr;
  }
}

static void
heredoc_push_indented(parser_state *p, parser_heredoc_info *hinf, node *pair, node *escaped, node *nspaces, mrb_bool empty_line)
{
  hinf->indented = push(hinf->indented, cons(pair, cons(escaped, nspaces)));
  while (nspaces) {
    size_t tspaces = (size_t)nspaces->car;
    if ((hinf->indent == ~0U || tspaces < hinf->indent) && !empty_line)
      hinf->indent = tspaces;
    nspaces = nspaces->cdr;
  }
}

static int
parse_string(parser_state *p)
{
  int c;
  string_type type = (string_type)(intptr_t)p->lex_strterm->car;
  int nest_level = intn(p->lex_strterm->cdr->car);
  int beg = intn(p->lex_strterm->cdr->cdr->car);
  int end = intn(p->lex_strterm->cdr->cdr->cdr);
  parser_heredoc_info *hinf = (type & STR_FUNC_HEREDOC) ? parsing_heredoc_inf(p) : NULL;

  mrb_bool unindent = hinf && hinf->remove_indent;
  mrb_bool head = hinf && hinf->line_head;
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
    if (hinf && (c == '\n' || c < 0)) {
      mrb_bool line_head;
      tokadd(p, '\n');
      tokfix(p);
      p->lineno++;
      p->column = 0;
      line_head = hinf->line_head;
      hinf->line_head = TRUE;
      if (line_head) {
        /* check whether end of heredoc */
        const char *s = tok(p);
        int len = toklen(p);
        if (hinf->allow_indent) {
          while (ISSPACE(*s) && len > 0) {
            ++s;
            --len;
          }
        }
        if ((len-1 == hinf->term_len) && (strncmp(s, hinf->term, len-1) == 0)) {
          heredoc_remove_indent(p, hinf);
          return tHEREDOC_END;
        }
      }
      if (c < 0) {
        char buf[256];
        const char s1[] = "can't find heredoc delimiter \"";
        const char s2[] = "\" anywhere before EOF";

        if (sizeof(s1)+sizeof(s2)+strlen(hinf->term)+1 >= sizeof(buf)) {
          yyerror(p, "can't find heredoc delimiter anywhere before EOF");
        } else {
          strcpy(buf, s1);
          strcat(buf, hinf->term);
          strcat(buf, s2);
          yyerror(p, buf);
        }
        return 0;
      }
      node *nd = new_str(p, tok(p), toklen(p));
      pylval.nd = nd;
      if (unindent && head) {
        nspaces = push(nspaces, nint(spaces));
        heredoc_push_indented(p, hinf, nd->cdr, escaped, nspaces, empty && line_head);
      }
      return tHD_STRING_MID;
    }
    if (unindent && empty) {
      if (c == '\t')
        spaces += 8;
      else if (ISSPACE(c))
        ++spaces;
      else
        empty = FALSE;
    }
    if (c < 0) {
      yyerror(p, "unterminated string meets end of file");
      return 0;
    }
    else if (c == beg) {
      nest_level++;
      p->lex_strterm->cdr->car = nint(nest_level);
    }
    else if (c == end) {
      nest_level--;
      p->lex_strterm->cdr->car = nint(nest_level);
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
            nspaces = push(nspaces, nint(spaces));
            escaped = push(escaped, nint(pos));
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
          if (hinf)
            hinf->line_head = FALSE;
        }
        else {
          pushback(p, c);
          tokadd(p, read_escape(p));
          if (hinf)
            hinf->line_head = FALSE;
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
        node *nd = new_str(p, tok(p), toklen(p));
        pylval.nd = nd;
        if (hinf) {
          if (unindent && head) {
            nspaces = push(nspaces, nint(spaces));
            heredoc_push_indented(p, hinf, nd->cdr, escaped, nspaces, FALSE);
          }
          hinf->line_head = FALSE;
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
        pylval.nd = new_str(p, tok(p), toklen(p));
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
  p->lstate = EXPR_ENDARG;
  end_strterm(p);

  if (type & STR_FUNC_XQUOTE) {
    pylval.nd = new_xstr(p, tok(p), toklen(p));
    return tXSTRING;
  }

  if (type & STR_FUNC_REGEXP) {
    int f = 0;
    int re_opt;
    char *s = strndup(tok(p), toklen(p));
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
      yyerror(p, msg);
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
    pylval.nd = new_regx(p, s, dup, encp);

    return tREGEXP;
  }
  pylval.nd = new_str(p, tok(p), toklen(p));

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
    list = push(list, nint(c));

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
      yyerror(p, "unterminated here document identifier");
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
  newnode = new_heredoc(p);
  info = (parser_heredoc_info*)newnode->cdr;
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
              yyerror(p, "embedded document meets end of file");
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
      yyerror(p, "incomplete character syntax");
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
          yyerror(p, buf);
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
    pylval.nd = new_str(p, tok(p), toklen(p));
    p->lstate = EXPR_ENDARG;
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
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '.') {
        if ((c = nextc(p)) == '.') {
          return is_beg ? tBDOT3 : tDOT3;
        }
        pushback(p, c);
        return is_beg ? tBDOT2 : tDOT2;
      }
      pushback(p, c);
      if (c >= 0 && ISDIGIT(c)) {
        yyerror(p, "no .<digit> floating literal anymore; put 0 before dot");
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
    p->lstate = EXPR_ENDARG;
    newtok(p);
    if (c == '-' || c == '+') {
      tokadd(p, c);
      c = nextc(p);
    }
    if (c == '0') {
#define no_digits() do {yyerror(p,"numeric literal without digits"); return 0;} while (0)
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
        yyerror(p, "Invalid octal digit");
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
      yywarning_s(p, "floating point numbers are not supported", tok(p));
      pylval.nd = new_int(p, "0", 10, 0);
      return tINTEGER;
#else
      double d;
      char *endp;

      errno = 0;
      d = mrb_float_read(tok(p), &endp);
      if (d == 0 && endp == tok(p)) {
        yywarning_s(p, "corrupted float value", tok(p));
      }
      else if (errno == ERANGE) {
        yywarning_s(p, "float out of range", tok(p));
        errno = 0;
      }
      suffix = number_literal_suffix(p);
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
    if (!ISSPACE(c) || (IS_BEG() && p->lstate != EXPR_MID )) {
      pushback(p, c);
      p->lstate = EXPR_FNAME;
      return tSYMBEG;
    }
    pushback(p, c);
    p->lstate = EXPR_BEG;
    return ':';

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
          yyerror(p, "unknown type of %string");
          return 0;
        }
      }
      if (c < 0 || term < 0) {
        yyerror(p, "unterminated quoted string meets end of file");
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
        yyerror(p, "unknown type of %string");
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
      yyerror(p, "incomplete global variable syntax");
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
        unsigned long n = strtoul(tok(p), NULL, 10);
        if (n > INT_MAX) {
          yyerror(p, "capture group index must be <= " MRB_STRINGIZE(INT_MAX));
          return 0;
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
          yyerror(p, "incomplete instance variable syntax");
        }
        else {
          yyerror(p, "incomplete class variable syntax");
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
        yyerror(p, buf);
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
          node *nvars = p->nvars->cdr;

          while (nvars) {
            nvar = intn(nvars->car);
            if (nvar == -2) break; /* top of the scope */
            if (nvar > 0) {
              yywarning(p, "numbered parameter used in outer block");
              break;
            }
            nvars->car = nint(-1);
            nvars = nvars->cdr;
          }
          nvar = intn(p->nvars->car);
          if (nvar == -1) {
            yywarning(p, "numbered parameter used in inner block");
          }
          if (nvar >= -1) {
            pylval.num = n;
            p->lstate = EXPR_END;
            return tNUMPARAM;
          }
          else {
            yywarning(p, "identifier for numbered parameter; consider another name");
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
          if (state == EXPR_BEG || state == EXPR_VALUE)
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
yylex(void *lval, parser_state *p)
{
  p->ylval = lval;
  return parser_yylex(p);
}

static void
parser_init_cxt(parser_state *p, mrbc_context *cxt)
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
  p->upper = cxt->upper;
  if (cxt->partial_hook) {
    p->cxt = cxt;
  }
}

static void
parser_update_cxt(parser_state *p, mrbc_context *cxt)
{
  node *n, *n0;
  int i = 0;

  if (!cxt) return;
  if (intn(p->tree->car) != NODE_SCOPE) return;
  n0 = n = p->tree->cdr->car;
  while (n) {
    i++;
    n = n->cdr;
  }
  cxt->syms = (mrb_sym *)mrb_realloc(p->mrb, cxt->syms, i*sizeof(mrb_sym));
  cxt->slen = i;
  for (i=0, n=n0; n; i++,n=n->cdr) {
    cxt->syms[i] = sym(n->car);
  }
}

void mrb_codedump_all(mrb_state*, struct RProc*);
void mrb_parser_dump(mrb_state *mrb, node *tree, int offset);

MRB_API void
mrb_parser_parse(parser_state *p, mrbc_context *c)
{
  struct mrb_jmpbuf buf1;
  p->jmp = &buf1;

  MRB_TRY(p->jmp) {
    int n = 1;

    p->cmd_start = TRUE;
    p->in_def = p->in_single = 0;
    p->nerr = p->nwarn = 0;
    p->lex_strterm = NULL;

    parser_init_cxt(p, c);

    if (p->mrb->jmp) {
      n = yyparse(p);
    }
    else {
      struct mrb_jmpbuf buf2;

      p->mrb->jmp = &buf2;
      MRB_TRY(p->mrb->jmp) {
        n = yyparse(p);
      }
      MRB_CATCH(p->mrb->jmp) {
        p->nerr++;
      }
      MRB_END_EXC(p->mrb->jmp);
      p->mrb->jmp = 0;
    }
    if (n != 0 || p->nerr > 0) {
      p->tree = 0;
      return;
    }
    if (!p->tree) {
      p->tree = new_nil(p);
    }
    parser_update_cxt(p, c);
    if (c && c->dump_result) {
      mrb_parser_dump(p->mrb, p->tree, 0);
    }
  }
  MRB_CATCH(p->jmp) {
    yyerror(p, "memory allocation error");
    p->nerr++;
    p->tree = 0;
    return;
  }
  MRB_END_EXC(p->jmp);
}

MRB_API parser_state*
mrb_parser_new(mrb_state *mrb)
{
  mrb_pool *pool;
  parser_state *p;
  static const parser_state parser_state_zero = { 0 };

  pool = mrb_pool_open(mrb);
  if (!pool) return NULL;
  p = (parser_state *)mrb_pool_alloc(pool, sizeof(parser_state));
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
  p->all_heredocs = p->parsing_heredoc = NULL;
  p->lex_strterm_before_heredoc = NULL;

  p->current_filename_index = -1;
  p->filename_table = NULL;
  p->filename_table_length = 0;

  return p;
}

MRB_API void
mrb_parser_free(parser_state *p) {
  if (p->tokbuf != p->buf) {
    mrb_free(p->mrb, p->tokbuf);
  }
  mrb_pool_close(p->pool);
}

MRB_API mrbc_context*
mrbc_context_new(mrb_state *mrb)
{
  return (mrbc_context *)mrb_calloc(mrb, 1, sizeof(mrbc_context));
}

MRB_API void
mrbc_context_free(mrb_state *mrb, mrbc_context *cxt)
{
  mrb_free(mrb, cxt->filename);
  mrb_free(mrb, cxt->syms);
  mrb_free(mrb, cxt);
}

MRB_API const char*
mrbc_filename(mrb_state *mrb, mrbc_context *c, const char *s)
{
  if (s) {
    size_t len = strlen(s);
    char *p = (char *)mrb_malloc(mrb, len + 1);

    memcpy(p, s, len + 1);
    if (c->filename) {
      mrb_free(mrb, c->filename);
    }
    c->filename = p;
  }
  return c->filename;
}

MRB_API void
mrbc_partial_hook(mrb_state *mrb, mrbc_context *c, int (*func)(struct mrb_parser_state*), void *data)
{
  c->partial_hook = func;
  c->partial_data = data;
}

MRB_API void
mrbc_cleanup_local_variables(mrb_state *mrb, mrbc_context *c)
{
  if (c->syms) {
    mrb_free(mrb, c->syms);
    c->syms = NULL;
    c->slen = 0;
  }
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

  for (i = 0; i < p->filename_table_length; ++i) {
    if (p->filename_table[i] == sym) {
      p->current_filename_index = i;
      return;
    }
  }

  if (p->filename_table_length == UINT16_MAX) {
    yyerror(p, "too many files to compile");
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
mrb_parse_file_continue(mrb_state *mrb, FILE *f, const void *prebuf, size_t prebufsize, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  if (prebuf) {
    p->s = (const char *)prebuf;
    p->send = (const char *)prebuf + prebufsize;
  }
  else {
    p->s = p->send = NULL;
  }
  p->f = f;

  mrb_parser_parse(p, c);
  return p;
}

MRB_API parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  return mrb_parse_file_continue(mrb, f, NULL, 0, c);
}
#endif

MRB_API parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, size_t len, mrbc_context *c)
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
mrb_parse_string(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_parse_nstring(mrb, s, strlen(s), c);
}

MRB_API mrb_value
mrb_load_exec(mrb_state *mrb, struct mrb_parser_state *p, mrbc_context *c)
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
mrb_load_file_cxt(mrb_state *mrb, FILE *f, mrbc_context *c)
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
mrb_load_detect_file_cxt(mrb_state *mrb, FILE *fp, mrbc_context *c)
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
    size_t binsize;
    uint8_t *bin;
    mrb_value bin_obj = mrb_nil_value(); /* temporary string object */
    mrb_value result;

    binsize = bin_to_uint32(leading.h.binary_size);
    bin_obj = mrb_str_new(mrb, NULL, binsize);
    bin = (uint8_t *)RSTRING_PTR(bin_obj);
    memcpy(bin, leading.b, bufsize);
    if (binsize > bufsize &&
        fread(bin + bufsize, binsize - bufsize, 1, fp) == 0) {
      binsize = bufsize;
      /* The error is reported by mrb_load_irep_buf_cxt() */
    }

    result = mrb_load_irep_buf_cxt(mrb, bin, binsize, c);
    if (mrb_string_p(bin_obj)) mrb_str_resize(mrb, bin_obj, 0);
    return result;
  }
}
#endif

MRB_API mrb_value
mrb_load_nstring_cxt(mrb_state *mrb, const char *s, size_t len, mrbc_context *c)
{
  return mrb_load_exec(mrb, mrb_parse_nstring(mrb, s, len, c), c);
}

MRB_API mrb_value
mrb_load_nstring(mrb_state *mrb, const char *s, size_t len)
{
  return mrb_load_nstring_cxt(mrb, s, len, NULL);
}

MRB_API mrb_value
mrb_load_string_cxt(mrb_state *mrb, const char *s, mrbc_context *c)
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
dump_prefix(node *tree, int offset)
{
  printf("%05d ", tree->lineno);
  while (offset--) {
    putc(' ', stdout);
    putc(' ', stdout);
  }
}

static void
dump_recur(mrb_state *mrb, node *tree, int offset)
{
  while (tree) {
    mrb_parser_dump(mrb, tree->car, offset);
    tree = tree->cdr;
  }
}

static void
dump_args(mrb_state *mrb, node *n, int offset)
{
  if (n->car) {
    dump_prefix(n, offset+1);
    printf("mandatory args:\n");
    dump_recur(mrb, n->car, offset+2);
  }
  n = n->cdr;
  if (n->car) {
    dump_prefix(n, offset+1);
    printf("optional args:\n");
    {
      node *n2 = n->car;

      while (n2) {
        dump_prefix(n2, offset+2);
        printf("%s=\n", mrb_sym_name(mrb, sym(n2->car->car)));
        mrb_parser_dump(mrb, n2->car->cdr, offset+3);
        n2 = n2->cdr;
      }
    }
  }
  n = n->cdr;
  if (n->car) {
    dump_prefix(n, offset+1);
    printf("rest=*%s\n", mrb_sym_name(mrb, sym(n->car)));
  }
  n = n->cdr;
  if (n->car) {
    dump_prefix(n, offset+1);
    printf("post mandatory args:\n");
    dump_recur(mrb, n->car, offset+2);
  }

  n = n->cdr;
  if (n) {
    mrb_assert(intn(n->car) == NODE_ARGS_TAIL);
    mrb_parser_dump(mrb, n, offset);
  }
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
  mrb_value s;
# if INT_MAX > MRB_INT_MAX / 4
  /* check maximum length with "\xNN" character */
  if (len > MRB_INT_MAX / 4) {
    len = MRB_INT_MAX / 4;
  }
# endif
  s = mrb_str_new(mrb, str, (mrb_int)len);
  s = mrb_str_dump(mrb, s);
  mrb_gc_arena_restore(mrb, ai);
  return RSTRING_PTR(s);
}
#endif

void
mrb_parser_dump(mrb_state *mrb, node *tree, int offset)
{
#ifndef MRB_NO_STDIO
  int nodetype;

  if (!tree) return;
  again:
  dump_prefix(tree, offset);
  nodetype = intn(tree->car);
  tree = tree->cdr;
  switch (nodetype) {
  case NODE_BEGIN:
    printf("NODE_BEGIN:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_RESCUE:
    printf("NODE_RESCUE:\n");
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n2 = tree->car;

      dump_prefix(n2, offset+1);
      printf("rescue:\n");
      while (n2) {
        node *n3 = n2->car;
        if (n3->car) {
          dump_prefix(n2, offset+2);
          printf("handle classes:\n");
          dump_recur(mrb, n3->car, offset+3);
        }
        if (n3->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("exc_var:\n");
          mrb_parser_dump(mrb, n3->cdr->car, offset+3);
        }
        if (n3->cdr->cdr->car) {
          dump_prefix(n3, offset+2);
          printf("rescue body:\n");
          mrb_parser_dump(mrb, n3->cdr->cdr->car, offset+3);
        }
        n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->car, offset+2);
    }
    break;

  case NODE_ENSURE:
    printf("NODE_ENSURE:\n");
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("ensure:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr, offset+2);
    break;

  case NODE_LAMBDA:
    printf("NODE_LAMBDA:\n");
    dump_prefix(tree, offset);
    goto block;

  case NODE_BLOCK:
    block:
    printf("NODE_BLOCK:\n");
    tree = tree->cdr;
    if (tree->car) {
      dump_args(mrb, tree->car, offset+1);
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    break;

  case NODE_IF:
    printf("NODE_IF:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("then:\n");
    mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    if (tree->cdr->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("else:\n");
      mrb_parser_dump(mrb, tree->cdr->cdr->car, offset+2);
    }
    break;

  case NODE_AND:
    printf("NODE_AND:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_OR:
    printf("NODE_OR:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_CASE:
    printf("NODE_CASE:\n");
    if (tree->car) {
      mrb_parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("case:\n");
      dump_recur(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("body:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_WHILE:
    printf("NODE_WHILE:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_UNTIL:
    printf("NODE_UNTIL:\n");
    dump_prefix(tree, offset+1);
    printf("cond:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_FOR:
    printf("NODE_FOR:\n");
    dump_prefix(tree, offset+1);
    printf("var:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(n2, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          mrb_parser_dump(mrb, n2->car, offset+3);
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("in:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("do:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    break;

  case NODE_SCOPE:
    printf("NODE_SCOPE:\n");
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym_name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    offset++;
    goto again;

  case NODE_FCALL:
  case NODE_CALL:
  case NODE_SCALL:
    switch (nodetype) {
    case NODE_FCALL:
      printf("NODE_FCALL:\n"); break;
    case NODE_CALL:
      printf("NODE_CALL(.):\n"); break;
    case NODE_SCALL:
      printf("NODE_SCALL(&.):\n"); break;
    default:
      break;
    }
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("method='%s' (%d)\n",
        mrb_sym_dump(mrb, sym(tree->cdr->car)),
        intn(tree->cdr->car));
    tree = tree->cdr->cdr->car;
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_DOT2:
    printf("NODE_DOT2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_DOT3:
    printf("NODE_DOT3:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    mrb_parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_COLON2:
    printf("NODE_COLON2:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("::%s\n", mrb_sym_name(mrb, sym(tree->cdr)));
    break;

  case NODE_COLON3:
    printf("NODE_COLON3: ::%s\n", mrb_sym_name(mrb, sym(tree)));
    break;

  case NODE_ARRAY:
    printf("NODE_ARRAY:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_HASH:
    printf("NODE_HASH:\n");
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("key:\n");
      mrb_parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("value:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_KW_HASH:
    printf("NODE_KW_HASH:\n");
    while (tree) {
      dump_prefix(tree, offset+1);
      printf("key:\n");
      mrb_parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(tree, offset+1);
      printf("value:\n");
      mrb_parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_SPLAT:
    printf("NODE_SPLAT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_ASGN:
    printf("NODE_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_MASGN:
    printf("NODE_MASGN:\n");
    dump_prefix(tree, offset+1);
    printf("mlhs:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
        dump_prefix(tree, offset+2);
        printf("pre:\n");
        dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
        if (n2->car) {
          dump_prefix(n2, offset+2);
          printf("rest:\n");
          if (n2->car == nint(-1)) {
            dump_prefix(n2, offset+2);
            printf("(empty)\n");
          }
          else {
            mrb_parser_dump(mrb, n2->car, offset+3);
          }
        }
        n2 = n2->cdr;
        if (n2) {
          if (n2->car) {
            dump_prefix(n2, offset+2);
            printf("post:\n");
            dump_recur(mrb, n2->car, offset+3);
          }
        }
      }
    }
    dump_prefix(tree, offset+1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_OP_ASGN:
    printf("NODE_OP_ASGN:\n");
    dump_prefix(tree, offset+1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf("op='%s' (%d)\n", mrb_sym_name(mrb, sym(tree->car)), intn(tree->car));
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_SUPER:
    printf("NODE_SUPER:\n");
    if (tree) {
      dump_prefix(tree, offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
        dump_prefix(tree, offset+1);
        printf("block:\n");
        mrb_parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_ZSUPER:
    printf("NODE_ZSUPER\n");
    break;

  case NODE_RETURN:
    printf("NODE_RETURN:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_YIELD:
    printf("NODE_YIELD:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_BREAK:
    printf("NODE_BREAK:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_NEXT:
    printf("NODE_NEXT:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_REDO:
    printf("NODE_REDO\n");
    break;

  case NODE_RETRY:
    printf("NODE_RETRY\n");
    break;

  case NODE_LVAR:
    printf("NODE_LVAR %s\n", mrb_sym_name(mrb, sym(tree)));
    break;

  case NODE_GVAR:
    printf("NODE_GVAR %s\n", mrb_sym_name(mrb, sym(tree)));
    break;

  case NODE_IVAR:
    printf("NODE_IVAR %s\n", mrb_sym_name(mrb, sym(tree)));
    break;

  case NODE_CVAR:
    printf("NODE_CVAR %s\n", mrb_sym_name(mrb, sym(tree)));
    break;

  case NODE_NVAR:
    printf("NODE_NVAR %d\n", intn(tree));
    break;

  case NODE_CONST:
    printf("NODE_CONST %s\n", mrb_sym_name(mrb, sym(tree)));
    break;

  case NODE_MATCH:
    printf("NODE_MATCH:\n");
    dump_prefix(tree, offset + 1);
    printf("lhs:\n");
    mrb_parser_dump(mrb, tree->car, offset + 2);
    dump_prefix(tree, offset + 1);
    printf("rhs:\n");
    mrb_parser_dump(mrb, tree->cdr, offset + 2);
    break;

  case NODE_BACK_REF:
    printf("NODE_BACK_REF: $%c\n", intn(tree));
    break;

  case NODE_NTH_REF:
    printf("NODE_NTH_REF: $%d\n", intn(tree));
    break;

  case NODE_ARG:
    printf("NODE_ARG %s\n", mrb_sym_name(mrb, sym(tree)));
    break;

  case NODE_BLOCK_ARG:
    printf("NODE_BLOCK_ARG:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_INT:
    printf("NODE_INT %s base %d\n", (char*)tree->car, intn(tree->cdr->car));
    break;

  case NODE_FLOAT:
    printf("NODE_FLOAT %s\n", (char*)tree);
    break;

  case NODE_NEGATE:
    printf("NODE_NEGATE:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_STR:
    printf("NODE_STR %s len %d\n", str_dump(mrb, (char*)tree->car, intn(tree->cdr)), intn(tree->cdr));
    break;

  case NODE_DSTR:
    printf("NODE_DSTR:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_XSTR:
    printf("NODE_XSTR %s len %d\n", str_dump(mrb, (char*)tree->car, intn(tree->cdr)), intn(tree->cdr));
    break;

  case NODE_DXSTR:
    printf("NODE_DXSTR:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_REGX:
    printf("NODE_REGX /%s/%s\n", (char*)tree->car, (char*)tree->cdr);
    break;

  case NODE_DREGX:
    printf("NODE_DREGX:\n");
    dump_recur(mrb, tree->car, offset+1);
    dump_prefix(tree, offset);
    printf("tail: %s\n", (char*)tree->cdr->cdr->car);
    if (tree->cdr->cdr->cdr->car) {
      dump_prefix(tree, offset);
      printf("opt: %s\n", (char*)tree->cdr->cdr->cdr->car);
    }
    if (tree->cdr->cdr->cdr->cdr) {
      dump_prefix(tree, offset);
      printf("enc: %s\n", (char*)tree->cdr->cdr->cdr->cdr);
    }
    break;

  case NODE_SYM:
    printf("NODE_SYM :%s (%d)\n", mrb_sym_dump(mrb, sym(tree)),
           intn(tree));
    break;

  case NODE_DSYM:
    printf("NODE_DSYM:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_WORDS:
    printf("NODE_WORDS:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_SYMBOLS:
    printf("NODE_SYMBOLS:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_LITERAL_DELIM:
    printf("NODE_LITERAL_DELIM\n");
    break;

  case NODE_SELF:
    printf("NODE_SELF\n");
    break;

  case NODE_NIL:
    printf("NODE_NIL\n");
    break;

  case NODE_TRUE:
    printf("NODE_TRUE\n");
    break;

  case NODE_FALSE:
    printf("NODE_FALSE\n");
    break;

  case NODE_ALIAS:
    printf("NODE_ALIAS %s %s:\n",
        mrb_sym_dump(mrb, sym(tree->car)),
        mrb_sym_dump(mrb, sym(tree->cdr)));
    break;

  case NODE_UNDEF:
    printf("NODE_UNDEF");
    {
      node *t = tree;
      while (t) {
        printf(" %s", mrb_sym_dump(mrb, sym(t->car)));
        t = t->cdr;
      }
    }
    printf(":\n");
    break;

  case NODE_CLASS:
    printf("NODE_CLASS:\n");
    if (tree->car->car == nint(0)) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym_name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == nint(1)) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym_name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym_name(mrb, sym(tree->car->cdr)));
    }
    if (tree->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("super:\n");
      mrb_parser_dump(mrb, tree->cdr->car, offset+2);
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->cdr->car->cdr, offset+2);
    break;

  case NODE_MODULE:
    printf("NODE_MODULE:\n");
    if (tree->car->car == nint(0)) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym_name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == nint(1)) {
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym_name(mrb, sym(tree->car->cdr)));
    }
    else {
      mrb_parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(tree, offset+1);
      printf("::%s\n", mrb_sym_name(mrb, sym(tree->car->cdr)));
    }
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_SCLASS:
    printf("NODE_SCLASS:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("body:\n");
    mrb_parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_DEF:
    printf("NODE_DEF:\n");
    dump_prefix(tree, offset+1);
    printf("%s\n", mrb_sym_dump(mrb, sym(tree->car)));
    tree = tree->cdr;
    {
      node *n2 = tree->car;
      mrb_bool first_lval = TRUE;

      if (n2 && (n2->car || n2->cdr)) {
        dump_prefix(n2, offset+1);
        printf("local variables:\n");
        dump_prefix(n2, offset+2);
        while (n2) {
          if (n2->car) {
            if (!first_lval) printf(", ");
            printf("%s", mrb_sym_name(mrb, sym(n2->car)));
            first_lval = FALSE;
          }
          n2 = n2->cdr;
        }
        printf("\n");
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_args(mrb, tree->car, offset);
    }
    mrb_parser_dump(mrb, tree->cdr->car, offset+1);
    break;

  case NODE_SDEF:
    printf("NODE_SDEF:\n");
    mrb_parser_dump(mrb, tree->car, offset+1);
    tree = tree->cdr;
    dump_prefix(tree, offset+1);
    printf(":%s\n", mrb_sym_dump(mrb, sym(tree->car)));
    tree = tree->cdr->cdr;
    if (tree->car) {
      dump_args(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    mrb_parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_POSTEXE:
    printf("NODE_POSTEXE:\n");
    mrb_parser_dump(mrb, tree, offset+1);
    break;

  case NODE_HEREDOC:
    printf("NODE_HEREDOC (<<%s):\n", ((parser_heredoc_info*)tree)->term);
    dump_recur(mrb, ((parser_heredoc_info*)tree)->doc, offset+1);
    break;

  case NODE_ARGS_TAIL:
    printf("NODE_ARGS_TAIL:\n");
    {
      node *kws = tree->car;

      while (kws) {
        mrb_parser_dump(mrb, kws->car, offset+1);
        kws = kws->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      mrb_assert(intn(tree->car->car) == NODE_KW_REST_ARGS);
      mrb_parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(tree, offset+1);
      printf("block='%s'\n", mrb_sym_name(mrb, sym(tree->car)));
    }
    break;

  case NODE_KW_ARG:
    printf("NODE_KW_ARG %s:\n", mrb_sym_name(mrb, sym(tree->car)));
    mrb_parser_dump(mrb, tree->cdr->car, offset + 1);
    break;

  case NODE_KW_REST_ARGS:
    printf("NODE_KW_REST_ARGS %s\n", mrb_sym_name(mrb, sym(tree)));
    break;

  default:
    printf("node type: %d (0x%x)\n", nodetype, (unsigned)nodetype);
    break;
  }
#endif
}
