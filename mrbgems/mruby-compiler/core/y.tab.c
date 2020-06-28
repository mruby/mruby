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

#define intern_lit(s) mrb_intern_lit(p->mrb, s)

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
    for (i=0; i < ir->nlocals; i++) {
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
  local_add_f(p, blk ? blk : MRB_QSYM(and));
}

static void
local_add_kw(parser_state *p, mrb_sym kwd)
{
  /* allocate register for keywords hash */
  local_add_f(p, kwd ? kwd : MRB_QSYM(pow));
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
    if (n->car->car == (node*)NODE_MASGN) {
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
  return new_call(p, new_const(p, MRB_SYM(Kernel)), MRB_SYM(Complex), list1(list2(list3((node*)NODE_INT, (node*)strdup("0"), nint(10)), imaginary)), 1);
}

static node*
new_rational(parser_state *p, node *rational)
{
  return new_call(p, new_const(p, MRB_SYM(Kernel)), MRB_SYM(Rational), list1(list1(rational)), 1);
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

#ifndef MRB_WITHOUT_FLOAT
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
  return (int)((enum node_type)(intptr_t)n->car == NODE_STR);
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

  switch ((enum node_type)intn(a->car)) {
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
  return cons(nint(type), cons((node*)0, cons(nint(paren), nint(term))));
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


#line 1434 "mrbgems/mruby-compiler/core/y.tab.c"

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
    tAREF = 340,
    tASET = 341,
    tLSHFT = 342,
    tRSHFT = 343,
    tCOLON2 = 344,
    tCOLON3 = 345,
    tOP_ASGN = 346,
    tASSOC = 347,
    tLPAREN = 348,
    tLPAREN_ARG = 349,
    tRPAREN = 350,
    tLBRACK = 351,
    tLBRACE = 352,
    tLBRACE_ARG = 353,
    tSTAR = 354,
    tDSTAR = 355,
    tAMPER = 356,
    tLAMBDA = 357,
    tANDDOT = 358,
    tSYMBEG = 359,
    tREGEXP_BEG = 360,
    tWORDS_BEG = 361,
    tSYMBOLS_BEG = 362,
    tSTRING_BEG = 363,
    tXSTRING_BEG = 364,
    tSTRING_DVAR = 365,
    tLAMBEG = 366,
    tHEREDOC_BEG = 367,
    tHEREDOC_END = 368,
    tLITERAL_DELIM = 369,
    tHD_LITERAL_DELIM = 370,
    tHD_STRING_PART = 371,
    tHD_STRING_MID = 372,
    tLOWEST = 373,
    tUMINUS_NUM = 374,
    tLAST_TOKEN = 375
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 1376 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1612 "mrbgems/mruby-compiler/core/y.tab.c"

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
#define YYLAST   11948

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  147
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  177
/* YYNRULES -- Number of rules.  */
#define YYNRULES  603
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1054

#define YYUNDEFTOK  2
#define YYMAXUTOK   375


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     146,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   133,     2,     2,     2,   131,   126,     2,
     142,   143,   129,   127,   140,   128,   145,   130,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,   121,   144,
     123,   119,   122,   120,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,   138,     2,   139,   125,     2,   141,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,   136,   124,   137,   134,     2,     2,     2,
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
     115,   116,   117,   118,   132,   135
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,  1534,  1534,  1534,  1545,  1551,  1555,  1560,  1564,  1570,
    1572,  1571,  1585,  1612,  1618,  1622,  1627,  1631,  1637,  1637,
    1641,  1645,  1649,  1653,  1657,  1661,  1665,  1670,  1671,  1675,
    1679,  1683,  1687,  1688,  1691,  1696,  1701,  1705,  1711,  1715,
    1719,  1723,  1727,  1731,  1736,  1740,  1747,  1748,  1752,  1756,
    1757,  1761,  1765,  1769,  1773,  1777,  1787,  1786,  1801,  1810,
    1811,  1814,  1815,  1822,  1821,  1836,  1840,  1845,  1849,  1854,
    1858,  1863,  1867,  1871,  1875,  1879,  1885,  1889,  1895,  1896,
    1902,  1906,  1910,  1914,  1918,  1922,  1926,  1930,  1934,  1938,
    1944,  1945,  1951,  1955,  1961,  1965,  1971,  1975,  1979,  1983,
    1987,  1991,  1997,  2003,  2010,  2014,  2018,  2022,  2026,  2030,
    2036,  2042,  2047,  2053,  2057,  2060,  2064,  2068,  2075,  2076,
    2077,  2078,  2083,  2090,  2091,  2094,  2098,  2098,  2104,  2105,
    2106,  2107,  2108,  2109,  2110,  2111,  2112,  2113,  2114,  2115,
    2116,  2117,  2118,  2119,  2120,  2121,  2122,  2123,  2124,  2125,
    2126,  2127,  2128,  2129,  2130,  2131,  2132,  2133,  2136,  2136,
    2136,  2137,  2137,  2138,  2138,  2138,  2139,  2139,  2139,  2139,
    2140,  2140,  2140,  2141,  2141,  2141,  2142,  2142,  2142,  2142,
    2143,  2143,  2143,  2143,  2144,  2144,  2144,  2144,  2145,  2145,
    2145,  2145,  2146,  2146,  2146,  2146,  2147,  2147,  2150,  2154,
    2158,  2162,  2166,  2170,  2174,  2179,  2184,  2189,  2193,  2197,
    2201,  2205,  2209,  2213,  2217,  2221,  2225,  2229,  2233,  2237,
    2241,  2245,  2249,  2253,  2257,  2261,  2265,  2269,  2273,  2277,
    2281,  2285,  2289,  2293,  2297,  2301,  2305,  2309,  2313,  2317,
    2321,  2329,  2338,  2347,  2357,  2363,  2364,  2369,  2373,  2380,
    2384,  2392,  2396,  2422,  2423,  2426,  2427,  2428,  2433,  2438,
    2445,  2451,  2456,  2461,  2466,  2473,  2473,  2484,  2490,  2494,
    2500,  2501,  2504,  2510,  2516,  2521,  2528,  2533,  2538,  2545,
    2546,  2547,  2548,  2549,  2550,  2551,  2552,  2556,  2561,  2560,
    2572,  2576,  2571,  2581,  2581,  2585,  2589,  2593,  2597,  2602,
    2607,  2611,  2615,  2619,  2623,  2627,  2628,  2634,  2640,  2633,
    2652,  2660,  2668,  2668,  2668,  2675,  2675,  2675,  2682,  2688,
    2693,  2695,  2692,  2704,  2702,  2720,  2725,  2718,  2742,  2740,
    2756,  2766,  2777,  2781,  2785,  2789,  2795,  2802,  2803,  2804,
    2807,  2808,  2811,  2812,  2820,  2821,  2827,  2831,  2834,  2838,
    2842,  2846,  2851,  2855,  2859,  2863,  2869,  2868,  2878,  2882,
    2886,  2890,  2896,  2901,  2906,  2910,  2914,  2918,  2922,  2926,
    2930,  2934,  2938,  2942,  2946,  2950,  2954,  2958,  2962,  2968,
    2973,  2980,  2980,  2984,  2989,  2996,  3000,  3006,  3007,  3010,
    3015,  3018,  3022,  3028,  3032,  3039,  3038,  3053,  3063,  3067,
    3072,  3079,  3083,  3087,  3091,  3095,  3099,  3103,  3107,  3111,
    3118,  3117,  3132,  3131,  3147,  3155,  3164,  3167,  3174,  3177,
    3181,  3182,  3185,  3189,  3192,  3196,  3199,  3200,  3201,  3202,
    3205,  3206,  3212,  3213,  3214,  3218,  3224,  3225,  3231,  3236,
    3235,  3246,  3250,  3256,  3260,  3266,  3270,  3276,  3279,  3280,
    3283,  3289,  3295,  3296,  3299,  3306,  3305,  3319,  3323,  3330,
    3335,  3342,  3348,  3349,  3350,  3351,  3352,  3356,  3362,  3366,
    3372,  3373,  3374,  3378,  3384,  3388,  3392,  3396,  3400,  3406,
    3410,  3416,  3420,  3424,  3428,  3432,  3436,  3444,  3451,  3462,
    3463,  3467,  3471,  3470,  3486,  3492,  3512,  3513,  3519,  3525,
    3531,  3538,  3543,  3550,  3554,  3560,  3564,  3570,  3571,  3574,
    3578,  3584,  3588,  3592,  3596,  3602,  3607,  3612,  3616,  3620,
    3624,  3628,  3632,  3636,  3640,  3644,  3648,  3652,  3656,  3660,
    3664,  3669,  3675,  3680,  3685,  3690,  3695,  3702,  3706,  3713,
    3718,  3717,  3729,  3733,  3739,  3747,  3755,  3763,  3767,  3773,
    3777,  3783,  3784,  3787,  3792,  3799,  3800,  3803,  3809,  3813,
    3819,  3824,  3824,  3849,  3850,  3856,  3861,  3867,  3868,  3871,
    3877,  3882,  3892,  3899,  3900,  3901,  3904,  3905,  3906,  3907,
    3910,  3911,  3912,  3915,  3916,  3919,  3923,  3929,  3930,  3936,
    3937,  3940,  3941,  3944,  3947,  3948,  3949,  3952,  3953,  3954,
    3957,  3964,  3965,  3969
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
  "tMATCH", "tNMATCH", "tDOT2", "tDOT3", "tAREF", "tASET", "tLSHFT",
  "tRSHFT", "tCOLON2", "tCOLON3", "tOP_ASGN", "tASSOC", "tLPAREN",
  "tLPAREN_ARG", "tRPAREN", "tLBRACK", "tLBRACE", "tLBRACE_ARG", "tSTAR",
  "tDSTAR", "tAMPER", "tLAMBDA", "tANDDOT", "tSYMBEG", "tREGEXP_BEG",
  "tWORDS_BEG", "tSYMBOLS_BEG", "tSTRING_BEG", "tXSTRING_BEG",
  "tSTRING_DVAR", "tLAMBEG", "tHEREDOC_BEG", "tHEREDOC_END",
  "tLITERAL_DELIM", "tHD_LITERAL_DELIM", "tHD_STRING_PART",
  "tHD_STRING_MID", "tLOWEST", "'='", "'?'", "':'", "'>'", "'<'", "'|'",
  "'^'", "'&'", "'+'", "'-'", "'*'", "'/'", "'%'", "tUMINUS_NUM", "'!'",
  "'~'", "tLAST_TOKEN", "'{'", "'}'", "'['", "']'", "','", "'`'", "'('",
  "')'", "';'", "'.'", "'\\n'", "$accept", "program", "$@1",
  "top_compstmt", "top_stmts", "top_stmt", "@2", "bodystmt", "compstmt",
  "stmts", "stmt", "$@3", "rassign", "command_asgn", "command_rhs", "expr",
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
     365,   366,   367,   368,   369,   370,   371,   372,   373,    61,
      63,    58,    62,    60,   124,    94,    38,    43,    45,    42,
      47,    37,   374,    33,   126,   375,   123,   125,    91,    93,
      44,    96,    40,    41,    59,    46,    10
};
# endif

#define YYPACT_NINF (-870)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-604)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -870,   123,  2747,  -870,  7459,  9431,  9767,  5537,  -870,  9083,
    9083,  -870,  -870,  9543,  6957,  5393,  7807,  7807,  -870,  -870,
    7807,  3172,  6307,  -870,  -870,  -870,  -870,   127,  6957,  -870,
      78,  -870,  -870,  -870,  5677,  5817,  -870,  -870,  5957,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,    30,  9199,  9199,    95,
    4664,   906,  8039,  8387,  7235,  -870,  6679,  1068,  1316,  1360,
    1376,   695,  -870,   125,  9315,  9199,  -870,  1474,  -870,  1634,
      60,  -870,    75,  1663,  1663,  -870,  -870,   163,    87,  -870,
      34,  9655,  -870,   114, 11676,   197,   488,   260,    39,  -870,
     322,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
      38,   204,  -870,   241,    22,  -870,  -870,  -870,  -870,  -870,
      94,    94,   155,   275,   998,  -870,  9083,   263,  4781,   161,
    1663,  1663,  -870,   180,  -870,   626,  -870,  -870,    22,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,    70,   109,   134,
     138,  -870,  -870,  -870,  -870,  -870,  -870,   146,   171,   189,
     201,  -870,   205,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,   221,  3854,
     256,    75,   546,   207, 11757,   662,   102,   252,   250,   546,
    9083,  9083,   823,   292,  -870,  -870,   866,   337,   542,   643,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  6818,
    -870,  -870,   236,  -870,  -870,  -870,  -870,  -870,  -870,  1474,
    -870,   483,  -870,   363,  -870,  -870,  1474,  3038,  9199,  9199,
    9199,  9199,  -870, 11736,  -870,  -870,   259,   349,   259,  -870,
    -870,  -870,  7575,  -870,  -870,  -870,  7807,  -870,  -870,  -870,
    5393,  9083,  -870,  -870,   272,  4898,  -870,   902,   344,   305,
    7691,  4664,   279,  1474,  1634,  1474,   310,  -870,  7691,  1474,
     316,  1341,  1341,  -870, 11736,   289,  1341,  -870,   391,  9879,
     325,   936,   999,  1050,  1903,  -870,  -870,  -870,  -870,  1382,
    -870,  -870,  -870,  -870,  -870,  -870,   498,  1322,  -870,  -870,
    1398,  -870,  1451,  -870,  1460,  -870,   836,   399,   417,  -870,
    -870,  -870,  -870,  5159,  9083,  9083,  9083,  9083,  7691,  9543,
    9083,  9083,    54,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  2024,   378,  3854,  9199,  -870,
     370,   452,   384,  -870,  1474,  -870,  -870,  -870,   386,  9199,
    -870,   407,   467,   408,   504,  -870,   430,  3854,  -870,  -870,
    8503,  -870,  4664,  7347,   432,  8503,  9199,  9199,  9199,  9199,
    9199,  9199,  9199,  9199,  9199,  9199,  9199,  9199,  9199,  9199,
    9199,  9543,  9199,  9199,  9199,  9199,  9199,  9199,  9199,  9199,
    9199,  9199,  9199,  1804,  -870,  7807,  -870, 10151,  -870,  -870,
   11327,  -870,  -870,  -870,  -870,  9315,  9315,  -870,   459,  -870,
      75,  -870,  1169,  -870,  -870,  -870,  -870,  -870,  -870, 10235,
    7807, 10319,  3854,  9083,  -870,  -870,  -870,   555,   561,   295,
    -870,  3998,   562,  9199, 10403,  7807, 10487,  9199,  9199,  4286,
      88,    88,   827, 10571,  7807, 10655,  -870,   517,  -870,  4898,
     363,  -870,  -870,  8619,   569,  -870,   498,  9199, 11757, 11757,
   11757,  9199,   710,  -870,  7923,  -870,  9199,  -870,  8155,  1474,
     443,  1474,   259,   259,  -870,  -870,    64,   445,  -870,  -870,
    6957,  4403,   455, 10403, 10487,  9199,  1634,  1474,  -870,  -870,
    5276,   449,  1634,  -870,  -870,  8271,  -870,  1474,  8387,  -870,
    -870,  -870,  1169,    34,  9879,  -870,  9879, 10739,  7807, 10823,
    1771,  -870,  -870,  -870,  1539,  4898,   498,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  9199,  9199,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  1724,  1474,
    1474,  9199,   586, 11757,   139,  -870,  -870,  -870,   145,  -870,
    -870,  1771,  -870, 11757,  1771,  -870,  -870,  1417,  -870,  -870,
    9199,   591,    48,  9199,  -870, 11500,   259,  -870,  1474,  9879,
     478,  -870,  -870,  -870,   581,   515,  2373,  -870,  -870,  1180,
     321,   344,  2795,  2795,  2795,  2795,  1483,  1483, 11817,  2139,
    2795,  2795,  2250,  2250,   511,   511,  -870,  -870, 11475,  1483,
    1483,   981,   981,  1377,   178,   178,   344,   344,   344,  3306,
    6423,  3574,  6539,  -870,    94,  -870,   507,   334,  -870,   366,
    -870,  -870,  6307,  -870,  -870,  1624,    48,    48,  -870,  2860,
    -870,  -870,  -870,  -870,  -870,  1474,  9083,  3854,  1191,   100,
    -870,    94,   508,    94,   633,    64,  7096,  -870,  8735,   640,
    -870,   208,  -870,  6073,  6190,   521,   351,   439,   640,  -870,
    -870,  -870,  -870,  1148,   113,   522,  1227,  1261,  9083,  6957,
     528,   654, 11757,   601,  -870,   498, 11757, 11757,   498,  9199,
   11736,  -870,   259, 11757,  -870,  -870,  -870,  -870,  7923,  8155,
    -870,  -870,  -870,   533,  -870,  -870,    90,  1634,  1474,  1341,
     432,  -870,  1191,   100,   534,  1209,  1306,   532,    81,  -870,
     548,  -870,   344,   344,  -870,   834,  1474,   543,  -870,  -870,
   11397,  -870,   631,  -870,   384,  -870,  -870,  -870,   554,   556,
     558,  -870,   559,   631,   558, 11415,  -870,  -870,  1771,  3854,
    -870,  -870, 11569,  8851,  -870,  -870,  9879,  7691,  9315,  9199,
   10907,  7807, 10991,    61,  9315,  9315,  -870,   459,   414,  9315,
    9315,  -870,   459,    39,   163,  3854,  4898,    48,  -870,  1474,
     687,  -870,  -870,  -870,  -870, 11500,  -870,   613,  -870,  4547,
     698,  -870,  9083,   705,  -870,  9199,  9199,   441,  9199,  9199,
     707,  5042,  5042,   722,    88,  -870,  -870,  -870,  8967,  4142,
     498, 11757,  -870,   259,  -870,  -870,  -870,   732,   583,   580,
    3854,  4898,  -870,  -870,  -870,   587,  -870,  1746,  9199,  -870,
    1771,  -870,  1417,  -870,  1417,  -870,  1417,  -870,  -870,  9199,
    -870,   532,   532,  9991,  -870,   590,   384,   598,  9991,  -870,
     600,   604,  -870,   734,  9199, 11588,  -870,  -870, 11757,  3440,
    3708,   611,   460,   474,  9199,  9199,  -870,  -870,  -870,  -870,
    -870,  9315,  -870,  -870,  -870,  -870,  -870,  -870,  -870,   738,
     617,  4898,  3854,  -870,  -870, 10103,   546,  -870,  -870,  5042,
    -870,  -870,   546,  -870,  9199,  -870,   748,   752,  -870, 11757,
     112,  8155,  -870,  1287,   755,   635,  1210,  1210,  1010, 11757,
     558,   636,   558,   558, 11757,   658,   663,   735,  1221,   139,
    -870,  -870,  1580,  -870,  1221,  1771,  -870,  1417,  -870,  -870,
   11657,   475, 11757, 11757,  -870,  -870,  -870,  -870,   657,   786,
     761,  -870,  1272,   999,  1050,  3854,  -870,  3998,  -870,  -870,
    5042,  -870,  -870,  -870,  -870,    14,  -870,  -870,  -870,  -870,
     679,   679,  1210,   688,  -870,  1417,  -870,  -870,  -870,  -870,
    -870,  -870, 11075,  -870,   384,   139,  -870,  -870,   692,   693,
     694,  -870,   696,   694,  -870,  -870,  1169, 11159,  7807, 11243,
     561,   208,   826,  1287,  -870,  1210,   679,  1210,   558,   700,
     708,  -870,  1771,  -870,  1417,  -870,  1417,  -870,  1417,  -870,
    -870,  1191,   100,   699,  1043,  1087,  -870,  -870,  -870,  -870,
     679,  -870,   694,   712,   694,   694,   732,  -870,  1417,  -870,
    -870,  -870,   694,  -870
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   288,     0,
       0,   312,   315,     0,     0,   589,   332,   333,   334,   335,
     300,   265,   408,   483,   482,   484,   485,   591,     0,    10,
       0,   487,   486,   488,   474,   287,   476,   475,   478,   477,
     470,   471,   432,   433,   489,   490,   286,     0,     0,     0,
       0,   290,   603,   603,    88,   307,     0,     0,     0,     0,
       0,     0,   447,     0,     0,     0,     3,   589,     6,     9,
      32,    27,    33,   531,   531,    49,    60,    59,     0,    76,
       0,    80,    90,     0,    54,   244,     0,    61,   305,   279,
     280,   430,   281,   282,   283,   428,   427,   459,   429,   426,
     481,     0,   284,   285,   265,     5,     8,   332,   333,   300,
     603,   408,     0,   113,   114,   286,     0,     0,     0,     0,
     531,   531,   116,   491,   336,     0,   481,   285,     0,   328,
     168,   178,   169,   165,   194,   195,   196,   197,   176,   191,
     184,   174,   173,   189,   172,   171,   167,   192,   166,   179,
     183,   185,   177,   170,   186,   193,   188,   187,   180,   190,
     175,   164,   182,   181,   163,   161,   162,   158,   159,   160,
     118,   120,   119,   153,   154,   149,   131,   132,   133,   140,
     137,   139,   134,   135,   155,   156,   141,   142,   146,   150,
     136,   138,   128,   129,   130,   143,   144,   145,   147,   148,
     151,   152,   157,   561,    55,   121,   122,   560,     0,     0,
       0,    58,     0,     0,    54,     0,   481,     0,   285,     0,
       0,     0,   112,     0,   347,   346,     0,     0,   104,   111,
     187,   180,   190,   175,   158,   159,   160,   118,   119,     0,
     123,   125,    20,   124,   450,   455,   454,   597,   600,   589,
     599,     0,   452,     0,   601,   598,   590,   573,     0,     0,
       0,     0,   260,   272,    74,   264,   603,   430,   603,   565,
      75,    73,   603,   254,   301,    72,     0,   253,   407,    71,
     589,     0,   592,    18,     0,     0,   217,     0,   218,   297,
       0,     0,     0,   589,    15,   589,    78,    14,     0,   589,
       0,   594,   594,   245,     0,     0,   594,   563,     0,     0,
      86,     0,    96,   103,   531,   464,   463,   465,   466,     0,
     462,   461,   445,   439,   438,   441,     0,     0,   436,   457,
       0,   468,     0,   434,     0,   443,     0,   472,   473,    53,
     232,   233,     4,   590,     0,     0,     0,     0,     0,     0,
       0,     0,   538,   534,   533,   532,   535,   536,   507,   540,
     552,   508,   556,   555,   551,   531,   496,     0,   500,   505,
     603,   510,   603,   530,     0,   537,   539,   542,   516,     0,
     549,   516,   554,   516,     0,   514,   496,     0,   395,   397,
       0,    92,     0,    84,    81,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   586,   603,   585,     0,   588,   587,
       0,   412,   410,   306,   431,     0,     0,   401,    65,   304,
     325,   113,   114,   115,   472,   473,   496,   492,   323,     0,
     603,     0,     0,     0,   584,   583,    56,     0,   603,   297,
     338,     0,   337,     0,     0,   603,     0,     0,     0,     0,
       0,     0,   110,     0,   603,     0,   320,     0,   126,     0,
       0,   451,   453,     0,     0,   602,   567,     0,   273,   572,
     267,     0,   270,   261,     0,   269,     0,   262,     0,   589,
       0,   589,   603,   603,   255,   266,   589,     0,   303,    52,
       0,     0,     0,     0,     0,     0,    17,   589,   295,    13,
     590,    77,   291,   294,   298,   596,   246,   595,   596,   248,
     299,   564,   102,    94,     0,    89,     0,     0,   603,     0,
     531,   308,   392,   467,     0,     0,   442,   448,   446,   437,
     458,   469,   435,   444,     0,     0,     7,    21,    22,    23,
      24,    25,    37,    36,    50,    51,   498,   544,     0,   589,
     589,     0,     0,   499,     0,   512,   559,   509,     0,   513,
     497,     0,   523,   545,     0,   526,   553,     0,   528,   557,
       0,     0,   603,     0,    28,    30,     0,    31,   589,     0,
      82,    93,    48,    38,    46,     0,   249,   198,    29,     0,
     285,   214,   222,   227,   228,   229,   224,   226,   236,   237,
     230,   231,   207,   208,   234,   235,    35,    34,   591,   223,
     225,   219,   220,   221,   209,   210,   211,   212,   213,   576,
     581,   577,   582,   406,   265,   404,     0,   576,   578,   577,
     579,   405,   603,   576,   577,   265,   603,   603,    39,   249,
     199,    45,   206,    63,    66,     0,     0,     0,   113,   114,
     117,     0,     0,   603,     0,   589,     0,   289,   603,   603,
     418,   603,   339,   580,   296,     0,   576,   577,   603,   341,
     313,   340,   316,   107,   109,     0,   106,   108,     0,     0,
       0,     0,   272,     0,   319,   568,   570,   569,     0,     0,
     274,   268,   603,   571,   566,   252,   251,   256,   257,   259,
     302,   593,    19,     0,    26,   205,    79,    16,   589,   594,
      95,    87,    99,   101,     0,    98,   100,   591,     0,   460,
       0,   449,   215,   216,   538,   355,   589,   348,   495,   494,
     240,   330,     0,   506,   603,   558,   515,   543,   516,   516,
     516,   550,   516,   538,   516,   242,   331,   383,   381,     0,
     380,   379,   278,     0,    91,    85,     0,     0,     0,     0,
       0,   603,     0,     0,     0,     0,   403,    69,   409,     0,
       0,   402,    67,   398,    62,     0,     0,   603,   326,     0,
       0,   409,   329,   562,    57,   419,   420,   603,   421,     0,
     603,   344,     0,     0,   342,     0,     0,   409,     0,     0,
       0,     0,     0,   105,     0,   127,   456,   318,     0,     0,
     271,   275,   263,   603,    11,   292,   247,    97,     0,   385,
       0,     0,   309,   440,   356,   353,   541,     0,     0,   511,
       0,   519,     0,   521,     0,   527,     0,   524,   529,     0,
     378,   591,   591,   502,   503,   603,   603,   363,     0,   547,
     363,   363,   361,     0,     0,   276,    83,    47,   250,   576,
     577,     0,   576,   577,     0,     0,    44,   203,    43,   204,
      70,     0,    41,   201,    42,   202,    68,   399,   400,     0,
       0,     0,     0,   493,   324,     0,     0,   423,   345,     0,
      12,   425,     0,   310,     0,   311,     0,     0,   321,   274,
     603,   258,   391,     0,     0,     0,     0,     0,   351,   241,
     516,   516,   516,   516,   243,     0,     0,     0,   501,     0,
     359,   360,   363,   371,   546,     0,   374,     0,   376,   396,
     277,   409,   239,   238,    40,   200,   413,   411,     0,     0,
       0,   422,     0,   104,   111,     0,   424,     0,   314,   317,
       0,   415,   416,   414,   389,   591,   387,   390,   394,   393,
     357,   354,     0,   349,   520,     0,   517,   522,   525,   384,
     382,   297,     0,   504,   603,     0,   362,   369,   363,   363,
     363,   548,   363,   363,    64,   327,   110,     0,   603,     0,
     603,   603,     0,     0,   386,     0,   352,     0,   516,   580,
     296,   358,     0,   366,     0,   368,     0,   375,     0,   372,
     377,   107,   109,     0,   576,   577,   417,   343,   322,   388,
     350,   518,   363,   363,   363,   363,   105,   367,     0,   364,
     370,   373,   363,   365
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -870,  -870,  -870,   342,  -870,    37,  -870,   -62,   106,  -870,
      41,  -870,  -870,  -154,  -352,   896,    85,   132,  -870,    27,
     192,  -870,  -673,  -870,   -15,    16,  -192,    29,   -72,  -212,
    -436,    -5,  1770,   -82,   849,    13,     4,  -870,  -870,    24,
    -870,  1132,  -870,   307,    84,  -224,  -289,   131,    12,  -870,
    -406,  -229,  -181,    57,  -341,   323,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,     8,  -215,  -463,  -136,  -624,  -870,  -870,  -870,
     111,   164,  -870,  -575,  -870,  -870,  -469,  -870,  -133,  -870,
    -870,    92,  -870,  -870,  -870,   -85,  -870,  -870,  -454,  -870,
    -129,  -870,  -870,  -870,  -870,  -870,    43,    83,  -165,  -870,
    -870,  -870,  -870,  -433,  -201,  -870,   641,  -870,  -870,  -870,
       2,  -870,  -870,  -870,  1740,  2183,   886,  1575,  -870,  -870,
     421,    66,   287,   320,   -41,  -870,  -870,  -870,   280,   -27,
     239,  -248,  -816,  -684,  -524,  -870,   228,  -746,  -548,  -869,
     -38,   326,  -870,  -506,  -870,   273,  -345,  -870,  -870,  -870,
      51,   647,  -442,   593,  -296,  -870,  -870,   -80,  -870,    55,
     -12,   274,  -262,   425,   -16,   -34,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    66,    67,    68,   284,   457,   458,   293,
     294,   510,    70,    71,   603,    72,    73,    74,   676,   212,
      75,    76,   664,   797,    77,    78,   295,    79,    80,    81,
     535,    82,   213,   122,   123,   240,   241,   242,   699,   642,
     206,    84,   300,   607,   643,   274,   500,   501,   275,   276,
     265,   493,   528,   502,   597,    85,   209,   298,   728,   299,
     314,   738,   220,   821,   221,   822,   698,   970,   667,   665,
     902,   452,   287,   461,   690,   813,   814,   227,   746,   926,
     996,   943,   861,   769,   770,   862,   838,   975,   976,   541,
     842,   389,   592,    87,    88,   439,   657,   656,   484,   973,
     679,   807,   906,   910,    89,    90,    91,   327,   328,   545,
      92,    93,    94,   546,   250,   251,   252,   479,    95,    96,
      97,   321,    98,    99,   216,   217,   102,   218,   448,   666,
     446,   367,   368,   369,   864,   865,   370,   371,   372,   756,
     582,   374,   375,   376,   377,   568,   378,   379,   380,   869,
     870,   381,   382,   383,   384,   385,   575,   208,   453,   305,
     503,   487,   269,   128,   671,   645,   456,   451,   430,   507,
     839,   508,   526,   254,   255,   256,   297
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     105,   262,   262,   433,   469,   262,   427,   429,   692,   394,
      86,   282,    86,   125,   125,   281,   243,   215,   215,   277,
     204,   226,   868,   215,   215,   215,   701,   579,   215,   224,
     243,   205,   283,   343,   279,   443,   761,   219,   205,   497,
     529,   106,   249,    69,   531,    69,   373,   373,   431,   310,
     303,   307,   205,   705,   608,   810,   714,   757,    86,   267,
     267,   845,   311,   267,   820,   431,   542,   268,   268,   320,
     253,   268,   215,   266,   266,   759,  1001,   266,   762,   296,
     205,   795,   796,   658,   661,   494,   714,   498,   711,   311,
     120,   120,   711,   373,   373,   267,   267,   534,   120,   517,
     731,   350,   351,   302,   306,   273,   278,   977,   277,   301,
     840,   566,   350,   351,   890,   689,   438,   570,   884,   896,
     525,  -480,   342,     3,   215,   547,    86,   644,   767,  -479,
     809,   652,   483,   434,   655,   466,   646,   121,   121,   120,
     387,   330,   332,   334,   336,   121,   475,   264,   270,  -112,
     289,   271,   349,  1001,  1013,   673,   292,  -104,   432,  -483,
     248,   672,   549,   775,   272,   549,   120,   549,   644,   549,
     652,   549,   768,   567,   391,   432,   685,   244,   -96,   673,
     245,   246,   885,   337,   338,   695,   121,   387,   437,  -296,
     752,   388,   841,  -479,   273,   278,   868,   977,  -482,   868,
     598,   244,  -296,  -296,   245,   246,   390,   514,   247,   -77,
     248,   358,   437,   121,   285,  -483,  -296,    86,   673,   444,
     445,  -104,   901,  -484,   292,   812,   809,  -485,   215,   215,
     -91,   539,   247,   395,   248,  -487,   272,   533,  -296,   361,
     362,   602,  -577,   673,   983,  -296,   362,   470,   471,   734,
     396,  -296,   320,  -101,  -482,  -577,   339,   262,  -296,   520,
    -486,   262,   871,   205,   495,   363,   495,   527,   527,   280,
     504,   363,   527,   248,   497,   830,   868,   714,  -488,  -484,
     215,   602,   602,  -485,   215,   547,  -336,   373,   215,   215,
    -474,  -487,   737,    86,  -478,   435,   971,   280,    86,    86,
    -336,  -573,   761,   447,   480,   572,    86,   420,   421,   422,
     454,   459,   711,   711,   441,   267,  -486,   311,   442,   267,
     296,   718,   719,   757,   534,   591,   463,   268,   124,   124,
     757,   516,   436,   266,  -488,  -336,   124,   806,   373,   522,
     876,   468,  -336,   467,   931,   741,  -474,   472,   519,   428,
    -478,    86,   215,   215,   215,   215,    86,   226,   215,   215,
    -111,   918,   476,   424,  -474,   562,   455,   670,   576,  -111,
     576,   557,   558,   559,   560,    86,   478,   124,  -474,   549,
     556,  -103,    42,   483,    69,    43,   515,   534,   600,   561,
     674,   512,   935,   936,   120,    86,   515,   292,   215,   492,
      86,   311,   544,   609,   124,   426,   486,   505,   511,   849,
     262,  -573,   436,  -474,  -110,   773,   396,  -573,   757,   226,
    -474,   296,   518,   504,  -110,   789,   530,   626,   791,   -76,
      60,   793,   886,   215,   120,   262,   999,   892,   894,  1002,
    -111,   121,   818,   609,   609,  -102,   532,   596,   504,   791,
     262,   650,   596,  -106,   650,   524,   680,   790,   215,   262,
      86,   215,   730,   504,   533,   536,   972,   836,   267,    86,
    -106,   554,   504,   215,   -98,   650,   282,    86,   120,   714,
     708,   121,   215,   832,   644,  -108,   652,    86,   829,   555,
     650,   757,   881,   267,   366,   386,   120,   571,   292,   650,
     495,   495,   757,   577,   741,   891,  -100,   547,   267,   105,
     574,   651,   243,   373,   722,   711,  1043,   267,   586,    86,
     940,   941,   828,   262,   578,   121,   581,   533,    86,   782,
     819,   498,   914,  -105,   205,   651,   504,   267,   650,   954,
     703,   267,   311,   121,   311,   712,   215,   584,   587,   590,
     651,   789,    69,    86,   -97,   589,   717,   663,  -108,   651,
    -105,   727,   460,   650,   534,   790,   891,   681,   267,   521,
     677,   267,   601,   523,   678,   688,   729,   423,   682,  -106,
     543,   267,   594,   396,   704,   700,   716,   604,   721,   -91,
     771,   424,   724,  -108,  -105,   104,   481,   104,   651,   245,
     246,   751,   104,   104,   832,   800,   766,   311,   104,   104,
     104,   244,   282,   104,   245,   246,   783,   460,   776,   120,
     585,   120,   588,   651,   602,   777,   425,   604,   604,   741,
     602,  -481,   124,   426,   778,   602,   602,   462,   418,   419,
     420,   421,   422,   104,   462,  -481,   788,   801,   802,  1021,
     277,   740,   921,   277,   771,   771,   787,   104,   809,   244,
     817,   823,   245,   246,   792,   826,   121,   794,   121,   827,
     834,   277,   124,   837,   215,    86,   808,   811,   248,   811,
    -481,   485,   -96,   847,   120,   843,   811,  -481,   566,   804,
     247,   965,   248,   799,   850,   527,   852,   967,   854,   856,
     205,   243,   904,   825,   533,   905,   215,   873,   897,   104,
     495,   104,   909,   673,   244,   449,   124,   245,   246,  1033,
     913,   282,   915,   205,   923,   824,   922,   927,   786,   424,
     939,   121,  -285,   899,   124,   596,   273,   602,   942,   273,
     945,   492,   660,   662,   947,   247,  -285,   248,  -409,   949,
     951,   464,   576,   956,   957,   786,   335,   273,  -409,   323,
     324,   267,   267,   968,   450,   424,   262,   969,   485,   833,
     978,   426,   979,   715,   660,   662,   985,    86,   924,   504,
     720,  -285,   989,  -103,   311,    86,   609,   990,  -285,   215,
     991,   726,   609,   215,  1004,   771,   747,   609,   609,   580,
     465,  1005,   104,    86,    86,   907,   650,   426,   911,   325,
     326,  -409,   760,   104,   104,   764,  1006,    86,   877,  1015,
     215,  -409,   725,  -591,   267,  -409,  -591,  -591,  1017,    86,
      86,   495,  1022,  1024,  1026,  -409,  1028,    86,  1046,   912,
     959,  1038,  -576,   748,   749,   282,   282,   755,    86,    86,
    -577,   755,  1048,   723,   754,   129,   248,   124,  -409,   124,
    -409,   120,   -97,   576,   576,   104,   651,  -409,  -409,   104,
    -409,   938,   774,   104,   104,  1037,   944,  -409,   104,   860,
    1039,  1036,  1009,   104,   104,   744,   898,   353,   354,   355,
     356,   104,   482,   207,   753,   691,   691,   553,   993,   609,
     323,   324,   900,   357,   998,   211,   211,   758,   121,    86,
      86,   211,  -286,   962,   496,   908,  -297,    86,   811,     0,
       0,     0,   124,     0,     0,     0,  -286,   916,   917,     0,
    -297,     0,     0,     0,     0,   920,   104,   104,   104,   104,
     104,   104,     0,   104,   104,   485,     0,   925,   120,   803,
     325,   326,   485,   120,     0,   473,     0,     0,     0,   282,
     104,  -286,     0,  1014,   267,  -297,     0,  -102,  -286,   424,
     604,     0,  -297,    86,   844,    86,   604,   888,    86,     0,
     104,   604,   604,   104,     0,   104,     0,     0,   104,     0,
     120,   513,   576,   262,     0,   121,   867,   851,   853,   855,
     121,   857,   835,   858,   474,   424,   504,   958,   680,   811,
       0,   426,   440,     0,     0,   966,   215,     0,   104,  -293,
     846,     0,  -293,  -293,  -574,   537,     0,     0,   104,   104,
       0,     0,     0,   650,   946,   948,     0,   121,     0,   424,
     465,   872,     0,   104,     0,   104,   104,   426,   866,  -293,
    -293,   267,  -293,   396,   104,   863,     0,     0,   104,  -106,
       0,   744,   104,   353,   354,   355,   356,   104,   409,   410,
       0,  1010,   104,  1011,   538,     0,  1012,     0,   930,   357,
     932,   426,     0,   604,   933,     0,     0,  -478,  -481,     0,
     798,   887,   889,   651,     0,     0,   893,   895,     0,   124,
       0,  -478,  -481,  -108,   104,     0,   997,   417,   418,   419,
     420,   421,   422,   104,     0,     0,   211,   211,     0,     0,
       0,     0,   887,   889,     0,   893,   895,     0,   462,     0,
     322,   104,   323,   324,  -574,     0,  -478,  -481,   104,  -285,
    -574,   214,   214,  -478,  -481,     0,     0,   214,   263,   263,
     982,     0,   263,  -285,   980,   981,  -106,     0,     0,  -106,
    -106,     0,  1023,  1025,  1027,     0,  1029,  1030,     0,   984,
     986,   987,   988,  1000,  -580,  1003,   506,   509,     0,   286,
     288,     0,   325,   326,   263,   304,   124,  -106,  -285,  -106,
       0,   124,     0,     0,     0,  -285,   340,   341,   955,     0,
    -108,     0,     0,  -108,  -108,     0,  1047,  1049,  1050,  1051,
    1016,     0,   755,  1018,     0,   872,  1053,  -580,   872,   994,
     872,   955,   866,     0,   903,   866,   863,   866,   124,   863,
       0,  -108,   863,  -108,   863,  -576,     0,  -580,     0,     0,
     211,   211,   211,   211,     0,  1040,   564,   565,   214,   691,
    1042,  -580,  1044,  -576,     0,     0,  1045,  1041,  -297,   104,
     104,   744,     0,   353,   354,   355,   356,     0,   872,   780,
       0,     0,  -297,     0,     0,   866,  1052,     0,     0,   357,
    -580,     0,   863,   424,  -580,     0,  -580,  -577,   -99,     0,
    -576,   104,     0,  -580,  -580,   872,     0,   872,  -576,   872,
       0,   872,   866,   359,   866,     0,   866,  -297,   866,   863,
     992,   863,  -576,   863,  -297,   863,  -576,     0,   781,     0,
       0,   872,     0,     0,   424,   426,     0,  -580,   866,  -580,
    -576,   462,  -577,  -576,     0,   863,  -580,   462,   974,     0,
     353,   354,   355,   356,     0,  -576,     0,  -576,     0,   675,
    -577,  -576,   214,   214,  -576,     0,   357,     0,     0,   450,
       0,  1007,   104,  -576,  -577,  -576,   426,   -98,     0,  -576,
     104,   104,  -576,     0,   104,   424,     0,   104,   104,   329,
     323,   324,   104,   104,   548,     0,   323,   324,   104,   104,
     488,   489,   490,   340,     0,  -577,     0,  -577,     0,  -577,
       0,  -100,   104,  -577,   263,   104,  -577,     0,   263,  -577,
    1008,     0,   214,   214,   104,   104,     0,   426,     0,     0,
       0,     0,   104,   331,   323,   324,     0,     0,     0,     0,
     325,   326,     0,   104,   104,     0,   325,   326,     0,   333,
     323,   324,  -577,     0,  -577,   543,   323,   324,  -577,   396,
       0,  -577,     0,     0,   244,     0,     0,   245,   246,     0,
       0,   550,   323,   324,   409,   410,     0,     0,   763,     0,
     353,   354,   355,   356,   325,   326,   214,   214,   214,   214,
       0,   492,   214,   214,   104,   247,   357,   248,     0,   358,
     325,   326,     0,     0,   104,   104,   325,   326,     0,     0,
     573,     0,   104,     0,   418,   419,   420,   421,   422,     0,
     359,   583,   325,   326,   551,   323,   324,   361,   362,     0,
       0,     0,   595,   552,   323,   324,     0,   606,   611,   612,
     613,   614,   615,   616,   617,   618,   619,   620,   621,   622,
     623,   624,   625,   363,   628,   629,   630,   631,   632,   633,
     634,   635,   636,   637,   638,   396,     0,   263,   104,     0,
     104,     0,   211,   104,     0,   325,   326,   659,   659,     0,
     409,   410,     0,     0,   325,   326,     0,   103,     0,   103,
     127,   127,   263,     0,     0,   214,     0,   244,   229,     0,
     245,   246,     0,     0,   211,   659,     0,   263,     0,   659,
     659,   104,   739,   323,   324,     0,   263,   415,   416,   417,
     418,   419,   420,   421,   422,   702,     0,     0,   247,   706,
     248,     0,     0,   707,  -603,   103,   710,     0,   713,   313,
     304,   352,     0,   353,   354,   355,   356,  -603,  -603,  -603,
    -603,  -603,  -603,     0,  -603,     0,     0,   659,     0,   357,
    -603,  -603,   358,   325,   326,     0,   313,   710,     0,     0,
     304,  -603,  -603,     0,  -603,  -603,  -603,  -603,  -603,     0,
     263,     0,     0,   359,   344,   345,   346,   347,   348,   360,
     361,   362,     0,     0,     0,     0,   742,   743,     0,     0,
       0,     0,     0,   103,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   750,     0,     0,   363,     0,   211,   364,
       0,     0,     0,  -603,   352,     0,   353,   354,   355,   356,
     995,     0,   765,     0,     0,   772,     0,  -603,     0,     0,
       0,     0,   357,     0,     0,   358,     0,  -603,     0,     0,
    -603,  -603,   100,     0,   100,   126,   126,   126,     0,     0,
       0,     0,     0,   228,     0,     0,   359,     0,     0,     0,
    -603,  -603,   360,   361,   362,     0,   272,  -603,  -603,  -603,
    -603,     0,    83,     0,    83,   744,     0,   353,   354,   355,
     356,     0,     0,   225,   103,     0,     0,     0,     0,   363,
     100,     0,   364,   357,   312,     0,     0,   744,   214,   353,
     354,   355,   356,     0,     0,   365,     0,     0,     0,     0,
     805,     0,     0,     0,     0,   357,     0,   359,     0,     0,
      83,   312,   352,   745,   353,   354,   355,   356,     0,     0,
     214,     0,     0,     0,     0,     0,     0,     0,     0,   359,
     357,   831,     0,   358,     0,   928,     0,     0,     0,     0,
     710,   304,     0,     0,     0,   639,   640,     0,   100,   641,
     103,     0,     0,     0,   359,   103,   103,     0,     0,     0,
     360,   361,   362,   103,   173,   174,   175,   176,   177,   178,
     179,   180,   181,     0,   313,   182,   183,     0,    83,   184,
     185,   186,   187,     0,     0,     0,     0,   363,     0,     0,
     364,     0,     0,   188,   189,   875,     0,     0,     0,     0,
     659,   878,     0,   263,     0,     0,   659,   659,   103,     0,
       0,   659,   659,   103,   229,     0,   190,   191,   192,   193,
     194,   195,   196,   197,   198,   199,     0,   200,   201,     0,
       0,     0,   103,     0,   214,   202,   272,   659,   659,   100,
     659,   659,     0,     0,   352,     0,   353,   354,   355,   356,
     919,     0,   103,     0,     0,     0,     0,   103,   313,     0,
     610,     0,   357,     0,     0,   358,     0,     0,     0,    83,
     929,     0,     0,     0,     0,     0,   229,     0,     0,     0,
       0,   934,     0,     0,     0,     0,   359,     0,     0,     0,
       0,     0,   360,   361,   362,     0,   950,     0,     0,     0,
     610,   610,     0,     0,     0,     0,   952,   953,     0,     0,
       0,     0,     0,   659,     0,   100,     0,   103,     0,   363,
     100,   100,   364,     0,     0,     0,   103,     0,   100,     0,
       0,     0,     0,     0,   103,   540,   659,     0,     0,   312,
       0,     0,     0,   304,   103,    83,     0,     0,     0,     0,
      83,    83,     0,     0,     0,     0,     0,     0,    83,     0,
       0,     0,     0,     0,     0,   352,     0,   353,   354,   355,
     356,     0,     0,   100,     0,     0,   103,     0,   100,   228,
       0,     0,     0,   357,     0,   103,   358,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   100,   569,   313,
       0,   313,     0,    83,     0,     0,     0,   359,    83,   563,
     103,     0,     0,   360,   361,   362,     0,   100,     0,     0,
       0,     0,   100,   312,     0,     0,     0,    83,     0,     0,
     263,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     363,   228,     0,   364,     0,     0,     0,    83,     0,     0,
       0,     0,    83,     0,     0,   605,     0,     0,     0,     0,
       0,     0,     0,     0,   313,     0,     0,     0,     0,     0,
       0,   627,     0,     0,     0,   101,     0,   101,     0,     0,
       0,     0,   100,     0,     0,     0,     0,     0,     0,     0,
       0,   100,     0,     0,     0,   605,   605,     0,     0,   100,
       0,   396,   397,   398,   399,   400,   401,   402,   403,   100,
     405,   406,    83,     0,     0,     0,   409,   410,     0,     0,
       0,    83,     0,   101,     0,     0,     0,     0,     0,    83,
       0,     0,   103,     0,     0,     0,     0,     0,     0,    83,
       0,   100,     0,     0,     0,     0,     0,     0,     0,     0,
     100,   413,   414,   415,   416,   417,   418,   419,   420,   421,
     422,     0,     0,     0,   312,     0,   312,     0,     0,     0,
       0,    83,     0,     0,     0,   100,     0,     0,     0,     0,
      83,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    83,     0,     0,     0,     0,
       0,     0,   396,   397,   398,   399,   400,   401,   402,   403,
     404,   405,   406,  -604,  -604,     0,     0,   409,   410,   312,
       0,     0,     0,     0,   103,     0,     0,     0,     0,     0,
       0,   313,   103,   610,     0,     0,     0,     0,     0,   610,
       0,     0,     0,     0,   610,   610,     0,     0,     0,     0,
     103,   103,   413,   414,   415,   416,   417,   418,   419,   420,
     421,   422,     0,     0,   103,     0,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,   103,   103,     0,     0,
       0,     0,     0,     0,   103,     0,     0,   100,     0,     0,
       0,     0,     0,     0,     0,   103,   103,   779,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    83,   127,     0,
       0,     0,     0,   127,     0,   396,   397,   398,   399,   400,
     401,   402,   403,   404,   405,   406,   407,   408,     0,     0,
     409,   410,     0,     0,     0,     0,   610,     0,   101,     0,
       0,     0,     0,   101,   101,     0,   103,   103,     0,     0,
     964,   101,     0,     0,   103,     0,     0,     0,     0,     0,
       0,     0,     0,   412,     0,   413,   414,   415,   416,   417,
     418,   419,   420,   421,   422,     0,     0,     0,     0,   100,
       0,     0,     0,  -272,     0,     0,   312,   100,     0,     0,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,   101,     0,     0,     0,   100,   100,     0,     0,    83,
     103,     0,   103,     0,     0,   103,     0,    83,   605,   100,
     101,     0,     0,     0,   605,     0,     0,     0,     0,   605,
     605,   100,   100,     0,     0,    83,    83,     0,     0,   100,
     101,     0,     0,     0,     0,   101,     0,     0,   101,    83,
     100,   100,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    83,    83,     0,     0,     0,     0,     0,     0,    83,
       0,     0,     0,   126,     0,     0,     0,     0,   126,     0,
      83,    83,     0,     0,     0,     0,     0,     0,   101,   101,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
       0,   100,   100,     0,   101,   963,     0,     0,     0,   100,
       0,     0,   101,     0,     0,     0,     0,     0,     0,     0,
       0,   605,   101,     0,     0,     0,     0,     0,     0,     0,
       0,    83,    83,     0,     0,   961,     0,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   101,     0,     0,     0,     0,     0,
       0,     0,     0,   101,     0,   100,     0,   100,     0,     0,
     100,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,     0,     0,    83,     0,    83,     0,     0,
      83,     0,     0,     0,     0,     0,     0,  -603,     4,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,     0,     0,     0,     0,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,     0,     0,
      50,    51,     0,    52,    53,     0,    54,     0,     0,    55,
     101,    56,    57,    58,    59,    60,    61,     0,     0,    62,
    -603,     0,     0,  -603,  -603,     0,     0,   396,  -604,  -604,
    -604,  -604,   401,   402,     0,     0,  -604,  -604,     0,    63,
      64,    65,   409,   410,     0,     0,     0,     0,     0,     0,
       0,  -603,     0,  -603,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   779,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   413,   414,   415,
     416,   417,   418,   419,   420,   421,   422,     0,     0,     0,
       0,     0,   396,   397,   398,   399,   400,   401,   402,   403,
     404,   405,   406,   407,   408,     0,     0,   409,   410,     0,
       0,     0,   101,     0,     0,     0,     0,     0,     0,     0,
     101,   101,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,   101,   101,     0,     0,     0,     0,   101,   101,
     412,     0,   413,   414,   415,   416,   417,   418,   419,   420,
     421,   422,   101,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   101,   101,     0,     0,     0,     0,
       0,     0,   101,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   101,   101,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -474,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -474,  -474,  -474,  -474,  -474,  -474,     0,  -474,     0,
       0,     0,     0,     0,     0,  -474,  -474,     0,     0,     0,
       0,     0,     0,     0,   101,  -474,  -474,     0,  -474,  -474,
    -474,  -474,  -474,     0,   101,   101,     0,     0,     0,     0,
       0,     0,   101,     0,     0,   486,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,
    -474,  -474,  -474,     0,     0,  -474,  -474,  -474,     0,  -474,
    -474,     0,     0,     0,     0,     0,  -474,     0,     0,     0,
       0,  -474,     0,     0,     0,     0,     0,     0,   101,     0,
     101,  -474,     0,   101,  -474,  -474,     0,  -474,  -474,     0,
    -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,  -474,
       0,     0,  -603,     0,     0,  -474,  -474,  -474,  -474,     0,
       0,  -474,  -474,  -474,  -474,  -603,  -603,  -603,  -603,  -603,
    -603,     0,  -603,     0,     0,     0,     0,     0,     0,  -603,
    -603,     0,     0,     0,     0,     0,     0,     0,     0,  -603,
    -603,     0,  -603,  -603,  -603,  -603,  -603,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,  -603,  -603,  -603,     0,     0,  -603,
    -603,  -603,     0,     0,  -603,     0,     0,     0,     0,     0,
    -603,     0,     0,     0,     0,  -603,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -603,     0,     0,  -603,  -603,
       0,     0,  -603,     0,  -603,  -603,  -603,  -603,  -603,  -603,
    -603,  -603,  -603,  -603,     0,     0,  -580,     0,     0,  -603,
    -603,  -603,  -603,     0,   272,  -603,  -603,  -603,  -603,  -580,
    -580,  -580,     0,  -580,  -580,     0,  -580,     0,     0,     0,
       0,     0,  -580,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -580,  -580,     0,  -580,  -580,  -580,  -580,
    -580,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,     0,     0,  -580,  -580,  -580,     0,   784,  -580,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -580,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -580,
       0,     0,  -580,  -580,     0,  -107,  -580,     0,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,     0,     0,
    -580,     0,  -580,  -580,  -580,     0,   -99,     0,     0,  -580,
    -580,  -580,  -580,  -580,  -580,  -580,     0,  -580,  -580,     0,
    -580,     0,     0,     0,     0,     0,  -580,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -580,  -580,     0,
    -580,  -580,  -580,  -580,  -580,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,     0,     0,  -580,  -580,  -580,
       0,   784,  -580,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -580,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -580,     0,     0,  -580,  -580,     0,  -107,
    -580,     0,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,  -580,     0,     0,  -296,     0,  -580,  -580,  -580,     0,
    -580,     0,     0,  -580,  -580,  -580,  -580,  -296,  -296,  -296,
       0,  -296,  -296,     0,  -296,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -296,  -296,     0,  -296,  -296,  -296,  -296,  -296,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -296,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,     0,
       0,  -296,  -296,  -296,     0,   785,  -296,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -296,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -296,     0,     0,
    -296,  -296,     0,  -109,  -296,     0,  -296,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,  -296,  -296,     0,     0,  -296,     0,
       0,  -296,  -296,     0,  -101,     0,     0,  -296,  -296,  -296,
    -296,  -296,  -296,  -296,     0,  -296,  -296,     0,  -296,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -296,  -296,     0,  -296,  -296,
    -296,  -296,  -296,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
    -296,  -296,  -296,     0,     0,  -296,  -296,  -296,     0,   785,
    -296,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -296,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -296,     0,     0,  -296,  -296,     0,  -109,  -296,     0,
    -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,  -296,
       0,     0,     0,     0,     0,  -296,  -296,     0,  -296,     0,
       0,  -296,  -296,  -296,  -296,   290,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,  -603,  -603,  -603,
       0,     0,  -603,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,     0,     0,    50,    51,     0,
      52,    53,     0,    54,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,    62,  -603,     0,     0,
    -603,  -603,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    63,    64,    65,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -603,   290,
    -603,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -603,     0,  -603,  -603,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,     0,
       0,    50,    51,     0,    52,    53,     0,    54,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
      62,  -603,     0,     0,  -603,  -603,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,    65,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -603,   290,  -603,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,  -603,     0,     0,
    -603,    15,  -603,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,     0,     0,    50,    51,     0,    52,    53,
       0,    54,     0,     0,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,    62,  -603,     0,     0,  -603,  -603,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    63,    64,    65,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -603,   290,  -603,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,  -603,     0,     0,  -603,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,     0,     0,    50,
      51,     0,    52,    53,     0,    54,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,    62,  -603,
       0,     0,  -603,  -603,     4,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,    63,    64,
      65,     0,    15,     0,    16,    17,    18,    19,     0,     0,
    -603,     0,  -603,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,     0,     0,    50,    51,     0,    52,
      53,     0,    54,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,     0,     0,    62,  -603,     0,     0,  -603,
    -603,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,    65,     0,     0,
    -603,     0,     0,     0,     0,     0,     0,  -603,   290,  -603,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,  -603,  -603,     0,     0,     0,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,     0,     0,
      50,    51,     0,    52,    53,     0,    54,     0,     0,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,    62,
    -603,     0,     0,  -603,  -603,   290,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    63,
      64,    65,     0,    15,     0,    16,    17,    18,    19,     0,
       0,  -603,     0,  -603,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,     0,     0,   291,    51,     0,
      52,    53,     0,    54,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,    62,  -603,     0,     0,
    -603,  -603,   290,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    63,    64,    65,     0,
      15,     0,    16,    17,    18,    19,     0,  -603,  -603,     0,
    -603,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,     0,     0,    50,    51,     0,    52,    53,     0,
      54,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,    62,  -603,     0,     0,  -603,  -603,   290,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,    63,    64,    65,     0,    15,     0,    16,
      17,    18,    19,     0,  -603,  -603,     0,  -603,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,     0,
       0,    50,    51,     0,    52,    53,     0,    54,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
      62,  -603,     0,     0,  -603,  -603,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      63,    64,    65,     0,     0,  -603,     0,     0,     0,     0,
       0,     0,  -603,   290,  -603,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,  -603,     0,     0,
       0,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,     0,     0,    50,    51,     0,    52,    53,
       0,    54,     0,     0,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,    62,  -603,     0,     0,  -603,  -603,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,    63,    64,    65,     0,    15,     0,
      16,    17,    18,    19,     0,     0,  -603,     0,  -603,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
       0,     0,    50,    51,     0,    52,    53,     0,    54,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,     0,
       0,    62,   244,     0,     0,   245,   246,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,    63,    64,    65,     0,    15,     0,    16,    17,    18,
      19,     0,     0,   247,     0,   248,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,     0,     0,    50,
      51,     0,    52,    53,     0,    54,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,    62,   244,
       0,     0,   245,   246,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,    63,    64,
      65,     0,    15,     0,    16,    17,    18,    19,     0,     0,
     247,     0,   248,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   210,     0,     0,   118,    51,     0,    52,
      53,     0,     0,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,     0,     0,    62,   244,     0,     0,   245,
     246,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    63,    64,    65,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   247,     0,   248,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,   153,     0,     0,     0,   154,   155,   156,
     157,   158,   159,   160,   161,   162,   163,     0,     0,     0,
       0,     0,   164,   165,   166,   167,   168,   169,   170,   171,
      36,    37,   172,    39,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   173,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,     0,
     200,   201,     0,     0,     0,     0,     0,     0,   202,   203,
    -573,  -573,  -573,  -573,  -573,  -573,  -573,  -573,  -573,     0,
       0,     0,     0,     0,     0,     0,  -573,     0,  -573,  -573,
    -573,  -573,     0,  -573,     0,     0,     0,  -573,  -573,  -573,
    -573,  -573,  -573,  -573,     0,     0,  -573,     0,     0,     0,
       0,     0,     0,     0,     0,  -573,  -573,  -573,  -573,  -573,
    -573,  -573,  -573,  -573,     0,  -573,  -573,  -573,     0,     0,
    -573,     0,     0,  -573,  -573,     0,  -573,  -573,  -573,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -573,     0,     0,
    -573,  -573,     0,  -573,  -573,     0,  -573,  -573,  -573,  -573,
       0,  -573,  -573,  -573,  -573,  -573,  -573,     0,     0,  -573,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -573,
    -573,  -573,     0,  -573,     0,     0,     0,     0,     0,  -573,
    -575,  -575,  -575,  -575,  -575,  -575,  -575,  -575,  -575,     0,
       0,     0,     0,     0,     0,     0,  -575,     0,  -575,  -575,
    -575,  -575,     0,  -575,     0,     0,     0,  -575,  -575,  -575,
    -575,  -575,  -575,  -575,     0,     0,  -575,     0,     0,     0,
       0,     0,     0,     0,     0,  -575,  -575,  -575,  -575,  -575,
    -575,  -575,  -575,  -575,     0,  -575,  -575,  -575,     0,     0,
    -575,     0,     0,  -575,  -575,     0,  -575,  -575,  -575,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -575,     0,     0,
    -575,  -575,     0,  -575,  -575,     0,  -575,  -575,  -575,  -575,
       0,  -575,  -575,  -575,  -575,  -575,  -575,     0,     0,  -575,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -575,
    -575,  -575,     0,  -575,     0,     0,     0,     0,     0,  -575,
    -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,     0,
       0,     0,     0,     0,     0,     0,  -574,     0,  -574,  -574,
    -574,  -574,     0,  -574,     0,     0,     0,  -574,  -574,  -574,
    -574,  -574,  -574,  -574,     0,     0,  -574,     0,     0,     0,
       0,     0,     0,     0,     0,  -574,  -574,  -574,  -574,  -574,
    -574,  -574,  -574,  -574,     0,  -574,  -574,  -574,     0,     0,
    -574,     0,     0,  -574,  -574,     0,  -574,  -574,  -574,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -574,     0,     0,
    -574,  -574,     0,  -574,  -574,     0,  -574,  -574,  -574,  -574,
       0,  -574,  -574,  -574,  -574,  -574,  -574,     0,     0,  -574,
       0,     0,     0,     0,     0,     0,  -576,  -576,  -576,  -576,
    -576,  -576,  -576,  -576,  -576,     0,     0,     0,     0,  -574,
    -574,  -574,  -576,  -574,  -576,  -576,  -576,  -576,     0,  -574,
       0,     0,     0,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
       0,     0,  -576,     0,     0,     0,     0,     0,     0,     0,
       0,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,  -576,
       0,  -576,  -576,  -576,     0,     0,  -576,     0,     0,  -576,
    -576,     0,  -576,  -576,  -576,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -576,   815,     0,  -576,  -576,     0,  -576,
    -576,     0,  -576,  -576,  -576,  -576,     0,  -576,  -576,  -576,
    -576,  -576,  -576,     0,     0,  -576,     0,     0,     0,     0,
       0,     0,  -107,  -577,  -577,  -577,  -577,  -577,  -577,  -577,
    -577,  -577,     0,     0,     0,  -576,  -576,  -576,     0,  -577,
       0,  -577,  -577,  -577,  -577,  -576,     0,     0,     0,     0,
    -577,  -577,  -577,  -577,  -577,  -577,  -577,     0,     0,  -577,
       0,     0,     0,     0,     0,     0,     0,     0,  -577,  -577,
    -577,  -577,  -577,  -577,  -577,  -577,  -577,     0,  -577,  -577,
    -577,     0,     0,  -577,     0,     0,  -577,  -577,     0,  -577,
    -577,  -577,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -577,   816,     0,  -577,  -577,     0,  -577,  -577,     0,  -577,
    -577,  -577,  -577,     0,  -577,  -577,  -577,  -577,  -577,  -577,
       0,     0,  -577,     0,     0,     0,     0,     0,     0,  -109,
    -265,  -265,  -265,  -265,  -265,  -265,  -265,  -265,  -265,     0,
       0,     0,  -577,  -577,  -577,     0,  -265,     0,  -265,  -265,
    -265,  -265,  -577,     0,     0,     0,     0,  -265,  -265,  -265,
    -265,  -265,  -265,  -265,     0,     0,  -265,     0,     0,     0,
       0,     0,     0,     0,     0,  -265,  -265,  -265,  -265,  -265,
    -265,  -265,  -265,  -265,     0,  -265,  -265,  -265,     0,     0,
    -265,     0,     0,  -265,  -265,     0,  -265,  -265,  -265,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -265,     0,     0,
    -265,  -265,     0,  -265,  -265,     0,  -265,  -265,  -265,  -265,
       0,  -265,  -265,  -265,  -265,  -265,  -265,     0,     0,  -265,
       0,     0,     0,     0,     0,     0,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,     0,     0,     0,     0,  -265,
    -265,  -265,  -578,     0,  -578,  -578,  -578,  -578,     0,   272,
       0,     0,     0,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
       0,     0,  -578,     0,     0,     0,     0,     0,     0,     0,
       0,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
       0,  -578,  -578,  -578,     0,     0,  -578,     0,     0,  -578,
    -578,     0,  -578,  -578,  -578,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -578,     0,     0,  -578,  -578,     0,  -578,
    -578,     0,  -578,  -578,  -578,  -578,     0,  -578,  -578,  -578,
    -578,  -578,  -578,     0,     0,  -578,     0,     0,     0,     0,
       0,     0,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,     0,     0,     0,     0,  -578,  -578,  -578,  -579,     0,
    -579,  -579,  -579,  -579,     0,  -578,     0,     0,     0,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,     0,     0,  -579,     0,
       0,     0,     0,     0,     0,     0,     0,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,     0,  -579,  -579,  -579,
       0,     0,  -579,     0,     0,  -579,  -579,     0,  -579,  -579,
    -579,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -579,
       0,     0,  -579,  -579,     0,  -579,  -579,     0,  -579,  -579,
    -579,  -579,     0,  -579,  -579,  -579,  -579,  -579,  -579,     0,
       0,  -579,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -579,  -579,  -579,     0,     0,     0,     0,     0,     0,
       0,  -579,   130,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,     0,     0,     0,   154,
     155,   156,   230,   231,   232,   233,   161,   162,   163,     0,
       0,     0,     0,     0,   164,   165,   166,   234,   235,   236,
     237,   171,   315,   316,   238,   317,     0,     0,     0,     0,
       0,     0,   318,     0,     0,     0,     0,     0,     0,   173,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,     0,     0,     0,     0,     0,     0,   319,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,   191,   192,   193,   194,   195,   196,   197,   198,
     199,     0,   200,   201,     0,     0,     0,     0,     0,     0,
     202,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,     0,     0,     0,   154,   155,
     156,   230,   231,   232,   233,   161,   162,   163,     0,     0,
       0,     0,     0,   164,   165,   166,   234,   235,   236,   237,
     171,   315,   316,   238,   317,     0,     0,     0,     0,     0,
       0,   318,     0,     0,     0,     0,     0,     0,   173,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,     0,
       0,     0,     0,     0,     0,     0,   477,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,   192,   193,   194,   195,   196,   197,   198,   199,
       0,   200,   201,     0,     0,     0,     0,     0,     0,   202,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,   149,
     150,   151,   152,   153,     0,     0,     0,   154,   155,   156,
     230,   231,   232,   233,   161,   162,   163,     0,     0,     0,
       0,     0,   164,   165,   166,   234,   235,   236,   237,   171,
       0,     0,   238,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   173,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,     0,
       0,   239,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,     0,
     200,   201,     0,     0,     0,     0,     0,     0,   202,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,     0,     0,     0,   154,   155,   156,   230,
     231,   232,   233,   161,   162,   163,     0,     0,     0,     0,
       0,   164,   165,   166,   234,   235,   236,   237,   171,     0,
       0,   238,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   173,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190,   191,
     192,   193,   194,   195,   196,   197,   198,   199,     0,   200,
     201,     0,     0,     0,     0,     0,     0,   202,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,     0,   107,   108,    18,    19,
       0,     0,     0,     0,     0,   109,   110,   111,    23,    24,
      25,    26,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   115,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   308,     0,     0,   118,    51,
       0,    52,    53,     0,     0,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,    62,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   119,   107,   108,
      18,    19,     0,     0,     0,   309,     0,   109,   110,   111,
      23,    24,    25,    26,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,   115,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   308,     0,     0,
     118,    51,     0,    52,    53,     0,     0,     0,     0,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,    62,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,     0,     0,     0,     0,    15,   119,
      16,    17,    18,    19,     0,     0,     0,   599,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
       0,     0,    50,    51,     0,    52,    53,     0,    54,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,     0,
       0,    62,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    63,    64,    65,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   257,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   499,
       0,     0,     0,     0,     0,   210,     0,     0,   118,    51,
       0,    52,    53,     0,   258,   259,   260,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,    62,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,     0,    63,   261,    65,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,     0,     0,    50,    51,     0,    52,    53,     0,
      54,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,    62,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    63,    64,    65,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   257,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   210,     0,     0,
     118,    51,     0,    52,    53,     0,   258,   259,   260,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,    62,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    63,
     261,    65,    15,     0,   107,   108,    18,    19,     0,     0,
       0,     0,     0,   109,   110,   111,    23,    24,    25,    26,
       0,     0,   112,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,   257,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   210,     0,     0,   118,    51,     0,    52,
      53,     0,   709,   259,   260,    55,     0,    56,    57,    58,
      59,    60,    61,     0,     0,    62,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    63,   261,    65,    15,     0,
     107,   108,    18,    19,     0,     0,     0,     0,     0,   109,
     110,   111,    23,    24,    25,    26,     0,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
     257,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   210,
       0,     0,   118,    51,     0,    52,    53,     0,   258,   259,
       0,    55,     0,    56,    57,    58,    59,    60,    61,     0,
       0,    62,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    63,   261,    65,    15,     0,   107,   108,    18,    19,
       0,     0,     0,     0,     0,   109,   110,   111,    23,    24,
      25,    26,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   257,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   210,     0,     0,   118,    51,
       0,    52,    53,     0,     0,   259,   260,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,    62,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    63,   261,    65,
      15,     0,   107,   108,    18,    19,     0,     0,     0,     0,
       0,   109,   110,   111,    23,    24,    25,    26,     0,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   257,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   210,     0,     0,   118,    51,     0,    52,    53,     0,
     709,   259,     0,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,    62,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    63,   261,    65,    15,     0,   107,   108,
      18,    19,     0,     0,     0,     0,     0,   109,   110,   111,
      23,    24,    25,    26,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   257,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   210,     0,     0,
     118,    51,     0,    52,    53,     0,     0,   259,     0,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,    62,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    63,
     261,    65,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,   112,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   210,     0,     0,   118,    51,     0,    52,
      53,     0,   593,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,     0,     0,    62,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    63,   261,    65,    15,     0,
     107,   108,    18,    19,     0,     0,     0,     0,     0,   109,
     110,   111,    23,    24,    25,    26,     0,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   210,
       0,     0,   118,    51,     0,    52,    53,     0,   258,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,     0,
       0,    62,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    63,   261,    65,    15,     0,   107,   108,    18,    19,
       0,     0,     0,     0,     0,   109,   110,   111,    23,    24,
      25,    26,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   210,     0,     0,   118,    51,
       0,    52,    53,     0,   593,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,    62,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    63,   261,    65,
      15,     0,   107,   108,    18,    19,     0,     0,     0,     0,
       0,   109,   110,   111,    23,    24,    25,    26,     0,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   210,     0,     0,   118,    51,     0,    52,    53,     0,
     874,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,    62,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    63,   261,    65,    15,     0,   107,   108,
      18,    19,     0,     0,     0,     0,     0,   109,   110,   111,
      23,    24,    25,    26,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   210,     0,     0,
     118,    51,     0,    52,    53,     0,   709,     0,     0,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,    62,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    63,
     261,    65,    15,     0,    16,    17,    18,    19,     0,     0,
       0,     0,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   210,     0,     0,   118,    51,     0,    52,
      53,     0,     0,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,     0,     0,    62,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    63,    64,    65,    15,     0,
     107,   108,    18,    19,     0,     0,     0,     0,     0,   109,
     110,   111,    23,    24,    25,    26,     0,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   210,
       0,     0,   118,    51,     0,    52,    53,     0,     0,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,     0,
       0,    62,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    63,   261,    65,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   210,     0,     0,   118,    51,
       0,    52,    53,     0,     0,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,    62,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    63,   261,    65,
      15,     0,   107,   108,    18,    19,     0,     0,     0,     0,
       0,   109,   110,   111,    23,    24,    25,    26,     0,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   113,    35,    36,    37,   114,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
     115,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   116,     0,
       0,   117,     0,     0,   118,    51,     0,    52,    53,     0,
       0,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,    62,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,   119,   107,   108,    18,    19,     0,     0,
       0,     0,     0,   109,   110,   111,    23,    24,    25,    26,
       0,     0,   112,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,   222,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   223,     0,     0,    50,    51,     0,    52,
      53,     0,    54,     0,     0,    55,     0,    56,    57,    58,
      59,    60,    61,     0,     0,    62,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   119,   107,   108,    18,    19,
       0,     0,     0,     0,     0,   109,   110,   111,    23,    24,
      25,    26,     0,     0,   112,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   115,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   308,     0,     0,   392,    51,
       0,    52,    53,     0,   393,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,    62,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   119,   107,   108,
      18,    19,     0,     0,     0,     0,     0,   109,   110,   111,
      23,    24,    25,    26,     0,     0,   112,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   113,    35,
      36,    37,   114,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,   115,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   117,     0,     0,
     118,    51,     0,    52,    53,     0,     0,     0,     0,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,    62,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,   119,
     107,   108,    18,    19,     0,     0,     0,     0,     0,   109,
     110,   111,    23,    24,    25,    26,     0,     0,   112,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,   115,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   308,
       0,     0,   392,    51,     0,    52,    53,     0,     0,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,     0,
       0,    62,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,     0,     0,     0,
      15,   119,   107,   108,    18,    19,     0,     0,     0,     0,
       0,   109,   110,   111,    23,    24,    25,    26,     0,     0,
     112,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
     115,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   937,     0,     0,   118,    51,     0,    52,    53,     0,
       0,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,    62,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,   119,   107,   108,    18,    19,     0,     0,
       0,     0,     0,   109,   110,   111,    23,    24,    25,    26,
       0,     0,   112,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,   222,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   960,     0,     0,   118,    51,     0,    52,
      53,     0,   647,   648,     0,    55,   649,    56,    57,    58,
      59,    60,    61,     0,     0,    62,     0,     0,     0,     0,
       0,   173,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,   119,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,     0,   200,   201,   668,   640,     0,     0,
     669,     0,   202,   272,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   173,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     653,   648,     0,     0,   654,     0,   202,   272,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   173,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,   191,   192,   193,   194,   195,   196,   197,   198,
     199,     0,   200,   201,   683,   640,     0,     0,   684,     0,
     202,   272,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,     0,   200,   201,   686,   648,
       0,     0,   687,     0,   202,   272,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   173,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,     0,
     200,   201,   693,   640,     0,     0,   694,     0,   202,   272,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   173,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,     0,   200,   201,   696,   648,     0,     0,
     697,     0,   202,   272,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   173,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
     732,   640,     0,     0,   733,     0,   202,   272,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   173,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,   191,   192,   193,   194,   195,   196,   197,   198,
     199,     0,   200,   201,   735,   648,     0,     0,   736,     0,
     202,   272,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,     0,   200,   201,   879,   640,
       0,     0,   880,     0,   202,   272,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   173,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,     0,
     200,   201,   882,   648,     0,     0,   883,     0,   202,   272,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   173,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   190,   191,   192,   193,   194,   195,   196,
     197,   198,   199,     0,   200,   201,  1019,   640,     0,     0,
    1020,     0,   202,   272,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   173,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   190,   191,   192,
     193,   194,   195,   196,   197,   198,   199,     0,   200,   201,
    1031,   640,     0,     0,  1032,     0,   202,   272,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   173,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,   191,   192,   193,   194,   195,   196,   197,   198,
     199,     0,   200,   201,  1034,   648,     0,     0,  1035,     0,
     202,   272,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   173,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,     0,   200,   201,   653,   648,
       0,     0,   654,     0,   202,   272,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   173,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   848,     0,     0,     0,     0,     0,     0,     0,   190,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   859,
     200,   201,     0,     0,     0,     0,     0,     0,   202,   396,
     397,   398,   399,   400,   401,   402,   403,   404,   405,   406,
     407,   408,     0,     0,   409,   410,     0,   396,   397,   398,
     399,   400,   401,   402,   403,   404,   405,   406,   407,   408,
       0,     0,   409,   410,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,     0,   413,
     414,   415,   416,   417,   418,   419,   420,   421,   422,     0,
       0,     0,     0,     0,     0,   412,     0,   413,   414,   415,
     416,   417,   418,   419,   420,   421,   422,   396,   397,   398,
     399,   400,   401,   402,   403,   404,   405,   406,   407,   408,
       0,     0,   409,   410,     0,     0,     0,     0,     0,     0,
       0,     0,   396,   397,   398,   399,   400,   401,   402,   403,
     404,   405,   406,   407,   408,     0,     0,   409,   410,     0,
       0,     0,     0,     0,     0,   412,     0,   413,   414,   415,
     416,   417,   418,   419,   420,   421,   422,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     412,   248,   413,   414,   415,   416,   417,   418,   419,   420,
     421,   422,     0,     0,     0,     0,     0,     0,     0,     0,
    -272,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,     0,     0,   409,   410,     0,     0,
     396,   397,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,     0,     0,   409,   410,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   412,
       0,   413,   414,   415,   416,   417,   418,   419,   420,   421,
     422,     0,     0,     0,     0,     0,     0,     0,   412,  -273,
     413,   414,   415,   416,   417,   418,   419,   420,   421,   422,
       0,     0,     0,     0,     0,     0,     0,     0,  -274,   396,
     397,   398,   399,   400,   401,   402,   403,   404,   405,   406,
     407,   408,     0,     0,   409,   410,     0,     0,   396,   397,
     398,   399,   400,   401,   402,   403,   404,   405,   406,   407,
     408,     0,     0,   409,   410,     0,     0,     0,   411,     0,
       0,     0,     0,     0,     0,     0,     0,   412,     0,   413,
     414,   415,   416,   417,   418,   419,   420,   421,   422,     0,
       0,     0,     0,     0,     0,     0,   412,  -275,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   396,   397,
     398,   399,   400,   401,   402,   403,   404,   405,   406,   407,
     408,     0,     0,   409,   410,     0,     0,     0,   491,   396,
     397,   398,   399,   400,   401,   402,   403,   404,   405,   406,
     407,   408,     0,     0,   409,   410,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   412,     0,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   412,     0,   413,
     414,   415,   416,   417,   418,   419,   420,   421,   422,   396,
     397,   398,   399,   400,   401,   402,     0,     0,   405,   406,
       0,     0,     0,     0,   409,   410,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   413,
     414,   415,   416,   417,   418,   419,   420,   421,   422
};

static const yytype_int16 yycheck[] =
{
       2,    16,    17,    88,   219,    20,    86,    87,   471,    81,
       2,    27,     4,     5,     6,    27,    14,     9,    10,    21,
       7,    13,   768,    15,    16,    17,   480,   372,    20,    13,
      28,     7,    28,    67,    22,   117,   584,    10,    14,   268,
     302,     4,    15,     2,   306,     4,    73,    74,    26,    54,
      52,    53,    28,   486,   395,   679,   498,   581,    50,    16,
      17,   745,    54,    20,   688,    26,   314,    16,    17,    56,
      15,    20,    64,    16,    17,   581,   945,    20,   584,    50,
      56,   656,   657,   435,   436,   266,   528,   268,   494,    81,
       5,     6,   498,   120,   121,    52,    53,   309,    13,   291,
     536,    37,    38,    52,    53,    21,    22,   923,   110,    52,
      29,    57,    37,    38,   787,    27,   104,   365,    57,   792,
     301,    91,    67,     0,   116,   326,   118,   423,    80,    91,
      18,   427,    20,    90,   430,   215,   425,     5,     6,    54,
      74,    58,    59,    60,    61,    13,   226,    16,    17,   119,
      55,    20,    92,  1022,   140,   451,    50,   119,   136,    89,
     146,   450,   327,   599,   142,   330,    81,   332,   464,   334,
     466,   336,   124,   119,   140,   136,   465,   113,   140,   475,
     116,   117,   121,    58,    59,   474,    54,   121,   104,    89,
      51,    28,   111,    91,   110,   111,   942,  1013,    89,   945,
     392,   113,    89,   103,   116,   117,   119,   287,   144,   119,
     146,    72,   128,    81,   136,   145,   103,   209,   514,    58,
      59,   119,   797,    89,   118,    17,    18,    89,   220,   221,
     140,   311,   144,   119,   146,    89,   142,   309,   138,   100,
     101,   395,   142,   539,   928,   145,   101,   220,   221,   538,
      72,   138,   239,   140,   145,   142,    64,   272,   145,   293,
      89,   276,   768,   239,   266,   126,   268,   301,   302,   142,
     272,   126,   306,   146,   503,   708,  1022,   719,    89,   145,
     272,   435,   436,   145,   276,   486,    89,   314,   280,   281,
      89,   145,   540,   285,    89,    91,   920,   142,   290,   291,
     103,    26,   850,   123,   249,   367,   298,   129,   130,   131,
      89,    55,   718,   719,    51,   272,   145,   309,    55,   276,
     291,   502,   503,   847,   536,   387,   119,   276,     5,     6,
     854,   290,    91,   276,   145,   138,    13,   678,   365,   298,
     776,    91,   145,    91,   850,   546,   145,    55,   293,    89,
     145,   343,   344,   345,   346,   347,   348,   349,   350,   351,
     119,   824,    25,   103,    89,   349,   145,   449,   370,   119,
     372,   344,   345,   346,   347,   367,   140,    54,   103,   544,
     343,   140,    60,    20,   343,    63,    91,   599,   393,   348,
     452,   285,   861,   862,   309,   387,    91,   291,   390,   140,
     392,   393,   319,   395,    81,   145,    57,   276,   136,   754,
     425,   136,    91,   138,   119,   596,    72,   142,   942,   411,
     145,   392,   143,   425,   119,    91,   137,   411,   652,   119,
     108,   655,   784,   425,   349,   450,   942,   789,   790,   945,
     119,   309,    91,   435,   436,   140,    55,   390,   450,   673,
     465,   427,   395,   119,   430,   139,   458,    91,   450,   474,
     452,   453,   534,   465,   536,   140,   920,   729,   425,   461,
     119,    72,   474,   465,   140,   451,   492,   469,   393,   921,
     492,   349,   474,   712,   780,   119,   782,   479,   703,    72,
     466,  1015,   781,   450,    73,    74,   411,   119,   392,   475,
     502,   503,  1026,    51,   705,    91,   140,   708,   465,   511,
     140,   427,   510,   540,   510,   921,  1022,   474,    51,   511,
     865,   866,   703,   538,   140,   393,   140,   599,   520,   609,
      91,   712,    91,   119,   510,   451,   538,   494,   514,   891,
     483,   498,   534,   411,   536,   494,   538,   140,   140,   119,
     466,    91,   511,   545,   140,    51,   501,    98,   119,   475,
     119,   520,    16,   539,   776,    91,    91,   461,   525,   295,
      15,   528,   140,   299,    13,   469,   525,    89,    16,   119,
      63,   538,   390,    72,    15,   479,   143,   395,   143,   140,
     592,   103,   137,   119,   119,     2,   113,     4,   514,   116,
     117,    15,     9,    10,   833,   667,    15,   599,    15,    16,
      17,   113,   628,    20,   116,   117,   628,    16,   140,   534,
     381,   536,   383,   539,   778,    44,   138,   435,   436,   830,
     784,    89,   309,   145,   119,   789,   790,   212,   127,   128,
     129,   130,   131,    50,   219,   103,   139,   139,    15,   994,
     652,   545,   833,   655,   656,   657,   644,    64,    18,   113,
     139,   139,   116,   117,   652,   137,   534,   655,   536,    15,
     137,   673,   349,   139,   666,   667,   678,   679,   146,   681,
     138,   256,   140,   140,   599,   137,   688,   145,    57,   676,
     144,   906,   146,   666,   140,   729,   140,   912,   140,   140,
     676,   699,    15,   699,   776,    92,   698,   769,   793,   116,
     712,   118,    14,  1009,   113,    89,   393,   116,   117,  1008,
      15,   737,    15,   699,   144,   698,   143,   140,   644,   103,
     140,   599,    89,   795,   411,   678,   652,   891,   140,   655,
     140,   140,   435,   436,   140,   144,   103,   146,    26,    15,
     139,    89,   754,    15,   137,   671,    61,   673,    26,    64,
      65,   718,   719,    15,   138,   103,   781,    15,   343,   718,
      15,   145,   137,   499,   467,   468,   140,   769,   840,   781,
     506,   138,   124,   140,   776,   777,   778,   124,   145,   781,
      55,   517,   784,   785,   137,   797,   568,   789,   790,   374,
     138,    15,   209,   795,   796,   807,   782,   145,   810,   114,
     115,    89,   584,   220,   221,   587,    55,   809,   777,   140,
     812,    89,   515,   113,   781,   103,   116,   117,   140,   821,
     822,   833,   140,   140,   140,   103,   140,   829,   139,   812,
     902,    15,   142,   569,   570,   861,   862,   574,   840,   841,
     142,   578,   140,   511,   574,     6,   146,   534,   136,   536,
     138,   776,   140,   865,   866,   272,   782,   145,   136,   276,
     138,   863,   598,   280,   281,  1011,   868,   145,   285,   768,
    1013,  1010,   962,   290,   291,    51,   794,    53,    54,    55,
      56,   298,   251,     7,   574,   470,   471,    61,   939,   891,
      64,    65,   796,    69,   942,     9,    10,   581,   776,   901,
     902,    15,    89,   905,   267,   809,    89,   909,   920,    -1,
      -1,    -1,   599,    -1,    -1,    -1,   103,   821,   822,    -1,
     103,    -1,    -1,    -1,    -1,   829,   343,   344,   345,   346,
     347,   348,    -1,   350,   351,   520,    -1,   841,   863,   675,
     114,   115,   527,   868,    -1,    89,    -1,    -1,    -1,   975,
     367,   138,    -1,   975,   921,   138,    -1,   140,   145,   103,
     778,    -1,   145,   965,   140,   967,   784,   785,   970,    -1,
     387,   789,   790,   390,    -1,   392,    -1,    -1,   395,    -1,
     905,    89,   994,  1008,    -1,   863,   768,   758,   759,   760,
     868,   762,   728,   764,   138,   103,  1008,   901,  1010,  1011,
      -1,   145,   116,    -1,    -1,   909,  1008,    -1,   425,   113,
     746,    -1,   116,   117,    26,    89,    -1,    -1,   435,   436,
      -1,    -1,    -1,  1009,   870,   871,    -1,   905,    -1,   103,
     138,   768,    -1,   450,    -1,   452,   453,   145,   768,   143,
     144,  1008,   146,    72,   461,   768,    -1,    -1,   465,    16,
      -1,    51,   469,    53,    54,    55,    56,   474,    87,    88,
      -1,   965,   479,   967,   138,    -1,   970,    -1,   850,    69,
     852,   145,    -1,   891,   856,    -1,    -1,    89,    89,    -1,
     665,   784,   785,  1009,    -1,    -1,   789,   790,    -1,   776,
      -1,   103,   103,    16,   511,    -1,   942,   126,   127,   128,
     129,   130,   131,   520,    -1,    -1,   220,   221,    -1,    -1,
      -1,    -1,   815,   816,    -1,   818,   819,    -1,   703,    -1,
      62,   538,    64,    65,   136,    -1,   138,   138,   545,    89,
     142,     9,    10,   145,   145,    -1,    -1,    15,    16,    17,
     140,    -1,    20,   103,   926,   927,   113,    -1,    -1,   116,
     117,    -1,   998,   999,  1000,    -1,  1002,  1003,    -1,   930,
     931,   932,   933,   945,    26,   947,   280,   281,    -1,    47,
      48,    -1,   114,   115,    52,    53,   863,   144,   138,   146,
      -1,   868,    -1,    -1,    -1,   145,    64,    65,   891,    -1,
     113,    -1,    -1,   116,   117,    -1,  1042,  1043,  1044,  1045,
     982,    -1,   939,   985,    -1,   942,  1052,    26,   945,   939,
     947,   914,   942,    -1,   799,   945,   939,   947,   905,   942,
      -1,   144,   945,   146,   947,    26,    -1,    89,    -1,    -1,
     344,   345,   346,   347,    -1,  1017,   350,   351,   116,   824,
    1022,   103,  1024,    26,    -1,    -1,  1028,  1018,    89,   666,
     667,    51,    -1,    53,    54,    55,    56,    -1,   995,    89,
      -1,    -1,   103,    -1,    -1,   995,  1048,    -1,    -1,    69,
      89,    -1,   995,   103,   136,    -1,   138,    26,   140,    -1,
     142,   698,    -1,   145,   103,  1022,    -1,  1024,    89,  1026,
      -1,  1028,  1022,    93,  1024,    -1,  1026,   138,  1028,  1022,
      89,  1024,   103,  1026,   145,  1028,    89,    -1,   138,    -1,
      -1,  1048,    -1,    -1,   103,   145,    -1,   136,  1048,   138,
     103,   906,    26,   142,    -1,  1048,   145,   912,    51,    -1,
      53,    54,    55,    56,    -1,   136,    -1,   138,    -1,   453,
      89,   142,   220,   221,   145,    -1,    69,    -1,    -1,   138,
      -1,    89,   769,   136,   103,   138,   145,   140,    -1,   142,
     777,   778,   145,    -1,   781,   103,    -1,   784,   785,    63,
      64,    65,   789,   790,    62,    -1,    64,    65,   795,   796,
     258,   259,   260,   261,    -1,    89,    -1,   136,    -1,   138,
      -1,   140,   809,   142,   272,   812,   145,    -1,   276,   103,
     138,    -1,   280,   281,   821,   822,    -1,   145,    -1,    -1,
      -1,    -1,   829,    63,    64,    65,    -1,    -1,    -1,    -1,
     114,   115,    -1,   840,   841,    -1,   114,   115,    -1,    63,
      64,    65,   136,    -1,   138,    63,    64,    65,   142,    72,
      -1,   145,    -1,    -1,   113,    -1,    -1,   116,   117,    -1,
      -1,    63,    64,    65,    87,    88,    -1,    -1,    51,    -1,
      53,    54,    55,    56,   114,   115,   344,   345,   346,   347,
      -1,   140,   350,   351,   891,   144,    69,   146,    -1,    72,
     114,   115,    -1,    -1,   901,   902,   114,   115,    -1,    -1,
     368,    -1,   909,    -1,   127,   128,   129,   130,   131,    -1,
      93,   379,   114,   115,    63,    64,    65,   100,   101,    -1,
      -1,    -1,   390,    63,    64,    65,    -1,   395,   396,   397,
     398,   399,   400,   401,   402,   403,   404,   405,   406,   407,
     408,   409,   410,   126,   412,   413,   414,   415,   416,   417,
     418,   419,   420,   421,   422,    72,    -1,   425,   965,    -1,
     967,    -1,   666,   970,    -1,   114,   115,   435,   436,    -1,
      87,    88,    -1,    -1,   114,   115,    -1,     2,    -1,     4,
       5,     6,   450,    -1,    -1,   453,    -1,   113,    13,    -1,
     116,   117,    -1,    -1,   698,   463,    -1,   465,    -1,   467,
     468,  1008,    63,    64,    65,    -1,   474,   124,   125,   126,
     127,   128,   129,   130,   131,   483,    -1,    -1,   144,   487,
     146,    -1,    -1,   491,     0,    50,   494,    -1,   496,    54,
     498,    51,    -1,    53,    54,    55,    56,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    -1,   515,    -1,    69,
      26,    27,    72,   114,   115,    -1,    81,   525,    -1,    -1,
     528,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
     538,    -1,    -1,    93,    40,    41,    42,    43,    44,    99,
     100,   101,    -1,    -1,    -1,    -1,   554,   555,    -1,    -1,
      -1,    -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   571,    -1,    -1,   126,    -1,   812,   129,
      -1,    -1,    -1,    89,    51,    -1,    53,    54,    55,    56,
     140,    -1,   590,    -1,    -1,   593,    -1,   103,    -1,    -1,
      -1,    -1,    69,    -1,    -1,    72,    -1,   113,    -1,    -1,
     116,   117,     2,    -1,     4,     5,     6,     7,    -1,    -1,
      -1,    -1,    -1,    13,    -1,    -1,    93,    -1,    -1,    -1,
     136,   137,    99,   100,   101,    -1,   142,   143,   144,   145,
     146,    -1,     2,    -1,     4,    51,    -1,    53,    54,    55,
      56,    -1,    -1,    13,   209,    -1,    -1,    -1,    -1,   126,
      50,    -1,   129,    69,    54,    -1,    -1,    51,   666,    53,
      54,    55,    56,    -1,    -1,   142,    -1,    -1,    -1,    -1,
     678,    -1,    -1,    -1,    -1,    69,    -1,    93,    -1,    -1,
      50,    81,    51,    99,    53,    54,    55,    56,    -1,    -1,
     698,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    93,
      69,   709,    -1,    72,    -1,    99,    -1,    -1,    -1,    -1,
     718,   719,    -1,    -1,    -1,    51,    52,    -1,   118,    55,
     285,    -1,    -1,    -1,    93,   290,   291,    -1,    -1,    -1,
      99,   100,   101,   298,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,   309,    81,    82,    -1,   118,    85,
      86,    87,    88,    -1,    -1,    -1,    -1,   126,    -1,    -1,
     129,    -1,    -1,    99,   100,   773,    -1,    -1,    -1,    -1,
     778,   779,    -1,   781,    -1,    -1,   784,   785,   343,    -1,
      -1,   789,   790,   348,   349,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,   367,    -1,   812,   141,   142,   815,   816,   209,
     818,   819,    -1,    -1,    51,    -1,    53,    54,    55,    56,
     828,    -1,   387,    -1,    -1,    -1,    -1,   392,   393,    -1,
     395,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,   209,
     848,    -1,    -1,    -1,    -1,    -1,   411,    -1,    -1,    -1,
      -1,   859,    -1,    -1,    -1,    -1,    93,    -1,    -1,    -1,
      -1,    -1,    99,   100,   101,    -1,   874,    -1,    -1,    -1,
     435,   436,    -1,    -1,    -1,    -1,   884,   885,    -1,    -1,
      -1,    -1,    -1,   891,    -1,   285,    -1,   452,    -1,   126,
     290,   291,   129,    -1,    -1,    -1,   461,    -1,   298,    -1,
      -1,    -1,    -1,    -1,   469,   142,   914,    -1,    -1,   309,
      -1,    -1,    -1,   921,   479,   285,    -1,    -1,    -1,    -1,
     290,   291,    -1,    -1,    -1,    -1,    -1,    -1,   298,    -1,
      -1,    -1,    -1,    -1,    -1,    51,    -1,    53,    54,    55,
      56,    -1,    -1,   343,    -1,    -1,   511,    -1,   348,   349,
      -1,    -1,    -1,    69,    -1,   520,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   367,    84,   534,
      -1,   536,    -1,   343,    -1,    -1,    -1,    93,   348,   349,
     545,    -1,    -1,    99,   100,   101,    -1,   387,    -1,    -1,
      -1,    -1,   392,   393,    -1,    -1,    -1,   367,    -1,    -1,
    1008,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     126,   411,    -1,   129,    -1,    -1,    -1,   387,    -1,    -1,
      -1,    -1,   392,    -1,    -1,   395,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   599,    -1,    -1,    -1,    -1,    -1,
      -1,   411,    -1,    -1,    -1,     2,    -1,     4,    -1,    -1,
      -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   461,    -1,    -1,    -1,   435,   436,    -1,    -1,   469,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,   479,
      81,    82,   452,    -1,    -1,    -1,    87,    88,    -1,    -1,
      -1,   461,    -1,    50,    -1,    -1,    -1,    -1,    -1,   469,
      -1,    -1,   667,    -1,    -1,    -1,    -1,    -1,    -1,   479,
      -1,   511,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     520,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,    -1,    -1,   534,    -1,   536,    -1,    -1,    -1,
      -1,   511,    -1,    -1,    -1,   545,    -1,    -1,    -1,    -1,
     520,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   545,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,   599,
      -1,    -1,    -1,    -1,   769,    -1,    -1,    -1,    -1,    -1,
      -1,   776,   777,   778,    -1,    -1,    -1,    -1,    -1,   784,
      -1,    -1,    -1,    -1,   789,   790,    -1,    -1,    -1,    -1,
     795,   796,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,    -1,   809,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   209,    -1,    -1,    -1,   821,   822,    -1,    -1,
      -1,    -1,    -1,    -1,   829,    -1,    -1,   667,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   840,   841,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   667,   863,    -1,
      -1,    -1,    -1,   868,    -1,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    -1,    -1,
      87,    88,    -1,    -1,    -1,    -1,   891,    -1,   285,    -1,
      -1,    -1,    -1,   290,   291,    -1,   901,   902,    -1,    -1,
     905,   298,    -1,    -1,   909,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   120,    -1,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,    -1,    -1,    -1,    -1,   769,
      -1,    -1,    -1,   140,    -1,    -1,   776,   777,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   343,    -1,    -1,    -1,
      -1,   348,    -1,    -1,    -1,   795,   796,    -1,    -1,   769,
     965,    -1,   967,    -1,    -1,   970,    -1,   777,   778,   809,
     367,    -1,    -1,    -1,   784,    -1,    -1,    -1,    -1,   789,
     790,   821,   822,    -1,    -1,   795,   796,    -1,    -1,   829,
     387,    -1,    -1,    -1,    -1,   392,    -1,    -1,   395,   809,
     840,   841,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   821,   822,    -1,    -1,    -1,    -1,    -1,    -1,   829,
      -1,    -1,    -1,   863,    -1,    -1,    -1,    -1,   868,    -1,
     840,   841,    -1,    -1,    -1,    -1,    -1,    -1,   435,   436,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,
      -1,   901,   902,    -1,   461,   905,    -1,    -1,    -1,   909,
      -1,    -1,   469,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   891,   479,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   901,   902,    -1,    -1,   905,    -1,    -1,    -1,   909,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   511,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   520,    -1,   965,    -1,   967,    -1,    -1,
     970,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   545,    -1,
      -1,    -1,    -1,    -1,    -1,   965,    -1,   967,    -1,    -1,
     970,    -1,    -1,    -1,    -1,    -1,    -1,     0,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,
     667,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
     113,    -1,    -1,   116,   117,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,   132,
     133,   134,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   144,    -1,   146,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,   769,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     777,   778,    -1,    -1,    -1,    -1,    -1,   784,    -1,    -1,
      -1,    -1,   789,   790,    -1,    -1,    -1,    -1,   795,   796,
     120,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   809,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   821,   822,    -1,    -1,    -1,    -1,
      -1,    -1,   829,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   840,   841,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   891,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,   901,   902,    -1,    -1,    -1,    -1,
      -1,    -1,   909,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    89,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    98,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,   965,    -1,
     967,   113,    -1,   970,   116,   117,    -1,   119,   120,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,    -1,     0,    -1,    -1,   137,   138,   139,   140,    -1,
      -1,   143,   144,   145,   146,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    27,
      28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    87,
      88,    89,    -1,    -1,    92,    -1,    -1,    -1,    -1,    -1,
      98,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,   116,   117,
      -1,    -1,   120,    -1,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,    -1,     0,    -1,    -1,   137,
     138,   139,   140,    -1,   142,   143,   144,   145,   146,    13,
      14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    89,    -1,    91,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,    -1,   116,   117,    -1,   119,   120,    -1,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,    -1,
       0,    -1,   136,   137,   138,    -1,   140,    -1,    -1,   143,
     144,   145,   146,    13,    14,    15,    -1,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    89,
      -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   113,    -1,    -1,   116,   117,    -1,   119,
     120,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,    -1,     0,    -1,   136,   137,   138,    -1,
     140,    -1,    -1,   143,   144,   145,   146,    13,    14,    15,
      -1,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    89,    -1,    91,    92,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,
     116,   117,    -1,   119,   120,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,    -1,     0,    -1,
      -1,   137,   138,    -1,   140,    -1,    -1,   143,   144,   145,
     146,    13,    14,    15,    -1,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    89,    -1,    91,
      92,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,    -1,   116,   117,    -1,   119,   120,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,    -1,    -1,    -1,    -1,   137,   138,    -1,   140,    -1,
      -1,   143,   144,   145,   146,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      -1,    -1,    18,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    99,    -1,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,    -1,    -1,   112,   113,    -1,    -1,
     116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   132,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,     1,
     146,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    -1,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,    -1,    -1,
     112,   113,    -1,    -1,   116,   117,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   144,     1,   146,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    -1,    -1,
      18,    19,    20,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    99,    -1,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,    -1,    -1,   112,   113,    -1,    -1,   116,   117,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   144,     1,   146,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    15,    -1,    -1,    18,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,   113,
      -1,    -1,   116,   117,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,   132,   133,
     134,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
     144,    -1,   146,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,    -1,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,   113,    -1,    -1,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,   133,   134,    -1,    -1,
     137,    -1,    -1,    -1,    -1,    -1,    -1,   144,     1,   146,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    14,    15,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
     113,    -1,    -1,   116,   117,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,   132,
     133,   134,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,   144,    -1,   146,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    99,    -1,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,    -1,    -1,   112,   113,    -1,    -1,
     116,   117,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,   132,   133,   134,    -1,
      19,    -1,    21,    22,    23,    24,    -1,   143,   144,    -1,
     146,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      99,    -1,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,    -1,    -1,   112,   113,    -1,    -1,   116,   117,     1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,   132,   133,   134,    -1,    19,    -1,    21,
      22,    23,    24,    -1,   143,   144,    -1,   146,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,    -1,    -1,
     112,   113,    -1,    -1,   116,   117,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     132,   133,   134,    -1,    -1,   137,    -1,    -1,    -1,    -1,
      -1,    -1,   144,     1,   146,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    99,    -1,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,    -1,    -1,   112,   113,    -1,    -1,   116,   117,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,   132,   133,   134,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,   144,    -1,   146,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,   113,    -1,    -1,   116,   117,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,   132,   133,   134,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,   144,    -1,   146,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,   113,
      -1,    -1,   116,   117,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,   132,   133,
     134,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
     144,    -1,   146,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    -1,    -1,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,   113,    -1,    -1,   116,
     117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   132,   133,   134,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,    -1,   146,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,   142,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,   100,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
     133,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,   142,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,   100,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
     133,   134,    -1,   136,    -1,    -1,    -1,    -1,    -1,   142,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,   100,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,
     133,   134,    19,   136,    21,    22,    23,    24,    -1,   142,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    91,    -1,    93,    94,    -1,    96,
      97,    -1,    99,   100,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,
      -1,    -1,   119,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,   132,   133,   134,    -1,    19,
      -1,    21,    22,    23,    24,   142,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    91,    -1,    93,    94,    -1,    96,    97,    -1,    99,
     100,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,   119,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,   132,   133,   134,    -1,    19,    -1,    21,    22,
      23,    24,   142,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,   100,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,
     133,   134,    19,    -1,    21,    22,    23,    24,    -1,   142,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,   100,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   132,   133,   134,    19,    -1,
      21,    22,    23,    24,    -1,   142,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,   100,
     101,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   132,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
      -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,
      -1,   104,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    -1,
      -1,    55,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,   141,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    -1,    -1,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,   132,    21,    22,
      23,    24,    -1,    -1,    -1,   140,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    -1,    -1,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,   132,
      21,    22,    23,    24,    -1,    -1,    -1,   140,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   132,   133,   134,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,   100,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    -1,   132,   133,   134,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      99,    -1,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   132,   133,   134,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,   100,   101,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,
     133,   134,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,   100,   101,   102,    -1,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   132,   133,   134,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,   100,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   132,   133,   134,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    -1,   100,   101,   102,    -1,   104,
     105,   106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   132,   133,   134,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      99,   100,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   132,   133,   134,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    -1,   100,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,
     133,   134,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,    -1,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   132,   133,   134,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   132,   133,   134,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   132,   133,   134,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      99,    -1,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   132,   133,   134,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,
     133,   134,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    -1,    -1,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   132,   133,   134,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    -1,    -1,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   132,   133,   134,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    -1,    -1,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   132,   133,   134,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      -1,    -1,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,    -1,    -1,   112,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   132,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    99,    -1,    -1,   102,    -1,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   132,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,   132,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    -1,    -1,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   132,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    -1,    -1,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,   132,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      -1,    -1,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,    -1,    -1,   112,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   132,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,
      97,    -1,    51,    52,    -1,   102,    55,   104,   105,   106,
     107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,   132,    85,    86,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,   133,   134,    51,    52,    -1,    -1,
      55,    -1,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,
      85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,   100,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    -1,   133,   134,
      51,    52,    -1,    -1,    55,    -1,   141,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    51,    52,    -1,    -1,    55,    -1,
     141,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,    -1,   133,   134,    51,    52,
      -1,    -1,    55,    -1,   141,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
     133,   134,    51,    52,    -1,    -1,    55,    -1,   141,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,   133,   134,    51,    52,    -1,    -1,
      55,    -1,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,
      85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,   100,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    -1,   133,   134,
      51,    52,    -1,    -1,    55,    -1,   141,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    51,    52,    -1,    -1,    55,    -1,
     141,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,    -1,   133,   134,    51,    52,
      -1,    -1,    55,    -1,   141,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
     133,   134,    51,    52,    -1,    -1,    55,    -1,   141,   142,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,   133,   134,    51,    52,    -1,    -1,
      55,    -1,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,
      85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    99,   100,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    -1,   133,   134,
      51,    52,    -1,    -1,    55,    -1,   141,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    51,    52,    -1,    -1,    55,    -1,
     141,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,
      87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,    -1,   133,   134,    51,    52,
      -1,    -1,    55,    -1,   141,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    44,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,   141,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    -1,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
      -1,    -1,    -1,    -1,    -1,   120,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,   120,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,   146,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     140,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,   140,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   140,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    -1,    -1,    -1,    92,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   120,   140,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    87,    88,    -1,    -1,    -1,    92,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    -1,    -1,    87,    88,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   148,   149,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    19,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      58,    59,    60,    63,    66,    67,    69,    70,    71,    90,
      93,    94,    96,    97,    99,   102,   104,   105,   106,   107,
     108,   109,   112,   132,   133,   134,   150,   151,   152,   157,
     159,   160,   162,   163,   164,   167,   168,   171,   172,   174,
     175,   176,   178,   179,   188,   202,   219,   240,   241,   251,
     252,   253,   257,   258,   259,   265,   266,   267,   269,   270,
     271,   272,   273,   274,   310,   323,   152,    21,    22,    30,
      31,    32,    39,    51,    55,    69,    87,    90,    93,   132,
     163,   164,   180,   181,   202,   219,   271,   274,   310,   181,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    16,    17,    18,    19,    20,    21,    22,
      23,    24,    25,    26,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    45,    46,    47,    48,    49,    50,
      51,    52,    55,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    81,    82,    85,    86,    87,    88,    99,   100,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     133,   134,   141,   142,   182,   186,   187,   273,   304,   203,
      90,   162,   166,   179,   188,   219,   271,   272,   274,   166,
     209,   211,    69,    90,   172,   179,   219,   224,   271,   274,
      33,    34,    35,    36,    48,    49,    50,    51,    55,   104,
     182,   183,   184,   267,   113,   116,   117,   144,   146,   166,
     261,   262,   263,   316,   320,   321,   322,    51,    99,   100,
     101,   133,   171,   188,   194,   197,   200,   253,   307,   309,
     194,   194,   142,   191,   192,   195,   196,   323,   191,   195,
     142,   317,   321,   183,   153,   136,   188,   219,   188,    55,
       1,    93,   155,   156,   157,   173,   174,   323,   204,   206,
     189,   200,   307,   323,   188,   306,   307,   323,    90,   140,
     178,   219,   271,   274,   207,    53,    54,    56,    63,   108,
     182,   268,    62,    64,    65,   114,   115,   254,   255,    63,
     254,    63,   254,    63,   254,    61,   254,    58,    59,   167,
     188,   188,   316,   322,    40,    41,    42,    43,    44,    92,
      37,    38,    51,    53,    54,    55,    56,    69,    72,    93,
      99,   100,   101,   126,   129,   142,   277,   278,   279,   280,
     283,   284,   285,   286,   288,   289,   290,   291,   293,   294,
     295,   298,   299,   300,   301,   302,   277,   278,    28,   238,
     119,   140,    93,    99,   175,   119,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    87,
      88,    92,   120,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    89,   103,   138,   145,   314,    89,   314,
     315,    26,   136,   242,   253,    91,    91,   191,   195,   242,
     162,    51,    55,   180,    58,    59,   277,   123,   275,    89,
     138,   314,   218,   305,    89,   145,   313,   154,   155,    55,
      16,   220,   320,   119,    89,   138,   314,    91,    91,   220,
     166,   166,    55,    89,   138,   314,    25,   108,   140,   264,
     316,   113,   263,    20,   245,   320,    57,   308,   188,   188,
     188,    92,   140,   198,   199,   323,   308,   198,   199,    84,
     193,   194,   200,   307,   323,   194,   162,   316,   318,   162,
     158,   136,   155,    89,   314,    91,   157,   173,   143,   316,
     322,   318,   157,   318,   139,   199,   319,   322,   199,   319,
     137,   319,    55,   175,   176,   177,   140,    89,   138,   314,
     142,   236,   288,    63,   254,   256,   260,   261,    62,   255,
      63,    63,    63,    61,    72,    72,   152,   166,   166,   166,
     166,   157,   172,   179,   162,   162,    57,   119,   292,    84,
     288,   119,   154,   188,   140,   303,   323,    51,   140,   303,
     320,   140,   287,   188,   140,   287,    51,   140,   287,    51,
     119,   154,   239,    99,   167,   188,   200,   201,   173,   140,
     178,   140,   160,   161,   167,   179,   188,   190,   201,   219,
     274,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   172,   179,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,    51,
      52,    55,   186,   191,   311,   312,   193,    51,    52,    55,
     186,   191,   311,    51,    55,   311,   244,   243,   161,   188,
     190,   161,   190,    98,   169,   216,   276,   215,    51,    55,
     180,   311,   193,   311,   154,   162,   165,    15,    13,   247,
     323,   155,    16,    51,    55,   193,    51,    55,   155,    27,
     221,   320,   221,    51,    55,   193,    51,    55,   213,   185,
     155,   245,   188,   200,    15,   260,   188,   188,   317,    99,
     188,   197,   307,   188,   309,   318,   143,   316,   199,   199,
     318,   143,   183,   150,   137,   190,   318,   157,   205,   307,
     175,   177,    51,    55,   193,    51,    55,   288,   208,    63,
     155,   261,   188,   188,    51,    99,   225,   293,   318,   318,
     188,    15,    51,   280,   285,   302,   286,   291,   298,   300,
     293,   295,   300,    51,   293,   188,    15,    80,   124,   230,
     231,   323,   188,   199,   318,   177,   140,    44,   119,    44,
      89,   138,   314,   317,    91,    91,   191,   195,   139,    91,
      91,   192,   195,   192,   195,   230,   230,   170,   320,   166,
     154,   139,    15,   318,   182,   188,   201,   248,   323,    18,
     223,   323,    17,   222,   223,    91,    91,   139,    91,    91,
     223,   210,   212,   139,   166,   183,   137,    15,   199,   220,
     260,   188,   198,   307,   137,   318,   319,   139,   233,   317,
      29,   111,   237,   137,   140,   290,   318,   140,    44,   303,
     140,   287,   140,   287,   140,   287,   140,   287,   287,    44,
     227,   229,   232,   279,   281,   282,   285,   293,   294,   296,
     297,   300,   302,   154,    99,   188,   177,   157,   188,    51,
      55,   193,    51,    55,    57,   121,   161,   190,   167,   190,
     169,    91,   161,   190,   161,   190,   169,   242,   238,   154,
     155,   230,   217,   320,    15,    92,   249,   323,   155,    14,
     250,   323,   166,    15,    91,    15,   155,   155,   221,   188,
     155,   199,   143,   144,   154,   155,   226,   140,    99,   188,
     293,   300,   293,   293,   188,   233,   233,    90,   219,   140,
     303,   303,   140,   228,   219,   140,   228,   140,   228,    15,
     188,   139,   188,   188,   161,   190,    15,   137,   155,   154,
      90,   179,   219,   271,   274,   220,   155,   220,    15,    15,
     214,   223,   245,   246,    51,   234,   235,   289,    15,   137,
     293,   293,   140,   290,   287,   140,   287,   287,   287,   124,
     124,    55,    89,   281,   285,   140,   227,   228,   297,   300,
     293,   296,   300,   293,   137,    15,    55,    89,   138,   314,
     155,   155,   155,   140,   317,   140,   293,   140,   293,    51,
      55,   303,   140,   228,   140,   228,   140,   228,   140,   228,
     228,    51,    55,   193,    51,    55,   247,   222,    15,   235,
     293,   287,   293,   300,   293,   293,   139,   228,   140,   228,
     228,   228,   293,   228
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   147,   149,   148,   150,   151,   151,   151,   151,   152,
     153,   152,   154,   155,   156,   156,   156,   156,   158,   157,
     157,   157,   157,   157,   157,   157,   157,   157,   157,   157,
     157,   157,   157,   157,   159,   159,   159,   159,   160,   160,
     160,   160,   160,   160,   160,   160,   161,   161,   161,   162,
     162,   162,   162,   162,   162,   163,   165,   164,   166,   167,
     167,   168,   168,   170,   169,   171,   171,   171,   171,   171,
     171,   171,   171,   171,   171,   171,   172,   172,   173,   173,
     174,   174,   174,   174,   174,   174,   174,   174,   174,   174,
     175,   175,   176,   176,   177,   177,   178,   178,   178,   178,
     178,   178,   178,   178,   179,   179,   179,   179,   179,   179,
     179,   179,   179,   180,   180,   181,   181,   181,   182,   182,
     182,   182,   182,   183,   183,   184,   185,   184,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   186,   186,
     186,   186,   186,   186,   186,   186,   186,   186,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   187,   187,
     187,   187,   187,   187,   187,   187,   187,   187,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   189,   189,   189,   189,   190,
     190,   191,   191,   192,   192,   193,   193,   193,   193,   193,
     194,   194,   194,   194,   194,   196,   195,   197,   198,   198,
     199,   199,   200,   200,   200,   200,   201,   201,   201,   202,
     202,   202,   202,   202,   202,   202,   202,   202,   203,   202,
     204,   205,   202,   206,   202,   202,   202,   202,   202,   202,
     202,   202,   202,   202,   202,   202,   202,   207,   208,   202,
     202,   202,   209,   210,   202,   211,   212,   202,   202,   202,
     213,   214,   202,   215,   202,   216,   217,   202,   218,   202,
     202,   202,   202,   202,   202,   202,   219,   220,   220,   220,
     221,   221,   222,   222,   223,   223,   224,   224,   225,   225,
     225,   225,   225,   225,   225,   225,   226,   225,   227,   227,
     227,   227,   228,   228,   229,   229,   229,   229,   229,   229,
     229,   229,   229,   229,   229,   229,   229,   229,   229,   230,
     230,   232,   231,   231,   231,   233,   233,   234,   234,   235,
     235,   236,   236,   237,   237,   239,   238,   240,   240,   240,
     240,   241,   241,   241,   241,   241,   241,   241,   241,   241,
     243,   242,   244,   242,   245,   246,   246,   247,   247,   248,
     248,   248,   249,   249,   250,   250,   251,   251,   251,   251,
     252,   252,   253,   253,   253,   253,   254,   254,   255,   256,
     255,   255,   255,   257,   257,   258,   258,   259,   260,   260,
     261,   261,   262,   262,   263,   264,   263,   265,   265,   266,
     266,   267,   268,   268,   268,   268,   268,   268,   269,   269,
     270,   270,   270,   270,   271,   271,   271,   271,   271,   272,
     272,   273,   273,   273,   273,   273,   273,   273,   273,   274,
     274,   275,   276,   275,   277,   277,   278,   278,   279,   280,
     280,   281,   281,   282,   282,   283,   283,   284,   284,   285,
     285,   286,   286,   286,   286,   287,   287,   288,   288,   288,
     288,   288,   288,   288,   288,   288,   288,   288,   288,   288,
     288,   288,   289,   289,   289,   289,   289,   290,   290,   291,
     292,   291,   293,   293,   294,   295,   296,   297,   297,   298,
     298,   299,   299,   300,   300,   301,   301,   302,   303,   303,
     304,   305,   304,   306,   306,   307,   307,   308,   308,   309,
     309,   309,   309,   310,   310,   310,   311,   311,   311,   311,
     312,   312,   312,   313,   313,   314,   314,   315,   315,   316,
     316,   317,   317,   318,   319,   319,   319,   320,   320,   320,
     321,   322,   322,   323
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     1,     1,     3,     3,     3,     3,     3,     3,
       6,     5,     5,     5,     5,     3,     1,     3,     1,     1,
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
       6,     5,     5,     5,     5,     4,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     4,     4,     2,     2,     3,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     2,     2,     3,     3,     3,     3,     6,     6,
       4,     6,     4,     6,     1,     1,     2,     4,     2,     1,
       3,     3,     3,     1,     1,     1,     2,     2,     4,     2,
       1,     2,     2,     4,     1,     0,     2,     2,     2,     1,
       1,     3,     1,     2,     3,     4,     3,     4,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     4,
       0,     0,     5,     0,     3,     3,     3,     2,     3,     3,
       1,     2,     4,     3,     2,     1,     2,     0,     0,     5,
       6,     6,     0,     0,     7,     0,     0,     7,     5,     4,
       0,     0,     9,     0,     6,     0,     0,     8,     0,     5,
       4,     4,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     1,     5,     1,     2,     1,     1,     1,     4,
       6,     3,     5,     2,     4,     1,     0,     4,     4,     2,
       2,     1,     2,     0,     6,     8,     4,     6,     4,     3,
       6,     2,     4,     6,     2,     4,     2,     4,     1,     1,
       1,     0,     4,     1,     4,     1,     4,     1,     3,     1,
       1,     4,     1,     3,     3,     0,     5,     2,     4,     5,
       5,     2,     4,     4,     3,     3,     3,     2,     1,     4,
       0,     5,     0,     5,     5,     1,     1,     6,     1,     1,
       1,     1,     2,     1,     2,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     2,     3,     1,     2,     1,     0,
       4,     1,     2,     2,     3,     2,     3,     1,     1,     2,
       1,     2,     1,     2,     1,     0,     4,     2,     3,     1,
       4,     2,     1,     1,     1,     1,     1,     2,     2,     3,
       1,     1,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     0,     4,     3,     3,     1,     2,     2,     2,
       1,     2,     1,     1,     3,     1,     3,     1,     1,     2,
       1,     4,     2,     2,     1,     2,     0,     6,     8,     4,
       6,     4,     6,     2,     4,     6,     2,     4,     2,     4,
       1,     0,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     4,     1,     3,     2,     2,     2,     1,     3,     1,
       3,     1,     1,     2,     1,     1,     1,     2,     2,     1,
       1,     0,     4,     1,     2,     1,     3,     1,     2,     3,
       3,     3,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     0,     1,     2,     0,     1,     1,     1,     1,     1,
       1,     1,     2,     0
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
#line 1534 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 5782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3:
#line 1539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 5791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4:
#line 1546 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5:
#line 1552 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6:
#line 1556 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5816 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7:
#line 1561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8:
#line 1565 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10:
#line 1572 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 5841 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11:
#line 1577 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 5852 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12:
#line 1589 "mrbgems/mruby-compiler/core/parse.y"
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
#line 5878 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13:
#line 1613 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5886 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14:
#line 1619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5894 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15:
#line 1623 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16:
#line 1628 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5911 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17:
#line 1632 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 5919 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18:
#line 1637 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 5925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19:
#line 1638 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 5933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20:
#line 1642 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 5941 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21:
#line 1646 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 5949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22:
#line 1650 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 5957 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23:
#line 1654 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 5965 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24:
#line 1658 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 5973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25:
#line 1662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5981 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26:
#line 1666 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 5990 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28:
#line 1672 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5998 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29:
#line 1676 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30:
#line 1680 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6014 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31:
#line 1684 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6022 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34:
#line 1692 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_asgn(p, (yyvsp[0].nd), (yyvsp[-2].nd));
                    }
#line 6031 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35:
#line 1697 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_masgn(p, (yyvsp[0].nd), (yyvsp[-2].nd));
                    }
#line 6040 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36:
#line 1702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[0].nd), (yyvsp[-2].nd));
                    }
#line 6048 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37:
#line 1706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[0].nd), (yyvsp[-2].nd));
                    }
#line 6056 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38:
#line 1712 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6064 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39:
#line 1716 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40:
#line 1720 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), MRB_QSYM(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6080 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41:
#line 1724 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6088 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42:
#line 1728 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6096 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43:
#line 1732 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 6105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44:
#line 1737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6113 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 45:
#line 1741 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6122 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47:
#line 1749 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6130 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50:
#line 1758 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6138 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51:
#line 1762 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6146 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52:
#line 1766 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6154 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53:
#line 1770 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6162 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55:
#line 1778 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 6173 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56:
#line 1787 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 6181 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 57:
#line 1791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 6194 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58:
#line 1802 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 6205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62:
#line 1816 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6213 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63:
#line 1822 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 6222 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64:
#line 1829 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 6232 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65:
#line 1837 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6240 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66:
#line 1841 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 6249 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67:
#line 1846 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6257 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68:
#line 1850 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 6266 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69:
#line 1855 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 6274 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70:
#line 1859 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 6283 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71:
#line 1864 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 6291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72:
#line 1868 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6299 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73:
#line 1872 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6307 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74:
#line 1876 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75:
#line 1880 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76:
#line 1886 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6331 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77:
#line 1890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79:
#line 1897 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6347 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80:
#line 1903 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81:
#line 1907 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 6363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82:
#line 1911 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83:
#line 1915 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84:
#line 1919 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 6387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85:
#line 1923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 6395 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86:
#line 1927 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 6403 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87:
#line 1931 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88:
#line 1935 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 6419 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89:
#line 1939 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 6427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91:
#line 1946 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 6435 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92:
#line 1952 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 6443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93:
#line 1956 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 6451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94:
#line 1962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95:
#line 1966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 6467 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96:
#line 1972 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6475 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97:
#line 1976 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 6483 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98:
#line 1980 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6491 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99:
#line 1984 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6499 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100:
#line 1988 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6507 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101:
#line 1992 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6517 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102:
#line 1998 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6527 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103:
#line 2004 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104:
#line 2011 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105:
#line 2015 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 6552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106:
#line 2019 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107:
#line 2023 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108:
#line 2027 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6576 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109:
#line 2031 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6586 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110:
#line 2037 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6596 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111:
#line 2043 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6605 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112:
#line 2048 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 6613 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113:
#line 2054 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 6621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115:
#line 2061 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((node*)1, nsym((yyvsp[0].id)));
                    }
#line 6629 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116:
#line 2065 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((node*)0, nsym((yyvsp[0].id)));
                    }
#line 6637 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117:
#line 2069 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 6646 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121:
#line 2079 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6655 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122:
#line 2084 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6664 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125:
#line 2095 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 6672 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126:
#line 2098 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 6678 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127:
#line 2099 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 6686 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128:
#line 2104 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(or);     }
#line 6692 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129:
#line 2105 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(xor);    }
#line 6698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130:
#line 2106 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(and);    }
#line 6704 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131:
#line 2107 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(cmp);    }
#line 6710 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132:
#line 2108 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(eq);     }
#line 6716 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133:
#line 2109 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(eqq);    }
#line 6722 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134:
#line 2110 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(match);  }
#line 6728 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135:
#line 2111 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(nmatch); }
#line 6734 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136:
#line 2112 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(gt);     }
#line 6740 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137:
#line 2113 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(ge);     }
#line 6746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138:
#line 2114 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(lt);     }
#line 6752 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139:
#line 2115 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(le);     }
#line 6758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140:
#line 2116 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(neq);    }
#line 6764 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141:
#line 2117 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(lshift); }
#line 6770 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142:
#line 2118 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(rshift); }
#line 6776 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143:
#line 2119 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(add);    }
#line 6782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144:
#line 2120 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(sub);    }
#line 6788 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145:
#line 2121 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mul);    }
#line 6794 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146:
#line 2122 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mul);    }
#line 6800 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147:
#line 2123 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(div);    }
#line 6806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148:
#line 2124 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mod);    }
#line 6812 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149:
#line 2125 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(pow);    }
#line 6818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150:
#line 2126 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(pow);    }
#line 6824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151:
#line 2127 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(not);    }
#line 6830 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152:
#line 2128 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(neg);    }
#line 6836 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153:
#line 2129 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(plus);   }
#line 6842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154:
#line 2130 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(minus);  }
#line 6848 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155:
#line 2131 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(aref);   }
#line 6854 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156:
#line 2132 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(aset);   }
#line 6860 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157:
#line 2133 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(tick);   }
#line 6866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198:
#line 2151 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6874 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199:
#line 2155 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6882 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200:
#line 2159 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), MRB_QSYM(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6890 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201:
#line 2163 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6898 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202:
#line 2167 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6906 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203:
#line 2171 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6914 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204:
#line 2175 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6923 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205:
#line 2180 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6932 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206:
#line 2185 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6941 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207:
#line 2190 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208:
#line 2194 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6957 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209:
#line 2198 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 6965 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210:
#line 2202 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 6973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211:
#line 2206 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 6981 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212:
#line 2210 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 6989 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213:
#line 2214 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 6997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214:
#line 2218 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 7005 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215:
#line 2222 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216:
#line 2226 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217:
#line 2230 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 7029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218:
#line 2234 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "-@");
                    }
#line 7037 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219:
#line 2238 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 7045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220:
#line 2242 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 7053 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221:
#line 2246 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 7061 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222:
#line 2250 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 7069 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223:
#line 2254 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 7077 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224:
#line 2258 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 7085 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225:
#line 2262 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 7093 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226:
#line 2266 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 7101 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227:
#line 2270 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 7109 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228:
#line 2274 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 7117 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229:
#line 2278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 7125 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230:
#line 2282 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 7133 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231:
#line 2286 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 7141 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232:
#line 2290 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7149 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233:
#line 2294 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 7157 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234:
#line 2298 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 7165 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235:
#line 2302 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 7173 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236:
#line 2306 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7181 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237:
#line 2310 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7189 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238:
#line 2314 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7197 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239:
#line 2318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240:
#line 2322 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7217 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241:
#line 2330 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7230 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242:
#line 2339 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243:
#line 2348 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7257 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244:
#line 2358 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7265 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246:
#line 2365 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7274 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247:
#line 2370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd)));
                    }
#line 7282 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248:
#line 2374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 249:
#line 2381 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7299 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250:
#line 2385 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7309 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251:
#line 2393 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7317 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252:
#line 2397 "mrbgems/mruby-compiler/core/parse.y"
                    {
#if 1
                      mrb_sym r = MRB_QSYM(mul);
                      mrb_sym b = MRB_QSYM(and);
                      if (local_var_p(p, r)  && local_var_p(p, b)) {
                        (yyval.nd) = cons(list1(new_splat(p, new_lvar(p, r))),
                                  new_block_arg(p, new_lvar(p, b)));
                      }
#else
                      mrb_sym r = MRB_QSYM(mul);
                      mrb_sym k = MRB_QSYM(pow);
                      mrb_sym b = MRB_QSYM(and);
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
#line 7345 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257:
#line 2429 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7354 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 258:
#line 2434 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 259:
#line 2439 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7372 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 260:
#line 2446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(list1((yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7382 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 261:
#line 2452 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7391 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262:
#line 2457 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263:
#line 2462 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7409 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264:
#line 2467 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7418 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265:
#line 2473 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 7427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266:
#line 2478 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7436 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267:
#line 2485 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 7444 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268:
#line 2491 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7452 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269:
#line 2495 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 7460 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272:
#line 2505 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[0].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273:
#line 2511 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274:
#line 2517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275:
#line 2522 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 276:
#line 2529 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7507 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277:
#line 2534 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7516 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278:
#line 2539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 7525 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 286:
#line 2553 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 7533 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 287:
#line 2557 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 7541 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 288:
#line 2561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7550 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 289:
#line 2567 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7559 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 290:
#line 2572 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 291:
#line 2576 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 7574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 292:
#line 2577 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7583 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293:
#line 2581 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 7589 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294:
#line 2582 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 7597 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295:
#line 2586 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7605 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296:
#line 2590 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 7613 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297:
#line 2594 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 7621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298:
#line 2598 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7630 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299:
#line 2603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7639 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300:
#line 2608 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 7647 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301:
#line 2612 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7655 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302:
#line 2616 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 7663 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303:
#line 2620 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 7671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304:
#line 2624 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), cons(0, (yyvsp[0].nd)));
                    }
#line 7679 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306:
#line 2629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7688 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307:
#line 2634 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 7698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308:
#line 2640 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7707 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309:
#line 2645 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 7719 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310:
#line 2656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7728 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 311:
#line 2664 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7737 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312:
#line 2668 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7743 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313:
#line 2668 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7749 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314:
#line 2671 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315:
#line 2675 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7764 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316:
#line 2675 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7770 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317:
#line 2678 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7779 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318:
#line 2685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7787 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319:
#line 2689 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 7795 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320:
#line 2693 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 7801 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321:
#line 2695 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 7807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322:
#line 2698 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 7816 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323:
#line 2704 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7827 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324:
#line 2712 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 7838 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325:
#line 2720 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 7847 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326:
#line 2725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 7857 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327:
#line 2732 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 7870 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328:
#line 2742 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7881 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329:
#line 2750 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 7892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330:
#line 2760 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331:
#line 2770 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7915 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332:
#line 2778 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 7923 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333:
#line 2782 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 7931 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334:
#line 2786 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 7939 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335:
#line 2790 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 7947 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336:
#line 2796 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 7956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 343:
#line 2815 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 345:
#line 2822 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 346:
#line 2828 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 7980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 348:
#line 2835 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 7988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 349:
#line 2839 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 7996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 350:
#line 2843 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 351:
#line 2847 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3((yyvsp[-2].nd), (node*)-1, 0);
                    }
#line 8013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352:
#line 2852 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (node*)-1, (yyvsp[0].nd));
                    }
#line 8021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 353:
#line 2856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354:
#line 2860 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8037 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355:
#line 2864 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3(0, (node*)-1, 0);
                    }
#line 8046 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356:
#line 2869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                    }
#line 8054 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357:
#line 2873 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (node*)-1, (yyvsp[0].nd));
                    }
#line 8062 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358:
#line 2879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8070 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359:
#line 2883 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 8078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360:
#line 2887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8086 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361:
#line 2891 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 8094 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362:
#line 2897 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8102 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363:
#line 2901 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 8110 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364:
#line 2907 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8118 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365:
#line 2911 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366:
#line 2915 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8134 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367:
#line 2919 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8142 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368:
#line 2923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8150 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369:
#line 2927 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8158 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370:
#line 2931 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8166 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371:
#line 2935 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8174 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372:
#line 2939 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8182 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373:
#line 2943 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8190 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374:
#line 2947 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8198 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375:
#line 2951 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8206 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376:
#line 2955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377:
#line 2959 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8222 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378:
#line 2963 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8230 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379:
#line 2969 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8239 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380:
#line 2974 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8248 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381:
#line 2980 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p, 0);}
#line 8254 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382:
#line 2981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8262 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383:
#line 2985 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8271 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384:
#line 2990 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8279 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385:
#line 2997 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8287 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386:
#line 3001 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8295 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389:
#line 3011 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 8304 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 391:
#line 3019 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392:
#line 3023 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8320 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393:
#line 3029 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8328 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 394:
#line 3033 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8336 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395:
#line 3039 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8345 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396:
#line 3046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397:
#line 3054 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-1].nd)->car == (node*)NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8369 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398:
#line 3064 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8377 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399:
#line 3068 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8386 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400:
#line 3073 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8395 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401:
#line 3080 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8403 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402:
#line 3084 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403:
#line 3088 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8419 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404:
#line 3092 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405:
#line 3096 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM(call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 8435 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406:
#line 3100 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM(call), (yyvsp[0].nd), tCOLON2);
                    }
#line 8443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407:
#line 3104 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408:
#line 3108 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 8459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409:
#line 3112 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 8467 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410:
#line 3118 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8477 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411:
#line 3125 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412:
#line 3132 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413:
#line 3139 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8509 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414:
#line 3150 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 8517 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415:
#line 3156 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 8530 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417:
#line 3170 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 8539 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419:
#line 3178 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8547 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 422:
#line 3186 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8555 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 424:
#line 3193 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8563 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 431:
#line 3207 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = concat_string(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8571 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 434:
#line 3215 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8579 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435:
#line 3219 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8587 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 437:
#line 3226 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8595 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438:
#line 3232 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8603 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439:
#line 3236 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8612 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440:
#line 3242 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lex_strterm = (yyvsp[-2].nd);
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441:
#line 3247 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8629 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442:
#line 3251 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8637 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443:
#line 3257 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8645 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444:
#line 3261 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8653 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445:
#line 3267 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8661 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446:
#line 3271 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8669 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450:
#line 3284 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 8679 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 451:
#line 3290 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 8687 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 454:
#line 3300 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 8697 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 455:
#line 3306 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8706 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456:
#line 3312 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[-2].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 8716 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457:
#line 3320 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 8724 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458:
#line 3324 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459:
#line 3331 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 8741 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460:
#line 3336 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_dsym(p, new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd))));
                    }
#line 8750 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461:
#line 3343 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 466:
#line 3353 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8766 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467:
#line 3357 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8774 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 468:
#line 3363 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 8782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 469:
#line 3367 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8790 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 472:
#line 3375 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8798 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473:
#line 3379 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474:
#line 3385 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 8814 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475:
#line 3389 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 8822 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476:
#line 3393 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 8830 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 477:
#line 3397 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 8838 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478:
#line 3401 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 8846 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479:
#line 3407 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8854 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480:
#line 3411 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 8862 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481:
#line 3417 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 8870 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482:
#line 3421 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 8878 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483:
#line 3425 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 8886 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484:
#line 3429 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 8894 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485:
#line 3433 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 8902 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486:
#line 3437 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 8914 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487:
#line 3445 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 8925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488:
#line 3452 "mrbgems/mruby-compiler/core/parse.y"
                    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    }
#line 8938 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491:
#line 3467 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8946 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492:
#line 3471 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 8955 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 493:
#line 3476 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8963 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 494:
#line 3487 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 8973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495:
#line 3493 "mrbgems/mruby-compiler/core/parse.y"
                    {
#if 1
                      /* til real keyword args implemented */
                      mrb_sym r = MRB_QSYM(mul);
                      mrb_sym b = MRB_QSYM(and);
                      local_add_f(p, r);
                      (yyval.nd) = new_args(p, 0, 0, r, 0,
                                    new_args_tail(p, 0, 0, b));
#else
                      mrb_sym r = MRB_QSYM(mul);
                      mrb_sym k = MRB_QSYM(pow);
                      mrb_sym b = MRB_QSYM(and);
                      local_add_f(p, r); local_add_f(p, k);
                      (yyval.nd) = new_args(p, 0, 0, r, 0,
                                    new_args_tail(p, 0, new_kw_rest_args(p, nsym(k)), b));
#endif
                    }
#line 8995 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 497:
#line 3514 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9003 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 498:
#line 3520 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 9011 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499:
#line 3526 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 500:
#line 3532 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501:
#line 3539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502:
#line 3544 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9048 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 503:
#line 3551 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9056 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504:
#line 3555 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9064 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505:
#line 3561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506:
#line 3565 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9080 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509:
#line 3575 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, nsym((yyvsp[0].id)));
                    }
#line 9088 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510:
#line 3579 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, 0);
                    }
#line 9096 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511:
#line 3585 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9104 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512:
#line 3589 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9112 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513:
#line 3593 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9120 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514:
#line 3597 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9128 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515:
#line 3603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9136 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516:
#line 3607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9144 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517:
#line 3613 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9152 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518:
#line 3617 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9160 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519:
#line 3621 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9168 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520:
#line 3625 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9176 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521:
#line 3629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9184 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 522:
#line 3633 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9192 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523:
#line 3637 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9200 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524:
#line 3641 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9208 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525:
#line 3645 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9216 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526:
#line 3649 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9224 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527:
#line 3653 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9232 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528:
#line 3657 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9240 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529:
#line 3661 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9248 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530:
#line 3665 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9256 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531:
#line 3669 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, MRB_QSYM(and));
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 9265 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532:
#line 3676 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 9274 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533:
#line 3681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 9283 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534:
#line 3686 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 9292 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535:
#line 3691 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 9301 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536:
#line 3696 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 9310 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537:
#line 3703 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9318 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538:
#line 3707 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9327 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539:
#line 3714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 9335 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540:
#line 3718 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 9343 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541:
#line 3722 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn_param(p, (yyvsp[-1].nd), p->locals->car);
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 9353 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542:
#line 3730 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9361 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543:
#line 3734 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9369 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544:
#line 3740 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 9379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545:
#line 3748 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9389 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546:
#line 3756 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9399 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547:
#line 3764 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9407 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548:
#line 3768 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9415 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549:
#line 3774 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9423 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550:
#line 3778 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9431 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553:
#line 3788 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9440 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554:
#line 3793 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, MRB_QSYM(and));
                      (yyval.id) = -1;
                    }
#line 9449 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557:
#line 3804 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9457 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558:
#line 3810 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9465 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559:
#line 3814 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9473 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560:
#line 3820 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9482 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561:
#line 3824 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 9488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562:
#line 3825 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-1].nd) == 0) {
                        yyerror(p, "can't define singleton method for ().");
                      }
                      else {
                        switch ((enum node_type)intn((yyvsp[-1].nd)->car)) {
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
#line 9515 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564:
#line 3851 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9523 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 565:
#line 3857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 9532 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 566:
#line 3862 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9540 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 569:
#line 3872 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9550 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 570:
#line 3878 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9559 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 571:
#line 3883 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if ((yyvsp[-2].nd)->car == (node*)NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 9573 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572:
#line 3893 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 9582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 585:
#line 3920 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 9590 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 586:
#line 3924 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 9598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 588:
#line 3931 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 9606 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 597:
#line 3952 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 9612 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 600:
#line 3958 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 9621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 603:
#line 3969 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9629 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 9633 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 3973 "mrbgems/mruby-compiler/core/parse.y"

#define pylval  (*((YYSTYPE*)(p->ylval)))

static void
yyerror(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
#ifndef MRB_DISABLE_STDIO
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
#ifndef MRB_DISABLE_STDIO
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
#ifndef MRB_DISABLE_STDIO
  if (p->f) {
    if (feof(p->f)) return -1;
    c = fgetc(p->f);
    if (c == EOF) return -1;
  }
  else
#endif
    if (!p->s || p->s >= p->send) {
      return -1;
    }
    else {
      c = (unsigned char)*p->s++;
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
    p->pb = append((node*)list, p->pb);
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

#ifndef MRB_DISABLE_STDIO
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

static int
parse_string(parser_state *p)
{
  int c;
  string_type type = (string_type)(intptr_t)p->lex_strterm->car;
  int nest_level = intn(p->lex_strterm->cdr->car);
  int beg = intn(p->lex_strterm->cdr->cdr->car);
  int end = intn(p->lex_strterm->cdr->cdr->cdr);
  parser_heredoc_info *hinf = (type & STR_FUNC_HEREDOC) ? parsing_heredoc_inf(p) : NULL;

  if (beg == 0) beg = -3;       /* should never happen */
  if (end == 0) end = -3;
  newtok(p);
  while ((c = nextc(p)) != end || nest_level != 0) {
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
      pylval.nd = new_str(p, tok(p), toklen(p));
      return tHD_STRING_MID;
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
        pylval.nd = new_str(p, tok(p), toklen(p));
        if (hinf) {
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
    list = push(list, (node*)(intptr_t)c);

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
        p->pb = append((node*)list, p->pb);
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
  mrb_bool quote = FALSE;
  node *newnode;
  parser_heredoc_info *info;

  c = nextc(p);
  if (ISSPACE(c) || c == '=') {
    pushback(p, c);
    return 0;
  }
  if (c == '-') {
    indent = TRUE;
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
  info->allow_indent = indent;
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
        pylval.id = MRB_QSYM(pow);
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
        pylval.id = MRB_QSYM(mul);
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
        pylval.id = MRB_QSYM(lshift);
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
        pylval.id = MRB_QSYM(rshift);
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
        pylval.id = MRB_QSYM(andand);
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
      pylval.id = MRB_QSYM(and);
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
        pylval.id = MRB_QSYM(oror);
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tOROP;
    }
    if (c == '=') {
      pylval.id = MRB_QSYM(or);
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
      pylval.id = MRB_QSYM(add);
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
      pylval.id = MRB_QSYM(sub);
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
    p->lstate = EXPR_BEG;
    if ((c = nextc(p)) == '.') {
      if ((c = nextc(p)) == '.') {
        return tDOT3;
      }
      pushback(p, c);
      return tDOT2;
    }
    pushback(p, c);
    if (c >= 0 && ISDIGIT(c)) {
      yyerror(p, "no .<digit> floating literal anymore; put 0 before dot");
    }
    p->lstate = EXPR_DOT;
    return '.';

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
#ifdef MRB_WITHOUT_FLOAT
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
    if (!ISSPACE(c) || IS_BEG()) {
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
      pylval.id = MRB_QSYM(div);
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
      pylval.id = MRB_QSYM(xor);
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
      pylval.id = MRB_QSYM(mod);
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
#ifndef MRB_DISABLE_STDIO
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

#ifndef MRB_DISABLE_STDIO
MRB_API parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return NULL;
  p->s = p->send = NULL;
  p->f = f;

  mrb_parser_parse(p, c);
  return p;
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
  unsigned int keep = 0;

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
        mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SYNTAX_ERROR, "syntax error"));
      }
      mrb_parser_free(p);
      return mrb_undef_value();
    }
  }
  proc = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  if (proc == NULL) {
    if (mrb->exc == NULL) {
      mrb->exc = mrb_obj_ptr(mrb_exc_new_str_lit(mrb, E_SCRIPT_ERROR, "codegen error"));
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
    mrb->c->ci->target_class = target;
  }
  v = mrb_top_run(mrb, proc, mrb_top_self(mrb), keep);
  if (mrb->exc) return mrb_nil_value();
  return v;
}

#ifndef MRB_DISABLE_STDIO
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

#ifndef MRB_DISABLE_STDIO

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
  /* check maximum length with "\xNN" charactor */
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
#ifndef MRB_DISABLE_STDIO
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
          if (n2->car == (node*)-1) {
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
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym_name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
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
    if (tree->car->car == (node*)0) {
      dump_prefix(tree, offset+1);
      printf(":%s\n", mrb_sym_name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
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
