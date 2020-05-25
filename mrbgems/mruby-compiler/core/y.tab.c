/* A Bison parser, made by GNU Bison 3.6.2.  */

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

/* DO NOT RELY ON FEATURES THAT ARE NOT DOCUMENTED in the manual,
   especially those whose name start with YY_ or yy_.  They are
   private implementation details that can be changed or removed.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.6.2"

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
  node *l = p->locals;

  while (l) {
    node *n = l->car;
    while (n) {
      if (sym(n->car) == sym) return TRUE;
      n = n->cdr;
    }
    l = l->cdr;
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
  local_add_f(p, blk ? blk : mrb_intern_lit(p->mrb, "&"));
}

static void
local_add_kw(parser_state *p, mrb_sym kwd)
{
  /* allocate register for keywords hash */
  local_add_f(p, kwd ? kwd : mrb_intern_lit(p->mrb, "**"));
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
  return list5((node*)NODE_DEF, nsym(m), locals_node(p), a, b);
}

/* (:sdef obj m lv (arg . body)) */
static node*
new_sdef(parser_state *p, node *o, mrb_sym m, node *a, node *b)
{
  void_expr_error(p, o);
  return list6((node*)NODE_SDEF, o, nsym(m), locals_node(p), a, b);
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
  return new_call(p, new_const(p, intern_lit("Kernel")), intern_lit("Complex"), list1(list2(list3((node*)NODE_INT, (node*)strdup("0"), nint(10)), imaginary)), 1);
}

static node*
new_rational(parser_state *p, node *rational)
{
  return new_call(p, new_const(p, intern_lit("Kernel")), intern_lit("Rational"), list1(list1(rational)), 1);
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


#line 1396 "mrbgems/mruby-compiler/core/y.tab.c"

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
    keyword_class = 258,           /* keyword_class  */
    keyword_module = 259,          /* keyword_module  */
    keyword_def = 260,             /* keyword_def  */
    keyword_begin = 261,           /* keyword_begin  */
    keyword_if = 262,              /* keyword_if  */
    keyword_unless = 263,          /* keyword_unless  */
    keyword_while = 264,           /* keyword_while  */
    keyword_until = 265,           /* keyword_until  */
    keyword_for = 266,             /* keyword_for  */
    keyword_undef = 267,           /* keyword_undef  */
    keyword_rescue = 268,          /* keyword_rescue  */
    keyword_ensure = 269,          /* keyword_ensure  */
    keyword_end = 270,             /* keyword_end  */
    keyword_then = 271,            /* keyword_then  */
    keyword_elsif = 272,           /* keyword_elsif  */
    keyword_else = 273,            /* keyword_else  */
    keyword_case = 274,            /* keyword_case  */
    keyword_when = 275,            /* keyword_when  */
    keyword_break = 276,           /* keyword_break  */
    keyword_next = 277,            /* keyword_next  */
    keyword_redo = 278,            /* keyword_redo  */
    keyword_retry = 279,           /* keyword_retry  */
    keyword_in = 280,              /* keyword_in  */
    keyword_do = 281,              /* keyword_do  */
    keyword_do_cond = 282,         /* keyword_do_cond  */
    keyword_do_block = 283,        /* keyword_do_block  */
    keyword_do_LAMBDA = 284,       /* keyword_do_LAMBDA  */
    keyword_return = 285,          /* keyword_return  */
    keyword_yield = 286,           /* keyword_yield  */
    keyword_super = 287,           /* keyword_super  */
    keyword_self = 288,            /* keyword_self  */
    keyword_nil = 289,             /* keyword_nil  */
    keyword_true = 290,            /* keyword_true  */
    keyword_false = 291,           /* keyword_false  */
    keyword_and = 292,             /* keyword_and  */
    keyword_or = 293,              /* keyword_or  */
    keyword_not = 294,             /* keyword_not  */
    modifier_if = 295,             /* modifier_if  */
    modifier_unless = 296,         /* modifier_unless  */
    modifier_while = 297,          /* modifier_while  */
    modifier_until = 298,          /* modifier_until  */
    modifier_rescue = 299,         /* modifier_rescue  */
    keyword_alias = 300,           /* keyword_alias  */
    keyword_BEGIN = 301,           /* keyword_BEGIN  */
    keyword_END = 302,             /* keyword_END  */
    keyword__LINE__ = 303,         /* keyword__LINE__  */
    keyword__FILE__ = 304,         /* keyword__FILE__  */
    keyword__ENCODING__ = 305,     /* keyword__ENCODING__  */
    tIDENTIFIER = 306,             /* tIDENTIFIER  */
    tFID = 307,                    /* tFID  */
    tGVAR = 308,                   /* tGVAR  */
    tIVAR = 309,                   /* tIVAR  */
    tCONSTANT = 310,               /* tCONSTANT  */
    tCVAR = 311,                   /* tCVAR  */
    tLABEL_TAG = 312,              /* tLABEL_TAG  */
    tINTEGER = 313,                /* tINTEGER  */
    tFLOAT = 314,                  /* tFLOAT  */
    tCHAR = 315,                   /* tCHAR  */
    tXSTRING = 316,                /* tXSTRING  */
    tREGEXP = 317,                 /* tREGEXP  */
    tSTRING = 318,                 /* tSTRING  */
    tSTRING_PART = 319,            /* tSTRING_PART  */
    tSTRING_MID = 320,             /* tSTRING_MID  */
    tNTH_REF = 321,                /* tNTH_REF  */
    tBACK_REF = 322,               /* tBACK_REF  */
    tREGEXP_END = 323,             /* tREGEXP_END  */
    tNUMPARAM = 324,               /* tNUMPARAM  */
    tUPLUS = 325,                  /* tUPLUS  */
    tUMINUS = 326,                 /* tUMINUS  */
    tPOW = 327,                    /* tPOW  */
    tCMP = 328,                    /* tCMP  */
    tEQ = 329,                     /* tEQ  */
    tEQQ = 330,                    /* tEQQ  */
    tNEQ = 331,                    /* tNEQ  */
    tGEQ = 332,                    /* tGEQ  */
    tLEQ = 333,                    /* tLEQ  */
    tANDOP = 334,                  /* tANDOP  */
    tOROP = 335,                   /* tOROP  */
    tMATCH = 336,                  /* tMATCH  */
    tNMATCH = 337,                 /* tNMATCH  */
    tDOT2 = 338,                   /* tDOT2  */
    tDOT3 = 339,                   /* tDOT3  */
    tAREF = 340,                   /* tAREF  */
    tASET = 341,                   /* tASET  */
    tLSHFT = 342,                  /* tLSHFT  */
    tRSHFT = 343,                  /* tRSHFT  */
    tCOLON2 = 344,                 /* tCOLON2  */
    tCOLON3 = 345,                 /* tCOLON3  */
    tOP_ASGN = 346,                /* tOP_ASGN  */
    tASSOC = 347,                  /* tASSOC  */
    tLPAREN = 348,                 /* tLPAREN  */
    tLPAREN_ARG = 349,             /* tLPAREN_ARG  */
    tRPAREN = 350,                 /* tRPAREN  */
    tLBRACK = 351,                 /* tLBRACK  */
    tLBRACE = 352,                 /* tLBRACE  */
    tLBRACE_ARG = 353,             /* tLBRACE_ARG  */
    tSTAR = 354,                   /* tSTAR  */
    tDSTAR = 355,                  /* tDSTAR  */
    tAMPER = 356,                  /* tAMPER  */
    tLAMBDA = 357,                 /* tLAMBDA  */
    tANDDOT = 358,                 /* tANDDOT  */
    tSYMBEG = 359,                 /* tSYMBEG  */
    tREGEXP_BEG = 360,             /* tREGEXP_BEG  */
    tWORDS_BEG = 361,              /* tWORDS_BEG  */
    tSYMBOLS_BEG = 362,            /* tSYMBOLS_BEG  */
    tSTRING_BEG = 363,             /* tSTRING_BEG  */
    tXSTRING_BEG = 364,            /* tXSTRING_BEG  */
    tSTRING_DVAR = 365,            /* tSTRING_DVAR  */
    tLAMBEG = 366,                 /* tLAMBEG  */
    tHEREDOC_BEG = 367,            /* tHEREDOC_BEG  */
    tHEREDOC_END = 368,            /* tHEREDOC_END  */
    tLITERAL_DELIM = 369,          /* tLITERAL_DELIM  */
    tHD_LITERAL_DELIM = 370,       /* tHD_LITERAL_DELIM  */
    tHD_STRING_PART = 371,         /* tHD_STRING_PART  */
    tHD_STRING_MID = 372,          /* tHD_STRING_MID  */
    tLOWEST = 373,                 /* tLOWEST  */
    tUMINUS_NUM = 374,             /* tUMINUS_NUM  */
    tLAST_TOKEN = 375              /* tLAST_TOKEN  */
  };
  typedef enum yytokentype yytoken_kind_t;
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
union YYSTYPE
{
#line 1337 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1571 "mrbgems/mruby-compiler/core/y.tab.c"

};
typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif



int yyparse (parser_state *p);


/* Symbol kind.  */
enum yysymbol_kind_t
{
  YYSYMBOL_YYEMPTY = -2,
  YYSYMBOL_YYEOF = 0,                      /* "end of file"  */
  YYSYMBOL_YYerror = 1,                    /* error  */
  YYSYMBOL_YYUNDEF = 2,                    /* "invalid token"  */
  YYSYMBOL_keyword_class = 3,              /* keyword_class  */
  YYSYMBOL_keyword_module = 4,             /* keyword_module  */
  YYSYMBOL_keyword_def = 5,                /* keyword_def  */
  YYSYMBOL_keyword_begin = 6,              /* keyword_begin  */
  YYSYMBOL_keyword_if = 7,                 /* keyword_if  */
  YYSYMBOL_keyword_unless = 8,             /* keyword_unless  */
  YYSYMBOL_keyword_while = 9,              /* keyword_while  */
  YYSYMBOL_keyword_until = 10,             /* keyword_until  */
  YYSYMBOL_keyword_for = 11,               /* keyword_for  */
  YYSYMBOL_keyword_undef = 12,             /* keyword_undef  */
  YYSYMBOL_keyword_rescue = 13,            /* keyword_rescue  */
  YYSYMBOL_keyword_ensure = 14,            /* keyword_ensure  */
  YYSYMBOL_keyword_end = 15,               /* keyword_end  */
  YYSYMBOL_keyword_then = 16,              /* keyword_then  */
  YYSYMBOL_keyword_elsif = 17,             /* keyword_elsif  */
  YYSYMBOL_keyword_else = 18,              /* keyword_else  */
  YYSYMBOL_keyword_case = 19,              /* keyword_case  */
  YYSYMBOL_keyword_when = 20,              /* keyword_when  */
  YYSYMBOL_keyword_break = 21,             /* keyword_break  */
  YYSYMBOL_keyword_next = 22,              /* keyword_next  */
  YYSYMBOL_keyword_redo = 23,              /* keyword_redo  */
  YYSYMBOL_keyword_retry = 24,             /* keyword_retry  */
  YYSYMBOL_keyword_in = 25,                /* keyword_in  */
  YYSYMBOL_keyword_do = 26,                /* keyword_do  */
  YYSYMBOL_keyword_do_cond = 27,           /* keyword_do_cond  */
  YYSYMBOL_keyword_do_block = 28,          /* keyword_do_block  */
  YYSYMBOL_keyword_do_LAMBDA = 29,         /* keyword_do_LAMBDA  */
  YYSYMBOL_keyword_return = 30,            /* keyword_return  */
  YYSYMBOL_keyword_yield = 31,             /* keyword_yield  */
  YYSYMBOL_keyword_super = 32,             /* keyword_super  */
  YYSYMBOL_keyword_self = 33,              /* keyword_self  */
  YYSYMBOL_keyword_nil = 34,               /* keyword_nil  */
  YYSYMBOL_keyword_true = 35,              /* keyword_true  */
  YYSYMBOL_keyword_false = 36,             /* keyword_false  */
  YYSYMBOL_keyword_and = 37,               /* keyword_and  */
  YYSYMBOL_keyword_or = 38,                /* keyword_or  */
  YYSYMBOL_keyword_not = 39,               /* keyword_not  */
  YYSYMBOL_modifier_if = 40,               /* modifier_if  */
  YYSYMBOL_modifier_unless = 41,           /* modifier_unless  */
  YYSYMBOL_modifier_while = 42,            /* modifier_while  */
  YYSYMBOL_modifier_until = 43,            /* modifier_until  */
  YYSYMBOL_modifier_rescue = 44,           /* modifier_rescue  */
  YYSYMBOL_keyword_alias = 45,             /* keyword_alias  */
  YYSYMBOL_keyword_BEGIN = 46,             /* keyword_BEGIN  */
  YYSYMBOL_keyword_END = 47,               /* keyword_END  */
  YYSYMBOL_keyword__LINE__ = 48,           /* keyword__LINE__  */
  YYSYMBOL_keyword__FILE__ = 49,           /* keyword__FILE__  */
  YYSYMBOL_keyword__ENCODING__ = 50,       /* keyword__ENCODING__  */
  YYSYMBOL_tIDENTIFIER = 51,               /* tIDENTIFIER  */
  YYSYMBOL_tFID = 52,                      /* tFID  */
  YYSYMBOL_tGVAR = 53,                     /* tGVAR  */
  YYSYMBOL_tIVAR = 54,                     /* tIVAR  */
  YYSYMBOL_tCONSTANT = 55,                 /* tCONSTANT  */
  YYSYMBOL_tCVAR = 56,                     /* tCVAR  */
  YYSYMBOL_tLABEL_TAG = 57,                /* tLABEL_TAG  */
  YYSYMBOL_tINTEGER = 58,                  /* tINTEGER  */
  YYSYMBOL_tFLOAT = 59,                    /* tFLOAT  */
  YYSYMBOL_tCHAR = 60,                     /* tCHAR  */
  YYSYMBOL_tXSTRING = 61,                  /* tXSTRING  */
  YYSYMBOL_tREGEXP = 62,                   /* tREGEXP  */
  YYSYMBOL_tSTRING = 63,                   /* tSTRING  */
  YYSYMBOL_tSTRING_PART = 64,              /* tSTRING_PART  */
  YYSYMBOL_tSTRING_MID = 65,               /* tSTRING_MID  */
  YYSYMBOL_tNTH_REF = 66,                  /* tNTH_REF  */
  YYSYMBOL_tBACK_REF = 67,                 /* tBACK_REF  */
  YYSYMBOL_tREGEXP_END = 68,               /* tREGEXP_END  */
  YYSYMBOL_tNUMPARAM = 69,                 /* tNUMPARAM  */
  YYSYMBOL_tUPLUS = 70,                    /* tUPLUS  */
  YYSYMBOL_tUMINUS = 71,                   /* tUMINUS  */
  YYSYMBOL_tPOW = 72,                      /* tPOW  */
  YYSYMBOL_tCMP = 73,                      /* tCMP  */
  YYSYMBOL_tEQ = 74,                       /* tEQ  */
  YYSYMBOL_tEQQ = 75,                      /* tEQQ  */
  YYSYMBOL_tNEQ = 76,                      /* tNEQ  */
  YYSYMBOL_tGEQ = 77,                      /* tGEQ  */
  YYSYMBOL_tLEQ = 78,                      /* tLEQ  */
  YYSYMBOL_tANDOP = 79,                    /* tANDOP  */
  YYSYMBOL_tOROP = 80,                     /* tOROP  */
  YYSYMBOL_tMATCH = 81,                    /* tMATCH  */
  YYSYMBOL_tNMATCH = 82,                   /* tNMATCH  */
  YYSYMBOL_tDOT2 = 83,                     /* tDOT2  */
  YYSYMBOL_tDOT3 = 84,                     /* tDOT3  */
  YYSYMBOL_tAREF = 85,                     /* tAREF  */
  YYSYMBOL_tASET = 86,                     /* tASET  */
  YYSYMBOL_tLSHFT = 87,                    /* tLSHFT  */
  YYSYMBOL_tRSHFT = 88,                    /* tRSHFT  */
  YYSYMBOL_tCOLON2 = 89,                   /* tCOLON2  */
  YYSYMBOL_tCOLON3 = 90,                   /* tCOLON3  */
  YYSYMBOL_tOP_ASGN = 91,                  /* tOP_ASGN  */
  YYSYMBOL_tASSOC = 92,                    /* tASSOC  */
  YYSYMBOL_tLPAREN = 93,                   /* tLPAREN  */
  YYSYMBOL_tLPAREN_ARG = 94,               /* tLPAREN_ARG  */
  YYSYMBOL_tRPAREN = 95,                   /* tRPAREN  */
  YYSYMBOL_tLBRACK = 96,                   /* tLBRACK  */
  YYSYMBOL_tLBRACE = 97,                   /* tLBRACE  */
  YYSYMBOL_tLBRACE_ARG = 98,               /* tLBRACE_ARG  */
  YYSYMBOL_tSTAR = 99,                     /* tSTAR  */
  YYSYMBOL_tDSTAR = 100,                   /* tDSTAR  */
  YYSYMBOL_tAMPER = 101,                   /* tAMPER  */
  YYSYMBOL_tLAMBDA = 102,                  /* tLAMBDA  */
  YYSYMBOL_tANDDOT = 103,                  /* tANDDOT  */
  YYSYMBOL_tSYMBEG = 104,                  /* tSYMBEG  */
  YYSYMBOL_tREGEXP_BEG = 105,              /* tREGEXP_BEG  */
  YYSYMBOL_tWORDS_BEG = 106,               /* tWORDS_BEG  */
  YYSYMBOL_tSYMBOLS_BEG = 107,             /* tSYMBOLS_BEG  */
  YYSYMBOL_tSTRING_BEG = 108,              /* tSTRING_BEG  */
  YYSYMBOL_tXSTRING_BEG = 109,             /* tXSTRING_BEG  */
  YYSYMBOL_tSTRING_DVAR = 110,             /* tSTRING_DVAR  */
  YYSYMBOL_tLAMBEG = 111,                  /* tLAMBEG  */
  YYSYMBOL_tHEREDOC_BEG = 112,             /* tHEREDOC_BEG  */
  YYSYMBOL_tHEREDOC_END = 113,             /* tHEREDOC_END  */
  YYSYMBOL_tLITERAL_DELIM = 114,           /* tLITERAL_DELIM  */
  YYSYMBOL_tHD_LITERAL_DELIM = 115,        /* tHD_LITERAL_DELIM  */
  YYSYMBOL_tHD_STRING_PART = 116,          /* tHD_STRING_PART  */
  YYSYMBOL_tHD_STRING_MID = 117,           /* tHD_STRING_MID  */
  YYSYMBOL_tLOWEST = 118,                  /* tLOWEST  */
  YYSYMBOL_119_ = 119,                     /* '='  */
  YYSYMBOL_120_ = 120,                     /* '?'  */
  YYSYMBOL_121_ = 121,                     /* ':'  */
  YYSYMBOL_122_ = 122,                     /* '>'  */
  YYSYMBOL_123_ = 123,                     /* '<'  */
  YYSYMBOL_124_ = 124,                     /* '|'  */
  YYSYMBOL_125_ = 125,                     /* '^'  */
  YYSYMBOL_126_ = 126,                     /* '&'  */
  YYSYMBOL_127_ = 127,                     /* '+'  */
  YYSYMBOL_128_ = 128,                     /* '-'  */
  YYSYMBOL_129_ = 129,                     /* '*'  */
  YYSYMBOL_130_ = 130,                     /* '/'  */
  YYSYMBOL_131_ = 131,                     /* '%'  */
  YYSYMBOL_tUMINUS_NUM = 132,              /* tUMINUS_NUM  */
  YYSYMBOL_133_ = 133,                     /* '!'  */
  YYSYMBOL_134_ = 134,                     /* '~'  */
  YYSYMBOL_tLAST_TOKEN = 135,              /* tLAST_TOKEN  */
  YYSYMBOL_136_ = 136,                     /* '{'  */
  YYSYMBOL_137_ = 137,                     /* '}'  */
  YYSYMBOL_138_ = 138,                     /* '['  */
  YYSYMBOL_139_ = 139,                     /* ']'  */
  YYSYMBOL_140_ = 140,                     /* ','  */
  YYSYMBOL_141_ = 141,                     /* '`'  */
  YYSYMBOL_142_ = 142,                     /* '('  */
  YYSYMBOL_143_ = 143,                     /* ')'  */
  YYSYMBOL_144_ = 144,                     /* ';'  */
  YYSYMBOL_145_ = 145,                     /* '.'  */
  YYSYMBOL_146_n_ = 146,                   /* '\n'  */
  YYSYMBOL_YYACCEPT = 147,                 /* $accept  */
  YYSYMBOL_program = 148,                  /* program  */
  YYSYMBOL_149_1 = 149,                    /* $@1  */
  YYSYMBOL_top_compstmt = 150,             /* top_compstmt  */
  YYSYMBOL_top_stmts = 151,                /* top_stmts  */
  YYSYMBOL_top_stmt = 152,                 /* top_stmt  */
  YYSYMBOL_153_2 = 153,                    /* @2  */
  YYSYMBOL_bodystmt = 154,                 /* bodystmt  */
  YYSYMBOL_compstmt = 155,                 /* compstmt  */
  YYSYMBOL_stmts = 156,                    /* stmts  */
  YYSYMBOL_stmt = 157,                     /* stmt  */
  YYSYMBOL_158_3 = 158,                    /* $@3  */
  YYSYMBOL_command_asgn = 159,             /* command_asgn  */
  YYSYMBOL_command_rhs = 160,              /* command_rhs  */
  YYSYMBOL_expr = 161,                     /* expr  */
  YYSYMBOL_expr_value = 162,               /* expr_value  */
  YYSYMBOL_command_call = 163,             /* command_call  */
  YYSYMBOL_block_command = 164,            /* block_command  */
  YYSYMBOL_cmd_brace_block = 165,          /* cmd_brace_block  */
  YYSYMBOL_166_4 = 166,                    /* $@4  */
  YYSYMBOL_command = 167,                  /* command  */
  YYSYMBOL_mlhs = 168,                     /* mlhs  */
  YYSYMBOL_mlhs_inner = 169,               /* mlhs_inner  */
  YYSYMBOL_mlhs_basic = 170,               /* mlhs_basic  */
  YYSYMBOL_mlhs_item = 171,                /* mlhs_item  */
  YYSYMBOL_mlhs_list = 172,                /* mlhs_list  */
  YYSYMBOL_mlhs_post = 173,                /* mlhs_post  */
  YYSYMBOL_mlhs_node = 174,                /* mlhs_node  */
  YYSYMBOL_lhs = 175,                      /* lhs  */
  YYSYMBOL_cname = 176,                    /* cname  */
  YYSYMBOL_cpath = 177,                    /* cpath  */
  YYSYMBOL_fname = 178,                    /* fname  */
  YYSYMBOL_fsym = 179,                     /* fsym  */
  YYSYMBOL_undef_list = 180,               /* undef_list  */
  YYSYMBOL_181_5 = 181,                    /* $@5  */
  YYSYMBOL_op = 182,                       /* op  */
  YYSYMBOL_reswords = 183,                 /* reswords  */
  YYSYMBOL_arg = 184,                      /* arg  */
  YYSYMBOL_aref_args = 185,                /* aref_args  */
  YYSYMBOL_arg_rhs = 186,                  /* arg_rhs  */
  YYSYMBOL_paren_args = 187,               /* paren_args  */
  YYSYMBOL_opt_paren_args = 188,           /* opt_paren_args  */
  YYSYMBOL_opt_call_args = 189,            /* opt_call_args  */
  YYSYMBOL_call_args = 190,                /* call_args  */
  YYSYMBOL_command_args = 191,             /* command_args  */
  YYSYMBOL_192_6 = 192,                    /* @6  */
  YYSYMBOL_block_arg = 193,                /* block_arg  */
  YYSYMBOL_opt_block_arg = 194,            /* opt_block_arg  */
  YYSYMBOL_comma = 195,                    /* comma  */
  YYSYMBOL_args = 196,                     /* args  */
  YYSYMBOL_mrhs = 197,                     /* mrhs  */
  YYSYMBOL_primary = 198,                  /* primary  */
  YYSYMBOL_199_7 = 199,                    /* @7  */
  YYSYMBOL_200_8 = 200,                    /* @8  */
  YYSYMBOL_201_9 = 201,                    /* $@9  */
  YYSYMBOL_202_10 = 202,                   /* $@10  */
  YYSYMBOL_203_11 = 203,                   /* @11  */
  YYSYMBOL_204_12 = 204,                   /* @12  */
  YYSYMBOL_205_13 = 205,                   /* $@13  */
  YYSYMBOL_206_14 = 206,                   /* $@14  */
  YYSYMBOL_207_15 = 207,                   /* $@15  */
  YYSYMBOL_208_16 = 208,                   /* $@16  */
  YYSYMBOL_209_17 = 209,                   /* $@17  */
  YYSYMBOL_210_18 = 210,                   /* $@18  */
  YYSYMBOL_211_19 = 211,                   /* @19  */
  YYSYMBOL_212_20 = 212,                   /* @20  */
  YYSYMBOL_213_21 = 213,                   /* @21  */
  YYSYMBOL_214_22 = 214,                   /* @22  */
  YYSYMBOL_215_23 = 215,                   /* @23  */
  YYSYMBOL_216_24 = 216,                   /* @24  */
  YYSYMBOL_217_25 = 217,                   /* @25  */
  YYSYMBOL_218_26 = 218,                   /* @26  */
  YYSYMBOL_primary_value = 219,            /* primary_value  */
  YYSYMBOL_then = 220,                     /* then  */
  YYSYMBOL_do = 221,                       /* do  */
  YYSYMBOL_if_tail = 222,                  /* if_tail  */
  YYSYMBOL_opt_else = 223,                 /* opt_else  */
  YYSYMBOL_for_var = 224,                  /* for_var  */
  YYSYMBOL_f_margs = 225,                  /* f_margs  */
  YYSYMBOL_226_27 = 226,                   /* $@27  */
  YYSYMBOL_block_args_tail = 227,          /* block_args_tail  */
  YYSYMBOL_opt_block_args_tail = 228,      /* opt_block_args_tail  */
  YYSYMBOL_block_param = 229,              /* block_param  */
  YYSYMBOL_opt_block_param = 230,          /* opt_block_param  */
  YYSYMBOL_block_param_def = 231,          /* block_param_def  */
  YYSYMBOL_232_28 = 232,                   /* $@28  */
  YYSYMBOL_opt_bv_decl = 233,              /* opt_bv_decl  */
  YYSYMBOL_bv_decls = 234,                 /* bv_decls  */
  YYSYMBOL_bvar = 235,                     /* bvar  */
  YYSYMBOL_f_larglist = 236,               /* f_larglist  */
  YYSYMBOL_lambda_body = 237,              /* lambda_body  */
  YYSYMBOL_do_block = 238,                 /* do_block  */
  YYSYMBOL_239_29 = 239,                   /* $@29  */
  YYSYMBOL_block_call = 240,               /* block_call  */
  YYSYMBOL_method_call = 241,              /* method_call  */
  YYSYMBOL_brace_block = 242,              /* brace_block  */
  YYSYMBOL_243_30 = 243,                   /* @30  */
  YYSYMBOL_244_31 = 244,                   /* @31  */
  YYSYMBOL_case_body = 245,                /* case_body  */
  YYSYMBOL_cases = 246,                    /* cases  */
  YYSYMBOL_opt_rescue = 247,               /* opt_rescue  */
  YYSYMBOL_exc_list = 248,                 /* exc_list  */
  YYSYMBOL_exc_var = 249,                  /* exc_var  */
  YYSYMBOL_opt_ensure = 250,               /* opt_ensure  */
  YYSYMBOL_literal = 251,                  /* literal  */
  YYSYMBOL_string = 252,                   /* string  */
  YYSYMBOL_string_fragment = 253,          /* string_fragment  */
  YYSYMBOL_string_rep = 254,               /* string_rep  */
  YYSYMBOL_string_interp = 255,            /* string_interp  */
  YYSYMBOL_256_32 = 256,                   /* @32  */
  YYSYMBOL_xstring = 257,                  /* xstring  */
  YYSYMBOL_regexp = 258,                   /* regexp  */
  YYSYMBOL_heredoc = 259,                  /* heredoc  */
  YYSYMBOL_heredoc_bodies = 260,           /* heredoc_bodies  */
  YYSYMBOL_heredoc_body = 261,             /* heredoc_body  */
  YYSYMBOL_heredoc_string_rep = 262,       /* heredoc_string_rep  */
  YYSYMBOL_heredoc_string_interp = 263,    /* heredoc_string_interp  */
  YYSYMBOL_264_33 = 264,                   /* @33  */
  YYSYMBOL_words = 265,                    /* words  */
  YYSYMBOL_symbol = 266,                   /* symbol  */
  YYSYMBOL_basic_symbol = 267,             /* basic_symbol  */
  YYSYMBOL_sym = 268,                      /* sym  */
  YYSYMBOL_symbols = 269,                  /* symbols  */
  YYSYMBOL_numeric = 270,                  /* numeric  */
  YYSYMBOL_variable = 271,                 /* variable  */
  YYSYMBOL_var_lhs = 272,                  /* var_lhs  */
  YYSYMBOL_var_ref = 273,                  /* var_ref  */
  YYSYMBOL_backref = 274,                  /* backref  */
  YYSYMBOL_superclass = 275,               /* superclass  */
  YYSYMBOL_276_34 = 276,                   /* $@34  */
  YYSYMBOL_f_arglist = 277,                /* f_arglist  */
  YYSYMBOL_f_label = 278,                  /* f_label  */
  YYSYMBOL_f_kw = 279,                     /* f_kw  */
  YYSYMBOL_f_block_kw = 280,               /* f_block_kw  */
  YYSYMBOL_f_block_kwarg = 281,            /* f_block_kwarg  */
  YYSYMBOL_f_kwarg = 282,                  /* f_kwarg  */
  YYSYMBOL_kwrest_mark = 283,              /* kwrest_mark  */
  YYSYMBOL_f_kwrest = 284,                 /* f_kwrest  */
  YYSYMBOL_args_tail = 285,                /* args_tail  */
  YYSYMBOL_opt_args_tail = 286,            /* opt_args_tail  */
  YYSYMBOL_f_args = 287,                   /* f_args  */
  YYSYMBOL_f_bad_arg = 288,                /* f_bad_arg  */
  YYSYMBOL_f_norm_arg = 289,               /* f_norm_arg  */
  YYSYMBOL_f_arg_item = 290,               /* f_arg_item  */
  YYSYMBOL_291_35 = 291,                   /* @35  */
  YYSYMBOL_f_arg = 292,                    /* f_arg  */
  YYSYMBOL_f_opt_asgn = 293,               /* f_opt_asgn  */
  YYSYMBOL_f_opt = 294,                    /* f_opt  */
  YYSYMBOL_f_block_opt = 295,              /* f_block_opt  */
  YYSYMBOL_f_block_optarg = 296,           /* f_block_optarg  */
  YYSYMBOL_f_optarg = 297,                 /* f_optarg  */
  YYSYMBOL_restarg_mark = 298,             /* restarg_mark  */
  YYSYMBOL_f_rest_arg = 299,               /* f_rest_arg  */
  YYSYMBOL_blkarg_mark = 300,              /* blkarg_mark  */
  YYSYMBOL_f_block_arg = 301,              /* f_block_arg  */
  YYSYMBOL_opt_f_block_arg = 302,          /* opt_f_block_arg  */
  YYSYMBOL_singleton = 303,                /* singleton  */
  YYSYMBOL_304_36 = 304,                   /* $@36  */
  YYSYMBOL_assoc_list = 305,               /* assoc_list  */
  YYSYMBOL_assocs = 306,                   /* assocs  */
  YYSYMBOL_label_tag = 307,                /* label_tag  */
  YYSYMBOL_assoc = 308,                    /* assoc  */
  YYSYMBOL_operation = 309,                /* operation  */
  YYSYMBOL_operation2 = 310,               /* operation2  */
  YYSYMBOL_operation3 = 311,               /* operation3  */
  YYSYMBOL_dot_or_colon = 312,             /* dot_or_colon  */
  YYSYMBOL_call_op = 313,                  /* call_op  */
  YYSYMBOL_call_op2 = 314,                 /* call_op2  */
  YYSYMBOL_opt_terms = 315,                /* opt_terms  */
  YYSYMBOL_opt_nl = 316,                   /* opt_nl  */
  YYSYMBOL_rparen = 317,                   /* rparen  */
  YYSYMBOL_trailer = 318,                  /* trailer  */
  YYSYMBOL_term = 319,                     /* term  */
  YYSYMBOL_nl = 320,                       /* nl  */
  YYSYMBOL_terms = 321,                    /* terms  */
  YYSYMBOL_none = 322                      /* none  */
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
#define YYLAST   11586

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  147
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  176
/* YYNRULES -- Number of rules.  */
#define YYNRULES  594
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1034

#define YYMAXUTOK   375


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
  /* YYRLINEYYN -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,  1495,  1495,  1495,  1506,  1512,  1516,  1521,  1525,  1531,
    1533,  1532,  1546,  1573,  1579,  1583,  1588,  1592,  1598,  1598,
    1602,  1606,  1610,  1614,  1618,  1622,  1626,  1631,  1632,  1636,
    1640,  1644,  1648,  1651,  1655,  1659,  1663,  1667,  1671,  1676,
    1680,  1687,  1688,  1692,  1696,  1697,  1701,  1705,  1709,  1713,
    1716,  1725,  1726,  1729,  1730,  1737,  1736,  1751,  1755,  1760,
    1764,  1769,  1773,  1778,  1782,  1786,  1790,  1794,  1800,  1804,
    1810,  1811,  1817,  1821,  1825,  1829,  1833,  1837,  1841,  1845,
    1849,  1853,  1859,  1860,  1866,  1870,  1876,  1880,  1886,  1890,
    1894,  1898,  1902,  1906,  1912,  1918,  1925,  1929,  1933,  1937,
    1941,  1945,  1951,  1957,  1962,  1968,  1972,  1975,  1979,  1983,
    1990,  1991,  1992,  1993,  1998,  2005,  2006,  2009,  2013,  2013,
    2019,  2020,  2021,  2022,  2023,  2024,  2025,  2026,  2027,  2028,
    2029,  2030,  2031,  2032,  2033,  2034,  2035,  2036,  2037,  2038,
    2039,  2040,  2041,  2042,  2043,  2044,  2045,  2046,  2047,  2048,
    2051,  2051,  2051,  2052,  2052,  2053,  2053,  2053,  2054,  2054,
    2054,  2054,  2055,  2055,  2055,  2056,  2056,  2056,  2057,  2057,
    2057,  2057,  2058,  2058,  2058,  2058,  2059,  2059,  2059,  2059,
    2060,  2060,  2060,  2060,  2061,  2061,  2061,  2061,  2062,  2062,
    2065,  2069,  2073,  2077,  2081,  2085,  2089,  2094,  2099,  2104,
    2108,  2112,  2116,  2120,  2124,  2128,  2132,  2136,  2140,  2144,
    2148,  2152,  2156,  2160,  2164,  2168,  2172,  2176,  2180,  2184,
    2188,  2192,  2196,  2200,  2204,  2208,  2212,  2216,  2220,  2224,
    2228,  2232,  2236,  2242,  2243,  2248,  2252,  2259,  2263,  2271,
    2275,  2301,  2302,  2305,  2306,  2307,  2312,  2317,  2324,  2330,
    2335,  2340,  2345,  2352,  2352,  2363,  2369,  2373,  2379,  2380,
    2383,  2389,  2395,  2400,  2407,  2412,  2417,  2424,  2425,  2426,
    2427,  2428,  2429,  2430,  2431,  2435,  2440,  2439,  2451,  2455,
    2450,  2460,  2460,  2464,  2468,  2472,  2476,  2481,  2486,  2490,
    2494,  2498,  2502,  2506,  2507,  2513,  2519,  2512,  2531,  2539,
    2547,  2547,  2547,  2554,  2554,  2554,  2561,  2567,  2572,  2574,
    2571,  2583,  2581,  2599,  2604,  2597,  2621,  2619,  2636,  2640,
    2635,  2657,  2663,  2656,  2680,  2684,  2688,  2692,  2698,  2705,
    2706,  2707,  2710,  2711,  2714,  2715,  2723,  2724,  2730,  2734,
    2737,  2741,  2745,  2749,  2754,  2758,  2762,  2766,  2772,  2771,
    2781,  2785,  2789,  2793,  2799,  2804,  2809,  2813,  2817,  2821,
    2825,  2829,  2833,  2837,  2841,  2845,  2849,  2853,  2857,  2861,
    2865,  2871,  2876,  2883,  2883,  2887,  2892,  2899,  2903,  2909,
    2910,  2913,  2918,  2921,  2925,  2931,  2935,  2942,  2941,  2956,
    2966,  2970,  2975,  2982,  2986,  2990,  2994,  2998,  3002,  3006,
    3010,  3014,  3021,  3020,  3035,  3034,  3050,  3058,  3067,  3070,
    3077,  3080,  3084,  3085,  3088,  3092,  3095,  3099,  3102,  3103,
    3104,  3105,  3108,  3109,  3115,  3116,  3117,  3121,  3127,  3128,
    3134,  3139,  3138,  3149,  3153,  3159,  3163,  3169,  3173,  3179,
    3182,  3183,  3186,  3192,  3198,  3199,  3202,  3209,  3208,  3222,
    3226,  3233,  3238,  3245,  3251,  3252,  3253,  3254,  3255,  3259,
    3265,  3269,  3275,  3276,  3277,  3281,  3287,  3291,  3295,  3299,
    3303,  3309,  3313,  3319,  3323,  3327,  3331,  3335,  3339,  3347,
    3354,  3365,  3366,  3370,  3374,  3373,  3389,  3395,  3413,  3419,
    3425,  3431,  3438,  3443,  3450,  3454,  3460,  3464,  3470,  3471,
    3474,  3478,  3484,  3488,  3492,  3496,  3502,  3507,  3512,  3516,
    3520,  3524,  3528,  3532,  3536,  3540,  3544,  3548,  3552,  3556,
    3560,  3564,  3569,  3575,  3580,  3585,  3590,  3595,  3602,  3606,
    3613,  3618,  3617,  3629,  3633,  3639,  3647,  3655,  3663,  3667,
    3673,  3677,  3683,  3684,  3687,  3692,  3699,  3700,  3703,  3709,
    3713,  3719,  3724,  3724,  3749,  3750,  3756,  3761,  3767,  3768,
    3771,  3777,  3782,  3792,  3799,  3800,  3801,  3804,  3805,  3806,
    3807,  3810,  3811,  3812,  3815,  3816,  3819,  3823,  3829,  3830,
    3836,  3837,  3840,  3841,  3844,  3847,  3848,  3849,  3852,  3853,
    3854,  3857,  3864,  3865,  3869
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
  "\"end of file\"", "error", "\"invalid token\"", "keyword_class",
  "keyword_module", "keyword_def", "keyword_begin", "keyword_if",
  "keyword_unless", "keyword_while", "keyword_until", "keyword_for",
  "keyword_undef", "keyword_rescue", "keyword_ensure", "keyword_end",
  "keyword_then", "keyword_elsif", "keyword_else", "keyword_case",
  "keyword_when", "keyword_break", "keyword_next", "keyword_redo",
  "keyword_retry", "keyword_in", "keyword_do", "keyword_do_cond",
  "keyword_do_block", "keyword_do_LAMBDA", "keyword_return",
  "keyword_yield", "keyword_super", "keyword_self", "keyword_nil",
  "keyword_true", "keyword_false", "keyword_and", "keyword_or",
  "keyword_not", "modifier_if", "modifier_unless", "modifier_while",
  "modifier_until", "modifier_rescue", "keyword_alias", "keyword_BEGIN",
  "keyword_END", "keyword__LINE__", "keyword__FILE__",
  "keyword__ENCODING__", "tIDENTIFIER", "tFID", "tGVAR", "tIVAR",
  "tCONSTANT", "tCVAR", "tLABEL_TAG", "tINTEGER", "tFLOAT", "tCHAR",
  "tXSTRING", "tREGEXP", "tSTRING", "tSTRING_PART", "tSTRING_MID",
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
  "stmts", "stmt", "$@3", "command_asgn", "command_rhs", "expr",
  "expr_value", "command_call", "block_command", "cmd_brace_block", "$@4",
  "command", "mlhs", "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list",
  "mlhs_post", "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym",
  "undef_list", "$@5", "op", "reswords", "arg", "aref_args", "arg_rhs",
  "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "@6", "block_arg", "opt_block_arg", "comma", "args",
  "mrhs", "primary", "@7", "@8", "$@9", "$@10", "@11", "@12", "$@13",
  "$@14", "$@15", "$@16", "$@17", "$@18", "@19", "@20", "@21", "@22",
  "@23", "@24", "@25", "@26", "primary_value", "then", "do", "if_tail",
  "opt_else", "for_var", "f_margs", "$@27", "block_args_tail",
  "opt_block_args_tail", "block_param", "opt_block_param",
  "block_param_def", "$@28", "opt_bv_decl", "bv_decls", "bvar",
  "f_larglist", "lambda_body", "do_block", "$@29", "block_call",
  "method_call", "brace_block", "@30", "@31", "case_body", "cases",
  "opt_rescue", "exc_list", "exc_var", "opt_ensure", "literal", "string",
  "string_fragment", "string_rep", "string_interp", "@32", "xstring",
  "regexp", "heredoc", "heredoc_bodies", "heredoc_body",
  "heredoc_string_rep", "heredoc_string_interp", "@33", "words", "symbol",
  "basic_symbol", "sym", "symbols", "numeric", "variable", "var_lhs",
  "var_ref", "backref", "superclass", "$@34", "f_arglist", "f_label",
  "f_kw", "f_block_kw", "f_block_kwarg", "f_kwarg", "kwrest_mark",
  "f_kwrest", "args_tail", "opt_args_tail", "f_args", "f_bad_arg",
  "f_norm_arg", "f_arg_item", "@35", "f_arg", "f_opt_asgn", "f_opt",
  "f_block_opt", "f_block_optarg", "f_optarg", "restarg_mark",
  "f_rest_arg", "blkarg_mark", "f_block_arg", "opt_f_block_arg",
  "singleton", "$@36", "assoc_list", "assocs", "label_tag", "assoc",
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

#ifdef YYPRINT
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
#endif

#define YYPACT_NINF (-829)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-595)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACTSTATE-NUM -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -829,   164,  2491,  -829,  7022,  8994,  9330,  5100,  -829,  8646,
    8646,  -829,  -829,  9106,  6520,  4956,  7370,  7370,  -829,  -829,
    7370,  2735,  5870,  -829,  -829,  -829,  -829,   -39,  6520,  -829,
      36,  -829,  -829,  -829,  5240,  5380,  -829,  -829,  5520,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,    20,  8762,  8762,   129,
    4227,  1481,  7602,  7950,  6798,  -829,  6242,   614,   927,  1024,
    1126,   839,  -829,   410,  8878,  8762,  -829,   852,  -829,  1251,
    -829,   448,  -829,  -829,   166,   171,  -829,    80,  9218,  -829,
     198, 11318,   299,   402,    21,    59,  -829,   354,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,   203,   165,  -829,
     340,   137,  -829,  -829,  -829,  -829,  -829,   159,   159,   177,
      72,   552,  -829,  8646,    99,  4344,   607,  -829,   200,  -829,
     494,  -829,  -829,   137,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,    33,    44,    47,   101,  -829,  -829,  -829,  -829,
    -829,  -829,   170,   218,   219,   227,  -829,   229,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,   240,  3417,   270,   448,    83,   225,   526,
      61,   247,    86,    83,  8646,  8646,   539,   306,  -829,  -829,
     609,   329,    95,   110,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  6381,  -829,  -829,   253,  -829,  -829,  -829,
    -829,  -829,  -829,   852,  -829,   264,  -829,   386,  -829,  -829,
     852,  2601,  8762,  8762,  8762,  8762,  -829, 11297,  -829,  -829,
     271,   361,   271,  -829,  -829,  -829,  7138,  -829,  -829,  -829,
    7370,  -829,  -829,  -829,  4956,  8646,  -829,  -829,   286,  4461,
    -829,   796,   355,   398,  7254,  4227,   302,   852,  1251,   852,
     323,  -829,  7254,   852,   325,  1517,  1517,  -829, 11297,   316,
    1517,  -829,   421,  9442,   370,   798,   826,   859,  1597,  -829,
    -829,  -829,  -829,  1166,  -829,  -829,  -829,  -829,  -829,  -829,
     679,   749,  -829,  -829,  1186,  -829,  1195,  -829,  1257,  -829,
     860,   444,   446,  -829,  -829,  -829,  -829,  4722,  8646,  8646,
    8646,  8646,  7254,  8646,  8646,  -829,  -829,  8066,  -829,  4227,
    6910,   392,  8066,  8762,  8762,  8762,  8762,  8762,  8762,  8762,
    8762,  8762,  8762,  8762,  8762,  8762,  8762,  8762,  8762,  8762,
    8762,  8762,  8762,  8762,  8762,  8762,  8762,  8762,  8762,  9714,
    -829,  7370,  -829,  9798,  -829,  -829, 10974,  -829,  -829,  -829,
    -829,  8878,  8878,  -829,   428,  -829,   448,  -829,   961,  -829,
    -829,  -829,  -829,  -829,  9882,  7370,  9966,  3417,  8646,  -829,
    -829,  -829,  -829,   522,   528,   149,  -829,  3561,   533,  8762,
   10050,  7370, 10134,  8762,  8762,  3849,   126,   126,   113, 10218,
    7370, 10302,  -829,   501,  -829,  4461,   386,  -829,  -829,  8182,
     542,  -829,   679,  8762, 11318, 11318, 11318,  8762,   476,  -829,
    7486,  -829,  8762,  -829,  7718,   852,   425,   852,   271,   271,
    -829,  -829,   745,   431,  -829,  -829,  6520,  3966,   443, 10050,
   10134,  8762,  1251,   852,  -829,  -829,  4839,   445,  1251,  -829,
    -829,  7834,  -829,   852,  7950,  -829,  -829,  -829,   961,    80,
    9442,  -829,  9442, 10386,  7370, 10470,    30,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  1719,
    -829,  8762,  -829,   451,   554,   472,  -829,  -829,  -829,  -829,
    -829,   479,  8762,  -829,   497,   596,   511,   608,  -829,  -829,
    1283,  4461,   679,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    8762,  8762,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
     153,  8762,  -829, 11121,   271,  -829,   852,  9442,   532,  -829,
    -829,  -829,   570,   572,  2302,  -829,  -829,   976,   180,   355,
    2331,  2331,  2331,  2331,  1479,  1479, 11455, 11395,  2331,  2331,
   11378, 11378,   671,   671, 11061,  1479,  1479,  1462,  1462,  1490,
     175,   175,   355,   355,   355,  2869,  5986,  3137,  6102,  -829,
     159,  -829,   550,   437,  -829,   563,  -829,  -829,  5870,  -829,
    -829,  2061,   153,   153,  -829, 11044,  -829,  -829,  -829,  -829,
    -829,   852,  8646,  3417,   736,   813,  -829,   159,   560,   159,
     699,   745,  1650,  6659,  -829,  8298,   706,  -829,   690,  -829,
    5636,  5753,   605,   268,   276,   706,  -829,  -829,  -829,  -829,
      79,    88,   613,   121,   140,  8646,  6520,   616,   740, 11318,
     450,  -829,   679, 11318, 11318,   679,  8762, 11297,  -829,   271,
   11318,  -829,  -829,  -829,  -829,  7486,  7718,  -829,  -829,  -829,
     623,  -829,  -829,   136,  1251,   852,  1517,   392,  -829,   736,
     813,   626,   959,  1023,  -829,  -829,  1123,   622,    77, 11318,
     768,  -829,  -829,  -829,   201,  -829,  1719,  -829, 11318,  1719,
    -829,  -829,  1907,  -829,  -829,  -829,   637,  -829,   355,   355,
    -829,  1719,  3417,  -829,  -829, 11140,  8414,  -829,  -829,  9442,
    7254,  8878,  8762, 10554,  7370, 10638,    70,  8878,  8878,  -829,
     428,   672,  8878,  8878,  -829,   428,    59,   166,  3417,  4461,
     153,  -829,   852,   762,  -829,  -829,  -829,  1826,  3417,   852,
    -829, 11121,  -829,   689,  -829,  4110,   774,  -829,  8646,   779,
    -829,  8762,  8762,   358,  8762,  8762,   800,  4605,  4605,   156,
     126,  -829,  -829,  -829,  8530,  3705,   679, 11318,  -829,   271,
    -829,  -829,  -829,   192,  -829,   104,   852,   676,   674,   678,
    3417,  4461,  -829,   766,  -829,   472,  -829,  -829,  -829,   684,
     686,   687,  -829,   691,   766,   687,  -829,  -829,   622,   622,
    9554,  -829,   694,   472,   695,  9554,  -829,   698,   701,  -829,
     835,  8762, 11209,  -829,  -829, 11318,  3003,  3271,   712,   408,
     432,  8762,  8762,  -829,  -829,  -829,  -829,  -829,  8878,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,   840,   722,  4461,  3417,
    -829,  -829,   852,   852,   845,  -829,  1650,  9666,    83,  -829,
    -829,  4605,  -829,  -829,    83,  -829,  8762,  -829,   855,   856,
    -829, 11318,   193,  7718,  -829,   733,  -829,  1530,  -829,   680,
     868,   753,  -829,  1719,  -829,  1907,  -829,  1907,  -829,  1907,
    -829,  -829,   769,   771,   837,   995,   768,  -829,  -829,  1282,
    -829,   995,  1719,  -829,  1907,  -829,  -829, 11228,   439, 11318,
   11318,  -829,  -829,  -829,  -829,   761,   896,  -829,  -829,  -829,
    3417,   862,  -829,  1028,   826,   859,  3417,  -829,  3561,  -829,
    -829,  4605,  -829,  -829,  -829,  1585,  1585,   547,  -829,    22,
    -829,  -829,  -829,  -829,   687,   778,   687,   687,  -829,  -829,
    -829, 10722,  -829,   472,   768,  -829,  -829,   780,   792,   793,
    -829,   799,   793,  -829,  -829,   913,   961, 10806,  7370, 10890,
     528,   690,   935,   812,   812,  1585,   823,   680,  -829,  -829,
    1907,  -829,  -829,  -829,   824,   828,  -829,  1719,  -829,  1907,
    -829,  1907,  -829,  1907,  -829,  -829,  -829,   736,   813,   834,
     357,   579,  -829,  -829,  -829,  1585,   812,  1585,  -829,   687,
     793,   842,   793,   793,   192,   812,  -829,  -829,  1907,  -829,
    -829,  -829,   793,  -829
};

  /* YYDEFACTSTATE-NUM -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   276,     0,
       0,   300,   303,     0,     0,   580,   324,   325,   326,   327,
     288,   253,   400,   475,   474,   476,   477,   582,     0,    10,
       0,   479,   478,   480,   466,   275,   468,   467,   470,   469,
     462,   463,   424,   425,   481,   482,   274,     0,     0,     0,
       0,   278,   594,   594,    80,   295,     0,     0,     0,     0,
       0,     0,   439,     0,     0,     0,     3,   580,     6,     9,
      27,    32,    44,    52,    51,     0,    68,     0,    72,    82,
       0,    49,   232,     0,    53,   293,   267,   268,   422,   269,
     270,   271,   420,   419,   451,   421,   418,   473,     0,   272,
     273,   253,     5,     8,   324,   325,   288,   594,   400,     0,
     105,   106,   274,     0,     0,     0,     0,   108,   483,   328,
       0,   473,   273,     0,   316,   160,   170,   161,   157,   186,
     187,   188,   189,   168,   183,   176,   166,   165,   181,   164,
     163,   159,   184,   158,   171,   175,   177,   169,   162,   178,
     185,   180,   179,   172,   182,   167,   156,   174,   173,   155,
     153,   154,   150,   151,   152,   110,   112,   111,   145,   146,
     141,   123,   124,   125,   132,   129,   131,   126,   127,   147,
     148,   133,   134,   138,   142,   128,   130,   120,   121,   122,
     135,   136,   137,   139,   140,   143,   144,   149,   552,   318,
     113,   114,   551,     0,     0,     0,    50,     0,     0,     0,
     473,     0,   273,     0,     0,     0,   104,     0,   339,   338,
       0,     0,   473,   273,   179,   172,   182,   167,   150,   151,
     152,   110,   111,     0,   115,   117,    20,   116,   442,   447,
     446,   588,   591,   580,   590,     0,   444,     0,   592,   589,
     581,   564,     0,     0,     0,     0,   248,   260,    66,   252,
     594,   422,   594,   556,    67,    65,   594,   242,   289,    64,
       0,   241,   399,    63,   580,     0,   583,    18,     0,     0,
     209,     0,   210,   285,     0,     0,     0,   580,    15,   580,
      70,    14,     0,   580,     0,   585,   585,   233,     0,     0,
     585,   554,     0,     0,    78,     0,    88,    95,   522,   456,
     455,   457,   458,     0,   454,   453,   437,   431,   430,   433,
       0,     0,   428,   449,     0,   460,     0,   426,     0,   435,
       0,   464,   465,    48,   224,   225,     4,   581,     0,     0,
       0,     0,     0,     0,     0,   387,   389,     0,    84,     0,
      76,    73,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     577,   594,   576,     0,   579,   578,     0,   404,   402,   294,
     423,     0,     0,   393,    57,   292,   313,   105,   106,   107,
     464,   465,   484,   311,     0,   594,     0,     0,     0,   319,
     575,   574,   321,     0,   594,   285,   330,     0,   329,     0,
       0,   594,     0,     0,     0,     0,     0,     0,   285,     0,
     594,     0,   308,     0,   118,     0,     0,   443,   445,     0,
       0,   593,   558,     0,   261,   563,   255,     0,   258,   249,
       0,   257,     0,   250,     0,   580,     0,   580,   594,   594,
     243,   254,   580,     0,   291,    47,     0,     0,     0,     0,
       0,     0,    17,   580,   283,    13,   581,    69,   279,   282,
     286,   587,   234,   586,   587,   236,   287,   555,    94,    86,
       0,    81,     0,     0,   594,     0,   529,   525,   524,   523,
     526,   527,   498,   531,   543,   499,   547,   546,   542,   522,
     296,   491,   496,   594,   501,   594,   521,   384,   528,   530,
     533,   507,     0,   540,   507,   545,   507,     0,   505,   459,
       0,     0,   434,   440,   438,   429,   450,   461,   427,   436,
       0,     0,     7,    21,    22,    23,    24,    25,    45,    46,
     594,     0,    28,    30,     0,    31,   580,     0,    74,    85,
      43,    33,    41,     0,   237,   190,    29,     0,   273,   206,
     214,   219,   220,   221,   216,   218,   228,   229,   222,   223,
     199,   200,   226,   227,   582,   215,   217,   211,   212,   213,
     201,   202,   203,   204,   205,   567,   572,   568,   573,   398,
     253,   396,     0,   567,   569,   568,   570,   397,   594,   567,
     568,   253,   594,   594,    34,   237,   191,    40,   198,    55,
      58,     0,     0,     0,   105,   106,   109,     0,     0,   594,
       0,   580,   522,     0,   277,   594,   594,   410,   594,   331,
     571,   284,     0,   567,   568,   594,   333,   301,   332,   304,
     571,   284,     0,   567,   568,     0,     0,     0,     0,   260,
       0,   307,   559,   561,   560,     0,     0,   262,   256,   594,
     562,   557,   240,   239,   244,   245,   247,   290,   584,    19,
       0,    26,   197,    71,    16,   580,   585,    87,    79,    91,
      93,     0,    90,    92,   489,   535,     0,   582,     0,   490,
       0,   503,   550,   500,     0,   504,     0,   514,   536,     0,
     517,   544,     0,   519,   548,   452,     0,   441,   207,   208,
     375,   373,     0,   372,   371,   266,     0,    83,    77,     0,
       0,     0,     0,     0,   594,     0,     0,     0,     0,   395,
      61,   401,     0,     0,   394,    59,   390,    54,     0,     0,
     594,   314,     0,     0,   401,   317,   553,   522,     0,     0,
     322,   411,   412,   594,   413,     0,   594,   336,     0,     0,
     334,     0,     0,   401,     0,     0,     0,     0,     0,   401,
       0,   119,   448,   306,     0,     0,   259,   263,   251,   594,
      11,   280,   235,    89,   529,   347,   580,   340,     0,   377,
       0,     0,   297,     0,   497,   594,   549,   506,   534,   507,
     507,   507,   541,   507,   529,   507,   432,   370,   582,   582,
     493,   494,   594,   594,   355,     0,   538,   355,   355,   353,
       0,     0,   264,    75,    42,   238,   567,   568,     0,   567,
     568,     0,     0,    39,   195,    38,   196,    62,     0,    36,
     193,    37,   194,    60,   391,   392,     0,     0,     0,     0,
     485,   312,   580,   580,     0,   488,   522,     0,     0,   415,
     337,     0,    12,   417,     0,   298,     0,   299,     0,     0,
     309,   262,   594,   246,   348,   345,   532,     0,   383,     0,
       0,     0,   502,     0,   510,     0,   512,     0,   518,     0,
     515,   520,     0,     0,     0,   492,     0,   351,   352,   355,
     363,   537,     0,   366,     0,   368,   388,   265,   401,   231,
     230,    35,   192,   405,   403,     0,     0,   487,   486,   320,
       0,     0,   414,     0,    96,   103,     0,   416,     0,   302,
     305,     0,   407,   408,   406,     0,     0,   343,   381,   582,
     379,   382,   386,   385,   507,   507,   507,   507,   376,   374,
     285,     0,   495,   594,     0,   354,   361,   355,   355,   355,
     539,   355,   355,    56,   315,     0,   102,     0,   594,     0,
     594,   594,     0,   349,   346,     0,   341,     0,   378,   511,
       0,   508,   513,   516,   571,   284,   350,     0,   358,     0,
     360,     0,   367,     0,   364,   369,   323,    99,   101,     0,
     567,   568,   409,   335,   310,     0,   344,     0,   380,   507,
     355,   355,   355,   355,    97,   342,   509,   359,     0,   356,
     362,   365,   355,   357
};

  /* YYPGOTONTERM-NUM.  */
static const yytype_int16 yypgoto[] =
{
    -829,  -829,  -829,   510,  -829,    32,  -829,  -214,   182,  -829,
      28,  -829,  -155,  -302,   867,     1,   -16,  -829,  -536,  -829,
     131,   971,  -170,     4,   -69,  -266,  -431,   -15,  1295,   -48,
     981,    19,     5,  -829,  -829,    24,  -829,   653,  -829,   413,
      75,   -58,  -352,    54,    13,  -829,  -390,  -235,   -11,    39,
    -303,    89,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,  -829,
    -829,  -829,     8,  -206,  -382,     7,  -568,  -829,  -829,  -829,
     272,   538,  -829,  -512,  -829,  -829,   -78,  -829,     2,  -829,
    -829,   255,  -829,  -829,  -829,   -65,  -829,  -829,  -430,  -829,
      14,  -829,  -829,  -829,  -829,  -829,   154,    58,  -196,  -829,
    -829,  -829,  -829,  -377,  -257,  -829,   787,  -829,  -829,  -829,
      -6,  -829,  -829,  -829,  1461,  1552,  1026,  1065,  -829,  -829,
     173,   314,   343,   141,  -829,  -829,  -829,   524,  -306,   246,
    -307,  -801,  -716,  -519,  -829,   474,  -664,  -627,  -828,   142,
     346,  -829,   236,  -829,   517,  -439,  -829,  -829,  -829,    92,
     785,  -411,   505,  -339,  -829,  -829,   -80,  -829,    26,   -22,
    -152,  -254,   788,   -12,   -33,    -2
};

  /* YYDEFGOTONTERM-NUM.  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    66,    67,    68,   278,   413,   414,   287,
     288,   466,    70,   561,    71,   207,    72,    73,   620,   750,
      74,    75,   289,    76,    77,    78,   491,    79,   208,   117,
     118,   234,   235,   236,   656,   598,   201,    81,   294,   565,
     599,   268,   456,   457,   269,   270,   259,   449,   484,   458,
     555,    82,   204,   292,   685,   293,   308,   698,   214,   777,
     215,   778,   655,   941,   623,   621,   859,   407,   409,   632,
     633,   866,   281,   417,   647,   769,   770,   221,   796,   945,
     965,   910,   818,   722,   723,   819,   798,   949,   950,   510,
     802,   346,   550,    84,    85,   395,   613,   612,   440,   944,
     636,   763,   868,   872,    86,    87,    88,   321,   322,   531,
      89,    90,    91,   532,   244,   245,   246,   435,    92,    93,
      94,   315,    95,    96,   210,   211,    99,   212,   403,   622,
     758,   511,   512,   821,   822,   513,   514,   515,   807,   707,
     759,   518,   519,   520,   696,   521,   522,   523,   826,   827,
     524,   525,   526,   527,   528,   701,   203,   408,   299,   459,
     443,   263,   123,   627,   601,   412,   406,   386,   463,   799,
     464,   482,   248,   249,   250,   291
};

  /* YYTABLEYYPACT[STATE-NUM] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     102,   517,   516,   383,   385,   275,   658,   425,   237,   351,
      83,   213,    83,   120,   120,   276,   243,   209,   209,   271,
     389,   220,   237,   209,   209,   209,   199,   453,   209,   602,
      69,   200,    69,   277,   337,   273,   103,   490,   200,   304,
     600,   247,   485,   671,   608,   649,   487,   611,   333,   566,
     297,   301,   200,   628,   290,   260,   260,   825,    83,   260,
     668,   688,   305,   533,   668,   662,   399,   629,   766,   642,
     258,   264,   209,   671,   265,   314,   705,   776,   652,   885,
     200,   600,   812,   608,   970,   387,   305,   694,   951,   614,
     617,   295,   629,   336,   119,   119,   267,   272,  -564,   416,
     748,   749,   119,   274,   -99,   271,   800,   242,   262,   262,
     384,  -472,   262,  -101,   394,   473,   324,   326,   328,   330,
     -96,   209,  -475,    83,   380,   535,   728,   841,   535,   422,
     535,   629,   535,  -474,   535,  -103,  -476,   477,  -102,  -104,
     431,   479,   691,   119,   296,   300,   -98,   256,   256,   695,
     397,   256,  -471,   646,   398,   794,   629,   497,   498,   499,
     500,  -466,   987,   387,     3,  -100,   382,   119,   242,   970,
     261,   261,   279,   501,   261,  -466,   393,   424,  -475,   556,
     -96,   -97,   267,   272,   283,   533,   951,   808,   801,  -474,
    -477,   842,  -476,   630,   345,   388,   238,   560,   393,   239,
     240,   470,   697,   516,   847,  -103,   261,   261,  -564,   853,
    -466,   765,    83,   439,  -564,   426,   427,  -466,  -401,   -91,
     348,  -567,   209,   209,   453,   495,   490,   241,   -93,   242,
    -568,   986,   286,   720,   489,   -88,   560,   560,   858,   238,
     471,   390,   239,   240,   884,   825,  -477,   353,   825,   450,
     -95,   454,   314,   -94,   476,   -69,   391,   200,   451,  -479,
     451,   -90,   483,   483,   460,   671,   812,   483,  -102,   436,
     241,   392,   242,   388,   209,   717,   -83,   721,   209,   266,
     -92,  -401,   209,   209,   481,   668,   668,    83,   786,   290,
     347,   490,    83,    83,  -471,  -401,   -89,   286,   833,  -103,
      83,   266,   506,   672,   376,   377,   378,  -478,  -480,   260,
     677,   305,   472,   475,   942,  -479,  -466,   352,  -470,   274,
     478,   683,   -96,   402,   461,   415,   516,   507,  -401,   410,
    -401,   552,   762,   825,   535,   558,   562,  -401,   423,   543,
     544,   545,   546,   -88,   419,    83,   209,   209,   209,   209,
      83,   209,   209,   290,   432,   209,   626,    83,   305,   774,
     567,   428,   262,  -478,  -480,    69,   892,   775,   808,   542,
     547,   530,  -466,   -98,  -470,   562,   562,   437,   808,   460,
     239,   240,   838,   907,   908,   411,   554,   -98,  -328,   209,
     808,   554,   119,   434,   600,  -100,   608,   256,   880,   567,
     567,   256,  -328,   460,   727,   717,   439,   606,   533,   753,
     606,   448,   637,   209,    42,    83,   209,    43,   442,   460,
     261,   687,   467,   489,   261,    83,   665,   353,   460,   209,
     606,   392,   792,    83,   788,   843,   276,  -328,   209,   119,
     849,   851,   -68,    83,  -328,   474,   606,   675,   676,   876,
     863,   516,   943,   486,   785,   606,   451,   451,   607,  -103,
     237,   468,    60,   490,   480,   102,   416,   286,   331,   332,
     -98,   679,   671,   -98,   -98,    83,   488,   -97,   660,   756,
     -95,   607,   808,   674,    83,   343,   344,   735,   489,   471,
     200,   379,   460,   668,   606,    69,   808,   607,   305,   742,
     305,   -98,   209,   -98,   684,   380,   607,   101,   830,   101,
     492,   702,   256,   702,   101,   101,   540,  -102,   541,   606,
     101,   101,   101,   743,   996,   101,   619,   -98,   742,   717,
     848,   286,   559,   791,   856,   261,   256,   634,   -94,    83,
     381,   635,   669,   726,   864,   607,   921,   382,   724,   639,
     744,  -100,   256,   746,   788,   101,   -98,   661,   -97,   261,
     516,   256,   736,   238,   529,   305,   239,   240,   673,   101,
     607,   744,   276,   686,   678,   261,   560,   -90,  -565,   119,
     681,   119,   560,   404,   261,   -83,   890,   560,   560,  -582,
     448,   700,  -582,  -582,   241,  -100,   242,   380,   794,   638,
     497,   498,   499,   500,   261,   703,   271,   645,   261,   271,
     724,   724,   704,   740,   730,   420,   501,   657,   101,   706,
     101,   745,   242,   752,   747,   256,  1009,   271,  -274,   380,
     209,    83,   405,   764,   767,   261,   767,   709,   261,   382,
     629,  -470,  -274,   767,   886,   926,   119,   711,   261,   784,
     237,   712,   760,   483,   743,  -470,   780,   200,   454,   714,
     489,   781,   936,   209,   421,   400,   401,   451,   938,   257,
     257,   382,   729,   257,   554,   739,   316,  -274,   317,   318,
     200,   854,  -100,   267,  -274,   276,   267,   985,  -565,   741,
    -470,   731,  -100,   560,  -565,  -100,  -100,  -470,   429,   754,
     280,   282,   739,   -92,   267,   257,   298,   768,   765,   101,
     927,   928,   380,   716,   755,   562,   975,   334,   335,   101,
     101,   562,   845,  -100,   765,  -100,   562,   562,   319,   320,
      83,   948,   460,   497,   498,   499,   500,   305,    83,   567,
     902,   903,   209,   353,   773,   567,   209,   430,   724,   501,
     567,   567,   779,   782,   382,   783,    83,    83,   834,   606,
     790,   869,  -571,   848,   873,   793,    83,   789,   242,   874,
     710,   101,   713,    83,   816,   101,   209,   861,   883,   101,
     101,   867,   343,   344,   101,    83,    83,   451,   871,   101,
     101,   -97,   238,    83,   875,   239,   240,   101,   374,   375,
     376,   377,   378,   702,   616,   618,   276,   276,    83,    83,
     607,   534,   -89,   317,   318,   877,   887,   888,   119,   803,
     702,   702,   889,   694,   893,  -571,   895,   897,   905,   261,
     261,   899,   562,   911,   906,   909,   616,   618,   912,  -571,
     502,   914,   101,   101,   101,   101,   101,   101,   101,   101,
     916,   918,   101,   979,   101,   923,   567,   101,   238,   924,
     929,   239,   240,   319,   320,   256,    83,    83,   505,   506,
     939,   940,  -571,   946,  -571,   933,   206,   206,  -567,    83,
     767,  -571,   206,   952,   682,   469,   101,   493,   261,   241,
     953,   242,   960,   958,   507,   959,   101,   101,   973,   380,
     329,   380,  -284,   317,   318,   444,   445,   446,   334,   119,
     101,   974,   101,   101,   119,  -473,  -284,   976,   990,   257,
     997,   539,   101,   257,   317,   318,   101,   988,  1006,  -473,
     101,   857,   999,  1001,   421,   101,   494,   276,    83,  1003,
     101,   382,   810,   382,    83,   813,    83,   870,  -273,    83,
    1014,  -284,  1015,   319,   320,  -568,   119,   828,  -284,   878,
     879,   702,  -273,  1017,  -473,   238,  -567,   882,   239,   240,
    -568,  -473,   101,  1024,   319,   320,   460,   680,   637,   767,
     396,   101,  1028,   891,   218,  -567,   209,   124,  1013,  1018,
     323,   317,   318,   817,  1012,   418,   241,  -273,   242,   101,
     553,   418,   855,   606,  -273,   564,   569,   570,   571,   572,
     573,   574,   575,   576,   577,   578,   579,   580,   581,   582,
     583,   584,   585,   586,   587,   588,   589,   590,   591,   592,
     593,   594,   438,   202,   257,   820,   101,   261,   441,   930,
     925,   319,   320,   804,   615,   615,   452,   962,  -567,  -568,
    -285,   967,   809,   937,   607,   894,   896,   898,   257,   900,
       0,   901,  -567,     0,  -285,   733,     0,   100,     0,   100,
     122,   122,   615,     0,   257,     0,   615,   615,   223,   380,
       0,   206,   206,   257,   961,     0,     0,   325,   317,   318,
       0,     0,   659,     0,     0,  -567,   663,  -567,   380,  -285,
     664,  -567,     0,   667,  -567,   670,  -285,   298,     0,   256,
       0,     0,  -568,     0,   734,   100,     0,   977,   980,   307,
     981,   382,     0,   982,   615,   441,  -568,   101,   101,   955,
       0,   380,   261,   405,   667,     0,     0,   298,   319,   320,
     382,   462,   465,   307,     0,   968,     0,   257,   971,     0,
     844,   846,     0,     0,     0,   850,   852,     0,     0,  -568,
     101,  -568,     0,     0,   699,  -568,   978,     0,  -568,     0,
     797,     0,     0,   382,   794,   708,   497,   498,   499,   500,
     100,     0,     0,   811,   844,   846,   815,   850,   852,   327,
     317,   318,   501,   718,   719,   824,     0,     0,     0,     0,
     989,   991,   992,   993,   725,   206,   206,   206,   206,     0,
     548,   549,     0,     0,   648,   648,   503,   806,     0,     0,
     820,   806,   795,   820,   805,     0,   820,   101,   820,   529,
     317,   318,     0,  1021,     0,   101,   101,     0,   829,   101,
     319,   320,   101,   101,     0,   823,     0,   101,   101,   536,
     317,   318,     0,   101,   101,     0,     0,     0,   537,   317,
     318,   922,     0,   101,   441,  1026,     0,     0,     0,   100,
     101,   441,     0,   101,     0,   631,     0,     0,   820,     0,
     319,   320,   101,   101,     0,     0,     0,     0,   761,   922,
     101,   338,   339,   340,   341,   342,     0,    80,     0,    80,
     319,   320,     0,     0,     0,   101,   101,     0,   219,   319,
     320,   820,     0,   820,     0,   820,     0,   820,     0,   787,
     538,   317,   318,     0,     0,     0,     0,     0,   667,   298,
       0,     0,     0,   496,     0,   497,   498,   499,   500,     0,
       0,     0,   820,     0,   100,    80,   715,   317,   318,   100,
     100,   501,     0,   101,   502,     0,     0,   100,     0,     0,
       0,     0,     0,   101,   101,   913,   915,   954,   307,   956,
       0,   319,   320,   957,     0,   503,   101,     0,     0,   832,
       0,   504,   505,   506,   615,   835,   969,   257,   972,     0,
     615,   615,     0,     0,     0,   615,   615,   319,   320,     0,
       0,     0,   100,     0,     0,     0,     0,   100,   507,   751,
      80,   508,     0,     0,   100,   307,     0,   568,     0,   983,
     984,     0,   964,   806,   615,   615,   829,   615,   615,   829,
     963,   829,     0,   823,     0,   101,   823,   881,   823,     0,
       0,   101,     0,   101,     0,     0,   101,   966,   418,     0,
       0,     0,     0,     0,     0,     0,   568,   568,     0,  1016,
       0,     0,     0,    97,  1019,    97,   121,   121,   121,     0,
       0,  1020,   100,  1022,   222,     0,     0,  1023,     0,     0,
       0,   829,   100,   101,   917,     0,     0,     0,   823,   206,
     100,  1025,     0,     0,   919,   920,     0,     0,     0,    80,
     100,   615,  1032,     0,     0,   998,  1000,  1002,     0,  1004,
    1005,    97,     0,     0,   829,   306,   829,     0,   829,     0,
     829,   823,   206,   823,     0,   823,     0,   823,     0,   615,
       0,     0,   100,     0,   353,     0,   298,     0,     0,   306,
     860,   100,     0,     0,     0,   829,     0,   865,     0,   366,
     367,   353,   823,     0,    98,   307,    98,   307,  1027,  1029,
    1030,  1031,   353,     0,     0,     0,   366,   367,   648,     0,
    1033,     0,     0,     0,    80,     0,    97,   366,   367,    80,
      80,   794,     0,   497,   498,   499,   500,    80,   373,   374,
     375,   376,   377,   378,  -281,     0,   100,  -281,  -281,   501,
       0,     0,    98,   371,   372,   373,   374,   375,   376,   377,
     378,     0,     0,     0,     0,     0,     0,   374,   375,   376,
     377,   378,   307,   503,  -281,  -281,     0,  -281,     0,   947,
     238,   257,    80,   239,   240,   206,   794,    80,   497,   498,
     499,   500,     0,     0,    80,     0,     0,   563,   496,     0,
     497,   498,   499,   500,   501,     0,   418,   448,     0,     0,
       0,   241,   418,   242,     0,    97,   501,    98,     0,   502,
       0,     0,     0,     0,     0,     0,     0,     0,   503,     0,
       0,     0,     0,     0,     0,     0,   563,   563,   100,     0,
     503,     0,     0,     0,     0,     0,   504,   505,   506,     0,
       0,   496,    80,   497,   498,   499,   500,     0,     0,     0,
       0,     0,    80,     0,     0,     0,     0,     0,     0,   501,
      80,     0,   502,   507,     0,     0,   508,     0,     0,     0,
      80,     0,     0,     0,     0,     0,     0,     0,     0,   509,
      97,     0,     0,   503,     0,    97,    97,     0,     0,   504,
     505,   506,     0,    97,     0,     0,    98,     0,     0,     0,
       0,     0,    80,     0,   306,     0,     0,     0,     0,     0,
     496,    80,   497,   498,   499,   500,   507,     0,     0,   508,
       0,     0,     0,     0,     0,     0,     0,   100,   501,     0,
       0,   502,   757,     0,   307,   100,   568,     0,    97,     0,
       0,     0,   568,    97,     0,     0,     0,   568,   568,     0,
      97,   306,   503,   100,   100,     0,     0,     0,   504,   505,
     506,     0,     0,   100,     0,     0,    80,     0,     0,     0,
     100,    98,     0,     0,     0,     0,    98,    98,     0,     0,
       0,     0,   100,   100,    98,   507,     0,     0,   508,     0,
     100,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   100,   100,     0,    97,     0,
       0,     0,     0,     0,     0,     0,     0,   496,    97,   497,
     498,   499,   500,     0,     0,   122,    97,     0,     0,    98,
     122,     0,     0,     0,    98,   501,    97,     0,   502,     0,
       0,    98,     0,     0,    98,     0,     0,     0,     0,     0,
     862,     0,     0,   568,     0,     0,     0,     0,    80,   503,
       0,     0,     0,   100,   100,   504,   505,   506,    97,     0,
       0,     0,   935,     0,     0,     0,   100,    97,     0,     0,
       0,     0,     0,    98,    98,     0,     0,     0,     0,     0,
       0,   306,   507,   306,     0,   508,     0,     0,   814,    98,
     497,   498,   499,   500,     0,     0,     0,     0,     0,    98,
       0,     0,     0,     0,     0,     0,   501,    98,     0,   502,
       0,     0,     0,     0,     0,     0,     0,    98,     0,     0,
       0,     0,    97,     0,     0,   100,     0,     0,     0,     0,
     503,   100,     0,   100,     0,     0,   100,   505,   506,     0,
       0,     0,     0,     0,     0,     0,     0,    80,   306,    98,
       0,     0,     0,     0,     0,    80,   563,     0,    98,     0,
       0,     0,   563,   507,     0,     0,     0,   563,   563,     0,
       0,     0,     0,    80,    80,     0,     0,     0,     0,     0,
       0,     0,     0,    80,     0,     0,     0,     0,     0,     0,
      80,  -594,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    80,    80,  -594,  -594,  -594,  -594,  -594,  -594,
      80,  -594,     0,    98,    97,     0,     0,  -594,  -594,     0,
       0,     0,     0,     0,     0,    80,    80,     0,  -594,  -594,
       0,  -594,  -594,  -594,  -594,  -594,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   563,     0,     0,     0,     0,     0,     0,
    -594,     0,     0,    80,    80,     0,     0,     0,     0,     0,
       0,     0,   932,     0,  -594,     0,    80,     0,     0,     0,
       0,     0,     0,     0,  -594,    98,     0,  -594,  -594,     0,
       0,     0,     0,    97,     0,     0,     0,     0,     0,     0,
     306,    97,     0,     0,     0,     0,     0,  -594,  -594,     0,
       0,     0,     0,   266,  -594,  -594,  -594,  -594,     0,    97,
      97,     0,     0,     0,     0,     0,     0,     0,     0,    97,
       0,     0,     0,     0,     0,    80,    97,     0,     0,     0,
       0,    80,     0,    80,     0,     0,    80,     0,    97,    97,
       0,     0,     0,     0,     0,     0,    97,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    97,    97,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    98,     0,     0,     0,     0,     0,
       0,   121,    98,    98,     0,     0,   121,     0,     0,    98,
       0,     0,     0,     0,    98,    98,     0,     0,     0,     0,
      98,    98,     0,     0,     0,     0,     0,     0,     0,     0,
      98,     0,     0,     0,     0,     0,     0,    98,     0,    97,
      97,     0,     0,     0,     0,     0,     0,     0,   934,    98,
      98,     0,    97,     0,     0,     0,     0,    98,     0,     0,
       0,     0,     0,     0,     0,     0,   732,     0,     0,     0,
       0,     0,    98,    98,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,   363,   364,   365,     0,     0,   366,
     367,    97,     0,     0,     0,     0,     0,    97,     0,    97,
      98,     0,    97,   353,  -595,  -595,  -595,  -595,   358,   359,
      98,    98,  -595,  -595,     0,     0,     0,     0,   366,   367,
       0,     0,   368,    98,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,     0,     0,     0,     0,     0,     0,
       0,     0,  -260,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   369,   370,   371,   372,   373,   374,   375,
     376,   377,   378,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    98,     0,     0,     0,     0,     0,    98,     0,
      98,  -594,     4,    98,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,     0,     0,     0,     0,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,     0,     0,    50,    51,     0,    52,    53,     0,
      54,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,  -466,     0,    62,  -594,     0,     0,  -594,  -594,     0,
       0,     0,     0,     0,  -466,  -466,  -466,  -466,  -466,  -466,
       0,  -466,     0,    63,    64,    65,     0,     0,  -466,  -466,
       0,     0,     0,     0,     0,  -594,     0,  -594,  -466,  -466,
       0,  -466,  -466,  -466,  -466,  -466,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   442,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -466,  -466,  -466,  -466,  -466,  -466,  -466,
    -466,  -466,  -466,  -466,  -466,  -466,     0,     0,  -466,  -466,
    -466,     0,  -466,  -466,     0,     0,     0,     0,     0,  -466,
       0,     0,     0,     0,  -466,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -466,     0,     0,  -466,  -466,     0,
    -466,  -466,     0,  -466,  -466,  -466,  -466,  -466,  -466,  -466,
    -466,  -466,  -466,     0,     0,  -594,     0,     0,  -466,  -466,
    -466,  -466,     0,     0,  -466,  -466,  -466,  -466,  -594,  -594,
    -594,  -594,  -594,  -594,     0,  -594,     0,     0,     0,     0,
       0,     0,  -594,  -594,     0,     0,     0,     0,     0,     0,
       0,     0,  -594,  -594,     0,  -594,  -594,  -594,  -594,  -594,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -594,  -594,  -594,
    -594,  -594,  -594,  -594,  -594,  -594,  -594,  -594,  -594,  -594,
       0,     0,  -594,  -594,  -594,     0,     0,  -594,     0,     0,
       0,     0,     0,  -594,     0,     0,     0,     0,  -594,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -594,     0,
       0,  -594,  -594,     0,     0,  -594,     0,  -594,  -594,  -594,
    -594,  -594,  -594,  -594,  -594,  -594,  -594,     0,     0,  -571,
       0,     0,  -594,  -594,  -594,  -594,     0,   266,  -594,  -594,
    -594,  -594,  -571,  -571,  -571,     0,  -571,  -571,     0,  -571,
       0,     0,     0,     0,     0,  -571,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -571,  -571,     0,  -571,
    -571,  -571,  -571,  -571,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -571,  -571,  -571,  -571,  -571,  -571,  -571,  -571,  -571,
    -571,  -571,  -571,  -571,     0,     0,  -571,  -571,  -571,     0,
     737,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -571,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -571,     0,     0,  -571,  -571,     0,   -99,  -571,
       0,  -571,  -571,  -571,  -571,  -571,  -571,  -571,  -571,  -571,
    -571,     0,     0,  -571,     0,  -571,  -571,  -571,     0,   -91,
       0,     0,  -571,  -571,  -571,  -571,  -571,  -571,  -571,     0,
    -571,  -571,     0,  -571,     0,     0,     0,     0,     0,  -571,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -571,  -571,     0,  -571,  -571,  -571,  -571,  -571,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -571,  -571,  -571,  -571,  -571,
    -571,  -571,  -571,  -571,  -571,  -571,  -571,  -571,     0,     0,
    -571,  -571,  -571,     0,   737,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -571,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -571,     0,     0,  -571,
    -571,     0,   -99,  -571,     0,  -571,  -571,  -571,  -571,  -571,
    -571,  -571,  -571,  -571,  -571,     0,     0,  -284,     0,  -571,
    -571,  -571,     0,  -571,     0,     0,  -571,  -571,  -571,  -571,
    -284,  -284,  -284,     0,  -284,  -284,     0,  -284,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -284,  -284,     0,  -284,  -284,  -284,
    -284,  -284,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -284,
    -284,  -284,  -284,  -284,  -284,  -284,  -284,  -284,  -284,  -284,
    -284,  -284,     0,     0,  -284,  -284,  -284,     0,   738,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -284,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -284,     0,     0,  -284,  -284,     0,  -101,  -284,     0,  -284,
    -284,  -284,  -284,  -284,  -284,  -284,  -284,  -284,  -284,     0,
       0,  -284,     0,     0,  -284,  -284,     0,   -93,     0,     0,
    -284,  -284,  -284,  -284,  -284,  -284,  -284,     0,  -284,  -284,
       0,  -284,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -284,  -284,
       0,  -284,  -284,  -284,  -284,  -284,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -284,  -284,  -284,  -284,  -284,  -284,  -284,
    -284,  -284,  -284,  -284,  -284,  -284,     0,     0,  -284,  -284,
    -284,     0,   738,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -284,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -284,     0,     0,  -284,  -284,     0,
    -101,  -284,     0,  -284,  -284,  -284,  -284,  -284,  -284,  -284,
    -284,  -284,  -284,     0,     0,     0,     0,     0,  -284,  -284,
       0,  -284,     0,     0,  -284,  -284,  -284,  -284,   284,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
    -594,  -594,  -594,     0,     0,  -594,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,     0,     0,
      50,    51,     0,    52,    53,     0,    54,     0,     0,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,    62,
    -594,     0,     0,  -594,  -594,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    63,
      64,    65,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -594,   284,  -594,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,  -594,     0,  -594,  -594,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,     0,     0,    50,    51,     0,    52,    53,     0,
      54,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,    62,  -594,     0,     0,  -594,  -594,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,    65,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -594,   284,  -594,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
    -594,     0,     0,  -594,    15,  -594,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,     0,     0,    50,    51,
       0,    52,    53,     0,    54,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,    62,  -594,     0,
       0,  -594,  -594,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    63,    64,    65,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -594,
     284,  -594,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,  -594,     0,     0,  -594,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
       0,     0,    50,    51,     0,    52,    53,     0,    54,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,     0,
       0,    62,  -594,     0,     0,  -594,  -594,     4,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,    63,    64,    65,     0,    15,     0,    16,    17,    18,
      19,     0,     0,  -594,     0,  -594,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,     0,     0,    50,
      51,     0,    52,    53,     0,    54,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,    62,  -594,
       0,     0,  -594,  -594,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
      65,     0,     0,  -594,     0,     0,     0,     0,     0,     0,
    -594,   284,  -594,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,  -594,  -594,     0,     0,     0,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,     0,     0,    50,    51,     0,    52,    53,     0,    54,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
       0,     0,    62,  -594,     0,     0,  -594,  -594,   284,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    63,    64,    65,     0,    15,     0,    16,    17,
      18,    19,     0,     0,  -594,     0,  -594,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,     0,     0,
     285,    51,     0,    52,    53,     0,    54,     0,     0,    55,
       0,    56,    57,    58,    59,    60,    61,     0,     0,    62,
    -594,     0,     0,  -594,  -594,   284,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    63,
      64,    65,     0,    15,     0,    16,    17,    18,    19,     0,
    -594,  -594,     0,  -594,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,     0,     0,    50,    51,     0,
      52,    53,     0,    54,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,    62,  -594,     0,     0,
    -594,  -594,   284,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    63,    64,    65,     0,
      15,     0,    16,    17,    18,    19,     0,  -594,  -594,     0,
    -594,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,     0,     0,    50,    51,     0,    52,    53,     0,
      54,     0,     0,    55,     0,    56,    57,    58,    59,    60,
      61,     0,     0,    62,  -594,     0,     0,  -594,  -594,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    63,    64,    65,     0,     0,  -594,     0,
       0,     0,     0,     0,     0,  -594,   284,  -594,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
    -594,     0,     0,     0,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,     0,     0,    50,    51,
       0,    52,    53,     0,    54,     0,     0,    55,     0,    56,
      57,    58,    59,    60,    61,     0,     0,    62,  -594,     0,
       0,  -594,  -594,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    63,    64,    65,
       0,    15,     0,    16,    17,    18,    19,     0,     0,  -594,
       0,  -594,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,     0,     0,    50,    51,     0,    52,    53,
       0,    54,     0,     0,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,    62,   238,     0,     0,   239,   240,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,    63,    64,    65,     0,    15,     0,
      16,    17,    18,    19,     0,     0,   241,     0,   242,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
       0,     0,    50,    51,     0,    52,    53,     0,    54,     0,
       0,    55,     0,    56,    57,    58,    59,    60,    61,     0,
       0,    62,   238,     0,     0,   239,   240,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,    63,    64,    65,     0,    15,     0,    16,    17,    18,
      19,     0,     0,   241,     0,   242,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   205,     0,     0,   115,
      51,     0,    52,    53,     0,     0,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,    62,   238,
       0,     0,   239,   240,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    63,    64,
      65,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     241,     0,   242,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,     0,     0,     0,
     149,   150,   151,   152,   153,   154,   155,   156,   157,   158,
       0,     0,     0,     0,     0,   159,   160,   161,   162,   163,
     164,   165,   166,    36,    37,   167,    39,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,   169,   170,   171,   172,   173,   174,   175,   176,     0,
       0,   177,   178,     0,     0,   179,   180,   181,   182,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   185,   186,   187,   188,   189,   190,   191,   192,
     193,   194,     0,   195,   196,     0,     0,     0,     0,     0,
       0,   197,   198,  -564,  -564,  -564,  -564,  -564,  -564,  -564,
    -564,  -564,     0,     0,     0,     0,     0,     0,     0,  -564,
       0,  -564,  -564,  -564,  -564,     0,  -564,     0,     0,     0,
    -564,  -564,  -564,  -564,  -564,  -564,  -564,     0,     0,  -564,
       0,     0,     0,     0,     0,     0,     0,     0,  -564,  -564,
    -564,  -564,  -564,  -564,  -564,  -564,  -564,     0,  -564,  -564,
    -564,     0,     0,  -564,     0,     0,  -564,  -564,     0,  -564,
    -564,  -564,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -564,     0,     0,  -564,  -564,     0,  -564,  -564,     0,  -564,
    -564,  -564,  -564,     0,  -564,  -564,  -564,  -564,  -564,  -564,
       0,     0,  -564,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -564,  -564,  -564,     0,  -564,     0,     0,     0,
       0,     0,  -564,  -566,  -566,  -566,  -566,  -566,  -566,  -566,
    -566,  -566,     0,     0,     0,     0,     0,     0,     0,  -566,
       0,  -566,  -566,  -566,  -566,     0,  -566,     0,     0,     0,
    -566,  -566,  -566,  -566,  -566,  -566,  -566,     0,     0,  -566,
       0,     0,     0,     0,     0,     0,     0,     0,  -566,  -566,
    -566,  -566,  -566,  -566,  -566,  -566,  -566,     0,  -566,  -566,
    -566,     0,     0,  -566,     0,     0,  -566,  -566,     0,  -566,
    -566,  -566,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -566,     0,     0,  -566,  -566,     0,  -566,  -566,     0,  -566,
    -566,  -566,  -566,     0,  -566,  -566,  -566,  -566,  -566,  -566,
       0,     0,  -566,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -566,  -566,  -566,     0,  -566,     0,     0,     0,
       0,     0,  -566,  -565,  -565,  -565,  -565,  -565,  -565,  -565,
    -565,  -565,     0,     0,     0,     0,     0,     0,     0,  -565,
       0,  -565,  -565,  -565,  -565,     0,  -565,     0,     0,     0,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,     0,     0,  -565,
       0,     0,     0,     0,     0,     0,     0,     0,  -565,  -565,
    -565,  -565,  -565,  -565,  -565,  -565,  -565,     0,  -565,  -565,
    -565,     0,     0,  -565,     0,     0,  -565,  -565,     0,  -565,
    -565,  -565,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -565,     0,     0,  -565,  -565,     0,  -565,  -565,     0,  -565,
    -565,  -565,  -565,     0,  -565,  -565,  -565,  -565,  -565,  -565,
       0,     0,  -565,     0,     0,     0,     0,     0,     0,  -567,
    -567,  -567,  -567,  -567,  -567,  -567,  -567,  -567,     0,     0,
       0,     0,  -565,  -565,  -565,  -567,  -565,  -567,  -567,  -567,
    -567,     0,  -565,     0,     0,     0,  -567,  -567,  -567,  -567,
    -567,  -567,  -567,     0,     0,  -567,     0,     0,     0,     0,
       0,     0,     0,     0,  -567,  -567,  -567,  -567,  -567,  -567,
    -567,  -567,  -567,     0,  -567,  -567,  -567,     0,     0,  -567,
       0,     0,  -567,  -567,     0,  -567,  -567,  -567,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -567,   771,     0,  -567,
    -567,     0,  -567,  -567,     0,  -567,  -567,  -567,  -567,     0,
    -567,  -567,  -567,  -567,  -567,  -567,     0,     0,  -567,     0,
       0,     0,     0,     0,     0,   -99,  -568,  -568,  -568,  -568,
    -568,  -568,  -568,  -568,  -568,     0,     0,     0,  -567,  -567,
    -567,     0,  -568,     0,  -568,  -568,  -568,  -568,  -567,     0,
       0,     0,     0,  -568,  -568,  -568,  -568,  -568,  -568,  -568,
       0,     0,  -568,     0,     0,     0,     0,     0,     0,     0,
       0,  -568,  -568,  -568,  -568,  -568,  -568,  -568,  -568,  -568,
       0,  -568,  -568,  -568,     0,     0,  -568,     0,     0,  -568,
    -568,     0,  -568,  -568,  -568,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -568,   772,     0,  -568,  -568,     0,  -568,
    -568,     0,  -568,  -568,  -568,  -568,     0,  -568,  -568,  -568,
    -568,  -568,  -568,     0,     0,  -568,     0,     0,     0,     0,
       0,     0,  -101,  -253,  -253,  -253,  -253,  -253,  -253,  -253,
    -253,  -253,     0,     0,     0,  -568,  -568,  -568,     0,  -253,
       0,  -253,  -253,  -253,  -253,  -568,     0,     0,     0,     0,
    -253,  -253,  -253,  -253,  -253,  -253,  -253,     0,     0,  -253,
       0,     0,     0,     0,     0,     0,     0,     0,  -253,  -253,
    -253,  -253,  -253,  -253,  -253,  -253,  -253,     0,  -253,  -253,
    -253,     0,     0,  -253,     0,     0,  -253,  -253,     0,  -253,
    -253,  -253,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -253,     0,     0,  -253,  -253,     0,  -253,  -253,     0,  -253,
    -253,  -253,  -253,     0,  -253,  -253,  -253,  -253,  -253,  -253,
       0,     0,  -253,     0,     0,     0,     0,     0,     0,  -569,
    -569,  -569,  -569,  -569,  -569,  -569,  -569,  -569,     0,     0,
       0,     0,  -253,  -253,  -253,  -569,     0,  -569,  -569,  -569,
    -569,     0,   266,     0,     0,     0,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,     0,     0,  -569,     0,     0,     0,     0,
       0,     0,     0,     0,  -569,  -569,  -569,  -569,  -569,  -569,
    -569,  -569,  -569,     0,  -569,  -569,  -569,     0,     0,  -569,
       0,     0,  -569,  -569,     0,  -569,  -569,  -569,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -569,     0,     0,  -569,
    -569,     0,  -569,  -569,     0,  -569,  -569,  -569,  -569,     0,
    -569,  -569,  -569,  -569,  -569,  -569,     0,     0,  -569,     0,
       0,     0,     0,     0,     0,  -570,  -570,  -570,  -570,  -570,
    -570,  -570,  -570,  -570,     0,     0,     0,     0,  -569,  -569,
    -569,  -570,     0,  -570,  -570,  -570,  -570,     0,  -569,     0,
       0,     0,  -570,  -570,  -570,  -570,  -570,  -570,  -570,     0,
       0,  -570,     0,     0,     0,     0,     0,     0,     0,     0,
    -570,  -570,  -570,  -570,  -570,  -570,  -570,  -570,  -570,     0,
    -570,  -570,  -570,     0,     0,  -570,     0,     0,  -570,  -570,
       0,  -570,  -570,  -570,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -570,     0,     0,  -570,  -570,     0,  -570,  -570,
       0,  -570,  -570,  -570,  -570,     0,  -570,  -570,  -570,  -570,
    -570,  -570,     0,     0,  -570,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -570,  -570,  -570,     0,     0,     0,
       0,     0,     0,     0,  -570,   125,   126,   127,   128,   129,
     130,   131,   132,   133,   134,   135,   136,   137,   138,   139,
     140,   141,   142,   143,   144,   145,   146,   147,   148,     0,
       0,     0,   149,   150,   151,   224,   225,   226,   227,   156,
     157,   158,     0,     0,     0,     0,     0,   159,   160,   161,
     228,   229,   230,   231,   166,   309,   310,   232,   311,     0,
       0,     0,     0,     0,     0,   312,     0,     0,     0,     0,
       0,     0,   168,   169,   170,   171,   172,   173,   174,   175,
     176,     0,     0,   177,   178,     0,     0,   179,   180,   181,
     182,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,   184,     0,     0,     0,     0,     0,     0,     0,
     313,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,     0,   195,   196,     0,     0,     0,
       0,     0,     0,   197,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,     0,     0,
       0,   149,   150,   151,   224,   225,   226,   227,   156,   157,
     158,     0,     0,     0,     0,     0,   159,   160,   161,   228,
     229,   230,   231,   166,   309,   310,   232,   311,     0,     0,
       0,     0,     0,     0,   312,     0,     0,     0,     0,     0,
       0,   168,   169,   170,   171,   172,   173,   174,   175,   176,
       0,     0,   177,   178,     0,     0,   179,   180,   181,   182,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     183,   184,     0,     0,     0,     0,     0,     0,     0,   433,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   185,   186,   187,   188,   189,   190,   191,
     192,   193,   194,     0,   195,   196,     0,     0,     0,     0,
       0,     0,   197,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,     0,     0,     0,
     149,   150,   151,   224,   225,   226,   227,   156,   157,   158,
       0,     0,     0,     0,     0,   159,   160,   161,   228,   229,
     230,   231,   166,     0,     0,   232,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,   169,   170,   171,   172,   173,   174,   175,   176,     0,
       0,   177,   178,     0,     0,   179,   180,   181,   182,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
     184,     0,     0,     0,   233,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   185,   186,   187,   188,   189,   190,   191,   192,
     193,   194,     0,   195,   196,     0,     0,     0,     0,     0,
       0,   197,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,   139,   140,   141,   142,
     143,   144,   145,   146,   147,   148,     0,     0,     0,   149,
     150,   151,   224,   225,   226,   227,   156,   157,   158,     0,
       0,     0,     0,     0,   159,   160,   161,   228,   229,   230,
     231,   166,     0,     0,   232,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   168,
     169,   170,   171,   172,   173,   174,   175,   176,     0,     0,
     177,   178,     0,     0,   179,   180,   181,   182,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   183,   184,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   185,   186,   187,   188,   189,   190,   191,   192,   193,
     194,     0,   195,   196,     0,     0,     0,     0,     0,     0,
     197,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,     0,   104,
     105,    18,    19,     0,     0,     0,     0,     0,   106,   107,
     108,    23,    24,    25,    26,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   302,     0,
       0,   115,    51,     0,    52,    53,     0,     0,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
      62,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
     116,   104,   105,    18,    19,     0,     0,     0,   303,     0,
     106,   107,   108,    23,    24,    25,    26,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     302,     0,     0,   115,    51,     0,    52,    53,     0,     0,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
       0,     0,    62,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,     0,     0,     0,
       0,    15,   116,    16,    17,    18,    19,     0,     0,     0,
     557,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,     0,     0,    50,    51,     0,    52,    53,
       0,    54,     0,     0,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,    62,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    63,    64,    65,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   251,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   455,     0,     0,     0,     0,     0,   205,     0,
       0,   115,    51,     0,    52,    53,     0,   252,   253,   254,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
      62,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,     0,
      63,   255,    65,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,     0,     0,    50,    51,     0,
      52,    53,     0,    54,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,    62,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    63,    64,    65,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   251,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     205,     0,     0,   115,    51,     0,    52,    53,     0,   252,
     253,   254,    55,     0,    56,    57,    58,    59,    60,    61,
       0,     0,    62,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    63,   255,    65,    15,     0,   104,   105,    18,
      19,     0,     0,     0,     0,     0,   106,   107,   108,    23,
      24,    25,    26,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,   251,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   205,     0,     0,   115,
      51,     0,    52,    53,     0,   666,   253,   254,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,    62,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    63,   255,
      65,    15,     0,   104,   105,    18,    19,     0,     0,     0,
       0,     0,   106,   107,   108,    23,    24,    25,    26,     0,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,   251,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   205,     0,     0,   115,    51,     0,    52,    53,
       0,   252,   253,     0,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,    62,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    63,   255,    65,    15,     0,   104,
     105,    18,    19,     0,     0,     0,     0,     0,   106,   107,
     108,    23,    24,    25,    26,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   251,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   205,     0,
       0,   115,    51,     0,    52,    53,     0,     0,   253,   254,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
      62,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      63,   255,    65,    15,     0,   104,   105,    18,    19,     0,
       0,     0,     0,     0,   106,   107,   108,    23,    24,    25,
      26,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   251,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   205,     0,     0,   115,    51,     0,
      52,    53,     0,   666,   253,     0,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,    62,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    63,   255,    65,    15,
       0,   104,   105,    18,    19,     0,     0,     0,     0,     0,
     106,   107,   108,    23,    24,    25,    26,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   251,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     205,     0,     0,   115,    51,     0,    52,    53,     0,     0,
     253,     0,    55,     0,    56,    57,    58,    59,    60,    61,
       0,     0,    62,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    63,   255,    65,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   205,     0,     0,   115,
      51,     0,    52,    53,     0,   551,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,    62,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    63,   255,
      65,    15,     0,   104,   105,    18,    19,     0,     0,     0,
       0,     0,   106,   107,   108,    23,    24,    25,    26,     0,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   205,     0,     0,   115,    51,     0,    52,    53,
       0,   252,     0,     0,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,    62,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    63,   255,    65,    15,     0,   104,
     105,    18,    19,     0,     0,     0,     0,     0,   106,   107,
     108,    23,    24,    25,    26,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   205,     0,
       0,   115,    51,     0,    52,    53,     0,   551,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
      62,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      63,   255,    65,    15,     0,   104,   105,    18,    19,     0,
       0,     0,     0,     0,   106,   107,   108,    23,    24,    25,
      26,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   205,     0,     0,   115,    51,     0,
      52,    53,     0,   831,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,    62,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    63,   255,    65,    15,
       0,   104,   105,    18,    19,     0,     0,     0,     0,     0,
     106,   107,   108,    23,    24,    25,    26,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     205,     0,     0,   115,    51,     0,    52,    53,     0,   666,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
       0,     0,    62,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    63,   255,    65,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   205,     0,     0,   115,
      51,     0,    52,    53,     0,     0,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,    62,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    63,    64,
      65,    15,     0,   104,   105,    18,    19,     0,     0,     0,
       0,     0,   106,   107,   108,    23,    24,    25,    26,     0,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   205,     0,     0,   115,    51,     0,    52,    53,
       0,     0,     0,     0,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,    62,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    63,   255,    65,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   205,     0,
       0,   115,    51,     0,    52,    53,     0,     0,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
      62,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      63,   255,    65,    15,     0,   104,   105,    18,    19,     0,
       0,     0,     0,     0,   106,   107,   108,    23,    24,    25,
      26,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   110,    35,    36,    37,   111,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   113,     0,     0,   114,     0,     0,   115,    51,     0,
      52,    53,     0,     0,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,    62,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   116,   104,   105,    18,
      19,     0,     0,     0,     0,     0,   106,   107,   108,    23,
      24,    25,    26,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   216,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   217,     0,     0,    50,
      51,     0,    52,    53,     0,    54,     0,     0,    55,     0,
      56,    57,    58,    59,    60,    61,     0,     0,    62,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,   116,   104,
     105,    18,    19,     0,     0,     0,     0,     0,   106,   107,
     108,    23,    24,    25,    26,     0,     0,   109,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   112,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   302,     0,
       0,   349,    51,     0,    52,    53,     0,   350,     0,     0,
      55,     0,    56,    57,    58,    59,    60,    61,     0,     0,
      62,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
     116,   104,   105,    18,    19,     0,     0,     0,     0,     0,
     106,   107,   108,    23,    24,    25,    26,     0,     0,   109,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   110,    35,    36,    37,   111,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,   112,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     114,     0,     0,   115,    51,     0,    52,    53,     0,     0,
       0,     0,    55,     0,    56,    57,    58,    59,    60,    61,
       0,     0,    62,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,     0,     0,
       0,    15,   116,   104,   105,    18,    19,     0,     0,     0,
       0,     0,   106,   107,   108,    23,    24,    25,    26,     0,
       0,   109,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,   112,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   302,     0,     0,   349,    51,     0,    52,    53,
       0,     0,     0,     0,    55,     0,    56,    57,    58,    59,
      60,    61,     0,     0,    62,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,     0,    15,   116,   104,   105,    18,    19,     0,
       0,     0,     0,     0,   106,   107,   108,    23,    24,    25,
      26,     0,     0,   109,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   112,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   904,     0,     0,   115,    51,     0,
      52,    53,     0,     0,     0,     0,    55,     0,    56,    57,
      58,    59,    60,    61,     0,     0,    62,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   116,   104,   105,    18,
      19,     0,     0,     0,     0,     0,   106,   107,   108,    23,
      24,    25,    26,     0,     0,   109,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   216,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   931,     0,     0,   115,
      51,     0,    52,    53,     0,   595,   596,     0,    55,   597,
      56,    57,    58,    59,    60,    61,     0,     0,    62,     0,
       0,     0,     0,     0,   168,   169,   170,   171,   172,   173,
     174,   175,   176,     0,     0,   177,   178,     0,   116,   179,
     180,   181,   182,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,   184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   185,   186,   187,   188,
     189,   190,   191,   192,   193,   194,     0,   195,   196,   603,
     604,     0,     0,   605,     0,   197,   266,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   168,   169,
     170,   171,   172,   173,   174,   175,   176,     0,     0,   177,
     178,     0,     0,   179,   180,   181,   182,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,   184,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
       0,   195,   196,   624,   596,     0,     0,   625,     0,   197,
     266,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   168,   169,   170,   171,   172,   173,   174,   175,
     176,     0,     0,   177,   178,     0,     0,   179,   180,   181,
     182,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,   184,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,     0,   195,   196,   609,   604,     0,
       0,   610,     0,   197,   266,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   168,   169,   170,   171,
     172,   173,   174,   175,   176,     0,     0,   177,   178,     0,
       0,   179,   180,   181,   182,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,   184,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   194,     0,   195,
     196,   640,   596,     0,     0,   641,     0,   197,   266,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,   169,   170,   171,   172,   173,   174,   175,   176,     0,
       0,   177,   178,     0,     0,   179,   180,   181,   182,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   185,   186,   187,   188,   189,   190,   191,   192,
     193,   194,     0,   195,   196,   643,   604,     0,     0,   644,
       0,   197,   266,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   168,   169,   170,   171,   172,   173,
     174,   175,   176,     0,     0,   177,   178,     0,     0,   179,
     180,   181,   182,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,   184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   185,   186,   187,   188,
     189,   190,   191,   192,   193,   194,     0,   195,   196,   650,
     596,     0,     0,   651,     0,   197,   266,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   168,   169,
     170,   171,   172,   173,   174,   175,   176,     0,     0,   177,
     178,     0,     0,   179,   180,   181,   182,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,   184,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
       0,   195,   196,   653,   604,     0,     0,   654,     0,   197,
     266,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   168,   169,   170,   171,   172,   173,   174,   175,
     176,     0,     0,   177,   178,     0,     0,   179,   180,   181,
     182,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,   184,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,     0,   195,   196,   689,   596,     0,
       0,   690,     0,   197,   266,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   168,   169,   170,   171,
     172,   173,   174,   175,   176,     0,     0,   177,   178,     0,
       0,   179,   180,   181,   182,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,   184,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   194,     0,   195,
     196,   692,   604,     0,     0,   693,     0,   197,   266,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,   169,   170,   171,   172,   173,   174,   175,   176,     0,
       0,   177,   178,     0,     0,   179,   180,   181,   182,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   185,   186,   187,   188,   189,   190,   191,   192,
     193,   194,     0,   195,   196,   836,   596,     0,     0,   837,
       0,   197,   266,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   168,   169,   170,   171,   172,   173,
     174,   175,   176,     0,     0,   177,   178,     0,     0,   179,
     180,   181,   182,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,   184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   185,   186,   187,   188,
     189,   190,   191,   192,   193,   194,     0,   195,   196,   839,
     604,     0,     0,   840,     0,   197,   266,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   168,   169,
     170,   171,   172,   173,   174,   175,   176,     0,     0,   177,
     178,     0,     0,   179,   180,   181,   182,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,   184,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
       0,   195,   196,   994,   596,     0,     0,   995,     0,   197,
     266,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   168,   169,   170,   171,   172,   173,   174,   175,
     176,     0,     0,   177,   178,     0,     0,   179,   180,   181,
     182,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,   184,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   185,   186,   187,   188,   189,   190,
     191,   192,   193,   194,     0,   195,   196,  1007,   596,     0,
       0,  1008,     0,   197,   266,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   168,   169,   170,   171,
     172,   173,   174,   175,   176,     0,     0,   177,   178,     0,
       0,   179,   180,   181,   182,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   183,   184,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   185,   186,
     187,   188,   189,   190,   191,   192,   193,   194,     0,   195,
     196,  1010,   604,     0,     0,  1011,     0,   197,   266,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     168,   169,   170,   171,   172,   173,   174,   175,   176,     0,
       0,   177,   178,     0,     0,   179,   180,   181,   182,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   183,
     184,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   185,   186,   187,   188,   189,   190,   191,   192,
     193,   194,     0,   195,   196,   609,   604,     0,     0,   610,
       0,   197,   266,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   168,   169,   170,   171,   172,   173,
     174,   175,   176,     0,     0,   177,   178,     0,     0,   179,
     180,   181,   182,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   183,   184,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   732,     0,
       0,     0,     0,     0,     0,     0,   185,   186,   187,   188,
     189,   190,   191,   192,   193,   194,     0,   195,   196,     0,
       0,     0,     0,     0,     0,   197,   353,   354,   355,   356,
     357,   358,   359,   360,   361,   362,   363,   364,   365,     0,
       0,   366,   367,   353,   354,   355,   356,   357,   358,   359,
     360,   361,   362,   363,   364,   365,     0,     0,   366,   367,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   368,     0,   369,   370,   371,   372,
     373,   374,   375,   376,   377,   378,     0,     0,     0,     0,
       0,   368,     0,   369,   370,   371,   372,   373,   374,   375,
     376,   377,   378,   353,   354,   355,   356,   357,   358,   359,
     360,   361,   362,   363,   364,   365,     0,   242,   366,   367,
       0,     0,   353,   354,   355,   356,   357,   358,   359,   360,
     361,   362,   363,   364,   365,     0,     0,   366,   367,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   368,     0,   369,   370,   371,   372,   373,   374,   375,
     376,   377,   378,     0,     0,     0,     0,     0,     0,     0,
     368,  -260,   369,   370,   371,   372,   373,   374,   375,   376,
     377,   378,     0,     0,     0,     0,     0,     0,     0,     0,
    -261,   353,   354,   355,   356,   357,   358,   359,   360,   361,
     362,   363,   364,   365,     0,     0,   366,   367,     0,     0,
     353,   354,   355,   356,   357,   358,   359,   360,   361,   362,
     363,   364,   365,     0,     0,   366,   367,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   368,
       0,   369,   370,   371,   372,   373,   374,   375,   376,   377,
     378,     0,     0,     0,     0,     0,     0,     0,   368,  -262,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
       0,     0,     0,     0,     0,     0,     0,     0,  -263,   353,
     354,   355,   356,   357,   358,   359,   360,   361,   362,   363,
     364,   365,     0,     0,   366,   367,     0,     0,     0,   447,
     353,   354,   355,   356,   357,   358,   359,   360,   361,   362,
     363,   364,   365,     0,     0,   366,   367,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   368,     0,   369,
     370,   371,   372,   373,   374,   375,   376,   377,   378,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   368,     0,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
     353,   354,   355,   356,   357,   358,   359,   360,   361,   362,
     363,  -595,  -595,     0,     0,   366,   367,   353,   354,   355,
     356,   357,   358,   359,   360,     0,   362,   363,     0,     0,
       0,     0,   366,   367,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
       0,     0,     0,     0,     0,     0,     0,   369,   370,   371,
     372,   373,   374,   375,   376,   377,   378,   353,   354,   355,
     356,   357,   358,   359,     0,     0,   362,   363,     0,     0,
       0,     0,   366,   367,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   369,   370,   371,
     372,   373,   374,   375,   376,   377,   378
};

static const yytype_int16 yycheck[] =
{
       2,   308,   308,    83,    84,    27,   436,   213,    14,    78,
       2,    10,     4,     5,     6,    27,    15,     9,    10,    21,
      85,    13,    28,    15,    16,    17,     7,   262,    20,   381,
       2,     7,     4,    28,    67,    22,     4,   303,    14,    54,
     379,    15,   296,   454,   383,   427,   300,   386,    64,   352,
      52,    53,    28,   405,    50,    16,    17,   721,    50,    20,
     450,   492,    54,   320,   454,   442,   114,   406,   636,   421,
      16,    17,    64,   484,    20,    56,   515,   645,   430,   795,
      56,   420,   709,   422,   912,    26,    78,    57,   889,   391,
     392,    52,   431,    67,     5,     6,    21,    22,    26,    16,
     612,   613,    13,   142,    25,   107,    29,   146,    16,    17,
      89,    91,    20,    25,   101,   285,    58,    59,    60,    61,
      25,   113,    89,   115,   103,   321,   557,    57,   324,   209,
     326,   470,   328,    89,   330,    25,    89,   289,    25,   119,
     220,   293,   494,    54,    52,    53,    25,    16,    17,   119,
      51,    20,    91,    27,    55,    51,   495,    53,    54,    55,
      56,    89,   140,    26,     0,    25,   145,    78,   146,   997,
      16,    17,   136,    69,    20,   103,   101,    91,   145,   349,
     119,    25,   107,   108,    55,   442,   987,   706,   111,   145,
      89,   121,   145,   407,    28,   136,   113,   352,   123,   116,
     117,   281,   509,   509,   740,   119,    52,    53,   136,   745,
     138,    18,   204,    20,   142,   214,   215,   145,    26,   140,
     140,   142,   214,   215,   459,   305,   492,   144,   140,   146,
     142,   947,    50,    80,   303,   140,   391,   392,   750,   113,
      91,    87,   116,   117,   140,   909,   145,    72,   912,   260,
     140,   262,   233,   140,   287,   119,    91,   233,   260,    89,
     262,   140,   295,   296,   266,   676,   893,   300,   119,   243,
     144,    91,   146,   136,   266,   532,   140,   124,   270,   142,
     140,    89,   274,   275,   295,   675,   676,   279,   665,   285,
     119,   557,   284,   285,    91,   103,   140,   115,   729,   119,
     292,   142,   101,   455,   129,   130,   131,    89,    89,   270,
     462,   303,   284,   287,   882,   145,    89,   119,    89,   142,
     292,   473,   119,   123,   270,    55,   632,   126,   136,    89,
     138,   347,   635,   997,   530,   350,   352,   145,    91,   338,
     339,   340,   341,   140,   119,   337,   338,   339,   340,   341,
     342,   343,   344,   349,    25,   347,   404,   349,   350,    91,
     352,    55,   270,   145,   145,   337,   805,    91,   887,   337,
     342,   313,   145,    16,   145,   391,   392,   113,   897,   381,
     116,   117,   734,   822,   823,   145,   347,   119,    89,   381,
     909,   352,   303,   140,   733,   119,   735,   266,   780,   391,
     392,   270,   103,   405,   556,   662,    20,   383,   665,   623,
     386,   140,   414,   405,    60,   407,   408,    63,    57,   421,
     266,   490,   136,   492,   270,   417,   448,    72,   430,   421,
     406,    91,   686,   425,   669,   737,   448,   138,   430,   350,
     742,   743,   119,   435,   145,   143,   422,   458,   459,    91,
     757,   757,   882,   137,   660,   431,   458,   459,   383,   119,
     466,   279,   108,   729,   139,   467,    16,   285,    58,    59,
     113,   466,   883,   116,   117,   467,    55,   119,   439,   631,
     140,   406,  1001,   457,   476,    37,    38,   567,   557,    91,
     466,    89,   494,   883,   470,   467,  1015,   422,   490,    91,
     492,   144,   494,   146,   476,   103,   431,     2,   722,     4,
     140,   513,   381,   515,     9,    10,    72,   119,    72,   495,
      15,    16,    17,    91,   963,    20,    98,   119,    91,   786,
      91,   349,   140,   685,   748,   381,   405,    15,   140,   531,
     138,    13,   450,   554,   758,   470,   848,   145,   550,    16,
     608,   119,   421,   611,   789,    50,   119,    15,   119,   405,
     866,   430,   584,   113,    63,   557,   116,   117,   143,    64,
     495,   629,   584,   481,   143,   421,   731,   140,    26,   490,
     137,   492,   737,    89,   430,   140,   800,   742,   743,   113,
     140,   140,   116,   117,   144,    16,   146,   103,    51,   417,
      53,    54,    55,    56,   450,    51,   608,   425,   454,   611,
     612,   613,   140,   600,    44,    89,    69,   435,   113,   140,
     115,   608,   146,   622,   611,   494,   978,   629,    89,   103,
     622,   623,   138,   635,   636,   481,   638,   140,   484,   145,
     979,    89,   103,   645,   796,   859,   557,    51,   494,   660,
     656,   140,   633,   686,    91,   103,   655,   633,   669,    51,
     729,   656,   868,   655,   138,    58,    59,   669,   874,    16,
      17,   145,   140,    20,   635,   600,    62,   138,    64,    65,
     656,   746,   119,   608,   145,   697,   611,   140,   136,   139,
     138,   119,   113,   848,   142,   116,   117,   145,    89,   139,
      47,    48,   627,   140,   629,    52,    53,    17,    18,   204,
     862,   863,   103,   531,    15,   731,   930,    64,    65,   214,
     215,   737,   738,   144,    18,   146,   742,   743,   114,   115,
     722,    51,   734,    53,    54,    55,    56,   729,   730,   731,
     818,   819,   734,    72,   139,   737,   738,   138,   750,    69,
     742,   743,   139,   137,   145,    15,   748,   749,   730,   735,
     137,   763,    26,    91,   766,   139,   758,   675,   146,   768,
     524,   266,   526,   765,   137,   270,   768,    15,   789,   274,
     275,    92,    37,    38,   279,   777,   778,   789,    14,   284,
     285,   119,   113,   785,    15,   116,   117,   292,   127,   128,
     129,   130,   131,   805,   391,   392,   818,   819,   800,   801,
     735,    62,   140,    64,    65,    15,   140,   143,   729,    51,
     822,   823,   144,    57,   140,    89,   140,   140,   820,   675,
     676,   140,   848,   825,   140,   140,   423,   424,   140,   103,
      72,   140,   337,   338,   339,   340,   341,   342,   343,   344,
      15,   139,   347,   933,   349,    15,   848,   352,   113,   137,
      15,   116,   117,   114,   115,   734,   858,   859,   100,   101,
      15,    15,   136,   140,   138,   867,     9,    10,   142,   871,
     882,   145,    15,    15,   471,    89,   381,    89,   734,   144,
     137,   146,    55,   124,   126,   124,   391,   392,   137,   103,
      61,   103,    89,    64,    65,   252,   253,   254,   255,   820,
     405,    15,   407,   408,   825,    89,   103,    55,   140,   266,
     140,    61,   417,   270,    64,    65,   421,   949,    15,   103,
     425,   749,   140,   140,   138,   430,   138,   949,   930,   140,
     435,   145,   706,   145,   936,   709,   938,   765,    89,   941,
      15,   138,   140,   114,   115,   142,   867,   721,   145,   777,
     778,   963,   103,   140,   138,   113,   142,   785,   116,   117,
     142,   145,   467,   139,   114,   115,   978,   467,   980,   981,
     113,   476,   140,   801,    13,    26,   978,     6,   981,   987,
      63,    64,    65,   721,   980,   207,   144,   138,   146,   494,
     347,   213,   747,   979,   145,   352,   353,   354,   355,   356,
     357,   358,   359,   360,   361,   362,   363,   364,   365,   366,
     367,   368,   369,   370,   371,   372,   373,   374,   375,   376,
     377,   378,   245,     7,   381,   721,   531,   883,   250,   866,
     858,   114,   115,   700,   391,   392,   261,   906,    89,    26,
      89,   909,   706,   871,   979,   809,   810,   811,   405,   813,
      -1,   815,   103,    -1,   103,    89,    -1,     2,    -1,     4,
       5,     6,   419,    -1,   421,    -1,   423,   424,    13,   103,
      -1,   214,   215,   430,    89,    -1,    -1,    63,    64,    65,
      -1,    -1,   439,    -1,    -1,   136,   443,   138,   103,   138,
     447,   142,    -1,   450,   145,   452,   145,   454,    -1,   978,
      -1,    -1,    89,    -1,   138,    50,    -1,    89,   936,    54,
     938,   145,    -1,   941,   471,   337,   103,   622,   623,   893,
      -1,   103,   978,   138,   481,    -1,    -1,   484,   114,   115,
     145,   274,   275,    78,    -1,   909,    -1,   494,   912,    -1,
     737,   738,    -1,    -1,    -1,   742,   743,    -1,    -1,   136,
     655,   138,    -1,    -1,   511,   142,   138,    -1,   145,    -1,
     696,    -1,    -1,   145,    51,   522,    53,    54,    55,    56,
     115,    -1,    -1,   709,   771,   772,   712,   774,   775,    63,
      64,    65,    69,   540,   541,   721,    -1,    -1,    -1,    -1,
     954,   955,   956,   957,   551,   338,   339,   340,   341,    -1,
     343,   344,    -1,    -1,   426,   427,    93,   700,    -1,    -1,
     906,   704,    99,   909,   700,    -1,   912,   722,   914,    63,
      64,    65,    -1,   997,    -1,   730,   731,    -1,   721,   734,
     114,   115,   737,   738,    -1,   721,    -1,   742,   743,    63,
      64,    65,    -1,   748,   749,    -1,    -1,    -1,    63,    64,
      65,   848,    -1,   758,   476,  1019,    -1,    -1,    -1,   204,
     765,   483,    -1,   768,    -1,   408,    -1,    -1,   964,    -1,
     114,   115,   777,   778,    -1,    -1,    -1,    -1,   635,   876,
     785,    40,    41,    42,    43,    44,    -1,     2,    -1,     4,
     114,   115,    -1,    -1,    -1,   800,   801,    -1,    13,   114,
     115,   997,    -1,   999,    -1,  1001,    -1,  1003,    -1,   666,
      63,    64,    65,    -1,    -1,    -1,    -1,    -1,   675,   676,
      -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,    -1,
      -1,    -1,  1028,    -1,   279,    50,    63,    64,    65,   284,
     285,    69,    -1,   848,    72,    -1,    -1,   292,    -1,    -1,
      -1,    -1,    -1,   858,   859,   827,   828,   893,   303,   895,
      -1,   114,   115,   899,    -1,    93,   871,    -1,    -1,   726,
      -1,    99,   100,   101,   731,   732,   912,   734,   914,    -1,
     737,   738,    -1,    -1,    -1,   742,   743,   114,   115,    -1,
      -1,    -1,   337,    -1,    -1,    -1,    -1,   342,   126,   621,
     115,   129,    -1,    -1,   349,   350,    -1,   352,    -1,   945,
     946,    -1,   140,   906,   771,   772,   909,   774,   775,   912,
     906,   914,    -1,   909,    -1,   930,   912,   784,   914,    -1,
      -1,   936,    -1,   938,    -1,    -1,   941,   909,   660,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   391,   392,    -1,   985,
      -1,    -1,    -1,     2,   990,     4,     5,     6,     7,    -1,
      -1,   997,   407,   999,    13,    -1,    -1,  1003,    -1,    -1,
      -1,   964,   417,   978,   831,    -1,    -1,    -1,   964,   622,
     425,  1017,    -1,    -1,   841,   842,    -1,    -1,    -1,   204,
     435,   848,  1028,    -1,    -1,   967,   968,   969,    -1,   971,
     972,    50,    -1,    -1,   997,    54,   999,    -1,  1001,    -1,
    1003,   997,   655,   999,    -1,  1001,    -1,  1003,    -1,   876,
      -1,    -1,   467,    -1,    72,    -1,   883,    -1,    -1,    78,
     752,   476,    -1,    -1,    -1,  1028,    -1,   759,    -1,    87,
      88,    72,  1028,    -1,     2,   490,     4,   492,  1020,  1021,
    1022,  1023,    72,    -1,    -1,    -1,    87,    88,   780,    -1,
    1032,    -1,    -1,    -1,   279,    -1,   115,    87,    88,   284,
     285,    51,    -1,    53,    54,    55,    56,   292,   126,   127,
     128,   129,   130,   131,   113,    -1,   531,   116,   117,    69,
      -1,    -1,    50,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,    -1,    -1,    -1,    -1,    -1,   127,   128,   129,
     130,   131,   557,    93,   143,   144,    -1,   146,    -1,    99,
     113,   978,   337,   116,   117,   768,    51,   342,    53,    54,
      55,    56,    -1,    -1,   349,    -1,    -1,   352,    51,    -1,
      53,    54,    55,    56,    69,    -1,   868,   140,    -1,    -1,
      -1,   144,   874,   146,    -1,   204,    69,   115,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    93,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   391,   392,   623,    -1,
      93,    -1,    -1,    -1,    -1,    -1,    99,   100,   101,    -1,
      -1,    51,   407,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,   417,    -1,    -1,    -1,    -1,    -1,    -1,    69,
     425,    -1,    72,   126,    -1,    -1,   129,    -1,    -1,    -1,
     435,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
     279,    -1,    -1,    93,    -1,   284,   285,    -1,    -1,    99,
     100,   101,    -1,   292,    -1,    -1,   204,    -1,    -1,    -1,
      -1,    -1,   467,    -1,   303,    -1,    -1,    -1,    -1,    -1,
      51,   476,    53,    54,    55,    56,   126,    -1,    -1,   129,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   722,    69,    -1,
      -1,    72,   142,    -1,   729,   730,   731,    -1,   337,    -1,
      -1,    -1,   737,   342,    -1,    -1,    -1,   742,   743,    -1,
     349,   350,    93,   748,   749,    -1,    -1,    -1,    99,   100,
     101,    -1,    -1,   758,    -1,    -1,   531,    -1,    -1,    -1,
     765,   279,    -1,    -1,    -1,    -1,   284,   285,    -1,    -1,
      -1,    -1,   777,   778,   292,   126,    -1,    -1,   129,    -1,
     785,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   800,   801,    -1,   407,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,   417,    53,
      54,    55,    56,    -1,    -1,   820,   425,    -1,    -1,   337,
     825,    -1,    -1,    -1,   342,    69,   435,    -1,    72,    -1,
      -1,   349,    -1,    -1,   352,    -1,    -1,    -1,    -1,    -1,
      84,    -1,    -1,   848,    -1,    -1,    -1,    -1,   623,    93,
      -1,    -1,    -1,   858,   859,    99,   100,   101,   467,    -1,
      -1,    -1,   867,    -1,    -1,    -1,   871,   476,    -1,    -1,
      -1,    -1,    -1,   391,   392,    -1,    -1,    -1,    -1,    -1,
      -1,   490,   126,   492,    -1,   129,    -1,    -1,    51,   407,
      53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,   417,
      -1,    -1,    -1,    -1,    -1,    -1,    69,   425,    -1,    72,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,
      -1,    -1,   531,    -1,    -1,   930,    -1,    -1,    -1,    -1,
      93,   936,    -1,   938,    -1,    -1,   941,   100,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   722,   557,   467,
      -1,    -1,    -1,    -1,    -1,   730,   731,    -1,   476,    -1,
      -1,    -1,   737,   126,    -1,    -1,    -1,   742,   743,    -1,
      -1,    -1,    -1,   748,   749,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   758,    -1,    -1,    -1,    -1,    -1,    -1,
     765,     0,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   777,   778,    13,    14,    15,    16,    17,    18,
     785,    20,    -1,   531,   623,    -1,    -1,    26,    27,    -1,
      -1,    -1,    -1,    -1,    -1,   800,   801,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    -1,    -1,   858,   859,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   867,    -1,   103,    -1,   871,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,   623,    -1,   116,   117,    -1,
      -1,    -1,    -1,   722,    -1,    -1,    -1,    -1,    -1,    -1,
     729,   730,    -1,    -1,    -1,    -1,    -1,   136,   137,    -1,
      -1,    -1,    -1,   142,   143,   144,   145,   146,    -1,   748,
     749,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   758,
      -1,    -1,    -1,    -1,    -1,   930,   765,    -1,    -1,    -1,
      -1,   936,    -1,   938,    -1,    -1,   941,    -1,   777,   778,
      -1,    -1,    -1,    -1,    -1,    -1,   785,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   800,   801,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   722,    -1,    -1,    -1,    -1,    -1,
      -1,   820,   730,   731,    -1,    -1,   825,    -1,    -1,   737,
      -1,    -1,    -1,    -1,   742,   743,    -1,    -1,    -1,    -1,
     748,   749,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     758,    -1,    -1,    -1,    -1,    -1,    -1,   765,    -1,   858,
     859,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   867,   777,
     778,    -1,   871,    -1,    -1,    -1,    -1,   785,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    -1,   800,   801,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    87,
      88,   930,    -1,    -1,    -1,    -1,    -1,   936,    -1,   938,
     848,    -1,   941,    72,    73,    74,    75,    76,    77,    78,
     858,   859,    81,    82,    -1,    -1,    -1,    -1,    87,    88,
      -1,    -1,   120,   871,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   140,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   930,    -1,    -1,    -1,    -1,    -1,   936,    -1,
     938,     0,     1,   941,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      99,    -1,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,     0,    -1,   112,   113,    -1,    -1,   116,   117,    -1,
      -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,   132,   133,   134,    -1,    -1,    27,    28,
      -1,    -1,    -1,    -1,    -1,   144,    -1,   146,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    87,    88,
      89,    -1,    91,    92,    -1,    -1,    -1,    -1,    -1,    98,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,    -1,   116,   117,    -1,
     119,   120,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,    -1,     0,    -1,    -1,   137,   138,
     139,   140,    -1,    -1,   143,   144,   145,   146,    13,    14,
      15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,
      -1,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      -1,    -1,    87,    88,    89,    -1,    -1,    92,    -1,    -1,
      -1,    -1,    -1,    98,    -1,    -1,    -1,    -1,   103,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,
      -1,   116,   117,    -1,    -1,   120,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    -1,    -1,     0,
      -1,    -1,   137,   138,   139,   140,    -1,   142,   143,   144,
     145,   146,    13,    14,    15,    -1,    17,    18,    -1,    20,
      -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    87,    88,    89,    -1,
      91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   113,    -1,    -1,   116,   117,    -1,   119,   120,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,    -1,     0,    -1,   136,   137,   138,    -1,   140,
      -1,    -1,   143,   144,   145,   146,    13,    14,    15,    -1,
      17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    26,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    -1,    -1,
      87,    88,    89,    -1,    91,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,    -1,   116,
     117,    -1,   119,   120,    -1,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,    -1,    -1,     0,    -1,   136,
     137,   138,    -1,   140,    -1,    -1,   143,   144,   145,   146,
      13,    14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,
      43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    87,    88,    89,    -1,    91,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     113,    -1,    -1,   116,   117,    -1,   119,   120,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
      -1,     0,    -1,    -1,   137,   138,    -1,   140,    -1,    -1,
     143,   144,   145,   146,    13,    14,    15,    -1,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    87,    88,
      89,    -1,    91,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   103,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   113,    -1,    -1,   116,   117,    -1,
     119,   120,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,    -1,    -1,    -1,    -1,   137,   138,
      -1,   140,    -1,    -1,   143,   144,   145,   146,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,    15,    -1,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,
      93,    94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,
      -1,   104,   105,   106,   107,   108,   109,    -1,    -1,   112,
     113,    -1,    -1,   116,   117,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,
     133,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   144,     1,   146,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    15,    -1,    17,    18,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,
      99,    -1,    -1,   102,    -1,   104,   105,   106,   107,   108,
     109,    -1,    -1,   112,   113,    -1,    -1,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,     1,   146,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    -1,    -1,    18,    19,    20,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,    -1,    -1,   112,   113,    -1,
      -1,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,   133,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   144,
       1,   146,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    15,    -1,    -1,    18,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,   113,    -1,    -1,   116,   117,     1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,   132,   133,   134,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,   144,    -1,   146,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,   113,
      -1,    -1,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,   133,
     134,    -1,    -1,   137,    -1,    -1,    -1,    -1,    -1,    -1,
     144,     1,   146,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    14,    15,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
      -1,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,   113,    -1,    -1,   116,   117,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,   132,   133,   134,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,   144,    -1,   146,    30,    31,    32,
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
     143,   144,    -1,   146,    30,    31,    32,    33,    34,    35,
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
     109,    -1,    -1,   112,   113,    -1,    -1,   116,   117,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   132,   133,   134,    -1,    -1,   137,    -1,
      -1,    -1,    -1,    -1,    -1,   144,     1,   146,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,
      -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,   104,
     105,   106,   107,   108,   109,    -1,    -1,   112,   113,    -1,
      -1,   116,   117,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,   132,   133,   134,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,   144,
      -1,   146,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
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
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,
      -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,
      -1,   102,    -1,   104,   105,   106,   107,   108,   109,    -1,
      -1,   112,   113,    -1,    -1,   116,   117,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,   132,   133,   134,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,   144,    -1,   146,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    -1,    -1,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,   113,
      -1,    -1,   116,   117,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   132,   133,
     134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     144,    -1,   146,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,   142,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
     100,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,   133,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,   142,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
     100,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   132,   133,   134,    -1,   136,    -1,    -1,    -1,
      -1,    -1,   142,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
     100,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   132,   133,   134,    19,   136,    21,    22,    23,
      24,    -1,   142,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    91,    -1,    93,
      94,    -1,    96,    97,    -1,    99,   100,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    -1,   119,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,   132,   133,
     134,    -1,    19,    -1,    21,    22,    23,    24,   142,    -1,
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
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
     100,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   132,   133,   134,    19,    -1,    21,    22,    23,
      24,    -1,   142,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,   100,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,   133,
     134,    19,    -1,    21,    22,    23,    24,    -1,   142,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    99,   100,   101,   102,    -1,   104,   105,   106,   107,
     108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   132,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     108,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,
      -1,    -1,    -1,   141,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   108,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,
      -1,    -1,   141,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    -1,    -1,    55,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,    -1,    -1,    -1,   104,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,
      -1,   141,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    -1,    -1,    55,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,    -1,   133,   134,    -1,    -1,    -1,    -1,    -1,    -1,
     141,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    -1,    -1,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,    -1,    -1,
     112,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     132,    21,    22,    23,    24,    -1,    -1,    -1,   140,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    -1,
      -1,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   132,    21,    22,    23,    24,    -1,    -1,    -1,
     140,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    99,    -1,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   132,   133,   134,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,   100,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,    -1,    -1,
     112,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    -1,
     132,   133,   134,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      -1,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    99,    -1,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   132,   133,   134,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
     100,   101,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   132,   133,   134,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,   100,   101,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,   133,
     134,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    99,   100,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   132,   133,   134,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    -1,   100,   101,
     102,    -1,   104,   105,   106,   107,   108,   109,    -1,    -1,
     112,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     132,   133,   134,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    99,   100,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   132,   133,   134,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    -1,
     100,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   132,   133,   134,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,   133,
     134,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    99,    -1,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   132,   133,   134,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,    -1,    -1,
     112,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     132,   133,   134,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    99,    -1,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,    -1,
      -1,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,   132,   133,   134,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    99,
      -1,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,   132,   133,   134,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    -1,    -1,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,   132,   133,
     134,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    -1,    -1,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,    -1,    -1,   112,    -1,    -1,    -1,    -1,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,   132,   133,   134,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    -1,    -1,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,    -1,    -1,
     112,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
     132,   133,   134,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    87,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    -1,    -1,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,   132,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    99,    -1,    -1,   102,    -1,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   132,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,
      -1,    93,    94,    -1,    96,    97,    -1,    99,    -1,    -1,
     102,    -1,   104,   105,   106,   107,   108,   109,    -1,    -1,
     112,    -1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
     132,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,    93,    94,    -1,    96,    97,    -1,    -1,
      -1,    -1,   102,    -1,   104,   105,   106,   107,   108,   109,
      -1,    -1,   112,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    19,   132,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    90,    -1,    -1,    93,    94,    -1,    96,    97,
      -1,    -1,    -1,    -1,   102,    -1,   104,   105,   106,   107,
     108,   109,    -1,    -1,   112,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,   132,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    90,    -1,    -1,    93,    94,    -1,
      96,    97,    -1,    -1,    -1,    -1,   102,    -1,   104,   105,
     106,   107,   108,   109,    -1,    -1,   112,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    19,   132,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    90,    -1,    -1,    93,
      94,    -1,    96,    97,    -1,    51,    52,    -1,   102,    55,
     104,   105,   106,   107,   108,   109,    -1,    -1,   112,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,   132,    85,
      86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,   100,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,   133,   134,    51,
      52,    -1,    -1,    55,    -1,   141,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,   133,   134,    51,    52,    -1,    -1,    55,    -1,   141,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,   133,   134,    51,    52,    -1,
      -1,    55,    -1,   141,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,   133,
     134,    51,    52,    -1,    -1,    55,    -1,   141,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,   133,   134,    51,    52,    -1,    -1,    55,
      -1,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,   100,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,   133,   134,    51,
      52,    -1,    -1,    55,    -1,   141,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,   133,   134,    51,    52,    -1,    -1,    55,    -1,   141,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,   133,   134,    51,    52,    -1,
      -1,    55,    -1,   141,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,   133,
     134,    51,    52,    -1,    -1,    55,    -1,   141,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,   133,   134,    51,    52,    -1,    -1,    55,
      -1,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,   100,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,   133,   134,    51,
      52,    -1,    -1,    55,    -1,   141,   142,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    -1,    -1,    81,
      82,    -1,    -1,    85,    86,    87,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,   100,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,   133,   134,    51,    52,    -1,    -1,    55,    -1,   141,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    85,    86,    87,
      88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,   123,   124,   125,   126,   127,
     128,   129,   130,   131,    -1,   133,   134,    51,    52,    -1,
      -1,    55,    -1,   141,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    78,    -1,    -1,    81,    82,    -1,
      -1,    85,    86,    87,    88,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    99,   100,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,
     124,   125,   126,   127,   128,   129,   130,   131,    -1,   133,
     134,    51,    52,    -1,    -1,    55,    -1,   141,   142,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,    -1,
      -1,    81,    82,    -1,    -1,    85,    86,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    99,
     100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,    -1,   133,   134,    51,    52,    -1,    -1,    55,
      -1,   141,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    85,
      86,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    99,   100,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,   133,   134,    -1,
      -1,    -1,    -1,    -1,    -1,   141,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    87,    88,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   120,    -1,   122,   123,   124,   125,
     126,   127,   128,   129,   130,   131,    -1,    -1,    -1,    -1,
      -1,   120,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,   146,    87,    88,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    87,    88,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   120,    -1,   122,   123,   124,   125,   126,   127,   128,
     129,   130,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,   140,   122,   123,   124,   125,   126,   127,   128,   129,
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
      83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,    92,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   120,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    87,    88,    72,    73,    74,
      75,    76,    77,    78,    79,    -1,    81,    82,    -1,    -1,
      -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,
      -1,    -1,    87,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131
};

  /* YYSTOSSTATE-NUM -- The (internal number of the) accessing
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
     159,   161,   163,   164,   167,   168,   170,   171,   172,   174,
     175,   184,   198,   219,   240,   241,   251,   252,   253,   257,
     258,   259,   265,   266,   267,   269,   270,   271,   272,   273,
     274,   309,   322,   152,    21,    22,    30,    31,    32,    39,
      51,    55,    69,    87,    90,    93,   132,   176,   177,   198,
     219,   271,   274,   309,   177,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    45,
      46,    47,    48,    49,    50,    51,    52,    55,    70,    71,
      72,    73,    74,    75,    76,    77,    78,    81,    82,    85,
      86,    87,    88,    99,   100,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   133,   134,   141,   142,   178,
     182,   183,   273,   303,   199,    90,   161,   162,   175,   219,
     271,   272,   274,   162,   205,   207,    69,    90,   168,   175,
     219,   224,   271,   274,    33,    34,    35,    36,    48,    49,
      50,    51,    55,   104,   178,   179,   180,   267,   113,   116,
     117,   144,   146,   162,   261,   262,   263,   315,   319,   320,
     321,    51,    99,   100,   101,   133,   167,   184,   190,   193,
     196,   253,   306,   308,   190,   190,   142,   187,   188,   191,
     192,   322,   187,   191,   142,   316,   320,   179,   153,   136,
     184,   219,   184,    55,     1,    93,   155,   156,   157,   169,
     170,   322,   200,   202,   185,   196,   306,   322,   184,   305,
     306,   322,    90,   140,   174,   219,   271,   274,   203,    53,
      54,    56,    63,   108,   178,   268,    62,    64,    65,   114,
     115,   254,   255,    63,   254,    63,   254,    63,   254,    61,
     254,    58,    59,   163,   184,   184,   315,   321,    40,    41,
      42,    43,    44,    37,    38,    28,   238,   119,   140,    93,
      99,   171,   119,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    87,    88,   120,   122,
     123,   124,   125,   126,   127,   128,   129,   130,   131,    89,
     103,   138,   145,   313,    89,   313,   314,    26,   136,   242,
     253,    91,    91,   187,   191,   242,   161,    51,    55,   176,
      58,    59,   123,   275,    89,   138,   313,   214,   304,   215,
      89,   145,   312,   154,   155,    55,    16,   220,   319,   119,
      89,   138,   313,    91,    91,   220,   162,   162,    55,    89,
     138,   313,    25,   108,   140,   264,   315,   113,   263,    20,
     245,   319,    57,   307,   184,   184,   184,    92,   140,   194,
     195,   322,   307,   194,   195,    84,   189,   190,   196,   306,
     322,   190,   161,   315,   317,   161,   158,   136,   155,    89,
     313,    91,   157,   169,   143,   315,   321,   317,   157,   317,
     139,   195,   318,   321,   195,   318,   137,   318,    55,   171,
     172,   173,   140,    89,   138,   313,    51,    53,    54,    55,
      56,    69,    72,    93,    99,   100,   101,   126,   129,   142,
     236,   278,   279,   282,   283,   284,   285,   287,   288,   289,
     290,   292,   293,   294,   297,   298,   299,   300,   301,    63,
     254,   256,   260,   261,    62,   255,    63,    63,    63,    61,
      72,    72,   152,   162,   162,   162,   162,   157,   161,   161,
     239,    99,   163,   184,   196,   197,   169,   140,   174,   140,
     159,   160,   163,   175,   184,   186,   197,   219,   274,   184,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   184,    51,    52,    55,   182,   187,
     310,   311,   189,    51,    52,    55,   182,   187,   310,    51,
      55,   310,   244,   243,   160,   184,   186,   160,   186,    98,
     165,   212,   276,   211,    51,    55,   176,   310,   189,   310,
     154,   161,   216,   217,    15,    13,   247,   322,   155,    16,
      51,    55,   189,    51,    55,   155,    27,   221,   319,   221,
      51,    55,   189,    51,    55,   209,   181,   155,   245,   184,
     196,    15,   260,   184,   184,   316,    99,   184,   193,   306,
     184,   308,   317,   143,   315,   195,   195,   317,   143,   179,
     150,   137,   186,   317,   157,   201,   306,   171,   173,    51,
      55,   189,    51,    55,    57,   119,   291,   287,   204,   184,
     140,   302,   322,    51,   140,   302,   140,   286,   184,   140,
     286,    51,   140,   286,    51,    63,   155,   261,   184,   184,
      80,   124,   230,   231,   322,   184,   195,   317,   173,   140,
      44,   119,    44,    89,   138,   313,   316,    91,    91,   187,
     191,   139,    91,    91,   188,   191,   188,   191,   230,   230,
     166,   319,   162,   154,   139,    15,   317,   142,   277,   287,
     178,   184,   197,   248,   322,    18,   223,   322,    17,   222,
     223,    91,    91,   139,    91,    91,   223,   206,   208,   139,
     162,   179,   137,    15,   195,   220,   260,   184,   194,   306,
     137,   317,   318,   139,    51,    99,   225,   292,   233,   316,
      29,   111,   237,    51,   279,   284,   301,   285,   290,   297,
     299,   292,   294,   299,    51,   292,   137,   227,   229,   232,
     278,   280,   281,   284,   292,   293,   295,   296,   299,   301,
     154,    99,   184,   173,   157,   184,    51,    55,   189,    51,
      55,    57,   121,   160,   186,   163,   186,   165,    91,   160,
     186,   160,   186,   165,   242,   238,   154,   155,   230,   213,
     319,    15,    84,   287,   154,   319,   218,    92,   249,   322,
     155,    14,   250,   322,   162,    15,    91,    15,   155,   155,
     221,   184,   155,   195,   140,   289,   317,   140,   143,   144,
     154,   155,   302,   140,   286,   140,   286,   140,   286,   140,
     286,   286,   233,   233,    90,   219,   140,   302,   302,   140,
     228,   219,   140,   228,   140,   228,    15,   184,   139,   184,
     184,   160,   186,    15,   137,   155,   154,   317,   317,    15,
     277,    90,   175,   219,   271,   274,   220,   155,   220,    15,
      15,   210,   223,   245,   246,   226,   140,    99,    51,   234,
     235,   288,    15,   137,   292,   299,   292,   292,   124,   124,
      55,    89,   280,   284,   140,   227,   228,   296,   299,   292,
     295,   299,   292,   137,    15,   154,    55,    89,   138,   313,
     155,   155,   155,   292,   292,   140,   289,   140,   316,   286,
     140,   286,   286,   286,    51,    55,   302,   140,   228,   140,
     228,   140,   228,   140,   228,   228,    15,    51,    55,   189,
      51,    55,   247,   222,    15,   140,   292,   140,   235,   292,
     292,   299,   292,   292,   139,   292,   286,   228,   140,   228,
     228,   228,   292,   228
};

  /* YYR1YYN -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   147,   149,   148,   150,   151,   151,   151,   151,   152,
     153,   152,   154,   155,   156,   156,   156,   156,   158,   157,
     157,   157,   157,   157,   157,   157,   157,   157,   157,   157,
     157,   157,   157,   159,   159,   159,   159,   159,   159,   159,
     159,   160,   160,   160,   161,   161,   161,   161,   161,   161,
     162,   163,   163,   164,   164,   166,   165,   167,   167,   167,
     167,   167,   167,   167,   167,   167,   167,   167,   168,   168,
     169,   169,   170,   170,   170,   170,   170,   170,   170,   170,
     170,   170,   171,   171,   172,   172,   173,   173,   174,   174,
     174,   174,   174,   174,   174,   174,   175,   175,   175,   175,
     175,   175,   175,   175,   175,   176,   176,   177,   177,   177,
     178,   178,   178,   178,   178,   179,   179,   180,   181,   180,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     182,   182,   182,   182,   182,   182,   182,   182,   182,   182,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     183,   183,   183,   183,   183,   183,   183,   183,   183,   183,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   184,   184,   184,   184,   184,   184,   184,
     184,   184,   184,   185,   185,   185,   185,   186,   186,   187,
     187,   188,   188,   189,   189,   189,   189,   189,   190,   190,
     190,   190,   190,   192,   191,   193,   194,   194,   195,   195,
     196,   196,   196,   196,   197,   197,   197,   198,   198,   198,
     198,   198,   198,   198,   198,   198,   199,   198,   200,   201,
     198,   202,   198,   198,   198,   198,   198,   198,   198,   198,
     198,   198,   198,   198,   198,   203,   204,   198,   198,   198,
     205,   206,   198,   207,   208,   198,   198,   198,   209,   210,
     198,   211,   198,   212,   213,   198,   214,   198,   215,   216,
     198,   217,   218,   198,   198,   198,   198,   198,   219,   220,
     220,   220,   221,   221,   222,   222,   223,   223,   224,   224,
     225,   225,   225,   225,   225,   225,   225,   225,   226,   225,
     227,   227,   227,   227,   228,   228,   229,   229,   229,   229,
     229,   229,   229,   229,   229,   229,   229,   229,   229,   229,
     229,   230,   230,   232,   231,   231,   231,   233,   233,   234,
     234,   235,   235,   236,   236,   237,   237,   239,   238,   240,
     240,   240,   240,   241,   241,   241,   241,   241,   241,   241,
     241,   241,   243,   242,   244,   242,   245,   246,   246,   247,
     247,   248,   248,   248,   249,   249,   250,   250,   251,   251,
     251,   251,   252,   252,   253,   253,   253,   253,   254,   254,
     255,   256,   255,   255,   255,   257,   257,   258,   258,   259,
     260,   260,   261,   261,   262,   262,   263,   264,   263,   265,
     265,   266,   266,   267,   268,   268,   268,   268,   268,   268,
     269,   269,   270,   270,   270,   270,   271,   271,   271,   271,
     271,   272,   272,   273,   273,   273,   273,   273,   273,   273,
     273,   274,   274,   275,   276,   275,   277,   277,   277,   278,
     279,   279,   280,   280,   281,   281,   282,   282,   283,   283,
     284,   284,   285,   285,   285,   285,   286,   286,   287,   287,
     287,   287,   287,   287,   287,   287,   287,   287,   287,   287,
     287,   287,   287,   288,   288,   288,   288,   288,   289,   289,
     290,   291,   290,   292,   292,   293,   294,   295,   296,   296,
     297,   297,   298,   298,   299,   299,   300,   300,   301,   302,
     302,   303,   304,   303,   305,   305,   306,   306,   307,   307,
     308,   308,   308,   308,   309,   309,   309,   310,   310,   310,
     310,   311,   311,   311,   312,   312,   313,   313,   314,   314,
     315,   315,   316,   316,   317,   318,   318,   318,   319,   319,
     319,   320,   321,   321,   322
};

  /* YYR2YYN -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     1,     3,     3,     6,     5,     5,     5,     5,
       3,     1,     3,     1,     1,     3,     3,     3,     2,     1,
       1,     1,     1,     1,     4,     0,     5,     2,     3,     4,
       5,     4,     5,     2,     2,     2,     2,     2,     1,     3,
       1,     3,     1,     2,     3,     5,     2,     4,     2,     4,
       1,     3,     1,     3,     2,     3,     1,     2,     1,     4,
       3,     3,     3,     3,     2,     1,     1,     4,     3,     3,
       3,     3,     2,     1,     1,     1,     1,     2,     1,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     4,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     6,     5,     5,     5,     5,     4,     3,     3,
       3,     3,     3,     3,     3,     3,     3,     4,     4,     2,
       2,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     2,     2,     3,     3,     3,     3,
       6,     6,     1,     1,     2,     4,     2,     1,     3,     3,
       3,     1,     1,     1,     2,     2,     4,     2,     1,     2,
       2,     4,     1,     0,     2,     2,     2,     1,     1,     3,
       1,     2,     3,     4,     3,     4,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     4,     0,     0,
       5,     0,     3,     3,     3,     2,     3,     3,     1,     2,
       4,     3,     2,     1,     2,     0,     0,     5,     6,     6,
       0,     0,     7,     0,     0,     7,     5,     4,     0,     0,
       9,     0,     6,     0,     0,     8,     0,     5,     0,     0,
       7,     0,     0,     9,     1,     1,     1,     1,     1,     1,
       1,     2,     1,     1,     1,     5,     1,     2,     1,     1,
       1,     4,     6,     3,     5,     2,     4,     1,     0,     4,
       4,     2,     2,     1,     2,     0,     6,     8,     4,     6,
       4,     3,     6,     2,     4,     6,     2,     4,     2,     4,
       1,     1,     1,     0,     4,     1,     4,     1,     4,     1,
       3,     1,     1,     4,     1,     3,     3,     0,     5,     2,
       4,     5,     5,     2,     4,     4,     3,     3,     3,     2,
       1,     4,     0,     5,     0,     5,     5,     1,     1,     6,
       1,     1,     1,     1,     2,     1,     2,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     2,     3,     1,     2,
       1,     0,     4,     1,     2,     2,     3,     2,     3,     1,
       1,     2,     1,     2,     1,     2,     1,     0,     4,     2,
       3,     1,     4,     2,     1,     1,     1,     1,     1,     2,
       2,     3,     1,     1,     2,     2,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     0,     4,     3,     3,     2,     2,
       2,     1,     2,     1,     1,     3,     1,     3,     1,     1,
       2,     1,     4,     2,     2,     1,     2,     0,     6,     8,
       4,     6,     4,     6,     2,     4,     6,     2,     4,     2,
       4,     1,     0,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     4,     1,     3,     2,     2,     2,     1,     3,
       1,     3,     1,     1,     2,     1,     1,     1,     2,     2,
       1,     1,     0,     4,     1,     2,     1,     3,     1,     2,
       3,     3,     3,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     0,     1,     2,     0,     1,     1,     1,     1,
       1,     1,     1,     2,     0
};


enum { YYENOMEM = -2 };

#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)

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

/* Backward compatibility with an undocumented macro.
   Use YYerror or YYUNDEF. */
#define YYERRCODE YYUNDEF


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
# ifndef YY_LOCATION_PRINT
#  define YY_LOCATION_PRINT(File, Loc) ((void) 0)
# endif


# define YY_SYMBOL_PRINT(Title, Kind, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Kind, Value, p); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo,
                       yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, parser_state *p)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  YYUSE (p);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yykind < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yykind], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo,
                 yysymbol_kind_t yykind, YYSTYPE const * const yyvaluep, parser_state *p)
{
  YYFPRINTF (yyo, "%s %s (",
             yykind < YYNTOKENS ? "token" : "nterm", yysymbol_name (yykind));

  yy_symbol_value_print (yyo, yykind, yyvaluep, p);
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp,
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
                       &yyvsp[(yyi + 1) - (yynrhs)], p);
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
            yysymbol_kind_t yykind, YYSTYPE *yyvaluep, parser_state *p)
{
  YYUSE (yyvaluep);
  YYUSE (p);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yykind, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yykind);
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

    /* Their size.  */
    YYPTRDIFF_T yystacksize;

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

  int yyn;
  /* The return value of yyparse.  */
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  yysymbol_kind_t yytoken = YYSYMBOL_YYEMPTY;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yynerrs = 0;
  yystate = 0;
  yyerrstatus = 0;

  yystacksize = YYINITDEPTH;
  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;


  YYDPRINTF ((stderr, "Starting parse\n"));

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
  YY_STACK_PRINT (yyss, yyssp);

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
#  undef YYSTACK_RELOCATE
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

  /* YYCHAR is either empty, or end-of-input, or a valid lookahead.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token\n"));
      yychar = yylex (&yylval, p);
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
#line 1495 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 6063 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3:
#line 1500 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 6072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4:
#line 1507 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6080 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5:
#line 1513 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6088 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6:
#line 1517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6097 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7:
#line 1522 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8:
#line 1526 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6113 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10:
#line 1533 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 6122 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11:
#line 1538 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 6133 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12:
#line 1550 "mrbgems/mruby-compiler/core/parse.y"
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
#line 6159 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13:
#line 1574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6167 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14:
#line 1580 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6175 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15:
#line 1584 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6184 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16:
#line 1589 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6192 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17:
#line 1593 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 6200 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18:
#line 1598 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 6206 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19:
#line 1599 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 6214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20:
#line 1603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6222 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21:
#line 1607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6230 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22:
#line 1611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6238 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23:
#line 1615 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6246 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24:
#line 1619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6254 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25:
#line 1623 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6262 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26:
#line 1627 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 6271 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28:
#line 1633 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6279 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29:
#line 1637 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6287 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30:
#line 1641 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6295 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31:
#line 1645 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6303 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 33:
#line 1652 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6311 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34:
#line 1656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6319 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35:
#line 1660 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_lit("[]"), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6327 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36:
#line 1664 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6335 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37:
#line 1668 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6343 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38:
#line 1672 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 6352 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39:
#line 1677 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6360 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40:
#line 1681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6369 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42:
#line 1689 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6377 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 45:
#line 1698 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6385 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 46:
#line 1702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6393 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47:
#line 1706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6401 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 48:
#line 1710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6409 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50:
#line 1717 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 6420 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 54:
#line 1731 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6428 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55:
#line 1737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 6437 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56:
#line 1744 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 6447 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 57:
#line 1752 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6455 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58:
#line 1756 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 6464 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 59:
#line 1761 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6472 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 60:
#line 1765 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 6481 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 61:
#line 1770 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 6489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62:
#line 1774 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 6498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63:
#line 1779 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 6506 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64:
#line 1783 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6514 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65:
#line 1787 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6522 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66:
#line 1791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6530 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67:
#line 1795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6538 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68:
#line 1801 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6546 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69:
#line 1805 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6554 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71:
#line 1812 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6562 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72:
#line 1818 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6570 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73:
#line 1822 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 6578 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74:
#line 1826 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6586 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75:
#line 1830 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6594 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76:
#line 1834 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 6602 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77:
#line 1838 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 6610 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 78:
#line 1842 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 6618 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79:
#line 1846 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6626 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80:
#line 1850 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 6634 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81:
#line 1854 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 6642 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83:
#line 1861 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 6650 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84:
#line 1867 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 6658 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85:
#line 1871 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 6666 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86:
#line 1877 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6674 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87:
#line 1881 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 6682 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88:
#line 1887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6690 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89:
#line 1891 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_lit("[]"), (yyvsp[-1].nd), '.');
                    }
#line 6698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 90:
#line 1895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6706 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91:
#line 1899 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6714 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92:
#line 1903 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6722 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93:
#line 1907 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94:
#line 1913 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6742 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95:
#line 1919 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6751 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96:
#line 1926 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6759 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97:
#line 1930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_lit("[]"), (yyvsp[-1].nd), '.');
                    }
#line 6767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98:
#line 1934 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99:
#line 1938 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6783 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100:
#line 1942 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101:
#line 1946 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6801 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102:
#line 1952 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6811 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103:
#line 1958 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6820 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104:
#line 1963 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 6828 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105:
#line 1969 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 6836 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107:
#line 1976 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((node*)1, nsym((yyvsp[0].id)));
                    }
#line 6844 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108:
#line 1980 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((node*)0, nsym((yyvsp[0].id)));
                    }
#line 6852 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109:
#line 1984 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 6861 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113:
#line 1994 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6870 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 114:
#line 1999 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6879 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117:
#line 2010 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 6887 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 118:
#line 2013 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 6893 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 119:
#line 2014 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 6901 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 120:
#line 2019 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("|");   }
#line 6907 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121:
#line 2020 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("^");   }
#line 6913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122:
#line 2021 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("&");   }
#line 6919 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 123:
#line 2022 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("<=>"); }
#line 6925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 124:
#line 2023 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("==");  }
#line 6931 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125:
#line 2024 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("==="); }
#line 6937 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126:
#line 2025 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("=~");  }
#line 6943 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127:
#line 2026 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("!~");  }
#line 6949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128:
#line 2027 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit(">");   }
#line 6955 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129:
#line 2028 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit(">=");  }
#line 6961 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130:
#line 2029 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("<");   }
#line 6967 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131:
#line 2030 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("<=");  }
#line 6973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132:
#line 2031 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("!=");  }
#line 6979 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133:
#line 2032 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("<<");  }
#line 6985 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134:
#line 2033 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit(">>");  }
#line 6991 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135:
#line 2034 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("+");   }
#line 6997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136:
#line 2035 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("-");   }
#line 7003 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137:
#line 2036 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("*");   }
#line 7009 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138:
#line 2037 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("*");   }
#line 7015 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139:
#line 2038 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("/");   }
#line 7021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140:
#line 2039 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("%");   }
#line 7027 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141:
#line 2040 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("**");  }
#line 7033 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142:
#line 2041 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("**");  }
#line 7039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143:
#line 2042 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("!");   }
#line 7045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144:
#line 2043 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("~");   }
#line 7051 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145:
#line 2044 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("+@");  }
#line 7057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146:
#line 2045 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("-@");  }
#line 7063 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147:
#line 2046 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("[]");  }
#line 7069 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148:
#line 2047 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("[]="); }
#line 7075 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149:
#line 2048 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_lit("`");   }
#line 7081 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 190:
#line 2066 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7089 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 191:
#line 2070 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7097 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 192:
#line 2074 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_lit("[]"), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 193:
#line 2078 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7113 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 194:
#line 2082 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 195:
#line 2086 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 196:
#line 2090 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7138 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 197:
#line 2095 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7147 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198:
#line 2100 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7156 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199:
#line 2105 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7164 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200:
#line 2109 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7172 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201:
#line 2113 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 7180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202:
#line 2117 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 7188 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203:
#line 2121 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 7196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204:
#line 2125 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 7204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205:
#line 2129 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 7212 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206:
#line 2133 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 7220 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207:
#line 2137 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208:
#line 2141 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7236 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209:
#line 2145 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 7244 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210:
#line 2149 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "-@");
                    }
#line 7252 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211:
#line 2153 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 7260 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212:
#line 2157 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 7268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213:
#line 2161 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 7276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214:
#line 2165 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 7284 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215:
#line 2169 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 7292 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216:
#line 2173 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 7300 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217:
#line 2177 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 7308 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218:
#line 2181 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 7316 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219:
#line 2185 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 7324 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220:
#line 2189 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 7332 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221:
#line 2193 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 7340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222:
#line 2197 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 7348 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223:
#line 2201 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 7356 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224:
#line 2205 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7364 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225:
#line 2209 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 7372 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226:
#line 2213 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 7380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227:
#line 2217 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 7388 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228:
#line 2221 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7396 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229:
#line 2225 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7404 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230:
#line 2229 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7412 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231:
#line 2233 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7420 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232:
#line 2237 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7428 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234:
#line 2244 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7437 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235:
#line 2249 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd)));
                    }
#line 7445 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236:
#line 2253 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237:
#line 2260 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238:
#line 2264 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7472 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239:
#line 2272 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240:
#line 2276 "mrbgems/mruby-compiler/core/parse.y"
                    {
#if 1
                      mrb_sym r = mrb_intern_lit(p->mrb, "*");
                      mrb_sym b = mrb_intern_lit(p->mrb, "&");
                      if (local_var_p(p, r)  && local_var_p(p, b)) {
                        (yyval.nd) = cons(list1(new_splat(p, new_lvar(p, r))),
                                  new_block_arg(p, new_lvar(p, b)));
                      }
#else
                      mrb_sym r = mrb_intern_lit(p->mrb, "*");
                      mrb_sym k = mrb_intern_lit(p->mrb, "**");
                      mrb_sym b = mrb_intern_lit(p->mrb, "&");
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
#line 7508 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245:
#line 2308 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7517 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246:
#line 2313 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7526 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247:
#line 2318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7535 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248:
#line 2325 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(list1((yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7545 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 249:
#line 2331 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7554 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250:
#line 2336 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7563 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251:
#line 2341 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7572 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252:
#line 2346 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7581 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253:
#line 2352 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 7590 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254:
#line 2357 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7599 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255:
#line 2364 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 7607 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256:
#line 2370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7615 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257:
#line 2374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 7623 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 260:
#line 2384 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[0].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7633 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 261:
#line 2390 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7643 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262:
#line 2396 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7652 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263:
#line 2401 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7661 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264:
#line 2408 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265:
#line 2413 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7679 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266:
#line 2418 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 7688 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274:
#line 2432 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 7696 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275:
#line 2436 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 7704 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 276:
#line 2440 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7713 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277:
#line 2446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7722 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278:
#line 2451 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7731 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279:
#line 2455 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 7737 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280:
#line 2456 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281:
#line 2460 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 7752 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282:
#line 2461 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 7760 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 283:
#line 2465 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7768 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 284:
#line 2469 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 7776 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 285:
#line 2473 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 7784 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 286:
#line 2477 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7793 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 287:
#line 2482 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7802 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 288:
#line 2487 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 7810 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 289:
#line 2491 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 290:
#line 2495 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 7826 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 291:
#line 2499 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 7834 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 292:
#line 2503 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), cons(0, (yyvsp[0].nd)));
                    }
#line 7842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294:
#line 2508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7851 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295:
#line 2513 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 7861 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296:
#line 2519 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7870 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297:
#line 2524 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 7882 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298:
#line 2535 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7891 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299:
#line 2543 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300:
#line 2547 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7906 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301:
#line 2547 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7912 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302:
#line 2550 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303:
#line 2554 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7927 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304:
#line 2554 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305:
#line 2557 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7942 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306:
#line 2564 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7950 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307:
#line 2568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 7958 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308:
#line 2572 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 7964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309:
#line 2574 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 7970 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310:
#line 2577 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 7979 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 311:
#line 2583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7990 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312:
#line 2591 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8001 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313:
#line 2599 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 8010 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314:
#line 2604 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 8020 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315:
#line 2611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 8033 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316:
#line 2621 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8044 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317:
#line 2629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8055 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318:
#line 2636 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 8064 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319:
#line 2640 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->in_def++;
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8074 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320:
#line 2648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[-5].id), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->cmdarg_stack = (yyvsp[-4].stack);
                    }
#line 8087 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321:
#line 2657 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 8097 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322:
#line 2663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->in_single++;
                      p->lstate = EXPR_ENDFN; /* force for args */
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8108 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323:
#line 2672 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-7].nd), (yyvsp[-4].id), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      p->in_single--;
                      p->cmdarg_stack = (yyvsp[-5].stack);
                    }
#line 8121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324:
#line 2681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 8129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325:
#line 2685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 8137 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326:
#line 2689 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 8145 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327:
#line 2693 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 8153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328:
#line 2699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 8162 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335:
#line 2718 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8170 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337:
#line 2725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338:
#line 2731 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 8186 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340:
#line 2738 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 8194 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 341:
#line 2742 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8202 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 342:
#line 2746 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8210 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 343:
#line 2750 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3((yyvsp[-2].nd), (node*)-1, 0);
                    }
#line 8219 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 344:
#line 2755 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (node*)-1, (yyvsp[0].nd));
                    }
#line 8227 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 345:
#line 2759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8235 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 346:
#line 2763 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 347:
#line 2767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3(0, (node*)-1, 0);
                    }
#line 8252 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 348:
#line 2772 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                    }
#line 8260 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 349:
#line 2776 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (node*)-1, (yyvsp[0].nd));
                    }
#line 8268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 350:
#line 2782 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 351:
#line 2786 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 8284 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352:
#line 2790 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8292 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 353:
#line 2794 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 8300 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354:
#line 2800 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8308 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355:
#line 2804 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 8316 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356:
#line 2810 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8324 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357:
#line 2814 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8332 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358:
#line 2818 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359:
#line 2822 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8348 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360:
#line 2826 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8356 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361:
#line 2830 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8364 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362:
#line 2834 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8372 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363:
#line 2838 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364:
#line 2842 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8388 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365:
#line 2846 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8396 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366:
#line 2850 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8404 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367:
#line 2854 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8412 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368:
#line 2858 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8420 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369:
#line 2862 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8428 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370:
#line 2866 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8436 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371:
#line 2872 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8445 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372:
#line 2877 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373:
#line 2883 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p, 0);}
#line 8460 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374:
#line 2884 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8468 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375:
#line 2888 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8477 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376:
#line 2893 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8485 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377:
#line 2900 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8493 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378:
#line 2904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8501 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381:
#line 2914 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 8510 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383:
#line 2922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8518 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384:
#line 2926 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8526 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385:
#line 2932 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8534 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386:
#line 2936 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8542 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387:
#line 2942 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8551 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388:
#line 2949 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389:
#line 2957 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-1].nd)->car == (node*)NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8575 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390:
#line 2967 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8583 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 391:
#line 2971 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8592 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392:
#line 2976 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8601 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393:
#line 2983 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8609 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 394:
#line 2987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8617 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395:
#line 2991 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8625 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396:
#line 2995 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8633 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397:
#line 2999 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), intern_lit("call"), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 8641 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398:
#line 3003 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), intern_lit("call"), (yyvsp[0].nd), tCOLON2);
                    }
#line 8649 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399:
#line 3007 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8657 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400:
#line 3011 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 8665 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401:
#line 3015 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_lit("[]"), (yyvsp[-1].nd), '.');
                    }
#line 8673 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402:
#line 3021 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8683 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403:
#line 3028 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404:
#line 3035 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8704 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405:
#line 3042 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8715 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406:
#line 3053 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 8723 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407:
#line 3059 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 8736 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409:
#line 3073 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 8745 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411:
#line 3081 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8753 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414:
#line 3089 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8761 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416:
#line 3096 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8769 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423:
#line 3110 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = concat_string(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8777 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 426:
#line 3118 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8785 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 427:
#line 3122 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8793 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 429:
#line 3129 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8801 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 430:
#line 3135 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 431:
#line 3139 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 432:
#line 3145 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lex_strterm = (yyvsp[-2].nd);
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8827 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 433:
#line 3150 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8835 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 434:
#line 3154 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8843 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435:
#line 3160 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8851 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 436:
#line 3164 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8859 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 437:
#line 3170 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438:
#line 3174 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442:
#line 3187 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 8885 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443:
#line 3193 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 8893 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446:
#line 3203 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 8903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447:
#line 3209 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8912 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 448:
#line 3215 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[-2].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 8922 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 449:
#line 3223 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 8930 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450:
#line 3227 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8938 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 451:
#line 3234 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 8947 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 452:
#line 3239 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_dsym(p, new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd))));
                    }
#line 8956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 453:
#line 3246 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458:
#line 3256 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459:
#line 3260 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460:
#line 3266 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 8988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461:
#line 3270 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 464:
#line 3278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 9004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465:
#line 3282 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 9012 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 466:
#line 3288 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 9020 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467:
#line 3292 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 9028 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 468:
#line 3296 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 9036 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 469:
#line 3300 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 9044 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 470:
#line 3304 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 9052 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 471:
#line 3310 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 9060 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 472:
#line 3314 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 9068 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473:
#line 3320 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 9076 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474:
#line 3324 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9084 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475:
#line 3328 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 9092 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476:
#line 3332 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 9100 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 477:
#line 3336 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 9108 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478:
#line 3340 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 9120 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479:
#line 3348 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 9131 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480:
#line 3355 "mrbgems/mruby-compiler/core/parse.y"
                    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    }
#line 9144 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483:
#line 3370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9152 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484:
#line 3374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9161 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485:
#line 3379 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9169 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486:
#line 3390 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9179 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487:
#line 3396 "mrbgems/mruby-compiler/core/parse.y"
                    {
#if 1
                      /* til real keyword args implemented */
                      mrb_sym r = mrb_intern_lit(p->mrb, "*");
                      mrb_sym b = mrb_intern_lit(p->mrb, "&");
                      local_add_f(p, r);
                      (yyval.nd) = new_args(p, 0, 0, r, 0,
                                    new_args_tail(p, 0, 0, b));
#else
                      mrb_sym r = mrb_intern_lit(p->mrb, "*");
                      mrb_sym k = mrb_intern_lit(p->mrb, "**");
                      mrb_sym b = mrb_intern_lit(p->mrb, "&");
                      local_add_f(p, r); local_add_f(p, k);
                      (yyval.nd) = new_args(p, 0, 0, r, 0,
                                    new_args_tail(p, 0, new_kw_rest_args(p, nsym(k)), b));
#endif
                    }
#line 9201 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488:
#line 3414 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9209 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489:
#line 3420 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 9217 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 490:
#line 3426 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9227 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491:
#line 3432 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9236 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492:
#line 3439 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9245 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 493:
#line 3444 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9254 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 494:
#line 3451 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9262 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495:
#line 3455 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 496:
#line 3461 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9278 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 497:
#line 3465 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9286 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 500:
#line 3475 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, nsym((yyvsp[0].id)));
                    }
#line 9294 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501:
#line 3479 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, 0);
                    }
#line 9302 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502:
#line 3485 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9310 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 503:
#line 3489 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9318 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504:
#line 3493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9326 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505:
#line 3497 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9334 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506:
#line 3503 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9342 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507:
#line 3507 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9350 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508:
#line 3513 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9358 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509:
#line 3517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9366 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510:
#line 3521 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9374 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511:
#line 3525 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9382 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512:
#line 3529 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9390 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513:
#line 3533 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9398 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514:
#line 3537 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9406 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515:
#line 3541 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9414 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516:
#line 3545 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517:
#line 3549 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518:
#line 3553 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519:
#line 3557 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520:
#line 3561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521:
#line 3565 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 522:
#line 3569 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, mrb_intern_lit(p->mrb, "&"));
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 9471 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523:
#line 3576 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 9480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524:
#line 3581 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 9489 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525:
#line 3586 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 9498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526:
#line 3591 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 9507 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527:
#line 3596 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 9516 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528:
#line 3603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9524 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529:
#line 3607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9533 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530:
#line 3614 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 9541 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531:
#line 3618 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 9549 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532:
#line 3622 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn_param(p, (yyvsp[-1].nd), p->locals->car);
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 9559 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533:
#line 3630 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9567 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534:
#line 3634 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9575 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535:
#line 3640 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 9585 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536:
#line 3648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9595 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537:
#line 3656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9605 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538:
#line 3664 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9613 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539:
#line 3668 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540:
#line 3674 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9629 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541:
#line 3678 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9637 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544:
#line 3688 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9646 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545:
#line 3693 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, mrb_intern_lit(p->mrb, "*"));
                      (yyval.id) = -1;
                    }
#line 9655 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548:
#line 3704 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9663 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549:
#line 3710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550:
#line 3714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9679 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551:
#line 3720 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9688 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552:
#line 3724 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 9694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553:
#line 3725 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9721 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555:
#line 3751 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9729 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 556:
#line 3757 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 9738 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557:
#line 3762 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560:
#line 3772 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9756 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561:
#line 3778 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9765 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562:
#line 3783 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if ((yyvsp[-2].nd)->car == (node*)NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 9779 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563:
#line 3793 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 9788 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 576:
#line 3820 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 9796 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 577:
#line 3824 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 9804 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 579:
#line 3831 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 9812 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 588:
#line 3852 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 9818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 591:
#line 3858 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 9827 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 594:
#line 3869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9835 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 9839 "mrbgems/mruby-compiler/core/y.tab.c"

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
          = {yyssp, yytoken};
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
        yyerror (p, yymsgp);
        if (yysyntax_error_status == YYENOMEM)
          goto yyexhaustedlab;
      }
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


      yydestruct ("Error: popping",
                  YY_ACCESSING_SYMBOL (yystate), yyvsp, p);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", YY_ACCESSING_SYMBOL (yyn), yyvsp, yylsp);

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


#if 1
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
                  YY_ACCESSING_SYMBOL (+*yyssp), yyvsp, p);
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

#line 3873 "mrbgems/mruby-compiler/core/parse.y"

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
        pylval.id = intern_lit("**");
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
        pylval.id = intern_lit("*");
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
        pylval.id = intern_lit("<<");
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
        pylval.id = intern_lit(">>");
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
        pylval.id = intern_lit("&&");
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
      pylval.id = intern_lit("&");
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
        pylval.id = intern_lit("||");
        p->lstate = EXPR_BEG;
        return tOP_ASGN;
      }
      pushback(p, c);
      return tOROP;
    }
    if (c == '=') {
      pylval.id = intern_lit("|");
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
      pylval.id = intern_lit("+");
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
      pylval.id = intern_lit("-");
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
      pylval.id = intern_lit("/");
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
      pylval.id = intern_lit("^");
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
      pylval.id = intern_lit("%");
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
  p->on_eval = cxt->on_eval;
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
mrb_parser_set_filename(struct mrb_parser_state *p, const char *f)
{
  mrb_sym sym;
  size_t i;
  mrb_sym* new_table;

  sym = mrb_intern_cstr(p->mrb, f);
  p->filename_sym = sym;
  p->lineno = (p->filename_table_length > 0)? 0 : 1;

  for (i = 0; i < p->filename_table_length; ++i) {
    if (p->filename_table[i] == sym) {
      p->current_filename_index = (int)i;
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
  mrb_int ai = mrb_gc_arena_save(mrb);
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
