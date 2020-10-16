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
#line 1376 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1614 "mrbgems/mruby-compiler/core/y.tab.c"

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
#define YYLAST   12357

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  177
/* YYNRULES -- Number of rules.  */
#define YYNRULES  607
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1058

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
       0,  1535,  1535,  1535,  1546,  1552,  1556,  1561,  1565,  1571,
    1573,  1572,  1586,  1613,  1619,  1623,  1628,  1632,  1638,  1638,
    1642,  1646,  1650,  1654,  1658,  1662,  1666,  1671,  1672,  1676,
    1680,  1684,  1688,  1689,  1692,  1697,  1702,  1706,  1712,  1716,
    1720,  1724,  1728,  1732,  1737,  1741,  1748,  1749,  1753,  1757,
    1758,  1762,  1766,  1770,  1774,  1778,  1788,  1787,  1802,  1811,
    1812,  1815,  1816,  1823,  1822,  1837,  1841,  1846,  1850,  1855,
    1859,  1864,  1868,  1872,  1876,  1880,  1886,  1890,  1896,  1897,
    1903,  1907,  1911,  1915,  1919,  1923,  1927,  1931,  1935,  1939,
    1945,  1946,  1952,  1956,  1962,  1966,  1972,  1976,  1980,  1984,
    1988,  1992,  1998,  2004,  2011,  2015,  2019,  2023,  2027,  2031,
    2037,  2043,  2048,  2054,  2058,  2061,  2065,  2069,  2076,  2077,
    2078,  2079,  2084,  2091,  2092,  2095,  2099,  2099,  2105,  2106,
    2107,  2108,  2109,  2110,  2111,  2112,  2113,  2114,  2115,  2116,
    2117,  2118,  2119,  2120,  2121,  2122,  2123,  2124,  2125,  2126,
    2127,  2128,  2129,  2130,  2131,  2132,  2133,  2134,  2137,  2137,
    2137,  2138,  2138,  2139,  2139,  2139,  2140,  2140,  2140,  2140,
    2141,  2141,  2141,  2142,  2142,  2142,  2143,  2143,  2143,  2143,
    2144,  2144,  2144,  2144,  2145,  2145,  2145,  2145,  2146,  2146,
    2146,  2146,  2147,  2147,  2147,  2147,  2148,  2148,  2151,  2155,
    2159,  2163,  2167,  2171,  2175,  2180,  2185,  2190,  2194,  2198,
    2202,  2206,  2210,  2214,  2218,  2222,  2226,  2230,  2234,  2238,
    2242,  2246,  2250,  2254,  2258,  2262,  2266,  2270,  2274,  2278,
    2282,  2286,  2290,  2294,  2298,  2302,  2306,  2310,  2314,  2318,
    2322,  2326,  2330,  2334,  2338,  2346,  2355,  2364,  2374,  2380,
    2381,  2386,  2390,  2397,  2401,  2409,  2413,  2439,  2440,  2443,
    2444,  2445,  2450,  2455,  2462,  2468,  2473,  2478,  2483,  2490,
    2490,  2501,  2507,  2511,  2517,  2518,  2521,  2527,  2533,  2538,
    2545,  2550,  2555,  2562,  2563,  2564,  2565,  2566,  2567,  2568,
    2569,  2573,  2578,  2577,  2589,  2593,  2588,  2598,  2598,  2602,
    2606,  2610,  2614,  2619,  2624,  2628,  2632,  2636,  2640,  2644,
    2645,  2651,  2657,  2650,  2669,  2677,  2685,  2685,  2685,  2692,
    2692,  2692,  2699,  2705,  2710,  2712,  2709,  2721,  2719,  2737,
    2742,  2735,  2759,  2757,  2773,  2783,  2794,  2798,  2802,  2806,
    2812,  2819,  2820,  2821,  2824,  2825,  2828,  2829,  2837,  2838,
    2844,  2848,  2851,  2855,  2859,  2863,  2868,  2872,  2876,  2880,
    2886,  2885,  2895,  2899,  2903,  2907,  2913,  2918,  2923,  2927,
    2931,  2935,  2939,  2943,  2947,  2951,  2955,  2959,  2963,  2967,
    2971,  2975,  2979,  2985,  2990,  2997,  2997,  3001,  3006,  3013,
    3017,  3023,  3024,  3027,  3032,  3035,  3039,  3045,  3049,  3056,
    3055,  3070,  3080,  3084,  3089,  3096,  3100,  3104,  3108,  3112,
    3116,  3120,  3124,  3128,  3135,  3134,  3149,  3148,  3164,  3172,
    3181,  3184,  3191,  3194,  3198,  3199,  3202,  3206,  3209,  3213,
    3216,  3217,  3218,  3219,  3222,  3223,  3229,  3230,  3231,  3235,
    3241,  3242,  3248,  3253,  3252,  3263,  3267,  3273,  3277,  3283,
    3287,  3293,  3296,  3297,  3300,  3306,  3312,  3313,  3316,  3323,
    3322,  3336,  3340,  3347,  3352,  3359,  3365,  3366,  3367,  3368,
    3369,  3373,  3379,  3383,  3389,  3390,  3391,  3395,  3401,  3405,
    3409,  3413,  3417,  3423,  3427,  3433,  3437,  3441,  3445,  3449,
    3453,  3461,  3468,  3479,  3480,  3484,  3488,  3487,  3503,  3509,
    3529,  3530,  3536,  3542,  3548,  3555,  3560,  3567,  3571,  3577,
    3581,  3587,  3588,  3591,  3595,  3601,  3605,  3609,  3613,  3619,
    3624,  3629,  3633,  3637,  3641,  3645,  3649,  3653,  3657,  3661,
    3665,  3669,  3673,  3677,  3681,  3686,  3692,  3697,  3702,  3707,
    3712,  3719,  3723,  3730,  3735,  3734,  3746,  3750,  3756,  3764,
    3772,  3780,  3784,  3790,  3794,  3800,  3801,  3804,  3809,  3816,
    3817,  3820,  3826,  3830,  3836,  3841,  3841,  3866,  3867,  3873,
    3878,  3884,  3885,  3888,  3894,  3899,  3909,  3916,  3917,  3918,
    3921,  3922,  3923,  3924,  3927,  3928,  3929,  3932,  3933,  3936,
    3940,  3946,  3947,  3953,  3954,  3957,  3958,  3961,  3964,  3965,
    3966,  3969,  3970,  3971,  3974,  3981,  3982,  3986
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
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,    61,    63,    58,    62,    60,   124,    94,    38,    43,
      45,    42,    47,    37,   376,    33,   126,   377,   123,   125,
      91,    93,    44,    96,    40,    41,    59,    46,    10
};
# endif

#define YYPACT_NINF (-837)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-608)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -837,   103,  3160,  -837,  7773,  9779, 10121,  6081,  -837,  9425,
    9425,  -837,  -837,  9893,  7263,  5935,  8127,  8127,  -837,  -837,
    8127,  3680,  3272,  -837,  -837,  -837,  -837,   190,  7263,  -837,
      -3,  -837,  -837,  -837,  6223,  2693,  -837,  -837,  6365,  -837,
    -837,  -837,  -837,  -837,  -837,  -837,   133,  9543,  9543,  9543,
    9543,    77,  5194,  1544,  8363,  8717,  7545,  -837,  6981,   531,
    1282,  1296,  1309,    94,  -837,   400,  9661,  9543,  -837,  1192,
    -837,  1662,   101,  -837,   464,  1280,  1280,  -837,  -837,   118,
      80,  -837,    97, 10007,  -837,    96, 12077,   754,   868,    87,
      47,  -837,   186,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
    -837,  -837,   208,   122,  -837,   293,    70,  -837,  -837,  -837,
    -837,  -837,    22,    22,   109,   700,   840,  -837,  9425,   345,
    5313,   460,  1280,  1280,  -837,   134,  -837,  1048,  -837,  -837,
      70,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
    -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
    -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,    14,
      29,    36,    42,  -837,  -837,  -837,  -837,  -837,  -837,    82,
     116,   142,   151,  -837,   179,  -837,  -837,  -837,  -837,  -837,
    -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
    -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
    -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
     184,  4372,   207,   464,   125,   153, 12162,  1076,   206,   172,
     285,   125,  9425,  9425,  1099,   224,  -837,  -837,  1121,   269,
     462,   546,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
    -837,  7122,  -837,  -837,   191,  -837,  -837,  -837,  -837,  -837,
    -837,  1192,  -837,   234,  -837,   294,  -837,  -837,  1192,  3408,
    9543,  9543,  9543,  9543,  -837, 12139,  -837,  -837,   209,   297,
     209,  -837,  -837,  -837,  7891,  -837,  -837,  -837,  8127,  -837,
    -837,  -837,  5935,  9425,  -837,  -837,   233,  5432,  -837,  1142,
     301, 12224, 12224,   606,  8009,  5194,   245,  1192,  1662,  1192,
     272,  -837,  8009,  1192,   254,   959,   959,  -837, 12139,   259,
     959,  -837,   348, 10235,   273,  1176,  1181,  1185,  1417,  -837,
    -837,  -837,  -837,  1314,  -837,  -837,  -837,  -837,  -837,  -837,
     642,   713,  -837,  -837,  1389,  -837,  1427,  -837,  1430,  -837,
     379,   341,   350,  -837,  -837,  -837,  -837,  5697,  9425,  9425,
    9425,  9425,  8009,  9893,  9425,  9425,    73,  -837,  -837,  -837,
    -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  1643,
     317,  4372,  9543,  -837,   312,   405,   337,  -837,  1192,  -837,
    -837,  -837,   351,  9543,  -837,   368,   461,   380,   483,  -837,
     406,  4372,  -837,  -837,  8835,  -837,  5194,  7659,   395,  8835,
    9543,  9543,  9543,  9543,  9543,  9543,  9543,  9543,  9543,  9543,
    9543,  9543,  9543,  9543,  9543,  9893,  9543,  9543,  9543,  9543,
    9543,  9543,  9543,  9543,  9543,  9543,  9543,  2270,  -837,  8127,
    -837, 10513,  -837,  -837, 11717,  -837,  -837,  -837,  -837,  9661,
    9661,  -837,   454,  -837,   464,  -837,  1201,  -837,  -837,  -837,
    -837,  -837,  -837, 10599,  8127, 10685,  4372,  9425,  -837,  -837,
    -837,   560,   565,   332,  -837,  4518,   567,  9543, 10771,  8127,
   10857,  9543,  9543,  4810,   525,   525,   783, 10943,  8127, 11029,
    -837,   518,  -837,  5432,   294,  -837,  -837,  8953,   577,  -837,
     642,  9543, 12162, 12162, 12162,  9543,   899,  -837,  8245,  -837,
    9543,  -837,  8481,  1192,   465,  1192,   209,   209,  -837,  -837,
     624,   471,  -837,  -837,  7263,  4929,   469, 10771, 10857,  9543,
    1662,  1192,  -837,  -837,  5816,   480,  1662,  -837,  -837,  8599,
    -837,  1192,  8717,  -837,  -837,  -837,  1201,    97, 10235,  -837,
   10235, 11115,  8127, 11201,  1700,  -837,  -837,  -837,  1450,  5432,
     642,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  9543,  9543,
    -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
    -837,  -837,  1563,  1192,  1192,  9543,   611, 12162,    68,  -837,
    -837,  -837,   132,  -837,  -837,  1700,  -837, 12162,  1700,  -837,
    -837,  1804,  -837,  -837,  9543,   619,    41,  9543,  -837, 11897,
     209,  -837,  1192, 10235,   494,  -837,  -837,  -837,   595,   535,
    2140,  -837,  -837,  1203,   362,   301, 10535, 10535, 10535, 10535,
    1237,  1237, 10621,  2780, 10535, 10535, 12224, 12224,   215,   215,
    -837,  -837, 11872,  1237,  1237,  1225,  1225,  1348,    72,    72,
     301,   301,   301,  3816,  6721,  4088,  6839,  -837,    22,  -837,
     505,   608,  -837,   658,  -837,  -837,  3544,  -837,  -837,  1798,
      41,    41,  -837,  3019,  -837,  -837,  -837,  -837,  -837,  1192,
    9425,  4372,   901,   989,  -837,    22,   527,    22,   655,   624,
    7404,  -837,  9071,   659,  -837,   573,  -837,  6483,  6602,   544,
     387,   451,   659,  -837,  -837,  -837,  -837,   627,   671,   551,
     881,  1004,  9425,  7263,   548,   681, 12162,    45,  -837,   642,
   12162, 12162,   642,  9543, 12139,  -837,   209, 12162,  -837,  -837,
    -837,  -837,  8245,  8481,  -837,  -837,  -837,   561,  -837,  -837,
     143,  1662,  1192,   959,   395,  -837,   901,   989,   564,  1026,
    1063,   554,    75,  -837,   568,  -837,   301,   301,  -837,   167,
    1192,   569,  -837,  -837, 11789,  -837,   652,  -837,   337,  -837,
    -837,  -837,   574,   599,   607,  -837,   613,   652,   607, 11810,
    -837,  -837,  1700,  4372,  -837,  -837, 11968,  9189,  -837,  -837,
   10235,  8009,  9661,  9543, 11287,  8127, 11373,    74,  9661,  9661,
    -837,   454,   758,  9661,  9661,  -837,   454,    47,   118,  4372,
    5432,    41,  -837,  1192,   704,  -837,  -837,  -837,  -837, 11897,
    -837,   643,  -837,  5075,   752,  -837,  9425,   753,  -837,  9543,
    9543,   452,  9543,  9543,   767,  5578,  5578,   884,   525,  -837,
    -837,  -837,  9307,  4664,   642, 12162,  -837,   209,  -837,  -837,
    -837,   477,   639,   646,  4372,  5432,  -837,  -837,  -837,   653,
    -837,  1573,  9543,  -837,  1700,  -837,  1804,  -837,  1804,  -837,
    1804,  -837,  -837,  9543,  -837,   554,   554, 10349,  -837,   656,
     337,   674, 10349,  -837,   685,   686,  -837,   779,  9543, 11987,
    -837,  -837, 12162,  3952,  4224,   691,   456,   463,  9543,  9543,
    -837,  -837,  -837,  -837,  -837,  9661,  -837,  -837,  -837,  -837,
    -837,  -837,  -837,   822,   707,  5432,  4372,  -837,  -837, 10463,
     125,  -837,  -837,  5578,  -837,  -837,   125,  -837,  9543,  -837,
     824,   828,  -837, 12162,   136,  8481,  -837,  1502,   833,   717,
    1473,  1473,  1069, 12162,   607,   708,   607,   607, 12162,   731,
     732,   805,  1246,    68,  -837,  -837,  1665,  -837,  1246,  1700,
    -837,  1804,  -837,  -837, 12058,   510, 12162, 12162,  -837,  -837,
    -837,  -837,   722,   848,   814,  -837,  1336,  1181,  1185,  4372,
    -837,  4518,  -837,  -837,  5578,  -837,  -837,  -837,  -837,    -6,
    -837,  -837,  -837,  -837,   730,   730,  1473,   736,  -837,  1804,
    -837,  -837,  -837,  -837,  -837,  -837, 11459,  -837,   337,    68,
    -837,  -837,   742,   745,   747,  -837,   750,   747,  -837,  -837,
    1201, 11545,  8127, 11631,   565,   573,   858,  1502,  -837,  1473,
     730,  1473,   607,   749,   760,  -837,  1700,  -837,  1804,  -837,
    1804,  -837,  1804,  -837,  -837,   901,   989,   761,   193,   482,
    -837,  -837,  -837,  -837,   730,  -837,   747,   766,   747,   747,
     477,  -837,  1804,  -837,  -837,  -837,   747,  -837
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   292,     0,
       0,   316,   319,     0,     0,   593,   336,   337,   338,   339,
     304,   269,   269,   487,   486,   488,   489,   595,     0,    10,
       0,   491,   490,   492,   478,   579,   480,   479,   482,   481,
     474,   475,   436,   437,   493,   494,   290,     0,     0,     0,
       0,     0,     0,   294,   607,   607,    88,   311,     0,     0,
       0,     0,     0,     0,   451,     0,     0,     0,     3,   593,
       6,     9,    32,    27,    33,   535,   535,    49,    60,    59,
       0,    76,     0,    80,    90,     0,    54,   248,     0,    61,
     309,   283,   284,   434,   285,   286,   287,   432,   431,   463,
     433,   430,   485,     0,   288,   289,   269,     5,     8,   336,
     337,   304,   607,   412,     0,   113,   114,   290,     0,     0,
       0,     0,   535,   535,   116,   495,   340,     0,   485,   289,
       0,   332,   168,   178,   169,   165,   194,   195,   196,   197,
     176,   191,   184,   174,   173,   189,   172,   171,   167,   192,
     166,   179,   183,   185,   177,   170,   186,   193,   188,   187,
     180,   190,   175,   164,   182,   181,   163,   161,   162,   158,
     159,   160,   118,   120,   119,   153,   154,   149,   131,   132,
     133,   140,   137,   139,   134,   135,   155,   156,   141,   142,
     146,   150,   136,   138,   128,   129,   130,   143,   144,   145,
     147,   148,   151,   152,   157,   565,    55,   121,   122,   564,
       0,     0,     0,    58,     0,     0,    54,     0,   485,     0,
     289,     0,     0,     0,   112,     0,   351,   350,     0,     0,
     104,   111,   187,   180,   190,   175,   158,   159,   160,   118,
     119,     0,   123,   125,    20,   124,   454,   459,   458,   601,
     604,   593,   603,     0,   456,     0,   605,   602,   594,   577,
       0,     0,     0,     0,   264,   276,    74,   268,   607,   434,
     607,   569,    75,    73,   607,   258,   305,    72,     0,   257,
     411,    71,   593,     0,   596,    18,     0,     0,   221,     0,
     222,   209,   212,   301,     0,     0,     0,   593,    15,   593,
      78,    14,     0,   593,     0,   598,   598,   249,     0,     0,
     598,   567,     0,     0,    86,     0,    96,   103,   535,   468,
     467,   469,   470,     0,   466,   465,   449,   443,   442,   445,
       0,     0,   440,   461,     0,   472,     0,   438,     0,   447,
       0,   476,   477,    53,   236,   237,     4,   594,     0,     0,
       0,     0,     0,     0,     0,     0,   542,   538,   537,   536,
     539,   540,   511,   544,   556,   512,   560,   559,   555,   535,
     500,     0,   504,   509,   607,   514,   607,   534,     0,   541,
     543,   546,   520,     0,   553,   520,   558,   520,     0,   518,
     500,     0,   399,   401,     0,    92,     0,    84,    81,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   208,   211,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   590,   607,
     589,     0,   592,   591,     0,   416,   414,   310,   435,     0,
       0,   405,    65,   308,   329,   113,   114,   115,   476,   477,
     500,   496,   327,     0,   607,     0,     0,     0,   588,   587,
      56,     0,   607,   301,   342,     0,   341,     0,     0,   607,
       0,     0,     0,     0,     0,     0,   110,     0,   607,     0,
     324,     0,   126,     0,     0,   455,   457,     0,     0,   606,
     571,     0,   277,   576,   271,     0,   274,   265,     0,   273,
       0,   266,     0,   593,     0,   593,   607,   607,   259,   270,
     593,     0,   307,    52,     0,     0,     0,     0,     0,     0,
      17,   593,   299,    13,   594,    77,   295,   298,   302,   600,
     250,   599,   600,   252,   303,   568,   102,    94,     0,    89,
       0,     0,   607,     0,   535,   312,   396,   471,     0,     0,
     446,   452,   450,   441,   462,   473,   439,   448,     0,     0,
       7,    21,    22,    23,    24,    25,    37,    36,    50,    51,
     502,   548,     0,   593,   593,     0,     0,   503,     0,   516,
     563,   513,     0,   517,   501,     0,   527,   549,     0,   530,
     557,     0,   532,   561,     0,     0,   607,     0,    28,    30,
       0,    31,   593,     0,    82,    93,    48,    38,    46,     0,
     253,   198,    29,     0,   289,   218,   226,   231,   232,   233,
     228,   230,   240,   241,   234,   235,   207,   210,   238,   239,
      35,    34,   595,   227,   229,   223,   224,   225,   213,   214,
     215,   216,   217,   580,   585,   581,   586,   410,   269,   408,
       0,   580,   582,   581,   583,   409,   269,   580,   581,   269,
     607,   607,    39,   253,   199,    45,   206,    63,    66,     0,
       0,     0,   113,   114,   117,     0,     0,   607,     0,   593,
       0,   293,   607,   607,   422,   607,   343,   584,   300,     0,
     580,   581,   607,   345,   317,   344,   320,   107,   109,     0,
     106,   108,     0,     0,     0,     0,   276,     0,   323,   572,
     574,   573,     0,     0,   278,   272,   607,   575,   570,   256,
     255,   260,   261,   263,   306,   597,    19,     0,    26,   205,
      79,    16,   593,   598,    95,    87,    99,   101,     0,    98,
     100,   595,     0,   464,     0,   453,   219,   220,   542,   359,
     593,   352,   499,   498,   244,   334,     0,   510,   607,   562,
     519,   547,   520,   520,   520,   554,   520,   542,   520,   246,
     335,   387,   385,     0,   384,   383,   282,     0,    91,    85,
       0,     0,     0,     0,     0,   607,     0,     0,     0,     0,
     407,    69,   413,     0,     0,   406,    67,   402,    62,     0,
       0,   607,   330,     0,     0,   413,   333,   566,    57,   423,
     424,   607,   425,     0,   607,   348,     0,     0,   346,     0,
       0,   413,     0,     0,     0,     0,     0,   105,     0,   127,
     460,   322,     0,     0,   275,   279,   267,   607,    11,   296,
     251,    97,     0,   389,     0,     0,   313,   444,   360,   357,
     545,     0,     0,   515,     0,   523,     0,   525,     0,   531,
       0,   528,   533,     0,   382,   595,   595,   506,   507,   607,
     607,   367,     0,   551,   367,   367,   365,     0,     0,   280,
      83,    47,   254,   580,   581,     0,   580,   581,     0,     0,
      44,   203,    43,   204,    70,     0,    41,   201,    42,   202,
      68,   403,   404,     0,     0,     0,     0,   497,   328,     0,
       0,   427,   349,     0,    12,   429,     0,   314,     0,   315,
       0,     0,   325,   278,   607,   262,   395,     0,     0,     0,
       0,     0,   355,   245,   520,   520,   520,   520,   247,     0,
       0,     0,   505,     0,   363,   364,   367,   375,   550,     0,
     378,     0,   380,   400,   281,   413,   243,   242,    40,   200,
     417,   415,     0,     0,     0,   426,     0,   104,   111,     0,
     428,     0,   318,   321,     0,   419,   420,   418,   393,   595,
     391,   394,   398,   397,   361,   358,     0,   353,   524,     0,
     521,   526,   529,   388,   386,   301,     0,   508,   607,     0,
     366,   373,   367,   367,   367,   552,   367,   367,    64,   331,
     110,     0,   607,     0,   607,   607,     0,     0,   390,     0,
     356,     0,   520,   584,   300,   362,     0,   370,     0,   372,
       0,   379,     0,   376,   381,   107,   109,     0,   580,   581,
     421,   347,   326,   392,   354,   522,   367,   367,   367,   367,
     105,   371,     0,   368,   374,   377,   367,   369
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -837,  -837,  -837,   394,  -837,    18,  -837,  -274,   271,  -837,
     114,  -837,  -837,  -364,  -352,    33,    24,   144,  -837,    83,
     -30,  -837,  -544,  -837,    46,    13,  -216,     5,   -56,  -263,
    -438,   -17,  2090,   -85,   905,    -4,    -8,  -837,  -837,    37,
    -837,  1182,  -837,   476,    56,   -14,  -340,    92,    -7,  -837,
    -387,  -266,  -131,    52,  -350,   523,  -837,  -837,  -837,  -837,
    -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,  -837,
    -837,  -837,     8,  -219,  -417,  -103,  -602,  -837,  -837,  -837,
     154,   455,  -837,  -577,  -837,  -837,  -120,  -837,   -81,  -837,
    -837,   130,  -837,  -837,  -837,   -89,  -837,  -837,  -452,  -837,
     -82,  -837,  -837,  -837,  -837,  -837,   196,    62,  -159,  -837,
    -837,  -837,  -837,  -431,  -292,  -837,   690,  -837,  -837,  -837,
      19,  -837,  -837,  -837,  1778,  2229,   932,  1639,  -837,  -837,
     711,    76,   157,   371,    17,  -837,  -837,  -837,   292,    25,
     178,  -226,  -833,  -694,  -432,  -837,   231,  -701,  -547,  -836,
      10,   377,  -837,  -360,  -837,   228,  -309,  -837,  -837,  -837,
      -9,   695,  -462,   603,  -151,  -837,  -837,   -83,  -837,    16,
     -18,   361,  -224,   559,   -11,   -13,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    68,    69,    70,   286,   461,   462,   297,
     298,   514,    72,    73,   607,    74,    75,    76,   680,   214,
      77,    78,   668,   801,    79,    80,   299,    81,    82,    83,
     539,    84,   215,   124,   125,   242,   243,   244,   703,   646,
     208,    86,   304,   611,   647,   276,   504,   505,   277,   278,
     267,   497,   532,   506,   601,    87,   211,   302,   732,   303,
     318,   742,   222,   825,   223,   826,   702,   974,   671,   669,
     906,   456,   289,   465,   694,   817,   818,   229,   750,   930,
    1000,   947,   865,   773,   774,   866,   842,   979,   980,   545,
     846,   393,   596,    89,    90,   443,   661,   660,   488,   977,
     683,   811,   910,   914,    91,    92,    93,   331,   332,   549,
      94,    95,    96,   550,   252,   253,   254,   483,    97,    98,
      99,   325,   100,   101,   218,   219,   104,   220,   452,   670,
     450,   371,   372,   373,   868,   869,   374,   375,   376,   760,
     586,   378,   379,   380,   381,   572,   382,   383,   384,   873,
     874,   385,   386,   387,   388,   389,   579,   210,   457,   309,
     507,   491,   271,   130,   675,   649,   460,   455,   434,   511,
     843,   512,   530,   256,   257,   258,   301
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     107,   437,   473,   206,   501,   431,   433,   270,   270,   283,
      88,   270,    88,   127,   127,   281,   284,   217,   217,   279,
     285,   228,   108,   217,   217,   217,   226,   398,   217,   122,
     122,   255,   705,   245,   447,   606,   343,   122,   551,   314,
     718,   765,   213,   213,   207,   306,   310,   245,   213,   612,
     538,   207,   307,   311,   324,   849,   347,   300,   696,   709,
      88,   464,   264,   264,   315,   207,   264,   583,   268,   268,
     718,   872,   268,   435,   217,   606,   606,   275,   280,   521,
     122,   814,   533,   799,   800,   346,   535,   662,   665,   650,
     824,   315,   546,   221,   981,   207,   435,   576,   251,   442,
     377,   377,   735,     3,   844,  -487,   305,   122,   266,   272,
     279,   715,   273,  1005,   676,   715,    71,   595,    71,   756,
    -486,   771,   334,   336,   338,   340,   217,  -488,    88,   689,
     570,   888,   293,  -489,   470,   287,  1017,   498,   699,   502,
     362,   464,   250,   574,   400,   479,   392,   377,   377,   123,
     123,   444,   391,   761,   813,   339,   487,   123,   327,   328,
     246,  -487,   441,   247,   248,   779,   274,   772,   275,   280,
     365,   366,   553,  -491,   529,   553,  -486,   553,   432,   553,
     602,   553,   678,  -488,   981,   436,   441,   496,   845,  -489,
    1005,   249,   428,   250,   571,   353,   367,   889,   551,   391,
     123,   394,   738,   424,   425,   426,   518,  -490,   436,  -106,
     329,   330,   269,   269,   274,   439,   269,   399,   748,    88,
     357,   358,   359,   360,   905,   763,  -484,   123,   766,  -491,
     217,   217,   543,  -492,   430,   366,   361,   324,   987,   395,
     246,   501,  -478,   247,   248,   872,    42,   894,   872,    43,
     269,   269,   900,   282,  -112,   213,   213,   537,   745,   451,
     367,   718,   463,  -490,   -77,   471,   499,   484,   499,   270,
    -482,   249,   508,   250,   467,   458,   648,   538,   207,   476,
     656,   834,   217,   659,   524,   -91,   217,   400,   438,  -492,
     217,   217,   531,   531,   480,    88,    62,   531,  -478,  -483,
     300,  -483,    88,    88,   677,   474,   475,   765,  -106,   848,
      88,  -106,  -106,   523,   487,   510,   513,   648,   741,   656,
     264,   315,   975,   296,   264,   872,  -482,  -104,   677,  -104,
     268,   459,   810,   482,   282,   715,   715,   122,   250,  -106,
     538,  -106,   880,   377,   422,   423,   424,   425,   426,   485,
     -96,   496,   247,   248,   490,    88,   217,   217,   217,   217,
      88,   228,   217,   217,   598,   560,   566,   677,   674,   608,
     509,   515,   580,   400,   580,   722,   723,   122,   472,    88,
     604,   213,   213,   213,   213,   548,   440,   568,   569,   553,
     522,   296,   677,   -76,   377,   528,   445,   804,   534,    88,
     446,   300,   217,   536,    88,   315,  -111,   613,   520,   608,
     608,   922,   875,   558,  -111,   540,   526,   745,   606,   761,
     551,   122,   559,   228,   606,   519,   761,   508,   630,   606,
     606,   561,   562,   563,   564,  -103,   890,   217,   575,   122,
     557,   896,   898,   327,   328,   885,   600,   613,   613,   853,
     836,   600,   508,  -110,   578,   440,   581,   123,   341,   342,
     684,    71,   217,   718,    88,   217,   565,   508,   654,   777,
     269,   654,   976,    88,   269,   264,   508,   217,   712,   582,
     822,    88,   734,  -111,   537,   284,   217,   655,   833,   716,
     679,    88,   654,   585,   935,   329,   330,   123,  -108,   877,
     264,   354,   355,  -413,   499,   499,   726,   654,  -106,   840,
     588,   655,   590,   107,   761,   264,   654,   538,   448,   449,
     733,   721,   591,    88,   264,   903,   655,   594,   126,   126,
     786,   606,    88,   245,   593,   655,   126,   605,   715,   707,
     508,   123,   745,   958,   823,   918,   315,   537,   315,   793,
     217,   207,   693,  -485,   667,   654,   794,    88,   516,   123,
     944,   945,   122,   589,   122,   592,   296,  -485,  -413,   377,
     928,   836,  -108,  -105,   655,   681,   832,  -106,   682,   126,
     654,   547,  -413,   686,  -108,   502,  1003,   761,   264,  1006,
     816,   813,   708,   326,   775,   327,   328,  -108,   761,   655,
    -108,  -108,  -485,   895,   -96,   106,   126,   106,   728,  -485,
     720,   315,   106,   106,   787,  -413,   725,  -413,   106,   106,
     106,   284,   -91,   106,  -413,   269,   755,   122,  -108,    71,
    -108,  -105,   963,   648,   770,   656,   780,  -289,   731,   781,
     246,   791,   795,   247,   248,   797,   792,   329,   330,   796,
     269,  -289,   798,  -584,   279,   106,   782,   279,   775,   775,
     525,   354,   355,   795,   527,   269,  1047,   296,   805,   106,
     806,   249,  1037,   250,   269,   279,   808,   813,   217,    88,
     812,   815,   123,   815,   123,   821,  -289,   830,  -103,  1025,
     815,   969,   827,  -289,   269,   829,   831,   971,   269,   519,
     838,   793,   250,   213,   790,   841,   925,   847,   901,   570,
     217,   851,   275,   837,   499,   275,   854,   207,  -584,   908,
     531,   106,   245,   106,   537,   269,  -577,  -110,   269,  -106,
     284,   790,  -584,   275,   600,   213,   685,   909,   269,   246,
     207,   856,   247,   248,   692,   939,   940,   123,  -102,   858,
     -98,   794,   608,   803,   704,   860,   580,   246,   608,   892,
     247,   248,  -300,   608,   608,  -584,   913,  -584,   917,   -99,
     249,  -580,   250,   466,  -584,   552,  -300,   327,   328,  -108,
     466,    88,   919,   508,   926,   828,   370,   390,   315,    88,
     613,  -478,   927,   217,   953,   931,   613,   217,   943,   775,
    -100,   613,   613,   751,   122,  -478,   759,    88,    88,   911,
     759,  -300,   915,  -101,   106,  -581,   946,   489,  -300,   764,
     744,    88,   768,   654,   217,   106,   106,   949,   951,   329,
     330,   264,   955,    88,    88,   499,   126,   960,  -577,   972,
    -478,    88,   655,   973,  -577,  -340,   961,  -478,   982,   213,
     989,   895,    88,    88,   284,   284,   983,   993,   994,  -340,
     995,  1008,   677,  1009,   719,   608,  -578,   580,   580,  1010,
     758,   724,  1019,  1042,  -301,   942,   126,   106,  1021,  -105,
     948,   106,   730,  1013,  1026,   106,   106,  1028,  -301,  1030,
     106,   122,  1032,  -580,  -340,   881,   122,   106,   106,   916,
     -97,  -340,  1050,   613,  -581,   106,   489,  -580,  1052,   727,
    -413,   131,  1041,    88,    88,   664,   666,   966,   269,   269,
     126,    88,   815,  -301,   123,  -102,   864,  -584,   902,   867,
    -301,  -482,  1040,   122,   752,   753,  1043,   584,   126,   209,
     855,   857,   859,   486,   861,  -482,   862,   664,   666,   757,
     106,   106,   106,   106,   106,   106,  1002,   106,   106,   427,
     997,  1018,   762,   778,   500,     0,     0,     0,   284,     0,
       0,     0,  -580,   428,   106,  -413,     0,    88,  -578,    88,
    -482,   269,    88,     0,  -578,     0,  -580,  -482,     0,  -413,
       0,     0,  -584,     0,   106,   729,   580,   106,     0,   106,
     876,     0,   106,   871,     0,     0,  -584,     0,   429,     0,
     508,   123,   684,   815,  -595,   430,   123,  -595,  -595,  -580,
     217,  -580,  -413,   -98,  -413,  -580,   -97,     0,  -580,     0,
    -581,  -413,   106,   695,   695,     0,     0,     0,     0,  -584,
     807,  -584,   106,   106,     0,  -580,     0,   250,  -584,     0,
     654,     0,  -580,   123,     0,     0,     0,   106,   264,   106,
     106,   126,     0,   126,   870,     0,     0,     0,   106,   655,
       0,   904,   106,     0,   246,     0,   106,   247,   248,     0,
    -300,   106,     0,   489,   912,   934,   106,   936,     0,  -581,
     489,   937,     0,   839,  -300,  -581,   920,   921,     0,     0,
     867,   496,     0,   867,   924,   249,   867,   250,   867,  -581,
       0,   850,   988,   990,   991,   992,   929,  -580,   106,     0,
     748,   269,   357,   358,   359,   360,   126,   106,     0,  -300,
       0,  -580,     0,  -581,     0,     0,  -300,     0,   361,   453,
       0,     0,  -581,     0,  -581,   106,  -100,     0,  -581,     0,
       0,  -581,   106,   428,  -581,     0,   867,     0,     0,     0,
       0,   984,   985,     0,  -580,     0,  -580,   468,  -581,     0,
    -580,   759,     0,  -580,   876,     0,   962,   876,     0,   876,
    1004,   428,  1007,   867,   970,   867,     0,   867,   454,   867,
    -290,   216,   216,     0,     0,   430,     0,   216,   265,   265,
    1045,  -581,   265,  -581,  -290,     0,     0,  -581,   269,   867,
    -581,   986,   477,     0,     0,     0,   469,  1020,     0,     0,
    1022,     0,     0,   430,     0,     0,   428,   876,   802,   288,
     290,   291,   292,   517,     0,   998,   265,   308,   870,  -290,
    1014,   870,  1015,   870,     0,  1016,  -290,   428,   344,   345,
       0,     0,  1044,     0,   876,     0,   876,  1046,   876,  1048,
     876,   478,     0,  1049,   891,   893,   466,   541,   430,   897,
     899,     0,  -485,   106,   106,     0,  -289,     0,     0,     0,
     876,   428,   469,  1056,     0,     0,  -485,     0,     0,   430,
    -289,   870,  -301,     0,   784,   891,   893,   400,   897,   899,
     216,     0,     0,   126,     0,   106,  -301,   246,   428,   400,
     247,   248,     0,     0,   413,   414,   542,     0,   870,     0,
     870,  -485,   870,   430,   870,  -289,   413,   414,  -485,   950,
     952,   356,  -289,   357,   358,   359,   360,   996,   249,     0,
     250,  -301,     0,   785,   870,   333,   327,   328,  -301,   361,
     430,   428,   362,   421,   422,   423,   424,   425,   426,   335,
     327,   328,   907,   419,   420,   421,   422,   423,   424,   425,
     426,   959,   337,   327,   328,   363,   106,   547,   327,   328,
       0,   364,   365,   366,   106,   106,   454,   695,   106,     0,
     126,   106,   106,   430,   959,   126,   106,   106,   329,   330,
       0,  1001,   106,   106,   216,   216,     0,     0,   367,     0,
       0,   368,   329,   330,     0,     0,   106,     0,     0,   106,
     400,     0,     0,     0,   369,   329,   330,  1011,   106,   106,
     329,   330,   126,     0,     0,     0,   106,   413,   414,     0,
       0,   428,   492,   493,   494,   344,     0,   106,   106,     0,
       0,     0,   554,   327,   328,     0,   265,  1027,  1029,  1031,
     265,  1033,  1034,     0,   216,   216,     0,     0,   356,   466,
     357,   358,   359,   360,     0,   466,  1012,   422,   423,   424,
     425,   426,     0,   430,     0,     0,   361,     0,     0,   362,
     555,   327,   328,   556,   327,   328,     0,     0,   106,     0,
       0,  1051,  1053,  1054,  1055,   329,   330,     0,   106,   106,
       0,  1057,   363,   743,   327,   328,   106,     0,   364,   365,
     366,     0,     0,     0,   748,     0,   357,   358,   359,   360,
     216,   216,   216,   216,     0,     0,   216,   216,     0,     0,
       0,     0,   361,   329,   330,   367,   329,   330,   368,     0,
       0,     0,     0,   978,   577,   357,   358,   359,   360,     0,
       0,   544,     0,     0,     0,   587,   329,   330,   363,     0,
       0,   361,   106,     0,   106,     0,   599,   106,     0,     0,
       0,   610,   615,   616,   617,   618,   619,   620,   621,   622,
     623,   624,   625,   626,   627,   628,   629,     0,   632,   633,
     634,   635,   636,   637,   638,   639,   640,   641,   642,     0,
       0,   265,     0,     0,   748,   106,   357,   358,   359,   360,
       0,   663,   663,     0,   748,     0,   357,   358,   359,   360,
       0,     0,   361,     0,     0,     0,   265,     0,     0,   216,
       0,   105,   361,   105,   129,   129,     0,     0,     0,   663,
       0,   265,   231,   663,   663,     0,     0,     0,   363,  -297,
     265,     0,  -297,  -297,   749,     0,     0,     0,   363,   706,
       0,     0,     0,   710,   932,     0,     0,   711,     0,     0,
     714,     0,   717,     0,   308,     0,     0,     0,     0,  -297,
    -297,   105,  -297,     0,   356,   317,   357,   358,   359,   360,
       0,   663,   348,   349,   350,   351,   352,     0,     0,     0,
       0,   714,   361,     0,   308,   362,   356,     0,   357,   358,
     359,   360,   317,     0,   265,     0,     0,   573,     0,     0,
       0,     0,     0,     0,   361,     0,     0,   362,   363,     0,
     746,   747,     0,     0,   364,   365,   366,     0,     0,     0,
       0,   356,     0,   357,   358,   359,   360,   754,     0,   105,
     363,     0,     0,     0,     0,     0,   364,   365,   366,   361,
       0,   367,   362,     0,   368,     0,   769,     0,     0,   776,
     102,     0,   102,   128,   128,   128,     0,     0,     0,     0,
       0,   230,     0,   367,     0,   363,   368,     0,  -607,     0,
       0,   364,   365,   366,     0,     0,     0,   999,     0,     0,
       0,  -607,  -607,  -607,  -607,  -607,  -607,     0,  -607,     0,
       0,     0,     0,     0,  -607,  -607,     0,     0,   367,     0,
     102,   368,     0,     0,   316,  -607,  -607,     0,  -607,  -607,
    -607,  -607,  -607,     0,     0,     0,     0,     0,     0,     0,
     105,     0,   216,     0,     0,   767,     0,   357,   358,   359,
     360,   316,     0,     0,   809,     0,     0,     0,     0,     0,
       0,     0,     0,   361,     0,     0,   362,     0,     0,     0,
       0,     0,     0,     0,   216,     0,     0,     0,     0,  -607,
       0,     0,     0,     0,     0,   835,     0,     0,   102,   363,
       0,     0,     0,  -607,   714,   308,   365,   366,     0,     0,
       0,     0,     0,  -607,     0,     0,  -607,  -607,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,     0,     0,
       0,     0,   367,   105,   105,     0,  -607,  -607,     0,     0,
       0,   105,   274,  -607,  -607,  -607,  -607,     0,     0,     0,
       0,     0,   317,     0,     0,     0,     0,     0,     0,   879,
       0,     0,     0,     0,   663,   882,     0,   265,     0,     0,
     663,   663,     0,     0,     0,   663,   663,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   105,     0,     0,   102,
       0,   105,   231,     0,     0,     0,     0,     0,   216,     0,
       0,   663,   663,     0,   663,   663,     0,     0,     0,     0,
     105,     0,     0,     0,   923,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,   933,   105,   317,     0,   614,     0,
       0,     0,     0,     0,     0,   938,     0,     0,     0,     0,
       0,     0,     0,     0,   231,     0,     0,     0,     0,     0,
     954,     0,     0,     0,     0,   102,     0,     0,     0,     0,
     956,   957,   102,   102,     0,     0,     0,   663,   614,   614,
     102,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   316,    85,     0,    85,   105,     0,     0,     0,     0,
     663,     0,     0,   227,   105,     0,     0,   308,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   105,     0,     0,   102,     0,     0,     0,     0,
     102,   230,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    85,     0,     0,     0,     0,     0,     0,   102,
       0,     0,     0,     0,   105,     0,     0,     0,     0,     0,
       0,     0,     0,   105,     0,     0,     0,     0,     0,   102,
       0,     0,     0,     0,   102,   316,     0,   317,     0,   317,
       0,     0,     0,     0,   783,     0,     0,     0,   105,     0,
       0,     0,     0,   230,   265,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      85,     0,   400,   401,   402,   403,   404,   405,   406,   407,
     408,   409,   410,   411,   412,     0,     0,     0,     0,   413,
     414,   103,     0,   103,   102,     0,     0,     0,     0,     0,
       0,     0,   317,   102,     0,     0,     0,     0,     0,     0,
       0,   102,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   102,   416,     0,   417,   418,   419,   420,   421,   422,
     423,   424,   425,   426,     0,     0,     0,     0,     0,     0,
       0,   103,  -276,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   102,     0,     0,     0,     0,     0,     0,
       0,    85,   102,     0,     0,     0,     0,     0,     0,     0,
     105,     0,     0,     0,     0,     0,   316,     0,   316,     0,
       0,   643,   644,     0,     0,   645,     0,   102,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   103,
       0,   184,   185,     0,     0,     0,     0,   186,   187,   188,
     189,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   190,   191,     0,     0,     0,     0,    85,     0,     0,
       0,   316,     0,     0,    85,    85,     0,     0,     0,     0,
       0,     0,    85,     0,   192,   193,   194,   195,   196,   197,
     198,   199,   200,   201,     0,   202,   203,     0,     0,     0,
       0,     0,   105,   204,   274,     0,     0,     0,     0,   317,
     105,   614,     0,     0,     0,     0,     0,   614,     0,     0,
       0,     0,   614,   614,     0,     0,     0,    85,   105,   105,
     103,     0,    85,   567,     0,     0,     0,     0,     0,   102,
       0,     0,   105,     0,     0,     0,     0,     0,     0,     0,
       0,    85,     0,     0,   105,   105,     0,     0,     0,     0,
       0,     0,   105,     0,     0,     0,     0,     0,     0,     0,
       0,    85,     0,   105,   105,     0,    85,     0,     0,   609,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   631,   129,     0,     0,     0,
       0,   129,     0,     0,     0,     0,   103,     0,     0,     0,
       0,     0,     0,   103,   103,     0,     0,     0,     0,   609,
     609,   103,     0,     0,   614,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   105,   105,    85,     0,   968,     0,
       0,   102,   105,     0,     0,    85,     0,     0,   316,   102,
       0,     0,     0,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    85,     0,     0,   103,   102,   102,     0,
       0,   103,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   102,     0,     0,     0,     0,     0,     0,     0,     0,
     103,     0,     0,   102,   102,    85,     0,     0,   105,     0,
     105,   102,     0,   105,    85,     0,     0,     0,     0,     0,
     103,     0,   102,   102,     0,   103,     0,     0,   103,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    85,
       0,     0,     0,     0,     0,   128,     0,     0,     0,     0,
     128,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   103,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   102,   102,   103,     0,   967,     0,     0,
       0,   102,     0,  -291,   103,     0,     0,     0,     0,     0,
       0,     0,   103,     0,     0,     0,  -291,  -291,  -291,  -291,
    -291,  -291,   103,  -291,     0,     0,     0,     0,     0,     0,
    -291,  -291,  -291,     0,     0,     0,     0,     0,     0,     0,
    -291,  -291,     0,  -291,  -291,  -291,  -291,  -291,     0,     0,
       0,     0,     0,     0,   103,     0,     0,   102,     0,   102,
    -291,     0,   102,   103,     0,     0,     0,     0,     0,     0,
       0,    85,     0,     0,     0,  -291,  -291,  -291,  -291,  -291,
    -291,  -291,  -291,  -291,  -291,  -291,  -291,  -291,   103,     0,
       0,     0,  -291,  -291,  -291,     0,     0,  -291,     0,     0,
       0,     0,     0,  -291,     0,     0,     0,     0,  -291,     0,
       0,     0,     0,     0,     0,     0,  -291,     0,  -291,     0,
       0,  -291,  -291,     0,     0,  -291,  -291,  -291,  -291,  -291,
    -291,  -291,  -291,  -291,  -291,  -291,  -291,     0,     0,     0,
       0,     0,  -291,  -291,  -291,  -291,     0,     0,  -291,  -291,
    -291,  -291,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   400,   401,   402,   403,   404,   405,   406,   407,
       0,   409,   410,    85,     0,     0,     0,     0,     0,   413,
     414,    85,   609,     0,     0,     0,     0,     0,   609,     0,
       0,     0,     0,   609,   609,     0,     0,     0,     0,    85,
      85,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     103,     0,     0,    85,   417,   418,   419,   420,   421,   422,
     423,   424,   425,   426,     0,    85,    85,     0,     0,     0,
       0,     0,     0,    85,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    85,    85,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   609,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    85,    85,     0,     0,   965,
       0,     0,   103,    85,     0,     0,     0,     0,     0,     0,
     103,   103,     0,     0,     0,     0,     0,   103,     0,     0,
       0,     0,   103,   103,     0,     0,     0,     0,   103,   103,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   103,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   103,     0,     0,     0,    85,
       0,    85,   103,   783,    85,     0,     0,     0,     0,     0,
       0,     0,     0,   103,   103,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,     0,     0,     0,     0,   413,   414,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   103,   103,     0,     0,     0,     0,
       0,   416,   103,   417,   418,   419,   420,   421,   422,   423,
     424,   425,   426,     0,     0,     0,     0,     0,     0,     0,
    -607,     4,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,     0,     0,     0,     0,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,   103,    27,
     103,     0,     0,   103,     0,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,  -412,     0,    64,  -607,     0,     0,  -607,  -607,
       0,     0,     0,     0,     0,  -412,  -412,  -412,  -412,  -412,
    -412,     0,  -412,     0,    65,    66,    67,     0,  -412,  -412,
    -412,     0,     0,     0,     0,     0,  -607,     0,  -607,  -412,
    -412,     0,  -412,  -412,  -412,  -412,  -412,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -412,  -412,  -412,  -412,  -412,  -412,
    -412,  -412,  -412,  -412,  -412,  -412,  -412,     0,     0,     0,
       0,  -412,  -412,  -412,     0,     0,  -412,     0,     0,     0,
       0,     0,  -412,     0,     0,     0,     0,  -412,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -412,     0,     0,
    -412,  -412,     0,     0,  -412,     0,  -412,  -412,  -412,  -412,
    -412,  -412,  -412,  -412,  -412,  -412,     0,     0,  -478,     0,
    -412,  -412,  -412,  -412,  -412,     0,   274,  -412,  -412,  -412,
    -412,  -478,  -478,  -478,  -478,  -478,  -478,     0,  -478,     0,
       0,     0,     0,     0,     0,  -478,  -478,     0,     0,     0,
       0,     0,     0,     0,     0,  -478,  -478,     0,  -478,  -478,
    -478,  -478,  -478,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   490,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -478,  -478,  -478,  -478,  -478,  -478,  -478,  -478,  -478,  -478,
    -478,  -478,  -478,     0,     0,     0,     0,  -478,  -478,  -478,
       0,  -478,  -478,     0,     0,     0,     0,     0,  -478,     0,
       0,     0,     0,  -478,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -478,     0,     0,  -478,  -478,     0,  -478,
    -478,     0,  -478,  -478,  -478,  -478,  -478,  -478,  -478,  -478,
    -478,  -478,     0,     0,  -607,     0,     0,  -478,  -478,  -478,
    -478,     0,     0,  -478,  -478,  -478,  -478,  -607,  -607,  -607,
    -607,  -607,  -607,     0,  -607,     0,     0,     0,     0,     0,
    -607,  -607,  -607,     0,     0,     0,     0,     0,     0,     0,
       0,  -607,  -607,     0,  -607,  -607,  -607,  -607,  -607,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -607,  -607,  -607,  -607,
    -607,  -607,  -607,  -607,  -607,  -607,  -607,  -607,  -607,     0,
       0,     0,     0,  -607,  -607,  -607,     0,     0,  -607,     0,
       0,     0,     0,     0,  -607,     0,     0,     0,     0,  -607,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -607,
       0,     0,  -607,  -607,     0,     0,  -607,     0,  -607,  -607,
    -607,  -607,  -607,  -607,  -607,  -607,  -607,  -607,     0,     0,
    -607,     0,  -607,  -607,  -607,  -607,  -607,     0,   274,  -607,
    -607,  -607,  -607,  -607,  -607,  -607,  -607,  -607,  -607,     0,
    -607,     0,     0,     0,     0,     0,     0,  -607,  -607,     0,
       0,     0,     0,     0,     0,     0,     0,  -607,  -607,     0,
    -607,  -607,  -607,  -607,  -607,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -607,  -607,  -607,  -607,  -607,  -607,  -607,  -607,
    -607,  -607,  -607,  -607,  -607,     0,     0,     0,     0,  -607,
    -607,  -607,     0,     0,  -607,     0,     0,     0,     0,     0,
    -607,     0,     0,     0,     0,  -607,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -607,     0,     0,  -607,  -607,
       0,     0,  -607,     0,  -607,  -607,  -607,  -607,  -607,  -607,
    -607,  -607,  -607,  -607,     0,     0,  -584,     0,     0,  -607,
    -607,  -607,  -607,     0,   274,  -607,  -607,  -607,  -607,  -584,
    -584,  -584,     0,  -584,  -584,     0,  -584,     0,     0,     0,
       0,     0,  -584,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -584,  -584,     0,  -584,  -584,  -584,  -584,
    -584,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -584,  -584,
    -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,
    -584,     0,     0,     0,     0,  -584,  -584,  -584,     0,   788,
    -584,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -584,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -584,     0,     0,  -584,  -584,     0,  -107,  -584,     0,
    -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,
       0,     0,  -584,     0,  -584,  -584,  -584,     0,   -99,     0,
       0,  -584,  -584,  -584,  -584,  -584,  -584,  -584,     0,  -584,
    -584,     0,  -584,     0,     0,     0,     0,     0,  -584,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -584,
    -584,     0,  -584,  -584,  -584,  -584,  -584,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -584,  -584,  -584,  -584,  -584,  -584,
    -584,  -584,  -584,  -584,  -584,  -584,  -584,     0,     0,     0,
       0,  -584,  -584,  -584,     0,   788,  -584,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -584,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -584,     0,     0,
    -584,  -584,     0,  -107,  -584,     0,  -584,  -584,  -584,  -584,
    -584,  -584,  -584,  -584,  -584,  -584,     0,     0,  -300,     0,
    -584,  -584,  -584,     0,  -584,     0,     0,  -584,  -584,  -584,
    -584,  -300,  -300,  -300,     0,  -300,  -300,     0,  -300,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -300,  -300,     0,  -300,  -300,
    -300,  -300,  -300,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,
    -300,  -300,  -300,     0,     0,     0,     0,  -300,  -300,  -300,
       0,   789,  -300,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -300,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -300,     0,     0,  -300,  -300,     0,  -109,
    -300,     0,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,
    -300,  -300,     0,     0,  -300,     0,     0,  -300,  -300,     0,
    -101,     0,     0,  -300,  -300,  -300,  -300,  -300,  -300,  -300,
       0,  -300,  -300,     0,  -300,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -300,  -300,     0,  -300,  -300,  -300,  -300,  -300,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -300,  -300,  -300,  -300,
    -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,     0,
       0,     0,     0,  -300,  -300,  -300,     0,   789,  -300,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -300,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -300,
       0,     0,  -300,  -300,     0,  -109,  -300,     0,  -300,  -300,
    -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,     0,     0,
       0,     0,     0,  -300,  -300,     0,  -300,     0,     0,  -300,
    -300,  -300,  -300,   294,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,  -607,  -607,  -607,     0,     0,
    -607,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,  -607,     0,     0,
    -607,  -607,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -607,   294,
    -607,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -607,     0,  -607,  -607,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,  -607,     0,     0,  -607,  -607,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -607,   294,  -607,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,  -607,
       0,     0,  -607,    15,  -607,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,  -607,
       0,     0,  -607,  -607,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -607,   294,  -607,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,  -607,     0,     0,  -607,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,  -607,     0,     0,  -607,  -607,
       4,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,    65,    66,    67,     0,    15,     0,
      16,    17,    18,    19,     0,     0,  -607,     0,  -607,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,    51,     0,     0,    52,    53,     0,    54,    55,     0,
      56,     0,     0,    57,     0,    58,    59,    60,    61,    62,
      63,     0,     0,    64,  -607,     0,     0,  -607,  -607,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,    67,     0,     0,  -607,     0,
       0,     0,     0,     0,     0,  -607,   294,  -607,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,  -607,
    -607,     0,     0,     0,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,    51,     0,     0,
      52,    53,     0,    54,    55,     0,    56,     0,     0,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
    -607,     0,     0,  -607,  -607,   294,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    65,
      66,    67,     0,    15,     0,    16,    17,    18,    19,     0,
       0,  -607,     0,  -607,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,   295,
      53,     0,    54,    55,     0,    56,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,  -607,
       0,     0,  -607,  -607,   294,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,    65,    66,
      67,     0,    15,     0,    16,    17,    18,    19,     0,  -607,
    -607,     0,  -607,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,    51,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,  -607,     0,
       0,  -607,  -607,   294,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    65,    66,    67,
       0,    15,     0,    16,    17,    18,    19,     0,  -607,  -607,
       0,  -607,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,  -607,     0,     0,
    -607,  -607,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,    67,     0,
       0,  -607,     0,     0,     0,     0,     0,     0,  -607,   294,
    -607,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -607,     0,     0,     0,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,  -607,     0,     0,  -607,  -607,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    65,    66,    67,     0,    15,     0,    16,    17,
      18,    19,     0,     0,  -607,     0,  -607,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,    51,
       0,     0,    52,    53,     0,    54,    55,     0,    56,     0,
       0,    57,     0,    58,    59,    60,    61,    62,    63,     0,
       0,    64,   246,     0,     0,   247,   248,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,    65,    66,    67,     0,    15,     0,    16,    17,    18,
      19,     0,     0,   249,     0,   250,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,    51,     0,
       0,    52,    53,     0,    54,    55,     0,    56,     0,     0,
      57,     0,    58,    59,    60,    61,    62,    63,     0,     0,
      64,   246,     0,     0,   247,   248,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
      65,    66,    67,     0,    15,     0,    16,    17,    18,    19,
       0,     0,   249,     0,   250,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   212,     0,     0,
     120,    53,     0,    54,    55,     0,     0,     0,     0,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
     246,     0,     0,   247,   248,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    65,
      66,    67,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   249,     0,   250,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,     0,     0,
       0,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,     0,     0,     0,     0,     0,   166,   167,   168,   169,
     170,   171,   172,   173,    36,    37,   174,    39,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   175,   176,   177,   178,   179,   180,   181,   182,   183,
       0,     0,   184,   185,     0,     0,     0,     0,   186,   187,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,     0,   202,   203,     0,     0,
       0,     0,     0,     0,   204,   205,  -577,  -577,  -577,  -577,
    -577,  -577,  -577,  -577,  -577,     0,     0,     0,     0,     0,
       0,     0,  -577,     0,  -577,  -577,  -577,  -577,     0,  -577,
       0,     0,     0,  -577,  -577,  -577,  -577,  -577,  -577,  -577,
       0,     0,  -577,     0,     0,     0,     0,     0,     0,     0,
       0,  -577,  -577,  -577,  -577,  -577,  -577,  -577,  -577,  -577,
       0,  -577,  -577,  -577,     0,     0,  -577,     0,     0,  -577,
    -577,     0,  -577,  -577,  -577,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -577,  -577,
       0,     0,     0,     0,     0,  -577,     0,     0,  -577,  -577,
       0,  -577,  -577,     0,  -577,  -577,  -577,  -577,     0,  -577,
    -577,  -577,  -577,  -577,  -577,     0,     0,  -577,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -577,  -577,  -577,
       0,  -577,     0,     0,     0,     0,     0,  -577,  -578,  -578,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,     0,     0,     0,
       0,     0,     0,     0,  -578,     0,  -578,  -578,  -578,  -578,
       0,  -578,     0,     0,     0,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,     0,     0,  -578,     0,     0,     0,     0,     0,
       0,     0,     0,  -578,  -578,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,     0,  -578,  -578,  -578,     0,     0,  -578,     0,
       0,  -578,  -578,     0,  -578,  -578,  -578,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -578,  -578,     0,     0,     0,     0,     0,  -578,     0,     0,
    -578,  -578,     0,  -578,  -578,     0,  -578,  -578,  -578,  -578,
       0,  -578,  -578,  -578,  -578,  -578,  -578,     0,     0,  -578,
       0,     0,     0,     0,     0,     0,  -580,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,     0,     0,     0,     0,  -578,
    -578,  -578,  -580,  -578,  -580,  -580,  -580,  -580,     0,  -578,
       0,     0,     0,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
       0,     0,  -580,     0,     0,     0,     0,     0,     0,     0,
       0,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
       0,  -580,  -580,  -580,     0,     0,  -580,     0,     0,  -580,
    -580,     0,  -580,  -580,  -580,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -580,  -580,
       0,     0,     0,     0,     0,  -580,   819,     0,  -580,  -580,
       0,  -580,  -580,     0,  -580,  -580,  -580,  -580,     0,  -580,
    -580,  -580,  -580,  -580,  -580,     0,     0,  -580,     0,     0,
       0,     0,     0,     0,  -107,  -581,  -581,  -581,  -581,  -581,
    -581,  -581,  -581,  -581,     0,     0,     0,  -580,  -580,  -580,
       0,  -581,     0,  -581,  -581,  -581,  -581,  -580,     0,     0,
       0,     0,  -581,  -581,  -581,  -581,  -581,  -581,  -581,     0,
       0,  -581,     0,     0,     0,     0,     0,     0,     0,     0,
    -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,     0,
    -581,  -581,  -581,     0,     0,  -581,     0,     0,  -581,  -581,
       0,  -581,  -581,  -581,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -581,  -581,     0,
       0,     0,     0,     0,  -581,   820,     0,  -581,  -581,     0,
    -581,  -581,     0,  -581,  -581,  -581,  -581,     0,  -581,  -581,
    -581,  -581,  -581,  -581,     0,     0,  -581,     0,     0,     0,
       0,     0,     0,  -109,  -582,  -582,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,     0,     0,     0,  -581,  -581,  -581,     0,
    -582,     0,  -582,  -582,  -582,  -582,  -581,     0,     0,     0,
       0,  -582,  -582,  -582,  -582,  -582,  -582,  -582,     0,     0,
    -582,     0,     0,     0,     0,     0,     0,     0,     0,  -582,
    -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,     0,  -582,
    -582,  -582,     0,     0,  -582,     0,     0,  -582,  -582,     0,
    -582,  -582,  -582,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -582,  -582,     0,     0,
       0,     0,     0,  -582,     0,     0,  -582,  -582,     0,  -582,
    -582,     0,  -582,  -582,  -582,  -582,     0,  -582,  -582,  -582,
    -582,  -582,  -582,     0,     0,  -582,     0,     0,     0,     0,
       0,     0,  -583,  -583,  -583,  -583,  -583,  -583,  -583,  -583,
    -583,     0,     0,     0,     0,  -582,  -582,  -582,  -583,     0,
    -583,  -583,  -583,  -583,     0,  -582,     0,     0,     0,  -583,
    -583,  -583,  -583,  -583,  -583,  -583,     0,     0,  -583,     0,
       0,     0,     0,     0,     0,     0,     0,  -583,  -583,  -583,
    -583,  -583,  -583,  -583,  -583,  -583,     0,  -583,  -583,  -583,
       0,     0,  -583,     0,     0,  -583,  -583,     0,  -583,  -583,
    -583,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -583,  -583,     0,     0,     0,     0,
       0,  -583,     0,     0,  -583,  -583,     0,  -583,  -583,     0,
    -583,  -583,  -583,  -583,     0,  -583,  -583,  -583,  -583,  -583,
    -583,     0,     0,  -583,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -583,  -583,  -583,     0,     0,     0,     0,
       0,     0,     0,  -583,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,   155,     0,     0,
       0,   156,   157,   158,   232,   233,   234,   235,   163,   164,
     165,     0,     0,     0,     0,     0,   166,   167,   168,   236,
     237,   238,   239,   173,   319,   320,   240,   321,     0,     0,
       0,     0,     0,     0,   322,     0,     0,     0,     0,     0,
       0,   175,   176,   177,   178,   179,   180,   181,   182,   183,
       0,     0,   184,   185,     0,     0,     0,     0,   186,   187,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,     0,     0,     0,     0,     0,     0,
       0,   323,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,     0,   202,   203,     0,     0,
       0,     0,     0,     0,   204,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,   155,     0,
       0,     0,   156,   157,   158,   232,   233,   234,   235,   163,
     164,   165,     0,     0,     0,     0,     0,   166,   167,   168,
     236,   237,   238,   239,   173,   319,   320,   240,   321,     0,
       0,     0,     0,     0,     0,   322,     0,     0,     0,     0,
       0,     0,   175,   176,   177,   178,   179,   180,   181,   182,
     183,     0,     0,   184,   185,     0,     0,     0,     0,   186,
     187,   188,   189,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   190,   191,     0,     0,     0,     0,     0,
       0,     0,   481,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   192,   193,   194,   195,
     196,   197,   198,   199,   200,   201,     0,   202,   203,     0,
       0,     0,     0,     0,     0,   204,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,   155,
       0,     0,     0,   156,   157,   158,   232,   233,   234,   235,
     163,   164,   165,     0,     0,     0,     0,     0,   166,   167,
     168,   236,   237,   238,   239,   173,     0,     0,   240,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   175,   176,   177,   178,   179,   180,   181,
     182,   183,     0,     0,   184,   185,     0,     0,     0,     0,
     186,   187,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,     0,     0,     0,   241,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,     0,   202,   203,
       0,     0,     0,     0,     0,     0,   204,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,     0,     0,     0,   156,   157,   158,   232,   233,   234,
     235,   163,   164,   165,     0,     0,     0,     0,     0,   166,
     167,   168,   236,   237,   238,   239,   173,     0,     0,   240,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   175,   176,   177,   178,   179,   180,
     181,   182,   183,     0,     0,   184,   185,     0,     0,     0,
       0,   186,   187,   188,   189,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   190,   191,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   192,   193,
     194,   195,   196,   197,   198,   199,   200,   201,     0,   202,
     203,     0,     0,     0,     0,     0,     0,   204,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,     0,   109,   110,    18,    19,
       0,     0,     0,     0,     0,   111,   112,   113,    23,    24,
      25,    26,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   117,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   312,     0,     0,
     120,    53,     0,    54,    55,     0,     0,     0,     0,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,   121,
     109,   110,    18,    19,     0,     0,     0,   313,     0,   111,
     112,   113,    23,    24,    25,    26,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,   117,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   312,     0,     0,   120,    53,     0,    54,    55,     0,
       0,     0,     0,    57,     0,    58,    59,    60,    61,    62,
      63,     0,     0,    64,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,     0,     0,
       0,     0,    15,   121,    16,    17,    18,    19,     0,     0,
       0,   603,     0,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,    51,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    65,    66,    67,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   259,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   503,    49,    50,     0,     0,
       0,     0,     0,   212,     0,     0,   120,    53,     0,    54,
      55,     0,   260,   261,   262,    57,     0,    58,    59,    60,
      61,    62,    63,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,     0,    65,   263,    67,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,    51,     0,     0,    52,    53,     0,    54,    55,     0,
      56,     0,     0,    57,     0,    58,    59,    60,    61,    62,
      63,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    65,    66,    67,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,   114,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   259,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,   212,
       0,     0,   120,    53,     0,    54,    55,     0,   260,   261,
     262,    57,     0,    58,    59,    60,    61,    62,    63,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    65,   263,    67,    15,     0,   109,   110,    18,    19,
       0,     0,     0,     0,     0,   111,   112,   113,    23,    24,
      25,    26,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   259,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   212,     0,     0,
     120,    53,     0,    54,    55,     0,   713,   261,   262,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    65,
     263,    67,    15,     0,   109,   110,    18,    19,     0,     0,
       0,     0,     0,   111,   112,   113,    23,    24,    25,    26,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,   259,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   212,     0,     0,   120,    53,
       0,    54,    55,     0,   260,   261,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    65,   263,    67,
      15,     0,   109,   110,    18,    19,     0,     0,     0,     0,
       0,   111,   112,   113,    23,    24,    25,    26,     0,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   259,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,   212,     0,     0,   120,    53,     0,    54,
      55,     0,     0,   261,   262,    57,     0,    58,    59,    60,
      61,    62,    63,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    65,   263,    67,    15,     0,
     109,   110,    18,    19,     0,     0,     0,     0,     0,   111,
     112,   113,    23,    24,    25,    26,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
     259,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,   212,     0,     0,   120,    53,     0,    54,    55,     0,
     713,   261,     0,    57,     0,    58,    59,    60,    61,    62,
      63,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    65,   263,    67,    15,     0,   109,   110,
      18,    19,     0,     0,     0,     0,     0,   111,   112,   113,
      23,    24,    25,    26,     0,     0,   114,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   259,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,   212,
       0,     0,   120,    53,     0,    54,    55,     0,     0,   261,
       0,    57,     0,    58,    59,    60,    61,    62,    63,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    65,   263,    67,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   212,     0,     0,
     120,    53,     0,    54,    55,     0,   597,     0,     0,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    65,
     263,    67,    15,     0,   109,   110,    18,    19,     0,     0,
       0,     0,     0,   111,   112,   113,    23,    24,    25,    26,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   212,     0,     0,   120,    53,
       0,    54,    55,     0,   260,     0,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    65,   263,    67,
      15,     0,   109,   110,    18,    19,     0,     0,     0,     0,
       0,   111,   112,   113,    23,    24,    25,    26,     0,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,   212,     0,     0,   120,    53,     0,    54,
      55,     0,   597,     0,     0,    57,     0,    58,    59,    60,
      61,    62,    63,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    65,   263,    67,    15,     0,
     109,   110,    18,    19,     0,     0,     0,     0,     0,   111,
     112,   113,    23,    24,    25,    26,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,   212,     0,     0,   120,    53,     0,    54,    55,     0,
     878,     0,     0,    57,     0,    58,    59,    60,    61,    62,
      63,     0,     0,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    65,   263,    67,    15,     0,   109,   110,
      18,    19,     0,     0,     0,     0,     0,   111,   112,   113,
      23,    24,    25,    26,     0,     0,   114,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,   212,
       0,     0,   120,    53,     0,    54,    55,     0,   713,     0,
       0,    57,     0,    58,    59,    60,    61,    62,    63,     0,
       0,    64,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    65,   263,    67,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   212,     0,     0,
     120,    53,     0,    54,    55,     0,     0,     0,     0,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    65,
      66,    67,    15,     0,   109,   110,    18,    19,     0,     0,
       0,     0,     0,   111,   112,   113,    23,    24,    25,    26,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   212,     0,     0,   120,    53,
       0,    54,    55,     0,     0,     0,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    65,   263,    67,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,   212,     0,     0,   120,    53,     0,    54,
      55,     0,     0,     0,     0,    57,     0,    58,    59,    60,
      61,    62,    63,     0,     0,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    65,   263,    67,    15,     0,
     109,   110,    18,    19,     0,     0,     0,     0,     0,   111,
     112,   113,    23,    24,    25,    26,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
     115,    35,    36,    37,   116,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,   117,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,   119,     0,     0,   120,    53,     0,    54,    55,     0,
       0,     0,     0,    57,     0,    58,    59,    60,    61,    62,
      63,     0,     0,    64,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,   121,   109,   110,    18,    19,     0,     0,
       0,     0,     0,   111,   112,   113,    23,    24,    25,    26,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,   224,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   225,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   121,   109,   110,
      18,    19,     0,     0,     0,     0,     0,   111,   112,   113,
      23,    24,    25,    26,     0,     0,   114,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,   117,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   312,
       0,     0,   396,    53,     0,    54,    55,     0,   397,     0,
       0,    57,     0,    58,    59,    60,    61,    62,    63,     0,
       0,    64,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,     0,     0,     0,
      15,   121,   109,   110,    18,    19,     0,     0,     0,     0,
       0,   111,   112,   113,    23,    24,    25,    26,     0,     0,
     114,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   115,    35,    36,    37,   116,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
     117,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   119,     0,     0,   120,    53,     0,    54,
      55,     0,     0,     0,     0,    57,     0,    58,    59,    60,
      61,    62,    63,     0,     0,    64,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   121,   109,   110,    18,    19,
       0,     0,     0,     0,     0,   111,   112,   113,    23,    24,
      25,    26,     0,     0,   114,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   117,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   312,     0,     0,
     396,    53,     0,    54,    55,     0,     0,     0,     0,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,   121,
     109,   110,    18,    19,     0,     0,     0,     0,     0,   111,
     112,   113,    23,    24,    25,    26,     0,     0,   114,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,   117,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   941,     0,     0,   120,    53,     0,    54,    55,     0,
       0,     0,     0,    57,     0,    58,    59,    60,    61,    62,
      63,     0,     0,    64,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,   121,   109,   110,    18,    19,     0,     0,
       0,     0,     0,   111,   112,   113,    23,    24,    25,    26,
       0,     0,   114,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,   224,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   964,     0,     0,   120,    53,
       0,    54,    55,     0,   651,   652,     0,    57,   653,    58,
      59,    60,    61,    62,    63,     0,     0,    64,     0,     0,
       0,     0,     0,   175,   176,   177,   178,   179,   180,   181,
     182,   183,     0,     0,   184,   185,     0,   121,     0,     0,
     186,   187,   188,   189,     0,     0,     0,   400,  -608,  -608,
    -608,  -608,   405,   406,   190,   191,  -608,  -608,     0,     0,
       0,     0,     0,     0,   413,   414,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,     0,   202,   203,
     672,   644,     0,     0,   673,     0,   204,   274,     0,   417,
     418,   419,   420,   421,   422,   423,   424,   425,   426,   175,
     176,   177,   178,   179,   180,   181,   182,   183,     0,     0,
     184,   185,     0,     0,     0,     0,   186,   187,   188,   189,
       0,     0,     0,   400,   401,   402,   403,   404,   405,   406,
     190,   191,   409,   410,     0,     0,     0,     0,     0,     0,
     413,   414,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   192,   193,   194,   195,   196,   197,   198,
     199,   200,   201,     0,   202,   203,   657,   652,     0,     0,
     658,     0,   204,   274,     0,   417,   418,   419,   420,   421,
     422,   423,   424,   425,   426,   175,   176,   177,   178,   179,
     180,   181,   182,   183,     0,     0,   184,   185,     0,     0,
       0,     0,   186,   187,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   192,
     193,   194,   195,   196,   197,   198,   199,   200,   201,     0,
     202,   203,   687,   644,     0,     0,   688,     0,   204,   274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   175,   176,   177,   178,   179,   180,   181,   182,   183,
       0,     0,   184,   185,     0,     0,     0,     0,   186,   187,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,     0,   202,   203,   690,   652,
       0,     0,   691,     0,   204,   274,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   175,   176,   177,
     178,   179,   180,   181,   182,   183,     0,     0,   184,   185,
       0,     0,     0,     0,   186,   187,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190,   191,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,     0,   202,   203,   697,   644,     0,     0,   698,     0,
     204,   274,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   175,   176,   177,   178,   179,   180,   181,
     182,   183,     0,     0,   184,   185,     0,     0,     0,     0,
     186,   187,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,     0,   202,   203,
     700,   652,     0,     0,   701,     0,   204,   274,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   175,
     176,   177,   178,   179,   180,   181,   182,   183,     0,     0,
     184,   185,     0,     0,     0,     0,   186,   187,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   192,   193,   194,   195,   196,   197,   198,
     199,   200,   201,     0,   202,   203,   736,   644,     0,     0,
     737,     0,   204,   274,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   175,   176,   177,   178,   179,
     180,   181,   182,   183,     0,     0,   184,   185,     0,     0,
       0,     0,   186,   187,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   192,
     193,   194,   195,   196,   197,   198,   199,   200,   201,     0,
     202,   203,   739,   652,     0,     0,   740,     0,   204,   274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   175,   176,   177,   178,   179,   180,   181,   182,   183,
       0,     0,   184,   185,     0,     0,     0,     0,   186,   187,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,     0,   202,   203,   883,   644,
       0,     0,   884,     0,   204,   274,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   175,   176,   177,
     178,   179,   180,   181,   182,   183,     0,     0,   184,   185,
       0,     0,     0,     0,   186,   187,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190,   191,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,     0,   202,   203,   886,   652,     0,     0,   887,     0,
     204,   274,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   175,   176,   177,   178,   179,   180,   181,
     182,   183,     0,     0,   184,   185,     0,     0,     0,     0,
     186,   187,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,     0,   202,   203,
    1023,   644,     0,     0,  1024,     0,   204,   274,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   175,
     176,   177,   178,   179,   180,   181,   182,   183,     0,     0,
     184,   185,     0,     0,     0,     0,   186,   187,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   192,   193,   194,   195,   196,   197,   198,
     199,   200,   201,     0,   202,   203,  1035,   644,     0,     0,
    1036,     0,   204,   274,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   175,   176,   177,   178,   179,
     180,   181,   182,   183,     0,     0,   184,   185,     0,     0,
       0,     0,   186,   187,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   192,
     193,   194,   195,   196,   197,   198,   199,   200,   201,     0,
     202,   203,  1038,   652,     0,     0,  1039,     0,   204,   274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   175,   176,   177,   178,   179,   180,   181,   182,   183,
       0,     0,   184,   185,     0,     0,     0,     0,   186,   187,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,     0,   202,   203,   657,   652,
       0,     0,   658,     0,   204,   274,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   175,   176,   177,
     178,   179,   180,   181,   182,   183,     0,     0,   184,   185,
       0,     0,     0,     0,   186,   187,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190,   191,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   852,     0,     0,     0,     0,     0,     0,
       0,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,     0,   202,   203,   863,     0,     0,     0,     0,     0,
     204,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,     0,     0,     0,     0,   413,   414,
       0,     0,   400,   401,   402,   403,   404,   405,   406,   407,
     408,   409,   410,   411,   412,     0,     0,     0,     0,   413,
     414,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   416,     0,   417,   418,   419,   420,   421,   422,   423,
     424,   425,   426,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   416,     0,   417,   418,   419,   420,   421,   422,
     423,   424,   425,   426,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,     0,     0,     0,
       0,   413,   414,     0,     0,     0,     0,     0,     0,   400,
     401,   402,   403,   404,   405,   406,   407,   408,   409,   410,
     411,   412,     0,     0,     0,     0,   413,   414,     0,     0,
       0,     0,     0,     0,   416,     0,   417,   418,   419,   420,
     421,   422,   423,   424,   425,   426,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   416,
     250,   417,   418,   419,   420,   421,   422,   423,   424,   425,
     426,     0,     0,     0,     0,     0,     0,     0,     0,  -276,
     400,   401,   402,   403,   404,   405,   406,   407,   408,   409,
     410,   411,   412,     0,     0,     0,     0,   413,   414,   400,
     401,   402,   403,   404,   405,   406,   407,   408,   409,   410,
     411,   412,     0,     0,     0,     0,   413,   414,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     416,     0,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,   416,
    -277,   417,   418,   419,   420,   421,   422,   423,   424,   425,
     426,     0,     0,     0,     0,     0,     0,     0,     0,  -278,
     400,   401,   402,   403,   404,   405,   406,   407,   408,   409,
     410,   411,   412,     0,     0,     0,     0,   413,   414,   400,
     401,   402,   403,   404,   405,   406,   407,   408,   409,   410,
     411,   412,     0,     0,     0,     0,   413,   414,     0,     0,
       0,   415,     0,     0,     0,     0,     0,     0,     0,     0,
     416,     0,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,     0,     0,     0,     0,     0,     0,     0,   416,
    -279,   417,   418,   419,   420,   421,   422,   423,   424,   425,
     426,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,     0,     0,     0,     0,   413,   414,
       0,     0,     0,   495,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,     0,     0,     0,
       0,   413,   414,     0,     0,     0,     0,     0,     0,     0,
       0,   416,     0,   417,   418,   419,   420,   421,   422,   423,
     424,   425,   426,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   416,     0,   417,   418,   419,   420,
     421,   422,   423,   424,   425,   426,   400,   401,   402,   403,
     404,   405,   406,   407,   408,   409,   410,  -608,  -608,     0,
       0,     0,     0,   413,   414,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   417,   418,
     419,   420,   421,   422,   423,   424,   425,   426
};

static const yytype_int16 yycheck[] =
{
       2,    90,   221,     7,   270,    88,    89,    16,    17,    27,
       2,    20,     4,     5,     6,    22,    27,     9,    10,    21,
      28,    13,     4,    15,    16,    17,    13,    83,    20,     5,
       6,    15,   484,    14,   119,   399,    66,    13,   330,    56,
     502,   588,     9,    10,     7,    54,    55,    28,    15,   399,
     313,    14,    54,    55,    58,   749,    69,    52,   475,   490,
      52,    16,    16,    17,    56,    28,    20,   376,    16,    17,
     532,   772,    20,    26,    66,   439,   440,    21,    22,   295,
      56,   683,   306,   660,   661,    69,   310,   439,   440,   429,
     692,    83,   318,    10,   927,    58,    26,   371,    15,   106,
      75,    76,   540,     0,    29,    91,    54,    83,    16,    17,
     112,   498,    20,   949,   454,   502,     2,   391,     4,    51,
      91,    80,    60,    61,    62,    63,   118,    91,   120,   469,
      57,    57,    55,    91,   217,   138,   142,   268,   478,   270,
      72,    16,   148,   369,    72,   228,    28,   122,   123,     5,
       6,   118,    76,   585,    18,    61,    20,    13,    64,    65,
     115,   147,   106,   118,   119,   603,   144,   126,   112,   113,
     102,   103,   331,    91,   305,   334,   147,   336,    91,   338,
     396,   340,   456,   147,  1017,   138,   130,   142,   113,   147,
    1026,   146,   105,   148,   121,    94,   128,   123,   490,   123,
      56,   121,   542,   131,   132,   133,   289,    91,   138,    16,
     116,   117,    16,    17,   144,    93,    20,   121,    51,   211,
      53,    54,    55,    56,   801,   585,    93,    83,   588,   147,
     222,   223,   315,    91,   147,   103,    69,   241,   932,   142,
     115,   507,    91,   118,   119,   946,    60,   791,   949,    63,
      54,    55,   796,   144,   121,   222,   223,   313,   550,   125,
     128,   723,    55,   147,   121,    93,   268,   251,   270,   278,
      91,   146,   274,   148,   121,    91,   427,   540,   241,    55,
     431,   712,   274,   434,   297,   142,   278,    72,    92,   147,
     282,   283,   305,   306,    25,   287,   110,   310,   147,    93,
     295,    93,   294,   295,   455,   222,   223,   854,   115,   142,
     302,   118,   119,   297,    20,   282,   283,   468,   544,   470,
     274,   313,   924,    52,   278,  1026,   147,   121,   479,   121,
     278,   147,   682,   142,   144,   722,   723,   313,   148,   146,
     603,   148,   780,   318,   129,   130,   131,   132,   133,   115,
     142,   142,   118,   119,    57,   347,   348,   349,   350,   351,
     352,   353,   354,   355,   394,   347,   353,   518,   453,   399,
     278,   138,   374,    72,   376,   506,   507,   353,    93,   371,
     397,   348,   349,   350,   351,   323,    93,   354,   355,   548,
     145,   120,   543,   121,   369,   141,    51,   671,   139,   391,
      55,   396,   394,    55,   396,   397,   121,   399,   294,   439,
     440,   828,   772,    72,   121,   142,   302,   709,   782,   851,
     712,   397,    72,   415,   788,    93,   858,   429,   415,   793,
     794,   348,   349,   350,   351,   142,   788,   429,   121,   415,
      61,   793,   794,    64,    65,   785,   394,   439,   440,   758,
     716,   399,   454,   121,   142,    93,    51,   313,    58,    59,
     462,   347,   454,   925,   456,   457,   352,   469,   431,   600,
     274,   434,   924,   465,   278,   429,   478,   469,   496,   142,
      93,   473,   538,   121,   540,   496,   478,   431,   707,   498,
     457,   483,   455,   142,   854,   116,   117,   353,    16,   773,
     454,    37,    38,    26,   506,   507,   514,   470,   121,   733,
     142,   455,    51,   515,   946,   469,   479,   780,    58,    59,
     529,   505,   142,   515,   478,   799,   470,   121,     5,     6,
     613,   895,   524,   514,    51,   479,    13,   142,   925,   487,
     542,   397,   834,   895,    93,    93,   538,   603,   540,    93,
     542,   514,    27,    91,   100,   518,    93,   549,   287,   415,
     869,   870,   538,   385,   540,   387,   295,   105,    91,   544,
     844,   837,   121,   121,   518,    15,   707,   121,    13,    56,
     543,    63,   105,    16,   121,   716,   946,  1019,   542,   949,
      17,    18,    15,    62,   596,    64,    65,   115,  1030,   543,
     118,   119,   140,    93,   142,     2,    83,     4,   139,   147,
     145,   603,     9,    10,   632,   138,   145,   140,    15,    16,
      17,   632,   142,    20,   147,   429,    15,   603,   146,   515,
     148,   121,   906,   784,    15,   786,   142,    91,   524,    44,
     115,   648,   656,   118,   119,   659,   141,   116,   117,   656,
     454,   105,   659,    26,   656,    52,   121,   659,   660,   661,
     299,    37,    38,   677,   303,   469,  1026,   396,   141,    66,
      15,   146,  1012,   148,   478,   677,   680,    18,   670,   671,
     682,   683,   538,   685,   540,   141,   140,   139,   142,   998,
     692,   910,   141,   147,   498,   703,    15,   916,   502,    93,
     139,    93,   148,   670,   648,   141,   837,   139,   797,    57,
     702,   142,   656,   722,   716,   659,   142,   680,    91,    15,
     733,   118,   703,   120,   780,   529,    26,   121,   532,   121,
     741,   675,   105,   677,   682,   702,   465,    94,   542,   115,
     703,   142,   118,   119,   473,   865,   866,   603,   142,   142,
     142,    93,   782,   670,   483,   142,   758,   115,   788,   789,
     118,   119,    91,   793,   794,   138,    14,   140,    15,   142,
     146,   144,   148,   214,   147,    62,   105,    64,    65,   121,
     221,   773,    15,   785,   145,   702,    75,    76,   780,   781,
     782,    91,   146,   785,    15,   142,   788,   789,   142,   801,
     142,   793,   794,   572,   780,   105,   578,   799,   800,   811,
     582,   140,   814,   142,   211,   144,   142,   258,   147,   588,
     549,   813,   591,   786,   816,   222,   223,   142,   142,   116,
     117,   785,   141,   825,   826,   837,   313,    15,   138,    15,
     140,   833,   786,    15,   144,    91,   139,   147,    15,   816,
     142,    93,   844,   845,   865,   866,   139,   126,   126,   105,
      55,   139,  1013,    15,   503,   895,    26,   869,   870,    55,
     578,   510,   142,    15,    91,   867,   353,   274,   142,   121,
     872,   278,   521,   966,   142,   282,   283,   142,   105,   142,
     287,   867,   142,   144,   140,   781,   872,   294,   295,   816,
     142,   147,   141,   895,   144,   302,   347,    26,   142,   515,
      26,     6,  1015,   905,   906,   439,   440,   909,   722,   723,
     397,   913,   924,   140,   780,   142,   772,    26,   798,   772,
     147,    91,  1014,   909,   573,   574,  1017,   378,   415,     7,
     762,   763,   764,   253,   766,   105,   768,   471,   472,   578,
     347,   348,   349,   350,   351,   352,   946,   354,   355,    91,
     943,   979,   585,   602,   269,    -1,    -1,    -1,   979,    -1,
      -1,    -1,    91,   105,   371,    91,    -1,   969,   138,   971,
     140,   785,   974,    -1,   144,    -1,   105,   147,    -1,   105,
      -1,    -1,    91,    -1,   391,   519,   998,   394,    -1,   396,
     772,    -1,   399,   772,    -1,    -1,   105,    -1,   140,    -1,
    1012,   867,  1014,  1015,   115,   147,   872,   118,   119,   138,
    1012,   140,   138,   142,   140,   144,   142,    -1,   147,    -1,
      26,   147,   429,   474,   475,    -1,    -1,    -1,    -1,   138,
     679,   140,   439,   440,    -1,   144,    -1,   148,   147,    -1,
    1013,    -1,    26,   909,    -1,    -1,    -1,   454,  1012,   456,
     457,   538,    -1,   540,   772,    -1,    -1,    -1,   465,  1013,
      -1,   800,   469,    -1,   115,    -1,   473,   118,   119,    -1,
      91,   478,    -1,   524,   813,   854,   483,   856,    -1,    26,
     531,   860,    -1,   732,   105,    91,   825,   826,    -1,    -1,
     943,   142,    -1,   946,   833,   146,   949,   148,   951,   105,
      -1,   750,   934,   935,   936,   937,   845,    91,   515,    -1,
      51,   925,    53,    54,    55,    56,   603,   524,    -1,   140,
      -1,   105,    -1,   144,    -1,    -1,   147,    -1,    69,    91,
      -1,    -1,   138,    -1,   140,   542,   142,    -1,   144,    -1,
      -1,   147,   549,   105,    91,    -1,   999,    -1,    -1,    -1,
      -1,   930,   931,    -1,   138,    -1,   140,    91,   105,    -1,
     144,   943,    -1,   147,   946,    -1,   905,   949,    -1,   951,
     949,   105,   951,  1026,   913,  1028,    -1,  1030,   140,  1032,
      91,     9,    10,    -1,    -1,   147,    -1,    15,    16,    17,
    1022,   138,    20,   140,   105,    -1,    -1,   144,  1012,  1052,
     147,   142,    91,    -1,    -1,    -1,   140,   986,    -1,    -1,
     989,    -1,    -1,   147,    -1,    -1,   105,   999,   669,    47,
      48,    49,    50,    91,    -1,   943,    54,    55,   946,   140,
     969,   949,   971,   951,    -1,   974,   147,   105,    66,    67,
      -1,    -1,  1021,    -1,  1026,    -1,  1028,  1026,  1030,  1028,
    1032,   140,    -1,  1032,   788,   789,   707,    91,   147,   793,
     794,    -1,    91,   670,   671,    -1,    91,    -1,    -1,    -1,
    1052,   105,   140,  1052,    -1,    -1,   105,    -1,    -1,   147,
     105,   999,    91,    -1,    91,   819,   820,    72,   822,   823,
     118,    -1,    -1,   780,    -1,   702,   105,   115,   105,    72,
     118,   119,    -1,    -1,    89,    90,   140,    -1,  1026,    -1,
    1028,   140,  1030,   147,  1032,   140,    89,    90,   147,   874,
     875,    51,   147,    53,    54,    55,    56,    91,   146,    -1,
     148,   140,    -1,   140,  1052,    63,    64,    65,   147,    69,
     147,   105,    72,   128,   129,   130,   131,   132,   133,    63,
      64,    65,   803,   126,   127,   128,   129,   130,   131,   132,
     133,   895,    63,    64,    65,    95,   773,    63,    64,    65,
      -1,   101,   102,   103,   781,   782,   140,   828,   785,    -1,
     867,   788,   789,   147,   918,   872,   793,   794,   116,   117,
      -1,   946,   799,   800,   222,   223,    -1,    -1,   128,    -1,
      -1,   131,   116,   117,    -1,    -1,   813,    -1,    -1,   816,
      72,    -1,    -1,    -1,   144,   116,   117,    91,   825,   826,
     116,   117,   909,    -1,    -1,    -1,   833,    89,    90,    -1,
      -1,   105,   260,   261,   262,   263,    -1,   844,   845,    -1,
      -1,    -1,    63,    64,    65,    -1,   274,  1002,  1003,  1004,
     278,  1006,  1007,    -1,   282,   283,    -1,    -1,    51,   910,
      53,    54,    55,    56,    -1,   916,   140,   129,   130,   131,
     132,   133,    -1,   147,    -1,    -1,    69,    -1,    -1,    72,
      63,    64,    65,    63,    64,    65,    -1,    -1,   895,    -1,
      -1,  1046,  1047,  1048,  1049,   116,   117,    -1,   905,   906,
      -1,  1056,    95,    63,    64,    65,   913,    -1,   101,   102,
     103,    -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,
     348,   349,   350,   351,    -1,    -1,   354,   355,    -1,    -1,
      -1,    -1,    69,   116,   117,   128,   116,   117,   131,    -1,
      -1,    -1,    -1,    51,   372,    53,    54,    55,    56,    -1,
      -1,   144,    -1,    -1,    -1,   383,   116,   117,    95,    -1,
      -1,    69,   969,    -1,   971,    -1,   394,   974,    -1,    -1,
      -1,   399,   400,   401,   402,   403,   404,   405,   406,   407,
     408,   409,   410,   411,   412,   413,   414,    -1,   416,   417,
     418,   419,   420,   421,   422,   423,   424,   425,   426,    -1,
      -1,   429,    -1,    -1,    51,  1012,    53,    54,    55,    56,
      -1,   439,   440,    -1,    51,    -1,    53,    54,    55,    56,
      -1,    -1,    69,    -1,    -1,    -1,   454,    -1,    -1,   457,
      -1,     2,    69,     4,     5,     6,    -1,    -1,    -1,   467,
      -1,   469,    13,   471,   472,    -1,    -1,    -1,    95,   115,
     478,    -1,   118,   119,   101,    -1,    -1,    -1,    95,   487,
      -1,    -1,    -1,   491,   101,    -1,    -1,   495,    -1,    -1,
     498,    -1,   500,    -1,   502,    -1,    -1,    -1,    -1,   145,
     146,    52,   148,    -1,    51,    56,    53,    54,    55,    56,
      -1,   519,    40,    41,    42,    43,    44,    -1,    -1,    -1,
      -1,   529,    69,    -1,   532,    72,    51,    -1,    53,    54,
      55,    56,    83,    -1,   542,    -1,    -1,    84,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    -1,    -1,    72,    95,    -1,
     558,   559,    -1,    -1,   101,   102,   103,    -1,    -1,    -1,
      -1,    51,    -1,    53,    54,    55,    56,   575,    -1,   120,
      95,    -1,    -1,    -1,    -1,    -1,   101,   102,   103,    69,
      -1,   128,    72,    -1,   131,    -1,   594,    -1,    -1,   597,
       2,    -1,     4,     5,     6,     7,    -1,    -1,    -1,    -1,
      -1,    13,    -1,   128,    -1,    95,   131,    -1,     0,    -1,
      -1,   101,   102,   103,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    -1,    26,    27,    -1,    -1,   128,    -1,
      52,   131,    -1,    -1,    56,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     211,    -1,   670,    -1,    -1,    51,    -1,    53,    54,    55,
      56,    83,    -1,    -1,   682,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    -1,    72,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   702,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    -1,    -1,    -1,   713,    -1,    -1,   120,    95,
      -1,    -1,    -1,   105,   722,   723,   102,   103,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   287,    -1,    -1,    -1,
      -1,    -1,   128,   294,   295,    -1,   138,   139,    -1,    -1,
      -1,   302,   144,   145,   146,   147,   148,    -1,    -1,    -1,
      -1,    -1,   313,    -1,    -1,    -1,    -1,    -1,    -1,   777,
      -1,    -1,    -1,    -1,   782,   783,    -1,   785,    -1,    -1,
     788,   789,    -1,    -1,    -1,   793,   794,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   347,    -1,    -1,   211,
      -1,   352,   353,    -1,    -1,    -1,    -1,    -1,   816,    -1,
      -1,   819,   820,    -1,   822,   823,    -1,    -1,    -1,    -1,
     371,    -1,    -1,    -1,   832,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     391,    -1,    -1,    -1,   852,   396,   397,    -1,   399,    -1,
      -1,    -1,    -1,    -1,    -1,   863,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   415,    -1,    -1,    -1,    -1,    -1,
     878,    -1,    -1,    -1,    -1,   287,    -1,    -1,    -1,    -1,
     888,   889,   294,   295,    -1,    -1,    -1,   895,   439,   440,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   313,     2,    -1,     4,   456,    -1,    -1,    -1,    -1,
     918,    -1,    -1,    13,   465,    -1,    -1,   925,    -1,    -1,
      -1,    -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   483,    -1,    -1,   347,    -1,    -1,    -1,    -1,
     352,   353,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    52,    -1,    -1,    -1,    -1,    -1,    -1,   371,
      -1,    -1,    -1,    -1,   515,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   524,    -1,    -1,    -1,    -1,    -1,   391,
      -1,    -1,    -1,    -1,   396,   397,    -1,   538,    -1,   540,
      -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,   549,    -1,
      -1,    -1,    -1,   415,  1012,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     120,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,
      90,     2,    -1,     4,   456,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   603,   465,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   483,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    52,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   515,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   211,   524,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     671,    -1,    -1,    -1,    -1,    -1,   538,    -1,   540,    -1,
      -1,    51,    52,    -1,    -1,    55,    -1,   549,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    78,   120,
      -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   101,   102,    -1,    -1,    -1,    -1,   287,    -1,    -1,
      -1,   603,    -1,    -1,   294,   295,    -1,    -1,    -1,    -1,
      -1,    -1,   302,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,
      -1,    -1,   773,   143,   144,    -1,    -1,    -1,    -1,   780,
     781,   782,    -1,    -1,    -1,    -1,    -1,   788,    -1,    -1,
      -1,    -1,   793,   794,    -1,    -1,    -1,   347,   799,   800,
     211,    -1,   352,   353,    -1,    -1,    -1,    -1,    -1,   671,
      -1,    -1,   813,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   371,    -1,    -1,   825,   826,    -1,    -1,    -1,    -1,
      -1,    -1,   833,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   391,    -1,   844,   845,    -1,   396,    -1,    -1,   399,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   415,   867,    -1,    -1,    -1,
      -1,   872,    -1,    -1,    -1,    -1,   287,    -1,    -1,    -1,
      -1,    -1,    -1,   294,   295,    -1,    -1,    -1,    -1,   439,
     440,   302,    -1,    -1,   895,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   905,   906,   456,    -1,   909,    -1,
      -1,   773,   913,    -1,    -1,   465,    -1,    -1,   780,   781,
      -1,    -1,    -1,   473,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   483,    -1,    -1,   347,   799,   800,    -1,
      -1,   352,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   813,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     371,    -1,    -1,   825,   826,   515,    -1,    -1,   969,    -1,
     971,   833,    -1,   974,   524,    -1,    -1,    -1,    -1,    -1,
     391,    -1,   844,   845,    -1,   396,    -1,    -1,   399,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   549,
      -1,    -1,    -1,    -1,    -1,   867,    -1,    -1,    -1,    -1,
     872,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,   440,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   905,   906,   456,    -1,   909,    -1,    -1,
      -1,   913,    -1,     0,   465,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   473,    -1,    -1,    -1,    13,    14,    15,    16,
      17,    18,   483,    20,    -1,    -1,    -1,    -1,    -1,    -1,
      27,    28,    29,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      37,    38,    -1,    40,    41,    42,    43,    44,    -1,    -1,
      -1,    -1,    -1,    -1,   515,    -1,    -1,   969,    -1,   971,
      57,    -1,   974,   524,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   671,    -1,    -1,    -1,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,   549,    -1,
      -1,    -1,    89,    90,    91,    -1,    -1,    94,    -1,    -1,
      -1,    -1,    -1,   100,    -1,    -1,    -1,    -1,   105,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   113,    -1,   115,    -1,
      -1,   118,   119,    -1,    -1,   122,   123,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,   139,   140,   141,   142,    -1,    -1,   145,   146,
     147,   148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      -1,    81,    82,   773,    -1,    -1,    -1,    -1,    -1,    89,
      90,   781,   782,    -1,    -1,    -1,    -1,    -1,   788,    -1,
      -1,    -1,    -1,   793,   794,    -1,    -1,    -1,    -1,   799,
     800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     671,    -1,    -1,   813,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   825,   826,    -1,    -1,    -1,
      -1,    -1,    -1,   833,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   844,   845,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   905,   906,    -1,    -1,   909,
      -1,    -1,   773,   913,    -1,    -1,    -1,    -1,    -1,    -1,
     781,   782,    -1,    -1,    -1,    -1,    -1,   788,    -1,    -1,
      -1,    -1,   793,   794,    -1,    -1,    -1,    -1,   799,   800,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   813,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   825,   826,    -1,    -1,    -1,   969,
      -1,   971,   833,    44,   974,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   844,   845,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   895,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   905,   906,    -1,    -1,    -1,    -1,
      -1,   122,   913,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
       0,     1,    -1,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,   969,    39,
     971,    -1,    -1,   974,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,
      -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,
      -1,   101,    -1,    -1,   104,    -1,   106,   107,   108,   109,
     110,   111,     0,    -1,   114,   115,    -1,    -1,   118,   119,
      -1,    -1,    -1,    -1,    -1,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,   134,   135,   136,    -1,    26,    27,
      28,    -1,    -1,    -1,    -1,    -1,   146,    -1,   148,    37,
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
      -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,    91,
      -1,    93,    94,    -1,    -1,    -1,    -1,    -1,   100,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,     0,    -1,    -1,   139,   140,   141,
     142,    -1,    -1,   145,   146,   147,   148,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,
      26,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    37,    38,    -1,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    -1,    -1,    89,    90,    91,    -1,    -1,    94,    -1,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,    -1,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
       0,    -1,   138,   139,   140,   141,   142,    -1,   144,   145,
     146,   147,   148,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    -1,    -1,    27,    28,    -1,
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
     130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,   139,
     140,   141,   142,    -1,   144,   145,   146,   147,   148,    13,
      14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    -1,    26,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,     0,    -1,   138,   139,   140,    -1,   142,    -1,
      -1,   145,   146,   147,   148,    13,    14,    15,    -1,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    26,    -1,
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
     128,   129,   130,   131,   132,   133,    -1,    -1,     0,    -1,
     138,   139,   140,    -1,   142,    -1,    -1,   145,   146,   147,
     148,    13,    14,    15,    -1,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,    91,
      -1,    93,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,   121,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,     0,    -1,    -1,   139,   140,    -1,
     142,    -1,    -1,   145,   146,   147,   148,    13,    14,    15,
      -1,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,   139,   140,    -1,   142,    -1,    -1,   145,
     146,   147,   148,     1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    -1,    -1,
      18,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
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
      12,    -1,    -1,    15,    -1,    17,    18,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,
      96,    -1,    98,    99,    -1,   101,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,    -1,    -1,   114,   115,
      -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,
     136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,     1,   148,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    -1,    -1,    18,    19,
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
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,   134,   135,   136,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,   146,    -1,   148,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,
     101,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,   115,    -1,    -1,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,   135,   136,    -1,    -1,   139,    -1,
      -1,    -1,    -1,    -1,    -1,   146,     1,   148,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    14,
      15,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,   101,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
     115,    -1,    -1,   118,   119,     1,    -1,     3,     4,     5,
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
      -1,    -1,   118,   119,     1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,   134,   135,
     136,    -1,    19,    -1,    21,    22,    23,    24,    -1,   145,
     146,    -1,   148,    30,    31,    32,    33,    34,    35,    36,
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
      -1,    19,    -1,    21,    22,    23,    24,    -1,   145,   146,
      -1,   148,    30,    31,    32,    33,    34,    35,    36,    -1,
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
      -1,   139,    -1,    -1,    -1,    -1,    -1,    -1,   146,     1,
     148,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    15,    -1,    -1,    -1,    19,    -1,    21,
      22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,
      92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,
      -1,    -1,   104,    -1,   106,   107,   108,   109,   110,   111,
      -1,    -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,
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
      -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,   134,   135,   136,    -1,    19,    -1,    21,    22,    23,
      24,    -1,    -1,   146,    -1,   148,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,
      -1,    95,    96,    -1,    98,    99,    -1,   101,    -1,    -1,
     104,    -1,   106,   107,   108,   109,   110,   111,    -1,    -1,
     114,   115,    -1,    -1,   118,   119,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
     134,   135,   136,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,   146,    -1,   148,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
     115,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
     135,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   146,    -1,   148,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    -1,    -1,
      -1,    -1,    -1,    -1,   143,   144,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,    26,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,   101,   102,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,
      -1,   138,    -1,    -1,    -1,    -1,    -1,   144,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    26,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,   101,   102,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,
     135,   136,    19,   138,    21,    22,    23,    24,    -1,   144,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    93,    -1,    95,    96,
      -1,    98,    99,    -1,   101,   102,   103,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,   121,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,   134,   135,   136,
      -1,    19,    -1,    21,    22,    23,    24,   144,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,
      -1,    -1,    -1,    -1,    92,    93,    -1,    95,    96,    -1,
      98,    99,    -1,   101,   102,   103,   104,    -1,   106,   107,
     108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,
      -1,    -1,    -1,   121,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,   134,   135,   136,    -1,
      19,    -1,    21,    22,    23,    24,   144,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,
      99,    -1,   101,   102,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,   144,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,
     101,   102,   103,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   144,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    -1,    -1,
      -1,    -1,    -1,    -1,   143,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    19,    20,    21,    22,    23,    24,    25,    26,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,
      88,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,
      87,    88,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,   106,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      -1,    -1,    -1,    -1,    -1,    -1,   143,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      16,    17,    18,    19,    20,    21,    22,    23,    24,    25,
      26,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    -1,    -1,    55,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,    75,
      76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,
      -1,    87,    88,    89,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,   135,
     136,    -1,    -1,    -1,    -1,    -1,    -1,   143,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,
      21,    22,    23,    24,    -1,    -1,    -1,   142,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   134,    21,    22,    23,    24,    -1,    -1,
      -1,   142,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    86,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,
      99,    -1,   101,   102,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,
     101,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,
      -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,   102,
     103,   104,    -1,   106,   107,   108,   109,   110,   111,    -1,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   134,   135,   136,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,   101,   102,   103,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,
     135,   136,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,   101,   102,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,
      99,    -1,    -1,   102,   103,   104,    -1,   106,   107,   108,
     109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,
     101,   102,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,
      -1,    -1,    95,    96,    -1,    98,    99,    -1,    -1,   102,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,    -1,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   134,   135,   136,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,   101,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,
     135,   136,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,
      99,    -1,   101,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    85,    86,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,
     101,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,   134,   135,   136,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    85,    86,    -1,    -1,    -1,    -1,    -1,    92,
      -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,    -1,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,    -1,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   134,   135,   136,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      85,    86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,
     135,   136,    19,    -1,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,    86,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    85,    86,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,
      99,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,    -1,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    -1,
      -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   134,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,   101,    -1,    -1,   104,    -1,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    19,   134,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,
      -1,    -1,    95,    96,    -1,    98,    99,    -1,   101,    -1,
      -1,   104,    -1,   106,   107,   108,   109,   110,   111,    -1,
      -1,   114,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,   134,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    92,    -1,    -1,    95,    96,    -1,    98,
      99,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
     109,   110,   111,    -1,    -1,   114,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    19,   134,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,
      95,    96,    -1,    98,    99,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,   109,   110,   111,    -1,    -1,   114,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    92,    -1,    -1,    95,    96,    -1,    98,    99,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,   109,   110,
     111,    -1,    -1,   114,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   134,    21,    22,    23,    24,    -1,    -1,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,    96,
      -1,    98,    99,    -1,    51,    52,    -1,   104,    55,   106,
     107,   108,   109,   110,   111,    -1,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,   134,    -1,    -1,
      87,    88,    89,    90,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,   101,   102,    81,    82,    -1,    -1,
      -1,    -1,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
     101,   102,    81,    82,    -1,    -1,    -1,    -1,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    51,    52,    -1,    -1,
      55,    -1,   143,   144,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,
      -1,    -1,    87,    88,    89,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    51,    52,    -1,    -1,    55,    -1,   143,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    51,    52,
      -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    -1,    -1,    87,    88,    89,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,
      87,    88,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    51,    52,    -1,    -1,
      55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,
      -1,    -1,    87,    88,    89,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    51,    52,    -1,    -1,    55,    -1,   143,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    51,    52,
      -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    -1,    -1,    87,    88,    89,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    51,    52,    -1,    -1,    55,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    72,    73,    74,    75,    76,
      77,    78,    -1,    -1,    81,    82,    -1,    -1,    -1,    -1,
      87,    88,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,   102,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      51,    52,    -1,    -1,    55,    -1,   143,   144,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    -1,    -1,
      81,    82,    -1,    -1,    -1,    -1,    87,    88,    89,    90,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    51,    52,    -1,    -1,
      55,    -1,   143,   144,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    -1,    -1,    81,    82,    -1,    -1,
      -1,    -1,    87,    88,    89,    90,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,   102,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
     135,   136,    51,    52,    -1,    -1,    55,    -1,   143,   144,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    78,
      -1,    -1,    81,    82,    -1,    -1,    -1,    -1,    87,    88,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    51,    52,
      -1,    -1,    55,    -1,   143,   144,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    -1,    -1,    81,    82,
      -1,    -1,    -1,    -1,    87,    88,    89,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,   102,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    44,    -1,    -1,    -1,    -1,    -1,
     143,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,
      90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    -1,
      -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    -1,    -1,    89,    90,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     148,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    -1,    -1,    89,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     142,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    -1,    -1,    -1,    -1,    89,    90,    -1,    -1,
      -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,
     142,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,
      -1,    -1,    -1,    94,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    -1,
      -1,    89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133
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
     154,   159,   161,   162,   164,   165,   166,   169,   170,   173,
     174,   176,   177,   178,   180,   181,   190,   204,   221,   242,
     243,   253,   254,   255,   259,   260,   261,   267,   268,   269,
     271,   272,   273,   274,   275,   276,   312,   325,   154,    21,
      22,    30,    31,    32,    39,    51,    55,    69,    89,    92,
      95,   134,   165,   166,   182,   183,   204,   221,   273,   276,
     312,   183,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    45,    46,    47,    48,
      49,    50,    51,    52,    55,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    81,    82,    87,    88,    89,    90,
     101,   102,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   135,   136,   143,   144,   184,   188,   189,   275,
     306,   205,    92,   164,   168,   181,   190,   221,   273,   274,
     276,   168,   211,   213,    69,    92,   174,   181,   221,   226,
     273,   276,    33,    34,    35,    36,    48,    49,    50,    51,
      55,   106,   184,   185,   186,   269,   115,   118,   119,   146,
     148,   168,   263,   264,   265,   318,   322,   323,   324,    51,
     101,   102,   103,   135,   173,   190,   196,   199,   202,   255,
     309,   311,   196,   196,   144,   193,   194,   197,   198,   325,
     193,   197,   144,   319,   323,   185,   155,   138,   190,   221,
     190,   190,   190,    55,     1,    95,   157,   158,   159,   175,
     176,   325,   206,   208,   191,   202,   309,   325,   190,   308,
     309,   325,    92,   142,   180,   221,   273,   276,   209,    53,
      54,    56,    63,   110,   184,   270,    62,    64,    65,   116,
     117,   256,   257,    63,   256,    63,   256,    63,   256,    61,
     256,    58,    59,   169,   190,   190,   318,   324,    40,    41,
      42,    43,    44,    94,    37,    38,    51,    53,    54,    55,
      56,    69,    72,    95,   101,   102,   103,   128,   131,   144,
     279,   280,   281,   282,   285,   286,   287,   288,   290,   291,
     292,   293,   295,   296,   297,   300,   301,   302,   303,   304,
     279,   280,    28,   240,   121,   142,    95,   101,   177,   121,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    89,    90,    94,   122,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    91,   105,   140,
     147,   316,    91,   316,   317,    26,   138,   244,   255,    93,
      93,   193,   197,   244,   164,    51,    55,   182,    58,    59,
     279,   125,   277,    91,   140,   316,   220,   307,    91,   147,
     315,   156,   157,    55,    16,   222,   322,   121,    91,   140,
     316,    93,    93,   222,   168,   168,    55,    91,   140,   316,
      25,   110,   142,   266,   318,   115,   265,    20,   247,   322,
      57,   310,   190,   190,   190,    94,   142,   200,   201,   325,
     310,   200,   201,    84,   195,   196,   202,   309,   325,   196,
     164,   318,   320,   164,   160,   138,   157,    91,   316,    93,
     159,   175,   145,   318,   324,   320,   159,   320,   141,   201,
     321,   324,   201,   321,   139,   321,    55,   177,   178,   179,
     142,    91,   140,   316,   144,   238,   290,    63,   256,   258,
     262,   263,    62,   257,    63,    63,    63,    61,    72,    72,
     154,   168,   168,   168,   168,   159,   174,   181,   164,   164,
      57,   121,   294,    84,   290,   121,   156,   190,   142,   305,
     325,    51,   142,   305,   322,   142,   289,   190,   142,   289,
      51,   142,   289,    51,   121,   156,   241,   101,   169,   190,
     202,   203,   175,   142,   180,   142,   162,   163,   169,   181,
     190,   192,   203,   221,   276,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     174,   181,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,    51,    52,    55,   188,   193,   313,   314,
     195,    51,    52,    55,   188,   193,   313,    51,    55,   313,
     246,   245,   163,   190,   192,   163,   192,   100,   171,   218,
     278,   217,    51,    55,   182,   313,   195,   313,   156,   164,
     167,    15,    13,   249,   325,   157,    16,    51,    55,   195,
      51,    55,   157,    27,   223,   322,   223,    51,    55,   195,
      51,    55,   215,   187,   157,   247,   190,   202,    15,   262,
     190,   190,   319,   101,   190,   199,   309,   190,   311,   320,
     145,   318,   201,   201,   320,   145,   185,   152,   139,   192,
     320,   159,   207,   309,   177,   179,    51,    55,   195,    51,
      55,   290,   210,    63,   157,   263,   190,   190,    51,   101,
     227,   295,   320,   320,   190,    15,    51,   282,   287,   304,
     288,   293,   300,   302,   295,   297,   302,    51,   295,   190,
      15,    80,   126,   232,   233,   325,   190,   201,   320,   179,
     142,    44,   121,    44,    91,   140,   316,   319,    93,    93,
     193,   197,   141,    93,    93,   194,   197,   194,   197,   232,
     232,   172,   322,   168,   156,   141,    15,   320,   184,   190,
     203,   250,   325,    18,   225,   325,    17,   224,   225,    93,
      93,   141,    93,    93,   225,   212,   214,   141,   168,   185,
     139,    15,   201,   222,   262,   190,   200,   309,   139,   320,
     321,   141,   235,   319,    29,   113,   239,   139,   142,   292,
     320,   142,    44,   305,   142,   289,   142,   289,   142,   289,
     142,   289,   289,    44,   229,   231,   234,   281,   283,   284,
     287,   295,   296,   298,   299,   302,   304,   156,   101,   190,
     179,   159,   190,    51,    55,   195,    51,    55,    57,   123,
     163,   192,   169,   192,   171,    93,   163,   192,   163,   192,
     171,   244,   240,   156,   157,   232,   219,   322,    15,    94,
     251,   325,   157,    14,   252,   325,   168,    15,    93,    15,
     157,   157,   223,   190,   157,   201,   145,   146,   156,   157,
     228,   142,   101,   190,   295,   302,   295,   295,   190,   235,
     235,    92,   221,   142,   305,   305,   142,   230,   221,   142,
     230,   142,   230,    15,   190,   141,   190,   190,   163,   192,
      15,   139,   157,   156,    92,   181,   221,   273,   276,   222,
     157,   222,    15,    15,   216,   225,   247,   248,    51,   236,
     237,   291,    15,   139,   295,   295,   142,   292,   289,   142,
     289,   289,   289,   126,   126,    55,    91,   283,   287,   142,
     229,   230,   299,   302,   295,   298,   302,   295,   139,    15,
      55,    91,   140,   316,   157,   157,   157,   142,   319,   142,
     295,   142,   295,    51,    55,   305,   142,   230,   142,   230,
     142,   230,   142,   230,   230,    51,    55,   195,    51,    55,
     249,   224,    15,   237,   295,   289,   295,   302,   295,   295,
     141,   230,   142,   230,   230,   230,   295,   230
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   149,   151,   150,   152,   153,   153,   153,   153,   154,
     155,   154,   156,   157,   158,   158,   158,   158,   160,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   159,   161,   161,   161,   161,   162,   162,
     162,   162,   162,   162,   162,   162,   163,   163,   163,   164,
     164,   164,   164,   164,   164,   165,   167,   166,   168,   169,
     169,   170,   170,   172,   171,   173,   173,   173,   173,   173,
     173,   173,   173,   173,   173,   173,   174,   174,   175,   175,
     176,   176,   176,   176,   176,   176,   176,   176,   176,   176,
     177,   177,   178,   178,   179,   179,   180,   180,   180,   180,
     180,   180,   180,   180,   181,   181,   181,   181,   181,   181,
     181,   181,   181,   182,   182,   183,   183,   183,   184,   184,
     184,   184,   184,   185,   185,   186,   187,   186,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   191,
     191,   191,   191,   192,   192,   193,   193,   194,   194,   195,
     195,   195,   195,   195,   196,   196,   196,   196,   196,   198,
     197,   199,   200,   200,   201,   201,   202,   202,   202,   202,
     203,   203,   203,   204,   204,   204,   204,   204,   204,   204,
     204,   204,   205,   204,   206,   207,   204,   208,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   204,
     204,   209,   210,   204,   204,   204,   211,   212,   204,   213,
     214,   204,   204,   204,   215,   216,   204,   217,   204,   218,
     219,   204,   220,   204,   204,   204,   204,   204,   204,   204,
     221,   222,   222,   222,   223,   223,   224,   224,   225,   225,
     226,   226,   227,   227,   227,   227,   227,   227,   227,   227,
     228,   227,   229,   229,   229,   229,   230,   230,   231,   231,
     231,   231,   231,   231,   231,   231,   231,   231,   231,   231,
     231,   231,   231,   232,   232,   234,   233,   233,   233,   235,
     235,   236,   236,   237,   237,   238,   238,   239,   239,   241,
     240,   242,   242,   242,   242,   243,   243,   243,   243,   243,
     243,   243,   243,   243,   245,   244,   246,   244,   247,   248,
     248,   249,   249,   250,   250,   250,   251,   251,   252,   252,
     253,   253,   253,   253,   254,   254,   255,   255,   255,   255,
     256,   256,   257,   258,   257,   257,   257,   259,   259,   260,
     260,   261,   262,   262,   263,   263,   264,   264,   265,   266,
     265,   267,   267,   268,   268,   269,   270,   270,   270,   270,
     270,   270,   271,   271,   272,   272,   272,   272,   273,   273,
     273,   273,   273,   274,   274,   275,   275,   275,   275,   275,
     275,   275,   275,   276,   276,   277,   278,   277,   279,   279,
     280,   280,   281,   282,   282,   283,   283,   284,   284,   285,
     285,   286,   286,   287,   287,   288,   288,   288,   288,   289,
     289,   290,   290,   290,   290,   290,   290,   290,   290,   290,
     290,   290,   290,   290,   290,   290,   291,   291,   291,   291,
     291,   292,   292,   293,   294,   293,   295,   295,   296,   297,
     298,   299,   299,   300,   300,   301,   301,   302,   302,   303,
     303,   304,   305,   305,   306,   307,   306,   308,   308,   309,
     309,   310,   310,   311,   311,   311,   311,   312,   312,   312,
     313,   313,   313,   313,   314,   314,   314,   315,   315,   316,
     316,   317,   317,   318,   318,   319,   319,   320,   321,   321,
     321,   322,   322,   322,   323,   324,   324,   325
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
       6,     5,     5,     5,     5,     4,     3,     3,     2,     2,
       3,     2,     2,     3,     3,     3,     3,     3,     3,     4,
       4,     2,     2,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     3,     3,     3,     2,     2,     3,     3,
       3,     3,     6,     6,     4,     6,     4,     6,     1,     1,
       2,     4,     2,     1,     3,     3,     3,     1,     1,     1,
       2,     2,     4,     2,     1,     2,     2,     4,     1,     0,
       2,     2,     2,     1,     1,     3,     1,     2,     3,     4,
       3,     4,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     0,     0,     5,     0,     3,     3,
       3,     2,     3,     3,     1,     2,     4,     3,     2,     1,
       2,     0,     0,     5,     6,     6,     0,     0,     7,     0,
       0,     7,     5,     4,     0,     0,     9,     0,     6,     0,
       0,     8,     0,     5,     4,     4,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     5,     1,     2,
       1,     1,     1,     4,     6,     3,     5,     2,     4,     1,
       0,     4,     4,     2,     2,     1,     2,     0,     6,     8,
       4,     6,     4,     3,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     1,     1,     0,     4,     1,     4,     1,
       4,     1,     3,     1,     1,     4,     1,     3,     3,     0,
       5,     2,     4,     5,     5,     2,     4,     4,     3,     3,
       3,     2,     1,     4,     0,     5,     0,     5,     5,     1,
       1,     6,     1,     1,     1,     1,     2,     1,     2,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     2,     3,
       1,     2,     1,     0,     4,     1,     2,     2,     3,     2,
       3,     1,     1,     2,     1,     2,     1,     2,     1,     0,
       4,     2,     3,     1,     4,     2,     1,     1,     1,     1,
       1,     2,     2,     3,     1,     1,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     0,     4,     3,     3,
       1,     2,     2,     2,     1,     2,     1,     1,     3,     1,
       3,     1,     1,     2,     1,     4,     2,     2,     1,     2,
       0,     6,     8,     4,     6,     4,     6,     2,     4,     6,
       2,     4,     2,     4,     1,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     4,     1,     3,     2,     2,
       2,     1,     3,     1,     3,     1,     1,     2,     1,     1,
       1,     2,     2,     1,     1,     0,     4,     1,     2,     1,
       3,     1,     2,     3,     3,     3,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     1,     0,     1,     2,     0,     1,
       1,     1,     1,     1,     1,     1,     2,     0
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
#line 1535 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 5866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3:
#line 1540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 5875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4:
#line 1547 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5883 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5:
#line 1553 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5891 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6:
#line 1557 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7:
#line 1562 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5908 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8:
#line 1566 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5916 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10:
#line 1573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 5925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11:
#line 1578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 5936 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12:
#line 1590 "mrbgems/mruby-compiler/core/parse.y"
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
#line 5962 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13:
#line 1614 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5970 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14:
#line 1620 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5978 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15:
#line 1624 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5987 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16:
#line 1629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5995 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17:
#line 1633 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 6003 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18:
#line 1638 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 6009 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19:
#line 1639 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 6017 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20:
#line 1643 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6025 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21:
#line 1647 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6033 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22:
#line 1651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6041 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23:
#line 1655 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6049 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24:
#line 1659 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25:
#line 1663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6065 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26:
#line 1667 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 6074 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28:
#line 1673 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6082 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29:
#line 1677 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30:
#line 1681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6098 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31:
#line 1685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6106 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34:
#line 1693 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_asgn(p, (yyvsp[0].nd), (yyvsp[-2].nd));
                    }
#line 6115 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35:
#line 1698 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_masgn(p, (yyvsp[0].nd), (yyvsp[-2].nd));
                    }
#line 6124 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36:
#line 1703 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[0].nd), (yyvsp[-2].nd));
                    }
#line 6132 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37:
#line 1707 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[0].nd), (yyvsp[-2].nd));
                    }
#line 6140 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38:
#line 1713 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6148 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39:
#line 1717 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6156 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40:
#line 1721 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), MRB_QSYM(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6164 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41:
#line 1725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6172 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42:
#line 1729 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43:
#line 1733 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 6189 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44:
#line 1738 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6197 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 45:
#line 1742 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6206 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47:
#line 1750 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50:
#line 1759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6222 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51:
#line 1763 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6230 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52:
#line 1767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6238 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53:
#line 1771 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6246 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55:
#line 1779 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 6257 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56:
#line 1788 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 6265 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 57:
#line 1792 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 6278 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58:
#line 1803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 6289 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62:
#line 1817 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6297 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63:
#line 1823 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 6306 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64:
#line 1830 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 6316 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65:
#line 1838 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6324 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66:
#line 1842 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 6333 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67:
#line 1847 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6341 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68:
#line 1851 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 6350 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69:
#line 1856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 6358 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70:
#line 1860 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 6367 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71:
#line 1865 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 6375 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72:
#line 1869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6383 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73:
#line 1873 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6391 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74:
#line 1877 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6399 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75:
#line 1881 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6407 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76:
#line 1887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6415 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77:
#line 1891 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6423 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79:
#line 1898 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6431 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80:
#line 1904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6439 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81:
#line 1908 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 6447 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82:
#line 1912 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6455 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83:
#line 1916 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6463 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84:
#line 1920 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 6471 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85:
#line 1924 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 6479 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86:
#line 1928 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 6487 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87:
#line 1932 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6495 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88:
#line 1936 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 6503 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89:
#line 1940 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 6511 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91:
#line 1947 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 6519 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92:
#line 1953 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 6527 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93:
#line 1957 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 6535 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94:
#line 1963 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6543 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95:
#line 1967 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 6551 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96:
#line 1973 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6559 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97:
#line 1977 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 6567 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98:
#line 1981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6575 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99:
#line 1985 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6583 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100:
#line 1989 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6591 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101:
#line 1993 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6601 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102:
#line 1999 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6611 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103:
#line 2005 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6620 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104:
#line 2012 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6628 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105:
#line 2016 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 6636 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106:
#line 2020 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6644 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107:
#line 2024 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6652 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108:
#line 2028 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6660 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109:
#line 2032 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110:
#line 2038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111:
#line 2044 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112:
#line 2049 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 6697 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113:
#line 2055 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 6705 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115:
#line 2062 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((node*)1, nsym((yyvsp[0].id)));
                    }
#line 6713 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116:
#line 2066 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((node*)0, nsym((yyvsp[0].id)));
                    }
#line 6721 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117:
#line 2070 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 6730 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121:
#line 2080 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6739 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122:
#line 2085 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6748 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125:
#line 2096 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 6756 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126:
#line 2099 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 6762 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127:
#line 2100 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 6770 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128:
#line 2105 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(or);     }
#line 6776 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129:
#line 2106 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(xor);    }
#line 6782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130:
#line 2107 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(and);    }
#line 6788 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131:
#line 2108 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(cmp);    }
#line 6794 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132:
#line 2109 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(eq);     }
#line 6800 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133:
#line 2110 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(eqq);    }
#line 6806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134:
#line 2111 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(match);  }
#line 6812 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135:
#line 2112 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(nmatch); }
#line 6818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136:
#line 2113 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(gt);     }
#line 6824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137:
#line 2114 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(ge);     }
#line 6830 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138:
#line 2115 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(lt);     }
#line 6836 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139:
#line 2116 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(le);     }
#line 6842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140:
#line 2117 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(neq);    }
#line 6848 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141:
#line 2118 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(lshift); }
#line 6854 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142:
#line 2119 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(rshift); }
#line 6860 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143:
#line 2120 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(add);    }
#line 6866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144:
#line 2121 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(sub);    }
#line 6872 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145:
#line 2122 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mul);    }
#line 6878 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146:
#line 2123 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mul);    }
#line 6884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147:
#line 2124 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(div);    }
#line 6890 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148:
#line 2125 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mod);    }
#line 6896 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149:
#line 2126 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(pow);    }
#line 6902 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150:
#line 2127 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(pow);    }
#line 6908 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151:
#line 2128 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(not);    }
#line 6914 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152:
#line 2129 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(neg);    }
#line 6920 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153:
#line 2130 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(plus);   }
#line 6926 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154:
#line 2131 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(minus);  }
#line 6932 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155:
#line 2132 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(aref);   }
#line 6938 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156:
#line 2133 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(aset);   }
#line 6944 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157:
#line 2134 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(tick);   }
#line 6950 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198:
#line 2152 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6958 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199:
#line 2156 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6966 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200:
#line 2160 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), MRB_QSYM(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6974 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201:
#line 2164 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6982 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202:
#line 2168 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6990 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203:
#line 2172 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6998 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204:
#line 2176 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7007 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205:
#line 2181 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7016 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206:
#line 2186 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7025 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207:
#line 2191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7033 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208:
#line 2195 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7041 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209:
#line 2199 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7049 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210:
#line 2203 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211:
#line 2207 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7065 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212:
#line 2211 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7073 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213:
#line 2215 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 7081 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214:
#line 2219 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 7089 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215:
#line 2223 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 7097 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216:
#line 2227 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 7105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217:
#line 2231 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 7113 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218:
#line 2235 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 7121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219:
#line 2239 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220:
#line 2243 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7137 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221:
#line 2247 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 7145 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222:
#line 2251 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "-@");
                    }
#line 7153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223:
#line 2255 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 7161 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224:
#line 2259 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 7169 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225:
#line 2263 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 7177 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226:
#line 2267 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 7185 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227:
#line 2271 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 7193 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228:
#line 2275 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 7201 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229:
#line 2279 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 7209 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230:
#line 2283 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 7217 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231:
#line 2287 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 7225 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232:
#line 2291 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 7233 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233:
#line 2295 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 7241 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234:
#line 2299 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 7249 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235:
#line 2303 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 7257 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236:
#line 2307 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7265 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237:
#line 2311 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 7273 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238:
#line 2315 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 7281 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239:
#line 2319 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 7289 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240:
#line 2323 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7297 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241:
#line 2327 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7305 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242:
#line 2331 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7313 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243:
#line 2335 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7321 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244:
#line 2339 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7333 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245:
#line 2347 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7346 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246:
#line 2356 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7359 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247:
#line 2365 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7373 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248:
#line 2375 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7381 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250:
#line 2382 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7390 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251:
#line 2387 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd)));
                    }
#line 7398 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252:
#line 2391 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7407 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253:
#line 2398 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7415 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254:
#line 2402 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7425 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255:
#line 2410 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7433 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256:
#line 2414 "mrbgems/mruby-compiler/core/parse.y"
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
#line 7461 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 261:
#line 2446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262:
#line 2451 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7479 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263:
#line 2456 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264:
#line 2463 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(list1((yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265:
#line 2469 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7507 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266:
#line 2474 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7516 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267:
#line 2479 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7525 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268:
#line 2484 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7534 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269:
#line 2490 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 7543 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270:
#line 2495 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 271:
#line 2502 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 7560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272:
#line 2508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273:
#line 2512 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 7576 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 276:
#line 2522 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[0].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7586 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277:
#line 2528 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7596 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278:
#line 2534 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7605 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279:
#line 2539 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7614 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280:
#line 2546 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7623 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281:
#line 2551 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282:
#line 2556 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 7641 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 290:
#line 2570 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 7649 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 291:
#line 2574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 7657 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 292:
#line 2578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7666 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293:
#line 2584 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7675 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294:
#line 2589 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7684 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295:
#line 2593 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 7690 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296:
#line 2594 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7699 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297:
#line 2598 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 7705 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298:
#line 2599 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 7713 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299:
#line 2603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7721 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300:
#line 2607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 7729 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301:
#line 2611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 7737 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302:
#line 2615 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303:
#line 2620 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7755 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304:
#line 2625 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 7763 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305:
#line 2629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7771 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306:
#line 2633 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 7779 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307:
#line 2637 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 7787 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308:
#line 2641 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), cons(0, (yyvsp[0].nd)));
                    }
#line 7795 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310:
#line 2646 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7804 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 311:
#line 2651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 7814 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312:
#line 2657 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7823 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313:
#line 2662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 7835 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314:
#line 2673 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7844 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315:
#line 2681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7853 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316:
#line 2685 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7859 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317:
#line 2685 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7865 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318:
#line 2688 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7874 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319:
#line 2692 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7880 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320:
#line 2692 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7886 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321:
#line 2695 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7895 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322:
#line 2702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323:
#line 2706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 7911 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324:
#line 2710 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 7917 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325:
#line 2712 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 7923 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326:
#line 2715 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 7932 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327:
#line 2721 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7943 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328:
#line 2729 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 7954 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329:
#line 2737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 7963 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330:
#line 2742 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 7973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331:
#line 2749 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 7986 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332:
#line 2759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333:
#line 2767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8008 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334:
#line 2777 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8019 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335:
#line 2787 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8031 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336:
#line 2795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 8039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337:
#line 2799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 8047 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338:
#line 2803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 8055 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 339:
#line 2807 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 8063 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340:
#line 2813 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 8072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 347:
#line 2832 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8080 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 349:
#line 2839 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8088 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 350:
#line 2845 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 8096 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352:
#line 2852 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 8104 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 353:
#line 2856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8112 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354:
#line 2860 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8120 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355:
#line 2864 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3((yyvsp[-2].nd), (node*)-1, 0);
                    }
#line 8129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356:
#line 2869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (node*)-1, (yyvsp[0].nd));
                    }
#line 8137 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357:
#line 2873 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8145 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358:
#line 2877 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359:
#line 2881 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3(0, (node*)-1, 0);
                    }
#line 8162 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360:
#line 2886 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                    }
#line 8170 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361:
#line 2890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (node*)-1, (yyvsp[0].nd));
                    }
#line 8178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362:
#line 2896 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8186 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363:
#line 2900 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 8194 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364:
#line 2904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8202 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365:
#line 2908 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 8210 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366:
#line 2914 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8218 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367:
#line 2918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 8226 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368:
#line 2924 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8234 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369:
#line 2928 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8242 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370:
#line 2932 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8250 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371:
#line 2936 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8258 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372:
#line 2940 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8266 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373:
#line 2944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8274 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374:
#line 2948 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8282 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375:
#line 2952 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8290 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376:
#line 2956 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8298 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377:
#line 2960 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8306 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378:
#line 2964 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8314 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379:
#line 2968 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8322 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380:
#line 2972 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8330 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381:
#line 2976 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8338 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382:
#line 2980 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8346 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383:
#line 2986 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384:
#line 2991 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8364 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385:
#line 2997 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p, 0);}
#line 8370 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386:
#line 2998 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8378 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387:
#line 3002 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388:
#line 3007 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8395 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389:
#line 3014 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8403 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390:
#line 3018 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393:
#line 3028 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 8420 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395:
#line 3036 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8428 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396:
#line 3040 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8436 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397:
#line 3046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8444 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398:
#line 3050 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8452 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399:
#line 3056 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8461 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400:
#line 3063 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8471 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401:
#line 3071 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-1].nd)->car == (node*)NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8485 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402:
#line 3081 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8493 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403:
#line 3085 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8502 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404:
#line 3090 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8511 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405:
#line 3097 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8519 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406:
#line 3101 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8527 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407:
#line 3105 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8535 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408:
#line 3109 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8543 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409:
#line 3113 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM(call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 8551 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410:
#line 3117 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM(call), (yyvsp[0].nd), tCOLON2);
                    }
#line 8559 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411:
#line 3121 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8567 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412:
#line 3125 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 8575 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413:
#line 3129 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 8583 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414:
#line 3135 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8593 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415:
#line 3142 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8604 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416:
#line 3149 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8614 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417:
#line 3156 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8625 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418:
#line 3167 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 8633 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419:
#line 3173 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 8646 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 421:
#line 3187 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 8655 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423:
#line 3195 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8663 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 426:
#line 3203 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 428:
#line 3210 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8679 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435:
#line 3224 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = concat_string(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8687 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438:
#line 3232 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8695 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439:
#line 3236 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8703 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441:
#line 3243 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8711 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442:
#line 3249 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8719 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443:
#line 3253 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8728 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444:
#line 3259 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lex_strterm = (yyvsp[-2].nd);
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8737 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445:
#line 3264 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8745 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446:
#line 3268 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8753 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447:
#line 3274 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8761 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 448:
#line 3278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8769 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 449:
#line 3284 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8777 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450:
#line 3288 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8785 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 454:
#line 3301 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 8795 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 455:
#line 3307 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 8803 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458:
#line 3317 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 8813 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459:
#line 3323 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8822 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460:
#line 3329 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[-2].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 8832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461:
#line 3337 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 8840 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 462:
#line 3341 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8848 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 463:
#line 3348 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 8857 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 464:
#line 3353 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_dsym(p, new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd))));
                    }
#line 8866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465:
#line 3360 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8874 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 470:
#line 3370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8882 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 471:
#line 3374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8890 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 472:
#line 3380 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 8898 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473:
#line 3384 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8906 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476:
#line 3392 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8914 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 477:
#line 3396 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8922 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478:
#line 3402 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 8930 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479:
#line 3406 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 8938 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480:
#line 3410 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 8946 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481:
#line 3414 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 8954 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482:
#line 3418 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 8962 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483:
#line 3424 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8970 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484:
#line 3428 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 8978 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485:
#line 3434 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 8986 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486:
#line 3438 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 8994 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487:
#line 3442 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 9002 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488:
#line 3446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 9010 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489:
#line 3450 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 9018 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 490:
#line 3454 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 9030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491:
#line 3462 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 9041 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492:
#line 3469 "mrbgems/mruby-compiler/core/parse.y"
                    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    }
#line 9054 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495:
#line 3484 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9062 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 496:
#line 3488 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9071 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 497:
#line 3493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9079 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 498:
#line 3504 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9089 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499:
#line 3510 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9111 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501:
#line 3531 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9119 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502:
#line 3537 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 9127 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 503:
#line 3543 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9137 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504:
#line 3549 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9146 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505:
#line 3556 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9155 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506:
#line 3561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9164 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507:
#line 3568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9172 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508:
#line 3572 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509:
#line 3578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9188 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510:
#line 3582 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513:
#line 3592 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, nsym((yyvsp[0].id)));
                    }
#line 9204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514:
#line 3596 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, 0);
                    }
#line 9212 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515:
#line 3602 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9220 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516:
#line 3606 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517:
#line 3610 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9236 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518:
#line 3614 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9244 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519:
#line 3620 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9252 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520:
#line 3624 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9260 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521:
#line 3630 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 522:
#line 3634 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523:
#line 3638 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9284 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524:
#line 3642 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9292 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525:
#line 3646 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9300 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526:
#line 3650 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9308 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527:
#line 3654 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9316 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528:
#line 3658 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9324 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529:
#line 3662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9332 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530:
#line 3666 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531:
#line 3670 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9348 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532:
#line 3674 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9356 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533:
#line 3678 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9364 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534:
#line 3682 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9372 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535:
#line 3686 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, MRB_QSYM(and));
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 9381 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536:
#line 3693 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 9390 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537:
#line 3698 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 9399 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538:
#line 3703 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 9408 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539:
#line 3708 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 9417 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540:
#line 3713 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 9426 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541:
#line 3720 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9434 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542:
#line 3724 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543:
#line 3731 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 9451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544:
#line 3735 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 9459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545:
#line 3739 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn_param(p, (yyvsp[-1].nd), p->locals->car);
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 9469 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546:
#line 3747 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9477 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547:
#line 3751 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9485 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548:
#line 3757 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 9495 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549:
#line 3765 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9505 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550:
#line 3773 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9515 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551:
#line 3781 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9523 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552:
#line 3785 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9531 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553:
#line 3791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9539 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554:
#line 3795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9547 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557:
#line 3805 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9556 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558:
#line 3810 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, MRB_QSYM(mul));
                      (yyval.id) = -1;
                    }
#line 9565 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561:
#line 3821 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9573 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562:
#line 3827 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9581 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563:
#line 3831 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9589 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564:
#line 3837 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 565:
#line 3841 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 9604 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 566:
#line 3842 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9631 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 568:
#line 3868 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9639 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 569:
#line 3874 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 9648 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 570:
#line 3879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573:
#line 3889 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9666 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574:
#line 3895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9675 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 575:
#line 3900 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if ((yyvsp[-2].nd)->car == (node*)NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 9689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 576:
#line 3910 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 9698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 589:
#line 3937 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 9706 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 590:
#line 3941 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 9714 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 592:
#line 3948 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 9722 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 601:
#line 3969 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 9728 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 604:
#line 3975 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 9737 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 607:
#line 3986 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9745 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 9749 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 3990 "mrbgems/mruby-compiler/core/parse.y"

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
