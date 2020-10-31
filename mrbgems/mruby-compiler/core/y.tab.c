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
#define YYLAST   11940

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  177
/* YYNRULES -- Number of rules.  */
#define YYNRULES  604
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1054

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
    1680,  1684,  1688,  1689,  1692,  1701,  1705,  1709,  1713,  1717,
    1721,  1726,  1730,  1737,  1738,  1742,  1746,  1747,  1751,  1755,
    1759,  1763,  1767,  1777,  1776,  1791,  1800,  1801,  1804,  1805,
    1812,  1811,  1826,  1830,  1835,  1839,  1844,  1848,  1853,  1857,
    1861,  1865,  1869,  1875,  1879,  1885,  1886,  1892,  1896,  1900,
    1904,  1908,  1912,  1916,  1920,  1924,  1928,  1934,  1935,  1941,
    1945,  1951,  1955,  1961,  1965,  1969,  1973,  1977,  1981,  1987,
    1993,  2000,  2004,  2008,  2012,  2016,  2020,  2026,  2032,  2037,
    2043,  2047,  2050,  2054,  2058,  2065,  2066,  2067,  2068,  2073,
    2080,  2081,  2084,  2088,  2088,  2094,  2095,  2096,  2097,  2098,
    2099,  2100,  2101,  2102,  2103,  2104,  2105,  2106,  2107,  2108,
    2109,  2110,  2111,  2112,  2113,  2114,  2115,  2116,  2117,  2118,
    2119,  2120,  2121,  2122,  2123,  2126,  2126,  2126,  2127,  2127,
    2128,  2128,  2128,  2129,  2129,  2129,  2129,  2130,  2130,  2130,
    2131,  2131,  2131,  2132,  2132,  2132,  2132,  2133,  2133,  2133,
    2133,  2134,  2134,  2134,  2134,  2135,  2135,  2135,  2135,  2136,
    2136,  2136,  2136,  2137,  2137,  2140,  2144,  2148,  2152,  2156,
    2160,  2164,  2169,  2174,  2179,  2183,  2187,  2191,  2195,  2199,
    2203,  2207,  2211,  2215,  2219,  2223,  2227,  2231,  2235,  2239,
    2243,  2247,  2251,  2255,  2259,  2263,  2267,  2271,  2275,  2279,
    2283,  2287,  2291,  2295,  2299,  2303,  2307,  2311,  2315,  2319,
    2323,  2327,  2335,  2344,  2353,  2363,  2369,  2370,  2375,  2379,
    2386,  2390,  2398,  2402,  2428,  2429,  2432,  2433,  2434,  2439,
    2444,  2451,  2457,  2462,  2467,  2472,  2479,  2479,  2490,  2496,
    2500,  2506,  2507,  2510,  2516,  2522,  2527,  2534,  2539,  2544,
    2551,  2552,  2553,  2554,  2555,  2556,  2557,  2558,  2562,  2567,
    2566,  2578,  2582,  2577,  2587,  2587,  2591,  2595,  2599,  2603,
    2608,  2613,  2617,  2621,  2625,  2629,  2633,  2634,  2640,  2646,
    2639,  2658,  2666,  2674,  2674,  2674,  2681,  2681,  2681,  2688,
    2694,  2699,  2701,  2698,  2710,  2708,  2726,  2731,  2724,  2748,
    2746,  2762,  2772,  2783,  2787,  2791,  2795,  2801,  2808,  2809,
    2810,  2813,  2814,  2817,  2818,  2826,  2827,  2833,  2837,  2840,
    2844,  2848,  2852,  2857,  2861,  2865,  2869,  2875,  2874,  2884,
    2888,  2892,  2896,  2902,  2907,  2912,  2916,  2920,  2924,  2928,
    2932,  2936,  2940,  2944,  2948,  2952,  2956,  2960,  2964,  2968,
    2974,  2979,  2986,  2986,  2990,  2995,  3002,  3006,  3012,  3013,
    3016,  3021,  3024,  3028,  3034,  3038,  3045,  3044,  3059,  3069,
    3073,  3078,  3085,  3089,  3093,  3097,  3101,  3105,  3109,  3113,
    3117,  3124,  3123,  3138,  3137,  3153,  3161,  3170,  3173,  3180,
    3183,  3187,  3188,  3191,  3195,  3198,  3202,  3205,  3206,  3207,
    3208,  3211,  3212,  3218,  3219,  3220,  3224,  3230,  3231,  3237,
    3242,  3241,  3252,  3256,  3262,  3266,  3272,  3276,  3282,  3285,
    3286,  3289,  3295,  3301,  3302,  3305,  3312,  3311,  3325,  3329,
    3336,  3341,  3348,  3354,  3355,  3356,  3357,  3358,  3362,  3368,
    3372,  3378,  3379,  3380,  3384,  3390,  3394,  3398,  3402,  3406,
    3412,  3416,  3422,  3426,  3430,  3434,  3438,  3442,  3450,  3457,
    3468,  3469,  3473,  3477,  3476,  3492,  3498,  3518,  3519,  3525,
    3531,  3537,  3544,  3549,  3556,  3560,  3566,  3570,  3576,  3577,
    3580,  3584,  3590,  3594,  3598,  3602,  3608,  3613,  3618,  3622,
    3626,  3630,  3634,  3638,  3642,  3646,  3650,  3654,  3658,  3662,
    3666,  3670,  3675,  3681,  3686,  3691,  3696,  3701,  3708,  3712,
    3719,  3724,  3723,  3735,  3739,  3745,  3753,  3761,  3769,  3773,
    3779,  3783,  3789,  3790,  3793,  3798,  3805,  3806,  3809,  3815,
    3819,  3825,  3830,  3830,  3855,  3856,  3862,  3867,  3873,  3874,
    3877,  3883,  3888,  3898,  3905,  3906,  3907,  3910,  3911,  3912,
    3913,  3916,  3917,  3918,  3921,  3922,  3925,  3929,  3935,  3936,
    3942,  3943,  3946,  3947,  3950,  3953,  3954,  3955,  3958,  3959,
    3960,  3963,  3970,  3971,  3975
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

#define YYPACT_NINF (-815)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-605)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -815,   138,  2674,  -815,  7423,  9429,  9771,  5731,  -815,  9075,
    9075,  -815,  -815,  9543,  6913,  5585,  7777,  7777,  -815,  -815,
    7777,  3330,  2922,  -815,  -815,  -815,  -815,   168,  6913,  -815,
       9,  -815,  -815,  -815,  5873,  2786,  -815,  -815,  6015,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,    52,  9193,  9193,  9193,
    9193,   106,  4844,   904,  8013,  8367,  7195,  -815,  6631,   881,
     500,   564,  1230,   258,  -815,   259,  9311,  9193,  -815,  1206,
    -815,   798,  -815,  -815,   316,  1806,  1806,  -815,  -815,   142,
      64,  -815,    81,  9657,  -815,    94, 11641,   624,   667,   134,
     122,  -815,   103,  -815,  -815,  -815,  -815,  -815,  -815,  -815,
    -815,  -815,   306,    86,  -815,   312,   104,  -815,  -815,  -815,
    -815,  -815,   120,   120,   136,   974,  1014,  -815,  9075,   519,
    4963,   358,  1806,  1806,  -815,   133,  -815,   920,  -815,  -815,
     104,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,    28,
      85,   149,   162,  -815,  -815,  -815,  -815,  -815,  -815,   182,
     186,   236,   250,  -815,   278,  -815,  -815,  -815,  -815,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,
     290,  4022,   256,   316,   231,   184, 11726,   948,    57,   238,
      67,   231,  9075,  9075,   954,   279,  -815,  -815,  1139,   326,
      56,    80,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,
    -815,  6772,  -815,  -815,   222,  -815,  -815,  -815,  -815,  -815,
    -815,  1206,  -815,   655,  -815,   365,  -815,  -815,  1206,  3058,
    9193,  9193,  9193,  9193,  -815, 11703,  -815,  -815,   260,   366,
     260,  -815,  -815,  -815,  7541,  -815,  -815,  -815,  7777,  -815,
    -815,  -815,  5585,  9075,  -815,  -815,   294,  5082,  -815,  1226,
     371, 11788, 11788,   436,  7659,  4844,   305,  1206,   798,  1206,
     336,  -815,  7659,  1206,   324,  1375,  1375,  -815, 11703,   355,
    1375,  -815,   466,  9885,   357,  1262,  1294,  1319,  2000,  -815,
    -815,  -815,  -815,  1474,  -815,  -815,  -815,  -815,  -815,  -815,
     697,  1060,  -815,  -815,  1478,  -815,  1520,  -815,  1533,  -815,
     444,   462,   469,  -815,  -815,  -815,  -815,  5347,  9075,  9075,
    9075,  9075,  7659,  9075,  9075,    97,  -815,  -815,  -815,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  2357,   425,
    4022,  9193,  -815,   410,   504,   416,  -815,  1206,  -815,  -815,
    -815,   430,  9193,  -815,   434,   516,   445,   546,  -815,   479,
    4022,  -815,  -815,  8485,  -815,  4844,  7309,   461,  8485,  9193,
    9193,  9193,  9193,  9193,  9193,  9193,  9193,  9193,  9193,  9193,
    9193,  9193,  9193,  9193,   558,  9193,  9193,  9193,  9193,  9193,
    9193,  9193,  9193,  9193,  9193,  9193,  1682,  -815,  7777,  -815,
    2477,  -815,  -815, 11281,  -815,  -815,  -815,  -815,  9311,  9311,
    -815,   513,  -815,   316,  -815,  1320,  -815,  -815,  -815,  -815,
    -815,  -815, 10163,  7777, 10249,  4022,  9075,  -815,  -815,  -815,
     599,   602,   227,  -815,  4168,   609,  9193, 10335,  7777, 10421,
    9193,  9193,  4460,   866,   866,    92, 10507,  7777, 10593,  -815,
     555,  -815,  5082,   365,  -815,  -815,  8603,   622,  -815,   697,
    9193, 11726, 11726, 11726,  9193,   760,  -815,  7895,  -815,  9193,
    -815,  8131,  1206,   493,  1206,   260,   260,  -815,  -815,   965,
     495,  -815,  -815,  6913,  4579,   508, 10335, 10421,  9193,   798,
    1206,  -815,  -815,  5466,   506,   798,  -815,  -815,  8249,  -815,
    1206,  8367,  -815,  -815,  -815,  1320,    81,  9885,  -815,  9885,
   10679,  7777, 10765,  1234,  -815,  -815,  -815,  1541,  5082,   697,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  9193,  9193,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  1614,
    1206,  1206,  9193,   648, 11726,   173,  -815,  -815,  -815,    53,
    -815,  -815,  1234,  -815, 11726,  1234,  -815,  -815,  1782,  -815,
    -815,  9193,   649,   157,  9193,  -815, 11461,   260,  -815,  1206,
    9885,   524,  -815,  -815,  -815,   629,   557,  2241,  -815,  -815,
    1338,   244,   371, 10185, 10185, 10185, 10185,  1181,  1181, 10271,
   11807, 10185, 10185, 11788, 11788,    60,    60,  -815, 11436,  1181,
    1181,  1251,  1251,   805,   156,   156,   371,   371,   371,  3466,
    6371,  3738,  6489,  -815,   120,  -815,   531,   623,  -815,   626,
    -815,  -815,  3194,  -815,  -815,  1876,   157,   157,  -815,  2266,
    -815,  -815,  -815,  -815,  -815,  1206,  9075,  4022,  1042,   551,
    -815,   120,   547,   120,   675,   965,  7054,  -815,  8721,   676,
    -815,   470,  -815,  6133,  6252,   559,   273,   298,   676,  -815,
    -815,  -815,  -815,    23,    68,   560,    99,   114,  9075,  6913,
     563,   681, 11726,   806,  -815,   697, 11726, 11726,   697,  9193,
   11703,  -815,   260, 11726,  -815,  -815,  -815,  -815,  7895,  8131,
    -815,  -815,  -815,   566,  -815,  -815,    22,   798,  1206,  1375,
     461,  -815,  1042,   551,   568,  1114,  1158,   576,    58,  -815,
     586,  -815,   371,   371,  -815,   834,  1206,   565,  -815,  -815,
   11353,  -815,   670,  -815,   416,  -815,  -815,  -815,   591,   593,
     601,  -815,   604,   670,   601, 11374,  -815,  -815,  1234,  4022,
    -815,  -815, 11532,  8839,  -815,  -815,  9885,  7659,  9311,  9193,
   10851,  7777, 10937,    98,  9311,  9311,  -815,   513,   646,  9311,
    9311,  -815,   513,   122,   142,  4022,  5082,   157,  -815,  1206,
     726,  -815,  -815,  -815,  -815, 11461,  -815,   661,  -815,  4725,
     743,  -815,  9075,   747,  -815,  9193,  9193,   303,  9193,  9193,
     768,  5228,  5228,   115,   866,  -815,  -815,  -815,  8957,  4314,
     697, 11726,  -815,   260,  -815,  -815,  -815,    89,   654,   645,
    4022,  5082,  -815,  -815,  -815,   652,  -815,  1932,  9193,  -815,
    1234,  -815,  1782,  -815,  1782,  -815,  1782,  -815,  -815,  9193,
    -815,   576,   576,  9999,  -815,   658,   416,   668,  9999,  -815,
     684,   686,  -815,   794,  9193, 11551,  -815,  -815, 11726,  3602,
    3874,   677,   363,   409,  9193,  9193,  -815,  -815,  -815,  -815,
    -815,  9311,  -815,  -815,  -815,  -815,  -815,  -815,  -815,   808,
     693,  5082,  4022,  -815,  -815, 10113,   231,  -815,  -815,  5228,
    -815,  -815,   231,  -815,  9193,  -815,   818,   832,  -815, 11726,
     247,  8131,  -815,  1620,   835,   716,  1724,  1724,  1075, 11726,
     601,   717,   601,   601, 11726,   734,   741,   813,  1342,   173,
    -815,  -815,  2140,  -815,  1342,  1234,  -815,  1782,  -815,  -815,
   11622,   417, 11726, 11726,  -815,  -815,  -815,  -815,   730,   855,
     828,  -815,  1366,  1294,  1319,  4022,  -815,  4168,  -815,  -815,
    5228,  -815,  -815,  -815,  -815,    20,  -815,  -815,  -815,  -815,
     744,   744,  1724,   750,  -815,  1782,  -815,  -815,  -815,  -815,
    -815,  -815, 11023,  -815,   416,   173,  -815,  -815,   755,   756,
     758,  -815,   763,   758,  -815,  -815,  1320, 11109,  7777, 11195,
     602,   470,   887,  1620,  -815,  1724,   744,  1724,   601,   767,
     770,  -815,  1234,  -815,  1782,  -815,  1782,  -815,  1782,  -815,
    -815,  1042,   551,   771,   377,   706,  -815,  -815,  -815,  -815,
     744,  -815,   758,   773,   758,   758,    89,  -815,  1782,  -815,
    -815,  -815,   758,  -815
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   289,     0,
       0,   313,   316,     0,     0,   590,   333,   334,   335,   336,
     301,   266,   266,   484,   483,   485,   486,   592,     0,    10,
       0,   488,   487,   489,   475,   576,   477,   476,   479,   478,
     471,   472,   433,   434,   490,   491,   287,     0,     0,     0,
       0,     0,     0,   291,   604,   604,    85,   308,     0,     0,
       0,     0,     0,     0,   448,     0,     0,     0,     3,   590,
       6,     9,    32,    27,    33,   532,   532,    46,    57,    56,
       0,    73,     0,    77,    87,     0,    51,   245,     0,    58,
     306,   280,   281,   431,   282,   283,   284,   429,   428,   460,
     430,   427,   482,     0,   285,   286,   266,     5,     8,   333,
     334,   301,   604,   409,     0,   110,   111,   287,     0,     0,
       0,     0,   532,   532,   113,   492,   337,     0,   482,   286,
       0,   329,   165,   175,   166,   162,   191,   192,   193,   194,
     173,   188,   181,   171,   170,   186,   169,   168,   164,   189,
     163,   176,   180,   182,   174,   167,   183,   190,   185,   184,
     177,   187,   172,   161,   179,   178,   160,   158,   159,   155,
     156,   157,   115,   117,   116,   150,   151,   146,   128,   129,
     130,   137,   134,   136,   131,   132,   152,   153,   138,   139,
     143,   147,   133,   135,   125,   126,   127,   140,   141,   142,
     144,   145,   148,   149,   154,   562,    52,   118,   119,   561,
       0,     0,     0,    55,     0,     0,    51,     0,   482,     0,
     286,     0,     0,     0,   109,     0,   348,   347,     0,     0,
     482,   286,   184,   177,   187,   172,   155,   156,   157,   115,
     116,     0,   120,   122,    20,   121,   451,   456,   455,   598,
     601,   590,   600,     0,   453,     0,   602,   599,   591,   574,
       0,     0,     0,     0,   261,   273,    71,   265,   604,   431,
     604,   566,    72,    70,   604,   255,   302,    69,     0,   254,
     408,    68,   590,     0,   593,    18,     0,     0,   218,     0,
     219,   206,   209,   298,     0,     0,     0,   590,    15,   590,
      75,    14,     0,   590,     0,   595,   595,   246,     0,     0,
     595,   564,     0,     0,    83,     0,    93,   100,   532,   465,
     464,   466,   467,     0,   463,   462,   446,   440,   439,   442,
       0,     0,   437,   458,     0,   469,     0,   435,     0,   444,
       0,   473,   474,    50,   233,   234,     4,   591,     0,     0,
       0,     0,     0,     0,     0,   539,   535,   534,   533,   536,
     537,   508,   541,   553,   509,   557,   556,   552,   532,   497,
       0,   501,   506,   604,   511,   604,   531,     0,   538,   540,
     543,   517,     0,   550,   517,   555,   517,     0,   515,   497,
       0,   396,   398,     0,    89,     0,    81,    78,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     205,   208,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   587,   604,   586,
       0,   589,   588,     0,   413,   411,   307,   432,     0,     0,
     402,    62,   305,   326,   110,   111,   112,   473,   474,   497,
     493,   324,     0,   604,     0,     0,     0,   585,   584,    53,
       0,   604,   298,   339,     0,   338,     0,     0,   604,     0,
       0,     0,     0,     0,     0,   298,     0,   604,     0,   321,
       0,   123,     0,     0,   452,   454,     0,     0,   603,   568,
       0,   274,   573,   268,     0,   271,   262,     0,   270,     0,
     263,     0,   590,     0,   590,   604,   604,   256,   267,   590,
       0,   304,    49,     0,     0,     0,     0,     0,     0,    17,
     590,   296,    13,   591,    74,   292,   295,   299,   597,   247,
     596,   597,   249,   300,   565,    99,    91,     0,    86,     0,
       0,   604,     0,   532,   309,   393,   468,     0,     0,   443,
     449,   447,   438,   459,   470,   436,   445,     0,     0,     7,
      21,    22,    23,    24,    25,    47,    48,   499,   545,     0,
     590,   590,     0,     0,   500,     0,   513,   560,   510,     0,
     514,   498,     0,   524,   546,     0,   527,   554,     0,   529,
     558,     0,     0,   604,     0,    28,    30,     0,    31,   590,
       0,    79,    90,    45,    35,    43,     0,   250,   195,    29,
       0,   286,   215,   223,   228,   229,   230,   225,   227,   237,
     238,   231,   232,   204,   207,   235,   236,    34,   592,   224,
     226,   220,   221,   222,   210,   211,   212,   213,   214,   577,
     582,   578,   583,   407,   266,   405,     0,   577,   579,   578,
     580,   406,   266,   577,   578,   266,   604,   604,    36,   250,
     196,    42,   203,    60,    63,     0,     0,     0,   110,   111,
     114,     0,     0,   604,     0,   590,     0,   290,   604,   604,
     419,   604,   340,   581,   297,     0,   577,   578,   604,   342,
     314,   341,   317,   581,   297,     0,   577,   578,     0,     0,
       0,     0,   273,     0,   320,   569,   571,   570,     0,     0,
     275,   269,   604,   572,   567,   253,   252,   257,   258,   260,
     303,   594,    19,     0,    26,   202,    76,    16,   590,   595,
      92,    84,    96,    98,     0,    95,    97,   592,     0,   461,
       0,   450,   216,   217,   539,   356,   590,   349,   496,   495,
     241,   331,     0,   507,   604,   559,   516,   544,   517,   517,
     517,   551,   517,   539,   517,   243,   332,   384,   382,     0,
     381,   380,   279,     0,    88,    82,     0,     0,     0,     0,
       0,   604,     0,     0,     0,     0,   404,    66,   410,     0,
       0,   403,    64,   399,    59,     0,     0,   604,   327,     0,
       0,   410,   330,   563,    54,   420,   421,   604,   422,     0,
     604,   345,     0,     0,   343,     0,     0,   410,     0,     0,
       0,     0,     0,   410,     0,   124,   457,   319,     0,     0,
     272,   276,   264,   604,    11,   293,   248,    94,     0,   386,
       0,     0,   310,   441,   357,   354,   542,     0,     0,   512,
       0,   520,     0,   522,     0,   528,     0,   525,   530,     0,
     379,   592,   592,   503,   504,   604,   604,   364,     0,   548,
     364,   364,   362,     0,     0,   277,    80,    44,   251,   577,
     578,     0,   577,   578,     0,     0,    41,   200,    40,   201,
      67,     0,    38,   198,    39,   199,    65,   400,   401,     0,
       0,     0,     0,   494,   325,     0,     0,   424,   346,     0,
      12,   426,     0,   311,     0,   312,     0,     0,   322,   275,
     604,   259,   392,     0,     0,     0,     0,     0,   352,   242,
     517,   517,   517,   517,   244,     0,     0,     0,   502,     0,
     360,   361,   364,   372,   547,     0,   375,     0,   377,   397,
     278,   410,   240,   239,    37,   197,   414,   412,     0,     0,
       0,   423,     0,   101,   108,     0,   425,     0,   315,   318,
       0,   416,   417,   415,   390,   592,   388,   391,   395,   394,
     358,   355,     0,   350,   521,     0,   518,   523,   526,   385,
     383,   298,     0,   505,   604,     0,   363,   370,   364,   364,
     364,   549,   364,   364,    61,   328,   107,     0,   604,     0,
     604,   604,     0,     0,   387,     0,   353,     0,   517,   581,
     297,   359,     0,   367,     0,   369,     0,   376,     0,   373,
     378,   104,   106,     0,   577,   578,   418,   344,   323,   389,
     351,   519,   364,   364,   364,   364,   102,   368,     0,   365,
     371,   374,   364,   366
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -815,  -815,  -815,   402,  -815,    33,  -815,    18,   248,  -815,
      88,  -815,  -815,  -154,  -132,   400,   128,   131,  -815,    -6,
      -9,  -815,  -618,  -815,    30,   909,  -212,    44,   -20,  -287,
    -441,   -29,   549,   -76,   917,    13,   -12,  -815,  -815,    37,
    -815,  1154,  -815,   637,    90,  -221,  -369,    69,     7,  -815,
    -421,  -235,  -119,    62,  -353,   628,  -815,  -815,  -815,  -815,
    -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,  -815,
    -815,  -815,     8,  -189,  -452,   -71,  -582,  -815,  -815,  -815,
     174,   239,  -815,  -535,  -815,  -815,  -276,  -815,   -63,  -815,
    -815,   159,  -815,  -815,  -815,   -75,  -815,  -815,  -427,  -815,
     -55,  -815,  -815,  -815,  -815,  -815,   -15,    40,  -211,  -815,
    -815,  -815,  -815,  -414,  -294,  -815,   703,  -815,  -815,  -815,
      27,  -815,  -815,  -815,  1610,  1747,   950,  1392,  -815,  -815,
     530,    82,  -696,   383,    21,  -815,  -815,  -815,   252,    77,
     206,  -264,  -809,  -678,  -405,  -815,   674,  -707,  -536,  -814,
      19,   380,  -815,  -376,  -815,   203,  -333,  -815,  -815,  -815,
      14,   694,  -493,   579,  -360,  -815,  -815,   -82,  -815,    73,
     -24,  -102,  -248,   361,   -16,   -36,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    68,    69,    70,   286,   460,   461,   297,
     298,   513,    72,    73,   604,    74,    75,    76,   676,   214,
      77,    78,   664,   797,    79,    80,   299,    81,    82,    83,
     538,    84,   215,   124,   125,   242,   243,   244,   699,   642,
     208,    86,   304,   608,   643,   276,   503,   504,   277,   278,
     267,   496,   531,   505,   598,    87,   211,   302,   728,   303,
     318,   738,   222,   821,   223,   822,   698,   970,   667,   665,
     902,   455,   289,   464,   690,   813,   814,   229,   746,   926,
     996,   943,   861,   769,   770,   862,   838,   975,   976,   544,
     842,   392,   593,    89,    90,   442,   657,   656,   487,   973,
     679,   807,   906,   910,    91,    92,    93,   331,   332,   548,
      94,    95,    96,   549,   252,   253,   254,   482,    97,    98,
      99,   325,   100,   101,   218,   219,   104,   220,   451,   666,
     449,   370,   371,   372,   864,   865,   373,   374,   375,   756,
     583,   377,   378,   379,   380,   569,   381,   382,   383,   869,
     870,   384,   385,   386,   387,   388,   576,   210,   456,   309,
     506,   490,   271,   130,   671,   645,   459,   454,   433,   510,
     839,   511,   529,   256,   257,   258,   301
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     107,   269,   269,   283,   221,   269,   430,   432,   714,   251,
      88,   284,    88,   127,   127,   436,   285,   217,   217,   279,
     206,   228,   692,   217,   217,   217,   537,   314,   217,   281,
     270,   270,   472,   347,   270,   500,   550,   108,   714,   269,
     269,   245,   580,   446,   207,   609,   264,   264,  -104,   761,
     264,   207,   307,   311,   545,   245,   701,   343,   532,   646,
      88,   868,   534,   397,   315,   207,   644,   845,   306,   310,
     652,   324,   863,   655,   217,   705,   711,   437,   268,   268,
     711,  -101,   268,   520,   672,   266,   272,   840,   255,   273,
      71,   315,    71,  -106,   673,   207,   300,   810,   731,   685,
     334,   336,   338,   340,   571,  -108,   820,   644,   695,   652,
     279,   275,   280,   441,   977,  -410,   305,  -107,   673,  -484,
     552,   795,   796,   552,  -103,   552,   217,   552,    88,   552,
     434,  1001,   399,   122,   122,   469,   123,   123,     3,  -105,
    -102,   122,   346,   -74,   123,  -481,   478,   287,   434,   497,
    -480,   501,   376,   376,   567,   884,   365,   673,   390,   775,
     471,   293,  1013,    42,   -88,   -96,    43,  -577,   250,   890,
     391,   841,   734,  -109,   896,  -484,  -483,   757,  -101,   438,
    -410,   366,   673,   599,   122,   393,   528,   123,  -108,   421,
     422,   423,   424,   425,  -410,   550,   440,   524,   -93,   376,
     376,   526,   275,   280,   977,   390,   759,   517,  1001,   762,
     -98,   122,  -578,    62,   123,   398,   473,   474,   568,    88,
     440,   885,  -100,   394,   752,   431,   714,  -410,   399,  -410,
     217,   217,  -483,   542,   -99,   868,  -410,   767,   868,   427,
    -485,   -95,   435,   863,   603,   361,   863,   463,   274,   863,
     983,   863,   537,  -486,   324,   741,   -97,   -94,   450,   269,
     435,   523,   901,   269,   274,   809,   498,   486,   498,   530,
     530,   500,   507,  -488,   530,   364,   365,  -487,   207,   737,
     282,   429,   217,   768,   603,   603,   217,   423,   424,   425,
     217,   217,   270,   536,   830,    88,  -485,   711,   711,   863,
     296,   366,    88,    88,   264,   466,   658,   661,   264,  -486,
      88,   462,   282,   537,   761,   868,   250,   341,   342,   339,
     518,   315,   327,   328,   483,   806,   863,  -489,   863,  -488,
     863,   470,   863,  -487,   475,   876,   552,   439,   971,   300,
     268,  -475,   560,   561,   562,   563,   246,   508,  -107,   247,
     248,   479,   863,   353,   354,    88,   217,   217,   217,   217,
      88,   217,   217,   547,   481,  -108,   818,   601,   296,  -479,
     522,   577,   918,   577,   329,   330,   670,   249,    88,   250,
     559,   457,   519,  -489,   595,   486,   718,   719,   573,   605,
     525,   819,   871,  -103,  -103,   376,   914,  -475,    88,  -480,
     715,   217,   495,    88,   315,   439,   610,   720,   592,   213,
     213,   741,   881,   269,   550,   213,   447,   448,   726,  -105,
     644,   849,   652,   489,  -102,  -479,   507,  -101,   714,   605,
     605,   791,   514,  -108,   793,    71,   217,   458,   269,   300,
     564,   122,   757,   399,   123,   376,   610,   610,   -93,   757,
     521,   507,   791,   269,  -100,   597,   789,   -73,   264,   680,
     597,   217,   269,    88,   217,   527,   507,   650,   748,   749,
     650,   708,    88,   674,   931,   507,   217,   832,   773,   284,
      88,   836,   269,   264,  -103,   217,   269,   812,   809,   537,
      88,   650,  -103,   972,   533,  -103,  -103,   774,   264,   539,
     711,   722,   790,   498,   498,   556,   650,   264,   327,   328,
     891,   712,   107,   269,   829,   650,   269,   730,   443,   536,
     651,   535,    88,  -103,   122,  -103,   269,   123,   782,   518,
    -105,    88,   940,   941,   557,   515,   741,   757,  -102,   507,
     245,   558,   729,   296,   651,   315,   572,   315,   703,   217,
     207,    85,   575,    85,   650,   578,    88,  -107,   579,   651,
     329,   330,   227,   333,   327,   328,   999,   587,   651,  1002,
     444,   264,   582,   803,   445,   465,   585,   717,   -99,   650,
     536,   106,   465,   106,   828,   935,   936,   588,   106,   106,
     586,   771,   589,   501,   106,   106,   106,   590,   832,   106,
     591,    85,    71,   602,   783,   369,   389,   651,   315,   627,
     757,   727,   284,   663,   677,   678,   329,   330,   546,   488,
     376,   757,   213,   213,   603,   682,   835,   335,   327,   328,
     603,   106,   651,   126,   126,   603,   603,   704,   716,  1033,
     721,   126,  -297,   296,   846,   106,  1043,   724,   -88,   673,
     279,   787,   886,   279,   771,   771,  -297,   892,   894,   792,
     799,  1021,   794,   751,   766,   122,   776,   122,   123,    85,
     123,   279,   788,   777,   217,    88,   808,   811,   778,   811,
     329,   330,   509,   512,   126,   800,   811,   825,   801,   804,
     802,  -297,   824,   530,   809,  -578,   827,   106,  -297,   106,
     817,   823,   826,   269,   269,   834,   217,   847,   488,   837,
     498,   126,   681,   207,   921,  -337,   789,   965,   897,   790,
     688,   284,  -105,   967,   250,   843,   245,   567,   122,  -337,
     700,   123,   833,   850,   786,   852,   207,   603,   581,   891,
     597,   904,   275,   854,  -103,   275,   856,  -105,   213,   213,
     213,   213,   577,   565,   566,   905,   536,   909,   426,   954,
      85,   786,   913,   275,  -337,   -95,   269,  -102,   -97,   605,
     484,  -337,   427,   247,   248,   605,   888,    88,   755,   507,
     605,   605,   755,   915,   315,    88,   610,   873,   -94,   217,
     106,   923,   610,   217,   927,   771,   740,   610,   610,   922,
     939,   106,   106,    88,    88,   907,   912,   428,   911,   949,
     942,   264,   246,   899,   429,   247,   248,    88,   951,   650,
     217,  -105,   463,   956,  -105,  -105,   945,   754,   947,    88,
      88,   498,   957,   968,   691,   691,    85,    88,   348,   349,
     350,   351,   352,    85,    85,   284,   284,   969,    88,    88,
     978,    85,  -105,   106,  -105,   979,   675,   106,   924,   985,
     989,   106,   106,   577,   577,   877,   106,   990,   991,  1004,
    1005,   938,   651,   106,   106,  -592,   944,   399,  -592,  -592,
    1009,   106,   605,  1006,   488,   744,  1015,   356,   357,   358,
     359,   488,  1017,   689,   412,   413,    85,  1022,  1024,   610,
    1026,    85,  1038,   360,   122,  1028,   269,   123,   250,    88,
      88,  -577,  1046,   962,  -578,  1048,   723,    88,   811,    85,
     959,   246,   226,   131,   247,   248,   106,   106,   106,   106,
     106,   106,   106,   106,   421,   422,   423,   424,   425,    85,
    1037,   126,   860,   326,    85,   327,   328,   606,   495,   106,
    1039,  1014,   249,   898,   250,  1036,   485,   209,   753,   284,
     993,   998,   758,   499,   851,   853,   855,     0,   857,   106,
     858,   872,   106,    88,   106,    88,   844,   106,    88,     0,
       0,   246,     0,     0,   247,   248,     0,   606,   606,     0,
       0,   122,   577,   269,   123,     0,   122,   329,   330,   123,
    -574,     0,   353,   354,    85,     0,   507,   106,   680,   811,
       0,   452,   249,    85,   250,     0,   217,   106,   106,  -294,
     866,    85,  -294,  -294,   126,   427,   798,     0,     0,     0,
       0,    85,   106,   122,   106,   106,   123,     0,   264,   467,
    -575,     0,     0,   106,   900,  -287,   650,   106,     0,  -294,
    -294,   106,  -294,   427,     0,     0,   106,   908,     0,  -287,
     453,   106,     0,    85,   465,  -475,   213,   429,  -581,   916,
     917,     0,    85,     0,     0,   660,   662,   920,     0,  -475,
     246,     0,     0,   247,   248,     0,     0,     0,   468,   925,
       0,     0,     0,   106,  -287,   429,     0,    85,   213,   651,
       0,  -287,   106,     0,     0,  -479,     0,   660,   662,   946,
     948,   249,  -574,   250,  -475,     0,     0,     0,  -574,  -479,
     106,  -475,   551,     0,   327,   328,   744,   106,   356,   357,
     358,   359,     0,  -581,     0,     0,   984,   986,   987,   988,
    -577,     0,   755,     0,   360,   872,     0,  -581,   872,   958,
     872,     0,  -575,     0,  -479,   725,     0,   966,  -575,     0,
     903,  -479,     0,   216,   216,   126,     0,   126,     0,   216,
     265,   265,     0,     0,   265,     0,   329,   330,     0,     0,
    -581,   997,  -581,     0,  -578,   691,  -577,     0,     0,  -581,
       0,   994,     0,     0,   866,     0,     0,   866,   872,   866,
       0,   288,   290,   291,   292,  -577,     0,     0,   265,   308,
       0,     0,   213,  1010,     0,  1011,    85,   982,  1012,  -577,
     344,   345,     0,     0,  1041,   872,     0,   872,   126,   872,
     476,   872,     0,     0,     0,     0,     0,  1023,  1025,  1027,
       0,  1029,  1030,   747,   427,   106,   106,   866,     0,  -578,
       0,   872,  -577,   399,  -577,     0,     0,     0,  -577,   760,
       0,  -577,   764,  -578,     0,     0,     0,   465,     0,     0,
     412,   413,   216,   465,   866,     0,   866,   106,   866,   477,
     866,  1047,  1049,  1050,  1051,   355,   429,   356,   357,   358,
     359,  1053,     0,   337,   327,   328,  -578,     0,  -578,     0,
     866,     0,  -578,   360,     0,  -578,   361,   418,   419,   420,
     421,   422,   423,   424,   425,     0,     0,   516,    85,     0,
       0,   246,     0,   399,   247,   248,    85,   606,     0,   362,
       0,   427,     0,   606,     0,   363,   364,   365,   606,   606,
     412,   413,     0,     0,    85,    85,   329,   330,   106,     0,
       0,     0,   249,   540,   250,     0,   106,   106,    85,     0,
     106,     0,   366,   106,   106,   367,   468,   427,   106,   106,
      85,    85,     0,   429,   106,   106,   216,   216,    85,   420,
     421,   422,   423,   424,   425,  -482,     0,     0,   106,    85,
      85,   106,     0,     0,   105,     0,   105,   129,   129,  -482,
     106,   106,   541,     0,   126,   231,     0,     0,   106,   429,
    -286,  -298,     0,     0,   491,   492,   493,   344,     0,   106,
     106,   887,   889,     0,  -286,  -298,   893,   895,   265,   780,
       0,     0,   265,   992,  -482,     0,   216,   216,     0,     0,
     606,  -482,   867,   427,   105,     0,     0,   427,   317,     0,
      85,    85,   887,   889,   961,   893,   895,  1007,    85,  -286,
    -298,     0,     0,     0,     0,     0,  -286,  -298,     0,     0,
     106,   427,     0,     0,     0,   317,     0,     0,   781,     0,
     106,   106,   453,     0,     0,   429,     0,     0,   106,   429,
     246,   126,     0,   247,   248,     0,   126,     0,     0,     0,
       0,     0,   216,   216,   216,   216,  1008,   216,   216,     0,
       0,     0,   105,   429,    85,     0,    85,   495,     0,    85,
       0,   249,     0,   250,   930,   574,   932,     0,   955,     0,
     933,     0,     0,   126,     0,     0,   584,   546,   327,   328,
       0,   553,   327,   328,   106,     0,   106,   596,     0,   106,
       0,   955,   607,   612,   613,   614,   615,   616,   617,   618,
     619,   620,   621,   622,   623,   624,   625,   626,     0,   628,
     629,   630,   631,   632,   633,   634,   635,   636,   637,   638,
       0,     0,   265,   554,   327,   328,     0,   106,     0,     0,
     329,   330,   659,   659,   329,   330,   555,   327,   328,     0,
     980,   981,     0,   105,   739,   327,   328,   265,     0,     0,
     216,     0,   102,     0,   102,   128,   128,   128,     0,  1000,
     659,  1003,   265,   230,   659,   659,     0,     0,     0,     0,
       0,   265,     0,     0,     0,     0,   329,   330,     0,     0,
     702,     0,     0,     0,   706,     0,     0,     0,   707,   329,
     330,   710,     0,   713,     0,   308,  1016,   329,   330,  1018,
       0,     0,   102,     0,     0,   744,   316,   356,   357,   358,
     359,   974,   659,   356,   357,   358,   359,     0,     0,   105,
       0,     0,   710,   360,     0,   308,   105,   105,     0,   360,
       0,  1040,     0,   316,   105,   265,  1042,     0,  1044,     0,
       0,     0,  1045,     0,     0,   317,     0,     0,     0,   362,
       0,   742,   743,     0,     0,   745,     0,     0,     0,     0,
       0,     0,  1052,     0,     0,     0,   750,     0,     0,     0,
     102,     0,     0,   639,   640,     0,     0,   641,     0,   105,
       0,     0,     0,     0,   105,   765,     0,     0,   772,   103,
       0,   103,   175,   176,   177,   178,   179,   180,   181,   182,
     183,     0,   105,   184,   185,     0,     0,     0,     0,   186,
     187,   188,   189,     0,     0,   744,     0,   356,   357,   358,
     359,     0,   105,   190,   191,     0,     0,   105,   317,     0,
     611,     0,     0,   360,     0,     0,     0,     0,     0,   103,
       0,     0,     0,     0,     0,     0,   192,   193,   194,   195,
     196,   197,   198,   199,   200,   201,     0,   202,   203,   362,
     216,   102,     0,     0,     0,   204,   274,     0,     0,     0,
     611,   611,   805,   763,     0,   356,   357,   358,   359,     0,
       0,     0,     0,     0,     0,     0,     0,   105,     0,     0,
       0,   360,   216,     0,   361,     0,   105,   355,     0,   356,
     357,   358,   359,   831,   105,     0,     0,   103,     0,     0,
       0,     0,   710,   308,   105,   360,  -604,   362,   361,     0,
       0,     0,     0,     0,   364,   365,     0,     0,     0,  -604,
    -604,  -604,  -604,  -604,  -604,     0,  -604,   102,     0,     0,
       0,   362,  -604,  -604,   102,   102,   105,   363,   364,   365,
     366,     0,   102,  -604,  -604,   105,  -604,  -604,  -604,  -604,
    -604,     0,     0,   316,     0,     0,     0,   875,     0,   317,
       0,   317,   659,   878,   366,   265,     0,   367,   659,   659,
     105,     0,     0,   659,   659,     0,     0,     0,     0,     0,
     368,     0,     0,     0,     0,     0,     0,   102,   103,     0,
       0,     0,   102,     0,     0,     0,   216,  -604,     0,   659,
     659,     0,   659,   659,     0,     0,     0,     0,     0,     0,
     102,  -604,   919,   744,     0,   356,   357,   358,   359,     0,
       0,  -604,   317,     0,  -604,  -604,     0,     0,     0,     0,
     102,   360,   929,     0,     0,   102,   316,     0,     0,     0,
       0,     0,     0,   934,  -604,  -604,     0,     0,     0,     0,
     274,  -604,  -604,  -604,  -604,     0,     0,   362,   950,     0,
       0,     0,     0,   928,   103,     0,     0,     0,   952,   953,
       0,   103,   103,     0,     0,   659,     0,     0,     0,   103,
       0,   355,     0,   356,   357,   358,   359,     0,     0,   105,
       0,     0,     0,     0,     0,   102,     0,     0,   659,   360,
       0,     0,   361,     0,   102,   308,     0,     0,     0,     0,
       0,     0,   102,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   102,     0,   103,   362,     0,     0,     0,   103,
       0,   363,   364,   365,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   103,     0,     0,
       0,     0,     0,     0,   102,     0,     0,     0,   366,     0,
       0,   367,     0,   102,     0,     0,     0,   103,     0,     0,
       0,     0,   103,     0,   543,   103,     0,   316,     0,   316,
       0,     0,     0,     0,     0,     0,     0,     0,   102,     0,
       0,   105,   265,     0,     0,     0,     0,     0,   317,   105,
     611,     0,     0,     0,     0,     0,   611,     0,     0,     0,
       0,   611,   611,     0,     0,   103,   103,   105,   105,     0,
       0,   355,     0,   356,   357,   358,   359,     0,     0,     0,
       0,   105,   103,     0,     0,     0,     0,     0,     0,   360,
     316,   103,   361,   105,   105,     0,     0,     0,     0,   103,
       0,   105,     0,     0,     0,     0,     0,     0,     0,   103,
       0,     0,   105,   105,     0,   362,     0,     0,     0,     0,
       0,   363,   364,   365,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   129,     0,     0,     0,     0,
     129,   103,     0,     0,     0,     0,     0,     0,   366,     0,
     103,   367,     0,     0,     0,     0,     0,   102,     0,     0,
       0,     0,   995,   611,     0,   779,     0,     0,     0,     0,
       0,     0,     0,   105,   105,   103,     0,   964,     0,     0,
       0,   105,     0,     0,     0,     0,     0,     0,     0,     0,
     779,     0,     0,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,     0,     0,     0,     0,
     412,   413,     0,     0,     0,     0,     0,     0,   399,   400,
     401,   402,   403,   404,   405,   406,   407,   408,   409,   410,
     411,     0,     0,     0,     0,   412,   413,   105,     0,   105,
       0,     0,   105,   415,     0,   416,   417,   418,   419,   420,
     421,   422,   423,   424,   425,     0,     0,     0,     0,   102,
       0,     0,     0,  -273,     0,     0,   316,   102,   415,     0,
     416,   417,   418,   419,   420,   421,   422,   423,   424,   425,
       0,     0,     0,     0,     0,   102,   102,     0,   355,     0,
     356,   357,   358,   359,   103,     0,     0,     0,     0,   102,
       0,     0,     0,     0,     0,     0,   360,     0,     0,   361,
       0,   102,   102,     0,     0,     0,     0,     0,     0,   102,
       0,   570,     0,     0,     0,     0,     0,     0,     0,     0,
     102,   102,   362,     0,     0,     0,     0,     0,   363,   364,
     365,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   128,     0,     0,     0,     0,   128,     0,
       0,     0,     0,     0,     0,   366,     0,     0,   367,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   102,   102,     0,     0,   963,   103,     0,     0,   102,
       0,     0,     0,     0,   103,   103,     0,     0,   647,   648,
       0,   103,   649,     0,     0,     0,   103,   103,     0,     0,
       0,     0,   103,   103,     0,     0,     0,   175,   176,   177,
     178,   179,   180,   181,   182,   183,   103,     0,   184,   185,
       0,     0,     0,     0,   186,   187,   188,   189,   103,   103,
       0,     0,     0,     0,     0,   102,   103,   102,   190,   191,
     102,     0,     0,     0,     0,     0,     0,   103,   103,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,     0,   202,   203,     0,     0,     0,     0,     0,     0,
     204,   274,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   103,
       0,     0,     0,     0,     0,     0,   103,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -604,     4,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,     0,
       0,     0,     0,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,   103,    27,   103,     0,     0,   103,     0,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,  -288,     0,    64,  -604,
       0,     0,  -604,  -604,     0,     0,     0,     0,     0,  -288,
    -288,  -288,  -288,  -288,  -288,     0,  -288,     0,    65,    66,
      67,     0,     0,  -288,  -288,  -288,     0,     0,     0,     0,
    -604,     0,  -604,  -288,  -288,     0,  -288,  -288,  -288,  -288,
    -288,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -288,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -288,  -288,
    -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,
    -288,     0,     0,     0,     0,  -288,  -288,  -288,     0,     0,
    -288,     0,     0,     0,     0,     0,  -288,     0,     0,     0,
       0,  -288,     0,     0,     0,     0,     0,     0,     0,  -288,
       0,  -288,     0,     0,  -288,  -288,     0,     0,  -288,  -288,
    -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,  -288,
       0,     0,  -409,     0,     0,  -288,  -288,  -288,  -288,     0,
       0,  -288,  -288,  -288,  -288,  -409,  -409,  -409,  -409,  -409,
    -409,     0,  -409,     0,     0,     0,     0,     0,  -409,  -409,
    -409,     0,     0,     0,     0,     0,     0,     0,     0,  -409,
    -409,     0,  -409,  -409,  -409,  -409,  -409,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -409,  -409,  -409,  -409,  -409,  -409,
    -409,  -409,  -409,  -409,  -409,  -409,  -409,     0,     0,     0,
       0,  -409,  -409,  -409,     0,     0,  -409,     0,     0,     0,
       0,     0,  -409,     0,     0,     0,     0,  -409,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -409,     0,     0,
    -409,  -409,     0,     0,  -409,     0,  -409,  -409,  -409,  -409,
    -409,  -409,  -409,  -409,  -409,  -409,     0,     0,  -475,     0,
    -409,  -409,  -409,  -409,  -409,     0,   274,  -409,  -409,  -409,
    -409,  -475,  -475,  -475,  -475,  -475,  -475,     0,  -475,     0,
       0,     0,     0,     0,     0,  -475,  -475,     0,     0,     0,
       0,     0,     0,     0,     0,  -475,  -475,     0,  -475,  -475,
    -475,  -475,  -475,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   489,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,
    -475,  -475,  -475,     0,     0,     0,     0,  -475,  -475,  -475,
       0,  -475,  -475,     0,     0,     0,     0,     0,  -475,     0,
       0,     0,     0,  -475,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -475,     0,     0,  -475,  -475,     0,  -475,
    -475,     0,  -475,  -475,  -475,  -475,  -475,  -475,  -475,  -475,
    -475,  -475,     0,     0,  -604,     0,     0,  -475,  -475,  -475,
    -475,     0,     0,  -475,  -475,  -475,  -475,  -604,  -604,  -604,
    -604,  -604,  -604,     0,  -604,     0,     0,     0,     0,     0,
    -604,  -604,  -604,     0,     0,     0,     0,     0,     0,     0,
       0,  -604,  -604,     0,  -604,  -604,  -604,  -604,  -604,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -604,  -604,  -604,  -604,
    -604,  -604,  -604,  -604,  -604,  -604,  -604,  -604,  -604,     0,
       0,     0,     0,  -604,  -604,  -604,     0,     0,  -604,     0,
       0,     0,     0,     0,  -604,     0,     0,     0,     0,  -604,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -604,
       0,     0,  -604,  -604,     0,     0,  -604,     0,  -604,  -604,
    -604,  -604,  -604,  -604,  -604,  -604,  -604,  -604,     0,     0,
    -604,     0,  -604,  -604,  -604,  -604,  -604,     0,   274,  -604,
    -604,  -604,  -604,  -604,  -604,  -604,  -604,  -604,  -604,     0,
    -604,     0,     0,     0,     0,     0,     0,  -604,  -604,     0,
       0,     0,     0,     0,     0,     0,     0,  -604,  -604,     0,
    -604,  -604,  -604,  -604,  -604,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -604,  -604,  -604,  -604,  -604,  -604,  -604,  -604,
    -604,  -604,  -604,  -604,  -604,     0,     0,     0,     0,  -604,
    -604,  -604,     0,     0,  -604,     0,     0,     0,     0,     0,
    -604,     0,     0,     0,     0,  -604,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -604,     0,     0,  -604,  -604,
       0,     0,  -604,     0,  -604,  -604,  -604,  -604,  -604,  -604,
    -604,  -604,  -604,  -604,     0,     0,  -581,     0,     0,  -604,
    -604,  -604,  -604,     0,   274,  -604,  -604,  -604,  -604,  -581,
    -581,  -581,     0,  -581,  -581,     0,  -581,     0,     0,     0,
       0,     0,  -581,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -581,  -581,     0,  -581,  -581,  -581,  -581,
    -581,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -581,  -581,
    -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,
    -581,     0,     0,     0,     0,  -581,  -581,  -581,     0,   784,
    -581,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -581,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -581,     0,     0,  -581,  -581,     0,  -104,  -581,     0,
    -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,  -581,
       0,     0,  -581,     0,  -581,  -581,  -581,     0,   -96,     0,
       0,  -581,  -581,  -581,  -581,  -581,  -581,  -581,     0,  -581,
    -581,     0,  -581,     0,     0,     0,     0,     0,  -581,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -581,
    -581,     0,  -581,  -581,  -581,  -581,  -581,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -581,  -581,  -581,  -581,  -581,  -581,
    -581,  -581,  -581,  -581,  -581,  -581,  -581,     0,     0,     0,
       0,  -581,  -581,  -581,     0,   784,  -581,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -581,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -581,     0,     0,
    -581,  -581,     0,  -104,  -581,     0,  -581,  -581,  -581,  -581,
    -581,  -581,  -581,  -581,  -581,  -581,     0,     0,  -297,     0,
    -581,  -581,  -581,     0,  -581,     0,     0,  -581,  -581,  -581,
    -581,  -297,  -297,  -297,     0,  -297,  -297,     0,  -297,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -297,  -297,     0,  -297,  -297,
    -297,  -297,  -297,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,
    -297,  -297,  -297,     0,     0,     0,     0,  -297,  -297,  -297,
       0,   785,  -297,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -297,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -297,     0,     0,  -297,  -297,     0,  -106,
    -297,     0,  -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,
    -297,  -297,     0,     0,  -297,     0,     0,  -297,  -297,     0,
     -98,     0,     0,  -297,  -297,  -297,  -297,  -297,  -297,  -297,
       0,  -297,  -297,     0,  -297,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -297,  -297,     0,  -297,  -297,  -297,  -297,  -297,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -297,  -297,  -297,  -297,
    -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,     0,
       0,     0,     0,  -297,  -297,  -297,     0,   785,  -297,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -297,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -297,
       0,     0,  -297,  -297,     0,  -106,  -297,     0,  -297,  -297,
    -297,  -297,  -297,  -297,  -297,  -297,  -297,  -297,     0,     0,
       0,     0,     0,  -297,  -297,     0,  -297,     0,     0,  -297,
    -297,  -297,  -297,   294,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,  -604,  -604,  -604,     0,     0,
    -604,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,  -604,     0,     0,
    -604,  -604,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -604,   294,
    -604,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -604,     0,  -604,  -604,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,  -604,     0,     0,  -604,  -604,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -604,   294,  -604,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,  -604,
       0,     0,  -604,    15,  -604,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,  -604,
       0,     0,  -604,  -604,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -604,   294,  -604,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,  -604,     0,     0,  -604,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,    57,     0,    58,    59,    60,    61,
      62,    63,     0,     0,    64,  -604,     0,     0,  -604,  -604,
       4,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,    65,    66,    67,     0,    15,     0,
      16,    17,    18,    19,     0,     0,  -604,     0,  -604,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,    51,     0,     0,    52,    53,     0,    54,    55,     0,
      56,     0,     0,    57,     0,    58,    59,    60,    61,    62,
      63,     0,     0,    64,  -604,     0,     0,  -604,  -604,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,    67,     0,     0,  -604,     0,
       0,     0,     0,     0,     0,  -604,   294,  -604,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,  -604,
    -604,     0,     0,     0,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,    51,     0,     0,
      52,    53,     0,    54,    55,     0,    56,     0,     0,    57,
       0,    58,    59,    60,    61,    62,    63,     0,     0,    64,
    -604,     0,     0,  -604,  -604,   294,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    65,
      66,    67,     0,    15,     0,    16,    17,    18,    19,     0,
       0,  -604,     0,  -604,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,   295,
      53,     0,    54,    55,     0,    56,     0,     0,    57,     0,
      58,    59,    60,    61,    62,    63,     0,     0,    64,  -604,
       0,     0,  -604,  -604,   294,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,    65,    66,
      67,     0,    15,     0,    16,    17,    18,    19,     0,  -604,
    -604,     0,  -604,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,    51,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,    57,     0,    58,
      59,    60,    61,    62,    63,     0,     0,    64,  -604,     0,
       0,  -604,  -604,   294,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    65,    66,    67,
       0,    15,     0,    16,    17,    18,    19,     0,  -604,  -604,
       0,  -604,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,    57,     0,    58,    59,
      60,    61,    62,    63,     0,     0,    64,  -604,     0,     0,
    -604,  -604,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,    67,     0,
       0,  -604,     0,     0,     0,     0,     0,     0,  -604,   294,
    -604,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -604,     0,     0,     0,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,    57,     0,    58,    59,    60,    61,    62,    63,
       0,     0,    64,  -604,     0,     0,  -604,  -604,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    65,    66,    67,     0,    15,     0,    16,    17,
      18,    19,     0,     0,  -604,     0,  -604,    20,    21,    22,
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
       0,     0,     0,     0,   204,   205,  -574,  -574,  -574,  -574,
    -574,  -574,  -574,  -574,  -574,     0,     0,     0,     0,     0,
       0,     0,  -574,     0,  -574,  -574,  -574,  -574,     0,  -574,
       0,     0,     0,  -574,  -574,  -574,  -574,  -574,  -574,  -574,
       0,     0,  -574,     0,     0,     0,     0,     0,     0,     0,
       0,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,  -574,
       0,  -574,  -574,  -574,     0,     0,  -574,     0,     0,  -574,
    -574,     0,  -574,  -574,  -574,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -574,  -574,
       0,     0,     0,     0,     0,  -574,     0,     0,  -574,  -574,
       0,  -574,  -574,     0,  -574,  -574,  -574,  -574,     0,  -574,
    -574,  -574,  -574,  -574,  -574,     0,     0,  -574,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -574,  -574,  -574,
       0,  -574,     0,     0,     0,     0,     0,  -574,  -575,  -575,
    -575,  -575,  -575,  -575,  -575,  -575,  -575,     0,     0,     0,
       0,     0,     0,     0,  -575,     0,  -575,  -575,  -575,  -575,
       0,  -575,     0,     0,     0,  -575,  -575,  -575,  -575,  -575,
    -575,  -575,     0,     0,  -575,     0,     0,     0,     0,     0,
       0,     0,     0,  -575,  -575,  -575,  -575,  -575,  -575,  -575,
    -575,  -575,     0,  -575,  -575,  -575,     0,     0,  -575,     0,
       0,  -575,  -575,     0,  -575,  -575,  -575,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -575,  -575,     0,     0,     0,     0,     0,  -575,     0,     0,
    -575,  -575,     0,  -575,  -575,     0,  -575,  -575,  -575,  -575,
       0,  -575,  -575,  -575,  -575,  -575,  -575,     0,     0,  -575,
       0,     0,     0,     0,     0,     0,  -577,  -577,  -577,  -577,
    -577,  -577,  -577,  -577,  -577,     0,     0,     0,     0,  -575,
    -575,  -575,  -577,  -575,  -577,  -577,  -577,  -577,     0,  -575,
       0,     0,     0,  -577,  -577,  -577,  -577,  -577,  -577,  -577,
       0,     0,  -577,     0,     0,     0,     0,     0,     0,     0,
       0,  -577,  -577,  -577,  -577,  -577,  -577,  -577,  -577,  -577,
       0,  -577,  -577,  -577,     0,     0,  -577,     0,     0,  -577,
    -577,     0,  -577,  -577,  -577,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -577,  -577,
       0,     0,     0,     0,     0,  -577,   815,     0,  -577,  -577,
       0,  -577,  -577,     0,  -577,  -577,  -577,  -577,     0,  -577,
    -577,  -577,  -577,  -577,  -577,     0,     0,  -577,     0,     0,
       0,     0,     0,     0,  -104,  -578,  -578,  -578,  -578,  -578,
    -578,  -578,  -578,  -578,     0,     0,     0,  -577,  -577,  -577,
       0,  -578,     0,  -578,  -578,  -578,  -578,  -577,     0,     0,
       0,     0,  -578,  -578,  -578,  -578,  -578,  -578,  -578,     0,
       0,  -578,     0,     0,     0,     0,     0,     0,     0,     0,
    -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,  -578,     0,
    -578,  -578,  -578,     0,     0,  -578,     0,     0,  -578,  -578,
       0,  -578,  -578,  -578,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -578,  -578,     0,
       0,     0,     0,     0,  -578,   816,     0,  -578,  -578,     0,
    -578,  -578,     0,  -578,  -578,  -578,  -578,     0,  -578,  -578,
    -578,  -578,  -578,  -578,     0,     0,  -578,     0,     0,     0,
       0,     0,     0,  -106,  -579,  -579,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,     0,     0,     0,  -578,  -578,  -578,     0,
    -579,     0,  -579,  -579,  -579,  -579,  -578,     0,     0,     0,
       0,  -579,  -579,  -579,  -579,  -579,  -579,  -579,     0,     0,
    -579,     0,     0,     0,     0,     0,     0,     0,     0,  -579,
    -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,     0,  -579,
    -579,  -579,     0,     0,  -579,     0,     0,  -579,  -579,     0,
    -579,  -579,  -579,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -579,  -579,     0,     0,
       0,     0,     0,  -579,     0,     0,  -579,  -579,     0,  -579,
    -579,     0,  -579,  -579,  -579,  -579,     0,  -579,  -579,  -579,
    -579,  -579,  -579,     0,     0,  -579,     0,     0,     0,     0,
       0,     0,  -580,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,     0,     0,     0,     0,  -579,  -579,  -579,  -580,     0,
    -580,  -580,  -580,  -580,     0,  -579,     0,     0,     0,  -580,
    -580,  -580,  -580,  -580,  -580,  -580,     0,     0,  -580,     0,
       0,     0,     0,     0,     0,     0,     0,  -580,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,  -580,     0,  -580,  -580,  -580,
       0,     0,  -580,     0,     0,  -580,  -580,     0,  -580,  -580,
    -580,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -580,  -580,     0,     0,     0,     0,
       0,  -580,     0,     0,  -580,  -580,     0,  -580,  -580,     0,
    -580,  -580,  -580,  -580,     0,  -580,  -580,  -580,  -580,  -580,
    -580,     0,     0,  -580,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -580,  -580,  -580,     0,     0,     0,     0,
       0,     0,     0,  -580,   132,   133,   134,   135,   136,   137,
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
       0,     0,   480,     0,     0,     0,     0,     0,     0,     0,
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
       0,   600,     0,    20,    21,    22,    23,    24,    25,    26,
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
       0,     0,     0,     0,     0,   502,    49,    50,     0,     0,
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
     120,    53,     0,    54,    55,     0,   709,   261,   262,    57,
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
     709,   261,     0,    57,     0,    58,    59,    60,    61,    62,
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
     120,    53,     0,    54,    55,     0,   594,     0,     0,    57,
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
      55,     0,   594,     0,     0,    57,     0,    58,    59,    60,
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
     874,     0,     0,    57,     0,    58,    59,    60,    61,    62,
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
       0,     0,   120,    53,     0,    54,    55,     0,   709,     0,
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
       0,     0,   395,    53,     0,    54,    55,     0,   396,     0,
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
     395,    53,     0,    54,    55,     0,     0,     0,     0,    57,
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
       0,   937,     0,     0,   120,    53,     0,    54,    55,     0,
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
       0,     0,     0,     0,     0,   960,     0,     0,   120,    53,
       0,    54,    55,     0,   668,   640,     0,    57,   669,    58,
      59,    60,    61,    62,    63,     0,     0,    64,     0,     0,
       0,     0,     0,   175,   176,   177,   178,   179,   180,   181,
     182,   183,     0,     0,   184,   185,     0,   121,     0,     0,
     186,   187,   188,   189,     0,     0,     0,   399,  -605,  -605,
    -605,  -605,   404,   405,   190,   191,  -605,  -605,     0,     0,
       0,     0,     0,     0,   412,   413,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,     0,   202,   203,
     653,   648,     0,     0,   654,     0,   204,   274,     0,   416,
     417,   418,   419,   420,   421,   422,   423,   424,   425,   175,
     176,   177,   178,   179,   180,   181,   182,   183,     0,     0,
     184,   185,     0,     0,     0,     0,   186,   187,   188,   189,
       0,     0,     0,   399,   400,   401,   402,   403,   404,   405,
     190,   191,   408,   409,     0,     0,     0,     0,     0,     0,
     412,   413,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   192,   193,   194,   195,   196,   197,   198,
     199,   200,   201,     0,   202,   203,   683,   640,     0,     0,
     684,     0,   204,   274,     0,   416,   417,   418,   419,   420,
     421,   422,   423,   424,   425,   175,   176,   177,   178,   179,
     180,   181,   182,   183,     0,     0,   184,   185,     0,     0,
       0,     0,   186,   187,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   192,
     193,   194,   195,   196,   197,   198,   199,   200,   201,     0,
     202,   203,   686,   648,     0,     0,   687,     0,   204,   274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   175,   176,   177,   178,   179,   180,   181,   182,   183,
       0,     0,   184,   185,     0,     0,     0,     0,   186,   187,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,     0,   202,   203,   693,   640,
       0,     0,   694,     0,   204,   274,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   175,   176,   177,
     178,   179,   180,   181,   182,   183,     0,     0,   184,   185,
       0,     0,     0,     0,   186,   187,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190,   191,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,     0,   202,   203,   696,   648,     0,     0,   697,     0,
     204,   274,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   175,   176,   177,   178,   179,   180,   181,
     182,   183,     0,     0,   184,   185,     0,     0,     0,     0,
     186,   187,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,     0,   202,   203,
     732,   640,     0,     0,   733,     0,   204,   274,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   175,
     176,   177,   178,   179,   180,   181,   182,   183,     0,     0,
     184,   185,     0,     0,     0,     0,   186,   187,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   192,   193,   194,   195,   196,   197,   198,
     199,   200,   201,     0,   202,   203,   735,   648,     0,     0,
     736,     0,   204,   274,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   175,   176,   177,   178,   179,
     180,   181,   182,   183,     0,     0,   184,   185,     0,     0,
       0,     0,   186,   187,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   192,
     193,   194,   195,   196,   197,   198,   199,   200,   201,     0,
     202,   203,   879,   640,     0,     0,   880,     0,   204,   274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   175,   176,   177,   178,   179,   180,   181,   182,   183,
       0,     0,   184,   185,     0,     0,     0,     0,   186,   187,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,     0,   202,   203,   882,   648,
       0,     0,   883,     0,   204,   274,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   175,   176,   177,
     178,   179,   180,   181,   182,   183,     0,     0,   184,   185,
       0,     0,     0,     0,   186,   187,   188,   189,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   190,   191,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     201,     0,   202,   203,  1019,   640,     0,     0,  1020,     0,
     204,   274,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   175,   176,   177,   178,   179,   180,   181,
     182,   183,     0,     0,   184,   185,     0,     0,     0,     0,
     186,   187,   188,   189,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   190,   191,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,     0,   202,   203,
    1031,   640,     0,     0,  1032,     0,   204,   274,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   175,
     176,   177,   178,   179,   180,   181,   182,   183,     0,     0,
     184,   185,     0,     0,     0,     0,   186,   187,   188,   189,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     190,   191,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   192,   193,   194,   195,   196,   197,   198,
     199,   200,   201,     0,   202,   203,  1034,   648,     0,     0,
    1035,     0,   204,   274,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   175,   176,   177,   178,   179,
     180,   181,   182,   183,     0,     0,   184,   185,     0,     0,
       0,     0,   186,   187,   188,   189,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   190,   191,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   192,
     193,   194,   195,   196,   197,   198,   199,   200,   201,     0,
     202,   203,   653,   648,     0,     0,   654,     0,   204,   274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   175,   176,   177,   178,   179,   180,   181,   182,   183,
       0,     0,   184,   185,     0,     0,     0,     0,   186,   187,
     188,   189,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   190,   191,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   848,     0,     0,
       0,     0,     0,     0,     0,   192,   193,   194,   195,   196,
     197,   198,   199,   200,   201,     0,   202,   203,   859,     0,
       0,     0,     0,     0,   204,   399,   400,   401,   402,   403,
     404,   405,   406,   407,   408,   409,   410,   411,     0,     0,
       0,     0,   412,   413,     0,     0,   399,   400,   401,   402,
     403,   404,   405,   406,   407,   408,   409,   410,   411,     0,
       0,     0,     0,   412,   413,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   415,     0,   416,   417,   418,
     419,   420,   421,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   415,     0,   416,   417,
     418,   419,   420,   421,   422,   423,   424,   425,   399,   400,
     401,   402,   403,   404,   405,   406,   407,   408,   409,   410,
     411,     0,     0,     0,     0,   412,   413,     0,     0,     0,
       0,     0,     0,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,     0,     0,     0,     0,
     412,   413,     0,     0,     0,     0,     0,     0,   415,     0,
     416,   417,   418,   419,   420,   421,   422,   423,   424,   425,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   415,   250,   416,   417,   418,   419,   420,
     421,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,     0,  -273,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,     0,     0,     0,
       0,   412,   413,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,     0,     0,     0,     0,
     412,   413,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   415,     0,   416,   417,   418,   419,
     420,   421,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   415,  -274,   416,   417,   418,   419,   420,
     421,   422,   423,   424,   425,     0,     0,     0,     0,     0,
       0,     0,     0,  -275,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,     0,     0,     0,
       0,   412,   413,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,     0,     0,     0,     0,
     412,   413,     0,     0,     0,   414,     0,     0,     0,     0,
       0,     0,     0,     0,   415,     0,   416,   417,   418,   419,
     420,   421,   422,   423,   424,   425,     0,     0,     0,     0,
       0,     0,     0,   415,  -276,   416,   417,   418,   419,   420,
     421,   422,   423,   424,   425,   399,   400,   401,   402,   403,
     404,   405,   406,   407,   408,   409,   410,   411,     0,     0,
       0,     0,   412,   413,     0,     0,     0,   494,   399,   400,
     401,   402,   403,   404,   405,   406,   407,   408,   409,   410,
     411,     0,     0,     0,     0,   412,   413,     0,     0,     0,
       0,     0,     0,     0,     0,   415,     0,   416,   417,   418,
     419,   420,   421,   422,   423,   424,   425,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   415,     0,
     416,   417,   418,   419,   420,   421,   422,   423,   424,   425,
     399,   400,   401,   402,   403,   404,   405,   406,   407,   408,
     409,  -605,  -605,     0,     0,     0,     0,   412,   413,   399,
     400,   401,   402,   403,   404,   405,   406,     0,   408,   409,
       0,     0,     0,     0,     0,     0,   412,   413,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   416,   417,   418,   419,   420,   421,   422,   423,
     424,   425,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425
};

static const yytype_int16 yycheck[] =
{
       2,    16,    17,    27,    10,    20,    88,    89,   501,    15,
       2,    27,     4,     5,     6,    90,    28,     9,    10,    21,
       7,    13,   474,    15,    16,    17,   313,    56,    20,    22,
      16,    17,   221,    69,    20,   270,   330,     4,   531,    54,
      55,    14,   375,   119,     7,   398,    16,    17,    25,   585,
      20,    14,    54,    55,   318,    28,   483,    66,   306,   428,
      52,   768,   310,    83,    56,    28,   426,   745,    54,    55,
     430,    58,   768,   433,    66,   489,   497,    92,    16,    17,
     501,    25,    20,   295,   453,    16,    17,    29,    15,    20,
       2,    83,     4,    25,   454,    58,    52,   679,   539,   468,
      60,    61,    62,    63,   368,    25,   688,   467,   477,   469,
     112,    21,    22,   106,   923,    26,    54,    25,   478,    91,
     331,   656,   657,   334,    25,   336,   118,   338,   120,   340,
      26,   945,    72,     5,     6,   217,     5,     6,     0,    25,
      25,    13,    69,   121,    13,    93,   228,   138,    26,   268,
      93,   270,    75,    76,    57,    57,   103,   517,    76,   600,
      93,    55,   142,    60,   142,   142,    63,   144,   148,   787,
      28,   113,   541,   121,   792,   147,    91,   582,   121,    93,
      91,   128,   542,   395,    56,   121,   305,    56,   121,   129,
     130,   131,   132,   133,   105,   489,   106,   299,   142,   122,
     123,   303,   112,   113,  1013,   123,   582,   289,  1022,   585,
     142,    83,   144,   110,    83,   121,   222,   223,   121,   211,
     130,   123,   142,   142,    51,    91,   719,   138,    72,   140,
     222,   223,   147,   315,   142,   942,   147,    80,   945,   105,
      91,   142,   138,   939,   398,    72,   942,    16,   144,   945,
     928,   947,   539,    91,   241,   549,   142,   142,   125,   274,
     138,   297,   797,   278,   144,    18,   268,    20,   270,   305,
     306,   506,   274,    91,   310,   102,   103,    91,   241,   543,
     144,   147,   274,   126,   438,   439,   278,   131,   132,   133,
     282,   283,   278,   313,   708,   287,   147,   718,   719,   995,
      52,   128,   294,   295,   274,   121,   438,   439,   278,   147,
     302,    55,   144,   600,   850,  1022,   148,    58,    59,    61,
      93,   313,    64,    65,   251,   678,  1022,    91,  1024,   147,
    1026,    93,  1028,   147,    55,   776,   547,    93,   920,   295,
     278,    91,   348,   349,   350,   351,   115,   278,   121,   118,
     119,    25,  1048,    37,    38,   347,   348,   349,   350,   351,
     352,   353,   354,   323,   142,   121,    93,   396,   120,    91,
     297,   373,   824,   375,   116,   117,   452,   146,   370,   148,
     347,    91,   294,   147,   393,    20,   505,   506,   370,   398,
     302,    93,   768,    16,   121,   318,    93,   147,   390,    93,
     502,   393,   142,   395,   396,    93,   398,   509,   390,     9,
      10,   705,   781,   428,   708,    15,    58,    59,   520,   121,
     780,   754,   782,    57,   121,   147,   428,   121,   921,   438,
     439,   652,   138,   121,   655,   347,   428,   147,   453,   395,
     352,   313,   847,    72,   313,   368,   438,   439,   142,   854,
     145,   453,   673,   468,   142,   393,    93,   121,   428,   461,
     398,   453,   477,   455,   456,   141,   468,   430,   570,   571,
     433,   495,   464,   455,   850,   477,   468,   712,   597,   495,
     472,   729,   497,   453,   121,   477,   501,    17,    18,   776,
     482,   454,   115,   920,   139,   118,   119,   599,   468,   142,
     921,   513,    93,   505,   506,    61,   469,   477,    64,    65,
      93,   497,   514,   528,   703,   478,   531,   537,   118,   539,
     430,    55,   514,   146,   396,   148,   541,   396,   610,    93,
     121,   523,   865,   866,    72,   287,   830,   942,   121,   541,
     513,    72,   528,   295,   454,   537,   121,   539,   486,   541,
     513,     2,   142,     4,   517,    51,   548,   121,   142,   469,
     116,   117,    13,    63,    64,    65,   942,    51,   478,   945,
      51,   541,   142,   675,    55,   214,   142,   504,   142,   542,
     600,     2,   221,     4,   703,   861,   862,   142,     9,    10,
     384,   593,   386,   712,    15,    16,    17,    51,   833,    20,
     121,    52,   514,   142,   628,    75,    76,   517,   600,    51,
    1015,   523,   628,   100,    15,    13,   116,   117,    63,   258,
     543,  1026,   222,   223,   778,    16,   728,    63,    64,    65,
     784,    52,   542,     5,     6,   789,   790,    15,   145,  1008,
     145,    13,    91,   395,   746,    66,  1022,   139,   142,  1009,
     652,   644,   784,   655,   656,   657,   105,   789,   790,   652,
     666,   994,   655,    15,    15,   537,   142,   539,   537,   120,
     539,   673,   141,    44,   666,   667,   678,   679,   121,   681,
     116,   117,   282,   283,    56,   667,   688,   699,   141,   676,
      15,   140,   698,   729,    18,   144,    15,   118,   147,   120,
     141,   141,   139,   718,   719,   139,   698,   142,   347,   141,
     712,    83,   464,   676,   833,    91,    93,   906,   793,    93,
     472,   737,    16,   912,   148,   139,   699,    57,   600,   105,
     482,   600,   718,   142,   644,   142,   699,   891,   377,    93,
     678,    15,   652,   142,   121,   655,   142,   121,   348,   349,
     350,   351,   754,   353,   354,    94,   776,    14,    91,   891,
     211,   671,    15,   673,   140,   142,   781,   121,   142,   778,
     115,   147,   105,   118,   119,   784,   785,   769,   575,   781,
     789,   790,   579,    15,   776,   777,   778,   769,   142,   781,
     211,   146,   784,   785,   142,   797,   548,   789,   790,   145,
     142,   222,   223,   795,   796,   807,   812,   140,   810,    15,
     142,   781,   115,   795,   147,   118,   119,   809,   141,   782,
     812,   115,    16,    15,   118,   119,   142,   575,   142,   821,
     822,   833,   139,    15,   473,   474,   287,   829,    40,    41,
      42,    43,    44,   294,   295,   861,   862,    15,   840,   841,
      15,   302,   146,   274,   148,   139,   456,   278,   840,   142,
     126,   282,   283,   865,   866,   777,   287,   126,    55,   139,
      15,   863,   782,   294,   295,   115,   868,    72,   118,   119,
     962,   302,   891,    55,   523,    51,   142,    53,    54,    55,
      56,   530,   142,    27,    89,    90,   347,   142,   142,   891,
     142,   352,    15,    69,   776,   142,   921,   776,   148,   901,
     902,   144,   141,   905,   144,   142,   514,   909,   920,   370,
     902,   115,    13,     6,   118,   119,   347,   348,   349,   350,
     351,   352,   353,   354,   129,   130,   131,   132,   133,   390,
    1011,   313,   768,    62,   395,    64,    65,   398,   142,   370,
    1013,   975,   146,   794,   148,  1010,   253,     7,   575,   975,
     939,   942,   582,   269,   758,   759,   760,    -1,   762,   390,
     764,   768,   393,   965,   395,   967,   142,   398,   970,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,   438,   439,    -1,
      -1,   863,   994,  1008,   863,    -1,   868,   116,   117,   868,
      26,    -1,    37,    38,   455,    -1,  1008,   428,  1010,  1011,
      -1,    91,   146,   464,   148,    -1,  1008,   438,   439,   115,
     768,   472,   118,   119,   396,   105,   665,    -1,    -1,    -1,
      -1,   482,   453,   905,   455,   456,   905,    -1,  1008,    91,
      26,    -1,    -1,   464,   796,    91,  1009,   468,    -1,   145,
     146,   472,   148,   105,    -1,    -1,   477,   809,    -1,   105,
     140,   482,    -1,   514,   703,    91,   666,   147,    26,   821,
     822,    -1,   523,    -1,    -1,   438,   439,   829,    -1,   105,
     115,    -1,    -1,   118,   119,    -1,    -1,    -1,   140,   841,
      -1,    -1,    -1,   514,   140,   147,    -1,   548,   698,  1009,
      -1,   147,   523,    -1,    -1,    91,    -1,   470,   471,   870,
     871,   146,   138,   148,   140,    -1,    -1,    -1,   144,   105,
     541,   147,    62,    -1,    64,    65,    51,   548,    53,    54,
      55,    56,    -1,    91,    -1,    -1,   930,   931,   932,   933,
      26,    -1,   939,    -1,    69,   942,    -1,   105,   945,   901,
     947,    -1,   138,    -1,   140,   518,    -1,   909,   144,    -1,
     799,   147,    -1,     9,    10,   537,    -1,   539,    -1,    15,
      16,    17,    -1,    -1,    20,    -1,   116,   117,    -1,    -1,
     138,   942,   140,    -1,    26,   824,   144,    -1,    -1,   147,
      -1,   939,    -1,    -1,   942,    -1,    -1,   945,   995,   947,
      -1,    47,    48,    49,    50,    91,    -1,    -1,    54,    55,
      -1,    -1,   812,   965,    -1,   967,   667,   142,   970,   105,
      66,    67,    -1,    -1,  1018,  1022,    -1,  1024,   600,  1026,
      91,  1028,    -1,    -1,    -1,    -1,    -1,   998,   999,  1000,
      -1,  1002,  1003,   569,   105,   666,   667,   995,    -1,    91,
      -1,  1048,   138,    72,   140,    -1,    -1,    -1,   144,   585,
      -1,   147,   588,   105,    -1,    -1,    -1,   906,    -1,    -1,
      89,    90,   118,   912,  1022,    -1,  1024,   698,  1026,   140,
    1028,  1042,  1043,  1044,  1045,    51,   147,    53,    54,    55,
      56,  1052,    -1,    63,    64,    65,   138,    -1,   140,    -1,
    1048,    -1,   144,    69,    -1,   147,    72,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    91,   769,    -1,
      -1,   115,    -1,    72,   118,   119,   777,   778,    -1,    95,
      -1,   105,    -1,   784,    -1,   101,   102,   103,   789,   790,
      89,    90,    -1,    -1,   795,   796,   116,   117,   769,    -1,
      -1,    -1,   146,    91,   148,    -1,   777,   778,   809,    -1,
     781,    -1,   128,   784,   785,   131,   140,   105,   789,   790,
     821,   822,    -1,   147,   795,   796,   222,   223,   829,   128,
     129,   130,   131,   132,   133,    91,    -1,    -1,   809,   840,
     841,   812,    -1,    -1,     2,    -1,     4,     5,     6,   105,
     821,   822,   140,    -1,   776,    13,    -1,    -1,   829,   147,
      91,    91,    -1,    -1,   260,   261,   262,   263,    -1,   840,
     841,   784,   785,    -1,   105,   105,   789,   790,   274,    91,
      -1,    -1,   278,    91,   140,    -1,   282,   283,    -1,    -1,
     891,   147,   768,   105,    52,    -1,    -1,   105,    56,    -1,
     901,   902,   815,   816,   905,   818,   819,    91,   909,   140,
     140,    -1,    -1,    -1,    -1,    -1,   147,   147,    -1,    -1,
     891,   105,    -1,    -1,    -1,    83,    -1,    -1,   140,    -1,
     901,   902,   140,    -1,    -1,   147,    -1,    -1,   909,   147,
     115,   863,    -1,   118,   119,    -1,   868,    -1,    -1,    -1,
      -1,    -1,   348,   349,   350,   351,   140,   353,   354,    -1,
      -1,    -1,   120,   147,   965,    -1,   967,   142,    -1,   970,
      -1,   146,    -1,   148,   850,   371,   852,    -1,   891,    -1,
     856,    -1,    -1,   905,    -1,    -1,   382,    63,    64,    65,
      -1,    63,    64,    65,   965,    -1,   967,   393,    -1,   970,
      -1,   914,   398,   399,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   413,    -1,   415,
     416,   417,   418,   419,   420,   421,   422,   423,   424,   425,
      -1,    -1,   428,    63,    64,    65,    -1,  1008,    -1,    -1,
     116,   117,   438,   439,   116,   117,    63,    64,    65,    -1,
     926,   927,    -1,   211,    63,    64,    65,   453,    -1,    -1,
     456,    -1,     2,    -1,     4,     5,     6,     7,    -1,   945,
     466,   947,   468,    13,   470,   471,    -1,    -1,    -1,    -1,
      -1,   477,    -1,    -1,    -1,    -1,   116,   117,    -1,    -1,
     486,    -1,    -1,    -1,   490,    -1,    -1,    -1,   494,   116,
     117,   497,    -1,   499,    -1,   501,   982,   116,   117,   985,
      -1,    -1,    52,    -1,    -1,    51,    56,    53,    54,    55,
      56,    51,   518,    53,    54,    55,    56,    -1,    -1,   287,
      -1,    -1,   528,    69,    -1,   531,   294,   295,    -1,    69,
      -1,  1017,    -1,    83,   302,   541,  1022,    -1,  1024,    -1,
      -1,    -1,  1028,    -1,    -1,   313,    -1,    -1,    -1,    95,
      -1,   557,   558,    -1,    -1,   101,    -1,    -1,    -1,    -1,
      -1,    -1,  1048,    -1,    -1,    -1,   572,    -1,    -1,    -1,
     120,    -1,    -1,    51,    52,    -1,    -1,    55,    -1,   347,
      -1,    -1,    -1,    -1,   352,   591,    -1,    -1,   594,     2,
      -1,     4,    70,    71,    72,    73,    74,    75,    76,    77,
      78,    -1,   370,    81,    82,    -1,    -1,    -1,    -1,    87,
      88,    89,    90,    -1,    -1,    51,    -1,    53,    54,    55,
      56,    -1,   390,   101,   102,    -1,    -1,   395,   396,    -1,
     398,    -1,    -1,    69,    -1,    -1,    -1,    -1,    -1,    52,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    95,
     666,   211,    -1,    -1,    -1,   143,   144,    -1,    -1,    -1,
     438,   439,   678,    51,    -1,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,
      -1,    69,   698,    -1,    72,    -1,   464,    51,    -1,    53,
      54,    55,    56,   709,   472,    -1,    -1,   120,    -1,    -1,
      -1,    -1,   718,   719,   482,    69,     0,    95,    72,    -1,
      -1,    -1,    -1,    -1,   102,   103,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,   287,    -1,    -1,
      -1,    95,    26,    27,   294,   295,   514,   101,   102,   103,
     128,    -1,   302,    37,    38,   523,    40,    41,    42,    43,
      44,    -1,    -1,   313,    -1,    -1,    -1,   773,    -1,   537,
      -1,   539,   778,   779,   128,   781,    -1,   131,   784,   785,
     548,    -1,    -1,   789,   790,    -1,    -1,    -1,    -1,    -1,
     144,    -1,    -1,    -1,    -1,    -1,    -1,   347,   211,    -1,
      -1,    -1,   352,    -1,    -1,    -1,   812,    91,    -1,   815,
     816,    -1,   818,   819,    -1,    -1,    -1,    -1,    -1,    -1,
     370,   105,   828,    51,    -1,    53,    54,    55,    56,    -1,
      -1,   115,   600,    -1,   118,   119,    -1,    -1,    -1,    -1,
     390,    69,   848,    -1,    -1,   395,   396,    -1,    -1,    -1,
      -1,    -1,    -1,   859,   138,   139,    -1,    -1,    -1,    -1,
     144,   145,   146,   147,   148,    -1,    -1,    95,   874,    -1,
      -1,    -1,    -1,   101,   287,    -1,    -1,    -1,   884,   885,
      -1,   294,   295,    -1,    -1,   891,    -1,    -1,    -1,   302,
      -1,    51,    -1,    53,    54,    55,    56,    -1,    -1,   667,
      -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,   914,    69,
      -1,    -1,    72,    -1,   464,   921,    -1,    -1,    -1,    -1,
      -1,    -1,   472,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   482,    -1,   347,    95,    -1,    -1,    -1,   352,
      -1,   101,   102,   103,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   370,    -1,    -1,
      -1,    -1,    -1,    -1,   514,    -1,    -1,    -1,   128,    -1,
      -1,   131,    -1,   523,    -1,    -1,    -1,   390,    -1,    -1,
      -1,    -1,   395,    -1,   144,   398,    -1,   537,    -1,   539,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   548,    -1,
      -1,   769,  1008,    -1,    -1,    -1,    -1,    -1,   776,   777,
     778,    -1,    -1,    -1,    -1,    -1,   784,    -1,    -1,    -1,
      -1,   789,   790,    -1,    -1,   438,   439,   795,   796,    -1,
      -1,    51,    -1,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,   809,   455,    -1,    -1,    -1,    -1,    -1,    -1,    69,
     600,   464,    72,   821,   822,    -1,    -1,    -1,    -1,   472,
      -1,   829,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   482,
      -1,    -1,   840,   841,    -1,    95,    -1,    -1,    -1,    -1,
      -1,   101,   102,   103,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   863,    -1,    -1,    -1,    -1,
     868,   514,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
     523,   131,    -1,    -1,    -1,    -1,    -1,   667,    -1,    -1,
      -1,    -1,   142,   891,    -1,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   901,   902,   548,    -1,   905,    -1,    -1,
      -1,   909,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      44,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    -1,    -1,    89,    90,   965,    -1,   967,
      -1,    -1,   970,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,   769,
      -1,    -1,    -1,   142,    -1,    -1,   776,   777,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,    -1,    -1,    -1,   795,   796,    -1,    51,    -1,
      53,    54,    55,    56,   667,    -1,    -1,    -1,    -1,   809,
      -1,    -1,    -1,    -1,    -1,    -1,    69,    -1,    -1,    72,
      -1,   821,   822,    -1,    -1,    -1,    -1,    -1,    -1,   829,
      -1,    84,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     840,   841,    95,    -1,    -1,    -1,    -1,    -1,   101,   102,
     103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   863,    -1,    -1,    -1,    -1,   868,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,   131,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   901,   902,    -1,    -1,   905,   769,    -1,    -1,   909,
      -1,    -1,    -1,    -1,   777,   778,    -1,    -1,    51,    52,
      -1,   784,    55,    -1,    -1,    -1,   789,   790,    -1,    -1,
      -1,    -1,   795,   796,    -1,    -1,    -1,    70,    71,    72,
      73,    74,    75,    76,    77,    78,   809,    -1,    81,    82,
      -1,    -1,    -1,    -1,    87,    88,    89,    90,   821,   822,
      -1,    -1,    -1,    -1,    -1,   965,   829,   967,   101,   102,
     970,    -1,    -1,    -1,    -1,    -1,    -1,   840,   841,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   891,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   901,   902,
      -1,    -1,    -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,     0,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,   965,    39,   967,    -1,    -1,   970,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    85,
      86,    -1,    -1,    -1,    -1,    -1,    92,    -1,    -1,    95,
      96,    -1,    98,    99,    -1,   101,    -1,    -1,   104,    -1,
     106,   107,   108,   109,   110,   111,     0,    -1,   114,   115,
      -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,   134,   135,
     136,    -1,    -1,    27,    28,    29,    -1,    -1,    -1,    -1,
     146,    -1,   148,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    -1,    -1,    89,    90,    91,    -1,    -1,
      94,    -1,    -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   113,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,   123,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,   135,   136,    44,    -1,
      -1,    -1,    -1,    -1,   143,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    -1,    -1,
      -1,    -1,    89,    90,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    84,    -1,
      -1,    -1,    -1,    89,    90,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    -1,    -1,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   148,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    -1,
      -1,    89,    90,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,
      89,    90,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   142,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   142,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    84,    -1,    -1,    -1,
      -1,    89,    90,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    84,    -1,    -1,    -1,    -1,
      89,    90,    -1,    -1,    -1,    94,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   142,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    84,    -1,    -1,
      -1,    -1,    89,    90,    -1,    -1,    -1,    94,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      84,    -1,    -1,    -1,    -1,    89,    90,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    84,    -1,    -1,    -1,    -1,    89,    90,    72,
      73,    74,    75,    76,    77,    78,    79,    -1,    81,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    89,    90,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133
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
      42,    43,    44,    37,    38,    51,    53,    54,    55,    56,
      69,    72,    95,   101,   102,   103,   128,   131,   144,   279,
     280,   281,   282,   285,   286,   287,   288,   290,   291,   292,
     293,   295,   296,   297,   300,   301,   302,   303,   304,   279,
     280,    28,   240,   121,   142,    95,   101,   177,   121,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    84,    89,    90,    94,   122,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    91,   105,   140,   147,
     316,    91,   316,   317,    26,   138,   244,   255,    93,    93,
     193,   197,   244,   164,    51,    55,   182,    58,    59,   279,
     125,   277,    91,   140,   316,   220,   307,    91,   147,   315,
     156,   157,    55,    16,   222,   322,   121,    91,   140,   316,
      93,    93,   222,   168,   168,    55,    91,   140,   316,    25,
     110,   142,   266,   318,   115,   265,    20,   247,   322,    57,
     310,   190,   190,   190,    94,   142,   200,   201,   325,   310,
     200,   201,    84,   195,   196,   202,   309,   325,   196,   164,
     318,   320,   164,   160,   138,   157,    91,   316,    93,   159,
     175,   145,   318,   324,   320,   159,   320,   141,   201,   321,
     324,   201,   321,   139,   321,    55,   177,   178,   179,   142,
      91,   140,   316,   144,   238,   290,    63,   256,   258,   262,
     263,    62,   257,    63,    63,    63,    61,    72,    72,   154,
     168,   168,   168,   168,   159,   164,   164,    57,   121,   294,
      84,   290,   121,   156,   190,   142,   305,   325,    51,   142,
     305,   322,   142,   289,   190,   142,   289,    51,   142,   289,
      51,   121,   156,   241,   101,   169,   190,   202,   203,   175,
     142,   180,   142,   162,   163,   169,   181,   190,   192,   203,
     221,   276,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,    51,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,    51,
      52,    55,   188,   193,   313,   314,   195,    51,    52,    55,
     188,   193,   313,    51,    55,   313,   246,   245,   163,   190,
     192,   163,   192,   100,   171,   218,   278,   217,    51,    55,
     182,   313,   195,   313,   156,   164,   167,    15,    13,   249,
     325,   157,    16,    51,    55,   195,    51,    55,   157,    27,
     223,   322,   223,    51,    55,   195,    51,    55,   215,   187,
     157,   247,   190,   202,    15,   262,   190,   190,   319,   101,
     190,   199,   309,   190,   311,   320,   145,   318,   201,   201,
     320,   145,   185,   152,   139,   192,   320,   159,   207,   309,
     177,   179,    51,    55,   195,    51,    55,   290,   210,    63,
     157,   263,   190,   190,    51,   101,   227,   295,   320,   320,
     190,    15,    51,   282,   287,   304,   288,   293,   300,   302,
     295,   297,   302,    51,   295,   190,    15,    80,   126,   232,
     233,   325,   190,   201,   320,   179,   142,    44,   121,    44,
      91,   140,   316,   319,    93,    93,   193,   197,   141,    93,
      93,   194,   197,   194,   197,   232,   232,   172,   322,   168,
     156,   141,    15,   320,   184,   190,   203,   250,   325,    18,
     225,   325,    17,   224,   225,    93,    93,   141,    93,    93,
     225,   212,   214,   141,   168,   185,   139,    15,   201,   222,
     262,   190,   200,   309,   139,   320,   321,   141,   235,   319,
      29,   113,   239,   139,   142,   292,   320,   142,    44,   305,
     142,   289,   142,   289,   142,   289,   142,   289,   289,    44,
     229,   231,   234,   281,   283,   284,   287,   295,   296,   298,
     299,   302,   304,   156,   101,   190,   179,   159,   190,    51,
      55,   195,    51,    55,    57,   123,   163,   192,   169,   192,
     171,    93,   163,   192,   163,   192,   171,   244,   240,   156,
     157,   232,   219,   322,    15,    94,   251,   325,   157,    14,
     252,   325,   168,    15,    93,    15,   157,   157,   223,   190,
     157,   201,   145,   146,   156,   157,   228,   142,   101,   190,
     295,   302,   295,   295,   190,   235,   235,    92,   221,   142,
     305,   305,   142,   230,   221,   142,   230,   142,   230,    15,
     190,   141,   190,   190,   163,   192,    15,   139,   157,   156,
      92,   181,   221,   273,   276,   222,   157,   222,    15,    15,
     216,   225,   247,   248,    51,   236,   237,   291,    15,   139,
     295,   295,   142,   292,   289,   142,   289,   289,   289,   126,
     126,    55,    91,   283,   287,   142,   229,   230,   299,   302,
     295,   298,   302,   295,   139,    15,    55,    91,   140,   316,
     157,   157,   157,   142,   319,   142,   295,   142,   295,    51,
      55,   305,   142,   230,   142,   230,   142,   230,   142,   230,
     230,    51,    55,   195,    51,    55,   249,   224,    15,   237,
     295,   289,   295,   302,   295,   295,   141,   230,   142,   230,
     230,   230,   295,   230
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   149,   151,   150,   152,   153,   153,   153,   153,   154,
     155,   154,   156,   157,   158,   158,   158,   158,   160,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   159,   161,   162,   162,   162,   162,   162,
     162,   162,   162,   163,   163,   163,   164,   164,   164,   164,
     164,   164,   165,   167,   166,   168,   169,   169,   170,   170,
     172,   171,   173,   173,   173,   173,   173,   173,   173,   173,
     173,   173,   173,   174,   174,   175,   175,   176,   176,   176,
     176,   176,   176,   176,   176,   176,   176,   177,   177,   178,
     178,   179,   179,   180,   180,   180,   180,   180,   180,   180,
     180,   181,   181,   181,   181,   181,   181,   181,   181,   181,
     182,   182,   183,   183,   183,   184,   184,   184,   184,   184,
     185,   185,   186,   187,   186,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   188,   188,   188,   188,   188,
     188,   188,   188,   188,   188,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   190,   190,   190,   190,
     190,   190,   190,   190,   190,   190,   191,   191,   191,   191,
     192,   192,   193,   193,   194,   194,   195,   195,   195,   195,
     195,   196,   196,   196,   196,   196,   198,   197,   199,   200,
     200,   201,   201,   202,   202,   202,   202,   203,   203,   203,
     204,   204,   204,   204,   204,   204,   204,   204,   204,   205,
     204,   206,   207,   204,   208,   204,   204,   204,   204,   204,
     204,   204,   204,   204,   204,   204,   204,   204,   209,   210,
     204,   204,   204,   211,   212,   204,   213,   214,   204,   204,
     204,   215,   216,   204,   217,   204,   218,   219,   204,   220,
     204,   204,   204,   204,   204,   204,   204,   221,   222,   222,
     222,   223,   223,   224,   224,   225,   225,   226,   226,   227,
     227,   227,   227,   227,   227,   227,   227,   228,   227,   229,
     229,   229,   229,   230,   230,   231,   231,   231,   231,   231,
     231,   231,   231,   231,   231,   231,   231,   231,   231,   231,
     232,   232,   234,   233,   233,   233,   235,   235,   236,   236,
     237,   237,   238,   238,   239,   239,   241,   240,   242,   242,
     242,   242,   243,   243,   243,   243,   243,   243,   243,   243,
     243,   245,   244,   246,   244,   247,   248,   248,   249,   249,
     250,   250,   250,   251,   251,   252,   252,   253,   253,   253,
     253,   254,   254,   255,   255,   255,   255,   256,   256,   257,
     258,   257,   257,   257,   259,   259,   260,   260,   261,   262,
     262,   263,   263,   264,   264,   265,   266,   265,   267,   267,
     268,   268,   269,   270,   270,   270,   270,   270,   270,   271,
     271,   272,   272,   272,   272,   273,   273,   273,   273,   273,
     274,   274,   275,   275,   275,   275,   275,   275,   275,   275,
     276,   276,   277,   278,   277,   279,   279,   280,   280,   281,
     282,   282,   283,   283,   284,   284,   285,   285,   286,   286,
     287,   287,   288,   288,   288,   288,   289,   289,   290,   290,
     290,   290,   290,   290,   290,   290,   290,   290,   290,   290,
     290,   290,   290,   291,   291,   291,   291,   291,   292,   292,
     293,   294,   293,   295,   295,   296,   297,   298,   299,   299,
     300,   300,   301,   301,   302,   302,   303,   303,   304,   305,
     305,   306,   307,   306,   308,   308,   309,   309,   310,   310,
     311,   311,   311,   311,   312,   312,   312,   313,   313,   313,
     313,   314,   314,   314,   315,   315,   316,   316,   317,   317,
     318,   318,   319,   319,   320,   321,   321,   321,   322,   322,
     322,   323,   324,   324,   325
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     1,     3,     2,     1,
       0,     5,     4,     2,     1,     1,     3,     2,     0,     4,
       2,     3,     3,     3,     3,     3,     4,     1,     3,     3,
       3,     3,     1,     1,     3,     3,     3,     6,     5,     5,
       5,     5,     3,     1,     3,     1,     1,     3,     3,     3,
       2,     1,     2,     0,     5,     1,     1,     1,     1,     4,
       0,     5,     2,     3,     4,     5,     4,     5,     2,     2,
       2,     2,     2,     1,     3,     1,     3,     1,     2,     3,
       5,     2,     4,     2,     4,     1,     3,     1,     3,     2,
       3,     1,     2,     1,     4,     3,     3,     3,     3,     2,
       1,     1,     4,     3,     3,     3,     3,     2,     1,     1,
       1,     1,     2,     1,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     4,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     3,     3,     6,     5,     5,
       5,     5,     4,     3,     3,     2,     2,     3,     2,     2,
       3,     3,     3,     3,     3,     3,     4,     4,     2,     2,
       3,     3,     3,     3,     3,     3,     3,     3,     3,     3,
       3,     3,     3,     2,     2,     3,     3,     3,     3,     6,
       6,     4,     6,     4,     6,     1,     1,     2,     4,     2,
       1,     3,     3,     3,     1,     1,     1,     2,     2,     4,
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
       1,     1,     0,     0,     4,     3,     3,     1,     2,     2,
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
#line 5784 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3:
#line 1540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 5793 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4:
#line 1547 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5801 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5:
#line 1553 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6:
#line 1557 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7:
#line 1562 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5826 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8:
#line 1566 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5834 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10:
#line 1573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 5843 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11:
#line 1578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 5854 "mrbgems/mruby-compiler/core/y.tab.c"
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
#line 5880 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13:
#line 1614 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 5888 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14:
#line 1620 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 5896 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15:
#line 1624 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 5905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16:
#line 1629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 5913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17:
#line 1633 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 5921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18:
#line 1638 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 5927 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19:
#line 1639 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 5935 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20:
#line 1643 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 5943 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21:
#line 1647 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 5951 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22:
#line 1651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 5959 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23:
#line 1655 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 5967 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24:
#line 1659 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 5975 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25:
#line 1663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 5983 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26:
#line 1667 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 5992 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28:
#line 1673 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6000 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29:
#line 1677 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6008 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30:
#line 1681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6016 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31:
#line 1685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6024 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34:
#line 1693 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *lhs = new_lvar(p, (yyvsp[0].id));
                      void_expr_error(p, (yyvsp[-2].nd));
                      assignable(p, lhs);
                      (yyval.nd) = new_asgn(p, lhs, (yyvsp[-2].nd));
                    }
#line 6035 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35:
#line 1702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6043 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36:
#line 1706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6051 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37:
#line 1710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), MRB_QSYM(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6059 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38:
#line 1714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6067 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39:
#line 1718 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6075 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40:
#line 1722 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 6084 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41:
#line 1727 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6092 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42:
#line 1731 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6101 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44:
#line 1739 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6109 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47:
#line 1748 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6117 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 48:
#line 1752 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6125 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 49:
#line 1756 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6133 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50:
#line 1760 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6141 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52:
#line 1768 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 6152 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53:
#line 1777 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 6160 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 54:
#line 1781 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 6173 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55:
#line 1792 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 6184 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 59:
#line 1806 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6192 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 60:
#line 1812 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 6201 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 61:
#line 1819 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 6211 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62:
#line 1827 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6219 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63:
#line 1831 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 6228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64:
#line 1836 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6236 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65:
#line 1840 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 6245 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66:
#line 1845 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 6253 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67:
#line 1849 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 6262 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68:
#line 1854 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 6270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69:
#line 1858 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6278 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70:
#line 1862 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6286 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71:
#line 1866 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6294 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72:
#line 1870 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6302 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73:
#line 1876 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6310 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74:
#line 1880 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6318 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76:
#line 1887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6326 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77:
#line 1893 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6334 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 78:
#line 1897 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 6342 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79:
#line 1901 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6350 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80:
#line 1905 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6358 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81:
#line 1909 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 6366 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82:
#line 1913 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 6374 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83:
#line 1917 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 6382 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84:
#line 1921 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6390 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85:
#line 1925 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 6398 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86:
#line 1929 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 6406 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88:
#line 1936 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 6414 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89:
#line 1942 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 6422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 90:
#line 1946 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 6430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91:
#line 1952 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92:
#line 1956 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 6446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93:
#line 1962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94:
#line 1966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 6462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95:
#line 1970 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96:
#line 1974 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6478 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97:
#line 1978 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98:
#line 1982 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6496 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99:
#line 1988 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6506 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100:
#line 1994 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6515 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101:
#line 2001 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6523 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102:
#line 2005 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 6531 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103:
#line 2009 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6539 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104:
#line 2013 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6547 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105:
#line 2017 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6555 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106:
#line 2021 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6565 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107:
#line 2027 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6575 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108:
#line 2033 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6584 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109:
#line 2038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 6592 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110:
#line 2044 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 6600 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112:
#line 2051 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((node*)1, nsym((yyvsp[0].id)));
                    }
#line 6608 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113:
#line 2055 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((node*)0, nsym((yyvsp[0].id)));
                    }
#line 6616 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 114:
#line 2059 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 6625 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 118:
#line 2069 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6634 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 119:
#line 2074 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6643 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122:
#line 2085 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 6651 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 123:
#line 2088 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 6657 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 124:
#line 2089 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 6665 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125:
#line 2094 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(or);     }
#line 6671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126:
#line 2095 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(xor);    }
#line 6677 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127:
#line 2096 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(and);    }
#line 6683 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128:
#line 2097 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(cmp);    }
#line 6689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129:
#line 2098 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(eq);     }
#line 6695 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130:
#line 2099 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(eqq);    }
#line 6701 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131:
#line 2100 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(match);  }
#line 6707 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132:
#line 2101 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(nmatch); }
#line 6713 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133:
#line 2102 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(gt);     }
#line 6719 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134:
#line 2103 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(ge);     }
#line 6725 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135:
#line 2104 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(lt);     }
#line 6731 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136:
#line 2105 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(le);     }
#line 6737 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137:
#line 2106 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(neq);    }
#line 6743 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138:
#line 2107 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(lshift); }
#line 6749 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139:
#line 2108 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(rshift); }
#line 6755 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140:
#line 2109 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(add);    }
#line 6761 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141:
#line 2110 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(sub);    }
#line 6767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142:
#line 2111 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mul);    }
#line 6773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143:
#line 2112 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mul);    }
#line 6779 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144:
#line 2113 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(div);    }
#line 6785 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145:
#line 2114 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(mod);    }
#line 6791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146:
#line 2115 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(pow);    }
#line 6797 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147:
#line 2116 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(pow);    }
#line 6803 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148:
#line 2117 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(not);    }
#line 6809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149:
#line 2118 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(neg);    }
#line 6815 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150:
#line 2119 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(plus);   }
#line 6821 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151:
#line 2120 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(minus);  }
#line 6827 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152:
#line 2121 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(aref);   }
#line 6833 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153:
#line 2122 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(aset);   }
#line 6839 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154:
#line 2123 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = MRB_QSYM(tick);   }
#line 6845 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 195:
#line 2141 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6853 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 196:
#line 2145 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6861 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 197:
#line 2149 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), MRB_QSYM(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6869 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198:
#line 2153 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6877 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199:
#line 2157 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6885 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200:
#line 2161 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6893 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201:
#line 2165 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6902 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202:
#line 2170 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6911 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203:
#line 2175 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6920 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204:
#line 2180 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6928 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205:
#line 2184 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 6936 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206:
#line 2188 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 6944 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207:
#line 2192 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6952 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208:
#line 2196 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 6960 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209:
#line 2200 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 6968 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210:
#line 2204 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 6976 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211:
#line 2208 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 6984 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212:
#line 2212 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 6992 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213:
#line 2216 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 7000 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214:
#line 2220 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 7008 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215:
#line 2224 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 7016 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216:
#line 2228 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7024 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217:
#line 2232 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)), "-@");
                    }
#line 7032 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218:
#line 2236 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 7040 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219:
#line 2240 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "-@");
                    }
#line 7048 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220:
#line 2244 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 7056 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221:
#line 2248 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 7064 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222:
#line 2252 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 7072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223:
#line 2256 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 7080 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224:
#line 2260 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 7088 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225:
#line 2264 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 7096 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226:
#line 2268 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 7104 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227:
#line 2272 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 7112 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228:
#line 2276 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 7120 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229:
#line 2280 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 7128 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230:
#line 2284 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 7136 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231:
#line 2288 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 7144 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232:
#line 2292 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 7152 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233:
#line 2296 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7160 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234:
#line 2300 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 7168 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235:
#line 2304 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 7176 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236:
#line 2308 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 7184 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237:
#line 2312 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7192 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238:
#line 2316 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7200 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239:
#line 2320 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7208 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240:
#line 2324 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7216 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241:
#line 2328 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7228 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242:
#line 2336 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7241 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243:
#line 2345 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7254 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244:
#line 2354 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245:
#line 2364 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247:
#line 2371 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7285 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248:
#line 2376 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd)));
                    }
#line 7293 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 249:
#line 2380 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7302 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250:
#line 2387 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7310 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251:
#line 2391 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7320 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252:
#line 2399 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7328 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253:
#line 2403 "mrbgems/mruby-compiler/core/parse.y"
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
#line 7356 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 258:
#line 2435 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7365 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 259:
#line 2440 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7374 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 260:
#line 2445 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7383 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 261:
#line 2452 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(list1((yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7393 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262:
#line 2458 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7402 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263:
#line 2463 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264:
#line 2468 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7420 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265:
#line 2473 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7429 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266:
#line 2479 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 7438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267:
#line 2484 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7447 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268:
#line 2491 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 7455 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269:
#line 2497 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7463 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270:
#line 2501 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 7471 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273:
#line 2511 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[0].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7481 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274:
#line 2517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7491 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275:
#line 2523 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7500 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 276:
#line 2528 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7509 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277:
#line 2535 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7518 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278:
#line 2540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7527 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279:
#line 2545 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 7536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 287:
#line 2559 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 7544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 288:
#line 2563 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 7552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 289:
#line 2567 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 290:
#line 2573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7570 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 291:
#line 2578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7579 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 292:
#line 2582 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 7585 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293:
#line 2583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7594 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294:
#line 2587 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 7600 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295:
#line 2588 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 7608 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296:
#line 2592 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7616 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297:
#line 2596 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 7624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298:
#line 2600 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 7632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299:
#line 2604 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7641 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300:
#line 2609 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7650 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301:
#line 2614 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 7658 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302:
#line 2618 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7666 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303:
#line 2622 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 7674 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304:
#line 2626 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 7682 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305:
#line 2630 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), cons(0, (yyvsp[0].nd)));
                    }
#line 7690 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307:
#line 2635 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7699 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308:
#line 2640 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 7709 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309:
#line 2646 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7718 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310:
#line 2651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 7730 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 311:
#line 2662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7739 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312:
#line 2670 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 7748 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313:
#line 2674 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7754 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314:
#line 2674 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7760 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315:
#line 2677 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7769 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316:
#line 2681 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 7775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317:
#line 2681 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 7781 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318:
#line 2684 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 7790 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319:
#line 2691 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 7798 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320:
#line 2695 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 7806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321:
#line 2699 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 7812 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322:
#line 2701 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 7818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323:
#line 2704 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 7827 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324:
#line 2710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7838 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325:
#line 2718 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 7849 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326:
#line 2726 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 7858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327:
#line 2731 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 7868 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328:
#line 2738 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 7881 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329:
#line 2748 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 7892 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330:
#line 2756 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 7903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331:
#line 2766 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7914 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332:
#line 2776 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7926 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333:
#line 2784 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 7934 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334:
#line 2788 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 7942 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335:
#line 2792 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 7950 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336:
#line 2796 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 7958 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337:
#line 2802 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 7967 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 344:
#line 2821 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7975 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 346:
#line 2828 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7983 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 347:
#line 2834 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 7991 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 349:
#line 2841 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 7999 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 350:
#line 2845 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8007 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 351:
#line 2849 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8015 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352:
#line 2853 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3((yyvsp[-2].nd), (node*)-1, 0);
                    }
#line 8024 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 353:
#line 2858 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (node*)-1, (yyvsp[0].nd));
                    }
#line 8032 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354:
#line 2862 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8040 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355:
#line 2866 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8048 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356:
#line 2870 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3(0, (node*)-1, 0);
                    }
#line 8057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357:
#line 2875 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                    }
#line 8065 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358:
#line 2879 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (node*)-1, (yyvsp[0].nd));
                    }
#line 8073 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359:
#line 2885 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8081 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360:
#line 2889 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 8089 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361:
#line 2893 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8097 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362:
#line 2897 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 8105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363:
#line 2903 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8113 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364:
#line 2907 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 8121 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365:
#line 2913 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366:
#line 2917 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8137 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367:
#line 2921 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8145 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368:
#line 2925 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369:
#line 2929 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8161 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370:
#line 2933 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8169 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371:
#line 2937 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8177 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372:
#line 2941 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8185 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373:
#line 2945 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8193 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374:
#line 2949 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8201 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375:
#line 2953 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8209 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376:
#line 2957 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8217 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377:
#line 2961 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8225 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378:
#line 2965 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8233 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379:
#line 2969 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8241 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380:
#line 2975 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8250 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381:
#line 2980 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8259 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382:
#line 2986 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p, 0);}
#line 8265 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383:
#line 2987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8273 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384:
#line 2991 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8282 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385:
#line 2996 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8290 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386:
#line 3003 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8298 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387:
#line 3007 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8306 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390:
#line 3017 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 8315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392:
#line 3025 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393:
#line 3029 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8331 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 394:
#line 3035 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395:
#line 3039 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8347 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396:
#line 3045 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8356 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397:
#line 3052 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8366 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398:
#line 3060 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-1].nd)->car == (node*)NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399:
#line 3070 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8388 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400:
#line 3074 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8397 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401:
#line 3079 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8406 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402:
#line 3086 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8414 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403:
#line 3090 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404:
#line 3094 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405:
#line 3098 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406:
#line 3102 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM(call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 8446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407:
#line 3106 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM(call), (yyvsp[0].nd), tCOLON2);
                    }
#line 8454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408:
#line 3110 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409:
#line 3114 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 8470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410:
#line 3118 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), MRB_QSYM(aref), (yyvsp[-1].nd), '.');
                    }
#line 8478 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411:
#line 3124 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412:
#line 3131 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8499 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413:
#line 3138 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8509 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414:
#line 3145 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415:
#line 3156 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 8528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416:
#line 3162 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 8541 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418:
#line 3176 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 8550 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 420:
#line 3184 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8558 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423:
#line 3192 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8566 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 425:
#line 3199 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 432:
#line 3213 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = concat_string(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435:
#line 3221 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8590 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 436:
#line 3225 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438:
#line 3232 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8606 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439:
#line 3238 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8614 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440:
#line 3242 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8623 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441:
#line 3248 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lex_strterm = (yyvsp[-2].nd);
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442:
#line 3253 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8640 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443:
#line 3257 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8648 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444:
#line 3263 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445:
#line 3267 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dxstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8664 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446:
#line 3273 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8672 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447:
#line 3277 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 451:
#line 3290 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 8690 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 452:
#line 3296 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 8698 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 455:
#line 3306 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 8708 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456:
#line 3312 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8717 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457:
#line 3318 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[-2].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 8727 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458:
#line 3326 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 8735 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459:
#line 3330 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8743 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460:
#line 3337 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 8752 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461:
#line 3342 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_dsym(p, new_dstr(p, push((yyvsp[-1].nd), (yyvsp[0].nd))));
                    }
#line 8761 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 462:
#line 3349 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 8769 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467:
#line 3359 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8777 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 468:
#line 3363 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 8785 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 469:
#line 3369 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 8793 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 470:
#line 3373 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, push((yyvsp[-1].nd), (yyvsp[0].nd)));
                    }
#line 8801 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473:
#line 3381 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474:
#line 3385 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = negate_lit(p, (yyvsp[0].nd));
                    }
#line 8817 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475:
#line 3391 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 8825 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476:
#line 3395 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 8833 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 477:
#line 3399 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 8841 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478:
#line 3403 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 8849 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479:
#line 3407 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 8857 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480:
#line 3413 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 8865 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481:
#line 3417 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 8873 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482:
#line 3423 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 8881 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483:
#line 3427 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 8889 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484:
#line 3431 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 8897 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485:
#line 3435 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 8905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486:
#line 3439 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 8913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487:
#line 3443 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 8925 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488:
#line 3451 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 8936 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489:
#line 3458 "mrbgems/mruby-compiler/core/parse.y"
                    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    }
#line 8949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492:
#line 3473 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8957 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 493:
#line 3477 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 8966 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 494:
#line 3482 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8974 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495:
#line 3493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 8984 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 496:
#line 3499 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 498:
#line 3520 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9014 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499:
#line 3526 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 9022 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 500:
#line 3532 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9032 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501:
#line 3538 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9041 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502:
#line 3545 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9050 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 503:
#line 3550 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9059 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504:
#line 3557 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9067 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505:
#line 3561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9075 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506:
#line 3567 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9083 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507:
#line 3571 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9091 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510:
#line 3581 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, nsym((yyvsp[0].id)));
                    }
#line 9099 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511:
#line 3585 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, 0);
                    }
#line 9107 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512:
#line 3591 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9115 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513:
#line 3595 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9123 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514:
#line 3599 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9131 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515:
#line 3603 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9139 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516:
#line 3609 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9147 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517:
#line 3613 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9155 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518:
#line 3619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9163 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519:
#line 3623 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9171 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520:
#line 3627 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9179 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521:
#line 3631 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9187 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 522:
#line 3635 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9195 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523:
#line 3639 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9203 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524:
#line 3643 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9211 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525:
#line 3647 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9219 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526:
#line 3651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9227 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527:
#line 3655 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9235 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528:
#line 3659 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529:
#line 3663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9251 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530:
#line 3667 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9259 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531:
#line 3671 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9267 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532:
#line 3675 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, MRB_QSYM(and));
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 9276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533:
#line 3682 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 9285 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534:
#line 3687 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 9294 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535:
#line 3692 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 9303 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536:
#line 3697 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 9312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537:
#line 3702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 9321 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538:
#line 3709 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9329 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539:
#line 3713 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9338 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540:
#line 3720 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 9346 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541:
#line 3724 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 9354 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542:
#line 3728 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn_param(p, (yyvsp[-1].nd), p->locals->car);
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 9364 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543:
#line 3736 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9372 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544:
#line 3740 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545:
#line 3746 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 9390 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546:
#line 3754 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547:
#line 3762 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9410 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548:
#line 3770 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9418 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549:
#line 3774 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9426 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550:
#line 3780 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9434 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551:
#line 3784 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9442 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554:
#line 3794 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555:
#line 3799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, MRB_QSYM(mul));
                      (yyval.id) = -1;
                    }
#line 9460 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558:
#line 3810 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9468 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559:
#line 3816 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9476 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560:
#line 3820 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9484 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561:
#line 3826 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9493 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562:
#line 3830 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 9499 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563:
#line 3831 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9526 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 565:
#line 3857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9534 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 566:
#line 3863 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 9543 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 567:
#line 3868 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9551 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 570:
#line 3878 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 571:
#line 3884 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9570 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572:
#line 3889 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if ((yyvsp[-2].nd)->car == (node*)NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 9584 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573:
#line 3899 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 9593 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 586:
#line 3926 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 9601 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 587:
#line 3930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 9609 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 589:
#line 3937 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 9617 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 598:
#line 3958 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 9623 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 601:
#line 3964 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 9632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 604:
#line 3975 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9640 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 9644 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 3979 "mrbgems/mruby-compiler/core/parse.y"

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
