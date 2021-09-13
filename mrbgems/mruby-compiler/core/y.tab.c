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
#include <string.h>
#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/error.h>
#include <mruby/throw.h>
#include <mruby/string.h>
#include <mruby/dump.h>
#include <mruby/presym.h>
#include "node.h"

#define YYLEX_PARAM p

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;
typedef struct mrb_parser_heredoc_info parser_heredoc_info;

static int yyparse(parser_state *p);
static int yylex(void *lval, parser_state *p);
static void yyerror(parser_state *p, const char *s);
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
    MRB_THROW(p->mrb->jmp);
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
endless_method_name(parser_state *p, node *defn)
{
  mrb_sym sym = sym(defn->cdr->car);
  mrb_int len;
  const char *name = mrb_sym_name_len(p->mrb, sym, &len);

  if (len > 1 && name[len-1] == '=') {
    for (int i=0; i<len-1; i++) {
      if (!identchar(name[i])) return;
    }
    yyerror(p, "setter method cannot be defined by endless method definition");
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
new_negate(parser_state *p, node *n)
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


#line 1451 "mrbgems/mruby-compiler/core/y.tab.c"

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
    tCMP = 327,
    tEQ = 328,
    tEQQ = 329,
    tNEQ = 330,
    tGEQ = 331,
    tLEQ = 332,
    tANDOP = 333,
    tOROP = 334,
    tMATCH = 335,
    tNMATCH = 336,
    tDOT2 = 337,
    tDOT3 = 338,
    tBDOT2 = 339,
    tBDOT3 = 340,
    tAREF = 341,
    tASET = 342,
    tLSHFT = 343,
    tRSHFT = 344,
    tCOLON2 = 345,
    tCOLON3 = 346,
    tOP_ASGN = 347,
    tASSOC = 348,
    tLPAREN = 349,
    tLPAREN_ARG = 350,
    tRPAREN = 351,
    tLBRACK = 352,
    tLBRACE = 353,
    tLBRACE_ARG = 354,
    tSTAR = 355,
    tPOW = 356,
    tDSTAR = 357,
    tAMPER = 358,
    tLAMBDA = 359,
    tANDDOT = 360,
    tSYMBEG = 361,
    tSTRING_BEG = 362,
    tXSTRING_BEG = 363,
    tSTRING_DVAR = 364,
    tREGEXP_BEG = 365,
    tWORDS_BEG = 366,
    tSYMBOLS_BEG = 367,
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
#line 1393 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1631 "mrbgems/mruby-compiler/core/y.tab.c"

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
#define YYLAST   12926

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  176
/* YYNRULES -- Number of rules.  */
#define YYNRULES  609
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1072

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
       0,  1564,  1564,  1564,  1575,  1581,  1585,  1590,  1594,  1600,
    1602,  1601,  1615,  1642,  1648,  1652,  1657,  1661,  1667,  1667,
    1671,  1675,  1679,  1683,  1687,  1691,  1695,  1700,  1701,  1705,
    1709,  1713,  1717,  1724,  1727,  1731,  1735,  1739,  1743,  1747,
    1752,  1756,  1765,  1775,  1784,  1794,  1801,  1802,  1806,  1810,
    1811,  1815,  1819,  1823,  1827,  1831,  1841,  1840,  1855,  1864,
    1865,  1868,  1869,  1876,  1875,  1890,  1894,  1899,  1903,  1908,
    1912,  1917,  1921,  1925,  1929,  1933,  1939,  1943,  1949,  1950,
    1956,  1960,  1964,  1968,  1972,  1976,  1980,  1984,  1988,  1992,
    1998,  1999,  2005,  2009,  2015,  2019,  2025,  2029,  2033,  2037,
    2041,  2045,  2051,  2057,  2064,  2068,  2072,  2076,  2080,  2084,
    2090,  2096,  2101,  2107,  2111,  2114,  2118,  2122,  2129,  2130,
    2131,  2132,  2137,  2144,  2145,  2148,  2152,  2152,  2158,  2159,
    2160,  2161,  2162,  2163,  2164,  2165,  2166,  2167,  2168,  2169,
    2170,  2171,  2172,  2173,  2174,  2175,  2176,  2177,  2178,  2179,
    2180,  2181,  2182,  2183,  2184,  2185,  2186,  2187,  2190,  2190,
    2190,  2191,  2191,  2192,  2192,  2192,  2193,  2193,  2193,  2193,
    2194,  2194,  2194,  2195,  2195,  2195,  2196,  2196,  2196,  2196,
    2197,  2197,  2197,  2197,  2198,  2198,  2198,  2198,  2199,  2199,
    2199,  2199,  2200,  2200,  2200,  2200,  2201,  2201,  2204,  2208,
    2212,  2216,  2220,  2224,  2228,  2233,  2238,  2243,  2247,  2251,
    2255,  2259,  2263,  2267,  2271,  2275,  2279,  2283,  2287,  2291,
    2295,  2299,  2303,  2307,  2311,  2315,  2319,  2323,  2327,  2331,
    2335,  2339,  2343,  2347,  2351,  2355,  2359,  2363,  2367,  2371,
    2375,  2379,  2383,  2387,  2391,  2400,  2410,  2419,  2429,  2435,
    2436,  2441,  2445,  2452,  2456,  2464,  2468,  2484,  2510,  2511,
    2514,  2515,  2516,  2521,  2526,  2533,  2539,  2544,  2549,  2554,
    2561,  2561,  2572,  2578,  2582,  2588,  2591,  2597,  2603,  2608,
    2615,  2620,  2625,  2632,  2633,  2634,  2635,  2636,  2637,  2638,
    2639,  2643,  2648,  2647,  2659,  2663,  2658,  2668,  2668,  2672,
    2676,  2680,  2684,  2689,  2694,  2698,  2702,  2706,  2710,  2714,
    2715,  2721,  2727,  2720,  2739,  2747,  2755,  2755,  2755,  2762,
    2762,  2762,  2769,  2775,  2780,  2782,  2779,  2791,  2789,  2807,
    2812,  2805,  2829,  2827,  2843,  2853,  2864,  2868,  2872,  2876,
    2882,  2889,  2890,  2891,  2894,  2895,  2898,  2899,  2907,  2908,
    2914,  2918,  2921,  2925,  2929,  2933,  2938,  2942,  2946,  2950,
    2956,  2955,  2965,  2969,  2973,  2977,  2983,  2988,  2993,  2997,
    3001,  3005,  3009,  3013,  3017,  3021,  3025,  3029,  3033,  3037,
    3041,  3045,  3049,  3055,  3060,  3067,  3067,  3071,  3076,  3083,
    3087,  3093,  3094,  3097,  3102,  3105,  3109,  3115,  3119,  3126,
    3125,  3140,  3150,  3154,  3159,  3166,  3170,  3174,  3178,  3182,
    3186,  3190,  3194,  3198,  3205,  3204,  3219,  3218,  3234,  3242,
    3251,  3254,  3261,  3264,  3268,  3269,  3272,  3276,  3279,  3283,
    3286,  3287,  3288,  3289,  3292,  3293,  3299,  3300,  3301,  3305,
    3315,  3316,  3322,  3327,  3326,  3337,  3341,  3347,  3351,  3361,
    3365,  3371,  3374,  3375,  3378,  3384,  3390,  3391,  3394,  3401,
    3400,  3414,  3418,  3429,  3434,  3445,  3451,  3452,  3453,  3454,
    3455,  3459,  3465,  3469,  3479,  3480,  3481,  3485,  3491,  3495,
    3499,  3503,  3507,  3513,  3517,  3523,  3527,  3531,  3535,  3539,
    3543,  3551,  3558,  3569,  3570,  3574,  3578,  3577,  3594,  3595,
    3598,  3604,  3622,  3642,  3643,  3649,  3655,  3661,  3668,  3673,
    3680,  3684,  3690,  3694,  3700,  3701,  3704,  3708,  3714,  3718,
    3722,  3726,  3732,  3737,  3742,  3746,  3750,  3754,  3758,  3762,
    3766,  3770,  3774,  3778,  3782,  3786,  3790,  3794,  3799,  3805,
    3810,  3815,  3820,  3825,  3832,  3836,  3843,  3848,  3847,  3859,
    3863,  3869,  3877,  3885,  3893,  3897,  3903,  3907,  3913,  3914,
    3917,  3922,  3929,  3930,  3933,  3939,  3943,  3949,  3954,  3954,
    3979,  3980,  3986,  3991,  3997,  4003,  4008,  4012,  4022,  4029,
    4030,  4031,  4034,  4035,  4036,  4037,  4040,  4041,  4042,  4045,
    4046,  4049,  4053,  4059,  4060,  4066,  4067,  4070,  4071,  4074,
    4077,  4078,  4079,  4082,  4083,  4086,  4091,  4094,  4095,  4099
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
  "keyword__FILE__", "keyword__ENCODING__", "\"local variable or method\"",
  "\"method\"", "\"global variable\"", "\"instance variable\"",
  "\"constant\"", "\"class variable\"", "\"label\"", "\"integer literal\"",
  "\"float literal\"", "\"character literal\"", "tXSTRING", "tREGEXP",
  "tSTRING", "tSTRING_PART", "tSTRING_MID", "tNTH_REF", "tBACK_REF",
  "tREGEXP_END", "\"numbered parameter\"", "\"unary plus\"",
  "\"unary minus\"", "\"<=>\"", "\"==\"", "\"===\"", "\"!=\"", "\">=\"",
  "\"<=\"", "\"&&\"", "\"||\"", "\"=~\"", "\"!~\"", "\"..\"", "\"...\"",
  "tBDOT2", "tBDOT3", "tAREF", "tASET", "\"<<\"", "\">>\"", "\"::\"",
  "tCOLON3", "tOP_ASGN", "\"=>\"", "tLPAREN", "\"(\"", "\")\"", "\"[\"",
  "tLBRACE", "\"{\"", "\"*\"", "tPOW", "\"**\"", "\"&\"", "\"->\"",
  "\"&.\"", "\"symbol\"", "\"string literal\"", "tXSTRING_BEG",
  "tSTRING_DVAR", "tREGEXP_BEG", "tWORDS_BEG", "tSYMBOLS_BEG", "tLAMBEG",
  "\"here document\"", "tHEREDOC_END", "tLITERAL_DELIM",
  "tHD_LITERAL_DELIM", "tHD_STRING_PART", "tHD_STRING_MID", "tLOWEST",
  "'='", "'?'", "':'", "'>'", "'<'", "'|'", "'^'", "'&'", "'+'", "'-'",
  "'*'", "'/'", "'%'", "tUMINUS_NUM", "'!'", "'~'", "tLAST_TOKEN", "'{'",
  "'}'", "'['", "']'", "','", "'`'", "'('", "')'", "';'", "'.'", "'\\n'",
  "$accept", "program", "$@1", "top_compstmt", "top_stmts", "top_stmt",
  "@2", "bodystmt", "compstmt", "stmts", "stmt", "$@3", "command_asgn",
  "command_rhs", "expr", "defn_head", "defs_head", "$@4", "expr_value",
  "command_call", "block_command", "cmd_brace_block", "$@5", "command",
  "mlhs", "mlhs_inner", "mlhs_basic", "mlhs_item", "mlhs_list",
  "mlhs_post", "mlhs_node", "lhs", "cname", "cpath", "fname", "fsym",
  "undef_list", "$@6", "op", "reswords", "arg", "aref_args", "arg_rhs",
  "paren_args", "opt_paren_args", "opt_call_args", "call_args",
  "command_args", "@7", "block_arg", "opt_block_arg", "comma", "args",
  "mrhs", "primary", "@8", "@9", "$@10", "$@11", "@12", "@13", "$@14",
  "$@15", "$@16", "$@17", "$@18", "$@19", "@20", "@21", "@22", "@23",
  "primary_value", "then", "do", "if_tail", "opt_else", "for_var",
  "f_margs", "$@24", "block_args_tail", "opt_block_args_tail",
  "block_param", "opt_block_param", "block_param_def", "$@25",
  "opt_bv_decl", "bv_decls", "bvar", "f_larglist", "lambda_body",
  "do_block", "$@26", "block_call", "method_call", "brace_block", "@27",
  "@28", "case_body", "cases", "opt_rescue", "exc_list", "exc_var",
  "opt_ensure", "literal", "string", "string_fragment", "string_rep",
  "string_interp", "@29", "xstring", "regexp", "heredoc", "heredoc_bodies",
  "heredoc_body", "heredoc_string_rep", "heredoc_string_interp", "@30",
  "words", "symbol", "basic_symbol", "sym", "symbols", "numeric",
  "variable", "var_lhs", "var_ref", "backref", "superclass", "$@31",
  "f_opt_arglist_paren", "f_arglist_paren", "f_arglist", "f_label", "f_kw",
  "f_block_kw", "f_block_kwarg", "f_kwarg", "kwrest_mark", "f_kwrest",
  "args_tail", "opt_args_tail", "f_args", "f_bad_arg", "f_norm_arg",
  "f_arg_item", "@32", "f_arg", "f_opt_asgn", "f_opt", "f_block_opt",
  "f_block_optarg", "f_optarg", "restarg_mark", "f_rest_arg",
  "blkarg_mark", "f_block_arg", "opt_f_block_arg", "singleton", "$@33",
  "assoc_list", "assocs", "assoc", "operation", "operation2", "operation3",
  "dot_or_colon", "call_op", "call_op2", "opt_terms", "opt_nl", "rparen",
  "trailer", "term", "nl", "terms", "none", YY_NULLPTR
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

#define YYPACT_NINF (-862)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-610)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -862,   113,  3344,  -862,  8328, 10452, 10794,  6636,  -862, 10098,
   10098,  -862,  -862, 10566,  7818,  6252,  8564,  8564,  -862,  -862,
    8564,  3997,  3582,  -862,  -862,  -862,  -862,    10,  7818,  -862,
      20,  -862,  -862,  -862,  6778,  3179,  -862,  -862,  6920,  -862,
    -862,  -862,  -862,  -862,  -862,  -862,   292, 10216, 10216, 10216,
   10216,   114,  5511,  1275,  9036,  9390,  8100,  -862,  7536,   628,
     100,   502,   965,   992,  -862,   487, 10334, 10216,  -862,   756,
    -862,  1407,  -862,   364,  1613,  1613,  -862,  -862,   155,    65,
    -862,    67, 10680,  -862,   115, 12651,    82,   154,   137,    88,
    -862,   103,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,    81,   142,  -862,   277,    97,  -862,  -862,  -862,  -862,
    -862,   107,   107,    10,   618,  1033,  -862, 10098,   148,  5630,
     567,  1261,  1261,  -862,   145,  -862,   206,  -862,  -862,    97,
    -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,    54,   106,
     110,   158,  -862,  -862,  -862,  -862,  -862,  -862,   173,   192,
     216,   219,  -862,   238,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,   280,
    4689,   224,   364,  1613,  1613,   810,   160, 12775,   377,   320,
     193,   331,   810, 10098, 10098,   644,   235,  -862,  -862,   645,
     275,    86,   116,  -862,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,  -862,  7677,  -862,  -862,   162,  -862,  -862,  -862,  -862,
    -862,  -862,   756,  -862,   557,  -862,   287,  -862,  -862,   756,
    3725, 10216, 10216, 10216, 10216,  -862, 12713,  -862,  -862,   180,
     258,   180,  -862,  -862,  -862,  8682,  -862,  -862,  -862,  8564,
    -862,  -862,  -862,  6252,  6490,  -862,   186,  5749,  -862,   678,
     225,  2722,  2722,   288,  8446,  5511,   198,   756,  1407,   756,
     267,  -862,  8446,   756,   251,  1367,  1367,  -862, 12713,   256,
    1367,  -862,   342, 10908,   257,   768,   926,  1070,  1707,  -862,
    -862,  -862,  -862,  1192,  -862,  -862,  -862,  -862,  -862,  -862,
     570,  1262,  -862,  -862,   326,  -862,   663,  -862,  1270,  -862,
    1280,   313,   324,  -862,  -862,  -862,  -862,  6014, 10098, 10098,
   10098, 10098,  8446, 10098, 10098,    63,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  2066,   318,
     337,  4689, 10216,  -862,   308,   409,   336,  -862,   756,  -862,
    -862,  -862,   339, 10216,  -862,   341,   453,   367,   462,  -862,
    -862,   399,  4689,  -862,  -862,  9508,  -862,  5511,  8214,   380,
    9508, 10216, 10216, 10216, 10216, 10216, 10216, 10216, 10216, 10216,
   10216, 10216, 10216, 10216, 10216,   479, 10216, 10216, 10216, 10216,
   10216, 10216, 10216, 10216, 10216, 10216, 10216, 10216, 11186,  -862,
    8564,  -862, 11272,  -862,  -862, 12476,  -862,  -862,  -862,  -862,
   10334, 10334,  -862,   438,  -862,   364,  -862,  1071,  -862,  -862,
    -862,  -862,  -862,  -862, 11358,  8564, 11444,  4689, 10098,  -862,
    -862,  -862,   532,   550,   382,   456,   460,  -862,  4835,   578,
   10216, 11530,  8564, 11616, 10216, 10216,  5127,    73,    73,   124,
   11702,  8564, 11788,  -862,   535,  -862,  5749,   287,  -862,  -862,
    9626,   585,  -862, 10216, 12775, 12775, 12775, 10216,  -862,  -862,
    8800,  -862, 10216,  -862,  9154,  6371,   470,   756,   180,   180,
    -862,  -862,  1006,   472,  -862,  -862,  -862,  7818,  5246,   464,
   11530, 11616, 10216,  1407,   756,  -862,  -862,  6133,   489,  1407,
    -862,  -862,  9272,  -862,   756,  9390,  -862,  -862,  -862,  1071,
      67, 10908,  -862, 10908, 11874,  8564, 11960,  1470,  -862,  -862,
    -862,  1285,  5749,   570,  -862,  -862,  -862,  -862,  -862,  -862,
    -862, 10216, 10216,  -862,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,  1586,   756,   756,   500, 10334,   633, 12775,
      92,  -862,  -862,  -862,    47,  -862,  -862,  1470,  -862, 12775,
    1470,  -862,  -862,  1815,  -862,  -862, 10334,   638,    56, 10216,
    -862,  2992,   180,  -862,   756, 10908,   526,  -862,  -862,  -862,
     626,   553,  1418,  -862,  -862,  1092,   407,  3503,  3503,  3503,
    3503,  1726,  1726, 12793,  2453,  3503,  3503,  2722,  2722,   918,
     918,  -862,   225, 12775,  1726,  1726,  1345,  1345,  1622,   314,
     314,   225,   225,   225,  4133,  7276,  4405,  7394,  -862,   107,
    -862,   538,   180,   343,  -862,   413,  -862,  -862,  3861,  -862,
    -862,  1934,    56,    56,  -862,  2201,  -862,  -862,  -862,  -862,
    -862,   756, 10098,  4689,  1091,   634,  -862,   107,   543,   107,
     683,  1006,  7959,  -862,  9744,   682,  -862, 10216, 10216,   619,
    -862,  7038,  7157,   562,   458,   465,   682,  -862,  -862,  -862,
    -862,    62,   112,   568,   130,   134, 10098,  7818,   576,   701,
   12775,   774,  -862, 12775, 12775,   493, 10216, 12713,  -862,   180,
   12775,  -862,  -862,  -862,  -862,  8918,  9154,  -862,  -862,  -862,
     582,  -862,  -862,    55,  1407,   756,  1367,   380,  -862,  1091,
     634,   581,  1159,  1160,  -862,    72,  -862,   593,  -862,   225,
     225,  -862,  1007,   756,   606,  -862,  -862,  2252,   708,  2789,
    -862,   703,  -862,   336,  -862,  -862,  -862,   624,   625,   627,
    -862,   635,   703,   627,   744,  2908,  -862,  -862,  1470,  4689,
    -862,  -862,  3393,  9862,  -862,  -862, 10908,  8446, 10334, 10216,
   12046,  8564, 12132,   217, 10334, 10334,  -862,   438,   433,  8800,
   10334, 10334,  -862,   438,    88,   155,  4689,  5749,    56,  -862,
     756,   778,  -862,  -862,  -862,  -862,  2992,  -862,   704,  -862,
    5392,   787,  -862, 10098,   789,  -862, 10216, 10216,   476, 10216,
   10216,   795,  5895,  5895,   135,    73,  -862,  -862,  -862,  9980,
    4981, 12775,  -862,  6371,   180,  -862,  -862,  -862,  1173,   667,
    1415,  4689,  5749,  -862,  -862,  -862,   671,  -862,  1596,   756,
   10216, 10216,  -862,  1470,  -862,  1815,  -862,  1815,  -862,  1815,
    -862,  -862, 10216, 10216,  -862,  -862,  -862, 11022,  -862,   681,
     336,   688, 11022,  -862,   692,   696,  -862,   828, 10216, 12509,
    -862,  -862, 12775,  4269,  4541,   705,   517,   528, 10216, 10216,
    -862,  -862,  -862,  -862,  -862, 10334,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,   829,   711,  5749,  4689,  -862,  -862, 11136,
     810,  -862,  -862,  5895,  -862,  -862,   810,  -862, 10216,  -862,
     836,   840,  -862, 12775,   244,  -862,  9154,  -862,  1385,   842,
     723,  1676,  1676,  1081,  -862, 12775, 12775,   627,   721,   627,
     627, 12775, 12775,   740,   741,   821,  1093,    92,  -862,  -862,
    1793,  -862,  1093,  1470,  -862,  1815,  -862,  -862, 12580,   575,
   12775, 12775,  -862,  -862,  -862,  -862,   742,   864,   827,  -862,
    1136,   926,  1070,  4689,  -862,  4835,  -862,  -862,  5895,  -862,
    -862,  -862,  -862,   746,  -862,  -862,  -862,  -862,   752,   752,
    1676,   755,  -862,  1815,  -862,  -862,  -862,  -862,  -862,  -862,
   12218,  -862,   336,    92,  -862,  -862,   761,   763,   769,  -862,
     779,   769,  -862,  -862,  1071, 12304,  8564, 12390,   550,   619,
     868,  1385,   493,  1676,   752,  1676,   627,   766,   782,  -862,
    1470,  -862,  1815,  -862,  1815,  -862,  1815,  -862,  -862,  1091,
     634,   745,   853,   995,  -862,  -862,  -862,  -862,   752,  -862,
     769,   788,   769,   769,  1173,  -862,  1815,  -862,  -862,  -862,
     769,  -862
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   292,     0,
       0,   316,   319,     0,     0,   595,   336,   337,   338,   339,
     304,   270,   270,   487,   486,   488,   489,   597,     0,    10,
       0,   491,   490,   492,   478,   581,   480,   479,   482,   481,
     474,   475,   436,   437,   493,   494,   290,     0,     0,     0,
       0,     0,     0,   294,   609,   609,    88,   311,     0,     0,
       0,     0,     0,     0,   451,     0,     0,     0,     3,   595,
       6,     9,    27,    33,   538,   538,    49,    60,    59,     0,
      76,     0,    80,    90,     0,    54,   248,     0,    61,   309,
     283,   284,   434,   285,   286,   287,   432,   431,   463,   433,
     430,   485,     0,   288,   289,   270,     5,     8,   336,   337,
     304,   609,   412,     0,   113,   114,   290,     0,     0,     0,
       0,   538,   538,   116,   495,   340,     0,   485,   289,     0,
     332,   168,   178,   169,   165,   194,   195,   196,   197,   176,
     191,   184,   174,   173,   189,   172,   171,   167,   192,   166,
     179,   183,   185,   177,   170,   186,   193,   188,   187,   180,
     190,   175,   164,   182,   181,   163,   161,   162,   158,   159,
     160,   118,   120,   119,   153,   154,   131,   132,   133,   140,
     137,   139,   134,   135,   155,   156,   141,   142,   146,   149,
     150,   136,   138,   128,   129,   130,   143,   144,   145,   147,
     148,   151,   152,   157,   568,    55,   121,   122,   567,     0,
       0,     0,    58,   538,   538,     0,     0,    54,     0,   485,
       0,   289,     0,     0,     0,   112,     0,   351,   350,     0,
       0,   485,   289,   187,   180,   190,   175,   158,   159,   160,
     118,   119,     0,   123,   125,    20,   124,   454,   459,   458,
     603,   605,   595,   606,     0,   456,     0,   607,   604,   596,
     579,     0,     0,     0,     0,   265,   276,    74,   269,   609,
     434,   609,   572,    75,    73,   609,   259,   305,    72,     0,
     258,   411,    71,   595,     0,    18,     0,     0,   221,     0,
     222,   209,   212,   301,     0,     0,     0,   595,    15,   595,
      78,    14,     0,   595,     0,   600,   600,   249,     0,     0,
     600,   570,     0,     0,    86,     0,    96,   103,   538,   468,
     467,   469,   470,     0,   466,   465,   438,   443,   442,   445,
       0,     0,   440,   447,     0,   449,     0,   461,     0,   472,
       0,   476,   477,    53,   236,   237,     4,   596,     0,     0,
       0,     0,     0,     0,     0,   545,   541,   540,   539,   542,
     543,   547,   559,   514,   515,   563,   562,   558,   538,     0,
     503,     0,   507,   512,   609,   517,   609,   537,     0,   544,
     546,   549,   523,     0,   556,   523,   561,   523,     0,   521,
     499,     0,     0,   399,   401,     0,    92,     0,    84,    81,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   208,   211,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   592,
     609,   591,     0,   594,   593,     0,   416,   414,   310,   435,
       0,     0,   405,    65,   308,   329,   113,   114,   115,   476,
     477,   503,   496,   327,     0,   609,     0,     0,     0,   590,
     589,    56,     0,   609,   301,     0,     0,   342,     0,   341,
       0,     0,   609,     0,     0,     0,     0,     0,     0,   301,
       0,   609,     0,   324,     0,   126,     0,     0,   455,   457,
       0,     0,   608,   576,   277,   578,   272,     0,   597,   266,
       0,   274,     0,   267,     0,   595,     0,   595,   609,   609,
     260,   271,   595,     0,   307,    52,   598,     0,     0,     0,
       0,     0,     0,    17,   595,   299,    13,   596,    77,   295,
     298,   302,   602,   250,   601,   602,   252,   303,   571,   102,
      94,     0,    89,     0,     0,   609,     0,   538,   312,   396,
     471,     0,     0,   446,   452,   439,   441,   448,   450,   462,
     473,     0,     0,     7,    21,    22,    23,    24,    25,    50,
      51,   505,   551,     0,   595,   595,   523,     0,     0,   506,
       0,   519,   566,   516,     0,   520,   504,     0,   530,   552,
       0,   533,   560,     0,   535,   564,     0,     0,   609,     0,
      28,    30,     0,    31,   595,     0,    82,    93,    48,    34,
      46,     0,   253,   198,    29,     0,   289,   226,   231,   232,
     233,   228,   230,   240,   241,   234,   235,   207,   210,   238,
     239,    32,   218,   597,   227,   229,   223,   224,   225,   213,
     214,   215,   216,   217,   582,   587,   583,   588,   410,   270,
     408,     0,   609,   582,   584,   583,   585,   409,   270,   582,
     583,   270,   609,   609,    35,   253,   199,    45,   206,    63,
      66,     0,     0,     0,   113,   114,   117,     0,     0,   609,
       0,   595,     0,   293,   609,   609,   422,     0,     0,   609,
     343,   586,   300,     0,   582,   583,   609,   345,   317,   344,
     320,   586,   300,     0,   582,   583,     0,     0,     0,     0,
     276,     0,   323,   575,   574,   275,     0,   278,   273,   609,
     577,   573,   257,   255,   261,   262,   264,   306,   599,    19,
       0,    26,   205,    79,    16,   595,   600,    95,    87,    99,
     101,     0,    98,   100,   597,     0,   464,     0,   453,   219,
     220,   545,   359,   595,   352,   502,   500,     0,    41,   244,
     334,     0,   513,   609,   565,   522,   550,   523,   523,   523,
     557,   523,   545,   523,    43,   246,   335,   387,   385,     0,
     384,   383,   282,     0,    91,    85,     0,     0,     0,     0,
       0,   609,     0,     0,     0,     0,   407,    69,   413,   262,
       0,     0,   406,    67,   402,    62,     0,     0,   609,   330,
       0,     0,   413,   333,   569,    57,   423,   424,   609,   425,
       0,   609,   348,     0,     0,   346,     0,     0,   413,     0,
       0,     0,     0,     0,   413,     0,   127,   460,   322,     0,
       0,   279,   268,   595,   609,    11,   296,   251,    97,     0,
     389,     0,     0,   313,   444,   360,   357,   548,     0,   595,
       0,     0,   518,     0,   526,     0,   528,     0,   534,     0,
     531,   536,     0,     0,   382,   597,   597,   509,   510,   609,
     609,   367,     0,   554,   367,   367,   365,     0,     0,   280,
      83,    47,   254,   582,   583,     0,   582,   583,     0,     0,
      40,   203,    39,   204,    70,     0,    37,   201,    38,   202,
      68,   403,   404,     0,     0,     0,     0,   497,   328,     0,
       0,   427,   349,     0,    12,   429,     0,   314,     0,   315,
       0,     0,   325,   278,   609,   256,   263,   395,     0,     0,
       0,     0,     0,   355,   501,    42,   245,   523,   523,   523,
     523,    44,   247,     0,     0,     0,   508,     0,   363,   364,
     367,   375,   553,     0,   378,     0,   380,   400,   281,   413,
     243,   242,    36,   200,   417,   415,     0,     0,     0,   426,
       0,   104,   111,     0,   428,     0,   318,   321,     0,   419,
     420,   418,   393,   597,   391,   394,   398,   397,   361,   358,
       0,   353,   527,     0,   524,   529,   532,   388,   386,   301,
       0,   511,   609,     0,   366,   373,   367,   367,   367,   555,
     367,   367,    64,   331,   110,     0,   609,     0,   609,   609,
       0,     0,   390,     0,   356,     0,   523,   586,   300,   362,
       0,   370,     0,   372,     0,   379,     0,   376,   381,   107,
     109,     0,   582,   583,   421,   347,   326,   392,   354,   525,
     367,   367,   367,   367,   105,   371,     0,   368,   374,   377,
     367,   369
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -862,  -862,  -862,   419,  -862,    28,  -862,  -277,    32,  -862,
      74,  -862,  -195,  -390,   422,  1715,  1901,  -862,     1,   -58,
    -862,  -635,  -862,    14,   927,  -207,   -30,   -53,  -275,  -506,
     -12,   985,   -38,   933,    13,   -21,  -862,  -862,    19,  -862,
    1204,  -862,   149,    41,  -202,  -374,    66,     5,  -862,  -392,
    -262,  -131,   161,  -361,    35,  -862,  -862,  -862,  -862,  -862,
    -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,  -862,
    -862,     8,  -179,  -442,   -88,  -590,  -862,  -862,  -862,   164,
      85,  -862,  -558,  -862,  -862,  -218,  -862,   -86,  -862,  -862,
     141,  -862,  -862,  -862,   -85,  -862,  -862,  -472,  -862,   -77,
    -862,  -862,  -862,  -862,  -862,    38,    71,  -210,  -862,  -862,
    -862,  -862,  -862,  -245,  -862,   698,  -862,  -862,  -862,    31,
    -862,  -862,  -862,  2358,  2761,   946,  2172,  -862,  -862,     0,
     590,    -6,    76,   397,    24,  -862,  -862,  -862,   329,  -445,
     -14,  -229,  -842,  -706,   -60,  -862,   227,  -711,  -525,  -861,
      -3,  -517,  -862,    75,  -862,   202,  -308,  -862,  -862,  -862,
     102,  -438,   612,  -157,  -862,  -862,   -82,  -862,    34,   -26,
     344,  -249,  -180,  -281,   -67,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    68,    69,    70,   286,   462,   463,   297,
     298,   517,    72,   609,    73,   213,   214,   682,   215,    76,
      77,   670,   808,    78,    79,   299,    80,    81,    82,   542,
      83,   216,   123,   124,   243,   244,   245,   707,   647,   207,
      85,   304,   613,   648,   277,   506,   507,   278,   279,   268,
     499,   535,   652,   603,    86,   210,   302,   735,   303,   318,
     745,   223,   832,   224,   833,   706,   988,   673,   671,   916,
     457,   289,   468,   698,   824,   825,   230,   753,   941,  1014,
     961,   875,   779,   780,   876,   849,   993,   994,   548,   853,
     394,   598,    88,    89,   444,   663,   662,   491,   991,   685,
     818,   920,   924,    90,    91,    92,   331,   332,   552,    93,
      94,    95,   553,   253,   254,   255,   486,    96,    97,    98,
     325,    99,   100,   219,   220,   103,   221,   453,   672,   369,
     370,   371,   372,   373,   878,   879,   374,   375,   376,   377,
     588,   378,   379,   380,   381,   573,   382,   383,   384,   883,
     884,   385,   386,   387,   388,   389,   581,   209,   458,   309,
     509,   272,   129,   677,   650,   461,   456,   435,   513,   850,
     514,   533,   257,   258,   259,   301
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     106,   284,   347,   516,   438,   432,   434,   285,   343,   503,
      87,   222,    87,   126,   126,   709,   252,   218,   218,   280,
     205,   229,   300,   218,   218,   218,   206,   282,   218,   399,
     265,   265,   107,   206,   265,   469,   700,   738,   541,   614,
     125,   125,   469,   476,   314,   246,   856,   206,   125,   256,
     664,   667,   307,   311,   270,   270,   651,   536,   270,   246,
      87,   538,   276,   281,   315,   770,   721,   882,   585,   392,
     767,   324,   390,   390,   218,   391,    71,   206,    71,   492,
     448,   678,   267,   273,   296,   554,   274,  -107,   524,   549,
     315,   125,   270,   270,   578,   821,   995,   721,   693,   785,
     697,   851,  1019,   346,   806,   807,   831,   703,   718,   280,
     443,  -104,   718,     3,   436,   597,   392,   125,   271,   271,
     571,   556,   271,   436,   556,   218,   556,    87,   556,   439,
     556,   334,   336,   338,   340,   777,   473,  -109,   500,   575,
     504,  -111,   765,   761,  -487,   765,   442,   482,   765,  -110,
     365,   296,   276,   281,   283,  -106,   306,   310,   287,  -108,
    -105,   333,   904,    42,   327,   328,    43,   492,   910,   293,
     442,   741,  -340,  -483,   532,   366,   -77,   269,   269,  1019,
     680,   269,   778,   393,   572,   852,   395,  -340,   247,   995,
     604,   248,   249,   363,   364,   365,  -486,   -91,   586,   446,
    -488,  -487,  -104,   447,   -99,   608,  -582,   521,   392,   396,
      59,   390,   390,   465,   466,   305,   329,   330,    87,   250,
     366,   251,  -340,   -96,   477,   478,   437,   433,   -96,  -340,
     527,   218,   218,   546,   440,   437,   400,  1001,   534,   534,
     767,   275,   429,   534,   428,   608,   608,   503,  -489,   882,
     915,   275,   882,  -486,  -101,   324,  -583,  -488,  -103,   429,
     540,   206,   820,  -491,   490,   300,  -102,   501,   541,   501,
     452,   649,   -98,   510,   898,   658,  -100,   -97,   661,   464,
     890,   470,  -490,   218,   431,   474,   487,   218,   721,   265,
     479,   218,   218,   265,   430,    87,   454,   699,   699,   679,
     483,   431,    87,    87,   485,  -489,  -492,   490,   748,  -478,
      87,   429,   765,   270,   649,   502,   658,   270,   744,   519,
    -491,   315,   498,   817,   518,   679,   416,   296,  -482,   882,
     541,   526,   247,   718,   718,   248,   249,   600,   770,  -490,
     899,   556,   610,   525,   989,   511,   455,   492,   125,   564,
     565,   566,   567,   431,   492,    87,   218,   218,   218,   218,
      87,   218,   218,  -492,   679,   251,  -478,   300,   523,   441,
     459,   591,   582,   594,   582,   563,   529,   725,   726,    87,
     522,   271,   610,   610,  -484,  -482,   606,   557,   -76,   679,
     327,   328,   531,   932,   551,   537,   811,   539,  -111,   543,
      87,   353,   354,   218,   900,    87,   315,   718,   615,  -110,
     906,   908,  -483,  -112,   561,   416,   676,   895,   765,  -103,
     765,    71,   765,   475,   765,   562,   568,   460,   510,   296,
    -102,   212,   212,   125,   516,   800,   508,   212,   218,   577,
     269,  -104,   329,   330,   265,   425,   426,   427,   615,   615,
     580,   656,  -111,   510,   656,   862,   802,   842,  -498,   804,
     583,   686,   990,   218,  -106,    87,   218,   471,   270,   265,
     510,   783,   715,   657,   522,   656,    87,   802,   584,   510,
     218,   587,   429,   590,    87,   -98,   265,   847,   737,   218,
     540,   809,   656,   270,    87,   265,   729,   657,   721,   441,
     689,   656,   887,  -110,   592,   801,   501,   501,   696,   593,
     270,   541,   516,   595,   657,   972,   106,   472,   708,   270,
     596,   799,   607,   657,   431,   905,    87,   766,  -111,   913,
     631,   469,   840,   792,  -108,    87,   206,   669,   270,   445,
     656,   724,   270,   510,   718,   341,   342,   683,   246,   315,
     829,   315,   540,   218,  -105,  -100,   602,   830,   765,   265,
      87,   602,   657,   684,   335,   656,   327,   328,   928,   516,
     270,   958,   959,   270,   939,   -97,   125,   687,   125,  -106,
     839,   688,   842,   270,   747,   218,  -108,   657,   504,   666,
     668,   758,    71,   608,   690,   576,   781,  -105,   550,   608,
     712,   734,   719,   731,   218,   608,   608,   793,   247,   800,
     774,   248,   249,   315,   105,   723,   105,   728,   329,   330,
     801,   105,   105,   666,   668,   449,   450,   105,   105,   105,
     917,   -91,   105,   649,   736,   658,   823,   820,  -106,   977,
     125,   251,   757,   528,  -579,   212,   212,   530,   760,  -108,
     501,   711,  1051,   776,   797,   699,   280,   953,   954,   280,
     781,   781,   768,   803,   105,   771,   805,   905,   786,   534,
     787,   732,   488,   810,   788,   248,   249,   280,   105,   798,
     218,    87,   819,   822,   812,   247,   836,   822,   248,   249,
     796,   326,   327,   328,   822,   815,  -105,   766,   813,   276,
     820,   206,   276,   828,  1039,   512,   515,   835,  -478,   834,
     608,   451,   451,   936,   218,   837,   838,   501,   796,   911,
     276,   845,   848,  -478,  -300,   558,   206,   327,   328,   105,
     610,   105,   854,   540,  -290,   480,   610,   902,   246,  -300,
     469,   983,   610,   610,   329,   330,   469,   985,   858,  -290,
     429,   516,   860,   864,   866,   868,  -579,   870,  -478,   871,
     571,   582,  -579,   270,   270,  -478,   863,   865,   520,   867,
     212,   212,   212,   212,  -300,   569,   570,   869,  -583,   329,
     330,  -300,   764,   429,  -290,   481,   764,    87,   872,   510,
     467,  -290,   431,   918,   315,    87,   615,   919,   766,   218,
     754,   923,   615,   218,   927,   265,   781,   766,   615,   615,
     929,   656,   937,   942,    87,    87,   921,   769,   472,   925,
     773,   125,   105,   957,   926,   431,   467,   844,    87,   270,
     960,   218,   768,   657,   963,   105,   105,   270,   965,   914,
      87,    87,   501,   967,   974,   602,   969,   610,    87,   722,
     975,   986,   922,   885,   877,   987,   727,   996,   544,    87,
      87,   891,   997,  1003,   930,   931,  1007,  1008,   733,  -106,
     679,   247,   934,   429,   248,   249,  1009,   582,   582,  1023,
     681,  1022,  1024,  1056,   940,   956,  1064,   105,  1031,   247,
     962,   105,   248,   249,  1033,   105,   105,  1035,  1027,   105,
     766,   844,   250,  1040,   251,  1042,   105,   105,   545,   763,
    -582,  1044,   125,   615,   105,   431,   498,   125,   755,   756,
     250,  1046,   251,    87,    87,   247,  -583,   980,   248,   249,
    1066,    87,   822,  1002,  1004,  1005,  1006,   730,   948,   130,
     227,  1055,   874,   901,   903,  1057,   912,   976,   784,   907,
     909,  1054,   489,   208,   125,   984,   250,  1016,   251,   105,
     105,   105,   105,   105,   105,   105,   105,  1032,  -106,   964,
     966,  -106,  -106,   766,   270,   901,   903,   762,   907,   909,
     886,  1011,     0,   105,   766,     0,     0,    84,     0,    84,
       0,    87,     0,    87,     0,     0,    87,     0,   228,  -106,
       0,  -106,     0,     0,   105,   881,     0,   105,     0,   105,
     582,  -108,   105,     0,     0,  1028,  -485,  1029,     0,   416,
    1030,     0,  1059,     0,   510,   814,   686,   822,   337,   327,
     328,  -485,     0,   877,   218,  1017,   877,    84,  1020,   877,
     265,   877,   105,   353,   354,  1015,   656,   423,   424,   425,
     426,   427,   105,   105,   973,   339,   327,   328,   751,  -580,
     356,   357,   358,   359,   270,     0,  -485,   105,   657,   105,
     105,     0,     0,  -485,     0,     0,   360,   973,     0,   846,
     105,   329,   330,     0,   105,     0,     0,     0,   105,   877,
     947,     0,   949,   105,   212,     0,   950,   857,   105,     0,
       0,  1041,  1043,  1045,    84,  1047,  1048,   880,   329,   330,
    -108,     0,     0,  -108,  -108,  1061,   877,  -586,   877,     0,
     877,   247,   877,  -482,   248,   249,     0,     0,   212,     0,
     105,     0,   751,     0,   356,   357,   358,   359,  -482,   105,
       0,  -108,   877,  -108,     0,  1065,  1067,  1068,  1069,   855,
     360,     0,   250,     0,   251,  1071,     0,   105,     0,   764,
    -289,  -301,   886,     0,   105,   886,     0,   886,   998,   999,
       0,  -580,     0,  -482,     0,  -289,  -301,  -580,     0,     0,
    -482,  -586,   790,  1010,     0,  -582,  -583,   935,     0,   105,
    1018,     0,  1021,     0,     0,    84,  -586,   429,   429,  -413,
       0,     0,     0,   944,     0,     0,     0,     0,   105,     0,
    -289,  -301,     0,   217,   217,   886,     0,  -289,  -301,   217,
     266,   266,     0,  1000,   266,     0,  1025,  1034,     0,  -586,
    1036,  -586,   791,   455,     0,  -582,     0,     0,  -586,   431,
     431,   429,   886,     0,   886,   212,   886,     0,   886,  -582,
    -583,   288,   290,   291,   292,   550,   327,   328,   266,   308,
       0,     0,  1058,  -413,  -582,  -583,     0,  1060,   886,  1062,
     344,   345,    84,  1063,     0,     0,  1026,     0,  -413,    84,
      84,     0,     0,   431,   105,   105,  1012,    84,     0,   880,
       0,     0,   880,  1070,   880,     0,     0,  -582,  -583,  -582,
    -583,     0,     0,  -582,  -583,     0,  -582,  -583,   329,   330,
       0,  -413,   355,  -413,   356,   357,   358,   359,   105,     0,
    -413,   217,     0,     0,     0,   555,   327,   328,     0,     0,
     360,     0,    84,   559,   327,   328,     0,    84,     0,     0,
       0,     0,   880,   560,   327,   328,     0,     0,   746,   327,
     328,     0,     0,     0,     0,   361,    84,     0,     0,     0,
       0,   362,   363,   364,   365,     0,     0,     0,     0,   880,
       0,   880,     0,   880,     0,   880,     0,    84,   329,   330,
       0,     0,    84,     0,     0,   611,   329,   330,     0,   366,
    -297,   105,   367,  -297,  -297,   880,   329,   330,     0,   105,
     105,   329,   330,   105,     0,   368,   105,   105,     0,     0,
       0,     0,   105,   105,     0,     0,     0,     0,   105,   105,
    -297,  -297,     0,  -297,     0,   611,   611,   217,   217,     0,
       0,     0,   105,   413,   414,   105,   992,     0,   356,   357,
     358,   359,    84,     0,   105,   105,   416,   348,   349,   350,
     351,   352,   105,    84,   360,     0,     0,     0,     0,     0,
       0,    84,   789,   105,   105,   494,   495,   496,   344,     0,
       0,    84,     0,   422,   423,   424,   425,   426,   427,   266,
       0,     0,   247,   266,     0,   248,   249,   217,   217,     0,
     401,   402,   403,   404,   405,   406,   407,   408,   409,   410,
     411,   412,     0,    84,     0,     0,   413,   414,     0,   498,
       0,     0,    84,   250,     0,   251,     0,   105,     0,   416,
       0,   355,     0,   356,   357,   358,   359,   105,   105,     0,
     247,     0,     0,   248,   249,   105,     0,    84,     0,   360,
     417,     0,   418,   419,   420,   421,   422,   423,   424,   425,
     426,   427,   217,   217,   217,   217,     0,   217,   217,     0,
    -276,   938,     0,   251,   361,     0,     0,     0,     0,     0,
     362,   363,   364,   365,     0,     0,   579,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   589,     0,     0,
       0,     0,     0,     0,     0,   105,     0,   105,   366,   601,
     105,   367,     0,     0,   612,   617,   618,   619,   620,   621,
     622,   623,   624,   625,   626,   627,   628,   629,   630,     0,
     632,   633,   634,   635,   636,   637,   638,   639,   640,   641,
     642,   643,     0,     0,   266,     0,     0,   751,   105,   356,
     357,   358,   359,     0,   665,   665,     0,   751,     0,   356,
     357,   358,   359,     0,     0,   360,     0,     0,    84,   266,
       0,     0,   217,     0,   355,   360,   356,   357,   358,   359,
       0,     0,     0,     0,   665,     0,   266,     0,   665,   665,
     361,     0,   360,     0,     0,   266,   752,     0,     0,     0,
     361,     0,     0,     0,   710,     0,   943,   713,     0,     0,
       0,   714,     0,     0,   717,     0,   720,   361,   308,   292,
     413,   414,     0,   362,   363,   364,   365,    74,     0,    74,
     121,   121,     0,   416,     0,     0,   665,   751,   121,   356,
     357,   358,   359,     0,  -609,     0,   717,     0,     0,   308,
       0,   366,     0,     0,   367,   360,     0,     0,     0,   266,
       0,   423,   424,   425,   426,   427,     0,   368,   355,     0,
     356,   357,   358,   359,    84,   749,   750,    74,     0,     0,
     361,   121,    84,   611,     0,     0,   360,     0,     0,   611,
       0,   759,     0,     0,     0,   611,   611,     0,     0,     0,
       0,    84,    84,     0,     0,     0,     0,   121,     0,     0,
     775,   361,     0,   782,     0,    84,     0,   362,   363,   364,
     365,     0,     0,     0,   413,   414,     0,    84,    84,     0,
       0,     0,     0,     0,     0,    84,     0,   416,     0,     0,
       0,     0,     0,     0,    74,   366,    84,    84,   367,     0,
       0,     0,     0,     0,   355,     0,   356,   357,   358,   359,
       0,   547,   420,   421,   422,   423,   424,   425,   426,   427,
       0,     0,   360,     0,     0,     0,   772,     0,   356,   357,
     358,   359,     0,     0,     0,     0,   217,     0,     0,     0,
       0,     0,     0,     0,   360,     0,     0,   361,   816,     0,
     611,   759,   775,   362,   363,   364,   365,     0,     0,     0,
      84,    84,     0,    75,   979,    75,   122,   122,    84,   361,
     217,     0,     0,     0,   122,     0,   363,   364,   365,     0,
     841,   366,     0,     0,   367,    74,     0,     0,     0,   717,
     308,     0,     0,     0,  -609,  1013,     0,     0,     0,     0,
       0,     0,     0,   366,     0,     0,     0,  -609,  -609,  -609,
    -609,  -609,  -609,    75,  -609,     0,     0,   122,     0,     0,
    -609,  -609,     0,     0,     0,     0,     0,     0,    84,     0,
      84,  -609,  -609,    84,  -609,  -609,  -609,  -609,  -609,     0,
       0,     0,     0,   122,     0,     0,     0,   889,     0,     0,
       0,     0,   665,   892,     0,   266,     0,     0,   665,   665,
       0,     0,    74,   717,   665,   665,     0,     0,     0,    74,
      74,     0,     0,     0,     0,     0,     0,    74,     0,     0,
      75,     0,     0,     0,  -609,     0,     0,   217,   121,     0,
     665,   665,     0,   665,   665,     0,     0,     0,     0,  -609,
       0,     0,     0,   933,     0,     0,     0,   292,     0,  -609,
       0,     0,  -609,  -609,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,   945,   946,     0,    74,     0,     0,
       0,     0,  -609,  -609,     0,     0,   951,   952,   275,  -609,
    -609,  -609,  -609,     0,     0,     0,    74,     0,     0,     0,
       0,     0,   968,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   970,   971,     0,     0,     0,    74,     0,   665,
       0,    75,    74,   121,     0,    74,     0,   355,     0,   356,
     357,   358,   359,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   665,     0,     0,   360,     0,     0,     0,     0,
     308,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   574,     0,     0,     0,    74,    74,     0,     0,     0,
     361,     0,     0,     0,     0,     0,   362,   363,   364,   365,
       0,     0,    74,     0,   104,     0,   104,   128,   128,     0,
       0,     0,     0,    74,     0,   232,     0,     0,    75,     0,
       0,    74,     0,     0,   366,    75,    75,   367,     0,     0,
       0,    74,     0,    75,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   104,     0,     0,     0,   317,     0,
     266,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,   789,     0,     0,    75,     0,
       0,     0,     0,    75,   317,     0,   121,     0,   121,     0,
       0,     0,     0,     0,     0,     0,     0,    74,     0,     0,
       0,     0,    75,   401,   402,   403,   404,   405,   406,   407,
     408,   409,   410,   411,   412,     0,     0,     0,     0,   413,
     414,   104,     0,    75,     0,     0,     0,     0,    75,   122,
       0,    75,   416,   355,     0,   356,   357,   358,   359,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     121,   360,     0,   417,     0,   418,   419,   420,   421,   422,
     423,   424,   425,   426,   427,     0,     0,   859,     0,     0,
       0,    75,    75,     0,     0,     0,   361,     0,     0,     0,
       0,     0,   362,   363,   364,   365,     0,     0,    75,     0,
     101,     0,   101,   127,   127,   127,     0,     0,     0,    75,
       0,   231,     0,     0,     0,     0,     0,    75,     0,     0,
     366,     0,   104,   367,     0,     0,     0,    75,    74,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,   316,     0,     0,     0,     0,    75,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     316,     0,   122,     0,   122,     0,     0,     0,     0,     0,
       0,     0,     0,    75,     0,     0,     0,     0,     0,   104,
       0,     0,     0,     0,     0,     0,   104,   104,     0,     0,
       0,     0,     0,     0,   104,     0,     0,   101,     0,     0,
       0,     0,     0,     0,     0,   317,     0,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,   121,    74,    74,     0,     0,   122,     0,     0,    74,
       0,     0,     0,     0,     0,    74,    74,     0,     0,   104,
       0,    74,    74,     0,   104,   401,   402,   403,   404,   405,
     406,   407,     0,   409,   410,    74,     0,     0,     0,     0,
       0,   413,   414,   104,     0,     0,     0,    74,    74,     0,
       0,     0,     0,     0,   416,    74,     0,     0,     0,     0,
       0,     0,     0,     0,   104,     0,    74,    74,   101,   104,
     317,     0,   616,     0,    75,     0,     0,   418,   419,   420,
     421,   422,   423,   424,   425,   426,   427,     0,     0,     0,
       0,     0,   121,     0,     0,     0,     0,   121,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   616,   616,     0,     0,     0,     0,     0,     0,
      74,     0,     0,     0,     0,     0,     0,     0,     0,   104,
      74,    74,     0,     0,   121,     0,     0,     0,    74,     0,
     104,     0,     0,     0,     0,   101,     0,     0,   104,     0,
       0,     0,   101,   101,     0,     0,     0,     0,   104,     0,
     101,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   316,     0,     0,     0,     0,     0,     0,     0,     0,
      75,     0,     0,     0,     0,     0,     0,   122,    75,    75,
     104,     0,     0,     0,     0,    75,     0,     0,    74,   104,
      74,    75,    75,    74,     0,   101,     0,    75,    75,     0,
     101,     0,     0,   317,     0,   317,     0,     0,     0,     0,
       0,    75,     0,     0,   104,     0,     0,     0,     0,   101,
       0,     0,     0,    75,    75,     0,     0,     0,     0,     0,
       0,    75,     0,     0,     0,     0,     0,     0,     0,     0,
     101,     0,    75,    75,     0,   101,   316,     0,     0,     0,
       0,     0,     0,   102,     0,   102,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   317,   122,     0,
       0,     0,     0,   122,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   401,   402,   403,   404,   405,   406,
     407,   408,   409,   410,  -610,  -610,    75,     0,     0,     0,
     413,   414,     0,   102,     0,   101,    75,    75,     0,     0,
     122,     0,     0,   416,    75,     0,   101,     0,     0,     0,
       0,     0,     0,   861,   101,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   101,   104,   418,   419,   420,   421,
     422,   423,   424,   425,   426,   427,     0,     0,     0,     0,
       0,   401,   402,   403,   404,   405,   406,   407,   408,   409,
     410,   411,   412,     0,     0,     0,   101,   413,   414,     0,
     102,     0,     0,     0,    75,   101,    75,     0,     0,    75,
     416,     0,     0,     0,     0,     0,     0,     0,     0,   316,
       0,   316,     0,     0,     0,     0,     0,     0,     0,     0,
     101,   417,     0,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   104,   873,     0,     0,     0,     0,     0,   317,   104,
     616,     0,     0,   316,     0,     0,   616,     0,     0,     0,
       0,   102,   616,   616,     0,     0,     0,     0,   104,   104,
     401,   402,   403,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   104,     0,     0,     0,   413,   414,     0,     0,
       0,     0,     0,     0,   104,   104,     0,     0,     0,   416,
       0,     0,   104,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   104,   104,     0,     0,     0,     0,     0,
     417,   101,   418,   419,   420,   421,   422,   423,   424,   425,
     426,   427,     0,     0,     0,     0,     0,     0,   102,   128,
       0,     0,     0,     0,   128,   102,   102,     0,     0,     0,
       0,     0,     0,   102,   401,   402,   403,   404,   405,   406,
     407,   408,   409,   410,   411,   412,     0,   616,     0,     0,
     413,   414,     0,     0,     0,     0,     0,   104,   104,     0,
       0,   982,     0,   416,     0,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   102,     0,
       0,     0,     0,   102,   417,     0,   418,   419,   420,   421,
     422,   423,   424,   425,   426,   427,     0,     0,     0,     0,
       0,     0,   102,     0,  -276,     0,     0,   101,     0,     0,
       0,     0,     0,     0,   316,   101,     0,     0,     0,     0,
       0,     0,     0,   102,     0,   104,     0,   104,   102,     0,
     104,   102,     0,     0,   101,   101,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   101,  -291,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     101,   101,  -291,  -291,  -291,  -291,  -291,  -291,   101,  -291,
       0,   102,   102,     0,     0,     0,  -291,  -291,  -291,   101,
     101,     0,     0,     0,     0,     0,  -291,  -291,   102,  -291,
    -291,  -291,  -291,  -291,     0,     0,     0,     0,     0,   102,
       0,     0,     0,     0,     0,   127,  -291,   102,     0,     0,
     127,     0,     0,     0,     0,     0,     0,   102,     0,     0,
       0,  -291,  -291,  -291,  -291,  -291,  -291,  -291,  -291,  -291,
    -291,  -291,  -291,     0,     0,     0,     0,  -291,  -291,  -291,
       0,     0,  -291,   101,   101,     0,     0,   981,  -291,   102,
    -291,   101,     0,     0,  -291,     0,     0,     0,   102,     0,
       0,     0,  -291,     0,  -291,     0,     0,  -291,  -291,     0,
       0,  -291,  -291,  -291,  -291,  -291,  -291,  -291,  -291,  -291,
    -291,  -291,  -291,   102,     0,     0,     0,     0,  -291,  -291,
    -291,  -291,     0,     0,  -291,  -291,  -291,  -291,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   101,     0,   101,  -609,     4,   101,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,     0,
       0,     0,     0,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,   102,    51,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,  -609,
       0,     0,  -609,  -609,     0,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,     0,    65,    66,
      67,   413,   414,     0,     0,     0,     0,     0,     0,     0,
    -609,     0,  -609,     0,   416,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   417,     0,   418,   419,   420,
     421,   422,   423,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,     0,  -277,     0,     0,     0,     0,
     102,     0,     0,     0,     0,     0,     0,     0,   102,   102,
       0,     0,     0,     0,     0,   102,     0,     0,     0,     0,
       0,   102,   102,     0,     0,     0,     0,   102,   102,     0,
       0,     0,     0,     0,     0,  -610,  -610,  -610,  -610,   405,
     406,   102,  -412,  -610,  -610,     0,     0,     0,     0,     0,
       0,   413,   414,   102,   102,  -412,  -412,  -412,  -412,  -412,
    -412,   102,  -412,     0,   416,     0,     0,     0,  -412,  -412,
    -412,     0,   102,   102,     0,     0,     0,     0,     0,  -412,
    -412,     0,  -412,  -412,  -412,  -412,  -412,   418,   419,   420,
     421,   422,   423,   424,   425,   426,   427,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -412,  -412,  -412,  -412,  -412,  -412,
    -412,  -412,  -412,  -412,  -412,  -412,   102,     0,     0,     0,
    -412,  -412,  -412,     0,     0,  -412,   102,   102,     0,     0,
       0,  -412,     0,  -412,   102,     0,     0,  -412,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -412,     0,     0,
    -412,  -412,     0,     0,  -412,     0,  -412,  -412,  -412,  -412,
    -412,  -412,  -412,  -412,  -412,  -412,     0,     0,     0,     0,
    -412,  -412,  -412,  -412,  -412,  -478,   275,  -412,  -412,  -412,
    -412,     0,     0,     0,     0,     0,     0,     0,  -478,  -478,
    -478,  -478,  -478,  -478,   102,  -478,   102,     0,     0,   102,
       0,     0,  -478,  -478,     0,     0,     0,     0,     0,     0,
       0,     0,  -478,  -478,     0,  -478,  -478,  -478,  -478,  -478,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   493,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -478,  -478,  -478,
    -478,  -478,  -478,  -478,  -478,  -478,  -478,  -478,  -478,     0,
       0,     0,     0,  -478,  -478,  -478,     0,  -478,  -478,     0,
       0,     0,     0,     0,  -478,     0,  -478,     0,     0,     0,
    -478,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -478,     0,     0,  -478,  -478,     0,  -478,  -478,     0,  -478,
    -478,  -478,  -478,  -478,  -478,  -478,  -478,  -478,  -478,     0,
       0,  -609,     0,     0,  -478,  -478,  -478,  -478,     0,     0,
    -478,  -478,  -478,  -478,  -609,  -609,  -609,  -609,  -609,  -609,
       0,  -609,     0,     0,     0,     0,     0,  -609,  -609,  -609,
       0,     0,     0,     0,     0,     0,     0,     0,  -609,  -609,
       0,  -609,  -609,  -609,  -609,  -609,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -609,  -609,  -609,  -609,  -609,  -609,  -609,
    -609,  -609,  -609,  -609,  -609,     0,     0,     0,     0,  -609,
    -609,  -609,     0,     0,  -609,     0,     0,     0,     0,     0,
    -609,     0,  -609,     0,     0,     0,  -609,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -609,     0,     0,  -609,
    -609,     0,     0,  -609,     0,  -609,  -609,  -609,  -609,  -609,
    -609,  -609,  -609,  -609,  -609,     0,     0,  -609,     0,  -609,
    -609,  -609,  -609,  -609,     0,   275,  -609,  -609,  -609,  -609,
    -609,  -609,  -609,  -609,  -609,  -609,     0,  -609,     0,     0,
       0,     0,     0,     0,  -609,  -609,     0,     0,     0,     0,
       0,     0,     0,     0,  -609,  -609,     0,  -609,  -609,  -609,
    -609,  -609,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -609,
    -609,  -609,  -609,  -609,  -609,  -609,  -609,  -609,  -609,  -609,
    -609,     0,     0,     0,     0,  -609,  -609,  -609,     0,     0,
    -609,     0,     0,     0,     0,     0,  -609,     0,  -609,     0,
       0,     0,  -609,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -609,     0,     0,  -609,  -609,     0,     0,  -609,
       0,  -609,  -609,  -609,  -609,  -609,  -609,  -609,  -609,  -609,
    -609,     0,     0,  -586,     0,     0,  -609,  -609,  -609,  -609,
       0,   275,  -609,  -609,  -609,  -609,  -586,  -586,  -586,     0,
    -586,  -586,     0,  -586,     0,     0,     0,     0,     0,  -586,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -586,  -586,     0,  -586,  -586,  -586,  -586,  -586,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -586,  -586,  -586,  -586,  -586,
    -586,  -586,  -586,  -586,  -586,  -586,  -586,     0,     0,     0,
       0,  -586,  -586,  -586,     0,   794,  -586,     0,     0,     0,
       0,     0,     0,     0,  -586,     0,     0,     0,  -586,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -586,     0,
       0,  -586,  -586,     0,  -107,  -586,     0,  -586,  -586,  -586,
    -586,  -586,  -586,  -586,  -586,  -586,  -586,     0,     0,  -586,
       0,  -586,  -586,  -586,     0,   -99,     0,     0,  -586,  -586,
    -586,  -586,  -586,  -586,  -586,     0,  -586,  -586,     0,  -586,
       0,     0,     0,     0,     0,  -586,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -586,  -586,     0,  -586,
    -586,  -586,  -586,  -586,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -586,  -586,  -586,  -586,  -586,  -586,  -586,  -586,  -586,
    -586,  -586,  -586,     0,     0,     0,     0,  -586,  -586,  -586,
       0,   794,  -586,     0,     0,     0,     0,     0,     0,     0,
    -586,     0,     0,     0,  -586,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -586,     0,     0,  -586,  -586,     0,
    -107,  -586,     0,  -586,  -586,  -586,  -586,  -586,  -586,  -586,
    -586,  -586,  -586,     0,     0,  -300,     0,  -586,  -586,  -586,
       0,  -586,     0,     0,  -586,  -586,  -586,  -586,  -300,  -300,
    -300,     0,  -300,  -300,     0,  -300,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -300,  -300,     0,  -300,  -300,  -300,  -300,  -300,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -300,  -300,  -300,
    -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,     0,
       0,     0,     0,  -300,  -300,  -300,     0,   795,  -300,     0,
       0,     0,     0,     0,     0,     0,  -300,     0,     0,     0,
    -300,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -300,     0,     0,  -300,  -300,     0,  -109,  -300,     0,  -300,
    -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,  -300,     0,
       0,  -300,     0,     0,  -300,  -300,     0,  -101,     0,     0,
    -300,  -300,  -300,  -300,  -300,  -300,  -300,     0,  -300,  -300,
       0,  -300,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -300,  -300,
       0,  -300,  -300,  -300,  -300,  -300,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -300,  -300,  -300,  -300,  -300,  -300,  -300,
    -300,  -300,  -300,  -300,  -300,     0,     0,     0,     0,  -300,
    -300,  -300,     0,   795,  -300,     0,     0,     0,     0,     0,
       0,     0,  -300,     0,     0,     0,  -300,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -300,     0,     0,  -300,
    -300,     0,  -109,  -300,     0,  -300,  -300,  -300,  -300,  -300,
    -300,  -300,  -300,  -300,  -300,     0,     0,     0,     0,     0,
    -300,  -300,     0,  -300,     0,     0,  -300,  -300,  -300,  -300,
     294,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,  -609,  -609,  -609,     0,     0,  -609,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,  -609,     0,     0,  -609,  -609,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,    67,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -609,   294,  -609,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
    -609,     0,  -609,  -609,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
    -609,     0,     0,  -609,  -609,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    65,
      66,    67,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -609,   294,  -609,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,  -609,     0,     0,  -609,
      15,  -609,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,  -609,     0,     0,  -609,
    -609,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    65,    66,    67,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -609,   294,  -609,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,  -609,     0,     0,  -609,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,    51,     0,
       0,    52,    53,     0,    54,    55,     0,    56,     0,     0,
       0,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,  -609,     0,     0,  -609,  -609,     4,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,    65,    66,    67,     0,    15,     0,    16,    17,    18,
      19,     0,     0,  -609,     0,  -609,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,    51,     0,     0,
      52,    53,     0,    54,    55,     0,    56,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,  -609,     0,     0,  -609,  -609,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      65,    66,    67,     0,     0,  -609,     0,     0,     0,     0,
       0,     0,  -609,   294,  -609,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,  -609,  -609,     0,     0,
       0,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,    51,     0,     0,    52,    53,     0,    54,
      55,     0,    56,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,  -609,     0,     0,
    -609,  -609,   294,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,    65,    66,    67,     0,
      15,     0,    16,    17,    18,    19,     0,     0,  -609,     0,
    -609,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,     0,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,   295,    53,     0,    54,    55,
       0,    56,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,  -609,     0,     0,  -609,
    -609,   294,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,    65,    66,    67,     0,    15,
       0,    16,    17,    18,    19,     0,  -609,  -609,     0,  -609,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,    51,     0,     0,    52,    53,     0,    54,    55,     0,
      56,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,  -609,     0,     0,  -609,  -609,
     294,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,    65,    66,    67,     0,    15,     0,
      16,    17,    18,    19,     0,  -609,  -609,     0,  -609,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,  -609,     0,     0,  -609,  -609,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,    67,     0,     0,  -609,     0,
       0,     0,     0,     0,     0,  -609,   294,  -609,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
    -609,     0,     0,     0,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
    -609,     0,     0,  -609,  -609,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    65,
      66,    67,     0,    15,     0,    16,    17,    18,    19,     0,
       0,  -609,     0,  -609,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
      29,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,    51,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,   247,
       0,     0,   248,   249,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,    65,    66,
      67,     0,    15,     0,    16,    17,    18,    19,     0,     0,
     250,     0,   251,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,   247,     0,
       0,   248,   249,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,    65,    66,    67,
       0,    15,     0,    16,    17,    18,    19,     0,     0,   250,
       0,   251,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    53,     0,    54,
      55,     0,     0,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,   247,     0,     0,
     248,   249,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,    65,    66,    67,     0,
      15,     0,   108,   109,    18,    19,     0,     0,   250,     0,
     251,   110,   111,   112,    23,    24,    25,    26,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    53,     0,    54,    55,
       0,     0,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,   247,     0,     0,   248,
     249,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,    65,   264,    67,     0,    15,
       0,    16,    17,    18,    19,     0,     0,   250,     0,   251,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    53,     0,    54,    55,     0,
       0,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,   247,     0,     0,   248,   249,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    65,    66,    67,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   251,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,     0,     0,     0,   155,   156,   157,   158,
     159,   160,   161,   162,   163,   164,     0,     0,     0,     0,
       0,   165,   166,   167,   168,   169,   170,   171,   172,    36,
      37,   173,    39,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,     0,     0,     0,     0,     0,     0,   203,
     204,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,  -579,
       0,     0,     0,     0,     0,     0,     0,  -579,     0,  -579,
    -579,  -579,  -579,     0,  -579,     0,     0,     0,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,     0,     0,  -579,     0,     0,
       0,     0,     0,     0,     0,     0,  -579,  -579,  -579,  -579,
    -579,  -579,  -579,  -579,  -579,     0,  -579,  -579,  -579,     0,
       0,  -579,     0,     0,  -579,  -579,     0,  -579,  -579,  -579,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -579,  -579,     0,     0,     0,     0,     0,  -579,
       0,     0,  -579,  -579,     0,  -579,  -579,     0,  -579,     0,
    -579,  -579,  -579,     0,  -579,  -579,  -579,     0,  -579,  -579,
    -579,     0,  -579,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -579,  -579,  -579,     0,  -579,     0,     0,     0,
       0,     0,  -579,  -580,  -580,  -580,  -580,  -580,  -580,  -580,
    -580,  -580,     0,     0,     0,     0,     0,     0,     0,  -580,
       0,  -580,  -580,  -580,  -580,     0,  -580,     0,     0,     0,
    -580,  -580,  -580,  -580,  -580,  -580,  -580,     0,     0,  -580,
       0,     0,     0,     0,     0,     0,     0,     0,  -580,  -580,
    -580,  -580,  -580,  -580,  -580,  -580,  -580,     0,  -580,  -580,
    -580,     0,     0,  -580,     0,     0,  -580,  -580,     0,  -580,
    -580,  -580,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -580,  -580,     0,     0,     0,     0,
       0,  -580,     0,     0,  -580,  -580,     0,  -580,  -580,     0,
    -580,     0,  -580,  -580,  -580,     0,  -580,  -580,  -580,     0,
    -580,  -580,  -580,     0,  -580,     0,     0,     0,     0,     0,
       0,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,  -582,
       0,     0,     0,     0,  -580,  -580,  -580,  -582,  -580,  -582,
    -582,  -582,  -582,     0,  -580,     0,     0,     0,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,     0,     0,  -582,     0,     0,
       0,     0,     0,     0,     0,     0,  -582,  -582,  -582,  -582,
    -582,  -582,  -582,  -582,  -582,     0,  -582,  -582,  -582,     0,
       0,  -582,     0,     0,  -582,  -582,     0,  -582,  -582,  -582,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -582,  -582,     0,     0,     0,     0,     0,  -582,
     826,     0,  -582,  -582,     0,  -582,  -582,     0,  -582,     0,
    -582,  -582,  -582,     0,  -582,  -582,  -582,     0,  -582,  -582,
    -582,     0,  -582,     0,     0,     0,     0,     0,     0,  -107,
    -583,  -583,  -583,  -583,  -583,  -583,  -583,  -583,  -583,     0,
       0,     0,  -582,  -582,  -582,     0,  -583,     0,  -583,  -583,
    -583,  -583,  -582,     0,     0,     0,     0,  -583,  -583,  -583,
    -583,  -583,  -583,  -583,     0,     0,  -583,     0,     0,     0,
       0,     0,     0,     0,     0,  -583,  -583,  -583,  -583,  -583,
    -583,  -583,  -583,  -583,     0,  -583,  -583,  -583,     0,     0,
    -583,     0,     0,  -583,  -583,     0,  -583,  -583,  -583,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -583,  -583,     0,     0,     0,     0,     0,  -583,   827,
       0,  -583,  -583,     0,  -583,  -583,     0,  -583,     0,  -583,
    -583,  -583,     0,  -583,  -583,  -583,     0,  -583,  -583,  -583,
       0,  -583,     0,     0,     0,     0,     0,     0,  -109,  -584,
    -584,  -584,  -584,  -584,  -584,  -584,  -584,  -584,     0,     0,
       0,  -583,  -583,  -583,     0,  -584,     0,  -584,  -584,  -584,
    -584,  -583,     0,     0,     0,     0,  -584,  -584,  -584,  -584,
    -584,  -584,  -584,     0,     0,  -584,     0,     0,     0,     0,
       0,     0,     0,     0,  -584,  -584,  -584,  -584,  -584,  -584,
    -584,  -584,  -584,     0,  -584,  -584,  -584,     0,     0,  -584,
       0,     0,  -584,  -584,     0,  -584,  -584,  -584,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -584,  -584,     0,     0,     0,     0,     0,  -584,     0,     0,
    -584,  -584,     0,  -584,  -584,     0,  -584,     0,  -584,  -584,
    -584,     0,  -584,  -584,  -584,     0,  -584,  -584,  -584,     0,
    -584,     0,     0,     0,     0,     0,     0,  -585,  -585,  -585,
    -585,  -585,  -585,  -585,  -585,  -585,     0,     0,     0,     0,
    -584,  -584,  -584,  -585,     0,  -585,  -585,  -585,  -585,     0,
    -584,     0,     0,     0,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,     0,     0,  -585,     0,     0,     0,     0,     0,     0,
       0,     0,  -585,  -585,  -585,  -585,  -585,  -585,  -585,  -585,
    -585,     0,  -585,  -585,  -585,     0,     0,  -585,     0,     0,
    -585,  -585,     0,  -585,  -585,  -585,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -585,  -585,
       0,     0,     0,     0,     0,  -585,     0,     0,  -585,  -585,
       0,  -585,  -585,     0,  -585,     0,  -585,  -585,  -585,     0,
    -585,  -585,  -585,     0,  -585,  -585,  -585,     0,  -585,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -585,  -585,
    -585,     0,     0,     0,     0,     0,     0,     0,  -585,   131,
     132,   133,   134,   135,   136,   137,   138,   139,   140,   141,
     142,   143,   144,   145,   146,   147,   148,   149,   150,   151,
     152,   153,   154,     0,     0,     0,   155,   156,   157,   233,
     234,   235,   236,   162,   163,   164,     0,     0,     0,     0,
       0,   165,   166,   167,   237,   238,   239,   240,   172,   319,
     320,   241,   321,     0,     0,     0,     0,     0,     0,   322,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,   323,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,     0,     0,     0,     0,     0,     0,   203,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151,   152,   153,   154,     0,     0,     0,   155,   156,   157,
     233,   234,   235,   236,   162,   163,   164,     0,     0,     0,
       0,     0,   165,   166,   167,   237,   238,   239,   240,   172,
     319,   320,   241,   321,     0,     0,     0,     0,     0,     0,
     322,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,   484,     0,     0,     0,     0,     0,
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
       0,     0,   203,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
       0,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,   116,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   312,     0,     0,   119,    53,     0,    54,    55,     0,
       0,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,     0,    15,   120,   108,   109,    18,    19,     0,
       0,     0,   313,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   312,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,     0,     0,     0,     0,    15,   120,    16,
      17,    18,    19,     0,     0,     0,   605,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,    51,
       0,     0,    52,    53,     0,    54,    55,     0,    56,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,     0,    65,    66,    67,    15,     0,    16,    17,    18,
      19,     0,     0,     0,     0,     0,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,    51,     0,     0,
      52,    53,     0,    54,    55,     0,    56,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,    66,    67,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   260,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,   261,     0,   262,   263,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   264,
      67,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,   260,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,   505,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    53,     0,    54,
      55,     0,   261,     0,   262,   263,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    65,   264,    67,    15,
       0,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   260,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    53,     0,    54,    55,     0,
     716,     0,   262,   263,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,   264,    67,    15,     0,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,   260,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,   843,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    53,     0,    54,    55,     0,   716,     0,
     262,   263,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,   264,    67,    15,     0,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,   260,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    53,     0,    54,    55,     0,   261,     0,   262,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   264,    67,    15,     0,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   260,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,   262,   263,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   264,
      67,    15,     0,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,   260,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    53,     0,    54,
      55,     0,   716,     0,   262,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     0,
       0,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,    65,   264,    67,    15,
       0,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,   260,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,   211,     0,     0,   119,    53,     0,    54,    55,     0,
       0,     0,   262,     0,    57,     0,    58,    59,    60,     0,
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
       0,     0,   119,    53,     0,    54,    55,     0,   599,     0,
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
     119,    53,     0,    54,    55,     0,   261,     0,     0,     0,
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
       0,    54,    55,     0,   599,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   264,
      67,    15,     0,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,   211,     0,     0,   119,    53,     0,    54,
      55,     0,   888,     0,     0,     0,    57,     0,    58,    59,
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
     716,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     0,     0,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,    65,   264,    67,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,   211,
       0,     0,   119,    53,     0,    54,    55,     0,     0,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,     0,     0,     0,     0,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,    65,    66,    67,    15,     0,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    53,     0,    54,    55,     0,     0,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     0,     0,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
      65,   264,    67,    15,     0,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     0,     0,     0,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,     0,     0,    65,   264,
      67,    15,     0,   108,   109,    18,    19,     0,     0,     0,
       0,     0,   110,   111,   112,    23,    24,    25,    26,     0,
       0,   113,     0,     0,     0,     0,     0,     0,     0,     0,
      31,    32,    33,   114,    35,    36,    37,   115,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,   116,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     117,     0,     0,   118,     0,     0,   119,    53,     0,    54,
      55,     0,     0,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   120,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   225,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   226,     0,     0,
      52,    53,     0,    54,    55,     0,    56,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,     0,     0,     0,     0,     0,    15,
     120,   108,   109,    18,    19,     0,     0,     0,     0,     0,
     110,   111,   112,    23,    24,    25,    26,     0,     0,   113,
       0,     0,     0,     0,     0,     0,     0,     0,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,   116,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   312,     0,     0,   397,    53,     0,    54,    55,     0,
     398,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,     0,    15,   120,   108,   109,    18,    19,     0,
       0,     0,     0,     0,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,   114,    35,    36,    37,   115,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,   116,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   118,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,     0,
       0,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,     0,    15,   120,   108,
     109,    18,    19,     0,     0,     0,     0,     0,   110,   111,
     112,    23,    24,    25,    26,     0,     0,   113,     0,     0,
       0,     0,     0,     0,     0,     0,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,   116,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   312,
       0,     0,   397,    53,     0,    54,    55,     0,     0,     0,
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
       0,     0,     0,   955,     0,     0,   119,    53,     0,    54,
      55,     0,     0,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
       0,     0,     0,     0,     0,    15,   120,   108,   109,    18,
      19,     0,     0,     0,     0,     0,   110,   111,   112,    23,
      24,    25,    26,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,   225,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   978,     0,     0,
     119,    53,     0,    54,    55,     0,     0,   644,   645,     0,
      57,   646,    58,    59,    60,     0,    61,    62,    63,     0,
      64,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
     120,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   653,   654,     0,     0,   655,     0,   203,
     275,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   674,
     645,     0,     0,   675,     0,   203,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   659,   654,     0,     0,   660,
       0,   203,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,   691,   645,     0,     0,   692,     0,   203,   275,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   694,   654,     0,
       0,   695,     0,   203,   275,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   701,   645,     0,     0,   702,     0,   203,
     275,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,   704,
     654,     0,     0,   705,     0,   203,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,   739,   645,     0,     0,   740,
       0,   203,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,   742,   654,     0,     0,   743,     0,   203,   275,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   893,   645,     0,
       0,   894,     0,   203,   275,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
       0,   201,   202,   896,   654,     0,     0,   897,     0,   203,
     275,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,  1037,
     645,     0,     0,  1038,     0,   203,   275,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   174,   175,
     176,   177,   178,   179,   180,   181,     0,     0,   182,   183,
       0,     0,     0,     0,   184,   185,   186,   187,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   188,   189,
     190,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   191,   192,   193,   194,   195,   196,   197,   198,
     199,   200,     0,   201,   202,  1049,   645,     0,     0,  1050,
       0,   203,   275,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,  1052,   654,     0,     0,  1053,     0,   203,   275,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   659,   654,     0,
       0,   660,     0,   203,   275,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   174,   175,   176,   177,
     178,   179,   180,   181,     0,     0,   182,   183,     0,     0,
       0,     0,   184,   185,   186,   187,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   188,   189,   190,     0,
       0,   401,   402,   403,   404,   405,   406,   407,   408,   409,
     410,   411,   412,     0,     0,     0,     0,   413,   414,     0,
     191,   192,   193,   194,   195,   196,   197,   198,   199,   200,
     416,   201,   202,     0,     0,     0,     0,     0,     0,   203,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   417,     0,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,     0,     0,     0,     0,     0,     0,     0,
       0,  -278,   401,   402,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,     0,     0,     0,     0,   413,   414,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   416,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   417,     0,   418,   419,   420,   421,   422,   423,
     424,   425,   426,   427,     0,     0,     0,     0,     0,     0,
       0,     0,  -279,   401,   402,   403,   404,   405,   406,   407,
     408,   409,   410,   411,   412,     0,     0,     0,     0,   413,
     414,     0,     0,     0,   415,     0,     0,     0,     0,     0,
       0,     0,   416,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   417,     0,   418,   419,   420,   421,   422,
     423,   424,   425,   426,   427,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,     0,     0,     0,
       0,   413,   414,     0,     0,     0,   497,     0,     0,     0,
       0,     0,     0,     0,   416,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   417,     0,   418,   419,   420,
     421,   422,   423,   424,   425,   426,   427,   401,   402,   403,
     404,   405,   406,   407,   408,   409,   410,   411,   412,     0,
       0,     0,     0,   413,   414,   401,   402,   403,   404,   405,
     406,     0,     0,   409,   410,     0,   416,     0,     0,     0,
       0,   413,   414,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   416,     0,     0,   417,     0,   418,
     419,   420,   421,   422,   423,   424,   425,   426,   427,     0,
       0,     0,     0,     0,     0,     0,     0,   418,   419,   420,
     421,   422,   423,   424,   425,   426,   427
};

static const yytype_int16 yycheck[] =
{
       2,    27,    69,   284,    89,    87,    88,    28,    66,   271,
       2,    10,     4,     5,     6,   487,    15,     9,    10,    21,
       7,    13,    52,    15,    16,    17,     7,    22,    20,    82,
      16,    17,     4,    14,    20,   215,   478,   543,   313,   400,
       5,     6,   222,   222,    56,    14,   752,    28,    13,    15,
     440,   441,    54,    55,    16,    17,   430,   306,    20,    28,
      52,   310,    21,    22,    56,   590,   504,   778,   376,    75,
     587,    58,    74,    75,    66,    75,     2,    58,     4,   259,
     118,   455,    16,    17,    52,   330,    20,    25,   295,   318,
      82,    56,    54,    55,   371,   685,   938,   535,   472,   605,
      27,    29,   963,    69,   662,   663,   696,   481,   500,   111,
     105,    25,   504,     0,    26,   392,   122,    82,    16,    17,
      57,   331,    20,    26,   334,   117,   336,   119,   338,    91,
     340,    60,    61,    62,    63,    79,   218,    25,   269,   368,
     271,    25,   587,    51,    90,   590,   105,   229,   593,    25,
     103,   119,   111,   112,   144,    25,    54,    55,   138,    25,
      25,    61,   797,    60,    64,    65,    63,   347,   803,    55,
     129,   545,    90,    92,   305,   128,   121,    16,    17,  1040,
     457,    20,   126,    28,   121,   113,   121,   105,   115,  1031,
     397,   118,   119,   101,   102,   103,    90,   142,   378,    51,
      90,   147,   121,    55,   142,   400,   144,   289,   214,   142,
     107,   213,   214,   213,   214,    54,   116,   117,   210,   146,
     128,   148,   140,   142,   223,   224,   138,    90,   142,   147,
     297,   223,   224,   315,    92,   138,   121,   943,   305,   306,
     757,   144,   105,   310,    90,   440,   441,   509,    90,   960,
     808,   144,   963,   147,   142,   242,   144,   147,   142,   105,
     313,   242,    18,    90,    20,   295,   142,   269,   543,   271,
     125,   428,   142,   275,    57,   432,   142,   142,   435,    55,
     786,   121,    90,   275,   147,    92,   252,   279,   726,   275,
      55,   283,   284,   279,   140,   287,    90,   477,   478,   456,
      25,   147,   294,   295,   142,   147,    90,    20,   553,    90,
     302,   105,   757,   275,   471,    57,   473,   279,   547,   287,
     147,   313,   142,   684,   138,   482,   101,   295,    90,  1040,
     605,   297,   115,   725,   726,   118,   119,   395,   863,   147,
     123,   551,   400,   145,   934,   279,   140,   527,   313,   348,
     349,   350,   351,   147,   534,   347,   348,   349,   350,   351,
     352,   353,   354,   147,   521,   148,   147,   397,   294,    92,
      90,   385,   374,   387,   376,   347,   302,   508,   509,   371,
      92,   279,   440,   441,    92,   147,   398,    61,   121,   546,
      64,    65,   141,   835,   323,   139,   673,    55,   121,   142,
     392,    37,    38,   395,   794,   397,   398,   799,   400,   121,
     800,   801,    92,   121,   101,   101,   454,   791,   863,   142,
     865,   347,   867,    92,   869,   101,   352,   147,   430,   397,
     142,     9,    10,   398,   715,    92,   275,    15,   430,   121,
     279,   121,   116,   117,   430,   131,   132,   133,   440,   441,
     142,   432,   121,   455,   435,   763,   658,   719,   121,   661,
      51,   463,   934,   455,   121,   457,   458,    90,   430,   455,
     472,   602,   498,   432,    92,   456,   468,   679,   142,   481,
     472,   142,   105,   142,   476,   142,   472,   736,   541,   481,
     543,   671,   473,   455,   486,   481,   517,   456,   936,    92,
     468,   482,   779,   121,    51,    92,   508,   509,   476,   142,
     472,   786,   793,    51,   473,   905,   518,   140,   486,   481,
     121,   652,   142,   482,   147,    92,   518,   587,   121,   806,
      51,   711,   711,   615,   121,   527,   517,    99,   500,   117,
     521,   507,   504,   545,   936,    58,    59,    15,   517,   541,
      92,   543,   605,   545,   121,   142,   395,    92,  1003,   545,
     552,   400,   521,    13,    62,   546,    64,    65,    92,   850,
     532,   879,   880,   535,   851,   142,   541,   121,   543,   121,
     711,   121,   844,   545,   552,   577,   121,   546,   719,   440,
     441,   577,   518,   788,    16,   368,   598,   121,    63,   794,
      15,   527,   500,   139,   596,   800,   801,   633,   115,    92,
     596,   118,   119,   605,     2,   145,     4,   145,   116,   117,
      92,     9,    10,   474,   475,    58,    59,    15,    16,    17,
     810,   142,    20,   790,   532,   792,    17,    18,   121,   916,
     605,   148,   142,   299,    26,   223,   224,   303,    15,   121,
     652,   490,  1026,    15,   649,   835,   658,   875,   876,   661,
     662,   663,   587,   658,    52,   590,   661,    92,   142,   736,
      44,   522,   115,   672,   121,   118,   119,   679,    66,   141,
     672,   673,   684,   685,   141,   115,   707,   689,   118,   119,
     649,    63,    64,    65,   696,   682,   121,   757,    15,   658,
      18,   682,   661,   141,  1012,   283,   284,   706,    90,   141,
     905,   121,   122,   844,   706,   139,    15,   719,   677,   804,
     679,   139,   141,   105,    90,    62,   707,    64,    65,   117,
     788,   119,   139,   786,    90,    90,   794,   795,   707,   105,
     920,   920,   800,   801,   116,   117,   926,   926,   142,   105,
     105,  1032,    44,   767,   768,   769,   138,   771,   140,   773,
      57,   763,   144,   725,   726,   147,   142,   142,    90,   142,
     348,   349,   350,   351,   140,   353,   354,   142,   144,   116,
     117,   147,   580,   105,   140,   140,   584,   779,    44,   791,
      16,   147,   147,    15,   786,   787,   788,    93,   858,   791,
     573,    14,   794,   795,    15,   791,   808,   867,   800,   801,
      15,   792,   145,   142,   806,   807,   818,   590,   140,   821,
     593,   786,   210,   142,   823,   147,    16,   725,   820,   791,
     142,   823,   757,   792,   142,   223,   224,   799,   142,   807,
     832,   833,   844,    15,    15,   684,   141,   905,   840,   505,
     139,    15,   820,   778,   778,    15,   512,    15,    90,   851,
     852,   787,   139,   142,   832,   833,   126,   126,   524,    16,
    1027,   115,   840,   105,   118,   119,    55,   879,   880,    15,
     458,   139,    55,    15,   852,   877,   141,   275,   142,   115,
     882,   279,   118,   119,   142,   283,   284,   142,   980,   287,
     960,   799,   146,   142,   148,   142,   294,   295,   140,   580,
     144,   142,   877,   905,   302,   147,   142,   882,   574,   575,
     146,   142,   148,   915,   916,   115,   144,   919,   118,   119,
     142,   923,   934,   947,   948,   949,   950,   518,   863,     6,
      13,  1029,   778,   794,   795,  1031,   805,   915,   604,   800,
     801,  1028,   254,     7,   919,   923,   146,   960,   148,   347,
     348,   349,   350,   351,   352,   353,   354,   993,   115,   884,
     885,   118,   119,  1033,   936,   826,   827,   580,   829,   830,
     778,   957,    -1,   371,  1044,    -1,    -1,     2,    -1,     4,
      -1,   983,    -1,   985,    -1,    -1,   988,    -1,    13,   146,
      -1,   148,    -1,    -1,   392,   778,    -1,   395,    -1,   397,
    1012,    16,   400,    -1,    -1,   983,    90,   985,    -1,   101,
     988,    -1,  1036,    -1,  1026,   681,  1028,  1029,    63,    64,
      65,   105,    -1,   957,  1026,   960,   960,    52,   963,   963,
    1026,   965,   430,    37,    38,   960,  1027,   129,   130,   131,
     132,   133,   440,   441,   905,    63,    64,    65,    51,    26,
      53,    54,    55,    56,  1026,    -1,   140,   455,  1027,   457,
     458,    -1,    -1,   147,    -1,    -1,    69,   928,    -1,   735,
     468,   116,   117,    -1,   472,    -1,    -1,    -1,   476,  1013,
     863,    -1,   865,   481,   672,    -1,   869,   753,   486,    -1,
      -1,  1016,  1017,  1018,   119,  1020,  1021,   778,   116,   117,
     115,    -1,    -1,   118,   119,  1040,  1040,    26,  1042,    -1,
    1044,   115,  1046,    90,   118,   119,    -1,    -1,   706,    -1,
     518,    -1,    51,    -1,    53,    54,    55,    56,   105,   527,
      -1,   146,  1066,   148,    -1,  1060,  1061,  1062,  1063,   142,
      69,    -1,   146,    -1,   148,  1070,    -1,   545,    -1,   957,
      90,    90,   960,    -1,   552,   963,    -1,   965,   941,   942,
      -1,   138,    -1,   140,    -1,   105,   105,   144,    -1,    -1,
     147,    90,    90,    90,    -1,    26,    26,   843,    -1,   577,
     963,    -1,   965,    -1,    -1,   210,   105,   105,   105,    26,
      -1,    -1,    -1,   859,    -1,    -1,    -1,    -1,   596,    -1,
     140,   140,    -1,     9,    10,  1013,    -1,   147,   147,    15,
      16,    17,    -1,   142,    20,    -1,    90,  1000,    -1,   138,
    1003,   140,   140,   140,    -1,   144,    -1,    -1,   147,   147,
     147,   105,  1040,    -1,  1042,   823,  1044,    -1,  1046,    90,
      90,    47,    48,    49,    50,    63,    64,    65,    54,    55,
      -1,    -1,  1035,    90,   105,   105,    -1,  1040,  1066,  1042,
      66,    67,   287,  1046,    -1,    -1,   140,    -1,   105,   294,
     295,    -1,    -1,   147,   672,   673,   957,   302,    -1,   960,
      -1,    -1,   963,  1066,   965,    -1,    -1,   138,   138,   140,
     140,    -1,    -1,   144,   144,    -1,   147,   147,   116,   117,
      -1,   138,    51,   140,    53,    54,    55,    56,   706,    -1,
     147,   117,    -1,    -1,    -1,    63,    64,    65,    -1,    -1,
      69,    -1,   347,    63,    64,    65,    -1,   352,    -1,    -1,
      -1,    -1,  1013,    63,    64,    65,    -1,    -1,    63,    64,
      65,    -1,    -1,    -1,    -1,    94,   371,    -1,    -1,    -1,
      -1,   100,   101,   102,   103,    -1,    -1,    -1,    -1,  1040,
      -1,  1042,    -1,  1044,    -1,  1046,    -1,   392,   116,   117,
      -1,    -1,   397,    -1,    -1,   400,   116,   117,    -1,   128,
     115,   779,   131,   118,   119,  1066,   116,   117,    -1,   787,
     788,   116,   117,   791,    -1,   144,   794,   795,    -1,    -1,
      -1,    -1,   800,   801,    -1,    -1,    -1,    -1,   806,   807,
     145,   146,    -1,   148,    -1,   440,   441,   223,   224,    -1,
      -1,    -1,   820,    88,    89,   823,    51,    -1,    53,    54,
      55,    56,   457,    -1,   832,   833,   101,    40,    41,    42,
      43,    44,   840,   468,    69,    -1,    -1,    -1,    -1,    -1,
      -1,   476,    44,   851,   852,   261,   262,   263,   264,    -1,
      -1,   486,    -1,   128,   129,   130,   131,   132,   133,   275,
      -1,    -1,   115,   279,    -1,   118,   119,   283,   284,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,   518,    -1,    -1,    88,    89,    -1,   142,
      -1,    -1,   527,   146,    -1,   148,    -1,   905,    -1,   101,
      -1,    51,    -1,    53,    54,    55,    56,   915,   916,    -1,
     115,    -1,    -1,   118,   119,   923,    -1,   552,    -1,    69,
     122,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,   348,   349,   350,   351,    -1,   353,   354,    -1,
     142,   146,    -1,   148,    94,    -1,    -1,    -1,    -1,    -1,
     100,   101,   102,   103,    -1,    -1,   372,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   383,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   983,    -1,   985,   128,   395,
     988,   131,    -1,    -1,   400,   401,   402,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   413,   414,    -1,
     416,   417,   418,   419,   420,   421,   422,   423,   424,   425,
     426,   427,    -1,    -1,   430,    -1,    -1,    51,  1026,    53,
      54,    55,    56,    -1,   440,   441,    -1,    51,    -1,    53,
      54,    55,    56,    -1,    -1,    69,    -1,    -1,   673,   455,
      -1,    -1,   458,    -1,    51,    69,    53,    54,    55,    56,
      -1,    -1,    -1,    -1,   470,    -1,   472,    -1,   474,   475,
      94,    -1,    69,    -1,    -1,   481,   100,    -1,    -1,    -1,
      94,    -1,    -1,    -1,   490,    -1,   100,   493,    -1,    -1,
      -1,   497,    -1,    -1,   500,    -1,   502,    94,   504,   505,
      88,    89,    -1,   100,   101,   102,   103,     2,    -1,     4,
       5,     6,    -1,   101,    -1,    -1,   522,    51,    13,    53,
      54,    55,    56,    -1,   121,    -1,   532,    -1,    -1,   535,
      -1,   128,    -1,    -1,   131,    69,    -1,    -1,    -1,   545,
      -1,   129,   130,   131,   132,   133,    -1,   144,    51,    -1,
      53,    54,    55,    56,   779,   561,   562,    52,    -1,    -1,
      94,    56,   787,   788,    -1,    -1,    69,    -1,    -1,   794,
      -1,   577,    -1,    -1,    -1,   800,   801,    -1,    -1,    -1,
      -1,   806,   807,    -1,    -1,    -1,    -1,    82,    -1,    -1,
     596,    94,    -1,   599,    -1,   820,    -1,   100,   101,   102,
     103,    -1,    -1,    -1,    88,    89,    -1,   832,   833,    -1,
      -1,    -1,    -1,    -1,    -1,   840,    -1,   101,    -1,    -1,
      -1,    -1,    -1,    -1,   119,   128,   851,   852,   131,    -1,
      -1,    -1,    -1,    -1,    51,    -1,    53,    54,    55,    56,
      -1,   144,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,    69,    -1,    -1,    -1,    51,    -1,    53,    54,
      55,    56,    -1,    -1,    -1,    -1,   672,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    69,    -1,    -1,    94,   684,    -1,
     905,   687,   688,   100,   101,   102,   103,    -1,    -1,    -1,
     915,   916,    -1,     2,   919,     4,     5,     6,   923,    94,
     706,    -1,    -1,    -1,    13,    -1,   101,   102,   103,    -1,
     716,   128,    -1,    -1,   131,   210,    -1,    -1,    -1,   725,
     726,    -1,    -1,    -1,     0,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,    -1,    -1,    -1,    13,    14,    15,
      16,    17,    18,    52,    20,    -1,    -1,    56,    -1,    -1,
      26,    27,    -1,    -1,    -1,    -1,    -1,    -1,   983,    -1,
     985,    37,    38,   988,    40,    41,    42,    43,    44,    -1,
      -1,    -1,    -1,    82,    -1,    -1,    -1,   783,    -1,    -1,
      -1,    -1,   788,   789,    -1,   791,    -1,    -1,   794,   795,
      -1,    -1,   287,   799,   800,   801,    -1,    -1,    -1,   294,
     295,    -1,    -1,    -1,    -1,    -1,    -1,   302,    -1,    -1,
     119,    -1,    -1,    -1,    90,    -1,    -1,   823,   313,    -1,
     826,   827,    -1,   829,   830,    -1,    -1,    -1,    -1,   105,
      -1,    -1,    -1,   839,    -1,    -1,    -1,   843,    -1,   115,
      -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   347,    -1,   860,   861,    -1,   352,    -1,    -1,
      -1,    -1,   138,   139,    -1,    -1,   872,   873,   144,   145,
     146,   147,   148,    -1,    -1,    -1,   371,    -1,    -1,    -1,
      -1,    -1,   888,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   898,   899,    -1,    -1,    -1,   392,    -1,   905,
      -1,   210,   397,   398,    -1,   400,    -1,    51,    -1,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   928,    -1,    -1,    69,    -1,    -1,    -1,    -1,
     936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    85,    -1,    -1,    -1,   440,   441,    -1,    -1,    -1,
      94,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,   103,
      -1,    -1,   457,    -1,     2,    -1,     4,     5,     6,    -1,
      -1,    -1,    -1,   468,    -1,    13,    -1,    -1,   287,    -1,
      -1,   476,    -1,    -1,   128,   294,   295,   131,    -1,    -1,
      -1,   486,    -1,   302,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   313,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    52,    -1,    -1,    -1,    56,    -1,
    1026,    -1,    -1,   518,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   527,    -1,    -1,    44,    -1,    -1,   347,    -1,
      -1,    -1,    -1,   352,    82,    -1,   541,    -1,   543,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   552,    -1,    -1,
      -1,    -1,   371,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,   119,    -1,   392,    -1,    -1,    -1,    -1,   397,   398,
      -1,   400,   101,    51,    -1,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     605,    69,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,    85,    -1,    -1,
      -1,   440,   441,    -1,    -1,    -1,    94,    -1,    -1,    -1,
      -1,    -1,   100,   101,   102,   103,    -1,    -1,   457,    -1,
       2,    -1,     4,     5,     6,     7,    -1,    -1,    -1,   468,
      -1,    13,    -1,    -1,    -1,    -1,    -1,   476,    -1,    -1,
     128,    -1,   210,   131,    -1,    -1,    -1,   486,   673,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      52,    -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,   518,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   527,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      82,    -1,   541,    -1,   543,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   552,    -1,    -1,    -1,    -1,    -1,   287,
      -1,    -1,    -1,    -1,    -1,    -1,   294,   295,    -1,    -1,
      -1,    -1,    -1,    -1,   302,    -1,    -1,   119,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   313,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   779,    -1,    -1,    -1,    -1,    -1,
      -1,   786,   787,   788,    -1,    -1,   605,    -1,    -1,   794,
      -1,    -1,    -1,    -1,    -1,   800,   801,    -1,    -1,   347,
      -1,   806,   807,    -1,   352,    72,    73,    74,    75,    76,
      77,    78,    -1,    80,    81,   820,    -1,    -1,    -1,    -1,
      -1,    88,    89,   371,    -1,    -1,    -1,   832,   833,    -1,
      -1,    -1,    -1,    -1,   101,   840,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   392,    -1,   851,   852,   210,   397,
     398,    -1,   400,    -1,   673,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,   877,    -1,    -1,    -1,    -1,   882,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   440,   441,    -1,    -1,    -1,    -1,    -1,    -1,
     905,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   457,
     915,   916,    -1,    -1,   919,    -1,    -1,    -1,   923,    -1,
     468,    -1,    -1,    -1,    -1,   287,    -1,    -1,   476,    -1,
      -1,    -1,   294,   295,    -1,    -1,    -1,    -1,   486,    -1,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   313,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     779,    -1,    -1,    -1,    -1,    -1,    -1,   786,   787,   788,
     518,    -1,    -1,    -1,    -1,   794,    -1,    -1,   983,   527,
     985,   800,   801,   988,    -1,   347,    -1,   806,   807,    -1,
     352,    -1,    -1,   541,    -1,   543,    -1,    -1,    -1,    -1,
      -1,   820,    -1,    -1,   552,    -1,    -1,    -1,    -1,   371,
      -1,    -1,    -1,   832,   833,    -1,    -1,    -1,    -1,    -1,
      -1,   840,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     392,    -1,   851,   852,    -1,   397,   398,    -1,    -1,    -1,
      -1,    -1,    -1,     2,    -1,     4,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   605,   877,    -1,
      -1,    -1,    -1,   882,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,   905,    -1,    -1,    -1,
      88,    89,    -1,    52,    -1,   457,   915,   916,    -1,    -1,
     919,    -1,    -1,   101,   923,    -1,   468,    -1,    -1,    -1,
      -1,    -1,    -1,    44,   476,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   486,   673,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,   518,    88,    89,    -1,
     119,    -1,    -1,    -1,   983,   527,   985,    -1,    -1,   988,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   541,
      -1,   543,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     552,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   779,    44,    -1,    -1,    -1,    -1,    -1,   786,   787,
     788,    -1,    -1,   605,    -1,    -1,   794,    -1,    -1,    -1,
      -1,   210,   800,   801,    -1,    -1,    -1,    -1,   806,   807,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,   820,    -1,    -1,    -1,    88,    89,    -1,    -1,
      -1,    -1,    -1,    -1,   832,   833,    -1,    -1,    -1,   101,
      -1,    -1,   840,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   851,   852,    -1,    -1,    -1,    -1,    -1,
     122,   673,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,   287,   877,
      -1,    -1,    -1,    -1,   882,   294,   295,    -1,    -1,    -1,
      -1,    -1,    -1,   302,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,   905,    -1,    -1,
      88,    89,    -1,    -1,    -1,    -1,    -1,   915,   916,    -1,
      -1,   919,    -1,   101,    -1,   923,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   347,    -1,
      -1,    -1,    -1,   352,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
      -1,    -1,   371,    -1,   142,    -1,    -1,   779,    -1,    -1,
      -1,    -1,    -1,    -1,   786,   787,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   392,    -1,   983,    -1,   985,   397,    -1,
     988,   400,    -1,    -1,   806,   807,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   820,     0,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     832,   833,    13,    14,    15,    16,    17,    18,   840,    20,
      -1,   440,   441,    -1,    -1,    -1,    27,    28,    29,   851,
     852,    -1,    -1,    -1,    -1,    -1,    37,    38,   457,    40,
      41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,   468,
      -1,    -1,    -1,    -1,    -1,   877,    57,   476,    -1,    -1,
     882,    -1,    -1,    -1,    -1,    -1,    -1,   486,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,
      -1,    -1,    93,   915,   916,    -1,    -1,   919,    99,   518,
     101,   923,    -1,    -1,   105,    -1,    -1,    -1,   527,    -1,
      -1,    -1,   113,    -1,   115,    -1,    -1,   118,   119,    -1,
      -1,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   552,    -1,    -1,    -1,    -1,   139,   140,
     141,   142,    -1,    -1,   145,   146,   147,   148,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   983,    -1,   985,     0,     1,   988,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,
      -1,    -1,    -1,    -1,   673,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,   115,
      -1,    -1,   118,   119,    -1,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,   134,   135,
     136,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     146,    -1,   148,    -1,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,
     779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   787,   788,
      -1,    -1,    -1,    -1,    -1,   794,    -1,    -1,    -1,    -1,
      -1,   800,   801,    -1,    -1,    -1,    -1,   806,   807,    -1,
      -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,
      77,   820,     0,    80,    81,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    89,   832,   833,    13,    14,    15,    16,    17,
      18,   840,    20,    -1,   101,    -1,    -1,    -1,    26,    27,
      28,    -1,   851,   852,    -1,    -1,    -1,    -1,    -1,    37,
      38,    -1,    40,    41,    42,    43,    44,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,   905,    -1,    -1,    -1,
      88,    89,    90,    -1,    -1,    93,   915,   916,    -1,    -1,
      -1,    99,    -1,   101,   923,    -1,    -1,   105,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,
     118,   119,    -1,    -1,   122,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,
     138,   139,   140,   141,   142,     0,   144,   145,   146,   147,
     148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,
      15,    16,    17,    18,   983,    20,   985,    -1,    -1,   988,
      -1,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    37,    38,    -1,    40,    41,    42,    43,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    57,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    90,    -1,    92,    93,    -1,
      -1,    -1,    -1,    -1,    99,    -1,   101,    -1,    -1,    -1,
     105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,   118,   119,    -1,   121,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,     0,    -1,    -1,   139,   140,   141,   142,    -1,    -1,
     145,   146,   147,   148,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    26,    27,    28,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    90,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,
      99,    -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,
     119,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    -1,    -1,     0,    -1,   138,
     139,   140,   141,   142,    -1,   144,   145,   146,   147,   148,
      13,    14,    15,    16,    17,    18,    -1,    20,    -1,    -1,
      -1,    -1,    -1,    -1,    27,    28,    -1,    -1,    -1,    -1,
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
     133,    -1,    -1,     0,    -1,    -1,   139,   140,   141,   142,
      -1,   144,   145,   146,   147,   148,    13,    14,    15,    -1,
      17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    26,
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
      -1,   138,   139,   140,    -1,   142,    -1,    -1,   145,   146,
     147,   148,    13,    14,    15,    -1,    17,    18,    -1,    20,
      -1,    -1,    -1,    -1,    -1,    26,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,     0,    -1,    -1,   139,   140,    -1,   142,    -1,    -1,
     145,   146,   147,   148,    13,    14,    15,    -1,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
     129,   130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,
     139,   140,    -1,   142,    -1,    -1,   145,   146,   147,   148,
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    -1,    -1,    18,    19,    -1,
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
      15,    -1,    17,    18,    19,    -1,    21,    22,    23,    24,
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
       9,    10,    11,    12,    -1,    -1,    15,    -1,    -1,    18,
      19,    20,    21,    22,    23,    24,    -1,    -1,    -1,    -1,
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
      -1,    -1,    15,    -1,    -1,    18,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    -1,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,   115,    -1,    -1,   118,   119,     1,    -1,     3,
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
     114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,   135,   136,    -1,    -1,   139,    -1,    -1,    -1,    -1,
      -1,    -1,   146,     1,   148,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    14,    15,    -1,    -1,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,    -1,
      -1,    -1,    30,    31,    32,    33,    34,    35,    36,    -1,
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
      19,    -1,    21,    22,    23,    24,    -1,    -1,   146,    -1,
     148,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
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
      -1,    21,    22,    23,    24,    -1,   145,   146,    -1,   148,
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
     111,   112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,   135,   136,    -1,    -1,   139,    -1,
      -1,    -1,    -1,    -1,    -1,   146,     1,   148,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      15,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      45,    -1,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
     115,    -1,    -1,   118,   119,    -1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,   134,
     135,   136,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,   146,    -1,   148,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    49,    50,    51,    52,    53,    54,    55,
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
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    45,    -1,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,   115,    -1,
      -1,   118,   119,    -1,    -1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,   134,   135,   136,
      -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,   146,
      -1,   148,    30,    31,    32,    33,    34,    35,    36,    -1,
      -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,
      -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,
      -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
      98,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   134,   135,   136,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,    15,    16,    17,    18,    19,    20,    21,    22,    23,
      24,    25,    26,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,    -1,    -1,    -1,    -1,
      -1,    45,    46,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
      74,    75,    76,    77,    -1,    -1,    80,    81,    -1,    -1,
      -1,    -1,    86,    87,    88,    89,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   100,   101,   102,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,
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
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   134,   135,   136,    -1,   138,    -1,    -1,    -1,
      -1,    -1,   144,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    26,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,   134,   135,   136,    19,   138,    21,
      22,    23,    24,    -1,   144,    -1,    -1,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      92,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
     102,   103,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,   121,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    -1,
      -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,    22,
      23,    24,   144,    -1,    -1,    -1,    -1,    30,    31,    32,
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
      -1,    -1,    -1,    -1,    -1,    -1,    70,    71,    72,    73,
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
       6,     7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,   134,    21,    22,    23,    24,    -1,
      -1,    -1,   142,    -1,    30,    31,    32,    33,    34,    35,
      36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    55,
      56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,
      66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,
      -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,
     106,   107,   108,    -1,   110,   111,   112,    -1,   114,    -1,
      -1,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,    21,
      22,    23,    24,    -1,    -1,    -1,   142,    -1,    30,    31,
      32,    33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,
      -1,    -1,    -1,    45,    46,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    -1,
      -1,    -1,   134,   135,   136,    19,    -1,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
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
      -1,    97,    98,    -1,    -1,    -1,   102,   103,   104,    -1,
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
      -1,    -1,   102,    -1,   104,    -1,   106,   107,   108,    -1,
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
      -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,
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
      94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,
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
      -1,    69,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      88,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,
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
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     101,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,   143,
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
      89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,    -1,    93,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   122,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    72,    73,    74,    75,    76,
      77,    -1,    -1,    80,    81,    -1,   101,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,   122,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   150,   151,     0,     1,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    19,    21,    22,    23,    24,
      30,    31,    32,    33,    34,    35,    36,    39,    45,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      58,    59,    60,    63,    66,    67,    69,    70,    71,    84,
      85,    91,    94,    95,    97,    98,   100,   104,   106,   107,
     108,   110,   111,   112,   114,   134,   135,   136,   152,   153,
     154,   159,   161,   163,   164,   165,   168,   169,   172,   173,
     175,   176,   177,   179,   180,   189,   203,   220,   241,   242,
     252,   253,   254,   258,   259,   260,   266,   267,   268,   270,
     271,   272,   273,   274,   275,   311,   324,   154,    21,    22,
      30,    31,    32,    39,    51,    55,    69,    88,    91,    94,
     134,   164,   165,   181,   182,   203,   220,   272,   275,   311,
     182,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    55,    70,    71,    72,    73,    74,    75,
      76,    77,    80,    81,    86,    87,    88,    89,   100,   101,
     102,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   135,   136,   143,   144,   183,   187,   188,   274,   306,
     204,    91,   163,   164,   165,   167,   180,   189,   220,   272,
     273,   275,   167,   210,   212,    69,    91,   173,   180,   220,
     225,   272,   275,    33,    34,    35,    36,    48,    49,    50,
      51,    55,   106,   183,   184,   185,   268,   115,   118,   119,
     146,   148,   167,   262,   263,   264,   317,   321,   322,   323,
      51,   100,   102,   103,   135,   172,   189,   195,   198,   201,
     254,   309,   310,   195,   195,   144,   192,   193,   196,   197,
     324,   192,   196,   144,   318,   184,   155,   138,   189,   220,
     189,   189,   189,    55,     1,    94,   157,   158,   159,   174,
     175,   324,   205,   207,   190,   201,   309,   324,   189,   308,
     309,   324,    91,   142,   179,   220,   272,   275,   208,    53,
      54,    56,    63,   107,   183,   269,    63,    64,    65,   116,
     117,   255,   256,    61,   255,    62,   255,    63,   255,    63,
     255,    58,    59,   168,   189,   189,   317,   323,    40,    41,
      42,    43,    44,    37,    38,    51,    53,    54,    55,    56,
      69,    94,   100,   101,   102,   103,   128,   131,   144,   278,
     279,   280,   281,   282,   285,   286,   287,   288,   290,   291,
     292,   293,   295,   296,   297,   300,   301,   302,   303,   304,
     324,   278,   280,    28,   239,   121,   142,    94,   100,   176,
     121,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    88,    89,    93,   101,   122,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    90,   105,
     140,   147,   315,    90,   315,   316,    26,   138,   243,   254,
      92,    92,   192,   196,   243,   163,    51,    55,   181,    58,
      59,   279,   125,   276,    90,   140,   315,   219,   307,    90,
     147,   314,   156,   157,    55,   278,   278,    16,   221,   321,
     121,    90,   140,   315,    92,    92,   221,   167,   167,    55,
      90,   140,   315,    25,   107,   142,   265,   317,   115,   264,
      20,   246,   321,    57,   189,   189,   189,    93,   142,   199,
     200,   324,    57,   199,   200,    85,   194,   195,   201,   309,
     324,   195,   163,   317,   319,   163,   322,   160,   138,   157,
      90,   315,    92,   159,   174,   145,   317,   323,   319,   159,
     319,   141,   200,   320,   323,   200,   320,   139,   320,    55,
     176,   177,   178,   142,    90,   140,   315,   144,   237,   290,
      63,   255,   257,   261,   262,    63,   256,    61,    62,    63,
      63,   101,   101,   154,   167,   167,   167,   167,   159,   163,
     163,    57,   121,   294,    85,   290,   295,   121,   156,   189,
     142,   305,   324,    51,   142,   305,   321,   142,   289,   189,
     142,   289,    51,   142,   289,    51,   121,   156,   240,   100,
     168,   189,   201,   202,   174,   142,   179,   142,   161,   162,
     168,   180,   189,   191,   202,   220,   275,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,    51,   189,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,    51,    52,    55,   187,   192,   312,
     313,   194,   201,    51,    52,    55,   187,   192,   312,    51,
      55,   312,   245,   244,   162,   189,   191,   162,   191,    99,
     170,   217,   277,   216,    51,    55,   181,   312,   194,   312,
     156,   163,   166,    15,    13,   248,   324,   121,   121,   157,
      16,    51,    55,   194,    51,    55,   157,    27,   222,   321,
     222,    51,    55,   194,    51,    55,   214,   186,   157,   246,
     189,   201,    15,   189,   189,   318,   100,   189,   198,   309,
     189,   310,   319,   145,   317,   200,   200,   319,   145,   184,
     152,   139,   191,   319,   159,   206,   309,   176,   178,    51,
      55,   194,    51,    55,   290,   209,    63,   157,   262,   189,
     189,    51,   100,   226,   295,   319,   319,   142,   172,   189,
      15,    51,   282,   287,   304,   288,   293,   300,   302,   295,
     297,   302,    51,   295,   172,   189,    15,    79,   126,   231,
     232,   324,   189,   200,   319,   178,   142,    44,   121,    44,
      90,   140,   315,   318,    92,    92,   192,   196,   141,   200,
      92,    92,   193,   196,   193,   196,   231,   231,   171,   321,
     167,   156,   141,    15,   319,   183,   189,   202,   249,   324,
      18,   224,   324,    17,   223,   224,    92,    92,   141,    92,
      92,   224,   211,   213,   141,   167,   184,   139,    15,   200,
     221,   189,   199,    85,   309,   139,   319,   320,   141,   234,
     318,    29,   113,   238,   139,   142,   292,   319,   142,    85,
      44,    44,   305,   142,   289,   142,   289,   142,   289,   142,
     289,   289,    44,    44,   228,   230,   233,   281,   283,   284,
     287,   295,   296,   298,   299,   302,   304,   156,   100,   189,
     178,   159,   189,    51,    55,   194,    51,    55,    57,   123,
     162,   191,   168,   191,   170,    92,   162,   191,   162,   191,
     170,   243,   239,   156,   157,   231,   218,   321,    15,    93,
     250,   324,   157,    14,   251,   324,   167,    15,    92,    15,
     157,   157,   222,   189,   157,   319,   200,   145,   146,   156,
     157,   227,   142,   100,   319,   189,   189,   295,   302,   295,
     295,   189,   189,   234,   234,    91,   220,   142,   305,   305,
     142,   229,   220,   142,   229,   142,   229,    15,   189,   141,
     189,   189,   162,   191,    15,   139,   157,   156,    91,   180,
     220,   272,   275,   221,   157,   221,    15,    15,   215,   224,
     246,   247,    51,   235,   236,   291,    15,   139,   295,   295,
     142,   292,   289,   142,   289,   289,   289,   126,   126,    55,
      90,   283,   287,   142,   228,   229,   299,   302,   295,   298,
     302,   295,   139,    15,    55,    90,   140,   315,   157,   157,
     157,   142,   318,   142,   295,   142,   295,    51,    55,   305,
     142,   229,   142,   229,   142,   229,   142,   229,   229,    51,
      55,   194,    51,    55,   248,   223,    15,   236,   295,   289,
     295,   302,   295,   295,   141,   229,   142,   229,   229,   229,
     295,   229
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   149,   151,   150,   152,   153,   153,   153,   153,   154,
     155,   154,   156,   157,   158,   158,   158,   158,   160,   159,
     159,   159,   159,   159,   159,   159,   159,   159,   159,   159,
     159,   159,   159,   159,   161,   161,   161,   161,   161,   161,
     161,   161,   161,   161,   161,   161,   162,   162,   162,   163,
     163,   163,   163,   163,   163,   164,   166,   165,   167,   168,
     168,   169,   169,   171,   170,   172,   172,   172,   172,   172,
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
     197,   196,   198,   199,   199,   200,   201,   201,   201,   201,
     202,   202,   202,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   204,   203,   205,   206,   203,   207,   203,   203,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   208,   209,   203,   203,   203,   210,   211,   203,   212,
     213,   203,   203,   203,   214,   215,   203,   216,   203,   217,
     218,   203,   219,   203,   203,   203,   203,   203,   203,   203,
     220,   221,   221,   221,   222,   222,   223,   223,   224,   224,
     225,   225,   226,   226,   226,   226,   226,   226,   226,   226,
     227,   226,   228,   228,   228,   228,   229,   229,   230,   230,
     230,   230,   230,   230,   230,   230,   230,   230,   230,   230,
     230,   230,   230,   231,   231,   233,   232,   232,   232,   234,
     234,   235,   235,   236,   236,   237,   237,   238,   238,   240,
     239,   241,   241,   241,   241,   242,   242,   242,   242,   242,
     242,   242,   242,   242,   244,   243,   245,   243,   246,   247,
     247,   248,   248,   249,   249,   249,   250,   250,   251,   251,
     252,   252,   252,   252,   253,   253,   254,   254,   254,   254,
     255,   255,   256,   257,   256,   256,   256,   258,   258,   259,
     259,   260,   261,   261,   262,   262,   263,   263,   264,   265,
     264,   266,   266,   267,   267,   268,   269,   269,   269,   269,
     269,   269,   270,   270,   271,   271,   271,   271,   272,   272,
     272,   272,   272,   273,   273,   274,   274,   274,   274,   274,
     274,   274,   274,   275,   275,   276,   277,   276,   278,   278,
     279,   279,   279,   280,   280,   281,   282,   282,   283,   283,
     284,   284,   285,   285,   286,   286,   287,   287,   288,   288,
     288,   288,   289,   289,   290,   290,   290,   290,   290,   290,
     290,   290,   290,   290,   290,   290,   290,   290,   290,   291,
     291,   291,   291,   291,   292,   292,   293,   294,   293,   295,
     295,   296,   297,   298,   299,   299,   300,   300,   301,   301,
     302,   302,   303,   303,   304,   305,   305,   306,   307,   306,
     308,   308,   309,   309,   310,   310,   310,   310,   310,   311,
     311,   311,   312,   312,   312,   312,   313,   313,   313,   314,
     314,   315,   315,   316,   316,   317,   317,   318,   318,   319,
     320,   320,   320,   321,   321,   322,   322,   323,   323,   324
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
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
       0,     2,     2,     2,     1,     2,     1,     2,     3,     4,
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
       1,     1,     1,     1,     1,     0,     0,     4,     1,     1,
       3,     5,     3,     1,     2,     2,     2,     1,     2,     1,
       1,     3,     1,     3,     1,     1,     2,     1,     4,     2,
       2,     1,     2,     0,     6,     8,     4,     6,     4,     6,
       2,     4,     6,     2,     4,     2,     4,     1,     0,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     4,     1,
       3,     2,     2,     2,     1,     3,     1,     3,     1,     1,
       2,     1,     1,     1,     2,     2,     1,     1,     0,     4,
       1,     2,     1,     3,     3,     3,     2,     3,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     1,     0,     2,     2,
       0,     1,     1,     1,     1,     1,     1,     1,     2,     0
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
#line 1564 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 6005 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3:
#line 1569 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 6014 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4:
#line 1576 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6022 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5:
#line 1582 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6:
#line 1586 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7:
#line 1591 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6047 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8:
#line 1595 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6055 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10:
#line 1602 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 6064 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11:
#line 1607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 6075 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12:
#line 1619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[-2].nd)) {
                        (yyval.nd) = new_rescue(p, (yyvsp[-3].nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                        NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                      }
                      else if ((yyvsp[-1].nd)) {
                        yywarning(p, "else without rescue is useless");
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
#line 6101 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13:
#line 1643 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6109 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14:
#line 1649 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6117 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15:
#line 1653 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16:
#line 1658 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6134 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17:
#line 1662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 6142 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18:
#line 1667 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 6148 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19:
#line 1668 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 6156 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20:
#line 1672 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6164 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21:
#line 1676 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6172 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22:
#line 1680 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23:
#line 1684 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6188 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24:
#line 1688 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25:
#line 1692 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26:
#line 1696 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 6213 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28:
#line 1702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6221 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29:
#line 1706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6229 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30:
#line 1710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31:
#line 1714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6245 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 32:
#line 1718 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *lhs = new_lvar(p, (yyvsp[0].id));
                      void_expr_error(p, (yyvsp[-2].nd));
                      assignable(p, lhs);
                      (yyval.nd) = new_asgn(p, lhs, (yyvsp[-2].nd));
                    }
#line 6256 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34:
#line 1728 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6264 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35:
#line 1732 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6272 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36:
#line 1736 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37:
#line 1740 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38:
#line 1744 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39:
#line 1748 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 6305 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40:
#line 1753 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6313 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41:
#line 1757 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 6326 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42:
#line 1766 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 6340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43:
#line 1776 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 6353 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44:
#line 1785 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 6367 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 45:
#line 1795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6376 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47:
#line 1803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6384 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50:
#line 1812 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6392 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51:
#line 1816 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52:
#line 1820 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6408 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53:
#line 1824 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6416 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55:
#line 1832 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 6427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56:
#line 1841 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 6435 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 57:
#line 1845 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 6448 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58:
#line 1856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 6459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62:
#line 1870 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6467 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63:
#line 1876 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 6476 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64:
#line 1883 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 6486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65:
#line 1891 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6494 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66:
#line 1895 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 6503 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67:
#line 1900 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6511 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68:
#line 1904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 6520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69:
#line 1909 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 6528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70:
#line 1913 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 6537 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71:
#line 1918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 6545 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72:
#line 1922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6553 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73:
#line 1926 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74:
#line 1930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6569 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75:
#line 1934 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6577 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76:
#line 1940 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6585 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77:
#line 1944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6593 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79:
#line 1951 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6601 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80:
#line 1957 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6609 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81:
#line 1961 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 6617 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82:
#line 1965 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6625 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83:
#line 1969 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6633 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84:
#line 1973 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 6641 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85:
#line 1977 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 6649 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86:
#line 1981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 6657 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87:
#line 1985 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6665 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88:
#line 1989 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 6673 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89:
#line 1993 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 6681 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91:
#line 2000 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 6689 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92:
#line 2006 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 6697 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93:
#line 2010 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 6705 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94:
#line 2016 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6713 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95:
#line 2020 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 6721 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96:
#line 2026 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6729 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97:
#line 2030 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 6737 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98:
#line 2034 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6745 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99:
#line 2038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6753 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100:
#line 2042 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6761 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101:
#line 2046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6771 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102:
#line 2052 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6781 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103:
#line 2058 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6790 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104:
#line 2065 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6798 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105:
#line 2069 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 6806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106:
#line 2073 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6814 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107:
#line 2077 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6822 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108:
#line 2081 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6830 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109:
#line 2085 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6840 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110:
#line 2091 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111:
#line 2097 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6859 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112:
#line 2102 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 6867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113:
#line 2108 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 6875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115:
#line 2115 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(nint(1), nsym((yyvsp[0].id)));
                    }
#line 6883 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116:
#line 2119 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(nint(0), nsym((yyvsp[0].id)));
                    }
#line 6891 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117:
#line 2123 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 6900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121:
#line 2133 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6909 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122:
#line 2138 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 6918 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125:
#line 2149 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 6926 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126:
#line 2152 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 6932 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127:
#line 2153 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 6940 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128:
#line 2158 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(or);     }
#line 6946 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129:
#line 2159 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(xor);    }
#line 6952 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130:
#line 2160 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(and);    }
#line 6958 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131:
#line 2161 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(cmp);    }
#line 6964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132:
#line 2162 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eq);     }
#line 6970 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133:
#line 2163 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eqq);    }
#line 6976 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134:
#line 2164 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(match);  }
#line 6982 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135:
#line 2165 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(nmatch); }
#line 6988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136:
#line 2166 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(gt);     }
#line 6994 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137:
#line 2167 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(ge);     }
#line 7000 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138:
#line 2168 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lt);     }
#line 7006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139:
#line 2169 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(le);     }
#line 7012 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140:
#line 2170 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neq);    }
#line 7018 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141:
#line 2171 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lshift); }
#line 7024 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142:
#line 2172 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(rshift); }
#line 7030 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143:
#line 2173 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(add);    }
#line 7036 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144:
#line 2174 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(sub);    }
#line 7042 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145:
#line 2175 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 7048 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146:
#line 2176 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 7054 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147:
#line 2177 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(div);    }
#line 7060 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148:
#line 2178 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mod);    }
#line 7066 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149:
#line 2179 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 7072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150:
#line 2180 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 7078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151:
#line 2181 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(not);    }
#line 7084 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152:
#line 2182 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neg);    }
#line 7090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153:
#line 2183 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(plus);   }
#line 7096 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154:
#line 2184 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(minus);  }
#line 7102 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155:
#line 2185 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aref);   }
#line 7108 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156:
#line 2186 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aset);   }
#line 7114 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157:
#line 2187 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(tick);   }
#line 7120 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198:
#line 2205 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7128 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199:
#line 2209 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7136 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200:
#line 2213 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7144 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201:
#line 2217 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7152 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202:
#line 2221 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7160 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203:
#line 2225 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7168 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204:
#line 2229 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7177 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205:
#line 2234 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7186 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206:
#line 2239 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7195 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207:
#line 2244 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7203 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208:
#line 2248 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7211 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209:
#line 2252 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7219 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210:
#line 2256 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7227 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211:
#line 2260 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7235 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212:
#line 2264 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213:
#line 2268 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 7251 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214:
#line 2272 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 7259 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215:
#line 2276 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 7267 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216:
#line 2280 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 7275 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217:
#line 2284 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 7283 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218:
#line 2288 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 7291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219:
#line 2292 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 7299 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220:
#line 2296 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 7307 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221:
#line 2300 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 7315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222:
#line 2304 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 7323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223:
#line 2308 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 7331 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224:
#line 2312 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 7339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225:
#line 2316 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 7347 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226:
#line 2320 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 7355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227:
#line 2324 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 7363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228:
#line 2328 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 7371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229:
#line 2332 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 7379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230:
#line 2336 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 7387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231:
#line 2340 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 7395 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232:
#line 2344 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 7403 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233:
#line 2348 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 7411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234:
#line 2352 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 7419 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235:
#line 2356 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 7427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236:
#line 2360 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7435 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237:
#line 2364 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 7443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238:
#line 2368 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 7451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239:
#line 2372 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 7459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240:
#line 2376 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7467 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241:
#line 2380 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7475 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242:
#line 2384 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7483 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243:
#line 2388 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7491 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244:
#line 2392 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7504 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245:
#line 2401 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7518 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246:
#line 2411 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7531 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247:
#line 2420 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7545 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248:
#line 2430 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7553 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250:
#line 2437 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7562 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251:
#line 2442 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd)));
                    }
#line 7570 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252:
#line 2446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7579 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253:
#line 2453 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7587 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254:
#line 2457 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7597 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255:
#line 2465 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7605 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256:
#line 2469 "mrbgems/mruby-compiler/core/parse.y"
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
#line 7625 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257:
#line 2485 "mrbgems/mruby-compiler/core/parse.y"
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
#line 7653 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262:
#line 2517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7662 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263:
#line 2522 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264:
#line 2527 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265:
#line 2534 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(list1((yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7690 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266:
#line 2540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons((yyvsp[-1].nd), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7699 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267:
#line 2545 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(list1(new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7708 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268:
#line 2550 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(push((yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd))), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7717 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269:
#line 2555 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7726 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270:
#line 2561 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 7735 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 271:
#line 2566 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7744 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272:
#line 2573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 7752 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273:
#line 2579 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7760 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274:
#line 2583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 7768 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 276:
#line 2592 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[0].nd), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7778 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277:
#line 2598 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_splat(p, (yyvsp[0].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7788 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278:
#line 2604 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7797 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279:
#line 2609 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280:
#line 2616 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7815 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281:
#line 2621 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7824 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282:
#line 2626 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 7833 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 290:
#line 2640 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 7841 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 291:
#line 2644 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 7849 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 292:
#line 2648 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293:
#line 2654 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294:
#line 2659 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7876 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295:
#line 2663 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 7882 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296:
#line 2664 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7891 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297:
#line 2668 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 7897 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298:
#line 2669 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 7905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299:
#line 2673 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300:
#line 2677 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 7921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301:
#line 2681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 7929 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302:
#line 2685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7938 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303:
#line 2690 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7947 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304:
#line 2695 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 7955 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305:
#line 2699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7963 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306:
#line 2703 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 7971 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307:
#line 2707 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 7979 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308:
#line 2711 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), cons(0, (yyvsp[0].nd)));
                    }
#line 7987 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310:
#line 2716 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 311:
#line 2721 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 8006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312:
#line 2727 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 8015 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313:
#line 2732 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 8027 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314:
#line 2743 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 8036 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315:
#line 2751 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 8045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316:
#line 2755 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 8051 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317:
#line 2755 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 8057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318:
#line 2758 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 8066 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319:
#line 2762 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 8072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320:
#line 2762 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 8078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321:
#line 2765 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 8087 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322:
#line 2772 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8095 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323:
#line 2776 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 8103 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324:
#line 2780 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 8109 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325:
#line 2782 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 8115 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326:
#line 2785 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 8124 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327:
#line 2791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8135 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328:
#line 2799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8146 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329:
#line 2807 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 8155 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330:
#line 2812 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 8165 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331:
#line 2819 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 8178 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332:
#line 2829 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8189 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333:
#line 2837 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8200 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334:
#line 2847 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8211 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335:
#line 2857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8223 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336:
#line 2865 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 8231 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337:
#line 2869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 8239 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338:
#line 2873 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 8247 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 339:
#line 2877 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 8255 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340:
#line 2883 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 8264 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 347:
#line 2902 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8272 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 349:
#line 2909 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 350:
#line 2915 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 8288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352:
#line 2922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 8296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 353:
#line 2926 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8304 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354:
#line 2930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355:
#line 2934 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3((yyvsp[-2].nd), nint(-1), 0);
                    }
#line 8321 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356:
#line 2939 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), nint(-1), (yyvsp[0].nd));
                    }
#line 8329 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357:
#line 2943 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8337 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358:
#line 2947 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8345 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359:
#line 2951 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = list3(0, nint(-1), 0);
                    }
#line 8354 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360:
#line 2956 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                    }
#line 8362 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361:
#line 2960 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, nint(-1), (yyvsp[0].nd));
                    }
#line 8370 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362:
#line 2966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8378 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363:
#line 2970 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 8386 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364:
#line 2974 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8394 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365:
#line 2978 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 8402 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366:
#line 2984 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8410 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367:
#line 2988 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 8418 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368:
#line 2994 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8426 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369:
#line 2998 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8434 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370:
#line 3002 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8442 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371:
#line 3006 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8450 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372:
#line 3010 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8458 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373:
#line 3014 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8466 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374:
#line 3018 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8474 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375:
#line 3022 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8482 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376:
#line 3026 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8490 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377:
#line 3030 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378:
#line 3034 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8506 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379:
#line 3038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8514 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380:
#line 3042 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8522 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381:
#line 3046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8530 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382:
#line 3050 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8538 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383:
#line 3056 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8547 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384:
#line 3061 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8556 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385:
#line 3067 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p, 0);}
#line 8562 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386:
#line 3068 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8570 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387:
#line 3072 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8579 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388:
#line 3077 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8587 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389:
#line 3084 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8595 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390:
#line 3088 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8603 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 393:
#line 3098 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 8612 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395:
#line 3106 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8620 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 396:
#line 3110 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8628 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397:
#line 3116 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8636 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398:
#line 3120 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8644 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399:
#line 3126 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8653 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400:
#line 3133 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8663 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401:
#line 3141 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (typen((yyvsp[-1].nd)->car) == NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8677 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402:
#line 3151 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8685 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403:
#line 3155 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404:
#line 3160 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8703 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405:
#line 3167 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8711 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406:
#line 3171 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8719 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407:
#line 3175 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8727 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408:
#line 3179 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8735 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409:
#line 3183 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 8743 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410:
#line 3187 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), tCOLON2);
                    }
#line 8751 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411:
#line 3191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8759 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412:
#line 3195 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 8767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413:
#line 3199 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414:
#line 3205 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8785 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415:
#line 3212 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8796 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416:
#line 3219 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417:
#line 3226 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8817 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418:
#line 3237 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 8825 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419:
#line 3243 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 8838 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 421:
#line 3257 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 8847 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423:
#line 3265 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8855 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 426:
#line 3273 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8863 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 428:
#line 3280 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8871 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 435:
#line 3294 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = concat_string(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8879 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 438:
#line 3302 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8887 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 439:
#line 3306 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      (yyval.nd) = new_dstr(p, n);
                    }
#line 8899 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441:
#line 3317 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8907 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 442:
#line 3323 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8915 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443:
#line 3327 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 8924 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444:
#line 3333 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lex_strterm = (yyvsp[-2].nd);
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445:
#line 3338 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8941 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446:
#line 3342 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 8949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447:
#line 3348 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8957 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 448:
#line 3352 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      (yyval.nd) = new_dxstr(p, n);
                    }
#line 8969 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 449:
#line 3362 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8977 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450:
#line 3366 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8985 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 454:
#line 3379 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 8995 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 455:
#line 3385 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 9003 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 458:
#line 3395 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      inf->doc = push(inf->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 9013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 459:
#line 3401 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = p->lex_strterm;
                      p->lex_strterm = NULL;
                    }
#line 9022 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460:
#line 3407 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info * inf = parsing_heredoc_inf(p);
                      p->lex_strterm = (yyvsp[-2].nd);
                      inf->doc = push(push(inf->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 9032 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461:
#line 3415 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 9040 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 462:
#line 3419 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      (yyval.nd) = new_words(p, n);
                    }
#line 9052 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 463:
#line 3430 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 9061 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 464:
#line 3435 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      p->lstate = EXPR_ENDARG;
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      (yyval.nd) = new_dsym(p, new_dstr(p, n));
                    }
#line 9074 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465:
#line 3446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9082 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 470:
#line 3456 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 9090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 471:
#line 3460 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 9098 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 472:
#line 3466 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 9106 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473:
#line 3470 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      (yyval.nd) = new_symbols(p, n);
                    }
#line 9118 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476:
#line 3482 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 9126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 477:
#line 3486 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 9134 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478:
#line 3492 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 9142 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479:
#line 3496 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 9150 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480:
#line 3500 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 9158 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481:
#line 3504 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 9166 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482:
#line 3508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 9174 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483:
#line 3514 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 9182 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484:
#line 3518 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 9190 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485:
#line 3524 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 9198 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486:
#line 3528 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9206 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487:
#line 3532 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 9214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488:
#line 3536 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 9222 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489:
#line 3540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 9230 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 490:
#line 3544 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 9242 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491:
#line 3552 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 9253 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492:
#line 3559 "mrbgems/mruby-compiler/core/parse.y"
                    {
#ifdef MRB_UTF8_STRING
                      const char *enc = "UTF-8";
#else
                      const char *enc = "ASCII-8BIT";
#endif
                      (yyval.nd) = new_str(p, enc, strlen(enc));
                    }
#line 9266 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495:
#line 3574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9274 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 496:
#line 3578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9283 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 497:
#line 3583 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 500:
#line 3599 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9301 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501:
#line 3605 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502:
#line 3623 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9345 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504:
#line 3644 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9353 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505:
#line 3650 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 9361 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506:
#line 3656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507:
#line 3662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9380 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508:
#line 3669 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9389 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509:
#line 3674 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9398 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510:
#line 3681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9406 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511:
#line 3685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9414 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512:
#line 3691 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513:
#line 3695 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9430 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516:
#line 3705 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, nsym((yyvsp[0].id)));
                    }
#line 9438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517:
#line 3709 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, 0);
                    }
#line 9446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518:
#line 3715 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519:
#line 3719 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520:
#line 3723 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521:
#line 3727 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9478 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 522:
#line 3733 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523:
#line 3737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9494 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524:
#line 3743 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9502 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525:
#line 3747 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9510 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526:
#line 3751 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9518 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527:
#line 3755 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9526 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528:
#line 3759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9534 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529:
#line 3763 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9542 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530:
#line 3767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9550 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531:
#line 3771 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9558 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532:
#line 3775 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9566 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533:
#line 3779 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9574 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534:
#line 3783 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535:
#line 3787 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9590 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536:
#line 3791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537:
#line 3795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9606 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538:
#line 3799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(and));
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 9615 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539:
#line 3806 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 9624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540:
#line 3811 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 9633 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541:
#line 3816 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 9642 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542:
#line 3821 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 9651 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543:
#line 3826 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 9660 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544:
#line 3833 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9668 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545:
#line 3837 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9677 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546:
#line 3844 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 9685 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547:
#line 3848 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 9693 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548:
#line 3852 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn_param(p, (yyvsp[-1].nd), p->locals->car);
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 9703 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549:
#line 3860 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9711 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550:
#line 3864 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9719 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551:
#line 3870 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 9729 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552:
#line 3878 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9739 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553:
#line 3886 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9749 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554:
#line 3894 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9757 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555:
#line 3898 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9765 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 556:
#line 3904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557:
#line 3908 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9781 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560:
#line 3918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9790 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561:
#line 3923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.id) = -1;
                    }
#line 9799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564:
#line 3934 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 565:
#line 3940 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9815 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 566:
#line 3944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9823 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 567:
#line 3950 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9832 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 568:
#line 3954 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 9838 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 569:
#line 3955 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9865 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 571:
#line 3981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9873 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572:
#line 3987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 9882 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573:
#line 3992 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9890 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574:
#line 3998 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9900 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 575:
#line 4004 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9909 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 576:
#line 4009 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), new_lvar(p, (yyvsp[-1].id)));
                    }
#line 9917 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 577:
#line 4013 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if (typen((yyvsp[-2].nd)->car) == NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 9931 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 578:
#line 4023 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 9940 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 591:
#line 4050 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 9948 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 592:
#line 4054 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 9956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 594:
#line 4061 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 9964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 603:
#line 4082 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 9970 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 605:
#line 4087 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 9979 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 609:
#line 4099 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9987 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 9991 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 4103 "mrbgems/mruby-compiler/core/parse.y"

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
    c = (char *)parser_palloc(p, n + 1);
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
        if (end > len) end = len;
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
      if (newlen < len)
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
      yywarning_s(p, "floating-point numbers are not supported", tok(p));
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
        mrb_int n = mrb_int_read(tok(p), NULL, NULL);
        if (n > INT32_MAX) {
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
      if (p->lstate != EXPR_FNAME && toklen(p) == 2 && ISDIGIT(tok(p)[1]) && p->nvars) {
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
  if (!p->tree) return;
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
      mrb_parser_dump(p->mrb, p->tree, 0);
    }
  }
  MRB_CATCH(p->mrb->jmp) {
    p->nerr++;
    if (p->mrb->exc == NULL) {
      yyerror(p, "memory allocation error");
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

typedef mrb_bool mrb_parser_foreach_top_variable_func(mrb_state *mrb, mrb_sym sym, void *user);
void mrb_parser_foreach_top_variable(mrb_state *mrb, struct mrb_parser_state *p, mrb_parser_foreach_top_variable_func *func, void *user);

void
mrb_parser_foreach_top_variable(mrb_state *mrb, struct mrb_parser_state *p, mrb_parser_foreach_top_variable_func *func, void *user)
{
  const mrb_ast_node *n = p->tree;
  if ((intptr_t)n->car == NODE_SCOPE) {
    n = n->cdr->car;
    for (; n; n = n->cdr) {
      mrb_sym sym = sym(n->car);
      if (sym && !func(mrb, sym, user)) break;
    }
  }
}
