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
  node *c;

  if (p->cells) {
    c = p->cells;
    p->cells = p->cells->cdr;
  }
  else {
    c = (node*)parser_palloc(p, sizeof(mrb_ast_node));
  }

  c->car = car;
  c->cdr = cdr;
  c->lineno = p->lineno;
  c->filename_index = p->current_filename_index;
  /* beginning of next partial file; need to point the previous file */
  if (p->lineno == 0 && p->current_filename_index > 0) {
    c->filename_index--;
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
    node *n = p->locals->car;
    while (n) {
      if (sym(n->car) == sym) {
        mrb_int len;
        const char* name = mrb_sym_name_len(p->mrb, sym, &len);
        if (len > 0 && name[0] != '_') {
          yyerror(NULL, p, "duplicated argument name");
          return;
        }
      }
      n = n->cdr;
    }
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
  node *n = list4((node*)NODE_FCALL, 0, nsym(b), c);
  NODE_LINENO(n, c);
  return n;
}

/* (a b . c) */
static node*
new_callargs(parser_state *p, node *a, node *b, node *c)
{
  return cons(a, cons(b, c));
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
  return cons((node*)NODE_ZSUPER, 0);
}

/* (:yield . c) */
static node*
new_yield(parser_state *p, node *c)
{
  if (c && c->cdr && c->cdr->cdr) {
        yyerror(NULL, p, "both block arg and actual block given");
  }

  return cons((node*)NODE_YIELD, c);
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
  void_expr_error(p, a);
  return cons((node*)NODE_AND, cons(a, b));
}

/* (:or a b) */
static node*
new_or(parser_state *p, node *a, node *b)
{
  void_expr_error(p, a);
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
  void_expr_error(p, a);
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
  int nvar;
  node *nvars = p->nvars->cdr;
  while (nvars) {
    nvar = intn(nvars->car);
    if (nvar == -2) break; /* top of the scope */
    if (nvar > 0) {
      yyerror(NULL, p, "numbered parameter used in outer block");
      break;
    }
    nvars->car = nint(-1);
    nvars = nvars->cdr;
  }
  nvar = intn(p->nvars->car);
  if (nvar == -1) {
    yyerror(NULL, p, "numbered parameter used in inner block");
  }
  else {
    p->nvars->car = nint(nvar > num ? nvar : num);
  }
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

  local_add_blk(p);
  if (blk) local_add_f(p, blk);

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
new_kw_rest_args(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_KW_REST_ARGS, nsym(sym));
}

static node*
new_args_dots(parser_state *p, node *m)
{
  mrb_sym r = intern_op(mul);
  mrb_sym k = intern_op(pow);
  mrb_sym b = intern_op(and);
  local_add_f(p, r);
  return new_args(p, m, 0, r, 0,
                  new_args_tail(p, 0, new_kw_rest_args(p, k), b));
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
  a = setup_numparams(p, a);
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
  return new_fcall(p, MRB_SYM_2(p->mrb, Complex),
                  new_callargs(p, list2(list3((node*)NODE_INT, (node*)strdup("0"), nint(10)), imaginary), 0, 0));
}

static node*
new_rational(parser_state *p, node *rational)
{
  return new_fcall(p, MRB_SYM_2(p->mrb, Rational), new_callargs(p, list1(rational), 0, 0));
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
  char *str = (char*)mempool_realloc(p->pool, a->car, (size_t)a->cdr + 1, newlen + 1);
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
    for (c = a; c->cdr != NULL; c = c->cdr)
      ;
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
  parser_heredoc_info *inf = (parser_heredoc_info*)parser_palloc(p, sizeof(parser_heredoc_info));
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
    if (a->cdr && a->cdr->cdr) {
      yyerror(NULL, p, "both block arg and actual block given");
    }
    a->cdr->cdr = b;
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
    yyerror(NULL, p, "setter method cannot be defined by endless method definition");
  }
}

static void
call_with_block(parser_state *p, node *a, node *b)
{
  node *n;

  switch (typen(a->car)) {
  case NODE_SUPER:
  case NODE_ZSUPER:
    if (!a->cdr) a->cdr = new_callargs(p, 0, 0, b);
    else args_with_block(p, a->cdr, b);
    break;
  case NODE_CALL:
  case NODE_FCALL:
  case NODE_SCALL:
    /* (NODE_CALL recv mid (args kw . blk)) */
    n = a->cdr->cdr->cdr; /* (args kw . blk) */
    if (!n->car) n->car = new_callargs(p, 0, 0, b);
    else args_with_block(p, n->car, b);
    break;
  case NODE_RETURN:
  case NODE_BREAK:
  case NODE_NEXT:
    if (a->cdr == NULL) return;
    call_with_block(p, a->cdr, b);
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
  if (n->cdr->cdr) {
    yyerror(NULL, p, "block argument should not be given");
    return NULL;
  }
  if (!n->car) return NULL;
  if (!n->car->cdr) return n->car->car;
  return new_array(p, n->car);
}

static void
assignable(parser_state *p, node *lhs)
{
  switch (intn(lhs->car)) {
  case NODE_LVAR:
    local_add(p, sym(lhs->cdr));
    break;
  case NODE_CONST:
    if (p->in_def)
      yyerror(NULL, p, "dynamic constant assignment");
    break;
  }
}

static node*
var_reference(parser_state *p, node *lhs)
{
  if (intn(lhs->car) == NODE_LVAR) {
    if (!local_var_p(p, sym(lhs->cdr))) {
      node *n = new_fcall(p, sym(lhs->cdr), 0);
      cons_free(lhs);
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
    switch (typen(n->car)) {
    case NODE_INT:
    case NODE_STR:
    case NODE_DSTR:
    case NODE_XSTR:
    case NODE_DXSTR:
    case NODE_DREGX:
    case NODE_MATCH:
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


#line 1551 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 1494 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1730 "mrbgems/mruby-compiler/core/y.tab.c"

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
  YYSYMBOL_opt_rescue = 248,               /* opt_rescue  */
  YYSYMBOL_exc_list = 249,                 /* exc_list  */
  YYSYMBOL_exc_var = 250,                  /* exc_var  */
  YYSYMBOL_opt_ensure = 251,               /* opt_ensure  */
  YYSYMBOL_literal = 252,                  /* literal  */
  YYSYMBOL_string = 253,                   /* string  */
  YYSYMBOL_string_fragment = 254,          /* string_fragment  */
  YYSYMBOL_string_rep = 255,               /* string_rep  */
  YYSYMBOL_string_interp = 256,            /* string_interp  */
  YYSYMBOL_257_29 = 257,                   /* @29  */
  YYSYMBOL_xstring = 258,                  /* xstring  */
  YYSYMBOL_regexp = 259,                   /* regexp  */
  YYSYMBOL_heredoc = 260,                  /* heredoc  */
  YYSYMBOL_heredoc_bodies = 261,           /* heredoc_bodies  */
  YYSYMBOL_heredoc_body = 262,             /* heredoc_body  */
  YYSYMBOL_heredoc_string_rep = 263,       /* heredoc_string_rep  */
  YYSYMBOL_heredoc_string_interp = 264,    /* heredoc_string_interp  */
  YYSYMBOL_265_30 = 265,                   /* @30  */
  YYSYMBOL_words = 266,                    /* words  */
  YYSYMBOL_symbol = 267,                   /* symbol  */
  YYSYMBOL_basic_symbol = 268,             /* basic_symbol  */
  YYSYMBOL_sym = 269,                      /* sym  */
  YYSYMBOL_symbols = 270,                  /* symbols  */
  YYSYMBOL_numeric = 271,                  /* numeric  */
  YYSYMBOL_variable = 272,                 /* variable  */
  YYSYMBOL_var_lhs = 273,                  /* var_lhs  */
  YYSYMBOL_var_ref = 274,                  /* var_ref  */
  YYSYMBOL_backref = 275,                  /* backref  */
  YYSYMBOL_superclass = 276,               /* superclass  */
  YYSYMBOL_277_31 = 277,                   /* $@31  */
  YYSYMBOL_f_opt_arglist_paren = 278,      /* f_opt_arglist_paren  */
  YYSYMBOL_f_arglist_paren = 279,          /* f_arglist_paren  */
  YYSYMBOL_f_arglist = 280,                /* f_arglist  */
  YYSYMBOL_f_label = 281,                  /* f_label  */
  YYSYMBOL_f_kw = 282,                     /* f_kw  */
  YYSYMBOL_f_block_kw = 283,               /* f_block_kw  */
  YYSYMBOL_f_block_kwarg = 284,            /* f_block_kwarg  */
  YYSYMBOL_f_kwarg = 285,                  /* f_kwarg  */
  YYSYMBOL_kwrest_mark = 286,              /* kwrest_mark  */
  YYSYMBOL_f_kwrest = 287,                 /* f_kwrest  */
  YYSYMBOL_args_tail = 288,                /* args_tail  */
  YYSYMBOL_opt_args_tail = 289,            /* opt_args_tail  */
  YYSYMBOL_f_args = 290,                   /* f_args  */
  YYSYMBOL_f_bad_arg = 291,                /* f_bad_arg  */
  YYSYMBOL_f_norm_arg = 292,               /* f_norm_arg  */
  YYSYMBOL_f_arg_item = 293,               /* f_arg_item  */
  YYSYMBOL_294_32 = 294,                   /* @32  */
  YYSYMBOL_f_arg = 295,                    /* f_arg  */
  YYSYMBOL_f_opt_asgn = 296,               /* f_opt_asgn  */
  YYSYMBOL_f_opt = 297,                    /* f_opt  */
  YYSYMBOL_f_block_opt = 298,              /* f_block_opt  */
  YYSYMBOL_f_block_optarg = 299,           /* f_block_optarg  */
  YYSYMBOL_f_optarg = 300,                 /* f_optarg  */
  YYSYMBOL_restarg_mark = 301,             /* restarg_mark  */
  YYSYMBOL_f_rest_arg = 302,               /* f_rest_arg  */
  YYSYMBOL_blkarg_mark = 303,              /* blkarg_mark  */
  YYSYMBOL_f_block_arg = 304,              /* f_block_arg  */
  YYSYMBOL_opt_f_block_arg = 305,          /* opt_f_block_arg  */
  YYSYMBOL_singleton = 306,                /* singleton  */
  YYSYMBOL_307_33 = 307,                   /* $@33  */
  YYSYMBOL_assoc_list = 308,               /* assoc_list  */
  YYSYMBOL_assocs = 309,                   /* assocs  */
  YYSYMBOL_assoc = 310,                    /* assoc  */
  YYSYMBOL_operation = 311,                /* operation  */
  YYSYMBOL_operation2 = 312,               /* operation2  */
  YYSYMBOL_operation3 = 313,               /* operation3  */
  YYSYMBOL_dot_or_colon = 314,             /* dot_or_colon  */
  YYSYMBOL_call_op = 315,                  /* call_op  */
  YYSYMBOL_call_op2 = 316,                 /* call_op2  */
  YYSYMBOL_opt_terms = 317,                /* opt_terms  */
  YYSYMBOL_opt_nl = 318,                   /* opt_nl  */
  YYSYMBOL_rparen = 319,                   /* rparen  */
  YYSYMBOL_trailer = 320,                  /* trailer  */
  YYSYMBOL_term = 321,                     /* term  */
  YYSYMBOL_nl = 322,                       /* nl  */
  YYSYMBOL_terms = 323,                    /* terms  */
  YYSYMBOL_none = 324                      /* none  */
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
#define YYLAST   13386

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  176
/* YYNRULES -- Number of rules.  */
#define YYNRULES  620
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1085

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
       0,  1665,  1665,  1665,  1676,  1682,  1686,  1691,  1695,  1701,
    1703,  1702,  1716,  1743,  1749,  1753,  1758,  1762,  1768,  1768,
    1772,  1776,  1780,  1784,  1788,  1792,  1796,  1801,  1802,  1806,
    1810,  1814,  1818,  1824,  1827,  1831,  1835,  1839,  1843,  1847,
    1852,  1856,  1865,  1874,  1883,  1892,  1899,  1900,  1904,  1908,
    1909,  1913,  1917,  1921,  1925,  1929,  1939,  1938,  1953,  1962,
    1963,  1966,  1967,  1974,  1973,  1988,  1992,  1997,  2001,  2006,
    2010,  2015,  2019,  2023,  2027,  2031,  2037,  2041,  2047,  2048,
    2054,  2058,  2062,  2066,  2070,  2074,  2078,  2082,  2086,  2090,
    2096,  2097,  2103,  2107,  2113,  2117,  2123,  2127,  2131,  2135,
    2139,  2143,  2149,  2155,  2162,  2166,  2170,  2174,  2178,  2182,
    2188,  2194,  2199,  2205,  2209,  2212,  2216,  2220,  2227,  2228,
    2229,  2230,  2235,  2242,  2243,  2246,  2250,  2250,  2256,  2257,
    2258,  2259,  2260,  2261,  2262,  2263,  2264,  2265,  2266,  2267,
    2268,  2269,  2270,  2271,  2272,  2273,  2274,  2275,  2276,  2277,
    2278,  2279,  2280,  2281,  2282,  2283,  2284,  2285,  2288,  2288,
    2288,  2289,  2289,  2290,  2290,  2290,  2291,  2291,  2291,  2291,
    2292,  2292,  2292,  2293,  2293,  2293,  2294,  2294,  2294,  2294,
    2295,  2295,  2295,  2295,  2296,  2296,  2296,  2296,  2297,  2297,
    2297,  2297,  2298,  2298,  2298,  2298,  2299,  2299,  2302,  2306,
    2310,  2314,  2318,  2322,  2326,  2331,  2336,  2341,  2345,  2349,
    2353,  2357,  2361,  2365,  2369,  2373,  2377,  2381,  2385,  2389,
    2393,  2397,  2401,  2405,  2409,  2413,  2417,  2421,  2425,  2429,
    2433,  2437,  2441,  2445,  2449,  2453,  2457,  2461,  2465,  2469,
    2473,  2477,  2481,  2485,  2489,  2498,  2507,  2516,  2525,  2531,
    2532,  2537,  2541,  2548,  2552,  2559,  2563,  2572,  2589,  2590,
    2593,  2594,  2595,  2600,  2605,  2612,  2618,  2623,  2628,  2633,
    2640,  2640,  2651,  2655,  2661,  2665,  2671,  2674,  2680,  2684,
    2689,  2694,  2698,  2704,  2709,  2713,  2719,  2720,  2721,  2722,
    2723,  2724,  2725,  2726,  2731,  2730,  2742,  2746,  2741,  2751,
    2751,  2755,  2759,  2763,  2767,  2772,  2777,  2781,  2785,  2789,
    2793,  2797,  2798,  2804,  2811,  2803,  2824,  2832,  2840,  2840,
    2840,  2847,  2847,  2847,  2854,  2860,  2865,  2867,  2864,  2876,
    2874,  2892,  2897,  2890,  2914,  2912,  2928,  2938,  2949,  2953,
    2957,  2961,  2967,  2974,  2975,  2976,  2979,  2980,  2983,  2984,
    2992,  2993,  2999,  3003,  3006,  3010,  3014,  3018,  3023,  3027,
    3031,  3035,  3041,  3040,  3050,  3054,  3058,  3062,  3068,  3073,
    3078,  3082,  3086,  3090,  3094,  3098,  3102,  3106,  3110,  3114,
    3118,  3122,  3126,  3130,  3134,  3140,  3145,  3152,  3152,  3156,
    3161,  3168,  3172,  3178,  3179,  3182,  3187,  3190,  3194,  3200,
    3204,  3211,  3210,  3227,  3237,  3241,  3246,  3253,  3257,  3261,
    3265,  3269,  3273,  3277,  3281,  3285,  3292,  3291,  3306,  3305,
    3321,  3329,  3338,  3341,  3348,  3351,  3355,  3356,  3359,  3363,
    3366,  3370,  3373,  3374,  3375,  3376,  3379,  3380,  3386,  3387,
    3388,  3392,  3405,  3406,  3412,  3417,  3416,  3426,  3430,  3436,
    3440,  3453,  3457,  3463,  3466,  3467,  3470,  3476,  3482,  3483,
    3486,  3493,  3492,  3505,  3509,  3523,  3527,  3539,  3546,  3553,
    3554,  3555,  3556,  3557,  3561,  3567,  3571,  3581,  3582,  3583,
    3587,  3593,  3597,  3601,  3605,  3609,  3615,  3619,  3625,  3629,
    3633,  3637,  3641,  3645,  3649,  3657,  3664,  3670,  3671,  3675,
    3679,  3678,  3695,  3696,  3699,  3705,  3709,  3715,  3716,  3720,
    3724,  3730,  3734,  3740,  3746,  3753,  3759,  3766,  3770,  3776,
    3780,  3786,  3787,  3790,  3794,  3800,  3804,  3808,  3812,  3818,
    3823,  3828,  3832,  3836,  3840,  3844,  3848,  3852,  3856,  3860,
    3864,  3868,  3872,  3876,  3880,  3885,  3891,  3896,  3901,  3906,
    3911,  3918,  3922,  3929,  3934,  3933,  3945,  3949,  3955,  3963,
    3971,  3979,  3983,  3989,  3993,  3999,  4000,  4003,  4008,  4015,
    4016,  4019,  4023,  4029,  4033,  4039,  4045,  4045,  4052,  4053,
    4059,  4064,  4070,  4076,  4081,  4085,  4090,  4095,  4105,  4110,
    4116,  4117,  4118,  4121,  4122,  4123,  4124,  4127,  4128,  4129,
    4132,  4133,  4136,  4140,  4146,  4147,  4153,  4154,  4157,  4158,
    4161,  4164,  4165,  4166,  4169,  4170,  4173,  4178,  4181,  4182,
    4186
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

#define YYPACT_NINF (-870)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-621)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -870,  3957,   130,  8673, 10797, 11139,  6981,  -870, 10443, 10443,
    -870,  -870, 10911,  8163,  6597,  8909,  8909,  -870,  -870,  8909,
    4478,  1831,  -870,  -870,  -870,  -870,    19,  8163,  -870,    36,
    -870,  -870,  -870,  7123,  4070,  -870,  -870,  7265,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,    89, 10561, 10561, 10561, 10561,
     152,  5856,  1426,  9381,  9735,  8445,  -870,  7881,   422,   515,
    1221,   920,  1245,  -870,    90, 10679, 10561,  -870,  1443,  -870,
     988,  -870,   306,  1273,  1273,  -870,  -870,   138,    69,  -870,
      74, 11025,  -870,   107,  2381,   735,   769,    47,    53,  -870,
     399,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
     319,   119,  -870,   347,    98,  -870,  -870,  -870,  -870,  -870,
    -870,   124,   124,    19,   743,   773,  -870, 10443,   120,  5975,
     292,  1295,  1295,  -870,   147,  -870,   775,  -870,  -870,    98,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,    23,    51,
      54,    87,  -870,  -870,  -870,  -870,  -870,  -870,    93,   102,
     112,   145,  -870,   193,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,   200,
    5034,   222,   306,  1273,  1273,   188,   134, 13191,   831,   218,
     199,   224,   188, 10443, 10443,   852,   243,  -870,  -870,   960,
     275,    79,   104,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  8022,  -870,  -870,   167,  -870,  -870,  -870,  -870,
    -870,  -870,  1443,  -870,   620,  -870,   310,  -870,  -870,  1443,
    4206,   122, 10561, 10561, 10561, 10561,  -870, 13129,  -870,  -870,
     195,   320,   195,  -870,  -870,  -870,  9027,  -870,  -870,  8909,
    -870,  -870,  -870,  -870,  6597,  6835,  -870,   250,  6094,  -870,
     987,   296, 13253, 13253,   425,  8791,  5856,   263,  1443,   988,
    1443,   317,  -870,  8791,  1443,   281,  1129,  1129,  -870, 13129,
     302,  1129,  -870,   389, 11253,   316,  1004,  1007,  1068,  1402,
    -870,  -870,  -870,  -870,  -870,  1252,  -870,  -870,  -870,  -870,
    -870,  -870,   666,  1255,  -870,  -870,  1021,  -870,  1242,  -870,
    1317,  -870,  1320,   374,   378,  -870,  -870,  -870,  -870,  6359,
   10443, 10443, 10443, 10443,  8791, 10443, 10443,    68,  -870,  -870,
    -870,  -870,   424,  1443,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,  1589,   369,   381,  5034, 10561,  -870,   367,   465,   380,
    -870,  1443,  -870,  -870,  -870,   392, 10561,  -870,   410,   509,
     423,   518,  -870,  -870,   462,  5034,  -870,  -870,  9853,  -870,
    5856,  8559,   444,  9853, 10561, 10561, 10561, 10561, 10561, 10561,
   10561, 10561, 10561, 10561, 10561, 10561, 10561, 10561,   539, 10561,
   10561, 10561, 10561, 10561, 10561, 10561, 10561, 10561, 10561, 10561,
   10561,  3007,  -870,  8909,  -870,  3752,  -870,  -870, 12649,  -870,
    -870,  -870,  -870, 10679, 10679,  -870,   503,  -870,   306,  -870,
    1093,  -870,  -870,  -870,  -870,  -870,  -870, 11531,  8909, 11617,
    5034, 10443,  -870,  -870,  -870,   595,   604,   225,   501,   505,
    -870,  5180,   608, 10561, 11703,  8909, 11789, 10561, 10561,  5472,
     308,   308,   106, 11875,  8909, 11961,  -870,   566,  -870,  6094,
     310,  -870,  -870,  9971,   621,  -870, 10561, 10561, 13191, 13191,
   13191, 10561,  -870,  -870,  9145,  -870, 10561,  -870,  9499,  6716,
     493,  1443,   195,   195,  -870,  -870,   318,   499,  -870,  -870,
    -870,  8163,  5591,   506, 11703, 11789, 10561,   988,  1443,  -870,
    -870,  6478,   504,   988,  -870,  -870,  9617,  -870,  1443,  9735,
    -870,  -870,  -870,  1093,    74, 11253,  -870, 11253, 12047,  8909,
   12133,  2010,  -870,  -870,   507,  -870,  1345,  6094,   666,  -870,
    -870,  -870,  -870,  -870,  -870,  -870, 10561, 10561,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    1431,  1443,  1443,   513, 10679,   642, 13191,   605,  -870,  -870,
    -870,    37,  -870,  -870,  1691,  -870, 13191,  2010,  -870,  -870,
    1712,  -870,  -870, 10679,   644,    83, 10561,  -870, 12845,   195,
    -870,  1443, 11253,   521,  -870,  -870,  -870,   616,   549,  3375,
    -870,  -870,  1116,   307,  2135,  2135,  2135,  2135,  2029,  2029,
    3438,  2505,  2135,  2135, 13253, 13253,  1521,  1521,  -870,   296,
   13191,  2029,  2029,  1668,  1668,  1694,   237,   237,   296,   296,
     296,  3634,  7621,  4750,  7739,  -870,   124,  -870,   530,   195,
     432,  -870,   450,  -870,  -870,  4342,  -870,  -870,  2280,    83,
      83,  -870,  2882,  -870,  -870,  -870,  -870,  -870,  1443, 10443,
    5034,   874,   654,  -870,   124,   536,   124,   671,   318,  8304,
    -870, 10089,   674,  -870, 10561, 10561,   278,  -870,  7383,  7502,
     556,   309,   327,   674,  -870,  -870,  -870,  -870,    64,    73,
     558,   111,   121, 10443,  8163,   572,   690, 13191,    81,  -870,
   13191, 13191, 13191,   287, 10561, 13129,  -870,   195, 13191,  -870,
    -870,  -870,  -870,  9263,  9499,  -870,  -870,  -870,   576,  -870,
    -870,    22,   988,  1443,  1129,   444,  -870,   874,   654,   575,
    1031,  1156,  -870,   113,  2010,  -870,   579,  -870,   296,   296,
    -870,  -870,   912,  1443,   578,  -870,  -870,  1929,   681, 12721,
    -870,   679,   424,  -870,   380,  -870,  1443,  -870,  -870,   586,
     589,   606,  -870,   607,   679,   606,   710, 12783,  -870,  -870,
    2010,  5034,  -870,  -870, 12916, 10207,  -870,  -870, 11253,  8791,
   10679, 10561, 12219,  8909, 12305,   561, 10679, 10679,  -870,   503,
     551,  9145, 10679, 10679,  -870,   503,    53,   138,  5034,  6094,
      83,  -870,  1443,   740,  -870,  -870,  -870,  -870, 12845,  -870,
     665,  -870,  5737,   746,  -870, 10443,   749,  -870, 10561, 10561,
     379, 10561, 10561,   753,  6240,  6240,   128,   308,  -870,  -870,
    -870, 10325,  5326, 13191,  -870,  6716,   195,  -870,  -870,  -870,
     161,   625,  1449,  5034,  6094,  -870,  -870,  -870,   619,  -870,
    1484,  1443, 10561, 10561,  -870,  -870,  2010,  -870,  1712,  -870,
    1712,  -870,  1712,  -870,  -870, 10561, 10561,  -870,  -870,  -870,
   11367,  -870,   632,   380,   637, 11367,  -870,   640,   645,  -870,
     774, 10561, 12987,  -870,  -870, 13191,  4614,  4886,   647,   407,
     463, 10561, 10561,  -870,  -870,  -870,  -870,  -870, 10679,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,   778,   656,  6094,  5034,
    -870,  -870, 11481,   188,  -870,  -870,  6240,  -870,  -870,   188,
    -870, 10561,  -870,   782,   790,  -870, 13191,   168,  -870,  9499,
    -870,  1615,   791,   672,  1457,  1457,  1063,  -870, 13191, 13191,
     606,   675,   606,   606, 13191, 13191,   687,   688,   763,  1119,
     605,  -870,  -870,  1421,  -870,  1119,  2010,  -870,  1712,  -870,
    -870, 13058,   478, 13191, 13191,  -870,  -870,  -870,  -870,   691,
     805,   776,  -870,  1127,  1007,  1068,  5034,  -870,  5180,  -870,
    -870,  6240,  -870,  -870,  -870,  -870,   693,  -870,  -870,  -870,
    -870,   696,   696,  1457,   697,  -870,  1712,  -870,  -870,  -870,
    -870,  -870,  -870, 12391,  -870,   380,   605,  -870,  -870,   701,
     705,   708,  -870,   720,   708,  -870,  -870,  1093, 12477,  8909,
   12563,   604,   278,   851,  1615,   287,  1457,   696,  1457,   606,
     723,   733,  -870,  2010,  -870,  1712,  -870,  1712,  -870,  1712,
    -870,  -870,   874,   654,   745,   299,   535,  -870,  -870,  -870,
    -870,   696,  -870,   708,   751,   708,   708,   161,  -870,  1712,
    -870,  -870,  -870,   708,  -870
};

/* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE does not specify something else to do.  Zero
   means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     0,     0,     0,     0,   294,     0,     0,
     318,   321,     0,     0,   606,   338,   339,   340,   341,   306,
     270,   270,   491,   490,   492,   493,   608,     0,    10,     0,
     495,   494,   496,   481,   592,   483,   482,   485,   484,   477,
     478,   438,   439,   497,   498,   489,     0,     0,     0,     0,
       0,     0,   296,   620,   620,    88,   313,     0,     0,     0,
       0,     0,     0,   453,     0,     0,     0,     3,   606,     6,
       9,    27,    33,   545,   545,    49,    60,    59,     0,    76,
       0,    80,    90,     0,    54,   248,     0,    61,   311,   286,
     287,   436,   288,   289,   290,   434,   433,   465,   435,   432,
     488,     0,   291,   292,   270,     5,     1,     8,   338,   339,
     306,   620,   414,     0,   113,   114,   489,     0,     0,     0,
       0,   545,   545,   116,   499,   342,     0,   488,   292,     0,
     334,   168,   178,   169,   165,   194,   195,   196,   197,   176,
     191,   184,   174,   173,   189,   172,   171,   167,   192,   166,
     179,   183,   185,   177,   170,   186,   193,   188,   187,   180,
     190,   175,   164,   182,   181,   163,   161,   162,   158,   159,
     160,   118,   120,   119,   153,   154,   131,   132,   133,   140,
     137,   139,   134,   135,   155,   156,   141,   142,   146,   149,
     150,   136,   138,   128,   129,   130,   143,   144,   145,   147,
     148,   151,   152,   157,   576,    55,   121,   122,   575,     0,
       0,     0,    58,   545,   545,     0,     0,    54,     0,   488,
       0,   292,     0,     0,     0,   112,     0,   353,   352,     0,
       0,   488,   292,   187,   180,   190,   175,   158,   159,   160,
     118,   119,     0,   123,   125,    20,   124,   456,   461,   460,
     614,   616,   606,   617,     0,   458,     0,   618,   615,   607,
     590,   489,   278,   589,   273,     0,   265,   277,    74,   269,
     620,   436,   620,   580,    75,    73,   620,   259,   307,     0,
      72,   258,   413,    71,   606,     0,    18,     0,     0,   221,
       0,   222,   209,   212,   303,     0,     0,     0,   606,    15,
     606,    78,    14,     0,   606,     0,   611,   611,   249,     0,
       0,   611,   578,     0,     0,    86,     0,    96,   103,   545,
     471,   470,   472,   473,   467,     0,   469,   468,   440,   445,
     444,   447,     0,     0,   442,   449,     0,   451,     0,   463,
       0,   475,     0,   479,   480,    53,   236,   237,     4,   607,
       0,     0,     0,     0,     0,     0,     0,   552,   548,   547,
     546,   549,   550,     0,   554,   566,   521,   522,   570,   569,
     565,   545,     0,   507,     0,   514,   519,   620,   524,   620,
     544,     0,   551,   553,   556,   530,     0,   563,   530,   568,
     530,   572,   528,   503,     0,     0,   401,   403,     0,    92,
       0,    84,    81,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   208,   211,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   603,   620,   602,     0,   605,   604,     0,   418,
     416,   312,   437,     0,     0,   407,    65,   310,   331,   113,
     114,   115,   479,   480,   507,   500,   329,     0,   620,     0,
       0,     0,   601,   600,    56,     0,   620,   303,     0,     0,
     344,     0,   343,     0,     0,   620,     0,     0,     0,     0,
       0,     0,   303,     0,   620,     0,   326,     0,   126,     0,
       0,   457,   459,     0,     0,   619,   584,   585,   279,   588,
     272,     0,   608,   266,     0,   275,     0,   267,     0,   606,
       0,   606,   620,   620,   260,   271,   606,     0,   309,    52,
     609,     0,     0,     0,     0,     0,     0,    17,   606,   301,
      13,   607,    77,   297,   300,   304,   613,   250,   612,   613,
     252,   305,   579,   102,    94,     0,    89,     0,     0,   620,
       0,   545,   314,   398,   530,   474,     0,     0,   448,   454,
     441,   443,   450,   452,   464,   476,     0,     0,     7,    21,
      22,    23,    24,    25,    50,    51,   511,   558,   512,   510,
       0,   606,   606,   530,     0,     0,   513,     0,   526,   574,
     523,     0,   527,   508,     0,   537,   559,     0,   540,   567,
       0,   542,   571,     0,     0,   620,   278,    28,    30,     0,
      31,   606,     0,    82,    93,    48,    34,    46,     0,   253,
     198,    29,     0,   292,   226,   231,   232,   233,   228,   230,
     240,   241,   234,   235,   207,   210,   238,   239,    32,   218,
     608,   227,   229,   223,   224,   225,   213,   214,   215,   216,
     217,   593,   598,   594,   599,   412,   270,   410,     0,   620,
     593,   595,   594,   596,   411,   270,   593,   594,   270,   620,
     620,    35,   253,   199,    45,   206,    63,    66,     0,     0,
       0,   113,   114,   117,     0,     0,   620,     0,   606,     0,
     295,   620,   620,   424,     0,     0,   620,   345,   597,   302,
       0,   593,   594,   620,   347,   319,   346,   322,   597,   302,
       0,   593,   594,     0,     0,     0,     0,   277,     0,   325,
     583,   586,   582,   276,   281,   280,   274,   620,   587,   581,
     257,   255,   261,   262,   264,   308,   610,    19,     0,    26,
     205,    79,    16,   606,   611,    95,    87,    99,   101,     0,
      98,   100,   608,     0,     0,   466,     0,   455,   219,   220,
     552,   550,   361,   606,   354,   506,   504,     0,    41,   244,
     336,     0,     0,   520,   620,   573,     0,   529,   557,   530,
     530,   530,   564,   530,   552,   530,    43,   246,   337,   389,
     387,     0,   386,   385,   285,     0,    91,    85,     0,     0,
       0,     0,     0,   620,     0,     0,     0,     0,   409,    69,
     415,   262,     0,     0,   408,    67,   404,    62,     0,     0,
     620,   332,     0,     0,   415,   335,   577,    57,   425,   426,
     620,   427,     0,   620,   350,     0,     0,   348,     0,     0,
     415,     0,     0,     0,     0,     0,   415,     0,   127,   462,
     324,     0,     0,   282,   268,   606,   620,    11,   298,   251,
      97,     0,   391,     0,     0,   315,   446,   362,   359,   555,
       0,   606,     0,     0,   525,   509,     0,   533,     0,   535,
       0,   541,     0,   538,   543,     0,     0,   384,   608,   608,
     516,   517,   620,   620,   369,     0,   561,   369,   369,   367,
       0,   281,   283,    83,    47,   254,   593,   594,     0,   593,
     594,     0,     0,    40,   203,    39,   204,    70,     0,    37,
     201,    38,   202,    68,   405,   406,     0,     0,     0,     0,
     501,   330,     0,     0,   429,   351,     0,    12,   431,     0,
     316,     0,   317,     0,     0,   327,   280,   620,   256,   263,
     397,     0,     0,     0,     0,     0,   357,   505,    42,   245,
     530,   530,   530,   530,    44,   247,     0,     0,     0,   515,
       0,   365,   366,   369,   377,   560,     0,   380,     0,   382,
     402,   284,   415,   243,   242,    36,   200,   419,   417,     0,
       0,     0,   428,     0,   104,   111,     0,   430,     0,   320,
     323,     0,   421,   422,   420,   395,   608,   393,   396,   400,
     399,   363,   360,     0,   355,   534,     0,   531,   536,   539,
     390,   388,   303,     0,   518,   620,     0,   368,   375,   369,
     369,   369,   562,   369,   369,    64,   333,   110,     0,   620,
       0,   620,   620,     0,     0,   392,     0,   358,     0,   530,
     597,   302,   364,     0,   372,     0,   374,     0,   381,     0,
     378,   383,   107,   109,     0,   593,   594,   423,   349,   328,
     394,   356,   532,   369,   369,   369,   369,   105,   373,     0,
     370,   376,   379,   369,   371
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -870,  -870,  -870,   372,  -870,    43,  -870,  -299,   301,  -870,
    -870,    46,  -165,  -305,    34,  1724,  -870,  1884,    21,   -50,
    -870,  -870,  -483,   -12,   885,  -205,    15,   -27,  -285,  -477,
     -47,  2818,   -59,   893,     5,   -18,  -870,  -870,    38,  -870,
    1204,  -870,   184,    56,  -252,  -325,    96,  -870,    10,  -420,
    -231,    14,    52,  -358,    28,  -870,  -870,  -870,  -870,  -870,
    -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,  -870,
    -870,     9,  -188,  -444,  -141,  -623,  -870,  -870,  -870,   115,
     258,  -870,  -583,  -870,  -870,  -464,  -870,  -142,  -870,  -870,
    -870,    91,  -870,  -870,  -870,   -87,  -870,  -470,  -870,  -131,
    -870,  -870,  -870,  -870,  -870,   103,    41,  -160,  -870,  -870,
    -870,  -870,  -870,  -277,  -870,   670,  -870,  -870,  -870,   -11,
    -870,  -870,  -870,  2545,  2999,   913,  2385,  -870,  -870,    60,
     512,    25,   135,   341,   -40,  -870,  -870,  -870,   117,   730,
    -185,  -226,  -824,  -706,  -513,  -870,   186,  -751,  -547,  -869,
     -42,  -536,  -870,   252,  -870,   -44,  -343,  -870,  -870,  -870,
     101,  -445,   600,  -353,  -870,  -870,   -60,  -870,    24,   -20,
     492,  -250,   508,  -280,   -46,    -1
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
       0,     1,     2,    67,    68,    69,   287,   465,   466,   298,
     521,   299,    71,   616,    72,   213,   689,   214,   215,    75,
      76,   820,   677,    77,    78,   300,    79,    80,    81,   546,
      82,   216,   123,   124,   243,   244,   245,   714,   654,   207,
      84,   305,   620,   655,   278,   510,   511,   279,   280,   269,
     503,   539,   659,   610,    85,   210,   303,   743,   304,   319,
     753,   223,   844,   224,   845,   713,  1001,   680,   678,   929,
     460,   290,   471,   705,   836,   837,   230,   763,   954,  1027,
     974,   888,   791,   889,   792,   861,  1006,  1007,   552,   865,
     605,   397,    87,    88,   670,   447,   669,   494,  1004,   692,
     830,   933,   937,    89,    90,    91,   333,   334,   557,    92,
      93,    94,   558,   253,   254,   255,   489,    95,    96,    97,
     327,    98,    99,   219,   220,   102,   221,   456,   679,   372,
     373,   374,   375,   376,   891,   892,   377,   378,   379,   777,
     595,   381,   382,   383,   384,   580,   385,   386,   387,   896,
     897,   388,   389,   390,   391,   392,   588,   209,   461,   310,
     513,   273,   129,   684,   657,   464,   459,   438,   517,   862,
     518,   537,   257,   258,   259,   302
};

/* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule whose
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     105,   441,   246,   266,   266,   520,   285,   266,   315,   286,
      86,   205,    86,   126,   126,   345,   246,   218,   218,   281,
     716,   229,   349,   218,   218,   218,   435,   437,   218,   545,
     222,   283,   125,   125,   479,   252,   592,   707,   256,   895,
     125,   507,   212,   212,   206,   621,   107,    70,   212,    70,
     782,   206,   308,   312,   402,   559,   868,   540,   779,   451,
      86,   542,   326,   729,   316,   206,   301,   270,   270,   833,
     746,   270,   393,   393,   218,   585,   277,   282,   656,   439,
     843,   778,   665,   125,   726,   668,   818,   819,   726,  -107,
     316,   528,   348,   553,   729,   206,   604,   470,  -109,   395,
     336,   338,   340,   342,  -104,   306,   686,  1032,   658,   125,
     281,   268,   274,  -491,   446,   275,   272,   272,   271,   271,
     272,   656,   271,   665,   439,   576,   218,  1008,    86,  -111,
     106,  -110,   686,   685,   394,   797,  -106,   436,   671,   674,
     368,  -490,   863,   -77,  -492,   582,  -108,   395,   343,   344,
     700,   448,   432,  -105,   307,   311,   271,   271,   476,   710,
     445,   687,   789,   284,   -91,   369,   396,   277,   282,   485,
    -491,   449,   686,   561,   288,   450,   561,  -493,   561,   497,
     561,  -487,   561,  -495,  1032,   445,   832,  -415,   493,   577,
     398,   440,  -494,   442,   434,   611,   247,   686,  -490,   248,
     249,  -492,  -496,   598,   470,   601,   -99,   294,  -593,   790,
    -112,   443,   393,   393,  -487,  -101,   399,  -594,   779,    86,
    1008,   -96,   895,   502,   749,   895,   864,   250,   403,   251,
     525,   779,   218,   218,  -493,  -481,   440,   928,   615,   395,
    -495,   778,   276,  -112,   480,   481,  -103,   326,  -102,  -494,
    1014,  -415,   531,   -98,   778,   473,   550,   212,   212,  -496,
     538,   538,   545,  -100,   266,   538,  -415,   266,   276,   505,
     -97,   505,   455,   468,   469,   514,   490,   467,   615,   615,
     206,   757,   507,  -485,   504,   218,   508,   544,   218,   729,
     462,   477,  -481,   218,   218,   835,   832,    86,   482,  -415,
     486,  -415,   895,   247,    86,    86,   248,   249,  -415,   488,
    -486,   301,    86,   726,   726,  -106,   478,   526,   516,   519,
     536,   903,   530,   316,  1002,   752,   917,   545,   512,   782,
     493,   270,   923,   829,   250,   704,   251,   502,   419,  -104,
    -485,   527,   125,   355,   356,  -111,  -110,   463,   607,   533,
     452,   453,   297,   617,   613,   355,   356,   778,    86,   218,
     218,   218,   218,    86,   218,   218,   556,   778,   428,   429,
     430,   569,   570,   571,   572,   515,   589,   506,   589,   271,
     272,   823,   271,    86,   212,   212,   212,   212,   522,   574,
     575,   726,   568,   617,   617,    70,   561,   419,   683,   444,
     573,   841,   247,   945,    86,   248,   249,   218,   529,    86,
     316,  -486,   622,   814,  -106,   301,   816,  -106,  -106,   842,
     297,   266,   535,   247,   966,   967,   248,   249,  -111,   125,
    -106,   874,   514,   247,   814,   251,   248,   249,   -76,   444,
    -104,   541,   218,   520,   543,  -106,   266,  -106,  -108,   656,
     609,   665,   622,   622,   250,   609,   251,   514,   547,    41,
     778,   -96,    42,   266,   250,   693,   251,   218,  -111,    86,
     218,   941,   266,   663,   514,   566,   663,  1003,   908,   567,
      86,   578,   723,   514,   218,   328,   329,   330,    86,  -103,
     584,   664,   900,   218,   859,   688,   854,   663,    86,   812,
    -105,   913,  -502,   737,   729,   554,    58,   919,   921,   587,
     246,   505,   505,   545,   663,   664,   590,   526,   745,   926,
     544,   105,   591,   663,   812,   520,   733,   734,  -106,   726,
     852,    86,   664,   778,   594,   732,   271,   266,   331,   332,
      86,   664,   813,   775,   778,   718,  -110,   775,   514,   971,
     972,  -108,   597,  -106,   316,   813,   316,   583,   218,   206,
     599,   271,   804,   663,   952,   600,    86,  -102,    70,   602,
     918,  -108,   768,   125,   -98,   125,   335,   742,   271,   329,
     330,   664,   520,   603,  -108,   544,   614,   271,   663,   523,
     638,   786,  -100,   218,   877,   879,   881,   297,   883,  -105,
     884,   104,   676,   104,   793,   727,   664,   271,   104,   104,
     690,   271,   218,   985,   104,   104,   104,   691,   911,   104,
     805,   316,   694,   795,   697,   854,   695,   673,   675,   555,
     990,   331,   332,   454,   454,   615,   719,   744,   731,   271,
     125,   615,   271,   918,   736,   739,   -91,   615,   615,   754,
    -108,   104,   271,  -108,  -108,   767,   771,   770,   505,   788,
     799,   673,   675,   798,   281,   104,   809,   281,   793,   793,
     800,   810,  -105,   811,   772,   815,   247,   824,   817,   248,
     249,  -108,  1052,  -108,   912,   281,   825,   686,   218,    86,
     831,   834,   832,   -97,   827,   834,   848,   840,   538,   846,
     822,   297,   834,   246,   774,   850,   366,   367,   368,   251,
     740,   849,   808,   212,  1064,   857,   860,   104,   866,   104,
     870,   277,   218,   472,   277,   872,   505,   206,   876,   924,
     472,   878,   851,   369,   847,   491,   576,   554,   248,   249,
     808,   508,   277,   609,  -302,   996,   899,   212,   880,   882,
     617,   998,   206,   615,   885,   931,   617,   915,   932,  -302,
     936,   955,   617,   617,   940,   520,   764,   495,   942,  -590,
     950,   544,   696,   589,   970,  1015,  1017,  1018,  1019,   973,
     703,   247,   976,   781,   248,   249,   785,   978,   982,   980,
     715,   266,   532,   987,  -302,   988,   534,   999,  -594,  -591,
      86,  -302,   514,   380,   380,  1000,  1009,   316,    86,   622,
     104,  1010,   218,  1020,  1021,   622,   218,  1016,  1022,   793,
    1036,   622,   622,   104,   104,  -342,   125,    86,    86,   934,
    1035,  1037,   938,  -481,   856,  1044,   271,   271,  1046,  1048,
    -342,    86,   663,  1053,   218,   904,   780,  1055,  -481,   783,
    1057,   380,   380,    86,    86,   505,   939,   495,   756,   431,
     664,    86,  1059,  -485,  1072,   457,  1069,  -593,   617,   212,
     949,   579,    86,    86,   432,  -342,   104,  -594,  -485,   104,
     432,  -590,  -342,  -481,   104,   104,  1077,  -590,   104,   593,
    -481,   589,   589,  1079,   738,   104,   104,   227,   130,   969,
    -597,  1068,  1070,   104,   975,   887,   271,   893,   925,   433,
    1067,  -591,   856,  -485,   271,   458,   434,  -591,   125,   208,
    -485,   474,   434,   125,   492,   890,   775,   622,   773,   899,
    1024,  1029,   899,  1040,   899,     0,   432,    86,    86,     0,
       0,   993,  -489,   380,   380,    86,   834,     0,     0,   104,
     104,   104,   104,   104,   104,   104,   104,  -489,     0,     0,
     125,     0,     0,   760,  -597,   358,   359,   360,   361,     0,
       0,   475,     0,     0,   104,     0,   894,     0,   434,  -597,
       0,   761,   899,   339,   329,   330,  1045,     0,   706,   706,
     914,   916,  -489,     0,     0,   104,   920,   922,   104,  -489,
     104,   730,     0,   104,     0,    86,   780,    86,   735,   899,
      86,   899,  -597,   899,  -597,   899,     0,     0,  -593,   780,
     741,  -597,   914,   916,   589,   920,   922,   266,   350,   351,
     352,   353,   354,   104,     0,   899,   331,   332,   514,   495,
     693,   834,   898,   104,   104,     0,   495,     0,   218,   380,
     483,     0,   271,     0,   867,     0,     0,  -593,   104,     0,
     104,   104,   960,     0,   962,   432,     0,     0,   963,     0,
       0,   104,     0,   765,   766,   104,     0,   524,   663,   104,
       0,     0,   562,     0,   104,   329,   330,  1025,     0,   104,
     893,     0,   432,   893,   548,   893,   664,  -488,     0,     0,
     484,   380,   986,   796,     0,   890,     0,   434,   890,   432,
       0,   890,  -488,   890,   760,     0,   358,   359,   360,   361,
     927,  -593,   104,     0,     0,   986,     0,   475,   961,     0,
       0,   104,   761,   935,   434,     0,  -593,   331,   332,     0,
    1011,  1012,   271,   893,   549,   943,   944,  -488,     0,   104,
       0,   434,     0,   947,  -488,   977,   979,   104,  -292,     0,
       0,   890,  1031,     0,  1034,   953,     0,     0,     0,  -593,
     893,  -593,   893,  -292,   893,  -593,   893,     0,  -593,     0,
     826,     0,  -594,  -303,   104,     0,   821,     0,   890,     0,
     890,     0,   890,     0,   890,     0,   893,     0,  -303,  1047,
       0,     0,  1049,   104,     0,  1013,   802,     0,  -292,  1023,
       0,     0,   217,   217,   890,  -292,     0,  1038,   217,   267,
     267,   432,     0,   267,   432,  1030,   472,     0,  1033,   989,
       0,  1028,   432,  -303,  1071,   858,     0,   997,     0,  1073,
    -303,  1075,     0,     0,   247,  1076,  -594,   248,   249,     0,
     289,   291,   292,   293,     0,   869,   803,   267,   309,   458,
       0,  -594,     0,   434,     0,  1083,   434,  1039,     0,   346,
     347,   502,     0,     0,   434,   250,     0,   251,     0,   104,
     104,   380,     0,   337,   875,   329,   330,  1054,  1056,  1058,
       0,  1060,  1061,     0,  -594,     0,  -594,  1041,     0,  1042,
    -594,     0,  1043,  -594,   563,  1074,   329,   330,   341,   329,
     330,     0,     0,   104,     0,   555,   329,   330,   560,   329,
     330,   217,     0,     0,   357,     0,   358,   359,   360,   361,
     930,  1078,  1080,  1081,  1082,     0,     0,   331,   332,     0,
       0,  1084,   362,     0,     0,     0,   357,   948,   358,   359,
     360,   361,     0,     0,     0,   706,   363,     0,   331,   332,
       0,   331,   332,   957,   362,     0,     0,   364,   331,   332,
       0,   331,   332,   365,   366,   367,   368,     0,   363,     0,
     564,   329,   330,   565,   329,   330,     0,     0,     0,   364,
       0,   104,     0,     0,  -620,   365,   366,   367,   368,   104,
     104,   369,     0,   104,   370,     0,   104,   104,   755,   329,
     330,     0,   104,   104,     0,     0,     0,   371,   104,   104,
       0,     0,     0,   369,     0,     0,   370,   217,   217,     0,
       0,     0,   104,   331,   332,   104,   331,   332,     0,   371,
       0,   472,     0,     0,   104,   104,     0,   472,     0,     0,
       0,     0,   104,   357,     0,   358,   359,   360,   361,     0,
       0,   331,   332,   104,   104,     0,   498,   499,   500,   346,
       0,   362,   357,     0,   358,   359,   360,   361,     0,     0,
     267,     0,   760,   267,   358,   359,   360,   361,   217,   217,
     362,     0,     0,     0,     0,     0,   364,     0,     0,     0,
     761,     0,   365,   366,   367,   368,     0,     0,   760,     0,
     358,   359,   360,   361,     0,   364,     0,     0,   104,     0,
       0,   365,   366,   367,   368,   364,   761,     0,   104,   104,
     369,   762,     0,   370,     0,   760,   104,   358,   359,   360,
     361,  -299,     0,     0,  -299,  -299,   551,     0,     0,   369,
       0,   364,   370,   761,   217,   217,   217,   217,   247,   217,
     217,   248,   249,  1026,   247,     0,     0,   248,   249,     0,
       0,  -299,  -299,     0,  -299,     0,     0,     0,   364,   586,
       0,     0,     0,     0,   956,     0,     0,     0,     0,   250,
     596,   251,     0,     0,     0,   951,   104,   251,   104,     0,
       0,   104,   608,     0,     0,     0,     0,   619,   624,   625,
     626,   627,   628,   629,   630,   631,   632,   633,   634,   635,
     636,   637,   419,   639,   640,   641,   642,   643,   644,   645,
     646,   647,   648,   649,   650,     0,     0,   267,     0,   104,
     357,     0,   358,   359,   360,   361,     0,   672,   672,     0,
     426,   427,   428,   429,   430,     0,     0,     0,   362,     0,
       0,     0,   267,     0,     0,   217,  1005,     0,   358,   359,
     360,   361,     0,     0,   581,     0,     0,   672,     0,   267,
       0,   672,   672,   364,   761,     0,     0,     0,   267,   365,
     366,   367,   368,     0,     0,     0,     0,   717,     0,     0,
     720,   721,     0,     0,     0,   722,     0,     0,   725,     0,
     728,     0,   309,   293,     0,     0,     0,   369,     0,     0,
     370,     0,     0,     0,     0,    73,     0,    73,   121,   121,
     672,     0,     0,     0,     0,     0,   121,     0,     0,     0,
     725,     0,   357,   309,   358,   359,   360,   361,     0,     0,
       0,     0,     0,   267,     0,     0,   416,   417,     0,     0,
     362,     0,     0,   784,     0,   358,   359,   360,   361,   419,
     758,   759,     0,     0,     0,    73,   776,     0,     0,   121,
       0,   362,   416,   417,     0,   364,     0,     0,   769,     0,
       0,   365,   366,   367,   368,   419,   425,   426,   427,   428,
     429,   430,     0,     0,     0,   121,   364,   787,     0,     0,
     794,     0,     0,   366,   367,   368,     0,     0,     0,   369,
       0,     0,   370,   426,   427,   428,   429,   430,     0,     0,
       0,  -414,     0,     0,     0,     0,     0,     0,     0,     0,
     369,     0,     0,    73,  -414,  -414,  -414,  -414,  -414,  -414,
       0,  -414,     0,     0,     0,     0,     0,  -414,  -414,  -414,
       0,     0,     0,     0,     0,     0,     0,     0,  -414,  -414,
       0,  -414,  -414,  -414,  -414,  -414,     0,     0,     0,     0,
       0,     0,     0,   217,     0,    74,     0,    74,   122,   122,
       0,     0,     0,     0,     0,   828,   122,     0,   769,   787,
       0,     0,     0,  -414,  -414,  -414,  -414,  -414,  -414,  -414,
    -414,  -414,  -414,  -414,  -414,     0,     0,   217,     0,  -414,
    -414,  -414,     0,     0,  -414,     0,     0,     0,   853,     0,
    -414,     0,  -414,     0,    73,    74,  -414,   725,   309,   122,
       0,     0,     0,     0,     0,     0,  -414,     0,     0,  -414,
    -414,     0,     0,  -414,     0,  -414,  -414,  -414,  -414,  -414,
    -414,  -414,  -414,  -414,  -414,   122,     0,     0,     0,  -414,
    -414,  -414,  -414,  -414,     0,   276,  -414,  -414,  -414,  -414,
     357,     0,   358,   359,   360,   361,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   362,   902,
       0,     0,     0,    74,   672,   905,     0,   267,     0,     0,
     672,   672,    73,     0,   871,   725,   672,   672,     0,    73,
      73,     0,     0,   364,     0,     0,     0,    73,     0,   365,
     366,   367,   368,     0,     0,     0,     0,     0,   121,   217,
       0,     0,   672,   672,     0,   672,   672,     0,     0,     0,
       0,     0,     0,     0,     0,   946,     0,   369,     0,   293,
     370,   357,     0,   358,   359,   360,   361,     0,     0,     0,
       0,     0,     0,    73,     0,     0,   958,   959,    73,   362,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   964,
     965,     0,     0,     0,    74,     0,     0,     0,    73,     0,
       0,     0,     0,     0,   364,   981,     0,     0,     0,     0,
     365,   366,   367,   368,     0,   983,   984,   416,   417,    73,
       0,     0,   672,     0,    73,   121,     0,    73,     0,     0,
     419,     0,     0,     0,     0,     0,     0,     0,   369,     0,
       0,   370,     0,     0,     0,   672,     0,     0,     0,     0,
       0,     0,     0,   309,     0,   423,   424,   425,   426,   427,
     428,   429,   430,     0,     0,     0,     0,    73,    73,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,    74,
      74,     0,     0,     0,    73,     0,     0,    74,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,   122,     0,
       0,     0,     0,    73,     0,     0,     0,  -621,  -621,  -621,
    -621,   408,   409,    73,     0,  -621,  -621,     0,     0,     0,
       0,     0,     0,   416,   417,     0,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,   419,     0,    74,     0,
       0,     0,     0,   267,     0,     0,    73,     0,     0,     0,
       0,     0,     0,     0,     0,    73,     0,     0,    74,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,   121,
       0,   121,     0,     0,     0,     0,     0,     0,     0,    74,
    -620,    73,     0,     0,    74,   122,     0,    74,     0,     0,
       0,     0,     0,  -620,  -620,  -620,  -620,  -620,  -620,     0,
    -620,     0,     0,     0,     0,     0,  -620,  -620,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -620,  -620,     0,
    -620,  -620,  -620,  -620,  -620,     0,     0,    74,    74,     0,
       0,     0,     0,     0,     0,     0,   121,     0,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,    74,     0,     0,     0,     0,     0,     0,
    -620,     0,     0,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -620,   103,     0,   103,   128,
     128,     0,     0,     0,     0,  -620,     0,   232,  -620,  -620,
       0,     0,     0,     0,    73,     0,    74,     0,     0,     0,
       0,     0,     0,     0,     0,    74,     0,     0,  -620,  -620,
       0,     0,     0,     0,   276,  -620,  -620,  -620,  -620,   122,
       0,   122,     0,     0,     0,     0,   103,     0,     0,     0,
     318,    74,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,   415,     0,   318,     0,     0,   416,
     417,     0,     0,     0,   418,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   122,     0,     0,     0,
       0,     0,     0,   420,   103,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,    73,     0,     0,     0,     0,
       0,     0,   121,    73,    73,     0,     0,     0,     0,     0,
      73,     0,     0,     0,     0,     0,    73,    73,     0,     0,
       0,     0,    73,    73,     0,     0,   100,     0,   100,   127,
     127,   127,     0,     0,     0,     0,    73,   231,     0,     0,
       0,     0,     0,     0,    74,     0,     0,     0,    73,    73,
       0,     0,     0,     0,     0,     0,    73,   404,   405,   406,
     407,   408,   409,   410,     0,   412,   413,    73,    73,     0,
       0,     0,     0,   416,   417,   103,   100,     0,     0,     0,
     317,     0,     0,     0,     0,     0,   419,     0,     0,     0,
       0,     0,     0,     0,   121,     0,     0,     0,     0,   121,
       0,     0,     0,     0,     0,     0,   317,     0,     0,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,     0,
       0,     0,    73,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    73,    73,     0,     0,   121,     0,     0,     0,
      73,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,     0,     0,   103,     0,    74,     0,     0,     0,     0,
     103,   103,   122,    74,    74,     0,     0,     0,   103,     0,
      74,     0,     0,     0,     0,     0,    74,    74,     0,   318,
       0,     0,    74,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    74,     0,     0,     0,
      73,     0,    73,     0,     0,    73,     0,     0,    74,    74,
       0,     0,     0,     0,   103,     0,    74,     0,     0,   103,
       0,     0,     0,     0,     0,     0,     0,    74,    74,     0,
       0,     0,     0,     0,     0,   100,     0,     0,     0,   103,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   122,     0,     0,     0,     0,   122,
     103,     0,     0,     0,     0,   103,   318,     0,   623,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    74,    74,     0,     0,   122,     0,     0,    83,
      74,    83,     0,     0,     0,     0,     0,     0,   623,   623,
     228,     0,     0,   100,     0,     0,     0,     0,     0,     0,
     100,   100,     0,     0,     0,   103,     0,     0,   100,     0,
       0,     0,     0,     0,     0,     0,   103,     0,     0,   317,
       0,     0,     0,     0,   103,     0,     0,     0,     0,    83,
       0,     0,     0,     0,   103,     0,     0,     0,     0,     0,
      74,     0,    74,     0,     0,    74,     0,     0,     0,     0,
       0,     0,     0,     0,   100,     0,     0,     0,     0,   100,
       0,     0,     0,     0,     0,     0,     0,   103,     0,     0,
       0,     0,     0,     0,     0,     0,   103,     0,     0,   100,
       0,     0,     0,     0,     0,     0,   801,     0,     0,     0,
     318,     0,   318,     0,     0,     0,     0,    83,     0,     0,
     100,     0,   103,     0,     0,   100,   317,     0,     0,     0,
       0,     0,     0,     0,   404,   405,   406,   407,   408,   409,
     410,   411,   412,   413,   414,   415,     0,     0,     0,     0,
     416,   417,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   318,     0,     0,
     101,     0,   101,     0,   420,   100,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   100,     0,     0,     0,
       0,     0,     0,     0,   100,     0,     0,     0,    83,     0,
       0,     0,     0,     0,   100,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,   651,   652,
       0,     0,   653,     0,     0,   103,     0,   100,     0,     0,
       0,     0,     0,     0,     0,     0,   100,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
     317,     0,   317,   184,   185,   186,   187,     0,     0,     0,
       0,     0,   100,     0,     0,     0,    83,   188,   189,   190,
       0,     0,     0,    83,    83,     0,     0,     0,   101,     0,
       0,    83,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,     0,     0,     0,     0,     0,     0,
     203,   276,     0,     0,     0,     0,     0,   317,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    83,     0,     0,
       0,     0,    83,     0,     0,     0,   103,     0,     0,     0,
       0,     0,     0,   318,   103,   623,     0,     0,     0,     0,
       0,   623,    83,     0,     0,     0,     0,   623,   623,     0,
       0,     0,     0,   103,   103,     0,     0,     0,     0,   101,
       0,     0,     0,    83,     0,     0,     0,   103,    83,     0,
       0,   618,     0,     0,     0,   100,     0,     0,     0,   103,
     103,     0,     0,     0,     0,     0,     0,   103,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   103,   103,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   618,   618,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   128,     0,     0,    83,     0,
     128,     0,     0,     0,     0,     0,     0,   101,     0,    83,
       0,     0,     0,     0,   101,   101,     0,    83,     0,     0,
       0,     0,   101,   623,     0,     0,     0,    83,     0,     0,
       0,     0,     0,   103,   103,     0,     0,   995,     0,     0,
       0,   103,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   100,     0,     0,     0,
      83,     0,     0,   317,   100,     0,     0,     0,   101,    83,
       0,     0,     0,   101,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   100,   100,     0,     0,     0,     0,     0,
       0,     0,     0,   101,     0,    83,     0,   100,     0,     0,
       0,   103,     0,   103,     0,     0,   103,     0,     0,   100,
     100,     0,     0,     0,   101,     0,     0,   100,     0,   101,
       0,     0,   101,     0,     0,     0,     0,     0,   100,   100,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   801,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   127,     0,     0,     0,     0,
     127,     0,   101,   101,     0,     0,     0,   404,   405,   406,
     407,   408,   409,   410,   411,   412,   413,   414,   415,   101,
       0,     0,     0,   416,   417,     0,     0,     0,     0,     0,
     101,     0,     0,   100,   100,     0,   419,   994,   101,     0,
       0,   100,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,     0,     0,     0,     0,   420,    83,   421,
     422,   423,   424,   425,   426,   427,   428,   429,   430,     0,
     404,   405,   406,   407,   408,   409,     0,  -277,   412,   413,
       0,   101,     0,     0,     0,     0,   416,   417,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,   100,     0,   100,     0,     0,   100,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   101,     0,     0,     0,
       0,     0,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   430,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    83,
       0,     0,     0,     0,     0,     0,     0,    83,   618,     0,
       0,     0,     0,     0,   618,     0,     0,     0,     0,     0,
     618,   618,     0,     0,  -597,     0,    83,    83,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -597,  -597,  -597,
      83,  -597,  -597,     0,  -597,     0,     0,     0,     0,     0,
    -597,     0,    83,    83,     0,     0,     0,     0,     0,     0,
      83,  -597,  -597,     0,  -597,  -597,  -597,  -597,  -597,   101,
       0,    83,    83,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,     0,     0,
       0,     0,  -597,  -597,  -597,     0,   806,  -597,     0,     0,
       0,     0,     0,     0,     0,  -597,   618,     0,     0,  -597,
       0,     0,     0,     0,     0,     0,    83,    83,     0,  -597,
     992,     0,  -597,  -597,    83,  -107,  -597,     0,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,     0,     0,
       0,     0,  -597,  -597,  -597,     0,   -99,     0,     0,  -597,
    -597,  -597,  -597,     0,     0,     0,     0,     0,     0,     0,
     101,     0,     0,     0,     0,     0,     0,     0,   101,   101,
       0,     0,     0,   660,   661,   101,     0,   662,     0,     0,
       0,   101,   101,     0,    83,     0,    83,   101,   101,    83,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,   101,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,   101,   101,     0,     0,     0,     0,     0,
       0,   101,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,   101,   101,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,     0,
       0,     0,     0,     0,     0,   203,   276,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   101,   101,     0,
       0,     0,     0,     0,     0,   101,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -620,     3,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,     0,     0,     0,     0,     0,    14,     0,    15,    16,
      17,    18,     0,     0,     0,     0,     0,    19,    20,    21,
      22,    23,    24,    25,     0,   101,    26,   101,     0,     0,
     101,     0,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,    50,     0,
       0,    51,    52,     0,    53,    54,     0,    55,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
    -293,    63,  -620,     0,     0,  -620,  -620,     0,     0,     0,
       0,     0,     0,  -293,  -293,  -293,  -293,  -293,  -293,     0,
    -293,    64,    65,    66,     0,     0,     0,  -293,  -293,  -293,
       0,     0,     0,  -620,     0,  -620,     0,  -293,  -293,     0,
    -293,  -293,  -293,  -293,  -293,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -293,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,
    -293,  -293,  -293,  -293,     0,     0,     0,     0,  -293,  -293,
    -293,     0,     0,  -293,     0,     0,     0,     0,     0,  -293,
       0,  -293,     0,     0,     0,  -293,     0,     0,     0,     0,
       0,     0,     0,  -293,     0,  -293,     0,     0,  -293,  -293,
       0,     0,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,
    -293,  -293,  -293,  -293,     0,     0,  -481,     0,     0,  -293,
    -293,  -293,  -293,     0,     0,  -293,  -293,  -293,  -293,  -481,
    -481,  -481,  -481,  -481,  -481,     0,  -481,     0,     0,     0,
       0,     0,     0,  -481,  -481,     0,     0,     0,     0,     0,
       0,     0,     0,  -481,  -481,     0,  -481,  -481,  -481,  -481,
    -481,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   496,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -481,  -481,
    -481,  -481,  -481,  -481,  -481,  -481,  -481,  -481,  -481,  -481,
       0,     0,     0,     0,  -481,  -481,  -481,     0,  -481,  -481,
       0,     0,     0,     0,     0,  -481,     0,  -481,     0,     0,
       0,  -481,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -481,     0,     0,  -481,  -481,     0,  -481,  -481,     0,
    -481,  -481,  -481,  -481,  -481,  -481,  -481,  -481,  -481,  -481,
       0,     0,  -620,     0,     0,  -481,  -481,  -481,  -481,     0,
       0,  -481,  -481,  -481,  -481,  -620,  -620,  -620,  -620,  -620,
    -620,     0,  -620,     0,     0,     0,     0,     0,  -620,  -620,
    -620,     0,     0,     0,     0,     0,     0,     0,     0,  -620,
    -620,     0,  -620,  -620,  -620,  -620,  -620,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,     0,     0,     0,     0,
    -620,  -620,  -620,     0,     0,  -620,     0,     0,     0,     0,
       0,  -620,     0,  -620,     0,     0,     0,  -620,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -620,     0,     0,
    -620,  -620,     0,     0,  -620,     0,  -620,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,     0,     0,  -620,     0,
    -620,  -620,  -620,  -620,  -620,     0,   276,  -620,  -620,  -620,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,     0,  -620,     0,
       0,     0,     0,     0,     0,  -620,  -620,     0,     0,     0,
       0,     0,     0,     0,     0,  -620,  -620,     0,  -620,  -620,
    -620,  -620,  -620,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,     0,     0,     0,     0,  -620,  -620,  -620,     0,
       0,  -620,     0,     0,     0,     0,     0,  -620,     0,  -620,
       0,     0,     0,  -620,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -620,     0,     0,  -620,  -620,     0,     0,
    -620,     0,  -620,  -620,  -620,  -620,  -620,  -620,  -620,  -620,
    -620,  -620,     0,     0,  -597,     0,     0,  -620,  -620,  -620,
    -620,     0,   276,  -620,  -620,  -620,  -620,  -597,  -597,  -597,
       0,  -597,  -597,     0,  -597,     0,     0,     0,     0,     0,
    -597,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -597,  -597,     0,  -597,  -597,  -597,  -597,  -597,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -597,  -597,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,     0,     0,
       0,     0,  -597,  -597,  -597,     0,   806,  -597,     0,     0,
       0,     0,     0,     0,     0,  -597,     0,     0,     0,  -597,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -597,
       0,     0,  -597,  -597,     0,  -107,  -597,     0,  -597,  -597,
    -597,  -597,  -597,  -597,  -597,  -597,  -597,  -597,     0,     0,
    -302,     0,  -597,  -597,  -597,     0,  -597,     0,     0,  -597,
    -597,  -597,  -597,  -302,  -302,  -302,     0,  -302,  -302,     0,
    -302,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -302,  -302,     0,
    -302,  -302,  -302,  -302,  -302,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,     0,     0,     0,     0,  -302,  -302,
    -302,     0,   807,  -302,     0,     0,     0,     0,     0,     0,
       0,  -302,     0,     0,     0,  -302,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -302,     0,     0,  -302,  -302,
       0,  -109,  -302,     0,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,     0,     0,  -302,     0,     0,  -302,
    -302,     0,  -101,     0,     0,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,     0,  -302,  -302,     0,  -302,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -302,  -302,     0,  -302,  -302,  -302,  -302,
    -302,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
       0,     0,     0,     0,  -302,  -302,  -302,     0,   807,  -302,
       0,     0,     0,     0,     0,     0,     0,  -302,     0,     0,
       0,  -302,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -302,     0,     0,  -302,  -302,     0,  -109,  -302,     0,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
       0,     0,     0,     0,     0,  -302,  -302,     0,  -302,     0,
       0,  -302,  -302,  -302,  -302,   295,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,  -620,  -620,  -620,
       0,     0,  -620,    14,     0,    15,    16,    17,    18,     0,
       0,     0,     0,     0,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,    27,
       0,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,  -620,
       0,     0,  -620,  -620,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,    65,
      66,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -620,   295,  -620,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,  -620,     0,  -620,  -620,    14,
       0,    15,    16,    17,    18,     0,     0,     0,     0,     0,
      19,    20,    21,    22,    23,    24,    25,     0,     0,    26,
       0,     0,     0,     0,     0,    27,     0,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,  -620,     0,     0,  -620,  -620,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    64,    65,    66,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -620,   295,  -620,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,  -620,     0,     0,  -620,    14,  -620,    15,    16,    17,
      18,     0,     0,     0,     0,     0,    19,    20,    21,    22,
      23,    24,    25,     0,     0,    26,     0,     0,     0,     0,
       0,    27,     0,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,    50,     0,     0,
      51,    52,     0,    53,    54,     0,    55,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,  -620,     0,     0,  -620,  -620,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      64,    65,    66,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -620,   295,  -620,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,  -620,     0,     0,
    -620,    14,     0,    15,    16,    17,    18,     0,     0,     0,
       0,     0,    19,    20,    21,    22,    23,    24,    25,     0,
       0,    26,     0,     0,     0,     0,     0,    27,     0,    29,
      30,    31,    32,    33,    34,    35,    36,    37,    38,     0,
      39,    40,    41,     0,     0,    42,     0,     0,    43,    44,
       0,    45,    46,    47,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    48,    49,     0,     0,
       0,     0,     0,    50,     0,     0,    51,    52,     0,    53,
      54,     0,    55,     0,     0,     0,    56,     0,    57,    58,
      59,     0,    60,    61,    62,     0,    63,  -620,     0,     0,
    -620,  -620,     3,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,     0,     0,    64,    65,    66,     0,
      14,     0,    15,    16,    17,    18,     0,     0,  -620,     0,
    -620,    19,    20,    21,    22,    23,    24,    25,     0,     0,
      26,     0,     0,     0,     0,     0,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
      45,    46,    47,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    48,    49,     0,     0,     0,
       0,     0,    50,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,  -620,     0,     0,  -620,
    -620,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    64,    65,    66,     0,     0,
    -620,     0,     0,     0,     0,     0,     0,  -620,   295,  -620,
       4,     5,     6,     7,     8,     9,    10,    11,    12,    13,
       0,  -620,  -620,     0,     0,     0,    14,     0,    15,    16,
      17,    18,     0,     0,     0,     0,     0,    19,    20,    21,
      22,    23,    24,    25,     0,     0,    26,     0,     0,     0,
       0,     0,    27,     0,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,    50,     0,
       0,    51,    52,     0,    53,    54,     0,    55,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,  -620,     0,     0,  -620,  -620,   295,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,    64,    65,    66,     0,    14,     0,    15,    16,    17,
      18,     0,     0,  -620,     0,  -620,    19,    20,    21,    22,
      23,    24,    25,     0,     0,    26,     0,     0,     0,     0,
       0,    27,     0,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,    50,     0,     0,
     296,    52,     0,    53,    54,     0,    55,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,  -620,     0,     0,  -620,  -620,   295,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,     0,     0,
      64,    65,    66,     0,    14,     0,    15,    16,    17,    18,
       0,  -620,  -620,     0,  -620,    19,    20,    21,    22,    23,
      24,    25,     0,     0,    26,     0,     0,     0,     0,     0,
      27,     0,    29,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,    50,     0,     0,    51,
      52,     0,    53,    54,     0,    55,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
    -620,     0,     0,  -620,  -620,   295,     0,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,    64,
      65,    66,     0,    14,     0,    15,    16,    17,    18,     0,
    -620,  -620,     0,  -620,    19,    20,    21,    22,    23,    24,
      25,     0,     0,    26,     0,     0,     0,     0,     0,    27,
       0,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,     0,    39,    40,    41,     0,     0,    42,     0,     0,
      43,    44,     0,    45,    46,    47,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    48,    49,
       0,     0,     0,     0,     0,    50,     0,     0,    51,    52,
       0,    53,    54,     0,    55,     0,     0,     0,    56,     0,
      57,    58,    59,     0,    60,    61,    62,     0,    63,  -620,
       0,     0,  -620,  -620,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    64,    65,
      66,     0,     0,  -620,     0,     0,     0,     0,     0,     0,
    -620,   295,  -620,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,  -620,     0,     0,     0,    14,
       0,    15,    16,    17,    18,     0,     0,     0,     0,     0,
      19,    20,    21,    22,    23,    24,    25,     0,     0,    26,
       0,     0,     0,     0,     0,    27,     0,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,     0,    39,    40,
      41,     0,     0,    42,     0,     0,    43,    44,     0,    45,
      46,    47,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    48,    49,     0,     0,     0,     0,
       0,    50,     0,     0,    51,    52,     0,    53,    54,     0,
      55,     0,     0,     0,    56,     0,    57,    58,    59,     0,
      60,    61,    62,     0,    63,  -620,     0,     0,  -620,  -620,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,     0,     0,    64,    65,    66,     0,    14,     0,
      15,    16,    17,    18,     0,     0,  -620,     0,  -620,    19,
      20,    21,    22,    23,    24,    25,     0,     0,    26,     0,
       0,     0,     0,     0,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
      50,     0,     0,    51,    52,     0,    53,    54,     0,    55,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,   247,     0,     0,   248,   249,     0,
       0,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,    64,    65,    66,     0,    14,     0,    15,
      16,    17,    18,     0,     0,   250,     0,   251,    19,    20,
      21,    22,    23,    24,    25,     0,     0,    26,     0,     0,
       0,     0,     0,    27,     0,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,     0,    39,    40,    41,     0,
       0,    42,     0,     0,    43,    44,     0,    45,    46,    47,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    48,    49,     0,     0,     0,     0,     0,    50,
       0,     0,    51,    52,     0,    53,    54,     0,    55,     0,
       0,     0,    56,     0,    57,    58,    59,     0,    60,    61,
      62,     0,    63,   247,     0,     0,   248,   249,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,    64,    65,    66,     0,    14,     0,    15,    16,
      17,    18,     0,     0,   250,     0,   251,    19,    20,    21,
      22,    23,    24,    25,     0,     0,    26,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,    45,    46,    47,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    48,    49,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,   247,     0,     0,   248,   249,     0,     0,     4,
       5,     6,     7,     8,     9,    10,    11,    12,     0,     0,
       0,    64,    65,    66,     0,    14,     0,   108,   109,    17,
      18,     0,     0,   250,     0,   251,   110,   111,   112,    22,
      23,    24,    25,     0,     0,   113,     0,     0,     0,     0,
       0,     0,     0,     0,    30,    31,    32,    33,    34,    35,
      36,    37,    38,     0,    39,    40,    41,     0,     0,    42,
       0,     0,    43,    44,     0,    45,    46,    47,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      48,    49,     0,     0,     0,     0,     0,   211,     0,     0,
     119,    52,     0,    53,    54,     0,     0,     0,     0,     0,
      56,     0,    57,    58,    59,     0,    60,    61,    62,     0,
      63,   247,     0,     0,   248,   249,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
      64,   265,    66,     0,    14,     0,    15,    16,    17,    18,
       0,     0,   250,     0,   251,    19,    20,    21,    22,    23,
      24,    25,     0,     0,    26,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,    45,    46,    47,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    48,
      49,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      52,     0,    53,    54,     0,     0,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
     247,     0,     0,   248,   249,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    64,
      65,    66,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   251,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,     0,     0,
       0,   155,   156,   157,   158,   159,   160,   161,   162,   163,
     164,     0,     0,     0,     0,     0,   165,   166,   167,   168,
     169,   170,   171,   172,    35,    36,   173,    38,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     116,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,     0,     0,
       0,     0,     0,     0,   203,   204,  -590,  -590,  -590,  -590,
    -590,  -590,  -590,  -590,  -590,     0,     0,     0,     0,     0,
       0,     0,  -590,     0,  -590,  -590,  -590,  -590,     0,  -590,
       0,     0,     0,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
       0,     0,  -590,     0,     0,     0,     0,     0,     0,     0,
       0,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,  -590,
       0,  -590,  -590,  -590,     0,     0,  -590,     0,     0,  -590,
    -590,     0,  -590,  -590,  -590,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -590,  -590,     0,
       0,     0,     0,     0,  -590,     0,     0,  -590,  -590,     0,
    -590,  -590,     0,  -590,     0,  -590,  -590,  -590,     0,  -590,
    -590,  -590,     0,  -590,  -590,  -590,     0,  -590,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -590,  -590,  -590,
       0,  -590,     0,     0,     0,     0,     0,  -590,  -591,  -591,
    -591,  -591,  -591,  -591,  -591,  -591,  -591,     0,     0,     0,
       0,     0,     0,     0,  -591,     0,  -591,  -591,  -591,  -591,
       0,  -591,     0,     0,     0,  -591,  -591,  -591,  -591,  -591,
    -591,  -591,     0,     0,  -591,     0,     0,     0,     0,     0,
       0,     0,     0,  -591,  -591,  -591,  -591,  -591,  -591,  -591,
    -591,  -591,     0,  -591,  -591,  -591,     0,     0,  -591,     0,
       0,  -591,  -591,     0,  -591,  -591,  -591,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -591,
    -591,     0,     0,     0,     0,     0,  -591,     0,     0,  -591,
    -591,     0,  -591,  -591,     0,  -591,     0,  -591,  -591,  -591,
       0,  -591,  -591,  -591,     0,  -591,  -591,  -591,     0,  -591,
       0,     0,     0,     0,     0,     0,  -593,  -593,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,     0,     0,     0,     0,  -591,
    -591,  -591,  -593,  -591,  -593,  -593,  -593,  -593,     0,  -591,
       0,     0,     0,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
       0,     0,  -593,     0,     0,     0,     0,     0,     0,     0,
       0,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
       0,  -593,  -593,  -593,     0,     0,  -593,     0,     0,  -593,
    -593,     0,  -593,  -593,  -593,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -593,  -593,     0,
       0,     0,     0,     0,  -593,   838,     0,  -593,  -593,     0,
    -593,  -593,     0,  -593,     0,  -593,  -593,  -593,     0,  -593,
    -593,  -593,     0,  -593,  -593,  -593,     0,  -593,     0,     0,
       0,     0,     0,     0,  -107,  -594,  -594,  -594,  -594,  -594,
    -594,  -594,  -594,  -594,     0,     0,     0,  -593,  -593,  -593,
       0,  -594,     0,  -594,  -594,  -594,  -594,  -593,     0,     0,
       0,     0,  -594,  -594,  -594,  -594,  -594,  -594,  -594,     0,
       0,  -594,     0,     0,     0,     0,     0,     0,     0,     0,
    -594,  -594,  -594,  -594,  -594,  -594,  -594,  -594,  -594,     0,
    -594,  -594,  -594,     0,     0,  -594,     0,     0,  -594,  -594,
       0,  -594,  -594,  -594,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -594,  -594,     0,     0,
       0,     0,     0,  -594,   839,     0,  -594,  -594,     0,  -594,
    -594,     0,  -594,     0,  -594,  -594,  -594,     0,  -594,  -594,
    -594,     0,  -594,  -594,  -594,     0,  -594,     0,     0,     0,
       0,     0,     0,  -109,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,     0,     0,     0,  -594,  -594,  -594,     0,
    -595,     0,  -595,  -595,  -595,  -595,  -594,     0,     0,     0,
       0,  -595,  -595,  -595,  -595,  -595,  -595,  -595,     0,     0,
    -595,     0,     0,     0,     0,     0,     0,     0,     0,  -595,
    -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,     0,  -595,
    -595,  -595,     0,     0,  -595,     0,     0,  -595,  -595,     0,
    -595,  -595,  -595,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -595,  -595,     0,     0,     0,
       0,     0,  -595,     0,     0,  -595,  -595,     0,  -595,  -595,
       0,  -595,     0,  -595,  -595,  -595,     0,  -595,  -595,  -595,
       0,  -595,  -595,  -595,     0,  -595,     0,     0,     0,     0,
       0,     0,  -596,  -596,  -596,  -596,  -596,  -596,  -596,  -596,
    -596,     0,     0,     0,     0,  -595,  -595,  -595,  -596,     0,
    -596,  -596,  -596,  -596,     0,  -595,     0,     0,     0,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,     0,     0,  -596,     0,
       0,     0,     0,     0,     0,     0,     0,  -596,  -596,  -596,
    -596,  -596,  -596,  -596,  -596,  -596,     0,  -596,  -596,  -596,
       0,     0,  -596,     0,     0,  -596,  -596,     0,  -596,  -596,
    -596,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -596,  -596,     0,     0,     0,     0,     0,
    -596,     0,     0,  -596,  -596,     0,  -596,  -596,     0,  -596,
       0,  -596,  -596,  -596,     0,  -596,  -596,  -596,     0,  -596,
    -596,  -596,     0,  -596,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -596,  -596,  -596,     0,     0,     0,     0,
       0,     0,     0,  -596,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,     0,     0,
       0,   155,   156,   157,   233,   234,   235,   236,   162,   163,
     164,     0,     0,     0,     0,     0,   165,   166,   167,   237,
     238,   239,   240,   172,   320,   321,   241,   322,     0,     0,
       0,     0,     0,     0,   323,     0,     0,     0,     0,     0,
     324,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,   325,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,     0,     0,
       0,     0,     0,     0,   203,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,     0,
       0,     0,   155,   156,   157,   233,   234,   235,   236,   162,
     163,   164,     0,     0,     0,     0,     0,   165,   166,   167,
     237,   238,   239,   240,   172,   320,   321,   241,   322,     0,
       0,     0,     0,     0,     0,   323,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,   487,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,     0,
       0,     0,     0,     0,     0,   203,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
       0,     0,     0,   155,   156,   157,   233,   234,   235,   236,
     162,   163,   164,     0,     0,     0,     0,     0,   165,   166,
     167,   237,   238,   239,   240,   172,     0,     0,   241,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,   242,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
       0,     0,     0,     0,     0,     0,   203,   131,   132,   133,
     134,   135,   136,   137,   138,   139,   140,   141,   142,   143,
     144,   145,   146,   147,   148,   149,   150,   151,   152,   153,
     154,     0,     0,     0,   155,   156,   157,   233,   234,   235,
     236,   162,   163,   164,     0,     0,     0,     0,     0,   165,
     166,   167,   237,   238,   239,   240,   172,     0,     0,   241,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   174,   175,   176,   177,   178,   179,
     180,   181,     0,     0,   182,   183,     0,     0,     0,     0,
     184,   185,   186,   187,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   188,   189,   190,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   191,   192,
     193,   194,   195,   196,   197,   198,   199,   200,     0,   201,
     202,     0,     0,     0,     0,     0,     0,   203,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,     0,     0,     0,    14,     0,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,   116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   313,     0,     0,   119,
      52,     0,    53,    54,     0,     0,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,     0,     0,     0,    14,   120,
     108,   109,    17,    18,     0,     0,     0,   314,     0,   110,
     111,   112,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,   116,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     313,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,     0,
       0,     0,    14,   120,    15,    16,    17,    18,     0,     0,
       0,   612,     0,    19,    20,    21,    22,    23,    24,    25,
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
      52,     0,    53,    54,     0,   724,     0,   263,   264,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     0,     0,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,    64,
     265,    66,    14,     0,   108,   109,    17,    18,     0,     0,
       0,     0,     0,   110,   111,   112,    22,    23,    24,    25,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,    32,   260,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,   261,    46,    47,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    48,   855,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    52,     0,
      53,    54,     0,   724,     0,   263,   264,    56,     0,    57,
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
       0,   119,    52,     0,    53,    54,     0,   724,     0,   263,
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
       0,   119,    52,     0,    53,    54,     0,   901,     0,     0,
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
      52,     0,    53,    54,     0,   724,     0,     0,     0,    56,
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
       0,     0,    42,     0,     0,    43,    44,     0,    45,    46,
      47,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    48,    49,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     0,     0,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,    64,   265,    66,    14,     0,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,   114,    34,
      35,    36,   115,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   117,     0,     0,   118,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,     0,     0,     0,     0,
      14,   120,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
     225,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   226,     0,     0,    51,    52,     0,    53,    54,
       0,    55,     0,     0,     0,    56,     0,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     4,     5,
       6,     7,     8,     9,    10,    11,    12,     0,     0,     0,
       0,     0,     0,     0,    14,   120,   108,   109,    17,    18,
       0,     0,     0,     0,     0,   110,   111,   112,    22,    23,
      24,    25,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,     0,    39,    40,    41,     0,     0,    42,     0,
       0,    43,    44,     0,   116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   313,     0,     0,   400,
      52,     0,    53,    54,     0,   401,     0,     0,     0,    56,
       0,    57,    58,    59,     0,    60,    61,    62,     0,    63,
       0,     0,     4,     5,     6,     7,     8,     9,    10,    11,
      12,     0,     0,     0,     0,     0,     0,     0,    14,   120,
     108,   109,    17,    18,     0,     0,     0,     0,     0,   110,
     111,   112,    22,    23,    24,    25,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    30,    31,    32,
     114,    34,    35,    36,   115,    38,     0,    39,    40,    41,
       0,     0,    42,     0,     0,    43,    44,     0,   116,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     118,     0,     0,   119,    52,     0,    53,    54,     0,     0,
       0,     0,     0,    56,     0,    57,    58,    59,     0,    60,
      61,    62,     0,    63,     0,     0,     4,     5,     6,     7,
       8,     9,    10,    11,    12,     0,     0,     0,     0,     0,
       0,     0,    14,   120,   108,   109,    17,    18,     0,     0,
       0,     0,     0,   110,   111,   112,    22,    23,    24,    25,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
       0,    39,    40,    41,     0,     0,    42,     0,     0,    43,
      44,     0,   116,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   313,     0,     0,   400,    52,     0,
      53,    54,     0,     0,     0,     0,     0,    56,     0,    57,
      58,    59,     0,    60,    61,    62,     0,    63,     0,     0,
       4,     5,     6,     7,     8,     9,    10,    11,    12,     0,
       0,     0,     0,     0,     0,     0,    14,   120,   108,   109,
      17,    18,     0,     0,     0,     0,     0,   110,   111,   112,
      22,    23,    24,    25,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,     0,    39,    40,    41,     0,     0,
      42,     0,     0,    43,    44,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   968,     0,
       0,   119,    52,     0,    53,    54,     0,     0,     0,     0,
       0,    56,     0,    57,    58,    59,     0,    60,    61,    62,
       0,    63,     0,     0,     4,     5,     6,     7,     8,     9,
      10,    11,    12,     0,     0,     0,     0,     0,     0,     0,
      14,   120,   108,   109,    17,    18,     0,     0,     0,     0,
       0,   110,   111,   112,    22,    23,    24,    25,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,     0,    39,
      40,    41,     0,     0,    42,     0,     0,    43,    44,     0,
     225,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   991,     0,     0,   119,    52,     0,    53,    54,
       0,     0,   681,   652,     0,    56,   682,    57,    58,    59,
       0,    60,    61,    62,     0,    63,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,   120,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   666,   661,
       0,     0,   667,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   698,   652,     0,     0,   699,     0,
     203,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
     701,   661,     0,     0,   702,     0,   203,   276,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   708,   652,     0,     0,
     709,     0,   203,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,   711,   661,     0,     0,   712,     0,   203,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   747,   652,
       0,     0,   748,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   750,   661,     0,     0,   751,     0,
     203,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
     906,   652,     0,     0,   907,     0,   203,   276,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   909,   661,     0,     0,
     910,     0,   203,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,  1050,   652,     0,     0,  1051,     0,   203,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,  1062,   652,
       0,     0,  1063,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,  1065,   661,     0,     0,  1066,     0,
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
       0,     0,     0,     0,     0,   873,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,     0,     0,     0,     0,
       0,     0,   203,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,   415,     0,     0,     0,     0,   416,
     417,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,   886,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   420,     0,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,   415,     0,     0,     0,
       0,   416,   417,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   420,     0,   421,   422,   423,
     424,   425,   426,   427,   428,   429,   430,   404,   405,   406,
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
       0,     0,   501,     0,     0,     0,     0,     0,     0,     0,
     419,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   420,     0,   421,   422,   423,   424,   425,   426,   427,
     428,   429,   430,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,   415,     0,     0,     0,     0,   416,
     417,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   420,     0,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,  -621,  -621,     0,     0,     0,
       0,   416,   417,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   421,   422,   423,
     424,   425,   426,   427,   428,   429,   430
};

static const yytype_int16 yycheck[] =
{
       1,    88,    13,    15,    16,   285,    26,    19,    55,    27,
       1,     6,     3,     4,     5,    65,    27,     8,     9,    20,
     490,    12,    68,    14,    15,    16,    86,    87,    19,   314,
       9,    21,     4,     5,   222,    14,   379,   481,    14,   790,
      12,   272,     8,     9,     6,   403,     3,     1,    14,     3,
     597,    13,    53,    54,    81,   332,   762,   307,   594,   118,
      51,   311,    57,   508,    55,    27,    51,    15,    16,   692,
     547,    19,    73,    74,    65,   374,    20,    21,   431,    26,
     703,   594,   435,    55,   504,   438,   669,   670,   508,    25,
      81,   296,    68,   319,   539,    57,   395,    16,    25,    74,
      59,    60,    61,    62,    25,    53,   459,   976,   433,    81,
     111,    15,    16,    90,   104,    19,    15,    16,    15,    16,
      19,   474,    19,   476,    26,    57,   117,   951,   119,    25,
       0,    25,   485,   458,    74,   612,    25,    90,   443,   444,
     103,    90,    29,   121,    90,   371,    25,   122,    58,    59,
     475,   117,   105,    25,    53,    54,    53,    54,   218,   484,
     104,   460,    79,   144,   142,   128,    28,   111,   112,   229,
     147,    51,   525,   333,   138,    55,   336,    90,   338,    57,
     340,    92,   342,    90,  1053,   129,    18,    26,    20,   121,
     121,   138,    90,    90,   147,   400,   115,   550,   147,   118,
     119,   147,    90,   388,    16,   390,   142,    55,   144,   126,
     121,    92,   213,   214,    92,   142,   142,   144,   754,   210,
    1044,   142,   973,   142,   549,   976,   113,   146,   121,   148,
     290,   767,   223,   224,   147,    90,   138,   820,   403,   214,
     147,   754,   144,   121,   223,   224,   142,   242,   142,   147,
     956,    90,   298,   142,   767,   121,   316,   223,   224,   147,
     306,   307,   547,   142,   276,   311,   105,   279,   144,   270,
     142,   272,   125,   213,   214,   276,   252,    55,   443,   444,
     242,   558,   513,    90,   270,   276,   272,   314,   279,   734,
      90,    92,   147,   284,   285,    17,    18,   288,    55,   138,
      25,   140,  1053,   115,   295,   296,   118,   119,   147,   142,
      92,   296,   303,   733,   734,    16,    92,    92,   284,   285,
     306,   798,   298,   314,   947,   551,   809,   612,   276,   876,
      20,   279,   815,   691,   146,    27,   148,   142,   101,   121,
     147,   295,   314,    37,    38,   121,   121,   147,   398,   303,
      58,    59,    51,   403,   401,    37,    38,   870,   349,   350,
     351,   352,   353,   354,   355,   356,   325,   880,   131,   132,
     133,   350,   351,   352,   353,   279,   377,    57,   379,   276,
     279,   680,   279,   374,   350,   351,   352,   353,   138,   355,
     356,   811,   349,   443,   444,   349,   556,   101,   457,    92,
     354,    92,   115,   847,   395,   118,   119,   398,   145,   400,
     401,    92,   403,   665,   115,   400,   668,   118,   119,    92,
     119,   433,   141,   115,   888,   889,   118,   119,   121,   401,
     121,   774,   433,   115,   686,   148,   118,   119,   121,    92,
     121,   139,   433,   723,    55,   146,   458,   148,   121,   802,
     398,   804,   443,   444,   146,   403,   148,   458,   142,    60,
     973,   142,    63,   475,   146,   466,   148,   458,   121,   460,
     461,    92,   484,   435,   475,   101,   438,   947,   803,   101,
     471,    57,   502,   484,   475,    63,    64,    65,   479,   142,
     121,   435,   791,   484,   744,   461,   727,   459,   489,    92,
     121,   806,   121,   521,   949,   319,   107,   812,   813,   142,
     521,   512,   513,   798,   476,   459,    51,    92,   545,   818,
     547,   522,   142,   485,    92,   805,   512,   513,   121,   949,
     718,   522,   476,  1046,   142,   511,   433,   549,   116,   117,
     531,   485,    92,   587,  1057,   493,   121,   591,   549,   892,
     893,    16,   142,   121,   545,    92,   547,   371,   549,   521,
      51,   458,   622,   525,   863,   142,   557,   142,   522,    51,
      92,   121,   584,   545,   142,   547,    61,   531,   475,    64,
      65,   525,   862,   121,   121,   612,   142,   484,   550,   288,
      51,   603,   142,   584,   779,   780,   781,   296,   783,   121,
     785,     1,    99,     3,   605,   504,   550,   504,     8,     9,
      15,   508,   603,   918,    14,    15,    16,    13,    57,    19,
     640,   612,   121,   609,    16,   856,   121,   443,   444,    63,
     929,   116,   117,   121,   122,   800,    15,   536,   145,   536,
     612,   806,   539,    92,   145,   139,   142,   812,   813,   142,
     115,    51,   549,   118,   119,   142,    51,    15,   659,    15,
      44,   477,   478,   142,   665,    65,   656,   668,   669,   670,
     121,   141,   121,   659,    69,   665,   115,   141,   668,   118,
     119,   146,  1025,   148,   123,   686,    15,  1040,   679,   680,
     691,   692,    18,   142,   689,   696,   714,   141,   744,   141,
     679,   400,   703,   714,   587,    15,   101,   102,   103,   148,
     526,   139,   656,   679,  1039,   139,   141,   117,   139,   119,
     142,   665,   713,   215,   668,    44,   727,   689,   142,   816,
     222,   142,   718,   128,   713,   115,    57,   551,   118,   119,
     684,   727,   686,   691,    90,   933,   790,   713,   142,   142,
     800,   939,   714,   918,    44,    15,   806,   807,    93,   105,
      14,   142,   812,   813,    15,  1045,   580,   259,    15,    26,
     145,   798,   471,   774,   142,   960,   961,   962,   963,   142,
     479,   115,   142,   597,   118,   119,   600,   142,   141,    15,
     489,   803,   300,    15,   140,   139,   304,    15,   144,    26,
     791,   147,   803,    73,    74,    15,    15,   798,   799,   800,
     210,   139,   803,   126,   126,   806,   807,   142,    55,   820,
      15,   812,   813,   223,   224,    90,   798,   818,   819,   830,
     139,    55,   833,    90,   733,   142,   733,   734,   142,   142,
     105,   832,   804,   142,   835,   799,   594,   142,   105,   597,
     142,   121,   122,   844,   845,   856,   835,   349,   557,    90,
     804,   852,   142,    90,  1049,    90,    15,   144,   918,   835,
     856,   363,   863,   864,   105,   140,   276,   144,   105,   279,
     105,   138,   147,   140,   284,   285,   141,   144,   288,   381,
     147,   892,   893,   142,   522,   295,   296,    12,     5,   890,
      26,  1042,  1044,   303,   895,   790,   803,   790,   817,   140,
    1041,   138,   811,   140,   811,   140,   147,   144,   890,     6,
     147,    90,   147,   895,   254,   790,   970,   918,   587,   973,
     970,   973,   976,   993,   978,    -1,   105,   928,   929,    -1,
      -1,   932,    90,   213,   214,   936,   947,    -1,    -1,   349,
     350,   351,   352,   353,   354,   355,   356,   105,    -1,    -1,
     932,    -1,    -1,    51,    90,    53,    54,    55,    56,    -1,
      -1,   140,    -1,    -1,   374,    -1,   790,    -1,   147,   105,
      -1,    69,  1026,    63,    64,    65,  1006,    -1,   480,   481,
     806,   807,   140,    -1,    -1,   395,   812,   813,   398,   147,
     400,   509,    -1,   403,    -1,   996,   754,   998,   516,  1053,
    1001,  1055,   138,  1057,   140,  1059,    -1,    -1,   144,   767,
     528,   147,   838,   839,  1025,   841,   842,  1039,    40,    41,
      42,    43,    44,   433,    -1,  1079,   116,   117,  1039,   531,
    1041,  1042,   790,   443,   444,    -1,   538,    -1,  1039,   319,
      90,    -1,   949,    -1,   142,    -1,    -1,    26,   458,    -1,
     460,   461,   876,    -1,   878,   105,    -1,    -1,   882,    -1,
      -1,   471,    -1,   581,   582,   475,    -1,    90,  1040,   479,
      -1,    -1,    61,    -1,   484,    64,    65,   970,    -1,   489,
     973,    -1,   105,   976,    90,   978,  1040,    90,    -1,    -1,
     140,   371,   918,   611,    -1,   970,    -1,   147,   973,   105,
      -1,   976,   105,   978,    51,    -1,    53,    54,    55,    56,
     819,    90,   522,    -1,    -1,   941,    -1,   140,   876,    -1,
      -1,   531,    69,   832,   147,    -1,   105,   116,   117,    -1,
     954,   955,  1039,  1026,   140,   844,   845,   140,    -1,   549,
      -1,   147,    -1,   852,   147,   897,   898,   557,    90,    -1,
      -1,  1026,   976,    -1,   978,   864,    -1,    -1,    -1,   138,
    1053,   140,  1055,   105,  1057,   144,  1059,    -1,   147,    -1,
     688,    -1,    26,    90,   584,    -1,   678,    -1,  1053,    -1,
    1055,    -1,  1057,    -1,  1059,    -1,  1079,    -1,   105,  1013,
      -1,    -1,  1016,   603,    -1,   142,    90,    -1,   140,    90,
      -1,    -1,     8,     9,  1079,   147,    -1,    90,    14,    15,
      16,   105,    -1,    19,   105,   973,   718,    -1,   976,   928,
      -1,   973,   105,   140,  1048,   743,    -1,   936,    -1,  1053,
     147,  1055,    -1,    -1,   115,  1059,    90,   118,   119,    -1,
      46,    47,    48,    49,    -1,   763,   140,    53,    54,   140,
      -1,   105,    -1,   147,    -1,  1079,   147,   140,    -1,    65,
      66,   142,    -1,    -1,   147,   146,    -1,   148,    -1,   679,
     680,   551,    -1,    62,   776,    64,    65,  1029,  1030,  1031,
      -1,  1033,  1034,    -1,   138,    -1,   140,   996,    -1,   998,
     144,    -1,  1001,   147,    62,  1053,    64,    65,    63,    64,
      65,    -1,    -1,   713,    -1,    63,    64,    65,    63,    64,
      65,   117,    -1,    -1,    51,    -1,    53,    54,    55,    56,
     822,  1073,  1074,  1075,  1076,    -1,    -1,   116,   117,    -1,
      -1,  1083,    69,    -1,    -1,    -1,    51,   855,    53,    54,
      55,    56,    -1,    -1,    -1,   847,    83,    -1,   116,   117,
      -1,   116,   117,   871,    69,    -1,    -1,    94,   116,   117,
      -1,   116,   117,   100,   101,   102,   103,    -1,    83,    -1,
      63,    64,    65,    63,    64,    65,    -1,    -1,    -1,    94,
      -1,   791,    -1,    -1,   121,   100,   101,   102,   103,   799,
     800,   128,    -1,   803,   131,    -1,   806,   807,    63,    64,
      65,    -1,   812,   813,    -1,    -1,    -1,   144,   818,   819,
      -1,    -1,    -1,   128,    -1,    -1,   131,   223,   224,    -1,
      -1,    -1,   832,   116,   117,   835,   116,   117,    -1,   144,
      -1,   933,    -1,    -1,   844,   845,    -1,   939,    -1,    -1,
      -1,    -1,   852,    51,    -1,    53,    54,    55,    56,    -1,
      -1,   116,   117,   863,   864,    -1,   262,   263,   264,   265,
      -1,    69,    51,    -1,    53,    54,    55,    56,    -1,    -1,
     276,    -1,    51,   279,    53,    54,    55,    56,   284,   285,
      69,    -1,    -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,
      69,    -1,   100,   101,   102,   103,    -1,    -1,    51,    -1,
      53,    54,    55,    56,    -1,    94,    -1,    -1,   918,    -1,
      -1,   100,   101,   102,   103,    94,    69,    -1,   928,   929,
     128,   100,    -1,   131,    -1,    51,   936,    53,    54,    55,
      56,   115,    -1,    -1,   118,   119,   144,    -1,    -1,   128,
      -1,    94,   131,    69,   350,   351,   352,   353,   115,   355,
     356,   118,   119,   142,   115,    -1,    -1,   118,   119,    -1,
      -1,   145,   146,    -1,   148,    -1,    -1,    -1,    94,   375,
      -1,    -1,    -1,    -1,   100,    -1,    -1,    -1,    -1,   146,
     386,   148,    -1,    -1,    -1,   146,   996,   148,   998,    -1,
      -1,  1001,   398,    -1,    -1,    -1,    -1,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   413,   414,   415,
     416,   417,   101,   419,   420,   421,   422,   423,   424,   425,
     426,   427,   428,   429,   430,    -1,    -1,   433,    -1,  1039,
      51,    -1,    53,    54,    55,    56,    -1,   443,   444,    -1,
     129,   130,   131,   132,   133,    -1,    -1,    -1,    69,    -1,
      -1,    -1,   458,    -1,    -1,   461,    51,    -1,    53,    54,
      55,    56,    -1,    -1,    85,    -1,    -1,   473,    -1,   475,
      -1,   477,   478,    94,    69,    -1,    -1,    -1,   484,   100,
     101,   102,   103,    -1,    -1,    -1,    -1,   493,    -1,    -1,
     496,   497,    -1,    -1,    -1,   501,    -1,    -1,   504,    -1,
     506,    -1,   508,   509,    -1,    -1,    -1,   128,    -1,    -1,
     131,    -1,    -1,    -1,    -1,     1,    -1,     3,     4,     5,
     526,    -1,    -1,    -1,    -1,    -1,    12,    -1,    -1,    -1,
     536,    -1,    51,   539,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,   549,    -1,    -1,    88,    89,    -1,    -1,
      69,    -1,    -1,    51,    -1,    53,    54,    55,    56,   101,
     566,   567,    -1,    -1,    -1,    51,    85,    -1,    -1,    55,
      -1,    69,    88,    89,    -1,    94,    -1,    -1,   584,    -1,
      -1,   100,   101,   102,   103,   101,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    81,    94,   603,    -1,    -1,
     606,    -1,    -1,   101,   102,   103,    -1,    -1,    -1,   128,
      -1,    -1,   131,   129,   130,   131,   132,   133,    -1,    -1,
      -1,     0,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,   119,    13,    14,    15,    16,    17,    18,
      -1,    20,    -1,    -1,    -1,    -1,    -1,    26,    27,    28,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,
      -1,    40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   679,    -1,     1,    -1,     3,     4,     5,
      -1,    -1,    -1,    -1,    -1,   691,    12,    -1,   694,   695,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,   713,    -1,    88,
      89,    90,    -1,    -1,    93,    -1,    -1,    -1,   724,    -1,
      99,    -1,   101,    -1,   210,    51,   105,   733,   734,    55,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,
     119,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    81,    -1,    -1,    -1,   138,
     139,   140,   141,   142,    -1,   144,   145,   146,   147,   148,
      51,    -1,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    69,   795,
      -1,    -1,    -1,   119,   800,   801,    -1,   803,    -1,    -1,
     806,   807,   288,    -1,    85,   811,   812,   813,    -1,   295,
     296,    -1,    -1,    94,    -1,    -1,    -1,   303,    -1,   100,
     101,   102,   103,    -1,    -1,    -1,    -1,    -1,   314,   835,
      -1,    -1,   838,   839,    -1,   841,   842,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   851,    -1,   128,    -1,   855,
     131,    51,    -1,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,   349,    -1,    -1,   872,   873,   354,    69,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   885,
     886,    -1,    -1,    -1,   210,    -1,    -1,    -1,   374,    -1,
      -1,    -1,    -1,    -1,    94,   901,    -1,    -1,    -1,    -1,
     100,   101,   102,   103,    -1,   911,   912,    88,    89,   395,
      -1,    -1,   918,    -1,   400,   401,    -1,   403,    -1,    -1,
     101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,
      -1,   131,    -1,    -1,    -1,   941,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   949,    -1,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,   443,   444,    -1,
      -1,    -1,   288,    -1,    -1,    -1,    -1,    -1,    -1,   295,
     296,    -1,    -1,    -1,   460,    -1,    -1,   303,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   471,    -1,    -1,   314,    -1,
      -1,    -1,    -1,   479,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,   489,    -1,    80,    81,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   349,    -1,    -1,   101,    -1,   354,    -1,
      -1,    -1,    -1,  1039,    -1,    -1,   522,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   531,    -1,    -1,   374,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   545,
      -1,   547,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   395,
       0,   557,    -1,    -1,   400,   401,    -1,   403,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    -1,    26,    27,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,   443,   444,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   612,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   460,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   471,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   479,    -1,    -1,    -1,    -1,    -1,    -1,
      90,    -1,    -1,   489,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   105,     1,    -1,     3,     4,
       5,    -1,    -1,    -1,    -1,   115,    -1,    12,   118,   119,
      -1,    -1,    -1,    -1,   680,    -1,   522,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   531,    -1,    -1,   138,   139,
      -1,    -1,    -1,    -1,   144,   145,   146,   147,   148,   545,
      -1,   547,    -1,    -1,    -1,    -1,    51,    -1,    -1,    -1,
      55,   557,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    81,    -1,    -1,    88,
      89,    -1,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   612,    -1,    -1,    -1,
      -1,    -1,    -1,   122,   119,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,   791,    -1,    -1,    -1,    -1,
      -1,    -1,   798,   799,   800,    -1,    -1,    -1,    -1,    -1,
     806,    -1,    -1,    -1,    -1,    -1,   812,   813,    -1,    -1,
      -1,    -1,   818,   819,    -1,    -1,     1,    -1,     3,     4,
       5,     6,    -1,    -1,    -1,    -1,   832,    12,    -1,    -1,
      -1,    -1,    -1,    -1,   680,    -1,    -1,    -1,   844,   845,
      -1,    -1,    -1,    -1,    -1,    -1,   852,    72,    73,    74,
      75,    76,    77,    78,    -1,    80,    81,   863,   864,    -1,
      -1,    -1,    -1,    88,    89,   210,    51,    -1,    -1,    -1,
      55,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   890,    -1,    -1,    -1,    -1,   895,
      -1,    -1,    -1,    -1,    -1,    -1,    81,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,    -1,   918,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   928,   929,    -1,    -1,   932,    -1,    -1,    -1,
     936,    -1,    -1,    -1,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   288,    -1,   791,    -1,    -1,    -1,    -1,
     295,   296,   798,   799,   800,    -1,    -1,    -1,   303,    -1,
     806,    -1,    -1,    -1,    -1,    -1,   812,   813,    -1,   314,
      -1,    -1,   818,   819,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   832,    -1,    -1,    -1,
     996,    -1,   998,    -1,    -1,  1001,    -1,    -1,   844,   845,
      -1,    -1,    -1,    -1,   349,    -1,   852,    -1,    -1,   354,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   863,   864,    -1,
      -1,    -1,    -1,    -1,    -1,   210,    -1,    -1,    -1,   374,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   890,    -1,    -1,    -1,    -1,   895,
     395,    -1,    -1,    -1,    -1,   400,   401,    -1,   403,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   918,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   928,   929,    -1,    -1,   932,    -1,    -1,     1,
     936,     3,    -1,    -1,    -1,    -1,    -1,    -1,   443,   444,
      12,    -1,    -1,   288,    -1,    -1,    -1,    -1,    -1,    -1,
     295,   296,    -1,    -1,    -1,   460,    -1,    -1,   303,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   471,    -1,    -1,   314,
      -1,    -1,    -1,    -1,   479,    -1,    -1,    -1,    -1,    51,
      -1,    -1,    -1,    -1,   489,    -1,    -1,    -1,    -1,    -1,
     996,    -1,   998,    -1,    -1,  1001,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   349,    -1,    -1,    -1,    -1,   354,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   522,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   531,    -1,    -1,   374,
      -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,
     545,    -1,   547,    -1,    -1,    -1,    -1,   119,    -1,    -1,
     395,    -1,   557,    -1,    -1,   400,   401,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   612,    -1,    -1,
       1,    -1,     3,    -1,   122,   460,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,   471,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   479,    -1,    -1,    -1,   210,    -1,
      -1,    -1,    -1,    -1,   489,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      51,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    51,    52,
      -1,    -1,    55,    -1,    -1,   680,    -1,   522,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   531,    70,    71,    72,
      73,    74,    75,    76,    77,    -1,    -1,    80,    81,    -1,
     545,    -1,   547,    86,    87,    88,    89,    -1,    -1,    -1,
      -1,    -1,   557,    -1,    -1,    -1,   288,   100,   101,   102,
      -1,    -1,    -1,   295,   296,    -1,    -1,    -1,   119,    -1,
      -1,   303,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,
     143,   144,    -1,    -1,    -1,    -1,    -1,   612,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   349,    -1,    -1,
      -1,    -1,   354,    -1,    -1,    -1,   791,    -1,    -1,    -1,
      -1,    -1,    -1,   798,   799,   800,    -1,    -1,    -1,    -1,
      -1,   806,   374,    -1,    -1,    -1,    -1,   812,   813,    -1,
      -1,    -1,    -1,   818,   819,    -1,    -1,    -1,    -1,   210,
      -1,    -1,    -1,   395,    -1,    -1,    -1,   832,   400,    -1,
      -1,   403,    -1,    -1,    -1,   680,    -1,    -1,    -1,   844,
     845,    -1,    -1,    -1,    -1,    -1,    -1,   852,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   863,   864,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   443,   444,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   890,    -1,    -1,   460,    -1,
     895,    -1,    -1,    -1,    -1,    -1,    -1,   288,    -1,   471,
      -1,    -1,    -1,    -1,   295,   296,    -1,   479,    -1,    -1,
      -1,    -1,   303,   918,    -1,    -1,    -1,   489,    -1,    -1,
      -1,    -1,    -1,   928,   929,    -1,    -1,   932,    -1,    -1,
      -1,   936,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   791,    -1,    -1,    -1,
     522,    -1,    -1,   798,   799,    -1,    -1,    -1,   349,   531,
      -1,    -1,    -1,   354,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   818,   819,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   374,    -1,   557,    -1,   832,    -1,    -1,
      -1,   996,    -1,   998,    -1,    -1,  1001,    -1,    -1,   844,
     845,    -1,    -1,    -1,   395,    -1,    -1,   852,    -1,   400,
      -1,    -1,   403,    -1,    -1,    -1,    -1,    -1,   863,   864,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   890,    -1,    -1,    -1,    -1,
     895,    -1,   443,   444,    -1,    -1,    -1,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,   460,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,
     471,    -1,    -1,   928,   929,    -1,   101,   932,   479,    -1,
      -1,   936,    -1,    -1,    -1,    -1,    -1,    -1,   489,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   122,   680,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      72,    73,    74,    75,    76,    77,    -1,   142,    80,    81,
      -1,   522,    -1,    -1,    -1,    -1,    88,    89,    -1,    -1,
     531,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,
      -1,   996,    -1,   998,    -1,    -1,  1001,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   557,    -1,    -1,    -1,
      -1,    -1,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   791,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,   800,    -1,
      -1,    -1,    -1,    -1,   806,    -1,    -1,    -1,    -1,    -1,
     812,   813,    -1,    -1,     0,    -1,   818,   819,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    13,    14,    15,
     832,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,
      26,    -1,   844,   845,    -1,    -1,    -1,    -1,    -1,    -1,
     852,    37,    38,    -1,    40,    41,    42,    43,    44,   680,
      -1,   863,   864,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    72,    73,    74,    75,
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    90,    -1,    92,    93,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,   918,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,   928,   929,    -1,   115,
     932,    -1,   118,   119,   936,   121,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
      -1,    -1,   138,   139,   140,    -1,   142,    -1,    -1,   145,
     146,   147,   148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     791,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   799,   800,
      -1,    -1,    -1,    51,    52,   806,    -1,    55,    -1,    -1,
      -1,   812,   813,    -1,   996,    -1,   998,   818,   819,  1001,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,   832,    80,    81,    -1,    -1,    -1,    -1,    86,    87,
      88,    89,    -1,   844,   845,    -1,    -1,    -1,    -1,    -1,
      -1,   852,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   863,   864,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    -1,
      -1,    -1,    -1,    -1,    -1,   143,   144,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   918,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   928,   929,    -1,
      -1,    -1,    -1,    -1,    -1,   936,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     0,     1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,    -1,    -1,    -1,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,   996,    39,   998,    -1,    -1,
    1001,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
       0,   114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,
      -1,    -1,    -1,    13,    14,    15,    16,    17,    18,    -1,
      20,   134,   135,   136,    -1,    -1,    -1,    27,    28,    29,
      -1,    -1,    -1,   146,    -1,   148,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      90,    -1,    -1,    93,    -1,    -1,    -1,    -1,    -1,    99,
      -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   113,    -1,   115,    -1,    -1,   118,   119,
      -1,    -1,   122,   123,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,   139,
     140,   141,   142,    -1,    -1,   145,   146,   147,   148,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    -1,    -1,    27,    28,    -1,    -1,    -1,    -1,    -1,
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
      18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    26,    27,
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
      -1,    -1,    -1,    -1,    -1,    27,    28,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,
      -1,    93,    -1,    -1,    -1,    -1,    -1,    99,    -1,   101,
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
      76,    77,    78,    79,    80,    81,    82,    83,    -1,    -1,
      -1,    -1,    88,    89,    90,    -1,    92,    93,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,   105,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,   118,   119,    -1,   121,   122,    -1,   124,   125,
     126,   127,   128,   129,   130,   131,   132,   133,    -1,    -1,
       0,    -1,   138,   139,   140,    -1,   142,    -1,    -1,   145,
     146,   147,   148,    13,    14,    15,    -1,    17,    18,    -1,
      20,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      14,    15,    -1,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,   139,   140,    -1,   142,    -1,
      -1,   145,   146,   147,   148,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,    15,
      -1,    -1,    18,    19,    -1,    21,    22,    23,    24,    -1,
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
      10,    11,    12,    -1,    -1,    15,    -1,    17,    18,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
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
      -1,    15,    -1,    -1,    18,    19,    20,    21,    22,    23,
      24,    -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,
      34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,
     104,    -1,   106,   107,   108,    -1,   110,   111,   112,    -1,
     114,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     134,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   146,     1,   148,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    -1,    -1,    15,    -1,    -1,
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
     118,   119,     1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,   134,   135,   136,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    -1,   146,    -1,
     148,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
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
      -1,    14,    15,    -1,    -1,    -1,    19,    -1,    21,    22,
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
      -1,   145,   146,    -1,   148,    30,    31,    32,    33,    34,
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
      -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,
     136,    -1,    -1,   139,    -1,    -1,    -1,    -1,    -1,    -1,
     146,     1,   148,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    -1,    -1,    15,    -1,    -1,    -1,    19,
      -1,    21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    -1,    -1,    39,
      -1,    -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    58,    59,
      60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,
      70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,
      -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,   115,    -1,    -1,   118,   119,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,   134,   135,   136,    -1,    19,    -1,
      21,    22,    23,    24,    -1,    -1,   146,    -1,   148,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
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
      -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,    51,
      52,    53,    54,    55,    56,    -1,    58,    59,    60,    -1,
      -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,    71,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
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
     115,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,
     135,   136,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   148,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,   102,   103,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,   102,   103,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,    -1,    -1,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,   134,
     135,   136,    19,   138,    21,    22,    23,    24,    -1,   144,
      -1,    -1,    -1,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    92,    -1,    94,    95,    -1,
      97,    98,    -1,   100,    -1,   102,   103,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,    -1,    -1,
      -1,    -1,    -1,    -1,   121,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    -1,    -1,    -1,   134,   135,   136,
      -1,    19,    -1,    21,    22,    23,    24,   144,    -1,    -1,
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
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,   102,   103,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,   144,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    70,
      71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,
      -1,   102,   103,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   134,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   144,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,    15,    16,    17,    18,
      19,    20,    21,    22,    23,    24,    25,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    -1,
      -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,
      69,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
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
      48,    49,    50,    51,    52,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,
      -1,    -1,    70,    71,    72,    73,    74,    75,    76,    77,
      -1,    -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,
      88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,   107,
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
      -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,   106,
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
      76,    77,    -1,    -1,    80,    81,    -1,    -1,    -1,    -1,
      86,    87,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   100,   101,   102,    -1,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,   134,
      21,    22,    23,    24,    -1,    -1,    -1,   142,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    58,    59,    60,
      -1,    -1,    63,    -1,    -1,    66,    67,    -1,    69,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,
      -1,    -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    12,    -1,    -1,    -1,    -1,
      -1,    -1,    19,   134,    21,    22,    23,    24,    -1,    -1,
      -1,   142,    -1,    30,    31,    32,    33,    34,    35,    36,
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
      -1,    -1,    -1,    -1,    -1,    88,    -1,    -1,    91,    -1,
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
      -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
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
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    19,   134,    21,    22,
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
      -1,    -1,    51,    52,    -1,   104,    55,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,   134,    -1,    86,    87,    88,
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
      -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,   143,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    44,    -1,    -1,
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
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   122,    -1,   124,   125,   126,   127,   128,
     129,   130,   131,   132,   133,    72,    73,    74,    75,    76,
      77,    78,    79,    80,    81,    82,    83,    -1,    -1,    -1,
      -1,    88,    89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133
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
     176,   177,   179,   180,   189,   203,   220,   241,   242,   252,
     253,   254,   258,   259,   260,   266,   267,   268,   270,   271,
     272,   273,   274,   275,   311,   324,     0,   154,    21,    22,
      30,    31,    32,    39,    51,    55,    69,    88,    91,    94,
     134,   164,   166,   181,   182,   203,   220,   272,   275,   311,
     182,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,    17,    18,    19,    20,    21,
      22,    23,    24,    25,    26,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    45,    46,    47,    48,    49,
      50,    51,    52,    55,    70,    71,    72,    73,    74,    75,
      76,    77,    80,    81,    86,    87,    88,    89,   100,   101,
     102,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   135,   136,   143,   144,   183,   187,   188,   274,   306,
     204,    91,   163,   164,   166,   167,   180,   189,   220,   272,
     273,   275,   167,   210,   212,    69,    91,   173,   180,   220,
     225,   272,   275,    33,    34,    35,    36,    48,    49,    50,
      51,    55,   106,   183,   184,   185,   268,   115,   118,   119,
     146,   148,   167,   262,   263,   264,   317,   321,   322,   323,
      51,    69,   100,   102,   103,   135,   172,   189,   195,   198,
     201,   254,   309,   310,   195,   195,   144,   192,   193,   196,
     197,   324,   192,   197,   144,   318,   184,   155,   138,   189,
     220,   189,   189,   189,    55,     1,    94,   157,   158,   160,
     174,   175,   324,   205,   207,   190,   201,   309,   324,   189,
     308,   309,   324,    91,   142,   179,   220,   272,   275,   208,
      53,    54,    56,    63,    69,   107,   183,   269,    63,    64,
      65,   116,   117,   255,   256,    61,   255,    62,   255,    63,
     255,    63,   255,    58,    59,   168,   189,   189,   317,   323,
      40,    41,    42,    43,    44,    37,    38,    51,    53,    54,
      55,    56,    69,    83,    94,   100,   101,   102,   103,   128,
     131,   144,   278,   279,   280,   281,   282,   285,   286,   287,
     288,   290,   291,   292,   293,   295,   296,   297,   300,   301,
     302,   303,   304,   324,   278,   280,    28,   240,   121,   142,
      94,   100,   176,   121,    72,    73,    74,    75,    76,    77,
      78,    79,    80,    81,    82,    83,    88,    89,    93,   101,
     122,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    90,   105,   140,   147,   315,    90,   315,   316,    26,
     138,   244,   254,    92,    92,   192,   197,   244,   163,    51,
      55,   181,    58,    59,   279,   125,   276,    90,   140,   315,
     219,   307,    90,   147,   314,   156,   157,    55,   278,   278,
      16,   221,   321,   121,    90,   140,   315,    92,    92,   221,
     167,   167,    55,    90,   140,   315,    25,   107,   142,   265,
     317,   115,   264,    20,   246,   321,    57,    57,   189,   189,
     189,    93,   142,   199,   200,   324,    57,   199,   200,    85,
     194,   195,   201,   309,   324,   195,   163,   317,   319,   163,
     322,   159,   138,   157,    90,   315,    92,   160,   174,   145,
     317,   323,   319,   160,   319,   141,   200,   320,   323,   200,
     320,   139,   320,    55,   176,   177,   178,   142,    90,   140,
     315,   144,   237,   290,   295,    63,   255,   257,   261,   262,
      63,   256,    61,    62,    63,    63,   101,   101,   154,   167,
     167,   167,   167,   160,   163,   163,    57,   121,    57,   321,
     294,    85,   290,   295,   121,   156,   189,   142,   305,   324,
      51,   142,   305,   321,   142,   289,   189,   142,   289,    51,
     142,   289,    51,   121,   156,   239,   100,   168,   189,   201,
     202,   174,   142,   179,   142,   161,   162,   168,   180,   189,
     191,   202,   220,   275,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,    51,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
     189,    51,    52,    55,   187,   192,   312,   313,   194,   201,
      51,    52,    55,   187,   192,   312,    51,    55,   312,   245,
     243,   162,   189,   191,   162,   191,    99,   171,   217,   277,
     216,    51,    55,   181,   312,   194,   312,   156,   163,   165,
      15,    13,   248,   324,   121,   121,   157,    16,    51,    55,
     194,    51,    55,   157,    27,   222,   321,   222,    51,    55,
     194,    51,    55,   214,   186,   157,   246,   189,   201,    15,
     189,   189,   189,   318,   100,   189,   198,   309,   189,   310,
     319,   145,   317,   200,   200,   319,   145,   184,   152,   139,
     191,   319,   160,   206,   309,   176,   178,    51,    55,   194,
      51,    55,   290,   209,   142,    63,   157,   262,   189,   189,
      51,    69,   100,   226,   295,   319,   319,   142,   172,   189,
      15,    51,    69,   282,   287,   304,    85,   288,   293,   300,
     302,   295,   297,   302,    51,   295,   172,   189,    15,    79,
     126,   231,   233,   324,   189,   200,   319,   178,   142,    44,
     121,    44,    90,   140,   315,   318,    92,    92,   192,   197,
     141,   200,    92,    92,   193,   197,   193,   197,   231,   231,
     170,   321,   167,   156,   141,    15,   319,   183,   189,   202,
     249,   324,    18,   224,   324,    17,   223,   224,    92,    92,
     141,    92,    92,   224,   211,   213,   141,   167,   184,   139,
      15,   200,   221,   189,   199,    85,   309,   139,   319,   320,
     141,   234,   318,    29,   113,   238,   139,   142,   292,   319,
     142,    85,    44,    44,   305,   321,   142,   289,   142,   289,
     142,   289,   142,   289,   289,    44,    44,   228,   230,   232,
     281,   283,   284,   287,   295,   296,   298,   299,   302,   304,
     156,   100,   189,   178,   160,   189,    51,    55,   194,    51,
      55,    57,   123,   162,   191,   168,   191,   171,    92,   162,
     191,   162,   191,   171,   244,   240,   156,   157,   231,   218,
     321,    15,    93,   250,   324,   157,    14,   251,   324,   167,
      15,    92,    15,   157,   157,   222,   189,   157,   319,   200,
     145,   146,   156,   157,   227,   142,   100,   319,   189,   189,
     295,   302,   295,   295,   189,   189,   234,   234,    91,   220,
     142,   305,   305,   142,   229,   220,   142,   229,   142,   229,
      15,   189,   141,   189,   189,   162,   191,    15,   139,   157,
     156,    91,   180,   220,   272,   275,   221,   157,   221,    15,
      15,   215,   224,   246,   247,    51,   235,   236,   291,    15,
     139,   295,   295,   142,   292,   289,   142,   289,   289,   289,
     126,   126,    55,    90,   283,   287,   142,   228,   229,   299,
     302,   295,   298,   302,   295,   139,    15,    55,    90,   140,
     315,   157,   157,   157,   142,   318,   142,   295,   142,   295,
      51,    55,   305,   142,   229,   142,   229,   142,   229,   142,
     229,   229,    51,    55,   194,    51,    55,   248,   223,    15,
     236,   295,   289,   295,   302,   295,   295,   141,   229,   142,
     229,   229,   229,   295,   229
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
     203,   212,   213,   203,   203,   203,   214,   215,   203,   216,
     203,   217,   218,   203,   219,   203,   203,   203,   203,   203,
     203,   203,   220,   221,   221,   221,   222,   222,   223,   223,
     224,   224,   225,   225,   226,   226,   226,   226,   226,   226,
     226,   226,   227,   226,   228,   228,   228,   228,   229,   229,
     230,   230,   230,   230,   230,   230,   230,   230,   230,   230,
     230,   230,   230,   230,   230,   231,   231,   232,   233,   233,
     233,   234,   234,   235,   235,   236,   236,   237,   237,   238,
     238,   239,   240,   241,   241,   241,   241,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   243,   244,   245,   244,
     246,   247,   247,   248,   248,   249,   249,   249,   250,   250,
     251,   251,   252,   252,   252,   252,   253,   253,   254,   254,
     254,   254,   255,   255,   256,   257,   256,   256,   256,   258,
     258,   259,   259,   260,   261,   261,   262,   262,   263,   263,
     264,   265,   264,   266,   266,   267,   267,   267,   268,   269,
     269,   269,   269,   269,   269,   270,   270,   271,   271,   271,
     271,   272,   272,   272,   272,   272,   273,   273,   274,   274,
     274,   274,   274,   274,   274,   274,   274,   275,   275,   276,
     277,   276,   278,   278,   279,   279,   279,   280,   280,   280,
     280,   281,   281,   282,   282,   283,   283,   284,   284,   285,
     285,   286,   286,   287,   287,   288,   288,   288,   288,   289,
     289,   290,   290,   290,   290,   290,   290,   290,   290,   290,
     290,   290,   290,   290,   290,   290,   291,   291,   291,   291,
     291,   292,   292,   293,   294,   293,   295,   295,   296,   297,
     298,   299,   299,   300,   300,   301,   301,   302,   302,   303,
     303,   304,   304,   305,   305,   306,   307,   306,   308,   308,
     309,   309,   310,   310,   310,   310,   310,   310,   310,   310,
     311,   311,   311,   312,   312,   312,   312,   313,   313,   313,
     314,   314,   315,   315,   316,   316,   317,   317,   318,   318,
     319,   320,   320,   320,   321,   321,   322,   322,   323,   323,
     324
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
       7,     0,     0,     7,     5,     4,     0,     0,     9,     0,
       6,     0,     0,     8,     0,     5,     4,     4,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     5,
       1,     2,     1,     1,     1,     4,     6,     3,     5,     2,
       4,     1,     0,     4,     4,     2,     2,     1,     2,     0,
       6,     8,     4,     6,     4,     3,     6,     2,     4,     6,
       2,     4,     2,     4,     1,     1,     1,     0,     4,     1,
       4,     1,     4,     1,     3,     1,     1,     4,     1,     3,
       3,     0,     5,     2,     4,     5,     5,     2,     4,     4,
       3,     3,     3,     2,     1,     4,     0,     5,     0,     5,
       5,     1,     1,     6,     1,     1,     1,     1,     2,     1,
       2,     1,     1,     1,     1,     1,     1,     2,     1,     1,
       2,     3,     1,     2,     1,     0,     4,     1,     2,     2,
       3,     2,     3,     1,     1,     2,     1,     2,     1,     2,
       1,     0,     4,     2,     3,     1,     4,     2,     2,     1,
       1,     1,     1,     1,     2,     2,     3,     1,     1,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       0,     4,     1,     1,     3,     5,     3,     1,     2,     4,
       2,     2,     2,     2,     1,     2,     1,     1,     3,     1,
       3,     1,     1,     2,     1,     4,     2,     2,     1,     2,
       0,     6,     8,     4,     6,     4,     6,     2,     4,     6,
       2,     4,     2,     4,     1,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     4,     1,     3,     2,     2,
       2,     1,     3,     1,     3,     1,     1,     2,     1,     1,
       1,     2,     1,     2,     1,     1,     0,     4,     1,     2,
       1,     3,     3,     3,     2,     2,     3,     3,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     0,     2,
       2,     0,     1,     1,     1,     1,     1,     1,     1,     2,
       0
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



#line 6528 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 1665 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 6746 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3: /* program: $@1 top_compstmt  */
#line 1670 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 6755 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4: /* top_compstmt: top_stmts opt_terms  */
#line 1677 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6763 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5: /* top_stmts: none  */
#line 1683 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6771 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6: /* top_stmts: top_stmt  */
#line 1687 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6780 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7: /* top_stmts: top_stmts terms top_stmt  */
#line 1692 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6788 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8: /* top_stmts: error top_stmt  */
#line 1696 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6796 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10: /* @2: %empty  */
#line 1703 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 6805 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11: /* top_stmt: "'BEGIN'" @2 '{' top_compstmt '}'  */
#line 1708 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 6816 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12: /* bodystmt: compstmt opt_rescue opt_else opt_ensure  */
#line 1720 "mrbgems/mruby-compiler/core/parse.y"
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
#line 6842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13: /* compstmt: stmts opt_terms  */
#line 1744 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14: /* stmts: none  */
#line 1750 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15: /* stmts: stmt  */
#line 1754 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6867 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16: /* stmts: stmts terms stmt  */
#line 1759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6875 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17: /* stmts: error stmt  */
#line 1763 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 6883 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18: /* $@3: %empty  */
#line 1768 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 6889 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19: /* stmt: "'alias'" fsym $@3 fsym  */
#line 1769 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 6897 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20: /* stmt: "'undef'" undef_list  */
#line 1773 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21: /* stmt: stmt "'if' modifier" expr_value  */
#line 1777 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22: /* stmt: stmt "'unless' modifier" expr_value  */
#line 1781 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23: /* stmt: stmt "'while' modifier" expr_value  */
#line 1785 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6929 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24: /* stmt: stmt "'until' modifier" expr_value  */
#line 1789 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6937 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25: /* stmt: stmt "'rescue' modifier" stmt  */
#line 1793 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6945 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26: /* stmt: "'END'" '{' compstmt '}'  */
#line 1797 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 6954 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28: /* stmt: mlhs '=' command_call  */
#line 1803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6962 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29: /* stmt: lhs '=' mrhs  */
#line 1807 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6970 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30: /* stmt: mlhs '=' arg  */
#line 1811 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6978 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31: /* stmt: mlhs '=' mrhs  */
#line 1815 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6986 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 32: /* stmt: arg "=>" "local variable or method"  */
#line 1819 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *lhs = new_lvar(p, (yyvsp[0].id));
                      assignable(p, lhs);
                      (yyval.nd) = new_asgn(p, lhs, (yyvsp[-2].nd));
                    }
#line 6996 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34: /* command_asgn: lhs '=' command_rhs  */
#line 1828 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35: /* command_asgn: var_lhs tOP_ASGN command_rhs  */
#line 1832 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7012 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36: /* command_asgn: primary_value '[' opt_call_args ']' tOP_ASGN command_rhs  */
#line 1836 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7020 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37: /* command_asgn: primary_value call_op "local variable or method" tOP_ASGN command_rhs  */
#line 1840 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7028 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38: /* command_asgn: primary_value call_op "constant" tOP_ASGN command_rhs  */
#line 1844 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7036 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39: /* command_asgn: primary_value "::" "constant" tOP_ASGN command_call  */
#line 1848 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 7045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40: /* command_asgn: primary_value "::" "local variable or method" tOP_ASGN command_rhs  */
#line 1853 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7053 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41: /* command_asgn: defn_head f_opt_arglist_paren '=' command  */
#line 1857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7066 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42: /* command_asgn: defn_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 1866 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7079 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43: /* command_asgn: defs_head f_opt_arglist_paren '=' command  */
#line 1875 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7092 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44: /* command_asgn: defs_head f_opt_arglist_paren '=' command "'rescue' modifier" arg  */
#line 1884 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 45: /* command_asgn: backref tOP_ASGN command_rhs  */
#line 1893 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7114 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47: /* command_rhs: command_call "'rescue' modifier" stmt  */
#line 1901 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7122 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50: /* expr: expr "'and'" expr  */
#line 1910 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7130 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51: /* expr: expr "'or'" expr  */
#line 1914 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7138 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52: /* expr: "'not'" opt_nl expr  */
#line 1918 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7146 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53: /* expr: '!' command_call  */
#line 1922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7154 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55: /* defn_head: "'def'" fname  */
#line 1930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 7165 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56: /* $@4: %empty  */
#line 1939 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 7173 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 57: /* defs_head: "'def'" singleton dot_or_colon $@4 fname  */
#line 1943 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 7186 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58: /* expr_value: expr  */
#line 1954 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 7197 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62: /* block_command: block_call call_op2 operation2 command_args  */
#line 1968 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63: /* $@5: %empty  */
#line 1974 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 7214 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64: /* cmd_brace_block: "{" $@5 opt_block_param compstmt '}'  */
#line 1981 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 7224 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65: /* command: operation command_args  */
#line 1989 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7232 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66: /* command: operation command_args cmd_brace_block  */
#line 1993 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 7241 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67: /* command: primary_value call_op operation2 command_args  */
#line 1998 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 7249 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68: /* command: primary_value call_op operation2 command_args cmd_brace_block  */
#line 2002 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 7258 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69: /* command: primary_value "::" operation2 command_args  */
#line 2007 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 7266 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70: /* command: primary_value "::" operation2 command_args cmd_brace_block  */
#line 2011 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 7275 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71: /* command: "'super'" command_args  */
#line 2016 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 7283 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72: /* command: "'yield'" command_args  */
#line 2020 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 7291 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73: /* command: "'return'" call_args  */
#line 2024 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7299 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74: /* command: "'break'" call_args  */
#line 2028 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7307 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75: /* command: "'next'" call_args  */
#line 2032 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 7315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76: /* mlhs: mlhs_basic  */
#line 2038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77: /* mlhs: tLPAREN mlhs_inner rparen  */
#line 2042 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7331 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79: /* mlhs_inner: tLPAREN mlhs_inner rparen  */
#line 2049 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7339 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80: /* mlhs_basic: mlhs_list  */
#line 2055 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7347 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81: /* mlhs_basic: mlhs_list mlhs_item  */
#line 2059 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 7355 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82: /* mlhs_basic: mlhs_list "*" mlhs_node  */
#line 2063 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7363 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83: /* mlhs_basic: mlhs_list "*" mlhs_node ',' mlhs_post  */
#line 2067 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7371 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84: /* mlhs_basic: mlhs_list "*"  */
#line 2071 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 7379 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85: /* mlhs_basic: mlhs_list "*" ',' mlhs_post  */
#line 2075 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 7387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86: /* mlhs_basic: "*" mlhs_node  */
#line 2079 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 7395 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87: /* mlhs_basic: "*" mlhs_node ',' mlhs_post  */
#line 2083 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7403 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88: /* mlhs_basic: "*"  */
#line 2087 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 7411 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89: /* mlhs_basic: "*" ',' mlhs_post  */
#line 2091 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 7419 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91: /* mlhs_item: tLPAREN mlhs_inner rparen  */
#line 2098 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 7427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92: /* mlhs_list: mlhs_item ','  */
#line 2104 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 7435 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93: /* mlhs_list: mlhs_list mlhs_item ','  */
#line 2108 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 7443 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94: /* mlhs_post: mlhs_item  */
#line 2114 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 7451 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95: /* mlhs_post: mlhs_list mlhs_item  */
#line 2118 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 7459 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96: /* mlhs_node: variable  */
#line 2124 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 7467 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97: /* mlhs_node: primary_value '[' opt_call_args ']'  */
#line 2128 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 7475 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98: /* mlhs_node: primary_value call_op "local variable or method"  */
#line 2132 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 7483 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99: /* mlhs_node: primary_value "::" "local variable or method"  */
#line 2136 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 7491 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100: /* mlhs_node: primary_value call_op "constant"  */
#line 2140 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 7499 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101: /* mlhs_node: primary_value "::" "constant"  */
#line 2144 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 7509 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102: /* mlhs_node: tCOLON3 "constant"  */
#line 2150 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 7519 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103: /* mlhs_node: backref  */
#line 2156 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 7528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104: /* lhs: variable  */
#line 2163 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 7536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105: /* lhs: primary_value '[' opt_call_args ']'  */
#line 2167 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 7544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106: /* lhs: primary_value call_op "local variable or method"  */
#line 2171 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 7552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107: /* lhs: primary_value "::" "local variable or method"  */
#line 2175 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 7560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108: /* lhs: primary_value call_op "constant"  */
#line 2179 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 7568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109: /* lhs: primary_value "::" "constant"  */
#line 2183 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 7578 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110: /* lhs: tCOLON3 "constant"  */
#line 2189 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 7588 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111: /* lhs: backref  */
#line 2195 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 7597 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112: /* lhs: "numbered parameter"  */
#line 2200 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 7605 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113: /* cname: "local variable or method"  */
#line 2206 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "class/module name must be CONSTANT");
                    }
#line 7613 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115: /* cpath: tCOLON3 cname  */
#line 2213 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(nint(1), nsym((yyvsp[0].id)));
                    }
#line 7621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116: /* cpath: cname  */
#line 2217 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(nint(0), nsym((yyvsp[0].id)));
                    }
#line 7629 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117: /* cpath: primary_value "::" cname  */
#line 2221 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 7638 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121: /* fname: op  */
#line 2231 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 7647 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122: /* fname: reswords  */
#line 2236 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 7656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125: /* undef_list: fsym  */
#line 2247 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 7664 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126: /* $@6: %empty  */
#line 2250 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 7670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127: /* undef_list: undef_list ',' $@6 fsym  */
#line 2251 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 7678 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128: /* op: '|'  */
#line 2256 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(or);     }
#line 7684 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129: /* op: '^'  */
#line 2257 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(xor);    }
#line 7690 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130: /* op: '&'  */
#line 2258 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(and);    }
#line 7696 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131: /* op: "<=>"  */
#line 2259 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(cmp);    }
#line 7702 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132: /* op: "=="  */
#line 2260 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eq);     }
#line 7708 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133: /* op: "==="  */
#line 2261 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eqq);    }
#line 7714 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134: /* op: "=~"  */
#line 2262 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(match);  }
#line 7720 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135: /* op: "!~"  */
#line 2263 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(nmatch); }
#line 7726 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136: /* op: '>'  */
#line 2264 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(gt);     }
#line 7732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137: /* op: ">="  */
#line 2265 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(ge);     }
#line 7738 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138: /* op: '<'  */
#line 2266 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lt);     }
#line 7744 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139: /* op: "<="  */
#line 2267 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(le);     }
#line 7750 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140: /* op: "!="  */
#line 2268 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neq);    }
#line 7756 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141: /* op: "<<"  */
#line 2269 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lshift); }
#line 7762 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142: /* op: ">>"  */
#line 2270 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(rshift); }
#line 7768 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143: /* op: '+'  */
#line 2271 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(add);    }
#line 7774 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144: /* op: '-'  */
#line 2272 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(sub);    }
#line 7780 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145: /* op: '*'  */
#line 2273 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 7786 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146: /* op: "*"  */
#line 2274 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 7792 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147: /* op: '/'  */
#line 2275 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(div);    }
#line 7798 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148: /* op: '%'  */
#line 2276 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mod);    }
#line 7804 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149: /* op: tPOW  */
#line 2277 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 7810 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150: /* op: "**"  */
#line 2278 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 7816 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151: /* op: '!'  */
#line 2279 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(not);    }
#line 7822 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152: /* op: '~'  */
#line 2280 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neg);    }
#line 7828 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153: /* op: "unary plus"  */
#line 2281 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(plus);   }
#line 7834 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154: /* op: "unary minus"  */
#line 2282 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(minus);  }
#line 7840 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155: /* op: tAREF  */
#line 2283 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aref);   }
#line 7846 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156: /* op: tASET  */
#line 2284 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aset);   }
#line 7852 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157: /* op: '`'  */
#line 2285 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(tick);   }
#line 7858 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198: /* arg: lhs '=' arg_rhs  */
#line 2303 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7866 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199: /* arg: var_lhs tOP_ASGN arg_rhs  */
#line 2307 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7874 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200: /* arg: primary_value '[' opt_call_args ']' tOP_ASGN arg_rhs  */
#line 2311 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7882 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201: /* arg: primary_value call_op "local variable or method" tOP_ASGN arg_rhs  */
#line 2315 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7890 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202: /* arg: primary_value call_op "constant" tOP_ASGN arg_rhs  */
#line 2319 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7898 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203: /* arg: primary_value "::" "local variable or method" tOP_ASGN arg_rhs  */
#line 2323 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7906 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204: /* arg: primary_value "::" "constant" tOP_ASGN arg_rhs  */
#line 2327 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-4]), p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7915 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205: /* arg: tCOLON3 "constant" tOP_ASGN arg_rhs  */
#line 2332 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[-3]), p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7924 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206: /* arg: backref tOP_ASGN arg_rhs  */
#line 2337 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207: /* arg: arg ".." arg  */
#line 2342 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7941 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208: /* arg: arg ".."  */
#line 2346 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7949 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209: /* arg: tBDOT2 arg  */
#line 2350 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7957 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210: /* arg: arg "..." arg  */
#line 2354 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7965 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211: /* arg: arg "..."  */
#line 2358 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7973 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212: /* arg: tBDOT3 arg  */
#line 2362 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7981 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213: /* arg: arg '+' arg  */
#line 2366 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 7989 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214: /* arg: arg '-' arg  */
#line 2370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 7997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215: /* arg: arg '*' arg  */
#line 2374 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 8005 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216: /* arg: arg '/' arg  */
#line 2378 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 8013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217: /* arg: arg '%' arg  */
#line 2382 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 8021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218: /* arg: arg tPOW arg  */
#line 2386 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 8029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219: /* arg: tUMINUS_NUM "integer literal" tPOW arg  */
#line 2390 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 8037 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220: /* arg: tUMINUS_NUM "float literal" tPOW arg  */
#line 2394 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 8045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221: /* arg: "unary plus" arg  */
#line 2398 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 8053 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222: /* arg: "unary minus" arg  */
#line 2402 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 8061 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223: /* arg: arg '|' arg  */
#line 2406 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 8069 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224: /* arg: arg '^' arg  */
#line 2410 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 8077 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225: /* arg: arg '&' arg  */
#line 2414 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 8085 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226: /* arg: arg "<=>" arg  */
#line 2418 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 8093 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227: /* arg: arg '>' arg  */
#line 2422 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 8101 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228: /* arg: arg ">=" arg  */
#line 2426 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 8109 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229: /* arg: arg '<' arg  */
#line 2430 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 8117 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230: /* arg: arg "<=" arg  */
#line 2434 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 8125 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231: /* arg: arg "==" arg  */
#line 2438 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 8133 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232: /* arg: arg "===" arg  */
#line 2442 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 8141 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233: /* arg: arg "!=" arg  */
#line 2446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 8149 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234: /* arg: arg "=~" arg  */
#line 2450 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 8157 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235: /* arg: arg "!~" arg  */
#line 2454 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 8165 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236: /* arg: '!' arg  */
#line 2458 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 8173 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237: /* arg: '~' arg  */
#line 2462 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 8181 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238: /* arg: arg "<<" arg  */
#line 2466 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 8189 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239: /* arg: arg ">>" arg  */
#line 2470 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 8197 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240: /* arg: arg "&&" arg  */
#line 2474 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241: /* arg: arg "||" arg  */
#line 2478 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8213 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242: /* arg: arg '?' arg opt_nl ':' arg  */
#line 2482 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 8221 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243: /* arg: arg '?' arg opt_nl "label" arg  */
#line 2486 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 8229 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244: /* arg: defn_head f_opt_arglist_paren '=' arg  */
#line 2490 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8242 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245: /* arg: defn_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 2499 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8255 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246: /* arg: defs_head f_opt_arglist_paren '=' arg  */
#line 2508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247: /* arg: defs_head f_opt_arglist_paren '=' arg "'rescue' modifier" arg  */
#line 2517 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8281 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248: /* arg: primary  */
#line 2526 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8289 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250: /* aref_args: args trailer  */
#line 2533 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8298 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251: /* aref_args: args comma assocs trailer  */
#line 2538 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd)));
                    }
#line 8306 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252: /* aref_args: assocs trailer  */
#line 2542 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8315 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253: /* arg_rhs: arg  */
#line 2549 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8323 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254: /* arg_rhs: arg "'rescue' modifier" arg  */
#line 2553 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8332 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255: /* paren_args: '(' opt_call_args ')'  */
#line 2560 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256: /* paren_args: '(' args comma tBDOT3 rparen  */
#line 2564 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      (yyval.nd) = new_callargs(p, push((yyvsp[-3].nd), new_splat(p, new_lvar(p, r))),
                                        new_kw_hash(p, list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k)))),
                                        new_block_arg(p, new_lvar(p, b)));
                    }
#line 8353 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257: /* paren_args: '(' tBDOT3 rparen  */
#line 2573 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      if (local_var_p(p, r) && local_var_p(p, k) && local_var_p(p, b)) {
                        (yyval.nd) = new_callargs(p, list1(new_splat(p, new_lvar(p, r))),
                                          new_kw_hash(p, list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k)))),
                                          new_block_arg(p, new_lvar(p, b)));
                      }
                      else {
                        yyerror(&(yylsp[-2]), p, "unexpected argument forwarding ...");
                        (yyval.nd) = 0;
                      }
                    }
#line 8372 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262: /* opt_call_args: args comma  */
#line 2596 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-1].nd),0,0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8381 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263: /* opt_call_args: args comma assocs comma  */
#line 2601 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-3].nd),new_kw_hash(p,(yyvsp[-1].nd)),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 8390 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264: /* opt_call_args: assocs comma  */
#line 2606 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,0,new_kw_hash(p,(yyvsp[-1].nd)),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8399 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265: /* call_args: command  */
#line 2613 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_callargs(p, list1((yyvsp[0].nd)), 0, 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 8409 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266: /* call_args: args opt_block_arg  */
#line 2619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-1].nd), 0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8418 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267: /* call_args: assocs opt_block_arg  */
#line 2624 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, new_kw_hash(p, (yyvsp[-1].nd)), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8427 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268: /* call_args: args comma assocs opt_block_arg  */
#line 2629 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd)), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 8436 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269: /* call_args: block_arg  */
#line 2634 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, 0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 8445 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270: /* @7: %empty  */
#line 2640 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 8454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 271: /* command_args: @7 call_args  */
#line 2645 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8463 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272: /* block_arg: "&" arg  */
#line 2652 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 8471 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273: /* block_arg: "&"  */
#line 2656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, 0);
                    }
#line 8479 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274: /* opt_block_arg: comma block_arg  */
#line 2662 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8487 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275: /* opt_block_arg: none  */
#line 2666 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8495 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277: /* args: arg  */
#line 2675 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 8505 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278: /* args: "*"  */
#line 2681 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 8513 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279: /* args: "*" arg  */
#line 2685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 8522 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280: /* args: args comma arg  */
#line 2690 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8531 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281: /* args: args comma "*"  */
#line 2695 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 8539 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282: /* args: args comma "*" arg  */
#line 2699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 8547 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 283: /* mrhs: args comma arg  */
#line 2705 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 8556 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 284: /* mrhs: args comma "*" arg  */
#line 2710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 8564 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 285: /* mrhs: "*" arg  */
#line 2714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 8572 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293: /* primary: "method"  */
#line 2727 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 8580 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294: /* @8: %empty  */
#line 2731 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 8589 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295: /* primary: "'begin'" @8 bodystmt "'end'"  */
#line 2737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296: /* @9: %empty  */
#line 2742 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 8607 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297: /* $@10: %empty  */
#line 2746 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 8613 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298: /* primary: "(" @9 stmt $@10 rparen  */
#line 2747 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8622 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299: /* $@11: %empty  */
#line 2751 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 8628 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300: /* primary: "(" $@11 rparen  */
#line 2752 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 8636 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301: /* primary: tLPAREN compstmt ')'  */
#line 2756 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8644 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302: /* primary: primary_value "::" "constant"  */
#line 2760 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8652 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303: /* primary: tCOLON3 "constant"  */
#line 2764 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8660 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304: /* primary: "[" aref_args ']'  */
#line 2768 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8669 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305: /* primary: tLBRACE assoc_list '}'  */
#line 2773 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8678 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306: /* primary: "'return'"  */
#line 2778 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 8686 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307: /* primary: "'yield'" opt_paren_args  */
#line 2782 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 8694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308: /* primary: "'not'" '(' expr rparen  */
#line 2786 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 8702 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309: /* primary: "'not'" '(' rparen  */
#line 2790 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 8710 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310: /* primary: operation brace_block  */
#line 2794 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), new_callargs(p, 0, 0, (yyvsp[0].nd)));
                    }
#line 8718 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312: /* primary: method_call brace_block  */
#line 2799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8727 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313: /* @12: %empty  */
#line 2804 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 8738 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314: /* @13: %empty  */
#line 2811 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 8747 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315: /* primary: "->" @12 f_larglist @13 lambda_body  */
#line 2816 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 8760 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316: /* primary: "'if'" expr_value then compstmt if_tail "'end'"  */
#line 2828 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 8769 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317: /* primary: "'unless'" expr_value then compstmt opt_else "'end'"  */
#line 2836 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 8778 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318: /* $@14: %empty  */
#line 2840 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 8784 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319: /* $@15: %empty  */
#line 2840 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 8790 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320: /* primary: "'while'" $@14 expr_value do $@15 compstmt "'end'"  */
#line 2843 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 8799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321: /* $@16: %empty  */
#line 2847 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 8805 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322: /* $@17: %empty  */
#line 2847 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 8811 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323: /* primary: "'until'" $@16 expr_value do $@17 compstmt "'end'"  */
#line 2850 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 8820 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324: /* primary: "'case'" expr_value opt_terms case_body "'end'"  */
#line 2857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8828 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325: /* primary: "'case'" opt_terms case_body "'end'"  */
#line 2861 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 8836 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326: /* $@18: %empty  */
#line 2865 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 8842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327: /* $@19: %empty  */
#line 2867 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 8848 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328: /* primary: "'for'" for_var "'in'" $@18 expr_value do $@19 compstmt "'end'"  */
#line 2870 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 8857 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329: /* @20: %empty  */
#line 2876 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-2]), p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8868 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330: /* primary: "'class'" cpath superclass @20 bodystmt "'end'"  */
#line 2884 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8879 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331: /* @21: %empty  */
#line 2892 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 8888 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332: /* @22: %empty  */
#line 2897 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 8898 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333: /* primary: "'class'" "<<" expr @21 term @22 bodystmt "'end'"  */
#line 2904 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 8911 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334: /* @23: %empty  */
#line 2914 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(&(yylsp[-1]), p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8922 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335: /* primary: "'module'" cpath @23 bodystmt "'end'"  */
#line 2922 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336: /* primary: defn_head f_arglist bodystmt "'end'"  */
#line 2932 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8944 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337: /* primary: defs_head f_arglist bodystmt "'end'"  */
#line 2942 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8956 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338: /* primary: "'break'"  */
#line 2950 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 8964 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 339: /* primary: "'next'"  */
#line 2954 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 8972 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340: /* primary: "'redo'"  */
#line 2958 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 8980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 341: /* primary: "'retry'"  */
#line 2962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 8988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 342: /* primary_value: primary  */
#line 2968 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 8997 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 349: /* if_tail: "'elsif'" expr_value then compstmt if_tail  */
#line 2987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9005 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 351: /* opt_else: "'else'" compstmt  */
#line 2994 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9013 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352: /* for_var: lhs  */
#line 3000 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 9021 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354: /* f_margs: f_arg  */
#line 3007 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 9029 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355: /* f_margs: f_arg ',' "*" f_norm_arg  */
#line 3011 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 9037 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356: /* f_margs: f_arg ',' "*" f_norm_arg ',' f_arg  */
#line 3015 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357: /* f_margs: f_arg ',' "*"  */
#line 3019 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3((yyvsp[-2].nd), nint(-1), 0);
                    }
#line 9054 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358: /* f_margs: f_arg ',' "*" ',' f_arg  */
#line 3024 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), nint(-1), (yyvsp[0].nd));
                    }
#line 9062 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359: /* f_margs: "*" f_norm_arg  */
#line 3028 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 9070 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360: /* f_margs: "*" f_norm_arg ',' f_arg  */
#line 3032 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 9078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361: /* f_margs: "*"  */
#line 3036 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3(0, nint(-1), 0);
                    }
#line 9087 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362: /* $@24: %empty  */
#line 3041 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                    }
#line 9095 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363: /* f_margs: "*" ',' $@24 f_arg  */
#line 3045 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, nint(-1), (yyvsp[0].nd));
                    }
#line 9103 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364: /* block_args_tail: f_block_kwarg ',' f_kwrest opt_f_block_arg  */
#line 3051 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9111 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365: /* block_args_tail: f_block_kwarg opt_f_block_arg  */
#line 3055 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9119 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366: /* block_args_tail: f_kwrest opt_f_block_arg  */
#line 3059 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9127 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367: /* block_args_tail: f_block_arg  */
#line 3063 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9135 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368: /* opt_block_args_tail: ',' block_args_tail  */
#line 3069 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9143 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369: /* opt_block_args_tail: %empty  */
#line 3073 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9151 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3079 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9159 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371: /* block_param: f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3083 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9167 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372: /* block_param: f_arg ',' f_block_optarg opt_block_args_tail  */
#line 3087 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9175 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373: /* block_param: f_arg ',' f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3091 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9183 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374: /* block_param: f_arg ',' f_rest_arg opt_block_args_tail  */
#line 3095 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9191 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375: /* block_param: f_arg ',' opt_block_args_tail  */
#line 3099 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9199 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376: /* block_param: f_arg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3103 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9207 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377: /* block_param: f_arg opt_block_args_tail  */
#line 3107 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9215 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378: /* block_param: f_block_optarg ',' f_rest_arg opt_block_args_tail  */
#line 3111 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9223 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379: /* block_param: f_block_optarg ',' f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3115 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9231 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380: /* block_param: f_block_optarg opt_block_args_tail  */
#line 3119 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9239 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381: /* block_param: f_block_optarg ',' f_arg opt_block_args_tail  */
#line 3123 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9247 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382: /* block_param: f_rest_arg opt_block_args_tail  */
#line 3127 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9255 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383: /* block_param: f_rest_arg ',' f_arg opt_block_args_tail  */
#line 3131 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9263 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384: /* block_param: block_args_tail  */
#line 3135 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9271 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385: /* opt_block_param: none  */
#line 3141 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 9280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386: /* opt_block_param: block_param_def  */
#line 3146 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9289 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387: /* $@25: %empty  */
#line 3152 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p);}
#line 9295 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388: /* block_param_def: '|' $@25 opt_bv_decl '|'  */
#line 3153 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9303 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389: /* block_param_def: "||"  */
#line 3157 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p);
                      (yyval.nd) = 0;
                    }
#line 9312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390: /* block_param_def: '|' block_param opt_bv_decl '|'  */
#line 3162 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 9320 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 391: /* opt_bv_decl: opt_nl  */
#line 3169 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9328 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392: /* opt_bv_decl: opt_nl ';' bv_decls opt_nl  */
#line 3173 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9336 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395: /* bvar: "local variable or method"  */
#line 3183 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 9345 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397: /* f_larglist: '(' f_args opt_bv_decl ')'  */
#line 3191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 9353 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398: /* f_larglist: f_args  */
#line 3195 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9361 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399: /* lambda_body: tLAMBEG compstmt '}'  */
#line 3201 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9369 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400: /* lambda_body: "'do' for lambda" bodystmt "'end'"  */
#line 3205 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9377 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401: /* @26: %empty  */
#line 3211 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 9387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402: /* do_block: "'do' for block" @26 opt_block_param bodystmt "'end'"  */
#line 3219 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 9398 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403: /* block_call: command do_block  */
#line 3228 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (typen((yyvsp[-1].nd)->car) == NODE_YIELD) {
                        yyerror(&(yylsp[-1]), p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9412 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404: /* block_call: block_call call_op2 operation2 opt_paren_args  */
#line 3238 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 9420 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405: /* block_call: block_call call_op2 operation2 opt_paren_args brace_block  */
#line 3242 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 9429 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406: /* block_call: block_call call_op2 operation2 command_args do_block  */
#line 3247 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 9438 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407: /* method_call: operation paren_args  */
#line 3254 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 9446 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408: /* method_call: primary_value call_op operation2 opt_paren_args  */
#line 3258 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 9454 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409: /* method_call: primary_value "::" operation2 paren_args  */
#line 3262 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 9462 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410: /* method_call: primary_value "::" operation3  */
#line 3266 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 9470 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411: /* method_call: primary_value call_op paren_args  */
#line 3270 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 9478 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412: /* method_call: primary_value "::" paren_args  */
#line 3274 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), tCOLON2);
                    }
#line 9486 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413: /* method_call: "'super'" paren_args  */
#line 3278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 9494 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414: /* method_call: "'super'"  */
#line 3282 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 9502 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415: /* method_call: primary_value '[' opt_call_args ']'  */
#line 3286 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 9510 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416: /* @27: %empty  */
#line 3292 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 9520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417: /* brace_block: '{' @27 opt_block_param compstmt '}'  */
#line 3299 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 9531 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418: /* @28: %empty  */
#line 3306 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 9541 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419: /* brace_block: "'do'" @28 opt_block_param bodystmt "'end'"  */
#line 3313 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 9552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 420: /* case_body: "'when'" args then compstmt cases  */
#line 3324 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 9560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 421: /* cases: opt_else  */
#line 3330 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 9573 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423: /* opt_rescue: "'rescue'" exc_list exc_var then compstmt opt_rescue  */
#line 3344 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 9582 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 425: /* exc_list: arg  */
#line 3352 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9590 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 428: /* exc_var: "=>" lhs  */
#line 3360 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9598 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 430: /* opt_ensure: "'ensure'" compstmt  */
#line 3367 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9606 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 437: /* string: string string_fragment  */
#line 3381 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = concat_string(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9614 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440: /* string_fragment: "string literal" tSTRING  */
#line 3389 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9622 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441: /* string_fragment: "string literal" string_rep tSTRING  */
#line 3393 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      else {
                        cons_free((yyvsp[0].nd));
                      }
                      (yyval.nd) = new_dstr(p, n);
                    }
#line 9637 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443: /* string_rep: string_rep string_interp  */
#line 3407 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9645 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444: /* string_interp: tSTRING_MID  */
#line 3413 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9653 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445: /* @29: %empty  */
#line 3417 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 9661 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446: /* string_interp: tSTRING_PART @29 compstmt '}'  */
#line 3422 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p,(yyvsp[-2].nd));
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 9670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447: /* string_interp: tLITERAL_DELIM  */
#line 3427 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 9678 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 448: /* string_interp: tHD_LITERAL_DELIM heredoc_bodies  */
#line 3431 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 9686 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 449: /* xstring: tXSTRING_BEG tXSTRING  */
#line 3437 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450: /* xstring: tXSTRING_BEG string_rep tXSTRING  */
#line 3441 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      else {
                        cons_free((yyvsp[0].nd));
                      }
                      (yyval.nd) = new_dxstr(p, n);
                    }
#line 9709 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 451: /* regexp: tREGEXP_BEG tREGEXP  */
#line 3454 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9717 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 452: /* regexp: tREGEXP_BEG string_rep tREGEXP  */
#line 3458 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9725 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456: /* heredoc_body: tHEREDOC_END  */
#line 3471 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 9735 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457: /* heredoc_body: heredoc_string_rep tHEREDOC_END  */
#line 3477 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 9743 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460: /* heredoc_string_interp: tHD_STRING_MID  */
#line 3487 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 9753 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461: /* @30: %empty  */
#line 3493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 9761 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 462: /* heredoc_string_interp: tHD_STRING_PART @30 compstmt '}'  */
#line 3498 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p, (yyvsp[-2].nd));
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(push(info->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 9771 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 463: /* words: tWORDS_BEG tSTRING  */
#line 3506 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 9779 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 464: /* words: tWORDS_BEG string_rep tSTRING  */
#line 3510 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      else {
                        cons_free((yyvsp[0].nd));
                      }
                      (yyval.nd) = new_words(p, n);
                    }
#line 9794 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465: /* symbol: basic_symbol  */
#line 3524 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 9802 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 466: /* symbol: "symbol" "string literal" string_rep tSTRING  */
#line 3528 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      p->lstate = EXPR_ENDARG;
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      else {
                        cons_free((yyvsp[0].nd));
                      }
                      (yyval.nd) = new_dsym(p, new_dstr(p, n));
                    }
#line 9818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467: /* symbol: "symbol" "numbered parameter"  */
#line 3540 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[0].num));
                      (yyval.nd) = new_sym(p, sym);
                    }
#line 9827 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 468: /* basic_symbol: "symbol" sym  */
#line 3547 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_END;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9836 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473: /* sym: tSTRING  */
#line 3558 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 9844 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474: /* sym: "string literal" tSTRING  */
#line 3562 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 9852 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475: /* symbols: tSYMBOLS_BEG tSTRING  */
#line 3568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 9860 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 476: /* symbols: tSYMBOLS_BEG string_rep tSTRING  */
#line 3572 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      (yyval.nd) = new_symbols(p, n);
                    }
#line 9872 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479: /* numeric: tUMINUS_NUM "integer literal"  */
#line 3584 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 9880 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480: /* numeric: tUMINUS_NUM "float literal"  */
#line 3588 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 9888 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481: /* variable: "local variable or method"  */
#line 3594 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 9896 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482: /* variable: "instance variable"  */
#line 3598 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 9904 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483: /* variable: "global variable"  */
#line 3602 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 9912 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484: /* variable: "class variable"  */
#line 3606 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 9920 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485: /* variable: "constant"  */
#line 3610 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 9928 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486: /* var_lhs: variable  */
#line 3616 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 9936 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487: /* var_lhs: "numbered parameter"  */
#line 3620 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "can't assign to numbered parameter");
                    }
#line 9944 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488: /* var_ref: variable  */
#line 3626 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 9952 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489: /* var_ref: "numbered parameter"  */
#line 3630 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 9960 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 490: /* var_ref: "'nil'"  */
#line 3634 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9968 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491: /* var_ref: "'self'"  */
#line 3638 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 9976 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492: /* var_ref: "'true'"  */
#line 3642 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 9984 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 493: /* var_ref: "'false'"  */
#line 3646 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 9992 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 494: /* var_ref: "'__FILE__'"  */
#line 3650 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 10004 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 495: /* var_ref: "'__LINE__'"  */
#line 3658 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 10015 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 496: /* var_ref: "'__ENCODING__'"  */
#line 3665 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, MRB_SYM_2(p->mrb, __ENCODING__), 0);
                    }
#line 10023 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499: /* superclass: %empty  */
#line 3675 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10031 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 500: /* $@31: %empty  */
#line 3679 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 10040 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 501: /* superclass: '<' $@31 expr_value term  */
#line 3684 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10048 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504: /* f_arglist_paren: '(' f_args rparen  */
#line 3700 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 10058 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 505: /* f_arglist_paren: '(' f_arg ',' tBDOT3 rparen  */
#line 3706 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 10066 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506: /* f_arglist_paren: '(' tBDOT3 rparen  */
#line 3710 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 10074 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508: /* f_arglist: f_args term  */
#line 3717 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10082 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509: /* f_arglist: f_arg ',' tBDOT3 term  */
#line 3721 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 10090 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510: /* f_arglist: "..." term  */
#line 3725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 10098 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511: /* f_label: "local variable or method" "label"  */
#line 3731 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 10106 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512: /* f_label: "numbered parameter" "label"  */
#line 3735 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 10114 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513: /* f_kw: f_label arg  */
#line 3741 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 10124 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514: /* f_kw: f_label  */
#line 3747 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 10133 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515: /* f_block_kw: f_label primary_value  */
#line 3754 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 10143 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516: /* f_block_kw: f_label  */
#line 3760 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 10152 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517: /* f_block_kwarg: f_block_kw  */
#line 3767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10160 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518: /* f_block_kwarg: f_block_kwarg ',' f_block_kw  */
#line 3771 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10168 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 519: /* f_kwarg: f_kw  */
#line 3777 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10176 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 520: /* f_kwarg: f_kwarg ',' f_kw  */
#line 3781 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10184 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523: /* f_kwrest: kwrest_mark "local variable or method"  */
#line 3791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, (yyvsp[0].id));
                    }
#line 10192 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524: /* f_kwrest: kwrest_mark  */
#line 3795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, 0);
                    }
#line 10200 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525: /* args_tail: f_kwarg ',' f_kwrest opt_f_block_arg  */
#line 3801 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 10208 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526: /* args_tail: f_kwarg opt_f_block_arg  */
#line 3805 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 10216 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527: /* args_tail: f_kwrest opt_f_block_arg  */
#line 3809 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 10224 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528: /* args_tail: f_block_arg  */
#line 3813 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 10232 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529: /* opt_args_tail: ',' args_tail  */
#line 3819 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 10240 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530: /* opt_args_tail: %empty  */
#line 3823 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 10248 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531: /* f_args: f_arg ',' f_optarg ',' f_rest_arg opt_args_tail  */
#line 3829 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10256 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532: /* f_args: f_arg ',' f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 3833 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10264 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533: /* f_args: f_arg ',' f_optarg opt_args_tail  */
#line 3837 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 10272 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534: /* f_args: f_arg ',' f_optarg ',' f_arg opt_args_tail  */
#line 3841 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535: /* f_args: f_arg ',' f_rest_arg opt_args_tail  */
#line 3845 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536: /* f_args: f_arg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 3849 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537: /* f_args: f_arg opt_args_tail  */
#line 3853 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10304 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538: /* f_args: f_optarg ',' f_rest_arg opt_args_tail  */
#line 3857 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539: /* f_args: f_optarg ',' f_rest_arg ',' f_arg opt_args_tail  */
#line 3861 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10320 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540: /* f_args: f_optarg opt_args_tail  */
#line 3865 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 10328 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541: /* f_args: f_optarg ',' f_arg opt_args_tail  */
#line 3869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10336 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542: /* f_args: f_rest_arg opt_args_tail  */
#line 3873 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 10344 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543: /* f_args: f_rest_arg ',' f_arg opt_args_tail  */
#line 3877 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 10352 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544: /* f_args: args_tail  */
#line 3881 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 10360 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545: /* f_args: %empty  */
#line 3885 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 10369 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546: /* f_bad_arg: "constant"  */
#line 3892 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 10378 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547: /* f_bad_arg: "instance variable"  */
#line 3897 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 10387 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548: /* f_bad_arg: "global variable"  */
#line 3902 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 10396 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549: /* f_bad_arg: "class variable"  */
#line 3907 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 10405 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550: /* f_bad_arg: "numbered parameter"  */
#line 3912 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(&(yylsp[0]), p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 10414 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551: /* f_norm_arg: f_bad_arg  */
#line 3919 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 10422 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552: /* f_norm_arg: "local variable or method"  */
#line 3923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 10431 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553: /* f_arg_item: f_norm_arg  */
#line 3930 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 10439 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554: /* @32: %empty  */
#line 3934 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 10447 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555: /* f_arg_item: tLPAREN @32 f_margs rparen  */
#line 3938 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn_param(p, (yyvsp[-1].nd), p->locals->car);
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 10457 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 556: /* f_arg: f_arg_item  */
#line 3946 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10465 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557: /* f_arg: f_arg ',' f_arg_item  */
#line 3950 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10473 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558: /* f_opt_asgn: "local variable or method" '='  */
#line 3956 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 10483 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559: /* f_opt: f_opt_asgn arg  */
#line 3964 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 10493 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560: /* f_block_opt: f_opt_asgn primary_value  */
#line 3972 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 10503 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561: /* f_block_optarg: f_block_opt  */
#line 3980 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10511 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562: /* f_block_optarg: f_block_optarg ',' f_block_opt  */
#line 3984 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10519 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 563: /* f_optarg: f_opt  */
#line 3990 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 10527 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 564: /* f_optarg: f_optarg ',' f_opt  */
#line 3994 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10535 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 567: /* f_rest_arg: restarg_mark "local variable or method"  */
#line 4004 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 10544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 568: /* f_rest_arg: restarg_mark  */
#line 4009 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(mul);
                      local_add_f(p, (yyval.id));
                    }
#line 10553 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 571: /* f_block_arg: blkarg_mark "local variable or method"  */
#line 4020 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 10561 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572: /* f_block_arg: blkarg_mark  */
#line 4024 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(and);
                    }
#line 10569 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573: /* opt_f_block_arg: ',' f_block_arg  */
#line 4030 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 10577 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574: /* opt_f_block_arg: none  */
#line 4034 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 10585 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 575: /* singleton: var_ref  */
#line 4040 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 10595 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 576: /* $@33: %empty  */
#line 4045 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 10601 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 577: /* singleton: '(' $@33 expr rparen  */
#line 4046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      prohibit_literals(p, (yyvsp[-1].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10610 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 579: /* assoc_list: assocs trailer  */
#line 4054 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 10618 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 580: /* assocs: assoc  */
#line 4060 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 10627 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 581: /* assocs: assocs comma assoc  */
#line 4065 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10635 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 582: /* assoc: arg "=>" arg  */
#line 4071 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 10645 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 583: /* assoc: "local variable or method" "label" arg  */
#line 4077 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 10654 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 584: /* assoc: "local variable or method" "label"  */
#line 4082 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), label_reference(p, (yyvsp[-1].id)));
                    }
#line 10662 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 585: /* assoc: "numbered parameter" "label"  */
#line 4086 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[-1].num));
                      (yyval.nd) = cons(new_sym(p, sym), label_reference(p, sym));
                    }
#line 10671 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 586: /* assoc: "numbered parameter" "label" arg  */
#line 4091 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, intern_numparam((yyvsp[-2].num))), (yyvsp[0].nd));
                    }
#line 10680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 587: /* assoc: string_fragment "label" arg  */
#line 4096 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if (typen((yyvsp[-2].nd)->car) == NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 10694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 588: /* assoc: "**" arg  */
#line 4106 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 10703 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 589: /* assoc: "**"  */
#line 4111 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), new_lvar(p, intern_op(pow)));
                    }
#line 10711 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 602: /* call_op: '.'  */
#line 4137 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 10719 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 603: /* call_op: "&."  */
#line 4141 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 10727 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 605: /* call_op2: "::"  */
#line 4148 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 10735 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 614: /* term: ';'  */
#line 4169 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 10741 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 616: /* nl: '\n'  */
#line 4174 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 10750 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 620: /* none: %empty  */
#line 4186 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 10762 "mrbgems/mruby-compiler/core/y.tab.c"

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

#line 4190 "mrbgems/mruby-compiler/core/parse.y"

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

  c = intn(n->car);

  if (c == NODE_NTH_REF) {
    yyerror_c(p, "can't set variable $", (char)intn(n->cdr)+'0');
  }
  else if (c == NODE_BACK_REF) {
    yyerror_c(p, "can't set variable $", (char)intn(n->cdr));
  }
  else {
    yyerror(NULL, p, "Internal error in backref_error()");
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
    yyerror(NULL, p, "void value expression");
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
      pair->car = (node*)newstr;
      pair->cdr = (node*)newlen;
    }
    else {
      spaces = (size_t)nspaces->car;
      heredoc_count_indent(hinfo, str, len, spaces, &offset);
      pair->car = (node*)(str + offset);
      pair->cdr = (node*)(len - offset);
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
      node *nd = new_str(p, tok(p), toklen(p));
      pylval.nd = nd;
      if (unindent && head) {
        nspaces = push(nspaces, nint(spaces));
        heredoc_push_indented(p, hinfo, nd->cdr, escaped, nspaces, empty && line_head);
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
        node *nd = new_str(p, tok(p), toklen(p));
        pylval.nd = nd;
        if (hinfo) {
          if (unindent && head) {
            nspaces = push(nspaces, nint(spaces));
            heredoc_push_indented(p, hinfo, nd->cdr, escaped, nspaces, FALSE);
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
  p->lstate = EXPR_END;
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
    pylval.nd = new_str(p, tok(p), toklen(p));
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
          nvar = intn(p->nvars->car);
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
  if (intn(p->tree->car) != NODE_SCOPE) return;
  n0 = n = p->tree->cdr->car;
  while (n) {
    i++;
    n = n->cdr;
  }
  cxt->syms = (mrb_sym*)mrbc_realloc(cxt->syms, i*sizeof(mrb_sym));
  cxt->slen = i;
  for (i=0, n=n0; n; i++,n=n->cdr) {
    cxt->syms[i] = sym(n->car);
  }
}

void mrb_parser_dump(mrb_state *mrb, node *tree, int offset);

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
      mrb_parser_dump(p->mrb, p->tree, 0);
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
    mrb_int binsize;
    uint8_t *bin;
    mrb_value bin_obj = mrb_nil_value(); /* temporary string object */
    mrb_value result;

    binsize = bin_to_uint32(leading.h.binary_size);
    bin_obj = mrb_str_new(mrb, NULL, binsize);
    bin = (uint8_t*)RSTRING_PTR(bin_obj);
    if ((size_t)binsize > bufsize)  {
      memcpy(bin, leading.b, bufsize);
      if (fread(bin + bufsize, binsize - bufsize, 1, fp) == 0) {
        binsize = bufsize;
        /* The error is reported by mrb_load_irep_buf_cxt() */
      }
    }

    result = mrb_load_irep_buf_cxt(mrb, bin, binsize, c);
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
    mrb_sym rest = sym(n->car);

    dump_prefix(n, offset+1);
    if (rest == MRB_OPSYM(mul))
      printf("rest=*\n");
    else
      printf("rest=*%s\n", mrb_sym_name(mrb, rest));
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
    dump_prefix(tree, offset+1);
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
        if (tree->cdr->car) {
          dump_prefix(tree, offset+1);
          printf("kwargs:\n");
          mrb_parser_dump(mrb, tree->cdr->car, offset+2);
        }
        if (tree->cdr->cdr) {
          dump_prefix(tree, offset+1);
          printf("block:\n");
          mrb_parser_dump(mrb, tree->cdr->cdr, offset+2);
        }
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
        if (n2 && n2->car) {
          dump_prefix(n2, offset+2);
          printf("post:\n");
          dump_recur(mrb, n2->car, offset+3);
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
    printf("NODE_ZSUPER:\n");
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
    printf("NODE_REGX /%s/\n", (char*)tree->car);
    if (tree->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("opt: %s\n", (char*)tree->cdr->car);
    }
    if (tree->cdr->cdr) {
      dump_prefix(tree, offset+1);
      printf("enc: %s\n", (char*)tree->cdr->cdr);
    }
    break;

  case NODE_DREGX:
    printf("NODE_DREGX:\n");
    dump_recur(mrb, tree->car, offset+1);
    dump_prefix(tree, offset+1);
    printf("tail: %s\n", (char*)tree->cdr->cdr->car);
    if (tree->cdr->cdr->cdr->car) {
      dump_prefix(tree, offset+1);
      printf("opt: %s\n", (char*)tree->cdr->cdr->cdr->car);
    }
    if (tree->cdr->cdr->cdr->cdr) {
      dump_prefix(tree, offset+1);
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
    if (tree)
      printf("NODE_KW_REST_ARGS %s\n", mrb_sym_name(mrb, sym(tree)));
    else
      printf("NODE_KW_REST_ARGS\n");
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
