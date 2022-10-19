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
#include <mruby/internal.h>
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
  void *m = mrb_pool_alloc(p->pool, size);

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
          yyerror(p, "duplicated argument name");
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

static void
local_add_blk(parser_state *p, mrb_sym blk)
{
  /* allocate register for block */
  local_add_f(p, blk ? blk : 0);
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
  if (c) {
    if (c->cdr) {
      if (c->cdr->cdr) {
        yyerror(p, "both block arg and actual block given");
      }
      if (c->cdr->car) {
        return cons((node*)NODE_YIELD, push(c->car, c->cdr->car));
      }
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
  return new_call(p, new_const(p, MRB_SYM_2(p->mrb, Kernel)), MRB_SYM_2(p->mrb, Complex),
                  new_callargs(p, list2(list3((node*)NODE_INT, (node*)strdup("0"), nint(10)), imaginary), 0, 0), '.');
}

static node*
new_rational(parser_state *p, node *rational)
{
  return new_call(p, new_const(p, MRB_SYM_2(p->mrb, Kernel)), MRB_SYM_2(p->mrb, Rational), new_callargs(p, list1(rational), 0, 0), '.');
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
      yyerror(p, "both block arg and actual block given");
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
    yyerror(p, "block argument should not be given");
    return NULL;
  }
  if (!n->car) return NULL;
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

static node*
label_reference(parser_state *p, mrb_sym sym)
{
  const char *name = mrb_sym_name(p->mrb, sym);
  node *n;

  if (local_var_p(p, sym)) {
    n = new_lvar(p, sym);
  }
  else if (ISUPPER(name[0])) {
    n = new_const(p, sym);
  }
  else {
    n = new_fcall(p, sym, 0);
  }
  return n;
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
  if (nd == NULL)
    return NULL;
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

/* xxx ----------------------------- */


#line 1507 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 1449 "mrbgems/mruby-compiler/core/parse.y"

    node *nd;
    mrb_sym id;
    int num;
    stack_type stack;
    const struct vtable *vars;

#line 1687 "mrbgems/mruby-compiler/core/y.tab.c"

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
#define YYLAST   13092

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  149
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  176
/* YYNRULES -- Number of rules.  */
#define YYNRULES  618
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  1084

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
       0,  1620,  1620,  1620,  1631,  1637,  1641,  1646,  1650,  1656,
    1658,  1657,  1671,  1698,  1704,  1708,  1713,  1717,  1723,  1723,
    1727,  1731,  1735,  1739,  1743,  1747,  1751,  1756,  1757,  1761,
    1765,  1769,  1773,  1779,  1782,  1786,  1790,  1794,  1798,  1802,
    1807,  1811,  1820,  1829,  1838,  1847,  1854,  1855,  1859,  1863,
    1864,  1868,  1872,  1876,  1880,  1884,  1894,  1893,  1908,  1917,
    1918,  1921,  1922,  1929,  1928,  1943,  1947,  1952,  1956,  1961,
    1965,  1970,  1974,  1978,  1982,  1986,  1992,  1996,  2002,  2003,
    2009,  2013,  2017,  2021,  2025,  2029,  2033,  2037,  2041,  2045,
    2051,  2052,  2058,  2062,  2068,  2072,  2078,  2082,  2086,  2090,
    2094,  2098,  2104,  2110,  2117,  2121,  2125,  2129,  2133,  2137,
    2143,  2149,  2154,  2160,  2164,  2167,  2171,  2175,  2182,  2183,
    2184,  2185,  2190,  2197,  2198,  2201,  2205,  2205,  2211,  2212,
    2213,  2214,  2215,  2216,  2217,  2218,  2219,  2220,  2221,  2222,
    2223,  2224,  2225,  2226,  2227,  2228,  2229,  2230,  2231,  2232,
    2233,  2234,  2235,  2236,  2237,  2238,  2239,  2240,  2243,  2243,
    2243,  2244,  2244,  2245,  2245,  2245,  2246,  2246,  2246,  2246,
    2247,  2247,  2247,  2248,  2248,  2248,  2249,  2249,  2249,  2249,
    2250,  2250,  2250,  2250,  2251,  2251,  2251,  2251,  2252,  2252,
    2252,  2252,  2253,  2253,  2253,  2253,  2254,  2254,  2257,  2261,
    2265,  2269,  2273,  2277,  2281,  2286,  2291,  2296,  2300,  2304,
    2308,  2312,  2316,  2320,  2324,  2328,  2332,  2336,  2340,  2344,
    2348,  2352,  2356,  2360,  2364,  2368,  2372,  2376,  2380,  2384,
    2388,  2392,  2396,  2400,  2404,  2408,  2412,  2416,  2420,  2424,
    2428,  2432,  2436,  2440,  2444,  2453,  2462,  2471,  2480,  2486,
    2487,  2492,  2496,  2503,  2507,  2514,  2518,  2527,  2544,  2545,
    2548,  2549,  2550,  2555,  2560,  2567,  2573,  2578,  2583,  2588,
    2595,  2595,  2606,  2610,  2616,  2620,  2626,  2629,  2635,  2639,
    2644,  2649,  2655,  2660,  2664,  2670,  2671,  2672,  2673,  2674,
    2675,  2676,  2677,  2681,  2686,  2685,  2697,  2701,  2696,  2706,
    2706,  2710,  2714,  2718,  2722,  2727,  2732,  2736,  2740,  2744,
    2748,  2752,  2753,  2759,  2766,  2758,  2779,  2787,  2795,  2795,
    2795,  2802,  2802,  2802,  2809,  2815,  2820,  2822,  2819,  2831,
    2829,  2847,  2852,  2845,  2869,  2867,  2883,  2893,  2904,  2908,
    2912,  2916,  2922,  2929,  2930,  2931,  2934,  2935,  2938,  2939,
    2947,  2948,  2954,  2958,  2961,  2965,  2969,  2973,  2978,  2982,
    2986,  2990,  2996,  2995,  3005,  3009,  3013,  3017,  3023,  3028,
    3033,  3037,  3041,  3045,  3049,  3053,  3057,  3061,  3065,  3069,
    3073,  3077,  3081,  3085,  3089,  3095,  3100,  3107,  3107,  3111,
    3116,  3123,  3127,  3133,  3134,  3137,  3142,  3145,  3149,  3155,
    3159,  3166,  3165,  3180,  3190,  3194,  3199,  3206,  3210,  3214,
    3218,  3222,  3226,  3230,  3234,  3238,  3245,  3244,  3259,  3258,
    3274,  3282,  3291,  3294,  3301,  3304,  3308,  3309,  3312,  3316,
    3319,  3323,  3326,  3327,  3328,  3329,  3332,  3333,  3339,  3340,
    3341,  3345,  3358,  3359,  3365,  3370,  3369,  3379,  3383,  3389,
    3393,  3406,  3410,  3416,  3419,  3420,  3423,  3429,  3435,  3436,
    3439,  3446,  3445,  3458,  3462,  3476,  3481,  3495,  3501,  3502,
    3503,  3504,  3505,  3509,  3515,  3519,  3529,  3530,  3531,  3535,
    3541,  3545,  3549,  3553,  3557,  3563,  3567,  3573,  3577,  3581,
    3585,  3589,  3593,  3601,  3608,  3614,  3615,  3619,  3623,  3622,
    3639,  3640,  3643,  3649,  3653,  3659,  3660,  3664,  3668,  3674,
    3678,  3684,  3690,  3697,  3703,  3710,  3714,  3720,  3724,  3730,
    3731,  3734,  3738,  3744,  3748,  3752,  3756,  3762,  3767,  3772,
    3776,  3780,  3784,  3788,  3792,  3796,  3800,  3804,  3808,  3812,
    3816,  3820,  3824,  3829,  3835,  3840,  3845,  3850,  3855,  3862,
    3866,  3873,  3878,  3877,  3889,  3893,  3899,  3907,  3915,  3923,
    3927,  3933,  3937,  3943,  3944,  3947,  3952,  3959,  3960,  3963,
    3967,  3973,  3977,  3983,  3988,  3988,  4013,  4014,  4020,  4025,
    4031,  4037,  4042,  4046,  4051,  4056,  4066,  4071,  4077,  4078,
    4079,  4082,  4083,  4084,  4085,  4088,  4089,  4090,  4093,  4094,
    4097,  4101,  4107,  4108,  4114,  4115,  4118,  4119,  4122,  4125,
    4126,  4127,  4130,  4131,  4134,  4139,  4142,  4143,  4147
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

#define YYPACT_NINF (-868)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-619)

#define yytable_value_is_error(Yyn) \
  ((Yyn) == YYTABLE_NINF)

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -868,   115,  3515,  -868,  8231, 10355, 10697,  6539,  -868, 10001,
   10001,  -868,  -868, 10469,  7721,  6155,  8467,  8467,  -868,  -868,
    8467,  4172,  3764,  -868,  -868,  -868,  -868,    18,  7721,  -868,
      38,  -868,  -868,  -868,  6681,  3628,  -868,  -868,  6823,  -868,
    -868,  -868,  -868,  -868,  -868,  -868,    45, 10119, 10119, 10119,
   10119,   217,  5414,   792,  8939,  9293,  8003,  -868,  7439,  1134,
     894,    67,  1139,  1203,  -868,    94, 10237, 10119,  -868,   780,
    -868,  1204,  -868,   529,  1741,  1741,  -868,  -868,   271,   173,
    -868,   184, 10583,  -868,   272, 12773,   579,   668,   228,    75,
    -868,   381,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,
    -868,    48,   278,  -868,   300,   129,  -868,  -868,  -868,  -868,
    -868,   243,   243,    18,   110,   646,  -868, 10001,   360,  5533,
     517,  1837,  1837,  -868,   279,  -868,   756,  -868,  -868,   129,
    -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,
    -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,
    -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,    33,    89,
     108,   109,  -868,  -868,  -868,  -868,  -868,  -868,   117,   154,
     192,   199,  -868,   208,  -868,  -868,  -868,  -868,  -868,  -868,
    -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,
    -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,
    -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,   290,
    4592,   354,   529,  1741,  1741,    77,   305, 12897,   801,   159,
     347,   193,    77, 10001, 10001,   866,   397,  -868,  -868,   890,
     433,    78,    85,  -868,  -868,  -868,  -868,  -868,  -868,  -868,
    -868,  -868,  7580,  -868,  -868,   320,  -868,  -868,  -868,  -868,
    -868,  -868,   780,  -868,   944,  -868,   444,  -868,  -868,   780,
    3900,   124, 10119, 10119, 10119, 10119,  -868, 12835,  -868,  -868,
     324,   428,   324,  -868,  -868,  -868,  8585,  -868,  -868,  -868,
    8467,  -868,  -868,  -868,  6155,  6393,  -868,   367,  5652,  -868,
     974,   391, 12959, 12959,   355,  8349,  5414,   378,   780,  1204,
     780,   416,  -868,  8349,   780,   408,   714,   714,  -868, 12835,
     423,   714,  -868,   500, 10811,   429,   988,   999,  1035,  1982,
    -868,  -868,  -868,  -868,  1241,  -868,  -868,  -868,  -868,  -868,
    -868,   981,  1248,  -868,  -868,  1126,  -868,  1051,  -868,  1252,
    -868,  1265,   480,   482,  -868,  -868,  -868,  -868,  5917, 10001,
   10001, 10001, 10001,  8349, 10001, 10001,    93,  -868,  -868,  -868,
    -868,   536,   780,  -868,  -868,  -868,  -868,  -868,  -868,  -868,
    2113,   475,   479,  4592, 10119,  -868,   463,   563,   476,  -868,
     780,  -868,  -868,  -868,   483, 10119,  -868,   487,   576,   489,
     581,  -868,  -868,   521,  4592,  -868,  -868,  9411,  -868,  5414,
    8117,   503,  9411, 10119, 10119, 10119, 10119, 10119, 10119, 10119,
   10119, 10119, 10119, 10119, 10119, 10119, 10119,   592, 10119, 10119,
   10119, 10119, 10119, 10119, 10119, 10119, 10119, 10119, 10119, 10119,
    3310,  -868,  8467,  -868, 11089,  -868,  -868, 12293,  -868,  -868,
    -868,  -868, 10237, 10237,  -868,   550,  -868,   529,  -868,  1036,
    -868,  -868,  -868,  -868,  -868,  -868, 11175,  8467, 11261,  4592,
   10001,  -868,  -868,  -868,   636,   642,   210,   538,   540,  -868,
    4738,   659, 10119, 11347,  8467, 11433, 10119, 10119,  5030,   620,
     620,   134, 11519,  8467, 11605,  -868,   616,  -868,  5652,   444,
    -868,  -868,  9529,   665,  -868, 10119, 10119, 12897, 12897, 12897,
   10119,  -868,  -868,  8703,  -868, 10119,  -868,  9057,  6274,   546,
     780,   324,   324,  -868,  -868,   313,   547,  -868,  -868,  -868,
    7721,  5149,   557, 11347, 11433, 10119,  1204,   780,  -868,  -868,
    6036,   555,  1204,  -868,  -868,  9175,  -868,   780,  9293,  -868,
    -868,  -868,  1036,   184, 10811,  -868, 10811, 11691,  8467, 11777,
    2314,  -868,  -868,   561,  -868,  1290,  5652,   981,  -868,  -868,
    -868,  -868,  -868,  -868,  -868, 10119, 10119,  -868,  -868,  -868,
    -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  1428,
     780,   780,   564, 10237,   699, 12897,   266,  -868,  -868,  -868,
     263,  -868,  -868,  2562,  -868, 12897,  2314,  -868,  -868,  2042,
    -868,  -868, 10237,   701,    65, 10119,  -868, 12489,   324,  -868,
     780, 10811,   566,  -868,  -868,  -868,   674,   591,  2410,  -868,
    -868,  1037,   220,  3100,  3100,  3100,  3100,  1377,  1377,  3375,
    2808,  3100,  3100, 12959, 12959,  1279,  1279,  -868,   391, 12897,
    1377,  1377,  1483,  1483,  1288,   383,   383,   391,   391,   391,
    2930,  7179,  4308,  7297,  -868,   243,  -868,   584,   324,   452,
    -868,   516,  -868,  -868,  4036,  -868,  -868,  1908,    65,    65,
    -868,  3027,  -868,  -868,  -868,  -868,  -868,   780, 10001,  4592,
     697,   530,  -868,   243,   587,   243,   707,   313,  7862,  -868,
    9647,   715,  -868, 10119, 10119,   572,  -868,  6941,  7060,   596,
     223,   285,   715,  -868,  -868,  -868,  -868,   116,   121,   603,
     135,   136, 10001,  7721,   609,   734, 12897,   206,  -868, 12897,
   12897, 12897,   281, 10119, 12835,  -868,   324, 12897,  -868,  -868,
    -868,  -868,  8821,  9057,  -868,  -868,  -868,   611,  -868,  -868,
     211,  1204,   780,   714,   503,  -868,   697,   530,   613,   860,
     904,  -868,    36,  2314,  -868,   618,  -868,   391,   391,  -868,
    -868,   812,   780,   619,  -868,  -868,  2628,   711, 12365,  -868,
     706,   536,  -868,   476,  -868,   780,  -868,  -868,   623,   627,
     628,  -868,   630,   706,   628,   731, 12427,  -868,  -868,  2314,
    4592,  -868,  -868, 12560,  9765,  -868,  -868, 10811,  8349, 10237,
   10119, 11863,  8467, 11949,   413, 10237, 10237,  -868,   550,   560,
    8703, 10237, 10237,  -868,   550,    75,   271,  4592,  5652,    65,
    -868,   780,   766,  -868,  -868,  -868,  -868, 12489,  -868,   689,
    -868,  5295,   771,  -868, 10001,   774,  -868, 10119, 10119,   333,
   10119, 10119,   776,  5798,  5798,   148,   620,  -868,  -868,  -868,
    9883,  4884, 12897,  -868,  6274,   324,  -868,  -868,  -868,    88,
     647,   990,  4592,  5652,  -868,  -868,  -868,   653,  -868,  1554,
     780, 10119, 10119,  -868,  -868,  2314,  -868,  2042,  -868,  2042,
    -868,  2042,  -868,  -868, 10119, 10119,  -868,  -868,  -868, 10925,
    -868,   655,   476,   657, 10925,  -868,   661,   667,  -868,   786,
   10119, 12631,  -868,  -868, 12897,  3179,  4444,   670,   406,   426,
   10119, 10119,  -868,  -868,  -868,  -868,  -868, 10237,  -868,  -868,
    -868,  -868,  -868,  -868,  -868,   803,   677,  5652,  4592,  -868,
    -868, 11039,    77,  -868,  -868,  5798,  -868,  -868,    77,  -868,
   10119,  -868,   808,   809,  -868, 12897,   219,  -868,  9057,  -868,
    1629,   813,   682,  1445,  1445,  1019,  -868, 12897, 12897,   628,
     694,   628,   628, 12897, 12897,   705,   712,   788,  1054,   266,
    -868,  -868,  1801,  -868,  1054,  2314,  -868,  2042,  -868,  -868,
   12702,   432, 12897, 12897,  -868,  -868,  -868,  -868,   718,   830,
     795,  -868,  1083,   999,  1035,  4592,  -868,  4738,  -868,  -868,
    5798,  -868,  -868,  -868,  -868,   722,  -868,  -868,  -868,  -868,
     727,   727,  1445,   733,  -868,  2042,  -868,  -868,  -868,  -868,
    -868,  -868, 12035,  -868,   476,   266,  -868,  -868,   736,   746,
     750,  -868,   751,   750,  -868,  -868,  1036, 12121,  8467, 12207,
     642,   572,   879,  1629,   281,  1445,   727,  1445,   628,   757,
     761,  -868,  2314,  -868,  2042,  -868,  2042,  -868,  2042,  -868,
    -868,   697,   530,   772,   201,   467,  -868,  -868,  -868,  -868,
     727,  -868,   750,   781,   750,   750,    88,  -868,  2042,  -868,
    -868,  -868,   750,  -868
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,     0,     1,     0,     0,     0,     0,   294,     0,
       0,   318,   321,     0,     0,   604,   338,   339,   340,   341,
     306,   270,   270,   489,   488,   490,   491,   606,     0,    10,
       0,   493,   492,   494,   480,   590,   482,   481,   484,   483,
     476,   477,   438,   439,   495,   496,   292,     0,     0,     0,
       0,     0,     0,   296,   618,   618,    88,   313,     0,     0,
       0,     0,     0,     0,   453,     0,     0,     0,     3,   604,
       6,     9,    27,    33,   543,   543,    49,    60,    59,     0,
      76,     0,    80,    90,     0,    54,   248,     0,    61,   311,
     285,   286,   436,   287,   288,   289,   434,   433,   465,   435,
     432,   487,     0,   290,   291,   270,     5,     8,   338,   339,
     306,   618,   414,     0,   113,   114,   292,     0,     0,     0,
       0,   543,   543,   116,   497,   342,     0,   487,   291,     0,
     334,   168,   178,   169,   165,   194,   195,   196,   197,   176,
     191,   184,   174,   173,   189,   172,   171,   167,   192,   166,
     179,   183,   185,   177,   170,   186,   193,   188,   187,   180,
     190,   175,   164,   182,   181,   163,   161,   162,   158,   159,
     160,   118,   120,   119,   153,   154,   131,   132,   133,   140,
     137,   139,   134,   135,   155,   156,   141,   142,   146,   149,
     150,   136,   138,   128,   129,   130,   143,   144,   145,   147,
     148,   151,   152,   157,   574,    55,   121,   122,   573,     0,
       0,     0,    58,   543,   543,     0,     0,    54,     0,   487,
       0,   291,     0,     0,     0,   112,     0,   353,   352,     0,
       0,   487,   291,   187,   180,   190,   175,   158,   159,   160,
     118,   119,     0,   123,   125,    20,   124,   456,   461,   460,
     612,   614,   604,   615,     0,   458,     0,   616,   613,   605,
     588,   292,   278,   587,   273,     0,   265,   277,    74,   269,
     618,   436,   618,   578,    75,    73,   618,   259,   307,    72,
       0,   258,   413,    71,   604,     0,    18,     0,     0,   221,
       0,   222,   209,   212,   303,     0,     0,     0,   604,    15,
     604,    78,    14,     0,   604,     0,   609,   609,   249,     0,
       0,   609,   576,     0,     0,    86,     0,    96,   103,   543,
     470,   469,   471,   472,     0,   468,   467,   440,   445,   444,
     447,     0,     0,   442,   449,     0,   451,     0,   463,     0,
     474,     0,   478,   479,    53,   236,   237,     4,   605,     0,
       0,     0,     0,     0,     0,     0,   550,   546,   545,   544,
     547,   548,     0,   552,   564,   519,   520,   568,   567,   563,
     543,     0,   505,     0,   512,   517,   618,   522,   618,   542,
       0,   549,   551,   554,   528,     0,   561,   528,   566,   528,
     570,   526,   501,     0,     0,   401,   403,     0,    92,     0,
      84,    81,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   208,   211,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   601,   618,   600,     0,   603,   602,     0,   418,   416,
     312,   437,     0,     0,   407,    65,   310,   331,   113,   114,
     115,   478,   479,   505,   498,   329,     0,   618,     0,     0,
       0,   599,   598,    56,     0,   618,   303,     0,     0,   344,
       0,   343,     0,     0,   618,     0,     0,     0,     0,     0,
       0,   303,     0,   618,     0,   326,     0,   126,     0,     0,
     457,   459,     0,     0,   617,   582,   583,   279,   586,   272,
       0,   606,   266,     0,   275,     0,   267,     0,   604,     0,
     604,   618,   618,   260,   271,   604,     0,   309,    52,   607,
       0,     0,     0,     0,     0,     0,    17,   604,   301,    13,
     605,    77,   297,   300,   304,   611,   250,   610,   611,   252,
     305,   577,   102,    94,     0,    89,     0,     0,   618,     0,
     543,   314,   398,   528,   473,     0,     0,   448,   454,   441,
     443,   450,   452,   464,   475,     0,     0,     7,    21,    22,
      23,    24,    25,    50,    51,   509,   556,   510,   508,     0,
     604,   604,   528,     0,     0,   511,     0,   524,   572,   521,
       0,   525,   506,     0,   535,   557,     0,   538,   565,     0,
     540,   569,     0,     0,   618,   278,    28,    30,     0,    31,
     604,     0,    82,    93,    48,    34,    46,     0,   253,   198,
      29,     0,   291,   226,   231,   232,   233,   228,   230,   240,
     241,   234,   235,   207,   210,   238,   239,    32,   218,   606,
     227,   229,   223,   224,   225,   213,   214,   215,   216,   217,
     591,   596,   592,   597,   412,   270,   410,     0,   618,   591,
     593,   592,   594,   411,   270,   591,   592,   270,   618,   618,
      35,   253,   199,    45,   206,    63,    66,     0,     0,     0,
     113,   114,   117,     0,     0,   618,     0,   604,     0,   295,
     618,   618,   424,     0,     0,   618,   345,   595,   302,     0,
     591,   592,   618,   347,   319,   346,   322,   595,   302,     0,
     591,   592,     0,     0,     0,     0,   277,     0,   325,   581,
     584,   580,   276,     0,   280,   274,   618,   585,   579,   257,
     255,   261,   262,   264,   308,   608,    19,     0,    26,   205,
      79,    16,   604,   609,    95,    87,    99,   101,     0,    98,
     100,   606,     0,     0,   466,     0,   455,   219,   220,   550,
     548,   361,   604,   354,   504,   502,     0,    41,   244,   336,
       0,     0,   518,   618,   571,     0,   527,   555,   528,   528,
     528,   562,   528,   550,   528,    43,   246,   337,   389,   387,
       0,   386,   385,   284,     0,    91,    85,     0,     0,     0,
       0,     0,   618,     0,     0,     0,     0,   409,    69,   415,
     262,     0,     0,   408,    67,   404,    62,     0,     0,   618,
     332,     0,     0,   415,   335,   575,    57,   425,   426,   618,
     427,     0,   618,   350,     0,     0,   348,     0,     0,   415,
       0,     0,     0,     0,     0,   415,     0,   127,   462,   324,
       0,     0,   281,   268,   604,   618,    11,   298,   251,    97,
       0,   391,     0,     0,   315,   446,   362,   359,   553,     0,
     604,     0,     0,   523,   507,     0,   531,     0,   533,     0,
     539,     0,   536,   541,     0,     0,   384,   606,   606,   514,
     515,   618,   618,   369,     0,   559,   369,   369,   367,     0,
       0,   282,    83,    47,   254,   591,   592,     0,   591,   592,
       0,     0,    40,   203,    39,   204,    70,     0,    37,   201,
      38,   202,    68,   405,   406,     0,     0,     0,     0,   499,
     330,     0,     0,   429,   351,     0,    12,   431,     0,   316,
       0,   317,     0,     0,   327,   280,   618,   256,   263,   397,
       0,     0,     0,     0,     0,   357,   503,    42,   245,   528,
     528,   528,   528,    44,   247,     0,     0,     0,   513,     0,
     365,   366,   369,   377,   558,     0,   380,     0,   382,   402,
     283,   415,   243,   242,    36,   200,   419,   417,     0,     0,
       0,   428,     0,   104,   111,     0,   430,     0,   320,   323,
       0,   421,   422,   420,   395,   606,   393,   396,   400,   399,
     363,   360,     0,   355,   532,     0,   529,   534,   537,   390,
     388,   303,     0,   516,   618,     0,   368,   375,   369,   369,
     369,   560,   369,   369,    64,   333,   110,     0,   618,     0,
     618,   618,     0,     0,   392,     0,   358,     0,   528,   595,
     302,   364,     0,   372,     0,   374,     0,   381,     0,   378,
     383,   107,   109,     0,   591,   592,   423,   349,   328,   394,
     356,   530,   369,   369,   369,   369,   105,   373,     0,   370,
     376,   379,   369,   371
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -868,  -868,  -868,   411,  -868,    25,  -868,  -282,   693,  -868,
      42,  -868,  -254,  -213,   768,  1343,  1513,  -868,    84,   -59,
    -868,  -429,  -868,   -14,   916,  -190,     4,   -33,  -271,  -487,
     -11,  1993,   -84,   936,    29,   -19,  -868,  -868,    19,  -868,
     867,  -868,  -392,    46,  -461,  -327,   118,    -7,  -868,  -446,
    -233,  -184,    15,  -360,    57,  -868,  -868,  -868,  -868,  -868,
    -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,  -868,
    -868,     8,  -211,  -460,   -94,  -610,  -868,  -868,  -868,   163,
     501,  -868,  -572,  -868,  -868,  -276,  -868,   -90,  -868,  -868,
     145,  -868,  -868,  -868,   -81,  -868,  -868,  -451,  -868,   -78,
    -868,  -868,  -868,  -868,  -868,   147,    58,  -167,  -868,  -868,
    -868,  -868,  -868,  -248,  -868,   710,  -868,  -868,  -868,     2,
    -868,  -868,  -868,  2347,  2558,   960,  1777,  -868,  -868,   -27,
     502,    20,    -9,   396,    16,  -868,  -868,  -868,   181,   485,
     249,  -244,  -839,  -672,  -556,  -868,   180,  -723,  -541,  -867,
      14,  -513,  -868,  -388,  -868,   675,  -351,  -868,  -868,  -868,
      62,  -436,   624,  -330,  -868,  -868,   -47,  -868,     7,   -22,
     806,  -253,   394,  -284,   -65,    -2
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,    68,    69,    70,   287,   464,   465,   298,
     299,   520,    72,   615,    73,   213,   214,   688,   215,    76,
      77,   676,   819,    78,    79,   300,    80,    81,    82,   545,
      83,   216,   123,   124,   243,   244,   245,   713,   653,   207,
      85,   305,   619,   654,   278,   509,   510,   279,   280,   269,
     502,   538,   658,   609,    86,   210,   303,   742,   304,   319,
     752,   223,   843,   224,   844,   712,  1000,   679,   677,   928,
     459,   290,   470,   704,   835,   836,   230,   762,   953,  1026,
     973,   887,   790,   791,   888,   860,  1005,  1006,   551,   864,
     396,   604,    88,    89,   446,   669,   668,   493,  1003,   691,
     829,   932,   936,    90,    91,    92,   332,   333,   556,    93,
      94,    95,   557,   253,   254,   255,   488,    96,    97,    98,
     326,    99,   100,   219,   220,   103,   221,   455,   678,   371,
     372,   373,   374,   375,   890,   891,   376,   377,   378,   776,
     594,   380,   381,   382,   383,   579,   384,   385,   386,   895,
     896,   387,   388,   389,   390,   391,   587,   209,   460,   310,
     512,   273,   129,   683,   656,   463,   458,   437,   516,   861,
     517,   536,   257,   258,   259,   302
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     106,   519,   266,   266,   348,   285,   266,   344,   440,   286,
      87,   478,    87,   126,   126,   283,   246,   218,   218,   281,
     706,   229,   256,   218,   218,   218,   206,   591,   218,   107,
     246,   270,   270,   206,   450,   270,   205,   777,   715,   506,
     434,   436,   620,   544,    71,   315,    71,   206,   393,   401,
     672,   674,   308,   312,   539,   781,   301,   725,   541,   745,
      87,   725,   125,   125,   316,   862,   894,   277,   282,   306,
     125,   728,   392,   392,   218,   552,   347,   206,   272,   272,
     778,   832,   272,   558,   672,   674,   503,   325,   507,   867,
     316,   584,   842,   469,   222,   394,   817,   818,   445,   252,
     655,   438,   728,  -104,   664,   657,   527,   667,  1031,   281,
    -111,  1007,   603,   125,  -415,     3,   307,   311,   335,   337,
     339,   341,   535,  -489,   796,   218,   581,    87,   685,   336,
     684,   328,   329,   739,   268,   274,  -588,  -486,   275,   125,
    -485,  -107,   394,   655,   788,   664,  -109,   699,   614,   863,
     575,   444,   342,   343,   685,   438,   709,   277,   282,  -110,
    -106,  -108,   284,   271,   271,   560,  -112,   271,   560,  -104,
     560,   475,   560,  -105,   560,   444,   288,   686,  -415,  -488,
    -489,   496,   484,   330,   331,  1031,   467,   468,   614,   614,
     -96,   789,   247,  -415,   685,   248,   249,   777,  -490,  -491,
    -480,   271,   271,   813,  1007,   779,   815,  -493,   782,   610,
     777,   392,   392,   439,   576,  -480,  -486,  -106,    87,   685,
     -96,   748,   469,   250,   813,   251,  -415,  -103,  -415,   670,
     673,   218,   218,   530,   394,  -415,  -488,   831,   441,   492,
     778,   537,   537,   524,  -492,  -112,   537,   927,  -588,   894,
    -480,  -485,   894,   778,  -588,  -490,  -491,  -480,   -99,   489,
    -591,   206,   266,  -101,  -493,  -592,   266,   439,   504,   549,
     504,   325,   294,   276,   513,   544,  -102,   -98,  -100,   506,
    -104,   543,  -494,  1013,   218,   477,   725,   725,   218,  -480,
     -97,   511,   218,   218,   397,   270,    87,   728,  -484,   395,
     301,  -492,   525,    87,    87,   529,   751,   479,   480,   756,
     902,    87,   443,   777,  -111,   840,  -106,   770,   435,  -106,
    -106,   247,   316,   777,   248,   249,   398,   732,   733,   894,
     828,  -110,   -77,   431,   781,   771,  1001,   526,   606,  -494,
     544,  -111,   272,   616,  -106,   532,  -480,  -106,   501,  -106,
     354,   355,   250,   -91,   251,  -484,    87,   218,   218,   218,
     218,    87,   218,   218,   725,   779,   367,   365,   366,   367,
     442,   125,   682,   567,   588,   433,   588,   841,   779,   916,
     461,    87,   555,   616,   616,   922,   944,   276,   560,   612,
      71,   368,   443,   402,   368,   572,   247,   822,   514,   248,
     249,   897,    87,   301,   454,   218,  -108,    87,   316,   466,
     621,   448,   608,   913,   915,   449,   777,   608,   266,   919,
     921,  -111,   873,   271,   794,   940,   472,   271,   247,   251,
     513,   248,   249,   568,   569,   570,   571,   462,   519,   476,
     218,    42,  -103,   266,    43,   913,   915,   525,   919,   921,
     621,   621,   481,   662,  -105,   513,   662,   125,   485,   250,
     266,   251,   487,   692,   492,   218,   501,    87,   218,   266,
     910,   655,   513,   664,   810,   907,  -110,   662,    87,   722,
     663,   513,   218,  -108,   418,   505,    87,   960,    59,   777,
     858,   218,   418,   853,   662,  1002,    87,  -102,   811,   553,
     777,   736,   725,   662,   663,   521,   851,   717,   899,   504,
     504,   744,   728,   543,   427,   428,   429,   731,   812,   106,
     519,   663,   246,   528,   917,   985,   544,  -106,   247,    87,
     663,   248,   249,   850,   266,   925,   911,   -76,    87,   206,
     970,   971,   507,   662,   811,   614,   513,  -108,   985,   534,
     582,   614,   316,  -105,   316,   542,   218,   614,   614,   379,
     379,   251,   540,    71,    87,   726,   354,   355,   662,   767,
     663,   546,   741,  -106,   803,   451,   452,   519,   543,   271,
     951,   565,  -108,   566,  1029,  -108,  -108,  1032,   785,   834,
     831,   218,   912,   577,   -98,   663,   583,   743,   918,   920,
    -500,   125,   792,   125,   271,   586,   379,   379,   812,   471,
     218,   965,   966,  -108,   589,  -108,   471,   804,   590,   316,
    -302,   271,   853,   453,   453,   593,   105,   598,   105,   596,
     271,   599,   601,   105,   105,  -302,   597,  -108,   600,   105,
     105,   105,   602,   637,   105,   613,   989,   703,   808,   675,
     271,   689,   917,   494,   271,   690,   504,   814,  -100,   693,
     816,   694,   281,   614,  1073,   281,   792,   792,   125,  -342,
    -302,   948,  -589,  1051,  -592,   696,   105,  -302,   537,   554,
     718,  -105,   271,   281,  -342,   271,   218,    87,   830,   833,
     105,   730,   735,   833,   847,   271,   738,   -91,   379,   379,
     833,   807,   -97,   753,   984,   608,   766,   206,   797,   685,
     277,  1063,   799,   277,   769,   246,   787,   826,   798,  -342,
     218,   995,   824,  -595,   504,   809,  -342,   997,   823,   807,
     553,   277,   206,   831,   923,   247,  -484,   839,   248,   249,
     616,   105,   494,   105,   845,   297,   616,   914,   848,   849,
     856,  -484,   616,   616,   859,   871,   578,   865,   430,   763,
     519,   869,   821,   575,   543,   875,   250,   773,   251,   877,
     879,   588,   881,   431,   592,   884,   780,   212,   212,   784,
     889,   930,   931,   212,  -589,   935,  -484,  -595,   266,   939,
    -589,   941,   949,  -484,   855,   954,   846,   969,    87,   972,
     513,   979,  -595,   975,   379,   316,    87,   621,   432,   977,
     218,   981,   297,   621,   218,   433,   987,   792,   986,   621,
     621,  1009,   662,   998,   999,    87,    87,   933,  1008,   247,
     937,  1019,   248,   249,   105,  -595,  1015,  -595,  1020,    87,
     903,  -591,   218,  1021,  -595,  1035,   456,   105,   105,   663,
    1036,    87,    87,   504,   125,   379,   501,  1034,   616,    87,
     250,   431,   251,   759,  1043,   357,   358,   359,   360,  1045,
      87,    87,   855,   705,   705,  1047,   217,   217,  1052,   271,
     271,   760,   217,   267,   267,   447,  -591,   267,  1054,   588,
     588,   473,  1056,  1058,  1068,   247,   457,   968,   248,   249,
     105,  -591,   974,   433,   105,  -592,   431,  -299,   105,   105,
    -299,  -299,   105,  1076,   289,   291,   292,   293,   938,   105,
     105,   267,   309,  1078,   494,   621,   250,   105,   251,   227,
    -592,   494,   737,   345,   346,    87,    87,  -299,  -299,   992,
    -299,   474,   130,    87,   833,  1039,   125,  1067,   433,   271,
    -591,   125,   886,  1069,   866,   334,  -292,   271,   328,   329,
     889,   924,  1066,   889,   491,  -591,   889,   208,   889,   893,
     892,  -292,   105,   105,   105,   105,   105,   105,   105,   105,
     482,   522,   772,  1044,   217,  1023,  1028,     0,   125,   297,
       0,   212,   212,     0,  -592,   431,     0,   105,  -591,     0,
    -591,     0,     0,    87,  -591,    87,  -292,  -591,    87,  -592,
     330,   331,     0,  -292,     0,     0,   889,     0,   105,     0,
       0,   105,   588,   105,   266,     0,   105,   876,   878,   880,
     483,   882,     0,   883,     0,   379,   513,   433,   692,   833,
       0,     0,  -592,   889,  -592,   889,   218,   889,  -592,   889,
       0,  -592,   515,   518,     0,   959,   105,   961,   662,   490,
       0,   962,   248,   249,   523,     0,   105,   105,     0,   889,
     759,   820,   357,   358,   359,   360,     0,     0,   547,   431,
       0,   105,     0,   105,   105,   663,     0,     0,   760,  -487,
     217,   217,   297,   431,   105,   271,   247,     0,   105,   248,
     249,     0,   105,     0,  -487,   247,   531,   105,   248,   249,
     533,   471,   105,   562,   474,   328,   329,   212,   212,   212,
     212,   433,   573,   574,     0,  -291,  -303,   801,   548,   497,
     498,   499,   345,  1010,  1011,   433,   950,     0,   251,  -487,
    -291,  -303,   431,   267,  1022,   105,  -487,   267,     0,     0,
    1024,   217,   217,   892,   105,  1030,   892,  1033,   892,   431,
       0,  1012,     0,   695,     0,     0,     0,   330,   331,   874,
       0,   702,   105,  1037,     0,  -291,  -303,   802,     0,     0,
     105,   714,  -291,  -303,   433,   271,     0,   561,   431,     0,
     328,   329,  1046,     0,   457,  1048,     0,   327,   328,   329,
       0,   433,   338,   328,   329,     0,   892,   105,  1014,  1016,
    1017,  1018,     0,     0,     0,   929,   217,   217,   217,   217,
       0,   217,   217,  1038,     0,     0,   105,  1070,   687,     0,
     433,     0,  1072,   892,  1074,   892,     0,   892,  1075,   892,
     705,   585,   330,   331,   349,   350,   351,   352,   353,   755,
     330,   331,   595,     0,     0,   330,   331,     0,  1082,   892,
       0,   774,     0,     0,   607,   774,   340,   328,   329,   618,
     623,   624,   625,   626,   627,   628,   629,   630,   631,   632,
     633,   634,   635,   636,     0,   638,   639,   640,   641,   642,
     643,   644,   645,   646,   647,   648,   649,  1071,     0,   267,
       0,     0,   105,   105,   554,   328,   329,     0,     0,   671,
     671,   559,   328,   329,   729,   563,   328,   329,     0,   330,
     331,   734,     0,     0,   267,     0,   471,   217,   564,   328,
     329,     0,   471,   740,     0,     0,   105,     0,     0,   671,
       0,   267,     0,   671,   671,    74,     0,    74,   121,   121,
     267,     0,     0,   754,   328,   329,   121,   330,   331,   716,
       0,     0,   719,   720,   330,   331,     0,   721,   330,   331,
     724,     0,   727,     0,   309,   293,   415,   416,     0,     0,
     418,   330,   331,     0,     0,     0,   764,   765,     0,   418,
       0,     0,   671,     0,     0,    74,     0,   976,   978,   121,
       0,     0,   724,     0,     0,   309,   330,   331,   425,   426,
     427,   428,   429,     0,   105,   267,   795,   425,   426,   427,
     428,   429,   105,   105,     0,   121,   105,     0,     0,   105,
     105,     0,   757,   758,     0,   105,   105,     0,     0,     0,
       0,   105,   105,     0,     0,     0,   212,     0,     0,     0,
     768,     0,     0,     0,     0,   105,     0,     0,   105,     0,
       0,     0,    74,     0,   898,   415,   416,   105,   105,   786,
       0,     0,   793,  1027,     0,   105,     0,     0,   418,   759,
     212,   357,   358,   359,   360,     0,   105,   105,     0,     0,
       0,     0,     0,   825,     0,     0,   759,   760,   357,   358,
     359,   360,     0,   422,   423,   424,   425,   426,   427,   428,
     429,   926,     0,     0,   760,    75,     0,    75,   122,   122,
       0,     0,   363,     0,   934,     0,   122,     0,   761,  1053,
    1055,  1057,     0,  1059,  1060,     0,   942,   943,     0,   363,
       0,   105,     0,     0,   946,   217,     0,     0,   857,     0,
       0,   105,   105,    74,     0,     0,   952,   827,     0,   105,
     768,   786,     0,     0,     0,    75,     0,     0,   868,   122,
       0,   415,   416,  1077,  1079,  1080,  1081,     0,     0,   217,
       0,     0,     0,  1083,   418,     0,     0,     0,     0,     0,
     852,     0,     0,     0,     0,   122,     0,     0,     0,   724,
     309,     0,   212,     0,     0,   759,     0,   357,   358,   359,
     360,   424,   425,   426,   427,   428,   429,     0,     0,   105,
     988,   105,     0,   760,   105,     0,     0,     0,   996,     0,
       0,    74,    75,     0,     0,     0,     0,     0,    74,    74,
       0,     0,     0,     0,   774,     0,    74,   898,   363,     0,
     898,     0,   898,     0,   955,     0,     0,   121,     0,     0,
     947,   901,   105,     0,     0,     0,   671,   904,     0,   267,
       0,     0,   671,   671,     0,     0,   956,   724,   671,   671,
    1004,     0,   357,   358,   359,   360,     0,     0,  1040,     0,
    1041,    74,     0,  1042,     0,     0,    74,     0,   760,     0,
     898,   217,     0,     0,   671,   671,     0,   671,   671,     0,
       0,     0,     0,     0,     0,     0,    74,   945,     0,     0,
       0,   293,     0,    75,     0,     0,     0,   898,     0,   898,
       0,   898,     0,   898,     0,     0,     0,    74,   957,   958,
       0,     0,    74,   121,     0,    74,     0,     0,     0,     0,
       0,   963,   964,   898,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   980,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   982,   983,   104,
       0,   104,   128,   128,   671,    74,    74,     0,     0,     0,
     232,     0,   356,     0,   357,   358,   359,   360,     0,     0,
       0,    75,    74,     0,     0,     0,     0,   671,    75,    75,
     361,     0,     0,    74,     0,   309,    75,     0,     0,     0,
       0,    74,     0,     0,   362,     0,     0,   122,     0,   104,
       0,    74,     0,   318,     0,   363,     0,     0,     0,     0,
       0,   364,   365,   366,   367,     0,     0,     0,     0,     0,
       0,     0,   356,     0,   357,   358,   359,   360,     0,   318,
       0,    75,  -618,     0,    74,     0,    75,     0,     0,   368,
     361,     0,   369,    74,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   370,    75,   121,   356,   121,
     357,   358,   359,   360,     0,   363,   104,     0,     0,    74,
       0,   364,   365,   366,   367,   267,   361,    75,  -618,     0,
       0,     0,    75,   122,     0,    75,     0,     0,     0,     0,
     362,  -618,  -618,  -618,  -618,  -618,  -618,     0,  -618,   368,
       0,   363,   369,     0,  -618,  -618,     0,   364,   365,   366,
     367,     0,     0,  1025,     0,  -618,  -618,     0,  -618,  -618,
    -618,  -618,  -618,     0,   121,    75,    75,     0,     0,     0,
       0,     0,     0,     0,     0,   368,     0,     0,   369,     0,
       0,     0,    75,     0,     0,     0,     0,     0,     0,     0,
       0,   370,     0,    75,     0,     0,     0,   104,     0,     0,
       0,    75,     0,     0,     0,    84,     0,    84,  -618,     0,
       0,    75,     0,     0,     0,     0,   228,     0,     0,     0,
       0,     0,     0,  -618,     0,     0,     0,     0,     0,     0,
       0,     0,    74,  -618,     0,     0,  -618,  -618,     0,     0,
       0,     0,     0,   356,    75,   357,   358,   359,   360,     0,
       0,     0,     0,    75,     0,    84,  -618,  -618,     0,     0,
       0,   361,   276,  -618,  -618,  -618,  -618,   122,     0,   122,
       0,     0,     0,     0,     0,   104,     0,     0,     0,    75,
       0,     0,   104,   104,     0,     0,   363,     0,     0,     0,
     104,     0,   364,   365,   366,   367,     0,     0,     0,     0,
       0,   318,     0,   783,     0,   357,   358,   359,   360,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     368,   361,    84,   369,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   122,   104,   550,     0,     0,     0,
     104,     0,     0,    74,     0,     0,   363,     0,     0,     0,
     121,    74,    74,   365,   366,   367,     0,     0,    74,     0,
     104,     0,     0,     0,    74,    74,     0,     0,     0,     0,
      74,    74,     0,     0,   356,     0,   357,   358,   359,   360,
     368,   104,     0,     0,    74,     0,   104,   318,     0,   622,
       0,     0,   361,     0,     0,     0,    74,    74,     0,     0,
       0,     0,    75,     0,    74,     0,     0,     0,   580,     0,
       0,     0,     0,    84,     0,    74,    74,   363,     0,     0,
       0,     0,     0,   364,   365,   366,   367,     0,     0,   622,
     622,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   121,     0,     0,     0,   104,   121,     0,     0,
       0,   368,     0,     0,   369,     0,     0,   104,     0,     0,
       0,     0,     0,     0,     0,   104,     0,     0,     0,     0,
      74,     0,     0,     0,     0,   104,     0,     0,     0,     0,
      74,    74,     0,     0,   121,     0,     0,     0,    74,     0,
       0,    84,     0,     0,     0,     0,     0,     0,    84,    84,
       0,     0,     0,     0,     0,     0,    84,     0,   104,     0,
       0,     0,     0,    75,     0,     0,     0,   104,     0,     0,
     122,    75,    75,     0,     0,     0,     0,     0,    75,     0,
       0,   318,     0,   318,    75,    75,     0,     0,     0,     0,
      75,    75,     0,   104,     0,     0,     0,     0,    74,     0,
      74,    84,     0,    74,    75,     0,    84,     0,     0,   101,
       0,   101,   127,   127,   127,     0,    75,    75,     0,     0,
     231,     0,     0,     0,    75,   356,    84,   357,   358,   359,
     360,     0,     0,     0,     0,    75,    75,     0,     0,     0,
       0,     0,     0,   361,     0,     0,     0,    84,   318,     0,
       0,     0,    84,     0,     0,   617,     0,     0,     0,   101,
       0,     0,   122,   317,     0,     0,     0,   122,   363,     0,
       0,     0,     0,     0,   364,   365,   366,   367,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   317,
      75,     0,     0,     0,     0,   617,   617,     0,     0,     0,
      75,    75,   368,     0,   122,   369,     0,     0,    75,     0,
       0,     0,    84,     0,   800,     0,   104,     0,     0,     0,
       0,     0,     0,    84,     0,     0,   101,     0,     0,     0,
       0,    84,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    84,   403,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,     0,     0,     0,     0,   415,   416,
       0,     0,     0,     0,     0,     0,     0,     0,    75,     0,
      75,   418,     0,    75,    84,     0,     0,     0,     0,     0,
       0,     0,     0,    84,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,   420,   421,   422,   423,   424,   425,
     426,   427,   428,   429,     0,     0,     0,     0,     0,    84,
       0,     0,  -277,     0,     0,     0,     0,   101,     0,     0,
     102,     0,   102,     0,     0,     0,     0,   104,     0,     0,
       0,     0,     0,     0,   318,   104,   622,     0,     0,     0,
       0,     0,   622,     0,     0,     0,     0,     0,   622,   622,
       0,     0,     0,     0,   104,   104,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   104,     0,
     102,     0,     0,   356,     0,   357,   358,   359,   360,     0,
     104,   104,     0,     0,     0,     0,     0,     0,   104,     0,
       0,   361,     0,     0,     0,   101,     0,     0,     0,   104,
     104,     0,   101,   101,     0,     0,     0,   775,     0,     0,
     101,     0,     0,     0,     0,     0,   363,     0,     0,     0,
       0,   317,   364,   365,   366,   367,   128,     0,     0,     0,
       0,   128,    84,     0,     0,     0,     0,   102,     0,   356,
       0,   357,   358,   359,   360,     0,     0,     0,     0,     0,
     368,     0,     0,   369,   622,   101,     0,   361,     0,     0,
     101,     0,     0,     0,   104,   104,     0,     0,   994,     0,
       0,     0,   104,   870,     0,     0,     0,     0,     0,     0,
     101,     0,   363,     0,     0,     0,     0,     0,   364,   365,
     366,   367,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   101,     0,     0,     0,     0,   101,   317,     0,     0,
       0,     0,     0,     0,     0,     0,   368,     0,     0,   369,
       0,     0,     0,     0,     0,     0,     0,     0,   102,     0,
       0,     0,   104,     0,   104,     0,     0,   104,     0,     0,
       0,     0,     0,    84,     0,     0,     0,     0,     0,     0,
       0,    84,   617,     0,     0,     0,     0,     0,   617,     0,
       0,     0,     0,     0,   617,   617,   101,     0,     0,     0,
      84,    84,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,     0,     0,    84,   101,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   101,    84,    84,     0,     0,
       0,     0,     0,     0,    84,     0,   102,     0,     0,     0,
       0,     0,     0,   102,   102,    84,    84,     0,     0,     0,
       0,   102,     0,     0,     0,     0,     0,     0,   101,     0,
       0,     0,     0,     0,     0,     0,     0,   101,     0,     0,
     403,   404,   405,   406,   407,   408,   409,     0,   411,   412,
       0,   317,     0,   317,     0,     0,   415,   416,     0,     0,
       0,     0,     0,   101,     0,     0,   102,     0,     0,   418,
     617,   102,     0,     0,     0,     0,     0,     0,     0,     0,
      84,    84,     0,     0,   991,     0,     0,     0,    84,     0,
    -595,   102,   420,   421,   422,   423,   424,   425,   426,   427,
     428,   429,     0,  -595,  -595,  -595,     0,  -595,  -595,     0,
    -595,     0,   102,     0,     0,     0,  -595,   102,   317,     0,
     102,     0,     0,     0,     0,     0,     0,  -595,  -595,     0,
    -595,  -595,  -595,  -595,  -595,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    84,     0,
      84,     0,     0,    84,     0,     0,     0,     0,     0,     0,
     102,   102,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,  -595,     0,     0,     0,   102,  -595,  -595,
    -595,     0,   805,  -595,     0,     0,   101,     0,   102,     0,
       0,  -595,     0,     0,     0,  -595,   102,     0,     0,     0,
       0,     0,     0,     0,     0,  -595,   102,     0,  -595,  -595,
       0,  -107,  -595,     0,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,  -595,     0,     0,     0,     0,  -595,  -595,
    -595,   800,   -99,     0,     0,  -595,  -595,  -595,  -595,   102,
       0,     0,     0,     0,     0,     0,     0,     0,   102,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   403,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,     0,     0,     0,   102,   415,   416,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   418,     0,
       0,     0,     0,     0,     0,     0,     0,   101,     0,     0,
       0,     0,     0,     0,   317,   101,     0,     0,     0,   419,
       0,   420,   421,   422,   423,   424,   425,   426,   427,   428,
     429,     0,     0,     0,   101,   101,     0,     0,     0,     0,
       0,     0,  -619,  -619,  -619,  -619,   407,   408,   101,  -595,
    -619,  -619,     0,     0,     0,     0,     0,     0,   415,   416,
     101,   101,  -595,  -595,  -595,     0,  -595,  -595,   101,  -595,
       0,   418,     0,     0,     0,  -595,     0,     0,     0,   101,
     101,     0,     0,     0,     0,     0,  -595,  -595,     0,  -595,
    -595,  -595,  -595,  -595,   420,   421,   422,   423,   424,   425,
     426,   427,   428,   429,     0,     0,   127,   102,     0,     0,
       0,   127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,     0,     0,     0,     0,  -595,  -595,  -595,
       0,   805,  -595,     0,   101,   101,     0,     0,   993,     0,
    -595,     0,   101,     0,  -595,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -595,     0,     0,  -595,  -595,     0,
    -107,  -595,     0,  -595,  -595,  -595,  -595,  -595,  -595,  -595,
    -595,  -595,  -595,     0,     0,     0,     0,  -595,  -595,  -595,
       0,  -595,     0,     0,  -595,  -595,  -595,  -595,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   101,     0,   101,     0,     0,   101,   102,     0,
       0,     0,     0,     0,     0,     0,   102,   102,     0,     0,
       0,   650,   651,   102,     0,   652,     0,     0,     0,   102,
     102,     0,     0,     0,     0,   102,   102,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,   102,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,   102,   102,     0,     0,     0,     0,     0,     0,   102,
     188,   189,   190,     0,     0,     0,     0,     0,     0,     0,
     102,   102,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,   403,   404,   405,
     406,   407,   408,   203,   276,   411,   412,     0,     0,     0,
       0,     0,     0,   415,   416,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   102,   418,     0,     0,     0,
       0,     0,     0,     0,     0,   102,   102,     0,     0,     0,
       0,     0,     0,   102,     0,     0,     0,     0,     0,   420,
     421,   422,   423,   424,   425,   426,   427,   428,   429,     0,
       0,     0,     0,     0,     0,  -618,     4,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,     0,
       0,     0,     0,     0,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,   102,    27,   102,     0,     0,   102,     0,
      28,    29,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,  -293,    64,
    -618,     0,     0,  -618,  -618,     0,     0,     0,     0,     0,
       0,  -293,  -293,  -293,  -293,  -293,  -293,     0,  -293,    65,
      66,    67,     0,     0,     0,  -293,  -293,  -293,     0,     0,
       0,  -618,     0,  -618,     0,  -293,  -293,     0,  -293,  -293,
    -293,  -293,  -293,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -293,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,
    -293,  -293,     0,     0,     0,     0,  -293,  -293,  -293,     0,
       0,  -293,     0,     0,     0,     0,     0,  -293,     0,  -293,
       0,     0,     0,  -293,     0,     0,     0,     0,     0,     0,
       0,  -293,     0,  -293,     0,     0,  -293,  -293,     0,     0,
    -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,  -293,
    -293,  -293,     0,     0,  -414,     0,     0,  -293,  -293,  -293,
    -293,     0,     0,  -293,  -293,  -293,  -293,  -414,  -414,  -414,
    -414,  -414,  -414,     0,  -414,     0,     0,     0,     0,     0,
    -414,  -414,  -414,     0,     0,     0,     0,     0,     0,     0,
       0,  -414,  -414,     0,  -414,  -414,  -414,  -414,  -414,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -414,  -414,  -414,  -414,
    -414,  -414,  -414,  -414,  -414,  -414,  -414,  -414,     0,     0,
       0,     0,  -414,  -414,  -414,     0,     0,  -414,     0,     0,
       0,     0,     0,  -414,     0,  -414,     0,     0,     0,  -414,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -414,
       0,     0,  -414,  -414,     0,     0,  -414,     0,  -414,  -414,
    -414,  -414,  -414,  -414,  -414,  -414,  -414,  -414,     0,     0,
    -480,     0,  -414,  -414,  -414,  -414,  -414,     0,   276,  -414,
    -414,  -414,  -414,  -480,  -480,  -480,  -480,  -480,  -480,     0,
    -480,     0,     0,     0,     0,     0,     0,  -480,  -480,     0,
       0,     0,     0,     0,     0,     0,     0,  -480,  -480,     0,
    -480,  -480,  -480,  -480,  -480,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   495,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -480,  -480,  -480,  -480,  -480,  -480,  -480,  -480,
    -480,  -480,  -480,  -480,     0,     0,     0,     0,  -480,  -480,
    -480,     0,  -480,  -480,     0,     0,     0,     0,     0,  -480,
       0,  -480,     0,     0,     0,  -480,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -480,     0,     0,  -480,  -480,
       0,  -480,  -480,     0,  -480,  -480,  -480,  -480,  -480,  -480,
    -480,  -480,  -480,  -480,     0,     0,  -618,     0,     0,  -480,
    -480,  -480,  -480,     0,     0,  -480,  -480,  -480,  -480,  -618,
    -618,  -618,  -618,  -618,  -618,     0,  -618,     0,     0,     0,
       0,     0,  -618,  -618,  -618,     0,     0,     0,     0,     0,
       0,     0,     0,  -618,  -618,     0,  -618,  -618,  -618,  -618,
    -618,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -618,  -618,
    -618,  -618,  -618,  -618,  -618,  -618,  -618,  -618,  -618,  -618,
       0,     0,     0,     0,  -618,  -618,  -618,     0,     0,  -618,
       0,     0,     0,     0,     0,  -618,     0,  -618,     0,     0,
       0,  -618,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -618,     0,     0,  -618,  -618,     0,     0,  -618,     0,
    -618,  -618,  -618,  -618,  -618,  -618,  -618,  -618,  -618,  -618,
       0,     0,  -618,     0,  -618,  -618,  -618,  -618,  -618,     0,
     276,  -618,  -618,  -618,  -618,  -618,  -618,  -618,  -618,  -618,
    -618,     0,  -618,     0,     0,     0,     0,     0,     0,  -618,
    -618,     0,     0,     0,     0,     0,     0,     0,     0,  -618,
    -618,     0,  -618,  -618,  -618,  -618,  -618,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -618,  -618,  -618,  -618,  -618,  -618,
    -618,  -618,  -618,  -618,  -618,  -618,     0,     0,     0,     0,
    -618,  -618,  -618,     0,     0,  -618,     0,     0,     0,     0,
       0,  -618,     0,  -618,     0,     0,     0,  -618,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -618,     0,     0,
    -618,  -618,     0,     0,  -618,     0,  -618,  -618,  -618,  -618,
    -618,  -618,  -618,  -618,  -618,  -618,     0,     0,  -302,     0,
       0,  -618,  -618,  -618,  -618,     0,   276,  -618,  -618,  -618,
    -618,  -302,  -302,  -302,     0,  -302,  -302,     0,  -302,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -302,  -302,     0,  -302,  -302,
    -302,  -302,  -302,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,     0,     0,     0,     0,  -302,  -302,  -302,     0,
     806,  -302,     0,     0,     0,     0,     0,     0,     0,  -302,
       0,     0,     0,  -302,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -302,     0,     0,  -302,  -302,     0,  -109,
    -302,     0,  -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
    -302,  -302,     0,     0,  -302,     0,     0,  -302,  -302,     0,
    -101,     0,     0,  -302,  -302,  -302,  -302,  -302,  -302,  -302,
       0,  -302,  -302,     0,  -302,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -302,  -302,     0,  -302,  -302,  -302,  -302,  -302,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -302,  -302,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,     0,     0,
       0,     0,  -302,  -302,  -302,     0,   806,  -302,     0,     0,
       0,     0,     0,     0,     0,  -302,     0,     0,     0,  -302,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -302,
       0,     0,  -302,  -302,     0,  -109,  -302,     0,  -302,  -302,
    -302,  -302,  -302,  -302,  -302,  -302,  -302,  -302,     0,     0,
       0,     0,     0,  -302,  -302,     0,  -302,     0,     0,  -302,
    -302,  -302,  -302,   295,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,  -618,  -618,  -618,     0,     0,
    -618,    15,     0,    16,    17,    18,    19,     0,     0,     0,
       0,     0,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,    51,     0,     0,    52,    53,     0,    54,
      55,     0,    56,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,  -618,     0,     0,
    -618,  -618,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,    67,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -618,   295,
    -618,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -618,     0,  -618,  -618,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,    51,
       0,     0,    52,    53,     0,    54,    55,     0,    56,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,  -618,     0,     0,  -618,  -618,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    65,    66,    67,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -618,   295,  -618,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,  -618,
       0,     0,  -618,    15,  -618,    16,    17,    18,    19,     0,
       0,     0,     0,     0,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,    51,     0,     0,    52,    53,
       0,    54,    55,     0,    56,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,  -618,
       0,     0,  -618,  -618,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    65,    66,
      67,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -618,   295,  -618,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,     0,     0,  -618,     0,     0,  -618,    15,
       0,    16,    17,    18,    19,     0,     0,     0,     0,     0,
      20,    21,    22,    23,    24,    25,    26,     0,     0,    27,
       0,     0,     0,     0,     0,    28,     0,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,     0,    40,    41,
      42,     0,     0,    43,     0,     0,    44,    45,     0,    46,
      47,    48,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,    49,    50,     0,     0,     0,     0,
       0,    51,     0,     0,    52,    53,     0,    54,    55,     0,
      56,     0,     0,     0,    57,     0,    58,    59,    60,     0,
      61,    62,    63,     0,    64,  -618,     0,     0,  -618,  -618,
       4,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,    65,    66,    67,     0,    15,     0,
      16,    17,    18,    19,     0,     0,  -618,     0,  -618,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,    29,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,  -618,     0,     0,  -618,  -618,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    65,    66,    67,     0,     0,  -618,     0,
       0,     0,     0,     0,     0,  -618,   295,  -618,     5,     6,
       7,     8,     9,    10,    11,    12,    13,    14,     0,  -618,
    -618,     0,     0,     0,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
      28,     0,    30,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,    51,     0,     0,    52,
      53,     0,    54,    55,     0,    56,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
    -618,     0,     0,  -618,  -618,   295,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,     0,     0,    65,
      66,    67,     0,    15,     0,    16,    17,    18,    19,     0,
       0,  -618,     0,  -618,    20,    21,    22,    23,    24,    25,
      26,     0,     0,    27,     0,     0,     0,     0,     0,    28,
       0,    30,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,    51,     0,     0,   296,    53,
       0,    54,    55,     0,    56,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,  -618,
       0,     0,  -618,  -618,   295,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,    14,     0,     0,    65,    66,
      67,     0,    15,     0,    16,    17,    18,    19,     0,  -618,
    -618,     0,  -618,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,    28,     0,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,    51,     0,     0,    52,    53,     0,
      54,    55,     0,    56,     0,     0,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,  -618,     0,
       0,  -618,  -618,   295,     0,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,     0,     0,    65,    66,    67,
       0,    15,     0,    16,    17,    18,    19,     0,  -618,  -618,
       0,  -618,    20,    21,    22,    23,    24,    25,    26,     0,
       0,    27,     0,     0,     0,     0,     0,    28,     0,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,     0,
      40,    41,    42,     0,     0,    43,     0,     0,    44,    45,
       0,    46,    47,    48,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    49,    50,     0,     0,
       0,     0,     0,    51,     0,     0,    52,    53,     0,    54,
      55,     0,    56,     0,     0,     0,    57,     0,    58,    59,
      60,     0,    61,    62,    63,     0,    64,  -618,     0,     0,
    -618,  -618,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    65,    66,    67,     0,
       0,  -618,     0,     0,     0,     0,     0,     0,  -618,   295,
    -618,     5,     6,     7,     8,     9,    10,    11,    12,    13,
      14,     0,     0,  -618,     0,     0,     0,    15,     0,    16,
      17,    18,    19,     0,     0,     0,     0,     0,    20,    21,
      22,    23,    24,    25,    26,     0,     0,    27,     0,     0,
       0,     0,     0,    28,     0,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,     0,    40,    41,    42,     0,
       0,    43,     0,     0,    44,    45,     0,    46,    47,    48,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,    49,    50,     0,     0,     0,     0,     0,    51,
       0,     0,    52,    53,     0,    54,    55,     0,    56,     0,
       0,     0,    57,     0,    58,    59,    60,     0,    61,    62,
      63,     0,    64,  -618,     0,     0,  -618,  -618,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
       0,     0,    65,    66,    67,     0,    15,     0,    16,    17,
      18,    19,     0,     0,  -618,     0,  -618,    20,    21,    22,
      23,    24,    25,    26,     0,     0,    27,     0,     0,     0,
       0,     0,    28,    29,    30,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,    51,     0,
       0,    52,    53,     0,    54,    55,     0,    56,     0,     0,
       0,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,   247,     0,     0,   248,   249,     0,     0,     5,
       6,     7,     8,     9,    10,    11,    12,    13,    14,     0,
       0,    65,    66,    67,     0,    15,     0,    16,    17,    18,
      19,     0,     0,   250,     0,   251,    20,    21,    22,    23,
      24,    25,    26,     0,     0,    27,     0,     0,     0,     0,
       0,    28,     0,    30,    31,    32,    33,    34,    35,    36,
      37,    38,    39,     0,    40,    41,    42,     0,     0,    43,
       0,     0,    44,    45,     0,    46,    47,    48,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
      49,    50,     0,     0,     0,     0,     0,    51,     0,     0,
      52,    53,     0,    54,    55,     0,    56,     0,     0,     0,
      57,     0,    58,    59,    60,     0,    61,    62,    63,     0,
      64,   247,     0,     0,   248,   249,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
      65,    66,    67,     0,    15,     0,    16,    17,    18,    19,
       0,     0,   250,     0,   251,    20,    21,    22,    23,    24,
      25,    26,     0,     0,    27,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      53,     0,    54,    55,     0,     0,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
     247,     0,     0,   248,   249,     0,     0,     5,     6,     7,
       8,     9,    10,    11,    12,    13,     0,     0,     0,    65,
      66,    67,     0,    15,     0,   108,   109,    18,    19,     0,
       0,   250,     0,   251,   110,   111,   112,    23,    24,    25,
      26,     0,     0,   113,     0,     0,     0,     0,     0,     0,
       0,     0,    31,    32,    33,    34,    35,    36,    37,    38,
      39,     0,    40,    41,    42,     0,     0,    43,     0,     0,
      44,    45,     0,    46,    47,    48,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    49,    50,
       0,     0,     0,     0,     0,   211,     0,     0,   119,    53,
       0,    54,    55,     0,     0,     0,     0,     0,    57,     0,
      58,    59,    60,     0,    61,    62,    63,     0,    64,   247,
       0,     0,   248,   249,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,    65,   265,
      67,     0,    15,     0,    16,    17,    18,    19,     0,     0,
     250,     0,   251,    20,    21,    22,    23,    24,    25,    26,
       0,     0,    27,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    53,     0,
      54,    55,     0,     0,     0,     0,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,   247,     0,
       0,   248,   249,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    65,    66,    67,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   251,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,     0,     0,     0,   155,
     156,   157,   158,   159,   160,   161,   162,   163,   164,     0,
       0,     0,     0,     0,   165,   166,   167,   168,   169,   170,
     171,   172,    36,    37,   173,    39,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,     0,     0,     0,     0,
       0,     0,   203,   204,  -588,  -588,  -588,  -588,  -588,  -588,
    -588,  -588,  -588,     0,     0,     0,     0,     0,     0,     0,
    -588,     0,  -588,  -588,  -588,  -588,     0,  -588,     0,     0,
       0,  -588,  -588,  -588,  -588,  -588,  -588,  -588,     0,     0,
    -588,     0,     0,     0,     0,     0,     0,     0,     0,  -588,
    -588,  -588,  -588,  -588,  -588,  -588,  -588,  -588,     0,  -588,
    -588,  -588,     0,     0,  -588,     0,     0,  -588,  -588,     0,
    -588,  -588,  -588,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -588,  -588,     0,     0,     0,
       0,     0,  -588,     0,     0,  -588,  -588,     0,  -588,  -588,
       0,  -588,     0,  -588,  -588,  -588,     0,  -588,  -588,  -588,
       0,  -588,  -588,  -588,     0,  -588,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -588,  -588,  -588,     0,  -588,
       0,     0,     0,     0,     0,  -588,  -589,  -589,  -589,  -589,
    -589,  -589,  -589,  -589,  -589,     0,     0,     0,     0,     0,
       0,     0,  -589,     0,  -589,  -589,  -589,  -589,     0,  -589,
       0,     0,     0,  -589,  -589,  -589,  -589,  -589,  -589,  -589,
       0,     0,  -589,     0,     0,     0,     0,     0,     0,     0,
       0,  -589,  -589,  -589,  -589,  -589,  -589,  -589,  -589,  -589,
       0,  -589,  -589,  -589,     0,     0,  -589,     0,     0,  -589,
    -589,     0,  -589,  -589,  -589,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -589,  -589,     0,
       0,     0,     0,     0,  -589,     0,     0,  -589,  -589,     0,
    -589,  -589,     0,  -589,     0,  -589,  -589,  -589,     0,  -589,
    -589,  -589,     0,  -589,  -589,  -589,     0,  -589,     0,     0,
       0,     0,     0,     0,  -591,  -591,  -591,  -591,  -591,  -591,
    -591,  -591,  -591,     0,     0,     0,     0,  -589,  -589,  -589,
    -591,  -589,  -591,  -591,  -591,  -591,     0,  -589,     0,     0,
       0,  -591,  -591,  -591,  -591,  -591,  -591,  -591,     0,     0,
    -591,     0,     0,     0,     0,     0,     0,     0,     0,  -591,
    -591,  -591,  -591,  -591,  -591,  -591,  -591,  -591,     0,  -591,
    -591,  -591,     0,     0,  -591,     0,     0,  -591,  -591,     0,
    -591,  -591,  -591,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -591,  -591,     0,     0,     0,
       0,     0,  -591,   837,     0,  -591,  -591,     0,  -591,  -591,
       0,  -591,     0,  -591,  -591,  -591,     0,  -591,  -591,  -591,
       0,  -591,  -591,  -591,     0,  -591,     0,     0,     0,     0,
       0,     0,  -107,  -592,  -592,  -592,  -592,  -592,  -592,  -592,
    -592,  -592,     0,     0,     0,  -591,  -591,  -591,     0,  -592,
       0,  -592,  -592,  -592,  -592,  -591,     0,     0,     0,     0,
    -592,  -592,  -592,  -592,  -592,  -592,  -592,     0,     0,  -592,
       0,     0,     0,     0,     0,     0,     0,     0,  -592,  -592,
    -592,  -592,  -592,  -592,  -592,  -592,  -592,     0,  -592,  -592,
    -592,     0,     0,  -592,     0,     0,  -592,  -592,     0,  -592,
    -592,  -592,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -592,  -592,     0,     0,     0,     0,
       0,  -592,   838,     0,  -592,  -592,     0,  -592,  -592,     0,
    -592,     0,  -592,  -592,  -592,     0,  -592,  -592,  -592,     0,
    -592,  -592,  -592,     0,  -592,     0,     0,     0,     0,     0,
       0,  -109,  -593,  -593,  -593,  -593,  -593,  -593,  -593,  -593,
    -593,     0,     0,     0,  -592,  -592,  -592,     0,  -593,     0,
    -593,  -593,  -593,  -593,  -592,     0,     0,     0,     0,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,     0,     0,  -593,     0,
       0,     0,     0,     0,     0,     0,     0,  -593,  -593,  -593,
    -593,  -593,  -593,  -593,  -593,  -593,     0,  -593,  -593,  -593,
       0,     0,  -593,     0,     0,  -593,  -593,     0,  -593,  -593,
    -593,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -593,  -593,     0,     0,     0,     0,     0,
    -593,     0,     0,  -593,  -593,     0,  -593,  -593,     0,  -593,
       0,  -593,  -593,  -593,     0,  -593,  -593,  -593,     0,  -593,
    -593,  -593,     0,  -593,     0,     0,     0,     0,     0,     0,
    -594,  -594,  -594,  -594,  -594,  -594,  -594,  -594,  -594,     0,
       0,     0,     0,  -593,  -593,  -593,  -594,     0,  -594,  -594,
    -594,  -594,     0,  -593,     0,     0,     0,  -594,  -594,  -594,
    -594,  -594,  -594,  -594,     0,     0,  -594,     0,     0,     0,
       0,     0,     0,     0,     0,  -594,  -594,  -594,  -594,  -594,
    -594,  -594,  -594,  -594,     0,  -594,  -594,  -594,     0,     0,
    -594,     0,     0,  -594,  -594,     0,  -594,  -594,  -594,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -594,  -594,     0,     0,     0,     0,     0,  -594,     0,
       0,  -594,  -594,     0,  -594,  -594,     0,  -594,     0,  -594,
    -594,  -594,     0,  -594,  -594,  -594,     0,  -594,  -594,  -594,
       0,  -594,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -594,  -594,  -594,     0,     0,     0,     0,     0,     0,
       0,  -594,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   143,   144,   145,   146,   147,   148,
     149,   150,   151,   152,   153,   154,     0,     0,     0,   155,
     156,   157,   233,   234,   235,   236,   162,   163,   164,     0,
       0,     0,     0,     0,   165,   166,   167,   237,   238,   239,
     240,   172,   320,   321,   241,   322,     0,     0,     0,     0,
       0,     0,   323,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,   324,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,     0,     0,     0,     0,
       0,     0,   203,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,   143,   144,   145,   146,   147,
     148,   149,   150,   151,   152,   153,   154,     0,     0,     0,
     155,   156,   157,   233,   234,   235,   236,   162,   163,   164,
       0,     0,     0,     0,     0,   165,   166,   167,   237,   238,
     239,   240,   172,   320,   321,   241,   322,     0,     0,     0,
       0,     0,     0,   323,     0,     0,     0,     0,     0,     0,
     174,   175,   176,   177,   178,   179,   180,   181,     0,     0,
     182,   183,     0,     0,     0,     0,   184,   185,   186,   187,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     188,   189,   190,     0,     0,     0,     0,   486,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   191,   192,   193,   194,   195,   196,
     197,   198,   199,   200,     0,   201,   202,     0,     0,     0,
       0,     0,     0,   203,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,   143,   144,   145,   146,
     147,   148,   149,   150,   151,   152,   153,   154,     0,     0,
       0,   155,   156,   157,   233,   234,   235,   236,   162,   163,
     164,     0,     0,     0,     0,     0,   165,   166,   167,   237,
     238,   239,   240,   172,     0,     0,   241,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,   242,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,     0,     0,
       0,     0,     0,     0,   203,   131,   132,   133,   134,   135,
     136,   137,   138,   139,   140,   141,   142,   143,   144,   145,
     146,   147,   148,   149,   150,   151,   152,   153,   154,     0,
       0,     0,   155,   156,   157,   233,   234,   235,   236,   162,
     163,   164,     0,     0,     0,     0,     0,   165,   166,   167,
     237,   238,   239,   240,   172,     0,     0,   241,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   174,   175,   176,   177,   178,   179,   180,   181,
       0,     0,   182,   183,     0,     0,     0,     0,   184,   185,
     186,   187,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   188,   189,   190,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,     0,   201,   202,     0,
       0,     0,     0,     0,     0,   203,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,     0,   108,   109,    18,    19,     0,     0,
       0,     0,     0,   110,   111,   112,    23,    24,    25,    26,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,   116,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   313,     0,     0,   119,    53,     0,
      54,    55,     0,     0,     0,     0,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   120,   108,   109,
      18,    19,     0,     0,     0,   314,     0,   110,   111,   112,
      23,    24,    25,    26,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   313,     0,
       0,   119,    53,     0,    54,    55,     0,     0,     0,     0,
       0,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,     0,     0,     0,     0,     0,     0,
      15,   120,    16,    17,    18,    19,     0,     0,     0,   611,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,    51,     0,     0,    52,    53,     0,    54,    55,
       0,    56,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,    14,     0,     0,     0,    65,    66,    67,    15,     0,
      16,    17,    18,    19,     0,     0,     0,     0,     0,    20,
      21,    22,    23,    24,    25,    26,     0,     0,    27,     0,
       0,     0,     0,     0,    28,     0,    30,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
      51,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    65,    66,    67,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   260,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,   261,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    53,     0,    54,    55,     0,   262,     0,   263,
     264,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    65,   265,    67,    15,     0,    16,    17,    18,    19,
       0,     0,     0,     0,     0,    20,    21,    22,    23,    24,
      25,    26,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   260,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   261,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
     508,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      53,     0,    54,    55,     0,   262,     0,   263,   264,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    65,
     265,    67,    15,     0,   108,   109,    18,    19,     0,     0,
       0,     0,     0,   110,   111,   112,    23,    24,    25,    26,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,   260,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,   261,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    53,     0,
      54,    55,     0,   723,     0,   263,   264,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    65,   265,    67,
      15,     0,   108,   109,    18,    19,     0,     0,     0,     0,
       0,   110,   111,   112,    23,    24,    25,    26,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,   260,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
     261,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,   854,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    53,     0,    54,    55,
       0,   723,     0,   263,   264,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    65,   265,    67,    15,     0,
     108,   109,    18,    19,     0,     0,     0,     0,     0,   110,
     111,   112,    23,    24,    25,    26,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
     260,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,   261,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    53,     0,    54,    55,     0,   262,
       0,   263,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    65,   265,    67,    15,     0,   108,   109,
      18,    19,     0,     0,     0,     0,     0,   110,   111,   112,
      23,    24,    25,    26,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   260,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,   261,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    53,     0,    54,    55,     0,     0,     0,   263,
     264,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    65,   265,    67,    15,     0,   108,   109,    18,    19,
       0,     0,     0,     0,     0,   110,   111,   112,    23,    24,
      25,    26,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   260,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   261,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      53,     0,    54,    55,     0,   723,     0,   263,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    65,
     265,    67,    15,     0,   108,   109,    18,    19,     0,     0,
       0,     0,     0,   110,   111,   112,    23,    24,    25,    26,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,   260,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,   261,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    53,     0,
      54,    55,     0,     0,     0,   263,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    65,   265,    67,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    53,     0,    54,    55,
       0,   605,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    65,   265,    67,    15,     0,
     108,   109,    18,    19,     0,     0,     0,     0,     0,   110,
     111,   112,    23,    24,    25,    26,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    53,     0,    54,    55,     0,   262,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    65,   265,    67,    15,     0,   108,   109,
      18,    19,     0,     0,     0,     0,     0,   110,   111,   112,
      23,    24,    25,    26,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    53,     0,    54,    55,     0,   605,     0,     0,
       0,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    65,   265,    67,    15,     0,   108,   109,    18,    19,
       0,     0,     0,     0,     0,   110,   111,   112,    23,    24,
      25,    26,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,    46,    47,    48,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    49,
      50,     0,     0,     0,     0,     0,   211,     0,     0,   119,
      53,     0,    54,    55,     0,   900,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
       0,     0,     0,     0,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,    65,
     265,    67,    15,     0,   108,   109,    18,    19,     0,     0,
       0,     0,     0,   110,   111,   112,    23,    24,    25,    26,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,    46,    47,    48,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    49,    50,     0,
       0,     0,     0,     0,   211,     0,     0,   119,    53,     0,
      54,    55,     0,   723,     0,     0,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,     0,     0,
       0,     0,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,    65,   265,    67,
      15,     0,    16,    17,    18,    19,     0,     0,     0,     0,
       0,    20,    21,    22,    23,    24,    25,    26,     0,     0,
      27,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
      46,    47,    48,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,    49,    50,     0,     0,     0,
       0,     0,   211,     0,     0,   119,    53,     0,    54,    55,
       0,     0,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,     0,     0,     0,     0,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,    65,    66,    67,    15,     0,
     108,   109,    18,    19,     0,     0,     0,     0,     0,   110,
     111,   112,    23,    24,    25,    26,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,    46,    47,
      48,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,    49,    50,     0,     0,     0,     0,     0,
     211,     0,     0,   119,    53,     0,    54,    55,     0,     0,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,     0,     0,     0,     0,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,    65,   265,    67,    15,     0,    16,    17,
      18,    19,     0,     0,     0,     0,     0,    20,    21,    22,
      23,    24,    25,    26,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,    34,    35,
      36,    37,    38,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,    46,    47,    48,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    49,    50,     0,     0,     0,     0,     0,   211,     0,
       0,   119,    53,     0,    54,    55,     0,     0,     0,     0,
       0,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,     0,     0,     0,     0,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,    65,   265,    67,    15,     0,   108,   109,    18,    19,
       0,     0,     0,     0,     0,   110,   111,   112,    23,    24,
      25,    26,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,   114,    35,    36,    37,
     115,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   117,     0,     0,   118,     0,     0,   119,
      53,     0,    54,    55,     0,     0,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,   120,
     108,   109,    18,    19,     0,     0,     0,     0,     0,   110,
     111,   112,    23,    24,    25,    26,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,   225,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     226,     0,     0,    52,    53,     0,    54,    55,     0,    56,
       0,     0,     0,    57,     0,    58,    59,    60,     0,    61,
      62,    63,     0,    64,     0,     0,     5,     6,     7,     8,
       9,    10,    11,    12,    13,     0,     0,     0,     0,     0,
       0,     0,    15,   120,   108,   109,    18,    19,     0,     0,
       0,     0,     0,   110,   111,   112,    23,    24,    25,    26,
       0,     0,   113,     0,     0,     0,     0,     0,     0,     0,
       0,    31,    32,    33,    34,    35,    36,    37,    38,    39,
       0,    40,    41,    42,     0,     0,    43,     0,     0,    44,
      45,     0,   116,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   313,     0,     0,   399,    53,     0,
      54,    55,     0,   400,     0,     0,     0,    57,     0,    58,
      59,    60,     0,    61,    62,    63,     0,    64,     0,     0,
       5,     6,     7,     8,     9,    10,    11,    12,    13,     0,
       0,     0,     0,     0,     0,     0,    15,   120,   108,   109,
      18,    19,     0,     0,     0,     0,     0,   110,   111,   112,
      23,    24,    25,    26,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,    31,    32,    33,   114,    35,
      36,    37,   115,    39,     0,    40,    41,    42,     0,     0,
      43,     0,     0,    44,    45,     0,   116,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   118,     0,
       0,   119,    53,     0,    54,    55,     0,     0,     0,     0,
       0,    57,     0,    58,    59,    60,     0,    61,    62,    63,
       0,    64,     0,     0,     5,     6,     7,     8,     9,    10,
      11,    12,    13,     0,     0,     0,     0,     0,     0,     0,
      15,   120,   108,   109,    18,    19,     0,     0,     0,     0,
       0,   110,   111,   112,    23,    24,    25,    26,     0,     0,
     113,     0,     0,     0,     0,     0,     0,     0,     0,    31,
      32,    33,    34,    35,    36,    37,    38,    39,     0,    40,
      41,    42,     0,     0,    43,     0,     0,    44,    45,     0,
     116,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   313,     0,     0,   399,    53,     0,    54,    55,
       0,     0,     0,     0,     0,    57,     0,    58,    59,    60,
       0,    61,    62,    63,     0,    64,     0,     0,     5,     6,
       7,     8,     9,    10,    11,    12,    13,     0,     0,     0,
       0,     0,     0,     0,    15,   120,   108,   109,    18,    19,
       0,     0,     0,     0,     0,   110,   111,   112,    23,    24,
      25,    26,     0,     0,   113,     0,     0,     0,     0,     0,
       0,     0,     0,    31,    32,    33,    34,    35,    36,    37,
      38,    39,     0,    40,    41,    42,     0,     0,    43,     0,
       0,    44,    45,     0,   116,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   967,     0,     0,   119,
      53,     0,    54,    55,     0,     0,     0,     0,     0,    57,
       0,    58,    59,    60,     0,    61,    62,    63,     0,    64,
       0,     0,     5,     6,     7,     8,     9,    10,    11,    12,
      13,     0,     0,     0,     0,     0,     0,     0,    15,   120,
     108,   109,    18,    19,     0,     0,     0,     0,     0,   110,
     111,   112,    23,    24,    25,    26,     0,     0,   113,     0,
       0,     0,     0,     0,     0,     0,     0,    31,    32,    33,
      34,    35,    36,    37,    38,    39,     0,    40,    41,    42,
       0,     0,    43,     0,     0,    44,    45,     0,   225,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     990,     0,     0,   119,    53,     0,    54,    55,     0,     0,
     659,   660,     0,    57,   661,    58,    59,    60,     0,    61,
      62,    63,     0,    64,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,   120,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   680,   651,     0,     0,
     681,     0,   203,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,   665,   660,     0,     0,   666,     0,   203,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   697,   651,
       0,     0,   698,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   700,   660,     0,     0,   701,     0,
     203,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
     707,   651,     0,     0,   708,     0,   203,   276,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,   710,   660,     0,     0,
     711,     0,   203,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,   746,   651,     0,     0,   747,     0,   203,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,   749,   660,
       0,     0,   750,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   905,   651,     0,     0,   906,     0,
     203,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
     908,   660,     0,     0,   909,     0,   203,   276,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   174,
     175,   176,   177,   178,   179,   180,   181,     0,     0,   182,
     183,     0,     0,     0,     0,   184,   185,   186,   187,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   188,
     189,   190,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   191,   192,   193,   194,   195,   196,   197,
     198,   199,   200,     0,   201,   202,  1049,   651,     0,     0,
    1050,     0,   203,   276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   174,   175,   176,   177,   178,
     179,   180,   181,     0,     0,   182,   183,     0,     0,     0,
       0,   184,   185,   186,   187,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   188,   189,   190,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   191,
     192,   193,   194,   195,   196,   197,   198,   199,   200,     0,
     201,   202,  1061,   651,     0,     0,  1062,     0,   203,   276,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   174,   175,   176,   177,   178,   179,   180,   181,     0,
       0,   182,   183,     0,     0,     0,     0,   184,   185,   186,
     187,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   188,   189,   190,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   191,   192,   193,   194,   195,
     196,   197,   198,   199,   200,     0,   201,   202,  1064,   660,
       0,     0,  1065,     0,   203,   276,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   174,   175,   176,
     177,   178,   179,   180,   181,     0,     0,   182,   183,     0,
       0,     0,     0,   184,   185,   186,   187,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   188,   189,   190,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   191,   192,   193,   194,   195,   196,   197,   198,   199,
     200,     0,   201,   202,   665,   660,     0,     0,   666,     0,
     203,   276,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   174,   175,   176,   177,   178,   179,   180,
     181,     0,     0,   182,   183,     0,     0,     0,     0,   184,
     185,   186,   187,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   188,   189,   190,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   872,
       0,     0,     0,     0,     0,     0,     0,   191,   192,   193,
     194,   195,   196,   197,   198,   199,   200,     0,   201,   202,
       0,     0,     0,     0,     0,     0,   203,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   413,   414,     0,
       0,     0,     0,   415,   416,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   418,     0,     0,     0,
       0,   885,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,   420,
     421,   422,   423,   424,   425,   426,   427,   428,   429,   403,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,     0,     0,     0,     0,   415,   416,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   418,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,   420,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   403,   404,   405,   406,   407,   408,   409,   410,   411,
     412,   413,   414,     0,     0,     0,     0,   415,   416,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     418,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   419,     0,   420,   421,   422,   423,   424,   425,   426,
     427,   428,   429,     0,     0,     0,     0,     0,     0,     0,
       0,  -277,   403,   404,   405,   406,   407,   408,   409,   410,
     411,   412,   413,   414,     0,     0,     0,     0,   415,   416,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   418,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   419,     0,   420,   421,   422,   423,   424,   425,
     426,   427,   428,   429,     0,     0,     0,     0,     0,     0,
       0,     0,  -279,   403,   404,   405,   406,   407,   408,   409,
     410,   411,   412,   413,   414,     0,     0,     0,     0,   415,
     416,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   418,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   419,     0,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,     0,     0,     0,     0,     0,
       0,     0,     0,  -280,   403,   404,   405,   406,   407,   408,
     409,   410,   411,   412,   413,   414,     0,     0,     0,     0,
     415,   416,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   418,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   419,     0,   420,   421,   422,   423,
     424,   425,   426,   427,   428,   429,     0,     0,     0,     0,
       0,     0,     0,     0,  -281,   403,   404,   405,   406,   407,
     408,   409,   410,   411,   412,   413,   414,     0,     0,     0,
       0,   415,   416,     0,     0,     0,   417,     0,     0,     0,
       0,     0,     0,     0,   418,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   419,     0,   420,   421,   422,
     423,   424,   425,   426,   427,   428,   429,   403,   404,   405,
     406,   407,   408,   409,   410,   411,   412,   413,   414,     0,
       0,     0,     0,   415,   416,     0,     0,     0,   500,     0,
       0,     0,     0,     0,     0,     0,   418,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   419,     0,   420,
     421,   422,   423,   424,   425,   426,   427,   428,   429,   403,
     404,   405,   406,   407,   408,   409,   410,   411,   412,   413,
     414,     0,     0,     0,     0,   415,   416,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   418,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   419,
       0,   420,   421,   422,   423,   424,   425,   426,   427,   428,
     429,   403,   404,   405,   406,   407,   408,   409,   410,   411,
     412,  -619,  -619,     0,     0,     0,     0,   415,   416,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     418,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   420,   421,   422,   423,   424,   425,   426,
     427,   428,   429
};

static const yytype_int16 yycheck[] =
{
       2,   285,    16,    17,    69,    27,    20,    66,    89,    28,
       2,   222,     4,     5,     6,    22,    14,     9,    10,    21,
     480,    13,    15,    15,    16,    17,     7,   378,    20,     4,
      28,    16,    17,    14,   118,    20,     7,   593,   489,   272,
      87,    88,   402,   314,     2,    56,     4,    28,    75,    82,
     442,   443,    54,    55,   307,   596,    52,   503,   311,   546,
      52,   507,     5,     6,    56,    29,   789,    21,    22,    54,
      13,   507,    74,    75,    66,   319,    69,    58,    16,    17,
     593,   691,    20,   331,   476,   477,   270,    58,   272,   761,
      82,   373,   702,    16,    10,    75,   668,   669,   105,    15,
     430,    26,   538,    25,   434,   432,   296,   437,   975,   111,
      25,   950,   394,    56,    26,     0,    54,    55,    60,    61,
      62,    63,   306,    90,   611,   117,   370,   119,   458,    62,
     457,    64,    65,   525,    16,    17,    26,    92,    20,    82,
      92,    25,   122,   473,    79,   475,    25,   474,   402,   113,
      57,   105,    58,    59,   484,    26,   483,   111,   112,    25,
      25,    25,   144,    16,    17,   332,   121,    20,   335,   121,
     337,   218,   339,    25,   341,   129,   138,   459,    90,    90,
     147,    57,   229,   116,   117,  1052,   213,   214,   442,   443,
     142,   126,   115,   105,   524,   118,   119,   753,    90,    90,
      90,    54,    55,   664,  1043,   593,   667,    90,   596,   399,
     766,   213,   214,   138,   121,   105,    92,    16,   210,   549,
     142,   548,    16,   146,   685,   148,   138,   142,   140,   442,
     443,   223,   224,   298,   214,   147,   147,    18,    91,    20,
     753,   306,   307,   290,    90,   121,   311,   819,   138,   972,
     140,    92,   975,   766,   144,   147,   147,   147,   142,   252,
     144,   242,   276,   142,   147,   144,   280,   138,   270,   316,
     272,   242,    55,   144,   276,   546,   142,   142,   142,   512,
     121,   314,    90,   955,   276,    92,   732,   733,   280,    90,
     142,   276,   284,   285,   121,   280,   288,   733,    90,    28,
     296,   147,    92,   295,   296,   298,   550,   223,   224,   557,
     797,   303,    92,   869,   121,    92,   115,    51,    90,   118,
     119,   115,   314,   879,   118,   119,   142,   511,   512,  1052,
     690,   121,   121,   105,   875,    69,   946,   295,   397,   147,
     611,   121,   280,   402,   121,   303,   147,   146,   142,   148,
      37,    38,   146,   142,   148,   147,   348,   349,   350,   351,
     352,   353,   354,   355,   810,   753,   103,   101,   102,   103,
      92,   314,   456,   348,   376,   147,   378,    92,   766,   808,
      90,   373,   324,   442,   443,   814,   846,   144,   555,   400,
     348,   128,    92,   121,   128,   353,   115,   679,   280,   118,
     119,   789,   394,   399,   125,   397,   121,   399,   400,    55,
     402,    51,   397,   805,   806,    55,   972,   402,   432,   811,
     812,   121,   773,   276,   608,    92,   121,   280,   115,   148,
     432,   118,   119,   349,   350,   351,   352,   147,   722,    92,
     432,    60,   142,   457,    63,   837,   838,    92,   840,   841,
     442,   443,    55,   434,   121,   457,   437,   400,    25,   146,
     474,   148,   142,   465,    20,   457,   142,   459,   460,   483,
      57,   801,   474,   803,   658,   802,   121,   458,   470,   501,
     434,   483,   474,    16,   101,    57,   478,   875,   107,  1045,
     743,   483,   101,   726,   475,   946,   488,   142,    92,   319,
    1056,   520,   948,   484,   458,   138,   717,   492,   790,   511,
     512,   544,   948,   546,   131,   132,   133,   510,    92,   521,
     804,   475,   520,   145,    92,   917,   797,   121,   115,   521,
     484,   118,   119,   717,   548,   817,   123,   121,   530,   520,
     891,   892,   726,   524,    92,   799,   548,   121,   940,   141,
     370,   805,   544,   121,   546,    55,   548,   811,   812,    74,
      75,   148,   139,   521,   556,   503,    37,    38,   549,   583,
     524,   142,   530,   121,   621,    58,    59,   861,   611,   432,
     862,   101,   115,   101,   972,   118,   119,   975,   602,    17,
      18,   583,   805,    57,   142,   549,   121,   535,   811,   812,
     121,   544,   604,   546,   457,   142,   121,   122,    92,   215,
     602,   887,   888,   146,    51,   148,   222,   639,   142,   611,
      90,   474,   855,   121,   122,   142,     2,    51,     4,   142,
     483,   142,    51,     9,    10,   105,   387,   121,   389,    15,
      16,    17,   121,    51,    20,   142,   928,    27,   655,    99,
     503,    15,    92,   259,   507,    13,   658,   664,   142,   121,
     667,   121,   664,   917,  1052,   667,   668,   669,   611,    90,
     140,   855,    26,  1024,   144,    16,    52,   147,   743,    63,
      15,   121,   535,   685,   105,   538,   678,   679,   690,   691,
      66,   145,   145,   695,   713,   548,   139,   142,   213,   214,
     702,   655,   142,   142,   917,   690,   142,   688,   142,  1039,
     664,  1038,   121,   667,    15,   713,    15,   688,    44,   140,
     712,   932,    15,    26,   726,   141,   147,   938,   141,   683,
     550,   685,   713,    18,   815,   115,    90,   141,   118,   119,
     799,   117,   348,   119,   141,    52,   805,   806,   139,    15,
     139,   105,   811,   812,   141,    44,   362,   139,    90,   579,
    1044,   142,   678,    57,   797,   142,   146,   586,   148,   142,
     142,   773,   142,   105,   380,    44,   596,     9,    10,   599,
     789,    15,    93,    15,   138,    14,   140,    90,   802,    15,
     144,    15,   145,   147,   732,   142,   712,   142,   790,   142,
     802,    15,   105,   142,   319,   797,   798,   799,   140,   142,
     802,   141,   119,   805,   806,   147,   139,   819,    15,   811,
     812,   139,   803,    15,    15,   817,   818,   829,    15,   115,
     832,   126,   118,   119,   210,   138,   142,   140,   126,   831,
     798,   144,   834,    55,   147,    15,    90,   223,   224,   803,
      55,   843,   844,   855,   797,   370,   142,   139,   917,   851,
     146,   105,   148,    51,   142,    53,    54,    55,    56,   142,
     862,   863,   810,   479,   480,   142,     9,    10,   142,   732,
     733,    69,    15,    16,    17,   117,    26,    20,   142,   891,
     892,    90,   142,   142,    15,   115,   140,   889,   118,   119,
     276,   144,   894,   147,   280,   144,   105,   115,   284,   285,
     118,   119,   288,   141,    47,    48,    49,    50,   834,   295,
     296,    54,    55,   142,   530,   917,   146,   303,   148,    13,
      26,   537,   521,    66,    67,   927,   928,   145,   146,   931,
     148,   140,     6,   935,   946,   992,   889,  1041,   147,   802,
      90,   894,   789,  1043,   142,    61,    90,   810,    64,    65,
     969,   816,  1040,   972,   254,   105,   975,     7,   977,   789,
     789,   105,   348,   349,   350,   351,   352,   353,   354,   355,
      90,   288,   586,  1005,   117,   969,   972,    -1,   931,   296,
      -1,   223,   224,    -1,    90,   105,    -1,   373,   138,    -1,
     140,    -1,    -1,   995,   144,   997,   140,   147,  1000,   105,
     116,   117,    -1,   147,    -1,    -1,  1025,    -1,   394,    -1,
      -1,   397,  1024,   399,  1038,    -1,   402,   778,   779,   780,
     140,   782,    -1,   784,    -1,   550,  1038,   147,  1040,  1041,
      -1,    -1,   138,  1052,   140,  1054,  1038,  1056,   144,  1058,
      -1,   147,   284,   285,    -1,   875,   432,   877,  1039,   115,
      -1,   881,   118,   119,    90,    -1,   442,   443,    -1,  1078,
      51,   677,    53,    54,    55,    56,    -1,    -1,    90,   105,
      -1,   457,    -1,   459,   460,  1039,    -1,    -1,    69,    90,
     223,   224,   399,   105,   470,   948,   115,    -1,   474,   118,
     119,    -1,   478,    -1,   105,   115,   300,   483,   118,   119,
     304,   717,   488,    62,   140,    64,    65,   349,   350,   351,
     352,   147,   354,   355,    -1,    90,    90,    90,   140,   262,
     263,   264,   265,   953,   954,   147,   146,    -1,   148,   140,
     105,   105,   105,   276,    90,   521,   147,   280,    -1,    -1,
     969,   284,   285,   972,   530,   975,   975,   977,   977,   105,
      -1,   142,    -1,   470,    -1,    -1,    -1,   116,   117,   775,
      -1,   478,   548,    90,    -1,   140,   140,   140,    -1,    -1,
     556,   488,   147,   147,   147,  1038,    -1,    61,   105,    -1,
      64,    65,  1012,    -1,   140,  1015,    -1,    63,    64,    65,
      -1,   147,    63,    64,    65,    -1,  1025,   583,   959,   960,
     961,   962,    -1,    -1,    -1,   821,   349,   350,   351,   352,
      -1,   354,   355,   140,    -1,    -1,   602,  1047,   460,    -1,
     147,    -1,  1052,  1052,  1054,  1054,    -1,  1056,  1058,  1058,
     846,   374,   116,   117,    40,    41,    42,    43,    44,   556,
     116,   117,   385,    -1,    -1,   116,   117,    -1,  1078,  1078,
      -1,   586,    -1,    -1,   397,   590,    63,    64,    65,   402,
     403,   404,   405,   406,   407,   408,   409,   410,   411,   412,
     413,   414,   415,   416,    -1,   418,   419,   420,   421,   422,
     423,   424,   425,   426,   427,   428,   429,  1048,    -1,   432,
      -1,    -1,   678,   679,    63,    64,    65,    -1,    -1,   442,
     443,    63,    64,    65,   508,    63,    64,    65,    -1,   116,
     117,   515,    -1,    -1,   457,    -1,   932,   460,    63,    64,
      65,    -1,   938,   527,    -1,    -1,   712,    -1,    -1,   472,
      -1,   474,    -1,   476,   477,     2,    -1,     4,     5,     6,
     483,    -1,    -1,    63,    64,    65,    13,   116,   117,   492,
      -1,    -1,   495,   496,   116,   117,    -1,   500,   116,   117,
     503,    -1,   505,    -1,   507,   508,    88,    89,    -1,    -1,
     101,   116,   117,    -1,    -1,    -1,   580,   581,    -1,   101,
      -1,    -1,   525,    -1,    -1,    52,    -1,   896,   897,    56,
      -1,    -1,   535,    -1,    -1,   538,   116,   117,   129,   130,
     131,   132,   133,    -1,   790,   548,   610,   129,   130,   131,
     132,   133,   798,   799,    -1,    82,   802,    -1,    -1,   805,
     806,    -1,   565,   566,    -1,   811,   812,    -1,    -1,    -1,
      -1,   817,   818,    -1,    -1,    -1,   678,    -1,    -1,    -1,
     583,    -1,    -1,    -1,    -1,   831,    -1,    -1,   834,    -1,
      -1,    -1,   119,    -1,   789,    88,    89,   843,   844,   602,
      -1,    -1,   605,   972,    -1,   851,    -1,    -1,   101,    51,
     712,    53,    54,    55,    56,    -1,   862,   863,    -1,    -1,
      -1,    -1,    -1,   687,    -1,    -1,    51,    69,    53,    54,
      55,    56,    -1,   126,   127,   128,   129,   130,   131,   132,
     133,   818,    -1,    -1,    69,     2,    -1,     4,     5,     6,
      -1,    -1,    94,    -1,   831,    -1,    13,    -1,   100,  1028,
    1029,  1030,    -1,  1032,  1033,    -1,   843,   844,    -1,    94,
      -1,   917,    -1,    -1,   851,   678,    -1,    -1,   742,    -1,
      -1,   927,   928,   210,    -1,    -1,   863,   690,    -1,   935,
     693,   694,    -1,    -1,    -1,    52,    -1,    -1,   762,    56,
      -1,    88,    89,  1072,  1073,  1074,  1075,    -1,    -1,   712,
      -1,    -1,    -1,  1082,   101,    -1,    -1,    -1,    -1,    -1,
     723,    -1,    -1,    -1,    -1,    82,    -1,    -1,    -1,   732,
     733,    -1,   834,    -1,    -1,    51,    -1,    53,    54,    55,
      56,   128,   129,   130,   131,   132,   133,    -1,    -1,   995,
     927,   997,    -1,    69,  1000,    -1,    -1,    -1,   935,    -1,
      -1,   288,   119,    -1,    -1,    -1,    -1,    -1,   295,   296,
      -1,    -1,    -1,    -1,   969,    -1,   303,   972,    94,    -1,
     975,    -1,   977,    -1,   100,    -1,    -1,   314,    -1,    -1,
     854,   794,  1038,    -1,    -1,    -1,   799,   800,    -1,   802,
      -1,    -1,   805,   806,    -1,    -1,   870,   810,   811,   812,
      51,    -1,    53,    54,    55,    56,    -1,    -1,   995,    -1,
     997,   348,    -1,  1000,    -1,    -1,   353,    -1,    69,    -1,
    1025,   834,    -1,    -1,   837,   838,    -1,   840,   841,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   373,   850,    -1,    -1,
      -1,   854,    -1,   210,    -1,    -1,    -1,  1052,    -1,  1054,
      -1,  1056,    -1,  1058,    -1,    -1,    -1,   394,   871,   872,
      -1,    -1,   399,   400,    -1,   402,    -1,    -1,    -1,    -1,
      -1,   884,   885,  1078,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   900,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   910,   911,     2,
      -1,     4,     5,     6,   917,   442,   443,    -1,    -1,    -1,
      13,    -1,    51,    -1,    53,    54,    55,    56,    -1,    -1,
      -1,   288,   459,    -1,    -1,    -1,    -1,   940,   295,   296,
      69,    -1,    -1,   470,    -1,   948,   303,    -1,    -1,    -1,
      -1,   478,    -1,    -1,    83,    -1,    -1,   314,    -1,    52,
      -1,   488,    -1,    56,    -1,    94,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,   103,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    51,    -1,    53,    54,    55,    56,    -1,    82,
      -1,   348,   121,    -1,   521,    -1,   353,    -1,    -1,   128,
      69,    -1,   131,   530,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   144,   373,   544,    51,   546,
      53,    54,    55,    56,    -1,    94,   119,    -1,    -1,   556,
      -1,   100,   101,   102,   103,  1038,    69,   394,     0,    -1,
      -1,    -1,   399,   400,    -1,   402,    -1,    -1,    -1,    -1,
      83,    13,    14,    15,    16,    17,    18,    -1,    20,   128,
      -1,    94,   131,    -1,    26,    27,    -1,   100,   101,   102,
     103,    -1,    -1,   142,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,   611,   442,   443,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,   131,    -1,
      -1,    -1,   459,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   144,    -1,   470,    -1,    -1,    -1,   210,    -1,    -1,
      -1,   478,    -1,    -1,    -1,     2,    -1,     4,    90,    -1,
      -1,   488,    -1,    -1,    -1,    -1,    13,    -1,    -1,    -1,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   679,   115,    -1,    -1,   118,   119,    -1,    -1,
      -1,    -1,    -1,    51,   521,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,   530,    -1,    52,   138,   139,    -1,    -1,
      -1,    69,   144,   145,   146,   147,   148,   544,    -1,   546,
      -1,    -1,    -1,    -1,    -1,   288,    -1,    -1,    -1,   556,
      -1,    -1,   295,   296,    -1,    -1,    94,    -1,    -1,    -1,
     303,    -1,   100,   101,   102,   103,    -1,    -1,    -1,    -1,
      -1,   314,    -1,    51,    -1,    53,    54,    55,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     128,    69,   119,   131,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   611,   348,   144,    -1,    -1,    -1,
     353,    -1,    -1,   790,    -1,    -1,    94,    -1,    -1,    -1,
     797,   798,   799,   101,   102,   103,    -1,    -1,   805,    -1,
     373,    -1,    -1,    -1,   811,   812,    -1,    -1,    -1,    -1,
     817,   818,    -1,    -1,    51,    -1,    53,    54,    55,    56,
     128,   394,    -1,    -1,   831,    -1,   399,   400,    -1,   402,
      -1,    -1,    69,    -1,    -1,    -1,   843,   844,    -1,    -1,
      -1,    -1,   679,    -1,   851,    -1,    -1,    -1,    85,    -1,
      -1,    -1,    -1,   210,    -1,   862,   863,    94,    -1,    -1,
      -1,    -1,    -1,   100,   101,   102,   103,    -1,    -1,   442,
     443,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   889,    -1,    -1,    -1,   459,   894,    -1,    -1,
      -1,   128,    -1,    -1,   131,    -1,    -1,   470,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   478,    -1,    -1,    -1,    -1,
     917,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,
     927,   928,    -1,    -1,   931,    -1,    -1,    -1,   935,    -1,
      -1,   288,    -1,    -1,    -1,    -1,    -1,    -1,   295,   296,
      -1,    -1,    -1,    -1,    -1,    -1,   303,    -1,   521,    -1,
      -1,    -1,    -1,   790,    -1,    -1,    -1,   530,    -1,    -1,
     797,   798,   799,    -1,    -1,    -1,    -1,    -1,   805,    -1,
      -1,   544,    -1,   546,   811,   812,    -1,    -1,    -1,    -1,
     817,   818,    -1,   556,    -1,    -1,    -1,    -1,   995,    -1,
     997,   348,    -1,  1000,   831,    -1,   353,    -1,    -1,     2,
      -1,     4,     5,     6,     7,    -1,   843,   844,    -1,    -1,
      13,    -1,    -1,    -1,   851,    51,   373,    53,    54,    55,
      56,    -1,    -1,    -1,    -1,   862,   863,    -1,    -1,    -1,
      -1,    -1,    -1,    69,    -1,    -1,    -1,   394,   611,    -1,
      -1,    -1,   399,    -1,    -1,   402,    -1,    -1,    -1,    52,
      -1,    -1,   889,    56,    -1,    -1,    -1,   894,    94,    -1,
      -1,    -1,    -1,    -1,   100,   101,   102,   103,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    82,
     917,    -1,    -1,    -1,    -1,   442,   443,    -1,    -1,    -1,
     927,   928,   128,    -1,   931,   131,    -1,    -1,   935,    -1,
      -1,    -1,   459,    -1,    44,    -1,   679,    -1,    -1,    -1,
      -1,    -1,    -1,   470,    -1,    -1,   119,    -1,    -1,    -1,
      -1,   478,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   488,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   995,    -1,
     997,   101,    -1,  1000,   521,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   530,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,    -1,   556,
      -1,    -1,   142,    -1,    -1,    -1,    -1,   210,    -1,    -1,
       2,    -1,     4,    -1,    -1,    -1,    -1,   790,    -1,    -1,
      -1,    -1,    -1,    -1,   797,   798,   799,    -1,    -1,    -1,
      -1,    -1,   805,    -1,    -1,    -1,    -1,    -1,   811,   812,
      -1,    -1,    -1,    -1,   817,   818,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   831,    -1,
      52,    -1,    -1,    51,    -1,    53,    54,    55,    56,    -1,
     843,   844,    -1,    -1,    -1,    -1,    -1,    -1,   851,    -1,
      -1,    69,    -1,    -1,    -1,   288,    -1,    -1,    -1,   862,
     863,    -1,   295,   296,    -1,    -1,    -1,    85,    -1,    -1,
     303,    -1,    -1,    -1,    -1,    -1,    94,    -1,    -1,    -1,
      -1,   314,   100,   101,   102,   103,   889,    -1,    -1,    -1,
      -1,   894,   679,    -1,    -1,    -1,    -1,   119,    -1,    51,
      -1,    53,    54,    55,    56,    -1,    -1,    -1,    -1,    -1,
     128,    -1,    -1,   131,   917,   348,    -1,    69,    -1,    -1,
     353,    -1,    -1,    -1,   927,   928,    -1,    -1,   931,    -1,
      -1,    -1,   935,    85,    -1,    -1,    -1,    -1,    -1,    -1,
     373,    -1,    94,    -1,    -1,    -1,    -1,    -1,   100,   101,
     102,   103,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   394,    -1,    -1,    -1,    -1,   399,   400,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   128,    -1,    -1,   131,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   210,    -1,
      -1,    -1,   995,    -1,   997,    -1,    -1,  1000,    -1,    -1,
      -1,    -1,    -1,   790,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   798,   799,    -1,    -1,    -1,    -1,    -1,   805,    -1,
      -1,    -1,    -1,    -1,   811,   812,   459,    -1,    -1,    -1,
     817,   818,    -1,    -1,    -1,    -1,    -1,   470,    -1,    -1,
      -1,    -1,    -1,    -1,   831,   478,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   488,   843,   844,    -1,    -1,
      -1,    -1,    -1,    -1,   851,    -1,   288,    -1,    -1,    -1,
      -1,    -1,    -1,   295,   296,   862,   863,    -1,    -1,    -1,
      -1,   303,    -1,    -1,    -1,    -1,    -1,    -1,   521,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   530,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    -1,    80,    81,
      -1,   544,    -1,   546,    -1,    -1,    88,    89,    -1,    -1,
      -1,    -1,    -1,   556,    -1,    -1,   348,    -1,    -1,   101,
     917,   353,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     927,   928,    -1,    -1,   931,    -1,    -1,    -1,   935,    -1,
       0,   373,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    13,    14,    15,    -1,    17,    18,    -1,
      20,    -1,   394,    -1,    -1,    -1,    26,   399,   611,    -1,
     402,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   995,    -1,
     997,    -1,    -1,  1000,    -1,    -1,    -1,    -1,    -1,    -1,
     442,   443,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,   459,    88,    89,
      90,    -1,    92,    93,    -1,    -1,   679,    -1,   470,    -1,
      -1,   101,    -1,    -1,    -1,   105,   478,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,   488,    -1,   118,   119,
      -1,   121,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,    -1,    -1,   138,   139,
     140,    44,   142,    -1,    -1,   145,   146,   147,   148,   521,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   530,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,
      73,    74,    75,    76,    77,    78,    79,    80,    81,    82,
      83,    -1,    -1,    -1,   556,    88,    89,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   790,    -1,    -1,
      -1,    -1,    -1,    -1,   797,   798,    -1,    -1,    -1,   122,
      -1,   124,   125,   126,   127,   128,   129,   130,   131,   132,
     133,    -1,    -1,    -1,   817,   818,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,   831,     0,
      80,    81,    -1,    -1,    -1,    -1,    -1,    -1,    88,    89,
     843,   844,    13,    14,    15,    -1,    17,    18,   851,    20,
      -1,   101,    -1,    -1,    -1,    26,    -1,    -1,    -1,   862,
     863,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,    40,
      41,    42,    43,    44,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,   889,   679,    -1,    -1,
      -1,   894,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,
      -1,    92,    93,    -1,   927,   928,    -1,    -1,   931,    -1,
     101,    -1,   935,    -1,   105,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,    -1,
     121,   122,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,    -1,    -1,    -1,   138,   139,   140,
      -1,   142,    -1,    -1,   145,   146,   147,   148,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   995,    -1,   997,    -1,    -1,  1000,   790,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   798,   799,    -1,    -1,
      -1,    51,    52,   805,    -1,    55,    -1,    -1,    -1,   811,
     812,    -1,    -1,    -1,    -1,   817,   818,    -1,    -1,    -1,
      70,    71,    72,    73,    74,    75,    76,    77,    -1,   831,
      80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,
      -1,   843,   844,    -1,    -1,    -1,    -1,    -1,    -1,   851,
     100,   101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     862,   863,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,   135,   136,    72,    73,    74,
      75,    76,    77,   143,   144,    80,    81,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   917,   101,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   927,   928,    -1,    -1,    -1,
      -1,    -1,    -1,   935,    -1,    -1,    -1,    -1,    -1,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,    -1,
      -1,    -1,    -1,    -1,    -1,     0,     1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    -1,    -1,
      -1,    -1,    -1,    -1,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,   995,    39,   997,    -1,    -1,  1000,    -1,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,     0,   114,
     115,    -1,    -1,   118,   119,    -1,    -1,    -1,    -1,    -1,
      -1,    13,    14,    15,    16,    17,    18,    -1,    20,   134,
     135,   136,    -1,    -1,    -1,    27,    28,    29,    -1,    -1,
      -1,   146,    -1,   148,    -1,    37,    38,    -1,    40,    41,
      42,    43,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      72,    73,    74,    75,    76,    77,    78,    79,    80,    81,
      82,    83,    -1,    -1,    -1,    -1,    88,    89,    90,    -1,
      -1,    93,    -1,    -1,    -1,    -1,    -1,    99,    -1,   101,
      -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   113,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,
     122,   123,   124,   125,   126,   127,   128,   129,   130,   131,
     132,   133,    -1,    -1,     0,    -1,    -1,   139,   140,   141,
     142,    -1,    -1,   145,   146,   147,   148,    13,    14,    15,
      16,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,
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
      20,    -1,    -1,    -1,    -1,    -1,    -1,    27,    28,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    37,    38,    -1,
      40,    41,    42,    43,    44,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    57,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    72,    73,    74,    75,    76,    77,    78,    79,
      80,    81,    82,    83,    -1,    -1,    -1,    -1,    88,    89,
      90,    -1,    92,    93,    -1,    -1,    -1,    -1,    -1,    99,
      -1,   101,    -1,    -1,    -1,   105,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,   118,   119,
      -1,   121,   122,    -1,   124,   125,   126,   127,   128,   129,
     130,   131,   132,   133,    -1,    -1,     0,    -1,    -1,   139,
     140,   141,   142,    -1,    -1,   145,   146,   147,   148,    13,
      14,    15,    16,    17,    18,    -1,    20,    -1,    -1,    -1,
      -1,    -1,    26,    27,    28,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    37,    38,    -1,    40,    41,    42,    43,
      44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    72,    73,
      74,    75,    76,    77,    78,    79,    80,    81,    82,    83,
      -1,    -1,    -1,    -1,    88,    89,    90,    -1,    -1,    93,
      -1,    -1,    -1,    -1,    -1,    99,    -1,   101,    -1,    -1,
      -1,   105,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   115,    -1,    -1,   118,   119,    -1,    -1,   122,    -1,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      -1,    -1,     0,    -1,   138,   139,   140,   141,   142,    -1,
     144,   145,   146,   147,   148,    13,    14,    15,    16,    17,
      18,    -1,    20,    -1,    -1,    -1,    -1,    -1,    -1,    27,
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
      -1,   139,   140,   141,   142,    -1,   144,   145,   146,   147,
     148,    13,    14,    15,    -1,    17,    18,    -1,    20,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      -1,    17,    18,    -1,    20,    -1,    -1,    -1,    -1,    -1,
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
       1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,
      85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,   104,
      -1,   106,   107,   108,    -1,   110,   111,   112,    -1,   114,
     115,    -1,    -1,   118,   119,     1,    -1,     3,     4,     5,
       6,     7,     8,     9,    10,    11,    12,    -1,    -1,   134,
     135,   136,    -1,    19,    -1,    21,    22,    23,    24,    -1,
      -1,   146,    -1,   148,    30,    31,    32,    33,    34,    35,
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
      -1,    -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,
      -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,    -1,
      -1,    -1,   104,    -1,   106,   107,   108,    -1,   110,   111,
     112,    -1,   114,   115,    -1,    -1,   118,   119,    -1,    -1,
       3,     4,     5,     6,     7,     8,     9,    10,    11,    12,
      -1,    -1,   134,   135,   136,    -1,    19,    -1,    21,    22,
      23,    24,    -1,    -1,   146,    -1,   148,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    45,    46,    47,    48,    49,    50,    51,    52,
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
      -1,    45,    -1,    47,    48,    49,    50,    51,    52,    53,
      54,    55,    56,    -1,    58,    59,    60,    -1,    -1,    63,
      -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,    -1,
      94,    95,    -1,    97,    98,    -1,   100,    -1,    -1,    -1,
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
      -1,    -1,   118,   119,    -1,    -1,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,   134,   135,
     136,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
     146,    -1,   148,    30,    31,    32,    33,    34,    35,    36,
      -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    48,    49,    50,    51,    52,    53,    54,    55,    56,
      -1,    58,    59,    60,    -1,    -1,    63,    -1,    -1,    66,
      67,    -1,    69,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    85,    -1,
      -1,    -1,    -1,    -1,    91,    -1,    -1,    94,    95,    -1,
      97,    98,    -1,    -1,    -1,    -1,    -1,   104,    -1,   106,
     107,   108,    -1,   110,   111,   112,    -1,   114,   115,    -1,
      -1,   118,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   134,   135,   136,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   148,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   102,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,   143,   144,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      19,    -1,    21,    22,    23,    24,    -1,    26,    -1,    -1,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,   102,   103,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   134,   135,   136,    -1,   138,
      -1,    -1,    -1,    -1,    -1,   144,     3,     4,     5,     6,
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
      -1,    -1,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    -1,    -1,    -1,    -1,   134,   135,   136,
      19,   138,    21,    22,    23,    24,    -1,   144,    -1,    -1,
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
      -1,    91,    92,    -1,    94,    95,    -1,    97,    98,    -1,
     100,    -1,   102,   103,   104,    -1,   106,   107,   108,    -1,
     110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,
      -1,   121,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    -1,    -1,    -1,   134,   135,   136,    -1,    19,    -1,
      21,    22,    23,    24,   144,    -1,    -1,    -1,    -1,    30,
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
      23,    24,    -1,   144,    -1,    -1,    -1,    30,    31,    32,
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
      -1,   134,   135,   136,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   144,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    17,    18,    19,    20,
      21,    22,    23,    24,    25,    26,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    -1,
      -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,    -1,    -1,    86,    87,    88,    89,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   100,
     101,   102,    -1,    -1,    -1,    -1,   107,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,    -1,   135,   136,    -1,    -1,    -1,    -1,
      -1,    -1,   143,     3,     4,     5,     6,     7,     8,     9,
      10,    11,    12,    13,    14,    15,    16,    17,    18,    19,
      20,    21,    22,    23,    24,    25,    26,    -1,    -1,    -1,
      30,    31,    32,    33,    34,    35,    36,    37,    38,    39,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    -1,    -1,    -1,
      -1,    -1,    -1,    63,    -1,    -1,    -1,    -1,    -1,    -1,
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
      49,    50,    51,    52,    -1,    -1,    55,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    70,    71,    72,    73,    74,    75,    76,    77,    -1,
      -1,    80,    81,    -1,    -1,    -1,    -1,    86,    87,    88,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   100,   101,   102,    -1,    -1,    -1,   106,    -1,    -1,
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
      -1,    -1,   100,   101,   102,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,   127,
     128,   129,   130,   131,   132,   133,    -1,   135,   136,    -1,
      -1,    -1,    -1,    -1,    -1,   143,     3,     4,     5,     6,
       7,     8,     9,    10,    11,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    19,    -1,    21,    22,    23,    24,    -1,    -1,
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
      23,    24,    -1,    -1,    -1,   142,    -1,    30,    31,    32,
      33,    34,    35,    36,    -1,    -1,    39,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    48,    49,    50,    51,    52,
      53,    54,    55,    56,    -1,    58,    59,    60,    -1,    -1,
      63,    -1,    -1,    66,    67,    -1,    69,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,     3,     4,     5,     6,     7,     8,
       9,    10,    11,    12,    -1,    -1,    -1,    -1,    -1,    -1,
      19,   134,    21,    22,    23,    24,    -1,    -1,    -1,   142,
      -1,    30,    31,    32,    33,    34,    35,    36,    -1,    -1,
      39,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,
      49,    50,    51,    52,    53,    54,    55,    56,    -1,    58,
      59,    60,    -1,    -1,    63,    -1,    -1,    66,    67,    -1,
      69,    70,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    85,    -1,    -1,    -1,
      -1,    -1,    91,    -1,    -1,    94,    95,    -1,    97,    98,
      -1,   100,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
      -1,   110,   111,   112,    -1,   114,    -1,    -1,    -1,    -1,
      -1,    -1,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    -1,    -1,    -1,   134,   135,   136,    19,    -1,
      21,    22,    23,    24,    -1,    -1,    -1,    -1,    -1,    30,
      31,    32,    33,    34,    35,    36,    -1,    -1,    39,    -1,
      -1,    -1,    -1,    -1,    45,    -1,    47,    48,    49,    50,
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
      -1,   100,    -1,   102,   103,   104,    -1,   106,   107,   108,
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
      -1,   102,    -1,   104,    -1,   106,   107,   108,    -1,   110,
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
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,   102,
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
      95,    -1,    97,    98,    -1,   100,    -1,   102,    -1,   104,
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
      97,    98,    -1,    -1,    -1,   102,    -1,   104,    -1,   106,
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
      63,    -1,    -1,    66,    67,    -1,    69,    70,    71,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    84,    85,    -1,    -1,    -1,    -1,    -1,    91,    -1,
      -1,    94,    95,    -1,    97,    98,    -1,    -1,    -1,    -1,
      -1,   104,    -1,   106,   107,   108,    -1,   110,   111,   112,
      -1,   114,    -1,    -1,    -1,    -1,    -1,    -1,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    -1,    -1,    -1,
      -1,   134,   135,   136,    19,    -1,    21,    22,    23,    24,
      -1,    -1,    -1,    -1,    -1,    30,    31,    32,    33,    34,
      35,    36,    -1,    -1,    39,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    -1,    58,    59,    60,    -1,    -1,    63,    -1,
      -1,    66,    67,    -1,    69,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    91,    -1,    -1,    94,
      95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,
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
      91,    -1,    -1,    94,    95,    -1,    97,    98,    -1,   100,
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
      97,    98,    -1,   100,    -1,    -1,    -1,   104,    -1,   106,
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
      -1,    -1,    -1,    -1,    -1,   104,    -1,   106,   107,   108,
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
      95,    -1,    97,    98,    -1,    -1,    -1,    -1,    -1,   104,
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
      51,    52,    -1,   104,    55,   106,   107,   108,    -1,   110,
     111,   112,    -1,   114,    -1,    -1,    -1,    -1,    -1,    70,
      71,    72,    73,    74,    75,    76,    77,    -1,    -1,    80,
      81,    -1,    -1,   134,    -1,    86,    87,    88,    89,    -1,
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
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   124,   125,   126,
     127,   128,   129,   130,   131,   132,   133,    -1,   135,   136,
      -1,    -1,    -1,    -1,    -1,    -1,   143,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    -1,
      -1,    -1,    -1,    88,    89,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   101,    -1,    -1,    -1,
      -1,    44,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
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
      51,    69,   100,   102,   103,   135,   172,   189,   195,   198,
     201,   254,   309,   310,   195,   195,   144,   192,   193,   196,
     197,   324,   192,   196,   144,   318,   184,   155,   138,   189,
     220,   189,   189,   189,    55,     1,    94,   157,   158,   159,
     174,   175,   324,   205,   207,   190,   201,   309,   324,   189,
     308,   309,   324,    91,   142,   179,   220,   272,   275,   208,
      53,    54,    56,    63,   107,   183,   269,    63,    64,    65,
     116,   117,   255,   256,    61,   255,    62,   255,    63,   255,
      63,   255,    58,    59,   168,   189,   189,   317,   323,    40,
      41,    42,    43,    44,    37,    38,    51,    53,    54,    55,
      56,    69,    83,    94,   100,   101,   102,   103,   128,   131,
     144,   278,   279,   280,   281,   282,   285,   286,   287,   288,
     290,   291,   292,   293,   295,   296,   297,   300,   301,   302,
     303,   304,   324,   278,   280,    28,   239,   121,   142,    94,
     100,   176,   121,    72,    73,    74,    75,    76,    77,    78,
      79,    80,    81,    82,    83,    88,    89,    93,   101,   122,
     124,   125,   126,   127,   128,   129,   130,   131,   132,   133,
      90,   105,   140,   147,   315,    90,   315,   316,    26,   138,
     243,   254,    92,    92,   192,   196,   243,   163,    51,    55,
     181,    58,    59,   279,   125,   276,    90,   140,   315,   219,
     307,    90,   147,   314,   156,   157,    55,   278,   278,    16,
     221,   321,   121,    90,   140,   315,    92,    92,   221,   167,
     167,    55,    90,   140,   315,    25,   107,   142,   265,   317,
     115,   264,    20,   246,   321,    57,    57,   189,   189,   189,
      93,   142,   199,   200,   324,    57,   199,   200,    85,   194,
     195,   201,   309,   324,   195,   163,   317,   319,   163,   322,
     160,   138,   157,    90,   315,    92,   159,   174,   145,   317,
     323,   319,   159,   319,   141,   200,   320,   323,   200,   320,
     139,   320,    55,   176,   177,   178,   142,    90,   140,   315,
     144,   237,   290,   295,    63,   255,   257,   261,   262,    63,
     256,    61,    62,    63,    63,   101,   101,   154,   167,   167,
     167,   167,   159,   163,   163,    57,   121,    57,   321,   294,
      85,   290,   295,   121,   156,   189,   142,   305,   324,    51,
     142,   305,   321,   142,   289,   189,   142,   289,    51,   142,
     289,    51,   121,   156,   240,   100,   168,   189,   201,   202,
     174,   142,   179,   142,   161,   162,   168,   180,   189,   191,
     202,   220,   275,   189,   189,   189,   189,   189,   189,   189,
     189,   189,   189,   189,   189,   189,   189,    51,   189,   189,
     189,   189,   189,   189,   189,   189,   189,   189,   189,   189,
      51,    52,    55,   187,   192,   312,   313,   194,   201,    51,
      52,    55,   187,   192,   312,    51,    55,   312,   245,   244,
     162,   189,   191,   162,   191,    99,   170,   217,   277,   216,
      51,    55,   181,   312,   194,   312,   156,   163,   166,    15,
      13,   248,   324,   121,   121,   157,    16,    51,    55,   194,
      51,    55,   157,    27,   222,   321,   222,    51,    55,   194,
      51,    55,   214,   186,   157,   246,   189,   201,    15,   189,
     189,   189,   318,   100,   189,   198,   309,   189,   310,   319,
     145,   317,   200,   200,   319,   145,   184,   152,   139,   191,
     319,   159,   206,   309,   176,   178,    51,    55,   194,    51,
      55,   290,   209,   142,    63,   157,   262,   189,   189,    51,
      69,   100,   226,   295,   319,   319,   142,   172,   189,    15,
      51,    69,   282,   287,   304,    85,   288,   293,   300,   302,
     295,   297,   302,    51,   295,   172,   189,    15,    79,   126,
     231,   232,   324,   189,   200,   319,   178,   142,    44,   121,
      44,    90,   140,   315,   318,    92,    92,   192,   196,   141,
     200,    92,    92,   193,   196,   193,   196,   231,   231,   171,
     321,   167,   156,   141,    15,   319,   183,   189,   202,   249,
     324,    18,   224,   324,    17,   223,   224,    92,    92,   141,
      92,    92,   224,   211,   213,   141,   167,   184,   139,    15,
     200,   221,   189,   199,    85,   309,   139,   319,   320,   141,
     234,   318,    29,   113,   238,   139,   142,   292,   319,   142,
      85,    44,    44,   305,   321,   142,   289,   142,   289,   142,
     289,   142,   289,   289,    44,    44,   228,   230,   233,   281,
     283,   284,   287,   295,   296,   298,   299,   302,   304,   156,
     100,   189,   178,   159,   189,    51,    55,   194,    51,    55,
      57,   123,   162,   191,   168,   191,   170,    92,   162,   191,
     162,   191,   170,   243,   239,   156,   157,   231,   218,   321,
      15,    93,   250,   324,   157,    14,   251,   324,   167,    15,
      92,    15,   157,   157,   222,   189,   157,   319,   200,   145,
     146,   156,   157,   227,   142,   100,   319,   189,   189,   295,
     302,   295,   295,   189,   189,   234,   234,    91,   220,   142,
     305,   305,   142,   229,   220,   142,   229,   142,   229,    15,
     189,   141,   189,   189,   162,   191,    15,   139,   157,   156,
      91,   180,   220,   272,   275,   221,   157,   221,    15,    15,
     215,   224,   246,   247,    51,   235,   236,   291,    15,   139,
     295,   295,   142,   292,   289,   142,   289,   289,   289,   126,
     126,    55,    90,   283,   287,   142,   228,   229,   299,   302,
     295,   298,   302,   295,   139,    15,    55,    90,   140,   315,
     157,   157,   157,   142,   318,   142,   295,   142,   295,    51,
      55,   305,   142,   229,   142,   229,   142,   229,   142,   229,
     229,    51,    55,   194,    51,    55,   248,   223,    15,   236,
     295,   289,   295,   302,   295,   295,   141,   229,   142,   229,
     229,   229,   295,   229
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
     197,   196,   198,   198,   199,   199,   200,   201,   201,   201,
     201,   201,   202,   202,   202,   203,   203,   203,   203,   203,
     203,   203,   203,   203,   204,   203,   205,   206,   203,   207,
     203,   203,   203,   203,   203,   203,   203,   203,   203,   203,
     203,   203,   203,   208,   209,   203,   203,   203,   210,   211,
     203,   212,   213,   203,   203,   203,   214,   215,   203,   216,
     203,   217,   218,   203,   219,   203,   203,   203,   203,   203,
     203,   203,   220,   221,   221,   221,   222,   222,   223,   223,
     224,   224,   225,   225,   226,   226,   226,   226,   226,   226,
     226,   226,   227,   226,   228,   228,   228,   228,   229,   229,
     230,   230,   230,   230,   230,   230,   230,   230,   230,   230,
     230,   230,   230,   230,   230,   231,   231,   233,   232,   232,
     232,   234,   234,   235,   235,   236,   236,   237,   237,   238,
     238,   240,   239,   241,   241,   241,   241,   242,   242,   242,
     242,   242,   242,   242,   242,   242,   244,   243,   245,   243,
     246,   247,   247,   248,   248,   249,   249,   249,   250,   250,
     251,   251,   252,   252,   252,   252,   253,   253,   254,   254,
     254,   254,   255,   255,   256,   257,   256,   256,   256,   258,
     258,   259,   259,   260,   261,   261,   262,   262,   263,   263,
     264,   265,   264,   266,   266,   267,   267,   268,   269,   269,
     269,   269,   269,   269,   270,   270,   271,   271,   271,   271,
     272,   272,   272,   272,   272,   273,   273,   274,   274,   274,
     274,   274,   274,   274,   274,   275,   275,   276,   277,   276,
     278,   278,   279,   279,   279,   280,   280,   280,   280,   281,
     281,   282,   282,   283,   283,   284,   284,   285,   285,   286,
     286,   287,   287,   288,   288,   288,   288,   289,   289,   290,
     290,   290,   290,   290,   290,   290,   290,   290,   290,   290,
     290,   290,   290,   290,   291,   291,   291,   291,   291,   292,
     292,   293,   294,   293,   295,   295,   296,   297,   298,   299,
     299,   300,   300,   301,   301,   302,   302,   303,   303,   304,
     304,   305,   305,   306,   307,   306,   308,   308,   309,   309,
     310,   310,   310,   310,   310,   310,   310,   310,   311,   311,
     311,   312,   312,   312,   312,   313,   313,   313,   314,   314,
     315,   315,   316,   316,   317,   317,   318,   318,   319,   320,
     320,   320,   321,   321,   322,   322,   323,   323,   324
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
       0,     2,     2,     1,     2,     1,     2,     1,     1,     2,
       3,     4,     3,     4,     2,     1,     1,     1,     1,     1,
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
       1,     0,     4,     2,     3,     1,     4,     2,     1,     1,
       1,     1,     1,     2,     2,     3,     1,     1,     2,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     0,     4,
       1,     1,     3,     5,     3,     1,     2,     4,     2,     2,
       2,     2,     1,     2,     1,     1,     3,     1,     3,     1,
       1,     2,     1,     4,     2,     2,     1,     2,     0,     6,
       8,     4,     6,     4,     6,     2,     4,     6,     2,     4,
       2,     4,     1,     0,     1,     1,     1,     1,     1,     1,
       1,     1,     0,     4,     1,     3,     2,     2,     2,     1,
       3,     1,     3,     1,     1,     2,     1,     1,     1,     2,
       1,     2,     1,     1,     0,     4,     1,     2,     1,     3,
       3,     3,     2,     2,     3,     3,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     0,     2,     2,     0,
       1,     1,     1,     1,     1,     1,     1,     2,     0
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
#line 1620 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      if (!p->locals) p->locals = cons(0,0);
                    }
#line 6101 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 3:
#line 1625 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->tree = new_scope(p, (yyvsp[0].nd));
                      NODE_LINENO(p->tree, (yyvsp[0].nd));
                    }
#line 6110 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 4:
#line 1632 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6118 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 5:
#line 1638 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6126 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 6:
#line 1642 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6135 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 7:
#line 1647 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6143 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 8:
#line 1651 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6151 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 10:
#line 1658 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 6160 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 11:
#line 1663 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "BEGIN not supported");
                      local_resume(p, (yyvsp[-3].nd));
                      nvars_unnest(p);
                      (yyval.nd) = 0;
                    }
#line 6171 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 12:
#line 1675 "mrbgems/mruby-compiler/core/parse.y"
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
#line 6197 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 13:
#line 1699 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6205 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 14:
#line 1705 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6213 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 15:
#line 1709 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 6222 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 16:
#line 1714 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), newline_node((yyvsp[0].nd)));
                    }
#line 6230 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 17:
#line 1718 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_begin(p, (yyvsp[0].nd));
                    }
#line 6238 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 18:
#line 1723 "mrbgems/mruby-compiler/core/parse.y"
                                     {p->lstate = EXPR_FNAME;}
#line 6244 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 19:
#line 1724 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_alias(p, (yyvsp[-2].id), (yyvsp[0].id));
                    }
#line 6252 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 20:
#line 1728 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6260 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 21:
#line 1732 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6268 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 22:
#line 1736 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd), 0);
                    }
#line 6276 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 23:
#line 1740 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6284 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 24:
#line 1744 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[0].nd)), (yyvsp[-2].nd));
                    }
#line 6292 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 25:
#line 1748 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6300 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 26:
#line 1752 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "END not supported");
                      (yyval.nd) = new_postexe(p, (yyvsp[-1].nd));
                    }
#line 6309 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 28:
#line 1758 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6317 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 29:
#line 1762 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6325 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 30:
#line 1766 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6333 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 31:
#line 1770 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-2].nd), new_array(p, (yyvsp[0].nd)));
                    }
#line 6341 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 32:
#line 1774 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *lhs = new_lvar(p, (yyvsp[0].id));
                      assignable(p, lhs);
                      (yyval.nd) = new_asgn(p, lhs, (yyvsp[-2].nd));
                    }
#line 6351 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 34:
#line 1783 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6359 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 35:
#line 1787 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6367 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 36:
#line 1791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6375 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 37:
#line 1795 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6383 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 38:
#line 1799 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6391 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 39:
#line 1803 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = 0;
                    }
#line 6400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 40:
#line 1808 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6408 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 41:
#line 1812 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 6421 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 42:
#line 1821 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 6434 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 43:
#line 1830 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 6447 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 44:
#line 1839 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 6460 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 45:
#line 1848 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 6469 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 47:
#line 1856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6477 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 50:
#line 1865 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6485 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 51:
#line 1869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6493 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 52:
#line 1873 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6501 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 53:
#line 1877 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 6509 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 55:
#line 1885 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_def(p, (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      nvars_block(p);
                    }
#line 6520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 56:
#line 1894 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_FNAME;
                    }
#line 6528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 57:
#line 1898 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sdef(p, (yyvsp[-3].nd), (yyvsp[0].id), nint(p->cmdarg_stack), local_switch(p));
                      p->cmdarg_stack = 0;
                      p->in_def++;
                      p->in_single++;
                      nvars_block(p);
                      p->lstate = EXPR_ENDFN; /* force for args */
                    }
#line 6541 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 58:
#line 1909 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (!(yyvsp[0].nd)) (yyval.nd) = new_nil(p);
                      else {
                        (yyval.nd) = (yyvsp[0].nd);
                      }
                    }
#line 6552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 62:
#line 1923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 63:
#line 1929 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 6569 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 64:
#line 1936 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p, (yyvsp[-2].nd), (yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 6579 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 65:
#line 1944 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 6587 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 66:
#line 1948 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_fcall(p, (yyvsp[-2].id), (yyvsp[-1].nd));
                    }
#line 6596 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 67:
#line 1953 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 6604 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 68:
#line 1957 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                   }
#line 6613 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 69:
#line 1962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 6621 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 70:
#line 1966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      args_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), tCOLON2);
                    }
#line 6630 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 71:
#line 1971 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 6638 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 72:
#line 1975 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 6646 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 73:
#line 1979 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6654 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 74:
#line 1983 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6662 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 75:
#line 1987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, ret_args(p, (yyvsp[0].nd)));
                    }
#line 6670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 76:
#line 1993 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 6678 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 77:
#line 1997 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6686 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 79:
#line 2004 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 6694 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 80:
#line 2010 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6702 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 81:
#line 2014 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(push((yyvsp[-1].nd),(yyvsp[0].nd)));
                    }
#line 6710 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 82:
#line 2018 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6718 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 83:
#line 2022 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6726 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 84:
#line 2026 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2((yyvsp[-1].nd), new_nil(p));
                    }
#line 6734 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 85:
#line 2030 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_nil(p), (yyvsp[0].nd));
                    }
#line 6742 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 86:
#line 2034 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, (yyvsp[0].nd));
                    }
#line 6750 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 87:
#line 2038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 6758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 88:
#line 2042 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list2(0, new_nil(p));
                    }
#line 6766 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 89:
#line 2046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_nil(p), (yyvsp[0].nd));
                    }
#line 6774 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 91:
#line 2053 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn(p, (yyvsp[-1].nd), NULL);
                    }
#line 6782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 92:
#line 2059 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[-1].nd));
                    }
#line 6790 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 93:
#line 2063 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[-1].nd));
                    }
#line 6798 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 94:
#line 2069 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 6806 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 95:
#line 2073 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 6814 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 96:
#line 2079 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6822 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 97:
#line 2083 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 6830 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 98:
#line 2087 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6838 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 99:
#line 2091 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6846 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 100:
#line 2095 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6854 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 101:
#line 2099 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6864 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 102:
#line 2105 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6874 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 103:
#line 2111 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6883 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 104:
#line 2118 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 6891 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 105:
#line 2122 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 6899 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 106:
#line 2126 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6907 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 107:
#line 2130 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 6915 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 108:
#line 2134 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, (yyvsp[-1].num));
                    }
#line 6923 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 109:
#line 2138 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 6933 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 110:
#line 2144 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "dynamic constant assignment");
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 6943 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 111:
#line 2150 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[0].nd));
                      (yyval.nd) = 0;
                    }
#line 6952 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 112:
#line 2155 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 6960 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 113:
#line 2161 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "class/module name must be CONSTANT");
                    }
#line 6968 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 115:
#line 2168 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(nint(1), nsym((yyvsp[0].id)));
                    }
#line 6976 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 116:
#line 2172 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(nint(0), nsym((yyvsp[0].id)));
                    }
#line 6984 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 117:
#line 2176 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), nsym((yyvsp[0].id)));
                    }
#line 6993 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 121:
#line 2186 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 7002 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 122:
#line 2191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDFN;
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 7011 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 125:
#line 2202 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_undef(p, (yyvsp[0].id));
                    }
#line 7019 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 126:
#line 2205 "mrbgems/mruby-compiler/core/parse.y"
                                 {p->lstate = EXPR_FNAME;}
#line 7025 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 127:
#line 2206 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), nsym((yyvsp[0].id)));
                    }
#line 7033 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 128:
#line 2211 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(or);     }
#line 7039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 129:
#line 2212 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(xor);    }
#line 7045 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 130:
#line 2213 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(and);    }
#line 7051 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 131:
#line 2214 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(cmp);    }
#line 7057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 132:
#line 2215 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eq);     }
#line 7063 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 133:
#line 2216 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(eqq);    }
#line 7069 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 134:
#line 2217 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(match);  }
#line 7075 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 135:
#line 2218 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(nmatch); }
#line 7081 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 136:
#line 2219 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(gt);     }
#line 7087 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 137:
#line 2220 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(ge);     }
#line 7093 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 138:
#line 2221 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lt);     }
#line 7099 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 139:
#line 2222 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(le);     }
#line 7105 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 140:
#line 2223 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neq);    }
#line 7111 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 141:
#line 2224 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(lshift); }
#line 7117 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 142:
#line 2225 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(rshift); }
#line 7123 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 143:
#line 2226 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(add);    }
#line 7129 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 144:
#line 2227 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(sub);    }
#line 7135 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 145:
#line 2228 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 7141 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 146:
#line 2229 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mul);    }
#line 7147 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 147:
#line 2230 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(div);    }
#line 7153 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 148:
#line 2231 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(mod);    }
#line 7159 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 149:
#line 2232 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 7165 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 150:
#line 2233 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(pow);    }
#line 7171 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 151:
#line 2234 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(not);    }
#line 7177 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 152:
#line 2235 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(neg);    }
#line 7183 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 153:
#line 2236 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(plus);   }
#line 7189 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 154:
#line 2237 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(minus);  }
#line 7195 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 155:
#line 2238 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aref);   }
#line 7201 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 156:
#line 2239 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(aset);   }
#line 7207 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 157:
#line 2240 "mrbgems/mruby-compiler/core/parse.y"
                                { (yyval.id) = intern_op(tick);   }
#line 7213 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 198:
#line 2258 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_asgn(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7221 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 199:
#line 2262 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, (yyvsp[-2].nd), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7229 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 200:
#line 2266 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-5].nd), intern_op(aref), (yyvsp[-3].nd), '.'), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7237 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 201:
#line 2270 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7245 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 202:
#line 2274 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, (yyvsp[-3].num)), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7253 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 203:
#line 2278 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_op_asgn(p, new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), 0, tCOLON2), (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 7261 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 204:
#line 2282 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7270 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 205:
#line 2287 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "constant re-assignment");
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7279 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 206:
#line 2292 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      backref_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_begin(p, 0);
                    }
#line 7288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 207:
#line 2297 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 208:
#line 2301 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7304 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 209:
#line 2305 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot2(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 210:
#line 2309 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7320 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 211:
#line 2313 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, (yyvsp[-1].nd), new_nil(p));
                    }
#line 7328 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 212:
#line 2317 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dot3(p, new_nil(p), (yyvsp[0].nd));
                    }
#line 7336 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 213:
#line 2321 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "+", (yyvsp[0].nd));
                    }
#line 7344 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 214:
#line 2325 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "-", (yyvsp[0].nd));
                    }
#line 7352 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 215:
#line 2329 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "*", (yyvsp[0].nd));
                    }
#line 7360 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 216:
#line 2333 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "/", (yyvsp[0].nd));
                    }
#line 7368 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 217:
#line 2337 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "%", (yyvsp[0].nd));
                    }
#line 7376 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 218:
#line 2341 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd));
                    }
#line 7384 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 219:
#line 2345 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 7392 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 220:
#line 2349 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, call_bin_op(p, (yyvsp[-2].nd), "**", (yyvsp[0].nd)));
                    }
#line 7400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 221:
#line 2353 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, (yyvsp[0].nd), "+@");
                    }
#line 7408 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 222:
#line 2357 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 7416 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 223:
#line 2361 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "|", (yyvsp[0].nd));
                    }
#line 7424 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 224:
#line 2365 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "^", (yyvsp[0].nd));
                    }
#line 7432 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 225:
#line 2369 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "&", (yyvsp[0].nd));
                    }
#line 7440 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 226:
#line 2373 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=>", (yyvsp[0].nd));
                    }
#line 7448 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 227:
#line 2377 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">", (yyvsp[0].nd));
                    }
#line 7456 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 228:
#line 2381 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">=", (yyvsp[0].nd));
                    }
#line 7464 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 229:
#line 2385 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<", (yyvsp[0].nd));
                    }
#line 7472 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 230:
#line 2389 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<=", (yyvsp[0].nd));
                    }
#line 7480 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 231:
#line 2393 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "==", (yyvsp[0].nd));
                    }
#line 7488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 232:
#line 2397 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "===", (yyvsp[0].nd));
                    }
#line 7496 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 233:
#line 2401 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!=", (yyvsp[0].nd));
                    }
#line 7504 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 234:
#line 2405 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "=~", (yyvsp[0].nd));
                    }
#line 7512 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 235:
#line 2409 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "!~", (yyvsp[0].nd));
                    }
#line 7520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 236:
#line 2413 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "!");
                    }
#line 7528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 237:
#line 2417 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[0].nd)), "~");
                    }
#line 7536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 238:
#line 2421 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), "<<", (yyvsp[0].nd));
                    }
#line 7544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 239:
#line 2425 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_bin_op(p, (yyvsp[-2].nd), ">>", (yyvsp[0].nd));
                    }
#line 7552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 240:
#line 2429 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_and(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 241:
#line 2433 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_or(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 242:
#line 2437 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7576 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 243:
#line 2441 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-5].nd)), (yyvsp[-3].nd), (yyvsp[0].nd));
                    }
#line 7584 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 244:
#line 2445 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      endless_method_name(p, (yyvsp[-3].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7597 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 245:
#line 2454 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      endless_method_name(p, (yyvsp[-5].nd));
                      void_expr_error(p, (yyvsp[-2].nd));
                      defn_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 7610 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 246:
#line 2463 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      void_expr_error(p, (yyvsp[0].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[0].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7623 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 247:
#line 2472 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-5].nd);
                      void_expr_error(p, (yyvsp[-2].nd));
                      defs_setup(p, (yyval.nd), (yyvsp[-4].nd), new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd)));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 7636 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 248:
#line 2481 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7644 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 250:
#line 2488 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7653 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 251:
#line 2493 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_hash(p, (yyvsp[-1].nd)));
                    }
#line 7661 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 252:
#line 2497 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_hash(p, (yyvsp[-1].nd)), 0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7670 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 253:
#line 2504 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7678 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 254:
#line 2508 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      (yyval.nd) = new_mod_rescue(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7687 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 255:
#line 2515 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7695 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 256:
#line 2519 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym r = intern_op(mul);
                      mrb_sym k = intern_op(pow);
                      mrb_sym b = intern_op(and);
                      (yyval.nd) = new_callargs(p, push((yyvsp[-3].nd), new_splat(p, new_lvar(p, r))),
                                        new_kw_hash(p, list1(cons(new_kw_rest_args(p, 0), new_lvar(p, k)))),
                                        new_block_arg(p, new_lvar(p, b)));
                    }
#line 7708 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 257:
#line 2528 "mrbgems/mruby-compiler/core/parse.y"
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
                        yyerror(p, "unexpected argument forwarding ...");
                        (yyval.nd) = 0;
                      }
                    }
#line 7727 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 262:
#line 2551 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-1].nd),0,0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7736 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 263:
#line 2556 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,(yyvsp[-3].nd),new_kw_hash(p,(yyvsp[-1].nd)),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7745 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 264:
#line 2561 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p,0,new_kw_hash(p,(yyvsp[-1].nd)),0);
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7754 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 265:
#line 2568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_callargs(p, list1((yyvsp[0].nd)), 0, 0);
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7764 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 266:
#line 2574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-1].nd), 0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 267:
#line 2579 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, new_kw_hash(p, (yyvsp[-1].nd)), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 7782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 268:
#line 2584 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, (yyvsp[-3].nd), new_kw_hash(p, (yyvsp[-1].nd)), (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-3].nd));
                    }
#line 7791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 269:
#line 2589 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_callargs(p, 0, 0, (yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7800 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 270:
#line 2595 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      (yyval.stack) = p->cmdarg_stack;
                      CMDARG_PUSH(1);
                    }
#line 7809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 271:
#line 2600 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7818 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 272:
#line 2607 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, (yyvsp[0].nd));
                    }
#line 7826 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 273:
#line 2611 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block_arg(p, 0);
                    }
#line 7834 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 274:
#line 2617 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 7842 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 275:
#line 2621 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 7850 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 277:
#line 2630 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7860 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 278:
#line 2636 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, new_lvar(p, intern_op(mul))));
                    }
#line 7868 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 279:
#line 2640 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 7877 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 280:
#line 2645 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7886 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 281:
#line 2650 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7894 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 282:
#line 2656 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 7903 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 283:
#line 2661 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-3].nd), new_splat(p, (yyvsp[0].nd)));
                    }
#line 7911 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 284:
#line 2665 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_splat(p, (yyvsp[0].nd)));
                    }
#line 7919 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 292:
#line 2678 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nvar(p, (yyvsp[0].num));
                    }
#line 7927 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 293:
#line 2682 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[0].id), 0);
                    }
#line 7935 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 294:
#line 2686 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7944 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 295:
#line 2692 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-2].stack);
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7953 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 296:
#line 2697 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 7962 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 297:
#line 2701 "mrbgems/mruby-compiler/core/parse.y"
                       {p->lstate = EXPR_ENDARG;}
#line 7968 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 298:
#line 2702 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->cmdarg_stack = (yyvsp[-3].stack);
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 7977 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 299:
#line 2706 "mrbgems/mruby-compiler/core/parse.y"
                              {p->lstate = EXPR_ENDARG;}
#line 7983 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 300:
#line 2707 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 7991 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 301:
#line 2711 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 7999 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 302:
#line 2715 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon2(p, (yyvsp[-2].nd), (yyvsp[0].id));
                    }
#line 8007 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 303:
#line 2719 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_colon3(p, (yyvsp[0].id));
                    }
#line 8015 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 304:
#line 2723 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_array(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8024 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 305:
#line 2728 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_hash(p, (yyvsp[-1].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[-1].nd));
                    }
#line 8033 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 306:
#line 2733 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_return(p, 0);
                    }
#line 8041 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 307:
#line 2737 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_yield(p, (yyvsp[0].nd));
                    }
#line 8049 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 308:
#line 2741 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, cond((yyvsp[-1].nd)), "!");
                    }
#line 8057 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 309:
#line 2745 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = call_uni_op(p, new_nil(p), "!");
                    }
#line 8065 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 310:
#line 2749 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), new_callargs(p, 0, 0, (yyvsp[0].nd)));
                    }
#line 8073 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 312:
#line 2754 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8082 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 313:
#line 2759 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lpar_beg;
                      p->lpar_beg = ++p->paren_nest;
                    }
#line 8093 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 314:
#line 2766 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.stack) = p->cmdarg_stack;
                      p->cmdarg_stack = 0;
                    }
#line 8102 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 315:
#line 2771 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lpar_beg = (yyvsp[-3].num);
                      (yyval.nd) = new_lambda(p, (yyvsp[-2].nd), (yyvsp[0].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                      p->cmdarg_stack = (yyvsp[-1].stack);
                      CMDARG_LEXPOP();
                    }
#line 8115 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 316:
#line 2783 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 8124 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 317:
#line 2791 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_unless(p, cond((yyvsp[-4].nd)), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                    }
#line 8133 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 318:
#line 2795 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 8139 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 319:
#line 2795 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 8145 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 320:
#line 2798 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_while(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 8154 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 321:
#line 2802 "mrbgems/mruby-compiler/core/parse.y"
                                {COND_PUSH(1);}
#line 8160 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 322:
#line 2802 "mrbgems/mruby-compiler/core/parse.y"
                                                              {COND_POP();}
#line 8166 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 323:
#line 2805 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_until(p, cond((yyvsp[-4].nd)), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-6].num));
                    }
#line 8175 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 324:
#line 2812 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 8183 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 325:
#line 2816 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_case(p, 0, (yyvsp[-1].nd));
                    }
#line 8191 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 326:
#line 2820 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_PUSH(1);}
#line 8197 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 327:
#line 2822 "mrbgems/mruby-compiler/core/parse.y"
                  {COND_POP();}
#line 8203 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 328:
#line 2825 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_for(p, (yyvsp[-7].nd), (yyvsp[-4].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-8].num));
                    }
#line 8212 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 329:
#line 2831 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "class definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8223 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 330:
#line 2839 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_class(p, (yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-5].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8234 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 331:
#line 2847 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = p->in_def;
                      p->in_def = 0;
                    }
#line 8243 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 332:
#line 2852 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(local_switch(p), nint(p->in_single));
                      nvars_block(p);
                      p->in_single = 0;
                    }
#line 8253 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 333:
#line 2859 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_sclass(p, (yyvsp[-5].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-7].num));
                      local_resume(p, (yyvsp[-2].nd)->car);
                      nvars_unnest(p);
                      p->in_def = (yyvsp[-4].num);
                      p->in_single = intn((yyvsp[-2].nd)->cdr);
                    }
#line 8266 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 334:
#line 2869 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (p->in_def || p->in_single)
                        yyerror(p, "module definition in method body");
                      (yyval.nd) = local_switch(p);
                      nvars_block(p);
                    }
#line 8277 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 335:
#line 2877 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_module(p, (yyvsp[-3].nd), (yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-4].num));
                      local_resume(p, (yyvsp[-2].nd));
                      nvars_unnest(p);
                    }
#line 8288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 336:
#line 2887 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defn_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                    }
#line 8299 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 337:
#line 2897 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-3].nd);
                      defs_setup(p, (yyval.nd), (yyvsp[-2].nd), (yyvsp[-1].nd));
                      nvars_unnest(p);
                      p->in_def--;
                      p->in_single--;
                    }
#line 8311 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 338:
#line 2905 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_break(p, 0);
                    }
#line 8319 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 339:
#line 2909 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_next(p, 0);
                    }
#line 8327 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 340:
#line 2913 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_redo(p);
                    }
#line 8335 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 341:
#line 2917 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_retry(p);
                    }
#line 8343 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 342:
#line 2923 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 8352 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 349:
#line 2942 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_if(p, cond((yyvsp[-3].nd)), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8360 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 351:
#line 2949 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8368 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 352:
#line 2955 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list1((yyvsp[0].nd)));
                    }
#line 8376 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 354:
#line 2962 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[0].nd),0,0);
                    }
#line 8384 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 355:
#line 2966 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-3].nd), new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8392 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 356:
#line 2970 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-5].nd), new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8400 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 357:
#line 2974 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3((yyvsp[-2].nd), nint(-1), 0);
                    }
#line 8409 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 358:
#line 2979 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3((yyvsp[-4].nd), nint(-1), (yyvsp[0].nd));
                    }
#line 8417 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 359:
#line 2983 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[0].id)), 0);
                    }
#line 8425 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 360:
#line 2987 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, new_arg(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 8433 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 361:
#line 2991 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                      (yyval.nd) = list3(0, nint(-1), 0);
                    }
#line 8442 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 362:
#line 2996 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, intern_op(mul));
                    }
#line 8450 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 363:
#line 3000 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list3(0, nint(-1), (yyvsp[0].nd));
                    }
#line 8458 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 364:
#line 3006 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8466 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 365:
#line 3010 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 8474 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 366:
#line 3014 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 8482 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 367:
#line 3018 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 8490 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 368:
#line 3024 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8498 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 369:
#line 3028 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 8506 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 370:
#line 3034 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8514 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 371:
#line 3038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8522 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 372:
#line 3042 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8530 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 373:
#line 3046 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8538 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 374:
#line 3050 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8546 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 375:
#line 3054 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-2].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8554 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 376:
#line 3058 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8562 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 377:
#line 3062 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8570 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 378:
#line 3066 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8578 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 379:
#line 3070 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8586 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 380:
#line 3074 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 8594 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 381:
#line 3078 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8602 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 382:
#line 3082 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 8610 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 383:
#line 3086 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8618 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 384:
#line 3090 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 8626 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 385:
#line 3096 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8635 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 386:
#line 3101 "mrbgems/mruby-compiler/core/parse.y"
                   {
                      p->cmd_start = TRUE;
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8644 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 387:
#line 3107 "mrbgems/mruby-compiler/core/parse.y"
                      {local_add_blk(p, 0);}
#line 8650 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 388:
#line 3108 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8658 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 389:
#line 3112 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_blk(p, 0);
                      (yyval.nd) = 0;
                    }
#line 8667 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 390:
#line 3117 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8675 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 391:
#line 3124 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8683 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 392:
#line 3128 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 8691 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 395:
#line 3138 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      new_bv(p, (yyvsp[0].id));
                    }
#line 8700 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 397:
#line 3146 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-2].nd);
                    }
#line 8708 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 398:
#line 3150 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8716 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 399:
#line 3156 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8724 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 400:
#line 3160 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 401:
#line 3166 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                    }
#line 8741 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 402:
#line 3173 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8751 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 403:
#line 3181 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if (typen((yyvsp[-1].nd)->car) == NODE_YIELD) {
                        yyerror(p, "block given to yield");
                      }
                      else {
                        call_with_block(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                      }
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 8765 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 404:
#line 3191 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8773 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 405:
#line 3195 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8782 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 406:
#line 3200 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-4].nd), (yyvsp[-2].id), (yyvsp[-1].nd), (yyvsp[-3].num));
                      call_with_block(p, (yyval.nd), (yyvsp[0].nd));
                    }
#line 8791 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 407:
#line 3207 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, (yyvsp[-1].id), (yyvsp[0].nd));
                    }
#line 8799 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 408:
#line 3211 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), (yyvsp[-2].num));
                    }
#line 8807 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 409:
#line 3215 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), (yyvsp[-1].id), (yyvsp[0].nd), tCOLON2);
                    }
#line 8815 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 410:
#line 3219 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), (yyvsp[0].id), 0, tCOLON2);
                    }
#line 8823 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 411:
#line 3223 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), (yyvsp[-1].num));
                    }
#line 8831 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 412:
#line 3227 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-2].nd), MRB_SYM_2(p->mrb, call), (yyvsp[0].nd), tCOLON2);
                    }
#line 8839 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 413:
#line 3231 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_super(p, (yyvsp[0].nd));
                    }
#line 8847 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 414:
#line 3235 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_zsuper(p);
                    }
#line 8855 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 415:
#line 3239 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_call(p, (yyvsp[-3].nd), intern_op(aref), (yyvsp[-1].nd), '.');
                    }
#line 8863 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 416:
#line 3245 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8873 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 417:
#line 3252 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8884 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 418:
#line 3259 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                      nvars_nest(p);
                      (yyval.num) = p->lineno;
                    }
#line 8894 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 419:
#line 3266 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_block(p,(yyvsp[-2].nd),(yyvsp[-1].nd));
                      SET_LINENO((yyval.nd), (yyvsp[-3].num));
                      local_unnest(p);
                      nvars_unnest(p);
                    }
#line 8905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 420:
#line 3277 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(cons((yyvsp[-3].nd), (yyvsp[-1].nd)), (yyvsp[0].nd));
                    }
#line 8913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 421:
#line 3283 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      if ((yyvsp[0].nd)) {
                        (yyval.nd) = cons(cons(0, (yyvsp[0].nd)), 0);
                      }
                      else {
                        (yyval.nd) = 0;
                      }
                    }
#line 8926 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 423:
#line 3297 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(list3((yyvsp[-4].nd), (yyvsp[-3].nd), (yyvsp[-1].nd)));
                      if ((yyvsp[0].nd)) (yyval.nd) = append((yyval.nd), (yyvsp[0].nd));
                    }
#line 8935 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 425:
#line 3305 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 8943 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 428:
#line 3313 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8951 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 430:
#line 3320 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8959 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 437:
#line 3334 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = concat_string(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8967 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 440:
#line 3342 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 8975 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 441:
#line 3346 "mrbgems/mruby-compiler/core/parse.y"
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
#line 8990 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 443:
#line 3360 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = append((yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 8998 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 444:
#line 3366 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9006 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 445:
#line 3370 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 9014 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 446:
#line 3375 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p,(yyvsp[-2].nd));
                      (yyval.nd) = list2((yyvsp[-3].nd), (yyvsp[-1].nd));
                    }
#line 9023 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 447:
#line 3380 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 9031 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 448:
#line 3384 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1(new_literal_delim(p));
                    }
#line 9039 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 449:
#line 3390 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9047 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 450:
#line 3394 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9062 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 451:
#line 3407 "mrbgems/mruby-compiler/core/parse.y"
                    {
                        (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9070 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 452:
#line 3411 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_dregx(p, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9078 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 456:
#line 3424 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, new_str(p, "", 0));
                      heredoc_end(p);
                    }
#line 9088 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 457:
#line 3430 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      heredoc_end(p);
                    }
#line 9096 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 460:
#line 3440 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(info->doc, (yyvsp[0].nd));
                      heredoc_treat_nextline(p);
                    }
#line 9106 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 461:
#line 3446 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push_strterm(p);
                    }
#line 9114 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 462:
#line 3451 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      pop_strterm(p, (yyvsp[-2].nd));
                      parser_heredoc_info *info = parsing_heredoc_info(p);
                      info->doc = push(push(info->doc, (yyvsp[-3].nd)), (yyvsp[-1].nd));
                    }
#line 9124 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 463:
#line 3459 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_words(p, list1((yyvsp[0].nd)));
                    }
#line 9132 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 464:
#line 3463 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9147 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 465:
#line 3477 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_ENDARG;
                      (yyval.nd) = new_sym(p, (yyvsp[0].id));
                    }
#line 9156 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 466:
#line 3482 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9172 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 467:
#line 3496 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9180 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 472:
#line 3506 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 9188 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 473:
#line 3510 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = new_strsym(p, (yyvsp[0].nd));
                    }
#line 9196 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 474:
#line 3516 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_symbols(p, list1((yyvsp[0].nd)));
                    }
#line 9204 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 475:
#line 3520 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      node *n = (yyvsp[-1].nd);
                      if (intn((yyvsp[0].nd)->cdr->cdr) > 0) {
                        n = push(n, (yyvsp[0].nd));
                      }
                      (yyval.nd) = new_symbols(p, n);
                    }
#line 9216 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 478:
#line 3532 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 9224 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 479:
#line 3536 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_negate(p, (yyvsp[0].nd));
                    }
#line 9232 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 480:
#line 3542 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_lvar(p, (yyvsp[0].id));
                    }
#line 9240 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 481:
#line 3546 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_ivar(p, (yyvsp[0].id));
                    }
#line 9248 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 482:
#line 3550 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_gvar(p, (yyvsp[0].id));
                    }
#line 9256 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 483:
#line 3554 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_cvar(p, (yyvsp[0].id));
                    }
#line 9264 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 484:
#line 3558 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_const(p, (yyvsp[0].id));
                    }
#line 9272 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 485:
#line 3564 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      assignable(p, (yyvsp[0].nd));
                    }
#line 9280 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 486:
#line 3568 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "can't assign to numbered parameter");
                    }
#line 9288 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 487:
#line 3574 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = var_reference(p, (yyvsp[0].nd));
                    }
#line 9296 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 488:
#line 3578 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_nil(p);
                    }
#line 9304 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 489:
#line 3582 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_self(p);
                    }
#line 9312 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 490:
#line 3586 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_true(p);
                    }
#line 9320 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 491:
#line 3590 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_false(p);
                    }
#line 9328 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 492:
#line 3594 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      const char *fn = mrb_sym_name_len(p->mrb, p->filename_sym, NULL);
                      if (!fn) {
                        fn = "(null)";
                      }
                      (yyval.nd) = new_str(p, fn, strlen(fn));
                    }
#line 9340 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 493:
#line 3602 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      char buf[16];

                      dump_int(p->lineno, buf);
                      (yyval.nd) = new_int(p, buf, 10, 0);
                    }
#line 9351 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 494:
#line 3609 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_fcall(p, MRB_SYM_2(p->mrb, __ENCODING__), 0);
                    }
#line 9359 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 497:
#line 3619 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 9367 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 498:
#line 3623 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9376 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 499:
#line 3628 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9384 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 502:
#line 3644 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                      p->lstate = EXPR_BEG;
                      p->cmd_start = TRUE;
                    }
#line 9394 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 503:
#line 3650 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 9402 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 504:
#line 3654 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 9410 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 506:
#line 3661 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9418 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 507:
#line 3665 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, (yyvsp[-3].nd));
                    }
#line 9426 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 508:
#line 3669 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_dots(p, 0);
                    }
#line 9434 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 509:
#line 3675 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 9442 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 510:
#line 3679 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_nest(p);
                    }
#line 9450 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 511:
#line 3685 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9460 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 512:
#line 3691 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9469 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 513:
#line 3698 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = new_kw_arg(p, (yyvsp[-1].id), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9479 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 514:
#line 3704 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_arg(p, (yyvsp[0].id), 0);
                      local_unnest(p);
                    }
#line 9488 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 515:
#line 3711 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9496 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 516:
#line 3715 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9504 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 517:
#line 3721 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9512 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 518:
#line 3725 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9520 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 521:
#line 3735 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, (yyvsp[0].id));
                    }
#line 9528 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 522:
#line 3739 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_kw_rest_args(p, 0);
                    }
#line 9536 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 523:
#line 3745 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-3].nd), (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9544 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 524:
#line 3749 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, (yyvsp[-1].nd), 0, (yyvsp[0].id));
                    }
#line 9552 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 525:
#line 3753 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, (yyvsp[-1].nd), (yyvsp[0].id));
                    }
#line 9560 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 526:
#line 3757 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, (yyvsp[0].id));
                    }
#line 9568 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 527:
#line 3763 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                    }
#line 9576 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 528:
#line 3767 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args_tail(p, 0, 0, 0);
                    }
#line 9584 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 529:
#line 3773 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9592 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 530:
#line 3777 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-7].nd), (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9600 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 531:
#line 3781 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9608 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 532:
#line 3785 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9616 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 533:
#line 3789 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-3].nd), 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9624 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 534:
#line 3793 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-5].nd), 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9632 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 535:
#line 3797 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, (yyvsp[-1].nd), 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9640 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 536:
#line 3801 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9648 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 537:
#line 3805 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-5].nd), (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9656 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 538:
#line 3809 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-1].nd), 0, 0, (yyvsp[0].nd));
                    }
#line 9664 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 539:
#line 3813 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, (yyvsp[-3].nd), 0, (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9672 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 540:
#line 3817 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-1].id), 0, (yyvsp[0].nd));
                    }
#line 9680 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 541:
#line 3821 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, (yyvsp[-3].id), (yyvsp[-1].nd), (yyvsp[0].nd));
                    }
#line 9688 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 542:
#line 3825 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, (yyvsp[0].nd));
                    }
#line 9696 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 543:
#line 3829 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, 0);
                      (yyval.nd) = new_args(p, 0, 0, 0, 0, 0);
                    }
#line 9705 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 544:
#line 3836 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a constant");
                      (yyval.nd) = 0;
                    }
#line 9714 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 545:
#line 3841 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be an instance variable");
                      (yyval.nd) = 0;
                    }
#line 9723 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 546:
#line 3846 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a global variable");
                      (yyval.nd) = 0;
                    }
#line 9732 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 547:
#line 3851 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a class variable");
                      (yyval.nd) = 0;
                    }
#line 9741 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 548:
#line 3856 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      yyerror(p, "formal argument cannot be a numbered parameter");
                      (yyval.nd) = 0;
                    }
#line 9750 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 549:
#line 3863 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9758 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 550:
#line 3867 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9767 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 551:
#line 3874 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_arg(p, (yyvsp[0].id));
                    }
#line 9775 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 552:
#line 3878 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = local_switch(p);
                    }
#line 9783 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 553:
#line 3882 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = new_masgn_param(p, (yyvsp[-1].nd), p->locals->car);
                      local_resume(p, (yyvsp[-2].nd));
                      local_add_f(p, 0);
                    }
#line 9793 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 554:
#line 3890 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9801 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 555:
#line 3894 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9809 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 556:
#line 3900 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[-1].id));
                      local_nest(p);
                      (yyval.id) = (yyvsp[-1].id);
                    }
#line 9819 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 557:
#line 3908 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9829 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 558:
#line 3916 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(nsym((yyvsp[-1].id)), cons((yyvsp[0].nd), locals_node(p)));
                      local_unnest(p);
                    }
#line 9839 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 559:
#line 3924 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9847 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 560:
#line 3928 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9855 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 561:
#line 3934 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                    }
#line 9863 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 562:
#line 3938 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9871 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 565:
#line 3948 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      local_add_f(p, (yyvsp[0].id));
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9880 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 566:
#line 3953 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(mul);
                      local_add_f(p, (yyval.id));
                    }
#line 9889 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 569:
#line 3964 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9897 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 570:
#line 3968 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = intern_op(and);
                    }
#line 9905 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 571:
#line 3974 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = (yyvsp[0].id);
                    }
#line 9913 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 572:
#line 3978 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.id) = 0;
                    }
#line 9921 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 573:
#line 3984 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[0].nd);
                      if (!(yyval.nd)) (yyval.nd) = new_nil(p);
                    }
#line 9930 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 574:
#line 3988 "mrbgems/mruby-compiler/core/parse.y"
                      {p->lstate = EXPR_BEG;}
#line 9936 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 575:
#line 3989 "mrbgems/mruby-compiler/core/parse.y"
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
#line 9963 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 577:
#line 4015 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = (yyvsp[-1].nd);
                    }
#line 9971 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 578:
#line 4021 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = list1((yyvsp[0].nd));
                      NODE_LINENO((yyval.nd), (yyvsp[0].nd));
                    }
#line 9980 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 579:
#line 4026 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = push((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9988 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 580:
#line 4032 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[-2].nd));
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons((yyvsp[-2].nd), (yyvsp[0].nd));
                    }
#line 9998 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 581:
#line 4038 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-2].id)), (yyvsp[0].nd));
                    }
#line 10007 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 582:
#line 4043 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_sym(p, (yyvsp[-1].id)), label_reference(p, (yyvsp[-1].id)));
                    }
#line 10015 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 583:
#line 4047 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      mrb_sym sym = intern_numparam((yyvsp[-1].num));
                      (yyval.nd) = cons(new_sym(p, sym), label_reference(p, sym));
                    }
#line 10024 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 584:
#line 4052 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_sym(p, intern_numparam((yyvsp[-2].num))), (yyvsp[0].nd));
                    }
#line 10033 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 585:
#line 4057 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      if (typen((yyvsp[-2].nd)->car) == NODE_DSTR) {
                        (yyval.nd) = cons(new_dsym(p, (yyvsp[-2].nd)), (yyvsp[0].nd));
                      }
                      else {
                        (yyval.nd) = cons(new_sym(p, new_strsym(p, (yyvsp[-2].nd))), (yyvsp[0].nd));
                      }
                    }
#line 10047 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 586:
#line 4067 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      void_expr_error(p, (yyvsp[0].nd));
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), (yyvsp[0].nd));
                    }
#line 10056 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 587:
#line 4072 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = cons(new_kw_rest_args(p, 0), new_lvar(p, intern_op(pow)));
                    }
#line 10064 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 600:
#line 4098 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = '.';
                    }
#line 10072 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 601:
#line 4102 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = 0;
                    }
#line 10080 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 603:
#line 4109 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.num) = tCOLON2;
                    }
#line 10088 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 612:
#line 4130 "mrbgems/mruby-compiler/core/parse.y"
                      {yyerrok;}
#line 10094 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 614:
#line 4135 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      p->lineno += (yyvsp[0].num);
                      p->column = 0;
                    }
#line 10103 "mrbgems/mruby-compiler/core/y.tab.c"
    break;

  case 618:
#line 4147 "mrbgems/mruby-compiler/core/parse.y"
                    {
                      (yyval.nd) = 0;
                    }
#line 10111 "mrbgems/mruby-compiler/core/y.tab.c"
    break;


#line 10115 "mrbgems/mruby-compiler/core/y.tab.c"

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
#line 4151 "mrbgems/mruby-compiler/core/parse.y"

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
    } else {
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
            ++s;
            --len;
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
          yyerror(p, "can't find heredoc delimiter anywhere before EOF");
        } else {
          strcpy(buf, s1);
          strcat(buf, hinfo->term);
          strcat(buf, s2);
          yyerror(p, buf);
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
          if (nvar != -2) {     /* numbered parameters never appear on toplevel */
            if (nvar == -1) {
              yywarning(p, "numbered parameter used in inner block");
            }
            else {
              p->nvars->car = nint(nvar > n ? nvar : n);
            }
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
  p->no_ext_ops = cxt->no_ext_ops;
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
  cxt->syms = (mrb_sym*)mrb_realloc(p->mrb, cxt->syms, i*sizeof(mrb_sym));
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
  p = (parser_state*)mrb_pool_alloc(pool, sizeof(parser_state));
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
    mrb_free(p->mrb, p->tokbuf);
  }
  mrb_pool_close(p->pool);
}

MRB_API mrbc_context*
mrbc_context_new(mrb_state *mrb)
{
  return (mrbc_context*)mrb_calloc(mrb, 1, sizeof(mrbc_context));
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
    char *p = (char*)mrb_malloc(mrb, len + 1);

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
