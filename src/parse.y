/*
** parse.y - mruby parser
** 
** See Copyright Notice in mruby.h
*/

%{
#undef PARSER_TEST
#undef PARSER_DEBUG

#define YYDEBUG 1
#define YYERROR_VERBOSE 1
#define YYSTACK_USE_ALLOCA 0

#include "mruby.h"
#include "st.h"
#include "compile.h"
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <string.h>

#define YYLEX_PARAM p

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;

static int yylex(void *lval, parser_state *p);
static void yyerror(parser_state *p, const char *s);
static void yywarn(parser_state *p, const char *s);
static void yywarning(parser_state *p, const char *s);
static void backref_error(parser_state *p, node *n);

#define identchar(c) (isalnum(c) || (c) == '_' || !isascii(c))

#define TRUE  1
#define FALSE 0

typedef unsigned int stack_type;

#define BITSTACK_PUSH(stack, n)	((stack) = ((stack)<<1)|((n)&1))
#define BITSTACK_POP(stack)	((stack) = (stack) >> 1)
#define BITSTACK_LEXPOP(stack)	((stack) = ((stack) >> 1) | ((stack) & 1))
#define BITSTACK_SET_P(stack)	((stack)&1)

#define COND_PUSH(n)	BITSTACK_PUSH(p->cond_stack, (n))
#define COND_POP()	BITSTACK_POP(p->cond_stack)
#define COND_LEXPOP()	BITSTACK_LEXPOP(p->cond_stack)
#define COND_P()	BITSTACK_SET_P(p->cond_stack)

#define CMDARG_PUSH(n)	BITSTACK_PUSH(p->cmdarg_stack, (n))
#define CMDARG_POP()	BITSTACK_POP(p->cmdarg_stack)
#define CMDARG_LEXPOP()	BITSTACK_LEXPOP(p->cmdarg_stack)
#define CMDARG_P()	BITSTACK_SET_P(p->cmdarg_stack)

static mrb_sym
intern_gen(parser_state *p, const char *s)
{
  return mrb_intern(p->mrb, s);
}
#define intern(s) intern_gen(p,(s))

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
    longjmp(p->jmp, 1);
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
    c = parser_palloc(p, sizeof(mrb_ast_node));
  }

  c->car = car;
  c->cdr = cdr;
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
  while (c->cdr) {
    c = c->cdr;
  }
  if (b) {
    c->cdr = b;
  }
  return a;
}
#define append(a,b) append_gen(p,(a),(b))
#define push(a,b) append_gen(p,(a),list1(b))

static char*
parser_strndup(parser_state *p, const char *s, size_t len)
{
  char *b = parser_palloc(p, len+1);

  memcpy(b, s, len);
  b[len] = '\0';
  return b;
}
#define strndup(s,len) parser_strndup(p, s, len)

static char*
parser_strdup(parser_state *p, const char *s)
{
  return parser_strndup(p, s, strlen(s));
}
#undef strdup
#define strdup(s) parser_strdup(p, s)

// xxx -----------------------------

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
  p->locals = p->locals->cdr;
}

static int
local_var_p(parser_state *p, mrb_sym sym)
{
  node *l = p->locals;

  while (l) {
    node *n = l->car;
    while (n) {
      if ((mrb_sym)n->car == sym) return 1;
      n = n->cdr;
    }
    l = l->cdr;
  }
  return 0;
}

static void
local_add_f(parser_state *p, mrb_sym sym)
{
  p->locals->car = push(p->locals->car, (node*)sym);
}

static void
local_add(parser_state *p, mrb_sym sym)
{
  if (!local_var_p(p, sym)) {
    local_add_f(p, sym);
  }
}

// (:scope (vars..) (prog...))
static node*
new_scope(parser_state *p, node *body)
{
  return cons((node*)NODE_SCOPE, cons(p->locals->car, body));
}

// (:begin prog...)
static node*
new_begin(parser_state *p, node *body)
{
  if (body) 
    return list2((node*)NODE_BEGIN, body);
  return cons((node*)NODE_BEGIN, 0);
}

#define newline_node(n) (n)

// (:rescue body rescue else)
static node*
new_rescue(parser_state *p, node *body, node *resq, node *els)
{
  return list4((node*)NODE_RESCUE, body, resq, els);
}

// (:ensure body ensure)
static node*
new_ensure(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_ENSURE, cons(a, cons(0, b)));
}

// (:nil)
static node*
new_nil(parser_state *p)
{
  return list1((node*)NODE_NIL);
}

// (:true)
static node*
new_true(parser_state *p)
{
  return list1((node*)NODE_TRUE);
}

// (:false)
static node*
new_false(parser_state *p)
{
  return list1((node*)NODE_FALSE);
}

// (:alias new old)
static node*
new_alias(parser_state *p, mrb_sym a, mrb_sym b)
{
  return cons((node*)NODE_ALIAS, cons((node*)a, (node*)b));
}

// (:if cond then else)
static node*
new_if(parser_state *p, node *a, node *b, node *c)
{
  return list4((node*)NODE_IF, a, b, c);
}

// (:unless cond then else)
static node*
new_unless(parser_state *p, node *a, node *b, node *c)
{
  return list4((node*)NODE_IF, a, c, b);
}

// (:while cond body)
static node*
new_while(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_WHILE, cons(a, b));
}

// (:until cond body)
static node*
new_until(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_UNTIL, cons(a, b));
}

// (:for var obj body)
static node*
new_for(parser_state *p, node *v, node *o, node *b)
{
  return list4((node*)NODE_FOR, v, o, b);
}

// (:case a ((when ...) body) ((when...) body))
static node*
new_case(parser_state *p, node *a, node *b)
{
  node *n = list2((node*)NODE_CASE, a);
  node *n2 = n;

  while (n2->cdr) {
    n2 = n2->cdr;
  }
  n2->cdr = b;
  return n;
}

// (:postexe a)
static node*
new_postexe(parser_state *p, node *a)
{
  return cons((node*)NODE_POSTEXE, a);
}

// (:self)
static node*
new_self(parser_state *p)
{
  return list1((node*)NODE_SELF);
}

// (:call a b c)
static node*
new_call(parser_state *p, node *a, mrb_sym b, node *c)
{
  return list4((node*)NODE_CALL, a, (node*)b, c);
}

// (:fcall self mid args)
static node*
new_fcall(parser_state *p, mrb_sym b, node *c)
{
  return list4((node*)NODE_FCALL, new_self(p), (node*)b, c);
}

#if 0
// (:vcall self mid)
static node*
new_vcall(parser_state *p, mrb_sym b)
{
  return list3((node*)NODE_VCALL, new_self(p), (node*)b);
}
#endif

// (:super . c)
static node*
new_super(parser_state *p, node *c)
{
  return cons((node*)NODE_SUPER, c);
}

// (:zsuper)
static node*
new_zsuper(parser_state *p)
{
  return list1((node*)NODE_ZSUPER);
}

// (:yield . c)
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

// (:return . c)
static node*
new_return(parser_state *p, node *c)
{
  return cons((node*)NODE_RETURN, c);
}

// (:break . c)
static node*
new_break(parser_state *p, node *c)
{
  return cons((node*)NODE_BREAK, c);
}

// (:next . c)
static node*
new_next(parser_state *p, node *c)
{
  return cons((node*)NODE_NEXT, c);
}

// (:redo)
static node*
new_redo(parser_state *p)
{
  return list1((node*)NODE_REDO);
}

// (:retry)
static node*
new_retry(parser_state *p)
{
  return list1((node*)NODE_RETRY);
}

// (:dot2 a b)
static node*
new_dot2(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT2, cons(a, b));
}

// (:dot3 a b)
static node*
new_dot3(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_DOT3, cons(a, b));
}

// (:colon2 b c)
static node*
new_colon2(parser_state *p, node *b, mrb_sym c)
{
  return cons((node*)NODE_COLON2, cons(b, (node*)c));
}

// (:colon3 . c)
static node*
new_colon3(parser_state *p, mrb_sym c)
{
  return cons((node*)NODE_COLON3, (node*)c);
}

// (:and a b)
static node*
new_and(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_AND, cons(a, b));
}

// (:or a b)
static node*
new_or(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_OR, cons(a, b));
}

// (:array a...)
static node*
new_array(parser_state *p, node *a)
{
  return cons((node*)NODE_ARRAY, a);
}

// (:splat . a)
static node*
new_splat(parser_state *p, node *a)
{
  return cons((node*)NODE_SPLAT, a);
}

// (:hash (k . v) (k . v)...)
static node*
new_hash(parser_state *p, node *a)
{
  return cons((node*)NODE_HASH, a);
}

// (:sym . a)
static node*
new_sym(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_SYM, (node*)sym);
}

// (:lvar . a)
static node*
new_lvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_LVAR, (node*)sym);
}

// (:gvar . a)
static node*
new_gvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_GVAR, (node*)sym);
}

// (:ivar . a)
static node*
new_ivar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_IVAR, (node*)sym);
}

// (:cvar . a)
static node*
new_cvar(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CVAR, (node*)sym);
}

// (:const . a)
static node*
new_const(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_CONST, (node*)sym);
}

// (:undef a...)
static node*
new_undef(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_UNDEF, (node*)sym);
}

// (:class class super body)
static node*
new_class(parser_state *p, node *c, node *s, node *b)
{
  return list4((node*)NODE_CLASS, c, s, cons(p->locals->car, b));
}

// (:sclass obj body)
static node*
new_sclass(parser_state *p, node *o, node *b)
{
  return list3((node*)NODE_SCLASS, o, cons(p->locals->car, b));
}

// (:module module body)
static node*
new_module(parser_state *p, node *m, node *b)
{
  return list3((node*)NODE_MODULE, m, cons(p->locals->car, b));
}

// (:def m lv (arg . body))
static node*
new_def(parser_state *p, mrb_sym m, node *a, node *b)
{
  return list5((node*)NODE_DEF, (node*)m, p->locals->car, a, b);
}

// (:sdef obj m lv (arg . body))
static node*
new_sdef(parser_state *p, node *o, mrb_sym m, node *a, node *b)
{
  return list6((node*)NODE_SDEF, o, (node*)m, p->locals->car, a, b);
}

// (:arg . sym)
static node*
new_arg(parser_state *p, mrb_sym sym)
{
  return cons((node*)NODE_ARG, (node*)sym);
}

// (m o r m2 b)
// m: (a b c)
// o: ((a . e1) (b . e2))
// r: a
// m2: (a b c)
// b: a
static node*
new_args(parser_state *p, node *m, node *opt, mrb_sym rest, node *m2, mrb_sym blk)
{
  node *n;

  n = cons(m2, (node*)blk);
  n = cons((node*)rest, n);
  n = cons(opt, n);
  return cons(m, n);
}

// (:block_arg . a)
static node*
new_block_arg(parser_state *p, node *a)
{
  return cons((node*)NODE_BLOCK_ARG, a);
}

// (:block arg body)
static node*
new_block(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_BLOCK, p->locals->car, a, b);
}

// (:lambda arg body)
static node*
new_lambda(parser_state *p, node *a, node *b)
{
  return list4((node*)NODE_LAMBDA, p->locals->car, a, b);
}

// (:asgn lhs rhs)
static node*
new_asgn(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_ASGN, cons(a, b));
}

// (:masgn mlhs=(pre rest post)  mrhs)
static node*
new_masgn(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_MASGN, cons(a, b));
}

// (:asgn lhs rhs)
static node*
new_op_asgn(parser_state *p, node *a, mrb_sym op, node *b)
{
  return list4((node*)NODE_OP_ASGN, a, (node*)op, b);
}

// (:int . i)
static node*
new_int(parser_state *p, const char *s, int base)
{
  return list3((node*)NODE_INT, (node*)strdup(s), (node*)(intptr_t)base);
}

// (:float . i)
static node*
new_float(parser_state *p, const char *s)
{
  return cons((node*)NODE_FLOAT, (node*)strdup(s));
}

// (:str . (s . len))
static node*
new_str(parser_state *p, const char *s, size_t len)
{
  return cons((node*)NODE_STR, cons((node*)strndup(s, len), (node*)len));
}

// (:dstr . a)
static node*
new_dstr(parser_state *p, node *a)
{
  return cons((node*)NODE_DSTR, a);
}

// (:backref . n)
static node*
new_back_ref(parser_state *p, int n)
{
  return cons((node*)NODE_BACK_REF, (node*)(intptr_t)n);
}

// (:nthref . n)
static node*
new_nth_ref(parser_state *p, int n)
{
  return cons((node*)NODE_NTH_REF, (node*)(intptr_t)n);
}

static void
new_bv(parser_state *p, mrb_sym id)
{
}

// xxx -----------------------------

// (:call a op)
static node*
call_uni_op(parser_state *p, node *recv, char *m)
{
  return new_call(p, recv, intern(m), 0);
}

// (:call a op b)
static node*
call_bin_op(parser_state *p, node *recv, char *m, node *arg1)
{
  return new_call(p, recv, intern(m), list1(list1(arg1)));
}

// (:match (a . b))
static node*
match_op(parser_state *p, node *a, node *b)
{
  return cons((node*)NODE_MATCH, cons((node*)a, (node*)b));
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
  node *n = a->cdr->cdr->cdr;

  if (!n->car) n->car = cons(0, b);
  else {
    args_with_block(p, n->car, b);
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
  }
  if (!n->car->cdr) return n->car->car;
  return new_array(p, n->car);
}

static void
assignable(parser_state *p, node *lhs)
{
  switch ((int)(intptr_t)lhs->car) {
  case NODE_LVAR:
    local_add(p, (mrb_sym)lhs->cdr);
    break;
  default:
    break;
  }
}

static node*
var_reference(parser_state *p, node *lhs)
{
  node *n;

  switch ((int)(intptr_t)lhs->car) {
  case NODE_LVAR:
    if (!local_var_p(p, (mrb_sym)lhs->cdr)) {
      n = new_fcall(p, (mrb_sym)lhs->cdr, 0);
      cons_free(lhs);
      return n;
    }
    break;
  default:
    break;
  }
  return lhs;
}

// xxx -----------------------------

%}

%pure_parser
%parse-param {parser_state *p}
%lex-param {parser_state *p}

%union {
    node *node;
    mrb_sym id;
    int num;
    unsigned int stack;
    const struct vtable *vars;
}

%token
	keyword_class
	keyword_module
	keyword_def
	keyword_undef
	keyword_begin
	keyword_rescue
	keyword_ensure
	keyword_end
	keyword_if
	keyword_unless
	keyword_then
	keyword_elsif
	keyword_else
	keyword_case
	keyword_when
	keyword_while
	keyword_until
	keyword_for
	keyword_break
	keyword_next
	keyword_redo
	keyword_retry
	keyword_in
	keyword_do
	keyword_do_cond
	keyword_do_block
	keyword_do_LAMBDA
	keyword_return
	keyword_yield
	keyword_super
	keyword_self
	keyword_nil
	keyword_true
	keyword_false
	keyword_and
	keyword_or
	keyword_not
	modifier_if
	modifier_unless
	modifier_while
	modifier_until
	modifier_rescue
	keyword_alias
	keyword_BEGIN
	keyword_END
	keyword__LINE__
	keyword__FILE__
	keyword__ENCODING__

%token <id>   tIDENTIFIER tFID tGVAR tIVAR tCONSTANT tCVAR tLABEL
%token <node> tINTEGER tFLOAT tCHAR tREGEXP
%token <node> tSTRING tSTRING_PART
%token <node> tNTH_REF tBACK_REF
%token <num>  tREGEXP_END

%type <node> singleton string string_interp regexp
%type <node> literal numeric cpath
%type <node> top_compstmt top_stmts top_stmt
%type <node> bodystmt compstmt stmts stmt expr arg primary command command_call method_call
%type <node> expr_value arg_value primary_value
%type <node> if_tail opt_else case_body cases opt_rescue exc_list exc_var opt_ensure
%type <node> args call_args opt_call_args
%type <node> paren_args opt_paren_args variable
%type <node> command_args aref_args opt_block_arg block_arg var_ref var_lhs
%type <node> command_asgn mrhs superclass block_call block_command
%type <node> f_block_optarg f_block_opt
%type <node> f_arglist f_args f_arg f_arg_item f_optarg f_marg f_marg_list f_margs
%type <node> assoc_list assocs assoc undef_list backref for_var
%type <node> block_param opt_block_param block_param_def f_opt
%type <node> bv_decls opt_bv_decl bvar f_larglist lambda_body
%type <node> brace_block cmd_brace_block do_block lhs none fitem f_bad_arg
%type <node> mlhs mlhs_list mlhs_post mlhs_basic mlhs_item mlhs_node mlhs_inner
%type <id>   fsym sym symbol operation operation2 operation3
%type <id>   cname fname op f_rest_arg f_block_arg opt_f_block_arg f_norm_arg

%token tUPLUS		/* unary+ */
%token tUMINUS		/* unary- */
%token tPOW		/* ** */
%token tCMP		/* <=> */
%token tEQ		/* == */
%token tEQQ		/* === */
%token tNEQ		/* != */
%token tGEQ		/* >= */
%token tLEQ		/* <= */
%token tANDOP tOROP	/* && and || */
%token tMATCH tNMATCH	/* =~ and !~ */
%token tDOT2 tDOT3	/* .. and ... */
%token tAREF tASET	/* [] and []= */
%token tLSHFT tRSHFT	/* << and >> */
%token tCOLON2		/* :: */
%token tCOLON3		/* :: at EXPR_BEG */
%token <id> tOP_ASGN	/* +=, -=  etc. */
%token tASSOC		/* => */
%token tLPAREN		/* ( */
%token tLPAREN_ARG	/* ( */
%token tRPAREN		/* ) */
%token tLBRACK		/* [ */
%token tLBRACE		/* { */
%token tLBRACE_ARG	/* { */
%token tSTAR		/* * */
%token tAMPER		/* & */
%token tLAMBDA		/* -> */
%token tSYMBEG tREGEXP_BEG tWORDS_BEG tQWORDS_BEG
%token tSTRING_BEG tSTRING_DVAR tLAMBEG

/*
 *	precedence table
 */

%nonassoc tLOWEST
%nonassoc tLBRACE_ARG

%nonassoc  modifier_if modifier_unless modifier_while modifier_until
%left  keyword_or keyword_and
%right keyword_not
%right '=' tOP_ASGN
%left modifier_rescue
%right '?' ':'
%nonassoc tDOT2 tDOT3
%left  tOROP
%left  tANDOP
%nonassoc  tCMP tEQ tEQQ tNEQ tMATCH tNMATCH
%left  '>' tGEQ '<' tLEQ
%left  '|' '^'
%left  '&'
%left  tLSHFT tRSHFT
%left  '+' '-'
%left  '*' '/' '%'
%right tUMINUS_NUM tUMINUS
%right tPOW
%right '!' '~' tUPLUS

%nonassoc idNULL
%nonassoc idRespond_to
%nonassoc idIFUNC
%nonassoc idCFUNC
%nonassoc id_core_set_method_alias
%nonassoc id_core_set_variable_alias
%nonassoc id_core_undef_method
%nonassoc id_core_define_method
%nonassoc id_core_define_singleton_method
%nonassoc id_core_set_postexe

%token tLAST_TOKEN

%%
program		:  {
		     p->lstate = EXPR_BEG;
		     local_nest(p);
		   }
		  top_compstmt
		    {
		      p->tree = new_scope(p, $2);
		     local_unnest(p);
		    }
		;

top_compstmt	: top_stmts opt_terms
		    {
		      $$ = $1;
		    }
		;

top_stmts	: none
                    {
		      $$ = new_begin(p, 0);
		    }
		| top_stmt
		    {
		      $$ = new_begin(p, $1);
		    }
		| top_stmts terms top_stmt
		    {
		      $$ = push($1, newline_node($3));
		    }
		| error top_stmt
		    {
		      $$ = $2;
		    }
		;

top_stmt	: stmt
		| keyword_BEGIN
		    {
		      if (p->in_def || p->in_single) {
			yyerror(p, "BEGIN in method");
		      }
		      $<node>$ = local_switch(p);
		    }
		  '{' top_compstmt '}'
		    {
		      p->begin_tree = push(p->begin_tree, $4);
		      local_resume(p, $<node>2);
		      $$ = 0;
		    }
		;

bodystmt	: compstmt
		  opt_rescue
		  opt_else
		  opt_ensure
		    {
		      if ($2) {
			$$ = new_rescue(p, $1, $2, $3);
		      }
		      else if ($3) {
			yywarn(p, "else without rescue is useless");
			$$ = append($$, $3);
		      }
		      else {
			$$ = $1;
		      }
		      if ($4) {
			if ($$) {
			  $$ = new_ensure(p, $$, $4);
			}
			else {
			  $$ = push($4, new_nil(p));
			}
		      }
		    }
		;

compstmt	: stmts opt_terms
		    {
		      $$ = $1;
		    }
		;

stmts		: none
                    {
		      $$ = new_begin(p, 0);
		    }
		| stmt
		    {
		      $$ = new_begin(p, $1);
		    }
		| stmts terms stmt
		    {
			$$ = push($1, newline_node($3));
		    }
		| error stmt
		    {
		      $$ = new_begin(p, $2);
		    }
		;

stmt		: keyword_alias fsym {p->lstate = EXPR_FNAME;} fsym
		    {
		      $$ = new_alias(p, $2, $4);
		    }
		| keyword_undef undef_list
		    {
		      $$ = $2;
		    }
		| stmt modifier_if expr_value
		    {
			$$ = new_if(p, cond($3), $1, 0);
		    }
		| stmt modifier_unless expr_value
		    {
		      $$ = new_unless(p, cond($3), $1, 0);
		    }
		| stmt modifier_while expr_value
		    {
		      $$ = new_while(p, cond($3), $1);
		    }
		| stmt modifier_until expr_value
		    {
		      $$ = new_until(p, cond($3), $1);
		    }
		| stmt modifier_rescue stmt
		    {
		      $$ = new_rescue(p, $1, list1(list3(0, 0, $3)), 0);
		    }
		| keyword_END '{' compstmt '}'
		    {
		      if (p->in_def || p->in_single) {
			yywarn(p, "END in method; use at_exit");
		      }
		      $$ = new_postexe(p, $3);
		    }
		| command_asgn
		| mlhs '=' command_call
		    {
		      $$ = new_masgn(p, $1, list1($3));
		    }
		| var_lhs tOP_ASGN command_call
		    {
		      $$ = new_op_asgn(p, $1, $2, $3);
		    }
		| primary_value '[' opt_call_args rbracket tOP_ASGN command_call
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, intern("[]"), $3), $5, $6);
		    }
		| primary_value '.' tIDENTIFIER tOP_ASGN command_call
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, $3, 0), $4, $5);
		    }
		| primary_value '.' tCONSTANT tOP_ASGN command_call
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, $3, 0), $4, $5);
		    }
		| primary_value tCOLON2 tCONSTANT tOP_ASGN command_call
		    {
		      yyerror(p, "constant re-assignment");
		      $$ = 0;
		    }
		| primary_value tCOLON2 tIDENTIFIER tOP_ASGN command_call
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, $3, 0), $4, $5);
		    }
		| backref tOP_ASGN command_call
		    {
		      backref_error(p, $1);
		      $$ = new_begin(p, 0);
		    }
		| lhs '=' mrhs
		    {
		      $$ = new_asgn(p, $1, new_array(p, $3));
		    }
		| mlhs '=' arg_value
		    {
		      $$ = new_masgn(p, $1, $3);
		    }
		| mlhs '=' mrhs
		    {
		      $$ = new_masgn(p, $1, new_array(p, $3));
		    }
		| expr
		;

command_asgn	: lhs '=' command_call
		    {
		      $$ = new_asgn(p, $1, $3);
		    }
		| lhs '=' command_asgn
		    {
		      $$ = new_asgn(p, $1, $3);
		    }
		;


expr		: command_call
		| expr keyword_and expr
		    {
		      $$ = new_and(p, $1, $3);
		    }
		| expr keyword_or expr
		    {
		      $$ = new_or(p, $1, $3);
		    }
		| keyword_not opt_nl expr
		    {
		      $$ = call_uni_op(p, cond($3), "!");
		    }
		| '!' command_call
		    {
		      $$ = call_uni_op(p, cond($2), "!");
		    }
		| arg
		;

expr_value	: expr
		    {
		      if (!$1) $$ = new_nil(p);
		      else $$ = $1;
		    }
		;

command_call	: command
		| block_command
		;

block_command	: block_call
		| block_call dot_or_colon operation2 command_args
		;

cmd_brace_block	: tLBRACE_ARG
		    {
		      local_nest(p);
		    }
		  opt_block_param
		  compstmt
		  '}'
		    {
		      $$ = new_block(p, $3, $4);
		      local_unnest(p);
		    }
		;

command		: operation command_args       %prec tLOWEST
		    {
		      $$ = new_fcall(p, $1, $2);
		    }
		| operation command_args cmd_brace_block
		    {
		      args_with_block(p, $2, $3);
		      $$ = new_fcall(p, $1, $2);
		    }
		| primary_value '.' operation2 command_args	%prec tLOWEST
		    {
		      $$ = new_call(p, $1, $3, $4);
		    }
		| primary_value '.' operation2 command_args cmd_brace_block
		    {
		      args_with_block(p, $4, $5);
		      $$ = new_call(p, $1, $3, $4);
		   }
		| primary_value tCOLON2 operation2 command_args	%prec tLOWEST
		    {
		      $$ = new_call(p, $1, $3, $4);
		    }
		| primary_value tCOLON2 operation2 command_args cmd_brace_block
		    {
		      args_with_block(p, $4, $5);
		      $$ = new_call(p, $1, $3, $4);
		    }
		| keyword_super command_args
		    {
		      $$ = new_super(p, $2);
		    }
		| keyword_yield command_args
		    {
		      $$ = new_yield(p, $2);
		    }
		| keyword_return call_args
		    {
		      $$ = new_return(p, ret_args(p, $2));
		    }
		| keyword_break call_args
		    {
		      $$ = new_break(p, ret_args(p, $2));
		    }
		| keyword_next call_args
		    {
		      $$ = new_next(p, ret_args(p, $2));
		    }
		;

mlhs		: mlhs_basic
		    {
		      $$ = $1;
		    }
		| tLPAREN mlhs_inner rparen
		    {
		      $$ = $2;
		    }
		;

mlhs_inner	: mlhs_basic
		| tLPAREN mlhs_inner rparen
		    {
		      $$ = list1($2);
		    }
		;

mlhs_basic	: mlhs_list
		    {
		      $$ = list1($1);
		    }
		| mlhs_list mlhs_item
		    {
		      $$ = list1(push($1,$2));
		    }
		| mlhs_list tSTAR mlhs_node
		    {
		      $$ = list2($1, $3);
		    }
		| mlhs_list tSTAR mlhs_node ',' mlhs_post
		    {
		      $$ = list3($1, $3, $5);
		    }
		| mlhs_list tSTAR
		    {
		      $$ = list2($1, new_nil(p));
		    }
		| mlhs_list tSTAR ',' mlhs_post
		    {
		      $$ = list3($1, new_nil(p), $4);
		    }
		| tSTAR mlhs_node
		    {
		      $$ = list2(0, $2);
		    }
		| tSTAR mlhs_node ',' mlhs_post
		    {
		      $$ = list3(0, $2, $4);
		    }
		| tSTAR
		    {
		      $$ = list2(0, new_nil(p));
		    }
		| tSTAR ',' mlhs_post
		    {
		      $$ = list3(0, new_nil(p), $3);
		    }
		;

mlhs_item	: mlhs_node
		| tLPAREN mlhs_inner rparen
		    {
		      $$ = $2;
		    }
		;

mlhs_list	: mlhs_item ','
		    {
		      $$ = list1($1);
		    }
		| mlhs_list mlhs_item ','
		    {
		      $$ = push($1, $2);
		    }
		;

mlhs_post	: mlhs_item
		    {
		      $$ = list1($1);
		    }
		| mlhs_list mlhs_item
		    {
		      $$ = push($1, $2);
		    }
		;

mlhs_node	: variable
		    {
		      assignable(p, $1);
		    }
		| primary_value '[' opt_call_args rbracket
		    {
		      $$ = new_call(p, $1, intern("[]"), $3);
		    }
		| primary_value '.' tIDENTIFIER
		    {
		      $$ = new_call(p, $1, $3, 0);
		    }
		| primary_value tCOLON2 tIDENTIFIER
		    {
		      $$ = new_call(p, $1, $3, 0);
		    }
		| primary_value '.' tCONSTANT
		    {
		      $$ = new_call(p, $1, $3, 0);
		    }
		| primary_value tCOLON2 tCONSTANT
		    {
		      if (p->in_def || p->in_single)
			yyerror(p, "dynamic constant assignment");
		      $$ = new_colon2(p, $1, $3);
		    }
		| tCOLON3 tCONSTANT
		    {
		      if (p->in_def || p->in_single)
			yyerror(p, "dynamic constant assignment");
		      $$ = new_colon3(p, $2);
		    }
		| backref
		    {
		      backref_error(p, $1);
		      $$ = 0;
		    }
		;

lhs		: variable
		    {
		      assignable(p, $1);
		    }
		| primary_value '[' opt_call_args rbracket
		    {
		      $$ = new_call(p, $1, intern("[]"), $3);
		    }
		| primary_value '.' tIDENTIFIER
		    {
		      $$ = new_call(p, $1, $3, 0);
		    }
		| primary_value tCOLON2 tIDENTIFIER
		    {
		      $$ = new_call(p, $1, $3, 0);
		    }
		| primary_value '.' tCONSTANT
		    {
		      $$ = new_call(p, $1, $3, 0);
		    }
		| primary_value tCOLON2 tCONSTANT
		    {
		      if (p->in_def || p->in_single)
			yyerror(p, "dynamic constant assignment");
		      $$ = new_colon2(p, $1, $3);
		    }
		| tCOLON3 tCONSTANT
		    {
		      if (p->in_def || p->in_single)
			yyerror(p, "dynamic constant assignment");
		      $$ = new_colon3(p, $2);
		    }
		| backref
		    {
		      backref_error(p, $1);
		      $$ = 0;
		    }
		;

cname		: tIDENTIFIER
		    {
		      yyerror(p, "class/module name must be CONSTANT");
		    }
		| tCONSTANT
		;

cpath		: tCOLON3 cname
		    {
		      $$ = cons((node*)1, (node*)$2);
		    }
		| cname
		    {
		      $$ = cons((node*)0, (node*)$1);
		    }
		| primary_value tCOLON2 cname
		    {
		      $$ = cons($1, (node*)$3);
		    }
		;

fname		: tIDENTIFIER
		| tCONSTANT
		| tFID
		| op
		    {
		      p->lstate = EXPR_ENDFN;
		      $$ = $1;
		    }
		| reswords
		    {
		      p->lstate = EXPR_ENDFN;
		      $$ = $<id>1;
		    }
		;

fsym		: fname
		| symbol
		;

fitem		: fsym
		    {
		      $$ = new_sym(p, $1);
		    }
		;

undef_list	: fsym
		    {
		      $$ = new_undef(p, $1);
		    }
		| undef_list ',' {p->lstate = EXPR_FNAME;} fitem
		    {
		      $$ = push($1, (node*)$4);
		    }
		;

op		: '|'		{ $$ = intern("|"); }
		| '^'		{ $$ = intern("^"); }
		| '&'		{ $$ = intern("&"); }
		| tCMP		{ $$ = intern("<=>"); }
		| tEQ		{ $$ = intern("=="); }
		| tEQQ		{ $$ = intern("==="); }
		| tMATCH	{ $$ = intern("=~"); }
		| tNMATCH	{ $$ = intern("!~"); }
		| '>'		{ $$ = intern(">"); }
		| tGEQ		{ $$ = intern(">="); }
		| '<'		{ $$ = intern("<"); }
		| tLEQ		{ $$ = intern(">="); }
		| tNEQ		{ $$ = intern("!="); }
		| tLSHFT	{ $$ = intern("<<"); }
		| tRSHFT	{ $$ = intern(">>"); }
		| '+'		{ $$ = intern("+"); }
		| '-'		{ $$ = intern("-"); }
		| '*'		{ $$ = intern("*"); }
		| tSTAR		{ $$ = intern("*"); }
		| '/'		{ $$ = intern("/"); }
		| '%'		{ $$ = intern("%"); }
		| tPOW		{ $$ = intern("**"); }
		| '!'		{ $$ = intern("!"); }
		| '~'		{ $$ = intern("~"); }
		| tUPLUS	{ $$ = intern("+@"); }
		| tUMINUS	{ $$ = intern("-@"); }
		| tAREF		{ $$ = intern("[]"); }
		| tASET		{ $$ = intern("[]="); }
		;

reswords	: keyword__LINE__ | keyword__FILE__ | keyword__ENCODING__
		| keyword_BEGIN | keyword_END
		| keyword_alias | keyword_and | keyword_begin
		| keyword_break | keyword_case | keyword_class | keyword_def
		| keyword_do | keyword_else | keyword_elsif
		| keyword_end | keyword_ensure | keyword_false
		| keyword_for | keyword_in | keyword_module | keyword_next
		| keyword_nil | keyword_not | keyword_or | keyword_redo
		| keyword_rescue | keyword_retry | keyword_return | keyword_self
		| keyword_super | keyword_then | keyword_true | keyword_undef
		| keyword_when | keyword_yield | keyword_if | keyword_unless
		| keyword_while | keyword_until
		;

arg		: lhs '=' arg
		    {
		      $$ = new_asgn(p, $1, $3);
		    }
		| lhs '=' arg modifier_rescue arg
		    {
		      $$ = new_asgn(p, $1, new_rescue(p, $3, list1(list3(0, 0, $5)), 0));
		    }
		| var_lhs tOP_ASGN arg
		    {
		      $$ = new_op_asgn(p, $1, $2, $3);
		    }
		| var_lhs tOP_ASGN arg modifier_rescue arg
		    {
		      $$ = new_op_asgn(p, $1, $2, new_rescue(p, $3, list1(list3(0, 0, $5)), 0));
		    }
		| primary_value '[' opt_call_args rbracket tOP_ASGN arg
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, intern("[]"), $3), $5, $6);
		    }
		| primary_value '.' tIDENTIFIER tOP_ASGN arg
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, $3, 0), $4, $5);
		    }
		| primary_value '.' tCONSTANT tOP_ASGN arg
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, $3, 0), $4, $5);
		    }
		| primary_value tCOLON2 tIDENTIFIER tOP_ASGN arg
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, $3, 0), $4, $5);
		    }
		| primary_value tCOLON2 tCONSTANT tOP_ASGN arg
		    {
		      yyerror(p, "constant re-assignment");
		      $$ = new_begin(p, 0);
		    }
		| tCOLON3 tCONSTANT tOP_ASGN arg
		    {
		      yyerror(p, "constant re-assignment");
		      $$ = new_begin(p, 0);
		    }
		| backref tOP_ASGN arg
		    {
		      backref_error(p, $1);
		      $$ = new_begin(p, 0);
		    }
		| arg tDOT2 arg
		    {
		      $$ = new_dot2(p, $1, $3);
		    }
		| arg tDOT3 arg
		    {
		      $$ = new_dot3(p, $1, $3);
		    }
		| arg '+' arg
		    {
		      $$ = call_bin_op(p, $1, "+", $3);
		    }
		| arg '-' arg
		    {
		      $$ = call_bin_op(p, $1, "-", $3);
		    }
		| arg '*' arg
		    {
		      $$ = call_bin_op(p, $1, "*", $3);
		    }
		| arg '/' arg
		    {
		      $$ = call_bin_op(p, $1, "/", $3);
		    }
		| arg '%' arg
		    {
		      $$ = call_bin_op(p, $1, "%", $3);
		    }
		| arg tPOW arg
		    {
		      $$ = call_bin_op(p, $1, "**", $3);
		    }
		| tUMINUS_NUM tINTEGER tPOW arg
		    {
		      $$ = call_uni_op(p, call_bin_op(p, $2, "**", $4), "-@");
		    }
		| tUMINUS_NUM tFLOAT tPOW arg
		    {
		      $$ = call_uni_op(p, call_bin_op(p, $2, "**", $4), "-@");
		    }
		| tUPLUS arg
		    {
		      $$ = call_uni_op(p, $2, "+@");
		    }
		| tUMINUS arg
		    {
		      $$ = call_uni_op(p, $2, "-@");
		    }
		| arg '|' arg
		    {
		      $$ = call_bin_op(p, $1, "|", $3);
		    }
		| arg '^' arg
		    {
		      $$ = call_bin_op(p, $1, "^", $3);
		    }
		| arg '&' arg
		    {
		      $$ = call_bin_op(p, $1, "&", $3);
		    }
		| arg tCMP arg
		    {
		      $$ = call_bin_op(p, $1, "<=>", $3);
		    }
		| arg '>' arg
		    {
		      $$ = call_bin_op(p, $1, ">", $3);
		    }
		| arg tGEQ arg
		    {
		      $$ = call_bin_op(p, $1, ">=", $3);
		    }
		| arg '<' arg
		    {
		      $$ = call_bin_op(p, $1, "<", $3);
		    }
		| arg tLEQ arg
		    {
		      $$ = call_bin_op(p, $1, "<=", $3);
		    }
		| arg tEQ arg
		    {
		      $$ = call_bin_op(p, $1, "==", $3);
		    }
		| arg tEQQ arg
		    {
		      $$ = call_bin_op(p, $1, "===", $3);
		    }
		| arg tNEQ arg
		    {
		      $$ = call_bin_op(p, $1, "!=", $3);
		    }
		| arg tMATCH arg
		    {
		      $$ = match_op(p, $1, $3);
#if 0
		      if (nd_type($1) == NODE_LIT && TYPE($1->nd_lit) == T_REGEXP) {
			$$ = reg_named_capture_assign($1->nd_lit, $$);
		      }
#endif
		    }
		| arg tNMATCH arg
		    {
		      $$ = call_bin_op(p, $1, "!~", $3);
		    }
		| '!' arg
		    {
		      $$ = call_uni_op(p, cond($2), "!");
		    }
		| '~' arg
		    {
		      $$ = call_uni_op(p, cond($2), "~");
		    }
		| arg tLSHFT arg
		    {
		      $$ = call_bin_op(p, $1, "<<", $3);
		    }
		| arg tRSHFT arg
		    {
		      $$ = call_bin_op(p, $1, ">>", $3);
		    }
		| arg tANDOP arg
		    {
		      $$ = new_and(p, $1, $3);
		    }
		| arg tOROP arg
		    {
		      $$ = new_or(p, $1, $3);
		    }
		| arg '?' arg opt_nl ':' arg
		    {
		      $$ = new_if(p, cond($1), $3, $6);
		    }
		| primary
		    {
		      $$ = $1;
		    }
		;

arg_value	: arg
		    {
		      $$ = $1;
		      if (!$$) $$ = new_nil(p);
		    }
		;

aref_args	: none
		| args trailer
		    {
		      $$ = $1;
		    }
		| args ',' assocs trailer
		    {
		      $$ = push($1, new_hash(p, $3));
		    }
		| assocs trailer
		    {
		      $$ = new_hash(p, $1);
		    }
		;

paren_args	: '(' opt_call_args rparen
		    {
		      $$ = $2;
		    }
		;

opt_paren_args	: none
		| paren_args
		;

opt_call_args	: none
		| call_args
		| args ','
		    {
		      $$ = cons($1,0);
		    }
		| args ',' assocs ','
		    {
		      $$ = cons(push($1, new_hash(p, $3)), 0);
		    }
		| assocs ','
		    {
		      $$ = cons(list1(new_hash(p, $1)), 0);
		    }
		;

call_args	: command
		    {
		      $$ = cons(list1($1), 0);
		    }
		| args opt_block_arg
		    {
		      $$ = cons($1, $2);
		    }
		| assocs opt_block_arg
		    {
		      $$ = cons(list1(new_hash(p, $1)), $2);
		    }
		| args ',' assocs opt_block_arg
		    {
		      $$ = cons(push($1, new_hash(p, $3)), $4);
		    }
		| block_arg
		    {
		      $$ = cons(0, $1);
		    }
		;

command_args	:  {
		      $<stack>$ = p->cmdarg_stack;
		      CMDARG_PUSH(1);
		    }
		  call_args
		    {
		      p->cmdarg_stack = $<stack>1;
		      $$ = $2;
		    }
		;

block_arg	: tAMPER arg_value
		    {
		      $$ = new_block_arg(p, $2);
		    }
		;

opt_block_arg	: ',' block_arg
		    {
		      $$ = $2;
		    }
		| none
		    {
		      $$ = 0;
		    }
		;

args		: arg_value
		    {
		      $$ = cons($1, 0);
		    }
		| tSTAR arg_value
		    {
		      $$ = cons(new_splat(p, $2), 0);
		    }
		| args ',' arg_value
		    {
		      $$ = push($1, $3);
		    }
		| args ',' tSTAR arg_value
		    {
		      $$ = push($1, new_splat(p, $4));
		    }
		;

mrhs		: args ',' arg_value
		    {
		      $$ = push($1, $3);
		    }
		| args ',' tSTAR arg_value
		    {
		      $$ = push($1, new_splat(p, $4));
		    }
		| tSTAR arg_value
		    {
		      $$ = list1(new_splat(p, $2));
		    }
		;

primary		: literal
		| string
		| regexp
		| var_ref
		| backref
		| tFID
		    {
		      $$ = new_fcall(p, $1, 0);
		    }
		| keyword_begin
		    {
		      $<stack>1 = p->cmdarg_stack;
		      p->cmdarg_stack = 0;
		    }
		  bodystmt
		  keyword_end
		    {
		      p->cmdarg_stack = $<stack>1;
		      $$ = $3;
		    }
		| tLPAREN_ARG expr {p->lstate = EXPR_ENDARG;} rparen
		    {
		      yywarning(p, "(...) interpreted as grouped expression");
		      $$ = $2;
		    }
		| tLPAREN compstmt ')'
		    {
		      $$ = $2;
		    }
		| primary_value tCOLON2 tCONSTANT
		    {
		      $$ = new_colon2(p, $1, $3);
		    }
		| tCOLON3 tCONSTANT
		    {
		      $$ = new_colon3(p, $2);
		    }
		| tLBRACK aref_args ']'
		    {
		      $$ = new_array(p, $2);
		    }
		| tLBRACE assoc_list '}'
		    {
		      $$ = new_hash(p, $2);
		    }
		| keyword_return
		    {
		      $$ = new_return(p, 0);
		    }
		| keyword_yield '(' call_args rparen
		    {
		      $$ = new_yield(p, $3);
		    }
		| keyword_yield '(' rparen
		    {
		      $$ = new_yield(p, 0);
		    }
		| keyword_yield
		    {
		      $$ = new_yield(p, 0);
		    }
		| keyword_not '(' expr rparen
		    {
		      $$ = call_uni_op(p, cond($3), "!");
		    }
		| keyword_not '(' rparen
		    {
		      $$ = call_uni_op(p, new_nil(p), "!");
		    }
		| operation brace_block
		    {
		      $$ = new_fcall(p, $1, cons(0, $2));
		    }
		| method_call
		| method_call brace_block
		    {
		      call_with_block(p, $1, $2);
		      $$ = $1;
		    }
		| tLAMBDA
		    {
		      local_nest(p);
		      $<num>$ = p->lpar_beg;
		      p->lpar_beg = ++p->paren_nest;
		    }
		  f_larglist
		  lambda_body
		    {
		      p->lpar_beg = $<num>2;
		      $$ = new_lambda(p, $3, $4);
		      local_unnest(p);
		    }
		| keyword_if expr_value then
		  compstmt
		  if_tail
		  keyword_end
		    {
		      $$ = new_if(p, cond($2), $4, $5);
		    }
		| keyword_unless expr_value then
		  compstmt
		  opt_else
		  keyword_end
		    {
		      $$ = new_unless(p, cond($2), $4, $5);
		    }
		| keyword_while {COND_PUSH(1);} expr_value do {COND_POP();}
		  compstmt
		  keyword_end
		    {
		      $$ = new_while(p, cond($3), $6);
		    }
		| keyword_until {COND_PUSH(1);} expr_value do {COND_POP();}
		  compstmt
		  keyword_end
		    {
		      $$ = new_until(p, cond($3), $6);
		    }
		| keyword_case expr_value opt_terms
		  case_body
		  keyword_end
		    {
		      $$ = new_case(p, $2, $4);
		    }
		| keyword_case opt_terms case_body keyword_end
		    {
		      $$ = new_case(p, 0, $3);
		    }
		| keyword_for for_var keyword_in
		  {COND_PUSH(1);}
		  expr_value do
		  {COND_POP();}
		  compstmt
		  keyword_end
		    {
		      $$ = new_for(p, $2, $5, $8);
		    }
		| keyword_class cpath superclass
		    {
		      if (p->in_def || p->in_single)
			yyerror(p, "class definition in method body");
		      $<node>$ = local_switch(p);
		    }
		  bodystmt
		  keyword_end
		    {
		      $$ = new_class(p, $2, $3, $5);
		      local_resume(p, $<node>4);
		    }
		| keyword_class tLSHFT expr
		    {
		      $<num>$ = p->in_def;
		      p->in_def = 0;
		    }
		  term
		    {
		      $<node>$ = cons(local_switch(p), (node*)(intptr_t)p->in_single);
		      p->in_single = 0;
		    }
		  bodystmt
		  keyword_end
		    {
		      $$ = new_sclass(p, $3, $7);
		      local_resume(p, $<node>6->car);
		      p->in_def = $<num>4;
		      p->in_single = (int)(intptr_t)$<node>6->cdr;
		    }
		| keyword_module cpath
		    {
		      if (p->in_def || p->in_single)
			yyerror(p, "module definition in method body");
		      $<node>$ = local_switch(p);
		    }
		  bodystmt
		  keyword_end
		    {
		      $$ = new_module(p, $2, $4);
		      local_resume(p, $<node>3);
		    }
		| keyword_def fname
		    {
		      p->in_def++;
		      $<node>$ = local_switch(p);
		    }
		  f_arglist
		  bodystmt
		  keyword_end
		    {
		      $$ = new_def(p, $2, $4, $5);
		      local_resume(p, $<node>3);
		      p->in_def--;
		    }
		| keyword_def singleton dot_or_colon {p->lstate = EXPR_FNAME;} fname
		    {
		      p->in_single++;
		      p->lstate = EXPR_ENDFN; /* force for args */
		      $<node>$ = local_switch(p);
		    }
		  f_arglist
		  bodystmt
		  keyword_end
		    {
		      $$ = new_sdef(p, $2, $5, $7, $8);
		      local_resume(p, $<node>6);
		      p->in_single--;
		    }
		| keyword_break
		    {
		      $$ = new_break(p, 0);
		    }
		| keyword_next
		    {
		      $$ = new_next(p, 0);
		    }
		| keyword_redo
		    {
		      $$ = new_redo(p);
		    }
		| keyword_retry
		    {
		      $$ = new_retry(p);
		    }
		;

primary_value	: primary
		    {
		      $$ = $1;
		      if (!$$) $$ = new_nil(p);
		    }
		;

then		: term
		| keyword_then
		| term keyword_then
		;

do		: term
		| keyword_do_cond
		;

if_tail		: opt_else
		| keyword_elsif expr_value then
		  compstmt
		  if_tail
		    {
		      $$ = new_if(p, cond($2), $4, $5);
		    }
		;

opt_else	: none
		| keyword_else compstmt
		    {
		      $$ = $2;
		    }
		;

for_var		: lhs
		    {
		      $$ = list1(list1($1));
		    }
		| mlhs
		;

f_marg		: f_norm_arg
		    {
		      $$ = new_arg(p, $1);
		    }
		| tLPAREN f_margs rparen
		    {
		      $$ = new_masgn(p, $2, 0);
		    }
		;

f_marg_list	: f_marg
		    {
		      $$ = list1($1);
		    }
		| f_marg_list ',' f_marg
		    {
		      $$ = push($1, $3);
		    }
		;

f_margs		: f_marg_list
		    {
		      $$ = list3($1,0,0);
		    }
		| f_marg_list ',' tSTAR f_norm_arg
		    {
		      $$ = list3($1, new_arg(p, $4), 0);
		    }
		| f_marg_list ',' tSTAR f_norm_arg ',' f_marg_list
		    {
		      $$ = list3($1, new_arg(p, $4), $6);
		    }
		| f_marg_list ',' tSTAR
		    {
		      $$ = list3($1, (node*)-1, 0);
		    }
		| f_marg_list ',' tSTAR ',' f_marg_list
		    {
		      $$ = list3($1, (node*)-1, $5);
		    }
		| tSTAR f_norm_arg
		    {
		      $$ = list3(0, new_arg(p, $2), 0);
		    }
		| tSTAR f_norm_arg ',' f_marg_list
		    {
		      $$ = list3(0, new_arg(p, $2), $4);
		    }
		| tSTAR
		    {
		      $$ = list3(0, (node*)-1, 0);
		    }
		| tSTAR ',' f_marg_list
		    {
		      $$ = list3(0, (node*)-1, $3);
		    }
		;

block_param	: f_arg ',' f_block_optarg ',' f_rest_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, $3, $5, 0, $6);
		    }
		| f_arg ',' f_block_optarg ',' f_rest_arg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, $3, $5, $7, $8);
		    }
		| f_arg ',' f_block_optarg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, $3, 0, 0, $4);
		    }
		| f_arg ',' f_block_optarg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, $3, 0, $5, $6);
		    }
                | f_arg ',' f_rest_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, 0, $3, 0, $4);
		    }
		| f_arg ','
		    {
		      $$ = new_args(p, $1, 0, 1, 0, 0);
		    }
		| f_arg ',' f_rest_arg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, 0, $3, $5, $6);
		    }
		| f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, 0, 0, 0, $2);
		    }
		| f_block_optarg ',' f_rest_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, $1, $3, 0, $4);
		    }
		| f_block_optarg ',' f_rest_arg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, $1, $3, $5, $6);
		    }
		| f_block_optarg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, $1, 0, 0, $2);
		    }
		| f_block_optarg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, $1, 0, $3, $4);
		    }
		| f_rest_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, 0, $1, 0, $2);
		    }
		| f_rest_arg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, 0, $1, $3, $4);
		    }
		| f_block_arg
		    {
		      $$ = new_args(p, 0, 0, 0, 0, $1);
		    }
		;

opt_block_param	: none
		| block_param_def
		    {
		      p->cmd_start = TRUE;
		      $$ = $1;
		    }
		;

block_param_def	: '|' opt_bv_decl '|'
		    {
		      local_add_f(p, 0);
		      $$ = 0;
		    }
		| tOROP
		    {
		      local_add_f(p, 0);
		      $$ = 0;
		    }
		| '|' block_param opt_bv_decl '|'
		    {
		      $$ = $2;
		    }
		;


opt_bv_decl	: opt_nl
		    {
		      $$ = 0;
		    }
		| opt_nl ';' bv_decls opt_nl
		    {
		      $$ = 0;
		    }
		;

bv_decls	: bvar
		| bv_decls ',' bvar
		;

bvar		: tIDENTIFIER
		    {
		      local_add_f(p, $1);
		      new_bv(p, $1);
		    }
		| f_bad_arg
		;

f_larglist	: '(' f_args opt_bv_decl ')'
		    {
		      $$ = $2;
		    }
		| f_args
		    {
		      $$ = $1;
		    }
		;

lambda_body	: tLAMBEG compstmt '}'
		    {
		      $$ = $2;
		    }
		| keyword_do_LAMBDA compstmt keyword_end
		    {
		      $$ = $2;
		    }
		;

do_block	: keyword_do_block
		    {
		      local_nest(p);
		    }
		  opt_block_param
		  compstmt
		  keyword_end
		    {
		      $$ = new_block(p,$3,$4);
		      local_unnest(p);
		    }
		;

block_call	: command do_block
		    {
		      if ($1->car == (node*)NODE_YIELD) {
			yyerror(p, "block given to yield");
		      }
		      else {
		        call_with_block(p, $1, $2);
		      }
		      $$ = $1;
		    }
		| block_call dot_or_colon operation2 opt_paren_args
		    {
		      $$ = new_call(p, $1, $3, $4);
		    }
		| block_call dot_or_colon operation2 opt_paren_args brace_block
		    {
		      $$ = new_call(p, $1, $3, $4);
		      call_with_block(p, $$, $5);
		    }
		| block_call dot_or_colon operation2 command_args do_block
		    {
		      $$ = new_call(p, $1, $3, $4);
		      call_with_block(p, $$, $5);
		    }
		;

method_call	: operation paren_args
		    {
		      $$ = new_fcall(p, $1, $2);
		    }
		| primary_value '.' operation2 opt_paren_args
		    {
		      $$ = new_call(p, $1, $3, $4);
		    }
		| primary_value tCOLON2 operation2 paren_args
		    {
		      $$ = new_call(p, $1, $3, $4);
		    }
		| primary_value tCOLON2 operation3
		    {
		      $$ = new_call(p, $1, $3, 0);
		    }
		| primary_value '.' paren_args
		    {
		      $$ = new_call(p, $1, intern("call"), $3);
		    }
		| primary_value tCOLON2 paren_args
		    {
		      $$ = new_call(p, $1, intern("call"), $3);
		    }
		| keyword_super paren_args
		    {
		      $$ = new_super(p, $2);
		    }
		| keyword_super
		    {
		      $$ = new_zsuper(p);
		    }
		| primary_value '[' opt_call_args rbracket
		    {
		      $$ = new_call(p, $1, intern("[]"), $3);
		    }
		;

brace_block	: '{'
		    {
		      local_nest(p);
		    }
		  opt_block_param
		  compstmt '}'
		    {
		      $$ = new_block(p,$3,$4);
		      local_unnest(p);
		    }
		| keyword_do
		    {
		      local_nest(p);
		    }
		  opt_block_param
		  compstmt keyword_end
		    {
		      $$ = new_block(p,$3,$4);
		      local_unnest(p);
		    }
		;

case_body	: keyword_when args then
		  compstmt
		  cases
		    {
		      $$ = cons(cons($2, $4), $5);
		    }
		;

cases		: opt_else
		    {
		      if ($1) {
			$$ = cons(cons(0, $1), 0);
		      }
		      else {
			$$ = 0;
		      }
		    }
		| case_body
		;

opt_rescue	: keyword_rescue exc_list exc_var then
		  compstmt
		  opt_rescue
		    {
		      $$ = list1(list3($2, $3, $5));
		      if ($6) $$ = append($$, $6);
		    }
		| none
		;

exc_list	: arg_value
		    {
			$$ = list1($1);
		    }
		| mrhs
		| none
		;

exc_var		: tASSOC lhs
		    {
		      $$ = $2;
		    }
		| none
		;

opt_ensure	: keyword_ensure compstmt
		    {
		      $$ = $2;
		    }
		| none
		;

literal		: numeric
		| symbol
		    {
		      $$ = new_sym(p, $1);
		    }
		;

string		: tCHAR
		| tSTRING
		| tSTRING_BEG tSTRING
		    {
		      $$ = $2;
		    }
		| tSTRING_BEG string_interp tSTRING
		    {
		      $$ = new_dstr(p, push($2, $3));
		    }
		;

string_interp	: tSTRING_PART
		    {
		      $<num>$ = p->sterm;
		      p->sterm = 0;
		    }
		  compstmt
		  '}'
		    {
		      p->sterm = $<num>2;
		      $$ = list2($1, $3);
		    }
		| string_interp
		  tSTRING_PART
		    {
		      $<num>$ = p->sterm;
		      p->sterm = 0;
		    }
		  compstmt
		  '}'
		    {
		      p->sterm = $<num>3;
		      $$ = push(push($1, $2), $4);
		    }
		;

regexp		: tREGEXP
		;

symbol		: tSYMBEG sym
		    {
		      p->lstate = EXPR_END;
		      $$ = $2;
		    }
		;

sym		: fname
		| tIVAR
		| tGVAR
		| tCVAR
		;

numeric 	: tINTEGER
		| tFLOAT
		| tUMINUS_NUM tINTEGER	       %prec tLOWEST
		    {
		      $$ = negate_lit(p, $2);
		    }
		| tUMINUS_NUM tFLOAT	       %prec tLOWEST
		    {
		      $$ = negate_lit(p, $2);
		    }
		;

variable	: tIDENTIFIER
		    {
		      $$ = new_lvar(p, $1);
		    }
		| tIVAR
		    {
		      $$ = new_ivar(p, $1);
		    }
		| tGVAR
		    {
		      $$ = new_gvar(p, $1);
		    }
		| tCVAR
		    {
		      $$ = new_cvar(p, $1);
		    }
		| tCONSTANT
		    {
		      $$ = new_const(p, $1);
		    }
		;

var_lhs		: variable
		    {
		      assignable(p, $1);
		    }
		;

var_ref		: variable
		    {
		      $$ = var_reference(p, $1);
		    }
		| keyword_nil 
		    {
		      $$ = new_nil(p);
		    }
		| keyword_self
		    {
		      $$ = new_self(p);
   		    }
		| keyword_true
		    {
		      $$ = new_true(p);
   		    }
		| keyword_false
		    {
		      $$ = new_false(p);
   		    }
		| keyword__FILE__
		    {
		      if (!p->filename) {
			p->filename = "(null)";
		      }
		      $$ = new_str(p, p->filename, strlen(p->filename));
		    }
		| keyword__LINE__
		    {
		      char buf[16];

		      snprintf(buf, 16, "%d", p->lineno);
		      $$ = new_int(p, buf, 10);
		    }
		;

backref		: tNTH_REF
		| tBACK_REF
		;

superclass	: term
		    {
		      $$ = 0;
		    }
		| '<'
		    {
		      p->lstate = EXPR_BEG;
		      p->cmd_start = TRUE;
		    }
		  expr_value term
		    {
		      $$ = $3;
		    }
		| error term
		    {
		      yyerrok;
		      $$ = 0;
		    }
		;

f_arglist	: '(' f_args rparen
		    {
		      $$ = $2;
		      p->lstate = EXPR_BEG;
		      p->cmd_start = TRUE;
		    }
		| f_args term
		    {
		      $$ = $1;
		    }
		;

f_args		: f_arg ',' f_optarg ',' f_rest_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, $3, $5, 0, $6);
		    }
		| f_arg ',' f_optarg ',' f_rest_arg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, $3, $5, $7, $8);
		    }
		| f_arg ',' f_optarg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, $3, 0, 0, $4);
		    }
		| f_arg ',' f_optarg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, $3, 0, $5, $6);
		    }
		| f_arg ',' f_rest_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, 0, $3, 0, $4);
		    }
		| f_arg ',' f_rest_arg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, 0, $3, $5, $6);
		    }
		| f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, $1, 0, 0, 0, $2);
		    }
		| f_optarg ',' f_rest_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, $1, $3, 0, $4);
		    }
		| f_optarg ',' f_rest_arg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, $1, $3, $5, $6);
		    }
		| f_optarg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, $1, 0, 0, $2);
		    }
		| f_optarg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, $1, 0, $3, $4);
		    }
		| f_rest_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, 0, $1, 0, $2);
		    }
		| f_rest_arg ',' f_arg opt_f_block_arg
		    {
		      $$ = new_args(p, 0, 0, $1, $3, $4);
		    }
		| f_block_arg
		    {
		      $$ = new_args(p, 0, 0, 0, 0, $1);
		    }
		| /* none */
		    {
		      local_add_f(p, 0);
		      $$ = new_args(p, 0, 0, 0, 0, 0);
		    }
		;

f_bad_arg	: tCONSTANT
		    {
		      yyerror(p, "formal argument cannot be a constant");
		      $$ = 0;
		    }
		| tIVAR
		    {
		      yyerror(p, "formal argument cannot be an instance variable");
		      $$ = 0;
		    }
		| tGVAR
		    {
		      yyerror(p, "formal argument cannot be a global variable");
		      $$ = 0;
		    }
		| tCVAR
		    {
		      yyerror(p, "formal argument cannot be a class variable");
		      $$ = 0;
		    }
		;

f_norm_arg	: f_bad_arg
		    {
		      $$ = 0;
		    }
		| tIDENTIFIER
		    {
		      local_add_f(p, $1);
		      $$ = $1;
		    }
		;

f_arg_item	: f_norm_arg
		    {
		      $$ = new_arg(p, $1);
		    }
		| tLPAREN f_margs rparen
		    {
		      $$ = new_masgn(p, $2, 0);
		    }
		;

f_arg		: f_arg_item
		    {
		      $$ = list1($1);
		    }
		| f_arg ',' f_arg_item
		    {
		      $$ = push($1, $3);
		    }
		;

f_opt		: tIDENTIFIER '=' arg_value
		    {
		      local_add_f(p, $1);
		      $$ = cons((node*)$1, $3);
		    }
		;

f_block_opt	: tIDENTIFIER '=' primary_value
		    {
		      local_add_f(p, $1);
		      $$ = cons((node*)$1, $3);
		    }
		;

f_block_optarg	: f_block_opt
		    {
		      $$ = list1($1);
		    }
		| f_block_optarg ',' f_block_opt
		    {
		      $$ = push($1, $3);
		    }
		;

f_optarg	: f_opt
		    {
		      $$ = list1($1);
		    }
		| f_optarg ',' f_opt
		    {
		      $$ = push($1, $3);
		    }
		;

restarg_mark	: '*'
		| tSTAR
		;

f_rest_arg	: restarg_mark tIDENTIFIER
		    {
		      local_add_f(p, $2);
		      $$ = $2;
		    }
		| restarg_mark
		    {
		      $$ = 0;
		    }
		;

blkarg_mark	: '&'
		| tAMPER
		;

f_block_arg	: blkarg_mark tIDENTIFIER
		    {
		      local_add_f(p, $2);
		      $$ = $2;
		    }
		;

opt_f_block_arg	: ',' f_block_arg
		    {
		      $$ = $2;
		    }
		| none
		    {
		      local_add_f(p, 0);
		      $$ = 0;
		    }
		;

singleton	: var_ref
		    {
		      $$ = $1;
		      if (!$$) $$ = new_nil(p);
		    }
		| '(' {p->lstate = EXPR_BEG;} expr rparen
		    {
		      if ($3 == 0) {
			yyerror(p, "can't define singleton method for ().");
		      }
		      else {
			switch ((enum node_type)$3->car) {
			case NODE_STR:
			case NODE_DSTR:
			case NODE_DREGX:
			case NODE_MATCH:
			case NODE_FLOAT:
			case NODE_ARRAY:
			  yyerror(p, "can't define singleton method for literals");
			default:
			  break;
			}
		      }
		      $$ = $3;
		    }
		;

assoc_list	: none
		| assocs trailer
		    {
		      $$ = $1;
		    }
		;

assocs		: assoc
		    {
		      $$ = list1($1);
		    }
		| assocs ',' assoc
		    {
		      $$ = push($1, $3);
		    }
		;

assoc		: arg_value tASSOC arg_value
		    {
		      $$ = cons($1, $3);
		    }
		| tLABEL arg_value
		    {
		      $$ = cons(new_sym(p, $1), $2);
		    }
		;

operation	: tIDENTIFIER
		| tCONSTANT
		| tFID
		;

operation2	: tIDENTIFIER
		| tCONSTANT
		| tFID
		| op
		;

operation3	: tIDENTIFIER
		| tFID
		| op
		;

dot_or_colon	: '.'
		| tCOLON2
		;

opt_terms	: /* none */
		| terms
		;

opt_nl		: /* none */
		| '\n'
		;

rparen		: opt_nl ')'
		;

rbracket	: opt_nl ']'
		;

trailer		: /* none */
		| '\n'
		| ','
		;

term		: ';' {yyerrok;}
		| '\n'
		;

terms		: term
		| terms ';' {yyerrok;}
		;

none		: /* none */
		    {
		      $$ = 0;
		    }
		;
%%
#define yylval  (*((YYSTYPE*)(p->ylval)))

static void
yyerror(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
    fputs(s, stderr);
    fputs("\n", stderr);
  }
  else if (p->nerr < sizeof(p->error_buffer) / sizeof(p->error_buffer[0])) {
    n = strlen(s);
    c = parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->error_buffer[p->nerr].message = c;
    p->error_buffer[p->nerr].lineno = p->lineno;
    p->error_buffer[p->nerr].column = p->column;
  }
  p->nerr++;
}

static void
yyerror_i(parser_state *p, const char *fmt, int i)
{
  char buf[256];

  snprintf(buf, 256, fmt, i);
  yyerror(p, buf);
}

static void
yywarn(parser_state *p, const char *s)
{
  char* c;
  size_t n;

  if (! p->capture_errors) {
    fputs(s, stderr);
    fputs("\n", stderr);
  }
  else if (p->nerr < sizeof(p->warn_buffer) / sizeof(p->warn_buffer[0])) {
    n = strlen(s);
    c = parser_palloc(p, n + 1);
    memcpy(c, s, n + 1);
    p->error_buffer[p->nwarn].message = c;
    p->error_buffer[p->nwarn].lineno = p->lineno;
    p->error_buffer[p->nwarn].column = p->column;
  }
  p->nwarn++;
}

static void
yywarning(parser_state *p, const char *s)
{
  fputs(s, stderr);
  fputs("\n", stderr);
}

static void
yywarning_s(parser_state *p, const char *fmt, const char *s)
{
  char buf[256];

  snprintf(buf, 256, fmt, s);
  yywarning(p, buf);
}

static void
backref_error(parser_state *p, node *n)
{
  switch ((int)(intptr_t)n->car) {
  case NODE_NTH_REF:
    yyerror_i(p, "can't set variable $%d", (int)(intptr_t)n->cdr);
    break;
  case NODE_BACK_REF:
    yyerror_i(p, "can't set variable $%c", (int)(intptr_t)n->cdr);
    break;
  }
}

static int peeks(parser_state *p, const char *s);
static int skips(parser_state *p, const char *s);

static inline int
nextc(parser_state *p)
{
  int c;

  if (p->pb) {
    node *tmp;

    c = (int)(intptr_t)p->pb->car;
    tmp = p->pb;
    p->pb = p->pb->cdr;
    cons_free(tmp);
  }
  else if (p->f) {
    if (feof(p->f)) return -1;
    c = fgetc(p->f);
    if (c == EOF) return -1;
  }
  else if (!p->s || p->s >= p->send) {
    return -1;
  }
  else {
    c = *p->s++;
  }
  if (c == '\n') {
    if (p->column < 0) {
      p->column++; // pushback caused an underflow
    }
    else {
      p->lineno++;
      p->column = 0;
    }
    // must understand heredoc
  }
  else {
    p->column++;
  }
  return c;
}

static void
pushback(parser_state *p, int c)
{
  if (c < 0) return;
  p->column--;
  p->pb = cons((node*)(intptr_t)c, p->pb);
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
peek_n(parser_state *p, int c, int n)
{
  node *list = 0;
  int c0;

  do {
    c0 = nextc(p);
    if (c0 < 0) return FALSE;
    list = push(list, (node*)(intptr_t)c0);
  } while(n--);
  if (p->pb) {
    p->pb = push(p->pb, (node*)list);
  }
  else {
    p->pb = list;
  }
  if (c0 == c) return TRUE;
  return FALSE;
}
#define peek(p,c) peek_n((p), (c), 0)

static int
peeks(parser_state *p, const char *s)
{
  int len = strlen(s);

  if (p->f) {
    int n = 0;
    while (*s) {
      if (!peek_n(p, *s++, n++)) return FALSE;
    }
    return TRUE;
  }
  else if (p->s && p->s + len >= p->send) {
    if (memcmp(p->s, s, len) == 0) return TRUE;
  }
  return FALSE;
}

static int
skips(parser_state *p, const char *s)
{
  int c;

  for (;;) {
    // skip until first char
    for (;;) {
      c = nextc(p);
      if (c < 0) return c;
      if (c == *s) break;
    }
    s++;
    if (peeks(p, s)) {
      int len = strlen(s);

      while (len--) {
	nextc(p);
      }
      return TRUE;
    }
  }
  return FALSE;
}

#define STR_FUNC_ESCAPE 0x01
#define STR_FUNC_EXPAND 0x02
#define STR_FUNC_REGEXP 0x04
#define STR_FUNC_QWORDS 0x08
#define STR_FUNC_SYMBOL 0x10
#define STR_FUNC_INDENT 0x20

enum string_type {
    str_squote = (0),
    str_dquote = (STR_FUNC_EXPAND),
    str_xquote = (STR_FUNC_EXPAND),
    str_regexp = (STR_FUNC_REGEXP|STR_FUNC_ESCAPE|STR_FUNC_EXPAND),
    str_sword  = (STR_FUNC_QWORDS),
    str_dword  = (STR_FUNC_QWORDS|STR_FUNC_EXPAND),
    str_ssym   = (STR_FUNC_SYMBOL),
    str_dsym   = (STR_FUNC_SYMBOL|STR_FUNC_EXPAND)
};

static int
newtok(parser_state *p)
{
  p->bidx = 0;
  return p->column - 1;
}

static void
tokadd(parser_state *p, int c)
{
  if (p->bidx < 1024) {
    p->buf[p->bidx++] = c;
  }
}

static int
toklast(parser_state *p)
{
  return p->buf[p->bidx-1];
}

static void
tokfix(parser_state *p)
{
  if (p->bidx >= 1024) {
    yyerror(p, "string too long (truncated)");
  }
  p->buf[p->bidx] = '\0';
}

static const char*
tok(parser_state *p)
{
  return p->buf;
}

static int
toklen(parser_state *p)
{
  return p->bidx;
}

#define IS_ARG() (p->lstate == EXPR_ARG || p->lstate == EXPR_CMDARG)
#define IS_END() (p->lstate == EXPR_END || p->lstate == EXPR_ENDARG || p->lstate == EXPR_ENDFN)
#define IS_BEG() (p->lstate == EXPR_BEG || p->lstate == EXPR_MID || p->lstate == EXPR_VALUE || p->lstate == EXPR_CLASS)
#define IS_SPCARG(c) (IS_ARG() && space_seen && !ISSPACE(c))
#define IS_LABEL_POSSIBLE() ((p->lstate == EXPR_BEG && !cmd_state) || IS_ARG())
#define IS_LABEL_SUFFIX(n) (peek_n(p, ':',(n)) && !peek_n(p, ':', (n)+1))

static unsigned long
scan_oct(const char *start, int len, int *retlen)
{
  const char *s = start;
  unsigned long retval = 0;

  while (len-- && *s >= '0' && *s <= '7') {
    retval <<= 3;
    retval |= *s++ - '0';
  }
  *retlen = s - start;
  return retval;
}

static unsigned long
scan_hex(const char *start, int len, int *retlen)
{
  static const char hexdigit[] = "0123456789abcdef0123456789ABCDEF";
  register const char *s = start;
  register unsigned long retval = 0;
  char *tmp;

  while (len-- && *s && (tmp = strchr(hexdigit, *s))) {
    retval <<= 4;
    retval |= (tmp - hexdigit) & 15;
    s++;
  }
  *retlen = s - start;
  return retval;
}

static int
read_escape(parser_state *p)
{
  int c;

  switch (c = nextc(p)) {
  case '\\':	/* Backslash */
    return c;

  case 'n':	/* newline */
    return '\n';

  case 't':	/* horizontal tab */
    return '\t';

  case 'r':	/* carriage-return */
    return '\r';

  case 'f':	/* form-feed */
    return '\f';

  case 'v':	/* vertical tab */
    return '\13';

  case 'a':	/* alarm(bell) */
    return '\007';

  case 'e':	/* escape */
    return 033;

  case '0': case '1': case '2': case '3': /* octal constant */
  case '4': case '5': case '6': case '7':
    {
       char buf[3];
       int i;

       for (i=0; i<3; i++) {
	 buf[i] = nextc(p);
	 if (buf[i] == -1) goto eof;
	 if (buf[i] < '0' || '7' < buf[i]) {
	   pushback(p, buf[i]);
	   break;
	 }
       }
       c = scan_oct(buf, i+1, &i);
    }
    return c;

  case 'x':	/* hex constant */
    {
      char buf[2];
      int i;

      for (i=0; i<2; i++) {
	buf[i] = nextc(p);
	if (buf[i] == -1) goto eof;
	if (!isxdigit(buf[i])) {
	  pushback(p, buf[i]);
	  break;
	}
      }
      c = scan_hex(buf, i+1, &i);
      if (i == 0) {
	yyerror(p, "Invalid escape character syntax");
	return 0;
      }
    }
    return c;

  case 'b':	/* backspace */
    return '\010';

  case 's':	/* space */
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
    else if (c == -1) goto eof;
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
    else if (c == -1) goto eof;
    return c & 0x9f;

  eof:
  case -1:
    yyerror(p, "Invalid escape character syntax");
    return '\0';

  default:
    return c;
  }
}

static int
parse_string(parser_state *p, int term)
{
  int c;

  newtok(p);

  while ((c = nextc(p)) != term) {
    if (c  == -1) {
      yyerror(p, "unterminated string meets end of file");
      return 0;
    }
    else if (c == '\\') {
      c = nextc(p);
      if (c == term) {
	tokadd(p, c);
      }
      else {
	pushback(p, c);
	tokadd(p, read_escape(p));
      }
      continue;
    }
    if (c == '#') {
      c = nextc(p);
      if (c == '{') {
	tokfix(p);
	p->lstate = EXPR_BEG;
	p->sterm = term;
	p->cmd_start = TRUE;
	yylval.node = new_str(p, tok(p), toklen(p));
	return tSTRING_PART;
      }
      tokadd(p, '#');
      pushback(p, c);
      continue;
    }
    tokadd(p, c);
  }

  tokfix(p);
  p->lstate = EXPR_END;
  p->sterm = 0;
  yylval.node = new_str(p, tok(p), toklen(p));
  return tSTRING;
}

static int
parse_qstring(parser_state *p, int term)
{
  int c;

  newtok(p);
  while ((c = nextc(p)) != term) {
    if (c  == -1)  {
      yyerror(p, "unterminated string meets end of file");
      return 0;
    }
    if (c == '\\') {
      c = nextc(p);
      switch (c) {
      case '\n':
	continue;

      case '\\':
	c = '\\';
	break;

      case '\'':
	if (term == '\'') {
	  c = '\'';
	  break;
	}
	/* fall through */
      default:
	tokadd(p, '\\');
      }
    }
    tokadd(p, c);
  }

  tokfix(p);
  yylval.node = new_str(p, tok(p), toklen(p));
  p->lstate = EXPR_END;
  return tSTRING;
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
  register int c;
  int space_seen = 0;
  int cmd_state;
  enum mrb_lex_state_enum last_state;
  int token_column;

  if (p->sterm) {
    return parse_string(p, p->sterm);
  }
  cmd_state = p->cmd_start;
  p->cmd_start = FALSE;
 retry:
  last_state = p->lstate;
  switch (c = nextc(p)) {
  case '\0':		/* NUL */
  case '\004':		/* ^D */
  case '\032':		/* ^Z */
  case -1:		/* end of script. */
    return 0;

    /* white spaces */
  case ' ': case '\t': case '\f': case '\r':
  case '\13': /* '\v' */
    space_seen = 1;
    goto retry;

  case '#':		/* it's a comment */
    skip(p, '\n');
    /* fall through */
  case '\n':
    switch (p->lstate) {
    case EXPR_BEG:
    case EXPR_FNAME:
    case EXPR_DOT:
    case EXPR_CLASS:
    case EXPR_VALUE:
      goto retry;
    default:
      break;
    }
    while ((c = nextc(p))) {
      switch (c) {
      case ' ': case '\t': case '\f': case '\r':
      case '\13': /* '\v' */
	space_seen = 1;
	break;
      case '.':
	if ((c = nextc(p)) != '.') {
	  pushback(p, c);
	  pushback(p, '.');
	  goto retry;
	}
      case -1:			/* EOF */
	goto normal_newline;
      default:
	pushback(p, c);
	goto normal_newline;
      }
    }
  normal_newline:
    p->cmd_start = TRUE;
    p->lstate = EXPR_BEG;
    return '\n';

  case '*':
    if ((c = nextc(p)) == '*') {
      if ((c = nextc(p)) == '=') {
	yylval.id = intern("**");
	p->lstate = EXPR_BEG;
	return tOP_ASGN;
      }
      pushback(p, c);
      c = tPOW;
    }
    else {
      if (c == '=') {
	yylval.id = intern("*");
	p->lstate = EXPR_BEG;
	return tOP_ASGN;
      }
      pushback(p, c);
      if (IS_SPCARG(c)) {
	yywarning(p, "`*' interpreted as argument prefix");
	c = tSTAR;
      }
      else if (IS_BEG()) {
	c = tSTAR;
      }
      else {
	c = '*';
      }
    }
    switch (p->lstate) {
    case EXPR_FNAME: case EXPR_DOT:
      p->lstate = EXPR_ARG; break;
    default:
      p->lstate = EXPR_BEG; break;
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
      if (peeks(p, "begin\n")) {
	skips(p, "\n=end\n");
      }
      goto retry;
    }
    switch (p->lstate) {
    case EXPR_FNAME: case EXPR_DOT:
      p->lstate = EXPR_ARG; break;
    default:
      p->lstate = EXPR_BEG; break;
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
    last_state = p->lstate;
    c = nextc(p);
#if 0
    // no heredoc supported yet
    if (c == '<' &&
	p->lstate != EXPR_DOT &&
	p->lstate != EXPR_CLASS &&
	!IS_END() &&
	(!IS_ARG() || space_seen)) {
      int token = heredoc_identifier();
      if (token) return token;
    }
#endif
    switch (p->lstate) {
    case EXPR_FNAME: case EXPR_DOT:
      p->lstate = EXPR_ARG; break;
    case EXPR_CLASS:
      p->cmd_start = TRUE;
    default:
      p->lstate = EXPR_BEG; break;
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
	yylval.id = intern("<<");
	p->lstate = EXPR_BEG;
	return tOP_ASGN;
      }
      pushback(p, c);
      return tLSHFT;
    }
    pushback(p, c);
    return '<';

  case '>':
    switch (p->lstate) {
    case EXPR_FNAME: case EXPR_DOT:
      p->lstate = EXPR_ARG; break;
    default:
      p->lstate = EXPR_BEG; break;
    }
    if ((c = nextc(p)) == '=') {
      return tGEQ;
    }
    if (c == '>') {
      if ((c = nextc(p)) == '=') {
	yylval.id = intern(">>");
	p->lstate = EXPR_BEG;
	return tOP_ASGN;
      }
      pushback(p, c);
      return tRSHFT;
    }
    pushback(p, c);
    return '>';

  case '"':
    p->sterm = '"';
    return tSTRING_BEG;

  case '\'':
    return parse_qstring(p, c);

  case '?':
    if (IS_END()) {
      p->lstate = EXPR_VALUE;
      return '?';
    }
    c = nextc(p);
    if (c == -1) {
      yyerror(p, "incomplete character syntax");
      return 0;
    }
    if (isspace(c)) {
      if (!IS_ARG()) {
	int c2 = 0;
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
	}
	if (c2) {
	  char buf[256];
	  snprintf(buf, 256, "invalid character syntax; use ?\\%c", c2);
	  yyerror(p, buf);
	}
      }
    ternary:
      pushback(p, c);
      p->lstate = EXPR_VALUE;
      return '?';
    }
    token_column = newtok(p);
    // need support UTF-8 if configured
    if ((isalnum(c) || c == '_')) {
      int c2 = nextc(p);
      pushback(p, c2);
      if ((isalnum(c2) || c2 == '_')) {
	goto ternary;
      }
    }
    if (c == '\\') {
      c = nextc(p);
      if (c == 'u') {
#if 0
	tokadd_utf8(p);
#endif
      }
      else {
	pushback(p, c);
	c = read_escape(p);
	tokadd(p, c);
      }
    }
    else {
      tokadd(p, c);
    }
    tokfix(p);
    yylval.node = new_str(p, tok(p), toklen(p));
    p->lstate = EXPR_END;
    return tCHAR;

  case '&':
    if ((c = nextc(p)) == '&') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
	yylval.id = intern("&&");
	p->lstate = EXPR_BEG;
	return tOP_ASGN;
      }
      pushback(p, c);
      return tANDOP;
    }
    else if (c == '=') {
      yylval.id = intern("&");
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      yywarning(p, "`&' interpreted as argument prefix");
      c = tAMPER;
    }
    else if (IS_BEG()) {
      c = tAMPER;
    }
    else {
      c = '&';
    }
    switch (p->lstate) {
    case EXPR_FNAME: case EXPR_DOT:
      p->lstate = EXPR_ARG; break;
    default:
      p->lstate = EXPR_BEG;
    }
    return c;

  case '|':
    if ((c = nextc(p)) == '|') {
      p->lstate = EXPR_BEG;
      if ((c = nextc(p)) == '=') {
	yylval.id = intern("||");
	p->lstate = EXPR_BEG;
	return tOP_ASGN;
      }
      pushback(p, c);
      return tOROP;
    }
    if (c == '=') {
      yylval.id = intern("|");
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
      yylval.id = intern("+");
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_BEG() || (IS_SPCARG(c) && arg_ambiguous(p))) {
      p->lstate = EXPR_BEG;
      pushback(p, c);
      if (c != -1 && ISDIGIT(c)) {
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
      yylval.id = intern("-");
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
      if (c != -1 && ISDIGIT(c)) {
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
    if (c != -1 && ISDIGIT(c)) {
      yyerror(p, "no .<digit> floating literal anymore; put 0 before dot");
    }
    p->lstate = EXPR_DOT;
    return '.';

  start_num:
  case '0': case '1': case '2': case '3': case '4':
  case '5': case '6': case '7': case '8': case '9':
    {
      int is_float, seen_point, seen_e, nondigit;
      
      is_float = seen_point = seen_e = nondigit = 0;
      p->lstate = EXPR_END;
      token_column = newtok(p);
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
	  if (c != -1 && ISXDIGIT(c)) {
	    do {
	      if (c == '_') {
		if (nondigit) break;
		nondigit = c;
		continue;
	      }
	      if (!ISXDIGIT(c)) break;
	      nondigit = 0;
	      tokadd(p, c);
	    } while ((c = nextc(p)) != -1);
	  }
	  pushback(p, c);
	  tokfix(p);
	  if (toklen(p) == start) {
	    no_digits();
	  }
	  else if (nondigit) goto trailing_uc;
	  yylval.node = new_int(p, tok(p), 16);
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
	    } while ((c = nextc(p)) != -1);
	  }
	  pushback(p, c);
	  tokfix(p);
	  if (toklen(p) == start) {
	    no_digits();
	  }
	  else if (nondigit) goto trailing_uc;
	  yylval.node = new_int(p, tok(p), 2);
	  return tINTEGER;
	}
	if (c == 'd' || c == 'D') {
	  /* decimal */
	  c = nextc(p);
	  if (c != -1 && ISDIGIT(c)) {
	    do {
	      if (c == '_') {
		if (nondigit) break;
		nondigit = c;
		continue;
	      }
	      if (!ISDIGIT(c)) break;
	      nondigit = 0;
	      tokadd(p, c);
	    } while ((c = nextc(p)) != -1);
	  }
	  pushback(p, c);
	  tokfix(p);
	  if (toklen(p) == start) {
	    no_digits();
	  }
	  else if (nondigit) goto trailing_uc;
	  yylval.node = new_int(p, tok(p), 10);
	  return tINTEGER;
	}
	if (c == '_') {
	  /* 0_0 */
	  goto octal_number;
	}
	if (c == 'o' || c == 'O') {
	  /* prefixed octal */
	  c = nextc(p);
	  if (c == -1 || c == '_' || !ISDIGIT(c)) {
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
	  } while ((c = nextc(p)) != -1);

	  if (toklen(p) > start) {
	    pushback(p, c);
	    tokfix(p);
	    if (nondigit) goto trailing_uc;
	    yylval.node = new_int(p, tok(p), 8);
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
	  yylval.node = new_int(p, "0", 10);
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
	    if (c0 == -1 || !ISDIGIT(c0)) {
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

	case '_':	/* `_' in number just ignored */
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
	yyerror_i(p, "trailing `%c' in number", nondigit);
      }
      tokfix(p);
      if (is_float) {
	strtod(tok(p), 0);
	if (errno == ERANGE) {
	  yywarning_s(p, "float %s out of range", tok(p));
	  errno = 0;
	}
	yylval.node = new_float(p, tok(p));
	return tFLOAT;
      }
      yylval.node = new_int(p, tok(p), 10);
      return tINTEGER;
    }

  case ')':
  case ']':
    p->paren_nest--;
  case '}':
    COND_LEXPOP();
    CMDARG_LEXPOP();
    if (c == ')')
      p->lstate = EXPR_ENDFN;
    else
      p->lstate = EXPR_ENDARG;
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
    if (IS_END() || ISSPACE(c)) {
      pushback(p, c);
      p->lstate = EXPR_BEG;
      return ':';
    }
    switch (c) {
    case '\'':
#if 0
      p->lex_strterm = new_strterm(p, str_ssym, c, 0);
#endif
      break;
    case '"':
#if 0
      p->lex_strterm = new_strterm(p, str_dsym, c, 0);
#endif
      break;
    default:
      pushback(p, c);
      break;
    }
    p->lstate = EXPR_FNAME;
    return tSYMBEG;

  case '/':
    if (IS_BEG()) {
#if 0
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
#endif
      return tREGEXP_BEG;
    }
    if ((c = nextc(p)) == '=') {
      yylval.id = intern("/");
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    pushback(p, c);
    if (IS_SPCARG(c)) {
      arg_ambiguous(p);
#if 0
      p->lex_strterm = new_strterm(p, str_regexp, '/', 0);
#endif
      return tREGEXP_BEG;
    }
    switch (p->lstate) {
    case EXPR_FNAME: case EXPR_DOT:
      p->lstate = EXPR_ARG; break;
    default:
      p->lstate = EXPR_BEG; break;
    }
    return '/';

  case '^':
    if ((c = nextc(p)) == '=') {
      yylval.id = intern("^");
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    switch (p->lstate) {
    case EXPR_FNAME: case EXPR_DOT:
      p->lstate = EXPR_ARG; break;
    default:
      p->lstate = EXPR_BEG; break;
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
      space_seen = 1;
      goto retry; /* skip \\n */
    }
    pushback(p, c);
    return '\\';

  case '%':
    if (IS_BEG()) {
      int term;
#if 0
      int paren;
#endif

      c = nextc(p);
    quotation:
      if (c == -1 || !ISALNUM(c)) {
	term = c;
	c = 'Q';
      }
      else {
	term = nextc(p);
	if (isalnum(term)) {
	  yyerror(p, "unknown type of %string");
	  return 0;
	}
      }
      if (c == -1 || term == -1) {
	yyerror(p, "unterminated quoted string meets end of file");
	return 0;
      }
#if 0
      paren = term;
#endif
      if (term == '(') term = ')';
      else if (term == '[') term = ']';
      else if (term == '{') term = '}';
      else if (term == '<') term = '>';
      p->sterm = term;
#if 0
      else paren = 0;
#endif

      switch (c) {
      case 'Q':
#if 0
	p->lex_strterm = new_strterm(p, str_dquote, term, paren);
#endif
	return tSTRING_BEG;

      case 'q':
#if 0
	p->lex_strterm = new_strterm(p, str_squote, term, paren);
#endif
	return tSTRING_BEG;

      case 'W':
#if 0
	p->lex_strterm = new_strterm(p, str_dword, term, paren);
#endif
	do {c = nextc(p);} while (isspace(c));
	pushback(p, c);
	return tWORDS_BEG;

      case 'w':
#if 0
	p->lex_strterm = new_strterm(p, str_sword, term, paren);
#endif
	do {c = nextc(p);} while (isspace(c));
	pushback(p, c);
	return tQWORDS_BEG;

      case 'r':
#if 0
	p->lex_strterm = new_strterm(p, str_regexp, term, paren);
#endif
	return tREGEXP_BEG;

      case 's':
#if 0
	p->lex_strterm = new_strterm(p, str_ssym, term, paren);
#endif
	p->lstate = EXPR_FNAME;
	return tSYMBEG;

      default:
	yyerror(p, "unknown type of %string");
	return 0;
      }
    }
    if ((c = nextc(p)) == '=') {
      yylval.id = intern("%");
      p->lstate = EXPR_BEG;
      return tOP_ASGN;
    }
    if (IS_SPCARG(c)) {
      goto quotation;
    }
    switch (p->lstate) {
    case EXPR_FNAME: case EXPR_DOT:
      p->lstate = EXPR_ARG; break;
    default:
      p->lstate = EXPR_BEG; break;
    }
    pushback(p, c);
    return '%';

  case '$':
    p->lstate = EXPR_END;
    token_column = newtok(p);
    c = nextc(p);
    switch (c) {
    case '_':		     /* $_: last read line string */
      c = nextc(p);
      pushback(p, c);
      c = '_';
      /* fall through */
    case '~':		   /* $~: match-data */
    case '*':		   /* $*: argv */
    case '$':		   /* $$: pid */
    case '?':		   /* $?: last status */
    case '!':		   /* $!: error string */
    case '@':		   /* $@: error position */
    case '/':		   /* $/: input record separator */
    case '\\':		   /* $\: output record separator */
    case ';':		   /* $;: field separator */
    case ',':		   /* $,: output field separator */
    case '.':		   /* $.: last read line number */
    case '=':		   /* $=: ignorecase */
    case ':':		   /* $:: load path */
    case '<':		   /* $<: reading filename */
    case '>':		   /* $>: default output handle */
    case '\"':		   /* $": already loaded files */
      tokadd(p, '$');
      tokadd(p, c);
      tokfix(p);
      yylval.id = intern(tok(p));
      return tGVAR;

    case '-':
      tokadd(p, '$');
      tokadd(p, c);
      c = nextc(p);
      pushback(p, c);
    gvar:
      tokfix(p);
      yylval.id = intern(tok(p));
      return tGVAR;

    case '&':		/* $&: last match */
    case '`':		/* $`: string before last match */
    case '\'':		/* $': string after last match */
    case '+':		/* $+: string matches last paren. */
      if (last_state == EXPR_FNAME) {
	tokadd(p, '$');
	tokadd(p, c);
	goto gvar;
      }
      yylval.node = new_back_ref(p, c);
      return tBACK_REF;

    case '1': case '2': case '3':
    case '4': case '5': case '6':
    case '7': case '8': case '9':
      tokadd(p, '$');
      do {
	tokadd(p, c);
	c = nextc(p);
      } while (c != -1 && isdigit(c));
      pushback(p, c);
      if (last_state == EXPR_FNAME) goto gvar;
      tokfix(p);
      yylval.node = new_nth_ref(p, atoi(tok(p)+1)); 
      return tNTH_REF;

    default:
      if (!identchar(c)) {
	pushback(p,  c);
	return '$';
      }
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
    if (c != -1 && isdigit(c)) {
      if (p->bidx == 1) {
	yyerror_i(p, "`@%c' is not allowed as an instance variable name", c);
      }
      else {
	yyerror_i(p, "`@@%c' is not allowed as a class variable name", c);
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
      yyerror_i(p,  "Invalid char `\\x%02X' in expression", c);
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

    last_state = p->lstate;
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
	}
	if (result == 0 && isupper(tok(p)[0])) {
	  result = tCONSTANT;
	}
	else {
	  result = tIDENTIFIER;
	}
      }

      if (IS_LABEL_POSSIBLE()) {
	if (IS_LABEL_SUFFIX(0)) {
	  p->lstate = EXPR_BEG;
	  nextc(p);
	  tokfix(p);
	  yylval.id = intern(tok(p));
	  return tLABEL;
	}
      }
      if (p->lstate != EXPR_DOT) {
	const struct kwtable *kw;

	/* See if it is a reserved word.  */
	kw = mrb_reserved_word(tok(p), toklen(p));
	if (kw) {
	  enum mrb_lex_state_enum state = p->lstate;
	  p->lstate = kw->state;
	  if (state == EXPR_FNAME) {
	    yylval.id = intern(kw->name);
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

      if (IS_BEG() ||
	  p->lstate == EXPR_DOT ||
	  IS_ARG()) {
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
      mrb_sym ident = intern(tok(p));

      yylval.id = ident;
#if 0
      if (last_state != EXPR_DOT && islower(tok(p)[0]) && lvar_defined(ident)) {
	p->lstate = EXPR_END;
      }
#endif
    }
    return result;
  }
}

static int
yylex(void *lval, parser_state *p)
{
    int t;

    p->ylval = lval;
    t = parser_yylex(p);

    return t;
}

static void
start_parser(parser_state *p)
{
  node *tree;

  if (setjmp(p->jmp) != 0) {
    yyerror(p, "memory allocation error");
    p->nerr++;
    p->tree = p->begin_tree = 0;
    return;
  }
  yyparse(p);
  tree = p->tree;
  if (!tree) {
    if (p->begin_tree) {
      tree = p->begin_tree;
    }
    else {
      tree = new_nil(p);
    }
  }
  else if (p->begin_tree) {
    tree = new_begin(p, p->begin_tree);
    append(tree, p->tree);
  }
}

static parser_state*
parser_new(mrb_state *mrb)
{
  mrb_pool *pool;
  parser_state *p;

  pool = mrb_pool_open(mrb);
  if (!pool) return 0;
  p = mrb_pool_alloc(pool, sizeof(parser_state));
  if (!p) return 0;

  memset(p, 0, sizeof(parser_state));
  p->mrb = mrb;
  p->pool = pool;
  p->in_def = p->in_single = 0;

  p->cmd_start = TRUE;
  p->in_def = p->in_single = FALSE;

  p->capture_errors = 0;

  p->lineno = 1;
#if defined(PARSER_TEST) || defined(PARSER_DEBUG)
  yydebug = 1;
#endif

  return p;
}

parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f)
{
  parser_state *p;
 
  p = parser_new(mrb);
  if (!p) return 0;
  p->s = p->send = NULL;
  p->f = f;

  start_parser(p);
  return p;
}

parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, size_t len)
{
  parser_state *p;

  p = parser_new(mrb);
  if (!p) return 0;
  p->s = s;
  p->send = s + len;
  p->f = NULL;

  start_parser(p);
  return p;
}

parser_state*
mrb_parse_nstring_ext(mrb_state *mrb, const char *s, size_t len)
{
  parser_state *p;

  p = parser_new(mrb);
  if (!p) return 0;
  p->s = s;
  p->send = s + len;
  p->f = NULL;
  p->capture_errors = 1;

  start_parser(p);
  return p;
}

parser_state*
mrb_parse_string(mrb_state *mrb, const char *s)
{
  return mrb_parse_nstring(mrb, s, strlen(s));
}

#define PARSER_DUMP

void parser_dump(mrb_state *mrb, node *tree, int offset);
int mrb_generate_code(mrb_state*, mrb_ast_node*);

int
mrb_compile_file(mrb_state * mrb, FILE *f)
{
  parser_state *p;
  int n;

  p = mrb_parse_file(mrb, f);
  if (!p) return -1;
  if (!p->tree) return -1;
  if (p->nerr) return -1;
#ifdef PARSER_DUMP
  parser_dump(mrb, p->tree, 0);
#endif
  n = mrb_generate_code(mrb, p->tree);
  mrb_pool_close(p->pool);

  return n;
}

const char*
mrb_parser_filename(parser_state *p, const char *s)
{
  if (s) {
    p->filename = strdup(s);
  }
  return p->filename;
}

int
mrb_parser_lineno(struct mrb_parser_state *p, int n)
{
  if (n <= 0) {
    return p->lineno;
  }
  return p->lineno = n;
}

int
mrb_compile_nstring(mrb_state *mrb, char *s, size_t len)
{
  parser_state *p;
  int n;

  p = mrb_parse_nstring(mrb, s, len);
  if (!p) return -1;
  if (!p->tree) return -1;
  if (p->nerr) return -1;
#ifdef PARSER_DUMP
  parser_dump(mrb, p->tree, 0);
#endif
  n = mrb_generate_code(mrb, p->tree);
  mrb_pool_close(p->pool);

  return n;
}

int
mrb_compile_string(mrb_state *mrb, char *s)
{
  return mrb_compile_nstring(mrb, s, strlen(s));
}

static void
dump_prefix(int offset)
{
  while (offset--) {
    putc(' ', stdout);
    putc(' ', stdout);
  }
}

static void
dump_recur(mrb_state *mrb, node *tree, int offset)
{
  while (tree) {
    parser_dump(mrb, tree->car, offset);
    tree = tree->cdr;
  }
}

void
parser_dump(mrb_state *mrb, node *tree, int offset)
{
  int n;

  if (!tree) return;
 again:
  dump_prefix(offset);
  n = (int)(intptr_t)tree->car;
  tree = tree->cdr;
  switch (n) {
  case NODE_BEGIN:
    printf("NODE_BEGIN:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_RESCUE:
    printf("NODE_RESCUE:\n");
    if (tree->car) {
      dump_prefix(offset+1);
      printf("body:\n");
      parser_dump(mrb, tree->car, offset+2);
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n2 = tree->car;

      dump_prefix(offset+1);
      printf("rescue:\n");
      while (n2) {
	node *n3 = n2->car;
	if (n3->car) {
	  dump_prefix(offset+2);
	  printf("handle classes:\n");
	  dump_recur(mrb, n3->car, offset+3);
	}
	if (n3->cdr->car) {
	  dump_prefix(offset+2);
	  printf("exc_var:\n");
	  parser_dump(mrb, n3->cdr->car, offset+3);
	}
	if (n3->cdr->cdr->car) {
	  dump_prefix(offset+2);
	  printf("rescue body:\n");
	  parser_dump(mrb, n3->cdr->cdr->car, offset+3);
	}
	n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      dump_prefix(offset+1);
      printf("else:\n");
      parser_dump(mrb, tree->car, offset+2);
    }
    break;

  case NODE_ENSURE:
    printf("NODE_ENSURE:\n");
    dump_prefix(offset+1);
    printf("body:\n");
    parser_dump(mrb, tree->car, offset+2);
    dump_prefix(offset+1);
    printf("ensure:\n");
    parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_LAMBDA:
    printf("NODE_BLOCK:\n");
    goto block;

  case NODE_BLOCK:
  block:
    printf("NODE_BLOCK:\n");
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
	dump_prefix(offset+1);
	printf("mandatory args:\n");
	dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("optional args:\n");
	{
	  node *n2 = n->car;

	  while (n2) {
	    dump_prefix(offset+2);
	    printf("%s=", mrb_sym2name(mrb, (mrb_sym)n2->car->car));
	    parser_dump(mrb, n2->car->cdr, 0);
	    n2 = n2->cdr;
	  }
	}
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("rest=*%s\n", mrb_sym2name(mrb, (mrb_sym)n->car));
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("post mandatory args:\n");
	dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
	dump_prefix(offset+1);
	printf("blk=&%s\n", mrb_sym2name(mrb, (mrb_sym)n));
      }
    }
    dump_prefix(offset+1);
    printf("body:\n");
    parser_dump(mrb, tree->cdr->car, offset+2);
    break;

  case NODE_IF:
    printf("NODE_IF:\n");
    dump_prefix(offset+1);
    printf("cond:\n");
    parser_dump(mrb, tree->car, offset+2);
    dump_prefix(offset+1);
    printf("then:\n");
    parser_dump(mrb, tree->cdr->car, offset+2);
    if (tree->cdr->cdr->car) {
      dump_prefix(offset+1);
      printf("else:\n");
      parser_dump(mrb, tree->cdr->cdr->car, offset+2);
    }
    break;

  case NODE_AND:
    printf("NODE_AND:\n");
    parser_dump(mrb, tree->car, offset+1);
    parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_OR:
    printf("NODE_OR:\n");
    parser_dump(mrb, tree->car, offset+1);
    parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_CASE:
    printf("NODE_CASE:\n");
    if (tree->car) {
      parser_dump(mrb, tree->car, offset+1);
    }
    tree = tree->cdr;
    while (tree) {
      dump_prefix(offset+1);
      printf("case:\n");
      dump_recur(mrb, tree->car->car, offset+2);
      dump_prefix(offset+1);
      printf("body:\n");
      parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_WHILE:
    printf("NODE_WHILE:\n");
    dump_prefix(offset+1);
    printf("cond:\n");
    parser_dump(mrb, tree->car, offset+2);
    dump_prefix(offset+1);
    printf("body:\n");
    parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_UNTIL:
    printf("NODE_UNTIL:\n");
    dump_prefix(offset+1);
    printf("cond:\n");
    parser_dump(mrb, tree->car, offset+2);
    dump_prefix(offset+1);
    printf("body:\n");
    parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_FOR:
    printf("NODE_FOR:\n");
    dump_prefix(offset+1);
    printf("var:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
	dump_prefix(offset+2);
	printf("pre:\n");
	dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
	if (n2->car) {
	  dump_prefix(offset+2);
	  printf("rest:\n");
	  parser_dump(mrb, n2->car, offset+3);
	}
	n2 = n2->cdr;
	if (n2) {
	  if (n2->car) {
	    dump_prefix(offset+2);
	    printf("post:\n");
	    dump_recur(mrb, n2->car, offset+3);
	  }
	}
      }
    }
    tree = tree->cdr;
    dump_prefix(offset+1);
    printf("in:\n");
    parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(offset+1);
    printf("do:\n");
    parser_dump(mrb, tree->car, offset+2);
    break;

  case NODE_SCOPE:
    printf("NODE_SCOPE:\n");
    dump_prefix(offset+1);
    printf("local variables:\n");
    {
      node *n2 = tree->car;

      while (n2) {
	dump_prefix(offset+2);
	printf("%s\n", mrb_sym2name(mrb, (mrb_sym)n2->car));
	n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    offset++;
    goto again;

  case NODE_FCALL:
  case NODE_CALL:
    printf("NODE_CALL:\n");
    parser_dump(mrb, tree->car, offset+1);
    dump_prefix(offset+1);
    printf("method='%s' (%d)\n", 
	   mrb_sym2name(mrb, (mrb_sym)tree->cdr->car),
	   (int)(intptr_t)tree->cdr->car);
    tree = tree->cdr->cdr->car;
    if (tree) {
      dump_prefix(offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
	dump_prefix(offset+1);
	printf("block:\n");
	parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_DOT2:
    printf("NODE_DOT2:\n");
    parser_dump(mrb, tree->car, offset+1);
    parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_DOT3:
    printf("NODE_DOT3:\n");
    parser_dump(mrb, tree->car, offset+1);
    parser_dump(mrb, tree->cdr, offset+1);
    break;

  case NODE_COLON2:
    printf("NODE_COLON2:\n");
    parser_dump(mrb, tree->car, offset+1);
    dump_prefix(offset+1);
    printf("::%s\n", mrb_sym2name(mrb, (mrb_sym)tree->cdr));
    break;

  case NODE_COLON3:
    printf("NODE_COLON3:\n");
    dump_prefix(offset+1);
    printf("::%s\n", mrb_sym2name(mrb, (mrb_sym)tree));
    break;

  case NODE_ARRAY:
    printf("NODE_ARRAY:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_HASH:
    printf("NODE_HASH:\n");
    while (tree) {
      dump_prefix(offset+1);
      printf("key:\n");
      parser_dump(mrb, tree->car->car, offset+2);
      dump_prefix(offset+1);
      printf("value:\n");
      parser_dump(mrb, tree->car->cdr, offset+2);
      tree = tree->cdr;
    }
    break;

  case NODE_SPLAT:
    printf("NODE_SPLAT:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_ASGN:
    printf("NODE_ASGN:\n");
    dump_prefix(offset+1);
    printf("lhs:\n");
    parser_dump(mrb, tree->car, offset+2);
    dump_prefix(offset+1);
    printf("rhs:\n");
    parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_MASGN:
    printf("NODE_MASGN:\n");
    dump_prefix(offset+1);
    printf("mlhs:\n");
    {
      node *n2 = tree->car;

      if (n2->car) {
	dump_prefix(offset+2);
	printf("pre:\n");
	dump_recur(mrb, n2->car, offset+3);
      }
      n2 = n2->cdr;
      if (n2) {
	if (n2->car) {
	  dump_prefix(offset+2);
	  printf("rest:\n");
	  parser_dump(mrb, n2->car, offset+3);
	}
	n2 = n2->cdr;
	if (n2) {
	  if (n2->car) {
	    dump_prefix(offset+2);
	    printf("post:\n");
	    dump_recur(mrb, n2->car, offset+3);
	  }
	}
      }
    }
    dump_prefix(offset+1);
    printf("rhs:\n");
    parser_dump(mrb, tree->cdr, offset+2);
    break;

  case NODE_OP_ASGN:
    printf("NODE_OP_ASGN:\n");
    dump_prefix(offset+1);
    printf("lhs:\n");
    parser_dump(mrb, tree->car, offset+2);
    tree = tree->cdr;
    dump_prefix(offset+1);
    printf("op='%s' (%d)\n", mrb_sym2name(mrb, (mrb_sym)tree->car), (int)(intptr_t)tree->car);
    tree = tree->cdr;
    parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_SUPER:
    printf("NODE_SUPER:\n");
    if (tree) {
      dump_prefix(offset+1);
      printf("args:\n");
      dump_recur(mrb, tree->car, offset+2);
      if (tree->cdr) {
	dump_prefix(offset+1);
	printf("block:\n");
	parser_dump(mrb, tree->cdr, offset+2);
      }
    }
    break;

  case NODE_ZSUPER:
    printf("NODE_ZSUPER\n");
    break;

  case NODE_RETURN:
    printf("NODE_RETURN:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_YIELD:
    printf("NODE_YIELD:\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_BREAK:
    printf("NODE_BREAK:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_NEXT:
    printf("NODE_NEXT:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_REDO:
    printf("NODE_REDO\n");
    break;

  case NODE_RETRY:
    printf("NODE_RETRY\n");
    break;

  case NODE_LVAR:
    printf("NODE_LVAR %s\n", mrb_sym2name(mrb, (mrb_sym)tree));
    break;

  case NODE_GVAR:
    printf("NODE_GVAR %s\n", mrb_sym2name(mrb, (mrb_sym)tree));
    break;

  case NODE_IVAR:
    printf("NODE_IVAR %s\n", mrb_sym2name(mrb, (mrb_sym)tree));
    break;

  case NODE_CVAR:
    printf("NODE_CVAR %s\n", mrb_sym2name(mrb, (mrb_sym)tree));
    break;

  case NODE_CONST:
    printf("NODE_CONST %s\n", mrb_sym2name(mrb, (mrb_sym)tree));
    break;

  case NODE_BACK_REF:
    printf("NODE_BACK_REF:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_NTH_REF:
    printf("NODE_NTH_REF:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_ARG:
    printf("NODE_ARG %s\n", mrb_sym2name(mrb, (mrb_sym)tree));
    break;

  case NODE_BLOCK_ARG:
    printf("NODE_BLOCK_ARG:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_INT:
    printf("NODE_INT %s base %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr->car);
    break;

  case NODE_FLOAT:
    printf("NODE_FLOAT %s\n", (char*)tree);
    break;

  case NODE_NEGATE:
    printf("NODE_NEGATE\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_STR:
    printf("NODE_STR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DSTR:
    printf("NODE_DSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_SYM:
    printf("NODE_SYM :%s\n", mrb_sym2name(mrb, (mrb_sym)tree));
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
	   mrb_sym2name(mrb, (mrb_sym)tree->car),
	   mrb_sym2name(mrb, (mrb_sym)tree->cdr));
    break;

  case NODE_UNDEF:
    printf("NODE_UNDEF %s:\n",
	   mrb_sym2name(mrb, (mrb_sym)tree));
    break;

  case NODE_CLASS:
    printf("NODE_CLASS:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(offset+1);
      printf(":%s\n", mrb_sym2name(mrb, (mrb_sym)tree->car->cdr));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(offset+1);
      printf("::%s\n", mrb_sym2name(mrb, (mrb_sym)tree->car->cdr));
    }
    else {
      parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(offset+1);
      printf("::%s\n", mrb_sym2name(mrb, (mrb_sym)tree->car->cdr));
    }
    if (tree->cdr->car) {
      dump_prefix(offset+1);
      printf("super:\n");
      parser_dump(mrb, tree->cdr->car, offset+2);
    }
    dump_prefix(offset+1);
    printf("body:\n");
    parser_dump(mrb, tree->cdr->cdr->car->cdr, offset+2);
    break;

  case NODE_MODULE:
    printf("NODE_MODULE:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(offset+1);
      printf(":%s\n", mrb_sym2name(mrb, (mrb_sym)tree->car->cdr));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(offset+1);
      printf("::%s\n", mrb_sym2name(mrb, (mrb_sym)tree->car->cdr));
    }
    else {
      parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(offset+1);
      printf("::%s\n", mrb_sym2name(mrb, (mrb_sym)tree->car->cdr));
    }
    dump_prefix(offset+1);
    printf("body:\n");
    parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_SCLASS:
    printf("NODE_SCLASS:\n");
    parser_dump(mrb, tree->car, offset+1);
    dump_prefix(offset+1);
    printf("body:\n");
    parser_dump(mrb, tree->cdr->car->cdr, offset+2);
    break;

  case NODE_DEF:
    printf("NODE_DEF:\n");
    dump_prefix(offset+1);
    printf("%s\n", mrb_sym2name(mrb, (mrb_sym)tree->car));
    tree = tree->cdr;
    dump_prefix(offset+1);
    printf("local variables:\n");
    {
      node *n2 = tree->car;

      while (n2) {
	dump_prefix(offset+2);
	if (n2->car)
	  printf("%s\n", mrb_sym2name(mrb, (mrb_sym)n2->car));
	n2 = n2->cdr;
      }
    }
    tree = tree->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
	dump_prefix(offset+1);
	printf("mandatory args:\n");
	dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("optional args:\n");
	{
	  node *n2 = n->car;

	  while (n2) {
	    dump_prefix(offset+2);
	    printf("%s=", mrb_sym2name(mrb, (mrb_sym)n2->car->car));
	    parser_dump(mrb, n2->car->cdr, 0);
	    n2 = n2->cdr;
	  }
	}
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("rest=*%s\n", mrb_sym2name(mrb, (mrb_sym)n->car));
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("post mandatory args:\n");
	dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
	dump_prefix(offset+1);
	printf("blk=&%s\n", mrb_sym2name(mrb, (mrb_sym)n));
      }
    }
    parser_dump(mrb, tree->cdr->car, offset+1);
    break;

  case NODE_SDEF:
    printf("NODE_SDEF:\n");
    parser_dump(mrb, tree->car, offset+1);
    tree = tree->cdr;
    dump_prefix(offset+1);
    printf(":%s\n", mrb_sym2name(mrb, (mrb_sym)tree->car));
    tree = tree->cdr->cdr;
    if (tree->car) {
      node *n = tree->car;

      if (n->car) {
	dump_prefix(offset+1);
	printf("mandatory args:\n");
	dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("optional args:\n");
	{
	  node *n2 = n->car;

	  while (n2) {
	    dump_prefix(offset+2);
	    printf("%s=", mrb_sym2name(mrb, (mrb_sym)n2->car->car));
	    parser_dump(mrb, n2->car->cdr, 0);
	    n2 = n2->cdr;
	  }
	}
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("rest=*%s\n", mrb_sym2name(mrb, (mrb_sym)n->car));
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("post mandatory args:\n");
	dump_recur(mrb, n->car, offset+2);
      }
      n = n->cdr;
      if (n) {
	dump_prefix(offset+1);
	printf("blk=&%s\n", mrb_sym2name(mrb, (mrb_sym)n));
      }
    }
    tree = tree->cdr;
    parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_POSTEXE:
    printf("NODE_POSTEXE:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  default:
    printf("node type: %d (0x%x)\n", (int)n, (int)n);
    break;
  }
  return;
}

#ifdef PARSER_TEST
int
main()
{
  mrb_state *mrb = mrb_open();
  int n;

  n = mrb_compile_string(mrb, "\
def fib(n)\n\
  if n<2\n\
    n\n\
  else\n\
    fib(n-2)+fib(n-1)\n\
  end\n\
end\n\
print(fib(20), \"\\n\")\n\
");
  printf("ret: %d\n", n);

  return 0;
}
#endif
