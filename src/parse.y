/*
** parse.y - mruby parser
**
** See Copyright Notice in mruby.h
*/

%{
#undef PARSER_DEBUG

#define YYDEBUG 1
#define YYERROR_VERBOSE 1
/*
 * Force yacc to use our memory management.  This is a little evil because
 * the macros assume that "parser_state *p" is in scope
 */
#define YYMALLOC(n)    mrb_malloc(p->mrb, (n))
#define YYFREE(o)      mrb_free(p->mrb, (o))
#define YYSTACK_USE_ALLOCA 0

#include <ctype.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include "mruby.h"
#include "mruby/compile.h"
#include "mruby/proc.h"
#include "lex.h"
#include "node.h"
#include "node_type.h"

#define YYLEX_PARAM p

static int yylex(void *lval, parser_state *p);
static void yyerror(parser_state *p, const char *s);
static void yywarn(parser_state *p, const char *s);
static void backref_error(parser_state *p, node *n);

#define sym(x) ((mrb_sym)(intptr_t)(x))
#define nsym(x) ((node*)(intptr_t)(x))

// xxx -----------------------------

%}

%pure-parser
%parse-param {parser_state *p}
%lex-param {parser_state *p}

%union {
    node *nd;
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

%token <id>  tIDENTIFIER tFID tGVAR tIVAR tCONSTANT tCVAR tLABEL
%token <nd>  tINTEGER tFLOAT tCHAR tXSTRING tREGEXP
%token <nd>  tSTRING tSTRING_PART tSTRING_MID
%token <nd>  tNTH_REF tBACK_REF
%token <num> tREGEXP_END

%type <nd> singleton string string_rep string_interp xstring regexp
%type <nd> literal numeric cpath symbol
%type <nd> top_compstmt top_stmts top_stmt
%type <nd> bodystmt compstmt stmts stmt expr arg primary command command_call method_call
%type <nd> expr_value arg_value primary_value
%type <nd> if_tail opt_else case_body cases opt_rescue exc_list exc_var opt_ensure
%type <nd> args call_args opt_call_args
%type <nd> paren_args opt_paren_args variable
%type <nd> command_args aref_args opt_block_arg block_arg var_ref var_lhs
%type <nd> command_asgn mrhs superclass block_call block_command
%type <nd> f_block_optarg f_block_opt
%type <nd> f_arglist f_args f_arg f_arg_item f_optarg f_marg f_marg_list f_margs
%type <nd> assoc_list assocs assoc undef_list backref for_var
%type <nd> block_param opt_block_param block_param_def f_opt
%type <nd> bv_decls opt_bv_decl bvar f_larglist lambda_body
%type <nd> brace_block cmd_brace_block do_block lhs none f_bad_arg
%type <nd> mlhs mlhs_list mlhs_post mlhs_basic mlhs_item mlhs_node mlhs_inner
%type <id> fsym sym basic_symbol operation operation2 operation3
%type <id> cname fname op f_rest_arg f_block_arg opt_f_block_arg f_norm_arg
%type <nd> heredoc words symbols

%token tUPLUS             /* unary+ */
%token tUMINUS            /* unary- */
%token tPOW               /* ** */
%token tCMP               /* <=> */
%token tEQ                /* == */
%token tEQQ               /* === */
%token tNEQ               /* != */
%token tGEQ               /* >= */
%token tLEQ               /* <= */
%token tANDOP tOROP       /* && and || */
%token tMATCH tNMATCH     /* =~ and !~ */
%token tDOT2 tDOT3        /* .. and ... */
%token tAREF tASET        /* [] and []= */
%token tLSHFT tRSHFT      /* << and >> */
%token tCOLON2            /* :: */
%token tCOLON3            /* :: at EXPR_BEG */
%token <id> tOP_ASGN      /* +=, -=  etc. */
%token tASSOC             /* => */
%token tLPAREN            /* ( */
%token tLPAREN_ARG        /* ( */
%token tRPAREN            /* ) */
%token tLBRACK            /* [ */
%token tLBRACE            /* { */
%token tLBRACE_ARG        /* { */
%token tSTAR              /* * */
%token tAMPER             /* & */
%token tLAMBDA            /* -> */
%token tSYMBEG tREGEXP_BEG tWORDS_BEG tSYMBOLS_BEG
%token tSTRING_BEG tXSTRING_BEG tSTRING_DVAR tLAMBEG
%token <nd> tHEREDOC_BEG  /* <<, <<- */
%token tHEREDOC_END tLITERAL_DELIM tHD_LITERAL_DELIM
%token <nd> tHD_STRING_PART tHD_STRING_MID

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
		     if (!p->locals) p->locals = cons(0,0);
		   }
		  top_compstmt
		    {
		      p->tree = new_scope(p, $2);
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
		      $$ = new_begin(p, 0);
		    }
		;

top_stmt	: stmt
		| keyword_BEGIN
		    {
		      $<nd>$ = local_switch(p);
		    }
		  '{' top_compstmt '}'
		    {
		      yyerror(p, "BEGIN not supported");
		      local_resume(p, $<nd>2);
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
			$$ = push($1, $3);
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
		      yyerror(p, "END not suported");
		      $$ = new_postexe(p, $3);
		    }
		| command_asgn
		| mlhs '=' command_call
		    {
		      $$ = new_masgn(p, $1, $3);
		    }
		| var_lhs tOP_ASGN command_call
		    {
		      $$ = new_op_asgn(p, $1, $2, $3);
		    }
		| primary_value '[' opt_call_args rbracket tOP_ASGN command_call
		    {
		      $$ = new_op_asgn(p, new_call(p, $1, intern2("[]",2), $3), $5, $6);
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
		      $$ = new_call(p, $1, intern2("[]",2), $3);
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
		      $$ = new_call(p, $1, intern2("[]",2), $3);
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
		      $$ = cons((node*)1, nsym($2));
		    }
		| cname
		    {
		      $$ = cons((node*)0, nsym($1));
		    }
		| primary_value tCOLON2 cname
		    {
		      $$ = cons($1, nsym($3));
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
		| basic_symbol
		;

undef_list	: fsym
		    {
		      $$ = new_undef(p, $1);
		    }
		| undef_list ',' {p->lstate = EXPR_FNAME;} fsym
		    {
		      $$ = push($1, nsym($4));
		    }
		;

op		: '|'		{ $$ = intern_c('|'); }
		| '^'		{ $$ = intern_c('^'); }
		| '&'		{ $$ = intern_c('&'); }
		| tCMP		{ $$ = intern2("<=>",3); }
		| tEQ		{ $$ = intern2("==",2); }
		| tEQQ		{ $$ = intern2("===",3); }
		| tMATCH	{ $$ = intern2("=~",2); }
		| tNMATCH	{ $$ = intern2("!~",2); }
		| '>'		{ $$ = intern_c('>'); }
		| tGEQ		{ $$ = intern2(">=",2); }
		| '<'		{ $$ = intern_c('<'); }
		| tLEQ		{ $$ = intern2("<=",2); }
		| tNEQ		{ $$ = intern2("!=",2); }
		| tLSHFT	{ $$ = intern2("<<",2); }
		| tRSHFT	{ $$ = intern2(">>",2); }
		| '+'		{ $$ = intern_c('+'); }
		| '-'		{ $$ = intern_c('-'); }
		| '*'		{ $$ = intern_c('*'); }
		| tSTAR		{ $$ = intern_c('*'); }
		| '/'		{ $$ = intern_c('/'); }
		| '%'		{ $$ = intern_c('%'); }
		| tPOW		{ $$ = intern2("**",2); }
		| '!'		{ $$ = intern_c('!'); }
		| '~'		{ $$ = intern_c('~'); }
		| tUPLUS	{ $$ = intern2("+@",2); }
		| tUMINUS	{ $$ = intern2("-@",2); }
		| tAREF		{ $$ = intern2("[]",2); }
		| tASET		{ $$ = intern2("[]=",3); }
		| '`'		{ $$ = intern_c('`'); }
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
		      $$ = new_op_asgn(p, new_call(p, $1, intern2("[]",2), $3), $5, $6);
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
		      $$ = call_bin_op(p, $1, "=~", $3);
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
		      $$ = cons(new_hash(p, $1), 0);
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
		| args ',' heredoc_bodies arg_value
		    {
		      $$ = push($1, $4);
		    }
		| args ',' heredoc_bodies tSTAR arg_value
		    {
		      $$ = push($1, new_splat(p, $5));
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
		| xstring
		| regexp
		| heredoc
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
		      $$ = $2;
		    }
		| tLPAREN_ARG {p->lstate = EXPR_ENDARG;} rparen
		    {
		      $$ = 0;
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
		      $<nd>$ = local_switch(p);
		    }
		  bodystmt
		  keyword_end
		    {
		      $$ = new_class(p, $2, $3, $5);
		      local_resume(p, $<nd>4);
		    }
		| keyword_class tLSHFT expr
		    {
		      $<num>$ = p->in_def;
		      p->in_def = 0;
		    }
		  term
		    {
		      $<nd>$ = cons(local_switch(p), (node*)(intptr_t)p->in_single);
		      p->in_single = 0;
		    }
		  bodystmt
		  keyword_end
		    {
		      $$ = new_sclass(p, $3, $7);
		      local_resume(p, $<nd>6->car);
		      p->in_def = $<num>4;
		      p->in_single = (int)(intptr_t)$<nd>6->cdr;
		    }
		| keyword_module cpath
		    {
		      if (p->in_def || p->in_single)
			yyerror(p, "module definition in method body");
		      $<nd>$ = local_switch(p);
		    }
		  bodystmt
		  keyword_end
		    {
		      $$ = new_module(p, $2, $4);
		      local_resume(p, $<nd>3);
		    }
		| keyword_def fname
		    {
		      p->in_def++;
		      $<nd>$ = local_switch(p);
		    }
		  f_arglist
		  bodystmt
		  keyword_end
		    {
		      $$ = new_def(p, $2, $4, $5);
		      local_resume(p, $<nd>3);
		      p->in_def--;
		    }
		| keyword_def singleton dot_or_colon {p->lstate = EXPR_FNAME;} fname
		    {
		      p->in_single++;
		      p->lstate = EXPR_ENDFN; /* force for args */
		      $<nd>$ = local_switch(p);
		    }
		  f_arglist
		  bodystmt
		  keyword_end
		    {
		      $$ = new_sdef(p, $2, $5, $7, $8);
		      local_resume(p, $<nd>6);
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
		      $$ = new_call(p, $1, intern2("call",4), $3);
		    }
		| primary_value tCOLON2 paren_args
		    {
		      $$ = new_call(p, $1, intern2("call",4), $3);
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
		      $$ = new_call(p, $1, intern2("[]",2), $3);
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
		| words
		| symbols
		;

string		: tCHAR
		| tSTRING
		| tSTRING_BEG tSTRING
		    {
		      $$ = $2;
		    }
		| tSTRING_BEG string_rep tSTRING
		    {
		      $$ = new_dstr(p, push($2, $3));
		    }
		;

string_rep      : string_interp
		| string_rep string_interp
		    {
		      $$ = append($1, $2);
		    }
		;

string_interp	: tSTRING_MID
		    {
		      $$ = list1($1);
		    }
		| tSTRING_PART
		    {
		      $<nd>$ = p->lex_strterm;
		      p->lex_strterm = NULL;
		    }
		  compstmt
		  '}'
		    {
		      p->lex_strterm = $<nd>2;
		      $$ = list2($1, $3);
		    }
		| tLITERAL_DELIM
		    {
		      $$ = list1(new_literal_delim(p));
		    }
		| tHD_LITERAL_DELIM heredoc_bodies
		    {
		      $$ = list1(new_literal_delim(p));
		    }
		;

xstring		: tXSTRING_BEG tXSTRING
		    {
			$$ = $2;
		    }
		| tXSTRING_BEG string_rep tXSTRING
		    {
		      $$ = new_dxstr(p, push($2, $3));
		    }
		;

regexp		: tREGEXP_BEG tREGEXP
		    {
			$$ = $2;
		    }
		| tREGEXP_BEG string_rep tREGEXP
		    {
		      $$ = new_dregx(p, $2, $3);
		    }
		;

heredoc		: tHEREDOC_BEG
		;

opt_heredoc_bodies : /* none */
		   | heredoc_bodies
		   ;

heredoc_bodies	: heredoc_body
		| heredoc_bodies heredoc_body
		;

heredoc_body	: tHEREDOC_END
		    {
		      parser_heredoc_info * inf = parsing_heredoc_inf(p);
		      inf->doc = push(inf->doc, new_str(p, "", 0));
		      heredoc_end(p);
		    }
		| heredoc_string_rep tHEREDOC_END
		    {
		      heredoc_end(p);
		    }
		;

heredoc_string_rep : heredoc_string_interp
		   | heredoc_string_rep heredoc_string_interp
		   ;

heredoc_string_interp : tHD_STRING_MID
		    {
		      parser_heredoc_info * inf = parsing_heredoc_inf(p);
		      inf->doc = push(inf->doc, $1);
		      heredoc_treat_nextline(p);
		    }
		| tHD_STRING_PART
		    {
		      $<nd>$ = p->lex_strterm;
		      p->lex_strterm = NULL;
		    }
		  compstmt
		  '}'
		    {
		      parser_heredoc_info * inf = parsing_heredoc_inf(p);
		      p->lex_strterm = $<nd>2;
		      inf->doc = push(push(inf->doc, $1), $3);
		    }
		;

words		: tWORDS_BEG tSTRING
		    {
		      $$ = new_words(p, list1($2));
		    }
		| tWORDS_BEG string_rep tSTRING
		    {
		      $$ = new_words(p, push($2, $3));
		    }
		;


symbol		: basic_symbol
		    {
		      $$ = new_sym(p, $1);
		    }
		| tSYMBEG tSTRING_BEG string_interp tSTRING
		    {
		      p->lstate = EXPR_END;
		      $$ = new_dsym(p, push($3, $4));
		    }
		;

basic_symbol	: tSYMBEG sym
		    {
		      p->lstate = EXPR_END;
		      $$ = $2;
		    }
		;

sym		: fname
		| tIVAR
		| tGVAR
		| tCVAR
		| tSTRING
		    {
		      $$ = new_strsym(p, $1);
		    }
		| tSTRING_BEG tSTRING
		    {
		      $$ = new_strsym(p, $2);
		    }
		;

symbols		: tSYMBOLS_BEG tSTRING
		    {
		      $$ = new_symbols(p, list1($2));
		    }
		| tSYMBOLS_BEG string_rep tSTRING
		    {
		      $$ = new_symbols(p, push($2, $3));
		    }
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

		      snprintf(buf, sizeof(buf), "%d", p->lineno);
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
		      $$ = cons(nsym($1), $3);
		    }
		;

f_block_opt	: tIDENTIFIER '=' primary_value
		    {
		      local_add_f(p, $1);
		      $$ = cons(nsym($1), $3);
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
		      local_add_f(p, 0);
		      $$ = -1;
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
			switch ((enum node_type)(int)(intptr_t)$3->car) {
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
		| nl
		;

rparen		: opt_nl ')'
		;

rbracket	: opt_nl ']'
		;

trailer		: /* none */
		| nl
		| ','
		;

term		: ';' {yyerrok;}
		| nl
		;

nl		: '\n'
		    {
		      p->lineno++;
		      p->column = 0;
		    }
		  opt_heredoc_bodies

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
  int n;

  if (! p->capture_errors) {
#ifdef ENABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
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
void
mrb_yyerror(parser_state *p, const char *s) {
  yyerror(p, s);
}

static void
yyerror_i(parser_state *p, const char *fmt, int i)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, i);
  yyerror(p, buf);
}
void
mrb_yyerror_i(parser_state *p, const char *fmt, int i) {
  yyerror_i(p, fmt, i);
}

static void
yywarn(parser_state *p, const char *s)
{
  char* c;
  int n;

  if (! p->capture_errors) {
#ifdef ENABLE_STDIO
    if (p->filename) {
      fprintf(stderr, "%s:%d:%d: %s\n", p->filename, p->lineno, p->column, s);
    }
    else {
      fprintf(stderr, "line %d:%d: %s\n", p->lineno, p->column, s);
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
void
mrb_yywarning(parser_state *p, const char *s) {
  yywarning(p, s);
}

void
mrb_yywarning_s(parser_state *p, const char *fmt, const char *s)
{
  char buf[256];

  snprintf(buf, sizeof(buf), fmt, s);
  yywarning(p, buf);
}

static void
backref_error(parser_state *p, node *n)
{
  int c;

  c = (int)(intptr_t)n->car;

  if (c == NODE_NTH_REF) {
    yyerror_i(p, "can't set variable $%d", (int)(intptr_t)n->cdr);
  } else if (c == NODE_BACK_REF) {
    yyerror_i(p, "can't set variable $%c", (int)(intptr_t)n->cdr);
  } else {
    mrb_bug(p->mrb, "Internal error in backref_error() : n=>car == %d", c);
  }
}

static int
yylex(void *lval, parser_state *p)
{
  int t;

  p->ylval = lval;
  t = mrb_parser_yylex(p);

  return t;
}

static void
parser_init_cxt(parser_state *p, mrbc_context *cxt)
{
  if (!cxt) return;
  if (cxt->lineno) p->lineno = cxt->lineno;
  if (cxt->filename) mrb_parser_set_filename(p, cxt->filename);
  if (cxt->syms) {
    int i;

    p->locals = cons(0,0);
    for (i=0; i<cxt->slen; i++) {
      local_add_f(p, cxt->syms[i]);
    }
  }
  p->capture_errors = cxt->capture_errors;
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
  if ((int)(intptr_t)p->tree->car != NODE_SCOPE) return;
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

void codedump_all(mrb_state*, int);
void parser_dump(mrb_state *mrb, node *tree, int offset);

void
mrb_parser_parse(parser_state *p, mrbc_context *c)
{
  if (setjmp(p->jmp) != 0) {
    yyerror(p, "memory allocation error");
    p->nerr++;
    p->tree = 0;
    return;
  }

  p->cmd_start = TRUE;
  p->in_def = p->in_single = FALSE;
  p->nerr = p->nwarn = 0;
  p->lex_strterm = NULL;

  parser_init_cxt(p, c);
  yyparse(p);
  if (!p->tree) {
    p->tree = new_nil(p);
  }
  parser_update_cxt(p, c);
  if (c && c->dump_result) {
    parser_dump(p->mrb, p->tree, 0);
  }
}

parser_state*
mrb_parser_new(mrb_state *mrb)
{
  mrb_pool *pool;
  parser_state *p;
  static const parser_state parser_state_zero = { 0 };

  pool = mrb_pool_open(mrb);
  if (!pool) return 0;
  p = (parser_state *)mrb_pool_alloc(pool, sizeof(parser_state));
  if (!p) return 0;

  *p = parser_state_zero;
  p->mrb = mrb;
  p->pool = pool;
  p->in_def = p->in_single = 0;

  p->s = p->send = NULL;
#ifdef ENABLE_STDIO
  p->f = NULL;
#endif

  p->cmd_start = TRUE;
  p->in_def = p->in_single = FALSE;

  p->capture_errors = 0;
  p->lineno = 1;
  p->column = 0;
#if defined(PARSER_TEST) || defined(PARSER_DEBUG)
  yydebug = 1;
#endif

  p->lex_strterm = NULL;
  p->all_heredocs = p->parsing_heredoc = NULL;
  p->lex_strterm_before_heredoc = NULL;

  p->current_filename_index = -1;
  p->filename_table = NULL;
  p->filename_table_length = 0;

  return p;
}

void
mrb_parser_free(parser_state *p) {
  mrb_pool_close(p->pool);
}

mrbc_context*
mrbc_context_new(mrb_state *mrb)
{
  mrbc_context *c;

  c = (mrbc_context *)mrb_calloc(mrb, 1, sizeof(mrbc_context));
  return c;
}

void
mrbc_context_free(mrb_state *mrb, mrbc_context *cxt)
{
  mrb_free(mrb, cxt->syms);
  mrb_free(mrb, cxt);
}

const char*
mrbc_filename(mrb_state *mrb, mrbc_context *c, const char *s)
{
  if (s) {
    int len = strlen(s);
    char *p = (char *)mrb_alloca(mrb, len + 1);

    memcpy(p, s, len + 1);
    c->filename = p;
  }
  return c->filename;
}

void
mrbc_partial_hook(mrb_state *mrb, mrbc_context *c, int (*func)(struct mrb_parser_state*), void *data)
{
  c->partial_hook = func;
  c->partial_data = data;
}

void
mrb_parser_set_filename(struct mrb_parser_state *p, const char *f)
{
  mrb_sym sym;
  size_t len;
  size_t i;
  mrb_sym* new_table;

  sym = mrb_intern_cstr(p->mrb, f);
  p->filename = mrb_sym2name_len(p->mrb, sym, &len);
  p->lineno = (p->filename_table_length > 0)? 0 : 1;
  
  for(i = 0; i < p->filename_table_length; ++i) {
    if(p->filename_table[i] == sym) {
      p->current_filename_index = i;
      return;
    }
  }

  p->current_filename_index = p->filename_table_length++;

  new_table = parser_palloc(p, sizeof(mrb_sym) * p->filename_table_length);
  if (p->filename_table) {
    memcpy(new_table, p->filename_table, sizeof(mrb_sym) * p->filename_table_length);
  }
  p->filename_table = new_table;
  p->filename_table[p->filename_table_length - 1] = sym;
}

char const* mrb_parser_get_filename(struct mrb_parser_state* p, uint16_t idx) {
  if (idx >= p->filename_table_length) { return NULL; }
  else {
    size_t len;
    return mrb_sym2name_len(p->mrb, p->filename_table[idx], &len);
  }
}

#ifdef ENABLE_STDIO
parser_state*
mrb_parse_file(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return 0;
  p->s = p->send = NULL;
  p->f = f;

  mrb_parser_parse(p, c);
  return p;
}
#endif

parser_state*
mrb_parse_nstring(mrb_state *mrb, const char *s, int len, mrbc_context *c)
{
  parser_state *p;

  p = mrb_parser_new(mrb);
  if (!p) return 0;
  p->s = s;
  p->send = s + len;

  mrb_parser_parse(p, c);
  return p;
}

parser_state*
mrb_parse_string(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_parse_nstring(mrb, s, strlen(s), c);
}

static mrb_value
load_exec(mrb_state *mrb, parser_state *p, mrbc_context *c)
{
  struct RClass *target = mrb->object_class;
  struct RProc *proc;
  int n;
  mrb_value v;

  if (!p) {
    return mrb_undef_value();
  }
  if (!p->tree || p->nerr) {
    if (p->capture_errors) {
      char buf[256];

      n = snprintf(buf, sizeof(buf), "line %d: %s\n",
      p->error_buffer[0].lineno, p->error_buffer[0].message);
      mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SYNTAX_ERROR, buf, n));
      mrb_parser_free(p);
      return mrb_undef_value();
    }
    else {
      static const char msg[] = "syntax error";
      mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SYNTAX_ERROR, msg, sizeof(msg) - 1));
      mrb_parser_free(p);
      return mrb_undef_value();
    }
  }
  n = mrb_generate_code(mrb, p);
  mrb_parser_free(p);
  if (n < 0) {
    static const char msg[] = "codegen error";
    mrb->exc = mrb_obj_ptr(mrb_exc_new(mrb, E_SCRIPT_ERROR, msg, sizeof(msg) - 1));
    return mrb_nil_value();
  }
  if (c) {
    if (c->dump_result) codedump_all(mrb, n);
    if (c->no_exec) return mrb_fixnum_value(n);
    if (c->target_class) {
      target = c->target_class;
    }
  }
  proc = mrb_proc_new(mrb, mrb->irep[n]);
  proc->target_class = target;
  if (mrb->c->ci) {
    mrb->c->ci->target_class = target;
  }
  v = mrb_run(mrb, proc, mrb_top_self(mrb));
  if (mrb->exc) return mrb_nil_value();
  return v;
}

#ifdef ENABLE_STDIO
mrb_value
mrb_load_file_cxt(mrb_state *mrb, FILE *f, mrbc_context *c)
{
  return load_exec(mrb, mrb_parse_file(mrb, f, c), c);
}

mrb_value
mrb_load_file(mrb_state *mrb, FILE *f)
{
  return mrb_load_file_cxt(mrb, f, NULL);
}
#endif

mrb_value
mrb_load_nstring_cxt(mrb_state *mrb, const char *s, int len, mrbc_context *c)
{
  return load_exec(mrb, mrb_parse_nstring(mrb, s, len, c), c);
}

mrb_value
mrb_load_nstring(mrb_state *mrb, const char *s, int len)
{
  return mrb_load_nstring_cxt(mrb, s, len, NULL);
}

mrb_value
mrb_load_string_cxt(mrb_state *mrb, const char *s, mrbc_context *c)
{
  return mrb_load_nstring_cxt(mrb, s, strlen(s), c);
}

mrb_value
mrb_load_string(mrb_state *mrb, const char *s)
{
  return mrb_load_string_cxt(mrb, s, NULL);
}

#ifdef ENABLE_STDIO

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

#endif

void
parser_dump(mrb_state *mrb, node *tree, int offset)
{
#ifdef ENABLE_STDIO
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
    parser_dump(mrb, tree->cdr->cdr, offset+2);
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
	    printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
	    parser_dump(mrb, n2->car->cdr, 0);
	    n2 = n2->cdr;
	  }
	}
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
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
	printf("blk=&%s\n", mrb_sym2name(mrb, sym(n)));
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
    {
      node *n2 = tree->car;

      if (n2  && (n2->car || n2->cdr)) {
	dump_prefix(offset+1);
	printf("local variables:\n");
	dump_prefix(offset+2);
	while (n2) {
	  if (n2->car) {
	    if (n2 != tree->car) printf(", ");
	    printf("%s", mrb_sym2name(mrb, sym(n2->car)));
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
    printf("NODE_CALL:\n");
    parser_dump(mrb, tree->car, offset+1);
    dump_prefix(offset+1);
    printf("method='%s' (%d)\n",
    mrb_sym2name(mrb, sym(tree->cdr->car)),
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
    printf("::%s\n", mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_COLON3:
    printf("NODE_COLON3:\n");
    dump_prefix(offset+1);
    printf("::%s\n", mrb_sym2name(mrb, sym(tree)));
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
	  if (n2->car == (node*)-1) {
	    dump_prefix(offset+2);
	    printf("(empty)\n");
	  }
	  else {
	    parser_dump(mrb, n2->car, offset+3);
	  }
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
    printf("op='%s' (%d)\n", mrb_sym2name(mrb, sym(tree->car)), (int)(intptr_t)tree->car);
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
    printf("NODE_LVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_GVAR:
    printf("NODE_GVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_IVAR:
    printf("NODE_IVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CVAR:
    printf("NODE_CVAR %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_CONST:
    printf("NODE_CONST %s\n", mrb_sym2name(mrb, sym(tree)));
    break;

  case NODE_MATCH:
    printf("NODE_MATCH:\n");
    dump_prefix(offset + 1);
    printf("lhs:\n");
    parser_dump(mrb, tree->car, offset + 2);
    dump_prefix(offset + 1);
    printf("rhs:\n");
    parser_dump(mrb, tree->cdr, offset + 2);
    break;

  case NODE_BACK_REF:
    printf("NODE_BACK_REF: $%c\n", (int)(intptr_t)tree);
    break;

  case NODE_NTH_REF:
    printf("NODE_NTH_REF: $%d\n", (int)(intptr_t)tree);
    break;

  case NODE_ARG:
    printf("NODE_ARG %s\n", mrb_sym2name(mrb, sym(tree)));
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

  case NODE_XSTR:
    printf("NODE_XSTR \"%s\" len %d\n", (char*)tree->car, (int)(intptr_t)tree->cdr);
    break;

  case NODE_DXSTR:
    printf("NODE_DXSTR\n");
    dump_recur(mrb, tree, offset+1);
    break;

  case NODE_REGX:
    printf("NODE_REGX /%s/%s\n", (char*)tree->car, (char*)tree->cdr);
    break;

  case NODE_DREGX:
    printf("NODE_DREGX\n");
    dump_recur(mrb, tree->car, offset+1);
    dump_prefix(offset);
    printf("tail: %s\n", (char*)tree->cdr->cdr->car);
    dump_prefix(offset);
    printf("opt: %s\n", (char*)tree->cdr->cdr->cdr);
    break;

  case NODE_SYM:
    printf("NODE_SYM :%s\n", mrb_sym2name(mrb, sym(tree)));
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
	    mrb_sym2name(mrb, sym(tree->car)),
	    mrb_sym2name(mrb, sym(tree->cdr)));
    break;

  case NODE_UNDEF:
    printf("NODE_UNDEF");
    {
      node *t = tree;
      while (t) {
	printf(" %s", mrb_sym2name(mrb, sym(t->car)));
	t = t->cdr;
      }
    }
    printf(":\n");
    break;

  case NODE_CLASS:
    printf("NODE_CLASS:\n");
    if (tree->car->car == (node*)0) {
      dump_prefix(offset+1);
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
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
      printf(":%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else if (tree->car->car == (node*)1) {
      dump_prefix(offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
    }
    else {
      parser_dump(mrb, tree->car->car, offset+1);
      dump_prefix(offset+1);
      printf("::%s\n", mrb_sym2name(mrb, sym(tree->car->cdr)));
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
    printf("%s\n", mrb_sym2name(mrb, sym(tree->car)));
    tree = tree->cdr;
    {
      node *n2 = tree->car;

      if (n2 && (n2->car || n2->cdr)) {
	dump_prefix(offset+1);
	printf("local variables:\n");
	dump_prefix(offset+2);
	while (n2) {
	  if (n2->car) {
	    if (n2 != tree->car) printf(", ");
	    printf("%s", mrb_sym2name(mrb, sym(n2->car)));
	  }
	  n2 = n2->cdr;
	}
	printf("\n");
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
	    printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
	    parser_dump(mrb, n2->car->cdr, 0);
	    n2 = n2->cdr;
	  }
	}
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
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
	printf("blk=&%s\n", mrb_sym2name(mrb, sym(n)));
      }
    }
    parser_dump(mrb, tree->cdr->car, offset+1);
    break;

  case NODE_SDEF:
    printf("NODE_SDEF:\n");
    parser_dump(mrb, tree->car, offset+1);
    tree = tree->cdr;
    dump_prefix(offset+1);
    printf(":%s\n", mrb_sym2name(mrb, sym(tree->car)));
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
	    printf("%s=", mrb_sym2name(mrb, sym(n2->car->car)));
	    parser_dump(mrb, n2->car->cdr, 0);
	    n2 = n2->cdr;
	  }
	}
      }
      n = n->cdr;
      if (n->car) {
	dump_prefix(offset+1);
	printf("rest=*%s\n", mrb_sym2name(mrb, sym(n->car)));
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
	printf("blk=&%s\n", mrb_sym2name(mrb, sym(n)));
      }
    }
    tree = tree->cdr;
    parser_dump(mrb, tree->car, offset+1);
    break;

  case NODE_POSTEXE:
    printf("NODE_POSTEXE:\n");
    parser_dump(mrb, tree, offset+1);
    break;

  case NODE_HEREDOC:
    printf("NODE_HEREDOC:\n");
    parser_dump(mrb, ((parser_heredoc_info*)tree)->doc, offset+1);
    break;

  default:
    printf("node type: %d (0x%x)\n", (int)n, (int)n);
    break;
  }
#endif
}
