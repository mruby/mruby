/*
** node.h - nodes of abstract syntax tree
**
** See Copyright Notice in mruby.h
*/

#ifndef NODE_H
#define NODE_H

typedef mrb_ast_node node;
typedef struct mrb_parser_state parser_state;
typedef struct mrb_parser_heredoc_info parser_heredoc_info;

// To avoid global namespace pollution, rename all functions.
#define NODE_PREFIX  "mrb_"
#define cons_free_gen NODE_PREFIX##cons_free_gen
#define parser_palloc NODE_PREFIX##parser_palloc
#define cons_gen NODE_PREFIX##cons_gen
#define list1_gen NODE_PREFIX##list1_gen
#define list2_gen NODE_PREFIX##list2_gen
#define list3_gen NODE_PREFIX##list3_gen
#define list4_gen NODE_PREFIX##list4_gen
#define list5_gen NODE_PREFIX##list5_gen
#define list6_gen NODE_PREFIX##list6_gen
#define append_gen NODE_PREFIX##append_gen
#define local_switch NODE_PREFIX##local_switch
#define local_resume NODE_PREFIX##local_resume
#define local_nest NODE_PREFIX##local_nest
#define local_unnest NODE_PREFIX##local_unnest
#define local_var_p NODE_PREFIX##local_var_p
#define local_add_f NODE_PREFIX##local_add_f
#define local_add NODE_PREFIX##local_add
#define new_scope NODE_PREFIX##new_scope
#define new_begin NODE_PREFIX##new_begin
#define new_rescue NODE_PREFIX##new_rescue
#define new_ensure NODE_PREFIX##new_ensure
#define new_nil NODE_PREFIX##new_nil
#define new_true NODE_PREFIX##new_true
#define new_false NODE_PREFIX##new_false
#define new_alias NODE_PREFIX##new_alias
#define new_if NODE_PREFIX##new_if
#define new_unless NODE_PREFIX##new_unless
#define new_while NODE_PREFIX##new_while
#define new_until NODE_PREFIX##new_until
#define new_for NODE_PREFIX##new_for
#define new_case NODE_PREFIX##new_case
#define new_postexe NODE_PREFIX##new_postexe
#define new_self NODE_PREFIX##new_self
#define new_call NODE_PREFIX##new_call
#define new_fcall NODE_PREFIX##new_fcall
#define new_super NODE_PREFIX##new_super
#define new_zsuper NODE_PREFIX##new_zsuper
#define new_yield NODE_PREFIX##new_yield
#define new_return NODE_PREFIX##new_return
#define new_break NODE_PREFIX##new_break
#define new_next NODE_PREFIX##new_next
#define new_redo NODE_PREFIX##new_redo
#define new_retry NODE_PREFIX##new_retry
#define new_dot2 NODE_PREFIX##new_dot2
#define new_dot3 NODE_PREFIX##new_dot3
#define new_colon2 NODE_PREFIX##new_colon2
#define new_colon3 NODE_PREFIX##new_colon3
#define new_and NODE_PREFIX##new_and
#define new_or NODE_PREFIX##new_or
#define new_array NODE_PREFIX##new_array
#define new_splat NODE_PREFIX##new_splat
#define new_hash NODE_PREFIX##new_hash
#define new_sym NODE_PREFIX##new_sym
#define new_strsym NODE_PREFIX##new_strsym
#define new_lvar NODE_PREFIX##new_lvar
#define new_gvar NODE_PREFIX##new_gvar
#define new_ivar NODE_PREFIX##new_ivar
#define new_cvar NODE_PREFIX##new_cvar
#define new_const NODE_PREFIX##new_const
#define new_undef NODE_PREFIX##new_undef
#define new_class NODE_PREFIX##new_class
#define new_sclass NODE_PREFIX##new_sclass
#define new_module NODE_PREFIX##new_module
#define new_def NODE_PREFIX##new_def
#define new_sdef NODE_PREFIX##new_sdef
#define new_arg NODE_PREFIX##new_arg
#define new_args NODE_PREFIX##new_args
#define new_block_arg NODE_PREFIX##new_block_arg
#define new_block NODE_PREFIX##new_block
#define new_lambda NODE_PREFIX##new_lambda
#define new_asgn NODE_PREFIX##new_asgn
#define new_masgn NODE_PREFIX##new_masgn
#define new_op_asgn NODE_PREFIX##new_op_asgn
#define new_int NODE_PREFIX##new_int
#define new_float NODE_PREFIX##new_float
#define new_str NODE_PREFIX##new_str
#define new_dstr NODE_PREFIX##new_dstr
#define new_xstr NODE_PREFIX##new_xstr
#define new_dxstr NODE_PREFIX##new_dxstr
#define new_dsym NODE_PREFIX##new_dsym
#define new_regx NODE_PREFIX##new_regx
#define new_dregx NODE_PREFIX##new_dregx
#define new_back_ref NODE_PREFIX##new_back_ref
#define new_nth_ref NODE_PREFIX##new_nth_ref
#define new_heredoc NODE_PREFIX##new_heredoc
#define new_bv NODE_PREFIX##new_bv
#define new_literal_delim NODE_PREFIX##new_literal_delim
#define new_words NODE_PREFIX##new_words
#define new_symbols NODE_PREFIX##new_symbols
#define call_uni_op NODE_PREFIX##call_uni_op
#define call_bin_op NODE_PREFIX##call_bin_op
#define args_with_block NODE_PREFIX##args_with_block
#define call_with_block NODE_PREFIX##call_with_block
#define negate_lit NODE_PREFIX##negate_lit
#define cond NODE_PREFIX##cond
#define ret_args NODE_PREFIX##ret_args
#define assignable NODE_PREFIX##assignable
#define var_reference NODE_PREFIX##var_reference
#define new_strterm NODE_PREFIX##new_strterm
#define end_strterm NODE_PREFIX##end_strterm
#define parsing_heredoc_inf NODE_PREFIX##parsing_heredoc_inf
#define heredoc_treat_nextline NODE_PREFIX##heredoc_treat_nextline
#define heredoc_end NODE_PREFIX##heredoc_end

void cons_free_gen(parser_state *p, node *cons);
#define cons_free(c) cons_free_gen(p, (c))
void* parser_palloc(parser_state *p, size_t size);
node* cons_gen(parser_state *p, node *car, node *cdr);
#define cons(a,b) cons_gen(p,(a),(b))
node* list1_gen(parser_state *p, node *a);
#define list1(a) list1_gen(p, (a))
node* list2_gen(parser_state *p, node *a, node *b);
#define list2(a,b) list2_gen(p, (a),(b))
node* list3_gen(parser_state *p, node *a, node *b, node *c);
#define list3(a,b,c) list3_gen(p, (a),(b),(c))
node* list4_gen(parser_state *p, node *a, node *b, node *c, node *d);
#define list4(a,b,c,d) list4_gen(p, (a),(b),(c),(d))
node* list5_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e);
#define list5(a,b,c,d,e) list5_gen(p, (a),(b),(c),(d),(e))
node* list6_gen(parser_state *p, node *a, node *b, node *c, node *d, node *e, node *f);
#define list6(a,b,c,d,e,f) list6_gen(p, (a),(b),(c),(d),(e),(f))
node* append_gen(parser_state *p, node *a, node *b);
#define append(a,b) append_gen(p,(a),(b))
#define push(a,b) append_gen(p,(a),list1(b))

node* local_switch(parser_state *p);
void local_resume(parser_state *p, node *prev);
void local_nest(parser_state *p);
void local_unnest(parser_state *p);
int local_var_p(parser_state *p, mrb_sym sym);
void local_add_f(parser_state *p, mrb_sym sym);
void local_add(parser_state *p, mrb_sym sym);
node* new_scope(parser_state *p, node *body);
node* new_begin(parser_state *p, node *body);
#define newline_node(n) (n)
node* new_rescue(parser_state *p, node *body, node *resq, node *els);
node* new_ensure(parser_state *p, node *a, node *b);
node* new_nil(parser_state *p);
node* new_true(parser_state *p);
node* new_false(parser_state *p);
node* new_alias(parser_state *p, mrb_sym a, mrb_sym b);
node* new_if(parser_state *p, node *a, node *b, node *c);
node* new_unless(parser_state *p, node *a, node *b, node *c);
node* new_while(parser_state *p, node *a, node *b);
node* new_until(parser_state *p, node *a, node *b);
node* new_for(parser_state *p, node *v, node *o, node *b);
node* new_case(parser_state *p, node *a, node *b);
node* new_postexe(parser_state *p, node *a);
node* new_self(parser_state *p);
node* new_call(parser_state *p, node *a, mrb_sym b, node *c);
node* new_fcall(parser_state *p, mrb_sym b, node *c);
node* new_super(parser_state *p, node *c);
node* new_zsuper(parser_state *p);
node* new_yield(parser_state *p, node *c);
node* new_return(parser_state *p, node *c);
node* new_break(parser_state *p, node *c);
node* new_next(parser_state *p, node *c);
node* new_redo(parser_state *p);
node* new_retry(parser_state *p);
node* new_dot2(parser_state *p, node *a, node *b);
node* new_dot3(parser_state *p, node *a, node *b);
node* new_colon2(parser_state *p, node *b, mrb_sym c);
node* new_colon3(parser_state *p, mrb_sym c);
node* new_and(parser_state *p, node *a, node *b);
node* new_or(parser_state *p, node *a, node *b);
node* new_array(parser_state *p, node *a);
node* new_splat(parser_state *p, node *a);
node* new_hash(parser_state *p, node *a);
node* new_sym(parser_state *p, mrb_sym sym);
mrb_sym new_strsym(parser_state *p, node* str);
node* new_lvar(parser_state *p, mrb_sym sym);
node* new_gvar(parser_state *p, mrb_sym sym);
node* new_ivar(parser_state *p, mrb_sym sym);
node* new_cvar(parser_state *p, mrb_sym sym);
node* new_const(parser_state *p, mrb_sym sym);
node* new_undef(parser_state *p, mrb_sym sym);
node* new_class(parser_state *p, node *c, node *s, node *b);
node* new_sclass(parser_state *p, node *o, node *b);
node* new_module(parser_state *p, node *m, node *b);
node* new_def(parser_state *p, mrb_sym m, node *a, node *b);
node* new_sdef(parser_state *p, node *o, mrb_sym m, node *a, node *b);
node* new_arg(parser_state *p, mrb_sym sym);
node* new_args(parser_state *p, node *m, node *opt, mrb_sym rest, node *m2, mrb_sym blk);
node* new_block_arg(parser_state *p, node *a);
node* new_block(parser_state *p, node *a, node *b);
node* new_lambda(parser_state *p, node *a, node *b);
node* new_asgn(parser_state *p, node *a, node *b);
node* new_masgn(parser_state *p, node *a, node *b);
node* new_op_asgn(parser_state *p, node *a, mrb_sym op, node *b);
node* new_int(parser_state *p, const char *s, int base);
node* new_float(parser_state *p, const char *s);
node* new_str(parser_state *p, const char *s, int len);
node* new_dstr(parser_state *p, node *a);
node* new_xstr(parser_state *p, const char *s, int len);
node* new_dxstr(parser_state *p, node *a);
node* new_dsym(parser_state *p, node *a);
node* new_regx(parser_state *p, const char *p1, const char* p2);
node* new_dregx(parser_state *p, node *a, node *b);
node* new_back_ref(parser_state *p, int n);
node* new_nth_ref(parser_state *p, int n);
node* new_heredoc(parser_state *p);
void new_bv(parser_state *p, mrb_sym id);
node* new_literal_delim(parser_state *p);
node* new_words(parser_state *p, node *a);
node* new_symbols(parser_state *p, node *a);

node* call_uni_op(parser_state *p, node *recv, char *m);
node* call_bin_op(parser_state *p, node *recv, char *m, node *arg1);
void args_with_block(parser_state *p, node *a, node *b);
void call_with_block(parser_state *p, node *a, node *b);
node* negate_lit(parser_state *p, node *n);
node* cond(node *n);
node* ret_args(parser_state *p, node *n);
void assignable(parser_state *p, node *lhs);
node* var_reference(parser_state *p, node *lhs);
node* new_strterm(parser_state *p, enum mrb_string_type type, int term, int paren);
void end_strterm(parser_state *p);
parser_heredoc_info * parsing_heredoc_inf(parser_state *p);
void heredoc_treat_nextline(parser_state *p);
void heredoc_end(parser_state *p);
#define is_strterm_type(p,str_func) ((int)(intptr_t)((p)->lex_strterm->car) & (str_func))

#endif  /* NODE_H */
