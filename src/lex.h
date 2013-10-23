/*
** lex.h - mruby lexer
**
** See Copyright Notice in mruby.h
*/

#ifndef LEX_H
#define LEX_H

int mrb_parser_yylex(struct mrb_parser_state *p);

//typedef unsigned int stack_type;  // Unused.

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

static inline mrb_sym
intern_gen(struct mrb_parser_state *p, const char *s)
{
  return mrb_intern_cstr(p->mrb, s);
}
#define intern(s) intern_gen(p,(s))

static inline mrb_sym
intern_gen2(struct mrb_parser_state *p, const char *s, size_t len)
{
  return mrb_intern2(p->mrb, s, len);
}
#define intern2(s,len) intern_gen2(p,(s),(len))

static inline mrb_sym
intern_gen_c(struct mrb_parser_state *p, const char c)
{
  return mrb_intern2(p->mrb, &c, 1);
}
#define intern_c(c) intern_gen_c(p,(c))

#endif  /* LEX_H */
