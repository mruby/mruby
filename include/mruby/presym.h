/**
** @file mruby/presym.h - Preallocated Symbols
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_PRESYM_H
#define MRUBY_PRESYM_H

/*
 * Where `mrb_intern_lit` is allowed for symbol interning, it is directly
 * replaced by the symbol ID if presym is enabled by using the following
 * macros.
 *
 *   MRB_OPSYM(xor)  //=> ^      (Operator)
 *   MRB_CVSYM(xor)  //=> @@xor  (Class Variable)
 *   MRB_IVSYM(xor)  //=> @xor   (Instance Variable)
 *   MRB_SYM_B(xor)  //=> xor!   (Method with Bang)
 *   MRB_SYM_Q(xor)  //=> xor?   (Method with Question mark)
 *   MRB_SYM_E(xor)  //=> xor=   (Method with Equal)
 *   MRB_SYM(xor)    //=> xor    (Word characters)
 *
 * For `MRB_OPSYM`, specify the names corresponding to operators (see macros
 * starting with `MRB_OPSYM__` below for the names that can be specified for
 * it). Other than that, describe only word characters excluding leading and
 * ending punctuations.
 *
 * These macros are expanded to `mrb_intern_lit` if presym is disabled,
 * therefore the mruby state variable is required. The above macros can be
 * used when the variable name is `mrb`. If you want to use other variable
 * names, you need to use macros with `_2` suffix, such as `MRB_SYM_2`.
 */

#ifdef MRB_NO_PRESYM

#include <string.h>

# define MRB_PRESYM_MAX 0

# define MRB_OPSYM(name) MRB_OPSYM__##name(mrb)
# define MRB_CVSYM(name) mrb_intern_lit(mrb, "@@" #name)
# define MRB_IVSYM(name) mrb_intern_lit(mrb, "@" #name)
# define MRB_SYM_B(name) mrb_intern_lit(mrb, #name "!")
# define MRB_SYM_Q(name) mrb_intern_lit(mrb, #name "?")
# define MRB_SYM_E(name) mrb_intern_lit(mrb, #name "=")
# define MRB_SYM(name) mrb_intern_lit(mrb, #name)

# define MRB_OPSYM_2(mrb, name) MRB_OPSYM__##name(mrb)
# define MRB_CVSYM_2(mrb, name) mrb_intern_lit(mrb, "@@" #name)
# define MRB_IVSYM_2(mrb, name) mrb_intern_lit(mrb, "@" #name)
# define MRB_SYM_B_2(mrb, name) mrb_intern_lit(mrb, #name "!")
# define MRB_SYM_Q_2(mrb, name) mrb_intern_lit(mrb, #name "?")
# define MRB_SYM_E_2(mrb, name) mrb_intern_lit(mrb, #name "=")
# define MRB_SYM_2(mrb, name) mrb_intern_lit(mrb, #name)

# define MRB_OPSYM__not(mrb) mrb_intern_lit(mrb, "!")
# define MRB_OPSYM__neq(mrb) mrb_intern_lit(mrb, "!=")
# define MRB_OPSYM__nmatch(mrb) mrb_intern_lit(mrb, "!~")
# define MRB_OPSYM__mod(mrb) mrb_intern_lit(mrb, "%")
# define MRB_OPSYM__and(mrb) mrb_intern_lit(mrb, "&")
# define MRB_OPSYM__andand(mrb) mrb_intern_lit(mrb, "&&")
# define MRB_OPSYM__mul(mrb) mrb_intern_lit(mrb, "*")
# define MRB_OPSYM__pow(mrb) mrb_intern_lit(mrb, "**")
# define MRB_OPSYM__add(mrb) mrb_intern_lit(mrb, "+")
# define MRB_OPSYM__plus(mrb) mrb_intern_lit(mrb, "+@")
# define MRB_OPSYM__sub(mrb) mrb_intern_lit(mrb, "-")
# define MRB_OPSYM__minus(mrb) mrb_intern_lit(mrb, "-@")
# define MRB_OPSYM__div(mrb) mrb_intern_lit(mrb, "/")
# define MRB_OPSYM__lt(mrb) mrb_intern_lit(mrb, "<")
# define MRB_OPSYM__le(mrb) mrb_intern_lit(mrb, "<=")
# define MRB_OPSYM__lshift(mrb) mrb_intern_lit(mrb, "<<")
# define MRB_OPSYM__cmp(mrb) mrb_intern_lit(mrb, "<=>")
# define MRB_OPSYM__eq(mrb) mrb_intern_lit(mrb, "==")
# define MRB_OPSYM__eqq(mrb) mrb_intern_lit(mrb, "===")
# define MRB_OPSYM__match(mrb) mrb_intern_lit(mrb, "=~")
# define MRB_OPSYM__gt(mrb) mrb_intern_lit(mrb, ">")
# define MRB_OPSYM__ge(mrb) mrb_intern_lit(mrb, ">=")
# define MRB_OPSYM__rshift(mrb) mrb_intern_lit(mrb, ">>")
# define MRB_OPSYM__aref(mrb) mrb_intern_lit(mrb, "[]")
# define MRB_OPSYM__aset(mrb) mrb_intern_lit(mrb, "[]=")
# define MRB_OPSYM__xor(mrb) mrb_intern_lit(mrb, "^")
# define MRB_OPSYM__tick(mrb) mrb_intern_lit(mrb, "`")
# define MRB_OPSYM__or(mrb) mrb_intern_lit(mrb, "|")
# define MRB_OPSYM__oror(mrb) mrb_intern_lit(mrb, "||")
# define MRB_OPSYM__neg(mrb) mrb_intern_lit(mrb, "~")

# define MRB_PRESYM_DEFINE_VAR_AND_INITER(name, size, ...)                    \
  static mrb_sym name[size];                                                  \
  static void init_##name(mrb_state *mrb) {                                   \
    mrb_sym name__[] = {__VA_ARGS__};                                         \
    memcpy(name, name__, sizeof(name));                                       \
  }

#else  /* not MRB_NO_PRESYM */

# undef MRB_PRESYM_MAX
# ifdef MRB_USE_ALL_SYMBOLS
#  define MRB_PRESYM_NAMED(lit, num, type, name) MRB_##type##__##name = (num),
# else
#  define MRB_PRESYM_NAMED(lit, num, type, name) MRB_##type##__##name = (num<<1),
# endif
# define MRB_PRESYM_UNNAMED(lit, num)

enum mruby_presym {
# include <mruby/presym.inc>
};

# undef MRB_PRESYM_NAMED
# undef MRB_PRESYM_UNNAMED

# define MRB_OPSYM(name) MRB_OPSYM__##name
# define MRB_CVSYM(name) MRB_CVSYM__##name
# define MRB_IVSYM(name) MRB_IVSYM__##name
# define MRB_SYM_B(name) MRB_SYM_B__##name
# define MRB_SYM_Q(name) MRB_SYM_Q__##name
# define MRB_SYM_E(name) MRB_SYM_E__##name
# define MRB_SYM(name)   MRB_SYM__##name

# define MRB_OPSYM_2(mrb, name) MRB_OPSYM__##name
# define MRB_CVSYM_2(mrb, name) MRB_CVSYM__##name
# define MRB_IVSYM_2(mrb, name) MRB_IVSYM__##name
# define MRB_SYM_B_2(mrb, name) MRB_SYM_B__##name
# define MRB_SYM_Q_2(mrb, name) MRB_SYM_Q__##name
# define MRB_SYM_E_2(mrb, name) MRB_SYM_E__##name
# define MRB_SYM_2(mrb, name)   MRB_SYM__##name

# define MRB_PRESYM_DEFINE_VAR_AND_INITER(name, size, ...)                    \
  static const mrb_sym name[] = {__VA_ARGS__};                                \
  static void init_##name(mrb_state *mrb) {}

#endif  /* MRB_NO_PRESYM */

#endif  /* MRUBY_PRESYM_H */
