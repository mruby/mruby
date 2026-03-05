/**
** @file mruby/presym.h - Preallocated Symbols
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_PRESYM_H
#define MRUBY_PRESYM_H

#if !defined(MRB_PRESYM_SCANNING)

#include <mruby/presym/id.h>

/*
 * Where `mrb_intern_lit` is allowed for symbol interning, it is directly
 * replaced by the symbol ID using the following macros.
 *
 *   MRB_OPSYM(xor)  //=> ^      (Operator)
 *   MRB_GVSYM(xor)  //=> $xor   (Global Variable)
 *   MRB_CVSYM(xor)  //=> @@xor  (Class Variable)
 *   MRB_IVSYM(xor)  //=> @xor   (Instance Variable)
 *   MRB_SYM_B(xor)  //=> xor!   (Method with Bang)
 *   MRB_SYM_Q(xor)  //=> xor?   (Method with Question mark)
 *   MRB_SYM_E(xor)  //=> xor=   (Method with Equal)
 *   MRB_SYM(xor)    //=> xor    (Word characters)
 *
 * For `MRB_OPSYM`, specify the names corresponding to operators (see
 * `MRuby::Presym::OPERATORS` in `lib/mruby/presym.rb` for the names that
 * can be specified for it). Other than that, describe only word characters
 * excluding leading and ending punctuation.
 *
 * These macros are expanded to compile-time integer constants.
 */

#define MRB_OPSYM(name) MRB_OPSYM__##name
#define MRB_GVSYM(name) MRB_GVSYM__##name
#define MRB_CVSYM(name) MRB_CVSYM__##name
#define MRB_IVSYM(name) MRB_IVSYM__##name
#define MRB_SYM_B(name) MRB_SYM_B__##name
#define MRB_SYM_Q(name) MRB_SYM_Q__##name
#define MRB_SYM_E(name) MRB_SYM_E__##name
#define MRB_SYM(name) MRB_SYM__##name

/* backward compatibility: _2 variants accept but ignore mrb_state* */
#define MRB_OPSYM_2(mrb, name) MRB_OPSYM(name)
#define MRB_GVSYM_2(mrb, name) MRB_GVSYM(name)
#define MRB_CVSYM_2(mrb, name) MRB_CVSYM(name)
#define MRB_IVSYM_2(mrb, name) MRB_IVSYM(name)
#define MRB_SYM_B_2(mrb, name) MRB_SYM_B(name)
#define MRB_SYM_Q_2(mrb, name) MRB_SYM_Q(name)
#define MRB_SYM_E_2(mrb, name) MRB_SYM_E(name)
#define MRB_SYM_2(mrb, name) MRB_SYM(name)

#define MRB_PRESYM_DEFINE_VAR_AND_INITER(name, size, ...)                     \
  static const mrb_sym name[] = {__VA_ARGS__};

#define MRB_PRESYM_INIT_SYMBOLS(mrb, name) (void)(mrb)

/* use MRB_SYM() for E_RUNTIME_ERROR etc. */
#undef MRB_ERROR_SYM
#define MRB_ERROR_SYM(sym) MRB_SYM(sym)

#endif  /* !MRB_PRESYM_SCANNING */

#endif  /* MRUBY_PRESYM_H */
