/**
** @file mruby/presym.h - Preallocated Symbols
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_PRESYM_H
#define MRUBY_PRESYM_H

#if !defined(MRB_PRESYM_SCANNING)
# include <mruby/presym/enable.h>
#endif

/*
 * Where `mrb_intern_lit` is allowed for symbol interning, it is directly
 * replaced by the symbol ID if presym is enabled by using the following
 * macros.
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
 * The `_2` suffix variants (e.g., `MRB_SYM_2`) accept an explicit
 * mruby state parameter but currently ignore it.
 */

#endif  /* MRUBY_PRESYM_H */
