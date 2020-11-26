/**
** @file mruby/presym.h - Preallocated Symbols
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_PRESYM_H
#define MRUBY_PRESYM_H

#undef MRB_PRESYM_MAX
#ifdef MRB_USE_ALL_SYMBOLS
#define MRB_PRESYM_NAMED(lit, num, type, name) MRB_##type##__##name = (num),
#else
#define MRB_PRESYM_NAMED(lit, num, type, name) MRB_##type##__##name = (num<<1),
#endif
#define MRB_PRESYM_UNNAMED(lit, num)

enum mruby_presym {
#include <../build/presym.inc>
};

#undef MRB_PRESYM_NAMED
#undef MRB_PRESYM_UNNAMED

/*
 * For `MRB_OPSYM`, specify the names corresponding to operators (refer to
 * `op_table` in `Rakefile` for the names that can be specified for it).
 * Other than that, describe only word characters excluding leading and
 * ending punctuations.
 *
 * Example:
 *   MRB_OPSYM(and)  //=> &
 *   MRB_CVSYM(foo)  //=> @@foo
 *   MRB_IVSYM(foo)  //=> @foo
 *   MRB_SYM_B(foo)  //=> foo!
 *   MRB_SYM_Q(foo)  //=> foo?
 *   MRB_SYM_E(foo)  //=> foo=
 *   MRB_SYM(foo)    //=> foo
 */
#define MRB_OPSYM(name) MRB_OPSYM__##name  /* Operator */
#define MRB_CVSYM(name) MRB_CVSYM__##name  /* Class Variable */
#define MRB_IVSYM(name) MRB_IVSYM__##name  /* Instance Variable */
#define MRB_SYM_B(name) MRB_SYM_B__##name  /* Method with Bang */
#define MRB_SYM_Q(name) MRB_SYM_Q__##name  /* Method with Question mark */
#define MRB_SYM_E(name) MRB_SYM_E__##name  /* Method with Equal */
#define MRB_SYM(name)   MRB_SYM__##name    /* Word characters */

#endif  /* MRUBY_PRESYM_H */
