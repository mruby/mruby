/**
** @file mruby/presym/scanning.h - Enable Preallocated Symbols
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_PRESYM_ENABLE_H
#define MRUBY_PRESYM_ENABLE_H

#undef MRB_PRESYM_MAX
#define MRB_PRESYM_NAMED(lit, num, type, name)
#define MRB_PRESYM_UNNAMED(lit, num)
/* test for proper -I `<build-dir>/include` */
/* fallback `presym.inc` defines `MRB_NO_PRESYM` */
# include <mruby/presym.inc>

#undef MRB_PRESYM_MAX
#undef MRB_PRESYM_NAMED
#undef MRB_PRESYM_UNNAMED

#if defined(MRB_NO_PRESYM)
# include <mruby/presym/disable.h>
#else

#define MRB_PRESYM_NAMED(lit, num, type, name) MRB_##type##__##name = (num),
#define MRB_PRESYM_UNNAMED(lit, num)

enum mruby_presym {
# include <mruby/presym.inc>
};

#undef MRB_PRESYM_NAMED
#undef MRB_PRESYM_UNNAMED

#define MRB_OPSYM(name) MRB_OPSYM__##name
#define MRB_CVSYM(name) MRB_CVSYM__##name
#define MRB_IVSYM(name) MRB_IVSYM__##name
#define MRB_SYM_B(name) MRB_SYM_B__##name
#define MRB_SYM_Q(name) MRB_SYM_Q__##name
#define MRB_SYM_E(name) MRB_SYM_E__##name
#define MRB_SYM(name) MRB_SYM__##name

#define MRB_OPSYM_2(mrb, name) MRB_OPSYM__##name
#define MRB_CVSYM_2(mrb, name) MRB_CVSYM__##name
#define MRB_IVSYM_2(mrb, name) MRB_IVSYM__##name
#define MRB_SYM_B_2(mrb, name) MRB_SYM_B__##name
#define MRB_SYM_Q_2(mrb, name) MRB_SYM_Q__##name
#define MRB_SYM_E_2(mrb, name) MRB_SYM_E__##name
#define MRB_SYM_2(mrb, name) MRB_SYM__##name

#define MRB_PRESYM_DEFINE_VAR_AND_INITER(name, size, ...)                     \
  static const mrb_sym name[] = {__VA_ARGS__};                                \
  static void init_##name(mrb_state *mrb) {}

#endif  /* !MRUBY_NO_PRESYM */

#endif  /* MRUBY_PRESYM_ENABLE_H */
