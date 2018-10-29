/*
** mruby/symbol.h - symbol utilities
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_SYMBOL_H
#define MRUBY_SYMBOL_H

#include "common.h"

/**
 * Symbol utilities
 */
MRB_BEGIN_DECL

typedef enum mrb_reserved_symbol {
  mrb_sym_null = 0, // NULL symbol

  mrb_sym_add = 1, // +
  mrb_sym_sub = 2, // -
  mrb_sym_mul = 3, // *
  mrb_sym_div = 4, // /
  mrb_sym_eq = 5, // ==
  mrb_sym_lt = 6, // <
  mrb_sym_le = 7, // <=
  mrb_sym_gt = 8, // >
  mrb_sym_ge = 9, // >=

  mrb_sym_method_missing = 10, // method_missing
} mrb_reserved_symbol;

static inline mrb_bool
mrb_symbol_constsym_send_p(mrb_sym sym) {
  return mrb_sym_add <= sym && sym <= mrb_sym_ge;
}

MRB_END_DECL

#endif  /* MRUBY_SYMBOL_H */
