/*
** mruby/addition.h - additional defines for mruby.h
**
** See Copyright Notice in mruby.h
**
** This file collects the things that would prevent documentation generation if defined in mruby.h.
*/

#ifndef MRUBY_ADDITION_H
#define MRUBY_ADDITION_H

MRB_BEGIN_DECL

mrb_noreturn void mrb_raise_str(mrb_state *mrb, struct RClass *c, mrb_value mesg_str);

#define mrb_raise(mrb, c, mesg) do { \
  mrb_value _exc_mesg = mrb_str_new_cstr(mrb, mesg); \
  struct RClass *_exc_class = (c); \
  mrb_raise_str(mrb, _exc_class, _exc_mesg); \
} while (0)

#define mrb_raisef(mrb, c, ...) do { \
  mrb_value _exc_mesg = mrb_format(mrb, __VA_ARGS__); \
  struct RClass *_exc_class = (c); \
  mrb_raise_str(mrb, _exc_class, _exc_mesg); \
} while (0)

MRB_END_DECL

#endif /* MRUBY_ADDITION_H */
