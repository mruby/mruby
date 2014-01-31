/*
** error.h - Exception class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_ERROR_H
#define MRUBY_ERROR_H

void mrb_sys_fail(mrb_state *mrb, const char *mesg);
mrb_value mrb_exc_new_str(mrb_state *mrb, struct RClass* c, mrb_value str);
mrb_value mrb_make_exception(mrb_state *mrb, int argc, mrb_value *argv);
mrb_value mrb_format(mrb_state *mrb, const char *format, ...);
void mrb_exc_print(mrb_state *mrb, struct RObject *exc);
void mrb_longjmp(mrb_state *mrb);

#endif  /* MRUBY_ERROR_H */
