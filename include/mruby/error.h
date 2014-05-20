/*
** error.h - Exception class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_ERROR_H
#define MRUBY_ERROR_H

#if defined(__cplusplus)
extern "C" {
#endif

void mrb_sys_fail(mrb_state *mrb, const char *mesg);
mrb_value mrb_exc_new_str(mrb_state *mrb, struct RClass* c, mrb_value str);
#define mrb_exc_new_str_lit(mrb, c, lit) mrb_exc_new_str(mrb, c, mrb_str_new_lit(mrb, lit))
mrb_value mrb_make_exception(mrb_state *mrb, int argc, const mrb_value *argv);
void mrb_exc_print(mrb_state *mrb, struct RObject *exc);
void mrb_print_backtrace(mrb_state *mrb);
mrb_value mrb_exc_backtrace(mrb_state *mrb, mrb_value exc);
mrb_value mrb_get_backtrace(mrb_state *mrb);

/* declaration for fail method */
mrb_value mrb_f_raise(mrb_state*, mrb_value);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_ERROR_H */
