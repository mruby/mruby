/*
** mruby/error.h - Exception class
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_ERROR_H
#define MRUBY_ERROR_H

#if defined(__cplusplus)
extern "C" {
#endif

struct RException {
  MRB_OBJECT_HEADER;
  struct iv_tbl *iv;
};

#define mrb_exc_ptr(v) ((struct RException*)mrb_ptr(v))

MRB_API void mrb_sys_fail(mrb_state *mrb, const char *mesg);
MRB_API mrb_value mrb_exc_new_str(mrb_state *mrb, struct RClass* c, mrb_value str);
#define mrb_exc_new_str_lit(mrb, c, lit) mrb_exc_new_str(mrb, c, mrb_str_new_lit(mrb, lit))
MRB_API mrb_value mrb_make_exception(mrb_state *mrb, int argc, const mrb_value *argv);
MRB_API mrb_value mrb_exc_backtrace(mrb_state *mrb, mrb_value exc);
MRB_API mrb_value mrb_get_backtrace(mrb_state *mrb);
MRB_API mrb_noreturn void mrb_no_method_error(mrb_state *mrb, mrb_sym id, mrb_int argc, const mrb_value *argv, const char *fmt, ...);

/* declaration for fail method */
MRB_API mrb_value mrb_f_raise(mrb_state*, mrb_value);

#if defined(__cplusplus)
}  /* extern "C" { */
#endif

#endif  /* MRUBY_ERROR_H */
