# mruby/error.h

## Undocumented

```C
void mrb_sys_fail(mrb_state *mrb, const char *mesg);
mrb_value mrb_exc_new_str(mrb_state *mrb, struct RClass* c, mrb_value str);
#define mrb_exc_new_str_lit(mrb, c, lit) mrb_exc_new_str(mrb, c, mrb_str_new_lit(mrb, lit))
mrb_value mrb_make_exception(mrb_state *mrb, int argc, const mrb_value *argv);
void mrb_exc_print(mrb_state *mrb, struct RObject *exc);
void mrb_print_backtrace(mrb_state *mrb);
mrb_value mrb_exc_backtrace(mrb_state *mrb, mrb_value exc);
mrb_value mrb_get_backtrace(mrb_state *mrb);
mrb_noreturn void mrb_no_method_error(mrb_state *mrb, mrb_sym id, mrb_int argc, const mrb_value *argv, const char *fmt, ...);

/* declaration for fail method */
mrb_value mrb_f_raise(mrb_state*, mrb_value);
```
