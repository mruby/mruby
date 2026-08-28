/*
** sysfail.c - reach `mrb_sys_fail()` from Ruby
**
** `mrb_sys_fail()` reports the errno through SystemCallError when that class
** is there, and mruby-errno is what defines it. A build without that gem
** takes the other path, and the core test state is such a build: it holds
** `mrb_open_core()` and nothing else. Nothing reachable from Ruby in that
** state makes the call, and the errno it reads is a C one, so testing either
** path needs a method that sets errno and calls it.
*/

#include <errno.h>
#include <mruby.h>
#include <mruby/class.h>
#include <mruby/error.h>
#include <mruby/string.h>

static mrb_value
sf_s_fail(mrb_state *mrb, mrb_value klass)
{
  mrb_int no;
  mrb_value mesg = mrb_nil_value();

  mrb_get_args(mrb, "i|S!", &no, &mesg);
  errno = (int)no;
  mrb_sys_fail(mrb, mrb_nil_p(mesg) ? NULL : RSTRING_CSTR(mrb, mesg));
  /* not reached */
  return mrb_nil_value();
}

void
mrb_init_test_sysfail(mrb_state *mrb)
{
  struct RClass *c = mrb_define_class(mrb, "TestSysFail", mrb->object_class);

  mrb_define_class_method(mrb, c, "fail", sf_s_fail, MRB_ARGS_ARG(1, 1));

  /* Which path the tests can expect is decided by the state they run in, and
  ** asking here keeps that answer next to the call it decides for. */
  mrb_define_const(mrb, c, "SYSTEM_CALL_ERROR_DEFINED",
                   mrb_bool_value(mrb_class_defined_id(mrb, MRB_SYM(SystemCallError))));
}
