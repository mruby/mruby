#include "mruby.h"

extern mrb_value mrb_format(mrb_state *mrb, const char *format, ...);

/* Each test function should have test_mrb_ prefix. */
/* The name of test class initializer must be 'test_mrb_#{filename}_init'. */

mrb_value
test_1_mrb_format(mrb_state *mrb, mrb_value self)
{
  /* Will return "foo". */
  return mrb_format(mrb, "%S", mrb_str_new_cstr(mrb, "foo"));
}

mrb_value
test_2_mrb_format(mrb_state *mrb, mrb_value self)
{
  /* Will return "10". */
  return mrb_format(mrb, "%S", mrb_fixnum_value(10));
}

mrb_value
test_3_mrb_format(mrb_state *mrb, mrb_value self)
{
  /* Will return " foo". */
  return mrb_format(mrb, " %S", mrb_str_new_cstr(mrb, "foo"));
}

mrb_value
test_4_mrb_format(mrb_state *mrb, mrb_value self)
{
  /* Will return "foo bar". */
  return mrb_format(mrb, "%S %S",
                    mrb_str_new_cstr(mrb, "foo"),
                    mrb_str_new_cstr(mrb, "bar"));
}

mrb_value
test_5_mrb_format(mrb_state *mrb, mrb_value self)
{
  /* Will return "%S" as % is escaped. */
  return mrb_format(mrb, "\\%S", mrb_str_new_cstr(mrb, "foo"));
}

void
test_mrb_format_init(mrb_state *mrb)
{
  struct RClass *test;

  test = mrb_define_module(mrb, "CAPITest_mrb_format");
  mrb_define_module_function(mrb, test, "test1", test_1_mrb_format, ARGS_NONE());
  mrb_define_module_function(mrb, test, "test2", test_2_mrb_format, ARGS_NONE());
  mrb_define_module_function(mrb, test, "test3", test_3_mrb_format, ARGS_NONE());
  mrb_define_module_function(mrb, test, "test4", test_4_mrb_format, ARGS_NONE());
  mrb_define_module_function(mrb, test, "test5", test_5_mrb_format, ARGS_NONE());
}
