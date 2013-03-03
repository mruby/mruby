#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/compile.h>
#include <mruby/variable.h>
#include <mruby/string.h>

static mrb_value
mrb_str_to_cstr_test(mrb_state *mrb, mrb_value self)
{
  mrb_value str;

  mrb_get_args(mrb, "S", &str);

  char *res = mrb_str_to_cstr(mrb, str);

  return mrb_str_new(mrb, res, strlen(res));
}

void
mrb_init_string_test(mrb_state *mrb)
{
  struct RClass *str_test;
  str_test = mrb_define_module(mrb, "StringTest");

  mrb_define_module_function(mrb, str_test, "test_str_to_cstr", mrb_str_to_cstr_test, ARGS_REQ(1));
}
