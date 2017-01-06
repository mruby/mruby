#include <mruby.h>
#include <mruby/class.h>
#include <mruby/proc.h>
#include <mruby/string.h>

static mrb_value
numeric_override_run(mrb_state *mrb, mrb_value self)
{
    char *string;
    mrb_state *sub_mrb;
    mrb_value result;
    const char *string_result;
    mrb_value boxed_string_result;

    mrb_get_args(mrb, "z", &string);
	sub_mrb = mrb_open();
  	result = mrb_load_string(sub_mrb, string);
  	string_result = mrb_string_value_cstr(sub_mrb, &result);
  	boxed_string_result = mrb_str_new_cstr(mrb, string_result);
  	mrb_close(sub_mrb);  	
	return boxed_string_result;
}

void mrb_mruby_numeric_override_gem_test(mrb_state *mrb)
{
  struct RClass *cls;

  cls = mrb_define_module(mrb, "NumericOverrideTest");

  mrb_define_class_method(mrb, cls, "run", numeric_override_run, MRB_ARGS_REQ(1));

#ifdef MRB_ENABLE_NUMERIC_OVERRIDE
  mrb_define_const(mrb, cls, "MRB_ENABLE_NUMERIC_OVERRIDE", mrb_bool_value(1));
#else
  mrb_define_const(mrb, cls, "MRB_ENABLE_NUMERIC_OVERRIDE", mrb_bool_value(0));
#endif
}
