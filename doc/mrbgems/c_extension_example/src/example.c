#include <mruby.h>
#include <stdio.h>

struct example_data {
  struct RClass *class_cextension;
  const char *message;
};

static mrb_value
mrb_c_method(mrb_state *mrb, mrb_value self)
{
  struct example_data *data = mrb_c_extension_example_gem_data(mrb);
  puts(data->message);
  return self;
}

void
mrb_c_extension_example_gem_init(mrb_state* mrb)
{
  struct example_data *data = malloc(sizeof(struct example_data));
  mrb_c_extension_example_gem_data(mrb) = data;
  
  data->message = "A C Extension";
  data->class_cextension = mrb_define_module(mrb, "CExtension");

  mrb_define_class_method(mrb, data->class_cextension, "c_method", mrb_c_method, ARGS_NONE());
}

void
mrb_c_extension_example_gem_final(mrb_state* mrb)
{
  struct example_data *data = mrb_c_extension_example_gem_data(mrb);
  free(data);
}
