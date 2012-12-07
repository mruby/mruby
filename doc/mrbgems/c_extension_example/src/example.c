#include <mruby.h>
#include <stdio.h>

static struct RClass *_class_cextension;

static mrb_value
mrb_c_method(mrb_state *mrb, mrb_value self)
{
  puts("A C Extension");
  return self;
}

void
mrb_c_extension_example_gem_init(mrb_state* mrb) {
  _class_cextension = mrb_define_module(mrb, "CExtension");
  mrb_define_class_method(mrb, _class_cextension, "c_method", mrb_c_method, ARGS_NONE());
}
