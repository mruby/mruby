#include <mruby.h>
#include <stdio.h>

static struct RClass *_class_clib;

static mrb_value
mrb_clib_example(mrb_state *mrb, mrb_value self)
{
  puts("A C Extension");
  return self;
}

void
mrb_clib_example_gem_init(mrb_state* mrb) {
  _class_clib = mrb_define_module(mrb, "CLib");
  mrb_define_class_method(mrb, _class_clib, "clib_method", mrb_clib_example, ARGS_NONE());
}
