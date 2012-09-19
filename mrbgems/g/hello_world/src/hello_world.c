#include <mruby.h>
#include <stdio.h>

static struct RClass *_class_hw;

static mrb_value
mrb_hello_world(mrb_state *mrb, mrb_value self)
{
  puts("Hello World");
  return self;
}

void
mrb_hello_world_gem_init(mrb_state* mrb) {
  _class_hw = mrb_define_module(mrb, "HW");
  mrb_define_class_method(mrb, _class_hw, "say", mrb_hello_world, ARGS_NONE());
}
