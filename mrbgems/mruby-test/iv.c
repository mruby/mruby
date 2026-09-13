/*
** iv.c - helpers for test/t/kernel.rb
**
** An instance variable whose name has no '@' can only be set from C
** (mrb_iv_set with a bare symbol); Ruby's own instance_variable_set refuses
** the name. Extensions keep private state that way, so a test needs a way
** to plant one and check what the Ruby-facing methods make of it.
*/

#include <mruby.h>
#include <mruby/variable.h>

void mrb_init_test_iv(mrb_state *mrb);

static mrb_value
iv_set_hidden(mrb_state *mrb, mrb_value self)
{
  mrb_sym name;
  mrb_value val;

  mrb_get_args(mrb, "no", &name, &val);
  mrb_iv_set(mrb, self, name, val);
  return val;
}

void
mrb_init_test_iv(mrb_state *mrb)
{
  struct RClass *o = mrb->object_class;

  mrb_define_method(mrb, o, "__iv_set_hidden", iv_set_hidden, MRB_ARGS_REQ(2));
}
