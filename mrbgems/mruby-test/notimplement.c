/*
** notimplement.c - methods standing for features a build does not have
**
** `mrb_notimplement_m` is what a gem installs when the platform lacks the
** call behind a method (`IO#pread` without pread(2), and so on). Which of
** those a build ends up with depends on the platform, so the tests for how
** such a method behaves need one that is always there.
*/

#include <mruby.h>
#include <mruby/class.h>

void
mrb_init_test_notimplement(mrb_state *mrb)
{
  struct RClass *c = mrb_define_class(mrb, "TestNotImplement", mrb->object_class);

  mrb_define_method(mrb, c, "gone", mrb_notimplement_m, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, c, "gone", mrb_notimplement_m, MRB_ARGS_NONE());
}
