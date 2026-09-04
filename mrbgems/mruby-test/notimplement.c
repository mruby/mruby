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
#include <mruby/error.h>
#include <mruby/variable.h>

static mrb_value
notimplement_here(mrb_state *mrb, void *userdata)
{
  mrb_notimplement(mrb);
  /* not reached */
  return mrb_nil_value();
}

void
mrb_init_test_notimplement(mrb_state *mrb)
{
  struct RClass *c = mrb_define_class(mrb, "TestNotImplement", mrb->object_class);

  mrb_define_method(mrb, c, "gone", mrb_notimplement_m, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, c, "gone", mrb_notimplement_m, MRB_ARGS_NONE());

  /* `mrb_notimplement()` names the method from the frame it is called on. A
  ** gem init function is called by the embedder rather than by a method call,
  ** so there is no name on this frame. Ruby cannot build such a frame, so make
  ** the call from here and hand the outcome to the tests. */
  mrb_bool error;
  mrb_value result = mrb_protect_error(mrb, notimplement_here, NULL, &error);
  mrb_define_const(mrb, c, "NAMELESS_RAISED", mrb_bool_value(error));
  mrb_define_const(mrb, c, "NAMELESS_RESULT", result);
}
