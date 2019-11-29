#include <mruby.h>

static const char *FROZEN_ERROR_MSG = "can't modify frozen %s";
static const char *ARG_ERROR_MSG = "wrong number of arguments (given %s, expected %d)";

void
mrb_carbuncle_check_frozen(mrb_state *mrb, mrb_value obj)
{
  mrb_value is_frozen = mrb_funcall(mrb, obj, "frozen?", 0);
  if (mrb_test(is_frozen))
  {
    const char *class_name = mrb_class_name(mrb, mrb_obj_class(mrb, obj));
    mrb_raisef(mrb, E_FROZEN_ERROR, FROZEN_ERROR_MSG, class_name);
  }
}

void
mrb_carbuncle_raise(mrb_state *mrb, const char *name, const char *msg)
{
  struct RClass *carbuncle = mrb_module_get(mrb, "Carbuncle");
  struct RClass *error_class = mrb_class_get_under(mrb, carbuncle, name);
  mrb_raise(mrb, error_class, msg);
}

void
mrb_carbuncle_arg_error(mrb_state *mrb, const char *options, mrb_int argc)
{
  mrb_raisef(mrb, E_ARGUMENT_ERROR, ARG_ERROR_MSG, options, argc);
}