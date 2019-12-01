#include "carbuncle/core.h"

#include <mruby.h>
#include <mruby/data.h>

#include "raylib.h"

static const char *FROZEN_ERROR_MSG = "can't modify frozen %s";
static const char *ARG_ERROR_MSG = "wrong number of arguments (given %d, expected %s)";
static const char *DISPOSED_ERROR_MSG = "can't obtain disposed %s.";

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
  struct RClass *error_class = mrb_carbuncle_class_get(mrb, name);
  mrb_raise(mrb, error_class, msg);
}

void
mrb_carbuncle_arg_error(mrb_state *mrb, const char *options, mrb_int argc)
{
  mrb_raisef(mrb, E_ARGUMENT_ERROR, ARG_ERROR_MSG, argc, options);
}

void *
mrb_check_disposed(mrb_state *mrb, mrb_value obj, const struct mrb_data_type *type)
{
  void *ptr = DATA_GET_PTR(mrb, obj, type, void);
  if (!ptr)
  {
    struct RClass *dispose_error = mrb_carbuncle_class_get(mrb, "DisposedObject");
    const char *class_name = mrb_class_name(mrb, mrb_obj_class(mrb, obj));
    mrb_raisef(mrb, dispose_error, DISPOSED_ERROR_MSG, class_name);
  }
  return ptr;
}

void
mrb_carbuncle_check_file(mrb_state *mrb, const char *filename)
{
  if (!FileExists(filename))
  {
    struct RClass *file_not_exists = mrb_carbuncle_class_get(mrb, "FileNotExists");
    mrb_raisef(mrb, file_not_exists, "file %s was not found.", filename);
  }
}
