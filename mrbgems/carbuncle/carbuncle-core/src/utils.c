#include "carbuncle/core.h"

#include <mruby/data.h>

struct RClass *
mrb_carbuncle_define_data_class(mrb_state *mrb, const char *name, struct RClass *super)
{
  struct RClass *module = mrb_module_get(mrb, "Carbuncle");
  struct RClass *type = mrb_define_class_under(mrb, module, name, super);
  MRB_SET_INSTANCE_TT(type, MRB_TT_DATA);
  return type;  
}