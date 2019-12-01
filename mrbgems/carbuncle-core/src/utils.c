#include "carbuncle/core.h"

#include <mruby/data.h>
#include <mruby/class.h>

#define CARBUNCLE_MODULE mrb_module_get(mrb, "Carbuncle")

struct RClass *
mrb_carbuncle_class_get(mrb_state *mrb, const char *name)
{
  struct RClass *carbuncle = CARBUNCLE_MODULE;
  return mrb_class_get_under(mrb, carbuncle, name);  
}

struct RClass *
mrb_carbuncle_module_get(mrb_state *mrb, const char *name)
{
  struct RClass *carbuncle = CARBUNCLE_MODULE;
  return mrb_module_get_under(mrb, carbuncle, name);  
}

struct RClass *
mrb_carbuncle_define_data_class(mrb_state *mrb, const char *name, struct RClass *super)
{
  struct RClass *carbuncle = CARBUNCLE_MODULE;
  struct RClass *type = mrb_define_class_under(mrb, carbuncle, name, super);
  MRB_SET_INSTANCE_TT(type, MRB_TT_DATA);
  return type;  
}

struct RClass *
mrb_carbuncle_get(mrb_state *mrb)
{
  return CARBUNCLE_MODULE;
}