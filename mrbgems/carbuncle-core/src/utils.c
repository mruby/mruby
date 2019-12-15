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
mrb_carbuncle_get(mrb_state *mrb)
{
  return CARBUNCLE_MODULE;
}

mrb_value
mrb_carbuncle_call_block(mrb_state *mrb, mrb_value block)
{
  return mrb_funcall(mrb, block, "call", 0);
}
