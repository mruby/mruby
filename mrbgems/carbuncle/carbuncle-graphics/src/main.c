#include <mruby.h>

void
mrb_carbuncle_graphics_gem_init(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_module_get(mrb, "Carbuncle");
}

void
mrb_carbuncle_graphics_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
