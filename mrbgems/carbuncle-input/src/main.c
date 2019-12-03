#include <mruby.h>

#include "carbuncle/keyboard.h"
#include "carbuncle/mouse.h"

void
mrb_carbuncle_input_gem_init(mrb_state *mrb)
{
  mrb_carbuncle_keyboard_init(mrb);
  mrb_carbuncle_mouse_init(mrb);
}

void
mrb_carbuncle_input_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
