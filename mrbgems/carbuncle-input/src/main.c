#include <mruby.h>

#include "carbuncle/keyboard.h"
#include "carbuncle/mouse.h"
#include "carbuncle/touch.h"
#include "carbuncle/gesture.h"
#include "carbuncle/gamepad.h"

void
mrb_carbuncle_input_gem_init(mrb_state *mrb)
{
  mrb_init_carbuncle_keyboard(mrb);
  mrb_init_carbuncle_mouse(mrb);
  mrb_init_carbuncle_touch(mrb);
  mrb_init_carbuncle_gesture(mrb);
  mrb_init_carbuncle_gamepad(mrb);
}

void
mrb_carbuncle_input_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
