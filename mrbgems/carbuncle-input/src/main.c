#include <mruby.h>

#include "carbuncle/keyboard.h"
#include "carbuncle/mouse.h"
#include "carbuncle/touch.h"
#include "carbuncle/gesture.h"
#include "carbuncle/gamepad.h"

void
mrb_carbuncle_input_gem_init(mrb_state *mrb)
{
  mrb_carbuncle_keyboard_init(mrb);
  mrb_carbuncle_mouse_init(mrb);
  mrb_carbuncle_touch_init(mrb);
  mrb_carbuncle_gesture_init(mrb);
  mrb_carbuncle_gamepad_init(mrb);
}

void
mrb_carbuncle_input_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
