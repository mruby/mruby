#include "carbuncle/core.h"

#include "raylib.h"

#ifdef CARBUNCLE_DEBUG

static Color DEBUG_COLORS[22] = {
  LIGHTGRAY,
  GRAY,
  DARKGRAY,
  YELLOW,
  GOLD,
  ORANGE,
  PINK,
  RED,
  MAROON,
  GREEN,
  LIME,
  DARKGREEN,
  SKYBLUE,
  BLUE,
  DARKBLUE,
  PURPLE,
  VIOLET,
  DARKPURPLE,
  BEIGE,
  BROWN,
  DARKBROWN,
  MAGENTA
};

mrb_int mrb_carbuncle_debug_color_index = 0;

void
mrb_carbuncle_draw_debug_rect(Rectangle rect, Vector2 origin, float rotation)
{
  if (mrb_carbuncle_debug_drawing)
  {
    DrawRectanglePro(rect, origin, rotation, DEBUG_COLORS[mrb_carbuncle_debug_color_index]);
    mrb_carbuncle_debug_color_index = (mrb_carbuncle_debug_color_index + 1) % 22;
  }
}

#endif
