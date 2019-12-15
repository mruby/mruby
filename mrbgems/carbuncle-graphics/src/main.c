#include <mruby.h>

#include "carbuncle/color.h"
#include "carbuncle/bitmap.h"
#include "carbuncle/texture.h"
#include "carbuncle/sprite.h"
#include "carbuncle/font.h"
#include "carbuncle/text.h"
#include "carbuncle/plane.h"
#include "carbuncle/viewport.h"
#include "carbuncle/canvas.h"
#include "carbuncle/nine_patch.h"
#include "carbuncle/shader.h"
#include "carbuncle/graphics.h"

void
mrb_carbuncle_graphics_gem_init(mrb_state *mrb)
{
  mrb_init_carbuncle_color(mrb);
  mrb_init_carbuncle_bitmap(mrb);
  mrb_init_carbuncle_texture(mrb);
  mrb_init_carbuncle_sprite(mrb);
  mrb_init_carbuncle_font(mrb);
  mrb_init_carbuncle_text(mrb);
  mrb_init_carbuncle_plane(mrb);
  mrb_init_carbuncle_viewport(mrb);
  mrb_init_carbuncle_canvas(mrb);
  mrb_init_carbuncle_nine_patch(mrb);
  mrb_init_carbuncle_shader(mrb);
  mrb_init_carbuncle_graphics(mrb);
}

void
mrb_carbuncle_graphics_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
