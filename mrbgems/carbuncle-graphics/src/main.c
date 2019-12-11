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
  mrb_carbuncle_color_init(mrb);
  mrb_carbuncle_bitmap_init(mrb);
  mrb_carbuncle_texture_init(mrb);
  mrb_carbuncle_sprite_init(mrb);
  mrb_carbuncle_font_init(mrb);
  mrb_carbuncle_text_init(mrb);
  mrb_carbuncle_plane_init(mrb);
  mrb_carbuncle_viewport_init(mrb);
  mrb_carbuncle_canvas_init(mrb);
  mrb_carbuncle_nine_patch_init(mrb);
  mrb_carbuncle_shader_init(mrb);
  mrb_carbuncle_graphics_init(mrb);
}

void
mrb_carbuncle_graphics_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
