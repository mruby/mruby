#include <mruby.h>

#include "carbuncle/color.h"
#include "carbuncle/texture.h"
#include "carbuncle/sprite.h"
#include "carbuncle/font.h"
#include "carbuncle/text.h"

void
mrb_carbuncle_graphics_gem_init(mrb_state *mrb)
{
  mrb_carbuncle_color_init(mrb);
  mrb_carbuncle_texture_init(mrb);
  mrb_carbuncle_sprite_init(mrb);
  mrb_carbuncle_font_init(mrb);
  mrb_carbuncle_text_init(mrb);
}

void
mrb_carbuncle_graphics_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
