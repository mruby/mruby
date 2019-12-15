#ifndef CARBUNCLE_SPRITE_H
#define CARBUNCLE_SPRITE_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

struct mrb_Sprite
{
  mrb_float angle;
  Rectangle *src_rect;
  Vector2 *position;
  Vector2 *scale;
  Vector2 *pivot;
  Color   *color;
};

void
mrb_init_carbuncle_sprite(mrb_state *mrb);

struct mrb_Sprite *
mrb_carbuncle_get_sprite(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_sprite_p(mrb_value obj);

void
mrb_carbuncle_sprite_draw_texture(Texture2D texture, Rectangle src, Rectangle dst, Vector2 origin, float angle, Color tint);

#ifdef __cplusplus
}
#endif

#endif