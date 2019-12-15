#include "carbuncle/core.h"
#include "carbuncle/color.h"
#include "carbuncle/point.h"
#include "carbuncle/texture.h"
#include "carbuncle/rect.h"
#include "carbuncle/sprite.h"

#include "carbuncle/plane.h"

#include <mruby/variable.h>
#include <mruby/class.h>
#include <mruby/data.h>

#include <math.h>

#include "raylib.h"
#include "rlgl.h"

#define TEXTURE_SYMBOL mrb_intern_cstr(mrb, "#texture")
#define ORIGIN_SYMBOL mrb_intern_cstr(mrb, "#origin")
#define SCALE_SYMBOL mrb_intern_cstr(mrb, "#scale")
#define COLOR_SYMBOL mrb_intern_cstr(mrb, "#color")
#define SRC_RECT_SYMBOL mrb_intern_cstr(mrb, "#src_rect")

static inline mrb_int
ruby_mod(mrb_int a, mrb_int b)
{
  return (a % b + b) % b;
}

struct mrb_Plane
{
  Texture2D *texture;
  Vector2   *origin;
  Vector2   *scale;
  Color     *color;
  Rectangle *src_rect;
};

static const struct mrb_data_type plane_data_type = {
  "Carbuncle::Plane", mrb_free
};

static inline struct mrb_Plane *
get_plane(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &plane_data_type, struct mrb_Plane);
}

mrb_value
mrb_plane_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value origin, scale, color, rect, texture = mrb_nil_value();
  mrb_int argc = mrb_get_args(mrb, "|o", &texture);
  struct mrb_Plane *plane = mrb_malloc(mrb, sizeof *plane);
  if (argc > 0 && !mrb_nil_p(texture))
  {
    plane->texture = mrb_carbuncle_get_texture(mrb, texture);
  }
  else
  {
    plane->texture = NULL;
  }
  origin = mrb_carbuncle_point_new(mrb, 0, 0);
  mrb_iv_set(mrb, self, ORIGIN_SYMBOL, origin);
  scale = mrb_carbuncle_point_new(mrb, 1, 1);
  mrb_iv_set(mrb, self, SCALE_SYMBOL, scale);
  color = mrb_carbuncle_color_new(mrb, 255, 255, 255, 255);
  mrb_iv_set(mrb, self, COLOR_SYMBOL, color);
  plane->origin = mrb_carbuncle_get_point(mrb, origin);
  plane->scale = mrb_carbuncle_get_point(mrb, scale);
  plane->color = mrb_carbuncle_get_color(mrb, color);
  mrb_iv_set(mrb, self, TEXTURE_SYMBOL, texture);
  rect = mrb_carbuncle_rect_new(mrb, 0, 0, plane->texture->width, plane->texture->height);
  plane->src_rect = mrb_carbuncle_get_rect(mrb, rect);
  mrb_iv_set(mrb, self, SRC_RECT_SYMBOL, rect);
  DATA_PTR(self) = plane;
  DATA_TYPE(self) = &plane_data_type;
  return self;
}

mrb_value
mrb_plane_get_texture(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  return mrb_iv_get(mrb, self, TEXTURE_SYMBOL);
}

mrb_value
mrb_plane_get_origin(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  return mrb_iv_get(mrb, self, ORIGIN_SYMBOL);
}

mrb_value
mrb_plane_get_scale(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  return mrb_iv_get(mrb, self, SCALE_SYMBOL);
}

mrb_value
mrb_plane_get_color(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  return mrb_iv_get(mrb, self, COLOR_SYMBOL);
}

mrb_value
mrb_plane_get_src_rect(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  return mrb_iv_get(mrb, self, SRC_RECT_SYMBOL);
}


mrb_value
mrb_plane_set_texture(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  mrb_value value;
  mrb_get_args(mrb, "o", value);
  if (!mrb_nil_p(value))
  {
    plane->texture = mrb_carbuncle_get_texture(mrb, value);
  }
  mrb_iv_set(mrb, self, TEXTURE_SYMBOL, value);
  return value;
}

mrb_value
mrb_plane_set_origin(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  mrb_value value;
  mrb_get_args(mrb, "o", value);
  if (!mrb_nil_p(value))
  {
    plane->origin = mrb_carbuncle_get_point(mrb, value);
  }
  mrb_iv_set(mrb, self, ORIGIN_SYMBOL, value);
  return value;
}

mrb_value
mrb_plane_set_scale(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  mrb_value value;
  mrb_get_args(mrb, "o", value);
  if (!mrb_nil_p(value))
  {
    plane->scale = mrb_carbuncle_get_point(mrb, value);
  }
  mrb_iv_set(mrb, self, SCALE_SYMBOL, value);
  return value;
}

mrb_value
mrb_plane_set_color(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  mrb_value value;
  mrb_get_args(mrb, "o", value);
  if (!mrb_nil_p(value))
  {
    plane->color = mrb_carbuncle_get_color(mrb, value);
  }
  mrb_iv_set(mrb, self, COLOR_SYMBOL, value);
  return value;
}

mrb_value
mrb_plane_set_src_rect(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  mrb_value value;
  mrb_get_args(mrb, "o", value);
  if (!mrb_nil_p(value))
  {
    plane->src_rect = mrb_carbuncle_get_rect(mrb, value);
  }
  mrb_iv_set(mrb, self, SRC_RECT_SYMBOL, value);
  return value;
}

mrb_value
mrb_plane_draw(mrb_state *mrb, mrb_value self)
{
  mrb_int ox, oy, tx, ty, w, h;
  struct mrb_Plane *plane = get_plane(mrb, self);
  if (!plane->texture)
  {
    return self;
  }
  if (!(plane->scale->x) || !(plane->scale->y))
  {
    return self;
  }
  if (!(plane->color->a))
  {
    return self;
  }
  
  ox = ruby_mod(plane->origin->x, plane->texture->width);
  oy = ruby_mod(plane->origin->y, plane->texture->height);
  w = plane->texture->width  * plane->scale->x;
  h = plane->texture->height * plane->scale->y;
  tx = GetScreenWidth() / plane->texture->width + 2;
  ty = GetScreenHeight() / plane->texture->height + 2;
  Vector2 origin = (Vector2){0, 0};

  rlEnableTexture(plane->texture->id);
  for (mrb_int i = -1; i < tx; ++i) 
  {
    for (mrb_int j = -1; j < ty; ++j)
    {
      Rectangle dst_rect = {
        ox + w * i, oy + h * j, w, h
      };
      mrb_carbuncle_sprite_draw_texture(
        *(plane->texture), *(plane->src_rect), dst_rect, origin, 0, *(plane->color)
      );
    }
  }
  rlDisableTexture();

  return self;
}

mrb_value
mrb_plane_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

mrb_value
mrb_plane_dispose(mrb_state *mrb, mrb_value self)
{
  struct mrb_Plane *plane = get_plane(mrb, self);
  if (!plane->texture)
  {
    return self;
  }
  mrb_free(mrb, plane);
  DATA_PTR(self) = NULL;
  return self;
}


void
mrb_init_carbuncle_plane(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *plane = mrb_define_class_under(mrb, carbuncle, "Plane", mrb->object_class);
  MRB_SET_INSTANCE_TT(plane, MRB_TT_DATA);

  mrb_define_method(mrb, plane, "initialize", mrb_plane_initialize, MRB_ARGS_OPT(1));

  mrb_define_method(mrb, plane, "texture", mrb_plane_get_texture, MRB_ARGS_NONE());
  mrb_define_method(mrb, plane, "origin", mrb_plane_get_origin, MRB_ARGS_NONE());
  mrb_define_method(mrb, plane, "scale", mrb_plane_get_scale, MRB_ARGS_NONE());
  mrb_define_method(mrb, plane, "color", mrb_plane_get_color, MRB_ARGS_NONE());
  mrb_define_method(mrb, plane, "src_rect", mrb_plane_get_src_rect, MRB_ARGS_NONE());

  mrb_define_method(mrb, plane, "texture=", mrb_plane_set_texture, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, plane, "origin=", mrb_plane_set_origin, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, plane, "scale=", mrb_plane_set_scale, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, plane, "color=", mrb_plane_set_color, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, plane, "src_rect=", mrb_plane_set_src_rect, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, plane, "disposed?", mrb_plane_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, plane, "dispose", mrb_plane_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, plane, "draw", mrb_plane_draw, MRB_ARGS_NONE());
}
