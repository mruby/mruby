#include "carbuncle/core.h"
#include "carbuncle/color.h"
#include "carbuncle/texture.h"
#include "carbuncle/sprite.h"

#include "carbuncle/point.h"
#include "carbuncle/rect.h"

#include <mruby/data.h>
#include <mruby/variable.h>

#include <math.h>

static const char *TEXTURE_SYM = "#texture";
static const char *POSITION_SYM = "#position";
static const char *SCALE_SYM = "#scale";
static const char *PIVOT_SYM = "#pivot";
static const char *SRC_RECT_SYM = "#src_rect";
static const char *COLOR_SYM = "#color";

#define TEXTURE_SYMBOL mrb_intern_cstr(mrb, TEXTURE_SYM)
#define POSITION_SYMBOL mrb_intern_cstr(mrb, POSITION_SYM)
#define SCALE_SYMBOL mrb_intern_cstr(mrb, SCALE_SYM)
#define PIVOT_SYMBOL mrb_intern_cstr(mrb, PIVOT_SYM)
#define SRC_RECT_SYMBOL mrb_intern_cstr(mrb, SRC_RECT_SYM)
#define COLOR_SYMBOL mrb_intern_cstr(mrb, COLOR_SYM)

static const struct mrb_data_type sprite_data_type = {
  "Carbuncle::Sprite", mrb_free
};

static mrb_value
mrb_sprite_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value position, scale, pivot, src_rect, color;
  mrb_value texture = mrb_nil_value();
  mrb_float width = 0;
  mrb_float height = 0;
  mrb_get_args(mrb, "|o", &texture);
  if (!mrb_nil_p(texture))
  {
    Texture2D *data = mrb_carbuncle_get_texture(mrb, texture);
    width = data->width;
    height = data->height;
  }
  position = mrb_carbuncle_point_new(mrb, 0, 0);
  scale = mrb_carbuncle_point_new(mrb, 1, 1);
  pivot = mrb_carbuncle_point_new(mrb, 0, 0);
  src_rect =  mrb_carbuncle_rect_new(mrb, 0, 0, width, height);
  color = mrb_carbuncle_color_new(mrb, 255, 255, 255, 255);
  mrb_iv_set(mrb, self, TEXTURE_SYMBOL, texture);
  mrb_iv_set(mrb, self, POSITION_SYMBOL, position);
  mrb_iv_set(mrb, self, SCALE_SYMBOL, scale);
  mrb_iv_set(mrb, self, PIVOT_SYMBOL, pivot);
  mrb_iv_set(mrb, self, SRC_RECT_SYMBOL, src_rect);
  mrb_iv_set(mrb, self, COLOR_SYMBOL, color);
  struct mrb_Sprite *sprite = mrb_malloc(mrb, sizeof *sprite);
  sprite->angle = 0;
  sprite->position = mrb_carbuncle_get_point(mrb, position);
  sprite->scale = mrb_carbuncle_get_point(mrb, scale);
  sprite->pivot = mrb_carbuncle_get_point(mrb, pivot);
  sprite->src_rect = mrb_carbuncle_get_rect(mrb, src_rect);
  sprite->color = mrb_carbuncle_get_color(mrb, color);
  DATA_PTR(self) = sprite;
  DATA_TYPE(self) = &sprite_data_type;
  return self;
}

static mrb_value
mrb_sprite_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

static mrb_value
mrb_sprite_get_texture(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sprite(mrb, self);
  return mrb_iv_get(mrb, self, TEXTURE_SYMBOL);
}

static mrb_value
mrb_sprite_get_position(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sprite(mrb, self);
  return mrb_iv_get(mrb, self, POSITION_SYMBOL);
}

static mrb_value
mrb_sprite_get_scale(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sprite(mrb, self);
  return mrb_iv_get(mrb, self, SCALE_SYMBOL);
}

static mrb_value
mrb_sprite_get_pivot(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sprite(mrb, self);
  return mrb_iv_get(mrb, self, PIVOT_SYMBOL);
}

static mrb_value
mrb_sprite_get_src_rect(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sprite(mrb, self);
  return mrb_iv_get(mrb, self, SRC_RECT_SYMBOL);
}

static mrb_value
mrb_sprite_get_color(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sprite(mrb, self);
  return mrb_iv_get(mrb, self, COLOR_SYMBOL);
}

static mrb_value
mrb_sprite_get_angle(mrb_state *mrb, mrb_value self)
{
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  return mrb_float_value(mrb, data->angle);
}

static mrb_value
mrb_sprite_get_width(mrb_state *mrb, mrb_value self)
{
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  return mrb_float_value(mrb, data->src_rect->width);
}

static mrb_value
mrb_sprite_get_height(mrb_state *mrb, mrb_value self)
{
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  return mrb_float_value(mrb, data->src_rect->height);
}

static mrb_value
mrb_sprite_set_texture(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  mrb_carbuncle_get_sprite(mrb, self);
  if (!mrb_nil_p(value))
  {
    mrb_carbuncle_get_texture(mrb, value);
  }
  mrb_iv_set(mrb, self, TEXTURE_SYMBOL, value);
  return value;
}

static mrb_value
mrb_sprite_set_position(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  struct Vector2 *point = mrb_carbuncle_get_point(mrb, value);
  data->position = point;
  return value;
}

static mrb_value
mrb_sprite_set_scale(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  struct Vector2 *point = mrb_carbuncle_get_point(mrb, value);
  data->scale = point;
  return value;
}

static mrb_value
mrb_sprite_set_pivot(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  struct Vector2 *point = mrb_carbuncle_get_point(mrb, value);
  data->pivot = point;
  return value;
}

static mrb_value
mrb_sprite_set_src_rect(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  struct Rectangle *rect = mrb_carbuncle_get_rect(mrb, value);
  data->src_rect = rect;
  return value;
}

static mrb_value
mrb_sprite_set_color(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  struct Color *color = mrb_carbuncle_get_color(mrb, value);
  data->color = color;
  return value;
}

static mrb_value
mrb_sprite_set_angle(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  data->angle = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_sprite_update(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sprite(mrb, self);
  return self;
}

static mrb_value
mrb_sprite_draw(mrb_state *mrb, mrb_value self)
{
  struct mrb_Sprite *data = mrb_carbuncle_get_sprite(mrb, self);
  mrb_value texture = mrb_iv_get(mrb, self, TEXTURE_SYMBOL);
  Texture2D *texture_data = mrb_carbuncle_get_texture(mrb, texture);
  if (mrb_nil_p(texture))
  {
    return;
  }  
  Rectangle *src_rect = data->src_rect;
  Rectangle dst_rect = (Rectangle){
    data->position->x,
    data->position->y,
    src_rect->width * data->scale->x,
    src_rect->height * data->scale->y
  };
  Vector2 origin = (Vector2){
    src_rect->width * data->pivot->x * fabsf(data->scale->x),
    src_rect->height * data->pivot->y * fabsf(data->scale->y)
  };
  DrawTexturePro(*texture_data, *src_rect, dst_rect, origin, data->angle, *(data->color));
  return self;
}

static mrb_value
mrb_sprite_dispose(mrb_state *mrb, mrb_value self)
{
  struct mrb_Sprite *sprite = mrb_carbuncle_get_sprite(mrb, self);
  mrb_free(mrb, sprite);
  DATA_PTR(self) = NULL;
  return self;
}

void
mrb_carbuncle_sprite_init(mrb_state *mrb)
{
  struct RClass *texture = mrb_carbuncle_define_data_class(mrb, "Sprite", mrb->object_class);
  mrb_define_method(mrb, texture, "initialize", mrb_sprite_initialize, MRB_ARGS_OPT(1));

  mrb_define_method(mrb, texture, "disposed?", mrb_sprite_disposedQ, MRB_ARGS_NONE());

  mrb_define_method(mrb, texture, "texture", mrb_sprite_get_texture, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "position", mrb_sprite_get_position, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "scale", mrb_sprite_get_scale, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "pivot", mrb_sprite_get_pivot, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "src_rect", mrb_sprite_get_src_rect, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "color", mrb_sprite_get_color, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "angle", mrb_sprite_get_angle, MRB_ARGS_NONE());

  mrb_define_method(mrb, texture, "width", mrb_sprite_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "height", mrb_sprite_get_height, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "w", mrb_sprite_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, texture, "h", mrb_sprite_get_height, MRB_ARGS_NONE());

  mrb_define_method(mrb, texture, "texture=", mrb_sprite_set_texture, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, texture, "position=", mrb_sprite_set_position, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, texture, "scale=", mrb_sprite_set_scale, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, texture, "pivot=", mrb_sprite_set_pivot, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, texture, "src_rect=", mrb_sprite_set_src_rect, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, texture, "color=", mrb_sprite_set_color, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, texture, "angle=", mrb_sprite_set_angle, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, texture, "update", mrb_sprite_update, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, texture, "draw", mrb_sprite_draw, MRB_ARGS_NONE());

  mrb_define_method(mrb, texture, "dispose", mrb_sprite_dispose, MRB_ARGS_NONE());
}

struct mrb_Sprite *
mrb_carbuncle_get_sprite(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &sprite_data_type, struct mrb_Sprite);
}

mrb_bool
mrb_carbuncle_sprite_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &sprite_data_type);
}