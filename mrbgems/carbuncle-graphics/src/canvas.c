#include "carbuncle/core.h"
#include "carbuncle/point.h"
#include "carbuncle/rect.h"
#include "carbuncle/color.h"
#include "carbuncle/sprite.h"
#include "carbuncle/canvas.h"

#include "raylib.h"
#include "rlgl.h"

#include <mruby/variable.h>
#include <mruby/error.h>

struct mrb_Canvas
{
  RenderTexture render;
  Vector2      *position;
  Vector2      *scale;
  Vector2      *pivot;
  Rectangle    *src_rect;
  Color        *color;
  mrb_float     angle;
};

#define POSITION_SYMBOL mrb_intern_cstr(mrb, "#position")
#define SCALE_SYMBOL mrb_intern_cstr(mrb, "#scale")
#define PIVOT_SYMBOL mrb_intern_cstr(mrb, "#pivot")
#define SRC_RECT_SYMBOL mrb_intern_cstr(mrb, "#src_rect")
#define COLOR_SYMBOL mrb_intern_cstr(mrb, "#color")

static void
mrb_canvas_free(mrb_state *mrb, void *ptr)
{
  if (ptr)
  {
    struct mrb_Canvas *canvas = ptr;
    UnloadRenderTexture(canvas->render);
    mrb_free(mrb, ptr);
  }
}

static const struct mrb_data_type canvas_data_type = {
  "Carbuncle::Canvas", mrb_canvas_free
};

static struct mrb_Canvas *
get_canvas(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &canvas_data_type, struct mrb_Canvas);
}

static mrb_value
mrb_canvas_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value position, scale, pivot, src_rect, color;
  mrb_int width, height;
  mrb_get_args(mrb, "ii", &width, &height);
  struct mrb_Canvas *data = mrb_malloc(mrb, sizeof *data);
  int arena = mrb_gc_arena_save(mrb);
  position = mrb_carbuncle_point_new(mrb, 0, 0);
  scale = mrb_carbuncle_point_new(mrb, 1, 1);
  pivot = mrb_carbuncle_point_new(mrb, 0, 0);
  src_rect =  mrb_carbuncle_rect_new(mrb, 0, 0, width, height);
  color = mrb_carbuncle_color_new(mrb, 255, 255, 255, 255);
  mrb_iv_set(mrb, self, POSITION_SYMBOL, position);
  mrb_iv_set(mrb, self, SCALE_SYMBOL, scale);
  mrb_iv_set(mrb, self, PIVOT_SYMBOL, pivot);
  mrb_iv_set(mrb, self, SRC_RECT_SYMBOL, src_rect);
  mrb_iv_set(mrb, self, COLOR_SYMBOL, color);
  data->position = mrb_carbuncle_get_point(mrb, position);
  data->scale = mrb_carbuncle_get_point(mrb, scale);
  data->pivot = mrb_carbuncle_get_point(mrb, pivot);
  data->src_rect = mrb_carbuncle_get_rect(mrb, src_rect);
  data->color = mrb_carbuncle_get_color(mrb, color);
  data->angle = 0;
  data->render = LoadRenderTexture(width, height);
  mrb_gc_arena_restore(mrb, arena);
  return self;
}

static mrb_value
mrb_canvas_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

static mrb_value
mrb_canvas_dispose(mrb_state *mrb, mrb_value self)
{
  struct mrb_Canvas *canvas = get_canvas(mrb, self);
  mrb_canvas_free(mrb, canvas);
  DATA_PTR(self) = NULL;
  return self;
}

static mrb_value
mrb_canvas_get_position(mrb_state *mrb, mrb_value self)
{
  get_canvas(mrb, self);
  return mrb_iv_get(mrb, self, POSITION_SYMBOL);
}

static mrb_value
mrb_canvas_get_scale(mrb_state *mrb, mrb_value self)
{
  get_canvas(mrb, self);
  return mrb_iv_get(mrb, self, SCALE_SYMBOL);
}

static mrb_value
mrb_canvas_get_pivot(mrb_state *mrb, mrb_value self)
{
  get_canvas(mrb, self);
  return mrb_iv_get(mrb, self, PIVOT_SYMBOL);
}

static mrb_value
mrb_canvas_get_src_rect(mrb_state *mrb, mrb_value self)
{
  get_canvas(mrb, self);
  return mrb_iv_get(mrb, self, SRC_RECT_SYMBOL);
}

static mrb_value
mrb_canvas_get_color(mrb_state *mrb, mrb_value self)
{
  get_canvas(mrb, self);
  return mrb_iv_get(mrb, self, COLOR_SYMBOL);
}

static mrb_value
mrb_canvas_get_angle(mrb_state *mrb, mrb_value self)
{
  struct mrb_Canvas *data = get_canvas(mrb, self);
  return mrb_float_value(mrb, data->angle);
}

static mrb_value
mrb_canvas_get_width(mrb_state *mrb, mrb_value self)
{
  struct mrb_Canvas *data = get_canvas(mrb, self);
  return mrb_float_value(mrb, data->src_rect->width);
}

static mrb_value
mrb_canvas_get_height(mrb_state *mrb, mrb_value self)
{
  struct mrb_Canvas *data = get_canvas(mrb, self);
  return mrb_float_value(mrb, data->src_rect->height);
}

static mrb_value
mrb_canvas_set_position(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Canvas *data = get_canvas(mrb, self);
  struct Vector2 *point = mrb_carbuncle_get_point(mrb, value);
  data->position = point;
  return value;
}

static mrb_value
mrb_canvas_set_scale(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Canvas *data = get_canvas(mrb, self);
  struct Vector2 *point = mrb_carbuncle_get_point(mrb, value);
  data->scale = point;
  return value;
}

static mrb_value
mrb_canvas_set_pivot(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Canvas *data = get_canvas(mrb, self);
  struct Vector2 *point = mrb_carbuncle_get_point(mrb, value);
  data->pivot = point;
  return value;
}

static mrb_value
mrb_canvas_set_src_rect(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Canvas *data = get_canvas(mrb, self);
  struct Rectangle *rect = mrb_carbuncle_get_rect(mrb, value);
  data->src_rect = rect;
  return value;
}

static mrb_value
mrb_canvas_set_color(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  struct mrb_Canvas *data = get_canvas(mrb, self);
  struct Color *color = mrb_carbuncle_get_color(mrb, value);
  data->color = color;
  return value;
}

static mrb_value
mrb_canvas_set_angle(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_Canvas *data = get_canvas(mrb, self);
  data->angle = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_canvas_update(mrb_state *mrb, mrb_value self)
{
  get_canvas(mrb, self);
  return self;
}

static mrb_value
mrb_canvas_begin_render(mrb_state *mrb, mrb_value self)
{
  struct mrb_Canvas *data = get_canvas(mrb, self);
  mrb_value block = mrb_nil_value();
  mrb_get_args(mrb, "|&", &block);
  BeginTextureMode(data->render);
  if (!mrb_nil_p(block))
  {
    mrb_bool raised = FALSE;
    mrb_value result = mrb_protect(mrb, mrb_carbuncle_call_block, block, &raised);
    EndTextureMode();
    if (raised) {
      mrb_exc_raise(mrb, result);
    }
  }
  return self;
}

static mrb_value
mrb_canvas_end_render(mrb_state *mrb, mrb_value self)
{
  EndTextureMode();
  return self;
}

static mrb_value
mrb_canvas_draw(mrb_state *mrb, mrb_value self)
{
  struct mrb_Canvas *data = get_canvas(mrb, self);
  Texture2D texture = data->render.texture;
  Rectangle src_rect = (Rectangle){
    data->src_rect->x,
    data->src_rect->y,
    data->src_rect->width  * (data->scale->x < 0 ? -1 : 1),
    data->src_rect->height * (data->scale->y < 0 ? -1 : 1)
  };
  Rectangle dst_rect = (Rectangle){
    data->position->x,
    data->position->y,
    data->src_rect->width * fabsf(data->scale->x),
    data->src_rect->height * fabsf(data->scale->y)
  };
  Vector2 origin = (Vector2){
    data->pivot->x * dst_rect.width,
    data->pivot->y * dst_rect.height
  };
  rlEnableTexture(texture.id);
  mrb_carbuncle_sprite_draw_texture(texture, src_rect, dst_rect, origin, data->angle, *(data->color));
  rlDisableTexture();
  return self;
}

void
mrb_init_carbuncle_canvas(mrb_state *mrb)
{
  struct RClass *canvas = mrb_carbuncle_define_data_class(mrb, "Canvas", mrb->object_class);

  mrb_define_method(mrb, canvas, "initialize", mrb_canvas_initialize, MRB_ARGS_REQ(2));

  mrb_define_method(mrb, canvas, "disposed?", mrb_canvas_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, canvas, "dispose", mrb_canvas_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, canvas, "src_rect", mrb_canvas_get_src_rect, MRB_ARGS_NONE());
  mrb_define_method(mrb, canvas, "position", mrb_canvas_get_position, MRB_ARGS_NONE());
  mrb_define_method(mrb, canvas, "color", mrb_canvas_get_color, MRB_ARGS_NONE());
  mrb_define_method(mrb, canvas, "scale", mrb_canvas_get_scale, MRB_ARGS_NONE());
  mrb_define_method(mrb, canvas, "pivot", mrb_canvas_get_pivot, MRB_ARGS_NONE());
  mrb_define_method(mrb, canvas, "width", mrb_canvas_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, canvas, "height", mrb_canvas_get_height, MRB_ARGS_NONE());

  mrb_define_method(mrb, canvas, "src_rect=", mrb_canvas_set_src_rect, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, canvas, "position=", mrb_canvas_set_position, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, canvas, "color=", mrb_canvas_set_color, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, canvas, "scale=", mrb_canvas_set_scale, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, canvas, "pivot=", mrb_canvas_set_pivot, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, canvas, "render", mrb_canvas_begin_render, MRB_ARGS_BLOCK());
  mrb_define_method(mrb, canvas, "begin_render", mrb_canvas_begin_render, MRB_ARGS_NONE());
  mrb_define_method(mrb, canvas, "end_render", mrb_canvas_end_render, MRB_ARGS_NONE());

  mrb_define_method(mrb, canvas, "update", mrb_canvas_update, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, canvas, "draw", mrb_canvas_draw, MRB_ARGS_NONE());
}
