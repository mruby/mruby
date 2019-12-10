#include "carbuncle/core.h"
#include "carbuncle/point.h"
#include "carbuncle/rect.h"
#include "carbuncle/texture.h"
#include "carbuncle/color.h"
#include "carbuncle/nine_patch.h"

#include "raylib.h"

#include <mruby/variable.h>

struct mrb_9Patch
{
  mrb_float  angle;
  mrb_float  width, height, left, top, right, bottom;
  Texture   *texture;
  Rectangle *src_rect;
  Vector2   *position;
  Vector2   *pivot;
  Color     *color;
};

static const struct mrb_data_type nine_patch_data_type = {
  "Carbuncle::NinePatch", mrb_free
};

#define TEXTURE_SYMBOL mrb_intern_cstr(mrb, "#texture")
#define POSITION_SYMBOL mrb_intern_cstr(mrb, "#position")
#define SRC_RECT_SYMBOL mrb_intern_cstr(mrb, "#src_rect")
#define PIVOT_SYMBOL mrb_intern_cstr(mrb, "#pivot")
#define COLOR_SYMBOL mrb_intern_cstr(mrb, "#color")

static struct mrb_Canvas *
get_nine_patch(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &nine_patch_data_type, struct mrb_9Patch);
}

static mrb_value
mrb_nine_patch_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value position, pivot, color, src_rect;
  mrb_value texture = mrb_nil_value();
  mrb_get_args(mrb, "|o", &texture);
  struct mrb_9Patch *patch = mrb_malloc(mrb, sizeof *patch);
  DATA_PTR(self) = patch;
  DATA_TYPE(self) = &nine_patch_data_type;
  int arena = mrb_gc_arena_save(mrb);
  if (mrb_nil_p(texture))
  {
    patch->texture = NULL;
    src_rect = mrb_carbuncle_rect_new(mrb, 0, 0, 1, 1);
    patch->width = patch->height = 1;
  }
  else
  {
    patch->texture = mrb_carbuncle_get_texture(mrb, texture);
    src_rect = mrb_carbuncle_rect_new(mrb, 0, 0, patch->texture->width, patch->texture->height);
    patch->width = patch->texture->width;
    patch->height = patch->texture->height;
  }
  color = mrb_carbuncle_color_new(mrb, 255, 255, 255, 255);
  pivot = mrb_carbuncle_point_new(mrb, 0, 0);
  position = mrb_carbuncle_point_new(mrb, 0, 0);
  patch->position = mrb_carbuncle_get_point(mrb, position);
  patch->pivot = mrb_carbuncle_get_point(mrb, pivot);
  patch->src_rect  = mrb_carbuncle_get_rect(mrb, src_rect);
  patch->color = mrb_carbuncle_get_color(mrb, color);
  patch->angle = patch->left = patch->top = patch->right = patch->bottom = 0;
  mrb_iv_set(mrb, self, TEXTURE_SYMBOL,  texture);
  mrb_iv_set(mrb, self, POSITION_SYMBOL, position);
  mrb_iv_set(mrb, self, PIVOT_SYMBOL, pivot);
  mrb_iv_set(mrb, self, COLOR_SYMBOL, color);
  mrb_iv_set(mrb, self, SRC_RECT_SYMBOL, src_rect);
  mrb_gc_arena_restore(mrb, arena);
  return self;
}

static mrb_value
mrb_nine_patch_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

static mrb_value
mrb_nine_patch_dispose(mrb_state *mrb, mrb_value self)
{
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  mrb_free(mrb, patch);
  DATA_PTR(self) = NULL;
  return self;
}

static mrb_value
mrb_nine_patch_get_texture(mrb_state *mrb, mrb_value self)
{
  get_nine_patch(mrb, self);
  return mrb_iv_get(mrb, self, TEXTURE_SYMBOL);
}

static mrb_value
mrb_nine_patch_get_position(mrb_state *mrb, mrb_value self)
{
  get_nine_patch(mrb, self);
  return mrb_iv_get(mrb, self, POSITION_SYMBOL);
}

static mrb_value
mrb_nine_patch_get_pivot(mrb_state *mrb, mrb_value self)
{
  get_nine_patch(mrb, self);
  return mrb_iv_get(mrb, self, PIVOT_SYMBOL);
}

static mrb_value
mrb_nine_patch_get_src_rect(mrb_state *mrb, mrb_value self)
{
  get_nine_patch(mrb, self);
  return mrb_iv_get(mrb, self, SRC_RECT_SYMBOL);
}

static mrb_value
mrb_nine_patch_get_color(mrb_state *mrb, mrb_value self)
{
  get_nine_patch(mrb, self);
  return mrb_iv_get(mrb, self, COLOR_SYMBOL);
}

static mrb_value
mrb_nine_patch_get_angle(mrb_state *mrb, mrb_value self)
{
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  return mrb_float_value(mrb, patch->angle);
}

static mrb_value
mrb_nine_patch_get_top(mrb_state *mrb, mrb_value self)
{
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  return mrb_float_value(mrb, patch->top);
}

static mrb_value
mrb_nine_patch_get_left(mrb_state *mrb, mrb_value self)
{
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  return mrb_float_value(mrb, patch->left);
}

static mrb_value
mrb_nine_patch_get_right(mrb_state *mrb, mrb_value self)
{
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  return mrb_float_value(mrb, patch->right);
}

static mrb_value
mrb_nine_patch_get_bottom(mrb_state *mrb, mrb_value self)
{
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  return mrb_float_value(mrb, patch->bottom);
}

static mrb_value
mrb_nine_patch_get_width(mrb_state *mrb, mrb_value self)
{
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  return mrb_float_value(mrb, patch->width);
}

static mrb_value
mrb_nine_patch_get_height(mrb_state *mrb, mrb_value self)
{
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  return mrb_float_value(mrb, patch->height);
}

static mrb_value
mrb_nine_patch_set_texture(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  mrb_get_args(mrb, "o", &value);
  if (mrb_nil_p(value))
  {
    data->texture = NULL;
  }
  else
  {
    data->texture = mrb_carbuncle_get_texture(mrb, value);
  }
  mrb_iv_set(mrb, self, TEXTURE_SYMBOL, value);
  return value;
}

static mrb_value
mrb_nine_patch_set_position(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  mrb_get_args(mrb, "o", &value);
  data->position = mrb_carbuncle_get_point(mrb, value);
  return value;
}

static mrb_value
mrb_nine_patch_set_pivot(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  mrb_get_args(mrb, "o", &value);
  data->pivot = mrb_carbuncle_get_point(mrb, value);
  return value;
}

static mrb_value
mrb_nine_patch_set_src_rect(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  mrb_get_args(mrb, "o", &value);
  data->src_rect = mrb_carbuncle_get_rect(mrb, value);
  return value;
}

static mrb_value
mrb_nine_patch_set_color(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  mrb_get_args(mrb, "o", &value);
  data->color = mrb_carbuncle_get_color(mrb, value);
  return value;
}


static mrb_value
mrb_nine_patch_set_angle(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  data->angle = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_nine_patch_set_top(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  data->top = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_nine_patch_set_left(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  data->left = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_nine_patch_set_right(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  data->right = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_nine_patch_set_bottom(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  data->bottom = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_nine_patch_set_width(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  if (value < 0) { mrb_raise(mrb, E_ARGUMENT_ERROR, "Width cannot be a negative number."); }
  data->width = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_nine_patch_set_height(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  struct mrb_9Patch *data = get_nine_patch(mrb, self);
  if (value < 0) { mrb_raise(mrb, E_ARGUMENT_ERROR, "Height cannot be a negative number."); }
  data->height = value;
  return mrb_float_value(mrb, value);
}

static mrb_value
mrb_nine_patch_draw(mrb_state *mrb, mrb_value self)
{
  NPatchInfo info;
  Rectangle dest;
  Vector2 offset;
  struct mrb_9Patch *patch = get_nine_patch(mrb, self);
  if (!patch->texture) {  return self; }
  info = (NPatchInfo){
    .sourceRec = *(patch->src_rect),
    .left = patch->left,
    .top = patch->top,
    .right = patch->right,
    .bottom = patch->bottom,
    .type = NPT_9PATCH
  };
  dest = (Rectangle){
    patch->position->x,
    patch->position->y,
    patch->width,
    patch->height
  };
  offset = (Vector2){
    patch->pivot->x * patch->width,
    patch->pivot->y * patch->height
  };
#ifdef CARBUNCLE_DEBUG
  mrb_carbuncle_draw_debug_rect(dest, offset, patch->angle);
#endif
  DrawTextureNPatch(*(patch->texture), info, dest, offset, patch->angle, *(patch->color));
  return self;
}

void
mrb_carbuncle_nine_patch_init(mrb_state *mrb)
{
  struct RClass *nine_patch = mrb_carbuncle_define_data_class(mrb, "NinePatch", mrb->object_class);

  mrb_define_method(mrb, nine_patch, "initialize", mrb_nine_patch_initialize, MRB_ARGS_OPT(1));

  mrb_define_method(mrb, nine_patch, "disposed?", mrb_nine_patch_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "dispose", mrb_nine_patch_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, nine_patch, "texture", mrb_nine_patch_get_texture, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "position", mrb_nine_patch_get_position, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "pivot", mrb_nine_patch_get_pivot, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "src_rect", mrb_nine_patch_get_src_rect, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "color", mrb_nine_patch_get_color, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "angle", mrb_nine_patch_get_angle, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "top", mrb_nine_patch_get_top, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "left", mrb_nine_patch_get_left, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "right", mrb_nine_patch_get_right, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "bottom", mrb_nine_patch_get_bottom, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "width", mrb_nine_patch_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, nine_patch, "height", mrb_nine_patch_get_height, MRB_ARGS_NONE());

  mrb_define_method(mrb, nine_patch, "texture=", mrb_nine_patch_set_texture, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "position=", mrb_nine_patch_set_position, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "pivot=", mrb_nine_patch_set_pivot, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "src_rect=", mrb_nine_patch_set_src_rect, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "color=", mrb_nine_patch_set_color, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "angle=", mrb_nine_patch_set_angle, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "top=", mrb_nine_patch_set_top, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "left=", mrb_nine_patch_set_left, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "right=", mrb_nine_patch_set_right, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "bottom=", mrb_nine_patch_set_bottom, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "width=", mrb_nine_patch_set_width, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, nine_patch, "height=", mrb_nine_patch_set_height, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, nine_patch, "draw", mrb_nine_patch_draw, MRB_ARGS_NONE());
}
