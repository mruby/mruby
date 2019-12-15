#include "carbuncle/core.h"
#include "carbuncle/viewport.h"
#include "carbuncle/rect.h"
#include "carbuncle/color.h"

#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/array.h>

#include "raylib.h"
#include "rlgl.h"

#define CHILDREN_SYMBOL mrb_intern_cstr(mrb, "@children")
#define RECT_SYMBOL mrb_intern_cstr(mrb, "#rect")
#define COLOR_SYMBOL mrb_intern_cstr(mrb, "#color")

mrb_value
mrb_viewport_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value rect = mrb_carbuncle_rect_new(mrb, 0, 0, GetScreenWidth(), GetScreenHeight());
  mrb_iv_set(mrb, self, RECT_SYMBOL, rect);
  mrb_iv_set(mrb, self, CHILDREN_SYMBOL, mrb_ary_new(mrb));
  mrb_iv_set(mrb, self, COLOR_SYMBOL, mrb_carbuncle_color_new(mrb, 255, 255, 255, 255));
  return self;
}

mrb_value
mrb_viewport_get_rect(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, RECT_SYMBOL);
}

mrb_value
mrb_viewport_get_color(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, COLOR_SYMBOL);
}

mrb_value
mrb_viewport_set_rect(mrb_state *mrb, mrb_value self)
{
  mrb_value rect;
  mrb_get_args(mrb, "o",  &rect);
  mrb_carbuncle_get_rect(mrb, rect);
  mrb_iv_set(mrb, self, RECT_SYMBOL, rect);
  return rect;
}

mrb_value
mrb_viewport_set_color(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o",  &value);
  mrb_carbuncle_get_color(mrb, value);
  mrb_iv_set(mrb, self, COLOR_SYMBOL, value);
  return value;
}

mrb_value
mrb_viewport_draw(mrb_state *mrb, mrb_value self)
{
  mrb_value children = mrb_iv_get(mrb, self, CHILDREN_SYMBOL);
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, mrb_iv_get(mrb, self, RECT_SYMBOL));
  Color *color = mrb_carbuncle_get_color(mrb, mrb_iv_get(mrb, self, COLOR_SYMBOL));
  BeginScissorMode(rect->x, rect->y, rect->width, rect->height);
  for (size_t i = 0; i < RARRAY_LEN(children);  ++i)
  {
    mrb_funcall(mrb, mrb_ary_entry(children, i), "draw", 0);
  }
  EndScissorMode();
  return self;
}

void
mrb_init_carbuncle_viewport(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *container = mrb_define_class_under(mrb, carbuncle, "Container", mrb->object_class);
  struct RClass *viewport = mrb_define_class_under(mrb, carbuncle, "Viewport", container);

  mrb_define_method(mrb, viewport, "initialize", mrb_viewport_initialize, MRB_ARGS_NONE());

  mrb_define_method(mrb, viewport, "rect", mrb_viewport_get_rect, MRB_ARGS_NONE());
  mrb_define_method(mrb, viewport, "color", mrb_viewport_get_color, MRB_ARGS_NONE());

  mrb_define_method(mrb, viewport, "rect=", mrb_viewport_set_rect, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, viewport, "color=", mrb_viewport_set_color, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, viewport, "draw", mrb_viewport_draw, MRB_ARGS_NONE());
}
