#include "carbuncle/core.h"
#include "carbuncle/point.h"
#include "carbuncle/rect.h"
#include "carbuncle/color.h"
#include "carbuncle/graphics.h"

#include "raylib.h"

#include <mruby/variable.h>

#define COLOR_SYMBOL mrb_intern_cstr(mrb, "#color")
#define END_COLOR_SYMBOL mrb_intern_cstr(mrb, "#end_color")

static mrb_int carbuncle_target_fps = 0;

static Color *
get_graphics_color(mrb_state *mrb, mrb_value self)
{
  return mrb_carbuncle_get_color(mrb, mrb_cv_get(mrb, self, COLOR_SYMBOL));
}

static Color *
get_graphics_end_color(mrb_state *mrb, mrb_value self)
{
  return mrb_carbuncle_get_color(mrb, mrb_cv_get(mrb, self, END_COLOR_SYMBOL));
}

static mrb_value
mrb_s_graphics_get_fps(mrb_state *mrb, mrb_value self)
{
  return mrb_fixnum_value(GetFPS());
}

static mrb_value
mrb_s_graphics_get_time(mrb_state *mrb, mrb_value self)
{
  return mrb_float_value(mrb, GetTime());
}

static mrb_value
mrb_s_graphics_get_delta_time(mrb_state *mrb, mrb_value self)
{
  return mrb_float_value(mrb, GetFrameTime());
}

static mrb_value
mrb_s_graphics_get_target_fps(mrb_state *mrb, mrb_value self)
{
  return mrb_fixnum_value(carbuncle_target_fps);
}

static mrb_value
mrb_s_graphics_set_target_fps(mrb_state *mrb, mrb_value self)
{
  mrb_int target;
  mrb_get_args(mrb, "i", &target);
  target = target < 0 ? 0 : target;
  carbuncle_target_fps = target;
  SetTargetFPS(carbuncle_target_fps);
  return mrb_fixnum_value(carbuncle_target_fps);
}

static mrb_value
mrb_s_graphics_get_color(mrb_state *mrb, mrb_value self)
{
  return mrb_cv_get(mrb, self, COLOR_SYMBOL);
}

static mrb_value
mrb_s_graphics_get_end_color(mrb_state *mrb, mrb_value self)
{
  return mrb_cv_get(mrb, self, END_COLOR_SYMBOL);
}

static mrb_value
mrb_s_graphics_set_color(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  mrb_carbuncle_get_color(mrb, value);
  mrb_cv_set(mrb, self, COLOR_SYMBOL, value);
  return value;
}

static mrb_value
mrb_s_graphics_set_end_color(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_get_args(mrb, "o", &value);
  mrb_carbuncle_get_color(mrb, value);
  mrb_cv_set(mrb, self, END_COLOR_SYMBOL, value);
  return value;
}

static mrb_value
mrb_s_graphics_draw_pixel(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    case 1: {
      mrb_value arg1, arg2;
      Vector2 *point;
      mrb_get_args(mrb, "o", &arg1);
      point = mrb_carbuncle_get_point(mrb, arg1);
      DrawPixelV(*point, *color);
      break;
    }
    case 2: {
      mrb_int x, y;
      mrb_get_args(mrb, "ii", &x, &y);
      DrawPixel(x, y, *color);
      break;
    }
    default: {
      mrb_carbuncle_arg_error(mrb, "1 or 2", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_line(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    case 2: {
      mrb_value arg1, arg2;
      Vector2 *p1, *p2;
      mrb_get_args(mrb, "oo", &arg1, &arg2);
      p1 = mrb_carbuncle_get_point(mrb, arg1);
      p2 = mrb_carbuncle_get_point(mrb, arg2);
      DrawLineV(*p1, *p2, *color);
      break;
    }
    case 4: {
      mrb_int x1, y1, x2, y2;
      mrb_get_args(mrb, "iiii", &x1, &y1, &x2, &y2);
      DrawLine(x1, y1, x2, y2, *color);
      break;
    }
    default: {
      mrb_carbuncle_arg_error(mrb, "3 or 5", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_bezier(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_lines(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_circle(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_circle_gradient(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_arc(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_ring(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_rect(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_rect_gradient(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_triangle(mrb_state *mrb, mrb_value self)
{
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    default: {
      mrb_carbuncle_arg_error(mrb, "", argc);
    }
  }
  return self;
}

void
mrb_carbuncle_graphics_init(mrb_state *mrb)
{
  struct RClass *graphics = mrb_define_module_under(mrb, mrb_carbuncle_get(mrb), "Graphics");

  // Time functions
  mrb_define_module_method(mrb, graphics, "fps", mrb_s_graphics_get_fps, MRB_ARGS_NONE());
  mrb_define_module_method(mrb, graphics, "time", mrb_s_graphics_get_time, MRB_ARGS_NONE());
  mrb_define_module_method(mrb, graphics, "delta_time", mrb_s_graphics_get_delta_time, MRB_ARGS_NONE());
  mrb_define_module_method(mrb, graphics, "target_fps", mrb_s_graphics_get_target_fps, MRB_ARGS_NONE());

  mrb_define_module_method(mrb, graphics, "target_fps=", mrb_s_graphics_set_target_fps, MRB_ARGS_REQ(1));

  // Drawing functions
  mrb_cv_set(mrb, mrb_obj_value(graphics), COLOR_SYMBOL, mrb_carbuncle_color_new(mrb, 255, 255, 255, 255));

  mrb_define_module_method(mrb, graphics, "color", mrb_s_graphics_get_color, MRB_ARGS_NONE());
  mrb_define_module_method(mrb, graphics, "start_color", mrb_s_graphics_get_color, MRB_ARGS_NONE());
  mrb_define_module_method(mrb, graphics, "end_color", mrb_s_graphics_get_end_color, MRB_ARGS_NONE());

  mrb_define_module_method(mrb, graphics, "color=", mrb_s_graphics_set_color, MRB_ARGS_REQ(1));
  mrb_define_module_method(mrb, graphics, "start_color=", mrb_s_graphics_set_color, MRB_ARGS_REQ(1));
  mrb_define_module_method(mrb, graphics, "end_color=", mrb_s_graphics_set_end_color, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, graphics, "draw_pixel", mrb_s_graphics_draw_pixel, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, graphics, "draw_line", mrb_s_graphics_draw_line, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(2));
  mrb_define_module_function(mrb, graphics, "draw_bezier", mrb_s_graphics_draw_bezier, MRB_ARGS_REQ(3));
  mrb_define_module_function(mrb, graphics, "draw_lines", mrb_s_graphics_draw_lines, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "draw_circle", mrb_s_graphics_draw_circle, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, graphics, "draw_circle_gradient", mrb_s_graphics_draw_circle_gradient, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, graphics, "draw_gradient_circle", mrb_s_graphics_draw_circle_gradient, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, graphics, "draw_arc", mrb_s_graphics_draw_arc, MRB_ARGS_REQ(5)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, graphics, "draw_ring", mrb_s_graphics_draw_ring, MRB_ARGS_REQ(6)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, graphics, "draw_rect", mrb_s_graphics_draw_rect, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(3));
  mrb_define_module_function(mrb, graphics, "draw_rect_gradient", mrb_s_graphics_draw_rect_gradient, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(4));
  mrb_define_module_function(mrb, graphics, "draw_gradient_rect", mrb_s_graphics_draw_rect_gradient, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(4));
  mrb_define_module_function(mrb, graphics, "draw_triangle", mrb_s_graphics_draw_triangle, MRB_ARGS_REQ(3));
}
