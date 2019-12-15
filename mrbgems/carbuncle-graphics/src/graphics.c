#include "carbuncle/core.h"
#include "carbuncle/point.h"
#include "carbuncle/rect.h"
#include "carbuncle/color.h"
#include "carbuncle/matrix.h"
#include "carbuncle/graphics.h"

#include "raylib.h"
#include "rlgl.h"

#include <mruby.h>
#include <mruby/variable.h>
#include <mruby/string.h>
#include <mruby/numeric.h>

#define COLOR_SYMBOL mrb_intern_cstr(mrb, "#color")
#define END_COLOR_SYMBOL mrb_intern_cstr(mrb, "#end_color")

#define MODE_FILL   0
#define MODE_GRADIENT 2
#define MODE_STROKE 1

static mrb_int carbuncle_graphics_style = 0;
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
mrb_s_graphics_get_style(mrb_state *mrb, mrb_value self)
{
  return mrb_fixnum_value(carbuncle_graphics_style);
}

static mrb_value
mrb_s_graphics_get_model_view(mrb_state *mrb, mrb_value self)
{
  mrb_value matrix = mrb_obj_new(mrb, mrb_carbuncle_class_get(mrb, "Matrix"), 0, NULL);
  Matrix *data = mrb_carbuncle_get_matrix(mrb, matrix);
  *data = GetMatrixModelview();
  return matrix;
}

static mrb_value
mrb_s_graphics_get_shader_version(mrb_state *mrb, mrb_value self)
{
#if (defined(__linux__) && !defined(__ANDROID__)) || defined(__APPLE__) || defined(_WIN32)
  return mrb_fixnum_value(330);
#else
  return mrb_fixnum_value(100);
#endif
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
mrb_s_graphics_set_style(mrb_state *mrb, mrb_value self)
{
  mrb_value first;
  mrb_get_args(mrb, "o", &first);
  if (mrb_fixnum_p(first) || mrb_float_p(first))
  {
    mrb_int mode = mrb_fixnum(mrb_to_int(mrb, first));
    if (mode == MODE_FILL || mode == MODE_GRADIENT || mode == MODE_STROKE)
    {
      carbuncle_graphics_style = mode;
    }
    return first;
  }
  mrb_value str = mrb_funcall(mrb, mrb_to_str(mrb, first), "upcase", 0);
  mrb_sym cv = mrb_intern_str(mrb, str);
  if (mrb_const_defined(mrb, self, cv))
  {
    carbuncle_graphics_style = mrb_fixnum(mrb_cv_get(mrb, self, cv));
  }
  else
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "Unknown Graphics style '%s'.", mrb_str_to_cstr(mrb, str));
  }
  return first;
}

static mrb_value
mrb_s_graphics_set_model_view(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_get_args(mrb, "o", &obj);
  Matrix *matrix = mrb_carbuncle_get_matrix(mrb, obj);
  SetMatrixModelview(*matrix);
  return obj;
}

static mrb_value
mrb_s_graphics_draw_pixel(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    case 1:
    {
      mrb_value arg1;
      Vector2 *point;
      mrb_get_args(mrb, "o", &arg1);
      point = mrb_carbuncle_get_point(mrb, arg1);
      DrawPixelV(*point, *color);
      break;
    }
    case 2:
    {
      mrb_int x, y;
      mrb_get_args(mrb, "ii", &x, &y);
      DrawPixel(x, y, *color);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "1 or 2", argc); }
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
    case 2:
    {
      mrb_value arg1, arg2;
      Vector2 *p1, *p2;
      mrb_get_args(mrb, "oo", &arg1, &arg2);
      p1 = mrb_carbuncle_get_point(mrb, arg1);
      p2 = mrb_carbuncle_get_point(mrb, arg2);
      DrawLineV(*p1, *p2, *color);
      break;
    }
    case 4:
    {
      mrb_int x1, y1, x2, y2;
      mrb_get_args(mrb, "iiii", &x1, &y1, &x2, &y2);
      DrawLine(x1, y1, x2, y2, *color);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "3 or 5", argc); }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_bezier(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  mrb_int argc = mrb_get_argc(mrb); 
  switch (argc)
  {
    case 2:
    {
      mrb_value arg1, arg2;
      Vector2 *p1, *p2;
      mrb_get_args(mrb, "oo", &arg1, &arg2);
      p1 = mrb_carbuncle_get_point(mrb, arg1);
      p2 = mrb_carbuncle_get_point(mrb, arg2);
      DrawLineBezier(*p1, *p2, 1, *color);
      break;
    }
    case 3:
    {
      mrb_value arg1, arg2;
      Vector2 *p1, *p2;
      mrb_float thick;
      mrb_get_args(mrb, "oof", &arg1, &arg2, &thick);
      p1 = mrb_carbuncle_get_point(mrb, arg1);
      p2 = mrb_carbuncle_get_point(mrb, arg2);
      DrawLineBezier(*p1, *p2, thick, *color);
      break;
    }
    case 4:
    {
      mrb_float x1, y1, x2, y2;
      mrb_get_args(mrb, "ffff", &x1, &y1, &x2, &y2);
      Vector2 p1 = (Vector2){x1, y1};
      Vector2 p2 = (Vector2){x2, y2};
      DrawLineBezier(p1, p2, 1, *color);
      break;
    }
    case 5:
    {
      mrb_float x1, y1, x2, y2, thick;
      mrb_get_args(mrb, "fffff", &x1, &y1, &x2, &y2, &thick);
      Vector2 p1 = (Vector2){x1, y1};
      Vector2 p2 = (Vector2){x2, y2};
      DrawLineBezier(p1, p2, thick, *color);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "2, 3, 4 or 5", argc); }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_lines(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  mrb_value *points;
  mrb_int size;
  mrb_get_args(mrb, "*", &points, &size);
  if (size > 0)
  {
    Vector2 lines[size];
    for (mrb_int i = 0; i < size; ++i)
    {
      Vector2 *data = mrb_carbuncle_get_point(mrb, points[i]);
      lines[i] = *data;
    }
    DrawLineStrip(lines, size, *color);
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_circle(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  mrb_int argc = mrb_get_argc(mrb); 
  mrb_float x, y, r;
  switch (argc)
  {
    case 2:
    {
      mrb_value arg1;
      Vector2 *point;
      mrb_get_args(mrb, "of", &arg1, &r);
      point = mrb_carbuncle_get_point(mrb, arg1);
      x = point->x; y = point->y;
      break;
    }
    case 3:
    {
      mrb_get_args(mrb, "fff", &x, &y, &r);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "2 or 3", argc); }
  }
  switch (carbuncle_graphics_style)
  {
    case MODE_GRADIENT:
    {
      Color *end = get_graphics_end_color(mrb, self);
      DrawCircleGradient(x, y, r, *color, *end);
      break;
    }
    case MODE_STROKE:
    {
      DrawCircleLines(x, y, r, *color);
      break;
    }
    default:
    {
      DrawCircle(x, y, r, *color);
      break;
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_arc(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  mrb_int argc = mrb_get_argc(mrb); 
  Vector2 pos;
  mrb_float r, start, end, segments;
  switch (argc)
  {
    case 4:
    {
      mrb_value arg1;
      mrb_get_args(mrb, "offf", &arg1, &r, &start, &end);
      segments = 100;
      pos = *mrb_carbuncle_get_point(mrb, arg1);
      break;
    }
    case 5:
    {
      mrb_value arg1;
      mrb_float arg2, arg3, arg4, arg5;
      mrb_get_args(mrb, "offff", &arg1, &arg2, &arg3, &arg4, &arg5);
      if (mrb_carbuncle_point_p(arg1))
      {
        pos = *mrb_carbuncle_get_point(mrb, arg1);
        r = arg2;
        start = arg3;
        end = arg4;
        segments = arg5;
      }
      else
      {
        pos.x = mrb_to_flo(mrb, arg1);
        pos.y = arg2;
        r = arg3;
        start = arg4;
        end = arg5;
        segments = 100;
      }
      break;
    }
    case 6:
    {
      mrb_float x, y;
      mrb_get_args(mrb, "ffffff", &x, &y, &r, &start, &end, &segments);
      pos.x = x; pos.y = y;
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "4, 5 or 6", argc); }
  }
  switch (carbuncle_graphics_style)
  {
    case MODE_STROKE:
    {
      DrawCircleSectorLines(pos, r, start, end, segments, *color);
      break;
    }
    default:
    {
      DrawCircleSector(pos, r, start, end, segments, *color);
      break;
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_ring(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  mrb_int argc = mrb_get_argc(mrb);
  mrb_float inner, outer, start, end, segments;
  Vector2 pos;
  start = 0; end = 360; segments = 100;
  switch (argc)
  {
    case 3:
    {
      mrb_value obj;
      mrb_get_args(mrb, "off", &obj, &inner, &outer);
      pos = *mrb_carbuncle_get_point(mrb, obj);
      break;
    }
    case 4:
    {
      mrb_float x, y;
      mrb_get_args(mrb, "ffff", &x, &y, &inner, &outer);
      pos.x = x; pos.y = y;
      break;
    }
    case 5:
    {
      mrb_value arg1;
      mrb_float arg2, arg3, arg4, arg5;
      mrb_get_args(mrb, "offff", &arg1, &arg2, &arg3, &arg4, &arg5);
      pos = *mrb_carbuncle_get_point(mrb, arg1);
      inner = arg2;
      outer = arg3;
      start = arg4;
      end   = arg5;
      break;
    }
    case 6:
    {
      mrb_value arg1;
      mrb_float arg2, arg3, arg4, arg5, arg6;
      mrb_get_args(mrb, "offfff", &arg1, &arg2, &arg3, &arg4, &arg5, &arg6);
      if (mrb_carbuncle_point_p(arg1))
      {
        pos = *mrb_carbuncle_get_point(mrb, arg1);
        inner = arg2;
        outer = arg3;
        start = arg4;
        end   = arg5;
        segments = arg6;
      }
      else
      {
        pos.x = mrb_to_flo(mrb, arg1);
        pos.y = arg2;
        inner = arg3;
        outer = arg4;
        start = arg5;
        end   = arg6;
      }
      break;
    }
    case 7:
    {
      mrb_float x, y;
      mrb_get_args(mrb, "fffffff", &x, &y, &inner, &outer, &start, &end, &segments);
      pos.x = x; pos.y = y;
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "3, 4, 5, 6 or 7", argc); }
  }
  switch (carbuncle_graphics_style)
  {
    case MODE_STROKE:
    {
      DrawRingLines(pos, inner, outer, start, end, segments, *color);
      break;
    }
    default:
    {
      DrawRing(pos, inner, outer, start, end, segments, *color);
      break;
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_rect(mrb_state *mrb, mrb_value self)
{
  mrb_float x, y, width, height;
  mrb_bool vertical = FALSE;
  mrb_int line_height = 1;
  const char *kw_names[2] = { "vertical", "line_height" };
  mrb_value kw_values[2] = { mrb_false_value(), mrb_float_value(mrb, 1) };
  const mrb_kwargs kwargs = { 2, kw_values, kw_names, 0, NULL };
  Color *color = get_graphics_color(mrb, self);
  mrb_int argc = mrb_get_argc(mrb);
  switch (argc)
  {
    case 1: case 2:
    {
      mrb_value value;
      Rectangle *rect;
      mrb_get_args(mrb, "o:", &value, &kwargs);
      rect = mrb_carbuncle_get_rect(mrb, value);
      x = rect->x; y = rect->y; width = rect->width; height = rect->height;
      vertical = mrb_test(kw_values[0]);
      line_height = mrb_to_flo(mrb, kw_values[1]);
      break;
    }
    case 4: case 5:
    {
      mrb_get_args(mrb, "ffff:", &x, &y, &width, &height, &kwargs);
      vertical = mrb_test(kw_values[0]);
      line_height = mrb_to_flo(mrb, kw_values[1]);
      break;
    }
    default: { mrb_carbuncle_arg_error(mrb, "1 or 4", argc);  }
  }
  switch (carbuncle_graphics_style)
  {
    case MODE_GRADIENT:
    {
      Color *end = get_graphics_end_color(mrb, self);
      if (vertical)
      {
        DrawRectangleGradientV(x, y, width, height, *color, *end);
      }
      else
      {
        DrawRectangleGradientH(x, y, width, height, *color, *end);
      }
      break;
    }
    case MODE_STROKE:
    {
      Rectangle rect = (Rectangle){ x, y, width, height };
      DrawRectangleLinesEx(rect, line_height, *color);
      break;
    }
    default:
    {
      DrawRectangle(x, y, width, height, *color);
      break;
    }
  }
  return self;
}

static mrb_value
mrb_s_graphics_draw_triangle(mrb_state *mrb, mrb_value self)
{
  Color *color = get_graphics_color(mrb, self);
  Vector2 p1, p2, p3;
  mrb_value arg1, arg2, arg3;
  mrb_get_args(mrb, "ooo", &arg1, &arg2, &arg3);
  p1 = *mrb_carbuncle_get_point(mrb, arg1);
  p2 = *mrb_carbuncle_get_point(mrb, arg2);
  p3 = *mrb_carbuncle_get_point(mrb, arg3);
  switch (carbuncle_graphics_style)
  {
    case MODE_STROKE:
    {
      DrawTriangleLines(p1, p2, p3, *color);
      break;
    }
    default:
    {
      DrawTriangle(p1, p2, p3, *color);
      break;
    }
  }
  return self;
}

void
mrb_init_carbuncle_graphics(mrb_state *mrb)
{
  struct RClass *graphics = mrb_define_module_under(mrb, mrb_carbuncle_get(mrb), "Graphics");

  carbuncle_graphics_style = MODE_FILL;

  // Time functions
  mrb_define_module_function(mrb, graphics, "fps", mrb_s_graphics_get_fps, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, graphics, "time", mrb_s_graphics_get_time, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, graphics, "delta_time", mrb_s_graphics_get_delta_time, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, graphics, "target_fps", mrb_s_graphics_get_target_fps, MRB_ARGS_NONE());

  mrb_define_module_function(mrb, graphics, "target_fps=", mrb_s_graphics_set_target_fps, MRB_ARGS_REQ(1));

  // Drawing functions
  mrb_cv_set(mrb, mrb_obj_value(graphics), COLOR_SYMBOL, mrb_carbuncle_color_new(mrb, 255, 255, 255, 255));

  mrb_define_module_function(mrb, graphics, "color", mrb_s_graphics_get_color, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, graphics, "start_color", mrb_s_graphics_get_color, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, graphics, "end_color", mrb_s_graphics_get_end_color, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, graphics, "style", mrb_s_graphics_get_style, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "mode", mrb_s_graphics_get_style, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "model_view", mrb_s_graphics_get_model_view, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, graphics, "modelview", mrb_s_graphics_get_model_view, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, graphics, "shader_version", mrb_s_graphics_get_shader_version, MRB_ARGS_NONE());
  
  mrb_define_module_function(mrb, graphics, "color=", mrb_s_graphics_set_color, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "start_color=", mrb_s_graphics_set_color, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "end_color=", mrb_s_graphics_set_end_color, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "style=", mrb_s_graphics_set_style, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "mode=", mrb_s_graphics_set_style, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "model_view=", mrb_s_graphics_set_model_view, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, graphics, "modelview=", mrb_s_graphics_set_model_view, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, graphics, "draw_pixel", mrb_s_graphics_draw_pixel, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, graphics, "draw_line", mrb_s_graphics_draw_line, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(2));
  mrb_define_module_function(mrb, graphics, "draw_bezier", mrb_s_graphics_draw_bezier, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(3));
  mrb_define_module_function(mrb, graphics, "draw_lines", mrb_s_graphics_draw_lines, MRB_ARGS_ANY());
  mrb_define_module_function(mrb, graphics, "draw_circle", mrb_s_graphics_draw_circle, MRB_ARGS_REQ(2)|MRB_ARGS_OPT(1));
  mrb_define_module_function(mrb, graphics, "draw_arc", mrb_s_graphics_draw_arc, MRB_ARGS_REQ(4)|MRB_ARGS_OPT(2));
  mrb_define_module_function(mrb, graphics, "draw_ring", mrb_s_graphics_draw_ring, MRB_ARGS_REQ(3)|MRB_ARGS_OPT(4));
  mrb_define_module_function(mrb, graphics, "draw_rect", mrb_s_graphics_draw_rect, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(3));
  mrb_define_module_function(mrb, graphics, "draw_triangle", mrb_s_graphics_draw_triangle, MRB_ARGS_REQ(3));

  mrb_define_const(mrb, graphics, "FILL", mrb_fixnum_value(MODE_FILL));
  mrb_define_const(mrb, graphics, "STROKE", mrb_fixnum_value(MODE_STROKE));
  mrb_define_const(mrb, graphics, "LINE", mrb_fixnum_value(MODE_STROKE));
  mrb_define_const(mrb, graphics, "GRADIENT", mrb_fixnum_value(MODE_GRADIENT));
  mrb_define_const(mrb, graphics, "GRADIENT_FILL", mrb_fixnum_value(MODE_GRADIENT));
  mrb_define_const(mrb, graphics, "FILL_GRADIENT", mrb_fixnum_value(MODE_GRADIENT));
}
