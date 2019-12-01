#include "carbuncle/core.h"
#include "carbuncle/color.h"

#include <mruby/data.h>

static mrb_int
max(mrb_int a, mrb_int b)
{
  return a > b ? a : b;
}

static mrb_int
min(mrb_int a, mrb_int b)
{
  return a < b ? a : b;
}

static const struct mrb_data_type color_data_type = {
  "Carbuncle::Color", mrb_free
};

static void
set_color(mrb_state *mrb, mrb_value self, mrb_bool from_initialize)
{
  mrb_value obj;
  mrb_int r, g, b, a, argc;
  Color *data = mrb_carbuncle_get_color(mrb, self);
  argc = mrb_get_args(mrb, "|oiii", &obj, &g, &b, &a);
  switch (argc)
  {
    case 0: {
      if (!from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "1, 3 or 4", argc);
      }
      data->r = data->g = data->b = 0;
      break;
    }
    case 1: {
      Color *color = mrb_carbuncle_get_color(mrb, obj);
      *data = *color;
      break;
    }
    case 3: {
      a = data->a;
    }
    case 4: {
      r = mrb_fixnum(mrb_to_int(mrb, obj));
      data->r = min(255, max(0, r));
      data->g = min(255, max(0, g));
      data->b = min(255, max(0, b));
      data->a = min(255, max(0, a));
      break;
    }
    default: {
      if (from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "0, 1, 3 or 4", argc);
      }
      else
      {
        mrb_carbuncle_arg_error(mrb, "1, 3 or 4", argc);
      }        
      break;
    }
  }
}

static mrb_value
mrb_color_initialize(mrb_state *mrb, mrb_value self)
{
  struct Color *color = mrb_malloc(mrb, sizeof *color);
  color->a = 255;
  DATA_PTR(self) = color;
  DATA_TYPE(self) = &color_data_type;
  set_color(mrb, self, TRUE);
  return self;
}

static mrb_value
mrb_color_get_red(mrb_state *mrb, mrb_value self)
{
  Color *color = mrb_carbuncle_get_color(mrb, self);
  return mrb_fixnum_value(color->r);
}

static mrb_value
mrb_color_get_green(mrb_state *mrb, mrb_value self)
{
  Color *color = mrb_carbuncle_get_color(mrb, self);
  return mrb_fixnum_value(color->g);
}

static mrb_value
mrb_color_get_blue(mrb_state *mrb, mrb_value self)
{
  Color *color = mrb_carbuncle_get_color(mrb, self);
  return mrb_fixnum_value(color->b);
}

static mrb_value
mrb_color_get_alpha(mrb_state *mrb, mrb_value self)
{
  Color *color = mrb_carbuncle_get_color(mrb, self);
  return mrb_fixnum_value(color->a);
}

static mrb_value
mrb_color_set_red(mrb_state *mrb, mrb_value self)
{
  mrb_int value;
  mrb_get_args(mrb, "i", &value);
  Color *color = mrb_carbuncle_get_color(mrb, self);
  color->r = min(255, max(0, value));
  return mrb_fixnum_value(color->r);
}

static mrb_value
mrb_color_set_green(mrb_state *mrb, mrb_value self)
{
  mrb_int value;
  mrb_get_args(mrb, "i", &value);
  Color *color = mrb_carbuncle_get_color(mrb, self);
  color->g = min(255, max(0, value));
  return mrb_fixnum_value(color->g);
}

static mrb_value
mrb_color_set_blue(mrb_state *mrb, mrb_value self)
{
  mrb_int value;
  mrb_get_args(mrb, "i", &value);
  Color *color = mrb_carbuncle_get_color(mrb, self);
  color->b = min(255, max(0, value));
  return mrb_fixnum_value(color->b);
}

static mrb_value
mrb_color_set_alpha(mrb_state *mrb, mrb_value self)
{
  mrb_int value;
  mrb_get_args(mrb, "i", &value);
  Color *color = mrb_carbuncle_get_color(mrb, self);
  color->a = min(255, max(0, value));
  return mrb_fixnum_value(color->a);
}

static mrb_value
mrb_color_grayscale(mrb_state *mrb, mrb_value self)
{
  return mrb_funcall(mrb, mrb_funcall(mrb, self, "dup", 0), "grayscale!", 0);
}

static mrb_value
mrb_color_invert(mrb_state *mrb, mrb_value self)
{
  return mrb_funcall(mrb, mrb_funcall(mrb, self, "dup", 0), "invert!", 0);
}

static mrb_value
mrb_color_grayscaleE(mrb_state *mrb, mrb_value self)
{
  mrb_float r, g, b;
  Color *color = mrb_carbuncle_get_color(mrb, self);
  r = ((mrb_float)color->r) * 0.299;
  g = ((mrb_float)color->g) * 0.587;
  b = ((mrb_float)color->b) * 0.114;
  mrb_int avg = max(0, min(255, r + g + b));
  color->r = color->g = color->b = avg;
  return self;
}

static mrb_value
mrb_color_invertE(mrb_state *mrb, mrb_value self)
{
  Color *color = mrb_carbuncle_get_color(mrb, self);
  color->r = 255 - color->r;
  color->g = 255 - color->g;
  color->b = 255 - color->b;
  return self;
}

static mrb_value
mrb_color_set(mrb_state *mrb, mrb_value self)
{
  set_color(mrb, self, FALSE);
  return self;
}

static mrb_value
mrb_color_equal(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  if (!mrb_carbuncle_color_p(other))
  {
    return mrb_false_value();
  }
  Color *data = mrb_carbuncle_get_color(mrb, self);
  Color *color = mrb_carbuncle_get_color(mrb, other);
  return mrb_bool_value(
    data->r == color->r &&
    data->g == color->g &&
    data->b == color->b &&
    data->a == color->a
  );
}

void
mrb_carbuncle_color_init(mrb_state *mrb)
{
  struct RClass *color = mrb_carbuncle_define_data_class(mrb, "Color", mrb->object_class);
  mrb_define_method(mrb, color, "initialize", mrb_color_initialize, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(3));
  mrb_define_method(mrb, color, "initialize_copy", mrb_color_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, color, "red", mrb_color_get_red, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "green", mrb_color_get_green, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "blue", mrb_color_get_blue, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "alpha", mrb_color_get_alpha, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "r", mrb_color_get_red, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "g", mrb_color_get_green, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "b", mrb_color_get_blue, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "a", mrb_color_get_alpha, MRB_ARGS_NONE());

  mrb_define_method(mrb, color, "red=", mrb_color_set_red, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, color, "green=", mrb_color_set_green, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, color, "blue=", mrb_color_set_blue, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, color, "alpha=", mrb_color_set_alpha, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, color, "r=", mrb_color_set_red, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, color, "g=", mrb_color_set_green, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, color, "b=", mrb_color_set_blue, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, color, "a=", mrb_color_set_alpha, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, color, "grayscale", mrb_color_grayscale, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "invert", mrb_color_invert, MRB_ARGS_NONE());

  mrb_define_method(mrb, color, "grayscale!", mrb_color_grayscaleE, MRB_ARGS_NONE());
  mrb_define_method(mrb, color, "invert!", mrb_color_invertE, MRB_ARGS_NONE());

  mrb_define_method(mrb, color, "set", mrb_color_set, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(3));

  mrb_define_method(mrb, color, "==", mrb_color_equal, MRB_ARGS_REQ(1));
}

Color *
mrb_carbuncle_get_color(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &color_data_type, Color);
}

mrb_bool
mrb_carbuncle_color_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &color_data_type);
}

mrb_value
mrb_carbuncle_color_new(mrb_state *mrb, mrb_int r, mrb_int g, mrb_int b, mrb_int a)
{
  struct RClass *point = mrb_carbuncle_class_get(mrb, "Color");
  mrb_value args[4] = {
    mrb_fixnum_value(r),
    mrb_fixnum_value(g),
    mrb_fixnum_value(b),
    mrb_fixnum_value(a)
  };
  return mrb_obj_new(mrb, point, 4, args);  
}