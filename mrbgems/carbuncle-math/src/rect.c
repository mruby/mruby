#include "carbuncle/core.h"
#include "carbuncle/rect.h"

#include <mruby/data.h>
#include <mruby/class.h>
#include <mruby/numeric.h>

static const struct mrb_data_type rect_data_type = {
  "Carbuncle::Rect", mrb_free
};

static void
validate_size(mrb_state *mrb, const char *name, mrb_float value)
{
  if (value < 0)
  {
    mrb_raisef(mrb, "rect %s must be a positive number.", name);
  }
}

static mrb_value
set_rect_values(mrb_state *mrb, mrb_value self, mrb_bool from_initialize)
{
  mrb_float y, width, height;
  mrb_value first;
  mrb_int argc = mrb_get_args(mrb, "|offf", &first, &y, &width, &height);
  Rectangle *data = mrb_carbuncle_get_rect(mrb, self);
  switch(argc)
  {
    case 0: {
      if (!from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "1 or 4", argc);
      }      
      break;
    }
    case 1: {
      Rectangle *rect = mrb_carbuncle_get_rect(mrb, first);
      *data = *rect;
      break;
    }
    case 4: {
      validate_size(mrb, "width", width);
      validate_size(mrb, "height", height);
      *data = (Rectangle){mrb_to_flo(mrb, first), y, width, height};
      break;
    }
    default: {
      if (from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "0, 1 or 4", argc);
      }
      else
      {
        mrb_carbuncle_arg_error(mrb, "1 or 4", argc);
      }      
      break;
    }
  }
  return self;
}

static mrb_value
mrb_rect_initialize(mrb_state *mrb, mrb_value self)
{
  Rectangle *data = mrb_malloc(mrb, sizeof *data);
  DATA_TYPE(self) = &rect_data_type;
  DATA_PTR(self) = data;
  return set_rect_values(mrb, self, TRUE);
}

static mrb_value
mrb_rect_get_x(mrb_state *mrb, mrb_value self)
{
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, self);
  return mrb_float_value(mrb, rect->x);
}

static mrb_value
mrb_rect_get_y(mrb_state *mrb, mrb_value self)
{
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, self);
  return mrb_float_value(mrb, rect->y);
}

static mrb_value
mrb_rect_get_width(mrb_state *mrb, mrb_value self)
{
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, self);
  return mrb_float_value(mrb, rect->width);
}

static mrb_value
mrb_rect_get_height(mrb_state *mrb, mrb_value self)
{
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, self);
  return mrb_float_value(mrb, rect->height);
}

static mrb_value
mrb_rect_set_x(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float x;
  mrb_get_args(mrb, "f", &x);
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, self);
  rect->x = x;
  return  mrb_float_value(mrb, x);  
}

static mrb_value
mrb_rect_set_y(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float y;
  mrb_get_args(mrb, "f", &y);
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, self);
  rect->y = y;
  return  mrb_float_value(mrb, y);  
}

static mrb_value
mrb_rect_set_width(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float width;
  mrb_get_args(mrb, "f", &width);
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, self);
  rect->width = width;
  return  mrb_float_value(mrb, width);  
}

static mrb_value
mrb_rect_set_height(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float height;
  mrb_get_args(mrb, "f", &height);
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, self);
  rect->height = height;
  return  mrb_float_value(mrb, height);  
}

static mrb_value
mrb_rect_set(mrb_state *mrb, mrb_value self)
{
  return set_rect_values(mrb, self, FALSE);
}

static mrb_value
mrb_rect_equal(mrb_state *mrb, mrb_value self)
{
  mrb_value other;
  mrb_get_args(mrb, "o", &other);
  if (!mrb_carbuncle_rect_p(other))
  {
    return mrb_false_value();
  }
  Rectangle *data = mrb_carbuncle_get_rect(mrb, self);
  Rectangle *rect = mrb_carbuncle_get_rect(mrb, other);
  return mrb_bool_value(
    data->x == rect->x &&
    data->y == rect->y &&
    data->width == rect->width &&
    data->height == rect->height
  );
}

void
mrb_init_carbuncle_rect(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *rect = mrb_define_class_under(mrb, carbuncle, "Rect", mrb->object_class);
  MRB_SET_INSTANCE_TT(rect, MRB_TT_DATA);

  mrb_define_method(mrb, rect, "initialize", mrb_rect_initialize, MRB_ARGS_OPT(4));
  mrb_define_method(mrb, rect, "initialize_copy", mrb_rect_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, rect, "x", mrb_rect_get_x, MRB_ARGS_NONE());
  mrb_define_method(mrb, rect, "y", mrb_rect_get_y, MRB_ARGS_NONE());
  mrb_define_method(mrb, rect, "width", mrb_rect_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, rect, "height", mrb_rect_get_height, MRB_ARGS_NONE());
  mrb_define_method(mrb, rect, "w", mrb_rect_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, rect, "h", mrb_rect_get_height, MRB_ARGS_NONE());

  mrb_define_method(mrb, rect, "x=", mrb_rect_set_x, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, rect, "y=", mrb_rect_set_y, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, rect, "width=", mrb_rect_set_width, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, rect, "height=", mrb_rect_set_height, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, rect, "w=", mrb_rect_set_width, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, rect, "h=", mrb_rect_set_height, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, rect, "set", mrb_rect_set, MRB_ARGS_OPT(4));

  mrb_define_method(mrb, rect, "==", mrb_rect_equal, MRB_ARGS_REQ(1));

  mrb_value empty_rect = mrb_obj_freeze(mrb, mrb_obj_new(mrb, rect, 0, NULL));
  mrb_define_const(mrb, rect, "EMPTY", empty_rect);
}

Rectangle *
mrb_carbuncle_get_rect(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &rect_data_type, Rectangle);
}

mrb_bool
mrb_carbuncle_rect_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &rect_data_type);
}

mrb_value
mrb_carbuncle_rect_new(mrb_state *mrb, mrb_float x, mrb_float y, mrb_float w, mrb_float h)
{
  struct RClass *point = mrb_carbuncle_class_get(mrb, "Rect");
  mrb_value args[4] = {
    mrb_float_value(mrb, x),
    mrb_float_value(mrb, y),
    mrb_float_value(mrb, w),
    mrb_float_value(mrb, h)
  };
  return mrb_obj_new(mrb, point, 4, args);
}
