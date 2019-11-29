#include "carbuncle/core.h"
#include "carbuncle/point.h"

#include <mruby/array.h>
#include <mruby/data.h>

#include "raylib.h"

static const struct mrb_data_type point_data_type = {
  "Carbuncle::Point", mrb_free
};

static mrb_value
set_point_values(mrb_state *mrb, mrb_value self, mrb_bool from_initialize)
{
  mrb_int argc, x, y;
  mrb_value first;
  argc = mrb_get_args(mrb, "|oi", &first, &y);
  Vector2 *data = DATA_GET_PTR(mrb, self, &point_data_type, Vector2);
  switch (argc)
  {
    case 0: {
      if (!from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "1 or 2", argc);
      }
      data->x = 0;
      data->y = 0;
      break;
    }
    case 1: {
      Vector2 *point = mrb_carbuncle_get_point(mrb, first);
      data->x = point->x;
      data->y = point->y;      
      break;
    }
    case 2: {
      mrb_int x = mrb_fixnum(first);
      data->x = x;
      data->y = y;
      break;
    }
    default: {
      if (from_initialize)
      {
        mrb_carbuncle_arg_error(mrb, "0, 1 or 2", argc);
      }
      else
      {
        mrb_carbuncle_arg_error(mrb, "1 or 2", argc);
      }
      break;
    }
  }  
  return self;
}

static void
copy_struct(mrb_state *mrb, mrb_value self, mrb_value clone)
{
  Vector2 *data = mrb_malloc(mrb, sizeof *data);
  Vector2 *clone_data = mrb_carbuncle_get_point(mrb, clone);
  DATA_TYPE(clone) = &point_data_type;
  DATA_PTR(clone) = data;
  *clone_data = *data;
}

static mrb_value
mrb_point_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  Vector2 *data = mrb_malloc(mrb, sizeof *data);
  DATA_TYPE(self) = &point_data_type;
  DATA_PTR(self) = data;
  return set_point_values(mrb, self, TRUE);
}

static mrb_value
mrb_point_get_x(mrb_state *mrb, mrb_value self)
{
  Vector2 *point = mrb_carbuncle_get_point(mrb, self);
  return mrb_fixnum_value(point->x);
}

static mrb_value
mrb_point_get_y(mrb_state *mrb, mrb_value self)
{
  Vector2 *point = mrb_carbuncle_get_point(mrb, self);
  return mrb_fixnum_value(point->y);
}

static mrb_value
mrb_point_set_x(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float x;
  mrb_get_args(mrb, "f", &x);
  Vector2 *point = mrb_carbuncle_get_point(mrb, self);
  point->x = x;
  return mrb_fixnum_value(x);
}

static mrb_value
mrb_point_set_y(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_float y;
  mrb_get_args(mrb, "f", &y);
  Vector2 *point = mrb_carbuncle_get_point(mrb, self);
  point->y = y;
  return mrb_fixnum_value(y);
}

static mrb_value
mrb_point_set(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_check_frozen(mrb, self);
  return set_point_values(mrb, self, FALSE);
}

static mrb_value
mrb_point_dup(mrb_state *mrb, mrb_value self)
{
  mrb_value dup = mrb_obj_dup(mrb, self);
  copy_struct(mrb, self, dup);
  return dup;
}

static mrb_value
mrb_point_clone(mrb_state *mrb, mrb_value self)
{
  mrb_value dup = mrb_obj_clone(mrb, self);
  copy_struct(mrb, self, dup);
  return dup;
}


void
mrb_carbuncle_point_init(mrb_state *mrb)
{
  struct RClass *point = mrb_carbuncle_define_data_class(mrb, "Point", mrb->object_class);

  mrb_define_method(mrb, point, "initialize", mrb_point_initialize, MRB_ARGS_OPT(2));

  mrb_define_method(mrb, point, "x", mrb_point_get_x, MRB_ARGS_NONE());
  mrb_define_method(mrb, point, "y", mrb_point_get_y, MRB_ARGS_NONE());

  mrb_define_method(mrb, point, "x=", mrb_point_set_x, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, point, "y=", mrb_point_set_y, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, point, "set", mrb_point_set, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));

  mrb_define_method(mrb, point, "dup", mrb_point_dup, MRB_ARGS_NONE());
  mrb_define_method(mrb, point, "clone", mrb_point_clone, MRB_ARGS_NONE());

  mrb_value empty_point = mrb_obj_freeze(mrb, mrb_obj_new(mrb, point, 0, NULL));
  mrb_define_const(mrb, point, "EMPTY", empty_point);
}

Vector2 *
mrb_carbuncle_get_point(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &point_data_type, Vector2);
}

mrb_bool
mrb_carbuncle_point_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &point_data_type);
}
