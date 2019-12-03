#include <mruby.h>

#include "raylib.h"

#include "carbuncle/core.h"
#include "carbuncle/touch.h"
#include "carbuncle/point.h"

#include <mruby/gc.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/class.h>

static mrb_value
mrb_touch_get_points(mrb_state *mrb, mrb_value self)
{
  mrb_value result, *values;
  mrb_int size = GetTouchPointsCount();
  if (size <= 0)
  {
    return mrb_ary_new(mrb);
  }
  int arena = mrb_gc_arena_save(mrb);
  values = mrb_alloca(mrb, (sizeof *values) * size);
  for (mrb_int i = 0; i < size; ++i)
  {
    Vector2 point = GetTouchPosition(i);
    values[i] = mrb_carbuncle_point_new(mrb, point.x, point.y);
  }
  result = mrb_ary_new_from_values(mrb, size, values);
  mrb_gc_arena_restore(mrb, arena);
  return result;
}

static mrb_value
mrb_touch_get_size(mrb_state *mrb, mrb_value self)
{
  return mrb_fixnum_value(GetTouchPointsCount());
}

static mrb_value
mrb_touch_subscript(mrb_state *mrb, mrb_value self)
{
  mrb_int index;
  mrb_int size = GetTouchPointsCount();
  mrb_get_args(mrb, "i", &index);
  if (size < 1)
  {
    return mrb_nil_value();
  }
  while (index < 0)
  {
    index = size - index;
  }
  if (index >= size)
  {
    return mrb_nil_value();
  }
  Vector2 point = GetTouchPosition(index);
  return mrb_carbuncle_point_new(mrb, point.x, point.y);
}

static mrb_value
mrb_touch_enable_gesture(mrb_state *mrb, mrb_value self)
{
}

static mrb_value
mrb_touch_disable_gesture(mrb_state *mrb, mrb_value self)
{
}

static mrb_value
mrb_touch_gesture_enableQ(mrb_state *mrb, mrb_value self)
{
}

static mrb_value
mrb_touch_gestureQ(mrb_state *mrb, mrb_value self)
{
}

void
mrb_carbuncle_touch_init(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *touch = mrb_define_module_under(mrb, carbuncle, "Touch");

  mrb_define_module_function(mrb, touch, "points", mrb_touch_get_points, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, touch, "size", mrb_touch_get_size, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, touch, "count", mrb_touch_get_size, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, touch, "length", mrb_touch_get_size, MRB_ARGS_NONE());

  mrb_define_module_function(mrb, touch, "[]", mrb_touch_subscript, MRB_ARGS_REQ(1));
}
