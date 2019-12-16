#include <mruby.h>

#include "raylib.h"

#include "carbuncle/core.h"
#include "carbuncle/touch.h"
#include "carbuncle/point.h"

#include <mruby/gc.h>
#include <mruby/string.h>
#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/class.h>

static mrb_int carbuncle_enabled_gestures = 0;

#define CARBUNCLE_GESTURES_COUNT 10

static const mrb_int CARBUNCLE_GESTURES[CARBUNCLE_GESTURES_COUNT] = {
  GESTURE_TAP,
  GESTURE_DOUBLETAP,
  GESTURE_HOLD,
  GESTURE_DRAG,
  GESTURE_SWIPE_RIGHT,
  GESTURE_SWIPE_LEFT,
  GESTURE_SWIPE_UP,
  GESTURE_SWIPE_DOWN,
  GESTURE_PINCH_IN,
  GESTURE_PINCH_OUT
};

static const char *CARBUNCLE_GESTURE_NAMES[CARBUNCLE_GESTURES_COUNT] = {
  "TAP",
  "DOUBLE_TAP",
  "HOLD",
  "DRAG",
  "SWIPE_RIGHT",
  "SWIPE_LEFT",
  "SWIPE_UP",
  "SWIPE_DOWN",
  "PINCH_IN",
  "PINCH_OUT"
};

#define HOLD_CLASS mrb_class_get_under(mrb, mrb_carbuncle_module_get(mrb, "Gesture"), "Hold")
#define DRAG_CLASS mrb_class_get_under(mrb, mrb_carbuncle_module_get(mrb, "Gesture"), "Drag")
#define PINCH_CLASS mrb_class_get_under(mrb, mrb_carbuncle_module_get(mrb, "Gesture"), "Pinch")

static inline mrb_int
get_gesture_value(mrb_state *mrb, mrb_value name)
{
  const char *gesture = mrb_str_to_cstr(mrb, name);
  mrb_sym gesture_symbol = mrb_intern_cstr(mrb, gesture);
  mrb_value gesture_class = mrb_obj_value(mrb_carbuncle_module_get(mrb, "Gesture"));
  if (!mrb_const_defined(mrb, gesture_class, gesture_symbol))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot find gesture '%s'.", gesture);
  }
  return mrb_fixnum(mrb_const_get(mrb, gesture_class, gesture_symbol));
}

static inline mrb_int
convert_gesture(mrb_state *mrb)
{
  mrb_value arg;
  mrb_get_args(mrb, "o", &arg);
  if (mrb_fixnum_p(arg) || mrb_float_p(arg))
  {
    return mrb_fixnum(mrb_to_int(mrb, arg));
  }
  else if (mrb_symbol_p(arg) || mrb_string_p(arg))
  {
    mrb_value string_value = mrb_funcall(mrb, arg, "to_s", 0);
    return get_gesture_value(mrb, mrb_funcall(mrb, string_value, "upcase", 0));
  }
  mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid gesture type, must be a symbol or an integer.");
  return -1;
}

static mrb_value
mrb_gesture_get_enabled(mrb_state *mrb, mrb_value self)
{
  mrb_value result;
  int arena = mrb_gc_arena_save(mrb);
  result = mrb_ary_new(mrb);
  for (mrb_int i = 0; i < CARBUNCLE_GESTURES_COUNT; ++i)
  {
    if (carbuncle_enabled_gestures & CARBUNCLE_GESTURES[i])
    {
      mrb_ary_push(mrb, result, mrb_str_new_cstr(mrb, CARBUNCLE_GESTURE_NAMES[i]));
    }
  }
  mrb_gc_arena_restore(mrb, arena);
  return result;
}

static mrb_value
mrb_gesture_enable(mrb_state *mrb, mrb_value self)
{
  mrb_int gesture = convert_gesture(mrb);
  carbuncle_enabled_gestures |= gesture;
  return self;
}

static mrb_value
mrb_gesture_get_hold(mrb_state *mrb, mrb_value self)
{
  mrb_value values[1] = {
    mrb_float_value(mrb, GetGestureHoldDuration())
  };
  return mrb_obj_new(mrb, HOLD_CLASS, 1, values);
}

static mrb_value
mrb_gesture_get_drag(mrb_state *mrb, mrb_value self)
{
  Vector2 vector = GetGestureDragVector();
  mrb_value values[2] = {
    mrb_carbuncle_point_new(mrb, vector.x, vector.y),
    mrb_float_value(mrb, GetGestureDragAngle())
  };
  return mrb_obj_new(mrb, DRAG_CLASS, 2, values);
}

static mrb_value
mrb_gesture_get_pinch(mrb_state *mrb, mrb_value self)
{
  Vector2 vector = GetGesturePinchVector();
  mrb_value values[2] = {
    mrb_carbuncle_point_new(mrb, vector.x, vector.y),
    mrb_float_value(mrb, GetGesturePinchAngle())
  };
  return mrb_obj_new(mrb, PINCH_CLASS, 2, values);
}


static mrb_value
mrb_gesture_disable(mrb_state *mrb, mrb_value self)
{
  mrb_int gesture = convert_gesture(mrb);
  carbuncle_enabled_gestures &= ~gesture;
  return self;
}

static mrb_value
mrb_gesture_enableQ(mrb_state *mrb, mrb_value self)
{
  mrb_int gesture = convert_gesture(mrb);
  return mrb_bool_value(carbuncle_enabled_gestures  & gesture);
}

static mrb_value
mrb_gesture_disableQ(mrb_state *mrb, mrb_value self)
{
  mrb_int gesture = convert_gesture(mrb);
  return mrb_bool_value(!(carbuncle_enabled_gestures  & gesture));
}

static mrb_value
mrb_gesture_detectQ(mrb_state *mrb, mrb_value self)
{
  mrb_int gesture = convert_gesture(mrb);
  return mrb_bool_value(IsGestureDetected(gesture));
}

void
mrb_init_carbuncle_gesture(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  /**
   * This class handles touch gestures.
   * Gestures like swipe, tap and hold are emulated via a mouse on desktop.
   * On touch screens, gestures are handled by the screen itself.
   * Gestures should be enable before use.
   */
  struct RClass *gesture = mrb_define_module_under(mrb, carbuncle, "Gesture");

  mrb_define_module_function(mrb, gesture, "enabled", mrb_gesture_get_enabled, MRB_ARGS_NONE());

  mrb_define_module_function(mrb, gesture, "hold", mrb_gesture_get_hold, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, gesture, "drag", mrb_gesture_get_drag, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, gesture, "pinch", mrb_gesture_get_pinch, MRB_ARGS_NONE());

  mrb_define_module_function(mrb, gesture, "enable", mrb_gesture_enable, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, gesture, "disable", mrb_gesture_disable, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, gesture, "enable?", mrb_gesture_enableQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, gesture, "disable?", mrb_gesture_disableQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, gesture, "detect?", mrb_gesture_detectQ, MRB_ARGS_REQ(1));

  for (mrb_int i = 0; i < CARBUNCLE_GESTURES_COUNT; ++i)
  {
    mrb_define_const(mrb, gesture, CARBUNCLE_GESTURE_NAMES[i], mrb_fixnum_value(CARBUNCLE_GESTURES[i]));
  }
}
