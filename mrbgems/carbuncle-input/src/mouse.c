#include <mruby.h>

#include "raylib.h"

#include "carbuncle/core.h"
#include "carbuncle/mouse.h"
#include "carbuncle/point.h"

#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/class.h>

static inline mrb_int
get_mouse_value(mrb_state *mrb, mrb_value name)
{
  const char *key = mrb_str_to_cstr(mrb, name);
  mrb_sym key_symbol = mrb_intern_cstr(mrb, key);
  mrb_value mouse_class = mrb_obj_value(mrb_carbuncle_module_get(mrb, "Mouse"));
  if (!mrb_const_defined(mrb, mouse_class, key_symbol))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot find Mouse button '%s'.", key);
  }
  return mrb_fixnum(mrb_const_get(mrb, mouse_class, key_symbol));
}

static inline mrb_int
convert_mouse_key(mrb_state *mrb)
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
    return get_mouse_value(mrb, mrb_funcall(mrb, string_value, "upcase", 0));
  }
  mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid button type, must be a symbol or an integer.");
  return -1;
}

/*
 * @overload press?(key)
 *   Checks if a button is being pressed on this frame.
 *   @param [Symbol, Integer, String] key The button to check.
 *   @return [Boolean]
 */
static mrb_value
mrb_mouse_pressQ(mrb_state *mrb, mrb_value self)
{
  mrb_int key = convert_mouse_key(mrb);
  return mrb_bool_value(IsMouseButtonPressed(key));
}


/*
 * @overload down?(key)
 *   Checks if a button is down.
 *   @param [Symbol, Integer, String] key The button to check.
 *   @return [Boolean]
 */
static mrb_value
mrb_mouse_downQ(mrb_state *mrb, mrb_value self)
{
  mrb_int key = convert_mouse_key(mrb);
  return mrb_bool_value(IsMouseButtonDown(key));
}


/*
 * @overload release?(key)
 *   Checks if a button is being released on this frame.
 *   @param [Symbol, Integer, String] key The button to check.
 *   @return [Boolean]
 */
static mrb_value
mrb_mouse_releaseQ(mrb_state *mrb, mrb_value self)
{
  mrb_int key = convert_mouse_key(mrb);
  return mrb_bool_value(IsMouseButtonReleased(key));
}

/*
 * @overload up?(key)
 *   Checks if a button is not being pressed.
 *   @param [Symbol, Integer, String] key The button to check.
 *   @return [Boolean]
 */
static mrb_value
mrb_mouse_upQ(mrb_state *mrb, mrb_value self)
{
  mrb_int key = convert_mouse_key(mrb);
  return mrb_bool_value(IsMouseButtonUp(key));
}

/*
 * Gets the x position for the mouse.
 * x positions are measured from the left side of the screen into the right.
 * @return [Integer]
 */
static mrb_value
mrb_mouse_get_x(mrb_state *mrb, mrb_value self)
{
  return mrb_fixnum_value(GetMouseX());
}

/*
 * Gets the y position for the mouse.
 * y positions are measured from the top of the screen into the bottom.
 * @return [Integer]
 */
static mrb_value
mrb_mouse_get_y(mrb_state *mrb, mrb_value self)
{
  return mrb_fixnum_value(GetMouseY());
}

/*
 * Gets the position for the mouse.
 * @return [Carbuncle::Point]
 */
static mrb_value
mrb_mouse_get_position(mrb_state *mrb, mrb_value self)
{
  return mrb_carbuncle_point_new(mrb, GetMouseX(), GetMouseY());
}

/*
 * Gets the movement for the mouse wheel.
 * -1 means up, 1 means down and 0 means no movement.
 * @return [Integer]
 */
static mrb_value
mrb_mouse_get_wheel_move(mrb_state *mrb, mrb_value self)
{
  return mrb_fixnum_value(GetMouseWheelMove());
}

/*
 * Sets the x coordinate of the mouse.
 * @return [Integer]
 */
static mrb_value
mrb_mouse_set_x(mrb_state *mrb, mrb_value self)
{
  mrb_int value;
  mrb_get_args(mrb, "i", &value);
  SetMousePosition(value, GetMouseY());
  return mrb_fixnum_value(value);
}

/*
 * Sets the y coordinate of the mouse.
 * @return [Integer]
 */
static mrb_value
mrb_mouse_set_y(mrb_state *mrb, mrb_value self)
{
  mrb_int value;
  mrb_get_args(mrb, "i", &value);
  SetMousePosition(GetMouseX(), value);
  return mrb_fixnum_value(value);
}

/*
 * @overload set_position(x, y)
 *   Sets the position of the mouse.
 *   @param [Integer] x The x coordinate for the mouse.
 *   @param [Integer] y The y coordinate for the mouse.
 *   @return [Integer]
 * @overload set_position(position)
 *   Sets the position of the mouse.
 *   @param [Carbuncle::Point] position a position for the mouse.
 *   @return [Integer]
 */
static mrb_value
mrb_mouse_set_position(mrb_state *mrb, mrb_value self)
{
  mrb_value obj;
  mrb_int x, y, argc;
  argc = mrb_get_args(mrb, "o|i", &obj, &y);
  if (argc == 1)
  {
    Vector2 *point = mrb_carbuncle_get_point(mrb, obj);
    x = point->x;
    y = point->y;
  }
  else
  {
    x = mrb_fixnum(mrb_to_int(mrb, obj));
  }
  SetMousePosition(x, y);
  return obj;
}

void
mrb_init_carbuncle_mouse(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  /**
   * This class handles mouse input.
   * On touch screens, the first touch you do at the screen is interpreted as the mouse.
   */
  struct RClass *mouse = mrb_define_module_under(mrb, carbuncle, "Mouse");

  mrb_define_module_function(mrb, mouse, "press?", mrb_mouse_pressQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, mouse, "down?", mrb_mouse_downQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, mouse, "release?", mrb_mouse_releaseQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, mouse, "up?", mrb_mouse_upQ, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, mouse, "x", mrb_mouse_get_x, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, mouse, "y", mrb_mouse_get_y, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, mouse, "position", mrb_mouse_get_position, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, mouse, "wheel_move", mrb_mouse_get_wheel_move, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, mouse, "wheel", mrb_mouse_get_wheel_move, MRB_ARGS_NONE());

  mrb_define_module_function(mrb, mouse, "x=", mrb_mouse_set_x, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, mouse, "y=", mrb_mouse_set_y, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, mouse, "position=", mrb_mouse_set_position, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, mouse, "set_position", mrb_mouse_set_position, MRB_ARGS_REQ(1)|MRB_ARGS_OPT(1));

  mrb_define_const(mrb, mouse, "LEFT", mrb_fixnum_value(MOUSE_LEFT_BUTTON));
  mrb_define_const(mrb, mouse, "RIGHT", mrb_fixnum_value(MOUSE_RIGHT_BUTTON));
  mrb_define_const(mrb, mouse, "MIDDLE", mrb_fixnum_value(MOUSE_MIDDLE_BUTTON));
  mrb_define_const(mrb, mouse, "LEFT_BUTTON", mrb_fixnum_value(MOUSE_LEFT_BUTTON));
  mrb_define_const(mrb, mouse, "RIGHT_BUTTON", mrb_fixnum_value(MOUSE_RIGHT_BUTTON));
  mrb_define_const(mrb, mouse, "MIDDLE_BUTTON", mrb_fixnum_value(MOUSE_MIDDLE_BUTTON));
}
