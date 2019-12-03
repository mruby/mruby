#include "carbuncle/core.h"
#include "carbuncle/keyboard.h"

#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/class.h>

#include "raylib.h"

static inline mrb_int
get_keyboard_value(mrb_state *mrb, mrb_value name)
{
  const char *key = mrb_str_to_cstr(mrb, name);
  mrb_sym key_symbol = mrb_intern_cstr(mrb, key);
  mrb_value keyboard_class = mrb_obj_value(mrb_carbuncle_module_get(mrb, "Keyboard"));
  if (!mrb_const_defined(mrb, keyboard_class, key_symbol))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot find keyboard key '%s'.", key);
  }
  return mrb_fixnum(mrb_const_get(mrb, keyboard_class, key_symbol));
}

static inline mrb_int
convert_keyboard_key(mrb_state *mrb)
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
    return get_keyboard_value(mrb, mrb_funcall(mrb, string_value, "upcase", 0));
  }
  mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid key type, must be a symbol or an integer.");
  return -1;
}

static mrb_value
mrb_keyboard_pressQ(mrb_state *mrb, mrb_value self)
{
  mrb_int key = convert_keyboard_key(mrb);
  return mrb_bool_value(IsKeyPressed(key));
}

static mrb_value
mrb_keyboard_downQ(mrb_state *mrb, mrb_value self)
{
  mrb_int key = convert_keyboard_key(mrb);
  return mrb_bool_value(IsKeyDown(key));
}

static mrb_value
mrb_keyboard_releaseQ(mrb_state *mrb, mrb_value self)
{
  mrb_int key = convert_keyboard_key(mrb);
  return mrb_bool_value(IsKeyReleased(key));
}

void
mrb_carbuncle_keyboard_init(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *keyboard = mrb_define_module_under(mrb, carbuncle, "Keyboard");

  mrb_define_module_function(mrb, keyboard, "press?", mrb_keyboard_pressQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, keyboard, "down?", mrb_keyboard_downQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, keyboard, "release?", mrb_keyboard_releaseQ, MRB_ARGS_REQ(1));

  mrb_define_const(mrb, keyboard, "BACKSPACE", mrb_fixnum_value(KEY_BACKSPACE));
  mrb_define_const(mrb, keyboard, "SPACE", mrb_fixnum_value(KEY_SPACE));
  mrb_define_const(mrb, keyboard, "ESCAPE", mrb_fixnum_value(KEY_ESCAPE));
  mrb_define_const(mrb, keyboard, "ESC", mrb_fixnum_value(KEY_ESCAPE));
  mrb_define_const(mrb, keyboard, "ENTER", mrb_fixnum_value(KEY_ENTER));
  mrb_define_const(mrb, keyboard, "RETURN", mrb_fixnum_value(KEY_ENTER));
  mrb_define_const(mrb, keyboard, "DELETE", mrb_fixnum_value(KEY_DELETE));
  mrb_define_const(mrb, keyboard, "DEL", mrb_fixnum_value(KEY_DELETE));
  mrb_define_const(mrb, keyboard, "RIGHT", mrb_fixnum_value(KEY_RIGHT));
  mrb_define_const(mrb, keyboard, "LEFT", mrb_fixnum_value(KEY_LEFT));
  mrb_define_const(mrb, keyboard, "DOWN", mrb_fixnum_value(KEY_DOWN));
  mrb_define_const(mrb, keyboard, "UP", mrb_fixnum_value(KEY_UP));
  mrb_define_const(mrb, keyboard, "ARROW_RIGHT", mrb_fixnum_value(KEY_RIGHT));
  mrb_define_const(mrb, keyboard, "ARROW_LEFT", mrb_fixnum_value(KEY_LEFT));
  mrb_define_const(mrb, keyboard, "ARROW_DOWN", mrb_fixnum_value(KEY_DOWN));
  mrb_define_const(mrb, keyboard, "ARROW_UP", mrb_fixnum_value(KEY_UP));  
  mrb_define_const(mrb, keyboard, "F1", mrb_fixnum_value(KEY_F1));
  mrb_define_const(mrb, keyboard, "F2", mrb_fixnum_value(KEY_F2));
  mrb_define_const(mrb, keyboard, "F3", mrb_fixnum_value(KEY_F3));
  mrb_define_const(mrb, keyboard, "F4", mrb_fixnum_value(KEY_F4));
  mrb_define_const(mrb, keyboard, "F5", mrb_fixnum_value(KEY_F5));
  mrb_define_const(mrb, keyboard, "F6", mrb_fixnum_value(KEY_F6));
  mrb_define_const(mrb, keyboard, "F7", mrb_fixnum_value(KEY_F7));
  mrb_define_const(mrb, keyboard, "F8", mrb_fixnum_value(KEY_F8));
  mrb_define_const(mrb, keyboard, "F9", mrb_fixnum_value(KEY_F9));
  mrb_define_const(mrb, keyboard, "F10", mrb_fixnum_value(KEY_F10));
  mrb_define_const(mrb, keyboard, "F11", mrb_fixnum_value(KEY_F11));
  mrb_define_const(mrb, keyboard, "F12", mrb_fixnum_value(KEY_F12));
  mrb_define_const(mrb, keyboard, "LEFT_SHIFT", mrb_fixnum_value(KEY_LEFT_SHIFT));
  mrb_define_const(mrb, keyboard, "LEFT_CONTROL", mrb_fixnum_value(KEY_LEFT_CONTROL));
  mrb_define_const(mrb, keyboard, "LEFT_ALT", mrb_fixnum_value(KEY_LEFT_ALT));
  mrb_define_const(mrb, keyboard, "RIGHT_SHIFT", mrb_fixnum_value(KEY_RIGHT_SHIFT));
  mrb_define_const(mrb, keyboard, "RIGHT_CONTROL", mrb_fixnum_value(KEY_RIGHT_CONTROL));
  mrb_define_const(mrb, keyboard, "RIGHT_ALT", mrb_fixnum_value(KEY_RIGHT_ALT));
  mrb_define_const(mrb, keyboard, "ZERO", mrb_fixnum_value(KEY_ZERO));
  mrb_define_const(mrb, keyboard, "ONE", mrb_fixnum_value(KEY_ONE));
  mrb_define_const(mrb, keyboard, "TWO", mrb_fixnum_value(KEY_TWO));
  mrb_define_const(mrb, keyboard, "THREE", mrb_fixnum_value(KEY_THREE));
  mrb_define_const(mrb, keyboard, "FOUR", mrb_fixnum_value(KEY_FOUR));
  mrb_define_const(mrb, keyboard, "FIVE", mrb_fixnum_value(KEY_FIVE));
  mrb_define_const(mrb, keyboard, "SIX", mrb_fixnum_value(KEY_SIX));
  mrb_define_const(mrb, keyboard, "SEVEN", mrb_fixnum_value(KEY_SEVEN));
  mrb_define_const(mrb, keyboard, "EIGHT", mrb_fixnum_value(KEY_EIGHT));
  mrb_define_const(mrb, keyboard, "NINE", mrb_fixnum_value(KEY_NINE));
  mrb_define_const(mrb, keyboard, "0", mrb_fixnum_value(KEY_ZERO));
  mrb_define_const(mrb, keyboard, "1", mrb_fixnum_value(KEY_ONE));
  mrb_define_const(mrb, keyboard, "2", mrb_fixnum_value(KEY_TWO));
  mrb_define_const(mrb, keyboard, "3", mrb_fixnum_value(KEY_THREE));
  mrb_define_const(mrb, keyboard, "4", mrb_fixnum_value(KEY_FOUR));
  mrb_define_const(mrb, keyboard, "5", mrb_fixnum_value(KEY_FIVE));
  mrb_define_const(mrb, keyboard, "6", mrb_fixnum_value(KEY_SIX));
  mrb_define_const(mrb, keyboard, "7", mrb_fixnum_value(KEY_SEVEN));
  mrb_define_const(mrb, keyboard, "8", mrb_fixnum_value(KEY_EIGHT));
  mrb_define_const(mrb, keyboard, "9", mrb_fixnum_value(KEY_NINE));
  mrb_define_const(mrb, keyboard, "A", mrb_fixnum_value(KEY_A));
  mrb_define_const(mrb, keyboard, "B", mrb_fixnum_value(KEY_B));
  mrb_define_const(mrb, keyboard, "C", mrb_fixnum_value(KEY_C));
  mrb_define_const(mrb, keyboard, "D", mrb_fixnum_value(KEY_D));
  mrb_define_const(mrb, keyboard, "E", mrb_fixnum_value(KEY_E));
  mrb_define_const(mrb, keyboard, "F", mrb_fixnum_value(KEY_F));
  mrb_define_const(mrb, keyboard, "G", mrb_fixnum_value(KEY_G));
  mrb_define_const(mrb, keyboard, "H", mrb_fixnum_value(KEY_H));
  mrb_define_const(mrb, keyboard, "I", mrb_fixnum_value(KEY_I));
  mrb_define_const(mrb, keyboard, "J", mrb_fixnum_value(KEY_J));
  mrb_define_const(mrb, keyboard, "K", mrb_fixnum_value(KEY_K));
  mrb_define_const(mrb, keyboard, "L", mrb_fixnum_value(KEY_L));
  mrb_define_const(mrb, keyboard, "M", mrb_fixnum_value(KEY_M));
  mrb_define_const(mrb, keyboard, "N", mrb_fixnum_value(KEY_N));
  mrb_define_const(mrb, keyboard, "O", mrb_fixnum_value(KEY_O));
  mrb_define_const(mrb, keyboard, "P", mrb_fixnum_value(KEY_P));
  mrb_define_const(mrb, keyboard, "Q", mrb_fixnum_value(KEY_Q));
  mrb_define_const(mrb, keyboard, "R", mrb_fixnum_value(KEY_R));
  mrb_define_const(mrb, keyboard, "S", mrb_fixnum_value(KEY_S));
  mrb_define_const(mrb, keyboard, "T", mrb_fixnum_value(KEY_T));
  mrb_define_const(mrb, keyboard, "U", mrb_fixnum_value(KEY_U));
  mrb_define_const(mrb, keyboard, "V", mrb_fixnum_value(KEY_V));
  mrb_define_const(mrb, keyboard, "W", mrb_fixnum_value(KEY_W));
  mrb_define_const(mrb, keyboard, "X", mrb_fixnum_value(KEY_X));
  mrb_define_const(mrb, keyboard, "Y", mrb_fixnum_value(KEY_Y));
  mrb_define_const(mrb, keyboard, "Z", mrb_fixnum_value(KEY_Z));
}
