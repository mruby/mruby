#include "carbuncle/core.h"
#include "carbuncle/keyboard.h"

#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/class.h>

#include "raylib.h"

#define CARBUNCLE_KEYBOAR_SIZE 84

static const mrb_int CARBUNCLE_KEYBOARD_VALUES[CARBUNCLE_KEYBOAR_SIZE] = {
  KEY_BACKSPACE, KEY_SPACE,
  KEY_ESCAPE, KEY_ESCAPE,
  KEY_ENTER, KEY_ENTER,
  KEY_DELETE, KEY_DELETE,
  KEY_RIGHT, KEY_LEFT, KEY_DOWN, KEY_UP,
  KEY_RIGHT, KEY_LEFT, KEY_DOWN, KEY_UP,
  KEY_RIGHT, KEY_LEFT, KEY_DOWN, KEY_UP,
  KEY_F1, KEY_F2, KEY_F3, KEY_F4, KEY_F5, KEY_F6,
  KEY_F7, KEY_F8, KEY_F9, KEY_F10, KEY_F11, KEY_F12,
  KEY_LEFT_SHIFT, KEY_LEFT_CONTROL, KEY_LEFT_ALT,
  KEY_RIGHT_SHIFT, KEY_RIGHT_CONTROL, KEY_RIGHT_ALT,
  KEY_ZERO, KEY_ONE, KEY_TWO, KEY_THREE, KEY_FOUR,
  KEY_FIVE, KEY_SIX, KEY_SEVEN, KEY_EIGHT, KEY_NINE,
  KEY_ZERO, KEY_ONE, KEY_TWO, KEY_THREE, KEY_FOUR,
  KEY_FIVE, KEY_SIX, KEY_SEVEN, KEY_EIGHT, KEY_NINE,
  KEY_A, KEY_B, KEY_C, KEY_D, KEY_E, KEY_F, KEY_G, KEY_H, KEY_I, KEY_J,
  KEY_K, KEY_L, KEY_M, KEY_N, KEY_O, KEY_P, KEY_Q, KEY_R, KEY_S, KEY_T,
  KEY_U, KEY_V, KEY_W, KEY_X, KEY_Y, KEY_Z
};

static const char *CARBUNCLE_KEYBOARD_NAMES[CARBUNCLE_KEYBOAR_SIZE] = {
  "BACKSPACE", "SPACE",
  "ESCAPE", "ESC",
  "ENTER", "RETURN",
  "DELETE", "DEL",
  "RIGHT", "LEFT", "DOWN", "UP",
  "ARROW_RIGHT", "ARROW_LEFT", "ARROW_DOWN", "ARROW_UP",
  "RIGHT_ARROW", "LEFT_ARROW", "DOWN_ARROW", "UP_ARROW",
  "F1", "F2", "F3", "F4", "F5", "F6",
  "F7", "F8", "F9", "F10", "F11", "F12",
  "LEFT_SHIFT", "LEFT_CONTROL", "LEFT_ALT",
  "RIGHT_SHIFT", "RIGHT_CONTROL", "RIGHT_ALT",
  "ZERO", "ONE", "TWO", "THREE", "FOUR",
  "FIVE", "SIX", "SEVEN", "EIGHT", "NINE",
  "0", "1", "2", "3", "4",
  "5", "6", "7", "8", "9",
  "A", "B", "C", "D", "E", "F", "G", "H", "I", "J",
  "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T",
  "U", "V", "W", "X", "Y", "Z"
};

static int
keyboard_utf8_encode(char *out, uint32_t utf)
{
  if (utf <= 0x7F) {
    // Plain ASCII
    out[0] = (char) utf;
    out[1] = 0;
    return 1;
  }
  else if (utf <= 0x07FF) {
    // 2-byte unicode
    out[0] = (char) (((utf >> 6) & 0x1F) | 0xC0);
    out[1] = (char) (((utf >> 0) & 0x3F) | 0x80);
    out[2] = 0;
    return 2;
  }
  else if (utf <= 0xFFFF) {
    // 3-byte unicode
    out[0] = (char) (((utf >> 12) & 0x0F) | 0xE0);
    out[1] = (char) (((utf >>  6) & 0x3F) | 0x80);
    out[2] = (char) (((utf >>  0) & 0x3F) | 0x80);
    out[3] = 0;
    return 3;
  }
  else if (utf <= 0x10FFFF) {
    // 4-byte unicode
    out[0] = (char) (((utf >> 18) & 0x07) | 0xF0);
    out[1] = (char) (((utf >> 12) & 0x3F) | 0x80);
    out[2] = (char) (((utf >>  6) & 0x3F) | 0x80);
    out[3] = (char) (((utf >>  0) & 0x3F) | 0x80);
    out[4] = 0;
    return 4;
  }
  else { 
    // error - use replacement character
    out[0] = (char) 0xEF;  
    out[1] = (char) 0xBF;
    out[2] = (char) 0xBD;
    out[3] = 0;
    return 0;
  }
}

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

static mrb_value
mrb_keyboard_upQ(mrb_state *mrb, mrb_value self)
{
  mrb_int key = convert_keyboard_key(mrb);
  return mrb_bool_value(IsKeyUp(key));
}

static mrb_value
mrb_keyboard_get_character(mrb_state *mrb, mrb_value self)
{
  char character[5];
  int key = GetKeyPressed();
  if (key <= 0) { return mrb_nil_value(); }
  keyboard_utf8_encode(character, key);
  return mrb_str_new_cstr(mrb, character);
}

void
mrb_carbuncle_keyboard_init(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *keyboard = mrb_define_module_under(mrb, carbuncle, "Keyboard");

  mrb_define_module_function(mrb, keyboard, "press?", mrb_keyboard_pressQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, keyboard, "down?", mrb_keyboard_downQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, keyboard, "release?", mrb_keyboard_releaseQ, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, keyboard, "up?", mrb_keyboard_upQ, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, keyboard, "character", mrb_keyboard_get_character, MRB_ARGS_NONE());

  for (mrb_int i = 0; i < CARBUNCLE_KEYBOAR_SIZE; ++i)
  {
    mrb_define_const(mrb, keyboard, CARBUNCLE_KEYBOARD_NAMES[i], mrb_fixnum_value(CARBUNCLE_KEYBOARD_VALUES[i]));
  }
}
