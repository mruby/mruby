#include "carbuncle/core.h"
#include "carbuncle/gamepad.h"
#include "carbuncle/point.h"

#include <mruby/array.h>
#include <mruby/variable.h>
#include <mruby/string.h>

#define MAX_AXIS_COUNT 16
#define GAMEPAD_COUNT 4

#define GAMEPADS_SYMBOL mrb_intern_cstr(mrb, "#gamepads")

#define CARBUNCLE_GAMEPAD_SIZE 52

struct mrb_Gamepad
{
  mrb_int id;
  mrb_int axis_count;
};

static const mrb_int GAMEPAD_IDS[GAMEPAD_COUNT] = {
    GAMEPAD_PLAYER1,
    GAMEPAD_PLAYER2,
    GAMEPAD_PLAYER3,
    GAMEPAD_PLAYER4
};

static const mrb_int CARBUNCLE_GAMEPAD_VALUES[CARBUNCLE_GAMEPAD_SIZE] = {
    GAMEPAD_BUTTON_LEFT_FACE_UP,
    GAMEPAD_BUTTON_LEFT_FACE_RIGHT,
    GAMEPAD_BUTTON_LEFT_FACE_DOWN,
    GAMEPAD_BUTTON_LEFT_FACE_LEFT,
    GAMEPAD_BUTTON_LEFT_FACE_UP,
    GAMEPAD_BUTTON_LEFT_FACE_RIGHT,
    GAMEPAD_BUTTON_LEFT_FACE_DOWN,
    GAMEPAD_BUTTON_LEFT_FACE_LEFT,
    GAMEPAD_BUTTON_LEFT_FACE_UP,
    GAMEPAD_BUTTON_LEFT_FACE_RIGHT,
    GAMEPAD_BUTTON_LEFT_FACE_DOWN,
    GAMEPAD_BUTTON_LEFT_FACE_LEFT,
    GAMEPAD_BUTTON_LEFT_FACE_UP,
    GAMEPAD_BUTTON_LEFT_FACE_RIGHT,
    GAMEPAD_BUTTON_LEFT_FACE_DOWN,
    GAMEPAD_BUTTON_LEFT_FACE_LEFT,

    GAMEPAD_BUTTON_RIGHT_FACE_UP,
    GAMEPAD_BUTTON_RIGHT_FACE_RIGHT,
    GAMEPAD_BUTTON_RIGHT_FACE_DOWN,
    GAMEPAD_BUTTON_RIGHT_FACE_LEFT,
    GAMEPAD_BUTTON_RIGHT_FACE_UP,
    GAMEPAD_BUTTON_RIGHT_FACE_RIGHT,
    GAMEPAD_BUTTON_RIGHT_FACE_DOWN,
    GAMEPAD_BUTTON_RIGHT_FACE_LEFT,

    GAMEPAD_BUTTON_LEFT_TRIGGER_1,
    GAMEPAD_BUTTON_LEFT_TRIGGER_2,
    GAMEPAD_BUTTON_LEFT_TRIGGER_1,
    GAMEPAD_BUTTON_LEFT_TRIGGER_2,
    GAMEPAD_BUTTON_LEFT_TRIGGER_1,
    GAMEPAD_BUTTON_LEFT_TRIGGER_2,
    GAMEPAD_BUTTON_LEFT_TRIGGER_1,
    GAMEPAD_BUTTON_LEFT_TRIGGER_2,
    GAMEPAD_BUTTON_LEFT_TRIGGER_2,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_1,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_1,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_1,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_1,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,
    GAMEPAD_BUTTON_RIGHT_TRIGGER_2,

    GAMEPAD_BUTTON_MIDDLE_LEFT,
    GAMEPAD_BUTTON_MIDDLE_LEFT,
    GAMEPAD_BUTTON_MIDDLE,
    GAMEPAD_BUTTON_MIDDLE,
    GAMEPAD_BUTTON_MIDDLE_RIGHT,
    GAMEPAD_BUTTON_MIDDLE_RIGHT,

    GAMEPAD_BUTTON_LEFT_THUMB,
    GAMEPAD_BUTTON_LEFT_THUMB,
    GAMEPAD_BUTTON_RIGHT_THUMB,
    GAMEPAD_BUTTON_RIGHT_THUMB
};

static const const char *CARBUNCLE_GAMEPAD_NAMES[CARBUNCLE_GAMEPAD_SIZE] = {
    "DPAD_UP",
    "DPAD_RIGHT",
    "DPAD_DOWN",
    "DPAD_LEFT",
    "LEFT_UP",
    "LEFT_RIGHT",
    "LEFT_DOWN",
    "LEFT_FACE_LEFT",
    "LEFT_FACE_UP",
    "LEFT_FACE_RIGHT",
    "LEFT_FACE_DOWN",
    "LEFT_FACE_LEFT",
    "UP",
    "RIGHT",
    "DOWN",
    "LEFT",

    "RIGHT_UP",
    "RIGHT_RIGHT",
    "RIGHT_DOWN",
    "RIGHT_LEFT",
    "RIGHT_FACE_UP",
    "RIGHT_FACE_RIGHT",
    "RIGHT_FACE_DOWN",
    "RIGHT_FACE_LEFT",

    "L1",
    "L2",
    "LEFT1",
    "LEFT2",
    "LB",
    "LT",
    "LEFT_BUTTON",
    "LEFT_TRIGGER",
    "ZL",
    "R1",
    "R2",
    "RIGHT1",
    "RIGHT2",
    "RB",
    "RT",
    "RIGHT_BUTTON",
    "RIGHT_TRIGGER",
    "ZR",

    "MIDDLE_LEFT",
    "SELECT",
    "MIDDLE",
    "HOME",
    "MIDDLE_RIGHT",
    "START",

    "LEFT_THUMB",
    "L3",
    "RIGHT_THUMB",
    "R3"
};

static const struct mrb_data_type gamepad_data_type = {
  "Carbuncle::Gamepad", mrb_free
};

static struct mrb_Gamepad *
get_gamepad(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &gamepad_data_type, struct mrb_Gamepad);
}

static inline mrb_int
get_button(mrb_state *mrb, mrb_value name)
{
  const char *button = mrb_str_to_cstr(mrb, name);
  mrb_sym button_symbol = mrb_intern_cstr(mrb, button);
  mrb_value gamepad_class = mrb_obj_value(mrb_carbuncle_module_get(mrb, "Gamepad"));
  if (!mrb_const_defined(mrb, gamepad_class, button_symbol))
  {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot find Gamepad button '%s'.", button);
  }
  return mrb_fixnum(mrb_const_get(mrb, gamepad_class, button_symbol));
}

static inline mrb_int
convert_gamepad_key(mrb_state *mrb)
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
    return get_button(mrb, mrb_funcall(mrb, string_value, "upcase", 0));
  }
  mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid key type, must be a symbol or an integer.");
  return -1;
}

static mrb_value
mrb_gamepad_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_int id;
  struct mrb_Gamepad *pad = mrb_malloc(mrb, sizeof *pad);
  mrb_get_args(mrb, "i", &id);
  pad->id = id;
  if (IsGamepadAvailable(id)) { pad->axis_count = GetGamepadAxisCount(id); }
  else { pad->axis_count = 0; }
  if (pad->axis_count > MAX_AXIS_COUNT) { pad->axis_count = MAX_AXIS_COUNT; }
  DATA_PTR(self) = pad;
  DATA_TYPE(self) = &gamepad_data_type;
  return self;
}

static mrb_value
mrb_gamepad_get_left_axis(mrb_state *mrb, mrb_value self)
{
  struct mrb_Gamepad *pad = get_gamepad(mrb, self);
  if (IsGamepadAvailable(pad->id))
  {
    return mrb_carbuncle_point_new(
      mrb,
      GetGamepadAxisMovement(pad->id, GAMEPAD_AXIS_LEFT_X),
      GetGamepadAxisMovement(pad->id, GAMEPAD_AXIS_LEFT_Y)
    );
  }
  return mrb_carbuncle_point_new(mrb, 0, 0);
}

static mrb_value
mrb_gamepad_get_right_axis(mrb_state *mrb, mrb_value self)
{
  struct mrb_Gamepad *pad = get_gamepad(mrb, self);
  if (IsGamepadAvailable(pad->id))
  {
    return mrb_carbuncle_point_new(
      mrb,
      GetGamepadAxisMovement(pad->id, GAMEPAD_AXIS_RIGHT_X),
      GetGamepadAxisMovement(pad->id, GAMEPAD_AXIS_RIGHT_Y)
    );
  }
  return mrb_carbuncle_point_new(mrb, 0, 0);
}

static mrb_value
mrb_gamepad_get_name(mrb_state *mrb, mrb_value self)
{
  struct mrb_Gamepad *pad = get_gamepad(mrb, self);
  const char *name = GetGamepadName(pad->id);
  return name ? mrb_str_new_cstr(mrb, name) : mrb_nil_value();
}

static mrb_value
mrb_gamepad_availableQ(mrb_state *mrb, mrb_value self)
{
  struct mrb_Gamepad *pad = get_gamepad(mrb, self);
  return mrb_bool_value(IsGamepadAvailable(pad->id));
}

static mrb_value
mrb_gamepad_upQ(mrb_state *mrb, mrb_value self)
{
  struct mrb_Gamepad *pad = get_gamepad(mrb, self);
  mrb_int button = convert_gamepad_key(mrb);
  return mrb_bool_value(IsGamepadButtonUp(pad->id, button));
}

static mrb_value
mrb_gamepad_downQ(mrb_state *mrb, mrb_value self)
{
  struct mrb_Gamepad *pad = get_gamepad(mrb, self);
  mrb_int button = convert_gamepad_key(mrb);
  return mrb_bool_value(IsGamepadButtonDown(pad->id, button));
}

static mrb_value
mrb_gamepad_pressQ(mrb_state *mrb, mrb_value self)
{
  struct mrb_Gamepad *pad = get_gamepad(mrb, self);
  mrb_int button = convert_gamepad_key(mrb);
  return mrb_bool_value(IsGamepadButtonPressed(pad->id, button));
}

static mrb_value
mrb_gamepad_releaseQ(mrb_state *mrb, mrb_value self)
{
  struct mrb_Gamepad *pad = get_gamepad(mrb, self);
  mrb_int button = convert_gamepad_key(mrb);
  return mrb_bool_value(IsGamepadButtonReleased(pad->id, button));
}

static mrb_value
mrb_s_gamepad_get_subscript(mrb_state *mrb, mrb_value self)
{
  mrb_int index;
  mrb_get_args(mrb, "i", &index);
  return mrb_ary_entry(mrb_cv_get(mrb, self, GAMEPADS_SYMBOL), index);
}

void
mrb_init_carbuncle_gamepad(mrb_state *mrb)
{
  struct RClass *gamepad = mrb_carbuncle_define_data_class(mrb, "Gamepad", mrb->object_class);

  mrb_define_method(mrb, gamepad, "initialize", mrb_gamepad_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, gamepad, "left_axis", mrb_gamepad_get_left_axis, MRB_ARGS_NONE());
  mrb_define_method(mrb, gamepad, "right_axis", mrb_gamepad_get_right_axis, MRB_ARGS_NONE());
  mrb_define_method(mrb, gamepad, "name", mrb_gamepad_get_name, MRB_ARGS_NONE());

  mrb_define_method(mrb, gamepad, "available?", mrb_gamepad_availableQ, MRB_ARGS_NONE());

  mrb_define_method(mrb, gamepad, "up?", mrb_gamepad_upQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, gamepad, "down?", mrb_gamepad_downQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, gamepad, "press?", mrb_gamepad_pressQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, gamepad, "release?", mrb_gamepad_releaseQ, MRB_ARGS_NONE());

  mrb_define_class_method(mrb, gamepad, "[]", mrb_s_gamepad_get_subscript, MRB_ARGS_REQ(1));

  int arena = mrb_gc_arena_save(mrb);

  mrb_value gamepads[GAMEPAD_COUNT];
  for (mrb_int i = 0; i < GAMEPAD_COUNT; ++i)
  {
    mrb_value id = mrb_fixnum_value(GAMEPAD_IDS[i]);
    gamepads[i] = mrb_obj_new(mrb, gamepad, 1, &id);
  }
  mrb_cv_set(mrb, mrb_obj_value(gamepad), GAMEPADS_SYMBOL, mrb_ary_new_from_values(mrb, GAMEPAD_COUNT, gamepads));

  mrb_gc_arena_restore(mrb, arena);

  for (mrb_int i = 0; i < CARBUNCLE_GAMEPAD_SIZE; ++i)
  {
    mrb_define_const(mrb, gamepad, CARBUNCLE_GAMEPAD_NAMES[i], mrb_fixnum_value(CARBUNCLE_GAMEPAD_VALUES[i]));
  }
}
