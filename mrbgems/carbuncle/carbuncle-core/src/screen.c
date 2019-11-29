#include "carbuncle/core.h"
#include "carbuncle/screen.h"
#include "carbuncle/game.h"

#include "raylib.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/gc.h>
#include <mruby/string.h>
#include <mruby/class.h>

static const char *TITLE_SYM = "#title";
static const char *GAME_SYM = "#game";

static const struct mrb_data_type screen_data_type = {
  "carbuncle/screen", mrb_free
};

static void
check_game(mrb_state *mrb, mrb_value self, Screen *screen)
{
  mrb_value game = mrb_iv_get(mrb, self, mrb_intern_cstr(mrb, GAME_SYM));
  if (mrb_carbuncle_is_current_game(mrb, game))
  {
    SetWindowSize(screen->width, screen->height);
  }
}

static mrb_value
mrb_screen_initialize(mrb_state *mrb, mrb_value self, Screen *screen)
{
  mrb_value game;
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_get_args(mrb, "o", &game);
  Screen *data = mrb_malloc(mrb, sizeof *data);
  data->width = 640;
  data->height = 480;
  DATA_TYPE(self) = &screen_data_type;
  DATA_PTR(self) = data;
  mrb_iv_set(mrb, self, mrb_intern_cstr(mrb, GAME_SYM), game);
  mrb_iv_set(mrb, self, mrb_intern_cstr(mrb, TITLE_SYM), mrb_str_new_cstr(mrb, "Carbuncle Game"));
  return self;
}

static mrb_value
mrb_screen_get_width(mrb_state *mrb, mrb_value self)
{
  Screen *screen = mrb_carbuncle_get_screen(mrb, self);
  return mrb_fixnum_value(screen->width);
}

static mrb_value
mrb_screen_get_height(mrb_state *mrb, mrb_value self)
{
  Screen *screen = mrb_carbuncle_get_screen(mrb, self);
  return mrb_fixnum_value(screen->height);
}

static mrb_value
mrb_screen_get_title(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, mrb_intern_cstr(mrb, TITLE_SYM));
}

static mrb_value
mrb_screen_get_game(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, mrb_intern_cstr(mrb, GAME_SYM));
}

static mrb_value
mrb_screen_set_width(mrb_state *mrb, mrb_value self)
{
  mrb_int value;
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_get_args(mrb, "i", &value);
  if (value < 1)
  {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Screen width must be positive.");
    return mrb_nil_value();
  }
  Screen *screen = mrb_carbuncle_get_screen(mrb, self);
  screen->width = value;
  mrb_value width = mrb_fixnum_value(value);
  check_game(mrb, self, screen);
  return width;
}

static mrb_value
mrb_screen_set_height(mrb_state *mrb, mrb_value self)
{
  mrb_int value;
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_get_args(mrb, "i", &value);
  if (value < 1)
  {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "Screen height must be positive.");
    return mrb_nil_value();
  }
  Screen *screen = mrb_carbuncle_get_screen(mrb, self);
  screen->height = value;
  mrb_value height = mrb_fixnum_value(value);
  check_game(mrb, self, screen);
  return height;
}

static mrb_value
mrb_screen_set_title(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_get_args(mrb, "o", &value);
  mrb_value string_value = mrb_funcall(mrb, value, "to_s", 0);
  mrb_iv_set(mrb, self, mrb_intern_cstr(mrb, TITLE_SYM), string_value);
  mrb_value game = mrb_iv_get(mrb, self, mrb_intern_cstr(mrb, GAME_SYM));
  if (mrb_carbuncle_is_current_game(mrb, game))
  {
    SetWindowTitle(mrb_string_cstr(mrb, string_value));
  }    
  return string_value;
}

static mrb_value
mrb_screen_resize(mrb_state *mrb, mrb_value self)
{
  mrb_int width, height;
  mrb_carbuncle_check_frozen(mrb, self);
  Screen *screen = mrb_carbuncle_get_screen(mrb, self);
  mrb_get_args(mrb, "ii", &width, &height);
  screen->width = width;
  screen->height = height;
  check_game(mrb, self, screen);
}

void
mrb_carbuncle_screen_init(mrb_state *mrb, struct RClass *carbuncle)
{
  struct RClass *screen = mrb_carbuncle_define_data_class(mrb, "Screen", mrb->object_class);

  mrb_define_method(mrb, screen, "initialize", mrb_screen_initialize, MRB_ARGS_REQ(1));

  /* Getters */
  mrb_define_method(mrb, screen, "width", mrb_screen_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, screen, "height", mrb_screen_get_height, MRB_ARGS_NONE());
  mrb_define_method(mrb, screen, "title", mrb_screen_get_title, MRB_ARGS_NONE());
  mrb_define_method(mrb, screen, "game", mrb_screen_get_game, MRB_ARGS_NONE());

  /* Setters */
  mrb_define_method(mrb, screen, "width=", mrb_screen_set_width, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, screen, "height=", mrb_screen_set_height, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, screen, "title=", mrb_screen_set_title, MRB_ARGS_REQ(1));  

  mrb_define_method(mrb, screen, "resize", mrb_screen_resize, MRB_ARGS_REQ(2));
}

Screen *
mrb_carbuncle_get_screen(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_PTR(mrb, obj, &screen_data_type, Screen);
}

mrb_bool
mrb_carbuncle_screen_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &screen_data_type);
}

mrb_value
mrb_carbuncle_screen_new(mrb_state *mrb, mrb_value game)
{
  struct RClass *carbuncle = mrb_module_get(mrb, "Carbuncle");
  struct RClass *screen_class = mrb_class_get_under(mrb, carbuncle, "Screen");
  return mrb_obj_new(mrb, screen_class, 1, &game);
}