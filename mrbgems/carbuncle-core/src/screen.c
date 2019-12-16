#include "carbuncle/core.h"
#include "carbuncle/screen.h"
#include "carbuncle/game.h"

#include "raylib.h"

#include <mruby/data.h>
#include <mruby/variable.h>
#include <mruby/gc.h>
#include <mruby/string.h>
#include <mruby/class.h>

#define TITLE_SYMBOL mrb_intern_cstr(mrb, "#title")
#define GAME_SYMBOL mrb_intern_cstr(mrb, "#game")
#define CURSOR_SYMBOL mrb_intern_cstr(mrb, "#cursor")

static const struct mrb_data_type screen_data_type = {
  "Carbuncle::Screen", mrb_free
};

static void
check_game(mrb_state *mrb, mrb_value self, Screen *screen)
{
  mrb_value game = mrb_iv_get(mrb, self, GAME_SYMBOL);
  if (mrb_carbuncle_is_current_game(mrb, game))
  {
    SetWindowSize(screen->width, screen->height);
  }
}

static mrb_value
mrb_cursor_new(mrb_state *mrb, mrb_value game)
{
  struct RClass *screen = mrb_carbuncle_class_get(mrb, "Screen");
  struct RClass *cursor = mrb_class_get_under(mrb, screen, "Cursor");
  return mrb_obj_new(mrb, cursor, 1, &game);
}

/**
 * @overload initialize(game)
 *   Creates a new screen object.
 *   @param [Carbuncle::Game] game The game the screen corresponds to.
 *   @return [self]
 */
static mrb_value
mrb_screen_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value game;
  mrb_get_args(mrb, "o", &game);
  Screen *data = mrb_malloc(mrb, sizeof *data);
  data->width = 640;
  data->height = 480;
  DATA_TYPE(self) = &screen_data_type;
  DATA_PTR(self) = data;
  mrb_iv_set(mrb, self, GAME_SYMBOL, game);
  mrb_iv_set(mrb, self, TITLE_SYMBOL, mrb_str_new_cstr(mrb, "Carbuncle Game"));
  mrb_iv_set(mrb, self, CURSOR_SYMBOL, mrb_cursor_new(mrb, game));
  return self;
}

/**
 * @overload width
 *   The screen's width.
 *   @return [Integer]
 */
static mrb_value
mrb_screen_get_width(mrb_state *mrb, mrb_value self)
{
  Screen *screen = mrb_carbuncle_get_screen(mrb, self);
  return mrb_fixnum_value(screen->width);
}

/**
 * @overload height
 *   The screen's title.
 *   @return [Integer]
 */
static mrb_value
mrb_screen_get_height(mrb_state *mrb, mrb_value self)
{
  Screen *screen = mrb_carbuncle_get_screen(mrb, self);
  return mrb_fixnum_value(screen->height);
}

/**
 * @overload title
 *   The screen's title.
 *   @return [String]
 */
static mrb_value
mrb_screen_get_title(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, TITLE_SYMBOL);
}

/**
 * @return [Carbuncle::Game]
 */
static mrb_value
mrb_screen_get_game(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, GAME_SYMBOL);
}

/**
 * @return [Carbuncle::Screen::Cursor]
 */
static mrb_value
mrb_screen_get_cursor(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, CURSOR_SYMBOL);
}

/**
 * @overload width=(value)
 *   Changes the screen's width.
 *   @param [Integer] value The new screen width.
 *   @return [Integer]
 */
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

/**
 * @overload height=(value)
 *   Changes the screen's height.
 *   @param [Integer] value The new screen height.
 *   @return [Integer]
 */
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

/**
 * @overload title=(value)
 *   Changes the screen's title.
 *   @param [String] value The new screen title.
 *   @return [String]
 */
static mrb_value
mrb_screen_set_title(mrb_state *mrb, mrb_value self)
{
  mrb_value value;
  mrb_carbuncle_check_frozen(mrb, self);
  mrb_get_args(mrb, "o", &value);
  mrb_value string_value = mrb_funcall(mrb, value, "to_s", 0);
  mrb_iv_set(mrb, self, TITLE_SYMBOL, string_value);
  mrb_value game = mrb_iv_get(mrb, self, GAME_SYMBOL);
  if (mrb_carbuncle_is_current_game(mrb, game))
  {
    SetWindowTitle(mrb_string_cstr(mrb, string_value));
  }    
  return string_value;
}

/**
 * @overload resize(width, height)
 *   Changes the size of the screen.
 *   @param [Integer] width the screen's new width.
 *   @param [Integer] height the screen's new height.
 *   @return [self]
 */
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
  return self;
}

/**
 * @overload initialize(game)
 *   Assigns a new cursor to the corresponding game.
 *   @param [Carbuncle::Game] game The game is attached to.
 */
static mrb_value
mrb_cursor_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value game;
  mrb_get_args(mrb, "o", &game);
  mrb_iv_set(mrb, self, GAME_SYMBOL, game);
  return self;
}

/**
 * @overload visible?
 *   Returns if the window's cursor is visible.
 *   @return [Boolean]
 */
static mrb_value
mrb_cursor_get_visible(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!IsCursorHidden());
}

/**
 * @overload visible=(value)
 *   @param [Boolean] value
 *   @return [Boolean]
 */
static mrb_value
mrb_cursor_set_visible(mrb_state *mrb, mrb_value self)
{
  mrb_bool visible;
  mrb_get_args(mrb, "b", &visible);
  if (visible) { ShowCursor(); }
  else { HideCursor(); }
  return mrb_bool_value(visible);
}

void
mrb_init_carbuncle_screen(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  /**
   * Represents the Screen, where all objects are drawn.
   * @!attribute [rw] width
   *   The screen's width.
   * @!attribute [rw] height
   *   The screen's height.
   * @!attribute [rw] title
   *   The screen's title.
   * @!attribute [r] game
   *   The screen's attached game.
   * @!attribute [r] cursor
   *   The screen's cursor.
   */
  struct RClass *screen = mrb_define_class_under(mrb, carbuncle, "Screen", mrb->object_class);
  MRB_SET_INSTANCE_TT(screen, MRB_TT_DATA);

  mrb_define_method(mrb, screen, "initialize", mrb_screen_initialize, MRB_ARGS_REQ(1));

  /* Getters */
  mrb_define_method(mrb, screen, "width", mrb_screen_get_width, MRB_ARGS_NONE());
  mrb_define_method(mrb, screen, "height", mrb_screen_get_height, MRB_ARGS_NONE());
  mrb_define_method(mrb, screen, "title", mrb_screen_get_title, MRB_ARGS_NONE());
  mrb_define_method(mrb, screen, "game", mrb_screen_get_game, MRB_ARGS_NONE());
  mrb_define_method(mrb, screen, "cursor", mrb_screen_get_cursor, MRB_ARGS_NONE());

  /* Setters */
  mrb_define_method(mrb, screen, "width=", mrb_screen_set_width, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, screen, "height=", mrb_screen_set_height, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, screen, "title=", mrb_screen_set_title, MRB_ARGS_REQ(1));  

  mrb_define_method(mrb, screen, "resize", mrb_screen_resize, MRB_ARGS_REQ(2));

  /**
   * Handles the window's cursor.
   * @!attribute [rw] visible
   *   Indicates if the cursor is visible or not.
   */
  struct RClass *cursor = mrb_define_class_under(mrb, screen, "Cursor", mrb->object_class);

  mrb_define_method(mrb, cursor, "initialize", mrb_cursor_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, cursor, "visible?", mrb_cursor_get_visible, MRB_ARGS_NONE());
  mrb_define_method(mrb, cursor, "visible",  mrb_cursor_get_visible, MRB_ARGS_NONE());
  mrb_define_method(mrb, cursor, "visible=", mrb_cursor_set_visible, MRB_ARGS_REQ(1));
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