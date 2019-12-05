#include "carbuncle/core.h"
#include "carbuncle/game.h"
#include "carbuncle/screen.h"
#include "carbuncle/audio_manager.h"

#include "raylib.h"

#include <mruby/variable.h>
#include <mruby/gc.h>
#include <mruby/string.h>
#include <mruby/class.h>

#ifdef __EMSCRIPTEN__
#include <emscripten/emscripten.h>
#endif

/* Constants */

#ifdef __EMSCRIPTEN__
  static mrb_state *emscripten_mrb_state = NULL;
  static mrb_value emscripten_carbuncle_game;
#endif

static const char *CURRENT_GAME_GV_NAME = "#carbunce_current_game";
static const char *SCREEN_IV_NAME = "#screen";

#define CURRENT_GAME_SYMBOL mrb_intern_cstr(mrb, CURRENT_GAME_GV_NAME)
#define SCREEN_SYMBOL mrb_intern_cstr(mrb, SCREEN_IV_NAME)

/* Variables */

static mrb_bool carbuncle_game_is_running = FALSE;

/* Helper functions */

static inline void
create_window(mrb_state *mrb, mrb_value instance)
{
  mrb_value screen_value = mrb_iv_get(mrb, instance, SCREEN_SYMBOL);
  Screen *screen = mrb_carbuncle_get_screen(mrb, screen_value);
  mrb_value title_value = mrb_funcall(mrb, screen_value, "title", 0);
  const char *title = mrb_str_to_cstr(mrb, title_value);
  InitWindow(screen->width, screen->height, title);
}

static inline void
begin_game(mrb_state *mrb, mrb_value self, mrb_value instance)
{
  //SetTraceLogLevel(LOG_ERROR);
  mrb_funcall(mrb, instance, "before_run", 0);
  mrb_gv_set(mrb, CURRENT_GAME_SYMBOL, instance);
  mrb_gc_register(mrb, instance);
  carbuncle_game_is_running = TRUE;
  create_window(mrb, instance);
  InitAudioDevice();
  mrb_funcall(mrb, instance, "load", 0);
  mrb_funcall(mrb, instance, "after_run", 0);
}

static inline void
check_closing(mrb_state *mrb, mrb_value instance)
{
  if (WindowShouldClose())
  {
    mrb_value should_close = mrb_funcall(mrb, instance, "close?", 0);
    if (mrb_test(should_close))
    {
      mrb_funcall(mrb, instance, "close", 0);
    }
  }  
}

static inline void
draw_game(mrb_state *mrb, mrb_value instance)
{
  BeginDrawing();
  ClearBackground(BLACK);
  mrb_funcall(mrb, instance, "draw", 0);
  EndDrawing();
}

#ifdef __EMSCRIPTEN__
static void
carbuncle_emscripten_game_frame(void)
{
  mrb_value dt = mrb_float_value(emscripten_mrb_state, GetFrameTime());
  carbuncle_audio_manager_update();
  mrb_funcall(emscripten_mrb_state, emscripten_carbuncle_game, "update", 1, dt);
  draw_game(emscripten_mrb_state, emscripten_carbuncle_game);
}
#endif

static inline void
game_loop(mrb_state *mrb, mrb_value instance)
{
#ifdef __EMSCRIPTEN__
  emscripten_mrb_state = mrb;
  emscripten_carbuncle_game = instance;
  emscripten_set_main_loop(carbuncle_emscripten_game_frame, 0, 1);
  emscripten_carbuncle_game = mrb_nil_value();
  emscripten_mrb_state = NULL;
#else
  while (carbuncle_game_is_running)
  {
    check_closing(mrb, instance);
    carbuncle_audio_manager_update();
    mrb_funcall(mrb, instance, "update", 1, mrb_float_value(mrb, GetFrameTime()));
    draw_game(mrb, instance);
  }
#endif
}

static inline void
close_game(mrb_state *mrb, mrb_value self, mrb_value instance)
{
  mrb_gc_unregister(mrb, instance);
  mrb_gv_set(mrb, CURRENT_GAME_SYMBOL, mrb_nil_value());
  mrb_funcall(mrb, instance, "before_close", 0);
  CloseAudioDevice();
  HideWindow();
  CloseWindow();
  mrb_funcall(mrb, instance, "after_close", 0);
}

/* Ruby Bindings */

static mrb_value
mrb_s_game_run(mrb_state *mrb, mrb_value self)
{
  mrb_value instance = mrb_funcall(mrb, self, "new", 0);
  begin_game(mrb, self, instance);
  game_loop(mrb, instance);
  close_game(mrb, self, instance);
  return mrb_nil_value();
}

static mrb_value
mrb_s_game_runningQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(carbuncle_game_is_running);
}

static mrb_value
mrb_s_game_get_current_game(mrb_state *mrb, mrb_value self)
{
  return mrb_gv_get(mrb, CURRENT_GAME_SYMBOL);
}

static mrb_value
mrb_game_get_screen(mrb_state *mrb, mrb_value self)
{
  return mrb_iv_get(mrb, self, SCREEN_SYMBOL);
}

static mrb_value
mrb_game_initialize(mrb_state *mrb, mrb_value self)
{
  mrb_value new_screen = mrb_carbuncle_screen_new(mrb, self);
  mrb_iv_set(mrb, self, SCREEN_SYMBOL, new_screen);
  return self;
}

static mrb_value
mrb_game_closeQ(mrb_state *mrb, mrb_value self)
{
  return mrb_true_value();
}

static mrb_value
mrb_game_dummy(mrb_state *mrb, mrb_value self)
{
  return mrb_nil_value();
}

static mrb_value
mrb_game_close(mrb_state *mrb, mrb_value self)
{
  carbuncle_game_is_running = FALSE;
  return mrb_nil_value();
}

static mrb_value
mrb_game_resize(mrb_state *mrb, mrb_value self)
{
  mrb_int width, height;
  mrb_get_args(mrb, "ii", &width, &height);
  SetWindowSize(width, height);
  return mrb_nil_value();
}

/* Init Game class */

void
mrb_carbuncle_game_init(mrb_state *mrb, struct RClass *carbuncle)
{
  struct RClass *game = mrb_define_class_under(mrb, carbuncle, "Game", mrb->object_class);

  /* Class methods */

  mrb_define_class_method(mrb, game, "run", mrb_s_game_run, MRB_ARGS_NONE());

  mrb_define_class_method(mrb, game, "running?", mrb_s_game_runningQ, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, game, "current_game", mrb_s_game_get_current_game, MRB_ARGS_NONE());

  /* Getters */

  mrb_define_method(mrb, game, "screen", mrb_game_get_screen, MRB_ARGS_NONE());

  /* Instance Methods */
  mrb_define_method(mrb, game, "initialize", mrb_game_initialize, MRB_ARGS_NONE());
  mrb_define_method(mrb, game, "close?", mrb_game_closeQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, game, "resize", mrb_game_resize, MRB_ARGS_REQ(2));

  /* Game lifecycle */
  mrb_define_method(mrb, game, "load", mrb_game_dummy, MRB_ARGS_NONE());
  mrb_define_method(mrb, game, "update", mrb_game_dummy, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, game, "draw", mrb_game_dummy, MRB_ARGS_NONE());
  mrb_define_method(mrb, game, "close", mrb_game_close, MRB_ARGS_NONE());

  /* Lifecycle hooks */
  mrb_define_method(mrb, game, "before_run", mrb_game_dummy, MRB_ARGS_NONE());
  mrb_define_method(mrb, game, "after_run", mrb_game_dummy, MRB_ARGS_NONE());
  mrb_define_method(mrb, game, "before_close", mrb_game_dummy, MRB_ARGS_NONE());
  mrb_define_method(mrb, game, "after_close", mrb_game_dummy, MRB_ARGS_NONE());

  mrb_gv_set(mrb, CURRENT_GAME_SYMBOL, mrb_nil_value());
}

mrb_value
mrb_carbuncle_get_current_game(mrb_state *mrb)
{
  return mrb_gv_get(mrb, CURRENT_GAME_SYMBOL);
}

mrb_bool
mrb_carbuncle_is_current_game(mrb_state *mrb, mrb_value self)
{
  mrb_value current_game = mrb_carbuncle_get_current_game(mrb);
  return mrb_equal(mrb, self, current_game);
}