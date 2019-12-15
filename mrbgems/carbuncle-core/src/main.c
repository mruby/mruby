#include <mruby.h>

#include <stdlib.h>

#include "raylib.h"

#include "carbuncle/game.h"
#include "carbuncle/screen.h"
#include "carbuncle/audio_manager.h"

#ifdef CARBUNCLE_DEBUG
mrb_bool mrb_carbuncle_debug_drawing = FALSE;

static mrb_value
mrb_s_carbuncle_debug_drawQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(mrb_carbuncle_debug_drawing);
}

static mrb_value
mrb_s_carbuncle_toggle_debug_draw(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_debug_drawing = !mrb_carbuncle_debug_drawing;
  return mrb_bool_value(mrb_carbuncle_debug_drawing);
}

#endif

void
mrb_carbuncle_core_gem_init(mrb_state *mrb)
{
  InitAudioDevice();
  carbuncle_audio_manager_init(mrb);
  struct RClass *carbuncle = mrb_define_module(mrb, "Carbuncle");
  
  struct RClass *base_error = mrb_define_class_under(mrb, carbuncle, "Error", mrb->eStandardError_class);

  mrb_define_class_under(mrb, carbuncle, "GameIsRunning", base_error);
  mrb_define_class_under(mrb, carbuncle, "FileNotExists", base_error);
  mrb_define_class_under(mrb, carbuncle, "DisposedObject", base_error);

  mrb_init_carbuncle_game(mrb);
  mrb_init_carbuncle_screen(mrb);

#ifdef CARBUNCLE_DEBUG
  mrb_define_module_function(mrb, carbuncle, "debug_draw?", mrb_s_carbuncle_debug_drawQ, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, carbuncle, "toggle_debug_draw", mrb_s_carbuncle_toggle_debug_draw, MRB_ARGS_NONE());
#endif
}

void
mrb_carbuncle_core_gem_final(mrb_state *mrb)
{
  carbuncle_audio_manager_final(mrb);
  CloseAudioDevice();
}
