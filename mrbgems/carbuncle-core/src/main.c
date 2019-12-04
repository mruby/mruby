#include <mruby.h>

#include <stdlib.h>

#include "raylib.h"

#include "carbuncle/game.h"
#include "carbuncle/screen.h"
#include "carbuncle/audio_manager.h"

void
mrb_carbuncle_core_gem_init(mrb_state *mrb)
{
  carbuncle_audio_manager_init(mrb);
  struct RClass *carbuncle = mrb_define_module(mrb, "Carbuncle");
  
  struct RClass *base_error = mrb_define_class_under(mrb, carbuncle, "Error", mrb->eStandardError_class);

  mrb_define_class_under(mrb, carbuncle, "GameIsRunning", base_error);
  mrb_define_class_under(mrb, carbuncle, "FileNotExists", base_error);
  mrb_define_class_under(mrb, carbuncle, "DisposedObject", base_error);

  mrb_carbuncle_game_init(mrb, carbuncle);
  mrb_carbuncle_screen_init(mrb, carbuncle);
}

void
mrb_carbuncle_core_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
