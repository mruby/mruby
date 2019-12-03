#include "carbuncle/core.h"
#include "carbuncle/audio.h"

#include "raylib.h"

mrb_value
mrb_audio_set_master_volume(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  SetMasterVolume(value);
  return mrb_float_value(mrb, value);
}

void
mrb_carbuncle_audio_init(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *audio = mrb_define_module_under(mrb, carbuncle, "Audio");

  mrb_define_module_function(mrb, audio, "master_volume=", mrb_audio_set_master_volume, MRB_ARGS_REQ(1));
}
