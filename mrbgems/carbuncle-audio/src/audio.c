#include "carbuncle/core.h"
#include "carbuncle/audio.h"

#include "raylib.h"

/**
 * @overload master_volume=(value)
 *   Set's the speaker's volume.
 *   @param [Float] value
 */
mrb_value
mrb_audio_set_master_volume(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  SetMasterVolume(value);
  return mrb_float_value(mrb, value);
}

/**
 * Indicates if the audio devise is ready.
 * @return [Boolean]
 */
mrb_value
mrb_audio_readyQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(IsAudioDeviceReady());
}

void
mrb_init_carbuncle_audio(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  /**
   * This class handles all global settings about audio.
   */
  struct RClass *audio = mrb_define_module_under(mrb, carbuncle, "Audio");

  mrb_define_module_function(mrb, audio, "master_volume=", mrb_audio_set_master_volume, MRB_ARGS_REQ(1));

  mrb_define_module_function(mrb, audio, "ready?", mrb_audio_readyQ, MRB_ARGS_NONE());
}
