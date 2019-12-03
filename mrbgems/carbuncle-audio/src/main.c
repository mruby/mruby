#include <mruby.h>

#include "carbuncle/audio.h"
#include "carbuncle/music.h"
#include "carbuncle/sound.h"

void
mrb_carbuncle_audio_gem_init(mrb_state *mrb)
{
  mrb_carbuncle_audio_init(mrb);
  mrb_carbuncle_music_init(mrb);
  mrb_carbuncle_sound_init(mrb);
}

void
mrb_carbuncle_audio_gem_final(mrb_state *mrb)
{
  /* finalizer */
}