#include "carbuncle/audio_manager.h"

static Music **music_streams = NULL;
static size_t music_capa = 0;
static size_t music_size = 0;

static mrb_state *audio_mrb = NULL;

void
carbuncle_audio_manager_init(mrb_state *mrb)
{
  audio_mrb = mrb;
  music_size = 0;
  music_capa = 3;
  music_streams = mrb_malloc(mrb, music_capa * (sizeof *music_streams));
}

void
carbuncle_audio_manager_register(Music *music)
{
  if (music_size >= music_capa)
  {
    music_capa *= 2 + 1;
    music_streams = mrb_realloc(audio_mrb, music_streams, music_capa * (sizeof *music_streams));
  }
  music_streams[music_size] = music;
  ++music_size;
}

void
carbuncle_audio_manager_unregister(Music *music)
{
  for (size_t i = 0; i < music_size; ++i)
  {
    if (music_streams[i] == music)
    {
      for (size_t j = i; j < music_size; ++j)
      {
        if (j > 0)
        {
          music_streams[j - 1] = music_streams[j];
        }
      }
      --music_size;
      return;
    }
  }
}

#include <stdio.h>

void
carbuncle_audio_manager_update(void)
{
  for (size_t i = 0; i < music_size; ++i)
  {
    Music music = *(music_streams[i]);
    if (IsMusicPlaying(music)) { UpdateMusicStream(music); }
  }
}

void
carbuncle_audio_manager_final(mrb_state *mrb)
{
  mrb_free(mrb, music_streams);
}
