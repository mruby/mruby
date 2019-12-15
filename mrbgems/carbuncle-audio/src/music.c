#include "carbuncle/core.h"
#include "carbuncle/music.h"
#include "carbuncle/audio_manager.h"

#include <mruby/class.h>
#include <mruby/data.h>

static inline float
max(mrb_float a, mrb_float b)
{
  return a > b ? a : b;
}

static inline float
min(mrb_float a, mrb_float b)
{
  return a < b ? a : b;
}

struct mrb_Music
{
  Music data;
  mrb_float volume, pitch;
};

static void
mrb_music_free(mrb_state *mrb, void *p)
{
  if (p)
  {
    struct mrb_Music *music = p;
    if (IsMusicPlaying(music->data))
    {
      StopMusicStream(music->data);
    }
    carbuncle_audio_manager_unregister(&(music->data));
    UnloadMusicStream(music->data);
    mrb_free(mrb, p);
  }
}

static const struct mrb_data_type music_data_type = {
  "Carbuncle::Music", mrb_music_free
};

mrb_value
mrb_music_initialize(mrb_state *mrb, mrb_value self)
{
  const char *name;
  mrb_get_args(mrb, "z", &name);
  mrb_carbuncle_check_file(mrb, name);
  struct mrb_Music *music = mrb_malloc(mrb, sizeof *music);
  music->data = LoadMusicStream(name);
  SetMusicVolume(music->data, 1);
  SetMusicPitch(music->data, 1);
  music->volume = 1;
  music->pitch = 1;
  DATA_PTR(self) = music;
  DATA_TYPE(self) = &music_data_type;
  carbuncle_audio_manager_register(&(music->data));
  return self;
}

mrb_value
mrb_music_playingQ(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  return mrb_bool_value(IsMusicPlaying(*music));
}

mrb_value
mrb_music_get_rate(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  return mrb_float_value(mrb, GetMusicTimePlayed(*music) / GetMusicTimeLength(*music));
}

mrb_value
mrb_music_get_position(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  return mrb_float_value(mrb, GetMusicTimePlayed(*music));
}

mrb_value
mrb_music_get_size(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  return mrb_float_value(mrb, GetMusicTimeLength(*music));
}

mrb_value
mrb_music_get_volume(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_music(mrb, self);
  struct mrb_Music *ptr = DATA_PTR(self);
  return mrb_float_value(mrb, ptr->volume);
}

mrb_value
mrb_music_get_pitch(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_music(mrb, self);
  struct mrb_Music *ptr = DATA_PTR(self);
  return mrb_float_value(mrb, ptr->pitch);
}

mrb_value
mrb_music_set_volume(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  mrb_carbuncle_get_music(mrb, self);
  struct mrb_Music *ptr = DATA_PTR(self);
  ptr->volume = max(0, min(1, value));
  SetMusicVolume(ptr->data, ptr->volume);
  return mrb_float_value(mrb, ptr->volume);
}

mrb_value
mrb_music_set_pitch(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  mrb_carbuncle_get_music(mrb, self);
  struct mrb_Music *ptr = DATA_PTR(self);
  ptr->pitch = max(-1, min(1, value));
  SetMusicPitch(ptr->data, ptr->pitch + 1);
  return mrb_float_value(mrb, ptr->pitch);
}

static mrb_value
mrb_music_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}


static mrb_value
mrb_music_dispose(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  mrb_music_free(mrb, music);
  DATA_PTR(self) = NULL;
  return self;
}

mrb_value
mrb_music_play(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  PlayMusicStream(*music);
  return self;
}

mrb_value
mrb_music_pause(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  PauseMusicStream(*music);
  return self;
}

mrb_value
mrb_music_stop(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  StopMusicStream(*music);
  return self;
}

mrb_value
mrb_music_resume(mrb_state *mrb, mrb_value self)
{
  Music *music = mrb_carbuncle_get_music(mrb, self);
  ResumeMusicStream(*music);
  return self;
}

void
mrb_init_carbuncle_music(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  struct RClass *music = mrb_define_class_under(mrb, carbuncle, "Music", mrb->object_class);
  MRB_SET_INSTANCE_TT(music, MRB_TT_DATA);

  mrb_define_method(mrb, music, "initialize", mrb_music_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, music, "playing?", mrb_music_playingQ, MRB_ARGS_NONE());

  mrb_define_method(mrb, music, "rate", mrb_music_get_rate, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "position", mrb_music_get_position, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "size", mrb_music_get_size, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "length", mrb_music_get_size, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "count", mrb_music_get_size, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "volume", mrb_music_get_volume, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "pitch", mrb_music_get_pitch, MRB_ARGS_NONE());

  mrb_define_method(mrb, music, "volume=", mrb_music_set_volume, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, music, "pitch=", mrb_music_set_pitch, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, music, "disposed?", mrb_music_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "dispose", mrb_music_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, music, "play", mrb_music_play, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "pause", mrb_music_pause, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "stop", mrb_music_stop, MRB_ARGS_NONE());
  mrb_define_method(mrb, music, "resume", mrb_music_resume, MRB_ARGS_NONE());
}

Music *
mrb_carbuncle_get_music(mrb_state *mrb, mrb_value obj)
{
  struct mrb_Music *music = DATA_GET_DISPOSABLE_PTR(mrb, obj, &music_data_type, struct mrb_Music);
  return &(music->data);
}

mrb_bool
mrb_carbuncle_music_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &music_data_type);
}
