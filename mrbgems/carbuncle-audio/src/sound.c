#include "carbuncle/core.h"
#include "carbuncle/sound.h"

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

struct mrb_Sound
{
  Sound data;
  mrb_float volume, pitch;
};

static void
mrb_sound_free(mrb_state *mrb, void *p)
{
  if (p)
  {
    struct mrb_Sound *sound = p;
    if (IsSoundPlaying(sound->data))
    {
      StopSound(sound->data);
    }
    CloseSound(sound->data);
  }
}

static const struct mrb_data_type sound_data_type = {
  "Carbuncle::Sound", mrb_sound_free
};

mrb_value
mrb_sound_initialize(mrb_state *mrb, mrb_value self)
{
  return self;
}

mrb_value
mrb_sound_playingQ(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  return mrb_bool_value(IsSoundPlaying(*sound));
}

mrb_value
mrb_sound_get_volume(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sound(mrb, self);
  struct mrb_Sound *ptr = DATA_PTR(self);
  return mrb_float_value(mrb, ptr->volume);
}

mrb_value
mrb_sound_get_pitch(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sound(mrb, self);
  struct mrb_Sound *ptr = DATA_PTR(self);
  return mrb_float_value(mrb, ptr->pitch);
}

mrb_value
mrb_sound_set_volume(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  mrb_carbuncle_get_sound(mrb, self);
  struct mrb_Sound *ptr = DATA_PTR(self);
  ptr->volume = max(0, min(1, value));
  SetSoundVolume(ptr->data, ptr->volume);
  return mrb_float_value(mrb, ptr->volume);
}

mrb_value
mrb_sound_set_pitch(mrb_state *mrb, mrb_value self)
{
  mrb_float value;
  mrb_get_args(mrb, "f", &value);
  mrb_carbuncle_get_sound(mrb, self);
  struct mrb_Sound *ptr = DATA_PTR(self);
  ptr->pitch = max(-1, min(1, value));
  SetSoundPitch(ptr->data, ptr->pitch + 1);
  return mrb_float_value(mrb, ptr->pitch);
}

static mrb_value
mrb_sound_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}


static mrb_value
mrb_sound_dispose(mrb_state *mrb, mrb_value self)
{
  Sound *music = mrb_carbuncle_get_sound(mrb, self);
  mrb_sound_free(mrb, music);
  DATA_PTR(self) = NULL;
  return self;
}

mrb_value
mrb_sound_play(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  PlaySound(*sound);
  return self;
}

mrb_value
mrb_sound_pause(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  PauseSound(*sound);
  return self;
}

mrb_value
mrb_sound_stop(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  StopSound(*sound);
  return self;
}

mrb_value
mrb_sound_resume(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  ResumeSound(*sound);
  return self;
}

void
mrb_carbuncle_sound_init(mrb_state *mrb)
{
  struct RClass *sound = mrb_carbuncle_define_data_class(mrb, "Sound", mrb->object_class);

  mrb_define_method(mrb, sound, "initialize", mrb_sound_initialize, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, sound, "initialize_copy", mrb_sound_initialize, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, sound, "playing?", mrb_sound_playingQ, MRB_ARGS_NONE());

  mrb_define_method(mrb, sound, "volume", mrb_sound_get_volume, MRB_ARGS_NONE());
  mrb_define_method(mrb, sound, "pitch", mrb_sound_get_pitch, MRB_ARGS_NONE());

  mrb_define_method(mrb, sound, "volume=", mrb_sound_set_volume, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, sound, "pitch=", mrb_sound_set_pitch, MRB_ARGS_REQ(1));

  mrb_define_method(mrb, sound, "disposed?", mrb_sound_disposedQ, MRB_ARGS_NONE());
  mrb_define_method(mrb, sound, "dispose", mrb_sound_dispose, MRB_ARGS_NONE());

  mrb_define_method(mrb, sound, "play", mrb_sound_play, MRB_ARGS_NONE());
  mrb_define_method(mrb, sound, "pause", mrb_sound_pause, MRB_ARGS_NONE());
  mrb_define_method(mrb, sound, "stop", mrb_sound_stop, MRB_ARGS_NONE());
  mrb_define_method(mrb, sound, "resume", mrb_sound_resume, MRB_ARGS_NONE());
}

Sound *
mrb_carbuncle_get_sound(mrb_state *mrb, mrb_value obj)
{
  return DATA_GET_DISPOSABLE_PTR(mrb, obj, &sound_data_type, Sound);
}

mrb_bool
mrb_carbuncle_sound_p(mrb_value obj)
{
  return mrb_data_p(obj) && (DATA_TYPE(obj) == &sound_data_type);
}
