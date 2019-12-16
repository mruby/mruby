#include "carbuncle/core.h"
#include "carbuncle/sound.h"

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
    UnloadSound(sound->data);
  }
}

static const struct mrb_data_type sound_data_type = {
  "Carbuncle::Sound", mrb_sound_free
};

/**
 * @overload initialize(filename)
 *   Creates a new Sound object from a filename
 *   @param [String] filename The name of the audio file.
 *   @raise [Carbuncle::FileNotFound] If the file does not exists.
 *   @return [self]
 */
mrb_value
mrb_sound_initialize(mrb_state *mrb, mrb_value self)
{
  const char *name;
  mrb_get_args(mrb, "z", &name);
  mrb_carbuncle_check_file(mrb, name);
  struct mrb_Sound *sound = mrb_malloc(mrb, sizeof *sound);
  sound->data = LoadSound(name);
  sound->volume = 1;
  sound->pitch = 1;
  DATA_PTR(self) = sound;
  DATA_TYPE(self) = &sound_data_type;
  return self;
}

/**
 * Checks if the sound is playing.
 * @return [Boolean]
 */
mrb_value
mrb_sound_playingQ(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  return mrb_bool_value(IsSoundPlaying(*sound));
}

/**
 * The sound's volume in the rangre from 0..1
 * @return [Float]
 */
mrb_value
mrb_sound_get_volume(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sound(mrb, self);
  struct mrb_Sound *ptr = DATA_PTR(self);
  return mrb_float_value(mrb, ptr->volume);
}

/**
 * The sound's pitch, in the range of -1..1
 * @return [Float]
 */
mrb_value
mrb_sound_get_pitch(mrb_state *mrb, mrb_value self)
{
  mrb_carbuncle_get_sound(mrb, self);
  struct mrb_Sound *ptr = DATA_PTR(self);
  return mrb_float_value(mrb, ptr->pitch);
}

/**
 * @return [Float]
 */
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

/**
 * @return [Float]
 */
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

/**
 * Checks if the sound is dosposed
 * @return [Boolean]
 */
static mrb_value
mrb_sound_disposedQ(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(!DATA_PTR(self));
}

/**
 * Frees the memory associated with this resource.
 * Be aware, this object can't be used anymore or an error will be thrown.
 * @return [nil]
 */
static mrb_value
mrb_sound_dispose(mrb_state *mrb, mrb_value self)
{
  Sound *music = mrb_carbuncle_get_sound(mrb, self);
  mrb_sound_free(mrb, music);
  DATA_PTR(self) = NULL;
  return self;
}

/**
 * Plays the sound.
 * @return [nil]
 */
mrb_value
mrb_sound_play(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  PlaySound(*sound);
  return self;
}

/**
 * Pauses the sound, allowing to be resumed later.
 * @return [nil]
 */
mrb_value
mrb_sound_pause(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  PauseSound(*sound);
  return self;
}

/**
 * Stops the sound, if it's playing, and resets it's position from 0.
 * @return [nil]
 */
mrb_value
mrb_sound_stop(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  StopSound(*sound);
  return self;
}

/**
 * Resumes the sound from the position it was.
 * @return [nil]
 */
mrb_value
mrb_sound_resume(mrb_state *mrb, mrb_value self)
{
  Sound *sound = mrb_carbuncle_get_sound(mrb, self);
  ResumeSound(*sound);
  return self;
}

void
mrb_init_carbuncle_sound(mrb_state *mrb)
{
  struct RClass *carbuncle = mrb_carbuncle_get(mrb);
  /**
   * A sound is any audio file loaded directly from memory and without looping.
   * Used for sound effects and small audio files.
   * @!attribute [rw] volume
   *   The sound's volume in the rangre from 0..1
   * @!attribute [rw] pitch
   *   The sound's pitch, in the range of -1..1
   */  
  struct RClass *sound = mrb_define_class_under(mrb, carbuncle, "Sound", mrb->object_class);
  MRB_SET_INSTANCE_TT(sound, MRB_TT_DATA);

  mrb_define_method(mrb, sound, "initialize", mrb_sound_initialize, MRB_ARGS_REQ(1));

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
