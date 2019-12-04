#include "carbuncle/audio_manager.h"

struct AudioEntry
{
  Music *value;
  struct AudioEntry *next;
  struct AudioEntry *prev;
};

static struct AudioEntry *MUSIC_STREAMS = NULL;
static mrb_state *AUDIO_MRB = NULL;

void
add_music(struct AudioEntry *prev, struct AudioEntry **entry, Music *music)
{
  struct AudioEntry *current = *entry;
  if (current)
  {
    add_music(*entry, &(current->next), music);
  }
  else
  {
    current = *entry = mrb_malloc(AUDIO_MRB, sizeof *current);
    prev->next = *entry;
    current->prev = prev;
    current->next = NULL;
    current->value = music;
  }
}

void
remove_music(struct AudioEntry **entry, Music *music)
{
  struct AudioEntry *current = *entry;
  if (!current)
  {
    return;
  }
  if (current->value != music)
  {
    remove_music(&(current->next), music);
    return;
  }
  if (current->next) { current->next->prev = current->prev; }
  if (current->prev) { current->prev->next = current->next; }
  else { MUSIC_STREAMS = current->next; }
  mrb_free(AUDIO_MRB, current);
}

void
carbuncle_audio_manager_init(mrb_state *mrb)
{
  AUDIO_MRB = mrb;
}

void
carbuncle_audio_manager_register(Music *music)
{
  add_music(NULL, &MUSIC_STREAMS, music);
}

void
carbuncle_audio_manager_unregister(Music *music)
{
  remove_music(&MUSIC_STREAMS, music);
}

void
carbuncle_audio_manager_update(void)
{
  struct AudioEntry *current = MUSIC_STREAMS;
  while (current)
  {
    Music music = *(current->value);
    if (IsMusicPlaying(music)) { UpdateMusicStream(music); }
    current = current->next;
  }
}
