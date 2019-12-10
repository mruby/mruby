#ifndef CARBUNCLE_AUDIO_MANAGER_H
#define CARBUNCLE_AUDIO_MANAGER_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
carbuncle_audio_manager_init(mrb_state *mrb);

void
carbuncle_audio_manager_register(Music *music);

void
carbuncle_audio_manager_unregister(Music *music);

void
carbuncle_audio_manager_update(void);

void
carbuncle_audio_manager_final(mrb_state *mrb);

#ifdef __cplusplus
}
#endif

#endif
