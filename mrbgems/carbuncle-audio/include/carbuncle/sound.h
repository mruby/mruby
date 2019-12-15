#ifndef CARBUNCLE_SOUND_H
#define CARBUNCLE_SOUND_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_init_carbuncle_sound(mrb_state *mrb);

Sound *
mrb_carbuncle_get_sound(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_sound_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
