#ifndef CARBUNCLE_MUSIC_H
#define CARBUNCLE_MUSIC_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_music_init(mrb_state *mrb);

Music *
mrb_carbuncle_get_music(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_music_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
