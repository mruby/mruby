#ifndef CARBUNCLE_AUIO_H
#define CARBUNCLE_AUIO_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_audio_init(mrb_state *mrb);

#ifdef __cplusplus
}
#endif

#endif
