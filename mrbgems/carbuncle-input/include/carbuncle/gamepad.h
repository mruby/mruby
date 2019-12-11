#ifndef CARBUNCLE_GAMEPAD_H
#define CARBUNCLE_GAMEPAD_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_gamepad_init(mrb_state *mrb);

#ifdef __cplusplus
}
#endif

#endif
