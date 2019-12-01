#ifndef CARBUNCLE_TEXTURE_H
#define CARBUNCLE_TEXTURE_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_texture_init(mrb_state *mrb);

Texture2D *
mrb_carbuncle_get_texture(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_texture_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
