#ifndef CARBUNCLE_RECT_H
#define CARBUNCLE_RECT_H

#include <mruby.h>

#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_init_carbuncle_rect(mrb_state *mrb);

Rectangle *
mrb_carbuncle_get_rect(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_rect_p(mrb_value obj);

mrb_value
mrb_carbuncle_rect_new(mrb_state *mrb, mrb_float x, mrb_float y, mrb_float w, mrb_float h);

#ifdef __cplusplus
}
#endif

#endif
