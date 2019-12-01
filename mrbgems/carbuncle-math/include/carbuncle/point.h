#ifndef CARBUNCLE_POINT_H
#define CARBUNCLE_POINT_H

#include <mruby.h>

#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_point_init(mrb_state *mrb);

Vector2 *
mrb_carbuncle_get_point(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_point_p(mrb_value obj);

mrb_value
mrb_carbuncle_point_new(mrb_state *mrb, mrb_float x, mrb_float y);

#ifdef __cplusplus
}
#endif

#endif
