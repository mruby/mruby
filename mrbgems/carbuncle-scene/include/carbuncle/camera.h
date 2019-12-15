#ifndef CARBUNCLE_CAMERA_H
#define CARBUNCLE_CAMERA_H

#include <mruby.h>

#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

struct mrb_Camera
{
  Vector2  *target;
  Vector2  *offset;
  mrb_float angle;
  mrb_float scale;
};

void
mrb_init_carbuncle_camera(mrb_state *mrb);

struct mrb_Camera *
mrb_carbuncle_get_camera(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_camera_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
