#ifndef CARBUNCLE_VECTOR3_H
#define CARBUNCLE_VECTOR3_H

#include <mruby.h>

#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_vector3_init(mrb_state *mrb);

Vector3 *
mrb_carbuncle_get_vector3(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_vector3_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
