#ifndef CARBUNCLE_VECTOR4_H
#define CARBUNCLE_VECTOR4_H

#include <mruby.h>

#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_vector4_init(mrb_state *mrb);

Vector4 *
mrb_carbuncle_get_vector4(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_vector4_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
