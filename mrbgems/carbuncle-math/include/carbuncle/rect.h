#ifndef CARBUNCLE_RECT_H
#define CARBUNCLE_RECT_H

#include <mruby.h>

#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_rect_init(mrb_state *mrb);

Rectangle *
mrb_carbuncle_get_rect(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_rect_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
