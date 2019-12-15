#ifndef CARBUNCLE_COLOR_H
#define CARBUNCLE_COLOR_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_init_carbuncle_color(mrb_state *mrb);

Color *
mrb_carbuncle_get_color(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_color_p(mrb_value obj);

mrb_value
mrb_carbuncle_color_new(mrb_state *mrb, mrb_int r, mrb_int g, mrb_int b, mrb_int a);

#ifdef __cplusplus
}
#endif

#endif
