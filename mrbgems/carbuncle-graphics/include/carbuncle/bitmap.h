#ifndef CARBUNCLE_BITMAP_H
#define CARBUNCLE_BITMAP_H

#include <mruby.h>

#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_bitmap_init(mrb_state *mrb);

Image *
mrb_carbuncle_get_bitmap(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_bitmap_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif