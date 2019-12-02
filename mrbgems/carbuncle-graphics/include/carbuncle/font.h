#ifndef CARBUNCLE_FONT_H
#define CARBUNCLE_FONT_H

#include <mruby.h>
#include "raylib.h"

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_font_init(mrb_state *mrb);

Font *
mrb_carbuncle_get_font(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_font_p(mrb_value obj);

#ifdef __cplusplus
}
#endif

#endif
