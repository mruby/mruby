#ifndef CARBUNCLE_SCREEN_H
#define CARBUNCLE_SCREEN_H

#include <mruby.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct Screen
{
  mrb_int width, height;
} Screen;

void
mrb_carbuncle_screen_init(mrb_state *mrb, struct RClass *carbuncle);

Screen *
mrb_carbuncle_get_screen(mrb_state *mrb, mrb_value obj);

mrb_bool
mrb_carbuncle_screen_p(mrb_value obj);

mrb_value
mrb_carbuncle_screen_new(mrb_state *mrb, mrb_value game);

#ifdef __cplusplus
}
#endif

#endif
