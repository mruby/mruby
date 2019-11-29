#ifndef CARBUNCLE_GAME_H
#define CARBUNCLE_GAME_H

#include <mruby.h>

#ifdef __cplusplus
extern "C" {
#endif

void
mrb_carbuncle_game_init(mrb_state *mrb, struct RClass *carbuncle);

mrb_value
mrb_carbuncle_get_current_game(mrb_state *mrb);

mrb_bool
mrb_carbuncle_is_current_game(mrb_state *mrb, mrb_value self);

#ifdef __cplusplus
}
#endif

#endif
