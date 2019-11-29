#ifndef CARBUNCLE_RECT_H
#define CARBUNCLE_RECT_H

#include <mruby.h>

#ifdef __cplusplus
extern "C" {
#endif

mrb_bool
mrb_carbuncle_rect_p(mrb_state *mrb, mrb_value obj);

void
mrb_carbuncle_rect_init(mrb_state *mrb);


#ifdef __cplusplus
}
#endif

#endif
