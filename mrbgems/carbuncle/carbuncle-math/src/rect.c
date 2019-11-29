#include "carbuncle/core.h"
#include "carbuncle/rect.h"

#include <mruby/data.h>

void
mrb_carbuncle_rect_init(mrb_state *mrb)
{
  struct RClass *rect = mrb_carbuncle_define_data_class(mrb, "Rect", mrb->object_class);

  mrb_value empty_rect = mrb_obj_freeze(mrb, mrb_obj_new(mrb, rect, 0, NULL));
  mrb_define_const(mrb, rect, "EMPTY", empty_rect);
}
