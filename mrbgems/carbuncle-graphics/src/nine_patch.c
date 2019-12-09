#include "carbuncle/core.h"
#include "carbuncle/nine_patch.h"

void
mrb_carbuncle_nine_patch_init(mrb_state *mrb)
{
  struct RClass *nine_patch = mrb_carbuncle_define_data_class(mrb, "NinePatch", mrb->object_class);
}
