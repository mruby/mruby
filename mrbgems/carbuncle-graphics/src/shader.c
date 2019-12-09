#include "carbuncle/core.h"
#include "carbuncle/shader.h"

void
mrb_carbuncle_shader_init(mrb_state *mrb)
{
  struct RClass *shader = mrb_carbuncle_define_data_class(mrb, "Shader", mrb->object_class);
}
