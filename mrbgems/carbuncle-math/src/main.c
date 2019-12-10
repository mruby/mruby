#include <mruby.h>

#include "carbuncle/matrix.h"
#include "carbuncle/point.h"
#include "carbuncle/rect.h"
#include "carbuncle/vector3.h"
#include "carbuncle/vector4.h"

void
mrb_carbuncle_math_gem_init(mrb_state *mrb)
{
  mrb_carbuncle_matrix_init(mrb);
  mrb_carbuncle_point_init(mrb);
  mrb_carbuncle_rect_init(mrb);
  mrb_carbuncle_vector3_init(mrb);
  mrb_carbuncle_vector4_init(mrb);
}

void
mrb_carbuncle_math_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
