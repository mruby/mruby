#include <mruby.h>

#include "carbuncle/matrix.h"
#include "carbuncle/point.h"
#include "carbuncle/rect.h"
#include "carbuncle/vector3.h"
#include "carbuncle/vector4.h"

void
mrb_carbuncle_math_gem_init(mrb_state *mrb)
{
  mrb_init_carbuncle_matrix(mrb);
  mrb_init_carbuncle_point(mrb);
  mrb_init_carbuncle_rect(mrb);
  mrb_init_carbuncle_vector3(mrb);
  mrb_init_carbuncle_vector4(mrb);
}

void
mrb_carbuncle_math_gem_final(mrb_state *mrb)
{
  /* finalizer */
}
