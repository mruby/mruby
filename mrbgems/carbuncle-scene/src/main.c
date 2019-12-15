#include <mruby.h>

#include "carbuncle/camera.h"

void
mrb_carbuncle_scene_gem_init(mrb_state *mrb)
{
  mrb_init_carbuncle_camera(mrb);
}

void
mrb_carbuncle_scene_gem_final(mrb_state *mrb)
{
}
