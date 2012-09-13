/*
 * This file will contain a list of all
 * initializing methods necessary to
 * bootstrap every gem.
 *
 */

#include "mruby.h"

void mrb_md5_gem_init(mrb_state*);

void
mrb_init_mrbgems(mrb_state *mrb)
{
  mrb_md5_gem_init(mrb);
}
