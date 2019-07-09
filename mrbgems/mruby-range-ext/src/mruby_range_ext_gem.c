#include "mruby.h"

void mrb_init_range_range_ext(mrb_state *mrb);
void mrb_init_range_string_ext(mrb_state *mrb);

#define DONE mrb_gc_arena_restore(mrb, 0)

void
mrb_mruby_range_ext_gem_init(mrb_state* mrb)
{
  mrb_init_range_range_ext(mrb); DONE;
  mrb_init_range_string_ext(mrb); DONE;
}

void
mrb_mruby_range_ext_gem_final(mrb_state* mrb)
{
}
