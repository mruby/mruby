#include <mruby.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/compile.h>
#include <mruby/variable.h>
#include <mruby/string.h>

#include <stdio.h>

/**
 * Test for https://github.com/mruby/mruby/issues/903
 */
static mrb_value
mrb_gc_protect_stress_test(mrb_state *mrb, mrb_value self)
{
  mrb_value val = mrb_str_new(mrb, "test", 4);

  while(mrb->arena_idx <= MRB_ARENA_SIZE)
    mrb_gc_protect(mrb, val);

  mrb_garbage_collect(mrb);

  return mrb_nil_value();
}

void
mrb_init_gc_test(mrb_state *mrb)
{
  struct RClass *gc_test;
  gc_test = mrb_define_module(mrb, "GCTest");

  mrb_define_module_function(mrb, gc_test, "gc_protect_stress_test", mrb_gc_protect_stress_test, ARGS_NONE());
}
