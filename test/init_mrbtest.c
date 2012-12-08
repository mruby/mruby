#include "mruby.h"
#include "mruby/irep.h"
#include "mruby/dump.h"
#include "mruby/string.h"
#include "mruby/proc.h"

extern const char mrbtest_irep[];
extern const char mrbgemtest_irep[];

void
mrb_init_mrbtest(mrb_state *mrb)
{
  mrb_load_irep(mrb, mrbtest_irep);
#ifndef DISABLE_GEMS
  mrb_load_irep(mrb, mrbgemtest_irep);
#endif
  if (mrb->exc) {
    mrb_p(mrb, mrb_obj_value(mrb->exc));
    exit(0);
  }
}

