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
  int n = mrb_read_irep(mrb, mrbtest_irep);
  int m = mrb_read_irep(mrb, mrbgemtest_irep);

  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[n]), mrb_top_self(mrb));
  mrb_run(mrb, mrb_proc_new(mrb, mrb->irep[m]), mrb_top_self(mrb));
  if (mrb->exc) {
    mrb_p(mrb, mrb_obj_value(mrb->exc));
    exit(0);
  }
}

