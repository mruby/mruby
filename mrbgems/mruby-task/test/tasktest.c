#include <time.h>
#include <mruby.h>
#include <mruby/class.h>

/* Burn CPU until `ms` milliseconds of CPU time have elapsed, then raise.
   Blocking longer than MRB_TICK_UNIT * MRB_TIMESLICE_TICK_COUNT guarantees
   the tick handler has expired the running task's timeslice, so
   mrb->task.switching is pending when mrb_raise unwinds into the VM's
   catch-handler dispatch — the window where a pending switch used to
   swallow a handled exception into the task result. */
static mrb_value
tasktest_block_then_raise(mrb_state *mrb, mrb_value self)
{
  mrb_int ms;
  mrb_get_args(mrb, "i", &ms);
  clock_t end = clock() + (clock_t)(((double)ms / 1000.0) * (double)CLOCKS_PER_SEC);
  while (clock() < end) {
    /* busy-wait; ticks keep firing */
  }
  mrb_raise(mrb, E_RUNTIME_ERROR, "raised after blocking");
  return mrb_nil_value(); /* not reached */
}

void
mrb_mruby_task_gem_test(mrb_state* mrb)
{
  struct RClass *tasktest = mrb_define_module(mrb, "TaskTest");
  mrb_define_module_function(mrb, tasktest, "block_then_raise", tasktest_block_then_raise, MRB_ARGS_REQ(1));
}
