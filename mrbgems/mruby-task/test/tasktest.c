#include <time.h>
#include <mruby.h>
#include <mruby/class.h>
#include <mruby/proc.h>
#include "task.h"

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

/* Scheduler-hook probes. Two counters so the replace semantics can be
   observed: which counter grows tells which hook is installed. */
static uint32_t probe_count_a;
static uint32_t probe_count_b;

static void
tasktest_probe_hook(mrb_state *mrb, void *ud)
{
  (void)mrb;
  uint32_t *count = (uint32_t *)ud;
  (*count)++;
}

/* One-shot wake hook: on its first invocation after arming, push 42 into
   the armed queue. The queue object is kept alive by the Ruby test scope;
   the static mrb_value only mirrors it for the C callback. */
static mrb_value wake_queue;
static mrb_bool wake_armed;

static void
tasktest_wake_hook(mrb_state *mrb, void *ud)
{
  (void)ud;
  if (wake_armed) {
    wake_armed = FALSE;
    mrb_task_queue_push(mrb, wake_queue, mrb_fixnum_value(42));
  }
}

static mrb_value
tasktest_install_probe_hook(mrb_state *mrb, mrb_value self)
{
  mrb_int which;
  uint32_t *count;
  mrb_get_args(mrb, "i", &which);
  count = (which == 0) ? &probe_count_a : &probe_count_b;
  *count = 0;
  mrb_task_set_scheduler_hook(mrb, tasktest_probe_hook, count);
  return mrb_nil_value();
}

static mrb_value
tasktest_probe_count(mrb_state *mrb, mrb_value self)
{
  mrb_int which;
  mrb_get_args(mrb, "i", &which);
  return mrb_fixnum_value((mrb_int)((which == 0) ? probe_count_a : probe_count_b));
}

static mrb_value
tasktest_install_wake_hook(mrb_state *mrb, mrb_value self)
{
  mrb_value q;
  mrb_get_args(mrb, "o", &q);
  wake_queue = q;
  wake_armed = TRUE;
  mrb_task_set_scheduler_hook(mrb, tasktest_wake_hook, NULL);
  return mrb_nil_value();
}

static mrb_value
tasktest_clear_hook(mrb_state *mrb, mrb_value self)
{
  mrb_task_set_scheduler_hook(mrb, NULL, NULL);
  return mrb_nil_value();
}

/* Drive the mrb_task_run_once scheduler entry, which has no Ruby-facing
   wrapper (Task.run covers task_run_body; Task.pass covers the
   root-context helper). */
static mrb_value
tasktest_run_once(mrb_state *mrb, mrb_value self)
{
  return mrb_task_run_once(mrb);
}


/* Drive mrb_task_init_context(): reuse an existing task's context for a
   new proc. The old context's stack is freed inside, which is where an
   escaped env must have been detached. */
static mrb_value
tasktest_reinit_context(mrb_state *mrb, mrb_value self)
{
  mrb_value task, blk;
  mrb_get_args(mrb, "o&", &task, &blk);
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "block required");
  }
  mrb_task_init_context(mrb, task, mrb_proc_ptr(blk));
  return mrb_nil_value();
}

/* Drive the synchronous-execution teardown path. */
static mrb_value
tasktest_run_sync(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;
  mrb_get_args(mrb, "&", &blk);
  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "block required");
  }
  return mrb_execute_proc_synchronously(mrb, blk, 0, NULL);
}

void
mrb_mruby_task_gem_test(mrb_state* mrb)
{
  struct RClass *tasktest = mrb_define_module(mrb, "TaskTest");
  mrb_define_module_function(mrb, tasktest, "block_then_raise", tasktest_block_then_raise, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, tasktest, "install_probe_hook", tasktest_install_probe_hook, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, tasktest, "probe_count", tasktest_probe_count, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, tasktest, "install_wake_hook", tasktest_install_wake_hook, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, tasktest, "clear_hook", tasktest_clear_hook, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, tasktest, "run_once", tasktest_run_once, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, tasktest, "reinit_context", tasktest_reinit_context, MRB_ARGS_REQ(1) | MRB_ARGS_BLOCK());
  mrb_define_module_function(mrb, tasktest, "run_sync", tasktest_run_sync, MRB_ARGS_BLOCK());
}
