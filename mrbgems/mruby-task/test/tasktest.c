#include <time.h>
#include <mruby.h>
#include <mruby/class.h>
#include <mruby/proc.h>
#include <mruby/data.h>
#include <mruby/array.h>
#include <mruby/error.h>
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

static mrb_value
tasktest_yield_nil(mrb_state *mrb, void *ud)
{
  return mrb_yield_argv(mrb, mrb_obj_value((struct RProc*)ud), 0, NULL);
}

/* mrb_disable_task_scheduler() is the embedder's call, so the test makes it
   from C. What Ruby can then check is the promise it makes: a VM that
   already has tasks refuses, because a queue nobody ticks is a hang. */
static mrb_value
tasktest_disable_tasks(mrb_state *mrb, mrb_value self)
{
  mrb_disable_task_scheduler(mrb);
  return mrb_nil_value();
}

/* The flag off for the length of a block, so the refusal can be tested
   in a VM that is otherwise running the suite's own tasks. Not an API -
   mrb_disable_task_scheduler() is one-way on purpose. */
static mrb_value
tasktest_with_tasks_disabled(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;
  mrb_get_args(mrb, "&", &blk);
  mrb->task.enabled = FALSE;
  mrb_bool error = FALSE;
  mrb_value ret = mrb_protect_error(mrb, tasktest_yield_nil, mrb_ptr(blk), &error);
  mrb->task.enabled = TRUE;
  /* mrb_protect_error returns the exception as an ordinary value. Without
     this, a raise in the block looks like a result and the test passes. */
  if (error) mrb_exc_raise(mrb, ret);
  return ret;
}

static mrb_value
tasktest_tasks_enabled(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(mrb_task_scheduler_enabled_p(mrb));
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

/* Regression for the undersized-stack overflow: creating a task sizes its
   stack for the INITIAL proc only. Setting a larger proc afterwards (the
   picoruby-sandbox reset_context + proc_set path) must grow the stack to
   cover the new proc's nregs, otherwise mrb_vm_exec()/OP_ENTER writes past
   the stack. Returns [stack_slots, replacement_nregs]. */
static mrb_value
tasktest_proc_set_stack(mrb_state *mrb, mrb_value self)
{
  mrb_value small_blk, big_blk;
  mrb_get_args(mrb, "oo", &small_blk, &big_blk);
  struct RProc *small = mrb_proc_ptr(small_blk);
  struct RProc *big = mrb_proc_ptr(big_blk);

  mrb_value task = mrb_create_task(mrb, small, mrb_nil_value(),
                                   mrb_nil_value(), mrb_obj_value(mrb->top_self));
  mrb_task_reset_context(mrb, task);
  mrb_task_proc_set(mrb, task, big);

  mrb_task *t = (mrb_task*)DATA_PTR(task);
  mrb_value r[2];
  r[0] = mrb_fixnum_value((mrb_int)(t->c.stend - t->c.stbase));
  r[1] = mrb_fixnum_value((mrb_int)big->body.irep->nregs);
  /* Never run this probe task: unschedule it so a deliberately undersized
     stack (unfixed build) fails as a clean assertion, not a later crash. */
  mrb_terminate_task(mrb, task);
  return mrb_ary_new_from_values(mrb, 2, r);
}

struct extend_probe {
  struct mrb_context *c;
  mrb_int room;
};

static mrb_value
extend_probe_body(mrb_state *mrb, void *ud)
{
  struct extend_probe *a = (struct extend_probe*)ud;
  mrb_stack_extend(mrb, a->room);
  return mrb_fixnum_value((mrb_int)(a->c->stend - a->c->stbase));
}

/* Regression for the frame-offset floor in stack_extend_alloc(): a context
   whose ci->stack sits past stend has to grow all the same. That is what an
   undersized context looks like from inside the VM, and is the state this
   gem reached before #7279, since cipush places a frame before the stack is
   extended. Reading the floor as `stend - ci->stack` rather than
   `ci->stack - stbase` made the unsigned subtraction wrap, and the growth
   math then asked for a stack no allocator could give: NoMemoryError instead
   of a bigger one.

   A task's own context is the probe, so the running VM's stack is never the
   one moved. Returns the stack size after the extend, or nil where it
   raised. */
static mrb_value
tasktest_extend_past_stend(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;
  mrb_int over, room;
  mrb_get_args(mrb, "oii", &blk, &over, &room);

  mrb_value task = mrb_create_task(mrb, mrb_proc_ptr(blk), mrb_nil_value(),
                                   mrb_nil_value(), mrb_obj_value(mrb->top_self));
  mrb_task *t = (mrb_task*)DATA_PTR(task);

  t->c.ci->stack = t->c.stend + over;

  struct extend_probe probe;
  probe.c = &t->c;
  probe.room = room;

  struct mrb_context *saved = mrb->c;
  mrb_bool err = FALSE;
  mrb->c = &t->c;
  mrb_value size = mrb_protect_error(mrb, extend_probe_body, &probe, &err);
  mrb->c = saved;

  /* Never run the probe task; its context was deliberately made incoherent. */
  mrb_terminate_task(mrb, task);
  return err ? mrb_nil_value() : size;
}

/* Deep-frame variant of the proc_set sizing regression: a task suspended
   mid-call-chain holds its current frame at an offset from the stack base,
   and proc_set installs the proc on that frame. The stack must be sized
   from the frame offset, not the total capacity. Returns
   [frame_off, replacement_nregs, slots_before, slots_after]. */
struct proc_set_deep_ctx {
  mrb_value task;
  struct RProc *proc;
};

static mrb_value
proc_set_deep_body(mrb_state *mrb, void *data)
{
  struct proc_set_deep_ctx *ctx = (struct proc_set_deep_ctx*)data;
  mrb_task_proc_set(mrb, ctx->task, ctx->proc);
  return mrb_nil_value();
}

static mrb_value
tasktest_proc_set_deep(mrb_state *mrb, mrb_value self)
{
  mrb_value task, big_blk;
  mrb_get_args(mrb, "oo", &task, &big_blk);
  if (!mrb_obj_is_kind_of(mrb, task, mrb_class_get(mrb, "Task"))) {
    mrb_raise(mrb, E_TYPE_ERROR, "Task required");
  }
  if (mrb_type(big_blk) != MRB_TT_PROC) {
    mrb_raise(mrb, E_TYPE_ERROR, "Proc required");
  }
  struct RProc *big = mrb_proc_ptr(big_blk);
  if (MRB_PROC_CFUNC_P(big) || MRB_PROC_ALIAS_P(big) || !big->body.irep) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "irep proc required");
  }

  mrb_task *t = (mrb_task*)DATA_PTR(task);
  if (!t || !t->c.stbase || !t->c.ci) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "task has no context");
  }
  mrb_value r[4];
  r[0] = mrb_fixnum_value((mrb_int)(t->c.ci->stack - t->c.stbase));
  r[1] = mrb_fixnum_value((mrb_int)big->body.irep->nregs);
  r[2] = mrb_fixnum_value((mrb_int)(t->c.stend - t->c.stbase));

  struct proc_set_deep_ctx ctx = { task, big };
  mrb_value result;
  MRB_ENSURE(mrb, result, proc_set_deep_body, &ctx) {
    r[3] = mrb_fixnum_value((mrb_int)(t->c.stend - t->c.stbase));
    /* Drop the probe task even when proc_set raises: a sleep-forever task
       left suspended keeps a later Task.run from terminating. It is never
       resumed either way, the proc was installed on a deep frame only to
       measure the sizing. */
    mrb_terminate_task(mrb, task);
  }
  return mrb_ary_new_from_values(mrb, 4, r);
}

void
mrb_mruby_task_gem_test(mrb_state* mrb)
{
  struct RClass *tasktest = mrb_define_module(mrb, "TaskTest");
  mrb_define_module_function(mrb, tasktest, "disable_tasks", tasktest_disable_tasks, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, tasktest, "tasks_enabled?", tasktest_tasks_enabled, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, tasktest, "with_tasks_disabled", tasktest_with_tasks_disabled, MRB_ARGS_BLOCK());
  mrb_define_module_function(mrb, tasktest, "block_then_raise", tasktest_block_then_raise, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, tasktest, "install_probe_hook", tasktest_install_probe_hook, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, tasktest, "probe_count", tasktest_probe_count, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, tasktest, "install_wake_hook", tasktest_install_wake_hook, MRB_ARGS_REQ(1));
  mrb_define_module_function(mrb, tasktest, "clear_hook", tasktest_clear_hook, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, tasktest, "run_once", tasktest_run_once, MRB_ARGS_NONE());
  mrb_define_module_function(mrb, tasktest, "reinit_context", tasktest_reinit_context, MRB_ARGS_REQ(1) | MRB_ARGS_BLOCK());
  mrb_define_module_function(mrb, tasktest, "run_sync", tasktest_run_sync, MRB_ARGS_BLOCK());
  mrb_define_module_function(mrb, tasktest, "proc_set_stack", tasktest_proc_set_stack, MRB_ARGS_REQ(2));
  mrb_define_module_function(mrb, tasktest, "extend_past_stend", tasktest_extend_past_stend, MRB_ARGS_REQ(3));
  mrb_define_module_function(mrb, tasktest, "proc_set_deep", tasktest_proc_set_deep, MRB_ARGS_REQ(2));
}
