/*
** task.c - Task scheduler
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/error.h>
#include <mruby/gc.h>
#include <mruby/hash.h>
#include <mruby/internal.h>
#include <mruby/presym.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "task.h"
#include "task_hal.h"

/*
 * Queue helper macros
 */
#define q_dormant_   (mrb->task.queues[MRB_TASK_QUEUE_DORMANT])
#define q_ready_     (mrb->task.queues[MRB_TASK_QUEUE_READY])
#define q_waiting_   (mrb->task.queues[MRB_TASK_QUEUE_WAITING])
#define q_suspended_ (mrb->task.queues[MRB_TASK_QUEUE_SUSPENDED])
#define tick_        (mrb->task.tick)
#define wakeup_tick_ (mrb->task.wakeup_tick)
#define switching_   (mrb->task.switching)

/* Get task from current context using pointer arithmetic */
#define MRB2TASK(mrb) ((mrb_task *)((uint8_t *)mrb->c - offsetof(mrb_task, c)))

/* Get task pointer from self with validation */
#define TASK_GET_PTR_OR_RAISE(var, self) \
  do { \
    (var) = (mrb_task*)mrb_data_get_ptr(mrb, (self), &mrb_task_type); \
    if (!(var)) { \
      mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task"); \
    } \
  } while (0)

/* Convert microseconds to tick count */
#define USEC_TO_TICKS(usec) (((usec) / 1000) / MRB_TICK_UNIT)

/*
 * Task data type for GC
 */
static void
mrb_task_free(mrb_state *mrb, void *ptr)
{
  mrb_task *t = (mrb_task*)ptr;
  if (t) {
    /* Unregister from GC protection (unless it's the main task during shutdown) */
    if (t != mrb->task.main_task) {
      mrb_gc_unregister(mrb, t->self);
    }

    /* Free context resources - always free if allocated */
    /* Main task never has allocated context (stbase/cibase are NULL) */
    if (t->c.stbase) {
      mrb_free(mrb, t->c.stbase);
    }
    if (t->c.cibase) {
      mrb_free(mrb, t->c.cibase);
    }

    /* Free the task structure itself */
    mrb_free(mrb, t);
  }
}

static const struct mrb_data_type mrb_task_type = {
  "Task", mrb_task_free,
};

/*
 * GC marking function for all tasks
 * Called from gc.c during root_scan_phase
 */
void
mrb_task_mark_all(mrb_state *mrb)
{
  int qi;
  for (qi = 0; qi < 4; qi++) {
    mrb_task *t = mrb->task.queues[qi];
    while (t) {
      struct mrb_context *c = &t->c;
      mrb_callinfo *ci;
      size_t i, e;

      /* Mark task's stack */
      if (c->stbase) {
        if (c->ci) {
          e = (c->ci->stack ? c->ci->stack - c->stbase : 0);
          e += mrb_ci_nregs(c->ci);
        }
        else {
          e = 0;
        }
        if (c->stbase + e > c->stend) e = c->stend - c->stbase;
        for (i = 0; i < e; i++) {
          mrb_gc_mark_value(mrb, c->stbase[i]);
        }
      }

      /* Mark call stack */
      if (c->cibase && c->ci) {
        for (ci = c->cibase; ci <= c->ci; ci++) {
          if (ci->proc) {
            mrb_gc_mark(mrb, (struct RBasic*)ci->proc);
          }
          if (ci->u.target_class) {
            mrb_gc_mark(mrb, (struct RBasic*)ci->u.target_class);
          }
        }
      }

      /* Mark fiber */
      mrb_gc_mark(mrb, (struct RBasic*)c->fib);

      /* Mark task-specific values */
      mrb_gc_mark_value(mrb, t->self);
      if (t->status == MRB_TASK_STATUS_DORMANT) {
        mrb_gc_mark_value(mrb, t->state.result);
      }
      mrb_gc_mark_value(mrb, t->name);

      t = t->next;
    }
  }
}

/*
 * Queue operations
 */

/* Get queue head pointer based on task status */
static mrb_task**
q_get_queue(mrb_state *mrb, mrb_task *t)
{
  switch (t->status) {
    case MRB_TASK_STATUS_DORMANT:
      return &q_dormant_;
    case MRB_TASK_STATUS_READY:
    case MRB_TASK_STATUS_RUNNING:
      return &q_ready_;
    case MRB_TASK_STATUS_WAITING:
      return &q_waiting_;
    case MRB_TASK_STATUS_SUSPENDED:
      return &q_suspended_;
    default:
      return &q_dormant_;
  }
}

/* Insert task into queue based on priority (higher priority = lower number = earlier in queue) */
static void
q_insert_task(mrb_state *mrb, mrb_task *t)
{
  mrb_task **q = q_get_queue(mrb, t);
  mrb_task *curr = *q;
  mrb_task *prev = NULL;

  /* Find insertion point - insert before first task with lower priority */
  while (curr != NULL && curr->priority <= t->priority) {
    prev = curr;
    curr = curr->next;
  }

  /* Insert task */
  t->next = curr;
  if (prev == NULL) {
    *q = t;  /* Insert at head */
  }
  else {
    prev->next = t;  /* Insert after prev */
  }
}

/* Delete task from its current queue */
static void
q_delete_task(mrb_state *mrb, mrb_task *t)
{
  mrb_task **q = q_get_queue(mrb, t);
  mrb_task *curr = *q;
  mrb_task *prev = NULL;

  /* Find and remove task */
  while (curr != NULL) {
    if (curr == t) {
      if (prev == NULL) {
        *q = curr->next;  /* Remove from head */
      }
      else {
        prev->next = curr->next;  /* Remove from middle/end */
      }
      t->next = NULL;
      return;
    }
    prev = curr;
    curr = curr->next;
  }
}

/*
 * Task lifecycle
 */

/* Allocate new task */
static mrb_task*
task_alloc(mrb_state *mrb)
{
  mrb_task *t = (mrb_task*)mrb_malloc(mrb, sizeof(mrb_task));
  memset(t, 0, sizeof(mrb_task));
  return t;
}

/* Initialize task context (stack and callinfo) - similar to Fiber */
static void
task_init_context(mrb_state *mrb, mrb_task *t, const struct RProc *proc)
{
  static const struct mrb_context mrb_context_zero = { 0 };
  struct mrb_context *c = &t->c;

  *c = mrb_context_zero;

  /* Initialize VM stack */
  size_t slen = TASK_STACK_INIT_SIZE;
  if (proc->body.irep->nregs > slen) {
    slen += proc->body.irep->nregs;
  }
  c->stbase = (mrb_value*)mrb_malloc(mrb, slen * sizeof(mrb_value));
  c->stend = c->stbase + slen;

  /* Initialize stack values to nil */
  {
    mrb_value *s = c->stbase + 1;
    mrb_value *send = c->stend;
    while (s < send) {
      SET_NIL_VALUE(*s);
      s++;
    }
  }

  /* Copy receiver from current context */
  c->stbase[0] = mrb->c->ci->stack[0];

  /* Initialize callinfo stack */
  static const mrb_callinfo ci_zero = { 0 };
  c->cibase = (mrb_callinfo*)mrb_malloc(mrb, TASK_CI_INIT_SIZE * sizeof(mrb_callinfo));
  c->ciend = c->cibase + TASK_CI_INIT_SIZE;
  c->ci = c->cibase;
  c->cibase[0] = ci_zero;

  /* Setup callinfo */
  mrb_callinfo *ci = c->ci;
  mrb_vm_ci_target_class_set(ci, MRB_PROC_TARGET_CLASS(proc));
  mrb_vm_ci_proc_set(ci, proc);
  ci->stack = c->stbase;
  ci->pc = proc->body.irep->iseq;  /* Initialize PC to start of bytecode */
  ci[1] = ci[0];
  c->ci++;  /* Push dummy callinfo */

  c->status = MRB_TASK_CREATED;
}

/*
 * Scheduler core
 */

/* Wake up tasks waiting on join for a completed task */
static void
wake_up_join_waiters(mrb_state *mrb, mrb_task *completed_task)
{
  mrb_task *curr = q_waiting_;
  while (curr != NULL) {
    mrb_task *next = curr->next;
    if (curr->reason == MRB_TASK_REASON_JOIN && curr->wait.join == completed_task) {
      mrb_task_disable_irq();
      q_delete_task(mrb, curr);
      curr->status = MRB_TASK_STATUS_READY;
      curr->reason = MRB_TASK_REASON_NONE;
      curr->wait.join = NULL;
      q_insert_task(mrb, curr);
      mrb_task_enable_irq();
    }
    curr = next;
  }
}

/* Change task state with IRQ protection and queue management */
static void
task_change_state(mrb_state *mrb, mrb_task *t, uint8_t new_status)
{
  mrb_task_disable_irq();
  q_delete_task(mrb, t);
  t->status = new_status;
  q_insert_task(mrb, t);
  mrb_task_enable_irq();
}

/* Execute a single task - core task execution logic */
static void
execute_task(mrb_state *mrb, mrb_task *t)
{
  struct mrb_context *prev_c;
  mrb_callinfo *prev_ci;
  uint8_t prev_cci;

  /* Set task as running */
  t->status = MRB_TASK_STATUS_RUNNING;
  t->state.timeslice = MRB_TIMESLICE_TICK_COUNT;

  /* Switch to task context */
  prev_c = mrb->c;
  prev_ci = prev_c->ci;
  prev_cci = prev_c->ci->cci;
  t->c.prev = mrb->c;
  mrb->c = &t->c;

  /* If task hasn't started yet, pop dummy callinfo */
  if (t->c.status == MRB_FIBER_CREATED) {
    t->c.ci--;
  }

  /* Clear switching flag */
  switching_ = FALSE;

  /* Set status to RUNNING so VM can transition it properly */
  t->c.status = MRB_FIBER_RUNNING;

  /* Save proc and PC to locals before calling mrb_vm_exec */
  const struct RProc *proc = t->c.ci->proc;
  const mrb_code *pc = t->c.ci->pc;

  /* With C function boundary checks, proc should never be NULL on resume */
  if (!proc) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "task context corrupted: no proc on resume");
  }

  /* Set vmexec flag to prevent fiber_terminate from being called */
  t->c.vmexec = TRUE;

  /* Execute task - PC is saved in ci->pc from previous run */
  t->state.result = mrb_vm_exec(mrb, proc, pc);

  /* Clear vmexec flag */
  t->c.vmexec = FALSE;

  /* Clear switching flag */
  switching_ = FALSE;

  /* Restore context */
  mrb->c = prev_c;
  t->c.prev = NULL;
  prev_c->ci = prev_ci;
  prev_ci->cci = prev_cci;

  /* Handle task termination */
  if (t->c.status == MRB_FIBER_TERMINATED) {
    switching_ = FALSE;
    mrb_task_disable_irq();
    q_delete_task(mrb, t);
    t->status = MRB_TASK_STATUS_DORMANT;
    q_insert_task(mrb, t);
    mrb_task_enable_irq();

    /* Wake up tasks waiting on join */
    wake_up_join_waiters(mrb, t);
  }
  else if (t->status == MRB_TASK_STATUS_RUNNING) {
    /* Task yielded but still running - move to ready queue */
    t->status = MRB_TASK_STATUS_READY;
  }
}

/* Tick handler - called by timer interrupt */
void
mrb_tick(mrb_state *mrb)
{
  mrb_task *t;

  /* Increment global tick counter */
  tick_++;

  /* Decrease timeslice for running task */
  t = q_ready_;
  if (t && t->status == MRB_TASK_STATUS_RUNNING && t->state.timeslice > 0) {
    t->state.timeslice--;
    if (t->state.timeslice == 0) {
      switching_ = TRUE;  /* Trigger context switch */
    }
  }

  /* Wake up sleeping tasks whose wakeup time has passed */
  if ((int32_t)(wakeup_tick_ - tick_) <= 0) {
    mrb_task *curr = q_waiting_;
    mrb_task *next;
    uint32_t next_wakeup = UINT32_MAX;

    while (curr != NULL) {
      next = curr->next;

      if (curr->reason == MRB_TASK_REASON_SLEEP) {
        if ((int32_t)(curr->wait.wakeup_tick - tick_) <= 0) {
          /* Time to wake up */
          q_delete_task(mrb, curr);
          curr->status = MRB_TASK_STATUS_READY;
          curr->reason = MRB_TASK_REASON_NONE;
          q_insert_task(mrb, curr);
          switching_ = TRUE;
        }
        else if (curr->wait.wakeup_tick < next_wakeup) {
          next_wakeup = curr->wait.wakeup_tick;
        }
      }

      curr = next;
    }

    wakeup_tick_ = next_wakeup;
  }
}

/* Main scheduler loop */
mrb_value
mrb_task_run(mrb_state *mrb)
{
  mrb_task *t;

  while (1) {
    t = q_ready_;

    /* No task ready - check if all tasks are done */
    if (!t) {
      /* If there are tasks waiting or suspended, idle */
      if (q_waiting_ || q_suspended_) {
        mrb_hal_task_idle_cpu(mrb);
        continue;
      }
      /* All tasks are dormant - scheduler done */
      break;
    }

    /* Safety check - don't execute terminated tasks */
    if (t->status == MRB_TASK_STATUS_DORMANT || t->c.status == MRB_FIBER_TERMINATED) {
      /* Task is terminated but still in queue - remove it */
      mrb_task_disable_irq();
      q_delete_task(mrb, t);
      if (t->status != MRB_TASK_STATUS_DORMANT) {
        t->status = MRB_TASK_STATUS_DORMANT;
        q_insert_task(mrb, t);
      }
      mrb_task_enable_irq();
      continue;
    }

    /* Execute task using core logic */
    execute_task(mrb, t);

    /* Move to end of ready queue if still running (round-robin) */
    if (t->status == MRB_TASK_STATUS_READY) {
      task_change_state(mrb, t, MRB_TASK_STATUS_READY);
    }

    /* Run incremental GC if active */
    if (mrb->gc.state != MRB_GC_STATE_ROOT) {
      mrb_incremental_gc(mrb);
    }
  }

  return mrb_nil_value();
}

/*
 * Sleep operations
 */

static void
sleep_us_impl(mrb_state *mrb, uint32_t usec)
{
  mrb_task *t;

  /* Check if we're in a task context */
  if (mrb->c == mrb->root_c) {
    /* Not in task context - sleep in real wall-clock time using HAL */
    mrb_hal_task_sleep_us(mrb, usec);
    /* Clear switching flag - we're in root context, not switching to a task */
    switching_ = FALSE;
    return;
  }

  /* Check for C function boundary - cannot do cooperative context switch */
  mrb_callinfo *ci;
  for (ci = mrb->c->ci; ci >= mrb->c->cibase; ci--) {
    if (ci->cci > 0) {
      /* Inside C function - fall back to blocking sleep without context switch */
      mrb_hal_task_sleep_us(mrb, usec);
      switching_ = FALSE;
      return;
    }
  }

  /* In task context - get current running task */
  t = MRB2TASK(mrb);

  mrb_task_disable_irq();

  /* Remove from ready queue */
  q_delete_task(mrb, t);

  /* Move to waiting queue */
  t->status = MRB_TASK_STATUS_WAITING;
  t->reason = MRB_TASK_REASON_SLEEP;
  /* Convert microseconds to ticks (tick unit is in milliseconds) */
  t->wait.wakeup_tick = tick_ + USEC_TO_TICKS(usec);

  /* Update next wakeup time if this task wakes earlier */
  if ((int32_t)(t->wait.wakeup_tick - wakeup_tick_) < 0) {
    wakeup_tick_ = t->wait.wakeup_tick;
  }

  q_insert_task(mrb, t);

  mrb_task_enable_irq();

  /* Trigger context switch */
  switching_ = TRUE;
}

static void
sleep_ms_impl(mrb_state *mrb, uint32_t ms)
{
  sleep_us_impl(mrb, ms * 1000);
}

static mrb_value
mrb_f_sleep(mrb_state *mrb, mrb_value self)
{
  mrb_float sec = 0;
  mrb_int n = mrb_get_args(mrb, "|f", &sec);

  if (n == 0) {
    /* No argument - suspend indefinitely */
    mrb_task *t = q_ready_;
    if (t) {
      mrb_task_disable_irq();
      q_delete_task(mrb, t);
      t->status = MRB_TASK_STATUS_SUSPENDED;
      q_insert_task(mrb, t);
      mrb_task_enable_irq();
      switching_ = TRUE;
    }
    return mrb_nil_value();
  }

  if (sec < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "time interval must be positive");
  }

  mrb_int ms = (mrb_int)(sec * 1000);
  sleep_ms_impl(mrb, (uint32_t)ms);

  return mrb_fixnum_value((mrb_int)sec);
}

static mrb_value
mrb_f_sleep_ms(mrb_state *mrb, mrb_value self)
{
  mrb_int ms;

  mrb_get_args(mrb, "i", &ms);

  if (ms < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "time interval must be positive");
  }

  sleep_ms_impl(mrb, (uint32_t)ms);

  return mrb_nil_value();
}

static mrb_value
mrb_f_usleep(mrb_state *mrb, mrb_value self)
{
  mrb_int usec;

  mrb_get_args(mrb, "i", &usec);

  if (usec < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "time interval must be positive");
  }

  sleep_us_impl(mrb, (uint32_t)usec);

  return mrb_fixnum_value(usec);
}

/*
 * Task class methods
 */

static mrb_value
mrb_task_s_new(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;
  mrb_value name_val = mrb_nil_value();
  mrb_int priority = 128;  /* Default middle priority */
  mrb_value kw_values[2] = {mrb_undef_value(), mrb_undef_value()};
  mrb_sym kw_names[2] = {mrb_intern_lit(mrb, "name"), mrb_intern_lit(mrb, "priority")};
  const mrb_kwargs kwargs = {
    2, 0, kw_names, kw_values, NULL
  };

  /* Get block and optional keyword arguments */
  mrb_get_args(mrb, "&:", &blk, &kwargs);

  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "tried to create task without a block");
  }

  const struct RProc *proc = mrb_proc_ptr(blk);

  /* Parse keyword arguments */
  if (!mrb_undef_p(kw_values[0])) {
    /* Validate name type - must be String */
    if (!mrb_string_p(kw_values[0])) {
      mrb_raise(mrb, E_TYPE_ERROR, "name must be a String");
    }
    name_val = kw_values[0];
  }
  if (!mrb_undef_p(kw_values[1])) {
    /* Validate priority type - must be Integer */
    if (!mrb_integer_p(kw_values[1])) {
      mrb_raise(mrb, E_TYPE_ERROR, "priority must be an Integer");
    }
    priority = mrb_integer(kw_values[1]);
    if (priority < 0 || priority > 255) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "priority must be 0-255");
    }
  }

  /* Allocate and initialize task */
  mrb_task *t = task_alloc(mrb);
  t->priority = (uint8_t)priority;
  t->status = MRB_TASK_STATUS_READY;
  t->reason = MRB_TASK_REASON_NONE;
  t->name = name_val;
  /* Note: proc is stored in t->c.ci->proc and marked via callinfo GC */

  /* Create Ruby object to hold task */
  mrb_value task_obj = mrb_obj_value(mrb_data_object_alloc(mrb, mrb_class_get(mrb, "Task"),
                                                           t, &mrb_task_type));
  t->self = task_obj;

  /* Register with GC to protect task object from collection */
  mrb_gc_register(mrb, task_obj);

  /* Initialize task context */
  task_init_context(mrb, t, proc);

  /* Insert into ready queue */
  mrb_task_disable_irq();
  q_insert_task(mrb, t);
  mrb_task_enable_irq();

  /* Trigger context switch if this task has higher priority than current */
  if (q_ready_ && q_ready_->status == MRB_TASK_STATUS_RUNNING) {
    if (t->priority < q_ready_->priority) {
      switching_ = TRUE;
    }
  }

  return task_obj;
}

static mrb_value
mrb_task_s_current(mrb_state *mrb, mrb_value self)
{
  /* Check if we're in root context */
  if (mrb->c == mrb->root_c) {
    /* Return main task wrapper (lazy-allocate if needed) */
    if (!mrb->task.main_task) {
      struct RClass *task_class = mrb_class_ptr(self);
      struct RData *data = mrb_data_object_alloc(mrb, task_class, NULL, &mrb_task_type);
      mrb_task *t = (mrb_task*)mrb_calloc(mrb, 1, sizeof(mrb_task));

      /* Initialize as main task - special status that's never scheduled */
      t->priority = 0;
      t->status = MRB_TASK_STATUS_RUNNING;  /* Always running */
      t->name = mrb_str_new_cstr(mrb, "main");
      t->self = mrb_obj_value(data);
      data->data = t;
      data->type = &mrb_task_type;

      /* Register for GC protection */
      mrb_gc_register(mrb, t->self);

      /* Note: t->c is not used - root context is in mrb->root_c */
      mrb->task.main_task = t;
    }
    return mrb->task.main_task->self;
  }

  /* Use pointer arithmetic to get task from context - O(1) */
  mrb_task *t = MRB2TASK(mrb);
  return t->self;
}

static mrb_value
mrb_task_s_list(mrb_state *mrb, mrb_value self)
{
  mrb_value ary = mrb_ary_new(mrb);

  /* Iterate all queues and collect tasks */
  for (int i = 0; i < MRB_NUM_TASK_QUEUE; i++) {
    mrb_task *t = mrb->task.queues[i];
    while (t != NULL) {
      mrb_ary_push(mrb, ary, t->self);
      t = t->next;
    }
  }

  return ary;
}

/*
 * Run one task iteration - helper for Task.pass from root context
 * Waits for ready tasks if needed (cooperative yielding)
 */
static void
task_run_one_iteration(mrb_state *mrb)
{
  mrb_task *t = q_ready_;

  /* No ready task - just return (sleep from root provides delays) */
  if (!t) {
    return;
  }

  /* Skip terminated tasks */
  if (t->status == MRB_TASK_STATUS_DORMANT || t->c.status == MRB_FIBER_TERMINATED) {
    mrb_task_disable_irq();
    q_delete_task(mrb, t);
    if (t->status != MRB_TASK_STATUS_DORMANT) {
      t->status = MRB_TASK_STATUS_DORMANT;
      q_insert_task(mrb, t);
    }
    mrb_task_enable_irq();
    return;
  }

  /* Execute ready task */
  execute_task(mrb, t);
}

static mrb_value
mrb_task_s_pass(mrb_state *mrb, mrb_value self)
{
  if (mrb->c == mrb->root_c) {
    /* Called from root context - run one task iteration */
    task_run_one_iteration(mrb);
  }
  else {
    /* Check for C function boundary - cannot yield from C function */
    mrb_callinfo *ci;
    for (ci = mrb->c->ci; ci >= mrb->c->cibase; ci--) {
      if (ci->cci > 0) {
        mrb_raise(mrb, E_RUNTIME_ERROR, "can't pass across C function boundary");
      }
    }

    /* In task context - trigger context switch */
    switching_ = TRUE;
  }

  return mrb_nil_value();
}

/* Helper to build statistics for a task queue */
static mrb_value
mrb_stat_sub(mrb_state *mrb, mrb_task *queue)
{
  mrb_value stat = mrb_hash_new(mrb);
  mrb_value tasks = mrb_ary_new(mrb);
  mrb_task *curr = queue;
  int count = 0;

  /* Walk the queue and collect task objects */
  while (curr) {
    count++;
    mrb_ary_push(mrb, tasks, curr->self);
    curr = curr->next;
  }

  /* Build statistics hash */
  mrb_hash_set(mrb, stat, mrb_symbol_value(MRB_SYM(count)), mrb_fixnum_value(count));
  mrb_hash_set(mrb, stat, mrb_symbol_value(MRB_SYM(tasks)), tasks);

  return stat;
}

static mrb_value
mrb_task_s_stat(mrb_state *mrb, mrb_value self)
{
  mrb_value data = mrb_hash_new(mrb);

  mrb_task_disable_irq();

  /* Add global scheduler state */
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(tick)), mrb_fixnum_value(tick_));
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(wakeup_tick)), mrb_fixnum_value(wakeup_tick_));

  /* Add statistics for each queue */
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(dormant)), mrb_stat_sub(mrb, q_dormant_));
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(ready)), mrb_stat_sub(mrb, q_ready_));
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(waiting)), mrb_stat_sub(mrb, q_waiting_));
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(suspended)), mrb_stat_sub(mrb, q_suspended_));

  mrb_task_enable_irq();

  return data;
}

static mrb_value
mrb_task_s_run(mrb_state *mrb, mrb_value self)
{
  return mrb_task_run(mrb);
}

static mrb_value
mrb_task_s_get(mrb_state *mrb, mrb_value self)
{
  mrb_value name;

  mrb_get_args(mrb, "o", &name);

  /* Search all queues for task with matching name */
  for (int i = 0; i < MRB_NUM_TASK_QUEUE; i++) {
    mrb_task *t = mrb->task.queues[i];
    while (t != NULL) {
      if (mrb_equal(mrb, t->name, name)) {
        return t->self;
      }
      t = t->next;
    }
  }

  return mrb_nil_value();
}

/*
 * Task instance methods
 */

static mrb_value
mrb_task_status(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);

  /* Return status as symbol matching original implementation */
  return mrb_symbol_value(
    (t->status == MRB_TASK_STATUS_RUNNING)   ? MRB_SYM(RUNNING)   :
    (t->status == MRB_TASK_STATUS_READY)     ? MRB_SYM(READY)     :
    (t->status == MRB_TASK_STATUS_WAITING)   ? MRB_SYM(WAITING)   :
    (t->status == MRB_TASK_STATUS_SUSPENDED) ? MRB_SYM(SUSPENDED) :
    (t->status == MRB_TASK_STATUS_DORMANT)   ? MRB_SYM(DORMANT)   :
                                               MRB_SYM(UNKNOWN));
}

static mrb_value
mrb_task_inspect(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;
  char buf[256];
  const char *name_str;
  const char *status_str;

  TASK_GET_PTR_OR_RAISE(t, self);

  /* Get status string directly from task status field */
  switch (t->status) {
    case MRB_TASK_STATUS_RUNNING:
      status_str = "RUNNING";
      break;
    case MRB_TASK_STATUS_READY:
      status_str = "READY";
      break;
    case MRB_TASK_STATUS_WAITING:
      status_str = "WAITING";
      break;
    case MRB_TASK_STATUS_SUSPENDED:
      status_str = "SUSPENDED";
      break;
    case MRB_TASK_STATUS_DORMANT:
      status_str = "DORMANT";
      break;
    default:
      status_str = "UNKNOWN";
      break;
  }

  /* Get name as C string - avoid mrb_funcall to prevent VM state issues */
  if (mrb_string_p(t->name)) {
    name_str = RSTRING_PTR(t->name);
  }
  else if (mrb_symbol_p(t->name)) {
    name_str = mrb_sym_name(mrb, mrb_symbol(t->name));
  }
  else {
    /* Treat nil, undef, or any other type as unnamed */
    name_str = "(unnamed)";
  }

  /* Format: #<Task:0x12345678 name:STATUS> */
  snprintf(buf, sizeof(buf), "#<Task:%p %s:%s>",
           (void *)t,
           name_str,
           status_str);

  return mrb_str_new_cstr(mrb, buf);
}

static mrb_value
mrb_task_name(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);

  /* Return "(noname)" if name is not set */
  if (mrb_nil_p(t->name)) {
    return mrb_str_new_lit(mrb, "(noname)");
  }

  return t->name;
}

static mrb_value
mrb_task_set_name(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;
  mrb_value name;

  TASK_GET_PTR_OR_RAISE(t, self);

  mrb_get_args(mrb, "o", &name);
  t->name = name;

  return name;
}

static mrb_value
mrb_task_priority(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);

  return mrb_fixnum_value(t->priority);
}

static mrb_value
mrb_task_set_priority(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;
  mrb_int priority;

  TASK_GET_PTR_OR_RAISE(t, self);

  mrb_get_args(mrb, "i", &priority);

  if (priority < 0 || priority > 255) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "priority must be 0-255");
  }

  mrb_task_disable_irq();
  t->priority = (uint8_t)priority;

  /* Re-sort in queue if task is ready */
  if (t->status == MRB_TASK_STATUS_READY || t->status == MRB_TASK_STATUS_RUNNING) {
    q_delete_task(mrb, t);
    q_insert_task(mrb, t);
  }
  mrb_task_enable_irq();

  return mrb_fixnum_value(priority);
}

static mrb_value
mrb_task_suspend(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);

  /* Can only suspend ready or running tasks */
  if (t->status != MRB_TASK_STATUS_READY && t->status != MRB_TASK_STATUS_RUNNING) {
    return self;
  }

  task_change_state(mrb, t, MRB_TASK_STATUS_SUSPENDED);

  /* If suspending self, trigger context switch */
  if (t == q_ready_ || t->status == MRB_TASK_STATUS_RUNNING) {
    switching_ = TRUE;
  }

  return self;
}

static mrb_value
mrb_task_resume(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);

  /* Can only resume suspended tasks */
  if (t->status != MRB_TASK_STATUS_SUSPENDED) {
    return self;
  }

  task_change_state(mrb, t, MRB_TASK_STATUS_READY);

  /* Trigger context switch if resumed task has higher priority */
  if (q_ready_ && q_ready_->status == MRB_TASK_STATUS_RUNNING) {
    if (t->priority < q_ready_->priority) {
      switching_ = TRUE;
    }
  }

  return self;
}

static mrb_value
mrb_task_terminate(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);

  /* Don't terminate already dormant tasks */
  if (t->status == MRB_TASK_STATUS_DORMANT) {
    return self;
  }

  mrb_task_disable_irq();

  /* Move to dormant queue */
  q_delete_task(mrb, t);
  t->status = MRB_TASK_STATUS_DORMANT;
  t->c.status = MRB_TASK_STOPPED;
  q_insert_task(mrb, t);

  mrb_task_enable_irq();

  /* Wake up tasks waiting on join */
  wake_up_join_waiters(mrb, t);

  /* If terminating self, trigger context switch */
  if (t == q_ready_) {
    switching_ = TRUE;
  }

  return self;
}

static mrb_value
mrb_task_join(mrb_state *mrb, mrb_value self)
{
  mrb_task *t, *current;

  TASK_GET_PTR_OR_RAISE(t, self);

  /* Get current task using pointer arithmetic */
  if (mrb->c == mrb->root_c) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "join can only be called from running task");
  }
  current = MRB2TASK(mrb);

  /* Can't join self */
  if (t == current) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "can't join self");
  }

  /* If task is already dormant, return immediately */
  if (t->status == MRB_TASK_STATUS_DORMANT) {
    return t->state.result;
  }

  /* Wait for task to complete */
  mrb_task_disable_irq();
  q_delete_task(mrb, current);
  current->status = MRB_TASK_STATUS_WAITING;
  current->reason = MRB_TASK_REASON_JOIN;
  current->wait.join = t;
  q_insert_task(mrb, current);
  mrb_task_enable_irq();

  /* Trigger context switch */
  switching_ = TRUE;

  return t->state.result;
}

/*
 * Initialization
 */

void
mrb_mruby_task_gem_init(mrb_state *mrb)
{
  struct RClass *task_class;

  /* Initialize HAL (timer and interrupts) */
  mrb_hal_task_init(mrb);

  /* Initialize main task to NULL */
  mrb->task.main_task = NULL;

  task_class = mrb_define_class(mrb, "Task", mrb->object_class);
  MRB_SET_INSTANCE_TT(task_class, MRB_TT_DATA);

  /* Class methods */
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(new),     mrb_task_s_new,     MRB_ARGS_BLOCK());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(current), mrb_task_s_current, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(list),    mrb_task_s_list,    MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(pass),    mrb_task_s_pass,    MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(stat),    mrb_task_s_stat,    MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(get),     mrb_task_s_get,     MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(run),     mrb_task_s_run,     MRB_ARGS_NONE());

  /* Instance methods */
  mrb_define_method_id(mrb, task_class, MRB_SYM(status),      mrb_task_status,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, task_class, MRB_SYM(inspect),     mrb_task_inspect,      MRB_ARGS_NONE());
  mrb_define_method_id(mrb, task_class, MRB_SYM(name),        mrb_task_name,         MRB_ARGS_NONE());
  mrb_define_method_id(mrb, task_class, MRB_SYM_E(name),      mrb_task_set_name,     MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, task_class, MRB_SYM(priority),    mrb_task_priority,     MRB_ARGS_NONE());
  mrb_define_method_id(mrb, task_class, MRB_SYM_E(priority),  mrb_task_set_priority, MRB_ARGS_REQ(1));
  mrb_define_method_id(mrb, task_class, MRB_SYM(suspend),     mrb_task_suspend,      MRB_ARGS_NONE());
  mrb_define_method_id(mrb, task_class, MRB_SYM(resume),      mrb_task_resume,       MRB_ARGS_NONE());
  mrb_define_method_id(mrb, task_class, MRB_SYM(terminate),   mrb_task_terminate,    MRB_ARGS_NONE());
  mrb_define_method_id(mrb, task_class, MRB_SYM(join),        mrb_task_join,         MRB_ARGS_NONE());

  /* Kernel methods (module functions like CRuby)
   * Note: sleep and usleep override mruby-sleep's implementation to be task-aware
   * (cooperative sleep within tasks, blocking sleep otherwise)
   */
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(sleep),    mrb_f_sleep,    MRB_ARGS_OPT(1));
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(usleep),   mrb_f_usleep,   MRB_ARGS_REQ(1));
  mrb_define_module_function_id(mrb, mrb->kernel_module, MRB_SYM(sleep_ms), mrb_f_sleep_ms, MRB_ARGS_REQ(1));
}

void
mrb_mruby_task_gem_final(mrb_state *mrb)
{
  /* Clear main task pointer - GC will handle freeing the object */
  if (mrb->task.main_task) {
    mrb_gc_unregister(mrb, mrb->task.main_task->self);
    mrb->task.main_task = NULL;
  }

  mrb_hal_task_final(mrb);
}
