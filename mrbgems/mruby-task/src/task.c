/*
** task.c - Task scheduler
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>

#ifdef MRB_USE_TASK_SCHEDULER

#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/error.h>
#include <mruby/gc.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <string.h>
#include <stdint.h>
#include "../include/task.h"

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

/*
 * Task data type for GC
 */
static void
mrb_task_free(mrb_state *mrb, void *ptr)
{
  /* TODO: Free TCB and associated resources */
}

static const struct mrb_data_type mrb_task_type = {
  "Task", mrb_task_free,
};

/*
 * Queue operations
 */

/* Get queue head pointer based on task status */
static mrb_task**
q_get_queue(mrb_state *mrb, mrb_task *t)
{
  switch (t->status) {
    case MRB_TASKSTATUS_DORMANT:
      return &q_dormant_;
    case MRB_TASKSTATUS_READY:
    case MRB_TASKSTATUS_RUNNING:
      return &q_ready_;
    case MRB_TASKSTATUS_WAITING:
      return &q_waiting_;
    case MRB_TASKSTATUS_SUSPENDED:
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
  while (curr != NULL && curr->priority_preemption <= t->priority_preemption) {
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

/* Find task in all queues */
static mrb_task*
q_find_task(mrb_state *mrb, mrb_task *t)
{
  mrb_task *curr;
  int i;

  for (i = 0; i < MRB_NUM_TASK_QUEUE; i++) {
    curr = mrb->task.queues[i];
    while (curr != NULL) {
      if (curr == t) {
        return curr;
      }
      curr = curr->next;
    }
  }
  return NULL;
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

/* Free task and context */
static void
task_free(mrb_state *mrb, mrb_task *t)
{
  if (t->c.stbase) {
    mrb_free(mrb, t->c.stbase);
  }
  if (t->c.cibase) {
    mrb_free(mrb, t->c.cibase);
  }
  mrb_free(mrb, t);
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
  ci[1] = ci[0];
  c->ci++;  /* Push dummy callinfo */

  c->status = MRB_TASK_CREATED;
}

/*
 * Scheduler core
 */

/* Tick handler - called by timer interrupt */
void
mrb_tick(mrb_state *mrb)
{
  mrb_task *t;

  /* Increment global tick counter */
  tick_++;

  /* Decrease timeslice for running task */
  t = q_ready_;
  if (t && t->status == MRB_TASKSTATUS_RUNNING && t->timeslice > 0) {
    t->timeslice--;
    if (t->timeslice == 0) {
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

      if (curr->reason == MRB_TASKREASON_SLEEP) {
        if ((int32_t)(curr->wakeup_tick - tick_) <= 0) {
          /* Time to wake up */
          q_delete_task(mrb, curr);
          curr->status = MRB_TASKSTATUS_READY;
          curr->reason = MRB_TASKREASON_NONE;
          q_insert_task(mrb, curr);
          switching_ = TRUE;
        }
        else if (curr->wakeup_tick < next_wakeup) {
          next_wakeup = curr->wakeup_tick;
        }
      }

      curr = next;
    }

    wakeup_tick_ = next_wakeup;
  }
}

/* Main scheduler loop */
mrb_value
mrb_tasks_run(mrb_state *mrb)
{
  mrb_task *t;
  struct mrb_context *prev_c;

  while (1) {
    t = q_ready_;

    /* No task ready - idle */
    if (!t) {
      mrb_task_hal_idle_cpu(mrb);
      continue;
    }

    /* Set task as running */
    t->status = MRB_TASKSTATUS_RUNNING;
    t->timeslice = MRB_TIMESLICE_TICK_COUNT;

    /* Switch to task context */
    prev_c = mrb->c;
    mrb->c = &t->c;

    /* Clear switching flag */
    switching_ = FALSE;

    /* Execute task */
    t->result = mrb_vm_exec(mrb, t->c.ci->proc, t->c.ci->pc);

    /* Restore context */
    mrb->c = prev_c;

    /* Check if task finished */
    if (t->c.status == MRB_TASK_STOPPED) {
      /* Move to dormant queue */
      mrb_task_disable_irq();
      q_delete_task(mrb, t);
      t->status = MRB_TASKSTATUS_DORMANT;
      q_insert_task(mrb, t);
      mrb_task_enable_irq();

      /* Wake up tasks waiting on join */
      mrb_task *curr = q_waiting_;
      while (curr != NULL) {
        mrb_task *next = curr->next;
        if (curr->reason == MRB_TASKREASON_JOIN && curr->join == t) {
          mrb_task_disable_irq();
          q_delete_task(mrb, curr);
          curr->status = MRB_TASKSTATUS_READY;
          curr->reason = MRB_TASKREASON_NONE;
          curr->join = NULL;
          q_insert_task(mrb, curr);
          mrb_task_enable_irq();
        }
        curr = next;
      }
    }
    /* Context switch requested or task still running */
    else if (t->status == MRB_TASKSTATUS_RUNNING) {
      /* Move to end of ready queue (round-robin) */
      mrb_task_disable_irq();
      q_delete_task(mrb, t);
      t->status = MRB_TASKSTATUS_READY;
      q_insert_task(mrb, t);
      mrb_task_enable_irq();
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
sleep_ms_impl(mrb_state *mrb, mrb_int ms)
{
  mrb_task *t = q_ready_;  /* Current running task */

  if (!t) return;  /* No task to sleep */

  mrb_task_disable_irq();

  /* Remove from ready queue */
  q_delete_task(mrb, t);

  /* Move to waiting queue */
  t->status = MRB_TASKSTATUS_WAITING;
  t->reason = MRB_TASKREASON_SLEEP;
  t->wakeup_tick = tick_ + (ms / MRB_TICK_UNIT);

  /* Update next wakeup time if this task wakes earlier */
  if (t->wakeup_tick < wakeup_tick_) {
    wakeup_tick_ = t->wakeup_tick;
  }

  q_insert_task(mrb, t);

  mrb_task_enable_irq();

  /* Trigger context switch */
  switching_ = TRUE;
}

static mrb_value
mrb_f_sleep(mrb_state *mrb, mrb_value self)
{
  mrb_float sec = 0;
  mrb_int n;

  n = mrb_get_args(mrb, "|f", &sec);

  if (n == 0) {
    /* No argument - suspend indefinitely */
    mrb_task *t = q_ready_;
    if (t) {
      mrb_task_disable_irq();
      q_delete_task(mrb, t);
      t->status = MRB_TASKSTATUS_SUSPENDED;
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
  sleep_ms_impl(mrb, ms);

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

  sleep_ms_impl(mrb, ms);

  return mrb_nil_value();
}

/*
 * HAL stub implementations (temporary)
 * These will be replaced with real implementations in Phase 6
 */

void
mrb_task_hal_init(mrb_state *mrb)
{
  /* TODO: Initialize hardware timer */
}

void
mrb_task_enable_irq(void)
{
  /* TODO: Enable interrupts */
}

void
mrb_task_disable_irq(void)
{
  /* TODO: Disable interrupts */
}

void
mrb_task_hal_idle_cpu(mrb_state *mrb)
{
  /* TODO: Put CPU in idle state */
}

/*
 * Task class methods
 */

static mrb_value
mrb_task_s_new(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task.new */
  return mrb_nil_value();
}

static mrb_value
mrb_task_s_current(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task.current */
  return mrb_nil_value();
}

static mrb_value
mrb_task_s_list(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task.list */
  return mrb_ary_new(mrb);
}

static mrb_value
mrb_task_s_pass(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task.pass */
  return mrb_nil_value();
}

static mrb_value
mrb_task_s_stat(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task.stat */
  return mrb_nil_value();
}

static mrb_value
mrb_task_s_get(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task.get */
  return mrb_nil_value();
}

/*
 * Task instance methods
 */

static mrb_value
mrb_task_status(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#status */
  return mrb_nil_value();
}

static mrb_value
mrb_task_name(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#name */
  return mrb_nil_value();
}

static mrb_value
mrb_task_set_name(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#name= */
  return mrb_nil_value();
}

static mrb_value
mrb_task_priority(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#priority */
  return mrb_nil_value();
}

static mrb_value
mrb_task_set_priority(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#priority= */
  return mrb_nil_value();
}

static mrb_value
mrb_task_suspend(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#suspend */
  return self;
}

static mrb_value
mrb_task_resume(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#resume */
  return self;
}

static mrb_value
mrb_task_terminate(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#terminate */
  return self;
}

static mrb_value
mrb_task_join(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task#join */
  return self;
}

/*
 * Initialization
 */

void
mrb_mruby_task_gem_init(mrb_state *mrb)
{
  struct RClass *task_class;

  task_class = mrb_define_class(mrb, "Task", mrb->object_class);
  MRB_SET_INSTANCE_TT(task_class, MRB_TT_DATA);

  /* Class methods */
  mrb_define_class_method(mrb, task_class, "new",     mrb_task_s_new,     MRB_ARGS_BLOCK());
  mrb_define_class_method(mrb, task_class, "current", mrb_task_s_current, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, task_class, "list",    mrb_task_s_list,    MRB_ARGS_NONE());
  mrb_define_class_method(mrb, task_class, "pass",    mrb_task_s_pass,    MRB_ARGS_NONE());
  mrb_define_class_method(mrb, task_class, "stat",    mrb_task_s_stat,    MRB_ARGS_NONE());
  mrb_define_class_method(mrb, task_class, "get",     mrb_task_s_get,     MRB_ARGS_REQ(1));

  /* Instance methods */
  mrb_define_method(mrb, task_class, "status",    mrb_task_status,       MRB_ARGS_NONE());
  mrb_define_method(mrb, task_class, "name",      mrb_task_name,         MRB_ARGS_NONE());
  mrb_define_method(mrb, task_class, "name=",     mrb_task_set_name,     MRB_ARGS_REQ(1));
  mrb_define_method(mrb, task_class, "priority",  mrb_task_priority,     MRB_ARGS_NONE());
  mrb_define_method(mrb, task_class, "priority=", mrb_task_set_priority, MRB_ARGS_REQ(1));
  mrb_define_method(mrb, task_class, "suspend",   mrb_task_suspend,      MRB_ARGS_NONE());
  mrb_define_method(mrb, task_class, "resume",    mrb_task_resume,       MRB_ARGS_NONE());
  mrb_define_method(mrb, task_class, "terminate", mrb_task_terminate,    MRB_ARGS_NONE());
  mrb_define_method(mrb, task_class, "join",      mrb_task_join,         MRB_ARGS_NONE());

  /* Kernel methods */
  mrb_define_method(mrb, mrb->kernel_module, "sleep",    mrb_f_sleep,    MRB_ARGS_OPT(1));
  mrb_define_method(mrb, mrb->kernel_module, "sleep_ms", mrb_f_sleep_ms, MRB_ARGS_REQ(1));
}

void
mrb_mruby_task_gem_final(mrb_state *mrb)
{
  /* Cleanup if needed */
}

#else

/* Task scheduler not enabled - provide stub */
void
mrb_mruby_task_gem_init(mrb_state *mrb)
{
}

void
mrb_mruby_task_gem_final(mrb_state *mrb)
{
}

#endif /* MRB_USE_TASK_SCHEDULER */
