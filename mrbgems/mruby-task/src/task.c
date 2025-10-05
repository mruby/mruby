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
#include <mruby/presym.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#ifdef _WIN32
#include <windows.h>
#else
#include <time.h>
#include <unistd.h>
#endif
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
  mrb_task *t = (mrb_task*)ptr;
  if (t) {
    /* Free context resources */
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
  mrb_callinfo *prev_ci;
  uint8_t prev_cci;

  while (1) {
    t = q_ready_;

    /* No task ready - check if all tasks are done */
    if (!t) {
      /* If there are tasks waiting or suspended, idle */
      if (q_waiting_ || q_suspended_) {
        mrb_task_hal_idle_cpu(mrb);
        continue;
      }
      /* All tasks are dormant - scheduler done */
      break;
    }

    /* Set task as running */
    t->status = MRB_TASKSTATUS_RUNNING;
    t->timeslice = MRB_TIMESLICE_TICK_COUNT;

    /* Switch to task context */
    prev_c = mrb->c;
    prev_ci = prev_c->ci;  /* Save ci pointer */
    prev_cci = prev_c->ci->cci;  /* Save cci before context switch */
    t->c.prev = mrb->c;  /* Link task context to current context */
    mrb->c = &t->c;

    /* If task hasn't started yet, pop dummy callinfo */
    if (!t->started) {
      t->c.ci--;  /* pop dummy callinfo */
      t->started = 1;  /* Mark as started */
    }

    /* Clear switching flag */
    switching_ = FALSE;

    /* Set status to RUNNING so VM can transition it properly */
    t->c.status = MRB_FIBER_RUNNING;

    /* Execute task - PC is saved in ci->pc from previous run */
    t->result = mrb_vm_exec(mrb, t->c.ci->proc, t->c.ci->pc);

    /* Restore context */
    mrb->c = prev_c;
    t->c.prev = NULL;  /* Clear prev pointer to avoid dangling reference */
    prev_c->ci = prev_ci;  /* Restore ci pointer */
    prev_ci->cci = prev_cci;  /* Restore cci as it may have changed */

    /* Check if task finished (context status is TERMINATED) */
    if (t->c.status == MRB_FIBER_TERMINATED) {
      /* Task completed naturally - mark as dormant */
      switching_ = FALSE;  /* Clear switching flag */
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

  if (!t) {
    /* Not in task context - use blocking sleep with retry on interruption */
#ifdef __unix__
    struct timespec req, rem;
    req.tv_sec = ms / 1000;
    req.tv_nsec = (ms % 1000) * 1000000;
    /* nanosleep automatically retries on EINTR with SA_RESTART */
    while (nanosleep(&req, &rem) == -1) {
      req = rem;  /* Continue with remaining time if interrupted */
    }
#elif defined(_WIN32)
    Sleep((DWORD)ms);
#endif
    return;
  }

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

static mrb_value
mrb_f_usleep(mrb_state *mrb, mrb_value self)
{
  mrb_int usec;

  mrb_get_args(mrb, "i", &usec);

  if (usec < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "time interval must be positive");
  }

  /* Convert microseconds to milliseconds */
  mrb_int ms = usec / 1000;
  sleep_ms_impl(mrb, ms);

  return mrb_fixnum_value(usec);
}

/*
 * HAL POSIX implementation
 */

#ifdef __unix__
#include <signal.h>
#include <sys/time.h>
#include <unistd.h>

static mrb_state *global_mrb = NULL;
static sigset_t alarm_mask;

static void
sigalrm_handler(int sig)
{
  (void)sig;
  if (global_mrb) {
    mrb_tick(global_mrb);
  }
}

void
mrb_task_hal_init(mrb_state *mrb)
{
  struct sigaction sa;
  struct itimerval timer;
  int i;

  /* Initialize task state */
  for (i = 0; i < 4; i++) {
    mrb->task.queues[i] = NULL;
  }
  mrb->task.tick = 0;
  mrb->task.wakeup_tick = UINT32_MAX;
  mrb->task.switching = FALSE;

  global_mrb = mrb;

  /* Set up SIGALRM mask for interrupt control */
  sigemptyset(&alarm_mask);
  sigaddset(&alarm_mask, SIGALRM);

  /* Set up signal handler */
  sa.sa_handler = sigalrm_handler;
  sa.sa_flags = SA_RESTART;  /* Restart interrupted syscalls */
  sigemptyset(&sa.sa_mask);
  sigaction(SIGALRM, &sa, NULL);

  /* Set up periodic timer (MRB_TICK_UNIT ms) */
  timer.it_value.tv_sec = 0;
  timer.it_value.tv_usec = MRB_TICK_UNIT * 1000;
  timer.it_interval.tv_sec = 0;
  timer.it_interval.tv_usec = MRB_TICK_UNIT * 1000;
  setitimer(ITIMER_REAL, &timer, NULL);
}

void
mrb_task_enable_irq(void)
{
  sigprocmask(SIG_UNBLOCK, &alarm_mask, NULL);
}

void
mrb_task_disable_irq(void)
{
  sigprocmask(SIG_BLOCK, &alarm_mask, NULL);
}

void
mrb_task_hal_idle_cpu(mrb_state *mrb)
{
  (void)mrb;
  /* On POSIX, just pause briefly */
  usleep(MRB_TICK_UNIT * 1000);
}

#else
/* Stub implementation for non-POSIX platforms */

void
mrb_task_hal_init(mrb_state *mrb)
{
  int i;

  /* Initialize task state */
  for (i = 0; i < 4; i++) {
    mrb->task.queues[i] = NULL;
  }
  mrb->task.tick = 0;
  mrb->task.wakeup_tick = UINT32_MAX;
  mrb->task.switching = FALSE;

  /* TODO: Platform-specific timer initialization */
}

void
mrb_task_enable_irq(void)
{
  /* TODO: Platform-specific interrupt enable */
}

void
mrb_task_disable_irq(void)
{
  /* TODO: Platform-specific interrupt disable */
}

void
mrb_task_hal_idle_cpu(mrb_state *mrb)
{
  (void)mrb;
  /* TODO: Platform-specific idle/sleep */
}
#endif

/*
 * Task class methods
 */

static mrb_value
mrb_task_s_new(mrb_state *mrb, mrb_value self)
{
  mrb_value blk;
  mrb_value name_val = mrb_nil_value();
  mrb_int priority = 128;  /* Default middle priority */
  const struct RProc *proc;
  mrb_task *t;
  mrb_value task_obj;
  mrb_value kw_values[2];
  const mrb_kwargs kwargs = {
    2, 0, (mrb_sym[]){mrb_intern_lit(mrb, "name"), mrb_intern_lit(mrb, "priority")}, kw_values, NULL
  };

  /* Get block and optional keyword arguments */
  mrb_get_args(mrb, "&:", &blk, &kwargs);

  if (mrb_nil_p(blk)) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "tried to create task without a block");
  }

  proc = mrb_proc_ptr(blk);

  /* Parse keyword arguments */
  if (!mrb_nil_p(kw_values[0])) {
    name_val = kw_values[0];
  }
  if (!mrb_nil_p(kw_values[1])) {
    priority = mrb_integer(kw_values[1]);
    if (priority < 0 || priority > 255) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "priority must be 0-255");
    }
  }

  /* Allocate and initialize task */
  t = task_alloc(mrb);
  t->priority = (uint8_t)priority;
  t->priority_preemption = (uint8_t)priority;
  t->status = MRB_TASKSTATUS_READY;
  t->reason = MRB_TASKREASON_NONE;
  t->name = name_val;

  /* Create Ruby object to hold task */
  task_obj = mrb_obj_value(mrb_data_object_alloc(mrb, mrb_class_get(mrb, "Task"),
                                                  t, &mrb_task_type));
  t->self = task_obj;

  /* Initialize task context */
  task_init_context(mrb, t, proc);

  /* Insert into ready queue */
  mrb_task_disable_irq();
  q_insert_task(mrb, t);
  mrb_task_enable_irq();

  /* Trigger context switch if this task has higher priority than current */
  if (q_ready_ && q_ready_->status == MRB_TASKSTATUS_RUNNING) {
    if (t->priority < q_ready_->priority) {
      switching_ = TRUE;
    }
  }

  return task_obj;
}

static mrb_value
mrb_task_s_current(mrb_state *mrb, mrb_value self)
{
  mrb_task *t = q_ready_;

  if (t && t->status == MRB_TASKSTATUS_RUNNING) {
    return t->self;
  }

  return mrb_nil_value();
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

static mrb_value
mrb_task_s_pass(mrb_state *mrb, mrb_value self)
{
  /* Yield to other tasks by triggering a context switch */
  mrb_task *t = q_ready_;

  if (t && t->status == MRB_TASKSTATUS_RUNNING) {
    switching_ = TRUE;
  }

  return mrb_nil_value();
}

static mrb_value
mrb_task_s_stat(mrb_state *mrb, mrb_value self)
{
  /* TODO: Implement Task::Stat class with statistics */
  return mrb_nil_value();
}

static mrb_value
mrb_task_s_run(mrb_state *mrb, mrb_value self)
{
  return mrb_tasks_run(mrb);
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
  mrb_sym status_sym;

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  /* Convert status to symbol */
  switch (t->status) {
    case MRB_TASKSTATUS_DORMANT:
      status_sym = mrb_intern_lit(mrb, "DORMANT");
      break;
    case MRB_TASKSTATUS_READY:
      status_sym = mrb_intern_lit(mrb, "READY");
      break;
    case MRB_TASKSTATUS_RUNNING:
      status_sym = mrb_intern_lit(mrb, "RUNNING");
      break;
    case MRB_TASKSTATUS_WAITING:
      status_sym = mrb_intern_lit(mrb, "WAITING");
      break;
    case MRB_TASKSTATUS_SUSPENDED:
      status_sym = mrb_intern_lit(mrb, "SUSPENDED");
      break;
    default:
      status_sym = mrb_intern_lit(mrb, "UNKNOWN");
      break;
  }

  return mrb_symbol_value(status_sym);
}

static mrb_value
mrb_task_name(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  return t->name;
}

static mrb_value
mrb_task_set_name(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;
  mrb_value name;

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  mrb_get_args(mrb, "o", &name);
  t->name = name;

  return name;
}

static mrb_value
mrb_task_priority(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  return mrb_fixnum_value(t->priority);
}

static mrb_value
mrb_task_set_priority(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;
  mrb_int priority;

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  mrb_get_args(mrb, "i", &priority);

  if (priority < 0 || priority > 255) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "priority must be 0-255");
  }

  mrb_task_disable_irq();
  t->priority = (uint8_t)priority;
  t->priority_preemption = (uint8_t)priority;

  /* Re-sort in queue if task is ready */
  if (t->status == MRB_TASKSTATUS_READY || t->status == MRB_TASKSTATUS_RUNNING) {
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

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  /* Can only suspend ready or running tasks */
  if (t->status != MRB_TASKSTATUS_READY && t->status != MRB_TASKSTATUS_RUNNING) {
    return self;
  }

  mrb_task_disable_irq();
  q_delete_task(mrb, t);
  t->status = MRB_TASKSTATUS_SUSPENDED;
  q_insert_task(mrb, t);
  mrb_task_enable_irq();

  /* If suspending self, trigger context switch */
  if (t == q_ready_ || t->status == MRB_TASKSTATUS_RUNNING) {
    switching_ = TRUE;
  }

  return self;
}

static mrb_value
mrb_task_resume(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  /* Can only resume suspended tasks */
  if (t->status != MRB_TASKSTATUS_SUSPENDED) {
    return self;
  }

  mrb_task_disable_irq();
  q_delete_task(mrb, t);
  t->status = MRB_TASKSTATUS_READY;
  q_insert_task(mrb, t);
  mrb_task_enable_irq();

  /* Trigger context switch if resumed task has higher priority */
  if (q_ready_ && q_ready_->status == MRB_TASKSTATUS_RUNNING) {
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

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  /* Don't terminate already dormant tasks */
  if (t->status == MRB_TASKSTATUS_DORMANT) {
    return self;
  }

  mrb_task_disable_irq();

  /* Move to dormant queue */
  q_delete_task(mrb, t);
  t->status = MRB_TASKSTATUS_DORMANT;
  t->c.status = MRB_TASK_STOPPED;
  q_insert_task(mrb, t);

  /* Wake up tasks waiting on join */
  mrb_task *curr = q_waiting_;
  while (curr != NULL) {
    mrb_task *next = curr->next;
    if (curr->reason == MRB_TASKREASON_JOIN && curr->join == t) {
      q_delete_task(mrb, curr);
      curr->status = MRB_TASKSTATUS_READY;
      curr->reason = MRB_TASKREASON_NONE;
      curr->join = NULL;
      q_insert_task(mrb, curr);
    }
    curr = next;
  }

  mrb_task_enable_irq();

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

  t = (mrb_task*)mrb_data_get_ptr(mrb, self, &mrb_task_type);
  if (!t) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "invalid task");
  }

  /* Get current task */
  current = q_ready_;
  if (!current || current->status != MRB_TASKSTATUS_RUNNING) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "join can only be called from running task");
  }

  /* Can't join self */
  if (t == current) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "can't join self");
  }

  /* If task is already dormant, return immediately */
  if (t->status == MRB_TASKSTATUS_DORMANT) {
    return t->result;
  }

  /* Wait for task to complete */
  mrb_task_disable_irq();
  q_delete_task(mrb, current);
  current->status = MRB_TASKSTATUS_WAITING;
  current->reason = MRB_TASKREASON_JOIN;
  current->join = t;
  q_insert_task(mrb, current);
  mrb_task_enable_irq();

  /* Trigger context switch */
  switching_ = TRUE;

  return t->result;
}

/*
 * Initialization
 */

void
mrb_mruby_task_gem_init(mrb_state *mrb)
{
  struct RClass *task_class;

  /* Initialize HAL (timer and interrupts) */
  mrb_task_hal_init(mrb);

  task_class = mrb_define_class(mrb, "Task", mrb->object_class);
  MRB_SET_INSTANCE_TT(task_class, MRB_TT_DATA);

  /* Class methods */
  mrb_define_class_method(mrb, task_class, "new",     mrb_task_s_new,     MRB_ARGS_BLOCK());
  mrb_define_class_method(mrb, task_class, "current", mrb_task_s_current, MRB_ARGS_NONE());
  mrb_define_class_method(mrb, task_class, "list",    mrb_task_s_list,    MRB_ARGS_NONE());
  mrb_define_class_method(mrb, task_class, "pass",    mrb_task_s_pass,    MRB_ARGS_NONE());
  mrb_define_class_method(mrb, task_class, "stat",    mrb_task_s_stat,    MRB_ARGS_NONE());
  mrb_define_class_method(mrb, task_class, "get",     mrb_task_s_get,     MRB_ARGS_REQ(1));
  mrb_define_class_method(mrb, task_class, "run",     mrb_task_s_run,     MRB_ARGS_NONE());

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

  /* Kernel methods
   * Note: sleep and usleep override mruby-sleep's implementation to be task-aware
   * (cooperative sleep within tasks, blocking sleep otherwise)
   */
  mrb_define_private_method_id(mrb, mrb->kernel_module, MRB_SYM(sleep),    mrb_f_sleep,    MRB_ARGS_OPT(1));
  mrb_define_private_method_id(mrb, mrb->kernel_module, MRB_SYM(usleep),   mrb_f_usleep,   MRB_ARGS_REQ(1));
  mrb_define_private_method_id(mrb, mrb->kernel_module, MRB_SYM(sleep_ms), mrb_f_sleep_ms, MRB_ARGS_REQ(1));
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
