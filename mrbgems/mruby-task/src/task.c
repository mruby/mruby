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
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <string.h>
#include <stdint.h>
#include <stdlib.h>
#include "task.h"

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

/* Maximum value for scheduler_lock (uint8_t max) */
#define MRB_TASK_SCHEDULER_LOCK_MAX 255

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

  /* GC can run before mruby-task's gem init (allocations during
     earlier gem inits trigger it, deterministically so under GC
     stress). At that point the queues are necessarily empty AND the
     HAL exclusion may not exist yet (the Windows HAL initializes its
     CRITICAL_SECTION in mrb_hal_task_init) — return before touching
     either. Tasks can only be queued after the HAL is initialized,
     so non-empty queues imply the exclusion is safe to take. */
  if (mrb->task.queues[0] == NULL && mrb->task.queues[1] == NULL &&
      mrb->task.queues[2] == NULL && mrb->task.queues[3] == NULL) {
    return;
  }

  /* The tick IRQ relinks tasks between queues (sleep wakeups, timeslice
     rotation). A relink that lands mid-traversal makes this walk skip
     still-queued tasks; a skipped task's object is swept while its
     mrb_task stays linked, and the freed chunk's reuse turns the queue
     links into garbage (observed on RP2350 as ASCII string bytes where
     next pointers should be). Exclude the scheduler IRQ for the whole
     walk — same family as the gc.iterating guard in vm.c. */
  mrb_task_excl_enter(mrb);
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
        /* Clear the dead slots above the live range, matching
           mark_context_stack() in gc.c. A preempted task whose live range
           later shrinks (a frame returned) would otherwise leave stale
           object pointers in those slots; the objects get swept while the
           pointers survive, and a subsequent mark of the resumed task trips
           the MRB_TT_FREE assertion in mrb_gc_mark (issue #6870). */
        size_t stend = c->stend - c->stbase;
        for (; i < stend; i++) {
          SET_NIL_VALUE(c->stbase[i]);
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
      mrb_gc_mark_value(mrb, t->result);
      mrb_gc_mark_value(mrb, t->name);

      t = t->next;
    }
  }
  mrb_task_excl_exit(mrb);
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
void
mrb_task_q_insert(mrb_state *mrb, mrb_task *t)
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
void
mrb_task_q_delete(mrb_state *mrb, mrb_task *t)
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

/* Cleanup terminated task and move to dormant queue if needed */
static inline mrb_bool
task_cleanup_if_stopped(mrb_state *mrb, mrb_task *t)
{
  if (t->status == MRB_TASK_STATUS_DORMANT || t->c.status == MRB_TASK_STOPPED) {
    /* Task is terminated but still in queue - remove it */
    mrb_task_excl_enter(mrb);
    mrb_task_q_delete(mrb, t);
    if (t->status != MRB_TASK_STATUS_DORMANT) {
      t->status = MRB_TASK_STATUS_DORMANT;
      mrb_task_q_insert(mrb, t);
    }
    mrb_task_excl_exit(mrb);
    return TRUE;
  }
  return FALSE;
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

  /* Allocate both buffers atomically: mrb_malloc() raises on OOM via
   * longjmp, which would leave a half-built context (ci == NULL) on a
   * task that is still queued — the scheduler then dereferences it.
   * Allocate with the non-raising variant, and on failure retire the
   * task coherently BEFORE raising NoMemoryError. */
  mrb_value *stbase = (mrb_value*)mrb_malloc_simple(mrb, slen * sizeof(mrb_value));
  mrb_callinfo *cibase = (mrb_callinfo*)mrb_malloc_simple(mrb, TASK_CI_INIT_SIZE * sizeof(mrb_callinfo));
  if (stbase == NULL || cibase == NULL) {
    mrb_free(mrb, stbase);
    mrb_free(mrb, cibase);
    /* Mark only the CONTEXT as stopped. t->status must keep its
     * current value: q_get_queue() derives a task's queue from
     * t->status, so flipping it to DORMANT while the task is still
     * linked in another queue would make every later q_delete search
     * the wrong list (task_cleanup_if_stopped would then spin on an
     * unremovable queue head). The scheduler's existing safety nets
     * see c->status == MRB_TASK_STOPPED, unlink the task from its
     * true queue, and transition it to DORMANT coherently. */
    c->status = MRB_TASK_STOPPED;
    mrb_exc_raise(mrb, mrb_obj_value(mrb->nomem_err));
  }
  c->stbase = stbase;
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

  /* Set receiver to top self */
  c->stbase[0] = mrb_top_self(mrb);

  /* Initialize callinfo stack (allocated above) */
  static const mrb_callinfo ci_zero = { 0 };
  c->cibase = cibase;
  c->ciend = c->cibase + TASK_CI_INIT_SIZE;
  c->ci = c->cibase;
  c->cibase[0] = ci_zero;

  /* Setup callinfo */
  mrb_callinfo *ci = c->ci;
  mrb_vm_ci_target_class_set(ci, MRB_PROC_TARGET_CLASS(proc));
  mrb_vm_ci_proc_set(ci, proc);
  ci->stack = c->stbase;
  ci->pc = proc->body.irep->iseq;  /* Initialize PC to start of bytecode */

  c->status = MRB_TASK_CREATED;
}

/*
 * Scheduler core
 */

/* Wake up tasks waiting on join for a completed task */
static void
wake_up_join_waiters(mrb_state *mrb, mrb_task *completed_task)
{
  mrb_task_excl_enter(mrb);
  mrb_task *curr = q_waiting_;
  while (curr != NULL) {
    mrb_task *next = curr->next;
    if (curr->reason == MRB_TASK_REASON_JOIN && curr->wait.join == completed_task) {
      mrb_task_q_delete(mrb, curr);
      curr->status = MRB_TASK_STATUS_READY;
      curr->reason = MRB_TASK_REASON_NONE;
      curr->wait.join = NULL;
      mrb_task_q_insert(mrb, curr);
      /* If a higher-priority waiter is resumed from task context,
       * request a context switch after leaving the critical section. */
      if (mrb->c != mrb->root_c && !switching_) {
        mrb_task *running = MRB2TASK(mrb);
        if (curr->priority < running->priority) {
          switching_ = TRUE;
        }
      }
    }
    curr = next;
  }
  mrb_task_excl_exit(mrb);
}

/* Change task state with IRQ protection and queue management */
static void
task_change_state(mrb_state *mrb, mrb_task *t, uint8_t new_status)
{
  mrb_task_excl_enter(mrb);
  mrb_task_q_delete(mrb, t);
  t->status = new_status;
  mrb_task_q_insert(mrb, t);
  mrb_task_excl_exit(mrb);
}

typedef struct execute_task_vm_args {
  mrb_task *t;
  const struct RProc *proc;
  const mrb_code *pc;
} execute_task_vm_args;

static mrb_value
execute_task_vm(mrb_state *mrb, void *ud)
{
  execute_task_vm_args *args = (execute_task_vm_args*)ud;

  mrb->task.exception_as_result = TRUE;
  args->t->result = mrb_vm_exec(mrb, args->proc, args->pc);
  if (mrb->exc) {
    args->t->result = mrb_obj_value(mrb->exc);
    mrb->exc = NULL;
  }
  mrb->task.exception_as_result = FALSE;
  return args->t->result;
}

/* Execute a single task - core task execution logic */
static void
execute_task(mrb_state *mrb, mrb_task *t)
{
  struct mrb_context *prev_c;
  mrb_callinfo *prev_ci;
  uint8_t prev_cci;

  /* A task can lose its context without leaving the queues: OOM during
   * task_init_context unwinds via longjmp before the context is built
   * (ci == NULL). Retire such a task instead of dereferencing the hole
   * — checked BEFORE the context switch so the scheduler's own context
   * stays usable. */
  if (t->c.ci == NULL || t->c.ci->proc == NULL) {
    mrb_task_excl_enter(mrb);
    mrb_task_q_delete(mrb, t);
    t->status = MRB_TASK_STATUS_DORMANT;
    t->c.status = MRB_TASK_STOPPED;
    mrb_task_q_insert(mrb, t);
    mrb_task_excl_exit(mrb);
    return;
  }

  /* Set task as running */
  t->timeslice = MRB_TIMESLICE_TICK_COUNT;
  t->status = MRB_TASK_STATUS_RUNNING;

  /* Switch to task context */
  prev_c = mrb->c;
  prev_ci = prev_c->ci;
  prev_cci = prev_c->ci->cci;
  t->c.prev = mrb->c;
  mrb->c = &t->c;

  /* Clear switching flag */
  switching_ = FALSE;

  /* Save proc and PC to locals before calling mrb_vm_exec */
  const struct RProc *proc = t->c.ci->proc;
  const mrb_code *pc = t->c.ci->pc;

  /* Set vmexec flag to prevent fiber_terminate from being called */
  t->c.vmexec = TRUE;

  /* Execute task - PC is saved in ci->pc from previous run.
     Unhandled task exceptions are converted to the task result by
     mrb_vm_exec() in task mode, so the scheduler protect frame stays intact. */
  execute_task_vm_args args = { t, proc, pc };
  mrb_bool error = FALSE;
  /* mrb_protect_error() roots its result in the GC arena (+1 entry,
   * never popped by the caller loop): one slot per execution slice
   * accumulates forever and pins every slice's result object. The
   * result is already reachable — and marked — through t->result
   * (mrb_task_mark_all), so the arena root is redundant here. */
  int ai = mrb_gc_arena_save(mrb);
  t->result = mrb_protect_error(mrb, execute_task_vm, &args, &error);
  mrb_gc_arena_restore(mrb, ai);
  mrb->task.exception_as_result = FALSE;

  /* Clear vmexec flag */
  t->c.vmexec = FALSE;

  /* Clear switching flag */
  switching_ = FALSE;

  /* Restore context */
  mrb->c = prev_c;
  t->c.prev = NULL;
  prev_c->ci = prev_ci;
  prev_ci->cci = prev_cci;

  /* If an abnormal path inside mrb_vm_exec() bypassed
     exception_as_result and unwound via MRB_THROW (e.g. a
     CINFO_SKIP frame), mrb_protect_error caught it and stored the
     exception object in t->result. Force the task to terminate
     cleanly so the scheduler keeps running instead of aborting -
     re-raising into the scheduler would abort in pattern 1, where
     no outer jmpbuf exists. The exception remains observable via
     mrb_task_value() / Task#value. */
  if (error) {
    t->c.status = MRB_TASK_STOPPED;
  }

  /* Handle task termination */
  if (t->c.status == MRB_TASK_STOPPED) {
    switching_ = FALSE;
    mrb_task_excl_enter(mrb);
    mrb_task_q_delete(mrb, t);
    t->status = MRB_TASK_STATUS_DORMANT;
    mrb_task_q_insert(mrb, t);
    mrb_task_excl_exit(mrb);

    /* Wake up tasks waiting on join */
    wake_up_join_waiters(mrb, t);
  }
  else if (t->status == MRB_TASK_STATUS_RUNNING) {
    /* Task yielded but still running - move to ready queue */
    t->status = MRB_TASK_STATUS_READY;
  }
}

/* Tick handler - called by timer interrupt */
MRB_API void
mrb_tick(mrb_state *mrb)
{
  mrb_task *t;

  /* Increment global tick counter */
  tick_++;

  /* Decrease timeslice for running task */
  t = q_ready_;
  if (t && t->status == MRB_TASK_STATUS_RUNNING && t->timeslice > 0) {
    t->timeslice--;
    if (t->timeslice == 0) {
      switching_ = TRUE;  /* Trigger context switch */
    }
  }

  /* Wake up sleeping tasks whose wakeup time has passed.
   *
   * UINT32_MAX is the "no sleepers" sentinel. Without the explicit
   * check, (int32_t)(UINT32_MAX - tick_) evaluates negative for the
   * first half of the 32-bit range, so the queue walk fires every
   * tick with nothing to do. Short-circuiting saves the walk in the
   * common no-sleepers case -- benefits every HAL, not just
   * tickless ones. */
  if (wakeup_tick_ != UINT32_MAX &&
      (int32_t)(wakeup_tick_ - tick_) <= 0) {
    mrb_task *curr = q_waiting_;
    mrb_task *next;
    uint32_t next_wakeup = UINT32_MAX;

    while (curr != NULL) {
      next = curr->next;

      uint32_t curr_wakeup = UINT32_MAX;
      if (curr->reason == MRB_TASK_REASON_SLEEP) {
        curr_wakeup = curr->wait.wakeup_tick;
      }
      else if (curr->reason == MRB_TASK_REASON_QUEUE) {
        curr_wakeup = curr->wait.queue.wakeup_tick;
      }

      if (curr_wakeup != UINT32_MAX) {
        if ((int32_t)(curr_wakeup - tick_) <= 0) {
          /* Time to wake up. Only clear the queue-specific wait state when the
           * task was actually waiting on a queue; for a SLEEP waiter the active
           * union member is wait.wakeup_tick, not wait.queue. */
          mrb_task_q_delete(mrb, curr);
          curr->status = MRB_TASK_STATUS_READY;
          if (curr->reason == MRB_TASK_REASON_QUEUE) {
            curr->wait.queue.target = NULL;
            curr->wait.queue.wakeup_tick = UINT32_MAX;
          }
          curr->reason = MRB_TASK_REASON_NONE;
          mrb_task_q_insert(mrb, curr);
          switching_ = TRUE;
        }
        else if (next_wakeup == UINT32_MAX ||
                 (int32_t)(curr_wakeup - next_wakeup) < 0) {
          next_wakeup = curr_wakeup;
        }
      }

      curr = next;
    }

    wakeup_tick_ = next_wakeup;
  }
}

/* Body of the main scheduler loop. Wrapped by mrb_task_run() under
   mrb_protect_error so an exception raised from a task body unwinds
   cleanly without leaving `loop_running` set. */
static mrb_value
task_run_body(mrb_state *mrb, void *ud)
{
  mrb_task *t;
  (void)ud;

  while (1) {
    t = q_ready_;

    /* No task ready - check if all tasks are done */
    if (!t) {
      mrb_task_excl_enter(mrb);
      mrb_bool exiting = !q_ready_ && !q_waiting_ && !q_suspended_;
      mrb_task_excl_exit(mrb);
      if (exiting) {
        /* All tasks are dormant - scheduler done */
        break;
      }
      /* Task-scheduled GC: every remaining task is waiting for a tick, so this
         CPU would otherwise idle. Spend it on one unit of GC and loop back to
         re-check q_ready_ -- if mrb_tick woke a task meanwhile we hand it the
         CPU immediately, so GC never delays a tick-driven wakeup. Only sleep
         once there is no GC work left. No-op (returns FALSE) unless
         GC.scheduler_driven is on. */
      if (mrb_gc_scheduler_pending(mrb)) {
        mrb_gc_step(mrb);
        /* q_ready_ was empty above; if a task is READY now, mrb_tick woke it
           *during* the step, so the step delayed it. Record the step's wall
           time as the jitter that task suffered (no-op without MRB_GC_PROFILE). */
        mrb_gc_scheduler_jitter(mrb, q_ready_ != NULL);
        continue;
      }
      /* If there are tasks waiting or suspended, idle */
      mrb_hal_task_idle_cpu(mrb);
      continue;
    }

    /* Safety check - don't execute terminated tasks */
    if (task_cleanup_if_stopped(mrb, t)) {
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

/* Main scheduler loop */
MRB_API mrb_value
mrb_task_run(mrb_state *mrb)
{
  if (mrb->task.loop_running) {
    return mrb_nil_value();
  }
  mrb->task.loop_running = TRUE;

  mrb_bool error = FALSE;
  mrb_value result = mrb_protect_error(mrb, task_run_body, NULL, &error);
  mrb->task.loop_running = FALSE;
  if (error) {
    mrb_exc_raise(mrb, result);
  }
  return result;
}

/* Single-step task execution for WASM event loop integration */
MRB_API mrb_value
mrb_task_run_once(mrb_state *mrb)
{
  mrb_task *t = q_ready_;

  /* No task ready */
  if (!t) {
    /* Task-scheduled GC, single-step variant. Unlike mrb_task_run we cannot
       drain GC in a loop here: this call must stay non-blocking so the host
       event loop keeps spinning, so advance the collector by exactly one unit
       and return. Returning true (progress made) rather than nil tells a host
       that branches on the result to pump again promptly; nil is reserved for
       "truly idle", so GC never stalls because the host backed off to a long
       sleep. Hosts that ignore the result (e.g. a fixed timer) still advance
       GC one step per tick. No-op unless GC.scheduler_driven is on. */
    if (mrb_gc_scheduler_pending(mrb)) {
      mrb_gc_step(mrb);
      mrb_gc_scheduler_jitter(mrb, q_ready_ != NULL);
      return mrb_true_value();
    }
    return mrb_nil_value();
  }

  /* Safety check - don't execute terminated tasks */
  if (task_cleanup_if_stopped(mrb, t)) {
    return mrb_true_value();
  }

  /* Execute task using core logic */
  execute_task(mrb, t);

  /* Move to end of ready queue if still ready (round-robin) */
  if (t->status == MRB_TASK_STATUS_READY) {
    task_change_state(mrb, t, MRB_TASK_STATUS_READY);
  }

  /* Run incremental GC if active */
  if (mrb->gc.state != MRB_GC_STATE_ROOT) {
    mrb_incremental_gc(mrb);
  }

  if (t->status == MRB_TASK_STATUS_DORMANT) {
    return t->result;
  }

  return mrb_true_value();
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

  mrb_task_excl_enter(mrb);

  /* Remove from ready queue */
  mrb_task_q_delete(mrb, t);

  /* Move to waiting queue */
  t->status = MRB_TASK_STATUS_WAITING;
  t->reason = MRB_TASK_REASON_SLEEP;
  /* Convert microseconds to ticks (tick unit is in milliseconds) */
  t->wait.wakeup_tick = mrb_task_normalize_wakeup(tick_ + USEC_TO_TICKS(usec));

  /* Update next wakeup time if this task wakes earlier.
   *
   * When wakeup_tick_ is UINT32_MAX (no prior sleepers), the
   * unsigned subtraction wraps to a small positive value and the
   * int32_t cast stays non-negative, so the update is skipped and
   * the global stays stale until the next mrb_tick self-heals it.
   * Handle the sentinel explicitly so tickless HALs that read this
   * field get a consistent value as soon as the sleeper is
   * installed. */
  if (wakeup_tick_ == UINT32_MAX ||
      (int32_t)(t->wait.wakeup_tick - wakeup_tick_) < 0) {
    wakeup_tick_ = t->wait.wakeup_tick;
  }
  mrb_task_q_insert(mrb, t);

  mrb_task_excl_exit(mrb);

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
    if (mrb->c == mrb->root_c) {
      /* Root context has no task to suspend */
      return mrb_nil_value();
    }
    mrb_callinfo *ci;
    for (ci = mrb->c->ci; ci >= mrb->c->cibase; ci--) {
      if (ci->cci > 0) {
        mrb_raise(mrb, E_RUNTIME_ERROR, "can't sleep across C function boundary");
      }
    }
    mrb_task *t = MRB2TASK(mrb);
    mrb_task_excl_enter(mrb);
    mrb_task_q_delete(mrb, t);
    t->status = MRB_TASK_STATUS_SUSPENDED;
    mrb_task_q_insert(mrb, t);
    mrb_task_excl_exit(mrb);
    switching_ = TRUE;
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

/* Common task creation logic shared by Task.new and mrb_create_task */
static mrb_task*
task_create_common(mrb_state *mrb, const struct RProc *proc,
                   mrb_value name, uint8_t priority)
{
  mrb_task *t = task_alloc(mrb);
  t->priority = priority;
  t->status = MRB_TASK_STATUS_READY;
  t->reason = MRB_TASK_REASON_NONE;
  t->name = name;

  mrb_value task_obj = mrb_obj_value(mrb_data_object_alloc(mrb, mrb_class_get(mrb, "Task"),
                                                           t, &mrb_task_type));
  t->self = task_obj;
  mrb_gc_register(mrb, task_obj);
  task_init_context(mrb, t, proc);

  mrb_task_excl_enter(mrb);
  mrb_task_q_insert(mrb, t);
  mrb_task_excl_exit(mrb);

  if (q_ready_ && q_ready_->status == MRB_TASK_STATUS_RUNNING) {
    if (t->priority < q_ready_->priority) {
      switching_ = TRUE;
    }
  }

  return t;
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
  mrb_sym kw_names[2] = {MRB_SYM(name), MRB_SYM(priority)};
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
    if (!mrb_string_p(kw_values[0])) {
      mrb_raise(mrb, E_TYPE_ERROR, "name must be a String");
    }
    name_val = kw_values[0];
  }
  if (!mrb_undef_p(kw_values[1])) {
    if (!mrb_integer_p(kw_values[1])) {
      mrb_raise(mrb, E_TYPE_ERROR, "priority must be an Integer");
    }
    priority = mrb_integer(kw_values[1]);
    if (priority < 0 || priority > 255) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "priority must be 0-255");
    }
  }

  mrb_task *t = task_create_common(mrb, proc, name_val, (uint8_t)priority);
  return t->self;
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
  if (task_cleanup_if_stopped(mrb, t)) {
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

  mrb_task_excl_enter(mrb);

  /* Add global scheduler state */
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(tick)), mrb_fixnum_value(tick_));
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(wakeup_tick)), mrb_fixnum_value(wakeup_tick_));

  /* Add statistics for each queue */
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(dormant)), mrb_stat_sub(mrb, q_dormant_));
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(ready)), mrb_stat_sub(mrb, q_ready_));
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(waiting)), mrb_stat_sub(mrb, q_waiting_));
  mrb_hash_set(mrb, data, mrb_symbol_value(MRB_SYM(suspended)), mrb_stat_sub(mrb, q_suspended_));

  mrb_task_excl_exit(mrb);

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

  mrb_get_args(mrb, "S", &name);

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

MRB_API mrb_value
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

  mrb_task_excl_enter(mrb);
  t->priority = (uint8_t)priority;

  /* Re-sort in queue if task is ready */
  if (t->status == MRB_TASK_STATUS_READY || t->status == MRB_TASK_STATUS_RUNNING) {
    mrb_task_q_delete(mrb, t);
    mrb_task_q_insert(mrb, t);
  }
  mrb_task_excl_exit(mrb);

  return mrb_fixnum_value(priority);
}

/*
 * Forward declarations for internal functions
 */
static void suspend_task_internal(mrb_state *mrb, mrb_task *t);
static void resume_task_internal(mrb_state *mrb, mrb_task *t);
static void terminate_task_internal(mrb_state *mrb, mrb_task *t);

static mrb_value
mrb_task_suspend(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);
  task_check_scheduler_lock(mrb);

  suspend_task_internal(mrb, t);
  return self;
}

static mrb_value
mrb_task_resume(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);
  task_check_scheduler_lock(mrb);

  resume_task_internal(mrb, t);
  return self;
}

static mrb_value
mrb_task_terminate(mrb_state *mrb, mrb_value self)
{
  mrb_task *t;

  TASK_GET_PTR_OR_RAISE(t, self);
  task_check_scheduler_lock(mrb);

  terminate_task_internal(mrb, t);
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
    return t->result;
  }

  /* Wait for task to complete */
  mrb_task_excl_enter(mrb);
  mrb_task_q_delete(mrb, current);
  current->status = MRB_TASK_STATUS_WAITING;
  current->reason = MRB_TASK_REASON_JOIN;
  current->wait.join = t;
  mrb_task_q_insert(mrb, current);
  mrb_task_excl_exit(mrb);

  /* Trigger context switch */
  switching_ = TRUE;

  return t->result;
}

/*
 * Synchronous execution
 */

/* Execute a proc synchronously without context switching
 *
 * This function creates a temporary task, executes it to completion,
 * and returns the result. The scheduler_lock prevents any asynchronous
 * task operations during execution.
 */
MRB_API mrb_value
mrb_execute_proc_synchronously(mrb_state *mrb, mrb_value proc_val, mrb_int argc, const mrb_value *argv)
{
  struct RProc *proc = mrb_proc_ptr(proc_val);
  int ai = mrb_gc_arena_save(mrb);

  /*
   * argc/argv are reserved for future use (e.g., passing arguments to
   * event handlers or callback functions). Currently all callers pass
   * 0 and NULL.
   */
  (void)argc;
  (void)argv;

  /* 1. Lock scheduler and save context */
  if (mrb->task.scheduler_lock >= MRB_TASK_SCHEDULER_LOCK_MAX) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "scheduler lock overflow");
  }
  mrb->task.scheduler_lock++;
  struct mrb_context *original_c = mrb->c;

  /* 2. Create a temporary task */
  mrb_task *t = task_alloc(mrb);
  t->priority = 0;  /* Highest priority */
  t->status = MRB_TASK_STATUS_DORMANT;
  t->reason = MRB_TASK_REASON_NONE;
  t->name = mrb_str_new_lit(mrb, "(sync)");

  /* Initialize task context */
  task_init_context(mrb, t, proc);

  /* Create wrapper object (not registered with GC as we'll free it manually) */
  struct RClass *task_class = mrb_class_get(mrb, "Task");
  mrb_value task_obj = mrb_obj_value(mrb_data_object_alloc(mrb, task_class, t, &mrb_task_type));
  t->self = task_obj;

  /* 3. Move task from DORMANT to READY */
  mrb_task_excl_enter(mrb);
  t->status = MRB_TASK_STATUS_READY;
  mrb_task_q_insert(mrb, t);
  mrb_task_excl_exit(mrb);

  /* 4. Execute the task in a dedicated loop (no context switching) */
  t->status = MRB_TASK_STATUS_RUNNING;
  mrb->c = &t->c;

  while (t->c.status != MRB_TASK_STOPPED) {
    t->result = mrb_vm_exec(mrb, mrb->c->ci->proc, mrb->c->ci->pc);
  }

  /* If there's an unhandled exception after VM stops, save it as result */
  if (mrb->exc) {
    t->result = mrb_obj_value(mrb->exc);
  }

  /* 5. Get result and clean up */
  mrb_value result = t->result;
  if (mrb_obj_ptr(result) == mrb->exc) {
    mrb->exc = NULL;  /* Clear exception */
  }

  /* 6. Free the temporary task's resources */
  mrb_task_excl_enter(mrb);
  mrb_task_q_delete(mrb, t);
  mrb_task_excl_exit(mrb);

  /* Prevent double-free: clear Data object's type before freeing task */
  DATA_TYPE(task_obj) = NULL;

  /* Free context resources directly (bypass GC since we own this task) */
  if (t->c.stbase) {
    mrb_free(mrb, t->c.stbase);
    t->c.stbase = NULL;
  }
  if (t->c.cibase) {
    mrb_free(mrb, t->c.cibase);
    t->c.cibase = NULL;
  }
  mrb_free(mrb, t);

  /* 7. Restore context and unlock */
  mrb->c = original_c;
  mrb->task.scheduler_lock--;

  mrb_gc_arena_restore(mrb, ai);
  mrb_gc_protect(mrb, result);

  return result;
}

/*
 * Task.tick class method
 */
static mrb_value
mrb_task_s_tick(mrb_state *mrb, mrb_value self)
{
  return mrb_int_value(mrb, tick_ * MRB_TICK_UNIT);
}

/*
 * Create a task from a proc
 * This is called from mrc_create_task() in mrc_utils.c
 */
MRB_API mrb_value
mrb_create_task(mrb_state *mrb, struct RProc *proc, mrb_value name, mrb_value priority, mrb_value top_self)
{
  task_check_scheduler_lock(mrb);

  /* Validate/default priority */
  mrb_int prio = 128;  /* Default priority */
  if (!mrb_nil_p(priority)) {
    if (!mrb_integer_p(priority)) {
      mrb_raise(mrb, E_TYPE_ERROR, "priority must be an Integer");
    }
    prio = mrb_integer(priority);
    if (prio < 0 || prio > 255) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "priority must be 0-255");
    }
  }

  /* Validate/default name */
  mrb_value name_val = mrb_nil_p(name) ? mrb_str_new_lit(mrb, "(noname)") : name;

  mrb_task *t = task_create_common(mrb, proc, name_val, (uint8_t)prio);

  /* Set top_self if provided */
  if (!mrb_nil_p(top_self)) {
    t->c.ci->stack[0] = top_self;
  }

  return t->self;
}

/*
 * Internal: Suspend a task (no validation, no scheduler_lock check)
 */
static void
suspend_task_internal(mrb_state *mrb, mrb_task *t)
{
  /*
   * WAITING task should also be suspended:
   *   Suspend trigger may occur while the task is sleeping (WAITING).
   * DORMANT task should also be suspended:
   *   e.g., IRB in PicoRuby suspends a DORMANT task to use it again.
   */
  if (t->status == MRB_TASK_STATUS_SUSPENDED) return;

  /*
   * Determine if context switch is needed BEFORE changing state.
   * Context switch is needed when suspending a RUNNING task or
   * the current ready task.
   */
  mrb_bool need_switch = (t == q_ready_ || t->status == MRB_TASK_STATUS_RUNNING);

  task_change_state(mrb, t, MRB_TASK_STATUS_SUSPENDED);

  if (need_switch) {
    switching_ = TRUE;
  }
}

/*
 * Suspend a task
 */
MRB_API void
mrb_suspend_task(mrb_state *mrb, mrb_value task)
{
  task_check_scheduler_lock(mrb);

  mrb_task *t = (mrb_task*)mrb_data_check_get_ptr(mrb, task, &mrb_task_type);
  if (!t) return;

  suspend_task_internal(mrb, t);
}

/*
 * Internal: Resume a task (no validation, no scheduler_lock check)
 */
static void
resume_task_internal(mrb_state *mrb, mrb_task *t)
{
  if (t->status != MRB_TASK_STATUS_SUSPENDED) return;

  /* Determine target state based on reason */
  uint8_t target_status = (t->reason == MRB_TASK_REASON_NONE) ?
                          MRB_TASK_STATUS_READY : MRB_TASK_STATUS_WAITING;

  task_change_state(mrb, t, target_status);

  /* Trigger context switch if resumed task has higher priority */
  if (target_status == MRB_TASK_STATUS_READY && q_ready_ &&
      q_ready_->status == MRB_TASK_STATUS_RUNNING) {
    if (t->priority < q_ready_->priority) {
      switching_ = TRUE;
    }
  }

  /* Update wakeup_tick if task has sleep reason.
   *
   * Two fixes here vs the original:
   *   - The UINT32_MAX sentinel case (see comments in
   *     sleep_us_impl).
   *   - The read-modify-write on wakeup_tick_ races with
   *     mrb_tick, which also rewrites this field. sleep_us_impl
   *     already wraps its update in the IRQ pair; we need the
   *     same here to match the locking discipline. */
  if (t->reason == MRB_TASK_REASON_SLEEP ||
      (t->reason == MRB_TASK_REASON_QUEUE &&
       t->wait.queue.wakeup_tick != UINT32_MAX)) {
    uint32_t task_wakeup = t->reason == MRB_TASK_REASON_SLEEP ?
                           t->wait.wakeup_tick : t->wait.queue.wakeup_tick;
    mrb_task_excl_enter(mrb);
    if (wakeup_tick_ == UINT32_MAX ||
        (int32_t)(task_wakeup - wakeup_tick_) < 0) {
      wakeup_tick_ = task_wakeup;
    }
    mrb_task_excl_exit(mrb);
  }
}

/*
 * Resume a task
 */
MRB_API void
mrb_resume_task(mrb_state *mrb, mrb_value task)
{
  task_check_scheduler_lock(mrb);

  mrb_task *t = (mrb_task*)mrb_data_check_get_ptr(mrb, task, &mrb_task_type);
  if (!t) return;

  resume_task_internal(mrb, t);
}

/*
 * Internal: Terminate a task (no validation, no scheduler_lock check)
 */
static void
terminate_task_internal(mrb_state *mrb, mrb_task *t)
{
  if (t->status == MRB_TASK_STATUS_DORMANT) return;

  mrb_bool was_running = (t->status == MRB_TASK_STATUS_RUNNING);

  mrb_task_excl_enter(mrb);
  mrb_task_q_delete(mrb, t);
  t->status = MRB_TASK_STATUS_DORMANT;
  t->c.status = MRB_TASK_STOPPED;
  mrb_task_q_insert(mrb, t);
  mrb_task_excl_exit(mrb);

  wake_up_join_waiters(mrb, t);

  /* If terminating self, trigger context switch */
  if (was_running) {
    switching_ = TRUE;
  }
}

/*
 * Terminate a task
 */
MRB_API void
mrb_terminate_task(mrb_state *mrb, mrb_value task)
{
  task_check_scheduler_lock(mrb);

  mrb_task *t = (mrb_task*)mrb_data_check_get_ptr(mrb, task, &mrb_task_type);
  if (!t) return;

  terminate_task_internal(mrb, t);
}

/*
 * Stop a task (mark as stopped but don't move to dormant)
 */
MRB_API mrb_bool
mrb_stop_task(mrb_state *mrb, mrb_value task)
{
  task_check_scheduler_lock(mrb);

  mrb_task *t = (mrb_task*)mrb_data_check_get_ptr(mrb, task, &mrb_task_type);
  if (!t) return FALSE;

  if (t->c.status == MRB_TASK_STOPPED) {
    return FALSE;  /* Already stopped */
  }
  t->c.status = MRB_TASK_STOPPED;
  return TRUE;
}

/*
 * Get task result value
 */
MRB_API mrb_value
mrb_task_value(mrb_state *mrb, mrb_value task)
{
  mrb_task *t = (mrb_task*)mrb_data_check_get_ptr(mrb, task, &mrb_task_type);
  if (!t) return mrb_nil_value();

  return t->result;
}

/*
 * Initialize task context with a new proc
 */
MRB_API void
mrb_task_init_context(mrb_state *mrb, mrb_value task, struct RProc *proc)
{
  task_check_scheduler_lock(mrb);

  mrb_task *t = (mrb_task*)mrb_data_check_get_ptr(mrb, task, &mrb_task_type);
  if (!t) return;

  struct mrb_context *c = &t->c;

  /* Cleanup existing context if any */
  if (c->stbase) {
    mrb_free(mrb, c->stbase);
    c->stbase = NULL;
  }
  if (c->cibase) {
    mrb_free(mrb, c->cibase);
    c->cibase = NULL;
  }

  /* Re-initialize context */
  task_init_context(mrb, t, proc);
}

/*
 * Reset task context to initial state
 */
MRB_API void
mrb_task_reset_context(mrb_state *mrb, mrb_value task)
{
  task_check_scheduler_lock(mrb);

  mrb_task *t = (mrb_task*)mrb_data_check_get_ptr(mrb, task, &mrb_task_type);
  if (!t) return;

  struct mrb_context *c = &t->c;
  c->ci = c->cibase;
  c->status = MRB_TASK_CREATED;
  if (c->ci) {
    mrb_vm_ci_target_class_set(c->ci, mrb->object_class);
  }
}

/*
 * Set proc for task
 */
MRB_API void
mrb_task_proc_set(mrb_state *mrb, mrb_value task, struct RProc *proc)
{
  task_check_scheduler_lock(mrb);

  mrb_task *t = (mrb_task*)mrb_data_check_get_ptr(mrb, task, &mrb_task_type);
  if (!t) return;

  /* Handle environment resize if needed */
  if (t->c.cibase && t->c.cibase->u.env) {
    struct REnv *e = mrb_vm_ci_env(t->c.cibase);
    if (e && MRB_ENV_LEN(e) < proc->body.irep->nlocals) {
      MRB_ENV_SET_LEN(e, proc->body.irep->nlocals);
    }
  }

  if (t->c.ci) {
    mrb_vm_ci_proc_set(t->c.ci, proc);
  }
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

  /* Initialize main task to NULL and scheduler_lock to 0 */
  mrb->task.main_task = NULL;
  mrb->task.scheduler_lock = 0;
  mrb->task.irq_nesting = 0;
  mrb->task.loop_running = FALSE;
  mrb->task.exception_as_result = FALSE;

  task_class = mrb_define_class_id(mrb, MRB_SYM(Task), mrb->object_class);
  MRB_SET_INSTANCE_TT(task_class, MRB_TT_DATA);

  /* Task::Error - base error class for task synchronization errors */
  mrb_define_class_under_id(mrb, task_class, MRB_SYM(Error), mrb->eStandardError_class);

  /* Task::Queue */
  mrb_init_task_queue(mrb, task_class);

  /* GC.scheduler_driven family */
  mrb_init_task_gc(mrb);

  /* Class methods */
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(new),     mrb_task_s_new,     MRB_ARGS_KEY(2,0)|MRB_ARGS_BLOCK());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(current), mrb_task_s_current, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(list),    mrb_task_s_list,    MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(pass),    mrb_task_s_pass,    MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(stat),    mrb_task_s_stat,    MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(get),     mrb_task_s_get,     MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(run),     mrb_task_s_run,     MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, task_class, MRB_SYM(tick),    mrb_task_s_tick,    MRB_ARGS_NONE());

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
  mrb_define_method_id(mrb, task_class, MRB_SYM(value),       mrb_task_value,        MRB_ARGS_NONE());

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
