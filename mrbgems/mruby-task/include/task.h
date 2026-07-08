/*
** task.h - Task scheduler
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_TASK_H
#define MRUBY_TASK_H

#include <mruby.h>

/*
 * Task status values (bit-mapped)
 */
enum {
  MRB_TASK_STATUS_DORMANT   = 0x00,  /* Not started or finished */
  MRB_TASK_STATUS_READY     = 0x02,  /* Ready to run */
  MRB_TASK_STATUS_RUNNING   = 0x03,  /* Currently executing */
  MRB_TASK_STATUS_WAITING   = 0x04,  /* Waiting for condition */
  MRB_TASK_STATUS_SUSPENDED = 0x08,  /* Manually suspended */
};

/*
 * Task wait reason
 */
enum {
  MRB_TASK_REASON_NONE  = 0x00,  /* No specific reason */
  MRB_TASK_REASON_SLEEP = 0x01,  /* Sleeping for time */
  MRB_TASK_REASON_MUTEX = 0x02,  /* Waiting for mutex (reserved) */
  MRB_TASK_REASON_JOIN  = 0x04,  /* Waiting for another task */
  MRB_TASK_REASON_QUEUE = 0x08,  /* Waiting for queue item */
};

struct mrb_task_queue;

/*
 * Task structure - represents a single task in the scheduler
 *
 * Memory-optimized layout:
 * - Removed priority_preemption (always equals priority): 1 byte
 * - Removed started flag (inferred from c.status): 1 byte
 * - Unified wait-specific state into a single union
 * - Removed redundant proc field (stored in c.ci->proc): 8 bytes
 * Total savings on 32-bit targets: ~14 bytes per task
 */
typedef struct mrb_task {
  struct mrb_task *next;           /* Linked list pointer */
  uint8_t priority;                /* Priority (0-255, 0=highest) */
  uint8_t status;                  /* Current status (TASKSTATUS enum) */
  uint8_t reason;                  /* Wait reason (TASKREASON enum) */
  volatile uint8_t timeslice;      /* Remaining ticks while RUNNING */
  mrb_value name;                  /* Optional task name */

  /* Wait-specific data - mutually exclusive based on reason field */
  union {
    uint32_t wakeup_tick;          /* Tick count to wake up (REASON_SLEEP) */
    const struct mrb_task *join;   /* Task being waited on (REASON_JOIN) */
    void *mutex;                   /* Mutex pointer (REASON_MUTEX, reserved) */
    struct {
      struct mrb_task_queue *target;
      uint32_t wakeup_tick;        /* UINT32_MAX when no timeout */
    } queue;                       /* Queue wait state (REASON_QUEUE) */
  } wait;

  mrb_value self;                  /* Ruby Task object reference */

  mrb_value result;                /* Task return value */

  struct mrb_context c;            /* Execution context (stack, callinfo, etc) */
} mrb_task;

/* Normalize a freshly computed wakeup deadline so it can never equal the
 * UINT32_MAX "no timed wakeup" sentinel used in the wait.wakeup_tick /
 * wait.queue.wakeup_tick slots. The wrapping tick counter can, once every
 * 2^32 ticks, produce a real deadline of exactly UINT32_MAX; left as-is the
 * scheduler would mistake it for "no timeout" and never wake the task. Pulling
 * it back by one tick removes the collision at a cost of at most one tick. */
static inline uint32_t
mrb_task_normalize_wakeup(uint32_t deadline)
{
  return deadline == UINT32_MAX ? UINT32_MAX - 1 : deadline;
}

/*
 * Task queue configuration
 * (mrb_task_state is defined in mruby.h)
 */
#define MRB_NUM_TASK_QUEUE 4

/* Queue indices */
#define MRB_TASK_QUEUE_DORMANT   0
#define MRB_TASK_QUEUE_READY     1
#define MRB_TASK_QUEUE_WAITING   2
#define MRB_TASK_QUEUE_SUSPENDED 3

/* Configuration */
#ifndef MRB_TICK_UNIT
#define MRB_TICK_UNIT 4  /* Tick period in milliseconds */
#endif

#ifndef MRB_TIMESLICE_TICK_COUNT
#define MRB_TIMESLICE_TICK_COUNT 3  /* Number of ticks per timeslice */
#endif

#define TASK_STACK_INIT_SIZE 64   /* Initial task stack size */
#define TASK_CI_INIT_SIZE 4       /* Initial task callinfo size */

/*
 * HAL (Hardware Abstraction Layer) functions are declared in task_hal.h.
 */

/*
 * GC integration
 */
void mrb_task_mark_all(mrb_state *mrb);

/*
 * Core task scheduler API
 */
MRB_API void mrb_tick(mrb_state *mrb);
MRB_API mrb_value mrb_task_run(mrb_state *mrb);
MRB_API mrb_value mrb_task_run_once(mrb_state *mrb);

/*
 * Task creation API
 */
MRB_API mrb_value mrb_create_task(mrb_state *mrb, struct RProc *proc, mrb_value name, mrb_value priority, mrb_value top_self);

/*
 * Synchronous execution API (for picoruby-wasm)
 */
MRB_API mrb_value mrb_execute_proc_synchronously(mrb_state *mrb, mrb_value proc, mrb_int argc, const mrb_value *argv);

/*
 * Task control API
 * Note: mrb_task_run is the main scheduler loop (for picoruby-sandbox and picoruby-wasm)
 */
MRB_API void mrb_suspend_task(mrb_state *mrb, mrb_value task);
MRB_API void mrb_resume_task(mrb_state *mrb, mrb_value task);
MRB_API void mrb_terminate_task(mrb_state *mrb, mrb_value task);
MRB_API mrb_bool mrb_stop_task(mrb_state *mrb, mrb_value task);
MRB_API mrb_value mrb_task_value(mrb_state *mrb, mrb_value task);
MRB_API mrb_value mrb_task_status(mrb_state *mrb, mrb_value self);

/*
 * Task context management API (for picoruby-sandbox)
 */
MRB_API void mrb_task_init_context(mrb_state *mrb, mrb_value task, struct RProc *proc);
MRB_API void mrb_task_reset_context(mrb_state *mrb, mrb_value task);
MRB_API void mrb_task_proc_set(mrb_state *mrb, mrb_value task, struct RProc *proc);

/*
 * Internal helpers - used by task.c and task_queue.c
 */
#include <stddef.h>
#include "task_hal.h"

/* Scheduler state accessors (require a local mrb variable in scope) */
#define q_dormant_   (mrb->task.queues[MRB_TASK_QUEUE_DORMANT])
#define q_ready_     (mrb->task.queues[MRB_TASK_QUEUE_READY])
#define q_waiting_   (mrb->task.queues[MRB_TASK_QUEUE_WAITING])
#define q_suspended_ (mrb->task.queues[MRB_TASK_QUEUE_SUSPENDED])
#define tick_        (mrb->task.tick)
#define wakeup_tick_ (mrb->task.wakeup_tick)
#define switching_   (mrb->task.switching)

/* Recover the mrb_task that owns the current mruby context */
#define MRB2TASK(mrb) ((mrb_task *)((uint8_t *)(mrb)->c - offsetof(mrb_task, c)))

/* Raise if the scheduler is locked (synchronous execution in progress) */
static inline void
task_check_scheduler_lock(mrb_state *mrb)
{
  if (mrb->task.scheduler_lock > 0) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "Cannot use asynchronous Task API during synchronous execution");
  }
}

/* Priority-queue insert/delete - defined in task.c */
void mrb_task_q_insert(mrb_state *mrb, mrb_task *t);
void mrb_task_q_delete(mrb_state *mrb, mrb_task *t);

/* Task::Queue class registration - defined in task_queue.c */
void mrb_init_task_queue(mrb_state *mrb, struct RClass *task_class);

/*
 * Nesting-safe scheduler-IRQ exclusion.
 *
 * The HAL primitives (mrb_task_disable_irq/mrb_task_enable_irq) are not
 * required to nest (sigprocmask on POSIX, plain interrupt enable/disable
 * on microcontrollers): a naive inner enable would reopen the exclusion
 * for its outer section. Nesting does happen — GC marking
 * (mrb_task_mark_all) excludes the tick, and a GC cycle can be triggered
 * by an allocation made inside an already-excluded section (e.g.
 * Task.stat building its result hash). Every scheduler-IRQ exclusion in
 * this gem must therefore go through these counted helpers; only the
 * outermost level touches the HAL.
 *
 * The depth counter is per-VM (mrb->task.irq_nesting) and is only ever
 * accessed from the VM's own thread — the tick itself never takes the
 * exclusion — so no atomicity is required, and multiple VMs on
 * different threads stay independent even when the HAL shares one lock
 * (as the Windows HAL does).
 */
static inline void
mrb_task_excl_enter(mrb_state *mrb)
{
  mrb_assert(mrb->task.irq_nesting < UINT8_MAX);
  if (mrb->task.irq_nesting++ == 0) {
    mrb_task_disable_irq();
  }
}

static inline void
mrb_task_excl_exit(mrb_state *mrb)
{
  mrb_assert(mrb->task.irq_nesting > 0);
  if (--mrb->task.irq_nesting == 0) {
    mrb_task_enable_irq();
  }
}

#endif /* MRUBY_TASK_H */
