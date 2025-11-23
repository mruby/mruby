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
};

/*
 * Task structure - represents a single task in the scheduler
 *
 * Memory-optimized layout:
 * - Removed priority_preemption (always equals priority): 1 byte
 * - Removed started flag (inferred from c.status): 1 byte
 * - Unified wakeup_tick/join/mutex into single union: 4 bytes
 * - Removed redundant proc field (stored in c.ci->proc): 8 bytes
 * - Unified timeslice/result into state union: ~4 bytes
 * Total savings: ~18 bytes per task (14% reduction)
 */
typedef struct mrb_task {
  struct mrb_task *next;           /* Linked list pointer */
  uint8_t priority;                /* Priority (0-255, 0=highest) */
  uint8_t status;                  /* Current status (TASKSTATUS enum) */
  uint8_t reason;                  /* Wait reason (TASKREASON enum) */
  mrb_value name;                  /* Optional task name */

  /* Wait-specific data - mutually exclusive based on reason field */
  union {
    uint32_t wakeup_tick;          /* Tick count to wake up (REASON_SLEEP) */
    const struct mrb_task *join;   /* Task being waited on (REASON_JOIN) */
    void *mutex;                   /* Mutex pointer (REASON_MUTEX, reserved) */
  } wait;

  mrb_value self;                  /* Ruby Task object reference */

  /* State-specific data - mutually exclusive based on status */
  union {
    volatile uint8_t timeslice;    /* Remaining ticks (RUNNING only) */
    mrb_value result;              /* Task return value (DORMANT only) */
  } state;

  struct mrb_context c;            /* Execution context (stack, callinfo, etc) */
} mrb_task;

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
 * HAL (Hardware Abstraction Layer) functions
 * Platform-specific implementations must provide these
 */
void mrb_task_hal_init(mrb_state *mrb);
void mrb_task_hal_final(mrb_state *mrb);
void mrb_task_enable_irq(void);
void mrb_task_disable_irq(void);
void mrb_task_hal_idle_cpu(mrb_state *mrb);

/*
 * Core task scheduler API
 */
void mrb_tick(mrb_state *mrb);
mrb_value mrb_task_run(mrb_state *mrb);
void mrb_task_mark_all(mrb_state *mrb);

#endif /* MRUBY_TASK_H */
