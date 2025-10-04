/*
** task.h - Task scheduler
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_TASK_H
#define MRUBY_TASK_H

#include <mruby.h>

#ifdef MRB_USE_TASK_SCHEDULER

/*
 * Task status values (bit-mapped)
 */
enum {
  MRB_TASKSTATUS_DORMANT   = 0x00,  /* Not started or finished */
  MRB_TASKSTATUS_READY     = 0x02,  /* Ready to run */
  MRB_TASKSTATUS_RUNNING   = 0x03,  /* Currently executing */
  MRB_TASKSTATUS_WAITING   = 0x04,  /* Waiting for condition */
  MRB_TASKSTATUS_SUSPENDED = 0x08,  /* Manually suspended */
};

/*
 * Task wait reason
 */
enum {
  MRB_TASKREASON_NONE  = 0x00,  /* No specific reason */
  MRB_TASKREASON_SLEEP = 0x01,  /* Sleeping for time */
  MRB_TASKREASON_MUTEX = 0x02,  /* Waiting for mutex (reserved) */
  MRB_TASKREASON_JOIN  = 0x04,  /* Waiting for another task */
};

/*
 * Task structure - represents a single task in the scheduler
 */
typedef struct mrb_task {
  struct mrb_task *next;           /* Linked list pointer */
  uint8_t priority;                /* Initial priority (0-255, 0=highest) */
  uint8_t priority_preemption;     /* Effective priority for preemption */
  volatile uint8_t timeslice;      /* Remaining time slice ticks */
  uint8_t status;                  /* Current status (TASKSTATUS enum) */
  uint8_t reason;                  /* Wait reason (TASKREASON enum) */
  mrb_value name;                  /* Optional task name */

  union {
    uint32_t wakeup_tick;          /* Tick count to wake up (for sleep) */
    void *mutex;                   /* Mutex pointer (reserved) */
  };
  const struct mrb_task *join;     /* Task being waited on (for join) */

  mrb_value self;                  /* Ruby Task object reference */
  mrb_value result;                /* Task return value */
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
#define TASK_CI_INIT_SIZE 8       /* Initial task callinfo size */

/*
 * HAL (Hardware Abstraction Layer) functions
 * Platform-specific implementations must provide these
 */
void mrb_task_hal_init(mrb_state *mrb);
void mrb_task_enable_irq(void);
void mrb_task_disable_irq(void);
void mrb_task_hal_idle_cpu(mrb_state *mrb);

/*
 * Core task scheduler API
 */
void mrb_tick(mrb_state *mrb);
mrb_value mrb_tasks_run(mrb_state *mrb);

/*
 * Task context status values (extends mrb_fiber_state)
 */
#define MRB_TASK_CREATED  (MRB_FIBER_TRANSFERRED + 1)
#define MRB_TASK_STOPPED  (MRB_FIBER_TRANSFERRED + 2)

#endif /* MRB_USE_TASK_SCHEDULER */

#endif /* MRUBY_TASK_H */
