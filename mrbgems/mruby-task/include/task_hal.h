/*
** task_hal.h - Task scheduler Hardware Abstraction Layer (HAL)
**
** See Copyright Notice in mruby.h
**
** This header defines the HAL interface that platform-specific implementations
** must provide. The HAL separates platform-specific timer and interrupt handling
** from the core task scheduler logic.
*/

#ifndef MRUBY_TASK_HAL_H
#define MRUBY_TASK_HAL_H

#include <mruby.h>

/*
 * Configuration - can be overridden in platform-specific build configs
 */

/* Tick period in milliseconds - how often the timer fires */
#ifndef MRB_TICK_UNIT
#define MRB_TICK_UNIT 4
#endif

/* Number of timer ticks per task timeslice */
#ifndef MRB_TIMESLICE_TICK_COUNT
#define MRB_TIMESLICE_TICK_COUNT 3
#endif

/* Maximum number of concurrent mrb_state instances with task scheduler */
#ifndef MRB_TASK_MAX_VMS
#define MRB_TASK_MAX_VMS 8
#endif

/*
 * HAL Interface Functions
 *
 * Platform-specific implementations (hal-posix-task, hal-win-task, etc.)
 * must provide these functions.
 */

/**
 * Initialize hardware timer and interrupt system
 *
 * Called once during mruby-task gem initialization. Should set up a periodic
 * timer that calls mrb_tick(mrb) every MRB_TICK_UNIT milliseconds.
 *
 * Requirements:
 * - Initialize platform-specific timer hardware/APIs
 * - Set up timer to fire every MRB_TICK_UNIT milliseconds
 * - Register mrb_state for multi-VM support if needed
 * - Initialize interrupt protection mechanisms (mutexes, signal masks, etc.)
 * - Timer should call mrb_tick() on each tick for all registered VMs
 *
 * @param mrb The mruby state to associate with the timer
 */
void mrb_hal_task_init(mrb_state *mrb);

/**
 * Cleanup timer and interrupt resources
 *
 * Called during mruby-task gem finalization. Should clean up all resources
 * allocated by mrb_hal_task_init().
 *
 * Requirements:
 * - Stop and cleanup platform timer
 * - Unregister mrb_state from multi-VM support
 * - Free any allocated HAL resources
 * - If last VM, cleanup global HAL state
 *
 * @param mrb The mruby state to disassociate from the timer
 */
void mrb_hal_task_final(mrb_state *mrb);

/**
 * Enable timer interrupts (exit critical section)
 *
 * Called by the task scheduler when it's safe to allow timer interrupts.
 * Should enable timer interrupts/callbacks that were disabled by
 * mrb_task_disable_irq().
 *
 * Requirements:
 * - Must be reentrant (can be called multiple times)
 * - Should use nesting counter or equivalent for nested critical sections
 * - On POSIX: unmask signals
 * - On Windows: leave critical section
 * - On embedded: enable timer interrupts
 */
void mrb_task_enable_irq(void);

/**
 * Disable timer interrupts (enter critical section)
 *
 * Called by the task scheduler before modifying shared task state.
 * Should disable timer interrupts/callbacks to prevent concurrent access.
 *
 * Requirements:
 * - Must be reentrant (can be called multiple times)
 * - Should use nesting counter or equivalent for nested critical sections
 * - On POSIX: block signals
 * - On Windows: enter critical section
 * - On embedded: disable timer interrupts
 */
void mrb_task_disable_irq(void);

/**
 * Put CPU in low-power/idle mode
 *
 * Called by the scheduler when no tasks are ready to run but some tasks
 * are waiting or suspended. Should briefly idle the CPU or sleep for
 * approximately MRB_TICK_UNIT milliseconds to allow timer to fire.
 *
 * Requirements:
 * - Should return when timer fires or after ~MRB_TICK_UNIT milliseconds
 * - Must allow timer interrupts to occur during idle
 * - On POSIX: usleep() or nanosleep()
 * - On Windows: Sleep()
 * - On embedded: platform-specific sleep/wait-for-interrupt instruction
 *
 * @param mrb The mruby state (for context, may be unused)
 */
void mrb_hal_task_idle_cpu(mrb_state *mrb);

/**
 * Sleep for specified microseconds in wall-clock time
 *
 * Called by sleep functions when in root context (not in a task).
 * Should sleep for the specified number of microseconds in real wall-clock
 * time, allowing timer interrupts to occur during the sleep.
 *
 * Requirements:
 * - Sleep for approximately usec microseconds in wall-clock time
 * - Must allow timer interrupts/callbacks during sleep
 * - Should handle interruptions gracefully and complete full sleep duration
 * - On POSIX: use clock_gettime + nanosleep loop for accuracy
 * - On Windows: use Sleep() with millisecond conversion
 * - On embedded: platform-specific delay with interrupt support
 *
 * @param mrb The mruby state (for context, may be unused)
 * @param usec Number of microseconds to sleep
 */
void mrb_hal_task_sleep_us(mrb_state *mrb, mrb_int usec);

/*
 * Core scheduler function (implemented in task.c, called by HAL)
 */

/**
 * Tick handler - advances scheduler time and wakes sleeping tasks
 *
 * HAL timer callback must call this function every MRB_TICK_UNIT milliseconds
 * for each registered mrb_state. This function:
 * - Increments the global tick counter
 * - Decrements running task's timeslice
 * - Wakes tasks whose sleep time has expired
 * - Triggers context switches when needed
 *
 * @param mrb The mruby state to tick
 */
void mrb_tick(mrb_state *mrb);

#endif /* MRUBY_TASK_HAL_H */
