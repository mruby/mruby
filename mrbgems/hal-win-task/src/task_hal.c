/*
** task_hal.c - Windows HAL implementation for mruby-task
**
** See Copyright Notice in mruby.h
**
** Windows implementation using multimedia timer (timeSetEvent/timeKillEvent)
** for periodic timer, and CRITICAL_SECTION for interrupt protection.
**
** Supported platforms: Windows (all versions with multimedia timer support)
*/

#include <mruby.h>
#include "task_hal.h"
#include <windows.h>
#include <timeapi.h>
#include <stdint.h>

/* Multi-VM support */
static mrb_state *vm_list[MRB_TASK_MAX_VMS];
static volatile LONG vm_count = 0;
static CRITICAL_SECTION irq_lock;
static MMRESULT timer_id = 0;

/* Multimedia timer callback - called periodically by Windows */
static void CALLBACK
timer_callback(UINT uID, UINT uMsg, DWORD_PTR dwUser, DWORD_PTR dw1, DWORD_PTR dw2)
{
  int i;
  (void)uID; (void)uMsg; (void)dwUser; (void)dw1; (void)dw2;

  /* Tick all registered VMs */
  EnterCriticalSection(&irq_lock);
  for (i = 0; i < vm_count; i++) {
    if (vm_list[i]) {
      mrb_tick(vm_list[i]);
    }
  }
  LeaveCriticalSection(&irq_lock);
}

/*
 * HAL Interface Implementation
 */

void
mrb_hal_task_init(mrb_state *mrb)
{
  int i;
  LONG idx;

  /* Initialize task state */
  for (i = 0; i < 4; i++) {
    mrb->task.queues[i] = NULL;
  }
  mrb->task.tick = 0;
  mrb->task.wakeup_tick = UINT32_MAX;
  mrb->task.switching = FALSE;

  /* Initialize critical section on first VM */
  if (vm_count == 0) {
    InitializeCriticalSection(&irq_lock);
  }

  EnterCriticalSection(&irq_lock);

  /* Check if this VM is already registered */
  idx = -1;
  for (i = 0; i < vm_count; i++) {
    if (vm_list[i] == mrb) {
      idx = i;
      break;
    }
  }

  /* Register new VM if not already present */
  if (idx < 0) {
    if (vm_count >= MRB_TASK_MAX_VMS) {
      LeaveCriticalSection(&irq_lock);
      mrb_raisef(mrb, E_RUNTIME_ERROR,
                 "too many mrb_states with task scheduler (max: %d)",
                 MRB_TASK_MAX_VMS);
    }
    vm_list[vm_count] = mrb;
    InterlockedIncrement(&vm_count);
  }

  /* Start timer for first VM */
  if (vm_count == 1) {
    /* Request 1ms timer resolution */
    timeBeginPeriod(1);

    /* Create periodic timer with MRB_TICK_UNIT interval */
    timer_id = timeSetEvent(
      MRB_TICK_UNIT,           /* interval in milliseconds */
      1,                        /* resolution in milliseconds */
      timer_callback,           /* callback function */
      0,                        /* user data */
      TIME_PERIODIC | TIME_KILL_SYNCHRONOUS
    );
  }

  LeaveCriticalSection(&irq_lock);
}

void
mrb_task_enable_irq(void)
{
  LeaveCriticalSection(&irq_lock);
}

void
mrb_task_disable_irq(void)
{
  EnterCriticalSection(&irq_lock);
}

void
mrb_hal_task_idle_cpu(mrb_state *mrb)
{
  (void)mrb;
  /* On Windows, just sleep briefly */
  Sleep(MRB_TICK_UNIT);
}

void
mrb_hal_task_sleep_us(mrb_state *mrb, mrb_int usec)
{
  (void)mrb;

  /* Windows Sleep() takes milliseconds, convert from microseconds */
  if (usec >= 0) {
    Sleep((DWORD)(usec / 1000));
  }
}

void
mrb_hal_task_final(mrb_state *mrb)
{
  int i, j;

  EnterCriticalSection(&irq_lock);

  /* Find and remove this VM from the list */
  for (i = 0; i < vm_count; i++) {
    if (vm_list[i] == mrb) {
      /* Shift remaining VMs down */
      for (j = i; j < vm_count - 1; j++) {
        vm_list[j] = vm_list[j + 1];
      }
      vm_list[vm_count - 1] = NULL;
      InterlockedDecrement(&vm_count);
      break;
    }
  }

  /* Stop timer if last VM */
  if (vm_count == 0) {
    if (timer_id != 0) {
      timeKillEvent(timer_id);
      timeEndPeriod(1);
      timer_id = 0;
    }
    LeaveCriticalSection(&irq_lock);
    DeleteCriticalSection(&irq_lock);
  }
  else {
    LeaveCriticalSection(&irq_lock);
  }
}

/*
 * Gem initialization (empty - HAL functions called by mruby-task)
 */

void
mrb_hal_win_task_gem_init(mrb_state *mrb)
{
  (void)mrb;
  /* HAL interface functions are called by mruby-task gem */
}

void
mrb_hal_win_task_gem_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup handled by mrb_hal_task_final called from mruby-task */
}
