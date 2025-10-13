/*
** task_hal.c - POSIX HAL implementation for mruby-task
**
** See Copyright Notice in mruby.h
**
** POSIX implementation using SIGALRM and setitimer() for timer,
** and sigprocmask() for interrupt protection.
**
** Supported platforms: Linux, macOS, BSD, Unix
*/

#include <mruby.h>
#include "task_hal.h"
#include <signal.h>
#include <sys/time.h>
#include <unistd.h>
#include <stdint.h>

/* Multi-VM support */
static mrb_state *vm_list[MRB_TASK_MAX_VMS];
static volatile sig_atomic_t vm_count = 0;
static sigset_t alarm_mask;

/* SIGALRM signal handler - ticks all registered VMs */
static void
sigalrm_handler(int sig)
{
  int i;
  (void)sig;
  /* Tick all registered VMs */
  for (i = 0; i < vm_count; i++) {
    if (vm_list[i]) {
      mrb_tick(vm_list[i]);
    }
  }
}

/*
 * HAL Interface Implementation
 */

void
mrb_task_hal_init(mrb_state *mrb)
{
  struct sigaction sa;
  struct itimerval timer;
  int i;
  int vm_index = -1;

  /* Initialize task state */
  for (i = 0; i < 4; i++) {
    mrb->task.queues[i] = NULL;
  }
  mrb->task.tick = 0;
  mrb->task.wakeup_tick = UINT32_MAX;
  mrb->task.switching = FALSE;

  /* Block SIGALRM during registration to avoid race */
  sigemptyset(&alarm_mask);
  sigaddset(&alarm_mask, SIGALRM);
  sigprocmask(SIG_BLOCK, &alarm_mask, NULL);

  /* Check if this VM is already registered */
  for (i = 0; i < vm_count; i++) {
    if (vm_list[i] == mrb) {
      vm_index = i;
      break;
    }
  }

  /* Register new VM if not already present */
  if (vm_index < 0) {
    if (vm_count >= MRB_TASK_MAX_VMS) {
      sigprocmask(SIG_UNBLOCK, &alarm_mask, NULL);
      mrb_raisef(mrb, E_RUNTIME_ERROR,
                 "too many mrb_states with task scheduler (max: %d)",
                 MRB_TASK_MAX_VMS);
    }
    vm_list[vm_count] = mrb;
    vm_count++;
  }

  /* Set up signal handler and timer only for first VM */
  if (vm_count == 1) {
    /* Set up signal handler - SA_RESTART to avoid breaking IO operations */
    sa.sa_handler = sigalrm_handler;
    sa.sa_flags = SA_RESTART;
    sigemptyset(&sa.sa_mask);
    sigaction(SIGALRM, &sa, NULL);

    /* Start timer */
    timer.it_value.tv_sec = 0;
    timer.it_value.tv_usec = MRB_TICK_UNIT * 1000;
    timer.it_interval.tv_sec = 0;
    timer.it_interval.tv_usec = MRB_TICK_UNIT * 1000;
    setitimer(ITIMER_REAL, &timer, NULL);
  }

  /* Unblock SIGALRM */
  sigprocmask(SIG_UNBLOCK, &alarm_mask, NULL);
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

void
mrb_task_hal_final(mrb_state *mrb)
{
  struct itimerval timer;
  int i, j;

  /* Block SIGALRM during unregistration */
  sigprocmask(SIG_BLOCK, &alarm_mask, NULL);

  /* Find and remove this VM from the list */
  for (i = 0; i < vm_count; i++) {
    if (vm_list[i] == mrb) {
      /* Shift remaining VMs down */
      for (j = i; j < vm_count - 1; j++) {
        vm_list[j] = vm_list[j + 1];
      }
      vm_list[vm_count - 1] = NULL;
      vm_count--;
      break;
    }
  }

  /* Stop timer if last VM */
  if (vm_count == 0) {
    timer.it_value.tv_sec = 0;
    timer.it_value.tv_usec = 0;
    timer.it_interval.tv_sec = 0;
    timer.it_interval.tv_usec = 0;
    setitimer(ITIMER_REAL, &timer, NULL);
  }

  /* Unblock SIGALRM */
  sigprocmask(SIG_UNBLOCK, &alarm_mask, NULL);
}

/*
 * Gem initialization (empty - HAL functions called by mruby-task)
 */

void
mrb_hal_posix_task_gem_init(mrb_state *mrb)
{
  (void)mrb;
  /* HAL interface functions are called by mruby-task gem */
}

void
mrb_hal_posix_task_gem_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup handled by mrb_task_hal_final called from mruby-task */
}
