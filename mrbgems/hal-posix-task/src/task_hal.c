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
#include <time.h>
#include <unistd.h>
#include <stdint.h>

/* Time conversion constants */
#define NSEC_PER_MSEC 1000000ULL
#define NSEC_PER_SEC  1000000000ULL
#define USEC_PER_MSEC 1000ULL

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
mrb_hal_task_init(mrb_state *mrb)
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
mrb_hal_task_idle_cpu(mrb_state *mrb)
{
  (void)mrb;
  /* On POSIX, just pause briefly */
  usleep(MRB_TICK_UNIT * 1000);
}

void
mrb_hal_task_sleep_us(mrb_state *mrb, mrb_int usec)
{
  struct timespec start, now, sleep_time;
  int ret;

  (void)mrb;

  /* Validate input to prevent overflow */
  if (usec < 0) {
    return;
  }

  ret = clock_gettime(CLOCK_MONOTONIC, &start);
  if (ret != 0) {
    /* Fallback to simple usleep if clock_gettime fails */
    usleep(usec);
    return;
  }

  uint64_t target_ns = (uint64_t)usec * USEC_PER_MSEC;

  /* Loop until enough real time has elapsed */
  while (1) {
    ret = clock_gettime(CLOCK_MONOTONIC, &now);
    if (ret != 0) {
      break;  /* Clock failure - exit loop */
    }

    uint64_t elapsed_ns = (uint64_t)(now.tv_sec - start.tv_sec) * NSEC_PER_SEC +
                          (uint64_t)(now.tv_nsec - start.tv_nsec);

    if (elapsed_ns >= target_ns) {
      break;
    }

    /* Sleep for remaining time, but at least 1ms to allow timer interrupts */
    uint64_t remaining_ns = target_ns - elapsed_ns;
    if (remaining_ns > NSEC_PER_MSEC) {
      sleep_time.tv_sec = remaining_ns / NSEC_PER_SEC;
      sleep_time.tv_nsec = remaining_ns % NSEC_PER_SEC;
    }
    else {
      sleep_time.tv_sec = 0;
      sleep_time.tv_nsec = NSEC_PER_MSEC;
    }

    nanosleep(&sleep_time, NULL);  /* Interrupted by signals - that's OK */
  }
}

void
mrb_hal_task_final(mrb_state *mrb)
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
  /* Cleanup handled by mrb_hal_task_final called from mruby-task */
}
