/*
** process_hal.c - POSIX HAL implementation for mruby-process
**
** See Copyright Notice in mruby.h
**
** POSIX implementation of the process HAL using getpid(2), getppid(2),
** waitpid(2) and kill(2).  The clocks are clock_hal.c's.
** Supported platforms: Linux, macOS, BSD, Unix
*/

#include <mruby.h>
#include "process_hal.h"

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <unistd.h>

/*
 * Feature Capabilities
 *
 * Each MRB_PROCESS_HAVE_* is always defined, to 0 or 1, so the rest of this
 * file tests it with #if rather than #ifdef; the #ifndef guard around each
 * one lets a build override the detection below where it gets a host wrong.
 */

/* Whether WIFSIGNALED's status can also say the process dumped core; not
   every host's <sys/wait.h> defines this. */
#ifndef MRB_PROCESS_HAVE_WCOREDUMP
# ifdef WCOREDUMP
#  define MRB_PROCESS_HAVE_WCOREDUMP 1
# else
#  define MRB_PROCESS_HAVE_WCOREDUMP 0
# endif
#endif

/* An mrb_int is wider than a pid_t where mrb_int is 64-bit, so a pid from
   Ruby is range-checked rather than truncated into one. */
#define PID_FITS(pid) ((pid) >= (mrb_int)INT_MIN && (pid) <= (mrb_int)INT_MAX)

/*
 * Process Identity
 */

mrb_int
mrb_hal_process_pid(mrb_state *mrb)
{
  (void)mrb;
  return (mrb_int)getpid();
}

mrb_int
mrb_hal_process_ppid(mrb_state *mrb)
{
  (void)mrb;
  return (mrb_int)getppid();
}

/*
 * Waiting
 */

#ifdef MRB_HAL_PROCESS_HAS_WAIT
int
mrb_hal_process_waitpid(mrb_state *mrb, mrb_int pid, unsigned int flags,
                        mrb_int *result_pid, mrb_int *raw_status)
{
  pid_t result;
  int status = 0;
  int options = 0;
  (void)mrb;

  if (!PID_FITS(pid)) {
    errno = ECHILD;
    return -1;
  }
  if (flags & MRB_PROCESS_WAIT_NOHANG) options |= WNOHANG;
  if (flags & MRB_PROCESS_WAIT_UNTRACED) options |= WUNTRACED;

  do {
    result = waitpid((pid_t)pid, &status, options);
  } while (result == -1 && errno == EINTR);

  if (result == -1) return -1;

  /* result is 0 when WNOHANG found nothing ready; status is untouched then */
  *result_pid = (mrb_int)result;
  *raw_status = (result == 0) ? 0 : (mrb_int)status;
  return 0;
}
#endif

/*
 * Signalling
 */

int
mrb_hal_process_kill(mrb_state *mrb, mrb_int pid, mrb_int signo)
{
  (void)mrb;

  /* Which numbers name a signal is kill(2)'s to say, and it answers EINVAL
     for the ones this host does not have, so the range asked for here is only
     the one an int can carry. */
  if (signo < 0 || signo > (mrb_int)INT_MAX) {
    errno = EINVAL;
    return -1;
  }
  if (!PID_FITS(pid)) {
    errno = ESRCH;
    return -1;
  }
  return kill((pid_t)pid, (int)signo);
}

/*
 * Status Decoding
 */

void
mrb_hal_process_status_decode(mrb_state *mrb, mrb_int pid, mrb_int raw_status,
                              mrb_process_status *status)
{
  int raw = (int)raw_status;
  (void)mrb;

  status->pid = pid;
  status->raw_status = raw_status;
  status->exitstatus = 0;
  status->termsig = 0;
  status->stopsig = 0;
  status->flags = 0;

  /* WIFSTOPPED comes first: a stopped status can also satisfy WIFSIGNALED on
     some platforms, and stopping is the more specific answer. */
  if (WIFSTOPPED(raw)) {
    status->flags |= MRB_PROCESS_STATUS_STOPPED;
    status->stopsig = (mrb_int)WSTOPSIG(raw);
  }
  else if (WIFEXITED(raw)) {
    status->flags |= MRB_PROCESS_STATUS_EXITED;
    status->exitstatus = (mrb_int)WEXITSTATUS(raw);
  }
  else if (WIFSIGNALED(raw)) {
    status->flags |= MRB_PROCESS_STATUS_SIGNALED;
    status->termsig = (mrb_int)WTERMSIG(raw);
#if MRB_PROCESS_HAVE_WCOREDUMP
    if (WCOREDUMP(raw)) {
      status->flags |= MRB_PROCESS_STATUS_COREDUMP;
    }
#endif
  }
}

/*
 * HAL Initialization/Finalization
 */

void
mrb_hal_process_init(mrb_state *mrb)
{
  (void)mrb;
}

void
mrb_hal_process_final(mrb_state *mrb)
{
  (void)mrb;
}
