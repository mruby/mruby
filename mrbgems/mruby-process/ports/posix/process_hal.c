/*
** process_hal.c - POSIX HAL implementation for mruby-process
**
** See Copyright Notice in mruby.h
**
** POSIX implementation of the process HAL using getpid(2), getppid(2),
** waitpid(2), kill(2), clock_gettime(2) and clock_getres(2), falling back to
** gettimeofday(2) where the host has no POSIX clocks, and getrusage(2) for
** the CPU time totals behind Process.times, falling back to times(2) scaled
** by sysconf(_SC_CLK_TCK) where the host has no getrusage(2).
** Supported platforms: Linux, macOS, BSD, Unix
*/

#include <mruby.h>
#include "process_hal.h"

#include <sys/time.h>
#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <stdint.h>
#include <time.h>
#include <unistd.h>

/*
 * Feature Capabilities
 *
 * Each MRB_PROCESS_HAVE_* is always defined, to 0 or 1, so the rest of this
 * file tests it with #if rather than #ifdef; the #ifndef guard around each
 * one lets a build override the detection below where it gets a host wrong.
 */

/* Whether this host has POSIX clocks.  _POSIX_TIMERS is the feature-test
   macro for POSIX's interval-timer option (timer_create(2) and friends),
   not for clock_gettime(2) itself, and Apple's libc leaves it undefined
   because it has never implemented that option, even though clock_gettime(2)
   and CLOCK_REALTIME/CLOCK_MONOTONIC have been there since macOS 10.12.
   Relying on _POSIX_TIMERS alone therefore misses a host that plainly has
   the call, so Apple is also asked for directly. Where the answer is no,
   the wall clock is still reachable through gettimeofday(2) and nothing
   else is. */
#ifndef MRB_PROCESS_HAVE_CLOCK_GETTIME
# if (defined(_POSIX_TIMERS) && (_POSIX_TIMERS + 0) > 0 && defined(CLOCK_REALTIME)) || \
     (defined(__APPLE__) && defined(CLOCK_REALTIME))
#  define MRB_PROCESS_HAVE_CLOCK_GETTIME 1
# else
#  define MRB_PROCESS_HAVE_CLOCK_GETTIME 0
# endif
#endif

/* CLOCK_MONOTONIC is absent on a few old hosts that still have
   CLOCK_REALTIME. */
#ifndef MRB_PROCESS_HAVE_CLOCK_MONOTONIC
# ifdef CLOCK_MONOTONIC
#  define MRB_PROCESS_HAVE_CLOCK_MONOTONIC 1
# else
#  define MRB_PROCESS_HAVE_CLOCK_MONOTONIC 0
# endif
#endif

/* The CPU-time clocks are both optional POSIX extensions. */
#ifndef MRB_PROCESS_HAVE_CLOCK_PROCESS_CPUTIME
# ifdef CLOCK_PROCESS_CPUTIME_ID
#  define MRB_PROCESS_HAVE_CLOCK_PROCESS_CPUTIME 1
# else
#  define MRB_PROCESS_HAVE_CLOCK_PROCESS_CPUTIME 0
# endif
#endif

#ifndef MRB_PROCESS_HAVE_CLOCK_THREAD_CPUTIME
# ifdef CLOCK_THREAD_CPUTIME_ID
#  define MRB_PROCESS_HAVE_CLOCK_THREAD_CPUTIME 1
# else
#  define MRB_PROCESS_HAVE_CLOCK_THREAD_CPUTIME 0
# endif
#endif

/* Whether WIFSIGNALED's status can also say the process dumped core; not
   every host's <sys/wait.h> defines this. */
#ifndef MRB_PROCESS_HAVE_WCOREDUMP
# ifdef WCOREDUMP
#  define MRB_PROCESS_HAVE_WCOREDUMP 1
# else
#  define MRB_PROCESS_HAVE_WCOREDUMP 0
# endif
#endif

/* Whether this host has getrusage(2), which is how Process.times reads CPU
   time here: RUSAGE_SELF and RUSAGE_CHILDREN are both XSI extensions
   (SUSv2), present on Linux, macOS and the BSDs but not guaranteed by base
   POSIX.1, so a host missing either falls back to times(2).

   <sys/resource.h> is part of that same XSI extension; whether it is there
   is asked of the compiler by the gem's mrbgem.rake, for the reason given
   there, and answered as HAVE_SYS_RESOURCE_H.
   Overriding MRB_PROCESS_HAVE_GETRUSAGE to 1 asserts the call is there,
   which it can only be where the header is, so HAVE_SYS_RESOURCE_H goes
   with such an override; overriding it to 0 stands on its own. */
#ifdef HAVE_SYS_RESOURCE_H
# include <sys/resource.h>
#endif
#ifndef MRB_PROCESS_HAVE_GETRUSAGE
# if defined(RUSAGE_SELF) && defined(RUSAGE_CHILDREN)
#  define MRB_PROCESS_HAVE_GETRUSAGE 1
# else
#  define MRB_PROCESS_HAVE_GETRUSAGE 0
# endif
#endif

/* times(2) is only reached where getrusage(2) is not, so <sys/times.h> is
   only asked for there: a host that has the XSI call needs nothing from
   this header, and is not made to have it. */
#if !MRB_PROCESS_HAVE_GETRUSAGE
# include <sys/times.h>
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
 * Clocks
 */

#if MRB_PROCESS_HAVE_CLOCK_GETTIME
/* Which clockid_t stands for one of mruby's clocks here.  A clock this host
   does not have is left out, and the caller answers EINVAL for it. */
static int
posix_clock_id(mrb_int clock_id, clockid_t *out)
{
  switch (clock_id) {
  case MRB_PROCESS_CLOCK_REALTIME:
    *out = CLOCK_REALTIME;
    return 0;
#if MRB_PROCESS_HAVE_CLOCK_MONOTONIC
  case MRB_PROCESS_CLOCK_MONOTONIC:
    *out = CLOCK_MONOTONIC;
    return 0;
#endif
#if MRB_PROCESS_HAVE_CLOCK_PROCESS_CPUTIME
  case MRB_PROCESS_CLOCK_PROCESS_CPUTIME:
    *out = CLOCK_PROCESS_CPUTIME_ID;
    return 0;
#endif
#if MRB_PROCESS_HAVE_CLOCK_THREAD_CPUTIME
  case MRB_PROCESS_CLOCK_THREAD_CPUTIME:
    *out = CLOCK_THREAD_CPUTIME_ID;
    return 0;
#endif
  default:
    break;
  }
  return -1;
}
#endif /* MRB_PROCESS_HAVE_CLOCK_GETTIME */

int
mrb_hal_process_clock_gettime(mrb_state *mrb, mrb_int clock_id,
                              mrb_process_clock_time *t)
{
#if MRB_PROCESS_HAVE_CLOCK_GETTIME
  clockid_t c;
  struct timespec ts;
  (void)mrb;

  if (posix_clock_id(clock_id, &c) != 0) {
    errno = EINVAL;
    return -1;
  }
  if (clock_gettime(c, &ts) != 0) return -1;
  t->sec = (int64_t)ts.tv_sec;
  t->nsec = (int64_t)ts.tv_nsec;
  return 0;
#else
  /* Without POSIX clocks the wall clock is the only one there is, and
     gettimeofday(2) answers it in microsecond units. */
  struct timeval tv;
  (void)mrb;

  if (clock_id != MRB_PROCESS_CLOCK_REALTIME) {
    errno = EINVAL;
    return -1;
  }
  if (gettimeofday(&tv, NULL) != 0) return -1;
  t->sec = (int64_t)tv.tv_sec;
  t->nsec = (int64_t)tv.tv_usec * 1000;
  return 0;
#endif
}

int
mrb_hal_process_clock_getres(mrb_state *mrb, mrb_int clock_id,
                             mrb_process_clock_time *t)
{
#if MRB_PROCESS_HAVE_CLOCK_GETTIME
  clockid_t c;
  struct timespec ts;
  (void)mrb;

  if (posix_clock_id(clock_id, &c) != 0) {
    errno = EINVAL;
    return -1;
  }
  if (clock_getres(c, &ts) != 0) return -1;
  t->sec = (int64_t)ts.tv_sec;
  t->nsec = (int64_t)ts.tv_nsec;
  return 0;
#else
  /* A microsecond, which is what gettimeofday(2) writes its answer in and so
     how finely this port reads the wall clock where that is the only way to
     read it.  CRuby reports the same number for its own gettimeofday(2)-based
     clock. */
  (void)mrb;

  if (clock_id != MRB_PROCESS_CLOCK_REALTIME) {
    errno = EINVAL;
    return -1;
  }
  t->sec = 0;
  t->nsec = 1000;
  return 0;
#endif
}

/*
 * CPU Time Totals
 */

#if MRB_PROCESS_HAVE_GETRUSAGE

/* Convert a struct timeval, as getrusage(2) reports CPU time in, into an
   mrb_process_clock_time.  tv_usec is always in [0, 1000000), so unlike the
   FILETIME split on Windows there is nothing here to carry downwards. */
static void
timeval_to_clock_time(mrb_process_clock_time *t, const struct timeval *tv)
{
  t->sec = (int64_t)tv->tv_sec;
  t->nsec = (int64_t)tv->tv_usec * 1000;
}

int
mrb_hal_process_times(mrb_state *mrb, mrb_process_times *t)
{
  struct rusage self, children;
  (void)mrb;

  /* RUSAGE_SELF is this process, summed across every thread that has ever
     run in it; RUSAGE_CHILDREN is every child this process has reaped via
     wait(2)/waitpid(2), summed the same way, which is exactly the
     utime/stime and cutime/cstime split Process.times reports. Both report
     CPU time as a struct timeval with microsecond units, with no
     sysconf(_SC_CLK_TCK) scale factor to look up. */
  if (getrusage(RUSAGE_SELF, &self) != 0) return -1;
  if (getrusage(RUSAGE_CHILDREN, &children) != 0) return -1;

  timeval_to_clock_time(&t->utime,  &self.ru_utime);
  timeval_to_clock_time(&t->stime,  &self.ru_stime);
  timeval_to_clock_time(&t->cutime, &children.ru_utime);
  timeval_to_clock_time(&t->cstime, &children.ru_stime);
  return 0;
}

#else /* !MRB_PROCESS_HAVE_GETRUSAGE */

/* Split a count of times(2) ticks into seconds and nanoseconds. */
static void
ticks_to_clock_time(mrb_process_clock_time *t, clock_t ticks, long clk_tck)
{
  int64_t v = (int64_t)ticks;

  t->sec = v / (int64_t)clk_tck;
  t->nsec = (v % (int64_t)clk_tck) * NSEC_PER_SEC / (int64_t)clk_tck;
}

int
mrb_hal_process_times(mrb_state *mrb, mrb_process_times *t)
{
  struct tms tm;
  long clk_tck;
  (void)mrb;

  /* Where getrusage(2) is unavailable, times(2) is the POSIX.1 baseline:
     the same four totals, as clock_t ticks to be scaled by _SC_CLK_TCK.

     Its return value is elapsed real time, not an error indicator: it
     passes through (clock_t)-1 legitimately as that counter wraps, and
     POSIX leaves errno after a successful call unspecified, so a failure
     cannot be told from one portably. The tms fields are all this reading
     needs, so the return value is ignored, as CRuby's fallback ignores it. */
  (void)times(&tm);

  /* Ticks per second. POSIX guarantees _SC_CLK_TCK an answer; a
     non-positive one is a host declining to say. CLOCKS_PER_SEC is
     clock(3)'s unit, not always times(2)'s, so declining is reported
     rather than guessed past. */
  clk_tck = sysconf(_SC_CLK_TCK);
  if (clk_tck <= 0) {
    errno = EINVAL;
    return -1;
  }

  ticks_to_clock_time(&t->utime,  tm.tms_utime,  clk_tck);
  ticks_to_clock_time(&t->stime,  tm.tms_stime,  clk_tck);
  ticks_to_clock_time(&t->cutime, tm.tms_cutime, clk_tck);
  ticks_to_clock_time(&t->cstime, tm.tms_cstime, clk_tck);
  return 0;
}

#endif /* MRB_PROCESS_HAVE_GETRUSAGE */

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
