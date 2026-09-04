/*
** process_hal.h - Process Hardware Abstraction Layer (HAL)
**
** See Copyright Notice in mruby.h
**
** This header defines the HAL interface for platform-specific process
** operations.  A port under mruby-process/ports/<port_name>/, or an
** external provider gem named hal-process-<conf>, supplies every function
** declared here.
**
** The HAL answers OS-level facts and performs OS-level operations.  It knows
** nothing about `Process::Status`, `Process::Tms`, `$?`, `$$`, blocks or any
** other Ruby notion: those belong to the common sources under src/.  In the
** other direction, no platform type or macro (`pid_t`, `WIFEXITED`,
** `SIGTERM`, `WNOHANG`, `CLOCK_MONOTONIC`, `clock_t`, ...) crosses into the
** common layer: process and signal numbers travel as `mrb_int`, wait options
** as the MRB_PROCESS_WAIT_* bits below, a decoded wait status as
** `mrb_process_status`, a clock as one of the `mrb_process_clock_id` values,
** a time it reported as `mrb_process_clock_time`, whose two fields are
** `int64_t` rather than `mrb_int` for the reason given where it is defined,
** and the four CPU time totals behind `Process.times` as `mrb_process_times`,
** four more `mrb_process_clock_time` readings rather than platform ticks.
**
** What a signal is *called* is not asked here at all.  mruby-signal owns
** that table, and both `Process.kill` and `Process::Status#to_s` reach it
** through signal_hal.h.
*/

#ifndef MRUBY_PROCESS_HAL_H
#define MRUBY_PROCESS_HAL_H

#include <mruby.h>
#include <stdint.h>

/*
 * What the port implements
 *
 * The port publishes it in a header under its include/, and the build puts
 * that directory on the include path of this gem and of every gem that
 * depends on it.  Whether a method exists is the port's to say, because the
 * port is what a build names: a cross build has no host to detect one from,
 * and a `hal-process-<conf>` gem may stand in for the bundled ports
 * altogether.  What each macro says, and what it guards, is written where it
 * is defined.
 */
#include "process_hal_features.h"

MRB_BEGIN_DECL

/*
 * Decoded wait status
 */

/* What a decoded status says about how the process left the CPU.  A port sets
   the flags it can tell apart; a platform that only reports an exit code sets
   MRB_PROCESS_STATUS_EXITED alone. */
typedef enum mrb_process_status_flags {
  MRB_PROCESS_STATUS_EXITED    = 1 << 0,  /* ran to completion; exitstatus is set */
  MRB_PROCESS_STATUS_SIGNALED  = 1 << 1,  /* killed by a signal; termsig is set */
  MRB_PROCESS_STATUS_STOPPED   = 1 << 2,  /* stopped, not reaped; stopsig is set */
  MRB_PROCESS_STATUS_COREDUMP  = 1 << 3,  /* a core dump accompanied the signal */
} mrb_process_status_flags;

/* A wait status in platform-neutral form.  Fields not selected by `flags`
   hold 0.  `raw_status` is the platform value the status was decoded from,
   kept so that `Process::Status#to_i` can hand it back unchanged. */
typedef struct mrb_process_status {
  mrb_int pid;
  mrb_int raw_status;
  mrb_int exitstatus;
  mrb_int termsig;
  mrb_int stopsig;
  unsigned int flags;
} mrb_process_status;

/*
 * Wait options
 *
 * These are the values `Process::WNOHANG` and `Process::WUNTRACED` carry, so
 * they are mruby's own; a port translates them to whatever its platform
 * spells them as.
 */
#define MRB_PROCESS_WAIT_NOHANG   (1u << 0)  /* return at once if nothing is ready */
#define MRB_PROCESS_WAIT_UNTRACED (1u << 1)  /* report stopped children too */

/* Every bit a wait may carry.  The common layer refuses anything else, so a
   port is handed only bits it knows and does not have to say what it would
   do with the others. */
#define MRB_PROCESS_WAIT_FLAGS (MRB_PROCESS_WAIT_NOHANG | MRB_PROCESS_WAIT_UNTRACED)

/* Wait for any child, whichever finishes first.  Other non-positive pids keep
   their platform meaning (on POSIX, a process group selector); a port that
   cannot express them fails with ENOSYS. */
#define MRB_PROCESS_WAIT_ANY ((mrb_int)-1)

/*
 * Clocks
 *
 * Which clocks there are is mruby's own list rather than the platform's, as
 * the wait options above are: `CLOCK_MONOTONIC` is 1 on Linux, 6 on macOS
 * and 4 on FreeBSD, and Windows has no `clockid_t` to give it a number at
 * all, so a program that names a clock would otherwise be naming a different
 * one on each port.  The common layer refuses an id outside this list before
 * a port sees it, and a port whose platform has no such clock fails that
 * one with EINVAL.
 */
typedef enum mrb_process_clock_id {
  MRB_PROCESS_CLOCK_REALTIME = 0,     /* wall clock, counted from the epoch */
  MRB_PROCESS_CLOCK_MONOTONIC,        /* never steps; unspecified origin */
  MRB_PROCESS_CLOCK_PROCESS_CPUTIME,  /* CPU time this process has spent */
  MRB_PROCESS_CLOCK_THREAD_CPUTIME,   /* CPU time this thread has spent */
  MRB_PROCESS_CLOCK_COUNT             /* how many ids there are; not a clock */
} mrb_process_clock_id;

/*
 * A time a clock reported, kept in two fields so that nothing is lost on the
 * way up.
 *
 * A Float would hold 53 of the 61 bits a wall-clock nanosecond needs, which
 * would leave `:nanosecond` unable to answer honestly however it was asked,
 * and a single count of nanoseconds runs out in 2262.  A port therefore
 * always reports the same two numbers and knows nothing of the unit a caller
 * wanted: arriving at that unit is the common layer's.
 *
 * The fields are `int64_t` rather than `mrb_int`, which is what every other
 * quantity crossing this interface travels as.  A reading is a fact about
 * the platform and its size is the platform's, so a port reports what its
 * clock said; how much of that this build's Integer can carry is a question
 * about mruby, and is answered where RangeError can be said and a bigint
 * can be built.  Were it `mrb_int`, a build with a 32-bit one would have
 * every port refusing the wall clock from 2038 on, through `errno`, which
 * has no way to say that the platform was fine and the Integer was not.
 * `int64_t` is no more a platform type than `mrb_int` is: `time_t`,
 * `clockid_t` and FILETIME still stop at the port.
 *
 * `nsec` is always in [0, 999999999], whatever the platform counts in.
 */
typedef struct mrb_process_clock_time {
  int64_t sec;
  int64_t nsec;
} mrb_process_clock_time;

/* Nanoseconds in one second, the scale `nsec` above is a count in.  Darwin's
   <mach/clock_types.h> spells the same name (as 1000000000ull), so a
   definition already in scope is left standing. */
#ifndef NSEC_PER_SEC
#define NSEC_PER_SEC 1000000000LL
#endif

/*
 * CPU time totals
 *
 * What Process.times reports: how much CPU time this process, and the
 * children it has already reaped, have spent in user and kernel mode.  Each
 * of the four travels the way a clock reading does, as an
 * mrb_process_clock_time, for the reasons given above it, plus one of its
 * own: mrb_float would not compile under MRB_NO_FLOAT.  Turning the four
 * into a Ruby Float, and building the Process::Tms they are answered as, is
 * Process.times's job once every port has answered the same shape; a port
 * is asked for nothing beyond the four readings themselves.
 */
typedef struct mrb_process_times {
  mrb_process_clock_time utime;   /* user CPU time this process has used */
  mrb_process_clock_time stime;   /* system CPU time this process has used */
  mrb_process_clock_time cutime;  /* user CPU time of reaped children */
  mrb_process_clock_time cstime;  /* system CPU time of reaped children */
} mrb_process_times;

/*
 * HAL Interface Functions
 */

/* Process ID of the calling process.  Returns -1 with errno set on failure. */
mrb_int mrb_hal_process_pid(mrb_state *mrb);

/* Process ID of the parent.  Returns -1 with errno set where the platform
   cannot name a parent (errno ENOSYS when it has no such notion at all). */
mrb_int mrb_hal_process_ppid(mrb_state *mrb);

#ifdef MRB_HAL_PROCESS_HAS_WAIT
/*
 * Wait for a child process to change state.
 *
 * Declared in the port's process_hal_features.h.  A port without it has no
 * children to wait for, and the gem defines no wait rather than one that
 * fails; the status decoder below is asked for all the same, since a status
 * can arrive from elsewhere, mruby-io's `IO.popen` being one source.
 *
 * @param pid         child to wait for, or MRB_PROCESS_WAIT_ANY
 * @param flags       zero or more MRB_PROCESS_WAIT_* bits
 * @param result_pid  out: the child that changed state, or 0 when
 *                    MRB_PROCESS_WAIT_NOHANG found none ready
 * @param raw_status  out: the platform status, to be read back through
 *                    mrb_hal_process_status_decode()
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_process_waitpid(mrb_state *mrb, mrb_int pid, unsigned int flags,
                            mrb_int *result_pid, mrb_int *raw_status);
#endif

/*
 * Send a signal to a process.
 *
 * Signal 0 sends nothing and only reports whether the process may be
 * signalled, as POSIX `kill(2)` does.
 *
 * @return 0 on success, -1 on error (sets errno; ENOSYS where the platform
 *         cannot deliver this signal at all)
 */
int mrb_hal_process_kill(mrb_state *mrb, mrb_int pid, mrb_int signo);

/*
 * Read a platform wait status into its neutral form.
 *
 * Every field of `status` is written, including `pid` and `raw_status`, which
 * are copied from the arguments.  Decoding cannot fail: a status the platform
 * does not recognize decodes to flags of 0.
 */
void mrb_hal_process_status_decode(mrb_state *mrb, mrb_int pid, mrb_int raw_status,
                                   mrb_process_status *status);

/*
 * Read a clock.
 *
 * MRB_PROCESS_CLOCK_REALTIME is counted from the Unix epoch and may step,
 * backwards included, when the host's idea of the time is corrected.  The
 * other three are counted from an origin this interface does not name; a
 * port must keep whichever it uses fixed for the life of the process, since
 * subtracting two readings is the whole of what such a clock is for.
 *
 * @param clock_id  one of the mrb_process_clock_id values
 * @param t         out: the reading, with nsec normalized to [0, 999999999]
 * @return 0 on success, -1 on error, with errno set.  A clock the platform
 *         does not have is EINVAL; a port that could read no clock at all
 *         would answer ENOSYS, as an inexpressible wait pid does.
 */
int mrb_hal_process_clock_gettime(mrb_state *mrb, mrb_int clock_id,
                                  mrb_process_clock_time *t);

/*
 * The granularity a clock is read at: how finely the mechanism a reading
 * comes out of can tell two moments apart.  It describes the way the port
 * reads the clock, not the clock itself: the interval a reading is driven
 * by where the platform states one, and otherwise the unit a reading is
 * written in, which is the finest two of them can differ by.  A caller gets
 * a bound on what it can distinguish, never a period the clock is promised
 * to advance on, and a port must not answer anything finer than the
 * mechanism it used, since that promises a difference no two readings can
 * show.
 *
 * A reading says little without it, since a monotonic clock that moves every
 * 15ms and one that moves every 100ns are read the same way, so a clock a
 * port can read is one it can answer this for: whatever a reading came out
 * of has a granularity, even where the clock behind it does not state one.
 *
 * The looseness is the platforms', not this interface's.  POSIX's own
 * clock_getres(2) is a statement of the same kind, Linux answering 1ns for
 * clocks whose readings move in tens or hundreds of them, and CRuby reports
 * the granularity of what it emulated a clock out of: a microsecond for the
 * gettimeofday(2)-based wall clock, a tick for the times(2)-based CPU one.
 * A port that refused to answer wherever a platform would not commit to a
 * true period would refuse for nearly every clock on every platform.
 *
 * @param clock_id  one of the mrb_process_clock_id values
 * @param t         out: the granularity, never zero (the common layer
 *                  divides by it to answer `:hertz`), nsec in [0, 999999999]
 * @return 0 on success, -1 on error, as for a reading
 */
int mrb_hal_process_clock_getres(mrb_state *mrb, mrb_int clock_id,
                                 mrb_process_clock_time *t);

/*
 * Read the CPU time totals above.
 *
 * cutime and cstime cover only waited-for terminated children, whichever
 * wait(2)/waitpid(2) call reaped them: Process.wait, Process.waitpid, or one
 * this gem did not itself make (mruby-io's own reap on IO.popen(...).close,
 * say). A child still running, or one never waited for, is not in them,
 * which is what POSIX's times(2) reports too. A platform with no notion of
 * a child's CPU time answers 0 for both.
 *
 * @param t  out: the four readings
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_process_times(mrb_state *mrb, mrb_process_times *t);

/*
 * HAL Initialization/Finalization
 */

/* Initialize HAL (called once at gem initialization) */
void mrb_hal_process_init(mrb_state *mrb);

/* Cleanup HAL (called once at gem finalization) */
void mrb_hal_process_final(mrb_state *mrb);

MRB_END_DECL

#endif /* MRUBY_PROCESS_HAL_H */
