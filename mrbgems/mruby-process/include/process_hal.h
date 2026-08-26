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
** nothing about `Process::Status`, `$?`, `$$`, blocks or any other Ruby
** notion: those belong to the common sources under src/.  In the other
** direction, no platform type or macro (`pid_t`, `WIFEXITED`, `SIGTERM`,
** `WNOHANG`, ...) crosses into the common layer: process and signal
** numbers travel as `mrb_int`, wait options as the MRB_PROCESS_WAIT_* bits
** below, and a decoded wait status as `mrb_process_status`.
**
** What a signal is *called* is not asked here at all.  mruby-signal owns
** that table, and both `Process.kill` and `Process::Status#to_s` reach it
** through signal_hal.h.
*/

#ifndef MRUBY_PROCESS_HAL_H
#define MRUBY_PROCESS_HAL_H

#include <mruby.h>

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
 * HAL Interface Functions
 */

/* Process ID of the calling process.  Returns -1 with errno set on failure. */
mrb_int mrb_hal_process_pid(mrb_state *mrb);

/* Process ID of the parent.  Returns -1 with errno set where the platform
   cannot name a parent (errno ENOSYS when it has no such notion at all). */
mrb_int mrb_hal_process_ppid(mrb_state *mrb);

/*
 * Wait for a child process to change state.
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
 * HAL Initialization/Finalization
 */

/* Initialize HAL (called once at gem initialization) */
void mrb_hal_process_init(mrb_state *mrb);

/* Cleanup HAL (called once at gem finalization) */
void mrb_hal_process_final(mrb_state *mrb);

MRB_END_DECL

#endif /* MRUBY_PROCESS_HAL_H */
