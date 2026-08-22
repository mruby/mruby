/*
** process_hal.c - POSIX HAL implementation for mruby-process
**
** See Copyright Notice in mruby.h
**
** POSIX implementation of the process HAL using getpid(2), getppid(2),
** waitpid(2) and kill(2).
** Supported platforms: Linux, macOS, BSD, Unix
*/

#include <mruby.h>
#include "process_hal.h"

#include <sys/types.h>
#include <sys/wait.h>

#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>

/* An mrb_int is wider than a pid_t where mrb_int is 64-bit, so a pid from
   Ruby is range-checked rather than truncated into one. */
#define PID_FITS(pid) ((pid) >= (mrb_int)INT_MIN && (pid) <= (mrb_int)INT_MAX)

/*
 * Signal table
 *
 * The names Ruby knows a signal by, in the order Ruby lists them, each one
 * behind the guard that says whether this host has it.  Taking the list from
 * Ruby rather than picking one keeps a name the host defines from being
 * reported as unsupported merely because it was left out here.
 *
 * `EXIT` is not among them: it names signal 0 only where a handler is being
 * set, which this gem does not do, and `Process.kill` refuses it in Ruby too.
 *
 * The order carries a second meaning.  Where a host gives one signal two
 * names, the alias follows the name Ruby answers with, so the reverse lookup
 * that `Process::Status#to_s` goes through finds `ABRT` before `IOT`, `CHLD`
 * before `CLD` and `IO` before `POLL`.
 */

struct signal_entry {
  const char *name;
  int signo;
};

static const struct signal_entry signal_table[] = {
#define SIGNAL_ENTRY(name) { #name, SIG##name },
#ifdef SIGHUP
  SIGNAL_ENTRY(HUP)
#endif
#ifdef SIGINT
  SIGNAL_ENTRY(INT)
#endif
#ifdef SIGQUIT
  SIGNAL_ENTRY(QUIT)
#endif
#ifdef SIGILL
  SIGNAL_ENTRY(ILL)
#endif
#ifdef SIGTRAP
  SIGNAL_ENTRY(TRAP)
#endif
#ifdef SIGABRT
  SIGNAL_ENTRY(ABRT)
#endif
#ifdef SIGIOT
  SIGNAL_ENTRY(IOT)
#endif
#ifdef SIGEMT
  SIGNAL_ENTRY(EMT)
#endif
#ifdef SIGFPE
  SIGNAL_ENTRY(FPE)
#endif
#ifdef SIGKILL
  SIGNAL_ENTRY(KILL)
#endif
#ifdef SIGBUS
  SIGNAL_ENTRY(BUS)
#endif
#ifdef SIGSEGV
  SIGNAL_ENTRY(SEGV)
#endif
#ifdef SIGSYS
  SIGNAL_ENTRY(SYS)
#endif
#ifdef SIGPIPE
  SIGNAL_ENTRY(PIPE)
#endif
#ifdef SIGALRM
  SIGNAL_ENTRY(ALRM)
#endif
#ifdef SIGTERM
  SIGNAL_ENTRY(TERM)
#endif
#ifdef SIGURG
  SIGNAL_ENTRY(URG)
#endif
#ifdef SIGSTOP
  SIGNAL_ENTRY(STOP)
#endif
#ifdef SIGTSTP
  SIGNAL_ENTRY(TSTP)
#endif
#ifdef SIGCONT
  SIGNAL_ENTRY(CONT)
#endif
#ifdef SIGCHLD
  SIGNAL_ENTRY(CHLD)
#endif
#ifdef SIGCLD
  SIGNAL_ENTRY(CLD)
#endif
#ifdef SIGTTIN
  SIGNAL_ENTRY(TTIN)
#endif
#ifdef SIGTTOU
  SIGNAL_ENTRY(TTOU)
#endif
#ifdef SIGIO
  SIGNAL_ENTRY(IO)
#endif
#ifdef SIGXCPU
  SIGNAL_ENTRY(XCPU)
#endif
#ifdef SIGXFSZ
  SIGNAL_ENTRY(XFSZ)
#endif
#ifdef SIGVTALRM
  SIGNAL_ENTRY(VTALRM)
#endif
#ifdef SIGPROF
  SIGNAL_ENTRY(PROF)
#endif
#ifdef SIGWINCH
  SIGNAL_ENTRY(WINCH)
#endif
#ifdef SIGUSR1
  SIGNAL_ENTRY(USR1)
#endif
#ifdef SIGUSR2
  SIGNAL_ENTRY(USR2)
#endif
#ifdef SIGLOST
  SIGNAL_ENTRY(LOST)
#endif
#ifdef SIGMSG
  SIGNAL_ENTRY(MSG)
#endif
#ifdef SIGPWR
  SIGNAL_ENTRY(PWR)
#endif
#ifdef SIGPOLL
  SIGNAL_ENTRY(POLL)
#endif
#ifdef SIGDANGER
  SIGNAL_ENTRY(DANGER)
#endif
#ifdef SIGMIGRATE
  SIGNAL_ENTRY(MIGRATE)
#endif
#ifdef SIGPRE
  SIGNAL_ENTRY(PRE)
#endif
#ifdef SIGGRANT
  SIGNAL_ENTRY(GRANT)
#endif
#ifdef SIGRETRACT
  SIGNAL_ENTRY(RETRACT)
#endif
#ifdef SIGSOUND
  SIGNAL_ENTRY(SOUND)
#endif
#ifdef SIGINFO
  SIGNAL_ENTRY(INFO)
#endif
#undef SIGNAL_ENTRY
};

#define SIGNAL_TABLE_LEN (sizeof(signal_table) / sizeof(signal_table[0]))

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

int
mrb_hal_process_signal_number(mrb_state *mrb, const char *name, mrb_int *signo)
{
  size_t i;
  (void)mrb;

  for (i = 0; i < SIGNAL_TABLE_LEN; i++) {
    if (strcmp(signal_table[i].name, name) == 0) {
      *signo = (mrb_int)signal_table[i].signo;
      return 0;
    }
  }
  return -1;
}

const char*
mrb_hal_process_signal_name(mrb_state *mrb, mrb_int signo)
{
  size_t i;
  (void)mrb;

  for (i = 0; i < SIGNAL_TABLE_LEN; i++) {
    if ((mrb_int)signal_table[i].signo == signo) {
      return signal_table[i].name;
    }
  }
  return NULL;
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
#ifdef WCOREDUMP
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
