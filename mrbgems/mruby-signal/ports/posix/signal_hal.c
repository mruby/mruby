/*
** signal_hal.c - POSIX HAL implementation for mruby-signal
**
** See Copyright Notice in mruby.h
**
** POSIX implementation of the signal HAL: the table <signal.h> defines on
** this host, read in both directions.
** Supported platforms: Linux, macOS, BSD, Unix
*/

#include <mruby.h>
#include "signal_hal.h"

#include <signal.h>
#include <string.h>

/*
 * Signal table
 *
 * The names Ruby knows a signal by, in the order Ruby lists them, each one
 * behind the guard that says whether this host has it.  Taking the list from
 * Ruby rather than picking one keeps a name the host defines from being
 * reported as unsupported merely because it was left out here.
 *
 * `EXIT` is not among them: it is Ruby's own name for signal 0 rather than a
 * signal a host has, so the common layer adds it and every port gets it
 * alike.
 *
 * The order carries a second meaning.  Where a host gives one signal two
 * names, the alias follows the name Ruby answers with, so the reverse lookup
 * `Signal.signame` goes through finds `ABRT` before `IOT`, `CHLD` before
 * `CLD` and `IO` before `POLL`.
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

int
mrb_hal_signal_number(mrb_state *mrb, const char *name, mrb_int *signo)
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
mrb_hal_signal_name(mrb_state *mrb, mrb_int signo)
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

mrb_int
mrb_hal_signal_count(mrb_state *mrb)
{
  (void)mrb;
  return (mrb_int)SIGNAL_TABLE_LEN;
}

const char*
mrb_hal_signal_at(mrb_state *mrb, mrb_int index, mrb_int *signo)
{
  (void)mrb;

  if (index < 0 || index >= (mrb_int)SIGNAL_TABLE_LEN) return NULL;
  *signo = (mrb_int)signal_table[index].signo;
  return signal_table[index].name;
}
