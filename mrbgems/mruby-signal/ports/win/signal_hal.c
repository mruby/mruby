/*
** signal_hal.c - Windows HAL implementation for mruby-signal
**
** See Copyright Notice in mruby.h
**
** Windows has no signals between processes, so what a name stands for here
** is a number and nothing more: the table says which names resolve, and
** whether any of them can be delivered is `Process.kill`'s question, not
** this one's.
*/

#include <mruby.h>
#include "signal_hal.h"

#include <string.h>

/*
 * Signal table
 *
 * The numbers Windows' <signal.h> uses, plus KILL at its conventional POSIX
 * value so that `Process.kill(:KILL, pid)` names something here.
 *
 * `EXIT` is not among them, for the same reason as on POSIX: it is Ruby's
 * own name for signal 0 rather than one the platform has, and the common
 * layer adds it.
 */

struct signal_entry {
  const char *name;
  int signo;
};

static const struct signal_entry signal_table[] = {
  { "INT",   2 },
  { "ILL",   4 },
  { "FPE",   8 },
  { "KILL",  9 },
  { "SEGV",  11 },
  { "TERM",  15 },
  { "BREAK", 21 },
  { "ABRT",  22 },
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
