/*
** process_internal.h - shared declarations within mruby-process
**
** See Copyright Notice in mruby.h
**
** Not part of the HAL and not for other gems: these are the handful of
** things process.c, clock.c and status.c say to each other.
*/

#ifndef MRUBY_PROCESS_INTERNAL_H
#define MRUBY_PROCESS_INTERNAL_H

#include <mruby.h>
#include "process_hal.h"

MRB_BEGIN_DECL

/* Refuse `v` with a RangeError naming `what` unless it fits the `int` a port
   has to narrow it to.  A no-op where mruby's own Integer is no wider. */
mrb_int mrb_process_int_arg(mrb_state *mrb, mrb_int v, const char *what);

/* Define Process::Status under `process`.  Called once from gem init. */
void mrb_process_status_init(mrb_state *mrb, struct RClass *process);

/* Define the CLOCK_* constants, Process.clock_gettime, Process.clock_getres,
   Process.times and Process::Tms on `process`.  Called once from gem init. */
void mrb_process_clock_init(mrb_state *mrb, struct RClass *process);

/* Build a Process::Status for a pid and the platform status it was reaped
   with.  The status decodes itself through the HAL as it is asked questions. */
mrb_value mrb_process_status_new(mrb_state *mrb, mrb_int pid, mrb_int raw_status);

/* Answer the clock reading `t` in the unit `unit` names.  `resolution` says
   whether `t` is a resolution rather than a moment, which is what makes
   `:hertz` a unit it can be asked for.  Shared with the gem's tests, which
   hand it readings no clock reports: the ends of an int64_t and of this
   build's Integer are decided here, so that is where they can be asked
   about. */
mrb_value mrb_process_clock_result(mrb_state *mrb, mrb_value unit,
                                   const mrb_process_clock_time *t,
                                   mrb_bool resolution);

MRB_END_DECL

#endif /* MRUBY_PROCESS_INTERNAL_H */
