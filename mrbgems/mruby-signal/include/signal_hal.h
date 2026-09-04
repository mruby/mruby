/*
** signal_hal.h - Signal Hardware Abstraction Layer (HAL)
**
** See Copyright Notice in mruby.h
**
** This header defines the HAL interface for the platform's signal table.  A
** port under mruby-signal/ports/<port_name>/, or an external provider gem
** named hal-signal-<conf>, supplies every function declared here.
**
** The HAL answers one question in two directions: which name this platform
** gives a signal number, and which number it gives a name.  It knows nothing
** about `Signal`, `Process.kill` or `EXIT`, Ruby's own name for signal 0:
** those belong to the common sources under src/.  In the other direction, no
** platform type or macro (`SIGTERM`, `NSIG`, ...) crosses into the common
** layer: signal numbers travel as `mrb_int`, and names as bare C strings
** without the "SIG" prefix.
*/

#ifndef MRUBY_SIGNAL_HAL_H
#define MRUBY_SIGNAL_HAL_H

#include <mruby.h>

MRB_BEGIN_DECL

/*
 * Resolve a signal name to its number on this platform.
 *
 * @param name  a bare name such as "TERM", without the "SIG" prefix, which
 *              the caller has already stripped
 * @return 0 with *signo set, or -1 when the platform has no such signal
 */
int mrb_hal_signal_number(mrb_state *mrb, const char *name, mrb_int *signo);

/*
 * Name the signal `signo` stands for on this platform.
 *
 * @return a static bare name such as "TERM", or NULL when the number names
 *         no signal here.  The caller must not free it.
 */
const char *mrb_hal_signal_name(mrb_state *mrb, mrb_int signo);

/*
 * How many names this platform's table holds.
 *
 * A signal a host spells two ways is counted once per spelling, since each
 * is a name `Signal.list` reports.
 */
mrb_int mrb_hal_signal_count(mrb_state *mrb);

/*
 * The `index`-th name and its number, counting from 0.
 *
 * The order is the one Ruby lists signals in, so a caller walking the table
 * meets the name Ruby answers with before its aliases.
 *
 * @return a static bare name with *signo set, or NULL when `index` is past
 *         the end.  The caller must not free the name.
 */
const char *mrb_hal_signal_at(mrb_state *mrb, mrb_int index, mrb_int *signo);

MRB_END_DECL

#endif /* MRUBY_SIGNAL_HAL_H */
