/*
** process.c - Process module
**
** See Copyright Notice in mruby.h
**
** The common half of mruby-process: everything Ruby promises about a
** process, expressed over the platform-neutral primitives in process_hal.h.
** Argument shapes, return conventions, `$?` and `$$` live here; what a pid
** or a signal or a wait status *is* stays behind the HAL.
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/error.h>
#include <mruby/proc.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#ifdef MRB_USE_BIGINT
#include <mruby/internal.h>
#endif
#include "process_hal.h"
#include "process_internal.h"
#include "signal_hal.h"

#include <errno.h>
#include <limits.h>
#include <stdint.h>
#include <string.h>

/* `$?` and `$$` are not word names, so MRB_GVSYM() cannot spell them and
   they are interned where they are used. */
static void
set_last_status(mrb_state *mrb, mrb_value status)
{
  mrb_gv_set(mrb, mrb_intern_lit(mrb, "$?"), status);
}

/*
 * Refuse a value that would not survive the narrowing a port has to do with
 * it.
 *
 * What is wrong with such a value is its size, and size is not something a
 * port can report: the HAL answers with an `errno`, which has no spelling for
 * "that was never a pid" and would have to borrow one that also means
 * something else.  So the value is refused here, where `RangeError` can be
 * said, and a port keeps `errno` for what the platform actually answered.
 * mruby-socket checks the `int` fields of a getaddrinfo hint the same way.
 */
mrb_int
mrb_process_int_arg(mrb_state *mrb, mrb_int v, const char *what)
{
#if MRB_INT_MAX > INT_MAX
  if (v < (mrb_int)INT_MIN || v > (mrb_int)INT_MAX) {
    mrb_raisef(mrb, E_RANGE_ERROR, "%s out of range: %i", what, v);
  }
#else
  (void)mrb;
  (void)what;
#endif
  return v;
}

/*
 * Read a signal argument into a number.
 *
 * Ruby lets a signal be an Integer, or a name as a String or Symbol, with or
 * without the "SIG" prefix.  Deciding that much is this gem's work; which
 * number a name stands for is mruby-signal's, through the signal HAL.
 */
static mrb_int
signal_to_number(mrb_state *mrb, mrb_value sig)
{
  const char *name;
  mrb_int len, signo;
  char bare[32];

  if (mrb_integer_p(sig)) {
    signo = mrb_integer(sig);
    if (signo < 0) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "signalling a process group is not supported");
    }
    return mrb_process_int_arg(mrb, signo, "signal number");
  }

  if (mrb_symbol_p(sig)) {
    name = mrb_sym_name_len(mrb, mrb_symbol(sig), &len);
  }
  else if (mrb_string_p(sig)) {
    name = RSTRING_PTR(sig);
    len = RSTRING_LEN(sig);
  }
  else {
    /* Anything else is refused by its class, as Ruby reports it.  A bigint
       lands here too: it is an Integer that no signal number can be, and
       `mrb_integer_p` is false for one, which is also how CRuby comes to
       report a value like 2**70 by class rather than by size. */
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "bad signal type %C", mrb_obj_class(mrb, sig));
  }
  if (name == NULL) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "bad signal name");
  }

  /* The HAL is asked in C strings, so a name holding a NUL would be read as
     the part before it and would signal something that was never asked for.
     Checked before the name is taken apart, as CRuby checks it. */
  if (memchr(name, '\0', (size_t)len) != NULL) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "signal name with null byte");
  }

  /* A leading "-" asks for the process group, which this gem does not do
     yet; say so rather than quietly signalling the process instead. */
  if (len > 0 && name[0] == '-') {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "signalling a process group is not supported");
  }
  /* A name that is exactly "SIG" loses the prefix like any other, leaving
     nothing to look up.  Nothing is not an error of its own: Ruby reports the
     empty name the way it reports the empty string, as the message below
     writing "SIG" and then nothing, so it goes on to the lookup and fails
     there. */
  if (len >= 3 && memcmp(name, "SIG", 3) == 0) {
    name += 3;
    len -= 3;
  }
  if ((size_t)len >= sizeof(bare)) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unsupported signal 'SIG%l'", name, (size_t)len);
  }
  /* The HAL takes a C string, and `name` is a slice of a longer one. */
  memcpy(bare, name, (size_t)len);
  bare[len] = '\0';

  if (mrb_hal_signal_number(mrb, bare, &signo) != 0) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unsupported signal 'SIG%s'", bare);
  }
  return signo;
}

/*
 * call-seq:
 *   Process.pid -> integer
 *
 * The process ID of the running process.  Also available as <code>$$</code>.
 */
static mrb_value
process_pid(mrb_state *mrb, mrb_value self)
{
  mrb_int pid = mrb_hal_process_pid(mrb);

  if (pid < 0) mrb_sys_fail(mrb, "getpid");
  return mrb_int_value(mrb, pid);
}

/*
 * call-seq:
 *   Process.ppid -> integer
 *
 * The process ID of the parent of the running process.
 */
static mrb_value
process_ppid(mrb_state *mrb, mrb_value self)
{
  mrb_int ppid = mrb_hal_process_ppid(mrb);

  if (ppid < 0) mrb_sys_fail(mrb, "getppid");
  return mrb_int_value(mrb, ppid);
}

/*
 * call-seq:
 *   Process.kill(signal, pid, ...) -> integer
 *
 * Sends +signal+ to each process, and returns how many it was sent to.
 * At least one process must be named.  +signal+ is a number, or a name as a
 * String or Symbol with or without the "SIG" prefix.  Signal 0 sends nothing
 * and only checks that the process is there to be signalled; it is spelled
 * as the number, since "EXIT" names it only where a handler is being set.
 *
 *   Process.kill(:TERM, pid)
 *   Process.kill("SIGTERM", pid)
 *   Process.kill(0, pid)          # => 1 if pid exists, Errno::ESRCH if not
 *
 * Which processes a +pid+ names is what its sign says, and what a sign says
 * is the platform's to answer.  A positive number names the process with that
 * ID everywhere; where the platform reads the rest as POSIX does, 0 names
 * every process in the caller's process group, -1 every process the caller
 * has permission to signal, and a number below -1 every process in the
 * process group whose ID is -pid.  Windows has no such selectors and answers
 * Errno::ESRCH for every one of them.
 *
 * Naming a process group through the signal instead, which is a negative
 * signal number or a name written with a leading "-" and asks for the group
 * of each +pid+ given, is not supported yet and raises ArgumentError.  A
 * signal of any other class, a big integer included, raises ArgumentError
 * naming that class.  A signal number or a pid too large for the platform
 * to carry raises RangeError.
 */
static mrb_value
process_kill(mrb_state *mrb, mrb_value self)
{
  mrb_value sig, *pids;
  mrb_int argc, i, signo;

  mrb_get_args(mrb, "o*", &sig, &pids, &argc);
  /* The rest is what names the processes, so an empty one leaves nothing to
     signal.  Counting the signal back in makes the message read as the call
     was written. */
  if (argc == 0) {
    mrb_argnum_error(mrb, 1, 2, -1);
  }
  signo = signal_to_number(mrb, sig);

  for (i = 0; i < argc; i++) {
    mrb_int pid = mrb_process_int_arg(mrb, mrb_as_int(mrb, pids[i]), "pid");
    if (mrb_hal_process_kill(mrb, pid, signo) != 0) {
      /* What a SystemCallError message carries after the error itself is the
         object the call was working on, the way `File.open` names the path it
         could not open.  Signalling a process works on no such object, and
         CRuby names nothing here.  Passing the name of the call instead would
         put a word in the message that CRuby never prints. */
      mrb_sys_fail(mrb, NULL);
    }
  }
  return mrb_int_value(mrb, argc);
}

/* The wait itself.  Two module functions differ only in whether the status
   is handed back beside the pid, so the wait is done here and both of them
   publish it through `$?`. */
static mrb_value
wait_for_child(mrb_state *mrb, mrb_value *statusp)
{
  mrb_int pid = MRB_PROCESS_WAIT_ANY;
  mrb_int flags = 0;
  mrb_int result_pid = 0, raw_status = 0;

  mrb_get_args(mrb, "|ii", &pid, &flags);
  pid = mrb_process_int_arg(mrb, pid, "pid");

  /* A port is told what a wait means in mruby's own bits and answers only for
     the ones it was given, so a bit that stands for nothing has to be refused
     before it reaches one.  Checked here rather than in each port, so that
     every port refuses the same values. */
  if (flags < 0 || (flags & ~(mrb_int)MRB_PROCESS_WAIT_FLAGS) != 0) {
    errno = EINVAL;
    mrb_sys_fail(mrb, NULL);
  }

  /* A wait names no object either, so both failures report the error alone. */
  if (mrb_hal_process_waitpid(mrb, pid, (unsigned int)flags, &result_pid, &raw_status) != 0) {
    mrb_sys_fail(mrb, NULL);
  }
  if (result_pid == 0) {
    /* MRB_PROCESS_WAIT_NOHANG and nothing had finished */
    *statusp = mrb_nil_value();
    set_last_status(mrb, mrb_nil_value());
    return mrb_nil_value();
  }
  *statusp = mrb_process_status_new(mrb, result_pid, raw_status);
  set_last_status(mrb, *statusp);
  return mrb_int_value(mrb, result_pid);
}

/*
 * call-seq:
 *   Process.waitpid(pid = -1, flags = 0) -> integer or nil
 *   Process.wait(pid = -1, flags = 0)    -> integer or nil
 *
 * Waits for a child process to finish and returns its process ID, setting
 * <code>$?</code> to the Process::Status it finished with.  Which children
 * are waited for is what +pid+ chooses: a positive number names one child,
 * 0 any child in the caller's process group, -1 (the default) any child at
 * all, and a number below -1 any child in the process group whose ID is
 * -pid.
 *
 * With Process::WNOHANG among +flags+, returns nil and sets <code>$?</code>
 * to nil when no child is ready.  With Process::WUNTRACED, a stopped child
 * is reported too, where the platform has such a thing.
 *
 * Raises Errno::ECHILD when there is no child to wait for, Errno::EINVAL
 * when +flags+ holds a bit that is not one of the two, and RangeError when
 * +pid+ is too large for the platform to carry.
 */
static mrb_value
process_waitpid(mrb_state *mrb, mrb_value self)
{
  mrb_value status;

  return wait_for_child(mrb, &status);
}

/*
 * call-seq:
 *   Process.waitpid2(pid = -1, flags = 0) -> [pid, status] or nil
 *   Process.wait2(pid = -1, flags = 0)    -> [pid, status] or nil
 *
 * Waits as Process.waitpid does and returns the process ID and the
 * Process::Status together, rather than leaving the status to be read from
 * <code>$?</code>, which is set either way.  With Process::WNOHANG among
 * +flags+ and no child ready, returns nil.
 *
 *   pid, status = Process.wait2
 *   status.exitstatus   #=> 0
 */
static mrb_value
process_waitpid2(mrb_state *mrb, mrb_value self)
{
  mrb_value status, pid = wait_for_child(mrb, &status);

  if (mrb_nil_p(pid)) return mrb_nil_value();
  return mrb_assoc_new(mrb, pid, status);
}

/*
 * Clocks
 *
 * A reading crosses the HAL as whole seconds and nanoseconds, and everything
 * Ruby says about it is said here: which numbers name a clock, which unit
 * names are understood, whether the answer is an Integer or a Float, and
 * what happens where this build has no Float to answer with.  A port always
 * reports the same two numbers and is asked nothing about any of that.
 */

/*
 * Put `sec` seconds and `frac` `per_sec`-ths of one together, and say
 * whether they fit an int64_t at all.
 *
 * int64_t's own ends are split into the same two parts as a reading, so that
 * whole seconds are weighed against whole seconds and fractions against
 * fractions, and no product is asked for that is itself past int64_t.  The
 * second `INT64_MIN` falls in is one below what `INT64_MIN / per_sec` gives,
 * C dividing towards zero rather than downwards, and its own product is off
 * the end; a reading landing in that last second is counted up from
 * `INT64_MIN` instead of multiplied.
 *
 * `frac` is at least zero and less than `per_sec`, a reading's nanoseconds
 * being carried into its seconds before it is handed up, so the fraction
 * only ever moves a value towards positive.
 */
static mrb_bool
clock_int64_value(int64_t sec, int64_t frac, int64_t per_sec, int64_t *value)
{
  int64_t max_sec = INT64_MAX / per_sec, max_frac = INT64_MAX % per_sec;
  int64_t min_sec = INT64_MIN / per_sec, min_frac = INT64_MIN % per_sec;

  if (min_frac != 0) { /* min_sec is one above the second INT64_MIN falls in */
    min_sec -= 1;
    min_frac += per_sec;
  }
  if (sec > max_sec || (sec == max_sec && frac > max_frac)) return FALSE;
  if (sec < min_sec || (sec == min_sec && frac < min_frac)) return FALSE;

  *value = (sec == min_sec) ? INT64_MIN + (frac - min_frac) : sec * per_sec + frac;
  return TRUE;
}

#ifdef MRB_USE_BIGINT
/*
 * The same sum built as a bigint, for a reading past what an int64_t holds.
 *
 * How large a reading is, is the platform's business, and the HAL carries it
 * whole; how much of it an Integer can hold is mruby's, and a build with
 * bigints can hold all of it however the reading is scaled.  Nothing about
 * the answer is decided by the width of the arithmetic that would have been
 * used: the sum comes back normalized, so one that turns out to fit is an
 * ordinary Integer.
 *
 * That normalizing is not only the final sum's to do: `sec * per_sec` is
 * itself a bigint multiply, and mrb_bint_mul() normalizes its own result the
 * same way, so a product that turns out to fit is handed back as an ordinary
 * Integer already, which happens whenever `sec` is small enough that only
 * adding `frac` pushes the total past what fits.  mrb_as_bint() is the part
 * that keeps the add after it correct either way: it takes a bigint back
 * unchanged and widens an ordinary Integer rather than assuming the product
 * stayed one.
 */
static mrb_value
clock_bint_result(mrb_state *mrb, int64_t sec, int64_t frac, int64_t per_sec)
{
  mrb_value value = mrb_bint_mul(mrb, mrb_bint_new_int64(mrb, sec),
                                 mrb_bint_new_int64(mrb, per_sec));
  return mrb_bint_add(mrb, mrb_as_bint(mrb, value), mrb_bint_new_int64(mrb, frac));
}
#endif

/*
 * Read `t` as a whole number of `per_sec`-ths of a second.
 *
 * The nanoseconds contribute whatever the unit can hold of them and the rest
 * is dropped, which is the truncation CRuby does: an integer unit answers
 * with the reading as of the last tick of that unit, never with the one
 * after it.
 *
 * A value the build's Integer cannot hold is answered as a bigint where the
 * build has them, which is the Integer CRuby answers with, and refused with
 * RangeError where it has not, the way an oversized pid is refused above.
 * That is reachable rather than theoretical: a wall clock in nanoseconds is
 * about 1.8e18, which fits a 64-bit mrb_int and no narrower one, and in
 * milliseconds it is already past what 32 bits hold.  Past what an int64_t
 * holds, a wall clock in nanoseconds from 2262 on, is answered the same way.
 * This is the only place a reading meets mruby's Integer: a port hands up
 * what its clock said, in int64_t, so a wall clock in seconds outgrowing a
 * 32-bit mrb_int in 2038 arrives here like any other value too large to hand
 * back.
 */
static mrb_value
clock_int_result(mrb_state *mrb, mrb_sym unit, const mrb_process_clock_time *t,
                 int64_t per_sec)
{
  int64_t frac = t->nsec / (NSEC_PER_SEC / per_sec);
  int64_t value;

#ifdef MRB_USE_BIGINT
  (void)unit; /* nothing is refused for its size where there are bigints */
#endif

  if (!clock_int64_value(t->sec, frac, per_sec, &value)) {
#ifdef MRB_USE_BIGINT
    return clock_bint_result(mrb, t->sec, frac, per_sec);
#else
    goto too_large;
#endif
  }
#if MRB_INT_MAX < INT64_MAX
  if (value > MRB_INT_MAX || value < MRB_INT_MIN) {
# ifdef MRB_USE_BIGINT
    return mrb_bint_new_int64(mrb, value);
# else
    goto too_large;
# endif
  }
#endif
  return mrb_int_value(mrb, (mrb_int)value);

#ifndef MRB_USE_BIGINT
too_large:
  mrb_raisef(mrb, E_RANGE_ERROR, "clock reading in %n does not fit an Integer", unit);
  /* not reached */
  return mrb_nil_value();
#endif
}

#ifndef MRB_NO_FLOAT
/*
 * Read `t` as a fractional number of `per_sec`-ths of a second.
 *
 * The two fields are scaled apart rather than added first, so that the
 * nanoseconds are not rounded away against a wall-clock second before they
 * are used.  `NSEC_PER_SEC / per_sec` is exact for every unit here, being a
 * power of ten small enough to be written whole in an mrb_float, a float32
 * build included.
 */
static mrb_value
clock_float_result(mrb_state *mrb, const mrb_process_clock_time *t, mrb_float per_sec)
{
  mrb_float value = (mrb_float)t->sec * per_sec +
                    (mrb_float)t->nsec / ((mrb_float)NSEC_PER_SEC / per_sec);
  return mrb_float_value(mrb, value);
}

/*
 * Read a resolution as the number of times a second it can tell apart, which
 * is one over what `:float_second` would say.
 *
 * Divided out of the nanoseconds rather than out of a fraction of a second,
 * so that a resolution which is a whole number of them, which is every one a
 * port can report, comes back as a whole number of hertz wherever one
 * exists: 1ns is 1000000000.0 and 100ns is 10000000.0, where dividing 1.0 by
 * a rounded fraction of a second would land beside them.
 *
 * The divisor is mrb_hal_process_clock_getres()'s promise that a
 * granularity is never zero; no check stands behind it here.  The gem's
 * tests are the one caller that could break it, and a zero there divides to
 * infinity rather than trapping.
 */
static mrb_value
clock_hertz_result(mrb_state *mrb, const mrb_process_clock_time *t)
{
  mrb_float nsec = (mrb_float)t->sec * (mrb_float)NSEC_PER_SEC + (mrb_float)t->nsec;
  return mrb_float_value(mrb, (mrb_float)NSEC_PER_SEC / nsec);
}
#endif

/*
 * The unit a caller named, or the one leaving it out means.
 *
 * The unit is a Symbol, as it is in CRuby, which takes nothing else: a
 * String naming the same thing is not a unit, and neither is a number.
 * `nil` is the one value that is not a Symbol and not refused: naming no
 * unit is what a caller who passes it means, which is what leaving the
 * argument out means, and CRuby cannot tell the two apart at all: an
 * omitted unit arrives there as `nil`.
 *
 * `resolution` says whether the answer being asked for is one, which
 * decides `:hertz`: a resolution can be asked for as a rate, and a clock
 * reading cannot, since there is no rate at which a moment happened.
 * CRuby draws the line in the same place, and answers a reading asked for
 * in hertz with the same "unexpected unit" this falls through to.
 *
 * Takes no reading as an argument: whether a unit is one this build knows
 * is a mistake in the call, not a fact about what the platform's clock can
 * do, and is settled before a port is ever asked for one.  Callers resolve
 * the unit first and let the HAL call fail afterward, so a bad unit is
 * reported as that rather than as whatever errno the HAL happened to fail
 * with first.
 */
static mrb_sym
clock_unit_resolve(mrb_state *mrb, mrb_value unit, mrb_bool resolution)
{
  mrb_sym u;

  if (mrb_undef_p(unit) || mrb_nil_p(unit)) {
    u = MRB_SYM(float_second);
  }
  else if (mrb_symbol_p(unit)) {
    u = mrb_symbol(unit);
  }
  else {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "unexpected unit: %!v", unit);
    return 0; /* not reached */
  }

  if (u == MRB_SYM(second) || u == MRB_SYM(millisecond) ||
      u == MRB_SYM(microsecond) || u == MRB_SYM(nanosecond)) {
    return u;
  }
#ifndef MRB_NO_FLOAT
  if (u == MRB_SYM(float_second) || u == MRB_SYM(float_millisecond) ||
      u == MRB_SYM(float_microsecond)) {
    return u;
  }
  if (resolution && u == MRB_SYM(hertz)) return u;
#else
  /* Without a Float there is nothing to hand back, and the method is not
     made to disappear for it: a program written once is told at the call
     site what this build will not do, as it is told what a platform will
     not do.  The integer units above are untouched, and `:nanosecond` says
     everything `:float_second` would have wherever this build's Integer can
     carry it. */
  if (u == MRB_SYM(float_second) || u == MRB_SYM(float_millisecond) ||
      u == MRB_SYM(float_microsecond) || (resolution && u == MRB_SYM(hertz))) {
    mrb_raisef(mrb, E_NOTIMP_ERROR, "%n needs a build with Float", u);
  }
#endif
  mrb_raisef(mrb, E_ARGUMENT_ERROR, "unexpected unit: %n", u);
  /* not reached */
  return 0;
}

/* Answer a reading in the unit `u` already resolved to: a dispatch on the
   unit alone, the ends of an int64_t and of this build's Integer being
   clock_int_result's. */
static mrb_value
clock_unit_convert(mrb_state *mrb, mrb_sym u, const mrb_process_clock_time *t,
                   mrb_bool resolution)
{
  if (u == MRB_SYM(second))      return clock_int_result(mrb, u, t, 1);
  if (u == MRB_SYM(millisecond)) return clock_int_result(mrb, u, t, 1000);
  if (u == MRB_SYM(microsecond)) return clock_int_result(mrb, u, t, 1000000);
  if (u == MRB_SYM(nanosecond))  return clock_int_result(mrb, u, t, NSEC_PER_SEC);
#ifndef MRB_NO_FLOAT
  if (u == MRB_SYM(float_second))      return clock_float_result(mrb, t, 1.0);
  if (u == MRB_SYM(float_millisecond)) return clock_float_result(mrb, t, 1.0e3);
  if (u == MRB_SYM(float_microsecond)) return clock_float_result(mrb, t, 1.0e6);
  if (resolution && u == MRB_SYM(hertz)) return clock_hertz_result(mrb, t);
#endif
  /* u is clock_unit_resolve's own return value, which never hands back
     anything other than one of the symbols above. */
  return mrb_nil_value(); /* not reached */
}

/* mrb_process_clock_result: clock_unit_resolve and clock_unit_convert in one
   step.  process_internal.h says why the tests are handed this. */
mrb_value
mrb_process_clock_result(mrb_state *mrb, mrb_value unit,
                         const mrb_process_clock_time *t, mrb_bool resolution)
{
  mrb_sym u = clock_unit_resolve(mrb, unit, resolution);
  return clock_unit_convert(mrb, u, t, resolution);
}

/*
 * The clock a Symbol names, or -1 for one that names none of them.
 *
 * A clock can be named as well as numbered, as it can in CRuby, and the name
 * is the constant's: `Process.clock_gettime(:CLOCK_MONOTONIC)` reads what
 * `Process::CLOCK_MONOTONIC` numbers.  That is worth more here than there,
 * since the numbers are mruby's own, so a program that names its clock
 * rather than numbering it reads the same on both.
 *
 * CRuby knows further names: the clocks only some platforms have, and the
 * ways it emulates one the host lacks (`:GETTIMEOFDAY_BASED_CLOCK_REALTIME`
 * and the rest).  A port here either has one of the four or says it has not,
 * so there is nothing for such a name to pick out, and it names no clock the
 * way an unknown name in CRuby names none.
 */
static mrb_int
clock_id_for_sym(mrb_sym name)
{
  if (name == MRB_SYM(CLOCK_REALTIME))
    return MRB_PROCESS_CLOCK_REALTIME;
  if (name == MRB_SYM(CLOCK_MONOTONIC))
    return MRB_PROCESS_CLOCK_MONOTONIC;
  if (name == MRB_SYM(CLOCK_PROCESS_CPUTIME_ID))
    return MRB_PROCESS_CLOCK_PROCESS_CPUTIME;
  if (name == MRB_SYM(CLOCK_THREAD_CPUTIME_ID))
    return MRB_PROCESS_CLOCK_THREAD_CPUTIME;
  return -1;
}

/*
 * Fail naming the call and the clock it was asked for, as CRuby names it:
 * "clock_gettime(:NOPE)", not "clock_gettime".  What went wrong is the
 * clock rather than the call, and a caller who named one is shown the name
 * back rather than a number never written.
 *
 * The description is built before `errno` is handed on because building a
 * String allocates, and an allocation can leave `errno` anywhere.
 */
static void
clock_sys_fail(mrb_state *mrb, const char *what, mrb_value clock_id)
{
  int no = errno;
  const char *at =
    mrb_str_to_cstr(mrb, mrb_format(mrb, "%s(%!v)", what, clock_id));

  errno = no;
  mrb_sys_fail(mrb, at);
}

/*
 * Read a clock argument into one of mruby's own clock numbers.
 *
 * An id outside the list is refused before a port sees it, as an unknown
 * wait flag is; the list is mruby's own, for the reason process_hal.h
 * gives.  It is refused with the errno a platform's own call answers for a
 * clock it does not have, which is what CRuby raises here too, rather than
 * with RangeError: nothing is wrong with the size of the number, it simply
 * names no clock.  A name that picks out none of the four arrives at the
 * same place, since it says the same thing.
 */
static mrb_int
clock_id_arg(mrb_state *mrb, mrb_value clock_id, const char *what)
{
  mrb_int id;

  if (mrb_symbol_p(clock_id)) {
    id = clock_id_for_sym(mrb_symbol(clock_id));
  }
  else {
    /* A String is not a name: it is refused for its type, as CRuby refuses
       it, rather than read for a clock name it might spell. */
    id = mrb_as_int(mrb, clock_id);
  }
  if (id < 0 || id >= MRB_PROCESS_CLOCK_COUNT) {
    errno = EINVAL;
    clock_sys_fail(mrb, what, clock_id);
  }
  return id;
}

/*
 * call-seq:
 *   Process.clock_gettime(clock_id, unit = :float_second) -> float or integer
 *
 * Reads the clock +clock_id+ names and returns what it says, in +unit+.
 *
 *   Process.clock_gettime(Process::CLOCK_MONOTONIC)               #=> 1234.5678
 *   Process.clock_gettime(Process::CLOCK_MONOTONIC, :nanosecond)  #=> 1234567800000
 *
 * The clock is one of Process::CLOCK_REALTIME, which is the wall clock
 * counted from the Unix epoch and may step when the host's idea of the time
 * is corrected; Process::CLOCK_MONOTONIC, which never steps and is counted
 * from an origin the platform chooses and this process keeps;
 * Process::CLOCK_PROCESS_CPUTIME_ID, the CPU time this process has spent;
 * and Process::CLOCK_THREAD_CPUTIME_ID, the CPU time this thread has spent.
 * All four are defined everywhere, and one this platform does not have
 * raises Errno::EINVAL, as a number naming no clock at all does.
 *
 * A clock can also be named by the Symbol its constant is named with, as in
 * CRuby, so that a program need not depend on the number:
 *
 *   Process.clock_gettime(:CLOCK_MONOTONIC)  #=> 1234.5678
 *
 * A Symbol naming none of the four raises Errno::EINVAL too.
 *
 * The unit is +:float_second+, +:float_millisecond+, +:float_microsecond+,
 * which answer with a Float, or +:second+, +:millisecond+, +:microsecond+,
 * +:nanosecond+, which answer with an Integer, dropping what the unit cannot
 * hold rather than rounding it.  Process.clock_getres takes +:hertz+ as
 * well, and a reading asked for in it raises ArgumentError here.  Passing
 * +nil+ names no unit, and answers in the default, as leaving the argument
 * out does.  Any other unit raises ArgumentError, and in a build without
 * Float the three Float units raise NotImplementedError.
 * An answer too large for this build's Integer raises RangeError, unless the
 * build has bigints for it to be answered in.
 */
static mrb_value
process_clock_gettime(mrb_state *mrb, mrb_value self)
{
  mrb_value clock_id, unit = mrb_undef_value();
  mrb_process_clock_time t;
  mrb_int id;
  mrb_sym u;

  mrb_get_args(mrb, "o|o", &clock_id, &unit);
  id = clock_id_arg(mrb, clock_id, "clock_gettime");
  /* Resolved before the HAL is asked; see clock_unit_resolve(). */
  u = clock_unit_resolve(mrb, unit, FALSE);

  if (mrb_hal_process_clock_gettime(mrb, id, &t) != 0) {
    clock_sys_fail(mrb, "clock_gettime", clock_id);
  }
  return clock_unit_convert(mrb, u, &t, FALSE);
}

/*
 * call-seq:
 *   Process.clock_getres(clock_id, unit = :float_second) -> float or integer
 *
 * How finely the clock +clock_id+ names is read: the smallest difference
 * two readings of it can show, in +unit+.
 *
 *   Process.clock_getres(Process::CLOCK_MONOTONIC, :nanosecond)  #=> 1
 *
 * The clocks and the units are Process.clock_gettime's, and so are the
 * errors.  A resolution finer than the unit asked for reads as 0 in an
 * integer unit, since that is what is left of it after the truncation.
 *
 * This describes the way the platform is read rather than the clock behind
 * it: where the platform states the interval a clock advances on, that is
 * the answer, and where it does not, the answer is the granularity of the
 * call the reading came out of.  A reading is never distinguishable more
 * finely than this, and may well move more coarsely.  CRuby answers on the
 * same terms.
 *
 * A resolution can also be asked for as +:hertz+, the number of times a
 * second the clock can be told apart, which is one over what +:float_second+
 * says and is a Float like the rest of the Float units.  A clock reading
 * cannot be asked for in hertz, there being no rate at which a moment
 * happened, which is where CRuby draws the line too.
 *
 *   Process.clock_getres(Process::CLOCK_MONOTONIC, :hertz)  #=> 1000000000.0
 */
static mrb_value
process_clock_getres(mrb_state *mrb, mrb_value self)
{
  mrb_value clock_id, unit = mrb_undef_value();
  mrb_process_clock_time t;
  mrb_int id;
  mrb_sym u;

  mrb_get_args(mrb, "o|o", &clock_id, &unit);
  id = clock_id_arg(mrb, clock_id, "clock_getres");
  /* Resolved before the HAL is asked; see clock_unit_resolve(). */
  u = clock_unit_resolve(mrb, unit, TRUE);

  if (mrb_hal_process_clock_getres(mrb, id, &t) != 0) {
    clock_sys_fail(mrb, "clock_getres", clock_id);
  }
  return clock_unit_convert(mrb, u, &t, TRUE);
}

/*
 * call-seq:
 *   Process.times -> a Process::Tms
 *
 * How much CPU time this process, and its waited-for terminated children,
 * have used, as a Process::Tms holding four Float numbers of seconds;
 * Process::Tms says what each of the four members covers.
 *
 * Answers only in Float, as CRuby does, there being no argument to name an
 * Integer unit by the way Process.clock_gettime has one, so a build without
 * Float raises NotImplementedError rather than answering something narrower.
 */
static mrb_value
process_times(mrb_state *mrb, mrb_value self)
{
#ifndef MRB_NO_FLOAT
  /* The class gem_init captured into this method's environment, not
     whatever the Tms constant holds now: CRuby's Process.times builds on
     rb_cProcessTms the same way, so reassigning Process::Tms does not
     change what this answers. */
  struct RClass *tms = mrb_class_ptr(mrb_proc_cfunc_env_get(mrb, 0));
  mrb_process_times pt;
  mrb_value argv[4];

  if (mrb_hal_process_times(mrb, &pt) != 0) {
    /* Names no object, as a wait or a kill does not either: nothing this
       call was working on failed, the reading itself did. */
    mrb_sys_fail(mrb, NULL);
  }
  argv[0] = clock_float_result(mrb, &pt.utime,  1.0);
  argv[1] = clock_float_result(mrb, &pt.stime,  1.0);
  argv[2] = clock_float_result(mrb, &pt.cutime, 1.0);
  argv[3] = clock_float_result(mrb, &pt.cstime, 1.0);
  /* The four members go to Struct's #initialize in the order gem_init gave
     them to Struct.new. */
  return mrb_obj_new(mrb, tms, 4, argv);
#else
  mrb_raise(mrb, E_NOTIMP_ERROR, "Process.times needs a build with Float");
  return mrb_nil_value(); /* not reached */
#endif
}

void
mrb_mruby_process_gem_init(mrb_state *mrb)
{
  struct RClass *process;
  mrb_int pid;

  mrb_hal_process_init(mrb);

  process = mrb_define_module_id(mrb, MRB_SYM(Process));

  /* The wait flags are mruby's own bits, not the host's: a program that
     passes Process::WNOHANG means the same thing on every port. */
  mrb_define_const_id(mrb, process, MRB_SYM(WNOHANG),
                      mrb_fixnum_value(MRB_PROCESS_WAIT_NOHANG));
  mrb_define_const_id(mrb, process, MRB_SYM(WUNTRACED),
                      mrb_fixnum_value(MRB_PROCESS_WAIT_UNTRACED));

  /* The clock numbers are mruby's own too; see process_hal.h.  All four are
     defined everywhere, so a program is told at the call site where a
     platform has no such clock rather than finding the constant missing. */
  mrb_define_const_id(mrb, process, MRB_SYM(CLOCK_REALTIME),
                      mrb_fixnum_value(MRB_PROCESS_CLOCK_REALTIME));
  mrb_define_const_id(mrb, process, MRB_SYM(CLOCK_MONOTONIC),
                      mrb_fixnum_value(MRB_PROCESS_CLOCK_MONOTONIC));
  mrb_define_const_id(mrb, process, MRB_SYM(CLOCK_PROCESS_CPUTIME_ID),
                      mrb_fixnum_value(MRB_PROCESS_CLOCK_PROCESS_CPUTIME));
  mrb_define_const_id(mrb, process, MRB_SYM(CLOCK_THREAD_CPUTIME_ID),
                      mrb_fixnum_value(MRB_PROCESS_CLOCK_THREAD_CPUTIME));

  mrb_define_module_function_id(mrb, process, MRB_SYM(pid),     process_pid,     MRB_ARGS_NONE());
  mrb_define_module_function_id(mrb, process, MRB_SYM(ppid),    process_ppid,    MRB_ARGS_NONE());
  mrb_define_module_function_id(mrb, process, MRB_SYM(kill),    process_kill,    MRB_ARGS_REQ(2)|MRB_ARGS_REST());
  mrb_define_module_function_id(mrb, process, MRB_SYM(waitpid),  process_waitpid,  MRB_ARGS_OPT(2));
  mrb_define_module_function_id(mrb, process, MRB_SYM(wait),     process_waitpid,  MRB_ARGS_OPT(2));
  mrb_define_module_function_id(mrb, process, MRB_SYM(waitpid2), process_waitpid2, MRB_ARGS_OPT(2));
  mrb_define_module_function_id(mrb, process, MRB_SYM(wait2),    process_waitpid2, MRB_ARGS_OPT(2));
  mrb_define_module_function_id(mrb, process, MRB_SYM(clock_gettime), process_clock_gettime, MRB_ARGS_ARG(1, 1));
  mrb_define_module_function_id(mrb, process, MRB_SYM(clock_getres),  process_clock_getres,  MRB_ARGS_ARG(1, 1));

  /* Process::Tms, what Process.times answers with: a Struct of the four
     members, as CRuby's own Process::Tms is, so a Tms answers to everything
     a Struct does.  #utime and #stime are this process's own user and
     system CPU time, in seconds; #cutime and #cstime total the same over
     every terminated child this process has waited for so far.

     The class is made through the same Struct.new Ruby source would call,
     but from here rather than from mrblib, so that the created class itself
     can ride into Process.times as cfunc environment: Process.times then
     keeps answering instances of this class even after the Tms constant is
     reassigned, as CRuby's does by holding it in rb_cProcessTms.
     mrb_const_set is what names the anonymous Struct class "Process::Tms";
     mrb_define_const_id skips naming.  The two raw defines lay the method
     the way mrb_define_module_function_id would: public on the module's
     singleton class, private as an instance method. */
  {
    struct RClass *struct_cls = mrb_class_get_id(mrb, MRB_SYM(Struct));
    mrb_value tms = mrb_funcall_id(mrb, mrb_obj_value(struct_cls), MRB_SYM(new), 4,
                                   mrb_symbol_value(MRB_SYM(utime)),
                                   mrb_symbol_value(MRB_SYM(stime)),
                                   mrb_symbol_value(MRB_SYM(cutime)),
                                   mrb_symbol_value(MRB_SYM(cstime)));
    struct RProc *times_proc = mrb_proc_new_cfunc_with_env(mrb, process_times, 1, &tms);
    mrb_method_t m;

    /* mrb_define_module_function_id would carry MRB_ARGS_NONE() for us; a
       raw proc method checks nothing until the aspec is set on the proc. */
    mrb_proc_set_cfunc_aspec(times_proc, MRB_ARGS_NONE());

    mrb_const_set(mrb, mrb_obj_value(process), MRB_SYM(Tms), tms);
    MRB_METHOD_FROM_PROC(m, times_proc);
    mrb_define_method_raw(mrb, mrb_class_ptr(mrb_singleton_class(mrb, mrb_obj_value(process))),
                          MRB_SYM(times), m);
    MRB_METHOD_SET_VISIBILITY(m, MRB_METHOD_PRIVATE_FL);
    mrb_define_method_raw(mrb, process, MRB_SYM(times), m);
  }

  mrb_process_status_init(mrb, process);

  pid = mrb_hal_process_pid(mrb);
  if (pid >= 0) {
    mrb_gv_set(mrb, mrb_intern_lit(mrb, "$$"), mrb_int_value(mrb, pid));
  }
}

void
mrb_mruby_process_gem_final(mrb_state *mrb)
{
  mrb_hal_process_final(mrb);
}
