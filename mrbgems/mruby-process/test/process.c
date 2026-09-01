#include <mruby.h>
#include <mruby/class.h>
#include "process_hal.h"
#include "process_internal.h"

#include <errno.h>
#include <stdlib.h>

/* ProcessStatusTest.build(pid, raw_status, klass) -> status
 *
 * `Process::Status.new` is undefined, as it is in CRuby, and the statuses a
 * test can have a child report are the exited ones: signalled, stopped and
 * core dumped statuses have to be written from a raw value to be examined at
 * all.  This writes one the way the gem and mruby-io do, by allocating an
 * instance and initializing it, so what the tests read is the construction
 * path that is left rather than a door held open for them.
 *
 * Both numbers are passed on as they were written, so a value #initialize
 * turns away by type or by size is turned away here just the same.
 */
static mrb_value
test_status_build(mrb_state *mrb, mrb_value self)
{
  mrb_value argv[2];
  struct RClass *klass;

  mrb_get_args(mrb, "ooc", &argv[0], &argv[1], &klass);
  return mrb_obj_new(mrb, klass, 2, argv);
}

/* Read a decimal into an int64_t, refusing anything the type cannot hold. */
static int64_t
test_clock_int64(mrb_state *mrb, const char *s, const char *what)
{
  char *end;
  long long v;

  errno = 0;
  v = strtoll(s, &end, 10);
  if (*s == '\0' || *end != '\0' || errno == ERANGE) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "%s is not a number an int64_t holds: %s", what, s);
  }
  return (int64_t)v;
}

/* ProcessClockTest.convert(sec, nsec, unit, resolution = false) -> number
 *
 * The reading of `sec` seconds and `nsec` nanoseconds, answered in `unit` by
 * the very code a clock reading is answered by.  What a reading becomes at
 * the ends of an int64_t and at the ends of this build's Integer is decided
 * there, and no clock comes within centuries of either end, so the endings
 * are handed to it rather than waited for.
 *
 * `sec` is a String because a build whose Integer is 32 bits cannot write
 * the seconds this is about, and those are exactly the ones worth asking
 * about.  `nsec` is nanoseconds within one second, which every build can
 * write and which is what a port promises to report; a number outside that
 * is refused here rather than passed on, a port that broke the promise being
 * the bug in that case.
 */
static mrb_value
test_clock_convert(mrb_state *mrb, mrb_value self)
{
  const char *sec;
  mrb_int nsec;
  mrb_value unit;
  mrb_bool resolution = FALSE;
  mrb_process_clock_time t;

  mrb_get_args(mrb, "zio|b", &sec, &nsec, &unit, &resolution);
  if (nsec < 0 || nsec >= NSEC_PER_SEC) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "nsec outside one second: %i", nsec);
  }
  t.sec = test_clock_int64(mrb, sec, "sec");
  t.nsec = (int64_t)nsec;
  return mrb_process_clock_result(mrb, unit, &t, resolution);
}

/* ProcessClockTest.fits?(decimal) -> true or false
 *
 * Whether an Integer in this build holds the number `decimal` spells, so
 * that a test can say which of the two answers a reading is owed without
 * knowing how wide an mrb_int is here or whether there are bigints.  It
 * deliberately shares no line with the conversion it is used to check.
 */
static mrb_value
test_clock_fits(mrb_state *mrb, mrb_value self)
{
  const char *decimal;

  mrb_get_args(mrb, "z", &decimal);
#ifdef MRB_USE_BIGINT
  (void)decimal;
  return mrb_true_value(); /* an Integer here is as wide as it needs to be */
#else
  {
    char *end;
    long long v;

    errno = 0;
    v = strtoll(decimal, &end, 10);
    if (*decimal == '\0' || *end != '\0' || errno == ERANGE) return mrb_false_value();
    return mrb_bool_value(v >= MRB_INT_MIN && v <= MRB_INT_MAX);
  }
#endif
}

void
mrb_mruby_process_gem_test(mrb_state *mrb)
{
  struct RClass *test = mrb_define_module(mrb, "ProcessStatusTest");
  struct RClass *clock = mrb_define_module(mrb, "ProcessClockTest");

  mrb_define_module_function(mrb, test, "build", test_status_build, MRB_ARGS_REQ(3));
  mrb_define_module_function(mrb, clock, "convert", test_clock_convert,
                             MRB_ARGS_ARG(3, 1));
  mrb_define_module_function(mrb, clock, "fits?", test_clock_fits, MRB_ARGS_REQ(1));
}
