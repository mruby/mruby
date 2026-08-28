#include <mruby.h>
#include <mruby/class.h>

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

void
mrb_mruby_process_gem_test(mrb_state *mrb)
{
  struct RClass *test = mrb_define_module(mrb, "ProcessStatusTest");

  mrb_define_module_function(mrb, test, "build", test_status_build, MRB_ARGS_REQ(3));
}
