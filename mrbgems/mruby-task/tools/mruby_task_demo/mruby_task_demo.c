/*
** mruby-task-demo.c
**
** Three-thread test of the mruby-task GLib HAL. Each thread owns its
** own mrb_state and its own GMainContext (the HAL is thread-local and
** picks up the thread-default context at mrb_open time, then spawns
** its own ticker thread internally).
**
**   T1  pure foreign-loop driver. Tasks are registered, then
**       g_main_loop_run is what dispatches the scheduler via the
**       HAL's vm_run_src. No Task.run anywhere.
**
**   T2  mix. Phase 1 registers tasks and calls Task.run to drain
**       them synchronously. Phase 2 registers more tasks and lets
**       g_main_loop_run drive them. Exercises both drivers on the
**       same mrb_state in sequence.
**
**   T3  Task.run only. Registers tasks and calls Task.run. The demo
**       thread never enters g_main_loop_run -- Task.run is the
**       scheduler driver, and the HAL's idle hook iterates vm_ctx
**       from inside Task.run so the ticker's cross-thread wakes
**       still get dispatched.
*/

#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/error.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <glib.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>

static gint64 start_us;

static void
log_line(const char *msg)
{
  gint64 ms = (g_get_monotonic_time() - start_us) / 1000;
  printf("[t=%5" PRId64 " ms] %s\n", ms, msg);
  fflush(stdout);
}

static mrb_value
rb_log(mrb_state *mrb, mrb_value self)
{
  const char *msg;
  (void)self;
  mrb_get_args(mrb, "z", &msg);
  log_line(msg);
  return mrb_nil_value();
}

static void
run_ruby(mrb_state *mrb, const char *code)
{
  mrb_load_string(mrb, code);
  if (mrb->exc) {
    mrb_value exc = mrb_obj_value(mrb->exc);
    mrb_value str = mrb_funcall(mrb, exc, "to_s", 0);
    fprintf(stderr, "Ruby error: %s\n", RSTRING_PTR(str));
    fflush(stderr);
    mrb->exc = NULL;
  }
}

static void
banner(const char *msg)
{
  printf("===== %s =====\n", msg);
  fflush(stdout);
}

/*
 * T1 -- pure foreign-loop driver. Runs on the main thread (no
 * separate GThread for T1; the main thread is the foreign loop).
 *
 * Pulse task with mixed sleep styles, three staggered sleepers, and
 * spinner + stopper for timeslice preemption. g_main_loop_run is the
 * only scheduler driver.
 */
static void
run_glib_only(void)
{
  GMainContext *ctx;
  GMainLoop    *loop;
  GSource      *timeout;
  mrb_state    *mrb;

  ctx  = g_main_context_new();
  g_main_context_push_thread_default(ctx);
  loop = g_main_loop_new(ctx, FALSE);

  mrb = mrb_open();
  mrb_define_method(mrb, mrb->object_class, "log", rb_log, MRB_ARGS_REQ(1));

  banner("T1 (glib-only): pulse + 3 staggered sleepers + spinner/stopper");

  run_ruby(mrb,
    "$t1_done = false\n"
    "Task.new(name: 'T1.pulse') {\n"
    "  log 'T1.pulse: usleep 8000 x5'\n"
    "  5.times { usleep 8000; log 'T1.pulse: micro' }\n"
    "  log 'T1.pulse: sleep_ms 120 x2'\n"
    "  2.times { sleep_ms 120; log 'T1.pulse: chunk' }\n"
    "  log 'T1.pulse: sleep 0.3'\n"
    "  sleep 0.3\n"
    "  log 'T1.pulse: long done'\n"
    "}\n"
    "[30, 60, 90].each do |ms|\n"
    "  Task.new(name: 'T1.s' + ms.to_s) {\n"
    "    log 'T1.sleeper' + ms.to_s + ': sleeping'\n"
    "    sleep_ms ms\n"
    "    log 'T1.sleeper' + ms.to_s + ': woke'\n"
    "  }\n"
    "end\n"
    "Task.new(name: 'T1.spinner', priority: 200) {\n"
    "  log 'T1.spinner: entering tight loop'\n"
    "  loops = 0\n"
    "  loop {\n"
    "    loops += 1\n"
    "    break if $t1_done\n"
    "    break if loops > 200_000_000\n"
    "  }\n"
    "  log 'T1.spinner: exit loops=' + loops.to_s + ' done=' + $t1_done.to_s\n"
    "}\n"
    "Task.new(name: 'T1.stopper', priority: 50) {\n"
    "  log 'T1.stopper: sleeping 100 ms'\n"
    "  sleep 0.1\n"
    "  log 'T1.stopper: setting $t1_done'\n"
    "  $t1_done = true\n"
    "}\n"
  );

  timeout = g_timeout_source_new(700);
  g_source_set_callback(timeout, (GSourceFunc)g_main_loop_quit, loop, NULL);
  g_source_attach(timeout, ctx);
  g_source_unref(timeout);

  log_line("T1: entering g_main_loop_run (700 ms cap)");
  g_main_loop_run(loop);
  log_line("T1: g_main_loop_run returned");

  mrb_close(mrb);
  g_main_loop_unref(loop);
  g_main_context_pop_thread_default(ctx);
  g_main_context_unref(ctx);
}

/*
 * T2 -- mixed driver.
 *
 * Phase 1: register a small task set, call Task.run, which blocks
 * until those tasks drain. Phase 2: register more tasks and let
 * g_main_loop_run drive them.
 */
static gpointer
thread_glib_and_taskrun(gpointer data)
{
  GMainContext *ctx;
  GMainLoop    *loop;
  GSource      *timeout;
  mrb_state    *mrb;
  (void)data;

  ctx  = g_main_context_new();
  g_main_context_push_thread_default(ctx);
  loop = g_main_loop_new(ctx, FALSE);

  mrb = mrb_open();
  mrb_define_method(mrb, mrb->object_class, "log", rb_log, MRB_ARGS_REQ(1));

  banner("T2 (mix): phase 1 = yield + suspend/resume, drained by Task.run");

  run_ruby(mrb,
    "victim = Task.new(name: 'T2.victim') {\n"
    "  log 'T2.victim: sleeping 200 ms'\n"
    "  sleep 0.2\n"
    "  log 'T2.victim: woke'\n"
    "}\n"
    "Task.new(name: 'T2.controller', priority: 50) {\n"
    "  sleep 0.05\n"
    "  log 'T2.controller: suspending victim (was ' + victim.status.to_s + ')'\n"
    "  victim.suspend\n"
    "  log 'T2.controller: victim now ' + victim.status.to_s\n"
    "  sleep 0.1\n"
    "  log 'T2.controller: resuming victim (was ' + victim.status.to_s + ')'\n"
    "  victim.resume\n"
    "  log 'T2.controller: victim now ' + victim.status.to_s\n"
    "}\n"
    "Task.new(name: 'T2.yieldA', priority: 100) {\n"
    "  3.times { |i| log 'T2.yieldA: iter ' + i.to_s; Task.pass }\n"
    "}\n"
    "Task.new(name: 'T2.yieldB', priority: 100) {\n"
    "  3.times { |i| log 'T2.yieldB: iter ' + i.to_s; Task.pass }\n"
    "}\n"
    "log 'T2: calling Task.run (drains phase 1)'\n"
    "Task.run\n"
    "log 'T2: Task.run returned'\n"
  );

  banner("T2 (mix): phase 2 = 3 staggered sleepers, driven by g_main_loop_run");

  run_ruby(mrb,
    "[40, 80, 120].each do |ms|\n"
    "  Task.new(name: 'T2.s' + ms.to_s) {\n"
    "    log 'T2.sleeper' + ms.to_s + ': sleeping'\n"
    "    sleep_ms ms\n"
    "    log 'T2.sleeper' + ms.to_s + ': woke'\n"
    "  }\n"
    "end\n"
    "log 'T2: phase 2 registered'\n"
  );

  timeout = g_timeout_source_new(400);
  g_source_set_callback(timeout, (GSourceFunc)g_main_loop_quit, loop, NULL);
  g_source_attach(timeout, ctx);
  g_source_unref(timeout);

  log_line("T2: entering g_main_loop_run (400 ms cap)");
  g_main_loop_run(loop);
  log_line("T2: g_main_loop_run returned");

  mrb_close(mrb);
  g_main_loop_unref(loop);
  g_main_context_pop_thread_default(ctx);
  g_main_context_unref(ctx);
  return NULL;
}

/*
 * T3 -- Task.run only.
 *
 * No g_main_loop_run on the demo thread. Task.run drives the
 * scheduler; mrb_hal_task_idle_cpu iterates vm_ctx from inside
 * Task.run's idle loop so the ticker's cross-thread set_ready_time
 * still wakes the demo thread when sleepers come due. Returns when
 * all queues drain.
 */
static gpointer
thread_taskrun_only(gpointer data)
{
  GMainContext *ctx;
  mrb_state    *mrb;
  (void)data;

  ctx = g_main_context_new();
  g_main_context_push_thread_default(ctx);

  mrb = mrb_open();
  mrb_define_method(mrb, mrb->object_class, "log", rb_log, MRB_ARGS_REQ(1));

  banner("T3 (Task.run only): zombie/executioner + spinner/stopper + sleepers");

  run_ruby(mrb,
    "$t3_done = false\n"
    "$t3_zombie_ticks = 0\n"
    "zombie = Task.new(name: 'T3.zombie') {\n"
    "  log 'T3.zombie: alive (will tick every 50 ms forever)'\n"
    "  loop {\n"
    "    sleep_ms 50\n"
    "    $t3_zombie_ticks += 1\n"
    "    log 'T3.zombie: tick ' + $t3_zombie_ticks.to_s\n"
    "  }\n"
    "  log 'T3.zombie: NEVER REACHED'\n"
    "}\n"
    "Task.new(name: 'T3.executioner', priority: 50) {\n"
    "  sleep_ms 175\n"
    "  log 'T3.executioner: terminating zombie (was ' + zombie.status.to_s + ')'\n"
    "  zombie.terminate\n"
    "  log 'T3.executioner: zombie is now ' + zombie.status.to_s\n"
    "}\n"
    "Task.new(name: 'T3.spinner', priority: 200) {\n"
    "  log 'T3.spinner: entering tight loop'\n"
    "  loops = 0\n"
    "  loop {\n"
    "    loops += 1\n"
    "    break if $t3_done\n"
    "    break if loops > 200_000_000\n"
    "  }\n"
    "  log 'T3.spinner: exit loops=' + loops.to_s + ' done=' + $t3_done.to_s\n"
    "}\n"
    "Task.new(name: 'T3.stopper', priority: 50) {\n"
    "  log 'T3.stopper: sleeping 100 ms'\n"
    "  sleep 0.1\n"
    "  log 'T3.stopper: setting $t3_done'\n"
    "  $t3_done = true\n"
    "}\n"
    "[20, 40, 60].each do |ms|\n"
    "  Task.new(name: 'T3.s' + ms.to_s) {\n"
    "    log 'T3.sleeper' + ms.to_s + ': sleeping'\n"
    "    sleep_ms ms\n"
    "    log 'T3.sleeper' + ms.to_s + ': woke'\n"
    "  }\n"
    "end\n"
    "log 'T3: calling Task.run'\n"
    "Task.run\n"
    "log 'T3: Task.run returned (all queues empty)'\n"
  );

  mrb_close(mrb);
  g_main_context_pop_thread_default(ctx);
  g_main_context_unref(ctx);
  return NULL;
}

int main(int argc, char **argv)
{
  GThread *t2, *t3;
  (void)argc;
  (void)argv;

  start_us = g_get_monotonic_time();

  log_line("main: spawning T2 + T3; running T1 (glib-only) on main thread");

  t2 = g_thread_new("T2.mix",     thread_glib_and_taskrun, NULL);
  t3 = g_thread_new("T3.taskrun", thread_taskrun_only,     NULL);

  run_glib_only();

  g_thread_join(t2);
  g_thread_join(t3);

  log_line("main: T2 and T3 joined");
  printf("All scenarios completed.\n");
  return 0;
}
