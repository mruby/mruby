/*
** task_hal.c - GLib HAL for mruby-task
**
** See Copyright Notice in mruby.h
**
** Drives the mruby-task scheduler from an embedding application's GLib
** main loop. No Task.run is required from Ruby; the HAL fires
** mrb_task_run_once and mrb_tick automatically as the host loop iterates,
** which is the integration pattern a GTK or webview app uses in practice.
**
** Two GSources collaborate:
**
**   1. VM-run source on the thread-default GMainContext. Its callback
**      runs mrb_task_run_once on every registered VM.
**
**   2. Tick source on a dedicated GMainContext, iterated by a private
**      GThread. Its callback runs mrb_tick on every registered VM under
**      a recursive mutex. The separate thread is what gives us
**      preemption: it can fire mrb_tick while the VM thread is blocked
**      inside mrb_vm_exec.
**
** Both are manual GSources -- NULL prepare/check, dispatch driven purely
** by ready_time updates via g_source_set_ready_time. g_timeout_source's
** auto-reschedule would race with park-when-idle.
**
** State machine, evaluated after every dispatch under the IRQ lock:
**
**   has_ready (any q_ready_)  : vm_run = 0,                 tick = +1 interval
**   has_sleep (q_waiting_)    : vm_run = -1 (parked),       tick = soonest sleeper
**   neither                   : both = -1 (parked)
**
** In has_sleep state the ticker is the sole waker: it fires at the
** sleeper deadline, catches the scheduler clock up via mrb_tick, and
** sets vm_run = 0 once a task is promoted to ready. In neither state,
** mrb_task_enable_irq is the wake: any Ruby-side scheduler activity
** (Task.new from a bind callback, etc.) sets vm_run = 0 from outside.
**
** Tickless catch-up: in has_sleep state the ticker can be parked for
** arbitrarily long. On fire we compute (now - last_fire_us) + remainder,
** divide by MRB_TICK_INTERVAL_US, and call mrb_tick that many times in
** one go. The leftover < interval is carried in remainder_us to the
** next fire, keeping the scheduler clock aligned with monotonic time.
** Net effect: a loop of long sleeps costs one wakeup per sleep period.
**
** Threading:
**   - Per-thread state lives in heap-allocated mrb_task_thread_state,
**     reachable via thread-local `ts`. The ticker thread receives a
**     pointer at spawn; it never touches another thread's TLS.
**   - On the main thread no GMainContext push is needed; the default
**     context is used implicitly.
**   - On any other thread that opens an mrb_state, the caller MUST first
**     call g_main_context_push_thread_default(). This is the standard
**     GLib convention used by libsoup, GIO async, GTask, etc.
**
** Locking:
**   - irq_lock (GRecMutex) covers every mutation of mrb->task state.
**     mrb_task_disable_irq / mrb_task_enable_irq are lock / unlock,
**     with the enable side additionally setting vm_run = 0.
**   - The ticker holds the lock across its mrb_tick batch.
**   - arm_locked runs with the lock held so a concurrent ticker can't
**     promote a sleeper between our inspection and our arm decision.
**   - mrb_vm_exec runs WITHOUT the lock; the ticker can preempt it
**     mid-execution by setting switching_.
**
** Supported platforms: any system with GLib 2.x and GThread (Linux,
** BSD, macOS, Windows with MinGW or MSVC, ...).
*/

#include <mruby.h>
#include <mruby/error.h>
#include "task.h"
#include "task_hal.h"
#include <glib.h>
#include <stdint.h>
#include <string.h>

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
# define MRB_TASK_TLS _Thread_local
#elif defined(__GNUC__) || defined(__clang__) || defined(__SUNPRO_C) || defined(__xlC__) || defined(__IBMC__)
# define MRB_TASK_TLS __thread
#elif defined(_MSC_VER) || defined(__BORLANDC__)
# define MRB_TASK_TLS __declspec(thread)
#else
# error "mruby-task GLib HAL: no thread-local storage qualifier known for this compiler"
#endif

#define MRB_TICK_INTERVAL_US ((gint64)MRB_TICK_UNIT * 1000)

typedef struct mrb_task_thread_state {
  mrb_state    *vm_list[MRB_TASK_MAX_VMS];
  int           vm_count;
  GRecMutex     irq_lock;

  GMainContext *vm_ctx;
  GSource      *vm_run_src;

  GMainContext *tick_ctx;
  GMainLoop    *tick_loop;
  GSource      *tick_src;
  GThread      *ticker;

  /* Tickless catch-up: last_fire_us is the monotonic anchor; remainder_us
   * is the sub-interval carry from the previous fire. */
  gint64        last_fire_us;
  uint32_t      remainder_us;
} mrb_task_thread_state;

static MRB_TASK_TLS mrb_task_thread_state *ts;

static gboolean
deadline_source_dispatch(GSource *source, GSourceFunc callback, gpointer user_data)
{
  (void)source;
  if (!callback) {
    return G_SOURCE_REMOVE;
  }
  return callback(user_data);
}

static GSourceFuncs deadline_source_funcs = {
  NULL, NULL, deadline_source_dispatch, NULL, NULL, NULL,
};

static void
free_thread_state(mrb_task_thread_state *s)
{
  if (!s) {
    return;
  }
  if (s->ticker) {
    if (s->tick_loop) {
      g_main_loop_quit(s->tick_loop);
    }
    (void)g_thread_join(s->ticker);
    s->ticker = NULL;
  }
  if (s->tick_src) {
    g_source_destroy(s->tick_src);
    g_source_unref(s->tick_src);
    s->tick_src = NULL;
  }
  if (s->tick_loop) {
    g_main_loop_unref(s->tick_loop);
    s->tick_loop = NULL;
  }
  if (s->tick_ctx) {
    g_main_context_unref(s->tick_ctx);
    s->tick_ctx = NULL;
  }
  if (s->vm_run_src) {
    g_source_destroy(s->vm_run_src);
    g_source_unref(s->vm_run_src);
    s->vm_run_src = NULL;
  }
  if (s->vm_ctx) {
    g_main_context_unref(s->vm_ctx);
    s->vm_ctx = NULL;
  }
  g_rec_mutex_clear(&s->irq_lock);
  g_free(s);
}

/*
 * Walk every VM's queues, decide the arm state, apply it. Called with
 * the IRQ lock held. q_waiting_ is walked directly to find the soonest
 * SLEEP-reason wakeup; this is authoritative regardless of what
 * mrb->task.wakeup_tick currently reads.
 *
 * Idempotent. Deadlines are computed relative to s->last_fire_us (the
 * last actual ticker fire) rather than g_get_monotonic_time(), so
 * repeated calls between fires yield the same ready_time and don't
 * drift the cadence. set_ready_time is skipped when the value
 * wouldn't change, to avoid the eventfd-write side effect.
 */
static void
arm_locked(mrb_task_thread_state *s)
{
  gint64    monotonic_now    = g_get_monotonic_time();
  int32_t   min_wake_offset  = 0;
  gboolean  has_ready        = FALSE;
  gboolean  has_sleep        = FALSE;
  gint64    new_vm_ready;
  gint64    new_tick_ready;
  gint64    cur_vm_ready;
  gint64    cur_tick_ready;
  int i;

  for (i = 0; i < s->vm_count; i++) {
    mrb_state *vm = s->vm_list[i];
    if (!vm) continue;

    if (vm->task.queues[MRB_TASK_QUEUE_READY] != NULL) {
      has_ready = TRUE;
      continue;
    }

    mrb_task *w = vm->task.queues[MRB_TASK_QUEUE_WAITING];
    while (w) {
      if (w->reason == MRB_TASK_REASON_SLEEP ||
          (w->reason == MRB_TASK_REASON_QUEUE &&
           w->wait.queue.wakeup_tick != UINT32_MAX)) {
        uint32_t wake = w->reason == MRB_TASK_REASON_SLEEP ?
                        w->wait.wakeup_tick : w->wait.queue.wakeup_tick;
        uint32_t tick   = vm->task.tick;
        int32_t  offset = (int32_t)(wake - tick);
        if (!has_sleep || offset < min_wake_offset) {
          min_wake_offset = offset;
          has_sleep       = TRUE;
        }
      }
      w = w->next;
    }
  }

  /* Anchor the catch-up clock when the ticker transitions from parked
   * to active, so elapsed time counts from "now" rather than from
   * before the park. */
  if ((has_ready || has_sleep) &&
      g_source_get_ready_time(s->tick_src) == -1) {
    s->last_fire_us = monotonic_now;
    s->remainder_us = 0;
  }

  if (has_ready) {
    new_vm_ready   = 0;
    new_tick_ready = s->last_fire_us + MRB_TICK_INTERVAL_US;
  }
  else if (has_sleep) {
    new_vm_ready = -1;
    if (min_wake_offset <= 0) {
      new_tick_ready = 0;  /* overdue, fire immediately */
    }
    else {
      new_tick_ready = s->last_fire_us +
                       (gint64)min_wake_offset * MRB_TICK_INTERVAL_US;
    }
  }
  else {
    new_vm_ready   = -1;
    new_tick_ready = -1;
  }

  cur_vm_ready   = g_source_get_ready_time(s->vm_run_src);
  cur_tick_ready = g_source_get_ready_time(s->tick_src);

  if (new_vm_ready != cur_vm_ready) {
    g_source_set_ready_time(s->vm_run_src, new_vm_ready);
  }
  if (new_tick_ready != cur_tick_ready) {
    g_source_set_ready_time(s->tick_src, new_tick_ready);
  }
}

static gpointer
ticker_thread(gpointer data)
{
  mrb_task_thread_state *s = (mrb_task_thread_state *)data;
  g_main_context_push_thread_default(s->tick_ctx);
  g_main_loop_run(s->tick_loop);
  g_main_context_pop_thread_default(s->tick_ctx);
  return NULL;
}

/*
 * Tick GSource callback on the ticker thread. Computes catch-up ticks
 * from elapsed monotonic time plus carried remainder, calls mrb_tick
 * that many times under irq_lock, then lets arm_locked decide the
 * next state (steady-state cadence vs tickless deadline vs full park).
 */
static gboolean
tick_source_cb(gpointer user_data)
{
  mrb_task_thread_state *s = (mrb_task_thread_state *)user_data;
  int i;
  gint64 now = g_get_monotonic_time();
  gint64 total_us = (now - s->last_fire_us) + (gint64)s->remainder_us;
  gint64 raw_ticks = total_us / (gint64)MRB_TICK_INTERVAL_US;
  uint32_t catch_up_ticks = (raw_ticks > (gint64)UINT32_MAX)
                              ? UINT32_MAX
                              : (uint32_t)raw_ticks;

  s->remainder_us = (uint32_t)(total_us -
                               (gint64)catch_up_ticks * (gint64)MRB_TICK_INTERVAL_US);
  s->last_fire_us = now;

  g_rec_mutex_lock(&s->irq_lock);
  for (i = 0; i < s->vm_count; i++) {
    mrb_state *vm = s->vm_list[i];
    if (!vm) continue;
    for (uint32_t k = 0; k < catch_up_ticks; k++) {
      mrb_tick(vm);
    }
  }
  arm_locked(s);
  g_rec_mutex_unlock(&s->irq_lock);

  return G_SOURCE_CONTINUE;
}

/*
 * VM-run GSource callback on the VM thread. Snapshots vm_list under
 * the IRQ lock, runs mrb_task_run_once on each entry outside the
 * lock, then re-acquires to decide the next arm state. The snapshot
 * protects against list-shape changes while we're iterating (e.g., a
 * task body that registers or removes another mrb_state on this
 * thread).
 */
static gboolean
vm_run_source_cb(gpointer user_data)
{
  mrb_state *snapshot[MRB_TASK_MAX_VMS];
  int snapshot_count;
  int i;
  (void)user_data;

  if (!ts) {
    return G_SOURCE_CONTINUE;
  }

  g_rec_mutex_lock(&ts->irq_lock);
  snapshot_count = ts->vm_count;
  memcpy(snapshot, ts->vm_list, (size_t)snapshot_count * sizeof(mrb_state *));
  g_rec_mutex_unlock(&ts->irq_lock);

  for (i = 0; i < snapshot_count; i++) {
    if (snapshot[i]) {
      (void)mrb_task_run_once(snapshot[i]);
    }
  }

  g_rec_mutex_lock(&ts->irq_lock);
  arm_locked(ts);
  g_rec_mutex_unlock(&ts->irq_lock);

  return G_SOURCE_CONTINUE;
}

void
mrb_hal_task_init(mrb_state *mrb)
{
  int       i;
  int       idx = -1;
  gboolean  first_on_thread = FALSE;
  guint     attach_id;
  GError   *err = NULL;
  gchar    *err_msg;

  for (i = 0; i < MRB_NUM_TASK_QUEUE; i++) {
    mrb->task.queues[i] = NULL;
  }
  mrb->task.tick = 0;
  mrb->task.wakeup_tick = UINT32_MAX;
  mrb->task.switching = FALSE;

  if (ts == NULL) {
    ts = g_new0(mrb_task_thread_state, 1);
    g_rec_mutex_init(&ts->irq_lock);
    ts->last_fire_us = g_get_monotonic_time();
    first_on_thread = TRUE;
  }

  g_rec_mutex_lock(&ts->irq_lock);

  for (i = 0; i < ts->vm_count; i++) {
    if (ts->vm_list[i] == mrb) {
      idx = i;
      break;
    }
  }

  if (idx < 0) {
    if (ts->vm_count >= MRB_TASK_MAX_VMS) {
      g_rec_mutex_unlock(&ts->irq_lock);
      if (first_on_thread) {
        free_thread_state(ts);
        ts = NULL;
      }
      mrb_raisef(mrb, E_RUNTIME_ERROR,
                 "too many mrb_states with task scheduler on this thread "
                 "(max: %d)",
                 MRB_TASK_MAX_VMS);
    }
    ts->vm_list[ts->vm_count++] = mrb;
  }

  g_rec_mutex_unlock(&ts->irq_lock);

  if (first_on_thread) {
    ts->vm_ctx = g_main_context_ref_thread_default();
    g_assert_nonnull(ts->vm_ctx);

    ts->vm_run_src = g_source_new(&deadline_source_funcs, sizeof(GSource));
    g_assert_nonnull(ts->vm_run_src);
    g_source_set_callback(ts->vm_run_src, vm_run_source_cb, NULL, NULL);
    g_source_set_ready_time(ts->vm_run_src, -1);

    attach_id = g_source_attach(ts->vm_run_src, ts->vm_ctx);
    if (attach_id == 0) {
      free_thread_state(ts);
      ts = NULL;
      mrb_raise(mrb, E_RUNTIME_ERROR,
                "mruby-task GLib HAL: g_source_attach failed for VM-run source");
    }

    ts->tick_ctx = g_main_context_new();
    g_assert_nonnull(ts->tick_ctx);

    ts->tick_loop = g_main_loop_new(ts->tick_ctx, FALSE);
    g_assert_nonnull(ts->tick_loop);

    ts->tick_src = g_source_new(&deadline_source_funcs, sizeof(GSource));
    g_assert_nonnull(ts->tick_src);
    g_source_set_callback(ts->tick_src, tick_source_cb, ts, NULL);
    g_source_set_ready_time(ts->tick_src, -1);

    attach_id = g_source_attach(ts->tick_src, ts->tick_ctx);
    if (attach_id == 0) {
      free_thread_state(ts);
      ts = NULL;
      mrb_raise(mrb, E_RUNTIME_ERROR,
                "mruby-task GLib HAL: g_source_attach failed for tick source");
    }

    ts->ticker = g_thread_try_new("mruby-task-tick", ticker_thread, ts, &err);
    if (ts->ticker == NULL) {
      /* Copy GLib's error message into a stack buffer before any mruby
       * allocation, so mrb_raise's longjmp can't strand the GLib heap. */
      char buf[256];
      err_msg = g_strdup_printf(
          "mruby-task GLib HAL: failed to spawn ticker thread: %s",
          err ? err->message : "unknown error");
      g_strlcpy(buf, err_msg, sizeof(buf));
      g_free(err_msg);
      if (err) {
        g_error_free(err);
      }
      free_thread_state(ts);
      ts = NULL;
      mrb_raise(mrb, E_RUNTIME_ERROR, buf);
    }
  }
}

void
mrb_hal_task_final(mrb_state *mrb)
{
  int i, j;
  gboolean last_on_thread = FALSE;

  if (ts == NULL) {
    return;
  }

  g_rec_mutex_lock(&ts->irq_lock);

  for (i = 0; i < ts->vm_count; i++) {
    if (ts->vm_list[i] == mrb) {
      for (j = i; j < ts->vm_count - 1; j++) {
        ts->vm_list[j] = ts->vm_list[j + 1];
      }
      ts->vm_list[ts->vm_count - 1] = NULL;
      ts->vm_count--;
      break;
    }
  }

  if (ts->vm_count == 0) {
    last_on_thread = TRUE;
  }

  g_rec_mutex_unlock(&ts->irq_lock);

  if (last_on_thread) {
    free_thread_state(ts);
    ts = NULL;
  }
}

void
mrb_task_disable_irq(void)
{
  if (ts) {
    g_rec_mutex_lock(&ts->irq_lock);
  }
}

/* Hooked into the scheduler's IRQ-release path. After any state
 * change, re-evaluate the arm state of both sources via arm_locked.
 * This is what wires up preemption: arm_locked sets tick_src's
 * ready_time so the ticker thread fires mrb_tick on cadence.
 *
 * Without this, only vm_run_source_cb (the foreign-loop dispatch
 * callback) ever calls arm_locked, so a Task.run-driven scheduler
 * never arms the ticker -- preemption never happens, sleepers in
 * q_waiting_ are never woken, and CPU-bound tasks spin forever.
 * Calling arm_locked here covers both Task.run and foreign-loop
 * drivers symmetrically.
 *
 * arm_locked also sets vm_run_src ready_time to 0 only when there's
 * a ready task, which prevents the spurious wake during mrb_task_run's
 * idle queue check (the disable_irq/check/enable_irq pattern over a
 * read-only check produces no state change, so vm_run_src stays
 * parked at -1). */
void
mrb_task_enable_irq(void)
{
  if (!ts) {
    return;
  }
  arm_locked(ts);
  g_rec_mutex_unlock(&ts->irq_lock);
}

/* Called only from mrb_task_run's idle loop. Iterates the VM context
 * to dispatch any pending vm_run_src (e.g., the ticker just woke us
 * because a sleeper became ready), then returns. Block-wait is OK now
 * because mrb_task_enable_irq's q_ready_ guard prevents the spurious-
 * wake loop that would otherwise spin this iteration. */
void
mrb_hal_task_idle_cpu(mrb_state *mrb)
{
  (void)mrb;
  if (ts && ts->vm_ctx) {
    (void)g_main_context_iteration(ts->vm_ctx, TRUE);
  }
  else {
    g_usleep(MRB_TICK_UNIT * 1000);
  }
}

void
mrb_hal_task_switch_hook(mrb_state *mrb, mrb_task_switch_reason reason)
{
  (void)mrb;
  (void)reason;
  /* Nothing to service on this platform */
}

void
mrb_hal_task_sleep_us(mrb_state *mrb, mrb_int usec)
{
  (void)mrb;
  if (usec <= 0) {
    return;
  }
  g_usleep((gulong)usec);
}
