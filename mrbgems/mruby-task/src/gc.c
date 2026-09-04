/*
** gc.c - GC.scheduler_driven family
**
** See Copyright Notice in mruby.h
*/

#include <mruby.h>
#include <mruby/gc.h>
#include <mruby/presym.h>

/* mrb_gc_scheduler_driven(), mrb_gc_scheduler_pending() and mrb_gc_step() (see
   mruby/gc.h) are core, embedder-generic primitives: any custom scheduler
   (this gem's mrb_task_run/mrb_task_run_once, or a from-scratch one) can
   drive GC from its own idle points without linking this gem. What's
   specific to mruby-task is the Ruby-level on/off switch below -- the
   gc->sched_driven flag it toggles is read by mrb_gc_scheduler_pending(), which
   in turn is only ever polled by this gem's scheduler loop.

   GC.debt_limit lives here too, not in core: its only trigger condition is
   "auto_step is off" (mrb_obj_alloc_core's safety valve), and the only way to
   ever turn auto_step off from Ruby is GC.scheduler_driven= below -- there is
   no GC.auto_step= at all. So unlike GC.step_limit/GC.malloc_threshold (which
   affect the ordinary auto_step-on path too), GC.debt_limit is inert without
   this gem. */

static mrb_value
gc_scheduler_driven_get(mrb_state *mrb, mrb_value obj)
{
  return mrb_bool_value(mrb->gc.sched_driven);
}

/*
 *  call-seq:
 *     GC.scheduler_driven = bool  -> bool
 *
 *  Hands GC scheduling to the task scheduler (true) or back to the allocation
 *  path (false). Enabling stops the allocation path from driving collection
 *  and disables generational mode; the scheduler then advances the collector
 *  from its idle points. Enabling raises while GC is disabled or ObjectSpace
 *  is iterating, and GC.generational_mode = true raises while this mode is
 *  on (a generational minor cycle is one atomic step, which would defeat it).
 *
 *  Disabling restores auto_step (ordinary allocation-synchronous GC) but
 *  does NOT restore generational mode -- that stays off until you explicitly
 *  call GC.generational_mode = true. See mrb_gc_scheduler_driven in
 *  mruby/gc.h.
 */
static mrb_value
gc_scheduler_driven_set(mrb_state *mrb, mrb_value obj)
{
  mrb_bool flag;

  mrb_get_args(mrb, "b", &flag);
  mrb_gc_scheduler_driven(mrb, flag);
  return mrb_bool_value(flag);
}

/*
 *  call-seq:
 *     GC.debt_limit -> int
 *
 *  Returns the scheduler-driven safety-valve threshold (0 = disabled). While
 *  GC.scheduler_driven is on, if the scheduler never goes idle (the system is
 *  100% busy) it never steps and the heap would grow without bound; once
 *  GC.debt exceeds this limit, the allocation path forces a bounded
 *  synchronous step instead. Bounds heap *growth*, not latency -- the forced
 *  step is a real synchronous pause, same as stock GC.
 */
static mrb_value
gc_debt_limit_get(mrb_state *mrb, mrb_value obj)
{
  return mrb_int_value(mrb, mrb->gc.debt_limit);
}

/*
 *  call-seq:
 *     GC.debt_limit = int -> int
 *
 *  Sets the scheduler-driven safety-valve threshold. See GC.debt_limit.
 */
static mrb_value
gc_debt_limit_set(mrb_state *mrb, mrb_value obj)
{
  mrb_int limit;

  mrb_get_args(mrb, "i", &limit);
  if (limit < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "debt_limit must be non-negative");
  }
  mrb->gc.debt_limit = limit;
  return mrb_int_value(mrb, limit);
}

void
mrb_init_task_gc(mrb_state *mrb)
{
  struct RClass *gc = mrb_module_get_id(mrb, MRB_SYM(GC));

  mrb_define_class_method_id(mrb, gc, MRB_SYM(scheduler_driven), gc_scheduler_driven_get, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM_E(scheduler_driven), gc_scheduler_driven_set, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, gc, MRB_SYM(debt_limit), gc_debt_limit_get, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM_E(debt_limit), gc_debt_limit_set, MRB_ARGS_REQ(1));
}
