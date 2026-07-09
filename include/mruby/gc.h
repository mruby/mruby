/**
** @file mruby/gc.h - garbage collector for mruby
**
** See Copyright Notice in mruby.h
*/

#ifndef MRUBY_GC_H
#define MRUBY_GC_H

#include "common.h"

/**
 * Uncommon memory management stuffs.
 */
MRB_BEGIN_DECL

#define MRB_EACH_OBJ_OK 0
#define MRB_EACH_OBJ_BREAK 1
typedef int (mrb_each_object_callback)(mrb_state *mrb, struct RBasic *obj, void *data);
void mrb_objspace_each_objects(mrb_state *mrb, mrb_each_object_callback *callback, void *data);
size_t mrb_objspace_page_slot_size(void);
MRB_API void mrb_free_context(mrb_state *mrb, struct mrb_context *c);

#ifndef MRB_GC_ARENA_SIZE
#define MRB_GC_ARENA_SIZE 100
#endif

#ifndef MRB_GRAY_STACK_SIZE
#define MRB_GRAY_STACK_SIZE 1024
#endif

typedef enum {
  MRB_GC_STATE_ROOT = 0,
  MRB_GC_STATE_MARK,
  MRB_GC_STATE_SWEEP
} mrb_gc_state;

#ifdef MRB_GC_PROFILE
/* Number of log2 duration buckets for the pause histogram: bucket i counts
   pauses whose microsecond duration has its highest set bit at position i,
   i.e. [2^(i-1), 2^i). Bucket 0 counts 0us pauses. 20 buckets reaches ~0.5s. */
#define MRB_GC_PROFILE_NBUCKETS 20
typedef struct mrb_gc_prof_hist {
  uint64_t count;
  uint64_t total_us;
  uint64_t max_us;
  uint32_t buckets[MRB_GC_PROFILE_NBUCKETS];
} mrb_gc_prof_hist;
#endif

typedef struct mrb_gc {
  struct mrb_heap_page *heaps;     /* all heaps pages */
  struct mrb_heap_page *free_heaps;/* heaps for allocation */
  struct mrb_heap_page *sweeps;    /* page where sweep starts */
  struct mrb_heap_region *regions;  /* contiguous heap regions */
  struct RBasic *gray_stack[MRB_GRAY_STACK_SIZE]; /* stack of gray objects */
  size_t gray_stack_top;           /* top index of gray stack */
  mrb_bool gray_overflow:1;        /* gray stack overflowed; needs heap rescan */
  size_t live;                     /* count of live objects */
  size_t live_after_mark;          /* old generation objects */
  mrb_int gc_debt;                 /* <0:credit, >0:needs GC */
  size_t oldgen_threshold;         /* threshold to kick major GC */
  mrb_gc_state state;              /* current state of gc */
  int interval_ratio;
  int step_ratio;
  int current_white_part :2;       /* make white object by white_part */
  mrb_bool iterating     :1;       /* currently iterating over objects */
  mrb_bool disabled      :1;       /* GC disabled */
  mrb_bool generational  :1;       /* generational GC mode */
  mrb_bool full          :1;       /* major GC mode */
  mrb_bool out_of_memory :1;       /* out-of-memory error occurred */
  mrb_bool collecting    :1;       /* mark/sweep engine is on the stack;
                                      suppresses reentrant emergency GC */
  mrb_bool auto_step     :1;       /* run GC steps on allocation (default on) */
  mrb_bool sched_driven  :1;       /* task scheduler drives GC when idle (see
                                      mrb_gc_scheduler_driven); implies auto_step off */
  size_t step_limit;               /* 0=unlimited, >0=absolute step cap */
  size_t malloc_increase;          /* malloc bytes since last GC cycle */
  size_t malloc_threshold;         /* 0=disabled, >0=bytes to trigger GC */
  mrb_int debt_limit;              /* 0=disabled; when auto_step off, force a
                                      sync step once gc_debt exceeds this */

#ifdef MRB_GC_FIXED_ARENA
  struct RBasic *arena[MRB_GC_ARENA_SIZE]; /* GC protection array */
#else
  struct RBasic **arena;                   /* GC protection array */
  int arena_capa;                          /* size of protection array */
#endif
  int arena_idx;

#ifdef MRB_GC_STATS
  uint32_t gc_total_count;                 /* total GC invocations */
  uint32_t minor_gc_count;                 /* minor GC count */
  uint32_t major_gc_count;                 /* major GC count */
#endif

#ifdef MRB_GC_PROFILE
  mrb_gc_prof_hist prof_sync;   /* synchronous mutator pauses (mrb_incremental_gc/mrb_full_gc) */
  mrb_gc_prof_hist prof_step;   /* scheduler-driven step pauses (work moved off the allocation path) */
  mrb_gc_prof_hist prof_step_jitter; /* jitter GC steps actually injected: the
                                        wall time of each step after which a task
                                        was already READY (the step delayed it).
                                        Upper-bounds the delay that task suffered.
                                        Fed by the scheduler via mrb_gc_scheduler_jitter. */
  uint64_t prof_last_step_us;   /* wall time of the most recent mrb_gc_step, so
                                   the scheduler can attribute it as jitter */
  mrb_gc_prof_hist *prof_target;/* histogram the outermost GC entry records into */
  int prof_depth;               /* reentrancy guard so nested GC calls record once */
  uint64_t prof_final_mark_max_us;   /* longest final_marking_phase (irreducible pause) */
  size_t   prof_final_mark_max_live; /* live count at that longest final marking */
  uint64_t prof_mark_work_total;     /* machine-independent: objects mark-scanned */
  uint64_t prof_sweep_work_total;    /* machine-independent: slots swept */
  uint32_t prof_emergency_count;     /* full GCs triggered by allocator OOM */
#endif
} mrb_gc;

MRB_API mrb_bool mrb_object_dead_p(mrb_state *mrb, struct RBasic *object);
MRB_API int mrb_gc_add_region(mrb_state *mrb, void *start, size_t size);

/* Task-scheduled GC control.
 *
 * mrb_gc_scheduler_driven(mrb, TRUE) hands GC scheduling to the task
 * scheduler: it turns auto_step off (so the allocation path stops driving
 * collection) and disables generational mode (a minor cycle otherwise runs to
 * completion inside one atomic step, defeating the point). The scheduler then
 * advances the collector from its idle points -- see mrb_task_run /
 * mrb_task_run_once. Enabling raises E_RUNTIME_ERROR while GC is disabled or
 * an ObjectSpace iteration is running, leaving the flags unchanged; while
 * scheduler-driven mode is on, re-enabling generational mode raises for the
 * same reason (a minor cycle is one atomic step). Passing FALSE restores
 * auto_step (standard mruby allocation-synchronous GC); it does not restore
 * generational mode.
 *
 * mrb_gc_scheduler_pending() reports whether, in scheduler-driven mode, there is
 * GC work worth doing right now (a cycle is in progress, collection is overdue
 * by object debt, or malloc-backed byte pressure has built up past
 * malloc_threshold). It is FALSE whenever scheduler-driven mode is off, so a
 * caller can gate on it without checking the mode itself.
 *
 * mrb_gc_step() advances the incremental collector by one unit of work and
 * returns the amount done (objects mark-scanned plus slots swept), or 0 while
 * GC is disabled or an ObjectSpace iteration is in progress. It runs
 * regardless of auto_step, so the task scheduler can drive collection while
 * automatic (allocation-time) stepping is off. Note for embedders driving
 * this directly: with generational mode still on, one call runs a whole
 * minor cycle atomically -- go through mrb_gc_scheduler_driven (or disable
 * generational mode first) to get finely divisible steps. */
MRB_API void mrb_gc_scheduler_driven(mrb_state *mrb, mrb_bool enable);
MRB_API mrb_bool mrb_gc_scheduler_pending(mrb_state *mrb);
MRB_API mrb_int mrb_gc_step(mrb_state *mrb);

/* Attribute the just-finished mrb_gc_step as jitter, if it delayed a task.
 * The scheduler calls this right after an idle-point mrb_gc_step, passing
 * whether a task became READY during that step. When it did, the step held the
 * CPU while a task was due to run, so the step's wall time upper-bounds the
 * jitter that task suffered; it is recorded into the prof_step_jitter
 * histogram. A no-op unless built with MRB_GC_PROFILE. */
MRB_API void mrb_gc_scheduler_jitter(mrb_state *mrb, mrb_bool delayed_task);

#define MRB_GC_RED 7

MRB_END_DECL

#endif  /* MRUBY_GC_H */
