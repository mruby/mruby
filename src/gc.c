/*
** gc.c - garbage collector for mruby
**
** See Copyright Notice in mruby.h
*/

#include <string.h>
#ifdef MRB_USE_MALLOC_TRIM
#include <malloc.h>
#endif
#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/istruct.h>
#include <mruby/hash.h>
#include <mruby/proc.h>
#include <mruby/range.h>
#include <mruby/string.h>
#include <mruby/variable.h>
#include <mruby/gc.h>
#include <mruby/error.h>
#include <mruby/throw.h>
#include <mruby/internal.h>

#ifdef MRB_GC_STRESS
#include <stdlib.h>
#endif

#ifdef MRB_USE_TASK_SCHEDULER
/* Forward declaration - actual implementation in task.c */
void mrb_task_mark_all(mrb_state *mrb);
#endif

#ifdef MRB_GC_PROFILE
#include <time.h>
#include <stdio.h>

/* Monotonic microsecond clock for pause measurement. Returns 0 where no
   monotonic clock is available, which degrades the histogram gracefully
   (durations collapse to bucket 0) rather than breaking the build. */
static uint64_t
gc_prof_now_us(void)
{
#if defined(CLOCK_MONOTONIC)
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) == 0) {
    return (uint64_t)ts.tv_sec * 1000000u + (uint64_t)ts.tv_nsec / 1000u;
  }
#endif
  return 0;
}

static void
gc_prof_record(mrb_gc_prof_hist *h, uint64_t us)
{
  unsigned i = 0;
  uint64_t v = us;
  h->count++;
  h->total_us += us;
  if (us > h->max_us) h->max_us = us;
  while (v > 0 && i < MRB_GC_PROFILE_NBUCKETS - 1) { v >>= 1; i++; }
  h->buckets[i]++;
}

/* Reentrancy-guarded pause timing: only the outermost GC entry starts the
   clock and picks the target histogram, so a nested mrb_full_gc (e.g. major
   GC escalation during a step) is attributed to whichever entry the mutator
   actually called, not double-counted. Returns the start time; pass it back
   to gc_prof_leave. */
static uint64_t
gc_prof_enter(mrb_gc *gc, mrb_gc_prof_hist *target)
{
  if (gc->prof_depth++ == 0) {
    gc->prof_target = target;
    return gc_prof_now_us();
  }
  return 0;
}

static uint64_t
gc_prof_leave(mrb_gc *gc, uint64_t t0)
{
  if (--gc->prof_depth == 0 && gc->prof_target) {
    uint64_t dt = gc_prof_now_us() - t0;
    gc_prof_record(gc->prof_target, dt);
    return dt;
  }
  return 0;
}
#endif

/*
  = Tri-color Incremental Garbage Collection

  mruby's GC is Tri-color Incremental GC with Mark & Sweep.
  Algorithm details are omitted.
  Instead, the implementation part is described below.

  == Object's Color

  Each object can be painted in three colors:

    * White - Unmarked.
    * Gray - Marked, But the child objects are unmarked.
    * Black - Marked, the child objects are also marked.

  Extra color

    * Red - Static (ROM object) no need to be collected.
          - All child objects should be Red as well.

  == Two White Types

  There are two white color types in a flip-flop fashion: White-A and White-B,
  which respectively represent the Current White color (the newly allocated
  objects in the current GC cycle) and the Sweep Target White color (the
  dead objects to be swept).

  A and B will be switched just at the beginning of the next GC cycle. At
  that time, all the dead objects have been swept, while the newly created
  objects in the current GC cycle which finally remains White are now
  regarded as dead objects. Instead of traversing all the White-A objects and
  painting them as White-B, just switch the meaning of White-A and White-B as
  this will be much cheaper.

  As a result, the objects we sweep in the current GC cycle are always
  left from the previous GC cycle. This allows us to sweep objects
  incrementally, without the disturbance of the newly created objects.

  == Execution Timing

  GC Execution Time and Each step interval are decided by live objects count.
  List of Adjustment API:

    * gc_interval_ratio_set
    * gc_step_ratio_set

  For details, see the comments for each function.

  == Write Barrier

  mruby implementer and C extension library writer must insert a write
  barrier when updating a reference from a field of an object.
  When updating a reference from a field of object A to object B,
  two different types of write barrier are available:

    * mrb_field_write_barrier - target B object for a mark.
    * mrb_write_barrier       - target A object for a mark.

  == Generational Mode

  mruby's GC offers an Generational Mode while reusing the tri-color GC
  infrastructure. It will treat the Black objects as Old objects after each
  sweep phase, instead of painting them White. The key ideas are still the same
  as traditional generational GC:

    * Minor GC - just traverse the Young objects (Gray objects) in the mark
                 phase, then only sweep the newly created objects, and leave
                 the Old objects live.

    * Major GC - same as a full regular GC cycle.

  The difference from "traditional" generational GC is, that the major GC
  in mruby is triggered incrementally in a tri-color manner.


  For details, see the comments for each function.

*/

typedef struct RVALUE RVALUE;

struct free_obj {
  MRB_OBJECT_HEADER;
  RVALUE *next;
};

struct RVALUE_initializer {
  MRB_OBJECT_HEADER;
#if defined(MRB_WORD_BOXING) && defined(MRB_32BIT) && defined(MRB_USE_FLOAT32) && !defined(MRB_WORDBOX_NO_INLINE_FLOAT)
  /* inline float word boxing needs 8-byte aligned objects;
     pad RVALUE to 24 bytes (multiple of 8) on 32-bit */
  char padding[sizeof(void*) * 4];
#else
  char padding[sizeof(void*) * 3];
#endif
};

struct RVALUE {
  union {
    struct RVALUE_initializer init;  /* must be first member to ensure initialization */
    struct free_obj free;
    struct RBasic basic;
    struct RObject object;
    struct RClass klass;
#if defined(MRB_WORD_BOXING) || (defined(MRB_NAN_BOXING) && defined(MRB_INT64))
    struct RInteger integer;
#endif
#if defined(MRB_WORD_BOXING) && !defined(MRB_NO_FLOAT) && defined(MRB_WORDBOX_NO_INLINE_FLOAT)
    struct RFloat flt;
#endif
    struct RString string;
    struct RArray array;
    struct RHash hash;
    struct RRange range;
    struct RData data;
    struct RIStruct istruct;
    struct RProc proc;
    struct REnv env;
    struct RFiber fiber;
    struct RException exc;
    struct RBreak brk;
  } as;
};

#ifdef GC_DEBUG
#define DEBUG(x) (x)
#else
#define DEBUG(x)
#endif

#ifndef MRB_HEAP_PAGE_SIZE
#define MRB_HEAP_PAGE_SIZE 1024
#endif

typedef struct mrb_heap_page {
  RVALUE *freelist;
  struct mrb_heap_page *next;
  struct mrb_heap_page *free_next;
  mrb_bool old:1;
  mrb_bool region:1;             /* from contiguous region, not malloc */
  RVALUE objects[MRB_HEAP_PAGE_SIZE];
} mrb_heap_page;

typedef struct mrb_heap_region {
  struct mrb_heap_region *next;
  uint8_t *base;                 /* start of user buffer */
  size_t size;                   /* buffer size in bytes */
  uint16_t page_count;           /* pages carved from region */
} mrb_heap_region;

#define GC_STEP_SIZE 1024

/* white: 001 or 010, black: 100, gray: 000, red:111 */
#define GC_GRAY 0
#define GC_WHITE_A 1
#define GC_WHITE_B 2
#define GC_BLACK   4
#define GC_RED MRB_GC_RED
#define GC_WHITES (GC_WHITE_A | GC_WHITE_B)
#define GC_COLOR_MASK 7
mrb_static_assert(MRB_GC_RED <= GC_COLOR_MASK);

#define paint_gray(o) ((o)->gc_color = GC_GRAY)
#define paint_black(o) ((o)->gc_color = GC_BLACK)
#define paint_white(o) ((o)->gc_color = GC_WHITES)
#define paint_partial_white(s, o) ((o)->gc_color = (s)->current_white_part)
#define is_gray(o) ((o)->gc_color == GC_GRAY)
#define is_white(o) ((o)->gc_color & GC_WHITES)
#define is_black(o) ((o)->gc_color == GC_BLACK)
#define is_red(o) ((o)->gc_color == GC_RED)
#define flip_white_part(s) ((s)->current_white_part = other_white_part(s))
#define other_white_part(s) ((s)->current_white_part ^ GC_WHITES)
#define is_dead(s, o) (((o)->gc_color & other_white_part(s) & GC_WHITES) || (o)->tt == MRB_TT_FREE)

mrb_noreturn void mrb_raise_nomemory(mrb_state *mrb);

static size_t incremental_gc_finish(mrb_state *mrb, mrb_gc *gc);
static size_t incremental_gc_run(mrb_state *mrb, mrb_gc *gc);

MRB_API void*
mrb_realloc_simple(mrb_state *mrb, void *p,  size_t len)
{
  void *p2;

#if defined(MRB_GC_STRESS) && defined(MRB_DEBUG)
  if (mrb->gc.state != MRB_GC_STATE_SWEEP) {
    mrb_full_gc(mrb);
  }
#endif
  p2 = mrb_basic_alloc_func(p, len);
  if (!p2 && len > 0 && mrb->gc.heaps && !mrb->gc.collecting &&
      !mrb->gc.disabled && !mrb->gc.iterating) {
    /* collecting == FALSE means no mark/sweep is running on the stack, so
       this failure is a mutator allocation, not one from inside the GC
       engine (e.g. an RData dfree during sweep). Recovery runs only here; a
       reentrant failure falls through to raise NoMemoryError as before.
       gc_drive() sets collecting. disabled/iterating are checked here too so
       the retry (and the emergency counter) only happen when recovery can
       actually run. */
#ifdef MRB_GC_PROFILE
    mrb->gc.prof_emergency_count++;
#endif
    if (mrb->gc.state == MRB_GC_STATE_SWEEP) {
      /* Mid-sweep: starting a new mark cycle here is unsafe, but finishing
         the in-progress sweep is safe and is exactly what reclaims memory.
         Without this an allocation failure while parked in SWEEP raised
         NoMemoryError even though free slots were about to be produced.
         Also matters when GC steps are driven off the allocation path
         (auto_step off) and the heap can be parked in SWEEP for a while. */
      incremental_gc_finish(mrb, &mrb->gc);
    }
    else {
      mrb_full_gc(mrb);
    }
    p2 = mrb_basic_alloc_func(p, len);
  }

  if (p2 && len > 0) {
    mrb->gc.malloc_increase += len;
    if (p == NULL &&
        mrb->gc.malloc_threshold > 0 &&
        mrb->gc.malloc_increase >= mrb->gc.malloc_threshold &&
        mrb->gc.state == MRB_GC_STATE_ROOT &&
        !mrb->gc.disabled && !mrb->gc.iterating && mrb->gc.auto_step) {
      /* Only a fresh allocation (p == NULL) may drive the collector here. A
         realloc (p != NULL) has just freed the caller's old block, but the
         caller has not yet stored the returned pointer back into the object it
         belongs to -- e.g. ht_adjust_ea() does `ea = ea_resize(...)` and only
         then `ht_set_ea(h, ea)`, and ary_expand_capa() likewise. Running an
         incremental mark in that window would mark the still-reachable
         container (Hash/Array/String/...) while it holds the dangling old
         pointer, a use-after-free. A fresh allocation frees nothing the caller
         references, so it is a safe point to step GC. Byte pressure from
         reallocs is not lost: malloc_increase keeps accumulating above and
         fires at the next fresh allocation.

         auto_step is part of the condition (not just relied on inside
         mrb_incremental_gc) so malloc_increase is not cleared for a call
         that would no-op -- the GC task reads malloc_increase as a byte
         pressure signal. */
      mrb->gc.malloc_increase = 0;
      mrb_incremental_gc(mrb);
    }
  }

  return p2;
}

MRB_API void*
mrb_realloc(mrb_state *mrb, void *p, size_t len)
{
  void *p2;

  p2 = mrb_realloc_simple(mrb, p, len);
  if (len == 0) return p2;
  if (p2 == NULL) {
    mrb->gc.out_of_memory = TRUE;
    mrb_raise_nomemory(mrb);
  }
  else {
    mrb->gc.out_of_memory = FALSE;
  }

  return p2;
}

MRB_API void*
mrb_malloc(mrb_state *mrb, size_t len)
{
  return mrb_realloc(mrb, 0, len);
}

MRB_API void*
mrb_malloc_simple(mrb_state *mrb, size_t len)
{
  return mrb_realloc_simple(mrb, 0, len);
}

MRB_API void*
mrb_calloc(mrb_state *mrb, size_t nelem, size_t len)
{
  void *p;

  if (nelem == 0 || len == 0) {
    p = NULL;
  }
  else if (nelem <= SIZE_MAX / len) {
    size_t size = nelem * len;
    p = mrb_malloc(mrb, size);

    memset(p, 0, size);
  }
  else {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "memory allocation overflow");
  }

  return p;
}

MRB_API void
mrb_free(mrb_state *mrb, void *p)
{
  mrb_basic_alloc_func(p, 0);
}

MRB_API void*
mrb_temp_alloc(mrb_state *mrb, size_t size)
{
  struct RString *s;
  s = MRB_OBJ_ALLOC(mrb, MRB_TT_STRING, NULL);
  return s->as.heap.ptr = (char*)mrb_malloc(mrb, size);
}

static mrb_bool
heap_p(mrb_gc *gc, const struct RBasic *object)
{
  mrb_heap_page* page;
  mrb_heap_region *region;

  /* fast path: check contiguous regions via arithmetic */
  for (region = gc->regions; region; region = region->next) {
    uintptr_t addr = (uintptr_t)object;
    uintptr_t base = (uintptr_t)region->base;
    uintptr_t end = base + (size_t)region->page_count * sizeof(mrb_heap_page);
    if (addr >= base && addr < end) {
      return TRUE;
    }
  }

  page = gc->heaps;
  while (page) {
    RVALUE *p;

    p = page->objects;
    if ((uintptr_t)object - (uintptr_t)p <= (MRB_HEAP_PAGE_SIZE - 1) * sizeof(RVALUE)) {
      return TRUE;
    }
    page = page->next;
  }
  return FALSE;
}

MRB_API mrb_bool
mrb_object_dead_p(mrb_state *mrb, struct RBasic *object)
{
  mrb_gc *gc = &mrb->gc;
  if (!heap_p(gc, object)) return TRUE;
  return is_dead(gc, object);
}

static void
link_heap_page(mrb_gc *gc, mrb_heap_page *page)
{
  page->next = gc->heaps;
  gc->heaps = page;
  page->free_next = gc->free_heaps;
  gc->free_heaps = page;
}

static void
init_heap_page(mrb_heap_page *page)
{
  RVALUE *p, *e;
  RVALUE *prev = NULL;

  for (p = page->objects, e=p+MRB_HEAP_PAGE_SIZE; p<e; p++) {
    p->as.free.tt = MRB_TT_FREE;
    p->as.free.next = prev;
    prev = p;
  }
  page->freelist = prev;
}

static void
add_heap(mrb_state *mrb, mrb_gc *gc)
{
  mrb_heap_page *page = (mrb_heap_page*)mrb_calloc(mrb, 1, sizeof(mrb_heap_page));
  init_heap_page(page);
  link_heap_page(gc, page);
}

MRB_API int
mrb_gc_add_region(mrb_state *mrb, void *start, size_t size)
{
  mrb_gc *gc = &mrb->gc;
  uint8_t *base = (uint8_t*)start;
  mrb_heap_region *region;
  uint16_t page_count;
  uint16_t i;

  /* align base to pointer size */
  uintptr_t align = sizeof(void*);
  uintptr_t offset = ((uintptr_t)base + align - 1) & ~(align - 1);
  size -= (size_t)(offset - (uintptr_t)base);
  base = (uint8_t*)offset;

  page_count = (uint16_t)(size / sizeof(mrb_heap_page));
  if (page_count == 0) return 0;

  region = (mrb_heap_region*)mrb_malloc(mrb, sizeof(mrb_heap_region));
  region->base = base;
  region->size = size;
  region->page_count = page_count;
  region->next = gc->regions;
  gc->regions = region;

  /* carve pages from the contiguous buffer */
  for (i = 0; i < page_count; i++) {
    mrb_heap_page *page = (mrb_heap_page*)(base + (size_t)i * sizeof(mrb_heap_page));
    memset(page, 0, sizeof(mrb_heap_page));
    page->region = TRUE;
    init_heap_page(page);
    link_heap_page(gc, page);
  }
  return page_count;
}

#define DEFAULT_GC_INTERVAL_RATIO 200
#define DEFAULT_GC_STEP_RATIO 200
#define MAJOR_GC_INC_RATIO 120
#define MAJOR_GC_TOOMANY 10000
#define is_generational(gc) ((gc)->generational)
#define is_major_gc(gc) (is_generational(gc) && (gc)->full)
#define is_minor_gc(gc) (is_generational(gc) && !(gc)->full)

void
mrb_gc_init(mrb_state *mrb, mrb_gc *gc)
{
#ifndef MRB_GC_FIXED_ARENA
  gc->arena = (struct RBasic**)mrb_malloc(mrb, sizeof(struct RBasic*)*MRB_GC_ARENA_SIZE);
  gc->arena_capa = MRB_GC_ARENA_SIZE;
#endif

  gc->current_white_part = GC_WHITE_A;
  gc->heaps = NULL;
  gc->free_heaps = NULL;
  gc->regions = NULL;
  add_heap(mrb, gc);
  gc->interval_ratio = DEFAULT_GC_INTERVAL_RATIO;
  gc->step_ratio = DEFAULT_GC_STEP_RATIO;
  gc->auto_step = TRUE;
  gc->sched_driven = FALSE;
#ifndef MRB_GC_TURN_OFF_GENERATIONAL
  gc->generational = TRUE;
  gc->full = TRUE;
#endif
}

static void obj_free(mrb_state *mrb, struct RBasic *obj, mrb_bool end);

static void
free_heap(mrb_state *mrb, mrb_gc *gc)
{
  mrb_heap_page *page = gc->heaps;
  mrb_heap_page *tmp;
  RVALUE *p, *e;

  while (page) {
    tmp = page;
    page = page->next;
    for (p = tmp->objects, e=p+MRB_HEAP_PAGE_SIZE; p<e; p++) {
      if (p->as.free.tt != MRB_TT_FREE)
        obj_free(mrb, &p->as.basic, TRUE);
    }
    if (!tmp->region) {
      mrb_free(mrb, tmp);
    }
  }
}

void
mrb_gc_destroy(mrb_state *mrb, mrb_gc *gc)
{
  free_heap(mrb, gc);
  /* free region descriptors (buffer memory belongs to the caller) */
  {
    mrb_heap_region *region = gc->regions;
    while (region) {
      mrb_heap_region *next = region->next;
      mrb_free(mrb, region);
      region = next;
    }
  }
#ifndef MRB_GC_FIXED_ARENA
  mrb_free(mrb, gc->arena);
#endif
}

static void
gc_arena_keep(mrb_state *mrb, mrb_gc *gc)
{
#ifdef MRB_GC_FIXED_ARENA
  if (gc->arena_idx >= MRB_GC_ARENA_SIZE) {
    /* arena overflow error */
    gc->arena_idx = MRB_GC_ARENA_SIZE - 4; /* force room in arena */
    mrb_exc_raise(mrb, mrb_obj_value(mrb->arena_err));
  }
#else
  if (gc->arena_idx >= gc->arena_capa) {
    /* extend arena */
    int newcapa = gc->arena_capa * 3 / 2;
    gc->arena = (struct RBasic**)mrb_realloc(mrb, gc->arena, sizeof(struct RBasic*)*newcapa);
    gc->arena_capa = newcapa;
  }
#endif
}

static inline void
gc_protect(mrb_state *mrb, mrb_gc *gc, struct RBasic *p)
{
#ifdef MRB_GC_FIXED_ARENA
  mrb_assert(gc->arena_idx < MRB_GC_ARENA_SIZE);
#else
  mrb_assert(gc->arena_idx < gc->arena_capa);
#endif
  gc->arena[gc->arena_idx++] = p;
}

/* mrb_gc_protect() leaves the object in the arena */
MRB_API void
mrb_gc_protect(mrb_state *mrb, mrb_value obj)
{
  if (mrb_immediate_p(obj)) return;
  struct RBasic *p = mrb_basic_ptr(obj);
  if (is_red(p)) return;
  gc_arena_keep(mrb, &mrb->gc);
  gc_protect(mrb, &mrb->gc, p);
}

#define GC_ROOT_SYM MRB_SYM(_gc_root_)

/* mrb_gc_register() keeps the object from GC.

   Register your object when it's exported to C world,
   without reference from Ruby world, e.g. callback
   arguments.  Don't forget to remove the object using
   mrb_gc_unregister, otherwise your object will leak.
*/

MRB_API void
mrb_gc_register(mrb_state *mrb, mrb_value obj)
{
  if (mrb_immediate_p(obj)) return;
  mrb_value table = mrb_gv_get(mrb, GC_ROOT_SYM);
  int ai = mrb_gc_arena_save(mrb);
  mrb_gc_protect(mrb, obj);
  if (!mrb_array_p(table)) {
    table = mrb_ary_new(mrb);
    mrb_obj_ptr(table)->c = NULL; /* hide from ObjectSpace.each_object */
    mrb_gv_set(mrb, GC_ROOT_SYM, table);
  }
  mrb_ary_push(mrb, table, obj);
  mrb_gc_arena_restore(mrb, ai);
}

/* mrb_gc_unregister() removes the object from GC root. */
MRB_API void
mrb_gc_unregister(mrb_state *mrb, mrb_value obj)
{
  if (mrb_immediate_p(obj)) return;
  mrb_value table = mrb_gv_get(mrb, GC_ROOT_SYM);
  if (!mrb_array_p(table)) return;
  struct RArray *a = mrb_ary_ptr(table);
  mrb_ary_modify(mrb, a);
  mrb_int len = ARY_LEN(a);
  mrb_value *ptr = ARY_PTR(a);
  mrb_int w = 0;
  for (mrb_int r = 0; r < len; r++) {
    if (mrb_ptr(ptr[r]) != mrb_ptr(obj)) {
      ptr[w++] = ptr[r];
    }
  }
  ARY_SET_LEN(a, w);
}

/* Core allocation without type validation.
   Used internally by mrb_proc_new, mrb_env_new, etc. */
struct RBasic*
mrb_obj_alloc_core(mrb_state *mrb, enum mrb_vtype ttype, struct RClass *cls)
{
  static const RVALUE RVALUE_zero = { { { NULL, MRB_TT_FALSE } } };
  mrb_gc *gc = &mrb->gc;

#ifdef MRB_GC_STRESS
  mrb_full_gc(mrb);
#endif
  gc->gc_debt++;
  if (gc->gc_debt > 0) {
    mrb_incremental_gc(mrb);
    /* Safety valve: when auto_step is off, mrb_incremental_gc no-ops and the
       GC task is expected to drive collection. If it falls behind and debt
       runs past debt_limit, force synchronous progress here so the worst
       case stays bounded rather than growing the heap without limit. */
    if (!gc->auto_step && gc->debt_limit > 0 && gc->gc_debt > gc->debt_limit &&
        !gc->disabled && !gc->iterating) {
#ifdef MRB_GC_PROFILE
      uint64_t prof_t0 = gc_prof_enter(gc, &gc->prof_sync);
#endif
      incremental_gc_run(mrb, gc);
#ifdef MRB_GC_PROFILE
      gc_prof_leave(gc, prof_t0);
#endif
    }
  }
  gc_arena_keep(mrb, gc);
  if (gc->free_heaps == NULL) {
    /* Free slots ran out. Under the auto_step allocation-driven policy, try to
       reclaim before growing: a full collection finishes the (possibly
       half-run) incremental cycle and sweeps its garbage, usually refilling the
       freelists without adding a page. Growing immediately instead ratchets the
       page count up to the workload's transient high-water mark and it never
       comes back down — pages are only freed when COMPLETELY empty, so
       fragmentation keeps them pinned. On fixed-arena targets the pages
       eventually consume the whole arena even though most of their slots are
       free.

       Skip the reclaim entirely under GC.scheduler_driven (auto_step off): the
       allocation path must not run a synchronous collection there — it would
       re-introduce exactly the pauses the mode removes and, by finishing the
       cycle atomically, starve the scheduler's idle stepping. In that mode we
       grow here and leave reclamation to the idle steps and the debt_limit
       safety valve. Keeping the heap tight is not a goal of scheduler-driven
       mode; bounded latency is.

       The !collecting guard mirrors mrb_realloc_simple(): allocation can
       re-enter here from inside a running mark/sweep (an RData dfree callback
       that allocates during the sweep phase), and starting a nested collection
       there would corrupt the GC's in-progress state. */
    if (gc->auto_step && !gc->collecting) {
      /* gc->live is inflated by dead-but-unswept objects at this point, so it
         cannot distinguish "full of garbage" (reclaim!) from "full of live
         data" (grow!). live_after_mark from the last completed cycle is the
         garbage-free estimate of the true live set: sweep decrements it as
         objects are freed. Only reclaim when the accounting shows real slack
         (live well below capacity); otherwise a working set that is genuinely
         growing would collect before every page-add and just burn time, so
         grow directly. Walking the page list here is fine: growth events are
         rare and the walk is a few pointer hops per page. (mrb_full_gc() is
         also a no-op while GC is disabled or iterating; we grow then, too.) */
      size_t capacity = 0;
      for (mrb_heap_page *page = gc->heaps; page; page = page->next) {
        capacity += MRB_HEAP_PAGE_SIZE;
      }
      if (gc->live_after_mark + MRB_HEAP_PAGE_SIZE/2 < capacity) {
        mrb_full_gc(mrb);
      }
    }
    if (gc->free_heaps == NULL) {
      add_heap(mrb, gc);
    }
  }

  RVALUE *p = gc->free_heaps->freelist;
  gc->free_heaps->freelist = p->as.free.next;
  if (gc->free_heaps->freelist == NULL) {
    gc->free_heaps = gc->free_heaps->free_next;
  }

  gc->live++;
  gc_protect(mrb, gc, &p->as.basic);
  *p = RVALUE_zero;
  p->as.basic.tt = ttype;
  p->as.basic.c = cls;
  if (ttype == MRB_TT_OBJECT) {
    p->as.basic.flags |= MRB_FL_OBJ_SHAPED;
  }
  paint_partial_white(gc, &p->as.basic);
  return &p->as.basic;
}

MRB_API struct RBasic*
mrb_obj_alloc(mrb_state *mrb, enum mrb_vtype ttype, struct RClass *cls)
{
  if (cls) {
    enum mrb_vtype tt;

    switch (cls->tt) {
    case MRB_TT_CLASS:
    case MRB_TT_SCLASS:
    case MRB_TT_MODULE:
    case MRB_TT_ENV:
      break;
    default:
      mrb_raise(mrb, E_TYPE_ERROR, "allocation failure");
    }
    tt = MRB_INSTANCE_TT(cls);
    if (ttype != MRB_TT_SCLASS &&
        ttype != MRB_TT_ICLASS &&
        ttype != MRB_TT_ENV &&
        ttype != MRB_TT_BIGINT &&
        ttype != tt &&
        !(cls == mrb->object_class && (ttype == MRB_TT_CPTR || ttype == MRB_TT_CDATA || ttype == MRB_TT_ISTRUCT))) {
      mrb_raisef(mrb, E_TYPE_ERROR, "allocation failure of %C", cls);
    }
  }
  if (ttype <= MRB_TT_FREE) {
    mrb_raisef(mrb, E_TYPE_ERROR, "allocation failure of %C (type %d)", cls, (int)ttype);
  }
  return mrb_obj_alloc_core(mrb, ttype, cls);
}

static inline void
add_gray_list(mrb_gc *gc, struct RBasic *obj)
{
#ifdef MRB_GC_STRESS
  if (obj->tt > MRB_TT_MAXDEFINE) {
    abort();
  }
#endif
  paint_gray(obj);
  if (gc->gray_stack_top < MRB_GRAY_STACK_SIZE) {
    gc->gray_stack[gc->gray_stack_top++] = obj;
  }
  else {
    gc->gray_overflow = TRUE;
  }
}

static void
mark_context_stack(mrb_state *mrb, struct mrb_context *c)
{
  size_t i, e;

  if (c->stbase == NULL) return;
  if (c->ci) {
    e = (c->ci->stack ? c->ci->stack - c->stbase : 0);
    e += mrb_ci_nregs(c->ci);
  }
  else {
    e = 0;
  }
  if (c->stbase + e > c->stend) e = c->stend - c->stbase;
  for (i=0; i<e; i++) {
    mrb_value v = c->stbase[i];

    if (!mrb_immediate_p(v)) {
      mrb_gc_mark(mrb, mrb_basic_ptr(v));
    }
  }
  e = c->stend - c->stbase;
  for (; i<e; i++) {
    SET_NIL_VALUE(c->stbase[i]);
  }
}

static void
mark_context(mrb_state *mrb, struct mrb_context *c)
{
  mrb_callinfo *ci;

 start:
  if (c->status == MRB_FIBER_TERMINATED) return;

  /* mark VM stack */
  mark_context_stack(mrb, c);

  /* mark call stack */
  if (c->cibase) {
    for (ci = c->cibase; ci <= c->ci; ci++) {
      mrb_gc_mark(mrb, (struct RBasic*)ci->proc);
      mrb_gc_mark(mrb, (struct RBasic*)ci->u.target_class);
    }
  }
  /* mark fibers */
  mrb_gc_mark(mrb, (struct RBasic*)c->fib);
  if (c->prev) {
    c = c->prev;
    goto start;
  }
}

static size_t
gc_mark_children(mrb_state *mrb, mrb_gc *gc, struct RBasic *obj)
{
  size_t children = 0;

  mrb_assert(is_gray(obj));
  paint_black(obj);
  mrb_gc_mark(mrb, (struct RBasic*)obj->c);
  switch (obj->tt) {
  case MRB_TT_ICLASS:
    {
      struct RClass *c = (struct RClass*)obj;
      if (MRB_FLAG_TEST(c, MRB_FL_CLASS_IS_ORIGIN)) {
        children += mrb_gc_mark_mt(mrb, c);
      }
      mrb_gc_mark(mrb, (struct RBasic*)((struct RClass*)obj)->super);
      children++;
    }
    break;

  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
  case MRB_TT_SCLASS:
    {
      struct RClass *c = (struct RClass*)obj;

      mrb_gc_mark_mt(mrb, c);
      mrb_gc_mark(mrb, (struct RBasic*)c->super);
      children += mrb_gc_mark_mt(mrb, c);
      children++;
    }
    /* fall through */

  case MRB_TT_OBJECT:
  case MRB_TT_CDATA:
    children += mrb_gc_mark_iv(mrb, (struct RObject*)obj);
    break;

  case MRB_TT_PROC:
    {
      struct RProc *p = (struct RProc*)obj;

      mrb_gc_mark(mrb, (struct RBasic*)p->upper);
      mrb_gc_mark(mrb, (struct RBasic*)p->e.env);
      children+=2;
    }
    break;

  case MRB_TT_ENV:
    {
      struct REnv *e = (struct REnv*)obj;

      // The data stack must always be protected from GC regardless of the MRB_ENV_CLOSE flag.
      // This is because the data stack is not protected if the fiber is GC'd.
      mrb_int len = MRB_ENV_LEN(e);
      for (mrb_int i=0; i<len; i++) {
        mrb_gc_mark_value(mrb, e->stack[i]);
      }
      children += len;
    }
    break;

  case MRB_TT_FIBER:
    {
      struct mrb_context *c = ((struct RFiber*)obj)->cxt;

      if (!c || c->status == MRB_FIBER_TERMINATED) break;
      mark_context(mrb, c);
      if (!c->ci) break;

      /* mark stack */
      size_t i = c->ci->stack - c->stbase;
      i += mrb_ci_nregs(c->ci);
      if (c->stbase + i > c->stend) i = c->stend - c->stbase;
      children += i;

      /* mark closure */
      if (c->cibase) {
        children += c->ci - c->cibase + 1;
      }
    }
    break;

  case MRB_TT_STRUCT:
  case MRB_TT_ARRAY:
    {
      struct RArray *a = (struct RArray*)obj;
      size_t len = ARY_LEN(a);
      mrb_value *p = ARY_PTR(a);

      for (size_t i=0; i<len; i++) {
        mrb_gc_mark_value(mrb, p[i]);
      }
      children += len;
    }
    break;

  case MRB_TT_HASH:
    children += mrb_gc_mark_iv(mrb, (struct RObject*)obj);
    children += mrb_gc_mark_hash(mrb, (struct RHash*)obj);
    break;

  case MRB_TT_STRING:
    if (RSTR_FSHARED_P(obj)) {
      struct RString *s = (struct RString*)obj;
      mrb_gc_mark(mrb, (struct RBasic*)s->as.heap.aux.fshared);
    }
    break;

  case MRB_TT_RANGE:
    children += mrb_gc_mark_range(mrb, (struct RRange*)obj);
    break;

  case MRB_TT_BREAK:
    {
      struct RBreak *brk = (struct RBreak*)obj;
      mrb_gc_mark_value(mrb, mrb_break_value_get(brk));
      children++;
    }
    break;

  case MRB_TT_EXCEPTION:
    children += mrb_gc_mark_iv(mrb, (struct RObject*)obj);
    if (((struct RException*)obj)->mesg) {
      mrb_gc_mark(mrb, (struct RBasic*)((struct RException*)obj)->mesg);
      children++;
    }
    if (((struct RException*)obj)->backtrace) {
      mrb_gc_mark(mrb, (struct RBasic*)((struct RException*)obj)->backtrace);
      children++;
    }
    break;

  case MRB_TT_BACKTRACE:
    children += ((struct RBacktrace*)obj)->len;
    break;

#if defined(MRB_USE_RATIONAL) && defined(MRB_USE_BIGINT)
  case MRB_TT_RATIONAL:
    children += mrb_rational_mark(mrb, obj);
    break;
#endif
#ifdef MRB_USE_SET
  case MRB_TT_SET:
    children += mrb_gc_mark_set(mrb, obj);
    break;
#endif

  default:
    break;
  }
  return children;
}

MRB_API void
mrb_gc_mark(mrb_state *mrb, struct RBasic *obj)
{
  if (obj == 0) return;
  if (!is_white(obj)) return;
  if (is_red(obj)) return;
  mrb_assert((obj)->tt != MRB_TT_FREE);
  switch (obj->tt) {
  case MRB_TT_STRING:
    /* most strings have no children; handle fshared inline */
    paint_black(obj);
    mrb_gc_mark(mrb, (struct RBasic*)obj->c);
    if (RSTR_FSHARED_P(obj)) {
      struct RString *s = (struct RString*)obj;
      mrb_gc_mark(mrb, (struct RBasic*)s->as.heap.aux.fshared);
    }
    return;
  case MRB_TT_INTEGER:
  case MRB_TT_CPTR:
#ifdef MRB_USE_BIGINT
  case MRB_TT_BIGINT:
#endif
#ifdef MRB_USE_COMPLEX
  case MRB_TT_COMPLEX:
#endif
    /* leaf types: no children besides class */
    paint_black(obj);
    mrb_gc_mark(mrb, (struct RBasic*)obj->c);
    return;
  default:
    break;
  }
  add_gray_list(&mrb->gc, obj);
}

static void
obj_free(mrb_state *mrb, struct RBasic *obj, mrb_bool end)
{
  DEBUG(fprintf(stderr, "obj_free(%p,tt=%d)\n",obj,obj->tt));
  switch (obj->tt) {
  case MRB_TT_OBJECT:
    mrb_gc_free_iv(mrb, (struct RObject*)obj);
    break;

  case MRB_TT_EXCEPTION:
    mrb_gc_free_iv(mrb, (struct RObject*)obj);
    break;

  case MRB_TT_CLASS:
  case MRB_TT_MODULE:
  case MRB_TT_SCLASS:
    mrb_gc_free_mt(mrb, (struct RClass*)obj);
    mrb_gc_free_iv(mrb, (struct RObject*)obj);
    if (!end)
      mrb_mc_clear_by_class(mrb, (struct RClass*)obj);
    break;
  case MRB_TT_ICLASS:
    if (MRB_FLAG_TEST(obj, MRB_FL_CLASS_IS_ORIGIN))
      mrb_gc_free_mt(mrb, (struct RClass*)obj);
    if (!end)
      mrb_mc_clear_by_class(mrb, (struct RClass*)obj);
    break;
  case MRB_TT_ENV:
    {
      struct REnv *e = (struct REnv*)obj;

      if (!MRB_ENV_ONSTACK_P(e)) {
        mrb_free(mrb, e->stack);
      }
    }
    break;

  case MRB_TT_FIBER:
    {
      struct mrb_context *c = ((struct RFiber*)obj)->cxt;

      if (c && c != mrb->root_c) {
        if (!end && c->status != MRB_FIBER_TERMINATED) {
          mrb_callinfo *ci = c->ci;
          mrb_callinfo *ce = c->cibase;

          while (ce <= ci) {
            struct REnv *e = ci->u.env;
            if (e && heap_p(&mrb->gc, (struct RBasic*)e) && !is_dead(&mrb->gc, (struct RBasic*)e) &&
                e->tt == MRB_TT_ENV && MRB_ENV_ONSTACK_P(e)) {
              mrb_env_unshare(mrb, e, TRUE);
            }
            ci--;
          }
        }
        mrb_free_context(mrb, c);
      }
    }
    break;

  case MRB_TT_STRUCT:
  case MRB_TT_ARRAY:
    if (ARY_SHARED_P(obj))
      mrb_ary_decref(mrb, ((struct RArray*)obj)->as.heap.aux.shared);
    else if (!ARY_EMBED_P(obj))
      mrb_free(mrb, ((struct RArray*)obj)->as.heap.ptr);
    break;

  case MRB_TT_HASH:
    mrb_gc_free_iv(mrb, (struct RObject*)obj);
    mrb_gc_free_hash(mrb, (struct RHash*)obj);
    break;

  case MRB_TT_STRING:
    mrb_gc_free_str(mrb, (struct RString*)obj);
    break;

  case MRB_TT_PROC:
    {
      struct RProc *p = (struct RProc*)obj;

      if (!MRB_PROC_CFUNC_P(p) && !MRB_PROC_ALIAS_P(p) && p->body.irep) {
        mrb_irep *irep = (mrb_irep*)p->body.irep;
        if (end) {
          mrb_irep_cutref(mrb, irep);
        }
        mrb_irep_decref(mrb, irep);
      }
    }
    break;

  case MRB_TT_RANGE:
    mrb_gc_free_range(mrb, ((struct RRange*)obj));
    break;

#ifdef MRB_USE_SET
  case MRB_TT_SET:
    mrb_gc_free_set(mrb, obj);
    break;
#endif

  case MRB_TT_CDATA:
    {
      struct RData *d = (struct RData*)obj;
      if (d->type && d->type->dfree) {
        d->type->dfree(mrb, d->data);
      }
      mrb_gc_free_iv(mrb, (struct RObject*)obj);
    }
    break;

#if defined(MRB_USE_RATIONAL) && defined(MRB_INT64) && defined(MRB_32BIT)
  case MRB_TT_RATIONAL:
    {
      struct RData *o = (struct RData*)obj;
      mrb_free(mrb, o->iv);
    }
    break;
#endif

#if defined(MRB_USE_COMPLEX) && defined(MRB_32BIT) && !defined(MRB_USE_FLOAT32)
  case MRB_TT_COMPLEX:
    {
      struct RData *o = (struct RData*)obj;
      mrb_free(mrb, o->iv);
    }
    break;
#endif

#ifdef MRB_USE_BIGINT
  case MRB_TT_BIGINT:
    mrb_gc_free_bint(mrb, obj);
    break;
#endif

  case MRB_TT_BACKTRACE:
    {
      struct RBacktrace *bt = (struct RBacktrace*)obj;
      for (size_t i = 0; i < bt->len; i++) {
        const mrb_irep *irep = bt->locations[i].irep;
        if (irep == NULL) continue;
        mrb_irep_decref(mrb, (mrb_irep*)irep);
      }
      mrb_free(mrb, bt->locations);
    }

  default:
    break;
  }
#if defined(MRB_GC_STRESS) && defined(MRB_DEBUG)
  memset(obj, -1, sizeof(RVALUE));
  paint_white(obj);
#endif
  obj->tt = MRB_TT_FREE;
}

static void
root_scan_phase(mrb_state *mrb, mrb_gc *gc)
{
  int i, e;

  if (!is_minor_gc(gc)) {
    gc->gray_stack_top = 0;
    gc->gray_overflow = FALSE;
  }

  mrb_gc_mark_gv(mrb);
  /* mark arena */
  for (i=0,e=gc->arena_idx; i<e; i++) {
    mrb_gc_mark(mrb, gc->arena[i]);
  }
  /* mark class hierarchy */
  mrb_gc_mark(mrb, (struct RBasic*)mrb->object_class);

  /* mark built-in classes */
  mrb_gc_mark(mrb, (struct RBasic*)mrb->class_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->module_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->proc_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->string_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->array_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->hash_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->range_class);

#ifndef MRB_NO_FLOAT
  mrb_gc_mark(mrb, (struct RBasic*)mrb->float_class);
#endif
  mrb_gc_mark(mrb, (struct RBasic*)mrb->integer_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->true_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->false_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->nil_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->symbol_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->kernel_module);

  mrb_gc_mark(mrb, (struct RBasic*)mrb->eException_class);
  mrb_gc_mark(mrb, (struct RBasic*)mrb->eStandardError_class);

  /* mark top_self */
  mrb_gc_mark(mrb, (struct RBasic*)mrb->top_self);
  /* mark exception */
  mrb_gc_mark(mrb, (struct RBasic*)mrb->exc);

  mark_context(mrb, mrb->c);
  if (mrb->root_c != mrb->c) {
    mark_context(mrb, mrb->root_c);
  }

#ifdef MRB_USE_TASK_SCHEDULER
  /* mark tasks - calls into task.c to mark all task queues */
  mrb_task_mark_all(mrb);
#endif
}

static void
gc_gray_rescan(mrb_state *mrb, mrb_gc *gc)
{
  mrb_heap_page *page = gc->heaps;

  gc->gray_overflow = FALSE;
  while (page) {
    RVALUE *p = page->objects;
    RVALUE *e = p + MRB_HEAP_PAGE_SIZE;
    for (; p < e; p++) {
      if (is_gray(&p->as.basic) && p->as.basic.tt != MRB_TT_FREE) {
        if (gc->gray_stack_top >= MRB_GRAY_STACK_SIZE) {
          gc->gray_overflow = TRUE;
          return;
        }
        gc->gray_stack[gc->gray_stack_top++] = &p->as.basic;
      }
    }
    page = page->next;
  }
}

static void
gc_mark_gray_list(mrb_state *mrb, mrb_gc *gc) {
  for (;;) {
    while (gc->gray_stack_top > 0) {
      struct RBasic *obj = gc->gray_stack[--gc->gray_stack_top];
      gc_mark_children(mrb, gc, obj);
    }
    if (!gc->gray_overflow) break;
    gc_gray_rescan(mrb, gc);
  }
}

static size_t
incremental_marking_phase(mrb_state *mrb, mrb_gc *gc, size_t limit)
{
  size_t tried_marks = 0;

  while (tried_marks < limit) {
    if (gc->gray_stack_top > 0) {
      struct RBasic *obj = gc->gray_stack[--gc->gray_stack_top];
      tried_marks += gc_mark_children(mrb, gc, obj);
    }
    else if (gc->gray_overflow) {
      gc_gray_rescan(mrb, gc);
      if (gc->gray_stack_top == 0) break;
    }
    else {
      break;
    }
  }

  return tried_marks;
}

static void
clear_error_object(mrb_state *mrb, struct RObject *obj)
{
  if (obj == 0) return;
  if (!is_white(obj)) return;
  paint_black(obj);
  mrb_gc_mark(mrb, (struct RBasic*)obj->c);
  mrb_gc_free_iv(mrb, obj);
  struct RException *err = (struct RException*)obj;
  err->iv = NULL;
  err->mesg = NULL;
  err->backtrace = NULL;
}

static void
final_marking_phase(mrb_state *mrb, mrb_gc *gc)
{
  int i, e;

  /* mark arena */
  for (i=0,e=gc->arena_idx; i<e; i++) {
    mrb_gc_mark(mrb, gc->arena[i]);
  }
  mrb_gc_mark_gv(mrb);
  mark_context(mrb, mrb->c);
  if (mrb->c != mrb->root_c) {
    mark_context(mrb, mrb->root_c);
  }

#ifdef MRB_USE_TASK_SCHEDULER
  /* Re-mark task stacks atomically, the same way mrb->c is re-marked here.
     Task stacks are unbarriered roots: the VM mutates them during the
     incremental mark phase without a write barrier, so root_scan_phase's
     snapshot can go stale. mark_context() gives the running context this
     atomic re-scan; task contexts need it too. Omitting it lets a stack slot
     outlive the object it references, and the next cycle's mark of that slot
     trips the MRB_TT_FREE assertion in mrb_gc_mark (issue #6886). */
  mrb_task_mark_all(mrb);
#endif

  mrb_gc_mark(mrb, (struct RBasic*)mrb->exc);

  /* mark pre-allocated exception */
  clear_error_object(mrb, mrb->nomem_err);
  clear_error_object(mrb, mrb->stack_err);
#ifdef MRB_GC_FIXED_ARENA
  clear_error_object(mrb, mrb->arena_err);
#endif

  gc_mark_gray_list(mrb, gc);
}

static void
prepare_incremental_sweep(mrb_state *mrb, mrb_gc *gc)
{
  //  mrb_assert(gc->gray_stack_top == 0);
  gc->state = MRB_GC_STATE_SWEEP;
  gc->sweeps = NULL;
  gc->live_after_mark = gc->live;
}

static size_t
incremental_sweep_phase(mrb_state *mrb, mrb_gc *gc, size_t limit)
{
  mrb_heap_page *prev = gc->sweeps;
  mrb_heap_page *page = prev ? prev->next : gc->heaps;
  size_t tried_sweep = 0;

  while (page && (tried_sweep < limit)) {
    size_t freed = 0;
    mrb_bool dead_slot = TRUE;

    if (is_minor_gc(gc) && page->old) {
      /* skip a slot which doesn't contain any young object */
      dead_slot = FALSE;
    }
    else {
      RVALUE *p = page->objects;
      RVALUE *e = p + MRB_HEAP_PAGE_SIZE;
      while (p<e) {
        if (is_dead(gc, &p->as.basic)) {
          if (p->as.basic.tt != MRB_TT_FREE) {
            obj_free(mrb, &p->as.basic, FALSE);
            mrb_assert(p->as.basic.tt == MRB_TT_FREE);
            p->as.free.next = page->freelist;
            page->freelist = p;
            freed++;
          }
        }
        else {
          if (!is_generational(gc))
            paint_partial_white(gc, &p->as.basic); /* next gc target */
          dead_slot = FALSE;
        }
        p++;
      }
    }

    /* free dead slot */
    if (dead_slot && !page->region) {
      mrb_heap_page *next = page->next;

      if (prev) prev->next = next;
      if (gc->heaps == page)
        gc->heaps = page->next;

      mrb_free(mrb, page);
      page = next;
    }
    else {
      if (page->freelist == NULL && is_minor_gc(gc))
        page->old = TRUE;
      else
        page->old = FALSE;
      prev = page;
      page = page->next;
    }
    tried_sweep += MRB_HEAP_PAGE_SIZE;
    gc->live -= freed;
    gc->live_after_mark -= freed;
  }
  gc->sweeps = prev;

  /* rebuild free_heaps link */
  gc->free_heaps = NULL;
  for (mrb_heap_page *p = gc->heaps; p; p=p->next) {
    if (p->freelist) {
      p->free_next = gc->free_heaps;
      gc->free_heaps = p;
    }
  }

  return tried_sweep;
}

static size_t
incremental_gc(mrb_state *mrb, mrb_gc *gc, size_t limit)
{
  switch (gc->state) {
  case MRB_GC_STATE_ROOT:
    root_scan_phase(mrb, gc);
    gc->state = MRB_GC_STATE_MARK;
    flip_white_part(gc);
    return 0;
  case MRB_GC_STATE_MARK:
    if (gc->gray_stack_top > 0 || gc->gray_overflow) {
      size_t tried_marks = incremental_marking_phase(mrb, gc, limit);
#ifdef MRB_GC_PROFILE
      gc->prof_mark_work_total += tried_marks;
#endif
      return tried_marks;
    }
    else {
#ifdef MRB_GC_PROFILE
      uint64_t fm0 = gc_prof_now_us();
#endif
      final_marking_phase(mrb, gc);
#ifdef MRB_GC_PROFILE
      {
        uint64_t fmdt = gc_prof_now_us() - fm0;
        if (fmdt > gc->prof_final_mark_max_us) {
          gc->prof_final_mark_max_us = fmdt;
          gc->prof_final_mark_max_live = gc->live;
        }
      }
#endif
      prepare_incremental_sweep(mrb, gc);
      return 0;
    }
  case MRB_GC_STATE_SWEEP: {
     size_t tried_sweep = 0;
     tried_sweep = incremental_sweep_phase(mrb, gc, limit);
#ifdef MRB_GC_PROFILE
     gc->prof_sweep_work_total += tried_sweep;
#endif
     if (tried_sweep == 0)
       gc->state = MRB_GC_STATE_ROOT;
     return tried_sweep;
  }
  default:
    /* unknown state */
    mrb_assert(0);
    return 0;
  }
}

/* The bare engine loop. With run_to_root, drives a whole cycle to
   MRB_GC_STATE_ROOT; otherwise advances one step bounded by `limit`.
   Returns the work done. */
static size_t
run_incremental(mrb_state *mrb, mrb_gc *gc, size_t limit, mrb_bool run_to_root)
{
  size_t result = 0;

  if (run_to_root) {
    do {
      result += incremental_gc(mrb, gc, limit);
    } while (gc->state != MRB_GC_STATE_ROOT);
  }
  else {
    while (result < limit) {
      result += incremental_gc(mrb, gc, limit);
      if (gc->state == MRB_GC_STATE_ROOT)
        break;
    }
  }
  return result;
}

/* Drive the incremental collector with the reentrancy guard held.
 *
 * gc->collecting marks that a mark/sweep is running on the C stack. While it
 * is set, the emergency GC in mrb_realloc_simple is suppressed, so an
 * allocation failure raised from *inside* sweep (e.g. an RData dfree that
 * allocates) cannot recursively re-drive the same sweep -- which would
 * corrupt the page-list walk and could overflow the stack.
 *
 * Every path that can sweep funnels through this helper (incremental_gc is
 * only ever called from here), so all callers -- mrb_incremental_gc,
 * mrb_full_gc, clear_all_old, change_gen_gc_mode, gc_drive, and the emergency
 * path -- are covered without each having to manage the flag.
 *
 * When an outer jmp buffer exists, wrap the run in MRB_TRY/MRB_CATCH so the
 * flag is restored on both normal return and a longjmp out of a dfree (so it
 * can never leak and permanently wedge emergency GC), then rethrow. When
 * there is no outer handler (mrb->jmp == NULL, e.g. GC invoked from embedder
 * C code), do NOT install a temporary handler: a raise then follows mruby's
 * normal uncaught path (report and abort) instead of longjmp'ing to a NULL
 * buffer -- and since that aborts the process, the unrestored flag is moot.
 */
static size_t
gc_drive(mrb_state *mrb, mrb_gc *gc, size_t limit, mrb_bool run_to_root)
{
  mrb_bool was_collecting = gc->collecting;
  size_t result = 0;

  gc->collecting = TRUE;

  if (mrb->jmp) {
    struct mrb_jmpbuf *prev_jmp = mrb->jmp;
    struct mrb_jmpbuf c_jmp;

    MRB_TRY(&c_jmp) {
      mrb->jmp = &c_jmp;
      result = run_incremental(mrb, gc, limit, run_to_root);
      mrb->jmp = prev_jmp;
      gc->collecting = was_collecting;
    } MRB_CATCH(&c_jmp) {
      gc->collecting = was_collecting;
#ifdef MRB_GC_PROFILE
      /* This longjmp is about to unwind every gc_prof_enter frame on the
         stack: they all wrap gc_drive transitively and nothing between here
         and them catches, so their gc_prof_leave calls never run. Reset the
         depth, or profiling would silently stop recording for the rest of
         the process (enter would never see depth 0 again). */
      gc->prof_depth = 0;
      gc->prof_target = NULL;
#endif
      mrb->jmp = prev_jmp;
      MRB_THROW(prev_jmp);
    } MRB_END_EXC(&c_jmp);
  }
  else {
    result = run_incremental(mrb, gc, limit, run_to_root);
    gc->collecting = was_collecting;
  }

  return result;
}

static size_t
incremental_gc_finish(mrb_state *mrb, mrb_gc *gc)
{
  return gc_drive(mrb, gc, SIZE_MAX, TRUE);
}

static size_t
incremental_gc_step(mrb_state *mrb, mrb_gc *gc)
{
  size_t limit = (GC_STEP_SIZE/100) * gc->step_ratio;
  size_t result;
  if (gc->step_limit > 0 && limit > gc->step_limit) {
    limit = gc->step_limit;
  }
  result = gc_drive(mrb, gc, limit, FALSE);
  gc->gc_debt -= (mrb_int)result;
  return result;
}

static void
clear_all_old(mrb_state *mrb, mrb_gc *gc)
{
  mrb_assert(is_generational(gc));
  if (gc->full) {
    /* finish the half baked GC */
    incremental_gc_finish(mrb, gc);
  }
  /* Sweep the dead objects, then reset all the live objects
   * (including all the old objects, of course) to white. */
  gc->generational = FALSE;
  prepare_incremental_sweep(mrb, gc);
  incremental_gc_finish(mrb, gc);
  gc->generational = TRUE;
  /* The gray objects have already been painted as white */
  gc->gray_stack_top = 0;
  gc->gray_overflow = FALSE;
}

/* One unit of incremental GC progress plus the end-of-cycle bookkeeping.
   This is the body of the GC engine with no policy guard: callers decide
   whether it may run (mrb_incremental_gc honours disabled/auto_step;
   mrb_gc_step drives it directly). Kept separate so the scheduler-driven and
   automatic drivers share identical mark/sweep/debt/generational semantics. */
static size_t
incremental_gc_run(mrb_state *mrb, mrb_gc *gc)
{
  size_t work;

  if (is_minor_gc(gc)) {
#ifdef MRB_GC_STATS
    gc->gc_total_count++;
    gc->minor_gc_count++;
#endif
    work = incremental_gc_finish(mrb, gc);
  }
  else {
#ifdef MRB_GC_STATS
    if (gc->state == MRB_GC_STATE_ROOT) {
      gc->gc_total_count++;
      gc->major_gc_count++;
    }
#endif
    work = incremental_gc_step(mrb, gc);
  }

  if (gc->state == MRB_GC_STATE_ROOT) {
    gc->malloc_increase = 0;
    mrb_assert(gc->live >= gc->live_after_mark);
    {
      mrb_int credit = (mrb_int)((gc->live_after_mark/100) * gc->interval_ratio)
                     - (mrb_int)gc->live_after_mark;
      if (credit < (mrb_int)GC_STEP_SIZE) credit = (mrb_int)GC_STEP_SIZE;
      gc->gc_debt = -credit;
    }

    if (is_major_gc(gc)) {
      size_t threshold = gc->live_after_mark/100 * MAJOR_GC_INC_RATIO;

      gc->full = FALSE;
      if (threshold < MAJOR_GC_TOOMANY) {
        gc->oldgen_threshold = threshold;
      }
      else {
        /* too many objects allocated during incremental GC, */
        /* instead of increasing threshold, invoke full GC. */
        mrb_full_gc(mrb);
      }
    }
    else if (is_minor_gc(gc) && gc->live > gc->oldgen_threshold) {
      clear_all_old(mrb, gc);
      gc->full = TRUE;
    }
  }

  return work;
}

/* Advance the incremental collector by one policy-sized unit.
 *
 * NOTE: this honours the auto_step flag -- when collection is driven by the
 * task scheduler instead (GC.scheduler_driven = true, auto_step off), this
 * call silently no-ops. An embedder that needs unconditional collection must
 * use mrb_full_gc(). */
MRB_API void
mrb_incremental_gc(mrb_state *mrb)
{
  mrb_gc *gc = &mrb->gc;

  if (gc->disabled || gc->iterating || !gc->auto_step) return;

#ifdef MRB_GC_PROFILE
  uint64_t prof_t0 = gc_prof_enter(gc, &gc->prof_sync);
#endif
  incremental_gc_run(mrb, gc);
#ifdef MRB_GC_PROFILE
  gc_prof_leave(gc, prof_t0);
#endif
}

/* Perform a full gc cycle */
MRB_API void
mrb_full_gc(mrb_state *mrb)
{
  mrb_gc *gc = &mrb->gc;

  if (!mrb->c) return;
  if (gc->disabled || gc->iterating) return;

#ifdef MRB_GC_PROFILE
  uint64_t prof_t0 = gc_prof_enter(gc, &gc->prof_sync);
#endif

#ifdef MRB_GC_STATS
  gc->gc_total_count++;
  gc->major_gc_count++;
#endif
  if (is_generational(gc)) {
    /* clear all the old objects back to young */
    clear_all_old(mrb, gc);
    gc->full = TRUE;
  }
  else if (gc->state != MRB_GC_STATE_ROOT) {
    /* finish half baked GC cycle */
    incremental_gc_finish(mrb, gc);
  }

  incremental_gc_finish(mrb, gc);
  {
    mrb_int credit = (mrb_int)((gc->live_after_mark/100) * gc->interval_ratio)
                   - (mrb_int)gc->live_after_mark;
    if (credit < (mrb_int)GC_STEP_SIZE) credit = (mrb_int)GC_STEP_SIZE;
    gc->gc_debt = -credit;
  }

  if (is_generational(gc)) {
    gc->oldgen_threshold = gc->live_after_mark/100 * MAJOR_GC_INC_RATIO;
    gc->full = FALSE;
  }

#ifdef MRB_USE_MALLOC_TRIM
  malloc_trim(0);
#endif

#ifdef MRB_GC_PROFILE
  gc_prof_leave(gc, prof_t0);
#endif
}

MRB_API void
mrb_garbage_collect(mrb_state *mrb)
{
  mrb_full_gc(mrb);
}

/*
 * Field write barrier
 *   Paint obj(Black) -> value(White) to obj(Black) -> value(Gray).
 */

MRB_API void
mrb_field_write_barrier(mrb_state *mrb, struct RBasic *obj, struct RBasic *value)
{
  mrb_gc *gc = &mrb->gc;

  if (!value) return;
  if (!is_black(obj)) return;
  if (!is_white(value)) return;
  if (is_red(value)) return;

  mrb_assert(gc->state == MRB_GC_STATE_MARK || (!is_dead(gc, value) && !is_dead(gc, obj)));
  mrb_assert(is_generational(gc) || gc->state != MRB_GC_STATE_ROOT);

  if (is_generational(gc) || gc->state == MRB_GC_STATE_MARK) {
    add_gray_list(gc, value);
  }
  else {
    mrb_assert(gc->state == MRB_GC_STATE_SWEEP);
    paint_partial_white(gc, obj); /* for never write barriers */
  }
}

/*
 * Write barrier
 *   Paint obj(Black) to obj(Gray).
 *
 *   The object that is painted gray will be traversed atomically in final
 *   mark phase. So you use this write barrier if it's frequency written spot.
 *   e.g. Set element on Array.
 */

MRB_API void
mrb_write_barrier(mrb_state *mrb, struct RBasic *obj)
{
  mrb_gc *gc = &mrb->gc;

  if (!is_black(obj)) return;

  mrb_assert(!is_dead(gc, obj));
  mrb_assert(is_generational(gc) || gc->state != MRB_GC_STATE_ROOT);
  paint_gray(obj);
  if (gc->gray_stack_top < MRB_GRAY_STACK_SIZE) {
    gc->gray_stack[gc->gray_stack_top++] = obj;
  }
  else {
    gc->gray_overflow = TRUE;
  }
}

/*
 *  call-seq:
 *     GC.start                     -> nil
 *
 *  Initiates full garbage collection.
 *
 */

static mrb_value
gc_start(mrb_state *mrb, mrb_value obj)
{
  mrb_full_gc(mrb);
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     GC.enable    -> true or false
 *
 *  Enables garbage collection, returning `true` if garbage
 *  collection was previously disabled.
 *
 *     GC.disable   #=> false
 *     GC.enable    #=> true
 *     GC.enable    #=> false
 *
 */

static mrb_value
gc_enable(mrb_state *mrb, mrb_value obj)
{
  mrb_bool old = mrb->gc.disabled;

  mrb->gc.disabled = FALSE;

  return mrb_bool_value(old);
}

/*
 *  call-seq:
 *     GC.disable    -> true or false
 *
 *  Disables garbage collection, returning `true` if garbage
 *  collection was already disabled.
 *
 *     GC.disable   #=> false
 *     GC.disable   #=> true
 *
 */

static mrb_value
gc_disable(mrb_state *mrb, mrb_value obj)
{
  mrb_bool old = mrb->gc.disabled;

  mrb->gc.disabled = TRUE;

  return mrb_bool_value(old);
}

/*
 *  call-seq:
 *     GC.interval_ratio      -> int
 *
 *  Returns ratio of GC interval. Default value is 200(%).
 *
 */

static mrb_value
gc_interval_ratio_get(mrb_state *mrb, mrb_value obj)
{
  return mrb_int_value(mrb, mrb->gc.interval_ratio);
}

/*
 *  call-seq:
 *     GC.interval_ratio = int    -> nil
 *
 *  Updates ratio of GC interval. Default value is 200(%).
 *  GC start as soon as after end all step of GC if you set 100(%).
 *
 */

static mrb_value
gc_interval_ratio_set(mrb_state *mrb, mrb_value obj)
{
  mrb_int ratio;

  mrb_get_args(mrb, "i", &ratio);
  mrb->gc.interval_ratio = (int)ratio;
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     GC.step_ratio    -> int
 *
 *  Returns step span ratio of Incremental GC. Default value is 200(%).
 *
 */

static mrb_value
gc_step_ratio_get(mrb_state *mrb, mrb_value obj)
{
  return mrb_int_value(mrb, mrb->gc.step_ratio);
}

/*
 *  call-seq:
 *     GC.step_ratio = int   -> nil
 *
 *  Updates step span ratio of Incremental GC. Default value is 200(%).
 *  1 step of incrementalGC becomes long if a rate is big.
 *  Must be positive.
 *
 */

static mrb_value
gc_step_ratio_set(mrb_state *mrb, mrb_value obj)
{
  mrb_int ratio;

  mrb_get_args(mrb, "i", &ratio);
  if (ratio <= 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "step_ratio must be positive");
  }
  mrb->gc.step_ratio = (int)ratio;
  return mrb_nil_value();
}

/*
 *  call-seq:
 *     GC.step_limit -> int
 *
 *  Returns the cap on the work done by one incremental GC step (0 = unlimited).
 *  Applies to every step regardless of what drives it: the ordinary
 *  allocation path, a manual GC.step, or a scheduler-driven step (see
 *  mruby-task's GC.scheduler_driven). Bounds the length of the single
 *  longest non-preemptible GC pause.
 */
static mrb_value
gc_step_limit_get(mrb_state *mrb, mrb_value obj)
{
  return mrb_int_value(mrb, (mrb_int)mrb->gc.step_limit);
}

/*
 *  call-seq:
 *     GC.step_limit = int -> int
 *
 *  Sets the cap on the work done by one incremental GC step. See GC.step_limit.
 */
static mrb_value
gc_step_limit_set(mrb_state *mrb, mrb_value obj)
{
  mrb_int limit;

  mrb_get_args(mrb, "i", &limit);
  if (limit < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "step_limit must be non-negative");
  }
  mrb->gc.step_limit = (size_t)limit;
  return mrb_int_value(mrb, limit);
}

/*
 *  call-seq:
 *     GC.malloc_threshold -> int
 *
 *  Returns the malloc-backed byte-growth threshold that triggers an
 *  incremental GC cycle (0 = disabled). Unlike GC.debt (which counts
 *  objects), this catches workloads that allocate few but large
 *  malloc-backed payloads (long String/Array buffers). It fires in two
 *  places: the ordinary allocation path triggers GC.start's incremental
 *  counterpart once malloc growth crosses this threshold even in stock
 *  auto_step mode, and mruby-task's GC.scheduler_driven also treats crossing
 *  it as a reason to step.
 */
static mrb_value
gc_malloc_threshold_get(mrb_state *mrb, mrb_value obj)
{
  return mrb_int_value(mrb, (mrb_int)mrb->gc.malloc_threshold);
}

/*
 *  call-seq:
 *     GC.malloc_threshold = int -> int
 *
 *  Sets the malloc-backed byte-growth threshold. See GC.malloc_threshold.
 */
static mrb_value
gc_malloc_threshold_set(mrb_state *mrb, mrb_value obj)
{
  mrb_int threshold;

  mrb_get_args(mrb, "i", &threshold);
  if (threshold < 0) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "malloc_threshold must be non-negative");
  }
  mrb->gc.malloc_threshold = (size_t)threshold;
  return mrb_int_value(mrb, threshold);
}

static void
change_gen_gc_mode(mrb_state *mrb, mrb_gc *gc, mrb_bool enable)
{
  if (gc->disabled || gc->iterating) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "generational mode changed when GC disabled");
    return;
  }
  if (enable && gc->sched_driven) {
    /* Scheduler-driven mode requires generational off: a minor cycle runs to
       completion inside one atomic step, which defeats fine-grained idle
       stepping. Silently allowing this would re-introduce exactly the pauses
       the mode exists to remove, so refuse. (mrb_gc_scheduler_driven itself
       only ever disables generational mode here, never enables it.) */
    mrb_raise(mrb, E_RUNTIME_ERROR, "generational mode conflicts with scheduler-driven GC");
    return;
  }
  if (is_generational(gc) && !enable) {
    clear_all_old(mrb, gc);
    mrb_assert(gc->state == MRB_GC_STATE_ROOT);
    gc->full = FALSE;
  }
  else if (!is_generational(gc) && enable) {
    incremental_gc_finish(mrb, gc);
    gc->oldgen_threshold = gc->live_after_mark/100 * MAJOR_GC_INC_RATIO;
    gc->full = FALSE;
  }
  gc->generational = enable;
}

/* One unit of incremental GC progress, unconditional except for the
   disabled/iterating guard. This is the entry point the task scheduler drives
   from its idle points; it deliberately ignores auto_step so collection still
   advances while the allocation path is not driving it. Pauses are attributed
   to prof_step (work moved off the allocation path). */
MRB_API mrb_int
mrb_gc_step(mrb_state *mrb)
{
  mrb_gc *gc = &mrb->gc;
  size_t work;

  if (gc->disabled || gc->iterating) return 0;

#ifdef MRB_GC_PROFILE
  {
    uint64_t prof_t0 = gc_prof_enter(gc, &gc->prof_step);
    work = incremental_gc_run(mrb, gc);
    gc->prof_last_step_us = gc_prof_leave(gc, prof_t0);
  }
#else
  work = incremental_gc_run(mrb, gc);
#endif
  return (mrb_int)work;
}

/* See mruby/gc.h. Records the last step's wall time as jitter when the
   scheduler reports that step delayed a task. */
MRB_API void
mrb_gc_scheduler_jitter(mrb_state *mrb, mrb_bool delayed_task)
{
#ifdef MRB_GC_PROFILE
  if (delayed_task) {
    mrb_gc *gc = &mrb->gc;
    gc_prof_record(&gc->prof_step_jitter, gc->prof_last_step_us);
  }
#else
  (void)mrb;
  (void)delayed_task;
#endif
}

/* Whether scheduler-driven GC has work worth doing right now: a cycle is in
   progress, object-debt credit has run out, or malloc-backed byte pressure has
   accumulated. Always FALSE when scheduler-driven mode is off, so the
   scheduler can gate on this alone. Keeps the "is a step warranted" policy
   inside gc.c; the scheduler only wires idle points to it. */
MRB_API mrb_bool
mrb_gc_scheduler_pending(mrb_state *mrb)
{
  mrb_gc *gc = &mrb->gc;

  if (!gc->sched_driven || gc->disabled || gc->iterating) return FALSE;
  if (gc->state != MRB_GC_STATE_ROOT) return TRUE;
  if (gc->gc_debt >= 0) return TRUE;
  if (gc->malloc_increase > 0) return TRUE;
  return FALSE;
}

/* Hand GC scheduling to the task scheduler (enable) or back to the allocation
   path (disable). See the header comment for the contract. */
MRB_API void
mrb_gc_scheduler_driven(mrb_state *mrb, mrb_bool enable)
{
  mrb_gc *gc = &mrb->gc;

  if (enable) {
    /* Refuse while GC is disabled or ObjectSpace is iterating, regardless of
       whether generational mode happens to be on (change_gen_gc_mode below
       would only catch the generational case): the contract is deterministic,
       and the raise leaves the driver flags unchanged. */
    if (gc->disabled || gc->iterating) {
      mrb_raise(mrb, E_RUNTIME_ERROR, "scheduler-driven GC enabled when GC disabled");
      return;
    }
    /* A minor cycle otherwise completes inside one atomic step, which defeats
       incremental scheduler stepping, so drop to non-generational first.
       change_gen_gc_mode refuses to re-enable generational mode while
       sched_driven is set, so the two flags cannot get out of sync later. */
    if (is_generational(gc)) {
      change_gen_gc_mode(mrb, gc, FALSE);
    }
    gc->auto_step = FALSE;
    gc->sched_driven = TRUE;
  }
  else {
    gc->sched_driven = FALSE;
    gc->auto_step = TRUE;
  }
}

/*
 *  call-seq:
 *     GC.generational_mode -> true or false
 *
 *  Returns generational or normal gc mode.
 *
 */

static mrb_value
gc_generational_mode_get(mrb_state *mrb, mrb_value self)
{
  return mrb_bool_value(mrb->gc.generational);
}

/*
 *  call-seq:
 *     GC.generational_mode = true or false -> true or false
 *
 *  Changes to generational or normal gc mode.
 *
 */

static mrb_value
gc_generational_mode_set(mrb_state *mrb, mrb_value self)
{
  mrb_bool enable;

  mrb_get_args(mrb, "b", &enable);
  if (mrb->gc.generational != enable)
    change_gen_gc_mode(mrb, &mrb->gc, enable);

  return mrb_bool_value(enable);
}


static void
gc_each_objects(mrb_state *mrb, mrb_gc *gc, mrb_each_object_callback *callback, void *data)
{
  mrb_heap_page* page;

  page = gc->heaps;
  while (page != NULL) {
    RVALUE *p;

    p = page->objects;
    for (int i=0; i < MRB_HEAP_PAGE_SIZE; i++) {
      if ((*callback)(mrb, &p[i].as.basic, data) == MRB_EACH_OBJ_BREAK)
        return;
    }
    page = page->next;
  }
}

void
mrb_objspace_each_objects(mrb_state *mrb, mrb_each_object_callback *callback, void *data)
{
  mrb_full_gc(mrb);
  if (mrb->gc.iterating) {
    gc_each_objects(mrb, &mrb->gc, callback, data);
  }
  else {
    struct mrb_jmpbuf *prev_jmp = mrb->jmp;
    struct mrb_jmpbuf c_jmp;

    MRB_TRY(&c_jmp) {
      mrb->jmp = &c_jmp;
      mrb->gc.iterating = TRUE;
      gc_each_objects(mrb, &mrb->gc, callback, data);
      mrb->jmp = prev_jmp;
      mrb->gc.iterating = FALSE;
    } MRB_CATCH(&c_jmp) {
      mrb->gc.iterating = FALSE;
      mrb->jmp = prev_jmp;
      MRB_THROW(prev_jmp);
    } MRB_END_EXC(&c_jmp);
  }
}

size_t
mrb_objspace_page_slot_size(void)
{
  return sizeof(RVALUE);
}


/*
 *  call-seq:
 *     GC.stat    -> Hash
 *
 *  Returns a Hash with GC statistics.
 *  Keys: :live, :debt, :state, :generational, :full,
 *        :step_limit, :malloc_increase, :malloc_threshold
 *  With MRB_GC_STATS: :total, :minor, :major
 *
 */

static mrb_value
gc_stat(mrb_state *mrb, mrb_value self)
{
  mrb_gc *gc = &mrb->gc;
  mrb_value hash = mrb_hash_new_capa(mrb, 8);

  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(live)), mrb_int_value(mrb, (mrb_int)gc->live));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(debt)), mrb_int_value(mrb, gc->gc_debt));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(state)), mrb_int_value(mrb, (mrb_int)gc->state));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(generational)), mrb_bool_value(gc->generational));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(full)), mrb_bool_value(gc->full));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(step_limit)), mrb_int_value(mrb, (mrb_int)gc->step_limit));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(malloc_increase)), mrb_int_value(mrb, (mrb_int)gc->malloc_increase));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(malloc_threshold)), mrb_int_value(mrb, (mrb_int)gc->malloc_threshold));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(symbol_count)), mrb_int_value(mrb, (mrb_int)(MRB_PRESYM_MAX + mrb->symidx)));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(dynamic_symbol_count)), mrb_int_value(mrb, (mrb_int)mrb->dynamic_sym_count));

#ifdef MRB_GC_STATS
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(total)), mrb_int_value(mrb, (mrb_int)gc->gc_total_count));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(minor)), mrb_int_value(mrb, (mrb_int)gc->minor_gc_count));
  mrb_hash_set(mrb, hash, mrb_symbol_value(MRB_SYM(major)), mrb_int_value(mrb, (mrb_int)gc->major_gc_count));
#endif

#ifdef MRB_GC_PROFILE
  /* Profiling keys are only present when MRB_GC_PROFILE is compiled in;
     the interned literals avoid depending on presym regeneration. Two pause
     populations are reported separately: :prof_sync_* are the synchronous
     mutator pauses this feature aims to shrink, :prof_step_* are the pauses
     relocated onto the scheduler's idle-time GC steps. */
  {
    int i;
    mrb_gc_prof_hist *hs[3];
    const char *pfx[3];
    hs[0] = &gc->prof_sync;         pfx[0] = "prof_sync";
    hs[1] = &gc->prof_step;         pfx[1] = "prof_step";
    hs[2] = &gc->prof_step_jitter;  pfx[2] = "prof_step_jitter";
    for (i = 0; i < 3; i++) {
      char key[40];
      int j;
      mrb_value buckets;
      snprintf(key, sizeof(key), "%s_count", pfx[i]);
      mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_cstr(mrb, key)), mrb_int_value(mrb, (mrb_int)hs[i]->count));
      snprintf(key, sizeof(key), "%s_total_us", pfx[i]);
      mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_cstr(mrb, key)), mrb_int_value(mrb, (mrb_int)hs[i]->total_us));
      snprintf(key, sizeof(key), "%s_max_us", pfx[i]);
      mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_cstr(mrb, key)), mrb_int_value(mrb, (mrb_int)hs[i]->max_us));
      snprintf(key, sizeof(key), "%s_hist", pfx[i]);
      buckets = mrb_ary_new_capa(mrb, MRB_GC_PROFILE_NBUCKETS);
      for (j = 0; j < MRB_GC_PROFILE_NBUCKETS; j++) {
        mrb_ary_push(mrb, buckets, mrb_int_value(mrb, (mrb_int)hs[i]->buckets[j]));
      }
      mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_cstr(mrb, key)), buckets);
    }
    mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_lit(mrb, "prof_final_mark_max_us")), mrb_int_value(mrb, (mrb_int)gc->prof_final_mark_max_us));
    mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_lit(mrb, "prof_final_mark_max_live")), mrb_int_value(mrb, (mrb_int)gc->prof_final_mark_max_live));
    mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_lit(mrb, "prof_mark_work_total")), mrb_int_value(mrb, (mrb_int)gc->prof_mark_work_total));
    mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_lit(mrb, "prof_sweep_work_total")), mrb_int_value(mrb, (mrb_int)gc->prof_sweep_work_total));
    mrb_hash_set(mrb, hash, mrb_symbol_value(mrb_intern_lit(mrb, "prof_emergency_count")), mrb_int_value(mrb, (mrb_int)gc->prof_emergency_count));
  }
#endif

  return hash;
}

#ifdef MRB_GC_PROFILE
/*
 *  call-seq:
 *     GC.reset_stat  -> nil
 *
 *  Zeroes the MRB_GC_PROFILE pause/work counters. Only defined when the
 *  profiler is compiled in. Lets a benchmark discard warm-up before the
 *  measured window.
 */
static mrb_value
gc_reset_stat(mrb_state *mrb, mrb_value self)
{
  mrb_gc *gc = &mrb->gc;
  memset(&gc->prof_sync, 0, sizeof(gc->prof_sync));
  memset(&gc->prof_step, 0, sizeof(gc->prof_step));
  memset(&gc->prof_step_jitter, 0, sizeof(gc->prof_step_jitter));
  gc->prof_last_step_us = 0;
  gc->prof_final_mark_max_us = 0;
  gc->prof_final_mark_max_live = 0;
  gc->prof_mark_work_total = 0;
  gc->prof_sweep_work_total = 0;
  gc->prof_emergency_count = 0;
  return mrb_nil_value();
}
#endif

void
mrb_init_gc(mrb_state *mrb)
{
  struct RClass *gc;

#if defined(MRB_WORD_BOXING) && defined(MRB_32BIT) && defined(MRB_USE_FLOAT32) && !defined(MRB_WORDBOX_NO_INLINE_FLOAT)
  /* 6 words: padded to 8-byte alignment for inline float word boxing */
  mrb_static_assert(sizeof(RVALUE) <= sizeof(void*) * 6,
                    "RVALUE size must be within 6 words");
#else
  mrb_static_assert_object_size(RVALUE);
#endif

  gc = mrb_define_module_id(mrb, MRB_SYM(GC));

  mrb_define_class_method_id(mrb, gc, MRB_SYM(stat), gc_stat, MRB_ARGS_NONE());
#ifdef MRB_GC_PROFILE
  mrb_define_class_method(mrb, gc, "reset_stat", gc_reset_stat, MRB_ARGS_NONE());
#endif
  mrb_define_class_method_id(mrb, gc, MRB_SYM(start), gc_start, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM(enable), gc_enable, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM(disable), gc_disable, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM(interval_ratio), gc_interval_ratio_get, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM_E(interval_ratio), gc_interval_ratio_set, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, gc, MRB_SYM(step_ratio), gc_step_ratio_get, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM_E(step_ratio), gc_step_ratio_set, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, gc, MRB_SYM(step_limit), gc_step_limit_get, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM_E(step_limit), gc_step_limit_set, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, gc, MRB_SYM(malloc_threshold), gc_malloc_threshold_get, MRB_ARGS_NONE());
  mrb_define_class_method_id(mrb, gc, MRB_SYM_E(malloc_threshold), gc_malloc_threshold_set, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, gc, MRB_SYM_E(generational_mode), gc_generational_mode_set, MRB_ARGS_REQ(1));
  mrb_define_class_method_id(mrb, gc, MRB_SYM(generational_mode), gc_generational_mode_get, MRB_ARGS_NONE());
}
