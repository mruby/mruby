<!-- summary: Garbage Collector Internals -->

# Garbage Collector Internals

This document describes the internals of mruby's garbage collector
for developers working on `src/gc.c` and related code.

**Read this if you are:** modifying core data structures that hold
object references (and need to add write barriers), debugging
memory leaks or GC-related crashes, tuning GC performance for an
embedded target, or working on the GC code itself.

**For user-facing GC docs**, see
[gc-arena-howto.md](../guides/gc-arena-howto.md) (arena usage in C
extensions) and [memory.md](../guides/memory.md) (heap regions).

## Overview

mruby uses a **tri-color incremental mark-and-sweep** garbage collector
with an optional **generational mode**. The collector runs in small
incremental steps between VM instruction execution, avoiding long
pauses.

## Tri-Color Model

Every heap-allocated object has a color stored in
`RBasic::gc_color` (3 bits):

| Color | Value | Meaning |
| ----- | ----- | ------- |
| White (A or B) | 1 or 2 | Unmarked, candidate for collection |
| Gray | 0 | Marked, but children not yet scanned |
| Black | 4 | Fully marked and scanned |
| Red | 7 | Static/ROM object, never collected |

The GC uses two white types (A and B) in a flip-flop scheme. At the
start of each GC cycle, the meaning of "current white" is flipped by
XORing the white bits. This avoids recoloring all live objects at
cycle boundaries, which is an O(1) operation instead of O(n).

```c
#define is_dead(s, o) \
  (((o)->gc_color & other_white_part(s) & GC_WHITES) || \
   (o)->tt == MRB_TT_FREE)
```

An object is dead if it still carries the previous cycle's white color.

## Heap Structure

### Heap Pages

Objects are allocated from fixed-size heap pages:

```text
mrb_heap_page
+-- freelist        linked list of free slots
+-- next            next page in heap list
+-- free_next       next page with free slots
+-- old             old generation flag (generational mode)
+-- region          true if carved from a contiguous region
+-- objects[MRB_HEAP_PAGE_SIZE]   RVALUE array (default 1024)
```

Each page holds `MRB_HEAP_PAGE_SIZE` objects (default 1024). On
64-bit systems, a page is approximately 40 KB (40 bytes per slot).

### RVALUE Union

All mruby object types share the same slot size via a C union:

```text
RVALUE = union of {
  RBasic, RObject, RClass, RString, RArray, RHash,
  RRange, RData, RProc, REnv, RFiber, RException, ...
  struct { RBasic header; RVALUE *next; }  (free slot)
}
```

Free slots use the union space for a freelist pointer.

### Freelist

Each page maintains a singly-linked freelist of available slots.
Allocation pops from the freelist; deallocation during sweep
prepends to the freelist. The GC tracks pages with free slots in
`gc->free_heaps` for fast allocation.

### Heap Regions

For embedded systems with fixed memory banks, `mrb_gc_add_region()`
carves heap pages from a user-provided contiguous buffer:

```c
static uint8_t heap_buf[64 * 1024];
mrb_gc_add_region(mrb, heap_buf, sizeof(heap_buf));
```

Region pages are never freed by the GC (even if all objects die).
When region pages are exhausted, allocation falls back to `malloc()`.

## GC Phases

The GC operates as a three-state machine:

```text
GC_STATE_ROOT --> GC_STATE_MARK --> GC_STATE_SWEEP --> GC_STATE_ROOT
```

### Root Scan (GC\_STATE\_ROOT)

Marks objects directly reachable from the VM:

1. Global variables (`mrb_gc_mark_gv`)
2. GC arena (`gc->arena[0..arena_idx-1]`)
3. All built-in classes (Object, Class, Module, etc.)
4. Top-level self (`mrb->top_self`)
5. Current exception (`mrb->exc`)
6. Execution contexts (VM stacks, call info stacks, active fibers)
7. Task queues (if `MRB_USE_TASK_SCHEDULER` is defined)

After root scanning, the white color is flipped.

### Incremental Marking (GC\_STATE\_MARK)

Gray objects are popped from the gray stack and their children
marked. Each step processes a limited number of objects:

```text
limit = (GC_STEP_SIZE / 100) * step_ratio
```

With default `step_ratio = 200` and `GC_STEP_SIZE = 1024`, the
limit is 2048 objects per step.

When the gray stack is exhausted, the final marking phase re-marks
the arena and global variables to catch objects created during
marking, then transitions to sweep.

### Sweep (GC\_STATE\_SWEEP)

Iterates through heap pages. For each object:

- If dead (previous cycle's white): call `obj_free()`, return
  slot to freelist
- If alive: paint with current white for the next cycle

Sweep is also incremental: `gc->sweeps` tracks the current page
position between steps.

## Gray Stack

The gray stack is a fixed-size array of object pointers:

```c
struct RBasic *gray_stack[MRB_GRAY_STACK_SIZE];  /* default 1024 */
size_t gray_stack_top;
mrb_bool gray_overflow;
```

When the stack overflows, `gray_overflow` is set to `TRUE`. During
marking, `gc_gray_rescan()` scans the entire heap to find any gray
objects that could not be pushed. This guarantees correctness at the
cost of a full heap scan.

## Write Barriers

During incremental marking, a black (fully marked) object storing
a reference to a white (unmarked) object creates a dangerous edge
that could lead to premature collection. Write barriers prevent this.

### Field Write Barrier

Used when assigning a specific field:

```c
mrb_field_write_barrier(mrb, parent, child);
```

If `parent` is black and `child` is white:

- During marking or generational mode: paint `child` gray (add to
  gray stack for scanning)
- During sweep: paint `parent` with current white (demote it for
  next cycle)

### General Write Barrier

Used when an object has been modified but the specific child is
not known:

```c
mrb_write_barrier(mrb, obj);
```

Paints `obj` gray and pushes it onto the gray stack for re-scanning.

## GC Arena

The arena protects newly created objects from collection before
they are stored in a reachable location. Every `mrb_obj_alloc()`
automatically pushes the new object onto the arena.

C extensions must save and restore the arena index when creating
many temporary objects to prevent arena overflow:

```c
int ai = mrb_gc_arena_save(mrb);
/* create temporary objects */
mrb_gc_arena_restore(mrb, ai);
```

### Fixed vs Dynamic Arena

- **Dynamic** (default): arena grows by 1.5x when full. Risk of
  unbounded growth if arena is not properly managed.
- **Fixed** (`MRB_GC_FIXED_ARENA`): raises an exception on overflow.
  Arena size is `MRB_GC_ARENA_SIZE` (default 100).

### Permanent Registration

For long-lived C objects that must survive indefinitely:

```c
mrb_gc_register(mrb, obj);    /* add to permanent root */
mrb_gc_unregister(mrb, obj);  /* remove from root */
```

These store objects in a global array that is always marked as
part of the root set.

See [gc-arena-howto.md](../guides/gc-arena-howto.md) for detailed
usage patterns.

## Generational Mode

When enabled (default, unless `MRB_GC_TURN_OFF_GENERATIONAL` is
defined), the GC classifies objects into young and old generations.

### Minor GC

Only processes young objects. Pages where all objects are old are
marked with `page->old = TRUE` and skipped entirely during sweep.
Minor GC always runs to completion in a single step.

### Major GC

A full mark-and-sweep cycle that processes all objects. Triggered
when `gc->live > gc->oldgen_threshold`. Major GC runs
incrementally, like the non-generational collector.

After a major GC completes, the collector reverts to minor GC mode.
The old-generation threshold is recalculated:

```text
oldgen_threshold = live_after_mark * MAJOR_GC_INC_RATIO / 100
```

With `MAJOR_GC_INC_RATIO = 120`, a major GC triggers when live
objects exceed 120% of the last major GC's survivors.

### Transitioning Between Modes

```c
mrb_gc_generational_mode_set(mrb, TRUE);   /* enable */
mrb_gc_generational_mode_set(mrb, FALSE);  /* disable */
```

From Ruby: `GC.generational_mode = true/false`.

## Object Allocation

`mrb_obj_alloc()` is the core allocation function:

1. If `MRB_GC_STRESS` is defined, run a full GC
2. If `gc->live >= gc->threshold`, run `mrb_incremental_gc()`
3. Ensure arena has space (`gc_arena_keep`)
4. Pop an object from the freelist of `gc->free_heaps`
5. If no free pages, allocate a new page (`add_heap`)
6. Initialize the object (zero fill, set type and class)
7. Paint with current white color
8. Push onto arena (`gc_protect`)
9. Increment `gc->live`

## Object Freeing

`obj_free()` performs type-specific cleanup:

- **Objects/Exceptions**: free instance variable tables
- **Classes**: free method tables and instance variable tables
- **Arrays**: free heap buffer (if not embedded/shared)
- **Hashes**: free hash table
- **Strings**: free heap buffer (if not embedded/shared)
- **Data objects**: call user-provided `dfree` callback
- **Procs**: decrement irep reference count
- **Fibers**: free context (stacks)

The object's type is set to `MRB_TT_FREE` after freeing.

## Triggering GC

### Automatic

GC runs automatically when `gc->live >= gc->threshold` during
object allocation. After each cycle:

```text
threshold = (live_after_mark / 100) * interval_ratio
minimum: GC_STEP_SIZE (1024)
```

With default `interval_ratio = 200`, GC triggers when live objects
roughly double.

### Manual

```c
mrb_full_gc(mrb);          /* force complete GC cycle */
mrb_garbage_collect(mrb);  /* public API wrapper */
```

From Ruby: `GC.start`.

## Configuration

### Compile-Time

| Macro | Default | Description |
| ----- | ------- | ----------- |
| `MRB_HEAP_PAGE_SIZE` | 1024 | Objects per heap page |
| `MRB_GRAY_STACK_SIZE` | 1024 | Gray stack capacity |
| `MRB_GC_ARENA_SIZE` | 100 | Arena size (fixed mode) or initial size |
| `MRB_GC_FIXED_ARENA` | off | Use fixed-size arena |
| `MRB_GC_TURN_OFF_GENERATIONAL` | off | Disable generational mode |
| `MRB_GC_STRESS` | off | Full GC on every allocation (debug) |
| `MRB_USE_MALLOC_TRIM` | off | Call `malloc_trim()` after full GC |

### Runtime

From Ruby code:

```ruby
GC.interval_ratio = 200   # threshold = live * ratio / 100
GC.step_ratio = 200       # objects per incremental step
GC.generational_mode = true
GC.start                   # force full GC
GC.enable                  # re-enable GC
GC.disable                 # disable GC
```

## Source Files

| File | Contents |
| ---- | -------- |
| `src/gc.c` | GC implementation (~1400 lines) |
| `include/mruby/gc.h` | `mrb_gc` structure, public GC API |
| `include/mruby.h` | Arena save/restore macros |
