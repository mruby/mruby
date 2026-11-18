/*
** finalizer.c - ObjectSpace.define_finalizer and undefine_finalizer
**
** See Copyright Notice in mruby.h
**
** The objects that have finalizers are kept in a table the GC does not mark
** through (mrb_gc_weak_add). Once marking is done, an object nothing else
** marked is taken out of the table and its finalizers are queued, before
** the sweep frees it. The GC may not run Ruby code, so the queue is run
** later, where Ruby code can run: at the next define_finalizer, after
** GC.start, and when the state is closed, after the at_exit blocks, where
** the finalizers of the objects still alive run too, as CRuby runs them at
** exit.
**
** The finalizers of an object are kept in an Array, its object_id followed
** by the callables, in a slot of a list the table holds under a hidden
** instance variable. The table maps the object to its slot.
*/

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/data.h>
#include <mruby/error.h>
#include <mruby/gc.h>
#include <mruby/khash.h>
#include <mruby/variable.h>
#include <mruby/internal.h>
#include <mruby/presym.h>

#define fin_hash_func(mrb,key) mrb_int_hash_func(mrb, (intptr_t)(key) >> 4)
#define fin_hash_equal(mrb,a,b) ((a) == (b))
KHASH_DECLARE(fin, struct RBasic*, uint32_t, TRUE)
KHASH_DEFINE(fin, struct RBasic*, uint32_t, TRUE, fin_hash_func, fin_hash_equal)

struct fin {
  struct mrb_gc_weak weak;      /* first, so the GC's node is the table */
  kh_fin_t *objs;               /* an object with finalizers -> its slot */
  uint32_t *dead;               /* the slots of collected objects, to run */
  uint32_t ndead;
  uint32_t *free;               /* slots to use again */
  uint32_t nfree;
  uint32_t capa;                /* room in dead and in free: one per slot */
  uint32_t nslots;
  mrb_bool running;
};

static void
fin_free(mrb_state *mrb, void *p)
{
  struct fin *f = (struct fin*)p;

  mrb_gc_weak_remove(mrb, &f->weak);
  kh_destroy(fin, mrb, f->objs);
  mrb_free(mrb, f->dead);
  mrb_free(mrb, f->free);
  mrb_free(mrb, f);
}

static const struct mrb_data_type fin_type = { "finalizers", fin_free };

/* Called by the GC once marking is complete: it may not allocate, so the
   slot goes into dead, which has room for every slot. */
static void
fin_clear_dead(mrb_state *mrb, struct mrb_gc_weak *weak)
{
  struct fin *f = (struct fin*)weak;
  kh_fin_t *h = f->objs;
  khint_t k = kh_begin(h);

  while (!kh_is_end(h, k)) {
    if (kh_exist(fin, h, k) && mrb_gc_weak_dead_p(mrb, mrb_obj_value(kh_key(fin, h, k)))) {
      khint_t end = kh_end(h);
      f->dead[f->ndead++] = kh_value(fin, h, k);
      kh_del(fin, mrb, h, k);
      /* a small table moves the keys after this one down */
      if (kh_end(h) < end) continue;
    }
    k++;
  }
}

static struct fin*
fin_get(mrb_state *mrb, mrb_value *list)
{
  struct RClass *os = mrb_module_get_id(mrb, MRB_SYM(ObjectSpace));
  mrb_value d = mrb_obj_iv_get(mrb, (struct RObject*)os, MRB_SYM(__finalizers__));

  *list = mrb_iv_get(mrb, d, MRB_SYM(__list__));
  return (struct fin*)mrb_data_get_ptr(mrb, d, &fin_type);
}

static uint32_t
fin_slot_new(mrb_state *mrb, struct fin *f)
{
  if (f->nfree > 0) return f->free[--f->nfree];
  if (f->nslots == f->capa) {
    uint32_t capa = f->capa ? f->capa * 2 : 8;
    /* each may collect, which only adds to dead, within the old room */
    f->dead = (uint32_t*)mrb_realloc(mrb, f->dead, capa * sizeof(uint32_t));
    f->free = (uint32_t*)mrb_realloc(mrb, f->free, capa * sizeof(uint32_t));
    f->capa = capa;
  }
  return f->nslots++;
}

/* Takes the finalizers out of a slot, which can then be used again. */
static mrb_value
fin_take(mrb_state *mrb, struct fin *f, mrb_value list, uint32_t slot)
{
  mrb_value procs = mrb_ary_entry(list, slot);

  if (!mrb_array_p(procs)) return mrb_nil_value();
  mrb_ary_set(mrb, list, slot, mrb_nil_value());
  f->free[f->nfree++] = slot;
  return procs;
}

struct fin_call {
  mrb_value callable;
  mrb_value id;
  mrb_value exc;
};

static mrb_value
fin_call_body(mrb_state *mrb, void *p)
{
  struct fin_call *c = (struct fin_call*)p;
  return mrb_funcall_argv(mrb, c->callable, MRB_SYM(call), 1, &c->id);
}

/* the report inspects the callable and the exception, which may raise too */
static mrb_value
fin_warn_body(mrb_state *mrb, void *p)
{
  struct fin_call *c = (struct fin_call*)p;
  mrb_warn(mrb, "Exception in finalizer %!v: %!v", c->callable, c->exc);
  return mrb_nil_value();
}

/* Calls each finalizer with the object_id; one that raises is reported and
   the rest still run, as in CRuby. */
static void
fin_call(mrb_state *mrb, mrb_value procs)
{
  int ai0 = mrb_gc_arena_save(mrb);

  mrb_gc_protect(mrb, procs);
  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 1; i < RARRAY_LEN(procs); i++) {
    struct fin_call c = { RARRAY_PTR(procs)[i], RARRAY_PTR(procs)[0], mrb_nil_value() };
    mrb_bool error;
    c.exc = mrb_protect_error(mrb, fin_call_body, &c, &error);
    if (error) {
      mrb_protect_error(mrb, fin_warn_body, &c, &error);
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  mrb_gc_arena_restore(mrb, ai0);
}

/* Runs the finalizers of the objects collected so far. A finalizer that
   leads here again leaves the rest to the run already going. */
static void
fin_run(mrb_state *mrb)
{
  mrb_value list;
  struct fin *f = fin_get(mrb, &list);

  if (f->running) return;
  f->running = TRUE;
  while (f->ndead > 0) {
    mrb_value procs = fin_take(mrb, f, list, f->dead[--f->ndead]);
    if (mrb_array_p(procs)) fin_call(mrb, procs);
  }
  f->running = FALSE;
}

/* At exit: what was collected, then every object still alive, in the order
   of their slots, again while the finalizers define more. Each round takes
   all the slots out before it calls any, so that the finalizers defined
   meanwhile are left whole to the next round. */
static void
fin_atexit(mrb_state *mrb)
{
  mrb_value list;
  struct fin *f = fin_get(mrb, &list);
  int ai = mrb_gc_arena_save(mrb);

  fin_run(mrb);
  f->running = TRUE;
  for (;;) {
    mrb_value batch = mrb_ary_new(mrb);

    kh_clear(fin, mrb, f->objs);
    f->ndead = 0;
    for (uint32_t slot = 0; slot < f->nslots; slot++) {
      mrb_value procs = mrb_ary_entry(list, slot);
      if (!mrb_array_p(procs)) continue;
      /* in the batch before the list lets it go: pushing may collect */
      mrb_ary_push(mrb, batch, procs);
      fin_take(mrb, f, list, slot);
    }
    if (RARRAY_LEN(batch) == 0) break;
    for (mrb_int i = 0; i < RARRAY_LEN(batch); i++) {
      fin_call(mrb, RARRAY_PTR(batch)[i]);
    }
    mrb_gc_arena_restore(mrb, ai);
  }
  f->running = FALSE;
}

/*
 * call-seq:
 *   ObjectSpace.define_finalizer(obj, callable)  -> [0, callable]
 *   ObjectSpace.define_finalizer(obj) { |id| }   -> [0, proc]
 *
 * Calls `callable` with the object_id of `obj` once `obj` is collected,
 * at the next define_finalizer, after GC.start, or at exit, whichever
 * comes first. A callable that refers to `obj` keeps it alive.
 */
static mrb_value
os_define_finalizer(mrb_state *mrb, mrb_value self)
{
  mrb_value obj, callable, blk;
  mrb_int argc = mrb_get_args(mrb, "o|o&", &obj, &callable, &blk);

  if (argc < 2) {
    if (mrb_nil_p(blk)) {
      mrb_raise(mrb, E_ARGUMENT_ERROR, "tried to create Proc object without a block");
    }
    callable = blk;
  }
  if (mrb_immediate_p(obj)) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "cannot define finalizer for %T", obj);
  }
  mrb_check_frozen(mrb, mrb_basic_ptr(obj));
  if (!mrb_respond_to(mrb, callable, MRB_SYM(call))) {
    mrb_raisef(mrb, E_ARGUMENT_ERROR, "wrong type argument %T (should be callable)", callable);
  }
  fin_run(mrb);

  mrb_value list;
  struct fin *f = fin_get(mrb, &list);
  struct RBasic *p = mrb_basic_ptr(obj);
  khint_t k = kh_get(fin, mrb, f->objs, p);

  if (k != kh_end(f->objs)) {
    mrb_value procs = mrb_ary_entry(list, kh_value(fin, f->objs, k));
    for (mrb_int i = 1; i < RARRAY_LEN(procs); i++) {
      if (mrb_obj_eq(mrb, RARRAY_PTR(procs)[i], callable)) goto done;
    }
    mrb_ary_push(mrb, procs, callable);
  }
  else {
    uint32_t slot = fin_slot_new(mrb, f);
    mrb_value procs = mrb_ary_new_capa(mrb, 2);
    mrb_ary_push(mrb, procs, mrb_int_value(mrb, mrb_obj_id(obj)));
    mrb_ary_push(mrb, procs, callable);
    mrb_ary_set(mrb, list, slot, procs);
    k = kh_put(fin, mrb, f->objs, p);
    kh_value(fin, f->objs, k) = slot;
  }
 done:
  return mrb_assoc_new(mrb, mrb_fixnum_value(0), callable);
}

/*
 * call-seq:
 *   ObjectSpace.undefine_finalizer(obj)  -> obj
 *
 * Drops the finalizers of `obj`.
 */
static mrb_value
os_undefine_finalizer(mrb_state *mrb, mrb_value self)
{
  mrb_value obj = mrb_get_arg1(mrb);

  mrb_check_frozen_value(mrb, obj);

  mrb_value list;
  struct fin *f = fin_get(mrb, &list);
  khint_t k = kh_get(fin, mrb, f->objs, mrb_basic_ptr(obj));

  if (k != kh_end(f->objs)) {
    fin_take(mrb, f, list, kh_value(fin, f->objs, k));
    kh_del(fin, mrb, f->objs, k);
  }
  return obj;
}

/* called by GC.start once it has collected */
static void
fin_after_start(mrb_state *mrb, struct mrb_gc_weak *weak)
{
  fin_run(mrb);
}

void
mrb_mruby_objectspace_finalizer_gem_init(mrb_state *mrb)
{
  struct RClass *os = mrb_define_module_id(mrb, MRB_SYM(ObjectSpace));
  struct fin *f = (struct fin*)mrb_calloc(mrb, 1, sizeof(struct fin));

  f->objs = kh_init(fin, mrb);
  f->weak.clear = fin_clear_dead;
  f->weak.after = fin_after_start;
  mrb_value d = mrb_obj_value(mrb_data_object_alloc(mrb, mrb->object_class, f, &fin_type));
  mrb_gc_weak_add(mrb, &f->weak);
  mrb_iv_set(mrb, d, MRB_SYM(__list__), mrb_ary_new(mrb));
  mrb_obj_iv_set(mrb, (struct RObject*)os, MRB_SYM(__finalizers__), d);

  mrb_define_module_function_id(mrb, os, MRB_SYM(define_finalizer), os_define_finalizer, MRB_ARGS_ARG(1,1)|MRB_ARGS_BLOCK());
  mrb_define_module_function_id(mrb, os, MRB_SYM(undefine_finalizer), os_undefine_finalizer, MRB_ARGS_REQ(1));
  /* registered before any at_exit block, so it runs after them */
  mrb_state_atexit(mrb, fin_atexit);
}

void
mrb_mruby_objectspace_finalizer_gem_final(mrb_state *mrb)
{
}
