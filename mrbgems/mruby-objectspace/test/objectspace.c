#include <mruby.h>
#include <mruby/array.h>
#include <mruby/gc.h>
#include <mruby/string.h>
#include <mruby/internal.h>

struct survivors {
  mrb_value *v;
  char *alive;
  mrb_int n;
};

static int
survivors_mark(mrb_state *mrb, struct RBasic *obj, void *data)
{
  struct survivors *sv = (struct survivors*)data;

  for (mrb_int i = 0; i < sv->n; i++) {
    if (mrb_basic_ptr(sv->v[i]) == obj) sv->alive[i] = 1;
  }
  return MRB_EACH_OBJ_OK;
}

/* Take one string per [registrations, unregistrations] pair, apply the pair to
   it, then collect once and answer which of them the collection left alone.
   Survival is read off the heap walk rather than off the object: a swept
   object's cell is not to be read, as the sweep frees a page it emptied, and
   with MRB_HEAP_PAGE_SIZE=1 every swept object empties its page. Nothing is
   allocated between the collection and the walk, so no cell is handed out
   again in between and a pointer the walk finds is the object that was made
   for it.

   One collection for the whole table rather than one per pair: under
   MRB_GC_STRESS a collection runs at every allocation, and this test is in the
   path of every build. */
static mrb_value
gc_root_survivors(mrb_state *mrb, mrb_value self)
{
  mrb_value spec;
  mrb_get_args(mrb, "A", &spec);

  mrb_int n = RARRAY_LEN(spec);
  if (n < 1) mrb_raise(mrb, E_ARGUMENT_ERROR, "no pairs given");
  mrb_value *v = (mrb_value*)mrb_malloc(mrb, sizeof(mrb_value) * (size_t)n);
  char *alive = (char*)mrb_malloc(mrb, (size_t)n);

  int ai = mrb_gc_arena_save(mrb);
  for (mrb_int i = 0; i < n; i++) {
    mrb_value pair = RARRAY_PTR(spec)[i];
    mrb_int regs = mrb_as_int(mrb, RARRAY_PTR(pair)[0]);
    mrb_int unregs = mrb_as_int(mrb, RARRAY_PTR(pair)[1]);

    /* long enough not to sit inside the object header */
    v[i] = mrb_str_new_lit(mrb, "a body long enough to sit on the heap");
    for (mrb_int r = 0; r < regs; r++) mrb_gc_register(mrb, v[i]);
    for (mrb_int u = 0; u < unregs; u++) mrb_gc_unregister(mrb, v[i]);
  }
  /* the arena was all that held the ones no registration stands on */
  mrb_gc_arena_restore(mrb, ai);
  mrb_full_gc(mrb);

  struct survivors sv = { v, alive, n };
  for (mrb_int i = 0; i < n; i++) alive[i] = 0;
  mrb_gc_each_live_object(mrb, survivors_mark, &sv);

  mrb_value result = mrb_ary_new_capa(mrb, n);
  for (mrb_int i = 0; i < n; i++) {
    mrb_ary_push(mrb, result, mrb_bool_value(alive[i]));
    if (alive[i]) {
      mrb_value pair = RARRAY_PTR(spec)[i];
      mrb_int left = mrb_as_int(mrb, RARRAY_PTR(pair)[0]) -
                     mrb_as_int(mrb, RARRAY_PTR(pair)[1]);
      while (left-- > 0) mrb_gc_unregister(mrb, v[i]);  /* leave nothing pinned */
    }
  }
  mrb_free(mrb, alive);
  mrb_free(mrb, v);
  return result;
}

void
mrb_mruby_objectspace_gem_test(mrb_state *mrb)
{
  struct RClass *os = mrb_module_get(mrb, "ObjectSpace");
  mrb_define_module_function(mrb, os, "__gc_root_survivors",
                             gc_root_survivors, MRB_ARGS_REQ(1));
}
