#include <mruby.h>
#include <mruby/array.h>
#include <mruby/gc.h>
#include <mruby/string.h>

/* Take one string per [registrations, unregistrations] pair, apply the pair to
   it, then collect once and answer which of them the collection left alone. A
   swept object wears MRB_TT_FREE, which is how the collector itself tells a
   dead object from a live one; nothing is allocated between the collection and
   the reading, so no slot is handed out again in between.

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

  for (mrb_int i = 0; i < n; i++) {
    /* the object's own tag, not the value's: under MRB_NO_BOXING an mrb_value
       carries a tag of its own, and this copy of it keeps saying MRB_TT_STRING
       after the object behind it is gone */
    alive[i] = (char)(mrb_basic_ptr(v[i])->tt != MRB_TT_FREE);
  }

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
