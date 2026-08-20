#include <mruby.h>
#include <mruby/gc.h>
#include <mruby/string.h>

/* Make `n` strings, register each `regs` times, unregister each `unregs`
   times, then collect and answer how many of them are still alive. The
   answer is `n` where the registrations still standing pin them and 0 where
   they do not, which is what tells one registration from two. */
static mrb_value
gc_root_survivors(mrb_state *mrb, mrb_value self)
{
  mrb_int n, regs, unregs;
  mrb_get_args(mrb, "iii", &n, &regs, &unregs);
  if (n < 1) mrb_raise(mrb, E_ARGUMENT_ERROR, "count must be positive");

  mrb_value *v = (mrb_value*)mrb_malloc(mrb, sizeof(mrb_value) * (size_t)n);
  int ai = mrb_gc_arena_save(mrb);
  mrb_full_gc(mrb);
  size_t before = mrb->gc.live;

  for (mrb_int i = 0; i < n; i++) {
    v[i] = mrb_str_new_lit(mrb, "a body long enough to sit on the heap");
    for (mrb_int r = 0; r < regs; r++) mrb_gc_register(mrb, v[i]);
    for (mrb_int u = 0; u < unregs; u++) mrb_gc_unregister(mrb, v[i]);
  }
  mrb_gc_arena_restore(mrb, ai);
  mrb_full_gc(mrb);
  mrb_full_gc(mrb);
  size_t after = mrb->gc.live;

  /* leave nothing pinned behind */
  for (mrb_int r = regs - unregs; r > 0; r--) {
    for (mrb_int i = 0; i < n; i++) mrb_gc_unregister(mrb, v[i]);
  }
  mrb_free(mrb, v);
  return mrb_int_value(mrb, (mrb_int)(after - before));
}

void
mrb_mruby_objectspace_gem_test(mrb_state *mrb)
{
  struct RClass *os = mrb_module_get(mrb, "ObjectSpace");
  mrb_define_module_function(mrb, os, "__gc_root_survivors",
                             gc_root_survivors, MRB_ARGS_REQ(3));
}
