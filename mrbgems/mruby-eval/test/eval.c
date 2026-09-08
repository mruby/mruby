#include <mruby.h>
#include <mruby/compile.h>
#include <mruby/proc.h>
#include <mruby/irep.h>

#ifndef MRB_NO_CONST_CACHE
static mrb_bool
const_cache_holds_irep(mrb_state *mrb, const mrb_irep *irep)
{
  for (int i=0; i<MRB_CONST_CACHE_SIZE; i++) {
    if (mrb->const_cache[i].irep == irep) return TRUE;
  }
  return FALSE;
}
#endif

/* Compiles a read of the named constant into its own irep, runs it once so
   OP_GETCONST records the answer under that irep, lets the collector free
   the irep along with its proc, and reports whether the constant cache
   still holds an entry keyed by the freed address. Only the address is
   compared; the irep is never read after the collection. Nothing in Ruby
   can ask this: seeing the stale entry from Ruby needs the allocator to
   hand the next irep the same address. Answers nil without a cache. */
static mrb_value
const_cache_dangles_after_irep_free(mrb_state *mrb, mrb_value self)
{
#ifndef MRB_NO_CONST_CACHE
  const char *name;
  mrb_get_args(mrb, "z", &name);

  int ai = mrb_gc_arena_save(mrb);
  mrb_ccontext *cxt = mrb_ccontext_new(mrb);
  struct mrb_parser_state *p = mrb_parse_string(mrb, name, cxt);
  struct RProc *proc = (p && p->nerr == 0) ? mrb_generate_code(mrb, p) : NULL;
  if (p) mrb_parser_free(p);
  mrb_ccontext_free(mrb, cxt);
  if (!proc) mrb_raise(mrb, E_RUNTIME_ERROR, "the constant read did not compile");

  const mrb_irep *irep = proc->body.irep;
  mrb_top_run(mrb, proc, mrb_top_self(mrb), 0);
  if (!const_cache_holds_irep(mrb, irep)) {
    mrb_raise(mrb, E_RUNTIME_ERROR, "the constant read did not fill the cache");
  }

  proc = NULL;
  mrb_gc_arena_restore(mrb, ai);
  mrb_full_gc(mrb);
  return mrb_bool_value(const_cache_holds_irep(mrb, irep));
#else
  return mrb_nil_value();
#endif
}

void
mrb_mruby_eval_gem_test(mrb_state *mrb)
{
  struct RClass *cls = mrb_define_module(mrb, "ConstCacheTest");
  mrb_define_module_function(mrb, cls, "dangles_after_irep_free?", const_cache_dangles_after_irep_free, MRB_ARGS_REQ(1));
}
