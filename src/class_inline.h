static inline struct RProc *
mrb_method_search_vm_proc(mrb_state *mrb, struct RProc *p, struct RClass **cp, mrb_sym mid)
{
#ifdef MRB_ENABLE_METHOD_CACHE
  struct RClass *c = *cp;
  struct mrb_mcache_entry *es = p->mcache.entries;
  int i;

  for(i = 0; i < MRB_METHOD_CACHE_SIZE; i++) {
    struct mrb_mcache_entry *e = &es[i];

    if (e->c == c && e->mid == mid) {
      return p->mcache.procs[i];
    }
  }
#endif

  return _mrb_method_search_vm(mrb, cp, mid);
}
