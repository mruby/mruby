/*
** gc.c - helpers for test/t/gc.rb
**
** mrb_gc_add_region() (mruby/gc.h) has no Ruby-visible face, so the one
** invariant asked about from here is that a buffer too small to hold a page
** once its base has been aligned is refused, rather than having its usable
** size wrap and pages carved out past its end.
*/

#include <mruby.h>
#include <mruby/gc.h>

void mrb_init_test_gc(mrb_state *mrb);

/* Hand mrb_gc_add_region() a buffer smaller than the alignment padding that
   precedes its first usable byte: `base` sits one byte past an aligned
   address, so `sizeof(void*) - 1` bytes are skipped, and the size offered is
   one less again so the buffer cannot span even the padding. The count of
   pages carved must be zero, and on both 32- and 64-bit targets it is the
   guard that returns it: before the guard the subtraction that finds the
   usable size wrapped and a page was carved from past the buffer, which a
   throwaway state keeps clear of the live heap either way. */
static mrb_value
gc_add_region_undersized(mrb_state *mrb, mrb_value self)
{
  mrb_state *sub = mrb_open_core();
  mrb_int pages = -1;

  if (sub) {
    unsigned char raw[32];
    uintptr_t align = sizeof(void*);
    uintptr_t aligned = ((uintptr_t)raw + align - 1) & ~(align - 1);
    unsigned char *base = (unsigned char*)(aligned + 1);
    pages = (mrb_int)mrb_gc_add_region(sub, base, (size_t)(align - 2));
    mrb_close(sub);
  }
  return mrb_int_value(mrb, pages);
}

void
mrb_init_test_gc(mrb_state *mrb)
{
  struct RClass *o = mrb->object_class;

  mrb_define_method(mrb, o, "__gc_add_region_undersized", gc_add_region_undersized, MRB_ARGS_NONE());
}
