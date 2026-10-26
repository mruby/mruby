#include <string.h>
#include "../include/mrc_ccontext.h"
#include "../include/mrc_parser_util.h"

#if defined(MRC_TARGET_MRUBY)
/* The Prism xallocator routes allocations through this mrb_state. Define it
   in the compiler library so every executable that links libmruby (not just
   the mrbc/mruby/mirb front-ends) resolves the symbol. The front-ends assign
   it unconditionally for the mruby target, so it must exist regardless of
   MRC_ALLOC_LIBC even though only the non-libc allocator dereferences it. */
#include <stddef.h>
#include <mruby/error.h>

MRC_THREAD_LOCAL mrb_state *global_mrb = NULL;
#endif

static void
ccontext_init(mrc_ccontext *c)
{
  c->p = (mrc_parser_state *)mrc_calloc(c, 1, sizeof(mrc_parser_state));
}

#if defined(MRC_TARGET_MRUBY)
static mrb_value
ccontext_init_body(mrb_state *mrb, void *c)
{
  (void)mrb;
  ccontext_init((mrc_ccontext *)c);
  return mrb_nil_value();
}
#endif

MRC_API mrc_ccontext *
mrc_ccontext_new(mrb_state *mrb)
{
  mrc_ccontext temp_c = {0};
#if defined(MRC_TARGET_MRUBY)
  /* The state Prism's allocator takes from; see prism_xallocator.h. */
  global_mrb = mrb;
#endif
  temp_c.mrb = mrb;
  mrc_ccontext *c = (mrc_ccontext *)mrc_calloc((&temp_c), 1, sizeof(mrc_ccontext));
  c->mrb = temp_c.mrb;
#if defined(MRC_TARGET_MRUBY)
  if (mrb && mrb->jmp) {
    /* The allocations after the first raise NoMemoryError when they fail:
       give back what was taken before passing the error on, or c is lost.
       Caught through mrb_protect_error() rather than MRB_TRY, which is C++
       where mruby is built with C++ exceptions and this file is not. */
    mrb_bool failed;
    mrb_value exc;

    exc = mrb_protect_error(mrb, ccontext_init_body, c, &failed);
    if (failed) {
      if (c->p) mrc_free(c, c->p);
      mrc_free(c, c);
      mrb_exc_raise(mrb, exc);
    }
    return c;
  }
#endif
  ccontext_init(c);
  return c;
}


MRC_API void
mrc_ccontext_cleanup_local_variables(mrc_ccontext *cc)
{
  cc->keep_lv = FALSE;

  if (cc->options && cc->options->scopes) {
    for (size_t i = 0; i < cc->options->scopes[0].locals_count; i++) {
      mrc_free(cc, (void *)cc->options->scopes[0].locals[i].source);
    }
    /* The scope and locals arrays are Prism's own allocations. */
    pm_options_cleanup(cc->options);
    mrc_free(cc, cc->options);
    cc->options = NULL;
  }
}

MRC_API const char *
mrc_ccontext_filename(mrc_ccontext *c, const char *s)
{
  if (s) {
    size_t len = strlen(s);
    char *p = (char*)mrc_malloc(c, len + 1);

    if (p == NULL) return NULL;
    memcpy(p, s, len + 1);
    if (c->filename) {
      mrc_free(c, c->filename);
    }
    c->filename = p;
  }
  return c->filename;
}

MRC_API void
mrc_ccontext_free(mrc_ccontext *c)
{
  if (c->options) {
    /* pm_options_cleanup() releases the scope and locals arrays but not the
       per-local name copies (a pm_string_t never owns its bytes) nor the
       options struct itself, so free those here. The copies must go first,
       before pm_options_cleanup() releases the arrays. */
    for (size_t s = 0; s < c->options->scopes_count; s++) {
      pm_options_scope_t *scope = &c->options->scopes[s];
      for (size_t l = 0; l < scope->locals_count; l++) {
        mrc_free(c, (void *)scope->locals[l].source);
      }
    }
    pm_options_cleanup(c->options);
    mrc_free(c, c->options);
    c->options = NULL;
  }
  if (c->filename_table) mrc_free(c, c->filename_table);
  if (c->filename) mrc_free(c, c->filename);
  /* A context that never parsed has no parser state to give back. The tree
     goes back with its arena, block by block and not node by node, so a
     tree of any depth costs one loop and no C stack. */
  if (c->p->arena) pm_parser_cleanup(c->p);
  pm_arena_cleanup(&c->node_arena);
  mrc_diagnostic_list_free(c);
  mrc_free(c, c->p);
  mrc_free(c, c);
}
