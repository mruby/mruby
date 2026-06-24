#include <string.h>
#include "../include/mrc_ccontext.h"
#include "../include/mrc_parser_util.h"

#if defined(MRC_TARGET_MRUBY)
/* The Prism xallocator routes allocations through this mrb_state. Define it
   in the compiler library so every executable that links libmruby (not just
   the mrbc/mruby/mirb front-ends) resolves the symbol. The front-ends assign
   it unconditionally for the mruby target, so it must exist regardless of
   MRC_ALLOC_LIBC even though only the non-libc allocator dereferences it. */
mrb_state *global_mrb = NULL;
#endif

MRC_API mrc_ccontext *
mrc_ccontext_new(mrb_state *mrb)
{
  mrc_ccontext temp_c = {0};
#if defined(MRC_TARGET_MRUBY) && !defined(MRC_ALLOC_LIBC)
  global_mrb = mrb;
#endif
  temp_c.mrb = mrb;
  mrc_ccontext *c = (mrc_ccontext *)mrc_calloc((&temp_c), 1, sizeof(mrc_ccontext));
  c->p = (mrc_parser_state *)mrc_calloc((&temp_c), 1, sizeof(mrc_parser_state));
  c->mrb = temp_c.mrb;
  return c;
}


MRC_API void
mrc_ccontext_cleanup_local_variables(mrc_ccontext *cc)
{
  cc->keep_lv = FALSE;

  if (cc->options && cc->options->scopes) {
    for (int i = 0; i < cc->options->scopes[0].locals_count; i++) {
      mrc_free(cc, (void *)cc->options->scopes[0].locals[i].source);
    }
    mrc_free(cc, cc->options);
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
    /* pm_options_free() releases the scope and locals arrays but not the
       per-local name copies (they are PM_STRING_CONSTANT, which pm_string_free
       leaves alone) nor the options struct itself, so free those here. The
       copies must go first, before pm_options_free() releases the arrays. */
    for (size_t s = 0; s < c->options->scopes_count; s++) {
      pm_options_scope_t *scope = &c->options->scopes[s];
      for (size_t l = 0; l < scope->locals_count; l++) {
        mrc_free(c, (void *)scope->locals[l].source);
      }
    }
    pm_options_free(c->options);
    mrc_free(c, c->options);
    c->options = NULL;
  }
  if (c->filename_table) mrc_free(c, c->filename_table);
  if (c->filename) mrc_free(c, c->filename);
  pm_parser_free(c->p);
  mrc_diagnostic_list_free(c);
  if (c->p->lex_callback) {
    mrc_free(c, c->p->lex_callback);
  }
  mrc_free(c, c->p);
  mrc_free(c, c);
}
