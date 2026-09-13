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

mrb_state *global_mrb = NULL;

#if defined(MRC_TARGET_MRUBY) && defined(MRC_PRISM_ARENA)
/* The arena prism allocates a parse from.  Blocks are handed out by bumping a
   pointer; giving the arena back walks the chain of blocks rather than the
   tree, so a tree of any depth costs one loop and no C stack.  See
   prism_xallocator.h for why the tree is not walked.

   The blocks come from mrb_malloc(), so that a parse is on the allocator the
   state was opened with, except where Prism is compiled as C beside a C++
   core: mrb_malloc() raises on failure and the throw would pass through
   Prism's frames, so the blocks come from libc there.  See the gem's
   mrbgem.rake. */
#if defined(MRC_PRISM_ARENA_LIBC)
# include <stdlib.h>
# define arena_block_alloc(size)  malloc(size)
# define arena_block_free(ptr)    free(ptr)
#else
# define arena_block_alloc(size)  mrb_malloc(global_mrb, size)
# define arena_block_free(ptr)    mrb_free(global_mrb, ptr)
#endif

struct mrc_prism_arena_block *mrc_prism_arena = NULL;

#ifndef MRC_PRISM_ARENA_BLOCK
#define MRC_PRISM_ARENA_BLOCK (64 * 1024)
#endif

struct arena_block {
  struct mrc_prism_arena_block head;   /* must be first: the public view */
  size_t used, size;
};

/* Every answer carries the size it was given, so that growing one knows how
   much of it there is to copy.  realloc() is told the new size only. */
struct arena_chunk {
  size_t size;
  char body[];
};

static struct arena_block *
arena_block_new(size_t need)
{
  size_t size = MRC_PRISM_ARENA_BLOCK;
  while (size - sizeof(struct arena_block) < need) size *= 2;
  struct arena_block *b = (struct arena_block *)arena_block_alloc(size);
  b->head.prev = mrc_prism_arena;
  b->used = sizeof(struct arena_block);
  b->size = size;
  mrc_prism_arena = &b->head;
  return b;
}

/* Open an arena for one compiler context, putting aside the arena of the
   context this one is being made inside of.  Contexts are made and freed in
   the order of the calls that make them, so putting the outer one aside here
   and back at mrc_ccontext_free() leaves each context taking from its own. */
static void
arena_open(mrc_ccontext *c)
{
  c->prism_arena_outer = mrc_prism_arena;
  mrc_prism_arena = NULL;
  arena_block_new(0);
  c->prism_arena = mrc_prism_arena;
}

void *
mrc_prism_arena_alloc(size_t size)
{
  size_t need = (sizeof(struct arena_chunk) + size + 7u) & ~(size_t)7;
  struct arena_block *b = (struct arena_block *)mrc_prism_arena;

  if (b == NULL || b->size - b->used < need) {
    b = arena_block_new(need);
  }
  struct arena_chunk *chunk = (struct arena_chunk *)((char *)b + b->used);
  b->used += need;
  chunk->size = size;
  return chunk->body;
}

void *
mrc_prism_arena_realloc(void *ptr, size_t size)
{
  void *p = mrc_prism_arena_alloc(size);

  if (ptr != NULL && p != NULL) {
    /* The old bytes are left where they are: the arena gives everything back
       at once, so what a growing array leaves behind is reclaimed with it.
       How much is left behind is bounded by the doubling the caller does. */
    struct arena_chunk *old = (struct arena_chunk *)((char *)ptr - offsetof(struct arena_chunk, body));
    size_t copy = old->size < size ? old->size : size;
    memcpy(p, ptr, copy);
  }
  return p;
}

/* Give back everything this context took, and make the arena of the context
   it was made inside of the one that is open again. */
static void
arena_close(mrc_ccontext *c)
{
  struct mrc_prism_arena_block *b = mrc_prism_arena;

  while (b != NULL) {
    struct mrc_prism_arena_block *prev = b->prev;
    arena_block_free(b);
    b = prev;
  }
  c->prism_arena = NULL;
  mrc_prism_arena = (struct mrc_prism_arena_block *)c->prism_arena_outer;
  c->prism_arena_outer = NULL;
}
#endif
#endif

MRC_API mrc_ccontext *
mrc_ccontext_new(mrb_state *mrb)
{
  mrc_ccontext temp_c = {0};
#if defined(MRC_TARGET_MRUBY) && defined(MRC_PRISM_ARENA)
  global_mrb = mrb;
#endif
  temp_c.mrb = mrb;
  mrc_ccontext *c = (mrc_ccontext *)mrc_calloc((&temp_c), 1, sizeof(mrc_ccontext));
  c->p = (mrc_parser_state *)mrc_calloc((&temp_c), 1, sizeof(mrc_parser_state));
  c->mrb = temp_c.mrb;
#if defined(MRC_TARGET_MRUBY) && defined(MRC_PRISM_ARENA)
  /* Before Prism is asked for anything on this context's behalf, so that
     every pointer its allocator sees for this context is arena memory. */
  arena_open(c);
#endif
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
#if defined(MRC_TARGET_MRUBY) && defined(MRC_PRISM_ARENA)
  /* Everything Prism took for this context, the tree and what the parser
     kept beside it, came from the arena and is given back here in one piece.
     After pm_parser_free(), which reaches into that same arena. */
  arena_close(c);
#endif
  mrc_diagnostic_list_free(c);
  if (c->p->lex_callback) {
    mrc_free(c, c->p->lex_callback);
  }
  mrc_free(c, c->p);
  mrc_free(c, c);
}
