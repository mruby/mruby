#ifndef MRC_CCONTEXT_H
#define MRC_CCONTEXT_H

#include "mrc_common.h"
#include "mrc_diagnostic.h"
#include "mrc_throw.h"
#include "mrc_pool.h"
#include <stddef.h>

MRC_BEGIN_DECL

typedef pm_node_t mrc_node;
typedef pm_parser_t mrc_parser_state;
typedef pm_constant_id_list_t mrc_constant_id_list;
typedef struct {
  pm_parser_t parser;
  pm_options_t options;
  pm_string_t input;
  bool parsed;
} pm_parse_result_t;

struct mrc_diagnostic_list;

typedef struct mrc_filename_table {
  const char *filename;
  uint32_t start;
} mrc_filename_table;

typedef struct mrc_ccontext {
  mrb_state *mrb;
  struct mrc_jmpbuf *jmp;
  mrc_parser_state *p;
  pm_options_t *options; // instead of mrb_sym *syms
  int slen;
  char *filename;
  uint16_t lineno;
  struct RClass *target_class;
  mrc_bool capture_errors:1;   /* output: an error was recorded */
  mrc_bool quiet_errors:1;     /* input: caller reports them itself (eval) */
  mrc_bool dump_ast:1;
  mrc_bool dump_result:1;
  mrc_bool no_exec:1;
  mrc_bool keep_lv:1;
  mrc_bool no_optimize:1;
  mrc_bool no_ext_ops:1;
#if defined(MRC_TARGET_MRUBY)
  const struct RProc *upper;
#endif

  // TODO
  //size_t parser_nerr;
  struct mrc_diagnostic_list *diagnostic_list;
  /* diagnostic.c alone keeps the four below; see mrc_diagnostic_list_append().
     The last entry of diagnostic_list, so an append does not walk the list. */
  struct mrc_diagnostic_list *diagnostic_tail;
  /* Where each line of the parsed source begins, built only once a source
     has had more lines looked up than a scan per lookup can afford. */
  uint32_t *diagnostic_line_starts;
  uint32_t diagnostic_line_count;
  uint16_t diagnostic_lookups;

  // For PICOIRB
  uint16_t scope_sp;

  /* Where in the joined source each of the files given to this context
     begins, so that a position can be told which file it came from. The
     codegen and the diagnostics both read it, whether or not stdio is in. */
  mrc_filename_table *filename_table;
  uint16_t filename_table_length;
  uint16_t current_filename_index;

#ifndef MRC_NO_STDIO
  mrc_pool *pool; // for codedump
#endif

  /* The arena everything Prism allocates for this context is taken from.
     Prism's allocator has no context argument, so the arena is made the
     current one for the duration of every call that has Prism allocate on
     this context's behalf; see mrc_ccontext_arena_save(). Unused where
     Prism allocates through libc; see prism_xallocator.h for what the
     arena is for. */
  void *prism_arena;

  /* How deep the brackets stand where the lexer is, so that a nesting Prism
     would recurse through is refused instead. See src/compile.c. */
  uint32_t nesting;
} mrc_ccontext;                 /* compiler context */

#ifdef MRC_TARGET_MRUBY
static inline int mrc_gc_arena_save(mrc_ccontext *c)
{
  if (!c->mrb) return 0;
  return mrb_gc_arena_save(c->mrb);
}
static inline void mrc_gc_arena_restore(mrc_ccontext *c, int ai)
{
  if (!c->mrb) return;
  mrb_gc_arena_restore(c->mrb, ai);
}
#else
# define mrc_gc_arena_save(c)        0;(void)ai
# define mrc_gc_arena_restore(c,ai)
#endif

mrc_ccontext *mrc_ccontext_new(mrb_state *mrb);
void mrc_ccontext_cleanup_local_variables(mrc_ccontext *c);
const char *mrc_ccontext_filename(mrc_ccontext *c, const char *s);
void mrc_ccontext_free(mrc_ccontext *c);

/* Make c's arena the one Prism allocates from, answering the one that was
   current so that the matching restore can put it back. Wrap every call
   that has Prism allocate for c (pm_options_*, a parse, freeing the parser)
   in the pair; the compiler's own entry points already do. Contexts may be
   created, used and freed in any order, so nothing is inferred from
   nesting. */
#if defined(MRC_TARGET_MRUBY) && defined(MRC_PRISM_ARENA)
void *mrc_ccontext_arena_save(mrc_ccontext *c);
void mrc_ccontext_arena_restore(mrc_ccontext *c, void *prev);
#else
static inline void *mrc_ccontext_arena_save(mrc_ccontext *c) { (void)c; return NULL; }
static inline void mrc_ccontext_arena_restore(mrc_ccontext *c, void *prev) { (void)c; (void)prev; }
#endif

MRC_END_DECL

#endif // MRC_CCONTEXT_H
