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
  mrc_bool capture_errors:1;
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

  // For PICOIRB
  uint16_t scope_sp;

#ifndef MRC_NO_STDIO
  mrc_pool *pool; // for codedump

  mrc_filename_table *filename_table;
  uint16_t filename_table_length;
  uint16_t current_filename_index;
#endif
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

MRC_END_DECL

#endif // MRC_CCONTEXT_H
