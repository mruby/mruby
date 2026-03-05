/*
** mirb_completion.h - Tab completion support for mirb
**
** See Copyright Notice in mruby.h
*/

#ifndef MIRB_COMPLETION_H
#define MIRB_COMPLETION_H

#include <mruby.h>
#include <mruby/compile.h>

/**
 * @file mirb_completion.h
 *
 * Tab completion support for mirb.
 *
 * Architecture:
 * - Core engine is library-agnostic
 * - Adapters for readline/libedit and linenoise
 * - Context detection based on input line analysis
 * - Safe evaluation of receiver expressions
 *
 * Completion Types:
 * - COMPLETION_METHOD: After dot operator
 * - COMPLETION_KEYWORD: Ruby keywords
 * - COMPLETION_LOCAL_VAR: Variables in scope
 * - COMPLETION_GLOBAL_VAR: $variables
 * - COMPLETION_CONSTANT: Constants and classes
 * - COMPLETION_FILE: File paths (optional)
 *
 * Performance:
 * - Completions generated on-demand
 * - Results cached per tab press
 * - Safe evaluation with exception handling
 */

/* Completion types */
typedef enum {
  COMPLETION_METHOD,        /* Object methods */
  COMPLETION_KEYWORD,       /* Ruby keywords */
  COMPLETION_GLOBAL_VAR,    /* $global */
  COMPLETION_LOCAL_VAR,     /* local_var */
  COMPLETION_CONSTANT,      /* CONSTANT or Class */
  COMPLETION_FILE,          /* File paths */
} mirb_completion_type;

/* Completion context - shared state */
typedef struct mirb_completion_ctx {
  mrb_state *mrb;           /* mruby VM state */
  mrb_ccontext *cxt;        /* Compiler context for locals */
  const char *line_buf;     /* Current input line */
  int cursor_pos;           /* Cursor position in line */
  char *match_prefix;       /* Text to match against */
  int prefix_len;           /* Length of prefix */

  /* Completion results */
  char **completions;       /* Array of completion strings */
  int completion_count;     /* Number of completions */
  int completion_alloc;     /* Allocated size */
  int current_index;        /* For generator pattern (readline) */
} mirb_completion_ctx;

/* Core Completion Engine Interface */

/* Initialize completion context */
void mirb_completion_init(mirb_completion_ctx *ctx, mrb_state *mrb,
                          mrb_ccontext *cxt);

/* Free completion context */
void mirb_completion_free(mirb_completion_ctx *ctx);

/* Analyze line and generate completions */
void mirb_generate_completions(mirb_completion_ctx *ctx,
                               const char *line, int cursor_pos);

/* Get completion type from context */
mirb_completion_type mirb_detect_completion_type(const char *line,
                                                  int cursor_pos);

/* Individual completion generators */
void mirb_complete_methods(mirb_completion_ctx *ctx, mrb_value receiver);
void mirb_complete_keywords(mirb_completion_ctx *ctx);
void mirb_complete_local_vars(mirb_completion_ctx *ctx);
void mirb_complete_global_vars(mirb_completion_ctx *ctx);
void mirb_complete_constants(mirb_completion_ctx *ctx, struct RClass *scope);
void mirb_complete_files(mirb_completion_ctx *ctx, const char *partial_path);

/* Helper functions */

/* Add completion if matches prefix */
void mirb_add_completion(mirb_completion_ctx *ctx, const char *text);

/* Extract receiver expression from line */
char *mirb_extract_receiver(const char *line, int cursor_pos, int *recv_end);

/* Evaluate receiver expression to get object */
mrb_value mirb_eval_receiver(mrb_state *mrb, const char *receiver_expr, mrb_ccontext *cxt);

/* Check if in file completion context */
mrb_bool mirb_in_file_context(const char *line, int quote_pos);

/* Cleanup completion context (shared by all adapters) */
void mirb_cleanup_completion(void);

/* Readline/Libedit adapter setup */
#ifdef MRB_USE_READLINE
#ifndef MRB_USE_LINENOISE
void mirb_setup_readline_completion(mrb_state *mrb, mrb_ccontext *cxt);
#endif
#endif

/* Linenoise adapter setup */
#ifdef MRB_USE_LINENOISE
void mirb_setup_linenoise_completion(mrb_state *mrb, mrb_ccontext *cxt);
#endif

/* Custom editor adapter */
void mirb_setup_editor_completion(mrb_state *mrb, mrb_ccontext *cxt);

/* Get completions for custom editor - returns number of completions */
int mirb_get_completions(const char *line, int cursor_pos,
                         char ***completions_out, int *prefix_len_out);

/* Free completions returned by mirb_get_completions */
void mirb_free_completions(char **completions, int count);

#endif  /* MIRB_COMPLETION_H */
