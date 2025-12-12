/*
** mirb_editor.h - Multi-line editor for mirb
**
** See Copyright Notice in mruby.h
*/

#ifndef MIRB_EDITOR_H
#define MIRB_EDITOR_H

#include <mruby.h>
#include "mirb_term.h"
#include "mirb_buffer.h"
#include "mirb_history.h"

/*
 * Editor result codes
 */
typedef enum mirb_edit_result {
  MIRB_EDIT_OK = 0,       /* Input ready (Enter pressed) */
  MIRB_EDIT_CONTINUE,     /* Need more input (multi-line) */
  MIRB_EDIT_EOF,          /* End of file (Ctrl+D on empty) */
  MIRB_EDIT_INTERRUPT,    /* Interrupted (Ctrl+C) */
  MIRB_EDIT_ERROR         /* Error occurred */
} mirb_edit_result;

/*
 * Callback to check if input is complete
 * Returns TRUE if the code is syntactically complete
 */
typedef mrb_bool mirb_check_complete_fn(const char *code, void *user_data);

/*
 * Editor state
 */
typedef struct mirb_editor {
  mirb_term term;             /* terminal state */
  mirb_buffer buf;            /* editing buffer */
  mirb_history hist;          /* command history */

  const char *prompt;         /* primary prompt (e.g., "> ") */
  const char *prompt_cont;    /* continuation prompt (e.g., "* ") */
  size_t prompt_len;          /* length of primary prompt */
  size_t prompt_cont_len;     /* length of continuation prompt */

  mirb_check_complete_fn *check_complete;  /* completion checker */
  void *check_complete_data;              /* user data for checker */

  size_t display_cursor_row;  /* cursor row in buffer (for refresh tracking) */
  size_t prev_line_count;     /* line count from last refresh */

  mrb_bool initialized;       /* editor is initialized */
  mrb_bool use_color;         /* use colored output */
} mirb_editor;

/*
 * Initialize editor
 * Returns TRUE on success
 */
mrb_bool mirb_editor_init(mirb_editor *ed);

/*
 * Cleanup editor
 */
void mirb_editor_cleanup(mirb_editor *ed);

/*
 * Set prompts
 */
void mirb_editor_set_prompts(mirb_editor *ed,
                              const char *prompt,
                              const char *prompt_cont);

/*
 * Set completion checker callback
 */
void mirb_editor_set_check_complete(mirb_editor *ed,
                                     mirb_check_complete_fn *fn,
                                     void *user_data);

/*
 * Enable or disable colored output
 */
void mirb_editor_set_color(mirb_editor *ed, mrb_bool enable);

/*
 * Check if multi-line editing is supported
 */
mrb_bool mirb_editor_supported(mirb_editor *ed);

/*
 * Read input with multi-line editing
 *
 * Returns result code (OK, EOF, INTERRUPT, ERROR)
 * On success (OK), caller must free the returned string
 */
mirb_edit_result mirb_editor_read(mirb_editor *ed, char **out_str);

/*
 * Simple single-line input (fallback when raw mode not supported)
 * Used internally but can be called directly
 */
mirb_edit_result mirb_editor_read_simple(mirb_editor *ed, char **out_str);

/*
 * Add entry to history
 * Called after successful command execution
 */
void mirb_editor_history_add(mirb_editor *ed, const char *entry);

#endif /* MIRB_EDITOR_H */
