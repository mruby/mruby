/*
** mirb_highlight.h - Syntax highlighting for mirb
**
** See Copyright Notice in mruby.h
*/

#ifndef MIRB_HIGHLIGHT_H
#define MIRB_HIGHLIGHT_H

#include <mruby.h>
#include <stdio.h>

/*
 * Token types for syntax highlighting
 */
typedef enum mirb_token_type {
  MIRB_TOK_DEFAULT,      /* default text */
  MIRB_TOK_KEYWORD,      /* if, else, def, class, end, etc. */
  MIRB_TOK_STRING,       /* "..." or '...' */
  MIRB_TOK_COMMENT,      /* # to end of line */
  MIRB_TOK_NUMBER,       /* integers, floats */
  MIRB_TOK_SYMBOL,       /* :symbol */
  MIRB_TOK_CONSTANT,     /* Uppercase identifiers */
  MIRB_TOK_IVAR,         /* @instance_var */
  MIRB_TOK_GVAR,         /* $global_var */
  MIRB_TOK_REGEXP,       /* /regexp/ */
  MIRB_TOK_MAX
} mirb_token_type;

/*
 * Color theme
 */
typedef enum mirb_theme {
  MIRB_THEME_DARK,       /* light text on dark background (default) */
  MIRB_THEME_LIGHT       /* dark text on light background */
} mirb_theme;

/*
 * Highlighter state
 */
typedef struct mirb_highlighter {
  mirb_theme theme;
  mrb_bool enabled;
  /* Multi-line state tracking */
  mrb_bool in_string;
  char string_quote;     /* '"' or '\'' */
  mrb_bool in_heredoc;
  mrb_bool in_regexp;
} mirb_highlighter;

/*
 * Initialize highlighter with auto-detected or specified theme
 */
void mirb_highlight_init(mirb_highlighter *hl, mrb_bool enabled);

/*
 * Set theme explicitly
 */
void mirb_highlight_set_theme(mirb_highlighter *hl, mirb_theme theme);

/*
 * Detect theme from environment variables
 * Returns MIRB_THEME_DARK if cannot detect
 */
mirb_theme mirb_highlight_detect_theme(void);

/*
 * Print a line with syntax highlighting
 * Handles multi-line strings/comments by tracking state
 */
void mirb_highlight_print_line(mirb_highlighter *hl, const char *line);

/*
 * Reset multi-line state (call when starting new input)
 */
void mirb_highlight_reset(mirb_highlighter *hl);

#endif /* MIRB_HIGHLIGHT_H */
