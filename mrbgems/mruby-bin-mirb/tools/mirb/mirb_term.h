/*
** mirb_term.h - Terminal control for mirb multi-line editor
**
** See Copyright Notice in mruby.h
*/

#ifndef MIRB_TERM_H
#define MIRB_TERM_H

#include <mruby.h>

/*
 * Key codes for mirb editor
 * Values > 255 are used for special keys to avoid collision with ASCII
 */
enum mirb_key {
  MIRB_KEY_NONE = 0,

  /* Control characters (ASCII values) */
  MIRB_KEY_CTRL_A = 1,
  MIRB_KEY_CTRL_B = 2,
  MIRB_KEY_CTRL_C = 3,
  MIRB_KEY_CTRL_D = 4,
  MIRB_KEY_CTRL_E = 5,
  MIRB_KEY_CTRL_F = 6,
  MIRB_KEY_TAB = 9,
  MIRB_KEY_CTRL_K = 11,
  MIRB_KEY_CTRL_L = 12,
  MIRB_KEY_ENTER = 13,
  MIRB_KEY_CTRL_N = 14,
  MIRB_KEY_CTRL_P = 16,
  MIRB_KEY_CTRL_U = 21,
  MIRB_KEY_CTRL_W = 23,
  MIRB_KEY_CTRL_Y = 25,
  MIRB_KEY_ESC = 27,
  MIRB_KEY_BACKSPACE = 127,

  /* Special keys (escape sequences mapped to values > 255) */
  MIRB_KEY_UP = 256,
  MIRB_KEY_DOWN = 257,
  MIRB_KEY_RIGHT = 258,
  MIRB_KEY_LEFT = 259,
  MIRB_KEY_HOME = 260,
  MIRB_KEY_END = 261,
  MIRB_KEY_DELETE = 262,

  /* Alt/Meta key combinations */
  MIRB_KEY_ALT_B = 300,
  MIRB_KEY_ALT_F = 301,
  MIRB_KEY_ALT_D = 302
};

/*
 * Terminal state structure
 */
typedef struct mirb_term {
  mrb_bool raw_mode;      /* TRUE if terminal is in raw mode */
  mrb_bool supported;     /* TRUE if raw mode is supported */
  int cols;               /* terminal width in columns */
  int rows;               /* terminal height in rows */
#if !defined(_WIN32) && !defined(_WIN64)
  void *orig_termios;     /* original terminal settings (struct termios*) */
#endif
} mirb_term;

/*
 * Initialize terminal state
 * Returns TRUE if terminal operations are supported
 */
mrb_bool mirb_term_init(mirb_term *term);

/*
 * Cleanup terminal state and restore original settings
 */
void mirb_term_cleanup(mirb_term *term);

/*
 * Enable raw mode for character-by-character input
 * Returns TRUE on success
 */
mrb_bool mirb_term_raw_enable(mirb_term *term);

/*
 * Disable raw mode and restore normal terminal operation
 */
void mirb_term_raw_disable(mirb_term *term);

/*
 * Read a single key (handles escape sequences)
 * Returns key code from mirb_key enum or ASCII value
 */
int mirb_term_read_key(mirb_term *term);

/*
 * Cursor movement functions (ANSI escape sequences)
 */
void mirb_term_cursor_up(int n);
void mirb_term_cursor_down(int n);
void mirb_term_cursor_right(int n);
void mirb_term_cursor_left(int n);
void mirb_term_cursor_col(int col);  /* move to column (1-based) */

/*
 * Line and screen control
 */
void mirb_term_clear_line(void);     /* clear entire current line */
void mirb_term_clear_to_end(void);   /* clear from cursor to end of line */
void mirb_term_clear_screen(void);   /* clear entire screen */
void mirb_term_clear_below(void);    /* clear from cursor to end of screen */

/*
 * Update terminal size information
 */
void mirb_term_get_size(mirb_term *term);

/*
 * Flush output buffer
 */
void mirb_term_flush(void);

#endif /* MIRB_TERM_H */
