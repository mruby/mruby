/*
** mirb_buffer.h - Multi-line buffer for mirb editor
**
** See Copyright Notice in mruby.h
*/

#ifndef MIRB_BUFFER_H
#define MIRB_BUFFER_H

#include <mruby.h>

/*
 * Default sizes for buffer allocation
 */
#define MIRB_BUF_LINE_INIT    128    /* initial line buffer size */
#define MIRB_BUF_LINE_MAX     4096   /* maximum line length */
#define MIRB_BUF_LINES_INIT   8      /* initial number of lines */
#define MIRB_BUF_LINES_MAX    1024   /* maximum number of lines */
#define MIRB_BUF_KILL_SIZE    4096   /* kill buffer size */

/*
 * A single line in the buffer
 */
typedef struct mirb_line {
  char *data;           /* line content (null-terminated) */
  size_t len;           /* current length (excluding null) */
  size_t cap;           /* allocated capacity */
} mirb_line;

/*
 * Multi-line buffer with cursor tracking
 */
typedef struct mirb_buffer {
  mirb_line *lines;     /* array of lines */
  size_t line_count;    /* number of lines */
  size_t line_cap;      /* allocated line slots */

  size_t cursor_line;   /* current line (0-indexed) */
  size_t cursor_col;    /* current column (0-indexed) */

  char *kill_buf;       /* kill buffer for cut/paste */
  size_t kill_len;      /* length of kill buffer content */

  mrb_bool modified;    /* buffer has been modified */
} mirb_buffer;

/*
 * Initialize buffer
 * Returns TRUE on success
 */
mrb_bool mirb_buffer_init(mirb_buffer *buf);

/*
 * Free buffer resources
 */
void mirb_buffer_free(mirb_buffer *buf);

/*
 * Clear buffer content (reset to single empty line)
 */
void mirb_buffer_clear(mirb_buffer *buf);

/*
 * Get total character count across all lines
 */
size_t mirb_buffer_total_len(mirb_buffer *buf);

/*
 * Get buffer content as a single string
 * Lines are joined with newlines
 * Caller must free the returned string
 */
char *mirb_buffer_to_string(mirb_buffer *buf);

/*
 * Get buffer content up to and including a specific line
 * Caller must free the returned string
 */
char *mirb_buffer_to_string_upto_line(mirb_buffer *buf, size_t up_to_line);

/*
 * Set buffer content from string
 * String may contain newlines
 */
mrb_bool mirb_buffer_set_string(mirb_buffer *buf, const char *str);

/*
 * Insert a character at cursor position
 */
mrb_bool mirb_buffer_insert_char(mirb_buffer *buf, char c);

/*
 * Insert a string at cursor position
 */
mrb_bool mirb_buffer_insert_string(mirb_buffer *buf, const char *str, size_t len);

/*
 * Delete character before cursor (backspace)
 * Returns TRUE if a character was deleted
 */
mrb_bool mirb_buffer_delete_back(mirb_buffer *buf);

/*
 * Delete character at cursor (delete key)
 * Returns TRUE if a character was deleted
 */
mrb_bool mirb_buffer_delete_forward(mirb_buffer *buf);

/*
 * Insert newline at cursor position (split current line)
 */
mrb_bool mirb_buffer_newline(mirb_buffer *buf);

/*
 * Delete a line at the given index
 */
void mirb_buffer_delete_line(mirb_buffer *buf, size_t line_idx);

/*
 * Cursor movement functions
 * Return TRUE if cursor moved
 */
mrb_bool mirb_buffer_cursor_left(mirb_buffer *buf);
mrb_bool mirb_buffer_cursor_right(mirb_buffer *buf);
mrb_bool mirb_buffer_cursor_up(mirb_buffer *buf);
mrb_bool mirb_buffer_cursor_down(mirb_buffer *buf);

/*
 * Move cursor to beginning/end of current line
 */
void mirb_buffer_cursor_home(mirb_buffer *buf);
void mirb_buffer_cursor_end(mirb_buffer *buf);

/*
 * Move cursor to beginning/end of buffer
 */
void mirb_buffer_cursor_start(mirb_buffer *buf);
void mirb_buffer_cursor_finish(mirb_buffer *buf);

/*
 * Word movement (like Emacs Alt+B, Alt+F)
 */
mrb_bool mirb_buffer_cursor_word_back(mirb_buffer *buf);
mrb_bool mirb_buffer_cursor_word_forward(mirb_buffer *buf);

/*
 * Kill operations (cut to kill buffer)
 * Ctrl+K: kill to end of line
 * Ctrl+U: kill to beginning of line
 * Ctrl+W: kill word backward
 * Alt+D: kill word forward
 */
void mirb_buffer_kill_to_end(mirb_buffer *buf);
void mirb_buffer_kill_to_start(mirb_buffer *buf);
void mirb_buffer_kill_word_back(mirb_buffer *buf);
void mirb_buffer_kill_word_forward(mirb_buffer *buf);

/*
 * Yank (paste from kill buffer)
 * Ctrl+Y
 */
mrb_bool mirb_buffer_yank(mirb_buffer *buf);

/*
 * Get current line content
 */
const char *mirb_buffer_current_line(mirb_buffer *buf);

/*
 * Get line at index
 */
const char *mirb_buffer_line_at(mirb_buffer *buf, size_t index);

/*
 * Get length of line at index
 */
size_t mirb_buffer_line_len(mirb_buffer *buf, size_t index);

/*
 * Get cursor display column (visual column for terminal positioning)
 * Handles UTF-8 display width when MRB_UTF8_STRING is defined
 */
size_t mirb_buffer_cursor_display_col(mirb_buffer *buf);

/*
 * Check if character is a word character (alphanumeric or underscore)
 */
static inline mrb_bool
mirb_is_word_char(char c)
{
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
         (c >= '0' && c <= '9') || c == '_';
}

#endif /* MIRB_BUFFER_H */
