/*
** mirb_buffer.c - Multi-line buffer for mirb editor
**
** See Copyright Notice in mruby.h
*/

#include "mirb_buffer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

/*
 * Helper: Initialize a single line
 */
static mrb_bool
line_init(mirb_line *line)
{
  line->data = (char*)malloc(MIRB_BUF_LINE_INIT);
  if (line->data == NULL) return FALSE;
  line->data[0] = '\0';
  line->len = 0;
  line->cap = MIRB_BUF_LINE_INIT;
  return TRUE;
}

/*
 * Helper: Free a single line
 */
static void
line_free(mirb_line *line)
{
  free(line->data);
  line->data = NULL;
  line->len = 0;
  line->cap = 0;
}

/*
 * Helper: Ensure line has capacity for additional chars
 */
static mrb_bool
line_ensure_cap(mirb_line *line, size_t additional)
{
  size_t needed = line->len + additional + 1;  /* +1 for null */
  if (needed <= line->cap) return TRUE;

  size_t new_cap = line->cap * 2;
  while (new_cap < needed) new_cap *= 2;
  if (new_cap > MIRB_BUF_LINE_MAX) new_cap = MIRB_BUF_LINE_MAX;
  if (new_cap < needed) return FALSE;

  char *new_data = (char*)realloc(line->data, new_cap);
  if (new_data == NULL) return FALSE;

  line->data = new_data;
  line->cap = new_cap;
  return TRUE;
}

/*
 * Helper: Insert character at position in line
 */
static mrb_bool
line_insert_at(mirb_line *line, size_t pos, char c)
{
  if (pos > line->len) return FALSE;
  if (!line_ensure_cap(line, 1)) return FALSE;

  memmove(line->data + pos + 1, line->data + pos, line->len - pos + 1);
  line->data[pos] = c;
  line->len++;
  return TRUE;
}

/*
 * Helper: Delete character at position in line
 */
static mrb_bool
line_delete_at(mirb_line *line, size_t pos)
{
  if (pos >= line->len) return FALSE;

  memmove(line->data + pos, line->data + pos + 1, line->len - pos);
  line->len--;
  return TRUE;
}

/*
 * Helper: Set line content
 */
static mrb_bool
line_set(mirb_line *line, const char *str, size_t len)
{
  if (len + 1 > line->cap) {
    size_t new_cap = MIRB_BUF_LINE_INIT;
    while (new_cap < len + 1) new_cap *= 2;
    if (new_cap > MIRB_BUF_LINE_MAX) return FALSE;

    char *new_data = (char*)realloc(line->data, new_cap);
    if (new_data == NULL) return FALSE;
    line->data = new_data;
    line->cap = new_cap;
  }

  memcpy(line->data, str, len);
  line->data[len] = '\0';
  line->len = len;
  return TRUE;
}

/*
 * Initialize buffer
 */
mrb_bool
mirb_buffer_init(mirb_buffer *buf)
{
  memset(buf, 0, sizeof(*buf));

  buf->lines = (mirb_line*)malloc(sizeof(mirb_line) * MIRB_BUF_LINES_INIT);
  if (buf->lines == NULL) return FALSE;
  buf->line_cap = MIRB_BUF_LINES_INIT;

  /* Start with one empty line */
  if (!line_init(&buf->lines[0])) {
    free(buf->lines);
    return FALSE;
  }
  buf->line_count = 1;

  buf->kill_buf = (char*)malloc(MIRB_BUF_KILL_SIZE);
  if (buf->kill_buf == NULL) {
    line_free(&buf->lines[0]);
    free(buf->lines);
    return FALSE;
  }
  buf->kill_buf[0] = '\0';
  buf->kill_len = 0;

  return TRUE;
}

/*
 * Free buffer resources
 */
void
mirb_buffer_free(mirb_buffer *buf)
{
  if (buf->lines) {
    for (size_t i = 0; i < buf->line_count; i++) {
      line_free(&buf->lines[i]);
    }
    free(buf->lines);
    buf->lines = NULL;
  }
  free(buf->kill_buf);
  buf->kill_buf = NULL;
}

/*
 * Clear buffer content
 */
void
mirb_buffer_clear(mirb_buffer *buf)
{
  /* Free all lines except first */
  for (size_t i = 1; i < buf->line_count; i++) {
    line_free(&buf->lines[i]);
  }

  /* Clear first line */
  buf->lines[0].data[0] = '\0';
  buf->lines[0].len = 0;
  buf->line_count = 1;

  buf->cursor_line = 0;
  buf->cursor_col = 0;
  buf->modified = FALSE;
}

/*
 * Get total character count
 */
size_t
mirb_buffer_total_len(mirb_buffer *buf)
{
  size_t total = 0;
  for (size_t i = 0; i < buf->line_count; i++) {
    total += buf->lines[i].len;
    if (i < buf->line_count - 1) total++;  /* newline */
  }
  return total;
}

/*
 * Get buffer as string
 */
char *
mirb_buffer_to_string(mirb_buffer *buf)
{
  size_t total = mirb_buffer_total_len(buf);
  char *str = (char*)malloc(total + 1);
  if (str == NULL) return NULL;

  char *p = str;
  for (size_t i = 0; i < buf->line_count; i++) {
    memcpy(p, buf->lines[i].data, buf->lines[i].len);
    p += buf->lines[i].len;
    if (i < buf->line_count - 1) *p++ = '\n';
  }
  *p = '\0';

  return str;
}

/*
 * Set buffer from string
 */
mrb_bool
mirb_buffer_set_string(mirb_buffer *buf, const char *str)
{
  mirb_buffer_clear(buf);

  if (str == NULL || *str == '\0') return TRUE;

  const char *start = str;
  const char *p = str;
  size_t line_idx = 0;

  while (*p) {
    if (*p == '\n') {
      /* Set current line */
      if (line_idx >= buf->line_count) {
        /* Need to add new line */
        if (buf->line_count >= buf->line_cap) {
          size_t new_cap = buf->line_cap * 2;
          if (new_cap > MIRB_BUF_LINES_MAX) return FALSE;
          mirb_line *new_lines = (mirb_line*)realloc(buf->lines, sizeof(mirb_line) * new_cap);
          if (new_lines == NULL) return FALSE;
          buf->lines = new_lines;
          buf->line_cap = new_cap;
        }
        if (!line_init(&buf->lines[buf->line_count])) return FALSE;
        buf->line_count++;
      }
      if (!line_set(&buf->lines[line_idx], start, p - start)) return FALSE;

      start = p + 1;
      line_idx++;
      p++;
    }
    else {
      p++;
    }
  }

  /* Handle last line (may not end with newline) */
  if (start < p || line_idx == 0) {
    if (line_idx >= buf->line_count) {
      if (buf->line_count >= buf->line_cap) {
        size_t new_cap = buf->line_cap * 2;
        if (new_cap > MIRB_BUF_LINES_MAX) return FALSE;
        mirb_line *new_lines = (mirb_line*)realloc(buf->lines, sizeof(mirb_line) * new_cap);
        if (new_lines == NULL) return FALSE;
        buf->lines = new_lines;
        buf->line_cap = new_cap;
      }
      if (!line_init(&buf->lines[buf->line_count])) return FALSE;
      buf->line_count++;
    }
    if (!line_set(&buf->lines[line_idx], start, p - start)) return FALSE;
  }

  buf->cursor_line = 0;
  buf->cursor_col = 0;
  buf->modified = FALSE;

  return TRUE;
}

/*
 * Insert character at cursor
 */
mrb_bool
mirb_buffer_insert_char(mirb_buffer *buf, char c)
{
  mirb_line *line = &buf->lines[buf->cursor_line];

  if (!line_insert_at(line, buf->cursor_col, c)) return FALSE;

  buf->cursor_col++;
  buf->modified = TRUE;
  return TRUE;
}

/*
 * Insert string at cursor
 */
mrb_bool
mirb_buffer_insert_string(mirb_buffer *buf, const char *str, size_t len)
{
  for (size_t i = 0; i < len; i++) {
    if (str[i] == '\n') {
      if (!mirb_buffer_newline(buf)) return FALSE;
    }
    else {
      if (!mirb_buffer_insert_char(buf, str[i])) return FALSE;
    }
  }
  return TRUE;
}

/*
 * Delete character before cursor
 */
mrb_bool
mirb_buffer_delete_back(mirb_buffer *buf)
{
  if (buf->cursor_col > 0) {
    /* Delete within line */
    mirb_line *line = &buf->lines[buf->cursor_line];
    if (line_delete_at(line, buf->cursor_col - 1)) {
      buf->cursor_col--;
      buf->modified = TRUE;
      return TRUE;
    }
  }
  else if (buf->cursor_line > 0) {
    /* Join with previous line */
    mirb_line *prev = &buf->lines[buf->cursor_line - 1];
    mirb_line *curr = &buf->lines[buf->cursor_line];
    size_t prev_len = prev->len;

    /* Append current line to previous */
    if (!line_ensure_cap(prev, curr->len)) return FALSE;
    memcpy(prev->data + prev->len, curr->data, curr->len + 1);
    prev->len += curr->len;

    /* Remove current line */
    line_free(curr);
    memmove(&buf->lines[buf->cursor_line],
            &buf->lines[buf->cursor_line + 1],
            sizeof(mirb_line) * (buf->line_count - buf->cursor_line - 1));
    buf->line_count--;

    buf->cursor_line--;
    buf->cursor_col = prev_len;
    buf->modified = TRUE;
    return TRUE;
  }
  return FALSE;
}

/*
 * Delete character at cursor
 */
mrb_bool
mirb_buffer_delete_forward(mirb_buffer *buf)
{
  mirb_line *line = &buf->lines[buf->cursor_line];

  if (buf->cursor_col < line->len) {
    /* Delete within line */
    if (line_delete_at(line, buf->cursor_col)) {
      buf->modified = TRUE;
      return TRUE;
    }
  }
  else if (buf->cursor_line < buf->line_count - 1) {
    /* Join with next line */
    mirb_line *curr = &buf->lines[buf->cursor_line];
    mirb_line *next = &buf->lines[buf->cursor_line + 1];

    /* Append next line to current */
    if (!line_ensure_cap(curr, next->len)) return FALSE;
    memcpy(curr->data + curr->len, next->data, next->len + 1);
    curr->len += next->len;

    /* Remove next line */
    line_free(next);
    memmove(&buf->lines[buf->cursor_line + 1],
            &buf->lines[buf->cursor_line + 2],
            sizeof(mirb_line) * (buf->line_count - buf->cursor_line - 2));
    buf->line_count--;

    buf->modified = TRUE;
    return TRUE;
  }
  return FALSE;
}

/*
 * Insert newline (split line)
 */
mrb_bool
mirb_buffer_newline(mirb_buffer *buf)
{
  /* Ensure we have room for a new line */
  if (buf->line_count >= buf->line_cap) {
    size_t new_cap = buf->line_cap * 2;
    if (new_cap > MIRB_BUF_LINES_MAX) return FALSE;
    mirb_line *new_lines = (mirb_line*)realloc(buf->lines, sizeof(mirb_line) * new_cap);
    if (new_lines == NULL) return FALSE;
    buf->lines = new_lines;
    buf->line_cap = new_cap;
  }

  mirb_line *curr = &buf->lines[buf->cursor_line];
  size_t split_pos = buf->cursor_col;

  /* Make room for new line */
  memmove(&buf->lines[buf->cursor_line + 2],
          &buf->lines[buf->cursor_line + 1],
          sizeof(mirb_line) * (buf->line_count - buf->cursor_line - 1));

  /* Initialize new line with content after cursor */
  mirb_line *new_line = &buf->lines[buf->cursor_line + 1];
  if (!line_init(new_line)) {
    /* Restore lines array */
    memmove(&buf->lines[buf->cursor_line + 1],
            &buf->lines[buf->cursor_line + 2],
            sizeof(mirb_line) * (buf->line_count - buf->cursor_line - 1));
    return FALSE;
  }

  if (!line_set(new_line, curr->data + split_pos, curr->len - split_pos)) {
    line_free(new_line);
    memmove(&buf->lines[buf->cursor_line + 1],
            &buf->lines[buf->cursor_line + 2],
            sizeof(mirb_line) * (buf->line_count - buf->cursor_line - 1));
    return FALSE;
  }

  /* Truncate current line */
  curr->data[split_pos] = '\0';
  curr->len = split_pos;

  buf->line_count++;
  buf->cursor_line++;
  buf->cursor_col = 0;
  buf->modified = TRUE;

  return TRUE;
}

/* Delete a line at the given index */
void
mirb_buffer_delete_line(mirb_buffer *buf, size_t line_idx)
{
  if (line_idx >= buf->line_count) return;
  if (buf->line_count <= 1) return;  /* Keep at least one line */

  /* Free the line's data */
  line_free(&buf->lines[line_idx]);

  /* Shift remaining lines down */
  if (line_idx < buf->line_count - 1) {
    memmove(&buf->lines[line_idx],
            &buf->lines[line_idx + 1],
            sizeof(mirb_line) * (buf->line_count - line_idx - 1));
  }

  buf->line_count--;

  /* Adjust cursor if needed */
  if (buf->cursor_line >= buf->line_count) {
    buf->cursor_line = buf->line_count - 1;
  }
  if (buf->cursor_col > buf->lines[buf->cursor_line].len) {
    buf->cursor_col = buf->lines[buf->cursor_line].len;
  }

  buf->modified = TRUE;
}

/*
 * Move cursor left
 */
mrb_bool
mirb_buffer_cursor_left(mirb_buffer *buf)
{
  if (buf->cursor_col > 0) {
    buf->cursor_col--;
    return TRUE;
  }
  else if (buf->cursor_line > 0) {
    buf->cursor_line--;
    buf->cursor_col = buf->lines[buf->cursor_line].len;
    return TRUE;
  }
  return FALSE;
}

/*
 * Move cursor right
 */
mrb_bool
mirb_buffer_cursor_right(mirb_buffer *buf)
{
  mirb_line *line = &buf->lines[buf->cursor_line];

  if (buf->cursor_col < line->len) {
    buf->cursor_col++;
    return TRUE;
  }
  else if (buf->cursor_line < buf->line_count - 1) {
    buf->cursor_line++;
    buf->cursor_col = 0;
    return TRUE;
  }
  return FALSE;
}

/*
 * Move cursor up
 */
mrb_bool
mirb_buffer_cursor_up(mirb_buffer *buf)
{
  if (buf->cursor_line > 0) {
    buf->cursor_line--;
    /* Clamp column to line length */
    if (buf->cursor_col > buf->lines[buf->cursor_line].len) {
      buf->cursor_col = buf->lines[buf->cursor_line].len;
    }
    return TRUE;
  }
  return FALSE;
}

/*
 * Move cursor down
 */
mrb_bool
mirb_buffer_cursor_down(mirb_buffer *buf)
{
  if (buf->cursor_line < buf->line_count - 1) {
    buf->cursor_line++;
    /* Clamp column to line length */
    if (buf->cursor_col > buf->lines[buf->cursor_line].len) {
      buf->cursor_col = buf->lines[buf->cursor_line].len;
    }
    return TRUE;
  }
  return FALSE;
}

/*
 * Move to beginning of line
 */
void
mirb_buffer_cursor_home(mirb_buffer *buf)
{
  buf->cursor_col = 0;
}

/*
 * Move to end of line
 */
void
mirb_buffer_cursor_end(mirb_buffer *buf)
{
  buf->cursor_col = buf->lines[buf->cursor_line].len;
}

/*
 * Move to start of buffer
 */
void
mirb_buffer_cursor_start(mirb_buffer *buf)
{
  buf->cursor_line = 0;
  buf->cursor_col = 0;
}

/*
 * Move to end of buffer
 */
void
mirb_buffer_cursor_finish(mirb_buffer *buf)
{
  buf->cursor_line = buf->line_count - 1;
  buf->cursor_col = buf->lines[buf->cursor_line].len;
}

/*
 * Helper: Check if character is word character
 */
static mrb_bool
is_word_char(char c)
{
  return isalnum((unsigned char)c) || c == '_';
}

/*
 * Move cursor back one word
 */
mrb_bool
mirb_buffer_cursor_word_back(mirb_buffer *buf)
{
  mrb_bool moved = FALSE;

  /* Skip any whitespace/non-word chars going back */
  while (buf->cursor_col > 0 || buf->cursor_line > 0) {
    if (buf->cursor_col == 0) {
      if (buf->cursor_line == 0) break;
      buf->cursor_line--;
      buf->cursor_col = buf->lines[buf->cursor_line].len;
      moved = TRUE;
      continue;
    }

    char c = buf->lines[buf->cursor_line].data[buf->cursor_col - 1];
    if (is_word_char(c)) break;
    buf->cursor_col--;
    moved = TRUE;
  }

  /* Move through word chars */
  while (buf->cursor_col > 0) {
    char c = buf->lines[buf->cursor_line].data[buf->cursor_col - 1];
    if (!is_word_char(c)) break;
    buf->cursor_col--;
    moved = TRUE;
  }

  return moved;
}

/*
 * Move cursor forward one word
 */
mrb_bool
mirb_buffer_cursor_word_forward(mirb_buffer *buf)
{
  mrb_bool moved = FALSE;
  mirb_line *line = &buf->lines[buf->cursor_line];

  /* Move through current word chars */
  while (buf->cursor_col < line->len) {
    if (!is_word_char(line->data[buf->cursor_col])) break;
    buf->cursor_col++;
    moved = TRUE;
  }

  /* Skip whitespace/non-word chars */
  while (buf->cursor_col < line->len || buf->cursor_line < buf->line_count - 1) {
    if (buf->cursor_col >= line->len) {
      if (buf->cursor_line >= buf->line_count - 1) break;
      buf->cursor_line++;
      buf->cursor_col = 0;
      line = &buf->lines[buf->cursor_line];
      moved = TRUE;
      continue;
    }

    if (is_word_char(line->data[buf->cursor_col])) break;
    buf->cursor_col++;
    moved = TRUE;
  }

  return moved;
}

/*
 * Helper: Save text to kill buffer
 */
static void
save_to_kill(mirb_buffer *buf, const char *str, size_t len)
{
  if (len >= MIRB_BUF_KILL_SIZE) len = MIRB_BUF_KILL_SIZE - 1;
  memcpy(buf->kill_buf, str, len);
  buf->kill_buf[len] = '\0';
  buf->kill_len = len;
}

/*
 * Kill to end of line
 */
void
mirb_buffer_kill_to_end(mirb_buffer *buf)
{
  mirb_line *line = &buf->lines[buf->cursor_line];

  if (buf->cursor_col < line->len) {
    /* Kill text to end of line */
    save_to_kill(buf, line->data + buf->cursor_col, line->len - buf->cursor_col);
    line->data[buf->cursor_col] = '\0';
    line->len = buf->cursor_col;
    buf->modified = TRUE;
  }
  else if (line->len == 0 && buf->line_count > 1) {
    /* Empty line: delete the entire line */
    save_to_kill(buf, "\n", 1);
    mirb_buffer_delete_line(buf, buf->cursor_line);
    /* Adjust cursor to end of previous line if we deleted from middle */
    if (buf->cursor_line > 0 && buf->cursor_line >= buf->line_count) {
      buf->cursor_line = buf->line_count - 1;
    }
    buf->cursor_col = 0;
  }
  else if (buf->cursor_line < buf->line_count - 1) {
    /* At end of non-empty line: kill newline (join with next line) */
    save_to_kill(buf, "\n", 1);
    mirb_buffer_delete_forward(buf);
  }
}

/*
 * Kill to start of line
 */
void
mirb_buffer_kill_to_start(mirb_buffer *buf)
{
  mirb_line *line = &buf->lines[buf->cursor_line];

  if (buf->cursor_col > 0) {
    save_to_kill(buf, line->data, buf->cursor_col);
    memmove(line->data, line->data + buf->cursor_col, line->len - buf->cursor_col + 1);
    line->len -= buf->cursor_col;
    buf->cursor_col = 0;
    buf->modified = TRUE;
  }
}

/*
 * Kill word backward
 */
void
mirb_buffer_kill_word_back(mirb_buffer *buf)
{
  size_t start_line = buf->cursor_line;
  size_t start_col = buf->cursor_col;

  if (!mirb_buffer_cursor_word_back(buf)) return;

  if (buf->cursor_line == start_line) {
    /* Same line */
    mirb_line *line = &buf->lines[buf->cursor_line];
    size_t kill_len = start_col - buf->cursor_col;
    save_to_kill(buf, line->data + buf->cursor_col, kill_len);
    memmove(line->data + buf->cursor_col,
            line->data + start_col,
            line->len - start_col + 1);
    line->len -= kill_len;
    buf->modified = TRUE;
  }
  /* Cross-line kill is more complex; simplified: just delete chars */
}

/*
 * Kill word forward
 */
void
mirb_buffer_kill_word_forward(mirb_buffer *buf)
{
  size_t start_col = buf->cursor_col;
  mirb_line *line = &buf->lines[buf->cursor_line];

  /* Find end of word */
  size_t end_col = start_col;

  /* Skip word chars */
  while (end_col < line->len && is_word_char(line->data[end_col])) {
    end_col++;
  }

  /* Skip non-word chars */
  while (end_col < line->len && !is_word_char(line->data[end_col])) {
    end_col++;
  }

  if (end_col > start_col) {
    save_to_kill(buf, line->data + start_col, end_col - start_col);
    memmove(line->data + start_col,
            line->data + end_col,
            line->len - end_col + 1);
    line->len -= (end_col - start_col);
    buf->modified = TRUE;
  }
}

/*
 * Yank (paste) from kill buffer
 */
mrb_bool
mirb_buffer_yank(mirb_buffer *buf)
{
  if (buf->kill_len == 0) return FALSE;
  return mirb_buffer_insert_string(buf, buf->kill_buf, buf->kill_len);
}

/*
 * Get current line content
 */
const char *
mirb_buffer_current_line(mirb_buffer *buf)
{
  return buf->lines[buf->cursor_line].data;
}

/*
 * Get line at index
 */
const char *
mirb_buffer_line_at(mirb_buffer *buf, size_t index)
{
  if (index >= buf->line_count) return NULL;
  return buf->lines[index].data;
}

/*
 * Get line length at index
 */
size_t
mirb_buffer_line_len(mirb_buffer *buf, size_t index)
{
  if (index >= buf->line_count) return 0;
  return buf->lines[index].len;
}
