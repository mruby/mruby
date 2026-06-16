/*
** mirb_buffer.c - Multi-line buffer for mirb editor
**
** See Copyright Notice in mruby.h
*/

#include "mirb_buffer.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef MRB_UTF8_STRING
/*
 * UTF-8 helper functions
 * These are only compiled when MRB_UTF8_STRING is defined
 */

/* Check if byte is a UTF-8 lead byte (not a continuation byte) */
static mrb_bool
utf8_islead(unsigned char c)
{
  return (c & 0xC0) != 0x80;
}

/* UTF-8 character length table indexed by (first_byte >> 3) */
static const char utf8_len_table[] = {
  1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,  /* 0x00-0x7F: ASCII */
  0, 0, 0, 0, 0, 0, 0, 0,                          /* 0x80-0xBF: continuation (invalid start) */
  2, 2, 2, 2,                                      /* 0xC0-0xDF: 2-byte sequences */
  3, 3,                                            /* 0xE0-0xEF: 3-byte sequences */
  4,                                               /* 0xF0-0xF7: 4-byte sequences */
  0                                                /* 0xF8-0xFF: invalid */
};

/*
 * Get byte length of UTF-8 character at position
 * Returns 1 for invalid sequences (safe fallback)
 */
static size_t
utf8_char_len(const char *p, const char *end)
{
  size_t len;
  if (p >= end) return 0;

  len = (size_t)utf8_len_table[(unsigned char)p[0] >> 3];
  if (len == 0 || len > (size_t)(end - p)) return 1;

  /* Validate continuation bytes */
  switch (len) {
  case 4:
    if (!utf8_islead((unsigned char)p[3])) break;  /* continuation expected */
    return 1;
  case 3:
    if (!utf8_islead((unsigned char)p[2])) break;
    return 1;
  case 2:
    if (!utf8_islead((unsigned char)p[1])) break;
    return 1;
  }
  return len;
}

/*
 * Find start of previous UTF-8 character
 * Returns byte offset from start of string to the previous character
 * If at position 0, returns 0
 */
static size_t
utf8_prev_char_start(const char *str, size_t pos)
{
  size_t i;
  if (pos == 0) return 0;

  /* Scan back to find a lead byte (max 4 bytes back) */
  for (i = 1; i <= 4 && i <= pos; i++) {
    if (utf8_islead((unsigned char)str[pos - i])) {
      return pos - i;
    }
  }
  /* No lead byte found, assume single byte */
  return pos - 1;
}

/*
 * Calculate display width for a UTF-8 character
 * Returns 2 for CJK/wide characters, 1 for others
 *
 * This is a simplified version - proper implementation would use wcwidth()
 * We detect East Asian Wide characters by their code point ranges:
 * - CJK Unified Ideographs: U+4E00-U+9FFF (3-byte UTF-8: E4-E9)
 * - Hiragana/Katakana: U+3040-U+30FF (3-byte UTF-8: E3 81-83)
 * - Full-width forms: U+FF00-U+FFEF (3-byte UTF-8: EF BC-BF)
 */
static int
utf8_char_width(const char *p, const char *end)
{
  unsigned char c = (unsigned char)p[0];

  if (c < 0x80) return 1;  /* ASCII */
  if (c < 0xE0) return 1;  /* 2-byte (Latin extended, etc.) */

  /* 3-byte sequences - check for wide characters */
  if (c >= 0xE3 && c <= 0xE9 && (end - p) >= 3) {
    /* CJK and Japanese ranges are typically double-width */
    return 2;
  }
  if (c == 0xEF && (end - p) >= 3) {
    unsigned char c2 = (unsigned char)p[1];
    if (c2 >= 0xBC && c2 <= 0xBF) {
      /* Full-width ASCII and symbols */
      return 2;
    }
  }

  /* 4-byte sequences (emoji, etc.) - typically double-width */
  if (c >= 0xF0) return 2;

  return 1;
}

/*
 * Calculate display column from byte position
 * Sums up the display width of all characters before the byte position
 */
static size_t
utf8_display_col(const char *str, size_t byte_pos)
{
  size_t col = 0;
  const char *p = str;
  const char *end = str + byte_pos;

  while (p < end) {
    size_t char_len = utf8_char_len(p, str + byte_pos + 4);  /* +4 for safety */
    if (char_len == 0) break;
    col += (size_t)utf8_char_width(p, end);
    p += char_len;
  }
  return col;
}
#endif /* MRB_UTF8_STRING */

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
#ifndef MRB_UTF8_STRING
static mrb_bool
line_delete_at(mirb_line *line, size_t pos)
{
  if (pos >= line->len) return FALSE;

  memmove(line->data + pos, line->data + pos + 1, line->len - pos);
  line->len--;
  return TRUE;
}
#endif

#ifdef MRB_UTF8_STRING
/*
 * Helper: Delete N bytes at position in line (for UTF-8 multibyte chars)
 */
static mrb_bool
line_delete_bytes_at(mirb_line *line, size_t pos, size_t count)
{
  if (pos >= line->len || count == 0) return FALSE;
  if (pos + count > line->len) count = line->len - pos;

  memmove(line->data + pos, line->data + pos + count, line->len - pos - count + 1);
  line->len -= count;
  return TRUE;
}
#endif

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
 * Helper: Ensure buffer has capacity for one more line
 */
static mrb_bool
buffer_ensure_line_cap(mirb_buffer *buf)
{
  if (buf->line_count < buf->line_cap) return TRUE;
  size_t new_cap = buf->line_cap * 2;
  if (new_cap > MIRB_BUF_LINES_MAX) return FALSE;
  mirb_line *new_lines = (mirb_line*)realloc(buf->lines, sizeof(mirb_line) * new_cap);
  if (new_lines == NULL) return FALSE;
  buf->lines = new_lines;
  buf->line_cap = new_cap;
  return TRUE;
}

/*
 * Helper: Join line at line_idx with the previous line (line_idx-1).
 * Appends content of line_idx to line_idx-1, then removes line_idx.
 */
static mrb_bool
buffer_join_line_up(mirb_buffer *buf, size_t line_idx)
{
  mirb_line *prev = &buf->lines[line_idx - 1];
  mirb_line *curr = &buf->lines[line_idx];

  if (!line_ensure_cap(prev, curr->len)) return FALSE;
  memcpy(prev->data + prev->len, curr->data, curr->len + 1);
  prev->len += curr->len;

  line_free(curr);
  memmove(&buf->lines[line_idx],
          &buf->lines[line_idx + 1],
          sizeof(mirb_line) * (buf->line_count - line_idx - 1));
  buf->line_count--;
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
 * Get buffer content up to and including a specific line as string
 * Caller must free the returned string
 */
char *
mirb_buffer_to_string_upto_line(mirb_buffer *buf, size_t up_to_line)
{
  size_t total = 0;
  size_t lines_to_include = (up_to_line < buf->line_count) ? up_to_line + 1 : buf->line_count;

  for (size_t i = 0; i < lines_to_include; i++) {
    total += buf->lines[i].len;
    if (i < lines_to_include - 1) total++;  /* newline */
  }

  char *str = (char*)malloc(total + 1);
  if (str == NULL) return NULL;

  char *p = str;
  for (size_t i = 0; i < lines_to_include; i++) {
    memcpy(p, buf->lines[i].data, buf->lines[i].len);
    p += buf->lines[i].len;
    if (i < lines_to_include - 1) *p++ = '\n';
  }
  *p = '\0';

  return str;
}

/*
 * Get buffer as string
 */
char *
mirb_buffer_to_string(mirb_buffer *buf)
{
  return mirb_buffer_to_string_upto_line(buf, buf->line_count - 1);
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
        if (!buffer_ensure_line_cap(buf)) return FALSE;
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
      if (!buffer_ensure_line_cap(buf)) return FALSE;
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
#ifdef MRB_UTF8_STRING
    /* Find start of previous UTF-8 character and delete entire character */
    size_t prev_pos = utf8_prev_char_start(line->data, buf->cursor_col);
    size_t char_len = buf->cursor_col - prev_pos;
    if (line_delete_bytes_at(line, prev_pos, char_len)) {
      buf->cursor_col = prev_pos;
      buf->modified = TRUE;
      return TRUE;
    }
#else
    if (line_delete_at(line, buf->cursor_col - 1)) {
      buf->cursor_col--;
      buf->modified = TRUE;
      return TRUE;
    }
#endif
  }
  else if (buf->cursor_line > 0) {
    /* Join with previous line */
    size_t prev_len = buf->lines[buf->cursor_line - 1].len;
    if (!buffer_join_line_up(buf, buf->cursor_line)) return FALSE;
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
#ifdef MRB_UTF8_STRING
    /* Delete entire UTF-8 character at cursor */
    size_t char_len = utf8_char_len(line->data + buf->cursor_col,
                                    line->data + line->len);
    if (line_delete_bytes_at(line, buf->cursor_col, char_len)) {
      buf->modified = TRUE;
      return TRUE;
    }
#else
    if (line_delete_at(line, buf->cursor_col)) {
      buf->modified = TRUE;
      return TRUE;
    }
#endif
  }
  else if (buf->cursor_line < buf->line_count - 1) {
    /* Join with next line */
    if (!buffer_join_line_up(buf, buf->cursor_line + 1)) return FALSE;
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
  if (!buffer_ensure_line_cap(buf)) return FALSE;

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
#ifdef MRB_UTF8_STRING
    /* Move back to start of previous UTF-8 character */
    mirb_line *line = &buf->lines[buf->cursor_line];
    buf->cursor_col = utf8_prev_char_start(line->data, buf->cursor_col);
#else
    buf->cursor_col--;
#endif
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
#ifdef MRB_UTF8_STRING
    /* Skip entire UTF-8 character */
    size_t char_len = utf8_char_len(line->data + buf->cursor_col,
                                    line->data + line->len);
    buf->cursor_col += char_len;
#else
    buf->cursor_col++;
#endif
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
    if (mirb_is_word_char(c)) break;
    buf->cursor_col--;
    moved = TRUE;
  }

  /* Move through word chars */
  while (buf->cursor_col > 0) {
    char c = buf->lines[buf->cursor_line].data[buf->cursor_col - 1];
    if (!mirb_is_word_char(c)) break;
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
    if (!mirb_is_word_char(line->data[buf->cursor_col])) break;
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

    if (mirb_is_word_char(line->data[buf->cursor_col])) break;
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
  while (end_col < line->len && mirb_is_word_char(line->data[end_col])) {
    end_col++;
  }

  /* Skip non-word chars */
  while (end_col < line->len && !mirb_is_word_char(line->data[end_col])) {
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

/*
 * Get cursor display column (visual column for terminal positioning)
 * When MRB_UTF8_STRING is defined, calculates display width considering
 * multibyte characters. Otherwise, returns the byte position directly.
 */
size_t
mirb_buffer_cursor_display_col(mirb_buffer *buf)
{
#ifdef MRB_UTF8_STRING
  mirb_line *line = &buf->lines[buf->cursor_line];
  return utf8_display_col(line->data, buf->cursor_col);
#else
  return buf->cursor_col;
#endif
}
