/*
** mirb_editor.c - Multi-line editor for mirb
**
** See Copyright Notice in mruby.h
*/

#include "mirb_editor.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* ANSI color codes */
#define COLOR_GREEN   "\033[32m"

/*
 * Check if line contains only whitespace before given column
 */
static mrb_bool
line_is_blank_before(const char *line, size_t col)
{
  for (size_t i = 0; i < col; i++) {
    if (line[i] != ' ' && line[i] != '\t') {
      return FALSE;
    }
  }
  return TRUE;
}

/*
 * Get leading whitespace count on current line
 */
static size_t
leading_spaces(const char *line)
{
  size_t count = 0;
  while (line[count] == ' ' || line[count] == '\t') {
    count++;
  }
  return count;
}

/*
 * Dedent keyword table: keywords that reduce indentation level.
 * allow_eol: keyword can appear alone at end of line
 * delims: valid non-NUL characters that can follow the keyword
 */
static const struct {
  const char *word;
  const char *delims;
  mrb_bool allow_eol;
} dedent_table[] = {
  {"else",   " \t",   TRUE},
  {"elsif",  " ",     FALSE},
  {"end",    " \t.)", TRUE},
  {"ensure", " \t",   TRUE},
  {"in",     " ",     FALSE},
  {"rescue", " \t",   TRUE},
  {"when",   " ",     FALSE},
};

static mrb_bool
is_dedent_keyword(const char *content)
{
  size_t i;

  if (content[0] == '}') return TRUE;
  for (i = 0; i < sizeof(dedent_table)/sizeof(dedent_table[0]); i++) {
    size_t len = strlen(dedent_table[i].word);
    if (strncmp(content, dedent_table[i].word, len) == 0) {
      char c = content[len];
      if (c == '\0') return dedent_table[i].allow_eol;
      return strchr(dedent_table[i].delims, c) != NULL;
    }
  }
  return FALSE;
}

/*
 * Check if a line contains only whitespace (or is empty)
 */
static mrb_bool
is_line_blank(const mirb_line *line)
{
  for (size_t i = 0; i < line->len; i++) {
    if (line->data[i] != ' ' && line->data[i] != '\t') {
      return FALSE;
    }
  }
  return TRUE;
}

/*
 * Indent keyword table: keywords that affect indentation level.
 * delta: +1 for block-opening, -1 for block-closing
 * allow_eol: keyword can appear at end of line/string
 * delims: valid non-NUL characters that can follow the keyword
 */
static const struct {
  const char *word;
  const char *delims;
  mrb_bool allow_eol;
  int delta;
} indent_table[] = {
  {"begin",  "\n #",    TRUE,  +1},
  {"case",   " ",       FALSE, +1},
  {"class",  " ",       FALSE, +1},
  {"def",    " ",       FALSE, +1},
  {"do",     "\n #|",   TRUE,  +1},
  {"end",    "\n #.)",  TRUE,  -1},
  {"for",    " ",       FALSE, +1},
  {"if",     " ",       FALSE, +1},
  {"module", " ",       FALSE, +1},
  {"unless", " ",       FALSE, +1},
  {"until",  " ",       FALSE, +1},
  {"while",  " ",       FALSE, +1},
};

/*
 * Calculate indent level by counting open blocks in code
 */
static int
calc_indent_level(const char *code)
{
  int level = 0;
  const char *p = code;
  int at_line_start = 1;

  while (*p) {
    /* Skip strings */
    if (*p == '"' || *p == '\'') {
      char quote = *p++;
      while (*p && *p != quote) {
        if (*p == '\\' && p[1]) p++;
        p++;
      }
      if (*p) p++;
      at_line_start = 0;
      continue;
    }
    /* Skip comments */
    if (*p == '#') {
      while (*p && *p != '\n') p++;
      continue;
    }
    /* Track line starts for keyword detection */
    if (*p == '\n') {
      at_line_start = 1;
      p++;
      continue;
    }
    /* Skip whitespace but don't change at_line_start yet */
    if (*p == ' ' || *p == '\t') {
      p++;
      continue;
    }
    /* Check for block keywords at word boundary */
    if (at_line_start || (p > code && !mirb_is_word_char(p[-1]))) {
      size_t ki;
      for (ki = 0; ki < sizeof(indent_table)/sizeof(indent_table[0]); ki++) {
        size_t len = strlen(indent_table[ki].word);
        if (strncmp(p, indent_table[ki].word, len) == 0) {
          char c = p[len];
          if ((c == '\0' && indent_table[ki].allow_eol) ||
              (c != '\0' && strchr(indent_table[ki].delims, c))) {
            level += indent_table[ki].delta;
            if (level < 0) level = 0;
          }
          break;
        }
      }
    }
    /* Check for block opening/closing with braces */
    if (*p == '{') {
      level++;
    }
    else if (*p == '}') {
      if (level > 0) level--;
    }
    at_line_start = 0;
    p++;
  }
  return level;
}

/*
 * Calculate expected indent level for the given line index.
 * Uses code up to line_idx-1 to determine nesting depth.
 */
static int
calc_expected_indent(mirb_buffer *buf, size_t line_idx)
{
  int indent = 0;
  char *partial;

  if (line_idx == 0) return 0;
  partial = mirb_buffer_to_string_upto_line(buf, line_idx - 1);
  if (partial) {
    indent = calc_indent_level(partial);
    free(partial);
  }
  return indent;
}

/*
 * Adjust current line's leading whitespace to target_spaces.
 * Preserves cursor position relative to line content.
 */
static void
adjust_line_indent(mirb_buffer *buf, size_t target_spaces)
{
  size_t current_spaces = leading_spaces(mirb_buffer_current_line(buf));
  size_t saved_col = buf->cursor_col;

  if (target_spaces == current_spaces) return;

  if (target_spaces > current_spaces) {
    size_t add = target_spaces - current_spaces;
    buf->cursor_col = 0;
    for (size_t i = 0; i < add; i++) {
      mirb_buffer_insert_char(buf, ' ');
    }
    buf->cursor_col = saved_col + add;
  }
  else {
    size_t to_remove = current_spaces - target_spaces;
    buf->cursor_col = 0;
    for (size_t i = 0; i < to_remove; i++) {
      mirb_buffer_delete_forward(buf);
    }
    buf->cursor_col = (saved_col > to_remove) ? (saved_col - to_remove) : 0;
  }
}

/*
 * Insert indent spaces at cursor position
 */
static void
insert_indent_spaces(mirb_buffer *buf, int indent_level)
{
  for (int i = 0; i < indent_level * 2; i++) {
    mirb_buffer_insert_char(buf, ' ');
  }
}

/*
 * Check if we should dedent after typing a character
 * Returns TRUE if current line starts with 'end' or '}' after only whitespace
 */
static mrb_bool
should_dedent(mirb_buffer *buf, char last_char)
{
  const char *line = mirb_buffer_current_line(buf);
  size_t col = buf->cursor_col;
  size_t line_len = buf->lines[buf->cursor_line].len;

  /* Check for '}' - dedent immediately when typed at line start */
  if (last_char == '}') {
    if (col == 1 || (col > 1 && line_is_blank_before(line, col - 1))) {
      return TRUE;
    }
  }

  /* Helper macro: check keyword completion */
  #define CHECK_KEYWORD(keyword, len, trigger_char) \
    if (last_char == trigger_char && col >= len) { \
      if (strncmp(line + col - len, keyword, len) == 0) { \
        if (col == len || line_is_blank_before(line, col - len)) { \
          if (col == line_len || \
              line[col] == ' ' || line[col] == '\t' || line[col] == '\0' || \
              line[col] == '\n' || line[col] == '.' || line[col] == ')') { \
            return TRUE; \
          } \
        } \
      } \
    }

  /* Check for 'end' - dedent when 'd' completes "end" */
  CHECK_KEYWORD("end", 3, 'd');

  /* Check for 'else' - dedent when 'e' completes "else" */
  CHECK_KEYWORD("else", 4, 'e');

  /* Check for 'elsif' - dedent when 'f' completes "elsif" */
  CHECK_KEYWORD("elsif", 5, 'f');

  /* Check for 'when' - dedent when 'n' completes "when" */
  CHECK_KEYWORD("when", 4, 'n');

  /* Check for 'in' - dedent when 'n' completes "in" (pattern matching) */
  CHECK_KEYWORD("in", 2, 'n');

  /* Check for 'rescue' - dedent when 'e' completes "rescue" */
  CHECK_KEYWORD("rescue", 6, 'e');

  /* Check for 'ensure' - dedent when 'e' completes "ensure" */
  CHECK_KEYWORD("ensure", 6, 'e');

  #undef CHECK_KEYWORD

  return FALSE;
}

/*
 * Perform dedentation - adjust indent for dedent keyword
 */
static void
perform_dedent(mirb_buffer *buf)
{
  int indent = calc_expected_indent(buf, buf->cursor_line);
  if (indent > 0) indent--;
  adjust_line_indent(buf, (size_t)(indent * 2));
}


/*
 * Re-indent current line to match expected indent level
 * Used before inserting newline to fix any misaligned indentation
 */
static void
reindent_line(mirb_buffer *buf)
{
  mirb_line *line = &buf->lines[buf->cursor_line];
  int indent = calc_expected_indent(buf, buf->cursor_line);
  const char *content = line->data + leading_spaces(line->data);
  if (is_dedent_keyword(content)) {
    if (indent > 0) indent--;
  }
  adjust_line_indent(buf, (size_t)(indent * 2));
}

/*
 * Initialize editor
 */
mrb_bool
mirb_editor_init(mirb_editor *ed)
{
  memset(ed, 0, sizeof(*ed));

  if (!mirb_term_init(&ed->term)) {
    /* Terminal init may fail but we can still work in simple mode */
  }

  if (!mirb_buffer_init(&ed->buf)) {
    mirb_term_cleanup(&ed->term);
    return FALSE;
  }

  if (!mirb_history_init(&ed->hist, MIRB_HISTORY_SIZE)) {
    mirb_buffer_free(&ed->buf);
    mirb_term_cleanup(&ed->term);
    return FALSE;
  }

  ed->prompt = "> ";
  ed->prompt_cont = "* ";
  ed->prompt_len = 2;
  ed->prompt_cont_len = 2;
  ed->prompt_fmt = NULL;
  ed->prompt_cont_fmt = NULL;
  ed->line_num_base = 1;
  ed->use_color = FALSE;
  mirb_highlight_init(&ed->highlight, FALSE);
  ed->initialized = TRUE;

  return TRUE;
}

/*
 * Cleanup editor
 */
void
mirb_editor_cleanup(mirb_editor *ed)
{
  if (!ed->initialized) return;

  mirb_history_free(&ed->hist);
  mirb_buffer_free(&ed->buf);
  mirb_term_cleanup(&ed->term);
  ed->initialized = FALSE;
}

/*
 * Set prompts (fixed strings)
 */
void
mirb_editor_set_prompts(mirb_editor *ed, const char *prompt, const char *prompt_cont)
{
  ed->prompt = prompt;
  ed->prompt_cont = prompt_cont;
  ed->prompt_len = strlen(prompt);
  ed->prompt_cont_len = strlen(prompt_cont);
  ed->prompt_fmt = NULL;
  ed->prompt_cont_fmt = NULL;
}

/*
 * Set prompt format strings for line-numbered prompts
 */
void
mirb_editor_set_prompt_format(mirb_editor *ed, const char *prompt_fmt,
                               const char *prompt_cont_fmt, int line_num)
{
  ed->prompt_fmt = prompt_fmt;
  ed->prompt_cont_fmt = prompt_cont_fmt;
  ed->line_num_base = line_num;
  /* Estimate prompt length (assuming line numbers up to 999) */
  ed->prompt_len = strlen(prompt_fmt) + 2;  /* %d -> up to 3 digits, minus 2 for %d */
  ed->prompt_cont_len = strlen(prompt_cont_fmt) + 2;
}

/*
 * Set completion checker
 */
void
mirb_editor_set_check_complete(mirb_editor *ed, mirb_check_complete_fn *fn, void *user_data)
{
  ed->check_complete = fn;
  ed->check_complete_data = user_data;
}

/*
 * Set tab completion callbacks
 */
void
mirb_editor_set_tab_complete(mirb_editor *ed,
                              mirb_tab_complete_fn *complete_fn,
                              mirb_tab_complete_free_fn *free_fn,
                              void *user_data)
{
  ed->tab_complete = complete_fn;
  ed->tab_complete_free = free_fn;
  ed->tab_complete_data = user_data;
}

/*
 * Handle tab completion
 * Returns TRUE if completion was performed
 */

/*
 * Handle tab auto-indent
 * Adjusts current line's indentation to match the expected level
 */
static void
handle_tab_indent(mirb_editor *ed)
{
  reindent_line(&ed->buf);
}

static mrb_bool
handle_tab_completion(mirb_editor *ed)
{
  char **completions = NULL;
  int count, prefix_len;
  const char *current_line;
  int cursor_col;

  if (!ed->tab_complete) return FALSE;

  /* Get current line and cursor position */
  current_line = ed->buf.lines[ed->buf.cursor_line].data;
  cursor_col = (int)ed->buf.cursor_col;

  /* Get completions */
  count = ed->tab_complete(current_line, cursor_col, &completions, &prefix_len,
                           ed->tab_complete_data);

  if (count == 0 || !completions) {
    return FALSE;
  }

  if (count == 1) {
    /* Single completion - insert it */
    const char *completion = completions[0];
    int i;

    /* Delete the prefix we're replacing */
    for (i = 0; i < prefix_len; i++) {
      mirb_buffer_delete_back(&ed->buf);
    }

    /* Insert completion */
    mirb_buffer_insert_string(&ed->buf, completion, strlen(completion));
  }
  else {
    /* Multiple completions - find common prefix and show options */
    int common_len = (int)strlen(completions[0]);
    int i, j;

    /* Find longest common prefix */
    for (i = 1; i < count; i++) {
      for (j = 0; j < common_len && completions[i][j]; j++) {
        if (completions[0][j] != completions[i][j]) {
          common_len = j;
          break;
        }
      }
      if (j < common_len) common_len = j;
    }

    if (common_len > prefix_len) {
      /* Extend with common prefix */
      for (i = 0; i < prefix_len; i++) {
        mirb_buffer_delete_back(&ed->buf);
      }
      mirb_buffer_insert_string(&ed->buf, completions[0], common_len);
    }
    else {
      /* Show all completions */
      printf("\r\n");
      for (i = 0; i < count; i++) {
        printf("%s  ", completions[i]);
        if ((i + 1) % 4 == 0 && i + 1 < count) printf("\r\n");
      }
      printf("\r\n");
      /* Force full redraw */
      ed->prev_line_count = 0;
    }
  }

  /* Free completions */
  if (ed->tab_complete_free) {
    ed->tab_complete_free(completions, count, ed->tab_complete_data);
  }

  return TRUE;
}

/*
 * Enable/disable color
 */
void
mirb_editor_set_color(mirb_editor *ed, mrb_bool enable)
{
  ed->use_color = enable;
  mirb_highlight_init(&ed->highlight, enable);
}

/*
 * Check if multi-line editing is supported
 */
mrb_bool
mirb_editor_supported(mirb_editor *ed)
{
  return ed->term.supported;
}

/*
 * Calculate prompt length for given line
 */
static size_t
calc_prompt_len(mirb_editor *ed, size_t line_idx)
{
  if (ed->prompt_fmt != NULL) {
    /* Format string: calculate actual length */
    int line_num = ed->line_num_base + (int)line_idx;
    const char *fmt = (line_idx == 0) ? ed->prompt_fmt : ed->prompt_cont_fmt;
    return (size_t)snprintf(NULL, 0, fmt, line_num);
  }
  else {
    /* Fixed prompt string */
    return (line_idx == 0) ? ed->prompt_len : ed->prompt_cont_len;
  }
}

/*
 * Print prompt for given line
 */
static void
print_prompt(mirb_editor *ed, size_t line_idx)
{
  int line_num = ed->line_num_base + (int)line_idx;

  if (ed->use_color) {
    printf("%s", COLOR_GREEN);
  }

  if (ed->prompt_fmt != NULL) {
    /* Use format string with line number */
    const char *fmt = (line_idx == 0) ? ed->prompt_fmt : ed->prompt_cont_fmt;
    printf(fmt, line_num);
  }
  else {
    /* Use fixed prompt string */
    const char *p = (line_idx == 0) ? ed->prompt : ed->prompt_cont;
    printf("%s", p);
  }

  if (ed->use_color) {
    printf("%s", COLOR_RESET);
  }
}

/*
 * Refresh display - uses natural terminal scrolling like irb
 *
 * Strategy:
 * - Track which screen row we started on
 * - Move cursor back to start, clear everything below, redraw all lines
 * - This allows terminal to scroll naturally without corrupting history
 */
static void
refresh_display(mirb_editor *ed)
{
  size_t lines_to_go_up;

  /* Calculate how many lines up we need to go to reach start of input */
  /* We're currently on cursor_line, and prev_line_count tells us total displayed */
  if (ed->prev_line_count > 0) {
    /* Go up from current position to first line of input */
    lines_to_go_up = ed->display_cursor_row;
    if (lines_to_go_up > 0) {
      mirb_term_cursor_up((int)lines_to_go_up);
    }
  }

  /* Move to column 1 and clear from here to end of screen */
  mirb_term_cursor_col(1);
  mirb_term_clear_below();

  /* Reset highlight state for fresh scan */
  mirb_highlight_reset(&ed->highlight);

  /* Redraw all lines */
  for (size_t i = 0; i < ed->buf.line_count; i++) {
    print_prompt(ed, i);
    mirb_highlight_print_line(&ed->highlight, mirb_buffer_line_at(&ed->buf, i));

    if (i < ed->buf.line_count - 1) {
      printf("\r\n");
    }
  }

  /* Now position cursor correctly */
  /* We're at the end of last line, need to go to cursor position */
  size_t lines_up_from_end = ed->buf.line_count - 1 - ed->buf.cursor_line;
  if (lines_up_from_end > 0) {
    mirb_term_cursor_up((int)lines_up_from_end);
  }

  /* Position column on cursor line (calculate actual prompt length) */
  size_t prompt_len = calc_prompt_len(ed, ed->buf.cursor_line);
  size_t display_col = mirb_buffer_cursor_display_col(&ed->buf);
  mirb_term_cursor_col((int)(prompt_len + display_col + 1));

  /* Update tracking */
  ed->prev_line_count = ed->buf.line_count;
  ed->display_cursor_row = ed->buf.cursor_line;

  mirb_term_flush();
}

/*
 * Handle a keypress
 * Returns TRUE to continue editing, FALSE to finish
 */
static mrb_bool
handle_key(mirb_editor *ed, int key, mirb_edit_result *result)
{
  switch (key) {
  case MIRB_KEY_ENTER:
    /* Stop history browsing */
    mirb_history_browse_stop(&ed->hist);
    /* Re-indent current line before inserting newline */
    reindent_line(&ed->buf);
    {
      /*
       * Smart Enter behavior:
       * - Only evaluate when cursor is at end of last line and code is complete
       * - If cursor is not at end of last line, always insert/split (no evaluation)
       * - If next line is blank last line and cursor at end, move to it
       */
      mrb_bool at_last_line = (ed->buf.cursor_line == ed->buf.line_count - 1);
      mirb_line *current_line = &ed->buf.lines[ed->buf.cursor_line];
      mrb_bool at_end_of_line = (ed->buf.cursor_col == current_line->len);
      mrb_bool can_evaluate = at_last_line && at_end_of_line;

      /* Check for smart navigation to existing blank last line */
      if (!at_last_line && at_end_of_line) {
        size_t next_line_idx = ed->buf.cursor_line + 1;
        mrb_bool next_is_last = (next_line_idx == ed->buf.line_count - 1);

        if (next_is_last) {
          mirb_line *next_line = &ed->buf.lines[next_line_idx];
          if (is_line_blank(next_line)) {
            /* Move to existing blank last line with proper indentation */
            int indent = calc_expected_indent(&ed->buf, ed->buf.line_count);
            mirb_buffer_cursor_down(&ed->buf);
            /* Clear existing whitespace and set correct indent */
            mirb_line *line = &ed->buf.lines[ed->buf.cursor_line];
            line->len = 0;
            line->data[0] = '\0';
            ed->buf.cursor_col = 0;
            insert_indent_spaces(&ed->buf, indent);
            return TRUE;
          }
        }
      }

      /* If cursor in middle of line and next is blank last, remove it before split */
      if (!at_last_line && !at_end_of_line) {
        size_t next_line_idx = ed->buf.cursor_line + 1;
        if (next_line_idx == ed->buf.line_count - 1) {
          mirb_line *next_line = &ed->buf.lines[next_line_idx];
          if (is_line_blank(next_line)) {
            mirb_buffer_delete_line(&ed->buf, next_line_idx);
          }
        }
      }

      /* Check if input is complete - only when cursor at end of last line */
      if (can_evaluate && ed->check_complete) {
        char *code = mirb_buffer_to_string(&ed->buf);
        if (code) {
          mrb_bool complete = ed->check_complete(code, ed->check_complete_data);
          if (complete) {
            free(code);
            *result = MIRB_EDIT_OK;
            return FALSE;
          }
          /* Code not complete - add new line with indentation */
          int indent = calc_indent_level(code);
          free(code);
          mirb_buffer_newline(&ed->buf);
          insert_indent_spaces(&ed->buf, indent);
          return TRUE;
        }
      }

      /* Not at end of last line - just insert/split with appropriate indent */
      {
        int indent = calc_expected_indent(&ed->buf, ed->buf.cursor_line + 1);
        mirb_buffer_newline(&ed->buf);

        /* Check if new line starts with dedenting keyword */
        mirb_line *new_line = &ed->buf.lines[ed->buf.cursor_line];
        if (is_dedent_keyword(new_line->data)) {
          if (indent > 0) indent--;
        }

        insert_indent_spaces(&ed->buf, indent);
        return TRUE;
      }
    }

  case MIRB_KEY_CTRL_C:
    *result = MIRB_EDIT_INTERRUPT;
    return FALSE;

  case MIRB_KEY_CTRL_D:
    if (mirb_buffer_total_len(&ed->buf) == 0) {
      *result = MIRB_EDIT_EOF;
      return FALSE;
    }
    /* Delete forward if not empty */
    mirb_buffer_delete_forward(&ed->buf);
    return TRUE;

  case MIRB_KEY_BACKSPACE:
    mirb_buffer_delete_back(&ed->buf);
    return TRUE;

  case MIRB_KEY_DELETE:
    mirb_buffer_delete_forward(&ed->buf);
    return TRUE;

  case MIRB_KEY_LEFT:
  case MIRB_KEY_CTRL_B:
    mirb_buffer_cursor_left(&ed->buf);
    return TRUE;

  case MIRB_KEY_RIGHT:
  case MIRB_KEY_CTRL_F:
    mirb_buffer_cursor_right(&ed->buf);
    return TRUE;

  case MIRB_KEY_UP:
  case MIRB_KEY_CTRL_P:
    /* If on first line, navigate history; otherwise move cursor up */
    if (ed->buf.cursor_line == 0) {
      /* Start history browsing if not already */
      if (!ed->hist.browsing) {
        char *current = mirb_buffer_to_string(&ed->buf);
        mirb_history_browse_start(&ed->hist, current);
        free(current);
      }
      const char *prev = mirb_history_prev(&ed->hist);
      if (prev) {
        mirb_buffer_set_string(&ed->buf, prev);
        mirb_buffer_cursor_finish(&ed->buf);
      }
    }
    else {
      mirb_buffer_cursor_up(&ed->buf);
    }
    return TRUE;

  case MIRB_KEY_DOWN:
  case MIRB_KEY_CTRL_N:
    /* If on last line, navigate history; otherwise move cursor down */
    if (ed->buf.cursor_line == ed->buf.line_count - 1) {
      if (ed->hist.browsing) {
        const char *next = mirb_history_next(&ed->hist);
        if (next) {
          mirb_buffer_set_string(&ed->buf, next);
          mirb_buffer_cursor_finish(&ed->buf);
        }
      }
    }
    else {
      mirb_buffer_cursor_down(&ed->buf);
    }
    return TRUE;

  case MIRB_KEY_HOME:
  case MIRB_KEY_CTRL_A:
    mirb_buffer_cursor_home(&ed->buf);
    return TRUE;

  case MIRB_KEY_END:
  case MIRB_KEY_CTRL_E:
    mirb_buffer_cursor_end(&ed->buf);
    return TRUE;

  case MIRB_KEY_CTRL_K:
    mirb_buffer_kill_to_end(&ed->buf);
    return TRUE;

  case MIRB_KEY_CTRL_U:
    mirb_buffer_kill_to_start(&ed->buf);
    return TRUE;

  case MIRB_KEY_CTRL_W:
    mirb_buffer_kill_word_back(&ed->buf);
    return TRUE;

  case MIRB_KEY_CTRL_Y:
    mirb_buffer_yank(&ed->buf);
    return TRUE;

  case MIRB_KEY_ALT_B:
    mirb_buffer_cursor_word_back(&ed->buf);
    return TRUE;

  case MIRB_KEY_ALT_F:
    mirb_buffer_cursor_word_forward(&ed->buf);
    return TRUE;

  case MIRB_KEY_ALT_D:
    mirb_buffer_kill_word_forward(&ed->buf);
    return TRUE;

  case MIRB_KEY_CTRL_L:
    /* Clear screen and refresh */
    mirb_term_clear_screen();
    ed->prev_line_count = 0;
    return TRUE;

  case MIRB_KEY_TAB:
    /* Auto-indent if at start/end of line or preceded by whitespace */
    {
      mirb_line *line = &ed->buf.lines[ed->buf.cursor_line];
      mrb_bool do_indent = FALSE;

      if (ed->buf.cursor_col == 0) {
        do_indent = TRUE;
      }
      else if (ed->buf.cursor_col == line->len) {
        /* At end of line */
        do_indent = TRUE;
      }
      else {
        char prev_char = line->data[ed->buf.cursor_col - 1];
        if (prev_char == ' ' || prev_char == '\t') {
          do_indent = TRUE;
        }
      }

      if (do_indent) {
        handle_tab_indent(ed);
      }
      else {
        handle_tab_completion(ed);
      }
    }
    return TRUE;

  default:
    /* Insert printable characters */
    if (key >= 32 && key < 127) {
      /* Stop history browsing when user types */
      mirb_history_browse_stop(&ed->hist);
      mirb_buffer_insert_char(&ed->buf, (char)key);
      /* Check for auto-dedent after typing 'end' or '}' */
      if (should_dedent(&ed->buf, (char)key)) {
        perform_dedent(&ed->buf);
      }
    }
#ifdef MRB_UTF8_STRING
    /* Handle UTF-8 multibyte characters (bytes >= 0x80) */
    else if (key >= 128 && key <= 255) {
      mirb_history_browse_stop(&ed->hist);
      mirb_buffer_insert_char(&ed->buf, (char)key);
    }
#endif
    return TRUE;
  }
}

/*
 * Read input with multi-line editing
 */
mirb_edit_result
mirb_editor_read(mirb_editor *ed, char **out_str)
{
  mirb_edit_result result;
  int key;

  *out_str = NULL;

  /* Fall back to simple mode if raw mode not supported */
  if (!ed->term.supported) {
    return mirb_editor_read_simple(ed, out_str);
  }

  /* Clear buffer for new input */
  mirb_buffer_clear(&ed->buf);
  ed->prev_line_count = 0;
  ed->display_cursor_row = 0;

  /* Enable raw mode */
  if (!mirb_term_raw_enable(&ed->term)) {
    return mirb_editor_read_simple(ed, out_str);
  }

  /* Initial display */
  print_prompt(ed, 0);
  mirb_term_flush();
  ed->prev_line_count = 1;
  ed->display_cursor_row = 0;

  /* Main editing loop */
  result = MIRB_EDIT_ERROR;
  while (1) {
    key = mirb_term_read_key(&ed->term);
    if (key == MIRB_KEY_NONE) {
      result = MIRB_EDIT_ERROR;
      break;
    }

    if (!handle_key(ed, key, &result)) {
      break;
    }

    refresh_display(ed);
  }

  /* Disable raw mode */
  mirb_term_raw_disable(&ed->term);

  /* Move to end and print newline */
  if (ed->buf.cursor_line < ed->buf.line_count - 1) {
    mirb_term_cursor_down((int)(ed->buf.line_count - 1 - ed->buf.cursor_line));
  }
  printf("\n");

  /* Return result string */
  if (result == MIRB_EDIT_OK) {
    *out_str = mirb_buffer_to_string(&ed->buf);
    if (*out_str == NULL) {
      result = MIRB_EDIT_ERROR;
    }
  }

  return result;
}

/*
 * Simple single-line input (fallback)
 */
mirb_edit_result
mirb_editor_read_simple(mirb_editor *ed, char **out_str)
{
  char line[4096];
  size_t total_len = 0;
  size_t total_cap = 4096;
  char *total = (char*)malloc(total_cap);
  mrb_bool first_line = TRUE;

  *out_str = NULL;

  if (total == NULL) return MIRB_EDIT_ERROR;
  total[0] = '\0';

  while (1) {
    /* Print prompt */
    print_prompt(ed, first_line ? 0 : 1);
    fflush(stdout);

    /* Read line */
    if (fgets(line, sizeof(line), stdin) == NULL) {
      if (total_len == 0) {
        free(total);
        return MIRB_EDIT_EOF;
      }
      break;
    }

    /* Remove trailing newline */
    size_t len = strlen(line);
    if (len > 0 && line[len - 1] == '\n') {
      line[--len] = '\0';
    }

    /* Append to total */
    if (!first_line) {
      /* Add newline separator */
      if (total_len + 1 >= total_cap) {
        total_cap *= 2;
        char *new_total = (char*)realloc(total, total_cap);
        if (new_total == NULL) {
          free(total);
          return MIRB_EDIT_ERROR;
        }
        total = new_total;
      }
      total[total_len++] = '\n';
    }

    if (total_len + len >= total_cap) {
      total_cap *= 2;
      char *new_total = (char*)realloc(total, total_cap);
      if (new_total == NULL) {
        free(total);
        return MIRB_EDIT_ERROR;
      }
      total = new_total;
    }
    memcpy(total + total_len, line, len + 1);
    total_len += len;

    first_line = FALSE;

    /* Check if complete */
    if (ed->check_complete) {
      if (ed->check_complete(total, ed->check_complete_data)) {
        break;
      }
    }
    else {
      break;  /* No checker, single line mode */
    }
  }

  *out_str = total;
  return MIRB_EDIT_OK;
}

/*
 * Add entry to history
 */
void
mirb_editor_history_add(mirb_editor *ed, const char *entry)
{
  mirb_history_add(&ed->hist, entry);
}
