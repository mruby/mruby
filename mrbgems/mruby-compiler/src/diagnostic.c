#include "../include/mrc_diagnostic.h"

static void
line_and_column_by_scan(const uint8_t *source_start, const uint8_t *location_start, uint32_t *line, uint32_t *column)
{
  uint32_t start_offset = (uint32_t)(location_start - source_start);
  uint32_t l = 1, c = 1;
  const uint8_t *p = source_start;
  while (p < source_start + start_offset) {
    if (*p == '\n') {
      l++;
      c = 1;
    } else {
      c++;
    }
    p++;
  }
  *line = l;
  *column = c;
}

/* A diagnostic is placed by counting the newlines in front of it. Counting
   them afresh for each one makes a source with an error on every line
   quadratic, so past a few lookups the context indexes where each line of
   the source begins, in one scan, and a lookup is a binary search of that.
   The index is four bytes a line, which a compile that reports only a
   handful of diagnostics never allocates. It counts physical lines exactly
   as the scan does; Prism's own newline list does not serve, as it leaves
   out a newline the lexer takes as part of a token, the one in `?\`
   followed by a line break. */
#define DIAGNOSTIC_SCAN_LIMIT 16

static mrc_bool
line_starts_build(mrc_ccontext *c)
{
  const uint8_t *start = c->p->start, *end = c->p->end;
  uint32_t n = 1;

  for (const uint8_t *q = start; q < end; q++) {
    if (*q == '\n') n++;
  }
  uint32_t *v = (uint32_t *)mrc_malloc(c, sizeof(uint32_t) * n);
  if (!v) return FALSE;
  v[0] = 0;
  n = 1;
  for (const uint8_t *q = start; q < end; q++) {
    if (*q == '\n') v[n++] = (uint32_t)(q + 1 - start);
  }
  c->diagnostic_line_starts = v;
  c->diagnostic_line_count = n;
  return TRUE;
}

/* The line holding `off`: the last line start at or before it. */
static uint32_t
line_at(const uint32_t *v, uint32_t n, uint32_t off)
{
  uint32_t lo = 0, hi = n;

  while (hi - lo > 1) {
    uint32_t mid = lo + (hi - lo) / 2;
    if (v[mid] <= off) lo = mid;
    else hi = mid;
  }
  return lo;
}

/* The line and column of `location_start` counted from `file_start`, both
   from 1: a file joined after another need not begin a line, and on the line
   it begins, its columns count from where it does. */
static void
line_and_column(mrc_ccontext *c, const uint8_t *file_start, const uint8_t *location_start, uint32_t *line, uint32_t *column)
{
  if (!location_start) {
    *line = 0;
    *column = 0;
    return;
  }
  if (c->diagnostic_lookups < DIAGNOSTIC_SCAN_LIMIT) {
    c->diagnostic_lookups++;
    line_and_column_by_scan(file_start, location_start, line, column);
    return;
  }
  if (!c->diagnostic_line_starts && !line_starts_build(c)) {
    line_and_column_by_scan(file_start, location_start, line, column);
    return;
  }
  const uint32_t *v = c->diagnostic_line_starts;
  uint32_t n = c->diagnostic_line_count;
  uint32_t off = (uint32_t)(location_start - c->p->start);
  uint32_t foff = (uint32_t)(file_start - c->p->start);
  uint32_t li = line_at(v, n, off), fi = line_at(v, n, foff);

  *line = li - fi + 1;
  *column = (li == fi ? off - foff : off - v[li]) + 1;
}

/*
  const char *level;
  switch (diagnostic->level) {
    case PM_ERROR_LEVEL_SYNTAX:
      level = "syntax";
      break;
    case PM_ERROR_LEVEL_ARGUMENT:
      level = "argument";
      break;
    case PM_ERROR_LEVEL_LOAD:
      level = "load";
      break;
    default:
      abort();
  }
*/

const char *
mrc_diagnostic_code_to_string(mrc_diagnostic_code code)
{
  switch (code) {
    case MRC_PARSER_ERROR:
      return "syntax error";
    case MRC_GENERATOR_ERROR:
      return "generator error";
    case MRC_PARSER_WARNING:
      return "syntax warning";
    case MRC_GENERATOR_WARNING:
      return "generator warning";
    default:
      return "unknown";
  }
}

void
mrc_diagnostic_list_append(mrc_ccontext *c, const uint8_t * location_start, const char *message, mrc_diagnostic_code code)
{
  mrc_diagnostic_list *list = (mrc_diagnostic_list *)mrc_calloc(c, 1, sizeof(mrc_diagnostic_list));
  const uint8_t *file_start = c->p->start;
  list->filename = NULL;
  if (c->filename_table && 0 < c->filename_table_length && location_start) {
    uint32_t offset = (uint32_t)(location_start - c->p->start);
    int file_idx = 0;
    for (int i = 1; i < c->filename_table_length; i++) {
      if (offset < c->filename_table[i].start) break;
      file_idx = i;
    }
    list->filename = c->filename_table[file_idx].filename;
    file_start = c->p->start + c->filename_table[file_idx].start;
  }
  line_and_column(c, file_start, location_start, &list->line, &list->column);
  char buf[256];
  const char *diagnostic_code_str = mrc_diagnostic_code_to_string(code);
  snprintf(buf, sizeof(buf), "%s, %s", diagnostic_code_str, message);
  size_t len = strlen(buf);
  list->message = (char *)mrc_malloc(c, len + 1);
  memcpy(list->message, buf, len + 1);
  list->code = code;

  if (c->diagnostic_list == NULL) {
    c->diagnostic_list = list;
  }
  else {
    /* A list someone else started or emptied has its end found once. */
    mrc_diagnostic_list *p = c->diagnostic_tail;
    if (p == NULL) {
      p = c->diagnostic_list;
      while (p->next) {
        p = p->next;
      }
    }
    p->next = list;
  }
  c->diagnostic_tail = list;

  if (code == MRC_PARSER_ERROR || code == MRC_GENERATOR_ERROR) {
    c->capture_errors = TRUE;
  }
}

void
mrc_diagnostic_list_free(mrc_ccontext *c)
{
  mrc_diagnostic_list *p = c->diagnostic_list;
  while (p) {
    mrc_diagnostic_list *next = p->next;
    mrc_free(c, p->message);
    mrc_free(c, p);
    p = next;
  }
  c->diagnostic_list = NULL;
  c->diagnostic_tail = NULL;
  mrc_diagnostic_source_reset(c);
}

/* Drops what was worked out about the source being parsed, before a parse
   of another one: a context can be handed more than one source, and a
   buffer can come back at the same address holding another. */
void
mrc_diagnostic_source_reset(mrc_ccontext *c)
{
  if (c->diagnostic_line_starts) {
    mrc_free(c, c->diagnostic_line_starts);
    c->diagnostic_line_starts = NULL;
  }
  c->diagnostic_line_count = 0;
  c->diagnostic_lookups = 0;
}
