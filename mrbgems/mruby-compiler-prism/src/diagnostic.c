#include "../include/mrc_diagnostic.h"

static void
line_and_column_by_start_and_offset(const uint8_t *source_start, const uint8_t *location_start, uint32_t *line, uint32_t *column)
{
  if (!location_start) {
    *line = 0;
    *column = 0;
    return;
  }
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
  mrc_diagnostic_list *list = mrc_calloc(c, 1, sizeof(mrc_diagnostic_list));
  const uint8_t *file_start = c->p->start;
  list->filename = NULL;
#ifndef MRC_NO_STDIO
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
#endif
  line_and_column_by_start_and_offset(file_start, location_start, &list->line, &list->column);
  char buf[256];
  const char *diagnostic_code_str = mrc_diagnostic_code_to_string(code);
  snprintf(buf, sizeof(buf), "%s, %s", diagnostic_code_str, message);
  size_t len = strlen(buf);
  list->message = (char *)mrc_malloc(c, len + 1);
  strcpy(list->message, buf);
  list->message[len] = '\0';
  list->code = code;

  if (c->diagnostic_list == NULL) {
    c->diagnostic_list = list;
  } else {
    mrc_diagnostic_list *p = c->diagnostic_list;
    while (p->next) {
      p = p->next;
    }
    p->next = list;
  }

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
}

