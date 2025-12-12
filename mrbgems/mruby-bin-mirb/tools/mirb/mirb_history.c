/*
** mirb_history.c - Command history for mirb editor
**
** See Copyright Notice in mruby.h
*/

#include "mirb_history.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Initialize history
 */
mrb_bool
mirb_history_init(mirb_history *hist, size_t capacity)
{
  memset(hist, 0, sizeof(*hist));

  if (capacity == 0) capacity = MIRB_HISTORY_SIZE;

  hist->entries = (char**)calloc(capacity, sizeof(char*));
  if (hist->entries == NULL) return FALSE;

  hist->capacity = capacity;
  hist->count = 0;
  hist->start = 0;
  hist->pos = 0;
  hist->saved_input = NULL;
  hist->browsing = FALSE;

  return TRUE;
}

/*
 * Free history resources
 */
void
mirb_history_free(mirb_history *hist)
{
  if (hist->entries) {
    for (size_t i = 0; i < hist->capacity; i++) {
      free(hist->entries[i]);
    }
    free(hist->entries);
    hist->entries = NULL;
  }
  free(hist->saved_input);
  hist->saved_input = NULL;
  hist->count = 0;
  hist->capacity = 0;
}

/*
 * Get actual index in circular buffer
 */
static size_t
actual_index(mirb_history *hist, size_t logical_idx)
{
  return (hist->start + logical_idx) % hist->capacity;
}

/*
 * Add entry to history
 */
void
mirb_history_add(mirb_history *hist, const char *entry)
{
  if (entry == NULL || entry[0] == '\0') return;

  /* Don't add if same as last entry */
  if (hist->count > 0) {
    size_t last_idx = actual_index(hist, hist->count - 1);
    if (strcmp(hist->entries[last_idx], entry) == 0) {
      return;
    }
  }

  char *copy = strdup(entry);
  if (copy == NULL) return;

  if (hist->count < hist->capacity) {
    /* Still have room */
    size_t idx = actual_index(hist, hist->count);
    hist->entries[idx] = copy;
    hist->count++;
  }
  else {
    /* Buffer is full, overwrite oldest */
    size_t idx = hist->start;
    free(hist->entries[idx]);
    hist->entries[idx] = copy;
    hist->start = (hist->start + 1) % hist->capacity;
  }

  /* Reset browsing state */
  hist->browsing = FALSE;
  hist->pos = hist->count;
}

/*
 * Start browsing history
 */
void
mirb_history_browse_start(mirb_history *hist, const char *current_input)
{
  if (hist->browsing) return;

  free(hist->saved_input);
  hist->saved_input = current_input ? strdup(current_input) : NULL;
  hist->browsing = TRUE;
  hist->pos = hist->count;  /* Start past the end (at current input) */
}

/*
 * Stop browsing history
 */
void
mirb_history_browse_stop(mirb_history *hist)
{
  free(hist->saved_input);
  hist->saved_input = NULL;
  hist->browsing = FALSE;
  hist->pos = hist->count;
}

/*
 * Get previous entry (older)
 */
const char *
mirb_history_prev(mirb_history *hist)
{
  if (hist->count == 0) return NULL;

  if (!hist->browsing) {
    /* Should call browse_start first, but handle gracefully */
    hist->browsing = TRUE;
    hist->pos = hist->count;
  }

  if (hist->pos == 0) {
    /* Already at oldest entry */
    return NULL;
  }

  hist->pos--;
  return hist->entries[actual_index(hist, hist->pos)];
}

/*
 * Get next entry (newer)
 */
const char *
mirb_history_next(mirb_history *hist)
{
  if (!hist->browsing) return NULL;

  if (hist->pos >= hist->count) {
    /* Already at current input */
    return NULL;
  }

  hist->pos++;

  if (hist->pos >= hist->count) {
    /* Moved past newest entry, return saved input */
    return hist->saved_input ? hist->saved_input : "";
  }

  return hist->entries[actual_index(hist, hist->pos)];
}

/*
 * Get current entry count
 */
size_t
mirb_history_count(mirb_history *hist)
{
  return hist->count;
}
