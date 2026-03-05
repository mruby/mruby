/*
** mirb_history.h - Command history for mirb editor
**
** See Copyright Notice in mruby.h
*/

#ifndef MIRB_HISTORY_H
#define MIRB_HISTORY_H

#include <mruby.h>
#include <stddef.h>

/* Default history size */
#define MIRB_HISTORY_SIZE 100

/*
 * History entry
 */
typedef struct mirb_history {
  char **entries;         /* array of history entries */
  size_t capacity;        /* max number of entries */
  size_t count;           /* current number of entries */
  size_t start;           /* index of oldest entry (circular buffer) */
  size_t pos;             /* current browsing position */
  char *saved_input;      /* saved current input when browsing */
  mrb_bool browsing;      /* TRUE if currently browsing history */
} mirb_history;

/*
 * Initialize history
 * Returns TRUE on success
 */
mrb_bool mirb_history_init(mirb_history *hist, size_t capacity);

/*
 * Free history resources
 */
void mirb_history_free(mirb_history *hist);

/*
 * Add entry to history
 * Empty strings are not added
 * Duplicate of last entry is not added
 */
void mirb_history_add(mirb_history *hist, const char *entry);

/*
 * Start browsing history
 * Saves the current input for later restoration
 */
void mirb_history_browse_start(mirb_history *hist, const char *current_input);

/*
 * Stop browsing history
 */
void mirb_history_browse_stop(mirb_history *hist);

/*
 * Get previous entry (older)
 * Returns NULL if at oldest entry or history is empty
 */
const char *mirb_history_prev(mirb_history *hist);

/*
 * Get next entry (newer)
 * Returns saved input if moving past newest entry
 * Returns NULL if not browsing
 */
const char *mirb_history_next(mirb_history *hist);

/*
 * Get current entry count
 */
size_t mirb_history_count(mirb_history *hist);

#endif /* MIRB_HISTORY_H */
