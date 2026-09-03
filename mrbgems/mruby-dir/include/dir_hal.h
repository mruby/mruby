/*
** dir_hal.h - Directory HAL interface for mruby
**
** See Copyright Notice in mruby.h
**
** Hardware Abstraction Layer for directory operations.
** Provides platform-independent interface for filesystem directory operations.
** All path and entry name strings use UTF-8.
*/

#ifndef MRUBY_DIR_HAL_H
#define MRUBY_DIR_HAL_H

#include <mruby.h>

/*
 * What the port implements
 *
 * The port publishes it in a header under its include/, and the build puts
 * that directory on the include path of this gem and of every gem that
 * depends on it.  Whether a method exists is the port's to say, because the
 * port is what a build names: a cross build has no host to detect one from,
 * and a `hal-dir-<conf>` gem may stand in for the bundled ports altogether.
 * What each macro says, and what it guards, is written where it is defined.
 */
#include "dir_hal_features.h"

MRB_BEGIN_DECL

/*
 * Platform-independent directory handle
 * Each HAL implementation defines this structure internally
 */
typedef struct mrb_dir_handle mrb_dir_handle;

/*
 * Directory Operations
 */

/* Open a UTF-8 path for reading. Returns handle or NULL on error (sets errno). */
mrb_dir_handle* mrb_hal_dir_open(mrb_state *mrb, const char *path);

/* Close directory handle. Returns 0 on success, -1 on error. */
int mrb_hal_dir_close(mrb_state *mrb, mrb_dir_handle *dir);

/* Read next entry as UTF-8. Returns name or NULL at end/error. */
const char* mrb_hal_dir_read(mrb_state *mrb, mrb_dir_handle *dir);

/* Rewind directory to beginning */
void mrb_hal_dir_rewind(mrb_state *mrb, mrb_dir_handle *dir);

/*
 * Operations a port declares in its dir_hal_features.h
 */

#ifdef MRB_HAL_DIR_HAS_SEEK
/* Seek to position in directory. Returns 0 on success, -1 on error (sets errno). */
int mrb_hal_dir_seek(mrb_state *mrb, mrb_dir_handle *dir, long pos);
#endif

#ifdef MRB_HAL_DIR_HAS_TELL
/* Get current position in directory. Returns the position, -1 on error (sets errno). */
long mrb_hal_dir_tell(mrb_state *mrb, mrb_dir_handle *dir);
#endif

/*
 * Filesystem Operations
 */

/* Create directory with mode (mode may be ignored on some platforms). Returns 0 on success, -1 on error. */
int mrb_hal_dir_mkdir(mrb_state *mrb, const char *path, int mode);

/* Remove empty directory. Returns 0 on success, -1 on error. */
int mrb_hal_dir_rmdir(mrb_state *mrb, const char *path);

/* Change current working directory. Returns 0 on success, -1 on error. */
int mrb_hal_dir_chdir(mrb_state *mrb, const char *path);

/* Get current working directory as UTF-8. Returns 0 on success, -1 on error. */
int mrb_hal_dir_getcwd(mrb_state *mrb, char *buf, size_t size);

#ifdef MRB_HAL_DIR_HAS_CHROOT
/* Change root directory (privileged operation). Returns 0 on success, -1 on error (sets errno). */
int mrb_hal_dir_chroot(mrb_state *mrb, const char *path);
#endif

/* Check if path is a directory. Returns 1 if directory, 0 if not. */
int mrb_hal_dir_is_directory(mrb_state *mrb, const char *path);

/*
 * HAL Initialization/Finalization
 */

/* Initialize HAL (called once at gem initialization) */
void mrb_hal_dir_init(mrb_state *mrb);

/* Cleanup HAL (called once at gem finalization) */
void mrb_hal_dir_final(mrb_state *mrb);

MRB_END_DECL

#endif /* MRUBY_DIR_HAL_H */
