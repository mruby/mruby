/*
** dir_hal.h - Directory HAL interface for mruby
**
** See Copyright Notice in mruby.h
**
** Hardware Abstraction Layer for directory operations.
** Provides platform-independent interface for filesystem directory operations.
*/

#ifndef MRUBY_DIR_HAL_H
#define MRUBY_DIR_HAL_H

#include <mruby.h>

/*
 * Platform-independent directory handle
 * Each HAL implementation defines this structure internally
 */
typedef struct mrb_dir_handle mrb_dir_handle;

/*
 * Directory Operations
 */

/* Open directory for reading. Returns handle or NULL on error (sets errno). */
mrb_dir_handle* mrb_hal_dir_open(mrb_state *mrb, const char *path);

/* Close directory handle. Returns 0 on success, -1 on error. */
int mrb_hal_dir_close(mrb_state *mrb, mrb_dir_handle *dir);

/* Read next entry from directory. Returns name or NULL at end/error. */
const char* mrb_hal_dir_read(mrb_state *mrb, mrb_dir_handle *dir);

/* Rewind directory to beginning */
void mrb_hal_dir_rewind(mrb_state *mrb, mrb_dir_handle *dir);

/*
 * Optional Operations (may not be available on all platforms)
 */

/* Seek to position in directory. Returns -1 if unsupported (sets errno to ENOSYS). */
int mrb_hal_dir_seek(mrb_state *mrb, mrb_dir_handle *dir, long pos);

/* Get current position in directory. Returns -1 if unsupported (sets errno to ENOSYS). */
long mrb_hal_dir_tell(mrb_state *mrb, mrb_dir_handle *dir);

/*
 * Filesystem Operations
 */

/* Create directory with mode (mode may be ignored on some platforms). Returns 0 on success, -1 on error. */
int mrb_hal_dir_mkdir(mrb_state *mrb, const char *path, int mode);

/* Remove empty directory. Returns 0 on success, -1 on error. */
int mrb_hal_dir_rmdir(mrb_state *mrb, const char *path);

/* Change current working directory. Returns 0 on success, -1 on error. */
int mrb_hal_dir_chdir(mrb_state *mrb, const char *path);

/* Get current working directory. Returns 0 on success, -1 on error. */
int mrb_hal_dir_getcwd(mrb_state *mrb, char *buf, size_t size);

/* Change root directory (privileged operation). Returns -1 if unsupported (sets errno to ENOSYS). */
int mrb_hal_dir_chroot(mrb_state *mrb, const char *path);

/* Check if path is a directory. Returns 1 if directory, 0 if not. */
int mrb_hal_dir_is_directory(mrb_state *mrb, const char *path);

/*
 * HAL Initialization/Finalization
 */

/* Initialize HAL (called once at gem initialization) */
void mrb_hal_dir_init(mrb_state *mrb);

/* Cleanup HAL (called once at gem finalization) */
void mrb_hal_dir_final(mrb_state *mrb);

#endif /* MRUBY_DIR_HAL_H */
