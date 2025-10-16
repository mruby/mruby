/*
** dir_hal.c - POSIX HAL implementation for mruby-dir
**
** See Copyright Notice in mruby.h
**
** POSIX implementation for directory operations using standard POSIX APIs.
** Supported platforms: Linux, macOS, BSD, Unix
*/

#include <mruby.h>
#include "dir_hal.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>
#include <unistd.h>
#include <errno.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif

/* On POSIX, mrb_dir_handle wraps DIR */
struct mrb_dir_handle {
  DIR *dir;
};

/*
 * Directory Operations
 */

mrb_dir_handle*
mrb_hal_dir_open(mrb_state *mrb, const char *path)
{
  DIR *dir = opendir(path);
  if (dir == NULL) {
    return NULL;
  }

  mrb_dir_handle *handle = (mrb_dir_handle*)mrb_malloc(mrb, sizeof(mrb_dir_handle));
  handle->dir = dir;
  return handle;
}

int
mrb_hal_dir_close(mrb_state *mrb, mrb_dir_handle *handle)
{
  int result = closedir(handle->dir);
  mrb_free(mrb, handle);
  return result;
}

const char*
mrb_hal_dir_read(mrb_state *mrb, mrb_dir_handle *handle)
{
  (void)mrb;
  struct dirent *dp = readdir(handle->dir);
  return dp ? dp->d_name : NULL;
}

void
mrb_hal_dir_rewind(mrb_state *mrb, mrb_dir_handle *handle)
{
  (void)mrb;
  rewinddir(handle->dir);
}

/*
 * Optional Operations
 */

int
mrb_hal_dir_seek(mrb_state *mrb, mrb_dir_handle *handle, long pos)
{
#if defined(__ANDROID__)
  /* Android doesn't have reliable seekdir */
  (void)mrb; (void)handle; (void)pos;
  errno = ENOSYS;
  return -1;
#else
  (void)mrb;
  seekdir(handle->dir, pos);
  return 0;
#endif
}

long
mrb_hal_dir_tell(mrb_state *mrb, mrb_dir_handle *handle)
{
#if defined(__ANDROID__)
  /* Android doesn't have reliable telldir */
  (void)mrb; (void)handle;
  errno = ENOSYS;
  return -1;
#else
  (void)mrb;
  return telldir(handle->dir);
#endif
}

/*
 * Filesystem Operations
 */

int
mrb_hal_dir_mkdir(mrb_state *mrb, const char *path, int mode)
{
  (void)mrb;
  return mkdir(path, (mode_t)mode);
}

int
mrb_hal_dir_rmdir(mrb_state *mrb, const char *path)
{
  (void)mrb;
  return rmdir(path);
}

int
mrb_hal_dir_chdir(mrb_state *mrb, const char *path)
{
  (void)mrb;
  return chdir(path);
}

int
mrb_hal_dir_getcwd(mrb_state *mrb, char *buf, size_t size)
{
  (void)mrb;
  return getcwd(buf, size) ? 0 : -1;
}

int
mrb_hal_dir_chroot(mrb_state *mrb, const char *path)
{
#if defined(__ANDROID__) || defined(__MSDOS__)
  /* Not available on these platforms */
  (void)mrb; (void)path;
  errno = ENOSYS;
  return -1;
#else
  (void)mrb;
  return chroot(path);
#endif
}

int
mrb_hal_dir_is_directory(mrb_state *mrb, const char *path)
{
  struct stat sb;
  (void)mrb;

  if (stat(path, &sb) == 0 && S_ISDIR(sb.st_mode)) {
    return 1;
  }
  return 0;
}

/*
 * HAL Initialization/Finalization
 */

void
mrb_hal_dir_init(mrb_state *mrb)
{
  (void)mrb;
  /* No initialization needed for POSIX */
}

void
mrb_hal_dir_final(mrb_state *mrb)
{
  (void)mrb;
  /* No cleanup needed for POSIX */
}

/*
 * Gem initialization
 */

void
mrb_hal_posix_dir_gem_init(mrb_state *mrb)
{
  (void)mrb;
  /* HAL interface functions are called by mruby-dir gem */
}

void
mrb_hal_posix_dir_gem_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup handled by mrb_hal_dir_final called from mruby-dir */
}
