/*
** dir_hal.c - Windows HAL implementation for mruby-dir
**
** See Copyright Notice in mruby.h
**
** Windows implementation for directory operations using _findfirst/_findnext APIs.
** Provides POSIX-compatible interface on Windows.
**
** Based on dirent.c by Kevlin Henney (kevlin@acm.org, kevlin@curbralan.com)
** Original implementation: Created March 1997. Updated June 2003 and July 2012.
** See end of file for Kevlin Henney's copyright notice.
*/

#include <mruby.h>
#include "dir_hal.h"

#include <windows.h>
#include <direct.h>
#include <io.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>

/* Windows directory handle implementation */
struct mrb_dir_handle {
  intptr_t handle;          /* _findfirst/_findnext handle */
  struct _finddata_t info;  /* Current entry info */
  char *pattern;            /* Search pattern with wildcard */
  int first;                /* Flag: haven't read first entry yet */
};

/*
 * Directory Operations
 */

mrb_dir_handle*
mrb_hal_dir_open(mrb_state *mrb, const char *path)
{
  mrb_dir_handle *handle;
  size_t len = strlen(path);
  const char *suffix;

  /* Add wildcard suffix if needed */
  suffix = (len > 0 && (path[len-1] == '/' || path[len-1] == '\\')) ? "*" : "/*";

  handle = (mrb_dir_handle*)mrb_malloc(mrb, sizeof(mrb_dir_handle));
  handle->pattern = (char*)mrb_malloc(mrb, len + strlen(suffix) + 1);
  strcpy(handle->pattern, path);
  strcat(handle->pattern, suffix);

  handle->handle = _findfirst(handle->pattern, &handle->info);
  if (handle->handle == -1) {
    mrb_free(mrb, handle->pattern);
    mrb_free(mrb, handle);
    return NULL;
  }

  handle->first = 1;
  return handle;
}

int
mrb_hal_dir_close(mrb_state *mrb, mrb_dir_handle *handle)
{
  int result = -1;

  if (handle->handle != -1) {
    result = _findclose(handle->handle);
  }

  mrb_free(mrb, handle->pattern);
  mrb_free(mrb, handle);

  if (result == -1) {
    /* Map all errors to EBADF */
    errno = EBADF;
  }

  return result;
}

const char*
mrb_hal_dir_read(mrb_state *mrb, mrb_dir_handle *handle)
{
  (void)mrb;

  if (handle->handle == -1) {
    errno = EBADF;
    return NULL;
  }

  /* First call returns the result from _findfirst */
  if (handle->first) {
    handle->first = 0;
    return handle->info.name;
  }

  /* Subsequent calls use _findnext */
  if (_findnext(handle->handle, &handle->info) == -1) {
    return NULL;
  }

  return handle->info.name;
}

void
mrb_hal_dir_rewind(mrb_state *mrb, mrb_dir_handle *handle)
{
  (void)mrb;

  if (handle->handle == -1) {
    errno = EBADF;
    return;
  }

  /* Close and reopen to rewind */
  _findclose(handle->handle);
  handle->handle = _findfirst(handle->pattern, &handle->info);
  handle->first = 1;
}

/*
 * Optional Operations
 */

int
mrb_hal_dir_seek(mrb_state *mrb, mrb_dir_handle *handle, long pos)
{
  /* Not supported on Windows */
  (void)mrb; (void)handle; (void)pos;
  errno = ENOSYS;
  return -1;
}

long
mrb_hal_dir_tell(mrb_state *mrb, mrb_dir_handle *handle)
{
  /* Not supported on Windows */
  (void)mrb; (void)handle;
  errno = ENOSYS;
  return -1;
}

/*
 * Filesystem Operations
 */

int
mrb_hal_dir_mkdir(mrb_state *mrb, const char *path, int mode)
{
  /* Windows _mkdir ignores mode parameter */
  (void)mrb; (void)mode;
  return _mkdir(path);
}

int
mrb_hal_dir_rmdir(mrb_state *mrb, const char *path)
{
  (void)mrb;
  return _rmdir(path);
}

int
mrb_hal_dir_chdir(mrb_state *mrb, const char *path)
{
  (void)mrb;
  return _chdir(path);
}

int
mrb_hal_dir_getcwd(mrb_state *mrb, char *buf, size_t size)
{
  (void)mrb;
  return _getcwd(buf, (int)size) ? 0 : -1;
}

int
mrb_hal_dir_chroot(mrb_state *mrb, const char *path)
{
  /* Not available on Windows */
  (void)mrb; (void)path;
  errno = ENOSYS;
  return -1;
}

int
mrb_hal_dir_is_directory(mrb_state *mrb, const char *path)
{
  struct _stat sb;
  (void)mrb;

  if (_stat(path, &sb) == 0 && (sb.st_mode & _S_IFDIR)) {
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
  /* No initialization needed for Windows */
}

void
mrb_hal_dir_final(mrb_state *mrb)
{
  (void)mrb;
  /* No cleanup needed for Windows */
}

/*
 * Gem initialization
 */

void
mrb_hal_win_dir_gem_init(mrb_state *mrb)
{
  (void)mrb;
  /* HAL interface functions are called by mruby-dir gem */
}

void
mrb_hal_win_dir_gem_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup handled by mrb_hal_dir_final called from mruby-dir */
}

/*
** Portions derived from dirent.c by Kevlin Henney:
**
** Copyright Kevlin Henney, 1997, 2003, 2012. All rights reserved.
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose is hereby granted without fee, provided
** that this copyright and permissions notice appear in all copies and
** derivatives.
**
** This software is supplied "as is" without express or implied warranty.
**
** But that said, if there are any problems please get in touch.
*/
