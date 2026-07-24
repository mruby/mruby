/*
** dir_hal.c - Windows HAL implementation for mruby-dir
**
** See Copyright Notice in mruby.h
**
** Windows implementation for directory operations using _wfindfirst/_wfindnext APIs.
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
#include <wchar.h>

/* Windows directory handle implementation */
struct mrb_dir_handle {
  intptr_t handle;           /* _wfindfirst/_wfindnext handle */
  struct _wfinddata_t info;  /* Current entry info */
  wchar_t *pattern;          /* UTF-16 search pattern stored after this structure */
  char *name;                /* Current entry name encoded as UTF-8 */
  size_t name_capacity;      /* Allocated size of name */
  int first;                 /* Flag: haven't read first entry yet */
};

static wchar_t*
utf8_to_utf16(mrb_state *mrb, const char *utf8)
{
  int len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, utf8, -1, NULL, 0);
  wchar_t *utf16;

  if (len == 0) {
    errno = EINVAL;
    return NULL;
  }
  utf16 = (wchar_t*)mrb_malloc(mrb, (size_t)len * sizeof(wchar_t));
  if (MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, utf8, -1, utf16, len) == 0) {
    mrb_free(mrb, utf16);
    errno = EINVAL;
    return NULL;
  }
  return utf16;
}

static int
utf16_to_utf8(mrb_state *mrb, const wchar_t *utf16, char **buf, size_t *capacity)
{
  int len = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, utf16, -1, NULL, 0, NULL, NULL);

  if (len == 0) {
    errno = EILSEQ;
    return -1;
  }
  if ((size_t)len > *capacity) {
    char *newbuf = (char*)mrb_realloc(mrb, *buf, (size_t)len);
    *buf = newbuf;
    *capacity = (size_t)len;
  }
  if (WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, utf16, -1, *buf, len, NULL, NULL) == 0) {
    errno = EILSEQ;
    return -1;
  }
  return 0;
}

/*
 * Directory Operations
 */

mrb_dir_handle*
mrb_hal_dir_open(mrb_state *mrb, const char *path)
{
  mrb_dir_handle *handle;
  const wchar_t *suffix;
  int utf16_len = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1, NULL, 0);
  size_t len;

  if (utf16_len == 0) {
    errno = EINVAL;
    return NULL;
  }

  // Reserve enough trailing space for either "*" or "/*".
  handle = (mrb_dir_handle*)mrb_malloc(mrb,
    sizeof(mrb_dir_handle) + ((size_t)utf16_len + 2) * sizeof(wchar_t));
  handle->pattern = (wchar_t*)(handle + 1);
  handle->name = NULL;
  handle->name_capacity = 0;

  if (MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, path, -1,
                          handle->pattern, utf16_len) == 0) {
    mrb_free(mrb, handle);
    errno = EINVAL;
    return NULL;
  }
  len = (size_t)utf16_len - 1;

  /* Add wildcard suffix if needed */
  suffix = (len > 0 && (handle->pattern[len-1] == L'/' || handle->pattern[len-1] == L'\\')) ? L"*" : L"/*";
  wcscat(handle->pattern, suffix);

  handle->handle = _wfindfirst(handle->pattern, &handle->info);
  if (handle->handle == -1) {
    int saved_errno = errno;
    mrb_free(mrb, handle);
    errno = saved_errno;
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

  mrb_free(mrb, handle->name);
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
  const wchar_t *name;

  if (handle->handle == -1) {
    errno = EBADF;
    return NULL;
  }

  /* First call returns the result from _wfindfirst */
  if (handle->first) {
    handle->first = 0;
    name = handle->info.name;
  }
  else {
    /* Subsequent calls use _wfindnext */
    if (_wfindnext(handle->handle, &handle->info) == -1) {
      return NULL;
    }
    name = handle->info.name;
  }

  if (utf16_to_utf8(mrb, name, &handle->name, &handle->name_capacity) == -1) {
    return NULL;
  }
  return handle->name;
}

void
mrb_hal_dir_rewind(mrb_state *mrb, mrb_dir_handle *handle)
{
  if (handle->handle == -1) {
    errno = EBADF;
    return;
  }

  /* Close and reopen to rewind */
  _findclose(handle->handle);
  handle->handle = _wfindfirst(handle->pattern, &handle->info);
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
  wchar_t *utf16 = utf8_to_utf16(mrb, path);
  int result;
  int saved_errno;

  /* Windows _mkdir ignores mode parameter */
  (void)mode;
  if (utf16 == NULL) return -1;
  result = _wmkdir(utf16);
  saved_errno = errno;
  mrb_free(mrb, utf16);
  if (result == -1) errno = saved_errno;
  return result;
}

int
mrb_hal_dir_rmdir(mrb_state *mrb, const char *path)
{
  wchar_t *utf16 = utf8_to_utf16(mrb, path);
  int result;
  int saved_errno;

  if (utf16 == NULL) return -1;
  result = _wrmdir(utf16);
  saved_errno = errno;
  mrb_free(mrb, utf16);
  if (result == -1) errno = saved_errno;
  return result;
}

int
mrb_hal_dir_chdir(mrb_state *mrb, const char *path)
{
  wchar_t *utf16 = utf8_to_utf16(mrb, path);
  int result;
  int saved_errno;

  if (utf16 == NULL) return -1;
  result = _wchdir(utf16);
  saved_errno = errno;
  mrb_free(mrb, utf16);
  if (result == -1) errno = saved_errno;
  return result;
}

int
mrb_hal_dir_getcwd(mrb_state *mrb, char *buf, size_t size)
{
  wchar_t *utf16 = (wchar_t*)mrb_malloc(mrb, size * sizeof(wchar_t));
  int len;

  if (_wgetcwd(utf16, (int)size) == NULL) {
    int saved_errno = errno;
    mrb_free(mrb, utf16);
    errno = saved_errno;
    return -1;
  }
  len = WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, utf16, -1, NULL, 0, NULL, NULL);
  if (len == 0) {
    mrb_free(mrb, utf16);
    errno = EILSEQ;
    return -1;
  }
  if ((size_t)len > size) {
    mrb_free(mrb, utf16);
    errno = ERANGE;
    return -1;
  }
  if (WideCharToMultiByte(CP_UTF8, WC_ERR_INVALID_CHARS, utf16, -1,
                          buf, len, NULL, NULL) == 0) {
    mrb_free(mrb, utf16);
    errno = EILSEQ;
    return -1;
  }
  mrb_free(mrb, utf16);
  return 0;
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
  wchar_t *utf16 = utf8_to_utf16(mrb, path);
  int result;
  int saved_errno;

  if (utf16 == NULL) return 0;

  result = _wstat(utf16, &sb);
  saved_errno = errno;
  mrb_free(mrb, utf16);
  if (result == -1) {
    errno = saved_errno;
    return 0;
  }
  return (sb.st_mode & _S_IFDIR) != 0;
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
