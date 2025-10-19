/*
** io_hal.c - Windows HAL implementation for mruby-io
**
** See Copyright Notice in mruby.h
**
** Windows implementation for I/O operations using Win32 APIs.
** Supported platforms: Windows, MinGW
*/

#include <mruby.h>
#include "io_hal.h"

#include <windows.h>
#include <io.h>
#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <direct.h>
#include <stdint.h>

/* Maximum path length */
#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

/*
 * Helper Functions
 */

/* Convert Windows struct _stat64 to mrb_io_stat */
static void
convert_stat(const struct _stat64 *src, mrb_io_stat *dst)
{
  dst->st_dev = (uint64_t)src->st_dev;
  dst->st_ino = (uint64_t)src->st_ino;
  dst->st_mode = (uint32_t)src->st_mode;
  dst->st_nlink = (uint32_t)src->st_nlink;
  dst->st_uid = 0;  /* Windows doesn't have Unix-style UIDs */
  dst->st_gid = 0;  /* Windows doesn't have Unix-style GIDs */
  dst->st_rdev = (uint64_t)src->st_rdev;
  dst->st_size = (int64_t)src->st_size;
  dst->st_atime = (int64_t)src->st_atime;
  dst->st_mtime = (int64_t)src->st_mtime;
  dst->st_ctime = (int64_t)src->st_ctime;
  dst->st_blksize = 512;
  dst->st_blocks = (dst->st_size + 511) / 512;
}

/* Convert errno to Windows errno */
static void
set_errno_from_win_error(DWORD error)
{
  switch (error) {
    case ERROR_FILE_NOT_FOUND:
    case ERROR_PATH_NOT_FOUND:
      errno = ENOENT;
      break;
    case ERROR_ACCESS_DENIED:
      errno = EACCES;
      break;
    case ERROR_NOT_ENOUGH_MEMORY:
      errno = ENOMEM;
      break;
    case ERROR_INVALID_HANDLE:
      errno = EBADF;
      break;
    case ERROR_ALREADY_EXISTS:
      errno = EEXIST;
      break;
    default:
      errno = EIO;
      break;
  }
}

/*
 * File Operations
 */

int
mrb_hal_io_stat(mrb_state *mrb, const char *path, mrb_io_stat *st)
{
  struct _stat64 s;
  (void)mrb;

  if (_stat64(path, &s) == -1) {
    return -1;
  }
  convert_stat(&s, st);
  return 0;
}

int
mrb_hal_io_fstat(mrb_state *mrb, int fd, mrb_io_stat *st)
{
  struct _stat64 s;
  (void)mrb;

  if (_fstat64(fd, &s) == -1) {
    return -1;
  }
  convert_stat(&s, st);
  return 0;
}

int
mrb_hal_io_lstat(mrb_state *mrb, const char *path, mrb_io_stat *st)
{
  /* Windows doesn't distinguish lstat from stat */
  return mrb_hal_io_stat(mrb, path, st);
}

int
mrb_hal_io_chmod(mrb_state *mrb, const char *path, uint32_t mode)
{
  (void)mrb;
  return _chmod(path, (int)mode);
}

uint32_t
mrb_hal_io_umask(mrb_state *mrb, int32_t mask)
{
  int old;
  (void)mrb;

  if (mask < 0) {
    /* Just query current value */
    old = _umask(0);
    _umask(old);
  }
  else {
    old = _umask((int)mask);
  }
  return (uint32_t)old;
}

int
mrb_hal_io_ftruncate(mrb_state *mrb, int fd, int64_t length)
{
  (void)mrb;
  return _chsize_s(fd, length);
}

int
mrb_hal_io_flock(mrb_state *mrb, int fd, int operation)
{
  HANDLE h;
  OVERLAPPED overlapped;
  DWORD flags = 0;
  (void)mrb;

  h = (HANDLE)_get_osfhandle(fd);
  if (h == INVALID_HANDLE_VALUE) {
    errno = EBADF;
    return -1;
  }

  memset(&overlapped, 0, sizeof(overlapped));

  if (operation & MRB_IO_LOCK_UN) {
    if (!UnlockFileEx(h, 0, MAXDWORD, MAXDWORD, &overlapped)) {
      set_errno_from_win_error(GetLastError());
      return -1;
    }
    return 0;
  }

  if (operation & MRB_IO_LOCK_EX) {
    flags |= LOCKFILE_EXCLUSIVE_LOCK;
  }
  if (operation & MRB_IO_LOCK_NB) {
    flags |= LOCKFILE_FAIL_IMMEDIATELY;
  }

  if (!LockFileEx(h, flags, 0, MAXDWORD, MAXDWORD, &overlapped)) {
    set_errno_from_win_error(GetLastError());
    return -1;
  }

  return 0;
}

int
mrb_hal_io_unlink(mrb_state *mrb, const char *path)
{
  (void)mrb;
  return _unlink(path);
}

int
mrb_hal_io_rename(mrb_state *mrb, const char *oldpath, const char *newpath)
{
  (void)mrb;
  return rename(oldpath, newpath);
}

int
mrb_hal_io_symlink(mrb_state *mrb, const char *target, const char *linkpath)
{
  (void)target;
  (void)linkpath;
  /* Symlinks require special privileges on Windows */
  mrb_raise(mrb, E_NOTIMP_ERROR, "symlink is not supported on Windows");
  return -1;  /* not reached */
}

int64_t
mrb_hal_io_readlink(mrb_state *mrb, const char *path, char *buf, size_t bufsize)
{
  (void)path;
  (void)buf;
  (void)bufsize;
  /* Symlinks require special handling on Windows */
  mrb_raise(mrb, E_NOTIMP_ERROR, "readlink is not supported on Windows");
  return -1;  /* not reached */
}

char*
mrb_hal_io_realpath(mrb_state *mrb, const char *path, char *resolved)
{
  DWORD ret;
  (void)mrb;

  ret = GetFullPathName(path, PATH_MAX, resolved, NULL);
  if (ret == 0 || ret >= PATH_MAX) {
    set_errno_from_win_error(GetLastError());
    return NULL;
  }
  return resolved;
}

char*
mrb_hal_io_getcwd(mrb_state *mrb, char *buf, size_t size)
{
  (void)mrb;
  return _getcwd(buf, (int)size);
}

const char*
mrb_hal_io_getenv(mrb_state *mrb, const char *name)
{
  (void)mrb;
  return getenv(name);
}

const char*
mrb_hal_io_gethome(mrb_state *mrb, const char *username)
{
  const char *home;
  (void)mrb;

  if (username != NULL && *username != '\0') {
    /* Windows doesn't have a simple way to get other users' home directories */
    errno = ENOSYS;
    return NULL;
  }

  /* Try USERPROFILE first, then HOMEDRIVE+HOMEPATH */
  home = getenv("USERPROFILE");
  if (home == NULL) {
    const char *homedrive = getenv("HOMEDRIVE");
    const char *homepath = getenv("HOMEPATH");
    if (homedrive && homepath) {
      static char homebuf[PATH_MAX];
      snprintf(homebuf, PATH_MAX, "%s%s", homedrive, homepath);
      return homebuf;
    }
    errno = ENOENT;
    return NULL;
  }

  return home;
}

/*
 * Core I/O Operations
 */

int
mrb_hal_io_open(mrb_state *mrb, const char *path, int flags, uint32_t mode)
{
  int fd;
  (void)mrb;

  /* Windows uses _open with slightly different flags */
  fd = _open(path, flags | _O_BINARY, (int)mode);
  if (fd == -1) {
    return -1;
  }

  /* Set close-on-exec for non-standard descriptors */
  if (fd > 2) {
    SetHandleInformation((HANDLE)_get_osfhandle(fd), HANDLE_FLAG_INHERIT, 0);
  }

  return fd;
}

int
mrb_hal_io_close(mrb_state *mrb, int fd)
{
  (void)mrb;
  return _close(fd);
}

int64_t
mrb_hal_io_read(mrb_state *mrb, int fd, void *buf, size_t count)
{
  int n;
  (void)mrb;

  n = _read(fd, buf, (unsigned int)count);
  return (int64_t)n;
}

int64_t
mrb_hal_io_write(mrb_state *mrb, int fd, const void *buf, size_t count)
{
  int n;
  (void)mrb;

  n = _write(fd, buf, (unsigned int)count);
  return (int64_t)n;
}

int64_t
mrb_hal_io_lseek(mrb_state *mrb, int fd, int64_t offset, int whence)
{
  __int64 pos;
  int win_whence;
  (void)mrb;

  /* Convert MRB_IO_SEEK_* to Windows SEEK_* */
  switch (whence) {
    case MRB_IO_SEEK_SET: win_whence = SEEK_SET; break;
    case MRB_IO_SEEK_CUR: win_whence = SEEK_CUR; break;
    case MRB_IO_SEEK_END: win_whence = SEEK_END; break;
    default:
      errno = EINVAL;
      return -1;
  }

  pos = _lseeki64(fd, (__int64)offset, win_whence);
  return (int64_t)pos;
}

int
mrb_hal_io_dup(mrb_state *mrb, int fd)
{
  int new_fd;
  (void)mrb;

  new_fd = _dup(fd);
  if (new_fd == -1) {
    return -1;
  }

  /* Set close-on-exec */
  if (new_fd > 2) {
    SetHandleInformation((HANDLE)_get_osfhandle(new_fd), HANDLE_FLAG_INHERIT, 0);
  }

  return new_fd;
}

int
mrb_hal_io_fcntl(mrb_state *mrb, int fd, int cmd, int arg)
{
  /* Windows has limited fcntl support */
  (void)mrb;
  (void)fd;
  (void)cmd;
  (void)arg;
  errno = ENOSYS;
  return -1;
}

int
mrb_hal_io_isatty(mrb_state *mrb, int fd)
{
  (void)mrb;
  return _isatty(fd) ? 1 : 0;
}

int
mrb_hal_io_pipe(mrb_state *mrb, int fds[2])
{
  int ret;
  (void)mrb;

  ret = _pipe(fds, 4096, _O_BINARY);
  if (ret == -1) {
    return -1;
  }

  /* Set close-on-exec on both ends */
  SetHandleInformation((HANDLE)_get_osfhandle(fds[0]), HANDLE_FLAG_INHERIT, 0);
  SetHandleInformation((HANDLE)_get_osfhandle(fds[1]), HANDLE_FLAG_INHERIT, 0);

  return 0;
}

/*
 * Process Operations
 */

int
mrb_hal_io_spawn_process(mrb_state *mrb, const char *cmd,
                          int stdin_fd, int stdout_fd, int stderr_fd,
                          int *pid)
{
  STARTUPINFO si;
  PROCESS_INFORMATION pi;
  HANDLE h_stdin = INVALID_HANDLE_VALUE;
  HANDLE h_stdout = INVALID_HANDLE_VALUE;
  HANDLE h_stderr = INVALID_HANDLE_VALUE;
  char cmdline[8192];
  BOOL ret;
  (void)mrb;

  /* Skip leading whitespace */
  while (*cmd == ' ' || *cmd == '\t' || *cmd == '\n') {
    cmd++;
  }

  if (!*cmd) {
    errno = ENOENT;
    return -1;
  }

  /* Build command line - use cmd.exe to execute */
  snprintf(cmdline, sizeof(cmdline), "cmd.exe /c %s", cmd);

  /* Setup startup info */
  memset(&si, 0, sizeof(si));
  si.cb = sizeof(si);
  si.dwFlags = STARTF_USESTDHANDLES;

  /* Convert file descriptors to handles and make them inheritable */
  if (stdin_fd != -1) {
    h_stdin = (HANDLE)_get_osfhandle(stdin_fd);
    SetHandleInformation(h_stdin, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    si.hStdInput = h_stdin;
  }
  else {
    si.hStdInput = GetStdHandle(STD_INPUT_HANDLE);
  }

  if (stdout_fd != -1) {
    h_stdout = (HANDLE)_get_osfhandle(stdout_fd);
    SetHandleInformation(h_stdout, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    si.hStdOutput = h_stdout;
  }
  else {
    si.hStdOutput = GetStdHandle(STD_OUTPUT_HANDLE);
  }

  if (stderr_fd != -1) {
    h_stderr = (HANDLE)_get_osfhandle(stderr_fd);
    SetHandleInformation(h_stderr, HANDLE_FLAG_INHERIT, HANDLE_FLAG_INHERIT);
    si.hStdError = h_stderr;
  }
  else {
    si.hStdError = GetStdHandle(STD_ERROR_HANDLE);
  }

  /* Create process */
  memset(&pi, 0, sizeof(pi));
  ret = CreateProcess(NULL, cmdline, NULL, NULL, TRUE, 0, NULL, NULL, &si, &pi);

  if (!ret) {
    set_errno_from_win_error(GetLastError());
    return -1;
  }

  /* Close thread handle - we don't need it */
  CloseHandle(pi.hThread);

  /* Store process handle as PID (will be used in waitpid) */
  *pid = (int)(intptr_t)pi.hProcess;

  return 0;
}

int
mrb_hal_io_waitpid(mrb_state *mrb, int pid, int *status, int options)
{
  HANDLE h = (HANDLE)(intptr_t)pid;
  DWORD wait_result;
  DWORD exit_code;
  DWORD timeout;
  (void)mrb;

  /* Convert options to timeout */
  timeout = (options != 0) ? 0 : INFINITE;

  wait_result = WaitForSingleObject(h, timeout);

  if (wait_result == WAIT_TIMEOUT) {
    return 0;  /* Non-blocking wait, no change */
  }

  if (wait_result != WAIT_OBJECT_0) {
    set_errno_from_win_error(GetLastError());
    return -1;
  }

  /* Get exit code */
  if (!GetExitCodeProcess(h, &exit_code)) {
    set_errno_from_win_error(GetLastError());
    return -1;
  }

  if (status != NULL) {
    /* Store exit code in status (shifted to match Unix convention) */
    *status = (int)(exit_code << 8);
  }

  /* Close process handle */
  CloseHandle(h);

  return pid;
}

/*
 * I/O Multiplexing
 */

struct mrb_io_fdset {
  fd_set fds;
};

mrb_io_fdset*
mrb_hal_io_fdset_alloc(mrb_state *mrb)
{
  mrb_io_fdset *fdset = (mrb_io_fdset*)mrb_malloc(mrb, sizeof(mrb_io_fdset));
  FD_ZERO(&fdset->fds);
  return fdset;
}

void
mrb_hal_io_fdset_free(mrb_state *mrb, mrb_io_fdset *fdset)
{
  if (fdset) {
    mrb_free(mrb, fdset);
  }
}

void
mrb_hal_io_fdset_zero(mrb_state *mrb, mrb_io_fdset *fdset)
{
  (void)mrb;
  if (fdset) {
    FD_ZERO(&fdset->fds);
  }
}

void
mrb_hal_io_fdset_set(mrb_state *mrb, int fd, mrb_io_fdset *fdset)
{
  (void)mrb;
  if (fdset) {
    FD_SET(fd, &fdset->fds);
  }
}

int
mrb_hal_io_fdset_isset(mrb_state *mrb, int fd, mrb_io_fdset *fdset)
{
  (void)mrb;
  if (fdset) {
    return FD_ISSET(fd, &fdset->fds);
  }
  return 0;
}

int
mrb_hal_io_select(mrb_state *mrb, int nfds,
                   mrb_io_fdset *readfds,
                   mrb_io_fdset *writefds,
                   mrb_io_fdset *errorfds,
                   mrb_io_timeval *timeout)
{
  fd_set *r = readfds ? &readfds->fds : NULL;
  fd_set *w = writefds ? &writefds->fds : NULL;
  fd_set *e = errorfds ? &errorfds->fds : NULL;
  struct timeval *tv = NULL;
  struct timeval tv_storage;
  (void)mrb;
  (void)nfds;  /* Windows select() doesn't use nfds */

  if (timeout) {
    tv_storage.tv_sec = (long)timeout->tv_sec;
    tv_storage.tv_usec = (long)timeout->tv_usec;
    tv = &tv_storage;
  }

  return select(0, r, w, e, tv);
}

/*
 * HAL Initialization/Finalization
 */

void
mrb_hal_io_init(mrb_state *mrb)
{
  (void)mrb;
  /* Initialize Winsock for select() support */
  WSADATA wsaData;
  WSAStartup(MAKEWORD(2, 2), &wsaData);
}

void
mrb_hal_io_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup Winsock */
  WSACleanup();
}

/*
 * Gem initialization
 */

void
mrb_hal_win_io_gem_init(mrb_state *mrb)
{
  (void)mrb;
  /* HAL interface functions are called by mruby-io gem */
}

void
mrb_hal_win_io_gem_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup handled by mrb_hal_io_final called from mruby-io */
}
