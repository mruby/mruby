/*
** io_hal.c - POSIX HAL implementation for mruby-io
**
** See Copyright Notice in mruby.h
**
** POSIX implementation for I/O operations using standard POSIX APIs.
** Supported platforms: Linux, macOS, BSD, Unix
*/

#include <mruby.h>
#include "io_hal.h"

#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <sys/file.h>
#include <sys/param.h>

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <pwd.h>

#ifndef __DJGPP__
#include <libgen.h>
#endif

/* Maximum path length */
#ifndef PATH_MAX
#define PATH_MAX 4096
#endif

/*
 * Helper Functions
 */

/* Convert POSIX struct stat to mrb_io_stat */
static void
convert_stat(const struct stat *src, mrb_io_stat *dst)
{
  /* Save time values to avoid macro expansion issues */
  time_t atime = src->st_atime;
  time_t mtime = src->st_mtime;
  time_t ctime = src->st_ctime;

  dst->st_dev = (uint64_t)src->st_dev;
  dst->st_ino = (uint64_t)src->st_ino;
  dst->st_mode = (uint32_t)src->st_mode;
  dst->st_nlink = (uint32_t)src->st_nlink;
  dst->st_uid = (uint32_t)src->st_uid;
  dst->st_gid = (uint32_t)src->st_gid;
  dst->st_rdev = (uint64_t)src->st_rdev;
  dst->st_size = (int64_t)src->st_size;

  /* Assign time values after undefinining macros */
#undef st_atime
#undef st_mtime
#undef st_ctime
  dst->st_atime = (int64_t)atime;
  dst->st_mtime = (int64_t)mtime;
  dst->st_ctime = (int64_t)ctime;

#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
  dst->st_blksize = (int64_t)src->st_blksize;
#else
  dst->st_blksize = 512;
#endif
#ifdef HAVE_STRUCT_STAT_ST_BLOCKS
  dst->st_blocks = (int64_t)src->st_blocks;
#else
  dst->st_blocks = (dst->st_size + 511) / 512;
#endif
}

/*
 * File Operations
 */

int
mrb_hal_io_stat(mrb_state *mrb, const char *path, mrb_io_stat *st)
{
  struct stat s;
  (void)mrb;

  if (stat(path, &s) == -1) {
    return -1;
  }
  convert_stat(&s, st);
  return 0;
}

int
mrb_hal_io_fstat(mrb_state *mrb, int fd, mrb_io_stat *st)
{
  struct stat s;
  (void)mrb;

  if (fstat(fd, &s) == -1) {
    return -1;
  }
  convert_stat(&s, st);
  return 0;
}

int
mrb_hal_io_lstat(mrb_state *mrb, const char *path, mrb_io_stat *st)
{
  struct stat s;
  (void)mrb;

  if (lstat(path, &s) == -1) {
    return -1;
  }
  convert_stat(&s, st);
  return 0;
}

int
mrb_hal_io_chmod(mrb_state *mrb, const char *path, uint32_t mode)
{
  (void)mrb;
  return chmod(path, (mode_t)mode);
}

uint32_t
mrb_hal_io_umask(mrb_state *mrb, int32_t mask)
{
  mode_t old;
  (void)mrb;

  if (mask < 0) {
    /* Just query current value */
    old = umask(0);
    umask(old);
  }
  else {
    old = umask((mode_t)mask);
  }
  return (uint32_t)old;
}

int
mrb_hal_io_ftruncate(mrb_state *mrb, int fd, int64_t length)
{
  (void)mrb;
  return ftruncate(fd, (off_t)length);
}

int
mrb_hal_io_flock(mrb_state *mrb, int fd, int operation)
{
  (void)mrb;

  while (flock(fd, operation) == -1) {
    if (errno == EINTR) {
      continue;  /* Retry on interrupt */
    }
    return -1;
  }
  return 0;
}

int
mrb_hal_io_unlink(mrb_state *mrb, const char *path)
{
  (void)mrb;
  return unlink(path);
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
  (void)mrb;
  return symlink(target, linkpath);
}

int64_t
mrb_hal_io_readlink(mrb_state *mrb, const char *path, char *buf, size_t bufsize)
{
  ssize_t rc;
  (void)mrb;

  rc = readlink(path, buf, bufsize);
  return (int64_t)rc;
}

char*
mrb_hal_io_realpath(mrb_state *mrb, const char *path, char *resolved)
{
  (void)mrb;
  return realpath(path, resolved);
}

char*
mrb_hal_io_getcwd(mrb_state *mrb, char *buf, size_t size)
{
  (void)mrb;
  return getcwd(buf, size);
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

  if (username == NULL || *username == '\0') {
    /* Get current user's home */
    home = getenv("HOME");
    if (home == NULL) {
      errno = ENOENT;
      return NULL;
    }
  }
  else {
    /* Get specified user's home */
    struct passwd *pwd = getpwnam(username);
    if (pwd == NULL) {
      errno = ENOENT;
      return NULL;
    }
    home = pwd->pw_dir;
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

  fd = open(path, flags, (mode_t)mode);
  if (fd == -1) {
    return -1;
  }

  /* Set close-on-exec for non-standard descriptors */
#if defined(F_GETFD) && defined(F_SETFD) && defined(FD_CLOEXEC)
  if (fd > 2) {
    int fd_flags = fcntl(fd, F_GETFD);
    if (fd_flags != -1) {
      fcntl(fd, F_SETFD, fd_flags | FD_CLOEXEC);
    }
  }
#endif

  return fd;
}

int
mrb_hal_io_close(mrb_state *mrb, int fd)
{
  (void)mrb;
  return close(fd);
}

int64_t
mrb_hal_io_read(mrb_state *mrb, int fd, void *buf, size_t count)
{
  ssize_t n;
  (void)mrb;

  n = read(fd, buf, count);
  return (int64_t)n;
}

int64_t
mrb_hal_io_write(mrb_state *mrb, int fd, const void *buf, size_t count)
{
  ssize_t n;
  (void)mrb;

  n = write(fd, buf, count);
  return (int64_t)n;
}

int64_t
mrb_hal_io_lseek(mrb_state *mrb, int fd, int64_t offset, int whence)
{
  off_t pos;
  int posix_whence;
  (void)mrb;

  /* Convert MRB_IO_SEEK_* to POSIX SEEK_* */
  switch (whence) {
    case MRB_IO_SEEK_SET: posix_whence = SEEK_SET; break;
    case MRB_IO_SEEK_CUR: posix_whence = SEEK_CUR; break;
    case MRB_IO_SEEK_END: posix_whence = SEEK_END; break;
    default:
      errno = EINVAL;
      return -1;
  }

  pos = lseek(fd, (off_t)offset, posix_whence);
  return (int64_t)pos;
}

int
mrb_hal_io_dup(mrb_state *mrb, int fd)
{
  int new_fd;
  (void)mrb;

  new_fd = dup(fd);
  if (new_fd == -1) {
    return -1;
  }

  /* Set close-on-exec */
#if defined(F_GETFD) && defined(F_SETFD) && defined(FD_CLOEXEC)
  if (new_fd > 2) {
    int fd_flags = fcntl(new_fd, F_GETFD);
    if (fd_flags != -1) {
      fcntl(new_fd, F_SETFD, fd_flags | FD_CLOEXEC);
    }
  }
#endif

  return new_fd;
}

int
mrb_hal_io_fcntl(mrb_state *mrb, int fd, int cmd, int arg)
{
  (void)mrb;
  return fcntl(fd, cmd, arg);
}

int
mrb_hal_io_isatty(mrb_state *mrb, int fd)
{
  (void)mrb;
  return isatty(fd) ? 1 : 0;
}

int
mrb_hal_io_pipe(mrb_state *mrb, int fds[2])
{
  int ret;
  (void)mrb;

  ret = pipe(fds);
  if (ret == -1) {
    return -1;
  }

  /* Set close-on-exec on both ends */
#if defined(F_GETFD) && defined(F_SETFD) && defined(FD_CLOEXEC)
  for (int i = 0; i < 2; i++) {
    int fd_flags = fcntl(fds[i], F_GETFD);
    if (fd_flags != -1) {
      fcntl(fds[i], F_SETFD, fd_flags | FD_CLOEXEC);
    }
  }
#endif

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
  pid_t child_pid;
  (void)mrb;

  /* Skip leading whitespace */
  while (*cmd == ' ' || *cmd == '\t' || *cmd == '\n') {
    cmd++;
  }

  if (!*cmd) {
    errno = ENOENT;
    return -1;
  }

  child_pid = fork();
  if (child_pid == -1) {
    /* Fork failed */
    return -1;
  }

  if (child_pid == 0) {
    /* Child process */

    /* Redirect stdin */
    if (stdin_fd != -1) {
      dup2(stdin_fd, STDIN_FILENO);
      if (stdin_fd > 2) close(stdin_fd);
    }

    /* Redirect stdout */
    if (stdout_fd != -1) {
      dup2(stdout_fd, STDOUT_FILENO);
      if (stdout_fd > 2) close(stdout_fd);
    }

    /* Redirect stderr */
    if (stderr_fd != -1) {
      dup2(stderr_fd, STDERR_FILENO);
      if (stderr_fd > 2) close(stderr_fd);
    }

    /* Close all other file descriptors */
    int max_fd = sysconf(_SC_OPEN_MAX);
    if (max_fd == -1) max_fd = 1024;
    for (int i = 3; i < max_fd; i++) {
      close(i);
    }

    /* Execute command via shell */
    execl("/bin/sh", "sh", "-c", cmd, (char*)NULL);

    /* If execl returns, it failed */
    _exit(127);
  }

  /* Parent process */
  *pid = (int)child_pid;
  return 0;
}

int
mrb_hal_io_waitpid(mrb_state *mrb, int pid, int *status, int options)
{
  pid_t result;
  int stat;
  (void)mrb;

  result = waitpid((pid_t)pid, &stat, options);
  if (result == -1) {
    return -1;
  }

  if (status != NULL) {
    *status = stat;
  }

  return (int)result;
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

  if (timeout) {
    tv_storage.tv_sec = (time_t)timeout->tv_sec;
    tv_storage.tv_usec = (suseconds_t)timeout->tv_usec;
    tv = &tv_storage;
  }

  return select(nfds, r, w, e, tv);
}

/*
 * HAL Initialization/Finalization
 */

void
mrb_hal_io_init(mrb_state *mrb)
{
  (void)mrb;
  /* No special initialization needed for POSIX */
}

void
mrb_hal_io_final(mrb_state *mrb)
{
  (void)mrb;
  /* No special cleanup needed for POSIX */
}

/*
 * Gem initialization
 */

void
mrb_hal_posix_io_gem_init(mrb_state *mrb)
{
  (void)mrb;
  /* HAL interface functions are called by mruby-io gem */
}

void
mrb_hal_posix_io_gem_final(mrb_state *mrb)
{
  (void)mrb;
  /* Cleanup handled by mrb_hal_io_final called from mruby-io */
}
