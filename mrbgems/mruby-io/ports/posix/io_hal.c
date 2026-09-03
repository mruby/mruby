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

/* POSIX requires the file type constants to be usable in `#if`, so whether
   this host numbers the type as the HAL does is settled here rather than
   assumed.  Where it does, the field carries across as it is; where it does
   not, the kind is tested with the S_IS*() macros, which are what POSIX
   actually guarantees.  Neither host pays for the other. */
#if defined(S_IFMT)   && S_IFMT   == MRB_IO_S_IFMT   && \
    defined(S_IFREG)  && S_IFREG  == MRB_IO_S_IFREG  && \
    defined(S_IFDIR)  && S_IFDIR  == MRB_IO_S_IFDIR  && \
    defined(S_IFCHR)  && S_IFCHR  == MRB_IO_S_IFCHR  && \
    defined(S_IFBLK)  && S_IFBLK  == MRB_IO_S_IFBLK  && \
    defined(S_IFIFO)  && S_IFIFO  == MRB_IO_S_IFIFO  && \
    defined(S_IFLNK)  && S_IFLNK  == MRB_IO_S_IFLNK  && \
    defined(S_IFSOCK) && S_IFSOCK == MRB_IO_S_IFSOCK
# define MRB_IO_TYPE_IS_HAL_TYPE
#endif

/* The permission bits, this host's to the HAL's.  POSIX names them and
   leaves the numbers to the implementation, so each is read by name; where
   the two agree, as they do here, the compiler folds this to a mask. */
static mrb_int
perm_to_hal(mode_t m)
{
  return ((m & S_ISUID) ? MRB_IO_S_ISUID : 0) |
         ((m & S_ISGID) ? MRB_IO_S_ISGID : 0) |
         ((m & S_ISVTX) ? MRB_IO_S_ISVTX : 0) |
         ((m & S_IRUSR) ? MRB_IO_S_IRUSR : 0) |
         ((m & S_IWUSR) ? MRB_IO_S_IWUSR : 0) |
         ((m & S_IXUSR) ? MRB_IO_S_IXUSR : 0) |
         ((m & S_IRGRP) ? MRB_IO_S_IRGRP : 0) |
         ((m & S_IWGRP) ? MRB_IO_S_IWGRP : 0) |
         ((m & S_IXGRP) ? MRB_IO_S_IXGRP : 0) |
         ((m & S_IROTH) ? MRB_IO_S_IROTH : 0) |
         ((m & S_IWOTH) ? MRB_IO_S_IWOTH : 0) |
         ((m & S_IXOTH) ? MRB_IO_S_IXOTH : 0);
}

/* The same the other way, for a mode handed down from Ruby */
static mode_t
perm_from_hal(mrb_int m)
{
  return (mode_t)
         (((m & MRB_IO_S_ISUID) ? S_ISUID : 0) |
          ((m & MRB_IO_S_ISGID) ? S_ISGID : 0) |
          ((m & MRB_IO_S_ISVTX) ? S_ISVTX : 0) |
          ((m & MRB_IO_S_IRUSR) ? S_IRUSR : 0) |
          ((m & MRB_IO_S_IWUSR) ? S_IWUSR : 0) |
          ((m & MRB_IO_S_IXUSR) ? S_IXUSR : 0) |
          ((m & MRB_IO_S_IRGRP) ? S_IRGRP : 0) |
          ((m & MRB_IO_S_IWGRP) ? S_IWGRP : 0) |
          ((m & MRB_IO_S_IXGRP) ? S_IXGRP : 0) |
          ((m & MRB_IO_S_IROTH) ? S_IROTH : 0) |
          ((m & MRB_IO_S_IWOTH) ? S_IWOTH : 0) |
          ((m & MRB_IO_S_IXOTH) ? S_IXOTH : 0));
}

/* Convert POSIX struct stat to mrb_io_stat */
static void
convert_stat(const struct stat *src, mrb_io_stat *dst)
{
  /* Extract time values FIRST while macros are still defined.
   * On POSIX systems, st_atime may be a macro for st_atim.tv_sec */
  time_t atime_val, mtime_val, ctime_val;
#if defined(st_atime)
  /* st_atime is a macro - use it to extract from src */
  atime_val = src->st_atime;
  mtime_val = src->st_mtime;
  ctime_val = src->st_ctime;
#elif defined(__APPLE__) || defined(__FreeBSD__) || \
      defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFly__)
  /* BSD/macOS: st_atime is typically a direct member */
  atime_val = src->st_atime;
  mtime_val = src->st_mtime;
  ctime_val = src->st_ctime;
#else
  /* POSIX.1-2008: use st_atim.tv_sec directly */
  atime_val = src->st_atim.tv_sec;
  mtime_val = src->st_mtim.tv_sec;
  ctime_val = src->st_ctim.tv_sec;
#endif

  /* Undefine macros to avoid interference with mrb_io_stat fields */
#undef st_atime
#undef st_mtime
#undef st_ctime

  dst->st_dev = (mrb_int)src->st_dev;
  dst->st_ino = (mrb_int)src->st_ino;
  /* The kind is written in the HAL's terms.  What the gem may read of it
     is the port's declaration; where the kinds are mapped one by one, a
     kind the port does not declare is not written either. */
  dst->st_mode = perm_to_hal(src->st_mode);
#ifdef MRB_IO_TYPE_IS_HAL_TYPE
  dst->st_mode |= (mrb_int)(src->st_mode & S_IFMT);
#else
  if (S_ISREG(src->st_mode)) {
    dst->st_mode |= MRB_IO_S_IFREG;
  }
  else if (S_ISDIR(src->st_mode)) {
    dst->st_mode |= MRB_IO_S_IFDIR;
  }
  else if (S_ISCHR(src->st_mode)) {
    dst->st_mode |= MRB_IO_S_IFCHR;
  }
  else if (S_ISBLK(src->st_mode)) {
    dst->st_mode |= MRB_IO_S_IFBLK;
  }
#ifdef MRB_HAL_IO_HAS_STAT_FIFO
  else if (S_ISFIFO(src->st_mode)) {
    dst->st_mode |= MRB_IO_S_IFIFO;
  }
#endif
#ifdef MRB_HAL_IO_HAS_STAT_SYMLINK
  else if (S_ISLNK(src->st_mode)) {
    dst->st_mode |= MRB_IO_S_IFLNK;
  }
#endif
#ifdef MRB_HAL_IO_HAS_STAT_SOCKET
  else if (S_ISSOCK(src->st_mode)) {
    dst->st_mode |= MRB_IO_S_IFSOCK;
  }
#endif
#endif /* MRB_IO_TYPE_IS_HAL_TYPE */
  dst->st_nlink = (mrb_int)src->st_nlink;
  dst->st_uid = (mrb_int)src->st_uid;
  dst->st_gid = (mrb_int)src->st_gid;
  dst->st_rdev = (mrb_int)src->st_rdev;
  dst->st_size = (mrb_int)src->st_size;
  dst->st_atime = (mrb_int)atime_val;
  dst->st_mtime = (mrb_int)mtime_val;
  dst->st_ctime = (mrb_int)ctime_val;

#ifdef HAVE_STRUCT_STAT_ST_BLKSIZE
  dst->st_blksize = (mrb_int)src->st_blksize;
#else
  dst->st_blksize = 512;
#endif
#ifdef HAVE_STRUCT_STAT_ST_BLOCKS
  dst->st_blocks = (mrb_int)src->st_blocks;
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
mrb_hal_io_chmod(mrb_state *mrb, const char *path, mrb_int mode)
{
  (void)mrb;
  return chmod(path, perm_from_hal(mode));
}

mrb_int
mrb_hal_io_umask(mrb_state *mrb, mrb_int mask)
{
  mode_t old;
  (void)mrb;

  if (mask < 0) {
    /* Just query current value */
    old = umask(0);
    umask(old);
  }
  else {
    old = umask(perm_from_hal(mask));
  }
  return perm_to_hal(old);
}

int
mrb_hal_io_ftruncate(mrb_state *mrb, int fd, mrb_int length)
{
  (void)mrb;
  return ftruncate(fd, (off_t)length);
}

#ifdef MRB_HAL_IO_HAS_FLOCK
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
#endif /* MRB_HAL_IO_HAS_FLOCK */

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

#ifdef MRB_HAL_IO_HAS_SYMLINK
int
mrb_hal_io_symlink(mrb_state *mrb, const char *target, const char *linkpath)
{
  (void)mrb;
  return symlink(target, linkpath);
}

mrb_int
mrb_hal_io_readlink(mrb_state *mrb, const char *path, char *buf, size_t bufsize)
{
  (void)mrb;
  return (mrb_int)readlink(path, buf, bufsize);
}
#endif /* MRB_HAL_IO_HAS_SYMLINK */

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
mrb_hal_io_open(mrb_state *mrb, const char *path, int flags, mrb_int mode)
{
  int fd;
  (void)mrb;

  fd = open(path, flags, perm_from_hal(mode));
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

mrb_int
mrb_hal_io_read(mrb_state *mrb, int fd, void *buf, size_t count)
{
  (void)mrb;
  return (mrb_int)read(fd, buf, count);
}

mrb_int
mrb_hal_io_write(mrb_state *mrb, int fd, const void *buf, size_t count)
{
  (void)mrb;
  return (mrb_int)write(fd, buf, count);
}

mrb_int
mrb_hal_io_lseek(mrb_state *mrb, int fd, mrb_int offset, int whence)
{
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

  return (mrb_int)lseek(fd, (off_t)offset, posix_whence);
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
  if (fd < 0 || fd >= FD_SETSIZE) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "fd is out of range");
    return;
  }
  if (fdset) {
    FD_SET(fd, &fdset->fds);
  }
}

int
mrb_hal_io_fdset_isset(mrb_state *mrb, int fd, mrb_io_fdset *fdset)
{
  (void)mrb;
  if (fd < 0 || fd >= FD_SETSIZE) {
    mrb_raise(mrb, E_ARGUMENT_ERROR, "fd is out of range");
    return 0;
  }
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
