/*
** io_hal.h - IO Hardware Abstraction Layer (HAL)
**
** See Copyright Notice in mruby.h
**
** This header defines the HAL interface for platform-specific I/O operations.
** Platform-specific implementations (hal-posix-io, hal-win-io, etc.) must
** provide all functions declared here.
*/

#ifndef MRUBY_IO_HAL_H
#define MRUBY_IO_HAL_H

#include <mruby.h>
#include <stdint.h>

/*
 * Platform-independent type definitions
 */

/* File status structure - platform-independent representation */
typedef struct mrb_io_stat {
  uint64_t st_dev;       /* Device ID */
  uint64_t st_ino;       /* Inode number */
  uint32_t st_mode;      /* File mode/permissions */
  uint32_t st_nlink;     /* Number of hard links */
  uint32_t st_uid;       /* User ID */
  uint32_t st_gid;       /* Group ID */
  uint64_t st_rdev;      /* Device ID (if special file) */
  int64_t  st_size;      /* File size in bytes */
  int64_t  st_atime;     /* Last access time */
  int64_t  st_mtime;     /* Last modification time */
  int64_t  st_ctime;     /* Last status change time */
  int64_t  st_blksize;   /* Block size for filesystem I/O */
  int64_t  st_blocks;    /* Number of 512B blocks allocated */
} mrb_io_stat;

/* Timeval structure for select() */
typedef struct mrb_io_timeval {
  int64_t tv_sec;        /* Seconds */
  int64_t tv_usec;       /* Microseconds */
} mrb_io_timeval;

/* File descriptor set for select() */
typedef struct mrb_io_fdset mrb_io_fdset;

/*
 * File mode constants (POSIX-style)
 */

/* File type masks */
#define MRB_IO_S_IFMT   0170000  /* Type of file mask */
#define MRB_IO_S_IFSOCK 0140000  /* Socket */
#define MRB_IO_S_IFLNK  0120000  /* Symbolic link */
#define MRB_IO_S_IFREG  0100000  /* Regular file */
#define MRB_IO_S_IFBLK  0060000  /* Block device */
#define MRB_IO_S_IFDIR  0040000  /* Directory */
#define MRB_IO_S_IFCHR  0020000  /* Character device */
#define MRB_IO_S_IFIFO  0010000  /* FIFO */

/* File type test macros */
#define MRB_IO_S_ISREG(m)  (((m) & MRB_IO_S_IFMT) == MRB_IO_S_IFREG)
#define MRB_IO_S_ISDIR(m)  (((m) & MRB_IO_S_IFMT) == MRB_IO_S_IFDIR)
#define MRB_IO_S_ISCHR(m)  (((m) & MRB_IO_S_IFMT) == MRB_IO_S_IFCHR)
#define MRB_IO_S_ISBLK(m)  (((m) & MRB_IO_S_IFMT) == MRB_IO_S_IFBLK)
#define MRB_IO_S_ISFIFO(m) (((m) & MRB_IO_S_IFMT) == MRB_IO_S_IFIFO)
#define MRB_IO_S_ISLNK(m)  (((m) & MRB_IO_S_IFMT) == MRB_IO_S_IFLNK)
#define MRB_IO_S_ISSOCK(m) (((m) & MRB_IO_S_IFMT) == MRB_IO_S_IFSOCK)

/* File lock constants */
#define MRB_IO_LOCK_SH 1  /* Shared lock */
#define MRB_IO_LOCK_EX 2  /* Exclusive lock */
#define MRB_IO_LOCK_NB 4  /* Non-blocking */
#define MRB_IO_LOCK_UN 8  /* Unlock */

/* Seek constants */
#define MRB_IO_SEEK_SET 0  /* Seek from beginning */
#define MRB_IO_SEEK_CUR 1  /* Seek from current position */
#define MRB_IO_SEEK_END 2  /* Seek from end */

/*
 * HAL Interface - File Operations
 */

/**
 * Get file status by path
 *
 * @param mrb mruby state
 * @param path File path (UTF-8)
 * @param st Output stat structure
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_stat(mrb_state *mrb, const char *path, mrb_io_stat *st);

/**
 * Get file status by descriptor
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param st Output stat structure
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_fstat(mrb_state *mrb, int fd, mrb_io_stat *st);

/**
 * Get link status (don't follow symlinks)
 *
 * @param mrb mruby state
 * @param path File path (UTF-8)
 * @param st Output stat structure
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_lstat(mrb_state *mrb, const char *path, mrb_io_stat *st);

/**
 * Change file permissions
 *
 * @param mrb mruby state
 * @param path File path (UTF-8)
 * @param mode New permission mode
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_chmod(mrb_state *mrb, const char *path, uint32_t mode);

/**
 * Set/get file creation mask
 *
 * @param mrb mruby state
 * @param mask New umask value (if < 0, only returns current value)
 * @return Previous umask value
 */
uint32_t mrb_hal_io_umask(mrb_state *mrb, int32_t mask);

/**
 * Truncate file to specified length
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param length New file length
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_ftruncate(mrb_state *mrb, int fd, int64_t length);

/**
 * Apply or remove advisory lock on file
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param operation Lock operation (MRB_IO_LOCK_*)
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_flock(mrb_state *mrb, int fd, int operation);

/**
 * Delete a file
 *
 * @param mrb mruby state
 * @param path File path (UTF-8)
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_unlink(mrb_state *mrb, const char *path);

/**
 * Rename a file
 *
 * @param mrb mruby state
 * @param oldpath Old file path (UTF-8)
 * @param newpath New file path (UTF-8)
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_rename(mrb_state *mrb, const char *oldpath, const char *newpath);

/**
 * Create a symbolic link
 *
 * @param mrb mruby state
 * @param target Target path (UTF-8)
 * @param linkpath Link path (UTF-8)
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_symlink(mrb_state *mrb, const char *target, const char *linkpath);

/**
 * Read value of a symbolic link
 *
 * @param mrb mruby state
 * @param path Symlink path (UTF-8)
 * @param buf Buffer to store result (UTF-8)
 * @param bufsize Buffer size
 * @return Number of bytes placed in buf, -1 on error (sets errno)
 */
int64_t mrb_hal_io_readlink(mrb_state *mrb, const char *path, char *buf, size_t bufsize);

/**
 * Resolve pathname to absolute path
 *
 * @param mrb mruby state
 * @param path Pathname (UTF-8)
 * @param resolved Buffer for resolved path (must be at least PATH_MAX size)
 * @return Pointer to resolved on success, NULL on error (sets errno)
 */
char* mrb_hal_io_realpath(mrb_state *mrb, const char *path, char *resolved);

/**
 * Get current working directory
 *
 * @param mrb mruby state
 * @param buf Buffer to store result (UTF-8)
 * @param size Buffer size
 * @return Pointer to buf on success, NULL on error (sets errno)
 */
char* mrb_hal_io_getcwd(mrb_state *mrb, char *buf, size_t size);

/**
 * Get environment variable
 *
 * @param mrb mruby state
 * @param name Variable name
 * @return Value string (UTF-8) or NULL if not found
 */
const char* mrb_hal_io_getenv(mrb_state *mrb, const char *name);

/**
 * Get user's home directory
 *
 * @param mrb mruby state
 * @param username User name (NULL for current user)
 * @return Home directory path (UTF-8) or NULL on error (sets errno)
 */
const char* mrb_hal_io_gethome(mrb_state *mrb, const char *username);

/*
 * HAL Interface - Core I/O Operations
 */

/**
 * Open file
 *
 * @param mrb mruby state
 * @param path File path (UTF-8)
 * @param flags Open flags (O_RDONLY, O_WRONLY, O_RDWR, etc.)
 * @param mode Creation mode (used if O_CREAT is set)
 * @return File descriptor on success, -1 on error (sets errno)
 */
int mrb_hal_io_open(mrb_state *mrb, const char *path, int flags, uint32_t mode);

/**
 * Close file descriptor
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_close(mrb_state *mrb, int fd);

/**
 * Read from file descriptor
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param buf Buffer to store data
 * @param count Maximum bytes to read
 * @return Number of bytes read, 0 on EOF, -1 on error (sets errno)
 */
int64_t mrb_hal_io_read(mrb_state *mrb, int fd, void *buf, size_t count);

/**
 * Write to file descriptor
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param buf Data to write
 * @param count Number of bytes to write
 * @return Number of bytes written, -1 on error (sets errno)
 */
int64_t mrb_hal_io_write(mrb_state *mrb, int fd, const void *buf, size_t count);

/**
 * Reposition file offset
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param offset Offset value
 * @param whence Reference point (MRB_IO_SEEK_SET/CUR/END)
 * @return New offset from beginning of file, -1 on error (sets errno)
 */
int64_t mrb_hal_io_lseek(mrb_state *mrb, int fd, int64_t offset, int whence);

/**
 * Duplicate file descriptor
 *
 * @param mrb mruby state
 * @param fd File descriptor to duplicate
 * @return New descriptor on success, -1 on error (sets errno)
 */
int mrb_hal_io_dup(mrb_state *mrb, int fd);

/**
 * Manipulate file descriptor
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param cmd Command (F_GETFD, F_SETFD, etc.)
 * @param arg Command argument
 * @return Depends on command, -1 on error (sets errno)
 */
int mrb_hal_io_fcntl(mrb_state *mrb, int fd, int cmd, int arg);

/**
 * Check if descriptor refers to terminal
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @return 1 if TTY, 0 if not, -1 on error (sets errno)
 */
int mrb_hal_io_isatty(mrb_state *mrb, int fd);

/**
 * Create pipe
 *
 * @param mrb mruby state
 * @param fds Array to store two file descriptors [read_end, write_end]
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_pipe(mrb_state *mrb, int fds[2]);

/*
 * HAL Interface - Process Operations
 */

/**
 * Spawn a new process
 *
 * Creates a new process and executes the command. File descriptors can be
 * redirected for stdin/stdout/stderr (-1 means don't redirect).
 *
 * POSIX: Uses fork() + dup2() + execl()
 * Windows: Uses CreateProcess() with STARTUPINFO
 *
 * @param mrb mruby state
 * @param cmd Command to execute (shell command)
 * @param stdin_fd File descriptor to use for stdin (-1 = don't redirect)
 * @param stdout_fd File descriptor to use for stdout (-1 = don't redirect)
 * @param stderr_fd File descriptor to use for stderr (-1 = don't redirect)
 * @param pid Output parameter for process ID
 * @return 0 on success, -1 on error (sets errno)
 */
int mrb_hal_io_spawn_process(mrb_state *mrb, const char *cmd,
                               int stdin_fd, int stdout_fd, int stderr_fd,
                               int *pid);

/**
 * Wait for process to change state
 *
 * @param mrb mruby state
 * @param pid Process ID to wait for
 * @param status Output parameter for exit status
 * @param options Wait options (0 for blocking wait)
 * @return Process ID on success, -1 on error (sets errno)
 */
int mrb_hal_io_waitpid(mrb_state *mrb, int pid, int *status, int options);

/*
 * HAL Interface - I/O Multiplexing
 */

/**
 * Allocate file descriptor set
 *
 * @param mrb mruby state
 * @return Pointer to fdset or NULL on error
 */
mrb_io_fdset* mrb_hal_io_fdset_alloc(mrb_state *mrb);

/**
 * Free file descriptor set
 *
 * @param mrb mruby state
 * @param fdset File descriptor set to free
 */
void mrb_hal_io_fdset_free(mrb_state *mrb, mrb_io_fdset *fdset);

/**
 * Clear file descriptor set
 *
 * @param mrb mruby state
 * @param fdset File descriptor set
 */
void mrb_hal_io_fdset_zero(mrb_state *mrb, mrb_io_fdset *fdset);

/**
 * Add descriptor to set
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param fdset File descriptor set
 */
void mrb_hal_io_fdset_set(mrb_state *mrb, int fd, mrb_io_fdset *fdset);

/**
 * Check if descriptor is in set
 *
 * @param mrb mruby state
 * @param fd File descriptor
 * @param fdset File descriptor set
 * @return Non-zero if fd is in set, 0 otherwise
 */
int mrb_hal_io_fdset_isset(mrb_state *mrb, int fd, mrb_io_fdset *fdset);

/**
 * Monitor multiple file descriptors
 *
 * @param mrb mruby state
 * @param nfds Highest file descriptor number + 1
 * @param readfds Set of descriptors to check for reading (NULL = ignore)
 * @param writefds Set of descriptors to check for writing (NULL = ignore)
 * @param errorfds Set of descriptors to check for errors (NULL = ignore)
 * @param timeout Timeout (NULL = block indefinitely)
 * @return Number of ready descriptors, 0 on timeout, -1 on error (sets errno)
 */
int mrb_hal_io_select(mrb_state *mrb, int nfds,
                       mrb_io_fdset *readfds,
                       mrb_io_fdset *writefds,
                       mrb_io_fdset *errorfds,
                       mrb_io_timeval *timeout);

/*
 * HAL Initialization/Finalization
 */

/**
 * Initialize I/O HAL
 *
 * Called during gem initialization. Platform-specific HAL should perform
 * any necessary setup here.
 *
 * @param mrb mruby state
 */
void mrb_hal_io_init(mrb_state *mrb);

/**
 * Finalize I/O HAL
 *
 * Called during gem finalization. Platform-specific HAL should perform
 * any necessary cleanup here.
 *
 * @param mrb mruby state
 */
void mrb_hal_io_final(mrb_state *mrb);

#endif /* MRUBY_IO_HAL_H */
