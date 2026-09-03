/*
** io_hal_features.h - what the POSIX port of mruby-io implements
**
** See Copyright Notice in mruby.h
**
** The gem's include/io_hal.h reads this before it declares anything.  A macro defined
** here guards three things at once: the prototype there, the implementation
** in io_hal.c, and the method definition under src/.  A port that declared a
** capability and did not implement it would fail to link, and one that
** declares nothing owes nothing.
*/

#ifndef MRUBY_IO_HAL_FEATURES_H
#define MRUBY_IO_HAL_FEATURES_H

/* symlink(2) and readlink(2): `File.symlink` and `File.readlink`. */
#define MRB_HAL_IO_HAS_SYMLINK

/* flock(2): `File#flock`.  Solaris and Illumos have no flock(2). */
#if !defined(sun) && !defined(__sun)
# define MRB_HAL_IO_HAS_FLOCK
#endif

/* The file kinds mrb_hal_io_stat() and mrb_hal_io_lstat() can name in
   st_mode, beyond a regular file and a directory: `FileTest.pipe?`,
   `FileTest.symlink?` and `FileTest.socket?`.  Each guards the predicate
   that reads the kind, and the arm that writes it where io_hal.c maps
   the kinds one by one. */
#define MRB_HAL_IO_HAS_STAT_FIFO
#define MRB_HAL_IO_HAS_STAT_SYMLINK
#define MRB_HAL_IO_HAS_STAT_SOCKET

/* fork(2) and execl(3), and waitpid(2) for the child: `IO.popen`.  iOS
   lets no process run another, whatever the configuration asks for. */
#if defined(__APPLE__)
# include <TargetConditionals.h>
#endif
#if !(defined(TARGET_OS_IPHONE) && TARGET_OS_IPHONE)
# define MRB_HAL_IO_HAS_SPAWN_PROCESS
#endif

#endif /* MRUBY_IO_HAL_FEATURES_H */
