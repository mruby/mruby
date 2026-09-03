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

#endif /* MRUBY_IO_HAL_FEATURES_H */
