/*
** io_hal_features.h - what the Windows port of mruby-io implements
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

/* No symbolic links: creating one needs a privilege a process cannot count
   on, so `File.symlink` and `File.readlink` are left undefined rather than
   defined to fail. */

/* LockFileEx() stands in for flock(2): `File#flock`. */
#define MRB_HAL_IO_HAS_FLOCK

/* The st_mode _stat64() fills names a regular file, a directory or a
   character device and nothing else: no FIFO (an anonymous pipe is not
   one), no symbolic link, no socket.  `FileTest.pipe?`, `.symlink?` and
   `.socket?` are left undefined rather than defined to answer wrongly. */

#endif /* MRUBY_IO_HAL_FEATURES_H */
