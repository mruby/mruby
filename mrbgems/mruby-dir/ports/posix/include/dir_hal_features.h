/*
** dir_hal_features.h - what the POSIX port of mruby-dir implements
**
** See Copyright Notice in mruby.h
**
** The gem's include/dir_hal.h reads this before it declares anything.  A macro defined
** here guards three things at once: the prototype there, the implementation
** in dir_hal.c, and the method definition under src/.  A port that declared a
** capability and did not implement it would fail to link, and one that
** declares nothing owes nothing.
*/

#ifndef MRUBY_DIR_HAL_FEATURES_H
#define MRUBY_DIR_HAL_FEATURES_H

/* seekdir(3) and telldir(3): `Dir#seek` and `Dir#tell`.  Android's are not
   reliable. */
#if !defined(__ANDROID__)
# define MRB_HAL_DIR_HAS_SEEK
# define MRB_HAL_DIR_HAS_TELL
#endif

#endif /* MRUBY_DIR_HAL_FEATURES_H */
