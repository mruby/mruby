/*
** dir_hal_features.h - what the Windows port of mruby-dir implements
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

/* A FindNextFile() walk has no position to read or return to: `Dir#seek`
   and `Dir#tell` are left undefined rather than defined to fail. */

#endif /* MRUBY_DIR_HAL_FEATURES_H */
