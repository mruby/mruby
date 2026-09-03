/*
** process_hal_features.h - what the POSIX port of mruby-process implements
**
** See Copyright Notice in mruby.h
**
** The gem's include/process_hal.h reads this before it declares anything.  A macro defined
** here guards three things at once: the prototype there, the implementation
** in process_hal.c, and the method definition under src/.  A port that declared a
** capability and did not implement it would fail to link, and one that
** declares nothing owes nothing.
*/

#ifndef MRUBY_PROCESS_HAL_FEATURES_H
#define MRUBY_PROCESS_HAL_FEATURES_H

/* waitpid(2): `Process.wait`, `Process.waitpid`, `Process.wait2` and
   `Process.waitpid2`. */
#define MRB_HAL_PROCESS_HAS_WAIT

#endif /* MRUBY_PROCESS_HAL_FEATURES_H */
