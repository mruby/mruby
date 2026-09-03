/*
** process_hal_features.h - what the Windows port of mruby-process implements
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

/* No wait.  Win32 waits on a handle, and a handle is got by opening a
   process ID, which succeeds for any process this one may open and says
   nothing about parentage: waiting on one would report a stranger's exit
   code as a child's.  A port learns of its children when it creates them,
   so `Process.wait` and its three other spellings are answerable once
   spawn exists and not before. */

#endif /* MRUBY_PROCESS_HAL_FEATURES_H */
