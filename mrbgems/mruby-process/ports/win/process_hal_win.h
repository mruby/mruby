/*
** process_hal_win.h - what the two halves of the Windows port share
**
** See Copyright Notice in mruby.h
**
** Private to ports/win: process_hal.c and clock_hal.c both report a failed
** Win32 call through errno, and this is the one table that says which
** errno a Win32 error is.  Not for src/ and not for other gems, which is
** why it sits beside the sources that include it rather than under an
** include/ directory.
*/

#ifndef MRUBY_PROCESS_HAL_WIN_H
#define MRUBY_PROCESS_HAL_WIN_H

#include <windows.h>
#include <errno.h>

static inline void
set_errno_from_win32(DWORD err)
{
  switch (err) {
  case ERROR_INVALID_PARAMETER:
    errno = EINVAL;
    break;
  case ERROR_ACCESS_DENIED:
    errno = EPERM;
    break;
  case ERROR_INVALID_HANDLE:
  case ERROR_NOT_FOUND:
    errno = ESRCH;
    break;
  default:
    errno = EINVAL;
    break;
  }
}

#endif /* MRUBY_PROCESS_HAL_WIN_H */
