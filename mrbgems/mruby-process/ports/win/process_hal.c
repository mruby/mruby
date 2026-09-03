/*
** process_hal.c - Windows HAL implementation for mruby-process
**
** See Copyright Notice in mruby.h
**
** Windows implementation of the process HAL.  Windows has no signals between
** processes and no wait status beyond an exit code, so this port covers the
** part of the interface Win32 can answer honestly and fails, rather than
** guesses, at the rest:
**
**   - a wait cannot be answered at all.  Win32 identifies a process to wait
**     on by handle, and a handle is obtained by opening a process ID, which
**     succeeds for any process this one may open rather than only for its
**     own children.  Waiting on it would report a stranger's exit code as a
**     child's, so every wait fails instead: ECHILD for a specific process,
**     since this port can name no child, and ENOSYS for MRB_PROCESS_WAIT_ANY,
**     which has no handle standing for it.  A port learns of its children
**     when it creates them, so this is answerable once spawn exists and not
**     before.  No process is ever reported as stopped either;
**   - a raw status is the child's exit code, which is what mruby-io's
**     `IO.popen` already gives the `Process::Status` it builds on this
**     platform, so a decoded status always reads as exited;
**   - only `KILL` and `TERM` can be delivered, both as TerminateProcess(),
**     and signal 0 asks whether the process can be opened at all.
**
** The clocks are clock_hal.c's.
*/

#include <mruby.h>
#include "process_hal.h"
#include "process_hal_win.h"

#include <windows.h>
#include <tlhelp32.h>

#include <errno.h>
#include <stdint.h>

/* Vista and later; fall back to the access right every Windows has. */
#ifndef PROCESS_QUERY_LIMITED_INFORMATION
#define PROCESS_QUERY_LIMITED_INFORMATION PROCESS_QUERY_INFORMATION
#endif

/* The two signals this port can deliver, by the numbers mruby-signal's
   Windows table gives them.  Naming them here rather than asking the signal
   HAL keeps this port answering about delivery alone: which names resolve to
   which numbers is that gem's question, and a port that could not reach it
   would still have to know these two. */
#define SIGNAL_KILL 9
#define SIGNAL_TERM 15

/* Exit code TerminateProcess() stamps on the victim.  128+SIGTERM is the
   shell's convention for "died on a signal", which is the closest a Windows
   exit code gets to saying so. */
#define TERMINATED_EXIT_CODE (128 + SIGNAL_TERM)

/*
 * Helper Functions
 */

/* Open a process by pid.  A pid Windows will not open is "no such process"
   here, whatever it says: OpenProcess reports a pid that names nothing as
   ERROR_INVALID_PARAMETER rather than as a missing process. */
static HANDLE
open_process(mrb_int pid, DWORD access)
{
  HANDLE h;
  DWORD err;

  if (pid <= 0 || (uint64_t)pid > 0xFFFFFFFFu) {
    errno = ESRCH;
    return NULL;
  }
  h = OpenProcess(access, FALSE, (DWORD)pid);
  if (h == NULL) {
    err = GetLastError();
    switch (err) {
    case ERROR_INVALID_PARAMETER:
    case ERROR_INVALID_HANDLE:
    case ERROR_NOT_FOUND:
      errno = ESRCH;
      break;
    default:
      set_errno_from_win32(err);
      break;
    }
  }
  return h;
}

/*
 * Process Identity
 */

mrb_int
mrb_hal_process_pid(mrb_state *mrb)
{
  (void)mrb;
  return (mrb_int)GetCurrentProcessId();
}

mrb_int
mrb_hal_process_ppid(mrb_state *mrb)
{
  HANDLE snapshot;
  PROCESSENTRY32W entry;
  DWORD self = GetCurrentProcessId();
  mrb_int ppid = -1;
  (void)mrb;

  snapshot = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  if (snapshot == INVALID_HANDLE_VALUE) {
    set_errno_from_win32(GetLastError());
    return -1;
  }

  entry.dwSize = sizeof(entry);
  if (Process32FirstW(snapshot, &entry)) {
    do {
      if (entry.th32ProcessID == self) {
        ppid = (mrb_int)entry.th32ParentProcessID;
        break;
      }
    } while (Process32NextW(snapshot, &entry));
  }
  CloseHandle(snapshot);

  if (ppid < 0) errno = ESRCH;
  return ppid;
}

/*
 * Waiting
 */

int
mrb_hal_process_waitpid(mrb_state *mrb, mrb_int pid, unsigned int flags,
                        mrb_int *result_pid, mrb_int *raw_status)
{
  (void)mrb;
  (void)flags;
  (void)result_pid;
  (void)raw_status;

  /* A wait needs a handle, and there is no handle standing for "any child". */
  if (pid <= 0) {
    errno = ENOSYS;
    return -1;
  }

  /* OpenProcess() opens any process this one is allowed to open, and asks
     nothing about parentage, so a handle got that way is no evidence that
     the process is a child.  Waiting on it would answer Process.waitpid with
     an unrelated process's exit code and publish it as `$?`.  ECHILD is both
     the honest answer and the same one a real child gets from a wait that
     has already reaped it: this port knows of no children, because a port
     learns of its children by creating them, and creating them is spawn's
     to add. */
  errno = ECHILD;
  return -1;
}

/*
 * Signalling
 */

int
mrb_hal_process_kill(mrb_state *mrb, mrb_int pid, mrb_int signo)
{
  HANDLE h;
  (void)mrb;

  if (signo == 0) {
    h = open_process(pid, PROCESS_QUERY_LIMITED_INFORMATION);
    if (h == NULL) return -1;
    CloseHandle(h);
    return 0;
  }

  if (signo != SIGNAL_KILL && signo != SIGNAL_TERM) {
    errno = ENOSYS;
    return -1;
  }

  h = open_process(pid, PROCESS_TERMINATE);
  if (h == NULL) return -1;
  if (!TerminateProcess(h, TERMINATED_EXIT_CODE)) {
    set_errno_from_win32(GetLastError());
    CloseHandle(h);
    return -1;
  }
  CloseHandle(h);
  return 0;
}

/*
 * Status Decoding
 */

void
mrb_hal_process_status_decode(mrb_state *mrb, mrb_int pid, mrb_int raw_status,
                              mrb_process_status *status)
{
  (void)mrb;

  /* A Windows raw status is an exit code and nothing else: a process killed
     with TerminateProcess() is indistinguishable from one that exited with
     the same code, so every status reads as exited.  Nothing in this port
     produces one, since it performs no wait; a status reaching here was
     built by a caller with a raw status of its own, which is how mruby-io's
     IO.popen reports a child on this platform.  An exit code such as
     0xC0000005 does not fit a 32-bit mrb_int unsigned and reads back
     negative, which is what Process::Status#to_i then shows. */
  status->pid = pid;
  status->raw_status = raw_status;
  status->exitstatus = raw_status;
  status->termsig = 0;
  status->stopsig = 0;
  status->flags = MRB_PROCESS_STATUS_EXITED;
}

/*
 * HAL Initialization/Finalization
 */

void
mrb_hal_process_init(mrb_state *mrb)
{
  (void)mrb;
}

void
mrb_hal_process_final(mrb_state *mrb)
{
  (void)mrb;
}
