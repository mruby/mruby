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
**     and signal 0 asks whether the process can be opened at all;
**   - `Process.times`' cutime and cstime read 0, for the reason given at
**     mrb_hal_process_times() below.
**
** The clocks are the one part Win32 answers in full: the wall clock as a
** FILETIME, the monotonic one from the performance counter, and the two CPU
** times from GetProcessTimes() and GetThreadTimes(); the same
** GetProcessTimes() call also answers this process's own share of
** Process.times.
*/

#include <mruby.h>
#include "process_hal.h"

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

static void
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
 * Clocks
 *
 * Four clocks, each read through the Win32 call that suits it, and for each
 * way of reading one the granularity that way reads at.  The two travel
 * together as one `win_clock`, so neither is chosen without the other.
 */

/* A FILETIME counts 100ns intervals.  The wall-clock one is counted from
   1601-01-01 UTC, which is FILETIME_EPOCH_DELTA of them before the Unix
   epoch.

   That 100ns is what this port answers as the granularity of every reading
   written as a FILETIME: two moments closer together than a tick have no
   room to be written down apart, and Windows states no rate at which the
   clocks behind those readings move. */
#define FILETIME_TICKS_PER_SEC 10000000LL
#define FILETIME_EPOCH_DELTA   116444736000000000LL
#define NSEC_PER_FILETIME_TICK 100

static uint64_t
filetime_to_u64(const FILETIME *ft)
{
  return ((uint64_t)ft->dwHighDateTime << 32) | (uint64_t)ft->dwLowDateTime;
}

/* Split a count of FILETIME ticks into seconds and nanoseconds, normalizing
   a negative count downwards so that the nanoseconds land in the
   [0, 999999999] the HAL promises. */
static void
clock_time_from_ticks(mrb_process_clock_time *t, int64_t ticks)
{
  t->sec = ticks / FILETIME_TICKS_PER_SEC;
  t->nsec = (ticks % FILETIME_TICKS_PER_SEC) * NSEC_PER_FILETIME_TICK;

  if (t->nsec < 0) {
    t->sec -= 1;
    t->nsec += NSEC_PER_SEC;
  }
}

/*
 * A Win32 entry point this port resolves at run time.
 *
 * Looked up rather than linked, so that one binary runs both on the Windows
 * that has the call and on the one that does not, and remembered rather than
 * looked up per reading, since a clock is read in loops.
 *
 * `slot` holds NULL before anything has looked, PROC_ABSENT once a lookup
 * has come back empty, and the entry point itself otherwise.  PROC_ABSENT is
 * any value a lookup could not return; NULL cannot serve, being what the
 * slot already says before anyone has looked.
 *
 * The slot is published and read back through interlocked operations.  Two
 * threads reading a clock at once would otherwise be writing and reading a
 * plain static without synchronization, which is a data race whether or not
 * they arrive at the same answer, and a race is undefined rather than merely
 * unlikely to matter.  InitOnceExecuteOnce() would say this more directly,
 * but it is Vista and later, and binding to it would cost this port the
 * older Windows that resolving at run time is here to keep working on; the
 * interlocked calls have been there since XP.
 */
typedef struct win_proc {
  PVOID volatile slot;
  const wchar_t *dll;
  const char *name;
} win_proc;

#define PROC_ABSENT ((PVOID)(INT_PTR)-1)

static PVOID
resolve_proc(win_proc *p)
{
  PVOID resolved = InterlockedCompareExchangePointer(&p->slot, NULL, NULL);

  if (resolved == NULL) {
    HMODULE dll = GetModuleHandleW(p->dll);
    /* Through void*: a FARPROC is not the function's own type, and a direct
       cast between the two is what -Wcast-function-type warns about. */
    resolved = (dll == NULL) ? NULL
             : (PVOID)(void*)GetProcAddress(dll, p->name);
    if (resolved == NULL) resolved = PROC_ABSENT;
    /* Every thread that looked found the same entry point, so a later
       write only repeats what is already in the slot. */
    InterlockedExchangePointer(&p->slot, resolved);
  }
  return (resolved == PROC_ABSENT) ? NULL : resolved;
}

/* GetSystemTimePreciseAsFileTime(), or NULL where this Windows is older than
   8 and has only the coarse reading. */
typedef VOID (WINAPI *precise_system_time_fn)(LPFILETIME);

static win_proc precise_time_proc = {
  NULL, L"kernel32.dll", "GetSystemTimePreciseAsFileTime"
};

static precise_system_time_fn
precise_system_time(void)
{
  return (precise_system_time_fn)resolve_proc(&precise_time_proc);
}

/* NtQueryTimerResolution() answers a live "CurrentResolution": the interval
   the clock interrupt is firing at right now, in the FILETIME's unit.  That
   is what the coarse wall clock moves by, and it is the one number Windows
   will state that follows a timeBeginPeriod() call in either direction, so a
   caller reading a clock on a system that has raised the interrupt rate is
   told the rate it is being read at rather than the one the system booted
   with.

   Undocumented, but read rather than guessed at: it is ntdll's half of the
   same pair NtSetTimerResolution() is, has answered this since XP, and is
   how the runtimes already shipping on every Windows this port supports
   answer the same question.  Where it cannot be reached,
   GetSystemTimeAdjustment()'s TimeIncrement stands in below, naming the
   interrupt the system booted with rather than the one it is keeping now. */
typedef LONG (NTAPI *nt_query_timer_resolution_fn)(PULONG, PULONG, PULONG);

static win_proc timer_resolution_proc = {
  NULL, L"ntdll.dll", "NtQueryTimerResolution"
};

static nt_query_timer_resolution_fn
nt_query_timer_resolution(void)
{
  return (nt_query_timer_resolution_fn)resolve_proc(&timer_resolution_proc);
}

/* The performance counter and its frequency, which is what the monotonic
   clock is read from.  Windows documents both calls as succeeding on every
   version this port supports, so the failure arm below is only there to keep
   an undocumented failure from being read as a time.  A frequency above
   INT64_MAX / NSEC_PER_SEC, about 9.2GHz, is refused rather than wrapped:
   `(counter % freq) * NSEC_PER_SEC` would overflow past it. */
static int
performance_counter(int64_t *counter, int64_t *freq)
{
  LARGE_INTEGER c, f;

  if (!QueryPerformanceFrequency(&f) || !QueryPerformanceCounter(&c)) {
    set_errno_from_win32(GetLastError());
    return -1;
  }
  if (f.QuadPart <= 0 || f.QuadPart > INT64_MAX / NSEC_PER_SEC) {
    errno = EINVAL;
    return -1;
  }
  *counter = (int64_t)c.QuadPart;
  *freq = (int64_t)f.QuadPart;
  return 0;
}

/*
 * Readings
 */

static void
wall_clock_from_filetime(mrb_process_clock_time *t, const FILETIME *ft)
{
  clock_time_from_ticks(t, (int64_t)filetime_to_u64(ft) - FILETIME_EPOCH_DELTA);
}

/* The wall clock as GetSystemTimePreciseAsFileTime() interpolates it between
   two clock interrupts.  win_clock_for() selects this pair only after the
   lookup has answered, so the entry point here is never NULL. */
static int
precise_wall_read(mrb_process_clock_time *t)
{
  FILETIME ft;

  precise_system_time()(&ft);
  wall_clock_from_filetime(t, &ft);
  return 0;
}

/* The wall clock as the clock interrupt leaves it, which is the only reading
   of it a Windows older than 8 has. */
static int
coarse_wall_read(mrb_process_clock_time *t)
{
  FILETIME ft;

  GetSystemTimeAsFileTime(&ft);
  wall_clock_from_filetime(t, &ft);
  return 0;
}

static int
monotonic_read(mrb_process_clock_time *t)
{
  int64_t counter, freq;

  /* The performance counter never steps, and Windows names no origin for it
     beyond a point it fixes itself and holds while the system runs.  That is
     all this clock is asked for: the origin stands for the life of the
     process, so two readings can be subtracted, which is what a caller has
     one of these for. */
  if (performance_counter(&counter, &freq) != 0) return -1;
  t->sec = counter / freq;
  t->nsec = (counter % freq) * NSEC_PER_SEC / freq;
  return 0;
}

/* The kernel and user FILETIMEs behind a process's or a thread's CPU time,
   as raw FILETIME ticks: cpu_time() below sums the two into the one number
   MRB_PROCESS_CLOCK_PROCESS_CPUTIME/THREAD_CPUTIME reports, and
   mrb_hal_process_times() keeps them apart as utime and stime. */
static int
process_times_raw(mrb_bool thread, int64_t *kernel_ticks, int64_t *user_ticks)
{
  FILETIME creation, exit, kernel, user;
  BOOL ok;

  if (thread) {
    ok = GetThreadTimes(GetCurrentThread(), &creation, &exit, &kernel, &user);
  }
  else {
    ok = GetProcessTimes(GetCurrentProcess(), &creation, &exit, &kernel, &user);
  }
  if (!ok) {
    set_errno_from_win32(GetLastError());
    return -1;
  }
  *kernel_ticks = (int64_t)filetime_to_u64(&kernel);
  *user_ticks = (int64_t)filetime_to_u64(&user);
  return 0;
}

/* The CPU time a process or a thread has spent, as the sum of the kernel and
   user halves Win32 reports it in. */
static int
cpu_time(mrb_bool thread, mrb_process_clock_time *t)
{
  int64_t kernel_ticks, user_ticks;

  if (process_times_raw(thread, &kernel_ticks, &user_ticks) != 0) return -1;
  clock_time_from_ticks(t, kernel_ticks + user_ticks);
  return 0;
}

static int
process_cpu_read(mrb_process_clock_time *t)
{
  return cpu_time(FALSE, t);
}

static int
thread_cpu_read(mrb_process_clock_time *t)
{
  return cpu_time(TRUE, t);
}

/*
 * Granularities
 */

/* One tick of a FILETIME: the precise wall clock and both CPU clocks are all
   read as FILETIMEs, so this is the granularity of all three.  The FILETIME
   macros above say why a tick is what this port can state about them. */
static int
filetime_tick_resolution(mrb_process_clock_time *t)
{
  t->sec = 0;
  t->nsec = NSEC_PER_FILETIME_TICK;
  return 0;
}

/* The interval between two clock interrupts as it stands now, which is what
   the coarse wall clock moves by: that reading does not interpolate, so it
   is no finer than the interrupt that updates it. */
static int
clock_interrupt_resolution(mrb_process_clock_time *t)
{
  nt_query_timer_resolution_fn query = nt_query_timer_resolution();
  ULONG minimum, maximum, current;
  DWORD adjustment, increment;
  BOOL disabled;

  t->sec = 0;
  if (query != NULL && query(&minimum, &maximum, &current) == 0) {
    t->nsec = (int64_t)current * NSEC_PER_FILETIME_TICK;
    return 0;
  }
  if (!GetSystemTimeAdjustment(&adjustment, &increment, &disabled)) {
    set_errno_from_win32(GetLastError());
    return -1;
  }
  t->nsec = (int64_t)increment * NSEC_PER_FILETIME_TICK;
  return 0;
}

/* A tick of the performance counter, rounded up.  A frequency that does not
   divide a second into whole nanoseconds would otherwise be reported as
   finer than it is, saying that two readings a tick apart can differ by less
   than a tick; rounding up also keeps a frequency above 1GHz from flooring
   to nothing, and a granularity of nothing is not one. */
static int
performance_counter_resolution(mrb_process_clock_time *t)
{
  int64_t counter, freq;

  /* The counter is read and dropped: performance_counter() answers both, and
     a granularity is the frequency alone. */
  if (performance_counter(&counter, &freq) != 0) return -1;
  t->sec = 0;
  t->nsec = (NSEC_PER_SEC + freq - 1) / freq;
  return 0;
}

/*
 * A way of reading one clock, paired with the granularity that way of
 * reading has; mrb_hal_process_clock_getres() in process_hal.h says what
 * such a granularity is.  Keeping the two in one struct is what stops them
 * drifting apart, and three pairs share filetime_tick_resolution() because
 * three of the readings are FILETIMEs.
 */
typedef struct win_clock {
  int (*read)(mrb_process_clock_time *t);
  int (*resolution)(mrb_process_clock_time *t);
} win_clock;

static const win_clock precise_wall_clock =
  { precise_wall_read, filetime_tick_resolution };
static const win_clock coarse_wall_clock =
  { coarse_wall_read, clock_interrupt_resolution };
static const win_clock monotonic_clock =
  { monotonic_read, performance_counter_resolution };
static const win_clock process_cpu_clock =
  { process_cpu_read, filetime_tick_resolution };
static const win_clock thread_cpu_clock =
  { thread_cpu_read, filetime_tick_resolution };

static const win_clock *
win_clock_for(mrb_int clock_id)
{
  switch (clock_id) {
  case MRB_PROCESS_CLOCK_REALTIME:
    /* Which wall clock this Windows has decides both how it is read and how
       finely it is read, so it is asked once and answered as a pair. */
    if (precise_system_time() != NULL) return &precise_wall_clock;
    return &coarse_wall_clock;
  case MRB_PROCESS_CLOCK_MONOTONIC:       return &monotonic_clock;
  case MRB_PROCESS_CLOCK_PROCESS_CPUTIME: return &process_cpu_clock;
  case MRB_PROCESS_CLOCK_THREAD_CPUTIME:  return &thread_cpu_clock;
  default:                                return NULL;
  }
}

int
mrb_hal_process_clock_gettime(mrb_state *mrb, mrb_int clock_id,
                              mrb_process_clock_time *t)
{
  const win_clock *c = win_clock_for(clock_id);
  (void)mrb;

  if (c == NULL) {
    errno = EINVAL;
    return -1;
  }
  return c->read(t);
}

int
mrb_hal_process_clock_getres(mrb_state *mrb, mrb_int clock_id,
                             mrb_process_clock_time *t)
{
  const win_clock *c = win_clock_for(clock_id);
  (void)mrb;

  if (c == NULL) {
    errno = EINVAL;
    return -1;
  }
  return c->resolution(t);
}

/*
 * CPU Time Totals
 */

int
mrb_hal_process_times(mrb_state *mrb, mrb_process_times *t)
{
  int64_t kernel_ticks, user_ticks;
  (void)mrb;

  if (process_times_raw(FALSE, &kernel_ticks, &user_ticks) != 0) return -1;
  clock_time_from_ticks(&t->utime, user_ticks);
  clock_time_from_ticks(&t->stime, kernel_ticks);

  /* Win32 has no call that answers a reaped child's CPU time, and this port
     creates no children yet in any case (Process.spawn is a separate
     change), so there is nothing to add up.  0 says exactly that: nothing
     has been added, not that nothing was asked. */
  t->cutime.sec = 0;
  t->cutime.nsec = 0;
  t->cstime.sec = 0;
  t->cstime.nsec = 0;
  return 0;
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
