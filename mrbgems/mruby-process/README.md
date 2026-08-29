# mruby-process

`Process` module and `Process::Status` class for mruby.

## Installation

Add the line below to your build configuration.

```ruby
  conf.gem core: 'mruby-process'
```

It is part of the `stdlib-io` gembox, so `default.gembox` and `full-core.gembox`
already include it.

## Implemented methods

| method                            | mruby-process | memo                                     |
| --------------------------------- | ------------- | ---------------------------------------- |
| Process.pid                       | o             | also `$$`                                |
| Process.ppid                      | o             |                                          |
| Process.kill                      | o             | no negative-signal form yet, see below   |
| Process.wait, .wait2              | o             | `.waitpid2` too; not `.waitall`          |
| Process.waitpid                   | o             | sets `$?`; POSIX only, see below         |
| Process.clock_gettime             | o             | seven units; symbolic clock ids          |
| Process.clock_getres              | o             | takes `:hertz` too                       |
| Process::WNOHANG                  | o             | mruby's own value, not the platform's    |
| Process::WUNTRACED                | o             | mruby's own value, not the platform's    |
| Process::CLOCK_REALTIME           | o             | mruby's own value, not the platform's    |
| Process::CLOCK_MONOTONIC          | o             | mruby's own value, not the platform's    |
| Process::CLOCK_PROCESS_CPUTIME_ID | o             | mruby's own value, not the platform's    |
| Process::CLOCK_THREAD_CPUTIME_ID  | o             | mruby's own value, not the platform's    |
| Process::Status#pid               | o             |                                          |
| Process::Status#to_i              | o             | no `#to_int`; mruby converts nothing     |
| Process::Status#exited?           | o             |                                          |
| Process::Status#exitstatus        | o             |                                          |
| Process::Status#signaled?         | o             |                                          |
| Process::Status#termsig           | o             |                                          |
| Process::Status#stopped?          | o             |                                          |
| Process::Status#stopsig           | o             |                                          |
| Process::Status#coredump?         | o             |                                          |
| Process::Status#success?          | o             |                                          |
| Process::Status#to_s              | o             |                                          |
| Process::Status#inspect           | o             |                                          |
| Process::Status#==                | o             | the raw status decides, not the pid      |
| Process.fork                      |               | inherently non-portable; separate change |
| Process.spawn                     |               | separate change                          |
| Process.exec                      |               | separate change                          |
| Process.exit, .exit!              |               | see mruby-exit                           |
| Process.uid, .gid, ...            |               | separate change                          |
| Process.getpgrp, ...              |               | separate change                          |

## Architecture

`mruby-process` and `mruby-io` are independent sibling gems. Neither needs the
other to provide its own feature set:

```text
             mruby
               |
       +-------+-------+
       |               |
       v               v
   mruby-io       mruby-process ----> mruby-signal
       |               |                   |
       v               v                   v
    io_hal         process_hal         signal_hal
       |               |                   |
   +---+---+       +---+---+           +---+---+
 posix   win     posix   win         posix   win
```

`IO.popen` is the one place the two capabilities meet, and it is served by
`mruby-io`'s own private spawn/wait primitives rather than by anything here.
There is no dependency in either direction; `mrbgem.rake` names `mruby-io` only
as a _test_ dependency, because waiting on a child process is only testable
with a child, and `IO.popen` is how this build makes one. `mruby-errno` and
`mruby-metaprog` are test dependencies for the same kind of reason: a gem's
tests run in a state holding its dependency closure and nothing else, so
naming an `Errno` class or asking an object for its `instance_variables` means
asking for the gem that defines them.

`mruby-time` is not a dependency either, although `Process.clock_gettime`
and `Time.now` both end up asking the host what time it is. The two gems ask
the same question of the same OS and get the same answer, so there is no
table here that could drift out of step with one there — which is what makes
the `mruby-signal` dependency below worth its edge and this one not. The
dependency would also point the wrong way through the gemboxes: this gem is
in `stdlib-io`, `mruby-time` is in `stdlib-ext`, and depending on it would
pull a `Time` class into every build that only asked for I/O. Two of the four
clocks are CPU time this process and this thread have spent, which is not
something `Time` has an opinion about in any case. If `mruby-time` ever grows
a HAL of its own, the wall clock is the one reading the two gems could come
to share, and moving to it would be that gem's change to make.

`mruby-signal` is a real dependency rather than a test one. `Process.kill`
takes a signal by name and `Process::Status#to_s` spells one out, so both need
the platform's signal table; that table is `mruby-signal`'s, and this gem
reaches it through `signal_hal.h` rather than keeping a second copy that could
drift from the first. Nothing runs the other way: `mruby-signal` needs nothing
from here.

### The HAL boundary

`include/process_hal.h` declares platform-neutral primitives. The port under
`ports/<name>/` implements them; a gem named `hal-process-<conf>` may supply
them instead, in which case the bundled ports are dropped from the build.

The HAL answers OS-level facts and performs OS-level operations:

- `mrb_hal_process_pid()`, `mrb_hal_process_ppid()` — the native process
  identity, widened to `mrb_int`.
- `mrb_hal_process_waitpid()` — translates the `MRB_PROCESS_WAIT_*` bits into
  native wait options and waits.
- `mrb_hal_process_kill()` — delivers a signal.
- `mrb_hal_process_status_decode()` — reads a native wait status into
  `mrb_process_status`.
- `mrb_hal_process_clock_gettime()`, `mrb_hal_process_clock_getres()` — read
  one of the `mrb_process_clock_id` clocks, and say how finely it can be
  read, both as whole seconds and nanoseconds.

What a unit a reading is wanted in is not among them either. A port always
reports the same two numbers and is asked nothing about `:float_second`,
`:nanosecond` or what a build without `Float` should do about the first of
them; see the design decisions below.

What a signal is _called_ is not among them: `mruby-signal`'s
`mrb_hal_signal_number()` and `mrb_hal_signal_name()` answer that, and both
`Process.kill` and `Process::Status#to_s` call them directly.

The common sources under `src/` implement everything Ruby promises: the module
and class definitions, argument shapes and conversions, `Process.waitpid`
return semantics, `$?` and `$$`, the `Process::WNOHANG` / `Process::WUNTRACED`
constants, and every `Process::Status` method.

No POSIX type or macro — `pid_t`, `WIFEXITED`, `WEXITSTATUS`, `SIGTERM`,
`WNOHANG` — appears above the HAL, and the HAL knows nothing of `$?`, `$$`,
blocks or `Process::Status`.

### Process::Status and mruby-io

`mruby-io` sets `$?` after an `IO.popen` stream closes by building a status
when the class happens to be defined, and falling back to a plain Integer when
it is not. `Process::Status.new` is undefined, as it is in CRuby, so what it
builds one with is `mrb_obj_new()`: the instance is allocated and handed to
`#initialize`, which takes the pid and the raw platform status. That is the
same path `Process.waitpid` takes here, and a status decodes itself through
the same HAL whichever way it was built, so one `mruby-io` produced reads
exactly like one this gem reaped.

A `Process::Status` stores only the pid and the raw platform status, and asks
the HAL afresh for every question about it. Nothing above the HAL ever holds a
decoded copy that could disagree with the platform.

The tests exercise that seam on POSIX only. On Windows, `mruby-io` hands out a
process **handle** as `IO#pid` rather than a process ID, and its `IO.popen`
sets `$?` through a branch that never fires, so there is nothing there to give
`Process.waitpid` yet. Both are `mruby-io`'s to fix, and that cleanup is
kept separate from adding this gem.

### Design decisions

- **`mrb_hal_process_waitpid()` returns a raw status, decoded separately.**
  `Process.waitpid` itself returns the pid it reaped and publishes the status
  through `$?`. The decoding is a step of its own because `Process::Status`
  must be able to decode a status that arrived from somewhere else — that is
  what the `mruby-io` integration is — so it has to stand alone either way.
  Having the wait return a decoded struct as well would be a second path to
  keep in step with the first.
- **`raw_status` is permanent, not a compatibility detail.** It is what
  `Process::Status#to_i` returns, and it is the only thing a status needs to
  store to answer everything else.
- **Windows exposes the signals it can honour.** `KILL` and `TERM` are
  delivered as `TerminateProcess()`, and signal 0 asks whether the process can
  be opened. Other names resolve to a number, since `mruby-signal`'s table says
  what a name stands for and this port says only what can be delivered, but
  sending one fails with `ENOSYS`.
- **Unsupported operations fail through `errno`.** A port sets `ENOSYS` and the
  common layer raises through `mrb_sys_fail()`, which becomes `Errno::ENOSYS`
  when `mruby-errno` is in the build. Methods are never conditionally absent,
  so a program can be written once and told at the call site what this platform
  will not do.
- **`Process::Status.new` is undefined.** A status reports what happened to a
  process, so one written by hand reports nothing, and CRuby says so by
  undefining `new` on the class. What the
  `mruby-io` seam needs is not a public constructor but a way to build the
  object from C, and `mrb_obj_new()` allocates and initializes without asking
  the class for `new`. That is also why `MRB_UNDEF_ALLOCATOR()` is not set
  beside the undefinition, as `Data`, `Complex` and `Binding` set it:
  `mrb_obj_new()` goes through the allocator, so marking it undefined would
  close the seam along with the constructor. CRuby leaves its allocator alone
  too, so `Process::Status.allocate` answers there with an uninitialized
  status; here such an instance is answered with
  `RuntimeError: uninitialized Process::Status` rather than read past, so
  `allocate` gives nothing away that was not reachable before.
- **A `Process::Status` is frozen once built.** What a process did is over by
  the time there is a status for it, and the pid and the raw status set at
  construction are what every other question is read back from. Freezing says
  so, and keeps the two from being rewritten under the answers. CRuby freezes
  the status it leaves in `$?` for the same reason. An instance of a subclass
  is left unfrozen: it is still being built when `#initialize` returns, and
  every status this gem and `mruby-io` publish is a `Process::Status` itself.
- **A wait flag no port stands for is refused before a port sees it.** The
  flags are mruby's own bits, and a port reads the ones it knows and can say
  nothing about the rest, so `MRB_PROCESS_WAIT_FLAGS` names every bit a wait
  may carry and anything else is `EINVAL` in the common layer. Leaving the
  check to the ports would leave the answer to each of them.
- **A clock is named by mruby's own number, as a wait flag is.**
  `CLOCK_MONOTONIC` is 1 on Linux, 6 on macOS and 4 on FreeBSD, and Windows
  has no `clockid_t` to give it a number at all, so borrowing the platform's
  numbers would leave `Process::CLOCK_MONOTONIC` naming a different clock on
  each port. The four ids are `mrb_process_clock_id`, every constant is
  defined on every platform, and a number that names none of them is refused
  in the common layer rather than by each port — with `Errno::EINVAL`, which
  is what a platform's own call answers for a clock it does not have and what
  CRuby raises here. A platform that has no such clock answers the same way,
  so a program is told at the call site rather than finding the constant
  missing.
- **A reading crosses the HAL as seconds and nanoseconds, never as a Float.**
  `MRB_NO_FLOAT` builds take `stdlib-io`, and a HAL typed in `mrb_float`
  would not compile for one. A double holds 53 bits where a wall-clock
  nanosecond needs about 61, so a port that divided would throw away what
  `:nanosecond` was going to be asked for, and a single count of nanoseconds
  runs out in 2262. Splitting the reading leaves every unit reachable from
  the same two numbers, and leaves the unit itself entirely above the HAL.
- **The two fields are `int64_t`, the one place in the HAL that is not an
  `mrb_int`.** How large a reading is, is the platform's business; how much
  of it this build's Integer can carry is mruby's, and is settled in the
  common layer, where a bigint can be built and `RangeError` can be said.
  With `mrb_int` there instead, a build with a 32-bit one would have every
  port refusing the wall clock from 2038 on, and refusing it through `errno`,
  which cannot say that the platform was fine and the Integer was not — the
  same objection this gem raises to letting a port report an oversized pid.
  It would also leave both ports narrowing a value the common layer narrows
  again. `int64_t` is no more a platform type than `mrb_int` is: `time_t`,
  `clockid_t` and `FILETIME` still stop at the port.
- **What a build without `Float` does about a float unit is said at the call
  site.** The three float units raise `NotImplementedError` there rather than
  the methods disappearing or the default unit quietly becoming an integer
  one; the four integer units are untouched, and `:nanosecond` says
  everything `:float_second` would have. As in CRuby, `NotImplementedError`
  is a `ScriptError` and not caught by a bare `rescue`. `mruby-time` makes
  the other choice for `Time#to_f`, which is a method that could only ever
  answer in a Float; a unit is an argument, and refusing one argument is not
  a reason to withdraw a method that answers six others.
- **A reading too large for the build's Integer becomes a bigint, or a
  RangeError where there are none.** A 32-bit `mrb_int` carries a wall clock
  in seconds and not in milliseconds, and an `int64_t` carries one in
  nanoseconds only until 2262. Where the build has `mruby-bigint` the answer
  is a bigint, which is the Integer CRuby answers with, and that holds for a
  reading the `int64_t` sum itself cannot hold as much as for one only the
  Integer cannot: where the arithmetic in between runs out is a fact about
  that arithmetic, not about the reading or about the Integer being asked
  for. Where the build has no bigints, what is wrong with the value is its
  size, so `RangeError` says so, as it does for an oversized pid below. The
  two ends are pinned by tests that hand the conversion readings no clock
  reaches, rather than left to a clock to arrive at in 2262.
- **`Process.clock_getres` arrives with `Process.clock_gettime`.** A reading
  says little without it — a monotonic clock that moves every 15ms and one
  that moves every 100ns are read the same way — and adding it later would
  mean revising every port a second time for the same seam. It takes one
  unit the reading does not, `:hertz`, which is one over what
  `:float_second` says: a resolution can be given as a rate, and a moment
  cannot, which is where CRuby draws the line too. It is a Float like the
  other Float units, so a build without one answers it the same way.
- **A resolution describes how the clock is read, not the clock.** Where a
  platform states the interval a clock advances on, that is what
  `Process.clock_getres` answers; where it does not, the answer is the
  granularity of the call the reading came out of, which is the finest two
  readings can differ by. So a resolution here is a bound on what a caller
  can distinguish, and a clock may well move more coarsely than it. The
  alternative would be for a port to refuse wherever a platform declines to
  commit to a true period, which is nearly every clock on every platform:
  POSIX's own `clock_getres(2)` is a statement of the same kind, with Linux
  answering 1ns for clocks whose readings move in tens or hundreds of them,
  and CRuby reports the granularity of whatever it emulated a clock out of —
  a microsecond for its `gettimeofday(2)`-based wall clock, a tick for the
  `times(2)`-based CPU one. Every clock a port can read therefore has a
  resolution to answer with, and `Process.clock_getres` fails only where
  `Process.clock_gettime` would.
- **A pid, a signal or a raw status too large for the platform is refused in
  the common layer.** What is wrong with such a value is its size, and size is
  not something a port can report: the HAL answers with an `errno`, which has
  no spelling for "that was never a pid" and would have to borrow one that
  means something else, leaving `Errno::ESRCH` to stand both for "no process
  there" and for "that was never a process id". So the value is refused where
  `RangeError` can be said, which is also what CRuby raises for it and how
  `mruby-socket` checks the `int` fields of a getaddrinfo hint. The ports keep
  their own range guards, so that each is correct on its own.
- **A raw status is checked for the same reason, and only where it is
  narrowed.** A port reads one as the `int` the platform reported, so a wider
  value would leave `#to_i` giving back what was passed in while `#exited?`
  and the rest answered from the low half of it. Nothing this gem produces can
  reach that: `Process.waitpid` carries a platform status, and `mruby-io`
  hands `IO#close` the same `int` on both platforms. The check turns away only
  a value written by hand. The pid a status carries is not checked, because it
  is handed back whole rather than narrowed.

## Deviations from CRuby

- `Process.kill` does not name a process group through the signal yet. A
  negative signal number, or a name written with a leading `-`, asks CRuby for
  the group of each pid given; here it raises `ArgumentError` rather than
  quietly signalling the process instead. The `pid` selectors are untouched by
  that: a positive number names one process, and 0, -1, and numbers below -1
  reach the platform as they are written, which on POSIX is `kill(2)`'s
  caller's process group, every process the caller may signal, and the group
  whose ID is `-pid`.
- A clock can be named by the Symbol its constant is named with, as in
  CRuby: `Process.clock_gettime(:CLOCK_MONOTONIC)` reads what
  `Process::CLOCK_MONOTONIC` numbers. That is worth more here than there,
  the numbers being mruby's own rather than the platform's. The names CRuby
  knows beyond these four are not here: the platform-specific clocks, and
  the ways it emulates one the host lacks
  (`:GETTIMEOFDAY_BASED_CLOCK_REALTIME` and the rest), which a port that
  either has a clock or says it has not gives nothing to pick out. Such a
  name raises `Errno::EINVAL`, which is what CRuby raises for a name it does
  not know.
- The clocks are the four every platform can be expected to answer for. The
  platform-specific ones CRuby also defines where it finds them —
  `CLOCK_MONOTONIC_RAW`, `CLOCK_BOOTTIME` and the rest — are not here; each
  would need a name of mruby's own and an answer from every port.
- On Windows each clock is read by one Win32 call, and its resolution is the
  granularity of that call. The wall clock is
  `GetSystemTimePreciseAsFileTime()` where there is one and the coarse
  `GetSystemTimeAsFileTime()` on Windows 7 and older, and which of the two
  this Windows has decides both how the clock is read and how finely. The
  precise call writes a `FILETIME`, so its resolution is one 100ns tick;
  the coarse call does not interpolate between clock interrupts, so
  there it is the interval between two of them, from
  `NtQueryTimerResolution()` where that can be reached and from
  `GetSystemTimeAdjustment()`'s `TimeIncrement` otherwise. The two CPU clocks
  are `GetProcessTimes()` and `GetThreadTimes()`, both `FILETIME`s again, so
  both answer one tick. Windows documents no rate at which any of these
  clocks advances, and this port states none: a tick is how finely a reading
  written as a `FILETIME` can be told from the next, which is what a
  resolution is here.
- On the rare POSIX host without `clock_gettime(2)`, the wall clock reads
  through `gettimeofday(2)`, and its resolution is the microsecond that call
  writes its answer in — the same number CRuby reports for its own
  `gettimeofday(2)`-based clock.
- On Windows a wait status is the child's exit code and nothing more, so a
  status there always reads as exited — even for a process this gem terminated.
- On Windows, `Process.waitpid` fails for every process: `ENOSYS` for a pid of
  -1, which no handle stands for, and `ECHILD` for a specific one. Win32 names
  a process to wait on by handle, and a handle comes from opening a process ID,
  which succeeds for any process the caller may open rather than only for its
  own children. Waiting on such a handle would report a stranger's exit code as
  a child's and publish it as `$?`, so the wait says it has no such child
  instead. A port learns which processes are its children by creating them, so
  this becomes answerable when `Process.spawn` is added and not before.

## Adding a port

Create `ports/<name>/process_hal.c` implementing every function in
`include/process_hal.h`, then build with `conf.ports :<name>, :posix` so gems
without a `<name>` port fall back. A port that cannot do something should set
`errno` to `ENOSYS` and return the documented failure value rather than
pretending to succeed.
