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

| method                     | mruby-process | memo                                     |
| -------------------------- | ------------- | ---------------------------------------- |
| Process.pid                | o             | also `$$`                                |
| Process.ppid               | o             |                                          |
| Process.kill               | o             | no negative-signal form yet, see below   |
| Process.waitpid            | o             | sets `$?`; POSIX only, see below         |
| Process::WNOHANG           | o             | mruby's own value, not the platform's    |
| Process::WUNTRACED         | o             | mruby's own value, not the platform's    |
| Process::Status#pid        | o             |                                          |
| Process::Status#to_i       | o             | also `#to_int`                           |
| Process::Status#exited?    | o             |                                          |
| Process::Status#exitstatus | o             |                                          |
| Process::Status#signaled?  | o             |                                          |
| Process::Status#termsig    | o             |                                          |
| Process::Status#stopped?   | o             |                                          |
| Process::Status#stopsig    | o             |                                          |
| Process::Status#coredump?  | o             |                                          |
| Process::Status#success?   | o             |                                          |
| Process::Status#to_s       | o             |                                          |
| Process::Status#inspect    | o             |                                          |
| Process::Status#==         | o             | the raw status decides, not the pid      |
| Process.fork               |               | inherently non-portable; separate change |
| Process.spawn              |               | separate change                          |
| Process.exec               |               | separate change                          |
| Process.exit, .exit!       |               | see mruby-exit                           |
| Process.wait, .wait2       |               | separate change                          |
| Process.uid, .gid, ...     |               | separate change                          |
| Process.getpgrp, ...       |               | separate change                          |

## Architecture

`mruby-process` and `mruby-io` are independent sibling gems. Neither needs the
other to provide its own feature set:

```text
             mruby
               |
       +-------+-------+
       |               |
       v               v
   mruby-io       mruby-process
       |               |
       v               v
    io_hal         process_hal
       |               |
   +---+---+       +---+---+
 posix   win     posix   win
```

`IO.popen` is the one place the two capabilities meet, and it is served by
`mruby-io`'s own private spawn/wait primitives rather than by anything here.
There is no dependency in either direction; `mrbgem.rake` names `mruby-io` only
as a _test_ dependency, because waiting on a child process is only testable
with a child, and `IO.popen` is how this build makes one.

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
- `mrb_hal_process_signal_number()` / `_signal_name()` — map between a bare
  name such as `TERM` and the number this platform gives it.
- `mrb_hal_process_status_decode()` — reads a native wait status into
  `mrb_process_status`.

The common sources under `src/` implement everything Ruby promises: the module
and class definitions, argument shapes and conversions, `Process.waitpid`
return semantics, `$?` and `$$`, the `Process::WNOHANG` / `Process::WUNTRACED`
constants, and every `Process::Status` method.

No POSIX type or macro — `pid_t`, `WIFEXITED`, `WEXITSTATUS`, `SIGTERM`,
`WNOHANG` — appears above the HAL, and the HAL knows nothing of `$?`, `$$`,
blocks or `Process::Status`.

### Process::Status and mruby-io

`mruby-io` sets `$?` after an `IO.popen` stream closes by calling
`Process::Status.new(pid, raw_status)` when the class happens to be defined,
and falling back to a plain Integer when it is not. That soft integration keeps
working unchanged: `Process::Status.new(pid, raw_status)` is a supported way to
build a status, and the status decodes itself through the same HAL that
`Process.waitpid` uses, so a status `mruby-io` produced reads exactly like one
this gem reaped.

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
  be opened. Other names resolve to a number (so `Process::Status#to_s` can
  spell them) but fail with `ENOSYS` when sent.
- **Unsupported operations fail through `errno`.** A port sets `ENOSYS` and the
  common layer raises through `mrb_sys_fail()`, which becomes `Errno::ENOSYS`
  when `mruby-errno` is in the build. Methods are never conditionally absent,
  so a program can be written once and told at the call site what this platform
  will not do.
- **`Process::Status.new(pid, raw_status)` stays public.** Making it private
  would break the `mruby-io` path it exists for.
- **A wait flag no port stands for is refused before a port sees it.** The
  flags are mruby's own bits, and a port reads the ones it knows and can say
  nothing about the rest, so `MRB_PROCESS_WAIT_FLAGS` names every bit a wait
  may carry and anything else is `EINVAL` in the common layer. Leaving the
  check to the ports would leave the answer to each of them.
- **A pid or a signal too large for the platform is refused in the common
  layer.** What is wrong with such a value is its size, and size is not
  something a port can report: the HAL answers with an `errno`, which has no
  spelling for "that was never a pid" and would have to borrow one that means
  something else, leaving `Errno::ESRCH` to stand both for "no process there"
  and for "that was never a process id". So the value is refused where
  `RangeError` can be said, which is also what CRuby raises for it and how
  `mruby-socket` checks the `int` fields of a getaddrinfo hint. The ports keep
  their own range guards, so that each is correct on its own.
- **The POSIX signal list is Ruby's own, not a selection.** Every name Ruby
  knows is there, in Ruby's order, behind the guard that says whether the host
  has it. Taking the list rather than picking one keeps a name the host defines
  from being reported as unsupported, and keeping the order is what makes the
  reverse lookup answer `ABRT` rather than `IOT` where a host spells one signal
  two ways. `EXIT` is the one name left out, for the reason below.

## Deviations from CRuby

- `Process.kill` does not name a process group through the signal yet. A
  negative signal number, or a name written with a leading `-`, asks CRuby for
  the group of each pid given; here it raises `ArgumentError` rather than
  quietly signalling the process instead. The `pid` selectors are untouched by
  that: a positive number names one process, and 0, -1, and numbers below -1
  reach the platform as they are written, which on POSIX is `kill(2)`'s
  caller's process group, every process the caller may signal, and the group
  whose ID is `-pid`.
- `Process::Status._signame` and `Process::Status._signal_description` are
  internal helpers `Process::Status#to_s` uses to spell a signal number out.
  They are not a general signal API; `Signal` and `Signal.signame` are not
  implemented. `_signame(0)` is nil rather than `"EXIT"`, since a status never
  carries 0 as a signal and `Process.kill` does not take the name either.
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
