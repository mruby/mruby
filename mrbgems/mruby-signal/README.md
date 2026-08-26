# mruby-signal

`Signal` module for mruby.

## Installation

Add the line below to your build configuration.

```ruby
  conf.gem core: 'mruby-signal'
```

It is part of the `stdlib-io` gembox, so `default.gembox` and `full-core.gembox`
already include it. `mruby-process` depends on it, so a build that has
`Process.kill` has this gem whether or not it asked for it.

## Implemented methods

| method         | mruby-signal | memo                                        |
| -------------- | ------------ | ------------------------------------------- |
| Signal.signame | o            | `signame(0)` is `"EXIT"`, as in CRuby       |
| Signal.list    | o            | a fresh Hash each call                      |
| Signal.trap    |              | needs handler delivery in the VM, see below |
| Kernel#trap    |              | see `Signal.trap`                           |

## Architecture

The gem is one table read in two directions, so it splits the way the other
HAL gems do: `src/signal.c` says what Ruby promises, and the port under
`ports/<name>/` says what this platform has.

```text
             mruby
               |
               v
         mruby-signal
               |
               v
          signal_hal
               |
           +---+---+
         posix   win
```

`include/signal_hal.h` declares the platform-neutral primitives. The port under
`ports/<name>/` implements them; a gem named `hal-signal-<conf>` may supply
them instead, in which case the bundled ports are dropped from the build.

- `mrb_hal_signal_number()` / `mrb_hal_signal_name()` — map between a bare
  name such as `TERM` and the number this platform gives it.
- `mrb_hal_signal_count()` / `mrb_hal_signal_at()` — walk the table, in the
  order Ruby lists signals, which is what `Signal.list` is built from.

No platform macro — `SIGTERM`, `NSIG` — appears above the HAL, and the HAL
knows nothing of `Signal`, `EXIT` or `Process.kill`.

### mruby-process depends on this gem

`Process.kill(:TERM, pid)` takes a signal by name, and `Process::Status#to_s`
spells one out, so both need this table. Rather than keep a second copy of it,
`mruby-process` names `mruby-signal` as a dependency and calls the HAL
directly from C. The dependency runs one way only: nothing here needs
`Process`.

### Design decisions

- **`EXIT` is added above the HAL, not in the ports.** No platform numbers a
  signal 0 — `kill(2)` spends that number on asking whether a process can be
  signalled at all — so `EXIT` is a name Ruby gives rather than one a host
  reports. Putting it in `src/signal.c` gives every port the same answer for
  free, and keeps the ports to what `<signal.h>` actually defines.
- **`Process.kill` still refuses `EXIT`.** It resolves a name through
  `mrb_hal_signal_number()`, which does not know the name, so
  `Process.kill("EXIT", pid)` raises `ArgumentError` reading
  `unsupported signal 'SIGEXIT'` — the same error, for the same reason, as
  CRuby.
- **The signal list is Ruby's own, not a selection.** Every name Ruby knows is
  in the POSIX port, in Ruby's order, behind the guard that says whether the
  host has it. Taking the list rather than picking one keeps a name the host
  defines from being reported as unsupported, and keeping the order is what
  makes `Signal.signame` answer `ABRT` rather than `IOT` where a host spells
  one signal two ways.
- **An out-of-range number is nil, not an error.** `Signal.signame` compares in
  `mrb_int` and narrows nothing, so a number no signal has simply matches no
  entry. Only a value too large for an `mrb_int` at all is refused, and that is
  `mrb_get_args()` raising `RangeError` rather than a check of this gem's own.
- **`Signal.list` is rebuilt per call.** CRuby hands back a fresh Hash each
  time, so a program that mutates the result changes nothing; caching one here
  would make that mutation stick.

## Deviations from CRuby

- `Signal.trap` and `Kernel#trap` are not implemented. Setting a handler is not
  a table lookup: it needs a signal to be caught in a C handler, recorded, and
  the recorded flag noticed by the VM at a point where running Ruby is safe.
  That is a change to the VM rather than to a gem, so it is kept separate from
  adding the lookups.
- `Signal.signame` does not ask an object for `#to_int`. A Float is truncated,
  as CRuby truncates one, but an object answering only `#to_int` raises
  `TypeError` here where CRuby accepts it. That is how `mrb_get_args()` reads
  an Integer argument everywhere in mruby rather than a decision of this
  gem's; `mruby-process` records the same difference for
  `Process::Status#to_i`.
- On Windows the table is the seven names `<signal.h>` there defines, plus
  `KILL` at its conventional POSIX value. Windows has no signals between
  processes, so a name resolving here only means it can be spelled;
  `Process.kill` decides which of them can actually be delivered.

## Adding a port

Create `ports/<name>/signal_hal.c` implementing every function in
`include/signal_hal.h`, then build with `conf.ports :<name>, :posix` so gems
without a `<name>` port fall back. A platform with no signals at all is a
legitimate port: an empty table makes `Signal.list` hold `EXIT` alone and
`Signal.signame` answer nil for everything else.
