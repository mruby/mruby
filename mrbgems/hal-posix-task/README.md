# hal-posix-task

POSIX Hardware Abstraction Layer (HAL) implementation for mruby-task.

## Description

Provides timer and interrupt support for the mruby-task cooperative scheduler on POSIX-compliant platforms. Uses `SIGALRM` and `setitimer()` for periodic timer ticks, and `sigprocmask()` for interrupt protection.

## Supported Platforms

- Linux
- macOS
- BSD (FreeBSD, OpenBSD, NetBSD)
- Other POSIX-compliant Unix systems

## Requirements

- POSIX-compliant operating system
- Signal support (`SIGALRM`, `sigaction`, `sigprocmask`)
- Timer support (`setitimer`, `ITIMER_REAL`)

## Usage

### Explicit HAL Selection (Recommended)

```ruby
MRuby::Build.new do |conf|
  # ... other configuration ...

  # Specify POSIX HAL - automatically brings in mruby-task
  conf.gem core: 'hal-posix-task'
end
```

### Auto-detection (Development)

```ruby
MRuby::Build.new do |conf|
  # ... other configuration ...

  # Auto-detects and selects hal-posix-task on POSIX platforms
  conf.gem core: 'mruby-task'
end
```

## Implementation Details

### Timer Mechanism

- Uses `setitimer(ITIMER_REAL, ...)` to generate periodic `SIGALRM` signals
- Timer interval configured by `MRB_TICK_UNIT` (default: 4ms)
- Signal handler calls `mrb_tick()` for all registered VM instances

### Interrupt Protection

- Critical sections protected using `sigprocmask()` to block `SIGALRM`
- Prevents race conditions during task queue modifications
- Supports nested critical sections through signal masking

### Multi-VM Support

- Supports up to `MRB_TASK_MAX_VMS` concurrent mruby VM instances (default: 8)
- Single shared timer ticks all registered VMs
- Per-VM task counters optimize timer usage (timer disabled when idle)

### Timer Optimization

The implementation dynamically enables/disables the timer based on task state:

- **Timer enabled** when: Multiple ready tasks OR any waiting tasks exist
- **Timer disabled** when: Single task or all tasks dormant/suspended
- Reduces CPU usage and power consumption when scheduler is idle

## Configuration

Override these macros in your build config if needed:

```ruby
conf.gem core: 'hal-posix-task' do |spec|
  # Custom tick interval (10ms instead of default 4ms)
  spec.build.defines << 'MRB_TICK_UNIT=10'

  # Custom timeslice (5 ticks instead of default 3)
  spec.build.defines << 'MRB_TIMESLICE_TICK_COUNT=5'

  # More concurrent VMs (16 instead of default 8)
  spec.build.defines << 'MRB_TASK_MAX_VMS=16'
end
```

## Known Limitations

- `SIGALRM` conflicts with other code using the same signal
- Timer resolution limited by platform (typically 1-10ms)
- Signal delivery may be delayed under heavy system load
- Not suitable for hard real-time requirements

## See Also

- `mruby-task` - Core task scheduler
- `hal-win-task` - Windows HAL implementation
- Task scheduler documentation: `mrbgems/mruby-task/README.md`
