# hal-win-task

Windows Hardware Abstraction Layer (HAL) implementation for mruby-task.

## Description

Provides timer and interrupt support for the mruby-task cooperative scheduler on Windows platforms. Uses multimedia timer (`timeSetEvent`/`timeKillEvent`) for periodic timer ticks, and `CRITICAL_SECTION` for interrupt protection.

## Supported Platforms

- Windows 7 and later
- Windows Server 2008 R2 and later
- All versions with multimedia timer support

## Requirements

- Windows operating system
- Multimedia timer API (`winmm.lib`)
- Visual C++, MinGW, or compatible compiler

## Usage

### Explicit HAL Selection (Recommended)

```ruby
MRuby::Build.new do |conf|
  # ... other configuration ...

  # Specify Windows HAL - automatically brings in mruby-task
  conf.gem core: 'hal-win-task'
end
```

### Auto-detection (Development)

```ruby
MRuby::Build.new do |conf|
  # ... other configuration ...

  # Auto-detects and selects hal-win-task on Windows platforms
  conf.gem core: 'mruby-task'
end
```

## Implementation Details

### Timer Mechanism

- Uses Windows multimedia timer (`timeSetEvent`) for periodic callbacks
- Timer interval configured by `MRB_TICK_UNIT` (default: 4ms)
- `TIME_KILL_SYNCHRONOUS` flag ensures clean timer shutdown
- Requests 1ms timer resolution via `timeBeginPeriod(1)`

### Interrupt Protection

- Critical sections protected using `CRITICAL_SECTION` objects
- Prevents race conditions during task queue modifications
- `EnterCriticalSection`/`LeaveCriticalSection` used for mutual exclusion
- Supports nested critical sections (automatic lock counting)

### Multi-VM Support

- Supports up to `MRB_TASK_MAX_VMS` concurrent mruby VM instances (default: 8)
- Single shared timer ticks all registered VMs
- Per-VM task counters optimize timer usage (timer disabled when idle)
- Interlocked operations (`InterlockedIncrement`/`InterlockedDecrement`) for thread safety

### Timer Optimization

The implementation dynamically enables/disables the timer based on task state:

- **Timer enabled** when: Multiple ready tasks OR any waiting tasks exist
- **Timer disabled** when: Single task or all tasks dormant/suspended
- Reduces CPU usage and power consumption when scheduler is idle

## Configuration

Override these macros in your build config if needed:

```ruby
conf.gem core: 'hal-win-task' do |spec|
  # Custom tick interval (10ms instead of default 4ms)
  spec.build.defines << 'MRB_TICK_UNIT=10'

  # Custom timeslice (5 ticks instead of default 3)
  spec.build.defines << 'MRB_TIMESLICE_TICK_COUNT=5'

  # More concurrent VMs (16 instead of default 8)
  spec.build.defines << 'MRB_TASK_MAX_VMS=16'
end
```

## Known Limitations

- Timer resolution typically limited to 1-2ms even with `timeBeginPeriod(1)`
- Multimedia timers consume system resources (kernel timer objects)
- Timer callbacks execute in separate thread context (handled internally)
- Not suitable for hard real-time requirements
- May interfere with other multimedia applications requesting different timer resolutions

## See Also

- `mruby-task` - Core task scheduler
- `hal-posix-task` - POSIX/Unix HAL implementation
- Task scheduler documentation: `mrbgems/mruby-task/README.md`

## Build Notes

The `winmm` library is automatically linked by the gem specification. No additional linker configuration is needed.
