# mruby-task

mruby-task is an mrbgem that provides cooperative multitasking with preemptive
scheduling for mruby. It enables concurrent execution of multiple tasks within
a single mruby VM instance using a priority-based scheduler with tick-based
time slicing.

## Purpose

The primary purpose of `mruby-task` is to enable mruby applications to:

- Execute multiple tasks concurrently within a single VM.
- Schedule tasks based on priority (0-255, where 0 is highest priority).
- Provide cooperative yielding with `Task.pass`.
- Support preemptive scheduling via timer-based interrupts.
- Synchronize tasks using `sleep` and `join` operations.
- Suspend and resume tasks programmatically.

## Architecture

### Task Scheduler

The scheduler uses four priority-sorted queues:

- **DORMANT**: Tasks that have not started or have finished execution.
- **READY**: Tasks ready to run, ordered by priority.
- **WAITING**: Tasks waiting for sleep timeout or join completion.
- **SUSPENDED**: Tasks manually suspended via `#suspend`.

### Tick-Based Preemption

A platform-specific timer generates periodic ticks (default: 4ms). Each task
receives a timeslice (default: 3 ticks = 12ms) before being preempted. The
scheduler automatically switches to the next ready task when:

- A task's timeslice expires.
- A task calls `sleep`, `Task.pass`, or `join`.
- A task finishes execution.

## Functionality

### Creating Tasks

Tasks are created with `Task.new` and begin execution immediately:

```ruby
# Create a task with default priority (128)
task = Task.new do
  puts "Hello from task!"
  sleep 1
  puts "Task resumed"
end

# Create a named task with custom priority
task = Task.new(name: "worker", priority: 64) do
  loop do
    process_data
    Task.pass  # Yield to other tasks
  end
end

# Start the scheduler (blocks until all tasks complete or idle)
Task.run
```

### Task Class Methods

- **`Task.new(name: nil, priority: 128) { block }`**: Creates and starts a new task. Lower priority values run first (0 is highest priority). The `name` parameter must be a String if provided. The `priority` must be an Integer between 0-255.

  ```ruby
  task = Task.new(name: "background", priority: 200) do
    # Task code here
  end
  ```

- **`Task.current`**: Returns the currently executing task.

  ```ruby
  current = Task.current
  puts "Running: #{current.name}"
  ```

- **`Task.list`**: Returns an array of all tasks (including dormant tasks).

  ```ruby
  Task.list.each do |task|
    puts "#{task.name}: #{task.status}"
  end
  ```

- **`Task.pass`**: Cooperatively yields execution to other ready tasks.

  ```ruby
  loop do
    do_work
    Task.pass  # Let other tasks run
  end
  ```

- **`Task.get(name)`**: Finds a task by name. Returns `nil` if not found.

  ```ruby
  worker = Task.get("worker")
  worker.suspend if worker
  ```

- **`Task.stat`**: Returns a hash containing scheduler statistics:
  - `:tick` (Integer): Current tick count
  - `:wakeup_tick` (Integer): Next scheduled wakeup tick
  - `:dormant`, `:ready`, `:waiting`, `:suspended`: Each is a hash with:
    - `:count` (Integer): Number of tasks in this queue
    - `:tasks` (Array): Array of task objects in this queue

  ```ruby
  stats = Task.stat
  puts "Tick: #{stats[:tick]}"
  puts "Ready tasks: #{stats[:ready][:count]}"
  stats[:ready][:tasks].each { |t| puts t.name }
  ```

- **`Task.run`**: Starts the scheduler main loop. Blocks until no tasks remain ready or waiting.

  ```ruby
  Task.new { do_async_work }
  Task.run  # Run scheduler until tasks complete
  ```

### Task Instance Methods

- **`#status`**: Returns the task status as a symbol (`:DORMANT`, `:READY`, `:RUNNING`, `:WAITING`, `:SUSPENDED`).

  ```ruby
  puts task.status  # => :READY
  ```

- **`#name`** / **`#name=`**: Get or set the task name. Returns `"(noname)"` for unnamed tasks. Note: `name=` accepts any value, but `Task.new` requires a String.

  ```ruby
  task.name = "worker-1"
  puts task.name  # => "worker-1"

  task = Task.new { }
  puts task.name  # => "(noname)"
  ```

- **`#priority`** / **`#priority=`**: Get or set the task priority (0-255). Changing priority requeues the task.

  ```ruby
  task.priority = 100  # Lower priority
  ```

- **`#suspend`**: Suspends the task, moving it to the SUSPENDED queue. The task will not run until `#resume` is called.

  ```ruby
  task.suspend
  # Later...
  task.resume
  ```

- **`#resume`**: Resumes a suspended task, moving it to the READY queue.

  ```ruby
  task.resume
  ```

- **`#terminate`**: Terminates the task immediately, moving it to DORMANT state.

  ```ruby
  task.terminate
  ```

- **`#join`**: Blocks the current task until the target task completes.

  ```ruby
  worker = Task.new { do_long_operation }
  worker.join  # Wait for completion
  puts "Worker finished"
  ```

### Kernel Methods (Sleep)

The task scheduler provides task-aware sleep methods that cooperatively yield
to other tasks:

- **`sleep(seconds)`**: Sleeps for the specified duration. Accepts integers or floats (when `MRB_NO_FLOAT` is not defined).

  ```ruby
  sleep 1      # Sleep for 1 second
  sleep 0.5    # Sleep for 500ms (with float support)
  sleep        # Sleep indefinitely (no arguments)
  ```

- **`usleep(microseconds)`**: Sleeps for the specified number of microseconds.

  ```ruby
  usleep 500000  # Sleep for 500ms
  usleep 1000    # Sleep for 1ms
  ```

- **`sleep_ms(milliseconds)`**: Sleeps for the specified number of milliseconds.

  ```ruby
  sleep_ms 100  # Sleep for 100ms
  ```

**Note**: These methods override `mruby-sleep` when both gems are present. They
provide task-aware cooperative sleep when called from within a task, or
blocking sleep when called outside task context.

## Configuration

### Build Configuration

Enable the task scheduler by including the gem in your build config:

```ruby
MRuby::Build.new do |conf|
  # ... other configuration ...

  conf.gem :core => 'mruby-task'

  # ... other gems ...
end
```

This automatically defines `MRB_USE_TASK_SCHEDULER`.

### Timing Configuration

Timing parameters can be configured via C defines:

```c
#define MRB_TICK_UNIT 4              // Tick period in milliseconds (default: 4ms)
#define MRB_TIMESLICE_TICK_COUNT 3   // Ticks per timeslice (default: 3)
```

Default timeslice: `MRB_TICK_UNIT * MRB_TIMESLICE_TICK_COUNT = 12ms`

### Stack Configuration

Task stack and call info sizes:

```c
#define TASK_STACK_INIT_SIZE 64   // Initial stack entries (default: 64)
#define TASK_CI_INIT_SIZE 8       // Initial callinfo entries (default: 8)
```

These grow automatically as needed, similar to Fiber.

## Platform Requirements

### HAL (Hardware Abstraction Layer)

The task scheduler requires platform-specific timer and interrupt support via four HAL functions:

```c
void mrb_task_hal_init(mrb_state *mrb);      // Initialize timer
void mrb_task_enable_irq(void);               // Enable timer interrupts
void mrb_task_disable_irq(void);              // Disable timer interrupts
void mrb_task_hal_idle_cpu(mrb_state *mrb);   // Idle when no tasks ready
```

### POSIX Platform

On POSIX systems (Linux, macOS, BSD), the implementation uses:

- `SIGALRM` signal for timer interrupts
- `setitimer()` for periodic tick generation
- `sigprocmask()` for interrupt enable/disable
- `SA_RESTART` flag to prevent `EINTR` on system calls

### Other Platforms

For embedded or non-POSIX platforms, you must implement the HAL functions. See
the POSIX implementation in `src/task.c` as a reference.

Example for a hypothetical embedded platform:

```c
void mrb_task_hal_init(mrb_state *mrb) {
  // Setup hardware timer to call mrb_tick() every MRB_TICK_UNIT ms
  hardware_timer_init(MRB_TICK_UNIT, timer_irq_handler);
}

void timer_irq_handler(void) {
  mrb_tick(global_mrb);  // Must be called from timer interrupt
}
```

## Examples

### Basic Multitasking

```ruby
Task.new(name: "task1") do
  3.times do |i|
    puts "Task 1: #{i}"
    sleep 0.1
  end
end

Task.new(name: "task2") do
  3.times do |i|
    puts "Task 2: #{i}"
    sleep 0.1
  end
end

Task.run  # Run until both tasks complete
```

### Priority Scheduling

```ruby
# High priority task (runs first)
Task.new(priority: 0) do
  puts "High priority"
  sleep 0.1
end

# Low priority task (runs after high priority yields)
Task.new(priority: 255) do
  puts "Low priority"
end

Task.run
```

### Cooperative Yielding

```ruby
Task.new(name: "cooperative") do
  loop do
    do_some_work
    Task.pass  # Yield to other tasks
    break if done?
  end
end

Task.new(name: "other") do
  do_other_work
end

Task.run
```

### Task Synchronization

```ruby
worker = Task.new(name: "worker") do
  puts "Working..."
  sleep 1
  puts "Work done"
  42  # Return value
end

Task.new(name: "main") do
  puts "Waiting for worker..."
  worker.join
  puts "Worker completed!"
end

Task.run
```

### Task Control

```ruby
task = Task.new do
  loop do
    puts "Running..."
    sleep 0.5
  end
end

# From another task or after Task.run returns:
task.suspend     # Pause execution
sleep 1
task.resume      # Resume execution
sleep 1
task.terminate   # Stop permanently
```

## Limitations and Compatibility

### Relationship with Fiber

Tasks and Fibers both use `mrb_context` but are **not compatible**:

- Tasks are scheduled automatically by the preemptive scheduler.
- Fibers require explicit `Fiber.yield` and `resume` calls.
- Do not mix Tasks and Fibers in the same application.

### Relationship with mruby-sleep

When `mruby-task` is enabled:

- The `sleep`, `usleep`, and `sleep_ms` methods are task-aware.
- Inside a task, they cooperatively yield to the scheduler.
- Outside a task (or when scheduler is idle), they block.
- `mruby-sleep` should be excluded from your build when using `mruby-task`.

### Thread Safety

The task scheduler is **not thread-safe**. All tasks run in a single OS thread.
For multi-core concurrency, use OS threads with separate mruby VMs per thread.

### Exceptions

Uncaught exceptions in a task will terminate that task but not affect other
tasks. The exception is not propagated to the scheduler.

### GC Integration

Task contexts are registered with the garbage collector. Tasks and their
stacks/callinfo are properly marked and freed.

## Testing

The gem includes tests that verify:

- Task creation and execution
- Priority scheduling
- Sleep and wakeup
- Join synchronization
- Suspend and resume
- Task.pass cooperative yielding

Run tests with:

```bash
rake CONFIG=host-debug test:lib
```

## Implementation Details

### Task States

Each task can be in one of five states:

- `DORMANT (0x00)`: Not started or finished
- `READY (0x02)`: Ready to run
- `RUNNING (0x03)`: Currently executing
- `WAITING (0x04)`: Waiting (sleep, join, mutex)
- `SUSPENDED (0x08)`: Manually suspended

### Wait Reasons

When a task is in WAITING state, the reason indicates why:

- `NONE (0x00)`: No specific reason
- `SLEEP (0x01)`: Sleeping for time
- `MUTEX (0x02)`: Waiting for mutex (reserved, not yet implemented)
- `JOIN (0x04)`: Waiting for another task

### Scheduler Algorithm

1. Get the highest-priority task from the READY queue
2. Set task status to RUNNING and switch context (`mrb->c = &task->c`)
3. Execute task via `mrb_vm_exec()` until:
   - Task yields (sleep, pass, join)
   - Timeslice expires (preemption)
   - Task completes or terminates
4. Handle completion (wake joined tasks, move to DORMANT)
5. Run incremental GC if needed
6. Requeue task to READY (if still running) or appropriate queue
7. Repeat from step 1

## Future Enhancements

Planned features not yet implemented:

- **Mutex support**: Thread-safe synchronization primitives
- **Task.raise**: Throw exceptions to other tasks
- **Task#value**: Retrieve task return value (like Thread#value)
- **Per-task timeslice configuration**

## License

MIT License (same as mruby)

## See Also

- `mruby-fiber`: Cooperative fibers with manual control
- `mruby-sleep`: Blocking sleep (superseded by mruby-task)
