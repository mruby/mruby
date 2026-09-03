# Sleep/usleep tests (from mruby-sleep)
# Note: Use minimal sleep times to avoid test slowdown

assert("sleep accepts non-negative values") do
  assert_nothing_raised { sleep(0) }
end

assert("sleep accepts non-negative float values") do
  skip unless Object.const_defined?(:Float)
  assert_nothing_raised { sleep(0.0) }
  assert_nothing_raised { sleep(-0.0) }
end

assert("sleep raises ArgumentError for negative integer") do
  assert_raise(ArgumentError) { sleep(-1) }
end

assert("sleep raises ArgumentError for negative float") do
  skip unless Object.const_defined?(:Float)
  assert_raise(ArgumentError) { sleep(-0.1) }
end

assert("usleep accepts non-negative values") do
  assert_nothing_raised { usleep(0) }
end

assert("usleep raises ArgumentError for negative value") do
  assert_raise(ArgumentError) { usleep(-100) }
end

# Task creation tests

assert("Task.new creates a task") do
  task = Task.new { }
  assert_kind_of Task, task
end

assert("Task.new accepts name") do
  task = Task.new(name: "test") { }
  assert_equal "test", task.name
end

assert("Task.new accepts priority") do
  task = Task.new(priority: 100) { }
  assert_equal 100, task.priority
end

assert("Task.new raises without block") do
  assert_raise(ArgumentError) { Task.new }
end

# Task state tests

assert("Task#status returns symbol") do
  task = Task.new { }
  status = task.status
  assert_true [:READY, :RUNNING, :WAITING, :SUSPENDED, :DORMANT, :UNKNOWN].include?(status)
end

assert("new task has READY status") do
  task = Task.new { }
  assert_equal :READY, task.status
end

assert("Task#inspect returns formatted string") do
  task = Task.new(name: "test") { }
  inspect_str = task.inspect
  assert_kind_of String, inspect_str
  assert_true inspect_str.include?("Task")
  assert_true inspect_str.include?("test")
end

assert("Task#inspect shows status") do
  task = Task.new { }
  inspect_str = task.inspect
  assert_true inspect_str.include?("READY") || inspect_str.include?("DORMANT")
end

# Task control methods

assert("Task#suspend doesn't raise") do
  task = Task.new { }
  assert_nothing_raised { task.suspend }
  # Clean up: a suspended task left in q_suspended_ keeps a later
  # Task.run from terminating (the scheduler idles waiting on it
  # instead of exiting).
  task.terminate
end

assert("Task#resume doesn't raise") do
  task = Task.new { }
  assert_nothing_raised { task.resume }
end

assert("Task#terminate doesn't raise") do
  task = Task.new { }
  assert_nothing_raised { task.terminate }
end

assert("Task#close removes task and is idempotent") do
  ready_count = Task.stat[:ready][:count]
  task = Task.new { }
  assert_equal ready_count + 1, Task.stat[:ready][:count]
  assert_nil task.close
  assert_equal ready_count, Task.stat[:ready][:count]
  assert_nil task.close
  assert_raise(ArgumentError) { task.status }
end

assert("Task#close rejects current task") do
  assert_raise(RuntimeError) { Task.current.close }
end

# Task.current tests

assert("Task.current in root context") do
  # In root context, Task.current might be nil or a special value
  current = Task.current
  assert_true current.nil? || current.kind_of?(Task)
end

# Task.pass tests

assert("Task.pass yields control") do
  assert_nothing_raised { Task.pass }
end

# Task.stat tests

assert("Task.stat returns hash") do
  stat = Task.stat
  assert_kind_of Hash, stat
end

assert("Task.stat includes tick") do
  stat = Task.stat
  assert_true stat.has_key?(:tick)
  assert_kind_of Integer, stat[:tick]
end

assert("Task.stat includes wakeup_tick") do
  stat = Task.stat
  assert_true stat.has_key?(:wakeup_tick)
  assert_kind_of Integer, stat[:wakeup_tick]
end

assert("Task.stat includes queue counts") do
  stat = Task.stat
  [:ready, :waiting, :suspended, :dormant].each do |queue|
    assert_true stat.has_key?(queue), "Missing queue: #{queue}"
    assert_kind_of Hash, stat[queue]
    assert_true stat[queue].has_key?(:count)
    assert_kind_of Integer, stat[queue][:count]
    assert_true stat[queue].has_key?(:tasks)
    assert_kind_of Array, stat[queue][:tasks]
  end
end

assert("Task.stat tracks task counts") do
  stat_before = Task.stat
  ready_before = stat_before[:ready][:count]

  task1 = Task.new { sleep 0 }
  task2 = Task.new { sleep 0 }

  stat_after = Task.stat
  ready_after = stat_after[:ready][:count]

  assert_equal ready_before + 2, ready_after
end

# Priority tests

assert("Task.new accepts different priorities") do
  low = Task.new(priority: 200) { }
  high = Task.new(priority: 50) { }
  med = Task.new(priority: 128) { }

  assert_equal 200, low.priority
  assert_equal 50, high.priority
  assert_equal 128, med.priority
end

# Name handling

assert("Task with string name") do
  task = Task.new(name: "string_name") { }
  assert_equal "string_name", task.name
end

assert("Task without name returns (noname)") do
  task = Task.new { }
  assert_equal "(noname)", task.name
end

# Edge cases

assert("Task.new with block doesn't execute immediately") do
  executed = false
  task = Task.new { executed = true }
  # Block should not execute until scheduler runs
  assert_false executed
end

assert("Task.run inside Task.run is a noop") do
  assert_nothing_raised do
    Task.new { Task.run }
    Task.run
  end
end

assert("Task#value returns exception object for unhandled task errors") do
  child = nil

  Task.new do
    child = Task.new { raise "boom" }
  end

  Task.run

  result = child.value
  assert_kind_of RuntimeError, result
  assert_equal "boom", result.message
end

assert("Task#terminate on self triggers context switch to next task") do
  order = []

  Task.new(priority: 50) do
    order << :a_start
    Task.current.terminate  # self-terminate - must switch away
    order << :a_zombie      # should never execute
  end

  Task.new(priority: 100) do
    order << :b_runs
  end

  Task.run

  assert_equal [:a_start, :b_runs], order
  assert_false order.include?(:a_zombie)
end

assert("sleep() no-arg suspends the calling task, not another") do
  order = []

  # high-priority task (runs first) - calls sleep() to suspend itself
  high = Task.new(priority: 50) do
    order << :high_start
    sleep                 # should suspend THIS task, not low
    order << :high_resume
  end

  # low-priority task - should keep running after high suspends
  low = Task.new(priority: 200) do
    order << :low_runs
    high.resume           # wake high back up
  end

  Task.run

  assert_equal [:high_start, :low_runs, :high_resume], order
end

assert("exception raised from C after blocking past the timeslice is rescuable") do
  # TaskTest.block_then_raise busy-blocks longer than a timeslice before
  # raising, so task.switching is pending when the exception dispatches.
  # A pending switch must not preempt the catch-handler dispatch: honoring
  # it between catch_handler_find and OP_EXCEPT swallowed the exception
  # into the task result, and the rescue below saw nothing.
  result = nil

  Task.new do
    result =
      begin
        TaskTest.block_then_raise(50)
        :not_raised
      rescue RuntimeError => e
        "caught #{e.message}"
      end
  end

  Task.run

  assert_equal "caught raised after blocking", result
end

assert("exception raised from C after blocking is not leaked into Task#value") do
  child = nil

  Task.new do
    child = Task.new do
      begin
        TaskTest.block_then_raise(50)
      rescue RuntimeError
        :rescued
      end
    end
  end

  Task.run

  assert_equal :rescued, child.value
end

# Scheduler hook tests (mrb_task_set_scheduler_hook)

assert("scheduler hook fires at every scheduler entry") do
  TaskTest.install_probe_hook(0)
  c0 = TaskTest.probe_count(0)
  Task.pass            # entry: task_run_one_iteration (root-context Task.pass)
  c1 = TaskTest.probe_count(0)
  TaskTest.run_once    # entry: mrb_task_run_once
  c2 = TaskTest.probe_count(0)
  Task.new(name: "hook_noop") { }
  Task.run             # entry: task_run_body loop
  c3 = TaskTest.probe_count(0)
  TaskTest.clear_hook
  assert_true c0 + 1 <= c1
  assert_true c1 + 1 <= c2
  assert_true c2 + 1 <= c3
end

assert("scheduler hook wakes a queue-blocked task in the same iteration") do
  q = Task::Queue.new
  ran = []
  t = Task.new(name: "hook_waker") do
    ran << q.pop
  end
  Task.pass  # runs the task until it parks inside q.pop
  TaskTest.install_wake_hook(q)
  # The hook fires before the ready-queue read, so the push it makes must
  # wake the task and get it selected within this single Task.pass. If the
  # hook ran after the read, a second pass would be needed.
  Task.pass
  TaskTest.clear_hook
  assert_equal [42], ran
end

assert("setting a new scheduler hook replaces the previous one") do
  TaskTest.install_probe_hook(0)
  Task.pass
  a_after_first = TaskTest.probe_count(0)
  TaskTest.install_probe_hook(1)
  Task.pass
  TaskTest.clear_hook
  assert_equal a_after_first, TaskTest.probe_count(0)
  assert_true 1 <= TaskTest.probe_count(1)
end

assert("scheduler hook cleared with NULL stops firing") do
  TaskTest.install_probe_hook(0)
  Task.pass
  fired = TaskTest.probe_count(0)
  TaskTest.clear_hook
  Task.pass
  Task.pass
  assert_true 1 <= fired
  assert_equal fired, TaskTest.probe_count(0)
end

# Envs on a task stack must be detached before the stack is freed

assert("closure escaping a closed task survives GC") do
  t = Task.new(name: "escaper") do
    a1 = 1; a2 = 2; a3 = 3; a4 = 4; a5 = 5; a6 = 6
    $task_escaped_proc = -> { a1 + a2 + a3 + a4 + a5 + a6 }
    Task.current.suspend
  end
  Task.pass
  assert_equal 21, $task_escaped_proc.call
  t.terminate
  t.close                 # frees the task's stack
  GC.start                # marks the escaped env; must not read freed memory
  junk = []
  i = 0
  while i < 200
    junk << "x" * 64      # reuse the freed stack region
    i += 1
  end
  GC.start
  assert_equal 21, $task_escaped_proc.call
  $task_escaped_proc = nil
end

assert("closure escaping a task whose context is reinitialized survives GC") do
  t = Task.new(name: "reinit") do
    b1 = 7; b2 = 8; b3 = 9
    $task_escaped_proc2 = -> { b1 + b2 + b3 }
    Task.current.suspend
  end
  Task.pass
  assert_equal 24, $task_escaped_proc2.call
  t.terminate
  # Reuse the task's context for another proc: the old stack is freed
  # inside mrb_task_init_context, with the escaped env still pointing at it.
  TaskTest.reinit_context(t) { 0 }
  GC.start
  junk = []
  i = 0
  while i < 200
    junk << "y" * 48
    i += 1
  end
  GC.start
  assert_equal 24, $task_escaped_proc2.call
  $task_escaped_proc2 = nil
  t.close
end

assert("closure escaping a synchronously executed proc survives GC") do
  result = TaskTest.run_sync do
    c1 = 10; c2 = 20
    $task_escaped_proc3 = -> { c1 + c2 }
    "sync-result"
  end
  # The teardown frees the temporary task's stack; both the returned
  # object and the escaped env must survive it.
  assert_equal "sync-result", result
  GC.start
  junk = []
  i = 0
  while i < 200
    junk << "z" * 48
    i += 1
  end
  GC.start
  assert_equal 30, $task_escaped_proc3.call
  assert_equal "sync-result", result
  $task_escaped_proc3 = nil
end

assert('a disabled VM refuses to make a task') do
  # The VM under test has tasks, so it cannot be disabled here - what can
  # be checked is the other half: TaskTest.with_tasks_disabled runs a
  # block with the flag off, and Task.new must refuse rather than build
  # something nothing will ever schedule.
  caught = nil
  TaskTest.with_tasks_disabled do
    begin
      Task.new { 1 }
    rescue Task::Error => e
      caught = e
    end
  end
  assert_kind_of Task::Error, caught
  assert_include caught.message, 'disabled'
  assert_true TaskTest.tasks_enabled?, 'the flag was not put back'
end

assert('mrb_disable_task_scheduler refuses a VM that already has tasks') do
  # A queue nobody ticks is a hang, not a saving, so the switch is only
  # for a VM that has never had one. This VM has, so it must refuse - and
  # must still be enabled afterwards.
  Task.new { 1 }
  caught = nil
  begin
    TaskTest.disable_tasks
  rescue Task::Error => e
    caught = e
  end
  assert_kind_of Task::Error, caught
  assert_include caught.message, 'already has tasks'
  assert_true TaskTest.tasks_enabled?
end
