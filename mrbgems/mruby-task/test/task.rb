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
end

assert("Task#resume doesn't raise") do
  task = Task.new { }
  assert_nothing_raised { task.resume }
end

assert("Task#terminate doesn't raise") do
  task = Task.new { }
  assert_nothing_raised { task.terminate }
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
