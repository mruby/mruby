# Task::Queue tests

assert("Task::Queue.new creates a queue") do
  q = Task::Queue.new
  assert_kind_of Task::Queue, q
end

assert("Task::Queue push and non-blocking pop return item in FIFO order") do
  q = Task::Queue.new
  q.push(1)
  q.push(2)
  q.push(3)
  assert_equal 1, q.pop(true)
  assert_equal 2, q.pop(true)
  assert_equal 3, q.pop(true)
end

assert("Task::Queue << alias works") do
  q = Task::Queue.new
  q << :a
  q << :b
  assert_equal :a, q.pop(true)
  assert_equal :b, q.pop(true)
end

assert("Task::Queue enq/deq aliases work") do
  q = Task::Queue.new
  q.enq(10)
  assert_equal 10, q.deq(true)
end

assert("Task::Queue shift alias works") do
  q = Task::Queue.new
  q.push(:x)
  assert_equal :x, q.shift(true)
end

assert("Task::Queue size and length") do
  q = Task::Queue.new
  assert_equal 0, q.size
  assert_equal 0, q.length
  q.push(1)
  assert_equal 1, q.size
  q.push(2)
  assert_equal 2, q.length
  q.pop(true)
  assert_equal 1, q.size
end

assert("Task::Queue empty?") do
  q = Task::Queue.new
  assert_true q.empty?
  q.push(1)
  assert_false q.empty?
  q.pop(true)
  assert_true q.empty?
end

assert("Task::Queue clear") do
  q = Task::Queue.new
  q.push(1)
  q.push(2)
  q.clear
  assert_true q.empty?
  assert_equal 0, q.size
end

assert("Task::Queue pop(true) raises Task::Error when empty") do
  q = Task::Queue.new
  assert_raise(Task::Error) { q.pop(true) }
end

assert("Task::Queue close and closed?") do
  q = Task::Queue.new
  assert_false q.closed?
  q.close
  assert_true q.closed?
end

assert("Task::Queue push raises Task::Error after close") do
  q = Task::Queue.new
  q.close
  assert_raise(Task::Error) { q.push(1) }
end

assert("Task::Queue pop(true) returns nil when closed and empty") do
  q = Task::Queue.new
  q.close
  assert_equal nil, q.pop(true)
end

assert("Task::Queue pops remaining items after close, then nil") do
  q = Task::Queue.new
  q.push(1)
  q.push(2)
  q.close
  assert_equal 1, q.pop(true)
  assert_equal 2, q.pop(true)
  assert_equal nil, q.pop(true)
end

assert("Task::Queue double close is no-op") do
  q = Task::Queue.new
  q.close
  assert_nothing_raised { q.close }
  assert_true q.closed?
end

assert("Task::Queue num_waiting is 0 with no blocked tasks") do
  q = Task::Queue.new
  assert_equal 0, q.num_waiting
end

assert("Task::Queue blocking pop wakes on push") do
  q = Task::Queue.new
  results = []

  Task.new { results << q.pop }
  Task.new { q.push(99) }
  Task.run

  assert_equal [99], results
end

assert("Task::Queue multiple producers and consumers") do
  q = Task::Queue.new
  received = []

  Task.new { q.push(1) }
  Task.new { q.push(2) }
  Task.new { q.push(3) }
  Task.new { received << q.pop }
  Task.new { received << q.pop }
  Task.new { received << q.pop }
  Task.run

  assert_equal [1, 2, 3], received.sort
end

assert("Task::Queue blocking pop returns nil when queue is closed") do
  q = Task::Queue.new
  results = []

  Task.new { results << q.pop }
  Task.new { q.close }
  Task.run

  assert_equal [nil], results
end

assert("Task::Queue num_waiting reflects blocked task count") do
  q = Task::Queue.new
  counts = []

  Task.new { q.pop }
  Task.new do
    counts << q.num_waiting  # consumer should be waiting
    q.push(:done)
  end
  Task.run

  assert_equal [1], counts
end
