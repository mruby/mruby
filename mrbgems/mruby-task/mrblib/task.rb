class Task
  # Class methods

  # Create a new task with optional name and priority
  # @param name [String, Symbol, nil] optional task name
  # @param priority [Integer, nil] task priority (0-255, 0 is highest)
  # @yield block to execute in the task
  # @return [Task] the created task
  def self.new(name: nil, priority: nil, &block)
  end

  # Get the currently running task
  # @return [Task] the current task
  def self.current
  end

  # Get all tasks in the system
  # @return [Array<Task>] array of all tasks
  def self.list
  end

  # Yield execution to other tasks (cooperative yielding)
  # @return [nil]
  def self.pass
  end

  # Get task scheduler statistics
  # @return [Task::Stat] scheduler statistics object
  def self.stat
  end

  # Find a task by name
  # @param name [String, Symbol] task name to find
  # @return [Task, nil] the task if found, nil otherwise
  def self.get(name)
  end

  # Instance methods

  # Get the task status
  # @return [Symbol] one of :DORMANT, :READY, :RUNNING, :WAITING, :SUSPENDED
  def status
  end

  # Get the task name
  # @return [String, Symbol, nil] the task name
  def name
  end

  # Set the task name
  # @param val [String, Symbol] new task name
  def name=(val)
  end

  # Get the task priority
  # @return [Integer] priority value (0-255, 0 is highest)
  def priority
  end

  # Set the task priority
  # @param val [Integer] new priority (0-255, 0 is highest)
  def priority=(val)
  end

  # Suspend the task
  # @return [self]
  def suspend
  end

  # Resume a suspended task
  # @return [self]
  def resume
  end

  # Terminate the task
  # @return [self]
  def terminate
  end

  # Wait for the task to complete
  # @return [self]
  def join
  end

  # Task scheduler statistics class
  class Stat
    attr_reader :tick, :wakeup_tick, :tasks

    def initialize(tick, wakeup_tick, tasks)
      @tick = tick
      @wakeup_tick = wakeup_tick
      @tasks = tasks
    end

    def to_s
      "tick: #{@tick}, wakeup_tick: #{@wakeup_tick}, tasks: #{@tasks.size}"
    end
  end
end

# Kernel methods for sleeping

module Kernel
  # Sleep for specified duration
  # @param sec [Integer, Float, nil] seconds to sleep (nil = suspend indefinitely)
  # @return [Integer, nil] actual sleep time
  def sleep(sec = nil)
  end

  # Sleep for specified milliseconds
  # @param ms [Integer] milliseconds to sleep
  # @return [nil]
  def sleep_ms(ms)
  end
end
