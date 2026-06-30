class Task
  class Queue
    # WAIT_RETRY is defined in C (task_queue.c gem init)

    def push(obj)
      __push(obj)
      self
    end
    alias enq push
    alias << push

    # Blocks until an item is available (default), or raises if non_block is true.
    # Returns nil if the queue is closed and empty.
    #
    # The loop is not a busy-wait. When __pop_try finds the queue empty it moves
    # the current task to WAITING and sets switching_=TRUE before returning
    # WAIT_RETRY. The VM detects switching_ at the next opcode boundary and
    # exits mrb_vm_exec, handing control back to the scheduler. This task does
    # not run again until a push (or close) moves it back to READY. The loop
    # body therefore executes at most once per wakeup event.
    # When timeout_ms is given, pop returns nil once the deadline passes. Note
    # that a closed-and-empty queue also returns nil, so a nil result does not
    # distinguish "timed out" from "closed" -- this mirrors CRuby's
    # Thread::Queue#pop(timeout:).
    def pop(non_block = false, timeout_ms: nil)
      unless timeout_ms.nil?
        raise ArgumentError, "timeout cannot be combined with non_block" if non_block
        raise TypeError, "timeout_ms must be an Integer" unless timeout_ms.is_a?(Integer)
        raise ArgumentError, "timeout_ms must be non-negative" if timeout_ms < 0
      end
      deadline = timeout_ms.nil? ? nil : Task.tick + timeout_ms
      while true
        remaining = deadline.nil? ? nil : deadline - Task.tick
        v = __pop_try(non_block, remaining)
        return nil if v.equal?(WAIT_TIMEOUT)
        return v unless v.equal?(WAIT_RETRY)
      end
    end
    alias deq pop
    alias shift pop
  end
end
