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
    def pop(non_block = false)
      loop do
        v = __pop_try(non_block)
        return v unless v.equal?(WAIT_RETRY)
      end
    end
    alias deq pop
    alias shift pop
  end
end
