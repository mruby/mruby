module Process
  # The way a process finished, as reported by Process.waitpid and left in
  # <code>$?</code>.
  #
  # The questions a status answers (#exited?, #exitstatus, #termsig and the
  # rest) are decoded from the platform value by the process HAL. What is
  # here is the part that reads the same everywhere.
  class Status
    # Whether the process exited with a status of zero.
    #
    # Returns nil rather than false when the process did not exit at all,
    # because "did it succeed?" has no answer for one that was signalled.
    def success?
      exitstatus == 0 if exited?
    end

    def inspect
      "#<Process::Status: #{self}>"
    end
  end
end
