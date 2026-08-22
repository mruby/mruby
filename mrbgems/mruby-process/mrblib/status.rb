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

    # A description of how the process finished, in the shape CRuby uses:
    #
    #   pid 1234 exit 0
    #   pid 1234 SIGKILL (signal 9)
    #   pid 1234 SIGSEGV (signal 11) (core dumped)
    #   pid 1234 stopped SIGSTOP (signal 19)
    def to_s
      if exited?
        "pid #{pid} exit #{exitstatus}"
      elsif signaled?
        desc = "pid #{pid} #{Status._signal_description(termsig)}"
        coredump? ? "#{desc} (core dumped)" : desc
      elsif stopped?
        "pid #{pid} stopped #{Status._signal_description(stopsig)}"
      else
        "pid #{pid}"
      end
    end

    def inspect
      "#<Process::Status: #{self}>"
    end

    # Spell a signal number out for #to_s. A platform that does not name the
    # number gets the bare number rather than a guess.
    def self._signal_description(signo)
      name = _signame(signo)
      name ? "SIG#{name} (signal #{signo})" : "signal #{signo}"
    end
  end
end
