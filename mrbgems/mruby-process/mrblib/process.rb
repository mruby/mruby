module Process
  ##
  # call-seq:
  #  Process.fork -> integer, nil
  def self.fork
    pid = Process.__fork
    return pid if pid != 0
    ##
    # Context switch:
    # From this point onwards (pid == 0) the
    # active process is the child process.
    if block_given?
      begin
        yield
        ##
        # Given a block:
        # Don't return control to the caller.
        # Same as CRuby.
        exit!(0)
      rescue SystemExit
        exit!($!.status)
      rescue Exception
        ##
        # FIXME:
        # we should dump the error to $stderr here.
        exit!(1)
      end
    else
      ##
      # No block given:
      # Return control to the caller that's now
      # running on the child process.
      nil
    end
  end
end
