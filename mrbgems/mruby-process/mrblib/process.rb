module Process
  ##
  # call-seq:
  #   Process.fork                  -> integer or nil
  #   Process.fork { block }        -> integer
  #
  # Creates a child process. The parent gets the child's pid. Without a
  # block the child gets nil; with one, the child runs the block and
  # exits, with status 0 when it finishes, the status of a SystemExit it
  # raises, or 1 for any other exception, which is reported.
  def self.fork
    pid = __fork
    return pid if pid != 0
    return nil unless block_given?
    begin
      yield
    rescue Exception => e
      __exit_child(e)
    end
    __exit_child(nil)
  end
end
