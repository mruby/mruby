module Kernel
  module_function
  ##
  # call-seq:
  #  Kernel#fork -> integer, nil
  def fork(&)
    Process.fork(&)
  end

  ##
  # call-seq:
  #  Kernel#exec(command)        -> never returns
  #  Kernel#exec(cmd, arg1, ...) -> never returns
  def exec(*args)
    Process.exec(*args)
  end
end
