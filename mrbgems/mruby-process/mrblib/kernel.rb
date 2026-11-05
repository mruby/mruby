module Kernel
  module_function
  ##
  # call-seq:
  #  Kernel#fork -> integer, nil
  def fork(&)
    Process.fork(&)
  end
end
