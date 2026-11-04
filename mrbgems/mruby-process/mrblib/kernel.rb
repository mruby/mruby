module Kernel
  module_function
  ##
  # call-seq:
  #  Kernel#fork -> integer
  def fork(&)
    Process.fork(&)
  end
end
