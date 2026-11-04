module Kernel
  module_function
  ##
  # call-seq:
  #  Kernel#fork -> integer
  def fork(&)
    Process.fork(&)
  end

  ##
  # call-seq:
  #  Kernel#exit -> nil
  def exit!(status=0)
    Process.exit!(status)
  end
end
