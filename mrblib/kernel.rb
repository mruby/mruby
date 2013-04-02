module Kernel
  def open(file, *rest)
    raise ArgumentError unless file.is_a?(String)

    if file[0] == "|"
      IO.popen(file[1..-1], *rest)
    else
      File.open(file, *rest)
    end
  end
end
