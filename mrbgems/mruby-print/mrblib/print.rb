##
# Kernel
#
# ISO 15.3.1
module Kernel
  ##
  # Print human readable object description
  #
  # ISO 15.3.1.2.9   Kernel.p
  # ISO 15.3.1.3.34  Kernel#p
  def p(*args)
    i = 0
    len = args.size
    while i < len
      __printstr__ args[i].inspect
      __printstr__ "\n"
      i += 1
    end
    args.__svalue
  end

  # ISO 15.3.1.2.10 Kernel.print
  # ISO 15.3.1.3.35 Kernel#print
  def print(*args)
    i = 0
    len = args.size
    while i < len
      __printstr__ args[i].to_s
      i += 1
    end
  end

  # ISO 15.3.1.2.11 Kernel.puts
  # ISO 15.3.1.3.39 Kernel#puts
  def puts(*args)
    i = 0
    len = args.size
    while i < len
      s = args[i]
      if s.kind_of?(Array)
        puts(*s)
      else
        s = s.to_s
        __printstr__ s
        __printstr__ "\n" if (s[-1] != "\n")
      end
      i += 1
    end
    __printstr__ "\n" if len == 0
  end

  def printf(*args)
    __printstr__(sprintf(*args))
  end
end
