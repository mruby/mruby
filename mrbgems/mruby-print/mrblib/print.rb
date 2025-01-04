##
# Kernel
#
# ISO 15.3.1
module Kernel
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
        print s
        print "\n" if (s[-1] != "\n")
      end
      i += 1
    end
    print "\n" if len == 0
  end

  def printf(*args)
    print(sprintf(*args))
  end
end
