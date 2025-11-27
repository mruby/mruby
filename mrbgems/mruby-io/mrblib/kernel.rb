module Kernel
  #
  # call-seq:
  #   `cmd` -> string
  #
  # Returns the standard output of running cmd in a subshell.
  # The built-in syntax %x{...} uses this method.
  #
  #   `date`                   #=> "Wed Apr  9 08:56:30 CDT 2003\n"
  #   `ls testdir`.split[1]    #=> "main.rb"
  #   `echo oops && exit 99`   #=> "oops\n"
  #
  private def `(cmd) #`
    IO.popen(cmd) { |io| io.read }
  end

  #
  # call-seq:
  #   open(name [, mode [, perm]] [, opt])                -> io or nil
  #   open(name [, mode [, perm]] [, opt]) {|io| block }  -> obj
  #
  # Creates an IO object connected to the given stream, file, or subprocess.
  # If path starts with a pipe character ("|"), a subprocess is created,
  # connected to the caller by a pair of pipes. The returned IO object may
  # be used to write to the standard input and read from the standard output
  # of this subprocess.
  #
  #   open("testfile")      #=> #<File:testfile>
  #   open("| date")        #=> #<IO:fd 6>
  #   open("testfile") do |f|
  #     print f.gets
  #   end
  #
  private def open(file, *rest, &block)
    raise ArgumentError unless file.is_a?(String)

    if file[0] == "|"
      IO.popen(file[1..-1], *rest, &block)
    else
      File.open(file, *rest, &block)
    end
  end

  #
  # call-seq:
  #   p(obj)              -> obj
  #   p(obj1, obj2, ...)  -> [obj1, obj2, ...]
  #   p()                 -> nil
  #
  # For each object, directly writes obj.inspect followed by a newline
  # to the program's standard output.
  #
  #   S = Struct.new(:name, :state)
  #   s = S['dave', 'TX']
  #   p s             #=> #<struct S name="dave", state="TX">
  #
  private def p(*a)
    for e in a
      $stdout.write e.inspect
      $stdout.write "\n"
    end
    len = a.size
    return nil if len == 0
    return a[0] if len == 1
    a
  end

  #
  # call-seq:
  #   print(obj, ...)    -> nil
  #
  # Prints each object in turn to $stdout. If the output field separator
  # ($,) is not nil, its contents will appear between each field.
  # If the output record separator ($\) is not nil, it will be appended
  # to the output.
  #
  #   print "cat", [1,2,3], 99, "\n"
  #   $, = ", "
  #   $\ = "\n"
  #   print "cat", [1,2,3], 99
  #
  private def print(...)
    $stdout.print(...)
  end

  #
  # call-seq:
  #   puts(obj, ...)    -> nil
  #
  # Equivalent to $stdout.puts(obj, ...).
  #
  #   puts "this", "is", "a", "test"
  #
  private def puts(...)
    $stdout.puts(...)
  end

  #
  # call-seq:
  #   printf(io, string [, obj ... ])    -> nil
  #   printf(string [, obj ... ])        -> nil
  #
  # Equivalent to io.write(sprintf(string, obj, ...)) or
  # $stdout.write(sprintf(string, obj, ...)).
  #
  #   printf "Number: %5.2f,\nString: %s\n", 1.23, "hello"
  #
  private def printf(...)
    $stdout.printf(...)
  end

  #
  # call-seq:
  #   gets(sep=$/)     -> string or nil
  #   gets(limit)      -> string or nil
  #   gets(sep,limit)  -> string or nil
  #
  # Returns (and assigns to $_) the next line from the list of files in ARGV
  # (or $*), or from standard input if no files are present on the command line.
  # Returns nil at end of file.
  #
  #   print "Enter your name: "
  #   name = gets
  #   print "Hello #{name}"
  #
  private def gets(...)
    $stdin.gets(...)
  end

  #
  # call-seq:
  #   readline(sep=$/)     -> string
  #   readline(limit)      -> string
  #   readline(sep,limit)  -> string
  #
  # Equivalent to gets, except readline raises EOFError at end of file.
  #
  #   print "Enter your name: "
  #   name = readline
  #   print "Hello #{name}"
  #
  private def readline(...)
    $stdin.readline(...)
  end

  #
  # call-seq:
  #   readlines(sep=$/)     -> array
  #   readlines(limit)      -> array
  #   readlines(sep,limit)  -> array
  #
  # Returns an array containing the lines returned by calling gets(sep)
  # until the end of file.
  #
  #   lines = readlines
  #   lines[0]   #=> "This is line one\n"
  #
  private def readlines(...)
    $stdin.readlines(...)
  end
end
