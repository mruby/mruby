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
  module_function def `(cmd) #`
    IO.popen(cmd) {|io| io.read }
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
  module_function def open(file, *rest, &block)
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
  module_function def p(*a)
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
  #   pp(obj)              -> obj
  #   pp(obj1, obj2, ...)  -> [obj1, obj2, ...]
  #   pp()                 -> nil
  #
  # Same as p: mruby has no pretty printer, so each object is written as
  # its inspect.
  #
  module_function def pp(*a)
    p(*a)
  end

  #
  # call-seq:
  #   warn(*msgs, uplevel: nil, category: nil)  -> nil
  #
  # Writes each message to $stderr, each followed by a newline unless it
  # ends in one; an Array gives its elements. With uplevel: n the message
  # starts with "file:line: warning: " of the caller n frames up, when
  # Kernel#caller (mruby-kernel-ext) is there to tell it. category: is
  # taken for CRuby compatibility; mruby has no Warning categories.
  #
  #   warn("disk is full")                  # disk is full
  #   warn("use bar instead", uplevel: 0)   # app.rb:3: warning: use bar instead
  #
  module_function def warn(*msgs, uplevel: nil, category: nil)
    out = ""
    # the messages in turn, an Array giving its elements (Array#flatten
    # and String#<< are mruby-array-ext's and mruby-string-ext's)
    stack = [msgs]
    pos = [0]
    while (n = stack.size) > 0
      a = stack[n - 1]
      i = pos[n - 1]
      if i == a.size
        stack.pop
        pos.pop
        next
      end
      pos[n - 1] = i + 1
      m = a[i]
      if m.is_a?(Array)
        stack << m
        pos << 0
        next
      end
      s = m.to_s
      out += s
      out += "\n" unless s[-1] == "\n"
    end
    return nil if out.empty?
    if uplevel
      uplevel = uplevel.to_int
      raise ArgumentError, "negative level (#{uplevel})" if uplevel < 0
      if respond_to?(:caller, true)
        loc = caller(uplevel + 1, 1)
        loc = loc && loc[0]
        if loc
          i = loc.index(":in ")
          loc = loc[0, i] if i
          out = "#{loc}: warning: #{out}"
        end
      end
    end
    $stderr.write(out)
    nil
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
  module_function def print(...)
    $stdout.print(...)
  end

  #
  # call-seq:
  #   putc(int)    -> nil
  #
  # Equivalent to $stdout.putc(int).
  # If int is Integer, writes the byte (mod 256).
  # If int is String, writes the first character.
  # Returns nil.
  #
  module_function def putc(c)
    $stdout.putc(c)
    nil
  end

  #
  # call-seq:
  #   puts(obj, ...)    -> nil
  #
  # Equivalent to $stdout.puts(obj, ...).
  #
  #   puts "this", "is", "a", "test"
  #
  module_function def puts(...)
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
  module_function def printf(...)
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
  module_function def gets(...)
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
  module_function def readline(...)
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
  module_function def readlines(...)
    $stdin.readlines(...)
  end
end
