##
# IO
#
# ISO 15.2.20

class IOError < StandardError; end
class EOFError < IOError; end

class IO
  #
  # call-seq:
  #   IO.open(fd, mode="r" [, opt])                -> io
  #   IO.open(fd, mode="r" [, opt]) {|io| block }  -> obj
  #
  # With no associated block, IO.open is a synonym for IO.new. If the optional
  # code block is given, it will be passed io as an argument, and the IO object
  # will automatically be closed when the block terminates. In this instance,
  # IO.open returns the value of the block.
  #
  #   fd = IO.sysopen("/dev/tty", "w")
  #   a = IO.open(fd,"w")
  #   $stderr.puts "Hello"
  #   a.close
  #
  def self.open(*args, &block)
    io = self.new(*args)

    return io unless block

    begin
      yield io
    ensure
      begin
        io.close unless io.closed?
      rescue StandardError
      end
    end
  end

  #
  # call-seq:
  #   IO.popen(cmd, mode="r" [, opt])               -> io
  #   IO.popen(cmd, mode="r" [, opt]) {|io| block } -> obj
  #
  # Runs the specified command as a subprocess; the subprocess's standard input
  # and output will be connected to the returned IO object.
  #
  #   p IO.popen("date").read   #=> "Wed Apr  9 08:56:30 CDT 2003\n"
  #   IO.popen("dc", "r+") {|f|
  #     f.puts "5 2 *"
  #     f.close_write
  #     puts f.read
  #   }
  #
  def self.popen(command, mode = 'r', **opts, &block)
    if !self.respond_to?(:_popen)
      raise NotImplementedError, "popen is not supported on this platform"
    end
    io = self._popen(command, mode, **opts)
    return io unless block

    begin
      yield io
    ensure
      begin
        io.close unless io.closed?
      rescue IOError
        # nothing
      end
    end
  end

  #
  # call-seq:
  #   IO.pipe                    -> [read_io, write_io]
  #   IO.pipe {|read_io, write_io| ... } -> obj
  #
  # Creates a pair of pipe endpoints (connected to each other) and returns
  # them as a two-element array of IO objects: [read_io, write_io].
  #
  #   rd, wr = IO.pipe
  #   if fork
  #     wr.close
  #     puts rd.read
  #     rd.close
  #     Process.wait
  #   else
  #     rd.close
  #     wr.write "Hello, parent!"
  #     wr.close
  #     exit
  #   end
  #
  def self.pipe(&block)
    if !self.respond_to?(:_pipe)
      raise NotImplementedError, "pipe is not supported on this platform"
    end
    if block
      begin
        r, w = IO._pipe
        yield r, w
      ensure
        r.close unless r.closed?
        w.close unless w.closed?
      end
    else
      IO._pipe
    end
  end

  #
  # call-seq:
  #   IO.read(name, [length [, offset]] )   -> string
  #   IO.read(name, [length [, offset]], mode: mode)   -> string
  #
  # Opens the file, optionally seeks to the given offset, then returns length
  # bytes (defaulting to the rest of the file). read ensures the file is
  # closed before returning.
  #
  #   IO.read("testfile")           #=> "This is line one\nThis is line two\n"
  #   IO.read("testfile", 20)       #=> "This is line one\nTh"
  #   IO.read("testfile", 20, 10)   #=> "ne one\nThis is line "
  #
  def self.read(path, length=nil, offset=0, mode: "r")
    str = ""
    fd = -1
    io = nil
    begin
      fd = IO.sysopen(path, mode)
      io = IO.open(fd, mode)
      io.seek(offset) if offset > 0
      str = io.read(length)
    ensure
      if io
        io.close
      elsif fd != -1
        IO._sysclose(fd)
      end
    end
    str
  end



  #
  # call-seq:
  #   ios.hash   -> integer
  #
  # Compute a hash based on the IO object. Two IO objects with the same
  # content will have the same hash code (and will compare using eql?).
  # We must define IO#hash here because IO includes Enumerable and
  # Enumerable#hash will call IO#read() otherwise.
  #
  def hash
    # We must define IO#hash here because IO includes Enumerable and
    # Enumerable#hash will call IO#read() otherwise
    self.__id__
  end


  # Alias for eof?
  alias_method :eof, :eof?
  # Alias for pos
  alias_method :tell, :pos

  #
  # call-seq:
  #   ios.pos = integer    -> integer
  #
  # Seeks to the given position (in bytes) in ios. It is not guaranteed that
  # seeking to the right position when ios is textmode.
  #
  #   f = File.new("testfile")
  #   f.pos = 17
  #   f.gets   #=> "This is line two\n"
  #
  def pos=(i)
    seek(i, SEEK_SET)
  end

  #
  # call-seq:
  #   ios.rewind    -> 0
  #
  # Positions ios to the beginning of input, resetting lineno to zero.
  #
  #   f = File.new("testfile")
  #   f.readline   #=> "This is line one\n"
  #   f.rewind     #=> 0
  #   f.lineno     #=> 0
  #   f.readline   #=> "This is line one\n"
  #
  def rewind
    seek(0, SEEK_SET)
  end


  #
  # call-seq:
  #   ios.each(sep=$/) {|line| block }         -> ios
  #   ios.each(limit) {|line| block }          -> ios
  #   ios.each(sep,limit) {|line| block }      -> ios
  #   ios.each(...)                            -> an_enumerator
  #
  # Executes the block for every line in ios, where lines are separated by sep.
  # ios must be opened for reading. If no block is given, an enumerator is returned instead.
  #
  #   f = File.new("testfile")
  #   f.each {|line| puts "#{f.lineno}: #{line}" }
  #
  # 15.2.20.5.3
  def each(&block)
    return to_enum unless block

    while line = self.gets
      block.call(line)
    end
    self
  end

  #
  # call-seq:
  #   ios.each_byte {|byte| block }  -> ios
  #   ios.each_byte                  -> an_enumerator
  #
  # Calls the given block once for each byte (0..255) in ios, passing the byte
  # as an argument. The stream must be opened for reading or an IOError will be raised.
  #
  #   f = File.new("testfile")
  #   checksum = 0
  #   f.each_byte {|x| checksum ^= x }   #=> #<File:testfile>
  #   checksum                           #=> 12
  #
  # 15.2.20.5.4
  def each_byte(&block)
    return to_enum(:each_byte) unless block

    while byte = self.getbyte
      block.call(byte)
    end
    self
  end

  # Alias for each - 15.2.20.5.5
  alias each_line each

  #
  # call-seq:
  #   ios.each_char {|c| block }  -> ios
  #   ios.each_char               -> an_enumerator
  #
  # Calls the given block once for each character in ios, passing the character
  # as an argument. The stream must be opened for reading or an IOError will be raised.
  #
  #   f = File.new("testfile")
  #   ios.each_char {|c| print c, ' ' }   #=> #<File:testfile>
  #
  def each_char(&block)
    return to_enum(:each_char) unless block

    while char = self.getc
      block.call(char)
    end
    self
  end



  #
  # call-seq:
  #   ios.printf(format_string [, obj, ...])    -> nil
  #
  # Formats and writes to ios, converting parameters under control of the format string.
  # See sprintf for details of the format string.
  #
  #   $stdout.printf "Number: %5.2f,\nString: %s\n", 1.23, "hello"
  #   Number:  1.23,
  #   String: hello
  #
  def printf(*args)
    write sprintf(*args)
    nil
  end

  # Alias for fileno - returns the integer file descriptor for ios
  alias_method :to_i, :fileno
  # Alias for isatty - returns true if ios is associated with a terminal device
  alias_method :tty?, :isatty
end

# Standard input stream - connected to file descriptor 0
STDIN  = IO.open(0, "r")
# Standard output stream - connected to file descriptor 1
STDOUT = IO.open(1, "w")
# Standard error stream - connected to file descriptor 2
STDERR = IO.open(2, "w")

# Global variable for standard input
$stdin  = STDIN
# Global variable for standard output
$stdout = STDOUT
# Global variable for standard error
$stderr = STDERR
