class File < IO
  # The path to the file
  attr_accessor :path

  #
  # call-seq:
  #   file.to_path    -> filepath
  #
  # The path of the file, as File#path gives it: a File stands for its path
  # where an object with to_path is taken for one.
  #
  alias to_path path

  #
  # call-seq:
  #   File.new(filename, mode="r")            -> file
  #   File.new(filename [, mode [, perm]])    -> file
  #   File.new(fd [, mode])                   -> file
  #   File.new(path_or_fd, **options)         -> file
  #
  # Opens the file named by filename according to the given mode and returns
  # a new File object. If a file descriptor is given instead of a filename,
  # the new File object will be associated with that descriptor.
  #
  # The keyword options are those of IO.new: +mode:+ in place of the
  # positional mode, +perm:+ in place of the positional permission,
  # +autoclose:+ for a descriptor, and +path:+ for the path that File#path
  # reports for a descriptor. Other keywords are ignored.
  #
  #   f = File.new("testfile", "r")
  #   f = File.new("newfile",  "w+")
  #   f = File.new("tmpfile",  "a")
  #   f = File.new(fd, autoclose: false, path: "/dev/stdout")
  #
  def initialize(fd_or_path, mode = "r", perm = nil, **opts)
    mode = opts[:mode] if opts[:mode]
    # a nil perm: stands for no keyword, as a nil positional one does
    unless opts[:perm].nil?
      raise ArgumentError, "perm specified twice" unless perm.nil?
      perm = opts[:perm]
    end
    perm ||= 0666
    if Integer === fd_or_path
      @path = opts[:path]
      super(fd_or_path, mode, opts)
    else
      @path = fd_or_path
      fd = IO.sysopen(@path, mode, perm)
      super(fd, mode, opts)
    end
  end

  #
  # call-seq:
  #   file.atime    -> time
  #
  # Returns the last access time for file, or epoch if the platform
  # doesn't have access time.
  #
  #   File.new("testfile").atime   #=> Wed Apr 09 08:51:48 CDT 2003
  #
  def atime
    t = self._atime
    t && Time.at(t)
  end

  #
  # call-seq:
  #   file.ctime -> time
  #
  # Returns the change time for file (that is, the time directory
  # information about the file was changed, not the file itself).
  #
  #   File.new("testfile").ctime   #=> Wed Apr 09 08:53:13 CDT 2003
  #
  def ctime
    t = self._ctime
    t && Time.at(t)
  end

  #
  # call-seq:
  #   file.mtime -> time
  #
  # Returns the modification time for file.
  #
  #   File.new("testfile").mtime   #=> Wed Apr 09 08:53:14 CDT 2003
  #
  def mtime
    t = self._mtime
    t && Time.at(t)
  end

  #
  # call-seq:
  #   file.inspect  -> string
  #
  # Return a string describing this File object.
  #
  #   File.new("testfile").inspect   #=> "#<File:testfile>"
  #
  def inspect
    desc = @path || (closed? ? nil : "fd #{fileno}")
    desc = desc ? "#{desc} (closed)" : "(closed)" if closed?
    "#<#{self.class}:#{desc}>"
  end


  #
  # call-seq:
  #   File.foreach(name) {|line| block }  -> nil
  #   File.foreach(name)                  -> an_enumerator
  #
  # Executes the block for every line in the named I/O port, where lines
  # are separated by sep.
  #
  #   File.foreach("testfile") {|x| print "GOT ", x }
  #   GOT This is line one
  #   GOT This is line two
  #   GOT This is line three
  #   GOT And so on...
  #
  def self.foreach(file)
    if block_given?
      self.open(file) do |f|
        f.each {|l| yield l}
      end
    else
      return self.new(file)
    end
  end

  #
  # call-seq:
  #   File.split(file_name)   -> array
  #
  # Splits the given string into a directory and a file component and
  # returns them in a two-element array. See also File.dirname and
  # File.basename.
  #
  #   File.split("/home/gumby/.profile")   #=> ["/home/gumby", ".profile"]
  #
  def self.split(path)
    [dirname(path), basename(path)]
  end

  #
  # call-seq:
  #   File.utime(atime, mtime, file_name, ...)   -> integer
  #
  # Sets the access and modification times of each named file. A time is
  # a Time or a number of seconds since the Epoch; nil stands for the
  # current time. Returns the number of file names.
  #
  #   File.utime(0, Time.now, "testfile")   #=> 1
  #
  def self.utime(atime, mtime, *paths)
    # a Time is read here, so that the C side never calls back into Ruby
    atime = [atime.to_i, atime.usec] if atime.respond_to?(:usec)
    mtime = [mtime.to_i, mtime.usec] if mtime.respond_to?(:usec)
    __utime(atime, mtime, *paths)
  end



end
