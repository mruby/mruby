class File < IO
  # The path to the file
  attr_accessor :path

  #
  # call-seq:
  #   File.new(filename, mode="r")            -> file
  #   File.new(filename [, mode [, perm]])    -> file
  #   File.new(fd [, mode])                   -> file
  #
  # Opens the file named by filename according to the given mode and returns
  # a new File object. If a file descriptor is given instead of a filename,
  # the new File object will be associated with that descriptor.
  #
  #   f = File.new("testfile", "r")
  #   f = File.new("newfile",  "w+")
  #   f = File.new("tmpfile",  "a")
  #
  def initialize(fd_or_path, mode = "r", perm = 0666)
    if fd_or_path.kind_of? Integer
      super(fd_or_path, mode)
    else
      @path = fd_or_path
      fd = IO.sysopen(@path, mode, perm)
      super(fd, mode)
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
    "<#{self.class}:#{@path}>"
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



end
