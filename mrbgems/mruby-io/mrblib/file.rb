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

  #
  # call-seq:
  #   File.directory?(file_name)   -> true or false
  #
  # Returns true if the named file is a directory, or a symlink that points at a directory, and false otherwise.
  #
  #   File.directory?(".")   #=> true
  #
  def self.directory?(file)
    FileTest.directory?(file)
  end

  #
  # call-seq:
  #   File.exist?(file_name)    -> true or false
  #
  # Return true if the named file exists.
  #
  #   File.exist?("config.h")      #=> true
  #   File.exist?("no_such_file")  #=> false
  #
  def self.exist?(file)
    FileTest.exist?(file)
  end

  #
  # call-seq:
  #   File.exists?(file_name)   -> true or false
  #
  # Deprecated method that is equivalent to File.exist?.
  #
  def self.exists?(file)
    FileTest.exists?(file)
  end

  #
  # call-seq:
  #   File.file?(file) -> true or false
  #
  # Returns true if the named file exists and is a regular file.
  #
  #   File.file?("testfile")   #=> true
  #
  def self.file?(file)
    FileTest.file?(file)
  end

  #
  # call-seq:
  #   File.pipe?(file_name)   -> true or false
  #
  # Returns true if the named file is a pipe.
  #
  #   File.pipe?("/dev/stdin")   #=> true
  #
  def self.pipe?(file)
    FileTest.pipe?(file)
  end

  #
  # call-seq:
  #   File.size(file_name)   -> integer
  #
  # Returns the size of file_name.
  #
  #   File.size("testfile")   #=> 66
  #
  def self.size(file)
    FileTest.size(file)
  end

  #
  # call-seq:
  #   File.size?(file_name)   -> integer or nil
  #
  # Returns nil if file_name doesn't exist or has zero size, the size of the file otherwise.
  #
  #   File.size?("testfile")   #=> 66
  #
  def self.size?(file)
    FileTest.size?(file)
  end

  #
  # call-seq:
  #   File.socket?(file_name)   -> true or false
  #
  # Returns true if the named file is a socket.
  #
  #   File.socket?("/tmp/.X11-unix/X0")   #=> true
  #
  def self.socket?(file)
    FileTest.socket?(file)
  end

  #
  # call-seq:
  #   File.symlink?(file_name)   -> true or false
  #
  # Returns true if the named file is a symbolic link.
  #
  def self.symlink?(file)
    FileTest.symlink?(file)
  end

  #
  # call-seq:
  #   File.zero?(file_name)   -> true or false
  #
  # Returns true if the named file exists and has a zero size.
  #
  #   File.zero?("testfile")   #=> false
  #
  def self.zero?(file)
    FileTest.zero?(file)
  end


end
