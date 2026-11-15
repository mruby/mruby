class Dir
  include Enumerable

  #
  # call-seq:
  #   dir.each { |filename| block } -> dir
  #   dir.each                      -> enumerator
  #
  # Calls the block once for each entry in this directory, passing the filename
  # of each entry as a parameter to the block.
  #
  #   d = Dir.new("testdir")
  #   d.each { |x| puts "Got #{x}" }
  #
  def each(&block)
    return to_enum(:each) unless block
    while s = self.read
      block.call(s)
    end
    self
  end

  #
  # call-seq:
  #   dir.each_child { |filename| block } -> dir
  #   dir.each_child                      -> enumerator
  #
  # Calls the block once for each entry in this directory except for "." and "..",
  # passing the filename of each entry as a parameter to the block.
  #
  #   d = Dir.new("testdir")
  #   d.each_child { |x| puts "Got #{x}" }
  #
  def each_child(&block)
    return to_enum(:each_child) unless block
    while s = self.read
      block.call(s) unless s == "." || s == ".."
    end
    self
  end

  alias pos tell
  alias pos= seek

  class << self


    #
    # call-seq:
    #   Dir.foreach(dirname, encoding: nil) { |filename| block } -> nil
    #   Dir.foreach(dirname, encoding: nil)                      -> enumerator
    #
    # Calls the block once for each entry in the named directory, passing
    # the filename of each entry as a parameter to the block. encoding: is
    # as for Dir.new.
    #
    #   Dir.foreach("testdir") { |x| puts "Got #{x}" }
    #
    def foreach(path, encoding: nil, &block)
      return to_enum(:foreach, path, encoding: encoding) unless block
      self.open(path, encoding: encoding) do |d|
        d.each(&block)
      end
    end

    #
    # call-seq:
    #   Dir.each_child(dirname, encoding: nil) { |filename| block } -> nil
    #   Dir.each_child(dirname, encoding: nil)                      -> enumerator
    #
    # Calls the block once for each entry in the named directory except for
    # "." and "..", passing the filename of each entry to the block.
    # encoding: is as for Dir.new.
    #
    #   Dir.each_child("testdir") { |x| puts "Got #{x}" }
    #
    def each_child(path, encoding: nil, &block)
      return to_enum(:each_child, path, encoding: encoding) unless block
      self.open(path, encoding: encoding) do |d|
        d.each_child(&block)
      end
      nil
    end

    #
    # call-seq:
    #   Dir.open(string, encoding: nil) -> aDir
    #   Dir.open(string, encoding: nil) { |aDir| block } -> obj
    #
    # With no block, open is a synonym for Dir.new. If a block is present, it is
    # passed aDir as a parameter. The directory is closed at the end of the block,
    # and Dir.open returns the value of the block.
    #
    #   Dir.open("testdir") { |d| d.each { |x| puts "Got #{x}" } }
    #
    def open(path, encoding: nil, &block)
      if block
        d = self.new(path, encoding: encoding)
        begin
          block.call(d)
        ensure
          begin
            d.close
          rescue IOError
          end
        end
      else
        self.new(path, encoding: encoding)
      end
    end

    #
    # call-seq:
    #   Dir.chdir(string) -> 0
    #   Dir.chdir(string) { |path| block } -> obj
    #
    # Changes the current working directory of the calling process to the given
    # string. When called with a block, changes to the directory, executes the
    # block, then changes back to the original directory. Returns the value of
    # the block.
    #
    #   Dir.chdir("/var/spool/mail")
    #   Dir.chdir("/tmp") { Dir.pwd }   #=> "/tmp"
    #
    def chdir(path, &block)
      if block
        wd = self.getwd
        begin
          self._chdir(path)
          block.call(path)
        ensure
          self._chdir(wd)
        end
      else
        self._chdir(path)
      end
    end

    alias exists? exist?
    alias pwd getwd
    alias rmdir delete
    alias unlink delete
  end
end
