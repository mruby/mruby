class File < IO
  include Enumerable

  class FileError < Exception; end
  class NoFileError < FileError; end
  class UnableToStat < FileError; end
  class PermissionError < FileError; end

  attr_accessor :path

  def initialize(fd_or_path, mode = "r", perm = 0666)
    self._bless
    if fd_or_path.kind_of? Fixnum
      super(fd_or_path, mode)
    else
      @path = fd_or_path

      perm = 0666  unless perm.is_a? Fixnum
      fd = IO.sysopen(@path, mode, perm)
      if fd < 0 && Object.const_defined?(:Errno)
        begin
          Errno.handle @path
        rescue Errno::EMFILE
          GC.run(true)
          fd = IO.sysopen(@path, mode, perm)
          Errno.handle if fd < 0
        end
      elsif fd < 0
        raise NoFileError.new "no such file or directory"
      end
      super(fd, mode)
    end
  end

  def self.join(*names)
    if names.size == 0
      ""
    elsif names.size == 1
      names[0]
    else
      if names[0][-1] == File::SEPARATOR
        s = names[0][0..-2]
      else
        s = names[0].dup
      end
      (1..names.size-2).each { |i|
        t = names[i]
        if t[0] == File::SEPARATOR and t[-1] == File::SEPARATOR
          t = t[1..-2]
        elsif t[0] == File::SEPARATOR
          t = t[1..-1]
        elsif t[-1] == File::SEPARATOR
          t = t[0..-2]
        end
        s += File::SEPARATOR + t if t != ""
      }
      if names[-1][0] == File::SEPARATOR
        s += File::SEPARATOR + names[-1][1..-1]
      else
        s += File::SEPARATOR + names[-1]
      end
      s
    end
  end

  def self.directory?(file)
    FileTest.directory?(file)
  end

  def self.exist?(file)
    FileTest.exist?(file)
  end

  def self.exists?(file)
    FileTest.exists?(file)
  end

  def self.file?(file)
    FileTest.file?(file)
  end

  def self.pipe?(file)
    FileTest.pipe?(file)
  end

  def self.size?(file)
    FileTest.size?(file)
  end

  def self.socket?(file)
    FileTest.socket?(file)
  end

  def self.symlink?(file)
    FileTest.symlink?(file)
  end

  def self.zero?(file)
    FileTest.zero?(file)
  end

  def self.extname(filename)
    fname = self.basename(filename)
    return '' if fname[0] == '.' || fname.index('.').nil?
    ext = fname.split('.').last
    ext.empty? ? '' : ".#{ext}"
  end
end
