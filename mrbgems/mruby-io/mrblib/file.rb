class File < IO
  attr_accessor :path

  def initialize(fd_or_path, mode = "r", perm = 0666)
    if fd_or_path.kind_of? Integer
      super(fd_or_path, mode)
    else
      @path = fd_or_path
      fd = IO.sysopen(@path, mode, perm)
      super(fd, mode)
    end
  end

  def atime
    t = self._atime
    t && Time.at(t)
  end

  def ctime
    t = self._ctime
    t && Time.at(t)
  end

  def mtime
    t = self._mtime
    t && Time.at(t)
  end

  def inspect
    "<#{self.class}:#{@path}>"
  end

  def self.join(*names)
    return "" if names.empty?

    names.map! do |name|
      case name
      when String
        name
      when Array
        if names == name
          raise ArgumentError, "recursive array"
        end
        join(*name)
      else
        raise TypeError, "no implicit conversion of #{name.class} into String"
      end
    end

    return names[0] if names.size == 1

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

  def self.foreach(file)
    if block_given?
      self.open(file) do |f|
        f.each {|l| yield l}
      end
    else
      return self.new(file)
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

  def self.size(file)
    FileTest.size(file)
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
    epos = fname.rindex('.')
    return '' if epos == 0 || epos.nil?
    return fname[epos..-1]
  end

  def self.path(filename)
    if filename.kind_of?(String)
      filename
    else
      raise TypeError, "no implicit conversion of #{filename.class} into String"
    end
  end
end
