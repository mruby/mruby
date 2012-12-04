class File < IO
  include Enumerable

  class FileError < Exception; end
  class NoFileError < FileError; end
  class UnableToStat < FileError; end
  class PermissionError < FileError; end

  def initialize(fd_or_path, mode = "r", perm = 0666)
    if fd_or_path.kind_of? Integer
      super(fd_or_path, mode)
      path = nil
    else
      path = fd_or_path

      fd = IO.sysopen(path, mode, perm)
      if fd < 0
        begin
          Errno.handle path
        rescue Errno::EMFILE
          GC.run(true)

          fd = IO.sysopen(path, mode, perm)
          Errno.handle if fd < 0
        end
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
end
