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
end
