class IOError < StandardError; end
class EOFError < StandardError; end

class IO
  def self.open(*args, &block)
    io = self.new(*args)

    return io unless block

    begin
      yield io
    ensure
      begin
        io.close unless io.closed?
      rescue StandardError
        # nothing
      end
    end
  end

  def self.popen(command, mode = 'r', &block)
    io = self.__popen__(command, mode)

    return io unless block

    begin
      yield io
    ensure
      begin
        io.close unless io.closed?
      rescue StandardError
        # nothing
      end
    end
  end

  # 15.2.20.5.3
  def each(&block)
    while line = self.gets
      block.call(line)
    end
    self
  end

  # 15.2.20.5.4
  def each_byte(&block)
    while char = self.getc
      block.call(char)
    end
    self
  end

  # 15.2.20.5.5
  alias each_line each

  def puts(*args)
    i = 0
    len = args.size
    while i < len
      s = args[i].to_s
      write s
      write "\n" if (s[-1] != "\n")
      i += 1
    end
    write "\n" if len == 0
    nil
  end

  def printf(*args)
    write sprintf(*args)
    nil
  end
end

STDIN  = IO.open(0, 'r')
STDOUT = IO.open(1, 'w')
STDERR = IO.open(2, 'w')

$stdin  = STDIN
$stdout = STDOUT
$stderr = STDERR

module Kernel
  def print(*args)
    STDOUT.print(*args)
  end

  def puts(*args)
    STDOUT.puts(*args)
  end

  def p(*args)
    STDOUT.puts(*args)
  end

  def printf(*args)
    STDOUT.printf(*args)
  end

  def gets(*args)
    STDIN.gets(*args)
  end

  def getc(*args)
    STDIN.getc(*args)
  end
end
