module Kernel
  private def `(cmd) #`
    IO.popen(cmd) { |io| io.read }
  end

  private def open(file, *rest, &block)
    raise ArgumentError unless file.is_a?(String)

    if file[0] == "|"
      IO.popen(file[1..-1], *rest, &block)
    else
      File.open(file, *rest, &block)
    end
  end

  private def print(...)
    $stdout.print(...)
  end

  private def puts(...)
    $stdout.puts(...)
  end

  private def printf(...)
    $stdout.printf(...)
  end

  private def gets(...)
    $stdin.gets(...)
  end

  private def readline(...)
    $stdin.readline(...)
  end

  private def readlines(...)
    $stdin.readlines(...)
  end
end
