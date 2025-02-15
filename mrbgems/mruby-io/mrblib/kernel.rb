module Kernel
  private def `(cmd)
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

  private def print(*args)
    $stdout.print(*args)
  end

  private def puts(*args)
    $stdout.puts(*args)
  end

  private def printf(*args)
    $stdout.printf(*args)
  end

  private def gets(*args)
    $stdin.gets(*args)
  end
end
