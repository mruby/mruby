##
# Kernel Test (mruby-io)

# $stderr stand-in that keeps what is written
class MRubyIOTestSink
  attr_reader :out
  def initialize; @out = ""; end
  def write(s); @out += s; s.size; end
end

def mruby_io_test_stderr
  saved = $stderr
  sink = MRubyIOTestSink.new
  $stderr = sink
  begin
    yield
  ensure
    $stderr = saved
  end
  sink.out
end

def mruby_io_test_stdout
  saved = $stdout
  sink = MRubyIOTestSink.new
  $stdout = sink
  begin
    yield
  ensure
    $stdout = saved
  end
  sink.out
end

# what is written from the caller one frame up starts with "file:line",
# or is the message alone in a build that keeps no backtrace
def mruby_io_test_warning?(out, msg)
  return out == "#{msg}\n" if caller(0, 1).empty?
  tail = ": warning: #{msg}\n"
  out.size > tail.size && out[-tail.size, tail.size] == tail && out.include?("kernel.rb:")
end

assert('Kernel#warn') do
  assert_equal "a\n", mruby_io_test_stderr { assert_nil warn("a") }
  assert_equal "a\nb\nc\n1\n", mruby_io_test_stderr { warn("a", ["b", "c\n"], 1) }
  assert_equal "", mruby_io_test_stderr { warn }
  assert_equal "", mruby_io_test_stderr { warn([]) }
  assert_equal "d\n", mruby_io_test_stderr { warn("d", category: :deprecated) }
end

assert('Kernel#warn with uplevel:') do
  def mruby_io_test_warner
    warn("w", uplevel: 1)
  end
  out = mruby_io_test_stderr { warn("v", uplevel: 0) }
  assert_true mruby_io_test_warning?(out, "v"), out
  out = mruby_io_test_stderr { mruby_io_test_warner }
  assert_true mruby_io_test_warning?(out, "w"), out
  assert_raise(ArgumentError) { warn("x", uplevel: -1) }
end

assert('Kernel#pp') do
  assert_equal "", mruby_io_test_stdout { assert_nil pp }
  assert_equal "1\n", mruby_io_test_stdout { assert_equal 1, pp(1) }
  assert_equal "1\n\"a\"\n", mruby_io_test_stdout { assert_equal [1, "a"], pp(1, "a") }
end
