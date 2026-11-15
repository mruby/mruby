##
# Warning Test

# $stderr stand-in that keeps what is written
class WarningTestSink
  attr_reader :out
  def initialize; @out = ""; end
  def write(s); @out += s; s.size; end
end

def warning_test_stderr
  saved = $stderr
  sink = WarningTestSink.new
  $stderr = sink
  begin
    yield
  ensure
    $stderr = saved
  end
  sink.out
end

# runs the block with the category set to flag, and back as it was
def warning_test_with(category, flag)
  saved = Warning[category]
  Warning[category] = flag
  begin
    yield
  ensure
    Warning[category] = saved
  end
end

# the calls to Warning.warn while the block runs, as [message, category],
# Warning.warn redefined for the while; one_arg, it takes the message alone
def warning_test_calls(one_arg = false)
  $warning_test_calls = []
  if one_arg
    def Warning.warn(message)
      $warning_test_calls << [message]
      nil
    end
  else
    def Warning.warn(message, category: nil)
      $warning_test_calls << [message, category]
      nil
    end
  end
  begin
    yield
  ensure
    class << Warning
      remove_method :warn
    end
  end
  $warning_test_calls
end

assert('Warning.categories') do
  assert_equal [:deprecated, :experimental, :performance, :strict_unused_block], Warning.categories
end

assert('Warning[]') do
  assert_false Warning[:deprecated]
  assert_true Warning[:experimental]
  assert_false Warning[:performance]
  assert_false Warning[:strict_unused_block]
  assert_raise(TypeError) { Warning["deprecated"] }
  assert_raise(ArgumentError) { Warning[:no_such_category] }
end

assert('Warning[]=') do
  warning_test_with(:deprecated, false) do
    assert_equal 1, (Warning[:deprecated] = 1)
    assert_true Warning[:deprecated]
    Warning[:deprecated] = nil
    assert_false Warning[:deprecated]
  end
  assert_raise(TypeError) { Warning["deprecated"] = true }
  assert_raise(ArgumentError) { Warning[:no_such_category] = true }
end

assert('Warning.warn') do
  assert_equal "a", warning_test_stderr { assert_nil Warning.warn("a") }
  warning_test_with(:deprecated, false) do
    assert_equal "", warning_test_stderr { assert_nil Warning.warn("b\n", category: :deprecated) }
  end
  warning_test_with(:deprecated, true) do
    assert_equal "c\n", warning_test_stderr { Warning.warn("c\n", category: :deprecated) }
  end
  assert_raise(TypeError) { Warning.warn(1) }
  assert_raise(TypeError) { Warning.warn("e", category: false) }
  assert_raise(ArgumentError) { Warning.warn("d", category: :no_such_category) }
end

assert('Warning is extended with itself') do
  assert_true Warning.is_a?(Warning)
end

assert('Kernel#warn goes through Warning.warn') do
  assert_equal [["a\n", nil]], warning_test_calls { assert_nil warn("a") }
  assert_equal [["b\nc\n", nil]], warning_test_calls { warn("b", "c") }
  assert_equal [], warning_test_calls { warn }
  warning_test_with(:experimental, true) do
    assert_equal [["d\n", :experimental]], warning_test_calls { warn("d", category: :experimental) }
    assert_equal [["e\n", :experimental]], warning_test_calls { warn("e", category: "experimental") }
  end
end

assert('Kernel#warn drops a warning in a category that is off') do
  warning_test_with(:deprecated, false) do
    assert_equal [], warning_test_calls { assert_nil warn("a", category: :deprecated) }
    assert_equal "", warning_test_stderr { warn("a", category: :deprecated) }
  end
  warning_test_with(:deprecated, true) do
    assert_equal "b\n", warning_test_stderr { warn("b", category: :deprecated) }
  end
  assert_raise(ArgumentError) { warn("c", category: :no_such_category) }
  assert_raise(TypeError) { warn("d", category: false) }
end

assert('Kernel#warn calls Warning.warn without category: when there is none') do
  assert_equal [["a\n"]], warning_test_calls(true) { warn("a") }
end
