$ok_test = 0
$ko_test = 0
$kill_test = 0
$skip_test = 0
$asserts  = []
$test_start = Time.now if Object.const_defined?(:Time)

unless RUBY_ENGINE == "mruby"
  # For bintest on Ruby
  def t_print(*args)
    print(*args)
    $stdout.flush
    nil
  end
end

##
# Create the assertion in a readable way
def assertion_string(err, str, iso=nil, e=nil, bt=nil)
  msg = "#{err}#{str}"
  msg += " [#{iso}]" if iso && !iso.empty?
  msg += " => #{e}" if e && !e.to_s.empty?
  msg += " (#{GEMNAME == 'mruby-test' ? 'core' : "mrbgems: #{GEMNAME}"})"
  if $mrbtest_assert && $mrbtest_assert.size > 0
    $mrbtest_assert.each do |idx, assert_msg, diff|
      msg += "\n - Assertion[#{idx}]"
      msg += " #{assert_msg}." if assert_msg && !assert_msg.empty?
      msg += "\n#{diff}" if diff && !diff.empty?
    end
  end
  msg += "\nbacktrace:\n\t#{bt.join("\n\t")}" if bt
  msg
end

##
# Verify a code block.
#
# str : A remark which will be printed in case
#       this assertion fails
# iso : The ISO reference code of the feature
#       which will be tested by this
#       assertion
def assert(str = 'Assertion failed', iso = '')
  t_print(str, (iso != '' ? " [#{iso}]" : ''), ' : ') if $mrbtest_verbose
  begin
    $mrbtest_assert = []
    $mrbtest_assert_idx = 0
    yield
    if($mrbtest_assert.size > 0)
      $asserts.push(assertion_string('Fail: ', str, iso))
      $ko_test += 1
      t_print('F')
    else
      $ok_test += 1
      t_print('.')
    end
  rescue MRubyTestSkip => e
    $asserts.push(assertion_string('Skip: ', str, iso, e))
    $skip_test += 1
    t_print('?')
  rescue Exception => e
    bt = e.backtrace if $mrbtest_verbose
    $asserts.push(assertion_string("#{e.class}: ", str, iso, e, bt))
    $kill_test += 1
    t_print('X')
  ensure
    $mrbtest_assert = nil
  end
  t_print("\n") if $mrbtest_verbose
end

def assertion_diff(exp, act)
  "    Expected: #{exp.inspect}\n" \
  "      Actual: #{act.inspect}"
end

def assert_true(obj, msg = nil, diff = nil)
  if $mrbtest_assert
    $mrbtest_assert_idx += 1
    unless obj == true
      diff ||= "    Expected #{obj.inspect} to be true."
      $mrbtest_assert.push([$mrbtest_assert_idx, msg, diff])
    end
  end
  obj
end

def assert_false(obj, msg = nil, diff = nil)
  unless obj == false
    diff ||= "    Expected #{obj.inspect} to be false."
  end
  assert_true(!obj, msg, diff)
end

def assert_equal(exp, act_or_msg = nil, msg = nil, &block)
  ret, exp, act, msg = _eval_assertion(:==, exp, act_or_msg, msg, block)
  unless ret
    diff = assertion_diff(exp, act)
  end
  assert_true(ret, msg, diff)
end

def assert_not_equal(exp, act_or_msg = nil, msg = nil, &block)
  ret, exp, act, msg = _eval_assertion(:==, exp, act_or_msg, msg, block)
  if ret
    diff = "    Expected #{act.inspect} to not be equal to #{exp.inspect}."
  end
  assert_true(!ret, msg, diff)
end

def assert_same(*args); _assert_same(true, *args) end
def assert_not_same(*args); _assert_same(false, *args) end
def _assert_same(affirmed, exp, act, msg = nil)
  unless ret = exp.equal?(act) == affirmed
    exp_str, act_str = [exp, act].map do |o|
      "#{o.inspect} (class=#{o.class}, oid=#{o.__id__})"
    end
    diff = "    Expected #{act_str} to #{'not ' unless affirmed}be the same as #{exp_str}."
  end
  assert_true(ret, msg, diff)
end

def assert_nil(obj, msg = nil)
  unless ret = obj.nil?
    diff = "    Expected #{obj.inspect} to be nil."
  end
  assert_true(ret, msg, diff)
end

def assert_include(*args); _assert_include(true, *args) end
def assert_not_include(*args); _assert_include(false, *args) end
def _assert_include(affirmed, collection, obj, msg = nil)
  unless ret = collection.include?(obj) == affirmed
    diff = "    Expected #{collection.inspect} to #{'not ' unless affirmed}include #{obj.inspect}."
  end
  assert_true(ret, msg, diff)
end

##
# Fails unless +obj+ is a kind of +cls+.
def assert_kind_of(cls, obj, msg = nil)
  unless ret = obj.kind_of?(cls)
    diff = "    Expected #{obj.inspect} to be a kind of #{cls}, not #{obj.class}."
  end
  assert_true(ret, msg, diff)
end

##
# Fails unless +exp+ is equal to +act+ in terms of a Float
def assert_float(exp, act, msg = nil)
  e, a = exp.to_f, act.to_f
  if (e.infinite? || a.infinite?) && e != a ||
     e.nan? && !a.nan? || !e.nan? && a.nan?
    flunk(msg, "    Expected #{act} to be #{exp}.")
  elsif (n = (e - a).abs) > Mrbtest::FLOAT_TOLERANCE
    flunk(msg, "    Expected |#{exp} - #{act}| (#{n}) to be <= #{Mrbtest::FLOAT_TOLERANCE}.")
  else
    pass
  end
end

def assert_raise(*exc)
  msg = (exc.last.is_a? String) ? exc.pop : nil
  exc = exc.empty? ? StandardError : exc.size == 1 ? exc[0] : exc
  begin
    yield
  rescue *exc
    pass
  rescue Exception => e
    diff = "    #{exc} exception expected, not\n" \
           "    Class: <#{e.class}>\n" \
           "    Message: <#{e}>"
    flunk(msg, diff)
  else
    diff = "    #{exc} expected but nothing was raised."
    flunk(msg, diff)
  end
end

def assert_nothing_raised(msg = nil)
  begin
    yield
  rescue Exception => e
    diff = "    Exception raised:\n" \
           "    Class: <#{e.class}>\n" \
           "    Message: <#{e}>"
    flunk(msg, diff)
  else
    pass
  end
end

def pass
  assert_true(true)
end

def flunk(msg = nil, diff = "Epic Fail!")
  assert_true(false, msg, diff)
end

##
# Report the test result and print all assertions
# which were reported broken.
def report
  t_print("\n")

  $asserts.each do |msg|
    t_print("#{msg}\n")
  end

  $total_test = $ok_test + $ko_test + $kill_test + $skip_test
  t_print("Total: #{$total_test}\n")

  t_print("   OK: #{$ok_test}\n")
  t_print("   KO: #{$ko_test}\n")
  t_print("Crash: #{$kill_test}\n")
  t_print(" Skip: #{$skip_test}\n")

  if Object.const_defined?(:Time)
    t_time = Time.now - $test_start
    t_print(" Time: #{t_time.round(2)} seconds\n")
  end

  $ko_test == 0 && $kill_test == 0
end

def _eval_assertion(meth, exp, act_or_msg, msg, block)
  if block
    exp, act, msg = exp, block.call, act_or_msg
  else
    exp, act, msg = exp, act_or_msg, msg
  end
  return exp.__send__(meth, act), exp, act, msg
end

##
# Skip the test
class MRubyTestSkip < NotImplementedError; end

def skip(cause = "")
  raise MRubyTestSkip.new(cause)
end
