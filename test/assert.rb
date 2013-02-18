$ok_test = 0
$ko_test = 0
$kill_test = 0
$asserts  = []
$test_start = Time.now if Object.const_defined?(:Time)

##
# Create the assertion in a readable way
def assertion_string(err, str, iso=nil, e=nil)
  msg = "#{err}#{str}"
  msg += " [#{iso}]" if iso && iso != ''
  msg += " => #{e.message}" if e
  msg += " (mrbgems: #{GEMNAME})" if Object.const_defined?(:GEMNAME)
  if $mrbtest_assert && $mrbtest_assert.size > 0
    $mrbtest_assert.each do |idx, str, diff|
      msg += "\n - Assertion[#{idx}] Failed: #{str}\n#{diff}"
    end
  end
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
  print(str, (iso != '' ? " [#{iso}]" : ''), ' : ') if $mrbtest_verbose
  begin
    $mrbtest_assert = []
    $mrbtest_assert_idx = 0
    if(!yield || $mrbtest_assert.size > 0)
      $asserts.push(assertion_string('Fail: ', str, iso, nil))
      $ko_test += 1
      print('F')
    else
      $ok_test += 1
      print('.')
    end
  rescue Exception => e
    $asserts.push(assertion_string('Error: ', str, iso, e))
    $kill_test += 1
    print('X')
  ensure
    $mrbtest_assert = nil
  end
  print("\n") if $mrbtest_verbose
end

def assertion_diff(exp, act)
  "    Expected: #{exp.inspect}\n" +
  "      Actual: #{act.inspect}"
end

def assert_true(ret, msg = nil, diff = nil)
  if $mrbtest_assert
    $mrbtest_assert_idx += 1
    if !ret
      msg = "Expected #{ret.inspect} to be true" unless msg
      diff = assertion_diff(true, ret)  unless diff
      $mrbtest_assert.push([$mrbtest_assert_idx, msg, diff])
    end
  end
  ret
end

def assert_equal(exp, act, msg = nil)
  msg = "Expected to be equal" unless msg
  diff = assertion_diff(exp, act)
  assert_true(exp == act, msg, diff)
end

def assert_nil(obj, msg = nil)
  msg = "Expected #{obj.inspect} to be nil" unless msg
  diff = assertion_diff(nil, obj)
  assert_true(obj.nil?, msg, diff)
end

##
# Report the test result and print all assertions
# which were reported broken.
def report()
  print "\n"

  $asserts.each do |msg|
    puts msg
  end

  $total_test = $ok_test.+($ko_test)
  print('Total: ')
  print($total_test)
  print("\n")

  print('   OK: ')
  print($ok_test)
  print("\n")
  print('   KO: ')
  print($ko_test)
  print("\n")
  print('Crash: ')
  print($kill_test)
  print("\n")

  if Object.const_defined?(:Time)
    print(' Time: ')
    print(Time.now - $test_start)
    print(" seconds\n")
  end
end

##
# Performs fuzzy check for equality on methods returning floats
# on the basis of the Math::TOLERANCE constant.
def check_float(a, b)
  tolerance = Math::TOLERANCE
  a = a.to_f
  b = b.to_f
  if a.finite? and b.finite?
    (a-b).abs < tolerance
  else
    true
  end
end
