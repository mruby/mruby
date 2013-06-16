##
# RangeError ISO Test

assert('RangeError', '15.2.26') do
  assert_equal RangeError.class, Class
end

assert('RangeError superclass', '15.2.26.2') do
  assert_equal RangeError.superclass, StandardError
end
