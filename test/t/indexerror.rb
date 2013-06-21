##
# IndexError ISO Test

assert('IndexError', '15.2.33') do
  assert_equal IndexError.class, Class
end

assert('IndexError superclass', '15.2.33.2') do
  assert_equal IndexError.superclass, StandardError
end
