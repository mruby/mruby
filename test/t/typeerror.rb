##
# TypeError ISO Test

assert('TypeError', '15.2.29') do
  assert_equal TypeError.class, Class
end

assert('TypeError superclass', '15.2.29.2') do
  assert_equal TypeError.superclass, StandardError
end

