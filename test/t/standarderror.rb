##
# StandardError ISO Test

assert('StandardError', '15.2.23') do
  assert_equal StandardError.class, Class
end

assert('StandardError superclass', '15.2.23.2') do
  assert_equal StandardError.superclass, Exception
end
