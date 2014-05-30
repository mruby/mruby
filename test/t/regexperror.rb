##
# RegexpError ISO Test

# TODO broken ATM assert('RegexpError', '15.2.27') do

assert('direct superclass of RegexpError', '15.2.27.2') do
  assert_equal StandardError, RegexpError.superclass
end
