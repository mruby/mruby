##
# FalseClass ISO Test

assert('FalseClass', '15.2.6') do
  assert_equal FalseClass.class, Class
end

assert('FalseClass superclass', '15.2.6.2') do
  assert_equal FalseClass.superclass, Object
end

assert('FalseClass false', '15.2.6.1') do
  assert_false false
end

assert('FalseClass#&', '15.2.6.3.1') do
  assert_false false.&(true)
  assert_false false.&(false)
end

assert('FalseClass#^', '15.2.6.3.2') do
  assert_true false.^(true)
  assert_false false.^(false)
end

assert('FalseClass#to_s', '15.2.6.3.3') do
  assert_equal false.to_s, 'false'
end

assert('FalseClass#|', '15.2.6.3.4') do
  assert_true false.|(true)
  assert_false false.|(false)
end
