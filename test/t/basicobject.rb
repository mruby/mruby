##
# BasicObject

assert('BasicObject') do
  assert_equal(BasicObject.class, Class)
end

assert('BasicObject superclass') do
  assert_nil(BasicObject.superclass)
end

