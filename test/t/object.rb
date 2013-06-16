##
# Object ISO Test

assert('Object', '15.2.1') do
  assert_equal Object.class, Class
end

assert('Object superclass', '15.2.1.2') do
  assert_equal Object.superclass, BasicObject
end

