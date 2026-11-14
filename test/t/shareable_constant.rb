# shareable_constant_value: literal

# Constants written under the magic comment above. It asks for values that
# Ractors can share; mruby has no Ractor, and writes the constants as they
# are.

module ShareableConstantTest
  PLAIN = [1, "two"]
  OR_WRITE ||= 3
  OR_WRITE ||= 4
  SUM = 5
  SUM += 6
end
ShareableConstantTest::PATH = "path"

assert('constant writes under shareable_constant_value') do
  assert_equal [1, "two"], ShareableConstantTest::PLAIN
  assert_equal 3, ShareableConstantTest::OR_WRITE
  assert_equal 11, ShareableConstantTest::SUM
  assert_equal "path", ShareableConstantTest::PATH
  assert_equal "assignment", defined?(ShareableConstantTest::DEFINED = 1)
end
