assert("Comparable#clamp") do
  assert_equal(12, 12.clamp(0, 100))
  assert_equal(100, 532.clamp(0, 100))
  assert_equal(0, -3.123.clamp(0, 100))
  assert_equal('d', 'd'.clamp('a', 'f'))
  assert_equal('f', 'z'.clamp('a', 'f'))

  assert_equal(12, 12.clamp(0..100))
  assert_equal(100, 523.clamp(0..100))
  assert_equal(0, -3.123.clamp(0..100))

  assert_equal('d', 'd'.clamp('a'..'f'))
  assert_equal('f', 'z'.clamp('a'..'f'))

  assert_equal(0, -20.clamp(0..))
  assert_equal(100, 523.clamp(..100))

  assert_raise(ArgumentError) {
    100.clamp(0...100)
  }
end

assert("Comparable#clamp with a NaN for a bound") do
  # A NaN stands in no order with anything, so it bounds nothing: `min <=> max`
  # has no answer to give and clamp refuses the pair, as it does for any two
  # bounds it cannot put in order.
  skip unless Object.const_defined?(:Float)
  nan = Float::NAN

  assert_raise(ArgumentError) { 1.clamp(nan, 2) }
  assert_raise(ArgumentError) { 1.clamp(0, nan) }
  assert_raise(ArgumentError) { 1.0.clamp(nan, 2) }
  assert_raise(ArgumentError) { 1.clamp(nan..2) }
end
