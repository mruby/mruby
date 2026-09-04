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

assert('Comparable#clamp - a receiver that stands in no order with a bound') do
  # The bounds are put in order before the receiver is compared with them, so
  # a NaN receiver passes that check and reaches a comparison of its own.
  skip unless Object.const_defined?(:Float)
  nan = Float::NAN

  assert_raise(ArgumentError) { nan.clamp(0, 1) }
  assert_raise(ArgumentError) { nan.clamp(0..1) }

  assert_equal 3, 5.clamp(1, 3)
  assert_equal 1.0, 1.0.clamp(0.0, 2.0)
end
