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
