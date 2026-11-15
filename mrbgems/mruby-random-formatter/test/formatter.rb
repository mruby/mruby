# Random::Formatter draws from the #bytes of whatever includes it
class RandomFormatterTest
  include Random::Formatter

  attr_reader :sizes

  def initialize(byte)
    @byte = byte
    @sizes = []
  end

  def bytes(n)
    @sizes << n
    @byte * n
  end
end

assert("Random::Formatter") do
  assert_kind_of Module, Random::Formatter
  assert_true Random.include?(Random::Formatter)
  assert_true Random.respond_to?(:random_number)
end

assert("Random::Formatter#random_number") do
  r = Random.new(42)
  100.times do
    assert_include(0...10, r.random_number(10))
    assert_include(3..5, r.random_number(3..5))
    assert_include(3...5, Random.random_number(3...5))
    assert_include(0...10, r.rand(10))
  end
  assert_equal(Random.new(7).random_number(1000), Random.new(7).random_number(1000))

  assert_raise(ArgumentError) { r.random_number("x") }
  assert_raise(ArgumentError) { r.random_number(:a..:b) }
  # Random's own rand still rejects what the formatter's does not
  assert_raise(ArgumentError) { r.rand(-1) }
end

assert("Random::Formatter#random_number reads #bytes") do
  zero = RandomFormatterTest.new("\x00")
  assert_equal 0, zero.random_number(10)
  assert_equal 3, zero.random_number(3..5)
  assert_equal 0, zero.rand(10)
  assert_equal [4, 4, 4], zero.sizes

  ones = RandomFormatterTest.new("\xff")
  assert_equal 255, ones.random_number(256)
  # 2**32 is out of reach where mrb_int is 32 bits and bigint is absent; the
  # shift width comes from a variable so the compiler does not fold it
  shift = 32
  wide = begin
    1 << shift
  rescue RangeError
    nil
  end
  assert_equal wide - 1, ones.random_number(wide) if wide

  assert_raise(RangeError) { RandomFormatterTest.new("ab").random_number(10) }
  assert_raise(TypeError) { RandomFormatterTest.new(1).random_number(10) }
end

assert("Random::Formatter#random_number gives a Float for no limit") do
  skip unless Object.const_defined?(:Float)
  zero = RandomFormatterTest.new("\x00")
  [[], [nil], [0], [-3], [0.0], [-1.0], [1...1], [3..1], [1.0...1.0]].each do |args|
    assert_equal 0.0, zero.random_number(*args)
  end
  assert_equal [8] * 9, zero.sizes
  assert_equal 1.0, zero.random_number(1.0..1.0)

  ones = RandomFormatterTest.new("\xff")
  v = ones.random_number
  assert_true 0.99 < v && v < 1.0
  v = ones.random_number(2.5)
  assert_true 2.4 < v && v < 2.5

  r = Random.new(42)
  100.times do
    assert_include(0.0...1.0, r.random_number)
    assert_include(0.0...2.5, r.random_number(2.5))
    assert_include(1.0..2.0, r.random_number(1.0..2.0))
  end
  assert_raise(FloatDomainError) { r.random_number(Float::INFINITY) }

  # a span past the largest Float, and an end that rounding would reach
  max = 1e308 < 1.0 / 0 ? 1e308 : 3e38  # 3e38 under MRB_USE_FLOAT32
  assert_equal(-max, zero.random_number(-max..max))
  100.times { assert_include(-max..max, r.random_number(-max..max)) }
  v = ones.random_number(1.0...2.0)
  assert_true 1.0 < v && v < 2.0
end

assert("Random::Formatter#random_number with a big limit") do
  shift = 70
  big = begin
    1 << shift
  rescue RangeError
    nil
  end
  skip unless big
  r = Random.new(42)
  100.times { assert_include(0...big, r.random_number(big)) }
  assert_equal 0, RandomFormatterTest.new("\x00").random_number(big)
  assert_include(0...big, Random.random_number(big))
end
