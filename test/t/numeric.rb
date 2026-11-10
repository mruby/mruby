##
# Numeric ISO Test

def assert_step(exp, receiver, args, inf: false)
  act = []
  ret = receiver.step(*args) do |i|
    act << i
    break if inf && exp.size == act.size
  end
  expr = "#{receiver.inspect}.step(#{args.map(&:inspect).join(', ')})"
  assert "assert_step" do
    assert_true(exp.eql?(act), "#{expr}: counters", assertion_diff(exp, act))
    assert_same(receiver, ret, "#{expr}: return value") unless inf
  end
end

assert('Numeric', '15.2.7') do
  assert_equal(Class, Numeric.class)
end

assert('Numeric#+@', '15.2.7.4.1') do
  assert_equal(+1, +1)
end

assert('Numeric#-@', '15.2.7.4.2') do
  assert_equal(-1, -1)

  skip unless Object.const_defined?(:Float)
  pzero = 0.0
  nzero = -0.0
  assert_float(-Float::INFINITY, 1.0 / -pzero)
  assert_float(Float::INFINITY, 1.0 / -nzero)
  assert_equal("-0.0", (-pzero).to_s)
  assert_equal("0.0", (-nzero).to_s)
end

assert('Numeric#abs', '15.2.7.4.3') do
  assert_equal(1, 1.abs)
  skip unless Object.const_defined?(:Float)
  assert_equal(1.0, -1.abs)
end

assert('Numeric#/', '15.2.8.3.4') do
  n = Class.new(Numeric){ def /(x); 15.1;end }.new

  assert_equal(2, 10/5)
  assert_equal(0, 1/16)
  assert_equal(15.1, n/10)
  assert_raise(TypeError){ 1/n }
  assert_raise(TypeError){ 1/nil }
end

# Not ISO specified

assert('Numeric#**') do
  assert_equal(8, 2 ** 3)
  assert_equal(-8, -2 ** 3)
  assert_equal(1, 2 ** 0)
  skip unless Object.const_defined?(:Float)
  assert_equal(1.0, 2.2 ** 0)
  assert_equal(0.5, 2 ** -1)
  assert_equal(8.0, 2.0**3)
end

assert('Numeric#step') do
  assert_raise(ArgumentError) { 1.step(2, 0) { break } }
  assert_step([2, 3, 4], 2, [4])
  assert_step([10, 8, 6, 4, 2], 10, [1, -2])
  assert_step([], 2, [1, 3])
  assert_step([], -2, [-1, -3])
  assert_step([10, 11, 12, 13], 10, [], inf: true)
  assert_step([10, 7, 4], 10, [nil, -3], inf: true)

  skip unless Object.const_defined?(:Float)
  inf = Float::INFINITY
  assert_raise(ArgumentError) { 1.step(2, 0.0) { break } }
  assert_step([2, 3, 4], 2, [4.0])
  assert_step([7.0, 4.0, 1.0, -2.0], 7, [-4, -3.0])
  assert_step([2.0, 3.0, 4.0], 2.0, [4])
  assert_step([10.0, 11.0, 12.0, 13.0], 10.0, [], inf: true)
  assert_step([10.0, 7.0, 4.0], 10, [nil, -3.0], inf: true)
  assert_step([1.0], 1, [nil, inf])
  assert_step([1.0], 1, [nil, -inf])
  assert_step([1.0], 1, [3, inf])
  assert_step([], 1, [-3, inf])
  assert_step([], 1, [3, -inf])
  assert_step([1.0], 1, [-3, -inf])
  assert_step([1.0], 1, [inf, inf])
  assert_step([], 1, [inf, -inf])
  assert_step([], 1, [-inf, inf])
  assert_step([1.0], 1, [-inf, -inf])
  assert_step([], inf, [2])
  assert_step([], inf, [-2])
  assert_step([], inf, [2, 3])
  assert_step([inf, inf, inf], inf, [2, -3], inf: true)
  assert_step([], inf, [2, inf])
  assert_step([inf], inf, [2, -inf])
  assert_step([], inf, [-2, inf])
  assert_step([inf], inf, [-2, -inf])
  assert_step([], inf, [-2, 3])
  assert_step([inf, inf, inf], inf, [-2, -3], inf: true)
  assert_step([inf], inf, [inf])
  assert_step([], inf, [-inf])
  assert_step([inf], inf, [inf, inf])
  assert_step([inf], inf, [inf, -inf])
  assert_step([inf], inf, [-inf, -inf])
  assert_step([-inf, -inf, -inf], -inf, [2], inf: true)
  assert_step([-inf, -inf, -inf], -inf, [-2], inf: true)
  assert_step([-inf, -inf, -inf], -inf, [2, 3], inf: true)
  assert_step([], -inf, [2, -3])
  assert_step([-inf], -inf, [2, inf])
  assert_step([], -inf, [2, -inf])
  assert_step([-inf], -inf, [-2, inf])
  assert_step([], -inf, [-2, -inf])
  assert_step([-inf, -inf, -inf], -inf, [-2, 3], inf: true)
  assert_step([], -inf, [-2, -3])
  assert_step([-inf, -inf, -inf], -inf, [inf], inf: true)
  assert_step([-inf], -inf, [-inf])
  assert_step([-inf], -inf, [inf, inf])
  assert_step([], -inf, [inf, -inf])
  assert_step([-inf], -inf, [-inf, -inf])
end

# A value that is no Numeric and takes part in arithmetic through coerce,
# the way a duration or a money amount does
class CoerceTestLength
  attr_reader :n
  def initialize(n); @n = n; end
  def coerce(other); [CoerceTestLength.new(other), self]; end
  def +(o); CoerceTestLength.new(@n + (o.is_a?(CoerceTestLength) ? o.n : o)); end
  def -(o); CoerceTestLength.new(@n - (o.is_a?(CoerceTestLength) ? o.n : o)); end
  def *(o); CoerceTestLength.new(@n * (o.is_a?(CoerceTestLength) ? o.n : o)); end
  def /(o); CoerceTestLength.new(@n / (o.is_a?(CoerceTestLength) ? o.n : o)); end
  def <=>(o); @n <=> (o.is_a?(CoerceTestLength) ? o.n : o); end
  def <(o); (self <=> o) < 0; end
  def >(o); (self <=> o) > 0; end
end

# coerces into a pair whose comparison answers nil
class CoerceTestUnordered
  def initialize(unordered); @unordered = unordered; end
  def coerce(other); [@unordered, self]; end
end

assert('Integer#coerce') do
  assert_equal [2, 1], 1.coerce(2)
  skip unless Object.const_defined?(:Float)
  assert_equal [2.5, 1.0], 1.coerce(2.5)
end

assert('Float#coerce') do
  skip unless Object.const_defined?(:Float)
  assert_equal [2.0, 1.5], 1.5.coerce(2)
  assert_equal [2.5, 1.5], 1.5.coerce(2.5)
end

assert('Integer and Float operators coerce an operand that answers coerce') do
  len = CoerceTestLength.new(3)
  assert_equal 5, (2 + len).n
  assert_equal(-1, (2 - len).n)
  assert_equal 6, (2 * len).n
  assert_equal 2, (6 / len).n
  assert_equal(-1, 2 <=> len)
  assert_true 2 < len
  assert_false 2 > len
  skip unless Object.const_defined?(:Float)
  assert_equal 4.5, (1.5 * len).n
  assert_true 1.5 < len
end

assert('coerce must answer a pair') do
  bad = Object.new
  def bad.coerce(other); :nope; end
  assert_raise(TypeError) { 1 + bad }
end

assert('a comparison through coerce that answers nil fails') do
  unordered = Object.new
  def unordered.<(other); nil; end
  o = CoerceTestUnordered.new(unordered)
  assert_raise(ArgumentError) { 1 < o }
  assert_nil(1 <=> Object.new)
end
