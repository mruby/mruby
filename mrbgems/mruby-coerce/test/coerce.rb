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

# answers each operator with its name and operands
class CoerceTestRecorder
  def initialize(v = nil); @v = v; end
  attr_reader :v
  def coerce(other); [CoerceTestRecorder.new(other), self]; end
  %i[+ - * / % ** div quo fdiv divmod <=> < <= > >=].each do |op|
    define_method(op) { |o| [op, @v] }
  end
end

assert('every Integer and Float operator asks coerce') do
  r = CoerceTestRecorder.new
  ops = %i[+ - * / % ** div quo divmod <=> < <= > >=]
  ops << :fdiv if 1.respond_to?(:fdiv)
  ops.each do |op|
    assert_equal [op, 7], 7.__send__(op, r), "7 #{op} r"
  end
  skip unless Object.const_defined?(:Float)
  ops.each do |op|
    # Float#quo and #fdiv are Float#/, as in CRuby
    sent = (op == :quo || op == :fdiv) ? :/ : op
    assert_equal [sent, 7.5], 7.5.__send__(op, r), "7.5 #{op} r"
  end
end

assert('a Bigint receiver asks coerce too') do
  big = (2**70 rescue nil)
  skip unless big
  r = CoerceTestRecorder.new
  %i[+ - * % ** divmod <=>].each do |op|
    assert_equal [op, big], big.__send__(op, r), "big #{op} r"
  end
end

assert('an operand without coerce fails as before') do
  assert_raise(TypeError) { 1 + Object.new }
  assert_raise(TypeError) { 1.5 * Object.new }
  assert_raise(ArgumentError) { 1 < Object.new }
end
