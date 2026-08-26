assert('Comparable#<', '15.3.3.2.1') do
  class Foo
    include Comparable
    def <=>(x)
      x
    end
  end
  assert_false(Foo.new < 0)
  assert_false(Foo.new < 1)
  assert_true(Foo.new < -1)
  assert_raise(ArgumentError){ Foo.new < nil }
end

assert('Comparable#<=', '15.3.3.2.2') do
  class Foo
    include Comparable
    def <=>(x)
      x
    end
  end
  assert_true(Foo.new <= 0)
  assert_false(Foo.new <= 1)
  assert_true(Foo.new <= -1)
  assert_raise(ArgumentError){ Foo.new <= nil }
end

assert('Comparable#==', '15.3.3.2.3') do
  class Foo
    include Comparable
    def <=>(x)
      0
    end
  end

  assert_true(Foo.new == Foo.new)
end

assert('Comparable#>', '15.3.3.2.4') do
  class Foo
    include Comparable
    def <=>(x)
      x
    end
  end
  assert_false(Foo.new > 0)
  assert_true(Foo.new > 1)
  assert_false(Foo.new > -1)
  assert_raise(ArgumentError){ Foo.new > nil }
end

assert('Comparable#>=', '15.3.3.2.5') do
  class Foo
    include Comparable
    def <=>(x)
      x
    end
  end
  assert_true(Foo.new >= 0)
  assert_true(Foo.new >= 1)
  assert_false(Foo.new >= -1)
  assert_raise(ArgumentError){ Foo.new >= nil }
end

assert('Comparable#between?', '15.3.3.2.6') do
  class Foo
    include Comparable
    def <=>(x)
      x
    end
  end

  c = Foo.new

  assert_false(c.between?(-1,  1))
  assert_false(c.between?(-1, -1))
  assert_false(c.between?( 1,  1))
  assert_true(c.between?( 1, -1))
  assert_true(c.between?(0, 0))
end

assert('Comparable#between? - a comparison with no answer') do
  # #between? places the receiver against both bounds, so a pair that stands in
  # no order is refused rather than placed, as in Comparable#clamp.
  unordered = Class.new {
    include Comparable
    def <=>(other)
      nil
    end
  }.new
  assert_raise(ArgumentError) { unordered.between?(1, 2) }

  if Object.const_defined?(:Float)
    assert_raise(ArgumentError) { Float::NAN.between?(1, 2) }
    assert_raise(ArgumentError) { 1.between?(Float::NAN, 2) }
    assert_raise(ArgumentError) { 1.0.between?(0.0, Float::NAN) }
  end

  # A pair of different kinds stands in no order either.
  assert_raise(ArgumentError) { 1.between?('a', 2) }
  assert_raise(ArgumentError) { 3.between?(1, 'a') }

  # The max bound is not asked for once the receiver is below the min.
  assert_false(1.between?(2, 'a'))

  assert_true(2.between?(1, 3))
  assert_false(4.between?(1, 3))
end
