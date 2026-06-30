##
# Struct ISO Test

assert('Data') do
  assert_equal Class, Data.class
end

assert('Data.define') do
  c = Data.define(:m1, :m2)
  assert_equal Data, c.superclass
  assert_equal [:m1, :m2], c.members
end

assert('Data#==') do
  c = Data.define(:m1, :m2)
  cc1 = c.new(1,2)
  cc2 = c.new(1,2)
  assert_true cc1 == cc2
end

assert('Data#members') do
  c = Data.define(:m1, :m2)
  assert_equal [:m1, :m2], c.new(1,2).members
end

assert('wrong struct arg count') do
  c = Data.define(:m1)
  assert_raise ArgumentError do
    cc = c.new(1,2,3)
  end
end

assert('data dup') do
  c = Data.define(:m1, :m2, :m3, :m4, :m5)
  cc = c.new(1,2,3,4,5)
  assert_nothing_raised {
    assert_equal(cc, cc.dup)
  }
end

assert('Data inspect') do
  c = Data.define(:m1, :m2, :m3, :m4, :m5)
  cc = c.new(1,2,3,4,5)
  assert_equal "#<data m1=1, m2=2, m3=3, m4=4, m5=5>", cc.inspect
end

assert('Data#to_h') do
  s = Data.define(:white, :red, :green).new('ruuko', 'yuzuki', 'hitoe')
  assert_equal({:white => 'ruuko', :red => 'yuzuki', :green => 'hitoe'}) { s.to_h }
end

assert("Data.define does not allow array") do
  assert_raise(TypeError) do
    Data.define("Test", [:a])
  end
end

assert("Data.define generates subclass of Data") do
  begin
    original_struct = Data
    Data = String
    assert_equal original_struct, original_struct.define(:foo).superclass
  ensure
    Data = original_struct
  end
end

assert 'Data#freeze' do
  c = Data.define(:m)

  o = c.new(:test)
  assert_equal :test, o.m
  assert_nothing_raised {
    o.freeze
  }
end

assert 'Data#with' do
  c = Data.define(:x, :y)
  a = c.new(1, 2)

  # replace a subset of members, leaving the rest unchanged
  b = a.with(y: 20)
  assert_equal 1, b.x
  assert_equal 20, b.y
  # receiver is not modified
  assert_equal 1, a.x
  assert_equal 2, a.y
  # result is a frozen instance of the same class
  assert_true b.frozen?
  assert_equal c, b.class

  # replace every member
  assert_equal c.new(10, 20), a.with(x: 10, y: 20)

  # no arguments returns the receiver itself
  assert_true a.with.equal?(a)

  # unknown keyword is rejected
  assert_raise(ArgumentError) { a.with(z: 9) }
  # positional arguments are rejected
  assert_raise(ArgumentError) { a.with(1) }
end

assert 'Data#with with more than four members' do
  c = Data.define(:a, :b, :c, :d, :e, :f)
  x = c.new(1, 2, 3, 4, 5, 6)
  y = x.with(a: 10, f: 60)
  assert_equal [10, 2, 3, 4, 5, 60], [y.a, y.b, y.c, y.d, y.e, y.f]
  assert_true y.frozen?
end
