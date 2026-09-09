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

assert('Data#to_h with a block') do
  s = Data.define(:white, :red).new('ruuko', 'yuzuki')
  assert_equal({'white' => 'ruuko', 'red' => 'yuzuki'}, s.to_h { |k, v| [k.to_s, v] })
  assert_equal({:white => 'ruuko', :red => 'yuzuki'}, s.to_h(&->(k, v) { [k, v] }))
  assert_equal :stopped, s.to_h { |k, v| break :stopped }
  assert_raise(TypeError)     { s.to_h { |k, v| k } }
  assert_raise(ArgumentError) { s.to_h { |k, v| [k, v, 1] } }
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

assert 'Data with overridden initialize (keyword style)' do
  c = Data.define(:amount, :unit) do
    def initialize(amount:, unit: "USD")
      super(amount: amount, unit: unit.to_s)
    end
  end

  # keyword construction runs through the custom initialize
  a = c.new(amount: 5, unit: :EUR)
  assert_equal 5, a.amount
  assert_equal "EUR", a.unit
  assert_true a.frozen?

  # default value from the custom initialize applies
  assert_equal "USD", c.new(amount: 5).unit

  # positional arguments map to members in order, then run initialize
  b = c.new(5, :JPY)
  assert_equal "JPY", b.unit

  # too many positional arguments
  assert_raise(ArgumentError) { c.new(1, 2, 3) }

  # #with copies stored values and does NOT re-run the custom initialize
  d = Data.define(:v) do
    def initialize(v:); super(v: v * 2); end
  end
  e = d.new(v: 3)             # initialize doubles -> 6
  assert_equal 6, e.v
  assert_equal 5, e.with(v: 5).v   # with bypasses initialize -> 5, not 10
end

assert 'Data with no members' do
  # Data.define takes no members at all, and what it builds is not a broken
  # object: everything that reads the members has to answer for it.
  c = Data.define
  d = c.new

  assert_equal [], c.members
  assert_equal [], d.members
  assert_equal({}, d.to_h)
  assert_equal "#<data >", d.inspect
  assert_equal "#<data >", d.to_s
  assert_equal "#<data >", d.with.inspect
  assert_true d == c.new
  assert_true d.frozen?
end

assert "Data#deconstruct" do
  c = Data.define(:x, :y)
  d = c.new(1, 2)

  assert_equal [1, 2], d.deconstruct

  # the answer is a fresh array; writing to it leaves the object alone
  a = d.deconstruct
  a[0] = 9
  assert_equal 1, d.x

end

assert "Data#deconstruct_keys" do
  c = Data.define(:x, :y, :z)
  d = c.new(1, 2, 3)

  assert_equal({x: 1, y: 2, z: 3}, d.deconstruct_keys(nil))
  assert_equal d.to_h, d.deconstruct_keys(nil)
  assert_equal({}, d.deconstruct_keys([]))
  assert_equal({x: 1}, d.deconstruct_keys([:x]))
  assert_equal({z: 3, x: 1}, d.deconstruct_keys([:z, :x]))

  # a member can also be named by a string
  assert_equal({"x" => 1}, d.deconstruct_keys(["x"]))

  # the hash ends at the first key naming no member
  assert_equal({x: 1}, d.deconstruct_keys([:x, :zz, :y]))
  assert_equal({}, d.deconstruct_keys([:zz]))

  # a Data with no members answers an empty hash rather than refusing
  assert_equal({}, Data.define.new.deconstruct_keys(nil))
  assert_equal [], Data.define.new.deconstruct
  assert_equal({}, d.deconstruct_keys(["zz"]))

  # more keys than members: no pattern over them can match
  assert_equal({}, d.deconstruct_keys([:x, :y, :z, :x]))

  assert_raise(TypeError) { d.deconstruct_keys({x: 1}) }
  assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Array or nil)") do
    d.deconstruct_keys(:x)
  end

  # Data has no positional access, so a key names a member or nothing
  assert_raise_with_message(TypeError, "0 is not a symbol nor a string") do
    d.deconstruct_keys([0])
  end
  assert_raise_with_message(TypeError, "nil is not a symbol nor a string") do
    d.deconstruct_keys([nil])
  end
  assert_raise(TypeError) { d.deconstruct_keys([[]]) }
end

assert "Data in a pattern" do
  c = Data.define(:x, :y)
  d = c.new(1, 2)

  matched = case d
            in [1, v] then v
            end
  assert_equal 2, matched

  matched = case d
            in {y: 2, x: v} then v
            end
  assert_equal 1, matched

  assert_equal :none, (case d
                       in [1] then :one
                       in {x: 2} then :two
                       else :none
                       end)
end
