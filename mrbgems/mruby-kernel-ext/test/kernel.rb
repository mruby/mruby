assert('Kernel#fail') do
  assert_raise(RuntimeError) { fail }
end

assert('Kernel.caller, Kernel#caller') do
  skip "backtrace isn't available" if caller(0).empty?

  caller_lineno = __LINE__ + 3
  c = Class.new do
    def foo(*args)
      caller(*args)
    end

    def bar(*args)
      foo(*args)
    end

    def baz(*args)
      bar(*args)
    end
  end

  skip "backtrace isn't available" if (c.new.baz(0)[0].include?("unknown"))

  assert_equal "kernel.rb:#{caller_lineno}:in foo", c.new.baz(0)[0][-19..-1]
  assert_equal "bar", c.new.baz[0][-3..-1]
  assert_equal "foo", c.new.baz(0)[0][-3..-1]
  assert_equal "bar", c.new.baz(1)[0][-3..-1]
  assert_equal "baz", c.new.baz(2)[0][-3..-1]
  assert_equal ["foo", "bar"], c.new.baz(0, 2).map { |i| i[-3..-1] }
  assert_equal ["bar", "baz"], c.new.baz(1..2).map { |i| i[-3..-1] }
  assert_nil c.new.baz(10..20)
  assert_raise(ArgumentError) { c.new.baz(-1) }
  assert_raise(ArgumentError) { c.new.baz(-1, 1) }
  assert_raise(ArgumentError) { c.new.baz(1, -1) }
  assert_raise(TypeError) { c.new.baz(nil) }
end

assert('Kernel.caller_locations, Kernel#caller_locations') do
  skip "backtrace isn't available" if caller(0).empty?

  caller_lineno = __LINE__ + 3
  c = Class.new do
    def foo(*args)
      [caller_locations(*args), caller(*args)]
    end

    def bar(*args)
      foo(*args)
    end

    def baz(*args)
      bar(*args)
    end
  end

  skip "backtrace isn't available" if (c.new.baz(0)[1][0].include?("unknown"))

  locs, strs = c.new.baz(0)
  loc = locs[0]
  assert_kind_of Thread::Backtrace::Location, loc
  assert_equal "kernel.rb", loc.path[-9..-1]
  assert_equal loc.path, loc.absolute_path
  assert_equal caller_lineno, loc.lineno
  assert_equal "foo", loc.label
  assert_equal "foo", loc.base_label
  assert_equal strs[0], loc.to_s
  assert_equal strs[0].inspect, loc.inspect
  assert_equal strs, locs.map { |l| l.to_s }

  [[], [1], [2], [0, 2], [1..2], [1, 0]].each do |args|
    locs, strs = c.new.baz(*args)
    assert_equal strs, locs.map { |l| l.to_s }, "caller_locations(#{args.inspect[1..-2]})"
  end
  assert_nil c.new.baz(10..20)[0]
  assert_nil c.new.baz(1000)[0]
  assert_raise(ArgumentError) { c.new.baz(-1) }
  assert_raise(ArgumentError) { c.new.baz(1, -1) }
  assert_raise(TypeError) { c.new.baz(nil) }
  assert_raise(NoMethodError) { Thread::Backtrace::Location.new }
end

assert('Exception#backtrace_locations') do
  skip "backtrace isn't available" if caller(0).empty?

  assert_nil RuntimeError.new.backtrace_locations

  raise_lineno = __LINE__ + 1
  e = (raise "x" rescue $!)
  locs = e.backtrace_locations
  skip "backtrace isn't available" if locs.empty? || locs[0].to_s.include?("unknown")
  assert_kind_of Thread::Backtrace::Location, locs[0]
  assert_equal raise_lineno, locs[0].lineno
  assert_equal e.backtrace, locs.map { |l| l.to_s }

  # the locations stay once the backtrace is read
  assert_same e.backtrace, e.backtrace
  assert_equal e.backtrace, e.backtrace_locations.map { |l| l.to_s }
end

assert('Kernel#__method__') do
  c = Class.new do
    def m1; __method__ end
    define_method(:m2) {__method__}
    alias m3 m1
    alias_method :m4, :m2
  end
  assert_equal(:m1, c.new.m1)
  assert_equal(:m2, c.new.m2)
  assert_equal(:m1, c.new.m3)
  assert_equal(:m2, c.new.m4)
end

# CRuby __callee__ always gives the calling method name
# mruby __callee__ gives the original method name for aliases
assert('Kernel#__callee__') do
  c = Class.new do
    def m1; __callee__ end
    define_method(:m2) {__callee__}
#    alias m3 m1
#    alias_method :m4, :m2
  end
  assert_equal(:m1, c.new.m1)
  assert_equal(:m2, c.new.m2)
#  assert_equal(:m3, c.new.m3)
#  assert_equal(:m4, c.new.m4)
end

assert('Kernel#Integer') do
  assert_operator(26, :eql?, Integer("0x1a"))
  assert_operator(930, :eql?, Integer("0930", 10))
  assert_operator(7, :eql?, Integer("111", 2))
  assert_operator(0, :eql?, Integer("0"))
  assert_operator(0, :eql?, Integer("00000"))
  assert_operator(123, :eql?, Integer('1_2_3'))
  assert_operator(123, :eql?, Integer("\t\r\n\f\v 123 \t\r\n\f\v"))
  assert_raise(TypeError) { Integer(nil) }
  assert_raise(ArgumentError) { Integer('a') }
  assert_raise(ArgumentError) { Integer('4a5') }
  assert_raise(ArgumentError) { Integer('1_2__3') }
  assert_raise(ArgumentError) { Integer('68_') }
  assert_raise(ArgumentError) { Integer('68_ ') }
  assert_raise(ArgumentError) { Integer('_68') }
  assert_raise(ArgumentError) { Integer(' _68') }
  assert_raise(ArgumentError) { Integer('6 8') }
  assert_raise(ArgumentError) { Integer("15\0") }
  assert_raise(ArgumentError) { Integer("15.0") }
  # negative radix uses its absolute value; out-of-range must raise, and the
  # most-negative mrb_int must not overflow while being negated (#6948)
  assert_operator(7, :eql?, Integer("111", -2))
  assert_raise(ArgumentError) { Integer("z", -37) }
  skip unless Object.const_defined?(:Float)
  assert_operator(123, :eql?, Integer(123.999))
end

assert('Kernel#Integer with a number too big for mrb_int') do
  begin
    big = 1 << 70
  rescue RangeError
    skip 'requires mruby-bigint'
  end
  assert_equal(big, Integer("1180591620717411303424"))
  assert_equal(-big, Integer("-1180591620717411303424"))
  assert_equal(big, Integer("1_180_591_620_717_411_303_424"))
  assert_equal(big, Integer(" 1180591620717411303424\n"))
  assert_equal(big, Integer("0x400000000000000000"))
  assert_equal(big, Integer("1180591620717411303424", 10))
  assert_raise(ArgumentError) { Integer("1180591620717411303424x") }
  assert_raise(ArgumentError) { Integer("1180591620717411303424_") }
  assert_raise(ArgumentError) { Integer("11805916207174113034__24") }
  assert_raise(ArgumentError) { Integer("1180591620717411303424 5") }
  # without the check, "__" still ends the number
  assert_equal(Integer("11805916207174113034"), "11805916207174113034__24".to_i)
  # a digit after the smallest 64-bit mrb_int
  min10 = -(1 << 63) * 10
  assert_equal(min10, Integer("-92233720368547758080"))
  assert_equal(min10, "-92233720368547758080".to_i)
  assert_equal(min10, "-9223372036854775808_0".to_i)
  assert_equal(-(1 << 63), Integer("-0x8000000000000000"))
end

assert('Kernel#Integer with the smallest mrb_int') do
  begin
    min = -(1 << 62) * 2
  rescue RangeError
    skip 'mrb_int narrower than 64 bits, and no mruby-bigint'
  end
  assert_equal(min, Integer("-9223372036854775808"))
  assert_equal(min, Integer("-9223372036854775808 "))
  assert_equal(min, "-9223372036854775808".to_i)
  assert_equal(min, "-9223372036854775808x".to_i)
  assert_raise(ArgumentError) { Integer("-9223372036854775808x") }
  assert_raise(ArgumentError) { Integer("-9223372036854775808_") }
end

assert('Kernel#Float') do
  skip unless Object.const_defined?(:Float)
  assert_operator(1.0, :eql?, Float(1))
  assert_operator(123.456, :eql?, Float(123.456))
  assert_operator(123.456, :eql?, Float("123.456"))
  assert_operator(123.0, :eql?, Float('1_2_3'))
  assert_operator(12.34, :eql?, Float('1_2.3_4'))
  assert_operator(0.9, :eql?, Float('.9'))
  assert_operator(0.9, :eql?, Float(" \t\r\n\f\v.9 \t\r\n\f\v"))
  assert_operator(16.0, :eql?, Float("0x10"))
  assert_operator(0.0, :eql?, Float("0"))
  assert_operator(0.0, :eql?, Float("+0"))
  assert_equal("-0.0", Float("-0").to_s)
  assert_operator(0.0, :eql?, Float("00"))
  assert_operator(0.0, :eql?, Float(" 0 "))
  assert_operator(0.0, :eql?, Float("0_0"))
  assert_operator(0.0, :eql?, Float("0e5"))
  assert_raise(TypeError) { Float(nil) }
  assert_raise(ArgumentError) { Float("1. 5") }
  assert_raise(ArgumentError) { Float("1.5a") }
  assert_raise(ArgumentError) { Float("0a") }
  assert_raise(ArgumentError) { Float("0 0") }
  assert_raise(ArgumentError) { Float("1.5\0") }
  assert_raise(ArgumentError) { Float('a') }
  assert_raise(ArgumentError) { Float('4a5') }
  assert_raise(ArgumentError) { Float('1_2__3') }
  assert_raise(ArgumentError) { Float('68_') }
  assert_raise(ArgumentError) { Float('68._7') }
  assert_raise(ArgumentError) { Float('68.7_') }
  assert_raise(ArgumentError) { Float('68.7_ ') }
  assert_raise(ArgumentError) { Float('_68') }
  assert_raise(ArgumentError) { Float(' _68') }
  assert_raise(ArgumentError) { Float('1_2.3__4') }
end

assert('Kernel#Float with a hexadecimal number too big for mrb_int') do
  skip unless Object.const_defined?(:Float)
  begin
    big = 1 << 64
  rescue RangeError
    skip 'requires mruby-bigint'
  end
  assert_equal(big.to_f, Float("0x10000000000000000"))
  assert_equal((big * big - 1).to_f, Float("0xffffffffffffffffffffffffffffffff"))
end

assert('Kernel#String') do
  assert_equal("main", String(self))
  assert_equal("Object", String(self.class))
  assert_equal("123456", String(123456))
end

assert('Kernel#Array') do
  assert_equal([1], Array(1))
  assert_equal([1, 2, 3, 4, 5], Array([1, 2, 3, 4, 5]))
  assert_equal([1, 2, 3, 4, 5], Array(1..5))
  assert_equal([[:a, 1], [:b, 2], [:c, 3]], Array({a:1, b:2, c:3}))
end

assert('Kernel#Hash') do
  assert_equal({}, Hash([]))
  assert_equal({}, Hash(nil))
  assert_equal({:key => :value}, Hash(key: :value))
  assert_raise(TypeError) { Hash([1, 2, 3]) }
end

assert('Kernel.<conversion> module functions') do
  assert_equal 3, Kernel.Integer("3")
  assert_equal "3", Kernel.String(3)
  assert_equal [1], Kernel.Array([1])
  assert_equal({}, Kernel.Hash(nil))
  assert_nil Kernel.__method__
  assert_raise(RuntimeError) { Kernel.fail "boom" }

  obj = Object.new
  assert_raise(NoMethodError) { obj.Integer("3") }
  assert_raise(NoMethodError) { obj.fail "boom" }
end

assert('Kernel#Integer with exception: false') do
  assert_equal 12, Integer("12", exception: false)
  assert_nil Integer("x", exception: false)
  assert_nil Integer(nil, exception: false)
  assert_nil Integer("12", 2, exception: false)
  assert_equal 7, Integer("111", -2, exception: false)
  assert_equal 12, Integer("12", 0, exception: false)
  assert_equal 12, Integer("12", -1, exception: false)
  assert_equal 12, Integer("12", exception: true)
  assert_raise(ArgumentError) { Integer("x", exception: true) }
end

assert('Kernel#Integer with exception: false still raises on an illegal radix') do
  assert_raise(ArgumentError) { Integer("12", 1, exception: false) }
  assert_raise(ArgumentError) { Integer("12", 37, exception: false) }
  assert_raise(ArgumentError) { Integer("12", -37, exception: false) }
  # a radix given for a non-String is a conversion that fails
  assert_nil Integer(12, 1, exception: false)
  assert_nil Integer(12, 10, exception: false)
end

assert('Kernel#Float with exception: false') do
  skip unless Object.const_defined?(:Float)
  assert_equal 1.5, Float("1.5", exception: false)
  assert_nil Float("x", exception: false)
  assert_nil Float(nil, exception: false)
  assert_raise(ArgumentError) { Float("x") }
end
