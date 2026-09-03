##
# Integer ISO Test

assert('Integer', '15.2.8') do
  assert_equal Class, Integer.class
end

assert('Integer#+', '15.2.8.3.1') do
  a = 1+1
  b = 1+1.0 if Object.const_defined?(:Float)

  assert_equal 2, a
  assert_equal 2.0, b if Object.const_defined?(:Float)

  assert_raise(TypeError){ 0+nil }
  assert_raise(TypeError){ 1+nil }
end

assert('Integer#-', '15.2.8.3.2') do
  a = 2-1
  b = 2-1.0 if Object.const_defined?(:Float)

  assert_equal 1, a
  assert_equal 1.0, b if Object.const_defined?(:Float)
end

assert('Integer#*', '15.2.8.3.3') do
  a = 1*1
  assert_equal 1, a
  if Object.const_defined?(:Float)
    b = 1*1.0
    assert_equal 1.0, b
  end
  assert_raise(TypeError){ 0*nil }
  assert_raise(TypeError){ 1*nil }
end

assert('Integer#/', '15.2.8.3.4') do
  a = 2/1
  assert_equal 2, a
  a = 5/2
  assert_equal 2, a
  b = -1/2
  assert_equal(-1, b)
  b = 1/-2
  assert_equal(-1, b)
  skip unless Object.const_defined?(:Float)
  b = 2/1.0
  assert_equal 2.0, b
end

if Object.const_defined?(:Float)
  assert('Integer#quo') do
    a = 6.quo(5)
    assert_equal 1.2, a
  end
end

assert('Integer#%', '15.2.8.3.5') do
  a = 1%1
  b = 2%4
  c = 2%5
  d = 2%-5
  e = -2%5
  f = -2%-5
  g = 2%-2
  h = -2%2
  i = -2%-2

  assert_equal 0, a
  assert_equal 2, b
  assert_equal 2, c
  assert_equal(-3, d)
  assert_equal 3, e
  assert_equal(-2, f)
  assert_equal 0, g
  assert_equal 0, h
  assert_equal 0, i
  skip unless Object.const_defined?(:Float)
  j = 1%1.0
  assert_equal 0.0, j
end

assert('Integer#<=>', '15.2.9.3.6') do
  a = 1<=>0
  b = 1<=>1
  c = 1<=>2

  assert_equal  1, a
  assert_equal  0, b
  assert_equal(-1, c)
end

assert('Integer#==', '15.2.8.3.7') do
  a = 1==0
  b = 1==1

  assert_false a
  assert_true b
end

assert('Integer comparison with a Float that stands for another number') do
  # An mrb_float keeps fewer significant bits than an mrb_int, so an integer
  # past the significand rounds onto a neighbouring Float when the two are
  # compared as Floats, and answers equal to a Float it is not equal to.
  #
  # The pair is derived rather than written out. 2 ** p, for the p significant
  # bits this build's Float holds, is the first Float that cannot be told from
  # the integer above it, so the pair stays right where mrb_float is narrower
  # than the usual 53 bits. A literal that wide cannot be built where mrb_int
  # is narrow either, and failing to build one drops every test in this file.
  #
  # Where mrb_int cannot hold the integer, mruby-bigint answers the assertions
  # below through mrb_bint_cmp() instead, which is exact for its own reasons;
  # without that gem the build has no such integer at all and this skips.
  skip unless Object.const_defined?(:Float)
  f = 2.0
  f *= 2 while f + 1.0 != f
  begin
    n = f.to_i + 1
  rescue RangeError
    skip 'no mrb_int here is wider than the significand of an mrb_float'
  end

  assert_false(n == f)
  assert_false(n.__send__(:==, f))    # the method, where the line above is an opcode
  assert_equal(1, n <=> f)
  assert_true(n > f)
  assert_true(n.__send__(:>, f))
  assert_true(n >= f)
  assert_false(n < f)
  assert_false(n <= f)
  assert_false([n] == [f])        # Array#== reads the same comparison
end

assert('Integer comparison with a NaN') do
  # The mixed pair is compared exactly rather than as two Floats, and that
  # comparison reports a NaN apart; the answer it stands for is the one a pair
  # of Floats gets, no order at all. See the Float file for what that means.
  skip unless Object.const_defined?(:Float)
  nan = Float::NAN

  assert_nil(1 <=> nan)
  assert_false(1.__send__(:<, nan))
  assert_false(1.__send__(:<=, nan))
  assert_false(1.__send__(:>, nan))
  assert_false(1.__send__(:>=, nan))
  assert_false(1 < nan)           # the opcode, which already answered this
end

assert('Integer wider than an mrb_int compared with a NaN') do
  # A big integer is compared by a path of its own, which reported the NaN as a
  # pair it could not compare at all: `<=>` was already nil, but the four
  # operators raised where the ones above answered false.
  skip unless Object.const_defined?(:Float)
  begin
    big = 1 << 70
  rescue RangeError
    skip 'no integer here is wider than an mrb_int'
  end
  nan = Float::NAN

  assert_nil(big <=> nan)
  assert_nil(nan <=> big)
  assert_false(big.__send__(:<, nan))
  assert_false(big.__send__(:>=, nan))
  assert_false(nan.__send__(:<, big))
  assert_false(nan.__send__(:>=, big))
end

assert('Integer comparison with a Float at the ends of the mrb_int range') do
  # The far ends are where the neighbouring Floats stand furthest apart, and
  # where the bounds the comparison tests against are themselves built. Both
  # ends come from arithmetic for the reason the pair above does.
  #
  # A narrower mrb_int leaves the shift outside the range these name, and the
  # two cases part the same way as above: with mruby-bigint the assertions
  # still hold, answered by mrb_bint_cmp() rather than by the bounds, and
  # without it the shift raises and this skips.
  skip unless Object.const_defined?(:Float)
  begin
    half = 1 << 62
  rescue RangeError
    skip 'an mrb_int here is narrower than 64 bits'
  end
  imin = -half - half             # the least mrb_int, which is exact as a Float
  imax = half - 1 + half          # the greatest, which is not
  fmin = -(2.0 ** 63)
  fmax = 2.0 ** 63
  inf = 1.0 / 0.0

  assert_true(imin == fmin)
  assert_equal(0, imin <=> fmin)
  assert_equal(1, (imin + 3) <=> fmin)
  assert_true((imin + 3) > fmin)
  assert_false(imax == fmax)
  assert_equal(-1, imax <=> fmax)
  assert_true(imax < fmax)
  assert_true(imax < inf)
  assert_true(imin > -inf)
end

assert('Integer comparison of two values too wide to store inline') do
  # Word boxing keeps an Integer in the value itself only while it fits a
  # tagged machine word and allocates an object for a wider one, which is a
  # difference in where the number is kept and not in the number: a pair of
  # them stands in the same order as any other pair. The comparisons are asked
  # for by name so that each is the method's own, the operators being opcodes
  # that read a pair of Integers themselves.
  #
  # 2**30 is past what a 32-bit word carries inline and 2**62 past a 64-bit
  # one, so one pair or the other is outside it wherever this runs. Where an
  # mrb_int is too narrow for the wider pair, mruby-bigint answers the
  # comparison and a build without it raises at the shift, which is what the
  # rescue leaves that pair out for; the shift count is a variable for the
  # reason the big integer tests give, a constant one out of range failing the
  # build rather than raising.
  pairs = [[1 << 30, (1 << 30) + 1]]
  begin
    k = 62
    pairs << [1 << k, (1 << k) + 1]
  rescue RangeError
  end

  pairs.each do |small, large|
    assert_equal(-1, small.__send__(:<=>, large))
    assert_equal(1, large.__send__(:<=>, small))
    assert_equal(0, large.__send__(:<=>, large))
    assert_true(small.__send__(:<, large))
    assert_true(small.__send__(:<=, large))
    assert_false(small.__send__(:>, large))
    assert_false(small.__send__(:>=, large))
    assert_true(large.__send__(:>, small))
  end
end

assert('Integer#~', '15.2.8.3.8') do
  # Complement
  assert_equal(-1, ~0)
  assert_equal(-3, ~2)
end

assert('Integer#&', '15.2.8.3.9') do
  # Bitwise AND
  #   0101 (5)
  # & 0011 (3)
  # = 0001 (1)
  assert_equal 1, 5 & 3
end

assert('Integer#|', '15.2.8.3.10') do
  # Bitwise OR
  #   0101 (5)
  # | 0011 (3)
  # = 0111 (7)
  assert_equal 7, 5 | 3
end

assert('Integer#^', '15.2.8.3.11') do
  # Bitwise XOR
  #   0101 (5)
  # ^ 0011 (3)
  # = 0110 (6)
  assert_equal 6, 5 ^ 3
end

assert('Integer bitwise ops reject non-Integer operands') do
  # A non-Integer operand has no bit pattern to combine, so &, |, ^ raise
  # TypeError instead of silently reading garbage (Float used to return a
  # bogus value via an unchecked union access).
  if Object.const_defined?(:Float)
    # Without float support, a float literal compiles to the integer 0, so
    # these expressions are ordinary integer bitwise ops rather than errors.
    assert_raise(TypeError) { 5 & 5.0 }
    assert_raise(TypeError) { 5 | 5.0 }
    assert_raise(TypeError) { 5 ^ 5.0 }
  end
  assert_raise(TypeError) { 5 | "3" }
  assert_raise(TypeError) { 5 & nil }
  assert_raise(TypeError) { 5 ^ :sym }
end

assert('Integer#<<', '15.2.8.3.12') do
  # Left Shift by one
  #   00010111 (23)
  # = 00101110 (46)
  assert_equal 46, 23 << 1

  # Left Shift by a negative is Right Shift
  assert_equal 23, 46 << -1

  skip unless Object.const_defined?(:Float)
end

assert('Integer#>>', '15.2.8.3.13') do
  # Right Shift by one
  #   00101110 (46)
  # = 00010111 (23)
  assert_equal 23, 46 >> 1

  # Right Shift by a negative is Left Shift
  assert_equal 46, 23 >> -1

  # Don't raise on large Right Shift
  assert_equal 0, 23 >> 128
end

assert('Integer#ceil', '15.2.8.3.14') do
  assert_equal 10, 10.ceil
  assert_equal 10, 10.ceil(2)
  assert_equal 1300, 1234.ceil(-2)
  assert_equal(-1200, -1234.ceil(-2))
  assert_equal 0, (-99).ceil(-2)
end

assert('Integer#ceil keeps a value that is already a multiple') do
  assert_equal 0, 0.ceil(-1)
  assert_equal 10, 10.ceil(-1)
  assert_equal 100, 100.ceil(-2)
  assert_equal 1200, 1200.ceil(-2)
  assert_equal(-100, -100.ceil(-2))
  assert_equal(-1200, -1200.ceil(-2))
end

assert('Integer#downto', '15.2.8.3.15') do
  a = 0
  3.downto(1) do |i|
    a += i
  end
  assert_equal 6, a
end

assert('Integer#eql?', '15.2.8.3.16') do
  a = 1.eql?(1)
  b = 1.eql?(2)
  c = 1.eql?(nil)

  assert_true a
  assert_false b
  assert_false c

  # `eql?` compares class as well as value, unlike `==`.
  if Object.const_defined?(:Float)
    assert_false 1.eql?(1.0)
    assert_true 1 == 1.0
  end
end

assert('Integer#floor', '15.2.8.3.17') do
  a = 1.floor

  assert_equal 1, a
  assert_equal 1, 1.floor(2)
  assert_equal 1200, 1234.floor(-2)
  assert_equal(-1300, -1234.floor(-2))
  assert_equal 0, 99.floor(-2)
end

assert('Integer#floor keeps a value that is already a multiple') do
  assert_equal 0, 0.floor(-1)
  assert_equal(-10, -10.floor(-1))
  assert_equal(-100, -100.floor(-2))
  assert_equal(-1200, -1200.floor(-2))
  assert_equal 100, 100.floor(-2)
  assert_equal 1200, 1200.floor(-2)
end

assert('Integer#next', '15.2.8.3.19') do
  assert_equal 2, 1.next
end

assert('Integer#round', '15.2.8.3.20') do
  assert_equal 1, 1.round
  assert_equal 1, 1.round(2)
  assert_equal 12300, 12345.round(-2)
  assert_equal 12350, 12345.round(-1)
  assert_equal 12000, 12345.round(-3)
  assert_equal(-12300, -12345.round(-2))
  assert_equal 0, 4.round(-1)
  assert_equal 100, 99.round(-2)
  assert_equal 0, 0.round(-1)
end

assert('Integer#round keeps a value that is already a multiple') do
  assert_equal 10, 10.round(-1)
  assert_equal 100, 100.round(-2)
  assert_equal 1200, 1200.round(-2)
  assert_equal(-100, -100.round(-2))
  assert_equal(-1200, -1200.round(-2))
end

assert('Integer#round breaks a tie away from zero') do
  assert_equal 10, 5.round(-1)
  assert_equal 20, 15.round(-1)
  assert_equal 30, 25.round(-1)
  assert_equal 200, 150.round(-2)
  assert_equal 300, 250.round(-2)
  assert_equal(-10, -5.round(-1))
  assert_equal(-20, -15.round(-1))
  assert_equal(-30, -25.round(-1))
  assert_equal(-200, -150.round(-2))
end

assert('Integer#succ', '15.2.8.3.21') do
  assert_equal 2, 1.succ
end

assert('Integer#times', '15.2.8.3.22') do
  a = 0
  3.times do
    a += 1
  end
  assert_equal 3, a
end

assert('Integer#to_f', '15.2.8.3.23') do
  skip unless Object.const_defined?(:Float)
  assert_equal 1.0, 1.to_f
end

assert('Integer#to_i', '15.2.8.3.24') do
  assert_equal 1, 1.to_i
end

assert('Integer#to_s', '15.2.8.3.25') do
  assert_equal "1", 1.to_s
  assert_equal "-1", -1.to_s
  assert_equal "1010", 10.to_s(2)
  assert_equal "a", 10.to_s(36)
  assert_equal "-a", -10.to_s(36)
  assert_equal "30071", 12345.to_s(8)
  assert_raise(ArgumentError) { 10.to_s(-1) }
  assert_raise(ArgumentError) { 10.to_s(0) }
  assert_raise(ArgumentError) { 10.to_s(1) }
  assert_raise(ArgumentError) { 10.to_s(37) }
end

assert('Integer#truncate', '15.2.8.3.26') do
  assert_equal 1, 1.truncate
  assert_equal 1, 1.truncate(2)
  assert_equal 1200, 1234.truncate(-2)
  assert_equal(-1200, -1234.truncate(-2))
  assert_equal 1200, 1200.truncate(-2)
  assert_equal 0, 99.truncate(-2)
end

assert('Integer rounding to a power of ten wider than an mrb_int') do
  # 10 ** 19 is wider than an mrb_int, so the four methods answer 0 rather than
  # reaching for a power they cannot hold.
  (19..25).each do |n|
    assert_equal 0, 12345.round(-n)
    assert_equal 0, (-12345).round(-n)
    assert_equal 0, 12345.truncate(-n)
    assert_equal 0, 12345.floor(-n)
    assert_equal 0, (-12345).ceil(-n)
  end
end

assert('Integer#upto', '15.2.8.3.27') do
  a = 0
  1.upto(3) do |i|
    a += i
  end
  assert_equal 6, a
end

assert('Integer#divmod', '15.2.8.3.30') do
  assert_equal [ 0,  0],   0.divmod(1)
  assert_equal [ 0,  1],   1.divmod(3)
  assert_equal [ 3,  0],   3.divmod(1)
  assert_equal [ 2,  6],  20.divmod(7)
  assert_equal [-1,  2],  -3.divmod(5)
  assert_equal [-2, -1],  25.divmod(-13)
  assert_equal [ 1, -6], -13.divmod(-7)
end

assert('Integer.__ensure converts its argument without dispatching') do
  # The mrblib idiom used to be `obj.__to_int`, a dispatch the argument could
  # redefine, so an object defining `__to_int` was accepted everywhere an
  # object defining `to_int` is rejected.
  evil = Class.new { def __to_int; 2; end }.new
  assert_raise(TypeError) { Integer.__ensure(evil) }
  assert_raise(TypeError) { Integer.__ensure(Class.new { def to_int; 2; end }.new) }

  assert_equal 2, Integer.__ensure(2)
  assert_equal 1, Integer.__ensure(1.9) if Object.const_defined?(:Float)
  assert_raise(TypeError) { Integer.__ensure("2") }
  assert_raise(TypeError) { Integer.__ensure(nil) }
end
