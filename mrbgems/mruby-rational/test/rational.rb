def assert_rational(exp, real)
  assert "assert_rational" do
    assert_kind_of Rational, real
    # a numerator and a denominator are Integers, so they are compared as
    # such; assert_float asks them for to_f, which a build without Float
    # cannot answer
    assert_equal exp.numerator,   real.numerator
    assert_equal exp.denominator, real.denominator
  end
end

def assert_equal_rational(exp, o1, o2)
  assert "assert_equal_rational" do
    if exp
      assert_operator(o1, :==, o2)
      assert_not_operator(o1, :!=, o2)
    else
      assert_not_operator(o1, :==, o2)
      assert_operator(o1, :!=, o2)
    end
  end
end

def assert_cmp(exp, o1, o2)
  if exp == (o1 <=> o2)
    pass
  else
    flunk "", "    Expected #{o1.inspect} <=> #{o2.inspect} to be #{exp}."
  end
end

def assert_complex(real, imag)
  if Object.const_defined?(:Complex)
    assert "assert_complex" do
      c = yield
      assert_float(real, c.real)
      assert_float(imag, c.imaginary)
    end
  end
end

# A build without Float reads a float literal as Integer 0 and has no Float
# class, so every row that names one has to be asked for only where there is
# one; without this the whole file was left out of such a build instead.
RATIONAL_FLOAT = Object.const_defined?(:Float)

assert 'Rational' do
  r = 5r
  assert_equal(Rational, r.class)
  assert_equal([5, 1], [r.numerator, r.denominator])
end

assert 'Kernel#Rational' do
  r = Rational(4,10)
  assert_equal(2, r.numerator)
  assert_equal(5, r.denominator)

  r = Rational(3)
  assert_equal(3, r.numerator)
  assert_equal(1, r.denominator)

  assert_raise(ArgumentError) { Rational() }
  assert_raise(ArgumentError) { Rational(1,2,3) }
end

assert 'Rational#to_f' do
  skip 'no Float in this build' unless RATIONAL_FLOAT
  assert_float(2.0, Rational(2).to_f)
  assert_float(2.25, Rational(9, 4).to_f)
  assert_float(-0.75, Rational(-3, 4).to_f)
  assert_float(6.666666666666667, Rational(20, 3).to_f)
end

assert 'Rational#to_i' do
  assert_equal(0, Rational(2, 3).to_i)
  assert_equal(3, Rational(3).to_i)
  if RATIONAL_FLOAT
    assert_equal(300, Rational(300.6).to_i)
  end
  assert_equal(1, Rational(98, 71).to_i)
  assert_equal(-15, Rational(-30, 2).to_i)
end

assert 'Rational#*' do
  assert_rational(Rational(4, 9),    Rational(2, 3)  * Rational(2, 3))
  assert_rational(Rational(900, 1),  Rational(900)   * Rational(1))
  assert_rational(Rational(1, 1),    Rational(-2, 9) * Rational(-9, 2))
  assert_rational(Rational(9, 2),    Rational(9, 8)  * 4)
  if RATIONAL_FLOAT
    assert_float(   21.77777777777778, Rational(20, 9) * 9.8)
    assert_float(   21.77777777777778, 9.8 * Rational(20, 9))
    assert_complex(5.2, 2.6) {Rational(13,5)*(2.0+1i)}
    assert_complex(5.2, 2.6) {(2.0+1i)*Rational(13,5)}
  end
end

assert 'Rational#+' do
  assert_rational(Rational(4, 3),     Rational(2, 3)  + Rational(2, 3))
  assert_rational(Rational(901, 1),   Rational(900)   + Rational(1))
  assert_rational(Rational(-85, 18),  Rational(-2, 9) + Rational(-9, 2))
  assert_rational(Rational(41, 8),    Rational(9, 8)  + 4)
  assert_rational(Rational(41, 8),    4 + Rational(9, 8))
  if RATIONAL_FLOAT
    assert_float(   12.022222222222222, Rational(20, 9) + 9.8)
    assert_float(   12.022222222222222, 9.8 + Rational(20, 9))
    assert_complex(24.0, 0) {Rational(24,2)+(12.0+0i)}
    assert_complex(24.0, 0) {(12.0+0i)+Rational(24,2)}
  end
end

assert 'Rational#-' do
  assert_rational(Rational(0, 1),     Rational(2, 3)  - Rational(2, 3))
  assert_rational(Rational(899, 1),   Rational(900)   - Rational(1))
  assert_rational(Rational(77, 18),   Rational(-2, 9) - Rational(-9, 2))
  assert_rational(Rational(23, 8),    4 - Rational(9, 8))
  if RATIONAL_FLOAT
    assert_float(   -7.577777777777778, Rational(20, 9) - 9.8)
    assert_float(    7.577777777777778, 9.8 - Rational(20, 9))
    assert_complex(2.0, 0) {Rational(24,2)-(10.0+0i)}
    assert_complex(2.0, 0) {(14.0+0i)-Rational(24,2)}
  end
end

assert 'Rational#/' do
  assert_rational(Rational(1, 1),      Rational(2, 3)  / Rational(2, 3))
  assert_rational(Rational(900, 1),    Rational(900)   / Rational(1))
  assert_rational(Rational(4, 81),     Rational(-2, 9) / Rational(-9, 2))
  assert_rational(Rational(9, 32),     Rational(9, 8)  / 4)
  assert_rational(Rational(32, 9),     4 / Rational(9, 8))
  if RATIONAL_FLOAT
    assert_float(   0.22675736961451246, Rational(20, 9) / 9.8)
    assert_float(   4.41,                9.8 / Rational(20, 9))
    assert_complex(1.92, 1.44) {Rational(24,2)/(4.0-3i)}
    assert_complex(0.25, 0.25) {(3.0+3i)/Rational(24,2)}
  end
end

assert 'Rational#==, Rational#!=' do
  assert_equal_rational(true, Rational(1,1), Rational(1))
  assert_equal_rational(true, Rational(-1,1), -1r)
  if RATIONAL_FLOAT
    assert_equal_rational(true, Rational(13,4), 3.25)
    assert_equal_rational(true, Rational(13,3.25), Rational(4,1))
  end
  assert_equal_rational(true, Rational(-3,-4), Rational(3,4))
  assert_equal_rational(true, Rational(-4,5), Rational(4,-5))
  assert_equal_rational(true, Rational(4,2), 2)
  assert_equal_rational(true, Rational(-4,2), -2)
  assert_equal_rational(true, Rational(4,-2), -2)
  if RATIONAL_FLOAT
    assert_equal_rational(true, Rational(4,2), 2.0)
    assert_equal_rational(true, Rational(-4,2), -2.0)
    assert_equal_rational(true, Rational(4,-2), -2.0)
  end
  assert_equal_rational(true, Rational(8,6), Rational(4,3))
  assert_equal_rational(false, Rational(13,4), 3)
  if RATIONAL_FLOAT
    assert_equal_rational(false, Rational(13,4), 3.3)
  end
  assert_equal_rational(false, Rational(2,1), 1r)
  assert_equal_rational(false, Rational(1), nil)
  assert_equal_rational(false, Rational(1), '')
end

assert 'Rational#== between bigint-backed rationals' do
  # A bigint-backed Rational is compared by cross-multiplication, which is
  # exact.  The rows below are the ones a comparison through Float gets wrong:
  # at this magnitude a double has no bit left for the difference of 1, so
  # both quotients round to the same value.  The shift count is a variable
  # because a constant shift wider than mrb_int is folded at compile time,
  # which fails the build instead of raising.
  k = 70
  begin
    big = 1 << k
  rescue RangeError
    skip 'requires mruby-bigint'
  end
  assert_equal_rational(false, Rational(big + 1, 3), Rational(big, 3))
  assert_equal_rational(false, Rational(3, big + 1), Rational(3, big))
  assert_equal_rational(true,  Rational(big, 3), Rational(big * 2, 6))
  assert_equal_rational(false, Rational(big, 3), Rational(big, 5))
  assert_equal_rational(false, Rational(-big, 3), Rational(big, 3))
  assert_equal_rational(true,  Rational(-big, 3), Rational(big, -3))
  # One side bigint-backed, the other not: the reduced form of big/big is 1/1
  # but it keeps the bigint representation, so this crosses the two layouts.
  assert_equal_rational(true,  Rational(big, big), Rational(1, 1))
  assert_equal_rational(false, Rational(big, 3), Rational(1, 2))
  assert_equal_rational(true,  Rational(big, 1), big)
  assert_equal_rational(false, Rational(big, 1), big + 1)
  # The same crossing with the bigint-backed side on the right, which reaches
  # the comparison the other way around.
  assert_equal_rational(true,  Rational(1, 1), Rational(big, big))
  assert_equal_rational(true,  Rational(1, 2), Rational(big, big * 2))
  assert_equal_rational(false, Rational(1, 2), Rational(big, 3))
end

assert 'Rational#eql?' do
  assert_true  Rational(2,1).eql?(Rational(2,1))
  assert_true  Rational(1,2).eql?(Rational(2,4))
  assert_false Rational(2,1).eql?(2)
  if RATIONAL_FLOAT
    assert_false Rational(1,2).eql?(0.5)
  end
  assert_false 2.eql?(Rational(2,1))
  assert_false Rational(2,1).eql?(nil)
end

assert 'Integer#==(Rational), Integer#!=(Rational)' do
  assert_equal_rational(true, 2, Rational(4,2))
  assert_equal_rational(true, -2, Rational(-4,2))
  assert_equal_rational(true, -2, Rational(4,-2))
  assert_equal_rational(false, 3, Rational(13,4))
end

assert 'Float#==(Rational), Float#!=(Rational)' do
  skip 'no Float in this build' unless RATIONAL_FLOAT
  assert_equal_rational(true, 2.0, Rational(4,2))
  assert_equal_rational(true, -2.0, Rational(-4,2))
  assert_equal_rational(true, -2.0, Rational(4,-2))
  assert_equal_rational(false, 3.3, Rational(13,4))
end

assert 'Rational#<=>' do
  assert_cmp(-1, Rational(-1), Rational(0))
  assert_cmp(0, Rational(0), Rational(0))
  assert_cmp(1, Rational(1), Rational(0))
  assert_cmp(-1, Rational(-1), 0)
  assert_cmp(0, Rational(0), 0)
  assert_cmp(1, Rational(1), 0)
  if RATIONAL_FLOAT
    assert_cmp(-1, Rational(-1), 0.0)
    assert_cmp(0, Rational(0), 0.0)
    assert_cmp(1, Rational(1), 0.0)
  end
  assert_cmp(-1, Rational(1,2), Rational(2,3))
  assert_cmp(0, Rational(2,3), Rational(2,3))
  assert_cmp(1, Rational(2,3), Rational(1,2))
  assert_cmp(1, Rational(2,3), Rational(1,2))
  assert_cmp(1, Rational(0), Rational(-1))
  assert_cmp(-1, Rational(0), Rational(1))
  assert_cmp(1, Rational(2,3), Rational(1,2))
  assert_cmp(0, Rational(2,3), Rational(2,3))
  assert_cmp(-1, Rational(1,2), Rational(2,3))
  assert_cmp(-1, Rational(1,2), Rational(2,3))
  assert_cmp(nil, 3r, "3")
end

assert 'Rational#<=> is exact' do
  # The comparison cross-multiplies rather than going through Float, so a
  # difference no double can hold is still seen, and there is an answer at all
  # in a build carrying no Float, where every one of these used to be nil.
  #
  # 2**53 and 2**53 + 1 are the first pair a double cannot tell apart.
  m = 1 << 53
  assert_cmp(1,  Rational(m + 1, 1), Rational(m, 1))
  assert_cmp(-1, Rational(m, 1), Rational(m + 1, 1))
  assert_cmp(1,  Rational(m + 1, 3), Rational(m, 3))
  assert_cmp(-1, Rational(3, m + 1), Rational(3, m))
  assert_cmp(0,  Rational(m, 3), Rational(m * 2, 6))
  assert_cmp(0,  Rational(m, 1), m)
  assert_cmp(-1, Rational(m, 1), m + 1)

  # Wide enough that cross-multiplying leaves mrb_int behind, which is where
  # the walk that forms no product answers, or bigint where the build has it.
  begin
    w = 3 * (10 ** 18)
  rescue RangeError
    skip 'no integer this wide'
  end
  assert_cmp(-1, Rational(w, 7), Rational(w + 1, 7))
  assert_cmp(1,  Rational(w + 1, 7), Rational(w, 7))
  assert_cmp(0,  Rational(w, 7), Rational(w, 7))
  assert_cmp(-1, Rational(-w, 7), Rational(w, 7))
  assert_cmp(1,  Rational(-w, 7), Rational(-w - 1, 7))
  assert_cmp(1,  Rational(7, w), Rational(7, w + 1))

  # A bigint-backed rational reads its numerator and denominator out of the
  # other half of a union, so the crossing is asked both ways round: with the
  # bigint side on the left, and with it on the right, where reading it as if
  # it were the narrow layout gives two pointers for two integers.
  begin
    big = 1 << 70
  rescue RangeError
    skip 'requires mruby-bigint'
  end
  assert_cmp(1,  Rational(big + 1, 3), Rational(big, 3))
  assert_cmp(-1, Rational(big, 3), Rational(big + 1, 3))
  assert_cmp(0,  Rational(big, 3), Rational(big * 2, 6))
  assert_cmp(-1, Rational(-big, 3), Rational(big, 3))
  assert_cmp(-1, Rational(1, 2), Rational(big, 3))
  assert_cmp(1,  Rational(big, 3), Rational(1, 2))
  assert_cmp(-1, Rational(1, 2), Rational(big, 1))
  assert_cmp(0,  Rational(big, 1), big)
  assert_cmp(-1, Rational(big, 1), big + 1)
  assert_cmp(1,  Rational(big + 1, 1), big)
  assert_cmp(-1, Rational(3, 1), big)
  assert_cmp(1,  Rational(big, big), Rational(1, 2))
  # Rows where reading the wide layout as the narrow one answers the other
  # way round rather than merely inaccurately: the two pointers it would read
  # sit near each other, so their ratio is close to 1 and every comparison
  # against a value far from 1 comes out backwards.
  assert_cmp(-1, Rational(1000000000, 1), Rational(big, 3))
  assert_cmp(1,  Rational(big, 3), Rational(1000000000, 1))
  assert_cmp(1,  Rational(1, 2), Rational(3, big))
  assert_cmp(-1, Rational(3, big), Rational(1, 2))
  assert_cmp(1,  Rational(big, 3), 5)
  assert_cmp(-1, Rational(3, big), 5)

  # Rows chosen so that the walk that forms no product runs several rounds
  # and one of them divides out exactly, and so that a division truncating
  # towards zero rather than downwards would answer the other way. They are
  # reduced already, and their cross products leave a 64-bit mrb_int; a build
  # whose integers are narrower reaches the same answers through bigint.
  assert_cmp(-1, Rational(-3592724480034102866, 15), Rational(-718544896006820573, 3))
  assert_cmp(-1, Rational(-3257273943195690439, 30), Rational(-542878990532615073, 5))
  assert_cmp(-1, Rational(-3572821367931467021, 15), Rational(-714564273586293404, 3))
  assert_cmp(1,  Rational(380562786494054481, 4), Rational(3044502291952435847, 32))
  assert_cmp(1,  Rational(3028249174067900084, 35), Rational(432607024866842869, 5))
end

assert 'Integer#<=>(Rational)' do
  assert_cmp(-1, -2, Rational(-9,5))
  assert_cmp(0, 5, 5r)
  assert_cmp(1, 3, Rational(8,3))
end

assert 'Float#<=>(Rational)' do
  skip 'no Float in this build' unless RATIONAL_FLOAT
  assert_cmp(-1, -2.1, Rational(-9,5))
  assert_cmp(0, 5.0, 5r)
  assert_cmp(1, 2.7, Rational(8,3))
end

assert 'Rational#<' do
  assert_operator(Rational(1,2), :<, Rational(2,3))
  assert_not_operator(Rational(2,3), :<, Rational(2,3))
  assert_operator(Rational(2,3), :<, 1)
  assert_not_operator(2r, :<, 2)
  assert_not_operator(Rational(2,3), :<, -3)
  if RATIONAL_FLOAT
    assert_operator(Rational(-4,3), :<, -0.3)
    assert_not_operator(Rational(13,4), :<, 3.25)
    assert_not_operator(Rational(2,3), :<, 0.6)
  end
  assert_raise(ArgumentError) { 1r < "2" }
end

assert 'Integer#<(Rational)' do
  assert_not_operator(1, :<, Rational(2,3))
  assert_not_operator(2, :<, 2r)
  assert_operator(-3, :<, Rational(2,3))
end

assert 'Float#<(Rational)' do
  skip 'no Float in this build' unless RATIONAL_FLOAT
  assert_not_operator(-0.3, :<, Rational(-4,3))
  assert_not_operator(3.25, :<, Rational(13,4))
  assert_operator(0.6, :<, Rational(2,3))
end

assert 'Rational#<=' do
  assert_operator(Rational(1,2), :<=, Rational(2,3))
  assert_operator(Rational(2,3), :<=, Rational(2,3))
  assert_operator(Rational(2,3), :<=, 1)
  assert_operator(2r, :<=, 2)
  assert_not_operator(Rational(2,3), :<=, -3)
  if RATIONAL_FLOAT
    assert_operator(Rational(-4,3), :<=, -0.3)
    assert_operator(Rational(13,4), :<=, 3.25)
    assert_not_operator(Rational(2,3), :<=, 0.6)
  end
  assert_raise(ArgumentError) { 1r <= "2" }
end

assert 'Integer#<=(Rational)' do
  assert_not_operator(1, :<=, Rational(2,3))
  assert_operator(2, :<=, 2r)
  assert_operator(-3, :<=, Rational(2,3))
end

assert 'Float#<=(Rational)' do
  skip 'no Float in this build' unless RATIONAL_FLOAT
  assert_not_operator(-0.3, :<=, Rational(-4,3))
  assert_operator(3.25, :<=, Rational(13,4))
  assert_operator(0.6, :<=, Rational(2,3))
end

assert 'Rational#>' do
  assert_not_operator(Rational(1,2), :>, Rational(2,3))
  assert_not_operator(Rational(2,3), :>, Rational(2,3))
  assert_not_operator(Rational(2,3), :>, 1)
  assert_not_operator(2r, :>, 2)
  assert_operator(Rational(2,3), :>, -3)
  if RATIONAL_FLOAT
    assert_not_operator(Rational(-4,3), :>, -0.3)
    assert_not_operator(Rational(13,4), :>, 3.25)
    assert_operator(Rational(2,3), :>, 0.6)
  end
  assert_raise(ArgumentError) { 1r > "2" }
end

assert 'Integer#>(Rational)' do
  assert_operator(1, :>, Rational(2,3))
  assert_not_operator(2, :>, 2r)
  assert_not_operator(-3, :>, Rational(2,3))
end

assert 'Float#>(Rational)' do
  skip 'no Float in this build' unless RATIONAL_FLOAT
  assert_operator(-0.3, :>, Rational(-4,3))
  assert_not_operator(3.25, :>, Rational(13,4))
  assert_not_operator(0.6, :>, Rational(2,3))
end

assert 'Rational#>=' do
  assert_not_operator(Rational(1,2), :>=, Rational(2,3))
  assert_operator(Rational(2,3), :>=, Rational(2,3))
  assert_not_operator(Rational(2,3), :>=, 1)
  assert_operator(2r, :>=, 2)
  assert_operator(Rational(2,3), :>=, -3)
  if RATIONAL_FLOAT
    assert_not_operator(Rational(-4,3), :>=, -0.3)
    assert_operator(Rational(13,4), :>=, 3.25)
    assert_operator(Rational(2,3), :>=, 0.6)
  end
  assert_raise(ArgumentError) { 1r >= "2" }
end

assert 'Integer#>=(Rational)' do
  assert_operator(1, :>=, Rational(2,3))
  assert_operator(2, :>=, 2r)
  assert_not_operator(-3, :>=, Rational(2,3))
end

assert 'Float#>=(Rational)' do
  skip 'no Float in this build' unless RATIONAL_FLOAT
  assert_operator(-0.3, :>=, Rational(-4,3))
  assert_operator(3.25, :>=, Rational(13,4))
  assert_not_operator(0.6, :>=, Rational(2,3))
end

assert 'Rational#negative?' do
  assert_predicate(Rational(-2,3), :negative?)
  assert_predicate(Rational(2,-3), :negative?)
  assert_not_predicate(Rational(2,3), :negative?)
  assert_not_predicate(Rational(0), :negative?)
end

assert 'Rational#frozen?' do
  assert_predicate(1r, :frozen?)
  assert_predicate(Rational(2,3), :frozen?)
  assert_predicate(4/5r, :frozen?)
end

assert 'Rational#**' do
  assert_rational(1r, (14/2r)**0)
  assert_rational(14/2r, (14/2r)**1)
  assert_rational(49r, (14/2r)**2)
  assert_rational(27r, (6/2r)**3)
  if RATIONAL_FLOAT
    assert_float(2.0, (4r)**(1/2r))
  end
  assert_rational(4r, (4r)**(2/2r))
  assert_rational(16r, (4r)**(4/2r))
  if RATIONAL_FLOAT
    assert_float(1.0, (4r)**(0.0))
    assert_float(2.0, (4r)**(0.5))
    assert_float(4.0, (4r)**(1.0))
    assert_float(16.0, (4r)**(2.0))
    assert_float(3.5**1.5, (7/2r)**(1.5))
  end
end

assert 'Rational#** is exact for a whole exponent' do
  # The numerator and the denominator are raised on their own rather than the
  # quotient being handed to pow(), so the answer says what it is and there is
  # an answer at all without a Float. Through Float, Rational(3,7) ** 3 came
  # back as 88627689459915/1125899906842624.
  assert_rational(Rational(1, 1), Rational(3, 7) ** 0)
  assert_rational(Rational(3, 7), Rational(3, 7) ** 1)
  assert_rational(Rational(9, 49), Rational(3, 7) ** 2)
  assert_rational(Rational(27, 343), Rational(3, 7) ** 3)
  assert_rational(Rational(1024, 59049), Rational(2, 3) ** 10)
  assert_rational(Rational(1048576, 3486784401), Rational(2, 3) ** 20)

  # a negative exponent turns the fraction over
  assert_rational(Rational(7, 3), Rational(3, 7) ** -1)
  assert_rational(Rational(49, 9), Rational(3, 7) ** -2)
  assert_rational(Rational(27, 8), Rational(2, 3) ** -3)

  # the sign of the numerator follows the exponent
  assert_rational(Rational(-8, 27), Rational(-2, 3) ** 3)
  assert_rational(Rational(4, 9), Rational(-2, 3) ** 2)

  assert_rational(Rational(0, 1), Rational(0, 5) ** 3)
  assert_rational(Rational(1, 1), Rational(0, 5) ** 0)
  assert_raise(ZeroDivisionError) { Rational(0, 5) ** -1 }

  # a Rational exponent whose denominator is 1 is a whole number too
  assert_rational(Rational(4, 1), Rational(4, 1) ** Rational(2, 2))
  assert_rational(Rational(16, 1), Rational(4, 1) ** Rational(4, 2))
  assert_rational(Rational(9, 49), Rational(3, 7) ** Rational(2, 1))

  # wider than an mrb_int, which is where bigint takes over
  begin
    wide = Rational(2, 3) ** 40
  rescue RangeError
    skip 'requires mruby-bigint'
  end
  assert_rational(Rational(1099511627776, 12157665459056928801), wide)
  assert_rational(Rational(12157665459056928801, 1099511627776), Rational(2, 3) ** -40)
end

assert 'Integer#quo' do
  a = 6.quo(5)
  assert_equal 6/5r, a
end
