def assert_rational(exp, real)
  assert_float exp.numerator,   real.numerator
  assert_float exp.denominator, real.denominator
end

def assert_equal_rational(exp, o1, o2)
  if exp
    assert_operator(o1, :==, o2)
    assert_not_operator(o1, :!=, o2)
  else
    assert_not_operator(o1, :==, o2)
    assert_operator(o1, :!=, o2)
  end
end

def assert_cmp(exp, o1, o2)
  if exp == (o1 <=> o2)
    pass
  else
    flunk "", "    Expected #{o1.inspect} <=> #{o2.inspect} to be #{exp}"
  end
end

assert 'Rational' do
  r = 5r
  assert_equal(Rational, r.class)
  assert_equal([5, 1], [r.numerator, r.denominator])
end

assert 'Rational#to_f' do
  assert_float(2.0, Rational(2).to_f)
  assert_float(2.25, Rational(9, 4).to_f)
  assert_float(-0.75, Rational(-3, 4).to_f)
  assert_float(6.666666666666667, Rational(20, 3).to_f)
end

assert 'Rational#to_i' do
  assert_equal(0, Rational(2, 3).to_i)
  assert_equal(3, Rational(3).to_i)
  assert_equal(300, Rational(300.6).to_i)
  assert_equal(1, Rational(98, 71).to_i)
  assert_equal(-15, Rational(-30, 2).to_i)
end

assert 'Rational#*' do
  assert_rational(Rational(4, 9),    Rational(2, 3)  * Rational(2, 3))
  assert_rational(Rational(900, 1),  Rational(900)   * Rational(1))
  assert_rational(Rational(1, 1),    Rational(-2, 9) * Rational(-9, 2))
  assert_rational(Rational(9, 2),    Rational(9, 8)  * 4)
  assert_float(   21.77777777777778, Rational(20, 9) * 9.8)
end

assert 'Rational#+' do
  assert_rational(Rational(4, 3),     Rational(2, 3)  + Rational(2, 3))
  assert_rational(Rational(901, 1),   Rational(900)   + Rational(1))
  assert_rational(Rational(-85, 18),  Rational(-2, 9) + Rational(-9, 2))
  assert_rational(Rational(41, 8),    Rational(9, 8)  + 4)
  assert_float(   12.022222222222222, Rational(20, 9) + 9.8)
end

assert 'Rational#-' do
  assert_rational(Rational(0, 1),     Rational(2, 3)  - Rational(2, 3))
  assert_rational(Rational(899, 1),   Rational(900)   - Rational(1))
  assert_rational(Rational(77, 18),   Rational(-2, 9) - Rational(-9, 2))
  assert_rational(Rational(-23, 8),   Rational(9, 8)  - 4)
  assert_float(   -7.577777777777778, Rational(20, 9) - 9.8)
end

assert 'Rational#/' do
  assert_rational(Rational(1, 1),      Rational(2, 3)  / Rational(2, 3))
  assert_rational(Rational(900, 1),    Rational(900)   / Rational(1))
  assert_rational(Rational(4, 81),     Rational(-2, 9) / Rational(-9, 2))
  assert_rational(Rational(9, 32),     Rational(9, 8)  / 4)
  assert_float(   0.22675736961451246, Rational(20, 9) / 9.8)
end

assert 'Rational#==, Rational#!=' do
  assert_equal_rational(true, Rational(1,1), Rational(1))
  assert_equal_rational(true, Rational(-1,1), -1r)
  assert_equal_rational(true, Rational(13,4), 3.25)
  assert_equal_rational(true, Rational(13,3.25), Rational(4,1))
  assert_equal_rational(true, Rational(-3,-4), Rational(3,4))
  assert_equal_rational(true, Rational(-4,5), Rational(4,-5))
  assert_equal_rational(true, Rational(4,2), 2)
  assert_equal_rational(true, Rational(-4,2), -2)
  assert_equal_rational(true, Rational(4,-2), -2)
  assert_equal_rational(true, Rational(4,2), 2.0)
  assert_equal_rational(true, Rational(-4,2), -2.0)
  assert_equal_rational(true, Rational(4,-2), -2.0)
  assert_equal_rational(true, Rational(8,6), Rational(4,3))
  assert_equal_rational(false, Rational(13,4), 3)
  assert_equal_rational(false, Rational(13,4), 3.3)
  assert_equal_rational(false, Rational(2,1), 1r)
  assert_equal_rational(false, Rational(1), nil)
  assert_equal_rational(false, Rational(1), '')
end

assert 'Fixnum#==(Rational), Fixnum#!=(Rational)' do
  assert_equal_rational(true, 2, Rational(4,2))
  assert_equal_rational(true, -2, Rational(-4,2))
  assert_equal_rational(true, -2, Rational(4,-2))
  assert_equal_rational(false, 3, Rational(13,4))
end

assert 'Float#==(Rational), Float#!=(Rational)' do
  assert_equal_rational(true, 2.0, Rational(4,2))
  assert_equal_rational(true, -2.0, Rational(-4,2))
  assert_equal_rational(true, -2.0, Rational(4,-2))
  assert_equal_rational(false, 3.3, Rational(13,4))
end

assert 'Rational#<=>' do
  num = Class.new(Numeric) do
    def initialize(n)
      @n = n
    end

    def <=>(rhs)
      rhs = rhs.to_i
      rhs < 0 ? nil : @n <=> rhs
    end

    def inspect
      "num(#{@n})"
    end
  end

  assert_cmp(-1, Rational(-1), Rational(0))
  assert_cmp(0, Rational(0), Rational(0))
  assert_cmp(1, Rational(1), Rational(0))
  assert_cmp(-1, Rational(-1), 0)
  assert_cmp(0, Rational(0), 0)
  assert_cmp(1, Rational(1), 0)
  assert_cmp(-1, Rational(-1), 0.0)
  assert_cmp(0, Rational(0), 0.0)
  assert_cmp(1, Rational(1), 0.0)
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
  assert_cmp(1, 3r, num.new(2))
  assert_cmp(0, 3r, num.new(3))
  assert_cmp(-1, 3r, num.new(4))
  assert_cmp(nil, Rational(-3), num.new(5))
end

assert 'Fixnum#<=>(Rational)' do
  assert_cmp(-1, -2, Rational(-9,5))
  assert_cmp(0, 5, 5r)
  assert_cmp(1, 3, Rational(8,3))
end

assert 'Float#<=>(Rational)' do
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
  assert_operator(Rational(-4,3), :<, -0.3)
  assert_not_operator(Rational(13,4), :<, 3.25)
  assert_not_operator(Rational(2,3), :<, 0.6)
  assert_raise(ArgumentError) { 1r < "2" }
end

assert 'Fixnum#<(Rational)' do
  assert_not_operator(1, :<, Rational(2,3))
  assert_not_operator(2, :<, 2r)
  assert_operator(-3, :<, Rational(2,3))
end

assert 'Float#<(Rational)' do
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
  assert_operator(Rational(-4,3), :<=, -0.3)
  assert_operator(Rational(13,4), :<=, 3.25)
  assert_not_operator(Rational(2,3), :<=, 0.6)
  assert_raise(ArgumentError) { 1r <= "2" }
end

assert 'Fixnum#<=(Rational)' do
  assert_not_operator(1, :<=, Rational(2,3))
  assert_operator(2, :<=, 2r)
  assert_operator(-3, :<=, Rational(2,3))
end

assert 'Float#<=(Rational)' do
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
  assert_not_operator(Rational(-4,3), :>, -0.3)
  assert_not_operator(Rational(13,4), :>, 3.25)
  assert_operator(Rational(2,3), :>, 0.6)
  assert_raise(ArgumentError) { 1r > "2" }
end

assert 'Fixnum#>(Rational)' do
  assert_operator(1, :>, Rational(2,3))
  assert_not_operator(2, :>, 2r)
  assert_not_operator(-3, :>, Rational(2,3))
end

assert 'Float#>(Rational)' do
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
  assert_not_operator(Rational(-4,3), :>=, -0.3)
  assert_operator(Rational(13,4), :>=, 3.25)
  assert_operator(Rational(2,3), :>=, 0.6)
  assert_raise(ArgumentError) { 1r >= "2" }
end

assert 'Fixnum#>=(Rational)' do
  assert_operator(1, :>=, Rational(2,3))
  assert_operator(2, :>=, 2r)
  assert_not_operator(-3, :>=, Rational(2,3))
end

assert 'Float#>=(Rational)' do
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
