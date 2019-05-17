def assert_rational(real, exp)
  assert_float real.numerator,   exp.numerator
  assert_float real.denominator, exp.denominator
end

def assert_equal_rational(exp, r1, r2)
  if exp
    assert_operator(r1, :==, r2)
    assert_not_operator(r1, :!=, r2)
  else
    assert_not_operator(r1, :==, r2)
    assert_operator(r1, :!=, r2)
  end
end

assert 'Rational' do
  r = 5r
  assert_equal Rational, r.class
  assert_equal [r.numerator, r.denominator], [5, 1]
end

assert 'Rational#to_f' do
  assert_float Rational(2).to_f, 2.0
  assert_float Rational(9, 4).to_f, 2.25
  assert_float Rational(-3, 4).to_f, -0.75
  assert_float Rational(20, 3).to_f, 6.666666666666667
end

assert 'Rational#to_i' do
  assert_equal Rational(2, 3).to_i, 0
  assert_equal Rational(3).to_i, 3
  assert_equal Rational(300.6).to_i, 300
  assert_equal Rational(98, 71).to_i, 1
  assert_equal Rational(-30, 2).to_i, -15
end

assert 'Rational#*' do
  assert_rational Rational(2, 3)  * Rational(2, 3),  Rational(4, 9)
  assert_rational Rational(900)   * Rational(1),     Rational(900, 1)
  assert_rational Rational(-2, 9) * Rational(-9, 2), Rational(1, 1)
  assert_rational Rational(9, 8)  * 4,               Rational(9, 2)
  assert_float    Rational(20, 9) * 9.8,             21.77777777777778
end

assert 'Rational#+' do
  assert_rational Rational(2, 3)  + Rational(2, 3),  Rational(4, 3)
  assert_rational Rational(900)   + Rational(1),     Rational(901, 1)
  assert_rational Rational(-2, 9) + Rational(-9, 2), Rational(-85, 18)
  assert_rational Rational(9, 8)  + 4,               Rational(41, 8)
  assert_float    Rational(20, 9) + 9.8,             12.022222222222222
end

assert 'Rational#-' do
  assert_rational Rational(2, 3)  - Rational(2, 3),  Rational(0, 1)
  assert_rational Rational(900)   - Rational(1),     Rational(899, 1)
  assert_rational Rational(-2, 9) - Rational(-9, 2), Rational(77, 18)
  assert_rational Rational(9, 8)  - 4,               Rational(-23, 8)
  assert_float    Rational(20, 9) - 9.8,             -7.577777777777778
end

assert 'Rational#/' do
  assert_rational Rational(2, 3)  / Rational(2, 3),  Rational(1, 1)
  assert_rational Rational(900)   / Rational(1),     Rational(900, 1)
  assert_rational Rational(-2, 9) / Rational(-9, 2), Rational(4, 81)
  assert_rational Rational(9, 8)  / 4,               Rational(9, 32)
  assert_float    Rational(20, 9) / 9.8,             0.22675736961451246
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

assert 'Rational#negative?' do
  assert_predicate(Rational(-2,3), :negative?)
  assert_predicate(Rational(2,-3), :negative?)
  assert_not_predicate(Rational(2,3), :negative?)
  assert_not_predicate(Rational(0), :negative?)
end
