# The exact-division path of mruby-complex answers in Rationals, so its
# tests need both gems in the state. This gem test-depends on mruby-complex
# wherever the build has a Float, which is the state these run in; the
# complex gem cannot declare the mirror dependency without closing a cycle.
# An MRB_COMPLEX_FLOAT_ONLY build has no exact parts to ask about, which is
# what the second question detects.

if Object.const_defined?(:Complex) && Complex(1, 0).real.class == Integer
  assert 'Complex#/ is exact without a Float part' do
    c = Complex(1, 2) / 2
    assert_equal Rational(1, 2), c.real
    assert_equal Rational, c.real.class
    assert_equal 1, c.imaginary
    assert_equal Integer, c.imaginary.class

    c = Complex(9, 8) / 4
    assert_equal Rational(9, 4), c.real
    assert_equal 2, c.imaginary

    c = Complex(1, 2) / Complex(3, 4)
    assert_equal Rational(11, 25), c.real
    assert_equal Rational(2, 25), c.imaginary

    c = 1 / Complex(1, 2)
    assert_equal Rational(1, 5), c.real
    assert_equal Rational(-2, 5), c.imaginary

    assert_raise(ZeroDivisionError) { Complex(1, 2) / 0 }
    assert_raise(ZeroDivisionError) { Complex(1, 2) / Complex(0, 0) }
  end

  assert 'Complex#/ with a Float anywhere divides that part as a float' do
    c = Complex(2.0, 2) / 2
    assert_float 1.0, c.real
    assert_equal Float, c.real.class
    assert_equal 1, c.imaginary
    assert_equal Integer, c.imaginary.class

    c = Complex(1, 2) / 2.0
    assert_equal Float, c.real.class
    assert_float 0.5, c.real
  end

  assert 'Complex#** with a negative exponent is exact' do
    c = Complex(1, 2) ** -2
    assert_equal Rational(-3, 25), c.real
    assert_equal Rational(-4, 25), c.imaginary
  end

  assert 'Complex holds a Rational part' do
    c = Complex(Rational(1, 3), 0)
    assert_equal Rational(1, 3), c.real
    assert_equal "1/3+0i", c.to_s
    assert_equal "((1/3)+0i)", c.inspect
    assert_equal "(0+(1/2)*i)", Complex(0, Rational(1, 2)).inspect
    assert_equal "(0-(1/2)*i)", Complex(0, Rational(-1, 2)).inspect
    assert_equal "0-1/2i", Complex(0, Rational(-1, 2)).to_s
  end

  assert 'Complex#to_r keeps a Rational real part' do
    assert_equal Rational(1, 3), Complex(Rational(1, 3), 0).to_r
    assert_equal Rational(1, 1), Complex(1, 0).to_r
  end

  assert 'Complex#to_i truncates a Rational real part' do
    assert_equal 3, Complex(Rational(7, 2), 0).to_i
    assert_equal(-3, Complex(Rational(-7, 2), 0).to_i)
  end

  assert 'a Rational scalar touches only the part it lands on' do
    c = Rational(1, 2) + Complex(1, 2)
    assert_equal Rational(3, 2), c.real
    assert_equal 2, c.imaginary
    assert_equal Integer, c.imaginary.class

    c = Complex(Rational(1, 2), 3) * 2
    assert_equal Rational(1, 1), c.real
    assert_equal 6, c.imaginary
    assert_equal Integer, c.imaginary.class
  end

  assert 'Complex#== reaches a Rational part' do
    assert_true Complex(Rational(1, 2), 0) == Rational(1, 2)
    assert_true Complex(Rational(4, 2), 0) == 2
    assert_true Complex(Rational(1, 2), 0) == 0.5
    assert_false Complex(Rational(1, 2), 1) == Rational(1, 2)
  end
end
