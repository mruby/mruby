def assert_complex(real, exp)
  assert "assert_complex" do
    assert_float real.real,      exp.real
    assert_float real.imaginary, exp.imaginary
  end
end

assert 'Complex' do
  c = 123i
  assert_equal Complex, c.class
  assert_equal [c.real, c.imaginary], [0, 123]
  c = 123 + -1.23i
  assert_equal Complex, c.class
  assert_equal [c.real, c.imaginary], [123, -1.23]
end

assert 'Complex::polar' do
  assert_complex Complex.polar(3, 0),           (3  +  0i)
  assert_complex Complex.polar(3, Math::PI/2),  (0  +  3i)
  assert_complex Complex.polar(3, Math::PI),    (-3 +  0i)
  assert_complex Complex.polar(3, -Math::PI/2), (0  + -3i)
end

assert 'Complex::rectangular' do
  assert_complex Complex.rectangular(1, 2), (1 + 2i)
end

assert 'Complex#*' do
  assert_complex Complex(2, 3)  * Complex(2, 3),  (-5    + 12i)
  assert_complex Complex(900)   * Complex(1),     (900   + 0i)
  assert_complex Complex(-2, 9) * Complex(-9, 2), (0     - 85i)
  assert_complex Complex(9, 8)  * 4,              (36    + 32i)
  assert_complex Complex(20, 9) * 9.8,            (196.0 + 88.2i)
  assert_complex 4 * Complex(9, 8),               (36    + 32i)
  assert_complex 9.8 * Complex(20, 9),            (196.0 + 88.2i)
end

assert 'Complex#+' do
  assert_complex Complex(2, 3)  + Complex(2, 3) , (4    + 6i)
  assert_complex Complex(900)   + Complex(1)    , (901  + 0i)
  assert_complex Complex(-2, 9) + Complex(-9, 2), (-11  + 11i)
  assert_complex Complex(9, 8)  + 4             , (13   + 8i)
  assert_complex Complex(20, 9) + 9.8           , (29.8 + 9i)
  assert_complex 4 + Complex(9, 8)              , (13   + 8i)
  assert_complex 9.8 + Complex(20, 9)           , (29.8 + 9i)
end

assert 'Complex#-' do
  assert_complex Complex(2, 3)  - Complex(2, 3) , (0    + 0i)
  assert_complex Complex(900)   - Complex(1)    , (899  + 0i)
  assert_complex Complex(-2, 9) - Complex(-9, 2), (7    + 7i)
  assert_complex Complex(9, 8)  - 4             , (5    + 8i)
  assert_complex Complex(20, 9) - 9.8           , (10.2 + 9i)
  assert_complex 4 - Complex(9, 8)              , (-5   - 8i)
  assert_complex 10.5 - Complex(20, 9)          , (-9.5 - 9i)
end

assert 'Complex#-@' do
  assert_complex((-1 - 2i), -Complex(1, 2))
end

assert 'Complex#/' do
  assert_complex Complex(2, 3)  / Complex(2, 3) , (1                  + 0i)
  assert_complex Complex(900)   / Complex(1)    , (900                + 0i)
  assert_complex Complex(-2, 9) / Complex(-9, 2), ((36.0 / 85)        - (77i / 85))
  assert_complex Complex(9, 8)  / 4             , ((9.0 / 4)          + 2i)
  assert_complex Complex(20, 9) / 9.8           , (2.0408163265306123 + 0.9183673469387754i)
  assert_complex 4 / Complex(9, 8)              , (0.2482758620689655 - 0.2206896551724138i)
  assert_complex 9.8 / Complex(20, 9)           , (0.4074844074844075 - 0.1833679833679834i)
  if 1e39.infinite? then
    # MRB_USE_FLOAT32 in effect
    ten = 1e21
    one = 1e20
  else
    ten = 1e201
    one = 1e200
  end
  assert_complex Complex(ten, ten) / Complex(one, one), Complex(10.0, 0.0)
  assert_raise(ZeroDivisionError) { Complex(1,1) / 0 }
  assert_raise(ZeroDivisionError) { Complex(1,1) / Complex(0,0) }
end

assert 'Complex#==' do
  assert_true  Complex(2, 3)  == Complex(2, 3)
  assert_true  Complex(5)     == 5
  assert_true  Complex(0)     == 0.0
  assert_true  5 == Complex(5)
  assert_true  0.0 == Complex(0)
end

assert 'Complex#eql?' do
  assert_true  Complex(1, 2).eql?(Complex(1, 2))
  assert_false Complex(1, 0).eql?(1)
  assert_false Complex(0, 0).eql?(0.0)
  assert_false 1.eql?(Complex(1, 0))
  assert_false Complex(1, 0).eql?(nil)
end

assert 'Complex#abs' do
  assert_float Complex(-1).abs,        1
  assert_float Complex(3.0, -4.0).abs, 5.0
  if 1e39.infinite? then
    # MRB_USE_FLOAT32 in effect
    exp = 125
  else
    exp = 1021
  end
  assert_true Complex(3.0*2.0**exp, 4.0*2.0**exp).abs.finite?
  assert_float Complex(3.0*2.0**exp, 4.0*2.0**exp).abs, 5.0*2.0**exp
end

assert 'Complex#abs2' do
  assert_float Complex(-1).abs2,        1
  assert_float Complex(3.0, -4.0).abs2, 25.0
end

assert 'Complex#arg' do
  assert_float Complex.polar(3, Math::PI/2).arg, 1.5707963267948966
end

assert 'Complex#conjugate' do
  assert_complex Complex(1, 2).conjugate, (1 - 2i)
end

assert 'Complex#fdiv' do
  assert_complex Complex(11, 22).fdiv(3), (3.6666666666666665 + 7.333333333333333i)
end

assert 'Complex#imaginary' do
  assert_float Complex(7).imaginary    , 0
  assert_float Complex(9, -4).imaginary, -4
end

assert 'Complex#polar' do
  assert_equal Complex(1, 2).polar, [2.23606797749979, 1.1071487177940904]
end

assert 'Complex#real' do
  assert_float Complex(7).real,     7
  assert_float Complex(9, -4).real, 9
end

assert 'Complex#real?' do
  assert_false Complex(1).real?
end

assert 'Complex::rectangular' do
  assert_equal Complex(1, 2).rectangular, [1, 2]
end

assert 'Complex::to_c' do
  assert_equal Complex(1, 2).to_c, Complex(1, 2)
end

assert 'Complex::to_f' do
  assert_float Complex(1, 0).to_f, 1.0
  assert_raise(RangeError) do
    Complex(1, 2).to_f
  end
end

assert 'Complex::to_i' do
  assert_equal Complex(1, 0).to_i, 1
  assert_raise(RangeError) do
    Complex(1, 2).to_i
  end
end

assert 'Complex#to_s' do
  assert_equal "1.0+2.0i", Complex(1.0, 2.0).to_s
  assert_equal "1.0-2.0i", Complex(1.0, -2.0).to_s
  assert_equal "1.0+0.0i", Complex(1.0, 0.0).to_s
  assert_equal "1.0-0.0i", Complex(1.0, -0.0).to_s
  assert_equal "-0.0-0.0i", Complex(-0.0, -0.0).to_s
  assert_equal "(1.0-0.0i)", Complex(1.0, -0.0).inspect
  assert_equal "1.0+Infinity*i", Complex(1.0, 1.0 / 0).to_s
  assert_equal "1.0-Infinity*i", Complex(1.0, -1.0 / 0).to_s
  assert_equal "0.0+NaN*i", Complex(0.0, 0.0 / 0).to_s
end

assert 'Complex#frozen?' do
  assert_predicate(1i, :frozen?)
  assert_predicate(Complex(2,3), :frozen?)
  assert_predicate(4+5i, :frozen?)
end

assert 'Complex#**' do
  assert_complex Complex(2, 3) ** 2, Complex(-5, 12)
  assert_complex Complex(2, 3) ** 0, Complex(1, 0)
  assert_complex Complex(2, 3) ** 1, Complex(2, 3)
  assert_complex Complex(2, 3) ** Complex(1, 0), Complex(2, 3)
  assert_complex Complex(0, 1) ** 2, Complex(-1, 0)
  assert_complex Complex(0, 1) ** Complex(0, 1), Complex(Math::E ** (-Math::PI / 2), 0)
end

# An MRB_COMPLEX_FLOAT_ONLY build coerces every part through Float; the
# tests below ask about exact parts, so each skips itself there. The probe
# sits outside the assert helpers, as does the bigint one further down, so
# no rescue inside a helper can swallow what it asks.
complex_exact = Complex(1, 0).real.class == Integer

assert 'Complex parts keep their class' do
  skip "float-only build" unless complex_exact
  assert_equal Integer, Complex(1, 2).real.class
  assert_equal Integer, Complex(1, 2).imaginary.class
  assert_equal [1, 2], Complex(1, 2).rectangular
  assert_equal Float, Complex(1.5, 2).real.class
  assert_equal Integer, Complex(1.5, 2).imaginary.class
  assert_equal [1.5, 2.5], Complex(1.5, 2.5).rectangular
  assert_equal [0, 0], nil.to_c.rectangular
  assert_equal Integer, nil.to_c.real.class
  assert_equal Integer, 1.to_c.real.class
  assert_equal Integer, (-42.i).imaginary.class
end

assert 'Complex#to_s with exact parts' do
  skip "float-only build" unless complex_exact
  assert_equal "2+0i", Complex(2).to_s
  assert_equal "-8+6i", Complex(-8, 6).to_s
  assert_equal "1-2i", Complex(1, -2).to_s
  assert_equal "0-42i", (-42.i).to_s
  assert_equal "(2+0i)", Complex(2).inspect
  assert_equal "(1-2i)", Complex(1, -2).inspect
  assert_equal "2.0+2i", (Complex(1, 2) + 1.0).to_s
end

assert 'exact arithmetic keeps exact parts' do
  skip "float-only build" unless complex_exact
  c = Complex(1, 2) * Complex(3, 4)
  assert_equal [-5, 10], c.rectangular
  assert_equal Integer, c.real.class
  c = Complex(1, 2) + Complex(3, 4)
  assert_equal [4, 6], c.rectangular
  assert_equal Integer, c.real.class
  c = 2 - Complex(1, 2)
  assert_equal [1, -2], c.rectangular
  assert_equal Integer, c.real.class
  c = 3 * Complex(1, 2)
  assert_equal [3, 6], c.rectangular
  assert_equal Integer, c.real.class
end

assert 'a real scalar touches only the part it lands on' do
  skip "float-only build" unless complex_exact
  c = Complex(1, 2) + 1.0
  assert_equal Float, c.real.class
  assert_equal Integer, c.imaginary.class
  assert_equal [2.0, 2], c.rectangular
  c = Complex(1, 2) - 1.0
  assert_equal Integer, c.imaginary.class
  c = Complex(1, 2) * 2.0
  assert_equal Float, c.imaginary.class
end

assert 'Complex#** with an integer exponent is exact' do
  skip "float-only build" unless complex_exact
  c = Complex(1, 2) ** 2
  assert_equal [-3, 4], c.rectangular
  assert_equal Integer, c.real.class
  c = Complex(1, 2) ** 0
  assert_equal [1, 0], c.rectangular
  assert_equal Integer, c.real.class
end

assert 'Complex#** with an integer exponent multiplies float parts exactly' do
  c = Complex(1.0, 2.0) ** 2
  assert_equal [-3.0, 4.0], c.rectangular
  assert_equal Float, c.real.class
end

assert 'Complex#== converts across part classes' do
  assert_true Complex(1, 2) == Complex(1.0, 2.0)
  assert_true Complex(2, 0) == 2
  assert_true Complex(2, 0) == 2.0
  assert_false Complex(2, 1) == 2
  assert_false Complex(1, 2) == Complex(2, 1)
end

assert 'Complex#eql? and #hash key by part class' do
  skip "float-only build" unless complex_exact
  assert_true Complex(1, 2).eql?(Complex(1, 2))
  assert_false Complex(1, 2).eql?(Complex(1.0, 2.0))
  assert_false Complex(1, 2).eql?(1)
  assert_equal Complex(1, 2).hash, Complex(1, 2).hash
  assert_equal Complex(1.0, 2.0).hash, Complex(1.0, 2.0).hash
  assert_not_equal Complex(1, 2).hash, Complex(1.0, 2.0).hash
end

assert 'the two float zeros are one hash key' do
  assert_equal Complex(1.0, 0.0).hash, Complex(1.0, -0.0).hash
  assert_true Complex(1.0, 0.0).eql?(Complex(1.0, -0.0))
end

assert 'Complex#to_i and #to_f with exact parts' do
  assert_equal 3, Complex(3, 0).to_i
  assert_equal Integer, Complex(3, 0).to_i.class
  assert_float 3.0, Complex(3, 0).to_f
  assert_raise(RangeError) { Complex(3, 4).to_i }
  assert_raise(RangeError) { Complex(3, 4).to_f }
end

assert 'Complex#abs2 with exact parts' do
  skip "float-only build" unless complex_exact
  assert_equal 1, Complex(-1).abs2
  assert_equal Integer, Complex(-1).abs2.class
  assert_float 25.0, Complex(3.0, -4.0).abs2
end

complex_test_bigint = begin
  10 ** 20
rescue RangeError
  nil
end

assert 'Complex holds a Bigint part' do
  skip "float-only build" unless complex_exact
  skip "no bigint in this build" unless complex_test_bigint
  big = complex_test_bigint
  c = Complex(big, 0)
  assert_equal big, c.real
  assert_equal big * 2, (c * 2).real
  assert_equal big + 1, (c + 1).real
  assert_true c == big
  assert_equal big, c.to_i
end
