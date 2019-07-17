##
# Math Test

assert('Math.sin 0') do
  assert_float(0, Math.sin(0))
end

assert('Math.sin PI/2') do
  assert_float(1, Math.sin(Math::PI / 2))
end

assert('Math.cos 0') do
  assert_float(1, Math.cos(0))
end

assert('Math.cos PI/2') do
  assert_float(0, Math.cos(Math::PI / 2))
end

assert('Math.tan 0') do
  assert_float(0, Math.tan(0))
end

assert('Math.tan PI/4') do
  assert_float(1, Math.tan(Math::PI / 4))
end

assert('Fundamental trig identities') do
  N = 13
  N.times do |i|
    a  = Math::PI / N * i
    ca = Math::PI / 2 - a
    s  = Math.sin(a)
    c  = Math.cos(a)
    t  = Math.tan(a)
    assert_float(Math.cos(ca), s)
    assert_float(1 / Math.tan(ca), t)
    assert_float(1, s ** 2 + c ** 2)
    assert_float((1/c) ** 2, t ** 2 + 1)
    assert_float((1/s) ** 2, (1/t) ** 2 + 1)
  end
end

assert('Math.erf 0') do
  assert_float(0, Math.erf(0))
end

assert('Math.exp 0') do
  assert_float(1.0, Math.exp(0))
end

assert('Math.exp 1') do
  assert_float(2.718281828459045, Math.exp(1))
end

assert('Math.exp 1.5') do
  assert_float(4.4816890703380645, Math.exp(1.5))
end

assert('Math.log 1') do
  assert_float(0, Math.log(1))
end

assert('Math.log E') do
  assert_float(1.0, Math.log(Math::E))
end

assert('Math.log E**3') do
  assert_float(3.0, Math.log(Math::E**3))
end

assert('Math.log2 1') do
  assert_float(0.0, Math.log2(1))
end

assert('Math.log2 2') do
  assert_float(1.0, Math.log2(2))
end

assert('Math.log10 1') do
  assert_float(0.0, Math.log10(1))
end

assert('Math.log10 10') do
  assert_float(1.0, Math.log10(10))
end

assert('Math.log10 10**100') do
  assert_float(100.0, Math.log10(10**100))
end

assert('Math.sqrt') do
  num = [0.0, 1.0, 2.0, 3.0, 4.0]
  sqr = [0, 1, 4, 9, 16]
  sqr.each_with_index do |v,i|
    assert_float(num[i], Math.sqrt(v))
  end
end

assert('Math.cbrt') do
  num = [-2.0, -1.0, 0.0, 1.0, 2.0]
  cub = [-8, -1, 0, 1, 8]
  cub.each_with_index do |v,i|
    assert_float(num[i], Math.cbrt(v))
  end
end

assert('Math.hypot') do
  assert_float(5.0, Math.hypot(3, 4))
end

assert('Math.frexp 1234') do
  n = 1234
  fraction, exponent = Math.frexp(n)
  assert_float(n, Math.ldexp(fraction, exponent))
end

assert('Math.erf 1') do
  assert_float(0.842700792949715, Math.erf(1))
end

assert('Math.erfc 1') do
  assert_float(0.157299207050285, Math.erfc(1))
end

assert('Math.erf -1') do
  assert_float(-0.8427007929497148, Math.erf(-1))
end

assert('Math.erfc -1') do
  assert_float(1.8427007929497148, Math.erfc(-1))
end
