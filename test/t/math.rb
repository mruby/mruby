##
# Math Test

##
# Performs fuzzy check for equality on methods returning floats
# on the basis of the Math::TOLERANCE constant.
def check(a,b)
  a = a.to_f
  b = b.to_f
  if a.finite? and b.finite?
    (a-b).abs < Math::TOLERANCE
  else
    true
  end
end

assert('Math.sin 0') do
  check(Math.sin(0), 0)
end

assert('Math.sin PI/2') do
  check(Math.sin(Math::PI / 2), 1)
end


assert('Fundamental trig identities') do  
  result = true
  N = 15
  N.times do |i|
    a  = Math::PI / N * i
    ca = Math::PI / 2 - a
    s  = Math.sin(a)
    c  = Math.cos(a)
    t  = Math.tan(a)
    result &= check(s, Math.cos(ca))
    result &= check(t, 1 / Math.tan(ca))
    result &= check(s ** 2 + c ** 2, 1)
    result &= check(t ** 2 + 1, (1/c) ** 2)
    result &= check((1/t) ** 2 + 1, (1/s) ** 2)
  end
  result
end  

assert('Math.erf 0') do
  check(Math.erf(0), 0)
end

assert('Math.exp 0') do
  check(Math.exp(0), 1.0)
end

assert('Math.exp 1') do
  check(Math.exp(1), 2.718281828459045)
end

assert('Math.exp 1.5') do
  check(Math.exp(1.5), 4.4816890703380645)
end

assert('Math.log 1') do
  check(Math.log(1), 0)
end

assert('Math.log E') do
  check(Math.log(Math::E), 1.0)
end

assert('Math.log E**3') do
  check(Math.log(Math::E**3), 3.0)
end

assert('Math.log2 1') do
  check(Math.log2(1), 0.0)
end

assert('Math.log2 2') do
  check(Math.log2(2), 1.0)
end

assert('Math.log10 1') do
  check(Math.log10(1), 0.0)
end

assert('Math.log10 10') do
  check(Math.log10(10), 1.0)
end

assert('Math.log10 10**100') do
  check(Math.log10(10**100), 100.0)
end

assert('Math.cbrt') do
  num = [-2.0, -1.0, 0.0, 1.0, 2.0]
  cub = [-8, -1, 0, 1, 8]
  result = true
  cub.each_with_index do |v,i|
    result &= check(Math.cbrt(v), num[i])
  end
  result
end

assert('Math.hypot') do
  check(Math.hypot(3, 4), 5.0)
end

assert('Math.frexp 1234') do
  n = 1234
  fraction, exponent = Math.frexp(n)
  check(Math.ldexp(fraction, exponent), n)
end

assert('Math.erf 1') do
  check(Math.erf(1), 0.842700792949715)
end

assert('Math.erfc 1') do
  check(Math.erfc(1), 0.157299207050285)
end
