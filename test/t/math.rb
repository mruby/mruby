##
# Math Test

assert('Math.erf 0') do
  Math.erf(0) == 0
end

assert('Math.exp 0') do
  Math.exp(0) == 1.0
end

assert('Math.exp 1') do
  Math.exp(1) == 2.718281828459045
end

assert('Math.exp 1.5') do
  Math.exp(1.5) == 4.4816890703380645
end

assert('Math.log 1') do
  Math.log(1) == 0
end

assert('Math.log E') do
  Math.log(Math::E) == 1.0
end

assert('Math.log E**3') do
  Math.log(Math::E**3) == 3.0
end

assert('Math.log2 1') do
  Math.log2(1) == 0.0
end

assert('Math.log2 2') do
  Math.log2(2) == 1.0
end

assert('Math.log10 1') do
  Math.log10(1) == 0.0
end

assert('Math.log10 10') do
  Math.log10(10) == 1.0
end

assert('Math.log10 10**100') do
  Math.log10(10**100) == 100.0
end

assert('Math.cbrt') do
  a = [-8, -1, 0, 1, 8].map do |i|
    Math.cbrt(i)
  end

  a == [-2.0, -1.0, 0.0, 1.0, 2.0]
end

assert('Math.hypot') do
  Math.hypot(3, 4) == 5.0
end

