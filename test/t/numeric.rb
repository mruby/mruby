##
# Numeric ISO Test

assert('Numeric', '15.2.7') do
  Numeric.class == Class
end

assert('Numeric superclass', '15.2.7.2') do
  Numeric.superclass == Object
end

assert('Numeric#+@', '15.2.7.4.1') do
  +1 == +1
end

assert('Numeric#-@', '15.2.7.4.2') do
  -1 == -1
end

assert('Numeric#abs', '15.2.7.4.3') do
  1.abs == 1 and -1.abs == 1.0
end

# Not ISO specified

assert('Numeric#**') do
  2.0**3 == 8.0
end

assert('Numeric#chr') do
  e = nil
  begin
    256.chr
  rescue => e
  end

  0.chr == "\x00" and
  65.chr == 'A' and
  e.class == RangeError
end
