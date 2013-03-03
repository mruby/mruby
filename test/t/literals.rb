##
# Literals ISO Test

assert('Literals Numerical', '8.7.6.2') do
  # signed and unsigned integer
  1 == 1 and -1 == -1 and +1 == +1 and
    # signed and unsigned float
    1.0 == 1.0 and -1.0 == -1.0 and
    # binary
    0b10000000 == 128 and 0B10000000 == 128
    # octal
    0o10 == 8 and 0O10 == 8 and 0_10 == 8
    # hex
    0xff == 255 and 0Xff == 255 and
    # decimal
    0d999 == 999 and 0D999 == 999 and
    # decimal seperator
    10_000_000 == 10000000 and 1_0 == 10 and
    # integer with exponent
    1e1 == 10.0 and 1e-1 == 0.1 and 1e+1 == 10.0
    # float with exponent
    1.0e1 == 10.0 and 1.0e-1 == 0.1 and 1.0e+1 == 10.0
end

assert('Literals Strings Single Quoted', '8.7.6.3.2') do
  'abc' == 'abc' and '\'' == '\'' and '\\' == '\\'
end

assert('Literals Strings Double Quoted', '8.7.6.3.3') do
  a = "abc"

  "abc" == "abc" and "\"" == "\"" and "\\" == "\\" and
    "#{a}" == "abc"
end

assert('Literals Strings Quoted Non-Expanded', '8.7.6.3.4') do
  a = %q{abc}
  b = %q(abc)
  c = %q[abc]
  d = %q<abc>
  e = %q/abc/
  f = %q/ab\/c/
  g = %q{#{a}}

  a == 'abc' and b == 'abc' and c == 'abc' and d == 'abc' and
    e == 'abc' and f == 'ab/c' and g == '#{a}'
end

assert('Literals Strings Quoted Expanded', '8.7.6.3.5') do
  a = %Q{abc}
  b = %Q(abc)
  c = %Q[abc]
  d = %Q<abc>
  e = %Q/abc/
  f = %Q/ab\/c/
  g = %Q{#{a}}

  a == 'abc' and b == 'abc' and c == 'abc' and d == 'abc' and
    e == 'abc' and f == 'ab/c' and g == 'abc'
end

assert('Literals Strings Here documents', '8.7.6.3.6') do
  a = <<AAA
aaa
AAA
   b = <<b_b
bbb
b_b
    c = [<<CCC1, <<"CCC2", <<'CCC3']
c1
CCC1
c 2
CCC2
c  3
CCC3

      d = <<DDD
d#{1+2}DDD
d\t
DDD\n
DDD
  e = <<'EEE'
e#{1+2}EEE
e\t
EEE\n
EEE
  f = <<"FFF"
F
FF#{"f"}FFF
F
FFF

  g = <<-GGG
  ggg
  GGG
  h = <<-"HHH"
  hhh
  HHH
  i = <<-'III'
  iii
  III
  j = [<<-JJJ1   , <<-"JJJ2"   , <<-'JJJ3' ]
  j#{1}j
  JJJ1
  j#{2}j
  JJJ2
  j#{3}j
  JJJ3

  k = <<'KKK'.to_i
123
KKK

  z = <<'ZZZ'
ZZZ

  a == "aaa\n" and
  b == "bbb\n" and
  c == ["c1\n", "c 2\n", "c  3\n"] and
  d == "d3DDD\nd\t\nDDD\n\n" and
  e == "e\#{1+2}EEE\ne\\t\nEEE\\n\n" and
  f == "F\nFFfFFF\nF\n" and
  g == "  ggg\n" and
  h == "  hhh\n" and
  i == "  iii\n" and
  j == ["  j1j\n", "  j2j\n", "  j\#{3}j\n"] and
  k == 123 and
  z == ""
end

# Not Implemented ATM assert('Literals Array', '8.7.6.4') do

# Not Implemented ATM assert('Literals Regular expression', '8.7.6.5') do

# Not Implemented ATM assert('Literals Symbol', '8.7.6.6') do
