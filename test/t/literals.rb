##
# Literals ISO Test

assert('Literals Numerical', '8.7.6.2') do
  # signed and unsigned integer
  assert_equal 1, 1
  assert_equal(-1, -1)
  assert_equal(+1, +1)
  # signed and unsigned float
  assert_equal 1.0, 1.0
  assert_equal(-1.0, -1.0)
  # binary
  assert_equal 0b10000000, 128
  assert_equal 0B10000000, 128
  # octal
  assert_equal 0o10, 8
  assert_equal 0O10, 8
  assert_equal 0_10, 8
  # hex
  assert_equal 0xff, 255
  assert_equal 0Xff, 255
  # decimal
  assert_equal 0d999, 999
  assert_equal 0D999, 999
  # decimal seperator
  assert_equal 10_000_000, 10000000
  assert_equal 1_0, 10
  # integer with exponent
  assert_equal 1e1, 10.0
  assert_equal 1e-1, 0.1
  assert_equal 1e+1, 10.0
  # float with exponent
  assert_equal 1.0e1, 10.0
  assert_equal 1.0e-1, 0.1
  assert_equal 1.0e+1, 10.0
end

assert('Literals Strings Single Quoted', '8.7.6.3.2') do
  assert_equal 'abc', 'abc'
  assert_equal '\'', '\''
  assert_equal '\\', '\\'
end

assert('Literals Strings Double Quoted', '8.7.6.3.3') do
  a = "abc"

  assert_equal "abc", "abc"
  assert_equal "\"", "\""
  assert_equal "\\", "\\"
  assert_equal "#{a}", "abc"
end

assert('Literals Strings Quoted Non-Expanded', '8.7.6.3.4') do
  a = %q{abc}
  b = %q(abc)
  c = %q[abc]
  d = %q<abc>
  e = %q/abc/
  f = %q/ab\/c/
  g = %q{#{a}}

  assert_equal a, 'abc'
  assert_equal b, 'abc'
  assert_equal c, 'abc'
  assert_equal d, 'abc'
  assert_equal e, 'abc'
  assert_equal f, 'ab/c'
  assert_equal g, '#{a}'
end

assert('Literals Strings Quoted Expanded', '8.7.6.3.5') do
  a = %Q{abc}
  b = %Q(abc)
  c = %Q[abc]
  d = %Q<abc>
  e = %Q/abc/
  f = %Q/ab\/c/
  g = %Q{#{a}}

  assert_equal a, 'abc'
  assert_equal b, 'abc'
  assert_equal c, 'abc'
  assert_equal d, 'abc'
  assert_equal e, 'abc'
  assert_equal f, 'ab/c'
  assert_equal g, 'abc'
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

  assert_equal a, "aaa\n"
  assert_equal b, "bbb\n"
  assert_equal c, ["c1\n", "c 2\n", "c  3\n"]
  assert_equal d, "d3DDD\nd\t\nDDD\n\n"
  assert_equal e, "e\#{1+2}EEE\ne\\t\nEEE\\n\n"
  assert_equal f, "F\nFFfFFF\nF\n"
  assert_equal g, "  ggg\n"
  assert_equal h, "  hhh\n"
  assert_equal i, "  iii\n"
  assert_equal j, ["  j1j\n", "  j2j\n", "  j\#{3}j\n"]
  assert_equal k, 123
  assert_equal z, ""
end

assert('Literals Array', '8.7.6.4') do
  a = %W{abc#{1+2}def \}g}
  b = %W(abc #{2+3} def \(g)
  c = %W[#{3+4}]
  d = %W< #{4+5} >
  e = %W//
  f = %W[[ab cd][ef]]
  g = %W{
    ab
    #{-1}1
    2#{2}
  }
  h = %W(a\nb
         test\ abc
         c\
d
         x\y x\\y x\\\y)

  assert_equal a, ['abc3def', '}g']
  assert_equal b, ['abc', '5', 'def', '(g']
  assert_equal c, ['7']
  assert_equal d, ['9']
  assert_equal e, []
  assert_equal f, ['[ab', 'cd][ef]']
  assert_equal g, ['ab', '-11', '22']
  assert_equal h, ["a\nb", 'test abc', "c\nd", "xy", "x\\y", "x\\y"]

  a = %w{abc#{1+2}def \}g}
  b = %w(abc #{2+3} def \(g)
  c = %w[#{3+4}]
  d = %w< #{4+5} >
  e = %w//
  f = %w[[ab cd][ef]]
  g = %w{
    ab
    #{-1}1
    2#{2}
  }
  h = %w(a\nb
         test\ abc
         c\
d
         x\y x\\y x\\\y)

  assert_equal a, ['abc#{1+2}def', '}g']
  assert_equal b, ['abc', '#{2+3}', 'def', '(g']
  assert_equal c, ['#{3+4}']
  assert_equal d, ['#{4+5}']
  assert_equal e, []
  assert_equal f, ['[ab', 'cd][ef]']
  assert_equal g, ['ab', '#{-1}1', '2#{2}']
  assert_equal h, ["a\\nb", "test abc", "c\nd", "x\\y", "x\\y", "x\\\\y"]
end

assert('Literals Array of symbols') do
  a = %I{abc#{1+2}def \}g}
  b = %I(abc #{2+3} def \(g)
  c = %I[#{3+4}]
  d = %I< #{4+5} >
  e = %I//
  f = %I[[ab cd][ef]]
  g = %I{
    ab
    #{-1}1
    2#{2}
  }

  assert_equal a, [:'abc3def', :'}g']
  assert_equal b, [:'abc', :'5', :'def', :'(g']
  assert_equal c, [:'7']
  assert_equal d, [:'9']
  assert_equal e, []
  assert_equal f, [:'[ab', :'cd][ef]']
  assert_equal g, [:'ab', :'-11', :'22']

  a = %i{abc#{1+2}def \}g}
  b = %i(abc #{2+3} def \(g)
  c = %i[#{3+4}]
  d = %i< #{4+5} >
  e = %i//
  f = %i[[ab cd][ef]]
  g = %i{
    ab
    #{-1}1
    2#{2}
  }

  assert_equal a, [:'abc#{1+2}def', :'}g']
  assert_equal b, [:'abc', :'#{2+3}', :'def', :'(g']
  assert_equal c, [:'#{3+4}']
  assert_equal d, [:'#{4+5}']
  assert_equal e, []
  assert_equal f, [:'[ab', :'cd][ef]']
  assert_equal g, [:'ab', :'#{-1}1', :'2#{2}']
end

assert('Literals Symbol', '8.7.6.6') do
  # do not compile error
  :$asd
  :@asd
  :@@asd
  :asd=
  :asd!
  :asd?
  :+
  :+@
  :if
  :BEGIN

  a = :"asd qwe"
  b = :'foo bar'
  c = :"a#{1+2}b"
  d = %s(asd)
  e = %s( foo \))
  f = %s[asd \[
qwe]
  g = %s/foo#{1+2}bar/
  h = %s{{foo bar}}

  assert_equal a, :'asd qwe'
  assert_equal b, :"foo bar"
  assert_equal c, :a3b
  assert_equal d, :asd
  assert_equal e, :' foo )'
  assert_equal f, :"asd [\nqwe"
  assert_equal g, :'foo#{1+2}bar'
  assert_equal h, :'{foo bar}'
end

# Not Implemented ATM assert('Literals Regular expression', '8.7.6.5') do
