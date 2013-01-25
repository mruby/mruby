##
# Regexp ISO Test

assert('Regexp', '15.2.15.1') do
  Regexp.class == Class and
  Regexp.superclass == Object
end

assert('Regexp.compile', '15.2.15.6.1') do
  re = Regexp.new('abc')
  re.class == Regexp
end

assert('Regexp.escape', '15.2.15.6.2') do
  # simply test
  re1 = Regexp.escape("$bc^")
  # PENDING: \n \t \r \f 0x20
  #re2 = Regexp.escape("0x0a 0x09 0x0d 0x0c 0x20")
  # '#' '$' '(' ')' '*' '+' '-'
  re3 = Regexp.escape("#$()*+-")
  # '.' '?' '[' ']' '^' '{' '|' '}'
  re4 = Regexp.escape(".?[]^{|}")
  # '\'
  re5 = Regexp.escape('\\')

  re1 == '\$bc\^' and
  # re2 == "\n\t\r\f" + "\ 0x20" and
  re3 == '\#\$\(\)\*\+\-' and
  re4 == '\.\?\[\]\^\{\|\}' and
  re5 == '\\\\'
end

assert('Regexp.last_match', '15.2.15.6.3') do
  re = Regexp.compile("(.)(.)")
  re.match("ab")

  Regexp.last_match.class == MatchData and
  Regexp.last_match[0] == "ab" and
  Regexp.last_match[1] == "a" and
  Regexp.last_match[2] == "b" and
  Regexp.last_match[3] == nil
end

assert('Regexp.quote', '15.2.15.6.4') do
  # simply test
  re1 = Regexp.quote("$bc^")
  # PENDING: \n \t \r \f 0x20
  #re2 = Regexp.escape("0x0a 0x09 0x0d 0x0c 0x20")

  # '#' '$' '(' ')' '*' '+' '-'
  re3 = Regexp.quote("#$()*+-")
  # '.' '?' '[' ']' '^' '{' '|' '}'
  re4 = Regexp.quote(".?[]^{|}")
  # '\'
  re5 = Regexp.quote('\\')

  # re2 == "\n\t\r\f" + "\ 0x20" and

  re1 == '\$bc\^' and
  re3 == '\#\$\(\)\*\+\-' and
  re4 == '\.\?\[\]\^\{\|\}' and
  re5 == '\\\\'
end

assert('Regexp#==', '15.2.15.7.1') do
  a = Regexp.compile("abcd")
  b = Regexp.compile("abcd")
  c = Regexp.compile("c")

  (a == b) == true and
  (a == c) == false and
  (b == c) == false
end

assert('Regexp#===', '15.2.15.7.2') do
  a = "HELLO"
  b = "hello"
  re = Regexp.compile("^[A-Z]*$")

  (re === a) == true and
  (re === b) == false
end

assert('Regexp#=~', '15.2.15.7.3') do
  re = Regexp.compile("foo")
  (re =~ "foo") == 0 and
  (re =~ "afoo") == 1 and
  (re =~ "bar") == nil
end

assert('Regexp#casefold?', '15.2.15.7.4') do
  a = Regexp.compile("foobar", Regexp::IGNORECASE)
  b = Regexp.compile("hogehoge")

  a.casefold? == true and
  b.casefold? == false
end

#assert('Regexp#initialize', '15.2.15.7.5') do
#end

#assert('Regexp#initialize_copy', '15.2.15.7.6') do
#end

assert('Regexp#match', '15.2.15.7.7') do
  re = Regexp.compile("(.)(.)")
  m = re.match("afoo")

  m.class == MatchData and
  m.to_s == "af" and
  m[1] == "a" and
  m[2] == "f"
end

#assert('Regexp#source', '15.2.15.7.8') do
#end

assert('Regexp#options (literal)') do
  re1 = /aaa/
  re2 = /aaa/i
  re3 = /aaa/x
  re4 = /aaa/m

  re1.options == 0 and
  re2.options == 1 and
  re3.options == 2 and
  re4.options == 4
end

assert('Regexp#options') do
  re1 = Regexp.compile("aaa")
  re2 = Regexp.compile("aaa", Regexp::IGNORECASE)
  re3 = Regexp.compile("aaa", Regexp::EXTENDED)
  re4 = Regexp.compile("aaa", Regexp::MULTILINE)

  re1.options == 0 and
  re2.options == 1 and
  re3.options == 2 and
  re4.options == 4
end


assert('Regexp Literal (1)') do
  re1 = /aaa/
  re2 = Regexp.compile("aaa")
  re3 = /aaa/i
  re4 = Regexp.compile("aaa", Regexp::IGNORECASE)

  unless re1 == re2
    p re1, re2
  end
  unless re3 == re4
    p re3, re4
  end

  re1 == re2 and
  re3 == re4
end

assert('Regexp Literal (2)') do
  re1 = /a\nb/
  re2 = /a\\nb/
  re3 = /a\/b/
  re4 = /a"b/

  re1.source == "a\\nb" and
  re2.source == "a\\\\nb" and
  re3.source == "a/b"   and
  re4.source == 'a"b'
end

assert('Regexp Literal (3)') do
  re1 = /a\sb/
  re2 = /a\tb/
  re3 = /a\:b/
  re4 = /a\?b/

  if false
  puts
  puts re1, re1.source, re1.source == "a\\sb"
  puts re2, re2.source, re2.source == "a\\tb"
  puts re3, re3.source, re3.source == "a\\:b"
  puts re4, re4.source, re4.source == "a\\?b"
  end

  re1.source == "a\\sb" and
  re2.source == "a\\tb" and
  re3.source == "a\\:b"   and
  re4.source == 'a\\?b'
end

assert('Regexp Literal (4)') do
  re1 = /\A\w\W\s\S\D\b\B\Z/
  str = "\\A\\w\\W\\s\\S\\D\\b\\B\\Z"
  re2 = Regexp.compile(str)

  re1.source == str and
  re1 == re2
end

# TODO ATM broken assert('String#gsub', '15.2.10.5.18') do
assert('String#gsub', '15.2.10.5.18') do
  re = Regexp.compile('def')
  result1 = 'abcdefg'.gsub(re, '!!')
  re = Regexp.compile('b')
  result2 = 'abcabc'.gsub(re, '<<\&>>')
  re = Regexp.compile('x+(b+)')
  result3 = 'xxbbxbb'.gsub(re, 'X<<\1>>')
  result4 = '2.5'.gsub('.', ',')

  result1 == "abc!!g" and
  result2 == "a<<b>>ca<<b>>c" and
  result3 == "X<<bb>>X<<bb>>" and
  result4 == "2,5"
end

# TODO ATM broken assert('String#gsub!', '15.2.10.5.19') do
assert('String#gsub!', '15.2.10.5.19') do
  result1 = "String-String"
  re = Regexp.compile('in.')
  result1.gsub!(re, "!!")

  result2 = "String-String"
  re = Regexp.compile('in.')
  result2.gsub!(re, '<<\&>>')

  result1 == "Str!!-Str!!" and
  result2 == "Str<<ing>>-Str<<ing>>"
end

# TODO Broken ATM assert('String#scan', '15.2.10.5.32') do
assert('String#scan', '15.2.10.5.32') do
  re = Regexp.compile('..')
  result1 = "foobar".scan(re)
  re = Regexp.compile('ba.')
  result2 = "foobarbazfoobarbazz".scan(re)
  re = Regexp.compile('(.)')
  result3 = "foobar".scan(re)
  re = Regexp.compile('(ba)(.)')
  result4 = "foobarbazfoobarbaz".scan(re)

  result1 == ["fo", "ob", "ar"] and
  result2 == ["bar", "baz", "bar", "baz"] and
  result3 == [["f"], ["o"], ["o"], ["b"], ["a"], ["r"]] and
  result4 == [["ba", "r"], ["ba", "z"], ["ba", "r"], ["ba", "z"]]
end
