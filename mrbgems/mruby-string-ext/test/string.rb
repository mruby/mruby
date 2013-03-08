##
# String(Ext) Test

assert('String#getbyte') do
  str1 = "hello"
  bytes1 = [104, 101, 108, 108, 111]
  assert_equal bytes1[0], str1.getbyte(0)
  assert_equal bytes1[-1], str1.getbyte(-1)
  assert_equal bytes1[6], str1.getbyte(6)

  str2 = "\xFF"
  bytes2 = [0xFF]
  assert_equal bytes2[0], str2.getbyte(0)
end

assert('String#dump') do
  "foo".dump == "\"foo\""
end

assert('String#strip') do
  s = "  abc  " 
  s.strip
  "".strip == "" and " \t\r\n\f\v".strip == "" and
  "\0a\0".strip == "\0a" and
  "abc".strip     == "abc" and
  "  abc".strip   == "abc" and
  "abc  ".strip   == "abc" and
  "  abc  ".strip == "abc" and
  s == "  abc  "
end

assert('String#lstrip') do
  s = "  abc  " 
  s.lstrip
  "".lstrip == "" and " \t\r\n\f\v".lstrip == "" and
  "\0a\0".lstrip == "\0a\0" and
  "abc".lstrip     == "abc"   and
  "  abc".lstrip   == "abc"   and
  "abc  ".lstrip   == "abc  " and
  "  abc  ".lstrip == "abc  " and
  s == "  abc  "
end

assert('String#rstrip') do
  s = "  abc  " 
  s.rstrip
  "".rstrip == "" and " \t\r\n\f\v".rstrip == "" and
  "\0a\0".rstrip == "\0a" and
  "abc".rstrip     == "abc"   and
  "  abc".rstrip   == "  abc" and
  "abc  ".rstrip   == "abc"   and
  "  abc  ".rstrip == "  abc" and
  s == "  abc  "
end

assert('String#strip!') do
  s = "  abc  "
  t = "abc"
  s.strip! == "abc" and s == "abc" and t.strip! == nil
end

assert('String#lstrip!') do
  s = "  abc  "
  t = "abc  "
  s.lstrip! == "abc  " and s == "abc  " and t.lstrip! == nil
end

assert('String#rstrip!') do
  s = "  abc  "
  t = "  abc"
  s.rstrip! == "  abc" and s == "  abc" and t.rstrip! == nil
end
