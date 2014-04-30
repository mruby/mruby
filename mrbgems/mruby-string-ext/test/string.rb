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
  ("\1" * 100).dump     # should not raise an exception - regress #1210
  "\0".inspect == "\"\\000\"" and
  "foo".dump == "\"foo\""
end

assert('String#strip') do
  s = "  abc  "
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

assert('String#swapcase') do
  assert_equal "hELLO", "Hello".swapcase
  assert_equal "CyBeR_pUnK11", "cYbEr_PuNk11".swapcase
end

assert('String#swapcase!') do
  s = "Hello"
  t = s.clone
  t.swapcase!
  assert_equal s.swapcase, t
end

assert('String#concat') do
  s = "Hello "
  s.concat "World!"
  t = "Hello "
  t << "World!"
  assert_equal "Hello World!", t
  assert_equal "Hello World!", s
end

assert('String#casecmp') do
  assert_equal 1, "abcdef".casecmp("abcde")
  assert_equal 0, "aBcDeF".casecmp("abcdef")
  assert_equal(-1, "abcdef".casecmp("abcdefg"))
  assert_equal 0, "abcdef".casecmp("ABCDEF")
  o = Object.new
  def o.to_str
    "ABCDEF"
  end
  assert_equal 0, "abcdef".casecmp(o)
end

assert('String#start_with?') do
  assert_true "hello".start_with?("heaven", "hell")
  assert_true !"hello".start_with?("heaven", "paradise")
  assert_true !"h".start_with?("heaven", "hell")
  assert_raise TypeError do "hello".start_with?(true) end
end

assert('String#end_with?') do
  assert_true "string".end_with?("ing", "mng")
  assert_true !"string".end_with?("str", "tri")
  assert_true !"ng".end_with?("ing", "mng")
  assert_raise TypeError do "hello".end_with?(true) end
end

assert('String#partition') do
  assert_equal ["a", "x", "axa"], "axaxa".partition("x")
  assert_equal ["aaaaa", "", ""], "aaaaa".partition("x")
  assert_equal ["", "", "aaaaa"], "aaaaa".partition("")
  assert_equal ["", "a", "aaaa"], "aaaaa".partition("a")
  assert_equal ["aaaa", "b", ""], "aaaab".partition("b")
  assert_equal ["", "b", "aaaa"], "baaaa".partition("b")
  assert_equal ["", "", ""],      "".partition("a")
end

assert('String#rpartition') do
  assert_equal ["axa", "x", "a"], "axaxa".rpartition("x")
  assert_equal ["", "", "aaaaa"], "aaaaa".rpartition("x")
  assert_equal ["aaaaa", "", ""], "aaaaa".rpartition("")
  assert_equal ["aaaa", "a", ""], "aaaaa".rpartition("a")
  assert_equal ["aaaa", "b", ""], "aaaab".rpartition("b")
  assert_equal ["", "b", "aaaa"], "baaaa".rpartition("b")
  assert_equal ["", "", ""],      "".rpartition("a")
end

assert('String#hex') do
  assert_equal 16, "10".hex
  assert_equal 255, "ff".hex
  assert_equal 16, "0x10".hex
  assert_equal (-16), "-0x10".hex
  assert_equal 0, "xyz".hex
  assert_equal 16, "10z".hex
  assert_equal 16, "1_0".hex
  assert_equal 0, "".hex
end

assert('String#oct') do
  assert_equal 8, "10".oct
  assert_equal 7, "7".oct
  assert_equal 0, "8".oct
  assert_equal 0, "9".oct
  assert_equal 0, "xyz".oct
  assert_equal 8, "10z".oct
  assert_equal 8, "1_0".oct
  assert_equal 8, "010".oct
  assert_equal (-8), "-10".oct
end
