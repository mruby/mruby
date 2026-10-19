# \R matches any linebreak, as CRuby's (?>\r\n|[\n\v\f\r\u0085\u2028\u2029])
# does: a CR LF pair as one, which a later failure does not split. It raised
# RegexpError. Inside a character class it is the letter R, and in a
# look-behind it is refused as CRuby refuses it.

assert('Regexp \R matches a linebreak') do
  assert_equal ["\r\n"], "a\r\nb".scan(/\R/)
  assert_equal ["a", "", "b"], "a\r\n\nb".split(/\R/)
  assert_equal 1, "x\r\n" =~ /\R\z/
  assert_equal 0, "\r\n" =~ /\A\R\z/
  assert_nil "\r\n".match(/\R{2}/)
  assert_equal ["\r\r\n"], "\r\r\n".match(/\R{2}/).to_a
  assert_nil "\r\n" =~ /\A\R\n\z/
  assert_true "ab\v".match?(/\R/)
  assert_false "abc".match?(/\R/)
  assert_equal "a|b", "a\nb".gsub(/\R/, "|")
  assert_equal ["x", "y"], "x\r\n\r\ny".split(/\R+/)
  assert_equal ["\r\n", "\n"], "\r\n\n".scan(/\R/)
  assert_true "{ |x|\n  x }".match?(/\R/)
  assert_false "{ |x| x }".match?(/\R/)
end

assert('Regexp \R in a built pattern, a group and an alternation') do
  re = Regexp.new("a\\Rb")
  assert_true re.match?("a\nb")
  assert_true re.match?("a\r\nb")
  assert_false re.match?("a\n\nb")
  assert_equal 0, "x\r\ny" =~ /x(?:\R)y/
  assert_equal "\n", "x\ny"[/\R/]
  assert_equal 0, "xa" =~ /x(?:a|\R|b)/
  assert_equal 0, "x\n" =~ /x(?:a|\R|b)/
  assert_equal 0, "\n" =~ /a|\R|b/
  assert_equal 0, "A\r\n" =~ /a\R/i
  assert_equal 0, "a\n" =~ /a \R/x
end

assert('Regexp \R is the letter in a class and refused in a look-behind') do
  assert_true "R".match?(/[\R]/)
  assert_raise_with_message(RegexpError, "invalid pattern in look-behind: /(?<=\\R)x/") do
    Regexp.new("(?<=\\R)x")
  end
end

assert('Regexp \R matches the Unicode linebreaks') do
  skip unless __ENCODING__ == "UTF-8"
  assert_true "\u2028x".match?(/\R/)
  assert_true "\u0085".match?(/\R/)
end
