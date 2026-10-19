# A pattern whose every branch ends at \z, \Z or $ within a bounded number of
# bytes is tried only that far before where the assertion can pass. The
# answers are the ones trying every position gives: where the match starts,
# what the groups hold, $~, and what the String methods built on a search
# answer.

assert('Regexp end window: \z finds the leftmost match') do
  assert_equal true, "foo bar  ".match?(/[[:blank:]]\z/)
  assert_equal false, "foo bar\n".match?(/[[:blank:]]\z/)
  assert_equal 7, "foo bar \t" =~ /[[:blank:]]+\z/
  assert_equal 7, "foo bar \t" =~ /[[:blank:]]{1,2}\z/
  assert_equal 1, "aaaa" =~ /a{2,3}\z/
  assert_equal ["ab", "a", "b"], "xaab".match(/(a|ab)(b?)\z/).to_a
  assert_equal 4, "abcabc".index(/bc\z/)
  assert_equal 4, "abcabc".rindex(/bc\z/)
  assert_equal ["c"], "abcabc".scan(/c\z/)
  assert_equal "abcab!", "abcabc".gsub(/c\z/, "!")
  assert_equal "abcacb", "abcabc".sub(/(b)(c)\z/, '\2\1')
  assert_equal true, "ab".match?(/x?\z/)
  assert_equal 2, "ab" =~ /x?\z/
  assert_equal 0, "" =~ /a?\z/
  assert_equal false, "abc".match?(/\Aab\z/)
  assert_equal true, "ab".match?(/\Aab\z/)
end

assert('Regexp end window: \Z also passes before a final newline') do
  assert_equal 1, "ab\n" =~ /b\Z/
  assert_equal 1, "ab\n" =~ /b\n\Z/
  assert_equal nil, "ab\n\n" =~ /b\Z/
  assert_equal "b", "ab\n".match(/(.)\Z/)[1]
  assert_equal "b\n", "ab\n".match(/.\Z\n/)[0]
end

assert('Regexp end window: $ gives each line its own window') do
  assert_equal 4, "ab\ncd\nef" =~ /d$/
  assert_equal ["b", "d", "f"], "ab\ncd\nef".scan(/.$/)
  assert_equal "aB\ncD\n", "ab\ncd\n".gsub(/.$/) { $&.upcase }
  assert_equal 3, "ab\n\ncd" =~ /^$/
  assert_equal 1, "a\nb\n".index(/$/)
  assert_equal 4, "a\nb\n".rindex(/$/)
  assert_equal nil, "xxxxxxxxxx\nyz" =~ /x?y$/
  assert_equal 2, "x\nyy" =~ /[xy]{2}$/
end

assert('Regexp end window: a branch without an end assertion') do
  assert_equal 1, "abcab" =~ /ab\z|b/
  assert_equal 2, "abcab" =~ /b\z|c/
  assert_equal 0, "a\nb" =~ /a$|b\z/
end

assert('Regexp end window: a word boundary reads the text before it') do
  assert_equal 4, "foo bar" =~ /\bbar\z/
  assert_equal nil, "foobar" =~ /\bbar\z/
  assert_equal 3, "foobar" =~ /\Bbar\z/
end

assert('Regexp end window: a lookbehind reads the text before it') do
  need_backtracking_stack
  assert_equal 3, "xaab" =~ /(?<=a)b\z/
  assert_equal nil, "xyb" =~ /(?<=a)b\z/
  assert_equal nil, "xaab" =~ /(?<!a)b\z/
  assert_equal 3, "ab:cd" =~ /(?<=ab:)cd\z/
  assert_equal 4, "ab:cd" =~ /(?<=b:c)d$/
end

assert('Regexp end window: no match starts inside a character') do
  skip unless __ENCODING__ == "UTF-8"
  assert_equal "é", "aéé".match(/.\z/)[0]
  assert_equal "éé", "aéé".match(/..\z/)[0]
  assert_equal "本語", "日本語".match(/.{2}\z/)[0]
  assert_equal ["語"], "日本語".scan(/.$/)
  assert_equal ["本", "語"], "日本\n語\n".scan(/.$/)
  # U+3000 is blank only where the build classifies above ASCII; either way
  # the window answers what the class says of the whole character.
  assert_equal "　".match?(/[[:blank:]]/), "a　".match?(/[[:blank:]]\z/)
end

assert('Regexp end window: a lookbehind over a wide character') do
  skip unless __ENCODING__ == "UTF-8"
  need_backtracking_stack
  assert_equal "é", "aéé".match(/(?<=é)é\z/)[0]
  assert_equal "aé", "aéb".match(/(?<=é)b\z/).pre_match
end

assert('Regexp end window: $~ after a search') do
  "abc" =~ /a\z/
  assert_equal nil, $~
  "abc" =~ /c\z/
  assert_equal "c", $~[0]
  assert_equal "ab", $~.pre_match
end

assert('Regexp end window: a long subject') do
  line = ("word " * 400) + "end "
  assert_equal true, line.match?(/[[:blank:]]\z/)
  assert_equal 2003, line =~ /\s\z/
  assert_equal 2003, line.sub(/\s+\z/, "").size
end
