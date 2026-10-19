# A pattern whose program runs past 65535 instructions -- an alternation of
# 12,000 three-character words, each character three UTF-8 bytes -- compiles
# and matches as CRuby's does. Its jumps reach past 16 bits.

assert('Regexp program past 65535 instructions') do
  skip unless __ENCODING__ == "UTF-8"
  words = (0...12000).map do |i|
    [0x4E00 + i % 4000, 0x4E00 + (i * 7) % 4000, 0x4E00 + i / 4000].map { |c| c.chr("UTF-8") }.join
  end
  re = Regexp.new("(?:" + words.join("|") + ")!")
  assert_equal 48004, re.source.size
  s = "x#{words[0]}! y#{words[11999]}! z#{words[5000]}? #{words[7777]}!"
  assert_equal [4, 4, 4], s.scan(re).map(&:size)
  assert_nil "nothing here" =~ re
  assert_equal 1, s =~ re
  lit = Regexp.union(words[0, 3000] + ["end"])
  assert_equal 4, "the end" =~ lit
end
