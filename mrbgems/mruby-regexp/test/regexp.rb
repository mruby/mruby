assert("Regexp.new") do
  re = Regexp.new("abc")
  assert_kind_of Regexp, re
end

assert("Regexp#match - simple") do
  re = Regexp.new("abc")
  md = re.match("xabcy")
  assert_kind_of MatchData, md
  assert_equal "abc", md[0]
end

assert("Regexp#match - no match") do
  re = Regexp.new("xyz")
  assert_nil re.match("abc")
end

assert("Regexp#match?") do
  re = Regexp.new("abc")
  assert_true re.match?("xabcy")
  assert_false re.match?("xyz")
end

assert("Regexp#=~") do
  re = Regexp.new("bc")
  assert_equal 1, re =~ "abcd"
  assert_nil re =~ "xyz"
end

assert("Regexp#===") do
  re = Regexp.new("abc")
  assert_true re === "abc"
  assert_false re === "xyz"
end

assert("Regexp - character class") do
  re = Regexp.new("[a-z]+")
  md = re.match("123abc456")
  assert_equal "abc", md[0]
end

assert("Regexp - dot") do
  re = Regexp.new("a.c")
  assert_true re.match?("abc")
  assert_true re.match?("axc")
  assert_false re.match?("ac")
end

assert("Regexp - alternation") do
  re = Regexp.new("cat|dog")
  assert_equal "cat", re.match("I have a cat")[0]
  assert_equal "dog", re.match("I have a dog")[0]
end

assert("Regexp - quantifiers") do
  assert_equal "aaa", Regexp.new("a+").match("aaa")[0]
  assert_equal "", Regexp.new("a*").match("bbb")[0]
  assert_equal "ab", Regexp.new("ab?").match("ab")[0]
  assert_equal "a", Regexp.new("ab?").match("ac")[0]
end

assert("Regexp - captures") do
  re = Regexp.new("(\\w+)@(\\w+)")
  md = re.match("user@host")
  assert_equal "user@host", md[0]
  assert_equal "user", md[1]
  assert_equal "host", md[2]
end

assert("Regexp - \\d \\w \\s") do
  assert_true Regexp.new("\\d+").match?("123")
  assert_true Regexp.new("\\w+").match?("abc_123")
  assert_true Regexp.new("\\s+").match?("  ")
  assert_false Regexp.new("\\d+").match?("abc")
end

assert("Regexp - anchors") do
  assert_true Regexp.new("^abc").match?("abc")
  assert_false Regexp.new("^abc").match?("xabc")
  assert_true Regexp.new("abc$").match?("abc")
  assert_false Regexp.new("abc$").match?("abcx")
end

assert("Regexp - case insensitive") do
  re = Regexp.new("abc", Regexp::IGNORECASE)
  assert_true re.match?("ABC")
  assert_true re.match?("Abc")
end

assert("Regexp - repetition {n,m}") do
  assert_equal "aaa", Regexp.new("a{3}").match("aaaa")[0]
  assert_equal "aa", Regexp.new("a{2,3}").match("aa")[0]
  assert_equal "aaa", Regexp.new("a{2,3}").match("aaaa")[0]
end

assert("MatchData#captures") do
  re = Regexp.new("(a)(b)(c)")
  md = re.match("abc")
  assert_equal ["a", "b", "c"], md.captures
end

assert("MatchData#pre_match / #post_match") do
  re = Regexp.new("bc")
  md = re.match("abcde")
  assert_equal "a", md.pre_match
  assert_equal "de", md.post_match
end

assert("MatchData#begin / #end") do
  re = Regexp.new("bc")
  md = re.match("abcde")
  assert_equal 1, md.begin(0)
  assert_equal 3, md.end(0)
end

assert("Regexp.escape") do
  assert_equal "a\\.b\\*c", Regexp.escape("a.b*c")
end

assert("Regexp#inspect") do
  re = Regexp.new("abc", Regexp::IGNORECASE)
  assert_equal "/abc/i", re.inspect
end

assert("String#match") do
  md = "hello world".match(Regexp.new("(\\w+)\\s(\\w+)"))
  assert_equal "hello", md[1]
  assert_equal "world", md[2]
end

assert("String#sub") do
  assert_equal "hXllo", "hello".sub(Regexp.new("e"), "X")
end

assert("String#gsub") do
  assert_equal "h-ll-", "hello".gsub(Regexp.new("[eo]"), "-")
end

assert("String#scan") do
  assert_equal ["1", "2", "3"], "a1b2c3".scan(Regexp.new("\\d"))
end

assert("Regexp literal /regex/") do
  assert_true /abc/.match?("abc")
  assert_equal "123", /\d+/.match("abc123")[0]
  assert_true /hello/i.match?("HELLO")
end

assert("$~ global variable") do
  /(\w+)@(\w+)/ =~ "user@host"
  assert_kind_of MatchData, $~
  assert_equal "user", $~[1]
  assert_equal "host", $~[2]
end

assert("$~ is nil on no match") do
  /xyz/ =~ "abc"
  assert_nil $~
end

assert("Regexp.last_match") do
  /(\d+)/ =~ "abc123"
  assert_equal "123", Regexp.last_match(1)
  assert_equal "123", Regexp.last_match(0)
end
