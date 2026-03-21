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

assert("Regexp#to_s") do
  assert_equal "(?:abc)", Regexp.new("abc").to_s
  assert_equal "(?i:abc)", Regexp.new("abc", Regexp::IGNORECASE).to_s
  assert_equal "(?m:abc)", Regexp.new("abc", Regexp::MULTILINE).to_s
  assert_equal "(?im:abc)", Regexp.new("abc", Regexp::IGNORECASE | Regexp::MULTILINE).to_s
end

assert("Regexp#== and Regexp#eql?") do
  r1 = Regexp.new("abc", Regexp::IGNORECASE)
  r2 = Regexp.new("abc", Regexp::IGNORECASE)
  r3 = Regexp.new("abc")
  r4 = Regexp.new("def", Regexp::IGNORECASE)
  assert_true r1 == r2
  assert_true r1.eql?(r2)
  assert_false r1 == r3       # different flags
  assert_false r1 == r4       # different source
  assert_false r1 == "abc"    # not a Regexp
end

assert("Regexp#hash") do
  r1 = Regexp.new("abc", Regexp::IGNORECASE)
  r2 = Regexp.new("abc", Regexp::IGNORECASE)
  r3 = Regexp.new("abc")
  assert_equal r1.hash, r2.hash
  assert_not_equal r1.hash, r3.hash
end

assert("Regexp extended mode (x flag)") do
  # whitespace is ignored
  re = Regexp.new('a b c', Regexp::EXTENDED)
  assert_true re.match?("abc")
  assert_false re.match?("a b c")

  # comments are ignored
  re = Regexp.new("a  # match a\nb  # match b\nc", Regexp::EXTENDED)
  assert_true re.match?("abc")

  # whitespace inside character class is literal
  re = Regexp.new('[ ]', Regexp::EXTENDED)
  assert_true re.match?(" ")

  # escaped whitespace is preserved
  re = Regexp.new('a\\ b', Regexp::EXTENDED)
  assert_true re.match?("a b")

  # inspect shows x flag
  assert_equal "/abc/x", Regexp.new("abc", Regexp::EXTENDED).inspect

  # to_s shows x flag
  assert_equal "(?x:abc)", Regexp.new("abc", Regexp::EXTENDED).to_s
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

assert("Regexp - empty pattern") do
  assert_true //.match?("")
  assert_true //.match?("abc")
end

assert("Regexp - nested captures") do
  md = /((a)(b))c/.match("abc")
  assert_equal "abc", md[0]
  assert_equal "ab", md[1]
  assert_equal "a", md[2]
  assert_equal "b", md[3]
end

assert("Regexp - non-greedy quantifiers") do

  assert_equal "a", /a+?/.match("aaa")[0]
  assert_equal "", /a*?/.match("aaa")[0]
end

assert("Regexp - word boundary") do
  assert_equal "cat", /\bcat\b/.match("the cat sat")[0]
  assert_nil /\bcat\b/.match("concatenate")
end

assert("Regexp - non-capturing group") do
  md = /(?:a)(b)/.match("ab")
  assert_equal "ab", md[0]
  assert_equal "b", md[1]
  assert_nil md[2]
end

assert("String#sub with block") do
  assert_equal "HELLO world", "hello world".sub(/\w+/) { |m| m.upcase }
end

assert("String#gsub with block") do
  assert_equal "HELLO WORLD", "hello world".gsub(/\w+/) { |m| m.upcase }
end

assert("String#gsub date reformat") do
  result = "2026-03-21".gsub(/(\d+)-(\d+)-(\d+)/) { "#{$~[3]}/#{$~[2]}/#{$~[1]}" }
  assert_equal "21/03/2026", result
end

assert("String#scan with captures") do
  assert_equal [["1","a"],["2","b"]], "1a2b".scan(/(\d)(\w)/)
end

assert("String#split with regexp") do
  assert_equal ["a", "b", "c"], "a, b, c".split(/,\s*/)
end

assert("Regexp - case in when") do
  result = case "hello123"
           when /\d+/ then "has digits"
           else "no digits"
           end
  assert_equal "has digits", result
end

assert("Regexp - backreference \\1") do
  # match repeated word
  md = /(\w+) \1/.match("hello hello world")
  assert_equal "hello hello", md[0]
  assert_equal "hello", md[1]
end

assert("Regexp - backreference no match") do
  assert_nil /(\w+) \1/.match("hello world")
end

assert("Regexp - named captures") do
  md = /(?<year>\d+)-(?<month>\d+)-(?<day>\d+)/.match("2026-03-21")
  assert_equal "2026", md[:year]
  assert_equal "03", md[:month]
  assert_equal "21", md[:day]
  assert_equal "2026", md["year"]
end

assert("MatchData#named_captures") do
  md = /(?<a>\w+)@(?<b>\w+)/.match("user@host")
  nc = md.named_captures
  assert_equal "user", nc["a"]
  assert_equal "host", nc["b"]
end

assert("Regexp - positive lookahead (?=...)") do
  md = /\w+(?=@)/.match("user@host")
  assert_equal "user", md[0]
end

assert("Regexp - negative lookahead (?!...)") do
  md = /\d+(?!%)/.match("100%")
  assert_equal "10", md[0]
end

assert("Regexp - lookahead does not consume") do
  md = /foo(?=bar)/.match("foobar")
  assert_equal "foo", md[0]
  assert_nil /foo(?=baz)/.match("foobar")
end

assert("$1-$9 global variables") do
  /(\w+)\s(\w+)/ =~ "hello world"
  assert_equal "hello", $1
  assert_equal "world", $2
  assert_nil $3
end

assert("$1-$9 cleared on no match") do
  /(\w+)/ =~ "hello"
  assert_equal "hello", $1
  /xyz/ =~ "abc"
  assert_nil $1
end
