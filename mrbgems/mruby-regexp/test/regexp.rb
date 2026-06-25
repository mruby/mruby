assert("Regexp.new with string") do
  re = Regexp.new("abc")
  assert_kind_of Regexp, re
end

assert("Regexp.new with regexp") do
  r1 = Regexp.new("abc", Regexp::IGNORECASE)
  r2 = Regexp.new(r1)
  assert_equal r1.source, r2.source
  assert_equal r1.options, r2.options
  assert_true r2.match?("ABC")
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

assert("Regexp#match - nil argument") do
  $~ = /abc/.match("abc")
  assert_nil /abc/.match(nil)
  assert_nil $~
end

assert("Regexp#match - block") do
  result = /bc/.match("abcd") { |md| [md[0], md.begin(0)] }
  assert_equal ["bc", 1], result
  assert_nil(/xyz/.match("abcd") { |md| md[0] })
end

assert("Regexp#match?") do
  re = Regexp.new("abc")
  assert_true re.match?("xabcy")
  assert_false re.match?("xyz")
  assert_false re.match?(nil)
end

assert("Regexp#match? - does not update last match") do
  $~ = /matched/.match("matched")
  assert_true /abc/.match?("abc")
  assert_equal "matched", $~[0]
  assert_false /xyz/.match?("abc")
  assert_equal "matched", $~[0]
end

assert("Regexp#=~") do
  re = Regexp.new("bc")
  assert_equal 1, re =~ "abcd"
  assert_nil re =~ "xyz"
end

assert("Regexp#=~ - nil argument clears last match") do
  $~ = /abc/.match("abc")
  assert_nil(/abc/ =~ nil)
  assert_nil $~
end

assert("Regexp#===") do
  re = Regexp.new("abc")
  assert_true re === "abc"
  assert_false re === "xyz"
  re = Regexp.new("hello (theo)")
  assert_true re === "hello theo"
  assert_equal "theo", $1
end

assert("Regexp - character class") do
  re = Regexp.new("[a-z]+")
  md = re.match("123abc456")
  assert_equal "abc", md[0]
end

assert("Regexp - \\b inside character class is backspace") do
  # Outside [...], \b is the word boundary assertion; inside [...]
  # it must mean U+0008 (backspace), matching MRI/Onigmo.
  assert_equal "Ruby", "Ruby".gsub(/[\b]/, "X")
  assert_equal "aXc", "a\bc".gsub(/[\b]/, "X")
  assert_equal ["\b", "\t", "\n"], "ABC\b\t\n".scan(/[\b-\n]/)
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

assert("Regexp - alternation is leftmost-first") do
  # Ruby tries alternatives left to right and keeps the first that lets the
  # whole pattern match -- not the longest. The linear-time engine used to
  # pick the longest branch instead.
  assert_equal "a", "ab".match(/a|ab/)[0]
  assert_equal "foo", "foobar".match(/foo|foobar/)[0]
  assert_equal "ab", "ab".match(/ab|a/)[0]
  assert_equal ["abc", "ab", "c"], "abcd".match(/(ab|abc)(c|cd)/).to_a
  assert_equal "aa", "aaa".match(/aa|a/)[0]
  # three or more branches keep source order, not just the first two
  assert_equal "car", "cart".match(/cat|car|cart/)[0]
  assert_equal "cart", "cart".match(/cat|cart|car/)[0]
  assert_equal "a", "abc".match(/a|ab|abc/)[0]
  assert_equal "abc", "abc".match(/abc|ab|a/)[0]
  # greedy quantifiers stay longest-match
  assert_equal "aaa", "aaa".match(/a+/)[0]
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

assert("Regexp - negated shorthands \\D \\W \\S") do
  # \D \W \S must be the complement of \d \w \s, not aliases of them.
  # (A double negation in the compiler made \D match digits, etc.)
  assert_equal ["a", " ", "b"], "a1 b2".scan(/\D/)
  assert_equal [" "],           "a1 b2".scan(/\W/)
  assert_equal ["a", "1", "b", "2"], "a1 b2".scan(/\S/)
  assert_equal "_9__", "x9 z".gsub(/\D/, "_")
  # inside [...] the shorthands keep working, including mixed full-range sets
  assert_equal ["a", " ", "b"], "a5 b".scan(/[\D]/)
  assert_equal ["a", "5", " ", "b"], "a5 b".scan(/[\s\S]/)
  assert_equal [" "], "foo BAR".scan(/[\W\d]/)
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

assert("MatchData captures across alternation branches") do
  # The branch that matches must record its own capture, whichever side of
  # the alternation it is on (regression: the left branch used to come back
  # nil because the Pike VM clobbered its capture slot during compaction).
  md = /(\d)|(x)/.match("1")
  assert_equal "1", md[1]
  assert_nil md[2]
  md = /(\d)|(x)/.match("x")
  assert_nil md[1]
  assert_equal "x", md[2]
  md = /(cat)|(dog)/.match("cat")
  assert_equal ["cat", nil], md.captures
end

assert("MatchData#pre_match / #post_match") do
  re = Regexp.new("bc")
  md = re.match("abcde")
  assert_equal "a", md.pre_match
  assert_equal "de", md.post_match
end

assert("MatchData#string") do
  md = Regexp.new("bc").match("abcde")
  assert_equal "abcde", md.string
end

assert("MatchData#regexp") do
  re = Regexp.new("bc")
  md = re.match("abcde")
  assert_equal re, md.regexp
end

assert("MatchData#to_s") do
  md = Regexp.new("bc").match("abcde")
  assert_equal "bc", md.to_s
end

assert("MatchData#begin / #end") do
  re = Regexp.new("bc")
  md = re.match("abcde")
  assert_equal 1, md.begin(0)
  assert_equal 3, md.end(0)
end

assert("Regexp - multibyte (UTF-8) match extraction") do
  # Capture offsets are recorded in bytes; substring extraction must honor
  # them as byte ranges so multibyte matches are not corrupted.
  skip unless __ENCODING__ == "UTF-8"
  assert_equal "あ", "あa".match(/\S/)[0]
  assert_equal ["あ", "a", "い"], "あ a い".scan(/\S/)
  assert_equal "本", "日本語".match(/本/)[0]
  md = "いろは".match(/ろ/)
  assert_equal "い", md.pre_match
  assert_equal "は", md.post_match
  assert_equal ["β", "γ"], "αβγ".match(/(β)(γ)/).captures
  assert_equal "ああいいうう", "あいう".gsub(/./) { |m| m + m }
  assert_equal "x-y", "x—y".sub(/—/) { "-" }
  assert_equal ["1", "2", "3"], "ABCあいう123".scan(/\d/)

  # MatchData#begin/#end report CHARACTER offsets like CRuby, not bytes.
  m = "αβγ".match(/(β)(γ)/)
  assert_equal [1, 2], [m.begin(1), m.end(1)]
  assert_equal [2, 3], [m.begin(2), m.end(2)]
  assert_equal 2, "あいう".match(/う/).begin(0)

  assert_equal 2, /あ/.match("あいあ", 2).begin(0)
  assert_equal 2, /あ/.match("あいあ", -1).begin(0)
  assert_nil /い/.match("あいあ", 2)
  assert_nil /あ/.match("あいあ", 4)
  assert_nil /あ/.match("あいあ", -4)
  assert_true /あ/.match?("あいあ", 2)
  assert_false /い/.match?("あいあ", 2)
end

assert("String#gsub - regexp search position is byte-based internally") do
  skip unless __ENCODING__ == "UTF-8"
  assert_equal "あ-い-う", "あ,い,う".gsub(/,/, "-")
end

assert("String#split - regexp search position is byte-based internally") do
  skip unless __ENCODING__ == "UTF-8"
  assert_equal ["あ", "い", "う"], "あ,い,う".split(/,/)
  assert_equal ["あ", ",", "い", ",", "う"], "あ,い,う".split(/(,)/)
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

assert("Regexp#hash/== on uninitialized regexp") do
  # Regexp.allocate yields an object with no @source IV; hash/== must
  # not crash (regression: ObjectSpace.each_object could expose a
  # half-initialized Regexp after Regexp.new raised a compile error).
  r = Regexp.allocate
  assert_kind_of Integer, r.hash
  assert_true r == r
  assert_false r == Regexp.allocate
  assert_false r == Regexp.new("abc")
end

assert("Regexp#options") do
  assert_equal 0, Regexp.new("abc").options
  assert_equal Regexp::IGNORECASE, Regexp.new("abc", Regexp::IGNORECASE).options
  assert_equal Regexp::MULTILINE, Regexp.new("abc", Regexp::MULTILINE).options
  assert_equal Regexp::EXTENDED, Regexp.new("abc", Regexp::EXTENDED).options
  assert_equal Regexp::IGNORECASE | Regexp::MULTILINE,
               Regexp.new("abc", Regexp::IGNORECASE | Regexp::MULTILINE).options
  assert_equal Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE,
               Regexp.new("abc", Regexp::IGNORECASE | Regexp::EXTENDED | Regexp::MULTILINE).options
end

assert("Regexp#casefold?") do
  assert_true Regexp.new("abc", Regexp::IGNORECASE).casefold?
  assert_false Regexp.new("abc").casefold?
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

assert("String#sub with \\& \\` \\' specials") do
  # \& = full match
  assert_equal "a[bc]d", "abcd".sub(/bc/, '[\\&]')
  # \` = pre_match
  assert_equal "a[a]d", "abcd".sub(/bc/, '[\\`]')
  # \' = post_match
  assert_equal "a[d]d", "abcd".sub(/bc/, "[\\']")
  # \+ = last capture
  assert_equal "a[c]d", "abcd".sub(/(b)(c)/, '[\\+]')
  # \\ = literal backslash
  assert_equal "a\\d", "abcd".sub(/bc/, "\\\\")
  # \1 still works
  assert_equal "abbd", "abcd".sub(/(b)c/, '\\1\\1')
end

assert("String#sub \\' post-match uses byte length, not strlen (issue #6892)") do
  # An embedded NUL before the match end used to make \' compute its length
  # with strlen(), underflowing into a wild memcpy and crashing.
  s = "A\0" + ("B" * 40) + "MATCH"
  assert_equal 44, s.sub(/MATCH/, "X\\'Y").length
  assert_equal "A\0" + ("B" * 40) + "XY", s.sub(/MATCH/, "X\\'Y")

  # A shared substring whose logical end is not NUL-terminated must not let
  # \' copy bytes past the substring into the parent's buffer.
  parent = ("Q" * 200) + "MATCHzzzzzzzzzzzzzzzz"
  assert_equal ("Q" * 100) + "[]", parent[100, 105].sub(/MATCH/, "[\\']")
end

assert("String#gsub with \\& special") do
  assert_equal "[a][b][c]", "abc".gsub(/./, '[\\&]')
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

assert("String#gsub with block and zero-width match") do
  assert_equal "!abc", "abc".gsub(/^/) { "!" }
  assert_equal "a!bc", "abc".gsub(/(?=b)/) { "!" }
  assert_equal "!a!b!c!", "abc".gsub(//) { "!" }
  assert_equal "!\n", "\n".gsub(/^/m) { "!" }
  assert_equal "!a\n", "a\n".gsub(/^/m) { "!" }
  assert_equal "!a\n!b", "a\nb".gsub(/^/m) { "!" }
  if __ENCODING__ == "UTF-8"
    assert_equal "！いろは", "いろは".gsub(/^/) { "！" }
    assert_equal "い！ろは", "いろは".gsub(/(?=ろ)/) { "！" }
    assert_equal "！い！ろ！は！", "いろは".gsub(//) { "！" }
  end
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

assert("String#split delegates non-regexp patterns") do
  assert_equal ["a", "b"], " a  b ".split
  assert_equal ["a", "b"], " a  b ".split(nil)
  assert_equal ["a", "b", "c"], "a,b,c".split(",")
  assert_equal ["a", "b", "c"], "abc".split("")
  assert_equal ["a", "b", "c", ""], "abc".split("", -1)
  assert_equal ["a", "b"], "a\\b".split("\\")
end

assert("String#split with regexp limit") do
  assert_equal ["a"], "a,".split(/,/, 0)
  assert_equal ["a", ""], "a,".split(/,/, -1)
  assert_equal ["a", ""], "a,".split(/,/, 2)
  assert_equal ["a,b,"], "a,b,".split(/,/, 1)
  assert_raise(TypeError) { "a,b".split(/,/, nil) }
  assert_equal ["a,b"], "a,b".split(/,/, 1.5)

  limit = Object.new
  def limit.to_int; 2; end
  assert_equal ["a", "b"], "a,b".split(/,/, limit)

  limit = Object.new
  def limit.to_int; 1.5; end
  assert_raise(TypeError) { "a,b".split(/,/, limit) }
end

assert("String#split with empty regexp") do
  assert_equal ["a", "b", "c"], "abc".split(//)
  assert_equal ["a", "bc"], "abc".split(//, 2)
  assert_equal ["a", "b", "c"], "abc".split(//, 3)
  assert_equal ["a", "b", "c", ""], "abc".split(//, 4)
  assert_equal ["a", "b", "c", ""], "abc".split(//, -1)
  assert_equal [], "".split(//, -1)
  assert_equal ["a", ""], "a".split(//, -1)
  assert_equal ["あ", "い"], "あい".split(//)
  assert_equal ["あ", "い"], "あい".split(//, 2)
  assert_equal ["あ", "い", ""], "あい".split(//, -1)
end

assert("String#split with invalid regexp pattern type") do
  assert_raise(TypeError) { "abc".split(1) }
  assert_equal ["abc"], "abc".split(1, 1)
end

assert("String#split with regexp captures") do
  assert_equal ["a1b2c"], "a1b2c".split(/(\d)/, 1)
  assert_equal ["a", "1", "b", "2", "c"], "a1b2c".split(/(\d)/)
  assert_equal ["a", "1", "b2c"], "a1b2c".split(/(\d)/, 2)
  assert_equal ["a", "1", "b", "2", "c"], "a1b2c".split(/(\d)/, 3)
  assert_equal ["a", "1", "b", "2", "c"], "a1b2c".split(/(\d)/, -1)
  assert_equal ["hell"], "hello".split(/(x)?o/)
  assert_equal ["hell", ""], "hello".split(/(x)?o/, -1)
end

assert("String#split with zero-width regexp") do
  assert_equal ["ab"], "ab".split(/(?=b)/, 1)
  assert_equal ["a", "b"], "ab".split(/(?=b)/, 2)
  assert_equal ["a", "b"], "ab".split(/(?=b)/, 3)
  assert_equal ["a", "bc"], "abc".split(/(?=b)/)
  assert_equal ["a", "bc"], "abc".split(/(?=b)/, -1)
  assert_equal ["abc"], "abc".split(/(?=a)/)
  assert_equal ["abc"], "abc".split(/(?=a)/, -1)
  assert_equal ["ab", "c"], "abc".split(/(?=c)/)
  assert_equal ["abc"], "abc".split(/^/)
  assert_equal ["abc", ""], "abc".split(/$/, -1)
  assert_equal ["a", "b", "bc"], "abc".split(/(?=(b))/)
end

assert("String#split with multibyte regexp") do
  assert_equal ["あ", "い"], "あい".split(/(?=い)/)
  assert_equal ["あ", "い"], "あい".split(/(?=い)/, -1)
  assert_equal ["あ", "い", "い"], "あい".split(/(?=(い))/)
  assert_equal ["", "あ", "い"], "あい".split(/(あ)/)
  assert_equal ["", "あ", "い"], "あい".split(/(あ)/, -1)
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

assert("Regexp - named captures survive /x preprocessing") do
  # Regression: with /x, mrb_re_compile freed the stripped buffer that
  # named_captures[i].name pointed into.
  re = /(?<n>\d+) # comment
       \s* (?<u>\w+) /x
  m = re.match("42 px")
  assert_equal "42", m[:n]
  assert_equal "px", m[:u]
end

assert("Regexp - named captures survive source string mutation") do
  # Regression: name pointer used to alias RSTRING_PTR of the source.
  s = String.new("(?<key>\\d+)")
  re = Regexp.new(s)
  s.replace("X" * 10000)   # force buffer reallocation
  m = re.match("abc 123 def")
  assert_equal "123", m[:key]
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

assert("Regexp - positive lookbehind (?<=...)") do
  md = Regexp.new("(?<=@)\\w+").match("user@host")
  assert_equal "host", md[0]
  assert_nil Regexp.new("(?<=@)\\w+").match("user_host")
end

assert("Regexp - negative lookbehind (?<!...)") do
  md = Regexp.new("(?<!\\d)px").match("12px auto")
  assert_nil md  # preceded by digit
  md = Regexp.new("(?<!\\d)em").match("12px 1.5em auto")
  assert_nil md  # preceded by digit
  md = Regexp.new("(?<!\\d)px").match("top px")
  assert_equal "px", md[0]
end

assert("Regexp - lookbehind with literal string") do
  md = Regexp.new("(?<=foo)bar").match("foobar")
  assert_equal "bar", md[0]
  assert_nil Regexp.new("(?<=foo)bar").match("bazbar")
end

assert("Regexp - lookbehind at string start") do
  # lookbehind should fail if not enough text before
  assert_nil Regexp.new("(?<=abc)d").match("d")
  # but should work at correct position
  md = Regexp.new("(?<=abc)d").match("abcd")
  assert_equal "d", md[0]
end

assert("Regexp - negative lookbehind at string start") do
  # negative lookbehind succeeds when not enough text before
  md = Regexp.new("(?<!x)a").match("a")
  assert_equal "a", md[0]
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

assert("Regexp - consecutive optional quantifiers (#6853)") do
  # insert_inst was over-incrementing jump offsets that pointed *at* the
  # insertion site, sending earlier "skip this atom" SPLITs into the next
  # atom's body. Two adjacent zero-matchable atoms then both failed even
  # when both should match zero characters.
  assert_equal ["a", nil],   /\Aa(b)?c?\z/.match("a").to_a
  assert_equal ["ab", "b"],  /\Aa(b)?c?\z/.match("ab").to_a
  assert_equal ["ac", nil],  /\Aa(b)?c?\z/.match("ac").to_a
  assert_equal ["abc", "b"], /\Aa(b)?c?\z/.match("abc").to_a

  assert_equal [""], /a?b?/.match("").to_a
  assert_equal [""], /a*b*/.match("").to_a
  assert_equal [""], /a?b?c?d?/.match("").to_a
end

assert("Regexp - empty-matchable patterns find earliest match position") do
  # When a regex can match zero characters via epsilon transitions, the
  # first-byte skip-ahead optimization is unsafe: skipping past bytes
  # that aren't in the first-byte set would also skip past valid
  # empty-match positions.
  md = /a?/.match("b")
  assert_equal "", md[0]
  assert_equal 0, md.begin(0)

  md = /a?b?/.match("c")
  assert_equal "", md[0]
  assert_equal 0, md.begin(0)
end

assert("Regexp - UTF-8 codepoints in character class") do
  assert_equal 0, ("β" =~ /[α-ω]/)
  assert_nil ("Z" =~ /[α-ω]/)
  assert_equal ["₀₁₂"], "a₀₁₂b".scan(/[₀-₉]+/)
  assert_true "₇₈₉".match?(/[₀₁₂₃₄₅₆₇₈₉]+/)
  assert_equal 0, ("か" =~ /[あ-ん]/)
  # negation
  assert_nil ("β" =~ /[^α-ω]/)
  assert_equal 0, ("x" =~ /[^α-ω]/)
  # mixed ASCII / non-ASCII range
  assert_equal 0, ("m" =~ /[a-z₀-₉]/)
  assert_equal 0, ("₅" =~ /[a-z₀-₉]/)
end

assert("Regexp - quantifier over multi-byte char class") do
  assert_equal "a#b#c", "a₀₁b₂c".gsub(/[₀-₉]+/, "#")
  assert_equal ["₀₁₂"], "₀₁₂".scan(/[₀-₉]+/)
end

assert("Regexp - octal and hex escapes") do
  assert_equal 0, (/\033/ =~ "\e")
  assert_equal 0, (/\x1b/ =~ "\e")
  assert_equal 0, (/[\x41]/ =~ "A")
  assert_equal 0, (/[\101]/ =~ "A")
  assert_equal 0, (/\x7/ =~ "\a")
end

assert("Regexp - \\h and \\H hex-digit shorthands") do
  assert_equal 0, (/\h/ =~ "f")
  assert_nil (/\h/ =~ "g")
  assert_equal 0, (/\H/ =~ "g")
  assert_nil (/\H/ =~ "a")
  assert_equal ["3f"], "3fX".scan(/[\h]+/)
  assert_equal ["XY"], "3fXY".scan(/[\H]+/)
  assert_equal ["deadBEEF"], "deadBEEFzz".scan(/\h+/)
end
