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

assert("MatchData - subject is snapshotted at match time") do
  # Regression: source used to alias the subject, so mutating it afterwards
  # retroactively changed what an already-created MatchData reported.
  s = "hello"
  md = /l/.match(s)
  s.upcase!
  assert_equal "l", md[0]
  assert_equal "hello", md.string
  assert_equal "he", md.pre_match
  assert_equal "lo", md.post_match
  assert_true md.string.frozen?

  s2 = "hello"
  s2 =~ /l/
  s2.upcase!
  assert_equal "l", $~[0]
  assert_equal "hello", $~.string
end

assert("MatchData - match globals survive subject mutation in a gsub block") do
  # Regression: the mrblib gsub loop republishes $&, $` and $' from the
  # MatchData after the block runs, so a block that mutates the subject used
  # to make them describe the mutated string.
  t = "hello"
  n = 0
  t.gsub(/l/) { n += 1; t.upcase! if n == 2; "X" }
  assert_equal "l", $&
  assert_equal "hel", $`
  assert_equal "o", $'
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

assert("MatchData#begin / #end - group name") do
  md = /(?<x>b)(?<y>c)/.match("abcde")
  assert_equal 1, md.begin(:x)
  assert_equal 2, md.end(:x)
  assert_equal 2, md.begin("y")
  assert_equal 3, md.end("y")
  assert_raise(IndexError) { md.begin(:zz) }
  assert_raise(IndexError) { md.end("zz") }
  # a pattern without any named group raises just the same
  assert_raise(IndexError) { /(a)/.match("a").begin(:zz) }
end

assert("MatchData#begin / #end - index out of matches") do
  # An offset has no nil to fall back on, so an index naming no group raises
  # here where MatchData#[] returns nil.
  md = /(a)(b)/.match("ab")
  assert_raise(IndexError) { md.begin(3) }
  assert_raise(IndexError) { md.end(3) }
  assert_raise(IndexError) { md.begin(-1) }
  assert_raise(IndexError) { md.end(-1) }
  # a group that exists but did not participate is still nil
  md = /(a)|(b)/.match("a")
  assert_nil md.begin(2)
  assert_nil md.end(2)
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

assert("MatchData#[] - negative index") do
  md = /(a)(b)/.match("ab")
  assert_equal "b", md[-1]
  assert_equal "a", md[-2]
  # -num_captures and below are nil: a negative index never reaches group 0
  assert_nil md[-3]
  assert_nil md[-4]
  # a group that did not participate is nil either way
  assert_nil(/(a)|(b)/.match("a")[-1])
  # out of range upwards stays nil
  assert_nil md[5]
end

assert("MatchData#[] - undefined group name") do
  md = /(?<x>a)/.match("a")
  assert_equal "a", md[:x]
  assert_raise(IndexError) { md[:zz] }
  assert_raise(IndexError) { md["zz"] }
  # a pattern without any named group raises just the same
  assert_raise(IndexError) { /(a)/.match("a")[:zz] }
end

assert("MatchData#[] - group name longer than a uint16 length") do
  # Regression: the length test truncated the requested length to uint16_t
  # while the memcmp() next to it did not, so (uint16_t)65539 == 3 ==
  # "abc".length let a 65539-byte read run off a 3-byte arena. The result is
  # unchanged either way; only a sanitizer build fails on it.
  md = /(?<abc>x)/.match("x")
  assert_raise(IndexError) { md["abc" + "A" * 65536] }
  assert_equal "x", md[:abc]
end

assert("MatchData#begin / #end - group name longer than a stored name") do
  # begin/end share the name lookup with MatchData#[], so the bound that keeps
  # the memcmp() from being handed a length larger than what was measured
  # covers them too.
  md = /(?<abc>x)/.match("x")
  assert_raise(IndexError) { md.begin("abc" + "A" * 65536) }
  assert_raise(IndexError) { md.end("abc" + "A" * 65536) }
  assert_equal 0, md.begin(:abc)
end

assert("MatchData#named_captures") do
  md = /(?<a>\w+)@(?<b>\w+)/.match("user@host")
  nc = md.named_captures
  assert_equal "user", nc["a"]
  assert_equal "host", nc["b"]
end

assert("MatchData#names") do
  assert_equal ["a", "b"], /(?<a>\w+)@(?<b>\w+)/.match("user@host").names
  assert_equal [], /\w+/.match("user").names
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

assert("$&, $`, $' and $+ global variables") do
  /b(c)/ =~ "abcd"
  assert_equal "bc", $&
  assert_equal "a", $`
  assert_equal "d", $'
  assert_equal "c", $+

  # $+ is the last group that participated, not the last group in the pattern
  /(a)|(b)/ =~ "a"
  assert_equal "a", $+

  # a pattern without groups has no $+
  /cd/ =~ "abcd"
  assert_equal "cd", $&
  assert_nil $+
end

assert("$&, $`, $' and $+ cleared on no match") do
  /b(c)/ =~ "abcd"
  assert_equal "bc", $&
  /xyz/ =~ "abc"
  assert_nil $&
  assert_nil $`
  assert_nil $'
  assert_nil $+
end
