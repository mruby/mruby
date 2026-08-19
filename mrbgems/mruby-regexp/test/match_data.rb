assert("MatchData cannot be constructed") do
  # A match builds the only MatchData there is. The class defines no
  # `initialize`, so `new` and `allocate` used to hand back an instance with
  # no data behind it, and every method on it raised TypeError instead. CRuby
  # has neither constructor; a subclass inherits the absence.
  assert_raise(NoMethodError) { MatchData.new }
  assert_raise(NoMethodError) { MatchData.allocate }
  assert_raise(NoMethodError) { Class.new(MatchData).new }
  assert_raise(NoMethodError) { Class.new(MatchData).allocate }

  # and matching itself is untouched
  md = /(\w)(\d)/.match("a1")
  assert_equal MatchData, md.class
  assert_equal ["a1", "a", "1"], md.to_a
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

assert("MatchData - the match a gsub block leaves behind is a fresh search") do
  # CRuby's `str_gsub` searches once more when the loop is over, from the
  # offset the last match was found at and on the subject as it stands then,
  # and that search is what $~ and the names derived from it describe.  A
  # block that changes the subject in place can therefore leave nil behind,
  # where the loop used to republish the MatchData of the last match it had.
  t = "hello"
  n = 0
  t.gsub(/l/) { n += 1; t.upcase! if n == 2; "X" }
  assert_nil $~
  assert_nil $&
  assert_nil $`
  assert_nil $'

  # A change that leaves the last match where it was leaves a match behind,
  # and its subject is the changed string, not the one that was matched.
  t = "hello"
  t.gsub(/l/) { t.tr!("h", "H"); "X" }
  assert_equal "l", $&
  assert_equal "Hel", $`
  assert_equal "o", $'
  assert_equal "Hello", $~.string
  assert_true $~.string.frozen?

  # The search runs from the offset the last match was found from, so a
  # match the block writes in before that offset goes unseen, and one at or
  # after it is the match left behind.
  s = "abcbd"
  n = 0
  s.gsub(/b/) { n += 1; s[0] = "b" if n == 2; "!" }
  assert_equal 3, $~.begin(0)
  assert_equal "bbc", $`
  s = "abcd"
  s.gsub(/b/) { s[0] = "b"; "!" }
  assert_equal 0, $~.begin(0)
  assert_equal "bcd", $'
  assert_equal "bbcd", $~.string
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

assert("$&, $`, $', $+ and $1 onward read $~ as it stands") do
  # Each is a reading of $~ at the moment it is read, as in CRuby, where
  # they are derived from the backref and not kept as variables of their
  # own, so assigning $~ moves all of them at once and clearing it clears
  # them, whichever search last published.
  /(b)(c)?/ =~ "abz"
  md = $~
  assert_equal ["b", "a", "z", "b", "b", nil], [$&, $`, $', $+, $1, $2]

  $~ = nil
  assert_nil $&
  assert_nil $`
  assert_nil $'
  assert_nil $+
  assert_nil $1

  $~ = md
  assert_equal ["b", "a", "z", "b", "b", nil], [$&, $`, $', $+, $1, $2]

  # a match made by one search and assigned after another is the one read
  /z/ =~ "z"
  $~ = /(x)(y)/.match("wxyz")
  assert_equal ["xy", "w", "z", "y", "x", "y"], [$&, $`, $', $+, $1, $2]
end

assert("$10 and beyond read the group of that number") do
  /(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)/ =~ "abcdefghijk"
  assert_equal "j", $10
  assert_equal "k", $11
  assert_nil $12
end

assert("a piece cut from a subject is its own string") do
  # The pieces a match hands back share the subject's buffer where they are
  # too long to embed, rather than copying its bytes. Sharing is only sound
  # while nothing can see through it, so each of these writes through one name
  # and reads back another. A piece has to be long enough not to embed for the
  # sharing to be what is under test.
  s = ("abcdefghij" * 10) + "MARK" + ("klmnopqrst" * 10)
  snapshot = s.dup
  assert_equal 100, (s =~ /MARK/)
  pre, match, post = $`, $&, $'
  pre_was, post_was = pre.dup, post.dup

  # writing through a piece reaches neither the subject nor the other pieces
  pre << "written"
  assert_equal snapshot, s
  assert_equal post_was, post
  assert_equal "MARK", match

  post.upcase!
  assert_equal snapshot, s
  assert_equal pre_was + "written", pre

  # writing through the subject reaches none of the pieces
  s << "appended"
  s.upcase!
  assert_equal post_was.upcase, post
  assert_equal pre_was + "written", pre

  # and a MatchData holds the bytes it matched, whatever becomes of the subject
  t = ("mn" * 60) + "Q" + ("op" * 60)
  md = /(\w)Q(\w)/.match(t)
  whole, before, after = md[0], md.pre_match, md.post_match
  t.replace("gone")
  assert_equal "nQo", whole
  assert_equal 119, before.bytesize
  assert_equal 119, after.bytesize
  assert_equal "m", before[-1]
end

assert("a piece cut from a frozen subject is writable") do
  # A frozen subject can be shared from without any copy, so the piece has to
  # be the one that copies when it is written to.
  f = (("qr" * 60) + "K" + ("st" * 60)).freeze
  assert_equal 120, (f =~ /K/)
  piece = $`
  assert_equal 120, piece.bytesize
  piece << "z"
  assert_equal 121, piece.bytesize
  assert_equal 241, f.bytesize
  assert_true f.frozen?
end
