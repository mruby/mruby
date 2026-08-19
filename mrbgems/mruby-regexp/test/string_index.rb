assert("String#[] with regexp") do
  assert_equal "ll", "hello"[/l+/]
  assert_equal "ll", "hello".slice(/l+/)
  assert_nil "hello"[/z/]
  assert_nil "hello".slice(/z/)
  assert_equal "", "hello"[//]

  # the result is a plain String even for a subclass receiver, as in CRuby
  sub = Class.new(String)
  assert_equal String, sub.new("hello")[/l+/].class
end

assert("String#[] with regexp and capture") do
  assert_equal "llo", "hello"[/(l+)(o)/, 0]
  assert_equal "ll", "hello"[/(l+)(o)/, 1]
  assert_equal "o", "hello"[/(l+)(o)/, 2]
  assert_equal "ll", "hello"[/(?<x>l+)/, :x]
  assert_equal "ll", "hello"[/(?<x>l+)/, "x"]
  assert_equal "ll", "hello".slice(/(l+)/, 1)

  # a group that did not take part in the match answers nil
  assert_nil "hello"[/(z)?(l+)/, 1]

  # handed to MatchData#[] as it stands: a negative index counts back from
  # the last group, an index past the last group is nil, and a name that
  # resolves to no group is a mistake at the point of the call
  assert_equal "o", "hello"[/(l+)(o)/, -1]
  assert_nil "hello"[/(l+)/, 5]
  assert_raise(IndexError) { "hello"[/(?<x>l+)/, :zz] }
  assert_raise(IndexError) { "hello"[/(l+)/, "x"] }
  assert_raise(TypeError) { "hello"[/(l+)/, nil] }

  # a failed match answers nil without ever looking at the capture argument
  assert_nil "hello"[/(z)/, 1]
  assert_nil "hello"[/(?<x>z)/, :zz]
end

assert("String#[] with regexp sets the match globals") do
  assert_equal "ll", "hello"[/l+/]
  assert_equal "ll", $~[0]
  assert_equal "ll", Regexp.last_match(0)

  "hello"[/(l)(l)/, 2]
  assert_equal "l", $1

  # a failed match clears $~, which is why this goes through `match` rather
  # than `match?`
  assert_nil "hello"[/z/]
  assert_nil $~
end

assert("String#[] delegates every non-regexp argument") do
  assert_equal "e", "hello"[1]
  assert_equal "e", "hello".slice(1)
  assert_equal "ell", "hello"[1, 3]
  assert_equal "ell", "hello".slice(1, 3)
  assert_equal "ell", "hello"[1..3]
  assert_equal "llo", "hello"[-3..-1]
  assert_equal "ll", "hello"["ll"]
  assert_nil "hello"["bye"]
  assert_nil "hello"[12]
  assert_nil "hello"[12, 1]

  # the same delegation on a subclass receiver, which the inline index
  # opcodes never answer and which therefore always arrives here
  sub = Class.new(String)
  assert_equal "e", sub.new("hello")[1]
  assert_equal "ell", sub.new("hello")[1..3]
  assert_equal "ll", sub.new("hello")["ll"]

  # the arity and type errors are the ones the C method raises
  assert_raise(ArgumentError) { "hello"[] }
  assert_raise(ArgumentError) { "hello"[1, 2, 3] }
  assert_raise(ArgumentError) { "hello".slice(1, 2, 3) }
  assert_raise(ArgumentError) { "hello"[/l/, 1, 2] }
  assert_raise(TypeError) { "hello"[nil] }
end

assert("String#[] reads the real type of its argument") do
  # `is_a?` is redefinable, so a Regexp denying its own type must still be
  # matched against, and an object claiming to be one must not be
  re = /l+/
  def re.is_a?(klass); false; end
  assert_equal "ll", "hello"[re]

  fake = Object.new
  def fake.is_a?(klass); true; end
  def fake.match(str); raise "must not be called"; end
  assert_raise(TypeError) { "hello"[fake] }
end

assert("String#[]= with regexp") do
  s = "hello"
  assert_equal "X", (s[/l+/] = "X")
  assert_equal "heXo", s

  # a multibyte subject: `MatchData#begin` and `#end` report character
  # offsets, which is the space the two-integer form of `[]=` works in
  s = "あいlluえお"
  s[/l+/] = "X"
  assert_equal "あいXuえお", s

  # an empty match replaces an empty span
  s = "hello"
  s[/x*/] = "X"
  assert_equal "Xhello", s

  # a pattern that does not match is an error, unlike the read side's nil
  s = "hello"
  assert_raise(IndexError) { s[/z/] = "X" }
  assert_equal "hello", s
end

assert("String#[]= with regexp and capture") do
  s = "hello"
  s[/(l+)(o)/, 1] = "X"
  assert_equal "heXo", s

  s = "hello"
  s[/(l+)(o)/, 0] = "X"
  assert_equal "heX", s

  # a negative index counts back from the last group, and is rejected once
  # it reaches group 0, so the whole match is out of its reach
  s = "hello"
  s[/(l+)(o)/, -1] = "X"
  assert_equal "hellX", s
  assert_raise(IndexError) { "hello"[/l+/, -1] = "X" }

  s = "hello"
  s[/(?<x>l+)/, :x] = "Y"
  assert_equal "heYo", s

  s = "あいlluえお"
  s[/(?<x>l+)/, "x"] = "Y"
  assert_equal "あいYuえお", s

  # an index that reaches no group is an error here, where the read side
  # answers nil
  assert_raise(IndexError) { "hello"[/(l+)/, 5] = "X" }
  # so is a group that exists but did not take part in the match
  assert_raise(IndexError) { "hello"[/(h)|(z)/, 2] = "X" }
  # and so is a name that resolves to no group
  assert_raise(IndexError) { "hello"[/(?<x>l+)/, :zz] = "X" }
  assert_raise(IndexError) { "hello"[/(l+)/, "x"] = "X" }
  assert_raise(TypeError) { "hello"[/(l+)/, nil] = "X" }
end

assert("String#[]= with regexp sets the match globals") do
  s = "hello"
  s[/l+/] = "X"
  assert_equal "ll", $~[0]
  # the MatchData describes the subject as it was before the replacement
  assert_equal "hello", $~.string

  s = "hello"
  s[/(l)(l)/, 2] = "X"
  assert_equal "l", $1

  # a failed match clears $~ before the IndexError, which is why this goes
  # through `match` rather than `match?`
  assert_raise(IndexError) { "hello"[/z/] = "X" }
  assert_nil $~
end

assert("String#[]= with regexp searches before it checks the receiver") do
  # CRuby modifies last, so a frozen receiver raises only once the search
  # has left its match behind, and a pattern that does not match raises
  # IndexError rather than FrozenError
  $~ = nil
  assert_raise(FrozenError) { "hello".freeze[/l+/] = "X" }
  assert_equal "ll", $~[0]

  $~ = nil
  assert_raise(IndexError) { "hello".freeze[/z/] = "X" }
  assert_nil $~
end

assert("String#[]= delegates every non-regexp argument") do
  s = "hello"
  s[0] = "H"
  assert_equal "Hello", s
  s[1, 3] = "X"
  assert_equal "HXo", s
  s = "hello"
  s[1..3] = "X"
  assert_equal "hXo", s
  s = "hello"
  s["ll"] = "X"
  assert_equal "heXo", s

  # the errors are the ones the C method raises, and the replacement reaches
  # its type check unconverted.  Only the regexp form's arity is the
  # override's to report: the core reads its arguments in order, so a
  # four-argument call is rejected for the replacement's type before the
  # count is ever looked at.
  assert_raise(ArgumentError) { "hello"[/l/, 1, 2] = "X" }
  assert_raise(TypeError) { "hello"[1, 2, 3] = "X" }
  assert_raise(IndexError) { "hello"["bye"] = "X" }
  assert_raise(TypeError) { "hello"[nil] = "X" }
  assert_raise(TypeError) { "hello"[/l/] = :sym }

  # `is_a?` is redefinable, so the real type is what decides
  re = /l+/
  def re.is_a?(klass); false; end
  s = "hello"
  s[re] = "X"
  assert_equal "heXo", s
end

assert("String#slice! with regexp") do
  s = "hello"
  assert_equal "ll", s.slice!(/l+/)
  assert_equal "heo", s

  s = "あいlluえお"
  assert_equal "ll", s.slice!(/l+/)
  assert_equal "あいuえお", s

  # a pattern that does not match removes nothing and answers nil
  s = "hello"
  assert_nil s.slice!(/z/)
  assert_equal "hello", s

  # an empty match removes nothing but is still a match
  s = "hello"
  assert_equal "", s.slice!(/x*/)
  assert_equal "hello", s

  # a plain String even for a subclass receiver, as in CRuby
  sub = Class.new(String)
  assert_equal String, sub.new("hello").slice!(/l+/).class
end

assert("String#slice! with regexp and capture") do
  s = "hello"
  assert_equal "l", s.slice!(/(l)(o)/, 1)
  assert_equal "helo", s
  # the MatchData left behind describes the whole match, not the capture
  assert_equal "lo", $~[0]

  s = "hello"
  assert_equal "o", s.slice!(/(l+)(o)/, -1)
  assert_equal "hell", s

  s = "hello"
  assert_equal "ll", s.slice!(/(?<x>l+)/, :x)
  assert_equal "heo", s

  # where `[]=` raises, an index that reaches no group answers nil here,
  # group 0 included once the index is negative
  s = "hello"
  assert_nil s.slice!(/(l+)/, 5)
  assert_nil s.slice!(/l+/, -1)
  assert_equal "hello", s

  # a group that exists but did not take part in the match answers "" and
  # removes nothing, as in CRuby
  s = "hello"
  assert_equal "", s.slice!(/(h)|(z)/, 2)
  assert_equal "hello", s

  # only a name that resolves to no group raises, as it does for `[]=`
  assert_raise(IndexError) { "hello".slice!(/(?<x>l+)/, :zz) }
  assert_raise(IndexError) { "hello".slice!(/(l+)/, "x") }
  assert_raise(TypeError) { "hello".slice!(/(l+)/, nil) }
end

assert("String#slice! with regexp checks the receiver before it searches") do
  # the opposite order from `[]=`, and CRuby draws the same distinction: the
  # check comes first, so a pattern that would not have matched still raises
  # and $~ is left alone
  $~ = nil
  assert_raise(FrozenError) { "hello".freeze.slice!(/l+/) }
  assert_nil $~
  assert_raise(FrozenError) { "hello".freeze.slice!(/z/) }
  assert_nil $~
end

assert("String#slice! delegates every non-regexp argument") do
  s = "hello"
  assert_equal "e", s.slice!(1)
  assert_equal "hllo", s
  s = "hello"
  assert_equal "ell", s.slice!(1, 3)
  assert_equal "ho", s
  s = "hello"
  assert_equal "ell", s.slice!(1..3)
  assert_equal "ho", s
  s = "hello"
  assert_equal "ll", s.slice!("ll")
  assert_equal "heo", s
  assert_nil "hello".slice!("bye")

  assert_raise(ArgumentError) { "hello".slice! }
  assert_raise(ArgumentError) { "hello".slice!(1, 2, 3) }
  assert_raise(ArgumentError) { "hello".slice!(/l/, 1, 2) }
  assert_raise(TypeError) { "hello".slice!(nil) }
  assert_raise(FrozenError) { "hello".freeze.slice!(0) }
end

assert("String#index with regexp") do
  assert_equal 1, "hello".index(/e/)
  assert_equal 2, "hello".index(/l+/)
  assert_nil "hello".index(/z/)
  assert_equal 0, "hello".index(//)

  # a start position, and a negative one counting back from the end
  assert_equal 3, "hello".index(/l/, 3)
  assert_equal 3, "hello".index(/l/, -2)
  assert_nil "hello".index(/l/, 4)

  # the end of the subject is a position a match can start at, and anything
  # past either end is a miss
  assert_equal 5, "hello".index(//, 5)
  assert_nil "hello".index(//, 6)
  assert_nil "hello".index(/l/, -10)
end

assert("String#rindex with regexp") do
  assert_equal 3, "hello".rindex(/l/)
  assert_nil "hello".rindex(/z/)
  assert_equal 5, "hello".rindex(//)

  # the last match and not the first, which is the whole of the difference
  # from `index`
  assert_equal 4, "abcabc".rindex(/b/)
  # the last position a match starts at, so the longer match at 2 loses to
  # the shorter one at 3
  assert_equal 3, "hello".rindex(/l+/)
  # and overlapping matches are in view: a walk that resumed at the match end
  # would answer 0 here
  assert_equal 1, "aaa".rindex(/aa/)

  # the position bounds where the match starts, not where it ends
  assert_equal 1, "abcabc".rindex(/bca/, 1)
  assert_nil "abcabc".rindex(/bca/, 0)
  assert_equal 2, "hello".rindex(/l/, 2)
  assert_equal 3, "hello".rindex(/l/, -1)

  # past the end of the subject clamps to it, where past the negative end is
  # a miss
  assert_equal 3, "hello".rindex(/l/, 10)
  assert_nil "hello".rindex(/l/, -10)
end

assert("String#rindex bounds the match start in characters") do
  # The position `rindex` takes is a character offset and the one
  # `byterindex` takes is a byte offset. On a single-byte subject the two
  # name the same place, so only a multibyte one says which is being read.
  skip unless __ENCODING__ == "UTF-8"
  str = "あいうあいう"    # 6 characters, 18 bytes

  assert_equal 4, str.rindex(/い/)
  assert_equal 12, str.byterindex(/い/)

  # 1 as a character offset is the second character, which the first `い` is;
  # as a byte offset it is inside the first character and reaches no match at
  # all
  assert_equal 1, str.rindex(/い/, 1)
  assert_nil str.rindex(/い/, 0)
  assert_equal 3, str.byterindex(/い/, 3)

  # 4 as a character offset reaches the second `い`, where the same number of
  # bytes is still short of the first
  assert_equal 4, str.rindex(/い/, 4)
  assert_equal 1, str.rindex(/い/, 3)

  # a negative position counts back in the same space it counts forward in
  assert_equal 4, str.rindex(/い/, -1)
  assert_equal 1, str.rindex(/い/, -3)

  # overlapping matches stay in view across a multibyte character, where a
  # walk that resumed at the match end would answer 0
  assert_equal 1, "あああ".rindex(/ああ/)
  assert_equal 3, "あああ".byterindex(/ああ/)
  assert_equal ["あ", "ああ", ""], "あああ".rpartition(/ああ/)
  assert_equal ["あい", "うあ", "いう"], str.rpartition(/うあ/)

  # the position bounds where the match starts and not how far the subject is
  # read: `$` still asserts at the end of the subject, and a match that
  # reaches it is still found from a position well before it
  assert_nil "あいうい".rindex(/い$/, 1)
  assert_equal 3, "あいうい".rindex(/い$/)
  assert_equal 1, str.rindex(/いうあいう/, 1)
end

assert("a backward search bounds where a match starts, not how far it reads") do
  # `$` and `\z` assert at the end of the subject. A position that bounded
  # how much of the subject was read would put that end at the bound instead,
  # and the `b` at 1 would answer where it must not.
  assert_nil "abcb".rindex(/b$/, 1)
  assert_nil "abcb".rindex(/b\z/, 1)
  assert_equal 3, "abcb".rindex(/b$/)

  # and a match may reach past the bound, since what the bound names is where
  # the match begins
  assert_equal 1, "abcabc".rindex(/bcabc/, 1)
  assert_equal 1, "abcabc".byterindex(/bcabc/, 1)
end

assert("a backward search answers a match far from the end of the subject") do
  # A backward search asks about the end of the subject first, and only what
  # it does not find there sends it over the whole subject. A subject long
  # enough for those to be two different paths says that both answer what a
  # short one answers.
  str = "ab" + "c" * 4000

  assert_equal 0, str.rindex(/ab/)
  assert_equal 1, str.rindex(/b/)
  assert_equal 4001, str.rindex(/c/)
  assert_nil str.rindex(/z/)
  assert_equal 0, str.byterindex(/ab/)
  assert_equal 4001, str.byterindex(/c/)
  assert_equal ["", "ab", "c" * 4000], str.rpartition(/ab/)

  # a match at the end and further ones behind it: the last is still the
  # answer when the search starts from the end rather than the front
  tail = "c" * 4000 + "abab"
  assert_equal 4002, tail.rindex(/ab/)
  # overlapping matches stay in view there too, as they do on a short subject
  assert_equal 4001, tail.rindex(/ba/)
  assert_equal ["c" * 4000 + "ab", "ab", ""], tail.rpartition(/ab/)

  # the bound is read the same way whichever path answers
  assert_equal 0, str.rindex(/ab/, 0)
  assert_nil str.rindex(/b/, 0)
  assert_equal 4000, tail.rindex(/ab/, 4000)

  # and the match it settles on is the one the globals describe
  assert_equal 4002, tail.rindex(/a(b)/)
  assert_equal "b", $1
  assert_equal "ab", Regexp.last_match(0)
  assert_nil str.rindex(/(z)/)
  assert_nil $1
  assert_nil Regexp.last_match(0)
end

assert("a backward search answers a long multibyte subject") do
  # The subject above is single-byte, so the two paths of the search were
  # asked about characters only on a short one. A search reads the subject by
  # byte wherever it starts from, so a place it starts from falls inside a
  # character as often as not, and the path that reaches the front of a long
  # subject is not the path that answers from the end of it.
  skip unless __ENCODING__ == "UTF-8"
  mb = "あい" + "うえ" * 2000    # 4,002 characters, 12,006 bytes

  # near the end, where the search settles without crossing the subject
  assert_equal 4001, mb.rindex(/え/)
  assert_equal 12003, mb.byterindex(/え/)
  assert_equal 4000, mb.rindex(/うえ/)
  assert_equal 12000, mb.byterindex(/うえ/)

  # at the front, which is the far end of the same subject
  assert_equal 0, mb.rindex(/あい/)
  assert_equal 0, mb.byterindex(/あい/)
  assert_nil mb.rindex(/お/)
  assert_equal ["", "あい", "うえ" * 2000], mb.rpartition(/あい/)

  # overlapping matches stay in view at that end too
  assert_equal 1, ("あああ" + "い" * 3000).rindex(/ああ/)

  # and the position is still a character offset for one of the pair and a
  # byte offset for the other
  assert_equal 0, mb.rindex(/あい/, 0)
  assert_nil mb.rindex(/うえ/, 1)
  assert_equal 2, mb.rindex(/うえ/, 2)
  assert_equal 6, mb.byterindex(/うえ/, 6)
  assert_nil mb.byterindex(/うえ/, 3)
end

assert("a backward search raises where one of its searches hits a limit") do
  # The search makes up to three searches: a window at the end that widens,
  # the whole range when no window answers, and a walk forward from the match
  # it found. A limit in any of them is the answer to the whole question, so
  # each raises rather than read it as a window with no match in it, which
  # would widen, or as the walk having run out of matches, which would answer
  # the one before.
  #
  # The subjects are sized from the recursion limit, which the build sets and
  # `Regexp::RECURSION_LIMIT` reads back. An atomic group nested d deep spends
  # 2d frames per copy and one more per iteration of a repetition, and d is
  # chosen so that a run inside the 256 bytes the window reads spends the
  # limit; a build with a limit the window cannot reach sends the first and
  # third cases through the whole-range search instead, where they raise the
  # same.
  limit = Regexp::RECURSION_LIMIT
  d = limit / 600 + 1
  atom = "a"
  d.times { atom = "(?>#{atom})" }
  # the window, grown to the whole subject: the lookbehind lets only position
  # 0 try, and that one gives up
  n = limit / (2 * d + 1) + 1
  assert_raise(RegexpError) { ("a" * n + "b").rindex(/(?<!a)(?:#{atom})*b/) }   # was nil, CRuby 0
  # the whole range, once no window within 256 bytes of the end has answered
  assert_raise(RegexpError) { ("a" * limit + "b" + "c" * 300).rindex(/\A(?:(?>a))*b/) }  # was nil, CRuby 0
  # the walk from the window's match at position 1 to position 2, which has
  # the run the second alternative asks for and gives up on; the windows
  # before the one that reaches position 1 have less than that run
  n = limit / (2 * d) + 1
  assert_raise(RegexpError) { ("x" + "a" * (n + 1) + "c").rindex(/(?<=x)a|(?:#{atom}){#{n}}c/) }  # was 1, CRuby 2
  # Inside the limits the same three searches answer, as in CRuby.
  n = limit / (2 * (2 * d + 1))
  assert_equal 0, ("a" * n + "b").rindex(/(?<!a)(?:#{atom})*b/)
  assert_equal 0, ("a" * (limit / 4) + "b" + "c" * 300).rindex(/\A(?:(?>a))*b/)
  n = limit / (4 * d)
  assert_equal 2, ("x" + "a" * (n + 1) + "c").rindex(/(?<=x)a|(?:#{atom}){#{n}}c/)
end

assert("String#index and String#rindex with regexp set the match globals") do
  assert_equal 1, "abc".index(/(b)/)
  assert_equal "b", $1
  assert_equal "b", Regexp.last_match(0)

  # the match `rindex` stopped on, not one it walked past on the way there
  assert_equal 4, "abcabc".rindex(/(b)/)
  assert_equal 4, $~.begin(0)

  # a failed match clears $~, which is why both search through `match` rather
  # than `match?`
  "zzz" =~ /z/
  assert_nil "abc".index(/x/)
  assert_nil $~
  "zzz" =~ /z/
  assert_nil "abc".rindex(/x/)
  assert_nil $~

  # so does a position that lands outside the subject
  "zzz" =~ /z/
  assert_nil "abc".index(/b/, 10)
  assert_nil $~
  "zzz" =~ /z/
  assert_nil "abc".rindex(/b/, -10)
  assert_nil $~

  # and so does a match that starts past what the search asked for: `rindex`
  # walks past it and does not leave it behind
  "zzz" =~ /z/
  assert_nil "abc".rindex(/c/, 1)
  assert_nil $~
end

assert("String#index and String#rindex delegate every non-regexp argument") do
  assert_equal 2, "hello".index("l")
  assert_equal 3, "hello".index("l", 3)
  assert_nil "hello".index("z")
  assert_equal 3, "hello".rindex("l")
  assert_equal 2, "hello".rindex("l", 2)
  assert_nil "hello".rindex("z")

  # a delegated call still gets the C arity check and the C errors
  assert_raise(ArgumentError) { "hello".index }
  assert_raise(ArgumentError) { "hello".rindex }
  assert_raise(TypeError) { "hello".index(1) }
  assert_raise(TypeError) { "hello".rindex(1) }
  assert_raise(TypeError) { "hello".index("l", nil) }

  # and the regexp form takes the arguments the C form takes
  assert_raise(ArgumentError) { "hello".index(/l/, 1, 2) }
  assert_raise(ArgumentError) { "hello".rindex(/l/, 1, 2) }
  assert_raise(TypeError) { "hello".index(/l/, nil) }
  assert_raise(TypeError) { "hello".rindex(/l/, nil) }

  # `is_a?` is redefinable, so a Regexp denying its own type must still be
  # searched for, and an object claiming to be one must not be
  re = /l+/
  def re.is_a?(klass); false; end
  assert_equal 2, "hello".index(re)
  assert_equal 3, "hello".rindex(re)

  fake = Object.new
  def fake.is_a?(klass); true; end
  def fake.match(str, pos = 0); raise "must not be called"; end
  assert_raise(TypeError) { "hello".index(fake) }
  assert_raise(TypeError) { "hello".rindex(fake) }
end

assert("String#byteindex and String#byterindex with regexp") do
  assert_equal 1, "hello".byteindex(/e/)
  assert_nil "hello".byteindex(/z/)
  assert_equal 3, "hello".byteindex(/l/, 3)
  assert_equal 3, "hello".byteindex(/l/, -2)
  assert_nil "hello".byteindex(/l/, 6)
  assert_nil "hello".byteindex(/l/, -10)

  assert_equal 3, "hello".byterindex(/l/)
  assert_nil "hello".byterindex(/z/)
  assert_equal 1, "aaa".byterindex(/aa/)
  assert_equal 2, "hello".byterindex(/l/, 2)
  assert_equal 3, "hello".byterindex(/l/, 10)
  assert_nil "hello".byterindex(/l/, -10)

  # the same two searches read in the other space.  They part company with
  # `index` and `rindex` only on a build with MRB_UTF8_STRING, where the
  # answer and the position argument are both byte offsets
  if __ENCODING__ == "UTF-8"
    assert_equal 1, "あいうあいう".index(/い/)
    assert_equal 3, "あいうあいう".byteindex(/い/)
    assert_equal 4, "あいうあいう".rindex(/い/)
    assert_equal 12, "あいうあいう".byterindex(/い/)
    assert_equal 12, "あいうあいう".byteindex(/い/, 6)
    assert_equal 3, "あいうあいう".byterindex(/い/, 6)
  end
end

assert("String#byteindex and String#byterindex take an offset inside a character") do
  # A byte offset that lands inside a character names no position the string
  # has, and a byte search refuses one rather than search from the middle of a
  # character, whichever form of it the search was reached through.
  skip unless __ENCODING__ == "UTF-8"
  str = "あいうあいう"    # 6 characters, 18 bytes

  assert_raise(IndexError) { str.byteindex("い", 1) }
  assert_raise(IndexError) { str.byterindex("い", 1) }
  assert_raise(IndexError) { str.byteindex(/い/, 1) }
  assert_raise(IndexError) { str.byterindex(/い/, 1) }

  # a negative offset is read against the byte length first, so where it lands
  # is the question asked of it too
  assert_raise(IndexError) { "あ".byteindex("x", -1) }
  assert_raise(IndexError) { "あ".byterindex("x", -1) }
  assert_raise(IndexError) { "あ".byteindex(/x/, -1) }
  assert_raise(IndexError) { "あ".byterindex(/x/, -1) }

  # an offset on a boundary is a position either way, and the two forms agree
  assert_equal 3, str.byteindex("い", 3)
  assert_equal 3, str.byteindex(/い/, 3)
  assert_equal 3, str.byterindex("い", 3)
  assert_equal 3, str.byterindex(/い/, 3)

  # so is every offset of a string that indexes by byte, whatever its bytes
  # spell, and both forms take one
  bin = "あ".b
  assert_nil bin.byteindex("x", 1)
  assert_nil bin.byteindex(/x/, 1)
  assert_nil bin.byterindex("x", 1)
  assert_nil bin.byterindex(/x/, 1)

  # an offset before the string is a miss before it is a position, for either
  # form.  The pattern is one the subject holds, so that a nil is the offset
  # being answered rather than the search coming up empty on its own.
  assert_nil "あ".byteindex("あ", -9)
  assert_nil "あ".byteindex(/あ/, -9)
  assert_nil "あ".byterindex("あ", -9)
  assert_nil "あ".byterindex(/あ/, -9)

  # past the far end the two methods part company, as they do for a String:
  # `byteindex` misses, and `byterindex` reads the offset as the end it already
  # searches back from
  assert_nil "あ".byteindex("あ", 9)
  assert_nil "あ".byteindex(/あ/, 9)
  assert_equal 0, "あ".byterindex("あ", 9)
  assert_equal 0, "あ".byterindex(/あ/, 9)
end

assert("String#byteindex and String#byterindex with regexp set the match globals") do
  assert_equal 1, "abc".byteindex(/(b)/)
  assert_equal "b", $1
  assert_equal 4, "abcabc".byterindex(/(b)/)
  assert_equal 4, $~.begin(0)

  "zzz" =~ /z/
  assert_nil "abc".byteindex(/x/)
  assert_nil $~
  "zzz" =~ /z/
  assert_nil "abc".byterindex(/x/)
  assert_nil $~

  # a position outside the subject and a match that starts past the one asked
  # for both clear them too
  "zzz" =~ /z/
  assert_nil "abc".byteindex(/b/, 10)
  assert_nil $~
  "zzz" =~ /z/
  assert_nil "abc".byterindex(/c/, 1)
  assert_nil $~
end

assert("String#byteindex and String#byterindex delegate every non-regexp argument") do
  assert_equal 2, "hello".byteindex("l")
  assert_equal 3, "hello".byteindex("l", 3)
  assert_nil "hello".byteindex("z")
  assert_equal 3, "hello".byterindex("l")
  assert_equal 2, "hello".byterindex("l", 2)
  assert_nil "hello".byterindex("z")

  assert_raise(ArgumentError) { "hello".byteindex }
  assert_raise(ArgumentError) { "hello".byterindex }
  assert_raise(TypeError) { "hello".byteindex(1) }
  assert_raise(TypeError) { "hello".byterindex(1) }
  assert_raise(ArgumentError) { "hello".byteindex(/l/, 1, 2) }
  assert_raise(ArgumentError) { "hello".byterindex(/l/, 1, 2) }
  assert_raise(TypeError) { "hello".byteindex(/l/, nil) }
  assert_raise(TypeError) { "hello".byterindex(/l/, nil) }

  re = /l+/
  def re.is_a?(klass); false; end
  assert_equal 2, "hello".byteindex(re)
  assert_equal 3, "hello".byterindex(re)

  fake = Object.new
  def fake.is_a?(klass); true; end
  def fake.match(str, pos = 0); raise "must not be called"; end
  assert_raise(TypeError) { "hello".byteindex(fake) }
  assert_raise(TypeError) { "hello".byterindex(fake) }
end

assert("String#partition and String#rpartition with regexp") do
  assert_equal ["he", "ll", "o"], "hello".partition(/l+/)
  assert_equal ["hell", "o", ""], "hello".partition(/o/)
  assert_equal ["hel", "l", "o"], "hello".rpartition(/l/)
  assert_equal ["hello w", "o", "rld"], "hello world".rpartition(/o/)

  # the three pieces come from the match itself, so a capture group changes
  # nothing about where the subject is cut
  assert_equal ["he", "llo", ""], "hello".partition(/(l+)(o)/)

  # `rpartition` takes the last match, overlapping ones included
  assert_equal ["", "aa", "a"], "aaa".partition(/aa/)
  assert_equal ["a", "aa", ""], "aaa".rpartition(/aa/)

  # no match leaves the subject whole: at the head for `partition` and at the
  # tail for `rpartition`
  assert_equal ["hello", "", ""], "hello".partition(/z/)
  assert_equal ["", "", "hello"], "hello".rpartition(/z/)
  assert_equal ["", "", ""], "".partition(/z/)
  assert_equal ["", "", ""], "".rpartition(/z/)

  # an empty match still cuts, at the first position for one and at the last
  # for the other
  assert_equal ["", "", "hello"], "hello".partition(//)
  assert_equal ["hello", "", ""], "hello".rpartition(//)

  # the unmatched subject comes back as a copy, and every piece is a plain
  # String even for a subclass receiver, as in CRuby
  s = "hello"
  assert_false s.partition(/z/)[0].equal?(s)
  sub = Class.new(String)
  assert_equal [String, String, String], sub.new("hello").partition(/l/).map { |x| x.class }
  assert_equal [String, String, String], sub.new("hello").rpartition(/z/).map { |x| x.class }
end

assert("String#partition and String#rpartition with regexp set the match globals") do
  assert_equal ["a", "b", "cabc"], "abcabc".partition(/(b)/)
  assert_equal 1, $~.begin(0)
  assert_equal "b", $1

  # the match `rpartition` stopped on, not one it walked past on the way there
  assert_equal ["abca", "b", "c"], "abcabc".rpartition(/(b)/)
  assert_equal 4, $~.begin(0)

  "zzz" =~ /z/
  assert_equal ["abc", "", ""], "abc".partition(/x/)
  assert_nil $~
  "zzz" =~ /z/
  assert_equal ["", "", "abc"], "abc".rpartition(/x/)
  assert_nil $~
end

assert("a backward search publishes every global of the match it settled on") do
  # `$~` and `$1` are what the tests above read back. The rest of what a match
  # leaves behind is published by the same act and is asserted here, since the
  # walk these three share passes matches on the way to the one it answers
  # with and none of those may be what is left standing.
  assert_equal 4, "abcabc".rindex(/(b)(c)/)
  assert_equal "bc", $&
  assert_equal "abca", $`
  assert_equal "", $'
  assert_equal "b", $1
  assert_equal "c", $2
  assert_equal "c", $+
  assert_equal "abca", $~.pre_match
  assert_equal "", $~.post_match

  assert_equal 4, "abcabc".byterindex(/(b)(c)/)
  assert_equal "bc", $&
  assert_equal "abca", $`

  assert_equal ["abca", "bc", ""], "abcabc".rpartition(/(b)(c)/)
  assert_equal "bc", $&
  assert_equal "abca", $`
  assert_equal "c", $+

  # a group that did not take part leaves nil behind, and `$+` reaches past it
  assert_equal 1, "abc".rindex(/(b)(z)?/)
  assert_nil $2
  assert_equal "b", $+

  # and a search that finds nothing clears all of them, not `$~` alone.  Two
  # groups are seeded so that a `$2` left standing is one the clear missed
  # rather than one no seed ever filled
  "zzz" =~ /(z)(z)/
  assert_nil "abc".rindex(/x/)
  assert_nil $~
  assert_nil $&
  assert_nil $`
  assert_nil $'
  assert_nil $1
  assert_nil $2
  assert_nil $+
end

assert("String#partition and String#rpartition delegate every non-regexp argument") do
  assert_equal ["he", "ll", "o"], "hello".partition("ll")
  assert_equal ["hello", "", ""], "hello".partition("z")
  assert_equal ["hel", "l", "o"], "hello".rpartition("l")
  assert_equal ["", "", "hello"], "hello".rpartition("z")

  assert_raise(ArgumentError) { "hello".partition }
  assert_raise(ArgumentError) { "hello".rpartition }
  assert_raise(ArgumentError) { "hello".partition(/l/, 1) }
  assert_raise(ArgumentError) { "hello".rpartition(/l/, 1) }
  assert_raise(TypeError) { "hello".partition(1) }
  assert_raise(TypeError) { "hello".rpartition(1) }

  re = /l+/
  def re.is_a?(klass); false; end
  assert_equal ["he", "ll", "o"], "hello".partition(re)
  assert_equal ["hel", "l", "o"], "hello".rpartition(re)

  fake = Object.new
  def fake.is_a?(klass); true; end
  def fake.match(str, pos = 0); raise "must not be called"; end
  assert_raise(TypeError) { "hello".partition(fake) }
  assert_raise(TypeError) { "hello".rpartition(fake) }
end

assert("String#start_with? with regexp") do
  assert_true "hello".start_with?(/h/)
  assert_true "hello".start_with?(/hel+/)
  assert_true "hello".start_with?(//)
  assert_false "hello".start_with?(/z/)

  # anchored at the start rather than searched for, so a pattern that matches
  # further along is not an answer
  assert_false "hello".start_with?(/e/)
  assert_true "hello".start_with?(/^h/)
  assert_false "abc\ndef".start_with?(/^d/)

  # several patterns, read left to right, in any mix of the two kinds
  assert_true "hello".start_with?(/z/, /h/)
  assert_true "hello".start_with?("z", /h/)
  assert_true "hello".start_with?(/h/, "z")
  assert_false "hello".start_with?(/z/, "z")
  assert_false "hello".start_with?

  # an argument after the one that answers is never looked at
  assert_true "hello".start_with?(/h/, 1)

  # `end_with?` is not part of this family: CRuby rejects a Regexp there too
  assert_raise(TypeError) { "hello".end_with?(/o/) }
end

assert("String#start_with? with regexp sets the match globals") do
  assert_true "hello".start_with?(/(h)(e)/)
  assert_equal "h", $1
  assert_equal "e", $2

  # the pattern that answered, after the ones before it failed
  assert_true "hello".start_with?(/z/, /(h)/)
  assert_equal "h", $1

  "zzz" =~ /z/
  assert_false "hello".start_with?(/z/)
  assert_nil $~

  # a pattern that matches further along is refused, and its match is not
  # left behind either
  "zzz" =~ /z/
  assert_false "hello".start_with?(/e/)
  assert_nil $~

  # a non-regexp argument leaves them as they were
  "zzz" =~ /z/
  assert_true "hello".start_with?("he")
  assert_equal "z", $~[0]
  "zzz" =~ /z/
  assert_false "hello".start_with?
  assert_equal "z", $~[0]
end

assert("String#start_with? delegates every non-regexp argument") do
  assert_true "hello".start_with?("he")
  assert_false "hello".start_with?("z")
  assert_true "hello".start_with?("z", "he")
  assert_raise(TypeError) { "hello".start_with?(1) }

  re = /l+/
  def re.is_a?(klass); false; end
  assert_false "hello".start_with?(re)
  head = /h/
  def head.is_a?(klass); false; end
  assert_true "hello".start_with?(head)

  fake = Object.new
  def fake.is_a?(klass); true; end
  def fake.match(str, pos = 0); raise "must not be called"; end
  assert_raise(TypeError) { "hello".start_with?(fake) }
end

assert("String overrides search a pattern whose `match` was rewritten") do
  r = /l+/
  def r.match(*args); "PWNED"; end

  assert_equal "ll", "hello"[r]
  assert_equal "ll", "hello".slice(r)
  assert_equal "heLLo", "hello".sub(r) { |m| m.upcase }
  assert_equal "heLLo", "hello".dup.sub!(r) { |m| m.upcase }
  s = "hello".dup
  s[r] = "X"
  assert_equal "heXo", s
  assert_equal "ll", "hello".dup.slice!(r)
  assert_equal 2, "hello".index(r)
  assert_equal 3, "hello".rindex(r)
  assert_equal 2, "hello".byteindex(r)
  assert_equal 3, "hello".byterindex(r)
  assert_equal ["he", "ll", "o"], "hello".partition(r)
  assert_equal ["hel", "l", "o"], "hello".rpartition(r)
  assert_true "llama".start_with?(r)

  # The quiet half of the same surface: a rewritten `match` answering nil
  # used to decide the bang forms' return value, so they answered nil and
  # left the receiver untouched.
  quiet = /l+/
  def quiet.match(*args); nil; end
  s = "hello".dup
  assert_equal "heXo", s.sub!(quiet, "X")
  assert_equal "heXo", s
  s = "hello".dup
  assert_equal "heXo", s.gsub!(quiet, "X")
  assert_equal "heXo", s
end

assert("String overrides search a pattern whose `__byte_match` was rewritten") do
  r = /l+/
  def r.__byte_match(*args); "PWNED"; end

  assert_equal "heLLo", "hello".gsub(r) { |m| m.upcase }
  assert_equal ["he", "o"], "hello".split(r)
  assert_equal 2, "hello".byteindex(r)
end

assert("String#match? and String#=~ search a real pattern without asking it") do
  r = /l+/
  def r.match?(*args); "PWNED"; end
  def r.=~(*args); 99; end

  assert_true "hello".match?(r)
  assert_equal 2, "hello" =~ r

  # The forward for everything that is not a Regexp stays: CRuby sends `=~`
  # to the argument there too.
  o = Object.new
  def o.=~(str); 42; end
  assert_equal 42, "hello" =~ o
end

assert("String overrides run a pattern whose operation cores were rewritten") do
  r = /l+/
  def r.__sub_str(*args); "PWNED"; end
  def r.__gsub_str(*args); "PWNED"; end
  def r.__scan(*args); "PWNED"; end

  assert_equal "heXo", "hello".sub(r, "X")
  assert_equal "heXo", "hello".gsub(r, "X")
  assert_equal ["ll"], "hello".scan(r)
  s = "hello".dup
  assert_equal "heXo", s.sub!(r, "X")
  assert_equal "heXo", s
  s = "hello".dup
  assert_equal "heXo", s.gsub!(r, "X")
  assert_equal "heXo", s
end

assert("String overrides run a String pattern whose operation cores were rewritten") do
  # The literal path reaches the same class methods on Regexp, so a pattern
  # carrying its own `__sub_lit` or `__gsub_lit` is searched for, not asked.
  p = "l".dup
  def p.__sub_lit(*args); "PWNED"; end
  def p.__gsub_lit(*args); "PWNED"; end
  def p.__gsub_block(*args); "PWNED"; end

  assert_equal "heXlo", "hello".sub(p, "X")
  assert_equal "heXXo", "hello".gsub(p, "X")
  assert_equal "heXXo", "hello".gsub(p) { "X" }
  s = "hello".dup
  assert_equal "heXlo", s.sub!(p, "X")
  s = "hello".dup
  assert_equal "heXXo", s.gsub!(p, "X")
end

assert("String#[] answers the same through the opcode and through a send") do
  # `s[x]` is answered by `OP_GETIDX` / `OP_GETIDX0` in C, without a method
  # lookup, while `slice` is a second method table entry for the same
  # implementation that no opcode ever answers.  The two are only allowed to be
  # different code while they cannot be told apart, which is the promise
  # `mrb_idx_op_rearm()` takes from this gem: `str_aref()` takes the `[]` name
  # to reach a Regexp index, and re-arms the opcodes because it hands every
  # other argument form straight back to the same `mrb_str_aref()` the opcode
  # calls.  Widening it to another argument type the opcode answers would not
  # show up in `s[x]` at all, so ask both ways.
  s = "hello world"
  [0, 1, 5, -1, -11, 10, 11, 99, -99].each do |i|
    assert_equal s.slice(i), s[i], "s[#{i}]"
  end
  ["h", "lo w", "hello world", "", "zz", "d"].each do |sub|
    assert_equal s.slice(sub), s[sub], "s[#{sub.inspect}]"
  end
  [0..3, 1...3, -3..-1, 0..-1, 5..99, 99..100, 3..1, -99..2].each do |r|
    assert_equal s.slice(r), s[r], "s[#{r.inspect}]"
  end
  assert_equal "".slice(0), ""[0]
  assert_equal "".slice(0..1), ""[0..1]
  # A receiver whose characters are not one byte each.
  if "あ".length == 1
    u = "こんにちは"
    [0, 2, 4, -1, 5].each { |i| assert_equal u.slice(i), u[i], "u[#{i}]" }
    [0..2, 1...4, -2..-1].each { |r| assert_equal u.slice(r), u[r], "u[#{r.inspect}]" }
  end
end

assert("String#[]= answers the same through the opcode and through a send") do
  # `s[x] = repl` is answered by `OP_SETIDX` in C, without a method lookup.
  # The two are only allowed to be different code while they cannot be told
  # apart, which is the promise `mrb_idx_op_rearm()` takes from this gem for
  # `[]=` as it does for `[]`: `str_aset()` takes the name to reach a Regexp
  # index, and re-arms the opcode because it hands every other argument form to
  # the same `mrb_str_aset()` the opcode calls.  Widening it to another
  # argument type the opcode answers would not show up in `s[x] = repl` at all,
  # so ask both ways, and compare what each store left behind rather than the
  # replacement both forms answer with.
  #
  # The send side is a String subclass receiver: writing the call out as
  # `b.[]=(x, repl)` compiles to `OP_SETIDX` too, so it would ask the opcode
  # twice and could never see the promise broken, and `send` comes from
  # mruby-metaprog, which the core test build does not have.  A subclass fails
  # the opcode's class test for an unrelated reason and reaches the method,
  # which it inherits unchanged, so the two must agree.
  sub = Class.new(String)
  [0, 1, 5, -1, -11, 10].each do |i|
    a = "hello world"
    b = sub.new("hello world")
    a[i] = "X"
    b[i] = "X"
    assert_equal a, b.to_s, "s[#{i}] = 'X'"
  end
  ["h", "lo w", "hello world", "", "d"].each do |s|
    a = "hello world"
    b = sub.new("hello world")
    a[s] = "X"
    b[s] = "X"
    assert_equal a, b.to_s, "s[#{s.inspect}] = 'X'"
  end
  [0..3, 1...3, -3..-1, 0..-1, 5..99, 3..1].each do |r|
    a = "hello world"
    b = sub.new("hello world")
    a[r] = "X"
    b[r] = "X"
    assert_equal a, b.to_s, "s[#{r.inspect}] = 'X'"
  end
  # The form neither answers from the opcode: a Regexp index reaches
  # `str_aset()` through the send the opcode falls back to.
  a = "hello world"
  b = sub.new("hello world")
  a[/o.w/] = "X"
  b[/o.w/] = "X"
  assert_equal a, b.to_s
  # An index that matches nothing raises the same error either way.
  assert_raise(IndexError) { "hello world"[99] = "X" }
  assert_raise(IndexError) { sub.new("hello world")[99] = "X" }
  assert_raise(IndexError) { "hello world"["zz"] = "X" }
  assert_raise(IndexError) { sub.new("hello world")["zz"] = "X" }
  # A receiver whose characters are not one byte each.
  if "あ".length == 1
    a = "こんにちは"
    b = sub.new("こんにちは")
    a[1] = "X"
    b[1] = "X"
    assert_equal a, b.to_s
    a = "こんにちは"
    b = sub.new("こんにちは")
    a[1..3] = "X"
    b[1..3] = "X"
    assert_equal a, b.to_s
  end
end
