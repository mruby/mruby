assert("String#scan return shape") do
  # No capture group: an array of the matched strings.
  assert_equal ["a", "b", "c"], "abc".scan(/\w/)
  # Any capture group: an array per match holding that match's captures, so a
  # single group still yields one-element arrays (not bare strings).
  assert_equal [["x"], ["x"]], "xyxy".scan(/(x|xy)+/)
  assert_equal [["a", "1"], ["b", "2"]], "a1b2".scan(/(\w)(\d)/)
  assert_equal [["cat"], ["dog"]], "cats dogs".scan(/(cat|dog)s?/)
  # A group that did not participate is nil inside the per-match array.
  assert_equal [[nil]], "".scan(/(a|ab)*/)
  collected = []
  "foo".scan(/(o)/) { |m| collected << m }
  assert_equal [["o"], ["o"]], collected
end

assert("String#scan hands its block the globals of the match it is given") do
  # The block form used to collect every match before yielding any, so the
  # globals stood at the last match for every call of the block and $1 was the
  # same string throughout.
  seen = []
  "a1b2c3".scan(/(\d)/) { seen << [$~.begin(0), $&, $`, $', $1] }
  assert_equal [[1, "1", "a", "b2c3", "1"],
                [3, "2", "a1b", "c3", "2"],
                [5, "3", "a1b2c", "", "3"]], seen
  # a pattern with no group publishes the same way
  seen = []
  "aXbXc".scan(/X/) { seen << [$~.begin(0), $`, $'] }
  assert_equal [[1, "a", "bXc"], [3, "aXb", "c"]], seen
  # and the last match stays published after the call, as it does without a
  # block, while a scan that matched nothing leaves the globals cleared
  "a1b2".scan(/(\d)/) { }
  assert_equal 3, $~.begin(0)
  assert_equal "2", $1
  "a1b2".scan(/(z)/) { }
  assert_nil $~
  assert_nil $1
  # the block form answers the receiver, the bare form the matches
  assert_equal "abc", "abc".scan(/\w/) { }
end

assert("String#scan of a multibyte subject reports byte-correct globals") do
  skip unless __ENCODING__ == "UTF-8"
  seen = []
  "あXいXう".scan(/X/) { seen << [$~.begin(0), $`, $'] }
  assert_equal [[1, "あ", "いXう"], [3, "あXい", "う"]], seen
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

assert("String#split - a byte-indexed subject is split by byte") do
  skip unless __ENCODING__ == "UTF-8"
  # An empty match steps to the next position, and `String#b` makes every byte
  # one. The step read the subject as UTF-8 and cleared a whole character,
  # so a four-byte string came back in one piece.
  s = "\u{1F600}".b   # F0 9F 98 80: four bytes, one character
  assert_equal ["\xF0".b, "\x9F".b, "\x98".b, "\x80".b], s.split(//)
  assert_equal 4, s.split(//).size
  assert_equal ["a".b, "\xC3".b, "\xA9".b, "b".b], "a\u{E9}b".b.split(//)
  # a subject read as UTF-8 is still split by character
  assert_equal ["\u{1F600}"], "\u{1F600}".split(//)
  # and the limit still counts fields, not bytes
  assert_equal ["\xF0".b, "\x9F\x98\x80".b], s.split(//, 2)
end

assert("String#match") do
  md = "hello world".match(Regexp.new("(\\w+)\\s(\\w+)"))
  assert_equal "hello", md[1]
  assert_equal "world", md[2]
end

assert("String#match - block") do
  assert_equal "L", "hello".match(Regexp.new("l")) { |md| md[0].upcase }
  assert_equal "ll", "hello".match("l+") { |md| md[0] }

  called = false
  assert_nil("hello".match(Regexp.new("z")) { called = true })
  assert_false called

  called = false
  assert_nil("hello".match(Regexp.new("l"), 4) { called = true })
  assert_false called

  called = false
  result = "hello".match("l+") do
    called = true
    nil
  end
  assert_nil result
  assert_true called
end

assert("String#match - break out of the block") do
  assert_equal :broke, "hello".match("l+") { break :broke }
end

assert("String#match - block sees the match globals") do
  assert_equal "ll", "hello".match("l+") { $~[0] }
  assert_equal "ll", "hello".match("l+") { Regexp.last_match(0) }
end

assert("String#=~") do
  assert_equal 1, "abc" =~ Regexp.new("b")
  assert_nil "abc" =~ Regexp.new("z")
end

assert("String#=~ with a String argument raises TypeError") do
  assert_raise(TypeError) { "abc" =~ "b" }
  assert_raise(TypeError) { "abc" !~ "b" }
end

assert("String#=~ dispatches to the argument") do
  # A non-Regexp, non-String argument is answered by its own `=~`, so `nil`
  # gets a value from `NilClass#=~` and everything else without one raises.
  assert_nil "abc" =~ nil
  assert_true "abc" !~ nil
  assert_raise(NoMethodError) { "abc" =~ 1 }
  assert_raise(NoMethodError) { "abc" =~ Object.new }
end

class StringMatchIsALiar
  def is_a?(klass)
    true
  end
end

class StringMatchClassLiar
  def class
    Regexp
  end
end

class StringMatchToStr
  def to_str
    "b"
  end
end

class StringMatchRegexp < Regexp
end

class StringMatchString < String
end

class StringMatchStringDenier < String
  def is_a?(klass)
    false
  end

  def nil?
    true
  end
end

class StringMatchHelperOverride < String
  private def __check_pattern(re)
    Regexp.new(re.to_s)
  end
end

assert("String#match / #match? with a non-Regexp argument raise TypeError") do
  # nil, true and false are named by value, everything else by class.
  assert_raise_with_message(TypeError, "wrong argument type nil (expected Regexp)") do
    "abc".match(nil)
  end
  assert_raise_with_message(TypeError, "wrong argument type true (expected Regexp)") do
    "abc".match(true)
  end
  assert_raise_with_message(TypeError, "wrong argument type false (expected Regexp)") do
    "abc".match(false)
  end
  assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Regexp)") do
    "abc".match(:b)
  end
  assert_raise_with_message(TypeError, "wrong argument type Integer (expected Regexp)") do
    "abc".match(1)
  end
  assert_raise_with_message(TypeError, "wrong argument type Array (expected Regexp)") do
    "abc".match([])
  end

  assert_raise_with_message(TypeError, "wrong argument type nil (expected Regexp)") do
    "abc".match?(nil)
  end
  assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Regexp)") do
    "abc".match?(:b)
  end

  # An argument claiming to be a Regexp through `is_a?` is still rejected.
  liar = StringMatchIsALiar.new
  assert_raise_with_message(TypeError, "wrong argument type StringMatchIsALiar (expected Regexp)") do
    "abc".match(liar)
  end
  assert_raise_with_message(TypeError, "wrong argument type StringMatchIsALiar (expected Regexp)") do
    "abc".match?(liar)
  end

  # The type name in the message comes from the real class, so an argument
  # redefining `class` cannot make the message name something else.
  class_liar = StringMatchClassLiar.new
  assert_raise_with_message(TypeError, "wrong argument type StringMatchClassLiar (expected Regexp)") do
    "abc".match(class_liar)
  end
  assert_raise_with_message(TypeError, "wrong argument type StringMatchClassLiar (expected Regexp)") do
    "abc".match?(class_liar)
  end

  # CRuby converts an argument responding to `to_str` and matches with it.
  # mruby has no implicit String conversion in core, so the gem names such an
  # argument by class like any other, and this row stays an intentional
  # difference rather than a gap to close.
  to_str = StringMatchToStr.new
  assert_raise_with_message(TypeError, "wrong argument type StringMatchToStr (expected Regexp)") do
    "abc".match(to_str)
  end
  assert_raise_with_message(TypeError, "wrong argument type StringMatchToStr (expected Regexp)") do
    "abc".match?(to_str)
  end

  # The pattern is rejected before pos is looked at.
  assert_raise_with_message(TypeError, "wrong argument type nil (expected Regexp)") do
    "abc".match(nil, Object.new)
  end
  assert_raise_with_message(TypeError, "wrong argument type nil (expected Regexp)") do
    "abc".match?(nil, Object.new)
  end

  # The check lives on Regexp and no helper for it is defined on String, so a
  # subclass defining a method by such a name cannot widen what `match` and
  # `match?` accept.
  assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Regexp)") do
    StringMatchHelperOverride.new("abc").match(:abc)
  end
  assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Regexp)") do
    StringMatchHelperOverride.new("abc").match?(:abc)
  end

  # The accepted types still work.
  assert_equal "b", "abc".match(Regexp.new("b"))[0]
  assert_equal "b", "abc".match("b")[0]
  assert_true "abc".match?("b")

  # The check is by kind, so subclasses are accepted too.
  assert_equal "b", "abc".match(StringMatchRegexp.new("b"))[0]
  assert_true "abc".match?(StringMatchRegexp.new("b"))
  assert_equal "b", "abc".match(StringMatchString.new("b"))[0]
  assert_true "abc".match?(StringMatchString.new("b"))
end

assert("String#sub") do
  assert_equal "hXllo", "hello".sub(Regexp.new("e"), "X")
end

assert("String#gsub") do
  assert_equal "h-ll-", "hello".gsub(Regexp.new("[eo]"), "-")
end

assert("String#sub/#gsub - replacement string takes precedence over the block") do
  assert_equal "aXc", "abc".sub(/b/, "X") { "Y" }
  assert_equal "aXcX", "abcb".gsub(/b/, "X") { "Y" }
  # The block is only used when no replacement argument is given.
  assert_equal "aYc", "abc".sub(/b/) { "Y" }
  assert_equal "aYcY", "abcb".gsub(/b/) { "Y" }
end

assert("String#sub - wrong number of arguments") do
  # Without a block CRuby demands exactly 2 arguments, and says so.
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 0, expected 2)") do
    "abc".sub
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 1, expected 2)") do
    "abc".sub(/b/)
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 3, expected 2)") do
    "abc".sub(/b/, "X", "Y")
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 3, expected 1..2)") do
    "abc".sub(/b/, "X", "Y") { "Z" }
  end
end

assert("String#gsub - wrong number of arguments") do
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 0, expected 1..2)") do
    "abc".gsub
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 3, expected 1..2)") do
    "abc".gsub(/b/, "X", "Y")
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 3, expected 1..2)") do
    "abc".gsub(/b/, "X", "Y") { "Z" }
  end
end

assert("String#gsub without a block returns an enumerator") do
  skip "Enumerator is not available" unless Object.const_defined?(:Enumerator)
  assert_equal ["b", "b"], "abcb".gsub(/b/).to_a
  assert_equal ["b", "b"], "abcb".gsub("b").to_a
  # Iterating the enumerator with a block performs the substitution.
  assert_equal "aBcB", "abcb".gsub(/b/).each { |m| m.upcase }
end

assert("String#sub! / #gsub! with a Regexp pattern") do
  # The core definitions decide whether a substitution happened with
  # `String#index`, which only takes a String, so every form below used to
  # raise TypeError once the pattern reached it.
  s = "hello world"
  assert_equal "hell0 world", s.sub!(/o/, "0")
  assert_equal "hell0 world", s
  s = "hello world"
  assert_equal "hell0 w0rld", s.gsub!(/o/, "0")
  assert_equal "hell0 w0rld", s

  s = "hello"
  assert_equal "heLlo", s.sub!(/l/) { |m| m.upcase }
  s = "hello"
  assert_equal "heLLo", s.gsub!(/l/) { |m| m.upcase }

  s = "John Smith"
  assert_equal "Smith John", s.sub!(/(\w+) (\w+)/, '\2 \1')
  s = "a1b2"
  assert_equal "1a2b", s.gsub!(/([a-z])(\d)/, '\2\1')

  # The receiver itself comes back, not a copy of it.
  s = "abc"
  assert_same s, s.sub!(/b/, "X")
  s = "abcb"
  assert_same s, s.gsub!(/b/, "X")

  # A replacement argument wins over the block, as in `sub`/`gsub`.
  assert_equal "aYc", "abc".sub!(/b/, "Y") { "X" }
  assert_equal "aYcY", "abcb".gsub!(/b/, "Y") { "X" }
end

assert("String#sub! / #gsub! return nil only when nothing matched") do
  s = "abc"
  assert_nil s.sub!(/z/, "X")
  assert_nil s.gsub!(/z/, "X")
  assert_nil s.sub!("z", "X")
  assert_nil s.gsub!("z", "X")
  assert_equal "abc", s
  # The block is not called for a pattern that does not match.
  assert_nil "abc".sub!(/z/) { flunk "block called" }

  # A match is a match even where the replacement leaves the string as it was,
  # so the answer cannot come from comparing the result with the receiver.
  s = "aaa"
  assert_same s, s.sub!(/a/, "a")
  assert_same s, s.gsub!(/a/, "a")
  assert_equal "aaa", s
end

assert("String#sub! / #gsub! quote a String pattern") do
  # A String is a literal here, as it is for `sub`/`gsub`: `.` matches only `.`.
  s = "a.c.e"
  assert_equal "aXc.e", s.sub!(".", "X")
  s = "a.c.e"
  assert_equal "aXcXe", s.gsub!(".", "X")

  # Anything that is neither a Regexp nor a String is rejected, rather than
  # reaching a match with the operands reversed.
  [["nil", nil], ["true", true], ["Symbol", :b], ["Integer", 1]].each do |name, pat|
    message = "wrong argument type #{name} (expected Regexp)"
    assert_raise_with_message(TypeError, message) { "abc".sub!(pat, "X") }
    assert_raise_with_message(TypeError, message) { "abc".gsub!(pat, "X") }
    assert_raise_with_message(TypeError, message) { "abc".sub!(pat) { "X" } }
    assert_raise_with_message(TypeError, message) { "abc".gsub!(pat) { "X" } }
  end
end

assert("String#sub! / #gsub! - wrong number of arguments") do
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 0, expected 2)") do
    "abc".sub!
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 1, expected 2)") do
    "abc".sub!(/b/)
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 3, expected 2)") do
    "abc".sub!(/b/, "X", "Y")
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 3, expected 1..2)") do
    "abc".sub!(/b/, "X", "Y") { "Z" }
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 0, expected 1..2)") do
    "abc".gsub!
  end
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 3, expected 1..2)") do
    "abc".gsub!(/b/, "X", "Y")
  end
end

assert("String#gsub! without a block returns an enumerator") do
  skip "Enumerator is not available" unless Object.const_defined?(:Enumerator)
  assert_equal Enumerator, "abc".gsub!(/a/).class
  # Iterating it performs the substitution on the original receiver.
  s = "abcb"
  assert_equal "aBcB", s.gsub!(/b/).each { |m| m.upcase }
  assert_equal "aBcB", s
  # As with `gsub`, the pattern is examined on the first iteration, not at the
  # call.
  enum = "abc".gsub!(:b)
  assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Regexp)") do
    enum.each { "X" }
  end
end

assert("String#sub! / #gsub! on a frozen string") do
  message = "can't modify frozen String"
  assert_raise_with_message(FrozenError, message) { "abc".freeze.sub!(/a/, "X") }
  assert_raise_with_message(FrozenError, message) { "abc".freeze.gsub!(/a/, "X") }
  # Before the no-match check: the receiver is rejected whether or not a
  # substitution would have taken place.
  assert_raise_with_message(FrozenError, message) { "abc".freeze.sub!(/z/, "X") }
  assert_raise_with_message(FrozenError, message) { "abc".freeze.gsub!(/z/, "X") }
  # And before the enumerator, which CRuby does not hand back here either.
  assert_raise_with_message(FrozenError, message) { "abc".freeze.gsub!(/a/) }

  # `sub!` reads its arguments first, though, as CRuby does.
  assert_raise_with_message(ArgumentError, "wrong number of arguments (given 1, expected 2)") do
    "abc".freeze.sub!(/z/)
  end
  assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Regexp)") do
    "abc".freeze.sub!(:z, "X")
  end
end

assert("String#sub! / #gsub! leave the match behind") do
  $~ = nil
  s = "hello world"
  s.sub!(/o/, "0")
  assert_equal "o", $~[0]
  # The subject is the string as it was matched, not the replaced one: a
  # MatchData snapshots it, so overwriting the receiver afterwards is safe.
  assert_equal "hello world", $~.string
  assert_equal "hell", $~.pre_match
  assert_equal " world", $~.post_match

  # `gsub!` leaves the last match, as `gsub` does.
  $~ = nil
  s = "hello world"
  s.gsub!(/o/, "0")
  assert_equal "hello world", $~.string
  assert_equal "hello w", $~.pre_match

  $~ = nil
  s = "hello"
  s.gsub!(/l/) { "X" }
  assert_equal "hello", $~.string
  assert_equal "hel", $~.pre_match

  # Matching nothing clears it, which is why the check is `match` and not
  # `match?`.
  /b(c)/ =~ "abcd"
  assert_nil "hello".sub!(/z/, "X")
  assert_nil $~
  /b(c)/ =~ "abcd"
  assert_nil "hello".gsub!(/z/, "X")
  assert_nil $~
end

assert("String#sub / #gsub / #scan / #split with a non-Regexp pattern raise TypeError") do
  # The same check `match` uses, so the naming matches: nil, true and false by
  # value, everything else by class.
  [
    ["nil", nil], ["true", true], ["false", false],
    ["Symbol", :b], ["Integer", 1], ["Array", []],
  ].each do |name, pat|
    message = "wrong argument type #{name} (expected Regexp)"
    assert_raise_with_message(TypeError, message) { "abc".sub(pat, "X") }
    assert_raise_with_message(TypeError, message) { "abc".gsub(pat, "X") }
    assert_raise_with_message(TypeError, message) { "abc".sub(pat) { "X" } }
    assert_raise_with_message(TypeError, message) { "abc".gsub(pat) { "X" } }
    assert_raise_with_message(TypeError, message) { "abc".scan(pat) }
    # split delegates nil to the core implementation instead of raising.
    assert_raise_with_message(TypeError, message) { "abc".split(pat) } unless pat.nil?
  end

  # An argument claiming to be a Regexp through `is_a?` or `class` is still
  # rejected, and still named by its real class.  `split` routes nil and String
  # patterns to the core implementation, so it has to reach the same check
  # without asking the argument what it is.
  liar = StringMatchIsALiar.new
  assert_raise_with_message(TypeError, "wrong argument type StringMatchIsALiar (expected Regexp)") do
    "abc".sub(liar, "X")
  end
  assert_raise_with_message(TypeError, "wrong argument type StringMatchIsALiar (expected Regexp)") do
    "abc".split(liar)
  end
  class_liar = StringMatchClassLiar.new
  assert_raise_with_message(TypeError, "wrong argument type StringMatchClassLiar (expected Regexp)") do
    "abc".gsub(class_liar, "X")
  end
  assert_raise_with_message(TypeError, "wrong argument type StringMatchClassLiar (expected Regexp)") do
    "abc".split(class_liar)
  end

  # A Symbol answers `match`, which the block form of `sub` used to reach: it
  # matched with the operands reversed and returned a string built from the
  # symbol's name instead of raising.
  assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Regexp)") do
    "ab".sub(:xaby) { "Z" }
  end
end

assert("String#sub / #gsub / #scan / #split accept a Regexp subclass, and quote a String") do
  assert_equal "aXc", "abc".sub(StringMatchRegexp.new("b"), "X")
  assert_equal "aXcX", "abcb".gsub(StringMatchRegexp.new("b"), "X")
  assert_equal ["b", "b"], "abcb".scan(StringMatchRegexp.new("b"))
  assert_equal ["a", "c"], "abc".split(StringMatchRegexp.new("b"))

  # A String pattern is a literal here, not a pattern: `.` matches only `.`.
  assert_equal "aXc", "a.c".sub(".", "X")
  assert_equal "aXc", "a.c".gsub(".", "X")
  assert_equal ["."], "a.c".scan(".")
  assert_equal "a[.]c", "a.c".gsub(".") { |m| "[#{m}]" }
  # A String subclass is accepted on the same terms.
  assert_equal "aXc", "a.c".sub(StringMatchString.new("."), "X")
  # Even one denying that it is a String, or claiming to be nil: `split` reads
  # the real type before choosing between the core implementation and the
  # regexp path.
  assert_equal ["a", "c"], "a.c".split(StringMatchStringDenier.new("."))
end

assert("String#=~ reads the real type of a String argument") do
  # `=~` dispatches everything but a String to the argument, so a String
  # subclass denying its own type used to pass the guard and dispatch back
  # here, recursing until the stack ran out instead of raising.
  denier = StringMatchStringDenier.new("b")
  assert_raise_with_message(TypeError, "type mismatch: String given") do
    "abc" =~ denier
  end
  assert_raise_with_message(TypeError, "type mismatch: String given") do
    denier =~ denier
  end
end

assert("String#gsub / #split examine the pattern only where CRuby does") do
  # gsub without a block builds the enumerator first, so the TypeError is
  # raised on the first iteration rather than at the call.
  if Object.const_defined?(:Enumerator)
    enum = "abc".gsub(:b)
    assert_raise_with_message(TypeError, "wrong argument type Symbol (expected Regexp)") do
      enum.to_a
    end
  end

  # split returns before looking at the pattern when the limit is 1.
  assert_equal ["abc"], "abc".split(true, 1)
  assert_equal [], "".split(:b, 1)
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
  bin = "\xC3\xA9x".b
  assert_equal [45, 195, 45, 169, 45, 120, 45], bin.gsub(//, "-").bytes
  assert_equal [45, 195, 45, 169, 45, 120, 45], bin.gsub(//) { "-" }.bytes
  assert_equal [45, 195, 45, 169, 45, 120], bin.gsub(/(?=.)/, "-").bytes
  assert_equal [45, 195, 45, 169, 45, 120], bin.gsub(/(?=.)/) { "-" }.bytes
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
  assert_equal ["a,b"], "a,b".split(/,/, 1.5) if Object.const_defined?(:Float)

  # mruby has no implicit conversion protocol, so an object defining `to_int`
  # is rejected here exactly as `Array.new(obj)` and `ary[obj]` reject it. The
  # limit is never asked what it responds to, so an object overriding
  # `respond_to?` reaches the same TypeError rather than a NoMethodError.
  limit = Object.new
  def limit.to_int; 2; end
  assert_raise(TypeError) { "a,b".split(/,/, limit) }

  limit = Object.new
  def limit.respond_to?(name, include_all = false); true; end
  assert_raise(TypeError) { "a,b".split(/,/, limit) }
end

class StringSplitLimitIsALiar
  def is_a?(klass)
    true
  end
end

class StringSplitLimitComparable
  def is_a?(klass)
    true
  end

  def ==(other)
    false
  end

  def >(other)
    true
  end

  def -(other)
    1
  end
end

assert("String#split limit cannot pose as an Integer") do
  # `is_a?` is redefinable, so a limit claiming to be an Integer used to skip
  # the conversion and reach the split loop as itself.
  assert_raise(TypeError) { "a,b,c".split(/,/, StringSplitLimitIsALiar.new) }
  # The String pattern delegates to __split, which converts the limit again in
  # C, so this one held before the fix too. Asserted so that the two halves of
  # the method stay pinned to the same answer.
  assert_raise(TypeError) { "a,b,c".split(",", StringSplitLimitIsALiar.new) }

  # Answering the operators the loop uses used to produce a wrong result
  # instead of an error.
  assert_raise(TypeError) { "a,b,c".split(/,/, StringSplitLimitComparable.new) }
end

assert("String#split with a Bigint limit") do
  # A Bigint is an Integer, so a check on the class let one through unconverted
  # and the split loop ran with a limit that does not fit `mrb_int`, while the
  # String pattern raised in `__split`. The exponent is a variable because a
  # constant power out of `mrb_int` range fails the build rather than raising.
  exp = 70
  begin
    limit = 2 ** exp
  rescue RangeError
    skip "requires mruby-bigint"
  end
  assert_raise(RangeError) { "a,b,c".split(/,/, limit) }
  assert_raise(RangeError) { "a,b,c".split(",", limit) }
  assert_raise(RangeError) { "a,b,c".split(/,/, -limit) }
end

assert("String#split with empty regexp") do
  assert_equal ["a", "b", "c"], "abc".split(//)
  assert_equal ["a", "bc"], "abc".split(//, 2)
  assert_equal ["a", "b", "c"], "abc".split(//, 3)
  assert_equal ["a", "b", "c", ""], "abc".split(//, 4)
  assert_equal ["a", "b", "c", ""], "abc".split(//, -1)
  assert_equal [], "".split(//, -1)
  assert_equal ["a", ""], "a".split(//, -1)
  # An empty pattern splits at every position the subject has, which is one
  # per character where the build reads characters and one per byte where it
  # does not.
  if __ENCODING__ == "UTF-8"
    assert_equal ["あ", "い"], "あい".split(//)
    assert_equal ["あ", "い"], "あい".split(//, 2)
    assert_equal ["あ", "い", ""], "あい".split(//, -1)
  else
    bytes = ["\xE3", "\x81", "\x82", "\xE3", "\x81", "\x84"]
    assert_equal bytes, "あい".split(//)
    assert_equal ["\xE3", "\x81\x82\xE3\x81\x84"], "あい".split(//, 2)
    assert_equal bytes + [""], "あい".split(//, -1)
  end
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

assert("String#gsub with block leaves the last match behind") do
  # The block form drives the search from mrblib, and the failed match that
  # ends the loop used to clear the match the loop was supposed to leave.
  $~ = nil
  "hello".gsub(/l/) { |m| m }
  assert_equal "l", $~[0]

  # every name a match publishes, not just $~
  $~ = nil
  "a1b22c".gsub(/([a-c])(\d+)/) { |m| m }
  assert_equal "b22", $~[0]
  assert_equal "b22", $&
  assert_equal "a1", $`
  assert_equal "c", $'
  assert_equal "b", $1
  assert_equal "22", $2
  assert_equal "22", $+

  # a block that matches on its own does not get the last word
  $~ = nil
  "hello".gsub(/l/) { |m| /z+/ =~ "zzz"; m }
  assert_equal "l", $~[0]

  # a zero-width match at the end of the subject ends the loop on the
  # `pos <= len` test rather than a failed match, and lands the same way
  $~ = nil
  "ab".gsub(/x*/) { "-" }
  assert_equal "", $~[0]
  assert_equal 2, $~.begin(0)

  # matching nothing clears, as it does everywhere else
  /b(c)/ =~ "abcd"
  "hello".gsub(/z/) { |m| m }
  assert_nil $~
  assert_nil $&
  assert_nil $`
  assert_nil $'
  assert_nil $1
  assert_nil $+
end
