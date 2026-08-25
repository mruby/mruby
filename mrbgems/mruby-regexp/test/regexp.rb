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

class RegexpInitializedTwice < Regexp
  def initialize(first, second)
    super(first)
    super(second)
  end
end

assert("Regexp#initialize - second call") do
  assert_raise(TypeError) { RegexpInitializedTwice.new("abc", "xyz") }
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

assert("Regexp#match - break out of the block") do
  assert_equal :broke, /l+/.match("hello") { break :broke }
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
  assert_equal __ENCODING__ == "UTF-8" ? 1 : 3, /い/ =~ "あい"
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

assert("Regexp#match - Symbol argument") do
  md = /a(b)/.match(:xaby)
  assert_kind_of MatchData, md
  assert_equal "ab", md[0]
  assert_equal "b", md[1]
  assert_equal "xaby", md.string
  assert_equal "x", md.pre_match
  assert_equal "ab", $~[0]
  assert_equal "b", /(?<x>b)/.match(:ab)[:x]
  assert_equal "A", (/a/.match(:ab) { |m| m[0].upcase })
  assert_nil /z/.match(:ab)
end

assert("Regexp#match - Symbol argument with pos") do
  assert_equal 3, /a/.match(:abxay, 1).begin(0)
  assert_nil /a/.match(:ab, 2)
end

assert("Regexp#match - multibyte Symbol argument") do
  # a multibyte name never fits the inline symbol representation, so this is
  # the shared-buffer path, with a subject the offset conversion has to walk
  assert_equal "い", /(い)/.match(:あいう)[1]
  assert_equal __ENCODING__ == "UTF-8" ? 1 : 3, /い/ =~ :あい
  assert_true /う/.match?(:あいう, 2)
  assert_false /あ/.match?(:あいう, 1)
  assert_true(/^あ/ === :あい)
end

assert("Regexp#match - Symbol argument does not alias the symbol table") do
  # A symbol long enough to miss the inline representation shares the symbol
  # table's buffer, and a dup keeps sharing it, so a destructive update has to
  # copy first.
  s = /a/.match(:abcdefghijklmnop).string.dup
  s << "Z"
  assert_equal "abcdefghijklmnopZ", s
  assert_equal "abcdefghijklmnop", :abcdefghijklmnop.to_s
end

assert("Regexp#match? - Symbol argument") do
  assert_true /a/.match?(:ab)
  assert_false /z/.match?(:ab)
  assert_false /a/.match?(:ab, 1)
  assert_true /b/.match?(:ab, 1)
end

assert("Regexp#=~ - Symbol argument") do
  assert_equal 1, (/b/ =~ :ab)
  assert_equal "b", $~[0]
  assert_nil(/z/ =~ :ab)
  assert_nil $~
end

assert("Regexp#=== - Symbol argument") do
  assert_true(/^to_/ === :to_s)
  assert_false(/^to_/ === :size)
  # Enumerable#grep is the motivating case: it dispatches through #===, so it
  # used to answer [] rather than raise
  assert_equal %i[to_s to_i], %i[to_s to_i size].grep(/^to_/)
  result = case :hello123
           when /\d+/ then "has digits"
           else "no digits"
           end
  assert_equal "has digits", result
end

assert("Regexp - match operand rejects other types") do
  assert_raise(TypeError) { /a/.match(1) }
  assert_raise(TypeError) { /a/.match?(1) }
  assert_raise(TypeError) { /a/ =~ 1 }
  # #=== answers false rather than raising, for symbols and everything else
  assert_false(/a/ === 1)
  assert_false(/a/ === Object.new)
  assert_false(/a/ === nil)
end

assert("Regexp.__byte_search answers a position before the subject with a miss") do
  # The mrblib loops enter this at zero or at an offset a match answered with,
  # so a negative one arrives only from a direct call. Left to the engine it
  # would read behind the subject; the answer instead is the miss a position
  # past the end already gives, and it clears the match globals the same way.
  $~ = /b/.match("abc")
  assert_nil Regexp.__byte_search(/b/, "abc", -1)
  assert_nil $~

  $~ = /b/.match("abc")
  assert_nil Regexp.__byte_search(/b/, "abc", -1000000)
  assert_nil $~

  $~ = /b/.match("abc")
  assert_nil Regexp.__byte_search(/b/, "abc", 1000000)
  assert_nil $~

  # and it is answered before the subject is read, the way `__search` answers a
  # position it cannot place: a subject the position names nothing in is not
  # read either way
  bad = "\xFF"
  assert_nil Regexp.__byte_search(/b/, bad, -1)
  if __ENCODING__ == "UTF-8"
    assert_raise(ArgumentError) { Regexp.__byte_search(/b/, bad, 0) }
  else
    assert_nil Regexp.__byte_search(/b/, bad, 0)
  end
end

assert("Regexp.__byte_rsearch reads its limit at both ends of the subject") do
  # The limit is the last position a match may start at, so one past the end
  # of the subject is every position in it and not the miss the forward
  # search answers a position past the end with. `rindex` clamps for the same
  # reason: `"abc".rindex(/b/, 10)` is 1.
  assert_equal 1, Regexp.__byte_rsearch(/b/, "abc", 1000000).__byte_begin(0)
  assert_equal 1, Regexp.__byte_rsearch(/b/, "abc", 3).__byte_begin(0)
  assert_nil Regexp.__byte_rsearch(/b/, "abc", 0)

  # A negative limit names no position and reaches here only from a direct
  # call, as a negative position does in `__byte_search` above: it is the
  # miss, and it clears the match globals.
  $~ = /b/.match("abc")
  assert_nil Regexp.__byte_rsearch(/b/, "abc", -1)
  assert_nil $~

  # and the answer is the one the globals describe
  assert_equal 4, Regexp.__byte_rsearch(/b(c)/, "abcabc", 6).__byte_begin(0)
  assert_equal "c", $1
  assert_equal "bc", Regexp.last_match(0)
end

assert("Regexp.escape") do
  assert_equal "a\\.b\\*c", Regexp.escape("a.b*c")

  # characters that are only special under the x flag or inside [...]
  assert_equal "a\\ b", Regexp.escape("a b")
  assert_equal "a\\#b", Regexp.escape("a#b")
  assert_equal "a\\-b", Regexp.escape("a-b")

  # control characters become printable two-character escapes
  assert_equal "a\\nb", Regexp.escape("a\nb")
  assert_equal "a\\tb", Regexp.escape("a\tb")
  assert_equal "a\\rb", Regexp.escape("a\rb")
  assert_equal "a\\fb", Regexp.escape("a\fb")
  assert_equal "a\\vb", Regexp.escape("a\vb")

  # non-ASCII bytes pass through untouched
  assert_equal "あ\\-い", Regexp.escape("あ-い")

  # the escaped pattern matches the original literally in every mode
  [" ", "#", "-", "\n", "\t", "\r", "\f", "\v"].each do |c|
    src = Regexp.escape(c)
    assert_true Regexp.new(src).match?(c)
    assert_true Regexp.new(src, Regexp::EXTENDED).match?(c)
  end
  assert_true Regexp.new(Regexp.escape("a b"), Regexp::EXTENDED).match?("a b")
  assert_true Regexp.new(Regexp.escape("a # b"), Regexp::EXTENDED).match?("a # b")
end

assert("Regexp.__check_byte_pos passes a position the subject does not have") do
  # `String#byteindex` and `String#byterindex` read the position against the
  # byte length and answer both ends themselves before asking this, so one
  # outside the subject arrives only from a direct call. A position the subject
  # does not have sits on no boundary, and looking for one would read behind
  # the subject.
  s = "あいう"
  assert_nil Regexp.__check_byte_pos(s, -1)
  assert_nil Regexp.__check_byte_pos(s, -1000000)
  assert_nil Regexp.__check_byte_pos(s, s.bytesize + 1)
  assert_nil Regexp.__check_byte_pos(s, 1000000)

  # the ones it does have are still asked
  assert_nil Regexp.__check_byte_pos(s, 0)
  assert_nil Regexp.__check_byte_pos(s, s.bytesize)
  if __ENCODING__ == "UTF-8"
    assert_raise(IndexError) { Regexp.__check_byte_pos(s, 1) }
  else
    assert_nil Regexp.__check_byte_pos(s, 1)
  end
end

assert("Regexp#inspect") do
  re = Regexp.new("abc", Regexp::IGNORECASE)
  assert_equal "/abc/i", re.inspect
  # several flags are written in the m, i, x order, whatever order they
  # were given in
  assert_equal "/abc/mi", Regexp.new("abc", Regexp::IGNORECASE | Regexp::MULTILINE).inspect
  assert_equal "/abc/mix", Regexp.new("abc", Regexp::IGNORECASE | Regexp::MULTILINE | Regexp::EXTENDED).inspect
end

assert("Regexp#to_s") do
  assert_equal "(?-mix:abc)", Regexp.new("abc").to_s
  assert_equal "(?i-mx:abc)", Regexp.new("abc", Regexp::IGNORECASE).to_s
  assert_equal "(?m-ix:abc)", Regexp.new("abc", Regexp::MULTILINE).to_s
  assert_equal "(?mi-x:abc)", Regexp.new("abc", Regexp::IGNORECASE | Regexp::MULTILINE).to_s
  # the '-' run is dropped only when no flag is off
  assert_equal "(?mix:abc)", Regexp.new("abc", Regexp::IGNORECASE | Regexp::MULTILINE | Regexp::EXTENDED).to_s

  # the form recompiles, and the flags it names do not leak either way
  assert_true Regexp.new(Regexp.new("abc", Regexp::IGNORECASE).to_s).match?("ABC")
  assert_false Regexp.new(Regexp.new("abc").to_s + "d", Regexp::IGNORECASE).match?("ABCd")

  # an extended pattern round-trips too: the "(?x-mi:" it prints is read
  # back as free-spacing over the source it wraps
  assert_equal "(?x-mi:a b)", Regexp.new("a b", Regexp::EXTENDED).to_s
  assert_true Regexp.new(Regexp.new("a b", Regexp::EXTENDED).to_s).match?("ab")
  assert_false Regexp.new(Regexp.new("a b", Regexp::EXTENDED).to_s).match?("a b")
  # a `#` comment in the source rides along and is still a comment inside the
  # "(?x-mi:", so its newline has to come before the closing ")"
  assert_true Regexp.new(Regexp.new("a # c\nb", Regexp::EXTENDED).to_s).match?("ab")
  assert_true(/#{Regexp.new("a b", Regexp::EXTENDED)}c d/.match?("abc d"))
end

assert("Regexp#to_s - interpolation") do
  inner = Regexp.new("abc", Regexp::IGNORECASE)
  # the inner Regexp keeps its own flags where the outer has none
  assert_true(/#{inner}d/.match?("ABCd"))
  assert_false(/#{inner}d/.match?("ABCD"))
  # and does not pick up the outer ones
  assert_false(/#{Regexp.new("abc")}d/i.match?("ABCd"))
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

assert("Regexp#dup and Regexp#clone") do
  # The compiled pattern cannot be shared with the copy, since one pattern is
  # owned by one object, so the copy compiles its own from the same source and
  # flags. Without that it answered the readers and refused every match.
  r = Regexp.new("ab(?<n>c)", Regexp::IGNORECASE)
  [r.dup, r.clone].each do |c|
    assert_equal r.source, c.source
    assert_equal r.options, c.options
    assert_equal r.to_s, c.to_s
    assert_equal r.names, c.names
    assert_equal r.named_captures, c.named_captures
    assert_true c == r
    assert_equal r.hash, c.hash
    assert_true c.match?("xABCy")
    assert_equal 1, c =~ "xABCy"
    assert_equal "C", c.match("xABCy")[:n]
    assert_equal "x-y", "xABCy".gsub(c, "-")
  end
  # the copy owns its own pattern, so the original outlives it and vice versa
  c = r.dup
  100.times { r.dup }
  GC.start
  assert_true c.match?("ABC")
  assert_true r.match?("ABC")
  # a copy of a subclass instance is compiled the same way
  sub = Class.new(Regexp).new("a+")
  assert_true sub.dup.match?("aaa")
  # the capture names belong to the pattern the copy compiled, not to the
  # table it inherited, so a source that names nothing leaves it none
  n = Regexp.new("(?<n>a)")
  n.instance_variable_set(:@source, "b")
  assert_equal [], n.dup.names
  assert_equal({}, n.dup.named_captures)
  m = Regexp.new("(?<n>a)")
  m.instance_variable_set(:@source, "(?<z>b)")
  assert_equal ["z"], m.dup.names
  # clone carries the frozen state, and a frozen original still copies
  frozen = Regexp.new("a", Regexp::IGNORECASE).freeze
  assert_true frozen.clone.frozen?
  assert_true frozen.clone.match?("A")
  assert_false frozen.dup.frozen?
end

assert("Regexp#initialize_copy") do
  # an original with no source has nothing to compile the copy from
  assert_raise(TypeError) { Regexp.allocate.dup }
  assert_raise(TypeError) { Regexp.allocate.clone }
  # reachable directly, so it refuses what dup and clone cannot hand it
  assert_raise(TypeError) { Regexp.new("a").dup.send(:initialize_copy, Regexp.new("b")) }
  assert_raise(TypeError) { Regexp.new("a").dup.send(:initialize_copy, "b") }
  # copying onto itself is a no-op, not a second compile
  r = Regexp.new("a")
  assert_true r.send(:initialize_copy, r).match?("a")
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

assert("Regexp literal /regex/") do
  assert_true /abc/.match?("abc")
  assert_equal "123", /\d+/.match("abc123")[0]
  assert_true /hello/i.match?("HELLO")
end
