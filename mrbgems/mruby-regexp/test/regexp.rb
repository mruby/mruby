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

assert("backward search - the match the globals describe is the answered one") do
  # `rindex` searches backward through re_byte_rsearch(), and the match it
  # answers with is the one `$~` and `$1` describe, as in every other search.
  assert_equal 4, "abcabc".rindex(/b(c)/)
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

assert("Regexp#to_s - a leading option group folds into the printed flags") do
  # a toggle governs everything after it, so its letters are the flags of the
  # whole and the group is gone: the two spellings of /a/i print alike
  assert_equal "(?i-mx:a)", Regexp.new("(?i)a").to_s
  assert_equal Regexp.new("a", Regexp::IGNORECASE).to_s, Regexp.new("(?i)a").to_s
  assert_equal "(?x-mi:a b)", Regexp.new("(?x)a b").to_s
  assert_equal "(?mi-x:a)", Regexp.new("(?i)(?m)a").to_s
  # a '-' run turns letters off, including one the object carries
  assert_equal "(?-mix:a)", Regexp.new("(?-i)a", Regexp::IGNORECASE).to_s
  assert_equal "(?i-mx:)", Regexp.new("(?i)").to_s

  # a scoped group governs only what it encloses, so it folds only when that
  # is the whole source
  assert_equal "(?i-mx:a)", Regexp.new("(?i:a)").to_s
  assert_equal "(?i-mx:a)", Regexp.new("(?i-m:a)").to_s
  assert_equal "(?-mix:)", Regexp.new("(?:)").to_s
  assert_equal "(?mi-x:a)", Regexp.new("(?i:a)", Regexp::MULTILINE).to_s
  # one level only: the group inside is text like any other
  assert_equal "(?i-mx:(?m:a))", Regexp.new("(?i:(?m:a))").to_s
  # a toggle ahead of it folds first, and then it does
  assert_equal "(?mi-x:a)", Regexp.new("(?i)(?m:a)").to_s

  # what the group does not enclose stays outside it, so nothing folds
  assert_equal "(?-mix:(?i:a)b)", Regexp.new("(?i:a)b").to_s
  # the ')' at the end closes the second group, not the first: only trying
  # the text between them as a pattern of its own tells the two apart
  assert_equal "(?-mix:(?i:a)(b))", Regexp.new("(?i:a)(b)").to_s
  assert_equal "(?-mix:a(?i:b))", Regexp.new("a(?i:b)").to_s

  # a group that is not an option group is not one to fold, and the toggles
  # already peeled ahead of it are printed again with it
  assert_equal "(?-mix:(?=a))", Regexp.new("(?=a)").to_s
  assert_equal "(?-mix:(?i)(?=a))", Regexp.new("(?i)(?=a)").to_s
  assert_equal "(?-mix:(?#c)a)", Regexp.new("(?#c)a").to_s
  assert_equal "(?-mix:(?<n>a))", Regexp.new("(?<n>a)").to_s

  # inspect prints the source as written either way
  assert_equal "/(?i)a/", Regexp.new("(?i)a").inspect
  assert_equal "/(?i:a)/m", Regexp.new("(?i:a)", Regexp::MULTILINE).inspect
  # and == compares source and options, which the fold does not touch
  assert_false Regexp.new("(?i)a") == Regexp.new("a", Regexp::IGNORECASE)
end

assert("Regexp#to_s - a folded form matches what it was folded from") do
  ["(?i)a", "(?i:a)", "(?i:a)b", "(?i:a)(b)", "(?x)a b", "(?i)(?m)a",
   "(?i:a|b)", "(?i:a)|b", "(?m)a.b", "(?i:(?m:a))"].each do |src|
    re = Regexp.new(src)
    round = Regexp.new(re.to_s)
    ["a", "A", "ab", "AB", "a b", "a\nb", "b", "a.b"].each do |input|
      assert_equal re.match?(input), round.match?(input), "#{src.inspect} on #{input.inspect}"
      # and carries only its own flags into a pattern that has others
      assert_equal re.match?(input), /#{re}/m.match?(input), "#{src.inspect} nested, on #{input.inspect}"
    end
  end
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

assert("Regexp readers on an uninitialized regexp") do
  # Regexp.allocate hands out an object with no @source, no @flags and no
  # compiled pattern; every reader is a reading of that missing state and
  # raises TypeError rather than crashing on it or answering from nothing.
  r = Regexp.allocate
  assert_raise(TypeError) { r.source }
  assert_raise(TypeError) { r.options }
  assert_raise(TypeError) { r.casefold? }
  assert_raise(TypeError) { r.names }
  assert_raise(TypeError) { r.named_captures }
  assert_raise(TypeError) { r.hash }
  assert_raise(TypeError) { r.to_s }
  assert_raise(TypeError) { r == Regexp.allocate }
  assert_raise(TypeError) { r == Regexp.new("abc") }
  assert_raise(TypeError) { Regexp.new(r) }
  # identity answers without reading either source
  assert_true r == r
  # and a non-Regexp is unequal before the source is looked at
  assert_false r == "abc"
  # inspect is the one reader that answers, so the object stays displayable
  assert_kind_of String, r.inspect
  assert_not_equal "//", r.inspect
end

class RegexpFailedCompile < Regexp
  def initialize
    super("(")
  rescue RegexpError
  end
end

assert("Regexp readers on a regexp whose compile raised") do
  # regexp_init() sets @source and @flags before it compiles, so this object
  # has a source to answer from where the allocated one has none; it must
  # keep answering the readers that do not need the compiled pattern.
  r = RegexpFailedCompile.new
  assert_equal "(", r.source
  assert_equal 0, r.options
  assert_false r.casefold?
  assert_equal [], r.names
  assert_equal({}, r.named_captures)
  assert_kind_of Integer, r.hash
  assert_equal "/(/", r.inspect
  assert_equal "(?-mix:()", r.to_s
  assert_true r == RegexpFailedCompile.new
  assert_false r == Regexp.new("abc")
  # but matching against it is still refused
  assert_raise(ArgumentError) { r =~ "(" }
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

assert("Regexp readers on a copy that never reached initialize_copy") do
  # A subclass can override initialize_copy and never call super, which leaves
  # the copy holding the original's @source and @flags and no pattern at all.
  # The readers answer from the compiled pattern, so they refuse it the way
  # they refuse Regexp.allocate: what says the object was initialized is
  # DATA_PTR, which no IV copy carries over, not the source it inherited.
  c = Class.new(Regexp) do
    def initialize_copy(other)
      self
    end
  end
  d = c.new("(?<n>a)").dup
  assert_raise(TypeError) { d.source }
  assert_raise(TypeError) { d.options }
  assert_raise(TypeError) { d.casefold? }
  assert_raise(TypeError) { d.to_s }
  assert_raise(TypeError) { d.hash }
  assert_raise(TypeError) { d.names }
  assert_raise(TypeError) { d.named_captures }
  assert_raise(TypeError) { d == Regexp.new("a") }
  assert_raise(TypeError) { d.match?("a") }
  # inspect answers rather than raising, but it answers about the object it
  # has, not about the source it inherited
  assert_true d.inspect.start_with?("#<")

  # nor can an IV write alone make an uninitialized Regexp answer
  a = Regexp.allocate
  a.instance_variable_set(:@source, "a")
  a.instance_variable_set(:@flags, 0)
  assert_raise(TypeError) { a.to_s }
  assert_raise(TypeError) { a.source }
  assert_raise(TypeError) { a.hash }
  assert_true a.inspect.start_with?("#<")

  # and an initialized Regexp whose @source was replaced raises rather than
  # reading whatever was put there as a String
  r = Regexp.new("a")
  r.instance_variable_set(:@source, nil)
  assert_raise(TypeError) { r.to_s }
  assert_raise(TypeError) { r.source }
  assert_kind_of String, r.inspect
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
