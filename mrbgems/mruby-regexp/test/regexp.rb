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

assert("Regexp - dot advances by string mode") do
  str = "\xC3\xA9x"
  assert_equal [[0xC3, 0xA9], [0x78]], str.scan(/./).map { |m| m.bytes }
  assert_equal [0x5A, 0x78], str.sub(/./, "Z").bytes
  assert_equal "195,120,", str.gsub(/./) { |m| "#{m.bytes[0]}," }

  if Object.const_defined?(:Encoding)
    bin = str.dup.force_encoding("ASCII-8BIT")
    md = /x/.match(bin, 2)
    assert_equal "x", md[0]
    assert_equal 2, md.begin(0)
    assert_true /x/.match?(bin, 2)
    assert_equal 2, /x/ =~ bin

    assert_equal [[0xC3], [0xA9], [0x78]], bin.scan(/./).map { |m| m.bytes }
    assert_equal [0x5A, 0xA9, 0x78], bin.sub(/./, "Z").bytes
    assert_equal "195,169,120,", bin.gsub(/./) { |m| "#{m.bytes[0]}," }
  end
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

assert("Regexp - character class") do
  re = Regexp.new("[a-z]+")
  md = re.match("123abc456")
  assert_equal "abc", md[0]
end

assert("Regexp - POSIX bracket classes") do
  # ASCII semantics, like this gem's \w/\d shorthands.
  assert_equal "abc", "123abc456".match(/[[:alpha:]]+/)[0]
  assert_equal "123", "123abc".match(/[[:digit:]]+/)[0]
  assert_equal "abc123", "abc123!".match(/[[:alnum:]]+/)[0]
  assert_equal "deadBEEF", "deadBEEF".match(/[[:xdigit:]]+/)[0]
  assert_equal "snake_case", "snake_case".match(/[[:word:]]+/)[0]
  assert_equal "!", "ab!cd".match(/[[:punct:]]/)[0]
  assert_equal "AB", "abAB".match(/[[:upper:]]+/)[0]
  # combine with literals and other classes
  assert_equal "a1", "a1-".match(/[a[:digit:]]+/)[0]
  assert_equal "ab12", "ab12 ".match(/[[:alpha:][:digit:]]+/)[0]
  # negated forms
  assert_equal "abc", "abc123".match(/[[:^digit:]]+/)[0]
  assert_equal "x", " x".match(/[^[:space:]]/)[0]
  # an unknown class name is an error
  assert_raise(RegexpError) { Regexp.new("[[:bogus:]]") }
  # The name length used to be truncated with a (uint16_t) cast, so a name
  # 65536 bytes longer than "alpha" compared equal to "alpha" and compiled
  # as [[:alpha:]] instead of raising.
  long = "alpha" + "A" * 65536
  assert_raise(RegexpError) { Regexp.new("[[:#{long}:]]") }
  assert_raise(RegexpError) { Regexp.new("[[:^#{long}:]]") }
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

assert("Regexp - quantified first alternative does not leak into the next") do
  # A quantifier loops back to its own atom. When the atom starts the first
  # alternative, the alternation SPLIT is inserted in front of it; the
  # loop-back must follow the atom, not land on the new SPLIT (which used to
  # let /\d+|\w/ match "1b" by re-entering the alternation after "1").
  assert_equal "1", "1b2c3".match(/\d+|\w/)[0]
  assert_equal ["a", "1", "b", "2", "c", "3"], "a1b2c3".scan(/\d+|\w/)
  assert_equal "aaa", "aaa".match(/a+|b/)[0]
  assert_equal "123", "123abc".match(/\d+|\w+/)[0]
end

assert("Regexp - captures") do
  re = Regexp.new("(\\w+)@(\\w+)")
  md = re.match("user@host")
  assert_equal "user@host", md[0]
  assert_equal "user", md[1]
  assert_equal "host", md[2]
end

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

assert("Regexp - ^ and $ always match at line boundaries") do
  # In Ruby ^ and $ are line anchors regardless of /m (which only makes `.`
  # match a newline). \A and \z stay anchored to the whole string.
  assert_equal "bar", "foo\nbar".match(/^bar/)[0]
  assert_equal "foo", "foo\nbar".match(/foo$/)[0]
  assert_equal ["a", "b", "c"], "a\nb\nc".scan(/^./)
  assert_equal ["a", "b", "c"], "a\nb\nc".scan(/.$/)
  assert_equal 3, "a\nb\nc".scan(/^/).size
  # a trailing newline opens no final line, so ^ does not match at the end
  assert_equal 1, "a\n".scan(/^/).size
  assert_equal ">a\n>b\n>c", "a\nb\nc".gsub(/^/, ">")
  assert_equal ["a\n", "b\n", "c"], "a\nb\nc".split(/^/)
  # \A / \z remain absolute
  assert_nil(/\Abar/.match("foo\nbar"))
  assert_nil(/foo\z/.match("foo\nbar"))
  assert_equal "bar", "foo\nbar".match(/bar\z/)[0]
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

assert("Regexp - repeated group keeps each iteration self-contained") do
  # Copying a grouped quantifier body must relocate its internal jumps, or a
  # later copy jumps back into the first and reports the wrong capture span.
  m = "aaaaab".match(/(a{2,3}){2}/)
  assert_equal "aaaaa", m[0]
  assert_equal "aa", m[1]
  assert_equal "ab", "ababab".match(/(ab){2}/)[1]
  assert_equal "a", "abab".match(/(a|b){3}/)[1]
  assert_equal ["abab", "ab"], "abab".match(/((a)(b)){2}/).to_a[0, 2]
  assert_equal "34", "1234".match(/(\d{2}){2}/)[1]
end

assert("Regexp - repetition with a zero lower bound") do
  # A zero lower bound must not force the one already-compiled copy: {0,m}
  # caps at m (it used to match m+1), {0} matches nothing, {0,} is just *.
  assert_equal "aaa", "aaaa".match(/a{0,3}/)[0]
  assert_equal "aaa", "aaaa".match(/a{,3}/)[0]
  assert_equal "", "aaa".match(/a{0}/)[0]
  assert_equal "b", "b".match(/a{0}b/)[0]
  assert_equal "aaaa", "aaaa".match(/a{0,}/)[0]
  assert_equal "bc", "bc".match(/ba{0,2}c/)[0]
  assert_equal "baac", "baac".match(/ba{0,2}c/)[0]
  assert_nil "baaac".match(/\Aba{0,2}c\z/)
end

assert("Regexp - a curly brace that is not a quantifier is a literal") do
  # An invalid {...} used to spin the compiler forever (issue #6914); it must
  # be treated as a literal brace, matching CRuby. A well-formed quantifier
  # with nothing to repeat is an error instead.
  assert_equal "{a}", "x{a}y".match(/{a}/)[0]
  assert_equal "{", "a{b".match(/{/)[0]
  assert_equal "{}", "a{}b".match(/{}/)[0]
  assert_equal "a{}", "a{}".match(/a{}/)[0]
  assert_equal "{,}", "x{,}y".match(/{,}/)[0]
  assert_equal "a{b}c", "a{b}c".match(/a{b}c/)[0]
  assert_raise(RegexpError) { Regexp.new("{2}") }
end

assert("Regexp - patterns that used to hang the compiler now raise (A1)") do
  # These once looped forever in the compiler at 100% CPU instead of raising.
  # Regexp.new is used so the pattern reaches the regexp compiler directly,
  # bypassing the literal validation the parser performs on /.../ literals.

  # (?X) with an unsupported X: the absent operator (?~...) and conditionals
  # (?(...)) are not implemented (inline options (?i)/(?i:...) now are).
  assert_raise(RegexpError) { Regexp.new("(?~foo)") }
  assert_raise(RegexpError) { Regexp.new("(?(<x>)a|b)") }
  assert_raise(RegexpError) { Regexp.new("(?") }
  assert_raise(RegexpError) { Regexp.new("(?<") }

  # A quantifier metacharacter with no atom to repeat.
  assert_raise(RegexpError) { Regexp.new("a***") }
  assert_raise(RegexpError) { Regexp.new("*") }
  assert_raise(RegexpError) { Regexp.new("+") }
  assert_raise(RegexpError) { Regexp.new("?abc") }
end

assert("Regexp - inline options (?i) / (?i:...)") do
  # Toggle form: options apply to the rest of the enclosing group.
  assert_equal 0, (/(?i)abc/ =~ "ABC")
  assert_equal 0, (/a(?i)b/ =~ "aB")
  assert_nil (/a(?i)b/ =~ "Ab")          # the leading `a` stays case-sensitive
  assert_equal 0, (/(?i)a(?-i)b/ =~ "Ab") # `-i` turns it back off
  assert_nil (/(?i)a(?-i)b/ =~ "AB")

  # Scoped form: a non-capturing group whose options apply only to its body.
  assert_equal 0, (/(?i:abc)/ =~ "ABC")
  assert_nil (/(?i:a)b/ =~ "aB")          # option must not leak past the `)`
  assert_equal 0, (/(?i:ab)+/ =~ "AbaB")  # scoped group is still quantifiable

  # The toggle inside a group is confined to that group.
  assert_equal 0, (/(a(?i)b)c/ =~ "aBc")
  assert_nil (/(a(?i)b)c/ =~ "aBC")       # trailing `c` is case-sensitive again

  # m enables dot-matches-newline for its scope.
  assert_equal 0, (/(?m:a.b)/ =~ "a\nb")
  assert_nil (/a.b/ =~ "a\nb")

  # x (extended) cannot be scoped inline with the current architecture.
  assert_raise(RegexpError) { Regexp.new("(?x)a b") }
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

  # a POSIX bracket does not end the class, so what follows it is still
  # class content
  re = Regexp.new('[[:alpha:] ]', Regexp::EXTENDED)
  assert_true re.match?(" ")
  assert_true re.match?("a")

  re = Regexp.new('[[:alpha:]#x]', Regexp::EXTENDED)
  assert_true re.match?("#")

  assert_equal " 1 ", Regexp.new('[[:digit:] ]+', Regexp::EXTENDED).match(" 1 ")[0]

  # a ']' written first in a class is a literal member, so the class is
  # still open after it
  re = Regexp.new('[] ]', Regexp::EXTENDED)
  assert_true re.match?(" ")
  assert_true re.match?("]")

  re = Regexp.new('[^] ]', Regexp::EXTENDED)
  assert_false re.match?(" ")
  assert_true re.match?("a")

  # escaped whitespace is preserved
  re = Regexp.new('a\\ b', Regexp::EXTENDED)
  assert_true re.match?("a b")

  # inspect shows x flag
  assert_equal "/abc/x", Regexp.new("abc", Regexp::EXTENDED).inspect

  # to_s shows x flag
  assert_equal "(?x:abc)", Regexp.new("abc", Regexp::EXTENDED).to_s

  # errors quote the pattern as written, not the stripped text
  assert_raise_with_message(RegexpError, "unterminated character class: /a # c\n[/") do
    Regexp.new("a # c\n[", Regexp::EXTENDED)
  end
  assert_raise_with_message(RegexpError, "unmatched '(': /a b(/") do
    Regexp.new("a b(", Regexp::EXTENDED)
  end
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
  assert_equal ["a,b"], "a,b".split(/,/, 1.5)

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

assert("Regexp#named_captures") do
  assert_equal({"year" => [1], "month" => [2], "day" => [3]},
               /(?<year>\d+)-(?<month>\d+)-(?<day>\d+)/.named_captures)
  assert_equal({}, /\d+/.named_captures)

  # the returned Hash is a copy; mutating it must not affect a later call
  re = /(?<a>x)/
  re.named_captures["a"] = 99
  assert_equal({"a" => [1]}, re.named_captures)
end

assert("Regexp#names") do
  assert_equal ["year", "month", "day"],
               /(?<year>\d+)-(?<month>\d+)-(?<day>\d+)/.names
  assert_equal [], /\d+/.names

  # a name that is registered twice is reported once, as in CRuby
  assert_equal ["tag"], /(?<tag>\w+)-(?<tag>\w+)/.names
end

assert("Regexp - empty group name") do
  # (?<>x) used to compile and answer to "", and in /x mode the stored name
  # pointed into the preprocessing buffer the compiler frees on the way out.
  assert_raise(RegexpError) { Regexp.new("(?<>x)") }
  assert_raise(RegexpError) { Regexp.new("(?<>x) ", Regexp::EXTENDED) }
  assert_raise(RegexpError) { Regexp.new("(?<>x)\\k<>") }
  assert_raise(RegexpError) { Regexp.new("\\k<>") }
  assert_raise(RegexpError) { Regexp.new("\\k''") }

  # lookbehind is not a named group and is unaffected
  assert_equal "b", Regexp.new("(?<=a)b").match("ab")[0]
  assert_nil Regexp.new("(?<!a)b").match("ab")
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

assert("Regexp - group name longer than a uint16 length") do
  # The name length used to live in a uint16_t and was truncated with a cast,
  # so (uint16_t)65538 == 2 made this group answer to "ab" instead of to the
  # name it was given.
  long = "ab" + "A" * 65536
  re = Regexp.new("(?<#{long}>x)")
  assert_equal [long], re.named_captures.keys
  assert_equal "x", re.match("x")[long]
  assert_raise(IndexError) { re.match("x")["ab"] }

  # two names that shared a truncation stay distinct, and the two APIs that
  # resolve a name agree on which group it names
  re = Regexp.new("(?<ab>x)(?<#{long}>y)")
  md = re.match("xy")
  assert_equal "x", md["ab"]
  assert_equal "y", md[long]
  assert_equal({ "ab" => "x", long => "y" }, md.named_captures)

  # \k binds to the group the name was written on
  re = Regexp.new("(?<ab>x)(?<#{long}>y)\\k<#{long}>")
  assert_nil re.match("xyx")
  assert_equal "xyy", re.match("xyy")[0]

  # a name of exactly 65536 bytes is not the empty name
  z = "Z" * 65536
  assert_equal [z], Regexp.new("(?<#{z}>x)").named_captures.keys
end

assert("Regexp - named backreference \\k") do
  assert_equal "aa", "aa".match(/(?<n>\w)\k<n>/)[0]
  assert_equal "abba", "abba".match(/(?<a>.)(?<b>.)\k<b>\k<a>/)[0]
  assert_equal "1212", "1212".match(/(?<x>\d+)\k'x'/)[0]
  assert_nil "ab".match(/(?<n>\w)\k<n>/)
  # numeric and relative forms
  assert_equal "aa", "aa".match(/(a)\k<1>/)[0]
  assert_equal "abba", "abba".match(/(.)(.)\k<-1>\k<-2>/)[0]
  # an unknown name is an error
  assert_raise(RegexpError) { Regexp.new("\\k<missing>") }
end

assert("Regexp - numeric \\k backreference out of int range") do
  # The digit accumulator is an int with no bound, so 4294967297 used to wrap
  # to 1 and bind this backreference to group 1 instead of raising.
  assert_raise(RegexpError) { Regexp.new("(a)\\k<4294967297>") }
  assert_raise(RegexpError) { Regexp.new("(a)\\k<-4294967297>") }
  assert_raise(RegexpError) { Regexp.new("(a)(b)\\k<4294967298>") }
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

assert("Regexp - invalid UTF-8 byte near pattern end") do
  # a truncated multi-byte leader in a character class must not read
  # past the end of the pattern buffer
  re = Regexp.new("[   \xff ]")
  assert_kind_of Regexp, re
  assert_equal 0, (re =~ "\xff")
  assert_nil (re =~ "x")
end

assert("Regexp - truncated UTF-8 at subject end") do
  # a lone multi-byte leader at the end of the subject must not read
  # past the end of the string buffer when matched against a class
  assert_nil ("ab\xf0" =~ /[cd]/)
  assert_equal 0, ("ab\xf0" =~ /[^cd]+$/)
end

assert("Regexp - large non-ASCII character class does not overflow") do
  # a class listing tens of thousands of non-ASCII codepoints used to
  # overflow the 16-bit range capacity (32768 * 2 wrapped to 0, feeding a
  # size-0 realloc and a write through NULL). See issue #6937.
  # Patterns are always parsed as UTF-8, so build the bytes directly to
  # exercise this in both MRB_UTF8_STRING and byte-string builds.
  utf8 = ->(cp) {
    if cp < 0x800
      (0xC0 | (cp >> 6)).chr + (0x80 | (cp & 0x3F)).chr
    else
      (0xE0 | (cp >> 12)).chr + (0x80 | ((cp >> 6) & 0x3F)).chr + (0x80 | (cp & 0x3F)).chr
    end
  }
  s = "["
  i = 0x80
  while i <= 0x8080
    s += utf8.call(i)
    i += 1
  end
  s += "]"
  re = Regexp.new(s)
  assert_kind_of Regexp, re
  assert_equal 0, (re =~ utf8.call(0x80))
  assert_equal 0, (re =~ utf8.call(0x8080))
  assert_nil (re =~ utf8.call(0x8081))
  assert_nil (re =~ "A")
end
